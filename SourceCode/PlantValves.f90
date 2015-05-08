MODULE PlantValves

          ! Module containing the routines dealing with the <module_name>

          ! MODULE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Jan, 2006
          !       MODIFIED       Nov 2010, B. Griffith, plant upgrades
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Collect "valve" type models for Plant loops

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
          ! <use statements for data only modules>
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals, ONLY: MaxNameLength
USE DataInterfaces, ONLY: ShowWarningError, ShowSevereError, ShowFatalError
USE General, ONLY: TrimSigDigits

          ! <use statements for access to subroutines in other modules>

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
TYPE TemperValveData
  ! user input data
    CHARACTER(len=MaxNameLength) :: Name     =' '   ! User identifier
    INTEGER :: PltInletNodeNum      = 0   ! Node number on the inlet side of the plant
    INTEGER :: PltOutletNodeNum     = 0   ! Node number on the outlet side of the plant
    INTEGER :: PltStream2NodeNum    = 0   ! Node number on the outlet side of the second stream
    INTEGER :: PltSetPointNodeNum   = 0   ! Node number for the setpoint node.
    INTEGER :: PltPumpOutletNodeNum = 0   ! node number for the pump outlet (for flow rate)
  ! Calculated and from elsewhere
    LOGICAL :: Init                 = .true. ! flag for initializationL true means do the initializations
    REAL(r64)    :: FlowDivFract         = 0.0d0 ! Fraction of flow sent down diversion path
    REAL(r64)    :: Stream2SourceTemp    = 0.0d0 ! Temperature [C] of stream 2 being mixed
    REAL(r64)    :: InletTemp            = 0.0d0 ! Temperature [C] of inlet to valve
    REAL(r64)    :: SetpointTemp         = 0.0d0 ! setpoint Temperatures [C] at control node.
    REAL(r64)    :: MixedMassFlowRate    = 0.0d0 ! Flow rate downstream of mixer [kg/s]
    REAL(r64)    :: DivertedFlowRate     = 0.0d0 ! flow rate through tempering valve's diversion path [kg/s]
   !loop topology variables
   INTEGER       :: LoopNum         =0
   INTEGER       :: LoopSideNum     =0
   INTEGER       :: BranchNum       =0
   INTEGER       :: CompNum         =0
END TYPE TemperValveData

TYPE (TemperValveData), ALLOCATABLE, DIMENSION(:):: TemperValve  !dimension to No. of TemperingValve objects
          ! MODULE VARIABLE DECLARATIONS:
INTEGER :: NumTemperingValves
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

          ! SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>:

PUBLIC  SimPlantValves
PRIVATE GetPlantValvesInput
PRIVATE InitPlantValves
PRIVATE CalcPlantValves
PRIVATE UpdatePlantValves
PRIVATE ReportPlantValves

CONTAINS

SUBROUTINE SimPlantValves(CompTypeNum,CompName,CompNum,RunFlag,InitLoopEquip,  &
                          MyLoad,MaxCap,MinCap,OptCap,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith, NREL
          !       DATE WRITTEN   Jan. 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulation manager for Plant valves

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: BeginEnvrnFlag
  USE DataInterfaces, ONLY: ShowFatalError, ShowContinueError, ShowSevereError
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: CompTypeNum
  CHARACTER(len=*), INTENT(IN) :: CompName
  INTEGER                      :: CompNum
  LOGICAL, INTENT(IN)          :: RunFlag !unused1208
  LOGICAL, INTENT(INOUT)       :: InitLoopEquip
  REAL(r64), INTENT(INOUT)     :: MyLoad !unused1208
  REAL(r64), INTENT(OUT)       :: MinCap
  REAL(r64), INTENT(OUT)       :: MaxCap
  REAL(r64), INTENT(OUT)       :: OptCap
  LOGICAL, INTENT(IN)          :: FirstHVACIteration ! TRUE if First iteration of simulation

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE      :: GetInputFlag = .true.  ! First time, input is "gotten"
  INTEGER :: EqNum

  IF (GetInputFlag) THEN
    CALL GetPlantValvesInput
    GetInputFlag=.false.
  ENDIF

  ! Find the correct Equipment
  IF (CompNum == 0) THEN
    EqNum = FindItemInList(CompName, TemperValve%Name, NumTemperingValves)
    IF (EqNum == 0) THEN
      CALL ShowFatalError('SimPlantValves: Unit not found='//TRIM(CompName))
    ENDIF
    CompNum=EqNum
  ELSE
    EqNum=CompNum
    IF (EqNum > NumTemperingValves .or. EqNum < 1) THEN
      CALL ShowFatalError('SimPlantValves:  Invalid CompNum passed='//  &
                          TRIM(TrimSigDigits(EqNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumTemperingValves))//  &
                          ', Entered Unit name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(EqNum)) THEN
      IF (CompName /= TemperValve(EqNum)%Name) THEN
        CALL ShowFatalError('SimPlantValves: Invalid CompNum passed='//  &
                            TRIM(TrimSigDigits(EqNum))// &
                            ', Unit name='//TRIM(CompName)//', stored Unit Name for that index='//  &
                            TRIM(TemperValve(EqNum)%Name))
      ENDIF
      CheckEquipName(EqNum)=.false.
    ENDIF
  ENDIF

  IF (InitLoopEquip) THEN
    MinCap = 0.0d0
    MaxCap = 0.0d0
    OptCap = 0.0d0
    RETURN
  ENDIF

  CALL InitPlantValves(CompTypeNum,CompNum)

  CALL CalcPlantValves(CompTypeNum,CompNum)

  CALL UpdatePlantValves(CompTypeNum,CompNum)

  CALL ReportPlantValves !(Args)

  RETURN

END SUBROUTINE SimPlantValves

SUBROUTINE GetPlantValvesInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Jan. 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! get input from user

          ! METHODOLOGY EMPLOYED:
          ! usual method using InputProcessor

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY :  ShowSevereError, ShowWarningError, ShowFatalError, SetupOutputVariable
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem ! might also use FindItemInList
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE BranchNodeConnections, ONLY: TestCompSet
  USE NodeInputManager,   ONLY: GetOnlySingleNode

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
  INTEGER                        :: Item    ! Item to be "gotten"
  CHARACTER(len=MaxNameLength), &
                    DIMENSION(6) :: Alphas  ! Alpha items for object
  REAL(r64), DIMENSION(1)             :: Numbers ! Numeric items for object
  INTEGER                        :: NumAlphas  ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: IOStatus   ! Used in GetObjectItem
  LOGICAL                        :: ErrorsFound=.false.  ! Set to true if errors in input, fatal at end of routine
  CHARACTER(len=MaxNameLength)   :: CurrentModuleObject  ! for ease in renaming.

 CurrentModuleObject = 'TemperingValve'
 NumTemperingValves = GetNumObjectsFound(CurrentModuleObject)

 ALLOCATE(TemperValve(NumTemperingValves))
 ALLOCATE(CheckEquipName(NumTemperingValves))
 CheckEquipName=.true.

  DO Item=1,NumTemperingValves

    CALL GetObjectItem(CurrentModuleObject,Item,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus)
  !  <process, noting errors>
     TemperValve(Item)%Name = Alphas(1)
  ! Get Plant Inlet Node
     TemperValve(Item)%PltInletNodeNum     = &
               GetOnlySingleNode(Alphas(2),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
  ! Get Plant Outlet Node
     TemperValve(Item)%PltOutletNodeNum = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)

  ! Get Stream 2 Source Node
     TemperValve(Item)%PltStream2NodeNum =  &
               GetOnlySingleNode(Alphas(4),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Water,NodeConnectionType_Sensor, 1, ObjectIsNotParent)
  ! Get Mixed water Setpoint
     TemperValve(Item)%PltSetPointNodeNum =  &
               GetOnlySingleNode(Alphas(5),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Water,NodeConnectionType_Setpoint, 1, ObjectIsNotParent)

  ! Get Pump outlet
     TemperValve(Item)%PltPumpOutletNodeNum =  &
               GetOnlySingleNode(Alphas(6),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Water,NodeConnectionType_Sensor, 1, ObjectIsNotParent)


    ! Note most checks on user input are made in second pass thru init routine

     CALL TestCompSet(TRIM(CurrentModuleObject), Alphas(1), Alphas(2), Alphas(3), 'Supply Side Water Nodes')

  ENDDO

  DO Item=1,NumTemperingValves

      CALL SetupOutputVariable('Tempering Valve Flow Fraction []', &
          TemperValve(Item)%FlowDivFract,'System','Average',TemperValve(Item)%Name)
  ENDDO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetPlantValvesInput: '//TRIM(CurrentModuleObject)//' Errors found in input')
  ENDIF

  RETURN

END SUBROUTINE GetPlantValvesInput

SUBROUTINE InitPlantValves(CompTypeNum,CompNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith, NREL
          !       DATE WRITTEN   Jan. 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! intialize data for valve modeling

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,       ONLY: BeginEnvrnFlag
  USE DataInterfaces,    ONLY: ShowSevereError, ShowWarningError, ShowContinueError, ShowContinueErrorTimeStamp
  USE DataLoopNode,      ONLY: Node
  USE DataPlant,         ONLY: TypeOf_ValveTempering, PlantLoop, ScanPlantLoopsForObject, &
                               GenEquipTypes_Pump, TypeOf_ValveTempering
  USE DataBranchAirLoopPlant, ONLY : ControlType_Active
  USE InputProcessor,    ONLY: SameString
  USE DataHVACGlobals,   ONLY: NumPlantLoops
  USE PlantUtilities,    ONLY: InitComponentNodes

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: CompTypeNum
  INTEGER , INTENT(IN)         :: CompNum
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: InletNode  ! local working variable for inlet node number
  INTEGER :: OutletNode ! local working variable for outlet node number
  INTEGER :: Strm2Node  ! local working variable for stream 2 outlet node number
  INTEGER :: SetPntNode ! local working variable for setpoint node number
  INTEGER :: PumpOutNode ! local working variable for pump outlet node number

  INTEGER :: i  ! plant loop do loop counter
  INTEGER :: j  ! plant half loop do loop counter
  INTEGER :: k  ! plant branches do loop counter
  Integer :: kk ! plant branches do loop counter, nested
  INTEGER :: l  ! plant components do loop counter
!  INTEgER :: ll ! plant components do loop counter, nested
  INTEGER :: m  ! plant splitter do loop counter
  INTEGER :: n  ! plant mixer do loop counter
  LOGICAL :: InNodeOnSplitter  ! input data check
  LOGICAL :: PumpOutNodeOkay ! input data check
  LOGICAL :: ErrorsFound  ! input data check
  LOGICAL :: TwoBranchesBetwn ! input data check
  LOGICAL :: SetpointNodeOkay ! input data check
  LOGICAL :: Stream2NodeOkay  ! input data check
  LOGICAL :: IsBranchActive   ! input data check

  INTEGER :: numLoopSides ! set to SIZE(PlantLoop(i)%LoopSide)

  LOGICAL, SAVE :: MyOneTimeFlag = .true. ! first pass log
  LOGICAL, ALLOCATABLE, DIMENSION(:), SAVE :: MyTwoTimeFlag ! second pass do input check
  LOGICAL :: errFlag


  SELECT CASE (CompTypeNum)

  CASE (TypeOf_ValveTempering)

  IF (MyOneTimeFlag) THEN
     MyOneTimeFlag = .false.
     ALLOCATE(MyTwoTimeFlag(NumTemperingValves))
     MyTwoTimeFlag=.true.
  ELSE
    ! delay checks one pass so more of plant data structure gets filled in
    IF (MyTwoTimeFlag(CompNum)) THEN
    ! do some checks on input data
     ! Search thru PlantLoop Data Structure to check some things.
       ! Locate the component on the plant loops for later usage
       errFlag=.false.
       CALL ScanPlantLoopsForObject(TemperValve(CompNum)%Name, &
                                    TypeOf_ValveTempering, &
                                    TemperValve(CompNum)%LoopNum, &
                                    TemperValve(CompNum)%LoopSideNum, &
                                    TemperValve(CompNum)%BranchNum, &
                                    TemperValve(CompNum)%CompNum,  &
                                    errFlag=errFlag)

       IF (errFlag) THEN
         CALL ShowFatalError('InitPlantValves: Program terminated due to previous condition(s).')
       ENDIF
     ! init logical flags
       ErrorsFound      = .FALSE.
       InNodeOnSplitter = .FALSE.
       PumpOutNodeOkay  = .FALSE.
       TwoBranchesBetwn = .FALSE.
       SetpointNodeOkay = .FALSE.
       Stream2NodeOkay  = .FALSE.
       IsBranchActive   = .FALSE.

     ! . A) find indexes of PlantLoop, Half loop, and Branch by searching CompData
       IF (ALLOCATED(PlantLoop)) THEN
         DO i = 1, NumPlantLoops
           IF (.NOT.ALLOCATED(PlantLoop(i)%LoopSide)) CYCLE
           numLoopSides = SIZE(PlantLoop(i)%LoopSide)
           DO j = 1, numLoopSides
             IF (.NOT.ALLOCATED(PlantLoop(i)%LoopSide(j)%Branch)) CYCLE
             DO k = 1, PlantLoop(i)%LoopSide(j)%TotalBranches
               IF (.NOT.ALLOCATED(PlantLoop(i)%LoopSide(j)%Branch(k)%Comp)) CYCLE
               DO l = 1, PlantLoop(i)%LoopSide(j)%Branch(k)%TotalComponents

                 IF  ( ( PlantLoop(i)%LoopSide(j)%Branch(k)%Comp(l)%TypeOf_Num == CompTypeNum) .AND. &
                     (PlantLoop(i)%LoopSide(j)%Branch(k)%Comp(l)%CompNum == CompNum)) THEN ! we found it.

                   IF (.not.SameString(PlantLoop(i)%LoopSide(j)%Branch(k)%Comp(l)%Name, TemperValve(CompNum)%Name)) THEN
                     ! why not, maybe plant loop structures not completely filled with available data?
                  !write(*,*) 'Temper Valve names', PlantLoop(i)%LoopSide(j)%Branch(k)%Comp(l)%Name, TemperValve(CompNum)%Name
                   ENDIF

                   ! is branch control type 'Active'
                 IF (PlantLoop(i)%LoopSide(j)%Branch(k)%ControlType == ControlType_Active) IsBranchActive = .true.

                    ! is Valve inlet node an outlet node of a splitter
                   IF (ALLOCATED(PlantLoop(i)%LoopSide(j)%Splitter)) THEN
                     DO m = 1, PlantLoop(i)%LoopSide(j)%NumSplitters
                       IF (ALLOCATED(PlantLoop(i)%LoopSide(j)%Splitter(m)%NodeNumOut)) THEN
                         IF (ANY(PlantLoop(i)%LoopSide(j)%Splitter(m)%NodeNumOut &
                               == TemperValve(CompNum)%PltInletNodeNum)) THEN
                             InNodeOnSplitter = .true.
                         ENDIF
                       ENDIF ! allocated

                       ! are there only 2 branches between splitter and mixer?
                       IF (PlantLoop(i)%LoopSide(j)%Splitter(m)%TotalOutletNodes == 2) THEN
                         TwoBranchesBetwn = .true.
                       ENDIF
                     ENDDO !loop over splitters
                   ENDIF ! allocated %splitter

                  ! is stream 2 node an inlet to the mixer ?
                   IF (ALLOCATED(PlantLoop(i)%LoopSide(j)%Mixer)) THEN
                      DO n = 1, PlantLoop(i)%LoopSide(j)%NumMixers
                        IF (.NOT. ALLOCATED(PlantLoop(i)%LoopSide(j)%Mixer(n)%NodeNumIn)) CYCLE
                        IF (ANY(PlantLoop(i)%LoopSide(j)%Mixer(n)%NodeNumIn  &
                               == TemperValve(CompNum)%PltStream2NodeNum)) THEN

                           ! Check other branches component's node, current branch is k
                             DO kk = 1, PlantLoop(i)%LoopSide(j)%TotalBranches
                               IF (k == kk) CYCLE  !already looped into this one
                               IF (.NOT. ALLOCATED(PlantLoop(i)%LoopSide(j)%Branch(KK)%Comp)) CYCLE
                                 IF (ANY(PlantLoop(i)%LoopSide(j)%Branch(kk)%Comp%NodeNumOut &
                                     == TemperValve(CompNum)%PltStream2NodeNum)) THEN !it is on other branch

                                     Stream2NodeOkay = .TRUE.

                                 ENDIF
                             ENDDO ! kk branch nested loop
                        ENDIF ! stream 2 node is inlet to mixer
                      ENDDO  !mixer loop
                   ENDIF ! mixer allocated

                   ! is pump node really the outlet of a branch with a pump?
                   DO kk=1, PlantLoop(i)%LoopSide(j)%TotalBranches
                      IF (PlantLoop(i)%LoopSide(j)%Branch(kk)%NodeNumOut == TemperValve(CompNum)%PltPumpOutletNodeNum) THEN
                         IF (ANY(PlantLoop(i)%LoopSide(j)%Branch(kk)%Comp%GeneralEquipType==GenEquipTypes_Pump)) THEN
                         !IF (PlantLoop(i)%LoopSide(j)%Branch(kk)%PumpPresent) THEN
                           PumpOutNodeOkay = .true.
                         ENDIF
                      ENDIF
                    ENDDO

                 ! does sensor node agree with plant loop setpoint?
                 IF (PlantLoop(i)%TempSetPointNodeNum == TemperValve(CompNum)%PltSetPointNodeNum) THEN
                   SetpointNodeOkay = .TRUE.
                 ENDIF

               ENDIF !found item

               ENDDO ! comps  l
             ENDDO ! Branches k
           ENDDO ! Loop Sides j
         ENDDO  ! Plant loops i
       ENDIF ! plant loop allocated

      IF (.NOT.IsBranchActive) THEN
        CALL ShowSevereError('TemperingValve object needs to be on an ACTIVE branch')
        ErrorsFound = .true.
      ENDIF

      IF (.NOT.InNodeOnSplitter) THEN
        CALL ShowSevereError('TemperingValve object needs to be between a Splitter and Mixer')
        ErrorsFound = .true.
      ENDIF

      IF (.NOT.PumpOutNodeOkay) THEN
        CALL ShowSevereError('TemperingValve object needs to reference a node that is the outlet of a pump on its loop')
        ErrorsFound = .true.
      ENDIF

      IF (.NOT.TwoBranchesBetwn) THEN
        CALL ShowSevereError('TemperingValve object needs exactly two branches between a Splitter and Mixer')
        ErrorsFound = .true.
      ENDIF

      IF (.NOT.SetpointNodeOkay) THEN
        CALL ShowSevereError('TemperingValve object setpoint node not valid.  '//  &
                            'Check Setpoint manager for Plant Loop Temp Setpoint')
        ErrorsFound = .true.
      ENDIF

      IF (.NOT.Stream2NodeOkay) THEN
        CALL ShowSevereError('TemperingValve object stream 2 source node not valid.')
        CALL ShowContinueError('Check that node is a component outlet, enters a mixer, and on the other branch')
        ErrorsFound = .true.
      ENDIF
      IF (ErrorsFound) THEN
        CALL ShowFatalError('Errors found in input, TemperingValve object '// trim(TemperValve(CompNum)%Name))
      ENDIF
      MyTwoTimeFlag(CompNum) = .false.
    ENDIF ! my two time flag for input checking

  ENDIF ! my one time flag for input checking

  InletNode  = TemperValve(CompNum)%PltInletNodeNum
  OutletNode = TemperValve(CompNum)%PltOutletNodeNum
  Strm2Node  = TemperValve(CompNum)%PltStream2NodeNum
  SetPntNode = TemperValve(CompNum)%PltSetPointNodeNum
  PumpOutNode= TemperValve(CompNum)%PltPumpOutletNodeNum

  IF ((BeginEnvrnFlag) .AND. (TemperValve(CompNum)%init)) THEN

    IF ((InletNode > 0) .AND. (OutletNode > 0)) THEN
    !   Node(InletNode)%Temp = 0.0
       Call InitComponentNodes(0.d0, Node(PumpOutNode)%MassFlowRateMax, &
                               TemperValve(CompNum)%PltInletNodeNum, &
                               TemperValve(CompNum)%PltOutletNodeNum, &
                               TemperValve(CompNum)%LoopNum, &
                               TemperValve(CompNum)%LoopSideNum, &
                               TemperValve(CompNum)%BranchNum, &
                               TemperValve(CompNum)%CompNum )

    ENDIF
    TemperValve(CompNum)%Init = .False.
  ENDIF

  IF (.NOT. BeginEnvrnFlag) TemperValve(CompNum)%Init = .TRUE.

  IF (InletNode > 0) THEN
    TemperValve(CompNum)%InletTemp = Node(InletNode)%Temp
  ENDIF
  IF (Strm2Node > 0) THEN
    TemperValve(CompNum)%Stream2SourceTemp = Node(Strm2Node)%Temp
  ENDIF
  IF (SetPntNode > 0) THEN
    TemperValve(CompNum)%SetpointTemp = Node(SetPntNode)%TempSetPoint
  ENDIF

  IF (PumpOutNode > 0) THEN
    TemperValve(CompNum)%MixedMassFlowRate = Node(PumpOutNode)%MassFlowRate
  ENDIF

  CASE DEFAULT
    ! should not come here, would have been caught already
  END SELECT

  RETURN

END SUBROUTINE InitPlantValves

SUBROUTINE CalcPlantValves(CompTypeNum,CompNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith, NREL
          !       DATE WRITTEN   Jan. 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  This routine does the calculations for Valves.
          !  Currently only one type of valve, for Tempering.


          ! METHODOLOGY EMPLOYED:
          !   Tempering valve calculations involve computing a flow fraction
          !     that should be diverted.  See update routine for setting flow rates.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant, ONLY : PlantLoop, TypeOf_ValveTempering
  USE DataGlobals, ONLY : KickOffSimulation

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,  INTENT(IN)    :: CompTypeNum
  INTEGER , INTENT(IN)    :: CompNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: Tin  ! local working variable for Inlet Temperature (C)
  REAL(r64) :: Tset ! local working variable for Setpoint Temperature (C)
  REAL(r64) :: Ts2  ! local Working Variable for Stream 2 outlet Temperature (C)
  INTEGER :: LoopNum
  INTEGER :: LoopSideNum

  LoopNum = TemperValve(CompNum)%LoopNum
  LoopSideNum = TemperValve(CompNum)%LoopSideNum

  IF (KickOffSimulation) RETURN

  SELECT CASE (CompTypeNum)

  CASE (TypeOf_ValveTempering)

    IF (PlantLoop(LoopNum)%Loopside(LoopSideNum)%FlowLock == 0) THEN
      Tin = TemperValve(CompNum)%InletTemp
      Tset = TemperValve(CompNum)%SetpointTemp
      Ts2 = TemperValve(CompNum)%Stream2SourceTemp

      IF (Ts2 <= Tset) THEN
        TemperValve(CompNum)%FlowDivFract = 0.0d0
      ELSE  ! Divert some or all flow
        IF (Tin < Ts2) THEN
          TemperValve(CompNum)%FlowDivFract = (Ts2 - Tset) / (Ts2 - Tin)
        ELSE
          TemperValve(CompNum)%FlowDivFract = 1.0d0
        END IF
      END IF
    ELSEIF (PlantLoop(LoopNum)%Loopside(LoopSideNum)%FlowLock == 1) THEN ! don't recalc diversion, just reuse current flows
      IF (TemperValve(CompNum)%MixedMassFlowRate > 0.0d0) Then
        TemperValve(CompNum)%FlowDivFract = Node(TemperValve(CompNum)%PltOutletNodeNum)%MassFlowRate &
                                           / TemperValve(CompNum)%MixedMassFlowRate
      ELSE
        TemperValve(CompNum)%FlowDivFract = 0.0d0
      ENDIF

    END IF

    IF (TemperValve(CompNum)%FlowDivFract < 0.0d0) TemperValve(CompNum)%FlowDivFract = 0.0d0
    IF (TemperValve(CompNum)%FlowDivFract > 1.0d0) TemperValve(CompNum)%FlowDivFract = 1.0d0

  CASE DEFAULT
    ! should not come here. would have been caught in init routine

  END SELECT

  RETURN

END SUBROUTINE CalcPlantValves

SUBROUTINE UpdatePlantValves(CompTypeNum,CompNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith, NREL
          !       DATE WRITTEN   Jan. 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Propagate calculations to rest of program

          ! METHODOLOGY EMPLOYED:
          ! set values at nodes

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE PlantUtilities, ONLY: SetComponentFlowRate, SafeCopyPlantNode
  USE DataPlant,      ONLY: TypeOf_ValveTempering

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER , INTENT(IN)    :: CompTypeNum
  INTEGER , INTENT(IN)    :: CompNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)   :: mdot !local fluid mass flow rate

  SELECT CASE (CompTypeNum)

  CASE (TypeOf_ValveTempering)

    CALL SafeCopyPlantNode(TemperValve(CompNum)%PltInletNodeNum, TemperValve(CompNum)%PltOutletNodeNum)

    ! set mass flows in diverter path
    mdot =  TemperValve(CompNum)%MixedMassFlowRate   &
                                             * TemperValve(CompNum)%FlowDivFract

    IF (TemperValve(CompNum)%LoopNum > 0) THEN
      CALL SetComponentFlowRate( mdot, &
                                 TemperValve(CompNum)%PltInletNodeNum,  &
                                 TemperValve(CompNum)%PltOutletNodeNum, &
                                 TemperValve(CompNum)%LoopNum,          &
                                 TemperValve(CompNum)%LoopSideNum,      &
                                 TemperValve(CompNum)%BranchNum,        &
                                 TemperValve(CompNum)%CompNum  )

      TemperValve(CompNum)%DivertedFlowRate = mdot
    ELSE
      TemperValve(CompNum)%DivertedFlowRate = 0.d0
    ENDIF
  CASE DEFAULT

    ! should not come here
  END SELECT

  RETURN

END SUBROUTINE UpdatePlantValves

SUBROUTINE ReportPlantValves

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith, NREL
          !       DATE WRITTEN   Jan. 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

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
          ! na

  !<this routine is typically needed only for those cases where you must transform the internal data to a reportable form>

! Nothing needs to be done (yet)

  RETURN

END SUBROUTINE ReportPlantValves

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

END MODULE PlantValves
