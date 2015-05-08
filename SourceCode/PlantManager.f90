MODULE PlantManager

          ! MODULE INFORMATION:
          !       AUTHOR         Sankaranarayanan K P, Rich Liesen
          !       DATE WRITTEN   May 2005
          !       MODIFIED
          !       RE-ENGINEERED  Sept. 2010 D. Fisher, Edwin Lee, Brent Griffith
          !                      major plant upgrades:
          !                         Single half loop solver
          !                         Automated branch control types
          !                         new loop sequencing structure
          !                         Temperature out range checks
          !

          ! PURPOSE OF THIS MODULE:
          ! This module serves as the driver for the plant simulation. All necessary iterations and update related to plant
          ! connections are performed in this module.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology

          ! REFERENCES: none

          ! OTHER NOTES: none

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals
USE DataHVACGlobals
USE DataPlant
USE DataBranchAirLoopPlant
USE DataLoopNode
USE DataInterfaces
USE FluidProperties
USE PlantLoopSolver,          ONLY : PlantHalfLoopSolver

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS
INTEGER, PARAMETER   :: MaxBranchLevel  = 200
INTEGER, PARAMETER   :: Plant           = 1
INTEGER, PARAMETER   :: Condenser       = 2
INTEGER, PARAMETER   :: SupplyLoopPumpSingleSplitMix    = 1
INTEGER, PARAMETER   :: DemandSingleSplitterMixer       = 1
INTEGER, PARAMETER   :: TempSetPt = 1001
INTEGER, PARAMETER   :: FlowSetPt = 1007
LOGICAL              :: InitLoopEquip = .TRUE.
LOGICAL              :: GetCompSizFac = .FALSE.

          !MODULE DERIVED TYPE DEFINITIONS
TYPE TempLoopData
  CHARACTER(len=MaxNameLength)     :: Name=' '          ! Name of the component list
  ! Loop connections
  CHARACTER(len=MaxNameLength)     :: BranchList=' '    ! Branch list name for the half loop
  CHARACTER(len=MaxNameLength)     :: ConnectList=' '   ! Connector list name for the half loop
  INTEGER                          :: TotalBranches=0   ! Total number of branches on the loop
  TYPE (BranchData), &
         ALLOCATABLE, DIMENSION(:) :: Branch            ! Branch data
  TYPE (SplitterData), &
         ALLOCATABLE, DIMENSION(:) :: Splitter          ! Data for splitter on branch (if any)
  TYPE (MixerData), &
         ALLOCATABLE, DIMENSION(:) :: Mixer             ! Data for mixer on branch (if any)
  LOGICAL                          :: SplitterExists    = .FALSE.   !Logical Flag indication splitter exists in the half loop
  LOGICAL                          :: MixerExists       = .FALSE.   !Logical Flag indication mixer exists in the half loop
  LOGICAL                          :: ByPassExists      = .FALSE.
  LOGICAL                          :: LoopHasConnectionComp = .FALSE.
END TYPE

TYPE LoopPipeData
  INTEGER                                   :: NumPipes = 0   ! Total number of pipes
  TYPE(PipeData), DIMENSION(:), ALLOCATABLE :: Pipe           ! Pipe data, using definition from DataPlant
END TYPE

          ! MODULE VARIABLE DEFINITIONS
INTEGER           :: PlantSupplyLoopCase = 0
INTEGER           :: PlantDemandLoopCase = 0

INTEGER, DIMENSION(:), ALLOCATABLE :: SupplySideInletNode  ! Node number for the supply side inlet
INTEGER, DIMENSION(:), ALLOCATABLE :: SupplySideOutletNode ! Node number for the supply side outlet
INTEGER, ALLOCATABLE, DIMENSION(:) :: DemandSideInletNode  ! Inlet node on the demand side

TYPE(LoopPipeData), DIMENSION(:), ALLOCATABLE :: LoopPipe
TYPE(TempLoopData),SAVE  :: TempLoop !=(' ',' ',' ',0, , , ,.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.)

          ! SUBROUTINE SPECIFICATIONS:
    !The following public routines are called from HVAC Manager
PUBLIC  ManagePlantLoops
PUBLIC  GetPlantLoopData        !called from SimHVAC
PUBLIC  GetPlantInput           !called from SimHVAC
PUBLIC  SetupReports            !called from SimHVAC
PUBLIC  SetupInitialPlantCallingOrder  !called from SimHVAC
PUBLIC  SetupBranchControlTypes !called from SimHVAC
!PUBLIC  CheckPlantLoopData      !called from SimHVAC
PUBLIC  CheckPlantOnAbort       !called from AbortEnergyPlus:untilityroutines

PRIVATE InitializeLoops
PUBLIC InitOneTimePlantSizingInfo
PRIVATE SizePlantLoop
PRIVATE RevisePlantCallingOrder
PRIVATE FindLoopSideInCallingOrder

PUBLIC UpdateNodeThermalHistory
PUBLIC ReInitPlantLoopsAtFirstHVACIteration
PUBLIC CheckIfAnyPlant

CONTAINS
           ! MODULE SUBROUTINES

SUBROUTINE ManagePlantLoops(FirstHVACIteration,SimAirLoops,SimZoneEquipment,SimNonZoneEquipment, &
                            SimPlantLoops,  SimElecCircuits)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Sankaranarayanan K P
          !       DATE WRITTEN   Apr 2005
          !       MODIFIED
          !       RE-ENGINEERED  B. Griffith, Feb. 2010

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages the plant loop simulation

          ! METHODOLOGY EMPLOYED:
          ! Set up the while iteration block for the plant loop simulation.
          ! Calls half loop sides to be simulated in predetermined order.
          ! Reset the flags as necessary

          ! REFERENCES:
          ! na

          ! USE STATEMENTS: NA

  USE DataGlobals,    ONLY: AnyEnergyManagementSystemInModel
  USE PlantUtilities, ONLY: LogPlantConvergencePoints
  USE DataConvergParams, ONLY : MinPlantSubIterations, MaxPlantSubIterations

          ! SUBROUTINE ARGUMENT DEFINITIONS
  LOGICAL, INTENT(IN):: FirstHVACIteration
  LOGICAL, INTENT(INOUT):: SimAirLoops            ! True when the air loops need to be (re)simulated
  LOGICAL, INTENT(INOUT):: SimZoneEquipment       ! True when zone equipment components need to be (re)simulated
  LOGICAL, INTENT(INOUT):: SimNonZoneEquipment    ! True when non-zone equipment components need to be (re)simulated
  LOGICAL, INTENT(INOUT):: SimPlantLoops          ! True when some part of Plant needs to be (re)simulated
  LOGICAL, INTENT(INOUT):: SimElecCircuits        ! True when electic circuits need to be (re)simulated

          ! SUBROUTINE PARAMETER DEFINITIONS


          ! SUBROUTINE VARIABLE DEFINITIONS
  INTEGER              :: IterPlant
  INTEGER              :: LoopNum
  INTEGER              :: LoopSide
  INTEGER              :: LoopSideNum
  INTEGER              :: OtherSide
  LOGICAL              :: SimHalfLoopFlag
  INTEGER              :: HalfLoopNum
  INTEGER              :: CurntMinPlantSubIterations

  IF ( ANY(PlantLoop%CommonPipeType == CommonPipe_Single) .OR. &
             ANY(PlantLoop%CommonPipeType == CommonPipe_TwoWay) )  THEN
    CurntMinPlantSubIterations = MAX(7, MinPlantSubIterations)
  ELSE
    CurntMinPlantSubIterations = MinPlantSubIterations
  ENDIF

  IF (TotNumLoops <= 0) THEN ! quick return if no plant in model
    SimPlantLoops = .FALSE.
    RETURN
  END IF

  IterPlant = 0
  CALL InitializeLoops(FirstHVACIteration)

  DO WHILE ((SimPlantLoops) .AND. (IterPlant <= MaxPlantSubIterations) )
    ! go through half loops in predetermined calling order
    DO HalfLoopNum = 1, TotNumHalfLoops

      LoopNum      = PlantCallingOrderInfo(HalfLoopNum)%LoopIndex
      LoopSide     = PlantCallingOrderInfo(HalfLoopNum)%LoopSide
      OtherSide    = 3 - LoopSide !will give us 1 if loopside is 2, or 2 if loopside is 1
      SimHalfLoopFlag  = PlantLoop(LoopNum)%LoopSide(LoopSide)%SimLoopSideNeeded !set half loop sim flag

      IF (SimHalfLoopFlag .OR. IterPlant <= CurntMinPlantSubIterations) THEN

        CALL PlantHalfLoopSolver(FirstHVACIteration, LoopSide, LoopNum, PlantLoop(LoopNum)%LoopSide(OtherSide)%SimLoopSideNeeded)

        ! Always set this side to false,  so that it won't keep being turned on just because of first hvac
        PlantLoop(LoopNum)%LoopSide(LoopSide)%SimLoopSideNeeded = .FALSE.

        ! If we did the demand side, turn on the supply side (only if we need to do it last)
        IF (LoopSide == DemandSide) THEN
          IF (PlantLoop(LoopNum)%HasPressureComponents) THEN
            PlantLoop(LoopNum)%LoopSide(OtherSide)%SimLoopSideNeeded = .FALSE.
          END IF
        END IF

        ! Update the report variable
        PlantReport(LoopNum)%LastLoopSideSimulated = LoopSide

        PlantManageHalfLoopCalls = PlantManageHalfLoopCalls + 1
      ENDIF

    ENDDO ! half loop based calling order...

  ! decide new status for SimPlantLoops flag
    SimPlantLoops = .FALSE.
    LoopLevel: DO LoopNum = 1, TotNumLoops
      LoopSideLevel: DO LoopSideNum = 1, 2
        IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%SimLoopSideNeeded)THEN
          SimPlantLoops = .TRUE.
          EXIT LoopLevel
        ENDIF
      ENDDO LoopSideLevel
    ENDDO LoopLevel

    IterPlant = IterPlant + 1   ! Increment the iteration counter
    IF (IterPlant < CurntMinPlantSubIterations) SimPlantLoops = .TRUE.
    PLANTManageSubIterations = PLANTManageSubIterations + 1 ! these are summed across all half loops for reporting
  END DO !while

! add check for non-plant system sim flag updates
!  could set SimAirLoops, SimElecCircuits, SimZoneEquipment flags for now
   DO LoopNum = 1, TotNumLoops
    DO LoopSide = DemandSide,SupplySide
      IF (PlantLoop(LoopNum)%LoopSide(LoopSide)%SimAirLoopsNeeded) SimAirLoops = .TRUE.
      IF (PlantLoop(LoopNum)%LoopSide(LoopSide)%SimZoneEquipNeeded) SimZoneEquipment = .TRUE.
    !  IF (PlantLoop(LoopNum)%LoopSide(LoopSide)%SimNonZoneEquipNeeded) SimNonZoneEquipment = .TRUE.
      IF (PlantLoop(LoopNum)%LoopSide(LoopSide)%SimElectLoadCentrNeeded) SimElecCircuits = .TRUE.
    ENDDO
  ENDDO

  !Also log the convergence history of all loopsides once complete
  CALL LogPlantConvergencePoints(FirstHVACIteration)

  RETURN
END SUBROUTINE ManagePlantLoops

SUBROUTINE GetPlantLoopData

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Sankaranarayanan K P
          !       DATE WRITTEN   April 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reads the primary plant loop
          ! attributes from the input file

          ! METHODOLOGY EMPLOYED:
          ! calls the Input Processor to retrieve data from input file.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: GetNumObjectsFound, GetObjectItem, GetObjectItemNum, VerifyName,SameString, FindItemInList
  USE DataIPShortCuts  ! Data for field names, blank numerics
  USE ScheduleManager, ONLY: GetScheduleIndex
  USE SetPointManager, ONLY: IsNodeOnSetPtManager, TempSetPt=>iCtrlVarType_Temp
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE BranchInputManager
  USE DataSizing, ONLY: Autosize
  USE SystemAvailabilityManager, ONLY: GetPlantAvailabilityManager
  USE FluidProperties,    ONLY: CheckFluidPropertyName, FindGlycol
  USE General, ONLY: RoundSigDigits
  USE DataConvergParams, ONLY: PlantConvergence

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetPlant/CondenserLoopData: '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: LoopNum    ! DO loop counter for loops
  INTEGER :: PressSimLoop !DO loop counter for pressure simulation type
  INTEGER :: NumAlphas   ! Number of elements in the alpha array
  INTEGER :: NumNums     ! Number of elements in the numeric array
  INTEGER :: IOStat      ! IO Status when calling get input subroutine
  INTEGER :: NumFluids   ! number of fluids in sim
  INTEGER :: PlantLoopNum
  INTEGER :: CondLoopNum
  CHARACTER(len=MaxNameLength),DIMENSION(18) :: Alpha !dimension to num of alpha fields in input
  REAL(r64), DIMENSION(30) :: Num  !dimension to num of numeric data fields in input
  LOGICAL :: ErrorsFound=.false.
  LOGICAL       :: IsNotOK               ! Flag to verify name
  LOGICAL       :: IsBlank               ! Flag for blank name
  CHARACTER(len=MaxNameLength)  :: LoadingScheme
  LOGICAL :: ErrFound
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.
  LOGICAL :: MatchedPressureString
  INTEGER :: PressSimAlphaIndex
!  INTEGER :: OpSchemeFound

         ! FLOW:
  CurrentModuleObject = 'PlantLoop'
  NumPlantLoops = GetNumObjectsFound(CurrentModuleObject) ! Get the number of primary plant loops
  CurrentModuleObject = 'CondenserLoop'
  NumCondLoops = GetNumObjectsFound(CurrentModuleObject) ! Get the number of Condenser loops
  TotNumLoops = NumPlantLoops + NumCondLoops
  ErrFound=.false.

  IF (TotNumLoops > 0) THEN
    ALLOCATE(PlantLoop(TotNumLoops))
    ALLOCATE(PlantConvergence(TotNumLoops))
    IF (.not. ALLOCATED(PlantAvailMgr)) THEN
     ALLOCATE(PlantAvailMgr(TotNumLoops))
    ENDIF
  ELSE
    RETURN
  END IF


  DO LoopNum = 1, TotNumLoops
    Alpha=''
    Num=0.0d0
    ALLOCATE(PlantLoop(LoopNum)%LoopSide(SupplySide))
    IF(LoopNum .LE. NumPlantLoops) THEN
      PlantLoopNum = LoopNum
      PlantLoop(LoopNum)%TypeofLoop = Plant
      CurrentModuleObject = 'PlantLoop'
      CALL GetObjectItem(CurrentModuleObject,PlantLoopNum,Alpha,NumAlphas,Num,NumNums,IOSTAT,     &
                AlphaBlank=lAlphaFieldBlanks,                                                         &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    ELSE
      CondLoopNum = LoopNum-NumPlantLoops
      PlantLoop(LoopNum)%TypeofLoop = Condenser
      CurrentModuleObject = 'CondenserLoop'
      CALL GetObjectItem(CurrentModuleObject,CondLoopNum,Alpha,NumAlphas,Num,NumNums,IOSTAT,     &
                NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    END IF

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(Alpha(1),PlantLoop%Name,LoopNum-1,IsNotOK,IsBlank, TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) Alpha(1)='xxxxx'
    END IF

    PlantLoop(LoopNum)%Name              = Alpha(1)   ! Load the Plant Loop Name

    IF (Alpha(2) == 'STEAM') THEN
      PlantLoop(LoopNum)%FluidType        = NodeType_Steam
      PlantLoop(LoopNum)%FluidName        = Alpha(2)
    ELSEIF (Alpha(2) == 'WATER') THEN
      PlantLoop(LoopNum)%FluidType        = NodeType_Water
      PlantLoop(LoopNum)%FluidName        = Alpha(2)
      PlantLoop(LoopNum)%FluidIndex       = FindGlycol(Alpha(2))
    ELSEIF (Alpha(2) == 'USERDEFINEDFLUIDTYPE') THEN
      PlantLoop(LoopNum)%FluidType        = NodeType_Water
      PlantLoop(LoopNum)%FluidName        = Alpha(3)
      ! check for valid fluid name
      NumFluids = CheckFluidPropertyName(Alpha(3))
      IF (NumFluids == 0) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//TRIM(Alpha(1))//'", missing fluid data for Plant loop.'  )
        ErrorsFound=.true.
      ELSE
        PlantLoop(LoopNum)%FluidIndex =FindGlycol(Alpha(3))
        IF (PlantLoop(LoopNum)%FluidIndex  == 0) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//TRIM(Alpha(1))//'", invalid glycol fluid data for Plant loop.' )
          ErrorsFound=.true.
        ENDIF
      ENDIF
    ELSE
      CALL ShowWarningError('Input error: '//TRIM(cAlphaFieldNames(2))// '='//TRIM(Alpha(2))//  &
                               'entered, in '//TRIM(CurrentModuleObject)//'='//TRIM(Alpha(1)))
      CALL ShowContinueError('Will default to Water.')

      PlantLoop(LoopNum)%FluidType        = NodeType_Water
      PlantLoop(LoopNum)%FluidName        = 'WATER'
      PlantLoop(LoopNum)%FluidIndex       = FindGlycol('WATER')
    ENDIF

    PlantLoop(LoopNum)%OperationScheme   = Alpha(4)   ! Load the Plant Control Scheme Priority List
!   Check to make sure not used previously.
!    IF(LoopNum .LE. NumPlantLoops) THEN
!      IF (LoopNum-1 > 0) THEN
!        OpSchemeFound=FindItemInList(Alpha(4),PlantLoop(1:LoopNum-1)%OperationScheme,LoopNum-1)
!      ELSE
!        OpSchemeFound=0
!      ENDIF
!      IF (OpSchemeFound > 0) THEN
!        CALL ShowSevereError(RoutineName//'PlantLoop="'//trim(PlantLoop(LoopNum)%Name)//'", OperationScheme already used.')
!        CALL ShowContinueError('...'//trim(cAlphaFieldNames(4))//'="'//trim(Alpha(4))//'" used previously in PlantLoop='//  &
!           trim(PlantLoop(OpSchemeFound)%Name)//'".')
!        ErrorsFound=.true.
!      ENDIF
!    ELSE   ! Condenser Loop
!      IF (LoopNum-1 > NumPlantLoops) THEN
!        OpSchemeFound=FindItemInList(Alpha(4),PlantLoop(NumPlantLoops+1:LoopNum-1)%OperationScheme,CondLoopNum-1)
!      ELSE
!        OpSchemeFound=0
!      ENDIF
!      IF (OpSchemeFound > 0) THEN
!        CALL ShowSevereError(RoutineName//'CondenserLoop="'//trim(PlantLoop(LoopNum)%Name)//'", OperationScheme already used.')
!        CALL ShowContinueError('...'//trim(cAlphaFieldNames(4))//'="'//trim(Alpha(4))//'" used previously in CondenserLoop='//  &
!           trim(PlantLoop(OpSchemeFound)%Name)//'".')
!        ErrorsFound=.true.
!      ENDIF
!    ENDIF

          ! Load the temperature and flow rate maximum and minimum limits
    PlantLoop(LoopNum)%MaxTemp        = Num(1)
    PlantLoop(LoopNum)%MinTemp        = Num(2)
    PlantLoop(LoopNum)%MaxVolFlowRate = Num(3)
    PlantLoop(LoopNum)%MinVolFlowRate = Num(4)

    !The Plant loop volume for both halves of the loop is read in and used in this module for the
    ! correct loop temperature step.  Loop data is read in supply side, but the volume is not used in
    ! a calculation there.
    PlantLoop(LoopNum)%Volume         = Num(5)
    IF (lNumericFieldBlanks(5)) PlantLoop(LoopNum)%Volume = AutoCalculate

          ! Load the Loop Inlet and Outlet Nodes and Connection Info (Alpha(7-10) are related to the supply side)
    PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNameIn = Alpha(6)
    PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNameOut= Alpha(7)
    PlantLoop(LoopNum)%LoopSide(SupplySide)%BranchList = Alpha(8)
    PlantLoop(LoopNum)%LoopSide(SupplySide)%ConnectList= Alpha(9)
    PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNameIn = Alpha(10)
    PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNameOut= Alpha(11)
    PlantLoop(LoopNum)%LoopSide(DemandSide)%BranchList = Alpha(12)
    PlantLoop(LoopNum)%LoopSide(DemandSide)%ConnectList= Alpha(13)

    PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumIn = &
               GetOnlySingleNode(Alpha(6),ErrorsFound,TRIM(CurrentModuleObject),Alpha(1), &
               PlantLoop(LoopNum)%FluidType,NodeConnectionType_Inlet, 1, ObjectIsParent)

    PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumOut = &
               GetOnlySingleNode(Alpha(7),ErrorsFound,TRIM(CurrentModuleObject),Alpha(1), &
               PlantLoop(LoopNum)%FluidType,NodeConnectionType_Outlet, 1, ObjectIsParent)

    PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNumIn = &
               GetOnlySingleNode(Alpha(10),ErrorsFound,TRIM(CurrentModuleObject),Alpha(1), &
               PlantLoop(LoopNum)%FluidType,NodeConnectionType_Inlet, 1, ObjectIsParent)

    PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNumOut = &
               GetOnlySingleNode(Alpha(11),ErrorsFound,TRIM(CurrentModuleObject),Alpha(1), &
               PlantLoop(LoopNum)%FluidType,NodeConnectionType_Outlet, 1, ObjectIsParent)

    PlantLoop(LoopNum)%Loopside(DemandSide)%InletNodeSetPt = &
                          IsNodeOnSetPtManager(PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNumIn,TempSetPt)
    PlantLoop(LoopNum)%Loopside(DemandSide)%OutletNodeSetPt = &
                          IsNodeOnSetPtManager(PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNumOut,TempSetPt)
    PlantLoop(LoopNum)%Loopside(SupplySide)%InletNodeSetPt = &
                          IsNodeOnSetPtManager(PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumIn,TempSetPt)
    PlantLoop(LoopNum)%Loopside(SupplySide)%OutletNodeSetPt = &
                          IsNodeOnSetPtManager(PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumOut,TempSetPt)

    PlantLoop(LoopNum)%TempSetPointNodeNum  = &
             GetOnlySingleNode(Alpha(5),ErrorsFound,TRIM(CurrentModuleObject),Alpha(1), &
             PlantLoop(LoopNum)%FluidType,NodeConnectionType_Sensor, 1, ObjectIsParent)

          ! Load the load distribution scheme.
    LoadingScheme = Alpha(14)
    IF (SameString(LoadingScheme,'Optimal')) THEN
        PlantLoop(LoopNum)%LoadDistribution = OptimalLoading
    ELSE IF (Samestring(LoadingScheme,'Sequential')) THEN
        PlantLoop(LoopNum)%LoadDistribution = SequentialLoading
    ELSE IF (Samestring(LoadingScheme,'Uniform')) THEN
        PlantLoop(LoopNum)%LoadDistribution = UniformLoading
    ELSE
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alpha(1))//'", Invalid choice.')
        CALL ShowContinueError('...'//TRIM(cAlphaFieldNames(14))// '="'//TRIM(Alpha(14))//'".')
        CALL ShowContinueError('Will default to SequentialLoading.') ! TODO rename point
       PlantLoop(LoopNum)%LoadDistribution = SequentialLoading
    END IF

!When dual setpoint is allowed in condenser loop modify this code. Sankar 06/29/2009
     IF(PlantLoop(LoopNum)%TypeOfLoop == Plant) THEN
    ! Get the Loop Demand Calculation Scheme
      IF (SameString(Alpha(16),'SingleSetpoint')) THEN
        PlantLoop(LoopNum)%LoopDemandCalcScheme = SingleSetPoint
      ELSE IF (Samestring(Alpha(16),'DualSetpointDeadband'))    THEN
         IF (PlantLoop(LoopNum)%FluidType == NodeType_Steam) THEN
           CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alpha(1))//'", Invalid choice.')
           CALL ShowContinueError(TRIM(cAlphaFieldNames(16))//'="'//TRIM(Alpha(16))//  &
                                 '" not valid for '//TRIM(cAlphaFieldNames(2))// '= Steam')
           CALL ShowContinueError('Will reset '//TRIM(cAlphaFieldNames(16))// ' = SingleSetPoint and simulation will continue.')
           PlantLoop(LoopNum)%LoopDemandCalcScheme = SingleSetPoint
         ELSE
           PlantLoop(LoopNum)%LoopDemandCalcScheme = DualSetPointDeadBand
         END IF
      ELSE IF (Samestring(Alpha(16),''))    THEN
        PlantLoop(LoopNum)%LoopDemandCalcScheme = SingleSetPoint
      ELSE
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alpha(1))//'", Invalid choice.')
        CALL ShowContinueError('...'//TRIM(cAlphaFieldNames(16))//'="'//TRIM(Alpha(16))//'".')
        CALL ShowContinueError('Will default to SingleSetPoint.') ! TODO rename point
        PlantLoop(LoopNum)%LoopDemandCalcScheme = SingleSetPoint
      END IF
     ELSE IF(PlantLoop(LoopNum)%TypeOfLoop == Condenser) THEN
        PlantLoop(LoopNum)%LoopDemandCalcScheme = SingleSetPoint
     END IF

!When Commonpipe is allowed in condenser loop modify this code. Sankar 06/29/2009
     IF(PlantLoop(LoopNum)%TypeOfLoop == Plant) THEN
      IF(SameString(Alpha(17),'CommonPipe')) THEN
        PlantLoop(LoopNum)%CommonPipeType = CommonPipe_Single
      ELSE IF(SameString(Alpha(17),'TwoWayCommonPipe')) THEN
        PlantLoop(LoopNum)%CommonPipeType = CommonPipe_TwoWay
      ELSE IF(SameString(Alpha(17),'None') .OR. lAlphaFieldBlanks(17)) THEN
        PlantLoop(LoopNum)%CommonPipeType = CommonPipe_No
      ELSE
       CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alpha(1))//'", Invalid choice.')
       CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(17))//'="'//TRIM(Alpha(17))//'".')
       CALL ShowContinueError('Refer to I/O reference document for more details.')
       ErrorsFound=.true.
      END IF
     ELSE IF(PlantLoop(LoopNum)%TypeOfLoop == Condenser) THEN
        PlantLoop(LoopNum)%CommonPipeType = CommonPipe_No
     END IF

     IF(PlantLoop(LoopNum)%CommonPipeType == CommonPipe_TwoWay)THEN
      IF(PlantLoop(LoopNum)%Loopside(DemandSide)%InletNodeSetPt .AND. &
        PlantLoop(LoopNum)%Loopside(SupplySide)%InletNodeSetPt) THEN
       CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alpha(1))//'", Invalid condition.')
       CALL ShowContinueError('While using a two way common pipe there can be setpoint on only one node other '//  &
         'than Plant Supply Outlet node.')
       CALL ShowContinueError('Currently both Plant Demand inlet and plant supply inlet have setpoints.')
       CALL ShowContinueError('Select one of the two nodes and rerun the simulation.')
       ErrorsFound=.true.
      END IF
      IF(.NOT. PlantLoop(LoopNum)%Loopside(DemandSide)%InletNodeSetPt .AND. &
        .NOT. PlantLoop(LoopNum)%Loopside(SupplySide)%InletNodeSetPt) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alpha(1))//'", Invalid condition.')
        CALL ShowContinueError('While using a two way common pipe there must be a setpoint in addition to '//  &
            'the Plant Supply Outlet node.')
        CALL ShowContinueError('Currently neither plant demand inlet nor plant supply inlet have setpoints.')
        CALL ShowContinueError('Select one of the two nodes and rerun the simulation.')
        ErrorsFound=.true.
      END IF
     END IF

    !Pressure Simulation Type Input
    !First set the alpha index in the object as it is different for plant/condenser
!When CommonPipe, etc., is allowed in condenser loop, modify this code.  Edwin/Sankar 08/12/2009
    IF(PlantLoop(LoopNum)%TypeOfLoop == Plant) THEN
      PressSimAlphaIndex = 18
    ELSE
      PressSimAlphaIndex = 15
    ENDIF

    IF(NumAlphas .GE. PressSimAlphaIndex)THEN
      MatchedPressureString = .FALSE.

      !Check all types
      DO PressSimLoop = 1, 4
        IF (SameString(Alpha(PressSimAlphaIndex),PressureSimType(PressSimLoop))) THEN
          PlantLoop(LoopNum)%PressureSimType = PressSimLoop
          MatchedPressureString = .TRUE.
          EXIT
        ENDIF
      ENDDO

      !If we found a match, check to make sure it is one of the valid
      ! ones for this phase of pressure implementation
      IF (MatchedPressureString) THEN
        IF ( (PlantLoop(LoopNum)%PressureSimType == Press_NoPressure) .OR. &
             (PlantLoop(LoopNum)%PressureSimType == Press_PumpPowerCorrection) .OR. &
             (PlantLoop(LoopNum)%PressureSimType == Press_FlowCorrection) ) THEN
          !We are OK here, move on
        ELSE
          !We have an erroneous input, alert user
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alpha(1))//'", Invalid choice.')
          CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(PressSimAlphaIndex))//'="'//  &
             TRIM(Alpha(PressSimAlphaIndex))//'".')
          CALL ShowContinueError('Currently only options are: ')
          CALL ShowContinueError('  - '//PressureSimType(Press_NoPressure))
          CALL ShowContinueError('  - '//PressureSimType(Press_PumpPowerCorrection))
          CALL ShowContinueError('  - '//PressureSimType(Press_FlowCorrection))
          ErrorsFound=.TRUE.
        ENDIF
      ENDIF

      !if we made it this far and didn't get a match, check for blank
      IF (.NOT. MatchedPressureString) THEN
        IF (TRIM(Alpha(PressSimAlphaIndex)) .EQ. '') THEN
          PlantLoop(LoopNum)%PressureSimType = Press_NoPressure
          MatchedPressureString = .TRUE.
          EXIT
        ENDIF
      ENDIF

      !if we made it this far, there was no match, and it wasn't blank
      IF (.NOT. MatchedPressureString) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alpha(1))//'", Invalid condition.')
        CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(PressSimAlphaIndex))//'="'//TRIM(Alpha(PressSimAlphaIndex))//'".')
        ErrorsFound=.TRUE.
      ENDIF

    ENDIF

    ErrFound=.false.

    IF(PlantLoop(LoopNum)%TypeOfLoop == Plant) THEN
     CALL GetPlantAvailabilityManager(Alpha(15),LoopNum,TotNumLoops,ErrFound)
    END IF

    IF (ErrFound) THEN
      CALL ShowContinueError('Input errors in  '//TRIM(CurrentModuleObject)//'='//TRIM(Alpha(1)))
      ErrorsFound=.true.
    ENDIF

    IF (GetFirstBranchInletNodeName(PlantLoop(LoopNum)%LoopSide(DemandSide)%BranchList) &
                                    /= PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNameIn) THEN
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alpha(1))//'", Invalid condition.')
      CALL ShowContinueError('The inlet node of the first branch in the '//TRIM(cAlphaFieldNames(12))//'='// &
                              TRIM(Alpha(12)) ) !"Plant Demand Side Branch List"
      CALL ShowContinueError('is not the same as the '//TRIM(cAlphaFieldNames(10))//'='//  &
                             TRIM(Alpha(10)) ) ! "Plant Demand Side Inlet Node Name"
      CALL ShowContinueError('Branch List Inlet Node Name='//  &  ! TODO rename point
                             TRIM(GetFirstBranchInletNodeName(PlantLoop(LoopNum)%LoopSide(DemandSide)%BranchList)))
      CALL ShowContinueError('Branches in a BRANCH LIST must be listed in flow order: '//  &
         'inlet branch, then parallel branches, then outlet branch.')  ! TODO rename point
      ErrorsFound=.true.
    ENDIF

    IF (GetLastBranchOutletNodeName(PlantLoop(LoopNum)%LoopSide(DemandSide)%BranchList) &
                                    /= PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNameOut) THEN
                         !"Plant Demand Side Branch List"
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alpha(1))//'", Invalid condition.')
      CALL ShowContinueError('The outlet node of the last branch in the '//TRIM(cAlphaFieldNames(12))//'='// &
                             TRIM(Alpha(12)) )
                         !"Plant Demand Side Outlet Node Name"
      CALL ShowContinueError('is not the same as the '//TRIM(cAlphaFieldNames(11))//'='// &
                             TRIM(alpha(11)) )
      CALL ShowContinueError('Branch List Outlet Node Name='//  &  ! TODO rename point
                             TRIM(GetLastBranchOutletNodeName(PlantLoop(LoopNum)%LoopSide(DemandSide)%BranchList)))
      ! TODO rename point
      CALL ShowContinueError('Branches in a BRANCH LIST must be listed in flow order: inlet branch, then parallel branches, '//  &
                             'then outlet branch.')
      ErrorsFound=.true.
    ENDIF

    IF (GetFirstBranchInletNodeName(PlantLoop(LoopNum)%LoopSide(SupplySide)%BranchList) &
                                    /= PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNameIn) THEN
                              !"Plant Supply Side Branch List"
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alpha(1))//'", Invalid condition.')
      CALL ShowContinueError('The inlet node of the first branch in the '//TRIM(cAlphaFieldNames(8))//'='// &
                             TRIM(Alpha(8)) )
                             !"Plant Supply Side Inlet Node Name
      CALL ShowContinueError('is not the same as the '//TRIM(cAlphaFieldNames(6))// '='// &
                             TRIM(Alpha(6)) )
      CALL ShowContinueError('Branch List Inlet Node Name='//  & ! TODO rename point
                             TRIM(GetFirstBranchInletNodeName(PlantLoop(LoopNum)%LoopSide(SupplySide)%BranchList)))
      ! TODO rename point
      CALL ShowContinueError('Branches in a BRANCH LIST must be listed in flow order: inlet branch, then parallel branches, '//  &
                             'then outlet branch.')
      ErrorsFound=.true.
    ENDIF

    IF (GetLastBranchOutletNodeName(PlantLoop(LoopNum)%LoopSide(SupplySide)%BranchList) &
                                    /= PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNameOut) THEN
                         !"Plant Supply Side Branch List"
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(Alpha(1))//'", Invalid condition.')
      CALL ShowContinueError('The outlet node of the last branch in the '//TRIM(cAlphaFieldNames(8))//'='// &
                             TRIM(Alpha(8)) )
                         !"Plant Supply Side Outlet Node Name"
      CALL ShowContinueError('is not the same as the '//TRIM(cAlphaFieldNames(7))//'='// &
                             TRIM(alpha(7)) )
      CALL ShowContinueError('Branch List Outlet Node Name='//  &  ! TODO rename point
                             TRIM(GetLastBranchOutletNodeName(PlantLoop(LoopNum)%LoopSide(SupplySide)%BranchList)))
      ! TODO rename point
      CALL ShowContinueError('Branches in a BRANCH LIST must be listed in flow order: inlet branch, then parallel branches, '//  &
                             'then outlet branch.')
      ErrorsFound=.true.
    ENDIF

  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Errors found in processing input. Preceding conditions cause termination.')
  ENDIF

! set up loop status (set by system availability managers) report variables
! Condenser loop does not have availability manager yet. Once implemented, move the setup output variable to
! outside the IF statement.
  DO LoopNum = 1, TotNumLoops

    CALL SetupOutputVariable('Plant System Cycle On Off Status []', PlantAvailMgr(LoopNum)%AvailStatus, &
                             'Plant','Average', PlantLoop(LoopNum)%Name)

  END DO

 RETURN

END SUBROUTINE GetPlantLoopData

SUBROUTINE GetPlantInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Sankaranarayanan K P
          !       DATE WRITTEN   April 2005
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets input either through the Plant Loop derived type
          ! or by calls out to the branch manager to obtain data.  By the end of
          ! the routine the module level derived type Loop should be fully allocated
          ! and fully populated.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor
  USE NodeInputManager
  USE BranchInputManager
  USE Pipes,                         ONLY: InitializePipes
  USE PipeHeatTransfer,              ONLY: InitializeHeatTransferPipes
  USE DataGlobals, ONLY: outputfiledebug

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

         ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumHalfLoops
  INTEGER :: LoopNum    ! DO loop counter for loops
  INTEGER :: HalfLoopNum
  INTEGER :: NumOfPipesinLoop
  INTEGER :: SysPipeNum
  INTEGER :: LoopSideNum
  INTEGER :: BranchNum  ! DO loop counter for branches
  INTEGER :: CompNum    ! DO loop counter for components
  INTEGER :: NodeNum    ! DO loop counter for nodes
  INTEGER :: PipeNum    ! Counter for pipes
  INTEGER :: Outlet
  INTEGER :: Inlet
  INTEGER :: NumParams
  INTEGER :: NumAlphas
  INTEGER :: NumNumbers
  INTEGER :: SplitNum
  INTEGER :: MixNum
  INTEGER :: NumConnectorsInLoop
  INTEGER :: ConnNum
  INTEGER :: Pos
  INTEGER :: TotCompsOnBranch
  INTEGER :: MaxNumAlphas
  INTEGER :: MaxNumNumbers

  LOGICAL :: SplitInBranch
  LOGICAL :: MixerOutBranch
  LOGICAL :: ErrorsFound=.false.
  LOGICAL :: DemandSideHasPump
  LOGICAL :: ASeriesBranchHasPump
  LOGICAL :: AParallelBranchHasPump

  CHARACTER(len=16) :: LoopIdentifier

  CHARACTER(len=MaxNameLength),ALLOCATABLE,SAVE,DIMENSION(:) :: BranchNames     ! Branch names from GetBranchList call
  CHARACTER(len=MaxNameLength),ALLOCATABLE,SAVE,DIMENSION(:) :: CompTypes       ! Branch names from GetBranchList call
  CHARACTER(len=MaxNameLength),ALLOCATABLE,SAVE,DIMENSION(:) :: CompNames       ! Branch names from GetBranchList call
  INTEGER,ALLOCATABLE,SAVE,DIMENSION(:) :: CompCtrls       ! Branch names from GetBranchList call
  CHARACTER(len=MaxNameLength),ALLOCATABLE,SAVE,DIMENSION(:) :: InletNodeNames  ! Node names from GetBranchData call
  CHARACTER(len=MaxNameLength),ALLOCATABLE,SAVE,DIMENSION(:) :: OutletNodeNames ! Node names from GetBranchData call
  INTEGER,ALLOCATABLE,SAVE,DIMENSION(:) :: InletNodeNumbers  ! Node numbers from GetBranchData call
  INTEGER,ALLOCATABLE,SAVE,DIMENSION(:) :: OutletNodeNumbers ! Node numbers from GetBranchData call
  LOGICAL,ALLOCATABLE,SAVE,DIMENSION(:) :: SplitOutBranch
  LOGICAL,ALLOCATABLE,SAVE,DIMENSION(:) :: MixerInBranch
  LOGICAL errflag
  INTEGER GeneralEquipType
  INTEGER TypeOfNum
  INTEGER LoopNuminArray


  CALL GetObjectDefMaxArgs('Connector:Splitter',NumParams,NumAlphas,NumNumbers)
  MaxNumAlphas=NumAlphas
  MaxNumNumbers=NumNumbers
  CALL GetObjectDefMaxArgs('Connector:Mixer',NumParams,NumAlphas,NumNumbers)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNumbers)
        ! FLOW:

!  TotNumLoops = NumPlantLoops + NumCondLoops    !Needed when including condenser.
  NumHalfLoops = 2*TotNumLoops    !Will be NumLoops when condenser added
  NumPipes = 0
  NumPlantPipes = 0
  NumCondPipes = 0
  ALLOCATE (LoopPipe(NumHalfLoops))
  HalfLoopNum = 0
  SysPipeNum=0

  DO LoopNum = 1, TotNumLoops ! Begin demand side loops ... When condenser is added becomes NumLoops
    DemandSideHasPump = .FALSE.
    TempLoop%LoopHasConnectionComp = .FALSE.
    TempLoop%Name=PlantLoop(LoopNum)%Name

    DO LoopSideNum = DemandSide, SupplySide
      ASeriesBranchHasPump = .FALSE.
      AParallelBranchHasPump = .FALSE.
      NumOfPipesinLoop         = 0                      ! Initialization
      HalfLoopNum = HalfLoopNum + 1
      TempLoop%ByPassExists = .FALSE.
      IF(PlantLoop(LoopNum)%TypeofLoop == Plant .AND. LoopSideNum == DemandSide) THEN
        LoopIdentifier = 'Plant Demand'
      ELSE IF(PlantLoop(LoopNum)%TypeofLoop == Plant .AND. LoopSideNum == SupplySide) THEN
        LoopIdentifier = 'Plant Supply'
      ELSE IF(PlantLoop(LoopNum)%TypeofLoop == Condenser .AND. LoopSideNum == DemandSide) THEN
        LoopIdentifier = 'Condenser Demand'
      ELSE IF(PlantLoop(LoopNum)%TypeofLoop == Condenser .AND. LoopSideNum == SupplySide) THEN
        LoopIdentifier = 'Condenser Supply'
      END IF

      TempLoop%BranchList = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%BranchList
      TempLoop%ConnectList = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%ConnectList

            ! Get the branch list and size the Branch portion of the Loop derived type
      TempLoop%TotalBranches=NumBranchesInBranchList(TempLoop%BranchList)
      ALLOCATE(BranchNames(TempLoop%TotalBranches))
      BranchNames=' '
      CALL GetBranchList(TempLoop%Name,TempLoop%BranchList,  &
                         TempLoop%TotalBranches,BranchNames,TRIM(LoopIdentifier))
      ALLOCATE (TempLoop%Branch(TempLoop%TotalBranches))

            ! Cycle through all of the branches and set up the node data
      DO BranchNum = 1, TempLoop%TotalBranches

        TempLoop%Branch(BranchNum)%Name = BranchNames(BranchNum)

        TempLoop%Branch(BranchNum)%TotalComponents=NumCompsInBranch(BranchNames(BranchNum))

        TempLoop%Branch(BranchNum)%IsByPass = .FALSE.

        ALLOCATE(CompTypes(TempLoop%Branch(BranchNum)%TotalComponents))
        CompTypes=' '
        ALLOCATE(CompNames(TempLoop%Branch(BranchNum)%TotalComponents))
        CompNames=' '
        ALLOCATE(CompCtrls(TempLoop%Branch(BranchNum)%TotalComponents))
        CompCtrls=0
        ALLOCATE(InletNodeNames(TempLoop%Branch(BranchNum)%TotalComponents))
        InletNodeNames=' '
        ALLOCATE(InletNodeNumbers(TempLoop%Branch(BranchNum)%TotalComponents))
        InletNodeNumbers=0
        ALLOCATE(OutletNodeNames(TempLoop%Branch(BranchNum)%TotalComponents))
        OutletNodeNames=' '
        ALLOCATE(OutletNodeNumbers(TempLoop%Branch(BranchNum)%TotalComponents))
        OutletNodeNumbers=0

        CALL GetBranchData(TempLoop%Name,BranchNames(BranchNum),                                     &
                           TempLoop%Branch(BranchNum)%MaxVolFlowRate,  &  ! Why is this Vdot and not mdot?
                           TempLoop%Branch(BranchNum)%PressureCurveType, &
                           TempLoop%Branch(BranchNum)%PressureCurveIndex, &
                           TempLoop%Branch(BranchNum)%TotalComponents, &
                           CompTypes,CompNames,                              &
                           InletNodeNames,InletNodeNumbers,                            &
                           OutletNodeNames,OutletNodeNumbers,ErrorsFound)

        ALLOCATE (TempLoop%Branch(BranchNum)%Comp(TempLoop%Branch(BranchNum)%TotalComponents))

        DO CompNum = 1, TempLoop%Branch(BranchNum)%TotalComponents
          TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType =UnknownStatusOpSchemeType
          TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf      = CompTypes(CompNum)

          IF (SameString(CompTypes(CompNum),'Pipe:Adiabatic')) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num = TypeOf_Pipe
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Pipe
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = NoControlOpSchemeType
          ELSE IF (SameString(CompTypes(CompNum),'Pipe:Adiabatic:Steam')) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num = TypeOf_PipeSteam
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Pipe
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = NoControlOpSchemeType
          ELSE IF (SameString(CompTypes(CompNum),'Pipe:Outdoor')) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num = TypeOf_PipeExterior
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Pipe
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = NoControlOpSchemeType
          ELSE IF (SameString(CompTypes(CompNum),'Pipe:Indoor')) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num = TypeOf_PipeInterior
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Pipe
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = NoControlOpSchemeType
          ELSE IF (SameString(CompTypes(CompNum),'Pipe:Underground')) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num = TypeOf_PipeUnderground
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Pipe
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = NoControlOpSchemeType
          ELSE IF (SameString(CompTypes(CompNum),'PipingSystem:Underground:PipeCircuit')) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num = TypeOf_PipingSystemPipeCircuit
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Pipe
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = NoControlOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum)(1:4),'Pump') .or. SameString(CompTypes(CompNum)(1:13),'HeaderedPumps') ) THEN
            IF (SameString(CompTypes(CompNum)(1:18),'Pump:VariableSpeed')) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num = TypeOf_PumpVariableSpeed
            ELSE IF (SameString(CompTypes(CompNum)(1:18),'Pump:ConstantSpeed')) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num = TypeOf_PumpConstantSpeed
            ELSE IF (SameString(CompTypes(CompNum)(1:29),'Pump:VariableSpeed:Condensate')) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num = TypeOf_PumpCondensate
            ELSE IF (SameString(CompTypes(CompNum)(1:27),'HeaderedPumps:ConstantSpeed')) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num = TypeOf_PumpBankConstantSpeed
            ELSE IF (SameString(CompTypes(CompNum)(1:27),'HeaderedPumps:VariableSpeed')) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num = TypeOf_PumpBankVariableSpeed
            ELSE
             !discover unsupported equipment on branches.
              CALL ShowSevereError('GetPlantInput: trying to process a pump type that is not supported, dev note')
              CALL ShowContinueError('Component Type ='//TRIM(CompTypes(CompNum)) )
            END IF
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Pump
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = PumpOpSchemeType
            IF(LoopSideNum == DemandSide) DemandSideHasPump = .TRUE.
            IF(BranchNum==1 .OR. BranchNum==TempLoop%TotalBranches)THEN
              ASeriesBranchHasPump = .TRUE.
            ELSE
              AParallelBranchHasPump = .TRUE.
            END IF
            CALL StoreAPumpOnCurrentTempLoop(LoopNum, LoopSideNum, BranchNum, CompNum, CompNames(CompNum), &
                                               OutletNodeNumbers(CompNum), AParallelBranchHasPump)
          ELSEIF (SameString(CompTypes(CompNum),'WaterHeater:Mixed')) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_WtrHeaterMixed
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_WaterThermalTank
            IF (LoopSideNum == DemandSide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
            ELSEIF (LoopSideNum == SupplySide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
            ENDIF
          ELSEIF (SameString(CompTypes(CompNum),'WaterHeater:Stratified')) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_WtrHeaterStratified
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_WaterThermalTank
            IF (LoopSideNum == DemandSide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
            ELSEIF (LoopSideNum == SupplySide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
            ENDIF
          ELSEIF (SameString(CompTypes(CompNum),'ChillerHeater:Absorption:Directfired')) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_Chiller_DFAbsorption
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Chiller
          ELSEIF (SameString(CompTypes(CompNum),'ChillerHeater:Absorption:DoubleEffect')) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_Chiller_ExhFiredAbsorption
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Chiller
          ELSEIF (SameString(CompTypes(CompNum),'ThermalStorage:ChilledWater:Mixed')) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_ChilledWaterTankMixed
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_ThermalStorage
            IF (LoopSideNum == DemandSide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
            ELSEIF (LoopSideNum == SupplySide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
            ENDIF
          ELSEIF (SameString(CompTypes(CompNum),'ThermalStorage:ChilledWater:Stratified')) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_ChilledWaterTankStratified
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_ThermalStorage
            IF (LoopSideNum == DemandSide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
            ELSEIF (LoopSideNum == SupplySide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
            ENDIF
          ELSEIF (SameString(CompTypes(CompNum),'WaterUse:Connections')) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_WaterUseConnection
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_WaterUse
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum),'Coil:Cooling:Water')) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_CoilWaterCooling
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_DemandCoil
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum),'Coil:Cooling:Water:DetailedGeometry')) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_CoilWaterDetailedFlatCooling
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_DemandCoil
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum),'Coil:Heating:Water')) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_CoilWaterSimpleHeating
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_DemandCoil
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum),'Coil:Heating:Steam')) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_CoilSteamAirHeating
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_DemandCoil
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum),'SolarCollector:FlatPlate:Water')) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_SolarCollectorFlatPlate
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_SolarCollector
            IF (LoopSideNum == DemandSide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
            ELSEIF (LoopSideNum == SupplySide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UncontrolledOpSchemeType
            ENDIF
          ELSEIF (SameString(CompTypes(CompNum),'SolarCollector:IntegralCollectorStorage')) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_SolarCollectorICS
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_SolarCollector
            IF (LoopSideNum == DemandSide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
            ELSEIF (LoopSideNum == SupplySide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UncontrolledOpSchemeType
            ENDIF
          ELSEIF (SameString(CompTypes(CompNum),'LoadProfile:Plant')) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_PlantLoadProfile
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_LoadProfile
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'GroundHeatExchanger:Vertical') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_GrndHtExchgVertical
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_GroundHeatExchanger
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UncontrolledOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'GroundHeatExchanger:Surface') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_GrndHtExchgSurface
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_GroundHeatExchanger
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UncontrolledOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'GroundHeatExchanger:Pond') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_GrndHtExchgPond
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_GroundHeatExchanger
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UncontrolledOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'Chiller:Electric:EIR') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_Chiller_ElectricEIR
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Chiller
            IF (LoopSideNum == DemandSide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
            ELSEIF (LoopSideNum == SupplySide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
            ENDIF
          ELSEIF (SameString(CompTypes(CompNum), 'Chiller:Electric:ReformulatedEIR') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_Chiller_ElectricReformEIR
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Chiller
            IF (LoopSideNum == DemandSide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
            ELSEIF (LoopSideNum == SupplySide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
            ENDIF
          ELSEIF (SameString(CompTypes(CompNum), 'Chiller:Electric') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_Chiller_Electric
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Chiller
            IF (LoopSideNum == DemandSide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
            ELSEIF (LoopSideNum == SupplySide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
            ENDIF
          ELSEIF (SameString(CompTypes(CompNum), 'Chiller:EngineDriven') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_Chiller_EngineDriven
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Chiller
            IF (LoopSideNum == DemandSide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
            ELSEIF (LoopSideNum == SupplySide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
            ENDIF
          ELSEIF (SameString(CompTypes(CompNum), 'Chiller:CombustionTurbine') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_Chiller_CombTurbine
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Chiller
            IF (LoopSideNum == DemandSide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
            ELSEIF (LoopSideNum == SupplySide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
            ENDIF
          ELSEIF (SameString(CompTypes(CompNum), 'Chiller:ConstantCOP') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_Chiller_ConstCOP
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Chiller
            IF (LoopSideNum == DemandSide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
            ELSEIF (LoopSideNum == SupplySide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
            ENDIF
          ELSEIF (SameString(CompTypes(CompNum), 'Boiler:HotWater') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_Boiler_Simple
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Boiler
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'Boiler:Steam') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_Boiler_Steam
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Boiler
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'Chiller:Absorption:Indirect') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_Chiller_Indirect_Absorption
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Chiller
            IF (LoopSideNum == DemandSide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
            ELSEIF (LoopSideNum == SupplySide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
            ENDIF
          ELSEIF (SameString(CompTypes(CompNum), 'Chiller:Absorption') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_Chiller_Absorption
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Chiller
            IF (LoopSideNum == DemandSide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
            ELSEIF (LoopSideNum == SupplySide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
            ENDIF
          ELSEIF (SameString(CompTypes(CompNum), 'CoolingTower:SingleSpeed') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_CoolingTower_SingleSpd
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_CoolingTower
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'CoolingTower:TwoSpeed') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_CoolingTower_TwoSpd
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_CoolingTower
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'CoolingTower:VariableSpeed') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_CoolingTower_VarSpd
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_CoolingTower
          ELSEIF (SameString(CompTypes(CompNum), 'CoolingTower:VariableSpeed:Merkel') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_CoolingTower_VarSpdMerkel
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_CoolingTower
          ELSEIF (SameString(CompTypes(CompNum), 'Generator:FuelCell:ExhaustGasToWaterHeatExchanger') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_Generator_FCExhaust
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Generator
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'WaterHeater:HeatPump') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_HeatPumpWtrHeater
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_WaterThermalTank
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'HeatPump:WatertoWater:EquationFit:Cooling') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_HPWaterEFCooling
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_HeatPump
            IF (LoopSideNum == DemandSide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
            ELSEIF (LoopSideNum == SupplySide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
            ENDIF
          ELSEIF (SameString(CompTypes(CompNum), 'HeatPump:WatertoWater:EquationFit:Heating') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_HPWaterEFHeating
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_HeatPump
            IF (LoopSideNum == DemandSide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
            ELSEIF (LoopSideNum == SupplySide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
            ENDIF
          ELSEIF (SameString(CompTypes(CompNum), 'HeatPump:WaterToWater:ParameterEstimation:Heating') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_HPWaterPEHeating
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_HeatPump
            IF (LoopSideNum == DemandSide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
            ELSEIF (LoopSideNum == SupplySide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
            ENDIF
          ELSEIF (SameString(CompTypes(CompNum), 'HeatPump:WaterToWater:ParameterEstimation:Cooling') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_HPWaterPECooling
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_HeatPump
            IF (LoopSideNum == DemandSide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
            ELSEIF (LoopSideNum == SupplySide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
            ENDIF
          ELSEIF (SameString(CompTypes(CompNum), 'AirConditioner:VariableRefrigerantFlow') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_HeatPumpVRF
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_HeatPump
            IF (LoopSideNum == DemandSide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
            ELSEIF (LoopSideNum == SupplySide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
            ENDIF
          ELSEIF (SameString(CompTypes(CompNum), 'DistrictCooling') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_PurchChilledWater
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Purchased
          ELSEIF (SameString(CompTypes(CompNum), 'DistrictHeating') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_PurchHotWater
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Purchased
          ELSEIF (SameString(CompTypes(CompNum), 'ThermalStorage:Ice:Simple') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_TS_IceSimple
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_ThermalStorage
          ELSEIF (SameString(CompTypes(CompNum), 'ThermalStorage:Ice:Detailed') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_TS_IceDetailed
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_ThermalStorage
          ELSEIF (SameString(CompTypes(CompNum), 'TemperingValve') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_ValveTempering
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Valve
          ELSEIF (SameString(CompTypes(CompNum), 'HeatExchanger:FluidToFluid') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_FluidToFluidPlantHtExchg
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_HeatExchanger
            IF (LoopSideNum == DemandSide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
            ELSEIF (LoopSideNum == SupplySide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = FreeRejectionOpSchemeType
            ENDIF
          ELSEIF (SameString(CompTypes(CompNum), 'Generator:MicroTurbine') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_Generator_MicroTurbine
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Generator
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'Generator:InternalCombustionEngine') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_Generator_ICEngine
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Generator
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'Generator:CombustionTurbine') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_Generator_CTurbine
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Generator
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'Generator:MicroCHP') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_Generator_MicroCHP
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Generator
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'Generator:FuelCell:StackCooler') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_Generator_FCStackCooler
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Generator
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'Fluidcooler:SingleSpeed') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_FluidCooler_SingleSpd
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_FluidCooler
          ELSEIF (SameString(CompTypes(CompNum), 'Fluidcooler:TwoSpeed') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_FluidCooler_TwoSpd
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_FluidCooler
          ELSEIF (SameString(CompTypes(CompNum), 'EvaporativeFluidcooler:SingleSpeed') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_EvapFluidCooler_SingleSpd
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_EvapFluidCooler
          ELSEIF (SameString(CompTypes(CompNum), 'EvaporativeFluidcooler:TwoSpeed') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_EvapFluidCooler_TwoSpd
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_EvapFluidCooler
          ELSEIF (SameString(CompTypes(CompNum), 'SolarCollector:FlatPlate:PhotovoltaicThermal') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_PVTSolarCollectorFlatPlate
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_SolarCollector
            IF (LoopSideNum == DemandSide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
            ELSEIF (LoopSideNum == SupplySide) THEN
              TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
            ENDIF
          ELSEIF (SameString(CompTypes(CompNum), 'CentralHeatPumpSystem') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_CentralGroundSourceHeatPump
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_CentralHeatPumpSystem

          !now deal with demand components of the ZoneHVAC type served by ControlCompOutput
          ELSEIF (SameString(CompTypes(CompNum), 'ZoneHVAC:Baseboard:RadiantConvective:Water') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_Baseboard_Rad_Conv_Water
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_ZoneHVACDemand
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'ZoneHVAC:Baseboard:Convective:Water') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_Baseboard_Conv_Water
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_ZoneHVACDemand
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'ZoneHVAC:Baseboard:RadiantConvective:Steam') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_Baseboard_Rad_Conv_Steam
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_ZoneHVACDemand
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'ZoneHVAC:LowTemperatureRadiant:VariableFlow') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_LowTempRadiant_VarFlow
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_ZoneHVACDemand
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'ZoneHVAC:LowTemperatureRadiant:ConstantFlow') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_LowTempRadiant_ConstFlow
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_ZoneHVACDemand
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'AirTerminal:SingleDuct:ConstantVolume:CooledBeam') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_CooledBeamAirTerminal
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_ZoneHVACDemand
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'AirLoopHVAC:UnitaryHeatPump:AirToAir:MultiSpeed') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_MultiSpeedHeatPumpRecovery
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_ZoneHVACDemand
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType  = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'AirLoopHVAC:UnitarySystem') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_UnitarySystemRecovery
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_ZoneHVACDemand
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType  = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'Coil:Heating:WaterToAirHeatPump:EquationFit') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_CoilWAHPHeatingEquationFit
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_DemandCoil
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'Coil:Cooling:WaterToAirHeatPump:EquationFit') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_CoilWAHPCoolingEquationFit
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_DemandCoil
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'Coil:Heating:WaterToAirHeatPump:VariableSpeedEquationFit') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_CoilVSWAHPHeatingEquationFit
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_DemandCoil
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'Coil:Cooling:WaterToAirHeatPump:VariableSpeedEquationFit') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_CoilVSWAHPCoolingEquationFit
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_DemandCoil
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'Coil:Heating:WaterToAirHeatPump:ParameterEstimation') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_CoilWAHPHeatingParamEst
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_DemandCoil
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'Coil:Cooling:WaterToAirHeatPump:ParameterEstimation') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_CoilWAHPCoolingParamEst
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_DemandCoil
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'Refrigeration:Condenser:WaterCooled') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_RefrigSystemWaterCondenser
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Refrigeration
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'Refrigeration:CompressorRack') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_RefrigerationWaterCoolRack
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Refrigeration
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'PlantComponent:UserDefined') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = Typeof_PlantComponentUserDefined
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_PlantComponent
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'Coil:UserDefined') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = Typeof_CoilUserDefined
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_PlantComponent
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'ZoneHVAC:ForcedAir:UserDefined') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_ZoneHVACAirUserDefined
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_PlantComponent
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'AirTerminal:SingleDuct:UserDefined') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_AirTerminalUserDefined
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_PlantComponent
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UnknownStatusOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'PlantComponent:TemperatureSource')) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_WaterSource
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_PlantComponent
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = UncontrolledOpSchemeType
          ELSEIF (SameString(CompTypes(CompNum), 'GroundHeatExchanger:HorizontalTrench') ) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num = TypeOf_GrndHtExchgHorizTrench
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_Pipe
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = TypeOf_GrndHtExchgHorizTrench
          ELSEIF (SameString(CompTypes(CompNum),'Coil:Cooling:DX:SingleSpeed:ThermalStorage')) THEN
            TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_num = TypeOf_PackagedTESCoolingCoil
            TempLoop%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GenEquipTypes_DemandCoil
            TempLoop%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = DemandOpSchemeType
          ELSE
           !discover unsupported equipment on branches.
            CALL ShowSevereError('GetPlantInput: Branch="'//trim(BranchNames(BranchNum))//'", invalid component on branch.')
            CALL ShowContinueError('...invalid component type="'//trim(CompTypes(CompNum))//  &
               '", name="'//trim(CompNames(CompNum))//'".')
!            ErrorsFound=.true.
          ENDIF

          TempLoop%Branch(BranchNum)%Comp(CompNum)%Name        = CompNames(CompNum)
          TempLoop%Branch(BranchNum)%Comp(CompNum)%NodeNameIn  = InletNodeNames(CompNum)
          TempLoop%Branch(BranchNum)%Comp(CompNum)%NodeNumIn   = InletNodeNumbers(CompNum)
          TempLoop%Branch(BranchNum)%Comp(CompNum)%NodeNameOut = OutletNodeNames(CompNum)
          TempLoop%Branch(BranchNum)%Comp(CompNum)%NodeNumOut  = OutletNodeNumbers(CompNum)


           ! Increment pipe counter if component is a pipe
          IF (TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num == TypeOf_Pipe .or. &
              TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num == TypeOf_PipeInterior .or. &
              TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num == TypeOf_PipeExterior .or. &
              TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num == TypeOf_PipeUnderground .or. &
              TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num == TypeOf_PipeSteam) THEN
            NumOfPipesinLoop = NumOfPipesinLoop + 1
            IF(PlantLoop(LoopNum)%TypeOfLoop == Plant) THEN
             NumPlantPipes       = NumPlantPipes + 1
            ELSE IF(PlantLoop(LoopNum)%TypeOfLoop == Condenser) THEN
             NumCondPipes       = NumCondPipes + 1
            END IF
            NumPipes       = NumPipes + 1
          END IF

          TempLoop%Branch(BranchNum)%NodeNumIn = &
             TempLoop%Branch(BranchNum)%Comp(1)%NodeNumIn

             ! find branch outlet node
          TempLoop%Branch(BranchNum)%NodeNumOut = &
             TempLoop%Branch(BranchNum)%Comp(TempLoop%Branch(BranchNum)%TotalComponents)%NodeNumOut

        END DO


        DEALLOCATE(CompTypes)
        DEALLOCATE(CompNames)
        DEALLOCATE(CompCtrls)
        DEALLOCATE(InletNodeNames)
        DEALLOCATE(InletNodeNumbers)
        DEALLOCATE(OutletNodeNames)
        DEALLOCATE(OutletNodeNumbers)

      END DO

      DEALLOCATE(BranchNames)

      IF(ASeriesBranchHasPump .AND. AParallelBranchHasPump) THEN
        CALL ShowSevereError('Current version does not support Loop pumps and branch pumps together')
        CALL ShowContinueError('Occurs in loop '// TRIM(TempLoop%Name))
        ErrorsFound = .TRUE.
!      ELSE IF(.NOT. ASeriesBranchHasPump .AND. .NOT. TempLoop%BranchPump .AND. (LoopSideNum .NE. DemandSide)) THEN
!        CALL ShowSevereError('PlantLoop does not have a pump. A pump is required in each loop')
!        CALL ShowContinueError('Occurs in loop '// TRIM(TempLoop%Name))
!        ErrorsFound = .TRUE.
      END IF

!DSU?      IF(TempLoop%BranchPump) THEN
!        ! This makes sure we aren't trying to run a common pipe simulation with branch pumps
!        IF (PlantLoop(LoopNum)%CommonPipeType .NE. CommonPipe_No) THEN
!           CALL ShowSevereError('Current version cannot simulate a common pipe plant loop with branch pipes.')
!           CALL ShowContinueError('To correct, place supply pump on first branch of supply side.')
!           CALL ShowContinueError('Error occurs in PlantLoop = '//TRIM(PlantLoop(LoopNum)%Name))
!           CALL ShowFatalError('Program terminates due to above conditions.')
!        END IF

        ! Not sure why we can't do steam with branch pumps, but this is moved here from the Demand Calc procedure
!DSU?        IF(PlantLoop(LoopNum)%FluidType==NodeType_Steam) Then
!          !STEAM: Plant Loop Fluid Type is Steam: Calc loop Demand For STEAM cannot be done with branch pumps
!          CALL ShowSevereError('FluidType=STEAM:Branch Pumps cannot be used in a STEAM Loop')
!          CALL ShowContinueError('Occurs in PlantLoop='//TRIM(PlantLoop(LoopNum)%Name))
!          CALL ShowFatalError('Program terminates due to above conditions.')
!        END IF
!      END IF
!DSU? End Pump Checks

           ! Obtain the Splitter and Mixer information
      IF(TempLoop%ConnectList == '') THEN
        NumofSplitters = 0
        NumofMixers = 0
      ELSE
        errflag=.false.
        CALL GetNumSplitterMixerInConntrList(TempLoop%Name,TempLoop%ConnectList,NumofSplitters,NumofMixers,errflag)
        if (errflag) then
          ErrorsFound=.true.
        endif
        if (NumofSplitters /= NumofMixers) then
          CALL ShowSevereError('GetPlantInput: Loop Name='//TRIM(TempLoop%Name)//', ConnectorList='//  &
                   TRIM(TempLoop%ConnectList)//  &
                   ', unequal number of splitters and mixers')
          ErrorsFound=.true.
        endif
      END IF

      IF(NumofSplitters .GT. 0)Then
        TempLoop%SplitterExists = .TRUE.
      Else
        TempLoop%SplitterExists = .FALSE.
      End If

      IF(NumofMixers .GT. 0)Then
        TempLoop%MixerExists = .TRUE.
      Else
        TempLoop%MixerExists = .FALSE.
      End If

      IF (ErrorsFound) THEN
        CALL ShowFatalError('GetPlantInput: Previous Severe errors cause termination.')
      ENDIF

      NumConnectorsInLoop = NumofSplitters + NumofMixers
      ALLOCATE(TempLoop%Splitter(NumofSplitters))
      SplitNum = 1
      DO ConnNum = 1, NumConnectorsInLoop

        IF(SplitNum .GT. NumofSplitters) EXIT
        ALLOCATE(OutletNodeNames(MaxNumAlphas))
        ALLOCATE(OutletNodeNumbers(MaxNumAlphas))
        CALL GetLoopSplitter(TempLoop%Name,TempLoop%ConnectList,    &
                             TempLoop%Splitter(SplitNum)%Name,             &
                             TempLoop%Splitter(SplitNum)%Exists,           &
                             TempLoop%Splitter(SplitNum)%NodeNameIn,       &
                             TempLoop%Splitter(SplitNum)%NodeNumIn,        &
                             TempLoop%Splitter(SplitNum)%TotalOutletNodes, &
                             OutletNodeNames,OutletNodeNumbers,ErrorsFound,ConnNum,SplitNum)

        IF(SplitNum == 1) THEN
          DEALLOCATE(OutletNodeNames)
          DEALLOCATE(OutletNodeNumbers)
          CYCLE
        ENDIF

            ! Map the inlet node to the splitter to a branch number
        IF (TempLoop%Splitter(SplitNum-1)%Exists) THEN
                ! Map the inlet node to the splitter to a branch number
          DO BranchNum = 1, TempLoop%TotalBranches
            CompNum = TempLoop%Branch(BranchNum)%TotalComponents
            IF (TempLoop%Splitter(SplitNum-1)%NodeNumIn == &
                TempLoop%Branch(BranchNum)%Comp(CompNum)%NodeNumOut ) THEN
              TempLoop%Splitter(SplitNum-1)%BranchNumIn = BranchNum
              SplitInBranch=.true.
              EXIT ! BranchNum DO loop
            END IF
          END DO
          IF (.not. SplitInBranch) THEN
            CALL ShowSevereError('Splitter Inlet Branch not found, Splitter='//TRIM(TempLoop%Splitter(SplitNum-1)%Name))
            CALL ShowContinueError('Splitter Branch Inlet name='//TRIM(TempLoop%Splitter(SplitNum-1)%NodeNameIn))
            CALL ShowContinueError('In Loop='//TRIM(TempLoop%Name))
            ErrorsFound=.true.
          ENDIF

          ALLOCATE(TempLoop%Splitter(SplitNum-1)%NodeNameOut(TempLoop%Splitter(SplitNum-1)%TotalOutletNodes))
          TempLoop%Splitter(SplitNum-1)%NodeNameOut=' '
          ALLOCATE(TempLoop%Splitter(SplitNum-1)%NodeNumOut(TempLoop%Splitter(SplitNum-1)%TotalOutletNodes))
          TempLoop%Splitter(SplitNum-1)%NodeNumOut=0
          ALLOCATE(TempLoop%Splitter(SplitNum-1)%BranchNumOut(TempLoop%Splitter(SplitNum-1)%TotalOutletNodes))
          TempLoop%Splitter(SplitNum-1)%BranchNumOut=0

          ALLOCATE(SplitOutBranch(TempLoop%Splitter(SplitNum-1)%TotalOutletNodes))
          SplitOutBranch=.false.
          DO NodeNum = 1, TempLoop%Splitter(SplitNum-1)%TotalOutletNodes
            TempLoop%Splitter(SplitNum-1)%NodeNameOut(NodeNum) = OutletNodeNames(NodeNum)
            TempLoop%Splitter(SplitNum-1)%NodeNumOut(NodeNum)  = OutletNodeNumbers(NodeNum)
              ! The following DO loop series is intended to store the branch number for each outlet
              ! branch of the splitter
            DO BranchNum = 1, TempLoop%TotalBranches
              IF (TempLoop%Splitter(SplitNum-1)%NodeNumOut(NodeNum) == &
                  TempLoop%Branch(BranchNum)%Comp(1)%NodeNumIn ) THEN
                TempLoop%Splitter(SplitNum-1)%BranchNumOut(NodeNum) = BranchNum
                SplitOutBranch(NodeNum)=.true.
                EXIT ! BranchNum DO loop
              END IF
            END DO
          END DO

          DO Outlet = 1,  TempLoop%Splitter(SplitNum-1)%TotalOutletNodes
            IF (SplitOutBranch(Outlet)) CYCLE
            CALL ShowSevereError('Splitter Outlet Branch not found, Splitter='//TRIM(TempLoop%Splitter(SplitNum-1)%Name))
            CALL ShowContinueError('Splitter Branch Outlet node name='//TRIM(TempLoop%Splitter(SplitNum-1)%NodeNameOut(Outlet)))
            CALL ShowContinueError('In Loop='//TRIM(TempLoop%Name))
            CALL ShowContinueError('Loop BranchList='//trim(TempLoop%BranchList))
            CALL ShowContinueError('Loop ConnectorList='//trim(TempLoop%ConnectList))
            ErrorsFound=.true.
          ENDDO

        ENDIF ! Splitter exists
        DEALLOCATE(SplitOutBranch)
        DEALLOCATE(OutletNodeNames)
        DEALLOCATE(OutletNodeNumbers)
      END DO

      ALLOCATE(TempLoop%Mixer(NumofMixers))
      MixNum = 1
      DO ConnNum = 1, NumConnectorsInLoop

        IF(MixNum .GT. NumofMixers) EXIT
        ALLOCATE(InletNodeNames(MaxNumAlphas))
        ALLOCATE(InletNodeNumbers(MaxNumAlphas))
        CALL GetLoopMixer(TempLoop%Name,TempLoop%ConnectList,           &
                          TempLoop%Mixer(MixNum)%Name,            &
                          TempLoop%Mixer(MixNum)%Exists,          &
                          TempLoop%Mixer(MixNum)%NodeNameOut,     &
                          TempLoop%Mixer(MixNum)%NodeNumOut,      &
                          TempLoop%Mixer(MixNum)%TotalInletNodes, &
                          InletNodeNames,InletNodeNumbers,ErrorsFound,ConnNum,MixNum)

        IF(MixNum == 1) THEN
          DEALLOCATE(InletNodeNames)
          DEALLOCATE(InletNodeNumbers)
          CYCLE
        ENDIF
            ! Map the outlet node of the mixer to a branch number
        IF (TempLoop%Mixer(MixNum-1)%Exists) THEN
              ! Map the outlet node of the mixer to a branch number
          MixerOutBranch=.false.
          DO BranchNum = 1, TempLoop%TotalBranches
            IF (TempLoop%Mixer(MixNum-1)%NodeNumOut == &
                TempLoop%Branch(BranchNum)%Comp(1)%NodeNumIn ) THEN
              TempLoop%Mixer(MixNum-1)%BranchNumOut = BranchNum
              MixerOutBranch=.true.
              EXIT ! BranchNum DO loop
            END IF
          END DO
          IF (.not. MixerOutBranch) THEN
            CALL ShowSevereError('Mixer Outlet Branch not found, Mixer='//TRIM(TempLoop%Mixer(MixNum-1)%Name))
            ErrorsFound=.true.
          ENDIF

          ALLOCATE(TempLoop%Mixer(MixNum-1)%NodeNameIn(TempLoop%Mixer(MixNum-1)%TotalInletNodes))
          TempLoop%Mixer(MixNum-1)%NodeNameIn=' '
          ALLOCATE(TempLoop%Mixer(MixNum-1)%NodeNumIn(TempLoop%Mixer(MixNum-1)%TotalInletNodes))
          TempLoop%Mixer(MixNum-1)%NodeNumIn=0
          ALLOCATE(TempLoop%Mixer(MixNum-1)%BranchNumIn(TempLoop%Mixer(MixNum-1)%TotalInletNodes))
          TempLoop%Mixer(MixNum-1)%BranchNumIn=0

          ALLOCATE(MixerInBranch(TempLoop%Mixer(MixNum-1)%TotalInletNodes))
          MixerInBranch=.false.
          DO NodeNum = 1, TempLoop%Mixer(MixNum-1)%TotalInletNodes
            TempLoop%Mixer(MixNum-1)%NodeNameIn(NodeNum) = InletNodeNames(NodeNum)
            TempLoop%Mixer(MixNum-1)%NodeNumIn(NodeNum)  = InletNodeNumbers(NodeNum)
              ! The following DO loop series is intended to store the branch number for each inlet
              ! branch of the mixer
            DO BranchNum = 1, TempLoop%TotalBranches
              CompNum = TempLoop%Branch(BranchNum)%TotalComponents
              IF (TempLoop%Mixer(MixNum-1)%NodeNumIn(NodeNum) == &
                  TempLoop%Branch(BranchNum)%Comp(CompNum)%NodeNumOut ) THEN
                TempLoop%Mixer(MixNum-1)%BranchNumIn(NodeNum) = BranchNum
                MixerInBranch(NodeNum)=.true.
                EXIT ! BranchNum DO loop
              END IF
            END DO
          END DO

          DO Inlet = 1,  TempLoop%Mixer(MixNum-1)%TotalInletNodes
            IF (MixerInBranch(Inlet)) CYCLE
            CALL ShowSevereError('Mixer Inlet Branch not found, Mixer='//TRIM(TempLoop%Mixer(MixNum-1)%Name))
            CALL ShowContinueError('Mixer Branch Inlet name='//TRIM(TempLoop%Mixer(MixNum-1)%NodeNameIn(Inlet)))
            CALL ShowContinueError('In Loop='//TRIM(TempLoop%Name))
            CALL ShowContinueError('Loop BranchList='//trim(TempLoop%BranchList))
            CALL ShowContinueError('Loop ConnectorList='//trim(TempLoop%ConnectList))
            ErrorsFound=.true.
          ENDDO

        ENDIF ! Mixer exists
        DEALLOCATE(MixerInBranch)
        DEALLOCATE(InletNodeNames)
        DEALLOCATE(InletNodeNumbers)
      END DO

      IF(NumOfPipesInLoop .GT. 0) THEN
        PipeNum = 0
        ALLOCATE (LoopPipe(HalfLoopNum)%Pipe(NumOfPipesInLoop))
        DO BranchNum = 1, TempLoop%TotalBranches
          DO CompNum = 1, TempLoop%Branch(BranchNum)%TotalComponents
            IF (TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num == TypeOf_Pipe .or. &
                TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num == TypeOf_PipeSteam .or. &
                TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num == TypeOf_PipeInterior .or. &
                TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num == TypeOf_PipeUnderground .or. &
                TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num == TypeOf_PipeExterior) THEN

              PipeNum = PipeNum + 1
              IF (PipeNum > NumOfPipesInLoop) CALL ShowFatalError('Pipe counting problem in GetPlantSideLoops')

              LoopPipe(HalfLoopNum)%NumPipes                  = NumOfPipesInLoop
              LoopPipe(HalfLoopNum)%Pipe(PipeNum)%Name        = TempLoop%Branch(BranchNum)%Comp(CompNum)%Name
              IF(TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num == TypeOf_Pipe) THEN
               LoopPipe(HalfLoopNum)%Pipe(PipeNum)%Typeof = TypeOf_Pipe
              ELSE IF(TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num == TypeOf_PipeSteam) THEN
               LoopPipe(HalfLoopNum)%Pipe(PipeNum)%Typeof = TypeOf_PipeSteam
              ELSE IF(TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num == TypeOf_PipeInterior) THEN
               LoopPipe(HalfLoopNum)%Pipe(PipeNum)%Typeof = TypeOf_PipeInterior
              ELSE IF(TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num == TypeOf_PipeExterior) THEN
               LoopPipe(HalfLoopNum)%Pipe(PipeNum)%Typeof = TypeOf_PipeExterior
              ELSE IF(TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num == TypeOf_PipeUnderground) THEN
               LoopPipe(HalfLoopNum)%Pipe(PipeNum)%Typeof = TypeOf_PipeUnderground
              END IF
              LoopPipe(HalfLoopNum)%Pipe(PipeNum)%NodeNameIn  = TempLoop%Branch(BranchNum)%Comp(CompNum)%NodeNameIn
              LoopPipe(HalfLoopNum)%Pipe(PipeNum)%NodeNumIn   = TempLoop%Branch(BranchNum)%Comp(CompNum)%NodeNumIn
              LoopPipe(HalfLoopNum)%Pipe(PipeNum)%NodeNameOut = TempLoop%Branch(BranchNum)%Comp(CompNum)%NodeNameOut
              LoopPipe(HalfLoopNum)%Pipe(PipeNum)%NodeNumOut  = TempLoop%Branch(BranchNum)%Comp(CompNum)%NodeNumOut

              IF (TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num == TypeOf_Pipe .or.  &
                  TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num == TypeOf_PipeSteam) THEN
!                Call InitializePipes(TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num,  &
!                            LoopPipe(HalfLoopNum)%Pipe(PipeNum)%Name,  &
!                            TempLoop%Branch(BranchNum)%Comp(CompNum)%CompNum, &
!                            0.0d0)
              ELSEIF (TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num == TypeOf_PipeInterior .or.  &
                      TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num == TypeOf_PipeUnderground .or. &
                      TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num == TypeOf_PipeExterior) THEN
                Call InitializeHeatTransferPipes(TempLoop%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num,  &
                            LoopPipe(HalfLoopNum)%Pipe(PipeNum)%Name,  &
                            TempLoop%Branch(BranchNum)%Comp(CompNum)%CompNum)
              ENDIF
            END IF
          END DO
        END DO
      END IF

      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%SplitterExists = TempLoop%SplitterExists
      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%MixerExists = TempLoop%MixerExists
      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%ByPassExists = TempLoop%ByPassExists

      ALLOCATE(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(TempLoop%TotalBranches))
      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalBranches = TempLoop%TotalBranches
      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch = TempLoop%Branch

      ALLOCATE(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter(NumofSplitters))
      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%NumSplitters = NumofSplitters
      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Splitter = TempLoop%Splitter

      ALLOCATE(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Mixer(NumofMixers))
      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%NumMixers = NumofMixers
      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Mixer    = TempLoop%Mixer

  !   Add condenser CASE statement when required.

      DEALLOCATE(TempLoop%Branch)
      DEALLOCATE(TempLoop%Splitter)
      DEALLOCATE(TempLoop%Mixer)

    END DO  ! ... end LoopSideNum=DemandSide,SupplySide

    PlantLoop(LoopNum)%LoopHasConnectionComp = TempLoop%LoopHasConnectionComp

     ! CR 7883 check for missing demand side pump if common pipe set.
    IF ( (PlantLoop(LoopNum)%CommonPipeType /= CommonPipe_No) .AND. (.NOT. DemandSideHasPump) ) THEN
      CALL ShowSevereError('Input Error: a common pipe arrangement was selected but there is no pump for the secondary loop.')
      CALL ShowContinueError('Occurs in PlantLoop = '//TRIM(TempLoop%Name))
      CALL ShowContinueError('Add a pump to the demand side of this plant loop.')
      ErrorsFound=.true.
    ENDIF

  END DO    ! ...end of demand side loops DO loop

  ALLOCATE (Pipe(NumPipes))  ! Pipe definition in DataPlant
  SysPipeNum=0

  DO HalfLoopNum = 1, NumHalfLoops
    DO PipeNum = 1, LoopPipe(HalfLoopNum)%NumPipes
      SysPipeNum = SysPipeNum + 1
      IF(MOD(HalfLoopNum,2) .NE. 0) THEN
        Pipe(SysPipeNum)%ParentHalfLoop = DemandSide
      ELSE
        Pipe(SysPipeNum)%ParentHalfLoop = SupplySide
      END IF
       Pipe(SysPipeNum)%Name = LoopPipe(HalfLoopNum)%Pipe(PipeNum)%Name
       Pipe(SysPipeNum)%Typeof = LoopPipe(HalfLoopNum)%Pipe(PipeNum)%Typeof
       Pipe(SysPipeNum)%NodeNameIn  = LoopPipe(HalfLoopNum)%Pipe(PipeNum)%NodeNameIn
       Pipe(SysPipeNum)%NodeNumIn   = LoopPipe(HalfLoopNum)%Pipe(PipeNum)%NodeNumIn
       Pipe(SysPipeNum)%NodeNameOut = LoopPipe(HalfLoopNum)%Pipe(PipeNum)%NodeNameOut
       Pipe(SysPipeNum)%NodeNumOut  = LoopPipe(HalfLoopNum)%Pipe(PipeNum)%NodeNumOut
    END DO
  END DO

  DEALLOCATE(LoopPipe)

!DSU? can we clean this out this next do loop now? looks like bandaids.
  DO LoopNum = 1, TotNumLoops
    DO BranchNum=1, PlantLoop(LoopNum)%LoopSide(SupplySide)%TotalBranches
      DO CompNum = 1, PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%TotalComponents
        Pos = Index(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf, ":")
        GeneralEquipType=FindItemInList(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf(1:Pos-1), &
                           GeneralEquipTypes,NumGeneralEquipTypes)
        IF (GeneralEquipType == 0) THEN
          IF (SameString(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf(1:13),  &
                         'HeaderedPumps')) THEN
            GeneralEquipType=GenEquipTypes_Pump
          ELSEIF (SameString(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf,  &
                             'WaterHeater:HeatPump')) THEN
            GeneralEquipType=GenEquipTypes_WaterThermalTank
          ELSEIF (SameString(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf,  &
                             'TemperingValve')) THEN
            GeneralEquipType=GenEquipTypes_Valve
          ELSEIF (SameString(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf(1:14),  &
                             'Pipe:Adiabatic')) THEN
            GeneralEquipType=GenEquipTypes_Pipe
          ELSEIF (SameString(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf(1:12),  &
                             'PipingSystem')) THEN
            GeneralEquipType=GenEquipTypes_Pipe
          ELSEIF (SameString(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf(1:33), &
                            'Thermalstorage:ChilledWater:Mixed')) THEN
            GeneralEquipType=GenEquipTypes_ThermalStorage
          ELSEIF (SameString(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf(1:38), &
                            'Thermalstorage:ChilledWater:Stratified')) THEN
            GeneralEquipType=GenEquipTypes_ThermalStorage
          ELSEIF (SameString(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf, &
                            'ChillerHeater:Absorption:DirectFired')) THEN
            GeneralEquipType=GenEquipTypes_Chiller
          ELSEIF (SameString(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf, &
                            'ChillerHeater:Absorption:DoubleEffect')) THEN
            GeneralEquipType=GenEquipTypes_Chiller
          ELSEIF (SameString(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf(1:8), &
                            'District')) THEN
            GeneralEquipType=GenEquipTypes_Purchased
          ELSEIF (SameString(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf, &
                            'GroundHeatExchanger:Vertical')) THEN
            GeneralEquipType=GenEquipTypes_GroundHeatExchanger
          ELSEIF (SameString(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf, &
                            'GroundHeatExchanger:Surface')) THEN
            GeneralEquipType=GenEquipTypes_GroundHeatExchanger
          ELSEIF (SameString(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf, &
                            'GroundHeatExchanger:Pond')) THEN
            GeneralEquipType=GenEquipTypes_GroundHeatExchanger
          ELSEIF (SameString(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf, &
                            'PlantComponent:TemperatureSource')) THEN
            GeneralEquipType=GenEquipTypes_HeatExchanger
          ELSEIF (SameString(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf, &
                            'CENTRALHEATPUMPSYSTEM')) THEN
            GeneralEquipType=GenEquipTypes_CentralHeatPumpSystem

          ELSE
            CALL ShowSevereError('GetPlantInput: PlantLoop="'//trim(PlantLoop(LoopNum)%Name)//'" invalid equipment type.')
            CALL ShowContinueError('...on Branch="'//trim(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Name)//'".')
            CALL ShowContinueError('...Equipment type="'//  &
                 TRIM(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf)//'".')
            CALL ShowContinueError('...Equipment name="'//  &
                 TRIM(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%Name)//'".')
            ErrorsFound=.true.
          ENDIF
        ENDIF

        PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%GeneralEquipType = GeneralEquipType

        ! Set up "TypeOf" Num
        TypeOfNum=FindItemInList(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf,  &
                           SimPlantEquipTypes,NumSimPlantEquipTypes)
        IF (TypeOfNum == 0) THEN
          IF (.not. SameString(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf(1:4),'Pump') .AND. &
              .not. SameString(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf(1:12),  &
                               'HeaderedPump')) THEN
            ! Error.  May have already been flagged under General
            IF (GeneralEquipType /= 0) THEN  ! if GeneralEquipmentType == 0, then already flagged
              CALL ShowSevereError('GetPlantInput: PlantLoop="'//trim(PlantLoop(LoopNum)%Name)//'" invalid equipment type.')
              CALL ShowContinueError('...on Branch="'//trim(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Name)//'".')
              CALL ShowContinueError('...Equipment type="'//  &
                   TRIM(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf)//'".')
              CALL ShowContinueError('...Equipment name="'//  &
                   TRIM(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%Name)//'".')
              ErrorsFound=.true.
            ENDIF
          ENDIF
        ELSE
          PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num = TypeOfNum
        ENDIF

      END DO
    END DO
  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetPlantInput: Errors in getting PlantLoop Input')
  ENDIF

  IF (NumPlantLoops > 0) ALLOCATE(VentRepPlantSupplySide(NumPlantLoops))
  IF (NumPlantLoops > 0) ALLOCATE(VentRepPlantDemandSide(NumPlantLoops))

  DO LoopNum = 1, NumPlantLoops

    VentRepPlantSupplySide(LoopNum)%Name          = PlantLoop(LoopNum)%Name
    VentRepPlantSupplySide(LoopNum)%NodeNumIn     = PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumIn
    VentRepPlantSupplySide(LoopNum)%NodeNameIn    = PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNameIn
    VentRepPlantSupplySide(LoopNum)%NodeNumOut    = PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumOut
    VentRepPlantSupplySide(LoopNum)%NodeNameOut   = PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNameOut
    VentRepPlantSupplySide(LoopNum)%TotalBranches = PlantLoop(LoopNum)%LoopSide(SupplySide)%TotalBranches
    IF (VentRepPlantSupplySide(LoopNum)%TotalBranches > 0) &
      ALLOCATE (VentRepPlantSupplySide(LoopNum)%Branch(VentRepPlantSupplySide(LoopNum)%TotalBranches))

    DO BranchNum = 1, VentRepPlantSupplySide(LoopNum)%TotalBranches
      VentRepPlantSupplySide(LoopNum)%Branch(BranchNum)%Name            = &
             PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Name
      VentRepPlantSupplySide(LoopNum)%Branch(BranchNum)%NodeNumIn       = &
             PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%NodeNumIn
      VentRepPlantSupplySide(LoopNum)%Branch(BranchNum)%NodeNumOut      = &
             PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%NodeNumOut
      VentRepPlantSupplySide(LoopNum)%Branch(BranchNum)%TotalComponents = &
             PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%TotalComponents
      IF (VentRepPlantSupplySide(LoopNum)%Branch(BranchNum)%TotalComponents > 0) THEN
        TotCompsOnBranch = VentRepPlantSupplySide(LoopNum)%Branch(BranchNum)%TotalComponents
        ALLOCATE (VentRepPlantSupplySide(LoopNum)%Branch(BranchNum)%Comp(TotCompsOnBranch))
      END IF

      DO CompNum = 1, VentRepPlantSupplySide(LoopNum)%Branch(BranchNum)%TotalComponents

        VentRepPlantSupplySide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%Name        = &
               PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%Name
        VentRepPlantSupplySide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf      = &
               PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf
        VentRepPlantSupplySide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNameIn  = &
               PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%NodeNameIn
        VentRepPlantSupplySide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNameOut = &
               PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%NodeNameOut
        VentRepPlantSupplySide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn   = &
               PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn
        VentRepPlantSupplySide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumOut  = &
               PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%NodeNumOut

      END DO    ! loop over components in branches on the loop (ventilation report data)

    END DO      ! loop over branches on the loop (ventilation report data)

    VentRepPlantDemandSide(LoopNum)%Name          = PlantLoop(LoopNum)%Name
    VentRepPlantDemandSide(LoopNum)%NodeNumIn     = PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNumIn
    VentRepPlantDemandSide(LoopNum)%NodeNameIn    = PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNameIn
    VentRepPlantDemandSide(LoopNum)%NodeNumOut    = PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNumOut
    VentRepPlantDemandSide(LoopNum)%NodeNameOut   = PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNameOut
    VentRepPlantDemandSide(LoopNum)%TotalBranches = PlantLoop(LoopNum)%LoopSide(DemandSide)%TotalBranches

    IF (VentRepPlantDemandSide(LoopNum)%TotalBranches > 0) &
      ALLOCATE (VentRepPlantDemandSide(LoopNum)%Branch(VentRepPlantDemandSide(LoopNum)%TotalBranches))

    DO BranchNum = 1, VentRepPlantDemandSide(LoopNum)%TotalBranches
      VentRepPlantDemandSide(LoopNum)%Branch(BranchNum)%Name            = &
             PlantLoop(LoopNum)%LoopSide(DemandSide)%Branch(BranchNum)%Name
      VentRepPlantDemandSide(LoopNum)%Branch(BranchNum)%NodeNumIn       = &
             PlantLoop(LoopNum)%LoopSide(DemandSide)%Branch(BranchNum)%NodeNumIn
      VentRepPlantDemandSide(LoopNum)%Branch(BranchNum)%NodeNumOut      = &
             PlantLoop(LoopNum)%LoopSide(DemandSide)%Branch(BranchNum)%NodeNumOut
      VentRepPlantDemandSide(LoopNum)%Branch(BranchNum)%TotalComponents = &
             PlantLoop(LoopNum)%LoopSide(DemandSide)%Branch(BranchNum)%TotalComponents
      IF (VentRepPlantDemandSide(LoopNum)%Branch(BranchNum)%TotalComponents > 0) THEN
        TotCompsOnBranch = VentRepPlantDemandSide(LoopNum)%Branch(BranchNum)%TotalComponents
        ALLOCATE (VentRepPlantDemandSide(LoopNum)%Branch(BranchNum)%Comp(TotCompsOnBranch))
      END IF

      DO CompNum = 1, VentRepPlantDemandSide(LoopNum)%Branch(BranchNum)%TotalComponents

        VentRepPlantDemandSide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%Name        = &
               PlantLoop(LoopNum)%LoopSide(DemandSide)%Branch(BranchNum)%Comp(CompNum)%Name
        VentRepPlantDemandSide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf      = &
               PlantLoop(LoopNum)%LoopSide(DemandSide)%Branch(BranchNum)%Comp(CompNum)%TypeOf
        VentRepPlantDemandSide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNameIn  = &
               PlantLoop(LoopNum)%LoopSide(DemandSide)%Branch(BranchNum)%Comp(CompNum)%NodeNameIn
        VentRepPlantDemandSide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNameOut = &
               PlantLoop(LoopNum)%LoopSide(DemandSide)%Branch(BranchNum)%Comp(CompNum)%NodeNameOut
        VentRepPlantDemandSide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn   = &
               PlantLoop(LoopNum)%LoopSide(DemandSide)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn
        VentRepPlantDemandSide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumOut  = &
               PlantLoop(LoopNum)%LoopSide(DemandSide)%Branch(BranchNum)%Comp(CompNum)%NodeNumOut

      END DO    ! loop over components in branches on the loop (ventilation report data)

    END DO      ! loop over branches on the loop (ventilation report data)

  END DO        ! loop over plant supply loops (ventilation report data)

  IF (NumCondLoops > 0) ALLOCATE(VentRepCondSupplySide(NumCondLoops))
  IF (NumCondLoops > 0) ALLOCATE(VentRepCondDemandSide(NumCondLoops))

   DO LoopNum = 1, NumCondLoops
    LoopNumInArray = LoopNum + NumPlantLoops

    VentRepCondSupplySide(LoopNum)%Name          = PlantLoop(LoopNumInArray)%Name
    VentRepCondSupplySide(LoopNum)%NodeNumIn     = PlantLoop(LoopNumInArray)%LoopSide(SupplySide)%NodeNumIn
    VentRepCondSupplySide(LoopNum)%NodeNameIn    = PlantLoop(LoopNumInArray)%LoopSide(SupplySide)%NodeNameIn
    VentRepCondSupplySide(LoopNum)%NodeNumOut    = PlantLoop(LoopNumInArray)%LoopSide(SupplySide)%NodeNumOut
    VentRepCondSupplySide(LoopNum)%NodeNameOut   = PlantLoop(LoopNumInArray)%LoopSide(SupplySide)%NodeNameOut
    VentRepCondSupplySide(LoopNum)%TotalBranches = PlantLoop(LoopNumInArray)%LoopSide(SupplySide)%TotalBranches
    IF (VentRepCondSupplySide(LoopNum)%TotalBranches > 0) &
      ALLOCATE (VentRepCondSupplySide(LoopNum)%Branch(VentRepCondSupplySide(LoopNum)%TotalBranches))

    DO BranchNum = 1, VentRepCondSupplySide(LoopNum)%TotalBranches
      VentRepCondSupplySide(LoopNum)%Branch(BranchNum)%Name            = &
             PlantLoop(LoopNumInArray)%LoopSide(SupplySide)%Branch(BranchNum)%Name
      VentRepCondSupplySide(LoopNum)%Branch(BranchNum)%NodeNumIn       = &
             PlantLoop(LoopNumInArray)%LoopSide(SupplySide)%Branch(BranchNum)%NodeNumIn
      VentRepCondSupplySide(LoopNum)%Branch(BranchNum)%NodeNumOut      = &
             PlantLoop(LoopNumInArray)%LoopSide(SupplySide)%Branch(BranchNum)%NodeNumOut
      VentRepCondSupplySide(LoopNum)%Branch(BranchNum)%TotalComponents = &
             PlantLoop(LoopNumInArray)%LoopSide(SupplySide)%Branch(BranchNum)%TotalComponents
      IF (VentRepCondSupplySide(LoopNum)%Branch(BranchNum)%TotalComponents > 0) THEN
        TotCompsOnBranch = VentRepCondSupplySide(LoopNum)%Branch(BranchNum)%TotalComponents
        ALLOCATE (VentRepCondSupplySide(LoopNum)%Branch(BranchNum)%Comp(TotCompsOnBranch))
      END IF

      DO CompNum = 1, VentRepCondSupplySide(LoopNum)%Branch(BranchNum)%TotalComponents

        VentRepCondSupplySide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%Name        = &
               PlantLoop(LoopNumInArray)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%Name
        VentRepCondSupplySide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf      = &
               PlantLoop(LoopNumInArray)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf
        VentRepCondSupplySide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNameIn  = &
               PlantLoop(LoopNumInArray)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%NodeNameIn
        VentRepCondSupplySide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNameOut = &
               PlantLoop(LoopNumInArray)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%NodeNameOut
        VentRepCondSupplySide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn   = &
               PlantLoop(LoopNumInArray)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn
        VentRepCondSupplySide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumOut  = &
               PlantLoop(LoopNumInArray)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%NodeNumOut

      END DO    ! loop over components in branches on the loop (ventilation report data)

    END DO      ! loop over branches on the loop (ventilation report data)

    VentRepCondDemandSide(LoopNum)%Name          = PlantLoop(LoopNumInArray)%Name
    VentRepCondDemandSide(LoopNum)%NodeNumIn     = PlantLoop(LoopNumInArray)%LoopSide(DemandSide)%NodeNumIn
    VentRepCondDemandSide(LoopNum)%NodeNameIn    = PlantLoop(LoopNumInArray)%LoopSide(DemandSide)%NodeNameIn
    VentRepCondDemandSide(LoopNum)%NodeNumOut    = PlantLoop(LoopNumInArray)%LoopSide(DemandSide)%NodeNumOut
    VentRepCondDemandSide(LoopNum)%NodeNameOut   = PlantLoop(LoopNumInArray)%LoopSide(DemandSide)%NodeNameOut
    VentRepCondDemandSide(LoopNum)%TotalBranches = PlantLoop(LoopNumInArray)%LoopSide(DemandSide)%TotalBranches
    IF (VentRepCondDemandSide(LoopNum)%TotalBranches > 0) &
      ALLOCATE (VentRepCondDemandSide(LoopNum)%Branch(VentRepCondDemandSide(LoopNum)%TotalBranches))

    DO BranchNum = 1, VentRepCondDemandSide(LoopNum)%TotalBranches
      VentRepCondDemandSide(LoopNum)%Branch(BranchNum)%Name            = &
             PlantLoop(LoopNumInArray)%LoopSide(DemandSide)%Branch(BranchNum)%Name
      VentRepCondDemandSide(LoopNum)%Branch(BranchNum)%NodeNumIn       = &
             PlantLoop(LoopNumInArray)%LoopSide(DemandSide)%Branch(BranchNum)%NodeNumIn
      VentRepCondDemandSide(LoopNum)%Branch(BranchNum)%NodeNumOut      = &
             PlantLoop(LoopNumInArray)%LoopSide(DemandSide)%Branch(BranchNum)%NodeNumOut
      VentRepCondDemandSide(LoopNum)%Branch(BranchNum)%TotalComponents = &
             PlantLoop(LoopNumInArray)%LoopSide(DemandSide)%Branch(BranchNum)%TotalComponents
      IF (VentRepCondDemandSide(LoopNum)%Branch(BranchNum)%TotalComponents > 0) THEN
        TotCompsOnBranch = VentRepCondDemandSide(LoopNum)%Branch(BranchNum)%TotalComponents
        ALLOCATE (VentRepCondDemandSide(LoopNum)%Branch(BranchNum)%Comp(TotCompsOnBranch))
      END IF

      DO CompNum = 1, VentRepCondDemandSide(LoopNum)%Branch(BranchNum)%TotalComponents

        VentRepCondDemandSide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%Name        = &
               PlantLoop(LoopNumInArray)%LoopSide(DemandSide)%Branch(BranchNum)%Comp(CompNum)%Name
        VentRepCondDemandSide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf      = &
               PlantLoop(LoopNumInArray)%LoopSide(DemandSide)%Branch(BranchNum)%Comp(CompNum)%TypeOf
        VentRepCondDemandSide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNameIn  = &
               PlantLoop(LoopNumInArray)%LoopSide(DemandSide)%Branch(BranchNum)%Comp(CompNum)%NodeNameIn
        VentRepCondDemandSide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNameOut = &
               PlantLoop(LoopNumInArray)%LoopSide(DemandSide)%Branch(BranchNum)%Comp(CompNum)%NodeNameOut
        VentRepCondDemandSide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn   = &
               PlantLoop(LoopNumInArray)%LoopSide(DemandSide)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn
        VentRepCondDemandSide(LoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumOut  = &
               PlantLoop(LoopNumInArray)%LoopSide(DemandSide)%Branch(BranchNum)%Comp(CompNum)%NodeNumOut

      END DO    ! loop over components in branches on the loop (ventilation report data)

    END DO      ! loop over branches on the loop (ventilation report data)

   END DO        ! loop over plant supply loops (ventilation report data)

  RETURN

END SUBROUTINE GetPlantInput

SUBROUTINE SetupReports

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   July 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes the plant supply side reports.
          ! It was created during the splitting of supply and demand side functions.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE DataPlant,  ONLY: PlantReport, PlantLoop, DemandSide, SupplySide, DemandOpSchemeType
  USE DataGlobals, ONLY: DisplayAdvancedReportVariables

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
  INTEGER :: LoopNum        ! DO loop counter (plant supply sides)
  INTEGER :: LoopSideNum
  INTEGER :: BranchNum
  INTEGER :: CompNum
  INTEGER :: MaxBranches    ! Maximum number of branches on any plant loop (used for allocating arrays)
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.
  INTEGER :: FluidIndex

          ! FLOW:
  MaxBranches =0
  DO LoopNum=1,TotNumLoops
    MaxBranches=MAX(MaxBranches,PlantLoop(LoopNum)%LoopSide(DemandSide)%TotalBranches)
    MaxBranches=MAX(MaxBranches,PlantLoop(LoopNum)%LoopSide(SupplySide)%TotalBranches)
    PlantLoop(LoopNum)%MaxBranch = MaxBranches
  END DO

  ALLOCATE (PlantReport(TotNumLoops))

  PlantReport%CoolingDemand         = 0.d0
  PlantReport%HeatingDemand         = 0.d0
  PlantReport%DemandNotDispatched   = 0.d0
  PlantReport%UnmetDemand           = 0.d0
  PlantReport%InletNodeTemperature  = 0.d0
  PlantReport%OutletNodeTemperature = 0.d0
  PlantReport%InletNodeFlowrate     = 0.d0
  PlantReport%BypassFrac            = 0.d0
  PlantReport%OutletNodeFlowrate    = 0.d0

  DO LoopNum =1, TotNumLoops
   IF(LoopNum .LE. NumPlantLoops) THEN
    CurrentModuleObject = 'Plant Loop'
   ELSE
    CurrentModuleObject = 'Cond Loop'
   END IF
   ! CurrentModuleObject='Plant/Condenser Loop'
    CALL SetupOutputVariable('Plant Supply Side Cooling Demand Rate [W]', &
           PlantReport(LoopNum)%CoolingDemand,'System','Average',PlantLoop(LoopNum)%Name)
    CALL SetupOutputVariable('Plant Supply Side Heating Demand Rate [W]', &
           PlantReport(LoopNum)%HeatingDemand,'System','Average',PlantLoop(LoopNum)%Name)
    CALL SetupOutputVariable('Plant Supply Side Inlet Mass Flow Rate [kg/s]', &
           PlantReport(LoopNum)%InletNodeFlowrate,'System','Average',PlantLoop(LoopNum)%Name)

    CALL SetupOutputVariable('Plant Supply Side Inlet Temperature [C]', &
           PlantReport(LoopNum)%InletNodeTemperature,'System','Average',PlantLoop(LoopNum)%Name)
    CALL SetupOutputVariable('Plant Supply Side Outlet Temperature [C]', &
           PlantReport(LoopNum)%OutletNodeTemperature,'System','Average',PlantLoop(LoopNum)%Name)

    CALL SetupOutputVariable('Plant Supply Side Not Distributed Demand Rate [W]', &
           PlantReport(LoopNum)%DemandNotDispatched,'System','Average',PlantLoop(LoopNum)%Name)
    CALL SetupOutputVariable('Plant Supply Side Unmet Demand Rate [W]', &
           PlantReport(LoopNum)%UnmetDemand,'System','Average',PlantLoop(LoopNum)%Name)

          ! Debug variables -- used by OSU developers
    CALL SetupOutputVariable('Debug Plant Loop Bypass Fraction []', &
           PlantReport(LoopNum)%BypassFrac,'System','Average',PlantLoop(LoopNum)%Name)
!    CALL SetupOutputVariable('Debug SSInletNode Flowrate[kg/s]', &
!           PlantReport(LoopNum)%InletNodeFlowrate,'System','Average',PlantLoop(LoopNum)%Name)
!    CALL SetupOutputVariable('Debug SSInletNode Temperature[C]', &
!           PlantReport(LoopNum)%InletNodeTemperature,'System','Average',PlantLoop(LoopNum)%Name)
!    CALL SetupOutputVariable('Debug SSOutletNode Flowrate [kg/s]', &
!           PlantReport(LoopNum)%OutletNodeFlowrate,'System','Average',PlantLoop(LoopNum)%Name)
!    CALL SetupOutputVariable('Debug SSOutletNode Temperature[C]', &
!           PlantReport(LoopNum)%OutletNodeTemperature,'System','Average',PlantLoop(LoopNum)%Name)
    CALL SetupOutputVariable('Debug Plant Last Simulated Loop Side []', &
           PlantReport(LoopNum)%LastLoopSideSimulated, 'System', 'Average', PlantLoop(LoopNum)%Name)
  END DO


  ! setup more variables inside plant data structure
   ! CurrentModuleObject='Plant/Condenser Loop(Advanced)'
  IF (DisplayAdvancedReportVariables) THEN
    DO LoopNum =1, TotNumLoops
      CALL SetupOutputVariable('Plant Demand Side Lumped Capacitance Temperature [C]', &
                               PlantLoop(LoopNum)%LoopSide(DemandSide)%LoopSideInlet_TankTemp, &
                               'System', 'Average', PlantLoop(LoopNum)%Name)
      CALL SetupOutputVariable('Plant Supply Side Lumped Capacitance Temperature [C]', &
                               PlantLoop(LoopNum)%LoopSide(SupplySide)%LoopSideInlet_TankTemp, &
                               'System', 'Average', PlantLoop(LoopNum)%Name)
      DO LoopSideNum = DemandSide, SupplySide
        DO BranchNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalBranches
          DO CompNum = 1,  PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%TotalComponents
            IF ( PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType /=   &
                 DemandOpSchemeType ) THEN
              CALL SetupOutputVariable('Plant Component Distributed Demand Rate [W]', &
                  PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad, &
                  'System', 'Average', &
                  PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Name)
            ENDIF
          ENDDO
        ENDDO
      ENDDO
    ENDDO
  ENDIF

  ! now traverse plant loops and set fluid type index in all nodes on the loop
  DO LoopNum =1, TotNumLoops
    FluidIndex = PlantLoop(LoopNum)%FluidIndex
    DO LoopSideNum = DemandSide, SupplySide
      Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%NodeNumIn)%FluidIndex  = FluidIndex
      Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%NodeNumOut)%FluidIndex = FluidIndex
      DO BranchNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalBranches
        DO CompNum = 1,  PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%TotalComponents
          Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn )%FluidIndex = FluidIndex
          Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumOut)%FluidIndex = FluidIndex
        ENDDO
      ENDDO
    ENDDO
  ENDDO ! plant loops

  RETURN

END SUBROUTINE SetupReports

SUBROUTINE InitializeLoops(FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Sankaranarayanan K P
          !       DATE WRITTEN   May 2005
          !       MODIFIED       Dan Fisher Aug. 2008
          !                      Brent Griffith May 2009 EMS setpoint check
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes the
          ! Plant loop nodes one time at the beginning of the simulation.
          ! It also reinitializes loop temperatures if loop setpoint
          ! temperature changes. Branch levels for all branches are also set.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE DataEnvironment, ONLY: StdBaroPress
  USE DataSizing
  USE PlantLoopEquip,     ONLY : SimPlantEquip
  USE General,         ONLY: RoundSigDigits
  USE EMSManager,      ONLY: iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS, &
                             iTemperatureMaxSetpoint, iTemperatureMinSetpoint
  USE PlantUtilities, ONLY: SetAllFlowLocks
  USE DataHVACGlobals, ONLY : NumPlantLoops, NumCondLoops
  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN):: FirstHVACIteration             ! true if first iteration of the simulation

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64),PARAMETER::StartQuality = 1.0d0
  REAL(r64),PARAMETER::StartHumRat  = 0.0d0


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER            :: LoopNum                    ! plant loop counter
  INTEGER            :: LoopSideNum
  INTEGER            :: BranchNum                  ! branch loop counter
  INTEGER            :: CompNum                    ! plant side component counter
  INTEGER            :: SensedNode

  REAL(r64)          :: LoopSetPointTemp           ! the loop control or setpoint temperature

  LOGICAL            :: ErrorsFound=.false.
  LOGICAL            :: FinishSizingFlag

  LOGICAL,SAVE  :: SupplyEnvrnFlag = .TRUE.
!  LOGICAL,SAVE  :: MySizeFlag = .TRUE.
  LOGICAL,SAVE  :: MySetPointCheckFlag = .TRUE.

  LOGICAL,SAVE,DIMENSION(:),ALLOCATABLE :: PlantLoopSetPointInitFlag

  INTEGER             :: HalfLoopNum
  INTEGER             :: passNum


  IF (.NOT. ALLOCATED (PlantLoopSetPointInitFlag)) THEN
    ALLOCATE ( PlantLoopSetPointInitFlag(TotNumLoops))
  ENDIF


! Initialize the setpoints  for Load range based schemes only as determined by the init flag
! The input already requires a loop setpoint.  The plantloop object requires
! specification of a loop node and corresponding setpoint manager.  Using a 'component setpoint'
! control scheme does NOT eliminate the requirement for a plant loop setpoint.  So there is
! already the possibility that a component setpoint controlled object on the loop outlet
! branch would have the same setpoint node as the loop.  I don't think setpoint manager traps
! for this user input error, but it might.  Since both loop and component setpoints already
! peacefully coexist on the loop, we can allow the user to intentionally specify and use both.
! The only change required is to NOT smear the loop setpoint over all the loop nodes.  Just
! read it from the setpoint node and use it.  In the short term it will remain up to the user
! to specify the location of the loop setpoint control node and avoid conflicts with component
! setpoint nodes.  Operationally, we will ignore the user specified placement of the loop setpoint
! node and assume that it is physically located at each half loop outlet for purposes of calculating loop
! demand.  Long term, I recommend that we:
!     1. specify the setpointmanager:plant object name (not the node name) in the plantloop/condloop objects
!     2. write a new setpoint manager (setpointmanager:plant) that is more suitable for plant use and
!        accomodates AIR and GROUND setpoints...with offsets.

!*****************************************************************
  !ONE TIME LOOP NODE SETPOINT CHECK
!*****************************************************************
  IF (MySetPointCheckFlag .AND. DoSetPointTest) THEN

    ! check for missing setpoints
    DO LoopNum = 1, TotNumLoops
      LoopSetPointTemp = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetPoint

      SensedNode = PlantLoop(LoopNum)%TempSetPointNodeNum
      IF (SensedNode > 0) THEN
        IF (Node(SensedNode)%TempSetPoint == SensedNodeFlagValue) THEN
          IF (.NOT. AnyEnergyManagementSystemInModel) THEN
            CALL ShowSevereError ('PlantManager: No Setpoint Manager Defined for Node='//TRIM(NodeID(SensedNode))//  &
                                   ' in PlantLoop='//TRIM(PlantLoop(LoopNum)%Name))
            CALL ShowContinueError('Add Temperature Setpoint Manager with Control Variable = '//  &
              '"Temperature" for this PlantLoop.')
                SetPointErrorFlag = .TRUE.
          ELSE
           ! need call to EMS to check node
            CALL CheckIfNodeSetpointManagedByEMS(SensedNode,iTemperatureSetpoint, SetpointErrorFlag)
            IF (SetpointErrorFlag) THEN
              CALL ShowSevereError ('PlantManager: No Setpoint Manager Defined for Node='//TRIM(NodeID(SensedNode))//  &
                                   ' in PlantLoop='//TRIM(PlantLoop(LoopNum)%Name))
              CALL ShowContinueError('Add Temperature Setpoint Manager with Control Variable = '//  &
                '"Temperature" for this PlantLoop.')
              CALL ShowContinueError('Or add EMS Actuator to provide temperature setpoint at this node')
            ENDIF
          ENDIF
        END IF

      END IF
    END DO
    MySetPointCheckFlag = .FALSE.
  END IF
!*****************************************************************
  ! END ONE TIME LOOP NODE SETPOINT CHECK

!*****************************************************************
  !ONE TIME PUMP AND SIZING INIT
!*****************************************************************
  IF (PlantSizeNotComplete) THEN

!    ! Step 1:  init plant sizing numbers in main plant data structure
! moved up to HVACManager (so ready for demand side equipment)
!    DO LoopNum = 1, TotNumLoops
!      CALL InitOneTimePlantSizingInfo(LoopNum)
!    ENDDO

    CALL SetAllFlowLocks(FlowUnlocked)
    FinishSizingFlag = .FALSE.
    PlantSizesOkayToFinalize = .FALSE. ! set global flag for when it ready to store final sizes
    Do passNum = 1, 4     !begin while loop to iterate over the next calls sequentially
       InitLoopEquip = .TRUE.

      ! Step 2, call component models it  using PlantCallingOrderInfo for sizing
      DO HalfLoopNum = 1, TotNumHalfLoops
        LoopNum         = PlantCallingOrderInfo(HalfLoopNum)%LoopIndex
        LoopSideNum     = PlantCallingOrderInfo(HalfLoopNum)%LoopSide
        CurLoopNum      = LoopNum

        DO BranchNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalBranches
          DO CompNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%TotalComponents
            CALL SimPlantEquip(LoopNum,LoopSideNum,BranchNum,CompNum,FirstHVACIteration,InitLoopEquip,GetCompSizFac)
          END DO !-CompNum
        END DO !-BranchNum
      ENDDO

      ! step 3, revise calling order
      ! have now called each plant component model at least once with InitLoopEquip = .true.
      !  this means the calls to InterConnectTwoPlantLoopSides have now been made, so rework calling order
      CALL RevisePlantCallingOrder

      ! Step 4: Simulate plant loop components so their design flows are included

      DO HalfLoopNum = 1, TotNumHalfLoops

        LoopNum         = PlantCallingOrderInfo(HalfLoopNum)%LoopIndex
        LoopSideNum     = PlantCallingOrderInfo(HalfLoopNum)%LoopSide
        CurLoopNum      = LoopNum
        CALL SizePlantLoop(LoopNum, FinishSizingFlag)

      ENDDO
    ENDDO ! iterative passes thru sizing related routines.  end while?

    !Step 5 now one more time for the final
    DO HalfLoopNum = 1, TotNumHalfLoops
      PlantSizesOkayToFinalize = .TRUE.
      FinishSizingFlag = .TRUE.
      LoopNum         = PlantCallingOrderInfo(HalfLoopNum)%LoopIndex
      LoopSideNum     = PlantCallingOrderInfo(HalfLoopNum)%LoopSide
      CurLoopNum      = LoopNum
      DO BranchNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalBranches
        DO CompNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%TotalComponents
          CALL SimPlantEquip(LoopNum,LoopSideNum,BranchNum,CompNum,FirstHVACIteration,InitLoopEquip,GetCompSizFac)
        END DO !-CompNum
      END DO !-BranchNum
      IF(PlantLoop(LoopNum)%PlantSizNum .GT. 0) PlantSizData(PlantLoop(LoopNum)%PlantSizNum)%VolFlowSizingDone = .TRUE.
      CALL SizePlantLoop(LoopNum, FinishSizingFlag)
    ENDDO

    PlantSizeNotComplete = .FALSE.
  END IF
!*****************************************************************
  !END ONE TIME SIZING INIT
!*****************************************************************
!*****************************************************************
  !BEGIN ONE TIME ENVIRONMENT INITS
!*****************************************************************
  IF(SupplyEnvrnFlag .AND. BeginEnvrnFlag) THEN

    DO LoopNum = 1, TotNumLoops
      DO LoopSideNum = DemandSide, SupplySide
          ! check if setpoints being placed on node properly
          IF (PlantLoop(LoopNum)%LoopDemandCalcScheme == DualSetPointDeadBand) THEN
            IF (Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetpointHi == SensedNodeFlagValue) THEN
              IF (.NOT. AnyEnergyManagementSystemInModel) THEN
                CALL ShowSevereError('Plant Loop: missing high temperature setpoint for dual setpoint deadband demand scheme')
                CALL ShowContinueError('Node Referenced ='//TRIM(NodeID(PlantLoop(LoopNum)%TempSetPointNodeNum)))
                CALL ShowContinueError('Use a SetpointManager:Scheduled:DualSetpoint to establish appropriate setpoints')
                SetPointErrorFlag = .TRUE.
              ELSE
                CALL CheckIfNodeSetpointManagedByEMS(PlantLoop(LoopNum)%TempSetPointNodeNum,iTemperatureMaxSetpoint,  &
                                                       SetpointErrorFlag)
                IF (SetpointErrorFlag) Then
                  CALL ShowSevereError('Plant Loop: missing high temperature setpoint for dual setpoint deadband demand scheme')
                  CALL ShowContinueError('Node Referenced ='//TRIM(NodeID(PlantLoop(LoopNum)%TempSetPointNodeNum)))
                  CALL ShowContinueError('Use a SetpointManager:Scheduled:DualSetpoint to establish appropriate setpoints')
                  CALL ShowContinueError('Or add EMS Actuator for Temperature Maximum Setpoint')

                ENDIF !SetPointErrorFlag
              ENDIF !Not EMS
            ENDIF !Node TSPhi = Sensed
            IF (Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetpointLo == SensedNodeFlagValue) THEN
              IF (.NOT. AnyEnergyManagementSystemInModel) THEN
                CALL ShowSevereError('Plant Loop: missing low temperature setpoint for dual setpoint deadband demand scheme')
                CALL ShowContinueError('Node Referenced ='//TRIM(NodeID(PlantLoop(LoopNum)%TempSetPointNodeNum)))
                CALL ShowContinueError('Use a SetpointManager:Scheduled:DualSetpoint to establish appropriate setpoints')
                SetPointErrorFlag = .TRUE.
              ELSE
                CALL CheckIfNodeSetpointManagedByEMS(PlantLoop(LoopNum)%TempSetPointNodeNum,iTemperatureMinSetpoint,   &
                                                       SetpointErrorFlag)
                IF (SetpointErrorFlag) Then
                  CALL ShowSevereError('Plant Loop: missing low temperature setpoint for dual setpoint deadband demand scheme')
                  CALL ShowContinueError('Node Referenced ='//TRIM(NodeID(PlantLoop(LoopNum)%TempSetPointNodeNum)))
                  CALL ShowContinueError('Use a SetpointManager:Scheduled:DualSetpoint to establish appropriate setpoints')
                  CALL ShowContinueError('Or add EMS Actuator for Temperature Minimum Setpoint')

                ENDIF !SetPointErrorFlag
              ENDIF !NOT EMS
            ENDIF !Node TSPtLo = Sensed...
          ENDIF !LoopDemandScheme = DualSPDB
      END DO !LOOPSIDE
    END DO  !PLANT LOOP

    !Any per-environment load distribution init should be OK here
    !Just clear away any trailing MyLoad for now...
    !This could likely be moved into InitLoadDistribution also...
    DO LoopNum = 1, TotNumLoops
      DO LoopSideNum = DemandSide,SupplySide
        DO BranchNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalBranches
          DO CompNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%TotalComponents
            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad =0.d0
            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%FreeCoolCntrlShutDown = .FALSE.
            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Available             = .FALSE.
          ENDDO
        ENDDO
      ENDDO
    ENDDO


    SupplyEnvrnFlag = .FALSE.
!!*****************************************************************
! !END OF ONE TIME ENVIRONMENT INITS
!!*****************************************************************
  END IF !END OF FIRSTHVACITERATION INITS
!
  IF (.NOT. BeginEnvrnFlag) SupplyEnvrnFlag=.TRUE.

  IF(ErrorsFound) CALL ShowFatalError('Preceding errors caused termination')

  RETURN

END SUBROUTINE InitializeLoops


SUBROUTINE ReInitPlantLoopsAtFirstHVACIteration

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Sept 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! initialize node mass flow requests

          ! METHODOLOGY EMPLOYED:
          ! called from SimHVAC to reset mass flow rate requests
          ! this contains all the initializ

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment,      ONLY : OutWetBulbTemp,OutDryBulbTemp, GroundTemp_Deep, StdBaroPress
  USE HVACInterfaceManager, ONLY : PlantCommonPipe
  USE ScheduleManager,      ONLY : GetCurrentScheduleValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64),PARAMETER::StartQuality = 1.0d0
  REAL(r64),PARAMETER::StartHumRat  = 0.0d0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER            :: LoopNum                    ! plant loop counter
  INTEGER            :: LoopIn
  REAL(r64)          :: LoopMaxMassFlowRate        ! maximum allowable loop mass flow rate
  REAL(r64)          :: LoopSetPointTemp           ! the loop control or setpoint temperature
  REAL(r64)          :: LoopMaxTemp                ! maximum allowable loop temperature
  REAL(r64)          :: LoopMinTemp                ! minimum allowable loop temperature
  REAL(r64)          :: LoopSetPointTempLo           ! the loop control or setpoint temperature
  REAL(r64)          :: LoopSetPointTempHi           ! the loop control or setpoint temperature
  REAL(r64)          :: SecondaryLoopSetPointTemp  ! loop setpoint temperature for common pipes with different secondary setpt
  INTEGER            :: LoopSideNum
  INTEGER            :: BranchNum                  ! branch loop counter
  INTEGER            :: OpNum                      ! operation scheme counter
  INTEGER            :: CompNum                    ! plant side component counter
  INTEGER            :: BranchInlet                ! branch inlet node number
  INTEGER            :: ComponentInlet             ! component inlet node number
  INTEGER            :: ComponentOutlet            ! component outlet node number
  LOGICAL, SAVE      :: MyEnvrnFlag = .TRUE.
  REAL(r64)          :: LoopMinMassFlowRate        ! minimum allowable loop mass flow rate
  REAL(r64)          :: SteamDensity
  REAL(r64)          :: SteamTemp
  REAL(r64)          :: StartEnthalpy
  REAL(r64)          :: Cp
  REAL(r64)          :: rho
  REAL(r64)          :: LoopSetPointTemperatureHi
  REAL(R64)          :: LoopSetPointTemperatureLo

  !*****************************************************************
  !BEGIN ENVIRONMENT INITS
!*****************************************************************
  IF(MyEnvrnFlag .AND. BeginEnvrnFlag) THEN

    DO LoopNum = 1, TotNumLoops
      DO LoopSideNum = DemandSide, SupplySide

        SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)

        CASE (SingleSetPoint)
          LoopSetPointTemp = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetPoint

        CASE (DualSetPointDeadBand)
          ! Get the range of setpoints
          LoopSetPointTemperatureHi = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetpointHi
          LoopSetPointTemperatureLo = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetpointLo
          LoopSetPointTemp = (LoopSetPointTemperatureLo + LoopSetPointTemperatureHi) /2.d0
        END SELECT

        IF ((PlantLoop(LoopNum)%CommonPipeType == CommonPipe_TwoWay) .AND. (LoopSideNum == DemandSide) .AND. &
            (PlantLoop(LoopNum)%LoopSide(DemandSide)%InletNodeSetPt)) THEN ! get a second setpoint for secondaryLoop
          ! if the plant loop is two common pipe configured for temperature control on secondary side inlet, then
          ! we want to initialize the demand side of the loop using that setpoint
          LoopSetPointTemp = Node(PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNumIn)%TempSetPoint
        ENDIF

            ! Check the Loop Setpoint and make sure it is bounded by the Loop Max and Min
        LoopMaxTemp = PlantLoop(LoopNum)%MaxTemp
        LoopMinTemp = PlantLoop(LoopNum)%MinTemp

        ! trap for -999 and set to average of limits if so
        IF (LoopSetPointTemp == SensedNodeFlagValue) THEN
          LoopSetPointTemp = (LoopMinTemp + LoopMaxTemp) / 2.d0
        ENDIF
            ! Check it against the loop temperature limits
        LoopSetPointTemp = Min(LoopMaxTemp, LoopSetPointTemp)
        LoopSetPointTemp = Max(LoopMinTemp, LoopSetPointTemp)

        !Initialize the capacitance model at the tank interface, and other loop side values
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TempInterfaceTankOutlet = LoopSetPointTemp
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%LastTempInterfaceTankOutlet = LoopSetPointTemp
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%LoopSideInlet_TankTemp      = LoopSetPointTemp
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalPumpHeat               = 0.d0
        IF (ALLOCATED(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Pumps)) &
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Pumps%PumpHeatToFluid     = 0.d0
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowRequest                 = 0.d0
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TimeElapsed                 = 0.d0
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%FlowLock                    = 0
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%InletNode%TemperatureHistory =   0.0d0
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%InletNode%MassFlowRateHistory =  0.0d0
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%OutletNode%TemperatureHistory =  0.0d0
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%OutletNode%MassFlowRateHistory = 0.0d0

        IF(PlantLoop(LoopNum)%FluidType /= NodeType_Steam) Then
          Cp =  GetSpecificHeatGlycol(PlantLoop(LoopNum)%FluidName,  &
                                    LoopSetPointTemp,              &
                                    PlantLoop(LoopNum)%FluidIndex, &
                                    'InitializeLoops')
          StartEnthalpy = Cp * LoopSetPointTemp
        ENDIF
            ! Use Min/Max flow rates to initialize loop
        IF(PlantLoop(LoopNum)%FluidType==NodeType_Water) Then
          rho = GetDensityGlycol(PlantLoop(LoopNum)%FluidName,  &
                                  LoopSetPointTemp, &
                                  PlantLoop(LoopNum)%FluidIndex,&
                                  'InitializeLoops')

          LoopMaxMassFlowRate = PlantLoop(loopnum)%MaxVolFlowRate * rho
          LoopMinMassFlowRate = PlantLoop(loopnum)%MinVolFlowRate * rho

        END IF
              !use saturated liquid of steam at the loop setpoint temp as the starting enthalpy for a water loop
        IF(PlantLoop(LoopNum)%FluidType==NodeType_Steam) Then
          SteamTemp = 100.d0
          SteamDensity=GetSatDensityRefrig('STEAM',SteamTemp,1.0d0,PlantLoop(LoopNum)%FluidIndex,'PlantManager:InitializeLoop')
          LoopMaxMassFlowRate = PlantLoop(loopnum)%MaxVolFlowRate * SteamDensity
          StartEnthalpy = GetSatEnthalpyRefrig('STEAM',LoopSetPointTemp,0.0d0,PlantLoop(LoopNum)%FluidIndex, &
                                               'PlantManager:InitializeLoop')
          LoopMinMassFlowRate = PlantLoop(loopnum)%MinVolFlowRate * SteamDensity
        END IF

        LoopMaxMassFlowRate = MAX(0.d0, LoopMaxMassFlowRate)
        LoopMinMassFlowRate = MAX(0.d0, LoopMinMassFlowRate)

              !Initial all loop nodes by initializing all component inlet and outlet nodes
        DO BranchNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalBranches
          DO CompNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%TotalComponents
            ComponentInlet        = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn
            ComponentOutlet       = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumOut
            BranchInlet           = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%NodeNumIn

            Node(ComponentInlet)%Temp                 = LoopSetpointTemp
            Node(ComponentInlet)%TempMin              = LoopMinTemp
            Node(ComponentInlet)%TempMax              = LoopMaxTemp
            Node(ComponentInlet)%TempLastTimestep     = LoopSetpointTemp

            Node(ComponentInlet)%MassFlowRate         = 0.0d0
            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad = 0.0d0
            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Available = .FALSE.
            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%FreeCoolCntrlShutDown = .FALSE.
            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%RequestedMassFlow =0.0d0


            IF (Node(ComponentInlet)%MassFlowRateMin > 0.d0) THEN
              Node(ComponentInlet)%MassFlowRateMinAvail = Node(ComponentInlet)%MassFlowRateMin
            ELSE
              Node(ComponentInlet)%MassFlowRateMin      = LoopMinMassFlowRate
              Node(ComponentInlet)%MassFlowRateMinAvail = LoopMinMassFlowRate
            ENDIF

            IF (Node(ComponentInlet)%MassFlowRateMax > 0.d0) THEN
              Node(ComponentInlet)%MassFlowRateMaxAvail = Node(ComponentInlet)%MassFlowRateMax
            ELSE
              Node(ComponentInlet)%MassFlowRateMax      = LoopMaxMassFlowRate
              Node(ComponentInlet)%MassFlowRateMaxAvail = LoopMaxMassFlowRate
            ENDIF

            Node(ComponentInlet)%MassFlowRateRequest  = 0.0d0
            Node(ComponentInlet)%Quality              = StartQuality
            Node(ComponentInlet)%Press                = StdBaroPress
            Node(ComponentInlet)%Enthalpy             = StartEnthalpy
            Node(ComponentInlet)%HumRat               = StartHumRat

            Node(ComponentOutlet)%FluidType            = Node(BranchInlet)%FluidType
            Node(ComponentOutlet)%Temp                 = Node(BranchInlet)%Temp
            Node(ComponentOutlet)%TempMin              = Node(BranchInlet)%TempMin
            Node(ComponentOutlet)%TempMax              = Node(BranchInlet)%TempMax
            Node(ComponentOutlet)%TempLastTimestep     = Node(BranchInlet)%TempLastTimestep
            Node(ComponentOutlet)%MassFlowRate         = Node(BranchInlet)%MassFlowRate
            Node(ComponentOutlet)%MassFlowRateMin      = Node(BranchInlet)%MassFlowRateMin
            Node(ComponentOutlet)%MassFlowRateMax      = Node(BranchInlet)%MassFlowRateMax
            Node(ComponentOutlet)%MassFlowRateMinAvail = Node(BranchInlet)%MassFlowRateMinAvail
            Node(ComponentOutlet)%MassFlowRateMaxAvail = Node(BranchInlet)%MassFlowRateMaxAvail
            Node(ComponentOutlet)%MassFlowRateRequest  = 0.0d0
            Node(ComponentOutlet)%Quality              = StartQuality
            Node(ComponentOutlet)%Press                = StdBaroPress
            Node(ComponentOutlet)%Enthalpy             = StartEnthalpy
            Node(ComponentOutlet)%HumRat               = StartHumRat
          END DO !COMPONENT LOOP
        END DO !BRANCH LOOP
      END DO !LOOPSIDE
    END DO  !PLANT LOOP
    PlantReport%CoolingDemand       = 0.d0
    PlantReport%HeatingDemand       = 0.d0
    PlantReport%DemandNotDispatched = 0.d0
    PlantReport%UnmetDemand         = 0.d0
    PlantReport%LastLoopSideSimulated = 0
    PlantReport%InletNodeFlowrate    = 0.d0
    PlantReport%InletNodeTemperature  = 0.d0
    PlantReport%OutletNodeFlowrate    = 0.d0
    PlantReport%OutletNodeTemperature = 0.d0

    MyEnvrnFlag = .FALSE.
!*****************************************************************
 !END OF ENVIRONMENT INITS
!*****************************************************************
  END IF

  IF (.NOT. BeginEnvrnFlag) MyEnvrnFlag=.TRUE.

  ! FirstHVACiteration inits
  DO LoopNum = 1, TotNumLoops
    LoopIn = PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNumIn !DSU? Demand/Supply side inlet??
      !UPDATE LOOP FLOW SETPOINT
!    Node(LoopIn)%MassFlowRateSetPoint =  LoopMaxMassFlowRate !DSU? this is suspect, may not be set?
      !UPDATE LOOP TEMPERATURE SETPOINTS

    LoopSetPointTemp = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetPoint

        ! Check the Loop Setpoint and make sure it is bounded by the Loop Max and Min
    LoopMaxTemp = PlantLoop(LoopNum)%MaxTemp
    LoopMinTemp = PlantLoop(LoopNum)%MinTemp
          ! Check it against the loop temperature limits
    LoopSetPointTemp = Min(LoopMaxTemp, LoopSetPointTemp)
    LoopSetPointTemp = Max(LoopMinTemp, LoopSetPointTemp)

      !Update supply side loop setpoint in plant data structure
    PlantLoop(LoopNum)%LoopSide(SupplySide)%TempSetPoint = LoopSetPointTemp
    PlantLoop(LoopNum)%LoopSide(DemandSide)%TempSetPoint = LoopSetPointTemp

      !Update supply side hi-lo setpoints for dual SP control
    IF (PlantLoop(LoopNum)%LoopDemandCalcScheme == DualSetPointDeadBand) THEN
      LoopSetPointTempHi = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetPointHi
      LoopSetPointTempLo = Node(PlantLoop(LoopNum)%TempSetPointNodeNum)%TempSetPointLo
      LoopSetPointTempHi = Min(LoopMaxTemp, LoopSetPointTempHi)
      LoopSetPointTempHi = Max(LoopMinTemp, LoopSetPointTempHi)
      LoopSetPointTempLo = Min(LoopMaxTemp, LoopSetPointTempLo)
      LoopSetPointTempLo = Max(LoopMinTemp, LoopSetPointTempLo)
      PlantLoop(LoopNum)%LoopSide(SupplySide)%TempSetPointHi = LoopSetPointTempHi
      PlantLoop(LoopNum)%LoopSide(SupplySide)%TempSetPointLo = LoopSetPointTempLo
    ENDIF

      !update demand side loop setpoint in plant data structure
    IF (PlantLoop(LoopNum)%CommonPipeType == CommonPipe_TwoWay) THEN ! get a second setpoint for secondaryLoop
      ! if the plant loop is two common pipe configured for temperature control on secondary side inlet, then
      ! we want to initialize the demand side of the loop using that setpoint
      IF (PlantLoop(LoopNum)%LoopSide(DemandSide)%InletNodeSetPt) THEN
        SecondaryLoopSetPointTemp = Node(PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNumIn)%TempSetPoint
        SecondaryLoopSetPointTemp = Min(LoopMaxTemp, SecondaryLoopSetPointTemp)
        SecondaryLoopSetPointTemp = Max(LoopMinTemp, SecondaryLoopSetPointTemp)
        PlantLoop(LoopNum)%LoopSide(DemandSide)%TempSetPoint = SecondaryLoopSetPointTemp
          !Since Dual setpoint not explicitly available for demand side, we can't do the
          !bounding check on hi/lo setpoint.  IF we did we would over-write
          !the SensedNodeFlagValue of -999 for no dual setpoint case.
        PlantLoop(LoopNum)%LoopSide(DemandSide)%TempSetPointHi = &
              Node(PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNumIn)%TempSetPointHi
        PlantLoop(LoopNum)%LoopSide(DemandSide)%TempSetPointLo = &
              Node(PlantLoop(LoopNum)%LoopSide(DemandSide)%NodeNumIn)%TempSetPointLo
      ENDIF

      !initialize common pipe flows to zero.
      IF (ALLOCATED(PlantCommonPipe)) THEN
        PlantCommonPipe(LoopNum)%PriToSecFlow = 0.d0
        PlantCommonPipe(LoopNum)%SecToPriFlow = 0.d0
        PlantCommonPipe(LoopNum)%PriCPLegFlow = 0.d0
        PlantCommonPipe(LoopNum)%SecCPLegFlow = 0.d0
      ENDIF
    ELSE !no secondary loop, so use supply side loop SP on demand side too.
      PlantLoop(LoopNum)%LoopSide(DemandSide)%TempSetPoint = LoopSetPointTemp
      IF (PlantLoop(LoopNum)%LoopDemandCalcScheme == DualSetPointDeadBand) THEN
        PlantLoop(LoopNum)%LoopSide(DemandSide)%TempSetPointHi = LoopSetPointTempHi
        PlantLoop(LoopNum)%LoopSide(DemandSide)%TempSetPointLo = LoopSetPointTempLo
      ENDIF
    ENDIF

    DO LoopSideNum = DemandSide, SupplySide
      DO BranchNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalBranches
        DO CompNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%TotalComponents
          ComponentInlet    = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn
          ComponentOutlet   = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumOut

          !reinit to node hardware limits
          Node(ComponentInlet )%MassFlowRateMinAvail = Node(ComponentInlet )%MassFlowRateMin
          Node(ComponentOutlet)%MassFlowRateMinAvail = Node(ComponentInlet )%MassFlowRateMin
          Node(ComponentInlet )%MassFlowRateMaxAvail = Node(ComponentInlet )%MassFlowRateMax
          Node(ComponentOutlet)%MassFlowRateMaxAvail = Node(ComponentInlet )%MassFlowRateMax

          Node(ComponentInlet)%MassFlowRateRequest   = 0.0d0
          Node(ComponentOutlet)%MassFlowRateRequest  = 0.0d0

        END DO
      END DO
    END DO

    DO OpNum =1, PlantLoop(LoopNum)%NumOpSchemes
          ! If the operating scheme is scheduled "OFF", go to next scheme
      IF(GetCurrentScheduleValue(PlantLoop(LoopNum)%OpScheme(OpNum)%SchedPtr) <= 0.d0)THEN
        PlantLoop(LoopNum)%OpScheme(OpNum)%Available  = .FALSE.
      ELSE
        PlantLoop(LoopNum)%OpScheme(OpNum)%Available  = .TRUE.
      END IF
    END DO
  END DO

  RETURN

END SUBROUTINE ReInitPlantLoopsAtFirstHVACIteration

SUBROUTINE UpdateNodeThermalHistory

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Sept 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! update temperature history for plant capacitance model and other
          !

          ! METHODOLOGY EMPLOYED:
          ! copy current values into "LastTimestep" values

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

 ! array assignment
  IF (NumOfNodes > 0) THEN
    Node%TempLastTimestep     = Node%Temp
    Node%EnthalpyLastTimestep = Node%Enthalpy
  ENDIF

  RETURN

END SUBROUTINE UpdateNodeThermalHistory

SUBROUTINE CheckPlantOnAbort

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Septemeber 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Called once E+ is in the process of aborting because of fatal error
          !  check for plant input problems to help users find problems in input files

          ! METHODOLOGY EMPLOYED:
          !  search plant data structures for issues that may help solve problems in input files
          !  1.   if loop side has a splitter/mixer and one branch in there is control type bypass,
          !       then another branch in the s/m needs to be active
          !  other checks could/should be added!

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE DataInterfaces, ONLY: ShowWarningError, ShowContinueError
  USE DataErrorTracking, ONLY: AskForPlantCheckOnAbort
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
  INTEGER :: LoopNum    ! DO loop counter for loops
  LOGICAL :: ActiveCntrlfound ! used to search for active control branches in parallel with bypass branches
  INTEGER :: ParalBranchNum ! used to search for active control branches in parallel with bypass branches
  INTEGER :: ParalBranchNum2 ! used to search for active control branches in parallel with bypass branches
  INTEGER :: BranchNum2 ! used to search for active control branches in parallel with bypass branches
  INTEGER :: SideNum
  INTEGER :: numLoopSides
  INTEGER :: SplitNum
  INTEGER :: BranchNum  ! DO loop counter for branches
  INTEGER :: CompNum    ! do loop for multiple components on a branch
  LOGICAL :: ShouldBeACTIVE


If (.not. (AskForPlantCheckOnAbort)) then
  RETURN
endif

If (.not. (TotNumLoops  > 0)) return
If (.not.(allocated(PlantLoop))) return

  DO LoopNum = 1, TotNumLoops
    numLoopSides = 2
    Do SideNum = 1, numLoopSides
      IF (.not. (PlantLoop(LoopNum)%LoopSide(SideNum)%SplitterExists)) Cycle
        DO SplitNum = 1, PlantLoop(LoopNum)%LoopSide(SideNum)%NumSplitters
          DO  ParalBranchNum=1 , PlantLoop(LoopNum)%LoopSide(SideNum)%Splitter(SplitNum)%TotalOutletNodes
            BranchNum = PlantLoop(LoopNum)%LoopSide(SideNum)%Splitter(SplitNum)%BranchNumOut(ParalBranchNum)
            If (PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%IsByPass) then ! we know there is a bypass
              ! check that there is at least one 'Active' control type in parallel with bypass branch
              ActiveCntrlfound = .false.
              DO  ParalBranchNum2=1, PlantLoop(LoopNum)%LoopSide(SideNum)%Splitter(SplitNum)%TotalOutletNodes
                BranchNum2 = PlantLoop(LoopNum)%LoopSide(SideNum)%Splitter(SplitNum)%BranchNumOut(ParalBranchNum2)
                 If (PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum2)%ControlType == ControlType_Active) then
                   ActiveCntrlfound = .true.
                 endif
              ENDDO
              If ( .not. (ActiveCntrlfound)) then
                CALL ShowWarningError('Check control types on branches between splitter and mixer in '//  &
                                       'PlantLoop='//TRIM(PlantLoop(LoopNum)%Name))
                CALL ShowContinueError('Found a BYPASS branch with no ACTIVE branch in parallel with it')
                CALL ShowContinueError('In certain (but not all) situations, this can cause problems; please verify your inputs')
                CALL ShowContinueError('Bypass branch named: '//trim(PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%Name))
              ENDIF
            ENDIF   ! bypass present

            !check for possible components on demand side that should be ACTIVE but are not
            IF (SideNum == DemandSide) THEN
              ! check for presences of the following components whose branch control type should be active
               ! WATER HEATER:MIXED
               ! WATER HEATER:STRATIFIED
               ! WATER USE CONNECTIONS
               ! COIL:WATER:COOLING
               ! COIL:WATER:SIMPLEHEATING
               ! COIL:STEAM:AIRHEATING
               ! SOLAR COLLECTOR:FLAT PLATE
               ! PLANT LOAD PROFILE
              Do CompNum = 1, PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%TotalComponents
                ShouldBeACTIVE = .FALSE.
                SELECT CASE (PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num)

                Case (TypeOf_WtrHeaterMixed)
                  ShouldBeACTIVE = .TRUE.
                CASE (TypeOf_WtrHeaterStratified)
                  ShouldBeACTIVE = .TRUE.
                CASE (TypeOf_WaterUseConnection)
                  ShouldBeACTIVE = .TRUE.
                CASE (TypeOf_CoilWaterCooling)
                  ShouldBeACTIVE = .TRUE.
                CASE (TypeOf_CoilWaterDetailedFlatCooling)
                  ShouldBeACTIVE = .TRUE.
                CASE (TypeOf_CoilWaterSimpleHeating)
                  ShouldBeACTIVE = .TRUE.
                CASE (TypeOf_CoilSteamAirHeating)
                  ShouldBeACTIVE = .TRUE.
                CASE (TypeOf_SolarCollectorFlatPlate)
                  ShouldBeACTIVE = .TRUE.
                CASE (TypeOf_PlantLoadProfile)
                  ShouldBeACTIVE = .TRUE.
                CASE DEFAULT
                  ! not a demand side component that we know needs to be active, do nothing

                END SELECT

                If (ShouldBeACTIVE) THEN
                  SELECT CASE (PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%ControlType)

                  CASE (ControlType_Unknown)
                     CALL ShowWarningError('Found potential problem with Control Type for Branch named: '&
                                   //trim(PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%Name) )
                     CALL ShowContinueError('This branch should (probably) be ACTIVE but has control type unknown')
                  CASE (ControlType_Active)
                    ! do nothing, this is correct control type.
                  CASE (ControlType_Passive)
                     CALL ShowWarningError('Found potential problem with Control Type for Branch named: '&
                                   //trim(PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%Name) )
                     CALL ShowContinueError('This branch should (probably) be ACTIVE but has control type PASSIVE')
                  CASE (ControlType_SeriesActive)
                    ! do nothing, should be okay. (? don't really understand SeriesActive though)
                  CASE (ControlType_Bypass)
                     CALL ShowWarningError('Found potential problem with Control Type for Branch named: '&
                                   //trim(PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%Name) )
                     CALL ShowContinueError('This branch should (probably) be ACTIVE but has control type Bypass')
                  END SELECT
                ENDIF ! should be active
              ENDDO !comp num loop
            ENDIF ! demand side

          ENDDO ! splitter outlet nodes
        ENDDO ! splitters
       !check to see if bypass exists in demand side. If not warn error of possible flow problems
      IF(.NOT. PlantLoop(LoopNum)%LoopSide(SideNum)%ByPassExists) THEN
       IF(SideNum == DemandSide) THEN
         CALL ShowWarningError('There is no BYPASS component in the demand-side of PlantLoop =' &
                               //TRIM(PlantLoop(LoopNum)%Name))
         CALL ShowContinueError('You may be able to fix the fatal error above by adding a demand-side BYPASS PIPE.')
       END IF
      END IF
    ENDDO ! loop sides
  ENDDO ! plant loops

  RETURN

END SUBROUTINE CheckPlantOnAbort

!SUBROUTINE CheckPlantLoopData
!
!          ! SUBROUTINE INFORMATION:
!          !       AUTHOR         B. Griffith
!          !       DATE WRITTEN   May 2008
!          !       MODIFIED       na
!          !       RE-ENGINEERED  na
!
!          ! PURPOSE OF THIS SUBROUTINE:
!          ! This routine checks plant loop for input problems early in the simulation
!          ! Some of the same checks also occur in CheckPlantOnAbort but those only execute if aborted
!          ! Additional plant loop input checks can be added here.
!
!          ! METHODOLOGY EMPLOYED:
!          ! Test plant loop data for know issues.
!          !  1. CR 7431.  detect presence of water coils and check for "ACTIVE" branch control.
!
!          ! REFERENCES:
!          ! na
!
!          ! USE STATEMENTS:
!          ! na
!
!  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine
!
!          ! SUBROUTINE ARGUMENT DEFINITIONS:
!          ! na
!
!          ! SUBROUTINE PARAMETER DEFINITIONS:
!          ! na
!
!          ! INTERFACE BLOCK SPECIFICATIONS:
!          ! na
!
!          ! DERIVED TYPE DEFINITIONS:
!          ! na
!
!          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  LOGICAL :: ShouldBeACTIVE
!  INTEGER :: SideNum
!  INTEGER :: numLoopSides
!!unused-1208  INTEGER :: SplitNum
!  INTEGER :: BranchNum  ! DO loop counter for branches
!  INTEGER :: CompNum    ! do loop for multiple components on a branch
!  INTEGER :: LoopNum    ! DO loop counter for loops
!
!  IF (.not. (TotNumLoops  > 0)) RETURN
!  IF (.not.(ALLOCATED(PlantLoop))) RETURN
!
!  DO LoopNum = 1, TotNumLoops
!    numLoopSides = 2
!    DO SideNum = 1, numLoopSides
!      DO BranchNum =1, PlantLoop(LoopNum)%LoopSide(SideNum)%TotalBranches
!        DO CompNum= 1,  PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%TotalComponents
!          ShouldBeACTIVE = .FALSE.
!
!          SELECT CASE (PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num)
!          ! for now, check that all water coils are on "active" branch.
!          CASE (TypeOf_WaterUseConnection)
!            ShouldBeACTIVE = .TRUE.
!          CASE (TypeOf_CoilWaterCooling)
!            ShouldBeACTIVE = .TRUE.
!          CASE (TypeOf_CoilWaterDetailedFlatCooling)
!            ShouldBeACTIVE = .TRUE.
!          CASE (TypeOf_CoilWaterSimpleHeating)
!            ShouldBeACTIVE = .TRUE.
!          CASE (TypeOf_CoilSteamAirHeating)
!            ShouldBeACTIVE = .TRUE.
!
!          CASE DEFAULT
!
!          END SELECT
!
!          If (ShouldBeACTIVE) THEN
!            SELECT CASE (PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%Comp(CompNum)%FlowCtrl)
!
!            CASE (ControlType_Unknown)
!               CALL ShowWarningError('Found potential problem with Control Type for Branch named: '&
!                             //trim(PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%Name) )
!                             !DSU3 note, this confuses branch and components, should have reported out comp name as well.
!               CALL ShowContinueError('This branch should (probably) be ACTIVE but has control type unknown')
!            CASE (ControlType_Active)
!              ! do nothing, this is correct control type.
!            CASE (ControlType_Passive)
!               CALL ShowSevereError('Found problem with Control Type for Branch named: '&
!                             //trim(PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%Name) )
!               CALL ShowContinueError('This branch should be ACTIVE but has control type PASSIVE')
!            CASE (ControlType_SeriesActive)
!              ! do nothing, should be okay. (? don't really understand SeriesActive though)
!            CASE (ControlType_Bypass)
!               CALL ShowSevereError('Found problem with Control Type for Branch named: '&
!                             //trim(PlantLoop(LoopNum)%LoopSide(SideNum)%Branch(BranchNum)%Name) )
!               CALL ShowContinueError('This branch should be ACTIVE but has control type Bypass')
!            END SELECT
!          ENDIF ! should be active
!        ENDDO !comp num loop
!      ENDDO ! branches
!    ENDDO ! loop sides
!  ENDDO ! plant loops
!
!  RETURN
!
!END SUBROUTINE CheckPlantLoopData

SUBROUTINE InitOneTimePlantSizingInfo(LoopNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   April 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! one time init what can be set up related to plant sizing data structure.

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing , ONLY: NumPltSizInput, PlantSizData
  USE InputProcessor, ONLY: FindItemInList
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,          INTENT(IN)  :: LoopNum  ! loop being initialized for sizing

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: PlantSizNum  ! index of Plant Sizing data for this loop
  PlantSizNum = 0
  IF (PlantLoop(LoopNum)%PlantSizNum == 0) THEN
    IF (NumPltSizInput > 0) THEN
      PlantSizNum = FindItemInList(PlantLoop(LoopNum)%Name,PlantSizData%PlantLoopName,NumPltSizInput)
      IF (PlantSizNum > 0) THEN
        PlantLoop(LoopNum)%PlantSizNum = PlantSizNum
      ENDIF
    END IF
  END IF

  RETURN

END SUBROUTINE InitOneTimePlantSizingInfo


SUBROUTINE SizePlantLoop(LoopNum, OkayToFinish)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   December 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing the supply side of Plant Loops for which loop flow rates
          ! have not been specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains volumetric flow rate data from the PlantSizData array..

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: RoundSigDigits
  USE PlantLoopEquip, ONLY : SimPlantEquip
  USE FluidProperties, ONLY: GetDensityGlycol
  USE ReportSizingManager, ONLY: ReportSizingOutput

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,          INTENT(IN)  :: LoopNum  ! Supply side loop being simulated
  LOGICAL,          INTENT(IN)  :: OkayToFinish

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: PlantSizNum  ! index of Plant Sizing data for this loop
  INTEGER :: BranchNum          ! DO loop counter for cycling through branches on a demand side loop
  INTEGER :: CompNum            ! DO loop counter for cycling through components on a demand side loop
  INTEGER :: SupNodeNum         ! component inlet water node number
  INTEGER :: WaterCompNum       ! DO loop counter for cycling through all the components that demand water
  LOGICAL :: ErrorsFound        ! If errors detected in input
  LOGICAL :: InitLoopEquip
  LOGICAL :: SimNestedLoop
  LOGICAL :: ReSize
  LOGICAL :: AllSizFac
  REAL(r64) :: LoopSizFac
  REAL(r64) :: AvLoopSizFac
  REAL(r64) :: PlantSizFac
  REAL(r64) :: MaxSizFac
  REAL(r64) :: BranchSizFac
  REAL(r64) :: NumBrSizFac
  REAL(r64) :: FluidDensity ! local value from glycol routine
  LOGICAL   :: Finalize

  Finalize = OkayToFinish
  PlantSizNum = 0
  ErrorsFound = .FALSE.
  LoopSizFac = 0.0d0
  ! InitLoopEquip = .FALSE.
   InitLoopEquip = .TRUE.
  SimNestedLoop = .FALSE.

  AllSizFac = .TRUE.
  GetCompSizFac = .TRUE.
  MaxSizFac = 0.0d0
  PlantSizFac = 1.0d0
  NumBrSizFac = 0.0d0
  ReSize = .FALSE.

  IF (PlantLoop(LoopNum)%PlantSizNum > 0) THEN
    ReSize = .TRUE.
    PlantSizNum = PlantLoop(LoopNum)%PlantSizNum
   ! PlantSizData(PlantSizNum)%DesVolFlowRate = 0.0D0 ! DSU2
  ELSE
    IF (NumPltSizInput > 0) THEN
      PlantSizNum = FindItemInList(PlantLoop(LoopNum)%Name,PlantSizData%PlantLoopName,NumPltSizInput)
    END IF
  END IF
  PlantLoop(LoopNum)%PlantSizNum = PlantSizNum
  ! calculate a loop sizing factor and a branch sizing factor. Note that components without a sizing factor
  ! are assigned sizing factors of zero in this calculation
  IF (PlantSizNum > 0) THEN
    DO BranchNum= 1, PlantLoop(LoopNum)%LoopSide(SupplySide)%TotalBranches
      BranchSizFac = 0.0d0
      PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%PumpSizFac = 1.0d0
      IF (PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumIn ==   &
          PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%NodeNumIn) CYCLE
      IF (PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumOut ==   &
          PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%NodeNumOut) CYCLE
      DO CompNum = 1, PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%TotalComponents
        CALL SimPlantEquip(LoopNum,SupplySide,BranchNum,CompNum,.TRUE.,InitLoopEquip, GetCompSizFac)
        BranchSizFac = MAX(BranchSizFac , PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%SizFac)
      END DO
      LoopSizFac = LoopSizFac + BranchSizFac
      MaxSizFac = MAX(MaxSizFac, BranchSizFac)
      IF (BranchSizFac > 0.0d0) THEN
        PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%PumpSizFac = BranchSizFac
        NumBrSizFac = NumBrSizFac + 1.0d0
      ELSE
        AllSizFac = .FALSE.
      END IF
    END DO
    AvLoopSizFac = LoopSizFac / MAX(1.0d0, NumBrSizFac)

    ! sum up contributions from CompDesWaterFlow
    PlantSizData(PlantSizNum)%DesVolFlowRate = 0.d0 ! init for summation
    DO BranchNum = 1, PlantLoop(LoopNum)%LoopSide(DemandSide)%TotalBranches
      DO CompNum = 1, PlantLoop(LoopNum)%LoopSide(DemandSide)%Branch(BranchNum)%TotalComponents
        SupNodeNum = PlantLoop(LoopNum)%LoopSide(DemandSide)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn
        DO WaterCompNum=1,SaveNumPlantComps
          IF (SupNodeNum == CompDesWaterFlow(WaterCompNum)%SupNode) THEN
            PlantSizData(PlantSizNum)%DesVolFlowRate = PlantSizData(PlantSizNum)%DesVolFlowRate + &
                                                         CompDesWaterFlow(WaterCompNum)%DesVolFlowRate
          END IF
        END DO
      END DO
    END DO
    IF (PlantLoop(LoopNum)%MaxVolFlowRate .NE. AutoSize .AND. .NOT. ReSize) THEN
      PlantSizData(PlantSizNum)%DesVolFlowRate = PlantLoop(LoopNum)%MaxVolFlowRate
    ELSE IF (AvLoopSizFac > 0.0d0 .AND. AvLoopSizFac < 1.0d0) THEN
      PlantSizFac = LoopSizFac
    ELSE IF (AvLoopSizFac > 1.0d0) THEN
      PlantSizFac = MaxSizFac
    ELSE
      PlantSizFac = 1.0d0
    END IF
    DO BranchNum= 1, PlantLoop(LoopNum)%LoopSide(SupplySide)%TotalBranches
      IF (PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumIn ==   &
          PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%NodeNumIn) THEN
        PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%PumpSizFac = PlantSizFac
      END IF
      IF (PlantLoop(LoopNum)%LoopSide(SupplySide)%NodeNumOut ==   &
          PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%NodeNumOut) THEN
        PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%PumpSizFac = PlantSizFac
      END IF
    END DO
  END IF



  IF (PlantLoop(LoopNum)%MaxVolFlowRate == AutoSize) THEN

    IF ((PlantSizNum > 0) ) THEN

      IF (PlantSizData(PlantSizNum)%VolFlowSizingDone) THEN
        IF (PlantSizData(PlantSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
          PlantLoop(LoopNum)%MaxVolFlowRate = PlantSizData(PlantSizNum)%DesVolFlowRate * PlantSizFac
        ELSE
          PlantLoop(LoopNum)%MaxVolFlowRate = 0.0d0
          CALL ShowWarningError('SizePlantLoop: Calculated Plant Sizing Design Volume Flow Rate=['//  &
               TRIM(RoundSigDigits(PlantSizData(PlantSizNum)%DesVolFlowRate, 2))//'] is too small. Set to 0.0')
          CALL ShowContinueError('..occurs for PlantLoop='//TRIM(PlantLoop(LoopNum)%Name))
        END IF
        IF (Finalize) THEN
          IF (PlantLoop(LoopNum)%TypeOfLoop == LoopType_Plant) THEN
            CALL ReportSizingOutput('PlantLoop',PlantLoop(LoopNum)%Name,&
                                  'Maximum Loop Flow Rate [m3/s]',PlantLoop(LoopNum)%MaxVolFlowRate)
          ELSEIF (PlantLoop(LoopNum)%TypeOfLoop == LoopType_Condenser) Then
            CALL ReportSizingOutput('CondenserLoop',PlantLoop(LoopNum)%Name,&
                                  'Maximum Loop Flow Rate [m3/s]',PlantLoop(LoopNum)%MaxVolFlowRate)

          ENDIF
        ENDIF
      ENDIF
    ELSE
      CALL ShowFatalError('Autosizing of plant loop requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in PlantLoop object='//TRIM(PlantLoop(LoopNum)%Name))
      ErrorsFound = .TRUE.
    END IF

  END IF

  IF (.NOT. Finalize) THEN
    GetCompSizFac = .FALSE.
    RETURN
  ENDIF

  ! Small loop mass no longer introduces instability. Checks and warnings removed by SJR 20 July 2007.
  IF (PlantLoop(LoopNum)%Volume == AutoCalculate) THEN
    ! Although there is no longer a stability requirement (mass can be zero), autosizing is formulated the same way.
    PlantLoop(LoopNum)%Volume = PlantLoop(LoopNum)%MaxVolFlowRate*TimeStepZone*SecInHour/0.8d0
    IF (PlantLoop(LoopNum)%TypeOfLoop == LoopType_Plant) THEN
      ! condenser loop vs plant loop breakout needed.
      CALL ReportSizingOutput('PlantLoop', PlantLoop(LoopNum)%Name,'Plant Loop Volume [m3]', PlantLoop(LoopNum)%Volume)
    ELSEIF (PlantLoop(LoopNum)%TypeOfLoop == LoopType_Condenser) THEN
      CALL ReportSizingOutput('CondenserLoop', PlantLoop(LoopNum)%Name,'Condenser Loop Volume [m3]', PlantLoop(LoopNum)%Volume)
    ENDIF
  END IF

  !should now have plant volume, calculate plant volume's mass for fluid type
  IF (PlantLoop(LoopNum)%FluidType==NodeType_Water) THEN
    FluidDensity = GetDensityGlycol(PlantLoop(LoopNum)%FluidName, InitConvTemp,PlantLoop(LoopNum)%FluidIndex,'SizePlantLoop')
  ELSEIF (PlantLoop(LoopNum)%FluidType==NodeType_Steam) THEN
    FluidDensity = GetSatDensityRefrig('STEAM',100.0d0,1.0d0,PlantLoop(LoopNum)%FluidIndex,'SizePlantLoop')
  END IF

  PlantLoop(LoopNum)%Mass = PlantLoop(LoopNum)%Volume * FluidDensity

  PlantLoop(LoopNum)%MaxMassFlowRate = PlantLoop(LoopNum)%MaxVolFlowRate * FluidDensity
  PlantLoop(LoopNum)%MinMassFlowRate = PlantLoop(LoopNum)%MinVolFlowRate * FluidDensity



  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  GetCompSizFac = .FALSE.

  RETURN

END  SUBROUTINE SizePlantLoop

SUBROUTINE SetupInitialPlantCallingOrder

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Feb 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! setup the order that plant loops are to be called

          ! METHODOLOGY EMPLOYED:
          ! simple rule-based allocation of which order to call the half loops
          !  initially just mimicing historical practice until a better set of rules is
          ! developed
          ! 1.  first call all plant demand sides
          ! 2.  second call all plant supply sides
          ! 3.  third call all condenser demand sides
          ! 4.  fourth call all condenser supply sides

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:


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
  INTEGER :: OrderIndex ! local
  INTEGER :: I  ! local loop

  TotNumHalfLoops = 2*TotNumLoops

  IF (TotNumHalfLoops <= 0) Return

   ! first allocate to total number of plant half loops

  IF (.not. ALLOCATED(PlantCallingOrderInfo) )  ALLOCATE ( PlantCallingOrderInfo(TotNumHalfLoops))

  ! set plant loop demand sides
  Do I = 1, NumPlantLoops
    PlantCallingOrderInfo(I)%LoopIndex = I
    PlantCallingOrderInfo(I)%LoopSide  = DemandSide
  ENDDO

  ! set plant loop supply sides
  Do I = 1, NumPlantLoops
    OrderIndex  = I + NumPlantLoops
    PlantCallingOrderInfo(OrderIndex)%LoopIndex = I
    PlantCallingOrderInfo(OrderIndex)%LoopSide  = SupplySide
  ENDDO

  ! set condenser Loop demand sides
  Do I = 1, NumCondLoops
    OrderIndex  = 2 * NumPlantLoops + I
    PlantCallingOrderInfo(OrderIndex)%LoopIndex = NumPlantLoops + I
    PlantCallingOrderInfo(OrderIndex)%LoopSide  = DemandSide
  ENDDO

  ! set condenser Loop supply sides
  Do I = 1, NumCondLoops
    OrderIndex  = 2 * NumPlantLoops + NumCondLoops + I
    PlantCallingOrderInfo(OrderIndex)%LoopIndex = NumPlantLoops + I
    PlantCallingOrderInfo(OrderIndex)%LoopSide  = SupplySide
  ENDDO



  ! legacy one-time calling control stuff moved here from manager routine, hopefully remove
  IF(.NOT. ALLOCATED(LoadChangeDownStream)) ALLOCATE(LoadChangeDownStream(TotNumLoops))



  RETURN

END SUBROUTINE SetupInitialPlantCallingOrder

SUBROUTINE RevisePlantCallingOrder

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   april 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! setup the order that plant loops are to be called

          ! METHODOLOGY EMPLOYED:
          ! simple rule-based allocation of which order to call the half loops
          !
          ! Examine for interconnected components and rearrange to impose the following rules
          !
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE PlantUtilities,  ONLY : ShiftPlantLoopSideCallingOrder

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
  INTEGER :: HalfLoopNum
  INTEGER :: LoopNum
  INTEGER :: LoopSideNum
  INTEGER :: OtherLoopNum
  INTEGER :: OtherLoopSideNum
  INTEGER :: OtherLoopCallingIndex = 0
  INTEGER :: OtherLoopDemandSideCallingIndex = 0
  INTEGER :: NewOtherDemandSideCallingIndex = 0
  INTEGER :: newCallingIndex  = 0
  LOGICAL :: thisLoopPutsDemandOnAnother
  INTEGER :: ConnctNum


  DO HalfLoopNum = 1, TotNumHalfLoops

   LoopNum         = PlantCallingOrderInfo(HalfLoopNum)%LoopIndex
   LoopSideNum     = PlantCallingOrderInfo(HalfLoopNum)%LoopSide

   IF (ALLOCATED(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Connected)) THEN
     DO ConnctNum = 1, Size(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Connected)
       OtherLoopNum     = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Connected(ConnctNum)%LoopNum
       OtherLoopSideNum = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Connected(ConnctNum)%LoopSideNum
       OtherLoopCallingIndex = FindLoopSideInCallingOrder(OtherLoopNum, OtherLoopSideNum)

       thisLoopPutsDemandOnAnother = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Connected(ConnctNum)%LoopDemandsOnRemote
       If (thisLoopPutsDemandOnAnother) THEN ! make sure this loop side is called before the other loop side
         IF (OtherLoopCallingIndex < HalfLoopNum) THEN !rearrange
           newCallingIndex = MIN(HalfLoopNum + 1, TotNumHalfLoops)
           CALL ShiftPlantLoopSideCallingOrder(OtherLoopCallingIndex, newCallingIndex)
         ENDIF

       ELSE ! make sure the other is called before this one
         IF (OtherLoopCallingIndex > HalfLoopNum) THEN !rearrange
           newCallingIndex = MAX(HalfLoopNum, 1)

           IF (OtherLoopSideNum == SupplySide) THEN !if this is a supplyside, don't push it before its own demand side
             OtherLoopDemandSideCallingIndex = FindLoopSideInCallingOrder(OtherLoopNum, DemandSide)
             IF (OtherLoopDemandSideCallingIndex  < HalfLoopNum) THEN ! good to go
               NewCallingIndex = MIN(OtherLoopDemandSideCallingIndex + 1, TotNumHalfLoops) !put it right after its demand side
               CALL ShiftPlantLoopSideCallingOrder(OtherLoopCallingIndex, newCallingIndex)
             ELSE ! move both sides of other loop before this, keeping demand side in front
               NewOtherDemandSideCallingIndex = MAX(HalfLoopNum, 1)
               CALL ShiftPlantLoopSideCallingOrder(OtherLoopDemandSideCallingIndex, NewOtherDemandSideCallingIndex)
               ! get fresh pointer after it has changed in previous call
               OtherLoopCallingIndex = FindLoopSideInCallingOrder(OtherLoopNum, OtherLoopSideNum)
               newCallingIndex = NewOtherDemandSideCallingIndex + 1
               CALL ShiftPlantLoopSideCallingOrder(OtherLoopCallingIndex, newCallingIndex)
             ENDIF
           ELSE
             CALL ShiftPlantLoopSideCallingOrder(OtherLoopCallingIndex, newCallingIndex)
           ENDIF
         ENDIF
       ENDIF

     ENDDO
   ENDIF

  ENDDO

  RETURN

END SUBROUTINE RevisePlantCallingOrder


FUNCTION FindLoopSideInCallingOrder(LoopNum, LoopSide) RESULT (CallingIndex)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   April 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! locate loop and loop side in calling order structure

          ! METHODOLOGY EMPLOYED:
          ! returns integer "pointer" index to calling order structure

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: LoopNum
  INTEGER, INTENT(IN)  :: LoopSide
  INTEGER :: CallingIndex

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: HalfLoopNum

  CallingIndex = 0

  DO HalfLoopNum = 1, TotNumHalfLoops
    IF ((LoopNum == PlantCallingOrderInfo(HalfLoopNum)%LoopIndex) &
       .AND. (LoopSide == PlantCallingOrderInfo(HalfLoopNum)%LoopSide)) THEN

      CallingIndex = HalfLoopNum

    ENDIF
  ENDDO
  RETURN
END FUNCTION FindLoopSideInCallingOrder

SUBROUTINE StoreAPumpOnCurrentTempLoop(LoopNum, LoopSideNum, BranchNum, CompNum, PumpName, PumpOutletNode, HasBranchPumps)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Edwin Lee
          !       DATE WRITTEN   April 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine reallocates the pumps data structure in the loopside data structure
          !  and adds the pump data passed in as the next pumpe

          ! METHODOLOGY EMPLOYED:
    !Fills the following location items in the pump data structure which resides on the LoopSide
    ! TYPE LoopSidePumpInformation
    !   CHARACTER(len=MaxNameLength)     :: PumpName              = ' '
    !   INTEGER                          :: PumpTypeOf            = 0
    !   INTEGER                          :: BranchNum             = 0
    !   INTEGER                          :: CompNum               = 0
    !   ...

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant, ONLY: LoopSidePumpInformation !, SimPlantEquipTypes
 ! USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: LoopNum
  INTEGER, INTENT(IN) :: LoopSideNum
  INTEGER, INTENT(IN) :: BranchNum
  INTEGER, INTENT(IN) :: CompNum
  CHARACTER(*), INTENT(IN) :: PumpName
  INTEGER, INTENT(IN) :: PumpOutletNode
  LOGICAL, INTENT(IN) :: HasBranchPumps

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  TYPE(LoopSidePumpInformation), DIMENSION(:), ALLOCATABLE :: TempPumpArray
  INTEGER  :: PumpsBeforeIncrement
  INTEGER  :: PumpsAfterIncrement

  IF (ALLOCATED(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Pumps)) THEN
    PumpsBeforeIncrement = SIZE(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Pumps)
    ALLOCATE(TempPumpArray(PumpsBeforeIncrement+1))
    TempPumpArray(1:PumpsBeforeIncrement) = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Pumps
  ELSE
    PumpsBeforeIncrement = 0
    ALLOCATE(TempPumpArray(1))
  ENDIF

  PumpsAfterIncrement = SIZE(TempPumpArray)

  TempPumpArray(PumpsAfterIncrement)%PumpName   = PumpName
 ! TempPumpArray(PumpsAfterIncrement)%PumpTypeOf = FindItemInList(PumpType, SimPlantEquipTypes, SIZE(SimPlantEquipTypes))
  TempPumpArray(PumpsAfterIncrement)%BranchNum  = BranchNum
  TempPumpArray(PumpsAfterIncrement)%CompNum    = CompNum
  TempPumpArray(PumpsAfterIncrement)%PumpOutletNode = PumpOutletNode

  IF(ALLOCATED(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Pumps)) DEALLOCATE(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Pumps)
  ALLOCATE(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Pumps(PumpsAfterIncrement))
  PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Pumps             = TempPumpArray
  PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalPumps        = PumpsAfterIncrement
  PlantLoop(LoopNum)%LoopSide(LoopSideNum)%BranchPumpsExist  = HasBranchPumps
  
  IF (ALLOCATED(TempPumpArray)) DEALLOCATE(TempPumpArray)

END SUBROUTINE StoreAPumpOnCurrentTempLoop


 SUBROUTINE SetupBranchControlTypes

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   March 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! set the control types on plant branches using heuristics.
          !  Trying to obsolete branch control type  input

          ! METHODOLOGY EMPLOYED:
          ! set component control types based on component type
          !  process branches and set branch level control types based on the type of components on them
          !  Rules applied
          !   - Most component models are active
          !   - Pipes are passive unless located between splitter/mixers when assumed to be bypass
          !   - A branch with multiple active components becomes SeriesActive and so do its components
          !
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataInterfaces, ONLY: ShowSevereError

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
  INTEGER     :: LoopCtr
  INTEGER     :: LoopSideCtr
  INTEGER     :: BranchCtr
  INTEGER     :: CompCtr
  LOGICAL     :: BranchIsInSplitterMixer
  INTEGER     :: ComponentFlowCtrl
  INTEGER     :: ActiveCount
  INTEGER     :: ByPassCount
  INTEGER     :: NumComponentsOnBranch
  INTEGER     :: NumCount


  ! first set component level control type (obsoletes one input in field set for Branch )
  IF (ALLOCATED(PlantLoop)) THEN
    NumCount=SIZE(PlantLoop)
  ELSE
    NumCount=0
  ENDIF
  DO LoopCtr = 1, NumCount !SIZE(PlantLoop)
    DO LoopSideCtr = DemandSide, SupplySide
      DO BranchCtr = 1, PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%TotalBranches
        BranchIsInSplitterMixer = .FALSE.
        ! test if this branch is inside a splitter/mixer
        IF (PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%SplitterExists) THEN
          IF ((BranchCtr > 1) .and. (BranchCtr <  PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%TotalBranches)) THEN
            BranchIsInSplitterMixer = .TRUE.
          ENDIF

        ENDIF

        NumComponentsOnBranch = PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%TotalComponents

        DO CompCtr = 1, SIZE(PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp)

          SELECT CASE (PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%TypeOf_Num)

          CASE (TypeOf_Other) !                             = -1
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Unknown
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority  = LoopFlowStatus_Unknown
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_Unknown
          CASE (TypeOf_Boiler_Simple)             !         =  1
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed =HowMet_ByNominalCapHiOutLimit
          CASE (TypeOf_Boiler_Steam) !                      =  2
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_ByNominalCap
          CASE (TypeOf_Chiller_Absorption) !                =  3  ! older BLAST absorption chiller
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            IF (LoopSideCtr == DemandSide) THEN
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_TakesWhatGets
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_ByNominalCapLowOutLimit
            ENDIF
          CASE (TypeOf_Chiller_Indirect_Absorption) !       =  4  ! revised absorption chiller
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active

            IF (LoopSideCtr == DemandSide) THEN
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_TakesWhatGets
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_ByNominalCapLowOutLimit
            ENDIF
          CASE (TypeOf_Chiller_CombTurbine    ) !           =  5
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            IF (LoopSideCtr == DemandSide) THEN
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_TakesWhatGets
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_ByNominalCapLowOutLimit
            ENDIF
          CASE (TypeOf_Chiller_ConstCOP ) !                 =  6
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active

            IF (LoopSideCtr == DemandSide) THEN
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_TakesWhatGets
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_ByNominalCap
            ENDIF
          CASE (TypeOf_Chiller_DFAbsorption ) !             =  7
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            IF (LoopSideCtr == DemandSide) THEN
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyIfLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_ByNominalCapLowOutLimit
            ENDIF
          CASE (TypeOf_Chiller_ExhFiredAbsorption ) !             =  76
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            IF (LoopSideCtr == DemandSide) THEN
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyIfLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_ByNominalCapLowOutLimit
            ENDIF
          CASE (TypeOf_Chiller_Electric ) !                 =  8
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            IF (LoopSideCtr == DemandSide) THEN
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_TakesWhatGets
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_ByNominalCapLowOutLimit
            ENDIF
          CASE (TypeOf_Chiller_ElectricEIR ) !              =  9
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            IF (LoopSideCtr == DemandSide) THEN
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_TakesWhatGets
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_ByNominalCapLowOutLimit
            ENDIF
          CASE (TypeOf_Chiller_ElectricReformEIR ) !        = 10
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            IF (LoopSideCtr == DemandSide) THEN
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_TakesWhatGets
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_ByNominalCapLowOutLimit
            ENDIF
          CASE (TypeOf_Chiller_EngineDriven ) !             = 11
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_ByNominalCapLowOutLimit
            IF (LoopSideCtr == DemandSide) THEN
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_TakesWhatGets
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_ByNominalCapLowOutLimit
            ENDIF
          CASE (TypeOf_CoolingTower_SingleSpd ) !           = 12
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_ByNominalCap
          CASE (TypeOf_CoolingTower_TwoSpd ) !              = 13
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_ByNominalCap
          CASE (TypeOf_CoolingTower_VarSpd ) !              = 14
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_ByNominalCap
          CASE (TypeOf_CoolingTower_VarSpdMerkel ) !              = 89
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_ByNominalCap
          CASE (TypeOf_Generator_FCExhaust ) !              = 15
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority =   &
               LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_PassiveCap


          CASE (TypeOf_HeatPumpWtrHeater ) !                = 16
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_PassiveCap
          CASE (TypeOf_HPWaterEFCooling ) !                 = 17
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            IF (LoopSideCtr == DemandSide) THEN
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_TakesWhatGets
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_ByNominalCap
            ENDIF

          CASE (TypeOf_HPWaterEFHeating ) !                 = 18
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            IF (LoopSideCtr == DemandSide) THEN
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_TakesWhatGets
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_ByNominalCap
            ENDIF
          CASE (TypeOf_HPWaterPECooling ) !                 = 19
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            IF (LoopSideCtr == DemandSide) THEN
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyIfLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_ByNominalCap
            ENDIF
          CASE (TypeOf_HPWaterPEHeating ) !                 = 20
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            IF (LoopSideCtr == DemandSide) THEN
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyIfLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_ByNominalCap
            ENDIF
          CASE (TypeOf_Pipe ) !                             = 21
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
            IF (BranchIsInSplitterMixer) THEN
              IF (NumComponentsOnBranch == 1) THEN
                PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_ByPass
              ELSEIF (NumComponentsOnBranch >1) THEN
                PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Passive
              ELSE
                PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_ByPass
              ENDIF
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Passive
            ENDIF
          CASE (TypeOf_PipeSteam ) !                        = 22
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
            IF (BranchIsInSplitterMixer) THEN
              IF (NumComponentsOnBranch == 1) THEN
                PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_ByPass
              ELSEIF (NumComponentsOnBranch >1) THEN
                PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Passive
              ELSE
                PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_ByPass
              ENDIF
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Passive
            ENDIF
          CASE (TypeOf_PipeExterior ) !                     = 23
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
            IF (BranchIsInSplitterMixer) THEN
              IF (NumComponentsOnBranch == 1) THEN
                PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_ByPass
              ELSEIF (NumComponentsOnBranch >1) THEN
                PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Passive
              ELSE
                PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_ByPass
              ENDIF
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Passive
            ENDIF
          CASE (TypeOf_PipeInterior ) !                     = 24
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
            IF (BranchIsInSplitterMixer) THEN
              IF (NumComponentsOnBranch == 1) THEN
                PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_ByPass
              ELSEIF (NumComponentsOnBranch >1) THEN
                PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Passive
              ELSE
                PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_ByPass
              ENDIF
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Passive
            ENDIF
          CASE (TypeOf_PipeUnderground ) !                  = 25
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
            IF (BranchIsInSplitterMixer) THEN
              IF (NumComponentsOnBranch == 1) THEN
                PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_ByPass
              ELSEIF (NumComponentsOnBranch >1) THEN
                PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Passive
              ELSE
                PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_ByPass
              ENDIF
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Passive
            ENDIF
          CASE (TypeOf_PurchChilledWater ) !                = 26
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_ByNominalCapLowOutLimit
          CASE (TypeOf_PurchHotWater ) !                    = 27
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_ByNominalCapHiOutLimit
          CASE (TypeOf_TS_IceDetailed ) !                   = 28
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_NeedyIfLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_PassiveCap
          CASE (TypeOf_TS_IceSimple  ) !                    = 29
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_NeedyIfLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_PassiveCap
          CASE (TypeOf_ValveTempering  ) !                  = 30
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyIfLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
          CASE (TypeOf_WtrHeaterMixed ) !                   = 31
            IF (LoopSideCtr == DemandSide) THEN
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                          = LoopFlowStatus_NeedyAndTurnsLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                          = LoopFlowStatus_TakesWhatGets
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_PassiveCap
            ENDIF
          CASE (TypeOf_WtrHeaterStratified ) !              = 32
            IF (LoopSideCtr == DemandSide) THEN
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                          = LoopFlowStatus_NeedyAndTurnsLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                          = LoopFlowStatus_TakesWhatGets
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_PassiveCap
            ENDIF
          CASE (TypeOf_PumpVariableSpeed) !                 = 33
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
          CASE (TypeOf_PumpConstantSpeed) !                 = 34
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_NeedyIfLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
          CASE (TypeOf_PumpCondensate) !                    = 35
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
          CASE (TypeOf_PumpBankVariableSpeed) !             = 36
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyIfLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
          CASE (TypeOf_PumpBankConstantSpeed) !             = 37
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyIfLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
          CASE (TypeOf_WaterUseConnection  ) !              = 38
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
          CASE (TypeOf_CoilWaterCooling   ) !               = 39  ! demand side component
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
          CASE (TypeOf_CoilWaterDetailedFlatCooling) !      = 40  ! demand side component
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
          CASE (TypeOf_CoilWaterSimpleHeating ) !           = 41  ! demand side component
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
          CASE (TypeOf_CoilSteamAirHeating      ) !         = 42  ! demand side component
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
          CASE ( TypeOf_SolarCollectorFlatPlate ) !         = 43  ! demand side component
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_PassiveCap
          CASE ( TypeOf_PlantLoadProfile     ) !            = 44  ! demand side component
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
          CASE ( TypeOf_GrndHtExchgVertical  ) !            = 45
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_PassiveCap
          CASE ( TypeOf_GrndHtExchgSurface   ) !            = 46
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_PassiveCap
          CASE ( TypeOf_GrndHtExchgPond      ) !            = 47
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_PassiveCap

          CASE ( TypeOf_Generator_MicroTurbine ) !          = 48  !newer FSEC turbine
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_ByNominalCap
          CASE ( TypeOf_Generator_ICEngine  ) !             = 49
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                      = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_ByNominalCap
          CASE ( TypeOf_Generator_CTurbine  ) !             = 50  !older BLAST turbine
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                      = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_ByNominalCap
          CASE ( TypeOf_Generator_MicroCHP ) !              = 51
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                     = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_ByNominalCap
          CASE ( TypeOf_Generator_FCStackCooler ) !         = 52
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                     = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_ByNominalCap
          CASE ( TypeOf_FluidCooler_SingleSpd ) !           = 53
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_PassiveCap
          CASE ( TypeOf_FluidCooler_TwoSpd   ) !            = 54
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_PassiveCap
          CASE ( TypeOf_EvapFluidCooler_SingleSpd ) !       = 55
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_PassiveCap
          CASE ( TypeOf_EvapFluidCooler_TwoSpd  ) !         = 56
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_PassiveCap
          CASE ( TypeOf_ChilledWaterTankMixed   ) !         = 57
            IF (LoopSideCtr == DemandSide) THEN
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                          = LoopFlowStatus_NeedyAndTurnsLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                          = HowMet_NoneDemand
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                          = LoopFlowStatus_TakesWhatGets
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_PassiveCap
            ENDIF
          CASE ( TypeOf_ChilledWaterTankStratified ) !      = 58
            IF (LoopSideCtr == DemandSide) THEN
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                          = LoopFlowStatus_NeedyAndTurnsLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                          = HowMet_NoneDemand
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                          = LoopFlowStatus_TakesWhatGets
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_PassiveCap
            ENDIF
          CASE ( TypeOf_PVTSolarCollectorFlatPlate ) !      = 59
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_PassiveCap
          !next batch for ZoneHVAC
          CASE ( TypeOf_BASEBOARD_CONV_WATER   ) !        = 60
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
          CASE ( TypeOf_BASEBOARD_RAD_CONV_STEAM ) !      = 61
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
          CASE ( TypeOf_BASEBOARD_RAD_CONV_WATER ) !      = 62
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
          CASE ( TypeOf_LowTempRadiant_VarFlow )
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
          CASE ( TypeOf_LowTempRadiant_ConstFlow )
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
          CASE ( TypeOf_CooledBeamAirTerminal )
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
          CASE ( TypeOf_CoilWAHPHeatingEquationFit )
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
          CASE ( TypeOf_CoilWAHPCoolingEquationFit )
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
          CASE ( TypeOf_CoilVSWAHPHeatingEquationFit )
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
          CASE ( TypeOf_CoilVSWAHPCoolingEquationFit )
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
          CASE ( TypeOf_CoilWAHPHeatingParamEst )
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
          CASE ( TypeOf_CoilWAHPCoolingParamEst )
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
          CASE ( TypeOf_RefrigSystemWaterCondenser )
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_PassiveCap
          CASE ( TypeOf_RefrigerationWaterCoolRack )
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_PassiveCap
          CASE ( TypeOf_MultiSpeedHeatPumpRecovery )
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_PassiveCap
          CASE ( TypeOf_UnitarySystemRecovery )
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_PassiveCap
          CASE ( TypeOf_PipingSystemPipeCircuit )
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_PassiveCap
          CASE ( TypeOf_SolarCollectorICS ) !         = 75
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                = LoopFlowStatus_NeedyAndTurnsLoopOn
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_PassiveCap
          CASE ( Typeof_PlantComponentUserDefined )
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_Unknown
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_Unknown
          CASE ( Typeof_CoilUserDefined )
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_Unknown
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_Unknown
          CASE ( TypeOf_ZoneHVACAirUserDefined )
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_Unknown
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_Unknown
          CASE ( TypeOf_AirTerminalUserDefined )
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_Unknown
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_Unknown
          CASE (TypeOf_HeatPumpVRF) !       =  82  ! AirConditioner:VariableRefrigerantFlow
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active

            IF (LoopSideCtr == DemandSide) THEN
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
            ELSE ! should never happen
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_TakesWhatGets
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_PassiveCap
            ENDIF
          CASE (TypeOf_WaterSource) !
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_ByNominalCapLowOutLimit
          CASE (TypeOf_GrndHtExchgHorizTrench) ! = 83  GroundHeatExchanger:HorizontalTrench
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority = LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_PassiveCap
          CASE (TypeOf_FluidToFluidPlantHtExchg) !          = 84
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            IF (LoopSideCtr == DemandSide) THEN
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_NoneDemand
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_TakesWhatGets
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed = HowMet_PassiveCap
            ENDIF
          CASE (TypeOf_CentralGroundSourceHeatPump ) ! 86
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl = ControlType_Active
            IF (LoopSideCtr == DemandSide) THEN
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyAndTurnsLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_NoneDemand
            ELSE
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority &
                         = LoopFlowStatus_NeedyIfLoopOn
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         = HowMet_ByNominalCap
            ENDIF
          CASE (TypeOf_PackagedTESCoolingCoil) ! 88
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl =  ControlType_Active
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowPriority =  LoopFlowStatus_TakesWhatGets
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%HowLoadServed &
                         =  HowMet_NoneDemand
          CASE DEFAULT
            Call ShowSevereError('SetBranchControlTypes: Caught unexpected equipment type of number')

          END SELECT

        END DO
      END DO
    END DO
  END DO

  ! now set up branch control types based on components.

  IF (ALLOCATED(PlantLoop)) THEN
    NumCount=SIZE(PlantLoop)
  ELSE
    NumCount=0
  ENDIF
  DO LoopCtr = 1, NumCount !SIZE(PlantLoop)
    DO LoopSideCtr = DemandSide, SupplySide
      DO BranchCtr = 1, PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%TotalBranches
        ActiveCount = 0
        ByPassCount = 0
        DO CompCtr = 1, SIZE(PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp)
          ComponentFlowCtrl = PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp(CompCtr)%FlowCtrl

          SELECT CASE (ComponentFlowCtrl)

          CASE (ControlType_Unknown)
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%ControlType = ControlType_Passive
          CASE (ControlType_Active)
            ActiveCount = ActiveCount + 1
            IF (ActiveCount > 1) THEN
            !  assume multiple active components in series means branch is SeriesActive
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%ControlType = ControlType_SeriesActive
              ! assume all components on branch are to be SeriesActive as well
              PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Comp%FlowCtrl = ControlType_SeriesActive
            ELSE
             PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%ControlType = ControlType_Active
            ENDIF

            IF (ByPassCount > 0) THEN
              CALL ShowSevereError ('An active component is on the same branch as a pipe situated between splitter/mixer')
              CALL ShowContinueError('Occurs in Branch='//TRIM(PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Name))
              CALL ShowContinueError('Occurs in Plant Loop='//TRIM(PlantLoop(LoopCtr)%Name))
              CALL ShowContinueError('SetupBranchControlTypes: and the simulation continues')
              ! DSU3 note not sure why this is so bad.  heat transfer pipe might be a good reason to allow this?
              !   this used to fatal in older PlantFlowResolver.
            ENDIF

            ! test for active component in series with bypass
          CASE (ControlType_Bypass)

            ByPassCount = ByPassCount + 1
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%ControlType = ControlType_Bypass
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%IsBypass    = .TRUE.
            PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%ByPassExists                  = .TRUE.

            IF(CompCtr > 1) THEN
              CALL ShowSevereError ('A pipe used as a bypass should not be in series with another component')
              CALL ShowContinueError('Occurs in Branch = ' &
                                      //TRIM(PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%Name))
              CALL ShowContinueError('Occurs in PlantLoop = '//TRIM(PlantLoop(LoopCtr)%Name))
              CALL ShowFatalError('SetupBranchControlTypes: preceding condition causes termination.')
            END IF

          CASE (ControlType_Passive)
            IF (ActiveCount > 0) THEN
              ! do nothing, branch set before)
            ELSE
              IF (ByPassCount > 0) THEN

              ELSE
                PlantLoop(LoopCtr)%LoopSide(LoopSideCtr)%Branch(BranchCtr)%ControlType = ControlType_Passive
              ENDIF
            ENDIF
          CASE (ControlType_SeriesActive)
           ! do nothing, already set when more than one active component found on a branch

          END SELECT

        END DO
      END DO
    END DO
  END DO

  RETURN

END SUBROUTINE SetupBranchControlTypes

SUBROUTINE CheckIfAnyPlant

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Sept 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! determine if any plant loops will be ever be set up

          ! METHODOLOGY EMPLOYED:
          ! use input processor ot find number of plant loops

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortcuts
  USE InputProcessor , ONLY: GetNumObjectsFound
  USE DataGlobals ,    ONLY: AnyPlantInModel

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
  INTEGER :: numPlantLoopsCheck
  INTEGER :: numCondenserLoopsCheck

  cCurrentModuleObject = 'PlantLoop'
  numPlantLoopsCheck = GetNumObjectsFound(cCurrentModuleObject)

  cCurrentModuleObject = 'CondenserLoop'
  numCondenserLoopsCheck = GetNumObjectsFound(cCurrentModuleObject)

  IF ((numPlantLoopsCheck + numCondenserLoopsCheck) > 0) THEn
    AnyPlantInModel = .TRUE.
  ELSE
    AnyPlantInModel = .FALSE.
    ALLOCATE(PlantLoop(0))
  ENDIF

  RETURN

END SUBROUTINE CheckIfAnyPlant



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

END MODULE PlantManager
