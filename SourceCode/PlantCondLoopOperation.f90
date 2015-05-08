MODULE PlantCondLoopOperation

          ! MODIFIED       LKL Sep 03: adding integer/pointers for various parts of operation schemes
          ! MODIFIED       DEF JUL 10: complete re-write to support new Plant manager

          ! PURPOSE OF THIS MODULE: This module assigns loads to the equipment on
          ! the plant and condenser loops that will operate
          ! for a given timestep.

          ! METHODOLOGY EMPLOYED:  The main driver, "ManagePlantLoadDistribution",
          ! gets 'Plant Operation scheme' and 'Plant Equipment List' input.  Pointers are
          ! set up in the PlantLoop data structure to allow components to directly access the
          ! operation schemes and plant lists that the component shows up on.
          ! ManagePlantLoadDistribution is called one time for each component on the loop.
          ! It finds the operation scheme and equipment list associated with the component
          ! and calculates the component load.  If the component is part of a 'load range'
          ! based scheme, it also assigns a component load to each of the components on the
          ! equipment list.

          ! REFERENCES:

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength
USE DataInterfaces
USE DataPlant
USE DataHVACGlobals, ONLY: NumPlantLoops, NumCondLoops, SmallLoad
USE FluidProperties, ONLY: GetSpecificHeatGlycol

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS
  INTEGER, PARAMETER :: HeatingOp     = 1     ! Constant for Heating Operation
  INTEGER, PARAMETER :: CoolingOp     = 2     ! Constant for Cooling Operation
  INTEGER, PARAMETER :: DualOp        = 3     ! Constant for Cooling or Heating Operation

  LOGICAL, PARAMETER, PUBLIC :: TurnItemOn  = .TRUE.  !Convenient for calling TurnPlantItemOnOff instead of hardwired true/false
  LOGICAL, PARAMETER, PUBLIC :: TurnItemOff = .FALSE. !Convenient for calling TurnPlantItemOnOff instead of hardwired true/false

  !MODULE VARIABLE DECLARATIONS:

  !SUBROUTINE SPECIFICATIONS FOR MODULE  !SUBROUTINE SPECIFICATIONS FOR MODULE
     !Driver Routines
PUBLIC     ManagePlantLoadDistribution
     !Get Input Routines
PRIVATE    GetPlantOperationInput
PRIVATE    GetOperationSchemeInput
PRIVATE    FindRangeBasedOrUncontrolledInput
PRIVATE    FindDeltaTempRangeInput
PRIVATE    FindRangeVariable
PRIVATE    LoadEquipList
PRIVATE    FindCompSPInput
PRIVATE    GetUserDefinedOpSchemeInput
     !Initialization Routines
PUBLIC     InitLoadDistribution
     !Load Distribution/Calculation Routines
PRIVATE    DistributePlantLoad
PRIVATE    FindCompSPLoad
PRIVATE    DistributeUserDefinedPlantLoad

     !ON/OFF Utility Routines
PRIVATE    TurnOnPlantLoopPipes
PUBLIC     TurnOffLoopEquipment
PUBLIC     TurnOffLoopSideEquipment

     !PLANT EMS Utility Routines
PRIVATE    ActivateEMSControls
PUBLIC     SetupPlantEMSActuators
PRIVATE    AdjustChangeInLoadByEMSControls

CONTAINS

          ! MODULE SUBROUTINES:

! Beginning of Module Driver Subroutines
!*************************************************************************

SUBROUTINE ManagePlantLoadDistribution(LoopNum,LoopSideNum, BranchNum, CompNum, LoopDemand,RemLoopDemand,FirstHVACIteration, &
                                    LoopShutDownFlag,LoadDistributionWasPerformed)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Dan Fisher
          !       DATE WRITTEN:    April 1999
          !       REVISED:         March 2001
          !                        July 2001, Rick Strand (revision of pump and loop control code)
          !                        July 2010, Dan Fisher, complete rewrite to component based control

          ! PURPOSE OF THIS SUBROUTINE:
          ! ManageLoopOperation is the driver routine
          ! for plant equipment selection.  It calls the general "Get-
          ! Input" routines, initializes the loop pointers, then calls the
          ! appropriate type of control algorithm (setpoint, load range based,
          ! or uncontrolled) for the component

          ! METHODOLOGY EMPLOYED:
          ! na
          ! REFERENCES:
          ! na
          ! USE STATEMENTS:
  USE DataEnvironment, ONLY: OutWetBulbTemp, OutDryBulbTemp, OutDewPointTemp,  OutRelHum ! Current outdoor relative humidity [%]
  USE General,         ONLY: RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64)   , INTENT(INOUT)            :: LoopDemand
  REAL(r64)   , INTENT(INOUT)            :: RemLoopDemand
  INTEGER, INTENT(IN)                    :: LoopNum           ! PlantLoop data structure loop counter
  INTEGER, INTENT(IN)                    :: LoopSideNum       ! PlantLoop data structure loopside counter
  INTEGER, INTENT(IN)                    :: BranchNum         ! PlantLoop data structure branch counter
  INTEGER, INTENT(IN)                    :: CompNum           ! PlantLoop data structure component counter
  LOGICAL, INTENT(IN)                    :: FirstHVACIteration
  LOGICAL, INTENT(IN OUT)                :: LoopShutDownFlag  !EMS flag to tell loop solver to shut down pumps
  LOGICAL, INTENT(IN OUT), OPTIONAL      :: LoadDistributionWasPerformed

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na
          ! INTERFACE BLOCK SPECIFICATIONS
          ! na
          ! DERIVED TYPE DEFINITIONS
          ! na
          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:


  INTEGER                           :: ListNum          !DO loop index in PlantLoop()%LoopSide()%Branch()%Comp()%Opscheme()%EquipList(ListNum)
  INTEGER                           :: CurListNum       !Current list...= ListNum,  used for error checking only
    !Indices in PlantLoop()%LoopSide()%Branch()%Comp() data structure
  INTEGER                           :: CurCompLevelOpNum    !This is set by the init routine at each FirstHVACIteration.
                                                            !It tells which scheme for this component is currently scheduled
                                                            !and is used to avoid a 'schedule search' on each call
                                                            !It is used as the Opscheme index in PL()%LoopSide()%Branch()%Comp()%Opscheme(CurCompLevelOpNum)
    !Value of pointers held in PlantLoop()%LoopSide()%Branch()%Comp() data structure
    !Used as indices in PlantLoop()%OpScheme() data structure
  INTEGER                           :: CurSchemePtr     !set by PL()%LoopSide()%Branch()%Comp()%Opscheme(CurCompLevelOpNum)%OpSchemePtr
                                                        !used to locate data in PL()%Opscheme(CurSchemePtr)
  INTEGER                           :: ListPtr          !!set by PL()%LoopSide()%Branch()%Comp()%Opscheme(CurCompLevelOpNum)%EquipList(CurListNum)ListPtr
                                                        !used to locate data in PL()%Opscheme(CurSchemePtr)%EquipList(ListPtr)
    !Local values from the PlantLoop()%OpScheme() data structure
  CHARACTER(len=MaxNameLength)      :: CurSchemeTypeName !current operation scheme type
  CHARACTER(len=MaxNameLength)      :: CurSchemeName    !current operation scheme name
  INTEGER                           :: CurSchemeType    !identifier set in PlantData
  REAL(r64)                         :: RangeVariable    !holds the 'loop demand', wetbulb temp, etc.
  REAL(r64)                         :: TestRangeVariable ! abs of RangeVariable for logic tests etc.
  REAL(r64)                         :: RangeHiLimit     !upper limit of the range variable
  REAL(r64)                         :: RangeLoLimit     !lower limit of the range variable
    !Local values from the PlantLoop()%LoopSide()%Branch()%Comp() data structure
  INTEGER                           :: NumEquipLists    !number of equipment lists
    !Error control flags
  LOGICAL                           :: foundlist        !equipment list found
  LOGICAL                           :: UpperLimitTooLow  ! error processing
  REAL(r64)                         :: HighestRange      ! error processing
  INTEGER, SAVE                     :: TooLowIndex=0     ! error processing
  INTEGER, SAVE                     :: NotTooLowIndex=0  ! error processing
  !INTEGER , SAVE                    :: ErrCount = 0     !number of errors
  !CHARACTER(len=20)                 :: CharErrOut       !Error message
  INTEGER                           :: NumCompsOnList
  INTEGER                           :: CompIndex
  INTEGER                           :: EquipBranchNum
  INTEGER                           :: EquipCompNum

    !Shut down equipment and return if so instructed by LoopShutdownFlag
  IF(LoopShutdownFlag)THEN
    CALL TurnOffLoopEquipment(LoopNum)
    RETURN
  ENDIF

    !Return if there are no loop operation schemes available
  IF (.NOT. ANY(PlantLoop(LoopNum)%OpScheme%Available)) RETURN

    !Implement EMS control commands
  CALL ActivateEMSControls(LoopNum,LoopSideNum, BranchNum, CompNum, LoopShutDownFlag)

    !Schedules are checked and CurOpScheme updated on FirstHVACIteration in InitLoadDistribution
    !Here we just load CurOpScheme to a local variable
  CurCompLevelOpNum = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurCompLevelOpNum
    !If no current operation scheme for component, RETURN
  IF(CurCompLevelOpNum == 0)RETURN
    !set local variables from data structure
  NumEquipLists     =   &
     PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%OpScheme(CurCompLevelOpNum)%NumEquipLists
  CurSchemePtr      =   &
     PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%OpScheme(CurCompLevelOpNum)%OpSchemePtr
  CurSchemeType     = PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%OpSchemeType
  CurSchemeTypeName = PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%TypeOf
  CurSchemeName     = PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%Name

    !Load the 'range variable' according to the type of control scheme specified
  SELECT CASE(CurSchemeType)
    CASE(UncontrolledOpSchemeType, CompSetPtBasedSchemeType)
      CONTINUE !No RangeVariable specified for these types
    CASE (EMSOpSchemeType)
      CALL InitLoadDistribution(FirstHVACIteration)
      CONTINUE !No RangeVariable specified for these types
    CASE(HeatingRBOpSchemeType)
      ! For zero demand, we need to clean things out before we leave
      IF (LoopDemand < SmallLoad) THEN
        CALL InitLoadDistribution(FirstHVACIteration)
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Myload = 0.d0
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%On     = .FALSE.
        RETURN
      ENDIF
      RangeVariable = LoopDemand
    CASE(CoolingRBOpSchemeType)
      ! For zero demand, we need to clean things out before we leave
      IF (LoopDemand > (-1.d0 * SmallLoad)) THEN
        CALL InitLoadDistribution(FirstHVACIteration)
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Myload = 0.d0
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%On     = .FALSE.
        RETURN
      ENDIF
      RangeVariable = LoopDemand
    CASE(DryBulbRBOpSchemeType)
      RangeVariable = OutDryBulbTemp
    CASE(WetBulbRBOpSchemeType)
      RangeVariable = OutWetBulbTemp
    CASE(RelHumRBOpSchemeType)
      RangeVariable = OutRelHum
    CASE(DewpointRBOpSchemeType)
      RangeVariable = OutDewPointTemp
    CASE(DrybulbTDBOpSchemeType, WetBulbTDBOpSchemeType, DewpointTDBOpSchemeType)
      RangeVariable = FindRangeVariable(LoopNum, CurSchemePtr, CurSchemeType)
    CASE DEFAULT
      ! No controls specified.  This is a fatal error
      CALL ShowFatalError('Invalid Operation Scheme Type Requested='//TRIM(CurSchemeTypeName)// &
                          ', in ManagePlantLoadDistribution')
  END SELECT

        !Find the proper list within the specified scheme
  foundlist = .false.
  IF(CurSchemeType==UncontrolledOpSchemeType)THEN
        !!***what else do we do with 'uncontrolled' equipment?
        !There's an equipment list...but I think the idea is to just
        !Set one component to run in an 'uncontrolled' way (whatever that means!)

  ELSEIF(CurSchemeType==CompSetPtBasedSchemeType)THEN
        !check for EMS Control
    CALL TurnOnPlantLoopPipes(LoopNum,LoopSideNum)
    CALL FindCompSPLoad(LoopNum,LoopSideNum,BranchNum,CompNum,CurCompLevelOpNum)
  ELSEIF(CurSchemeType==EMSOpSchemeType) THEN
    CALL TurnOnPlantLoopPipes(LoopNum,LoopSideNum)
    CALL DistributeUserDefinedPlantLoad(LoopNum, LoopSideNum,BranchNum,CompNum,CurCompLevelOpNum,CurSchemePtr,  &
       LoopDemand,RemLoopDemand )
  ELSE !it's a range based control type with multiple equipment lists
    CurListNum = 0
    DO ListNum = 1,NumEquipLists
          !setpointers to 'PlantLoop()%OpScheme()...'structure
      ListPtr = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%  &
            OpScheme(CurCompLevelOpNum)%EquipList(ListNum)%ListPtr
      RangeHiLimit=PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(ListPtr)%RangeUpperLimit
      RangeLoLimit=PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(ListPtr)%RangeLowerLimit
      !these limits are stored with absolute values, but the LoopDemand can be negative for cooling
      TestRangeVariable = ABS(RangeVariable)

      !trying to do something where the last stage still runs the equipment but at the hi limit.

      IF (TestRangeVariable <  RangeLoLimit .OR. TestRangeVariable >  RangeHiLimit) THEN
        IF ((TestRangeVariable >  RangeHiLimit) .AND.   &
             ListPtr == (PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipListNumForLastStage)) THEN
          ! let this go thru, later AdjustChangeInLoadForLastStageUpperRangeLimit will cap dispatch to RangeHiLimit
          CurListNum = ListNum
          EXIT
        ELSE
         CYCLE
        ENDIF
      ELSE
        CurListNum = ListNum
        EXIT
      ENDIF
    ENDDO

    IF (CurListNum > 0)THEN
      ! there could be equipment on another list that needs to be nulled out, it may have a load from earlier iteration
      DO ListNum = 1,NumEquipLists
        IF (ListNum == CurListNum ) Cycle ! leave current one alone
        NumCompsOnList = PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(ListNum)%NumComps
        DO CompIndex =1, NumCompsOnList
          EquipBranchNum   =  PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(ListNum)%Comp(CompIndex)%BranchNumPtr
          EquipCompNum     =  PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(ListNum)%Comp(CompIndex)%CompNumPtr
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(EquipBranchNum)%Comp(EquipCompNum)%Myload = 0.d0
        ENDDO
      ENDDO
      IF (PlantLoop(LoopNum)%Opscheme(CurSchemePtr)%EquipList(ListPtr)%NumComps .GT. 0) THEN
        CALL TurnOnPlantLoopPipes(LoopNum, LoopSideNum)
        CALL DistributePlantLoad(LoopNum, LoopSideNum,CurSchemePtr,ListPtr,LoopDemand,RemLoopDemand)
        IF(PRESENT(LoadDistributionWasPerformed)) LoadDistributionWasPerformed = .TRUE.
      ENDIF
    ENDIF

  ENDIF  !End of range based schemes

RETURN
END SUBROUTINE ManagePlantLoadDistribution


! Beginning of GetInput subroutines for the Module
!******************************************************************************

SUBROUTINE GetPlantOperationInput(GetInputOK)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   October 1998
          !       MODIFIED       July 2010, Dan Fisher, restructure input data
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This subroutine reads the primary plant loop
          ! operation schemes from the input file

          ! METHODOLOGY EMPLOYED: calls the Input Processor to retrieve data from input file.
          ! The format of the Energy+.idd (the EnergyPlus input data dictionary) for the
          ! following keywords is reflected exactly in this subroutine:
          !    PlantEquipmentOperationSchemes
          !    CondenserEquipmentOperationSchemes

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetScheduleIndex
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, GetObjectItemNum, FindItemInList, VerifyName
  USE DataIPShortCuts  ! Data for field names, blank numerics

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT)     :: GetInputOK

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetPlantOperationInput: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na
          ! DERIVED TYPE DEFINITIONS
          ! na
          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: LoopNum            ! Loop counter (Plant or Cond)
  INTEGER :: OpNum              ! Scheme counter
  INTEGER :: Num                ! Item counter
  INTEGER :: NumPlantOpSchemes  ! Total Number of PlantEquipmentOperationSchemes
  INTEGER :: NumCondOpSchemes   ! Total Number of CondenserEquipmentOperationSchemes
  INTEGER :: NumAlphas          ! Number of alpha items in the input object
  INTEGER :: NumNums            ! Number of numeric items in the input object
  INTEGER :: IOSTAT
  CHARACTER(len=MaxNameLength) :: PlantOpSchemeName    ! Name of the plant or condenser operating scheme
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming
  CHARACTER(len=MaxNameLength) :: PlantLoopObject      ! for ease in renaming
  CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: OpSchemeNames ! Used to verify unique op scheme names
  LOGICAL :: IsNotOK
  LOGICAL :: IsBlank
  LOGICAL :: ErrorsFound  ! Passed in from OpSchemeInput

  ErrorsFound = .FALSE.

  IF(.NOT. ALLOCATED(PlantLoop))THEN
    GetInputOK = .FALSE.
    RETURN
  ELSE
    GetInputOK = .TRUE.
  ENDIF

  ! get number of operation schemes
  CurrentModuleObject ='PlantEquipmentOperationSchemes'
  NumPlantOpSchemes  = GetNumObjectsFound(CurrentModuleObject)

  IF (NumPlantOpSchemes > 0) THEN
  ! OpSchemeListNames is used to determine if there are any duplicate operation scheme names
    ALLOCATE(OpSchemeNames(NumPlantOpSchemes))
    OpSchemeNames=' '
    Num=0
    DO OpNum=1,NumPlantOpSchemes
      CALL GetObjectItem(CurrentModuleObject,OpNum,cAlphaArgs,NumAlphas, &
                     rNumericArgs,NumNums,IOSTAT)
      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),OpSchemeNames,Num,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        CYCLE
      ENDIF
      Num=Num+1
      OpSchemeNames(Num)=cAlphaArgs(1)
    ENDDO

    DEALLOCATE(OpSchemeNames)

  END IF

  CurrentModuleObject ='CondenserEquipmentOperationSchemes'
  NumCondOpSchemes  = GetNumObjectsFound(CurrentModuleObject)

  IF (NumCondOpSchemes > 0) THEN
  ! OpSchemeListNames is used to determine if there are any duplicate operation scheme names
    ALLOCATE(OpSchemeNames(NumCondOpSchemes))
    OpSchemeNames=' '
    Num=0
    DO OpNum=1,NumCondOpSchemes
      CALL GetObjectItem(CurrentModuleObject,OpNum,cAlphaArgs,NumAlphas, &
                         rNumericArgs,NumNums,IOSTAT)
      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),OpSchemeNames,Num,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        CYCLE
      ENDIF
      Num=Num+1
      OpSchemeNames(Num)=cAlphaArgs(1)
    ENDDO

    DEALLOCATE(OpSchemeNames)

  END IF

    !Load the Plant data structure
  DO LoopNum = 1, TotNumLoops
    PlantOpSchemeName = PlantLoop(LoopNum)%OperationScheme
    IF(LoopNum .LE. NumPlantLoops) THEN
     CurrentModuleObject = 'PlantEquipmentOperationSchemes'
     PlantLoopObject='PlantLoop'
    ELSE
     CurrentModuleObject = 'CondenserEquipmentOperationSchemes'
     PlantLoopObject='CondenserLoop'
    END IF
    OpNum=GetObjectItemNum(TRIM(CurrentModuleObject),PlantOpSchemeName)
    IF (OpNum > 0) THEN
      CALL GetObjectItem(CurrentModuleObject,OpNum,cAlphaArgs,NumAlphas, &
                     rNumericArgs,NumNums,IOSTAT, NumBlank=lNumericFieldBlanks, NumericFieldNames=cNumericFieldNames, &
                     AlphaBlank=lAlphaFieldBlanks,AlphaFieldNames=cAlphaFieldNames)
      PlantLoop(LoopNum)%NumOpSchemes  = (NumAlphas - 1)/3
      IF (PlantLoop(LoopNum)%NumOpSchemes .GT. 0) THEN
        ALLOCATE (PlantLoop(LoopNum)%OpScheme(PlantLoop(LoopNum)%NumOpSchemes))
        DO Num = 1, PlantLoop(LoopNum)%NumOpSchemes
          PlantLoop(LoopNum)%OpScheme(Num)%TypeOf  = cAlphaArgs(Num*3-1)

          SELECT CASE(PlantLoop(LoopNum)%OpScheme(Num)%TypeOf)

          CASE ('LOAD RANGE BASED OPERATION')  ! Deprecated
            PlantLoop(LoopNum)%OpScheme(Num)%OpSchemeType=LoadRBOpSchemeType  ! Deprecated
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(cAlphaArgs(1))//'" deprecated field value ="'//   &
               TRIM(PlantLoop(LoopNum)%OpScheme(Num)%TypeOf)//'".')
            CALL ShowContinueError('... should be replaced with PlantEquipmentOperation:CoolingLoad or '//  &
               'PlantEquipmentOperation:HeatingLoad')
          CASE ('PLANTEQUIPMENTOPERATION:COOLINGLOAD')
            PlantLoop(LoopNum)%OpScheme(Num)%OpSchemeType=CoolingRBOpSchemeType
          CASE ('PLANTEQUIPMENTOPERATION:HEATINGLOAD')
            PlantLoop(LoopNum)%OpScheme(Num)%OpSchemeType=HeatingRBOpSchemeType
          CASE ('PLANTEQUIPMENTOPERATION:COMPONENTSETPOINT') !* Temp Based Control
            PlantLoop(LoopNum)%OpScheme(Num)%OpSchemeType=CompSetPtBasedSchemeType
          CASE ('PLANTEQUIPMENTOPERATION:USERDEFINED')
            PlantLoop(LoopNum)%OpScheme(Num)%OpSchemeType=EMSOpSchemeType
            AnyEMSPlantOpSchemesInModel = .TRUE.
          CASE ('PLANTEQUIPMENTOPERATION:OUTDOORDRYBULB')
            PlantLoop(LoopNum)%OpScheme(Num)%OpSchemeType=DrybulbRBOpSchemeType
          CASE ('PLANTEQUIPMENTOPERATION:OUTDOORWETBULB')
            PlantLoop(LoopNum)%OpScheme(Num)%OpSchemeType=WetBulbRBOpSchemeType
          CASE ('PLANTEQUIPMENTOPERATION:OUTDOORDEWPOINT')
            PlantLoop(LoopNum)%OpScheme(Num)%OpSchemeType=DewpointRBOpSchemeType
          CASE ('PLANTEQUIPMENTOPERATION:OUTDOORRELATIVEHUMIDITY')
            PlantLoop(LoopNum)%OpScheme(Num)%OpSchemeType=RelHumRBOpSchemeType
          CASE ('PLANTEQUIPMENTOPERATION:OUTDOORDRYBULBDIFFERENCE')
            PlantLoop(LoopNum)%OpScheme(Num)%OpSchemeType=DrybulbTDBOpSchemeType
          CASE ('PLANTEQUIPMENTOPERATION:OUTDOORWETBULBDIFFERENCE')
            PlantLoop(LoopNum)%OpScheme(Num)%OpSchemeType=WetBulbTDBOpSchemeType
          CASE ('PLANTEQUIPMENTOPERATION:OUTDOORDEWPOINTDIFFERENCE')
            PlantLoop(LoopNum)%OpScheme(Num)%OpSchemeType=DewpointTDBOpSchemeType
          CASE ('PLANTEQUIPMENTOPERATION:UNCONTROLLED')
            PlantLoop(LoopNum)%OpScheme(Num)%OpSchemeType=UncontrolledOpSchemeType
          CASE DEFAULT ! invalid op scheme type for plant loop
            CALL ShowSevereError(RoutineName//'Invalid '//TRIM(cAlphaFieldNames(Num*3-1))//'='//TRIM(cAlphaArgs(Num*3-1))// &
                                 ', entered in '//TRIM(CurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
            ErrorsFound=.true.
          END SELECT

          PlantLoop(LoopNum)%OpScheme(Num)%Name     = cAlphaArgs(Num*3)
          PlantLoop(LoopNum)%OpScheme(Num)%Sched    = cAlphaArgs(Num*3+1)
          PlantLoop(LoopNum)%OpScheme(Num)%SchedPtr = GetScheduleIndex(PlantLoop(LoopNum)%OpScheme(Num)%Sched)
          IF (PlantLoop(LoopNum)%OpScheme(Num)%SchedPtr == 0) THEN
            CALL ShowSevereError(RoutineName//'Invalid '//TRIM(cAlphaFieldNames(Num*3+1))//' = "'//TRIM(cAlphaArgs(Num*3+1))// &
                                 '", entered in '//TRIM(CurrentModuleObject)//'= "'//TRIM(cAlphaArgs(1))//'".')
            Errorsfound=.true.
          ENDIF
        END DO
      ELSE
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(cAlphaArgs(1))// &
                             '", requires at least '//TRIM(cAlphaFieldNames(2))//', '//TRIM(cAlphaFieldNames(3))//&
                             ' and '//TRIM(cAlphaFieldNames(4))//' to be specified.')
        Errorsfound = .true.
      ENDIF
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(PlantLoopObject)//'='//TRIM(PlantLoop(LoopNum)%Name)//' is expecting')
      CALL ShowContinueError(TRIM(CurrentModuleObject)//'='//TRIM(PlantOpSchemeName)//', but not found.')
      Errorsfound = .true.
    ENDIF
  END DO

  IF (Errorsfound) THEN
    CALL ShowFatalError(RoutineName//'Errors found in getting input for PlantEquipmentOperationSchemes or '//&
                                     'CondenserEquipmentOperationSchemes')
  ENDIF

  RETURN

END SUBROUTINE GetPlantOperationInput

SUBROUTINE GetOperationSchemeInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   October 1998
          !       MODIFIED       August 2001, LKL -- Validations
          !       RE-ENGINEERED  July 2010, Dan Fisher, restructure input data

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reads the primary plant loop
          ! operation schemes from the input file

          ! METHODOLOGY EMPLOYED:
          ! calls the Input Processor to retrieve data from input file.
          ! The format of the Energy+.idd (the EnergyPlus input data dictionary) for the
          ! following keywords is reflected exactly in this subroutine:
          !    PlantEquipmentOperation:*

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString, FindItemInList
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE DataLoopNode
  USE DataSizing
  USE DataIPShortCuts

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na
          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetOperationSchemeInput: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na
          ! DERIVED TYPE DEFINITIONS
          ! na
          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: SchemeNum
  INTEGER :: Num
  INTEGER :: NumAlphas
  INTEGER :: NumNums
  INTEGER :: IOSTAT
  LOGICAL  :: IsNotOK               ! Flag to verify name
  LOGICAL  :: IsBlank               ! Flag for blank name
  LOGICAL  :: ErrorsFound  ! May be set here and passed on
  INTEGER  :: CLRBO ! Number ofCooling Load Range Based Operation Inputs
  INTEGER  :: HLRBO ! Number ofHeating Load Range Based Operation Inputs
  INTEGER  :: DBRBO ! Number ofDry Bulb Temperature Range Based Operation Inputs
  INTEGER  :: WBRBO ! Number ofWet Bulb Temperature Range Based Operation Inputs
  INTEGER  :: DPRBO ! Number ofDewPoint Temperature Range Based Operation Inputs
  INTEGER  :: RHRBO ! Number ofRelative Humidity Range Based Operation Inputs
  INTEGER  :: CSPBO ! Number of Component SetPoint Based Operation Inputs
  INTEGER  :: DBTDBO ! Number ofDry Bulb Temperature Range Based Operation Inputs
  INTEGER  :: WBTDBO ! Number ofWet Bulb Temperature Range Based Operation Inputs
  INTEGER  :: DPTDBO ! Number ofDewPoint Temperature Range Based Operation Inputs
  INTEGER  :: NumSchemes ! Number of Condenser equipment lists
  INTEGER  :: NumUncontrolledSchemes ! Number of Condenser equipment lists
  INTEGER  :: NumUserDefOpSchemes ! number of user defined EMS op schemes
  INTEGER  :: CELists ! Number of Condenser equipment lists
  INTEGER  :: PELists ! Number of Plant equipment lists
  INTEGER  :: Count ! Loop counter
  INTEGER  :: NumSchemeLists
  INTEGER  :: LoopNum
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming.
  CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: TempVerifyNames

  ErrorsFound = .FALSE.    !DSU CS


    !**********VERIFY THE 'PLANTEQUIPMENTOPERATION:...' KEYWORDS**********
    CLRBO = GetNumObjectsFound('PlantEquipmentOperation:CoolingLoad')
    HLRBO = GetNumObjectsFound('PlantEquipmentOperation:HeatingLoad')
    DBRBO = GetNumObjectsFound('PlantEquipmentOperation:OutdoorDryBulb')
    WBRBO = GetNumObjectsFound('PlantEquipmentOperation:OutdoorWetBulb')
    DPRBO = GetNumObjectsFound('PlantEquipmentOperation:OutdoorDewpoint')
    RHRBO = GetNumObjectsFound('PlantEquipmentOperation:OutdoorRelativeHumidity')
    CSPBO = GetNumObjectsFound('PlantEquipmentOperation:ComponentSetpoint')   !* Temp Based Control
    NumUserDefOpSchemes = GetNumObjectsFound('PlantEquipmentOperation:UserDefined' )
    DBTDBO = GetNumObjectsFound('PlantEquipmentOperation:OutdoorDryBulbDifference')
    WBTDBO = GetNumObjectsFound('PlantEquipmentOperation:OutdoorWetBulbDifference')
    DPTDBO = GetNumObjectsFound('PlantEquipmentOperation:OutdoorDewpointDifference')
    NumSchemes = CLRBO+HLRBO+DBRBO+WBRBO+DPRBO+RHRBO+CSPBO+DBTDBO+WBTDBO+DPTDBO+NumUserDefOpSchemes
    NumUncontrolledSchemes = GetNumObjectsFound('PlantEquipmentOperation:Uncontrolled')
    IF ( (NumSchemes + NumUncontrolledSchemes) .le. 0 ) THEN
      CALL ShowFatalError('No PlantEquipmentOperation:* objects specified. Stop simulation.')
    END IF

  ! test for blank or duplicates -- this section just determines if there are any duplicate operation scheme names
  ALLOCATE(TempVerifyNames(NumSchemes))
  TempVerifyNames=' '

    !Check for existence of duplicates in keyword names
 Count = 0
 DO Num = 1,NumSchemes
   IF (CLRBO > 0 .AND. Num <=CLRBO)THEN
     CurrentModuleObject ='PlantEquipmentOperation:CoolingLoad'
     Count = Num
   ELSEIF(HLRBO > 0 .AND. Num <=(CLRBO+HLRBO))THEN
     CurrentModuleObject ='PlantEquipmentOperation:HeatingLoad'
     Count = Num-CLRBO
   ELSEIF(DBRBO > 0 .AND. Num <=(CLRBO+HLRBO+DBRBO))THEN
     CurrentModuleObject ='PlantEquipmentOperation:OutdoorDryBulb'
     Count = Num-CLRBO-HLRBO
   ELSEIF(WBRBO > 0 .AND. Num <=(CLRBO+HLRBO+DBRBO+WBRBO))THEN
     CurrentModuleObject ='PlantEquipmentOperation:OutdoorWetBulb'
     Count = Num-CLRBO-HLRBO-DBRBO
   ELSEIF(DPRBO > 0 .AND. Num <=(CLRBO+HLRBO+DBRBO+WBRBO+DPRBO))THEN
     CurrentModuleObject ='PlantEquipmentOperation:OutdoorDewpoint'
     Count = Num-CLRBO-HLRBO-DBRBO-WBRBO
   ELSEIF(RHRBO > 0 .AND. Num <=(CLRBO+HLRBO+DBRBO+WBRBO+DPRBO+RHRBO))THEN
     CurrentModuleObject ='PlantEquipmentOperation:OutdoorRelativeHumidity'
     Count = Num-CLRBO-HLRBO-DBRBO-WBRBO-DPRBO
   ELSEIF(CSPBO > 0 .AND. Num <=(CLRBO+HLRBO+DBRBO+WBRBO+DPRBO+RHRBO+CSPBO))THEN
     CurrentModuleObject ='PlantEquipmentOperation:ComponentSetpoint'
     Count = Num-CLRBO-HLRBO-DBRBO-WBRBO-DPRBO-RHRBO
   ELSEIF(DBTDBO > 0 .AND. Num <=(CLRBO+HLRBO+DBRBO+WBRBO+DPRBO+RHRBO+CSPBO+DBTDBO))THEN
     CurrentModuleObject ='PlantEquipmentOperation:OutdoorDryBulbDifference'
     Count = Num-CLRBO-HLRBO-DBRBO-WBRBO-DPRBO-RHRBO-CSPBO
   ELSEIF(WBTDBO > 0 .AND. Num <=(CLRBO+HLRBO+DBRBO+WBRBO+DPRBO+RHRBO+CSPBO+DBTDBO+WBTDBO))THEN
     CurrentModuleObject ='PlantEquipmentOperation:OutdoorWetBulbDifference'
     Count = Num-CLRBO-HLRBO-DBRBO-WBRBO-DPRBO-RHRBO-CSPBO-DBTDBO
   ELSEIF(DPTDBO > 0 .AND. Num <=(CLRBO+HLRBO+DBRBO+WBRBO+DPRBO+RHRBO+CSPBO+DBTDBO+WBTDBO+DPTDBO))THEN
     CurrentModuleObject ='PlantEquipmentOperation:OutdoorDewpointDifference'
     Count = Num-CLRBO-HLRBO-DBRBO-WBRBO-DPRBO-RHRBO-CSPBO-DBTDBO-WBTDBO
   ELSEIF(NumUncontrolledSchemes > 0 .AND. &
          Num <=(CLRBO+HLRBO+DBRBO+WBRBO+DPRBO+RHRBO+CSPBO+DBTDBO+WBTDBO+DPTDBO + NumUncontrolledSchemes))THEN
     CurrentModuleObject ='PlantEquipmentOperation:Uncontrolled'
     Count = Num-CLRBO-HLRBO-DBRBO-WBRBO-DPRBO-RHRBO-CSPBO-DBTDBO-WBTDBO-DPTDBO
   ELSEIF(NumUserDefOpSchemes > 0 .AND. &
          Num <=(CLRBO+HLRBO+DBRBO+WBRBO+DPRBO+RHRBO+CSPBO+DBTDBO+WBTDBO+DPTDBO + NumUncontrolledSchemes + NumUserDefOpSchemes))THEN
     CurrentModuleObject ='PlantEquipmentOperation:UserDefined'
     Count = Num-CLRBO-HLRBO-DBRBO-WBRBO-DPRBO-RHRBO-CSPBO-DBTDBO-WBTDBO-DPTDBO-NumUncontrolledSchemes
   ELSE
     CALL ShowFatalError('Error in control scheme identification')
   ENDIF

   CALL GetObjectItem(CurrentModuleObject,Count,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT)
   IsNotOK=.false.
   IsBlank=.false.
   CALL VerifyName(cAlphaArgs(1),TempVerifyNames,Num-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
   IF (IsNotOK) THEN
     ErrorsFound=.true.
     CYCLE
   ENDIF
   TempVerifyNames(Num)=cAlphaArgs(1)

 ENDDO
 DEALLOCATE(TempVerifyNames)

 !**********VERIFY THE 'PlantEquipmentList' AND 'CondenserEquipmentList' KEYWORDS*********
 PELists = GetNumObjectsFound('PlantEquipmentList')
 CELists = GetNumObjectsFound('CondenserEquipmentList')
 NumSchemeLists = PELists + CELists
 ALLOCATE(TempVerifyNames(NumSchemeLists))
 TempVerifyNames=' '
 count = 0
 DO Num = 1,NumSchemeLists
   IF (Num <=PELists)THEN
     CurrentModuleObject ='PlantEquipmentList'
     Count = Num
   ELSE
     CurrentModuleObject ='CondenserEquipmentList'
     Count = Num-PeLists
   ENDIF
   CALL GetObjectItem(CurrentModuleObject,Count,cAlphaArgs,NumAlphas, &
                    rNumericArgs,NumNums,IOSTAT)
     IsNotOK=.false.
     IsBlank=.false.
     CALL VerifyName(cAlphaArgs(1),TempVerifyNames,Num-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
     IF (IsNotOK) THEN
       ErrorsFound=.true.
       CYCLE
     ENDIF
    TempVerifyNames(Num)=cAlphaArgs(1)
 ENDDO
 DEALLOCATE(TempVerifyNames)

    !**********GET INPUT AND LOAD PLANT DATA STRUCTURE*********

    !extend number of equipment lists to include one for each CSPBO
  NumSchemeLists =NumSchemeLists+CSPBO+NumUserDefOpSchemes
  DO LoopNum = 1, TotNumLoops
    DO SchemeNum = 1, PlantLoop(LoopNum)%NumOpSchemes

      SELECT CASE(PlantLoop(LoopNum)%OpScheme(SchemeNum)%TypeOf)

      CASE ('PLANTEQUIPMENTOPERATION:COOLINGLOAD')
        CurrentModuleObject = 'PlantEquipmentOperation:CoolingLoad'
        CALL FindRangeBasedOrUncontrolledInput(CurrentModuleObject,CLRBO,LoopNum,SchemeNum,ErrorsFound)

      CASE ('PLANTEQUIPMENTOPERATION:HEATINGLOAD')
        CurrentModuleObject = 'PlantEquipmentOperation:HeatingLoad'
        CALL FindRangeBasedOrUncontrolledInput(CurrentModuleObject,HLRBO,LoopNum,SchemeNum,ErrorsFound)

      CASE ('PLANTEQUIPMENTOPERATION:COMPONENTSETPOINT') !* Temp Based Control
        CurrentModuleObject = 'PlantEquipmentOperation:ComponentSetPoint'
        CALL FindCompSPInput(CurrentModuleObject,CSPBO,LoopNum,SchemeNum,ErrorsFound)

      CASE ('PLANTEQUIPMENTOPERATION:USERDEFINED')
        CurrentModuleObject = 'PlantEquipmentOperation:UserDefined'
        CALL GetUserDefinedOpSchemeInput(CurrentModuleObject,NumUserDefOpSchemes,LoopNum,SchemeNum,ErrorsFound)

      CASE ('PLANTEQUIPMENTOPERATION:OUTDOORDRYBULB')
        CurrentModuleObject = 'PlantEquipmentOperation:OutdoorDryBulb'
        CALL FindRangeBasedOrUncontrolledInput(CurrentModuleObject,DBRBO,LoopNum,SchemeNum,ErrorsFound)

      CASE ('PLANTEQUIPMENTOPERATION:OUTDOORWETBULB')
        CurrentModuleObject = 'PlantEquipmentOperation:OutdoorWetBulb'
        CALL FindRangeBasedOrUncontrolledInput(CurrentModuleObject,WBRBO,LoopNum,SchemeNum,ErrorsFound)

      CASE ('PLANTEQUIPMENTOPERATION:OUTDOORDEWPOINT')
        CurrentModuleObject = 'PlantEquipmentOperation:OutdoorDewPoint'
        CALL FindRangeBasedOrUncontrolledInput(CurrentModuleObject,DPRBO,LoopNum,SchemeNum,ErrorsFound)

      CASE ('PLANTEQUIPMENTOPERATION:OUTDOORRELATIVEHUMIDITY')
        CurrentModuleObject = 'PlantEquipmentOperation:OutdoorrelativeHumidity'
        CALL FindRangeBasedOrUncontrolledInput(CurrentModuleObject,RHRBO,LoopNum,SchemeNum,ErrorsFound)

      CASE ('PLANTEQUIPMENTOPERATION:OUTDOORDRYBULBDIFFERENCE')
        CurrentModuleObject = 'PlantEquipmentOperation:OutdoorDryBulbDifference'
        CALL FindDeltaTempRangeInput(CurrentModuleObject,DBTDBO,LoopNum,SchemeNum,ErrorsFound)

      CASE ('PLANTEQUIPMENTOPERATION:OUTDOORWETBULBDIFFERENCE')
        CurrentModuleObject = 'PlantEquipmentOperation:OutdoorWetBulbDifference'
        CALL FindDeltaTempRangeInput(CurrentModuleObject,WBTDBO,LoopNum,SchemeNum,ErrorsFound)

      CASE ('PLANTEQUIPMENTOPERATION:OUTDOORDEWPOINTDIFFERENCE')
        CurrentModuleObject = 'PlantEquipmentOperation:OutdoorDewPointDifference'
        CALL FindDeltaTempRangeInput(CurrentModuleObject,DPTDBO,LoopNum,SchemeNum,ErrorsFound)

      CASE ('PLANTEQUIPMENTOPERATION:UNCONTROLLED')
        CurrentModuleObject = 'PlantEquipmentOperation:Uncontrolled'
        CALL FindRangeBasedOrUncontrolledInput(CurrentModuleObject,NumUncontrolledSchemes,LoopNum,SchemeNum,ErrorsFound)

      CASE DEFAULT ! invalid op scheme type for plant loop
                   ! DSU?  Seems like the alpha args below is incorrect....
        CALL ShowSevereError('Invalid operation scheme type = "'//TRIM(cAlphaArgs(Num*3-1))// &
                             '", entered in '//TRIM(CurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.true.
      END SELECT
    ENDDO
  ENDDO

  ! Validate that component names/types in each list correspond to a valid component in input file
  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Errors found getting inputs. Previous error(s) cause program termination.')
  ENDIF
  RETURN
END SUBROUTINE GetOperationSchemeInput

SUBROUTINE FindRangeBasedOrUncontrolledInput(CurrentModuleObject,NumSchemes,LoopNum,SchemeNum,ErrorsFound)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   July 2010
          !       MODIFIED       Chandan Sharma, August 2010
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Load range based or uncontrolled input into PLANTLOOP data structure

          ! METHODOLOGY EMPLOYED:
          ! calls the Input Processor to retrieve data from input file.
          ! The format of the Energy+.idd (the EnergyPlus input data dictionary) for the
          ! following keywords is reflected exactly in this subroutine:
          !       PlantEquipmentOperation:CoolingLoad
          !       PlantEquipmentOperation:HeatingLoad
          !       PlantEquipmentOperation:OutdoorDryBulb
          !       PlantEquipmentOperation:OutdoorWetBulb
          !       PlantEquipmentOperation:OutdoorDewPoint
          !       PlantEquipmentOperation:OutdoorRelativeHumidity
          !       PlantEquipmentOperation:Uncontrolled

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetObjectItem, SameString, GetObjectDefMaxArgs
  USE General,        ONLY: RoundSigDigits

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT)    :: ErrorsFound  ! May be set here and passed on
  INTEGER, INTENT(IN)       :: LoopNum      ! May be set here and passed on
  INTEGER, INTENT(IN)       :: SchemeNum    ! May be set here and passed on
  INTEGER, INTENT(IN)       :: NumSchemes   ! May be set here and passed on
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER   :: Plant     = 1     ! Used to identify whether the current loop is Plant
  INTEGER, PARAMETER   :: Condenser = 2     ! Used to identify whether the current loop is Condenser

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na
          ! DERIVED TYPE DEFINITIONS
          ! na
          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumAlphas
  INTEGER :: NumNums
  INTEGER :: IOSTAT
  CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: AlphArray      ! Alpha input items for object
  CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
  CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: NumArray          ! Numeric input items for object
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.
  INTEGER   :: TotalArgs=0        ! Total number of alpha and numeric arguments (max) for a
                                  !   certain object in the input file
  INTEGER :: Num
  INTEGER :: NumEquipLists
  INTEGER :: ListNum
  CHARACTER(len=MaxNameLength) :: LoopOpSchemeObj    ! Used to identify the object name for loop equipment operation scheme
  LOGICAL :: SchemeNameFound ! Set to FALSE if a match of OpScheme object and OpScheme name is not found
  INTEGER :: InnerListNum  !inner loop list number
  REAL(r64) :: OuterListNumLowerLimit
  REAL(r64) :: OuterListNumUpperLimit
  REAL(r64) :: InnerListNumLowerLimit
  REAL(r64) :: InnerListNumUpperLimit

  SchemeNameFound = .TRUE.

! Determine max number of alpha and numeric arguments for all objects being read, in order to allocate local arrays
  CALL GetObjectDefMaxArgs(CurrentModuleObject,TotalArgs,NumAlphas,NumNums)

  ALLOCATE(AlphArray(NumAlphas))
  AlphArray=' '
  ALLOCATE(cAlphaFields(NumAlphas))
  cAlphaFields=' '
  ALLOCATE(cNumericFields(NumNums))
  cNumericFields=' '
  ALLOCATE(NumArray(NumNums))
  NumArray=0.0d0
  ALLOCATE(lAlphaBlanks(NumAlphas))
  lAlphaBlanks=.TRUE.
  ALLOCATE(lNumericBlanks(NumNums))
  lNumericBlanks=.TRUE.

  IF (PlantLoop(LoopNum)%TypeofLoop == Plant) THEN
    LoopOpSchemeObj ='PlantEquipmentOperationSchemes'
  ELSEIF (PlantLoop(LoopNum)%TypeofLoop == Condenser) THEN
    LoopOpSchemeObj ='CondenserEquipmentOperationSchemes'
  ENDIF

  IF (NumSchemes .GT. 0) THEN
    DO Num = 1, NumSchemes
      CALL GetObjectItem(CurrentModuleObject,Num, &
                        AlphArray,NumAlphas,NumArray,NumNums,IOSTAT)
      IF(SameString(PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name,AlphArray(1))) EXIT
      IF (Num == NumSchemes) THEN
        CALL ShowSevereError(TRIM(LoopOpSchemeObj)//' = "'//TRIM(PlantLoop(LoopNum)%OperationScheme)// &
                             '", could not find '// &
                            TRIM(CurrentModuleObject)//' = "'//TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name)//'".')
        ErrorsFound = .true.
        SchemeNameFound = .FALSE.
      ENDIF
    ENDDO
    IF (SchemeNameFound) THEN
      PlantLoop(LoopNum)%OpScheme(SchemeNum)%NumEquipLists   = (NumAlphas-1)
      IF (PlantLoop(LoopNum)%OpScheme(SchemeNum)%NumEquipLists <= 0) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(AlphArray(1))// &
                             '", specified without equipment list.')
        ErrorsFound = .true.
      ELSE
        ALLOCATE (PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList &
                 (PlantLoop(LoopNum)%OpScheme(SchemeNum)%NumEquipLists))
        NumEquipLists = PlantLoop(LoopNum)%OpScheme(SchemeNum)%NumEquipLists
        IF (NumNums .LE. 0) THEN     ! Uncontrolled OpScheme type
          ListNum = NumEquipLists    ! NumEquipLists is always 1 for Uncontrolled OpScheme type
          PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%name = AlphArray(2)
          CALL LoadEquipList(LoopNum,SchemeNum,ListNum,ErrorsFound)
        ELSE                         ! Range based OpScheme type
          DO ListNum = 1, NumEquipLists
            PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%RangeLowerLimit  = NumArray(ListNum*2-1)
            PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%RangeUpperLimit  = NumArray(ListNum*2)
            PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%name = AlphArray(ListNum+1)
            IF (PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%RangeUpperLimit < 0.d0) THEN
              CALL ShowSevereError(TRIM(LoopOpSchemeObj)//' = "'//TRIM(PlantLoop(LoopNum)%OperationScheme)// &
                          '", found a negative value for an upper limit in '// &
                         TRIM(CurrentModuleObject)//' = "'//TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name)//'".')
              ErrorsFound=.true.
            ENDIF

            SELECT CASE (TRIM(CurrentModuleObject)) ! different op schemes have different lower limit check values

            CASE ('PlantEquipmentOperation:CoolingLoad','PlantEquipmentOperation:HeatingLoad',   &
               'PlantEquipmentOperation:OutdoorrelativeHumidity')
              ! these should not be less than zero
              IF (PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%RangeLowerLimit < 0.d0) THEN
                CALL ShowSevereError(TRIM(LoopOpSchemeObj)//' = "'//TRIM(PlantLoop(LoopNum)%OperationScheme)// &
                            '", found a negative value for a lower limit in '// &
                           TRIM(CurrentModuleObject)//' = "'//TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name)//'".')
                ErrorsFound=.true.
              ENDIF
            CASE DEFAULT
              ! others should not be less than -70
              IF (PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%RangeLowerLimit < -70.d0) THEN
                CALL ShowSevereError(TRIM(LoopOpSchemeObj)//' = "'//TRIM(PlantLoop(LoopNum)%OperationScheme)// &
                            '", found too low of a value for a lower limit in '// &
                           TRIM(CurrentModuleObject)//' = "'//TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name)//'".')
                ErrorsFound=.true.
              ENDIF
            END SELECT

            IF (PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%RangeLowerLimit > &
                  PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%RangeUpperLimit  ) THEN
              CALL ShowSevereError(TRIM(LoopOpSchemeObj)//' = "'//TRIM(PlantLoop(LoopNum)%OperationScheme)// &
                          '", found a lower limit that is higher than an upper limit in '// &
                         TRIM(CurrentModuleObject)//' = "'//TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name)//'".')
              ErrorsFound=.true.
            ENDIF

            CALL LoadEquipList(LoopNum,SchemeNum,ListNum,ErrorsFound)
          END DO
          ! now run through lists again and check that range limits do not overlap each other
          DO ListNum = 1, NumEquipLists
            OuterListNumLowerLimit = PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%RangeLowerLimit
            OuterListNumUpperLimit = PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%RangeUpperLimit
            DO InnerListNum = 1, NumEquipLists
              IF (InnerListNum == ListNum) CYCLE ! don't check against self. 
              InnerListNumLowerLimit = PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(InnerListNum)%RangeLowerLimit
              InnerListNumUpperLimit = PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(InnerListNum)%RangeUpperLimit
              ! Check if inner list has a lower limit that is between an outer's lower and upper limit
              IF (  InnerListNumLowerLimit > OuterListNumLowerLimit .AND. InnerListNumLowerLimit < OuterListNumUpperLimit ) THEN
                CALL ShowWarningError(TRIM(LoopOpSchemeObj)//' = "'//TRIM(PlantLoop(LoopNum)%OperationScheme)// &
                                   '", detected overlapping ranges in '// &
                         TRIM(CurrentModuleObject)//' = "'//TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name)//'".')
                CALL ShowContinueError('Range # '//Trim(RoundSigDigits(InnerListNum,0))//' Lower limit = ' &
                                        //TRIM(RoundSigDigits(InnerListNumLowerLimit,1)) // &
                                       ' lies within the Range # '//TRIM(RoundSigDigits(ListNum,1)) &
                                       //' ('//TRIM(RoundSigDigits(OuterListNumLowerLimit,1))//' to ' &
                                         //TRIM(RoundSigDigits(OuterListNumUpperLimit ,1)) // &
                                        ').')
                CALL ShowContinueError('Check that input for load range limit values do not overlap, ' &
                                      // 'and the simulation continues...')

              ENDIF
              ! Check if inner list has an upper limit that is between an outer's lower and upper limit
              IF (  InnerListNumUpperLimit > OuterListNumLowerLimit .AND. InnerListNumUpperLimit < OuterListNumUpperLimit ) THEN
                CALL ShowWarningError(TRIM(LoopOpSchemeObj)//' = "'//TRIM(PlantLoop(LoopNum)%OperationScheme)// &
                                   '", detected overlapping ranges in '// &
                         TRIM(CurrentModuleObject)//' = "'//TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name)//'".')
                CALL ShowContinueError('Range # '//Trim(RoundSigDigits(InnerListNum,0))//' Upper limit = '&
                                       //TRIM(RoundSigDigits(InnerListNumUpperLimit,1)) // &
                                       ' lies within Range # '//TRIM(RoundSigDigits(ListNum,1)) &
                                         // ' ('//TRIM(RoundSigDigits(OuterListNumLowerLimit,1))//' to ' &
                                         //TRIM(RoundSigDigits(OuterListNumUpperLimit ,1)) // &
                                        ').')
                CALL ShowContinueError('Check that input for load range limit values do not overlap, ' &
                                        // 'and the simulation continues...')

              ENDIF
            ENDDO
          ENDDO

        ENDIF
      ENDIF
    ENDIF
  ELSE
    CALL ShowSevereError(TRIM(LoopOpSchemeObj)//' = "'//TRIM(PlantLoop(LoopNum)%OperationScheme)// &
                          '", could not find '// &
                         TRIM(CurrentModuleObject)//' = "'//TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name)//'".')
    ErrorsFound=.true.
  ENDIF

  DEALLOCATE(AlphArray)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(NumArray)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(lNumericBlanks)

  RETURN

END SUBROUTINE FindRangeBasedOrUncontrolledInput

SUBROUTINE FindDeltaTempRangeInput(CurrentModuleObject,NumSchemes,LoopNum,SchemeNum,ErrorsFound)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   August 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Load range based input into PLANTLOOP data structure

          ! METHODOLOGY EMPLOYED:
          ! calls the Input Processor to retrieve data from input file.
          ! The format of the Energy+.idd (the EnergyPlus input data dictionary) for the
          ! following keywords is reflected exactly in this subroutine:
          !       PlantEquipmentOperation:OutdoorDryBulbDifference
          !       PlantEquipmentOperation:OutdoorWetBulbDifference
          !       PlantEquipmentOperation:OutdoorDewPointDifference

          ! REFERENCES:
          ! Based on subroutine FindRangeBasedOrUncontrolledInput from Dan Fisher, July 2010

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetObjectItem, SameString, GetObjectDefMaxArgs
  USE NodeInputManager,      ONLY: GetOnlySingleNode
  USE DataLoopNode

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT)    :: ErrorsFound  ! May be set here and passed on
  INTEGER, INTENT(IN)       :: LoopNum      ! May be set here and passed on
  INTEGER, INTENT(IN)       :: SchemeNum    ! May be set here and passed on
  INTEGER, INTENT(IN)       :: NumSchemes   ! May be set here and passed on
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER   :: Plant     = 1     ! Used to identify whether the current loop is Plant
  INTEGER, PARAMETER   :: Condenser = 2     ! Used to identify whether the current loop is Condenser

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na
          ! DERIVED TYPE DEFINITIONS
          ! na
          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumAlphas
  INTEGER :: NumNums
  INTEGER :: IOSTAT
  CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: AlphArray      ! Alpha input items for object
  CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
  CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: NumArray          ! Numeric input items for object
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.
  INTEGER   :: TotalArgs=0        ! Total number of alpha and numeric arguments (max) for a
                                  !   certain object in the input file
  INTEGER :: Num
  INTEGER :: NumEquipLists
  INTEGER :: ListNum
  CHARACTER(len=MaxNameLength) :: LoopOpSchemeObj    ! Used to identify the object name for loop equipment operation scheme
  LOGICAL :: SchemeNameFound ! Set to FALSE if a match of OpScheme object and OpScheme name is not found

  SchemeNameFound = .TRUE.

! Determine max number of alpha and numeric arguments for all objects being read, in order to allocate local arrays
  CALL GetObjectDefMaxArgs(CurrentModuleObject,TotalArgs,NumAlphas,NumNums)

  ALLOCATE(AlphArray(NumAlphas))
  AlphArray=' '
  ALLOCATE(cAlphaFields(NumAlphas))
  cAlphaFields=' '
  ALLOCATE(cNumericFields(NumNums))
  cNumericFields=' '
  ALLOCATE(NumArray(NumNums))
  NumArray=0.0d0
  ALLOCATE(lAlphaBlanks(NumAlphas))
  lAlphaBlanks=.TRUE.
  ALLOCATE(lNumericBlanks(NumNums))
  lNumericBlanks=.TRUE.

  IF (PlantLoop(LoopNum)%TypeofLoop == Plant) THEN
    LoopOpSchemeObj = 'PlantEquipmentOperationSchemes'
  ELSEIF (PlantLoop(LoopNum)%TypeofLoop == Condenser) THEN
    LoopOpSchemeObj = 'CondenserEquipmentOperationSchemes'
  ENDIF

  IF (NumSchemes .GT. 0) THEN
    DO Num = 1, NumSchemes
      CALL GetObjectItem(CurrentModuleObject,Num, &
                        AlphArray,NumAlphas,NumArray,NumNums,IOSTAT)
      IF(SameString(PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name,AlphArray(1))) EXIT
      IF (Num == NumSchemes) THEN
        CALL ShowSevereError(TRIM(LoopOpSchemeObj)//' = "'//TRIM(PlantLoop(LoopNum)%OperationScheme)// &
                             '", could not find '// &
                            TRIM(CurrentModuleObject)//' = "'//TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name)//'".')
        ErrorsFound = .true.
        SchemeNameFound = .FALSE.
      ENDIF
    ENDDO
    IF (SchemeNameFound) THEN
      PlantLoop(LoopNum)%OpScheme(SchemeNum)%NumEquipLists   = (NumAlphas-2)
      IF (PlantLoop(LoopNum)%OpScheme(SchemeNum)%NumEquipLists <= 0) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(AlphArray(1))// &
                            '", specified without equipment list.')
        ErrorsFound = .true.
      ELSE
        ALLOCATE (PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList &
                     (PlantLoop(LoopNum)%OpScheme(SchemeNum)%NumEquipLists))
        NumEquipLists=PlantLoop(LoopNum)%OpScheme(SchemeNum)%NumEquipLists
        PlantLoop(LoopNum)%OpScheme(SchemeNum)%ReferenceNodeName = AlphArray(2)
        PlantLoop(LoopNum)%OpScheme(SchemeNum)%ReferenceNodeNumber =   &
                 GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                 NodeType_Water,NodeConnectionType_Sensor, 1, ObjectIsNotParent)
        !For DO Loop below -- Check for lower limit > upper limit.(invalid)
        DO ListNum = 1, NumEquipLists
          PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%RangeLowerLimit  = NumArray(ListNum*2-1)
          PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%RangeUpperLimit  = NumArray(ListNum*2)
          PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%name= AlphArray(ListNum+2)
          IF (PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%RangeLowerLimit > &
                PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%RangeUpperLimit  ) THEN
            CALL ShowSevereError(TRIM(LoopOpSchemeObj)//' = "'//TRIM(PlantLoop(LoopNum)%OperationScheme)// &
                      '", found a lower limit that is higher than an upper limit in '// &
                       TRIM(CurrentModuleObject)//' = "'//TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name)//'".')
            ErrorsFound=.true.
          ENDIF
          CALL LoadEquipList(LoopNum,SchemeNum,ListNum,ErrorsFound)
        END DO
      ENDIF
    ENDIF
  ELSE
    CALL ShowSevereError(TRIM(LoopOpSchemeObj)//' = "'//TRIM(PlantLoop(LoopNum)%OperationScheme)// &
                         '", could not find '// &
                         TRIM(CurrentModuleObject)//' = "'//TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name)//'".')
    ErrorsFound = .true.
  ENDIF

  DEALLOCATE(AlphArray)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(NumArray)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(lNumericBlanks)

  RETURN

END SUBROUTINE FindDeltaTempRangeInput

SUBROUTINE LoadEquipList(LoopNum,SchemeNum,ListNum,ErrorsFound)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   July 2010
          !       MODIFIED       B. Griffith Sept 2011, major rewrite
          !                      allow mixing list types across plant types, store info first time
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Load delta range based input into PLANTLOOP data structure

          ! METHODOLOGY EMPLOYED:
          ! calls the Input Processor to retrieve data from input file.

          ! REFERENCES:
          ! na
          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, SameString
  USE DataIPShortCuts

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT)    :: ErrorsFound    ! May be set here and passed on
  INTEGER, INTENT(IN)       :: LoopNum        ! May be set here and passed on
  INTEGER, INTENT(IN)       :: SchemeNum      ! May be set here and passed on
  INTEGER, INTENT(IN)       :: ListNum        ! May be set here and passed on

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na
          ! DERIVED TYPE DEFINITIONS
          ! na
          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE :: MyOneTimeFlag = .TRUE.
  LOGICAL :: FoundIntendedList
  INTEGER :: Num
  INTEGER :: MachineNum
  INTEGER :: PELists
  INTEGER :: CELists
!  INTEGER :: NumLists
  INTEGER :: NumAlphas
  INTEGER :: NumNums
  INTEGER :: IOSTAT
  LOGICAL :: IsNotOK
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject
  INTEGER, SAVE :: TotNumLists = 0
  CHARACTER(len=MaxNameLength), SAVE, DIMENSION(:), ALLOCATABLE :: EquipListsNameList
  INTEGER, SAVE,  DIMENSION(:), ALLOCATABLE :: EquipListsTypeList
  INTEGER, SAVE,  DIMENSION(:), ALLOCATABLE :: EquipListsIndexList
  INTEGER :: iIndex
  LOGICAL :: firstblank

  IF (MyOneTimeFlag) THEN
    ! assemble mapping between list names and indices one time
    PELists = GetNumObjectsFound('PlantEquipmentList')
    CELists = GetNumObjectsFound('CondenserEquipmentList')
    TotNumLists = PELists + CELists
    IF (TotNumLists > 0) THEN
      ALLOCATE(EquipListsNameList(TotNumLists))
      ALLOCATE(EquipListsTypeList(TotNumLists))
      ALLOCATE(EquipListsIndexList(TotNumLists))

      !First load PlantEquipmentList info
      IF (PELists > 0) THEN
        CurrentModuleObject = 'PlantEquipmentList'
        DO Num = 1,PELists
          iIndex = Num
          CALL GetObjectItem(CurrentModuleObject,Num,cAlphaArgs,NumAlphas, &
                           rNumericArgs,NumNums,IOSTAT, &
                           NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                           AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
          EquipListsNameList(iIndex) = cAlphaArgs(1)
          EquipListsTypeList(iIndex) = LoopType_Plant
          EquipListsIndexList(iIndex) = Num
          MachineNum=2
          DO WHILE (MachineNum <= NumAlphas)
            firstblank=.false.
            IF (lAlphaFieldBlanks(MachineNum) .or. lAlphaFieldBlanks(MachineNum+1)) THEN
              IF (lAlphaFieldBlanks(MachineNum)) THEN
                CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'", invalid component specification.')
                CALL ShowContinueError(trim(cAlphaFieldNames(MachineNum))//' is blank.')
                firstblank=.true.
                ErrorsFound=.true.
              ENDIF
              IF (lAlphaFieldBlanks(MachineNum+1)) THEN
                IF (.not. firstblank) THEN
                  CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'", invalid component specification.')
                ENDIF
                CALL ShowContinueError(trim(cAlphaFieldNames(MachineNum+1))//' is blank.')
                ErrorsFound=.true.
              ENDIF
            ELSE
              CALL ValidateComponent(cAlphaArgs(MachineNum),cAlphaArgs(MachineNum+1),IsNotOK,TRIM(CurrentModuleObject))
              IF (IsNotOK) THEN
                CALL ShowContinueError(TRIM(CurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", Input Error.')
                ErrorsFound = .true.
              ENDIF
            ENDIF
            MachineNum=MachineNum+2
          ENDDO
        ENDDO
      ENDIF
      IF (CELists > 0) THEN
        CurrentModuleObject = 'CondenserEquipmentList'
        DO Num = 1,CELists
          iIndex = Num + PELists
          CALL GetObjectItem(CurrentModuleObject,Num,cAlphaArgs,NumAlphas, &
                           rNumericArgs,NumNums,IOSTAT, &
                           NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                           AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
          EquipListsNameList(iIndex) = cAlphaArgs(1)
          EquipListsTypeList(iIndex) = LoopType_Condenser
          EquipListsIndexList(iIndex) = Num
          MachineNum=2
          DO WHILE (MachineNum <= NumAlphas)
            firstblank=.false.
            IF (lAlphaFieldBlanks(MachineNum) .or. lAlphaFieldBlanks(MachineNum+1)) THEN
              IF (lAlphaFieldBlanks(MachineNum)) THEN
                CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'", invalid component specification.')
                CALL ShowContinueError(trim(cAlphaFieldNames(MachineNum))//' is blank.')
                firstblank=.true.
                ErrorsFound=.true.
              ENDIF
              IF (lAlphaFieldBlanks(MachineNum+1)) THEN
                IF (.not. firstblank) THEN
                  CALL ShowSevereError(trim(CurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'", invalid component specification.')
                ENDIF
                CALL ShowContinueError(trim(cAlphaFieldNames(MachineNum+1))//' is blank.')
                ErrorsFound=.true.
              ENDIF
            ELSE
              CALL ValidateComponent(cAlphaArgs(MachineNum),cAlphaArgs(MachineNum+1),IsNotOK,TRIM(CurrentModuleObject))
              IF (IsNotOK) THEN
                CALL ShowContinueError(TRIM(CurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", Input Error.')
                ErrorsFound = .true.
              ENDIF
            ENDIF
            MachineNum=MachineNum+2
          ENDDO
        ENDDO
      ENDIF
    ENDIF
    IF (ErrorsFound) THEN
      CALL ShowFatalError('LoadEquipList/GetEquipmentLists: Failed due to preceding errors.')
    ENDIF
    MyOneTimeFlag = .FALSE.
  ENDIF

  FoundIntendedList = .FALSE.
  ! find name in set of possible list
  DO Num = 1, TotNumLists
    IF(SameString(PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%name, EquipListsNameList(Num))) THEN
      FoundIntendedList = .TRUE.
      ! get object item for real this time
      SELECT CASE (EquipListsTypeList(num))
      CASE (LoopType_Plant)
        CurrentModuleObject = 'PlantEquipmentList'
      CASE (LoopType_Condenser)
        CurrentModuleObject = 'CondenserEquipmentList'
      END SELECT
      CALL GetObjectItem(CurrentModuleObject,EquipListsIndexList(Num),cAlphaArgs,NumAlphas, &
                           rNumericArgs,NumNums,IOSTAT, &
                           NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                           AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%NumComps      = (NumAlphas - 1)/2
      IF (PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%NumComps .GT. 0) THEN
        ALLOCATE (PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%Comp &
               (PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%NumComps))
        DO MachineNum = 1, PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%NumComps
          PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%Comp(MachineNum)%TypeOf = cAlphaArgs(MachineNum*2)
          PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%Comp(MachineNum)%Name = cAlphaArgs(MachineNum*2+1)
        END DO !MachineList
      ENDIF
    ENDIF
  ENDDO

  IF (.NOT. FoundIntendedList) THEN
    CALL ShowSevereError('LoadEquipList: Failed to find PlantEquipmentList or CondenserEquipmentList object named = ' &
                       //TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(ListNum)%name) )
    ErrorsFound = .true.

  ENDIF
RETURN
END SUBROUTINE LoadEquipList

SUBROUTINE FindCompSPInput(CurrentModuleObject,NumSchemes,LoopNum,SchemeNum,ErrorsFound)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   July 2010
          !       MODIFIED       B. Griffith, check setpoint nodes have setpoint managers on EMS on them.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Load component setpoint based input into PLANTLOOP data structure

          ! METHODOLOGY EMPLOYED:
          ! calls the Input Processor to retrieve data from input file.
          ! The format of the Energy+.idd (the EnergyPlus input data dictionary) for the
          ! following keywords is reflected exactly in this subroutine:
          !    PlantEquipmentOperation:ComponentSetPoint

          ! REFERENCES:
          ! na
          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetObjectItem, SameString
  USE DataLoopNode
  USE NodeInputManager,      ONLY: GetOnlySingleNode
  USE DataSizing
  USE DataIPShortCuts
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE DataGlobals,         ONLY: AnyEnergyManagementSystemInModel
  USE EMSManager ,         ONLY: CheckIfNodeSetpointManagedByEMS, iTemperatureSetpoint, &
                                 iTemperatureMinSetpoint, iTemperatureMaxSetpoint
  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT)    :: ErrorsFound   ! May be set here and passed on
  INTEGER, INTENT(IN)       :: LoopNum       ! May be set here and passed on
  INTEGER, INTENT(IN)       :: SchemeNum     ! May be set here and passed on
  INTEGER, INTENT(IN)       :: NumSchemes    ! May be set here and passed on
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming
  CHARACTER(len=MaxNameLength) :: EquipNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER   :: Plant     = 1     ! Used to identify whether the current loop is Plant
  INTEGER, PARAMETER   :: Condenser = 2     ! Used to identify whether the current loop is Condenser

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na
          ! DERIVED TYPE DEFINITIONS
          ! na
          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumAlphas
  INTEGER :: NumNums
  INTEGER :: Compnum
  INTEGER :: CompInNode
  INTEGER :: IOSTAT
  REAL(r64):: CompFlowRate
  INTEGER :: Num
  CHARACTER(len=MaxNameLength) :: LoopOpSchemeObj    ! Used to identify the object name for loop equipment operation scheme
  LOGICAL :: SchemeNameFound ! Set to FALSE if a match of OpScheme object and OpScheme name is not found
  LOGICAL :: NodeEMSSetpointMissing
  SchemeNameFound = .TRUE.

  IF (PlantLoop(LoopNum)%TypeofLoop == Plant) THEN
    LoopOpSchemeObj = 'PlantEquipmentOperationSchemes'
  ELSEIF (PlantLoop(LoopNum)%TypeofLoop == Condenser) THEN
    LoopOpSchemeObj = 'CondenserEquipmentOperationSchemes'
  ENDIF

  IF (NumSchemes .GT. 0) THEN
    DO Num = 1, NumSchemes
      CALL GetObjectItem(CurrentModuleObject,Num, &
                        cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT)
      IF(SameString(PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name,cAlphaArgs(1))) EXIT
      IF (Num == NumSchemes) THEN
        CALL ShowSevereError(TRIM(LoopOpSchemeObj)//' = "'//TRIM(PlantLoop(LoopNum)%OperationScheme)// &
                             '", could not find '// &
                             TRIM(CurrentModuleObject)//' = "'//TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name)//'".')
        ErrorsFound = .true.
        SchemeNameFound = .FALSE.
      ENDIF
    ENDDO
    IF (SchemeNameFound) THEN
     ! why only one equip list assumed here? because component setpoint managers have their own lists contained.
      PlantLoop(LoopNum)%OpScheme(SchemeNum)%NumEquipLists = 1
      ALLOCATE (PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1))
      PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%NumComps   = (NumAlphas - 1)/5
      IF (PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%NumComps .GT. 0) THEN
        ALLOCATE (PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp &
               (PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%NumComps))
        DO Compnum = 1, PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%NumComps
          PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%Typeof   = cAlphaArgs(Compnum*5-3)
          PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%Name     = cAlphaArgs(Compnum*5-2)
          PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%DemandNodeName     = cAlphaArgs(Compnum*5-1)
          PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%DemandNodeNum  = &
            GetOnlySingleNode(cAlphaArgs(Compnum*5-1),ErrorsFound, TRIM(CurrentModuleObject),cAlphaArgs(1), &
              NodeType_Water,NodeConnectionType_Sensor, 1, ObjectIsNotParent)
          PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%SetpointNodeName     = cAlphaArgs(Compnum*5)
          PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%SetpointNodeNum     = &
            GetOnlySingleNode(cAlphaArgs(Compnum*5),ErrorsFound,TRIM(CurrentModuleObject),cAlphaArgs(1), &
                NodeType_Water,NodeConnectionType_Sensor, 1, ObjectIsNotParent)
          PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%SetpointFlowRate     = rNumericArgs(Compnum)

          IF(rNumericArgs(Compnum) == AutoSize) THEN
            DO Num = 1, SaveNumPlantComps
              CompInNode = CompDesWaterFlow(Num)%SupNode
              CompFlowRate = CompDesWaterFlow(Num)%DesVolFlowRate
              IF(CompInNode == PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%DemandNodeNum) THEN
                PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%SetpointFlowRate = CompFlowRate
              ELSE
                !call error...Demand node must be component inlet node for autosizing
              END IF
            END DO
            Write(EquipNum,*) Num
            CALL ReportSizingOutput(TRIM(CurrentModuleObject), PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name, &
                                    'Design Water Flow Rate [m3/s] Equipment # '//Trim(AdjustL(EquipNum)),CompFlowRate)
          END IF

          SELECT CASE(cAlphaArgs(compnum*5 + 1))
          CASE ('COOLING')
           PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%CtrlTypeNum = CoolingOp
          CASE ('HEATING')
           PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%CtrlTypeNum = HeatingOp
          CASE ('DUAL')
           PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%CtrlTypeNum = DualOp
          END SELECT

          IF((TRIM(cAlphaArgs(5 + 1)) .NE. 'COOLING') .AND. (TRIM(cAlphaArgs(5 + 1)) .NE. 'HEATING')  &
                                                             .AND. (TRIM(cAlphaArgs(5 + 1)) .NE. 'DUAL')) THEN
            Call ShowSevereError('Equipment Operation Mode should be either HEATING or COOLING or DUAL mode, for '// &
                                 TRIM(CurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          END IF
          !check that setpoint node has valid setpoint managers or EMS
          SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)
          CASE (SingleSetpoint)
            IF (Node(PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%SetpointNodeNum)%TempSetPoint &
                            == SensedNodeFlagValue) THEN
              IF (.NOT. AnyEnergyManagementSystemInModel) THEN
                CALL ShowSevereError('Missing temperature setpoint for '//TRIM(CurrentModuleObject) &
                                     //' named '//TRIM(cAlphaArgs(1)) )
                CALL ShowContinueError('A temperature setpoint is needed at the node named '//  &
                                     TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%SetpointNodeName) )
                IF (PlantLoop(LoopNum)%TypeofLoop == Plant) THEN
                  CALL ShowContinueError('PlantLoop="'//trim(PlantLoop(LoopNum)%Name)//  &
                     '", Plant Loop Demand Calculation Scheme=SingleSetpoint')
                ELSEIF (PlantLoop(LoopNum)%TypeofLoop == Condenser) THEN  ! not applicable to Condenser loops
                ENDIF
                CALL ShowContinueError(' Use a setpoint manager to place a single temperature setpoint on the node')
                ErrorsFound=.true.
              ELSE
                ! need call to EMS to check node
                NodeEMSSetpointMissing = .FALSE.
                CALL CheckIfNodeSetpointManagedByEMS( &
                        PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%SetpointNodeNum, &
                        iTemperatureSetpoint, NodeEMSSetpointMissing)
                IF (NodeEMSSetpointMissing) THEN
                  CALL ShowSevereError('Missing temperature setpoint for '//TRIM(CurrentModuleObject) &
                                       //' named '//TRIM(cAlphaArgs(1)) )
                  CALL ShowContinueError('A temperature setpoint is needed at the node named '//  &
                                        TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%SetpointNodeName))
                  IF (PlantLoop(LoopNum)%TypeofLoop == Plant) THEN
                    CALL ShowContinueError('PlantLoop="'//trim(PlantLoop(LoopNum)%Name)//  &
                       '", Plant Loop Demand Calculation Scheme=SingleSetpoint')
                  ELSEIF (PlantLoop(LoopNum)%TypeofLoop == Condenser) THEN  ! not applicable to Condenser loops
                  ENDIF
                  CALL ShowContinueError(' Use a setpoint manager or EMS actuator to place a single temperature setpoint on node')
                  ErrorsFound=.true.
                ENDIF
              ENDIF
            ENDIF
          CASE (DualSetpointDeadband)
            IF (PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%CtrlTypeNum == CoolingOp) THEN
              IF (Node(PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%SetpointNodeNum)%TempSetPointHI &
                            == SensedNodeFlagValue) THEN
                IF (.NOT. AnyEnergyManagementSystemInModel) THEN
                  CALL ShowSevereError('Missing temperature high setpoint for '//TRIM(CurrentModuleObject) &
                                       //' named '//TRIM(cAlphaArgs(1)) )
                  CALL ShowContinueError('A temperature high setpoint is needed at the node named '//  &
                                       TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%SetpointNodeName) )
                  IF (PlantLoop(LoopNum)%TypeofLoop == Plant) THEN
                    CALL ShowContinueError('PlantLoop="'//trim(PlantLoop(LoopNum)%Name)//  &
                       '", Plant Loop Demand Calculation Scheme=DualSetpointDeadband')
                  ELSEIF (PlantLoop(LoopNum)%TypeofLoop == Condenser) THEN  ! not applicable to Condenser loops
                  ENDIF
                  CALL ShowContinueError(' Use a setpoint manager to place a dual temperature setpoint on the node')
                  ErrorsFound=.true.
                ELSE
                  ! need call to EMS to check node
                  NodeEMSSetpointMissing = .FALSE.
                  CALL CheckIfNodeSetpointManagedByEMS( &
                          PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%SetpointNodeNum,&
                          iTemperatureMaxSetpoint, NodeEMSSetpointMissing)
                  IF (NodeEMSSetpointMissing) THEN
                    CALL ShowSevereError('Missing high temperature setpoint for '//TRIM(CurrentModuleObject) &
                                         //' named '//TRIM(cAlphaArgs(1)) )
                    CALL ShowContinueError('A high temperature setpoint is needed at the node named '//  &
                                       TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%SetpointNodeName))
                    IF (PlantLoop(LoopNum)%TypeofLoop == Plant) THEN
                      CALL ShowContinueError('PlantLoop="'//trim(PlantLoop(LoopNum)%Name)//  &
                         '", Plant Loop Demand Calculation Scheme=DualSetpointDeadband')
                    ELSEIF (PlantLoop(LoopNum)%TypeofLoop == Condenser) THEN  ! not applicable to Condenser loops
                    ENDIF
                    CALL ShowContinueError(' Use a setpoint manager or EMS actuator to place a dual or high temperature' &
                                          //' setpoint on node')
                    ErrorsFound=.true.
                  ENDIF
                ENDIF
              ENDIF
            ELSEIF (PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%CtrlTypeNum == HeatingOp) THEN
              IF (Node(PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%SetpointNodeNum)%TempSetPointLo &
                            == SensedNodeFlagValue) THEN
                IF (.NOT. AnyEnergyManagementSystemInModel) THEN
                  CALL ShowSevereError('Missing temperature low setpoint for '//TRIM(CurrentModuleObject) &
                                       //' named '//TRIM(cAlphaArgs(1)) )
                  CALL ShowContinueError('A temperature low setpoint is needed at the node named '//  &
                                       TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%SetpointNodeName) )
                  IF (PlantLoop(LoopNum)%TypeofLoop == Plant) THEN
                    CALL ShowContinueError('PlantLoop="'//trim(PlantLoop(LoopNum)%Name)//  &
                       '", Plant Loop Demand Calculation Scheme=DualSetpointDeadband')
                  ELSEIF (PlantLoop(LoopNum)%TypeofLoop == Condenser) THEN  ! not applicable to Condenser loops
                  ENDIF
                  CALL ShowContinueError(' Use a setpoint manager to place a dual temperature setpoint on the node')
                  ErrorsFound=.true.
                ELSE
                  ! need call to EMS to check node
                  NodeEMSSetpointMissing = .FALSE.
                  CALL CheckIfNodeSetpointManagedByEMS( &
                          PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%SetpointNodeNum, &
                          iTemperatureMinSetpoint, NodeEMSSetpointMissing)
                  CALL CheckIfNodeSetpointManagedByEMS( &
                          PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%SetpointNodeNum, &
                          iTemperatureMaxSetpoint, NodeEMSSetpointMissing)
                  IF (NodeEMSSetpointMissing) THEN
                    CALL ShowSevereError('Missing low temperature setpoint for '//TRIM(CurrentModuleObject) &
                                         //' named '//TRIM(cAlphaArgs(1)) )
                    CALL ShowContinueError('A low temperature setpoint is needed at the node named '//  &
                                       TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%SetpointNodeName))
                    IF (PlantLoop(LoopNum)%TypeofLoop == Plant) THEN
                      CALL ShowContinueError('PlantLoop="'//trim(PlantLoop(LoopNum)%Name)//  &
                         '", Plant Loop Demand Calculation Scheme=DualSetpointDeadband')
                    ELSEIF (PlantLoop(LoopNum)%TypeofLoop == Condenser) THEN  ! not applicable to Condenser loops
                    ENDIF
                    CALL ShowContinueError(' Use a setpoint manager or EMS actuator to place a dual or low temperature' &
                                          //' setpoint on node')
                    ErrorsFound=.true.
                  ENDIF
                ENDIF
          END IF
            ELSEIF (PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%CtrlTypeNum == DualOP) THEN
              IF ((Node(PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%SetpointNodeNum)%TempSetPointHI &
                            == SensedNodeFlagValue)  .OR. &
                  (Node(PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%SetpointNodeNum)%TempSetPointLo &
                            == SensedNodeFlagValue) ) THEN
                IF (.NOT. AnyEnergyManagementSystemInModel) THEN
                  CALL ShowSevereError('Missing temperature dual setpoints for '//TRIM(CurrentModuleObject) &
                                       //' named '//TRIM(cAlphaArgs(1)) )
                  CALL ShowContinueError('A dual temperaturesetpoint is needed at the node named '//  &
                                       TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%SetpointNodeName) )
                  IF (PlantLoop(LoopNum)%TypeofLoop == Plant) THEN
                    CALL ShowContinueError('PlantLoop="'//trim(PlantLoop(LoopNum)%Name)//  &
                       '", Plant Loop Demand Calculation Scheme=DualSetpointDeadband')
                  ELSEIF (PlantLoop(LoopNum)%TypeofLoop == Condenser) THEN  ! not applicable to Condenser loops
                  ENDIF
                  CALL ShowContinueError(' Use a setpoint manager to place a dual temperature setpoint on the node')
                  ErrorsFound=.true.
                ELSE
                  ! need call to EMS to check node
                  NodeEMSSetpointMissing = .FALSE.
                  CALL CheckIfNodeSetpointManagedByEMS( &
                          PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%SetpointNodeNum, &
                          iTemperatureMinSetpoint, NodeEMSSetpointMissing)
                  IF (NodeEMSSetpointMissing) THEN
                    CALL ShowSevereError('Missing dual temperature setpoint for '//TRIM(CurrentModuleObject) &
                                         //' named '//TRIM(cAlphaArgs(1)) )
                    CALL ShowContinueError('A dual temperature setpoint is needed at the node named '//  &
                                       TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%SetpointNodeName))
                    IF (PlantLoop(LoopNum)%TypeofLoop == Plant) THEN
                      CALL ShowContinueError('PlantLoop="'//trim(PlantLoop(LoopNum)%Name)//  &
                         '", Plant Loop Demand Calculation Scheme=DualSetpointDeadband')
                    ELSEIF (PlantLoop(LoopNum)%TypeofLoop == Condenser) THEN  ! not applicable to Condenser loops
                    ENDIF
                    CALL ShowContinueError(' Use a setpoint manager or EMS actuator to place a dual temperature' &
                                          //' setpoint on node')
                    ErrorsFound=.true.
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          END SELECT
        END DO
      ELSE
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(cAlphaArgs(1))//&
                            '", specified without any machines.')
        ErrorsFound=.true.
      ENDIF
    ENDIF
  ELSE
    CALL ShowSevereError(TRIM(LoopOpSchemeObj)//' = "'//TRIM(PlantLoop(LoopNum)%OperationScheme)// &
                         '", could not find '// &
                         TRIM(CurrentModuleObject)//' = "'//TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name)//'".')
    ErrorsFound=.true.
  ENDIF
  RETURN
END SUBROUTINE FindCompSPInput

SUBROUTINE GetUserDefinedOpSchemeInput(CurrentModuleObject,NumSchemes,LoopNum,SchemeNum,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         <author>
          !       DATE WRITTEN   <date_written>
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE DataPlant
  USE InputProcessor, ONLY: GetObjectItem, SameString, FindItemInList

  USE DataRuntimeLanguage, ONLY: EMSProgramCallManager, NumProgramCallManagers

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in renaming
  INTEGER, INTENT(IN)       :: NumSchemes    ! May be set here and passed on
  INTEGER, INTENT(IN)       :: LoopNum       ! May be set here and passed on
  INTEGER, INTENT(IN)       :: SchemeNum     ! May be set here and passed on
  LOGICAL, INTENT(INOUT)    :: ErrorsFound   ! May be set here and passed on

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER   :: Plant     = 1     ! Used to identify whether the current loop is Plant
  INTEGER, PARAMETER   :: Condenser = 2     ! Used to identify whether the current loop is Condenser

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumAlphas
  INTEGER :: NumNums
  INTEGER :: Num
  INTEGER :: Compnum
  INTEGER :: IOSTAT
  LOGICAL :: SchemeNameFound ! Set to FALSE if a match of OpScheme object and OpScheme name is not found
  CHARACTER(len=MaxNameLength) :: LoopOpSchemeObj    ! Used to identify the object name for loop equipment operation scheme
  INTEGER :: StackMngrNum ! local temporary for Erl program calling manager index
  LOGICAL  :: lDummy

  SchemeNameFound = .TRUE.

  IF (PlantLoop(LoopNum)%TypeofLoop == Plant) THEN
    LoopOpSchemeObj = 'PlantEquipmentOperationSchemes'
  ELSEIF (PlantLoop(LoopNum)%TypeofLoop == Condenser) THEN
    LoopOpSchemeObj = 'CondenserEquipmentOperationSchemes'
  ENDIF

  IF (NumSchemes .GT. 0) THEN

    DO Num = 1, NumSchemes
      CALL GetObjectItem(CurrentModuleObject,Num, &
                        cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, &
                        NumBlank=lNumericFieldBlanks,AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IF(SameString(PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name,cAlphaArgs(1))) EXIT !found the correct one
      IF (Num == NumSchemes) THEN ! did not find it
        CALL ShowSevereError(TRIM(LoopOpSchemeObj)//' = "'//TRIM(PlantLoop(LoopNum)%OperationScheme)// &
                             '", could not find '// &
                             TRIM(CurrentModuleObject)//' = "'//TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name)//'".')
        ErrorsFound = .true.
        SchemeNameFound = .FALSE.
      ENDIF
    ENDDO
    IF (SchemeNameFound) THEN
      PlantLoop(LoopNum)%OpScheme(SchemeNum)%NumEquipLists = 1
      ALLOCATE (PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1))

      PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%NumComps   = (NumAlphas - 3)/2
      IF (PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%NumComps .GT. 0) THEN
        ALLOCATE (PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp &
                  (PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%NumComps))
        DO Compnum = 1, PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%NumComps
          PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%Typeof   = cAlphaArgs(Compnum*2+2)
          PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%Name     = cAlphaArgs(Compnum*2+3)

          !Setup EMS actuators for machines' MyLoad.
          CALL SetupEMSActuator('Plant Equipment Operation', &
                                          TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name)//':'// &
                                          TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%Name), &
                                'Distributed Load Rate', '[W]', lDummy, &
                                PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%EMSActuatorDispatchedLoadValue)
          CALL SetupEMSInternalVariable('Component Remaining Current Demand Rate', &
                                          TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name)//':'// &
                                          TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%Name), '[W]', &
                                  PlantLoop(LoopNum)%OpScheme(SchemeNum)%EquipList(1)%Comp(compnum)%EMSIntVarRemainingLoadValue)
        ENDDO
      ENDIF
      StackMngrNum = FindItemInList(cAlphaArgs(2), EMSProgramCallManager%Name, NumProgramCallManagers)
      IF (StackMngrNum > 0) THEN ! found it
        PlantLoop(LoopNum)%OpScheme(SchemeNum)%ErlSimProgramMngr = StackMngrNum
      ELSE
        CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
        CALL ShowContinueError('Entered in '//TRIM(CurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
        CALL ShowContinueError('EMS Program Manager Name not found.')
        ErrorsFound = .TRUE.
      ENDIF
      IF (.NOT. lAlphaFieldBlanks(3)) THEN
        StackMngrNum = FindItemInList(cAlphaArgs(3), EMSProgramCallManager%Name, NumProgramCallManagers)
        IF (StackMngrNum > 0) THEN ! found it
          PlantLoop(LoopNum)%OpScheme(SchemeNum)%ErlInitProgramMngr = StackMngrNum
        ELSE
          CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(3))//'='//TRIM(cAlphaArgs(3)))
          CALL ShowContinueError('Entered in '//TRIM(CurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
          CALL ShowContinueError('EMS Program Manager Name not found.')
          ErrorsFound = .TRUE.
        ENDIF
      ENDIF

      ! setup internal variable for Supply Side Current Demand Rate [W]
      CALL SetupEMSInternalVariable('Supply Side Current Demand Rate',PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name, '[W]', &
                                        PlantLoop(LoopNum)%OpScheme(SchemeNum)%EMSIntVarLoopDemandRate )
    ENDIF

  ELSE
    CALL ShowSevereError(TRIM(LoopOpSchemeObj)//' = "'//TRIM(PlantLoop(LoopNum)%OperationScheme)// &
                         '", could not find '// &
                         TRIM(CurrentModuleObject)//' = "'//TRIM(PlantLoop(LoopNum)%OpScheme(SchemeNum)%Name)//'".')
    ErrorsFound=.true.
  ENDIF
  RETURN

END SUBROUTINE GetUserDefinedOpSchemeInput

! End of GetInput subroutines for the Module
!******************************************************************************


! Beginning Initialization Section of the Plant Loop Module
!******************************************************************************
SUBROUTINE InitLoadDistribution(FirstHVACIteration)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Dan Fisher
          !       DATE WRITTEN:    July 2010
          !       REVISED:
          !

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine scans equipment lists and matches a particular
          ! plant component with a component on the list.  Pointers to the
          ! operation scheme and equipment list are saved on the plant data
          ! structure to facilitate a new load management routine that calls
          ! ManageLoadDistribution for every component.

          ! METHODOLOGY EMPLOYED:
          ! na
          ! REFERENCES:
          ! na
          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetScheduleIndex, GetCurrentScheduleValue
  USE InputProcessor, ONLY: SameString, FindItem
  USE DataGlobals,    ONLY: BeginEnvrnFlag, emsCallFromUserDefinedComponentModel
  USE EMSManager,            ONLY: ManageEMS

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN)                    :: FirstHVACIteration
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na
          ! INTERFACE BLOCK SPECIFICATIONS
          ! na
          ! DERIVED TYPE DEFINITIONS

TYPE (OpSchemePtrData),          ALLOCATABLE, DIMENSION(:) :: TempCompOpscheme

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                           :: LoopPtr
  INTEGER                           :: LoopSidePtr
  INTEGER                           :: BranchPtr
  INTEGER                           :: CompPtr
  INTEGER                           :: LoopNum
  INTEGER                           :: DummyLoopNum
  INTEGER                           :: LoopSideNum
  INTEGER                           :: BranchNum
  INTEGER                           :: CompNum
  INTEGER                           :: Index
  INTEGER                           :: EquipNum
  INTEGER                           :: OpNum
  INTEGER                           :: OpSchemePtr
  INTEGER                           :: SchemeNum
  INTEGER                           :: thisSchemeNum
  INTEGER                           :: SchemeType
  INTEGER                           :: ListNum
  LOGICAL, SAVE                     :: MyOneTimeFlag = .true.
  LOGICAL                           :: FoundScheme
  LOGICAL                           :: FoundSchemeMatch
!  LOGICAL, SAVE                     :: FirstHVACInitsDone = .FALSE.
!  LOGICAL, SAVE                     :: MyEnvrnFlag = .TRUE.
  INTEGER                           :: ThisTypeOfNum
  INTEGER                           :: CompOpNum
  INTEGER                           :: OldNumOpSchemes
  INTEGER                           :: OldNumEquipLists
  INTEGER                           :: NewNumEquipLists
  INTEGER                           :: NewNumOpSchemes
  INTEGER                           :: NumSearchResults
  LOGICAL                           :: GetInputOK       !successful Get Input
  LOGICAL,SAVE                      :: GetPlantOpInput  =.TRUE.      !successful Get Input
  LOGICAL                           :: errFlag1
  LOGICAL                           :: errFlag2
  REAL(r64)                         :: HighestRange

  errFlag2 = .FALSE.
        !Get Input
  IF (GetPlantOpInput)THEN
    CALL GetPlantOperationInput(GetInputOK)
    IF(GetInputOK)THEN
      CALL GetOperationSchemeInput
      GetPlantOpInput = .FALSE.
    ELSE
      RETURN
    ENDIF
  END IF

    !ONE TIME INITS
IF (MyOneTimeFlag) THEN
    !Set up 'component' to 'op scheme' pointers in Plant data structure
    !We're looking for matches between a component on a PlantLoop()%Opscheme()%List()
    !and the same component in the PlantLoop()%LoopSide()%Branch()%Comp() data structure

  ! first loop over main operation scheme data and finish filling out indexes to plant topology for the components in the lists
  DO LoopNum = 1, TotNumLoops
    DO OpNum =1, PlantLoop(LoopNum)%NumOpschemes
      DO ListNum = 1,PlantLoop(LoopNum)%Opscheme(OpNum)%NumEquipLists
        DO EquipNum = 1,PlantLoop(LoopNum)%Opscheme(OpNum)%EquipList(ListNum)%NumComps

          ThisTypeOfNum = FindItem(TRIM(PlantLoop(LoopNum)%OpScheme(OpNum)%EquipList(ListNum)%Comp(EquipNum)%TypeOf), &
                                   SimPlantEquipTypes,      &
                                   NumSimPlantEquipTypes)
          errFlag1 = .FALSE.
          CALL ScanPlantLoopsForObject(TRIM(PlantLoop(LoopNum)%OpScheme(OpNum)%EquipList(ListNum)%Comp(EquipNum)%Name), &
                                       ThisTypeOfNum,             &
                                       DummyLoopNum,              &
                                       LoopSideNum,               &
                                       BranchNum,                 &
                                       CompNum,                   &
                                       CountMatchPlantLoops = NumSearchResults, &
                                       SingleLoopSearch = LoopNum, &
                                       errflag = errFlag1)

          IF ( errFlag1 ) THEN
            CALL ShowSevereError('InitLoadDistribution: Equipment specified for operation scheme not found on correct loop')
            CALL ShowContinueError('Operation Scheme name = '//TRIM(PlantLoop(LoopNum)%Opscheme(OpNum)%Name) )
            CALL ShowContinueError('Loop name = '//TRIM(PlantLoop(LoopNum)%Name) )
            CALL ShowContinueError('Component name = ' &
                                   //TRIM(PlantLoop(LoopNum)%OpScheme(OpNum)%EquipList(ListNum)%Comp(EquipNum)%Name) )
            CALL ShowFatalError('InitLoadDistribution: Simulation terminated because of error in operation scheme.')

          END IF

          PlantLoop(LoopNum)%OpScheme(OpNum)%EquipList(ListNum)%Comp(EquipNum)%LoopNumPtr      = DummyLoopNum
          PlantLoop(LoopNum)%OpScheme(OpNum)%EquipList(ListNum)%Comp(EquipNum)%LoopSideNumPtr  = LoopSideNum
          PlantLoop(LoopNum)%OpScheme(OpNum)%EquipList(ListNum)%Comp(EquipNum)%BranchNumPtr    = BranchNum
          PlantLoop(LoopNum)%OpScheme(OpNum)%EquipList(ListNum)%Comp(EquipNum)%CompNumPtr      = CompNum

          IF(ValidLoopEquipTypes(ThisTypeOfNum)==LoopType_Plant .AND. PlantLoop(LoopNum)%TypeOfLoop == LoopType_Condenser)THEN
            CALL ShowSevereError('InitLoadDistribution: CondenserLoop="'//trim(PlantLoop(LoopNum)%Name)//  &
               '", Operation Scheme="'//  &
              trim(PlantLoop(LoopNum)%OperationScheme)//'",')
            CALL ShowContinueError('Scheme type='//trim(PlantLoop(LoopNum)%OpScheme(OpNum)%TypeOf)//', Name="'//  &
              trim(PlantLoop(LoopNum)%OpScheme(OpNum)%Name)//'" includes equipment that is not valid on a Condenser Loop')
            CALL ShowContinueError('Component '//trim(ccSimPlantEquipTypes(ThisTypeOfNum))//  &
                                ' not allowed as supply equipment on this type of loop.')
            CALL ShowContinueError('Component name = ' &
                                     //TRIM(PlantLoop(LoopNum)%OpScheme(OpNum)%EquipList(ListNum)%Comp(EquipNum)%Name) )

            errFlag2 = .TRUE.
          ENDIF
          IF(ValidLoopEquipTypes(ThisTypeOfNum)==LoopType_Condenser .AND. PlantLoop(LoopNum)%TypeOfLoop == LoopType_Plant)THEN
            CALL ShowSevereError('InitLoadDistribution: PlantLoop="'//trim(PlantLoop(LoopNum)%Name)//'", Operation Scheme="'//  &
              trim(PlantLoop(LoopNum)%OperationScheme)//'",')
            CALL ShowContinueError('Scheme type='//trim(PlantLoop(LoopNum)%OpScheme(OpNum)%TypeOf)//', Name="'//  &
              trim(PlantLoop(LoopNum)%OpScheme(OpNum)%Name)//'" includes equipment that is not valid on a Plant Loop')
            CALL ShowContinueError('Component '//trim(ccSimPlantEquipTypes(ThisTypeOfNum))// &
                                ' not allowed as supply equipment on this type of loop.')
            CALL ShowContinueError('Component name = ' &
                                     //TRIM(PlantLoop(LoopNum)%OpScheme(OpNum)%EquipList(ListNum)%Comp(EquipNum)%Name) )
            errFlag2 = .TRUE.
          ENDIF

        ENDDO  !Equipment on List
      ENDDO  !List
    ENDDO  !operation scheme
  ENDDO  !loop

  !second loop, fill op schemes info at each component.
  DO LoopNum = 1, TotNumLoops
    DO OpNum =1, PlantLoop(LoopNum)%NumOpschemes
      DO ListNum = 1,PlantLoop(LoopNum)%Opscheme(OpNum)%NumEquipLists
        DO EquipNum = 1,PlantLoop(LoopNum)%Opscheme(OpNum)%EquipList(ListNum)%NumComps
          ! dereference indices (stored in previous loop)
          DummyLoopNum = PlantLoop(LoopNum)%OpScheme(OpNum)%EquipList(ListNum)%Comp(EquipNum)%LoopNumPtr
          LoopSideNum  = PlantLoop(LoopNum)%OpScheme(OpNum)%EquipList(ListNum)%Comp(EquipNum)%LoopSideNumPtr
          BranchNum    = PlantLoop(LoopNum)%OpScheme(OpNum)%EquipList(ListNum)%Comp(EquipNum)%BranchNumPtr
          CompNum      = PlantLoop(LoopNum)%OpScheme(OpNum)%EquipList(ListNum)%Comp(EquipNum)%CompNumPtr

          IF (PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NumOpSchemes == 0) THEN
           ! first op scheme for this component, allocate OpScheme and its EquipList to size 1
            ALLOCATE(PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%OpScheme(1))
            ALLOCATE(PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%OpScheme(1)%EquipList(1))
            PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NumOpSchemes = 1
            PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%OpScheme(1)%NumEquipLists = 1
            ! store pointers
            PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%OpScheme(1)%OpSchemePtr = OpNum
            PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%OpScheme(1)%EquipList(1)%ListPtr =   &
               ListNum
            PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%OpScheme(1)%EquipList(1)%CompPtr =   &
               EquipNum

          ELSEIF (PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NumOpSchemes > 0) THEN
             ! already an op scheme
            OldNumOpSchemes = PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NumOpSchemes
            ! create and store complete copy of old opScheme structure
            ALLOCATE(TempCompOpscheme(OldNumOpSchemes))
            DO thisSchemeNum = 1,OldNumOpSchemes
              OldNumEquipLists =   &
                PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%OpScheme(thisSchemeNum)%NumEquipLists
              ALLOCATE(TempCompOpscheme(thisSchemeNum)%EquipList(OldNumEquipLists))
            ENDDO
            TempCompOpscheme = PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%OpScheme

              !could be new list on existing scheme or new scheme with new list.  Check and see
            FoundSchemeMatch = .FALSE.
            DO thisSchemeNum = 1,OldNumOpSchemes
                  !compare the opscheme index, 'opnum', in the PlantLoop()%OpScheme()data structure
                  !with the opschemeptr in the PlantLoop()%LoopSide()%Branch()%Comp() data structure.
              IF(OpNum /=    &
                 PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%  &
                    OpScheme(thisSchemeNum)%OpSchemePtr)  &
                    CYCLE
              FoundSchemeMatch = .TRUE.
              EXIT
            ENDDO
            IF(FoundSchemeMatch)THEN
              !op scheme already exists, but need to add a list to the existing opscheme
              NewNumEquipLists = PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%  &
                 OpScheme(thisSchemeNum)%NumEquipLists + 1

              DEALLOCATE(PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%  &
                 OpScheme(thisSchemeNum)%EquipList)
              ALLOCATE(PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%  &
                 OpScheme(thisSchemeNum)%EquipList(NewNumEquipLists))
              PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%  &
                 OpScheme(thisSchemeNum)%EquipList(1:NewNumEquipLists-1) &
                   = TempCompOpscheme(thisSchemeNum)%EquipList  !structure array assignment
              PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%  &
                 OpScheme(thisSchemeNum)%NumEquipLists = NewNumEquipLists
              PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%  &
                 OpScheme(thisSchemeNum)%EquipList(NewNumEquipLists)%ListPtr &
                   = ListNum
              PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%  &
                 OpScheme(thisSchemeNum)%EquipList(NewNumEquipLists)%CompPtr &
                   = EquipNum

            ELSE !(.NOT.FoundSchemeMatch)THEN
              ! add new op scheme and a new list
              NewNumOpSchemes = OldNumOpSchemes + 1
              DEALLOCATE(PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%OpScheme)
              ALLOCATE(PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%OpScheme(NewNumOpSchemes))
              DO SchemeNum = 1,OldNumOpSchemes
                NewNumEquipLists = TempCompOpscheme(SchemeNum)%NumEquipLists
                ALLOCATE(PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%  &
                   OpScheme(SchemeNum)%EquipList(NewNumEquipLists))
              ENDDO
              PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%OpScheme(1:OldNumOpSchemes) =   &
                 TempCompOpscheme ! structure array assignment

              ALLOCATE(PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%  &
                 OpScheme(NewNumOpSchemes)%EquipList(1))
              PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NumOpSchemes = NewNumOpSchemes
              PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%  &
                 OpScheme(NewNumOpSchemes)%NumEquipLists = 1
              PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%  &
                 OpScheme(NewNumOpSchemes)%OpSchemePtr = OpNum
              PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%  &
                 OpScheme(NewNumOpSchemes)%EquipList(1)%ListPtr = ListNum
              PlantLoop(DummyLoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%  &
                 OpScheme(NewNumOpSchemes)%EquipList(1)%CompPtr = EquipNum

            ENDIF

            IF (ALLOCATED(TempCompOpscheme)) DEALLOCATE(TempCompOpscheme)

          ENDIF

        ENDDO  !Equipment on List
      ENDDO  !List
    ENDDO  !operation scheme
  ENDDO  !loop

        !check the pointers to see if a single component is attached to more than one type of control scheme
  DO LoopNum = 1, TotNumLoops
    DO LoopSideNum = DemandSide,SupplySide
      DO BranchNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalBranches
        DO CompNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%TotalComponents
          IF(ALLOCATED(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%OpScheme))THEN
            DO index = 1,PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NumOpSchemes
              OpSchemePtr = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%OpScheme(index)%OpschemePtr
              IF (OpSchemePtr ==0) THEN
                CALL ShowSevereError('InitLoadDistribution: no operation scheme index found for component on PlantLoop='&
                                         //TRIM(PlantLoop(LoopNum)%Name) )
                CALL ShowContinueError('Component name = ' &
                                    //TRIM(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Name) )
                errFlag2 = .TRUE.
              ENDIF
              IF(index ==1)THEN
                SchemeType = PlantLoop(LoopNum)%Opscheme(OpSchemePtr)%OpSchemeType
              ELSE
                IF(SchemeType /= PlantLoop(LoopNum)%Opscheme(OpSchemePtr)%OpSchemeType)THEN
                  !CALL FATAL ERROR 'component may not be specified on two types of operation schemes
                  !DSU?  BG do not understand.  Cannot different op schemes be in effect at different times?
                  !  I thought this would be allowed??
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO
    ENDDO
  ENDDO

  ! fill out information on which equipment list is the "last" meaning it has the highest upper limit for load range
  DO LoopNum = 1, TotNumLoops
    DO OpNum =1, PlantLoop(LoopNum)%NumOpschemes
      ! skip non-load based op schemes
      IF ((PlantLoop(LoopNum)%Opscheme(OpNum)%OpSchemeType  /= HeatingRBOpSchemeType) .AND. &
          (PlantLoop(LoopNum)%Opscheme(OpNum)%OpSchemeType  /= CoolingRBOpSchemeType) )  CYCLE
      HighestRange = 0.d0
      DO ListNum = 1,PlantLoop(LoopNum)%Opscheme(OpNum)%NumEquipLists
        HighestRange = MAX(HighestRange, PlantLoop(LoopNum)%OpScheme(OpNum)%EquipList(ListNum)%RangeUpperLimit)
      ENDDO  !List
      DO ListNum = 1,PlantLoop(LoopNum)%Opscheme(OpNum)%NumEquipLists
        IF (HighestRange == PlantLoop(LoopNum)%OpScheme(OpNum)%EquipList(ListNum)%RangeUpperLimit) THEN
          PlantLoop(LoopNum)%OpScheme(OpNum)%EquipListNumForLastStage = ListNum
        ENDIF
      ENDDO
    ENDDO  !operation scheme
  ENDDO  !loop



   MyOneTimeFlag = .FALSE.
ENDIF


If (AnyEMSPlantOpSchemesInModel) THEN
  ! Execute any Initialization EMS program calling managers for User-Defined operation.
  DO LoopNum = 1, TotNumLoops
    DO OpNum =1, PlantLoop(LoopNum)%NumOpschemes
      IF (PlantLoop(LoopNum)%Opscheme(OpNum)%OpSchemeType == EMSOpSchemeType) THEN
        IF (BeginEnvrnFlag .and. PlantLoop(LoopNum)%Opscheme(OpNum)%MyEnvrnFlag) THEN
          IF (PlantLoop(LoopNum)%OpScheme(OpNum)%ErlInitProgramMngr > 0) THEN
            CALL ManageEMS(emsCallFromUserDefinedComponentModel, &
                         ProgramManagerToRun = PlantLoop(LoopNum)%OpScheme(OpNum)%ErlInitProgramMngr)
          ENDIF
          PlantLoop(LoopNum)%Opscheme(OpNum)%MyEnvrnFlag = .FALSE.
        ENDIF
        IF (.NOT. BeginEnvrnFlag) PlantLoop(LoopNum)%Opscheme(OpNum)%MyEnvrnFlag = .TRUE.
      ENDIF
    ENDDO  !operation scheme
  ENDDO  !loop
ENDIF

    !FIRST HVAC INITS
IF (FirstHVACIteration )THEN
  DO LoopNum = 1, TotNumLoops
    DO LoopSideNum = DemandSide,SupplySide
      DO BranchNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalBranches
        DO CompNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%TotalComponents
            ! initalize components 'ON-AVAILABLE-NO LOAD-NO EMS CTRL'
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%ON = .TRUE.
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Available = .TRUE.
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad =0.d0
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%EMSLoadOverrideOn = .FALSE.
            !  Zero out the old curopschemeptr so that we don't get 'carry-over' when we update schedules
          IF(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType.NE.DemandOpSchemeType .and. &
             PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType.NE.PumpOpSchemeType .and. &
             PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType.NE.WSEconOpSchemeType .and. &
             PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType.NE.NoControlOpSchemeType)THEN
            PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurOpSchemeType = NoControlOpSchemeType
          END IF
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%CurCompLevelOpNum = 0
        ENDDO
      ENDDO
    ENDDO
  ENDDO
        !Update the opscheme schedules
  DO LoopNum = 1, TotNumLoops
    foundscheme = .false.
    DO OpNum = 1, PlantLoop(LoopNum)%NumOpSchemes
      IF(GetCurrentScheduleValue(PlantLoop(LoopNum)%OpScheme(OpNum)%SchedPtr) > 0.0d0) THEN
        PlantLoop(LoopNum)%OpScheme(OpNum)%Available = .TRUE.
        foundscheme = .true.
        DO ListNum = 1, PlantLoop(LoopNum)%OpScheme(OpNum)%NumEquipLists
            !The component loop loads the pointers from the Opscheme data structure
            !If the component happens to be active in more than schedule, the *LAST*
            !schedule found will be activated
          DO CompNum = 1, PlantLoop(LoopNum)%OpScheme(OpNum)%EquipList(ListNum)%NumComps
            LoopPtr     = PlantLoop(LoopNum)%Opscheme(OpNum)%EquipList(ListNum)%Comp(CompNum)%LoopNumPtr
            LoopSidePtr = PlantLoop(LoopNum)%Opscheme(OpNum)%EquipList(ListNum)%Comp(CompNum)%LoopSideNumPtr
            BranchPtr   = PlantLoop(LoopNum)%Opscheme(OpNum)%EquipList(ListNum)%Comp(CompNum)%BranchNumPtr
            CompPtr     = PlantLoop(LoopNum)%Opscheme(OpNum)%EquipList(ListNum)%Comp(CompNum)%CompNumPtr

            IF (PlantLoop(LoopPtr)%LoopSide(LoopSidePtr)%Branch(BranchPtr)%Comp(CompPtr)%CurOpSchemeType &
                                     .NE. PumpOpSchemeType ) THEN
              PlantLoop(LoopPtr)%LoopSide(LoopSidePtr)%Branch(BranchPtr)%Comp(CompPtr)%CurOpSchemeType = &
                 PlantLoop(LoopNum)%OpScheme(OpNum)%OpSchemeType
            ELSE
              CALL ShowSevereError('Invalid [pump] component found on equipment list.  Pumps are not allowed on equipment lists.')
              CALL ShowContinueError('Problem component name = ' &
                                  //TRIM(PlantLoop(LoopNum)%Opscheme(OpNum)%EquipList(ListNum)%Comp(CompNum)%Name) )
              CALL ShowContinueError('Remove pump component and place other plant equipment on the list to correct.')
              errFlag2 = .TRUE.
            END IF

             DO CompOpNum = 1, PlantLoop(LoopPtr)%LoopSide(LoopSidePtr)%Branch(BranchPtr)%Comp(CompPtr)%NumOpSchemes
              IF(PlantLoop(LoopPtr)%LoopSide(LoopSidePtr)%Branch(BranchPtr)%Comp(CompPtr)%Opscheme(CompOpNum)%OpschemePtr &
                        == OpNum)THEN
                PlantLoop(LoopPtr)%LoopSide(LoopSidePtr)%Branch(BranchPtr)%Comp(CompPtr)%CurCompLevelOpNum = CompOpNum
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ELSE
        PlantLoop(LoopNum)%OpScheme(OpNum)%Available = .FALSE.
      ENDIF

    ENDDO
!    IF(.NOT. foundscheme)THEN
!      !'call warning 'no current control scheme specified.  Loop Equipment will be shut down'
!    ENDIF
  ENDDO

END IF

IF ( errFlag2 ) THEN
  CALL ShowFatalError('InitLoadDistribution: Fatal error caused by previous severe error(s).')
ENDIF

RETURN
END SUBROUTINE InitLoadDistribution

! End Initialization Section of the Plant Loop Module
!******************************************************************************

! Begin Load Calculation/Distribution Section of the Plant Loop Module
!******************************************************************************

SUBROUTINE DistributePlantLoad(LoopNum, LoopSideNum, CurSchemePtr,ListPtr,LoopDemand,RemLoopDemand)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   July 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  July 2010
          !                      Sept 2010 B. Griffith, retain actual sign of load values

          ! PURPOSE OF THIS SUBROUTINE: This subroutine distributes the load
          ! to plant equipment according to one of two distribution schemes:
          !     OPTIMAL    = 1
          !     SEQUENTIAL = 2
          ! METHODOLOGY EMPLOYED:
          ! na
          ! REFERENCES:
          ! na
          ! USE STATEMENTS:
  USE DataLoopNode

  IMPLICIT NONE

           ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN )      :: LoopNum
  INTEGER, INTENT(IN )      :: LoopSideNum
  INTEGER, INTENT(IN )      :: CurSchemePtr !use as index in PlantLoop()Opscheme() data structure
  INTEGER, INTENT(IN )      :: ListPtr      !use as index in PlantLoop()Opscheme() data structure
  REAL(r64), INTENT(IN)     :: LoopDemand
  REAL(r64), INTENT(INOUT)  :: RemLoopDemand


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na
          ! INTERFACE BLOCK SPECIFICATIONS
          ! na
          ! DERIVED TYPE DEFINITIONS
          ! na
          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)                :: ChangeInLoad
  REAL(r64)                :: DivideLoad
  REAL(r64)                :: UniformLoad
  REAL(r64)                :: NewLoad
  INTEGER                  :: LoadFlag

  INTEGER                  :: BranchNum
  INTEGER                  :: CompNum
  INTEGER                  :: CompIndex
!  INTEGER                  :: EquipNum
  INTEGER                  :: NumCompsOnList



        ! load local variables
  NumCompsOnList = PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(ListPtr)%NumComps
  RemLoopDemand = LoopDemand
  IF (NumCompsOnList <= 0) RETURN
        !set flag to specify optimal or sequential loading of equipment
  LoadFlag = PlantLoop(LoopNum)%LoadDistribution

  IF (ABS(RemLoopDemand) < SmallLoad) THEN
   !no load to distribute
  ELSE

    SELECT CASE (LoadFlag)
    CASE (OptimalLoading) ! LoadFlag=1 indicates "optimal" load distribution
       !OPTIMAL DISTRIBUTION SCHEME
            !step 1: load all machines to optimal PLR
      DO CompIndex =1, NumCompsOnList
        BranchNum   =  PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(ListPtr)%Comp(CompIndex)%BranchNumPtr
        CompNum     =  PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(ListPtr)%Comp(CompIndex)%CompNumPtr
        IF(.NOT. PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Available) CYCLE

        IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Optload > 0.d0) THEN
          ChangeInLoad= MIN(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Optload,ABS(RemLoopDemand))
        ELSE
          ! this is for some components like cooling towers don't have well defined OptLoad
          ChangeInLoad = ABS(RemLoopDemand)
        ENDIF

        CALL AdjustChangeInLoadForLastStageUpperRangeLimit(LoopNum, CurSchemePtr, ListPtr, ChangeInLoad)

        CALL AdjustChangeInLoadByEMSControls(LoopNum, LoopSideNum, BranchNum, CompNum, ChangeInLoad)

        CALL AdjustChangeInLoadByHowServed(LoopNum, LoopSideNum, BranchNum, CompNum, ChangeInLoad)

        ChangeInLoad= MAX(0.0d0,ChangeInLoad)
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Myload = SIGN(ChangeInLoad, RemLoopDemand)

        RemLoopDemand = RemLoopDemand - PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Myload
        IF (ABS(RemLoopDemand) < SmallLoad) RemLoopDemand = 0.d0 !CR8631 don't just exit or %MyLoad on second device isn't reset
      END DO

            !step 2: Evenly distribute remaining loop demand
      IF (ABS(RemLoopDemand) > SmallLoad)THEN
        DivideLoad = ABS(RemLoopDemand)/NumCompsOnList
        DO CompIndex =1, NumCompsOnList
          BranchNum   =  PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(ListPtr)%Comp(CompIndex)%BranchNumPtr
          CompNum     =  PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(ListPtr)%Comp(CompIndex)%CompNumPtr
          IF(.NOT. PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Available)CYCLE
          NewLoad = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Myload
          NewLoad = MIN(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Maxload, &
                          ABS(NewLoad) + DivideLoad)
          ChangeInLoad = NewLoad - ABS(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Myload)
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Myload = SIGN(NewLoad, RemLoopDemand)
          RemLoopDemand= RemLoopDemand - SIGN(ChangeInLoad, RemLoopDemand)
          IF (ABS(RemLoopDemand) < SmallLoad) RemLoopDemand = 0.d0 !CR8631 don't just exit or %MyLoad on second device isn't reset
        END DO
      END IF

            ! step 3: If RemLoopDemand is still greater than zero, look for any machine
      IF (ABS(RemLoopDemand) > SmallLoad)THEN
        DO CompIndex =1, NumCompsOnList
          BranchNum   =  PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(ListPtr)%Comp(CompIndex)%BranchNumPtr
          CompNum     =  PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(ListPtr)%Comp(CompIndex)%CompNumPtr
          IF(.NOT. PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Available)CYCLE
          DivideLoad = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Maxload &
                       - ABS(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Myload)
          ChangeInLoad = MIN(ABS(RemLoopDemand), DivideLoad)
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Myload = &
              PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Myload + SIGN(ChangeInLoad, RemLoopDemand)
          RemLoopDemand= RemLoopDemand - SIGN(ChangeInLoad,RemLoopDemand)
          IF (ABS(RemLoopDemand) < SmallLoad) RemLoopDemand = 0.d0 !CR8631 don't just exit or %MyLoad on second device isn't reset
        END DO
      END IF

  !SEQUENTIAL DISTRIBUTION SCHEME
    CASE (SequentialLoading)! LoadFlag=2 indicates "sequential" load distribution

            ! step 1: Load machines in list order
      DO CompIndex =1, NumCompsOnList
        BranchNum   =  PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(ListPtr)%Comp(CompIndex)%BranchNumPtr
        CompNum     =  PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(ListPtr)%Comp(CompIndex)%CompNumPtr
        IF(.NOT. PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Available)CYCLE

        IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Maxload > 0.d0) THEN ! apply known limit
          ChangeInLoad = MIN(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Maxload, &
                              ABS(RemLoopDemand))
        ELSE
          ! this is for some components like cooling towers don't have well defined MaxLoad
          ChangeInLoad = ABS(RemLoopDemand)
        ENDIF

        CALL AdjustChangeInLoadForLastStageUpperRangeLimit(LoopNum, CurSchemePtr, ListPtr, ChangeInLoad)

        CALL AdjustChangeInLoadByEMSControls(LoopNum, LoopSideNum, BranchNum, CompNum, ChangeInLoad)

        CALL AdjustChangeInLoadByHowServed(LoopNum, LoopSideNum, BranchNum, CompNum, ChangeInLoad)

        ChangeInLoad = MAX(0.0d0, ChangeInLoad)
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Myload = SIGN(ChangeInLoad, RemLoopDemand)
        RemLoopDemand= RemLoopDemand - SIGN(ChangeInLoad, RemLoopDemand)
        IF (ABS(RemLoopDemand) < SmallLoad) RemLoopDemand = 0.d0 !CR8631 don't just exit or %MyLoad on second device isn't reset
      END DO


    !UNIFORM DISTRIBUTION SCHEME
    CASE (UniformLoading)! LoadFlag=3 indicates "uniform" load distribution

            ! step 1: distribute load equally to all machines
      UniformLoad = ABS(RemLoopDemand)/NumCompsOnList
      DO CompIndex =1, NumCompsOnList
        BranchNum   =  PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(ListPtr)%Comp(CompIndex)%BranchNumPtr
        CompNum     =  PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(ListPtr)%Comp(CompIndex)%CompNumPtr
        IF(.NOT. PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Available)CYCLE
        IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Maxload > 0.d0) THEN
          ChangeInLoad = MIN(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Maxload, &
                              UniformLoad)
        ELSE
          ! this is for some components like cooling towers don't have well defined MaxLoad
          ChangeInLoad = ABS(RemLoopDemand)
        ENDIF

        CALL AdjustChangeInLoadForLastStageUpperRangeLimit(LoopNum, CurSchemePtr, ListPtr, ChangeInLoad)

        CALL AdjustChangeInLoadByEMSControls(LoopNum, LoopSideNum, BranchNum, CompNum, ChangeInLoad)

        CALL AdjustChangeInLoadByHowServed(LoopNum, LoopSideNum, BranchNum, CompNum, ChangeInLoad)
        ChangeInLoad = MAX(0.0d0, ChangeInLoad)
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Myload = SIGN(ChangeInLoad, RemLoopDemand)
        RemLoopDemand = RemLoopDemand- SIGN(ChangeInLoad, RemLoopDemand)
        IF (ABS(RemLoopDemand) < SmallLoad) RemLoopDemand = 0.d0
      END DO

            ! step 2: If RemLoopDemand is not zero, then distribute remainder sequentially.
      IF (ABS(RemLoopDemand) > SmallLoad )THEN
        DO CompIndex =1, NumCompsOnList
          BranchNum   =  PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(ListPtr)%Comp(CompIndex)%BranchNumPtr
          CompNum     =  PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(ListPtr)%Comp(CompIndex)%CompNumPtr
          IF(.NOT. PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Available)CYCLE
           ChangeInLoad = MIN(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Maxload - &
                              ABS(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Myload), &
                              ABS(RemLoopDemand))
           ChangeInLoad = MAX(0.0d0, ChangeInLoad)
           PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Myload = &
           PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Myload + SIGN(ChangeInLoad, RemLoopDemand)
           RemLoopDemand= RemLoopDemand - SIGN(ChangeInLoad,RemLoopDemand)
           IF (ABS(RemLoopDemand) < SmallLoad ) RemLoopDemand = 0.d0
        END DO
      END IF
    END SELECT

  ENDIF ! load is small check

  ! now update On flags according to result for MyLoad
  DO CompIndex =1, NumCompsOnList
    BranchNum   =  PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(ListPtr)%Comp(CompIndex)%BranchNumPtr
    CompNum     =  PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(ListPtr)%Comp(CompIndex)%CompNumPtr
    IF (ABS(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Myload) < SmallLoad) THEN
      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%On = .FALSE.
    ELSE
      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%On = .TRUE.
    ENDIF

  ENDDO


  RETURN
END SUBROUTINE DistributePlantLoad

SUBROUTINE AdjustChangeInLoadForLastStageUpperRangeLimit(LoopNum, CurOpSchemePtr, CurEquipListPtr, ChangeInLoad)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   May 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! if this is the last stage for a load based operation, then limit load to upper range

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: LoopNum         ! component topology
  INTEGER, INTENT(IN)      :: CurOpSchemePtr  ! currect active operation scheme
  INTEGER, INTENT(IN)      :: CurEquipListPtr ! current equipment list
  REAL(r64), INTENT(INOUT) :: ChangeInLoad    ! positive magnitude of load change

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: RangeHiLimit

  IF (PlantLoop(LoopNum)%OpScheme(CurOpSchemePtr)%EquipListNumForLastStage == CurEquipListPtr) THEN ! at final last stage

    RangeHiLimit = PlantLoop(LoopNum)%OpScheme(CurOpSchemePtr)%EquipList(CurEquipListPtr)%RangeUpperLimit
    ChangeInLoad = MIN(ChangeInLoad, RangeHiLimit)
  ENDIF

  RETURN

END SUBROUTINE AdjustChangeInLoadForLastStageUpperRangeLimit


SUBROUTINE AdjustChangeInLoadByHowServed( LoopNum, LoopSideNum, BranchNum, CompNum, ChangeInLoad)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Nov 2011
          !       MODIFIED       March 2012, B. Griffith add controls for free cooling heat exchanger overrides of chillers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! central place to apply limits to machine load dispatch based on how the machine serves loads

          ! METHODOLOGY EMPLOYED:
          ! Components are machines on plant equipment operation lists.  Need to make adjustments to the
          ! load dispatch to account for limits and floating capacities.
          !

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
!  USE EconomizerHeatExchanger,  ONLY: GetEconHeatExchangerCurrentCapacity
  USE DataLoopNode, ONLY: Node
  USE DataEnvironment, ONLY : OutDryBulbTemp, OutWetBulbTemp
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: LoopNum ! component topology
  INTEGER, INTENT(IN)  :: LoopSideNum ! component topology
  INTEGER, INTENT(IN)  :: BranchNum ! component topology
  INTEGER, INTENT(IN)  :: CompNum ! component topology
  REAL(r64), INTENT(INOUT) :: ChangeInLoad ! positive magnitude of load change

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)                :: CurMassFlowRate = 0.d0
  REAL(r64)                :: ToutLowLimit    = 0.d0
  REAL(r64)                :: ToutHiLimit     = 0.d0
  REAL(r64)                :: TinLowLimit     = 0.d0
  REAL(r64)                :: Tinlet          = 0.d0
  REAL(r64)                :: Tsensor         = 0.d0
  REAL(r64)                :: CurSpecHeat     = 0.d0
  REAL(r64)                :: QdotTmp         = 0.d0
  INTEGER                  :: ControlNodeNum  = 0


  !start of bad band-aid, need a general and comprehensive approach for determining current capacity of all kinds of equipment
     ! Need to truncate the load down in case outlet temperature will hit a lower/upper limit
  SELECT CASE(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%HowLoadServed)

     !Chillers
  CASE(HowMet_ByNominalCapLowOutLimit) ! chillers with lower limit on outlet temperature

    !- Retrieve data from the plant loop data structure
    CurMassFlowRate = Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn)%MassFlowRate
    ToutLowLimit    = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MinOutletTemp
    Tinlet          = Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn)%Temp
    CurSpecHeat     = GetSpecificHeatGlycol(PlantLoop(LoopNum)%FluidName,Tinlet,PlantLoop(LoopNum)%FluidIndex, &
                                            'PlantCondLoopOperation:DistributePlantLoad')
    QdotTmp         = CurMassFlowRate*CurSpecHeat*(Tinlet-ToutLowLimit)

!        !- Don't correct if Q is zero, as this could indicate a component which this hasn't been implemented or not yet turned on
    IF(CurMassFlowRate > 0.d0) THEN
      ChangeInLoad = MIN(ChangeInLoad,QdotTmp)
    ENDIF

  CASE(HowMet_ByNominalCapFreeCoolCntrl)
    ! for chillers with free cooling shutdown (HeatExchanger:Hydronic currently)
      ! determine if free cooling controls shut off chiller
    TinLowLimit   = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%FreeCoolCntrlMinCntrlTemp
   SELECT CASE (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%FreeCoolCntrlMode)
    CASE (FreeCoolControlMode_WetBulb)
      Tsensor = OutWetBulbTemp
    CASE (FreeCoolControlMode_DryBulb)
      Tsensor = OutDryBulbTemp
    CASE (FreeCoolControlMode_Loop)
      ControlNodeNum = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%FreeCoolCntrlNodeNum
      IF (ControlNodeNum > 0) THEN
        Tsensor = Node(ControlNodeNum)%TempLastTimestep ! use lagged value for stability
      ELSE
        Tsensor = 23.d0
      ENDIF
    END SELECT

    IF (Tsensor < TinLowLimit) THEN ! turn off chiller to initiate free cooling
      ChangeInLoad = 0.d0
      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Available = .FALSE.
      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%FreeCoolCntrlShutDown = .TRUE.
    ELSE
      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Available = .TRUE.
      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%FreeCoolCntrlShutDown = .FALSE.
    ENDIF

  CASE(HowMet_ByNominalCapLowOutLimitFreeCoolCntrl)
    ! for chillers with free cooling shutdown (HeatExchanger:Hydronic currently)
      ! determine if free cooling controls shut off chiller
    TinLowLimit   = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%FreeCoolCntrlMinCntrlTemp
    SELECT CASE (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%FreeCoolCntrlMode)
    CASE (FreeCoolControlMode_WetBulb)
      Tsensor = OutWetBulbTemp
    CASE (FreeCoolControlMode_DryBulb)
      Tsensor = OutDryBulbTemp
    CASE (FreeCoolControlMode_Loop)
      ControlNodeNum = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%FreeCoolCntrlNodeNum
      IF (ControlNodeNum > 0) THEN
        Tsensor = Node(ControlNodeNum)%TempLastTimestep ! use lagged value for stability
      ELSE
        Tsensor = 23.d0
      ENDIF
    END SELECT

    IF (Tsensor < TinLowLimit) THEN ! turn off chiller to initiate free cooling
      ChangeInLoad = 0.d0
      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Available = .FALSE.
      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%FreeCoolCntrlShutDown = .TRUE.
    ELSE
      !- Retrieve data from the plant loop data structure
      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Available = .TRUE.
      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%FreeCoolCntrlShutDown = .FALSE.
      CurMassFlowRate = Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn)%MassFlowRate
      ToutLowLimit    = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MinOutletTemp
      Tinlet          = Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn)%Temp
      CurSpecHeat     = GetSpecificHeatGlycol(PlantLoop(LoopNum)%FluidName,Tinlet,PlantLoop(LoopNum)%FluidIndex, &
                                              'PlantCondLoopOperation:DistributePlantLoad')
      QdotTmp         = CurMassFlowRate*CurSpecHeat*(Tinlet-ToutLowLimit)

  !        !- Don't correct if Q is zero, as this could indicate a component which this hasn't been implemented or not yet turned on
      IF(CurMassFlowRate > 0.d0) THEN
        ChangeInLoad = MIN(ChangeInLoad,QdotTmp)
      ENDIF
    ENDIF

  CASE(HowMet_ByNominalCapHiOutLimit) ! boilers with upper limit on outlet temperature
    !- Retrieve data from the plant loop data structure
    CurMassFlowRate = Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn)%MassFlowRate
    ToutHiLimit     = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MaxOutletTemp
    Tinlet          = Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn)%Temp
    CurSpecHeat     = GetSpecificHeatGlycol(PlantLoop(LoopNum)%FluidName,Tinlet,PlantLoop(LoopNum)%FluidIndex, &
                                            'PlantCondLoopOperation:DistributePlantLoad')
    QdotTmp         = CurMassFlowRate*CurSpecHeat*(ToutHiLimit - Tinlet)

    IF(CurMassFlowRate> 0.d0) THEN
      ChangeInLoad = MIN(ChangeInLoad,QdotTmp)
    ENDIF

  CASE (HowMet_PassiveCap) ! need to estimate current capacity if more or less passive devices ??


  CASE DEFAULT

  END SELECT


  RETURN

END SUBROUTINE AdjustChangeInLoadByHowServed

SUBROUTINE FindCompSPLoad(LoopNum,LoopSideNum,BranchNum,CompNum,OpNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Sankaranarayanan K P
          !       DATE WRITTEN   Jan 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  Dan Fisher July 2010

          ! PURPOSE OF THIS SUBROUTINE:
          ! To calculate the load on a component controlled by
          ! Component SetPoint based scheme.

          ! USE STATEMENTS:
USE DataLoopNode, ONLY : Node, SensedNodeFlagValue
USE FluidProperties, ONLY: GetDensityGlycol

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN)         :: LoopNum
INTEGER, INTENT(IN)         :: LoopSideNum
INTEGER, INTENT(IN)         :: BranchNum
INTEGER, INTENT(IN)         :: CompNum
INTEGER, INTENT(IN)         :: OpNum  !index for Plant()%loopside()%branch()%comp()%opscheme()

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)                :: CompDemand
  REAL(r64)                :: DemandMdot
  REAL(r64)                :: ActualMdot
  REAL(r64)                :: TempIn
  REAL(r64)                :: CurSpecHeat
  REAL(r64)                :: TempSetPt
  REAL(r64)                :: CompMinLoad
  REAL(r64)                :: CompMaxLoad
  REAL(r64)                :: CompOptLoad
  INTEGER                  :: DemandNode
  INTEGER                  :: CompPtr
  INTEGER                  :: OpSchemePtr
  INTEGER                  :: ListPtr
  INTEGER                  :: SetPtNode
  INTEGER                  :: NumEquipLists
  REAL(r64)                :: rho
  REAL(r64)                :: CurrentDemandForCoolingOp
  REAL(r64)                :: CurrentDemandForHeatingOp

        !find the pointer to the 'PlantLoop()%OpScheme()'...data structure
  NumEquipLists = PlantLoop(LoopNum)%loopside(LoopSideNum)%branch(BranchNum)%comp(CompNum)%opscheme(OpNum)%NumEquipLists
  IF(NumEquipLists /= 1)THEN
    !CALL Severe error) there should be exactly one list associated with component setpoint scheme
  ENDIF

  OpSchemePtr = PlantLoop(LoopNum)%loopside(LoopSideNum)%branch(BranchNum)%comp(CompNum)%opscheme(OpNum)%OpSchemePtr
  ListPtr = PlantLoop(LoopNum)%loopside(LoopSideNum)%branch(BranchNum)%comp(CompNum)%opscheme(OpNum)%EquipList(1)%ListPtr
  CompPtr = PlantLoop(LoopNum)%loopside(LoopSideNum)%branch(BranchNum)%comp(CompNum)%opscheme(OpNum)%EquipList(1)%CompPtr


        !load local variables from the data structures
  CompMinLoad = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MinLoad
  CompMaxLoad = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MaxLoad
  CompOptLoad = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%OptLoad
  DemandMdot    = PlantLoop(LoopNum)%OpScheme(OpSchemePtr)%EquipList(ListPtr)%Comp(CompPtr)%SetPointFlowRate
  DemandNode    = PlantLoop(LoopNum)%OpScheme(OpSchemePtr)%EquipList(ListPtr)%Comp(CompPtr)%DemandNodeNum
  SetPtNode     = PlantLoop(LoopNum)%OpScheme(OpSchemePtr)%EquipList(ListPtr)%Comp(CompPtr)%SetPointNodeNum
  TempIn        = Node(DemandNode)%Temp
  rho   = GetDensityGlycol(PlantLoop(LoopNum)%FluidName,  &
                           TempIn, &
                           PlantLoop(LoopNum)%FluidIndex,&
                           'FindCompSPLoad')

  DemandMdot    = PlantLoop(LoopNum)%OpScheme(OpSchemePtr)%EquipList(ListPtr)%Comp(CompPtr)%SetPointFlowRate * rho
  !DSU?  DemandMDot is a constant design flow rate, next based on actual current flow rate for accurate current demand?
  ActualMdot    = Node(DemandNode)%MassFlowRate
  CurSpecHeat     = GetSpecificHeatGlycol(PlantLoop(loopNum)%FluidName,TempIn,PlantLoop(loopNum)%FluidIndex, &
                                            'FindCompSPLoad')
  IF ((ActualMdot > 0.d0) .AND. (ActualMdot /= DemandMdot)) THEN
    DemandMdot = ActualMdot
  ENDIF

  SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)
  CASE (SingleSetpoint)
    TempSetPt     = Node(SetPtNode)%TempSetPoint
  CASE (DualSetpointDeadband)
    IF (PlantLoop(LoopNum)%OpScheme(OpSchemePtr)%EquipList(ListPtr)%Comp(CompPtr)%CtrlTypeNum == CoolingOp) THEN
      TempSetPt     = Node(SetPtNode)%TempSetPointHi
    ELSEIF(PlantLoop(LoopNum)%OpScheme(OpSchemePtr)%EquipList(ListPtr)%Comp(CompPtr)%CtrlTypeNum == HeatingOP) THEN
      TempSetPt     = Node(SetPtNode)%TempSetPointLo
    ELSEIF(PlantLoop(LoopNum)%OpScheme(OpSchemePtr)%EquipList(ListPtr)%Comp(CompPtr)%CtrlTypeNum == DualOP)  THEN
      CurrentDemandForCoolingOp = DemandMdot*CurSpecHeat*(Node(SetPtNode)%TempSetPointHi - TempIn)
      CurrentDemandForHeatingOp = DemandMdot*CurSpecHeat*(Node(SetPtNode)%TempSetPointLo - TempIn)
      IF ((CurrentDemandForCoolingOp < 0.d0) .AND. (CurrentDemandForHeatingOp <= 0.d0)) THEN ! cooling
        TempSetPt     = Node(SetPtNode)%TempSetPointHi
      ELSEIF ((CurrentDemandForCoolingOp >= 0.d0) .AND. (CurrentDemandForHeatingOp > 0.d0)) THEN ! heating
        TempSetPt     = Node(SetPtNode)%TempSetPointLo
      ELSE ! deadband
        TempSetPt     = TempIn
      ENDIF

    ENDIF

  END SELECT



  IF(TempSetPt == SensedNodeFlagValue) THEN
     PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%ON = .FALSE.
     PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad = 0.d0
     PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%EquipDemand = 0.d0
  ELSE

    CompDemand = (DemandMdot*CurSpecHeat*(TempSetPt - TempIn))

    IF(ABS(CompDemand) < LoopDemandTol) CompDemand = 0.d0
    PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%EquipDemand = CompDemand

        !set MyLoad and runflag
    IF(PlantLoop(LoopNum)%OpScheme(OpSchemePtr)%EquipList(ListPtr)%Comp(CompPtr)%CtrlTypeNum == CoolingOp)THEN
      IF(CompDemand < (- LoopDemandTol))THEN
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%ON = .TRUE.
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad =  CompDemand
      ELSE
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%ON = .FALSE.
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad = 0.d0
      ENDIF
    ELSEIF(PlantLoop(LoopNum)%OpScheme(OpSchemePtr)%EquipList(ListPtr)%Comp(CompPtr)%CtrlTypeNum == HeatingOP)THEN
      IF(CompDemand > LoopDemandTol)THEN
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%ON = .TRUE.
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad = CompDemand
      ELSE
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%ON = .FALSE.
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad = 0.d0
      ENDIF
    ELSEIF(PlantLoop(LoopNum)%OpScheme(OpSchemePtr)%EquipList(ListPtr)%Comp(CompPtr)%CtrlTypeNum == DualOP)THEN
      IF(CompDemand > LoopDemandTol .OR. CompDemand < (- LoopDemandTol) )THEN
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%ON = .TRUE.
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad = CompDemand
      ELSE
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%ON = .FALSE.
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad = 0.d0
      ENDIF
    ENDIF

        !Check bounds on MyLoad
   IF  ( ABS(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad) > CompMaxLoad) THEN
     PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad = SIGN(CompMaxLoad, &
                                  PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad )
   ENDIF
!   PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad = &
!   MIN(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad,CompMaxLoad)

   IF (ABS(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad) < CompMinLoad) THEN
     PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad = SIGN( CompMinLoad, &
                                PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad )
   ENDIF
!   PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad = &
!   MAX(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad,CompMinLoad)

  END IF  !valid setpoint (TempSetPt /= SensedNodeFlagValue)
  RETURN
END SUBROUTINE FindCompSPLoad


SUBROUTINE DistributeUserDefinedPlantLoad(LoopNum, LoopSideNum,BranchNum,CompNum,CurCompLevelOpNum,CurSchemePtr,  &
   LoopDemand,RemLoopDemand )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   August 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,  ONLY: emsCallFromUserDefinedComponentModel
  USE EMSManager,   ONLY: ManageEMS

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)         :: LoopNum
  INTEGER, INTENT(IN)         :: LoopSideNum
  INTEGER, INTENT(IN)         :: BranchNum
  INTEGER, INTENT(IN)         :: CompNum
  INTEGER, INTENT(IN)         :: CurCompLevelOpNum  !index for Plant()%loopside()%branch()%comp()%opscheme()
  INTEGER, INTENT(IN)         :: CurSchemePtr
  REAL(r64), INTENT(IN)       :: LoopDemand
  REAL(r64), INTENT(INOUT)    :: RemLoopDemand

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                  :: CompPtr
  INTEGER                  :: ListPtr

 ! ListPtr = PlantLoop(LoopNum)%loopside(LoopSideNum)%branch(BranchNum)%comp(CompNum)%opscheme(CurCompLevelOpNum)%EquipList(1)%ListPtr
  CompPtr = PlantLoop(LoopNum)%loopside(LoopSideNum)%branch(BranchNum)%comp(CompNum)  &
     %opscheme(CurCompLevelOpNum)%EquipList(1)%CompPtr

  ! fill internal variable
  PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(1)%Comp(CompPtr)%EMSIntVarRemainingLoadValue = LoopDemand

  ! Call EMS program(s)
  IF ( PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%ErlSimProgramMngr > 0) THEN
    CALL ManageEMS(emsCallFromUserDefinedComponentModel, &
                       ProgramManagerToRun = PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%ErlSimProgramMngr)
  ENDIF

  ! move actuated value to MyLoad

  PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad = &
                               PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(1)%Comp(CompPtr)%EMSActuatorDispatchedLoadValue
  PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%EquipDemand = &
                               PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%EquipList(1)%Comp(CompPtr)%EMSActuatorDispatchedLoadValue
  IF (ABS(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad) > LoopDemandTol) THEN
    PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%ON = .TRUE.

  ELSE
    PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%ON = .FALSE.
  ENDIF

  RETURN

END SUBROUTINE DistributeUserDefinedPlantLoad

! End Load Calculation/Distribution Section of the Plant Loop Module
!******************************************************************************

!********************************
REAL(r64) FUNCTION FindRangeVariable (LoopNum, CurSchemePtr, CurSchemeType)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Sankaranarayanan K P
          !       DATE WRITTEN   Jan 2004
          !       MODIFIED       Chandan Sharma, August 2010
          !       RE-ENGINEERED  na

          ! USE STATEMENTS:
  USE DataLoopNode
  USE DataEnvironment, ONLY: OutWetBulbTemp, OutDryBulbTemp, OutDewPointTemp

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)     :: LoopNum        ! PlantLoop data structure loop counter
  INTEGER, INTENT(IN)     :: CurSchemePtr   ! set by PL()%LoopSide()%Branch()%Comp()%Opscheme()%OpSchemePtr
                                            ! used to locate data in PL()%Opscheme(CurSchemePtr)
  INTEGER, INTENT(IN)     :: CurSchemeType  ! identifier set in PlantData
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER          :: ReferenceNodeNum
  REAL(r64)        :: NodeTemperature

  OperationScheme: SELECT CASE (CurSchemeType)

    CASE (DrybulbTDBOpSchemeType) ! drybulb temp based controls
        ReferenceNodeNum = PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%ReferenceNodeNumber
        NodeTemperature = Node(ReferenceNodeNum)%Temp
        FindRangeVariable = NodeTemperature - OutDryBulbTemp
    CASE (WetBulbTDBOpSchemeType) ! wetbulb temp based controls
        ReferenceNodeNum = PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%ReferenceNodeNumber
        NodeTemperature = Node(ReferenceNodeNum)%Temp
        FindRangeVariable = NodeTemperature - OutWetBulbTemp
    CASE (DewpointTDBOpSchemeType) ! dewpoint temp based controls
        ReferenceNodeNum = PlantLoop(LoopNum)%OpScheme(CurSchemePtr)%ReferenceNodeNumber
        NodeTemperature = Node(ReferenceNodeNum)%Temp
        FindRangeVariable = NodeTemperature - OutDewPointTemp
    END SELECT OperationScheme
    !Objexx:Return Check/enforce that one of these CASEs holds or add a default case to assure return value is set

    RETURN

END FUNCTION FindRangeVariable

!********************************

! Begin Plant Loop ON/OFF Utility Subroutines
!******************************************************************************

SUBROUTINE TurnOnPlantLoopPipes(LoopNum,LoopSideNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   July 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This subroutine sets a logical flag
          ! for the loop circulation pump to TRUE.

          ! METHODOLOGY EMPLOYED:
          ! na
          ! REFERENCES:
          ! na
          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: LoopNum
  INTEGER, INTENT(IN)    :: LoopSideNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na
          ! INTERFACE BLOCK SPECIFICATIONS
          ! na
          ! DERIVED TYPE DEFINITIONS
          ! na
          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                :: MachineOnLoopNum
  INTEGER                :: Num

  DO Num = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalBranches
   DO MachineOnLoopNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(Num)%TotalComponents
    SELECT CASE (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(Num)%Comp(MachineOnLoopNum)%TypeOf_Num)
      CASE (TypeOf_Pipe, TypeOf_PipeInterior, TypeOf_PipeExterior, TypeOf_PipeUnderground)
        PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(Num)%Comp(MachineOnLoopNum)%ON = .TRUE.
      CASE DEFAULT
        !Don't do anything
    END SELECT
   END DO
  END DO

  RETURN
END SUBROUTINE TurnOnPlantLoopPipes

SUBROUTINE TurnOffLoopEquipment(LoopNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         D.E. Fisher
          !       DATE WRITTEN   July 1998
          !       MODIFIED       D.E. Fisher, Aug. 2010
          !       RE-ENGINEERED

          ! PURPOSE OF THIS SUBROUTINE:
          ! METHODOLOGY EMPLOYED:
          ! na
          ! REFERENCES:
          ! na
          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
     INTEGER, INTENT(IN)            :: LoopNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na
          ! INTERFACE BLOCK SPECIFICATIONS
          ! na
          ! DERIVED TYPE DEFINITIONS
          ! na
          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
     INTEGER                        :: MachineOnBranch
     INTEGER                        :: LoopSideNum
     INTEGER                        :: Num

    DO LoopSideNum =1, 2
      DO Num =1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalBranches
       DO MachineOnBranch = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(Num)%TotalComponents
        !Sankar Non Integrated Economizer
        IF(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(Num)%Comp(MachineOnBranch)%GeneralEquipType /= GenEquipTypes_Pump) THEN
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(Num)%Comp(MachineOnBranch)%ON = .FALSE.
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(Num)%Comp(MachineOnBranch)%Myload = 0.0d0
        ENDIF
       END DO
      END DO
    END DO

  RETURN
END SUBROUTINE TurnOffLoopEquipment

SUBROUTINE TurnOffLoopSideEquipment(LoopNum,LoopSideNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         D.E. Fisher
          !       DATE WRITTEN   July 1998
          !       MODIFIED       D.E. Fisher, Aug. 2010
          !       RE-ENGINEERED

          ! PURPOSE OF THIS SUBROUTINE:
          ! METHODOLOGY EMPLOYED:
          ! na
          ! REFERENCES:
          ! na
          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
     INTEGER, INTENT(IN)            :: LoopNum
     INTEGER, INTENT(IN)            :: LoopSideNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na
          ! INTERFACE BLOCK SPECIFICATIONS
          ! na
          ! DERIVED TYPE DEFINITIONS
          ! na
          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
     INTEGER                        :: MachineOnBranch
     INTEGER                        :: Num

      DO Num =1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalBranches
       DO MachineOnBranch = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(Num)%TotalComponents
        !Sankar Non Integrated Economizer
        IF(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(Num)%Comp(MachineOnBranch)%GeneralEquipType /= GenEquipTypes_Pump ) THEN
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(Num)%Comp(MachineOnBranch)%ON = .FALSE.
          PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(Num)%Comp(MachineOnBranch)%Myload = 0.0d0
        ENDIF
       END DO
      END DO

  RETURN
END SUBROUTINE TurnOffLoopSideEquipment

! End Plant Loop ON/OFF Utility Subroutines
!******************************************************************************


! Begin Plant EMS Control Routines
!******************************************************************************

SUBROUTINE SetupPlantEMSActuators

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         D.E. Fisher
          !       DATE WRITTEN   Feb 2007
          !       MODIFIED       B. Griffith August 2009, D. Fisher, Aug. 2010
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine loads the plant EMS actuators

          ! METHODOLOGY EMPLOYED:
          ! Call the setupAcuator routine

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

USE DataInterfaces, ONLY: SetupEMSActuator
          ! SUBROUTINE ARGUMENT DEFINITIONS

          ! SUBROUTINE PARAMETER DEFINITIONS
          ! na

          ! SUBROUTINE VARIABLE DEFINITIONS
 CHARACTER(len=MaxNameLength) :: ActuatorType
 CHARACTER(len=MaxNameLength) :: ActuatorName
 CHARACTER(len=MaxNameLength) :: UniqueIDName
 CHARACTER(len=MaxNameLength) :: Units = '[on/off]'
! INTEGER                      :: NumAct
 INTEGER                      :: LoopNum
 INTEGER                      :: LoopSideNum
 INTEGER                      :: BranchNum
 INTEGER                      :: CompNum


   DO LoopNum = 1, TotNumLoops
     ActuatorName  = 'Plant Loop Overall'
     UniqueIDName  = PlantLoop(LoopNum)%Name
     ActuatorType  = 'On/Off Supervisory'
     CALL SetupEMSActuator(ActuatorName,  UniqueIDName, ActuatorType, Units, PlantLoop(LoopNum)%EMSCtrl, &
                                 PlantLoop(LoopNum)%EMSValue)

     ActuatorName  = 'Supply Side Half Loop'
     UniqueIDName  = TRIM(PlantLoop(LoopNum)%Name)
     ActuatorType  = 'On/Off Supervisory'
     CALL SetupEMSActuator(ActuatorName,  UniqueIDName, ActuatorType, Units, PlantLoop(LoopNum)%LoopSide(SupplySide)%EMSCtrl, &
                                 PlantLoop(LoopNum)%LoopSide(SupplySide)%EMSValue)

     ActuatorName  = 'Demand Side Half Loop'
     UniqueIDName  = TRIM(PlantLoop(LoopNum)%Name)
     ActuatorType  = 'On/Off Supervisory'
     CALL SetupEMSActuator(ActuatorName,  UniqueIDName, ActuatorType, Units, PlantLoop(LoopNum)%LoopSide(DemandSide)%EMSCtrl, &
                                PlantLoop(LoopNum)%LoopSide(DemandSide)%EMSValue)

     DO LoopSideNum = 1, 2
       DO BranchNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%TotalBranches
         IF (LoopSideNum == SupplySide) THEN
           ActuatorName = 'Supply Side Branch'
           UniqueIDName   = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Name
           ActuatorType  = 'On/Off Supervisory'
           CALL SetupEMSActuator(ActuatorName,  UniqueIDName, ActuatorType, Units, &
                                   PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%EMSCtrlOverrideOn, &
                                   PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%EMSCtrlOverrideValue)
         ELSEIF (LoopSideNum == DemandSide) THEN
           ActuatorName = 'Demand Side Branch'
           UniqueIDName   = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Name
           ActuatorType  = 'On/Off Supervisory'
           CALL SetupEMSActuator(ActuatorName,  UniqueIDName, ActuatorType, Units, &
                                   PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%EMSCtrlOverrideOn, &
                                   PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%EMSCtrlOverrideValue)
         ENDIF
         DO CompNum = 1, PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%TotalComponents
            ActuatorName = 'Plant Component ' &
                  //Trim(ccSimPlantEquipTypes(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num))
            UniqueIDName   = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Name
            ActuatorType  = 'On/Off Supervisory'
            CALL SetupEMSActuator(ActuatorName,  UniqueIDName, ActuatorType, '[W]', &
                                     PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%EMSLoadOverrideOn, &
                                     PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%EMSLoadOverrideValue)
         END DO
       END DO
     END DO
   END DO

RETURN
END SUBROUTINE SetupPlantEMSActuators

SUBROUTINE ActivateEMSControls(LoopNum,LoopSideNum, BranchNum, CompNum, LoopShutDownFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         D.E. Fisher
          !       DATE WRITTEN   Feb 2007
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine loads the plant EMS actuators

          ! METHODOLOGY EMPLOYED: The EMS flags are evaluated in hierarchical order:
          !     LOOP flags override branch and component flags
          !     BRANCH flags override component flags
          ! If the loop flag (EMSCtrl) is true, then
          !     IF EMSValue <= 0, shut down the entire loop including the pumps
          !     IF EMSValue > 0, no action
          ! If the LoopSide flag (EMSCtrl) is true, then:
          !     IF EMSValue <=0, shut down all components on the loopside except the pumps
          !     IF EMSValue > 0, no action
          ! If a component flag (EMSCtrl) is true, then:
          !     EMSValue <=0, shut down the component
          !     EMSValue > 0, calc. component load: MyLoad=MIN(MaxCompLoad,MaxCompLoad*EMSValue)

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataLoopNode

          ! SUBROUTINE ARGUMENT DEFINITIONS
 INTEGER,INTENT(IN)         :: LoopNum
 INTEGER,INTENT(IN)         :: LoopSideNum
 INTEGER,INTENT(IN)         :: BranchNum
 INTEGER,INTENT(IN)         :: CompNum
 LOGICAL,INTENT(INOUT)      :: LoopShutdownFlag

          ! SUBROUTINE PARAMETER DEFINITIONS
          ! na

          ! SUBROUTINE VARIABLE DEFINITIONS
 REAL(r64)                  :: CurMassFlowRate
 REAL(r64)                  :: ToutLowLimit
 REAL(r64)                  :: Tinlet
 REAL(r64)                  :: CurSpecHeat
 REAL(r64)                  :: QTemporary
!unused REAL(r64)                  :: ChangeInLoad

  !MODULE VARIABLE DECLARATIONS:

        !Loop Control
     IF(PlantLoop(LoopNum)%EMSCtrl)THEN
       IF(PlantLoop(LoopNum)%EMSValue <= 0.0d0)THEN
         LoopShutdownFlag = .TRUE.
         CALL TurnOffLoopEquipment(LoopNum)
         RETURN
       ELSE
         LoopShutdownFlag = .FALSE.
       ENDIF
     ELSE
       LoopShutdownFlag = .FALSE.
     ENDIF

        !Half-loop control
     IF( PlantLoop(LoopNum)%LoopSide(LoopSideNum)%EMSCtrl)THEN
       IF(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%EMSValue <= 0.0d0)THEN
         CALL TurnOffLoopSideEquipment(LoopNum,LoopSideNum)
         RETURN
       ELSE
         !do nothing:  can't turn all loopside equip. ON with loop switch
       ENDIF
     ENDIF

     IF(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%EMSLoadOverrideOn)THEN
           !EMSValue <= 0 turn component OFF
       IF(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%EMSLoadOverrideValue <= 0.0d0)THEN
         PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%ON = .FALSE.
         PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Available = .FALSE.
         PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad =0.0d0
         RETURN
       ELSE
           !EMSValue > 0 Set Component Load and Turn component ON
         PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%ON = .TRUE.
         PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Available = .FALSE.
         PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad = &
         MIN(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Maxload, &
           (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Maxload * &
           PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%EMSLoadOverrideValue))

           ! Check lower/upper temperature limit for chillers
         SELECT CASE(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num)
           CASE(TypeOf_Chiller_ElectricEIR,TypeOf_Chiller_Electric,TypeOf_Chiller_ElectricReformEIR)

             !- Retrieve data from the plant loop data structure
             CurMassFlowRate = Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn)%MassFlowRate
             ToutLowLimit    = PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MinOutletTemp
             Tinlet          = Node(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn)%Temp
             CurSpecHeat     = GetSpecificHeatGlycol(PlantLoop(loopNum)%FluidName,Tinlet,PlantLoop(loopNum)%FluidIndex, &
                                                     'ActivateEMSControls')
             QTemporary      = CurMassFlowRate*CurSpecHeat*(Tinlet-ToutLowLimit)

             !- Don't correct if Q is zero, as this could indicate a component which this hasn't been implemented
             IF(QTemporary.GT.0.0d0)THEN

!unused               ChangeInLoad = MIN(ChangeInLoad,QTemporary)
               ! DSU?  weird ems thing here?
               IF ( ABS(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad) >  &
                      PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Maxload  ) THEN
                 PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad =  SIGN(     &
                           PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Maxload,  &
                           PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad    )
               ENDIF
               IF ( ABS(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad) >  &
                      QTemporary ) THEN
                 PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad =  SIGN( &
                           QTemporary,  &
                           PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad    )
               ENDIF

!               PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%MyLoad = &
!               MIN((PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%Maxload * &
!                  PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%EMSValue),Qtemporary)
             ENDIF
           CASE DEFAULT
             !Nothing Changes for now, could add in case statements for boilers, which would use upper limit temp check
         END SELECT
         RETURN
       ENDIF   !EMSValue <=> 0
     ENDIF  !EMSFlag

RETURN
END SUBROUTINE ActivateEMSControls

SUBROUTINE AdjustChangeInLoadByEMSControls(LoopNum, LoopSideNum, BranchNum, CompNum, ChangeInLoad)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   April 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! modify load dispatch if EMS controls are in place for a specific component

          ! METHODOLOGY EMPLOYED:
          ! Check if Loop Side is shutdown
          !  then check if branch is shutdown
          ! then  check if component is overridden and use the value if it is.
          ! take ABS() of EMS value to ensure sign is correct.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
 INTEGER,INTENT(IN)         :: LoopNum
 INTEGER,INTENT(IN)         :: LoopSideNum
 INTEGER,INTENT(IN)         :: BranchNum
 INTEGER,INTENT(IN)         :: CompNum
 REAL(r64), INTENT(INOUT)   :: ChangeInLoad ! positive magnitude of load change

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  IF ((PlantLoop(LoopNum)%LoopSide(LoopSideNum)%EMSCtrl) .AND. &
      (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%EMSValue <= 0.d0)) THEN
    ChangeInLoad = 0.d0
    RETURN
  ENDIF

  IF ((PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%EMSCtrlOverrideOn) .AND. &
      (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%EMSCtrlOverrideValue <= 0.d0)) THEN
    ChangeInLoad = 0.d0
    RETURN
  ENDIF

  IF(PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%EMSLoadOverrideOn) THEN
    IF (PlantLoop(LoopNum)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(CompNum)%EMSLoadOverrideValue == 0.d0) THEN
      ChangeInLoad = 0.d0
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE AdjustChangeInLoadByEMSControls


!*END PLANT EMS CONTROL ROUTINES!
!******************************************************************************

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

END MODULE PlantCondLoopOperation