MODULE OutsideEnergySources

  ! MODULE INFORMATION:
  !       AUTHOR         Dan Fisher
  !       DATE WRITTEN   Unknown
  !       MODIFIED       na
  !       RE-ENGINEERED  Brent Griffith, Sept 2010, revised plant interactions.

  ! PURPOSE OF THIS MODULE:
  ! Module containing the routines dealing with the OutsideEnergySources

  ! METHODOLOGY EMPLOYED:
  ! Needs description, as appropriate.

  ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength, SecInHour
USE DataInterfaces, ONLY: ShowWarningError, ShowSevereError, ShowFatalError, SetupOutputVariable, &
                       ShowContinueError
USE DataEnvironment
USE DataHVACGlobals
USE DataLoopNode
USE General,  ONLY: TrimSigDigits
USE DataPlant,  ONLY: PlantLoop, TypeOf_PurchHotWater, TypeOf_PurchChilledWater, ScanPlantLoopsForObject

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS
INTEGER, PARAMETER :: EnergyType_DistrictHeating = 1
INTEGER, PARAMETER :: EnergyType_DistrictCooling = 2

  ! DERIVED TYPE DEFINITIONS
TYPE OutsideEnergySourceSpecs
  CHARACTER(len=MaxNameLength) :: PlantLoopID        = ' '  ! main plant loop ID
  CHARACTER(len=MaxNameLength) :: SecndryLoopID      = ' '  ! secondary chiller loop (cond loop) ID
  CHARACTER(len=MaxNameLength) :: ScheduleID         = ' '  ! equipment availability schedule
  CHARACTER(len=MaxNameLength) :: Name               = ' '  ! user identifier
  REAL(r64)                    :: NomCap             = 0.d0 ! design nominal capacity of district service
  INTEGER                      :: CapFractionSchedNum = 0   ! capacity modifier schedule number
  INTEGER                      :: InletNodeNum       = 0    ! Node number on the inlet side of the plant
  INTEGER                      :: OutletNodeNum      = 0    ! Node number on the inlet side of the plant
  REAL(r64)                    :: EnergyTransfer     = 0.d0 ! cooling energy provided in time step
  REAL(r64)                    :: EnergyRate         = 0.d0 ! cooling power
  INTEGER                      :: EnergyType         = 0    ! flag for district heating OR cooling
  INTEGER                      :: MassFlowReSimIndex = 0
  !loop topology variables
  INTEGER                      :: LoopNum            = 0
  INTEGER                      :: LoopSideNum        = 0
  INTEGER                      :: BranchNum          = 0
  INTEGER                      :: CompNum            = 0
  !flags
  LOGICAL                      :: OneTimeInitFlag    = .TRUE.
  LOGICAL                      :: BeginEnvrnInitFlag = .TRUE.
  LOGICAL                      :: CheckEquipName     = .TRUE.
END TYPE OutsideEnergySourceSpecs

TYPE ReportVars
  REAL(r64)         :: MassFlowRate   =0.0d0
  REAL(r64)         :: InletTemp      =0.0d0
  REAL(r64)         :: OutletTemp     =0.0d0
  REAL(r64)         :: EnergyTransfer =0.0d0
END TYPE ReportVars

  !MODULE VARIABLE DECLARATIONS:
INTEGER                                                   :: NumDistrictUnits = 0
TYPE(OutsideEnergySourceSpecs), ALLOCATABLE, DIMENSION(:) :: EnergySource
TYPE(ReportVars),               ALLOCATABLE, DIMENSION(:) :: EnergySourceReport

          ! SUBROUTINE SPECIFICATIONS FOR MODULE OutsideEnergySources
PUBLIC    SimOutsideEnergy
PRIVATE   SimDistrictEnergy
PRIVATE   GetOutsideEnergySourcesInput
PRIVATE   InitSimVars
PRIVATE   UpdateRecords


CONTAINS

SUBROUTINE SimOutsideEnergy(EnergyType,EquipName,EquipFlowCtrl,CompIndex,RunFlag, &
                             InitLoopEquip,MyLoad,MaxCap,MinCap,OptCap, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Sept. 1998
          !       MODIFIED       May 2010; Edwin Lee; Linda Lawrie (consolidation)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Manage the simulation of district (aka purchased) energy.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: EnergyType
  CHARACTER(len=*), INTENT(IN) :: EquipName
  INTEGER, INTENT(IN)          :: EquipFlowCtrl  ! Flow control mode for the equipment
  INTEGER, INTENT(INOUT)       :: CompIndex
  LOGICAL, INTENT(IN)          :: RunFlag
  LOGICAL, INTENT(IN)          :: InitLoopEquip
  REAL(r64), INTENT(INOUT)     :: MyLoad
  REAL(r64), INTENT(INOUT)     :: MinCap
  REAL(r64), INTENT(INOUT)     :: MaxCap
  REAL(r64), INTENT(INOUT)     :: OptCap
  LOGICAL, INTENT(IN)          :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE   :: GetInputFlag = .true. ! Get input once and once only
  INTEGER         :: EqNum
  REAL(r64)       :: InletTemp
  REAL(r64)       :: OutletTemp
  REAL(r64)       :: MassFlowRate
          !FLOW

          !GET INPUT
  IF (GetInputFlag) THEN
    CALL GetOutsideEnergySourcesInput
    GetInputFlag=.false.
  ENDIF

  ! Find the correct Equipment
  IF (CompIndex == 0) THEN
    EqNum = FindItemInList(EquipName, EnergySource%Name, NumDistrictUnits)
    IF (EqNum == 0) THEN
      CALL ShowFatalError('SimOutsideEnergy: Unit not found='//TRIM(EquipName))
    ENDIF
    CompIndex=EqNum
  ELSE
    EqNum=CompIndex
    IF (EnergySource(EqNum)%CheckEquipName) THEN
      IF (EqNum > NumDistrictUnits .or. EqNum < 1) THEN
        CALL ShowFatalError('SimOutsideEnergy:  Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(EqNum))// &
                            ', Number of Units='//TRIM(TrimSigDigits(NumDistrictUnits))//  &
                            ', Entered Unit name='//TRIM(EquipName))
      ENDIF
      IF (EquipName /= EnergySource(EqNum)%Name) THEN
        CALL ShowFatalError('SimOutsideEnergy: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(EqNum))// &
                            ', Unit name='//TRIM(EquipName)//', stored Unit Name for that index='//  &
                            TRIM(EnergySource(EqNum)%Name))
      ENDIF
      EnergySource(EqNum)%CheckEquipName=.false.
    ENDIF
  ENDIF

          !CALCULATE
  IF (InitLoopEquip) THEN
    CALL InitSimVars(EqNum,MassFlowRate,InletTemp,OutletTemp, MyLoad)
    MinCap = 0.0d0
    MaxCap = EnergySource(EqNum)%NomCap
    OptCap = EnergySource(EqNum)%NomCap
    RETURN
  END IF

  CALL InitSimVars(EqNum,MassFlowRate,InletTemp,OutletTemp, MyLoad)
  CALL SimDistrictEnergy(RunFlag,EqNum,MyLoad,MassFlowRate,InletTemp,OutletTemp)
  CALL UpdateRecords(MyLoad,EqNum,MassFlowRate,OutletTemp)

RETURN
END SUBROUTINE SimOutsideEnergy

! End OutsideEnergySources Module Driver Subroutines
!******************************************************************************


! Beginning of OutsideEnergySources Module Get Input subroutines
!******************************************************************************

SUBROUTINE GetOutsideEnergySourcesInput
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   April 1998
          !       MODIFIED       May 2010; Edwin Lee; Linda Lawrie (consolidation)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
            ! This routine obtains the input data puts it into the
            ! component arrays. Data items in the component arrays
            ! are initialized. Output variables are set up.


          ! METHODOLOGY EMPLOYED: to be determined...

          ! USE STATEMENTS:
    USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName
    USE DataIPShortCuts
    USE NodeInputManager, ONLY: GetOnlySingleNode
    USE BranchNodeConnections, ONLY: TestCompSet
    USE ScheduleManager,       ONLY: GetScheduleIndex, CheckScheduleValueMinMax
    USE DataGlobals,           ONLY: ScheduleAlwaysOn

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
  INTEGER                     :: EnergySourceNum
  INTEGER                     :: NumAlphas ! Number of elements in the alpha array
  INTEGER                     :: NumNums   ! Number of elements in the numeric array
  INTEGER                     :: IOStat    ! IO Status when calling get input subroutine
  INTEGER                     :: NumDistrictUnitsHeat
  INTEGER                     :: NumDistrictUnitsCool
  INTEGER                     :: IndexCounter
  LOGICAL                     :: ErrorsFound = .false.   ! If errors detected in input
  LOGICAL                     :: IsNotOK               ! Flag to verify name
  LOGICAL                     :: IsBlank               ! Flag for blank name

          !GET NUMBER OF ALL EQUIPMENT TYPES
  cCurrentModuleObject    = 'DistrictHeating'
  NumDistrictUnitsHeat    = GetNumObjectsFound(cCurrentModuleObject)
  cCurrentModuleObject    = 'DistrictCooling'
  NumDistrictUnitsCool    = GetNumObjectsFound(cCurrentModuleObject)
  NumDistrictUnits = NumDistrictUnitsHeat + NumDistrictUnitsCool

  IF(ALLOCATED(EnergySource))RETURN

  ALLOCATE(EnergySource(NumDistrictUnits))
  ALLOCATE(EnergySourceReport(NumDistrictUnits))

  cCurrentModuleObject    = 'DistrictHeating'

  EnergySourceNum = 0
  DO IndexCounter = 1 , NumDistrictUnitsHeat
    EnergySourceNum = EnergySourceNum + 1
    CALL GetObjectItem(cCurrentModuleObject,EnergySourceNum,cAlphaArgs,NumAlphas, &
                       rNumericArgs,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, &
                        AlphaFieldNames= cAlphaFieldNames)

    IF (EnergySourceNum > 1) THEN
      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),EnergySource%Name,EnergySourceNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
    ENDIF
    EnergySource(EnergySourceNum)%Name               = cAlphaArgs(1)
    EnergySource(EnergySourceNum)%PlantLoopID = ' '
    EnergySource(EnergySourceNum)%SecndryLoopID = ' '
    EnergySource(EnergySourceNum)%ScheduleID = ' '
    EnergySource(EnergySourceNum)%InletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    EnergySource(EnergySourceNum)%OutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(2),cAlphaArgs(3),'Hot Water Nodes')
    EnergySource(EnergySourceNum)%NomCap             = rNumericArgs(1)
    EnergySource(EnergySourceNum)%EnergyTransfer = 0.0d0
    EnergySource(EnergySourceNum)%EnergyRate = 0.0d0
    EnergySource(EnergySourceNum)%EnergyType = EnergyType_DistrictHeating
    IF (.not. lAlphaFieldBlanks(4)) THEN
      EnergySource(EnergySourceNum)%CapFractionSchedNum =  GetScheduleIndex(cAlphaArgs(4))
      IF (EnergySource(EnergySourceNum)%CapFractionSchedNum == 0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(EnergySource(EnergySourceNum)%Name)// &
                             '", is not valid')
        CALL ShowContinueError(TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'" was not found.')
        ErrorsFound=.TRUE.
      ENDIF
      IF (.not. CheckScheduleValueMinMax(EnergySource(EnergySourceNum)%CapFractionSchedNum, '>=', 0.0d0) ) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(EnergySource(EnergySourceNum)%Name)// &
                             '", is not valid')
        CALL ShowContinueError(TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'" should not have negative values.')
        CALL ShowContinueError('Negative values will be treated as zero, and the simulation continues.')
      ENDIF
    ELSE
      EnergySource(EnergySourceNum)%CapFractionSchedNum= ScheduleAlwaysOn
    ENDIF
  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '//TRIM(cCurrentModuleObject)// &
            ', Preceding condition caused termination.')
  ENDIF

  EnergySourceNum = 0
  DO IndexCounter = 1 , NumDistrictUnitsHeat
    EnergySourceNum = EnergySourceNum + 1
    CALL SetupOutputVariable('District Heating Hot Water Energy [J]',EnergySource(EnergySourceNum)%EnergyTransfer, &
                             'System','Sum',EnergySource(EnergySourceNum)%Name,  &
                              ResourceTypeKey='DistrictHeating',EndUseKey='Heating',GroupKey='Plant')
    CALL SetupOutputVariable('District Heating Hot Water Rate [W]',EnergySource(EnergySourceNum)%EnergyRate, &
                             'System','Average',EnergySource(EnergySourceNum)%Name)

    CALL SetupOutputVariable('District Heating Rate [W]', &
          EnergySource(EnergySourceNum)%EnergyRate,'System','Average',EnergySource(EnergySourceNum)%Name)
    CALL SetupOutputVariable('District Heating Inlet Temperature [C]', &
          EnergySourceReport(EnergySourceNum)%InletTemp,'System','Average',EnergySource(EnergySourceNum)%Name)
    CALL SetupOutputVariable('District Heating Outlet Temperature [C]', &
          EnergySourceReport(EnergySourceNum)%OutletTemp,'System','Average',EnergySource(EnergySourceNum)%Name)
    CALL SetupOutputVariable('District Heating Mass Flow Rate [kg/s]', &
          EnergySourceReport(EnergySourceNum)%MassFlowRate,'System','Average',EnergySource(EnergySourceNum)%Name)
  END DO

  cCurrentModuleObject = 'DistrictCooling'

  EnergySourceNum = NumDistrictUnitsHeat !To initialize counter
  DO IndexCounter = 1 , NumDistrictUnitsCool
    EnergySourceNum = EnergySourceNum + 1
    CALL GetObjectItem(cCurrentModuleObject,IndexCounter,cAlphaArgs,NumAlphas, &
                       rNumericArgs,NumNums,IOSTAT, AlphaBlank=lAlphaFieldBlanks, &
                        AlphaFieldNames= cAlphaFieldNames)

    IF (EnergySourceNum > 1) THEN
      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),EnergySource%Name,EnergySourceNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
    ENDIF
    EnergySource(EnergySourceNum)%Name               = cAlphaArgs(1)
    EnergySource(EnergySourceNum)%PlantLoopID = ' '
    EnergySource(EnergySourceNum)%SecndryLoopID = ' '
    EnergySource(EnergySourceNum)%ScheduleID = ' '
    EnergySource(EnergySourceNum)%InletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    EnergySource(EnergySourceNum)%OutletNodeNum   = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(2),cAlphaArgs(3),'Chilled Water Nodes')
    EnergySource(EnergySourceNum)%NomCap          = rNumericArgs(1)
    EnergySource(EnergySourceNum)%EnergyTransfer = 0.0d0
    EnergySource(EnergySourceNum)%EnergyRate = 0.0d0
    EnergySource(EnergySourceNum)%EnergyType = EnergyType_DistrictCooling
    IF (.not. lAlphaFieldBlanks(4)) THEN
      EnergySource(EnergySourceNum)%CapFractionSchedNum =  GetScheduleIndex(cAlphaArgs(4))
      IF (EnergySource(EnergySourceNum)%CapFractionSchedNum == 0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(EnergySource(EnergySourceNum)%Name)// &
                             '", is not valid')
        CALL ShowContinueError(TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'" was not found.')
        ErrorsFound=.TRUE.
      ENDIF
      IF (.not. CheckScheduleValueMinMax(EnergySource(EnergySourceNum)%CapFractionSchedNum, '>=', 0.0d0) ) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(EnergySource(EnergySourceNum)%Name)// &
                             '", is not valid')
        CALL ShowContinueError(TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'" should not have negative values.')
        CALL ShowContinueError('Negative values will be treated as zero, and the simulation continues.')
      ENDIF
    ELSE
      EnergySource(EnergySourceNum)%CapFractionSchedNum= ScheduleAlwaysOn
    ENDIF

  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '//TRIM(cCurrentModuleObject)// &
            ', Preceding condition caused termination.')
  ENDIF

  EnergySourceNum = NumDistrictUnitsHeat !To initialize counter
  DO IndexCounter = 1 , NumDistrictUnitsCool
    EnergySourceNum = EnergySourceNum + 1
    CALL SetupOutputVariable('District Cooling Chilled Water Energy [J]',EnergySource(EnergySourceNum)%EnergyTransfer, &
                             'System','Sum',EnergySource(EnergySourceNum)%Name,  &
                              ResourceTypeKey='DistrictCooling',EndUseKey='Cooling',GroupKey='Plant')
    CALL SetupOutputVariable('District Cooling Chilled Water Rate [W]',EnergySource(EnergySourceNum)%EnergyRate, &
                             'System','Average',EnergySource(EnergySourceNum)%Name)

    CALL SetupOutputVariable('District Cooling Rate [W]', &
          EnergySource(EnergySourceNum)%EnergyRate,'System','Average',EnergySource(EnergySourceNum)%Name)
    CALL SetupOutputVariable('District Cooling Inlet Temperature [C]', &
          EnergySourceReport(EnergySourceNum)%InletTemp,'System','Average',EnergySource(EnergySourceNum)%Name)
    CALL SetupOutputVariable('District Cooling Outlet Temperature [C]', &
          EnergySourceReport(EnergySourceNum)%OutletTemp,'System','Average',EnergySource(EnergySourceNum)%Name)
    CALL SetupOutputVariable('District Cooling Mass Flow Rate [kg/s]', &
          EnergySourceReport(EnergySourceNum)%MassFlowRate,'System','Average',EnergySource(EnergySourceNum)%Name)
  END DO

RETURN
END SUBROUTINE GetOutsideEnergySourcesInput

! End of Get Input subroutines for the OutsideEnergySources Module
!******************************************************************************


! Beginning Initialization Section of the OutsideEnergySources Module
!******************************************************************************

SUBROUTINE InitSimVars(EnergySourceNum,MassFlowRate,InletTemp,OutletTemp, MyLoad)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Dan Fisher
          !       DATE WRITTEN:    October 1998
          !       MODIFIED       May 2010; Edwin Lee; Linda Lawrie (consolidation)
          !       RE-ENGINEERED  Sept 2010, Brent Griffith, plant rewrite

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine does one-time inits and sets the operating mass flow rate of this machine

          ! METHODOLOGY EMPLOYED:
          ! One time inits include validating source type (should happen in getinput?) and locating this
          !  component on the PlantLoop topology.
          ! The mass flow rate is determined based on component load, and making use of
          !  the SetComponentFlowRate routine.
          ! The mass flow rate could be an inter-connected-loop side trigger. This is not really the type of
          !  interconnect that that routine was written for, but it is the clearest example of using it.

          ! USE STATEMENTS:
  USE PlantUtilities, ONLY: SetComponentFlowRate, InitComponentNodes, RegisterPlantCompDesignFlow
  USE DataGlobals,    ONLY: BeginEnvrnFlag

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)    :: EnergySourceNum  ! Which item being initialized
  REAL(r64), INTENT(INOUT) :: MassFlowRate
  REAL(r64), INTENT(INOUT) :: InletTemp
  REAL(r64), INTENT(INOUT) :: OutletTemp
  REAL(r64), INTENT(IN)    :: MyLoad

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: TempTypeFlag
  REAL(r64)   :: TempPlantMdot = 0.d0 ! local copy of plant flow
  INTEGER     :: LoopNum
  INTEGER     :: LoopSideNum
  INTEGER     :: BranchIndex
  INTEGER     :: CompIndex
  INTEGER     :: InletNode
  INTEGER     :: OutletNode
  LOGICAL     :: errFlag
  ! Init more variables
  IF (EnergySource(EnergySourceNum)%OneTimeInitFlag) THEN
    IF (EnergySource(EnergySourceNum)%EnergyType == EnergyType_DistrictHeating) THEN
      TempTypeFlag = TypeOf_PurchHotWater
    ELSEIF (EnergySource(EnergySourceNum)%EnergyType == EnergyType_DistrictCooling) THEN
      TempTypeFlag = TypeOf_PurchChilledWater
    ELSE
      CALL ShowFatalError('InitSimVars: Invalid EnergyType for District Heating/Cooling='//  &
        trim(EnergySource(EnergySourceNum)%Name))
    ENDIF
    ! Locate the unit on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(EnergySource(EnergySourceNum)%Name, &
                                 TempTypeFlag, &
                                 EnergySource(EnergySourceNum)%LoopNum, &
                                 EnergySource(EnergySourceNum)%LoopSideNum, &
                                 EnergySource(EnergySourceNum)%BranchNum, &
                                 EnergySource(EnergySourceNum)%CompNum,  &
                                 errFlag=errFlag)
    IF (errFlag) THEN
      CALL ShowFatalError('InitSimVars: Program terminated due to previous condition(s).')
    ENDIF
    ! set limits on outlet node temps to plant loop limits
    PlantLoop(EnergySource(EnergySourceNum)%LoopNum)%LoopSide(EnergySource(EnergySourceNum)%LoopSideNum)%&
      Branch(EnergySource(EnergySourceNum)%BranchNum)%Comp(EnergySource(EnergySourceNum)%CompNum)%&
        MinOutletTemp = PlantLoop(EnergySource(EnergySourceNum)%LoopNum)%MinTemp
    PlantLoop(EnergySource(EnergySourceNum)%LoopNum)%LoopSide(EnergySource(EnergySourceNum)%LoopSideNum)%&
      Branch(EnergySource(EnergySourceNum)%BranchNum)%Comp(EnergySource(EnergySourceNum)%CompNum)%&
        MaxOutletTemp = PlantLoop(EnergySource(EnergySourceNum)%LoopNum)%MaxTemp
    ! Register design flow rate for inlet node (helps to autosize comp setpoint op scheme flows
    CALL RegisterPlantCompDesignFlow(EnergySource(EnergySourceNum)%InletNodeNum, &
                                 PlantLoop(EnergySource(EnergySourceNum)%LoopNum)%MaxVolFlowRate)

    EnergySource(EnergySourceNum)%OneTimeInitFlag=.FALSE.
  ENDIF

  !begin environment inits
  IF (BeginEnvrnFlag .AND. EnergySource(EnergySourceNum)%BeginEnvrnInitFlag) THEN
    ! component model has not design flow rates, using data for overall plant loop
    CALL InitComponentNodes( PlantLoop(EnergySource(EnergySourceNum)%LoopNum)%MinMassFlowRate, &
                             PlantLoop(EnergySource(EnergySourceNum)%LoopNum)%MaxMassFlowRate, &
                             EnergySource(EnergySourceNum)%InletNodeNum, &
                             EnergySource(EnergySourceNum)%OutletNodeNum, &
                             EnergySource(EnergySourceNum)%LoopNum, &
                             EnergySource(EnergySourceNum)%LoopSideNum, &
                             EnergySource(EnergySourceNum)%BranchNum, &
                             EnergySource(EnergySourceNum)%CompNum)
    EnergySource(EnergySourceNum)%BeginEnvrnInitFlag = .FALSE.
  ENDIF
  IF (.NOT. BeginEnvrnFlag) EnergySource(EnergySourceNum)%BeginEnvrnInitFlag = .TRUE.

  ! now do everytime inits
  InletNode   = EnergySource(EnergySourceNum)%InletNodeNum
  OutletNode  = EnergySource(EnergySourceNum)%OutletNodeNum
  InletTemp   = Node(InletNode)%Temp
  OutletTemp  = InletTemp
  LoopNum     = EnergySource(EnergySourceNum)%LoopNum
  LoopSideNum = EnergySource(EnergySourceNum)%LoopSideNum
  BranchIndex = EnergySource(EnergySourceNum)%BranchNum
  CompIndex   = EnergySource(EnergySourceNum)%CompNum

  IF (ABS(MyLoad) > 0.d0) THEN
    TempPlantMdot = PlantLoop(LoopNum)%MaxMassFlowRate
  ELSE
    TempPlantMdot = 0.d0 ! expect no flow needed
  ENDIF

  ! get actual mass flow to use, hold in MassFlowRate variable
  CALL SetComponentFlowRate(TempPlantMdot,InletNode,OutletNode,LoopNum,LoopSideNum,BranchIndex,CompIndex)
  MassFlowRate = TempPlantMdot

 RETURN

END SUBROUTINE InitSimVars

! End Initialization Section of the OutsideEnergySources Module
!******************************************************************************


! Beginning of OutsideEnergySources Module Utility Subroutines
! *****************************************************************************

SUBROUTINE  SimDistrictEnergy(RunFlag,DistrictEqNum,MyLoad,MassFlowRate, InletTemp,OutletTemp)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   July 1998
          !       MODIFIED       May 2010; Edwin Lee; Linda Lawrie (consolidation)
          !       RE-ENGINEERED  Sept 2010, Brent Griffith, plant rewrite

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY : GetSpecificHeatGlycol
  USE ScheduleManager, ONLY : GetCurrentScheduleValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL               :: RunFlag
  INTEGER               :: DistrictEqNum
  REAL(r64), INTENT(INOUT) :: MyLoad
  REAL(r64), INTENT(IN) :: MassFlowRate
  REAL(r64), INTENT(IN) :: InletTemp
  REAL(r64), INTENT(INOUT) :: OutletTemp

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: LoopNum
  INTEGER     :: LoopSideNum
  INTEGER     :: BranchIndex
  INTEGER     :: CompIndex
  INTEGER     :: InletNode
  INTEGER     :: OutletNode
  REAL(r64)   :: LoopMinTemp
  REAL(r64)   :: LoopMaxTemp
  REAL(r64)   :: Cp ! local cp at current temp
  REAL(r64)   :: CurrentCap
  REAL(r64)   :: CapFraction

          !FLOW

          !set inlet and outlet nodes
  InletNode   = EnergySource(DistrictEqNum)%InletNodeNum
  OutletNode  = EnergySource(DistrictEqNum)%OutletNodeNum
  LoopNum     = EnergySource(DistrictEqNum)%LoopNum
  LoopSideNum = EnergySource(DistrictEqNum)%LoopSideNum
  BranchIndex = EnergySource(DistrictEqNum)%BranchNum
  CompIndex   = EnergySource(DistrictEqNum)%CompNum
  LoopMinTemp = PlantLoop(loopNum)%MinTemp
  LoopMaxTemp = PlantLoop(loopNum)%MaxTemp

  Cp = GetSpecificHeatGlycol(PlantLoop(loopNum)%FluidName,InletTemp,PlantLoop(loopNum)%FluidIndex,'SimDistrictEnergy')

!  apply power limit from input
  CapFraction = GetCurrentScheduleValue( EnergySource(DistrictEqNum)%CapFractionSchedNum)
  CapFraction = MAX(0.d0, CapFraction) ! ensure non negative
  CurrentCap = EnergySource(DistrictEqNum)%NomCap * CapFraction
  IF ( ABS(MyLoad)  > CurrentCap) THEN
    MyLoad = SIGN(CurrentCap, MyLoad)
  ENDIF

  IF (EnergySource(DistrictEqNum)%EnergyType == EnergyType_DistrictCooling) THEN
    IF ( MyLoad > 0.d0 ) MyLoad = 0.d0
  ELSEIF (EnergySource(DistrictEqNum)%EnergyType == EnergyType_DistrictHeating) THEN
    IF ( MyLoad < 0.d0 ) MyLoad = 0.d0
  ENDIF

  ! determine outlet temp based on inlet temp, cp, and myload
  IF ((MassFlowRate > 0.d0) .AND. RunFlag ) THEN
    OutletTemp = (MyLoad + MassFlowRate * cp * InletTemp) / (MassFlowRate * cp)
    !apply loop limits on temperature result to keep in check
    IF (OutletTemp < LoopMinTemp) THEN
      OutletTemp = MAX(OutletTemp, LoopMinTemp)
      MyLoad =  MassFlowRate * cp * (OutletTemp - InletTemp)
    ENDIF
    IF (OutletTemp > LoopMaxTemp) THEN
      OutletTemp = MIN(OutletTemp, LoopMaxTemp)
      MyLoad =  MassFlowRate * cp * (OutletTemp - InletTemp)
    ENDIF
  ELSE
    OutletTemp = InletTemp
    MyLoad = 0.d0
  ENDIF

  RETURN
END SUBROUTINE SimDistrictEnergy

! End of OutsideEnergySources Module Utility Subroutines
! *****************************************************************************


! Beginning of Record Keeping subroutines for the OutsideEnergySources Module
! *****************************************************************************

SUBROUTINE UpdateRecords(MyLoad,EqNum,MassFlowRate,OutletTemp)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Dan Fisher
          !       DATE WRITTEN:    October 1998
          !       MODIFIED       May 2010; Edwin Lee; Linda Lawrie (consolidation)
          !       RE-ENGINEERED  Sept 2010, Brent Griffith, plant rewrite

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64),INTENT(IN)   :: MyLoad
  INTEGER, INTENT(IN)    :: EqNum
  REAL(r64), INTENT(IN) :: MassFlowRate
  REAL(r64), INTENT(IN) :: OutletTemp

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: InletNode
  INTEGER :: OutletNode

          !set inlet and outlet nodes
  InletNode                              = EnergySource(EqNum)%InletNodeNum
  OutletNode                             = EnergySource(EqNum)%OutletNodeNum
  Node(OutletNode)%Temp                  = OutletTemp
  EnergySourceReport(EqNum)%MassFlowRate = MassFlowRate
  EnergySourceReport(EqNum)%InletTemp    = Node(InletNode)%Temp
  EnergySourceReport(EqNum)%OutletTemp   = OutletTemp
  EnergySource(EqNum)%EnergyRate         = ABS(MyLoad)
  EnergySource(EqNum)%EnergyTransfer     = EnergySource(EqNum)%EnergyRate * TimeStepSys * SecInHour
  EnergySourceReport(EqNum)%EnergyTransfer = EnergySource(EqNum)%EnergyTransfer
RETURN
END SUBROUTINE UpdateRecords

! End of Record Keeping subroutines for the OutsideEnergySources Module
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

END MODULE OutsideEnergySources
