MODULE PlantHeatExchangerFluidToFluid

          ! Module containing the routines dealing with the HeatExchanger:FluidToFluid

          ! MODULE INFORMATION:
          !       AUTHOR         B. Griffith, derived from legacy code by  Sankaranarayanan K P, and S. Rees
          !       DATE WRITTEN   November 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Simulate a generic plant heat exchanger with a variety of control options

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataInterfaces
USE DataPlant
USE DataLoopNode
USE DataGlobals, ONLY: MaxNameLength
IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: CrossFlowBothUnMixed    = 1
  INTEGER, PARAMETER :: CrossFlowBothMixed      = 2
  INTEGER, PARAMETER :: CrossFlowSupplyLoopMixedDemandLoopUnMixed = 3
  INTEGER, PARAMETER :: CrossFlowSupplyLoopUnMixedDemandLoopMixed = 4
  INTEGER, PARAMETER :: CounterFlow   = 5
  INTEGER, PARAMETER :: ParallelFlow  = 6
  INTEGER, PARAMETER :: Ideal         = 7

  INTEGER, PARAMETER :: UncontrolledOn                            = 1001
  INTEGER, PARAMETER :: OperationSchemeModulated                  = 1002
  INTEGER, PARAMETER :: OperationSchemeOnOff                      = 1003
  INTEGER, PARAMETER :: HeatingSetpointModulated                  = 1004
  INTEGER, PARAMETER :: HeatingSetpointOnOff                      = 1005
  INTEGER, PARAMETER :: CoolingSetpointModulated                  = 1006
  INTEGER, PARAMETER :: CoolingSetpointOnOff                      = 1007
  INTEGER, PARAMETER :: DualDeadbandSetpointModulated             = 1008
  INTEGER, PARAMETER :: DualDeadbandSetpointOnOff                 = 1009
  INTEGER, PARAMETER :: CoolingDifferentialOnOff                  = 1010
  INTEGER, PARAMETER :: CoolingSetpointOnOffWithComponentOverride = 1011
  INTEGER, PARAMETER :: TrackComponentOnOff                       = 1012

  INTEGER, PARAMETER :: WetBulbTemperature                        = 10
  INTEGER, PARAMETER :: DryBulbTemperature                        = 11
  INTEGER, PARAMETER :: LoopTemperature                           = 12

  INTEGER, PARAMETER :: HeatingSupplySideLoop   = 501
  INTEGER, PARAMETER :: CoolingSupplySideLoop   = 502

          ! DERIVED TYPE DEFINITIONS:
  TYPE PlantConnectionStruct
    INTEGER        :: LoopNum              = 0  ! plant loop connection index
    INTEGER        :: LoopSideNum          = 0  ! plant loop side connection index
    INTEGER        :: BranchNum            = 0  ! plant loop branch connection index
    INTEGER        :: CompNum              = 0  ! plant loop component connection index
    INTEGER        :: InletNodeNum         = 0  ! plant loop inlet node index
    INTEGER        :: OutletNodeNum        = 0  ! plant loop outlet node index
    REAL(r64)      :: MassFlowRateMin      = 0.d0 ! minimum (hardware) flow rate for component [kg/s]
    REAL(r64)      :: MassFlowRateMax      = 0.d0 ! maximum (hardware) flow rate for component [kg/s]
    REAL(r64)      :: DesignVolumeFlowRate = 0.d0 ! design flow rate [m3/s]
    REAL(r64)      :: MyLoad  =0.d0   ! current load request of supply equip for op scheme control[W]
    REAL(r64)      :: MinLoad = 0.d0  ! reports back size for load dispatch routines [W]
    REAL(r64)      :: MaxLoad = 0.d0  ! reports back size for load dispatch [W]
    REAL(r64)      :: OptLoad = 0.d0  ! reports back size for load dispatch [W]
    REAL(r64)      :: InletTemp = 0.d0 ! current inlet fluid temperature [C]
    REAL(r64)      :: InletMassFlowRate = 0.d0 ! current inlet mass flow rate [kg/s]
    REAL(r64)      :: OutletTemp = 0.d0 ! componenent outlet temperature [C]
  END TYPE PlantConnectionStruct

  TYPE PlantLocatorStruct
    INTEGER        :: LoopNum              = 0  ! plant loop connection index
    INTEGER        :: LoopSideNum          = 0  ! plant loop side connection index
    INTEGER        :: BranchNum            = 0  ! plant loop branch connection index
    INTEGER        :: CompNum              = 0  ! plant loop component connection index
    INTEGER        :: InletNodeNum         = 0  ! plant loop inlet node index
  END TYPE PlantLocatorStruct

  TYPE HeatExchangerStruct
    CHARACTER(len=MaxNameLength) :: Name                  = ' '
    INTEGER                      :: AvailSchedNum         = 0
    INTEGER                      :: HeatExchangeModelType = 0
    REAL(r64)                    :: UA                    = 0.d0
    INTEGER                      :: ControlMode           = 0
    INTEGER                      :: SetpointNodeNum       = 0
    REAL(r64)                    :: TempControlTol        = 0.d0
    INTEGER                      :: ControlSignalTemp     = 0
    REAL(r64)                    :: MinOperationTemp      = -99999.d0
    REAL(r64)                    :: MaxOperationTemp      =  99999.d0
    TYPE(PlantConnectionStruct)  :: DemandSideLoop        ! plant connections and data for the side of HX connected to demand side
    TYPE(PlantConnectionStruct)  :: SupplySideLoop
    CHARACTER(len=MaxNameLength) :: HeatTransferMeteringEndUse = ' '
    CHARACTER(len=MaxNameLength) :: ComponentUserName     = ' ' ! user name for control-associated  component
    CHARACTER(len=MaxNameLength) :: ComponentClassName    = ' ' ! object class name for control-associated component
    INTEGER                      :: ComponentTypeOfNum    = 0
    TYPE(PlantLocatorStruct)     :: OtherCompSupplySideLoop
    TYPE(PlantLocatorStruct)     :: OtherCompDemandSideLoop
    REAL(r64)                    :: SizingFactor          = 1.d0
    REAL(r64)                    :: HeatTransferRate      = 0.d0
    REAL(r64)                    :: HeatTransferEnergy    = 0.d0
    REAL(r64)                    :: Effectiveness         = 0.d0
    REAL(r64)                    :: OperationStatus       = 0.d0
    INTEGER                      :: DmdSideModulatSolvNoConvergeErrorCount = 0
    INTEGER                      :: DmdSideModulatSolvNoConvergeErrorIndex = 0
    INTEGER                      :: DmdSideModulatSolvFailErrorCount = 0
    INTEGER                      :: DmdSideModulatSolvFailErrorIndex = 0
  END TYPE HeatExchangerStruct

          ! MODULE VARIABLE DECLARATIONS:
  CHARACTER(Len=26) :: ComponentClassName = 'HeatExchanger:FluidToFluid'
  INTEGER  :: NumberOfPlantFluidHXs = 0
  TYPE(HeatExchangerStruct), DIMENSION(:), ALLOCATABLE :: FluidHX
  LOGICAL  :: GetInput = .TRUE.
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckFluidHXs

          ! SUBROUTINE SPECIFICATIONS FOR MODULE
  PUBLIC SimFluidHeatExchanger
  PRIVATE GetFluidHeatExchangerInput

  PRIVATE InitFluidHeatExchanger
  PRIVATE SizeFluidHeatExchanger
  PRIVATE ControlFluidHeatExchanger
  PRIVATE FindHXDemandSideLoopFlow
  PRIVATE HXDemandSideLoopFlowResidual
  PRIVATE CalcFluidHeatExchanger
  PRIVATE UpdateFluidHeatExchanger
  PRIVATE ReportFluidHeatExchanger

CONTAINS

SUBROUTINE SimFluidHeatExchanger(LoopNum, LoopSideNum, EquipType,EquipName, &
                                 CompIndex,InitLoopEquip,MyLoad,MaxCap,MinCap,OptCap)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   November 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Main entry point and simulation manager for heat exchanger

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General,        ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: LoopNum  ! plant loop sim call originated from
  INTEGER, INTENT(IN)          :: LoopSideNum  ! plant loop side sim call originated from
  CHARACTER(len=*), INTENT(IN) :: EquipType  ! type of equipment, 'PlantComponent:UserDefined'
  CHARACTER(len=*), INTENT(IN) :: EquipName  ! user name for component
  INTEGER, INTENT(INOUT)       :: CompIndex
  LOGICAL, INTENT(INOUT)       :: InitLoopEquip
  REAL(r64), INTENT(IN)        :: MyLoad
  REAL(r64), INTENT(OUT)       :: MinCap
  REAL(r64), INTENT(OUT)       :: MaxCap
  REAL(r64), INTENT(OUT)       :: OptCap

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER       :: CompNum

  IF (GetInput) THEN
    CALL GetFluidHeatExchangerInput
    GetInput=.FALSE.
  END IF

  ! Find the correct Equipment
  IF (CompIndex == 0) THEN
    CompNum = FindItemInList(EquipName, FluidHX%Name, NumberOfPlantFluidHXs)
    IF (CompNum == 0) THEN
      CALL ShowFatalError('SimFluidHeatExchanger: HeatExchanger:FluidToFluid not found')
    ENDIF
    CompIndex = CompNum
  ELSE
    CompNum = CompIndex
    IF (CompNum < 1 .OR. CompNum > NumberOfPlantFluidHXs) THEN
      CALL ShowFatalError('SimFluidHeatExchanger: Invalid CompIndex passed='//  &
                           TRIM(TrimSigDigits(CompNum))// &
                           ', Number of heat exchangers ='//TRIM(TrimSigDigits(NumberOfPlantFluidHXs))// &
                           ', Entered heat exchanger name = '//TRIM(EquipName) )
    ENDIF
    IF(CheckFluidHXs(CompNum)) THEN
      IF (EquipName /= FluidHX(CompNum)%Name) THEN
        CALL ShowFatalError('SimFluidHeatExchanger: Invalid CompIndex passed='//  &
                           TRIM(TrimSigDigits(CompNum))// &
                           ', heat exchanger name='//TRIM(EquipName)//', stored name for that index='// &
                           TRIM(FluidHX(CompNum)%Name) )
      ENDIF
      CheckFluidHXs(CompNum) = .FALSE.
    ENDIF
  ENDIF

  IF (InitLoopEquip) THEN
    CALL InitFluidHeatExchanger(CompNum, LoopNum)
    CALL SizeFluidHeatExchanger(CompNum)
    IF (LoopNum == FluidHX(CompNum)%DemandSideLoop%LoopNum) THEN
      MinCap = 0.d0
      MaxCap = FluidHX(CompNum)%DemandSideLoop%MaxLoad
      OptCap = FluidHX(CompNum)%DemandSideLoop%MaxLoad * 0.9d0
    ELSEIF(LoopNum == FluidHX(CompNum)%SupplySideLoop%LoopNum) THEN
      MinCap = 0.d0
      MaxCap = FluidHX(CompNum)%SupplySideLoop%MaxLoad
      OptCap = FluidHX(CompNum)%SupplySideLoop%MaxLoad * 0.9d0
    ENDIF
  ENDIF

  CALL InitFluidHeatExchanger(CompNum, LoopNum)

  ! for op scheme led HXs, only call controls if called from Loop Supply Side
  IF ((FluidHX(CompNum)%ControlMode == OperationSchemeModulated) .OR. &
      (FluidHX(CompNum)%ControlMode == OperationSchemeOnOff ) ) THEN
    IF (LoopNum == FluidHX(CompNum)%SupplySideLoop%LoopNum) THEN
      CALL ControlFluidHeatExchanger(CompNum, LoopNum, MyLoad)
    ENDIF
  ELSE
    CALL ControlFluidHeatExchanger(CompNum, LoopNum, MyLoad)
  ENDIF

  CALL CalcFluidHeatExchanger(CompNum, &
                              Node(FluidHX(CompNum)%SupplySideLoop%InletNodeNum)%MassFlowRate, &
                              Node(FluidHX(CompNum)%DemandSideLoop%InletNodeNum)%MassFlowRate)

  CALL UpdateFluidHeatExchanger(CompNum)

  CALL ReportFluidHeatExchanger(CompNum)

  RETURN

END SUBROUTINE SimFluidHeatExchanger

SUBROUTINE GetFluidHeatExchangerInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   November 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! get input for heat exchanger model

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,        ONLY: GetNumObjectsFound, GetObjectDefMaxArgs, GetObjectItem, &
                                   FindItemInList, VerifyName, SameString, FindItem
  USE General,               ONLY: RoundSigDigits
  USE NodeInputManager,      ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet
  USE DataGlobals,           ONLY: ScheduleAlwaysOn, AnyEnergyManagementSystemInModel
  USE ScheduleManager,       ONLY: GetScheduleIndex
  USE EMSManager ,           ONLY: CheckIfNodeSetpointManagedByEMS, iTemperatureSetpoint, &
                                   iTemperatureMinSetpoint, iTemperatureMaxSetpoint

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetFluidHeatExchangerInput: '

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL    :: ErrorsFound = .FALSE.
  INTEGER    :: NumAlphas ! Number of elements in the alpha array
  INTEGER    :: NumNums   ! Number of elements in the numeric array
  INTEGER    :: IOStat    ! IO Status when calling get input subroutine
  LOGICAL    :: IsNotOK   ! Flag to verify name
  LOGICAL    :: IsBlank   ! Flag for blank name
  INTEGER    :: MaxNumAlphas = 0 !argument for call to GetObjectDefMaxArgs
  INTEGER    :: MaxNumNumbers = 0 !argument for call to GetObjectDefMaxArgs
  INTEGER    :: TotalArgs = 0 !argument for call to GetObjectDefMaxArgs
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cAlphaFieldNames
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cNumericFieldNames
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericFieldBlanks
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaFieldBlanks
  CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: cAlphaArgs
  REAL(r64),ALLOCATABLE, DIMENSION(:) :: rNumericArgs
  CHARACTER(len=MaxNameLength) :: cCurrentModuleObject
  INTEGER  :: CompLoop
  LOGICAL :: NodeEMSSetpointMissing

  cCurrentModuleObject = 'HeatExchanger:FluidToFluid'

  NumberOfPlantFluidHXs = GetNumObjectsFound(cCurrentModuleObject)
  IF (NumberOfPlantFluidHXs == 0) RETURN

  CALL GetObjectDefMaxArgs(cCurrentModuleObject,TotalArgs,NumAlphas,NumNums)
  MaxNumNumbers=NumNums
  MaxNumAlphas=NumAlphas

  ALLOCATE(cAlphaFieldNames(MaxNumAlphas))
  cAlphaFieldNames=' '
  ALLOCATE(cAlphaArgs(MaxNumAlphas))
  cAlphaArgs=' '
  ALLOCATE(lAlphaFieldBlanks(MaxNumAlphas))
  lAlphaFieldBlanks=.false.
  ALLOCATE(cNumericFieldNames(MaxNumNumbers))
  cNumericFieldNames=' '
  ALLOCATE(rNumericArgs(MaxNumNumbers))
  rNumericArgs=0.0d0
  ALLOCATE(lNumericFieldBlanks(MaxNumNumbers))
  lNumericFieldBlanks=.false.



  IF (NumberOfPlantFluidHXs > 0) THEN
    ALLOCATE(FluidHX(NumberOfPlantFluidHXs))
    ALLOCATE(CheckFluidHXs(NumberOfPlantFluidHXs))
    CheckFluidHXs = .TRUE.
    DO CompLoop =1, NumberOfPlantFluidHXs
      CALL GetObjectItem(cCurrentModuleObject, CompLoop, cAlphaArgs, NumAlphas, rNumericArgs, &
             NumNums, IOSTAT, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks, &
             AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(cAlphaArgs(1), FluidHX%Name, CompLoop - 1, IsNotOK, IsBlank, TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
      END IF
      FluidHX(CompLoop)%Name =  cAlphaArgs(1)

      IF (lAlphaFieldBlanks(2)) THEN
        FluidHX(CompLoop)%AvailSchedNum = ScheduleAlwaysOn
      ELSE
        FluidHX(CompLoop)%AvailSchedNum = GetScheduleIndex(cAlphaArgs(2))
        IF (FluidHX(CompLoop)%AvailSchedNum <= 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
          CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(2))//' = '//TRIM(cAlphaArgs(2)) )
          CALL ShowContinueError('Schedule was not found ')
          ErrorsFound = .TRUE.
        ENDIF
      ENDIF

      FluidHX(CompLoop)%DemandSideLoop%InletNodeNum = &
        GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Inlet, 1, ObjectIsNotParent)
      FluidHX(CompLoop)%DemandSideLoop%OutletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Outlet, 1, ObjectIsNotParent)
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4),  &
             'Loop Demand Side Plant Nodes')
      FluidHX(CompLoop)%DemandSideLoop%DesignVolumeFlowRate = rNumericArgs(1)

      FluidHX(CompLoop)%SupplySideLoop%InletNodeNum = &
        GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Inlet, 2, ObjectIsNotParent)
      FluidHX(CompLoop)%SupplySideLoop%OutletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Outlet, 2, ObjectIsNotParent)
      CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(5),cAlphaArgs(6),  &
             'Loop Supply Side Plant Nodes')
      FluidHX(CompLoop)%SupplySideLoop%DesignVolumeFlowRate = rNumericArgs(2)

      IF (SameString(cAlphaArgs(7), 'CrossFlowBothUnMixed')) THEN
        FluidHX(CompLoop)%HeatExchangeModelType = CrossFlowBothUnMixed
      ELSEIF (SameString(cAlphaArgs(7), 'CrossFlowBothMixed')) THEN
        FluidHX(CompLoop)%HeatExchangeModelType = CrossFlowBothMixed
      ELSEIF (SameString(cAlphaArgs(7), 'CrossFlowSupplyMixedDemandUnMixed')) THEN
        FluidHX(CompLoop)%HeatExchangeModelType = CrossFlowSupplyLoopMixedDemandLoopUnMixed
      ELSEIF (SameString(cAlphaArgs(7), 'CrossFlowSupplyUnMixedDemandMixed')) THEN
        FluidHX(CompLoop)%HeatExchangeModelType = CrossFlowSupplyLoopUnMixedDemandLoopMixed
      ELSEIF (SameString(cAlphaArgs(7), 'CounterFlow')) THEN
        FluidHX(CompLoop)%HeatExchangeModelType = CounterFlow
      ELSEIF (SameString(cAlphaArgs(7), 'ParallelFlow')) THEN
        FluidHX(CompLoop)%HeatExchangeModelType = ParallelFlow
      ELSEIF (SameString(cAlphaArgs(7),'Ideal')) THEN
        FluidHX(CompLoop)%HeatExchangeModelType = Ideal
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
        CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(7))//' = '//TRIM(cAlphaArgs(7)) )
        ErrorsFound = .TRUE.
      ENDIF

      IF (.NOT. lNumericFieldBlanks(3)) THEN
        FluidHX(CompLoop)%UA = rNumericArgs(3)
      ELSE
        IF (FluidHX(CompLoop)%HeatExchangeModelType /= Ideal) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
          CALL ShowContinueError('Missing entry for '//TRIM(cNumericFieldNames(3)) )
          ErrorsFound = .TRUE.
        ENDIF
      ENDIF

      IF (    SameString(cAlphaArgs(8) , 'UncontrolledOn')) THEN
        FluidHX(CompLoop)%ControlMode = UncontrolledOn
      ELSEIF (SameString(cAlphaArgs(8) , 'OperationSchemeModulated')) THEN
        FluidHX(CompLoop)%ControlMode = OperationSchemeModulated
      ELSEIF (SameString(cAlphaArgs(8) , 'OperationSchemeOnOff')) THEN
        FluidHX(CompLoop)%ControlMode = OperationSchemeOnOff
      ELSEIF (SameString(cAlphaArgs(8) , 'HeatingSetpointModulated')) THEN
        FluidHX(CompLoop)%ControlMode = HeatingSetpointModulated
      ELSEIF (SameString(cAlphaArgs(8) , 'HeatingSetpointOnOff')) THEN
        FluidHX(CompLoop)%ControlMode = HeatingSetpointOnOff
      ELSEIF (SameString(cAlphaArgs(8) , 'CoolingSetpointModulated')) THEN
        FluidHX(CompLoop)%ControlMode = CoolingSetpointModulated
      ELSEIF (SameString(cAlphaArgs(8) , 'CoolingSetpointOnOff')) THEN
        FluidHX(CompLoop)%ControlMode = CoolingSetpointOnOff
      ELSEIF (SameString(cAlphaArgs(8) , 'DualDeadbandSetpointModulated')) THEN
        FluidHX(CompLoop)%ControlMode = DualDeadbandSetpointModulated
      ELSEIF (SameString(cAlphaArgs(8) , 'DualDeadbandSetpointOnOff')) THEN
        FluidHX(CompLoop)%ControlMode = DualDeadbandSetpointOnOff
      ELSEIF (SameString(cAlphaArgs(8) , 'CoolingDifferentialOnOff')) THEN
        FluidHX(CompLoop)%ControlMode = CoolingDifferentialOnOff
      ELSEIF (SameString(cAlphaArgs(8) , 'CoolingSetpointOnOffWithComponentOverride')) THEN
        FluidHX(CompLoop)%ControlMode = CoolingSetpointOnOffWithComponentOverride
      ELSEIF (SameString(cAlphaArgs(8) , 'TrackComponentOnOff')) THEN
        FluidHX(CompLoop)%ControlMode = TrackComponentOnOff
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
        CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(8))//' = '//TRIM(cAlphaArgs(8)) )
        ErrorsFound = .TRUE.
      ENDIF

      IF (.NOT. lAlphaFieldBlanks(9)) THEN
        FluidHX(CompLoop)%SetpointNodeNum = &
               GetOnlySingleNode(cAlphaArgs(9),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Sensor, 1, ObjectIsNotParent)
        ! check that node actually has setpoints on it
        IF (    (FluidHX(CompLoop)%ControlMode == HeatingSetpointModulated) &
           .OR. (FluidHX(CompLoop)%ControlMode == HeatingSetpointOnOff)     &
           .OR. (FluidHX(CompLoop)%ControlMode == CoolingSetpointModulated) &
           .OR. (FluidHX(CompLoop)%ControlMode == CoolingSetpointOnOff)     &
           .OR. (FluidHX(CompLoop)%ControlMode == CoolingSetpointOnOffWithComponentOverride) ) THEN
          IF (Node(FluidHX(CompLoop)%SetpointNodeNum)%TempSetPoint == SensedNodeFlagValue) THEN
            IF (.NOT. AnyEnergyManagementSystemInModel) THEN
              CALL ShowSevereError(RoutineName//' Missing temperature setpoint for node = '//TRIM(cAlphaArgs(9)) )
              CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1)) )
              CALL ShowContinueError(' Use a setpoint manager to place a single temperature setpoint on the node')
              ErrorsFound=.true.
            ELSE
                ! need call to EMS to check node
              NodeEMSSetpointMissing = .FALSE.
              CALL CheckIfNodeSetpointManagedByEMS(FluidHX(CompLoop)%SetpointNodeNum,  &
                                   iTemperatureSetpoint, NodeEMSSetpointMissing)
              IF (NodeEMSSetpointMissing) THEN
                CALL ShowSevereError(RoutineName//' Missing temperature setpoint for node = '//TRIM(cAlphaArgs(9)) )
                CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1)) )
                CALL ShowContinueError( &
                     'Use a setpoint manager or EMS actuator to place a single temperature setpoint on the node')
                ErrorsFound=.true.
              ENDIF
            ENDIF
          ENDIF
        ELSEIF ((FluidHX(CompLoop)%ControlMode == DualDeadbandSetpointModulated) &
             .OR. (FluidHX(CompLoop)%ControlMode == DualDeadbandSetpointOnOff)) THEN
          IF ((Node(FluidHX(CompLoop)%SetpointNodeNum)%TempSetPointHi == SensedNodeFlagValue) &
              .OR. (Node(FluidHX(CompLoop)%SetpointNodeNum)%TempSetPointLo == SensedNodeFlagValue) ) THEN
            IF (.NOT. AnyEnergyManagementSystemInModel) THEN
              CALL ShowSevereError(RoutineName//' Missing dual temperature setpoints for node = '//TRIM(cAlphaArgs(9)) )
              CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1)) )
              CALL ShowContinueError(' Use a setpoint manager to place a dual temperature setpoint on the node')
              ErrorsFound=.true.
            ELSE
                ! need call to EMS to check node
              NodeEMSSetpointMissing = .FALSE.
              CALL CheckIfNodeSetpointManagedByEMS(FluidHX(CompLoop)%SetpointNodeNum,  &
                                   iTemperatureMinSetpoint, NodeEMSSetpointMissing)
              CALL CheckIfNodeSetpointManagedByEMS(FluidHX(CompLoop)%SetpointNodeNum,  &
                                   iTemperatureMaxSetpoint, NodeEMSSetpointMissing)
              IF (NodeEMSSetpointMissing) THEN
                CALL ShowSevereError(RoutineName//' Missing temperature setpoint for node = '//TRIM(cAlphaArgs(9)) )
                CALL ShowContinueError('Occurs for '//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1)) )
                CALL ShowContinueError( &
                     'Use a setpoint manager or EMS actuators to place a dual temperature setpoints on the node')
                ErrorsFound=.true.
              ENDIF
            ENDIF
          ENDIF
        ENDIF


      ELSE
        ! need to name a setpoint node if using a setpoint type control mode
        IF (    (FluidHX(CompLoop)%ControlMode == HeatingSetpointModulated) &
           .OR. (FluidHX(CompLoop)%ControlMode == HeatingSetpointOnOff)     &
           .OR. (FluidHX(CompLoop)%ControlMode == CoolingSetpointModulated) &
           .OR. (FluidHX(CompLoop)%ControlMode == CoolingSetpointOnOff)     &
           .OR. (FluidHX(CompLoop)%ControlMode == DualDeadbandSetpointModulated) &
           .OR. (FluidHX(CompLoop)%ControlMode == DualDeadbandSetpointOnOff) &
           .OR. (FluidHX(CompLoop)%ControlMode == CoolingSetpointOnOffWithComponentOverride) ) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
          CALL ShowContinueError('Missing entry for '//TRIM(cAlphaFieldNames(9)) )
          ErrorsFound = .TRUE.
        ENDIF
      ENDIF

      IF (.NOT. lNumericFieldBlanks(4)) THEN
        FluidHX(CompLoop)%TempControlTol = rNumericArgs(4)
      ELSE
        FluidHX(CompLoop)%TempControlTol = 0.01d0
      ENDIF

      FluidHX(CompLoop)%HeatTransferMeteringEndUse = cAlphaArgs(10)

      IF (.NOT. lAlphaFieldBlanks(11)) THEN
        FluidHX(CompLoop)%OtherCompSupplySideLoop%InletNodeNum = &
             GetOnlySingleNode(cAlphaArgs(11),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Actuator, 1, ObjectIsNotParent)
      ELSE
        IF (FluidHX(CompLoop)%ControlMode == CoolingSetpointOnOffWithComponentOverride) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
          CALL ShowContinueError('Missing entry for '//TRIM(cAlphaFieldNames(11)) )
          ErrorsFound = .TRUE.
        ENDIF
      ENDIF

      IF (.NOT. lAlphaFieldBlanks(12)) THEN
        FluidHX(CompLoop)%OtherCompDemandSideLoop%InletNodeNum = &
             GetOnlySingleNode(cAlphaArgs(12),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeType_Water, &
               NodeConnectionType_Actuator, 1, ObjectIsNotParent)
      ELSE
        IF (FluidHX(CompLoop)%ControlMode == CoolingSetpointOnOffWithComponentOverride) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
          CALL ShowContinueError('Missing entry for '//TRIM(cAlphaFieldNames(12)) )
          ErrorsFound = .TRUE.
        ENDIF
      ENDIF

      IF (.NOT. lAlphaFieldBlanks(13)) THEN
        IF (     SameString(cAlphaArgs(13) , 'WetBulbTemperature')) THEN
          FluidHX(CompLoop)%ControlSignalTemp = WetBulbTemperature
        ELSEIF ( SameString(cAlphaArgs(13) , 'DryBulbTemperature')) THEN
          FluidHX(CompLoop)%ControlSignalTemp = DryBulbTemperature
        ELSEIF ( SameString(cAlphaArgs(13) , 'Loop')) THEN
          FluidHX(CompLoop)%ControlSignalTemp = LoopTemperature
        ENDIF
      ELSE
        IF (FluidHX(CompLoop)%ControlMode == CoolingSetpointOnOffWithComponentOverride) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid entry.')
          CALL ShowContinueError('Missing entry for '//TRIM(cAlphaFieldNames(13)) )
          ErrorsFound = .TRUE.
        ENDIF

      ENDIF

      IF (.NOT. lNumericFieldBlanks(5)) THEN
        FluidHX(CompLoop)%SizingFactor = rNumericArgs(5)
      ELSE
        FluidHX(CompLoop)%SizingFactor = 1.d0
      ENDIF

      IF (.NOT. lNumericFieldBlanks(6)) THEN
        FluidHX(CompLoop)%MinOperationTemp = rNumericArgs(6)
      ELSE
        FluidHX(CompLoop)%MinOperationTemp = -9999.d0
      ENDIF

      IF (.NOT. lNumericFieldBlanks(7)) THEN
        FluidHX(CompLoop)%MaxOperationTemp = rNumericArgs(7)
      ELSE
        FluidHX(CompLoop)%MaxOperationTemp =  9999.d0
      ENDIF

    ENDDO
  ENDIF



  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Errors found in processing '//TRIM(cCurrentModuleObject)//' input.')
  ENDIF

  DO CompLoop =1, NumberOfPlantFluidHXs

    CALL SetupOutputVariable('Fluid Heat Exchanger Heat Transfer Rate [W]', &
                            FluidHX(CompLoop)%HeatTransferRate, 'System', 'Average', &
                            FluidHX(CompLoop)%Name )

    CALL SetupOutputVariable('Fluid Heat Exchanger Heat Transfer Energy [J]', &
                            FluidHX(CompLoop)%HeatTransferEnergy, 'System', 'Sum', &
                            FluidHX(CompLoop)%Name, &
                            ResourceTypeKey='ENERGYTRANSFER',EndUseKey=TRIM(FluidHX(CompLoop)%HeatTransferMeteringEndUse) &
                            ,GroupKey='Plant')

    CALL SetupOutputVariable('Fluid Heat Exchanger Loop Supply Side Mass Flow Rate [kg/s]', &
                            FluidHX(CompLoop)%SupplySideLoop%InletMassFlowRate, 'System', 'Average', &
                            FluidHX(CompLoop)%Name )
    CALL SetupOutputVariable('Fluid Heat Exchanger Loop Supply Side Inlet Temperature [C]', &
                            FluidHX(CompLoop)%SupplySideLoop%InletTemp, 'System', 'Average', &
                            FluidHX(CompLoop)%Name )
    CALL SetupOutputVariable('Fluid Heat Exchanger Loop Supply Side Outlet Temperature [C]', &
                            FluidHX(CompLoop)%SupplySideLoop%OutletTemp, 'System', 'Average', &
                            FluidHX(CompLoop)%Name )
    CALL SetupOutputVariable('Fluid Heat Exchanger Loop Demand Side Mass Flow Rate [kg/s]', &
                            FluidHX(CompLoop)%DemandSideLoop%InletMassFlowRate, 'System', 'Average', &
                            FluidHX(CompLoop)%Name )
    CALL SetupOutputVariable('Fluid Heat Exchanger Loop Demand Side Inlet Temperature [C]', &
                            FluidHX(CompLoop)%DemandSideLoop%InletTemp, 'System', 'Average', &
                            FluidHX(CompLoop)%Name )
    CALL SetupOutputVariable('Fluid Heat Exchanger Loop Demand Side Outlet Temperature [C]', &
                            FluidHX(CompLoop)%DemandSideLoop%OutletTemp, 'System', 'Average', &
                            FluidHX(CompLoop)%Name )
    CALL SetupOutputVariable('Fluid Heat Exchanger Operation Status [ ]', &
                            FluidHX(CompLoop)%OperationStatus, 'System', 'Average', &
                            FluidHX(CompLoop)%Name )
    CALL SetupOutputVariable('Fluid Heat Exchanger Effectiveness [ ]', &
                            FluidHX(CompLoop)%Effectiveness, 'System', 'Average', &
                            FluidHX(CompLoop)%Name )
  ENDDO

  RETURN

END SUBROUTINE GetFluidHeatExchangerInput

SUBROUTINE InitFluidHeatExchanger(CompNum, LoopNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   november, 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Initialize heat exchanger model

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE PlantUtilities,     ONLY: InitComponentNodes, InterConnectTwoPlantLoopSides
  USE FluidProperties,    ONLY: GetDensityGlycol, GetSpecificHeatGlycol
  USE DataGlobals,        ONLY: BeginEnvrnFlag, InitConvTemp

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: CompNum
  INTEGER, INTENT(IN) :: LoopNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE                            :: MyOneTimeFlag = .TRUE. ! one time flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag            ! environment flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFlag
  LOGICAL  :: errFlag
  CHARACTER(len=MaxNameLength):: RoutineName = 'InitFluidHeatExchanger: '
  REAL(r64)  :: rho
  INTEGER    :: LoopNum2
  INTEGER    :: LoopSideNum
  INTEGER    :: BranchNum
  INTEGER    :: LoopCompNum

  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyFlag(NumberOfPlantFluidHXs))
    ALLOCATE(MyEnvrnFlag(NumberOfPlantFluidHXs))
    MyFlag = .TRUE.
    MyEnvrnFlag = .TRUE.
    MyOneTimeFlag = .FALSE.
  END IF

  IF (MyFlag(CompNum)) THEN
    ! locate the main two connections to the plant loops
    errFlag = .FALSE.
    CALL ScanPlantLoopsForObject(FluidHX(CompNum)%Name,           &
                                 TypeOf_FluidToFluidPlantHtExchg, &
                                 FluidHX(CompNum)%DemandSideLoop%LoopNum, &
                                 FluidHX(CompNum)%DemandSideLoop%LoopSideNum, &
                                 FluidHX(CompNum)%DemandSideLoop%BranchNum, &
                                 FluidHX(CompNum)%DemandSideLoop%CompNum, &
                                 InletNodeNumber = FluidHX(CompNum)%DemandSideLoop%InletNodeNum, &
                                 errFlag=errFlag )

    IF (FluidHX(CompNum)%DemandSideLoop%LoopSideNum /= DemandSide) THEN ! throw error
      CALL ShowSevereError(TRIM(RoutineName)//' Invalid connections for '//  &
                            TRIM(ccSimPlantEquipTypes(TypeOf_FluidToFluidPlantHtExchg)) &
                            //' name = "'//TRIM(FluidHX(CompNum)%Name)//'"')
      CALL ShowContinueError('The "Loop Demand Side" connections are not on the Demand Side of a plant loop')
      errFlag = .TRUE.
    ENDIF

    CALL ScanPlantLoopsForObject(FluidHX(CompNum)%Name,           &
                                 TypeOf_FluidToFluidPlantHtExchg, &
                                 FluidHX(CompNum)%SupplySideLoop%LoopNum, &
                                 FluidHX(CompNum)%SupplySideLoop%LoopSideNum, &
                                 FluidHX(CompNum)%SupplySideLoop%BranchNum, &
                                 FluidHX(CompNum)%SupplySideLoop%CompNum, &
                                 InletNodeNumber = FluidHX(CompNum)%SupplySideLoop%InletNodeNum, &
                                 errFlag=errFlag )

    IF (FluidHX(CompNum)%SupplySideLoop%LoopSideNum /= SupplySide) THEN ! throw error
      CALL ShowSevereError(TRIM(RoutineName)//' Invalid connections for '//  &
         TRIM(ccSimPlantEquipTypes(TypeOf_FluidToFluidPlantHtExchg)) &
                            //' name = "'//TRIM(FluidHX(CompNum)%Name)//'"')
      CALL ShowContinueError('The "Loop Supply Side" connections are not on the Supply Side of a plant loop')
      errFlag = .TRUE.
    ENDIF

    ! make sure it is not the same loop on both sides.
    IF (FluidHX(CompNum)%SupplySideLoop%LoopNum == FluidHX(CompNum)%DemandSideLoop%LoopNum) THEN ! user is being too tricky, don't allow
      CALL ShowSevereError(TRIM(RoutineName)//' Invalid connections for '//  &
         TRIM(ccSimPlantEquipTypes(TypeOf_FluidToFluidPlantHtExchg)) &
                            //' name = "'//TRIM(FluidHX(CompNum)%Name)//'"')
      CALL ShowContinueError('The "Loop Supply Side" and "Loop Demand Side" need to be on different loops.')
      errFlag = .TRUE.
    ELSE

      CALL InterConnectTwoPlantLoopSides(FluidHX(CompNum)%SupplySideLoop%LoopNum, &
                                       FluidHX(CompNum)%SupplySideLoop%LoopSideNum, &
                                       FluidHX(CompNum)%DemandSideLoop%LoopNum, &
                                       FluidHX(CompNum)%DemandSideLoop%LoopSideNum, &
                                       TypeOf_FluidToFluidPlantHtExchg, &
                                       .TRUE. )
    ENDIF

    !find remote component if control mode is of that type.
    IF  (FluidHX(CompNum)%ControlMode == CoolingSetpointOnOffWithComponentOverride)  THEN

      CALL ScanPlantLoopsForNodeNum( RoutineName, &
                                     FluidHX(CompNum)%OtherCompSupplySideLoop%InletNodeNum, &
                                     FluidHX(CompNum)%OtherCompSupplySideLoop%LoopNum, &
                                     FluidHX(CompNum)%OtherCompSupplySideLoop%LoopSideNum, &
                                     FluidHX(CompNum)%OtherCompSupplySideLoop%BranchNum, &
                                     CompNum = FluidHX(CompNum)%OtherCompSupplySideLoop%CompNum)

      CALL ScanPlantLoopsForNodeNum( RoutineName, &
                                     FluidHX(CompNum)%OtherCompDemandSideLoop%InletNodeNum, &
                                     FluidHX(CompNum)%OtherCompDemandSideLoop%LoopNum, &
                                     FluidHX(CompNum)%OtherCompDemandSideLoop%LoopSideNum, &
                                     FluidHX(CompNum)%OtherCompDemandSideLoop%BranchNum, &
                                     CompNum = FluidHX(CompNum)%OtherCompDemandSideLoop%CompNum)

      ! revise how loads served category for other controlled equipment
      LoopNum2     = FluidHX(CompNum)%OtherCompSupplySideLoop%LoopNum
      LoopSideNum = FluidHX(CompNum)%OtherCompSupplySideLoop%LoopSideNum
      BranchNum   = FluidHX(CompNum)%OtherCompSupplySideLoop%BranchNum
      LoopCompNum = FluidHX(CompNum)%OtherCompSupplySideLoop%CompNum

      SELECT CASE (PlantLoop(LoopNum2)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(LoopCompNum)%HowLoadServed)

      CASE (HowMet_ByNominalCap)
        PlantLoop(LoopNum2)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(LoopCompNum)%HowLoadServed &
                = HowMet_ByNominalCapFreeCoolCntrl
      CASE (HowMet_ByNominalCapLowOutLimit)
        PlantLoop(LoopNum2)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(LoopCompNum)%HowLoadServed &
                = HowMet_ByNominalCapLowOutLimitFreeCoolCntrl
      END SELECT

      SELECT CASE(FluidHX(CompNum)%ControlSignalTemp)
      CASE(WetBulbTemperature)
        PlantLoop(LoopNum2)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(LoopCompNum)%FreeCoolCntrlMode &
                = FreeCoolControlMode_WetBulb
      CASE(DryBulbTemperature)
        PlantLoop(LoopNum2)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(LoopCompNum)%FreeCoolCntrlMode &
                = FreeCoolControlMode_DryBulb
      CASE(LoopTemperature)
        PlantLoop(LoopNum2)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(LoopCompNum)%FreeCoolCntrlMode &
                = FreeCoolControlMode_Loop
        PlantLoop(LoopNum2)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(LoopCompNum)%FreeCoolCntrlNodeNum &
                = FluidHX(CompNum)%OtherCompDemandSideLoop%InletNodeNum
      END SELECT

    ENDIF
    IF  (FluidHX(CompNum)%ControlMode == TrackComponentOnOff) THEN
      IF (FluidHX(CompNum)%OtherCompSupplySideLoop%InletNodeNum > 0) THEN
        CALL ScanPlantLoopsForObject( FluidHX(CompNum)%ComponentUserName,                   &
                                      FluidHX(CompNum)%ComponentTypeOfNum,                  &
                                      FluidHX(CompNum)%OtherCompSupplySideLoop%LoopNum,     &
                                      FluidHX(CompNum)%OtherCompSupplySideLoop%LoopSideNum, &
                                      FluidHX(CompNum)%OtherCompSupplySideLoop%BranchNum,   &
                                      FluidHX(CompNum)%OtherCompSupplySideLoop%CompNum,     &
                                      InletNodeNumber = FluidHX(CompNum)%OtherCompSupplySideLoop%InletNodeNum, &
                                      errFlag=errFlag )
      ENDIF
      IF (FluidHX(CompNum)%OtherCompDemandSideLoop%InletNodeNum > 0) THEN
        CALL ScanPlantLoopsForObject( FluidHX(CompNum)%ComponentUserName,                   &
                                      FluidHX(CompNum)%ComponentTypeOfNum,                  &
                                      FluidHX(CompNum)%OtherCompDemandSideLoop%LoopNum,     &
                                      FluidHX(CompNum)%OtherCompDemandSideLoop%LoopSideNum, &
                                      FluidHX(CompNum)%OtherCompDemandSideLoop%BranchNum,   &
                                      FluidHX(CompNum)%OtherCompDemandSideLoop%CompNum,     &
                                      InletNodeNumber = FluidHX(CompNum)%OtherCompDemandSideLoop%InletNodeNum, &
                                      errFlag=errFlag )
      ENDIF
    ENDIF

    IF (errFlag) THEN
      CALL ShowFatalError(TRIM(RoutineName)//'Program terminated due to previous condition(s).')
    ENDIF
    MyFlag(CompNum) = .FALSE.
  ENDIF ! plant setup

  IF (BeginEnvrnFlag .AND. MyEnvrnFlag(CompNum) .AND. (PlantSizesOkayToFinalize)) THEN
    IF (PlantSizeNotComplete)  CALL SizeFluidHeatExchanger(CompNum)
    rho = GetDensityGlycol(PlantLoop(FluidHX(CompNum)%DemandSideLoop%LoopNum)%FluidName, &
                           InitConvTemp, &
                           PlantLoop(FluidHX(CompNum)%DemandSideLoop%LoopNum)%FluidIndex, &
                           'InitFluidHeatExchanger')
    FluidHX(CompNum)%DemandSideLoop%MassFlowRateMax = rho *  &
                        FluidHX(CompNum)%DemandSideLoop%DesignVolumeFlowRate
    CALL InitComponentNodes(FluidHX(CompNum)%DemandSideLoop%MassFlowRateMin,  &
                            FluidHX(CompNum)%DemandSideLoop%MassFlowRateMax,  &
                            FluidHX(CompNum)%DemandSideLoop%InletNodeNum,     &
                            FluidHX(CompNum)%DemandSideLoop%OutletNodeNum,    &
                            FluidHX(CompNum)%DemandSideLoop%LoopNum,          &
                            FluidHX(CompNum)%DemandSideLoop%LoopSideNum,      &
                            FluidHX(CompNum)%DemandSideLoop%BranchNum,        &
                            FluidHX(CompNum)%DemandSideLoop%CompNum)

    rho = GetDensityGlycol(PlantLoop(FluidHX(CompNum)%SupplySideLoop%LoopNum)%FluidName, &
                           InitConvTemp, &
                           PlantLoop(FluidHX(CompNum)%SupplySideLoop%LoopNum)%FluidIndex, &
                           'InitFluidHeatExchanger')
    FluidHX(CompNum)%SupplySideLoop%MassFlowRateMax = rho *  &
                        FluidHX(CompNum)%SupplySideLoop%DesignVolumeFlowRate
    CALL InitComponentNodes(FluidHX(CompNum)%SupplySideLoop%MassFlowRateMin,  &
                            FluidHX(CompNum)%SupplySideLoop%MassFlowRateMax,  &
                            FluidHX(CompNum)%SupplySideLoop%InletNodeNum,     &
                            FluidHX(CompNum)%SupplySideLoop%OutletNodeNum,    &
                            FluidHX(CompNum)%SupplySideLoop%LoopNum,          &
                            FluidHX(CompNum)%SupplySideLoop%LoopSideNum,      &
                            FluidHX(CompNum)%SupplySideLoop%BranchNum,        &
                            FluidHX(CompNum)%SupplySideLoop%CompNum)
    MyEnvrnFlag(CompNum) = .FALSE.
  ENDIF
  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(CompNum) = .TRUE.
  ENDIF

  FluidHX(CompNum)%DemandSideLoop%InletTemp = Node(FluidHX(CompNum)%DemandSideLoop%InletNodeNum)%Temp
  FluidHX(CompNum)%SupplySideLoop%InletTemp = Node(FluidHX(CompNum)%SupplySideLoop%InletNodeNum)%Temp

  IF  (FluidHX(CompNum)%ControlMode == CoolingSetpointOnOffWithComponentOverride)  THEN
    ! store current value for setpoint in central plant loop data structure
    LoopNum2     = FluidHX(CompNum)%OtherCompSupplySideLoop%LoopNum
    LoopSideNum = FluidHX(CompNum)%OtherCompSupplySideLoop%LoopSideNum
    BranchNum   = FluidHX(CompNum)%OtherCompSupplySideLoop%BranchNum
    LoopCompNum = FluidHX(CompNum)%OtherCompSupplySideLoop%CompNum

    PlantLoop(LoopNum2)%LoopSide(LoopSideNum)%Branch(BranchNum)%Comp(LoopCompNum)%FreeCoolCntrlMinCntrlTemp &
                = Node(FluidHX(CompNum)%SetpointNodeNum)%TempSetPoint
  ENDIF

  RETURN

END SUBROUTINE InitFluidHeatExchanger

SUBROUTINE SizeFluidHeatExchanger(CompNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   December 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Size plant heat exchanger flow rates, UA, and max capacity

          ! METHODOLOGY EMPLOYED:
          ! the supply side flow rate is obtained from the plant sizing structure
          ! the demand side is sized to match the supply side
          ! the UA is sized for an effectiveness of 1.0 using sizing temps
          ! the capacity uses the full HX model

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataHVACGlobals,        ONLY: SmallWaterVolFlow
  USE DataGlobals,            ONLY: InitConvTemp
  USE FluidProperties,        ONLY: GetDensityGlycol, GetSpecificHeatGlycol
  USE OutputReportPredefined, ONLY: pdchMechType, pdchMechNomCap, PreDefTableEntry
  USE PlantUtilities,         ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager,    ONLY: ReportSizingOutput

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: CompNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER    :: PltSizNumSupSide     ! Plant Sizing index for Loop Supply Side
  INTEGER    :: PltSizNumDmdSide     ! plant sizing index for Loop Demand Side
  REAL(r64)  :: tmpSupSideDesignVolFlowRate
  REAL(r64)  :: tmpDmdSideDesignVolFlowRate
  REAL(r64)  :: tmpUA
  REAL(r64)  :: tmpDeltaTSupLoop
  REAL(r64)  :: tmpDeltaTloopToLoop
  LOGICAL    :: ErrorsFound
  REAL(r64)  :: Cp
  REAL(r64)  :: rho
  REAL(r64)  :: tmpDesCap
  REAL(r64)  :: SupSideMdot
  REAL(r64)  :: DmdSideMdot

  ! first deal with Loop Supply Side
  ErrorsFound = .FALSE.
  PltSizNumSupSide = PlantLoop(FluidHX(CompNum)%SupplySideLoop%LoopNum)%PlantSizNum
  PltSizNumDmdSide = PlantLoop(FluidHX(CompNum)%DemandSideLoop%LoopNum)%PlantSizNum
  tmpSupSideDesignVolFlowRate = FluidHX(CompNum)%SupplySideLoop%DesignVolumeFlowRate
  IF (FluidHX(CompNum)%SupplySideLoop%DesignVolumeFlowRate == AutoSize) THEN
    IF (PltSizNumSupSide > 0) THEN
      IF (PlantSizData(PltSizNumSupSide)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        tmpSupSideDesignVolFlowRate  = PlantSizData(PltSizNumSupSide)%DesVolFlowRate * FluidHX(CompNum)%SizingFactor
        IF (PlantSizesOkayToFinalize) FluidHX(CompNum)%SupplySideLoop%DesignVolumeFlowRate = tmpSupSideDesignVolFlowRate
      ELSE
        tmpSupSideDesignVolFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) FluidHX(CompNum)%SupplySideLoop%DesignVolumeFlowRate = tmpSupSideDesignVolFlowRate
      ENDIF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('HeatExchanger:FluidToFluid', FluidHX(CompNum)%Name, &
                                           'Loop Supply Side Design Fluid Flow Rate [m3/s]', &
                                           FluidHX(CompNum)%SupplySideLoop%DesignVolumeFlowRate )
    ELSE
      CALL ShowSevereError('SizeFluidHeatExchanger: Autosizing of requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in heat exchanger object='//TRIM(FluidHX(CompNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  ENDIF
  CALL RegisterPlantCompDesignFlow(FluidHX(CompNum)%SupplySideLoop%InletNodeNum, tmpSupSideDesignVolFlowRate)

  ! second deal with Loop Demand Side
  tmpDmdSideDesignVolFlowRate = FluidHX(CompNum)%DemandSideLoop%DesignVolumeFlowRate
  IF (FluidHX(CompNum)%DemandSideLoop%DesignVolumeFlowRate == AutoSize) THEN
    IF (tmpSupSideDesignVolFlowRate > SmallWaterVolFlow) THEN
      tmpDmdSideDesignVolFlowRate = tmpSupSideDesignVolFlowRate
      IF (PlantSizesOkayToFinalize) FluidHX(CompNum)%DemandSideLoop%DesignVolumeFlowRate = tmpDmdSideDesignVolFlowRate
    ELSE
      tmpDmdSideDesignVolFlowRate = 0.d0
      IF (PlantSizesOkayToFinalize) FluidHX(CompNum)%DemandSideLoop%DesignVolumeFlowRate = tmpDmdSideDesignVolFlowRate
    ENDIF
    IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('HeatExchanger:FluidToFluid', FluidHX(CompNum)%Name, &
                                           'Loop Demand Side Design Fluid Flow Rate [m3/s]', &
                                           FluidHX(CompNum)%DemandSideLoop%DesignVolumeFlowRate )
  ENDIF
  CALL RegisterPlantCompDesignFlow(FluidHX(CompNum)%DemandSideLoop%InletNodeNum, tmpDmdSideDesignVolFlowRate)

  ! size UA if needed
  tmpUA = FluidHX(CompNum)%UA
  IF (FluidHX(CompNum)%UA == AutoSize) THEN
    ! get nominal delta T between two loops
    IF (PltSizNumSupSide > 0 .AND. PltSizNumDmdSide > 0) THEN

      SELECT CASE (PlantSizData(PltSizNumSupSide)%LoopType)

      CASE (HeatingLoop)
        tmpDeltaTloopToLoop = ABS( (PlantSizData(PltSizNumSupSide)%ExitTemp - PlantSizData(PltSizNumSupSide)%DeltaT ) &
                                          - PlantSizData(PltSizNumDmdSide)%ExitTemp )
      CASE (CoolingLoop)
        tmpDeltaTloopToLoop = ABS( (PlantSizData(PltSizNumSupSide)%ExitTemp + PlantSizData(PltSizNumSupSide)%DeltaT ) &
                                          - PlantSizData(PltSizNumDmdSide)%ExitTemp )
      CASE (CondenserLoop)
        tmpDeltaTloopToLoop = ABS( (PlantSizData(PltSizNumSupSide)%ExitTemp + PlantSizData(PltSizNumSupSide)%DeltaT ) &
                                          - PlantSizData(PltSizNumDmdSide)%ExitTemp )
      CASE (SteamLoop)
        tmpDeltaTloopToLoop = ABS( (PlantSizData(PltSizNumSupSide)%ExitTemp - PlantSizData(PltSizNumSupSide)%DeltaT ) &
                                          - PlantSizData(PltSizNumDmdSide)%ExitTemp )
      END SELECT

      tmpDeltaTloopToLoop = MAX(2.d0, tmpDeltaTloopToLoop)
      tmpDeltaTSupLoop    = PlantSizData(PltSizNumSupSide)%DeltaT
      IF (tmpSupSideDesignVolFlowRate >= SmallWaterVolFlow) THEN

        Cp = GetSpecificHeatGlycol(PlantLoop(FluidHX(CompNum)%SupplySideLoop%LoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(FluidHX(CompNum)%SupplySideLoop%LoopNum)%FluidIndex, &
                                   'SizeFluidHeatExchanger' )

        rho = GetDensityGlycol(PlantLoop(FluidHX(CompNum)%SupplySideLoop%LoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(FluidHX(CompNum)%SupplySideLoop%LoopNum)%FluidIndex, &
                                   'SizeFluidHeatExchanger' )

        tmpDesCap = Cp * rho * tmpDeltaTSupLoop * tmpSupSideDesignVolFlowRate
        tmpUA = tmpDesCap/tmpDeltaTloopToLoop
        IF (PlantSizesOkayToFinalize) FluidHX(CompNum)%UA = tmpUA
      ELSE
        tmpUA = 0.d0
        IF (PlantSizesOkayToFinalize) FluidHX(CompNum)%UA = tmpUA
      END IF
      IF (PlantSizesOkayToFinalize) THEN
        CALL ReportSizingOutput('HeatExchanger:FluidToFluid', FluidHX(CompNum)%Name, &
                              'Heat Exchanger U-Factor Times Area Value [W/C]', FluidHX(CompNum)%UA)
        CALL ReportSizingOutput('HeatExchanger:FluidToFluid', FluidHX(CompNum)%Name, &
                              'Loop-to-loop Temperature Difference Used to Size Heat Exchanger U-Factor Times Area Value [C]' &
                              ,tmpDeltaTloopToLoop)
      ENDIF
    ELSE
      CALL ShowSevereError('SizeFluidHeatExchanger: Autosizing of heat '//  &
         'Exchanger UA requires a loop Sizing:Plant objects for both loops')
      CALL ShowContinueError('Occurs in heat exchanger object='//TRIM(FluidHX(CompNum)%Name))
      ErrorsFound = .TRUE.
    END IF

  ENDIF

  ! size capacities for load range based op schemes
  IF (PlantSizesOkayToFinalize  ) THEN

    IF (PltSizNumSupSide > 0 ) THEN
      SELECT CASE (PlantSizData(PltSizNumSupSide)%LoopType)
      CASE (HeatingLoop)
        Node(FluidHX(CompNum)%SupplySideLoop%InletNodeNum)%Temp = &
                          (PlantSizData(PltSizNumSupSide)%ExitTemp - PlantSizData(PltSizNumSupSide)%DeltaT )
      CASE (CoolingLoop)
        Node(FluidHX(CompNum)%SupplySideLoop%InletNodeNum)%Temp = &
                         (PlantSizData(PltSizNumSupSide)%ExitTemp + PlantSizData(PltSizNumSupSide)%DeltaT )
      CASE (CondenserLoop)
        Node(FluidHX(CompNum)%SupplySideLoop%InletNodeNum)%Temp = &
                         (PlantSizData(PltSizNumSupSide)%ExitTemp + PlantSizData(PltSizNumSupSide)%DeltaT )
      CASE (SteamLoop)
        Node(FluidHX(CompNum)%SupplySideLoop%InletNodeNum)%Temp = &
                         (PlantSizData(PltSizNumSupSide)%ExitTemp - PlantSizData(PltSizNumSupSide)%DeltaT )
      END SELECT

    ELSE ! don't rely on sizing, use loop setpoints
      ! loop supply side
      IF (PlantLoop(FluidHX(CompNum)%SupplySideLoop%LoopNum)%LoopDemandCalcScheme == SingleSetPoint ) THEN
        Node(FluidHX(CompNum)%SupplySideLoop%InletNodeNum)%Temp = &
                      Node(PlantLoop(FluidHX(CompNum)%SupplySideLoop%LoopNum)%TempSetPointNodeNum)%TempSetPoint
      ELSEIF (PlantLoop(FluidHX(CompNum)%SupplySideLoop%LoopNum)%LoopDemandCalcScheme == DualSetPointDeadBand) THEN
        Node(FluidHX(CompNum)%SupplySideLoop%InletNodeNum)%Temp = &
              (Node(PlantLoop(FluidHX(CompNum)%SupplySideLoop%LoopNum)%TempSetPointNodeNum)%TempSetPointHi + &
               Node(PlantLoop(FluidHX(CompNum)%SupplySideLoop%LoopNum)%TempSetPointNodeNum)%TempSetPointLo) / 2.d0
      ENDIF

    ENDIF

    IF (PltSizNumDmdSide > 0) THEN
      Node(FluidHX(CompNum)%DemandSideLoop%InletNodeNum)%Temp = PlantSizData(PltSizNumDmdSide)%ExitTemp
    ELSE ! don't rely on sizing, use loop setpoints
      ! loop demand side
      IF (PlantLoop(FluidHX(CompNum)%DemandSideLoop%LoopNum)%LoopDemandCalcScheme == SingleSetPoint ) THEN
        Node(FluidHX(CompNum)%DemandSideLoop%InletNodeNum)%Temp = &
                      Node(PlantLoop(FluidHX(CompNum)%DemandSideLoop%LoopNum)%TempSetPointNodeNum)%TempSetPoint
      ELSEIF (PlantLoop(FluidHX(CompNum)%DemandSideLoop%LoopNum)%LoopDemandCalcScheme == DualSetPointDeadBand) THEN
        Node(FluidHX(CompNum)%DemandSideLoop%InletNodeNum)%Temp = &
              (Node(PlantLoop(FluidHX(CompNum)%DemandSideLoop%LoopNum)%TempSetPointNodeNum)%TempSetPointHi + &
               Node(PlantLoop(FluidHX(CompNum)%DemandSideLoop%LoopNum)%TempSetPointNodeNum)%TempSetPointLo) / 2.d0
      ENDIF
    ENDIF

    rho = GetDensityGlycol(PlantLoop(FluidHX(CompNum)%SupplySideLoop%LoopNum)%FluidName, &
                           InitConvTemp, &
                           PlantLoop(FluidHX(CompNum)%SupplySideLoop%LoopNum)%FluidIndex, &
                           'SizeFluidHeatExchanger')
    SupSideMdot = FluidHX(CompNum)%SupplySideLoop%DesignVolumeFlowRate * rho
    rho = GetDensityGlycol(PlantLoop(FluidHX(CompNum)%DemandSideLoop%LoopNum)%FluidName, &
                           InitConvTemp, &
                           PlantLoop(FluidHX(CompNum)%DemandSideLoop%LoopNum)%FluidIndex, &
                           'SizeFluidHeatExchanger')
    DmdSideMdot = FluidHX(CompNum)%DemandSideLoop%DesignVolumeFlowRate * rho

    CALL CalcFluidHeatExchanger(CompNum, SupSideMdot, DmdSideMdot)
    FluidHX(CompNum)%SupplySideLoop%MaxLoad = ABS( FluidHX(CompNum)%HeatTransferRate )

  ENDIF
  IF (PlantSizesOkayToFinalize) THEN
    CALL PreDefTableEntry(pdchMechType,FluidHX(CompNum)%Name,'HeatExchanger:FluidToFluid')
    CALL PreDefTableEntry(pdchMechNomCap,FluidHX(CompNum)%Name,FluidHX(CompNum)%SupplySideLoop%MaxLoad)
  ENDIF

  RETURN

END SUBROUTINE SizeFluidHeatExchanger

SUBROUTINE ControlFluidHeatExchanger(CompNum, LoopNum, MyLoad)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   November 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! determine control state for fluid to fluid heat exchanger
          ! make fluid flow requests accordingly

          ! METHODOLOGY EMPLOYED:
          ! long CASE statement for different control options

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance
  USE ScheduleManager,        ONLY: GetCurrentScheduleValue
  USE DataHVACGlobals,        ONLY: SmallLoad
  USE FluidProperties,        ONLY: GetSpecificHeatGlycol
  USE PlantUtilities,         ONLY: SetComponentFlowRate
  USE DataEnvironment,        ONLY: OutDryBulbTemp, OutWetBulbTemp
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN) :: CompNum
  INTEGER,   INTENT(IN) :: LoopNum
  REAL(r64), INTENT(IN) :: MyLoad

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: AvailSchedValue
  LOGICAL    :: ScheduledOff
  LOGICAL    :: LimitTrippedOff
  REAL(r64)  :: mdotSupSide
  REAL(r64)  :: mdotDmdSide
  REAL(r64)  :: DeltaTCooling
  REAL(r64)  :: DeltaTHeating
  REAL(r64)  :: DeltaTCoolSetpointDemand
  REAL(r64)  :: DeltaTCoolSetpointSupply
  REAL(r64)  :: DeltaTHeatSetpointDemand
  REAL(r64)  :: DeltaTHeatSetpointSupply
  REAL(r64)  :: cp ! specific heat of fluid
  REAL(r64)  :: TargetLeavingTemp ! target temperature deg. C
  REAL(r64)  :: SetpointTemp  ! temperature setpoint for single setpoint
  REAL(r64)  :: SetpointTempLo ! low setpoint for dual deadband temperature setpoints
  REAL(r64)  :: SetpointTempHi ! High setpoint for dual deadband temperature setpoints
  REAL(r64)  :: ControlSignalValue
  LOGICAL    :: ChillerShutDown

  ! check if available by schedule
  AvailSchedValue = GetCurrentScheduleValue(FluidHX(CompNum)%AvailSchedNum)
  IF (AvailSchedValue <= 0) THEN
    ScheduledOff = .TRUE.
  ELSE
    ScheduledOff = .FALSE.
  ENDIF

  ! check if operational limits trip off unit
  LimitTrippedOff = .FALSE.
  IF ((Node(FluidHX(CompNum)%SupplySideLoop%InletNodeNum)%Temp < FluidHX(CompNum)%MinOperationTemp) &
       .OR.  ( Node(FluidHX(CompNum)%DemandSideLoop%InletNodeNum)%Temp <  FluidHX(CompNum)%MinOperationTemp)) THEN
    LimitTrippedOff = .TRUE.
  ENDIF
  IF ((Node(FluidHX(CompNum)%SupplySideLoop%InletNodeNum)%Temp > FluidHX(CompNum)%MaxOperationTemp) &
       .OR.  ( Node(FluidHX(CompNum)%DemandSideLoop%InletNodeNum)%Temp >  FluidHX(CompNum)%MaxOperationTemp)) THEN
    LimitTrippedOff = .TRUE.
  ENDIF

  IF (.NOT. ScheduledOff .AND. .NOT. LimitTrippedOff) THEN

    SELECT CASE (FluidHX(CompNum)%ControlMode)

    CASE (UncontrolledOn)

    ! make passive request for supply side loop flow
      mdotSupSide = FluidHX(CompNum)%SupplySideLoop%MassFlowRateMax
      CALL SetComponentFlowRate(mdotSupSide,                           &
                        FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                        FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                        FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                        FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                        FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                        FluidHX(CompNum)%SupplySideLoop%CompNum)
      IF (mdotSupSide > MassFlowTolerance) THEN
          ! if supply side loop has massflow, request demand side flow
        mdotDmdSide = FluidHX(CompNum)%DemandSideLoop%MassFlowRateMax
      ELSE
        mdotDmdSide = 0.d0
      ENDIF
      CALL SetComponentFlowRate(mdotDmdSide,                           &
                        FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                        FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                        FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                        FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                        FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                        FluidHX(CompNum)%DemandSideLoop%CompNum)

    CASE (OperationSchemeModulated)

      IF (ABS(MyLoad) > SmallLoad) THEN
        IF (MyLoad < -1.d0*SmallLoad) THEN ! requesting cooling
          DeltaTCooling = FluidHX(CompNum)%SupplySideLoop%InletTemp - FluidHX(CompNum)%DemandSideLoop%InletTemp
          IF ( DeltaTCooling > FluidHX(CompNum)%TempControlTol ) THEN ! can do cooling so turn on
            mdotSupSide = FluidHX(CompNum)%SupplySideLoop%MassFlowRateMax
            CALL SetComponentFlowRate(mdotSupSide,                      &
                         FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                         FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                         FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                         FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                         FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                         FluidHX(CompNum)%SupplySideLoop%CompNum)
            IF (mdotSupSide > MassFlowTolerance) THEN
              ! if supply side loop has massflow, request demand side flow
              cp = GetSpecificHeatGlycol(PlantLoop(FluidHX(CompNum)%SupplySideLoop%LoopNum)%FluidName, &
                                     FluidHX(CompNum)%SupplySideLoop%InletTemp,                        &
                                     PlantLoop(FluidHX(CompNum)%SupplySideLoop%LoopNum)%FluidIndex,    &
                                     'ControlFluidHeatExchanger' )
              TargetLeavingTemp = FluidHX(CompNum)%SupplySideLoop%InletTemp - ABS(MyLoad)/(cp * mdotSupSide)

              CALL FindHXDemandSideLoopFlow(CompNum, TargetLeavingTemp, CoolingSupplySideLoop)
            ELSE ! no flow on supply side so do not request flow on demand side
              mdotDmdSide = 0.d0
              CALL SetComponentFlowRate(mdotDmdSide,                          &
                               FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                               FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                               FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                               FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                               FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                               FluidHX(CompNum)%SupplySideLoop%CompNum)
            ENDIF
          ELSE ! not able to cool so turn off
            mdotSupSide = 0.d0
            CALL SetComponentFlowRate(mdotSupSide,                          &
                             FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                             FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                             FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                             FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                             FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                             FluidHX(CompNum)%SupplySideLoop%CompNum)
            mdotDmdSide = 0.d0
            CALL SetComponentFlowRate(mdotDmdSide,                          &
                             FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                             FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                             FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                             FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                             FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                             FluidHX(CompNum)%SupplySideLoop%CompNum)
          ENDIF

        ELSE ! requesting heating
          DeltaTHeating = FluidHX(CompNum)%DemandSideLoop%InletTemp - FluidHX(CompNum)%SupplySideLoop%InletTemp
          IF ( DeltaTHeating > FluidHX(CompNum)%TempControlTol ) THEN ! can do heating so turn on
            mdotSupSide = FluidHX(CompNum)%SupplySideLoop%MassFlowRateMax
            CALL SetComponentFlowRate(mdotSupSide,                      &
                         FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                         FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                         FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                         FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                         FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                         FluidHX(CompNum)%SupplySideLoop%CompNum)
            IF (mdotSupSide > MassFlowTolerance) THEN
              cp = GetSpecificHeatGlycol(PlantLoop(FluidHX(CompNum)%SupplySideLoop%LoopNum)%FluidName, &
                                     FluidHX(CompNum)%SupplySideLoop%InletTemp, &
                                     PlantLoop(FluidHX(CompNum)%SupplySideLoop%LoopNum)%FluidIndex, &
                                     'ControlFluidHeatExchanger' )
              TargetLeavingTemp = FluidHX(CompNum)%SupplySideLoop%InletTemp + ABS(MyLoad)/(cp * mdotSupSide)

              CALL FindHXDemandSideLoopFlow(CompNum, TargetLeavingTemp, HeatingSupplySideLoop)
            ELSE ! no flow on supply side so do not request flow on demand side
              mdotDmdSide = 0.d0
              CALL SetComponentFlowRate(mdotDmdSide,                          &
                               FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                               FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                               FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                               FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                               FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                               FluidHX(CompNum)%DemandSideLoop%CompNum)
            ENDIF
          ELSE ! not able to heat so turn off
            mdotSupSide = 0.d0
            CALL SetComponentFlowRate(mdotSupSide,                          &
                             FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                             FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                             FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                             FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                             FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                             FluidHX(CompNum)%SupplySideLoop%CompNum)
            mdotDmdSide = 0.d0
            CALL SetComponentFlowRate(mdotDmdSide,                          &
                             FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                             FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                             FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                             FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                             FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                             FluidHX(CompNum)%DemandSideLoop%CompNum)
          ENDIF
        ENDIF

      ELSE !  no load
        mdotSupSide = 0.d0
        CALL SetComponentFlowRate(mdotSupSide,                          &
                         FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                         FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                         FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                         FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                         FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                         FluidHX(CompNum)%SupplySideLoop%CompNum)
        mdotDmdSide = 0.d0
        CALL SetComponentFlowRate(mdotDmdSide,                          &
                         FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                         FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                         FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                         FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                         FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                         FluidHX(CompNum)%DemandSideLoop%CompNum)
      ENDIF

    CASE (OperationSchemeOnOff)
      IF (ABS(MyLoad) > SmallLoad)  THEN
        IF (MyLoad < SmallLoad) THEN ! requesting cooling
          DeltaTCooling = FluidHX(CompNum)%SupplySideLoop%InletTemp - FluidHX(CompNum)%DemandSideLoop%InletTemp
          IF ( DeltaTCooling > FluidHX(CompNum)%TempControlTol ) THEN ! can do cooling so turn on
            mdotSupSide = FluidHX(CompNum)%SupplySideLoop%MassFlowRateMax
            CALL SetComponentFlowRate(mdotSupSide,                      &
                         FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                         FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                         FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                         FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                         FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                         FluidHX(CompNum)%SupplySideLoop%CompNum)
            IF (mdotSupSide > MassFlowTolerance) THEN
              mdotDmdSide = FluidHX(CompNum)%DemandSideLoop%MassFlowRateMax
            ELSE
              mdotDmdSide = 0.d0
            ENDIF

            CALL SetComponentFlowRate(mdotDmdSide,                      &
                         FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                         FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                         FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                         FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                         FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                         FluidHX(CompNum)%DemandSideLoop%CompNum)
          ELSE ! not able to cool so turn off
            mdotSupSide = 0.d0
            CALL SetComponentFlowRate(mdotSupSide,                          &
                             FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                             FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                             FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                             FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                             FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                             FluidHX(CompNum)%SupplySideLoop%CompNum)
            mdotDmdSide = 0.d0
            CALL SetComponentFlowRate(mdotDmdSide,                          &
                             FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                             FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                             FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                             FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                             FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                             FluidHX(CompNum)%DemandSideLoop%CompNum)
          ENDIF

        ELSE ! requesting heating
          DeltaTHeating = FluidHX(CompNum)%DemandSideLoop%InletTemp - FluidHX(CompNum)%SupplySideLoop%InletTemp
          IF ( DeltaTHeating > FluidHX(CompNum)%TempControlTol ) THEN ! can do heating so turn on
            mdotSupSide = FluidHX(CompNum)%SupplySideLoop%MassFlowRateMax
            CALL SetComponentFlowRate(mdotSupSide,                      &
                         FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                         FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                         FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                         FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                         FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                         FluidHX(CompNum)%SupplySideLoop%CompNum)
            IF (mdotSupSide > MassFlowTolerance) THEN
              mdotDmdSide = FluidHX(CompNum)%DemandSideLoop%MassFlowRateMax
            ELSE
              mdotDmdSide = 0.d0
            ENDIF
            CALL SetComponentFlowRate(mdotDmdSide,                      &
                         FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                         FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                         FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                         FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                         FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                         FluidHX(CompNum)%DemandSideLoop%CompNum)
          ELSE ! not able to heat so turn off
            mdotSupSide = 0.d0
            CALL SetComponentFlowRate(mdotSupSide,                          &
                             FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                             FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                             FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                             FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                             FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                             FluidHX(CompNum)%SupplySideLoop%CompNum)
            mdotDmdSide = 0.d0
            CALL SetComponentFlowRate(mdotDmdSide,                          &
                             FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                             FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                             FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                             FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                             FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                             FluidHX(CompNum)%DemandSideLoop%CompNum)
          ENDIF
        ENDIF

      ELSE ! no load
        mdotSupSide = 0.d0
        CALL SetComponentFlowRate(mdotSupSide,                          &
                         FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                         FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                         FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                         FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                         FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                         FluidHX(CompNum)%SupplySideLoop%CompNum)
        mdotDmdSide = 0.d0
        CALL SetComponentFlowRate(mdotDmdSide,                          &
                         FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                         FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                         FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                         FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                         FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                         FluidHX(CompNum)%DemandSideLoop%CompNum)
      ENDIF

    CASE (HeatingSetpointModulated)

      SetpointTemp = Node(FluidHX(CompNum)%SetpointNodeNum)%TempSetPoint
      DeltaTHeating = FluidHX(CompNum)%DemandSideLoop%InletTemp - FluidHX(CompNum)%SupplySideLoop%InletTemp
      IF (( DeltaTHeating > FluidHX(CompNum)%TempControlTol ) .AND. &
          (SetpointTemp > FluidHX(CompNum)%SupplySideLoop%InletTemp)) THEN
        ! can and want to heat
        mdotSupSide = FluidHX(CompNum)%SupplySideLoop%MassFlowRateMax
        CALL SetComponentFlowRate(mdotSupSide,                       &
                      FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                      FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                      FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                      FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                      FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                      FluidHX(CompNum)%SupplySideLoop%CompNum)
        IF (mdotSupSide > MassFlowTolerance) THEN

          TargetLeavingTemp = SetpointTemp
          CALL FindHXDemandSideLoopFlow(CompNum, TargetLeavingTemp, HeatingSupplySideLoop)
        ELSE
          mdotDmdSide = 0.d0
          CALL SetComponentFlowRate(mdotDmdSide,                           &
                            FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                            FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                            FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                            FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                            FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                            FluidHX(CompNum)%DemandSideLoop%CompNum)
        ENDIF
      ELSE ! not able are wanting to heat so turn off
        mdotSupSide = 0.d0
        CALL SetComponentFlowRate(mdotSupSide,                           &
                          FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                          FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                          FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                          FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                          FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                          FluidHX(CompNum)%SupplySideLoop%CompNum)
        mdotDmdSide = 0.d0
        CALL SetComponentFlowRate(mdotDmdSide,                           &
                          FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                          FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                          FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                          FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                          FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                          FluidHX(CompNum)%DemandSideLoop%CompNum)
      ENDIF

    CASE (HeatingSetpointOnOff)

      SetpointTemp = Node(FluidHX(CompNum)%SetpointNodeNum)%TempSetPoint
      DeltaTHeating = FluidHX(CompNum)%DemandSideLoop%InletTemp - FluidHX(CompNum)%SupplySideLoop%InletTemp
      IF (( DeltaTHeating > FluidHX(CompNum)%TempControlTol ) .AND. &
          (SetpointTemp > FluidHX(CompNum)%SupplySideLoop%InletTemp)) THEN
        ! can and want to heat
        mdotSupSide = FluidHX(CompNum)%SupplySideLoop%MassFlowRateMax
        CALL SetComponentFlowRate(mdotSupSide,                       &
                      FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                      FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                      FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                      FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                      FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                      FluidHX(CompNum)%SupplySideLoop%CompNum)
        IF (mdotSupSide > MassFlowTolerance) THEN
          mdotDmdSide = FluidHX(CompNum)%DemandSideLoop%MassFlowRateMax
        ELSE
          mdotDmdSide = 0.d0
        ENDIF
        CALL SetComponentFlowRate(mdotDmdSide,                       &
                      FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                      FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                      FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                      FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                      FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                      FluidHX(CompNum)%DemandSideLoop%CompNum)
      ELSE ! not able are wanting to heat so turn off
        mdotSupSide = 0.d0
        CALL SetComponentFlowRate(mdotSupSide,                           &
                          FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                          FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                          FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                          FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                          FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                          FluidHX(CompNum)%SupplySideLoop%CompNum)
        mdotDmdSide = 0.d0
        CALL SetComponentFlowRate(mdotDmdSide,                           &
                          FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                          FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                          FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                          FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                          FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                          FluidHX(CompNum)%DemandSideLoop%CompNum)
      ENDIF

    CASE (CoolingSetpointModulated)

      SetpointTemp = Node(FluidHX(CompNum)%SetpointNodeNum)%TempSetPoint
      DeltaTCooling = FluidHX(CompNum)%SupplySideLoop%InletTemp - FluidHX(CompNum)%DemandSideLoop%InletTemp
      IF (( DeltaTCooling > FluidHX(CompNum)%TempControlTol ) .AND. &
          (SetpointTemp < FluidHX(CompNum)%SupplySideLoop%InletTemp)) THEN
        ! can and want to cool
        mdotSupSide = FluidHX(CompNum)%SupplySideLoop%MassFlowRateMax
        CALL SetComponentFlowRate(mdotSupSide,                       &
                      FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                      FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                      FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                      FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                      FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                      FluidHX(CompNum)%SupplySideLoop%CompNum)
        IF (mdotSupSide > MassFlowTolerance) THEN
          TargetLeavingTemp = SetpointTemp
          CALL FindHXDemandSideLoopFlow(CompNum, TargetLeavingTemp, CoolingSupplySideLoop)
        ELSE
          mdotDmdSide = 0.d0
          CALL SetComponentFlowRate(mdotDmdSide,                           &
                            FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                            FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                            FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                            FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                            FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                            FluidHX(CompNum)%DemandSideLoop%CompNum)
        ENDIF
      ELSE ! not able are wanting to cool so turn off
        mdotSupSide = 0.d0
        CALL SetComponentFlowRate(mdotSupSide,                           &
                          FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                          FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                          FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                          FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                          FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                          FluidHX(CompNum)%SupplySideLoop%CompNum)
        mdotDmdSide = 0.d0
        CALL SetComponentFlowRate(mdotDmdSide,                           &
                          FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                          FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                          FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                          FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                          FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                          FluidHX(CompNum)%DemandSideLoop%CompNum)
      ENDIF

    CASE (CoolingSetpointOnOff)

      SetpointTemp = Node(FluidHX(CompNum)%SetpointNodeNum)%TempSetPoint
      DeltaTCooling = FluidHX(CompNum)%SupplySideLoop%InletTemp - FluidHX(CompNum)%DemandSideLoop%InletTemp
      IF (( DeltaTCooling > FluidHX(CompNum)%TempControlTol ) .AND. &
          (SetpointTemp < FluidHX(CompNum)%SupplySideLoop%InletTemp)) THEN
        ! can and want to cool
        mdotSupSide = FluidHX(CompNum)%SupplySideLoop%MassFlowRateMax
        CALL SetComponentFlowRate(mdotSupSide,                       &
                      FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                      FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                      FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                      FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                      FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                      FluidHX(CompNum)%SupplySideLoop%CompNum)
        IF (mdotSupSide > MassFlowTolerance) THEN
          mdotDmdSide = FluidHX(CompNum)%DemandSideLoop%MassFlowRateMax
        ELSE
          mdotDmdSide = 0.d0
        ENDIF
        CALL SetComponentFlowRate(mdotDmdSide,                       &
                      FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                      FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                      FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                      FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                      FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                      FluidHX(CompNum)%DemandSideLoop%CompNum)
      ELSE ! not able or are wanting to cool so turn off
        mdotSupSide = 0.d0
        CALL SetComponentFlowRate(mdotSupSide,                           &
                          FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                          FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                          FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                          FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                          FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                          FluidHX(CompNum)%SupplySideLoop%CompNum)
        mdotDmdSide = 0.d0
        CALL SetComponentFlowRate(mdotDmdSide,                           &
                          FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                          FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                          FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                          FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                          FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                          FluidHX(CompNum)%DemandSideLoop%CompNum)
      ENDIF

    CASE (DualDeadbandSetpointModulated)

      SetpointTempLo = Node(FluidHX(CompNum)%SetpointNodeNum)%TempSetPointLo
      SetpointTempHi = Node(FluidHX(CompNum)%SetpointNodeNum)%TempSetPointHi
      DeltaTCooling = FluidHX(CompNum)%SupplySideLoop%InletTemp - FluidHX(CompNum)%DemandSideLoop%InletTemp
      DeltaTCoolSetpointDemand = SetpointTempHi - FluidHX(CompNum)%DemandSideLoop%InletTemp
      DeltaTCoolSetpointSupply = SetpointTempHi - FluidHX(CompNum)%SupplySideLoop%InletTemp
      DeltaTHeating = FluidHX(CompNum)%DemandSideLoop%InletTemp - FluidHX(CompNum)%SupplySideLoop%InletTemp
      DeltaTHeatSetpointDemand = SetpointTempLo - FluidHX(CompNum)%DemandSideLoop%InletTemp
      DeltaTHeatSetpointSupply = SetpointTempLo - FluidHX(CompNum)%SupplySideLoop%InletTemp
      IF (( DeltaTCooling > FluidHX(CompNum)%TempControlTol ) .AND. &
          (DeltaTCoolSetpointSupply < (-1.d0 * FluidHX(CompNum)%TempControlTol)) .AND. &
          (DeltaTCoolSetpointDemand > FluidHX(CompNum)%TempControlTol)  ) THEN
        ! can and want to cool
        mdotSupSide = FluidHX(CompNum)%SupplySideLoop%MassFlowRateMax
        CALL SetComponentFlowRate(mdotSupSide,                       &
                      FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                      FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                      FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                      FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                      FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                      FluidHX(CompNum)%SupplySideLoop%CompNum)
        IF (mdotSupSide > MassFlowTolerance) THEN
          TargetLeavingTemp = SetpointTempHi
          CALL FindHXDemandSideLoopFlow(CompNum, TargetLeavingTemp, CoolingSupplySideLoop)
        ELSE
          mdotDmdSide = 0.d0
          CALL SetComponentFlowRate(mdotDmdSide,                           &
                            FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                            FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                            FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                            FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                            FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                            FluidHX(CompNum)%DemandSideLoop%CompNum)
        ENDIF
      ELSEIF (( DeltaTHeating > FluidHX(CompNum)%TempControlTol ) .AND. &
              (DeltaTHeatSetpointDemand < (-1.d0 * FluidHX(CompNum)%TempControlTol)) .AND. &
              (DeltaTHeatSetpointSupply > FluidHX(CompNum)%TempControlTol) ) THEN
        ! can and want to heat
        mdotSupSide = FluidHX(CompNum)%SupplySideLoop%MassFlowRateMax
        CALL SetComponentFlowRate(mdotSupSide,                       &
                      FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                      FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                      FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                      FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                      FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                      FluidHX(CompNum)%SupplySideLoop%CompNum)
        IF (mdotSupSide > MassFlowTolerance) THEN
          TargetLeavingTemp = SetpointTempLo
          CALL FindHXDemandSideLoopFlow(CompNum, TargetLeavingTemp, HeatingSupplySideLoop)
        ELSE
          mdotDmdSide = 0.d0
          CALL SetComponentFlowRate(mdotDmdSide,                           &
                            FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                            FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                            FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                            FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                            FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                            FluidHX(CompNum)%DemandSideLoop%CompNum)
        ENDIF
      ELSE ! not able or don't want conditioning
        mdotSupSide = 0.d0
        CALL SetComponentFlowRate(mdotSupSide,                           &
                          FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                          FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                          FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                          FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                          FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                          FluidHX(CompNum)%SupplySideLoop%CompNum)
        mdotDmdSide = 0.d0
        CALL SetComponentFlowRate(mdotDmdSide,                           &
                          FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                          FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                          FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                          FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                          FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                          FluidHX(CompNum)%DemandSideLoop%CompNum)
      ENDIF

    CASE (DualDeadbandSetpointOnOff)

      SetpointTempLo = Node(FluidHX(CompNum)%SetpointNodeNum)%TempSetPointLo
      SetpointTempHi = Node(FluidHX(CompNum)%SetpointNodeNum)%TempSetPointHi
      DeltaTCooling = FluidHX(CompNum)%SupplySideLoop%InletTemp - FluidHX(CompNum)%DemandSideLoop%InletTemp
      DeltaTHeating = FluidHX(CompNum)%DemandSideLoop%InletTemp - FluidHX(CompNum)%SupplySideLoop%InletTemp
      IF (( DeltaTCooling > FluidHX(CompNum)%TempControlTol ) .AND. &
          (SetpointTempHi < FluidHX(CompNum)%SupplySideLoop%InletTemp)) THEN
        ! can and want to cool
        mdotSupSide = FluidHX(CompNum)%SupplySideLoop%MassFlowRateMax
        CALL SetComponentFlowRate(mdotSupSide,                       &
                      FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                      FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                      FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                      FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                      FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                      FluidHX(CompNum)%SupplySideLoop%CompNum)
        IF (mdotSupSide > MassFlowTolerance) THEN
          mdotDmdSide = FluidHX(CompNum)%DemandSideLoop%MassFlowRateMax
        ELSE
          mdotDmdSide = 0.d0
        ENDIF
        CALL SetComponentFlowRate(mdotDmdSide,                       &
                      FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                      FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                      FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                      FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                      FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                      FluidHX(CompNum)%DemandSideLoop%CompNum)
      ELSEIF (( DeltaTHeating > FluidHX(CompNum)%TempControlTol ) .AND. &
              (SetpointTempLo > FluidHX(CompNum)%SupplySideLoop%InletTemp)) THEN
        ! can and want to heat
        mdotSupSide = FluidHX(CompNum)%SupplySideLoop%MassFlowRateMax
        CALL SetComponentFlowRate(mdotSupSide,                       &
                      FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                      FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                      FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                      FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                      FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                      FluidHX(CompNum)%SupplySideLoop%CompNum)
        IF (mdotSupSide > MassFlowTolerance) THEN
          mdotDmdSide = FluidHX(CompNum)%DemandSideLoop%MassFlowRateMax
        ELSE
          mdotDmdSide = 0.d0
        ENDIF
        CALL SetComponentFlowRate(mdotDmdSide,                       &
                      FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                      FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                      FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                      FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                      FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                      FluidHX(CompNum)%DemandSideLoop%CompNum)
      ELSE ! not able or don't want conditioning
        mdotSupSide = 0.d0
        CALL SetComponentFlowRate(mdotSupSide,                           &
                          FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                          FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                          FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                          FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                          FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                          FluidHX(CompNum)%SupplySideLoop%CompNum)
        mdotDmdSide = 0.d0
        CALL SetComponentFlowRate(mdotDmdSide,                           &
                          FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                          FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                          FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                          FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                          FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                          FluidHX(CompNum)%DemandSideLoop%CompNum)
      ENDIF

    CASE (CoolingDifferentialOnOff)

      DeltaTCooling = FluidHX(CompNum)%SupplySideLoop%InletTemp - FluidHX(CompNum)%DemandSideLoop%InletTemp
      IF ( DeltaTCooling > FluidHX(CompNum)%TempControlTol ) THEN
        !  want to cool
        mdotSupSide = FluidHX(CompNum)%SupplySideLoop%MassFlowRateMax
        CALL SetComponentFlowRate(mdotSupSide,                       &
                      FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                      FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                      FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                      FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                      FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                      FluidHX(CompNum)%SupplySideLoop%CompNum)
        IF (mdotSupSide > MassFlowTolerance) THEN
          mdotDmdSide = FluidHX(CompNum)%DemandSideLoop%MassFlowRateMax
        ELSE
          mdotDmdSide = 0.d0
        ENDIF
        CALL SetComponentFlowRate(mdotDmdSide,                       &
                      FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                      FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                      FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                      FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                      FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                      FluidHX(CompNum)%DemandSideLoop%CompNum)
      ELSE ! not wanting to cool so turn off
        mdotSupSide = 0.d0
        CALL SetComponentFlowRate(mdotSupSide,                           &
                          FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                          FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                          FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                          FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                          FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                          FluidHX(CompNum)%SupplySideLoop%CompNum)
        mdotDmdSide = 0.d0
        CALL SetComponentFlowRate(mdotDmdSide,                           &
                          FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                          FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                          FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                          FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                          FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                          FluidHX(CompNum)%DemandSideLoop%CompNum)
      ENDIF

    CASE (CoolingSetpointOnOffWithComponentOverride)

      SELECT CASE (FluidHX(CompNum)%ControlSignalTemp)
      CASE (WetBulbTemperature)
        ControlSignalValue = OutWetBulbTemp
      CASE (DryBulbTemperature)
        ControlSignalValue = OutDryBulbTemp
      CASE (LoopTemperature )
        ! ControlSignalValue = FluidHX(CompNum)%DemandSideLoop%InletTemp
        ControlSignalValue = Node(FluidHX(CompNum)%OtherCompDemandSideLoop%InletNodeNum)%TempLastTimestep
      END SELECT

      SetpointTemp = Node(FluidHX(CompNum)%SetpointNodeNum)%TempSetPoint
      DeltaTCooling = SetpointTemp - ControlSignalValue
      !obtain shut down state
      ChillerShutDown = PlantLoop(FluidHX(CompNum)%OtherCompSupplySideLoop%LoopNum)     &
                        %LoopSide(FluidHX(CompNum)%OtherCompSupplySideLoop%LoopSideNum) &
                          %Branch(FluidHX(CompNum)%OtherCompSupplySideLoop%BranchNum)   &
                            %Comp(FluidHX(CompNum)%OtherCompSupplySideLoop%CompNum)%FreeCoolCntrlShutDown
      IF (ChillerShutDown .AND. ( DeltaTCooling > FluidHX(CompNum)%TempControlTol ) ) THEN
        ! can and want to cool
        mdotSupSide = FluidHX(CompNum)%SupplySideLoop%MassFlowRateMax
        CALL SetComponentFlowRate(mdotSupSide,                       &
                      FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                      FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                      FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                      FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                      FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                      FluidHX(CompNum)%SupplySideLoop%CompNum)
        IF (mdotSupSide > MassFlowTolerance) THEN
          mdotDmdSide = FluidHX(CompNum)%DemandSideLoop%MassFlowRateMax
        ELSE
          mdotDmdSide = 0.d0
        ENDIF
        CALL SetComponentFlowRate(mdotDmdSide,                       &
                      FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                      FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                      FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                      FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                      FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                      FluidHX(CompNum)%DemandSideLoop%CompNum)

      ELSE
        mdotSupSide = 0.d0
        CALL SetComponentFlowRate(mdotSupSide,                           &
                          FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                          FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                          FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                          FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                          FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                          FluidHX(CompNum)%SupplySideLoop%CompNum)
        mdotDmdSide = 0.d0
        CALL SetComponentFlowRate(mdotDmdSide,                           &
                          FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                          FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                          FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                          FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                          FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                          FluidHX(CompNum)%DemandSideLoop%CompNum)
      ENDIF

    END SELECT

  ELSE ! scheduled off
    mdotSupSide = 0.d0
    CALL SetComponentFlowRate(mdotSupSide,                           &
                      FluidHX(CompNum)%SupplySideLoop%InletNodeNum,  &
                      FluidHX(CompNum)%SupplySideLoop%OutletNodeNum, &
                      FluidHX(CompNum)%SupplySideLoop%LoopNum,       &
                      FluidHX(CompNum)%SupplySideLoop%LoopSideNum,   &
                      FluidHX(CompNum)%SupplySideLoop%BranchNum,     &
                      FluidHX(CompNum)%SupplySideLoop%CompNum)
    mdotDmdSide = 0.d0
    CALL SetComponentFlowRate(mdotDmdSide,                           &
                      FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                      FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                      FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                      FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                      FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                      FluidHX(CompNum)%DemandSideLoop%CompNum)
  ENDIF

  RETURN

END SUBROUTINE ControlFluidHeatExchanger

SUBROUTINE CalcFluidHeatExchanger(CompNum, SupSideMdot, DmdSideMdot)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B.Griffith, derived from CalcEconHeatExchanger by  Sankaranarayanan K P aug. 2007
          !       DATE WRITTEN   November 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Evalutate heat exchanger model and calculate leaving temperatures

          ! METHODOLOGY EMPLOYED:
          ! apply heat transfer model depending on type of HX used

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: GetSpecificHeatGlycol
  USE DataLoopNode,    ONLY: Node

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)  :: CompNum
  REAL(r64), INTENT(IN)  :: SupSideMdot ! mass flow rate of fluid entering from supply side loop
  REAL(r64), INTENT(IN)  :: DmdSideMdot ! mass flow rate of fluid entering from demand side loop
          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER     :: CmaxMixedCminUnmixed = 40
  INTEGER, PARAMETER     :: CmaxUnMixedCminMixed = 41

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: SupSideLoopInletTemp
  REAL(r64)  :: DmdSideLoopInletTemp
  REAL(r64)  :: SupSideLoopInletCp  ! specific heat of fluid entering from supply side loop at inlet temp
  REAL(r64)  :: DmdSideLoopInletCp  ! specific heat of fluid entering from demand side loop at inlet temp
  REAL(r64)  :: SupSideCapRate ! product of specific heat and mass flow for supply side loop at inlet temp
  REAL(r64)  :: DmdSideCapRate ! product of specific heat and mass flow for demand side loop at inlet temp
  REAL(r64)  :: MinCapRate ! minimum capacity flow rate
  REAL(r64)  :: MaxCapRate ! maximum capacity flow rate
  REAL(r64)  :: NTU ! number of transfer units for heat exchanger performance model
  REAL(r64)  :: CapRatio
  REAL(r64)  :: ExpCheckValue1
  REAL(r64)  :: ExpCheckValue2
  REAL(r64)  :: Effectiveness
  REAL(r64)  :: HeatTransferRate
  REAL(r64)  :: MdotDmdSide
  REAL(r64)  :: LeavingTempMinFlow
  REAL(r64)  :: LeavingTempFullFlow
  INTEGER    :: CrossFlowEquation

  SupSideLoopInletTemp = Node(FluidHX(CompNum)%SupplySideLoop%InletNodeNum)%Temp
  DmdSideLoopInletTemp = Node(FluidHX(CompNum)%DemandSideLoop%InletNodeNum)%Temp

  SupSideLoopInletCp = GetSpecificHeatGlycol(PlantLoop(FluidHX(CompNum)%SupplySideLoop%LoopNum)%FluidName, &
                                SupSideLoopInletTemp ,  &
                                PlantLoop(FluidHX(CompNum)%SupplySideLoop%LoopNum)%FluidIndex, &
                                'CalcFluidHeatExchanger')
  DmdSideLoopInletCp = GetSpecificHeatGlycol(PlantLoop(FluidHX(CompNum)%DemandSideLoop%LoopNum)%FluidName, &
                                DmdSideLoopInletTemp ,  &
                                PlantLoop(FluidHX(CompNum)%DemandSideLoop%LoopNum)%FluidIndex, &
                                'CalcFluidHeatExchanger')

  SupSideCapRate = SupSideMdot * SupSideLoopInletCp
  DmdSideCapRate = DmdSideMdot * DmdSideLoopInletCp
  MinCapRate = MIN(SupSideCapRate, DmdSideCapRate)
  MaxCapRate = MAX(SupSideCapRate, DmdSideCapRate)

  IF (MinCapRate > 0.d0) THEN

    SELECT CASE (FluidHX(CompNum)%HeatExchangeModelType)

    CASE (CrossFlowBothUnMixed)
      NTU = FluidHX(CompNum)%UA/MinCapRate
      CapRatio = MinCapRate/MaxCapRate
      EXPCheckValue1 = NTU**0.22d0/CapRatio
      EXPCheckValue2 = -CapRatio*NTU**0.78d0
      IF ((EXPCheckValue1 > EXP_UpperLimit) .OR. (EXPCheckValue2 > EXP_UpperLimit)) THEN
        IF (-NTU >= EXP_LowerLimit) THEN
          Effectiveness = 1.d0-EXP(-NTU)
          Effectiveness = MIN(1.d0,Effectiveness)
        ELSE
          Effectiveness = 1.d0
        ENDIF
      ELSE
        Effectiveness = 1.d0 - EXP((NTU**0.22d0/CapRatio) * &
                    (EXP(-CapRatio*NTU**0.78d0) - 1.0d0))
        Effectiveness = MIN(1.d0,Effectiveness)
      ENDIF

    CASE (CrossFlowBothMixed)
      NTU = FluidHX(CompNum)%UA/MinCapRate
      CapRatio = MinCapRate/MaxCapRate
      EXPCheckValue1 =  -CapRatio*NTU
      EXPCheckValue2 =  -NTU
      IF (EXPCheckValue1 <  EXP_LowerLimit) THEN
        IF (EXPCheckValue2 >= EXP_LowerLimit) THEN
          Effectiveness = 1.d0-EXP(-NTU)
          Effectiveness = MIN(1.d0,Effectiveness)
        ELSE
          Effectiveness = 1.d0
        ENDIF

      ELSEIF ( (EXP(-NTU) == 1.d0) .or. (NTU == 0.d0) .OR. (EXP(-CapRatio*NTU) == 1.d0) ) THEN ! don't div by zero

        Effectiveness = 0.d0
      ELSE
        Effectiveness = 1.d0/( (1.d0/(1.d0-EXP(-NTU))) + (CapRatio/(1.d0 - EXP(-CapRatio*NTU) ) ) - ( 1.d0/ NTU) )
        Effectiveness = MIN(1.0d0,Effectiveness)
      ENDIF

    CASE (CrossFlowSupplyLoopMixedDemandLoopUnMixed, CrossFlowSupplyLoopUnMixedDemandLoopMixed)

      IF (SupSideCapRate == MaxCapRate .AND. FluidHX(CompNum)%HeatExchangeModelType &
                         == CrossFlowSupplyLoopMixedDemandLoopUnMixed) THEN
        CrossFlowEquation = CmaxMixedCminUnmixed
      ELSEIF (SupSideCapRate == MinCapRate .AND. FluidHX(CompNum)%HeatExchangeModelType &
                         == CrossFlowSupplyLoopMixedDemandLoopUnMixed) THEN
        CrossFlowEquation = CmaxUnMixedCminMixed
      ELSEIF (DmdSideCapRate == MaxCapRate .AND. FluidHX(CompNum)%HeatExchangeModelType &
                         == CrossFlowSupplyLoopUnMixedDemandLoopMixed) THEN
        CrossFlowEquation = CmaxMixedCminUnmixed
      ELSEIF (DmdSideCapRate == MinCapRate .AND. FluidHX(CompNum)%HeatExchangeModelType &
                         == CrossFlowSupplyLoopUnMixedDemandLoopMixed) THEN
        CrossFlowEquation = CmaxUnMixedCminMixed
      ELSE
        CrossFlowEquation = CmaxMixedCminUnmixed
      ENDIF

      NTU = FluidHX(CompNum)%UA/MinCapRate
      CapRatio = MinCapRate/MaxCapRate
      IF (CrossFlowEquation == CmaxMixedCminUnmixed) THEN
        ExpCheckValue1 = -NTU
        IF (CapRatio == 0.d0) THEN ! protect div by zero
          IF (ExpCheckValue1 >= EXP_LowerLimit) THEN
            Effectiveness = 1.d0-EXP(-NTU)
            Effectiveness = MIN(1.d0,Effectiveness)
          ELSE
            Effectiveness = 1.d0
          ENDIF
        ElSEIF (ExpCheckValue1 < EXP_LowerLimit ) THEN
          Effectiveness = 0.632d0/CapRatio
          Effectiveness = MIN(1.d0,Effectiveness)
        ELSE
          Effectiveness = (1.d0/CapRatio) * (1.d0 - EXP(CapRatio*EXP(-NTU)- 1.d0 ) )
          Effectiveness = MIN(1.d0,Effectiveness)
        ENDIF
      ELSEIF (CrossFlowEquation == CmaxUnMixedCminMixed) THEN
        ExpCheckValue1 = -CapRatio*NTU
        IF (CapRatio == 0.d0) THEN
          IF (-NTU >= EXP_LowerLimit) THEN
            Effectiveness = 1.d0-EXP(-NTU)
            Effectiveness = MIN(1.d0,Effectiveness)
          ELSE
            Effectiveness = 1.d0
          ENDIF
        ELSE
          EXPCheckValue2 = -(1.0d0/CapRatio)*(1.0d0 - EXP(-CapRatio*NTU) )
          IF (EXPCheckValue2 < EXP_LowerLimit) THEN
            Effectiveness = 1.d0
          ELSE
            Effectiveness = 1.0d0 - EXP(-(1.0d0/CapRatio)*(1.0d0 - EXP(-CapRatio*NTU) ) )
            Effectiveness = MIN(1.d0,Effectiveness)
          ENDIF
        ENDIF
      ENDIF

    CASE (CounterFlow)
      NTU = FluidHX(CompNum)%UA/MinCapRate
      CapRatio = MinCapRate/MaxCapRate
      EXPCheckValue1 = -NTU*(1.d0-CapRatio)
      IF (EXPCheckValue1 > EXP_UpperLimit) THEN
        IF (-NTU >= EXP_LowerLimit) THEN
          Effectiveness = 1.d0-EXP(-NTU)
          Effectiveness = MIN(1.d0,Effectiveness)
        ELSE
          Effectiveness = 1.d0
        ENDIF
      ELSEIF (CapRatio*EXP(-NTU*(1.d0-CapRatio)) == 1.0d0) THEN
        IF (-NTU >= EXP_LowerLimit) THEN
          Effectiveness = 1.d0-EXP(-NTU)
          Effectiveness = MIN(1.d0,Effectiveness)
        ELSE
          Effectiveness = 1.d0
        ENDIF
      ELSE
        Effectiveness = (1.d0-EXP(-NTU*(1.d0-CapRatio)))/(1.d0-CapRatio*EXP(-NTU*(1.d0-CapRatio)))
        Effectiveness = MIN(1.0d0,Effectiveness)
      ENDIF

    CASE (ParallelFlow)
      NTU = FluidHX(CompNum)%UA/MinCapRate
      CapRatio = MinCapRate/MaxCapRate
      EXPCheckValue1 = -NTU*(1.d0+CapRatio)
      IF (EXPCheckValue1 > EXP_UpperLimit) THEN
        IF (-NTU >= EXP_LowerLimit) THEN
          Effectiveness = 1.d0-EXP(-NTU)
          Effectiveness = MIN(1.d0,Effectiveness)
        ELSE
          Effectiveness = 1.d0
        ENDIF
      ELSE
        Effectiveness = (1.d0-EXP(-NTU*(1.d0+CapRatio)))/(1.d0+CapRatio)
        Effectiveness = MIN(1.d0,Effectiveness)
      ENDIF

    CASE (Ideal)
      Effectiveness = 1.d0
    END SELECT

  ELSE ! no capacity
    Effectiveness = 0.d0

  ENDIF

  HeatTransferRate = Effectiveness * MinCapRate * ( SupSideLoopInletTemp - DmdSideLoopInletTemp) ! + means supply side is cooled

  IF (SupSideMdot > 0.d0 ) THEN
    FluidHX(CompNum)%SupplySideLoop%OutletTemp = SupSideLoopInletTemp - HeatTransferRate/ (SupSideLoopInletCp * SupSideMdot)
  ELSE
    FluidHX(CompNum)%SupplySideLoop%OutletTemp = SupSideLoopInletTemp
  ENDIF

  IF (DmdSideMdot > 0.d0) THEN
    FluidHX(CompNum)%DemandSideLoop%OutletTemp = DmdSideLoopInletTemp + HeatTransferRate/ (DmdSideLoopInletCp * DmdSideMdot)
  ELSE
    FluidHX(CompNum)%DemandSideLoop%OutletTemp = DmdSideLoopInletTemp
  ENDIF
  FluidHX(CompNum)%Effectiveness                    = Effectiveness
  FluidHX(CompNum)%HeatTransferRate                 = HeatTransferRate
  FluidHX(CompNum)%SupplySideLoop%InletTemp         = SupSideLoopInletTemp
  FluidHX(CompNum)%SupplySideLoop%InletMassFlowRate = SupSideMdot
  FluidHX(CompNum)%DemandSideLoop%InletTemp         = DmdSideLoopInletTemp
  FluidHX(CompNum)%DemandSideLoop%InletMassFlowRate = DmdSideMdot

  RETURN

END SUBROUTINE CalcFluidHeatExchanger

SUBROUTINE FindHXDemandSideLoopFlow(CompNum, TargetSupplySideLoopLeavingTemp, HXActionMode)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   November 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! modulate demand side flow rate to hit a target leaving temperature (within tolerance)

          ! METHODOLOGY EMPLOYED:
          ! uses E+'s Regula Falsi numercial method

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,    ONLY: WarmUpFlag
  USE General,        ONLY: RoundSigDigits, SolveRegulaFalsi
  USE PlantUtilities, ONLY: SetComponentFlowRate

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN) :: CompNum
  REAL(r64), INTENT(IN) :: TargetSupplySideLoopLeavingTemp
  INTEGER,   INTENT(IN) :: HXActionMode

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER,   PARAMETER  :: MaxIte    = 500     ! Maximum number of iterations for solver
  REAL(r64), PARAMETER  :: Acc       = 1.d-3   ! Accuracy of solver result

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                 :: SolFla              ! Flag of solver
  REAL(r64), DIMENSION(2) :: Par                 ! Parameter array passed to solver

  REAL(r64)               :: LeavingTempMinFlow
  REAL(r64)               :: LeavingTempFullFlow
  REAL(r64)  :: SupSideMdot ! mass flow rate of fluid entering from supply side loop
  REAL(r64)  :: DmdSideMdot ! mass flow rate of fluid entering from demand side loop

  SupSideMdot = Node(FluidHX(CompNum)%SupplySideLoop%InletNodeNum)%MassFlowRate
  ! first see if root is bracketed
  ! min demand flow
  DmdSideMdot = FluidHX(CompNum)%DemandSideLoop%MassFlowRateMin
  CALL CalcFluidHeatExchanger(CompNum,SupSideMdot, DmdSideMdot)
  LeavingTempMinFlow = FluidHX(CompNum)%SupplySideLoop%OutletTemp

  ! full demand flow
  DmdSideMdot = FluidHX(CompNum)%DemandSideLoop%MassFlowRateMax
  CALL CalcFluidHeatExchanger(CompNum,SupSideMdot, DmdSideMdot)
  LeavingTempFullFlow = FluidHX(CompNum)%SupplySideLoop%OutletTemp

  SELECT CASE (HXActionMode)

  CASE (HeatingSupplySideLoop)
    IF ((LeavingTempFullFlow > TargetSupplySideLoopLeavingTemp) &
        .AND. (TargetSupplySideLoopLeavingTemp > LeavingTempMinFlow )) THEN
      ! need to solve
      Par(1) = REAL( CompNum , r64) ! HX index
      Par(2) = TargetSupplySideLoopLeavingTemp

      CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, DmdSideMdot, HXDemandSideLoopFlowResidual,&
                            FluidHX(CompNum)%DemandSideLoop%MassFlowRateMin , &
                            FluidHX(CompNum)%DemandSideLoop%MassFlowRateMax, Par)

      IF (SolFla == -1) THEN ! no convergence
        IF (.NOT. WarmupFlag) THEN
          IF (FluidHX(CompNum)%DmdSideModulatSolvNoConvergeErrorCount < 1) THEN
            FluidHX(CompNum)%DmdSideModulatSolvNoConvergeErrorCount = FluidHX(CompNum)%DmdSideModulatSolvNoConvergeErrorCount + 1
            CALL ShowWarningError(ComponentClassName//' named '//TRIM(FluidHX(CompNum)%Name)// &
                                  ' - Iteration Limit exceeded calculating demand side loop flow rate' )
            CALL ShowContinueError('Simulation continues with calculated demand side mass flow rate = ' &
                                    //RoundSigDigits(DmdSideMdot, 7) )
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd(ComponentClassName//' named '//TRIM(FluidHX(CompNum)%Name)// &
                                  ' - Iteration Limit exceeded calculating demand side loop flow rate continues.', &
                                  FluidHX(CompNum)%DmdSideModulatSolvNoConvergeErrorIndex, DmdSideMdot, DmdSideMdot)
        ENDIF
      ELSEIF (SolFla == -2) THEN !f(x0) and f(x1) have the same sign
        DmdSideMdot = FluidHX(CompNum)%DemandSideLoop%MassFlowRateMax * &
                       (LeavingTempFullFlow - TargetSupplySideLoopLeavingTemp) &
                       /(LeavingTempFullFlow - LeavingTempMinFlow)
        IF (.NOT. WarmupFlag) THEN
          IF (FluidHX(CompNum)%DmdSideModulatSolvFailErrorCount < 1) THEN
            FluidHX(CompNum)%DmdSideModulatSolvFailErrorCount = FluidHX(CompNum)%DmdSideModulatSolvFailErrorCount + 1
            CALL ShowWarningError(ComponentClassName//' named '//TRIM(FluidHX(CompNum)%Name)// &
                                  ' - Solver failed to calculate demand side loop flow rate' )
            CALL ShowContinueError('Simulation continues with estimated demand side mass flow rate = ' &
                                    //RoundSigDigits(DmdSideMdot, 7) )
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd(ComponentClassName//' named '//TRIM(FluidHX(CompNum)%Name)// &
                                  ' - Solver failed to calculate demand side loop flow rate continues.', &
                                  FluidHX(CompNum)%DmdSideModulatSolvFailErrorIndex, DmdSideMdot, DmdSideMdot)
        ENDIF
      ENDIF
      CALL SetComponentFlowRate(DmdSideMdot,                      &
                 FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                 FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                 FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                 FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                 FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                 FluidHX(CompNum)%DemandSideLoop%CompNum)

    ELSEIF ( ( TargetSupplySideLoopLeavingTemp >= LeavingTempFullFlow ) &
         .AND. (LeavingTempFullFlow > LeavingTempMinFlow) ) THEN
      ! run at full flow
      DmdSideMdot = FluidHX(CompNum)%DemandSideLoop%MassFlowRateMax
      CALL SetComponentFlowRate(DmdSideMdot,                      &
                   FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                   FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                   FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                   FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                   FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                   FluidHX(CompNum)%DemandSideLoop%CompNum)

    ELSEIF ( LeavingTempMinFlow >=  TargetSupplySideLoopLeavingTemp) THEN

      ! run at min flow
      DmdSideMdot = FluidHX(CompNum)%DemandSideLoop%MassFlowRateMin
      CALL SetComponentFlowRate(DmdSideMdot,                      &
                 FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                 FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                 FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                 FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                 FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                 FluidHX(CompNum)%DemandSideLoop%CompNum)
    ENDIF
  CASE (CoolingSupplySideLoop)
    IF ((LeavingTempFullFlow < TargetSupplySideLoopLeavingTemp) &
        .AND. (TargetSupplySideLoopLeavingTemp <  LeavingTempMinFlow )) THEN
      ! need to solve
      Par(1) = REAL( CompNum , r64) ! HX index
      Par(2) = TargetSupplySideLoopLeavingTemp

      CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, DmdSideMdot, HXDemandSideLoopFlowResidual,&
                            FluidHX(CompNum)%DemandSideLoop%MassFlowRateMin , &
                            FluidHX(CompNum)%DemandSideLoop%MassFlowRateMax, Par)

      IF (SolFla == -1) THEN ! no convergence
        IF (.NOT. WarmupFlag) THEN
          IF (FluidHX(CompNum)%DmdSideModulatSolvNoConvergeErrorCount < 1) THEN
            FluidHX(CompNum)%DmdSideModulatSolvNoConvergeErrorCount = FluidHX(CompNum)%DmdSideModulatSolvNoConvergeErrorCount + 1
            CALL ShowWarningError(ComponentClassName//' named '//TRIM(FluidHX(CompNum)%Name)// &
                                  ' - Iteration Limit exceeded calculating demand side loop flow rate' )
            CALL ShowContinueError('Simulation continues with calculated demand side mass flow rate = ' &
                                    //RoundSigDigits(DmdSideMdot, 7) )
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd(ComponentClassName//' named '//TRIM(FluidHX(CompNum)%Name)// &
                                  ' - Iteration Limit exceeded calculating demand side loop flow rate continues.', &
                                  FluidHX(CompNum)%DmdSideModulatSolvNoConvergeErrorIndex, DmdSideMdot, DmdSideMdot)
        ENDIF
      ELSEIF (SolFla == -2) THEN !f(x0) and f(x1) have the same sign
        DmdSideMdot = FluidHX(CompNum)%DemandSideLoop%MassFlowRateMax * &
                       (LeavingTempFullFlow - TargetSupplySideLoopLeavingTemp) &
                       /(LeavingTempFullFlow - LeavingTempMinFlow)
        IF (.NOT. WarmupFlag) THEN
          IF (FluidHX(CompNum)%DmdSideModulatSolvFailErrorCount < 1) THEN
            FluidHX(CompNum)%DmdSideModulatSolvFailErrorCount = FluidHX(CompNum)%DmdSideModulatSolvFailErrorCount + 1
            CALL ShowWarningError(ComponentClassName//' named '//TRIM(FluidHX(CompNum)%Name)// &
                                  ' - Solver failed to calculate demand side loop flow rate' )
            CALL ShowContinueError('Simulation continues with estimated demand side mass flow rate = ' &
                                    //RoundSigDigits(DmdSideMdot, 7) )
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd(ComponentClassName//' named '//TRIM(FluidHX(CompNum)%Name)// &
                                  ' - Solver failed to calculate demand side loop flow rate continues.' ,&
                                  FluidHX(CompNum)%DmdSideModulatSolvFailErrorIndex, DmdSideMdot, DmdSideMdot)
        ENDIF
      ENDIF
      CALL SetComponentFlowRate(DmdSideMdot,                      &
                 FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                 FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                 FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                 FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                 FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                 FluidHX(CompNum)%DemandSideLoop%CompNum)
    ELSEIF ( ( TargetSupplySideLoopLeavingTemp <= LeavingTempFullFlow ) &
         .AND. (LeavingTempFullFlow < LeavingTempMinFlow) ) THEN
      ! run at full flow
      DmdSideMdot = FluidHX(CompNum)%DemandSideLoop%MassFlowRateMax
      CALL SetComponentFlowRate(DmdSideMdot,                      &
                   FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                   FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                   FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                   FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                   FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                   FluidHX(CompNum)%DemandSideLoop%CompNum)
    ELSEIF ( LeavingTempMinFlow <=  TargetSupplySideLoopLeavingTemp) THEN

        ! run at min flow
      DmdSideMdot = FluidHX(CompNum)%DemandSideLoop%MassFlowRateMin
      CALL SetComponentFlowRate(DmdSideMdot,                      &
                 FluidHX(CompNum)%DemandSideLoop%InletNodeNum,  &
                 FluidHX(CompNum)%DemandSideLoop%OutletNodeNum, &
                 FluidHX(CompNum)%DemandSideLoop%LoopNum,       &
                 FluidHX(CompNum)%DemandSideLoop%LoopSideNum,   &
                 FluidHX(CompNum)%DemandSideLoop%BranchNum,     &
                 FluidHX(CompNum)%DemandSideLoop%CompNum)
    ENDIF

  END SELECT

  RETURN

END SUBROUTINE FindHXDemandSideLoopFlow

FUNCTION HXDemandSideLoopFlowResidual(DmdSideMassFlowRate, Par ) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   December 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! calculate residual value for regula falsi solver

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE PlantUtilities,   ONLY: SetComponentFlowRate

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: DmdSideMassFlowRate
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! Par(1) = HX index number
                                                    ! Par(2) = desired supply side loop outlet temperature [C]
  REAL(r64)         :: Residuum                   ! Residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: CompNum
  REAL(r64) :: SupSideLoopOutletTemp
  REAL(r64) :: MdotTrial
  REAL(r64)  :: SupSideMdot ! mass flow rate of fluid entering from supply side loop


  MdotTrial = DmdSideMassFlowRate
  CompNum = INT(Par(1))
  SupSideMdot = Node(FluidHX(CompNum)%SupplySideLoop%InletNodeNum)%MassFlowRate

  CALL CalcFluidHeatExchanger(CompNum,SupSideMdot ,MdotTrial )

  SupSideLoopOutletTemp = FluidHX(CompNum)%SupplySideLoop%OutletTemp

  Residuum = Par(2) - SupSideLoopOutletTemp

  RETURN

END FUNCTION HXDemandSideLoopFlowResidual

SUBROUTINE UpdateFluidHeatExchanger(CompNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   December 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! update calculate results

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: CompNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  Node(FluidHX(CompNum)%DemandSideLoop%OutletNodeNum)%Temp = FluidHX(CompNum)%DemandSideLoop%OutletTemp
  Node(FluidHX(CompNum)%SupplySideLoop%OutletNodeNum)%Temp = FluidHX(CompNum)%SupplySideLoop%OutletTemp

  RETURN

END SUBROUTINE UpdateFluidHeatExchanger

SUBROUTINE ReportFluidHeatExchanger(CompNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   December, 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! update heat exchanger report variables

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals , ONLY: TimeStepSys, SmallLoad
  USE DataGlobals,      ONLY: SecInHour

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: CompNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  FluidHX(CompNum)%HeatTransferEnergy = FluidHX(CompNum)%HeatTransferRate *  TimeStepSys * SecInHour

  IF ( (ABS(FluidHX(CompNum)%HeatTransferRate) > SmallLoad) .AND. &
       (FluidHX(CompNum)%DemandSideLoop%InletMassFlowRate > 0.d0) .AND. &
       (FluidHX(CompNum)%SupplySideLoop%InletMassFlowRate > 0.d0) ) THEN
    FluidHX(CompNum)%OperationStatus = 1.d0
  ELSE
    FluidHX(CompNum)%OperationStatus = 0.d0
  ENDIF

  RETURN

END SUBROUTINE ReportFluidHeatExchanger

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
END MODULE PlantHeatExchangerFluidToFluid