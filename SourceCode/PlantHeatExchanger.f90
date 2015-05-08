MODULE EconomizerHeatExchanger

  ! Module containing the routines dealing with Economizer hydronic heat
  ! exchanger components.

  ! MODULE INFORMATION:
  !       AUTHOR         Sankaranarayanan K P
  !       DATE WRITTEN   August 2007
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! The purpose of this module is to simulate a Economizer hydronic heat
  ! exchanger.This component is a simple hydronic heat exchanger that can be
  ! used to couple plant side and condenser side hydronic loops. The purpose
  ! is primarily to enable implementation of Economizer. In other words,
  ! coupling the zone plant directly to the heat rejection equipment. For example,
  ! coupling a radiant hydronic system to a cooling tower and bypassing the
  ! heat pump or chiller.

  ! METHODOLOGY EMPLOYED:
  ! The component is concieved as a simple hydronic heat exchanger but with some
  ! control abilities. Another component (chiller or heat pump) on the
  ! loops is switched on or off by this component. When the other component is
  ! operating the loops are 'uncoupled' and no heat exchange takes place. When
  ! the control conditions are satisfied this heat exchanger switches the other
  ! component off and heat exchange between the loops is calculated. This is to
  ! emulate a bypass arrangement. The heat exchanger can be operated in 'Ideal'
  ! mode, that is the max possible heat exchanger occurs. If the loop fluids are
  ! the same, and the flow rates are the same, complete coupling will occur i.e.
  ! it will be as if the condenser fluid flowed through the plant loop.

  ! REFERENCES:

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals,       ONLY : MaxNameLength, InitConvTemp, BeginTimeStepFlag, BeginEnvrnFlag, SecInHour
USE DataInterfaces,    ONLY : ShowWarningError, ShowSevereError, ShowFatalError, &
                              ShowContinueError, SetupOutputVariable
USE DataPlant
USE General,           ONLY : TrimSigDigits

  ! Use statements for access to subroutines in other modules

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
  ! zero load tolerance used in equipment operation decisions (Watts)
  REAL(r64), PARAMETER                         :: zeroloadtol = 1.0d0
  !zero capacity tolerance used in heat exchanger cal (kg/s * J/kg/K)
  REAL(r64), PARAMETER                         :: zerocaptol  = .00001d0
  ! Economizer HX type string
  CHARACTER(len=*), PARAMETER :: cThisModuleObjectName = 'HeatExchanger:WatersideEconomizer'
  CHARACTER(len=*), PARAMETER :: Blank = ' '

  INTEGER, PARAMETER :: PlateFrame = 1
  INTEGER, PARAMETER :: CounterFlow = 2
  INTEGER, PARAMETER :: ParallelFlow    = 3
  INTEGER, PARAMETER :: Ideal = 4

  INTEGER, PARAMETER :: AirCooled = 1
  INTEGER, PARAMETER :: WaterCooled = 2

  REAL(r64), PARAMETER    :: BigNum = 1.0d+12

  ! DERIVED TYPE DEFINITIONS
TYPE HXData
  ! Input data
  CHARACTER(len=MaxNameLength) :: Name                  = Blank  ! name of the Economizer HX component
  CHARACTER(len=MaxNameLength) :: HXType                = Blank  ! Type of the Economizer HX component
  INTEGER                      :: HXTypeOf              = 0      ! Integer value to specify type of HX
  INTEGER                      :: AssociatedPlantLoop   =0      ! Plant Loop Num Associated with HX
  CHARACTER(len=MaxNameLength) :: Schedule              = Blank  ! Type of the Economizer HX component
  INTEGER                      :: ScheduleNum           =0       ! Integer value to specify Condenser Type
  CHARACTER(len=MaxNameLength) :: PlantInletNode        = Blank  ! plant side inlet node name
  INTEGER                      :: PlantInletNodeNum     =0       ! plant side inlet node number
  CHARACTER(len=MaxNameLength) :: PlantOutletNode       = Blank  ! plant side outlet node name
  INTEGER                      :: PlantOutletNodeNum    =0       ! plant side outlet node number
  CHARACTER(len=MaxNameLength) :: CondInletNode         = Blank  ! condenser side inlet node name
  INTEGER                      :: CondInletNodeNum      =0       ! condenser side inlet node number
  CHARACTER(len=MaxNameLength) :: CondOutletNode        = Blank  ! condenser side outlet node name
  INTEGER                      :: CondOutletNodeNum     =0       ! condenser side outlet node number
  REAL(r64)                    :: UA                    =0.0     ! UA for heat exchanger (ignored in ideal mode)
  REAL(r64)                    :: TempDiff              =0.0     ! Minimum Temperature Difference to activate HX
  REAL(r64)                    :: DesCapacity           =0.0     ! Design Capacity for HX
  REAL(r64)                    :: CondSideFlowRate      =0.0     ! volumetric flow rate through condenser side of unit m3/s
  REAL(r64)                    :: CondSideDesMassFlowRate = 0.d0 ! condenser side mass flow rate design level kg/s
  REAL(r64)                    :: PlantSideFlowRate     =0.0     ! volumetric flow rate through plant side of unit m3/s
  REAL(r64)                    :: PlantSideDesMassFlowRate = 0.d0 !plant side  mass flow rate design level kg/s
  REAL(r64)                    :: MinSideDesCpMassFlux  = 0.d0   ! lower of plant and cond cp*mdot 
  ! Report data
  REAL(r64)                    :: CondInletTemp          =0.0    ! condenser inlet temperature
  REAL(r64)                    :: CondOutletTemp         =0.0    ! condenser outlet temperature
  REAL(r64)                    :: PlantInletTemp         =0.0    ! plant inlet temperature
  REAL(r64)                    :: PlantOutletTemp        =0.0    ! plant outlet temperature
  REAL(r64)                    :: CondMassFlowRate       =0.0    ! condenser mass flow rate
  REAL(r64)                    :: PlantMassFlowRate      =0.0    ! plant mass flow rate
  REAL(r64)                    :: HeatTransRate          =0.0    ! total heat transfer rate, Watts
  REAL(r64)                    :: HeatTransEnergy        =0.0    ! total heat transfer energy, Joules
  REAL(r64)                    :: CurntCapacity          =0.d0    ! capacity for current temps and flows
  REAL(r64)                    :: Effectiveness          =0.d0    ! heat exchange effectiveness

  !loop topology variables
  INTEGER                      :: CondLoopNum            =0 ! condenser side plant loop number
  INTEGER                      :: CondLoopSideNum        =0 ! condenser side plant loop side number
  INTEGER                      :: CondBranchNum          =0 ! condenser side plant loop branch number
  INTEGER                      :: CondCompNum            =0 ! condenser side plant component number
  INTEGER                      :: PlantLoopNum            =0 ! plant side plant loop number
  INTEGER                      :: PlantLoopSideNum        =0 ! plant side plant loop side number
  INTEGER                      :: PlantBranchNum          =0 ! plant side plant loop branch number
  INTEGER                      :: PlantCompNum            =0 ! plant side plant component number

END TYPE HXData

TYPE(HXData), DIMENSION(:), ALLOCATABLE :: HXWaterEcon

  ! MODULE VARIABLE DECLARATIONS:

INTEGER :: NumOfHX                =0    ! Number of Economizer heat exchangers
INTEGER :: CondInletNodeNum       =0    ! module variable for condenser side inlet node number
INTEGER :: CondOutletNodeNum      =0    ! module variable for condenser side outlet node number
INTEGER :: PlantInletNodeNum      =0    ! module variable for plant side inlet node number
INTEGER :: PlantOutletNodeNum     =0    ! module variable for plant side outlet node number

REAL(r64)    :: PlantMassFlowRate=0.0   ! Flow rate of Plant side fluid
REAL(r64)    :: CondMassFlowRate =0.0   ! Flow rate of Condenser side fluid
REAL(r64)    :: AvailSchedValue

LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName
LOGICAL  :: GetInputFlag = .TRUE.    ! First time, input is "gotten"

  ! SUBROUTINE SPECIFICATIONS FOR MODULE PlantOutsideHX

PUBLIC  SimEconHeatExchanger
PRIVATE GetEconHeatExchanger
PRIVATE SizeEconHeatExchanger
PRIVATE InitEconHeatExchanger
PRIVATE EconomizerOperation
PRIVATE CalcEconHeatExchanger
PRIVATE UpdateEconHeatExchanger
PRIVATE ReportEconHeatExchanger

CONTAINS

!==============================================================================

SUBROUTINE SimEconHeatExchanger(LoopNum,CompType,CompName,CompFlowCtrl,CompIndex,RunFlag, &
                                InitLoopEquip,Demand,MaxLoad, MinLoad, OptLoad, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Sankaranarayanan K P
          !       DATE WRITTEN   August 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the public interface to this Economizer
          ! heat exchanger component. Control operation and requirement
          ! for coupling the plant side and condenser side are checked here.
          ! Other calcs are made by calling private routines.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY : FindItemInList
  USE DataEnvironment, ONLY : OutDryBulbTemp, OutWetBulbTemp
  USE ScheduleManager, ONLY : GetCurrentScheduleValue
  USE DataLoopNode,    ONLY : Node
  USE DataHVACGlobals, ONLY : NumPlantLoops,NumCondLoops
  USE PlantUtilities,  ONLY : UpdateChillerComponentCondenserSide

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: CompType 
  CHARACTER(len=*), INTENT(IN)  :: CompName            ! name of the Economizer heat exchanger.
  INTEGER,          INTENT(IN)  :: CompFlowCtrl
  INTEGER,       INTENT(INOUT)  :: CompIndex         ! index in local derived types
  LOGICAL,       INTENT(INOUT)  :: RunFlag       ! TRUE if Component is ON
  LOGICAL,          INTENT(IN)  :: InitLoopEquip       ! for init
  REAL(r64),        INTENT(IN)  :: Demand
  REAL(r64),     INTENT(INOUT)  :: MaxLoad
  REAL(r64),     INTENT(INOUT)  :: MinLoad
  REAL(r64),     INTENT(INOUT)  :: OptLoad
  LOGICAL,          INTENT(IN)  :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep
  INTEGER,          INTENT(IN)  :: LoopNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER       :: PlantLoopNum
  INTEGER       :: HXNum         ! index in local derived types

          ! check for input
  IF (GetInputFlag) THEN
    CALL GetEconHeatExchanger
    GetInputFlag=.FALSE.
  ENDIF

  ! Find the correct Equipment
  IF (CompIndex == 0) THEN
    HXNum = FindItemInList(CompName,HXWaterEcon%Name,NumOfHX)
    IF (HXNum == 0) THEN
      CALL ShowFatalError('SimEconHeatExchanger: Unit not found='//TRIM(CompName))
    ENDIF
    CompIndex=HXNum
  ELSE
    HXNum=CompIndex
    IF (HXNum > NumOfHX .or. HXNum < 1) THEN
      CALL ShowFatalError('SimEconHeatExchanger:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(HXNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumOfHX))//  &
                          ', Entered Unit name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(HXNum)) THEN
      IF (CompName /= HXWaterEcon(HXNum)%Name) THEN
        CALL ShowFatalError('SimEconHeatExchanger: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(HXNum))// &
                            ', Unit name='//TRIM(CompName)//', stored Unit Name for that index='//  &
                            TRIM(HXWaterEcon(HXNum)%Name))
      ENDIF
      CheckEquipName(HXNum)=.false.
    ENDIF
  ENDIF

  IF (InitLoopEquip) THEN
    IF(.NOT. ALLOCATED(EconOn)) ALLOCATE(EconOn(TotNumLoops)) 
    HXWaterEcon(HXNum)%AssociatedPlantLoop =LoopNum
    PlantLoopNum = HXWaterEcon(HXNum)%AssociatedPlantLoop
    PlantLoop(PlantLoopNum)%EconomizerHtExchanger = HXWaterEcon(HXNum)%Name
    PlantLoop(PlantLoopNum)%EconPlantSideSensedNodeNum = HXWaterEcon(HXNum)%PlantInletNodeNum
    PlantLoop(PlantLoopNum)%EconCondSideSensedNodeNum = HXWaterEcon(HXNum)%CondInletNodeNum
    PlantLoop(PlantLoopNum)%EconControlTempDiff = HXWaterEcon(HXNum)%TempDiff
    CALL InitEconHeatExchanger(HXNum,FirstHVACIteration, RunFlag)
    CALL SizeEconHeatExchanger(HXNum)
    IF (LoopNum == HXWaterEcon(HXNum)%PlantLoopNum) THEN
      MinLoad = 0.0
      OptLoad = HXWaterEcon(HXNum)%DesCapacity
      MaxLoad = HXWaterEcon(HXNum)%DesCapacity * 1.25d0
    ELSEIF (LoopNum == HXWaterEcon(HXNum)%CondLoopNum) THEN 
      MinLoad = 0.d0
      OptLoad = 0.d0
      MaxLoad = 0.d0
    ENDIF
    RETURN
  ENDIF

  ! Initialize economizer: ScanPlantLoops, Interconnect, InitComponentNodes, and reset module variables


  IF(LoopNum == HXWaterEcon(HXNum)%PlantLoopNum) THEN
  
    CALL InitEconHeatExchanger(HXNum,FirstHVACIteration, RunFlag)
    
   ! Determine whether the economizer should turn on for the current conditions
    CALL EconomizerOperation(HXNum,RunFlag,Demand)  

   ! SetComponentFlowRate and calculate heat transfer
    CALL CalcEconHeatExchanger(HXNum,RunFlag)

   ! update nodes
    CALL UpdateEconHeatExchanger(HXNum)
   
   ! update report variables
    CALL ReportEconHeatExchanger(HXNum)

  ELSEIF (LoopNum == HXWaterEcon(HXNum)%CondLoopNum) THEN 

    ! update condenser side
    CALL UpdateChillerComponentCondenserSide(HXWaterEcon(HXNum)%CondLoopNum, &
                                     HXWaterEcon(HXNum)%CondLoopSideNum,     &
                                     TypeOf_WaterSideEconHtExchg,            &
                                     HXWaterEcon(HXNum)%CondInletNodeNum,    &
                                     HXWaterEcon(HXNum)%CondOutletNodeNum,   &
                                     HXWaterEcon(HXNum)%HeatTransRate,    &
                                     HXWaterEcon(HXNum)%CondInletTemp,       &
                                     HXWaterEcon(HXNum)%CondOutletTemp,      &
                                     HXWaterEcon(HXNum)%CondMassFlowRate,    &
                                     FirstHVACIteration)

  END IF

 RETURN

END SUBROUTINE SimEconHeatExchanger

!==============================================================================

SUBROUTINE GetEconHeatExchanger

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Sankaranarayanan K P
          !       DATE WRITTEN   August 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reads the input for Economizer heat exchangers
          ! from the user input file.  This will contain all of the information
          ! needed to define and simulate the heat exchanger. Some input data
          ! checking is done, and report variables set up.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,     ONLY : GetNumObjectsFound, GetObjectItem, VerifyName, FindItemInList, SameString
  USE DataIPShortCuts
  USE NodeInputManager,   ONLY : GetOnlySingleNode
  USE ScheduleManager,    ONLY : GetScheduleIndex
  USE BranchNodeConnections, ONLY : TestCompSet
  USE FluidProperties,    ONLY : CheckFluidPropertyName
  USE OutAirNodeManager,  ONLY: CheckAndAddAirNodeNumber
  USE DataLoopNode
  USE General,            ONLY: RoundSigDigits

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

  INTEGER                        :: IOStatus   ! Used in GetObjectItem
  INTEGER                        :: Item       ! Item to be "gotten"
  INTEGER                        :: NumAlphas  ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: NumFluids  ! number of fluids in sim.
  LOGICAL                        :: ErrorsFound  ! Set to true if errors in input, fatal at end of routine
  LOGICAL                        :: IsNotOK    ! Used to validate component
  LOGICAL                        :: IsBlank    ! Used to validate component
!  LOGICAL                        :: Okay

          ! Initializations and allocations
  cCurrentModuleObject = cThisModuleObjectName
  NumOfHX = GetNumObjectsFound(TRIM(cCurrentModuleObject))       ! 'HeatExchanger:WatersideEconomizer'

  ALLOCATE(HXWaterEcon(NumOfHX))
  ALLOCATE(CheckEquipName(NumOfHX))
  CheckEquipName=.true.

  ErrorsFound=.false.

          ! Obtain all of the user data
  DO Item = 1, NumOfHX

    CALL GetObjectItem(TRIM(cCurrentModuleObject),Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus, &
                    AlphaBlank=lAlphaFieldBlanks, &
                    AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)


          ! General user input data
    HXWaterEcon(Item)%Name = cAlphaArgs(1)
    IsNotOK = .FALSE.
    IsBlank = .FALSE.
    CALL VerifyName(cAlphaArgs(1),HXWaterEcon%Name,Item-1, &
                    IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound = .TRUE.
      IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
    END IF

    HXWaterEcon(Item)%HXType = cAlphaArgs(2)

    IF(HXWaterEcon(Item)%HXType == 'PLATEFRAME') THEN
      HXWaterEcon(Item)%HXTypeOf = PlateFrame
    ELSE IF(HXWaterEcon(Item)%HXType == 'COUNTERFLOW') THEN
      HXWaterEcon(Item)%HXTypeOf = CounterFlow
    ELSE IF(HXWaterEcon(Item)%HXType == 'PARALLELFLOW') THEN
      HXWaterEcon(Item)%HXTypeOf = ParallelFlow
    ELSE IF(HXWaterEcon(Item)%HXType == 'IDEAL') THEN
      HXWaterEcon(Item)%HXTypeOf = Ideal
    ELSE
      CALL ShowWarningError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    END IF

    HXWaterEcon(Item)%Schedule = cAlphaArgs(3)
    HXWaterEcon(Item)%ScheduleNum = GetScheduleIndex(HXWaterEcon(Item)%Schedule)
    IF (HXWaterEcon(Item)%ScheduleNum == 0) THEN
      CALL ShowWarningError('Invalid '//TRIM(cAlphaFieldNames(3))//'='//TRIM(cAlphaArgs(3)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF

       ! get Plant side inlet node data
    HXWaterEcon(Item)%PlantInletNode = cAlphaArgs(4)
    HXWaterEcon(Item)%PlantInletNodeNum  = GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)
    IF (HXWaterEcon(Item)%PlantInletNodeNum == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(cAlphaArgs(4)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Inlet node was not found.')
      ErrorsFound=.true.
    END IF

        ! get Plant side outlet node data
    HXWaterEcon(Item)%PlantOutletNode = cAlphaArgs(5)
    HXWaterEcon(Item)%PlantOutletNodeNum  = GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    IF (HXWaterEcon(Item)%PlantOutletNodeNum == 0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cAlphaFieldNames(5))//'='//TRIM(cAlphaArgs(5)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Outlet node was not found.')
      ErrorsFound=.true.
    END IF

    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(4),cAlphaArgs(5),'Water Nodes')

    HXWaterEcon(Item)%CondInletNodeNum    = GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,  &
             TRIM(cCurrentModuleObject),cAlphaArgs(1), &
             NodeType_Water,NodeConnectionType_Inlet, 2, ObjectIsNotParent)
    HXWaterEcon(Item)%CondOutletNodeNum  = GetOnlySingleNode(cAlphaArgs(7),ErrorsFound,  &
             TRIM(cCurrentModuleObject),cAlphaArgs(1), &
             NodeType_Water,NodeConnectionType_Outlet, 2, ObjectIsNotParent)

    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(6),cAlphaArgs(7),'Condenser Water Nodes')

    !Condenser Inlet node name is necessary for Water Cooled
    IF (lAlphaFieldBlanks(6) .or.  lAlphaFieldBlanks(7) ) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Condenser Inlet or Outlet Node Name is blank,='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    ENDIF

        ! Flow Rate data
    HXWaterEcon(Item)%CondSideFlowRate = rNumericArgs(1)
    HXWaterEcon(Item)%PlantSideFlowRate = rNumericArgs(2)

        ! UA effectiveness data
    HXWaterEcon(Item)%UA = rNumericArgs(3)

    IF (rNumericArgs(3) == 0.0) THEN
      CALL ShowSevereError('Invalid '//TRIM(cNumericFieldNames(3))//'='//TRIM(RoundSigDigits(rNumericArgs(3),2)))
      CALL ShowContinueError('Entered in '//TRIM(cCurrentModuleObject)//'='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('UA must be greater than zero.')
      ErrorsFound=.true.
    END IF

    HXWaterEcon(Item)%TempDiff = rNumericArgs(4)

    IF(HXWaterEcon(Item)%TempDiff ==0.0) HXWaterEcon(Item)%TempDiff = 0.001d0


  END DO  ! end of input loop

          ! Set up the output variables
  DO Item = 1, NumOfHX

    ! heat transfer CurrentModuleObject='HeatExchanger:WatersideEconomizer'
    CALL SetupOutputVariable('Economizer Heat Exchanger Heat Transfer Rate [W]',    &
                              HXWaterEcon(Item)%HeatTransRate,'System','Average', &
                              HXWaterEcon(Item)%Name)
    CALL SetupOutputVariable('Economizer Heat Exchanger Heat Transfer Energy [J]',    &
                              HXWaterEcon(Item)%HeatTransEnergy,'System','Sum', HXWaterEcon(Item)%Name, &
                              ResourceTypeKey='ENERGYTRANSFER',EndUseKey='FREECOOLING',GroupKey='Plant')

    ! flow rates
    CALL SetupOutputVariable('Economizer Heat Exchanger Condenser Side Mass Flow rate [kg/s]',      &
                              HXWaterEcon(Item)%CondMassFlowRate,'System','Average', &
                              HXWaterEcon(Item)%Name)
    CALL SetupOutputVariable('Economizer Heat Exchanger Plant Side Mass Flow rate [kg/s]',      &
                              HXWaterEcon(Item)%PlantMassFlowRate,'System','Average', &
                              HXWaterEcon(Item)%Name)
    ! condenser side temps
    CALL SetupOutputVariable('Economizer Heat Exchanger Condenser Side Inlet Temp [C]',     &
                              HXWaterEcon(Item)%CondInletTemp,'System','Average', &
                              HXWaterEcon(Item)%Name)
    CALL SetupOutputVariable('Economizer Heat Exchanger Condenser Side Outlet Temp [C]',     &
                              HXWaterEcon(Item)%CondOutletTemp,'System','Average', &
                              HXWaterEcon(Item)%Name)
    ! plant side temps
    CALL SetupOutputVariable('Economizer Heat Exchanger Plant Side Inlet Temp [C]',     &
                              HXWaterEcon(Item)%PlantInletTemp,'System','Average', &
                              HXWaterEcon(Item)%Name)
    CALL SetupOutputVariable('Economizer Heat Exchanger Plant Side Outlet Temp [C]',     &
                              HXWaterEcon(Item)%PlantOutletTemp,'System','Average', &
                              HXWaterEcon(Item)%Name)
  END DO


  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in processing input for '//TRIM(cCurrentModuleObject) )
  END IF

  RETURN

END SUBROUTINE GetEconHeatExchanger

!==============================================================================

SUBROUTINE SizeEconHeatExchanger(HXNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Sankaranarayanan K P
          !       DATE WRITTEN   August 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reads the input for Economizer heat exchangers
          ! from the user input file.  This will contain all of the information
          ! needed to define and simulate the heat exchanger. Some input data
          ! checking is done, and report variables set up.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant
  USE DataHVACGlobals,     ONLY: SmallWaterVolFlow
  USE DataInterfaces,      ONLY: ShowFatalError, ShowSevereError, ShowContinueError
  USE PlantUtilities,      ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE OutputReportPredefined
  USE FluidProperties,     ONLY: GetDensityGlycol, GetSpecificHeatGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: HXNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PltSizIndex   ! Plant Sizing Do loop index
  INTEGER             :: PltSizNum     ! Plant Sizing index corresponding to CurLoopNum
  INTEGER             :: PltSizCondNum ! Plant Sizing index for condenser loop
  INTEGER             :: CurrentLoopNum
  LOGICAL             :: ErrorsFound   ! If errors detected in input
  CHARACTER(len=MaxNameLength) :: equipName
  REAL(r64)           :: rho
  REAL(r64)           :: Cp
  REAL(r64)           :: tmpCondSideFlowRate
  REAL(r64)           :: tmpPlantSideFlowRate
  REAL(r64)           :: tmpUA
  REAL(r64)           :: tmpDesCap
  REAL(r64)           :: AssumedDeltaT

  ErrorsFound=.false.
  tmpCondSideFlowRate = HXWaterEcon(HXNum)%CondSideFlowRate
  tmpPlantSideFlowRate = HXWaterEcon(HXNum)%PlantSideFlowRate
  tmpUA = HXWaterEcon(HXNum)%UA
  tmpDesCap = HXWaterEcon(HXNum)%DesCapacity
  
  PltSizCondNum = PlantLoop(HXWaterEcon(HXNum)%CondLoopNum)%PlantSizNum
  PltSizNum     = PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%PlantSizNum
  
  PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%EconomizerHtExchanger      = HXWaterEcon(HXNum)%Name
  PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%EconPlantSideSensedNodeNum = HXWaterEcon(HXNum)%PlantInletNodeNum
  PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%EconCondSideSensedNodeNum  = HXWaterEcon(HXNum)%CondInletNodeNum
  PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%EconControlTempDiff        = HXWaterEcon(HXNum)%TempDiff

  HXWaterEcon(HXNum)%AssociatedPlantLoop = HXWaterEcon(HXNum)%PlantLoopNum

  IF (HXWaterEcon(HXNum)%PlantSideFlowRate == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        tmpPlantSideFlowRate = PlantSizData(PltSizNum)%DesVolFlowRate
        IF (PlantSizesOkayToFinalize) HXWaterEcon(HXNum)%PlantSideFlowRate = tmpPlantSideFlowRate
      ELSE
        tmpPlantSideFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) HXWaterEcon(HXNum)%PlantSideFlowRate = tmpPlantSideFlowRate
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(cThisModuleObjectName, HXWaterEcon(HXNum)%Name, &
                              'Chilled Water Side Maximum Flow Rate [m3/s]', &
                              HXWaterEcon(HXNum)%PlantSideFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of Water to Water heat Exchanger plant flow rate requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Water to water heat exchanger object='//TRIM(HXWaterEcon(HXNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

  CALL RegisterPlantCompDesignFlow(HXWaterEcon(HXNum)%PlantInletNodeNum,tmpPlantSideFlowRate)

  IF (PltSizNum > 0) THEN
    IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN

      Cp = GetSpecificHeatGlycol(PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%FluidName, &
                                 InitConvTemp, &
                                 PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%FluidIndex, &
                                 'SizeEconHeatExchanger' )

      rho = GetDensityGlycol(PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%FluidName, &
                                 InitConvTemp, &
                                 PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%FluidIndex, &
                                 'SizeEconHeatExchanger' )

      tmpDesCap = Cp * rho * PlantSizData(PltSizNum)%DeltaT &
                                                * PlantSizData(PltSizNum)%DesVolFlowRate
      IF (PlantSizesOkayToFinalize) HXWaterEcon(HXNum)%DesCapacity = tmpDesCap

    END IF
  END IF

  IF (HXWaterEcon(HXNum)%UA  == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN


        Cp = GetSpecificHeatGlycol(PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%FluidIndex, &
                                   'SizeEconHeatExchanger' )

        rho = GetDensityGlycol(PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%FluidIndex, &
                                   'SizeEconHeatExchanger' )

        tmpDesCap = Cp * rho * PlantSizData(PltSizNum)%DeltaT &
                                                    * PlantSizData(PltSizNum)%DesVolFlowRate
        IF (PlantSizesOkayToFinalize) HXWaterEcon(HXNum)%DesCapacity = tmpDesCap
        tmpUA = tmpDesCap/PlantSizData(PltSizNum)%DeltaT 
        IF (PlantSizesOkayToFinalize) HXWaterEcon(HXNum)%UA = tmpUA
      ELSE
        tmpUA = 0.d0
        IF (PlantSizesOkayToFinalize) HXWaterEcon(HXNum)%UA = tmpUA
      END IF
      IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(cThisModuleObjectName, HXWaterEcon(HXNum)%Name, &
                              'Heat Exchanger U-Factor Times Area Value [W/C]', HXWaterEcon(HXNum)%UA)
    ELSE
      CALL ShowSevereError('Autosizing of Water to Water heat Exchanger UA requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Water to water heat exchanger object='//TRIM(HXWaterEcon(HXNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

   IF (HXWaterEcon(HXNum)%CondSideFlowRate == AutoSize) THEN
    IF (PltSizCondNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN


        Cp = GetSpecificHeatGlycol(PlantLoop(HXWaterEcon(HXNum)%CondLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(HXWaterEcon(HXNum)%CondLoopNum)%FluidIndex, &
                                   'SizeEconHeatExchanger' )

        rho = GetDensityGlycol(PlantLoop(HXWaterEcon(HXNum)%CondLoopNum)%FluidName, &
                                   InitConvTemp, &
                                   PlantLoop(HXWaterEcon(HXNum)%CondLoopNum)%FluidIndex, &
                                   'SizeEconHeatExchanger' )
        tmpCondSideFlowRate = HXWaterEcon(HXNum)%DesCapacity / &
                                (PlantSizData(PltSizCondNum)%DeltaT * Cp * rho)
        IF (PlantSizesOkayToFinalize) HXWaterEcon(HXNum)%CondSideFlowRate = tmpCondSideFlowRate
      ELSE
        tmpCondSideFlowRate = 0.d0
        IF (PlantSizesOkayToFinalize) HXWaterEcon(HXNum)%CondSideFlowRate = tmpCondSideFlowRate
      END IF
        IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput(cThisModuleObjectName, HXWaterEcon(HXNum)%Name, &
                              'Condenser Side Maximum Flow Rate [m3/s]', &
                              tmpCondSideFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of Economizer Heat Exchanger condenser flow rate requires a condenser')
      CALL ShowContinueError('loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Economizer Heat Exchanger object='//TRIM(HXWaterEcon(HXNum)%Name))
      ErrorsFound = .TRUE.
    END IF
   END IF
   CALL RegisterPlantCompDesignFlow(HXWaterEcon(HXNum)%CondInletNodeNum,tmpCondSideFlowRate)

  IF (tmpDesCap == 0.d0) THEN ! not autosized but still need a design capacity from inputs
    AssumedDeltaT = 2.d0
    Cp = GetSpecificHeatGlycol(PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%FluidName, &
                               InitConvTemp, &
                               PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%FluidIndex, &
                               'SizeEconHeatExchanger' )

    rho = GetDensityGlycol(PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%FluidName, &
                               InitConvTemp, &
                               PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%FluidIndex, &
                               'SizeEconHeatExchanger' )
    tmpDesCap = Cp * rho * AssumedDeltaT  * tmpPlantSideFlowRate
    IF (PlantSizesOkayToFinalize) HXWaterEcon(HXNum)%DesCapacity = tmpDesCap
  ENDIF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  IF (PlantSizesOkayToFinalize) THEN
    !create predefined report
    equipName = HXWaterEcon(HXNum)%Name
    CALL PreDefTableEntry(pdchMechType,equipName,cThisModuleObjectName)
  !  CALL PreDefTableEntry(pdchMechNomUA,equipName,HXWaterEcon(HXNum)%UA)
    CALL PreDefTableEntry(pdchMechNomCap,equipName,HXWaterEcon(HXNum)%DesCapacity)
  ENDIF

  RETURN

END SUBROUTINE SizeEconHeatExchanger

SUBROUTINE InitEconHeatExchanger(HXNum,FirstHVACIteration, RunFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Sankaranarayanan K P
          !       DATE WRITTEN   August 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine just sets some local variables used to store
          ! node numbers for current component.

          ! METHODOLOGY EMPLOYED:
          ! Reads Economizer HX component data structure

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  USE DataHVACGlobals, ONLY : NumPlantLoops, NumCondLoops
  USE DataLoopNode,    ONLY : Node
  USE ScheduleManager, ONLY : GetCurrentScheduleValue
  USE DataPlant,       ONLY : TypeOf_WaterSideEconHtExchg, DemandSide, SupplySide, &
                              PlantSizesOkayToFinalize, PlantSizeNotComplete
  USE PlantUtilities,  ONLY : InterConnectTwoPlantLoopSides, InitComponentNodes
  USE FluidProperties, ONLY : GetDensityGlycol, GetSpecificHeatGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

  INTEGER, INTENT(IN) :: HXNum             ! Index for the Economizer heat exchanger.
  LOGICAL, INTENT(IN) :: FirstHVACIteration !unused1208
  LOGICAL, INTENT(IN) :: RunFlag !unused1208

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER             :: Num                     ! counter
  REAL(r64)           :: DesignPlantMassFlowRate  ! Mass flow rate used to initialize plant nodes
  REAL(r64)           :: DesignCondMassFlowRate  ! Mass flow rate used to initialize condenser nodes
!  REAL(r64)           :: CondInletdensity    ! density on condenser side
  LOGICAL, SAVE       :: MyEnvrnFlag=.true.
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFlag
  LOGICAL, SAVE                            :: MyOneTimeFlag = .TRUE.
  LOGICAL :: errFlag
  REAL(r64) :: rhoCond   ! local fluid density for condenser side
  REAL(r64) :: rhoPlant  ! local fluid density for plant side
  REAL(r64) :: PlantFluidCp 
  REAL(r64) :: CondFluidCp
  REAL(r64) :: PlantCapRate
  REAL(r64) :: CondCapRate

  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyFlag(NumOfHX))
    MyOneTimeFlag = .false.
    MyFlag = .TRUE.
  END IF

  ! Init more variables
  IF (MyFlag(HXNum)) THEN
    ! Locate the hx on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(HXWaterEcon(HXNum)%Name, &
                                 TypeOf_WaterSideEconHtExchg, &
                                 HXWaterEcon(HXNum)%CondLoopNum, &
                                 HXWaterEcon(HXNum)%CondLoopSideNum, &
                                 HXWaterEcon(HXNum)%CondBranchNum, &
                                 HXWaterEcon(HXNum)%CondCompNum, &
                                 InletNodeNumber = HXWaterEcon(HXNum)%CondInletNodeNum,  &
                                 errFlag=errFlag)
    CALL ScanPlantLoopsForObject(HXWaterEcon(HXNum)%Name, &
                                 TypeOf_WaterSideEconHtExchg, &
                                 HXWaterEcon(HXNum)%PlantLoopNum, &
                                 HXWaterEcon(HXNum)%PlantLoopSideNum, &
                                 HXWaterEcon(HXNum)%PlantBranchNum, &
                                 HXWaterEcon(HXNum)%PlantCompNum, &
                                 InletNodeNumber = HXWaterEcon(HXNum)%PlantInletNodeNum,  &
                                 errFlag=errFlag)
    !now test if HX is nominally connected to the proper loop side CR 8276
    IF (HXWaterEcon(HXNum)%CondLoopSideNum /= DemandSide) THEN ! throw error
      CALL ShowSevereError('Invalid connections for '// cThisModuleObjectName //' = ' //HXWaterEcon(HXNum)%name )
      CALL ShowContinueError(' The condenser side of component is not connected to the demand side of the loop')
      errFlag = .TRUE.
    ENDIF
    
    IF (HXWaterEcon(HXNum)%PlantLoopSideNum /= SupplySide) THEN !throw error
      CALL ShowSevereError('Invalid connections for '// cThisModuleObjectName //' = ' //HXWaterEcon(HXNum)%name )
      CALL ShowContinueError(' The plant side of component is not connected to the supply side of the loop')
      errFlag = .TRUE.
    ENDIF
    
    CALL InterConnectTwoPlantLoopSides( HXWaterEcon(HXNum)%CondLoopNum,      &
                                        HXWaterEcon(HXNum)%CondLoopSideNum,  &
                                        HXWaterEcon(HXNum)%PlantLoopNum,     &
                                        HXWaterEcon(HXNum)%PlantLoopSideNum, &
                                        TypeOf_WaterSideEconHtExchg, .FALSE. )
    IF (errFlag) THEN
      CALL ShowFatalError('InitHeatExchanger: Program terminated due to previous condition(s).')
    ENDIF
    MyFlag(HXNum)=.FALSE.
  ENDIF

  IF (BeginEnvrnFlag .and. MyEnvrnFlag .AND. (PlantSizesOkayToFinalize)) THEN  

    DO Num = 1, NumOfHX
      IF (PlantSizeNotComplete)  CALL SizeEconHeatExchanger(HXNum)
      rhoPlant = GetDensityGlycol(PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%FluidName, &
                                  InitConvTemp, &
                                  PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%FluidIndex, &
                                  'InitHeatExchanger')
      rhoCond  = GetDensityGlycol(PlantLoop(HXWaterEcon(HXNum)%CondLoopNum)%FluidName, &
                                  InitConvTemp, &
                                  PlantLoop(HXWaterEcon(HXNum)%CondLoopNum)%FluidIndex, &
                                  'InitHeatExchanger')

      DesignPlantMassFlowRate = HXWaterEcon(Num)%PlantSideFlowRate * rhoPlant
      HXWaterEcon(Num)%PlantSideDesMassFlowRate = DesignPlantMassFlowRate
      DesignCondMassFlowRate  = HXWaterEcon(Num)%CondSideFlowRate  * rhoCond
      HXWaterEcon(Num)%CondSideDesMassFlowRate = DesignCondMassFlowRate

      CALL InitComponentNodes(0.0D0,DesignPlantMassFlowRate,  &
                              HXWaterEcon(Num)%PlantInletNodeNum,        &
                              HXWaterEcon(Num)%PlantOutletNodeNum,       &
                              HXWaterEcon(Num)%PlantLoopNum,               &
                              HXWaterEcon(Num)%PlantLoopSideNum,           &
                              HXWaterEcon(Num)%PlantBranchNum,             &
                              HXWaterEcon(Num)%PlantCompNum)

      CALL InitComponentNodes(0.0D0,DesignCondMassFlowRate,  &
                              HXWaterEcon(Num)%CondInletNodeNum,        &
                              HXWaterEcon(Num)%CondOutletNodeNum,       &
                              HXWaterEcon(Num)%CondLoopNum,               &
                              HXWaterEcon(Num)%CondLoopSideNum,           &
                              HXWaterEcon(Num)%CondBranchNum,             &
                              HXWaterEcon(Num)%CondCompNum)

      PlantFluidCp   = GetSpecificHeatGlycol(PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%FluidName, InitConvTemp, &
                                         PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%FluidIndex,'InitHeatExchanger')
      PlantCapRate = PlantFluidCp * DesignPlantMassFlowRate
      CondFluidCp    = GetSpecificHeatGlycol(PlantLoop(HXWaterEcon(HXNum)%CondLoopNum)%FluidName,InitConvTemp, &
                                             PlantLoop(HXWaterEcon(HXNum)%CondLoopNum)%FluidIndex,'InitHeatExchanger')
      CondCapRate  = CondFluidCp * DesignCondMassFlowRate
      HXWaterEcon(HXNum)%MinSideDesCpMassFlux  = MIN(CondCapRate, PlantCapRate)
      
    END DO
    MyEnvrnFlag = .FALSE.
  END IF
  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag=.true.
  ENDIF


  AvailSchedValue = GetCurrentScheduleValue(HXWaterEcon(HXNum)%ScheduleNum)

  !  set module variables for node numbers for this heat exchanger
  CondInletNodeNum   = HXWaterEcon(HXNum)%CondInletNodeNum
  CondOutletNodeNum  = HXWaterEcon(HXNum)%CondOutletNodeNum
  PlantInletNodeNum  = HXWaterEcon(HXNum)%PlantInletNodeNum
  PlantOutletNodeNum = HXWaterEcon(HXNum)%PlantOutletNodeNum
  HXWaterEcon(HXNum)%HeatTransRate = 0.0
  HXWaterEcon(HXNum)%HeatTransEnergy = 0.0

  EconLoadMet = 0.0

  RETURN

END SUBROUTINE InitEconHeatExchanger

SUBROUTINE EconomizerOperation(HXNum,RunFlag,LoopDemand)

   ! SUBROUTINE INFORMATION:
   !       AUTHOR         Sankaranarayanan K P
   !       DATE WRITTEN   August 2007
   !       MODIFIED       na
   !       RE-ENGINEERED  na
   !
   ! PURPOSE OF THIS SUBROUTINE:
   ! This subroutine places necessary call to the heat exchanger module. The branch and component number
   ! for the heat exchanger are saved in arrays EconBranch and EconComp. These numbers are used directly
   ! so that looping over all branches and components is avoided .
   !
   ! METHODOLOGY EMPLOYED:
   ! Standard EnergyPlus methodology.
   !
   ! USE STATEMENTS:
 USE DataPlant
 USE DataGlobals
 USE DataInterfaces
 USE DataLoopNode,  ONLY: Node

 IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

   ! SUBROUTINE ARGUMENT DEFINITIONS:
 INTEGER, INTENT(IN)  :: HXNum
 LOGICAL, INTENT(OUT) :: RunFlag  
 REAL(r64), INTENT(IN):: LoopDemand           

   ! SUBROUTINE PARAMETER DEFINITIONS:
   ! na

   ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
 INTEGER     :: LoopNum   
 INTEGER     :: LoopSideNum   
 INTEGER     :: EconBranch
 INTEGER     :: EconComp
 INTEGER     :: EquipNum       ! Plant side component list equipment number
 INTEGER     :: EquipTypeNum
 INTEGER     :: GeneralEquipType !Basic Equipment type from EquipType Used to
 INTEGER     :: PlantSideSensedNode      !Node num for control node on plant side
 INTEGER     :: CondSideSensedNode       !Node num for control node on condenser side

 REAL(r64)   :: PlantSideControlTemp             !Current temperature on PlantSideSensedNode
 REAL(r64)   :: CondSideControlTemp              !Current temperature on CondSideSensedNode
 REAL(r64)   :: ControlTemp                      !Difference between PlantSideControlTemp and CondSideControlTemp
 REAL(r64)   :: EconControlTempDiff              !User specified control treshold

 CHARACTER(len=MaxNameLength) :: EquipType !local equipment type
 CHARACTER(len=MaxNameLength) :: EquipName ! local equipment name
 LOGICAL    :: EconomizerOn
 
 LoopNum = HXWaterEcon(HXNum)%PlantLoopNum
 LoopSideNum = HXWaterEcon(HXNum)%PlantLoopSideNum

 PlantSideSensedNode = PlantLoop(LoopNum)%EconPlantSideSensedNodeNum
 CondSideSensedNode = PlantLoop(LoopNum)%EconCondSideSensedNodeNum

 PlantSideControlTemp = Node(PlantSideSensedNode)%Temp
 CondSideControlTemp = Node(CondSideSensedNode)%Temp

 ControlTemp = PlantSideControlTemp - CondSideControlTemp
 EconControlTempDiff = PlantLoop(LoopNum)%EconControlTempDiff
 EconOn(LoopNum) = .FALSE.
 IF(ControlTemp .GT. EconControlTempDiff) THEN
  EconOn(LoopNum) = .TRUE.
 END IF
 IF(LoopDemand .GE. 0.0) EconOn(LoopNum) = .FALSE.

 EconomizerOn = EconOn(LoopNum)
 RunFlag = EconomizerOn

 RETURN

END SUBROUTINE EconomizerOperation

!==============================================================================

SUBROUTINE CalcEconHeatExchanger(HXNum,RunFlag)

          !       AUTHOR         Sankaranarayanan K P
          !       DATE WRITTEN   August 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This calculates the total heat transfer rate between the
          ! two loop fluids streams. This heat transfer rate is used
          ! in the update routine to calc the node temps.

          ! METHODOLOGY EMPLOYED:
          ! NTU-effectiveness heat exchanger model. Effectiveness is
          ! calculated from the user supplied UA value. If 'Ideal' mode
          ! has been set, effectiveness is set to 1.0.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant
  USE DataLoopNode,    ONLY : Node
  USE FluidProperties, ONLY : GetSpecificHeatGlycol, GetDensityGlycol
  USE PlantUtilities,  ONLY : SetComponentFlowRate, PullCompInterconnectTrigger

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: HXNum          ! Index for the Economizer heat exchanger.
  LOGICAL, INTENT(IN)  :: RunFlag        ! TRUE if Component is ON

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: CalledFrom='PlantHeatExchanger:Calc'

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: PlantFluidCp        ! Specific heat of Plant side fluid
  REAL(r64)    :: PlantCapRate        ! Capacity rate (mdot*Cp) of Plant side fluid
  REAL(r64)    :: PlantInletTemp      ! Plant side inlet temperature
  REAL(r64)    :: CondFluidCp         ! Specific heat of condenser side fluid
  REAL(r64)    :: PlantInletdensity   ! density on plant side
  REAL(r64)    :: CondInletDensity    ! density on cond side
  REAL(r64)    :: CondCapRate         ! Capacity rate (mdot*Cp) of condenser side fluid
  REAL(r64)    :: CondInletTemp       ! condenser side inlet temperature
  REAL(r64)    :: MinCapRate          ! minimum capacity rate
  REAL(r64)    :: CapRatio            ! capacity ratio (min/max)
  REAL(r64)    :: Effectiveness       ! heat exchanger effectiveness
  REAL(r64)    :: NTU                 ! dimensionless NTU calculated from UA
  REAL(r64)    :: ChillerLoad         ! current load on chiller (Myload)
  LOGICAL      :: ItemNotFound        ! error flag

  INTEGER :: PlantLoopNum
  INTEGER :: LoopSideNum

  ItemNotFound = .FALSE.

  PlantInletdensity = GetDensityGlycol(PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%FluidName, &
                                       Node(HXWaterEcon(HXNum)%PlantInletNodeNum)%Temp, &
                                       PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%FluidIndex, &
                                       'CalcEconHeatExchanger')

  CondInletDensity = GetDensityGlycol(PlantLoop(HXWaterEcon(HXNum)%CondLoopNum)%FluidName, &
                                       Node(HXWaterEcon(HXNum)%CondInletNodeNum)%Temp, &
                                       PlantLoop(HXWaterEcon(HXNum)%CondLoopNum)%FluidIndex, &
                                       'CalcEconHeatExchanger')

  PlantLoopNum = HXWaterEcon(HXNum)%AssociatedPlantLoop
  LoopSideNum = HXWaterEcon(HXNum)%PlantLoopSideNum

  IF(.NOT. RunFlag .OR. AvailSchedValue == 0.0) THEN ! just try to turn off and RETURN
   CondMassFlowRate = 0.0
   PlantMassFlowRate = 0.0
   Call SetComponentFlowRate(PlantMassFlowRate,  &
                            PlantInletNodeNum , PlantOutletNodeNum  , &
                            HXWaterEcon(HXNum)%PlantLoopNum,     &
                            HXWaterEcon(HXNum)%PlantLoopSideNum, &
                            HXWaterEcon(HXNum)%PlantBranchNum, &
                            HXWaterEcon(HXNum)%PlantCompNum)

   Call SetComponentFlowRate(CondMassFlowRate,  &
                            CondInletNodeNum , CondOutletNodeNum  , &
                            HXWaterEcon(HXNum)%CondLoopNum, &
                            HXWaterEcon(HXNum)%CondLoopSideNum, &
                            HXWaterEcon(HXNum)%CondBranchNum, &
                            HXWaterEcon(HXNum)%CondCompNum)
   RETURN
  END IF

  ! if we didn't return, then we should try to turn on and run, first calculate desired flows
  PlantMassFlowRate = HXWaterEcon(HXNum)%PlantSideFlowRate*PlantInletdensity*AvailSchedValue
  CondMassFlowRate = HXWaterEcon(HXNum)%CondSideFlowRate * CondInletDensity*AvailSchedValue

  ! then use worker routines to determine if we can get this amount
  Call SetComponentFlowRate(PlantMassFlowRate,  &
                            PlantInletNodeNum , PlantOutletNodeNum  , &
                            HXWaterEcon(HXNum)%PlantLoopNum,     &
                            HXWaterEcon(HXNum)%PlantLoopSideNum, &
                            HXWaterEcon(HXNum)%PlantBranchNum, &
                            HXWaterEcon(HXNum)%PlantCompNum)
  Call SetComponentFlowRate(CondMassFlowRate,  &
                            CondInletNodeNum , CondOutletNodeNum  , &
                            HXWaterEcon(HXNum)%CondLoopNum, &
                            HXWaterEcon(HXNum)%CondLoopSideNum, &
                            HXWaterEcon(HXNum)%CondBranchNum, &
                            HXWaterEcon(HXNum)%CondCompNum)

    !set local variables for heat exchanger calculation
  CondInletTemp     = Node(CondInletNodeNum)%Temp
  PlantInletTemp    = Node(PlantInletNodeNum)%Temp
  PlantFluidCp   = GetSpecificHeatGlycol(PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%FluidName, PlantInletTemp, &
                                         PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%FluidIndex,CalledFrom)
  PlantCapRate = PlantFluidCp * PlantMassFlowRate
  CondFluidCp    = GetSpecificHeatGlycol(PlantLoop(HXWaterEcon(HXNum)%CondLoopNum)%FluidName,CondInletTemp, &
                                         PlantLoop(HXWaterEcon(HXNum)%CondLoopNum)%FluidIndex,CalledFrom)
  CondCapRate  = CondFluidCp * CondMassFlowRate
  MinCapRate = MIN(CondCapRate, PlantCapRate)

    !If there is no flow rate on either the condenser or plant side, try to turn off heat exchanger and return
  IF (CondCapRate <= zerocaptol .OR. PlantCapRate <= zerocaptol)THEN
    HXWaterEcon(HXNum)%HeatTransRate = 0.0
    
   CondMassFlowRate = 0.0
   PlantMassFlowRate = 0.0
   Call SetComponentFlowRate(PlantMassFlowRate,  &
                            PlantInletNodeNum , PlantOutletNodeNum  , &
                            HXWaterEcon(HXNum)%PlantLoopNum,     &
                            HXWaterEcon(HXNum)%PlantLoopSideNum, &
                            HXWaterEcon(HXNum)%PlantBranchNum, &
                            HXWaterEcon(HXNum)%PlantCompNum)

   Call SetComponentFlowRate(CondMassFlowRate,  &
                            CondInletNodeNum , CondOutletNodeNum  , &
                            HXWaterEcon(HXNum)%CondLoopNum, &
                            HXWaterEcon(HXNum)%CondLoopSideNum, &
                            HXWaterEcon(HXNum)%CondBranchNum, &
                            HXWaterEcon(HXNum)%CondCompNum)

    RETURN
  END IF

  ! calc effectiveness - 1.0 if in ideal mode
  IF(HXWaterEcon(HXNum)%HXTypeOf == PlateFrame)THEN
    ! assume cross flow, both mixed
   NTU = HXWaterEcon(HXNum)%UA/MinCapRate
   IF(CondCapRate == BigNum .OR. PlantCapRate ==  BigNum) THEN
    CapRatio = 0.0
    IF (-NTU >= EXP_LowerLimit) THEN
      Effectiveness = 1.0-EXP(-NTU)
      Effectiveness = MIN(1.0d0,Effectiveness)
    ELSE
      Effectiveness = 1.0
    ENDIF
   ELSE
    CapRatio = MinCapRate/MAX(CondCapRate, PlantCapRate)
    Effectiveness = 1.0d0 - EXP((NTU**0.22d0/CapRatio) * &
                (EXP(-CapRatio*NTU**0.78d0) - 1.0d0))
    Effectiveness = MIN(1.0d0,Effectiveness)
   END IF
  ELSE IF(HXWaterEcon(HXNum)%HXTypeOf == ParallelFlow)THEN
    ! assume cross flow, both mixed
   NTU = HXWaterEcon(HXNum)%UA/MinCapRate
   IF(CondCapRate == BigNum .OR. PlantCapRate ==  BigNum) THEN
    CapRatio = 0.0
    Effectiveness = 1.d0-EXP(-NTU)
    Effectiveness = MIN(1.0d0,Effectiveness)
   ELSE
    CapRatio = MinCapRate/MAX(CondCapRate, PlantCapRate)
    Effectiveness = (1.d0-EXP(-NTU*(1.d0+CapRatio)))/(1.d0+CapRatio)
    Effectiveness = MIN(1.0d0,Effectiveness)
   END IF
  ELSE IF(HXWaterEcon(HXNum)%HXTypeOf == CounterFlow)THEN
    ! assume cross flow, both mixed
   NTU = HXWaterEcon(HXNum)%UA/MinCapRate
   IF(CondCapRate == BigNum .OR. PlantCapRate ==  BigNum) THEN
    CapRatio = 0.0
    Effectiveness = 1.d0-EXP(-NTU)
    Effectiveness = MIN(1.0d0,Effectiveness)
   ELSE
    CapRatio = MinCapRate/MAX(CondCapRate, PlantCapRate)
    Effectiveness = (1.d0-EXP(-NTU*(1.d0-CapRatio)))/(1.d0-CapRatio*EXP(-NTU*(1.d0-CapRatio)))
    Effectiveness = MIN(1.0d0,Effectiveness)
   END IF
  ELSE IF(HXWaterEcon(HXNum)%HXTypeOf == Ideal) THEN
    ! must be in ideal mode
    Effectiveness = 1.0
  END IF
  ! overall heat transfer rate
  ! convention is +ve rate is rejected from plant side to condenser side
  HXWaterEcon(HXNum)%HeatTransRate = Effectiveness*MinCapRate*(PlantInletTemp-CondInletTemp)
  HXWaterEcon(HXNum)%Effectiveness = Effectiveness
  RETURN

END SUBROUTINE CalcEconHeatExchanger

!==============================================================================

SUBROUTINE UpdateEconHeatExchanger(HXNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Sankaranarayanan K P
          !       DATE WRITTEN   August 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine takes the inlet conditions and the previously
          ! calculated heat transfer rate to calculate the outlet temperatures.
          ! All flow rates are passed through. If the loops are not coupled
          ! All node info is passed through from inlet to outlet

          ! METHODOLOGY EMPLOYED:
          ! use previously calcultated heat transfer rate and update node data.

          ! USE STATEMENTS:
  USE DataLoopNode,    ONLY : Node
  USE FluidProperties, ONLY : GetSpecificHeatGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: HXNum  ! Index for the Economizer heat exchanger.

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: CondFluidCp         ! Specific heat of condenser side fluid
  REAL(r64)    :: PlantFluidCp        ! Specific heat of Plant side fluid
  REAL(r64)    :: CondInletTemp       ! condenser side inlet temperature
  REAL(r64)    :: PlantInletTemp      ! Plant side inlet temperature


  ! inlet temps and specific heats
  PlantInletTemp = Node(PlantInletNodeNum)%Temp
  CondInletTemp  = Node(CondInletNodeNum)%Temp
  PlantFluidCp   = GetSpecificHeatGlycol(PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%FluidName, PlantInletTemp, &
                                         PlantLoop(HXWaterEcon(HXNum)%PlantLoopNum)%FluidIndex,'UpdateEconHeatExchanger')
  CondFluidCp    = GetSpecificHeatGlycol(PlantLoop(HXWaterEcon(HXNum)%CondLoopNum)%FluidName,CondInletTemp, &
                                         PlantLoop(HXWaterEcon(HXNum)%CondLoopNum)%FluidIndex,'UpdateEconHeatExchanger')

  ! check if coupled or zero heat transfer rate
  IF(HXWaterEcon(HXNum)%HeatTransRate /= 0.0) THEN
    ! calc outlet temps from heat transfer rate
   IF(CondMassFlowRate .NE. 0.0) &
    Node(CondOutletNodeNum)%Temp = Node(CondInletNodeNum)%Temp + HXWaterEcon(HXNum)%HeatTransRate/ &
                                                           (CondMassFlowRate * CondFluidCp)

   IF(PlantMassFlowRate .NE. 0.0) &
    Node(PlantOutletNodeNum)%Temp = Node(PlantInletNodeNum)%Temp - HXWaterEcon(HXNum)%HeatTransRate/ &
                                                            (PlantMassFlowRate * PlantFluidCp)
  ELSE
    ! just pass through
    Node(CondOutletNodeNum)%Temp  = Node(CondInletNodeNum)%Temp
    Node(PlantOutletNodeNum)%Temp = Node(PlantInletNodeNum)%Temp
  END IF

  EconLoadMet = HXWaterEcon(HXNum)%HeatTransRate

  RETURN

END SUBROUTINE UpdateEconHeatExchanger

!==============================================================================

SUBROUTINE ReportEconHeatExchanger(HXNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Sankaranarayanan K P
          !       DATE WRITTEN   August 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Updates the Economizer heat exchanger variables used for reporting.

          ! METHODOLOGY EMPLOYED:
          ! Update variables from node data.

          ! USE STATEMENTS:
  USE DataLoopNode,    ONLY : Node
  USE DataHVACGlobals, ONLY : TimeStepSys

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: HXNum  ! Index for the Economizer heat exchanger.

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  ! update condenser side variables
  HXWaterEcon(HXNum)%CondInletTemp     = Node(CondInletNodeNum)%Temp
  HXWaterEcon(HXNum)%CondOutletTemp    = Node(CondOutletNodeNum)%Temp
  HXWaterEcon(HXNum)%CondMassFlowRate  = CondMassFlowRate   

  ! update plant side variables
  HXWaterEcon(HXNum)%PlantInletTemp    = Node(PlantInletNodeNum)%Temp
  HXWaterEcon(HXNum)%PlantOutletTemp   = Node(PlantOutletNodeNum)%Temp
  HXWaterEcon(HXNum)%PlantMassFlowRate = PlantMassFlowRate  

  ! update the energy reporting variable
  HXWaterEcon(HXNum)%HeatTransEnergy   = HXWaterEcon(HXNum)%HeatTransRate*TimeStepSys*SecInHour

  RETURN

END SUBROUTINE ReportEconHeatExchanger

!     NOTICE
!
!     Copyright © 1996-2012 The Board of Trustees of the University of Illinois
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

END MODULE EconomizerHeatExchanger
