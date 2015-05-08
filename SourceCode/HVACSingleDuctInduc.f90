MODULE HVACSingleDuctInduc

          ! Module containing routines dealing terminal 4 pipe induction terminal units

          ! MODULE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   June 15 2004
          !       MODIFIED       Brent Griffith, Sept 2010, plant upgrades, fluid props
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! To encapsulate the data and algorithms needed to simulate 4 pipe induction terminal units

          ! METHODOLOGY EMPLOYED:
          ! The terminal boxes are modeled as compound components: heating coil, cooling coil and
          ! mixer. The combined components are controlled to meet the zone load.

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals,     ONLY: BeginEnvrnFlag, MaxNameLength, NumOfZones, &
                           InitConvTemp, SysSizingCalc, ScheduleAlwaysOn, DisplayExtraWarnings
USE DataInterfaces
Use DataEnvironment, ONLY: StdBaroPress, StdRhoAir
          ! Use statements for access to subroutines in other modules
USE ScheduleManager
USE Psychrometrics,  ONLY: PsyRhoAirFnPbTdbW,PsyCpAirFnWTdb,PsyHFnTdbW
USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad, SmallAirVolFlow

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: SingleDuct_CV_FourPipeInduc=1
INTEGER, PARAMETER :: SingleDuct_CV_2PipeInduc=2
          ! DERIVED TYPE DEFINITIONS:
TYPE IndUnitData
  ! input data
  CHARACTER(len=MaxNameLength) :: Name                =' ' ! name of unit
  CHARACTER(len=MaxNameLength) :: UnitType            =' ' ! type of unit
  INTEGER                      :: UnitType_Num        =0   ! index to type of unit
  CHARACTER(len=MaxNameLength) :: Sched               =' ' ! availability schedule
  INTEGER                      :: SchedPtr            =0   ! index to schedule
  REAL(r64)                    :: MaxTotAirVolFlow    =0.0d0 ! m3/s (autosizable)
  REAL(r64)                    :: MaxTotAirMassFlow   =0.0d0 ! kg/s
  REAL(r64)                    :: InducRatio          =2.5d0 ! ratio of induced air flow to primary air flow
  INTEGER                      :: PriAirInNode        =0   ! unit primary air inlet node number
  INTEGER                      :: SecAirInNode        =0   ! unit induced air inlet node number
  INTEGER                      :: OutAirNode          =0   ! unit air outlet node number
  INTEGER                      :: HWControlNode       =0   ! hot water control node
  INTEGER                      :: CWControlNode       =0   ! cold water control node
  CHARACTER(len=MaxNameLength) :: HCoilType           =' ' ! type of heating coil component
  CHARACTER(len=MaxNameLength) :: HCoil               =' ' ! name of heating coil component
  INTEGER                      :: HCoil_Num           =0   ! index to this coil
  INTEGER                      :: HCoil_PlantTypeNum  =0   !
  REAL(r64)                    :: MaxVolHotWaterFlow  =0.0d0 ! m3/s (autosizable)
  REAL(r64)                    :: MaxHotWaterFlow     =0.0d0 ! kg/s
  REAL(r64)                    :: MinVolHotWaterFlow  =0.0d0 ! m3/s
  REAL(r64)                    :: MinHotWaterFlow     =0.0d0 ! kg/s
  REAL(r64)                    :: HotControlOffset    =0.0d0 ! control tolerance
  INTEGER                      :: HWLoopNum           =0   ! index for plant loop with hot water coil
  INTEGER                      :: HWLoopSide          =0   ! index for plant loop side for hot water coil
  INTEGER                      :: HWBranchNum         =0   ! index for plant branch for hot water coil
  INTEGER                      :: HWCompNum           =0   ! index for plant component for hot water coil
  INTEGER                      :: HWCoilFailNum1      =0   ! index for errors
  INTEGER                      :: HWCoilFailNum2      =0   ! index for errors


  CHARACTER(len=MaxNameLength) :: CCoilType           =' ' ! type of cooling coil component
  CHARACTER(len=MaxNameLength) :: CCoil               =' ' ! name of cooling coil component
  INTEGER                      :: CCoil_Num           =0   ! index to this coil
  INTEGER                      :: CCoil_PlantTypeNum  =0   !
  REAL(r64)                    :: MaxVolColdWaterFlow =0.0d0 ! m3/s (autosizable)
  REAL(r64)                    :: MaxColdWaterFlow    =0.0d0 ! kg/s
  REAL(r64)                    :: MinVolColdWaterFlow =0.0d0 ! m3/s
  REAL(r64)                    :: MinColdWaterFlow    =0.0d0 ! kg/s
  REAL(r64)                    :: ColdControlOffset   =0.0d0 ! control tolerance
  INTEGER                      :: CWLoopNum           =0   ! index for plant loop with chilled water coil
  INTEGER                      :: CWLoopSide          =0   ! index for plant loop side for chilled water coil
  INTEGER                      :: CWBranchNum         =0   ! index for plant branch for chilled water coil
  INTEGER                      :: CWCompNum           =0   ! index for plant component for chilled water coil
  INTEGER                      :: CWCoilFailNum1      =0   ! index for errors
  INTEGER                      :: CWCoilFailNum2      =0   ! index for errors


  CHARACTER(len=MaxNameLength) :: MixerName           =' ' ! name of air mixer component
  INTEGER                      :: Mixer_Num           =0   ! index to this mixer
  REAL(r64)                    :: MaxPriAirMassFlow   =0.0d0 ! kg/s
  REAL(r64)                    :: MaxSecAirMassFlow   =0.0d0 ! kg/s
  INTEGER                      :: ADUNum              =0   ! index of corresponding air distribution unit
  REAL(r64)                    :: DesCoolingLoad      = 0.0d0 ! used for reporting during coil sizing
  REAL(r64)                    :: DesHeatingLoad      = 0.0d0 ! used for reporting during coil sizing
END TYPE IndUnitData
          ! MODULE VARIABLE DECLARATIONS:
TYPE (IndUnitData), ALLOCATABLE, DIMENSION(:)         :: IndUnit

INTEGER :: NumIndUnits=0
INTEGER :: NumFourPipes=0
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName
LOGICAL :: GetIUInputFlag = .TRUE.  ! First time, input is "gotten"

          ! SUBROUTINE SPECIFICATIONS FOR MODULE HVACSingleDuctInduc:

PUBLIC  SimIndUnit
PRIVATE GetIndUnits
PRIVATE InitIndUnit
PRIVATE SizeIndUnit
PRIVATE SimFourPipeIndUnit
PRIVATE CalcFourPipeIndUnit
PUBLIC  FourPipeInductionUnitHasMixer
! PRIVATE UpdateIndUnit
! PRIVATE ReportIndUnit

CONTAINS

SUBROUTINE SimIndUnit(CompName, FirstHVACIteration, ZoneNum, ZoneNodeNum, CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   June 18 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Manages the simulation of a passive (no fan) induction terminal unit.
          ! Called from SimZoneAirLoopEquipment in module ZoneAirLoopEquipmentManager.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE DataSizing, ONLY: TermUnitIU
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompName    ! name of the terminal unit
  LOGICAL, INTENT (IN)         :: FirstHVACIteration ! TRUE if first HVAC iteration in time step
  INTEGER, INTENT (IN)         :: ZoneNum     ! index of zone served by the terminal unit
  INTEGER, INTENT (IN)         :: ZoneNodeNum ! zone node number of zone served by the terminal unit
  INTEGER, INTENT (INOUT)      :: CompIndex   ! which terminal unit in data structure

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: IUNum                  ! index of terminal unit being simulated

  ! First time SimIndUnit is called, get the input for all the passive terminal induction units
  IF (GetIUInputFlag) THEN
    CALL GetIndUnits
    GetIUInputFlag = .FALSE.
  END IF

  ! Get the induction unit index
  IF (CompIndex == 0) THEN
    IUNum = FindItemInList(CompName,IndUnit%Name,NumIndUnits)
    IF (IUNum == 0) THEN
      CALL ShowFatalError('SimIndUnit: Induction Unit not found='//TRIM(CompName))
    ENDIF
    CompIndex=IUNum
  ELSE
    IUNum=CompIndex
    IF (IUNum > NumIndUnits .or. IUNum < 1) THEN
      CALL ShowFatalError('SimIndUnit: Invalid CompIndex passed='//TRIM(TrimSigDigits(CompIndex))// &
                          ', Number of Induction Units='//TRIM(TrimSigDigits(NumIndUnits))//        &
                          ', System name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(IUNum)) THEN
      IF (CompName /= IndUnit(IUNum)%Name) THEN
        CALL ShowFatalError('SimIndUnit: Invalid CompIndex passed='//TRIM(TrimSigDigits(CompIndex))//      &
                            ', Induction Unit name='//TRIM(CompName)//', stored Induction Unit for that index='//  &
                            TRIM(IndUnit(IUNum)%Name))
      ENDIF
      CheckEquipName(IUNum)=.false.
    ENDIF
  ENDIF

  ! initialize the unit
  CALL InitIndUnit(IUNum,FirstHVACIteration)

  TermUnitIU = .TRUE.

  ! Select the correct unit type
  SELECT CASE(IndUnit(IUNum)%UnitType_Num)

    CASE (SingleDuct_CV_FourPipeInduc)

      CALL SimFourPipeIndUnit(IUNum,ZoneNum,ZoneNodeNum,FirstHVACIteration)

    CASE DEFAULT
      CALL ShowSevereError('Illegal Induction Unit Type used='//TRIM(IndUnit(IUNum)%UnitType))
      CALL ShowContinueError('Occurs in Induction Unit='//TRIM(IndUnit(IUNum)%Name))
      CALL ShowFatalError('Preceding condition causes termination.')

  END SELECT

  TermUnitIU = .FALSE.

  ! the tasks usually done by the Update and Report routines are not required in a compound terminal unit.

  ! Update the current unit's outlet nodes. No update needed
  ! CALL UpdateIndUnit(IUNum)

  ! Fill the report variables. There are no report variables
  ! CALL ReportIndUnit(IUNum)

  RETURN


END SUBROUTINE SimIndUnit

SUBROUTINE GetIndUnits

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   June 15 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for passive induction air terminal units and stores it in the
          ! induction terminal unit data structures

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,    ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString, GetObjectDefMaxArgs
  USE NodeInputManager,  ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet, SetUpCompSets
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE DataSizing
  USE DataDefineEquip,   ONLY: AirDistUnit, NumAirDistUnits
  USE WaterCoils,        ONLY: GetCoilWaterInletNode
  USE DataIPShortCuts
  USE DataPlant,         ONLY: TypeOf_CoilWaterSimpleHeating, TypeOf_CoilWaterCooling, TypeOf_CoilWaterDetailedFlatCooling

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER (len=*), PARAMETER   :: RoutineName='GetIndUnits '              ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  INTEGER                        :: IUIndex ! loop index
  INTEGER                        :: IUNum   ! current fan coil number
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject       ! for ease in getting objects
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas         ! Alpha input items for object
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers           ! Numeric input items for object
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.
  INTEGER :: NumAlphas=0  ! Number of Alphas for each GetObjectItem call
  INTEGER :: NumNumbers=0 ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: TotalArgs=0             ! Total number of alpha and numeric arguments (max) for a
                                                            !  certain object in the input file
  INTEGER :: IOStatus     ! Used in GetObjectItem
  LOGICAL :: ErrorsFound=.false.  ! Set to true if errors in input, fatal at end of routine
  LOGICAL :: IsNotOK              ! Flag to verify name
  LOGICAL :: IsBlank              ! Flag for blank name
  INTEGER :: CtrlZone             ! controlled zome do loop index
  INTEGER :: SupAirIn             ! controlled zone supply air inlet index
  LOGICAL :: AirNodeFound
  INTEGER :: ADUNum

  ! find the number of each type of induction unit
  CurrentModuleObject = 'AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction'
  NumFourPipes = GetNumObjectsFound(CurrentModuleObject)
  NumIndUnits = NumFourPipes
  ! allocate the data structures
  ALLOCATE(IndUnit(NumIndUnits))
  ALLOCATE(CheckEquipName(NumIndUnits))
  CheckEquipName=.true.

  CALL GetObjectDefMaxArgs(CurrentModuleObject,TotalArgs,NumAlphas,NumNumbers)

  ALLOCATE(Alphas(NumAlphas))
  Alphas=' '
  ALLOCATE(cAlphaFields(NumAlphas))
  cAlphaFields=' '
  ALLOCATE(cNumericFields(NumNumbers))
  cNumericFields=' '
  ALLOCATE(Numbers(NumNumbers))
  Numbers=0.0d0
  ALLOCATE(lAlphaBlanks(NumAlphas))
  lAlphaBlanks=.true.
  ALLOCATE(lNumericBlanks(NumNumbers))
  lNumericBlanks=.true.

  ! loop over Series PIUs; get and load the input data
  DO IUIndex = 1,NumFourPipes

    CALL GetObjectItem(CurrentModuleObject,IUIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

    IUNum = IUIndex
    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(Alphas(1),IndUnit%Name,IUNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    IndUnit(IUNum)%Name = Alphas(1)
    IndUnit(IUNum)%UnitType = TRIM(CurrentModuleObject)
    IndUnit(IUNum)%UnitType_Num = SingleDuct_CV_FourPipeInduc
    IndUnit(IUNum)%Sched = Alphas(2)
    IF (lAlphaBlanks(2)) THEN
      IndUnit(IUNum)%SchedPtr = ScheduleAlwaysOn
    ELSE
      IndUnit(IUNum)%SchedPtr = GetScheduleIndex(Alphas(2))  ! convert schedule name to pointer
      IF (IndUnit(IUNum)%SchedPtr .EQ. 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': invalid '//TRIM(cAlphaFields(2))//  &
             ' entered ='//TRIM(Alphas(2))// &
             ' for '//TRIM(cAlphaFields(1))//'='//TRIM(Alphas(1)))
        ErrorsFound=.TRUE.
      END IF
    END IF
    IndUnit(IUNum)%MaxTotAirVolFlow = Numbers(1)
    IndUnit(IUNum)%InducRatio       = Numbers(2)
    IF (lNumericBlanks(2)) IndUnit(IUNum)%InducRatio = 2.5d0

    IndUnit(IUNum)%PriAirInNode = &
      GetOnlySingleNode(Alphas(3),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                        NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent,cAlphaFields(3))
    IndUnit(IUNum)%SecAirInNode = &
      GetOnlySingleNode(Alphas(4),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                        NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent,cAlphaFields(4))
    IndUnit(IUNum)%OutAirNode = &
      GetOnlySingleNode(Alphas(5),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                        NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent,cAlphaFields(5))

    IndUnit(IUNum)%HCoilType = Alphas(8) ! type (key) of heating coil
    IF (SameString(IndUnit(IUNum)%HCoilType,'Coil:Heating:Water')) THEN
      IndUnit(IUNum)%HCoil_PlantTypeNum = TypeOf_CoilWaterSimpleHeating
    ENDIF

    IndUnit(IUNum)%HCoil     = Alphas(9) ! name of heating coil object
    IsNotOK=.false.
    IndUnit(IUNum)%HWControlNode = GetCoilWaterInletNode(IndUnit(IUNum)%HCoilType,IndUnit(IUNum)%HCoil,IsNotOK)
    IF (IsNotOK) THEN
      CALL ShowContinueError('In '//TRIM(CurrentModuleObject)//' = '//TRIM(IndUnit(IUNum)%Name))
      CALL ShowContinueError('..Only Coil:Heating:Water is allowed.')
      ErrorsFound=.true.
    ENDIF
!      GetOnlySingleNode(Alphas(6),ErrorsFound,'AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction',Alphas(1), &
!                        NodeType_Water,NodeConnectionType_Actuator,1,ObjectIsParent)
    IndUnit(IUNum)%MaxVolHotWaterFlow = Numbers(3)
    IndUnit(IUNum)%MinVolHotWaterFlow = Numbers(4)
    IndUnit(IUNum)%HotControlOffset   = Numbers(5)

    IndUnit(IUNum)%CCoilType = Alphas(10) ! type (key) of cooling coil

    IF (SameString(IndUnit(IUNum)%CCoilType , 'Coil:Cooling:Water')) THEN
      IndUnit(IUNum)%CCoil_PlantTypeNum =  TypeOf_CoilWaterCooling
    ELSEIF (SameString(IndUnit(IUNum)%CCoilType , 'Coil:Cooling:Water:DetailedGeometry')) THEN
      IndUnit(IUNum)%CCoil_PlantTypeNum =  TypeOf_CoilWaterDetailedFlatCooling
    ENDIF

    IndUnit(IUNum)%CCoil     = Alphas(11) ! name of cooling coil object
    IsNotOK=.false.
    IndUnit(IUNum)%CWControlNode = GetCoilWaterInletNode(IndUnit(IUNum)%CCoilType,IndUnit(IUNum)%CCoil,IsNotOK)
    IF (IsNotOK) THEN
      CALL ShowContinueError('In '//TRIM(CurrentModuleObject)//' = '//TRIM(IndUnit(IUNum)%Name))
      CALL ShowContinueError('..Only Coil:Cooling:Water or Coil:Cooling:Water:DetailedGeometry is allowed.')
      ErrorsFound=.true.
    ENDIF
!      GetOnlySingleNode(Alphas(7),ErrorsFound,'AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction',Alphas(1), &
!                        NodeType_Water,NodeConnectionType_Actuator,1,ObjectIsParent)
    IndUnit(IUNum)%MaxVolColdWaterFlow = Numbers(6)
    IndUnit(IUNum)%MinVolColdWaterFlow = Numbers(7)
    IndUnit(IUNum)%ColdControlOffset   = Numbers(8)
    IndUnit(IUNum)%MixerName = Alphas(12)
    ! Add heating coil to component sets array
    CALL SetUpCompSets(IndUnit(IUNum)%UnitType, IndUnit(IUNum)%Name, IndUnit(IUNum)%HCoilType, &
                       IndUnit(IUNum)%HCoil, Alphas(4), 'UNDEFINED')
    ! Add cooling coil to component sets array
    CALL SetUpCompSets(IndUnit(IUNum)%UnitType, IndUnit(IUNum)%Name, IndUnit(IUNum)%CCoilType, &
                       IndUnit(IUNum)%CCoil, 'UNDEFINED', 'UNDEFINED')

    ! Register component set data
    CALL TestCompSet(IndUnit(IUNum)%UnitType,IndUnit(IUNum)%Name, &
                     NodeID(IndUnit(IUNum)%PriAirInNode),NodeID(IndUnit(IUNum)%OutAirNode),'Air Nodes')

    ! Fill the Zone Equipment data with the supply air inlet node number of this unit.
    AirNodeFound=.false.
    DO CtrlZone = 1,NumOfZones
      IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
      DO SupAirIn = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
        IF (IndUnit(IUNum)%OutAirNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(SupAirIn)) THEN
          IF (ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode > 0) THEN
            CALL ShowSevereError('Error in connecting a terminal unit to a zone')
            CALL ShowContinueError(TRIM(NodeID(IndUnit(IUNum)%OutAirNode))//' already connects to another zone')
            CALL ShowContinueError('Occurs for terminal unit '//TRIM(IndUnit(IUNum)%UnitType)//' = '//TRIM(IndUnit(IUNum)%Name))
            CALL ShowContinueError('Check terminal unit node names for errors')
            ErrorsFound = .true.
          ELSE
            ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%InNode = IndUnit(IUNum)%PriAirInNode
            ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode = IndUnit(IUNum)%OutAirNode
          END IF
          AirNodeFound=.true.
          ! save the induction ratio in the term unit sizing array for use in the system sizing calculation
          IF (ZoneSizingRunDone) THEN
            TermUnitSizing(CtrlZone)%InducRat = IndUnit(IUNum)%InducRatio
          END IF
          EXIT
        END IF
      END DO
    END DO
    IF (.not. AirNodeFound) THEN
      CALL ShowSevereError('The outlet air node from the '//TRIM(CurrentModuleObject)//' = ' &
                           //TRIM(IndUnit(IUNum)%Name))
      CALL ShowContinueError('did not have a matching Zone Equipment Inlet Node, Node ='//TRIM(Alphas(3)))
      ErrorsFound=.true.
    ENDIF

  END DO

  DO IUNum=1,NumIndUnits
    DO ADUNum = 1,NumAirDistUnits
      IF (IndUnit(IUNum)%OutAirNode == AirDistUnit(ADUNum)%OutletNodeNum) THEN
!        AirDistUnit(ADUNum)%InletNodeNum = IndUnitIUNum)%InletNodeNum
        IndUnit(IUNum)%ADUNum = ADUNum
      END IF
    END DO
    ! one assumes if there isn't one assigned, it's an error?
    IF (IndUnit(IUNum)%ADUNum == 0) THEN
      CALL ShowSevereError(RoutineName//'No matching Air Distribution Unit, for Unit = ['//  &
         TRIM(IndUnit(IUNum)%UnitType)//','//TRIM(IndUnit(IUNum)%Name)//'].')
      CALL ShowContinueError('...should have outlet node='//TRIM(NodeID(IndUnit(IUNum)%OutAirNode)))
!          ErrorsFound=.true.
    ENDIF
  END DO


  DEALLOCATE(Alphas)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(Numbers)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(lNumericBlanks)

  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Errors found in getting input. Preceding conditions cause termination.')
  END IF

  RETURN


END SUBROUTINE GetIndUnits

SUBROUTINE InitIndUnit(IUNum,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   June 21 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initialization of the passive induction
          ! terminal boxes

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment,  ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList
  USE DataDefineEquip,    ONLY: AirDistUnit
  USE InputProcessor,     ONLY: SameString
  USE DataPlant,          ONLY: PlantLoop, ScanPlantLoopsForObject, TypeOf_CoilWaterSimpleHeating, &
                                TypeOf_CoilWaterCooling, TypeOf_CoilWaterDetailedFlatCooling
  USE FluidProperties,    ONLY: GetDensityGlycol
  USE PlantUtilities,     ONLY: InitComponentNodes
  USE DataGlobals,        ONLY: AnyPlantInModel

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: IUNum ! number of the current induction unit being simulated
  LOGICAL, INTENT (IN) :: FirstHVACIteration ! TRUE if first air loop solution this HVAC step

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PriNode         ! primary air inlet node number
  INTEGER             :: SecNode         ! secondary air inlet node number
  INTEGER             :: OutletNode      ! unit air outlet node
  INTEGER             :: HotConNode      ! hot water control node number
  INTEGER             :: ColdConNode     ! cold water control node  number
  REAL(r64)           :: IndRat          ! unit induction ratio
  REAL(r64)           :: RhoAir          ! air density at outside pressure and standard temperature and humidity
  LOGICAL,SAVE             :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MySizeFlag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlag

  LOGICAL,SAVE        :: ZoneEquipmentListChecked = .false.  ! True after the Zone Equipment List has been checked for items
  Integer             :: Loop  ! Loop checking control variable
  REAL(r64)           :: rho ! local fluid density
  INTEGER  :: HWOutletNode ! local node index for hot water coil's outlet node
  INTEGER  :: CWOutletNode ! local node index for cold water coil's outlet node
  LOGICAL  :: errFlag

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN

    ALLOCATE(MyEnvrnFlag(NumIndUnits))
    ALLOCATE(MySizeFlag(NumIndUnits))
    ALLOCATE(MyPlantScanFlag(NumIndUnits))
    MyEnvrnFlag = .TRUE.
    MySizeFlag = .TRUE.
    MyPlantScanFlag   = .TRUE.
    MyOneTimeFlag = .FALSE.
  END IF

  IF (MyPlantScanFlag(IUNum) .AND. ALLOCATED(PlantLoop)) THEN
    IF (IndUnit(IUNum)%HCoil_PlantTypeNum == TypeOf_CoilWaterSimpleHeating) THEN
      errFlag=.false.
      CALL ScanPlantLoopsForObject( IndUnit(IUNum)%HCoil, &
                                          IndUnit(IUNum)%HCoil_PlantTypeNum, &
                                          IndUnit(IUNum)%HWLoopNum,   &
                                          IndUnit(IUNum)%HWLoopSide,  &
                                          IndUnit(IUNum)%HWBranchNum, &
                                          IndUnit(IUNum)%HWCompNum,          &
                                          errFlag=errFlag)
    ENDIF
    IF (errFlag) THEN
      CALL ShowContinueError('Reference Unit="'//trim(IndUnit(IUNum)%Name)//'", type='//trim(IndUnit(IUNum)%UnitType))
    ENDIF
    IF (IndUnit(IUNum)%CCoil_PlantTypeNum == TypeOf_CoilWaterCooling .OR. &
          IndUnit(IUNum)%CCoil_PlantTypeNum == TypeOf_CoilWaterDetailedFlatCooling ) THEN
      errFlag=.false.
      CALL ScanPlantLoopsForObject( IndUnit(IUNum)%CCoil, &
                                          IndUnit(IUNum)%CCoil_PlantTypeNum, &
                                          IndUnit(IUNum)%CWLoopNum,   &
                                          IndUnit(IUNum)%CWLoopSide,  &
                                          IndUnit(IUNum)%CWBranchNum, &
                                          IndUnit(IUNum)%CWCompNum,   &
                                          errFlag=errFlag)
    ENDIF
    IF (errFlag) THEN
      CALL ShowContinueError('Reference Unit="'//trim(IndUnit(IUNum)%Name)//'", type='//trim(IndUnit(IUNum)%UnitType))
      CALL ShowFatalError('InitIndUnit: Program terminated for previous conditions.')
    ENDIF
    MyPlantScanFlag(IUNum) = .FALSE.
  ELSEIF(MyPlantScanFlag(IUNum) .AND. .NOT. AnyPlantInModel  ) THEN
    MyPlantScanFlag(IUNum) = .FALSE.
  ENDIF

  IF (.not. ZoneEquipmentListChecked .and. ZoneEquipInputsFilled) THEN
    ZoneEquipmentListChecked=.true.
    ! Check to see if there is a Air Distribution Unit on the Zone Equipment List
    DO Loop=1,NumIndUnits
      IF (IndUnit(Loop)%ADUNum == 0) CYCLE
      IF (CheckZoneEquipmentList('ZONEHVAC:AIRDISTRIBUTIONUNIT',AirDistUnit(IndUnit(Loop)%ADUNum)%Name)) CYCLE
      CALL ShowSevereError('InitIndUnit: ADU=[Air Distribution Unit,'//  &
           TRIM(AirDistUnit(IndUnit(Loop)%ADUNum)%Name)//  &
           '] is not on any ZoneHVAC:EquipmentList.')
      CALL ShowContinueError('...Unit=['//TRIM(IndUnit(Loop)%UnitType)//','//TRIM(IndUnit(Loop)%Name)//  &
           '] will not be simulated.')
    ENDDO
  ENDIF

  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(IUNum) ) THEN

    CALL SizeIndUnit(IUNum)
    MySizeFlag(IUNum) = .FALSE.
  END IF

  ! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(IUNum)) THEN
    RhoAir = StdRhoAir
    PriNode = IndUnit(IUNum)%PriAirInNode
    SecNode = IndUnit(IUNum)%SecAirInNode
    OutletNode = IndUnit(IUNum)%OutAirNode
    IndRat =  IndUnit(IUNum)%InducRatio
    ! set the mass flow rates from the input volume flow rates
    IF (SameString(IndUnit(IUNum)%UnitType,'AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction')) THEN
      IndUnit(IUNum)%MaxTotAirMassFlow = RhoAir * IndUnit(IUNum)%MaxTotAirVolFlow
      IndUnit(IUNum)%MaxPriAirMassFlow = IndUnit(IUNum)%MaxTotAirMassFlow / (1.0d0+IndRat)
      IndUnit(IUNum)%MaxSecAirMassFlow = IndRat*IndUnit(IUNum)%MaxTotAirMassFlow / (1.0d0+IndRat)
      Node(PriNode)%MassFlowRateMax = IndUnit(IUNum)%MaxPriAirMassFlow
      Node(PriNode)%MassFlowRateMin = IndUnit(IUNum)%MaxPriAirMassFlow
      Node(SecNode)%MassFlowRateMax = IndUnit(IUNum)%MaxSecAirMassFlow
      Node(SecNode)%MassFlowRateMin = IndUnit(IUNum)%MaxSecAirMassFlow
      Node(OutletNode)%MassFlowRateMax = IndUnit(IUNum)%MaxTotAirMassFlow
    END IF

    HotConNode = IndUnit(IUNum)%HWControlNode
    IF (HotConNode.GT.0 .AND. .NOT. MyPlantScanFlag(IUNum)) THEN

      rho = GetDensityGlycol(PlantLoop(IndUnit(IUNum)%HWLoopNum)%FluidName,  &
                                  60.d0, &
                                  PlantLoop(IndUnit(IUNum)%HWLoopNum)%FluidIndex,&
                                  'InitIndUnit' )
      IndUnit(IUNum)%MaxHotWaterFlow = rho * IndUnit(IUNum)%MaxVolHotWaterFlow
      IndUnit(IUNum)%MinHotWaterFlow = rho * IndUnit(IUNum)%MinVolHotWaterFlow
      ! get component outlet node from plant structure
      HWOutletNode = PlantLoop(IndUnit(IUNum)%HWLoopNum)%LoopSide(IndUnit(IUNum)%HWLoopSide) &
                         %Branch(IndUnit(IUNum)%HWBranchNum)%Comp(IndUnit(IUNum)%HWCompNum)%NodeNumOut
      CALL InitComponentNodes(IndUnit(IUNum)%MinHotWaterFlow, IndUnit(IUNum)%MaxHotWaterFlow, &
                                            HotConNode, HWOutletNode, &
                                            IndUnit(IUNum)%HWLoopNum,   &
                                            IndUnit(IUNum)%HWLoopSide,  &
                                            IndUnit(IUNum)%HWBranchNum, &
                                            IndUnit(IUNum)%HWCompNum)
    END IF

    ColdConNode = IndUnit(IUNum)%CWControlNode
    IF (ColdConNode.GT.0) THEN
      rho = GetDensityGlycol(PlantLoop(IndUnit(IUNum)%CWLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(IndUnit(IUNum)%CWLoopNum)%FluidIndex,&
                                  'InitIndUnit' )
      IndUnit(IUNum)%MaxColdWaterFlow =  rho * IndUnit(IUNum)%MaxVolColdWaterFlow
      IndUnit(IUNum)%MinColdWaterFlow =  rho * IndUnit(IUNum)%MinVolColdWaterFlow

      CWOutletNode = PlantLoop(IndUnit(IUNum)%CWLoopNum)%LoopSide(IndUnit(IUNum)%CWLoopSide) &
                         %Branch(IndUnit(IUNum)%CWBranchNum)%Comp(IndUnit(IUNum)%CWCompNum)%NodeNumOut
      CALL InitComponentNodes(IndUnit(IUNum)%MinColdWaterFlow, IndUnit(IUNum)%MaxColdWaterFlow, &
                                            ColdConNode, CWOutletNode, &
                                            IndUnit(IUNum)%CWLoopNum,   &
                                            IndUnit(IUNum)%CWLoopSide,  &
                                            IndUnit(IUNum)%CWBranchNum, &
                                            IndUnit(IUNum)%CWCompNum)
    END IF

    MyEnvrnFlag(IUNum) = .FALSE.
  END IF ! end one time inits

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(IUNum) = .true.
  ENDIF

  PriNode = IndUnit(IUNum)%PriAirInNode
  SecNode = IndUnit(IUNum)%SecAirInNode

  ! Do the start of HVAC time step initializations
  IF (FirstHVACIteration) THEN
    ! check for upstream zero flow. If nonzero and schedule ON, set primary flow to max
    IF (GetCurrentScheduleValue(IndUnit(IUNum)%SchedPtr) .GT. 0.0d0 .AND. &
        Node(PriNode)%MassFlowRate .GT. 0.0d0) THEN
      IF (SameString(IndUnit(IUNum)%UnitType,'AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction')) THEN
        Node(PriNode)%MassFlowRate = IndUnit(IUNum)%MaxPriAirMassFlow
        Node(SecNode)%MassFlowRate = IndUnit(IUNum)%MaxSecAirMassFlow
      END IF
    ELSE
      Node(PriNode)%MassFlowRate = 0.0d0
      Node(SecNode)%MassFlowRate = 0.0d0
    END IF
    ! reset the max and min avail flows
    IF (GetCurrentScheduleValue(IndUnit(IUNum)%SchedPtr) .GT. 0.0d0 .AND. &
        Node(PriNode)%MassFlowRateMaxAvail .GT. 0.0d0) THEN
      IF (SameString(IndUnit(IUNum)%UnitType,'AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction')) THEN
        Node(PriNode)%MassFlowRateMaxAvail = IndUnit(IUNum)%MaxPriAirMassFlow
        Node(PriNode)%MassFlowRateMinAvail = IndUnit(IUNum)%MaxPriAirMassFlow
        Node(SecNode)%MassFlowRateMaxAvail = IndUnit(IUNum)%MaxSecAirMassFlow
        Node(SecNode)%MassFlowRateMinAvail = IndUnit(IUNum)%MaxSecAirMassFlow
      END IF
    ELSE
      Node(PriNode)%MassFlowRateMaxAvail = 0.0d0
      Node(PriNode)%MassFlowRateMinAvail = 0.0d0
      Node(SecNode)%MassFlowRateMaxAvail = 0.0d0
      Node(SecNode)%MassFlowRateMinAvail = 0.0d0
    END IF
  END IF

  RETURN

END SUBROUTINE InitIndUnit

SUBROUTINE SizeIndUnit(IUNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   June 22 2004
          !       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing induction terminal units for which flow rates have not been
          ! specified in the input

          ! METHODOLOGY EMPLOYED:
          ! Accesses zone sizing array for air flow rates and zone and plant sizing arrays to
          ! calculate coil water flow rates.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE InputProcessor
  USE WaterCoils,          ONLY: SetCoilDesFlow, GetCoilWaterInletNode, GetCoilWaterOutletNode
!  USE BranchInputManager,  ONLY: MyPlantSizingIndex
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE FluidProperties,     ONLY: GetDensityGlycol, GetSpecificHeatGlycol
  USE DataPlant,           ONLY: PlantLoop, MyPlantSizingIndex
  USE General,             ONLY: RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: IUNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PltSizHeatNum ! index of plant sizing object for 1st heating loop
  INTEGER             :: PltSizCoolNum ! index of plant sizing object for 1st cooling loop
  REAL(r64)           :: DesCoilLoad
  REAL(r64)           :: DesPriVolFlow
  REAL(r64)           :: RhoAir
  REAL(r64)           :: CpAir
  INTEGER             :: CoilWaterInletNode=0
  INTEGER             :: CoilWaterOutletNode=0
  LOGICAL             :: ErrorsFound
  REAL(r64)           :: Cp ! local fluid specific heat
  REAL(r64)           :: rho ! local fluid density
  LOGICAL             :: IsAutosize
  REAL(r64)           ::  MaxTotAirVolFlowDes     ! Desing size maximum air volume flow for reproting
  REAL(r64)           ::  MaxTotAirVolFlowUser    ! User hard-sized maximum air volume flow for reporting
  REAL(r64)           ::  MaxVolHotWaterFlowDes   ! Desing size maximum hot water flow for reproting
  REAL(r64)           ::  MaxVolHotWaterFlowUser  ! User hard-sized maximum hot water flow for reporting
  REAL(r64)           ::  MaxVolColdWaterFlowDes  ! Desing size maximum cold water flow for reproting
  REAL(r64)           ::  MaxVolColdWaterFlowUser ! User hard-sized maximum cold water flow for reporting

  PltSizHeatNum = 0
  PltSizCoolNum = 0
  DesPriVolFlow = 0.0d0
  CpAir = 0.0d0
  RhoAir = StdRhoAir
  ErrorsFound = .FALSE.
  IsAutosize = .FALSE.
  MaxTotAirVolFlowDes = 0.0d0
  MaxTotAirVolFlowUser = 0.0d0
  MaxVolHotWaterFlowDes = 0.0d0
  MaxVolHotWaterFlowUser = 0.0d0
  MaxVolColdWaterFlowDes = 0.0d0
  MaxVolColdWaterFlowUser = 0.0d0

  IF (IndUnit(IUNum)%MaxTotAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF

  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! simulation continue
      IF (IndUnit(IUNum)%MaxTotAirVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(IndUnit(IUNum)%UnitType, IndUnit(IUNum)%Name, &
                              'User-Specified Maximum Total Air Flow Rate [m3/s]', IndUnit(IUNum)%MaxTotAirVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(IndUnit(IUNum)%UnitType, IndUnit(IUNum)%Name)
      MaxTotAirVolFlowDes = MAX(TermUnitFinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                            TermUnitFinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
      IF (MaxTotAirVolFlowDes < SmallAirVolFlow) THEN
        MaxTotAirVolFlowDes = 0.0d0
      END IF
      IF (IsAutosize) THEN
        IndUnit(IUNum)%MaxTotAirVolFlow = MaxTotAirVolFlowDes
        CALL ReportSizingOutput(IndUnit(IUNum)%UnitType, IndUnit(IUNum)%Name, &
                              'Design Size Maximum Total Air Flow Rate [m3/s]', MaxTotAirVolFlowDes)
      ELSE
        IF (IndUnit(IUNum)%MaxTotAirVolFlow > 0.0d0 .AND. MaxTotAirVolFlowDes > 0.0d0) THEN
          MaxTotAirVolFlowUser = IndUnit(IUNum)%MaxTotAirVolFlow
          CALL ReportSizingOutput(IndUnit(IUNum)%UnitType, IndUnit(IUNum)%Name, &
                              'Design Size Maximum Total Air Flow Rate [m3/s]', MaxTotAirVolFlowDes, &
                              'User-Specified Maximum Total Air Flow Rate [m3/s]', MaxTotAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MaxTotAirVolFlowDes - MaxTotAirVolFlowUser)/MaxTotAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeHVACSingleDuctInduction: Potential issue with equipment sizing for ' &
                                    //  TRIM(IndUnit(IUNum)%UnitType)//' = "'//TRIM(IndUnit(IUNum)%Name)//'".')
              CALL ShowContinueError('User-Specified Maximum Total Air Flow Rate of '// &
                                    TRIM(RoundSigDigits(MaxTotAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Maximum Total Air Flow Rate of ' // &
                                    TRIM(RoundSigDigits(MaxTotAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (IndUnit(IUNum)%MaxVolHotWaterFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! simulation continue
      IF (IndUnit(IUNum)%MaxVolHotWaterFlow > 0.0d0) THEN
        CALL ReportSizingOutput(IndUnit(IUNum)%UnitType, IndUnit(IUNum)%Name, &
                          'User-Specified Maximum Hot Water Flow Rate [m3/s]', IndUnit(IUNum)%MaxVolHotWaterFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(IndUnit(IUNum)%UnitType, IndUnit(IUNum)%Name)

      IF (SameString(IndUnit(IUNum)%HCoilType,'Coil:Heating:Water')) THEN

        CoilWaterInletNode = GetCoilWaterInletNode('Coil:Heating:Water',IndUnit(IUNum)%HCoil,ErrorsFound)
        CoilWaterOutletNode = GetCoilWaterOutletNode('Coil:Heating:Water',IndUnit(IUNum)%HCoil,ErrorsFound)
        IF (IsAutosize) THEN
          PltSizHeatNum = MyPlantSizingIndex('Coil:Heating:Water', IndUnit(IUNum)%HCoil, CoilWaterInletNode, &
                                       CoilWaterOutletNode, ErrorsFound)
          IF (PltSizHeatNum > 0) THEN

            IF (TermUnitFinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow >= SmallAirVolFlow) THEN
              DesPriVolFlow = IndUnit(IUNum)%MaxTotAirVolFlow / (1.d0+IndUnit(IUNum)%InducRatio)
              CpAir = PsyCpAirFnWTdb(TermUnitFinalZoneSizing(CurZoneEqNum)%HeatDesHumRat,  &
                                     TermUnitFinalZoneSizing(CurZoneEqNum)%HeatDesTemp)
            ! the design heating coil load is the zone load minus whatever the central system does. Note that
            ! DesHeatCoilInTempTU is really the primary air inlet temperature for the unit.
              IF (TermUnitFinalZoneSizing(CurZoneEqNum)%ZoneTempAtHeatPeak > 0.0d0) THEN
                DesCoilLoad = CalcFinalZoneSizing(CurZoneEqNum)%DesHeatLoad * CalcFinalZoneSizing(CurZoneEqNum)%HeatSizingFactor - &
                              CpAir*RhoAir*DesPriVolFlow* &
                             (TermUnitFinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTempTU -   &
                              TermUnitFinalZoneSizing(CurZoneEqNum)%ZoneTempAtHeatPeak)
              ELSE
                DesCoilLoad = CpAir*RhoAir*DesPriVolFlow*(ZoneSizThermSetPtLo(CurZoneEqNum) -   &
                                    TermUnitFinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTempTU)
              END IF
              IndUnit(IUNum)%DesHeatingLoad = DesCoilLoad
              Cp = GetSpecificHeatGlycol(PlantLoop(IndUnit(IUNum)%HWLoopNum)%FluidName, &
                                       60.d0, &
                                       PlantLoop(IndUnit(IUNum)%HWLoopNum)%FluidIndex, &
                                       'SizeIndUnit' )

              rho = GetDensityGlycol( PlantLoop(IndUnit(IUNum)%HWLoopNum)%FluidName, &
                                       60.d0, &
                                       PlantLoop(IndUnit(IUNum)%HWLoopNum)%FluidIndex, &
                                       'SizeIndUnit' )

              MaxVolHotWaterFlowDes = DesCoilLoad / &
                                    ( PlantSizData(PltSizHeatNum)%DeltaT * &
                                     Cp * rho )
              MaxVolHotWaterFlowDes = MAX(MaxVolHotWaterFlowDes,0.0d0)
            ELSE
              MaxVolHotWaterFlowDes = 0.0d0
            END IF
          ELSE
            CALL ShowSevereError('Autosizing of water flow requires a heating loop Sizing:Plant object')
            CALL ShowContinueError('Occurs in' //  TRIM(IndUnit(IUNum)%UnitType) // ' Object='//TRIM(IndUnit(IUNum)%Name))
            ErrorsFound = .TRUE.
          END IF
        END IF
        IF (IsAutosize) THEN
            IndUnit(IUNum)%MaxVolHotWaterFlow = MaxVolHotWaterFlowDes
            CALL ReportSizingOutput(IndUnit(IUNum)%UnitType, IndUnit(IUNum)%Name, &
                                  'Design Size Maximum Hot Water Flow Rate [m3/s]', MaxVolHotWaterFlowDes)
        ELSE
          IF (IndUnit(IUNum)%MaxVolHotWaterFlow > 0.0d0 .AND. MaxVolHotWaterFlowDes > 0.0d0) THEN
            MaxVolHotWaterFlowUser = IndUnit(IUNum)%MaxVolHotWaterFlow
            CALL ReportSizingOutput(IndUnit(IUNum)%UnitType, IndUnit(IUNum)%Name, &
                                  'Design Size Maximum Hot Water Flow Rate [m3/s]', MaxVolHotWaterFlowDes, &
                                  'User-Specified Maximum Hot Water Flow Rate [m3/s]', MaxVolHotWaterFlowUser)
            IF (DisplayExtraWarnings) THEN
              IF ((ABS(MaxVolHotWaterFlowDes - MaxVolHotWaterFlowUser)/MaxVolHotWaterFlowUser) > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizeHVACSingleDuctInduction: Potential issue with equipment sizing for '// &
                                    TRIM(IndUnit(IUNum)%UnitType)//' = "'//TRIM(IndUnit(IUNum)%Name)//'".')
                CALL ShowContinueError('User-Specified Maximum Hot Water Flow Rate of '// &
                                    TRIM(RoundSigDigits(MaxVolHotWaterFlowUser,5))// ' [m3/s]')
                CALL ShowContinueError('differs from Design Size Maximum Hot Water Flow Rate of ' // &
                                    TRIM(RoundSigDigits(MaxVolHotWaterFlowDes,5))// ' [m3/s]')
                CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
              END IF
            ENDIF
          END IF
        END IF
      ELSE
        IndUnit(IUNum)%MaxVolHotWaterFlow = 0.0d0
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (IndUnit(IUNum)%MaxVolColdWaterFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! simulation continue
      IF (IndUnit(IUNum)%MaxVolColdWaterFlow > 0.0d0) THEN
        CALL ReportSizingOutput(IndUnit(IUNum)%UnitType, IndUnit(IUNum)%Name, &
                          'User-Specified Maximum Cold Water Flow Rate [m3/s]', IndUnit(IUNum)%MaxVolColdWaterFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(IndUnit(IUNum)%UnitType, IndUnit(IUNum)%Name)

      IF (SameString(IndUnit(IUNum)%CCoilType,'Coil:Cooling:Water') .or. &
          SameString(IndUnit(IUNum)%CCoilType,'Coil:Cooling:Water:DetailedGeometry')) THEN

        CoilWaterInletNode = GetCoilWaterInletNode(IndUnit(IUNum)%CCoilType,IndUnit(IUNum)%CCoil,ErrorsFound)
        CoilWaterOutletNode = GetCoilWaterOutletNode(IndUnit(IUNum)%CCoilType,IndUnit(IUNum)%CCoil,ErrorsFound)
        IF (IsAutosize) THEN
          PltSizCoolNum = MyPlantSizingIndex(IndUnit(IUNum)%CCoilType, IndUnit(IUNum)%CCoil, CoilWaterInletNode, &
                                       CoilWaterOutletNode, ErrorsFound)
          IF (PltSizCoolNum > 0) THEN

            IF (TermUnitFinalZoneSizing(CurZoneEqNum)%DesCoolMassFlow >= SmallAirVolFlow) THEN
              DesPriVolFlow = IndUnit(IUNum)%MaxTotAirVolFlow / (1.d0+IndUnit(IUNum)%InducRatio)
              CpAir = PsyCpAirFnWTdb(TermUnitFinalZoneSizing(CurZoneEqNum)%CoolDesHumRat,  &
                                   TermUnitFinalZoneSizing(CurZoneEqNum)%CoolDesTemp)
            ! the design cooling coil load is the zone load minus whatever the central system does. Note that
            ! DesCoolCoilInTempTU is really the primary air inlet temperature for the unit.
              IF (TermUnitFinalZoneSizing(CurZoneEqNum)%ZoneTempAtCoolPeak > 0.0d0) THEN
                DesCoilLoad = CalcFinalZoneSizing(CurZoneEqNum)%DesCoolLoad * CalcFinalZoneSizing(CurZoneEqNum)%CoolSizingFactor - &
                            CpAir*RhoAir*DesPriVolFlow* &
                            (TermUnitFinalZoneSizing(CurZoneEqNum)%ZoneTempAtCoolPeak -   &
                             TermUnitFinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTempTU)
              ELSE
                DesCoilLoad = CpAir*RhoAir*DesPriVolFlow*(TermUnitFinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTempTU   &
                                         - ZoneSizThermSetPtHi(CurZoneEqNum))
              END IF
              IndUnit(IUNum)%DesCoolingLoad = DesCoilLoad
              Cp = GetSpecificHeatGlycol(PlantLoop(IndUnit(IUNum)%CWLoopNum)%FluidName, &
                                       5.0d0, &
                                       PlantLoop(IndUnit(IUNum)%CWLoopNum)%FluidIndex, &
                                       'SizeIndUnit' )

              rho = GetDensityGlycol( PlantLoop(IndUnit(IUNum)%CWLoopNum)%FluidName, &
                                       5.0d0, &
                                       PlantLoop(IndUnit(IUNum)%CWLoopNum)%FluidIndex, &
                                       'SizeIndUnit' )


              MaxVolColdWaterFlowDes = DesCoilLoad / &
                                     ( PlantSizData(PltSizCoolNum)%DeltaT * &
                                     Cp * rho )
              MaxVolColdWaterFlowDes = MAX(MaxVolColdWaterFlowDes,0.0d0)
            ELSE
              MaxVolColdWaterFlowDes = 0.0d0
            END IF
          ELSE
            CALL ShowSevereError('Autosizing of water flow requires a cooling loop Sizing:Plant object')
            CALL ShowContinueError('Occurs in' //  TRIM(IndUnit(IUNum)%UnitType) // ' Object='//TRIM(IndUnit(IUNum)%Name))
            ErrorsFound = .TRUE.
          END IF
        END IF
        IF (IsAutosize) THEN
          IndUnit(IUNum)%MaxVolColdWaterFlow = MaxVolColdWaterFlowDes
          CALL ReportSizingOutput(IndUnit(IUNum)%UnitType, IndUnit(IUNum)%Name, &
                                  'Design Size Maximum Cold Water Flow Rate [m3/s]', MaxVolColdWaterFlowDes)
        ELSE
          IF (IndUnit(IUNum)%MaxVolColdWaterFlow > 0.0d0 .AND. MaxVolColdWaterFlowDes > 0.0d0) THEN
            MaxVolColdWaterFlowUser = IndUnit(IUNum)%MaxVolColdWaterFlow
            CALL ReportSizingOutput(IndUnit(IUNum)%UnitType, IndUnit(IUNum)%Name, &
                                  'Design Size Maximum Cold Water Flow Rate [m3/s]', MaxVolColdWaterFlowDes, &
                                  'User-Specified Maximum Cold Water Flow Rate [m3/s]', MaxVolColdWaterFlowUser)
            IF (DisplayExtraWarnings) THEN
              IF ((ABS(MaxVolColdWaterFlowDes - MaxVolColdWaterFlowUser)/MaxVolColdWaterFlowUser) > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizeHVACSingleDuctInduction: Potential issue with equipment sizing for '// &
                                    TRIM(IndUnit(IUNum)%UnitType)//' = "'//TRIM(IndUnit(IUNum)%Name)//'".')
                CALL ShowContinueError('User-Specified Maximum Cold Water Flow Rate of '// &
                                    TRIM(RoundSigDigits(MaxVolColdWaterFlowUser,5))// ' [m3/s]')
                CALL ShowContinueError('differs from Design Size Maximum Cold Water Flow Rate of ' // &
                                    TRIM(RoundSigDigits(MaxVolColdWaterFlowDes,5))// ' [m3/s]')
                CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
              END IF
            ENDIF
          END IF
        END IF
      ELSE
        IndUnit(IUNum)%MaxVolColdWaterFlow = 0.0d0
      END IF
    END IF
  END IF

  IF (CurZoneEqNum > 0) THEN
    ! note we save the induced air flow for use by the hw and cw coil sizing routines
    TermUnitSizing(CurZoneEqNum)%AirVolFlow = IndUnit(IUNum)%MaxTotAirVolFlow * &
      IndUnit(IUNum)%InducRatio / (1.d0+ IndUnit(IUNum)%InducRatio)
    ! save the max hot and cold water flows for use in coil sizing
    TermUnitSizing(CurZoneEqNum)%MaxHWVolFlow = IndUnit(IUNum)%MaxVolHotWaterFlow
    TermUnitSizing(CurZoneEqNum)%MaxCWVolFlow = IndUnit(IUNum)%MaxVolColdWaterFlow
    ! save the design load used for reporting
    TermUnitSizing(CurZoneEqNum)%DesCoolingLoad = IndUnit(IUNum)%DesCoolingLoad
    TermUnitSizing(CurZoneEqNum)%DesHeatingLoad = IndUnit(IUNum)%DesHeatingLoad
    ! save the induction ratio for use in subsequent sizing calcs
    TermUnitSizing(CurZoneEqNum)%InducRat = IndUnit(IUNum)%InducRatio
    IF (SameString(IndUnit(IUNum)%HCoilType,'Coil:Heating:Water')) THEN
      CALL SetCoilDesFlow(IndUnit(IUNum)%HCoilType,IndUnit(IUNum)%HCoil,TermUnitSizing(CurZoneEqNum)%AirVolFlow,&
                          ErrorsFound)
    END IF
    IF (SameString(IndUnit(IUNum)%CCoilType,'Coil:Cooling:Water:DetailedGeometry')) THEN
      CALL SetCoilDesFlow(IndUnit(IUNum)%CCoilType,IndUnit(IUNum)%CCoil,TermUnitSizing(CurZoneEqNum)%AirVolFlow,&
                          ErrorsFound)
    END IF
  END IF

  RETURN

END SUBROUTINE SizeIndUnit

SUBROUTINE SimFourPipeIndUnit(IUNum,ZoneNum,ZoneNodeNum,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   June 23 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate a 4 pipe induction unit; adjust its heating or cooling
          ! coil outputs to match the zone load.

          ! METHODOLOGY EMPLOYED:
          ! (1) From the zone load and the primary air inlet conditions calculate the coil load
          !     in the secondary air stream
          ! (2) If there is a cooling coil load, set the heating coil off and control the cooling
          !     coil to meet the coil load
          ! (3) If there is a heating coil load, control the heating coil to meet the load and keep
          !     the cooling coil off.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands
  USE General, ONLY: SolveRegulaFalsi,RoundSigDigits
  USE DataPlant, ONLY: PlantLoop
  USE PlantUtilities, ONLY: SetComponentFlowRate

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT (IN)  :: FirstHVACIteration ! TRUE if 1st HVAC simulation of system timestep
  INTEGER, INTENT (IN)  :: IUNum              ! number of the current unit being simulated
  INTEGER, INTENT (IN)  :: ZoneNum            ! number of zone being served
  INTEGER, INTENT (IN)  :: ZoneNodeNum           ! zone node number

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: SolveMaxIter=50

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: QZnReq            ! heating or cooling needed by zone [Watts]
  REAL(r64)    :: QToHeatSetPt      ! [W]  remaining load to heating setpoint
  REAL(r64)    :: QToCoolSetPt      ! [W]  remaining load to cooling setpoint
  REAL(r64)    :: PowerMet          ! power supplied
  LOGICAL :: UnitOn            ! TRUE if unit is on
  REAL(r64)    :: MaxHotWaterFlow   ! maximum water flow for heating [kg/s]
  REAL(r64)    :: MinHotWaterFlow   ! minimum water flow for heating [kg/s]
  REAL(r64)    :: MaxColdWaterFlow  ! maximum water flow for cooling [kg/s]
  REAL(r64)    :: MinColdWaterFlow  ! minimum water flow for cooling [kg/s]
  REAL(r64)    :: HWFlow            ! hot water flow [kg/s]
  REAL(r64)    :: CWFlow            ! cold water flow [kg/s]
  INTEGER :: PriNode           ! unit primary air inlet node
  INTEGER :: SecNode           ! unit secondary air inlet node
  INTEGER :: OutletNode        ! unit air outlet node
  INTEGER :: HotControlNode    ! hot water coil inlet node
  INTEGER :: ColdControlNode   ! cold water coil inlet node
  REAL(r64)    :: QPriOnly          ! unit output with no zone coils active
  REAL(r64)    :: PriAirMassFlow    ! primary air mass flow rate [kg/s]
  REAL(r64)    :: SecAirMassFlow    ! secondary air mass flow rate [kg/s]
  REAL(r64)    :: InducRat          ! Induction Ratio
  REAL(r64), DIMENSION(7)  :: Par
  INTEGER :: SolFlag
  REAL(r64) :: ErrTolerance
  INTEGER   :: HWOutletNode
  INTEGER   :: CWOutletNode

  UnitOn = .TRUE.
  PowerMet = 0.0d0
  InducRat = IndUnit(IUNum)%InducRatio
  PriNode = IndUnit(IUNum)%PriAirInNode
  SecNode = IndUnit(IUNum)%SecAirInNode
  OutletNode = IndUnit(IUNum)%OutAirNode
  HotControlNode = IndUnit(IUNum)%HWControlNode
  HWOutletNode   = PlantLoop(IndUnit(IUNum)%HWLoopNum)%LoopSide(IndUnit(IUNum)%HWLoopSide) &
                         %Branch(IndUnit(IUNum)%HWBranchNum)%Comp(IndUnit(IUNum)%HWCompNum)%NodeNumOut
  ColdControlNode = IndUnit(IUNum)%CWControlNode
  CWOutletNode = PlantLoop(IndUnit(IUNum)%CWLoopNum)%LoopSide(IndUnit(IUNum)%CWLoopSide) &
                         %Branch(IndUnit(IUNum)%CWBranchNum)%Comp(IndUnit(IUNum)%CWCompNum)%NodeNumOut
  PriAirMassFlow = Node(PriNode)%MassFlowRateMaxAvail
  SecAirMassFlow = InducRat*PriAirMassFlow
  QZnReq = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired
  QToHeatSetPt=ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP
  QToCoolSetPt=ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToCoolSP
  !On the first HVAC iteration the system values are given to the controller, but after that
  ! the demand limits are in place and there needs to be feedback to the Zone Equipment

  MaxHotWaterFlow = IndUnit(IUNum)%MaxHotWaterFlow
  Call SetComponentFlowRate(MaxHotWaterFlow,                &
                               HotControlNode,              &
                               HWOutletNode,                &
                               IndUnit(IUNum)%HWLoopNum,    &
                               IndUnit(IUNum)%HWLoopSide,   &
                               IndUnit(IUNum)%HWBranchNum,  &
                               IndUnit(IUNum)%HWCompNum)

  MinHotWaterFlow = IndUnit(IUNum)%MinHotWaterFlow
  Call SetComponentFlowRate(MinHotWaterFlow,                &
                               HotControlNode,              &
                               HWOutletNode,                &
                               IndUnit(IUNum)%HWLoopNum,    &
                               IndUnit(IUNum)%HWLoopSide,   &
                               IndUnit(IUNum)%HWBranchNum,  &
                               IndUnit(IUNum)%HWCompNum)

  MaxColdWaterFlow = IndUnit(IUNum)%MaxColdWaterFlow
  Call SetComponentFlowRate(MaxColdWaterFlow,               &
                               ColdControlNode,             &
                               CWOutletNode,                &
                               IndUnit(IUNum)%CWLoopNum,    &
                               IndUnit(IUNum)%CWLoopSide,   &
                               IndUnit(IUNum)%CWBranchNum,  &
                               IndUnit(IUNum)%CWCompNum)

  MinColdWaterFlow = IndUnit(IUNum)%MinColdWaterFlow
  Call SetComponentFlowRate(MinColdWaterFlow,               &
                               ColdControlNode,             &
                               CWOutletNode,                &
                               IndUnit(IUNum)%CWLoopNum,    &
                               IndUnit(IUNum)%CWLoopSide,   &
                               IndUnit(IUNum)%CWBranchNum,  &
                               IndUnit(IUNum)%CWCompNum)

  IF (GetCurrentScheduleValue(IndUnit(IUNum)%SchedPtr) .LE. 0.0d0) UnitOn = .FALSE.
  IF (PriAirMassFlow.LE.SmallMassFlow) UnitOn = .FALSE.

  ! Set the unit's air inlet nodes mass flow rates
  Node(PriNode)%MassFlowRate = PriAirMassFlow
  Node(SecNode)%MassFlowRate = SecAirMassFlow
  ! initialize the water inlet nodes to minimum
  ! fire the unit at min water flow
  CALL CalcFourPipeIndUnit(IUNum,FirstHVACIteration,ZoneNodeNum,MinHotWaterFlow,MinColdWaterFlow,QPriOnly)
  ! the load to be met by the secondary air stream coils is QZnReq-PowerMet

  IF (UnitOn) THEN

    IF (QToHeatSetPt-QPriOnly > SmallLoad) THEN
      ! heating coil
      ! check that it can meet the load
      CALL CalcFourPipeIndUnit(IUNum,FirstHVACIteration,ZoneNodeNum,MaxHotWaterFlow,MinColdWaterFlow,PowerMet)
      IF (PowerMet > QToHeatSetPt + SmallLoad) THEN
        Par(1) = REAL(IUNum,r64)
        IF (FirstHVACIteration) THEN
          Par(2) = 1.d0
        ELSE
          Par(2) = 0.0d0
        END IF
        Par(3) = REAL(ZoneNodeNum,r64)
        Par(4) = MinColdWaterFlow
        Par(5) = QToHeatSetPt
        Par(6) = QPriOnly
        Par(7) = PowerMet
        ErrTolerance=IndUnit(IUNum)%HotControlOffset
        CALL SolveRegulaFalsi(ErrTolerance, SolveMaxIter, SolFlag, HWFlow, FourPipeIUHeatingResidual, &
                              MinHotWaterFlow, MaxHotWaterFlow, Par)
        IF (SolFlag == -1) THEN
          IF (IndUnit(IUNum)%HWCoilFailNum1 == 0) THEN
            CALL ShowWarningMessage('SimFourPipeIndUnit: Hot water coil control failed for '//  &
                trim(IndUnit(IUNum)%UnitType)//'="'//  &
                TRIM(IndUnit(IUNum)%Name)//'"')
            CALL ShowContinueErrorTimeStamp(' ')
            CALL ShowContinueError('  Iteration limit ['//trim(RoundSigDigits(SolveMaxIter))//  &
                 '] exceeded in calculating hot water mass flow rate')
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('SimFourPipeIndUnit: Hot water coil control failed (iteration limit ['//  &
              trim(RoundSigDigits(SolveMaxIter))//']) for '//trim(IndUnit(IUNum)%UnitType)//'="'// &
              TRIM(IndUnit(IUNum)%Name)//'"',IndUnit(IUNum)%HWCoilFailNum1)
        ELSE IF (SolFlag == -2) THEN
          IF (IndUnit(IUNum)%HWCoilFailNum2 == 0) THEN
            CALL ShowWarningMessage('SimFourPipeIndUnit: Hot water coil control failed (maximum flow limits) for '//  &
                trim(IndUnit(IUNum)%UnitType)//'="'// &
                TRIM(IndUnit(IUNum)%Name)//'"')
            CALL ShowContinueErrorTimeStamp(' ')
            CALL ShowContinueError('...Bad hot water maximum flow rate limits')
            CALL ShowContinueError('...Given minimum water flow rate='//trim(RoundSigDigits(MinHotWaterFlow,3))//' kg/s')
            CALL ShowContinueError('...Given maximum water flow rate='//trim(RoundSigDigits(MaxHotWaterFlow,3))//' kg/s')
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('SimFourPipeIndUnit: Hot water coil control failed (flow limits) for '//  &
              trim(IndUnit(IUNum)%UnitType)//'="'// &
              TRIM(IndUnit(IUNum)%Name)//'"', &
              IndUnit(IUNum)%HWCoilFailNum2,  &
              ReportMinOf=MinHotWaterFlow,ReportMaxOf=MaxHotWaterFlow,ReportMinUnits='[kg/s]',ReportMaxUnits='[kg/s]')
        END IF
      END IF
    ELSE IF (QToCoolSetPt - QPriOnly <  - SmallLoad) THEN
      ! cooling coil
      ! check that it can meet the load
      CALL CalcFourPipeIndUnit(IUNum,FirstHVACIteration,ZoneNodeNum,MinHotWaterFlow,MaxColdWaterFlow,PowerMet)
      IF (PowerMet < QToCoolSetPt - SmallLoad) THEN
        Par(1) = REAL(IUNum,r64)
        IF (FirstHVACIteration) THEN
          Par(2) = 1.d0
        ELSE
          Par(2) = 0.0d0
        END IF
        Par(3) = REAL(ZoneNodeNum,r64)
        Par(4) = MinHotWaterFlow
        Par(5) = QToCoolSetPt
        Par(6) = QPriOnly
        Par(7) = PowerMet
        ErrTolerance=IndUnit(IUNum)%ColdControlOffset
        CALL SolveRegulaFalsi(ErrTolerance, SolveMaxIter, SolFlag, CWFlow, FourPipeIUCoolingResidual, &
                              MinColdWaterFlow, MaxColdWaterFlow, Par)
        IF (SolFlag == -1) THEN
          IF (IndUnit(IUNum)%CWCoilFailNum1 == 0) THEN
            CALL ShowWarningMessage('SimFourPipeIndUnit: Cold water coil control failed for '//  &
                trim(IndUnit(IUNum)%UnitType)//'="'//  &
                TRIM(IndUnit(IUNum)%Name)//'"')
            CALL ShowContinueErrorTimeStamp(' ')
            CALL ShowContinueError('  Iteration limit ['//trim(RoundSigDigits(SolveMaxIter))//  &
                 '] exceeded in calculating cold water mass flow rate')
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('SimFourPipeIndUnit: Cold water coil control failed (iteration limit ['//  &
              trim(RoundSigDigits(SolveMaxIter))//']) for '//trim(IndUnit(IUNum)%UnitType)//'="'// &
              TRIM(IndUnit(IUNum)%Name),IndUnit(IUNum)%CWCoilFailNum1)
        ELSE IF (SolFlag == -2) THEN
          IF (IndUnit(IUNum)%CWCoilFailNum2 == 0) THEN
            CALL ShowWarningMessage('SimFourPipeIndUnit: Cold water coil control failed (maximum flow limits) for '//  &
                trim(IndUnit(IUNum)%UnitType)//'="'// &
                TRIM(IndUnit(IUNum)%Name)//'"')
            CALL ShowContinueErrorTimeStamp(' ')
            CALL ShowContinueError('...Bad cold water maximum flow rate limits')
            CALL ShowContinueError('...Given minimum water flow rate='//trim(RoundSigDigits(MinColdWaterFlow,3))//' kg/s')
            CALL ShowContinueError('...Given maximum water flow rate='//trim(RoundSigDigits(MaxColdWaterFlow,3))//' kg/s')
          ENDIF
          CALL ShowRecurringWarningErrorAtEnd('SimFourPipeIndUnit: Cold water coil control failed (flow limits) for '//  &
              trim(IndUnit(IUNum)%UnitType)//'="'// &
              TRIM(IndUnit(IUNum)%Name)//'"', &
              IndUnit(IUNum)%CWCoilFailNum2,  &
              ReportMinOf=MinColdWaterFlow,ReportMaxOf=MaxColdWaterFlow,ReportMinUnits='[kg/s]',ReportMaxUnits='[kg/s]')
        END IF
      END IF
    ELSE
      CALL CalcFourPipeIndUnit(IUNum,FirstHVACIteration,ZoneNodeNum,MinHotWaterFlow,MinColdWaterFlow,PowerMet)
    END IF

  ELSE
    ! unit off
    CALL CalcFourPipeIndUnit(IUNum,FirstHVACIteration,ZoneNodeNum,MinHotWaterFlow,MinColdWaterFlow,PowerMet)
  END IF
  Node(OutletNode)%MassFlowRateMax = IndUnit(IUNum)%MaxTotAirMassFlow

  ! At this point we are done. There is no output to report or pass back up: the output provided is calculated
  ! one level up in the calling routine SimZoneAirLoopEquipment. All the inlet and outlet flow rates and
  ! conditions have been set by CalcFourPipeIndUnit either explicitly or as a result of the simple component calls.

  RETURN

END SUBROUTINE SimFourPipeIndUnit

SUBROUTINE CalcFourPipeIndUnit(IUNum,FirstHVACIteration,ZoneNode,HWFlow,CWFlow,LoadMet)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   June 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate the components making up the 4 pipe induction unit.

          ! METHODOLOGY EMPLOYED:
          ! Simulates the unit components sequentially in the air flow direction.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE MixerComponent, ONLY: SimAirMixer
  USE HeatingCoils,   ONLY: SimulateHeatingCoilComponents
  USE WaterCoils,     ONLY: SimulateWaterCoilComponents
  USE PlantUtilities, ONLY: SetComponentFlowRate
  USE DataPlant,      ONLY: PlantLoop

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: IUNum         ! Unit index
  LOGICAL, INTENT (IN)  :: FirstHVACIteration ! flag for 1st HVAV iteration in the time step
  INTEGER, INTENT (IN)  :: ZoneNode           ! zone node number
  REAL(r64),    INTENT (IN)  :: HWFlow             ! hot water flow (kg/s)
  REAL(r64),    INTENT (IN)  :: CWFlow             ! cold water flow (kg/s)
  REAL(r64),    INTENT (OUT) :: LoadMet            ! load met by unit (watts)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: OutletNode        ! unit air outlet node
  INTEGER :: PriNode           ! unit primary air inlet node
  INTEGER :: HotControlNode    ! the hot water inlet node
  INTEGER :: ColdControlNode   ! the cold water inlet node
  REAL(r64)    :: PriAirMassFlow    ! primary air mass flow rate [kg/s]
  REAL(r64)    :: SecAirMassFlow    ! secondary air mass flow rate [kg/s]
  REAL(r64)    :: TotAirMassFlow    ! total air mass flow rate [kg/s]
  REAL(r64)    :: InducRat          ! induction ratio
  REAL(r64)    :: CpAirZn           ! zone air specific heat [J/kg-C]
  REAL(r64)    :: mdotHW  ! local temporary hot water flow rate [kg/s]
  REAL(r64)    :: mdotCW  ! local temporary cold water flow rate [kg/s]
  INTEGER      :: HWOutletNode
  INTEGER      :: CWOutletNode

          ! FLOW

  PriNode = IndUnit(IUNum)%PriAirInNode
  OutletNode = IndUnit(IUNum)%OutAirNode
  PriAirMassFlow = Node(PriNode)%MassFlowRateMaxAvail
  InducRat = IndUnit(IUNum)%InducRatio
  CpAirZn = PsyCpAirFnWTdb(Node(ZoneNode)%HumRat,Node(ZoneNode)%Temp)
  SecAirMassFlow = InducRat*PriAirMassFlow
  TotAirMassFlow = PriAirMassFlow + SecAirMassFlow
  HotControlNode = IndUnit(IUNum)%HWControlNode
  HWOutletNode   = PlantLoop(IndUnit(IUNum)%HWLoopNum)%LoopSide(IndUnit(IUNum)%HWLoopSide) &
                         %Branch(IndUnit(IUNum)%HWBranchNum)%Comp(IndUnit(IUNum)%HWCompNum)%NodeNumOut

  ColdControlNode = IndUnit(IUNum)%CWControlNode
  CWOutletNode    = PlantLoop(IndUnit(IUNum)%CWLoopNum)%LoopSide(IndUnit(IUNum)%CWLoopSide) &
                         %Branch(IndUnit(IUNum)%CWBranchNum)%Comp(IndUnit(IUNum)%CWCompNum)%NodeNumOut

  mdotHW = HWFlow
  Call SetComponentFlowRate(   mdotHW,  &
                               HotControlNode,  &
                               HWOutletNode,    &
                               IndUnit(IUNum)%HWLoopNum,       &
                               IndUnit(IUNum)%HWLoopSide,    &
                               IndUnit(IUNum)%HWBranchNum,     &
                               IndUnit(IUNum)%HWCompNum)

!  Node(HotControlNode)%MassFlowRate = HWFlow

  mdotCW = CWFlow
  CALL SetComponentFlowRate(   mdotCW, &
                               ColdControlNode,    &
                               CWOutletNode,    &
                               IndUnit(IUNum)%CWLoopNum,       &
                               IndUnit(IUNum)%CWLoopSide,    &
                               IndUnit(IUNum)%CWBranchNum,     &
                               IndUnit(IUNum)%CWCompNum)
!  Node(ColdControlNode)%MassFlowRate = CWFlow

  CALL SimulateWaterCoilComponents(IndUNit(IUNum)%HCoil,FirstHVACIteration,IndUNit(IUNum)%HCoil_Num)
  CALL SimulateWaterCoilComponents(IndUnit(IUNum)%CCoil,FirstHVACIteration,IndUNit(IUNum)%CCoil_Num)
  CALL SimAirMixer(IndUnit(IUNum)%MixerName,IndUnit(IUNum)%Mixer_Num)
  LoadMet = TotAirMassFlow*CpAirZn*(Node(OutletNode)%Temp-Node(ZoneNode)%Temp)

  RETURN

END SUBROUTINE CalcFourPipeIndUnit

FUNCTION FourPipeIUHeatingResidual(HWFlow, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   June 2004
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (Requested Zone Load - Unit Output) / Requested Coil Load
          ! Unit Output depends on the hot water flow rate which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls CalcFourPipeIndUnit, and calculates
          ! the residual as defined above.

          ! REFERENCES:

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: HWFlow ! hot water flow rate in kg/s
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! Par(5) is the requested zone load
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: IUIndex
  LOGICAL :: FirstHVACSoln
  INTEGER :: ZoneNodeIndex
  REAL(r64)    :: MinCWFlow
  REAL(r64)    :: UnitOutput

  IUIndex = INT(Par(1))
  IF (Par(2) > 0.0d0) THEN
    FirstHVACSoln = .TRUE.
  ELSE
    FirstHVACSoln = .FALSE.
  END IF
  ZoneNodeIndex = INT(Par(3))
  MinCWFlow = Par(4)
  CALL CalcFourPipeIndUnit(IUIndex,FirstHVACSoln,ZoneNodeIndex,HWFlow,MinCWFlow,UnitOutput)
  Residuum = (Par(5) - UnitOutput) / (Par(7) - Par(6))

  RETURN
END FUNCTION FourPipeIUHeatingResidual

FUNCTION FourPipeIUCoolingResidual(CWFlow, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   June 2004
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (Requested Zone Load - Unit Output) / Requested Coil Load
          ! Unit Output depends on the cold water flow rate which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls CalcFourPipeIndUnit, and calculates
          ! the residual as defined above.

          ! REFERENCES:

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: CWFlow ! cold water flow rate in kg/s
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! Par(5) is the requested zone load
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: IUIndex
  LOGICAL :: FirstHVACSoln
  INTEGER :: ZoneNodeIndex
  REAL(r64)    :: MinHWFlow
  REAL(r64)    :: UnitOutput

  IUIndex = INT(Par(1))
  IF (Par(2) > 0.0d0) THEN
    FirstHVACSoln = .TRUE.
  ELSE
    FirstHVACSoln = .FALSE.
  END IF
  ZoneNodeIndex = INT(Par(3))
  MinHWFlow = Par(4)
  CALL CalcFourPipeIndUnit(IUIndex,FirstHVACSoln,ZoneNodeIndex,MinHWFlow,CWFlow,UnitOutput)
  Residuum = (Par(5) - UnitOutput) / (Par(7) - Par(6))

  RETURN
END FUNCTION FourPipeIUCoolingResidual

! ========================= Utilities =======================

FUNCTION FourPipeInductionUnitHasMixer(CompName) RESULT(YesNo)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   September 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Given a mixer name, this routine determines if that mixer is found on
          ! PIUnits.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompName  ! component (mixer) name
  LOGICAL :: YesNo  ! True if found

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ItemNum

  IF (GetIUInputFlag) THEN
    CALL GetIndUnits
    GetIUInputFlag = .FALSE.
  END IF

  YesNo=.false.
  IF (NumIndUnits > 0) THEN
    ItemNum=FindItemInList(CompName,IndUnit%MixerName,NumIndUnits)
    IF (ItemNum > 0) YesNo=.true.
  ENDIF

  RETURN

END FUNCTION FourPipeInductionUnitHasMixer

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

END MODULE HVACSingleDuctInduc
