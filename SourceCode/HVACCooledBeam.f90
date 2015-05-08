MODULE HVACCooledBeam

          ! Module containing routines dealing with cooled beam units

          ! MODULE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   February 2, 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! To encapsulate the data and algorithms needed to simulate cooled beam units

          ! METHODOLOGY EMPLOYED:
          ! Cooled beam units are treated as terminal units. There is a fixed amount of supply air delivered
          ! either directly through a diffuser or through the cooled beam units. Thermodynamically the
          ! situation is similar to 4 pipe induction terminal units. The detailed methodology follows the
          ! method in DOE-2.1E.

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals,     ONLY: BeginEnvrnFlag, MaxNameLength, NumOfZones, &
                           InitConvTemp, SysSizingCalc, Pi, SecInHour, ScheduleAlwaysOn
USE DataInterfaces,  ONLY: ShowWarningError, ShowFatalError, ShowSevereError, ShowContinueError, &
                           SetupOutputVariable
Use DataEnvironment, ONLY: StdBaroPress, StdRhoAir
          ! Use statements for access to subroutines in other modules
USE ScheduleManager
USE Psychrometrics,  ONLY: PsyRhoAirFnPbTdbW,PsyCpAirFnWTdb,PsyHFnTdbW
USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad, SmallAirVolFlow, TimeStepSys, SmallWaterVolFlow

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: Passive_Cooled_Beam=1
INTEGER, PARAMETER :: Active_Cooled_Beam=2
REAL(r64), PARAMETER :: NomMassFlowPerBeam = 0.07d0 ! nominal water mass flow rate per beam [kg/s]
REAL(r64), PARAMETER :: MinWaterVel        = 0.2d0  ! minimum water velocity [m/s]
REAL(r64), PARAMETER :: Coeff2             = 10000.0d0
          ! DERIVED TYPE DEFINITIONS:
TYPE CoolBeamData
  ! input data
  CHARACTER(len=MaxNameLength) :: Name                 =' ' ! name of unit
  CHARACTER(len=MaxNameLength) :: UnitType             =' ' ! type of unit = AirTerminal:SingleDuct:ConstantVolume:CooledBeam
  INTEGER                      :: UnitType_Num         =0   ! index to type of unit = 1 (there's only 1 type so far)
  CHARACTER(len=MaxNameLength) :: CBType               =' ' ! type of cooled beam: active | passive
  INTEGER                      :: CBType_Num           =0   ! index to type of cooled beam: passive=1; active=2
  CHARACTER(len=MaxNameLength) :: Sched                =' ' ! availability schedule
  INTEGER                      :: SchedPtr             =0   ! index to schedule
  REAL(r64)                    :: MaxAirVolFlow        =0.0d0 ! m3/s (autosizable)
  REAL(r64)                    :: MaxAirMassFlow       =0.0d0 ! kg/s
  REAL(r64)                    :: MaxCoolWaterVolFlow  =0.0d0 ! m3/s
  REAL(r64)                    :: MaxCoolWaterMassFlow =0.0d0 !kg/s
  INTEGER                      :: AirInNode            =0   ! unit air inlet node number
  INTEGER                      :: AirOutNode           =0   ! unit air outlet node number
  INTEGER                      :: CWInNode             =0   ! chilled water inlet node
  INTEGER                      :: CWOutNode            =0   ! chilled water outlet node
  INTEGER                      :: ADUNum               =0   ! index of corresponding air distribution unit
  REAL(r64)                    :: NumBeams             =0.0d0 ! number of beams in the zone
  REAL(r64)                    :: BeamLength           =0.0d0 ! length of individual beam [m]
  REAL(r64)                    :: DesInletWaterTemp    =0.0d0 ! design inlet water temperature [C]
  REAL(r64)                    :: DesOutletWaterTemp   =0.0d0 ! design outlet water Temperature [c]
  REAL(r64)                    :: CoilArea             =0.0d0 ! coil surface area per coil length [m2/m]
  REAL(r64)                    :: a                    =0.0d0 ! model parameter a
  REAL(r64)                    :: n1                   =0.0d0 ! model parameter n0
  REAL(r64)                    :: n2                   =0.0d0 ! model parameter n1
  REAL(r64)                    :: n3                   =0.0d0 ! model parameter n2
  REAL(r64)                    :: a0                   =0.0d0 ! model parameter a0
  REAL(r64)                    :: K1                   =0.0d0 ! model parameter K1
  REAL(r64)                    :: n                    =0.0d0 ! model parameter n
  REAL(r64)                    :: Kin                  =0.0d0 ! Coefficient of Induction Kin
  REAL(r64)                    :: InDiam               =0.0d0 ! Leaving Pipe Inside Diameter
  ! time step variables
  REAL(r64)                    :: TWIn                 =0.0d0 ! current inlet water temperature [C]
  REAL(r64)                    :: TWOut                =0.0d0 ! current outlet water temperature [C]
  REAL(r64)                    :: EnthWaterOut         =0.0d0 ! current outlet water enthalpy [J/kg]
  REAL(r64)                    :: BeamFlow             =0.0d0 ! supply air flow per beam [m3/s]
  REAL(r64)                    :: CoolWaterMassFlow    =0.0d0 ! chilled water mass flow rate [kg/s]
  REAL(r64)                    :: BeamCoolingEnergy    =0.0d0 ! Cooled beam cooling energy of all beams in the zone [J]
  REAL(r64)                    :: BeamCoolingRate      =0.0d0 ! Cooled beam cooling rate of all beams in the zone [W]
  REAL(r64)                    :: SupAirCoolingEnergy  =0.0d0 ! Total cooling energy from supply air [J]
  REAL(r64)                    :: SupAirCoolingRate    =0.0d0 ! Total cooling rate from supply air [W]
  REAL(r64)                    :: SupAirHeatingEnergy  =0.0d0 ! Total cooling energy from supply air [J]
  REAL(r64)                    :: SupAirHeatingRate    =0.0d0 ! Total cooling rate from supply air [W]

  INTEGER                      :: CWLoopNum     = 0  ! cooling water plant loop index number
  INTEGER                      :: CWLoopSideNum = 0  ! cooling water plant loop side index
  INTEGER                      :: CWBranchNum   = 0  ! cooling water plant loop branch index
  INTEGER                      :: CWCompNum     = 0  ! cooling water plant loop component index
  INTEGER                      :: CBLoadReSimIndex = 0
  INTEGER                      :: CBMassFlowReSimIndex = 0
  INTEGER                      :: CBWaterOutletTempReSimIndex = 0

END TYPE CoolBeamData
          ! MODULE VARIABLE DECLARATIONS:
TYPE (CoolBeamData), ALLOCATABLE, DIMENSION(:)         :: CoolBeam
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

! INTEGER :: NumPassiveCB = 0
! INTEGER :: NumActiveCB = 0
INTEGER :: NumCB       = 0

          ! SUBROUTINE SPECIFICATIONS FOR MODULE HVACCooledBeam:

PUBLIC  SimCoolBeam
PRIVATE GetCoolBeams
PRIVATE InitCoolBeam
PRIVATE SizeCoolBeam
PRIVATE CalcCoolBeam
PRIVATE ControlCoolBeam
PRIVATE UpdateCoolBeam
PRIVATE ReportCoolBeam
PRIVATE CoolBeamResidual

CONTAINS

SUBROUTINE SimCoolBeam(CompName, FirstHVACIteration, ZoneNum, ZoneNodeNum, CompIndex, NonAirSysOutput)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Feb 3, 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Manages the simulation of a cooled beam unit.
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
  CHARACTER(len=*), INTENT(IN) :: CompName    ! name of the cooled beam unit
  LOGICAL, INTENT (IN)         :: FirstHVACIteration ! TRUE if first HVAC iteration in time step
  INTEGER, INTENT (IN)         :: ZoneNum     ! index of zone served by the unit
  INTEGER, INTENT (IN)         :: ZoneNodeNum ! zone node number of zone served by the unit
  INTEGER, INTENT (INOUT)      :: CompIndex   ! which cooled beam unit in data structure
  REAL(r64)                    :: NonAirSysOutput ! convective cooling by the beam system [W]

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: CBNum                  ! index of cooled beam unit being simulated
  LOGICAL,SAVE :: GetInputFlag = .TRUE.  ! First time, input is "gotten"

  ! First time SimIndUnit is called, get the input for all the cooled beam units
  IF (GetInputFlag) THEN
    CALL GetCoolBeams
    GetInputFlag = .FALSE.
  END IF

  ! Get the  unit index
  IF (CompIndex == 0) THEN
    CBNum = FindItemInList(CompName,CoolBeam%Name,NumCB)
    IF (CBNum == 0) THEN
      CALL ShowFatalError('SimCoolBeam: Cool Beam Unit not found='//TRIM(CompName))
    ENDIF
    CompIndex=CBNum
  ELSE
    CBNum=CompIndex
    IF (CBNum > NumCB .or. CBNum < 1) THEN
      CALL ShowFatalError('SimCoolBeam: Invalid CompIndex passed='//TRIM(TrimSigDigits(CompIndex))// &
                          ', Number of Cool Beam Units='//TRIM(TrimSigDigits(NumCB))//        &
                          ', System name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(CBNum)) THEN
      IF (CompName /= CoolBeam(CBNum)%Name) THEN
        CALL ShowFatalError('SimCoolBeam: Invalid CompIndex passed='//TRIM(TrimSigDigits(CompIndex))//      &
                            ', Cool Beam Unit name='//TRIM(CompName)//', stored Cool Beam Unit for that index='//  &
                            TRIM(CoolBeam(CBNum)%Name))
      ENDIF
      CheckEquipName(CBNum)=.false.
    ENDIF
  ENDIF
  IF (CBNum .EQ. 0) THEN
    CALL ShowFatalError('Cool Beam Unit not found = '//TRIM(CompName))
  END IF

  ! initialize the unit
  CALL InitCoolBeam(CBNum,FirstHVACIteration)

  CALL ControlCoolBeam(CBNum,ZoneNum,ZoneNodeNum,FirstHVACIteration,NonAirSysOutput)

  ! Update the current unit's outlet nodes. No update needed
  CALL UpdateCoolBeam(CBNum)

  ! Fill the report variables. There are no report variables
  CALL ReportCoolBeam(CBNum)

  RETURN

END SUBROUTINE SimCoolBeam

SUBROUTINE GetCoolBeams

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Feb 3, 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for cool beam units and stores it in the
          ! cool beam unit data structures

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

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER (len=*), PARAMETER   :: RoutineName='GetCoolBeams '              ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  INTEGER                        :: CBIndex ! loop index
  INTEGER                        :: CBNum   ! current fan coil number
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

  ! find the number of cooled beam units
  CurrentModuleObject = 'AirTerminal:SingleDuct:ConstantVolume:CooledBeam'
  NumCB = GetNumObjectsFound(CurrentModuleObject)
  ! allocate the data structures
  ALLOCATE(CoolBeam(NumCB))
  ALLOCATE(CheckEquipName(NumCB))
  CheckEquipName=.true.

  CALL GetObjectDefMaxArgs(CurrentModuleObject,TotalArgs,NumAlphas,NumNumbers)
  NumAlphas = 7
  NumNumbers = 16
  TotalArgs = 23

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

  ! loop over cooled beam units; get and load the input data
  DO CBIndex = 1,NumCB

    CALL GetObjectItem(CurrentModuleObject,CBIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
    CBNum = CBIndex
    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(Alphas(1),CoolBeam%Name,CBNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    CoolBeam(CBNum)%Name = Alphas(1)
    CoolBeam(CBNum)%UnitType = CurrentModuleObject
    CoolBeam(CBNum)%UnitType_Num = 1
    CoolBeam(CBNum)%CBType = TRIM(Alphas(3))
    IF (SameString(CoolBeam(CBNum)%CBType,'Passive')) THEN
      CoolBeam(CBNum)%CBType_Num = Passive_Cooled_Beam
    ELSE IF (SameString(CoolBeam(CBNum)%CBType,'Active')) THEN
      CoolBeam(CBNum)%CBType_Num = Active_Cooled_Beam
    ELSE
      CALL ShowSevereError('Illegal '//TRIM(cAlphaFields(3))//' = '//TRIM(CoolBeam(CBNum)%CBType)//'.')
      CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(CoolBeam(CBNum)%Name))
      ErrorsFound=.true.
    END IF
    CoolBeam(CBNum)%Sched = Alphas(2)
    IF (lAlphaBlanks(2)) THEN
      CoolBeam(CBNum)%SchedPtr = ScheduleAlwaysOn
    ELSE
      CoolBeam(CBNum)%SchedPtr = GetScheduleIndex(Alphas(2))  ! convert schedule name to pointer
      IF (CoolBeam(CBNum)%SchedPtr .EQ. 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': invalid '//TRIM(cAlphaFields(2))//  &
             ' entered ='//TRIM(Alphas(2))// &
             ' for '//TRIM(cAlphaFields(1))//'='//TRIM(Alphas(1)))
        ErrorsFound=.TRUE.
      END IF
    END IF
    CoolBeam(CBNum)%AirInNode = &
      GetOnlySingleNode(Alphas(4),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                        NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent,cAlphaFields(4))
    CoolBeam(CBNum)%AirOutNode = &
      GetOnlySingleNode(Alphas(5),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                        NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent,cAlphaFields(5))
    CoolBeam(CBNum)%CWInNode = &
      GetOnlySingleNode(Alphas(6),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                        NodeType_Water,NodeConnectionType_Inlet,2,ObjectIsNotParent,cAlphaFields(6))
    CoolBeam(CBNum)%CWOutNode = &
      GetOnlySingleNode(Alphas(7),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                        NodeType_Water,NodeConnectionType_Outlet,2,ObjectIsNotParent,cAlphaFields(7))
    CoolBeam(CBNum)%MaxAirVolFlow = Numbers(1)
    CoolBeam(CBNum)%MaxCoolWaterVolFlow = Numbers(2)
    CoolBeam(CBNum)%NumBeams = Numbers(3)
    CoolBeam(CBNum)%BeamLength = Numbers(4)
    CoolBeam(CBNum)%DesInletWaterTemp = Numbers(5)
    CoolBeam(CBNum)%DesOutletWaterTemp = Numbers(6)
    CoolBeam(CBNum)%CoilArea = Numbers(7)
    CoolBeam(CBNum)%a = Numbers(8)
    CoolBeam(CBNum)%n1 = Numbers(9)
    CoolBeam(CBNum)%n2 = Numbers(10)
    CoolBeam(CBNum)%n3 = Numbers(11)
    CoolBeam(CBNum)%a0 = Numbers(12)
    CoolBeam(CBNum)%K1 = Numbers(13)
    CoolBeam(CBNum)%n = Numbers(14)
    CoolBeam(CBNum)%Kin = Numbers(15)
    CoolBeam(CBNum)%InDiam = Numbers(16)

    ! Register component set data
    CALL TestCompSet(TRIM(CurrentModuleObject),CoolBeam(CBNum)%Name, &
                     NodeID(CoolBeam(CBNum)%AirInNode),NodeID(CoolBeam(CBNum)%AirOutNode),'Air Nodes')
    CALL TestCompSet(TRIM(CurrentModuleObject),CoolBeam(CBNum)%Name, &
                     NodeID(CoolBeam(CBNum)%CWInNode),NodeID(CoolBeam(CBNum)%CWOutNode),'Water Nodes')

   !Setup the Cooled Beam reporting variables
    CALL SetupOutputVariable('Zone Air Terminal Beam Sensible Cooling Energy [J]', CoolBeam(CBNum)%BeamCoolingEnergy, &
                              'System','Sum',CoolBeam(CBNum)%Name, &
                               ResourceTypeKey='ENERGYTRANSFER',EndUseKey='COOLINGCOILS',GroupKey='System')
    CALL SetupOutputVariable('Zone Air Terminal Beam Chilled Water Energy [J]', CoolBeam(CBNum)%BeamCoolingEnergy, &
                              'System','Sum',CoolBeam(CBNum)%Name, &
                               ResourceTypeKey='PLANTLOOPCOOLINGDEMAND',EndUseKey='COOLINGCOILS',GroupKey='System')
    CALL SetupOutputVariable('Zone Air Terminal Beam Sensible Cooling Rate [W]', CoolBeam(CBNum)%BeamCoolingRate, &
                              'System','Average',CoolBeam(CBNum)%Name)
    CALL SetupOutputVariable('Zone Air Terminal Supply Air Sensible Cooling Energy [J]', CoolBeam(CBNum)%SupAirCoolingEnergy, &
                              'System','Sum',CoolBeam(CBNum)%Name)
    CALL SetupOutputVariable('Zone Air Terminal Supply Air Sensible Cooling Rate [W]', CoolBeam(CBNum)%SupAirCoolingRate, &
                              'System','Average',CoolBeam(CBNum)%Name)
    CALL SetupOutputVariable('Zone Air Terminal Supply Air Sensible Heating Energy [J]', CoolBeam(CBNum)%SupAirHeatingEnergy, &
                              'System','Sum',CoolBeam(CBNum)%Name)
    CALL SetupOutputVariable('Zone Air Terminal Supply Air Sensible Heating Rate [W]', CoolBeam(CBNum)%SupAirHeatingRate, &
                              'System','Average',CoolBeam(CBNum)%Name)

    ! Fill the Zone Equipment data with the supply air inlet node number of this unit.
    AirNodeFound=.false.
    DO CtrlZone = 1,NumOfZones
      IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
      DO SupAirIn = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
        IF (CoolBeam(CBNum)%AirOutNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(SupAirIn)) THEN
          ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%InNode = CoolBeam(CBNum)%AirInNode
          ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode = CoolBeam(CBNum)%AirOutNode
          AirNodeFound=.true.
          EXIT
        END IF
      END DO
    END DO
    IF (.not. AirNodeFound) THEN
      CALL ShowSevereError('The outlet air node from the '//TRIM(CurrentModuleObject)//' = ' &
                           //TRIM(CoolBeam(CBNum)%Name))
      CALL ShowContinueError('did not have a matching Zone Equipment Inlet Node, Node ='//TRIM(Alphas(5)))
      ErrorsFound=.true.
    ENDIF

  END DO

  DO CBNum = 1,NumCB
    DO ADUNum = 1,NumAirDistUnits
      IF (CoolBeam(CBNum)%AirOutNode == AirDistUnit(ADUNum)%OutletNodeNum) THEN
        CoolBeam(CBNum)%ADUNum = ADUNum
      END IF
    END DO
    ! one assumes if there isn't one assigned, it's an error?
    IF (CoolBeam(CBNum)%ADUNum == 0) THEN
      CALL ShowSevereError(RoutineName//'No matching Air Distribution Unit, for Unit = ['//  &
         TRIM(CurrentModuleObject)//','//TRIM(CoolBeam(CBNum)%Name)//'].')
      CALL ShowContinueError('...should have outlet node='//TRIM(NodeID(CoolBeam(CBNum)%AirOutNode)))
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

END SUBROUTINE GetCoolBeams

SUBROUTINE InitCoolBeam(CBNum,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   February 6, 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initialization of the cooled beam units

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment,  ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList
  USE DataDefineEquip,    ONLY: AirDistUnit
  USE InputProcessor,     ONLY: SameString
  USE DataPlant,          ONLY: PlantLoop, ScanPlantLoopsForObject, TypeOf_CooledBeamAirTerminal
  USE FluidProperties,    ONLY: GetDensityGlycol
  USE PlantUtilities,     ONLY: InitComponentNodes, SetComponentFlowRate

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: CBNum ! number of the current cooled beam unit being simulated
  LOGICAL, INTENT (IN) :: FirstHVACIteration ! TRUE if first air loop solution this HVAC step

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: InAirNode       ! supply air inlet node number
  INTEGER             :: OutAirNode      ! unit air outlet node
  INTEGER             :: InWaterNode     ! unit inlet chilled water node
  INTEGER             :: OutWaterNode    ! unit outlet chilled water node
  REAL(r64)           :: RhoAir          ! air density at outside pressure and standard temperature and humidity
  LOGICAL,SAVE             :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MySizeFlag
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: PlantLoopScanFlag
  REAL(r64)           :: rho ! local fluid density
  LOGICAL,SAVE        :: ZoneEquipmentListChecked = .false.  ! True after the Zone Equipment List has been checked for items
  Integer             :: Loop  ! Loop checking control variable
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject
  LOGICAL             :: errFlag

  CurrentModuleObject = 'AirTerminal:SingleDuct:ConstantVolume:CooledBeam'
  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN

    ALLOCATE(MyEnvrnFlag(NumCB))
    ALLOCATE(MySizeFlag(NumCB))
    ALLOCATE(PlantLoopScanFlag(NumCB))
    MyEnvrnFlag       = .TRUE.
    MySizeFlag        = .TRUE.
    PlantLoopScanFlag = .TRUE.
    MyOneTimeFlag     = .FALSE.

  END IF

  IF (PlantLoopScanFlag(CBNum) .and. ALLOCATED(PlantLoop)) THEN
      errFlag=.false.
      CALL ScanPlantLoopsForObject(CoolBeam(CBNum)%Name,         &
                                   TypeOf_CooledBeamAirTerminal, &
                                   CoolBeam(CBNum)%CWLoopNum,          &
                                   CoolBeam(CBNum)%CWLoopSideNum,      &
                                   CoolBeam(CBNum)%CWBranchNum,        &
                                   CoolBeam(CBNum)%CWCompNum,          &
                                   errFlag=errFlag)
      IF (errFlag) THEN
        CALL ShowFatalError('InitCoolBeam: Program terminated for previous conditions.')
      ENDIF
      PlantLoopScanFlag(CBNum) = .FALSE.

  ENDIF

  IF (.not. ZoneEquipmentListChecked .and. ZoneEquipInputsFilled) THEN
    ZoneEquipmentListChecked=.true.
    ! Check to see if there is a Air Distribution Unit on the Zone Equipment List
    DO Loop=1,NumCB
      IF (CoolBeam(Loop)%ADUNum == 0) CYCLE
      IF (CheckZoneEquipmentList('ZONEHVAC:AIRDISTRIBUTIONUNIT',AirDistUnit(CoolBeam(Loop)%ADUNum)%Name)) CYCLE
      CALL ShowSevereError('InitCoolBeam: ADU=[Air Distribution Unit,'//  &
           TRIM(AirDistUnit(CoolBeam(Loop)%ADUNum)%Name)//  &
           '] is not on any ZoneHVAC:EquipmentList.')
      CALL ShowContinueError('...Unit=['//TRIM(CurrentModuleObject)//','//TRIM(CoolBeam(Loop)%Name)//  &
           '] will not be simulated.')
    ENDDO
  ENDIF

  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(CBNum) .AND. .NOT. PlantLoopScanFlag(CBNum)) THEN

    CALL SizeCoolBeam(CBNum)

    InWaterNode = CoolBeam(CBNum)%CWInNode
    OutWaterNode = CoolBeam(CBNum)%CWOutNode
    rho = GetDensityGlycol(PlantLoop(CoolBeam(CBNum)%CWLoopNum)%FluidName, &
                                     InitConvTemp, &
                                     PlantLoop(CoolBeam(CBNum)%CWLoopNum)%FluidIndex, &
                                     'InitCoolBeam')
    CoolBeam(CBNum)%MaxCoolWaterMassFlow = rho * CoolBeam(CBNum)%MaxCoolWaterVolFlow
    CALL InitComponentNodes(0.d0, CoolBeam(CBNum)%MaxCoolWaterMassFlow, &
                                   InWaterNode, OutWaterNode, &
                                   CoolBeam(CBNum)%CWLoopNum,          &
                                   CoolBeam(CBNum)%CWLoopSideNum,      &
                                   CoolBeam(CBNum)%CWBranchNum,        &
                                   CoolBeam(CBNum)%CWCompNum)
    MySizeFlag(CBNum) = .FALSE.

  END IF

  ! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(CBNum)) THEN
    RhoAir = StdRhoAir
    InAirNode = CoolBeam(CBNum)%AirInNode
    OutAirNode = CoolBeam(CBNum)%AirOutNode
    ! set the mass flow rates from the input volume flow rates
    CoolBeam(CBNum)%MaxAirMassFlow = RhoAir * CoolBeam(CBNum)%MaxAirVolFlow
    Node(InAirNode)%MassFlowRateMax = CoolBeam(CBNum)%MaxAirMassFlow
    Node(OutAirNode)%MassFlowRateMax = CoolBeam(CBNum)%MaxAirMassFlow
    Node(InAirNode)%MassFlowRateMin = 0.0d0
    Node(OutAirNode)%MassFlowRateMin = 0.0d0

    InWaterNode = CoolBeam(CBNum)%CWInNode
    OutWaterNode = CoolBeam(CBNum)%CWOutNode
    Call InitComponentNodes(0.d0, CoolBeam(CBNum)%MaxCoolWaterMassFlow, &
                                   InWaterNode, OutWaterNode, &
                                   CoolBeam(CBNum)%CWLoopNum,          &
                                   CoolBeam(CBNum)%CWLoopSideNum,      &
                                   CoolBeam(CBNum)%CWBranchNum,        &
                                   CoolBeam(CBNum)%CWCompNum)



    MyEnvrnFlag(CBNum) = .FALSE.
  END IF ! end one time inits

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(CBNum) = .true.
  ENDIF

  InAirNode = CoolBeam(CBNum)%AirInNode
  OutAirNode = CoolBeam(CBNum)%AirOutNode

  ! Do the start of HVAC time step initializations
  IF (FirstHVACIteration) THEN
    ! check for upstream zero flow. If nonzero and schedule ON, set primary flow to max
    IF (GetCurrentScheduleValue(CoolBeam(CBNum)%SchedPtr) .GT. 0.0d0 .AND. Node(InAirNode)%MassFlowRate .GT. 0.0d0) THEN
      Node(InAirNode)%MassFlowRate = CoolBeam(CBNum)%MaxAirMassFlow
    ELSE
      Node(InAirNode)%MassFlowRate = 0.0d0
    END IF
    ! reset the max and min avail flows
    IF (GetCurrentScheduleValue(CoolBeam(CBNum)%SchedPtr) .GT. 0.0d0 .AND. Node(InAirNode)%MassFlowRateMaxAvail .GT. 0.0d0) THEN
        Node(InAirNode)%MassFlowRateMaxAvail = CoolBeam(CBNum)%MaxAirMassFlow
        Node(InAirNode)%MassFlowRateMinAvail = CoolBeam(CBNum)%MaxAirMassFlow
    ELSE
      Node(InAirNode)%MassFlowRateMaxAvail = 0.0d0
      Node(InAirNode)%MassFlowRateMinAvail = 0.0d0
    END IF
!Plant should do this    InWaterNode = CoolBeam(CBNum)%CWInNode
!    Node(InWaterNode)%MassFlowRateMaxAvail = CoolBeam(CBNum)%MaxCoolWaterMassFlow
!    Node(InWaterNode)%MassFlowRateMinAvail = 0.0
  END IF

  ! do these initializations every time step
  InWaterNode = CoolBeam(CBNum)%CWInNode
  CoolBeam(CBNum)%TWin = Node(InWaterNode)%Temp
  CoolBeam(CBNum)%SupAirCoolingRate = 0.0d0
  CoolBeam(CBNum)%SupAirHeatingRate = 0.0d0

  ! CoolBeam(CBNum)%BeamFlow = Node(InAirNode)%MassFlowRate / (StdRhoAir*CoolBeam(CBNum)%NumBeams)

  RETURN

END SUBROUTINE InitCoolBeam

SUBROUTINE SizeCoolBeam(CBNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   February 10, 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing cooled beam units for which flow rates have not been
          ! specified in the input

          ! METHODOLOGY EMPLOYED:
          ! Accesses zone sizing array for air flow rates and zone and plant sizing arrays to
          ! calculate coil water flow rates.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE InputProcessor
  USE DataGlobals,         ONLY: AutoCalculate
  USE PlantUtilities,      ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
!  USE BranchInputManager,  ONLY: MyPlantSizingIndex
  USE FluidProperties,     ONLY: GetDensityGlycol, GetSpecificHeatGlycol
  USE DataPlant,           ONLY: PlantLoop, MyPlantSizingIndex

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: CBNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: PltSizCoolNum = 0    ! index of plant sizing object for the cooling loop
  INTEGER   :: NumBeams = 0         ! number of beams in the zone
  INTEGER   :: Iter=0               ! beam length iteration index
  REAL(r64) :: DesCoilLoad = 0.0d0    ! total cooling capacity of the beams in the zone [W]
  REAL(r64) :: DesLoadPerBeam = 0.0d0 ! cooling capacity per individual beam [W]
  REAL(r64) :: DesAirVolFlow = 0.0d0  ! design total supply air flow rate [m3/s]
  REAL(r64) :: DesAirFlowPerBeam = 0.0d0   ! design supply air volumetric flow per beam [m3/s]
  REAL(r64) :: RhoAir = 0.0d0
  REAL(r64) :: CpAir = 0.0d0
  REAL(r64) :: WaterVel = 0.0d0            ! design water velocity in beam
  REAL(r64) :: IndAirFlowPerBeamL = 0.0d0  ! induced volumetric air flow rate per beam length [m3/s-m]
  REAL(r64) :: DT = 0.0d0                  ! air - water delta T [C]
  REAL(r64) :: LengthX = 0.0d0             ! test value for beam length [m]
  REAL(r64) :: Length = 0.0d0              ! beam length [m]
  REAL(r64) :: ConvFlow = 0.0d0            ! convective and induced air mass flow rate across beam per beam plan area [kg/s-m2]
  REAL(r64) :: K = 0.0d0                   ! coil (beam) heat transfer coefficient [W/m2-K]
  REAL(r64) :: WaterVolFlowPerBeam = 0.0d0 ! Cooling water volumetric flow per beam [m3]
  LOGICAL   :: ErrorsFound
  REAL(r64) :: rho ! local fluid density
  REAL(r64) :: Cp  ! local fluid specific heat

  PltSizCoolNum = 0
  DesAirVolFlow = 0.0d0
  CpAir = 0.0d0
  RhoAir = StdRhoAir
  ErrorsFound = .FALSE.
  ! find the appropriate Plant Sizing object
  IF (CoolBeam(CBNum)%MaxAirVolFlow == AutoSize .or. CoolBeam(CBNum)%BeamLength == AutoSize) THEN
    PltSizCoolNum = MyPlantSizingIndex("cooled beam unit", CoolBeam(CBNum)%Name, CoolBeam(CBNum)%CWInNode, &
                                     CoolBeam(CBNum)%CWOutNode, ErrorsFound)
  ENDIF

  IF (CoolBeam(CBNum)%Kin == AutoCalculate) THEN
    IF (CoolBeam(CBNum)%CBType_Num == Passive_Cooled_Beam) THEN
      CoolBeam(CBNum)%Kin = 0.0d0
    ELSE
      CoolBeam(CBNum)%Kin = 2.0d0
    END IF
    CALL ReportSizingOutput(CoolBeam(CBNum)%UnitType, CoolBeam(CBNum)%Name, &
                            'Coefficient of Induction Kin', CoolBeam(CBNum)%Kin)

  END IF

  IF (CoolBeam(CBNum)%MaxAirVolFlow == AutoSize) THEN

    IF (CurZoneEqNum > 0) THEN

      CALL CheckZoneSizing(CoolBeam(CBNum)%UnitType, CoolBeam(CBNum)%Name)
      CoolBeam(CBNum)%MaxAirVolFlow = MAX(TermUnitFinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                                           TermUnitFinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
      IF (CoolBeam(CBNum)%MaxAirVolFlow < SmallAirVolFlow) THEN
        CoolBeam(CBNum)%MaxAirVolFlow = 0.0d0
      END IF
      CALL ReportSizingOutput(CoolBeam(CBNum)%UnitType, CoolBeam(CBNum)%Name, &
                              'Supply Air Flow Rate [m3/s]', CoolBeam(CBNum)%MaxAirVolFlow)
    END IF

  END IF

  IF (CoolBeam(CBNum)%MaxCoolWaterVolFlow == AutoSize) THEN

    IF (CurZoneEqNum > 0) THEN

      CALL CheckZoneSizing(CoolBeam(CBNum)%UnitType, CoolBeam(CBNum)%Name)

      IF (PltSizCoolNum > 0) THEN

        IF (FinalZoneSizing(CurZoneEqNum)%DesCoolMassFlow >= SmallAirVolFlow) THEN
          DesAirVolFlow = CoolBeam(CBNum)%MaxAirVolFlow
          CpAir = PsyCpAirFnWTdb(FinalZoneSizing(CurZoneEqNum)%CoolDesHumRat,FinalZoneSizing(CurZoneEqNum)%CoolDesTemp)
          ! the design cooling coil load is the zone load minus whatever the central system does. Note that
          ! DesCoolCoilInTempTU is really the primary air inlet temperature for the unit.
          IF (FinalZoneSizing(CurZoneEqNum)%ZoneTempAtCoolPeak > 0.0d0) THEN
            DesCoilLoad = FinalZoneSizing(CurZoneEqNum)%DesCoolLoad - CpAir*RhoAir*DesAirVolFlow* &
              (FinalZoneSizing(CurZoneEqNum)%ZoneTempAtCoolPeak - FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTempTU)
          ELSE
            DesCoilLoad = CpAir*RhoAir*DesAirVolFlow*(FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTempTU   &
                                       - ZoneSizThermSetPtHi(CurZoneEqNum))
          END IF

          rho = GetDensityGlycol(PlantLoop(CoolBeam(CBNum)%CWLoopNum)%FluidName,&
                                  InitConvTemp, &
                                  PlantLoop(CoolBeam(CBNum)%CWLoopNum)%FluidIndex,&
                                  'SizeCoolBeam')

          Cp  = GetSpecificHeatGlycol( PlantLoop(CoolBeam(CBNum)%CWLoopNum)%FluidName,&
                                  InitConvTemp, &
                                  PlantLoop(CoolBeam(CBNum)%CWLoopNum)%FluidIndex,&
                                  'SizeCoolBeam')

          CoolBeam(CBNum)%MaxCoolWaterVolFlow = DesCoilLoad / &
                                                ( (CoolBeam(CBNum)%DesOutletWaterTemp - CoolBeam(CBNum)%DesInletWaterTemp) * &
                                                 Cp * rho )
          CoolBeam(CBNum)%MaxCoolWaterVolFlow = MAX(CoolBeam(CBNum)%MaxCoolWaterVolFlow,0.0d0)
          IF (CoolBeam(CBNum)%MaxCoolWaterVolFlow < SmallWaterVolFlow) THEN
            CoolBeam(CBNum)%MaxCoolWaterVolFlow = 0.0d0
          END IF
        ELSE
          CoolBeam(CBNum)%MaxCoolWaterVolFlow = 0.0d0
        END IF

        CALL ReportSizingOutput(CoolBeam(CBNum)%UnitType, CoolBeam(CBNum)%Name, &
                                'Maximum Total Chilled Water Flow Rate [m3/s]', CoolBeam(CBNum)%MaxCoolWaterVolFlow)
      ELSE
        CALL ShowSevereError('Autosizing of water flow requires a cooling loop Sizing:Plant object')
        CALL ShowContinueError('Occurs in' //  TRIM(CoolBeam(CBNum)%UnitType) // ' Object='//TRIM(CoolBeam(CBNum)%Name))
        ErrorsFound = .TRUE.
      END IF

    END IF

  END IF

  IF (CoolBeam(CBNum)%NumBeams == AutoSize) THEN
    rho = GetDensityGlycol(PlantLoop(CoolBeam(CBNum)%CWLoopNum)%FluidName,&
                                  InitConvTemp, &
                                  PlantLoop(CoolBeam(CBNum)%CWLoopNum)%FluidIndex,&
                                  'SizeCoolBeam')

    NumBeams = INT( CoolBeam(CBNum)%MaxCoolWaterVolFlow * rho / NomMassFlowPerBeam ) + 1
    CoolBeam(CBNum)%NumBeams = REAL(NumBeams,r64)
    CALL ReportSizingOutput(CoolBeam(CBNum)%UnitType, CoolBeam(CBNum)%Name, &
                                'Number of Beams', CoolBeam(CBNum)%NumBeams)
  END IF

  IF (CoolBeam(CBNum)%BeamLength == AutoSize) THEN

    IF (CurZoneEqNum > 0) THEN

      CALL CheckZoneSizing(CoolBeam(CBNum)%UnitType, CoolBeam(CBNum)%Name)

      IF (PltSizCoolNum > 0) THEN
        rho = GetDensityGlycol(PlantLoop(CoolBeam(CBNum)%CWLoopNum)%FluidName,&
                                InitConvTemp, &
                                PlantLoop(CoolBeam(CBNum)%CWLoopNum)%FluidIndex,&
                                'SizeCoolBeam')

        Cp  = GetSpecificHeatGlycol( PlantLoop(CoolBeam(CBNum)%CWLoopNum)%FluidName,&
                                InitConvTemp, &
                                PlantLoop(CoolBeam(CBNum)%CWLoopNum)%FluidIndex,&
                                'SizeCoolBeam')
        DesCoilLoad = CoolBeam(CBNum)%MaxCoolWaterVolFlow *   &
           (CoolBeam(CBNum)%DesOutletWaterTemp - CoolBeam(CBNum)%DesInletWaterTemp) &
            * Cp  * rho
        IF (DesCoilLoad > 0.0d0) THEN
          DesLoadPerBeam = DesCoilLoad / NumBeams
          DesAirFlowPerBeam = CoolBeam(CBNum)%MaxAirVolFlow / NumBeams
          WaterVolFlowPerBeam = CoolBeam(CBNum)%MaxCoolWaterVolFlow / NumBeams
          WaterVel = WaterVolFlowPerBeam / (Pi * (CoolBeam(CBNum)%InDiam)**2 / 4.0d0)
          IF (FinalZoneSizing(CurZoneEqNum)%ZoneTempAtCoolPeak > 0.0d0) THEN
            DT = FinalZoneSizing(CurZoneEqNum)%ZoneTempAtCoolPeak - 0.5d0*  &
               (CoolBeam(CBNum)%DesInletWaterTemp + CoolBeam(CBNum)%DesOutletWaterTemp)
            IF (DT <= 0.0d0) THEN
              DT = 7.8d0
            END IF
          ELSE
            DT = 7.8d0
          END IF
          LengthX = 1.0d0
          DO Iter=1,100
            IndAirFlowPerBeamL = CoolBeam(CBNum)%K1 * DT**CoolBeam(CBNum)%n + CoolBeam(CBNum)%Kin * DesAirFlowPerBeam / LengthX
            ConvFlow = (IndAirFlowPerBeamL /CoolBeam(CBNum)%a0)*RhoAir
            IF (WaterVel > MinWaterVel) THEN
              K = CoolBeam(CBNum)%a * DT**CoolBeam(CBNum)%n1 * ConvFlow**CoolBeam(CBNum)%n2 * WaterVel**CoolBeam(CBNum)%n3
            ELSE
              K = CoolBeam(CBNum)%a * DT**CoolBeam(CBNum)%n1 * ConvFlow**CoolBeam(CBNum)%n2 * MinWaterVel**CoolBeam(CBNum)%n3 &
                  * (WaterVel/MinWaterVel)
            END IF
            Length = DesLoadPerBeam / (K * CoolBeam(CBNum)%CoilArea * DT)
            IF (CoolBeam(CBNum)%Kin <= 0.0d0) EXIT
            ! Check for convergence
            IF (ABS(Length - LengthX) > 0.01d0) THEN
              ! New guess for length
              LengthX = LengthX + 0.5d0*(Length - LengthX)
            ELSE
              EXIT ! convergence achieved
            END IF
          END DO
        ELSE
          Length = 0.0d0
        END IF
        CoolBeam(CBNum)%BeamLength = Length
        CoolBeam(CBNum)%BeamLength = MAX(CoolBeam(CBNum)%BeamLength,1.0d0)
        CALL ReportSizingOutput(CoolBeam(CBNum)%UnitType, CoolBeam(CBNum)%Name, &
                                'Beam Length [m]', CoolBeam(CBNum)%BeamLength)
      ELSE
        CALL ShowSevereError('Autosizing of cooled beam length requires a cooling loop Sizing:Plant object')
        CALL ShowContinueError('Occurs in' //  TRIM(CoolBeam(CBNum)%UnitType) // ' Object='//TRIM(CoolBeam(CBNum)%Name))
        ErrorsFound = .TRUE.
      END IF

    END IF

  END IF

    ! save the design water volumetric flow rate for use by the water loop sizing algorithms
  IF (CoolBeam(CBNum)%MaxCoolWaterVolFlow > 0.0d0) THEN
    CALL RegisterPlantCompDesignFlow(CoolBeam(CBNum)%CWInNode,CoolBeam(CBNum)%MaxCoolWaterVolFlow)
  END IF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding cooled beam sizing errors cause program termination')
  END IF

  RETURN

END SUBROUTINE SizeCoolBeam

SUBROUTINE ControlCoolBeam(CBNum,ZoneNum,ZoneNodeNum,FirstHVACIteration,NonAirSysOutput)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Feb 12, 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate a cooled beam unit;

          ! METHODOLOGY EMPLOYED:
          ! (1) From the zone load and the Supply air inlet conditions calculate the beam load
          ! (2) If there is a beam load, vary the water flow rate to match the beam load

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands
  USE General, ONLY: SolveRegulaFalsi
  USE PlantUtilities, ONLY:SetComponentFlowRate
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT (IN)  :: FirstHVACIteration ! TRUE if 1st HVAC simulation of system timestep
  INTEGER, INTENT (IN)  :: CBNum              ! number of the current unit being simulated
  INTEGER, INTENT (IN)  :: ZoneNum            ! number of zone being served
  INTEGER, INTENT (IN)  :: ZoneNodeNum           ! zone node number
  REAL(r64), INTENT(OUT) :: NonAirSysOutput   ! convective cooling by the beam system [W]

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: QZnReq            ! heating or cooling needed by zone [Watts]
  REAL(r64)    :: QToHeatSetPt      ! [W]  remaining load to heating setpoint
  REAL(r64)    :: QToCoolSetPt      ! [W]  remaining load to cooling setpoint
  REAL(r64)    :: QMin=0.0d0          ! cooled beam output at minimum water flow [W]
  REAL(r64)    :: QMax=0.0d0          ! cooled beam output at maximum water flow [W]
  REAL(r64)    :: QSup=0.0d0          ! heating or cooling by supply air [W]
  REAL(r64)    :: PowerMet=0.0d0      ! power supplied
  REAL(r64)    :: CWFlow=0.0d0        ! cold water flow [kg/s]
  REAL(r64)    :: AirMassFlow=0.0d0   ! air mass flow rate for the cooled beam system [kg/s]
  REAL(r64)    :: MaxColdWaterFlow=0.0d0   ! max water mass flow rate for the cooled beam system [kg/s]
  REAL(r64)    :: MinColdWaterFlow=0.0d0   ! min water mass flow rate for the cooled beam system [kg/s]
  REAL(r64)    :: CpAirZn=0.0d0       ! specific heat of air at zone conditions [J/kg-C]
  REAL(r64)    :: CpAirSys=0.0d0      ! specific heat of air at supply air conditions [J/kg-C]
  REAL(r64)    :: TWOut=0.0d0         ! outlet water tamperature [C]
  REAL(r64)    :: NumBeams=0.0d0      ! number of beams
  INTEGER      :: ControlNode       ! the water inlet node
  INTEGER      :: InAirNode         ! the air inlet node
  LOGICAL      :: UnitOn            ! TRUE if unit is on
  REAL(r64), DIMENSION(5)  :: Par
  INTEGER :: SolFlag
  REAL(r64) :: ErrTolerance

  UnitOn = .TRUE.
  PowerMet = 0.0d0
  InAirNode = CoolBeam(CBNum)%AirInNode
  ControlNode = CoolBeam(CBNum)%CWInNode
  AirMassFlow = Node(InAirNode)%MassFlowRateMaxAvail
  QZnReq = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired
  QToHeatSetPt=ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP
  QToCoolSetPt=ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToCoolSP
  CpAirZn = PsyCpAirFnWTdb(Node(ZoneNodeNum)%HumRat,Node(ZoneNodeNum)%Temp)
  CpAirSys = PsyCpAirFnWTdb(Node(InAirNode)%HumRat,Node(InAirNode)%Temp)
  MaxColdWaterFlow = CoolBeam(CBNum)%MaxCoolWaterMassFlow
  CALL SetComponentFlowRate(MaxColdWaterFlow,                  &
                                 CoolBeam(CBNum)%CWInNode,    &
                                 CoolBeam(CBNum)%CWOutNode,    &
                                 CoolBeam(CBNum)%CWLoopNum,       &
                                 CoolBeam(CBNum)%CWLoopSideNum,    &
                                 CoolBeam(CBNum)%CWBranchNum,     &
                                 CoolBeam(CBNum)%CWCompNum)
  MinColdWaterFlow = 0.d0
  CALL SetComponentFlowRate(MinColdWaterFlow,                  &
                                 CoolBeam(CBNum)%CWInNode,    &
                                 CoolBeam(CBNum)%CWOutNode,    &
                                 CoolBeam(CBNum)%CWLoopNum,       &
                                 CoolBeam(CBNum)%CWLoopSideNum,    &
                                 CoolBeam(CBNum)%CWBranchNum,     &
                                 CoolBeam(CBNum)%CWCompNum)

  IF (GetCurrentScheduleValue(CoolBeam(CBNum)%SchedPtr) .LE. 0.0d0) UnitOn = .FALSE.
  IF (MaxColdWaterFlow <= SmallMassFlow) UnitOn = .FALSE.

 ! Set the unit's air inlet nodes mass flow rates
  Node(InAirNode)%MassFlowRate = AirMassFlow
  ! set the air volumetric flow rate per beam
  CoolBeam(CBNum)%BeamFlow = Node(InAirNode)%MassFlowRate / (StdRhoAir*CoolBeam(CBNum)%NumBeams)
  ! fire the unit at min water flow
  CALL CalcCoolBeam(CBNum,ZoneNodeNum,MinColdWaterFlow,QMin,TWOut)
  ! cooling by supply air
  QSup = AirMassFlow * (CpAirSys*Node(InAirNode)%Temp - CpAirZn*Node(ZoneNodeNum)%Temp)
  ! load on the beams is QToCoolSetPt-QSup
  IF (UnitOn) THEN
    IF ((QToCoolSetPt-QSup) <  - SmallLoad) THEN
      ! There is a cooling demand on the cooled beam system.
      ! First, see if the system can meet the load
      CALL CalcCoolBeam(CBNum,ZoneNodeNum,MaxColdWaterFlow,QMax,TWOut)
      IF ((QMax < QToCoolSetPt - QSup - SmallLoad) .AND. (QMax /= QMin)) THEN
      ! The cooled beam system can meet the demand.
      ! Set up the iterative calculation of chilled water flow rate
        Par(1) = REAL(CBNum,r64)
        Par(2) = REAL(ZoneNodeNum,r64)
        Par(3) = QToCoolSetPt-QSup ! load to be met by the beams
        Par(4) = QMin
        Par(5) = QMax
        ErrTolerance = 0.01d0
        CALL SolveRegulaFalsi(ErrTolerance, 50, SolFlag, CWFlow, CoolBeamResidual, &
                              MinColdWaterFlow, MaxColdWaterFlow, Par)
        IF (SolFlag == -1) THEN
          CALL ShowWarningError('Cold water control failed in cooled beam unit '//TRIM(CoolBeam(CBNum)%Name))
          CALL ShowContinueError('  Iteration limit exceeded in calculating cold water mass flow rate')
        ELSE IF (SolFlag == -2) THEN
          CALL ShowWarningError('Cold water control failed in cooled beam unit '//TRIM(CoolBeam(CBNum)%Name))
          CALL ShowContinueError('  Bad cold water flow limits')
        END IF
      ELSE
        ! unit maxed out
        CWFlow = MaxColdWaterFlow
      END IF
    ELSE
      ! unit has no load
      CWFlow = MinColdWaterFlow
    END IF
  ELSE
    ! unit Off
    CWFlow = MinColdWaterFlow
  END IF
  ! Get the cooling output at the chosen water flow rate
  CALL CalcCoolBeam(CBNum,ZoneNodeNum,CWFlow,PowerMet,TWOut)
  CoolBeam(CBNum)%BeamCoolingRate = -PowerMet
  IF (QSup < 0.0d0) THEN
    CoolBeam(CBNum)%SupAirCoolingRate = ABS(QSup)
  ELSE
    CoolBeam(CBNum)%SupAirHeatingRate = QSup
  END IF
  CoolBeam(CBNum)%CoolWaterMassFlow = Node(ControlNode)%MassFlowRate
  CoolBeam(CBNum)%TWOut = TWOut
  CoolBeam(CBNum)%EnthWaterOut = Node(ControlNode)%Enthalpy + CoolBeam(CBNum)%BeamCoolingRate
!  Node(ControlNode)%MassFlowRate = CWFlow
  NonAirSysOutput = PowerMet

  RETURN

END SUBROUTINE ControlCoolBeam

SUBROUTINE CalcCoolBeam(CBNum,ZoneNode,CWFlow,LoadMet,TWOut)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Feb 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate a cooled beam given the chilled water flow rate

          ! METHODOLOGY EMPLOYED:
          ! Uses the cooled beam equations; iteratively varies water outlet  temperature
          ! until air-side and water-side cooling outputs match.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE PlantUtilities,  ONLY: SetComponentFlowRate
  USE FluidProperties, ONLY: GetSpecificHeatGlycol, GetDensityGlycol
  USE DataPlant,       ONLY: PlantLoop

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: CBNum         ! Unit index
  INTEGER, INTENT (IN)  :: ZoneNode           ! zone node number
  REAL(r64),    INTENT (IN)  :: CWFlow             ! cold water flow [kg/s]
  REAL(r64),    INTENT (OUT) :: LoadMet            ! load met by unit [W]
  REAL(r64),    INTENT (OUT) :: TWOut              ! chilled water outlet temperature [C]

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: Iter=0               ! TWOut iteration index
  REAL(r64) :: TWIn=0.0d0             ! Inlet water temperature [C]
  REAL(r64) :: ZTemp=0.0d0            ! zone air temperature [C]
  REAL(r64) :: WaterCoolPower=0.0d0   ! cooling power from water side [W]
  REAL(r64) :: DT=0.0d0               ! approximate air - water delta T [C]
  REAL(r64) :: IndFlow=0.0d0          ! induced air flow rate per beam length [m3/s-m]
  REAL(r64) :: CoilFlow=0.0d0         ! mass air flow rate of air passing through "coil" [kg/m2-s]
  REAL(r64) :: WaterVel=0.0d0         ! water velocity [m/s]
  REAL(r64) :: K=0.0d0                ! coil heat transfer coefficient [W/m2-K]
  REAL(r64) :: AirCoolPower=0.0d0     ! cooling power from the air side [W]
  REAL(r64) :: Diff                 ! difference between water side cooling power and air side cooling power [W]
  REAL(r64) :: CWFlowPerBeam=0.0d0    ! water mass flow rate per beam
  REAL(r64) :: Coeff = 0.0d0          ! iteration parameter
  REAL(r64) :: Delta = 0.0d0
  REAL(r64) :: mdot = 0.d0
  REAL(r64) :: Cp !local fluid specific heat
  REAL(r64) :: rho ! local fluid density


  !test CWFlow against plant
  mdot = CWFlow

  CALL SetComponentFlowRate(mdot,                  &
                                   CoolBeam(CBNum)%CWInNode,    &
                                   CoolBeam(CBNum)%CWOutNode,    &
                                   CoolBeam(CBNum)%CWLoopNum,       &
                                   CoolBeam(CBNum)%CWLoopSideNum,    &
                                   CoolBeam(CBNum)%CWBranchNum,     &
                                   CoolBeam(CBNum)%CWCompNum)


  CWFlowPerBeam = mdot / CoolBeam(CBNum)%NumBeams
  TWIn = CoolBeam(CBNum)%TWin

  Cp = GetSpecificHeatGlycol(PlantLoop(CoolBeam(CBNum)%CWLoopNum)%FluidName, &
                             TWin, &
                             PlantLoop(CoolBeam(CBNum)%CWLoopNum)%FluidIndex, &
                             'CalcCoolBeam')

  rho = GetDensityGlycol(PlantLoop(CoolBeam(CBNum)%CWLoopNum)%FluidName, &
                             TWin, &
                             PlantLoop(CoolBeam(CBNum)%CWLoopNum)%FluidIndex, &
                             'CalcCoolBeam')

  TWOut = TWIn + 2.0d0
  ZTemp = Node(ZoneNode)%Temp
  IF (mdot <= 0.0d0 .OR. TWIn <= 0.0d0) THEN
    LoadMet = 0.0d0
    TWOut = TWIn
    RETURN
  END IF
  DO Iter=1,200
    If (Iter > 50 .AND. Iter < 100) THEN
      Coeff = 0.1d0 * Coeff2
    ELSE IF (Iter > 100) THEN
      Coeff = 0.01d0 * Coeff2
    ELSE
      Coeff = Coeff2
    END IF

    WaterCoolPower = CWFlowPerBeam * Cp *(TWout-Twin)
    DT = MAX(ZTemp - 0.5d0*(TWin+TWout),0.0d0)
    IndFlow = CoolBeam(CBNum)%K1 * DT**CoolBeam(CBNum)%n + CoolBeam(CBNum)%Kin *   &
       CoolBeam(CBNum)%BeamFlow / CoolBeam(CBNum)%BeamLength
    CoilFlow = (IndFlow /CoolBeam(CBNum)%a0)*StdRhoAir
    WaterVel = CWFlowPerBeam/(rho * Pi * (CoolBeam(CBNum)%InDiam)**2 / 4.0d0)
    IF (WaterVel > MinWaterVel) THEN
      K = CoolBeam(CBNum)%a * DT**CoolBeam(CBNum)%n1 * CoilFlow**CoolBeam(CBNum)%n2 * WaterVel**CoolBeam(CBNum)%n3
    ELSE
      K = CoolBeam(CBNum)%a * DT**CoolBeam(CBNum)%n1 * CoilFlow**CoolBeam(CBNum)%n2 * MinWaterVel**CoolBeam(CBNum)%n3 &
                  * (WaterVel/MinWaterVel)
    END IF
    AirCoolPower = K * CoolBeam(CBNum)%CoilArea * DT * CoolBeam(CBNum)%BeamLength
    Diff = WaterCoolPower - AirCoolPower
    Delta = TWout*(ABS(Diff)/Coeff)
    IF (ABS(Diff) > 0.1d0) THEN
      IF (Diff < 0.0d0) THEN
        TWout = TWout + Delta ! increase TWout
        IF (TWout > ZTemp) THEN ! check that water outlet temperature is less than zone temperature
          WaterCoolPower = 0.0d0
          TWout = ZTemp
          EXIT
        END IF
      ELSE
        TWout = TWout - Delta ! Decrease TWout
        IF (TWout < TWin) THEN
          TWout = TWin
        END IF
      END IF
    ELSE
      ! water and air side outputs have converged
      EXIT
    END IF
  END DO
  LoadMet = -WaterCoolPower * CoolBeam(CBNum)%NumBeams

  RETURN

END SUBROUTINE CalcCoolBeam

FUNCTION CoolBeamResidual(CWFlow, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   February 2009
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (Requested Unit Load - Unit Output) / Max Unit Output
          ! Unit Output depends on the cold water flow rate which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls CalcCoolBeam, and calculates the residual as defined above.

          ! REFERENCES:

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: CWFlow ! cold water flow rate in kg/s
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: CBIndex
  INTEGER :: ZoneNodeIndex
  REAL(r64)    :: UnitOutput=0.0d0
  REAL(r64)    :: TWOut=0.0d0

  CBIndex = INT(Par(1))
  ZoneNodeIndex = INT(Par(2))
  CALL CalcCoolBeam(CBIndex,ZoneNodeIndex,CWFlow,UnitOutput,TWOut)
  Residuum = (Par(3) - UnitOutput) / (Par(5) - Par(4))

  RETURN
END FUNCTION CoolBeamResidual

SUBROUTINE UpdateCoolBeam(CBNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Feb 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the cooled beam unit outlet nodes

          ! METHODOLOGY EMPLOYED:
          ! Data is moved from the cooled beam unit data structure to the unit outlet nodes.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataContaminantBalance, ONLY: Contaminant
  USE PlantUtilities,         ONLY: SafeCopyPlantNode

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(In) :: CBNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  Integer             :: AirInletNode
  Integer             :: WaterInletNode
  Integer             :: AirOutletNode
  Integer             :: WaterOutletNode


  AirInletNode    = CoolBeam(CBNum)%AirInNode
  WaterInletNode  = CoolBeam(CBNum)%CWInNode
  AirOutletNode   = CoolBeam(CBNum)%AirOutNode
  WaterOutletNode = CoolBeam(CBNum)%CWOutNode

  ! Set the outlet air nodes of the unit; note that all quantities are unchanged
  Node(AirOutletNode)%MassFlowRate = Node(AirInletNode)%MassFlowRate
  Node(AirOutletNode)%Temp         = Node(AirInletNode)%Temp
  Node(AirOutletNode)%HumRat       = Node(AirInletNode)%HumRat
  Node(AirOutletNode)%Enthalpy     = Node(AirInletNode)%Enthalpy

  ! Set the outlet water nodes for the unit
!  Node(WaterOutletNode)%MassFlowRate = CoolBeam(CBNum)%CoolWaterMassFlow
  CALL SafeCopyPlantNode(WaterInletNode, WaterOutletNode)

  Node(WaterOutletNode)%Temp         = CoolBeam(CBNum)%TWOut
  Node(WaterOutletNode)%Enthalpy     = CoolBeam(CBNum)%EnthWaterOut

  ! Set the air outlet nodes for properties that just pass through & not used
  Node(AirOutletNode)%Quality             = Node(AirInletNode)%Quality
  Node(AirOutletNode)%Press               = Node(AirInletNode)%Press
  Node(AirOutletNode)%MassFlowRateMin     = Node(AirInletNode)%MassFlowRateMin
  Node(AirOutletNode)%MassFlowRateMax     = Node(AirInletNode)%MassFlowRateMax
  Node(AirOutletNode)%MassFlowRateMinAvail= Node(AirInletNode)%MassFlowRateMinAvail
  Node(AirOutletNode)%MassFlowRateMaxAvail= Node(AirInletNode)%MassFlowRateMaxAvail

  ! Set the outlet nodes for properties that just pass through & not used
!  Node(WaterOutletNode)%Quality             = Node(WaterInletNode)%Quality
!  Node(WaterOutletNode)%Press               = Node(WaterInletNode)%Press
!  Node(WaterOutletNode)%HumRat              = Node(WaterInletNode)%HumRat
!  Node(WaterOutletNode)%MassFlowRateMin     = Node(WaterInletNode)%MassFlowRateMin
!  Node(WaterOutletNode)%MassFlowRateMax     = Node(WaterInletNode)%MassFlowRateMax
!  Node(WaterOutletNode)%MassFlowRateMinAvail= Node(WaterInletNode)%MassFlowRateMinAvail
!  Node(WaterOutletNode)%MassFlowRateMaxAvail= Node(WaterInletNode)%MassFlowRateMaxAvail

!  IF (CoolBeam(CBNum)%CoolWaterMassFlow.EQ.0.0) THEN
!    Node(WaterInletNode)%MassFlowRateMinAvail= 0.0
!    Node(WaterOutletNode)%MassFlowRateMinAvail= 0.0
!  END IF

  IF (Contaminant%CO2Simulation) Then
    Node(AirOutletNode)%CO2 = Node(AirInletNode)%CO2
  End If

  IF (Contaminant%GenericContamSimulation) Then
    Node(AirOutletNode)%GenContam = Node(AirInletNode)%GenContam
  End If

  RETURN

END SUBROUTINE UpdateCoolBeam

SUBROUTINE ReportCoolBeam(CBNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Feb 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the report variable for the cooled beam units

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: CBNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

REAL(r64) :: ReportingConstant

   ReportingConstant = TimeStepSys*SecInHour
 ! report the WaterCoil energy from this component
   CoolBeam(CBNum)%BeamCoolingEnergy = CoolBeam(CBNum)%BeamCoolingRate*ReportingConstant
   CoolBeam(CBNum)%SupAirCoolingEnergy = CoolBeam(CBNum)%SupAirCoolingRate*ReportingConstant
   CoolBeam(CBNum)%SupAirHeatingEnergy = CoolBeam(CBNum)%SupAirHeatingRate*ReportingConstant

  RETURN
END Subroutine ReportCoolBeam




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

END MODULE HVACCooledBeam
