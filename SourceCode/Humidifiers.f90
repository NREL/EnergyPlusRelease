MODULE Humidifiers

  ! Module containing the routines dealing with humidifiers

  ! MODULE INFORMATION:
  !       AUTHOR         Fred Buhl
  !       DATE WRITTEN   September 2000
  !       MODIFIED       B Griffith, Aug. 2006 added water system interactions
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and routines required to model humidifier
  ! components in the EnergyPlus HVAC simulation

  ! METHODOLOGY EMPLOYED:
  ! The humidifier encompasses not just the component but also its
  ! control. The humidifier adds moisture to its air inlet to meet
  ! the HumRatMin setpoint at its exit node. The HumRatMin is set by
  ! an external setpoint manager.

  ! REFERENCES: ASHRAE HVAC 2 Toolkit, page 4-112

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
  USE DataPrecisionGlobals
  USE DataGlobals, ONLY: MaxNameLength, BeginEnvrnFlag, InitConvTemp, SysSizingCalc, SecInHour, ScheduleAlwaysOn
  USE DataInterfaces, ONLY: SetupOutputVariable, ShowWarningError, ShowSevereError, ShowFatalError, &
                       ShowContinueError
  USE DataLoopNode
  USE DataEnvironment, ONLY: OutBaroPress
  USE DataHVACGlobals, ONLY: SmallMassFlow, SetPointErrorFlag

  ! Use statements for access to subroutines in other modules
  USE ScheduleManager

  IMPLICIT NONE         ! Enforce explicit typing of all variables

  PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
  INTEGER, PARAMETER :: Humidifier_Steam_Electric=1

  CHARACTER(len=*), DIMENSION(1), PARAMETER :: HumidifierType='Humidifier:Steam:Electric'

  ! DERIVED TYPE DEFINITIONS
  TYPE HumidifierData
    CHARACTER(len=MaxNameLength) :: Name              =' ' ! unique name of component
!    CHARACTER(len=MaxNameLength) :: HumType           =' ' ! Type of humidifier
    INTEGER                      :: HumType_Code      =0   ! Pointer to Humidifier in list of humidifiers
    INTEGER                      :: EquipIndex        =0   ! Pointer to Humidifier in list of humidifiers
    CHARACTER(len=MaxNameLength) :: Sched             =' ' ! name of availability schedule
    INTEGER                      :: SchedPtr          =0   ! index of availability schedule
    REAL(r64)                    :: NomCapVol         =0.0d0 ! nominal capacity [m3/s of water]
    REAL(r64)                    :: NomCap            =0.0d0 ! nominal capacity [kg/s of water]
    REAL(r64)                    :: NomPower          =0.0d0 ! power consumption at full output [watts]
    REAL(r64)                    :: FanPower          =0.0d0 ! nominal fan power [watts]
    REAL(r64)                    :: StandbyPower      =0.0d0 ! standby power consumption [watts]
    INTEGER                      :: AirInNode         =0   ! air inlet node of humidifier
    INTEGER                      :: AirOutNode        =0   ! air outlet node of humidifier
    REAL(r64)                    :: AirInTemp         =0.0d0 ! inlet air temperature [C]
    REAL(r64)                    :: AirInHumRat       =0.0d0 ! inlet air humidity ratio [kg water / kg air]
    REAL(r64)                    :: AirInEnthalpy     =0.0d0 ! inlet air specific enthalpy [J/kg]
    REAL(r64)                    :: AirInMassFlowRate =0.0d0  ! inlet air mass flow rate [kg/s]
    REAL(r64)                    :: AirOutTemp        =0.0d0 ! outlet air temperature [C]
    REAL(r64)                    :: AirOutHumRat      =0.0d0 ! outlet air humidity ratio [kg water / kg air]
    REAL(r64)                    :: AirOutEnthalpy    =0.0d0 ! outlet air specific enthalpy [J/kg]
    REAL(r64)                    :: AirOutMassFlowRate=0.0d0 ! outlet air mass flow rate [kg/s]
    REAL(r64)                    :: HumRatSet         =0.0d0 ! humidity ratio setpoint [kg water / kg air]
    REAL(r64)                    :: WaterAdd          =0.0d0 ! water output (and consumption) [kg/s]
    REAL(r64)                    :: ElecUseEnergy     =0.0d0 ! electricity consumption [J]
    REAL(r64)                    :: ElecUseRate       =0.0d0 ! electricity consumption [W]
    REAL(r64)                    :: WaterCons         =0.0d0 ! water consumption in cubic meters
    REAL(r64)                    :: WaterConsRate     =0.0d0 ! water consumption rate in m3/s
    LOGICAL                      :: SuppliedByWaterSystem = .FALSE. ! true means there is storage tank, otherwise mains
    INTEGER                      :: WaterTankID       = 0  ! index pointer to water storage tank
    INTEGER                      :: WaterTankDemandARRID = 0 ! index pointer to WaterStorage Demand arrays.
    REAL(r64)                    :: TankSupplyVdot    = 0.0d0
    REAL(r64)                    :: TankSupplyVol     = 0.0d0
    REAL(r64)                    :: StarvedSupplyVdot = 0.0d0
    REAL(r64)                    :: StarvedSupplyVol  = 0.0d0
  END TYPE HumidifierData

  ! MODULE VARIABLE DECLARATIONS:
  INTEGER :: NumHumidifiers   =0 ! number of humidifiers of all types
  INTEGER :: NumElecSteamHums =0 ! number of electric steam humidifiers
  TYPE (HumidifierData), ALLOCATABLE, DIMENSION(:) :: Humidifier
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

  ! SUBROUTINE SPECIFICATIONS FOR MODULE

  PUBLIC  SimHumidifier
  PRIVATE ControlHumidifier
  PRIVATE GetHumidifierInput
  PRIVATE InitHumidifier
  PRIVATE SizeHumidifier
  PRIVATE CalcElecSteamHumidifier
  PRIVATE UpdateReportWaterSystem
  PRIVATE UpdateHumidifier
  PRIVATE ReportHumidifier

CONTAINS

SUBROUTINE SimHumidifier(CompName,FirstHVACIteration,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   September 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Manage the simulation of an air humidifier

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! NA

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList,MakeUPPERCase
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT (IN)    :: CompName            ! name of the humidifier unit
  LOGICAL,          INTENT (IN)    :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep
  INTEGER,          INTENT (INOUT) :: CompIndex           ! Pointer to Humidifier Unit

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: HumNum                 ! index of humidifier unit being simulated
  LOGICAL,SAVE :: GetInputFlag = .TRUE.  ! First time, input is "gotten"
  REAL(r64)    :: WaterAddNeeded         ! output in kg/s needed from humidifier to meet humidity setpoint


  IF (GetInputFlag) THEN
    CALL GetHumidifierInput
    GetInputFlag=.FALSE.
  ENDIF

  ! Get the humidifier unit index
  IF (CompIndex == 0) THEN
    HumNum = FindItemInList(CompName,Humidifier%Name,NumHumidifiers)
    IF (HumNum == 0) THEN
      CALL ShowFatalError('SimHumidifier: Unit not found='//TRIM(CompName))
    ENDIF
    CompIndex=HumNum
  ELSE
    HumNum=CompIndex
    IF (HumNum > NumHumidifiers .or. HumNum < 1) THEN
      CALL ShowFatalError('SimHumidifier:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(HumNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumHumidifiers))//  &
                          ', Entered Unit name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(HumNum)) THEN
      IF (CompName /= Humidifier(HumNum)%Name) THEN
        CALL ShowFatalError('SimHumidifier: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(HumNum))// &
                            ', Unit name='//TRIM(CompName)//', stored Unit Name for that index='//  &
                            TRIM(Humidifier(HumNum)%Name))
      ENDIF
      CheckEquipName(HumNum)=.false.
    ENDIF
  ENDIF
  IF (HumNum <= 0) THEN
    CALL ShowFatalError('SimHumidifier: Unit not found='//TRIM(CompName))
  ENDIF

  CALL InitHumidifier(HumNum)

  CALL ControlHumidifier(HumNum,WaterAddNeeded)

  ! call the correct humidifier calculation routine
  SELECT CASE(Humidifier(HumNum)%HumType_Code)

    CASE (Humidifier_Steam_Electric)  ! 'HUMIDIFIER:STEAM:ELECTRIC'

      CALL CalcElecSteamHumidifier(HumNum,WaterAddNeeded)

    CASE DEFAULT
      CALL ShowSevereError('SimHumidifier: Invalid Humidifier Type Code='//trim(TrimSigDigits(Humidifier(HumNum)%HumType_Code)))
      CALL ShowContinueError('...Component Name=['//trim(CompName)//'].')
      CALL ShowFatalError('Preceding Condition causes termination.')

  END SELECT

  CALL UpdateReportWaterSystem(HumNum)

  CALL UpdateHumidifier(HumNum)

  CALL ReportHumidifier(HumNum)

  RETURN

END SUBROUTINE SimHumidifier

SUBROUTINE GetHumidifierInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   September 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for humidifiers and stores it in humidifier data structures.

          ! METHODOLOGY EMPLOYED:
          ! Uses InputProcessor "Get" routines to obtain data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,        ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, GetObjectDefMaxArgs
  USE NodeInputManager,      ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet
  USE WaterManager         , ONLY: SetupTankDemandComponent
  USE DataIPShortCuts

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetHumidifierInputs: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER       :: HumidifierIndex ! loop index
  INTEGER       :: HumNum          ! current humidifier number
  INTEGER       :: NumAlphas       ! Number of Alphas for each GetObjectItem call
  INTEGER       :: NumNumbers      ! Number of Numbers for each GetObjectItem call
  INTEGER       :: IOStatus        ! Used in GetObjectItem
  LOGICAL       :: ErrorsFound=.FALSE.  ! Set to true if errors in input, fatal at end of routine
  LOGICAL       :: IsNotOK         ! Flag to verify name
  LOGICAL       :: IsBlank         ! Flag for blank name
  CHARACTER (len=MaxNameLength)  :: CurrentModuleObject     ! for ease in getting objects
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas         ! Alpha input items for object
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers           ! Numeric input items for object
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.
  INTEGER                              :: TotalArgs=0       ! Total number of alpha and numeric arguments (max) for a
                                                            !  certain object in the input file

  CurrentModuleObject='Humidifier:Steam:Electric'
  NumElecSteamHums = GetNumObjectsFound(CurrentModuleObject)
  NumHumidifiers = NumElecSteamHums
  ! allocate the data array
  ALLOCATE(Humidifier(NumHumidifiers))
  ALLOCATE(CheckEquipName(NumHumidifiers))
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

  ! loop over electric steam humidifiers and load the input data
  DO HumidifierIndex = 1,NumElecSteamHums
    CALL GetObjectItem(CurrentModuleObject,HumidifierIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
    HumNum = HumidifierIndex
    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(Alphas(1),Humidifier%Name,HumNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.TRUE.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    Humidifier(HumNum)%Name     = Alphas(1)
!    Humidifier(HumNum)%HumType  = TRIM(CurrentModuleObject)
    Humidifier(HumNum)%HumType_Code = Humidifier_Steam_Electric
    Humidifier(HumNum)%Sched    = Alphas(2)
    IF (lAlphaBlanks(2)) THEN
      Humidifier(HumNum)%SchedPtr = ScheduleAlwaysOn
    ELSE
      Humidifier(HumNum)%SchedPtr = GetScheduleIndex(Alphas(2))  ! convert schedule name to pointer
      IF (Humidifier(HumNum)%SchedPtr .EQ. 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': invalid '//TRIM(cAlphaFields(2))//  &
           ' entered ='//TRIM(Alphas(2))// &
           ' for '//TRIM(cAlphaFields(1))//'='//TRIM(Alphas(1)))
        ErrorsFound=.TRUE.
      END IF
    END IF
    Humidifier(HumNum)%NomCapVol    = Numbers(1)
    Humidifier(HumNum)%NomPower     = Numbers(2)
    Humidifier(HumNum)%FanPower     = Numbers(3)
    Humidifier(HumNum)%StandbyPower = Numbers(4)
    Humidifier(HumNum)%AirInNode = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
    Humidifier(HumNum)%AirOutNode = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)
    CALL TestCompSet(trim(CurrentModuleObject),Alphas(1),Alphas(3),Alphas(4),'Air Nodes')

    !  A5; \field Name of Water Storage Tank
    IF (lAlphaBlanks(5)) THEN
      Humidifier(HumNum)%SuppliedByWaterSystem = .FALSE.
    ELSE ! water from storage tank
      !
      Call SetupTankDemandComponent(Alphas(1),TRIM(CurrentModuleObject), Alphas(5), ErrorsFound, &
                              Humidifier(HumNum)%WaterTankID, Humidifier(HumNum)%WaterTankDemandARRID)
      Humidifier(HumNum)%SuppliedByWaterSystem = .TRUE.
    ENDIF


  END DO

  DO HumNum=1,NumHumidifiers
    ! Setup Report variables for the Humidifiers
    IF (Humidifier(HumNum)%SuppliedByWaterSystem) THEN
      CALL SetupOutputVariable('Humidifier Water Volume Flow Rate [m3/s]',Humidifier(HumNum)%WaterConsRate, &
                             'System','Average',Humidifier(HumNum)%Name)
      CALL SetupOutputVariable('Humidifier Water Volume [m3]',Humidifier(HumNum)%WaterCons, &
                             'System','Sum',Humidifier(HumNum)%Name)
      CALL SetupOutputVariable('Humidifier Storage Tank Water Volume Flow Rate [m3/s]',Humidifier(HumNum)%TankSupplyVdot , &
                             'System','Average',Humidifier(HumNum)%Name)
      CALL SetupOutputVariable('Humidifier Storage Tank Water Volume [m3]',Humidifier(HumNum)%TankSupplyVol , &
                             'System','Sum',Humidifier(HumNum)%Name, &
                             ResourceTypekey='Water', EndUseKey='HUMIDIFIER', GroupKey = 'SYSTEM')
      CALL SetupOutputVariable('Humidifier Starved Storage Tank Water Volume Flow Rate [m3/s]',  &
                             Humidifier(HumNum)%StarvedSupplyVdot , &
                             'System','Average',Humidifier(HumNum)%Name)
      CALL SetupOutputVariable('Humidifier Starved Storage Tank Water Volume [m3]',Humidifier(HumNum)%StarvedSupplyVol , &
                             'System','Sum',Humidifier(HumNum)%Name, &
                             ResourceTypeKey='Water', EndUseKey='HUMIDIFIER', GroupKey = 'SYSTEM')
      CALL SetupOutputVariable('Humidifier Mains Water Volume [m3]',Humidifier(HumNum)%StarvedSupplyVol , &
                             'System','Sum',Humidifier(HumNum)%Name, &
                             ResourceTypeKey='MainsWater', EndUseKey='HUMIDIFIER', GroupKey = 'SYSTEM')

    ELSE
      CALL SetupOutputVariable('Humidifier Water Volume Flow Rate [m3/s]',Humidifier(HumNum)%WaterConsRate, &
                             'System','Average',Humidifier(HumNum)%Name)
      CALL SetupOutputVariable('Humidifier Water Volume [m3]',Humidifier(HumNum)%WaterCons, &
                             'System','Sum',Humidifier(HumNum)%Name, &
                             ResourceTypeKey='WATER',EndUseKey = 'HUMIDIFIER',GroupKey = 'System')
      CALL SetupOutputVariable('Humidifier Mains Water Volume [m3]',Humidifier(HumNum)%WaterCons, &
                             'System','Sum',Humidifier(HumNum)%Name, &
                             ResourceTypeKey='MAINSWATER',EndUseKey = 'HUMIDIFIER',GroupKey = 'System')

    ENDIF
    CALL SetupOutputVariable('Humidifier Electric Power [W]',Humidifier(HumNum)%ElecUseRate,&
                             'System','Average',Humidifier(HumNum)%Name)
    CALL SetupOutputVariable('Humidifier Electric Energy [J]',Humidifier(HumNum)%ElecUseEnergy,&
                             'System','Sum',Humidifier(HumNum)%Name, &
                             ResourceTypeKey='ELECTRICITY',EndUseKey = 'HUMIDIFIER',GroupKey = 'System')

  END DO

  DEALLOCATE(Alphas)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(Numbers)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(lNumericBlanks)

  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Errors found in input.')
  END IF


  RETURN

END SUBROUTINE GetHumidifierInput

SUBROUTINE InitHumidifier(HumNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   September 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Humidifier Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Psychrometrics, ONlY: RhoH2O
  USE DataHVACGlobals, ONLY: DoSetPointTest
  USE InputProcessor, ONLY: SameString
  USE DataGlobals,    ONLY: AnyEnergyManagementSystemInModel
  USE EMSManager,     ONLY: iHumidityRatioMinSetpoint, CheckIfNodeSetpointManagedByEMS
  USE FluidProperties, ONLY: GetSatEnthalpyRefrig, GetSpecificHeatGlycol, FindGlycol, FindRefrigerant
  USE General,         ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: HumNum ! number of the current humidifier being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: CalledFrom='Humidifier:InitHumidifier'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: InNode ! inlet node number
  INTEGER             :: OutNode ! outlet node number
  INTEGER             :: NumHum
  INTEGER             :: RefrigerantIndex ! refiferant index
  INTEGER             :: WaterIndex       ! fluid type index
  REAL(r64)           :: NominalPower     ! Nominal power input to humidifier, W
  REAL(r64)           :: WaterSpecHeat    ! specific heat of water , J/kgK
  REAL(r64)           :: SteamSatEnthalpy ! enthalpy of saturated steam at 100C, J/kg
  REAL(r64)           :: WaterSatEnthalpy ! enthalpy of saturated water at 100C, J/kg

  LOGICAL,SAVE        :: MyOneTimeFlag = .TRUE.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL,SAVE        :: MySetPointCheckFlag = .TRUE.
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MySizeFlag

  ! do one time initializations
  IF (MyOneTimeFlag) THEN
    ! initialize the environment and sizing flags
    ALLOCATE(MyEnvrnFlag(NumHumidifiers))
    ALLOCATE(MySizeFlag(NumHumidifiers))
    MyEnvrnFlag = .TRUE.

    MyOneTimeFlag = .FALSE.
    MySizeFlag    = .TRUE.

  END IF

  ! do sizing calculation
  IF ( MySizeFlag(HumNum) ) THEN
    Call SizeHumidifier(HumNum)
    MySizeFlag(HumNum) =.FALSE.
  ENDIF

  IF ( .NOT. SysSizingCalc .AND. MySetPointCheckFlag .AND. DoSetPointTest) THEN
    DO NumHum = 1, NumHumidifiers
      OutNode = Humidifier(NumHum)%AirOutNode

      IF (OutNode > 0) THEN
        IF (Node(OutNode)%HumRatMin == SensedNodeFlagValue) THEN
          IF (.NOT. AnyEnergyManagementSystemInModel) THEN
            CALL ShowSevereError('Humidifiers: Missing humidity setpoint for '// &
                            trim(HumidifierType(Humidifier(NumHum)%HumType_Code))//' = '//TRIM(Humidifier(HumNum)%Name))
            CALL ShowContinueError('  use a Setpoint Manager with Control Variable = "MinimumHumidityRatio" to establish'//  &
                                   'a setpoint at the humidifier outlet node.')
            CALL ShowContinueError('  expecting it on Node="'//trim(NodeID(OutNode))//'".')
            SetPointErrorFlag = .TRUE.
          ELSE
            CALL CheckIfNodeSetpointManagedByEMS(OutNode,iHumidityRatioMinSetpoint, SetpointErrorFlag)
            IF (SetpointErrorFlag) THEN
              CALL ShowSevereError('Humidifiers: Missing humidity setpoint for '// &
                            trim(HumidifierType(Humidifier(NumHum)%HumType_Code))//' = '//TRIM(Humidifier(HumNum)%Name))
              CALL ShowContinueError('  use a Setpoint Manager with Control Variable = "MinimumHumidityRatio" to establish'//  &
                                   'a setpoint at the humidifier outlet node.')
              CALL ShowContinueError('  expecting it on Node="'//trim(NodeID(OutNode))//'".')
              CALL ShowContinueError('  or use an EMS actuator to control minimum humidity ratio to establish'//  &
                                   'a setpoint at the humidifier outlet node.')
            ENDIF
          ENDIF
        END IF
      END IF
    END DO
    MySetPointCheckFlag = .FALSE.
  END IF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(HumNum)=.TRUE.
  ENDIF

  ! do these initializations every HVAC time step
  InNode = Humidifier(HumNum)%AirInNode
  OutNode = Humidifier(HumNum)%AirOutNode
  Humidifier(HumNum)%HumRatSet = Node(OutNode)%HumRatMin
  Humidifier(HumNum)%AirInTemp = Node(InNode)%Temp
  Humidifier(HumNum)%AirInHumRat = Node(InNode)%HumRat
  Humidifier(HumNum)%AirInEnthalpy = Node(InNode)%Enthalpy
  Humidifier(HumNum)%AirInMassFlowRate = Node(InNode)%MassFlowRate
  Humidifier(HumNum)%WaterAdd = 0.0d0
  Humidifier(HumNum)%ElecUseEnergy = 0.0d0
  Humidifier(HumNum)%ElecUseRate = 0.0d0
  Humidifier(HumNum)%WaterCons = 0.0d0
  Humidifier(HumNum)%WaterConsRate = 0.0d0

  RETURN

END SUBROUTINE InitHumidifier

SUBROUTINE SizeHumidifier(HumNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket Nigusse, UCF/FSEC,
          !       DATE WRITTEN   March, 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for for sizing electric steam humidifier nominal electric power.

          ! METHODOLOGY EMPLOYED:
          ! Uses user sepecified nominal capacity in m3/s and water enthalpy change required to
          ! vaporize water from a reference temperature of 20.0C. to steam at 100.0C.
          !  m_dot = Nominal Capacity [m3/s] * Density of water at 5.05 [kg/m3]
          !  Nominal Capacity =  m_dot [kg/s] * delta_enthalpy [J/kg]

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Psychrometrics,      ONlY: RhoH2O
  USE FluidProperties,     ONLY: GetSatEnthalpyRefrig, GetSpecificHeatGlycol, FindGlycol, FindRefrigerant
  USE General,             ONLY: RoundSigDigits
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE DataSizing,          ONLY: Autosize

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: HumNum ! number of the current humidifier being sized

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: CalledFrom='Humidifier:SizeHumidifier'
  REAL(r64), PARAMETER        :: Tref   =  20.0d0 ! Reference temp of water for rated capacity calac [C]
  REAL(r64), PARAMETER        :: TSteam = 100.0d0 ! saturated steam temperatur generated by Humidifier [C]

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: NumHum
  INTEGER             :: RefrigerantIndex ! refiferant index
  INTEGER             :: WaterIndex       ! fluid type index
  REAL(r64)           :: NominalPower     ! Nominal power input to humidifier, W
  REAL(r64)           :: WaterSpecHeatAvg ! specific heat of water, J/kgK
  REAL(r64)           :: SteamSatEnthalpy ! enthalpy of saturated steam at 100C, J/kg
  REAL(r64)           :: WaterSatEnthalpy ! enthalpy of saturated water at 100C, J/kg


  IF (Humidifier(HumNum)%HumType_Code == Humidifier_Steam_Electric) THEN
      Humidifier(HumNum)%NomCap = RhoH2O(InitConvTemp)*Humidifier(HumNum)%NomCapVol

      RefrigerantIndex = FindRefrigerant('STEAM')
      WaterIndex = FindGlycol('WATER')
      SteamSatEnthalpy = GetSatEnthalpyRefrig('STEAM',TSteam,1.0d0,RefrigerantIndex,CalledFrom)
      WaterSatEnthalpy = GetSatEnthalpyRefrig('STEAM',TSteam,0.0d0,RefrigerantIndex,CalledFrom)
      WaterSpecHeatAvg = 0.5d0*(GetSpecificHeatGlycol('WATER',TSteam,WaterIndex,CalledFrom) + &
                                GetSpecificHeatGlycol('WATER',Tref,WaterIndex,CalledFrom))

      NominalPower = Humidifier(HumNum)%NomCap &
                   * ((SteamSatEnthalpy-WaterSatEnthalpy) + WaterSpecHeatAvg*(TSteam-Tref))

      IF (Humidifier(HumNum)%NomPower == AutoSize) THEN
        Humidifier(HumNum)%NomPower = NominalPower
        CALL ReportSizingOutput('Humidifier:Steam:Electric',Humidifier(HumNum)%Name, &
                                'Rated Power [W]', Humidifier(HumNum)%NomPower)
      ELSEIF (Humidifier(HumNum)%NomPower >= 0.0d0 .and. Humidifier(HumNum)%NomCap > 0.0d0) THEN
        IF (Humidifier(HumNum)%NomPower <  NominalPower) THEN
          CALL ShowWarningError('Humidifier:Steam:Electric: specified Rated Power is less than nominal Rated '// &
                                ' Power for electric steam humidifier = '//TRIM(Humidifier(HumNum)%Name)//'. ')
          CALL ShowContinueError(' specified Rated Power = '//TRIM(RoundSigDigits(Humidifier(HumNum)%NomPower,2)))
          CALL ShowContinueError(' while expecting a minimum Rated Power = '//TRIM(RoundSigDigits(NominalPower,2)))
        ENDIF
      ELSE
        CALL ShowWarningError('Humidifier:Steam:Electric: specified nominal capacity is zero '// &
                              ' for electric steam humidifier = '//TRIM(Humidifier(HumNum)%Name)//'. ')
        CALL ShowContinueError(' For zero rated capacity humidifier the rated power is zero.')
      ENDIF
  END IF

  RETURN

END SUBROUTINE SizeHumidifier

SUBROUTINE ControlHumidifier(HumNum,WaterAddNeeded)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   September 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets the output required from the humidifier

          ! METHODOLOGY EMPLOYED:
          ! Uses a minimum humidity setpoint and water mass balance to calculate moisture addition needed

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use Psychrometrics, ONLY:PsyWFnTdbRhPb

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: HumNum  ! number of the current humidifier being simulated
  REAL(r64), INTENT(OUT)    :: WaterAddNeeded ! moisture addition rate needed to meet minimum humidity ratio setpoint [kg/s]

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: UnitOn       ! unit on flag
  REAL(r64) :: AirMassFlowRate ! air mass flow rate [kg/s]
  REAL(r64) :: HumRatSatIn     ! humidity ratio at saturation at the inlet temperature  [kg H2O / kg dry air]


  AirMassFlowRate = 0.0d0
  UnitOn = .TRUE.
  IF (Humidifier(HumNum)%HumRatSet .LE. 0.0d0) UnitOn = .FALSE.
  AirMassFlowRate = Humidifier(HumNum)%AirInMassFlowRate
  IF (AirMassFlowRate .LE. SmallMassFlow) UnitOn = .FALSE.
  IF (GetCurrentScheduleValue(Humidifier(HumNum)%SchedPtr) .LE. 0.0d0)  UnitOn = .FALSE.
  IF (Humidifier(HumNum)%AirInHumRat .GE. Humidifier(HumNum)%HumRatSet)  UnitOn = .FALSE.
  HumRatSatIn = PsyWFnTdbRhPb(Humidifier(HumNum)%AirInTemp,1.0d0,OutBaroPress, 'ControlHumidifier')
  IF (Humidifier(HumNum)%AirInHumRat .GE. HumRatSatIn) UnitOn = .FALSE.
  IF (UnitOn) THEN
    ! AirMassFlowRate*AirInHumRat + WaterAddNeeded = AirMassFlowRate*HumRatSet
    WaterAddNeeded = AirMassFlowRate * (Humidifier(HumNum)%HumRatSet - Humidifier(HumNum)%AirInHumRat)
  ELSE
    WaterAddNeeded = 0.0d0
  END IF

  RETURN

END SUBROUTINE ControlHumidifier

SUBROUTINE CalcElecSteamHumidifier(HumNum,WaterAddNeeded)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   September 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate the electricity consumption and the outlet conditions for an electric steam
          ! humidifier, given the inlet conditions and the steam addition rate.

          ! METHODOLOGY EMPLOYED:
          ! Uses energy and mass balance as well as pschrometric relations.

          ! REFERENCES:
          ! ASHRAE HVAC 2 Toolkit, page 4-112
          ! 1997 ASHRAE Handbook Fundamentals, page 6.18

          ! USE STATEMENTS:
  Use Psychrometrics, ONLY:PsyWFnTdbRhPb,PsyTdbFnHW,PsyHFnTdbW,RhoH2O

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: HumNum  ! number of the current humidifier being simulated
  REAL(r64), INTENT(IN)    :: WaterAddNeeded ! moisture addition rate set by controller [kg/s]

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64) :: AirMassFlowRate     ! air mass flow rate [kg/s]
  REAL(r64) :: HumRatSatOut        ! humidity ratio at saturation at the outlet temperature [kg H2O / kg dry air]
  REAL(r64) :: HumRatSatIn         ! humidity ratio at saturation at the inlet temperature  [kg H2O / kg dry air]
  REAL(r64) :: WaterAddRate        ! moisture addition rate by humidifier [kg/s]
  REAL(r64) :: WaterAddNeededMax   ! moisture addition rate set by controller, limited by humidifier capacity
  REAL(r64) :: AirOutEnthalpy      ! outlet air enthalpy [J/kg]
  REAL(r64) :: AirOutHumRat        ! outlet air humidity ratio [kg H2O / kg dry air]
  REAL(r64) :: AirOutTemp          ! outlet air temperature [C]
  REAL(r64) :: WaterInEnthalpy     ! enthalpy of the inlet steam [J/kg]
  REAL(r64) :: HumRatSatApp        ! the approximate humidity ratio where the line drawn between inlet and desired outlet conditions
                              ! crosses the saturation line.
  REAL(r64) :: WaterDens           ! density of liquid water [kg/m3]


  AirMassFlowRate = Humidifier(HumNum)%AirInMassFlowRate
  HumRatSatIn = PsyWFnTdbRhPb(Humidifier(HumNum)%AirInTemp,1.0d0,OutBaroPress, 'CalcElecSteamHumidifier')
  HumRatSatOut = 0.0d0
  HumRatSatApp = 0.0d0
  WaterInEnthalpy = 2676125.d0  ! At 100 C
  WaterDens = RhoH2O(InitConvTemp)
  WaterAddNeededMax = MIN(WaterAddNeeded,Humidifier(HumNum)%NomCap)
  IF (WaterAddNeededMax.GT.0.0d0) THEN
    !   ma*W1 + mw = ma*W2
    !   ma*h1 + mw*hw = ma*h2
    ! where ma is air mass flow rate; h1,W1 are the inlet enthalpy and humidity ratio; h2 and W2 are
    ! the outlet enthalpy and humidity ratio; mw is the steam mass flow rate; hw is the steam enthalpy.
    ! Setting mw equal to the desired water addition rate, use the above 2 equations to calculate the
    ! outlet conditions
    AirOutEnthalpy=(AirMassFlowRate*Humidifier(HumNum)%AirInEnthalpy + WaterAddNeededMax*WaterInEnthalpy)/AirMassFlowRate
    AirOutHumRat = (AirMassFlowRate*Humidifier(HumNum)%AirInHumRat + WaterAddNeededMax) / AirMassFlowRate
    AirOutTemp = PsyTdbFnHW(AirOutEnthalpy,AirOutHumrat, 'CalcElecSteamHumidifier')
    HumRatSatOut = PsyWFnTdbRhPb(AirOutTemp,1.0d0,OutBaroPress, 'CalcElecSteamHumidifier')
    IF (AirOutHumRat .LE. HumRatSatOut) THEN
      ! If the outlet condition is below the saturation curve, the desired moisture addition rate can be met.
      WaterAddRate = WaterAddNeededMax
    ELSE
      ! The desired moisture addition rate results in an outlet state above the saturation curve. We need to
      ! find the point where the line drawn between state 1 (inlet) and state 2 (our desired outlet) crosses
      ! the saturation curve. This will be the new outlet condition. Rather than iterate to obtain this point,
      ! we find it approximately by solving for the point where 2 lines cross: the first drawn from
      ! state 1 to state 2, the second from T1, W1s to T2, W2s; where T1 is the inlet temperature, W1s is
      ! the humidity ratio at saturation at temperature T1; and T2 is the desired outlet temperature, W2s
      ! is the humidity ratio at saturation at temperature T2. The 2 lines are given by the equations:
      !   W = W1 + ((W2-W1)/(T2-T1))*(T-T1)
      !   W = W1s + ((W2s-W1s)/(T2-T1))*(T-T1)
      ! Solving for the point where the line cross (T3,W3):
      !   W3 = W1 + ((W2-W1)*(W1s-W1))/(W2-W2s + W1s-W1)
      !   T3 = T1 + (W3-W1)*((T2-T1)/(W2-W1))  ! "T1 +" added by Shirey 8/12/04  That's correct! [WFB 9/29/2004]
      HumRatSatApp = Humidifier(HumNum)%AirInHumRat + (AirOutHumRat - Humidifier(HumNum)%AirInHumRat) &
                    * (HumRatSatIn - Humidifier(HumNum)%AirInHumRat) &
                    / (AirOutHumRat - HumRatSatOut + HumRatSatIn - Humidifier(HumNum)%AirInHumRat)
      AirOutTemp = Humidifier(HumNum)%AirInTemp + &
                   (HumRatSatApp - Humidifier(HumNum)%AirInHumRat)*((AirOutTemp - Humidifier(HumNum)%AirInTemp) &
                     / (AirOutHumRat - Humidifier(HumNum)%AirInHumRat))
      ! This point isn't quite on the saturation curve since we made a linear approximation of the curve,
      ! but the temperature should be very close to the correct outlet temperature. We will use this temperature
      ! as the outlet temperature and move to the saturation curve for the outlet humidity and enthalpy
      AirOutHumRat = PsyWFnTdbRhPb(AirOutTemp,1.0d0,OutBaroPress, 'CalcElecSteamHumidifier')
      AirOutEnthalpy = PsyHFnTdbW(AirOutTemp,AirOutHumRat, 'CalcElecSteamHumidifier')
      WaterAddRate = AirMassFlowRate * (AirOutHumRat - Humidifier(HumNum)%AirInHumRat)
    END IF

  ELSE
    WaterAddRate = 0.0d0
    AirOutEnthalpy = Humidifier(HumNum)%AirInEnthalpy
    AirOutTemp = Humidifier(HumNum)%AirInTemp
    AirOutHumRat = Humidifier(HumNum)%AirInHumRat
  END IF

  Humidifier(HumNum)%WaterAdd = WaterAddRate
  Humidifier(HumNum)%AirOutTemp = AirOutTemp
  Humidifier(HumNum)%AirOutHumRat = AirOutHumRat
  Humidifier(HumNum)%AirOutEnthalpy = AirOutEnthalpy
  Humidifier(HumNum)%AirOutMassFlowRate = AirMassFlowRate

  IF (WaterAddRate.GT.0.0d0) THEN
    Humidifier(HumNum)%ElecUseRate = (WaterAddRate/Humidifier(HumNum)%NomCap)*Humidifier(HumNum)%NomPower &
                                       + Humidifier(HumNum)%FanPower + Humidifier(HumNum)%StandbyPower
  ELSE IF (GetCurrentScheduleValue(Humidifier(HumNum)%SchedPtr) .GT. 0.0d0)  THEN
    Humidifier(HumNum)%ElecUseRate = Humidifier(HumNum)%StandbyPower
  ELSE
    Humidifier(HumNum)%ElecUseRate = 0.0d0
  END IF
  Humidifier(HumNum)%WaterConsRate =  Humidifier(HumNum)%WaterAdd / WaterDens

RETURN

END SUBROUTINE CalcElecSteamHumidifier

SUBROUTINE UpdateReportWaterSystem(HumNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Aug. 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! collect water system calculations , update and report them


          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataWater , ONLY: WaterStorage
  USE DataGlobals,     ONLY: SecInHour, BeginTimeStepFlag
  USE DataHVACGlobals, ONLY: TimeStepSys

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: HumNum ! number of the current humidifier being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: AvailTankVdot
  REAL(r64)  :: TankSupplyVdot
  REAL(r64)  :: StarvedVdot


  ! set demand request in WaterStorage if needed.
  IF (Humidifier(HumNum)%SuppliedByWaterSystem) Then
    WaterStorage(Humidifier(HumNum)%WaterTankID)%VdotRequestDemand(Humidifier(HumNum)%WaterTankDemandARRID) &
      = Humidifier(HumNum)%WaterConsRate

    AvailTankVdot = &       ! check what tank can currently provide
      WaterStorage(Humidifier(HumNum)%WaterTankID)%VdotAvailDemand(Humidifier(HumNum)%WaterTankDemandARRID)

     StarvedVdot = 0.0d0
     TankSupplyVdot = Humidifier(HumNum)%WaterConsRate ! init
     If ((AvailTankVdot < Humidifier(HumNum)%WaterConsRate) .and. ( .NOT. (BeginTimeStepFlag)) ) Then ! calculate starved flow
        StarvedVdot = Humidifier(HumNum)%WaterConsRate - AvailTankVdot
        TankSupplyVdot = AvailTankVdot
     ENDIF

     Humidifier(HumNum)%TankSupplyVdot = TankSupplyVdot
     Humidifier(HumNum)%TankSupplyVol  = TankSupplyVdot * (TimeStepSys * SecInHour)
     Humidifier(HumNum)%StarvedSupplyVdot = StarvedVdot
     Humidifier(HumNum)%StarvedSupplyVol  = StarvedVdot * (TimeStepSys * SecInHour)

  ENDIF

  RETURN

END SUBROUTINE UpdateReportWaterSystem


SUBROUTINE UpdateHumidifier(HumNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   September 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Moves humidifier output to the outlet nodes.

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataContaminantBalance, ONLY: Contaminant

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: HumNum ! number of the current humidifier being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: InNode ! inlet node number
  INTEGER             :: OutNode ! outlet node number

  InNode =Humidifier(HumNum)%AirInNode
  OutNode = Humidifier(HumNum)%AirOutNode
  ! Set the outlet air node of the humidifier
  Node(OutNode)%MassFlowRate = Humidifier(HumNum)%AirOutMassFlowRate
  Node(OutNode)%Temp         = Humidifier(HumNum)%AirOutTemp
  Node(OutNode)%HumRat       = Humidifier(HumNum)%AirOutHumRat
  Node(OutNode)%Enthalpy     = Humidifier(HumNum)%AirOutEnthalpy

  ! Set the outlet nodes for properties that just pass through & not used
  Node(OutNode)%Quality             = Node(InNode)%Quality
  Node(OutNode)%Press               = Node(InNode)%Press
  Node(OutNode)%MassFlowRateMin     = Node(InNode)%MassFlowRateMin
  Node(OutNode)%MassFlowRateMax     = Node(InNode)%MassFlowRateMax
  Node(OutNode)%MassFlowRateMinAvail= Node(InNode)%MassFlowRateMinAvail
  Node(OutNode)%MassFlowRateMaxAvail= Node(InNode)%MassFlowRateMaxAvail

  IF (Contaminant%CO2Simulation) Then
    Node(OutNode)%CO2 = Node(InNode)%CO2
  End If
  IF (Contaminant%GenericContamSimulation) Then
    Node(OutNode)%GenContam = Node(InNode)%GenContam
  End If

  RETURN

END SUBROUTINE UpdateHumidifier

SUBROUTINE ReportHumidifier(HumNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   September 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Fill remaining report variables

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use DataHVACGlobals, ONLY: TimeStepSys

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: HumNum ! number of the current humidifier being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na


  Humidifier(HumNum)%ElecUseEnergy = Humidifier(HumNum)%ElecUseRate*TimeStepSys*SecInHour
  Humidifier(HumNum)%WaterCons = Humidifier(HumNum)%WaterConsRate*TimeStepSys*SecInHour

  RETURN

END SUBROUTINE ReportHumidifier

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

END MODULE Humidifiers

