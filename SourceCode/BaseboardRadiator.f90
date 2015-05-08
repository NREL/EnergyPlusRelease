! Note: This file contains two modules:
! Module BaseboardRadiator -- (ref: Object: ZoneHVAC:Baseboard:Convective:Water)
! Module BaseboardElectric -- (ref: Object: ZoneHVAC:Baseboard:Convective:Electric)
MODULE BaseboardRadiator
  ! Module containing the routines dealing with the BASEBOARD HEATER
  ! component(s).

  ! MODULE INFORMATION:
  !       AUTHOR         Russ Taylor
  !       DATE WRITTEN   Jan 1998
  !       MODIFIED       Fred Buhl, October 1999
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! Needs description

  ! METHODOLOGY EMPLOYED:
  ! Needs description, as appropriate

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals
USE DataHVACGlobals, ONLY: SmallLoad
USE DataEnvironment, ONLY: StdRhoAir
USE DataInterfaces
USE DataPlant      , ONLY: PlantLoop, TypeOf_Baseboard_Conv_Water

  ! Use statements for access to subroutines in other modules
USE ScheduleManager
USE Psychrometrics,     ONLY: PsyCpAirFnWTdb, PsyRhoAirFnPbTdbW
USE FluidProperties,    ONLY: GetDensityGlycol, GetSpecificHeatGlycol

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS
  REAL(r64), PARAMETER :: SimpConvAirFlowSpeed = 0.5d0 !m/s
  CHARACTER(len=*), PARAMETER :: cCMO_BBRadiator_Water = 'ZoneHVAC:Baseboard:Convective:Water'

  ! DERIVED TYPE DEFINITIONS
  TYPE BaseboardParams
    CHARACTER(len=MaxNameLength) :: EquipID   =' '
    CHARACTER(len=MaxNameLength) :: Schedule  =' '
    Integer      :: SchedPtr                  = 0
    INTEGER      :: EquipType                 = 0
    INTEGER      :: ZonePtr                   = 0
    INTEGER      :: WaterInletNode            = 0
    INTEGER      :: WaterOutletNode           = 0
    INTEGER      :: ControlCompTypeNum        = 0
    INTEGER      :: CompErrIndex              = 0
    REAL(r64)    :: UA                        =0.0d0
    REAL(r64)    :: WaterMassFlowRate         =0.0d0
    REAL(r64)    :: WaterVolFlowRateMax       =0.0d0  ! m3/s
    REAL(r64)    :: WaterMassFlowRateMax      =0.0d0  ! kg/s
    REAL(r64)    :: Offset                    =0.0d0
    REAL(r64)    :: AirMassFlowRate           =0.0d0  ! kg/s
    REAL(r64)    :: DesAirMassFlowRate        =0.0d0  ! kg/s
    REAL(r64)    :: WaterInletTemp            =0.0d0
    REAL(r64)    :: WaterOutletTemp           =0.0d0
    REAL(r64)    :: WaterInletEnthalpy        =0.0d0
    REAL(r64)    :: WaterOutletEnthalpy       =0.0d0
    REAL(r64)    :: AirInletTemp              =0.0d0
    REAL(r64)    :: AirInletHumRat            =0.0d0
    REAL(r64)    :: AirOutletTemp             =0.0d0
    REAL(r64)    :: Power                     =0.0d0
    REAL(r64)    :: Energy                    =0.0d0
    INTEGER      :: LoopNum                   =0  ! plant loop index
    INTEGER      :: LoopSideNum               =0  ! plant loop side index
    INTEGER      :: BranchNum                 =0  ! plant loop branch index
    INTEGER      :: CompNum                   =0  ! plant loop component index
    INTEGER      :: BBLoadReSimIndex          =0
    INTEGER      :: BBMassFlowReSimIndex      =0
    INTEGER      :: BBInletTempFlowReSimIndex =0
  END TYPE BaseboardParams

  !MODULE VARIABLE DECLARATIONS:
  TYPE (BaseboardParams), ALLOCATABLE, DIMENSION(:) :: Baseboard
  INTEGER :: NumBaseboards=0
  LOGICAL, ALLOCATABLE, DIMENSION(:)       :: MySizeFlag
  LOGICAL, ALLOCATABLE, DIMENSION(:)       :: CheckEquipName
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: SetLoopIndexFlag   ! get loop number flag

  !SUBROUTINE SPECIFICATIONS FOR MODULE BaseboardRadiator

  PUBLIC  :: SimBaseboard
  PUBLIC  :: SimHWConvective
  PRIVATE :: GetBaseboardInput
  PRIVATE :: InitBaseboard
  PRIVATE :: SizeBaseboard
  PRIVATE :: UpdateBaseboard
  PRIVATE :: ReportBaseboard
  PRIVATE :: HWBaseboardUAResidual
  PUBLIC  :: UpdateBaseboardPlantConnection

CONTAINS

  SUBROUTINE SimBaseboard(EquipName, ActualZoneNum, ControlledZoneNum, FirstHVACIteration, &
                             PowerMet, CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   Nov 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the Baseboard Radiators.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataLoopNode, ONLY: Node
    USE InputProcessor, ONLY: FindItemInList
    USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand
    USE General, ONLY: TrimSigDigits
    USE DataInterfaces, ONLY: ControlCompOutput
    USE PlantUtilities,       ONLY: SetActuatedBranchFlowRate

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN) :: EquipName
    INTEGER, INTENT(IN) :: ActualZoneNum
    INTEGER, INTENT(IN) :: ControlledZoneNum
    LOGICAL, INTENT(IN) :: FirstHVACIteration
    REAL(r64), INTENT(OUT)   :: PowerMet
    INTEGER, INTENT(INOUT) :: CompIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
    INTEGER, PARAMETER :: MaxIter = 30

          ! INTERFACE BLOCK SPECIFICATIONS
          ! see use DataInterfaces

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    INTEGER        :: BaseboardNum ! index of unit in baseboard array
    LOGICAL,SAVE   :: GetInputFlag = .TRUE. ! one time get input flag
    REAL(r64)      :: QZnReq       ! zone load not yet satisfied
    REAL(r64)      :: MaxWaterFlow
    REAL(r64)      :: MinWaterFlow
    REAL(r64)      :: DummyMdot


    IF (GetInputFlag) THEN
      CALL GetBaseboardInput
      GetInputFlag=.false.
    END IF

    ! Find the correct Baseboard Equipment
    IF (CompIndex == 0) THEN
      BaseboardNum = FindItemInList(EquipName, Baseboard%EquipID, NumBaseboards)
      IF (BaseboardNum == 0) THEN
        CALL ShowFatalError('SimBaseboard: Unit not found='//TRIM(EquipName))
      ENDIF
      CompIndex=BaseboardNum
    ELSE
      BaseboardNum=CompIndex
      IF (BaseboardNum > NumBaseboards .or. BaseboardNum < 1) THEN
        CALL ShowFatalError('SimBaseboard:  Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(BaseboardNum))// &
                            ', Number of Units='//TRIM(TrimSigDigits(NumBaseboards))//  &
                            ', Entered Unit name='//TRIM(EquipName))
      ENDIF
      IF (CheckEquipName(BaseboardNum)) THEN
        IF (EquipName /= Baseboard(BaseboardNum)%EquipID) THEN
          CALL ShowFatalError('SimBaseboard: Invalid CompIndex passed='//  &
                              TRIM(TrimSigDigits(BaseboardNum))// &
                              ', Unit name='//TRIM(EquipName)//', stored Unit Name for that index='//  &
                              TRIM(Baseboard(BaseboardNum)%EquipID))
        ENDIF
        CheckEquipName(BaseboardNum)=.false.
      ENDIF
    ENDIF

    CALL InitBaseboard(BaseboardNum, ControlledZoneNum)

    QZnReq = ZoneSysEnergyDemand(ActualZoneNum)%RemainingOutputReqToHeatSP

  IF ((QZnReq < SmallLoad) .OR. (Baseboard(BaseboardNum)%WaterInletTemp <= Baseboard(BaseboardNum)%AirInletTemp)) THEN
!  IF (Baseboard(BaseboardNum)%WaterInletTemp <= Baseboard(BaseboardNum)%AirInletTemp) THEN
! The baseboard cannot provide cooling.  Thus, if the zone required load is negative or the water inlet
! temperature is lower than the zone air temperature, then we need to shut down the baseboard unit

    Baseboard(BaseboardNum)%WaterOutletTemp   = Baseboard(BaseboardNum)%WaterInletTemp
    Baseboard(BaseboardNum)%AirOutletTemp     = Baseboard(BaseboardNum)%AirInletTemp
    Baseboard(BaseboardNum)%Power             = 0.0d0
    Baseboard(BaseboardNum)%WaterMassFlowRate = 0.0d0
    ! init hot water flow rate to zero
    DummyMdot = 0.d0
    CALL SetActuatedBranchFlowRate(DummyMdot,Baseboard(BaseboardNum)%WaterInletNode,  &
                                   Baseboard(BaseboardNum)%LoopNum,Baseboard(BaseboardNum)%LoopSideNum, &
                                   Baseboard(BaseboardNum)%BranchNum, .FALSE. )

  ELSE
    ! init hot water flow rate to zero
    DummyMdot = 0.d0
    CALL SetActuatedBranchFlowRate(DummyMdot,Baseboard(BaseboardNum)%WaterInletNode,  &
                                   Baseboard(BaseboardNum)%LoopNum,Baseboard(BaseboardNum)%LoopSideNum, &
                                   Baseboard(BaseboardNum)%BranchNum, .TRUE. )

    !On the first HVAC iteration the system values are given to the controller, but after that
    ! the demand limits are in place and there needs to be feedback to the Zone Equipment
    If(FirstHVACIteration)Then
       MaxWaterFlow = Baseboard(BaseboardNum)%WaterMassFlowRateMax
       MinWaterFlow = 0.0d0
    Else
       MaxWaterFlow = Node(Baseboard(BaseboardNum)%WaterInletNode)%MassFlowRateMaxAvail
       MinWaterFlow = Node(Baseboard(BaseboardNum)%WaterInletNode)%MassFlowRateMinAvail
    EndIf

    CALL ControlCompOutput(CompName=Baseboard(BaseboardNum)%EquipID,CompType=cCMO_BBRadiator_Water,  &
                           CompNum=BaseboardNum, &
                           FirstHVACIteration=FirstHVACIteration,QZnReq=QZnReq, &
                           ActuatedNode=Baseboard(BaseboardNum)%WaterInletNode, &
                           MaxFlow=MaxWaterFlow,MinFlow=MinWaterFlow, &
                           ControlOffset=Baseboard(BaseboardNum)%Offset, &
                           ControlCompTypeNum=Baseboard(BaseboardNum)%ControlCompTypeNum, &
                           CompErrIndex=Baseboard(BaseboardNum)%CompErrIndex, &
                           LoopNum = Baseboard(BaseboardNum)%LoopNum, &
                           LoopSide = Baseboard(BaseboardNum)%LoopSideNum, &
                           BranchIndex = Baseboard(BaseboardNum)%BranchNum)

    PowerMet = Baseboard(BaseboardNum)%Power

  END IF

  CALL UpdateBaseboard(BaseboardNum)
  CALL ReportBaseboard(BaseboardNum)

  END SUBROUTINE SimBaseboard

  SUBROUTINE GetBaseboardInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   Nov 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the input for the Baseboard units.

          ! METHODOLOGY EMPLOYED:
          ! Standard input processor calls.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, MakeUPPERCase, SameString
    USE NodeInputManager, ONLY: GetOnlySingleNode
    USE BranchNodeConnections, ONLY: TestCompSet
    USE DataLoopNode
    USE GlobalNames,    ONLY : VerifyUniqueBaseboardName
    USE DataIPShortCuts


    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER     :: RoutineName='GetBaseboardInput: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: BaseboardNum
    INTEGER :: NumConvHWBaseboards
    INTEGER :: ConvHWBaseboardNum
    INTEGER :: NumAlphas
    INTEGER :: NumNums
    INTEGER :: IOSTAT
    LOGICAL :: ErrorsFound = .false.  ! If errors detected in input
    LOGICAL :: IsNotOK                ! Flag to verify name
    LOGICAL :: IsBlank                ! Flag for blank name
    LOGICAL :: errflag

    cCurrentModuleObject = cCMO_BBRadiator_Water

    NumConvHWBaseboards = GetNumObjectsFound(cCurrentModuleObject)

    ! Calculate total number of baseboard units
    NumBaseboards = NumConvHWBaseboards

    ALLOCATE(Baseboard(NumBaseboards))
    ALLOCATE(CheckEquipName(NumBaseboards))
    CheckEquipName=.true.

    IF (NumConvHWBaseboards .GT. 0)THEN  !Get the data for cooling schemes
      BaseBoardNum=0
      DO ConvHWBaseboardNum = 1,  NumConvHWBaseboards

        CALL GetObjectItem(cCurrentModuleObject,ConvHWBaseboardNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                           NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                           AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(cAlphaArgs(1),Baseboard%EquipID,BaseboardNum,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          CYCLE
        ENDIF
        CALL VerifyUniqueBaseboardName(TRIM(cCurrentModuleObject),cAlphaArgs(1),errflag,TRIM(cCurrentModuleObject)//' Name')
        IF (errflag) THEN
          ErrorsFound=.true.
        ENDIF
        BaseboardNum = BaseboardNum + 1
        Baseboard(BaseboardNum)%EquipID   = cAlphaArgs(1) ! name of this baseboard
        Baseboard(BaseboardNum)%EquipType = TypeOf_Baseboard_Conv_Water
        Baseboard(BaseboardNum)%Schedule  = cAlphaArgs(2)
        IF (lAlphaFieldBlanks(2)) THEN
          Baseboard(BaseboardNum)%SchedPtr  = ScheduleAlwaysOn
        ELSE
          Baseboard(BaseboardNum)%SchedPtr  = GetScheduleIndex(cAlphaArgs(2))
          IF (Baseboard(BaseboardNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(2))//  &
                                  ' entered ='//TRIM(cAlphaArgs(2))// &
                                  ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
            ErrorsFound=.true.
          END IF
        ENDIF
        ! get inlet node number
        Baseboard(BaseboardNum)%WaterInletNode  = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet,1,ObjectIsNotParent)
        ! get outlet node number
        Baseboard(BaseboardNum)%WaterOutletNode  = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet,1,ObjectIsNotParent)

        CALL TestCompSet(cCMO_BBRadiator_Water,cAlphaArgs(1),cAlphaArgs(3), &
                         cAlphaArgs(4),'Hot Water Nodes')


        Baseboard(BaseboardNum)%UA                  = rNumericArgs(1)
        Baseboard(BaseboardNum)%WaterVolFlowRateMax = rNumericArgs(2)
        Baseboard(BaseboardNum)%Offset              = rNumericArgs(3)
        ! Set default convergence tolerance
        IF (Baseboard(BaseboardNum)%Offset .LE. 0.0d0) THEN
          Baseboard(BaseboardNum)%Offset = 0.001d0
        END IF
      END DO

      IF (ErrorsFound) THEN
        CALL ShowFatalError(RoutineName//'Errors found in getting input.  Preceding condition(s) cause termination.')
      ENDIF
    END IF

    DO BaseboardNum = 1,NumBaseboards

      ! Setup Report variables for the unit
      ! CurrentModuleObject='ZoneHVAC:Baseboard:Convective:Water'
      CALL SetupOutputVariable('Baseboard Total Heating Energy [J]', Baseboard(BaseboardNum)%Energy, &
                               'System','Sum',Baseboard(BaseboardNum)%EquipID, &
                               ResourceTypeKey='ENERGYTRANSFER',EndUseKey='BASEBOARD',GroupKey='System')

      CALL SetupOutputVariable('Baseboard Hot Water Energy [J]', Baseboard(BaseboardNum)%Energy, &
                               'System','Sum',Baseboard(BaseboardNum)%EquipID, &
                               ResourceTypeKey='PLANTLOOPHEATINGDEMAND',EndUseKey='BASEBOARD',GroupKey='System')

      CALL SetupOutputVariable('Baseboard Total Heating Rate [W]', Baseboard(BaseboardNum)%Power, &
                               'System','Average',Baseboard(BaseboardNum)%EquipID)

      CALL SetupOutputVariable('Baseboard Hot Water Mass Flow Rate [kg/s]', Baseboard(BaseboardNum)%WaterMassFlowRate, &
                               'System','Average',Baseboard(BaseboardNum)%EquipID)

      CALL SetupOutputVariable('Baseboard Air Mass Flow Rate [kg/s]', Baseboard(BaseboardNum)%AirMassFlowRate, &
                               'System','Average',Baseboard(BaseboardNum)%EquipID)

      CALL SetupOutputVariable('Baseboard Air Inlet Temperature [C]', Baseboard(BaseboardNum)%AirInletTemp, &
                               'System','Average',Baseboard(BaseboardNum)%EquipID)

      CALL SetupOutputVariable('Baseboard Air Outlet Temperature [C]', Baseboard(BaseboardNum)%AirOutletTemp, &
                               'System','Average',Baseboard(BaseboardNum)%EquipID)

      CALL SetupOutputVariable('Baseboard Water Inlet Temperature [C]', Baseboard(BaseboardNum)%WaterInletTemp, &
                               'System','Average',Baseboard(BaseboardNum)%EquipID)

      CALL SetupOutputVariable('Baseboard Water Outlet Temperature [C]', Baseboard(BaseboardNum)%WaterOutletTemp, &
                               'System','Average',Baseboard(BaseboardNum)%EquipID)
    END DO


    RETURN
  END SUBROUTINE GetBaseboardInput

SUBROUTINE InitBaseboard(BaseboardNum, ControlledZoneNumSub)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   Nov 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes the Baseboard units during simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataLoopNode, ONLY: Node
  USE DataZoneEquipment,  ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList,ZoneEquipConfig
  USE PlantUtilities,     ONLY: InitComponentNodes
  USE DataPlant,          ONLY: ScanPlantLoopsForObject

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: BaseboardNum
  INTEGER, INTENT(IN) :: ControlledZoneNumSub

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER        :: WaterInletNode
  INTEGER        :: ZoneNode
  LOGICAL,SAVE   :: MyOneTimeFlag = .true.
  LOGICAL,SAVE   :: ZoneEquipmentListChecked = .false.  ! True after the Zone Equipment List has been checked for items
  Integer        :: Loop
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  REAL(r64)      :: RhoAirStdInit
  REAL(r64)      :: rho ! local fluid density
  REAL(r64)      :: Cp  ! local fluid specific heat
  LOGICAL        :: errFlag

  IF (Baseboard(BaseboardNum)%ZonePtr <= 0) &
      Baseboard(BaseboardNum)%ZonePtr = ZoneEquipConfig(ControlledZoneNumSub)%ActualZoneNum

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ! initialize the environment and sizing flags
    ALLOCATE(MyEnvrnFlag(NumBaseboards))
    ALLOCATE(MySizeFlag(NumBaseboards))
    ALLOCATE(SetLoopIndexFlag(NumBaseboards))
    MyEnvrnFlag      = .TRUE.
    MySizeFlag       = .TRUE.
    MyOneTimeFlag    = .FALSE.
    SetLoopIndexFlag = .TRUE.
  END IF
  IF(SetLoopIndexFlag(BaseboardNum) .AND. ALLOCATED(PlantLoop))THEN
    errFlag=.false.
    CALL ScanPlantLoopsForObject(Baseboard(BaseboardNum)%EquipID,     &
                                 Baseboard(BaseboardNum)%EquipType,   &
                                 Baseboard(BaseboardNum)%LoopNum,     &
                                 Baseboard(BaseboardNum)%LoopSideNum, &
                                 Baseboard(BaseboardNum)%BranchNum,   &
                                 Baseboard(BaseboardNum)%CompNum,     &
                                 errFlag=errFlag)
    IF (errFlag) THEN
      CALL ShowFatalError('InitBaseboard: Program terminated for previous conditions.')
    ENDIF
    SetLoopIndexFlag(BaseboardNum) = .FALSE.
  ENDIF
  ! need to check all units to see if they are on ZoneHVAC:EquipmentList or issue warning
  IF (.not. ZoneEquipmentListChecked .and. ZoneEquipInputsFilled) THEN
    ZoneEquipmentListChecked=.true.
    DO Loop=1,NumBaseboards
      IF (CheckZoneEquipmentList(cCMO_BBRadiator_Water,Baseboard(Loop)%EquipID)) CYCLE
      CALL ShowSevereError('InitBaseboard: Unit=['//TRIM(cCMO_BBRadiator_Water)//','//  &
         TRIM(Baseboard(Loop)%EquipID)//  &
           '] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.')
    ENDDO
  ENDIF

  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(BaseboardNum) .AND. .NOT. SetLoopIndexFlag(BaseboardNum)) THEN
    ! for each coil, do the sizing once.
    CALL SizeBaseboard(BaseboardNum)

    MySizeFlag(BaseboardNum) = .FALSE.
  END IF

  ! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .AND. MyEnvrnFlag(BaseboardNum).AND. .NOT. SetLoopIndexFlag(BaseboardNum)) THEN
    RhoAirStdInit = StdRhoAir
    WaterInletNode = Baseboard(BaseboardNum)%WaterInletNode
    rho = GetDensityGlycol(PlantLoop(Baseboard(BaseboardNum)%LoopNum)%FluidName,  &
                           InitConvTemp, &
                           PlantLoop(Baseboard(BaseboardNum)%LoopNum)%FluidIndex,&
                           'BaseboardRadiator:InitBaseboard')
    Baseboard(BaseboardNum)%WaterMassFlowRateMax = rho * Baseboard(BaseboardNum)%WaterVolFlowRateMax
    CALL InitComponentNodes(0.d0,Baseboard(BaseboardNum)%WaterMassFlowRateMax, &
                                 Baseboard(BaseboardNum)%WaterInletNode,       &
                                 Baseboard(BaseboardNum)%WaterOutletNode,       &
                                 Baseboard(BaseboardNum)%LoopNum,              &
                                 Baseboard(BaseboardNum)%LoopSideNum,          &
                                 Baseboard(BaseboardNum)%BranchNum,            &
                                 Baseboard(BaseboardNum)%CompNum)
    Node(WaterInletNode)%Temp          = 60.0d0
    Cp =  GetSpecificHeatGlycol(PlantLoop(Baseboard(BaseboardNum)%LoopNum)%FluidName,  &
                                Node(WaterInletNode)%Temp,                      &
                                PlantLoop(Baseboard(BaseboardNum)%LoopNum)%FluidIndex, &
                                'BaseboardRadiator:InitBaseboard')
    Node(WaterInletNode)%Enthalpy      = Cp*Node(WaterInletNode)%Temp
    Node(WaterInletNode)%Quality       = 0.0d0
    Node(WaterInletNode)%Press         = 0.0d0
    Node(WaterInletNode)%HumRat        = 0.0d0
    ! pick a mass flow rate that depends on the max water mass flow rate. CR 8842 changed to factor of 2.0
    IF (Baseboard(BaseboardNum)%AirMassFlowRate <= 0.0d0) THEN
      Baseboard(BaseboardNum)%AirMassFlowRate = 2.0d0*Baseboard(BaseboardNum)%WaterMassFlowRateMax
    END IF
    MyEnvrnFlag(BaseboardNum) = .FALSE.
  END IF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(BaseboardNum) = .true.
  ENDIF

  ! Do the every time step initializations
  WaterInletNode = Baseboard(BaseboardNum)%WaterInletNode
  ZoneNode = ZoneEquipConfig(ControlledZoneNumSub)%ZoneNode
  Baseboard(BaseboardNum)%WaterMassFlowRate = Node(WaterInletNode)%MassFlowRate
  Baseboard(BaseboardNum)%WaterInletTemp = Node(WaterInletNode)%Temp
  Baseboard(BaseboardNum)%WaterInletEnthalpy = Node(WaterInletNode)%Enthalpy
  Baseboard(BaseboardNum)%AirInletTemp = Node(ZoneNode)%Temp
  Baseboard(BaseboardNum)%AirInletHumRat = Node(ZoneNode)%HumRat

  RETURN
END SUBROUTINE InitBaseboard

SUBROUTINE SizeBaseboard(BaseboardNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   February 2002
          !       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing hot water baseboard components for which flow rates and UAs have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone sizing arrays and plant sizing data. UAs are
          ! calculated by numerically inverting the baseboard calculation routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE General, ONLY: SolveRegulaFalsi, RoundSigDigits
  USE PlantUtilities, ONLY: RegisterPlantCompDesignFlow
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE DataLoopNode, ONLY: Node

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: BaseboardNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: Acc =  0.0001d0       ! Accuracy of result
  INTEGER, PARAMETER          :: MaxIte = 500        ! Maximum number of iterations

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: WaterInletNode
  INTEGER             :: PltSizNum       ! do loop index for plant sizing
  INTEGER             :: PltSizHeatNum   ! index of plant sizing object for 1st heating loop
  REAL(r64)           :: DesCoilLoad
  INTEGER             :: SolFla          ! Flag of solver
  REAL(r64)           :: UA0             ! lower bound for UA
  REAL(r64)           :: UA1             ! upper bound for UA
  REAL(r64)           :: UA
  REAL(r64), DIMENSION(2)  :: Par
  LOGICAL             :: ErrorsFound     ! If errors detected in input
  REAL(r64)           :: rho             ! local fluid density
  REAL(r64)           :: Cp              ! local fluid specific heat
  REAL(r64)           :: tmpWaterVolFlowRateMax ! local design plant fluid flow rate
  LOGICAL             :: FlowAutosize            ! Indicator to autosizing water volume flow
  LOGICAL             :: UAAutosize              ! Indicator to autosizing UA
  REAL(r64)           :: WaterVolFlowRateMaxDes  ! Design water volume flow for reproting
  REAL(r64)           :: WaterVolFlowRateMaxUser ! User hard-sized volume flow for reporting
  REAL(r64)           :: UADes                   ! Design UA value for reproting
  REAL(r64)           :: UAUser                  ! User hard-sized value for reporting

  PltSizHeatNum = 0
  PltSizNum = 0
  DesCoilLoad = 0.0d0
  ErrorsFound = .FALSE.
  FlowAutosize = .FALSE.
  UAAutosize = .FALSE.
  WaterVolFlowRateMaxDes = 0.0d0
  WaterVolFlowRateMaxUser = 0.0d0
  UADes = 0.0d0
  UAUser = 0.0d0

  ! find the appropriate heating Plant Sizing object
  PltSizHeatNum = PlantLoop(Baseboard(BaseboardNum)%LoopNum)%PlantSizNum

  IF (PltSizHeatNum > 0) THEN

    IF (CurZoneEqNum > 0) THEN

      IF (Baseboard(BaseboardNum)%WaterVolFlowRateMax == AutoSize) THEN
        FlowAutosize = .TRUE.
      END IF
      IF (.NOT. FlowAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation should continue
        IF (Baseboard(BaseboardNum)%WaterVolFlowRateMax > 0.0d0) THEN
          CALL ReportSizingOutput(cCMO_BBRadiator_Water,Baseboard(BaseboardNum)%EquipID,&
                 'User-Specified Maximum Water Flow Rate [m3/s]',Baseboard(BaseboardNum)%WaterVolFlowRateMax)
        END IF
      ELSE
        CALL CheckZoneSizing(cCMO_BBRadiator_Water,Baseboard(BaseboardNum)%EquipID)
        DesCoilLoad = CalcFinalZoneSizing(CurZoneEqNum)%DesHeatLoad * CalcFinalZoneSizing(CurZoneEqNum)%HeatSizingFactor
        IF (DesCoilLoad >= SmallLoad) THEN
          Cp =  GetSpecificHeatGlycol(PlantLoop(Baseboard(BaseboardNum)%LoopNum)%FluidName,  &
                                     60.0d0,                      &
                                     PlantLoop(Baseboard(BaseboardNum)%LoopNum)%FluidIndex, &
                                     cCMO_BBRadiator_Water//':SizeBaseboard')
          rho = GetDensityGlycol(PlantLoop(Baseboard(BaseboardNum)%LoopNum)%FluidName,  &
                                      InitConvTemp, &
                                      PlantLoop(Baseboard(BaseboardNum)%LoopNum)%FluidIndex,&
                                      cCMO_BBRadiator_Water//':SizeBaseboard')
          WaterVolFlowRateMaxDes = DesCoilLoad / ( PlantSizData(PltSizHeatNum)%DeltaT * Cp * rho )
        ELSE
          WaterVolFlowRateMaxDes = 0.0d0
        END IF

        IF (FlowAutosize) THEN
          Baseboard(BaseboardNum)%WaterVolFlowRateMax = WaterVolFlowRateMaxDes
          CALL ReportSizingOutput(cCMO_BBRadiator_Water,Baseboard(BaseboardNum)%EquipID,&
                                'Design Size Maximum Water Flow Rate [m3/s]',WaterVolFlowRateMaxDes)
        ELSE ! hard-sized with sizing data
          IF (Baseboard(BaseboardNum)%WaterVolFlowRateMax > 0.0d0 .AND. WaterVolFlowRateMaxDes > 0.0d0) THEN
            WaterVolFlowRateMaxUser = Baseboard(BaseboardNum)%WaterVolFlowRateMax
            CALL ReportSizingOutput(cCMO_BBRadiator_Water,Baseboard(BaseboardNum)%EquipID,&
                                'Design Size Maximum Water Flow Rate [m3/s]',WaterVolFlowRateMaxDes, &
                                'User-Specified Maximum Water Flow Rate [m3/s]',WaterVolFlowRateMaxUser)
            ! Report a warning to note difference between the two
            IF (DisplayExtraWarnings) THEN
              IF ((ABS(WaterVolFlowRateMaxDes - WaterVolFlowRateMaxUser)/WaterVolFlowRateMaxUser) > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizeBaseboard: Potential issue with equipment sizing for ZoneHVAC:Baseboard:Convective:Water="' &
                                           //   TRIM(Baseboard(BaseboardNum)%EquipID)//'".')
                CALL ShowContinueError('User-Specified Maximum Water Flow Rate of '// &
                                      TRIM(RoundSigDigits(WaterVolFlowRateMaxUser,5))// ' [m3/s]')
                CALL ShowContinueError('differs from Design Size Maximum Water Flow Rate of ' // &
                                      TRIM(RoundSigDigits(WaterVolFlowRateMaxDes,5))// ' [m3/s]')
                CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
              END IF
            ENDIF
          END IF
        END IF
      END IF

          ! UA sizing
          ! Set hard-sized values to the local variable to correct a false indication aftet SolFla function calculation
      IF (Baseboard(BaseboardNum)%UA == Autosize) THEN
        UAAutosize = .TRUE.
      ELSE
        UAUser = Baseboard(BaseboardNum)%UA
      END IF
      IF (.NOT. UAAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation should continue
        IF (Baseboard(BaseboardNum)%UA >0.0d0) THEN
          CALL ReportSizingOutput(cCMO_BBRadiator_Water,Baseboard(BaseboardNum)%EquipID, &
                                  'User-Specified U-Factor Times Area Value [W/K]',Baseboard(BaseboardNum)%UA)
        END IF
      ELSE
        !CALL CheckZoneSizing(cCMO_BBRadiator_Water,Baseboard(BaseboardNum)%EquipID)
        Baseboard(BaseboardNum)%WaterInletTemp = PlantSizData(PltSizHeatNum)%ExitTemp
        Baseboard(BaseboardNum)%AirInletTemp = FinalZoneSizing(CurZoneEqNum)%ZoneTempAtHeatPeak
        Baseboard(BaseboardNum)%AirInletHumRat = FinalZoneSizing(CurZoneEqNum)%ZoneHumRatAtHeatPeak
        WaterInletNode = Baseboard(BaseboardNum)%WaterInletNode
        rho = GetDensityGlycol(PlantLoop(Baseboard(BaseboardNum)%LoopNum)%FluidName,  &
                               InitConvTemp, &
                               PlantLoop(Baseboard(BaseboardNum)%LoopNum)%FluidIndex,&
                               cCMO_BBRadiator_Water//':SizeBaseboard')
        Node(WaterInletNode)%MassFlowRate = rho * Baseboard(BaseboardNum)%WaterVolFlowRateMax
        DesCoilLoad = CalcFinalZoneSizing(CurZoneEqNum)%DesHeatLoad * CalcFinalZoneSizing(CurZoneEqNum)%HeatSizingFactor
        IF (DesCoilLoad >= SmallLoad) THEN
           ! pick an air  mass flow rate that is twice the water mass flow rate (CR8842)
          Baseboard(BaseboardNum)%DesAirMassFlowRate = 2.0d0 * rho * Baseboard(BaseboardNum)%WaterVolFlowRateMax
           ! pass along the coil number and the design load to the residual calculation
          Par(1) = DesCoilLoad
          Par(2) = BaseboardNum
           ! set the lower and upper limits on the UA
          UA0 = .001d0 * DesCoilLoad
          UA1 = DesCoilLoad
            ! Invert the baseboard model: given the design inlet conditions and the design load,
            ! find the design UA.
          CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, UA, HWBaseboardUAResidual, UA0, UA1, Par)
            ! if the numerical inversion failed, issue error messages.
          IF (SolFla == -1) THEN
            CALL ShowSevereError('SizeBaseboard: Autosizing of HW baseboard UA failed for '//    &
                 cCMO_BBRadiator_Water//'="'// &
                 TRIM(Baseboard(BaseboardNum)%EquipID)//'"')
            CALL ShowContinueError('Iteration limit exceeded in calculating coil UA')
            IF (UAAutosize) THEN 
              ErrorsFound = .TRUE.
            ELSE
              CALL ShowContinueError('Could not calculate design value for comparison to user value, and the simulation continues')
              UA = 0.0d0
            ENDIF
          ELSE IF (SolFla == -2) THEN
            CALL ShowSevereError('SizeBaseboard: Autosizing of HW baseboard UA failed for '//    &
                 cCMO_BBRadiator_Water//'="'// &
                 TRIM(Baseboard(BaseboardNum)%EquipID)//'"')
            CALL ShowContinueError('Bad starting values for UA')
            IF (UAAutosize) THEN 
              ErrorsFound = .TRUE.
            ELSE
              CALL ShowContinueError('Could not calculate design value for comparison to user value, and the simulation continues')
              UA = 0.0d0
            ENDIF
          END IF
            UADes = UA !Baseboard(BaseboardNum)%UA = UA
        ELSE
          UADes = 0.0d0
        END IF

        IF (UAAutosize) THEN
          Baseboard(BaseboardNum)%UA = UADes
          CALL ReportSizingOutput(cCMO_BBRadiator_Water,Baseboard(BaseboardNum)%EquipID, &
                                  'Design Size U-Factor Times Area Value [W/K]',UADes)
        ELSE ! Hard-sized with sizing data
          Baseboard(BaseboardNum)%UA = UAUser ! need to put this back as HWBaseboardUAResidual will have reset it, CR9377
          IF (UAUser > 0.0d0 .AND. UADes > 0.0d0) THEN
            CALL ReportSizingOutput(cCMO_BBRadiator_Water,Baseboard(BaseboardNum)%EquipID, &
                                  'Design Size U-Factor Times Area Value [W/K]',UADes, &
                                  'User-Specified U-Factor Times Area Value [W/K]',UAUser)
            ! Report difference between design size and hard-sized values
            IF (DisplayExtraWarnings) THEN
              IF ((ABS(UADes-UAUser)/UAUser) > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizeBaseboard: Potential issue with equipment sizing for ZoneHVAC:Baseboard:Convective:Water="' &
                                     //       TRIM(Baseboard(BaseboardNum)%EquipID)//'".')
                CALL ShowContinueError('User-Specified U-Factor Times Area Value of '// &
                                      TRIM(RoundSigDigits(UAUser,2))// ' [W/K]')
                CALL ShowContinueError('differs from Design Size U-Factor Times Area Value of ' // &
                                      TRIM(RoundSigDigits(UADes,2))// ' [W/K]')
                CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
              END IF
            ENDIF
          END IF
        END IF
      END IF
    END IF
  ELSE
      ! if there is no heating Sizing:Plant object and autosizng was requested, issue an error message
    IF (FlowAutoSize .OR. UAAutoSize) THEN
      CALL ShowSevereError('SizeBaseboard: '//cCMO_BBRadiator_Water//'="'// &
                 TRIM(Baseboard(BaseboardNum)%EquipID)//'"')
      CALL ShowContinueError('...Autosizing of hot water baseboard requires a heating loop Sizing:Plant object')
      ErrorsFound = .TRUE.
    END IF
  END IF

  ! save the design water flow rate for use by the water loop sizing algorithms
  CALL RegisterPlantCompDesignFlow(Baseboard(BaseboardNum)%WaterInletNode,Baseboard(BaseboardNum)%WaterVolFlowRateMax)

  IF (ErrorsFound) THEN
    CALL ShowFatalError('SizeBaseboard: Preceding sizing errors cause program termination')
  END IF

  RETURN
END SUBROUTINE SizeBaseboard

  SUBROUTINE SimHWConvective(BaseboardNum, LoadMet)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   Nov 1997
          !       MODIFIED       May 2000 Fred Buhl
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This subroutine calculates the heat exchange rate
          ! in a pure convective baseboard heater.  The heater is assumed to be crossflow
          ! with both fluids unmixed. The air flow is bouyancy driven and a constant air
          ! flow velocity of 0.5m/s is assumed. The solution is by the effectiveness-NTU
          ! method found in Icropera and DeWitt, Fundamentals of Heat and Mass Transfer,
          ! Chapter 11.4, p. 523, eq. 11.33

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! Icropera and DeWitt, Fundamentals of Heat and Mass Transfer,
          ! Chapter 11.4, p. 523, eq. 11.33

          ! USE STATEMENTS:
    USE DataLoopNode, ONLY: Node
    USE DataSizing
    USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand, CurDeadbandOrSetback
    USE PlantUtilities,        ONLY: SetActuatedBranchFlowRate
    USE DataHVACGlobals, ONLY: SmallLoad

    IMPLICIT NONE
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER      :: BaseboardNum
    REAL(r64)    :: LoadMet

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: ZoneNum
    REAL(r64) :: WaterInletTemp
    REAL(r64) :: AirInletTemp
    REAL(r64) :: CpAir
    REAL(r64) :: CpWater
    REAL(r64) :: AirMassFlowRate
    REAL(r64) :: WaterMassFlowRate
    REAL(r64) :: CapacitanceAir
    REAL(r64) :: CapacitanceWater
    REAL(r64) :: CapacitanceMax
    REAL(r64) :: CapacitanceMin
    REAL(r64) :: CapacityRatio
    REAL(r64) :: NTU
    REAL(r64) :: Effectiveness
    REAL(r64) :: WaterOutletTemp
    REAL(r64) :: AirOutletTemp
    REAL(r64) :: AA
    REAL(r64) :: BB
    REAL(r64) :: CC
    REAL(r64) :: QZnReq

    ZoneNum = Baseboard(BaseboardNum)%ZonePtr
    QZnReq  = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP
    IF (MySizeFlag(BaseboardNum)) & ! If in sizing, assign design condition
        QZnReq = CalcFinalZoneSizing(CurZoneEqNum)%DesHeatLoad * CalcFinalZoneSizing(CurZoneEqNum)%HeatSizingFactor

    WaterInletTemp = Baseboard(BaseboardNum)%WaterInletTemp
    WaterOutletTemp = WaterInletTemp
    AirInletTemp   = Baseboard(BaseboardNum)%AirInletTemp
    AirOutletTemp = AirInletTemp

    CpWater =  GetSpecificHeatGlycol(PlantLoop(Baseboard(BaseboardNum)%LoopNum)%FluidName,  &
                           WaterInletTemp,                      &
                           PlantLoop(Baseboard(BaseboardNum)%LoopNum)%FluidIndex, &
                           cCMO_BBRadiator_Water//':SimHWConvective')
    CpAir = PsyCpAirFnWTdb(Baseboard(BaseboardNum)%AirInletHumRat,AirInletTemp)

    IF (Baseboard(BaseboardNum)%DesAirMassFlowRate > 0.0d0) THEN  ! If UA is autosized, assign design condition
      AirMassFlowRate = Baseboard(BaseboardNum)%DesAirMassFlowRate
    ELSE
      AirMassFlowRate = Baseboard(BaseboardNum)%AirMassFlowRate
            ! pick a mass flow rate that depends on the max water mass flow rate. CR 8842 changed to factor of 2.0
      IF (AirMassFlowRate <= 0.0d0) &
            AirMassFlowRate = 2.0*Baseboard(BaseboardNum)%WaterMassFlowRateMax
    END IF

    WaterMassFlowRate = Node(Baseboard(BaseboardNum)%WaterInletNode)%MassFlowRate
    CapacitanceAir = CpAir * AirMassFlowRate

     IF (QZnReq > SmallLoad &
       .AND. (.NOT. CurDeadbandOrSetback(ZoneNum) .OR. MySizeFlag(BaseboardNum)) &
       .AND. (GetCurrentScheduleValue(Baseboard(BaseboardNum)%SchedPtr) > 0 .OR. MySizeFlag(BaseboardNum)) &
       .AND. (WaterMassFlowRate > 0.0d0) ) THEN
      CapacitanceWater = CpWater * WaterMassFlowRate
      CapacitanceMax = MAX(CapacitanceAir,CapacitanceWater)
      CapacitanceMin = MIN(CapacitanceAir,CapacitanceWater)
      CapacityRatio = CapacitanceMin/CapacitanceMax
      NTU = Baseboard(BaseboardNum)%UA/CapacitanceMin
      ! The effectiveness is given by the following formula:
      ! Effectiveness = 1. - EXP((1./CapacityRatio)*(NTU)**0.22*(EXP(-CapacityRatio*(NTU)**0.78)-1.))
      ! To prevent possible underflows (numbers smaller than the computer can handle) we must break
      ! the calculation up into steps and check the size of the exponential arguments.
      AA = -CapacityRatio*(NTU)**0.78d0
      IF (AA.LT.EXP_LowerLimit) THEN
        BB = 0.0d0
      ELSE
        BB = EXP(AA)
      END IF
      CC = (1.0d0/CapacityRatio)*(NTU)**0.22d0*(BB-1.0d0)
      IF (CC.LT.EXP_LowerLimit) THEN
        Effectiveness = 1.0d0
      ELSE
        Effectiveness  = 1.d0 - EXP(CC)
      END IF
      AirOutletTemp = AirInletTemp + Effectiveness*CapacitanceMin*(WaterInletTemp-AirInletTemp)/CapacitanceAir
      WaterOutletTemp = WaterInletTemp - CapacitanceAir*(AirOutletTemp-AirInletTemp)/CapacitanceWater
      LoadMet = CapacitanceWater*(WaterInletTemp-WaterOutletTemp)
       Baseboard(BaseboardNum)%WaterOutletEnthalpy = Baseboard(BaseboardNum)%WaterInletEnthalpy - &
        LoadMet/WaterMassFlowRate
    ELSE
      CapacitanceWater = 0.0d0
      CapacitanceMax = CapacitanceAir
      CapacitanceMin = 0.0d0
      NTU = 0.0d0
      Effectiveness = 0.0d0
      AirOutletTemp = AirInletTemp
      WaterOutletTemp = WaterInletTemp
      LoadMet = 0.0d0
      Baseboard(BaseboardNum)%WaterOutletEnthalpy = Baseboard(BaseboardNum)%WaterInletEnthalpy
      WaterMassFlowRate = 0.0d0

      CALL SetActuatedBranchFlowRate(WaterMassFlowRate,Baseboard(BaseboardNum)%WaterInletNode,  &
                                   Baseboard(BaseboardNum)%LoopNum,Baseboard(BaseboardNum)%LoopSideNum, &
                                   Baseboard(BaseboardNum)%BranchNum, .FALSE. )
      AirMassFlowRate = 0.0d0
    END IF

    Baseboard(BaseboardNum)%WaterOutletTemp = WaterOutletTemp
    Baseboard(BaseboardNum)%AirOutletTemp = AirOutletTemp
    Baseboard(BaseboardNum)%Power = LoadMet
    Baseboard(BaseboardNum)%WaterMassFlowRate = WaterMassFlowRate
    Baseboard(BaseboardNum)%AirMassFlowRate = AirMassFlowRate

    RETURN
  END SUBROUTINE SimHWConvective

  SUBROUTINE UpdateBaseboard(BaseboardNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   Nov 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This subroutine

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataLoopNode, ONLY: Node
    USE PlantUtilities, ONLY: SafeCopyPlantNode

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER :: BaseboardNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: WaterInletNode
    INTEGER :: WaterOutletNode

   WaterInletNode  = Baseboard(BaseboardNum)%WaterInletNode
   WaterOutletNode = Baseboard(BaseboardNum)%WaterOutletNode

   Call SafeCopyPlantNode(WaterInletNode, WaterOutletNode)
   ! Set the outlet air nodes of the Baseboard
   ! Set the outlet water nodes for the Coil
!   Node(WaterOutletNode)%MassFlowRate = Baseboard(BaseboardNum)%WaterMassFlowRate
   Node(WaterOutletNode)%Temp         = Baseboard(BaseboardNum)%WaterOutletTemp
   Node(WaterOutletNode)%Enthalpy     = Baseboard(BaseboardNum)%WaterOutletEnthalpy

     ! Set the outlet nodes for properties that just pass through & not used

   ! Set the outlet nodes for properties that just pass through & not used
!   Node(WaterOutletNode)%Quality             = Node(WaterInletNode)%Quality
!   Node(WaterOutletNode)%Press               = Node(WaterInletNode)%Press
!   Node(WaterOutletNode)%HumRat              = Node(WaterInletNode)%HumRat
!   Node(WaterOutletNode)%MassFlowRateMin     = Node(WaterInletNode)%MassFlowRateMin
!   Node(WaterOutletNode)%MassFlowRateMax     = Node(WaterInletNode)%MassFlowRateMax
!   Node(WaterOutletNode)%MassFlowRateMinAvail= Node(WaterInletNode)%MassFlowRateMinAvail
!   Node(WaterOutletNode)%MassFlowRateMaxAvail= Node(WaterInletNode)%MassFlowRateMaxAvail

    RETURN
  END SUBROUTINE UpdateBaseboard

  SUBROUTINE ReportBaseboard(BaseboardNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   Nov 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This subroutine

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    Use DataHVACGlobals, ONLY: TimeStepSys

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: BaseboardNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

    Baseboard(BaseboardNum)%Energy = Baseboard(BaseboardNum)%Power * TimeStepSys*SecInHour

    RETURN
  END SUBROUTINE ReportBaseboard

FUNCTION HWBaseboardUAResidual(UA, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   February 2002
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (Design Coil Load - Coil Heating Output) / Design Coil Load.
          ! Coil Heating Output depends on the UA which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Puts UA into the baseboard data structure, calls SimHWConvective, and calculates
          ! the residual as defined above.

          ! REFERENCES:

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: UA ! UA of coil
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = design coil load [W]
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: BaseboardIndex
  REAL(r64)    :: LoadMet

  BaseboardIndex = INT(Par(2))
  Baseboard(BaseboardIndex)%UA = UA
  CALL SimHWConvective(BaseboardIndex,LoadMet)
  Residuum = (Par(1) - LoadMet) / Par(1)

  RETURN
END FUNCTION HWBaseboardUAResidual

SUBROUTINE UpdateBaseboardPlantConnection(BaseboardTypeNum, &
                                          BaseboardName,&
                                          EquipFlowCtrl, &
                                          LoopNum,LoopSide,CompIndex,&
                                          FirstHVACIteration, InitLoopEquip)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma, FSEC
          !       DATE WRITTEN   Sept. 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! update sim routine called from plant

          ! METHODOLOGY EMPLOYED:
          ! check input, provide comp index, call utility routines

          ! REFERENCES:
          ! Based on UpdateBaseboardPlantConnection from Brent Griffith, Sept 2010

          ! USE STATEMENTS:
  USE PlantUtilities , ONLY: PullCompInterconnectTrigger
  USE DataPlant,       ONLY: ccSimPlantEquipTypes, TypeOf_Baseboard_Conv_Water, &
                             CriteriaType_MassFlowRate, CriteriaType_Temperature, CriteriaType_HeatTransferRate
  USE InputProcessor,  ONLY: FindItemInList
  USE General,         ONLY: TrimSigDigits
  USE DataGlobals,     ONLY: KickOffSimulation

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
  INTEGER, INTENT(IN)   :: BaseboardTypeNum     ! type index
  CHARACTER(len=*), INTENT(IN) :: BaseboardName ! component name
  INTEGER, INTENT(IN)    :: EquipFlowCtrl       ! Flow control mode for the equipment
  INTEGER, INTENT(IN)    :: LoopNum             ! Plant loop index for where called from
  INTEGER, INTENT(IN)    :: LoopSide            ! Plant loop side index for where called from
  INTEGER, INTENT(INOUT) :: CompIndex           ! Chiller number pointer
  LOGICAL , INTENT(IN)   :: FirstHVACIteration
  LOGICAL, INTENT(INOUT) :: InitLoopEquip       ! If not zero, calculate the max load for operating conditions

  INTEGER :: BaseboardNum
  INTEGER :: InletNodeNum
  INTEGER :: OutletNodeNum

    ! Find the correct baseboard
  IF (CompIndex == 0) THEN
    BaseboardNum = FindItemInList(BaseboardName,Baseboard%EquipID,NumBaseboards)
    IF (BaseboardNum == 0) THEN
      CALL ShowFatalError('UpdateBaseboardPlantConnection: Invalid Unit Specified '//cCMO_BBRadiator_Water//  &
         '="'//TRIM(BaseboardName)//'"')
    ENDIF
    CompIndex=BaseboardNum
  ELSE
    BaseboardNum=CompIndex
    IF (BaseboardNum > NumBaseboards .or. BaseboardNum < 1) THEN
      CALL ShowFatalError('UpdateBaseboardPlantConnection:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(BaseboardNum))// &
                          ', Number of baseboards='//TRIM(TrimSigDigits(NumBaseboards))//  &
                          ', Entered baseboard name='//TRIM(BaseboardName))
    ENDIF
    IF (KickOffSimulation) THEN
      IF (BaseboardName /= Baseboard(BaseboardNum)%EquipID) THEN
        CALL ShowFatalError('UpdateBaseboardPlantConnection: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(BaseboardNum))// &
                            ', baseboard name='//TRIM(BaseboardName)//', stored baseboard Name for that index='//  &
                            TRIM(Baseboard(BaseboardNum)%EquipID))
      ENDIF
      IF (BaseboardTypeNum /= TypeOf_Baseboard_Conv_Water) THEN
        CALL ShowFatalError('UpdateBaseboardPlantConnection: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(BaseboardNum))// &
                            ', baseboard name='//TRIM(BaseboardName)//', stored baseboard Name for that index='//  &
                            TRIM(ccSimPlantEquipTypes(BaseboardTypeNum)) )
      ENDIF
    ENDIF
  ENDIF

  IF (InitLoopEquip) THEN
    RETURN
  END IF

  CALL PullCompInterconnectTrigger(Baseboard(BaseboardNum)%LoopNum,     &
                                   Baseboard(BaseboardNum)%LoopSideNum, &
                                   Baseboard(BaseboardNum)%BranchNum,   &
                                   Baseboard(BaseboardNum)%CompNum,     &
                                   Baseboard(BaseboardNum)%BBLoadReSimIndex, &
                                   Baseboard(BaseboardNum)%LoopNum,     &
                                   Baseboard(BaseboardNum)%LoopSideNum, &
                                   CriteriaType_HeatTransferRate,         &
                                   Baseboard(BaseboardNum)%Power)

  CALL PullCompInterconnectTrigger(Baseboard(BaseboardNum)%LoopNum,     &
                                   Baseboard(BaseboardNum)%LoopSideNum, &
                                   Baseboard(BaseboardNum)%BranchNum,   &
                                   Baseboard(BaseboardNum)%CompNum,     &
                                   Baseboard(BaseboardNum)%BBLoadReSimIndex, &
                                   Baseboard(BaseboardNum)%LoopNum,     &
                                   Baseboard(BaseboardNum)%LoopSideNum, &
                                   CriteriaType_MassFlowRate,         &
                                   Baseboard(BaseboardNum)%WaterMassFlowRate)

  CALL PullCompInterconnectTrigger(Baseboard(BaseboardNum)%LoopNum,     &
                                   Baseboard(BaseboardNum)%LoopSideNum, &
                                   Baseboard(BaseboardNum)%BranchNum,   &
                                   Baseboard(BaseboardNum)%CompNum,     &
                                   Baseboard(BaseboardNum)%BBLoadReSimIndex, &
                                   Baseboard(BaseboardNum)%LoopNum,     &
                                   Baseboard(BaseboardNum)%LoopSideNum, &
                                   CriteriaType_Temperature,         &
                                   Baseboard(BaseboardNum)%WaterOutletTemp)
  RETURN

END SUBROUTINE UpdateBaseboardPlantConnection

END MODULE BaseboardRadiator


!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!******************************************************************************************************
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!******************************************************************************************************
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


MODULE BaseboardElectric
  ! Module containing the routines dealing with the BASEBOARD Electric HEATER
  ! component(s).

  ! MODULE INFORMATION:  Richard Liesen
  !       DATE WRITTEN   Nov 2001
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! Needs description

  ! METHODOLOGY EMPLOYED:
  ! Needs description, as appropriate

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataGlobals
USE DataInterfaces

  ! Use statements for access to subroutines in other modules
USE ScheduleManager

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS
  CHARACTER(len=*), PARAMETER :: cCMO_BBRadiator_Electric = 'ZoneHVAC:Baseboard:Convective:Electric'
  REAL(r64), PARAMETER :: SimpConvAirFlowSpeed = 0.5d0 !m/s

  ! DERIVED TYPE DEFINITIONS
  TYPE BaseboardParams
    CHARACTER(len=MaxNameLength) :: EquipName =' '
    CHARACTER(len=MaxNameLength) :: EquipType =' '
    CHARACTER(len=MaxNameLength) :: Schedule  =' '
    INTEGER      :: SchedPtr                       = 0
    REAL(r64)    :: NominalCapacity                =0.0d0
    REAL(r64)    :: BaseBoardEfficiency            =0.0d0
    REAL(r64)    :: AirInletTemp                   =0.0d0
    REAL(r64)    :: AirInletHumRat                 =0.0d0
    REAL(r64)    :: AirOutletTemp                  =0.0d0
    REAL(r64)    :: Power                          =0.0d0
    REAL(r64)    :: Energy                         =0.0d0
    REAL(r64)    :: ElecUseLoad                    =0.0d0
    REAL(r64)    :: ElecUseRate                    =0.0d0
  END TYPE BaseboardParams

  !MODULE VARIABLE DECLARATIONS:
  TYPE (BaseboardParams), ALLOCATABLE, DIMENSION(:) :: Baseboard
  INTEGER :: NumBaseboards=0
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlag
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

  !SUBROUTINE SPECIFICATIONS FOR MODULE BaseboardRadiator

  PUBLIC  :: SimElectricBaseBoard
  PUBLIC  :: SimElectricConvective
  PRIVATE :: GetBaseboardInput
  PRIVATE :: InitBaseboard
  PRIVATE :: SizeElectricBaseboard
  PRIVATE :: ReportBaseboard


CONTAINS

  SUBROUTINE SimElectricBaseBoard(EquipName, ActualZoneNum, ControlledZoneNum, PowerMet, CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   Nov 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the Electric Baseboard units.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE DataLoopNode, ONLY: Node
    USE InputProcessor, ONLY: FindItemInList
    USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand
    USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    CHARACTER(len=*), INTENT(IN) :: EquipName
    INTEGER, INTENT(IN) :: ActualZoneNum
    INTEGER, INTENT(IN) :: ControlledZoneNum
    REAL(r64), INTENT(OUT)   :: PowerMet
    INTEGER, INTENT(INOUT) :: CompIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    INTEGER        :: BaseboardNum ! index of unit in baseboard array
    LOGICAL,SAVE   :: GetInputFlag = .TRUE. ! one time get input flag
    REAL(r64)      :: QZnReq       ! zone load not yet satisfied


    IF (GetInputFlag) THEN
      CALL GetBaseboardInput
      GetInputFlag=.false.
    END IF

    ! Find the correct Baseboard Equipment
    IF (CompIndex == 0) THEN
      BaseboardNum = FindItemInList(EquipName, Baseboard%EquipName, NumBaseboards)
      IF (BaseboardNum == 0) THEN
        CALL ShowFatalError('SimElectricBaseboard: Unit not found='//TRIM(EquipName))
      ENDIF
      CompIndex=BaseboardNum
    ELSE
      BaseboardNum=CompIndex
      IF (BaseboardNum > NumBaseboards .or. BaseboardNum < 1) THEN
        CALL ShowFatalError('SimElectricBaseboard:  Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(BaseboardNum))// &
                            ', Number of Units='//TRIM(TrimSigDigits(NumBaseboards))//  &
                            ', Entered Unit name='//TRIM(EquipName))
      ENDIF
      IF (CheckEquipName(BaseboardNum)) THEN
        IF (EquipName /= Baseboard(BaseboardNum)%EquipName) THEN
          CALL ShowFatalError('SimElectricBaseboard: Invalid CompIndex passed='//  &
                              TRIM(TrimSigDigits(BaseboardNum))// &
                              ', Unit name='//TRIM(EquipName)//', stored Unit Name for that index='//  &
                              TRIM(Baseboard(BaseboardNum)%EquipName))
        ENDIF
        CheckEquipName(BaseboardNum)=.false.
      ENDIF
    ENDIF

    CALL InitBaseboard(BaseboardNum, ControlledZoneNum)

    QZnReq = ZoneSysEnergyDemand(ActualZoneNum)%RemainingOutputReqToHeatSP

       ! Simulate baseboard
    CALL SimElectricConvective(BaseboardNum,QZnReq)

    PowerMet = Baseboard(BaseboardNum)%Power

    CALL ReportBaseboard(BaseboardNum)

  END SUBROUTINE SimElectricBaseBoard

  SUBROUTINE GetBaseboardInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   Nov 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the input for the Baseboard units.

          ! METHODOLOGY EMPLOYED:
          ! Standard input processor calls.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, MakeUPPERCase, SameString
    USE GlobalNames,    ONLY : VerifyUniqueBaseboardName
    USE DataIPShortCuts


    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER  :: RoutineName='GetBaseboardInput: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: BaseboardNum
    INTEGER :: NumConvElecBaseboards
    INTEGER :: ConvElecBBNum
    INTEGER :: NumAlphas
    INTEGER :: NumNums
    INTEGER :: IOSTAT
    LOGICAL :: ErrorsFound = .false.  ! If errors detected in input
    LOGICAL :: IsNotOK                ! Flag to verify name
    LOGICAL :: IsBlank                ! Flag for blank name
    LOGICAL :: errflag

          !BASEBOARD HEATER:ELECTRIC:Convective,
          !  A1 , \field Baseboard Name
          !        \required-field
          !  A2 , \field Available Schedule
          !        \required-field
          !       \type object-list
          !       \object-list ScheduleNames
          !  N1 , \field Efficiency of the Coil
          !       \maximum 1.0
          !       \minimum 0.0
          !       \default 1.0
          !  N2 ; \field Nominal Capacity of the Coil
          !       \units W

    cCurrentModuleObject = cCMO_BBRadiator_Electric

    NumConvElecBaseboards = GetNumObjectsFound(cCurrentModuleObject)

    ! Calculate total number of baseboard units
    NumBaseboards = NumConvElecBaseboards

    ALLOCATE(Baseboard(NumBaseboards))
    ALLOCATE(CheckEquipName(NumBaseboards))
    CheckEquipName=.true.

    IF (NumConvElecBaseboards .GT. 0)THEN  !Get the data for cooling schemes
      BaseBoardNum=0
      DO ConvElecBBNum = 1,  NumConvElecBaseboards

        CALL GetObjectItem(cCurrentModuleObject,ConvElecBBNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNums,IOSTAT, &
                           NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                           AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(cAlphaArgs(1),Baseboard%EquipName,BaseboardNum,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          CYCLE
        ENDIF
        CALL VerifyUniqueBaseboardName(TRIM(cCurrentModuleObject),cAlphaArgs(1),errflag,TRIM(cCurrentModuleObject)//' Name')
        IF (errflag) THEN
          ErrorsFound=.true.
        ENDIF
        BaseboardNum = BaseboardNum + 1
        Baseboard(BaseboardNum)%EquipName = cAlphaArgs(1) ! name of this baseboard
        Baseboard(BaseboardNum)%EquipType = MakeUPPERCase(cCurrentModuleObject) ! the type of baseboard-rename change
        Baseboard(BaseboardNum)%Schedule = cAlphaArgs(2)
        IF (lAlphaFieldBlanks(2)) THEN
          Baseboard(BaseboardNum)%SchedPtr = ScheduleAlwaysOn
        ELSE
          Baseboard(BaseboardNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(2))
          IF (Baseboard(BaseboardNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(2))//  &
                                  ' entered ='//TRIM(cAlphaArgs(2))// &
                                  ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
            ErrorsFound=.true.
          END IF
        ENDIF
        ! get inlet node number
        Baseboard(BaseboardNum)%NominalCapacity      = rNumericArgs(1)
        Baseboard(BaseboardNum)%BaseBoardEfficiency  = rNumericArgs(2)
      END DO

      IF (ErrorsFound) THEN
        CALL ShowFatalError(RoutineName//'Errors found in getting input.  Preceding condition(s) cause termination.')
      ENDIF
    END IF

    DO BaseboardNum = 1,NumBaseboards

      ! Setup Report variables for the Electric BaseBoards
      ! CurrentModuleObject='ZoneHVAC:Baseboard:Convective:Electric'
      CALL SetupOutputVariable('Baseboard Total Heating Energy [J]', Baseboard(BaseboardNum)%Energy, &
                               'System','Sum',Baseboard(BaseboardNum)%EquipName, &
                               ResourceTypeKey='ENERGYTRANSFER',EndUseKey='BASEBOARD',GroupKey='System')

      CALL SetupOutputVariable('Baseboard Total Heating Rate [W]', Baseboard(BaseboardNum)%Power, &
                               'System','Average',Baseboard(BaseboardNum)%EquipName)

      CALL SetupOutputVariable('Baseboard Electric Energy [J]',Baseboard(BaseboardNum)%ElecUseLoad, &
                              'System','Sum',Baseboard(BaseboardNum)%EquipName,  &
                               ResourceTypeKey='Electric',EndUseKey='HEATING',GroupKey='System')

      CALL SetupOutputVariable('Baseboard Electric Power [W]',Baseboard(BaseboardNum)%ElecUseRate, &
                              'System','Average',Baseboard(BaseboardNum)%EquipName)

    END DO


    RETURN
  END SUBROUTINE GetBaseboardInput

SUBROUTINE InitBaseboard(BaseboardNum, ControlledZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   Nov 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes the Baseboard units during simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataLoopNode, ONLY: Node
  USE DataZoneEquipment,  ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList,ZoneEquipConfig

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: BaseboardNum
  INTEGER, INTENT(IN) :: ControlledZoneNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER        :: ZoneNode
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL,SAVE        :: ZoneEquipmentListChecked = .false.  ! True after the Zone Equipment List has been checked for items
  Integer             :: Loop
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ! initialize the environment and sizing flags
    ALLOCATE(MyEnvrnFlag(NumBaseboards))
    ALLOCATE(MySizeFlag(NumBaseboards))
    MyEnvrnFlag = .TRUE.
    MySizeFlag = .TRUE.

    MyOneTimeFlag = .false.

  END IF

  ! need to check all units to see if they are on ZoneHVAC:EquipmentList or issue warning
  IF (.not. ZoneEquipmentListChecked .and. ZoneEquipInputsFilled) THEN
    ZoneEquipmentListChecked=.true.
    DO Loop=1,NumBaseboards
      IF (CheckZoneEquipmentList(Baseboard(Loop)%EquipType,Baseboard(Loop)%EquipName)) CYCLE
      CALL ShowSevereError('InitBaseboard: Unit=['//TRIM(Baseboard(Loop)%EquipType)//','//  &
         TRIM(Baseboard(Loop)%EquipName)//  &
           '] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.')
    ENDDO
  ENDIF

  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(BaseboardNum)) THEN
    ! for each coil, do the sizing once.
    CALL SizeElectricBaseboard(BaseboardNum)

    MySizeFlag(BaseboardNum) = .FALSE.
  END IF

  ! Set the reporting variables to zero at each timestep.
  Baseboard(BaseboardNum)%Energy = 0.0d0
  Baseboard(BaseboardNum)%Power  = 0.0d0
  Baseboard(BaseboardNum)%ElecUseLoad = 0.0d0
  Baseboard(BaseboardNum)%ElecUseRate = 0.0d0

   ! Do the every time step initializations
  ZoneNode = ZoneEquipConfig(ControlledZoneNum)%ZoneNode
  Baseboard(BaseboardNum)%AirInletTemp = Node(ZoneNode)%Temp
  Baseboard(BaseboardNum)%AirInletHumRat = Node(ZoneNode)%HumRat


 RETURN
END SUBROUTINE InitBaseboard

SUBROUTINE SizeElectricBaseboard(BaseboardNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   February 2002
          !       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing electric baseboard components for which nominal capacities have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone sizing arrays and plant sizing data. UAs are
          ! calculated by numerically inverting the baseboard calculation routine.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE General,             ONLY: RoundSigDigits



  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: BaseboardNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL   :: IsAutosize           ! Indicator to autosizing nominal capacity
  REAL(r64) :: NominalCapacityDes   ! Design nominal capacity for reporting
  REAL(r64) :: NominalCapacityUser  ! User hard-sized nominal capacity for reporting

  IsAutosize = .FALSE.
  NominalCapacityDes = 0.0d0
  NominalCapacityUser = 0.0d0

  IF (CurZoneEqNum > 0) THEN
    IF (Baseboard(BaseboardNum)%NominalCapacity == AutoSize) THEN
      IsAutosize = .TRUE.
    END IF
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
      IF (Baseboard(BaseboardNum)%NominalCapacity > 0.0d0) THEN
        CALL ReportSizingOutput(cCMO_BBRadiator_Electric,Baseboard(BaseboardNum)%EquipName,&
                                'User-Specified Nominal Capacity [W]',Baseboard(BaseboardNum)%NominalCapacity)
      END IF
    ELSE ! Autosize or hard-size with design run
      CALL CheckZoneSizing(Baseboard(BaseboardNum)%EquipType,Baseboard(BaseboardNum)%EquipName)
      NominalCapacityDes = CalcFinalZoneSizing(CurZoneEqNum)%DesHeatLoad *   &
                            CalcFinalZoneSizing(CurZoneEqNum)%HeatSizingFactor

      IF (IsAutosize) THEN
        Baseboard(BaseboardNum)%NominalCapacity = NominalCapacityDes
        CALL ReportSizingOutput(cCMO_BBRadiator_Electric,Baseboard(BaseboardNum)%EquipName,&
                                'Design Size Nominal Capacity [W]',NominalCapacityDes)
      ELSE ! hard-sized with sizing data
        IF (Baseboard(BaseboardNum)%NominalCapacity > 0.0d0 .AND. NominalCapacityDes > 0.0d0) THEN
          NominalCapacityUser = Baseboard(BaseboardNum)%NominalCapacity
          CALL ReportSizingOutput(cCMO_BBRadiator_Electric,Baseboard(BaseboardNum)%EquipName,&
                                'Design Size Nominal Capacity [W]',NominalCapacityDes, &
                                'User-Specified Nominal Capacity [W]',NominalCapacityUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(NominalCapacityDes - NominalCapacityUser)/NominalCapacityUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeBaseboard: Potential issue with equipment sizing for ZoneHVAC:Baseboard:Convective:Electric="'&
                                   //      TRIM(Baseboard(BaseboardNum)%EquipName)//'".')
              CALL ShowContinueError('User-Specified Nominal Capacity of '// &
                                      TRIM(RoundSigDigits(NominalCapacityUser,2))// ' [W]')
              CALL ShowContinueError('differs from Design Size Nominal Capacity of ' // &
                                      TRIM(RoundSigDigits(NominalCapacityDes,2))// ' [W]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            ENDIF
          END IF
        END IF
      END IF
    END IF
  END IF

  RETURN
END SUBROUTINE SizeElectricBaseboard

SUBROUTINE SimElectricConvective(BaseboardNum, LoadMet)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   Nov 2001
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This subroutine calculates the heat exchange rate
          ! in a pure Electricconvective baseboard heater.

          ! METHODOLOGY EMPLOYED:
          ! Currently this is primarily modified from HW Convective baseboard which has connections to
          !  a water loop and was necessary to calculate temps, flow rates and other things.  This
          !  model might be made more sophisticated and might use some of those data structures in the future
          !  so they are left in place even though this model does not utilize them.

          ! REFERENCES:

          ! USE STATEMENTS:
!unused0909    USE DataEnvironment, ONLY: OutBaroPress
    USE DataLoopNode,   ONLY: Node
    USE Psychrometrics, ONLY: PsyCpAirFnWTdb
    USE DataHVACGlobals, ONLY: SmallLoad

    IMPLICIT NONE
          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN)      :: BaseboardNum
    REAL(r64), INTENT(IN)    :: LoadMet

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: AirInletTemp
    REAL(r64) :: CpAir
    REAL(r64) :: AirMassFlowRate
    REAL(r64) :: CapacitanceAir
    REAL(r64) :: Effic
    REAL(r64) :: AirOutletTemp
    REAL(r64) :: QBBCap


    AirInletTemp = Baseboard(BaseboardNum)%AirInletTemp
    AirOutletTemp = AirInletTemp
    CpAir = PsyCpAirFnWTdb(Baseboard(BaseboardNum)%AirInletHumRat,AirInletTemp)
    AirMassFlowRate = SimpConvAirFlowSpeed
    CapacitanceAir = CpAir * AirMassFlowRate
    ! currently only the efficiency is used to calculate the electric consumption.  There could be some
    !  thermal loss that could be accounted for with this efficiency input.
    Effic = Baseboard(BaseboardNum)%BaseBoardEfficiency


    IF (GetCurrentScheduleValue(Baseboard(BaseboardNum)%SchedPtr) .GT. 0.0 .and.  &
        LoadMet >= SmallLoad) THEN

      ! if the load exceeds the capacity than the capacity is set to the BB limit.
      IF(LoadMet > Baseboard(BaseboardNum)%NominalCapacity) Then
        QBBCap = Baseboard(BaseboardNum)%NominalCapacity
      Else
        QBBCap = LoadMet
      End IF

      ! this could be utilized somehow or even reported so the data structures are left in place
      AirOutletTemp=AirInletTemp + QBBCap/CapacitanceAir

     !The BaseBoard electric Load is calculated using the efficiency
      Baseboard(BaseboardNum)%ElecUseRate = QBBCap/Effic

    ELSE
       !if there is an off condition the BB does nothing.
      AirOutletTemp = AirInletTemp
      QBBCap = 0.0d0
      Baseboard(BaseboardNum)%ElecUseRate = 0.0d0
    END IF

    Baseboard(BaseboardNum)%AirOutletTemp = AirOutletTemp
    Baseboard(BaseboardNum)%Power = QBBCap

    RETURN
  END SUBROUTINE SimElectricConvective


  SUBROUTINE ReportBaseboard(BaseboardNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   Nov 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This subroutine

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    Use DataHVACGlobals, ONLY: TimeStepSys

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: BaseboardNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

    Baseboard(BaseboardNum)%Energy = Baseboard(BaseboardNum)%Power * TimeStepSys*SecInHour
    Baseboard(BaseboardNum)%ElecUseLoad = Baseboard(BaseboardNum)%ElecUseRate * TimeStepSys*SecInHour

    RETURN
  END SUBROUTINE ReportBaseboard

END MODULE BaseboardElectric

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

