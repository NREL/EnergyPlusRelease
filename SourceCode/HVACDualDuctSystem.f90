Module DualDuct
  ! Module containing the DualDuct simulation routines

  ! MODULE INFORMATION:
  !       AUTHOR         Richard J. Liesen
  !       DATE WRITTEN   February 2000
  !       MODIFIED       Clayton Miller, Brent Griffith Aug. 2010 - Added DualDuctOA Terminal Unit to Simulate Decoupled OA/RA
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required to
  ! manage the DualDuct Systems Simulation

  ! METHODOLOGY EMPLOYED:
  ! Needs description, as appropriate.

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals,     ONLY: BeginEnvrnFlag, MaxNameLength, NumOfZones, &
                           InitConvTemp, SysSizingCalc, ScheduleAlwaysOn
USE DataInterfaces,  ONLY: ShowWarningError, ShowFatalError, ShowSevereError, SetupOutputVariable, &
                           ShowContinueError
Use DataEnvironment, ONLY: StdRhoAir
USE DataHVACGlobals, ONLY: SmallMassFlow, SmallAirVolFlow
USE DataSizing

  ! Use statements for access to subroutines in other modules
USE ScheduleManager

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS
INTEGER, PARAMETER :: DualDuct_ConstantVolume=1
INTEGER, PARAMETER :: DualDuct_VariableVolume=2
INTEGER, PARAMETER :: DualDuct_OutdoorAir    =3
CHARACTER(len=*), PARAMETER :: cCMO_DDConstantVolume='AirTerminal:DualDuct:ConstantVolume'
CHARACTER(len=*), PARAMETER :: cCMO_DDVariableVolume='AirTerminal:DualDuct:VAV'
CHARACTER(len=*), PARAMETER :: cCMO_DDVarVolOA      ='AirTerminal:DualDuct:VAV:OutdoorAir'

INTEGER, PARAMETER :: DD_OA_ConstantOAMode = 11
INTEGER, PARAMETER :: DD_OA_ScheduleOAMode = 12
INTEGER, PARAMETER :: DD_OA_DynamicOAMode  = 13

INTEGER, PARAMETER :: PerPersonModeNotSet         = 20
INTEGER, PARAMETER :: PerPersonDCVByCurrentLevel  = 21
INTEGER, PARAMETER :: PerPersonByDesignLevel      = 22

  ! DERIVED TYPE DEFINITIONS
TYPE DamperDesignParams
  CHARACTER(len=MaxNameLength) :: DamperName  = ' ' ! Name of the Damper
!  CHARACTER(len=MaxNameLength) :: DamperType  = ' ' ! Type of Damper ie. VAV, Mixing, Inducing, etc.
  INTEGER      :: DamperType  = 0 ! Type of Damper ie. VAV, Mixing, Inducing, etc.
  CHARACTER(len=MaxNameLength) :: Schedule    = ' ' ! Damper Operation Schedule
  INTEGER      :: SchedPtr                    = 0 ! Pointer to the correct schedule
  REAL(r64)    :: MaxAirVolFlowRate           = 0.0d0 ! Max Specified Volume Flow Rate of Damper [m3/sec]
  REAL(r64)    :: MaxAirMassFlowRate           = 0.0d0 ! Max Specified MAss Flow Rate of Damper [kg/s]
  INTEGER      :: InletNodeNum                = 0
  INTEGER      :: HotAirInletNodeNum          = 0
  INTEGER      :: ColdAirInletNodeNum         = 0
  INTEGER      :: OutletNodeNum               = 0
  REAL(r64)    :: ZoneMinAirFrac              = 0.0d0
  REAL(r64)    :: ColdAirDamperPosition       = 0.0d0
  REAL(r64)    :: HotAirDamperPosition        = 0.0d0

  INTEGER      :: OAInletNodeNum              = 0    ! Alternate Node for VAV:OutdoorAir for Outdoor Air
  INTEGER      :: RecircAirInletNodeNum       = 0    ! Alternate Node for VAV:OutdoorAir for Recirc Air
  LOGICAL      :: RecircIsUsed               = .TRUE. ! if true. then not using recirc duct, which is okay
  REAL(r64)    :: DesignOAFlowRate            = 0.d0 ! Terminal Outdoor Air Design Flow Rate for VAV:OutdoorAir, m3/s
  REAL(r64)    :: DesignRecircFlowRate        = 0.d0 ! Terminal Recirc Air Design Flow Rate for VAV:OutdoorAir, m3/s
  INTEGER      :: OAControlMode               = 0    ! Choice of scheduled, constant, or dynamic for VAV:OutdoorAir
  REAL(r64)    :: RecircAirDamperPosition     = 0.d0 ! Alternate Damper Pos Output for VAV:OutdoorAir for Recirc Air
  REAL(r64)    :: OADamperPosition            = 0.d0 ! Alternate Damper Pos Output for VAV:OutdoorAir for Recirc Air
  REAL(r64)    :: OAFraction                  = 0.d0 ! Outdoor Air Fraction for VAV:OutdoorAir

  INTEGER      :: ADUNum                      = 0   ! index of corresponding air distribution unit

  INTEGER      :: CtrlZoneNum                   = 0     ! Pointer to CtrlZone data structure
  INTEGER      :: ActualZoneNum                 = 0     ! Pointer to Zone data Structure

  REAL(r64)    :: OutdoorAirFlowRate            = 0.0D0 ! report variable for TU outdoor air flow rate
  LOGICAL      :: NoOAFlowInputFromUser         = .TRUE. ! avoids OA calculation if no input specified by user

  INTEGER      :: OARequirementsPtr = 0     !- Index to DesignSpecification:OutdoorAir object
  INTEGER      :: OAPerPersonMode   = PerPersonModeNotSet ! mode for how per person rates are determined, DCV or design.
  REAL(r64)    :: OAPerPersonByDesignLevel = 0.D0 ! store sum of people and per person rate, constant, m3/s

  INTEGER      :: AirLoopNum       = 0

END TYPE DamperDesignParams

TYPE DamperFlowConditions
  REAL(r64) :: AirMassFlowRate          = 0.0d0 ! MassFlow through the Damper being Simulated [kg/Sec]
  REAL(r64) :: AirMassFlowRateMaxAvail  = 0.0d0 ! MassFlow through the Damper being Simulated [kg/Sec]
  REAL(r64) :: AirMassFlowRateMinAvail  = 0.0d0 ! MassFlow through the Damper being Simulated [kg/Sec]
  REAL(r64) :: AirmassFlowRateMax       = 0.0d0 ! Max Mass Flow Rate or the Design Mass Flow Rate
  REAL(r64) :: AirTemp                  = 0.0d0
  REAL(r64) :: AirHumRat                = 0.0d0
  REAL(r64) :: AirEnthalpy              = 0.0d0
  REAL(r64) :: AirMassFlowRateHist1     = 0.d0 ! flow history back 1 iteration kg/s
  REAL(r64) :: AirMassFlowRateHist2     = 0.d0 ! flow history back 2 iteration kg/s
  REAL(r64) :: AirMassFlowRateHist3     = 0.d0 ! flow history back 3 iteration kg/s
  REAL(r64) :: AirMassFlowDiffMag       = 0.d0 ! flow difference scale, kg/s
END TYPE DamperFlowConditions

  !MODULE VARIABLE DECLARATIONS:
  TYPE (DamperDesignParams), ALLOCATABLE, DIMENSION(:)   :: Damper
  TYPE (DamperFlowConditions), ALLOCATABLE, DIMENSION(:) :: DamperInlet
  TYPE (DamperFlowConditions), ALLOCATABLE, DIMENSION(:) :: DamperHotAirInlet
  TYPE (DamperFlowConditions), ALLOCATABLE, DIMENSION(:) :: DamperColdAirInlet
  TYPE (DamperFlowConditions), ALLOCATABLE, DIMENSION(:) :: DamperOutlet
  TYPE (DamperFlowConditions), ALLOCATABLE, DIMENSION(:) :: DamperOAInlet       ! VAV:OutdoorAir Outdoor Air Inlet
  TYPE (DamperFlowConditions), ALLOCATABLE, DIMENSION(:) :: DamperRecircAirInlet ! VAV:OutdoorAir Recirculated Air Inlet
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

  INTEGER :: NumDampers     ! The Number of Dampers found in the Input
  INTEGER :: NumDualDuctConstVolDampers
  INTEGER :: NumDualDuctVarVolDampers
  INTEGER :: NumDualDuctVarVolOA
  REAL(r64)    :: MassFlowSetToler
  LOGICAL :: GetDualDuctInputFlag = .True.  ! Flag set to make sure you get input once

! Subroutine Specifications for the Module
          ! Driver/Manager Routines
Public  SimulateDualDuct

          ! Get Input routines for module
PRIVATE GetDualDuctInput

          ! Initialization routines for module
PRIVATE InitDualDuct
PRIVATE SizeDualDuct

          ! Algorithms for the module
Private SimDualDuctConstVol
Private SimDualDuctVarVol
Private SimDualDuctVAVOutdoorAir
Private CalcOAMassFlow
PRIVATE CalcOAOnlyMassFlow

          ! Update routine to check convergence and update nodes
Private UpdateDualDuct

          ! Reporting routines for module
Private ReportDualDuct
Public  ReportDualDuctConnections

PUBLIC  GetDualDuctOutdoorAirRecircUse

CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE SimulateDualDuct(CompName,FirstHVACIteration, ZoneNum, ZoneNodeNum,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   February 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages Damper component simulation.
          ! It is called from the SimAirLoopComponent
          ! at the system time step.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList, MakeUPPERcase
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompName
  LOGICAL,      INTENT (IN):: FirstHVACIteration
  INTEGER,      INTENT (IN):: ZoneNum
  INTEGER,      INTENT (IN):: ZoneNodeNum
  INTEGER,      INTENT (INOUT):: CompIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: DamperNum     ! The Damper that you are currently loading input into

          ! FLOW:

  ! Obtains and Allocates Damper related parameters from input file
  IF (GetDualDuctInputFlag) THEN  !First time subroutine has been entered
    CALL GetDualDuctInput
    GetDualDuctInputFlag=.false.
  End If


  ! Find the correct DamperNumber with the AirLoop & CompNum from AirLoop Derived Type
  IF (CompIndex == 0) THEN
    DamperNum = FindItemInList(CompName,Damper%DamperName,NumDampers)
    IF (DamperNum == 0) THEN
      CALL ShowFatalError('SimulateDualDuct: Damper not found='//TRIM(CompName))
    ENDIF
    CompIndex=DamperNum
  ELSE
    DamperNum=CompIndex
    IF (DamperNum > NumDampers .or. DamperNum < 1) THEN
      CALL ShowFatalError('SimulateDualDuct: Invalid CompIndex passed='//TRIM(TrimSigDigits(CompIndex))// &
                          ', Number of Dampers='//TRIM(TrimSigDigits(NumDampers))//', Damper name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(DamperNum)) THEN
      IF (CompName /= Damper(DamperNum)%DamperName) THEN
        CALL ShowFatalError('SimulateDualDuct: Invalid CompIndex passed='//TRIM(TrimSigDigits(CompIndex))// &
                            ', Damper name='//TRIM(CompName)//', stored Damper Name for that index='//              &
                            TRIM(Damper(DamperNum)%DamperName))
      ENDIF
      CheckEquipName(DamperNum)=.false.
    ENDIF
  ENDIF

  IF (CompIndex > 0) THEN
    ! With the correct DamperNum Initialize
    CALL InitDualDuct(DamperNum,FirstHVACIteration)  ! Initialize all Damper related parameters

    ! Calculate the Correct Damper Model with the current DamperNum
    SELECT CASE(Damper(DamperNum)%DamperType)

      CASE (DualDuct_ConstantVolume) ! 'AirTerminal:DualDuct:ConstantVolume'

        Call SimDualDuctConstVol(DamperNum, ZoneNum, ZoneNodeNum)

      CASE (DualDuct_VariableVolume) ! 'AirTerminal:DualDuct:VAV'

        Call SimDualDuctVarVol(DamperNum, ZoneNum, ZoneNodeNum)

      CASE (DualDuct_OutdoorAir)

        Call SimDualDuctVAVOutdoorAir(DamperNum, ZoneNum, ZoneNodeNum)  ! 'AirTerminal:DualDuct:VAV:OutdoorAir'

    END SELECT

    ! Update the current Damper to the outlet nodes
    Call UpdateDualDuct(DamperNum)

    ! Report the current Damper
    Call ReportDualDuct(DamperNum)
  ELSE
    CALL ShowFatalError('SimulateDualDuct: Damper not found='//TRIM(CompName))
  ENDIF

  RETURN

END SUBROUTINE SimulateDualDuct


! Get Input Section of the Module
!******************************************************************************
SUBROUTINE GetDualDuctInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   April 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the main routine to call other input routines and Get routines

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString, FindItemInList
    USE NodeInputManager, ONLY: GetOnlySingleNode
    USE DataZoneEquipment, ONLY: ZoneEquipConfig
    USE BranchNodeConnections, ONLY: TestCompSet
    USE DataDefineEquip,   ONLY: AirDistUnit, NumAirDistUnits
    USE DataIPShortCuts
    USE DataHeatBalance
    USE General,          ONLY : RoundSigDigits
    USE ReportSizingManager, ONLY: ReportSizingOutput

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER    :: RoutineName='GetDualDuctInput: ' ! include trailing bla

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    INTEGER :: DamperNum      ! The Damper that you are currently loading input into
    INTEGER :: DamperIndex    ! Loop index to Damper that you are currently loading input into
    INTEGER :: NumAlphas
    INTEGER :: NumNums
    INTEGER :: IOSTAT
    INTEGER :: ZoneNum        ! Index to actual zone number
    REAL(r64), DIMENSION(2) :: NumArray = 0.0d0
    CHARACTER(len=MaxNameLength), DIMENSION(7) :: AlphArray = ' '
    CHARACTER(len=MaxNameLength), DIMENSION(7) :: cAlphaFields = ' '     ! Alpha field names
    CHARACTER(len=MaxNameLength), DIMENSION(2) :: cNumericFields = ' '   ! Numeric field names
    LOGICAL, DIMENSION(7)   :: lAlphaBlanks = .TRUE.      ! Logical array, alpha field input BLANK = .true.
    LOGICAL, DIMENSION(2)   :: lNumericBlanks = .TRUE.    ! Logical array, numeric field input BLANK = .true.
    CHARACTER (len=MaxNameLength)  :: CurrentModuleObject     ! for ease in getting objects
    LOGICAL :: ErrorsFound = .false. ! If errors detected in input
    LOGICAL :: IsNotOK               ! Flag to verify name
    LOGICAL :: IsBlank               ! Flag for blank name
    INTEGER :: CtrlZone   ! controlled zone do loop index
    INTEGER :: SupAirIn   ! controlled zone supply air inlet index
    INTEGER :: ADUNum     ! loop control to search Air Distribution Units
    REAL(r64) :: DummyOAFlow = 0.0d0

          ! Flow
    NumDualDuctConstVolDampers = GetNumObjectsFound(cCMO_DDConstantVolume)
    NumDualDuctVarVolDampers = GetNumObjectsFound(cCMO_DDVariableVolume)
    NumDualDuctVarVolOA = GetNumObjectsFound( cCMO_DDVarVolOA )
    NumDampers = NumDualDuctConstVolDampers + NumDualDuctVarVolDampers + NumDualDuctVarVolOA
    ALLOCATE(Damper(NumDampers))
    ALLOCATE(CheckEquipName(NumDampers))
    CheckEquipName=.true.

    ALLOCATE(DamperInlet(NumDampers))
    ALLOCATE(DamperHotAirInlet(NumDampers))
    ALLOCATE(DamperColdAirInlet(NumDampers))
    ALLOCATE(DamperOutlet(NumDampers))

    ALLOCATE(DamperOAInlet(NumDampers))
    ALLOCATE(DamperRecircAirInlet(NumDampers))


    IF (NumDualDuctConstVolDampers .GT. 0) THEN
      DO DamperIndex = 1,  NumDualDuctConstVolDampers

        CurrentModuleObject=cCMO_DDConstantVolume

        CALL GetObjectItem(CurrentModuleObject,DamperIndex,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        DamperNum = DamperIndex
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(AlphArray(1),Damper%DamperName,DamperNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) AlphArray(1)='xxxxx'
        ENDIF
        Damper(DamperNum)%DamperName     = AlphArray(1)
        Damper(DamperNum)%DamperType     = DualDuct_ConstantVolume
        Damper(DamperNum)%Schedule       = AlphArray(2)
        IF (lAlphaBlanks(2)) THEN
          Damper(DamperNum)%SchedPtr       = ScheduleAlwaysOn
        ELSE
          Damper(DamperNum)%SchedPtr       = GetScheduleIndex(AlphArray(2))
          IF (Damper(DamperNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(Damper(DamperNum)%DamperName)//&
                                 '" '//TRIM(cAlphaFields(2))//' = '//TRIM(AlphArray(2))//' not found.')
            ErrorsFound=.true.
          ENDIF
        ENDIF
        Damper(DamperNum)%OutletNodeNum = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent,cAlphaFields(3))
        Damper(DamperNum)%HotAirInletNodeNum = &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent,cAlphaFields(4))
        Damper(DamperNum)%ColdAirInletNodeNum   = &
               GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent,cAlphaFields(5))

        Damper(DamperNum)%MaxAirVolFlowRate = NumArray(1)
        Damper(DamperNum)%ZoneMinAirFrac = 0.0d0

        ! Register component set data - one for heat and one for cool
        CALL TestCompSet(TRIM(CurrentModuleObject)//':HEAT',Damper(DamperNum)%DamperName, &
                         AlphArray(4),AlphArray(3),'Air Nodes')
        CALL TestCompSet(TRIM(CurrentModuleObject)//':COOL',Damper(DamperNum)%DamperName, &
                         AlphArray(5),AlphArray(3),'Air Nodes')

        ! Fill the Zone Equipment data with the inlet node numbers of this unit.
        DO CtrlZone = 1,NumOfZones
          IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
          DO SupAirIn = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
            IF (Damper(DamperNum)%OutletNodeNum .EQ. ZoneEquipConfig(CtrlZone)%InletNode(SupAirIn)) THEN
              IF (ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode > 0) THEN
                CALL ShowSevereError('Error in connecting a terminal unit to a zone')
                CALL ShowContinueError(TRIM(NodeID(Damper(DamperNum)%OutletNodeNum))//' already connects to another zone')
                CALL ShowContinueError('Occurs for terminal unit '//TRIM(CurrentModuleObject)//' = '//  &
                   TRIM(Damper(DamperNum)%DamperName))
                CALL ShowContinueError('Check terminal unit node names for errors')
                ErrorsFound = .true.
              ELSE
                ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%InNode = Damper(DamperNum)%ColdAirInletNodeNum
                ZoneEquipConfig(CtrlZone)%AirDistUnitHeat(SupAirIn)%InNode = Damper(DamperNum)%HotAirInletNodeNum
                ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode = Damper(DamperNum)%OutletNodeNum
                ZoneEquipConfig(CtrlZone)%AirDistUnitHeat(SupAirIn)%OutNode = Damper(DamperNum)%OutletNodeNum
              END IF
            END IF
          END DO
        END DO

        !Setup the Average damper Position output variable
        ! CurrentModuleObject='AirTerminal:DualDuct:ConstantVolume'
        CALL SetupOutputVariable('Zone Air Terminal Cold Supply Duct Damper Position []', Damper(DamperNum)%ColdAirDamperPosition, &
                              'System','Average',Damper(DamperNum)%DamperName)
        CALL SetupOutputVariable('Zone Air Terminal Hot Supply Duct Damper Position []', Damper(DamperNum)%HotAirDamperPosition, &
                              'System','Average',Damper(DamperNum)%DamperName)

      END DO   ! end Number of Damper Loop
    END IF

    IF (NumDualDuctVarVolDampers .GT. 0) THEN
      DO DamperIndex = 1,  NumDualDuctVarVolDampers

        CurrentModuleObject=cCMO_DDVariableVolume

        CALL GetObjectItem(CurrentModuleObject,DamperIndex,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        DamperNum = DamperIndex + NumDualDuctConstVolDampers
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(AlphArray(1),Damper%DamperName,DamperNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) AlphArray(1)='xxxxx'
        ENDIF
        Damper(DamperNum)%DamperName     = AlphArray(1)
        Damper(DamperNum)%DamperType     = DualDuct_VariableVolume
        Damper(DamperNum)%Schedule       = AlphArray(2)
        IF (lAlphaBlanks(2)) THEN
          Damper(DamperNum)%SchedPtr       = ScheduleAlwaysOn
        ELSE
          Damper(DamperNum)%SchedPtr       = GetScheduleIndex(AlphArray(2))
          IF (Damper(DamperNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(Damper(DamperNum)%DamperName)//&
                                 '" '//TRIM(cAlphaFields(2))//' = '//TRIM(AlphArray(2))//' not found.')
            ErrorsFound=.true.
          ENDIF
        ENDIF
        Damper(DamperNum)%OutletNodeNum = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent,cAlphaFields(3))
        Damper(DamperNum)%HotAirInletNodeNum = &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent,cAlphaFields(4))
        Damper(DamperNum)%ColdAirInletNodeNum   = &
               GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent,cAlphaFields(5))

        Damper(DamperNum)%MaxAirVolFlowRate = NumArray(1)
        Damper(DamperNum)%ZoneMinAirFrac = NumArray(2)

        ! Register component set data - one for heat and one for cool
        CALL TestCompSet(TRIM(CurrentModuleObject)//':HEAT',Damper(DamperNum)%DamperName, &
                         AlphArray(4),AlphArray(3),'Air Nodes')
        CALL TestCompSet(TRIM(CurrentModuleObject)//':COOL',Damper(DamperNum)%DamperName, &
                         AlphArray(5),AlphArray(3),'Air Nodes')

        ! Fill the Zone Equipment data with the inlet node numbers of this unit.
        DO CtrlZone = 1,NumOfZones
          IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
          DO SupAirIn = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
            IF (Damper(DamperNum)%OutletNodeNum .EQ. ZoneEquipConfig(CtrlZone)%InletNode(SupAirIn)) THEN
              ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%InNode = Damper(DamperNum)%ColdAirInletNodeNum
              ZoneEquipConfig(CtrlZone)%AirDistUnitHeat(SupAirIn)%InNode = Damper(DamperNum)%HotAirInletNodeNum
              ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode = Damper(DamperNum)%OutletNodeNum
              ZoneEquipConfig(CtrlZone)%AirDistUnitHeat(SupAirIn)%OutNode = Damper(DamperNum)%OutletNodeNum

              Damper(DamperNum)%CtrlZoneNum   = CtrlZone
              Damper(DamperNum)%ActualZoneNum = ZoneEquipConfig(CtrlZone)%ActualZoneNum

            END IF
          END DO
        END DO

        IF(.NOT. lAlphaBlanks(6))THEN
          Damper(DamperNum)%OARequirementsPtr = FindItemInList(AlphArray(6),OARequirements%Name,NumOARequirements)
          IF(Damper(DamperNum)%OARequirementsPtr .EQ. 0)THEN
            CALL ShowSevereError(TRIM(cAlphaFields(6))//' = '//TRIM(AlphArray(6))//' not found.')
            CALL ShowContinueError('Occurs in '//cCMO_DDVariableVolume//' = '//TRIM(Damper(DamperNum)%DamperName))
            ErrorsFound=.true.
          ELSE
            Damper(DamperNum)%NoOAFlowInputFromUser = .FALSE.
          END IF
        END IF

        !Setup the Average damper Position output variable
        ! CurrentModuleObject='AirTerminal:DualDuct:VAV'
        CALL SetupOutputVariable('Zone Air Terminal Cold Supply Duct Damper Position []', Damper(DamperNum)%ColdAirDamperPosition, &
                              'System','Average',Damper(DamperNum)%DamperName)
        CALL SetupOutputVariable('Zone Air Terminal Hot Supply Duct Damper Position []', Damper(DamperNum)%HotAirDamperPosition, &
                              'System','Average',Damper(DamperNum)%DamperName)
        CALL SetupOutputVariable('Zone Air Terminal Outdoor Air Volume Flow Rate [m3/s]', Damper(DamperNum)%OutdoorAirFlowRate, &
                              'System','Average',Damper(DamperNum)%DamperName)
      END DO   ! end Number of Damper Loop
    END IF

    IF (NumDualDuctVarVolOA > 0) THEN
      DO DamperIndex = 1,  NumDualDuctVarVolOA

        CurrentModuleObject=cCMO_DDVarVolOA

        CALL GetObjectItem(CurrentModuleObject,DamperIndex,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        DamperNum = DamperIndex + NumDualDuctConstVolDampers + NumDualDuctVarVolDampers
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(AlphArray(1),Damper%DamperName,DamperNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) AlphArray(1)='xxxxx'
        ENDIF
        Damper(DamperNum)%DamperName     = AlphArray(1)
        Damper(DamperNum)%DamperType     = DualDuct_OutdoorAir
        Damper(DamperNum)%Schedule       = AlphArray(2)
        IF (lAlphaBlanks(2)) THEN
          Damper(DamperNum)%SchedPtr       = ScheduleAlwaysOn
        ELSE
          Damper(DamperNum)%SchedPtr       = GetScheduleIndex(AlphArray(2))
          IF (Damper(DamperNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(Damper(DamperNum)%DamperName)//&
                                 '" '//TRIM(cAlphaFields(2))//' = '//TRIM(AlphArray(2))//' not found.')
            ErrorsFound=.true.
          ENDIF
        ENDIF
        Damper(DamperNum)%OutletNodeNum = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent,cAlphaFields(3))
        Damper(DamperNum)%OAInletNodeNum = &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent,cAlphaFields(4))

        IF (.NOT. lAlphaBlanks(5)) THEN
          Damper(DamperNum)%RecircAirInletNodeNum   = &
                 GetOnlySingleNode(AlphArray(5),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                              NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent,cAlphaFields(5))
        ELSE
          ! for this model, we intentionally allow not using the recirc side
          Damper(DamperNum)%RecircIsUsed = .FALSE.
        ENDIF

        Damper(DamperNum)%MaxAirVolFlowRate = NumArray(1)
        Damper(DamperNum)%MaxAirMassFlowRate = Damper(DamperNum)%MaxAirVolFlowRate * StdRhoAir

        ! Register component set data - one for OA and one for RA
        CALL TestCompSet(TRIM(CurrentModuleObject)//':OutdoorAir',Damper(DamperNum)%DamperName, &
                         AlphArray(4),AlphArray(3),'Air Nodes')
        IF (Damper(DamperNum)%RecircIsUsed) THEN
          CALL TestCompSet(TRIM(CurrentModuleObject)//':RecirculatedAir',Damper(DamperNum)%DamperName, &
                         AlphArray(5),AlphArray(3),'Air Nodes')
        ENDIF

        SELECT CASE (TRIM(AlphArray(7)) )
        CASE ('CURRENTOCCUPANCY')
          Damper(DamperNum)%OAPerPersonMode = PerPersonDCVByCurrentLevel

        CASE ('DESIGNOCCUPANCY')
          Damper(DamperNum)%OAPerPersonMode = PerPersonByDesignLevel
        END SELECT
        ! checks on this are done later

        ! Fill the Zone Equipment data with the inlet node numbers of this unit.
        DO CtrlZone = 1,NumOfZones
          IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
          DO SupAirIn = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
            IF (Damper(DamperNum)%OutletNodeNum .EQ. ZoneEquipConfig(CtrlZone)%InletNode(SupAirIn)) THEN
              IF (Damper(DamperNum)%RecircIsUsed) THEN
                ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%InNode = Damper(DamperNum)%RecircAirInletNodeNum
              ELSE
                ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%InNode = Damper(DamperNum)%OAInletNodeNum
              ENDIF
              ZoneEquipConfig(CtrlZone)%AirDistUnitHeat(SupAirIn)%InNode = Damper(DamperNum)%OAInletNodeNum
              ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode = Damper(DamperNum)%OutletNodeNum
              ZoneEquipConfig(CtrlZone)%AirDistUnitHeat(SupAirIn)%OutNode = Damper(DamperNum)%OutletNodeNum

              Damper(DamperNum)%CtrlZoneNum   = CtrlZone
              Damper(DamperNum)%ActualZoneNum = ZoneEquipConfig(CtrlZone)%ActualZoneNum
            END IF
          END DO
        END DO
        Damper(DamperNum)%OARequirementsPtr = FindItemInList(AlphArray(6),OARequirements%Name,NumOARequirements)
        IF(Damper(DamperNum)%OARequirementsPtr .EQ. 0)THEN
          CALL ShowSevereError(TRIM(cAlphaFields(6))//' = '//TRIM(AlphArray(6))//' not found.')
          CALL ShowContinueError('Occurs in '//cCMO_DDVarVolOA//' = '//TRIM(Damper(DamperNum)%DamperName))
          ErrorsFound=.true.
        ELSE
          Damper(DamperNum)%NoOAFlowInputFromUser = .FALSE.

          ! now fill design OA rate
          CALL CalcOAOnlyMassFlow (DamperNum, DummyOAFlow , MaxOAVolFlow = Damper(DamperNum)%DesignOAFlowRate)

          IF (Damper(DamperNum)%MaxAirVolFlowRate /= Autosize) THEN
            CALL ReportSizingOutput(CurrentModuleObject, Damper(DamperNum)%DamperName, &
                            'Maximum Outdoor Air Flow Rate [m3/s]', Damper(DamperNum)%DesignOAFlowRate)

            IF (Damper(DamperNum)%RecircIsUsed) THEN
              Damper(DamperNum)%DesignRecircFlowRate = Damper(DamperNum)%MaxAirVolFlowRate - Damper(DamperNum)%DesignOAFlowRate
              Damper(DamperNum)%DesignRecircFlowRate = MAX(0.d0, Damper(DamperNum)%DesignRecircFlowRate)
              CALL ReportSizingOutput(CurrentModuleObject, Damper(DamperNum)%DamperName, &
                            'Maximum Recirculated Air Flow Rate [m3/s]', Damper(DamperNum)%DesignRecircFlowRate)
            ELSE
              IF (Damper(DamperNum)%MaxAirVolFlowRate <  Damper(DamperNum)%DesignOAFlowRate) THEN
                CALL ShowSevereError('The value '//TRIM(RoundSigDigits(Damper(DamperNum)%MaxAirVolFlowRate, 5)) &
                                       //' in '// TRIM(cNumericFields(1)) // 'is lower than the outdoor air requirement.')
                CALL ShowContinueError('Occurs in '//cCMO_DDVarVolOA//' = '//TRIM(Damper(DamperNum)%DamperName))
                CALL ShowContinueError('The design outdoor air requirement is '//  &
                   TRIM(RoundSigDigits(Damper(DamperNum)%DesignOAFlowRate, 5)) )
                ErrorsFound = .TRUE.
              ENDIF
            ENDIF
          ENDIF
        END IF

        IF (Damper(DamperNum)%OAPerPersonMode == PerPersonModeNotSet) THEN
          DummyOAFlow = OARequirements(Damper(DamperNum)%OARequirementsPtr)%OAFlowPerPerson
          IF ((DummyOAFlow == 0.d0) .AND. (lAlphaBlanks(7))) THEN ! no worries
            ! do nothing, okay since no per person requirement involved
          ELSEIF ((DummyOAFlow > 0.d0) .AND. (lAlphaBlanks(7))) THEN ! missing input
            CALL ShowSevereError(TRIM(cAlphaFields(7))//' was blank.')
            CALL ShowContinueError('Occurs in '//cCMO_DDVarVolOA//' = '//TRIM(Damper(DamperNum)%DamperName))
            CALL ShowContinueError('Valid choices are "CurrentOccupancy" or "DesignOccupancy"')
            ErrorsFound=.true.
          ELSEIF ((DummyOAFlow > 0.d0) .AND. .NOT. (lAlphaBlanks(7))) THEN ! incorrect input
            CALL ShowSevereError(TRIM(cAlphaFields(7))//' = '//TRIM(AlphArray(7))//' not a valid key choice.')
            CALL ShowContinueError('Occurs in '//cCMO_DDVarVolOA//' = '//TRIM(Damper(DamperNum)%DamperName))
            CALL ShowContinueError('Valid choices are "CurrentOccupancy" or "DesignOccupancy"')
            ErrorsFound=.true.
          ENDIF
        ENDIF

        !Setup the Average damper Position output variable
        CALL SetupOutputVariable('Zone Air Terminal Outdoor Air Duct Damper Position []', Damper(DamperNum)%OADamperPosition , &
                              'System','Average',Damper(DamperNum)%DamperName)
        CALL SetupOutputVariable('Zone Air Terminal Recirculated Air Duct Damper Position []',   &
           Damper(DamperNum)%RecircAirDamperPosition , &
                              'System','Average',Damper(DamperNum)%DamperName)
        CALL SetupOutputVariable('Zone Air Terminal Outdoor Air Fraction []', Damper(DamperNum)%OAFraction, &
                              'System','Average',Damper(DamperNum)%DamperName)

      END DO   ! end Number of Damper Loop
    END IF

    DO DamperIndex = 1,  NumDampers
      DO ADUNum = 1,NumAirDistUnits
        IF (Damper(DamperIndex)%OutletNodeNum == AirDistUnit(ADUNum)%OutletNodeNum) THEN
!          AirDistUnit(ADUNum)%InletNodeNum = Damper(DamperIndex)%InletNodeNum
          Damper(DamperIndex)%ADUNum = ADUNum
        END IF
      END DO
      ! one assumes if there isn't one assigned, it's an error?
      IF (Damper(DamperIndex)%ADUNum == 0) THEN
        ! convenient String
        IF (Damper(DamperIndex)%DamperType == DualDuct_ConstantVolume) THEN
          CurrentModuleObject='ConstantVolume'
        ELSEIF (Damper(DamperIndex)%DamperType == DualDuct_VariableVolume) THEN
          CurrentModuleObject='VAV'
        ELSEIF (Damper(DamperIndex)%DamperType == DualDuct_OutdoorAir) THEN
          CurrentModuleObject='VAV:OutdoorAir'
        ELSE
          CurrentModuleObject='*invalid*'
        ENDIF
        CALL ShowSevereError(RoutineName//'No matching List:Zone:AirTerminal for AirTerminal:DualDuct = ['//  &
           TRIM(CurrentModuleObject)//','//TRIM(Damper(DamperIndex)%DamperName)//'].')
        CALL ShowContinueError('...should have outlet node='//TRIM(NodeID(Damper(DamperIndex)%OutletNodeNum)))
         ErrorsFound=.true.
      ENDIF
    END DO

    IF (ErrorsFound) THEN
      CALL ShowFatalError(RoutineName//'Errors found in input.  Preceding condition(s) cause termination.')
    ENDIF

  RETURN

END SUBROUTINE GetDualDuctInput

! End of Get Input subroutines for the Module
!******************************************************************************



 ! Beginning Initialization Section of the Module
!******************************************************************************

SUBROUTINE InitDualDuct(DamperNum,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   February 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for  initializations of the Damper Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Psychrometrics, ONLY:PsyRhoAirFnPbTdbW
  USE DataConvergParams, ONLY: HVACFlowRateToler
  USE DataZoneEquipment,  ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList,ZoneEquipConfig
  USE DataDefineEquip,   ONLY: AirDistUnit
  USE InputProcessor,    ONLY: SameString
  USE DataHeatBalance,   ONLY : TotPeople, People

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT (IN):: FirstHVACIteration
  Integer, Intent(IN) :: DamperNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: HotInNode
  INTEGER             :: ColdInNode
  INTEGER             :: OAInNode   !Outdoor Air Inlet Node for VAV:OutdoorAir units
  INTEGER             :: RAInNode   !Reciruclated Air Inlet Node for VAV:OutdoorAir units
  INTEGER             :: OutNode
  LOGICAL,SAVE             :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MySizeFlag
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyAirLoopFlag
  LOGICAL,SAVE        :: ZoneEquipmentListChecked = .false.  ! True after the Zone Equipment List has been checked for items
  INTEGER             :: Loop  ! Loop checking control variable
  REAL(r64)           :: PeopleFlow ! local sum variable, m3/s
          ! FLOW:

! Do the Begin Simulation initializations
  IF (MyOneTimeFlag) THEN

    ALLOCATE(MyEnvrnFlag(NumDampers))
    ALLOCATE(MySizeFlag(NumDampers))
    ALLOCATE(MyAirLoopFlag(NumDampers))
    MyAirLoopFlag = .TRUE.
    MyEnvrnFlag = .TRUE.
    MySizeFlag = .TRUE.
    MassFlowSetToler = HVACFlowRateToler * 0.00001d0

    MyOneTimeFlag = .false.

  END IF

  IF (.not. ZoneEquipmentListChecked .and. ZoneEquipInputsFilled) THEN
    ZoneEquipmentListChecked=.true.
    ! Check to see if there is a Air Distribution Unit on the Zone Equipment List
    DO Loop=1,NumDampers
      IF (Damper(Loop)%ADUNum == 0) CYCLE
      IF (CheckZoneEquipmentList('ZONEHVAC:AIRDISTRIBUTIONUNIT',AirDistUnit(Damper(Loop)%ADUNum)%Name)) CYCLE
      CALL ShowSevereError('InitDualDuct: ADU=[Air Distribution Unit,'//  &
           TRIM(AirDistUnit(Damper(Loop)%ADUNum)%Name)//  &
           '] is not on any ZoneHVAC:EquipmentList.')
      IF (Damper(Loop)%DamperType == DualDuct_ConstantVolume) THEN
        CALL ShowContinueError('...Dual Duct Damper=['//TRIM(cCMO_DDConstantVolume)//','//TRIM(Damper(Loop)%DamperName)//  &
           '] will not be simulated.')
      ELSEIF (Damper(Loop)%DamperType == DualDuct_VariableVolume) THEN
        CALL ShowContinueError('...Dual Duct Damper=['//TRIM(cCMO_DDVariableVolume)//','//TRIM(Damper(Loop)%DamperName)//  &
           '] will not be simulated.')
      ELSEIF (Damper(Loop)%DamperType == DualDuct_OutdoorAir) THEN
        CALL ShowContinueError('...Dual Duct Damper=['//TRIM(cCMO_DDVarVolOA)//','//TRIM(Damper(Loop)%DamperName)//  &
           '] will not be simulated.')
      ELSE
        CALL ShowContinueError('...Dual Duct Damper=[unknown/invalid,'//TRIM(Damper(Loop)%DamperName)//  &
           '] will not be simulated.')
      ENDIF
    ENDDO
  ENDIF

  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(DamperNum) ) THEN

    CALL SizeDualDuct(DamperNum)

    MySizeFlag(DamperNum) = .FALSE.
  END IF

  ! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(DamperNum)) THEN

    IF (Damper(DamperNum)%DamperType == DualDuct_ConstantVolume .OR. &
        Damper(DamperNum)%DamperType == DualDuct_VariableVolume) THEN
      OutNode = Damper(DamperNum)%OutletNodeNum
      HotInNode  = Damper(DamperNum)%HotAirInletNodeNum
      ColdInNode  = Damper(DamperNum)%ColdAirInletNodeNum
      Node(OutNode)%MassFlowRateMax = Damper(DamperNum)%MaxAirVolFlowRate * StdRhoAir
      IF (Damper(DamperNum)%DamperType == DualDuct_ConstantVolume) THEN
        Node(OutNode)%MassFlowRateMin = 0.0d0
      ELSE IF (Damper(DamperNum)%DamperType == DualDuct_VariableVolume) THEN
        Node(OutNode)%MassFlowRateMin = Node(OutNode)%MassFlowRateMax * Damper(DamperNum)%ZoneMinAirFrac
      ELSE
        Node(OutNode)%MassFlowRateMin = 0.0d0
      END IF
      DamperHotAirInlet(DamperNum)%AirMassFlowRateMax = Node(OutNode)%MassFlowRateMax
      DamperColdAirInlet(DamperNum)%AirMassFlowRateMax = Node(OutNode)%MassFlowRateMax
      Node(HotInNode)%MassFlowRateMax = Node(OutNode)%MassFlowRateMax
      Node(ColdInNode)%MassFlowRateMax = Node(OutNode)%MassFlowRateMax
      Node(HotInNode)%MassFlowRateMin = 0.0d0
      Node(ColdInNode)%MassFlowRateMin = 0.0d0
      MyEnvrnFlag(DamperNum) = .FALSE.

    ELSEIF (Damper(DamperNum)%DamperType == DualDuct_OutdoorAir) THEN
    ! Initialize for DualDuct:VAV:OutdoorAir
      OutNode = Damper(DamperNum)%OutletNodeNum
      OAInNode  = Damper(DamperNum)%OAInletNodeNum
      IF (Damper(DamperNum)%RecircIsUsed) RAInNode  = Damper(DamperNum)%RecircAirInletNodeNum
      Node(OutNode)%MassFlowRateMax = Damper(DamperNum)%MaxAirMassFlowRate
      Node(OutNode)%MassFlowRateMin = 0.0d0
      DamperOAInlet(DamperNum)%AirMassFlowRateMax = Damper(DamperNum)%DesignOAFlowRate * StdRhoAir
      IF (Damper(DamperNum)%RecircIsUsed) THEN
        DamperRecircAirInlet(DamperNum)%AirMassFlowRateMax = Damper(DamperNum)%MaxAirMassFlowRate &
                                          - DamperOAInlet(DamperNum)%AirMassFlowRateMax
        Node(RAInNode)%MassFlowRateMax = DamperRecircAirInlet(DamperNum)%AirMassFlowRateMax
        Node(RAInNode)%MassFlowRateMin = 0.0d0
        DamperRecircAirInlet(DamperNum)%AirMassFlowDiffMag = 1.0d-10 * DamperRecircAirInlet(DamperNum)%AirMassFlowRateMax
      ENDIF
      Node(OAInNode)%MassFlowRateMax = DamperOAInlet(DamperNum)%AirMassFlowRateMax
      Node(OAInNode)%MassFlowRateMin = 0.0d0
      !figure per person by design level for the OA duct.
      PeopleFlow = 0.d0
      DO Loop = 1, TotPeople
        IF (People(Loop)%ZonePtr /= Damper(DamperNum)%ActualZoneNum) CYCLE
        PeopleFlow = PeopleFlow + &
                     People(Loop)%NumberOfPeople * OARequirements(Damper(DamperNum)%OARequirementsPtr)%OAFlowPerPerson
      END DO
      Damper(DamperNum)%OAPerPersonByDesignLevel = PeopleFlow

      MyEnvrnFlag(DamperNum) = .FALSE.
    END IF
  END IF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(DamperNum) = .true.
  ENDIF

  ! Find air loop associated with VAV dual duct or VAV:OutdoorAir terminal units
  IF(MyAirLoopFlag(DamperNum))THEN
    IF(Damper(DamperNum)%DamperType == DualDuct_VariableVolume .OR. &
       Damper(DamperNum)%DamperType == DualDuct_OutdoorAir)THEN
      IF(Damper(DamperNum)%AirLoopNum == 0)THEN
        IF(Damper(DamperNum)%CtrlZoneNum .GT. 0)THEN
          Damper(DamperNum)%AirLoopNum = ZoneEquipConfig(Damper(DamperNum)%CtrlZoneNum)%AirLoopNum
        END IF
      ELSE
        MyAirLoopFlag(DamperNum) = .FALSE.
      END IF
    ELSE
      MyAirLoopFlag(DamperNum) = .FALSE.
    END IF
  END IF


  ! Initialize the Inlet Nodes of the Sys
  IF (Damper(DamperNum)%DamperType == DualDuct_ConstantVolume .OR. &
    Damper(DamperNum)%DamperType == DualDuct_VariableVolume) THEN
    HotInNode = Damper(DamperNum)%HotAirInletNodeNum
    ColdInNode = Damper(DamperNum)%ColdAirInletNodeNum
    OutNode = Damper(DamperNum)%OutletNodeNum
  ELSE IF (Damper(DamperNum)%DamperType == DualDuct_OutdoorAir) THEN
    OAInNode  = Damper(DamperNum)%OAInletNodeNum
    IF (Damper(DamperNum)%RecircIsUsed) RAInNode  = Damper(DamperNum)%RecircAirInletNodeNum
    OutNode = Damper(DamperNum)%OutletNodeNum
  END IF

  IF (FirstHVACIteration) THEN
!     CALL DisplayString('Init First HVAC Iteration {'//TRIM(Damper(DamperNum)%DamperName)//'}') !-For debugging - REMOVE
     !The first time through set the mass flow rate to the Max
     !Take care of the flow rates first. For Const Vol and VAV.
    IF (Damper(DamperNum)%DamperType == DualDuct_ConstantVolume .OR. &
    Damper(DamperNum)%DamperType == DualDuct_VariableVolume) THEN
      IF((Node(HotInNode)%MassFlowRate > 0.0d0) .AND.  &
         (GetCurrentScheduleValue(Damper(DamperNum)%SchedPtr) .gt. 0.0d0)) Then
        Node(HotInNode)%MassFlowRate = DamperHotAirInlet(DamperNum)%AirMassFlowRateMax
      ELSE
        Node(HotInNode)%MassFlowRate = 0.0d0
      END IF
      IF((Node(ColdInNode)%MassFlowRate > 0.0d0) .AND.  &
         (GetCurrentScheduleValue(Damper(DamperNum)%SchedPtr) .gt. 0.0d0)) Then
        Node(ColdInNode)%MassFlowRate = DamperColdAirInlet(DamperNum)%AirMassFlowRateMax
      ELSE
        Node(ColdInNode)%MassFlowRate = 0.0d0
      END IF
     !Next take care of the Max Avail Flow Rates
      If((Node(HotInNode)%MassFlowRateMaxAvail > 0.0d0) .AND.  &
         (GetCurrentScheduleValue(Damper(DamperNum)%SchedPtr) .gt. 0.0d0)) Then
        Node(HotInNode)%MassFlowRateMaxAvail = DamperHotAirInlet(DamperNum)%AirMassFlowRateMax
      Else
        Node(HotInNode)%MassFlowRateMaxAvail = 0.0d0
      END IF
      IF((Node(ColdInNode)%MassFlowRateMaxAvail > 0.0d0) .AND.  &
         (GetCurrentScheduleValue(Damper(DamperNum)%SchedPtr) .gt. 0.0d0)) Then
        Node(ColdInNode)%MassFlowRateMaxAvail = DamperColdAirInlet(DamperNum)%AirMassFlowRateMax
      ELSE
        Node(ColdInNode)%MassFlowRateMaxAvail = 0.0d0
      END IF
     !The last item is to take care of the Min Avail Flow Rates
      IF((Node(HotInNode)%MassFlowRate > 0.0d0) .AND.  &
         (GetCurrentScheduleValue(Damper(DamperNum)%SchedPtr) .gt. 0.0d0)) Then
        Node(HotInNode)%MassFlowRateMinAvail = DamperHotAirInlet(DamperNum)%AirMassFlowRateMax *  &
                                                Damper(DamperNum)%ZoneMinAirFrac
      ELSE
        Node(HotInNode)%MassFlowRateMinAvail = 0.0d0
      END IF
      IF((Node(ColdInNode)%MassFlowRate > 0.0d0) .AND.  &
         (GetCurrentScheduleValue(Damper(DamperNum)%SchedPtr) .gt. 0.0d0)) Then
        Node(ColdInNode)%MassFlowRateMinAvail = DamperColdAirInlet(DamperNum)%AirMassFlowRateMax *  &
                                                Damper(DamperNum)%ZoneMinAirFrac
      ELSE
        Node(ColdInNode)%MassFlowRateMinAvail = 0.0d0
      END IF

    ELSEIF (Damper(DamperNum)%DamperType == DualDuct_OutdoorAir) THEN
    !The first time through set the mass flow rate to the Max for VAV:OutdoorAir
      IF((Node(OAInNode)%MassFlowRate > 0.0d0) .AND.  &
         (GetCurrentScheduleValue(Damper(DamperNum)%SchedPtr) .gt. 0.0d0)) THEN
        Node(OAInNode)%MassFlowRate = DamperOAInlet(DamperNum)%AirMassFlowRateMax
      ELSE
        Node(OAInNode)%MassFlowRate = 0.0d0
      END IF
      IF (Damper(DamperNum)%RecircIsUsed) THEN
        IF((Node(RAInNode)%MassFlowRate > 0.0d0) .AND.  &
           (GetCurrentScheduleValue(Damper(DamperNum)%SchedPtr) .gt. 0.0d0)) THEN
          Node(RAInNode)%MassFlowRate = DamperRecircAirInlet(DamperNum)%AirMassFlowRateMax
        ELSE
          Node(RAInNode)%MassFlowRate = 0.0d0
        END IF
        ! clear flow history
        DamperRecircAirInlet(DamperNum)%AirMassFlowRateHist1 = 0.d0
        DamperRecircAirInlet(DamperNum)%AirMassFlowRateHist2 = 0.d0
        DamperRecircAirInlet(DamperNum)%AirMassFlowRateHist3 = 0.d0
      ENDIF
     !Next take care of the Max Avail Flow Rates
      IF((Node(OAInNode)%MassFlowRateMaxAvail > 0.0d0) .AND.  &
         (GetCurrentScheduleValue(Damper(DamperNum)%SchedPtr) .gt. 0.0d0)) THEN
        Node(OAInNode)%MassFlowRateMaxAvail = DamperOAInlet(DamperNum)%AirMassFlowRateMax
      ELSE
        Node(OAInNode)%MassFlowRateMaxAvail = 0.0d0
      END IF
      IF (Damper(DamperNum)%RecircIsUsed) THEN
        IF((Node(RAInNode)%MassFlowRateMaxAvail > 0.0d0) .AND.  &
           (GetCurrentScheduleValue(Damper(DamperNum)%SchedPtr) .gt. 0.0d0)) THEN
          Node(RAInNode)%MassFlowRateMaxAvail = DamperRecircAirInlet(DamperNum)%AirMassFlowRateMax
        ELSE
          Node(RAInNode)%MassFlowRateMaxAvail = 0.0d0
        END IF
      ENDIF
     !The last item is to take care of the Min Avail Flow Rates. VAV:OutdoorAir
      Node(OAInNode)%MassFlowRateMinAvail = 0.0d0
      IF (Damper(DamperNum)%RecircIsUsed)  Node(RAInNode)%MassFlowRateMinAvail = 0.0d0
    END IF
  END IF

  ! Initialize the Inlet Nodes of the Dampers for Const. Vol and VAV
  IF(Damper(DamperNum)%DamperType == DualDuct_ConstantVolume .OR. &
     Damper(DamperNum)%DamperType == DualDuct_VariableVolume) THEN


      DamperHotAirInlet(DamperNum)%AirMassFlowRateMaxAvail = MIN(Node(OutNode)%MassFlowRateMax, &
                                                 Node(HotInNode)%MassFlowRateMaxAvail)
      DamperHotAirInlet(DamperNum)%AirMassFlowRateMinAvail = MIN(MAX(Node(OutNode)%MassFlowRateMin, &
                                                 Node(HotInNode)%MassFlowRateMinAvail), &
                                                 Node(HotInNode)%MassFlowRateMaxAvail)

      DamperColdAirInlet(DamperNum)%AirMassFlowRateMaxAvail = MIN(Node(OutNode)%MassFlowRateMax, &
                                                 Node(ColdInNode)%MassFlowRateMaxAvail)
      DamperColdAirInlet(DamperNum)%AirMassFlowRateMinAvail = MIN(MAX(Node(OutNode)%MassFlowRateMin, &
                                                 Node(ColdInNode)%MassFlowRateMinAvail), &
                                                 Node(ColdInNode)%MassFlowRateMaxAvail)

      ! Do the following initializations (every time step): This should be the info from
      ! the previous components outlets or the node data in this section.
      ! Load the node data in this section for the component simulation
      DamperHotAirInlet(DamperNum)%AirMassFlowRate = Node(HotInNode)%MassFlowRate
      DamperHotAirInlet(DamperNum)%AirTemp         = Node(HotInNode)%Temp
      DamperHotAirInlet(DamperNum)%AirHumRat       = Node(HotInNode)%HumRat
      DamperHotAirInlet(DamperNum)%AirEnthalpy     = Node(HotInNode)%Enthalpy
      DamperColdAirInlet(DamperNum)%AirMassFlowRate = Node(ColdInNode)%MassFlowRate
      DamperColdAirInlet(DamperNum)%AirTemp         = Node(ColdInNode)%Temp
      DamperColdAirInlet(DamperNum)%AirHumRat       = Node(ColdInNode)%HumRat
      DamperColdAirInlet(DamperNum)%AirEnthalpy     = Node(ColdInNode)%Enthalpy

! Initialize the Inlet Nodes of the Dampers for VAV:OutdoorAir
    ELSEIF (Damper(DamperNum)%DamperType == DualDuct_OutdoorAir) THEN
      DamperOAInlet(DamperNum)%AirMassFlowRateMaxAvail = Node(OAInNode)%MassFlowRateMaxAvail
      DamperOAInlet(DamperNum)%AirMassFlowRateMinAvail = Node(OAInNode)%MassFlowRateMinAvail

      ! Do the following initializations (every time step): This should be the info from
      ! the previous components outlets or the node data in this section.
      ! Load the node data in this section for the component simulation
      DamperOAInlet(DamperNum)%AirMassFlowRate = Node(OAInNode)%MassFlowRate
      DamperOAInlet(DamperNum)%AirTemp         = Node(OAInNode)%Temp
      DamperOAInlet(DamperNum)%AirHumRat       = Node(OAInNode)%HumRat
      DamperOAInlet(DamperNum)%AirEnthalpy     = Node(OAInNode)%Enthalpy
      IF (Damper(DamperNum)%RecircIsUsed) THEN
        DamperRecircAirInlet(DamperNum)%AirMassFlowRateMaxAvail = Node(RAInNode)%MassFlowRateMaxAvail
        DamperRecircAirInlet(DamperNum)%AirMassFlowRateMinAvail = Node(RAInNode)%MassFlowRateMinAvail
        DamperRecircAirInlet(DamperNum)%AirMassFlowRate = Node(RAInNode)%MassFlowRate
        DamperRecircAirInlet(DamperNum)%AirTemp         = Node(RAInNode)%Temp
        DamperRecircAirInlet(DamperNum)%AirHumRat       = Node(RAInNode)%HumRat
        DamperRecircAirInlet(DamperNum)%AirEnthalpy     = Node(RAInNode)%Enthalpy
      ENDIF
    END IF

  RETURN

END SUBROUTINE InitDualDuct

SUBROUTINE SizeDualDuct(DamperNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   January 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Dual Duct air terminal units for which flow rates have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone or system sizing arrays.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor
  USE ReportSizingManager, ONLY: ReportSizingOutput

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: DamperNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength) :: DamperType

  IF (Damper(DamperNum)%MaxAirVolFlowRate == AutoSize) THEN

    IF (CurZoneEqNum > 0) THEN
      IF (Damper(DamperNum)%DamperType == DualDuct_ConstantVolume) THEN
        DamperType=cCMO_DDConstantVolume
      ELSEIF (Damper(DamperNum)%DamperType == DualDuct_VariableVolume) THEN
        DamperType=cCMO_DDVariableVolume
      ELSEIF (Damper(DamperNum)%DamperType == DualDuct_OutdoorAir) THEN
        DamperType=cCMO_DDVarVolOA
      ELSE
        DamperType='Invalid/Unknown'
      ENDIF
      CALL CheckZoneSizing(DamperType, Damper(DamperNum)%DamperName)
      Damper(DamperNum)%MaxAirVolFlowRate = MAX(TermUnitFinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                                                TermUnitFinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
      IF (Damper(DamperNum)%DamperType == DualDuct_OutdoorAir) THEN
        IF ( Damper(DamperNum)%RecircIsUsed ) THEN
          Damper(DamperNum)%DesignRecircFlowRate =  MAX(TermUnitFinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                                                  TermUnitFinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
          Damper(DamperNum)%MaxAirVolFlowRate =  Damper(DamperNum)%DesignRecircFlowRate &
                                                + Damper(DamperNum)%DesignOAFlowRate
        ELSE
          Damper(DamperNum)%MaxAirVolFlowRate = Damper(DamperNum)%DesignOAFlowRate
          Damper(DamperNum)%DesignRecircFlowRate = 0.d0
        ENDIF
        Damper(DamperNum)%MaxAirMassFlowRate = Damper(DamperNum)%MaxAirVolFlowRate * StdRhoAir
      ENDIF

      IF (Damper(DamperNum)%MaxAirVolFlowRate < SmallAirVolFlow) THEN
        Damper(DamperNum)%MaxAirVolFlowRate  = 0.d0
        Damper(DamperNum)%MaxAirMassFlowRate = 0.d0
        Damper(DamperNum)%DesignOAFlowRate   = 0.d0
        Damper(DamperNum)%DesignRecircFlowRate = 0.d0
      END IF
      CALL ReportSizingOutput(DamperType, Damper(DamperNum)%DamperName, &
                        'Maximum Air Flow Rate [m3/s]', Damper(DamperNum)%MaxAirVolFlowRate)
      IF (Damper(DamperNum)%DamperType == DualDuct_OutdoorAir) THEN
        CALL ReportSizingOutput(DamperType, Damper(DamperNum)%DamperName, &
                        'Maximum Outdoor Air Flow Rate [m3/s]', Damper(DamperNum)%DesignOAFlowRate)
        IF ( Damper(DamperNum)%RecircIsUsed ) THEN
          CALL ReportSizingOutput(DamperType, Damper(DamperNum)%DamperName, &
                        'Maximum Recirculated Air Flow Rate [m3/s]', Damper(DamperNum)%DesignRecircFlowRate)
        ENDIF
      ENDIF

    END IF

  END IF

  RETURN

END SUBROUTINE SizeDualDuct

 ! End Initialization Section of the Module
!******************************************************************************


 ! Begin Algorithm Section of the Module
!******************************************************************************
SUBROUTINE SimDualDuctConstVol(DamperNum, ZoneNum, ZoneNodeNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   Jan 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the simple mixing damper.

          ! METHODOLOGY EMPLOYED:
          ! There is method to this madness.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
   USE DataZoneEnergyDemands
!unused0909   USE DataHeatBalFanSys, ONLY: Mat
   USE Psychrometrics , ONLY:PsyCpAirFnWTdb, PsyTdbFnHW
   USE DataHVACGlobals, ONLY:SmallTempDiff

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   Integer, Intent(IN) :: DamperNum
   INTEGER, INTENT(IN) :: ZoneNum
   INTEGER, INTENT (IN):: ZoneNodeNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   REAL(r64) :: MassFlow    ! [kg/sec]   Total Mass Flow Rate from Hot & Cold Inlets
   REAL(r64) :: HumRat      ! [Kg Moisture / Kg dry air]
   REAL(r64) :: Enthalpy    ! [Watts]
   REAL(r64) :: Temperature ! [C]
   REAL(r64) :: QTotLoad    ! [W]
   REAL(r64) :: QZnReq      ! [W]
   REAL(r64) :: CpAirZn
   REAL(r64) :: CpAirSysHot
   REAL(r64) :: CpAirSysCold

      ! Get the calculated load from the Heat Balance from ZoneSysEnergyDemand
   QTotLoad=ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired
      ! Need the design massflowrate for calculations
   IF (GetCurrentScheduleValue(Damper(DamperNum)%SchedPtr) .gt. 0.0d0) THEN
     MassFlow = DamperHotAirInlet(DamperNum)%AirMassFlowRateMaxAvail/2.0d0 + &
                DamperColdAirInlet(DamperNum)%AirMassFlowRateMaxAvail/2.0d0
   ELSE
     MassFlow = 0.d0
   ENDIF
      ! If there is massflow then need to provide the correct amount of total
      !  required zone energy
   If(MassFlow .GT. SmallMassFlow) Then
     CpAirZn = PsyCpAirFnWTdb(Node(ZoneNodeNum)%HumRat,Node(ZoneNodeNum)%Temp)
     QZnReq = QTotLoad + Massflow * CpAirZn * Node(ZoneNodeNum)%Temp
      ! If the enthalpy is the same for the hot and cold duct then there would be a
      !  divide by zero so for heating or cooling set the damper to one max flow
      !  or the other.
     If(ABS(DamperColdAirInlet(DamperNum)%AirTemp - DamperHotAirInlet(DamperNum)%AirTemp) > SmallTempDiff) Then
       ! CpAirSysHot = PsyCpAirFnWTdb(DamperHotAirInlet(DamperNum)%AirHumRat,DamperHotAirInlet(DamperNum)%AirTemp)
       ! CpAirSysCold= PsyCpAirFnWTdb(DamperColdAirInlet(DamperNum)%AirHumRat,DamperColdAirInlet(DamperNum)%AirTemp)
       CpAirSysHot = CpAirZn
       CpAirSysCold= CpAirZn
       !Determine the Cold Air Mass Flow Rate
       DamperColdAirInlet(DamperNum)%AirMassFlowRate = (QZnReq - MassFlow*CpAirSysHot*DamperHotAirInlet(DamperNum)%AirTemp)/ &
            (CpAirSysCold*DamperColdAirInlet(DamperNum)%AirTemp - CpAirSysHot*DamperHotAirInlet(DamperNum)%AirTemp)
     Else If((QTotLoad > 0.0d0) .And. (DamperHotAirInlet(DamperNum)%AirMassFlowRate > 0.0d0)) Then
       DamperColdAirInlet(DamperNum)%AirMassFlowRate = 0.0d0
     Else
       DamperColdAirInlet(DamperNum)%AirMassFlowRate = MassFlow
     End If
      ! Check to make sure that the calculated flow is not greater than the available flows
     IF(DamperColdAirInlet(DamperNum)%AirMassFlowRate .gt. DamperColdAirInlet(DamperNum)%AirMassFlowRateMaxAvail) Then
          DamperColdAirInlet(DamperNum)%AirMassFlowRate = DamperColdAirInlet(DamperNum)%AirMassFlowRateMaxAvail
     Else If(DamperColdAirInlet(DamperNum)%AirMassFlowRate .lt. DamperColdAirInlet(DamperNum)%AirMassFlowRateMinAvail)Then
          DamperColdAirInlet(DamperNum)%AirMassFlowRate = DamperColdAirInlet(DamperNum)%AirMassFlowRateMinAvail
     End If
      ! Using Mass Continuity to determine the other duct flow quantity
     DamperHotAirInlet(DamperNum)%AirMassFlowRate = MassFlow - DamperColdAirInlet(DamperNum)%AirMassFlowRate
     IF(DamperHotAirInlet(DamperNum)%AirMassFlowRate .gt. DamperHotAirInlet(DamperNum)%AirMassFlowRateMaxAvail) Then
          DamperHotAirInlet(DamperNum)%AirMassFlowRate = DamperHotAirInlet(DamperNum)%AirMassFlowRateMaxAvail
     Else If(DamperHotAirInlet(DamperNum)%AirMassFlowRate .lt. DamperHotAirInlet(DamperNum)%AirMassFlowRateMinAvail)Then
          DamperHotAirInlet(DamperNum)%AirMassFlowRate = DamperHotAirInlet(DamperNum)%AirMassFlowRateMinAvail
     End If
     MassFlow = DamperColdAirInlet(DamperNum)%AirMassFlowRate + DamperHotAirInlet(DamperNum)%AirMassFlowRate
   Else
   ! System is Off set massflow to 0.0
     MassFlow = 0.0d0
   End If
   If(MassFlow .GT. SmallMassFlow) Then
      ! After flows are calculated then calculate the mixed air flow properties.
     HumRat = (DamperHotAirInlet(DamperNum)%AirHumRat * &
             DamperHotAirInlet(DamperNum)%AirMassFlowRate + &
             DamperColdAirInlet(DamperNum)%AirHumRat * &
             DamperColdAirInlet(DamperNum)%AirMassFlowRate) / MassFlow
     Enthalpy = (DamperHotAirInlet(DamperNum)%AirEnthalpy * &
               DamperHotAirInlet(DamperNum)%AirMassFlowRate + &
                  DamperColdAirInlet(DamperNum)%AirEnthalpy * &
                  DamperColdAirInlet(DamperNum)%AirMassFlowRate) / MassFlow

      ! If there is no air flow than calculate the No Flow conditions
   Else
     DamperColdAirInlet(DamperNum)%AirMassFlowRate = 0.0d0
     DamperHotAirInlet(DamperNum)%AirMassFlowRate = 0.0d0
     HumRat =(DamperHotAirInlet(DamperNum)%AirHumRat + DamperColdAirInlet(DamperNum)%AirHumRat)/2.0d0
     Enthalpy = (DamperHotAirInlet(DamperNum)%AirEnthalpy + DamperColdAirInlet(DamperNum)%AirEnthalpy)/2.0d0
   End IF
     Temperature = PsyTdbFnHW(Enthalpy,HumRat)

      ! Load all properties in the damper outlet
   DamperOutlet(DamperNum)%AirTemp         = Temperature
   DamperOutlet(DamperNum)%AirHumRat       = HumRat
   DamperOutlet(DamperNum)%AirMassFlowRate = MassFlow
   DamperOutlet(DamperNum)%AirMassFlowRateMaxAvail = MassFlow
   DamperOutlet(DamperNum)%AirMassFlowRateMinAvail = MIN(DamperHotAirInlet(DamperNum)%AirMassFlowRateMinAvail, &
                                                         DamperColdAirInlet(DamperNum)%AirMassFlowRateMinAvail)
   DamperOutlet(DamperNum)%AirEnthalpy            = Enthalpy

   !Calculate the hot and cold damper position in %
   If((DamperHotAirInlet(DamperNum)%AirmassFlowRateMax == 0.0d0) .or. &
      (DamperColdAirInlet(DamperNum)%AirmassFlowRateMax == 0.0d0)) Then
     Damper(DamperNum)%ColdAirDamperPosition = 0.0d0
     Damper(DamperNum)%HotAirDamperPosition  = 0.0d0
   Else
     Damper(DamperNum)%ColdAirDamperPosition = DamperColdAirInlet(DamperNum)%AirMassFlowRate/ &
                                             DamperColdAirInlet(DamperNum)%AirmassFlowRateMax
     Damper(DamperNum)%HotAirDamperPosition  = DamperHotAirInlet(DamperNum)%AirMassFlowRate/ &
                                             DamperHotAirInlet(DamperNum)%AirmassFlowRateMax
   End If

 RETURN
END SUBROUTINE SimDualDuctConstVol

SUBROUTINE SimDualDuctVarVol(DamperNum, ZoneNum, ZoneNodeNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   Jan 2000
          !       MODIFIED       na
          !                      TH 3/2012: added supply air flow adjustment based on zone maximum outdoor
          !                                 air fraction - a TRACE feature
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the simple mixing damper.

          ! METHODOLOGY EMPLOYED:
          ! There is method to this madness.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
   USE DataZoneEnergyDemands
!unused0909   USE DataHeatBalFanSys, ONLY: Mat
   USE Psychrometrics, ONLY:PsyCpAirFnWTdb, PsyTdbFnHW
   USE DataHVACGlobals, ONLY:SmallTempDiff

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   Integer, Intent(IN) :: DamperNum
   INTEGER, INTENT(IN) :: ZoneNum
   INTEGER, INTENT (IN):: ZoneNodeNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   REAL(r64) :: MassFlow    ! [kg/sec]   Total Mass Flow Rate from Hot & Cold Inlets
   REAL(r64) :: HumRat      ! [Kg Moisture / Kg dry air]
   REAL(r64) :: Enthalpy    ! [Watts]
   REAL(r64) :: Temperature ! [C]
   REAL(r64) :: QTotLoad    ! [W]
   REAL(r64) :: QZnReq      ! [W]
   REAL(r64) :: CpAirZn     ! specific heat of zone air
   REAL(r64) :: CpAirSysHot
   REAL(r64) :: CpAirSysCold
   REAL(r64) :: MassFlowBasedOnOA ! Supply air flow rate based on minimum OA requirement
   REAL(r64) :: AirLoopOAFrac     ! fraction of outdoor air entering air loop outside air system

   ! The calculated load from the Heat Balance
   QTotLoad=ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired
   !Calculate all of the required Cp's
   CpAirZn = PsyCpAirFnWTdb(Node(ZoneNodeNum)%HumRat,Node(ZoneNodeNum)%Temp)
   ! CpAirSysHot = PsyCpAirFnWTdb(DamperHotAirInlet(DamperNum)%AirHumRat,DamperHotAirInlet(DamperNum)%AirTemp)
   ! CpAirSysCold= PsyCpAirFnWTdb(DamperColdAirInlet(DamperNum)%AirHumRat,DamperColdAirInlet(DamperNum)%AirTemp)
   CpAirSysHot = CpAirZn
   CpAirSysCold = CpAirZn

   ! calculate supply air flow rate based on user specified OA requirement
   CALL CalcOAMassFlow(DamperNum, MassFlowBasedOnOA, AirLoopOAFrac)

   !Then depending on if the Load is for heating or cooling it is handled differently.  First
   ! the massflow rate of either heating or cooling is determined to meet the entire load.  Then
   ! if the massflow is below the minimum or greater than the Max it is set to either the Min
   ! or the Max as specified for the VAV model.
   IF (GetCurrentScheduleValue(Damper(DamperNum)%SchedPtr) == 0.0d0) THEN
     ! System is Off set massflow to 0.0
     MassFlow = 0.0d0

   ELSEIf((QTotLoad > 0.0d0) .AND. (DamperHotAirInlet(DamperNum)%AirMassFlowRateMaxAvail > 0.0d0)) Then
   ! Then heating is needed
   ! Next check for the denominator equal to zero
     If(ABS((CpAirSysHot*DamperHotAirInlet(DamperNum)%AirTemp) - (CpAirZn*Node(ZoneNodeNum)%Temp))/CpAirZn > SmallTempDiff) Then
       MassFlow= QTotLoad/(CpAirSysHot*DamperHotAirInlet(DamperNum)%AirTemp - CpAirZn*Node(ZoneNodeNum)%Temp)
     Else
       ! If denominator tends to zero then mass flow would go to infinity thus set to the max for this iteration
       MassFlow = DamperHotAirInlet(DamperNum)%AirMassFlowRateMaxAvail
     End IF
     !Check to see if the flow is < the Min or > the Max air Fraction to the zone; then set to min or max
     IF(MassFlow <= (DamperHotAirInlet(DamperNum)%AirMassFlowRateMax*Damper(DamperNum)%ZoneMinAirFrac)) Then
       MassFlow = DamperHotAirInlet(DamperNum)%AirMassFlowRateMax*Damper(DamperNum)%ZoneMinAirFrac
       MassFlow = MAX(MassFlow,DamperHotAirInlet(DamperNum)%AirMassFlowRateMinAvail)
     Else If(MassFlow >= DamperHotAirInlet(DamperNum)%AirMassFlowRateMaxAvail) Then
       MassFlow = DamperHotAirInlet(DamperNum)%AirMassFlowRateMaxAvail
     End If

     ! Apply the zone maximum outdoor air fraction for VAV boxes - a TRACE feature
     IF (ZoneSysEnergyDemand(ZoneNum)%SupplyAirAdjustFactor > 1.0d0) THEN
       MassFlow = MassFlow * ZoneSysEnergyDemand(ZoneNum)%SupplyAirAdjustFactor
     ENDIF

     MassFlow = MAX(MassFlow, MassFlowBasedOnOA)
     MassFlow = MIN(MassFlow,DamperHotAirInlet(DamperNum)%AirMassFlowRateMaxAvail)

   Else If((QTotLoad < 0.0d0) .AND. (DamperColdAirInlet(DamperNum)%AirMassFlowRateMaxAvail > 0.0d0)) Then
   ! Then cooling is required
   ! Next check for the denominator equal to zero
     If(ABS((CpAirSysCold*DamperColdAirInlet(DamperNum)%AirTemp) - (CpAirZn*Node(ZoneNodeNum)%Temp))/CpAirZn > SmallTempDiff) Then
       MassFlow= QTotLoad/(CpAirSysCold*DamperColdAirInlet(DamperNum)%AirTemp - CpAirZn*Node(ZoneNodeNum)%Temp)
     Else
       ! If denominator tends to zero then mass flow would go to infinity thus set to the max for this iteration
       MassFlow = DamperColdAirInlet(DamperNum)%AirMassFlowRateMaxAvail
     End IF

     !Check to see if the flow is < the Min or > the Max air Fraction to the zone; then set to min or max
     IF((MassFlow <= (DamperColdAirInlet(DamperNum)%AirMassFlowRateMax*Damper(DamperNum)%ZoneMinAirFrac)) .and. &
         (MassFlow >= 0.0d0)) Then
       MassFlow = DamperColdAirInlet(DamperNum)%AirMassFlowRateMax*Damper(DamperNum)%ZoneMinAirFrac
       MassFlow = MAX(MassFlow,DamperColdAirInlet(DamperNum)%AirMassFlowRateMinAvail)
     Else If(MassFlow < 0.0d0) Then
       MassFlow = DamperColdAirInlet(DamperNum)%AirMassFlowRateMaxAvail
     Else If(MassFlow >= DamperColdAirInlet(DamperNum)%AirMassFlowRateMaxAvail) Then
       MassFlow = DamperColdAirInlet(DamperNum)%AirMassFlowRateMaxAvail
     End If

     ! Apply the zone maximum outdoor air fraction for VAV boxes - a TRACE feature
     IF (ZoneSysEnergyDemand(ZoneNum)%SupplyAirAdjustFactor > 1.0d0) THEN
       MassFlow = MassFlow * ZoneSysEnergyDemand(ZoneNum)%SupplyAirAdjustFactor
     ENDIF

     MassFlow = MAX(MassFlow, MassFlowBasedOnOA)
     MassFlow = MIN(MassFlow,DamperColdAirInlet(DamperNum)%AirMassFlowRateMaxAvail)

   Else If((DamperHotAirInlet(DamperNum)%AirMassFlowRateMaxAvail > 0.0d0) .or.  &
           (DamperColdAirInlet(DamperNum)%AirMassFlowRateMaxAvail > 0.0d0)) Then
   ! No Load on Zone set to mixed condition
     MassFlow = (DamperHotAirInlet(DamperNum)%AirMassFlowRateMax/2.0d0) * Damper(DamperNum)%ZoneMinAirFrac + &
                DamperColdAirInlet(DamperNum)%AirMassFlowRateMax/2.0d0 * Damper(DamperNum)%ZoneMinAirFrac

     ! Apply the zone maximum outdoor air fraction for VAV boxes - a TRACE feature
     IF (ZoneSysEnergyDemand(ZoneNum)%SupplyAirAdjustFactor > 1.0d0) THEN
       MassFlow = MassFlow * ZoneSysEnergyDemand(ZoneNum)%SupplyAirAdjustFactor
     ENDIF

     MassFlow = MAX(MassFlow, MassFlowBasedOnOA)
     MassFlow = MIN(MassFlow,&
         (DamperHotAirInlet(DamperNum)%AirMassFlowRateMaxAvail+DamperColdAirInlet(DamperNum)%AirMassFlowRateMaxAvail))

   Else
   ! System is Off set massflow to 0.0
     MassFlow = 0.0d0
   End If

   !Now the massflow for heating or cooling has been determined and if the massflow was reset to the
   ! Min or Max we will need to mix the hot and cold deck to meet the zone load.  Knowing the enthalpy
   ! of the zone and the hot and cold air flows we can determine exactly by using the Energy and Continuity
   ! Eqns.  Of course we have to make sure that we are within the Min and Max flow conditions.
   If(MassFlow .GT. SmallMassFlow) Then
     !Determine the enthalpy required from Zone enthalpy and the zone load.
     QZnReq = QTotLoad + Massflow * CpAirZn * Node(ZoneNodeNum)%Temp
     !Using the known enthalpies the cold air inlet mass flow is determined.  If the enthalpy of the hot and cold
     ! air streams are equal the IF-Then block handles that condition.
     If(ABS(DamperColdAirInlet(DamperNum)%AirTemp - DamperHotAirInlet(DamperNum)%AirTemp) > SmallTempDiff) Then
       !Calculate the Cold air mass flow rate
       DamperColdAirInlet(DamperNum)%AirMassFlowRate = (QZnReq - MassFlow*CpAirSysHot*DamperHotAirInlet(DamperNum)%AirTemp)/ &
            (CpAirSysCold*DamperColdAirInlet(DamperNum)%AirTemp - CpAirSysHot*DamperHotAirInlet(DamperNum)%AirTemp)
     Else If((QTotLoad > 0.0d0) .And. (DamperHotAirInlet(DamperNum)%AirMassFlowRate > 0.0d0)) Then
       DamperColdAirInlet(DamperNum)%AirMassFlowRate = 0.0d0
     Else
       DamperColdAirInlet(DamperNum)%AirMassFlowRate = MassFlow
     End If

     !Need to make sure that the flows are within limits
     IF(DamperColdAirInlet(DamperNum)%AirMassFlowRate .gt. DamperColdAirInlet(DamperNum)%AirMassFlowRateMaxAvail) Then
          DamperColdAirInlet(DamperNum)%AirMassFlowRate = DamperColdAirInlet(DamperNum)%AirMassFlowRateMaxAvail

     !These are shutoff boxes for either the hot or the cold, therfore one side or other can = 0.0
     Else If(DamperColdAirInlet(DamperNum)%AirMassFlowRate .lt. 0.0d0)Then
          DamperColdAirInlet(DamperNum)%AirMassFlowRate = 0.0d0
     Else If(DamperColdAirInlet(DamperNum)%AirMassFlowRate .gt. MassFlow)Then
          DamperColdAirInlet(DamperNum)%AirMassFlowRate = MassFlow
     End If
     !Using Mass Continuity to determine the other duct flow quantity
     DamperHotAirInlet(DamperNum)%AirMassFlowRate = MassFlow - DamperColdAirInlet(DamperNum)%AirMassFlowRate

     IF (DamperHotAirInlet(DamperNum)%AirMassFlowRate < MassFlowSetToler) THEN
       DamperHotAirInlet(DamperNum)%AirMassFlowRate = 0.0d0
       DamperColdAirInlet(DamperNum)%AirMassFlowRate = MassFlow
     ELSE IF (DamperColdAirInlet(DamperNum)%AirMassFlowRate < MassFlowSetToler) THEN
       DamperColdAirInlet(DamperNum)%AirMassFlowRate = 0.0d0
       DamperHotAirInlet(DamperNum)%AirMassFlowRate = MassFlow
     END IF

     !After the flow rates are determined the properties are calculated.
     HumRat = (DamperHotAirInlet(DamperNum)%AirHumRat * &
               DamperHotAirInlet(DamperNum)%AirMassFlowRate + &
               DamperColdAirInlet(DamperNum)%AirHumRat * &
               DamperColdAirInlet(DamperNum)%AirMassFlowRate) / MassFlow
     Enthalpy = (DamperHotAirInlet(DamperNum)%AirEnthalpy * &
                 DamperHotAirInlet(DamperNum)%AirMassFlowRate + &
                 DamperColdAirInlet(DamperNum)%AirEnthalpy * &
                 DamperColdAirInlet(DamperNum)%AirMassFlowRate) / MassFlow

     !IF the system is OFF the properties are calculated for this special case.
   Else
     DamperColdAirInlet(DamperNum)%AirMassFlowRate = 0.0d0
     DamperHotAirInlet(DamperNum)%AirMassFlowRate = 0.0d0
     HumRat =(DamperHotAirInlet(DamperNum)%AirHumRat + DamperColdAirInlet(DamperNum)%AirHumRat)/2.0d0
     Enthalpy = (DamperHotAirInlet(DamperNum)%AirEnthalpy + DamperColdAirInlet(DamperNum)%AirEnthalpy)/2.0d0
   End IF
     Temperature = PsyTdbFnHW(Enthalpy,HumRat)


   DamperOutlet(DamperNum)%AirTemp         = Temperature
   DamperOutlet(DamperNum)%AirHumRat       = HumRat
   DamperOutlet(DamperNum)%AirMassFlowRate = MassFlow
   DamperOutlet(DamperNum)%AirMassFlowRateMaxAvail = MassFlow
   DamperOutlet(DamperNum)%AirMassFlowRateMinAvail = Damper(DamperNum)%ZoneMinAirFrac * &
                           DamperHotAirInlet(DamperNum)%AirMassFlowRateMax
   DamperOutlet(DamperNum)%AirEnthalpy            = Enthalpy
   Damper(DamperNum)%OutdoorAirFlowRate    = MassFlow * AirLoopOAFrac

   !Calculate the hot and cold damper position in %
   If((DamperHotAirInlet(DamperNum)%AirmassFlowRateMax == 0.0d0) .or. &
      (DamperColdAirInlet(DamperNum)%AirmassFlowRateMax == 0.0d0)) Then
     Damper(DamperNum)%ColdAirDamperPosition = 0.0d0
     Damper(DamperNum)%HotAirDamperPosition  = 0.0d0
   Else
     Damper(DamperNum)%ColdAirDamperPosition = DamperColdAirInlet(DamperNum)%AirMassFlowRate/ &
                                             DamperColdAirInlet(DamperNum)%AirmassFlowRateMax
     Damper(DamperNum)%HotAirDamperPosition  = DamperHotAirInlet(DamperNum)%AirMassFlowRate/ &
                                             DamperHotAirInlet(DamperNum)%AirmassFlowRateMax
   End If

 RETURN
END SUBROUTINE SimDualDuctVarVol

SUBROUTINE SimDualDuctVAVOutdoorAir(DamperNum, ZoneNum, ZoneNodeNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Clayton Miller
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       B. Griffith, Dec 2010, major rework
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Designed to accommodate for systems with outdoor air (OA) and recirculated air (RA)
          ! as two separate air streams to controlled at the zone level in a dual duct system.

          ! METHODOLOGY EMPLOYED:
          ! The terminal unit is be designed to set the airflow of the of the OA stream at the zone
          ! level based on the zonal ventilation requirements and the RA stream flowrate of recirculated
          ! cooling air stream in order to meet the remaining thermal load.
          ! If the zone calls for cooling but the inlet air temperature is too warm, recirc side set to zero
          ! if the zone calls for heating and the inlet air is warm enough, modulate damper to meet load
          ! if the zone calls for heating and the inlet air is too cold, zero flow (will not control sans reheat)

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands
  USE Psychrometrics, ONLY:PsyCpAirFnWTdb, PsyTdbFnHW
  USE DataGlobals
  USE General,             ONLY: TrimSigDigits
  USE DataHeatBalFanSys, ONLY: ZoneThermostatSetPointLo, ZoneThermostatSetPointHi
  USE DataHVACGlobals, ONLY:SmallTempDiff

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: DamperNum
  INTEGER, INTENT(IN) :: ZoneNum
  INTEGER, INTENT (IN):: ZoneNodeNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: MassFlowMax    ! [kg/sec]   Maximum Mass Flow Rate from OA and Recirc Inlets
  REAL(r64) :: HumRat      ! [Kg Moisture / Kg dry air]
  REAL(r64) :: Enthalpy    ! [Watts]
  REAL(r64) :: Temperature ! [C]
  REAL(r64) :: QTotLoadRemain    ! [W]
  REAL(r64) :: QtoHeatSPRemain  ! [W]
  REAL(r64) :: QtoCoolSPRemain  ! [W]
!  REAL(r64) :: QTotRemainAdjust  ! [W]
  REAL(r64) :: QtoHeatSPRemainAdjust  ! [W]
  REAL(r64) :: QtoCoolSPRemainAdjust  ! [W]
  REAL(r64) :: QOALoadToHeatSP  ! [W]
  REAL(r64) :: QOALoadToCoolSP  ! [W]
  REAL(r64) :: QOALoad     ! Amount of cooling load accounted for by OA Stream [W]
  REAL(r64) :: QRALoad     ! Amount of cooling load accounted for by Recirc Stream [W]
  REAL(r64) :: CpAirZn     ! specific heat of zone air
  REAL(r64) :: CpAirSysOA  ! specific heat of outdoor air
  REAL(r64) :: CpAirSysRA  ! specific heat of recirculated air
  REAL(r64) :: OAMassFlow  ! Supply air flow rate based on minimum OA requirement - for printing
  REAL(r64) :: TotMassFlow ! [kg/sec]   Total Mass Flow Rate from OA and Recirc Inlets
  INTEGER   :: OAInletNodeNum
  INTEGER   :: RecircInletNodeNum

  OAInletNodeNum      = Damper(DamperNum)%OAInletNodeNum
  IF ( Damper(DamperNum)%RecircIsUsed ) THEN
   RecircInletNodeNum  = Damper(DamperNum)%RecircAirInletNodeNum
  ENDIF
   ! Calculate required ventilation air flow rate based on user specified OA requirement
  CALL CalcOAOnlyMassFlow(DamperNum, OAMassFlow)

   ! The calculated load from the Heat Balance, adjusted for any equipment sequenced before terminal
  QTotLoadRemain  = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired
  QtoHeatSPRemain = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP
  QtoCoolSPRemain = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToCoolSP

   !Calculate all of the required Cp's
  CpAirZn    = PsyCpAirFnWTdb(Node(ZoneNodeNum)%HumRat,       Node(ZoneNodeNum)%Temp)
  CpAirSysOA = PsyCpAirFnWTdb(Node(OAInletNodeNum)%HumRat,    Node(OAInletNodeNum)%Temp)
  IF ( Damper(DamperNum)%RecircIsUsed ) CpAirSysRA = PsyCpAirFnWTdb(Node(RecircInletNodeNum)%HumRat,Node(RecircInletNodeNum)%Temp)

   ! Set the OA Damper to the calculated ventilation flow rate
  DamperOAInlet(DamperNum)%AirMassFlowRate = OAMassFlow
     !Need to make sure that the OA flows are within limits
  IF(DamperOAInlet(DamperNum)%AirMassFlowRate .gt. DamperOAInlet(DamperNum)%AirMassFlowRateMaxAvail) THEN
    DamperOAInlet(DamperNum)%AirMassFlowRate = DamperOAInlet(DamperNum)%AirMassFlowRateMaxAvail

  ELSE IF(DamperOAInlet(DamperNum)%AirMassFlowRate .lt. 0.0d0)THEN
    DamperOAInlet(DamperNum)%AirMassFlowRate = 0.0d0
  END IF

   !..Find the amount of load that the OAMassFlow accounted for
  IF(ABS((CpAirSysOA*DamperOAInlet(DamperNum)%AirTemp) - (CpAirZn*Node(ZoneNodeNum)%Temp))/CpAirZn > SmallTempDiff) THEN
    QOALoad = DamperOAInlet(DamperNum)%AirMassFlowRate *  &
                 (CpAirSysOA * DamperOAInlet(DamperNum)%AirTemp - CpAirZn * Node(ZoneNodeNum)%Temp)

    QOALoadToHeatSP =  DamperOAInlet(DamperNum)%AirMassFlowRate *  &
                 (CpAirSysOA * DamperOAInlet(DamperNum)%AirTemp - CpAirZn * ZoneThermostatSetPointLo(ZoneNum))
    QOALoadToCoolSP =  DamperOAInlet(DamperNum)%AirMassFlowRate *  &
                 (CpAirSysOA * DamperOAInlet(DamperNum)%AirTemp - CpAirZn * ZoneThermostatSetPointHi(ZoneNum))

  ELSE
    QOALoad = 0.0d0
    QOALoadToHeatSP = 0.0d0
    QOALoadToCoolSP = 0.0d0
  END IF

  IF ( Damper(DamperNum)%RecircIsUsed ) THEN

     !correct load for recirc side to account for impact of OA side
   ! QTotRemainAdjust      = QTotLoadRemain  - QOALoad
    QtoHeatSPRemainAdjust = QtoHeatSPRemain - QOALoadToHeatSP
    QtoCoolSPRemainAdjust = QtoCoolSPRemain - QOALoadToCoolSP

    IF (QtoCoolSPRemainAdjust < 0.d0) THEN
      QRALoad = QtoCoolSPRemainAdjust
    ELSEIF (QtoHeatSPRemainAdjust > 0.d0) THEN
      QRALoad = QtoHeatSPRemainAdjust
    ELSE
      QRALoad = 0.0d0
    ENDIF


  !
  !  IF (QTotLoadRemain == 0.d0) THEN  ! floating in deadband
  !    IF ((QTotRemainAdjust < 0.d0) .AND. (QtoCoolSPRemainAdjust < 0.d0)) THEN !really need cooling
  !      QRALoad = QtoCoolSPRemainAdjust
  !    ELSEIF ((QTotRemainAdjust > 0.d0) .AND. (QtoHeatSPRemainAdjust > 0.d0)) THEN ! really need heating
  !      QRALoad = QtoHeatSPRemainAdjust
  !    ELSE
  !      QRALoad = 0.0 ! still floating in deadband even with impact of OA side
  !    ENDIF
  !  ELSE
  !    QRALoad = QTotRemainAdjust
  !  ENDIF


    IF (QRALoad < 0.d0) THEN ! cooling
      IF ((DamperRecircAirInlet(DamperNum)%AirTemp - Node(ZoneNodeNum)%Temp) < -0.5d0)  THEN ! can cool
           !  Find the Mass Flow Rate of the RA Stream needed to meet the zone cooling load
        IF(ABS((CpAirSysRA*DamperRecircAirInlet(DamperNum)%AirTemp)-(CpAirZn*Node(ZoneNodeNum)%Temp))/CpAirZn > SmallTempDiff) THEN
          DamperRecircAirInlet(DamperNum)%AirMassFlowRate = QRALoad / &
                           (CpAirSysRA*DamperRecircAirInlet(DamperNum)%AirTemp - CpAirZn*Node(ZoneNodeNum)%Temp)
        ENDIF
      ELSE
        DamperRecircAirInlet(DamperNum)%AirMassFlowRate = 0.d0
      ENDIF

    ELSEIF (QRALoad > 0.d0) THEN ! heating
  !    IF ((DamperRecircAirInlet(DamperNum)%AirTemp - Node(ZoneNodeNum)%Temp) > 2.0d0)  THEN ! can heat
  !      DamperRecircAirInlet(DamperNum)%AirMassFlowRate = QRALoad / &
  !                         (CpAirSysRA*DamperRecircAirInlet(DamperNum)%AirTemp - CpAirZn*Node(ZoneNodeNum)%Temp)
  !    ELSE
        DamperRecircAirInlet(DamperNum)%AirMassFlowRate = 0.d0
  !    ENDIF

    ELSE ! none needed.
      DamperRecircAirInlet(DamperNum)%AirMassFlowRate = 0.d0

    ENDIF

     !Need to make sure that the RA flows are within limits
    IF(DamperRecircAirInlet(DamperNum)%AirMassFlowRate .gt. DamperRecircAirInlet(DamperNum)%AirMassFlowRateMaxAvail) THEN
      DamperRecircAirInlet(DamperNum)%AirMassFlowRate = DamperRecircAirInlet(DamperNum)%AirMassFlowRateMaxAvail
     !These are shutoff boxes for either the hot or the cold, therfore one side or other can = 0.0
    ELSE IF(DamperRecircAirInlet(DamperNum)%AirMassFlowRate < 0.d0)THEN
      DamperRecircAirInlet(DamperNum)%AirMassFlowRate = 0.d0
    END IF


  ELSE
    DamperRecircAirInlet(DamperNum)%AirMassFlowRate = 0.d0
    DamperRecircAirInlet(DamperNum)%AirMassFlowRateMaxAvail = 0.d0
  ENDIF ! recirc used


   ! look for bang-bang condition: flow rate oscillating between 2 values during the air loop / zone
   ! equipment iteration. If detected, set flow rate to previous value.
   IF ( ( (ABS(DamperRecircAirInlet(DamperNum)%AirMassFlowRate  &
                 -DamperRecircAirInlet(DamperNum)%AirMassFlowRateHist2)  &
                    < DamperRecircAirInlet(DamperNum)%AirMassFlowDiffMag) .OR. &
          (ABS(DamperRecircAirInlet(DamperNum)%AirMassFlowRate  &
                 -DamperRecircAirInlet(DamperNum)%AirMassFlowRateHist3)  &
                    < DamperRecircAirInlet(DamperNum)%AirMassFlowDiffMag) ) .AND. &
          (ABS(DamperRecircAirInlet(DamperNum)%AirMassFlowRate  &
                 -DamperRecircAirInlet(DamperNum)%AirMassFlowRateHist1) &
                    >= DamperRecircAirInlet(DamperNum)%AirMassFlowDiffMag) ) THEN
     IF (DamperRecircAirInlet(DamperNum)%AirMassFlowRate > 0.0d0) THEN
       DamperRecircAirInlet(DamperNum)%AirMassFlowRate = DamperRecircAirInlet(DamperNum)%AirMassFlowRateHist1
     ENDIF
   END IF

   ! Find the Max Box Flow Rate.
  MassFlowMax = DamperOAInlet(DamperNum)%AirMassFlowRateMaxAvail + DamperRecircAirInlet(DamperNum)%AirMassFlowRateMaxAvail
  IF  (GetCurrentScheduleValue(Damper(DamperNum)%SchedPtr) .gt. 0.0d0) THEN
    TotMassFlow = DamperOAInlet(DamperNum)%AirMassFlowRate + DamperRecircAirInlet(DamperNum)%AirMassFlowRate
  ELSE
    TotMassFlow = 0.d0
  ENDIF

  IF (TotMassFlow .GT. SmallMassFlow) THEN

   ! If the sum of the two air streams' flow is greater than the Max Box Flow Rate then reset the RA Stream
    IF (TotMassFlow > MassFlowMax) THEN
      DamperRecircAirInlet(DamperNum)%AirMassFlowRate = MassFlowMax - DamperOAInlet(DamperNum)%AirMassFlowRate
    END IF
   !After the flow rates are determined the properties are calculated.
    TotMassFlow = DamperOAInlet(DamperNum)%AirMassFlowRate + DamperRecircAirInlet(DamperNum)%AirMassFlowRate
    IF (TotMassFlow .GT. SmallMassFlow) THEN
      HumRat = (DamperOAInlet(DamperNum)%AirHumRat * &
                DamperOAInlet(DamperNum)%AirMassFlowRate + &
                DamperRecircAirInlet(DamperNum)%AirHumRat * &
                DamperRecircAirInlet(DamperNum)%AirMassFlowRate) / TotMassFlow
      Enthalpy = (DamperOAInlet(DamperNum)%AirEnthalpy * &
                  DamperOAInlet(DamperNum)%AirMassFlowRate + &
                  DamperRecircAirInlet(DamperNum)%AirEnthalpy * &
                  DamperRecircAirInlet(DamperNum)%AirMassFlowRate) / TotMassFlow
    ELSE
      HumRat =(DamperRecircAirInlet(DamperNum)%AirHumRat + DamperOAInlet(DamperNum)%AirHumRat)/2.0d0
      Enthalpy = (DamperRecircAirInlet(DamperNum)%AirEnthalpy + DamperOAInlet(DamperNum)%AirEnthalpy)/2.0d0
    ENDIF
  ELSE

    ! The Max Box Flow Rate is zero and the box is off.
    DamperRecircAirInlet(DamperNum)%AirMassFlowRate = 0.0d0
    DamperOAInlet(DamperNum)%AirMassFlowRate = 0.0d0
    HumRat =(DamperRecircAirInlet(DamperNum)%AirHumRat + DamperOAInlet(DamperNum)%AirHumRat)/2.0d0
    Enthalpy = (DamperRecircAirInlet(DamperNum)%AirEnthalpy + DamperOAInlet(DamperNum)%AirEnthalpy)/2.0d0
  END IF

  Temperature = PsyTdbFnHW(Enthalpy,HumRat)

  DamperOutlet(DamperNum)%AirTemp         = Temperature
  DamperOutlet(DamperNum)%AirHumRat       = HumRat
  DamperOutlet(DamperNum)%AirMassFlowRate = TotMassFlow
  DamperOutlet(DamperNum)%AirMassFlowRateMaxAvail = MassFlowMax
  DamperOutlet(DamperNum)%AirEnthalpy            = Enthalpy

   !Calculate the OA and RA damper position in %
  IF ( Damper(DamperNum)%RecircIsUsed ) THEN
    IF (DamperRecircAirInlet(DamperNum)%AirmassFlowRateMax == 0.d0) THEN !protect div by zero
      Damper(DamperNum)%RecircAirDamperPosition = 0.0d0
    ELSE
      Damper(DamperNum)%RecircAirDamperPosition = DamperRecircAirInlet(DamperNum)%AirMassFlowRate/ &
                                               DamperRecircAirInlet(DamperNum)%AirmassFlowRateMax
    ENDIF
  ENDIF

  IF (DamperOAInlet(DamperNum)%AirmassFlowRateMax == 0.0d0) THEN !protect div by zero
    Damper(DamperNum)%OADamperPosition  = 0.0d0
  ELSE
    Damper(DamperNum)%OADamperPosition  = DamperOAInlet(DamperNum)%AirMassFlowRate/ &
                                             DamperOAInlet(DamperNum)%AirmassFlowRateMax
  ENDIF

   !Calculate OAFraction of mixed air after the box
  IF (TotMassFlow > 0) THEN
    IF (Damper(DamperNum)%RecircIsUsed) THEN
      If(DamperOAInlet(DamperNum)%AirmassFlowRate == 0.0d0) THEN
        Damper(DamperNum)%OAFraction = 0.0d0
      ELSEIF( DamperRecircAirInlet(DamperNum)%AirmassFlowRate == 0.0d0) THEN
        Damper(DamperNum)%OAFraction = 1.0d0
      ELSE
        Damper(DamperNum)%OAFraction = DamperOAInlet(DamperNum)%AirmassFlowRate / TotMassFlow
      END IF
    ELSE
      Damper(DamperNum)%OAFraction = 1.0d0
    ENDIF
  ELSE
    Damper(DamperNum)%OAFraction = 0.0d0
  ENDIF


   DamperRecircAirInlet(DamperNum)%AirMassFlowRateHist3 = DamperRecircAirInlet(DamperNum)%AirMassFlowRateHist2
   DamperRecircAirInlet(DamperNum)%AirMassFlowRateHist2 = DamperRecircAirInlet(DamperNum)%AirMassFlowRateHist1
   DamperRecircAirInlet(DamperNum)%AirMassFlowRateHist1 = DamperRecircAirInlet(DamperNum)%AirmassFlowRate

  RETURN
END SUBROUTINE SimDualDuctVAVOutdoorAir

SUBROUTINE CalcOAMassFlow(DamperNum, SAMassFlow, AirLoopOAFrac)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad (FSEC)
          !       DATE WRITTEN   Mar 2010
          !       MODIFIED       Mangesh Basarkar, 06/2011: Modifying outside air based on airloop DCV flag
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates the amount of outside air required based on optional user input.
          ! Zone multipliers are included and are applied in GetInput.

          ! METHODOLOGY EMPLOYED:
          ! User input defines method used to calculate OA.

          ! REFERENCES:

          ! USE STATEMENTS:
   USE DataAirLoop,       ONLY: AirLoopFlow, AirLoopControlInfo
   USE DataZoneEquipment, ONLY: ZoneEquipConfig, CalcDesignSpecificationOutdoorAir
   USE Psychrometrics,    ONLY: PsyRhoAirFnPbTdbW

   IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   INTEGER, INTENT(IN)      :: DamperNum     ! index to terminal unit
   REAL(r64), INTENT(INOUT) :: SAMassFlow    ! outside air based on optional user input
   REAL(r64), INTENT(INOUT) :: AirLoopOAFrac ! outside air based on optional user input

          ! FUNCTION PARAMETER DEFINITIONS:
   LOGICAL, PARAMETER   :: UseMinOASchFlag = .TRUE. ! Always use min OA schedule in calculations.

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
   INTEGER   :: AirLoopNum        ! Index to air loop
   REAL(r64) :: RhoAir            ! density of terminal unit inlet air
   REAL(r64) :: OAVolumeFlowRate  ! outside air volume flow rate (m3/s)
   REAL(r64) :: OAMassFlow        ! outside air mass flow rate (kg/s)

   ! initialize OA flow rate and OA report variable
   SAMassFlow    = 0.0D0
   AirLoopOAFrac = 0.0D0
   AirLoopNum    = 0
   IF(Damper(DamperNum)%CtrlZoneNum .GT. 0)AirLoopNum = ZoneEquipConfig(Damper(DamperNum)%CtrlZoneNum)%AirLoopNum

   ! Calculate the amount of OA based on optional user inputs
   IF(AirLoopNum .GT. 0)THEN
     AirLoopOAFrac = AirLoopFlow(AirLoopNum)%OAFrac
     ! If no additional input from user, RETURN from subroutine
     IF(Damper(DamperNum)%NoOAFlowInputFromUser)RETURN
     ! Calculate outdoor air flow rate, zone multipliers are applied in GetInput
     IF(AirLoopOAFrac .GT. 0.0D0)THEN
       OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir(Damper(DamperNum)%OARequirementsPtr, &
                          Damper(DamperNum)%ActualZoneNum, AirLoopControlInfo(AirLoopNum)%AirLoopDCVFlag, UseMinOASchFlag)
       RhoAir = PsyRhoAirFnPbTdbW(Node(Damper(DamperNum)%OutletNodeNum)%Press, &
                                  Node(Damper(DamperNum)%OutletNodeNum)%Temp, &
                                  Node(Damper(DamperNum)%OutletNodeNum)%HumRat)
       OAMassFlow = OAVolumeFlowRate * RhoAir

       ! convert OA mass flow rate to supply air flow rate based on air loop OA fraction
       SAMassFlow = OAMassFlow / AirLoopOAFrac

     END IF

   END IF

   RETURN
END SUBROUTINE CalcOAMassFlow

SUBROUTINE CalcOAOnlyMassFlow(DamperNum, OAMassFlow, MaxOAVolFlow)

          ! FUNCTION INFORMATION:
          !       AUTHOR         C. Miller (Mod of CaclOAMassFlow by R. Raustad (FSEC))
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       B. Griffith, Dec 2010 clean up, sizing optional, scheduled OA
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates the amount of outside air required based on optional user input. Returns
          ! ONLY calculated OAMassFlow without consideration of AirLoopOAFrac. Used for
          ! the DualDuct:VAV:OutdoorAir object which does not mix OA with RA

          ! METHODOLOGY EMPLOYED:
          ! User input defines method used to calculate OA.

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: CalcDesignSpecificationOutdoorAir
  USE Psychrometrics,    ONLY: PsyRhoAirFnPbTdbW

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: DamperNum     ! index to terminal unit
  REAL(r64), INTENT(INOUT) :: OAMassFlow    ! outside air flow from user input kg/s
  REAL(r64), INTENT(OUT), OPTIONAL :: MaxOAVolFlow ! design level for outside air m3/s


          ! FUNCTION PARAMETER DEFINITIONS:
  LOGICAL, PARAMETER   :: UseMinOASchFlag = .TRUE. ! Always use min OA schedule in calculations.

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

  REAL(r64) :: RhoAir            ! density of terminal unit inlet air
  REAL(r64) :: OAVolumeFlowRate  ! outside air volume flow rate (m3/s)
  LOGICAL   :: UseOccSchFlag     ! TRUE = use actual occupancy, FALSE = use total zone people
  LOGICAL   :: PerPersonNotSet

 ! Calculate the amount of OA based on optional user inputs
  OAMassFlow   = 0.d0

  ! If no additional input from user, RETURN from subroutine
  IF(Damper(DamperNum)%NoOAFlowInputFromUser) THEN
    CALL ShowSevereError('CalcOAOnlyMassFlow: Problem in AirTerminal:DualDuct:VAV:OutdoorAir = ' // &
                         Trim(Damper(DamperNum)%DamperName) // ', check outdoor air specification' )
    IF(PRESENT(MaxOAVolFlow))MaxOAVolFlow = 0.d0
    RETURN
  ENDIF

  IF (Damper(DamperNum)%OAPerPersonMode == PerPersonDCVByCurrentLevel) THEN
    UseOccSchFlag   = .TRUE.
    PerPersonNotSet = .FALSE.
  ELSE
    UseOccSchFlag   = .FALSE.
    PerPersonNotSet = .FALSE.
    IF (Damper(DamperNum)%OAPerPersonMode == PerPersonModeNotSet) PerPersonNotSet = .TRUE.
  END IF

  OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir(Damper(DamperNum)%OARequirementsPtr, &
                       Damper(DamperNum)%ActualZoneNum, UseOccSchFlag, UseMinOASchFlag, PerPersonNotSet = PerPersonNotSet)

  RhoAir = PsyRhoAirFnPbTdbW(Node(Damper(DamperNum)%OutletNodeNum)%Press, &
                                Node(Damper(DamperNum)%OutletNodeNum)%Temp, &
                               Node(Damper(DamperNum)%OutletNodeNum)%HumRat, &
                               'HVACDualDuctSystem:CalcOAOnlyMassFlow')

  OAMassFlow = OAVolumeFlowRate * RhoAir

  IF (PRESENT(MaxOAVolFlow)) THEN
    OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir(Damper(DamperNum)%OARequirementsPtr, &
                       Damper(DamperNum)%ActualZoneNum, UseOccSchFlag, UseMinOASchFlag, MaxOAVolFlowFlag = .TRUE.)
    MaxOAVolFlow = OAVolumeFlowRate
  ENDIF


  RETURN
END SUBROUTINE CalcOAOnlyMassFlow

! End Algorithm Section of the Module
! *****************************************************************************

! Beginning of Update subroutines for the Damper Module
! *****************************************************************************

SUBROUTINE UpdateDualDuct(DamperNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   February 2000
          !       MODIFIED       Aug 2010 Clayton Miller - Added DualDuctVAVOutdoorAir
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the dampers.

          ! METHODOLOGY EMPLOYED:
          ! There is method to this madness.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataContaminantBalance, ONLY: Contaminant

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   Integer, Intent(IN) :: DamperNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  Integer             :: OutletNode
  Integer             :: HotInletNode
  Integer             :: ColdInletNode
  Integer             :: OAInletNode       !Outdoor Air Duct Inlet Node - for DualDuctOutdoorAir
  Integer             :: RAInletNode       !Recirculated Air Duct Inlet Node - for DualDuctOutdoorAir

  If (Damper(DamperNum)%DamperType == DualDuct_ConstantVolume .or. &
      Damper(DamperNum)%DamperType == DualDuct_VariableVolume) Then

      OutletNode = Damper(DamperNum)%OutletNodeNum
      HotInletNode = Damper(DamperNum)%HotAirInletNodeNum
      ColdInletNode = Damper(DamperNum)%ColdAirInletNodeNum

      ! Set the outlet air nodes of the Damper
      Node(HotInletNode)%MassFlowRate  = DamperHotAirInlet(DamperNum)%AirMassFlowRate
      Node(ColdInletNode)%MassFlowRate  = DamperColdAirInlet(DamperNum)%AirMassFlowRate
      Node(OutletNode)%MassFlowRate  = DamperOutlet(DamperNum)%AirMassFlowRate
      Node(OutletNode)%MassFlowRateMaxAvail  = DamperOutlet(DamperNum)%AirMassFlowRate
      Node(OutletNode)%MassFlowRateMinAvail  = DamperOutlet(DamperNum)%AirMassFlowRateMinAvail
      Node(OutletNode)%Temp          = DamperOutlet(DamperNum)%AirTemp
      Node(OutletNode)%HumRat        = DamperOutlet(DamperNum)%AirHumRat
      Node(OutletNode)%Enthalpy      = DamperOutlet(DamperNum)%AirEnthalpy
      ! Set the outlet nodes for properties that just pass through & not used
      ! FIX THIS LATER!!!!
      Node(OutletNode)%Quality         = Node(HotInletNode)%Quality
      Node(OutletNode)%Press           = Node(HotInletNode)%Press

  ELSE IF (Damper(DamperNum)%DamperType == DualDuct_OutdoorAir) Then

      OutletNode = Damper(DamperNum)%OutletNodeNum
      OAInletNode = Damper(DamperNum)%OAInletNodeNum
      IF ( Damper(DamperNum)%RecircIsUsed ) THEN
        RAInletNode = Damper(DamperNum)%RecircAirInletNodeNum
        Node(RAInletNode)%MassFlowRate  = DamperRecircAirInlet(DamperNum)%AirMassFlowRate
      ENDIF
      ! Set the outlet air nodes of the Damper
      Node(OAInletNode)%MassFlowRate  = DamperOAInlet(DamperNum)%AirMassFlowRate
      Node(OutletNode)%MassFlowRate  = DamperOutlet(DamperNum)%AirMassFlowRate
      Node(OutletNode)%MassFlowRateMaxAvail  = DamperOutlet(DamperNum)%AirMassFlowRate
      Node(OutletNode)%MassFlowRateMinAvail  = DamperOutlet(DamperNum)%AirMassFlowRateMinAvail
      Node(OutletNode)%Temp          = DamperOutlet(DamperNum)%AirTemp
      Node(OutletNode)%HumRat        = DamperOutlet(DamperNum)%AirHumRat
      Node(OutletNode)%Enthalpy      = DamperOutlet(DamperNum)%AirEnthalpy
      ! Set the outlet nodes for properties that just pass through & not used
      ! FIX THIS LATER!!!!
      Node(OutletNode)%Quality         = Node(OAInletNode)%Quality
      Node(OutletNode)%Press           = Node(OAInletNode)%Press
  END IF

  IF (Contaminant%CO2Simulation) Then
    Node(OutletNode)%CO2 = MAX(Node(HotInletNode)%CO2,Node(ColdInletNode)%CO2)
  End If
  IF (Contaminant%GenericContamSimulation) Then
    Node(OutletNode)%GenContam = MAX(Node(HotInletNode)%GenContam,Node(ColdInletNode)%GenContam)
  End If

  RETURN
END Subroutine UpdateDualDuct

!        End of Update subroutines for the Damper Module
! *****************************************************************************


! Beginning of Reporting subroutines for the Damper Module
! *****************************************************************************

SUBROUTINE ReportDualDuct(DamperNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Unknown
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the damper report variables.

          ! METHODOLOGY EMPLOYED:
          ! There is method to this madness.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   Integer, Intent(IN) :: DamperNum !unused1208

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

 ! Still needs to report the Damper power from this component


  RETURN
END Subroutine ReportDualDuct

SUBROUTINE ReportDualDuctConnections

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael J. Witte
          !       DATE WRITTEN   February 2004
          !       MODIFIED       B. Griffith, DOAS VAV dual duct
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Report dual duct damper connections to the BND file.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: OutputFileBNDetails
  USE DataAirLoop, ONLY: AirToZoneNodeInfo
  USE DataHVACGlobals, ONLY: NumPrimaryAirSys
  USE DataZoneEquipment, ONLY: SupplyAirPath, NumSupplyAirPaths

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
  INTEGER Count1
  INTEGER Count2
  INTEGER Count3
  INTEGER Found
  INTEGER SupplyAirPathNum ! Supply air path ID
  CHARACTER(len=20) ChrOut
  CHARACTER(len=MaxNameLength) ChrName
  CHARACTER(len=MaxNameLength) DamperType

          !Report Dual Duct Dampers to BND File
  WRITE(OutputFileBNDetails,101) '! ==============================================================='
  WRITE(OutputFileBNDetails,100)
  WRITE(ChrOut,*) NumDampers*2
  WRITE(OutputFileBNDetails,101) ' #Dual Duct Damper Connections,'//TRIM(ADJUSTL(ChrOut))
  WRITE(OutputFileBNDetails,102)
 100 FORMAT('! <#Dual Duct Damper Connections>,<Number of Dual Duct Damper Connections>')
 101 FORMAT(A)
 102 FORMAT('! <Dual Duct Damper>,<Dual Duct Damper Count>,<Dual Duct Damper Name>,<Inlet Node>,', &
            '<Outlet Node>,<Inlet Node Type>,<AirLoopHVAC Name>')

  DO Count1=1,NumDampers

          ! Determine if this damper is connected to a supply air path
    Found = 0
    DO Count2 = 1, NumSupplyAirPaths
      SupplyAirPathNum = Count2
      Found = 0
      DO Count3=1,SupplyAirPath(Count2)%NumOutletNodes
        IF (Damper(Count1)%HotAirInletNodeNum    == SupplyAirPath(Count2)%OutletNode(Count3)) Found = Count3
        IF (Damper(Count1)%ColdAirInletNodeNum   == SupplyAirPath(Count2)%OutletNode(Count3)) Found = Count3
        IF (Damper(Count1)%OAInletNodeNum        == SupplyAirPath(Count2)%OutletNode(Count3)) Found = Count3
        IF (Damper(Count1)%RecircAirInletNodeNum == SupplyAirPath(Count2)%OutletNode(Count3)) Found = Count3
      ENDDO
      IF (Found /= 0) EXIT
    ENDDO
    IF (Found == 0) SupplyAirPathNum = 0

          ! Determine which air loop this dual duct damper is connected to
    Found = 0
    DO Count2 = 1, NumPrimaryAirSys
      ChrName = TRIM(AirToZoneNodeInfo(Count2)%AirLoopName)
      Found = 0
      DO Count3=1,AirToZoneNodeInfo(Count2)%NumSupplyNodes
        IF (SupplyAirPathNum /=0) THEN
          IF (SupplyAirPath(SupplyAirPathNum)%InletNodeNum == AirToZoneNodeInfo(Count2)%ZoneEquipSupplyNodeNum(Count3))   &
                        Found = Count3
        ELSE
          IF (Damper(Count1)%HotAirInletNodeNum    == AirToZoneNodeInfo(Count2)%ZoneEquipSupplyNodeNum(Count3)) Found = Count3
          IF (Damper(Count1)%ColdAirInletNodeNum   == AirToZoneNodeInfo(Count2)%ZoneEquipSupplyNodeNum(Count3)) Found = Count3
          IF (Damper(Count1)%OAInletNodeNum        == AirToZoneNodeInfo(Count2)%ZoneEquipSupplyNodeNum(Count3)) Found = Count3
          IF (Damper(Count1)%RecircAirInletNodeNum == AirToZoneNodeInfo(Count2)%ZoneEquipSupplyNodeNum(Count3)) Found = Count3
        ENDIF
      ENDDO
      IF (Found /= 0) EXIT
    ENDDO
    IF (Found == 0) ChrName = '**Unknown**'

    WRITE(ChrOut,*) Count1
    IF (Damper(Count1)%DamperType == DualDuct_ConstantVolume) THEN
      DamperType=cCMO_DDConstantVolume
    ELSEIF (Damper(Count1)%DamperType == DualDuct_VariableVolume) THEN
      DamperType=cCMO_DDVariableVolume
    ELSEIF (Damper(Count1)%DamperType == DualDuct_OutdoorAir) THEN
      DamperType=cCMO_DDVarVolOA
    ELSE
      DamperType='Invalid/Unknown'
    ENDIF

    IF ((Damper(Count1)%DamperType == DualDuct_ConstantVolume) .OR. &
        (Damper(Count1)%DamperType == DualDuct_VariableVolume) ) THEN
      WRITE(OutputFileBNDetails,101) ' Dual Duct Damper,'//TRIM(ADJUSTL(ChrOut))//','// &
            TRIM(DamperType)//','//  &
            TRIM(Damper(Count1)%DamperName)//','//  &
            TRIM(NodeID(Damper(Count1)%HotAirInletNodeNum))//','//  &
            TRIM(NodeID(Damper(Count1)%OutletNodeNum))//','//  &
            'Hot Air'//','//TRIM(ChrName)

      WRITE(OutputFileBNDetails,101) ' Dual Duct Damper,'//TRIM(ADJUSTL(ChrOut))//','// &
            TRIM(DamperType)//','//  &
            TRIM(Damper(Count1)%DamperName)//','//  &
            TRIM(NodeID(Damper(Count1)%ColdAirInletNodeNum))//','//  &
            TRIM(NodeID(Damper(Count1)%OutletNodeNum))//','//  &
            'Cold Air'//','//TRIM(ChrName)
    ELSEIF (Damper(Count1)%DamperType == DualDuct_OutdoorAir)  THEN
      WRITE(OutputFileBNDetails, 101 ) 'Dual Duct Damper, '//Trim(Adjustl(ChrOut))//','// &
            TRIM(DamperType)//','// &
            TRIM(Damper(Count1)%DamperName)//','// &
            Trim(NodeID(Damper(Count1)%OAInletNodeNum))//','// &
            TRIM(NodeID(Damper(Count1)%OutletNodeNum))//','// &
            'Outdoor Air'//','//TRIM(ChrName)
      WRITE(OutputFileBNDetails, 101 ) 'Dual Duct Damper, '//Trim(Adjustl(ChrOut))//','// &
            TRIM(DamperType)//','// &
            TRIM(Damper(Count1)%DamperName)//','// &
            Trim(NodeID(Damper(Count1)%RecircAirInletNodeNum))//','// &
            TRIM(NodeID(Damper(Count1)%OutletNodeNum))//','// &
            'Recirculated Air'//','//TRIM(ChrName)
    ENDIF
  ENDDO

  RETURN

END SUBROUTINE ReportDualDuctConnections



SUBROUTINE GetDualDuctOutdoorAirRecircUse(CompTypeName, CompName, RecircIsUsed )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Aug 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! get routine to learn if a dual duct outdoor air unit is using its recirc deck

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY : FindItemInList, GetNumObjectsFound, GetObjectItem

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=MaxNameLength), INTENT(IN)  :: CompTypeName
  CHARACTER(len=MaxNameLength), INTENT(IN)  :: CompName
  LOGICAL                     , INTENT(OUT) :: RecircIsUsed

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  INTEGER :: DamperNum
  LOGICAL, SAVE :: FirstTimeOnly = .TRUE.
  LOGICAL, SAVE, ALLOCATABLE ,DIMENSION (:) :: RecircIsUsedARR
  CHARACTER(len=MaxNameLength),SAVE, ALLOCATABLE ,DIMENSION (:) :: DamperNamesARR
  INTEGER :: DamperIndex    ! Loop index to Damper that you are currently loading input into
  CHARACTER (len=MaxNameLength)  :: CurrentModuleObject     ! for ease in getting objects
  REAL(r64), DIMENSION(2) :: NumArray = 0.0d0
  CHARACTER(len=MaxNameLength), DIMENSION(7) :: AlphArray = ' '
  CHARACTER(len=MaxNameLength), DIMENSION(7) :: cAlphaFields = ' '     ! Alpha field names
  CHARACTER(len=MaxNameLength), DIMENSION(2) :: cNumericFields = ' '   ! Numeric field names
  LOGICAL, DIMENSION(7)   :: lAlphaBlanks = .TRUE.      ! Logical array, alpha field input BLANK = .true.
  LOGICAL, DIMENSION(2)   :: lNumericBlanks = .TRUE.    ! Logical array, numeric field input BLANK = .true.
  INTEGER :: NumAlphas
  INTEGER :: NumNums
  INTEGER :: IOSTAT

  RecircIsUsed = .TRUE.

! this doesn't work because it fires code that depends on things being further along
!  IF (GetDualDuctInputFlag) THEN  !First time subroutine has been entered
!    CALL GetDualDuctInput
!    GetDualDuctInputFlag=.false.
!  END IF

  IF (FirstTimeOnly) THEN
    NumDualDuctVarVolOA = GetNumObjectsFound( cCMO_DDVarVolOA )
    ALLOCATE(RecircIsUsedARR(NumDualDuctVarVolOA))
    ALLOCATE(DamperNamesARR(NumDualDuctVarVolOA))
    IF (NumDualDuctVarVolOA > 0) THEN
      DO DamperIndex = 1,  NumDualDuctVarVolOA

        CurrentModuleObject=cCMO_DDVarVolOA

        CALL GetObjectItem(CurrentModuleObject,DamperIndex,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
        DamperNamesARR(DamperIndex) = AlphArray(1)
        IF (.NOT. lAlphaBlanks(5)) THEN
          RecircIsUsedARR(DamperIndex) = .TRUE.
        ELSE
          RecircIsUsedARR(DamperIndex) = .FALSE.
        ENDIF
      ENDDO
    ENDIF
    FirstTimeOnly = .FALSE.
  ENDIF

  DamperIndex = FindItemInList(CompName, DamperNamesARR, NumDualDuctVarVolOA)
  IF (DamperIndex > 0) THEN
    RecircIsUsed = RecircIsUsedARR(DamperIndex)
  ENDIF

  RETURN

END SUBROUTINE GetDualDuctOutdoorAirRecircUse



!        End of Reporting subroutines for the Damper Module
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

End Module DualDuct

