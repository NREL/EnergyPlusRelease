MODULE SingleDuct
  ! Module containing the Single Duct Systems as a single component/ or really a single driver

  ! MODULE INFORMATION:
  !       AUTHOR         Richard J. Liesen
  !       DATE WRITTEN   January 2000
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required to
  ! simulate single duct systems as a single driver or inter-connecting controllers.

  ! METHODOLOGY EMPLOYED:
  ! Needs description, as appropriate.

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals,     ONLY: BeginEnvrnFlag, BeginDayFlag, MaxNameLength, &
                           InitConvTemp, SysSizingCalc, NumOfZones, DisplayExtraWarnings
Use DataEnvironment, ONLY: StdBaroPress, StdRhoAir
USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad, SmallAirVolFlow, TurnFansOn, SingleCoolingSetPoint, &
                           SingleHeatingSetPoint, SingleHeatCoolSetPoint, DualSetPointWithDeadBand, &
                           ATMixer_InletSide, ATMixer_SupplySide
USE DataHeatBalFanSys, ONLY: TempControlType
USE BranchNodeConnections, ONLY: SetUpCompSets, TestCompSet
USE DataSizing
USE Psychrometrics, ONLY:PsyRhoAirFnPbTdbW, PsyCpAirFnWTdb
USE FluidProperties
USE DataInterfaces,  ONLY: ControlCompOutput, ShowWarningError, ShowFatalError, ShowSevereError, &
                           SetupOutputVariable, ShowContinueError, ShowWarningMessage,  &
                           ShowContinueErrorTimeStamp, ShowRecurringWarningErrorAtEnd,   &
                           ShowRecurringContinueErrorAtEnd, ShowMessage

  ! Use statements for access to subroutines in other modules
USE ScheduleManager
USE SteamCoils

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS
INTEGER, PARAMETER :: Normal = 1
INTEGER, PARAMETER :: ReverseAction = 2
! SysTypes represented here
INTEGER, PARAMETER :: SingleDuctVAVReheat=3
INTEGER, PARAMETER :: SingleDuctConstVolReheat=4
INTEGER, PARAMETER :: SingleDuctVAVNoReheat=5
INTEGER, PARAMETER :: SingleDuctVAVReheatVSFan=6
INTEGER, PARAMETER :: SingleDuctCBVAVReheat=10
INTEGER, PARAMETER :: SingleDuctCBVAVNoReheat=11
! Reheat Coil Types used here
INTEGER, PARAMETER :: HCoilType_None = 0
INTEGER, PARAMETER :: HCoilType_Gas = 1
INTEGER, PARAMETER :: HCoilType_Electric = 2
INTEGER, PARAMETER :: HCoilType_SimpleHeating = 3
INTEGER, PARAMETER :: HCoilType_SteamAirHeating = 4
! Fan types used here
INTEGER, PARAMETER :: FanType_None = 0
INTEGER, PARAMETER :: FanType_VS = 1
! Minimum Flow Fraction Input Method
INTEGER, PARAMETER :: ConstantMinFrac = 1
INTEGER, PARAMETER :: ScheduledMinFrac = 2
INTEGER, PARAMETER :: FixedMin = 3
INTEGER            :: NumATMixers=0

  ! DERIVED TYPE DEFINITIONS
TYPE SysDesignParams
  CHARACTER(len=MaxNameLength) :: SysName     = ' ' ! Name of the Sys
  CHARACTER(len=MaxNameLength) :: SysType     = ' ' ! Type of Sys ie. VAV, Mixing, Inducing, etc.
  INTEGER                 :: SysType_Num      = 0   ! Numeric Equivalent for System type
  CHARACTER(len=MaxNameLength) :: Schedule    = ' ' ! Sys Operation Schedule
  INTEGER                      :: SchedPtr    = 0   ! Pointer to the correct schedule
  CHARACTER(len=MaxNameLength) :: ReheatComp  = ' ' ! Type of the Reheat Coil Object
  INTEGER                 :: ReheatComp_Num   = 0   ! Numeric Equivalent in this module for Coil type
  INTEGER                 :: ReheatComp_Index = 0   ! Returned Index number from other routines
  CHARACTER(len=MaxNameLength) :: ReheatName  = ' ' ! name of reheat coil
  INTEGER                 :: ReheatComp_PlantType  = 0   ! typeOf_ number for plant type of heating coil
  CHARACTER(len=MaxNameLength) :: FanType     = ' ' ! Type of the Fan Object
  INTEGER                 :: Fan_Num          = 0   ! Numeric Equivalent in this module for fan type
  INTEGER                 :: Fan_Index        = 0   ! Returned Index number from other routines
  INTEGER                 :: ControlCompTypeNum = 0
  INTEGER                 :: CompErrIndex       = 0
  CHARACTER(len=MaxNameLength) :: FanName     = ' ' ! name of fan
  REAL(r64)    :: MaxAirVolFlowRate           = 0.0D0 ! Max Specified Volume Flow Rate of Sys (cooling max) [m3/sec]
  REAL(r64)    :: AirMassFlowRateMax          = 0.0D0 ! Max Specified Mass Flow Rate of Sys (cooling max) [kg/sec]
  REAL(r64)    :: MaxHeatAirVolFlowRate       = 0.0D0 ! Max specified volume flow rate of unit at max heating [m3/s]
  REAL(r64)    :: HeatAirMassFlowRateMax      = 0.0D0 ! Max Specified Mass Flow Rate of unit at max heating [kg/sec]
  INTEGER      :: ZoneMinAirFracMethod        = ConstantMinFrac   ! parameter for what method is used for min flow fraction
  REAL(r64)    :: ZoneMinAirFrac              = 0.0D0 ! Fraction of supply air used as minimum flow
  REAL(r64)    :: ZoneFixedMinAir             = 0.0D0 ! Absolute minimum supply air flow
  INTEGER      :: ZoneMinAirFracSchPtr        = 0   ! pointer to the schedule for min flow fraction
  LOGICAL      :: ConstantMinAirFracSetByUser = .FALSE.  ! record if user left field blank for constant min fraction.
  LOGICAL      :: FixedMinAirSetByUser = .FALSE.  ! record if user left field blank for constant min fraction.
  REAL(r64)    :: DesignMinAirFrac            = 0.0D0    ! store user entered constant min flow fract for design
  REAL(r64)    :: DesignFixedMinAir           = 0.0D0    ! store user entered constant min flow for design
  INTEGER      :: InletNodeNum                = 0   ! terminal unit inlet node number; damper inlet node number
  INTEGER      :: OutletNodeNum               = 0   ! damper outlet node number for VAV; unused by CV; coil air inlet node for VAV
                                                    ! fan outlet node, coil inlet node for VAV VS Fan
  INTEGER      :: ReheatControlNode           = 0   ! hot water inlet node for heating coil
  INTEGER      :: ReheatCoilOutletNode        = 0   ! outlet node for heating coil
  REAL(r64)    :: ReheatCoilMaxCapacity       = 0.0D0  ! heating coil capacity, W
  INTEGER      :: ReheatAirOutletNode         = 0   ! terminal unit outlet node; heating coil air outlet node
  REAL(r64)    :: MaxReheatWaterVolFlow       = 0.0D0 ! m3/s
  REAL(r64)    :: MaxReheatSteamVolFlow       = 0.0D0 ! m3/s
  REAL(r64)    :: MaxReheatWaterFlow          = 0.0D0 ! kg/s
  REAL(r64)    :: MaxReheatSteamFlow          = 0.0D0 ! kg/s
  REAL(r64)    :: MinReheatWaterVolFlow       = 0.0D0 ! m3/s
  REAL(r64)    :: MinReheatSteamVolFlow       = 0.0D0 ! m3/s
  REAL(r64)    :: MinReheatWaterFlow          = 0.0D0 ! kg/s
  REAL(r64)    :: MinReheatSteamFlow          = 0.0D0 ! kg/s
  REAL(r64)    :: ControllerOffset            = 0.0D0 !
  REAL(r64)    :: MaxReheatTemp               = 0.0D0 ! C
  LOGICAL      :: MaxReheatTempSetByUser      = .FALSE.
  INTEGER      :: DamperHeatingAction         = 0     !! 1=NORMAL;  2=REVERSE ACTION
  REAL(r64)    :: DamperPosition              = 0.0D0 !
  INTEGER      :: ADUNum                      = 0   ! index of corresponding air distribution unit
  INTEGER      :: FluidIndex                  = 0   ! Refrigerant index
  INTEGER      :: ErrCount1                   = 0   ! iteration limit exceeded in Hot Water Flow Calc
  INTEGER      :: ErrCount1c                  = 0   ! iteration limit exceeded in Hot Water Flow Calc - continue
  INTEGER      :: ErrCount2                   = 0   ! bad iterations limits in hot water flow calc

  REAL(r64)    :: ZoneFloorArea                 = 0.0D0 ! Zone floor area
  INTEGER      :: CtrlZoneNum                   = 0     ! Pointer to CtrlZone data structure
  INTEGER      :: ActualZoneNum                 = 0     ! Pointer to Zone data Structure
  REAL(r64)    :: MaxAirVolFlowRateDuringReheat = 0.0D0 ! Maximum vol flow during reheat
  REAL(r64)    :: MaxAirVolFractionDuringReheat = 0.0D0 ! Maximum vol flow fraction during reheat
  REAL(r64)    :: AirMassFlowDuringReheatMax    = 0.0D0 ! Maximum mass flow during reheat
  INTEGER      :: ZoneOutdoorAirMethod          = 0     ! Outdoor air method
  REAL(r64)    :: OutdoorAirFlowRate            = 0.0D0 ! report variable for TU outdoor air flow rate
  LOGICAL      :: NoOAFlowInputFromUser         = .TRUE. ! avoids OA calculation if no input specified by user

  INTEGER      :: OARequirementsPtr = 0     !- Index to DesignSpecification:OutdoorAir object

  INTEGER      :: AirLoopNum       = 0
  INTEGER      :: HWLoopNum        = 0 !plant topology, loop number
  INTEGER      :: HWLoopSide       = 0 !plant topology, loop side number
  INTEGER      :: HWBranchIndex    = 0 !plant topology, Branch number
  INTEGER      :: HWCompIndex      = 0 !plant topology, Component number
  CHARACTER(len=MaxNameLength) :: ZoneHVACUnitType     =' '  ! type of Zone HVAC unit for air terminal mixer units
  CHARACTER(len=MaxNameLength) :: ZoneHVACUnitName     =' '  ! name of Zone HVAC unit for air terminal mixer units
  INTEGER                      :: SecInNode        =0    ! zone or zone unit air node number

! warning variables
  INTEGER      :: IterationLimit   = 0 ! Used for RegulaFalsi error -1
  INTEGER      :: IterationFailed  = 0 ! Used for RegulaFalsi error -2

END TYPE SysDesignParams

TYPE AirTerminalMixerData
  ! Input data
  CHARACTER(len=MaxNameLength) :: Name                 =' '  ! name of unit
  INTEGER                      :: MixerType            =0    ! type of inlet mixer, 1 = inlet side, 2 = supply side
  INTEGER                      :: ZoneHVACUnitType     =0    ! type of Zone HVAC unit. ZoneHVAC:WaterToAirHeatPump =1, ZoneHVAC:FourPipeFanCoil = 2
  CHARACTER(len=MaxNameLength) :: ZoneHVACUnitName     =' '  ! name of Zone HVAC unit
  INTEGER                      :: SecInNode            =0    ! secondary air inlet node number
  INTEGER                      :: PriInNode            =0    ! primary air inlet node number
  INTEGER                      :: MixedAirOutNode      =0    ! mixed air outlet node number

  REAL(r64)                    :: ZoneAirTemp          =0.0D0    ! zone air in temp
  REAL(r64)                    :: ZoneAirHumRat        =0.0D0    ! zone air in hum rat
  REAL(r64)                    :: ZoneAirEnthalpy      =0.0D0    ! zone air in enthalpy
  REAL(r64)                    :: ZoneAirPressure      =0.0D0    ! zone air in pressure
  REAL(r64)                    :: ZoneAirMassFlowRate  =0.0D0    ! zone air in mass flow rate

  REAL(r64)                    :: DOASTemp             =0.0D0    ! DOAS air in temp
  REAL(r64)                    :: DOASHumRat           =0.0D0    ! DOAS air in hum rat
  REAL(r64)                    :: DOASEnthalpy         =0.0D0    ! DOAS air in enthalpy
  REAL(r64)                    :: DOASPressure         =0.0D0    ! DOAS air in pressure
  REAL(r64)                    :: DOASMassFlowRate     =0.0D0    ! DOAS air in mass flow rate

  REAL(r64)                    :: MixedAirTemp         =0.0D0    ! mixed air in temp
  REAL(r64)                    :: MixedAirHumRat       =0.0D0    ! mixed air in hum rat
  REAL(r64)                    :: MixedAirEnthalpy     =0.0D0    ! mixed air in enthalpy
  REAL(r64)                    :: MixedAirPressure     =0.0D0    ! mixed air in pressure
  REAL(r64)                    :: MixedAirMassFlowRate =0.0D0    ! mixed air in mass flow rate

  REAL(r64)                    :: MaxAirMassFlowRate   =0.0D0    ! maximum air mass flow rate allowed through component
END TYPE AirTerminalMixerData

TYPE SysFlowConditions
  REAL(r64) :: AirMassFlowRate                    = 0.0D0 ! MassFlow through the Sys being Simulated [kg/Sec]
  REAL(r64) :: AirMassFlowRateMaxAvail            = 0.0D0 ! MassFlow through the Sys being Simulated [kg/Sec]
  REAL(r64) :: AirMassFlowRateMinAvail            = 0.0D0 ! MassFlow through the Sys being Simulated [kg/Sec]
  REAL(r64) :: AirTemp                            = 0.0D0 ! (C)
  REAL(r64) :: AirHumRat                          = 0.0D0 ! (Kg/Kg)
  REAL(r64) :: AirEnthalpy                        = 0.0D0 ! (J/Kg)
  REAL(r64) :: AirPressure                        = 0.0 !
END TYPE SysFlowConditions


  !MODULE VARIABLE DECLARATIONS:
  TYPE (SysDesignParams), ALLOCATABLE, DIMENSION(:)   :: Sys
  TYPE (SysFlowConditions), ALLOCATABLE, DIMENSION(:) :: SysInlet
  TYPE (SysFlowConditions), ALLOCATABLE, DIMENSION(:) :: SysOutlet
  TYPE (AirTerminalMixerData), ALLOCATABLE, DIMENSION(:) :: SysATMixer
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: MassFlow1 ! previous value of the terminal unit mass flow rate
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: MassFlow2 ! previous value of the previous value of the mass flow rate
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: MassFlow3
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: MassFlowDiff
  LOGICAL :: GetInputFlag = .True.  ! Flag set to make sure you get input once
  LOGICAL :: GetATMixerFlag = .True.  ! Flag set to make sure you get input once
  INTEGER :: NumConstVolSys = 0
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

  ! INTERFACE BLOCK SPECIFICATIONS
  ! see use DataInterfaces

  INTEGER :: NumSys = 0     ! The Number of Systems found in the Input

! Subroutine Specifications for the Module
          ! Driver/Manager Routines
Public  SimulateSingleDuct
Public  GetHVACSingleDuctSysIndex
Public  SimATMixer
Public  GetATMixerOutNode
Public  GetATMixerPriNode
Public  GetATMixerSecNode
Public  GetATMixer

          ! Get Input routines for module
PRIVATE GetSysInput
PRIVATE GetATMixers

          ! Initialization routines for module
PRIVATE InitSys
PRIVATE SizeSys
PRIVATE InitATMixer

          ! Algorithms for the module
Private SimVAV
Private SimConstVol
Private SimVAVVS
Private CalcVAVVS
Private SimCBVAV
Private CalcOAMassFlow
Private CalcATMixer

          ! Update routine to check convergence and update nodes
Private UpdateSys
Private UpdateATMixer

          ! Reporting routines for module
Private ReportSys


CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE SimulateSingleDuct(CompName,FirstHVACIteration, ZoneNum, ZoneNodeNum, CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   January 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages Sys system simulation.
          ! It is called from the ManageZoneEquip
          ! at the system time step.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
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
  INTEGER             :: SysNum     ! The Sys that you are currently loading input into

          ! FLOW:

  ! Obtains and Allocates Sys related parameters from input file
  IF (GetInputFlag) THEN  !First time subroutine has been entered
    CALL GetSysInput
    GetInputFlag=.false.
  End If


  ! Find the correct SysNumber with the Component Name
  IF (CompIndex == 0) THEN
    SysNum = FindItemInList(CompName,Sys%SysName,NumSys)
    IF (SysNum == 0) THEN
      CALL ShowFatalError('SimulateSingleDuct: System not found='//TRIM(CompName))
    ENDIF
    CompIndex=SysNum
  ELSE
    SysNum=CompIndex
    IF (SysNum > NumSys .or. SysNum < 1) THEN
      CALL ShowFatalError('SimulateSingleDuct: Invalid CompIndex passed='//TRIM(TrimSigDigits(CompIndex))// &
                          ', Number of Systems='//TRIM(TrimSigDigits(NumSys))//', System name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(SysNum)) THEN
      IF (CompName /= Sys(SysNum)%SysName) THEN
        CALL ShowFatalError('SimulateSingleDuct: Invalid CompIndex passed='//TRIM(TrimSigDigits(CompIndex))// &
                            ', System name='//TRIM(CompName)//', stored System Name for that index='//TRIM(Sys(SysNum)%SysName))
      ENDIF
      CheckEquipName(SysNum)=.false.
    ENDIF
  ENDIF

  TermUnitSingDuct = .TRUE.

  ! With the correct SysNum Initialize the system
  CALL InitSys(SysNum,FirstHVACIteration)  ! Initialize all Sys related parameters

  ! Calculate the Correct Sys Model with the current SysNum
  SELECT CASE(Sys(SysNum)%SysType_Num)

    CASE (SingleDuctConstVolReheat)  ! AirTerminal:SingleDuct:ConstantVolume:Reheat
    Call SimConstVol(SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum)

    CASE (SingleDuctVAVReheat) ! SINGLE DUCT:VAV:REHEAT
    Call SimVAV(SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum)

    CASE (SingleDuctVAVNoReheat) ! SINGLE DUCT:VAV:NOREHEAT
    Call SimVAV(SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum)

    CASE (SingleDuctVAVReheatVSFan) ! SINGLE DUCT:VAV:REHEAT:VS FAN
    Call SimVAVVS(SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum)

    CASE (SingleDuctCBVAVReheat) ! SINGLE DUCT:VAVHEATANDCOOL:REHEAT
    Call SimCBVAV(SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum)

    CASE (SingleDuctCBVAVNoReheat) ! SINGLE DUCT:VAVHEATANDCOOL:NOREHEAT
    Call SimCBVAV(SysNum, FirstHVACIteration, ZoneNum, ZoneNodeNum)

  END SELECT

  ! Report the current Sys
  Call ReportSys(SysNum)

  TermUnitSingDuct = .FALSE.
  RETURN

END SUBROUTINE SimulateSingleDuct


! Get Input Section of the Module
!******************************************************************************
SUBROUTINE GetSysInput

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
    USE InputProcessor
    USE NodeInputManager,  ONLY: GetOnlySingleNode
    USE DataZoneEquipment, ONLY: ZoneEquipConfig
    USE DataDefineEquip,   ONLY: AirDistUnit, NumAirDistUnits
    USE WaterCoils,        ONLY: GetCoilWaterInletNode,GetCoilOutletNode
    USE SteamCoils,        ONLY: GetCoilSteamInletNode,GetSteamCoilIndex,GetCoilAirOutletNode
    USE HeatingCoils,      ONLY: GetHeatingCoilCapacity=>GetCoilCapacity, GetHeatingCoilOutletNode=>GetCoilOutletNode
    USE Fans,              ONLY: GetFanInletNode,GetFanOutletNode
    USE DataIPShortCuts
    USE DataHeatBalance
    USE DataSizing,        ONLY: OARequirements, NumOARequirements
    USE DataPlant,         ONLY: TypeOf_CoilWaterSimpleHeating, TypeOf_CoilSteamAirHeating
    USE DataGlobals,       ONLY: DoZoneSizing, ScheduleAlwaysOn

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: RoutineName='GetSysInput: ' ! include trailing blank

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    INTEGER :: SysNum = 0     ! The Sys that you are currently loading input into
    INTEGER :: SysIndex = 0     ! The Sys that you are currently loading input into
    INTEGER :: NumVAVSys = 0
    INTEGER :: NumNoRHVAVSys = 0
    INTEGER :: NumVAVVS = 0
    INTEGER :: NumCBVAVSys = 0
    INTEGER :: NumNoRHCBVAVSys = 0
    INTEGER :: NumAlphas = 0
    INTEGER :: NumNums = 0
    INTEGER :: NumZoneSiz
    INTEGER :: ZoneSizIndex
    INTEGER :: IOSTAT
    INTEGER :: ZoneNum                 ! Index to actual zone number
    LOGICAL :: ErrorsFound = .false.   ! If errors detected in input
    LOGICAL :: IsNotOK                 ! Flag to verify name
    LOGICAL :: IsBlank                 ! Flag for blank name
    INTEGER :: CtrlZone   ! controlled zone do loop index
    INTEGER :: SupAirIn   ! controlled zone supply air inlet index
    INTEGER :: ADUNum     ! air distribution unit index
    CHARACTER(Len=MaxNameLength)  :: CurrentModuleObject      ! for ease in getting objects
    CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas         ! Alpha input items for object
    CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
    CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
    REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers           ! Numeric input items for object
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.
    INTEGER                              :: MaxNums=0         ! Maximum number of numeric input fields
    INTEGER                              :: MaxAlphas=0       ! Maximum number of alpha input fields
    INTEGER                              :: TotalArgs=0       ! Total number of alpha and numeric arguments (max) for a
                                                              !  certain object in the input file

          ! Flow
    NumVAVSys = GetNumObjectsFound('AirTerminal:SingleDuct:VAV:Reheat')
    NumNoRHVAVSys = GetNumObjectsFound('AirTerminal:SingleDuct:VAV:NoReheat')
    NumConstVolSys = GetNumObjectsFound('AirTerminal:SingleDuct:ConstantVolume:Reheat')
    NumVAVVS = GetNumObjectsFound('AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan')
    NumCBVAVSys = GetNumObjectsFound('AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat')
    NumNoRHCBVAVSys = GetNumObjectsFound('AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat')
    NumSys = NumVAVSys + NumConstVolSys + NumNoRHVAVSys + NumVAVVS + NumCBVAVSys + NumNoRHCBVAVSys

    ALLOCATE(Sys(NumSys))
    ALLOCATE(SysInlet(NumSys))
    ALLOCATE(SysOutlet(NumSys))
    ALLOCATE(CheckEquipName(NumSys))
    CheckEquipName=.true.

    ALLOCATE(MassFlow1(NumSys))
    ALLOCATE(MassFlow2(NumSys))
    ALLOCATE(MassFlow3(NumSys))
    ALLOCATE(MassFlowDiff(NumSys))

    MassFlow1 = 0.0D0
    MassFlow2 = 0.0D0
    MassFlow3 = 0.0D0
    MassFlowDiff = 0.0D0

    CALL GetObjectDefMaxArgs('AirTerminal:SingleDuct:VAV:Reheat',TotalArgs,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    CALL GetObjectDefMaxArgs('AirTerminal:SingleDuct:VAV:NoReheat',TotalArgs,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    CALL GetObjectDefMaxArgs('AirTerminal:SingleDuct:ConstantVolume:Reheat',TotalArgs,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    CALL GetObjectDefMaxArgs('AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan',TotalArgs,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    CALL GetObjectDefMaxArgs('AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat',TotalArgs,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    CALL GetObjectDefMaxArgs('AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat',TotalArgs,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)

    ALLOCATE(Alphas(MaxAlphas))
    Alphas=' '
    ALLOCATE(cAlphaFields(MaxAlphas))
    cAlphaFields=' '
    ALLOCATE(cNumericFields(MaxNums))
    cNumericFields=' '
    ALLOCATE(Numbers(MaxNums))
    Numbers=0.0D0
    ALLOCATE(lAlphaBlanks(MaxAlphas))
    lAlphaBlanks=.true.
    ALLOCATE(lNumericBlanks(MaxNums))
    lNumericBlanks=.true.

      !Start Loading the System Input
      DO SysIndex = 1,  NumVAVSys

        CurrentModuleObject='AirTerminal:SingleDuct:VAV:Reheat'

        CALL GetObjectItem(CurrentModuleObject,SysIndex,Alphas,NumAlphas,Numbers,NumNums,IOSTAT, &
                           AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                           AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)

        SysNum = SysIndex
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(Alphas(1),Sys%SysName,SysNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) Alphas(1)='xxxxx'
        ENDIF
        Sys(SysNum)%SysName     = Alphas(1)
        Sys(SysNum)%SysType     = TRIM(CurrentModuleObject)
        Sys(SysNum)%SysType_Num = SingleDuctVAVReheat
        Sys(SysNum)%ReheatComp  = Alphas(7)
        IF (SameString(Sys(SysNum)%ReheatComp,'Coil:Heating:Gas')) THEN
          Sys(SysNum)%ReheatComp_Num=HCoilType_Gas
        ELSEIF (SameString(Sys(SysNum)%ReheatComp,'Coil:Heating:Electric')) THEN
          Sys(SysNum)%ReheatComp_Num=HCoilType_Electric
        ELSEIF  (SameString(Sys(SysNum)%ReheatComp,'Coil:Heating:Water')) THEN
          Sys(SysNum)%ReheatComp_Num=HCoilType_SimpleHeating
          Sys(SysNum)%ReheatComp_PlantType = TypeOf_CoilWaterSimpleHeating
        ELSEIF  (SameString(Sys(SysNum)%ReheatComp,'Coil:Heating:Steam')) THEN
          Sys(SysNum)%ReheatComp_Num=HCoilType_SteamAirHeating
          Sys(SysNum)%ReheatComp_PlantType = TypeOf_CoilSteamAirHeating
        ELSEIF (Sys(SysNum)%ReheatComp /= ' ') THEN
          CALL ShowSevereError('Illegal '//TRIM(cAlphaFields(8))//' = '//TRIM(Sys(SysNum)%ReheatComp)//'.')
          CALL ShowContinueError('Occurs in '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
          ErrorsFound=.true.
        ENDIF
        Sys(SysNum)%ReheatName     = Alphas(8)
        CALL ValidateComponent(Sys(SysNum)%ReheatComp,Sys(SysNum)%ReheatName,IsNotOK,trim(Sys(SysNum)%SysType))
        IF (IsNotOK) THEN
          CALL ShowContinueError('In '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
          ErrorsFound=.true.
        ENDIF
        Sys(SysNum)%Schedule       = Alphas(2)
        IF (lAlphaBlanks(2)) THEN
          Sys(SysNum)%SchedPtr       = ScheduleAlwaysOn
        ELSE
          Sys(SysNum)%SchedPtr       = GetScheduleIndex(Alphas(2))
          IF (Sys(SysNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(TRIM(cAlphaFields(2))//' = '//TRIM(Alphas(2))//' not found.')
            CALL ShowContinueError('Occurs in '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
            ErrorsFound=.true.
          ENDIF
        ENDIF
          ! For node connections, this object is both a parent and a non-parent, because the
          ! VAV damper is not called out as a separate component, its nodes must be connected
          ! as ObjectIsNotParent.  But for the reheat coil, the nodes are connected as ObjectIsParent
        Sys(SysNum)%OutletNodeNum  = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent,cAlphaFields(3))
        Sys(SysNum)%InletNodeNum   = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent,cAlphaFields(4))
        Sys(SysNum)%MaxAirVolFlowRate = Numbers(1)

        IF (SameString(Alphas(5) , 'Constant')) THEN
          Sys(SysNum)%ZoneMinAirFracMethod = ConstantMinFrac
        ELSEIF (SameString(Alphas(5), 'FixedFlowRate')) THEN
          Sys(SysNum)%ZoneMinAirFracMethod = FixedMin
        ELSEIF (SameString(Alphas(5), 'Scheduled')) THEN
          Sys(SysNum)%ZoneMinAirFracMethod = ScheduledMinFrac
        ELSE
          CALL ShowSevereError(TRIM(cAlphaFields(5))//' = '//TRIM(Alphas(5))//' not found.')
          CALL ShowContinueError('Occurs in '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
          ErrorsFound=.true.
        ENDIF

        IF (.NOT. lNumericBlanks(2)) THEN
          Sys(SysNum)%ZoneMinAirFrac              = Numbers(2)
          Sys(SysNum)%ConstantMinAirFracSetByUser = .TRUE.
          Sys(SysNum)%DesignMinAirFrac            = Numbers(2)
        ENDIF

        IF (.NOT. lNumericBlanks(3)) THEN
          Sys(SysNum)%ZoneFixedMinAir              = Numbers(3)
          Sys(SysNum)%FixedMinAirSetByUser = .TRUE.
          Sys(SysNum)%DesignFixedMinAir            = Numbers(3)
        ENDIF

        Sys(SysNum)%ZoneMinAirFracSchPtr = GetScheduleIndex(Alphas(6))
        IF ((Sys(SysNum)%ZoneMinAirFracSchPtr == 0) .and. &
            ( Sys(SysNum)%ZoneMinAirFracMethod == ScheduledMinFrac) ) THEN
          CALL ShowSevereError(TRIM(cAlphaFields(6))//' = '//TRIM(Alphas(6))//' not found.')
          CALL ShowContinueError('Occurs in '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
          Call ShowContinueError('A valid schedule is required')
          ErrorsFound=.true.
        ELSEIf ((Sys(SysNum)%ZoneMinAirFracSchPtr > 0) .and. &
            ( Sys(SysNum)%ZoneMinAirFracMethod == ScheduledMinFrac) ) THEN
          ! check range of values in schedule
          IF (.NOT. CheckScheduleValueMinMax(Sys(SysNum)%ZoneMinAirFracSchPtr,'>=',0.0d0,'<=',1.0d0)) Then
            CALL ShowSevereError('Error found in '//TRIM(cAlphaFields(6))//' = '//TRIM(Alphas(6)) )
            CAll ShowContinueError('Occurs in '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
            CALL ShowContinueError('Schedule values must be (>=0., <=1.)')
          ENDIF

        ENDIF

        ! The reheat coil control node is necessary for hot water and steam reheat, but not necessary for
        ! electric or gas reheat.
        IF (Sys(SysNum)%ReheatComp_Num .NE. HCoilType_Gas .AND. Sys(SysNum)%ReheatComp_Num .NE. HCoilType_Electric) THEN
          IF  (Sys(SysNum)%ReheatComp_Num .EQ. HCoilType_SteamAirHeating)THEN
            IsNotOK=.false.
            Sys(SysNum)%ReheatControlNode  = GetCoilSteamInletNode(Sys(SysNum)%ReheatComp,Sys(SysNum)%ReheatName,IsNotOK)
            IF (IsNotOK) THEN
              CALL ShowContinueError('..Occurs in '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
              ErrorsFound=.true.
            ENDIF
          ELSE
            IsNotOK=.false.
            Sys(SysNum)%ReheatControlNode  = GetCoilWaterInletNode(Sys(SysNum)%ReheatComp,Sys(SysNum)%ReheatName,IsNotOK)
            IF (IsNotOK) THEN
              CALL ShowContinueError('..Occurs in '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
              ErrorsFound=.true.
            ENDIF
          END IF
        END IF
        Sys(SysNum)%ReheatAirOutletNode  = &
               GetOnlySingleNode(Alphas(9),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent,cAlphaFields(9))
        IF  (Sys(SysNum)%ReheatComp_Num .EQ. HCoilType_SteamAirHeating)THEN
          Sys(SysNum)%MaxReheatSteamVolFlow = Numbers(4)
          Sys(SysNum)%MinReheatSteamVolFlow = Numbers(5)
        ELSE
          Sys(SysNum)%MaxReheatWaterVolFlow = Numbers(4)
          Sys(SysNum)%MinReheatWaterVolFlow = Numbers(5)
        END IF
        Sys(SysNum)%ControllerOffset   = Numbers(6)
        ! Set default convergence tolerance
        IF (Sys(SysNum)%ControllerOffset .LE. 0.0d0) THEN
          Sys(SysNum)%ControllerOffset = 0.001d0
        END IF
        IF (SameString(Alphas(10),'Reverse')) THEN
          Sys(SysNum)%DamperHeatingAction = ReverseAction
        ELSE
          Sys(SysNum)%DamperHeatingAction = Normal
        END IF
        ! Register component set data
        CALL TestCompSet(Sys(SysNum)%SysType,Sys(SysNum)%SysName, &
                         NodeID(Sys(SysNum)%InletNodeNum),NodeID(Sys(SysNum)%ReheatAirOutletNode),'Air Nodes')

        ! Fill the Zone Equipment data with the inlet node number of this unit.
        DO CtrlZone = 1,NumOfZones
          IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
          DO SupAirIn = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
            IF (Sys(SysNum)%ReheatAirOutletNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(SupAirIn)) THEN
              IF (ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode > 0) THEN
                CALL ShowSevereError('Error in connecting a terminal unit to a zone')
                CALL ShowContinueError(TRIM(NodeID(Sys(SysNum)%ReheatAirOutletNode))//' already connects to another zone')
                CALL ShowContinueError('Occurs for terminal unit '//TRIM(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
                CALL ShowContinueError('Check terminal unit node names for errors')
                ErrorsFound = .true.
              ELSE
                ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%InNode = Sys(SysNum)%InletNodeNum
                ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode = Sys(SysNum)%ReheatAirOutletNode
              END IF

              Sys(SysNum)%CtrlZoneNum   = CtrlZone
              Sys(SysNum)%ActualZoneNum = ZoneEquipConfig(CtrlZone)%ActualZoneNum
              Sys(SysNum)%ZoneFloorArea = Zone(Sys(SysNum)%ActualZoneNum)%FloorArea* &
                    Zone(Sys(SysNum)%ActualZoneNum)%Multiplier*Zone(Sys(SysNum)%ActualZoneNum)%ListMultiplier

            END IF
          END DO
        END DO

        IF (.NOT. lNumericBlanks(7)) THEN
          IF(Numbers(7) .EQ. Autocalculate)THEN
             Sys(SysNum)%MaxAirVolFlowRateDuringReheat = Numbers(7)
          ELSE
             Sys(SysNum)%MaxAirVolFlowRateDuringReheat = Numbers(7)*Sys(SysNum)%ZoneFloorArea
          END IF
        END IF

        IF (.NOT. lNumericBlanks(8)) THEN
          Sys(SysNum)%MaxAirVolFractionDuringReheat = Numbers(8)
        END IF

        ! Maximum reheat air temperature, i.e. the maximum supply air temperature leaving the reheat coil
        IF (.NOT. lNumericBlanks(9)) THEN
          Sys(SysNum)%MaxReheatTemp = Numbers(9)
          Sys(SysNum)%MaxReheatTempSetByUser = .TRUE.
        ELSE
          ! user does not specify maximum supply air temperature
          ! Sys(SysNum)%MaxReheatTemp = 35.0D0 !C
          Sys(SysNum)%MaxReheatTempSetByUser = .FALSE.
        ENDIF

        IF(.NOT. lAlphaBlanks(11))THEN
          Sys(SysNum)%OARequirementsPtr = FindItemInList(Alphas(11),OARequirements%Name,NumOARequirements)
          IF(Sys(SysNum)%OARequirementsPtr .EQ. 0)THEN
            CALL ShowSevereError(TRIM(cAlphaFields(11))//' = '//TRIM(Alphas(11))//' not found.')
            CALL ShowContinueError('Occurs in '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
            ErrorsFound=.true.
          ELSE
            Sys(SysNum)%NoOAFlowInputFromUser = .FALSE.
          END IF
        END IF

        CALL ValidateComponent(Alphas(7),Alphas(8),IsNotOK,Sys(SysNum)%SysType)
        IF (IsNotOK) THEN
          CALL ShowContinueError('In '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
          ErrorsFound=.true.
        ENDIF

        ! Add reheat coil to component sets array
        CALL SetUpCompSets(Sys(SysNum)%SysType, Sys(SysNum)%SysName, &
                           Alphas(7),Alphas(8),Alphas(3),Alphas(9))

        !Setup the Average damper Position output variable
        CALL SetupOutputVariable('Zone Air Terminal VAV Damper Position []', Sys(Sysnum)%DamperPosition, &
                              'System','Average',Sys(Sysnum)%SysName)
        CALL SetupOutputVariable('Zone Air Terminal Minimum Air Flow Fraction []', Sys(Sysnum)%ZoneMinAirFrac, &
                              'System','Average',Sys(Sysnum)%SysName)
        CALL SetupOutputVariable('Zone Air Terminal Outdoor Air Volume Flow Rate [m3/s]', Sys(Sysnum)%OutdoorAirFlowRate, &
                              'System','Average',Sys(Sysnum)%SysName)

      END DO   ! end Number of Sys Loop

      DO SysIndex = 1,  NumCBVAVSys

        CurrentModuleObject='AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat'

        CALL GetObjectItem(CurrentModuleObject,SysIndex,Alphas,NumAlphas,Numbers,NumNums,IOSTAT, &
                           AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                           AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)

        SysNum = SysIndex + NumVAVSys
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(Alphas(1),Sys%SysName,SysNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) Alphas(1)='xxxxx'
        ENDIF
        Sys(SysNum)%SysName     = Alphas(1)
        Sys(SysNum)%SysType     = TRIM(CurrentModuleObject)
        Sys(SysNum)%SysType_Num = SingleDuctCBVAVReheat
        Sys(SysNum)%ReheatComp  = Alphas(6)
        IF (SameString(Sys(SysNum)%ReheatComp,'Coil:Heating:Gas')) THEN
          Sys(SysNum)%ReheatComp_Num=HCoilType_Gas
        ELSEIF (SameString(Sys(SysNum)%ReheatComp,'Coil:Heating:Electric')) THEN
          Sys(SysNum)%ReheatComp_Num=HCoilType_Electric
        ELSEIF  (SameString(Sys(SysNum)%ReheatComp,'Coil:Heating:Water')) THEN
          Sys(SysNum)%ReheatComp_Num=HCoilType_SimpleHeating
          Sys(SysNum)%ReheatComp_PlantType = TypeOf_CoilWaterSimpleHeating
        ELSEIF  (SameString(Sys(SysNum)%ReheatComp,'Coil:Heating:Steam')) THEN
          Sys(SysNum)%ReheatComp_Num=HCoilType_SteamAirHeating
          Sys(SysNum)%ReheatComp_PlantType = TypeOf_CoilSteamAirHeating
        ELSEIF (Sys(SysNum)%ReheatComp /= ' ') THEN
          CALL ShowSevereError('Illegal '//TRIM(cAlphaFields(6))//' = '//TRIM(Sys(SysNum)%ReheatComp)//'.')
          CALL ShowContinueError('Occurs in '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
          ErrorsFound=.true.
        ENDIF
        Sys(SysNum)%ReheatName     = Alphas(7)
        CALL ValidateComponent(Sys(SysNum)%ReheatComp,Sys(SysNum)%ReheatName,IsNotOK,Sys(SysNum)%SysType)
        IF (IsNotOK) THEN
          CALL ShowContinueError('In '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
          ErrorsFound=.true.
        ENDIF
        Sys(SysNum)%Schedule       = Alphas(2)
        IF (lAlphaBlanks(2)) THEN
          Sys(SysNum)%SchedPtr       = ScheduleAlwaysOn
        ELSE
          Sys(SysNum)%SchedPtr       = GetScheduleIndex(Alphas(2))
          IF (Sys(SysNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(TRIM(cAlphaFields(2))//' = '//TRIM(Alphas(2))//' not found.')
            CALL ShowContinueError('Occurs in '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
            ErrorsFound=.true.
          ENDIF
        ENDIF
          ! For node connections, this object is both a parent and a non-parent, because the
          ! VAV damper is not called out as a separate component, its nodes must be connected
          ! as ObjectIsNotParent.  But for the reheat coil, the nodes are connected as ObjectIsParent
        Sys(SysNum)%OutletNodeNum  = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent,cAlphaFields(3))
        Sys(SysNum)%InletNodeNum   = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent,cAlphaFields(4))
        Sys(SysNum)%MaxAirVolFlowRate = Numbers(1)
        Sys(SysNum)%ZoneMinAirFrac    = Numbers(2)
        IF (Sys(SysNum)%ZoneMinAirFrac .LT. 0.0d0) THEN
          CALL ShowWarningError(trim(Sys(SysNum)%SysType)//' "'//TRIM(Sys(SysNum)%SysName)//'"')
          CALL ShowContinueError(TRIM(cNumericFields(2))//' must be greater than or equal to 0. Resetting to 0 and' &
                                 //' the simulation continues.')
          Sys(SysNum)%ZoneMinAirFrac = 0.0d0
        END IF
        IF (Sys(SysNum)%ZoneMinAirFrac .GT. 1.0d0) THEN
          CALL ShowWarningError(trim(Sys(SysNum)%SysType)//' "'//TRIM(Sys(SysNum)%SysName)//'"')
          CALL ShowContinueError(TRIM(cNumericFields(2))//' must be less than or equal to 1. Resetting to 1 and' &
                                 //' the simulation continues.')
          Sys(SysNum)%ZoneMinAirFrac = 1.0d0
        END IF
        ! The reheat coil control node is necessary for hot water and steam reheat, but not necessary for
        ! electric or gas reheat.
        IF (Sys(SysNum)%ReheatComp_Num .EQ. HCoilType_Gas .OR. Sys(SysNum)%ReheatComp_Num .EQ. HCoilType_Electric) THEN
!          IF(.NOT. lAlphaBlanks(5)) THEN
!            CALL ShowWarningError('In '//trim(Sys(SysNum)%SysType)//' = ' //TRIM(Sys(SysNum)%SysName) &
!                                 // ' the '//TRIM(cAlphaFields(5))//' is not needed and will be ignored.')
!            CALL ShowContinueError('  It is used for hot water and steam reheat coils only.')
!          END IF
        ELSE
!          IF(lAlphaBlanks(5)) THEN
!            CALL ShowSevereError('In '//trim(Sys(SysNum)%SysType)//' = ' //TRIM(Sys(SysNum)%SysName) &
!                                 // ' the '//TRIM(cAlphaFields(5))//' is undefined.')
!            ErrorsFound=.true.
!          ELSE
            IF  (Sys(SysNum)%ReheatComp_Num .EQ. HCoilType_SteamAirHeating)THEN
              IsNotOK=.false.
              Sys(SysNum)%ReheatControlNode  = GetCoilSteamInletNode(Sys(SysNum)%ReheatComp,Sys(SysNum)%ReheatName,IsNotOK)
              IF (IsNotOK) THEN
                CALL ShowContinueError('..Occurs in '//trim(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName))
                ErrorsFound=.true.
              ENDIF
!                GetOnlySingleNode(Alphas(5),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
!                              NodeType_Steam,NodeConnectionType_Actuator,1,ObjectIsParent)
            ELSE
              IsNotOK=.false.
              Sys(SysNum)%ReheatControlNode  = GetCoilWaterInletNode(Sys(SysNum)%ReheatComp,Sys(SysNum)%ReheatName,IsNotOK)
              IF (IsNotOK) THEN
                CALL ShowContinueError('..Occurs in '//trim(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName))
                ErrorsFound=.true.
              ENDIF
!                GetOnlySingleNode(Alphas(5),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
!                              NodeType_Water,NodeConnectionType_Actuator,1,ObjectIsParent)
            END IF
        !  END IF
        END IF
        Sys(SysNum)%ReheatAirOutletNode  = &
               GetOnlySingleNode(Alphas(8),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent,cAlphaFields(8))
        IF  (Sys(SysNum)%ReheatComp_Num .EQ. HCoilType_SteamAirHeating)THEN
          Sys(SysNum)%MaxReheatSteamVolFlow = Numbers(3)
          Sys(SysNum)%MinReheatSteamVolFlow = Numbers(4)
        ELSE
          Sys(SysNum)%MaxReheatWaterVolFlow = Numbers(3)
          Sys(SysNum)%MinReheatWaterVolFlow = Numbers(4)
        END IF
        Sys(SysNum)%ControllerOffset   = Numbers(5)
        ! Set default convergence tolerance
        IF (Sys(SysNum)%ControllerOffset .LE. 0.0d0) THEN
          Sys(SysNum)%ControllerOffset = 0.001d0
        END IF

        Sys(SysNum)%DamperHeatingAction = ReverseAction

        ! Register component set data
        CALL TestCompSet(Sys(SysNum)%SysType,Sys(SysNum)%SysName, &
                         NodeID(Sys(SysNum)%InletNodeNum),NodeID(Sys(SysNum)%ReheatAirOutletNode),'Air Nodes')

        ! Fill the Zone Equipment data with the inlet node number of this unit
        DO CtrlZone = 1,NumOfZones
          IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
          DO SupAirIn = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
            IF (Sys(SysNum)%ReheatAirOutletNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(SupAirIn)) THEN
              IF (ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode > 0) THEN
                CALL ShowSevereError('Error in connecting a terminal unit to a zone')
                CALL ShowContinueError(TRIM(NodeID(Sys(SysNum)%ReheatAirOutletNode))//' already connects to another zone')
                CALL ShowContinueError('Occurs for terminal unit '//TRIM(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
                CALL ShowContinueError('Check terminal unit node names for errors')
                ErrorsFound = .true.
              ELSE
                ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%InNode = Sys(SysNum)%InletNodeNum
                ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode = Sys(SysNum)%ReheatAirOutletNode
              END IF
            END IF
          END DO
        END DO

       IF (.NOT. lNumericBlanks(6)) THEN
         Sys(SysNum)%MaxReheatTemp = Numbers(6)
         Sys(SysNum)%MaxReheatTempSetByUser = .TRUE.
       ELSE
          ! user does not specify maximum supply air temperature
          ! Sys(SysNum)%MaxReheatTemp = 35.0D0 !C
          Sys(SysNum)%MaxReheatTempSetByUser = .FALSE.
        ENDIF

        CALL ValidateComponent(Alphas(6),Alphas(7),IsNotOK,Sys(SysNum)%SysType)
        IF (IsNotOK) THEN
          CALL ShowContinueError('In '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
          ErrorsFound=.true.
        ENDIF

        ! Add reheat coil to component sets array
        CALL SetUpCompSets(Sys(SysNum)%SysType, Sys(SysNum)%SysName, &
                           Alphas(6),Alphas(7),Alphas(3),Alphas(8))

        !Setup the Average damper Position output variable
        CALL SetupOutputVariable('Zone Air Terminal VAV Damper Position []', Sys(Sysnum)%DamperPosition, &
                              'System','Average',Sys(Sysnum)%SysName)

      END DO   ! end Number of VAVHeatandCool Sys Loop

      CurrentModuleObject='AirTerminal:SingleDuct:ConstantVolume:Reheat'

      DO SysIndex = 1,  NumConstVolSys

        CALL GetObjectItem(CurrentModuleObject,SysIndex,Alphas,NumAlphas,Numbers,NumNums,IOSTAT, &
                           AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                           AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)

        SysNum = SysIndex + NumVAVSys + NumCBVAVSys
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(Alphas(1),Sys%SysName,SysNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) Alphas(1)='xxxxx'
        ENDIF
        Sys(SysNum)%SysName     = Alphas(1)
        Sys(SysNum)%SysType     = CurrentModuleObject
        Sys(SysNum)%SysType_Num = SingleDuctConstVolReheat
        Sys(SysNum)%ReheatComp  = Alphas(6)
        IF (SameString(Sys(SysNum)%ReheatComp,'Coil:Heating:Gas')) THEN
          Sys(SysNum)%ReheatComp_Num=HCoilType_Gas
        ELSEIF (SameString(Sys(SysNum)%ReheatComp,'Coil:Heating:Electric')) THEN
          Sys(SysNum)%ReheatComp_Num=HCoilType_Electric
        ELSEIF  (SameString(Sys(SysNum)%ReheatComp,'Coil:Heating:Water')) THEN
          Sys(SysNum)%ReheatComp_Num=HCoilType_SimpleHeating
          Sys(SysNum)%ReheatComp_PlantType = TypeOf_CoilWaterSimpleHeating
        ELSEIF  (SameString(Sys(SysNum)%ReheatComp,'Coil:Heating:Steam')) THEN
          Sys(SysNum)%ReheatComp_Num=HCoilType_SteamAirHeating
          Sys(SysNum)%ReheatComp_PlantType = TypeOf_CoilSteamAirHeating
        ELSE
          CALL ShowSevereError('Illegal '//TRIM(cAlphaFields(6))//' = '//TRIM(Sys(SysNum)%ReheatComp)//'.')
          CALL ShowContinueError('Occurs in '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
          ErrorsFound=.true.
        ENDIF
        Sys(SysNum)%ReheatName     = Alphas(7)
        CALL ValidateComponent(Sys(SysNum)%ReheatComp,Sys(SysNum)%ReheatName,IsNotOK,Sys(SysNum)%SysType)
        IF (IsNotOK) THEN
          CALL ShowContinueError('In '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
          ErrorsFound=.true.
        ENDIF
        Sys(SysNum)%Schedule       = Alphas(2)
        IF (lAlphaBlanks(2)) THEN
          Sys(SysNum)%SchedPtr       = ScheduleAlwaysOn
        ELSE
          Sys(SysNum)%SchedPtr       = GetScheduleIndex(Alphas(2))
          IF (Sys(SysNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(TRIM(cAlphaFields(2))//' = '//TRIM(Alphas(2))//' not found.')
            CALL ShowContinueError('Occurs in '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
            ErrorsFound=.true.
          ENDIF
        ENDIF
        Sys(SysNum)%OutletNodeNum  = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent,cAlphaFields(3))
        Sys(SysNum)%InletNodeNum   = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent,cAlphaFields(4))
        ! The reheat coil control node is necessary for hot water reheat, but not necessary for
        ! electric or gas reheat.
        IF (Sys(SysNum)%ReheatComp_Num .EQ. HCoilType_Gas .OR. Sys(SysNum)%ReheatComp_Num .EQ. HCoilType_Electric) THEN
!          IF(.NOT. lAlphaBlanks(5)) THEN
!            CALL ShowWarningError('In '//trim(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName) &
!                                 // ' the '//TRIM(cAlphaFields(5))//' is not needed and will be ignored.')
!            CALL ShowContinueError('  It is used for hot water reheat coils only.')
!          END IF
        ELSE
!          IF(lAlphaBlanks(5)) THEN
!            CALL ShowSevereError('In '//trim(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName) &
!                                 // ' the '//TRIM(cAlphaFields(5))//' is undefined.')
!            ErrorsFound=.true.
!          END IF
          IF  (Sys(SysNum)%ReheatComp_Num .EQ. HCoilType_SteamAirHeating)THEN
            IsNotOK=.false.
            Sys(SysNum)%ReheatControlNode  = GetCoilSteamInletNode(Sys(SysNum)%ReheatComp,Sys(SysNum)%ReheatName,IsNotOK)
            IF (IsNotOK) THEN
              CALL ShowContinueError('..Occurs in '//trim(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName))
              ErrorsFound=.true.
            ENDIF
!                 GetOnlySingleNode(Alphas(5),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
!                               NodeType_Steam,NodeConnectionType_Actuator,1,ObjectIsParent)
          Else
            IsNotOK=.false.
            Sys(SysNum)%ReheatControlNode  = GetCoilWaterInletNode(Sys(SysNum)%ReheatComp,Sys(SysNum)%ReheatName,IsNotOK)
            IF (IsNotOK) THEN
              CALL ShowContinueError('..Occurs in '//trim(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName))
              ErrorsFound=.true.
            ENDIF
!                 GetOnlySingleNode(Alphas(5),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
!                               NodeType_Water,NodeConnectionType_Actuator,1,ObjectIsParent)
          EndIF
        END IF
        Sys(SysNum)%ReheatAirOutletNode= Sys(SysNum)%OutletNodeNum
        Sys(SysNum)%MaxAirVolFlowRate     = Numbers(1)
        Sys(SysNum)%ZoneMinAirFrac        = 0.0D0
        IF  (Sys(SysNum)%ReheatComp_Num .EQ. HCoilType_SteamAirHeating)THEN
          Sys(SysNum)%MaxReheatSteamVolFlow = Numbers(2)
          Sys(SysNum)%MinReheatSteamVolFlow = Numbers(3)
        Else
          Sys(SysNum)%MaxReheatWaterVolFlow = Numbers(2)
          Sys(SysNum)%MinReheatWaterVolFlow = Numbers(3)
        END IF
        Sys(SysNum)%ControllerOffset   = Numbers(4)
        ! Set default convergence tolerance
        IF (Sys(SysNum)%ControllerOffset .LE. 0.0d0) THEN
          Sys(SysNum)%ControllerOffset = 0.001d0
        END IF

        ! Maximum reheat air temperature, i.e. the maximum supply air temperature leaving the reheat coil
        IF (.NOT. lNumericBlanks(5)) THEN
          Sys(SysNum)%MaxReheatTemp = Numbers(5)
          Sys(SysNum)%MaxReheatTempSetByUser = .TRUE.
        ELSE
          ! user does not specify maximum supply air temperature
          ! Sys(SysNum)%MaxReheatTemp = 35.0D0 !C
          Sys(SysNum)%MaxReheatTempSetByUser = .FALSE.
        ENDIF
        ! Register component set data
        CALL TestCompSet(Sys(SysNum)%SysType,Sys(SysNum)%SysName, &
                         NodeID(Sys(SysNum)%InletNodeNum),NodeID(Sys(SysNum)%OutletNodeNum),'Air Nodes')

        ! Fill the Zone Equipment data with the inlet node number of this unit.
        DO CtrlZone = 1,NumOfZones
          IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
          DO SupAirIn = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
            IF (Sys(SysNum)%OutletNodeNum .EQ. ZoneEquipConfig(CtrlZone)%InletNode(SupAirIn)) THEN
              IF (ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode > 0) THEN
                CALL ShowSevereError('Error in connecting a terminal unit to a zone')
                CALL ShowContinueError(TRIM(NodeID(Sys(SysNum)%OutletNodeNum))//' already connects to another zone')
                CALL ShowContinueError('Occurs for terminal unit '//TRIM(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
                CALL ShowContinueError('Check terminal unit node names for errors')
                ErrorsFound = .true.
              ELSE
                ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%InNode = Sys(SysNum)%InletNodeNum
                ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode = Sys(SysNum)%OutletNodeNum
              END IF
            END IF
          END DO
        END DO

        CALL ValidateComponent(Alphas(6),Alphas(7),IsNotOK,Sys(SysNum)%SysType)
        IF (IsNotOK) THEN
          CALL ShowContinueError('In '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
          ErrorsFound=.true.
        ENDIF

        ! Add reheat coil to component sets array
        CALL SetUpCompSets(Sys(SysNum)%SysType, Sys(SysNum)%SysName, &
                           Alphas(6),Alphas(7),Alphas(4),Alphas(3))

        !Setup the Average damper Position output variable
        ! BG removed 9-10-2009 during work on CR 7770, constant volume has no damper
      !  CALL SetupOutputVariable('Damper Position', Sys(Sysnum)%DamperPosition, &
      !                        'System','Average',Sys(Sysnum)%SysName)

      END DO   ! End Number of Sys Loop


      DO SysIndex = 1,  NumNoRHVAVSys

        CurrentModuleObject='AirTerminal:SingleDuct:VAV:NoReheat'

        CALL GetObjectItem(CurrentModuleObject,SysIndex,Alphas,NumAlphas,Numbers,NumNums,IOSTAT, &
                           AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                           AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)

        SysNum = SysIndex + NumVAVSys  + NumCBVAVSys + NumConstVolSys
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(Alphas(1),Sys%SysName,SysNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) Alphas(1)='xxxxx'
        ENDIF
        Sys(SysNum)%SysName     = Alphas(1)
        Sys(SysNum)%SysType     = CurrentModuleObject
        Sys(SysNum)%SysType_Num = SingleDuctVAVNoReheat
        Sys(SysNum)%ReheatComp  = ' '
        Sys(SysNum)%ReheatName  = ' '
        Sys(SysNum)%Schedule    = Alphas(2)
        IF (lAlphaBlanks(2)) THEN
          Sys(SysNum)%SchedPtr       = ScheduleAlwaysOn
        ELSE
          Sys(SysNum)%SchedPtr       = GetScheduleIndex(Alphas(2))
          IF (Sys(SysNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(TRIM(cAlphaFields(2))//' = '//TRIM(Alphas(2))//' not found.')
            CALL ShowContinueError('Occurs in '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
            ErrorsFound=.true.
          ENDIF
        ENDIF
        Sys(SysNum)%OutletNodeNum  = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent,cAlphaFields(3))
        Sys(SysNum)%InletNodeNum   = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent,cAlphaFields(4))
        Sys(SysNum)%MaxAirVolFlowRate  = Numbers(1)

        IF (SameString(Alphas(5) , 'Constant')) THEN
          Sys(SysNum)%ZoneMinAirFracMethod = ConstantMinFrac
        ELSEIF (SameString(Alphas(5), 'FixedFlowRate')) THEN
          Sys(SysNum)%ZoneMinAirFracMethod = FixedMin
        ELSEIF (SameString(Alphas(5), 'Scheduled')) THEN
          Sys(SysNum)%ZoneMinAirFracMethod = ScheduledMinFrac
        ELSE
          CALL ShowSevereError(TRIM(cAlphaFields(5))//' = '//TRIM(Alphas(5))//' not found.')
          CALL ShowContinueError('Occurs in '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
          ErrorsFound=.true.
        ENDIF

        IF (.NOT. lNumericBlanks(2)) THEN
          Sys(SysNum)%ZoneMinAirFrac              = Numbers(2)
          Sys(SysNum)%ConstantMinAirFracSetByUser = .TRUE.
          Sys(SysNum)%DesignMinAirFrac            = Numbers(2)
        ENDIF

        IF (.NOT. lNumericBlanks(3)) THEN
          Sys(SysNum)%ZoneFixedMinAir              = Numbers(3)
          Sys(SysNum)%FixedMinAirSetByUser = .TRUE.
          Sys(SysNum)%DesignFixedMinAir            = Numbers(3)
        ENDIF

        Sys(SysNum)%ZoneMinAirFracSchPtr = GetScheduleIndex(Alphas(6))
        IF ((Sys(SysNum)%ZoneMinAirFracSchPtr == 0) .and. &
            ( Sys(SysNum)%ZoneMinAirFracMethod == ScheduledMinFrac) ) THEN
          CALL ShowSevereError(TRIM(cAlphaFields(6))//' = '//TRIM(Alphas(6))//' not found.')
          CALL ShowContinueError('Occurs in '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
          Call ShowContinueError('A valid schedule is required')
          ErrorsFound=.true.
        ELSEIf ((Sys(SysNum)%ZoneMinAirFracSchPtr > 0) .and. &
            ( Sys(SysNum)%ZoneMinAirFracMethod == ScheduledMinFrac) ) THEN
          ! check range of values in schedule
          IF (.NOT. CheckScheduleValueMinMax(Sys(SysNum)%ZoneMinAirFracSchPtr,'>=',0.0d0,'<=',1.0d0)) Then
            CALL ShowSevereError('Error found in '//TRIM(cAlphaFields(6))//' = '//TRIM(Alphas(6)) )
            CAll ShowContinueError('Occurs in '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
            CALL ShowContinueError('Schedule values must be (>=0., <=1.)')
          ENDIF

        ENDIF

        Sys(SysNum)%ReheatControlNode  = 0
        Sys(SysNum)%ReheatAirOutletNode   = Sys(SysNum)%OutletNodeNum
        Sys(SysNum)%MaxReheatWaterVolFlow = 0.0d0
        Sys(SysNum)%MaxReheatSteamVolFlow = 0.0d0
        Sys(SysNum)%MinReheatWaterVolFlow = 0.0d0
        Sys(SysNum)%MinReheatSteamVolFlow = 0.0d0
        Sys(SysNum)%ControllerOffset    = 0.000001d0
        Sys(SysNum)%DamperHeatingAction = Normal

        ! Register component set data
        CALL TestCompSet(Sys(SysNum)%SysType,Sys(SysNum)%SysName, &
                         NodeID(Sys(SysNum)%InletNodeNum),NodeID(Sys(SysNum)%OutletNodeNum),'Air Nodes')

        ! Fill the Zone Equipment data with the inlet node number of this unit.
        DO CtrlZone = 1,NumOfZones
          IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
          DO SupAirIn = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
            IF (Sys(SysNum)%ReheatAirOutletNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(SupAirIn)) THEN
              IF (ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode > 0) THEN
                CALL ShowSevereError('Error in connecting a terminal unit to a zone')
                CALL ShowContinueError(TRIM(NodeID(Sys(SysNum)%ReheatAirOutletNode))//' already connects to another zone')
                CALL ShowContinueError('Occurs for terminal unit '//TRIM(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
                CALL ShowContinueError('Check terminal unit node names for errors')
                ErrorsFound = .true.
              ELSE
                ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%InNode = Sys(SysNum)%InletNodeNum
                ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode = Sys(SysNum)%ReheatAirOutletNode
              END IF

              Sys(SysNum)%CtrlZoneNum   = CtrlZone
              Sys(SysNum)%ActualZoneNum = ZoneEquipConfig(CtrlZone)%ActualZoneNum
              Sys(SysNum)%ZoneFloorArea = Zone(Sys(SysNum)%ActualZoneNum)%FloorArea* &
                    Zone(Sys(SysNum)%ActualZoneNum)%Multiplier*Zone(Sys(SysNum)%ActualZoneNum)%ListMultiplier

            END IF
          END DO
        END DO

        IF(.NOT. lAlphaBlanks(7))THEN
          Sys(SysNum)%OARequirementsPtr = FindItemInList(Alphas(7),OARequirements%Name,NumOARequirements)
          IF(Sys(SysNum)%OARequirementsPtr .EQ. 0)THEN
            CALL ShowSevereError(TRIM(cAlphaFields(7))//' = '//TRIM(Alphas(7))//' not found.')
            CALL ShowContinueError('Occurs in '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
            ErrorsFound=.true.
          ELSE
            Sys(SysNum)%NoOAFlowInputFromUser = .FALSE.
          END IF
        END IF

        !Setup the Average damper Position output variable
        CALL SetupOutputVariable('Zone Air Terminal VAV Damper Position []', Sys(Sysnum)%DamperPosition, &
                              'System','Average',Sys(Sysnum)%SysName)
        CALL SetupOutputVariable('Zone Air Terminal Outdoor Air Volume Flow Rate [m3/s]', Sys(Sysnum)%OutdoorAirFlowRate, &
                              'System','Average',Sys(Sysnum)%SysName)

      END DO   ! end Number of Sys Loop

      DO SysIndex = 1,  NumNoRHCBVAVSys

        CurrentModuleObject='AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat'

        CALL GetObjectItem(CurrentModuleObject,SysIndex,Alphas,NumAlphas,Numbers,NumNums,IOSTAT, &
                           AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                           AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)

        SysNum = SysIndex + NumVAVSys  + NumCBVAVSys + NumConstVolSys + NumNoRHVAVSys
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(Alphas(1),Sys%SysName,SysNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) Alphas(1)='xxxxx'
        ENDIF
        Sys(SysNum)%SysName     = Alphas(1)
        Sys(SysNum)%SysType     = TRIM(CurrentModuleObject)
        Sys(SysNum)%SysType_Num = SingleDuctCBVAVNoReheat
        Sys(SysNum)%ReheatComp  = ' '
        Sys(SysNum)%ReheatName  = ' '
        Sys(SysNum)%Schedule    = Alphas(2)
        IF (lAlphaBlanks(2)) THEN
          Sys(SysNum)%SchedPtr       = ScheduleAlwaysOn
        ELSE
          Sys(SysNum)%SchedPtr       = GetScheduleIndex(Alphas(2))
          IF (Sys(SysNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(TRIM(cAlphaFields(2))//' = '//TRIM(Alphas(2))//' not found.')
            CALL ShowContinueError('Occurs in '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
            ErrorsFound=.true.
          ENDIF
        ENDIF
        Sys(SysNum)%OutletNodeNum  = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent,cAlphaFields(3))
        Sys(SysNum)%InletNodeNum   = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent,cAlphaFields(4))
        Sys(SysNum)%MaxAirVolFlowRate = Numbers(1)
        Sys(SysNum)%ZoneMinAirFrac    = Numbers(2)
        IF (Sys(SysNum)%ZoneMinAirFrac .LT. 0.0d0) THEN
          CALL ShowWarningError(trim(Sys(SysNum)%SysType)//' = "'//TRIM(Sys(SysNum)%SysName))
          CALL ShowContinueError(TRIM(cNumericFields(2))//' must be greater than or equal to 0. Resetting to 0 and' &
                                 //' the simulation continues.')
          Sys(SysNum)%ZoneMinAirFrac = 0.0d0
        END IF
        IF (Sys(SysNum)%ZoneMinAirFrac .GT. 1.0d0) THEN
          CALL ShowWarningError(trim(Sys(SysNum)%SysType)//' = "'//TRIM(Sys(SysNum)%SysName))
          CALL ShowContinueError(TRIM(cNumericFields(2))//' must be less than or equal to 1. Resetting to 1 and' &
                                 //' the simulation continues.')
          Sys(SysNum)%ZoneMinAirFrac = 1.0d0
        END IF

        Sys(SysNum)%ReheatControlNode  = 0
        Sys(SysNum)%ReheatAirOutletNode  = Sys(SysNum)%OutletNodeNum
        Sys(SysNum)%MaxReheatWaterVolFlow = 0.0d0
        Sys(SysNum)%MaxReheatSteamVolFlow = 0.0d0
        Sys(SysNum)%MinReheatWaterVolFlow = 0.0d0
        Sys(SysNum)%MinReheatSteamVolFlow = 0.0d0
        Sys(SysNum)%ControllerOffset   = 0.000001d0
        Sys(SysNum)%DamperHeatingAction = ReverseAction

        ! Register component set data
        CALL TestCompSet(Sys(SysNum)%SysType,Sys(SysNum)%SysName, &
                         NodeID(Sys(SysNum)%InletNodeNum),NodeID(Sys(SysNum)%OutletNodeNum),'Air Nodes')

        ! Fill the Zone Equipment data with the inlet node number of this unit.
        DO CtrlZone = 1,NumOfZones
          IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
          DO SupAirIn = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
            IF (Sys(SysNum)%ReheatAirOutletNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(SupAirIn)) THEN
               IF (ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode > 0) THEN
                CALL ShowSevereError('Error in connecting a terminal unit to a zone')
                CALL ShowContinueError(TRIM(NodeID(Sys(SysNum)%ReheatAirOutletNode))//' already connects to another zone')
                CALL ShowContinueError('Occurs for terminal unit '//TRIM(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
                CALL ShowContinueError('Check terminal unit node names for errors')
                ErrorsFound = .true.
              ELSE
                ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%InNode = Sys(SysNum)%InletNodeNum
                ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode = Sys(SysNum)%ReheatAirOutletNode
              END IF
            END IF
          END DO
        END DO

        !Setup the Average damper Position output variable
        CALL SetupOutputVariable('Zone Air Terminal VAV Damper Position []', Sys(Sysnum)%DamperPosition, &
                              'System','Average',Sys(Sysnum)%SysName)

      END DO   ! end Number of VAVHeatandCool:NoReheat Sys Loop

      ! read in the SINGLE DUCT:VAV:REHEAT:VS FAN data
      DO SysIndex = 1,  NumVAVVS

        CurrentModuleObject='AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan'

        CALL GetObjectItem(CurrentModuleObject,SysIndex,Alphas,NumAlphas,Numbers,NumNums,IOSTAT, &
                           AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                           AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)

        SysNum = SysIndex + NumVAVSys + NumCBVAVSys + NumConstVolSys + NumNoRHVAVSys + NumNoRHCBVAVSys
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(Alphas(1),Sys%SysName,SysNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) Alphas(1)='xxxxx'
        ENDIF
        Sys(SysNum)%SysName     = Alphas(1)
        Sys(SysNum)%SysType     = TRIM(CurrentModuleObject)
        Sys(SysNum)%SysType_Num = SingleDuctVAVReheatVSFan
        Sys(SysNum)%ReheatComp  = Alphas(9)
        Sys(SysNum)%ReheatName  = Alphas(10)
        IsNotOK = .FALSE.
        IF (SameString(Sys(SysNum)%ReheatComp,'Coil:Heating:Gas')) THEN
          Sys(SysNum)%ReheatComp_Num=HCoilType_Gas
          Sys(SysNum)%ReheatAirOutletNode = &
               GetHeatingCoilOutletNode(Sys(SysNum)%ReheatComp,Sys(SysNum)%ReheatName,IsNotOK)
          Sys(SysNum)%ReheatCoilMaxCapacity = &
               GetHeatingCoilCapacity(Sys(SysNum)%ReheatComp,Sys(SysNum)%ReheatName,IsNotOK)
          IF(IsNotOK) CALL ShowContinueError('Occurs for terminal unit '// &
                           TRIM(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
        ELSEIF (SameString(Sys(SysNum)%ReheatComp,'Coil:Heating:Electric')) THEN
          Sys(SysNum)%ReheatComp_Num=HCoilType_Electric
          Sys(SysNum)%ReheatAirOutletNode = &
               GetHeatingCoilOutletNode(Sys(SysNum)%ReheatComp,Sys(SysNum)%ReheatName,IsNotOK)
          Sys(SysNum)%ReheatCoilMaxCapacity = &
               GetHeatingCoilCapacity(Sys(SysNum)%ReheatComp,Sys(SysNum)%ReheatName,IsNotOK)
          IF(IsNotOK) CALL ShowContinueError('Occurs for terminal unit '// &
                           TRIM(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
        ELSEIF  (SameString(Sys(SysNum)%ReheatComp,'Coil:Heating:Water')) THEN
          Sys(SysNum)%ReheatComp_Num=HCoilType_SimpleHeating
          Sys(SysNum)%ReheatComp_PlantType = TypeOf_CoilWaterSimpleHeating
        ELSEIF  (SameString(Sys(SysNum)%ReheatComp,'Coil:Heating:Steam')) THEN
          Sys(SysNum)%ReheatComp_Num=HCoilType_SteamAirHeating
          Sys(SysNum)%ReheatComp_PlantType = TypeOf_CoilSteamAirHeating
        ELSEIF (Sys(SysNum)%ReheatComp /= ' ') THEN
          CALL ShowSevereError('Illegal '//TRIM(cAlphaFields(9))//' = '//TRIM(Sys(SysNum)%ReheatComp)//'.')
          CALL ShowContinueError('Occurs in '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
          ErrorsFound=.true.
        ENDIF
        CALL ValidateComponent(Sys(SysNum)%ReheatComp,Sys(SysNum)%ReheatName,IsNotOK,Sys(SysNum)%SysType)
        IF (IsNotOK) THEN
          CALL ShowContinueError('In '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
          ErrorsFound=.true.
        ENDIF
        Sys(SysNum)%FanType = Alphas(7)
        IF (SameString(Sys(SysNum)%FanType,'Fan:VariableVolume')) THEN
          Sys(SysNum)%Fan_Num=FanType_VS
        ELSEIF (Sys(SysNum)%FanType /= ' ') THEN
          CALL ShowSevereError('Illegal '//TRIM(cAlphaFields(7))//' = '//TRIM(Sys(SysNum)%FanType)//'.')
          CALL ShowContinueError('Occurs in '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
          ErrorsFound=.true.
        ENDIF
        Sys(SysNum)%FanName = Alphas(8)
        CALL ValidateComponent(Sys(SysNum)%FanType,Sys(SysNum)%FanName,IsNotOK,Sys(SysNum)%SysType)
        IF (IsNotOK) THEN
          CALL ShowContinueError('In '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
          ErrorsFound=.true.
        ENDIF

        Sys(SysNum)%Schedule       = Alphas(2)
        IF (lAlphaBlanks(2)) THEN
          Sys(SysNum)%SchedPtr       = ScheduleAlwaysOn
        ELSE
          Sys(SysNum)%SchedPtr       = GetScheduleIndex(Alphas(2))
          IF (Sys(SysNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(TRIM(cAlphaFields(2))//' = '//TRIM(Alphas(2))//' not found.')
            CALL ShowContinueError('Occurs in '//trim(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
            ErrorsFound=.true.
          ENDIF
        ENDIF

!  A5,     \field heating coil air inlet node
!          \note same as fan outlet node
!          \type alpha
!          \required-field
        IsNotOK=.false.

        Sys(SysNum)%OutletNodeNum  = GetFanOutLetNode(Sys(SysNum)%FanType,Sys(SysNum)%FanName,IsNotOK)
        IF (IsNotOK) THEN
          CALL ShowContinueError('..Occurs in '//trim(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName))
          ErrorsFound=.true.
        ENDIF
!               GetOnlySingleNode(Alphas(5),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
!                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)
!  A3,     \field Unit supply air inlet node
!          \note same as fan inlet node
!          \type alpha
        IsNotOK=.false.
        Sys(SysNum)%InletNodeNum   = GetFanInletNode(Sys(SysNum)%FanType,Sys(SysNum)%FanName,IsNotOK)
        IF (IsNotOK) THEN
          CALL ShowContinueError('..Occurs in '//trim(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName))
          ErrorsFound=.true.
        ENDIF
!               GetOnlySingleNode(Alphas(3),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
!                           NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)
        Sys(SysNum)%MaxAirVolFlowRate     = Numbers(1)
        Sys(SysNum)%MaxHeatAirVolFlowRate = Numbers(2)
        Sys(SysNum)%ZoneMinAirFrac        = Numbers(3)
        ! The reheat coil control node is necessary for hot water reheat, but not necessary for
        ! electric or gas reheat.
        IF (Sys(SysNum)%ReheatComp_Num .EQ. HCoilType_Gas .OR. Sys(SysNum)%ReheatComp_Num .EQ. HCoilType_Electric) THEN
!          IF(.NOT. lAlphaBlanks(6)) THEN
!            CALL ShowWarningError('In '//trim(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName) &
!                                 // ' the '//TRIM(cAlphaFields(6))//' is not needed and will be ignored.')
!            CALL ShowContinueError('  It is used for hot water reheat coils only.')
!          END IF
        ELSE
!          IF(lAlphaBlanks(6)) THEN
!            CALL ShowSevereError('In '//trim(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName) &
!                                 // ' the '//TRIM(cAlphaFields(6))//' is undefined')
!            ErrorsFound=.true.
!          END IF
          IF  (Sys(SysNum)%ReheatComp_Num .EQ. HCoilType_SteamAirHeating)THEN
             IsNotOK=.false.
             Sys(SysNum)%ReheatControlNode  = GetCoilSteamInletNode(Sys(SysNum)%ReheatComp,Sys(SysNum)%ReheatName,IsNotOK)
             IF (IsNotOK) THEN
               CALL ShowContinueError('..Occurs in '//trim(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName))
               ErrorsFound=.true.
             ELSE
!  A4,     \field Unit supply air outlet node
!          \note same as heating coil air outlet node
!          \note same as zone inlet node
!          \type alpha
               IsNotOK=.false.
               Sys(SysNum)%ReheatAirOutletNode  = GetCoilAirOutletNode(Sys(SysNum)%ReheatComp,Sys(SysNum)%ReheatName,IsNotOK)
               IF (IsNotOK) THEN
                 CALL ShowContinueError('..Occurs in '//trim(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName))
                 ErrorsFound=.true.
               ENDIF
             ENDIF
!               GetOnlySingleNode(Alphas(6),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
!                                NodeType_Steam,NodeConnectionType_Actuator,1,ObjectIsParent)
          Else
             IsNotOK=.false.
             Sys(SysNum)%ReheatControlNode  = GetCoilWaterInletNode(Sys(SysNum)%ReheatComp,Sys(SysNum)%ReheatName,IsNotOK)
             IF (IsNotOK) THEN
               CALL ShowContinueError('..Occurs in '//trim(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName))
               ErrorsFound=.true.
             ELSE
!  A4,     \field Unit supply air outlet node
!          \note same as heating coil air outlet node
!          \note same as zone inlet node
!          \type alpha
               IsNotOK=.false.
               Sys(SysNum)%ReheatAirOutletNode  = GetCoilOutletNode(Sys(SysNum)%ReheatComp,Sys(SysNum)%ReheatName,IsNotOK)
               IF (IsNotOK) THEN
                 CALL ShowContinueError('..Occurs in '//trim(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName))
                 ErrorsFound=.true.
               ENDIF
             ENDIF
!               GetOnlySingleNode(Alphas(6),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
!                                NodeType_Water,NodeConnectionType_Actuator,1,ObjectIsParent)
          End If
        END IF
!  A4,     \field Unit supply air outlet node
!          \note same as heating coil air outlet node
!          \note same as zone inlet node
!          \type alpha
!        Sys(SysNum)%ReheatAirOutletNode  = &
!               GetOnlySingleNode(Alphas(4),ErrorsFound,Sys(SysNum)%SysType,Alphas(1), &
!                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)
        IF  (Sys(SysNum)%ReheatComp_Num .EQ. HCoilType_SteamAirHeating)THEN
           Sys(SysNum)%MaxReheatSteamVolFlow = Numbers(4)
           Sys(SysNum)%MinReheatSteamVolFlow = Numbers(5)
        Else
           Sys(SysNum)%MaxReheatWaterVolFlow = Numbers(4)
           Sys(SysNum)%MinReheatWaterVolFlow = Numbers(5)
        END IF
        Sys(SysNum)%ControllerOffset   = Numbers(6)
        ! Set default convergence tolerance
        IF (Sys(SysNum)%ControllerOffset .LE. 0.0d0) THEN
          Sys(SysNum)%ControllerOffset = 0.001d0
        END IF
        Sys(SysNum)%DamperHeatingAction = ReverseAction

        ! Register component set data
        CALL TestCompSet(Sys(SysNum)%SysType,Sys(SysNum)%SysName, &
                         NodeID(Sys(SysNum)%InletNodeNum),NodeID(Sys(SysNum)%ReheatAirOutletNode),'Air Nodes')

        ! Fill the Zone Equipment data with the inlet node number of this unit.
        ! what if not found?  error?
        IsNotOK=.true.
        DO CtrlZone = 1,NumOfZones
          IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
          DO SupAirIn = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
            IF (Sys(SysNum)%ReheatAirOutletNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(SupAirIn)) THEN
              IsNotOK=.false.
              IF (ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode > 0) THEN
                CALL ShowSevereError('Error in connecting a terminal unit to a zone')
                CALL ShowContinueError(TRIM(NodeID(Sys(SysNum)%ReheatAirOutletNode))//' already connects to another zone')
                CALL ShowContinueError('Occurs for terminal unit '//TRIM(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
                CALL ShowContinueError('Check terminal unit node names for errors')
                ErrorsFound = .true.
              ELSE
                ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%InNode = Sys(SysNum)%InletNodeNum
                ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode = Sys(SysNum)%ReheatAirOutletNode
              END IF
            END IF
          END DO
        END DO
        IF (IsNotOK) THEN
          CALL ShowWarningError('Did not Match Supply Air Outlet Node to any Zone Node')
          CALL ShowContinueError('..Occurs in '//trim(Sys(SysNum)%SysType)//' = ' // TRIM(Sys(SysNum)%SysName))
        ENDIF

        ! Add reheat coil to component sets array
        CALL SetUpCompSets(Sys(SysNum)%SysType, Sys(SysNum)%SysName, &
                           Alphas(9),Alphas(10),Alphas(5),Alphas(4))
        ! Add fan to component sets array
        CALL SetUpCompSets(Sys(SysNum)%SysType, Sys(SysNum)%SysName, &
                           Alphas(7),Alphas(8),Alphas(3),Alphas(5))

        !Setup the Average damper Position output variable
        CALL SetupOutputVariable('Zone Air Terminal VAV Damper Position []', Sys(Sysnum)%DamperPosition, &
                              'System','Average',Sys(Sysnum)%SysName)

      END DO

      DO SysIndex=1,NumSys
        DO ADUNum = 1,NumAirDistUnits
          IF (Sys(SysIndex)%ReheatAirOutletNode == AirDistUnit(ADUNum)%OutletNodeNum) THEN
            AirDistUnit(ADUNum)%InletNodeNum = Sys(SysIndex)%InletNodeNum
            Sys(SysIndex)%ADUNum = ADUNum
          END IF
        END DO
        ! one assumes if there isn't one assigned, it's an error?
        IF (Sys(SysIndex)%ADUNum == 0) THEN
          CALL ShowSevereError(RoutineName//'No matching Air Distribution Unit, for System = ['//  &
             TRIM(Sys(SysIndex)%SysType)//','//TRIM(Sys(SysIndex)%SysName)//'].')
          CALL ShowContinueError('...should have outlet node = '//TRIM(NodeID(Sys(SysIndex)%ReheatAirOutletNode)))
!          ErrorsFound=.true.
        ENDIF
      END DO


      ! Error check to see if a single duct air terminal is assigned to zone that has zone secondary recirculation
      ! specified in the Sizing:Zone object

      NumZoneSiz = GetNumObjectsFound("Sizing:Zone")
      IF (NumZoneSiz > 0) THEN
        DO SysIndex=1,NumSys
          SizLoop: DO ZoneSizIndex = 1, NumZoneSiz
            IF (DoZoneSizing) THEN
              IF (FinalZoneSizing(ZoneSizIndex)%ActualZoneNum == Sys(SysIndex)%ActualZoneNum) THEN
                IF (FinalZoneSizing(ZoneSizIndex)%ZoneSecondaryRecirculation > 0.0d0) THEN
                  CALL ShowWarningError(RoutineName//'A zone secondary recirculation fraction is specified for zone served by ')
                  CALL ShowContinueError('...terminal unit "'//TRIM(Sys(SysIndex)%SysName)//  &
                     '" , that indicates a single path system')
                  CALL ShowContinueError('...The zone secondary recirculation for that zone was set to 0.0')
                  FinalZoneSizing(ZoneSizIndex)%ZoneSecondaryRecirculation = 0.0D0
                  EXIT SizLoop
                END IF
              END IF
            END IF
          END DO SizLoop
        END DO
      END IF

    DEALLOCATE(Alphas)
    DEALLOCATE(cAlphaFields)
    DEALLOCATE(cNumericFields)
    DEALLOCATE(Numbers)
    DEALLOCATE(lAlphaBlanks)
    DEALLOCATE(lNumericBlanks)

    IF (ErrorsFound) THEN
      CALL ShowFatalError(RoutineName//'Errors found in input.  Preceding condition(s) cause termination.')
    ENDIF

  RETURN

END SUBROUTINE GetSysInput

! End of Get Input subroutines for the Module
!******************************************************************************



 ! Beginning Initialization Section of the Module
!******************************************************************************

SUBROUTINE InitSys(SysNum,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   January 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for  initializations of the Sys Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataAirflowNetwork, ONLY: SimulateAirflowNetwork,AirflowNetworkFanActivated,AirflowNetworkControlMultizone
  USE DataDefineEquip,    ONLY: AirDistUnit
  USE DataZoneEquipment,  ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList,ZoneEquipConfig
  USE InputProcessor,     ONLY: SameString
  USE DataPlant,          ONLY: PlantLoop, ScanPlantLoopsForObject, TypeOf_CoilWaterSimpleHeating, &
                                TypeOf_CoilSteamAirHeating
  USE PlantUtilities,     ONLY: InitComponentNodes
  USE DataGlobals,        ONLY: AnyPlantInModel
  USE HeatingCoils,       ONLY: GetHeatingCoilCapacity=>GetCoilCapacity

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT (IN):: FirstHVACIteration
  INTEGER, INTENT(IN) :: SysNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: InletNode
  INTEGER             :: OutletNode
  INTEGER             :: ADUNum
  INTEGER             :: SysIndex
  LOGICAL,SAVE             :: MyOneTimeFlag = .true.
  LOGICAL,SAVE             :: ZoneEquipmentListChecked = .false.  ! True after the Zone Equipment List has been checked for items
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MySizeFlag
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: GetGasElecHeatCoilCap ! Gets autosized value of coil capacity
  REAL(r64) :: SteamTemp
  REAL(r64) :: SteamDensity
  REAL(r64) :: rho
  LOGICAL   :: errFlag

  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: PlantLoopScanFlag

          ! FLOW:


! Do the Begin Simulation initializations
  IF (MyOneTimeFlag) THEN

    ALLOCATE(MyEnvrnFlag(NumSys))
    ALLOCATE(MySizeFlag(NumSys))
    ALLOCATE(PlantLoopScanFlag(NumSys))
    ALLOCATE(GetGasElecHeatCoilCap(NumSys))
    MyEnvrnFlag = .TRUE.
    MySizeFlag  = .TRUE.
    PlantLoopScanFlag = .TRUE.
    GetGasElecHeatCoilCap = .TRUE.
    MyOneTimeFlag = .FALSE.
  END IF

  IF (PlantLoopScanFlag(SysNum) .and. ALLOCATED(PlantLoop)) THEN
    IF( (Sys(SysNum)%ReheatComp_PlantType == TypeOf_CoilWaterSimpleHeating) &
        .OR. (Sys(SysNum)%ReheatComp_PlantType == TypeOf_CoilSteamAirHeating) )  THEN
      ! setup plant topology indices for plant fed heating coils
      errFlag=.false.
      CALL ScanPlantLoopsForObject(Sys(SysNum)%ReheatName,           &
                                   Sys(SysNum)%ReheatComp_PlantType, &
                                   Sys(SysNum)%HWLoopNum,            &
                                   Sys(SysNum)%HWLoopSide,           &
                                   Sys(SysNum)%HWBranchIndex,        &
                                   Sys(SysNum)%HWCompIndex,          &
                                   errFlag=errFlag)

      IF (errFlag) THEN
        CALL ShowContinueError('Reference Unit="'//trim(Sys(SysNum)%SysName)//'", type='//trim(Sys(SysNum)%SysType))
        CALL ShowFatalError('InitSys: Program terminated for previous conditions.')
      ENDIF

      Sys(SysNum)%ReheatCoilOutletNode = &
                      PlantLoop(Sys(SysNum)%HWLoopNum)%LoopSide(Sys(SysNum)%HWLoopSide) &
                         %Branch(Sys(SysNum)%HWBranchIndex)%Comp(Sys(SysNum)%HWCompIndex)%NodeNumOut

      PlantLoopScanFlag(SysNum) = .FALSE.
    ELSE
      PlantLoopScanFlag(SysNum) = .FALSE.
    ENDIF
  ELSEIF (PlantLoopScanFlag(SysNum) .AND. .NOT. AnyPlantInModel) THEN
    PlantLoopScanFlag(SysNum) = .FALSE.
  ENDIF

  IF (.not. ZoneEquipmentListChecked .and. ZoneEquipInputsFilled) THEN
    ZoneEquipmentListChecked=.true.
    ! Check to see if there is a Air Distribution Unit on the Zone Equipment List
    DO SysIndex=1,NumSys
      IF (Sys(SysIndex)%ADUNum == 0) CYCLE
      IF (CheckZoneEquipmentList('ZoneHVAC:AirDistributionUnit',AirDistUnit(Sys(SysIndex)%ADUNum)%Name)) CYCLE
      CALL ShowSevereError('InitSingleDuctSystems: ADU=[Air Distribution Unit,'//  &
           TRIM(AirDistUnit(Sys(SysIndex)%ADUNum)%Name)//  &
           '] is not on any ZoneHVAC:EquipmentList.')
      CALL ShowContinueError('...System=['//TRIM(Sys(SysIndex)%SysType)//','//TRIM(Sys(SysIndex)%SysName)//  &
           '] will not be simulated.')
    ENDDO
  ENDIF

  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(SysNum) ) THEN

    CALL SizeSys(SysNum)

    MySizeFlag(SysNum) = .FALSE.
  END IF

  IF(GetGasElecHeatCoilCap(SysNum))THEN
    IF(Sys(SysNum)%ReheatComp_Num .EQ. HCoilType_Electric .OR. Sys(SysNum)%ReheatComp_Num .EQ. HCoilType_Gas)THEN
      IF(Sys(SysNum)%ReheatCoilMaxCapacity == AutoSize)THEN
          errFlag = .FALSE.
          Sys(SysNum)%ReheatCoilMaxCapacity = &
               GetHeatingCoilCapacity(Sys(SysNum)%ReheatComp,Sys(SysNum)%ReheatName,errFlag)
          IF(errFlag) CALL ShowContinueError('Occurs for terminal unit '// &
                           TRIM(Sys(SysNum)%SysType)//' = '//TRIM(Sys(SysNum)%SysName))
      END IF
      IF(Sys(SysNum)%ReheatCoilMaxCapacity /= Autosize)THEN
        GetGasElecHeatCoilCap(SysNum) = .FALSE.
      END IF
    ELSE
      GetGasElecHeatCoilCap(SysNum) = .FALSE.
    END IF
  END IF
! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(SysNum)) THEN

    ! Set the outlet node max mass flow rate to the Max Air Flow specified for the Sys
    OutletNode = Sys(SysNum)%OutletNodeNum
    InletNode  = Sys(SysNum)%InletNodeNum
    Node(OutletNode)%MassFlowRateMax = Sys(SysNum)%MaxAirVolFlowRate * StdRhoAir
    Sys(SysNum)%AirMassFlowRateMax = Sys(SysNum)%MaxAirVolFlowRate * StdRhoAir
    Sys(SysNum)%HeatAirMassFlowRateMax = Sys(SysNum)%MaxHeatAirVolFlowRate * StdRhoAir
    Node(InletNode)%MassFlowRateMax = Sys(SysNum)%MaxAirVolFlowRate * StdRhoAir
    MassFlowDiff(SysNum) = 1.0d-10 * Sys(SysNum)%AirMassFlowRateMax

    IF (Sys(SysNum)%HWLoopNum > 0 .AND. &
          Sys(SysNum)%ReheatComp_Num .NE. HCoilType_SteamAirHeating ) THEN !protect early calls before plant is setup
      rho = GetDensityGlycol(PlantLoop(Sys(SysNum)%HWLoopNum)%FluidName,  &
                                  InitConvTemp, &
                                  PlantLoop(Sys(SysNum)%HWLoopNum)%FluidIndex,&
                                  'InitSys' )
    ELSE
      rho = 1000.d0
    ENDIF

    Sys(SysNum)%MaxReheatWaterFlow = rho * Sys(SysNum)%MaxReheatWaterVolFlow
    Sys(SysNum)%MinReheatWaterFlow = rho * Sys(SysNum)%MinReheatWaterVolFlow

    Sys(SysNum)%AirMassFlowDuringReheatMax = Sys(SysNum)%MaxAirVolFlowRateDuringReheat * StdRhoAir

    ! set the upstream leakage flowrate
    ADUNum = Sys(SysNum)%ADUNum
    IF (AirDistUnit(ADUNum)%UpStreamLeak) THEN
      AirDistUnit(ADUNum)%MassFlowRateUpStrLk = Sys(SysNum)%AirMassFlowRateMax * AirDistUnit(ADUNum)%UpStreamLeakFrac
    ELSE
      AirDistUnit(ADUNum)%MassFlowRateUpStrLk = 0.0D0
    END IF

    IF  (Sys(SysNum)%ReheatComp_Num .EQ. HCoilType_SteamAirHeating)THEN
        SteamTemp=100.d0
        SteamDensity=GetSatDensityRefrig('STEAM',SteamTemp,1.0d0,Sys(SysNum)%FluidIndex,'InitHVACSingleDuct')
        Sys(SysNum)%MaxReheatSteamFlow = SteamDensity * Sys(SysNum)%MaxReheatSteamVolFlow
        Sys(SysNum)%MinReheatSteamFlow = SteamDensity * Sys(SysNum)%MinReheatSteamVolFlow
    EndIf

    IF (SameString(Sys(SysNum)%SysType,'AirTerminal:SingleDuct:VAV:Reheat') .or. &
        SameString(Sys(SysNum)%SysType,'AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat') .or. &
        SameString(Sys(SysNum)%SysType,'AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat') .or. &
        SameString(Sys(SysNum)%SysType,'AirTerminal:SingleDuct:VAV:NoReheat')) THEN
      ! need the lowest schedule value
      If (Sys(SysNum)%ZoneMinAirFracMethod == ScheduledMinFrac) Then
        Sys(SysNum)%ZoneMinAirFrac = GetScheduleMinValue(Sys(SysNum)%ZoneMinAirFracSchPtr)
      ENDIF
      Node(OutletNode)%MassFlowRateMin = Node(OutletNode)%MassFlowRateMax * &
                                        Sys(SysNum)%ZoneMinAirFrac
      Node(InletNode)%MassFlowRateMin = Node(InletNode)%MassFlowRateMax * &
                                        Sys(SysNum)%ZoneMinAirFrac
    ELSE
      Node(OutletNode)%MassFlowRateMin = 0.0D0
      Node(InletNode)%MassFlowRateMin = 0.0D0
    END IF
    IF ((Sys(SysNum)%ReheatControlNode .gt. 0) .AND. .NOT. PlantLoopScanFlag(SysNum)) THEN
        IF  (Sys(SysNum)%ReheatComp_Num .EQ. HCoilType_SteamAirHeating)THEN
          CALL InitComponentNodes( Sys(SysNum)%MinReheatSteamFlow,   &
                                   Sys(SysNum)%MaxReheatSteamFlow,   &
                                   Sys(SysNum)%ReheatControlNode,    &
                                   Sys(SysNum)%ReheatCoilOutletNode, &
                                   Sys(SysNum)%HWLoopNum,            &
                                   Sys(SysNum)%HWLoopSide,           &
                                   Sys(SysNum)%HWBranchIndex,        &
                                   Sys(SysNum)%HWCompIndex)
        ELSE
          CALL InitComponentNodes( Sys(SysNum)%MinReheatWaterFlow,   &
                                   Sys(SysNum)%MaxReheatWaterFlow,   &
                                   Sys(SysNum)%ReheatControlNode,    &
                                   Sys(SysNum)%ReheatCoilOutletNode, &
                                   Sys(SysNum)%HWLoopNum,            &
                                   Sys(SysNum)%HWLoopSide,           &
                                   Sys(SysNum)%HWBranchIndex,        &
                                   Sys(SysNum)%HWCompIndex)

        END IF
    END IF
    ! Find air loop associated with terminal unit
    IF (Sys(SysNum)%SysType_Num == SingleDuctVAVReheat .OR. Sys(SysNum)%SysType_Num == SingleDuctVAVNoReheat)THEN
      IF (Sys(SysNum)%CtrlZoneNum .GT. 0) THEN
        Sys(SysNum)%AirLoopNum = ZoneEquipConfig(Sys(SysNum)%CtrlZoneNum)%AirLoopNum
      END IF
    END IF

    MyEnvrnFlag(SysNum) = .FALSE.
  END IF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(SysNum) = .TRUE.
  ENDIF

  ! Initialize the Inlet Nodes of the air side of air terminal
  InletNode  = Sys(SysNum)%InletNodeNum
  OutletNode = Sys(SysNum)%OutletNodeNum

  If (Sys(SysNum)%ZoneMinAirFracMethod == ScheduledMinFrac) Then
    Sys(SysNum)%ZoneMinAirFrac = GetCurrentScheduleValue(Sys(SysNum)%ZoneMinAirFracSchPtr)
    !now reset inlet node min avail
    Node(InletNode)%MassFlowRateMinAvail = Sys(SysNum)%AirMassFlowRateMax * Sys(SysNum)%ZoneMinAirFrac
  ENDIF

  IF (FirstHVACIteration) THEN
     !The first time through set the mass flow rate to the Max
    If((Node(InletNode)%MassFlowRate > 0.0D0) .AND.  &
       (GetCurrentScheduleValue(Sys(SysNum)%SchedPtr) .gt. 0.0D0)) Then
      if (.NOT. (SimulateAirflowNetwork .gt. AirflowNetworkControlMultizone .AND. AirflowNetworkFanActivated)) then
        Node(InletNode)%MassFlowRate = Sys(SysNum)%AirMassFlowRateMax
      endif
    Else
      Node(InletNode)%MassFlowRate = 0.0D0
    END IF

    If((Node(InletNode)%MassFlowRateMaxAvail > 0.0D0) .AND.  &
       (GetCurrentScheduleValue(Sys(SysNum)%SchedPtr) .gt. 0.0D0)) Then
      if (.NOT. (SimulateAirflowNetwork .GT. AirflowNetworkControlMultizone .AND. AirflowNetworkFanActivated)) then
        Node(InletNode)%MassFlowRateMaxAvail = Sys(SysNum)%AirMassFlowRateMax
      endif
    Else
      Node(InletNode)%MassFlowRateMaxAvail = 0.0D0
    END IF

    If((Node(InletNode)%MassFlowRate > 0.0D0) .AND.  &
       (GetCurrentScheduleValue(Sys(SysNum)%SchedPtr) .gt. 0.0D0)) Then
      if (.NOT. (SimulateAirflowNetwork .GT. AirflowNetworkControlMultizone .AND. AirflowNetworkFanActivated)) then
        Node(InletNode)%MassFlowRateMinAvail = Sys(SysNum)%AirMassFlowRateMax * Sys(SysNum)%ZoneMinAirFrac
      endif
    Else
      Node(InletNode)%MassFlowRateMinAvail = 0.0D0
    END IF
    ! reset the mass flow rate histories
    MassFlow1(SysNum) = 0.0D0
    MassFlow2(SysNum) = 0.0D0
    MassFlow3(SysNum) = 0.0D0
    MassFlow3(SysNum) = 0.0D0

  End If

  !Do a check and make sure that the max and min available(control) flow is
  !  between the physical max and min while operating.
  SysInlet(SysNum)%AirMassFlowRateMaxAvail = MIN(Sys(SysNum)%AirMassFlowRateMax, &
                                          Node(InletNode)%MassFlowRateMaxAvail)
  SysInlet(SysNum)%AirMassFlowRateMinAvail = Min(MAX(Node(OutletNode)%MassFlowRateMin, &
                                          Node(InletNode)%MassFlowRateMinAvail), &
                                          SysInlet(SysNum)%AirMassFlowRateMaxAvail)

  ! Do the following initializations (every time step): This should be the info from
  ! the previous components outlets or the node data in this section.
  ! Load the node data in this section for the component simulation
  SysInlet(SysNum)%AirMassFlowRate = Node(InletNode)%MassFlowRate
  SysInlet(SysNum)%AirTemp         = Node(InletNode)%Temp
  SysInlet(SysNum)%AirHumRat       = Node(InletNode)%HumRat
  SysInlet(SysNum)%AirEnthalpy     = Node(InletNode)%Enthalpy

  RETURN

END SUBROUTINE InitSys

SUBROUTINE SizeSys(SysNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   September 2001
          !       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Sys Components for which flow rates have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone or system sizing arrays.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor
  USE WaterCoils,          ONLY: SetCoilDesFlow, GetCoilWaterInletNode, GetCoilWaterOutletNode
  USE SteamCoils,          ONLY: GetCoilSteamInletNode, GetCoilSteamOutletNode
!  USE BranchInputManager,  ONLY: MyPlantSizingIndex
  USE General,             ONLY: SafeDivide, TrimSigDigits, RoundSigDigits
!unused  USE DataHeatBalance,     ONLY: Zone
  USE DataGlobals,         ONLY: AutoCalculate
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE DataPlant,           ONLY: PlantLoop, MyPlantSizingIndex
  USE FluidProperties,     ONLY: GetDensityGlycol, GetSpecificHeatGlycol
  USE General,             ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: SysNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PltSizHeatNum ! index of plant sizing object for 1st heating loop
  REAL(r64)           :: CoilInTemp = 0.0D0
  REAL(r64)           :: DesCoilLoad = 0.0D0
  REAL(r64)           :: DesZoneHeatLoad = 0.0D0
  REAL(r64)           :: ZoneDesTemp = 0.0D0
  REAL(r64)           :: ZoneDesHumRat = 0.0D0
  REAL(r64)           :: DesMassFlow
  REAL(r64)           :: TempSteamIn
  REAL(r64)           :: EnthSteamOutWet
  REAL(r64)           :: EnthSteamInDry
  REAL(r64)           :: LatentHeatSteam
  REAL(r64)           :: SteamDensity
  INTEGER             :: CoilWaterInletNode=0
  INTEGER             :: CoilWaterOutletNode=0
  INTEGER             :: CoilSteamInletNode=0
  INTEGER             :: CoilSteamOutletNode=0

  LOGICAL             :: ErrorsFound
  LOGICAL             :: PlantSizingErrorsFound
  REAL(r64)           :: rho ! local fluid density
  REAL(r64)           :: Cp  ! local fluid specific heat
  INTEGER             :: DummyWaterIndex = 1
  REAL(r64)           :: UserInputMaxHeatAirVolFlowRate = 0.0D0  ! user input for MaxHeatAirVolFlowRate
  LOGICAL             :: IsAutosize
  REAL(r64)           :: MaxAirVolFlowRateDes              ! Autosized maximum air flow rate for reporting
  REAL(r64)           :: MaxAirVolFlowRateUser             ! Hardsized maximum air flow rate for reporting
  REAL(r64)           :: MaxHeatAirVolFlowRateDes          ! Autosized maximum heating air flow rate for reporting
  REAL(r64)           :: MaxHeatAirVolFlowRateUser         ! Hardsized maximum heating air flow rate for reporting
  REAL(r64)           :: MaxAirVolFlowRateDuringReheatDes  ! Autosized maximum air flow durign reheat for reporting
  REAL(r64)           :: MaxAirVolFlowRateDuringReheatUser ! Hardsized maximum air flow durign reheat for reporting
  REAL(r64)           :: MaxAirVolFractionDuringReheatDes  ! Autosized maximum air fraction durign reheat for reporting
  REAL(r64)           :: MaxAirVolFractionDuringReheatUser ! Hardsized maximum air flow durign reheat for reporting
  REAL(r64)           :: MaxReheatWaterVolFlowDes          ! Autosized reheat water flow or reporting
  REAL(r64)           :: MaxReheatWaterVolFlowUser         ! Hardsized reheat water flow for reporting
  REAL(r64)           :: MaxReheatSteamVolFlowDes          ! Autosized reheat steam flow for reporting
  REAL(r64)           :: MaxReheatSteamVolFlowUser         ! Hardsized reheat steam flow for reporting

  PltSizHeatNum = 0
  DesMassFlow = 0.0D0
  ErrorsFound = .FALSE.
  IsAutosize = .FALSE.
  MaxAirVolFlowRateDes = 0.0d0
  MaxAirVolFlowRateUser = 0.0d0
  MaxHeatAirVolFlowRateDes = 0.0d0
  MaxHeatAirVolFlowRateUser = 0.0d0
  MaxAirVolFlowRateDuringReheatDes = 0.0d0
  MaxAirVolFlowRateDuringReheatUser = 0.0d0
  MaxAirVolFractionDuringReheatDes = 0.0d0
  MaxAirVolFractionDuringReheatUser = 0.0d0
  MaxReheatWaterVolFlowDes = 0.0d0
  MaxReheatWaterVolFlowUser = 0.0d0
  MaxReheatSteamVolFlowDes = 0.0d0
  MaxReheatSteamVolFlowUser = 0.0d0

  IF (Sys(SysNum)%MaxAirVolFlowRate == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF

  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! simulation continue
      IF (Sys(SysNum)%MaxAirVolFlowRate > 0.0d0) THEN
        CALL ReportSizingOutput(Sys(SysNum)%SysType, Sys(SysNum)%SysName, &
                            'User-Specified Maximum Air Flow Rate [m3/s]', Sys(SysNum)%MaxAirVolFlowRate)
      END IF
    ELSE ! Autosize or hard-size with sizing run

      CALL CheckZoneSizing(Sys(SysNum)%SysType, Sys(SysNum)%SysName)

      MaxAirVolFlowRateDes = MAX(TermUnitFinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                                           TermUnitFinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)

      IF (MaxAirVolFlowRateDes < SmallAirVolFlow) THEN
          MaxAirVolFlowRateDes = 0.0D0
      END IF
      IF (IsAutosize) THEN
        Sys(SysNum)%MaxAirVolFlowRate = MaxAirVolFlowRateDes
        CALL ReportSizingOutput(Sys(SysNum)%SysType, Sys(SysNum)%SysName, &
                              'Design Size Maximum Air Flow Rate [m3/s]', MaxAirVolFlowRateDes)
      ELSE ! Hard-size with sizing data
        IF (Sys(SysNum)%MaxAirVolFlowRate > 0.0d0 .AND. MaxAirVolFlowRateDes > 0.0d0) THEN
          MaxAirVolFlowRateUser = Sys(SysNum)%MaxAirVolFlowRate
          CALL ReportSizingOutput(Sys(SysNum)%SysType, Sys(SysNum)%SysName, &
                              'Design Size Maximum Air Flow Rate [m3/s]', MaxAirVolFlowRateDes, &
                              'User-Specified Maximum Air Flow Rate [m3/s]', MaxAirVolFlowRateUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MaxAirVolFlowRateDes -  MaxAirVolFlowRateUser)/MaxAirVolFlowRateUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeHVACSingleDuct: Potential issue with equipment sizing for '// &
                                    TRIM(Sys(SysNum)%SysType)//' = "'//TRIM(Sys(SysNum)%SysName)//'".')
              CALL ShowContinueError('User-Specified Maximum Air Flow Rate of '// &
                                    TRIM(RoundSigDigits(MaxAirVolFlowRateUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Maximum Air Flow Rate of ' // &
                                    TRIM(RoundSigDigits(MaxAirVolFlowRateDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (Sys(SysNum)%MaxHeatAirVolFlowRate == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! simulation should continue
      UserInputMaxHeatAirVolFlowRate = Sys(SysNum)%MaxHeatAirVolFlowRate
      IF (Sys(SysNum)%MaxHeatAirVolFlowRate > 0.d0) THEN
        CALL ReportSizingOutput(Sys(SysNum)%SysType, Sys(SysNum)%SysName, &
                      'User-Specified Maximum Heating Air Flow Rate [m3/s]', Sys(SysNum)%MaxHeatAirVolFlowRate)
      END IF
    ELSE
      CALL CheckZoneSizing(Sys(SysNum)%SysType, Sys(SysNum)%SysName)
      MaxHeatAirVolFlowRateDes =  TermUnitFinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow
      IF (MaxHeatAirVolFlowRateDes < SmallAirVolFlow) THEN
          MaxHeatAirVolFlowRateDes = 0.0D0
      END IF
      IF (IsAutosize) THEN
        Sys(SysNum)%MaxHeatAirVolFlowRate = MaxHeatAirVolFlowRateDes
        UserInputMaxHeatAirVolFlowRate = 0.0D0
        CALL ReportSizingOutput(Sys(SysNum)%SysType, Sys(SysNum)%SysName, &
                              'Design Size Maximum Heating Air Flow Rate [m3/s]', MaxHeatAirVolFlowRateDes)
      ELSE ! Hard-size with sizing data
        IF (Sys(SysNum)%MaxHeatAirVolFlowRate > 0.0d0 .AND. MaxHeatAirVolFlowRateDes > 0.0d0) THEN
          MaxHeatAirVolFlowRateUser = Sys(SysNum)%MaxHeatAirVolFlowRate
          UserInputMaxHeatAirVolFlowRate = Sys(SysNum)%MaxHeatAirVolFlowRate
          CALL ReportSizingOutput(Sys(SysNum)%SysType, Sys(SysNum)%SysName, &
                              'Design Size Maximum Heating Air Flow Rate [m3/s]', MaxHeatAirVolFlowRateDes, &
                              'User-Specified Maximum Heating Air Flow Rate [m3/s]', MaxHeatAirVolFlowRateUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MaxHeatAirVolFlowRateDes - MaxHeatAirVolFlowRateUser)/MaxHeatAirVolFlowRateUser ) &
                                          > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeHVACSingleDuct: Potential issue with equipment sizing for '// &
                                    TRIM(Sys(SysNum)%SysType)//' = "'//TRIM(Sys(SysNum)%SysName)//'".')
              CALL ShowContinueError('User-Specified Maximum Heating Air Flow Rate of '// &
                                    TRIM(RoundSigDigits(MaxHeatAirVolFlowRateUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Maximum Heating Air Flow Rate of ' // &
                                    TRIM(RoundSigDigits(MaxHeatAirVolFlowRateDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IF (Sys(SysNum)%ZoneMinAirFracMethod == ScheduledMinFrac) Then
    ! need a value for sizing.
    IF (Sys(SysNum)%ConstantMinAirFracSetByUser) THEN
      Sys(SysNum)%ZoneMinAirFrac = Sys(SysNum)%DesignMinAirFrac
      ! if both inputs are defined, use the max
      IF(Sys(SysNum)%FixedMinAirSetByUser) THEN
        Sys(SysNum)%ZoneMinAirFrac = &
          MIN(1.0d0,MAX(Sys(SysNum)%ZoneMinAirFrac,SafeDivide(Sys(SysNum)%DesignFixedMinAir,Sys(SysNum)%MaxAirVolFlowRate)))
      ENDIF
    ! if only fixed is defined, use the value
    ELSEIF(Sys(SysNum)%FixedMinAirSetByUser) THEN
        Sys(SysNum)%ZoneMinAirFrac = &
          MIN(1.0d0,SafeDivide(Sys(SysNum)%DesignFixedMinAir,Sys(SysNum)%MaxAirVolFlowRate))
    ELSE
      !use an average of min and max in schedule
      Sys(SysNum)%ZoneMinAirFrac = (GetScheduleMinValue(Sys(SysNum)%ZoneMinAirFracSchPtr) &
                                   + GetScheduleMaxValue(Sys(SysNum)%ZoneMinAirFracSchPtr)) &
                                   / 2.0D0
    ENDIF

  ENDIF

  IF (Sys(SysNum)%ZoneMinAirFracMethod == FixedMin) THEN
    ! need a value for sizing.
    Sys(SysNum)%ZoneMinAirFrac = &
          MIN(1.0d0,SafeDivide(Sys(SysNum)%DesignFixedMinAir,Sys(SysNum)%MaxAirVolFlowRate))

  END IF

  IsAutosize = .FALSE.
  IF(Sys(SysNum)%MaxAirVolFlowRateDuringReheat == Autocalculate)THEN
    IsAutosize = .TRUE.
  END IF

  MaxAirVolFlowRateDuringReheatDes = MIN(0.002032D0*Sys(SysNum)%ZoneFloorArea, &
                                     Sys(SysNum)%MaxAirVolFlowRate)
    ! apply limit based on min stop
  MaxAirVolFlowRateDuringReheatDes = MAX(MaxAirVolFlowRateDuringReheatDes, &
                                  (Sys(SysNum)%MaxAirVolFlowRate * Sys(SysNum)%ZoneMinAirFrac) )
  IF (IsAutosize) THEN
    Sys(SysNum)%MaxAirVolFlowRateDuringReheat = MaxAirVolFlowRateDuringReheatDes
    CALL ReportSizingOutput(Sys(SysNum)%SysType, Sys(SysNum)%SysName, &
               'Design Size Maximum Flow per Zone Floor Area during Reheat [m3/s-m2]', &
               MaxAirVolFlowRateDuringReheatDes/Sys(SysNum)%ZoneFloorArea)
  ELSE ! Hard size with sizing data
    IF (Sys(SysNum)%MaxAirVolFlowRateDuringReheat > 0.0d0 .AND. MaxAirVolFlowRateDuringReheatDes > 0.0d0 ) THEN
      MaxAirVolFlowRateDuringReheatUser = Sys(SysNum)%MaxAirVolFlowRateDuringReheat
      CALL ReportSizingOutput(Sys(SysNum)%SysType, Sys(SysNum)%SysName, &
               'Design Size Maximum Flow per Zone Floor Area during Reheat [m3/s-m2]', &
               MaxAirVolFlowRateDuringReheatDes/Sys(SysNum)%ZoneFloorArea, &
               'User-Specified Maximum Flow per Zone Floor Area during Reheat [m3/s-m2]', &
               MaxAirVolFlowRateDuringReheatUser)
      IF (DisplayExtraWarnings) THEN
        IF ((ABS(MaxAirVolFlowRateDuringReheatDes - MaxAirVolFlowRateDuringReheatUser)/MaxAirVolFlowRateDuringReheatUser) &
                                       > AutoVsHardSizingThreshold) THEN
          CALL ShowMessage('SizeHVACSingleDuct: Potential issue with equipment sizing for '// &
                                    TRIM(Sys(SysNum)%SysType)//' = "'//TRIM(Sys(SysNum)%SysName)//'".')
          CALL ShowContinueError('User-Specified Maximum Flow per Zone Floor Area during Reheat of '// &
                                    TRIM(RoundSigDigits(MaxAirVolFlowRateDuringReheatUser,5))// ' [m3/s-m2]')
          CALL ShowContinueError('differs from Design Size Maximum Flow per Zone Floor Area during Reheat of ' // &
                                    TRIM(RoundSigDigits(MaxAirVolFlowRateDuringReheatDes,5))// ' [m3/s-m2]')
          CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
          CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
        END IF
      ENDIF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF(Sys(SysNum)%MaxAirVolFractionDuringReheat == Autocalculate)THEN
    IsAutosize = .TRUE.
  END IF
  IF(Sys(SysNum)%MaxAirVolFlowRate .GT. 0.0D0)THEN
    MaxAirVolFractionDuringReheatDes = MIN(1.0D0,(0.002032D0*Sys(SysNum)%ZoneFloorArea/ &
                                                      Sys(SysNum)%MaxAirVolFlowRate))
      ! apply limit based on min stop
    MaxAirVolFractionDuringReheatDes = MAX(MaxAirVolFractionDuringReheatDes, &
                                                      Sys(SysNum)%ZoneMinAirFrac )
  ELSE
    MaxAirVolFractionDuringReheatDes = 0.0D0
  END IF
  IF (IsAutosize) THEN
    Sys(SysNum)%MaxAirVolFractionDuringReheat = MaxAirVolFractionDuringReheatDes
    CALL ReportSizingOutput(Sys(SysNum)%SysType, Sys(SysNum)%SysName, &
            'Design Size Maximum Flow Fraction during Reheat []', MaxAirVolFractionDuringReheatDes)
  ELSE
    IF (Sys(SysNum)%MaxAirVolFractionDuringReheat > 0.0d0 .AND. MaxAirVolFractionDuringReheatDes > 0.0d0) THEN
      MaxAirVolFractionDuringReheatUser = Sys(SysNum)%MaxAirVolFractionDuringReheat
      CALL ReportSizingOutput(Sys(SysNum)%SysType, Sys(SysNum)%SysName, &
                 'Design Size Maximum Flow Fraction during Reheat []', MaxAirVolFractionDuringReheatDes, &
                 'User-Specified Maximum Flow Fraction during Reheat []',MaxAirVolFractionDuringReheatUser)
      IF (DisplayExtraWarnings) THEN
        IF ((ABS(MaxAirVolFractionDuringReheatDes - MaxAirVolFractionDuringReheatUser)/MaxAirVolFractionDuringReheatUser) &
                                > AutoVsHardSizingThreshold) THEN
          CALL ShowMessage('SizeHVACSingleDuct: Potential issue with equipment sizing for '// &
                                    TRIM(Sys(SysNum)%SysType)//' = "'//TRIM(Sys(SysNum)%SysName)//'".')
          CALL ShowContinueError('User-Specified Maximum Flow Fraction during Reheat of '// &
                                    TRIM(RoundSigDigits(MaxAirVolFractionDuringReheatUser,5))// ' []')
          CALL ShowContinueError('differs from Design Size Maximum Flow Fraction during Reheat of ' // &
                                    TRIM(RoundSigDigits(MaxAirVolFractionDuringReheatDes,5))// ' []')
          CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
          CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
        END IF
      ENDIF
    END IF
  END IF

  ! use the larger of the two reheat flow rate methods for the simulated maximum flow during reheat
  Sys(SysNum)%MaxAirVolFlowRateDuringReheat = MAX(Sys(SysNum)%MaxAirVolFlowRateDuringReheat, &
                  Sys(SysNum)%MaxAirVolFractionDuringReheat * Sys(SysNum)%MaxAirVolFlowRate)
  Sys(SysNum)%MaxAirVolFlowRateDuringReheat = MIN(Sys(SysNum)%MaxAirVolFlowRateDuringReheat,Sys(SysNum)%MaxAirVolFlowRate)

  IF (CurZoneEqNum > 0) THEN
    TermUnitSizing(CurZoneEqNum)%ReheatAirFlowMult = 1.0D0
    TermUnitSizing(CurZoneEqNum)%ReheatLoadMult    = 1.0d0
    IF (ZoneSizingRunDone) THEN
      IF (Sys(SysNum)%SysType_Num == SingleDuctVAVReheatVSFan) THEN
        TermUnitSizing(CurZoneEqNum)%AirVolFlow = MAX(UserInputMaxHeatAirVolFlowRate, &
                                                      CalcFinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow * &
                                                      CalcFinalZoneSizing(CurZoneEqNum)%HeatSizingFactor, &
                                                      Sys(SysNum)%MaxAirVolFlowRate * Sys(SysNum)%ZoneMinAirFrac)
      ELSE
        TermUnitSizing(CurZoneEqNum)%AirVolFlow = MAX(CalcFinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow * &
                                                      CalcFinalZoneSizing(CurZoneEqNum)%HeatSizingFactor, &
                                                      Sys(SysNum)%MaxAirVolFlowRate * Sys(SysNum)%ZoneMinAirFrac)
      END IF
    ELSE
      IF (Sys(SysNum)%SysType_Num == SingleDuctVAVReheatVSFan) THEN
        TermUnitSizing(CurZoneEqNum)%AirVolFlow = MAX(Sys(SysNum)%MaxHeatAirVolFlowRate, &
                                                      Sys(SysNum)%MaxAirVolFlowRate * Sys(SysNum)%ZoneMinAirFrac)
      ELSE IF (Sys(SysNum)%SysType_Num == SingleDuctConstVolReheat) THEN
        TermUnitSizing(CurZoneEqNum)%AirVolFlow = Sys(SysNum)%MaxAirVolFlowRate
      ELSE
        IF (Sys(SysNum)%DamperHeatingAction == ReverseAction) THEN
          IF (Sys(SysNum)%MaxAirVolFlowRateDuringReheat > 0.0D0) THEN
            TermUnitSizing(CurZoneEqNum)%AirVolFlow = MAX(Sys(SysNum)%MaxAirVolFlowRateDuringReheat, &
                                                          (Sys(SysNum)%MaxAirVolFlowRate * Sys(SysNum)%ZoneMinAirFrac) )
          ELSE
            TermUnitSizing(CurZoneEqNum)%AirVolFlow = Sys(SysNum)%MaxAirVolFlowRate
          END IF
        ELSE
          TermUnitSizing(CurZoneEqNum)%AirVolFlow = Sys(SysNum)%MaxAirVolFlowRate * Sys(SysNum)%ZoneMinAirFrac
        END IF
      END IF
    END IF
    IF (TermUnitSizing(CurZoneEqNum)%AirVolFlow > SmallAirVolFlow) THEN
      IF (Sys(SysNum)%DamperHeatingAction == ReverseAction .AND. Sys(SysNum)%MaxAirVolFlowRateDuringReheat > 0.0D0) THEN
        TermUnitSizing(CurZoneEqNum)%ReheatAirFlowMult =  MIN(Sys(SysNum)%MaxAirVolFlowRateDuringReheat, &
                                                                       Sys(SysNum)%MaxAirVolFlowRate ) &
                                                             / TermUnitSizing(CurZoneEqNum)%AirVolFlow
        TermUnitSizing(CurZoneEqNum)%ReheatLoadMult = TermUnitSizing(CurZoneEqNum)%ReheatAirFlowMult
      ELSEIF (Sys(SysNum)%DamperHeatingAction == ReverseAction .AND. Sys(SysNum)%MaxAirVolFlowRateDuringReheat == 0.0D0) THEN
        TermUnitSizing(CurZoneEqNum)%ReheatAirFlowMult =  Sys(SysNum)%MaxAirVolFlowRate / TermUnitSizing(CurZoneEqNum)%AirVolFlow
        TermUnitSizing(CurZoneEqNum)%ReheatLoadMult    = TermUnitSizing(CurZoneEqNum)%ReheatAirFlowMult
      ELSEIF (Sys(SysNum)%DamperHeatingAction == Normal .AND. Sys(SysNum)%MaxAirVolFlowRateDuringReheat > 0.0D0) THEN
        TermUnitSizing(CurZoneEqNum)%ReheatAirFlowMult =  MIN(Sys(SysNum)%MaxAirVolFlowRateDuringReheat, &
                                                          (Sys(SysNum)%MaxAirVolFlowRate * Sys(SysNum)%ZoneMinAirFrac) ) &
                                                              / TermUnitSizing(CurZoneEqNum)%AirVolFlow
        TermUnitSizing(CurZoneEqNum)%ReheatLoadMult    = 1.0d0
      ELSEIF (Sys(SysNum)%DamperHeatingAction == Normal .AND. Sys(SysNum)%MaxAirVolFlowRateDuringReheat == 0.0D0) THEN
        TermUnitSizing(CurZoneEqNum)%ReheatAirFlowMult =  (Sys(SysNum)%MaxAirVolFlowRate * Sys(SysNum)%ZoneMinAirFrac) &
                                                              / TermUnitSizing(CurZoneEqNum)%AirVolFlow
        TermUnitSizing(CurZoneEqNum)%ReheatLoadMult    = 1.0D0
      ELSE
        TermUnitSizing(CurZoneEqNum)%ReheatAirFlowMult =  Sys(SysNum)%MaxAirVolFlowRate / TermUnitSizing(CurZoneEqNum)%AirVolFlow
        TermUnitSizing(CurZoneEqNum)%ReheatLoadMult = TermUnitSizing(CurZoneEqNum)%ReheatAirFlowMult
      END IF
      TermUnitSizing(CurZoneEqNum)%ReheatAirFlowMult = MAX(1.0D0, TermUnitSizing(CurZoneEqNum)%ReheatAirFlowMult)
      TermUnitSizing(CurZoneEqNum)%ReheatLoadMult    = MAX(1.0D0, TermUnitSizing(CurZoneEqNum)%ReheatLoadMult )
    ELSE
      TermUnitSizing(CurZoneEqNum)%ReheatAirFlowMult = 1.0D0
      TermUnitSizing(CurZoneEqNum)%ReheatLoadMult    = 1.0D0
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (Sys(SysNum)%MaxReheatWaterVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN
      IF (Sys(SysNum)%MaxReheatWaterVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(Sys(SysNum)%SysType, Sys(SysNum)%SysName, &
                   'User-Specified Maximum Reheat Water Flow Rate [m3/s]', Sys(SysNum)%MaxReheatWaterVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(Sys(SysNum)%SysType, Sys(SysNum)%SysName)
      IF (SameString(Sys(SysNum)%ReheatComp,'Coil:Heating:Water')) THEN
        CoilWaterInletNode = GetCoilWaterInletNode('Coil:Heating:Water',Sys(SysNum)%ReheatName,ErrorsFound)
        CoilWaterOutletNode = GetCoilWaterOutletNode('Coil:Heating:Water',Sys(SysNum)%ReheatName,ErrorsFound)
        IF (IsAutosize) THEN
          PlantSizingErrorsFound = .FALSE.
          PltSizHeatNum = MyPlantSizingIndex('Coil:Heating:Water', Sys(SysNum)%ReheatName, CoilWaterInletNode, &
                                       CoilWaterOutletNode, PlantSizingErrorsFound)
          IF(PlantSizingErrorsFound)THEN
            CALL ShowContinueError('...Occurs in '//TRIM(Sys(SysNum)%SysType)//':'//TRIM(Sys(SysNum)%SysName))
            ErrorsFound=.TRUE.
          END IF
          IF (PltSizHeatNum > 0) THEN
            CoilInTemp = TermUnitFinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTempTU
            DesMassFlow = StdRhoAir *   &
                                 TermUnitSizing(CurZoneEqNum)%AirVolFlow
            DesZoneHeatLoad = CalcFinalZoneSizing(CurZoneEqNum)%DesHeatLoad * CalcFinalZoneSizing(CurZoneEqNum)%HeatSizingFactor
            ZoneDesTemp = CalcFinalZoneSizing(CurZoneEqNum)%ZoneTempAtHeatPeak
            ZoneDesHumRat = CalcFinalZoneSizing(CurZoneEqNum)%ZoneHumRatAtHeatPeak
            ! the coil load is the zone design heating load plus (or minus!) the reheat load
            DesCoilLoad = DesZoneHeatLoad + PsyCpAirFnWTdb(ZoneDesHumRat, 0.5d0*(CoilInTemp+ZoneDesTemp)) &
                             * DesMassFlow * (ZoneDesTemp-CoilInTemp)
            IF (DesCoilLoad >= SmallLoad) THEN

              rho = GetDensityGlycol(PlantLoop(Sys(SysNum)%HWLoopNum)%FluidName, &
                                   60.d0, &
                                   PlantLoop(Sys(SysNum)%HWLoopNum)%FluidIndex, &
                                   'SizeSys')

              Cp  = GetSpecificHeatGlycol(PlantLoop(Sys(SysNum)%HWLoopNum)%FluidName, &
                                   60.d0, &
                                   PlantLoop(Sys(SysNum)%HWLoopNum)%FluidIndex, &
                                   'SizeSys')

              MaxReheatWaterVolFlowDes = DesCoilLoad / &
                                       ( PlantSizData(PltSizHeatNum)%DeltaT * &
                                       Cp * rho )
            ELSE
              MaxReheatWaterVolFlowDes = 0.0D0
            END IF
          ELSE
            CALL ShowSevereError('Autosizing of water flow requires a heating loop Sizing:Plant object')
            CALL ShowContinueError('Occurs in AirTerminal Object='//TRIM(Sys(SysNum)%SysName))
            Errorsfound = .TRUE.
          END IF
        END IF
        IF (IsAutosize) THEN
          Sys(SysNum)%MaxReheatWaterVolFlow = MaxReheatWaterVolFlowDes
          CALL ReportSizingOutput(Sys(SysNum)%SysType, Sys(SysNum)%SysName, &
                                'Design Size Maximum Reheat Water Flow Rate [m3/s]', MaxReheatWaterVolFlowDes)
          CALL ReportSizingOutput(Sys(SysNum)%SysType, Sys(SysNum)%SysName, &
                                'Design Size Reheat Coil Sizing Air Volume Flow Rate [m3/s]', &
                                TermUnitSizing(CurZoneEqNum)%AirVolFlow)
        ELSE ! Hard-size with sizing data
          IF (Sys(SysNum)%MaxReheatWaterVolFlow > 0.0d0 .AND. MaxReheatWaterVolFlowDes > 0.0d0) THEN
            MaxReheatWaterVolFlowUser = Sys(SysNum)%MaxReheatWaterVolFlow
            CALL ReportSizingOutput(Sys(SysNum)%SysType, Sys(SysNum)%SysName, &
                                'Design Size Maximum Reheat Water Flow Rate [m3/s]', MaxReheatWaterVolFlowDes, &
                                'User-Specified Maximum Reheat Water Flow Rate [m3/s]', MaxReheatWaterVolFlowUser)
            IF (DisplayExtraWarnings) THEN
              IF ((ABS(MaxReheatWaterVolFlowDes - MaxReheatWaterVolFlowUser)/MaxReheatWaterVolFlowUser) &
                                > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizeHVACSingleDuct: Potential issue with equipment sizing for '// &
                                    TRIM(Sys(SysNum)%SysType)//' = "'//TRIM(Sys(SysNum)%SysName)//'".')
                CALL ShowContinueError('User-Specified Maximum Reheat Water Flow Rate of '// &
                                    TRIM(RoundSigDigits(MaxReheatWaterVolFlowUser,5))// ' [m3/s]')
                CALL ShowContinueError('differs from Design Size Maximum Reheat Water Flow Rate of ' // &
                                    TRIM(RoundSigDigits(MaxReheatWaterVolFlowDes,5))// ' [m3/s]')
                CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
              END IF
            ENDIF
          END IF
        END IF
      END IF
    END IF
  ELSE
    Sys(SysNum)%MaxReheatWaterVolFlow = 0.0D0
  END IF

  IsAutosize = .FALSE.
  IF (Sys(SysNum)%MaxReheatSteamVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN
      IF (Sys(SysNum)%MaxReheatSteamVolFlow > 0.0d0) THEN
        CALL ReportSizingOutput(Sys(SysNum)%SysType, Sys(SysNum)%SysName, &
                   'User-Specified Maximum Reheat Steam Flow Rate [m3/s]', Sys(SysNum)%MaxReheatSteamVolFlow)
      END IF
    ELSE
      CALL CheckZoneSizing(Sys(SysNum)%SysType, Sys(SysNum)%SysName)
      IF (SameString(Sys(SysNum)%ReheatComp,'Coil:Heating:Steam')) THEN
        CoilSteamInletNode = GetCoilSteamInletNode('Coil:Heating:Steam',Sys(SysNum)%ReheatName,ErrorsFound)
        CoilSteamOutletNode = GetCoilSteamOutletNode('Coil:Heating:Steam',Sys(SysNum)%ReheatName,ErrorsFound)
        IF (IsAutosize) THEN
          PlantSizingErrorsFound = .FALSE.
          PltSizHeatNum = MyPlantSizingIndex('Coil:Heating:Steam', Sys(SysNum)%ReheatName, CoilSteamInletNode, &
                                       CoilSteamOutletNode, PlantSizingErrorsFound)
          IF(PlantSizingErrorsFound)THEN
            CALL ShowContinueError('...Occurs in '//TRIM(Sys(SysNum)%SysType)//':'//TRIM(Sys(SysNum)%SysName))
            ErrorsFound=.TRUE.
          END IF
          IF (PltSizHeatNum > 0) THEN
            CoilInTemp = TermUnitFinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTempTU
            DesMassFlow = StdRhoAir * TermUnitSizing(CurZoneEqNum)%AirVolFlow
            DesZoneHeatLoad = CalcFinalZoneSizing(CurZoneEqNum)%DesHeatLoad * CalcFinalZoneSizing(CurZoneEqNum)%HeatSizingFactor
            ZoneDesTemp = CalcFinalZoneSizing(CurZoneEqNum)%ZoneTempAtHeatPeak
            ZoneDesHumRat = CalcFinalZoneSizing(CurZoneEqNum)%ZoneHumRatAtHeatPeak
            ! the coil load is the zone design heating load plus (or minus!) the reheat load
            DesCoilLoad = DesZoneHeatLoad + PsyCpAirFnWTdb(ZoneDesHumRat, 0.5d0*(CoilInTemp+ZoneDesTemp)) &
                             * DesMassFlow * (ZoneDesTemp-CoilInTemp)
            IF (DesCoilLoad >= SmallLoad) THEN
               TempSteamIn= 100.00d0
               EnthSteamInDry =  GetSatEnthalpyRefrig('STEAM',TempSteamIn,1.0d0,Sys(SysNum)%FluidIndex,'SizeHVACSingleDuct')
               EnthSteamOutWet=  GetSatEnthalpyRefrig('STEAM',TempSteamIn,0.0d0,Sys(SysNum)%FluidIndex,'SizeHVACSingleDuct')
               LatentHeatSteam=EnthSteamInDry-EnthSteamOutWet
               SteamDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,Sys(SysNum)%FluidIndex,'SizeHVACSingleDuct')

               Cp = GetSpecificHeatGlycol('WATER', &
                                        PlantSizData(PltSizHeatNum)%ExitTemp, &
                                        DummyWaterIndex, &
                                        'SizeSys')
               MaxReheatSteamVolFlowDes = DesCoilLoad /(SteamDensity*(LatentHeatSteam + &
                                        PlantSizData(PltSizHeatNum)%DeltaT * Cp ))
            ELSE
              MaxReheatSteamVolFlowDes = 0.0D0
            END IF
          ELSE
            CALL ShowSevereError('Autosizing of Steam flow requires a heating loop Sizing:Plant object')
            CALL ShowContinueError('Occurs in AirTerminal:SingleDuct:ConstantVolume:Reheat Object='//TRIM(Sys(SysNum)%SysName))
            Errorsfound = .TRUE.
          END IF
        END IF
        IF (IsAutosize) THEN
          Sys(SysNum)%MaxReheatSteamVolFlow = MaxReheatSteamVolFlowDes
          CALL ReportSizingOutput(Sys(SysNum)%SysType, Sys(SysNum)%SysName, &
                                  'Design Size Maximum Reheat Steam Flow Rate [m3/s]', MaxReheatSteamVolFlowDes)
        ELSE
          IF (Sys(SysNum)%MaxReheatSteamVolFlow > 0.0d0 .AND. MaxReheatSteamVolFlowDes > 0.0d0) THEN
            MaxReheatSteamVolFlowUser = Sys(SysNum)%MaxReheatSteamVolFlow
            CALL ReportSizingOutput(Sys(SysNum)%SysType, Sys(SysNum)%SysName, &
                                  'Design Size Maximum Reheat Steam Flow Rate [m3/s]', MaxReheatSteamVolFlowDes, &
                                  'User-Specified Maximum Reheat Steam Flow Rate [m3/s]', MaxReheatSteamVolFlowUser)
            IF (DisplayExtraWarnings) THEN
              IF ((ABS(MaxReheatSteamVolFlowDes - MaxReheatSteamVolFlowUser)/MaxReheatSteamVolFlowUser) &
                                   > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizeHVACSingleDuct: Potential issue with equipment sizing for '// &
                                    TRIM(Sys(SysNum)%SysType)//' = "'//TRIM(Sys(SysNum)%SysName)//'".')
                CALL ShowContinueError('User-Specified Maximum Reheat Steam Flow Rate of '// &
                                    TRIM(RoundSigDigits(MaxReheatSteamVolFlowUser,5))// ' [m3/s]')
                CALL ShowContinueError('differs from Design Size Maximum Reheat Steam Flow Rate of ' // &
                                    TRIM(RoundSigDigits(MaxReheatSteamVolFlowDes,5))// ' [m3/s]')
                CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
              END IF
            ENDIF
          END IF
        END IF
      END IF
    END IF
  ELSE
    Sys(SysNum)%MaxReheatSteamVolFlow = 0.0D0
  END IF

  IF (CurZoneEqNum > 0) THEN
    TermUnitSizing(CurZoneEqNum)%MinFlowFrac = Sys(SysNum)%ZoneMinAirFrac
    TermUnitSizing(CurZoneEqNum)%MaxHWVolFlow = Sys(SysNum)%MaxReheatWaterVolFlow
    TermUnitSizing(CurZoneEqNum)%MaxSTVolFlow = Sys(SysNum)%MaxReheatSteamVolFlow
    IF (Sys(SysNum)%ReheatComp_Num == HCoilType_SimpleHeating) THEN
      IF (Sys(SysNum)%DamperHeatingAction == Normal) THEN
        CALL SetCoilDesFlow(Sys(SysNum)%ReheatComp,Sys(SysNum)%ReheatName,Sys(SysNum)%ZoneMinAirFrac*Sys(SysNum)%MaxAirVolFlowRate,&
                            ErrorsFound)
      ELSE
        CALL SetCoilDesFlow(Sys(SysNum)%ReheatComp,Sys(SysNum)%ReheatName,TermUnitSizing(CurZoneEqNum)%AirVolFlow,&
                            ErrorsFound)
      END IF
    END IF
  END IF

  IF (Sys(SysNum)%MaxAirVolFlowRateDuringReheat > 0.0D0) THEN
    ! check for inconsistent dual max input
    IF (Sys(SysNum)%MaxAirVolFlowRateDuringReheat < (Sys(SysNum)%ZoneMinAirFrac * Sys(SysNum)%MaxAirVolFlowRate) ) THEN
      ! Only warn when really out of bounds
      IF ((Sys(SysNum)%ZoneMinAirFrac*Sys(SysNum)%MaxAirVolFlowRate)-Sys(SysNum)%MaxAirVolFlowRateDuringReheat > 1.d-8) THEN
        CALL ShowWarningError('SingleDuctSystem:SizeSys: Air Terminal Unit flow limits are not consistent, '//  &
           'minimum flow limit is larger than reheat maximum')
        CALL ShowContinueError('Air Terminal Unit name = '//TRIM(Sys(SysNum)%SysName))
        CALL ShowContinueError('Maximum terminal flow during reheat = '  &
              //TRIM(RoundSigDigits(Sys(SysNum)%MaxAirVolFlowRateDuringReheat, 6))//' [m3/s] or flow fraction = ' &
              //TRIM(RoundSigDigits((Sys(SysNum)%MaxAirVolFlowRateDuringReheat/Sys(SysNum)%MaxAirVolFlowRate ),4)) )
        CALL ShowContinueError('Minimum terminal flow = '  &
              //TRIM(RoundSigDigits((Sys(SysNum)%ZoneMinAirFrac * Sys(SysNum)%MaxAirVolFlowRate), 6))//  &
                 ' [m3/s] or flow fraction = '&
              //TRIM(RoundSigDigits(Sys(SysNum)%ZoneMinAirFrac,4)) )
        CALL ShowContinueError('The reheat maximum flow limit will be replaced by the minimum limit, and the simulation continues')
      ENDIF
      Sys(SysNum)%MaxAirVolFlowRateDuringReheat = (Sys(SysNum)%ZoneMinAirFrac * Sys(SysNum)%MaxAirVolFlowRate)
    ENDIF
  ENDIF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN

END SUBROUTINE SizeSys
 ! End Initialization Section of the Module
!******************************************************************************


 ! Begin Algorithm Section of the Module
!******************************************************************************
SUBROUTINE SimVAV(SysNum,FirstHVACIteration, ZoneNum, ZoneNodeNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   January 2000
          !       MODIFIED       Fred Buhl: added reverse action damper heating action: August 2001
          !                      KHL/TH 7/2010: revise to support dual max
          !                      FB/KHL/TH 9/2010: added maximum supply air temperature leaving reheat coil
          !                      TH 3/2012: added supply air flow adjustment based on zone maximum outdoor
          !                                 air fraction - a TRACE feature
          !                      Brent Griffith, 5/2012, general cleanup, fix negatives CR 8767, fix phantom coil flows CR 8854
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the simple single duct volume VAV.

          ! METHODOLOGY EMPLOYED:
          ! There is method to this madness.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands
!unused   USE DataHeatBalFanSys, ONLY: Mat
  USE WaterCoils,        ONLY:SimulateWaterCoilComponents
  USE HeatingCoils,      ONLY:SimulateHeatingCoilComponents
  USE SteamCoils,        ONLY: SimulateSteamCoilComponents
  USE DataDefineEquip,   ONLY: AirDistUnit
!unused   USE DataAirLoop,       ONLY: AirLoopControlInfo
  USE PlantUtilities,    ONLY: SetActuatedBranchFlowRate
  USE DataHVACGlobals,   ONLY: SmallLoad
  USE DataAirflowNetwork, ONLY: SimulateAirflowNetwork,AirflowNetworkFanActivated,AirflowNetworkControlMultizone,VAVTerminalRatio

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: SysNum
  INTEGER, INTENT(IN) :: ZoneNum
  INTEGER, INTENT(IN) :: ZoneNodeNum
  LOGICAL, INTENT(IN) :: FirstHVACIteration



          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: MassFlow    ! [kg/sec]   Total Mass Flow Rate from Hot & Cold Inlets
  REAL(r64) :: QTotLoad    ! [Watts] Remaining load required for this zone
  REAL(r64) :: QZnReq      ! [Watts] Load calculated for heating coil
  REAL(r64) :: QToHeatSetPt  ! [W]  remaining load to heating setpoint
  INTEGER   :: ADUNum        ! index of air distribution unit for this terminal unit
  REAL(r64) :: CpAirZn
  REAL(r64) :: CpAirSysIn
  REAL(r64) :: DeltaTemp
  INTEGER   :: SysOutletNode  ! The node number of the terminal unit outlet node
  INTEGER   :: SysInletNode   ! the node number of the terminal unit inlet node
  INTEGER   :: WaterControlNode   !This is the Actuated Reheat Control Node
  REAL(r64) :: MaxFlowWater  !This is the value passed to the Controller depending if FirstHVACIteration or not
  REAL(r64) :: MinFlowWater  !This is the value passed to the Controller depending if FirstHVACIteration or not
  REAL(r64) :: QActualHeating    ! the heating load seen by the reheat coil
  REAL(r64) :: QHeatingDelivered ! the actual output from heating coil
  REAL(r64) :: LeakLoadMult      ! load multiplier to adjust for downstream leaks
  REAL(r64) :: MinFlowFrac       ! minimum flow fraction (and minimum damper position)
  REAL(r64) :: MinAirMassFlowRevAct=0.0D0 ! minimum air mass flow rate used in "reverse action" air mass flow rate calculation
  REAL(r64) :: MaxAirMassFlowRevAct=0.0D0 ! maximum air mass flow rate used in "reverse action" air mass flow rate calculation
  REAL(r64) :: MassFlowBasedOnOA ! supply air mass flow rate based on zone OA requirements
  REAL(r64) :: AirLoopOAFrac     ! fraction of outside air entering air loop
  REAL(r64) :: DummyMdot  ! temporary mass flow rate argument

  REAL(r64) :: ZoneTemp = 0.0D0         ! zone air temperature [C]
  REAL(r64) :: MaxHeatTemp = 0.0D0      ! maximum supply air temperature [C]
  REAL(r64) :: MaxDeviceAirMassFlowReheat = 0.0D0      ! air mass flow rate required to meet the coil heating load [W]
  REAL(r64) :: MassFlowReqToLimitLeavingTemp = 0.0D0   ! air mass flow rate actually used [W]
  REAL(r64) :: QZoneMaxRHTempLimit = 0.0D0         ! maximum zone heat addition rate given constraints of MaxHeatTemp and max
                                         ! available air mass flow rate [W]
  REAL(r64) :: MinMassAirFlow = 0.0D0   ! the air flow rate during heating for normal acting damper
  REAL(r64) :: QZoneMax2 = 0.0D0        ! temporary variable

   ! Note to the perplexed
   !
   ! The SINGLE DUCT:VAV:REHEAT terminal unit originally contained 2 components: a damper
   ! and a reheat coil. The damper has become a virtual component - it consists only of
   ! an air inlet node and an air outlet node. The damper is upstream of the heating coil.
   !
   ! Sys(SysNum)%InletNodeNum is the inlet node to the terminal unit and the damper
   ! Sys(SysNum)%OutletNodeNum is the outlet node of the damper and the inlet node of the heating coil
   ! Sys(SysNum)%ReheatAirOutletNode is the outlet node of the terminal unit and the heating coil

   ! The calculated load from the Heat Balance
  ADUNum = Sys(SysNum)%ADUNum
  LeakLoadMult = AirDistUnit(ADUNum)%LeakLoadMult
  QTotLoad=ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired * LeakLoadMult
  QToHeatSetPt=ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP * LeakLoadMult
  SysOutletNode = Sys(SysNum)%ReheatAirOutletNode
  SysInletNode = Sys(SysNum)%InletNodeNum
  CpAirZn = PsyCpAirFnWTdb(Node(ZoneNodeNum)%HumRat,Node(ZoneNodeNum)%Temp)
  MinFlowFrac = Sys(SysNum)%ZoneMinAirFrac
  MassFlowBasedOnOA = 0.0d0
  ZoneTemp = Node(ZoneNodeNum)%Temp
  MinMassAirFlow = MinFlowFrac * StdRhoAir * Sys(SysNum)%MaxAirVolFlowRate

   !Then depending on if the Load is for heating or cooling it is handled differently.  First
   ! the massflow rate for cooling is determined to meet the entire load.  Then
   ! if the massflow is below the minimum or greater than the Max it is set to either the Min
   ! or the Max as specified for the VAV model.
  If( (QTotLoad < 0.0D0) .AND. (SysInlet(SysNum)%AirMassFlowRateMaxAvail > 0.0D0) .AND. &
      (TempControlType(ZoneNum) .NE. SingleHeatingSetPoint) ) THEN
     ! Calculate the flow required for cooling
    CpAirSysIn = PsyCpAirFnWTdb(SysInlet(SysNum)%AirHumRat,SysInlet(SysNum)%AirTemp)
    DeltaTemp = CpAirSysIn*SysInlet(SysNum)%AirTemp - CpAirZn*ZoneTemp

     !Need to check DeltaTemp and ensure that it is not zero
    If (DeltaTemp .ne. 0.0D0) THEN
      MassFlow= QTotLoad/DeltaTemp
    ELSE
      MassFlow = SysInlet(SysNum)%AirMassFlowRateMaxAvail
    END IF

     ! Apply the zone maximum outdoor air fraction FOR VAV boxes - a TRACE feature
    IF (ZoneSysEnergyDemand(ZoneNum)%SupplyAirAdjustFactor > 1.0D0) THEN
      MassFlow = MassFlow * ZoneSysEnergyDemand(ZoneNum)%SupplyAirAdjustFactor
    ENDIF

     ! calculate supply air flow rate based on user specified OA requirement
    CALL CalcOAMassFlow(SysNum, MassFlowBasedOnOA, AirLoopOAFrac)
    MassFlow = MAX(MassFlow, MassFlowBasedOnOA)

     ! used for normal acting damper
    MinMassAirFlow = MAX(MinMassAirFlow, MassFlowBasedOnOA)
    MinMassAirFlow = MAX(MinMassAirFlow,SysInlet(SysNum)%AirMassFlowRateMinAvail)
    MinMassAirFlow = MIN(MinMassAirFlow,SysInlet(SysNum)%AirMassFlowRateMaxAvail)

     ! limit the OA based supply air flow rate based on optional user input
     !Check to see if the flow is < the Min or > the Max air Fraction to the zone; then set to min or max
    MassFlow = MAX(MassFlow,SysInlet(SysNum)%AirMassFlowRateMinAvail)
    MassFlow = MIN(MassFlow,SysInlet(SysNum)%AirMassFlowRateMaxAvail)

    If (SimulateAirflowNetwork .gt. AirflowNetworkControlMultizone .AND. AirflowNetworkFanActivated &
       .AND. VAVTerminalRatio .GT. 0.0d0) then
      MassFlow = MassFlow * VAVTerminalRatio
      If (MassFlow .gt. Node(Sys(SysNum)%InletNodeNum)%MassFlowRate) Then
        MassFlow = Node(Sys(SysNum)%InletNodeNum)%MassFlowRate
      End If
    End If

  ELSE IF ((SysInlet(SysNum)%AirMassFlowRateMaxAvail > 0.0D0) .AND. (QTotLoad >= 0.0D0 .OR. &
             TempControlType(ZoneNum) .EQ. SingleHeatingSetPoint) ) THEN
!     IF (Sys(SysNum)%DamperHeatingAction .EQ. ReverseAction .AND. SysInlet(SysNum)%AirMassFlowRateMinAvail <= SmallMassFlow) THEN
       ! special case for heating: reverse action and damper allowed to close - set the minimum flow rate to a small but nonzero value
!       MassFlow = 0.01d0*SysInlet(SysNum)%AirMassFlowRateMaxAvail
!     ELSE
       ! usual case for heating: set the air mass flow rate to the minimum
    MassFlow = SysInlet(SysNum)%AirMassFlowRateMinAvail
!     END IF

     ! Apply the zone maximum outdoor air fraction for VAV boxes - a TRACE feature
    IF (ZoneSysEnergyDemand(ZoneNum)%SupplyAirAdjustFactor > 1.0d0) THEN
      MassFlow = MassFlow * ZoneSysEnergyDemand(ZoneNum)%SupplyAirAdjustFactor
    ENDIF

     ! calculate supply air flow rate based on user specified OA requirement
    CALL CalcOAMassFlow(SysNum, MassFlowBasedOnOA, AirLoopOAFrac)
    MassFlow = MAX(MassFlow, MassFlowBasedOnOA)

     !Check to see if the flow is < the Min or > the Max air Fraction to the zone; then set to min or max
    IF(MassFlow <= SysInlet(SysNum)%AirMassFlowRateMinAvail) THEN
      MassFlow = SysInlet(SysNum)%AirMassFlowRateMinAvail
    ELSE IF (MassFlow >= SysInlet(SysNum)%AirMassFlowRateMaxAvail) THEN
      MassFlow = SysInlet(SysNum)%AirMassFlowRateMaxAvail
    END IF

    ! the AirflowNetwork model overrids the mass flow rate value
    If (SimulateAirflowNetwork .gt. AirflowNetworkControlMultizone .AND. AirflowNetworkFanActivated &
       .AND. VAVTerminalRatio .GT. 0.0d0) then
      MassFlow = MassFlow * VAVTerminalRatio
      If (MassFlow .gt. Node(Sys(SysNum)%InletNodeNum)%MassFlowRate) Then
        MassFlow = Node(Sys(SysNum)%InletNodeNum)%MassFlowRate
      End If
    End If

  ELSE
     ! System is Off set massflow to 0.0
    MassFlow = 0.0D0
    AirLoopOAFrac = 0.0D0
  END IF

   ! look for bang-bang condition: flow rate oscillating between 2 values during the air loop / zone
   ! equipment iteration. If detected, set flow rate to previous value.
  IF ( ( (ABS(MassFlow-MassFlow2(SysNum)) < MassFlowDiff(SysNum)) .OR. &
          (ABS(MassFlow-MassFlow3(SysNum)) < MassFlowDiff(SysNum)) ) .AND. &
          (ABS(MassFlow-MassFlow1(SysNum)) >= MassFlowDiff(SysNum)) ) THEN
    IF (MassFlow > 0.0D0) MassFlow = MassFlow1(SysNum)
  END IF

   !Move data to the damper outlet node
  SysOutlet(SysNum)%AirTemp         = SysInlet(SysNum)%AirTemp
  SysOutlet(SysNum)%AirHumRat       = SysInlet(SysNum)%AirHumRat
  SysOutlet(SysNum)%AirMassFlowRate = MassFlow
  SysOutlet(SysNum)%AirMassFlowRateMaxAvail = SysInlet(SysNum)%AirMassFlowRateMaxAvail
  SysOutlet(SysNum)%AirMassFlowRateMinAvail = SysInlet(SysNum)%AirMassFlowRateMinAvail
  SysOutlet(SysNum)%AirEnthalpy             = SysInlet(SysNum)%AirEnthalpy

!   ! Calculate the Damper Position when there is a Max air flow specified.
!  If (MassFlow == 0.0D0) THEN
!    Sys(SysNum)%DamperPosition = 0.0D0
!  ELSE IF (SysInlet(SysNum)%AirMassFlowRateMaxAvail > SysInlet(SysNum)%AirMassFlowRateMinAvail) THEN
!    Sys(SysNum)%DamperPosition = ((MassFlow-SysInlet(SysNum)%AirMassFlowRateMinAvail) / &
!                                   (SysInlet(SysNum)%AirMassFlowRateMaxAvail-SysInlet(SysNum)%AirMassFlowRateMinAvail)) * &
!                                  (1.0d0-MinFlowFrac) + MinFlowFrac
!  ELSE
!    Sys(SysNum)%DamperPosition = 1.0D0
!  END IF

  IF (MassFlow == 0.0D0) THEN
    Sys(SysNum)%DamperPosition = 0.0D0
  ELSEIF ((MassFlow > 0.0D0) .AND. (MassFlow < Sys(SysNum)%AirMassFlowRateMax)) THEN
    Sys(SysNum)%DamperPosition =  MassFlow / Sys(SysNum)%AirMassFlowRateMax
  ELSEIF (MassFlow == Sys(SysNum)%AirMassFlowRateMax) THEN
    Sys(SysNum)%DamperPosition = 1.d0
  ENDIF

   !Need to make sure that the damper outlets are passed to the coil inlet
  CALL UpdateSys(SysNum)

   ! At the current air mass flow rate, calculate heating coil load
  QActualHeating = QToHeatSetPt - Massflow * CpAirZn * (SysInlet(SysNum)%AirTemp-ZoneTemp) ! reheat needed

   ! do the reheat calculation if there's some air nass flow (or the damper action is "reverse action"), the flow is <= minimum ,
   ! there's a heating requirement, and there's a thermostat with a heating setpoint
   ! Reverse damper option is working only for water coils for now.
  IF((MassFlow > SmallMassFlow ) .AND. &
      (QActualHeating > 0.0D0) .AND. (TempControlType(ZoneNum) .NE. SingleCoolingSetPoint) ) THEN
     ! At this point we know that there is a heating requirement: i.e., the heating coil needs to
     ! be activated (there's a zone heating load or there's a reheat requirement). There are 3 possible
     ! situations: 1) the coil load can be met by variable temperature air (below the max heat temp) at
     ! the minimum air mass flow rate; 2) the coil load can be met by variable air flow rate with the air
     ! temperature fixed at the max heat temp; 3) the load cannot be met (we will run at max air temp and
     ! max air flow rate). We check for condition 2 by assuming the air temperatute is at the max heat temp
     ! and solving for the air mass flow rate that will meet the load. If the flow rate is between the min and
     ! max we are in condition 2.

    QZoneMax2 = QToHeatSetPt

    ! fill dual-max reheat flow limit, if any
    IF (Sys(SysNum)%DamperHeatingAction .EQ. ReverseAction) THEN
      IF (Sys(SysNum)%AirMassFlowDuringReheatMax > 0.0D0 ) THEN
        MaxDeviceAirMassFlowReheat = Sys(SysNum)%AirMassFlowDuringReheatMax
      ELSE
        MaxDeviceAirMassFlowReheat = Sys(SysNum)%AirMassFlowRateMax
      END IF
    ELSE
      MaxDeviceAirMassFlowReheat = Sys(SysNum)%AirMassFlowRateMax
    END IF

    ! determine flow based on leaving reheat temperature limit
    IF (Sys(SysNum)%MaxReheatTempSetByUser) THEN

      MaxHeatTemp = Sys(SysNum)%MaxReheatTemp
      IF (QToHeatSetPt > SmallLoad) THEN ! zone has a postive load to heating setpoint
        MassFlowReqToLimitLeavingTemp = QToHeatSetPt/(CpAirZn*(MaxHeatTemp - ZoneTemp))
      ELSE
        MassFlowReqToLimitLeavingTemp = 0.0D0
      ENDIF
    ENDIF

    ! (re)apply limits to find air mass flow
    MassFlow = MAX(MassFlow, MassFlowReqToLimitLeavingTemp)
    MassFlow = MIN(MassFlow, MaxDeviceAirMassFlowReheat)
    MassFlow = MAX(MassFlow, MassFlowBasedOnOA)
    MassFlow = MIN(MassFlow, SysInlet(SysNum)%AirMassFlowRateMaxAvail)
    MassFlow = MAX(MassFlow, SysInlet(SysNum)%AirMassFlowRateMinAvail)

    If (SimulateAirflowNetwork .gt. AirflowNetworkControlMultizone .AND. AirflowNetworkFanActivated &
       .AND. VAVTerminalRatio .GT. 0.0d0) then
      MassFlow = MassFlow * VAVTerminalRatio
      If (MassFlow .gt. Node(Sys(SysNum)%InletNodeNum)%MassFlowRate) Then
        MassFlow = Node(Sys(SysNum)%InletNodeNum)%MassFlowRate
      End If
    End If

    ! now make any corrections to heating coil loads
    IF (Sys(SysNum)%MaxReheatTempSetByUser) THEN
      QZoneMaxRHTempLimit = CpAirZn*MassFlow*(MaxHeatTemp - ZoneTemp)
      QZoneMax2 = MIN(QZoneMaxRHTempLimit,QToHeatSetPt)
    ENDIF

    SysOutlet(SysNum)%AirMassFlowRate = MassFlow

    CALL UpdateSys(SysNum)

     ! Now do the heating coil calculation for each heating coil type
    SELECT CASE(Sys(SysNum)%ReheatComp_Num)     ! Reverse damper option is working only for water coils for now.

     ! hot water heating coil
    CASE(HCoilType_SimpleHeating) ! COIL:WATER:SIMPLEHEATING
       ! Determine the load required to pass to the Component controller
       ! Although this equation looks strange (using temp instead of deltaT), it is corrected later in ControlCompOutput
       ! and is working as-is, temperature setpoints are maintained as expected.
      QZnReq = QZoneMax2 + MassFlow*CpAirZn*ZoneTemp

       ! Initialize hot water flow rate to zero.
      DummyMdot = 0.0D0
      CALL SetActuatedBranchFlowRate(DummyMdot,Sys(SysNum)%ReheatControlNode,  &
            Sys(SysNum)%HWLoopNum,Sys(SysNum)%HWLoopSide, Sys(SysNum)%HWBranchIndex, .TRUE. )
      !On the first HVAC iteration the system values are given to the controller, but after that
      ! the demand limits are in place and there needs to be feedback to the Zone Equipment
      IF (FirstHVACIteration) THEN
        MaxFlowWater = Sys(SysNum)%MaxReheatWaterFlow
        MinFlowWater = Sys(SysNum)%MinReheatWaterFlow
      ELSE
        WaterControlNode = Sys(SysNum)%ReheatControlNode
        MaxFlowWater = Node(WaterControlNode)%MassFlowRateMaxAvail
        MinFlowWater = Node(WaterControlNode)%MassFlowRateMinAvail
      ENDIF

       ! Simulate the reheat coil at constant air flow. Control by varying the
       ! hot water flow rate.
       !FB use QActualHeating, change ControlCompOutput to use new
      CALL ControlCompOutput(CompName=Sys(SysNum)%ReheatName,         &
                          CompType=Sys(SysNum)%ReheatComp,             &
                          CompNum=Sys(SysNum)%ReheatComp_Index,        &
                          FirstHVACIteration=FirstHVACIteration,       &
                          QZnReq=QZnReq,                               &
                          ActuatedNode=Sys(SysNum)%ReheatControlNode,  &
                          MaxFlow=MaxFlowWater,                        &
                          MinFlow=MinFlowWater,                        &
                          TempOutNode=SysOutletNode,                   &
                          ControlOffSet=Sys(SysNum)%ControllerOffset,  &
                          AirMassFlow=Massflow,                        &
                          ControlCompTypeNum=Sys(SysNum)%ControlCompTypeNum, &
                          CompErrIndex=Sys(SysNum)%CompErrIndex,       &
                          LoopNum     = Sys(SysNum)%HWLoopNum,         &
                          LoopSide    = Sys(SysNum)%HWLoopSide,        &
                          BranchIndex = Sys(SysNum)%HWBranchIndex)

       ! If reverse action damper and the hot water flow is at maximum, simulate the
       ! hot water coil with fixed (maximum) hot water flow but allow the air flow to
       ! vary up to the maximum (air damper opens to try to meet zone load)
      IF (Sys(SysNum)%DamperHeatingAction .EQ. ReverseAction) THEN
        IF (Node(Sys(SysNum)%ReheatControlNode)%MassFlowRate .EQ. MaxFlowWater) THEN
          ! fill limits for air flow for controller
          MinAirMassFlowRevAct = Sys(SysNum)%AirMassFlowRateMax * Sys(SysNum)%ZoneMinAirFrac
          MinAirMassFlowRevAct = MIN(MinAirMassFlowRevAct, SysInlet(SysNum)%AirMassFlowRateMaxAvail)
          MinAirMassFlowRevAct = MAX(MinAirMassFlowRevAct, SysInlet(SysNum)%AirMassFlowRateMinAvail)

          MaxAirMassFlowRevAct = Sys(SysNum)%AirMassFlowRateMax
          MaxAirMassFlowRevAct = MIN(MaxAirMassFlowRevAct,MaxDeviceAirMassFlowReheat)
          MaxAirMassFlowRevAct = MAX(MaxAirMassFlowRevAct, MinAirMassFlowRevAct)
          MaxAirMassFlowRevAct = MIN(MaxAirMassFlowRevAct, SysInlet(SysNum)%AirMassFlowRateMaxAvail)


          Node(Sys(SysNum)%OutletNodeNum)%MassFlowRateMaxAvail = MaxAirMassFlowRevAct  ! suspect, check how/if used in ControlCompOutput
          CALL ControlCompOutput(CompName=Sys(SysNum)%ReheatName,                  &
                                  CompType=Sys(SysNum)%ReheatComp,                  &
                                  CompNum=Sys(SysNum)%ReheatComp_Index,             &
                                  FirstHVACIteration=FirstHVACIteration,            &
                                  QZnReq= QZoneMax2 ,                               &  ! why not QZnReq  ?
                                  ActuatedNode=Sys(SysNum)%OutletNodeNum,           &
                                  MaxFlow=MaxAirMassFlowRevAct, &
                                  MinFlow=MinAirMassFlowRevAct,                     &
                                  TempOutNode=SysOutletNode,                        &
                                  TempInNode=ZoneNodeNum,                           &
                                  ControlOffSet=Sys(SysNum)%ControllerOffset,       &
                                  ControlCompTypeNum=Sys(SysNum)%ControlCompTypeNum, &
                                  CompErrIndex=Sys(SysNum)%CompErrIndex )
                                  ! air flow controller, not on plant, don't pass plant topology info
           ! reset terminal unit inlet air mass flow to new value.
          Node(Sys(SysNum)%OutletNodeNum)%MassFlowRateMaxAvail = SysInlet(SysNum)%AirMassFlowRateMaxAvail
          MassFlow = Node(SysOutletNode)%MassFlowRate

          !         ! look for bang-bang condition: flow rate oscillating between 2 values during the air loop / zone
          !         ! equipment iteration. If detected, set flow rate to previous value and recalc HW flow.
          IF ( ( (ABS(MassFlow-MassFlow2(SysNum)) < MassFlowDiff(SysNum)) .OR. &
              (ABS(MassFlow-MassFlow3(SysNum)) < MassFlowDiff(SysNum)) ) .AND. &
              (ABS(MassFlow-MassFlow1(SysNum)) >= MassFlowDiff(SysNum)) ) THEN
            IF (MassFlow > 0.0D0) MassFlow = MassFlow1(SysNum)
            SysOutlet(SysNum)%AirMassFlowRate = MassFlow
            CALL UpdateSys(SysNum)

               ! Although this equation looks strange (using temp instead of deltaT), it is corrected later in ControlCompOutput
               ! and is working as-is, temperature setpoints are maintained as expected.
            QZnReq = QZoneMax2 + MassFlow*CpAirZn*ZoneTemp
            CALL ControlCompOutput(CompName=Sys(SysNum)%ReheatName,         &
                                  CompType=Sys(SysNum)%ReheatComp,             &
                                  CompNum=Sys(SysNum)%ReheatComp_Index,        &
                                  FirstHVACIteration=FirstHVACIteration,       &
                                  QZnReq=QZnReq,                               &
                                  ActuatedNode=Sys(SysNum)%ReheatControlNode,  &
                                  MaxFlow=MaxFlowWater,                        &
                                  MinFlow=MinFlowWater,                        &
                                  TempOutNode=SysOutletNode,                   &
                                  ControlOffSet=Sys(SysNum)%ControllerOffset,  &
                                  AirMassFlow=Massflow,                        &
                                  ControlCompTypeNum=Sys(SysNum)%ControlCompTypeNum, &
                                  CompErrIndex=Sys(SysNum)%CompErrIndex,            &
                                  LoopNum     = Sys(SysNum)%HWLoopNum,              &
                                  LoopSide    = Sys(SysNum)%HWLoopSide,             &
                                  BranchIndex = Sys(SysNum)%HWBranchIndex)
          END IF

          SysOutlet(SysNum)%AirMassFlowRate = MassFlow
           ! reset OA report variable
          CALL UpdateSys(SysNum)
        END IF     ! IF (Node(Sys(SysNum)%ReheatControlNode)%MassFlowRate .EQ. MaxFlowWater) THEN
      END IF     ! IF (Sys(SysNum)%DamperHeatingAction .EQ. ReverseAction) THEN

      ! Recalculate the Damper Position.
    IF (MassFlow == 0.0D0) THEN
      Sys(SysNum)%DamperPosition = 0.0D0
    ELSEIF ((MassFlow > 0.0D0) .AND. (MassFlow < Sys(SysNum)%AirMassFlowRateMax)) THEN
      Sys(SysNum)%DamperPosition =  MassFlow / Sys(SysNum)%AirMassFlowRateMax
    ELSEIF (MassFlow == Sys(SysNum)%AirMassFlowRateMax) THEN
      Sys(SysNum)%DamperPosition = 1.d0
    ENDIF

    CASE(HCoilType_SteamAirHeating) ! ! COIL:STEAM:AIRHEATING
         ! Determine the load required to pass to the Component controller
      QZnReq = QZoneMax2 - Massflow * CpAirZn * (SysInlet(SysNum)%AirTemp - ZoneTemp)

         ! Simulate reheat coil for the VAV system
      CALL SimulateSteamCoilComponents (CompName=Sys(SysNum)%ReheatName,        &
                                           CompIndex=Sys(SysNum)%ReheatComp_Index, &
                                           FirstHVACIteration=FirstHVACIteration,  &
                                           QCoilReq=QZnReq)

    CASE(HCoilType_Electric) ! COIL:ELECTRIC:HEATING
         ! Determine the load required to pass to the Component controller
      QZnReq = QZoneMax2 - Massflow * CpAirZn * (SysInlet(SysNum)%AirTemp - ZoneTemp)

         ! Simulate reheat coil for the VAV system
      CALL SimulateHeatingCoilComponents(CompName=Sys(SysNum)%ReheatName,        &
                                            CompIndex=Sys(SysNum)%ReheatComp_Index, &
                                            FirstHVACIteration=FirstHVACIteration,  &
                                            QCoilReq=QZnReq)

    CASE(HCoilType_Gas) ! COIL:GAS:HEATING
         ! Determine the load required to pass to the Component controller
      QZnReq = QZoneMax2 - Massflow * CpAirZn * (SysInlet(SysNum)%AirTemp - ZoneTemp)

         ! Simulate reheat coil for the VAV system
      CALL SimulateHeatingCoilComponents(CompName=Sys(SysNum)%ReheatName,       &
                                            CompIndex=Sys(SysNum)%ReheatComp_Index, &
                                            FirstHVACIteration=FirstHVACIteration, &
                                            QCoilReq=QZnReq,QCoilActual=QHeatingDelivered)

    CASE(HCoilType_None) ! blank
         ! I no reheat is defined then assume that the damper is the only component.
         ! If something else is there that is not a reheat coil or a blank then give the error message

    CASE DEFAULT
      CALL ShowFatalError('Invalid Reheat Component='//TRIM(Sys(SysNum)%ReheatComp))
    END SELECT

   !the COIL is OFF the properties are calculated for this special case.
  ELSE
    SELECT CASE(Sys(SysNum)%ReheatComp_Num)

    CASE(HCoilType_SimpleHeating) ! COIL:WATER:SIMPLEHEATING
         ! Simulate reheat coil for the Const Volume system
        ! Node(Sys(SysNum)%ReheatControlNode)%MassFlowRate = 0.0D0  !DSU
      DummyMdot = 0.0D0
      CALL SetActuatedBranchFlowRate(DummyMdot,Sys(SysNum)%ReheatControlNode,  &
              Sys(SysNum)%HWLoopNum,Sys(SysNum)%HWLoopSide, Sys(SysNum)%HWBranchIndex, .TRUE. )
         !call the reheat coil with the NO FLOW condition to make sure that the Node values
         ! are passed through to the coil outlet correctly
      CALL SimulateWaterCoilComponents(Sys(SysNum)%ReheatName,FirstHVACIteration,  &
                                                 CompIndex=Sys(SysNum)%ReheatComp_Index)
    CASE(HCoilType_SteamAirHeating) ! COIL:STEAM:AIRHEATING
         ! Simulate reheat coil for the VAV system
      CALL SimulateSteamCoilComponents(CompName=Sys(SysNum)%ReheatName,       &
                                            FirstHVACIteration=FirstHVACIteration, &
                                            QCoilReq=0.0d0,       &
                                            CompIndex=Sys(SysNum)%ReheatComp_Index)


    CASE(HCoilType_Electric) ! COIL:ELECTRIC:HEATING
         ! Simulate reheat coil for the VAV system
      CALL SimulateHeatingCoilComponents(CompName=Sys(SysNum)%ReheatName,       &
                                            FirstHVACIteration=FirstHVACIteration, &
                                            QCoilReq=0.0d0,       &
                                            CompIndex=Sys(SysNum)%ReheatComp_Index)

    CASE(HCoilType_Gas) ! COIL:GAS:HEATING
         ! Simulate reheat coil for the VAV system
      CALL SimulateHeatingCoilComponents(CompName=Sys(SysNum)%ReheatName,       &
                                            FirstHVACIteration=FirstHVACIteration, &
                                            QCoilReq=0.0d0,       &
                                            CompIndex=Sys(SysNum)%ReheatComp_Index)
    CASE(HCoilType_None) ! blank
         ! If no reheat is defined then assume that the damper is the only component.
         ! If something else is that is not a reheat coil or a blank then give the error message

    CASE DEFAULT
      CALL ShowFatalError('Invalid Reheat Component='//TRIM(Sys(SysNum)%ReheatComp))
    END SELECT

  END IF

!  set OA report variable
  Sys(SysNum)%OutdoorAirFlowRate = (MassFlow/StdRhoAir) * AirLoopOAFrac

   ! push the flow rate history
  MassFlow3(SysNum) = MassFlow2(SysNum)
  MassFlow2(SysNum) = MassFlow1(SysNum)
  MassFlow1(SysNum) = MassFlow

  RETURN
END SUBROUTINE SimVAV

SUBROUTINE CalcOAMassFlow(SysNum, SAMassFlow, AirLoopOAFrac)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad (FSEC)
          !       DATE WRITTEN   Jan 2010
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
    INTEGER, INTENT(IN)      :: SysNum        ! index to terminal unit
    REAL(r64), INTENT(INOUT) :: SAMassFlow    ! outside air based on optional user input
    REAL(r64), INTENT(INOUT) :: AirLoopOAFrac ! outside air based on optional user input

          ! FUNCTION PARAMETER DEFINITIONS:
    LOGICAL, PARAMETER   :: UseMinOASchFlag = .TRUE. ! Always use min OA schedule in calculations.

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

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

     If(Sys(SysNum)%CtrlZoneNum .GT. 0) AirLoopNum = ZoneEquipConfig(Sys(SysNum)%CtrlZoneNum)%AirLoopNum

     ! Calculate the amount of OA based on optional user inputs
     IF(AirLoopNum .GT. 0)THEN
       AirLoopOAFrac = AirLoopFlow(AirLoopNum)%OAFrac
       ! If no additional input from user, RETURN from subroutine
       IF(Sys(SysNum)%NoOAFlowInputFromUser)RETURN
       ! Calculate outdoor air flow rate, zone multipliers are applied in GetInput
       IF(AirLoopOAFrac .GT. 0.0D0)THEN
         OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir(Sys(SysNum)%OARequirementsPtr, &
                            Sys(SysNum)%ActualZoneNum, AirLoopControlInfo(AirLoopNum)%AirLoopDCVFlag, UseMinOASchFlag)
         RhoAir = PsyRhoAirFnPbTdbW(Node(Sys(SysNum)%InletNodeNum)%Press, &
                                    Node(Sys(SysNum)%InletNodeNum)%Temp, &
                                    Node(Sys(SysNum)%InletNodeNum)%HumRat)
         OAMassFlow = OAVolumeFlowRate * RhoAir

         ! convert OA mass flow rate to supply air flow rate based on air loop OA fraction
         SAMassFlow = OAMassFlow / AirLoopOAFrac

       END IF

     END IF

  RETURN
END SUBROUTINE CalcOAMassFlow

SUBROUTINE SimCBVAV(SysNum,FirstHVACIteration, ZoneNum, ZoneNodeNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   August 2006
          !       MODIFIED       KHL/TH 10/2010: added maximum supply air temperature leaving reheat coil
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the VAV box with varying airflow in heating and cooling.
          ! Modified version of SimVAV.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
   USE DataZoneEnergyDemands
   USE DataHVACGlobals,      ONLY: SmallLoad
!unused   USE DataHeatBalFanSys,    ONLY: Mat
   USE WaterCoils,           ONLY: SimulateWaterCoilComponents
   USE HeatingCoils,         ONLY: SimulateHeatingCoilComponents
   USE SteamCoils,           ONLY: SimulateSteamCoilComponents
   USE DataDefineEquip,      ONLY: AirDistUnit
!unused   USE DataHeatBalFanSys,    ONLY: ZoneThermostatSetPointHi, ZoneThermostatSetPointLo
   USE PlantUtilities,       ONLY: SetActuatedBranchFlowRate

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   INTEGER, INTENT (IN):: SysNum
   INTEGER, INTENT (IN):: ZoneNum
   INTEGER, INTENT (IN):: ZoneNodeNum
   LOGICAL, INTENT (IN):: FirstHVACIteration


          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   REAL(r64)    :: MassFlow         ! Total Mass Flow Rate from Hot & Cold Inlets [kg/sec]
   REAL(r64)    :: QTotLoad         ! Total load based on thermostat setpoint temperature [Watts]
   REAL(r64)    :: QZnReq           ! Total load to be met by terminal heater [Watts]
   REAL(r64)    :: QToHeatSetPt     ! Remaining load to heating setpoint [W]
   REAL(r64)    :: QSupplyAir       ! Zone load met by VAVHeatandCool system
   REAL(r64)    :: CpAirZn          ! Specific heat of zone air [J/kg-C]
   REAL(r64)    :: CpAirSysIn       ! Specific heat of VAVHeatandCool box entering air [J/kg-C]
   REAL(r64)    :: DeltaTemp        ! Temperature difference multiplied by specific heat [J/kg]
   REAL(r64)    :: MaxFlowWater     ! This is the value passed to the Controller depending if FirstHVACIteration or not
   REAL(r64)    :: MinFlowWater     ! This is the value passed to the Controller depending if FirstHVACIteration or not
   REAL(r64)    :: LeakLoadMult     ! Load multiplier to adjust for downstream leaks
   INTEGER :: ADUNum           ! Index of air distribution unit for this terminal unit
   INTEGER :: SysOutletNode    ! The node number of the terminal unit outlet node
   INTEGER :: SysInletNode     ! The node number of the terminal unit inlet node
   INTEGER :: WaterControlNode ! This is the Actuated Reheat Control Node
   REAL(r64)  :: DummyMdot
   REAL(r64) :: QActualHeating
   REAL(r64) :: MinFlowFrac       ! minimum flow fraction (and minimum damper position)
   REAL(r64) :: ZoneTemp = 0.0D0         ! zone air temperature [C]
   REAL(r64) :: MaxHeatTemp = 0.0D0      ! maximum supply air temperature [C]
   REAL(r64) :: MassFlowReq = 0.0D0      ! air mass flow rate required to meet the coil heating load [W]
   REAL(r64) :: MassFlowActual = 0.0D0   ! air mass flow rate actually used [W]
   REAL(r64) :: QZoneMax = 0.0D0         ! maximum zone heat addition rate given constraints of MaxHeatTemp and max
                                         ! available air mass flow rate [W]
   REAL(r64) :: MinMassAirFlow = 0.0D0   ! the air flow rate during heating for normal acting damper
   REAL(r64) :: QZoneMax2 = 0.0D0        ! temporary variable
   REAL(r64) :: QZoneMax3 = 0.0D0        ! temporary variable

   ! Sys(SysNum)%InletNodeNum is the inlet node to the terminal unit and the damper
   ! Sys(SysNum)%OutletNodeNum is the outlet node of the damper and the inlet node of the heating coil
   ! Sys(SysNum)%ReheatAirOutletNode is the outlet node of the terminal unit and the heating coil

   ! The calculated load from the Heat Balance
   ADUNum = Sys(SysNum)%ADUNum
   LeakLoadMult = AirDistUnit(ADUNum)%LeakLoadMult
   QTotLoad=ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired * LeakLoadMult
   QToHeatSetPt=ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP * LeakLoadMult
   SysOutletNode = Sys(SysNum)%ReheatAirOutletNode
   SysInletNode = Sys(SysNum)%InletNodeNum
   CpAirZn = PsyCpAirFnWTdb(Node(ZoneNodeNum)%HumRat,Node(ZoneNodeNum)%Temp)
   MinFlowFrac = Sys(SysNum)%ZoneMinAirFrac
   MinMassAirFlow = MinFlowFrac * StdRhoAir * Sys(SysNum)%MaxAirVolFlowRate
   ZoneTemp = Node(ZoneNodeNum)%Temp

   !Then depending on if the Load is for heating or cooling it is handled differently.  First
   ! the massflow rate for cooling is determined to meet the entire load.  Then
   ! if the massflow is below the minimum or greater than the Max it is set to either the Min
   ! or the Max as specified for the VAV model.
   If(SysInlet(SysNum)%AirMassFlowRateMaxAvail .GT. 0.0D0) Then
   ! Calculate the flow required for cooling
     CpAirSysIn = PsyCpAirFnWTdb(SysInlet(SysNum)%AirHumRat,SysInlet(SysNum)%AirTemp)
     DeltaTemp = CpAirSysIn*SysInlet(SysNum)%AirTemp - CpAirZn*ZoneTemp

     !Need to check DeltaTemp and ensure that it is not zero
     If(DeltaTemp .ne. 0.0D0) Then
        MassFlow= QTotLoad/DeltaTemp
     Else
        MassFlow = SysInlet(SysNum)%AirMassFlowRateMaxAvail
     End If

     !Check to see if the flow is < the Min or > the Max air Fraction to the zone; then set to min or max
     MassFlow = MAX(MassFlow,SysInlet(SysNum)%AirMassFlowRateMinAvail)
     MassFlow = MIN(MassFlow,SysInlet(SysNum)%AirMassFlowRateMaxAvail)
   Else
   ! System is Off set massflow to 0.0
     MassFlow = 0.0D0
   End If
   ! look for bang-bang condition: flow rate oscillating between 2 values during the air loop / zone
   ! equipment iteration. If detected, set flow rate to previous value.
   IF ( ( (ABS(MassFlow-MassFlow2(SysNum)) < MassFlowDiff(SysNum)) .OR. &
          (ABS(MassFlow-MassFlow3(SysNum)) < MassFlowDiff(SysNum)) ) .AND. &
          (ABS(MassFlow-MassFlow1(SysNum)) >= MassFlowDiff(SysNum)) ) THEN
     MassFlow = MassFlow1(SysNum)
   END IF

   !Move data to the damper outlet node
   SysOutlet(SysNum)%AirTemp         = SysInlet(SysNum)%AirTemp
   SysOutlet(SysNum)%AirHumRat       = SysInlet(SysNum)%AirHumRat
   SysOutlet(SysNum)%AirMassFlowRate = MassFlow
   SysOutlet(SysNum)%AirMassFlowRateMaxAvail = SysInlet(SysNum)%AirMassFlowRateMaxAvail
   SysOutlet(SysNum)%AirMassFlowRateMinAvail = SysInlet(SysNum)%AirMassFlowRateMinAvail
   SysOutlet(SysNum)%AirEnthalpy             = SysInlet(SysNum)%AirEnthalpy

   ! Calculate the Damper Position when there is a Max air flow specified.
   If(Sys(Sysnum)%AirMassFlowRateMax == 0.0D0) Then
     Sys(Sysnum)%DamperPosition = 0.0D0
   Else
     Sys(Sysnum)%DamperPosition = MassFlow/Sys(Sysnum)%AirMassFlowRateMax
   End If

   !Need to make sure that the damper outlets are passed to the coil inlet
   Call UpdateSys(SysNum)

   QActualHeating = QToHeatSetPt - Massflow * CpAirZn * (SysInlet(SysNum)%AirTemp-ZoneTemp)

   If( (MassFlow > SmallMassFlow) .AND. &
      (QActualHeating > 0.0D0) .AND. (TempControlType(ZoneNum) .NE. SingleCoolingSetPoint) ) Then
!   VAVHeatandCool boxes operate at varying mass flow rates when reheating, VAV boxes operate at min flow
!      (MassFlow <= SysInlet(SysNum)%AirMassFlowRateMinAvail) .AND. &
!   Per Fred Buhl, don't use DeadBandOrSetback to determine if heaters operate
!      (.NOT. DeadBandOrSetback(ZoneNum))) Then

     ! At this point we know that there is a heating requirement: i.e., the heating coil needs to
     ! be activated (there's a zone heating load or there's a reheat requirement). There are 3 possible
     ! situations: 1) the coil load can be met by variable temperature air (below the max heat temp) at
     ! the minimum air mass flow rate; 2) the coil load can be met by variable air flow rate with the air
     ! temperature fixed at the max heat temp; 3) the load cannot be met (we will run at max air temp and
     ! max air flow rate). We check for condition 2 by assuming the air temperatute is at the max heat temp
     ! and solving for the air mass flow rate that will meet the load. If the flow rate is between the min and
     ! max we are in condition 2.

     QZoneMax2 = QToHeatSetPt

     IF (Sys(SysNum)%MaxReheatTempSetByUser) THEN

       MaxHeatTemp = Sys(SysNum)%MaxReheatTemp
       IF (QToHeatSetPt > SmallLoad) THEN ! zone has a postive load to heating setpoint
         MassFlowReq = QToHeatSetPt/(CpAirZn*(MaxHeatTemp - ZoneTemp))
       ELSE
         MassFlowReq = MassFlow
       ENDIF

       QZoneMax3 = CpAirZn * (MaxHeatTemp - ZoneTemp) * MassFlow

       MassFlowActual = MassFlow

       IF (QZoneMax3 < QToHeatSetPt) THEN
         MassFlowActual = MassFlowReq
         ! QZoneMax3 = CpAirZn * (MaxHeatTemp - ZoneTemp) * MassFlowActual
       END IF

       IF (MassFlowActual <= MinMassAirFlow) THEN
         MassFlowActual = MinMassAirFlow
       ELSE IF (MassFlowActual >= Sys(Sysnum)%AirMassFlowRateMax) THEN
         MassFlowActual = Sys(Sysnum)%AirMassFlowRateMax
       END IF

       QZoneMax = CpAirZn*MassFlowActual*(MaxHeatTemp - ZoneTemp)

       ! temporary variable
       QZoneMax2 = MIN(QZoneMax,QToHeatSetPt)

       MassFlow = MassFlowActual

     END IF     ! IF (Sys(SysNum)%MaxReheatTempSetByUser) THEN

     SysOutlet(SysNum)%AirMassFlowRate = MassFlow

     Call UpdateSys(SysNum)


     SELECT CASE(Sys(SysNum)%ReheatComp_Num)

       ! hot water heating coil
       CASE(HCoilType_SimpleHeating) ! COIL:WATER:SIMPLEHEATING
         ! Determine the load required to pass to the Component controller
         ! Although this equation looks strange (using temp instead of deltaT), it is corrected later in ControlCompOutput
         ! and is working as-is, temperature setpoints are maintained as expected.
         QZnReq = QZoneMax2 + MassFlow * CpAirZn * Node(ZoneNodeNum)%Temp
         IF(QZnReq .LT. SmallLoad)QZnReq = 0.0D0

         ! Initialize hot water flow rate to zero.
         ! Node(Sys(SysNum)%ReheatControlNode)%MassFlowRate = 0.0D0
         DummyMdot = 0.0D0
         CALL SetActuatedBranchFlowRate(DummyMdot,Sys(SysNum)%ReheatControlNode,  &
              Sys(SysNum)%HWLoopNum,Sys(SysNum)%HWLoopSide, Sys(SysNum)%HWBranchIndex, .TRUE. )
!On the first HVAC iteration the system values are given to the controller, but after that
! the demand limits are in place and there needs to be feedback to the Zone Equipment
         If(FirstHVACIteration)Then
            MaxFlowWater = Sys(SysNum)%MaxReheatWaterFlow
            MinFlowWater = Sys(SysNum)%MinReheatWaterFlow
         Else
            WaterControlNode = Sys(SysNum)%ReheatControlNode
            MaxFlowWater = Node(WaterControlNode)%MassFlowRateMaxAvail
            MinFlowWater = Node(WaterControlNode)%MassFlowRateMinAvail
         EndIf

         ! Simulate the reheat coil at constant air flow. Control by varying the
         ! hot water flow rate.
         CALL ControlCompOutput(CompName=Sys(SysNum)%ReheatName,         &
                            CompType=Sys(SysNum)%ReheatComp,             &
                            CompNum=Sys(SysNum)%ReheatComp_Index,        &
                            FirstHVACIteration=FirstHVACIteration,       &
                            QZnReq=QZnReq,                               &
                            ActuatedNode=Sys(SysNum)%ReheatControlNode,  &
                            MaxFlow=MaxFlowWater,                        &
                            MinFlow=MinFlowWater,                        &
                            TempOutNode=SysOutletNode,                   &
                            ControlOffSet=Sys(SysNum)%ControllerOffset,  &
                            AirMassFlow=Massflow,                        &
                            ControlCompTypeNum=Sys(SysNum)%ControlCompTypeNum, &
                            CompErrIndex=Sys(SysNum)%CompErrIndex, &
                            LoopNum     = Sys(SysNum)%HWLoopNum,              &
                            LoopSide    = Sys(SysNum)%HWLoopSide,             &
                            BranchIndex = Sys(SysNum)%HWBranchIndex)


         ! If reverse action damper and the hot water flow is at maximum, simulate the
         ! hot water coil with fixed (maximum) hot water flow but allow the air flow to
         ! vary up to the maximum (air damper opens to try to meet zone load).
         IF (Sys(SysNum)%DamperHeatingAction .EQ. ReverseAction) THEN
           IF (Node(Sys(SysNum)%ReheatControlNode)%MassFlowRate .EQ. Sys(SysNum)%MaxReheatWaterFlow) THEN
             CALL ControlCompOutput(CompName=Sys(SysNum)%ReheatName,                  &
                                    CompType=Sys(SysNum)%ReheatComp,                  &
                                    CompNum=Sys(SysNum)%ReheatComp_Index,             &
                                    FirstHVACIteration=FirstHVACIteration,            &
                                    QZnReq=QZoneMax2,                                  &
                                    ActuatedNode=Sys(SysNum)%OutletNodeNum,           &
                                    MaxFlow=SysInlet(SysNum)%AirMassFlowRateMaxAvail, &
                                    MinFlow=SysInlet(SysNum)%AirMassFlowRateMinAvail, &
                                    TempOutNode=SysOutletNode,                        &
                                    TempInNode=ZoneNodeNum,                           &
                                    ControlOffSet=Sys(SysNum)%ControllerOffset,       &
                                    ControlCompTypeNum=Sys(SysNum)%ControlCompTypeNum, &
                                    CompErrIndex=Sys(SysNum)%CompErrIndex )
!                                   ! air flow controller, not on plant, don't pass plant topology info

             ! reset terminal unit inlet air mass flow to new value.
             MassFlow = Node(SysOutletNode)%MassFlowRate
             SysOutlet(SysNum)%AirMassFlowRate = MassFlow
             Call UpdateSys(SysNum)
           END IF
           ! look for bang-bang condition: flow rate oscillating between 2 values during the air loop / zone
           ! equipment iteration. If detected, set flow rate to previous value and recalc HW flow.
           IF ( ( (ABS(MassFlow-MassFlow2(SysNum)) < MassFlowDiff(SysNum)) .OR. &
            (ABS(MassFlow-MassFlow3(SysNum)) < MassFlowDiff(SysNum)) ) .AND. &
            (ABS(MassFlow-MassFlow1(SysNum)) >= MassFlowDiff(SysNum)) ) THEN
             MassFlow = MassFlow1(SysNum)
             SysOutlet(SysNum)%AirMassFlowRate = MassFlow
             Call UpdateSys(SysNum)
             CALL ControlCompOutput(CompName=Sys(SysNum)%ReheatName,           &
                                CompType=Sys(SysNum)%ReheatComp,             &
                                CompNum=Sys(SysNum)%ReheatComp_Index,        &
                                FirstHVACIteration=FirstHVACIteration,       &
                                QZnReq=QZnReq,                               &
                                ActuatedNode=Sys(SysNum)%ReheatControlNode,  &
                                MaxFlow=MaxFlowWater,                        &
                                MinFlow=MinFlowWater,                        &
                                TempOutNode=SysOutletNode,                   &
                                ControlOffSet=Sys(SysNum)%ControllerOffset,  &
                                AirMassFlow=Massflow,                        &
                                ControlCompTypeNum=Sys(SysNum)%ControlCompTypeNum, &
                                CompErrIndex=Sys(SysNum)%CompErrIndex, &
                                LoopNum     = Sys(SysNum)%HWLoopNum,              &
                                LoopSide    = Sys(SysNum)%HWLoopSide,             &
                                BranchIndex = Sys(SysNum)%HWBranchIndex)

           END IF
           ! recalculate damper position
           If(Sys(Sysnum)%AirMassFlowRateMax == 0.0D0) Then
             Sys(Sysnum)%DamperPosition = 0.0D0
           Else
             Sys(Sysnum)%DamperPosition = MassFlow/Sys(Sysnum)%AirMassFlowRateMax
           End If
         END IF
       CASE(HCoilType_SteamAirHeating) ! ! COIL:STEAM:AIRHEATING
         ! Determine the load required to pass to the Component controller
         QZnReq = QZoneMax2 - Massflow * CpAirZn * (SysInlet(SysNum)%AirTemp-ZoneTemp)
         IF(QZnReq .LT. SmallLoad)QZnReq = 0.0D0

         ! Simulate reheat coil for the VAV system
         CALL SimulateSteamCoilComponents (CompName=Sys(SysNum)%ReheatName,        &
                                           CompIndex=Sys(SysNum)%ReheatComp_Index, &
                                           FirstHVACIteration=FirstHVACIteration,  &
                                           QCoilReq=QZnReq)

       CASE(HCoilType_Electric) ! COIL:ELECTRIC:HEATING
         ! Determine the load required to pass to the Component controller
         QSupplyAir = Massflow * CpAirZn * (SysInlet(SysNum)%AirTemp-ZoneTemp)
         QZnReq = QZoneMax2 - QSupplyAir
         IF(QZnReq .LT. SmallLoad)QZnReq = 0.0D0

         ! Simulate reheat coil for the VAV system
         CALL SimulateHeatingCoilComponents(CompName=Sys(SysNum)%ReheatName,        &
                                            CompIndex=Sys(SysNum)%ReheatComp_Index, &
                                            FirstHVACIteration=FirstHVACIteration,  &
                                            QCoilReq=QZnReq)

       CASE(HCoilType_Gas) ! COIL:GAS:HEATING
         ! Determine the load required to pass to the Component controller
         QZnReq = QZoneMax2 - Massflow * CpAirZn * (SysInlet(SysNum)%AirTemp-ZoneTemp)
         IF(QZnReq .LT. SmallLoad)QZnReq = 0.0D0

         ! Simulate reheat coil for the VAV system
         CALL SimulateHeatingCoilComponents(CompName=Sys(SysNum)%ReheatName,       &
                                            CompIndex=Sys(SysNum)%ReheatComp_Index, &
                                            FirstHVACIteration=FirstHVACIteration, &
                                            QCoilReq=QZnReq)
       CASE(HCoilType_None) ! blank
         ! If no reheat is defined then assume that the damper is the only component.
         ! If something else is there that is not a reheat coil then give the error message below.

       CASE DEFAULT
         CALL ShowFatalError('Invalid Reheat Component='//TRIM(Sys(SysNum)%ReheatComp))
     END SELECT

   !the COIL is OFF the properties are calculated for this special case.
   Else
     SELECT CASE(Sys(SysNum)%ReheatComp_Num)

       CASE(HCoilType_SimpleHeating) ! COIL:WATER:SIMPLEHEATING
         ! Simulate reheat coil for the Const Volume system
        ! Node(Sys(SysNum)%ReheatControlNode)%MassFlowRate = 0.0D0
         ! Initialize hot water flow rate to zero.
         DummyMdot = 0.0D0
         CALL SetActuatedBranchFlowRate(DummyMdot,Sys(SysNum)%ReheatControlNode,  &
              Sys(SysNum)%HWLoopNum,Sys(SysNum)%HWLoopSide, Sys(SysNum)%HWBranchIndex, .TRUE. )

         !call the reheat coil with the NO FLOW condition to make sure that the Node values
         ! are passed through to the coil outlet correctly
         CALL SimulateWaterCoilComponents(Sys(SysNum)%ReheatName,FirstHVACIteration,  &
                                                 CompIndex=Sys(SysNum)%ReheatComp_Index)
       CASE(HCoilType_SteamAirHeating) ! COIL:STEAM:AIRHEATING
         ! Simulate reheat coil for the VAV system
         CALL SimulateSteamCoilComponents(CompName=Sys(SysNum)%ReheatName,       &
                                            FirstHVACIteration=FirstHVACIteration, &
                                            QCoilReq=0.0d0,       &
                                            CompIndex=Sys(SysNum)%ReheatComp_Index)


       CASE(HCoilType_Electric) ! COIL:ELECTRIC:HEATING
         ! Simulate reheat coil for the VAV system
         CALL SimulateHeatingCoilComponents(CompName=Sys(SysNum)%ReheatName,       &
                                            FirstHVACIteration=FirstHVACIteration, &
                                            QCoilReq=0.0d0,       &
                                            CompIndex=Sys(SysNum)%ReheatComp_Index)

       CASE(HCoilType_Gas) ! COIL:GAS:HEATING
         ! Simulate reheat coil for the VAV system
         CALL SimulateHeatingCoilComponents(CompName=Sys(SysNum)%ReheatName,       &
                                            FirstHVACIteration=FirstHVACIteration, &
                                            QCoilReq=0.0d0,       &
                                            CompIndex=Sys(SysNum)%ReheatComp_Index)
       CASE(HCoilType_None) ! blank
         ! If no reheat is defined then assume that the damper is the only component.
         ! If something else is there that is not a reheat coil then give the error message

       CASE DEFAULT
         CALL ShowFatalError('Invalid Reheat Component='//TRIM(Sys(SysNum)%ReheatComp))
     END SELECT

   End IF
   ! push the flow rate history
   MassFlow3(SysNum) = MassFlow2(SysNum)
   MassFlow2(SysNum) = MassFlow1(SysNum)
   MassFlow1(SysNum) = MassFlow

 RETURN
END SUBROUTINE SimCBVAV

SUBROUTINE SimVAVVS(SysNum,FirstHVACIteration, ZoneNum, ZoneNodeNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   July 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates a single duct VAV terminal unit with a variable-speed fan upstream
          ! and a reheat coil on the downstream side.

          ! METHODOLOGY EMPLOYED:
          ! Define a compound component in CalcVAVVS. Break the heating/cooling load into 4 regions based
          ! on equip on/off combinations. Assign the heating load to the appropriate region and iteratively
          ! solve for the appropriate control variable value using Regula-Falsi solver.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands
  USE DataConvergParams, ONLY: HVACFlowRateToler
  USE General, ONLY: SolveRegulaFalsi
  USE SteamCoils, ONLY: GetCoilCapacity

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: SysNum
  INTEGER, INTENT(IN) :: ZoneNum
  INTEGER, INTENT (IN):: ZoneNodeNum
  LOGICAL, INTENT (IN):: FirstHVACIteration


          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: BigLoad = 1.0d+20

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: MassFlow    ! [kg/sec]   Total Mass Flow Rate from Hot & Cold Inlets
  REAL(r64) :: QTotLoad    ! [Watts]
!unused  REAL(r64) :: QZnReq      ! [Watts]
  REAL(r64) :: CpAirZn
!unused  REAL(r64) :: CpAirSysIn
!unused  REAL(r64) :: DeltaTemp
  Integer :: SysOutletNode  ! The node number of the terminal unit outlet node
  Integer :: SysInletNode   ! the node number of the terminal unit inlet node
  Integer:: WaterControlNode   !This is the Actuated Reheat Control Node
  Integer:: SteamControlNode
  REAL(r64) :: MaxFlowWater  !This is the value passed to the Controller depending if FirstHVACIteration or not
  REAL(r64) :: MinFlowWater  !This is the value passed to the Controller depending if FirstHVACIteration or not
  REAL(r64)  ::MaxFlowSteam !This is the value passed to the Controller depending if FirstHVACIteration or not
  REAL(r64)  ::MinFlowSteam !This is the value passed to the Controller depending if FirstHVACIteration or not
  REAL(r64) :: HWFlow        ! the hot water flow rate [kg/s]
  REAL(r64) :: QCoolFanOnMax ! max cooling - fan at max flow; note that cooling is always < 0. [W]
  REAL(r64) :: QCoolFanOnMin ! min active cooling with fan on - fan at lowest speed. [W]
  REAL(r64) :: QHeatFanOnMax ! max heating - fan at heat flow max, hot water flow at max [W]
  REAL(r64) :: QHeatFanOnMin ! min heating - fan at min flow, hot water at max flow [W]
  REAL(r64) :: QHeatFanOffMax ! max heating - fan off, hot water flow at max [W]
  REAL(r64) :: QNoHeatFanOff ! min heating - fan off, hot water at min flow [W]
  Integer :: HCType     ! heating coil type (as a number)
  Integer :: FanType    ! fan type (as a number)
  REAL(r64) :: HCLoad        ! load passed to a gas or electric heating coil [W]
  Integer :: FanOp      ! 1 if fan is on; 0 if off.
  REAL(r64) :: MaxCoolMassFlow  ! air flow at max cooling [kg/s]
  REAL(r64) :: MaxHeatMassFlow  ! air flow at max heating [kg/s]
  REAL(r64) :: MinMassFlow      ! minimum air flow rate [kg/s]
  REAL(r64) :: UnitFlowToler    ! flow rate tolerance
  REAL(r64) :: QDelivered
  REAL(r64) :: FracDelivered
  REAL(r64), DIMENSION(11)  :: Par
  INTEGER :: SolFlag
  REAL(r64) :: ErrTolerance
  REAL(r64) :: MaxSteamCap ! steam coil capacity at full load
  Logical :: ErrorsFound   ! returned from mining function call


  ! The calculated load from the Heat Balance
  QTotLoad=ZoneSysEnergyDemand(ZoneNum)%RemainingOutputRequired
  SysOutletNode = Sys(SysNum)%ReheatAirOutletNode
  SysInletNode = Sys(SysNum)%InletNodeNum
  CpAirZn = PsyCpAirFnWTdb(Node(ZoneNodeNum)%HumRat,Node(ZoneNodeNum)%Temp)
  HCType = Sys(SysNum)%ReheatComp_Num
  FanType = Sys(SysNum)%Fan_Num
  MaxCoolMassFlow = SysInlet(SysNum)%AirMassFlowRateMaxAvail
  MaxHeatMassFlow = MIN(Sys(SysNum)%HeatAirMassFlowRateMax,SysInlet(SysNum)%AirMassFlowRateMaxAvail)
  MinMassFlow = MaxCoolMassFlow * Sys(SysNum)%ZoneMinAirFrac
  UnitFlowToler = 0.001D0*HVACFlowRateToler
  QDelivered = 0.0D0
  HWFlow = 0.0D0
  IF (SysInlet(SysNum)%AirMassFlowRateMaxAvail <= 0.0D0 .OR. CurDeadbandOrSetback(ZoneNum)) THEN
    MassFlow = 0.0D0
    FanOp = 0
    CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,0.0d0,0.0d0,  &
                               FanType,MassFlow,FanOp,QDelivered)
    RETURN
  END IF

  IF (HCType == HCoilType_SimpleHeating) THEN
    WaterControlNode = Sys(SysNum)%ReheatControlNode
    HCLoad = 0.0D0
    If (FirstHVACIteration) Then
      MaxFlowWater = Sys(SysNum)%MaxReheatWaterFlow
      MinFlowWater = Sys(SysNum)%MinReheatWaterFlow
    Else
      WaterControlNode = Sys(SysNum)%ReheatControlNode
      MaxFlowWater = Node(WaterControlNode)%MassFlowRateMaxAvail
      MinFlowWater = Node(WaterControlNode)%MassFlowRateMinAvail
    EndIf
  ELSE
    WaterControlNode = 0
    HCLoad = BigLoad
    MaxFlowWater = 0.0D0
    MinFlowWater = 0.0D0
  END IF


  IF (HCType == HCoilType_SteamAirHeating) THEN
    SteamControlNode = Sys(SysNum)%ReheatControlNode
    HCLoad = 0.0D0
    If (FirstHVACIteration) Then
      MaxFlowSteam = Sys(SysNum)%MaxReheatSteamFlow
      MinFlowSteam = Sys(SysNum)%MinReheatSteamFlow
    Else
      SteamControlNode = Sys(SysNum)%ReheatControlNode
      MaxFlowSteam = Node(SteamControlNode)%MassFlowRateMaxAvail
      MinFlowSteam = Node(SteamControlNode)%MassFlowRateMinAvail
    EndIf
  ELSE
    SteamControlNode = 0
    HCLoad = BigLoad
    MaxFlowSteam = 0.0D0
    MinFlowSteam = 0.0D0
  END IF


  ! define 3 load regions and assign the current load to the correct region.
  ! region 1: active cooling with fan on
  FanOp = 1
 IF (HCType == HCoilType_SteamAirHeating) THEN
  CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,MinFlowSteam,0.0d0,  &
     FanType,MaxCoolMassFlow,FanOp,QCoolFanOnMax)
  CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,MinFlowSteam,0.0d0,  &
     FanType,MinMassFlow,FanOp,QCoolFanOnMin)
  ! region 2: active heating with fan on
  CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,MaxFlowSteam,BigLoad,FanType,MaxHeatMassFlow,FanOp,QHeatFanOnMax)
     MaxSteamCap = GetCoilCapacity(Sys(SysNum)%ReheatComp,Sys(SysNum)%ReheatName,ErrorsFound)
  CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,MaxFlowSteam,0.0d0,  &
     FanType,MinMassFlow,FanOp,QHeatFanOnMin)
  ! region 3: active heating with fan off
  FanOp = 0
  CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,MaxFlowSteam,BigLoad,FanType,MinMassFlow,FanOp,QHeatFanOffMax)
  CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,MinFlowSteam,0.0d0,  &
     FanType,MinMassFlow,FanOp,QNoHeatFanOff)
 Else
  CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,MinFlowWater,0.0d0,  &
     FanType,MaxCoolMassFlow,FanOp,QCoolFanOnMax)
  CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,MinFlowWater,0.0d0,  &
     FanType,MinMassFlow,FanOp,QCoolFanOnMin)
  ! region 2: active heating with fan on
  CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,MaxFlowWater,BigLoad,FanType,MaxHeatMassFlow,FanOp,QHeatFanOnMax)
  CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,MaxFlowWater,0.0d0,  &
     FanType,MinMassFlow,FanOp,QHeatFanOnMin)
  ! region 3: active heating with fan off
  FanOp = 0
  CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,MaxFlowWater,BigLoad,FanType,MinMassFlow,FanOp,QHeatFanOffMax)
  CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,MinFlowWater,0.0d0,  &
     FanType,MinMassFlow,FanOp,QNoHeatFanOff)
 EndIF

   ! Active cooling
  IF (QTotLoad < QCoolFanOnMin - SmallLoad .AND. SysInlet(SysNum)%AirMassFlowRateMaxAvail > 0.0D0 &
       .AND. .NOT. CurDeadBandOrSetback(ZoneNum)) THEN
    ! check that it can meet the load
    FanOp = 1
    IF (QCoolFanOnMax < QTotLoad - SmallLoad) THEN
      Par(1) = REAL(SysNum,r64)
      IF (FirstHVACIteration) THEN
        Par(2) = 1.0D0
      ELSE
        Par(2) = 0.0D0
      END IF
      Par(3) = REAL(ZoneNodeNum,r64)
      Par(4) = REAL(HCType,r64)
      IF (HCType == HCoilType_SteamAirHeating) THEN
         Par(5) = MinFlowSteam
      Else
         Par(5) = MinFlowWater
      End If
      Par(6) = REAL(FanType,r64)
      Par(7) = REAL(FanOp,r64)
      Par(8) = QTotLoad
      CALL SolveRegulaFalsi(UnitFlowToler, 50, SolFlag, MassFlow, VAVVSCoolingResidual, &
                            MinMassFlow, MaxCoolMassFlow, Par)
      IF (SolFlag == -1) THEN
        IF(Sys(SysNum)%IterationLimit == 0)THEN
          CALL ShowWarningError('Supply air flow control failed in VS VAV terminal unit '//TRIM(Sys(SysNum)%SysName))
          CALL ShowContinueError('  Iteration limit exceeded in calculating air flow rate')
        END IF
        CALL ShowRecurringWarningErrorAtEnd('Supply air flow Iteration limit exceeded in VS VAV terminal unit '//  &
                           TRIM(Sys(SysNum)%SysName),Sys(SysNum)%IterationLimit)
      ELSE IF (SolFlag == -2) THEN
        IF(Sys(SysNum)%IterationFailed == 0)THEN
          CALL ShowWarningError('Supply air flow control failed in VS VAV terminal unit '//TRIM(Sys(SysNum)%SysName))
          CALL ShowContinueError('  Bad air flow limits')
        END IF
        CALL ShowRecurringWarningErrorAtEnd('Supply air flow control failed in VS VAV terminal unit '//  &
                           TRIM(Sys(SysNum)%SysName),Sys(SysNum)%IterationFailed)
      END IF

    ELSE

      MassFlow = MaxCoolMassFlow

      IF (HCType == HCoilType_SteamAirHeating) THEN
         CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,MinFlowSteam,0.0d0,  &
            FanType,MassFlow,FanOp,QDelivered)
      Else
         CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,MinFlowWater,0.0d0,  &
            FanType,MassFlow,FanOp,QDelivered)
      End If

    END IF

  ! no active heating or cooling
  ELSE IF ( (QTotLoad >= QCoolFanOnMin - SmallLoad .AND. QTotLoad <= QNoHeatFanOff + SmallLoad .AND. &
             SysInlet(SysNum)%AirMassFlowRateMaxAvail > 0.0D0) .OR. (SysInlet(SysNum)%AirMassFlowRateMaxAvail > 0.0D0 &
             .AND. CurDeadBandOrSetback(ZoneNum)) ) THEN
    MassFlow = MinMassFlow
    FanOp = 0
      IF (HCType == HCoilType_SteamAirHeating) THEN
         CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,MinFlowSteam,QTotLoad,  &
            FanType,MassFlow,FanOp,QNoHeatFanOff)
      Else
         CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,MinFlowWater,0.0d0,  &
            FanType,MassFlow,FanOp,QNoHeatFanOff)
      End If

  ! active heating
  ELSE IF (QTotLoad > QNoHeatFanOff + SmallLoad .AND. SysInlet(SysNum)%AirMassFlowRateMaxAvail > 0.0D0 &
           .AND. .NOT. CurDeadBandOrSetback(ZoneNum)) THEN
    ! hot water coil
    IF (HCType == HCoilType_SimpleHeating) THEN
      IF (QTotLoad < QHeatFanOffMax - SmallLoad) THEN
        ! vary HW flow, leave air flow at minimum
        MassFlow = MinMassFlow
        FanOp = 0
        Par(1) = REAL(SysNum,r64)
        IF (FirstHVACIteration) THEN
          Par(2) = 1.0D0
        ELSE
          Par(2) = 0.0D0
        END IF
        Par(3) = REAL(ZoneNodeNum,r64)
        Par(4) = REAL(HCType,r64)
        Par(5) = MassFlow
        Par(6) = REAL(FanType,r64)
        Par(7) = REAL(FanOp,r64)
        Par(8) = QTotLoad
        ErrTolerance=Sys(SysNum)%ControllerOffset
        CALL SolveRegulaFalsi(ErrTolerance, 500, SolFlag, HWFlow, VAVVSHWNoFanResidual, &
                              MinFlowWater, MaxFlowWater, Par)
        IF (SolFlag == -1) THEN
          CALL ShowRecurringWarningErrorAtEnd('Hot Water flow control failed in VS VAV terminal unit '//  &
                           TRIM(Sys(SysNum)%SysName),Sys(SysNum)%ErrCount1)
          CALL ShowRecurringContinueErrorAtEnd('...Iteration limit (500) exceeded in calculating the hot water flow rate',  &
                           Sys(SysNum)%ErrCount1c)
          CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,HWFlow,0.0d0,  &
             FanType,MassFlow,FanOp,QDelivered)
        ELSE IF (SolFlag == -2) THEN
          CALL ShowRecurringWarningErrorAtEnd('Hot Water flow control failed (bad air flow limits) in VS VAV terminal unit '//  &
                             TRIM(Sys(SysNum)%SysName),Sys(SysNum)%ErrCount2)
        END IF
      ELSE IF (QTotLoad >= QHeatFanOffMax - SmallLoad .AND. QTotLoad <= QHeatFanOnMin + SmallLoad) THEN
        MassFlow = MinMassFlow
        FanOp = 0
        CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,MaxFlowWater,0.0d0,  &
           FanType,MassFlow,FanOp,QDelivered)
      ELSE IF (QTotLoad > QHeatFanOnMin + SmallLoad .AND. QTotLoad < QHeatFanOnMax - SmallLoad) THEN
        ! set hot water flow to max and vary the supply air flow rate
        FanOp = 1
        Par(1) = REAL(SysNum,r64)
        IF (FirstHVACIteration) THEN
          Par(2) = 1.0D0
        ELSE
          Par(2) = 0.0D0
        END IF
        Par(3) = REAL(ZoneNodeNum,r64)
        Par(4) = REAL(HCType,r64)
        Par(5) = MaxFlowWater
        Par(6) = REAL(FanType,r64)
        Par(7) = REAL(FanOp,r64)
        Par(8) = QTotLoad
        CALL SolveRegulaFalsi(UnitFlowToler, 50, SolFlag, MassFlow, VAVVSHWFanOnResidual, &
                              MinMassFlow, MaxHeatMassFlow, Par)
        IF (SolFlag == -1) THEN
          IF(Sys(SysNum)%IterationLimit == 0)THEN
            CALL ShowWarningError('Supply air flow control failed in VS VAV terminal unit '//TRIM(Sys(SysNum)%SysName))
            CALL ShowContinueError('  Iteration limit exceeded in calculating air flow rate')
          END IF
          CALL ShowRecurringWarningErrorAtEnd('Supply air flow Iteration limit exceeded in VS VAV terminal unit '//  &
                           TRIM(Sys(SysNum)%SysName),Sys(SysNum)%IterationLimit)
        ELSE IF (SolFlag == -2) THEN
          IF(Sys(SysNum)%IterationFailed == 0)THEN
            CALL ShowWarningError('Supply air flow control failed in VS VAV terminal unit '//TRIM(Sys(SysNum)%SysName))
            CALL ShowContinueError('  Bad air flow limits')
          END IF
          CALL ShowRecurringWarningErrorAtEnd('Supply air flow control failed in VS VAV terminal unit '//  &
                           TRIM(Sys(SysNum)%SysName),Sys(SysNum)%IterationFailed)
        END IF
      ELSE
        MassFlow = MaxHeatMassFlow
        FanOp = 1
        CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,MaxFlowWater,0.0d0,  &
           FanType,MassFlow,FanOp,QDelivered)
      END IF
    ELSE IF (HCType == HCoilType_SteamAirHeating) THEN
!      IF (QTotLoad > QNoHeatFanOff + SmallLoad .AND. QTotLoad < QHeatFanOffMax - SmallLoad) THEN
      IF (QTotLoad < QHeatFanOffMax - SmallLoad) THEN
        ! vary steam flow, leave air flow at minimum
        MassFlow = MinMassFlow
        FanOp = 0
        Par(1) = REAL(SysNum,r64)
        IF (FirstHVACIteration) THEN
          Par(2) = 1.0D0
        ELSE
          Par(2) = 0.0D0
        END IF
        Par(3) = REAL(ZoneNodeNum,r64)
        Par(4) = REAL(HCType,r64)
        Par(5) = MassFlow
        Par(6) = REAL(FanType,r64)
        Par(7) = REAL(FanOp,r64)
        Par(8) = QTotLoad
        Par(9) = MinFlowSteam
        Par(10) = MaxFlowSteam
        Par(11) = MaxSteamCap
        ErrTolerance=Sys(SysNum)%ControllerOffset
        CALL SolveRegulaFalsi(ErrTolerance, 500, SolFlag, HWFlow, VAVVSHWNoFanResidual, &
                              MinFlowSteam, MaxFlowSteam, Par)
        IF (SolFlag == -1) THEN
          CALL ShowRecurringWarningErrorAtEnd('Steam flow control failed in VS VAV terminal unit '//  &
                           TRIM(Sys(SysNum)%SysName),Sys(SysNum)%ErrCount1)
          CALL ShowRecurringContinueErrorAtEnd('...Iteration limit (500) exceeded in calculating the hot water flow rate',  &
                           Sys(SysNum)%ErrCount1c)
          CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,HWFlow,0.0d0,  &
             FanType,MassFlow,FanOp,QDelivered)
        ELSE IF (SolFlag == -2) THEN
          CALL ShowRecurringWarningErrorAtEnd('Steam flow control failed (bad air flow limits) in VS VAV terminal unit '//  &
                             TRIM(Sys(SysNum)%SysName),Sys(SysNum)%ErrCount2)
        END IF
      ELSE IF (QTotLoad >= QHeatFanOffMax - SmallLoad .AND. QTotLoad <= QHeatFanOnMin + SmallLoad) THEN
        MassFlow = MinMassFlow
        FanOp = 0
        CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,MaxFlowWater,0.0d0,  &
           FanType,MassFlow,FanOp,QDelivered)
      ELSE IF (QTotLoad > QHeatFanOnMin + SmallLoad .AND. QTotLoad < QHeatFanOnMax - SmallLoad) THEN
        FanOp = 1
        Par(1) = REAL(SysNum,r64)
        IF (FirstHVACIteration) THEN
          Par(2) = 1.d0
        ELSE
          Par(2) = 0.0d0
        END IF
        Par(3) = REAL(ZoneNodeNum,r64)
        Par(4) = REAL(HCType,r64)
        Par(5) = MaxFlowSteam
        Par(6) = REAL(FanType,r64)
        Par(7) = REAL(FanOp,r64)
        Par(8) = QTotLoad
        CALL SolveRegulaFalsi(UnitFlowToler, 50, SolFlag, MassFlow, VAVVSHWFanOnResidual, &
                              MinMassFlow, MaxHeatMassFlow, Par)
        IF (SolFlag == -1) THEN
          IF(Sys(SysNum)%IterationLimit == 0)THEN
            CALL ShowWarningError('Steam heating coil control failed in VS VAV terminal unit '//TRIM(Sys(SysNum)%SysName))
            CALL ShowContinueError('  Iteration limit exceeded in calculating air flow rate')
          END IF
          CALL ShowRecurringWarningErrorAtEnd('Steam heating coil iteration limit exceeded in VS VAV terminal unit '//  &
                           TRIM(Sys(SysNum)%SysName),Sys(SysNum)%IterationLimit)
        ELSE IF (SolFlag == -2) THEN
          IF(Sys(SysNum)%IterationFailed == 0)THEN
            CALL ShowWarningError('Steam heating coil control failed in VS VAV terminal unit '//TRIM(Sys(SysNum)%SysName))
            CALL ShowContinueError('  Bad air flow limits')
          END IF
          CALL ShowRecurringWarningErrorAtEnd('Steam heating coil control failed in VS VAV terminal unit '//  &
                           TRIM(Sys(SysNum)%SysName),Sys(SysNum)%IterationFailed)
        END IF
      ELSE
        MassFlow = MaxHeatMassFlow
        FanOp = 1
        CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,QTotLoad,  &
           QTotLoad,FanType,MassFlow,FanOp,QDelivered)
      END IF
    ELSE IF (HCType == HCoilType_Gas .OR. HCType == HCoilType_Electric) THEN
      IF (QTotLoad <= QHeatFanOnMin + SmallLoad) THEN
        ! vary heating coil power, leave mass flow at minimum
        MassFlow = MinMassFlow
        FanOp = 0
        CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,0.0d0,  &
           QTotLoad,FanType,MassFlow,FanOp,QDelivered)
      ELSE IF (QTotLoad > QHeatFanOnMin + SmallLoad .AND. QTotLoad < QHeatFanOnMax - SmallLoad) THEN
        FanOp = 1
        Par(1) = REAL(SysNum,r64)
        IF (FirstHVACIteration) THEN
          Par(2) = 1.d0
        ELSE
          Par(2) = 0.0d0
        END IF
        Par(3) = REAL(ZoneNodeNum,r64)
        Par(4) = REAL(HCType,r64)
        Par(5) = Sys(SysNum)%ReheatCoilMaxCapacity
        Par(6) = REAL(FanType,r64)
        Par(7) = REAL(FanOp,r64)
        Par(8) = QTotLoad
        CALL SolveRegulaFalsi(UnitFlowToler, 50, SolFlag, FracDelivered, VAVVSHCFanOnResidual, &
                              0.0d0,  1.0d0, Par)
        IF (SolFlag == -1) THEN
          IF(Sys(SysNum)%IterationLimit == 0)THEN
            CALL ShowWarningError('Heating coil control failed in VS VAV terminal unit '//TRIM(Sys(SysNum)%SysName))
            CALL ShowContinueError('  Iteration limit exceeded in calculating air flow rate')
          END IF
          CALL ShowRecurringWarningErrorAtEnd('Heating coil control iteration limit exceeded in VS VAV terminal unit '//  &
                           TRIM(Sys(SysNum)%SysName),Sys(SysNum)%IterationLimit)
        ELSE IF (SolFlag == -2) THEN
          IF(Sys(SysNum)%IterationFailed == 0)THEN
            CALL ShowWarningError('Heating coil control failed in VS VAV terminal unit '//TRIM(Sys(SysNum)%SysName))
            CALL ShowContinueError('  Bad air flow limits')
          END IF
          CALL ShowRecurringWarningErrorAtEnd('Heating coil control failed in VS VAV terminal unit '//  &
                           TRIM(Sys(SysNum)%SysName),Sys(SysNum)%IterationFailed)
        END IF
      ELSE
        MassFlow = MaxHeatMassFlow
        FanOp = 1
        CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,0.0d0,  &
           QTotLoad,FanType,MassFlow,FanOp,QDelivered)
      END IF
    ELSE
      CALL ShowFatalError('Invalid Reheat Component='//TRIM(Sys(SysNum)%ReheatComp))
    END IF

  ELSE

  MassFlow = 0.0D0
  FanOp = 0
  CALL CalcVAVVS(SysNum,FirstHVACIteration,ZoneNodeNum,HCType,0.0d0,0.0d0,  &
     FanType,MassFlow,FanOp,QDelivered)

  END IF

  RETURN
END SUBROUTINE SimVAVVS


SUBROUTINE SimConstVol(SysNum,FirstHVACIteration, ZoneNum, ZoneNodeNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   February 2000
          !       MODIFIED       FB/KHL/TH 2/2011: added maximum supply air temperature leaving reheat coil
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the simple single duct constant volume systems.

          ! METHODOLOGY EMPLOYED:
          ! There is method to this madness.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
   USE DataZoneEnergyDemands
!unused   USE DataHeatBalFanSys, ONLY: Mat
   USE WaterCoils,   ONLY:SimulateWaterCoilComponents
   USE HeatingCoils, ONLY:SimulateHeatingCoilComponents
   USE SteamCoils,   ONLY:SimulateSteamCoilComponents
   USE PlantUtilities, ONLY: SetActuatedBranchFlowRate

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   INTEGER, INTENT(IN) :: SysNum
   INTEGER, INTENT(IN) :: ZoneNum
   INTEGER, INTENT (IN):: ZoneNodeNum
   LOGICAL, INTENT (IN):: FirstHVACIteration


          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   REAL(r64) :: MassFlow          ! [kg/sec]   Total Mass Flow Rate from Hot & Cold Inlets
   REAL(r64) :: QZnReq            ! [Watts]
   REAL(r64) :: QToHeatSetPt      ! [W]  remaining load to heating setpoint
   REAL(r64) :: CpAir
   INTEGER   :: WaterControlNode  !This is the Actuated Reheat Control Node
   REAL(r64) :: MaxFlowWater      !This is the value passed to the Controller depending if FirstHVACIteration or not
   REAL(r64) :: MinFlowWater      !This is the value passed to the Controller depending if FirstHVACIteration or not
   REAL(r64) :: QActualHeating    ! the heating load seen by the reheat coil
   REAL(r64) :: TAirMax = 0.0D0   ! Maximum zone supply air temperature [C]
   REAL(r64) :: QMax    = 0.0D0   ! Maximum heat addition rate imposed by the max zone supply air temperature [W]
   REAL(r64) :: ZoneTemp   = 0.0D0   ! Zone temperature [C]
   REAL(r64) :: QMax2    = 0.0D0
   REAL(r64) :: DummyMdot  ! local fluid mass flow rate

   QToHeatSetPt=ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToHeatSP ! The calculated load from the Heat Balance
   MassFlow = SysInlet(SysNum)%AirMassFlowRateMaxAvail ! System massflow is set to the Available
   QMax2 = QToHeatSetPt
   ZoneTemp = Node(ZoneNodeNum)%Temp
   CpAir = PsyCpAirFnWTdb(Node(ZoneNodeNum)%HumRat,ZoneTemp) ! zone air specific heat
   IF (Sys(SysNum)%MaxReheatTempSetByUser) THEN
       TAirMax = Sys(Sysnum)%MaxReheatTemp
       QMax = CpAir*MassFlow*(TAirMax-ZoneTemp)
       QMax2 = MIN(QToHeatSetPt,QMax)
   END IF     ! IF (Sys(SysNum)%MaxReheatTempSetByUser) THEN

   If(((SysInlet(SysNum)%AirMassFlowRateMaxAvail == 0.0D0) .and.  &
            (SysInlet(SysNum)%AirMassFlowRateMinAvail == 0.0D0)) .or.  &
            (SysInlet(SysNum)%AirMassFlowRate == 0.0D0)) Then
   ! System is Off set massflow to 0.0
     MassFlow = 0.0D0
   End If

   ! Calculate the Damper Position when there is a Max air flow specified.
   If(Sys(Sysnum)%AirMassFlowRateMax == 0.0D0) Then
     Sys(Sysnum)%DamperPosition = 0.0D0
   Else
     Sys(Sysnum)%DamperPosition = MassFlow/Sys(Sysnum)%AirMassFlowRateMax
   End If

   ! make sure the inlet node flow rate is updated if the mass flow has been limited
   SysOutlet(SysNum)%AirMassFlowRate = MassFlow
   SysOutlet(SysNum)%AirMassFlowRateMaxAvail = SysInlet(SysNum)%AirMassFlowRateMaxAvail
   SysOutlet(SysNum)%AirMassFlowRateMinAvail = SysInlet(SysNum)%AirMassFlowRateMinAvail
   Call UpdateSys(SysNum)

   QActualHeating = QToHeatSetPt - Massflow * CpAir * (SysInlet(SysNum)%AirTemp-ZoneTemp) ! reheat needed
   !Now the massflow for reheating has been determined. If it is zero, or in SetBack, or the
   ! system scheduled OFF then not operational and shut the system down.
   If((MassFlow > SmallMassFlow) .AND. (QActualHeating > 0.0D0) .AND. &
      (TempControlType(ZoneNum) .NE. SingleCoolingSetPoint)) Then

     SELECT CASE(Sys(SysNum)%ReheatComp_Num)

       CASE(HCoilType_SimpleHeating) ! COIL:WATER:SIMPLEHEATING
         ! Determine the load required to pass to the Component controller
         QZnReq = QMax2 + Massflow * CpAir * ZoneTemp

         !Before Iterating through the Reheat Coil and Controller set the flags for the
         ! Do Loop to initialized conditions.
        ! Node(Sys(SysNum)%ReheatControlNode)%MassFlowRate = 0.0D0
         ! Initialize hot water flow rate to zero.
         DummyMdot = 0.0D0
         CALL SetActuatedBranchFlowRate(DummyMdot,Sys(SysNum)%ReheatControlNode,  &
              Sys(SysNum)%HWLoopNum,Sys(SysNum)%HWLoopSide, Sys(SysNum)%HWBranchIndex, .TRUE.)

         !On the first HVAC iteration the system values are given to the controller, but after that
         ! the demand limits are in place and there needs to be feedback to the Zone Equipment
         If(FirstHVACIteration)Then
            MaxFlowWater = Sys(SysNum)%MaxReheatWaterFlow
            MinFlowWater = Sys(SysNum)%MinReheatWaterFlow
         Else
            WaterControlNode = Sys(SysNum)%ReheatControlNode
            MaxFlowWater = Node(WaterControlNode)%MassFlowRateMaxAvail
            MinFlowWater = Node(WaterControlNode)%MassFlowRateMinAvail
         EndIf

         ! Simulate reheat coil for the Const Volume system
         ! Set Converged to True & when controller is not converged it will set to False.
         CALL ControlCompOutput(CompName=Sys(SysNum)%ReheatName,         &
                            CompType=Sys(SysNum)%ReheatComp,             &
                            CompNum=Sys(SysNum)%ReheatComp_Index,        &
                            FirstHVACIteration=FirstHVACIteration,       &
                            QZnReq=QZnReq,                               &
                            ActuatedNode=Sys(SysNum)%ReheatControlNode,  &
                            MaxFlow=MaxFlowWater,                        &
                            MinFlow=MinFlowWater,                        &
                            TempOutNode=Sys(SysNum)%ReheatAirOutletNode, &
                            ControlOffSet=Sys(SysNum)%ControllerOffset,  &
                            AirMassFlow=Massflow,                        &
                            ControlCompTypeNum=Sys(SysNum)%ControlCompTypeNum, &
                            CompErrIndex=Sys(SysNum)%CompErrIndex, &
                            LoopNum     = Sys(SysNum)%HWLoopNum,              &
                            LoopSide    = Sys(SysNum)%HWLoopSide,             &
                            BranchIndex = Sys(SysNum)%HWBranchIndex)

       CASE(HCoilType_SteamAirHeating) ! COIL:STEAM:STEAMAIRHEATING
         ! Determine the load required to pass to the Component controller
         QZnReq = QMax2 - Massflow * CpAir * (SysInlet(SysNum)%AirTemp-ZoneTemp)

         ! Simulate reheat coil for the VAV system
         CALL SimulateSteamCoilComponents(CompName=Sys(SysNum)%ReheatName,       &
                                          FirstHVACIteration=FirstHVACIteration, &
                                          QCoilReq=QZnReq,                       &
                                          CompIndex=Sys(SysNum)%ReheatComp_Index)
       CASE(HCoilType_Electric) ! COIL:ELECTRIC:HEATING
         ! Determine the load required to pass to the Component controller
         QZnReq = QMax2 - Massflow * CpAir * (SysInlet(SysNum)%AirTemp-ZoneTemp)

         ! Simulate reheat coil for the VAV system
         CALL SimulateHeatingCoilComponents(CompName=Sys(SysNum)%ReheatName,       &
                                            FirstHVACIteration=FirstHVACIteration, &
                                            QCoilReq=QZnReq,                       &
                                            CompIndex=Sys(SysNum)%ReheatComp_Index)

       CASE(HCoilType_Gas) ! COIL:GAS:HEATING
         ! Determine the load required to pass to the Component controller
         QZnReq = QMax2 - Massflow * CpAir * (SysInlet(SysNum)%AirTemp-ZoneTemp)

         ! Simulate reheat coil for the VAV system
         CALL SimulateHeatingCoilComponents(CompName=Sys(SysNum)%ReheatName,       &
                                            FirstHVACIteration=FirstHVACIteration, &
                                            QCoilReq=QZnReq,                       &
                                            CompIndex=Sys(SysNum)%ReheatComp_Index)
       CASE DEFAULT
         CALL ShowFatalError('Invalid Reheat Component='//TRIM(Sys(SysNum)%ReheatComp))
     END SELECT

   !the COIL is OFF the properties are calculated for this special case.
   Else
     SELECT CASE(Sys(SysNum)%ReheatComp_Num)

       CASE(HCoilType_SimpleHeating) ! COIL:WATER:SIMPLEHEATING
         ! Simulate reheat coil for the Const Volume system
         !Node(Sys(SysNum)%ReheatControlNode)%MassFlowRate = 0.0D0
         ! Initialize hot water flow rate to zero.
         DummyMdot = 0.0D0
         CALL SetActuatedBranchFlowRate(DummyMdot,Sys(SysNum)%ReheatControlNode,  &
              Sys(SysNum)%HWLoopNum,Sys(SysNum)%HWLoopSide, Sys(SysNum)%HWBranchIndex, .TRUE.)

         !call the reheat coil with the NO FLOW condition to make sure that the Node values
         ! are passed through to the coil outlet correctly
         CALL SimulateWaterCoilComponents(Sys(SysNum)%ReheatName,FirstHVACIteration,  &
                                            CompIndex=Sys(SysNum)%ReheatComp_Index)
       CASE(HCoilType_SteamAirHeating) ! COIL:STEAM:AIRHEATING
         ! Simulate reheat coil for the Const Volume system
         CALL SimulateSteamCoilComponents(CompName=Sys(SysNum)%ReheatName,       &
                                          FirstHVACIteration=FirstHVACIteration, &
                                          QCoilReq=0.0d0,       &
                                          CompIndex=Sys(SysNum)%ReheatComp_Index)


       CASE(HCoilType_Electric) ! COIL:ELECTRIC:HEATING
         ! Simulate reheat coil for the Const Volume system
         CALL SimulateHeatingCoilComponents(CompName=Sys(SysNum)%ReheatName,       &
                                            FirstHVACIteration=FirstHVACIteration, &
                                            QCoilReq=0.0d0,       &
                                            CompIndex=Sys(SysNum)%ReheatComp_Index)

       CASE(HCoilType_Gas) ! COIL:GAS:HEATING
         ! Simulate reheat coil for the Const Volume system
         CALL SimulateHeatingCoilComponents(CompName=Sys(SysNum)%ReheatName,       &
                                            FirstHVACIteration=FirstHVACIteration, &
                                            QCoilReq=0.0d0,       &
                                            CompIndex=Sys(SysNum)%ReheatComp_Index)
       CASE DEFAULT
         CALL ShowFatalError('Invalid Reheat Component='//TRIM(Sys(SysNum)%ReheatComp))
     END SELECT

   End IF

!Debugging output for model
!If((HourofDay .ge. 8) .and. (hourofDay .lt. 15)) Then
!      Write(OutputFileDebug,*)  'Day of Sim     Hour of Day    Time'
!      Write(OutputFileDebug,*)  DayofSim, HourOfDay, TimeStep*TimeStepZone
!      Write(OutputFileDebug,10)
!
!      Write(OutputFileDebug,20)ZoneNum, SysInlet(SysNum)%AirMassFlowRate, &
!                             SysInlet(SysNum)%AirMassFlowRate, &
!                              Temperature, Mat(ZoneNum), Node(ZoneNodeNum)%Temp, QTotLoad, &
!                             Enthalpy
!End If
!10 Format('ZoneNum    SysHot    SysCold   Temp  &
!      &    MAT        NodeZoneTemp    QTotLoad  Enthalpy')
!
!20 Format(1x,I3,3x, 5(2x, F9.4), 2(2x, F9.2))

 RETURN

END SUBROUTINE SimConstVol

SUBROUTINE CalcVAVVS(SysNum,FirstHVACIteration,ZoneNode,HCoilType,HWFlow,HCoilReq,FanType,AirFlow,FanOn,LoadMet)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   July 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate the components making up the VAV terminal unit with variable speed fan.

          ! METHODOLOGY EMPLOYED:
          ! Simulates the unit components sequentially in the air flow direction.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use WaterCoils,   Only: SimulateWaterCoilComponents
  Use HeatingCoils, Only: SimulateHeatingCoilComponents
  Use Fans,         Only: SimulateFanComponents
  Use DataHVACGlobals, Only: TurnFansOff
  Use SteamCoils, Only: SimulateSteamCoilComponents
  USE PlantUtilities, ONLY: SetComponentFlowRate

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SysNum         ! Unit index
  LOGICAL, INTENT (IN)  :: FirstHVACIteration ! flag for 1st HVAV iteration in the time step
  INTEGER, INTENT (IN)  :: ZoneNode           ! zone node number
  INTEGER, INTENT (IN)  :: HCoilType          ! type of hot water coil !unused1208
  REAL(r64),    INTENT (IN)  :: HWFlow             ! hot water flow (kg/s)
  REAL(r64),    INTENT (IN)  :: HCoilReq           ! gas or elec coil demand requested
  INTEGER, INTENT (IN)  :: FanType            ! type of fan
  REAL(r64),    INTENT (IN)  :: AirFlow            ! air flow rate (kg/s)
  INTEGER, INTENT (IN)  :: FanOn              ! 1 means fan is on
  REAL(r64),    INTENT (OUT) :: LoadMet            ! load met by unit (watts)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: FanInNode         ! unit air inlet node (fan inlet)
  INTEGER :: FanOutNode        ! fan outlet node (heating coil inlet node)
  INTEGER :: HCOutNode         ! unit air outlet node (heating coil air outlet node)
  INTEGER :: HotControlNode    ! the hot water inlet node
  REAL(r64)    :: AirMassFlow       ! total air mass flow rate [kg/s]
  REAL(r64)    :: CpAirZn           ! zone air specific heat [J/kg-C]
  Logical :: TurnFansOffSav    ! save the fan off flag
  REAL(r64) :: mdot

  TurnFansOffSav = TurnFansOff
  FanInNode = Sys(SysNum)%InletNodeNum
  FanOutNode = Sys(SysNum)%OutletNodeNum
  HCOutNode = Sys(SysNum)%ReheatAirOutletNode
  HotControlNode = Sys(SysNum)%ReheatControlNode
  AirMassFlow = AirFlow
  Node(FanInNode)%MassFlowRate = AirMassFlow
  CpAirZn = PsyCpAirFnWTdb(Node(ZoneNode)%HumRat,Node(ZoneNode)%Temp)
  IF (FanType == FanType_VS .AND. FanOn == 1) THEN
    CALL SimulateFanComponents(Sys(SysNum)%FanName,FirstHVACIteration,Sys(SysNum)%Fan_Index)
  ELSE ! pass through conditions
    TurnFansOff = .TRUE.
    CALL SimulateFanComponents(Sys(SysNum)%FanName,FirstHVACIteration,Sys(SysNum)%Fan_Index)
    TurnFansOff = TurnFansOffSav
    Node(FanOutNode)%MassFlowRate = Node(FanInNode)%MassFlowRate
    Node(FanOutNode)%MassFlowRateMaxAvail = Node(FanInNode)%MassFlowRateMaxAvail
    Node(FanOutNode)%MassFlowRateMinAvail = Node(FanInNode)%MassFlowRateMinAvail
  END IF
  SELECT CASE(Sys(SysNum)%ReheatComp_Num)
    CASE(HCoilType_SimpleHeating) ! COIL:WATER:SIMPLEHEATING
      mdot = HWFlow
      IF (Sys(SysNum)%HWLoopNum > 0 ) THEN
        CALL SetComponentFlowRate(mdot, &
                                 Sys(SysNum)%ReheatControlNode, &
                                 Sys(SysNum)%ReheatCoilOutletNode, &
                                 Sys(SysNum)%HWLoopNum, &
                                 Sys(SysNum)%HWLoopSide, &
                                 Sys(SysNum)%HWBranchIndex, &
                                 Sys(SysNum)%HWCompIndex)
      ENDIF

      CALL SimulateWaterCoilComponents(Sys(SysNum)%ReheatName,FirstHVACIteration,Sys(SysNum)%ReheatComp_Index)
    CASE(HCoilType_SteamAirHeating) ! HW Flow is steam mass flow here
      mdot = HWFlow
      IF (Sys(SysNum)%HWLoopNum > 0 ) THEN
        CALL SetComponentFlowRate(mdot, &
                                 Sys(SysNum)%ReheatControlNode, &
                                 Sys(SysNum)%ReheatCoilOutletNode, &
                                 Sys(SysNum)%HWLoopNum, &
                                 Sys(SysNum)%HWLoopSide, &
                                 Sys(SysNum)%HWBranchIndex, &
                                 Sys(SysNum)%HWCompIndex)
      ENDIF
      CALL SimulateSteamCoilComponents(Sys(SysNum)%ReheatName,FirstHVACIteration,HCoilReq,Sys(SysNum)%ReheatComp_Index)
    CASE(HCoilType_Electric) ! COIL:ELECTRIC:HEATING
      CALL SimulateHeatingCoilComponents(Sys(SysNum)%ReheatName,FirstHVACIteration,HCoilReq,Sys(SysNum)%ReheatComp_Index)
    CASE(HCoilType_Gas) ! COIL:GAS:HEATING
      CALL SimulateHeatingCoilComponents(Sys(SysNum)%ReheatName,FirstHVACIteration,HCoilReq,Sys(SysNum)%ReheatComp_Index)
    CASE DEFAULT
      CALL ShowFatalError('Invalid Reheat Component='//TRIM(Sys(SysNum)%ReheatComp))
  END SELECT

  LoadMet = AirMassFlow*CpAirZn*(Node(HCOutNode)%Temp-Node(ZoneNode)%Temp)

  RETURN

END SUBROUTINE CalcVAVVS

FUNCTION VAVVSCoolingResidual(SupplyAirMassFlow, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   July 2004
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (Requested Zone Load - Unit Output) / Requested Zone Load
          ! Unit Output depends on the supply air flow rate which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls CalcVAVVS, and calculates
          ! the residual as defined above.

          ! REFERENCES:

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: SupplyAirMassFlow ! supply air mass flow rate [kg/s]
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! Par(1) = REAL(SysNum)
                                                    ! Par(2) = FirstHVACIteration (1. or 0.)
                                                    ! Par(3) = REAL(ZoneNodeNum)
                                                    ! Par(4) = REAL(HCType)
                                                    ! Par(5) = minimum HW flow rate [kg/s]
                                                    ! Par(6) = REAL(FanType)
                                                    ! Par(7) = REAL(FanOp)
                                                    ! Par(8) = cooling demand [W] (negative)
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: UnitIndex
  LOGICAL :: FirstHVACSoln
  INTEGER :: ZoneNodeIndex
  REAL(r64)    :: MinHWFlow         ! min hot water flow rate
  INTEGER :: HCType            ! heating coil type (integer)
  INTEGER :: FanType           ! fan type (as an integer)
  INTEGER :: FanOp             ! fan operation; 0=off, 1=on.
  REAL(r64)    :: UnitOutput        ! cooling output [W] (cooling is negative)

  UnitIndex = INT(Par(1))
  IF (Par(2) > 0.0D0) THEN
    FirstHVACSoln = .TRUE.
  ELSE
    FirstHVACSoln = .FALSE.
  END IF
  ZoneNodeIndex = INT(Par(3))
  HCType = INT(Par(4))
  MinHWFlow = Par(5)
  FanType = INT(Par(6))
  FanOp = INT(Par(7))
  CALL CalcVAVVS(UnitIndex,FirstHVACSoln,ZoneNodeIndex,HCType,MinHWFlow,0.0d0,  &
     FanType,SupplyAirMassFlow,FanOp,UnitOutput)

  Residuum = (Par(8) - UnitOutput) / Par(8)

  RETURN
END FUNCTION VAVVSCoolingResidual

FUNCTION VAVVSHWNoFanResidual(HWMassFlow, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   July 2004
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (Requested Zone Load - Unit Output) / Requested Zone Load
          ! Unit Output depends on the hot water flow rate which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls CalcVAVVS, and calculates
          ! the residual as defined above.

          ! REFERENCES:

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: HWMassFlow ! hot water mass flow rate [kg/s]
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! Par(1) = REAL(SysNum)
                                                    ! Par(2) = FirstHVACIteration (1. or 0.)
                                                    ! Par(3) = REAL(ZoneNodeNum)
                                                    ! Par(4) = REAL(HCType)
                                                    ! Par(5) = air mass flow flow rate [kg/s]
                                                    ! Par(6) = REAL(FanType)
                                                    ! Par(7) = REAL(FanOp)
                                                    ! Par(8) = heating demand [W]
                                                    ! Par(9) = min steam flow rate [m3/s] - steam only
                                                    ! Par(10 = max steam flow rate [m3/s] - steam only
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: UnitIndex
  LOGICAL :: FirstHVACSoln
  INTEGER :: ZoneNodeIndex
  REAL(r64)    :: AirMassFlow       ! supply air mass flow rate [kg/s]
  INTEGER :: HCType            ! heating coil type (integer)
  INTEGER :: FanType           ! fan type (as an integer)
  INTEGER :: FanOp             ! fan operation; 0=off, 1=on.
  REAL(r64)    :: UnitOutput        ! heating output [W]
  REAL(r64) :: QSteamLoad      ! proportional load to calculate steam flow [W]
  REAL(r64) :: MinSteamFlow
  REAL(r64) :: MaxSteamFlow
  REAL(r64) :: MaxSteamCoilCapacity

  UnitIndex = INT(Par(1))
  IF (Par(2) > 0.0D0) THEN
    FirstHVACSoln = .TRUE.
  ELSE
    FirstHVACSoln = .FALSE.
  END IF
  ZoneNodeIndex = INT(Par(3))
  HCType = INT(Par(4))
  AirMassFlow = Par(5)
  FanType = INT(Par(6))
  FanOp = INT(Par(7))
  QSteamLoad = 0.0D0
! vary the load to be met by the steam coil to converge on a steam flow rate to meet the load
  IF(HCType == HCoilType_SteamAirHeating)THEN
!   backwards way of varying steam flow rate. Steam coil calculates a flow rate to meet a load.
    MinSteamFlow = Par(9)
    MaxSteamFlow = Par(10)
    MaxSteamCoilCapacity = Par(11)
    IF( (MaxSteamFlow - MinSteamFlow) == 0.0D0)THEN
      QSteamLoad = Par(8) ! Use QTotLoad, bad starting value error for RegulaFalsi will occur
    ELSE
      QSteamLoad = MaxSteamCoilCapacity * HWMassFlow/(MaxSteamFlow - MinSteamFlow)
    END IF
  END IF
  CALL CalcVAVVS(UnitIndex,FirstHVACSoln,ZoneNodeIndex,HCType,HWMassFlow,QSteamLoad,  &
     FanType,AirMassFlow,FanOp,UnitOutput)

  Residuum = (Par(8) - UnitOutput) / Par(8)

  RETURN
END FUNCTION VAVVSHWNoFanResidual

FUNCTION VAVVSHWFanOnResidual(SupplyAirMassFlow, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   July 2004
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (Requested Zone Load - Unit Output) / Requested Zone Load
          ! Unit Output depends on the supply air flow rate which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls CalcVAVVS, and calculates
          ! the residual as defined above.

          ! REFERENCES:

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: SupplyAirMassFlow ! supply air mass flow rate [kg/s]
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! Par(1) = REAL(SysNum)
                                                    ! Par(2) = FirstHVACIteration (1. or 0.)
                                                    ! Par(3) = REAL(ZoneNodeNum)
                                                    ! Par(4) = REAL(HCType)
                                                    ! Par(5) = hot water mass flow rate [kg/s]
                                                    ! Par(6) = REAL(FanType)
                                                    ! Par(7) = REAL(FanOp)
                                                    ! Par(8) = heating demand [W]
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: UnitIndex
  LOGICAL :: FirstHVACSoln
  INTEGER :: ZoneNodeIndex
  REAL(r64) :: HWMassFlow      ! hot water mass flow rate [kg/s]
  INTEGER :: HCType            ! heating coil type (integer)
  INTEGER :: FanType           ! fan type (as an integer)
  INTEGER :: FanOp             ! fan operation; 0=off, 1=on.
  REAL(r64)    :: UnitOutput        ! heating output [W]

  UnitIndex = INT(Par(1))
  IF (Par(2) > 0.0D0) THEN
    FirstHVACSoln = .TRUE.
  ELSE
    FirstHVACSoln = .FALSE.
  END IF
  ZoneNodeIndex = INT(Par(3))
  HCType = INT(Par(4))
  HWMassFlow = Par(5)
  FanType = INT(Par(6))
  FanOp = INT(Par(7))
  CALL CalcVAVVS(UnitIndex,FirstHVACSoln,ZoneNodeIndex,HCType,HWMassFlow,Par(8),  &
     FanType,SupplyAirMassFlow,FanOp,UnitOutput)

  Residuum = (Par(8) - UnitOutput) / Par(8)

  RETURN
END FUNCTION VAVVSHWFanOnResidual

FUNCTION VAVVSHCFanOnResidual(HeatingFrac, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   July 2004
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (Requested Zone Load - Unit Output) / Requested Zone Load
          ! Unit Output depends on the heating coil output which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls CalcVAVVS, and calculates
          ! the residual as defined above.

          ! REFERENCES:

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: HeatingFrac ! fraction of maximum heating output
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! Par(1) = REAL(SysNum)
                                                    ! Par(2) = FirstHVACIteration (1. or 0.)
                                                    ! Par(3) = REAL(ZoneNodeNum)
                                                    ! Par(4) = REAL(HCType)
                                                    ! Par(5) = max heating coil output [W]
                                                    ! Par(6) = REAL(FanType)
                                                    ! Par(7) = REAL(FanOp)
                                                    ! Par(8) = heating demand [W]
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: UnitIndex
  LOGICAL :: FirstHVACSoln
  INTEGER :: ZoneNodeIndex
  REAL(r64)    :: MaxHeatOut        ! maximum heating output [W]
  INTEGER :: HCType            ! heating coil type (integer)
  INTEGER :: FanType           ! fan type (as an integer)
  INTEGER :: FanOp             ! fan operation; 0=off, 1=on.
  REAL(r64)    :: UnitOutput        ! heating output [W]
  REAL(r64)    :: AirMassFlowRate   ! [kg/s]
  REAL(r64)    :: HeatOut           ! heating coil output [W]

  UnitIndex = INT(Par(1))
  IF (Par(2) > 0.0D0) THEN
    FirstHVACSoln = .TRUE.
  ELSE
    FirstHVACSoln = .FALSE.
  END IF
  ZoneNodeIndex = INT(Par(3))
  HCType = INT(Par(4))
  MaxHeatOut= Par(5)
  FanType = INT(Par(6))
  FanOp = INT(Par(7))
  HeatOut = HeatingFrac * MaxHeatOut
  AirMassFlowRate = MAX(HeatingFrac * Sys(UnitIndex)%HeatAirMassFlowRateMax, &
                        SysInlet(UnitIndex)%AirMassFlowRateMaxAvail * Sys(UnitIndex)%ZoneMinAirFrac)

  CALL CalcVAVVS(UnitIndex,FirstHVACSoln,ZoneNodeIndex,HCType,0.0d0,  &
     HeatOut,FanType,AirMassFlowRate,FanOp,UnitOutput)

  Residuum = (Par(8) - UnitOutput) / Par(8)

  RETURN
END FUNCTION VAVVSHCFanOnResidual
! End Algorithm Section of the Module
! *****************************************************************************

! Beginning of Update subroutines for the Sys Module
! *****************************************************************************

SUBROUTINE UpdateSys(SysNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   january 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the Syss.

          ! METHODOLOGY EMPLOYED:
          ! There is method to this madness.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataContaminantBalance, ONLY: Contaminant

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   Integer, Intent(IN) :: SysNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  Integer             :: OutletNode
  Integer             :: InletNode

  OutletNode = Sys(SysNum)%OutletNodeNum
  InletNode = Sys(SysNum)%InletNodeNum

  If(Sys(SysNum)%SysType_Num == SingleDuctVAVReheat .or. &
     Sys(SysNum)%SysType_Num == SingleDuctCBVAVReheat .or. &
     Sys(SysNum)%SysType_Num == SingleDuctCBVAVNoReheat .or. &
     Sys(SysNum)%SysType_Num == SingleDuctVAVNoReheat) Then
      ! Set the outlet air nodes of the Sys
      Node(OutletNode)%MassFlowRate  = SysOutlet(SysNum)%AirMassFlowRate
      Node(OutletNode)%Temp          = SysOutlet(SysNum)%AirTemp
      Node(OutletNode)%HumRat        = SysOutlet(SysNum)%AirHumRat
      Node(OutletNode)%Enthalpy      = SysOutlet(SysNum)%AirEnthalpy
      ! Set the outlet nodes for properties that just pass through & not used
      Node(OutletNode)%Quality         = Node(InletNode)%Quality
      Node(OutletNode)%Press           = Node(InletNode)%Press

  END IF

  !After all of the Oulets are updated the mass flow information needs to be
  ! passed back to the system inlet.
  Node(InletNode)%MassFlowRate = SysOutlet(SysNum)%AirMassFlowRate
  Node(OutletNode)%MassFlowRateMaxAvail  = MIN(SysOutlet(SysNum)%AirMassFlowRateMaxAvail, &
                                               Node(OutletNode)%MassFlowRateMax)
  Node(OutletNode)%MassFlowRateMinAvail  = SysOutlet(SysNum)%AirMassFlowRateMinAvail

  IF (Contaminant%CO2Simulation) Then
    Node(OutletNode)%CO2 = Node(InletNode)%CO2
  End If

  IF (Contaminant%GenericContamSimulation) Then
    Node(OutletNode)%GenContam = Node(InletNode)%GenContam
  End If

  RETURN
END Subroutine UpdateSys

!        End of Update subroutines for the Sys Module
! *****************************************************************************


! Beginning of Reporting subroutines for the Sys Module
! *****************************************************************************

SUBROUTINE ReportSys(SysNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Unknown
          !       DATE WRITTEN   Unknown
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the Sys report variables.

          ! METHODOLOGY EMPLOYED:
          ! There is method to this madness.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   Integer, Intent(IN) :: SysNum !unused1208

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

 ! Still needs to report the Sys power from this component


  RETURN
END Subroutine ReportSys

SUBROUTINE GetHVACSingleDuctSysIndex(SDSName,SDSIndex,ErrorsFound,ThisObjectType,DamperInletNode,DamperOutletNode)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   February 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets an index for a given single duct system -- issues error message if that system
          ! is not a legal system.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: SDSName
  INTEGER, INTENT(INOUT)       :: SDSIndex
  LOGICAL, INTENT(INOUT)       :: ErrorsFound
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ThisObjectType
  INTEGER, INTENT(OUT), OPTIONAL :: DamperInletNode       ! Damper inlet node number
  INTEGER, INTENT(OUT), OPTIONAL :: DamperOutletNode      ! Damper outlet node number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (GetInputFlag) THEN  !First time subroutine has been entered
    CALL GetSysInput
    GetInputFlag=.false.
  End If

  SDSIndex = FindItemInList(SDSName,Sys%SysName,NumSys)
  IF (SDSIndex == 0) THEN
    IF (PRESENT(ThisObjectType)) THEN
      CALL ShowSevereError(TRIM(ThisObjectType)//', GetHVACSingleDuctSysIndex: Single duct system not found='//TRIM(SDSName))
    ELSE
      CALL ShowSevereError('GetHVACSingleDuctSysIndex: Single duct system not found='//TRIM(SDSName))
    ENDIF
    ErrorsFound=.TRUE.
  ELSE
    IF ((Sys(SDSIndex)%SysType_Num .NE. SingleDuctConstVolReheat)  .AND. &
       (Sys(SDSIndex)%SysType_Num .NE. SingleDuctVAVReheat) ) THEN
      CALL ShowSevereError(TRIM(ThisObjectType)//', GetHVACSingleDuctSysIndex: Could not find allowed types='//TRIM(SDSName))
      CALL ShowContinueError('The allowed types are: AirTerminal:SingleDuct:ConstantVolume:Reheat and ' &
           //'AirTerminal:SingleDuct:VAV:Reheat')
      ErrorsFound=.TRUE.
    END IF
    If (Sys(SDSIndex)%SysType_Num .EQ. SingleDuctVAVReheat) Then
      If (PRESENT(DamperInletNode)) DamperInletNode = Sys(SDSIndex)%InletNodeNum
      If (PRESENT(DamperOutletNode)) DamperOutletNode = Sys(SDSIndex)%OutletNodeNum
    End If
  ENDIF

  RETURN

END SUBROUTINE GetHVACSingleDuctSysIndex

SUBROUTINE SimATMixer(SysName,FirstHVACIteration,SysIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Simulate an Air Terminal Mixer component

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
      USE InputProcessor, ONLY: FindItemInList

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS
    CHARACTER(len=*), INTENT(IN) :: SysName
    LOGICAL, INTENT(IN)      :: FirstHVACIteration
    INTEGER, INTENT(INOUT)   :: SysIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER      :: SysNum = 0

IF (GetATMixerFlag) THEN
  CALL GetATMixers
  GetATMixerFlag = .FALSE.
ENDIF

IF (SysIndex == 0) THEN
  SysNum=FindItemInList(TRIM(SysName),SysATMixer%Name,NumATMixers)
  SysIndex=SysNum
  IF (SysNum == 0) THEN
      CALL ShowFatalError('Object '//TRIM(SysName)//' not found')
  ENDIF
ELSE
  SysNum=SysIndex
ENDIF

Call InitATMixer(SysNum, FirstHVACIteration) ! Not being used, is placeholder

CALL CalcATMixer(SysNum)

CALL UpdateATMixer(SysNum)

RETURN

END SUBROUTINE SimATMixer

SUBROUTINE GetATMixers

          ! SUBROUTINE INFORMATION:
          !       AUTHOR
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Get input for inlet side air temrinal mixers and store it in the inlet side air terminal mixer array

          ! METHODOLOGY EMPLOYED:
          ! Use the Get routines from the InputProcessor module.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor,    ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, FindItemInList
    USE NodeInputManager,  ONLY: GetOnlySingleNode
    USE DataInterfaces,    ONLY: SetupOutputVariable
    USE DataZoneEquipment, ONLY: ZoneEquipConfig, ZoneEquipList, EquipmentData, SubEquipmentData
    USE DataLoopNode
    USE DataIPShortCuts
    USE BranchNodeConnections, ONLY: TestCompSet, SetUpCompSets
    USE DataGlobals,      ONLY: NumOfZones
 !   USE DataDefineEquip,   ONLY: AirDistUnit, NumAirDistUnits
!    USE PackagedTerminalHeatPump, ONLY: GetPTUnitZoneInletAirNode, GetPTUnitIndex, GetPTUnitInletAirNode
!    USE FanCoilUnits, ONLY: GetFanCoilIndex, GetFanCoilZoneInletAirNode, GetFanCoilInletAirNode

    IMPLICIT NONE

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: NumNums                ! Number of REAL(r64) numbers returned by GetObjectItem
INTEGER :: NumAlphas              ! Number of alphanumerics returned by GetObjectItem
INTEGER :: InletATMixerNum        ! Index of inlet side mixer air terminal unit
INTEGER :: SupplyATMixerNum       ! Index of supply side mixer air terminal unit
INTEGER :: NumInletATMixers       ! Number of inlet side mixer air terminal units
INTEGER :: NumSupplyATMixers      ! Number of supply side mixer air terminal units
INTEGER :: IOSTAT
CHARACTER(len=*), PARAMETER    :: RoutineName='GetATMixers: ' ! include trailing blank space
LOGICAL :: ErrorsFound=.false.    ! Error flag
LOGICAL :: IsNotOK                ! Flag to verify name
LOGICAL :: IsBlank                ! Flag for blank name
INTEGER :: NodeNum                ! Index to node number
INTEGER :: CtrlZone               ! Index to control zone
LOGICAL :: ZoneNodeNotFound       ! Flag for error checking
LOGICAL :: ZoneEquipNodeNotFound  ! Flag for error checking
INTEGER :: ADUNum                 ! Air distribution unit index
INTEGER :: SupAirIn               ! Supply air inlet node index
LOGICAL :: ErrFlag                ! error flag from component validation


NumInletATMixers = GetNumObjectsFound('AirTerminal:SingleDuct:InletSideMixer')
NumSupplyATMixers = GetNumObjectsFound('AirTerminal:SingleDuct:SupplySideMixer')

NumATMixers = NumInletATMixers + NumSupplyATMixers
ALLOCATE(SysATMixer(NumATMixers))

cCurrentModuleObject='AirTerminal:SingleDuct:InletSideMixer'

  DO InletATMixerNum=1,NumInletATMixers
    CALL GetObjectItem(TRIM(cCurrentModuleObject),InletATMixerNum,cAlphaArgs,NumAlphas,&
                       rNumericArgs,NumNums,IOSTAT,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),SysATMixer%Name,InletATMixerNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxxxxx'
    ENDIF
    SysATMixer(InletATMixerNum)%Name = TRIM(cAlphaArgs(1))
    SysATMixer(InletATMixerNum)%MixerType = 1 ! inlet side mixer
    IF (TRIM(cAlphaArgs(2)) == 'ZONEHVAC:WATERTOAIRHEATPUMP') THEN
      SysATMixer(InletATMixerNum)%ZoneHVACUnitType = 1
    ELSE IF (TRIM(cAlphaArgs(2)) == 'ZONEHVAC:FOURPIPEFANCOIL') THEN
      SysATMixer(InletATMixerNum)%ZoneHVACUnitType = 2
    END IF

    SysATMixer(InletATMixerNum)%ZoneHVACUnitName = TRIM(cAlphaArgs(3))

    CALL ValidateComponent(cAlphaArgs(2),SysATMixer(InletATMixerNum)%ZoneHVACUnitName, &
                                                                  ErrFlag,TRIM(cCurrentModuleObject))

    SysATMixer(InletATMixerNum)%MixedAirOutNode  = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                                 NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent,cAlphaFieldNames(4))

    SysATMixer(InletATMixerNum)%PriInNode   = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                                 NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent,cAlphaFieldNames(5))
    SysATMixer(InletATMixerNum)%SecInNode  = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                                 NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent,cAlphaFieldNames(6))
   ! Check for dupes in the three nodes.
    IF (SysATMixer(InletATMixerNum)%SecInNode == SysATMixer(InletATMixerNum)%PriInNode) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(SysATMixer(InletATMixerNum)%Name)//  &
                           ' '//TRIM(cAlphaArgs(5))//' = '//TRIM(NodeID(SysATMixer(InletATMixerNum)%PriInNode))// &
                           ' duplicates the '//TRIM(cAlphaArgs(4))//'.')
      ErrorsFound=.true.
    ELSEIF (SysATMixer(InletATMixerNum)%SecInNode == SysATMixer(InletATMixerNum)%MixedAirOutNode) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(SysATMixer(InletATMixerNum)%Name)//  &
                           ' '//TRIM(cAlphaArgs(6))//' = '//TRIM(NodeID(SysATMixer(InletATMixerNum)%MixedAirOutNode))// &
                           ' duplicates the '//TRIM(cAlphaArgs(4))//'.')
      ErrorsFound=.true.
    ENDIF

    IF (SysATMixer(InletATMixerNum)%PriInNode == SysATMixer(InletATMixerNum)%MixedAirOutNode) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(SysATMixer(InletATMixerNum)%Name)//  &
                           ' '//TRIM(cAlphaArgs(6))//' = '//TRIM(NodeID(SysATMixer(InletATMixerNum)%MixedAirOutNode))// &
                           ' duplicates the '//TRIM(cAlphaArgs(5))//'.')
      ErrorsFound=.true.
    ENDIF


   ! Air Terminal inlet node must be the same as a zone exhaust node
   ZoneNodeNotFound = .TRUE.
   ControlledZoneLoop: DO CtrlZone = 1,NumOfZones
     IF (.NOT. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
     DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumExhaustNodes
       IF (SysATMixer(InletATMixerNum)%SecInNode .EQ. ZoneEquipConfig(CtrlZone)%ExhaustNode(NodeNum)) THEN
         ZoneNodeNotFound = .FALSE.
         DO SupAirIn = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
           IF (SysATMixer(InletATMixerNum)%SecInNode .EQ. ZoneEquipConfig(CtrlZone)%ExhaustNode(SupAirIn)) THEN
             ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%InNode = SysATMixer(InletATMixerNum)%PriInNode
             ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode = SysATMixer(InletATMixerNum)%MixedAirOutNode
             ZoneEquipConfig(CtrlZone)%AirDistUnitHeat(SupAirIn)%InNode = SysATMixer(InletATMixerNum)%PriInNode
             ZoneEquipConfig(CtrlZone)%AirDistUnitHeat(SupAirIn)%OutNode = SysATMixer(InletATMixerNum)%MixedAirOutNode
           END IF
         END DO
         EXIT ControlledZoneLoop
       END IF
      END DO
    END DO ControlledZoneLoop
    IF(ZoneNodeNotFound)THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(SysATMixer(InletATMixerNum)%Name)//'".'// &
                         ' Inlet Side Air Terminal Mixer air inlet node name must be the same as a zone exhaust node name.')
      CALL ShowContinueError('..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.')
      CALL ShowContinueError('..Inlet Side Air Terminal Mixer inlet node name = '//  &
         TRIM(NodeID(SysATMixer(InletATMixerNum)%SecInNode)))
      ErrorsFound=.TRUE.
    END IF

    CALL TestCompSet(TRIM(cCurrentModuleObject),SysATMixer(InletATMixerNum)%Name,cAlphaArgs(5), &
                     cAlphaArgs(4),'Air Nodes')

  END DO

cCurrentModuleObject='AirTerminal:SingleDuct:SupplySideMixer'

  DO SupplyATMixerNum=NumInletATMixers+1,NumInletATMixers+NumSupplyATMixers
    CALL GetObjectItem(TRIM(cCurrentModuleObject),SupplyATMixerNum,cAlphaArgs,NumAlphas,&
                       rNumericArgs,NumNums,IOSTAT,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),SysATMixer%Name,SupplyATMixerNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxxxxx'
    ENDIF
    SysATMixer(SupplyATMixerNum)%Name = TRIM(cAlphaArgs(1))
    SysATMixer(SupplyATMixerNum)%MixerType = 2 ! supply side mixer
    IF (TRIM(cAlphaArgs(2)) == 'ZONEHVAC:WATERTOAIRHEATPUMP') THEN
      SysATMixer(SupplyATMixerNum)%ZoneHVACUnitType = 1
    ELSE IF (TRIM(cAlphaArgs(2)) == 'ZONEHVAC:FOURPIPEFANCOIL') THEN
      SysATMixer(SupplyATMixerNum)%ZoneHVACUnitType = 2
    END IF

    SysATMixer(SupplyATMixerNum)%ZoneHVACUnitName = TRIM(cAlphaArgs(3))

    CALL ValidateComponent(cAlphaArgs(2),SysATMixer(SupplyATMixerNum)%ZoneHVACUnitName, &
                                                                  ErrFlag,TRIM(cCurrentModuleObject))

    SysATMixer(SupplyATMixerNum)%MixedAirOutNode  = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                                 NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent,cAlphaFieldNames(4))

   SysATMixer(SupplyATMixerNum)%PriInNode   = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                                 NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent,cAlphaFieldNames(5))
    SysATMixer(SupplyATMixerNum)%SecInNode  = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                                 NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent,cAlphaFieldNames(6))
   ! Check for dupes in the three nodes.
    IF (SysATMixer(SupplyATMixerNum)%SecInNode == SysATMixer(SupplyATMixerNum)%PriInNode) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(SysATMixer(SupplyATMixerNum)%Name)//  &
                           ' '//TRIM(cAlphaArgs(5))//' = '//TRIM(NodeID(SysATMixer(SupplyATMixerNum)%PriInNode))// &
                           ' duplicates the '//TRIM(cAlphaArgs(4))//'.')
      ErrorsFound=.true.
    ELSEIF (SysATMixer(SupplyATMixerNum)%SecInNode == SysATMixer(SupplyATMixerNum)%MixedAirOutNode) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(SysATMixer(SupplyATMixerNum)%Name)//  &
                           ' '//TRIM(cAlphaArgs(6))//' = '//TRIM(NodeID(SysATMixer(SupplyATMixerNum)%MixedAirOutNode))// &
                           ' duplicates the '//TRIM(cAlphaArgs(4))//'.')
      ErrorsFound=.true.
    ENDIF

    IF (SysATMixer(SupplyATMixerNum)%PriInNode == SysATMixer(SupplyATMixerNum)%MixedAirOutNode) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = '//TRIM(SysATMixer(SupplyATMixerNum)%Name)//  &
                           ' '//TRIM(cAlphaArgs(6))//' = '//TRIM(NodeID(SysATMixer(SupplyATMixerNum)%MixedAirOutNode))// &
                           ' duplicates the '//TRIM(cAlphaArgs(5))//'.')
      ErrorsFound=.true.
    ENDIF

    CALL TestCompSet(TRIM(cCurrentModuleObject),SysATMixer(SupplyATMixerNum)%Name,cAlphaArgs(5), &
                     cAlphaArgs(4),'Air Nodes')

    ! Air Terminal outlet node must be the same as a zone inlet node
    ZoneNodeNotFound = .TRUE.
    ControlZoneLoop: DO CtrlZone = 1,NumOfZones
      IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
      DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
        IF (SysATMixer(SupplyATMixerNum)%MixedAirOutNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(NodeNum)) THEN
          ZoneNodeNotFound = .FALSE.
          DO SupAirIn = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
            IF (SysATMixer(SupplyATMixerNum)%MixedAirOutNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(SupAirIn)) THEN
              ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%InNode = SysATMixer(SupplyATMixerNum)%PriInNode
              ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode = SysATMixer(SupplyATMixerNum)%MixedAirOutNode
              ZoneEquipConfig(CtrlZone)%AirDistUnitHeat(SupAirIn)%InNode = SysATMixer(SupplyATMixerNum)%PriInNode
              ZoneEquipConfig(CtrlZone)%AirDistUnitHeat(SupAirIn)%OutNode = SysATMixer(SupplyATMixerNum)%MixedAirOutNode
            END IF
          END DO
          EXIT ControlZoneLoop
        END IF
      END DO
    END DO ControlZoneLoop
    IF(ZoneNodeNotFound)THEN
       CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(SysATMixer(SupplyATMixerNum)%Name)//'".'// &
                         ' Supply Side Air Terminal Mixer air outlet node name must be the same as a zone inlet node name.')
       CALL ShowContinueError('..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.')
       CALL ShowContinueError('..Inlet Side Air Terminal Mixer inlet node name = '//  &
          TRIM(NodeID(SysATMixer(SupplyATMixerNum)%SecInNode)))
       ErrorsFound=.TRUE.
    END IF
  END DO

IF (ErrorsFound) THEN
  CALL ShowFatalError(RoutineName//'Errors found in input.  Program terminates.')
ENDIF

RETURN
END SUBROUTINE GetATMixers

SUBROUTINE InitATMixer(ATMixerNum,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Initialize the AirTerminalMixers data structure with node data

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
    USE DataLoopNode

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS
    INTEGER, INTENT(IN) :: ATMixerNum
    LOGICAL, INTENT(IN) :: FirstHVACIteration

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: InletNode
INTEGER :: PriInNode
INTEGER :: MixedAirOutNode

InletNode = SysATMixer(ATMixerNum)%SecInNode
PriInNode = SysATMixer(ATMixerNum)%PriInNode
MixedAirOutNode = SysATMixer(ATMixerNum)%MixedAirOutNode

IF (FirstHVACIteration) THEN
!  SysATMixer(ATMixerNum)%ZoneAirMassFlowRate = SysATMixer(ATMixerNum)%MaxAirMassFlowRate
END IF

IF (BeginDayFlag) THEN
END IF

IF (FirstHVACIteration) THEN
END IF

RETURN
END SUBROUTINE InitATMixer

SUBROUTINE CalcATMixer(SysNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Calculate the mixed air flow and conditions in the air terminal mixer

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
USE Psychrometrics, ONLY:PsyTdbFnHW
Use DataEnvironment, ONLY: StdRhoAir

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS
    INTEGER, INTENT(IN) :: SysNum

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64) :: PriMassFlowRate = 0.0d0
  REAL(r64) :: PriPressure = 0.0d0
  REAL(r64) :: PriEnthalpy = 0.0d0
  REAL(r64) :: PriHumRat = 0.0d0
  REAL(r64) :: PriTemp = 0.0d0

  REAL(r64) :: SecAirMassFlowRate = 0.0d0
  REAL(r64) :: SecAirPressure = 0.0d0
  REAL(r64) :: SecAirEnthalpy = 0.0d0
  REAL(r64) :: SecAirHumRat = 0.0d0
  REAL(r64) :: SecAirTemp = 0.0d0

  REAL(r64) :: MixedAirMassFlowRate = 0.0d0
  REAL(r64) :: MixedAirPressure = 0.0d0
  REAL(r64) :: MixedAirEnthalpy = 0.0d0
  REAL(r64) :: MixedAirHumRat = 0.0d0
  REAL(r64) :: MixedAirTemp = 0.0d0


  PriEnthalpy = Node(SysATMixer(SysNum)%PriInNode)%Enthalpy
  PriHumRat = Node(SysATMixer(SysNum)%PriInNode)%HumRat
  PriTemp = Node(SysATMixer(SysNum)%PriInNode)%Temp
  PriMassFlowRate = Node(SysATMixer(SysNum)%PriInNode)%MassFlowRate

  SecAirMassFlowRate = Node(SysATMixer(SysNum)%SecInNode)%MassFlowRate
  SecAirEnthalpy = Node(SysATMixer(SysNum)%SecInNode)%Enthalpy
  SecAirHumRat = Node(SysATMixer(SysNum)%SecInNode)%HumRat
  SecAirTemp = Node(SysATMixer(SysNum)%SecInNode)%Temp

  IF (SysATMixer(SysNum)%MixerType == ATMixer_SupplySide) THEN
    MixedAirMassFlowRate = SecAirMassFlowRate + PriMassFlowRate
  ELSE
    ! for inlet side mixer, the mixed air flow has been set, but we don't know the secondary flow
    MixedAirMassFlowRate = Node(SysATMixer(SysNum)%MixedAirOutNode)%MassFlowRate
    SecAirMassFlowRate = MAX(MixedAirMassFlowRate - PriMassFlowRate,0.0d0)
    Node(SysATMixer(SysNum)%SecInNode)%MassFlowRate = SecAirMassFlowRate
  END IF
  ! now calculate the mixed (outlet) conditions
  IF (MixedAirMassFlowRate > 0.0d0) THEN
    MixedAirEnthalpy = (SecAirMassFlowRate*SecAirEnthalpy + PriMassFlowRate*PriEnthalpy) / MixedAirMassFlowRate
    MixedAirHumRat = (SecAirMassFlowRate*SecAirHumRat + PriMassFlowRate*PriHumRat) / MixedAirMassFlowRate
    ! Mixed air temperature is calculated from the mixed air enthalpy and humidity ratio.
    MixedAirTemp = PsyTdbFnHW(MixedAirEnthalpy,MixedAirHumRat)
  END IF

  SysATMixer(SysNum)%MixedAirMassFlowRate = MixedAirMassFlowRate
  SysATMixer(SysNum)%MixedAirEnthalpy = MixedAirEnthalpy
  SysATMixer(SysNum)%MixedAirHumRat = MixedAirHumRat
  SysATMixer(SysNum)%MixedAirTemp = MixedAirTemp

  RETURN

END SUBROUTINE CalcATMixer

SUBROUTINE UpdateATMixer(SysNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Move the results of CalcATMixer to the affected nodes

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
    USE DataLoopNode

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS
    INTEGER, INTENT(IN) :: SysNum

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: MixedAirOutNode

MixedAirOutNode = SysATMixer(SysNum)%MixedAirOutNode

! mixed air data
Node(MixedAirOutNode)%Temp = SysATMixer(SysNum)%MixedAirTemp
Node(MixedAirOutNode)%HumRat = SysATMixer(SysNum)%MixedAirHumRat
Node(MixedAirOutNode)%Enthalpy = SysATMixer(SysNum)%MixedAirEnthalpy
Node(MixedAirOutNode)%Press = SysATMixer(SysNum)%MixedAirPressure
Node(MixedAirOutNode)%MassFlowRate = SysATMixer(SysNum)%MixedAirMassFlowRate

RETURN
END SUBROUTINE UpdateATMixer

SUBROUTINE GetATMixerPriNode(ZoneEquipName,ATMixerPriNode)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR
          !       DATE WRITTEN   April 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the zone air inlet node for a given ATMixer

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ZoneEquipName
  INTEGER, INTENT(INOUT)       :: ATMixerPriNode

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ATMixerIndex ! local air terminal mixer index

  LOGICAL ErrorsFound ! for error trapping

  IF (GetATMixerFlag) THEN
    CALL GetATMixers
    GetATMixerFlag = .FALSE.
  END IF

  ATMixerIndex = FindItemInList(ZoneEquipName,SysATMixer%Name,NumATMixers)
  IF (ATMixerIndex > 0) THEN
    ATMixerPriNode = SysATMixer(ATMixerIndex)%PriInNode
  END IF

  IF (ATMixerIndex == 0) THEN
    CALL ShowSevereError('GetATMixerPriNode: Air Terminal Mixer zone air inlet node not found for zone equipment='//  &
       TRIM(ZoneEquipName))
    ErrorsFound=.TRUE.
  ENDIF

  RETURN

END SUBROUTINE GetATMixerPriNode

SUBROUTINE GetATMixerSecNode(ZoneEquipName,ATMixerSecNode)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR
          !       DATE WRITTEN   April 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the zone air inlet node for a given ATMixer

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ZoneEquipName
  INTEGER, INTENT(INOUT)       :: ATMixerSecNode

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ATMixerIndex ! local air terminal mixer index

  LOGICAL ErrorsFound ! for error trapping

  IF (GetATMixerFlag) THEN
    CALL GetATMixers
    GetATMixerFlag = .FALSE.
  END IF

  ATMixerIndex = FindItemInList(ZoneEquipName,SysATMixer%Name,NumATMixers)
  IF (ATMixerIndex > 0) THEN
    ATMixerSecNode = SysATMixer(ATMixerIndex)%SecInNode
  END IF

  IF (ATMixerIndex == 0) THEN
    CALL ShowSevereError('GetATMixerSecNode: Air Terminal Mixer zone air inlet node not found for zone equipment='//  &
       TRIM(ZoneEquipName))
    ErrorsFound=.TRUE.
  ENDIF

  RETURN

END SUBROUTINE GetATMixerSecNode

SUBROUTINE GetATMixerOutNode(ZoneEquipName,ATMixerOutNode)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR
          !       DATE WRITTEN   April 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the mixed air outlet node for a given ATMixer

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ZoneEquipName
  INTEGER, INTENT(INOUT)       :: ATMixerOutNode

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ATMixerIndex ! local air terminal mixer index

  LOGICAL ErrorsFound ! for error trapping

  IF (GetATMixerFlag) THEN
    CALL GetATMixers
    GetATMixerFlag = .FALSE.
  END IF

  ATMixerIndex = FindItemInList(ZoneEquipName,SysATMixer%Name,NumATMixers)
  IF (ATMixerIndex > 0) THEN
    ATMixerOutNode = SysATMixer(ATMixerIndex)%MixedAirOutNode
  END IF

  IF (ATMixerIndex == 0) THEN
    CALL ShowSevereError('GetATMixerOutNode: Air Terminal Mixer outlet node not found for zone equipment='//TRIM(ZoneEquipName))
    ErrorsFound=.TRUE.
  ENDIF

  RETURN

END SUBROUTINE GetATMixerOutNode

SUBROUTINE GetATMixer(ZoneEquipName,ATMixerName,ATMixerNum,ATMixerType,ATMixerPriNode,ATMixerSecNode,ATMixerOutNode)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   April 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets: 1) the index of the named AT Mixer in the SysATMixer data array
          !                       2) the node number of the primary air inlet node of the AT Mixer

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  ! USE ZoneAirLoopEquipmentManager, ONLY: GetZoneAirLoopEquipment

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: ZoneEquipName    ! zone unit name name
  CHARACTER(len=*), INTENT(OUT) :: ATMixerName      ! air terminal mixer name
  INTEGER, INTENT(OUT)          :: ATMixerNum       ! air terminal mixer index
  INTEGER, INTENT(OUT)          :: ATMixerType      ! air teminal mixer type
  INTEGER, INTENT(OUT)          :: ATMixerPriNode   ! air terminal mixer primary air node number
  INTEGER, INTENT(OUT)          :: ATMixerSecNode   ! air terminal mixer secondary air node number
  INTEGER, INTENT(OUT)          :: ATMixerOutNode   ! air terminal mixer outlet air node number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ATMixerIndex ! local air terminal mixer index

  LOGICAL ErrorsFound ! for error trapping

  IF (GetATMixerFlag) THEN
    ! CALL GetZoneAirLoopEquipment
    CALL GetATMixers
    GetATMixerFlag = .FALSE.
  END IF

  IF (NumATMixers <= 0) THEN
    ATMixerNum = 0
    ATMixerName = ' '
    ATMixerPriNode = 0
    ATMixerSecNode = 0
    ATMixerOutNode = 0
    ATMixerType    = 0
    RETURN
  END IF

  ATMixerIndex = FindItemInList(ZoneEquipName,SysATMixer%ZoneHVACUnitName,NumATMixers)
  IF (ATMixerIndex > 0) THEN
    ATMixerNum = ATMixerIndex
    ATMixerName    = SysATMixer(ATMixerIndex)%Name
    ATMixerPriNode = SysATMixer(ATMixerIndex)%PriInNode
    ATMixerSecNode = SysATMixer(ATMixerIndex)%SecInNode
    ATMixerOutNode = SysATMixer(ATMixerIndex)%MixedAirOutNode
    ATMixerType    = SysATMixer(ATMixerIndex)%MixerType
  ELSE
    ATMixerNum = 0
    ATMixerName = ' '
    ATMixerPriNode = 0
    ATMixerSecNode = 0
    ATMixerOutNode = 0
    ATMixerType    = 0
  END IF

  RETURN

END SUBROUTINE GetATMixer

SUBROUTINE SetATMixerPriFlow(ATMixerNum,PriAirMassFlowRate)

         ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   April 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This Subroutine sets the primary air mass flow rate on the primary air inlet
          ! node of a terminal unit mixer component.

          ! METHODOLOGY EMPLOYED:
          ! The flow is set to either the input PriAirMassFlowRate if this optional input
          ! parameter is present, or to the maximum available mass flow rate of the primary
          ! air inlet node.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! none

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)         :: ATMixerNum                 ! Air terminal mixer index
  REAL(r64), INTENT (IN), OPTIONAL :: PriAirMassFlowRate    ! Air terminal mixer primary air mass flow rate [kg/s]

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: PriAirNode         ! air terminal mixer primary air inlet node number

  IF (ATMixerNum <= 0) RETURN
  PriAirNode = SysATMixer(ATMixerNum)%PriInNode
  IF (PRESENT(PriAirMassFlowRate)) THEN
    Node(PriAirNode)%MassFlowRate = PriAirMassFlowRate
  ELSE
    Node(PriAirNode)%MassFlowRate = Node(PriAirNode)%MassFlowRateMaxAvail
  END IF

  RETURN

END SUBROUTINE SetATMixerPriFlow

!        End of Reporting subroutines for the Sys Module
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

End Module SingleDuct

