MODULE HVACStandAloneERV

  ! Module containing the routines dealing with stand alone energy recovery ventilators (ERVs)

  ! MODULE INFORMATION:
  !       AUTHOR         Richard Raustad, FSEC
  !       DATE WRITTEN   June 2003
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms needed to simulate stand alone
  ! energy recovery ventilators that condition outdoor ventilation air and
  ! supply that air directly to a zone.

  ! METHODOLOGY EMPLOYED:
  ! These units are modeled as a collection of components: air-to-air generic heat exchanger,
  ! supply air fan, exhaust air fan and an optional controller to avoid overheating
  ! of the supply air (economizer or free cooling operation).

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals,     ONLY: BeginEnvrnFlag, MaxNameLength, NumOfZones, SecInHour, &
                           SysSizingCalc, WarmupFlag, ScheduleAlwaysOn, DisplayExtraWarnings
USE DataInterfaces
Use DataEnvironment, ONLY: StdBaroPress, StdRhoAir
USE DataHVACGlobals

  ! Use statements for access to subroutines in other modules
USE ScheduleManager,  ONLY: GetScheduleIndex, GetCurrentScheduleValue

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS

CHARACTER(len=1), PARAMETER :: Blank=' '

INTEGER, PARAMETER :: ControllerSimple = 1
INTEGER, PARAMETER :: ControllerOutsideAir = 2
INTEGER, PARAMETER :: ControllerStandAloneERV = 3

  ! DERIVED TYPE DEFINITIONS
TYPE StandAloneERVData
  ! input data
  CHARACTER(len=MaxNameLength) :: Name                  =' ' ! name of the stand alone ERV unit
  CHARACTER(len=MaxNameLength) :: UnitType              =' ' ! ZoneHVAC:EnergyRecoveryVentilator
  INTEGER                      :: SchedPtr              =0   ! pointer to availability schedule
  CHARACTER(len=MaxNameLength) :: HeatExchangerName     =' ' ! name of the heat exchanger within the ERV unit
  INTEGER                      :: HeatExchangerIndex    =0   ! Pointer to heat exchanger
  INTEGER                      :: HeatExchangerTypeNum  =0   ! Parameter equivalent of HX object type
  INTEGER                      :: SupplyAirInletNode    =0   ! supply air inlet node for the stand alone ERV
  INTEGER                      :: SupplyAirOutletNode   =0   ! supply air outlet node for the stand alone ERV
  CHARACTER(len=MaxNameLength) :: SupplyAirFanName      =' ' ! fan name in the supply air stream of the ERV
  INTEGER                      :: SupplyAirFanIndex     =0   ! index to supply air fan
  INTEGER                      :: SupplyAirFanSchPtr    =0   ! index to supply air fan schedule
  INTEGER                      :: SupplyAirFanType_Num  =0   ! parameter equivalent of fan type
  INTEGER                      :: ExhaustAirInletNode   =0   ! exhaust air inlet node for the stand alone ERV
  INTEGER                      :: ExhaustAirOutletNode  =0   ! exhaust air outlet node for the stand alone ERV
  CHARACTER(len=MaxNameLength) :: ExhaustAirFanName     =' ' ! fan name in exhaust air stream of the ERV
  INTEGER                      :: ExhaustAirFanIndex    =0   ! index to exhaust air fan
  INTEGER                      :: ExhaustAirFanSchPtr   =0   ! index to exhaust air fan schedule
  INTEGER                      :: ExhaustAirFanType_Num =0   ! paramter equivalent of fan type
  REAL(r64)                    :: SupplyAirVolFlow      =0.0d0 ! volumetric flow rate through the supply side of the ERV
  REAL(r64)                    :: ExhaustAirVolFlow     =0.0d0 ! volumetric flow rate through the exhaust side of the ERV
  CHARACTER(len=MaxNameLength) :: ControllerName        =' ' ! name of the controller for the stand alone ERV
  LOGICAL                      :: ControllerNameDefined = .TRUE. ! controller for the stand alone ERV is defined
  INTEGER                      :: ControlledZoneNum     = 0  ! index to controlled zone for stand alone ERV
  INTEGER                      :: ControllerIndex       = 0  ! Pointer for updates by routines this module calls.
  REAL(r64)                    :: MaxSupAirMassFlow     =0.0d0 ! air mass flow rate through the supply side of the ERV
  REAL(r64)                    :: MaxExhAirMassFlow     =0.0d0 ! air mass flow rate through the exhaust side of the ERV
  REAL(r64)                    :: HighRHOAFlowRatio      =1.0d0 ! ratio of outside air flow to max outside air flow
  REAL(r64)                    :: DesignSAFanVolFlowRate = 0.0d0 ! SA fan volumetric flow rate
  REAL(r64)                    :: DesignEAFanVolFlowRate = 0.0d0 ! EA fan volumetric flow rate
  REAL(r64)                    :: DesignSAFanMassFlowRate = 0.0d0 ! SA fan mass flow rate
  REAL(r64)                    :: DesignEAFanMassFlowRate = 0.0d0 ! EA fan mass flow rate
  REAL(r64)                    :: AirVolFlowPerFloorArea = 0.0d0  ! Air flow rate per unit floor area, used for autosizing
  REAL(r64)                    :: AirVolFlowPerOccupant  = 0.0d0  ! Air flow rate per occupant, used for autosizing
  INTEGER                      :: EconomizerOASchedPtr        = 0    ! schedule to modify outdoor air
  LOGICAL                      :: FlowError              = .TRUE. ! used for one-time warning message for flow imbalance (Init)
  INTEGER                      :: AvailStatus         =0
  CHARACTER(len=MaxNameLength) :: AvailManagerListName = ' ' ! Name of an availability manager list object

  ! report variables
  REAL(r64)        :: ElecUseRate       =0.0d0 ! total electric use rate (power) for supply/exhaust fans & generic HX parasitics [W]
  REAL(r64)        :: ElecUseEnergy     =0.0d0 ! electric energy use for supply fan, exhaust fan, and generic HX parasitics [J]
  REAL(r64)        :: SensCoolingEnergy =0.0d0 ! sensible cooling energy delivered by the ERV supply air to the zone [J]
  REAL(r64)        :: SensCoolingRate   =0.0d0 ! rate of sensible cooling delivered to the zone [W]
  REAL(r64)        :: LatCoolingEnergy  =0.0d0 ! latent cooling energy delivered by the ERV supply air to the zone [J]
  REAL(r64)        :: LatCoolingRate    =0.0d0 ! rate of latent cooling delivered to the zone [W]
  REAL(r64)        :: TotCoolingEnergy  =0.0d0 ! total cooling energy delivered by the ERV supply air to the zone [J]
  REAL(r64)        :: TotCoolingRate    =0.0d0 ! rate of total cooling delivered to the zone [W]
  REAL(r64)        :: SensHeatingEnergy =0.0d0 ! sensible heating energy delivered by the ERV supply air to the zone [J]
  REAL(r64)        :: SensHeatingRate   =0.0d0 ! rate of sensible heating delivered to the zone [W]
  REAL(r64)        :: LatHeatingEnergy  =0.0d0 ! latent heating energy delivered by the ERV supply air to the zone [J]
  REAL(r64)        :: LatHeatingRate    =0.0d0 ! rate of latent heating delivered to the zone [W]
  REAL(r64)        :: TotHeatingEnergy  =0.0d0 ! total heating energy delivered by the ERV supply air to the zone [J]
  REAL(r64)        :: TotHeatingRate    =0.0d0 ! rate of total heating delivered to the zone [W]

END TYPE StandAloneERVData

  ! MODULE VARIABLE DECLARATIONS:
TYPE (StandAloneERVData), ALLOCATABLE, DIMENSION(:) :: StandAloneERV

INTEGER  :: NumStandAloneERVs                           ! Total number of stand alone ERVs defined in the idf

LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlag
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName
LOGICAL :: GetERVInputFlag = .TRUE.  ! First time, input is "gotten"

  ! SUBROUTINE SPECIFICATIONS FOR MODULE

          ! Driver/Manager Routine
PUBLIC SimStandAloneERV

          ! Algorithms/Calculation routine for the module
PRIVATE CalcStandAloneERV

          ! Get Input routine for module
PRIVATE GetStandAloneERV

          ! Sizing routine for the module
PRIVATE SizeStandAloneERV

          ! Initialization routine for module
PRIVATE InitStandAloneERV
PRIVATE ReportStandAloneERV

          ! Utility routines for module
PRIVATE GetSupplyAirFlowRate
PRIVATE GetSupplyAirInletNode
PRIVATE GetExhaustAirInletNode

PUBLIC  GetStandAloneERVOutAirNode
PUBLIC  GetStandAloneERVReturnAirNode
PUBLIC  GetStandAloneERVZoneInletAirNode

CONTAINS

SUBROUTINE SimStandAloneERV(CompName,ZoneNum,FirstHVACIteration,SensLoadMet,LatLoadMet,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   June 2003
          !       MODIFIED       Don Shirey, Aug 2009 (LatLoadMet)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Manages the simulation of a Stand Alone ERV unit. Called from SimZoneEquipment

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItem,MakeUPPERCase
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL,          INTENT (IN)  :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep
  CHARACTER(len=*), INTENT (IN)  :: CompName            ! name of the Stand Alone ERV unit
! ZoneNum not used at this time, future modifications may require zone information
  INTEGER,          INTENT (IN)  :: ZoneNum             ! number of zone being served unused1208
  REAL(r64),        INTENT (OUT) :: SensLoadMet         ! net sensible load supplied by the ERV unit to the zone (W)
  REAL(r64),        INTENT (OUT) :: LatLoadMet          ! net latent load supplied by ERV unit to the zone (kg/s),
                                                        ! dehumid = negative
  INTEGER,          INTENT(INOUT):: CompIndex           ! pointer to correct component

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: StandAloneERVNum       ! index of Stand Alone ERV unit being simulated

! First time SimStandAloneERV is called, get the input for all Stand Alone ERV units
  IF (GetERVInputFlag) THEN
    CALL GetStandAloneERV
    GetERVInputFlag = .FALSE.
  END IF

  ! Find the correct Stand Alone ERV unit index
  IF (CompIndex == 0) THEN
    StandAloneERVNum = FindItem(CompName,StandAloneERV%Name,NumStandAloneERVs)
    IF (StandAloneERVNum == 0) THEN
      CALL ShowFatalError('SimStandAloneERV: Unit not found='//TRIM(CompName))
    ENDIF
    CompIndex=StandAloneERVNum
  ELSE
    StandAloneERVNum=CompIndex
    IF (StandAloneERVNum > NumStandAloneERVs .or. StandAloneERVNum < 1) THEN
      CALL ShowFatalError('SimStandAloneERV:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(StandAloneERVNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumStandAloneERVs))//  &
                          ', Entered Unit name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(StandAloneERVNum)) THEN
      IF (CompName /= StandAloneERV(StandAloneERVNum)%Name) THEN
        CALL ShowFatalError('SimStandAloneERV: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(StandAloneERVNum))// &
                            ', Unit name='//TRIM(CompName)//', stored Unit Name for that index='//  &
                            TRIM(StandAloneERV(StandAloneERVNum)%Name))
      ENDIF
      CheckEquipName(StandAloneERVNum)=.false.
    ENDIF
  ENDIF

! Initialize the Stand Alone ERV unit
  CALL InitStandAloneERV(StandAloneERVNum,ZoneNum,FirstHVACIteration)

  CALL CalcStandAloneERV(StandAloneERVNum,FirstHVACIteration,SensLoadMet,LatLoadMet)

  CALL ReportStandAloneERV(StandAloneERVNum)

  RETURN
END SUBROUTINE SimStandAloneERV

SUBROUTINE GetStandAloneERV

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   June 2003
          !       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for Stand Alone ERV units and stores it in the Stand Alone ERV data structure

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, GetObjectItemNum, GetObjectDefMaxArgs, &
                            FindItemInList, SameString
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: SetUpCompSets
  USE MixedAir, ONLY: SetOAControllerData,CheckOAControllerName
  USE DataHeatBalance, ONLY: Zone
  USE DataZoneEquipment, ONLY: ZoneEquipConfig, ERVStandAlone_Num
  USE DataZoneControls, ONLY: HumidityControlZone, NumHumidityControlZones
  USE Fans, ONLY: GetFanAvailSchPtr, GetFanType, GetFanDesignVolumeFlowRate, GetFanIndex, &
                  GetFanOutletNode
  USE DataSizing, ONLY: AutoSize
  USE General, ONLY: RoundSigDigits
  USE HeatRecovery, ONLY: GetGenericSupplyAirFlowRate=>GetSupplyAirFlowRate, GetHeatExchangerObjectTypeNum, &
                          GetHXSupplyInletNode=>GetSupplyInletNode, GetHXSupplyOutletNode=>GetSupplyOutletNode, &
                          GetHXSecondaryInletNode=>GetSecondaryInletNode, GetHXSecondaryOutletNode=>GetSecondaryOutletNode
  USE OutAirNodeManager, ONLY: CheckOutAirNodeNumber
  USE CurveManager, ONLY: GetCurveIndex, GetCurveType
  USE DataIPShortCuts


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
  INTEGER                        :: StandAloneERVIndex ! loop index
  INTEGER                        :: StandAloneERVNum   ! current Stand Alone ERV number
  CHARACTER(len=MaxNameLength), ALLOCATABLE, &
                          DIMENSION(:) :: Alphas     ! Alpha items for object
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers    ! Numeric items for object
  CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields
  CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaBlanks
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericBlanks
  CHARACTER(len=MaxNameLength) :: CompSetSupplyFanInlet, CompSetSupplyFanOutlet
  CHARACTER(len=MaxNameLength) :: CompSetExhaustFanInlet, CompSetExhaustFanOutlet
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject ! Object type for getting and error messages
  INTEGER                         :: SAFanTypeNum  ! Integer equivalent to fan type
  INTEGER                         :: EAFanTypeNum  ! Integer equivalent to fan type
  INTEGER                         :: NumArg
  INTEGER                         :: NumAlphas  ! Number of Alphas for each GetObjectItem call
  INTEGER                         :: NumNumbers ! Number of Numbers for each GetObjectItem call
  INTEGER                         :: MaxAlphas  ! Max between the two objects gotten here
  INTEGER                         :: MaxNumbers ! Max between the two objects gotten here
  INTEGER                         :: IOStatus   ! Used in GetObjectItem
  LOGICAL                         :: ErrorsFound=.FALSE.  ! Set to true if errors in input, fatal at end of routine
  LOGICAL                         :: IsNotOK    ! Flag to verify name
  LOGICAL                         :: IsBlank    ! Flag for blank name
  INTEGER                         :: OutAirNum               ! total number of CONTROLLER:OUTSIDE AIR objects
  INTEGER                         :: NumERVCtrlrs            ! total number of CONTROLLER:STAND ALONE ERV objects
  INTEGER                         :: ERVControllerNum        ! index to ERV controller
  INTEGER                         :: WhichERV                ! used in controller GetInput
  REAL(r64)                       :: AirFlowRate             ! used to find zone with humidistat
  INTEGER                         :: NodeNumber              ! used to find zone with humidistat
  INTEGER                         :: HStatZoneNum            ! used to find zone with humidistat
  INTEGER                         :: NumHstatZone            ! index to humidity controlled zones
  INTEGER                         :: ControlledZoneNum = 0   ! used to find zone with humidistat
  LOGICAL                         :: ZoneNodeFound = .FALSE. ! used to find zone with humidistat
  LOGICAL                         :: HStatFound = .FALSE.    ! used to find zone with humidistat
  LOGICAL                         :: ErrFlag                 ! Error flag used in mining calls
  REAL(r64)                       :: SAFanVolFlowRate        ! supply air fan volumetric flow rate [m3/s]
  REAL(r64)                       :: EAFanVolFlowRate        ! exhaust air fan volumetric flow rate [m3/s]
  REAL(r64)                       :: HXSupAirFlowRate        ! HX supply air flow rate [m3/s]
  REAL(r64)                       :: HighRHOARatio           ! local variable for HighRHOAFlowRatio
  LOGICAL                         :: ZoneInletNodeFound      ! used for warning when zone node not listed in equipment connections
  LOGICAL                         :: ZoneExhaustNodeFound    ! used for warning when zone node not listed in equipment connections
  INTEGER                         :: ZoneInletCZN            ! used for warning when zone node not listed in equipment connections
  INTEGER                         :: ZoneExhaustCZN          ! used for warning when zone node not listed in equipment connections

  CALL GetObjectDefMaxArgs('ZoneHVAC:EnergyRecoveryVentilator',NumArg,NumAlphas,NumNumbers)
  MaxAlphas=NumAlphas
  MaxNumbers=NumNumbers
  CALL GetObjectDefMaxArgs('ZoneHVAC:EnergyRecoveryVentilator:Controller',NumArg,NumAlphas,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  MaxNumbers=MAX(MaxNumbers,NumNumbers)

  ALLOCATE(Alphas(MaxAlphas))
  Alphas=' '
  ALLOCATE(Numbers(MaxNumbers))
  Numbers=0.0d0
  ALLOCATE(cAlphaFields(MaxAlphas))
  cAlphaFields=' '
  ALLOCATE(cNumericFields(MaxNumbers))
  cNumericFields=' '
  ALLOCATE(lNumericBlanks(MaxNumbers))
  lNumericBlanks = .FALSE.
  ALLOCATE(lAlphaBlanks(MaxAlphas))
  lAlphaBlanks = .FALSE.

  GetERVInputFlag=.FALSE.

  ! find the number of each type of Stand Alone ERV unit
  CurrentModuleObject = 'ZoneHVAC:EnergyRecoveryVentilator'

  NumStandAloneERVs = GetNumObjectsFound(CurrentModuleObject)

  ! allocate the data structures
  ALLOCATE(StandAloneERV(NumStandAloneERVs))
  ALLOCATE(CheckEquipName(NumStandAloneERVs))
  CheckEquipName=.true.

! loop over Stand Alone ERV units; get and load the input data
  DO StandAloneERVIndex = 1,NumStandAloneERVs

    CALL GetObjectItem(CurrentModuleObject,StandAloneERVIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                       AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)
    StandAloneERVNum = StandAloneERVIndex ! separate variables in case other objects read by this module at some point later
    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(Alphas(1),StandAloneERV%Name,StandAloneERVNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.TRUE.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    StandAloneERV(StandAloneERVNum)%Name = Alphas(1)
    StandAloneERV(StandAloneERVNum)%UnitType = CurrentModuleObject

    IF (lAlphaBlanks(2)) THEN
      StandAloneERV(StandAloneERVNum)%SchedPtr = ScheduleAlwaysOn
    ELSE
      StandAloneERV(StandAloneERVNum)%SchedPtr = GetScheduleIndex(Alphas(2))  ! convert schedule name to pointer
      IF (StandAloneERV(StandAloneERVNum)%SchedPtr .EQ. 0) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(StandAloneERV(StandAloneERVNum)%Name)//&
             '" '//TRIM(cAlphaFields(2))//' not found = '//TRIM(Alphas(2)))
        ErrorsFound=.TRUE.
      ENDIF
    END IF

    CALL VerifyName(Alphas(3),StandAloneERV%HeatExchangerName,StandAloneERVNum-1,IsNotOK,IsBlank,  &
                              'HeatExchanger:AirToAir:SensibleAndLatent')
    IF (IsNotOK) THEN
      ErrorsFound=.TRUE.
      IF (IsBlank) Alphas(3)='xxxxx'
    ENDIF
    StandAloneERV(StandAloneERVNum)%HeatExchangerName = Alphas(3)
    ErrFlag = .FALSE.
    StandAloneERV(StandAloneERVNum)%HeatExchangerTypeNum = &
                              GetHeatExchangerObjectTypeNum(StandAloneERV(StandAloneERVNum)%HeatExchangerName,ErrFlag)
    IF(ErrFlag)THEN
      CALL ShowContinueError('... occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(StandAloneERV(StandAloneERVNum)%Name)//'"')
      ErrorsFound = .TRUE.
    END IF

    ErrFlag = .FALSE.
    HXSupAirFlowRate = GetGenericSupplyAirFlowRate(StandAloneERV(StandAloneERVNum)%HeatExchangerName, ErrFlag)
    IF(ErrFlag)THEN
      CALL ShowContinueError('... occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(StandAloneERV(StandAloneERVNum)%Name)//'"')
      ErrorsFound = .TRUE.
    END IF

    StandAloneERV(StandAloneERVNum)%SupplyAirFanName = Alphas(4)
    CALL VerifyName(Alphas(4),StandAloneERV%SupplyAirFanName,StandAloneERVNum-1,IsNotOK,IsBlank,'Fan:OnOff')
    IF (IsNotOK) THEN
      ErrorsFound=.TRUE.
      IF (IsBlank) Alphas(4)='xxxxx'
    ENDIF

    ErrFlag = .FALSE.
    CALL GetFanType(StandAloneERV(StandAloneERVNum)%SupplyAirFanName,SAFanTypeNum,ErrFlag,  &
          CurrentModuleObject,StandAloneERV(StandAloneERVNum)%Name)
    IF(ErrFlag)THEN
      ErrorsFound = .TRUE.
    END IF
    StandAloneERV(StandAloneERVNum)%SupplyAirFanType_Num = SAFanTypeNum

    ErrFlag = .FALSE.
    StandAloneERV(StandAloneERVNum)%SupplyAirFanSchPtr = GetFanAvailSchPtr(cFanTypes(SAFanTypeNum), &
                  StandAloneERV(StandAloneERVNum)%SupplyAirFanName,ErrFlag)
    IF(ErrFlag)THEN
      CALL ShowContinueError('... occurs in '//TRIM(CurrentModuleObject)//' "'//TRIM(StandAloneERV(StandAloneERVNum)%Name)//'"')
      ErrorsFound = .TRUE.
    END IF

    CALL GetFanIndex(StandAloneERV(StandAloneERVNum)%SupplyAirFanName,StandAloneERV(StandAloneERVNum)%SupplyAirFanIndex, &
                     ErrFlag,TRIM(CurrentModuleObject)//' "'//TRIM(StandAloneERV(StandAloneERVNum)%Name)//'"')

    !Set the SA Design Fan Volume Flow Rate
    ! get from fan module
    ErrFlag = .FALSE.
    SAFanVolFlowRate =    &
       GetFanDesignVolumeFlowRate(cFanTypes(SAFanTypeNum),StandAloneERV(StandAloneERVNum)%SupplyAirFanName,ErrFlag)
    IF (ErrFlag) THEN
      CALL ShowContinueError('... occurs in '//TRIM(CurrentModuleObject)//' ='//TRIM(StandAloneERV(StandAloneERVNum)%Name))
      ErrorsFound=.TRUE.
    ENDIF
    StandAloneERV(StandAloneERVNum)%DesignSAFanVolFlowRate = SAFanVolFlowRate

    StandAloneERV(StandAloneERVNum)%ExhaustAirFanName = Alphas(5)
    CALL VerifyName(Alphas(5),StandAloneERV%ExhaustAirFanName,StandAloneERVNum-1,IsNotOK,IsBlank,'Fan:OnOff Name')
    IF (IsNotOK) THEN
      ErrorsFound=.TRUE.
      IF (IsBlank) Alphas(5)='xxxxx'
    ENDIF
    ErrFlag = .FALSE.
    CALL GetFanType(StandAloneERV(StandAloneERVNum)%ExhaustAirFanName,EAFanTypeNum,ErrFlag,  &
       CurrentModuleObject,StandAloneERV(StandAloneERVNum)%Name)
    IF (.not. ErrFlag) THEN
      StandAloneERV(StandAloneERVNum)%ExhaustAirFanType_Num = EAFanTypeNum
      ! error for fan availability schedule?
      StandAloneERV(StandAloneERVNum)%ExhaustAirFanSchPtr = GetFanAvailSchPtr(cFanTypes(EAFanTypeNum), &
                    StandAloneERV(StandAloneERVNum)%ExhaustAirFanName,ErrFlag)
      CALL GetFanIndex(StandAloneERV(StandAloneERVNum)%ExhaustAirFanName,StandAloneERV(StandAloneERVNum)%ExhaustAirFanIndex, &
                       ErrFlag,TRIM(CurrentModuleObject)//' "'//TRIM(StandAloneERV(StandAloneERVNum)%Name)//'"')
    ELSE
      ErrorsFound = .TRUE.
    END IF

    !Set the EA Design Fan Volume Flow Rate
    ! get from fan module
    ErrFlag = .FALSE.
    EAFanVolFlowRate =    &
       GetFanDesignVolumeFlowRate(cFanTypes(EAFanTypeNum),StandAloneERV(StandAloneERVNum)%ExhaustAirFanName,ErrFlag)
    IF (ErrFlag) THEN
      CALL ShowContinueError('... occurs in '//TRIM(CurrentModuleObject)//' ='//TRIM(StandAloneERV(StandAloneERVNum)%Name))
      ErrorsFound=.TRUE.
    ENDIF
    StandAloneERV(StandAloneERVNum)%DesignEAFanVolFlowRate = EAFanVolFlowRate


    ErrFlag = .FALSE.
    StandAloneERV(StandAloneERVNum)%SupplyAirInletNode = &
                  GetHXSupplyInletNode(StandAloneERV(StandAloneERVNum)%HeatExchangerName, ErrFlag)
    StandAloneERV(StandAloneERVNum)%ExhaustAirInletNode = &
                  GetHXSecondaryInletNode(StandAloneERV(StandAloneERVNum)%HeatExchangerName, ErrFlag)
    IF (ErrFlag) THEN
      CALL ShowContinueError('... occurs in '//TRIM(CurrentModuleObject)//' ='//TRIM(StandAloneERV(StandAloneERVNum)%Name))
      ErrorsFound=.TRUE.
    ENDIF

    ErrFlag = .FALSE.
    StandAloneERV(StandAloneERVNum)%SupplyAirOutletNode = &
                  GetFanOutletNode(cFanTypes(SAFanTypeNum),StandAloneERV(StandAloneERVNum)%SupplyAirFanName,ErrFlag)
    StandAloneERV(StandAloneERVNum)%ExhaustAirOutletNode = &
                  GetFanOutletNode(cFanTypes(EAFanTypeNum),StandAloneERV(StandAloneERVNum)%ExhaustAirFanName,ErrFlag)
    IF (ErrFlag) THEN
      CALL ShowContinueError('... occurs in '//TRIM(CurrentModuleObject)//' ='//TRIM(StandAloneERV(StandAloneERVNum)%Name))
      ErrorsFound=.TRUE.
    ENDIF

    StandAloneERV(StandAloneERVNum)%SupplyAirInletNode = &
               GetOnlySingleNode(NodeID(StandAloneERV(StandAloneERVNum)%SupplyAirInletNode), &
               ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)
    StandAloneERV(StandAloneERVNum)%SupplyAirOutletNode = &
               GetOnlySingleNode(NodeID(StandAloneERV(StandAloneERVNum)%SupplyAirOutletNode), &
               ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)
    StandAloneERV(StandAloneERVNum)%ExhaustAirInletNode = &
               GetOnlySingleNode(NodeID(StandAloneERV(StandAloneERVNum)%ExhaustAirInletNode), &
               ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Air,NodeConnectionType_Inlet,2,ObjectIsParent)
    StandAloneERV(StandAloneERVNum)%ExhaustAirOutletNode = &
               GetOnlySingleNode(NodeID(StandAloneERV(StandAloneERVNum)%ExhaustAirOutletNode), &
               ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Air,NodeConnectionType_ReliefAir,2,ObjectIsParent)

!   Check that supply air inlet node is an OA node
    IF (.not. CheckOutAirNodeNumber(StandAloneERV(StandAloneERVNum)%SupplyAirInletNode)) THEN
      CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(StandAloneERV(StandAloneERVNum)%Name)//'"')
      CALL ShowContinueError(' Node name of supply air inlet node not valid Outdoor Air Node = '//  &
         TRIM(NodeID(StandAloneERV(StandAloneERVNum)%SupplyAirInletNode)))
      CALL ShowContinueError('...does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.')
      ErrorsFound=.true.
    END IF

!   Check to make sure inlet and exhaust nodes are listed in a ZoneHVAC:EquipmentConnections object
    ZoneInletNodeFound = .FALSE.
    ZoneExhaustNodeFound = .FALSE.
    DO ControlledZoneNum = 1,NumOfZones
      IF(.NOT. ZoneInletNodeFound)THEN
        DO NodeNumber = 1,ZoneEquipConfig(ControlledZoneNum)%NumInletNodes
          IF(ZoneEquipConfig(ControlledZoneNum)%InletNode(NodeNumber)==StandAloneERV(StandAloneERVNum)%SupplyAirOutletNode)THEN
            ZoneInletNodeFound = .TRUE.
            ZoneInletCZN = ControlledZoneNum
            EXIT  ! found zone inlet node
          END IF
        END DO
      END IF
      IF(.NOT. ZoneExhaustNodeFound)THEN
        DO NodeNumber = 1,ZoneEquipConfig(ControlledZoneNum)%NumExhaustNodes
          IF(ZoneEquipConfig(ControlledZoneNum)%ExhaustNode(NodeNumber)==StandAloneERV(StandAloneERVNum)%ExhaustAirInletNode)THEN
            ZoneExhaustNodeFound = .TRUE.
            ZoneExhaustCZN = ControlledZoneNum
            EXIT  ! found zone exhaust node
          END IF
        END DO
      END IF
    END DO
    IF(.NOT. ZoneInletNodeFound)THEN
      CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(StandAloneERV(StandAloneERVNum)%Name)//'"')
      CALL ShowContinueError('... Node name of supply air outlet node does not appear in a ZoneHVAC:EquipmentConnections object.')
      CALL ShowContinueError('... Supply air outlet node = '//TRIM(NodeID(StandAloneERV(StandAloneERVNum)%SupplyAirOutletNode)))
      ErrorsFound=.true.
    END IF
    IF(.NOT. ZoneExhaustNodeFound)THEN
      CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(StandAloneERV(StandAloneERVNum)%Name)//'"')
      CALL ShowContinueError('... Node name of exhaust air inlet node does not appear in a ZoneHVAC:EquipmentConnections object.')
      CALL ShowContinueError('... Exhaust air inlet node = '//TRIM(NodeID(StandAloneERV(StandAloneERVNum)%ExhaustAirInletNode)))
      ErrorsFound=.true.
    END IF
!   If nodes are found, make sure they are in the same zone
    IF(ZoneInletNodeFound .AND. ZoneExhaustNodeFound)THEN
      IF(ZoneInletCZN /= ZoneExhaustCZN)THEN
        CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' "'//TRIM(StandAloneERV(StandAloneERVNum)%Name)//'"')
        CALL ShowContinueError('... Node name of supply air outlet node and exhasut air inlet node must appear in the same '// &
                               'ZoneHVAC:EquipmentConnections object.')
        CALL ShowContinueError('... Supply air outlet node = '//TRIM(NodeID(StandAloneERV(StandAloneERVNum)%SupplyAirOutletNode)))
        CALL ShowContinueError('... ZoneHVAC:EquipmentConnections Zone Name = '//TRIM(ZoneEquipConfig(ZoneInletCZN)%ZoneName))
        CALL ShowContinueError('... Exhaust air inlet node = '//TRIM(NodeID(StandAloneERV(StandAloneERVNum)%ExhaustAirInletNode)))
        CALL ShowContinueError('... ZoneHVAC:EquipmentConnections Zone Name = '//TRIM(ZoneEquipConfig(ZoneExhaustCZN)%ZoneName))
        ErrorsFound=.true.
      END IF
    END IF

    StandAloneERV(StandAloneERVNum)%ControllerName = Alphas(6)
    ! If controller name is blank the ERV unit will operate with no controller
    IF(lAlphaBlanks(6))THEN
      StandAloneERV(StandAloneERVNum)%ControllerName = 'xxxxx'
      StandAloneERV(StandAloneERVNum)%ControllerNameDefined = .FALSE.
    ELSE
    ! Verify controller name in Stand Alone ERV object matches name of valid controller object
      CALL VerifyName(Alphas(6),StandAloneERV%ControllerName,StandAloneERVNum-1,IsNotOK,IsBlank,  &
         'ZoneHVAC:EnergyRecoveryVentilator:Controller Name')
      IF (IsNotOK) THEN
        ErrorsFound=.TRUE.
        IF (IsBlank) Alphas(6)='xxxxx'
      ENDIF

      IF(GetObjectItemNum('ZoneHVAC:EnergyRecoveryVentilator:Controller',StandAloneERV(StandAloneERVNum)%ControllerName) <= 0)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' controller '&
                            //'type ZoneHVAC:EnergyRecoveryVentilator:Controller not found = '//TRIM(Alphas(6)))
        ErrorsFound = .TRUE.
      ENDIF
    ENDIF

    IF (.NOT. lAlphaBlanks(7)) THEN
     StandAloneERV(StandAloneERVNum)%AvailManagerListName = Alphas(7)
     ZoneComp(ERVStandAlone_Num)%ZoneCompAvailMgrs(StandAloneERVNum)%AvailManagerListName  = Alphas(7)
    ENDIF

  ! Read supply and exhaust air flow rates
    StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow = Numbers(1)
    StandAloneERV(StandAloneERVNum)%ExhaustAirVolFlow = Numbers(2)

  ! Read ventilation rate per floor area for autosizing HX and fans
    StandAloneERV(StandAloneERVNum)%AirVolFlowPerFloorArea = Numbers(3)
    StandAloneERV(StandAloneERVNum)%AirVolFlowPerOccupant  = Numbers(4)

    IF(StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow == AutoSize .AND. SAFanVolFlowRate /= AutoSize)THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(StandAloneERV(StandAloneERVNum)%Name)//'"')
      CALL ShowContinueError('... When autosizing ERV, supply air fan = '//TRIM(cFanTypes(SAFanTypeNum))//' "'// &
                             TRIM(StandAloneERV(StandAloneERVNum)%SupplyAirFanName)//'" must also be autosized.')
    END IF

    IF(StandAloneERV(StandAloneERVNum)%ExhaustAirVolFlow == AutoSize .AND. EAFanVolFlowRate /= AutoSize)THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(StandAloneERV(StandAloneERVNum)%Name)//'"')
      CALL ShowContinueError('... When autosizing ERV, exhaust air fan = '//TRIM(cFanTypes(EAFanTypeNum))//' "'// &
                             TRIM(StandAloneERV(StandAloneERVNum)%ExhaustAirFanName)//'" must also be autosized.')
    END IF

    IF(StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow == AutoSize .AND. HXSupAirFlowRate /= AutoSize)THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(StandAloneERV(StandAloneERVNum)%Name)//'"')
      CALL ShowContinueError('... When autosizing ERV '//TRIM(cNumericFields(1))//', nominal supply air flow rate for heat '// &
                  'exchanger with name = '//TRIM(StandAloneERV(StandAloneERVNum)%HeatExchangerName)//' must also be autosized.')
    END IF

    IF(StandAloneERV(StandAloneERVNum)%ExhaustAirVolFlow == AutoSize .AND. HXSupAirFlowRate /= AutoSize)THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(StandAloneERV(StandAloneERVNum)%Name)//'"')
      CALL ShowContinueError('... When autosizing ERV '//TRIM(cNumericFields(2))//', nominal supply air flow rate for heat '// &
                  'exchanger with name = '//TRIM(StandAloneERV(StandAloneERVNum)%HeatExchangerName)//' must also be autosized.')
    END IF

    ! Compare the ERV SA flow rates to SA fan object.
    IF (SAFanVolFlowRate /= AutoSize .and. StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow /= AutoSize) THEN
      IF (StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow > SAFanVolFlowRate) THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' = '//TRIM(StandAloneERV(StandAloneERVNum)%Name)//  &
            ' has a '//TRIM(cNumericFields(1))//' > Max Volume Flow Rate defined in the associated fan object, should be <=')
        CALL ShowContinueError('... Entered value='//TRIM(RoundSigDigits(StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow,2))//  &
            '... Fan ['//TRIM(cFanTypes(SAFanTypeNum))//' "'//TRIM(StandAloneERV(StandAloneERVNum)%SupplyAirFanName)//  &
            '"] Max Value = '//TRIM(RoundSigDigits(SAFanVolFlowRate,2)))
        CALL ShowContinueError(' The ERV '//TRIM(cNumericFields(1))//' is reset to the' &
                             //' supply air fan flow rate and the simulation continues.')
        StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow = SAFanVolFlowRate
      ENDIF
    ENDIF
    IF (StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow /= AutoSize) THEN
      IF (StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow <= 0.0d0) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(StandAloneERV(StandAloneERVNum)%Name)//  &
            ' has a '//TRIM(cNumericFields(1))//' <= 0.0, it must be >0.0')
        CALL ShowContinueError('... Entered value='//TRIM(RoundSigDigits(StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow,2)))
        ErrorsFound=.TRUE.
      ENDIF
    ELSE
      IF(StandAloneERV(StandAloneERVNum)%AirVolFlowPerFloorArea .EQ. 0.0d0 .AND. &
         StandAloneERV(StandAloneERVNum)%AirVolFlowPerOccupant .EQ. 0.0d0)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(StandAloneERV(StandAloneERVNum)%Name)//'"')
        CALL ShowContinueError('... Autosizing '//TRIM(cNumericFields(1))//' requires at least one input for '// &
                               TRIM(cNumericFields(3))//' or '//TRIM(cNumericFields(4))//'.')
        ErrorsFound=.TRUE.
      END IF
      ! both inputs must be autosized
      IF (StandAloneERV(StandAloneERVNum)%ExhaustAirVolFlow /= AutoSize) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(StandAloneERV(StandAloneERVNum)%Name)//'"')
        CALL ShowContinueError('... When autosizing, '//TRIM(cNumericFields(1))//' and '//TRIM(cNumericFields(2))//&
                               ' must both be autosized.')
        ErrorsFound=.TRUE.
      END IF
    ENDIF

    ! Compare the ERV EA flow rates to EA fan object.
    IF (EAFanVolFlowRate /= AutoSize .and. StandAloneERV(StandAloneERVNum)%ExhaustAirVolFlow /= AutoSize) THEN
      IF (StandAloneERV(StandAloneERVNum)%ExhaustAirVolFlow > EAFanVolFlowRate) THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' = '//TRIM(StandAloneERV(StandAloneERVNum)%Name)//  &
            ' has an '//TRIM(cNumericFields(2))//' > Max Volume Flow Rate defined in the associated fan object, should be <=')
        CALL ShowContinueError('... Entered value='//TRIM(RoundSigDigits(StandAloneERV(StandAloneERVNum)%ExhaustAirVolFlow,2))// &
            '... Fan ['//TRIM(cFanTypes(EAFanTypeNum))//':'//TRIM(StandAloneERV(StandAloneERVNum)%ExhaustAirFanName)//  &
            '] Max Value = '//TRIM(RoundSigDigits(EAFanVolFlowRate,2)))
        CALL ShowContinueError(' The ERV '//TRIM(cNumericFields(2))//' is reset to the' &
                             //' exhaust air fan flow rate and the simulation continues.')
        StandAloneERV(StandAloneERVNum)%ExhaustAirVolFlow = EAFanVolFlowRate
      ENDIF
    ENDIF
    IF (StandAloneERV(StandAloneERVNum)%ExhaustAirVolFlow /= AutoSize) THEN
      IF (StandAloneERV(StandAloneERVNum)%ExhaustAirVolFlow <= 0.0d0) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(StandAloneERV(StandAloneERVNum)%Name)//  &
            ' has an '//TRIM(cNumericFields(2))//' <= 0.0, it must be >0.0')
        CALL ShowContinueError('... Entered value='//TRIM(RoundSigDigits(StandAloneERV(StandAloneERVNum)%ExhaustAirVolFlow,2)))
        ErrorsFound=.TRUE.
      ENDIF
    ELSE
      IF(StandAloneERV(StandAloneERVNum)%AirVolFlowPerFloorArea .EQ. 0.0d0 .AND. &
         StandAloneERV(StandAloneERVNum)%AirVolFlowPerOccupant .EQ. 0.0d0)THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(StandAloneERV(StandAloneERVNum)%Name)//'"')
        CALL ShowContinueError('... Autosizing '//TRIM(cNumericFields(2))//' requires at least one input for '// &
                               TRIM(cNumericFields(3))//' or '//TRIM(cNumericFields(4))//'.')
        ErrorsFound=.TRUE.
      END IF
      IF (StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow /= AutoSize) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(StandAloneERV(StandAloneERVNum)%Name)//'"')
        CALL ShowContinueError('... When autosizing, '//TRIM(cNumericFields(1))//' and '//TRIM(cNumericFields(2))//&
                               ' must both be autosized.')
        ErrorsFound=.TRUE.
      END IF
    ENDIF

  ! Add supply fan to component sets array
    CompSetSupplyFanInlet   = 'UNDEFINED'
    CompSetSupplyFanOutlet  = NodeID(StandAloneERV(StandAloneERVNum)%SupplyAirOutletNode)

  ! Add exhaust fan to component sets array
    CompSetExhaustFanInlet   = 'UNDEFINED'
    CompSetExhaustFanOutlet  = NodeID(StandAloneERV(StandAloneERVNum)%ExhaustAirOutletNode)

  ! Add HX to component sets array
    CALL SetUpCompSets(StandAloneERV(StandAloneERVNum)%UnitType, StandAloneERV(StandAloneERVNum)%Name, &
                     'UNDEFINED',StandAloneERV(StandAloneERVNum)%HeatExchangerName,'UNDEFINED','UNDEFINED')

  ! Add supply fan to component sets array
    CALL SetUpCompSets(StandAloneERV(StandAloneERVNum)%UnitType, StandAloneERV(StandAloneERVNum)%Name, &
                     'UNDEFINED',StandAloneERV(StandAloneERVNum)%SupplyAirFanName,CompSetSupplyFanInlet,CompSetSupplyFanOutlet)

  ! Add exhaust fan to component sets array
    CALL SetUpCompSets(StandAloneERV(StandAloneERVNum)%UnitType, StandAloneERV(StandAloneERVNum)%Name, &
                     'UNDEFINED',StandAloneERV(StandAloneERVNum)%ExhaustAirFanName,CompSetExhaustFanInlet,CompSetExhaustFanOutlet)

  ! Verify HX name in Stand Alone ERV object matches name of valid HX object
    IF(GetObjectItemNum('HeatExchanger:AirToAir:SensibleAndLatent',StandAloneERV(StandAloneERVNum)%HeatExchangerName) <= 0)THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' heat exchanger '&
                           //'type HeatExchanger:AirToAir:SensibleAndLatent not found = ' &
                           //TRIM(StandAloneERV(StandAloneERVNum)%HeatExchangerName))
      ErrorsFound = .TRUE.
    END IF
  ! Verify supply air fan name in Stand Alone ERV object matches name of valid fan object
    IF(GetObjectItemNum('Fan:OnOff',StandAloneERV(StandAloneERVNum)%SupplyAirFanName) <= 0)THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' supply fan '&
                           //'type Fan:OnOff not found = '//TRIM(StandAloneERV(StandAloneERVNum)%SupplyAirFanName))
      ErrorsFound = .TRUE.
    END IF

  ! Verify exhaust air fan name in Stand Alone ERV object matches name of valid fan object
    IF(GetObjectItemNum('Fan:OnOff',StandAloneERV(StandAloneERVNum)%ExhaustAirFanName) <= 0)THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' exhaust fan '&
                           //'type Fan:OnOff not found = '//TRIM(StandAloneERV(StandAloneERVNum)%ExhaustAirFanName))
      ErrorsFound = .TRUE.
    END IF

  END DO

  OutAirNum = GetNumObjectsFound('Controller:OutdoorAir')
  CurrentModuleObject='ZoneHVAC:EnergyRecoveryVentilator:Controller'
  NumERVCtrlrs = GetNumObjectsFound(CurrentModuleObject)

  DO ERVControllerNum=1,NumERVCtrlrs
    CALL GetObjectItem(CurrentModuleObject,ERVControllerNum,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       AlphaBlank=lAlphaBlanks,NumBlank=lNumericBlanks,  &
                       AlphaFieldnames=cAlphaFields,NumericFieldNames=cNumericFields)

    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL CheckOAControllerName(Alphas(1),OutAirNum,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.TRUE.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF

    OutAirNum=OutAirNum+1
    CALL SetOAControllerData(OutAirNum,ErrorsFound,Name=Alphas(1))
    CALL SetOAControllerData(OutAirNum,ErrorsFound,ControllerType=TRIM(CurrentModuleObject))
    CALL SetOAControllerData(OutAirNum,ErrorsFound,ControllerType_Num=ControllerStandAloneERV)
    WhichERV=FindItemInList(Alphas(1),StandAloneERV%ControllerName,NumStandAloneERVs)
    IF (WhichERV /= 0) THEN
      AirFlowRate=StandAloneERV(WhichERV)%SupplyAirVolFlow
      StandAloneERV(WhichERV)%ControllerIndex = OutAirNum
    ELSE
      CALL ShowSevereError('GetERVController: Could not find ZoneHVAC:EnergyRecoveryVentilator with '// &
                            TRIM(cAlphaFields(1))//' = "'//TRIM(Alphas(1))//'"')
      ErrorsFound=.TRUE.
      AirFlowRate=-1000.d0
    ENDIF
    CALL SetOAControllerData(OutAirNum,ErrorsFound,MinOA=AirFlowRate)
    CALL SetOAControllerData(OutAirNum,ErrorsFound,MaxOA=AirFlowRate)
!    OAController(OutAirNum)%TempLim = Numbers(1)
    IF (lNumericBlanks(1)) THEN
      CALL SetOAControllerData(OutAirNum,ErrorsFound,TempLim=BlankNumeric)
    ELSE
      CALL SetOAControllerData(OutAirNum,ErrorsFound,TempLim=Numbers(1))
    END IF
!    OAController(OutAirNum)%TempLowLim = Numbers(2)
    IF (lNumericBlanks(2)) THEN
      CALL SetOAControllerData(OutAirNum,ErrorsFound,TempLowLim=BlankNumeric)
    ELSE
      CALL SetOAControllerData(OutAirNum,ErrorsFound,TempLowLim=Numbers(2))
    END IF
!    OAController(OutAirNum)%EnthLim = Numbers(3)
    IF (lNumericBlanks(3)) THEN
      CALL SetOAControllerData(OutAirNum,ErrorsFound,EnthLim=BlankNumeric)
    ELSE
      CALL SetOAControllerData(OutAirNum,ErrorsFound,EnthLim=Numbers(3))
    END IF
!    OAController(OutAirNum)%DPTempLim = Numbers(4)
    IF (lNumericBlanks(4)) THEN
      CALL SetOAControllerData(OutAirNum,ErrorsFound,DPTempLim=BlankNumeric)
    ELSE
      CALL SetOAControllerData(OutAirNum,ErrorsFound,DPTempLim=Numbers(4))
    END IF

    IF (WhichERV /= 0) THEN
      NodeNumber=StandAloneERV(WhichERV)%SupplyAirInletNode
    ELSE
      NodeNumber=0
    ENDIF
    CALL SetOAControllerData(OutAirNum,ErrorsFound,OANode=NodeNumber)
      ! set the inlet node to also equal the OA node because this is a special controller for economizing stand alone ERV
      ! with the assumption that equipment is bypassed....(moved from module MixedAir)
    CALL SetOAControllerData(OutAirNum,ErrorsFound,InletNode=NodeNumber)

    IF (WhichERV /= 0) THEN
      NodeNumber=StandAloneERV(WhichERV)%ExhaustAirInletNode
    ELSE
      NodeNumber=0
    ENDIF
    CALL SetOAControllerData(OutAirNum,ErrorsFound,RetNode=NodeNumber)

    IF(.NOT. lAlphaBlanks(2))THEN
      CALL SetOAControllerData(OutAirNum,ErrorsFound,EnthalpyCurvePtr=GetCurveIndex(Alphas(2)))
      IF (GetCurveIndex(Alphas(2)) .EQ. 0) THEN
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
        CALL ShowContinueError('...'//TRIM(cAlphaFields(2))//' not found:'//TRIM(Alphas(2)))
        ErrorsFound = .TRUE.
      ELSE
        ! Verify Curve Object, only legal types are Quadratic and Cubic
        SELECT CASE(GetCurveType(GetCurveIndex(Alphas(2))))

        CASE('QUADRATIC')

        CASE('CUBIC')

        CASE DEFAULT
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
          CALL ShowContinueError('...illegal '//TRIM(cAlphaFields(2))//' type for this object = '// &
               TRIM(GetCurveType(GetCurveIndex(Alphas(2)))))
          ErrorsFound=.true.
        END SELECT
      END IF
    END IF

   ! Changed by AMIT for new implementation of the controller:outside air
    IF (Alphas(3).EQ.'EXHAUSTAIRTEMPERATURELIMIT' .AND. Alphas(4).EQ.'EXHAUSTAIRENTHALPYLIMIT') THEN
       CALL SetOAControllerData(OutAirNum, Errorsfound,Econotype = 'DIFFERENTIALDRYBULBANDENTHALPY')
    ELSEIF (Alphas(3).EQ.'EXHAUSTAIRTEMPERATURELIMIT'.AND. Alphas(4).EQ.'NOEXHAUSTAIRENTHALPYLIMIT') THEN
      CALL SetOAControllerData(OutAirNum, Errorsfound,Econotype = 'DIFFERENTIALDRYBULB')
    ELSEIF (Alphas(3).EQ.'NOEXHAUSTAIRTEMPERATURELIMIT'.AND. Alphas(4).EQ.'EXHAUSTAIRENTHALPYLIMIT')  THEN
      CALL SetOAControllerData(OutAirNum, Errorsfound,Econotype = 'DIFFERENTIALENTHALPY')
    ELSEIF (Alphas(3).EQ.'NOEXHAUSTAIRTEMPERATURELIMIT'.AND. Alphas(4).EQ.'NOEXHAUSTAIRENTHALPYLIMIT') THEN
      IF ((.NOT.lNumericBlanks(1)) .OR. (.NOT.lNumericBlanks(3)) .OR. (.NOT.lNumericBlanks(4)) .OR. (.NOT.lAlphaBlanks(2)) ) THEN
     ! This means that any of the FIXED DRY BULB, FIXED ENTHALPY, FIXED DEW POINT AND DRY BULB OR
     ! ELECTRONIC ENTHALPY ECONOMIZER STRATEGY is present
        CALL SetOAControllerData(OutAirNum, Errorsfound,Econotype = 'ECONOMIZER STRATEGY PRESENT')
      END IF
    ELSE IF((.NOT.lAlphaBlanks(3)).AND.(.NOT.lAlphaBlanks(4))) THEN
      IF ((lNumericBlanks(1)) .AND. (lNumericBlanks(3)) .AND. (lNumericBlanks(4)) .AND. lAlphaBlanks(2) ) THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
        CALL ShowContinueError('... Invalid '//TRIM(cAlphaFields(3))//TRIM(cAlphaFields(4))//' = '//  &
           TRIM(Alphas(3))//TRIM(Alphas(4)))
        CALL ShowContinueError('... Assumed NO EXHAUST AIR TEMP LIMIT and NO EXHAUST AIR ENTHALPY LIMIT.')
        CALL SetOAControllerData(OutAirNum, Errorsfound,Econotype = 'NOECONOMIZER')
      ELSE
     ! This means that any of the FIXED DRY BULB, FIXED ENTHALPY, FIXED DEW POINT AND DRY BULB OR
     ! ELECTRONIC ENTHALPY ECONOMIZER STRATEGY is present
        CALL SetOAControllerData(OutAirNum, Errorsfound,Econotype = 'ECONOMIZER STRATEGY PRESENT')
      END IF
    ELSE IF ((lAlphaBlanks(3)).AND.(.NOT.lAlphaBlanks(4))) THEN
      IF ((lNumericBlanks(1)) .AND. (lNumericBlanks(3)) .AND. (lNumericBlanks(4)) .AND. lAlphaBlanks(2) ) THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
        CALL ShowContinueError('... Invalid '//TRIM(cAlphaFields(4))//' = '//TRIM(Alphas(4)))
        CALL ShowContinueError('... Assumed  NO EXHAUST AIR ENTHALPY LIMIT.')
        CALL SetOAControllerData(OutAirNum, Errorsfound,Econotype = 'NOECONOMIZER')
      ELSE
    ! This means that any of the FIXED DRY BULB, FIXED ENTHALPY, FIXED DEW POINT AND DRY BULB OR
    ! ELECTRONIC ENTHALPY ECONOMIZER STRATEGY is present
        CALL SetOAControllerData(OutAirNum, Errorsfound,Econotype = 'ECONOMIZER STRATEGY PRESENT')
      END IF
    ELSE IF ((.NOT.lAlphaBlanks(3)).AND.(lAlphaBlanks(4))) THEN
      IF ((lNumericBlanks(1)) .AND. (lNumericBlanks(3)) .AND. (lNumericBlanks(4)) .AND. lAlphaBlanks(2) ) THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
        CALL ShowContinueError('... Invalid '//TRIM(cAlphaFields(3))//' = '//TRIM(Alphas(3)))
        CALL ShowContinueError('... Assumed NO EXHAUST AIR TEMP LIMIT ')
        CALL SetOAControllerData(OutAirNum, Errorsfound,Econotype = 'NOECONOMIZER')
      ELSE
      ! This means that any of the FIXED DRY BULB, FIXED ENTHALPY, FIXED DEW POINT AND DRY BULB OR
      ! ELECTRONIC ENTHALPY ECONOMIZER STRATEGY is present
        CALL SetOAControllerData(OutAirNum, Errorsfound,Econotype = 'ECONOMIZER STRATEGY PRESENT')
      END IF
    ELSE !NO Economizer
      CALL SetOAControllerData(OutAirNum, Errorsfound,Econotype = 'NOECONOMIZER')
    END IF


    CALL SetOAControllerData(OutAirNum,ErrorsFound,FixedMin=.FALSE.)
    CALL SetOAControllerData(OutAirNum, Errorsfound,Bypasstype = 'MINIMUMFLOWWITHBYPASS')

!   Initialize to zero in case high humidity control is NOT used and a schedule is entered
    HighRHOARatio = 0.0d0
!   READ Modify Air Flow Data
!   High humidity control option is YES, read in additional data
    IF(SameString(Alphas(6),'Yes'))THEN

      HStatZoneNum = FindItemInList(Alphas(7),Zone%Name,NumOfZones)
      CALL SetOAControllerData(OutAirNum,ErrorsFound,HumidistatZoneNum=HStatZoneNum)

      ! Get the node number for the zone with the humidistat
      IF (HStatZoneNum > 0) THEN
        ZoneNodeFound = .FALSE.
        HStatFound    = .FALSE.
        DO ControlledZoneNum = 1,NumOfZones
          IF (ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum /= HStatZoneNum) CYCLE
!         Find the controlled zone number for the specified humidistat location
          CALL SetOAControllerData(OutAirNum,ErrorsFound,NodeNumofHumidistatZone=ZoneEquipConfig(ControlledZoneNum)%ZoneNode)
          ZoneNodeFound = .TRUE.
          EXIT  ! found zone node
        END DO
        IF (.not. ZoneNodeFound) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
          CALL ShowContinueError('... Did not find Air Node (Zone with Humidistat)')
          CALL ShowContinueError('... Specified '//TRIM(cAlphaFields(7))//' = '//TRIM(Alphas(7)))
          CALL ShowContinueError('... A ZoneHVAC:EquipmentConnections object must be specified for this zone.')
          ErrorsFound=.TRUE.
        ELSE
          DO NumHstatZone = 1, NumHumidityControlZones
            IF(HumidityControlZone(NumHstatZone)%ActualZoneNum .NE. HStatZoneNum)CYCLE
            HStatFound=.TRUE.
            EXIT
          END DO
          IF (.not. HStatFound) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
            CALL ShowContinueError('... Did not find zone humidistat')
            CALL ShowContinueError('... A ZoneControl:Humidistat object must be specified for this zone.')
            ErrorsFound=.TRUE.
          END IF
        ENDIF
      ELSE
        CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
        CALL ShowContinueError('... Did not find Air Node (Zone with Humidistat)')
        CALL ShowContinueError('... A ZoneHVAC:EquipmentConnections object must be specified for this zone.')
        ErrorsFound=.TRUE.
      END IF

      IF(SameString(Alphas(8), 'Yes'))THEN
        CALL SetOAControllerData(OutAirNum,ErrorsFound,ModifyDuringHighOAMoisture = .FALSE.)
      ELSE
        CALL SetOAControllerData(OutAirNum,ErrorsFound,ModifyDuringHighOAMoisture = .TRUE.)
      END IF

    ELSE IF (.NOT. SameString(Alphas(6),'No') .AND. NumAlphas .GT. 4 .AND. (.NOT. lAlphaBlanks(5)))THEN
      CALL ShowWarningError(TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
      CALL ShowContinueError('... Invalid '//TRIM(cAlphaFields(6))//' = '//TRIM(Alphas(6)))
      CALL ShowContinueError('... '//TRIM(cAlphaFields(6))//' is assumed to be "No" and the simulation continues.')
    END IF ! IF(SameString(Alphas(6),'Yes'))THEN

    IF(Numbers(5) .LE. 0.0d0 .AND. NumNumbers .GT. 4)THEN

      CALL ShowWarningError(TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
      CALL ShowContinueError('... '//TRIM(cNumericFields(5))//' must be greater than 0.')
      CALL ShowContinueError('... '//TRIM(cNumericFields(5))//' is reset to 1 and the simulation continues.')

      HighRHOARatio = 1.0d0

    ELSE IF(NumNumbers .GT. 4)THEN

      HighRHOARatio = Numbers(5)

    ELSE

      HighRHOARatio = 1.0d0

    END IF

    CALL SetOAControllerData(OutAirNum,ErrorsFound,HighRHOAFlowRatio = HighRHOARatio)
    IF (WhichERV /= 0) THEN
      StandAloneERV(WhichERV)%HighRHOAFlowRatio=HighRHOARatio
    END IF

!   Check for a time of day outside air schedule
    CALL SetOAControllerData(OutAirNum,ErrorsFound,EconomizerOASchedPtr=GetScheduleIndex(Alphas(5)))

    IF(WhichERV /= 0)THEN
      StandAloneERV(WhichERV)%EconomizerOASchedPtr = GetScheduleIndex(Alphas(5))

      ErrFlag=.false.
    ! Compare the ERV SA fan flow rates to modified air flow rate.
      CALL GetFanType(StandAloneERV(WhichERV)%SupplyAirFanName,SAFanTypeNum,ErrFlag,  &
              CurrentModuleObject,StandAloneERV(WhichERV)%Name)
      IF (.not. ErrFlag) THEN
        SAFanVolFlowRate =    &
             GetFanDesignVolumeFlowRate(cFanTypes(SAFanTypeNum),StandAloneERV(WhichERV)%SupplyAirFanName,ErrFlag)
        IF (HighRHOARatio .GT. 1.0d0 .and. StandAloneERV(WhichERV)%SupplyAirVolFlow/=AutoSize .AND. SAFanVolFlowRate/=AutoSize) THEN
          IF (StandAloneERV(WhichERV)%SupplyAirVolFlow*HighRHOARatio > SAFanVolFlowRate) THEN
            CALL ShowWarningError(TRIM(CurrentModuleObject)//' "'//TRIM(Alphas(1))//'"')
            CALL ShowContinueError('... A '//TRIM(cNumericFields(5))//' was entered as ' &
                                   //TRIM(RoundSigDigits(HighRHOARatio,4)))
            CALL ShowContinueError('... This flow ratio results in a Supply Air Volume Flow Rate through the ERV which is '// &
                'greater than the Max Volume specified in the supply air fan object.')
            CALL ShowContinueError('... Associated fan object = '//TRIM(cFanTypes(SAFanTypeNum))//' "'// &
                 TRIM(StandAloneERV(WhichERV)%SupplyAirFanName)//'"')
            CALL ShowContinueError('... Modified value                   = ' &
                  //TRIM(RoundSigDigits(StandAloneERV(WhichERV)%SupplyAirVolFlow*HighRHOARatio,2)))
            CALL ShowContinueError(' ... Supply Fan Max Volume Flow Rate = '//TRIM(RoundSigDigits(SAFanVolFlowRate,2)))
            CALL ShowContinueError('... The ERV supply air fan will limit the air flow through the ERV'// &
                                   ' and the simulation continues.')
          ENDIF
        ENDIF
      ELSE
        ErrorsFound=.true.
      ENDIF


      ErrFlag=.false.
    ! Compare the ERV EA fan flow rates to modified air flow rate.
      CALL GetFanType(StandAloneERV(WhichERV)%ExhaustAirFanName,EAFanTypeNum,ErrFlag,  &
              CurrentModuleObject,StandAloneERV(WhichERV)%Name)
      IF (.not. ErrFlag) THEN
        EAFanVolFlowRate =    &
             GetFanDesignVolumeFlowRate(cFanTypes(EAFanTypeNum),StandAloneERV(WhichERV)%ExhaustAirFanName,ErrFlag)
        IF (HighRHOARatio .GT. 1.0d0 .and. StandAloneERV(WhichERV)%ExhaustAirVolFlow /= AutoSize .AND. &
            EAFanVolFlowRate /= AutoSize) THEN
          IF (StandAloneERV(WhichERV)%ExhaustAirVolFlow*HighRHOARatio > EAFanVolFlowRate) THEN
            CALL ShowWarningError('ZoneHVAC:EnergyRecoveryVentilator:Controller "'//TRIM(Alphas(1))//'"')
            CALL ShowContinueError('... A '//TRIM(cNumericFields(5))//' was entered as ' &
                                   //TRIM(RoundSigDigits(HighRHOARatio,4)))
            CALL ShowContinueError('... This flow ratio results in an Exhaust Air Volume Flow Rate through the ERV which is '// &
                'greater than the Max Volume specified in the exhaust air fan object.')
            CALL ShowContinueError('... Associated fan object = '//TRIM(cFanTypes(EAFanTypeNum))//' "'// &
                 TRIM(StandAloneERV(WhichERV)%ExhaustAirFanName)//'"')
            CALL ShowContinueError('... Modified value                    = ' &
                  //TRIM(RoundSigDigits(StandAloneERV(WhichERV)%ExhaustAirVolFlow*HighRHOARatio,2)))
            CALL ShowContinueError(' ... Exhaust Fan Max Volume Flow Rate = '//TRIM(RoundSigDigits(EAFanVolFlowRate,2)))
            CALL ShowContinueError('... The ERV exhaust air fan will limit the air flow through the ERV'// &
                                   ' and the simulation continues.')
          ENDIF
        ENDIF
      ELSE
        ErrorsFound=.true.
      ENDIF
    END IF ! IF(WhichERV /= 0)THEN

  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in getting ZoneHVAC:EnergyRecoveryVentilator input.')
  END IF

  ! Setup report variables for the stand alone ERVs
  DO StandAloneERVIndex = 1,NumStandAloneERVs
    CALL SetupOutputVariable('Zone Ventilator Sensible Cooling Rate [W]',StandAloneERV(StandAloneERVIndex)%SensCoolingRate,&
                              'System','Average',StandAloneERV(StandAloneERVIndex)%Name)
    CALL SetupOutputVariable('Zone Ventilator Sensible Cooling Energy [J]',&
                              StandAloneERV(StandAloneERVIndex)%SensCoolingEnergy,'System','Sum',&
                              StandAloneERV(StandAloneERVIndex)%Name)
    CALL SetupOutputVariable('Zone Ventilator Latent Cooling Rate [W]',StandAloneERV(StandAloneERVIndex)%LatCoolingRate,&
                              'System','Average',StandAloneERV(StandAloneERVIndex)%Name)
    CALL SetupOutputVariable('Zone Ventilator Latent Cooling Energy [J]',StandAloneERV(StandAloneERVIndex)%LatCoolingEnergy,&
                              'System','Sum',StandAloneERV(StandAloneERVIndex)%Name)
    CALL SetupOutputVariable('Zone Ventilator Total Cooling Rate [W]',StandAloneERV(StandAloneERVIndex)%TotCoolingRate,&
                              'System','Average',StandAloneERV(StandAloneERVIndex)%Name)
    CALL SetupOutputVariable('Zone Ventilator Total Cooling Energy [J]',StandAloneERV(StandAloneERVIndex)%TotCoolingEnergy,&
                              'System','Sum',StandAloneERV(StandAloneERVIndex)%Name)

    CALL SetupOutputVariable('Zone Ventilator Sensible Heating Rate [W]',StandAloneERV(StandAloneERVIndex)%SensHeatingRate,&
                              'System','Average',StandAloneERV(StandAloneERVIndex)%Name)
    CALL SetupOutputVariable('Zone Ventilator Sensible Heating Energy [J]',&
                              StandAloneERV(StandAloneERVIndex)%SensHeatingEnergy,'System','Sum',&
                              StandAloneERV(StandAloneERVIndex)%Name)
    CALL SetupOutputVariable('Zone Ventilator Latent Heating Rate [W]',StandAloneERV(StandAloneERVIndex)%LatHeatingRate,&
                              'System','Average',StandAloneERV(StandAloneERVIndex)%Name)
    CALL SetupOutputVariable('Zone Ventilator Latent Heating Energy [J]',StandAloneERV(StandAloneERVIndex)%LatHeatingEnergy,&
                              'System','Sum',StandAloneERV(StandAloneERVIndex)%Name)
    CALL SetupOutputVariable('Zone Ventilator Total Heating Rate [W]',StandAloneERV(StandAloneERVIndex)%TotHeatingRate,&
                              'System','Average',StandAloneERV(StandAloneERVIndex)%Name)
    CALL SetupOutputVariable('Zone Ventilator Total Heating Energy [J]',StandAloneERV(StandAloneERVIndex)%TotHeatingEnergy,&
                              'System','Sum',StandAloneERV(StandAloneERVIndex)%Name)


    CALL SetupOutputVariable('Zone Ventilator Electric Power [W]',StandAloneERV(StandAloneERVIndex)%ElecUseRate,&
                              'System','Average',StandAloneERV(StandAloneERVIndex)%Name)
    CALL SetupOutputVariable('Zone Ventilator Electric Energy [J]',StandAloneERV(StandAloneERVIndex)%ElecUseEnergy,&
                             'System','Sum',StandAloneERV(StandAloneERVIndex)%Name)
    CALL SetupOutputVariable('Zone Ventilator Supply Fan Availability Status []',StandAloneERV(StandAloneERVIndex)%AvailStatus, &
                             'System','Average',StandAloneERV(StandAloneERVIndex)%Name)
  END DO

  DEALLOCATE(Alphas)
  DEALLOCATE(Numbers)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(lNumericBlanks)
  DEALLOCATE(lAlphaBlanks)


  RETURN

END SUBROUTINE GetStandAloneERV

SUBROUTINE InitStandAloneERV(StandAloneERVNum,ZoneNum, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   June 2003
          !       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Stand Alone ERV unit information.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
!  USE Psychrometrics,     ONLY: PsyRhoAirFnPbTdbW
  USE DataZoneEquipment,  ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList,ERVStandAlone_Num
  USE MixedAir,           ONLY: SimOAController
  USE DataAirLoop,        ONLY: OAControllerInfo

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: StandAloneERVNum ! number of the current Stand Alone ERV unit being simulated
  INTEGER, INTENT (IN) :: ZoneNum             ! number of zone being served unused1208
  LOGICAL, INTENT (IN) :: FirstHVACIteration ! TRUE if first HVAC iteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: SupInNode      ! supply air inlet node number
  INTEGER             :: ExhInNode      ! exhaust air inlet node number
  INTEGER             :: SupInletNode   ! supply air inlet node number for Stand Alone ERV 'StandAloneERVNum'
  REAL(r64)           :: RhoAir      ! air density at SupInNode, standard conditions (dry air @ 20C,actual elevation pressure)
  LOGICAL,SAVE        :: MyOneTimeFlag = .TRUE.
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MySizeFlag
  LOGICAL,SAVE        :: ZoneEquipmentListChecked = .FALSE.  ! True after the Zone Equipment List has been checked for items
  INTEGER             :: Loop              ! loop counter

! Do the one time initializations
  IF (MyOneTimeFlag) THEN

    ALLOCATE(MyEnvrnFlag(NumStandAloneERVs))
    ALLOCATE(MySizeFlag(NumStandAloneERVs))
    MyEnvrnFlag = .TRUE.
    MySizeFlag  = .TRUE.
    MyOneTimeFlag = .FALSE.

  ENDIF

  IF (ALLOCATED(ZoneComp)) THEN
    ZoneComp(ERVStandAlone_Num)%ZoneCompAvailMgrs(StandAloneERVNum)%ZoneNum = ZoneNum
    StandAloneERV(StandAloneERVNum)%AvailStatus = ZoneComp(ERVStandAlone_Num)%ZoneCompAvailMgrs(StandAloneERVNum)%AvailStatus
  ENDIF

  ! need to check all units to see if they are on Zone Equipment List or issue warning
  IF (.not. ZoneEquipmentListChecked .and. ZoneEquipInputsFilled) THEN
    ZoneEquipmentListChecked=.TRUE.
    DO Loop=1,NumStandAloneERVs
      IF (CheckZoneEquipmentList(StandAloneERV(Loop)%UnitType,StandAloneERV(Loop)%Name)) CYCLE
      CALL ShowSevereError('InitStandAloneERV: Unit=['//TRIM(StandAloneERV(Loop)%UnitType)//','//  &
         TRIM(StandAloneERV(Loop)%Name)//  &
           '] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.')
    ENDDO
  ENDIF

  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(StandAloneERVNum)) THEN
    CALL SizeStandAloneERV(StandAloneERVNum)
    MySizeFlag(StandAloneERVNum) = .FALSE.
  END IF

! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(StandAloneERVNum)) THEN
    SupInNode = StandAloneERV(StandAloneERVNum)%SupplyAirInletNode
    ExhInNode = StandAloneERV(StandAloneERVNum)%ExhaustAirInletNode
    RhoAir = StdRhoAir
    ! set the mass flow rates from the input volume flow rates
    StandAloneERV(StandAloneERVNum)%MaxSupAirMassFlow = StdRhoAir*StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow
    StandAloneERV(StandAloneERVNum)%MaxExhAirMassFlow = StdRhoAir*StandAloneERV(StandAloneERVNum)%ExhaustAirVolFlow
    StandAloneERV(StandAloneERVNum)%DesignSAFanMassFlowRate = StdRhoAir*StandAloneERV(StandAloneERVNum)%DesignSAFanVolFlowRate
    StandAloneERV(StandAloneERVNum)%DesignEAFanMassFlowRate = StdRhoAir*StandAloneERV(StandAloneERVNum)%DesignEAFanVolFlowRate
    ! set the node max and min mass flow rates
    Node(SupInNode)%MassFlowRateMax = StandAloneERV(StandAloneERVNum)%MaxSupAirMassFlow
    Node(SupInNode)%MassFlowRateMin = 0.0d0
    Node(ExhInNode)%MassFlowRateMax = StandAloneERV(StandAloneERVNum)%MaxExhAirMassFlow
    Node(ExhInNode)%MassFlowRateMin = 0.0d0
    MyEnvrnFlag(StandAloneERVNum) = .FALSE.
!   Initialize OA Controller on BeginEnvrnFlag
    IF(StandAloneERV(StandAloneERVNum)%ControllerNameDefined)THEN
      CALL SimOAController(StandAloneERV(StandAloneERVNum)%ControllerName,StandAloneERV(StandAloneERVNum)%ControllerIndex,  &
                           FirstHVACIteration,0)
    END IF
  ENDIF ! end one time inits

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(StandAloneERVNum) = .TRUE.
  ENDIF

! These initializations are done every iteration
  StandAloneERV(StandAloneERVNum)%ElecUseRate = 0.0d0
  StandAloneERV(StandAloneERVNum)%SensCoolingRate = 0.0d0
  StandAloneERV(StandAloneERVNum)%LatCoolingRate = 0.0d0
  StandAloneERV(StandAloneERVNum)%TotCoolingRate = 0.0d0
  StandAloneERV(StandAloneERVNum)%SensHeatingRate = 0.0d0
  StandAloneERV(StandAloneERVNum)%LatHeatingRate = 0.0d0
  StandAloneERV(StandAloneERVNum)%TotHeatingRate = 0.0d0
  SupInletNode = StandAloneERV(StandAloneERVNum)%SupplyAirInletNode
  ExhInNode = StandAloneERV(StandAloneERVNum)%ExhaustAirInletNode

! Set the inlet node mass flow rate
  IF (GetCurrentScheduleValue(StandAloneERV(StandAloneERVNum)%SchedPtr) .GT. 0.0d0) THEN

!   IF optional ControllerName is defined SimOAController ONLY to set economizer and Modifyairflow flags
    IF(StandAloneERV(StandAloneERVNum)%ControllerNameDefined)THEN
!     Initialize a flow rate for controller
      Node(SupInletNode)%MassFlowRate = StandAloneERV(StandAloneERVNum)%MaxSupAirMassFlow
      CALL SimOAController(StandAloneERV(StandAloneERVNum)%ControllerName,StandAloneERV(StandAloneERVNum)%ControllerIndex,  &
                           FirstHVACIteration,0)
    END IF

    IF(GetCurrentScheduleValue(StandAloneERV(StandAloneERVNum)%SupplyAirFanSchPtr) .GT. 0 .OR. &
       ZoneCompTurnFansOn .AND. .NOT. ZoneCompTurnFansOff)THEN
      IF(StandAloneERV(StandAloneERVNum)%ControllerNameDefined)THEN
        IF(OAControllerInfo(StandAloneERV(StandAloneERVNum)%ControllerIndex)%HighHumCtrlActive)THEN
          Node(SupInletNode)%MassFlowRate = MIN(StandAloneERV(StandAloneERVNum)%DesignSAFanMassFlowRate, &
                                                StandAloneERV(StandAloneERVNum)%MaxSupAirMassFlow * &
                                                StandAloneERV(StandAloneERVNum)%HighRHOAFlowRatio)
        ELSE
          Node(SupInletNode)%MassFlowRate = MIN(StandAloneERV(StandAloneERVNum)%DesignSAFanMassFlowRate, &
                                                StandAloneERV(StandAloneERVNum)%MaxSupAirMassFlow)
        END IF
      ELSE
        Node(SupInletNode)%MassFlowRate = MIN(StandAloneERV(StandAloneERVNum)%DesignSAFanMassFlowRate, &
                                              StandAloneERV(StandAloneERVNum)%MaxSupAirMassFlow)
      END IF
    ELSE
      Node(SupInletNode)%MassFlowRate = 0.0d0
    END IF
    Node(SupInletNode)%MassFlowRateMaxAvail = Node(SupInletNode)%MassFlowRate
    Node(SupInletNode)%MassFlowRateMinAvail = Node(SupInletNode)%MassFlowRate

    IF(GetCurrentScheduleValue(StandAloneERV(StandAloneERVNum)%ExhaustAirFanSchPtr) .GT. 0)THEN
      IF(StandAloneERV(StandAloneERVNum)%ControllerNameDefined)THEN
        IF(OAControllerInfo(StandAloneERV(StandAloneERVNum)%ControllerIndex)%HighHumCtrlActive)THEN
          Node(ExhInNode)%MassFlowRate = MIN(StandAloneERV(StandAloneERVNum)%DesignEAFanMassFlowRate, &
                                             StandAloneERV(StandAloneERVNum)%MaxExhAirMassFlow * &
                                             StandAloneERV(StandAloneERVNum)%HighRHOAFlowRatio)
        ELSE
          Node(ExhInNode)%MassFlowRate = MIN(StandAloneERV(StandAloneERVNum)%DesignEAFanMassFlowRate, &
                                             StandAloneERV(StandAloneERVNum)%MaxExhAirMassFlow)
        END IF
      ELSE
        Node(ExhInNode)%MassFlowRate = MIN(StandAloneERV(StandAloneERVNum)%DesignEAFanMassFlowRate, &
                                           StandAloneERV(StandAloneERVNum)%MaxExhAirMassFlow)
      END IF
    ELSE
      Node(ExhInNode)%MassFlowRate = 0.0d0
    END IF
    Node(ExhInNode)%MassFlowRateMaxAvail = Node(ExhInNode)%MassFlowRate
    Node(ExhInNode)%MassFlowRateMinAvail = Node(ExhInNode)%MassFlowRate
  ELSE
    Node(SupInletNode)%MassFlowRate = 0.0d0
    Node(SupInletNode)%MassFlowRateMaxAvail = 0.0d0
    Node(SupInletNode)%MassFlowRateMinAvail = 0.0d0
    Node(ExhInNode)%MassFlowRate = 0.0d0
    Node(ExhInNode)%MassFlowRateMaxAvail = 0.0d0
    Node(ExhInNode)%MassFlowRateMinAvail = 0.0d0
  ENDIF

  RETURN
END SUBROUTINE InitStandAloneERV

SUBROUTINE SizeStandAloneERV(StandAloneERVNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   October 2007
          !       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Stand Alone ERV Components for which flow rates have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone or system sizing arrays.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing,        ONLY: AutoSize, CurZoneEqNum, FinalZoneSizing, AutoVsHardSizingThreshold
  USE DataHeatBalance,   ONLY: Zone, People, TotPeople
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE InputProcessor,    ONLY: SameString
  USE ScheduleManager,   ONLY: GetScheduleMaxValue
  USE HeatRecovery,      ONLY: SetHeatExchangerData
  USE Fans,              ONLY: SetFanData
  USE MixedAir,          ONLY: SetOAControllerData
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE General,             ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: StandAloneERVNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                      :: ZoneNum           ! Index to zone object
  INTEGER                      :: ActualZoneNum     ! Actual zone node number
  CHARACTER(len=MaxNameLength) :: ZoneName          ! Name of zone
  INTEGER                      :: PeopleNum         ! Index to people object
  REAL(r64)                    :: NumberOfPeople    ! Maximum number of people in zone
  INTEGER                      :: PeopleSchPtr      ! Pointer to people schedule
  REAL(r64)                    :: MaxPeopleSch      ! maximum people schedule value
  REAL(r64)                    :: FloorArea         ! Floor area of zone (m2)
  LOGICAL                      :: ErrorsFound       ! Used for warning messages
  LOGICAL                      :: IsAutosize        ! Indicator to autosize
  REAL(r64)                    :: SupplyAirVolFlowDes        ! Autosized supply air flow for reporting
  REAL(r64)                    :: SupplyAirVolFlowUser       ! Hardsized supply air flow for reporting
  REAL(r64)                    :: DesignSAFanVolFlowRateDes  ! Autosized supply air fan flow for reporting
  REAL(r64)                    :: DesignSAFanVolFlowRateUser ! Hardsized supply air fan flow for reporting
  REAL(r64)                    :: ExhaustAirVolFlowDes       ! Autosized exhaust air flow for reporting
  REAL(r64)                    :: ExhaustAirVolFlowUser      ! Hardsized exhaust air flow for reporting
  REAL(r64)                    :: DesignEAFanVolFlowRateDes  ! Autosized exhaust fan flow for reporting
  REAL(r64)                    :: DesignEAFanVolFlowRateUser ! Hardsized exhaust fan flow for reporting

  IsAutosize = .FALSE.
  SupplyAirVolFlowDes = 0.0d0
  SupplyAirVolFlowUser = 0.0d0
  DesignSAFanVolFlowRateDes = 0.0d0
  DesignSAFanVolFlowRateUser = 0.0d0
  ExhaustAirVolFlowDes = 0.0d0
  ExhaustAirVolFlowUser = 0.0d0
  DesignEAFanVolFlowRateDes = 0.0d0
  DesignEAFanVolFlowRateUser = 0.0d0

  IF (StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF

  IF (CurZoneEqNum > 0) THEN

!      Sizing objects are not required for stand alone ERV
!      CALL CheckZoneSizing('ZoneHVAC:EnergyRecoveryVentilator',StandAloneERV(StandAloneERVNum)%Name)
    ZoneName = ZoneEquipConfig(CurZoneEqNum)%ZoneName
    ActualZoneNum = ZoneEquipConfig(CurZoneEqNum)%ActualZoneNum
    FloorArea = 0.0d0
    IF(SameString(ZoneName,Zone(CurZoneEqNum)%Name))THEN
      FloorArea = Zone(CurZoneEqNum)%FloorArea
    ELSE
      DO ZoneNum = 1, NumOfZones
        IF(.NOT. SameString(ZoneName,Zone(CurZoneEqNum)%Name))CYCLE
          FloorArea = Zone(ZoneNum)%FloorArea
        EXIT
      END DO
    END IF
    NumberOfPeople = 0.0d0
    MaxPeopleSch = 0.0d0
    DO PeopleNum = 1, TotPeople
      IF(ActualZoneNum .NE. People(PeopleNum)%ZonePtr)CYCLE
        NumberOfPeople = People(PeopleNum)%NumberOfPeople
        PeopleSchPtr = People(PeopleNum)%NumberOfPeoplePtr
        MaxPeopleSch = GetScheduleMaxValue(PeopleSchPtr)
      EXIT
    END DO
    SupplyAirVolFlowDes = &
                         FloorArea * StandAloneERV(StandAloneERVNum)%AirVolFlowPerFloorArea + &
                         MaxPeopleSch * NumberOfPeople * StandAloneERV(StandAloneERVNum)%AirVolFlowPerOccupant

    IF (SupplyAirVolFlowDes < SmallAirVolFlow) THEN
      SupplyAirVolFlowDes = 0.0d0
    END IF

    IF (IsAutosize) THEN
      StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow = SupplyAirVolFlowDes
      CALL ReportSizingOutput('ZoneHVAC:EnergyRecoveryVentilator',StandAloneERV(StandAloneERVNum)%Name, &
                              'Design Size Supply Air Flow Rate [m3/s]',SupplyAirVolFlowDes)

      CALL SetHeatExchangerData(StandAloneERV(StandAloneERVNum)%HeatExchangerIndex,ErrorsFound, &
                                StandAloneERV(StandAloneERVNum)%HeatExchangerName, &
                                SupplyAirVolFlowDes)

      CALL ReportSizingOutput(cHXTypes(StandAloneERV(StandAloneERVNum)%HeatExchangerTypeNum), &
                              StandAloneERV(StandAloneERVNum)%HeatExchangerName, &
                             'Design Size Supply Air Flow Rate [m3/s]',SupplyAirVolFlowDes)

      CALL SetFanData(StandAloneERV(StandAloneERVNum)%SupplyAirFanIndex, ErrorsFound, &
                      StandAloneERV(StandAloneERVNum)%SupplyAirFanName, &
                      StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow* &
                      StandAloneERV(StandAloneERVNum)%HighRHOAFlowRatio, &
                      0.0d0)

      StandAloneERV(StandAloneERVNum)%DesignSAFanVolFlowRate = SupplyAirVolFlowDes * &
                                                               StandAloneERV(StandAloneERVNum)%HighRHOAFlowRatio

      CALL ReportSizingOutput(TRIM(cFanTypes(StandAloneERV(StandAloneERVNum)%SupplyAirFanType_Num)), &
                              StandAloneERV(StandAloneERVNum)%SupplyAirFanName, &
                             'Design Size Maximum Supply Air Flow Rate [m3/s]', SupplyAirVolFlowDes * &
                              StandAloneERV(StandAloneERVNum)%HighRHOAFlowRatio)

!      ERV fan type must be Fan:OnOff, min flow rate is assumed 0. Do not report min flow sizing.
!      CALL ReportSizingOutput(TRIM(cFanTypes(StandAloneERV(StandAloneERVNum)%SupplyAirFanType_Num)), &
!                              StandAloneERV(StandAloneERVNum)%SupplyAirFanName, &
!                               'Min Flow Rate [m3/s]', 0.0)

      IF(StandAloneERV(StandAloneERVNum)%ControllerNameDefined)THEN
        CALL SetOAControllerData(StandAloneERV(StandAloneERVNum)%ControllerIndex,ErrorsFound, &
                               MaxOA=SupplyAirVolFlowDes * StandAloneERV(StandAloneERVNum)%HighRHOAFlowRatio)

!        MaxOA is not an input for CONTROLLER:STAND ALONE ERV, do not report sizing
!        CALL ReportSizingOutput('ZoneHVAC:EnergyRecoveryVentilator:Controller',StandAloneERV(StandAloneERVNum)%ControllerName,&
!                                'maximum outside air flow rate [m3/s]',StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow)

        CALL SetOAControllerData(StandAloneERV(StandAloneERVNum)%ControllerIndex,ErrorsFound, MinOA=SupplyAirVolFlowDes)

!        MinOA is not an input for CONTROLLER:STAND ALONE ERV, do not report sizing
!        CALL ReportSizingOutput('ZoneHVAC:EnergyRecoveryVentilator:Controller',StandAloneERV(StandAloneERVNum)%ControllerName,&
!                                'minimum outside air flow rate [m3/s]',StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow)
      END IF
    ELSE
      IF (StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow > 0.0d0 .AND. SupplyAirVolFlowDes > 0.0d0) THEN
        SupplyAirVolFlowUser = StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow
        IF (StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow > 0.0d0) THEN
          CALL ReportSizingOutput('ZoneHVAC:EnergyRecoveryVentilator',StandAloneERV(StandAloneERVNum)%Name, &
                              'Design Size Supply Air Flow Rate [m3/s]',SupplyAirVolFlowDes, &
                              'User-Specified Supply Air Flow Rate [m3/s]',SupplyAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(SupplyAirVolFlowDes - SupplyAirVolFlowUser)/SupplyAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeStandAloneERV: Potential issue with equipment sizing for ZoneHVAC:EnergyRecoveryVentilator =' &
                           //TRIM(StandAloneERV(StandAloneERVNum)%Name))
              CALL ShowContinueError('User-Specified Supply Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(SupplyAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Supply Air Flow Rate of ' // &
                                      TRIM(RoundSigDigits(SupplyAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF

          CALL ReportSizingOutput(cHXTypes(StandAloneERV(StandAloneERVNum)%HeatExchangerTypeNum), &
                              StandAloneERV(StandAloneERVNum)%HeatExchangerName, &
                             'Design Size Supply Air Flow Rate [m3/s]',SupplyAirVolFlowDes, &
                             'User-Specified Supply Air Flow Rate [m3/s]',SupplyAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(SupplyAirVolFlowDes - SupplyAirVolFlowUser)/SupplyAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeStandAloneERV: Potential issue with equipment sizing for ZoneHVAC:EnergyRecoveryVentilator ' &
                                 //   TRIM(cHXTypes(StandAloneERV(StandAloneERVNum)%HeatExchangerTypeNum))//' '//  &
                                  TRIM(StandAloneERV(StandAloneERVNum)%HeatExchangerName))
              CALL ShowContinueError('User-Specified Supply Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(SupplyAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Supply Air Flow Rate of ' // &
                                      TRIM(RoundSigDigits(SupplyAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF

          CALL ReportSizingOutput(TRIM(cFanTypes(StandAloneERV(StandAloneERVNum)%SupplyAirFanType_Num)), &
                              StandAloneERV(StandAloneERVNum)%SupplyAirFanName, &
                             'Design Size Maximum Supply Air Flow Rate [m3/s]', SupplyAirVolFlowDes * &
                              StandAloneERV(StandAloneERVNum)%HighRHOAFlowRatio, &
                             'User-Specified Maximum Supply Air Flow Rate [m3/s]', SupplyAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(SupplyAirVolFlowDes - SupplyAirVolFlowUser)/SupplyAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeStandAloneERV: Potential issue with equipment sizing for ZoneHVAC:EnergyRecoveryVentilator ' &
                                  // TRIM(cFanTypes(StandAloneERV(StandAloneERVNum)%SupplyAirFanType_Num))//' '// &
                                  TRIM(StandAloneERV(StandAloneERVNum)%SupplyAirFanName))
              CALL ShowContinueError('User-Specified Maximum Supply Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(SupplyAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Maximum Supply Air Flow Rate of ' // &
                                      TRIM(RoundSigDigits(SupplyAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (StandAloneERV(StandAloneERVNum)%DesignSAFanVolFlowRate == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  CALL SetFanData(StandAloneERV(StandAloneERVNum)%SupplyAirFanIndex, ErrorsFound, &
                    StandAloneERV(StandAloneERVNum)%SupplyAirFanName, &
                    StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow* &
                    StandAloneERV(StandAloneERVNum)%HighRHOAFlowRatio, &
                    0.0d0)
  DesignSAFanVolFlowRateDes = StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow * &
                              StandAloneERV(StandAloneERVNum)%HighRHOAFlowRatio
  IF (IsAutosize) THEN
    StandAloneERV(StandAloneERVNum)%DesignSAFanVolFlowRate = DesignSAFanVolFlowRateDes
    CALL ReportSizingOutput(cFanTypes(StandAloneERV(StandAloneERVNum)%SupplyAirFanType_Num), &
                              StandAloneERV(StandAloneERVNum)%SupplyAirFanName, &
                             'Design Size Maximum Supply Air Flow Rate [m3/s]', DesignSAFanVolFlowRateDes)
  ELSE
    IF (StandAloneERV(StandAloneERVNum)%DesignSAFanVolFlowRate > 0.0d0 .AND. DesignSAFanVolFlowRateDes > 0.0d0) THEN
      DesignSAFanVolFlowRateUser = StandAloneERV(StandAloneERVNum)%DesignSAFanVolFlowRate
      CALL ReportSizingOutput(cFanTypes(StandAloneERV(StandAloneERVNum)%SupplyAirFanType_Num), &
                              StandAloneERV(StandAloneERVNum)%SupplyAirFanName, &
                             'Design Size Maximum Supply Air Flow Rate [m3/s]', DesignSAFanVolFlowRateDes, &
                             'User-Specified Maximum Supply Air Flow Rate [m3/s]', DesignSAFanVolFlowRateUser)
      IF (DisplayExtraWarnings) THEN
        IF ((ABS(DesignSAFanVolFlowRateDes - DesignSAFanVolFlowRateUser)/DesignSAFanVolFlowRateUser) &
                                 > AutoVsHardSizingThreshold) THEN
          CALL ShowMessage('SizeStandAloneERV: Potential issue with equipment sizing for ZoneHVAC:EnergyRecoveryVentilator '//  &
                                  TRIM(cFanTypes(StandAloneERV(StandAloneERVNum)%SupplyAirFanType_Num))//' '// &
                                  TRIM(StandAloneERV(StandAloneERVNum)%SupplyAirFanName))
          CALL ShowContinueError('User-Specified Maximum Supply Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(DesignSAFanVolFlowRateUser,5))//' [m3/s]')
          CALL ShowContinueError('differs from Design Size Maximum Supply Air Flow Rate of ' // &
                                      TRIM(RoundSigDigits(DesignSAFanVolFlowRateDes,5))//' [m3/s]')
          CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
          CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
        END IF
      ENDIF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (StandAloneERV(StandAloneERVNum)%ExhaustAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF

  IF (CurZoneEqNum > 0) THEN

!      Sizing objects are not required for stand alone ERV
!      CALL CheckZoneSizing('ZoneHVAC:EnergyRecoveryVentilator',StandAloneERV(StandAloneERVNum)%Name)
    ZoneName = ZoneEquipConfig(CurZoneEqNum)%ZoneName
    ActualZoneNum = ZoneEquipConfig(CurZoneEqNum)%ActualZoneNum
    FloorArea = 0.0d0
    IF(SameString(ZoneName,Zone(CurZoneEqNum)%Name))THEN
      FloorArea = Zone(CurZoneEqNum)%FloorArea
    ELSE
      DO ZoneNum = 1, NumOfZones
        IF(.NOT. SameString(ZoneName,Zone(CurZoneEqNum)%Name))CYCLE
          FloorArea = Zone(ZoneNum)%FloorArea
        EXIT
      END DO
    END IF
    NumberOfPeople = 0.0d0
    MaxPeopleSch = 0.0d0
    DO PeopleNum = 1, TotPeople
      IF(ActualZoneNum .NE. People(PeopleNum)%ZonePtr)CYCLE
        NumberOfPeople = People(PeopleNum)%NumberOfPeople
        PeopleSchPtr = People(PeopleNum)%NumberOfPeoplePtr
        MaxPeopleSch = GetScheduleMaxValue(PeopleSchPtr)
      EXIT
    END DO
    ExhaustAirVolFlowDes = &
                      FloorArea * StandAloneERV(StandAloneERVNum)%AirVolFlowPerFloorArea + &
                      MaxPeopleSch * NumberOfPeople * StandAloneERV(StandAloneERVNum)%AirVolFlowPerOccupant

    IF (ExhaustAirVolFlowDes < SmallAirVolFlow) THEN
      ExhaustAirVolFlowDes = 0.0d0
    END IF

    IF (ExhaustAirVolFlowDes > StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow) THEN
      ExhaustAirVolFlowDes = StandAloneERV(StandAloneERVNum)%SupplyAirVolFlow
    END IF

    IF (IsAutosize) THEN
      StandAloneERV(StandAloneERVNum)%ExhaustAirVolFlow = ExhaustAirVolFlowDes
      StandAloneERV(StandAloneERVNum)%DesignEAFanVolFlowRate = ExhaustAirVolFlowDes * &
                                                               StandAloneERV(StandAloneERVNum)%HighRHOAFlowRatio

      CALL ReportSizingOutput('ZoneHVAC:EnergyRecoveryVentilator',StandAloneERV(StandAloneERVNum)%Name, &
                            'Design Size Exhaust Air Flow Rate [m3/s]',ExhaustAirVolFlowDes)

      CALL SetHeatExchangerData(StandAloneERV(StandAloneERVNum)%HeatExchangerIndex,ErrorsFound, &
                                StandAloneERV(StandAloneERVNum)%HeatExchangerName, &
                                SecondaryAirVolFlow=ExhaustAirVolFlowDes)

      CALL ReportSizingOutput(cHXTypes(StandAloneERV(StandAloneERVNum)%HeatExchangerTypeNum), &
                              StandAloneERV(StandAloneERVNum)%HeatExchangerName, &
                             'Design Size Exhaust Air Flow Rate [m3/s]',ExhaustAirVolFlowDes)

      CALL SetFanData(StandAloneERV(StandAloneERVNum)%ExhaustAirFanIndex, ErrorsFound, &
                      StandAloneERV(StandAloneERVNum)%ExhaustAirFanName, &
                      StandAloneERV(StandAloneERVNum)%ExhaustAirVolFlow* &
                      StandAloneERV(StandAloneERVNum)%HighRHOAFlowRatio, &
                      0.0d0)

      CALL ReportSizingOutput(TRIM(cFanTypes(StandAloneERV(StandAloneERVNum)%ExhaustAirFanType_Num)), &
                              StandAloneERV(StandAloneERVNum)%ExhaustAirFanName, &
                             'Design Size Maximum Exhaust Air Flow Rate [m3/s]', ExhaustAirVolFlowDes* &
                              StandAloneERV(StandAloneERVNum)%HighRHOAFlowRatio)

!      ERV fan type must be Fan:OnOff, min flow rate is assumed 0. Do not report min flow sizing.
!      CALL ReportSizingOutput(TRIM(cFanTypes(StandAloneERV(StandAloneERVNum)%ExhaustAirFanType_Num)), &
!                              StandAloneERV(StandAloneERVNum)%ExhaustAirFanName, &
!                               'Min Flow Rate [m3/s]', 0.0)
    ELSE
      IF (StandAloneERV(StandAloneERVNum)%ExhaustAirVolFlow > 0.0d0 .AND. ExhaustAirVolFlowDes > 0.0d0) THEN
        ExhaustAirVolFlowUser = StandAloneERV(StandAloneERVNum)%ExhaustAirVolFlow

        CALL ReportSizingOutput('ZoneHVAC:EnergyRecoveryVentilator',StandAloneERV(StandAloneERVNum)%Name, &
                            'Design Size Exhaust Air Flow Rate [m3/s]',ExhaustAirVolFlowDes, &
                            'User-Specified Exhaust Air Flow Rate [m3/s]',ExhaustAirVolFlowUser)
        IF (DisplayExtraWarnings) THEN
          IF ((ABS(ExhaustAirVolFlowDes - ExhaustAirVolFlowUser)/ExhaustAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
            CALL ShowMessage('SizeStandAloneERV: Potential issue with equipment sizing for ZoneHVAC:EnergyRecoveryVentilator ' &
                                       //TRIM(StandAloneERV(StandAloneERVNum)%Name))
            CALL ShowContinueError('User-Specified Exhaust Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(ExhaustAirVolFlowUser,5))// ' [m3/s]')
            CALL ShowContinueError('differs from Design Size Exhaust Air Flow Rate of ' // &
                                      TRIM(RoundSigDigits(ExhaustAirVolFlowDes,5))// ' [m3/s]')
            CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
            CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
          END IF
        ENDIF

        CALL ReportSizingOutput(cHXTypes(StandAloneERV(StandAloneERVNum)%HeatExchangerTypeNum), &
                              StandAloneERV(StandAloneERVNum)%HeatExchangerName, &
                             'Design Size Exhaust Air Flow Rate [m3/s]',ExhaustAirVolFlowDes, &
                             'User-Specified Exhaust Air Flow Rate [m3/s]',ExhaustAirVolFlowUser)
        IF (DisplayExtraWarnings) THEN
          IF ((ABS(ExhaustAirVolFlowDes - ExhaustAirVolFlowUser)/ExhaustAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
            CALL ShowMessage('SizeStandAloneERV: Potential issue with equipment sizing for ZoneHVAC:EnergyRecoveryVentilator '//  &
                                  TRIM(cHXTypes(StandAloneERV(StandAloneERVNum)%HeatExchangerTypeNum))//' '//  &
                                  TRIM(StandAloneERV(StandAloneERVNum)%HeatExchangerName))
            CALL ShowContinueError('User-Specified Exhaust Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(ExhaustAirVolFlowUser,5))// ' [m3/s]')
            CALL ShowContinueError('differs from Design Size Exhaust Air Flow Rate of ' // &
                                      TRIM(RoundSigDigits(ExhaustAirVolFlowDes,5))// ' [m3/s]')
            CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
            CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
          END IF
        ENDIF

        CALL ReportSizingOutput(cFanTypes(StandAloneERV(StandAloneERVNum)%ExhaustAirFanType_Num), &
                              StandAloneERV(StandAloneERVNum)%ExhaustAirFanName, &
                             'Design Size Maximum Exhaust Air Flow Rate [m3/s]', ExhaustAirVolFlowDes* &
                              StandAloneERV(StandAloneERVNum)%HighRHOAFlowRatio, &
                             'User-Specified Maximum Exhaust Air Flow Rate [m3/s]', ExhaustAirVolFlowUser)
        IF (DisplayExtraWarnings) THEN
          IF ((ABS(ExhaustAirVolFlowDes - ExhaustAirVolFlowUser)/ExhaustAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
            CALL ShowMessage('SizeStandAloneERV: Potential issue with equipment sizing for ZoneHVAC:EnergyRecoveryVentilator '//  &
                                  TRIM(cFanTypes(StandAloneERV(StandAloneERVNum)%SupplyAirFanType_Num))//' '// &
                                  TRIM(StandAloneERV(StandAloneERVNum)%ExhaustAirFanName))
            CALL ShowContinueError('User-Specified Maximum Exhaust Air Flow Rate of '// &
                                        TRIM(RoundSigDigits(ExhaustAirVolFlowUser,5))// ' [m3/s]')
            CALL ShowContinueError('differs from Design Size Maximum Exhaust Air Flow Rate of ' // &
                                      TRIM(RoundSigDigits(ExhaustAirVolFlowDes,5))// ' [m3/s]')
            CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
            CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
          END IF
        ENDIF
      END IF
    END IF
  END IF

  IsAutosize = .FALSE.
  IF (StandAloneERV(StandAloneERVNum)%DesignEAFanVolFlowRate == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF
  CALL SetFanData(StandAloneERV(StandAloneERVNum)%ExhaustAirFanIndex, ErrorsFound, &
                      StandAloneERV(StandAloneERVNum)%ExhaustAirFanName, &
                      StandAloneERV(StandAloneERVNum)%ExhaustAirVolFlow* &
                      StandAloneERV(StandAloneERVNum)%HighRHOAFlowRatio, &
                      0.0d0)
  DesignEAFanVolFlowRateDes = StandAloneERV(StandAloneERVNum)%ExhaustAirVolFlow* &
                              StandAloneERV(StandAloneERVNum)%HighRHOAFlowRatio
  IF (IsAutosize) THEN
    StandAloneERV(StandAloneERVNum)%DesignEAFanVolFlowRate =  DesignEAFanVolFlowRateDes
    CALL ReportSizingOutput(TRIM(cFanTypes(StandAloneERV(StandAloneERVNum)%ExhaustAirFanType_Num)), &
                              StandAloneERV(StandAloneERVNum)%ExhaustAirFanName, &
                             'Design Size Maximum Exhaust Air Flow Rate [m3/s]', DesignEAFanVolFlowRateDes)
  ELSE
    IF (StandAloneERV(StandAloneERVNum)%DesignEAFanVolFlowRate > 0.0d0 .AND. DesignEAFanVolFlowRateDes > 0.0d0) THEN
      DesignEAFanVolFlowRateUser = StandAloneERV(StandAloneERVNum)%DesignEAFanVolFlowRate
      CALL ReportSizingOutput(TRIM(cFanTypes(StandAloneERV(StandAloneERVNum)%ExhaustAirFanType_Num)), &
                              StandAloneERV(StandAloneERVNum)%ExhaustAirFanName, &
                             'Design Size Maximum Exhaust Air Flow Rate [m3/s]', DesignEAFanVolFlowRateDes, &
                             'User-Specified Maximum Exhaust Air Flow Rate [m3/s]', DesignEAFanVolFlowRateUser)
      IF (DisplayExtraWarnings) THEN
        IF ((ABS(DesignEAFanVolFlowRateDes - DesignEAFanVolFlowRateUser)/DesignEAFanVolFlowRateUser) &
                                   > AutoVsHardSizingThreshold) THEN
          CALL ShowMessage('SizeStandAloneERV: Potential issue with equipment sizing for ZoneHVAC:EnergyRecoveryVentilator '//  &
                                  TRIM(cFanTypes(StandAloneERV(StandAloneERVNum)%SupplyAirFanType_Num))//' '// &
                                  TRIM(StandAloneERV(StandAloneERVNum)%SupplyAirFanName))
          CALL ShowContinueError('User-Specified Maximum Exhaust Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(DesignEAFanVolFlowRateUser,5))// ' [m3/s]')
          CALL ShowContinueError('differs from Design Size Maximum Exhaust Air Flow Rate of ' // &
                                      TRIM(RoundSigDigits(DesignEAFanVolFlowRateDes,5))// ' [m3/s]')
          CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
          CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
        END IF
      ENDIF
    END IF
  END IF

  RETURN

END SUBROUTINE SizeStandAloneERV

SUBROUTINE CalcStandAloneERV(StandAloneERVNum,FirstHVACIteration,SensLoadMet,LatentMassLoadMet)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   June 2003
          !       MODIFIED       Don Shirey, Aug 2009 (LatentMassLoadMet)
          !                      July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate the components making up the Stand Alone ERV unit.

          ! METHODOLOGY EMPLOYED:
          ! Simulates the unit components sequentially in the air flow direction.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Fans,           ONLY: SimulateFanComponents
  USE HeatRecovery,   ONLY: SimHeatRecovery
  USE Psychrometrics, ONlY: PsyHFnTdbW
  USE DataZoneEquipment,  ONLY: ZoneEquipConfig
  USE General,            ONLY: RoundSigDigits
  USE DataAirLoop,    ONLY: OAControllerInfo

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

            ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: StandAloneERVNum   ! Unit index in ERV data structure
  LOGICAL, INTENT (IN)  :: FirstHVACIteration ! flag for 1st HVAC iteration in the time step
  REAL(r64),    INTENT (OUT) :: SensLoadMet        ! sensible zone load met by unit (W)
  REAL(r64),    INTENT (OUT) :: LatentMassLoadMet  ! latent zone load met by unit (kg/s), dehumid = negative

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: SupOutletNode    ! unit supply air outlet node
  INTEGER :: ExhaustInletNode ! unit exhaust air inlet node
  INTEGER :: SupInletNode     ! unit supply air inlet node
  REAL(r64) :: AirMassFlow      ! total mass flow through supply side of the ERV (supply air outlet node)
  REAL(r64) :: MinHumRatio      ! minimum humidity ratio for calculating sensible load met
                                ! (so enthalpy routines work without error)
  REAL(r64) :: TotLoadMet       ! total zone load met by unit (W)
  REAL(r64) :: LatLoadMet       ! latent zone load met by unit (W)
  LOGICAL :: HXUnitOn           ! flag to operate heat exchanger heat recovery
  LOGICAL :: EconomizerFlag     ! economizer signal from OA controller
  LOGICAL :: HighHumCtrlFlag    ! high humditiy control signal from OA controller
!  INTEGER :: ControlledZoneNum ! index to controlled zones
!  INTEGER :: ExhaustNodeNum    ! index to exhaust nodes in controlled zones
!  INTEGER :: SupplyNodeNum     ! index to supply nodes in controlled zone
!  LOGICAL :: ExhaustNodeFound  ! used in controlled zone exhaust node search
  REAL(r64) :: TotalExhaustMassFlow ! total exhaust air mass flow rate in controlled zone
  REAL(r64) :: TotalSupplyMassFlow  ! total supply air mass flow rate in controlled zone

  SupInletNode = StandAloneERV(StandAloneERVNum)%SupplyAirInletNode
  SupOutletNode = StandAloneERV(StandAloneERVNum)%SupplyAirOutletNode
  ExhaustInletNode = StandAloneERV(StandAloneERVNum)%ExhaustAirInletNode

! Stand alone ERV's HX is ON by default
  HXUnitOn = .TRUE.

! Get stand alone ERV's controller economizer and high humidity control status
  IF(StandAloneERV(StandAloneERVNum)%ControllerNameDefined)THEN
    EconomizerFlag = OAControllerInfo(StandAloneERV(StandAloneERVNum)%ControllerIndex)%EconoActive
    HighHumCtrlFlag = OAControllerInfo(StandAloneERV(StandAloneERVNum)%ControllerIndex)%HighHumCtrlActive
  ELSE
    EconomizerFlag = .FALSE.
    HighHumCtrlFlag = .FALSE.
  END IF

  CALL SimHeatRecovery(StandAloneERV(StandAloneERVNum)%HeatExchangerName,FirstHVACIteration,  &
                            StandAloneERV(StandAloneERVNum)%HeatExchangerIndex, ContFanCycCoil, HXUnitEnable=HXUnitOn, &
                            EconomizerFlag = EconomizerFlag, HighHumCtrlFlag = HighHumCtrlFlag)
  StandAloneERV(StandAloneERVNum)%ElecUseRate = AirToAirHXElecPower

  CALL SimulateFanComponents(StandAloneERV(StandAloneERVNum)%SupplyAirFanName,FirstHVACIteration,  &
                             StandAloneERV(StandAloneERVNum)%SupplyAirFanIndex, &
                             ZoneCompTurnFansOn =ZoneCompTurnFansOn,ZoneCompTurnFansOff =ZoneCompTurnFansOff)
  StandAloneERV(StandAloneERVNum)%ElecUseRate = StandAloneERV(StandAloneERVNum)%ElecUseRate + FanElecPower

  CALL SimulateFanComponents(StandAloneERV(StandAloneERVNum)%ExhaustAirFanName,FirstHVACIteration,  &
                             StandAloneERV(StandAloneERVNum)%ExhaustAirFanIndex)
  StandAloneERV(StandAloneERVNum)%ElecUseRate = StandAloneERV(StandAloneERVNum)%ElecUseRate + FanElecPower

  MinHumRatio = Node(ExhaustInletNode)%HumRat
  IF(Node(SupOutletNode)%HumRat .LT. Node(ExhaustInletNode)%HumRat) MinHumRatio = Node(SupOutletNode)%HumRat

  AirMassFlow = Node(SupOutletNode)%MassFlowRate
  SensLoadMet = AirMassFlow * (PsyHFnTdbW(Node(SupOutletNode)%Temp,MinHumRatio)  &
                         - PsyHFnTdbW(Node(ExhaustInletNode)%Temp,MinHumRatio))
  TotLoadMet = AirMassFlow * (PsyHFnTdbW(Node(SupOutletNode)%Temp,Node(SupOutletNode)%HumRat)  &
                         - PsyHFnTdbW(Node(ExhaustInletNode)%Temp,Node(ExhaustInletNode)%HumRat))
  LatLoadMet = TotLoadMet - SensLoadMet ! watts

  LatentMassLoadMet = AirMassFlow * (Node(SupOutletNode)%HumRat - Node(ExhaustInletNode)%HumRat) ! kg/s, dehumidification = negative


  IF(SensLoadMet .LT. 0.0d0) THEN
    StandAloneERV(StandAloneERVNum)%SensCoolingRate = ABS(SensLoadMet)
    StandAloneERV(StandAloneERVNum)%SensHeatingRate = 0.0d0
  ELSE
    StandAloneERV(StandAloneERVNum)%SensCoolingRate = 0.0d0
    StandAloneERV(StandAloneERVNum)%SensHeatingRate = SensLoadMet
  END IF
  IF(TotLoadMet .LT. 0.0d0) THEN
    StandAloneERV(StandAloneERVNum)%TotCoolingRate = ABS(TotLoadMet)
    StandAloneERV(StandAloneERVNum)%TotHeatingRate = 0.0d0
  ELSE
    StandAloneERV(StandAloneERVNum)%TotCoolingRate = 0.0d0
    StandAloneERV(StandAloneERVNum)%TotHeatingRate = TotLoadMet
  END IF
  IF(LatLoadMet .LT. 0.0d0) THEN
    StandAloneERV(StandAloneERVNum)%LatCoolingRate = ABS(LatLoadMet)
    StandAloneERV(StandAloneERVNum)%LatHeatingRate = 0.0d0
  ELSE
    StandAloneERV(StandAloneERVNum)%LatCoolingRate = 0.0d0
    StandAloneERV(StandAloneERVNum)%LatHeatingRate = LatLoadMet
  END IF

! Provide a one time message when exhaust flow rate is greater than supply flow rate
  IF ( StandAloneERV(StandAloneERVNum)%FlowError .AND. .NOT. WarmupFlag) THEN

!! Adding up zone inlet/outlet nodes is not working correctly. When imbalance flow occurs, the difference
!! is placed on the zone return node even when there is nothing connected to it.

!    IF(StandAloneERV(StandAloneERVNum)%ControlledZoneNum .GT. 0)THEN
!      TotalExhaustMassFlow = 0.0
!      DO ExhaustNodeNum = 1, ZoneEquipConfig(StandAloneERV(StandAloneERVNum)%ControlledZoneNum)%NumExhaustNodes
!         TotalExhaustMassFlow = TotalExhaustMassFlow + &
!             Node(ZoneEquipConfig(StandAloneERV(StandAloneERVNum)%ControlledZoneNum)%ExhaustNode(ExhaustNodeNum))%MassFlowRate
!      END DO
!    ELSE
!      DO ControlledZoneNum = 1, NumOfControlledZones
!        TotalExhaustMassFlow = 0.0
!        ExhaustNodeFound = .FALSE.
!        DO ExhaustNodeNum = 1, ZoneEquipConfig(ControlledZoneNum)%NumExhaustNodes
!          TotalExhaustMassFlow = TotalExhaustMassFlow + &
!                                 Node(ZoneEquipConfig(ControlledZoneNum)%ExhaustNode(ExhaustNodeNum))%MassFlowRate
!          IF(ZoneEquipConfig(ControlledZoneNum)%ExhaustNode(ExhaustNodeNum) .EQ. ExhaustInletNode) THEN
!            ExhaustNodeFound = .TRUE.
!            StandAloneERV(StandAloneERVNum)%ControlledZoneNum = ControlledZoneNum
!          END IF
!        END DO
!        IF(ExhaustNodeFound)EXIT
!      END DO
!    END IF
!
!    IF(StandAloneERV(StandAloneERVNum)%ControlledZoneNum .GT. 0)THEN
!!     Add in return node mass flow rate to total exhaust
!      IF(ZoneEquipConfig(StandAloneERV(StandAloneERVNum)%ControlledZoneNum)%ReturnAirNode .GT. 0)THEN
!        TotalExhaustMassFlow = TotalExhaustMassFlow + &
!            Node(ZoneEquipConfig(StandAloneERV(StandAloneERVNum)%ControlledZoneNum)%ReturnAirNode)%MassFlowRate
!      END IF
!      TotalSupplyMassFlow = 0.0
!      DO SupplyNodeNum = 1, ZoneEquipConfig(StandAloneERV(StandAloneERVNum)%ControlledZoneNum)%NumInletNodes
!        TotalSupplyMassFlow = TotalSupplyMassFlow + &
!            Node(ZoneEquipConfig(StandAloneERV(StandAloneERVNum)%ControlledZoneNum)%InletNode(SupplyNodeNum))%MassFlowRate
!      END DO
!
      TotalExhaustMassFlow = Node(ExhaustInletNode)%MassFlowRate
      TotalSupplyMassFlow  = Node(SupInletNode)%MassFlowRate
      IF ( TotalExhaustMassFlow > TotalSupplyMassFlow ) THEN
        CALL ShowWarningError('For '//TRIM(StandAloneERV(StandAloneERVNum)%UnitType)//' "'// &
                           TRIM(StandAloneERV(StandAloneERVNum)%Name)// &
                          '" there is unbalanced exhaust air flow.')
        CALL ShowContinueError('... The exhaust air mass flow rate = ' &
                                  //TRIM(RoundSigDigits(Node(ExhaustInletNode)%MassFlowRate,6)))
        CALL ShowContinueError('... The  supply air mass flow rate = ' &
                                  //TRIM(RoundSigDigits(Node(SupInletNode)%MassFlowRate,6)))
        CALL ShowContinueErrorTimeStamp(' ')
        CALL ShowContinueError('... Unless there is balancing infiltration / ventilation air flow, this will result in')
        CALL ShowContinueError('... load due to induced outside air being neglected in the simulation.')
        StandAloneERV(StandAloneERVNum)%FlowError = .FALSE.
      END IF
!    END IF
  END IF

  RETURN
END SUBROUTINE CalcStandAloneERV

SUBROUTINE ReportStandAloneERV(StandAloneERVNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   June 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Fill remaining report variables

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: StandAloneERVNum ! number of the current Stand Alone ERV being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: ReportingConstant

  ReportingConstant = TimeStepSys*SecInHour
  StandAloneERV(StandAloneERVNum)%ElecUseEnergy = StandAloneERV(StandAloneERVNum)%ElecUseRate*ReportingConstant
  StandAloneERV(StandAloneERVNum)%SensCoolingEnergy = StandAloneERV(StandAloneERVNum)%SensCoolingRate*ReportingConstant
  StandAloneERV(StandAloneERVNum)%LatCoolingEnergy = StandAloneERV(StandAloneERVNum)%LatCoolingRate*ReportingConstant
  StandAloneERV(StandAloneERVNum)%TotCoolingEnergy = StandAloneERV(StandAloneERVNum)%TotCoolingRate*ReportingConstant
  StandAloneERV(StandAloneERVNum)%SensHeatingEnergy = StandAloneERV(StandAloneERVNum)%SensHeatingRate*ReportingConstant
  StandAloneERV(StandAloneERVNum)%LatHeatingEnergy = StandAloneERV(StandAloneERVNum)%LatHeatingRate*ReportingConstant
  StandAloneERV(StandAloneERVNum)%TotHeatingEnergy = StandAloneERV(StandAloneERVNum)%TotHeatingRate*ReportingConstant

  RETURN

END SUBROUTINE ReportStandAloneERV

!        End of Reporting subroutines for the Module

!        Utility subroutines/functions for the HeatingCoil Module

FUNCTION GetSupplyAirFlowRate(ERVType,ERVCtrlName,ErrorsFound) RESULT(AirFlowRate)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the ERVCtrlName in the ERV Stand Alone list and returns the
          ! Supply Air Flow rate, if found.  If incorrect name is given, errorsfound is returned as true
          ! and supply air flow rate as negative.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem,SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ERVType      ! must be "ZoneHVAC:EnergyRecoveryVentilator"
  CHARACTER(len=*), INTENT(IN) :: ERVCtrlName  ! must match a controller name in the ERV data structure
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  REAL(r64)                    :: AirFlowRate  ! returned supply air flow rate of the ERV unit

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichERV

  IF (GetERVInputFlag) THEN
    CALL GetStandAloneERV
    GetERVInputFlag = .FALSE.
  END IF

  IF (SameString(ERVType,'ZoneHVAC:EnergyRecoveryVentilator')) THEN
    WhichERV=FindItem(ERVCtrlName,StandAloneERV%ControllerName,NumStandAloneERVs)
    IF (WhichERV /= 0) THEN
      AirFlowRate=StandAloneERV(WhichERV)%SupplyAirVolFlow
    ENDIF
  ELSE
    WhichERV=0
  ENDIF

  IF (WhichERV == 0) THEN
    CALL ShowSevereError('Could not find ZoneHVAC:EnergyRecoveryVentilator with Controller Name="'//TRIM(ERVCtrlName)//'"')
    ErrorsFound=.TRUE.
    AirFlowRate=-1000.d0
  ENDIF

  RETURN

END FUNCTION GetSupplyAirFlowRate

FUNCTION GetSupplyAirInletNode(ERVType,ERVCtrlName,ErrorsFound) RESULT(AirInletNode)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the ERVCtrlName in the ERV Stand Alone list and returns the
          ! Supply Air Inlet Node Number, if found.  If incorrect name is given, errorsfound is returned as true
          ! and Supply Air Inlet Node Number as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem,SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ERVType      ! must be "ZoneHVAC:EnergyRecoveryVentilator"
  CHARACTER(len=*), INTENT(IN) :: ERVCtrlName  ! must match a controller name in the ERV data structure
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: AirInletNode ! returned air inlet node number of the ERV unit

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichERV

  IF (GetERVInputFlag) THEN
    CALL GetStandAloneERV
    GetERVInputFlag = .FALSE.
  END IF

  IF (SameString(ERVType,'ZoneHVAC:EnergyRecoveryVentilator')) THEN
    WhichERV=FindItem(ERVCtrlName,StandAloneERV%ControllerName,NumStandAloneERVs)
    IF (WhichERV /= 0) THEN
      AirInletNode=StandAloneERV(WhichERV)%SupplyAirInletNode
    ENDIF
  ELSE
    WhichERV=0
  ENDIF

  IF (WhichERV == 0) THEN
    CALL ShowSevereError('Could not find ZoneHVAC:EnergyRecoveryVentilator with Controller Name="'//TRIM(ERVCtrlName)//'"')
    ErrorsFound=.TRUE.
    AirInletNode=0
  ENDIF

  RETURN

END FUNCTION GetSupplyAirInletNode

FUNCTION GetExhaustAirInletNode(ERVType,ERVCtrlName,ErrorsFound) RESULT(AirInletNode)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the ERVCtrlName in the ERV Stand Alone list and returns the
          ! Exhaust Air Inlet Node Number, if found.  If incorrect name is given, errorsfound is returned as true
          ! and Exhaust Air Inlet Node Number as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem,SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ERVType      ! must be "ZoneHVAC:EnergyRecoveryVentilator"
  CHARACTER(len=*), INTENT(IN) :: ERVCtrlName  ! must match a controller name in the ERV data structure
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: AirInletNode ! returned air inlet node number of the ERV unit

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichERV

  IF (GetERVInputFlag) THEN
    CALL GetStandAloneERV
    GetERVInputFlag = .FALSE.
  END IF

  IF (SameString(ERVType,'ZoneHVAC:EnergyRecoveryVentilator')) THEN
    WhichERV=FindItem(ERVCtrlName,StandAloneERV%ControllerName,NumStandAloneERVs)
    IF (WhichERV /= 0) THEN
      AirInletNode=StandAloneERV(WhichERV)%ExhaustAirInletNode
    ENDIF
  ELSE
    WhichERV=0
  ENDIF

  IF (WhichERV == 0) THEN
    CALL ShowSevereError('Could not find ZoneHVAC:EnergyRecoveryVentilator with Controller Name="'//TRIM(ERVCtrlName)//'"')
    ErrorsFound=.TRUE.
    AirInletNode=0
  ENDIF

  RETURN

END FUNCTION GetExhaustAirInletNode

INTEGER FUNCTION GetStandAloneERVOutAirNode(StandAloneERVNum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for OA inlet node for ventilation rate reporting

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: StandAloneERVNum          !

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (GetERVInputFlag) THEN
    CALL GetStandAloneERV
    GetERVInputFlag = .FALSE.
  END IF

  GetStandAloneERVOutAirNode = 0
  If (StandAloneERVNum > 0 .and. StandAloneERVNum <= NumStandAloneERVs) THEN
    GetStandAloneERVOutAirNode = StandAloneERV(StandAloneERVNum)%SupplyAirInletNode
  ENDIF

  RETURN

END FUNCTION GetStandAloneERVOutAirNode

INTEGER FUNCTION GetStandAloneERVZoneInletAirNode(StandAloneERVNum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for OA inlet node for ventilation rate reporting

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: StandAloneERVNum          !

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (GetERVInputFlag) THEN
    CALL GetStandAloneERV
    GetERVInputFlag = .FALSE.
  END IF

  GetStandAloneERVZoneInletAirNode = 0
  If (StandAloneERVNum > 0 .and. StandAloneERVNum <= NumStandAloneERVs) THEN
    GetStandAloneERVZoneInletAirNode = StandAloneERV(StandAloneERVNum)%SupplyAirOutletNode
  ENDIF

  RETURN

END FUNCTION GetStandAloneERVZoneInletAirNode


INTEGER FUNCTION GetStandAloneERVReturnAirNode(StandAloneERVNum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for OA inlet node for ventilation rate reporting

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: StandAloneERVNum          !

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (GetERVInputFlag) THEN
    CALL GetStandAloneERV
    GetERVInputFlag = .FALSE.
  END IF

  GetStandAloneERVReturnAirNode = 0
  If (StandAloneERVNum > 0 .and. StandAloneERVNum <= NumStandAloneERVs) THEN
    GetStandAloneERVReturnAirNode = StandAloneERV(StandAloneERVNum)%ExhaustAirInletNode
  ENDIF

  RETURN

END FUNCTION GetStandAloneERVReturnAirNode


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

END MODULE HVACStandAloneERV
