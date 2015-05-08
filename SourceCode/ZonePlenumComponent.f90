MODULE ZonePlenum
  ! Module containing simulation routines for both zone return and zone supply plenums

  ! MODULE INFORMATION:
  !       AUTHOR         Peter Graham Ellis
  !       DATE WRITTEN   November 2000
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required to
  ! manage Air Path Zone Return Plenum Components

  ! METHODOLOGY EMPLOYED:
  ! The Zone Plenum

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals, ONLY: BeginEnvrnFlag,BeginDayFlag, NumOfZones
USE DataInterfaces, ONLY: ShowSevereError,ShowFatalError, ShowContinueError
USE DataLoopNode
USE DataHVACGlobals
USE DataEnvironment, ONLY: OUTBAROPRESS, OutHumRat

  ! Use statements for access to subroutines in other modules
USE Psychrometrics, ONLY:PsyTdbFnHW,PsyHFnTdbW

IMPLICIT NONE         ! Enforce explicit typing of all variables

PUBLIC ! Everything private unless explicitly made public


  ! DERIVED TYPE DEFINITIONS
TYPE ZoneReturnPlenumConditions
  CHARACTER(len=MaxNameLength) :: ZonePlenumName =' '
  CHARACTER(len=MaxNameLength) :: ZoneName       =' '
  CHARACTER(len=MaxNameLength) :: ZoneNodeName   =' '
  REAL(r64)    :: ZoneTemp                       =0.0d0
  REAL(r64)    :: ZoneHumRat                     =0.0d0
  REAL(r64)    :: ZoneEnthalpy                   =0.0d0
  REAL(r64)    :: OutletTemp                     =0.0d0
  REAL(r64)    :: OutletHumRat                   =0.0d0
  REAL(r64)    :: OutletEnthalpy                 =0.0d0
  REAL(r64)    :: OutletPressure                 =0.0d0
  INTEGER      :: ZoneNodeNum                    =0
  INTEGER      :: ActualZoneNum                  =0
  INTEGER      :: OutletNode                     =0
  REAL(r64)    :: OutletMassFlowRate             =0.0d0 !MassFlow through the ZonePlenum being Simulated [kg/Sec]
  REAL(r64)    :: OutletMassFlowRateMaxAvail     =0.0d0 ! [kg/Sec]
  REAL(r64)    :: OutletMassFlowRateMinAvail     =0.0d0 ! [kg/Sec]
  INTEGER      :: NumInducedNodes                =0
  INTEGER, DIMENSION(:), ALLOCATABLE      ::InducedNode
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::InducedMassFlowRate
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::InducedMassFlowRateMaxAvail
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::InducedMassFlowRateMinAvail
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::InducedTemp
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::InducedHumRat
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::InducedEnthalpy
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::InducedPressure
  LOGICAL      :: InitFlag                       =.false.
  INTEGER      :: NumInletNodes                  =0
  INTEGER, DIMENSION(:), ALLOCATABLE      ::InletNode
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::InletMassFlowRate
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::InletMassFlowRateMaxAvail
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::InletMassFlowRateMinAvail
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::InletTemp
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::InletHumRat
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::InletEnthalpy
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::InletPressure
  INTEGER, DIMENSION(:), ALLOCATABLE ::ADUIndex        ! index to AirDistUnit leaking to this plenum
  INTEGER                            ::NumADUs         ! number of ADU's that can leak to this plenum
  INTEGER, DIMENSION(:), ALLOCATABLE ::ZoneEqNum       ! list of zone equip config indices for this plenum
END TYPE ZoneReturnPlenumConditions

TYPE ZoneSupplyPlenumConditions
  CHARACTER(len=MaxNameLength) :: ZonePlenumName =' '
  CHARACTER(len=MaxNameLength) :: ZoneName       =' '
  CHARACTER(len=MaxNameLength) :: ZoneNodeName   =' '
  REAL(r64)    :: ZoneTemp                       =0.0d0
  REAL(r64)    :: ZoneHumRat                     =0.0d0
  REAL(r64)    :: ZoneEnthalpy                   =0.0d0
  REAL(r64)    :: InletTemp                      =0.0d0
  REAL(r64)    :: InletHumRat                    =0.0d0
  REAL(r64)    :: InletEnthalpy                  =0.0d0
  REAL(r64)    :: InletPressure                  =0.0d0
  INTEGER      :: ZoneNodeNum                    =0
  INTEGER      :: ActualZoneNum                  =0
  INTEGER      :: InletNode                      =0
  REAL(r64)    :: InletMassFlowRate              =0.0d0 !MassFlow through the ZonePlenum being Simulated [kg/Sec]
  REAL(r64)    :: InletMassFlowRateMaxAvail      =0.0d0 ! [kg/Sec]
  REAL(r64)    :: InletMassFlowRateMinAvail      =0.0d0 ! [kg/Sec]
  LOGICAL      :: InitFlag                       =.false.
  INTEGER      :: NumOutletNodes                 =0
  INTEGER, DIMENSION(:), ALLOCATABLE ::OutletNode
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::OutletMassFlowRate
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::OutletMassFlowRateMaxAvail
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::OutletMassFlowRateMinAvail
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::OutletTemp
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::OutletHumRat
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::OutletEnthalpy
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::OutletPressure
END TYPE ZoneSupplyPlenumConditions

INTEGER, PUBLIC     :: NumZonePlenums=0             ! The Number of ZonePlenums found in the Input
INTEGER, PUBLIC     :: NumZoneReturnPlenums=0             ! The Number of ZoneReturnPlenums found in the Input
INTEGER, PUBLIC     :: NumZoneSupplyPlenums=0             ! The Number of ZoneSupplyPlenums found in the Input
TYPE (ZoneReturnPlenumConditions), PUBLIC, ALLOCATABLE, DIMENSION(:) :: ZoneRetPlenCond
TYPE (ZoneSupplyPlenumConditions), PUBLIC, ALLOCATABLE, DIMENSION(:) :: ZoneSupPlenCond
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckRetEquipName
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckSupEquipName


  ! SUBROUTINE SPECIFICATIONS FOR MODULE ZONEPLENUM
PUBLIC  SimAirZonePlenum
PUBLIC  GetZonePlenumInput
PRIVATE InitAirZoneReturnPlenum
PRIVATE InitAirZoneSupplyPlenum
PRIVATE CalcAirZoneReturnPlenum
PRIVATE CalcAirZoneSupplyPlenum
PRIVATE UpdateAirZoneReturnPlenum
PRIVATE UpdateAirZoneSupplyPlenum
PRIVATE ReportZoneSupplyPlenum
PRIVATE ReportZoneReturnPlenum

CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE SimAirZonePlenum(CompName, iCompType, CompIndex, FirstHVACIteration, FirstCall, PlenumInletChanged)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   November 2000
          !       MODIFIED       March 2000
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages the ZonePlenum component simulation for both
          ! return and supply plenums.
          ! It is called from the SimAirLoopComponent at the system time step.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: TrimSigDigits
  USE DataZoneEquipment, ONLY: ZoneReturnPlenum_Type,ZoneSupplyPlenum_Type

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: CompName
  INTEGER, INTENT(IN)           :: iCompType
  INTEGER, INTENT(INOUT)        :: CompIndex
  LOGICAL, OPTIONAL, INTENT(IN) :: FirstHVACIteration !Objexx:OPTIONAL Used without PRESENT check
  LOGICAL, OPTIONAL, INTENT(IN) :: FirstCall !Objexx:OPTIONAL Used without PRESENT check
  LOGICAL, OPTIONAL             :: PlenumInletChanged !Objexx:OPTIONAL Used without PRESENT check


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ZonePlenumNum         ! The ZonePlenum that you are currently loading input into
  LOGICAL,SAVE :: GetInputFlag = .TRUE. ! Flag set to make sure you get input once

          ! FLOW:

  ! Obtains and Allocates ZonePlenum related parameters from input file
  IF (GetInputFlag) THEN  !First time subroutine has been entered
    CALL GetZonePlenumInput
    GetInputFlag=.FALSE.
  END IF

  SELECT CASE (iCompType)

    CASE (ZoneReturnPlenum_Type)  ! 'AirLoopHVAC:ReturnPlenum'
      ! Find the correct ZonePlenumNumber
      IF (CompIndex == 0) THEN
        ZonePlenumNum=FindItemInList(CompName,ZoneRetPlenCond%ZonePlenumName,NumZoneReturnPlenums)
        IF (ZonePlenumNum == 0) THEN
          CALL ShowFatalError('SimAirZonePlenum: AirLoopHVAC:ReturnPlenum not found='//TRIM(CompName))
        ENDIF
        CompIndex=ZonePlenumNum
      ELSE
        ZonePlenumNum=CompIndex
        IF (ZonePlenumNum > NumZoneReturnPlenums .or. ZonePlenumNum < 1) THEN
          CALL ShowFatalError('SimAirZonePlenum: Invalid CompIndex passed='//  &
                              TRIM(TrimSigDigits(ZonePlenumNum))// &
                              ', Number of AirLoopHVAC:ReturnPlenum='//TRIM(TrimSigDigits(NumZoneReturnPlenums))//  &
                              ', AirLoopHVAC:ReturnPlenum name='//TRIM(CompName))
        ENDIF
        IF (CheckRetEquipName(ZonePlenumNum)) THEN
          IF (CompName /= ZoneRetPlenCond(ZonePlenumNum)%ZonePlenumName) THEN
            CALL ShowFatalError('SimAirZonePlenum: Invalid CompIndex passed='//  &
                                TRIM(TrimSigDigits(ZonePlenumNum))//     &
                                ', AirLoopHVAC:ReturnPlenum name='//TRIM(CompName)//   &
                                ', stored AirLoopHVAC:ReturnPlenum Name for that index='//  &
                                TRIM(ZoneRetPlenCond(ZonePlenumNum)%ZonePlenumName))
          ENDIF
          CheckRetEquipName(ZonePlenumNum)=.false.
        ENDIF
      ENDIF

      CALL InitAirZoneReturnPlenum(ZonePlenumNum)  ! Initialize all ZonePlenum related parameters

      CALL CalcAirZoneReturnPlenum(ZonePlenumNum)

      CALL UpdateAirZoneReturnPlenum(ZonePlenumNum)  ! Update the current ZonePlenum to the outlet nodes

      CALL ReportZoneReturnPlenum(ZonePlenumNum)

    CASE (ZoneSupplyPlenum_Type)  ! 'AirLoopHVAC:SupplyPlenum'
      ! Find the correct ZonePlenumNumber
      IF (CompIndex == 0) THEN
        ZonePlenumNum=FindItemInList(CompName,ZoneSupPlenCond%ZonePlenumName,NumZoneSupplyPlenums)
        IF (ZonePlenumNum == 0) THEN
          CALL ShowFatalError('SimAirZonePlenum: AirLoopHVAC:SupplyPlenum not found='//TRIM(CompName))
        ENDIF
        CompIndex=ZonePlenumNum
      ELSE
        ZonePlenumNum=CompIndex
        IF (ZonePlenumNum > NumZoneSupplyPlenums .or. ZonePlenumNum < 1) THEN
          CALL ShowFatalError('SimAirZonePlenum: Invalid CompIndex passed='//  &
                              TRIM(TrimSigDigits(ZonePlenumNum))// &
                              ', Number of AirLoopHVAC:SupplyPlenum='//TRIM(TrimSigDigits(NumZoneReturnPlenums))//  &
                              ', AirLoopHVAC:SupplyPlenum name='//TRIM(CompName))
        ENDIF
        IF (CheckSupEquipName(ZonePlenumNum)) THEN
          IF (CompName /= ZoneSupPlenCond(ZonePlenumNum)%ZonePlenumName) THEN
            CALL ShowFatalError('SimAirZonePlenum: Invalid CompIndex passed='//  &
                                TRIM(TrimSigDigits(ZonePlenumNum))//     &
                                ', AirLoopHVAC:SupplyPlenum name='//TRIM(CompName)//   &
                                ', stored AirLoopHVAC:SupplyPlenum Name for that index='//  &
                                TRIM(ZoneSupPlenCond(ZonePlenumNum)%ZonePlenumName))
          ENDIF
          CheckSupEquipName(ZonePlenumNum)=.false.
        ENDIF
      ENDIF

      CALL InitAirZoneSupplyPlenum(ZonePlenumNum, FirstHVACIteration, FirstCall)  ! Initialize all ZonePlenum related parameters

      CALL CalcAirZoneSupplyPlenum(ZonePlenumNum, FirstCall)
      ! Update the current ZonePlenum to the outlet nodes
      CALL UpdateAirZoneSupplyPlenum(ZonePlenumNum, PlenumInletChanged, FirstCall)

      CALL ReportZoneSupplyPlenum(ZonePlenumNum)

    CASE DEFAULT
      CALL ShowSevereError('SimAirZonePlenum: Errors in Plenum='//TRIM(CompName))
      CALL ShowContinueError('ZonePlenum: Unhandled plenum type found:'//TRIM(TrimSigDigits(iCompType)))
      CALL ShowFatalError('Preceding conditions cause termination.')

  END SELECT

  RETURN

END SUBROUTINE SimAirZonePlenum


! Get Input Section of the Module
!******************************************************************************

SUBROUTINE GetZonePlenumInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   November 2000
          !       MODIFIED       August 2003, FCW: For each zone with a return air plenum put the ZoneRetPlenCond
          !                       number for the return air plenum in the ZoneEquipConfig array for the zone
          !                       for later access to the zone's return air plenum conditions.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is the main routine to call other input routines and Get routines

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor, ONLY: GetNumObjectsFound,GetObjectItem,VerifyName,FindItemInList,GetObjectDefMaxArgs
    USE NodeInputManager, ONLY: GetOnlySingleNode, GetNodeNums, InitUniqueNodeCheck, CheckUniqueNodes, EndUniqueNodeCheck
    USE DataHeatBalance, ONLY: Zone
    USE DataZoneEquipment, ONLY: ZoneEquipConfig
    USE DataIPShortCuts
    USE PoweredInductionUnits, ONLY: PIUInducesPlenumAir

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
    INTEGER :: ZonePlenumNum      ! The ZonePlenum that you are currently loading input into
    INTEGER :: ZonePlenumLoop
    INTEGER :: ZoneEquipConfigLoop
    INTEGER :: NumAlphas
    INTEGER :: NumNums
    INTEGER :: NumArgs
    INTEGER :: NumNodes
    INTEGER, ALLOCATABLE, DIMENSION(:) :: NodeNums
    INTEGER :: MaxNums
    INTEGER :: MaxAlphas
    INTEGER :: NodeNum
    INTEGER :: IOSTAT
    REAL(r64), ALLOCATABLE, DIMENSION(:) :: NumArray          ! Numeric input items for object
    CHARACTER(len=MaxNameLength)  :: CurrentModuleObject      ! for ease in getting objects
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: AlphArray      ! Alpha input items for object
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.
    LOGICAL :: ErrorsFound=.FALSE.
    LOGICAL       :: IsNotOK               ! Flag to verify name
    LOGICAL       :: IsBlank               ! Flag for blank name
    LOGICAL       :: NodeListError         ! Flag for node list error
    LOGICAL       :: UniqueNodeError
    CHARACTER(len=*), PARAMETER    :: RoutineName='GetZonePlenumInput: ' ! include trailing blank space
    CHARACTER(len=MaxNameLength)   :: InducedNodeListName

          ! Flow
    CALL GetObjectDefMaxArgs('AirLoopHVAC:ReturnPlenum',NumArgs,NumAlphas,NumNums)
    MaxNums=NumNums
    MaxAlphas=NumAlphas
    CALL GetObjectDefMaxArgs('AirLoopHVAC:SupplyPlenum',NumArgs,NumAlphas,NumNums)
    MaxNums=MAX(NumNums,MaxNums)
    MaxAlphas=MAX(NumAlphas,MaxAlphas)
    ALLOCATE(AlphArray(MaxAlphas))
    AlphArray=' '
    ALLOCATE(cAlphaFields(MaxAlphas))
    cAlphaFields=' '
    ALLOCATE(cNumericFields(MaxNums))
    cNumericFields=' '
    ALLOCATE(NumArray(MaxNums))
    NumArray=0.0d0
    ALLOCATE(lAlphaBlanks(MaxAlphas))
    lAlphaBlanks=.TRUE.
    ALLOCATE(lNumericBlanks(MaxNums))
    lNumericBlanks=.TRUE.
    CALL GetObjectDefMaxArgs('NodeList',NumArgs,NumAlphas,NumNums)
    ALLOCATE(NodeNums(NumArgs))
    NodeNums=0

    InducedNodeListName = ' '


    NumZoneReturnPlenums = GetNumObjectsFound('AirLoopHVAC:ReturnPlenum')
    NumZoneSupplyPlenums = GetNumObjectsFound('AirLoopHVAC:SupplyPlenum')
    NumZonePlenums = NumZoneReturnPlenums + NumZoneSupplyPlenums

    IF (NumZoneReturnPlenums.GT.0) ALLOCATE(ZoneRetPlenCond(NumZoneReturnPlenums))
    IF (NumZoneSupplyPlenums.GT.0) ALLOCATE(ZoneSupPlenCond(NumZoneSupplyPlenums))
    ALLOCATE(CheckRetEquipName(NumZoneReturnPlenums))
    CheckRetEquipName=.true.
    ALLOCATE(CheckSupEquipName(NumZoneSupplyPlenums))
    CheckSupEquipName=.true.


    ZonePlenumNum = 0

    CALL InitUniqueNodeCheck('AirLoopHVAC:ReturnPlenum')
    DO ZonePlenumLoop = 1, NumZoneReturnPlenums
      ZonePlenumNum = ZonePlenumNum + 1

      CurrentModuleObject='AirLoopHVAC:ReturnPlenum'

      CALL GetObjectItem(CurrentModuleObject,ZonePlenumNum,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT, &
                         NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                         AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
      IsNotOK=.FALSE.
      IsBlank=.FALSE.
      CALL VerifyName(AlphArray(1),ZoneRetPlenCond%ZonePlenumName,ZonePlenumNum-1,IsNotOK,IsBlank, &
                      TRIM(CurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.TRUE.
        IF (IsBlank) AlphArray(1)='xxxxx'
      ENDIF
      ZoneRetPlenCond(ZonePlenumNum)%ZonePlenumName = AlphArray(1)

      ! Check if this zone is also used in another return plenum
      IOSTAT=FindItemInList(AlphArray(2),ZoneRetPlenCond%ZoneName,ZonePlenumNum-1)
      IF (IOSTAT /= 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cAlphaFields(2))//' "'//TRIM(AlphArray(2))// &
                             '" is used more than once as a '//TRIM(CurrentModuleObject)//'.')
        CALL ShowContinueError('..Only one '//TRIM(CurrentModuleObject)//' object may be connected to a given zone.')
        CALL ShowContinueError('..occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphArray(1)))
        ErrorsFound=.true.
      ENDIF
      ZoneRetPlenCond(ZonePlenumNum)%ZoneName = AlphArray(2)
      ! put the X-Ref to the zone heat balance data structure
      ZoneRetPlenCond(ZonePlenumNum)%ActualZoneNum    = FindItemInList(AlphArray(2),Zone%Name,NumOfZones)
      IF (ZoneRetPlenCond(ZonePlenumNum)%ActualZoneNum == 0) THEN
        CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphArray(1))// &
                             ', '//TRIM(cAlphaFields(2))//' = '//TRIM(AlphArray(2))//' not found.')
        ErrorsFound=.TRUE.
        CYCLE
      ENDIF
      !  Check if this zone is used as a controlled zone
      ZoneEquipConfigLoop=FindItemInList(AlphArray(2),ZoneEquipConfig%ZoneName,NumOfZones)
      IF (ZoneEquipConfigLoop /= 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cAlphaFields(2))//' "'//TRIM(AlphArray(2))//'" is a controlled zone.'//  &
                          ' It cannot be used as a '//TRIM(CurrentModuleObject))
        CALL ShowContinueError('..occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphArray(1)))
        ErrorsFound=.true.
      ENDIF

      ZoneRetPlenCond(ZonePlenumNum)%ZoneNodeName = AlphArray(3)
      ZoneRetPlenCond(ZonePlenumNum)%ZoneNodeNum = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                            NodeType_Air,NodeConnectionType_ZoneNode,1,ObjectIsNotParent)
      !Insert the Plenum Zone Number into the Zone Heat Balance data structure for later reference
      Zone(ZoneRetPlenCond(ZonePlenumNum)%ActualZoneNum)%SystemZoneNodeNumber = ZoneRetPlenCond(ZonePlenumNum)%ZoneNodeNum

      ZoneRetPlenCond(ZonePlenumNum)%OutletNode = &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

      InducedNodeListName = AlphArray(5)
      NodeListError=.false.
      CALL GetNodeNums(InducedNodeListName,NumNodes,NodeNums,NodeListError,NodeType_Air,'AirLoopHVAC:ReturnPlenum', &
                       ZoneRetPlenCond(ZonePlenumNum)%ZonePlenumName,NodeConnectionType_InducedAir,1,ObjectIsNotParent,  &
                       InputFieldName=cAlphaFields(5))

      IF (.not. NodeListError) THEN
        ZoneRetPlenCond(ZonePlenumNum)%NumInducedNodes = NumNodes
        ALLOCATE(ZoneRetPlenCond(ZonePlenumNum)%InducedNode(ZoneRetPlenCond(ZonePlenumNum)%NumInducedNodes))
        ALLOCATE(ZoneRetPlenCond(ZonePlenumNum)%InducedMassFlowRate(ZoneRetPlenCond(ZonePlenumNum)%NumInducedNodes))
        ALLOCATE(ZoneRetPlenCond(ZonePlenumNum)%InducedMassFlowRateMaxAvail(ZoneRetPlenCond(ZonePlenumNum)%NumInducedNodes))
        ALLOCATE(ZoneRetPlenCond(ZonePlenumNum)%InducedMassFlowRateMinAvail(ZoneRetPlenCond(ZonePlenumNum)%NumInducedNodes))
        ALLOCATE(ZoneRetPlenCond(ZonePlenumNum)%InducedTemp(ZoneRetPlenCond(ZonePlenumNum)%NumInducedNodes))
        ALLOCATE(ZoneRetPlenCond(ZonePlenumNum)%InducedHumRat(ZoneRetPlenCond(ZonePlenumNum)%NumInducedNodes))
        ALLOCATE(ZoneRetPlenCond(ZonePlenumNum)%InducedEnthalpy(ZoneRetPlenCond(ZonePlenumNum)%NumInducedNodes))
        ALLOCATE(ZoneRetPlenCond(ZonePlenumNum)%InducedPressure(ZoneRetPlenCond(ZonePlenumNum)%NumInducedNodes))
        ZoneRetPlenCond(ZonePlenumNum)%InducedMassFlowRate = 0.0d0
        ZoneRetPlenCond(ZonePlenumNum)%InducedMassFlowRateMaxAvail = 0.0d0
        ZoneRetPlenCond(ZonePlenumNum)%InducedMassFlowRateMinAvail = 0.0d0
        ZoneRetPlenCond(ZonePlenumNum)%InducedTemp = 0.0d0
        ZoneRetPlenCond(ZonePlenumNum)%InducedHumRat = 0.0d0
        ZoneRetPlenCond(ZonePlenumNum)%InducedEnthalpy = 0.0d0
        ZoneRetPlenCond(ZonePlenumNum)%InducedPressure = 0.0d0
        DO NodeNum = 1, NumNodes
          ZoneRetPlenCond(ZonePlenumNum)%InducedNode(NodeNum) = NodeNums(NodeNum)
          UniqueNodeError=.false.
          CALL CheckUniqueNodes('Return Plenum Induced Air Nodes','NodeNumber',UniqueNodeError,CheckNumber=NodeNums(NodeNum))
          IF (UniqueNodeError) THEN
            CALL ShowContinueError('Occurs for ReturnPlenum = '//TRIM(AlphArray(1)))
            ErrorsFound=.true.
          ENDIF
          CALL PIUInducesPlenumAir(ZoneRetPlenCond(ZonePlenumNum)%InducedNode(NodeNum))
        END DO
      ELSE
        CALL ShowContinueError('Invalid Induced Air Outlet Node or NodeList name in AirLoopHVAC:ReturnPlenum object = '// &
                               TRIM(ZoneRetPlenCond(ZonePlenumNum)%ZonePlenumName))
        ErrorsFound=.true.
      ENDIF

      ZoneRetPlenCond(ZonePlenumNum)%NumInletNodes = NumAlphas - 5

      ZoneRetPlenCond%InitFlag = .TRUE.

      ALLOCATE(ZoneRetPlenCond(ZonePlenumNum)%InletNode(ZoneRetPlenCond(ZonePlenumNum)%NumInletNodes))
      ALLOCATE(ZoneRetPlenCond(ZonePlenumNum)%InletMassFlowRate(ZoneRetPlenCond(ZonePlenumNum)%NumInletNodes))
      ALLOCATE(ZoneRetPlenCond(ZonePlenumNum)%InletMassFlowRateMaxAvail(ZoneRetPlenCond(ZonePlenumNum)%NumInletNodes))
      ALLOCATE(ZoneRetPlenCond(ZonePlenumNum)%InletMassFlowRateMinAvail(ZoneRetPlenCond(ZonePlenumNum)%NumInletNodes))
      ALLOCATE(ZoneRetPlenCond(ZonePlenumNum)%InletTemp(ZoneRetPlenCond(ZonePlenumNum)%NumInletNodes))
      ALLOCATE(ZoneRetPlenCond(ZonePlenumNum)%InletHumRat(ZoneRetPlenCond(ZonePlenumNum)%NumInletNodes))
      ALLOCATE(ZoneRetPlenCond(ZonePlenumNum)%InletEnthalpy(ZoneRetPlenCond(ZonePlenumNum)%NumInletNodes))
      ALLOCATE(ZoneRetPlenCond(ZonePlenumNum)%InletPressure(ZoneRetPlenCond(ZonePlenumNum)%NumInletNodes))
      ALLOCATE(ZoneRetPlenCond(ZonePlenumNum)%ZoneEqNum(ZoneRetPlenCond(ZonePlenumNum)%NumInletNodes))

      ZoneRetPlenCond(ZonePlenumNum)%InletNode = 0
      ZoneRetPlenCond(ZonePlenumNum)%InletMassFlowRate = 0.0d0
      ZoneRetPlenCond(ZonePlenumNum)%InletMassFlowRateMaxAvail = 0.0d0
      ZoneRetPlenCond(ZonePlenumNum)%InletMassFlowRateMinAvail = 0.0d0
      ZoneRetPlenCond(ZonePlenumNum)%InletTemp = 0.0d0
      ZoneRetPlenCond(ZonePlenumNum)%InletHumRat = 0.0d0
      ZoneRetPlenCond(ZonePlenumNum)%InletEnthalpy = 0.0d0
      ZoneRetPlenCond(ZonePlenumNum)%InletPressure = 0.0d0
      ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRate = 0.0d0
      ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRateMaxAvail = 0.0d0
      ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRateMinAvail = 0.0d0
      ZoneRetPlenCond(ZonePlenumNum)%OutletTemp = 0.0d0
      ZoneRetPlenCond(ZonePlenumNum)%OutletHumRat = 0.0d0
      ZoneRetPlenCond(ZonePlenumNum)%OutletEnthalpy = 0.0d0
      ZoneRetPlenCond(ZonePlenumNum)%OutletPressure = 0.0d0
      ZoneRetPlenCond(ZonePlenumNum)%ZoneTemp = 0.0d0
      ZoneRetPlenCond(ZonePlenumNum)%ZoneHumRat = 0.0d0
      ZoneRetPlenCond(ZonePlenumNum)%ZoneEnthalpy = 0.0d0

      DO NodeNum = 1, ZoneRetPlenCond(ZonePlenumNum)%NumInletNodes

        ZoneRetPlenCond(ZonePlenumNum)%InletNode(NodeNum) = &
               GetOnlySingleNode(AlphArray(5+NodeNum),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)

      END DO

    END DO   ! end AirLoopHVAC:ReturnPlenum Loop
    CALL EndUniqueNodeCheck('AirLoopHVAC:ReturnPlenum')

    ZonePlenumNum = 0

    DO ZonePlenumLoop = 1, NumZoneSupplyPlenums
      ZonePlenumNum = ZonePlenumNum + 1

      CurrentModuleObject='AirLoopHVAC:SupplyPlenum'

      CALL GetObjectItem(CurrentModuleObject,ZonePlenumNum,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT, &
                         NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                         AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

      IsNotOK=.FALSE.
      IsBlank=.FALSE.
      CALL VerifyName(AlphArray(1),ZoneSupPlenCond%ZonePlenumName,ZonePlenumNum-1,IsNotOK,IsBlank, &
                      TRIM(CurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.TRUE.
        IF (IsBlank) AlphArray(1)='xxxxx'
      ENDIF
      ZoneSupPlenCond(ZonePlenumNum)%ZonePlenumName = AlphArray(1)

      ! Check if this zone is also used in another plenum
      IOSTAT=FindItemInList(AlphArray(2),ZoneSupPlenCond%ZoneName,ZonePlenumNum-1)
      IF (IOSTAT /= 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cAlphaFields(2))//' "'//TRIM(AlphArray(2))// &
                             '" is used more than once as a '//TRIM(CurrentModuleObject)//'.')
        CALL ShowContinueError('..Only one '//TRIM(CurrentModuleObject)//' object may be connected to a given zone.')
        CALL ShowContinueError('..occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphArray(1)))
        ErrorsFound=.true.
      ENDIF
      IF (NumZoneReturnPlenums > 0) THEN ! Check if this zone is also used in another plenum
        IOSTAT=FindItemInList(AlphArray(2),ZoneRetPlenCond%ZoneName,NumZoneReturnPlenums)
        IF (IOSTAT /= 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(cAlphaFields(2))//' "'//TRIM(AlphArray(2))//  &
                              '" is used more than once as a '//TRIM(CurrentModuleObject)//' or AirLoopHVAC:ReturnPlenum.')
          CALL ShowContinueError('..Only one '//TRIM(CurrentModuleObject)//' or AirLoopHVAC:ReturnPlenum object'// &
                                 ' may be connected to a given zone.')
          CALL ShowContinueError('..occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphArray(1)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      ZoneSupPlenCond(ZonePlenumNum)%ZoneName = AlphArray(2)
      ! put the X-Ref to the zone heat balance data structure
      ZoneSupPlenCond(ZonePlenumNum)%ActualZoneNum    = FindItemInList(AlphArray(2),Zone%Name,NumOfZones)
      IF (ZoneSupPlenCond(ZonePlenumNum)%ActualZoneNum == 0) THEN
        CALL ShowSevereError('For '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphArray(1))// &
                             ', '//TRIM(cAlphaFields(2))//' = '//TRIM(AlphArray(2))//' not found.')
        ErrorsFound=.TRUE.
        CYCLE
      ENDIF
      !  Check if this zone is used as a controlled zone
      IF (ANY(ZoneEquipConfig%IsControlled)) THEN
        ZoneEquipConfigLoop=FindItemInList(AlphArray(2),ZoneEquipConfig%ZoneName,NumOfZones)
        IF (ZoneEquipConfigLoop /= 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(cAlphaFields(2))//' "'//TRIM(AlphArray(2))//'" is a controlled zone.'// &
                               ' It cannot be used as a '//TRIM(CurrentModuleObject)//' or AirLoopHVAC:ReturnPlenum.')
          CALL ShowContinueError('..occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphArray(1)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
      ! Check if this is also used as a return plenum
      !  *** This next IF loop looks wrong.  Sent e-mail to Peter/Brent 8/14/08 for clarification ****
!      IF (NumZoneReturnPlenums > 0) THEN
!        IOSTAT=FindItemInList(AlphArray(1),ZoneRetPlenCond%ZoneName,NumZoneReturnPlenums)
!        IF (IOSTAT /= 0) THEN
!          CALL ShowSevereError(RoutineName//'Plenum "'//TRIM(AlphArray(2))//  &
!                               '" is a controlled zone.  It cannot be used as a '//  &
!                               'SUPPLY PLENUM or RETURN PLENUM.')
!          CALL ShowContinueError('..occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphArray(1)))
!          ErrorsFound=.true.
!        ENDIF
!      ENDIF

      ZoneSupPlenCond(ZonePlenumNum)%ZoneNodeName = AlphArray(3)
      ZoneSupPlenCond(ZonePlenumNum)%ZoneNodeNum = &
               GetOnlySingleNode(AlphArray(3),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                            NodeType_Air,NodeConnectionType_ZoneNode,1,ObjectIsNotParent)
      !Insert the Plenum Zone Number into the Zone Heat Balance data structure for later reference
      Zone(ZoneSupPlenCond(ZonePlenumNum)%ActualZoneNum)%SystemZoneNodeNumber = ZoneSupPlenCond(ZonePlenumNum)%ZoneNodeNum

      ZoneSupPlenCond(ZonePlenumNum)%InletNode = &
               GetOnlySingleNode(AlphArray(4),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)

      ZoneSupPlenCond(ZonePlenumNum)%NumOutletNodes = NumAlphas - 4

      ZoneSupPlenCond%InitFlag = .TRUE.

      ALLOCATE(ZoneSupPlenCond(ZonePlenumNum)%OutletNode(ZoneSupPlenCond(ZonePlenumNum)%NumOutletNodes))
      ALLOCATE(ZoneSupPlenCond(ZonePlenumNum)%OutletMassFlowRate(ZoneSupPlenCond(ZonePlenumNum)%NumOutletNodes))
      ALLOCATE(ZoneSupPlenCond(ZonePlenumNum)%OutletMassFlowRateMaxAvail(ZoneSupPlenCond(ZonePlenumNum)%NumOutletNodes))
      ALLOCATE(ZoneSupPlenCond(ZonePlenumNum)%OutletMassFlowRateMinAvail(ZoneSupPlenCond(ZonePlenumNum)%NumOutletNodes))
      ALLOCATE(ZoneSupPlenCond(ZonePlenumNum)%OutletTemp(ZoneSupPlenCond(ZonePlenumNum)%NumOutletNodes))
      ALLOCATE(ZoneSupPlenCond(ZonePlenumNum)%OutletHumRat(ZoneSupPlenCond(ZonePlenumNum)%NumOutletNodes))
      ALLOCATE(ZoneSupPlenCond(ZonePlenumNum)%OutletEnthalpy(ZoneSupPlenCond(ZonePlenumNum)%NumOutletNodes))
      ALLOCATE(ZoneSupPlenCond(ZonePlenumNum)%OutletPressure(ZoneSupPlenCond(ZonePlenumNum)%NumOutletNodes))

      ZoneSupPlenCond(ZonePlenumNum)%OutletNode = 0
      ZoneSupPlenCond(ZonePlenumNum)%OutletMassFlowRate = 0.0d0
      ZoneSupPlenCond(ZonePlenumNum)%OutletMassFlowRateMaxAvail = 0.0d0
      ZoneSupPlenCond(ZonePlenumNum)%OutletMassFlowRateMinAvail = 0.0d0
      ZoneSupPlenCond(ZonePlenumNum)%OutletTemp = 0.0d0
      ZoneSupPlenCond(ZonePlenumNum)%OutletHumRat = 0.0d0
      ZoneSupPlenCond(ZonePlenumNum)%OutletEnthalpy = 0.0d0
      ZoneSupPlenCond(ZonePlenumNum)%OutletPressure = 0.0d0
      ZoneSupPlenCond(ZonePlenumNum)%InletMassFlowRate = 0.0d0
      ZoneSupPlenCond(ZonePlenumNum)%InletMassFlowRateMaxAvail = 0.0d0
      ZoneSupPlenCond(ZonePlenumNum)%InletMassFlowRateMinAvail = 0.0d0
      ZoneSupPlenCond(ZonePlenumNum)%InletTemp = 0.0d0
      ZoneSupPlenCond(ZonePlenumNum)%InletHumRat = 0.0d0
      ZoneSupPlenCond(ZonePlenumNum)%InletEnthalpy = 0.0d0
      ZoneSupPlenCond(ZonePlenumNum)%InletPressure = 0.0d0
      ZoneSupPlenCond(ZonePlenumNum)%ZoneTemp = 0.0d0
      ZoneSupPlenCond(ZonePlenumNum)%ZoneHumRat = 0.0d0
      ZoneSupPlenCond(ZonePlenumNum)%ZoneEnthalpy = 0.0d0

      DO NodeNum = 1, ZoneSupPlenCond(ZonePlenumNum)%NumOutletNodes

        ZoneSupPlenCond(ZonePlenumNum)%OutletNode(NodeNum) = &
               GetOnlySingleNode(AlphArray(4+NodeNum),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

      END DO

    END DO   ! end AirLoopHVAC:SupplyPlenum Loop

    DEALLOCATE(AlphArray)
    DEALLOCATE(NumArray)
    DEALLOCATE(cAlphaFields)
    DEALLOCATE(cNumericFields)
    DEALLOCATE(lAlphaBlanks)
    DEALLOCATE(lNumericBlanks)
    DEALLOCATE(NodeNums)

    IF (ErrorsFound) THEN
      CALL ShowFatalError(RoutineName//'Errors found in input.  Preceding condition(s) cause termination.')
    ENDIF

  RETURN

END SUBROUTINE GetZonePlenumInput

! End of Get Input subroutines for the HB Module
!******************************************************************************


 ! Beginning Initialization Section of the Module
!******************************************************************************

SUBROUTINE InitAirZoneReturnPlenum(ZonePlenumNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   November 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the ZonePlenum components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE DataDefineEquip, ONLY: AirDistUnit, NumAirDistUnits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZonePlenumNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER    :: InletNode
  INTEGER    :: InducedNode = 0
  INTEGER    :: InletNodeLoop
  INTEGER    :: ZoneNodeNum
  INTEGER    :: NodeNum
  INTEGER    :: ZonePlenumLoop
  INTEGER    :: PlenumZoneNum
  INTEGER    :: ZoneEquipConfigLoop         ! Loop number of ZoneEquipConfig derived type
  INTEGER    :: ADUNum                      ! air distribution unit index
  INTEGER    :: NumADUsToPlen               ! number of ADUs that might leak to this plenum
  INTEGER    :: ADUsToPlenIndex             ! index of an ADU that might leak to this plenum in the plenum ADU list

  LOGICAL,SAVE   :: MyEnvrnFlag = .TRUE.
  LOGICAL,SAVE   :: MyOneTimeFlag = .true.
  ! FLOW:

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN

    ! For each zone with a return air plenum put the ZoneRetPlenCond number for the return air plenum
    ! in the ZoneEquipConfig array for the zone. This allows direct access of the zone's return air
    ! plenum conditions, such as plenum temperature and air flow. Also establish and save connections
    ! to the Air Distribution Units. This is needed for the simple duct leakage calculation.

    DO ZonePlenumLoop = 1, NumZoneReturnPlenums
      ADUsToPlenIndex = 0
      NumADUsToPlen = 0
      IF(ZoneRetPlenCond(ZonePlenumLoop)%NumInletNodes > 0) THEN
        DO InletNodeLoop = 1, ZoneRetPlenCond(ZonePlenumLoop)%NumInletNodes
          InletNode = ZoneRetPlenCond(ZonePlenumLoop)%InletNode(InletNodeLoop)
          ! Loop through ZoneEquipConfig's and look for return air node value = InletNode
          DO ZoneEquipConfigLoop = 1, NumOfZones
            IF (.not. ZoneEquipConfig(ZoneEquipConfigLoop)%IsControlled) CYCLE
            IF(ZoneEquipConfig(ZoneEquipConfigLoop)%ReturnAirNode == InletNode) THEN
              ZoneEquipConfig(ZoneEquipConfigLoop)%ReturnZonePlenumCondNum = ZonePlenumLoop
              ZoneRetPlenCond(ZonePlenumLoop)%ZoneEqNum(InletNodeLoop) = ZoneEquipConfigLoop
            END IF
          END DO
          ! count the ADUs that can leak to this plenum
          DO ADUNum=1,NumAirDistUnits
            IF (AirDistUnit(ADUNum)%ZoneEqNum == ZoneRetPlenCond(ZonePlenumLoop)%ZoneEqNum(InletNodeLoop)) THEN
              NumADUsToPlen = NumADUsToPlen + 1
            END IF
          END DO
        END DO
      END IF
      ALLOCATE(ZoneRetPlenCond(ZonePlenumLoop)%ADUIndex(NumADUsToPlen))
      ZoneRetPlenCond(ZonePlenumLoop)%NumADUs = NumADUsToPlen
      ! fill the list of air distribution units that can leak to this plenum
      IF (NumADUsToPlen > 0) THEN
        DO InletNodeLoop = 1, ZoneRetPlenCond(ZonePlenumLoop)%NumInletNodes
          DO ADUNum=1,NumAirDistUnits
            IF (AirDistUnit(ADUNum)%ZoneEqNum == ZoneRetPlenCond(ZonePlenumLoop)%ZoneEqNum(InletNodeLoop)) THEN
              ADUsToPlenIndex = ADUsToPlenIndex + 1
              ZoneRetPlenCond(ZonePlenumLoop)%ADUIndex(ADUsToPlenIndex) = ADUNum
            END IF
          END DO
        END DO
      END IF
    END DO

    MyOneTimeFlag = .false.

  END IF

  ! Do the Begin Environment initializations
  IF (MyEnvrnFlag .AND. BeginEnvrnFlag) THEN

    DO PlenumZoneNum = 1,  NumZoneReturnPlenums

      ZoneNodeNum = ZoneRetPlenCond(PlenumZoneNum)%ZoneNodeNum
      Node(ZoneNodeNum)%Temp = 20.0d0
      Node(ZoneNodeNum)%MassFlowRate = 0.0d0
      Node(ZoneNodeNum)%Quality = 1.0d0
      Node(ZoneNodeNum)%Press = OutBaroPress
      Node(ZoneNodeNum)%HumRat = OutHumRat
      Node(ZoneNodeNum)%Enthalpy = PsyHFnTdbW(Node(ZoneNodeNum)%Temp,Node(ZoneNodeNum)%HumRat)

    END DO

    MyEnvrnFlag=.FALSE.

  END IF

  IF (.NOT. BeginEnvrnFlag) THEN
    MyEnvrnFlag=.TRUE.
  END IF

  !Transfer the node data to ZoneRetPlenCond data structure
  DO NodeNum = 1, ZoneRetPlenCond(ZonePlenumNum)%NumInletNodes

    InletNode = ZoneRetPlenCond(ZonePlenumNum)%InletNode(NodeNum)
    ! Set all of the inlet mass flow variables from the nodes
    ZoneRetPlenCond(ZonePlenumNum)%InletMassFlowRate(NodeNum) = Node(InletNode)%MassFlowRate
    ZoneRetPlenCond(ZonePlenumNum)%InletMassFlowRateMaxAvail(NodeNum) = Node(InletNode)%MassFlowRateMaxAvail
    ZoneRetPlenCond(ZonePlenumNum)%InletMassFlowRateMinAvail(NodeNum) = Node(InletNode)%MassFlowRateMinAvail
!    ! Set all of the inlet state variables from the inlet nodes
!    ZoneRetPlenCond(ZonePlenumNum)%InletTemp(NodeNum)         = Node(InletNode)%Temp
!    ZoneRetPlenCond(ZonePlenumNum)%InletHumRat(NodeNum)       = Node(InletNode)%HumRat
!    ZoneRetPlenCond(ZonePlenumNum)%InletEnthalpy(NodeNum)     = Node(InletNode)%Enthalpy
    ZoneRetPlenCond(ZonePlenumNum)%InletPressure(NodeNum)     = Node(InletNode)%Press

  END DO

  ! Set the induced air flow rates
  DO NodeNum = 1, ZoneRetPlenCond(ZonePlenumNum)%NumInducedNodes
    InducedNode = ZoneRetPlenCond(ZonePlenumNum)%InducedNode(NodeNum)
    ZoneRetPlenCond(ZonePlenumNum)%InducedMassFlowRate(NodeNum) = Node(InducedNode)%MassFlowRate
    ZoneRetPlenCond(ZonePlenumNum)%InducedMassFlowRateMaxAvail(NodeNum) = Node(InducedNode)%MassFlowRateMaxAvail
    ZoneRetPlenCond(ZonePlenumNum)%InducedMassFlowRateMinAvail(NodeNum) = Node(InducedNode)%MassFlowRateMinAvail
  END DO

  ! Add stuff to calculate conduction inputs to the zone plenum
  ! Now load the zone conditions
  ZoneNodeNum = ZoneRetPlenCond(ZonePlenumNum)%ZoneNodeNum
  ZoneRetPlenCond(ZonePlenumNum)%ZoneTemp         = Node(ZoneNodeNum)%Temp
  ZoneRetPlenCond(ZonePlenumNum)%ZoneHumRat       = Node(ZoneNodeNum)%HumRat
  ZoneRetPlenCond(ZonePlenumNum)%ZoneEnthalpy     = Node(ZoneNodeNum)%Enthalpy

  RETURN

END SUBROUTINE InitAirZoneReturnPlenum


SUBROUTINE InitAirZoneSupplyPlenum(ZonePlenumNum, FirstHVACIteration, FirstCall)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   March 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the ZonePlenum components.

          ! METHODOLOGY EMPLOYED:
          ! Similar to the Zone Splitter component but with interactions to the plenum zone.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZonePlenumNum
  LOGICAL, INTENT(IN) :: FirstHVACIteration
  LOGICAL, INTENT(IN) :: FirstCall

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER    :: InletNode
  INTEGER    :: OutletNode
  INTEGER    :: ZoneNodeNum
  INTEGER    :: PlenumZoneNum
  INTEGER    :: NodeIndex

  LOGICAL,SAVE   :: MyEnvrnFlag = .TRUE.
  ! FLOW:

  ! Do the Begin Environment initializations
  IF (MyEnvrnFlag .AND. BeginEnvrnFlag) THEN

    DO PlenumZoneNum = 1,  NumZoneSupplyPlenums

      ZoneNodeNum = ZoneSupPlenCond(PlenumZoneNum)%ZoneNodeNum
      Node(ZoneNodeNum)%Temp = 20.0d0
      Node(ZoneNodeNum)%MassFlowRate = 0.0d0
      Node(ZoneNodeNum)%Quality = 1.0d0
      Node(ZoneNodeNum)%Press = OutBaroPress
      Node(ZoneNodeNum)%HumRat = OutHumRat
      Node(ZoneNodeNum)%Enthalpy = PsyHFnTdbW(Node(ZoneNodeNum)%Temp,Node(ZoneNodeNum)%HumRat)

    END DO

    MyEnvrnFlag=.FALSE.

  END IF

  IF (.NOT. BeginEnvrnFlag) THEN
    MyEnvrnFlag=.TRUE.
  END IF

  ! Do the following initializations (every time step): This should be the info from
  ! the previous components outlets or the node data in this section.

  InletNode = ZoneSupPlenCond(ZonePlenumNum)%InletNode
  ZoneNodeNum = ZoneSupPlenCond(ZonePlenumNum)%ZoneNodeNum

  IF (FirstHVACIteration .AND. FirstCall) THEN
    IF (Node(InletNode)%MassFlowRate > 0.0d0) THEN
      Node(ZoneNodeNum)%MassFlowRate = Node(InletNode)%MassFlowRate
      DO NodeIndex=1,ZoneSupPlenCond(ZonePlenumNum)%NumOutletNodes
         OutletNode = ZoneSupPlenCond(ZonePlenumNum)%OutletNode(NodeIndex)
         Node(OutletNode)%MassFlowRate = Node(InletNode)%MassFlowRate / ZoneSupPlenCond(ZonePlenumNum)%NumOutletNodes
      END DO
    END IF
    IF (Node(InletNode)%MassFlowRateMaxAvail > 0.0d0) THEN
      Node(ZoneNodeNum)%MassFlowRateMaxAvail = Node(InletNode)%MassFlowRateMaxAvail
      DO NodeIndex=1,ZoneSupPlenCond(ZonePlenumNum)%NumOutletNodes
        OutletNode = ZoneSupPlenCond(ZonePlenumNum)%OutletNode(NodeIndex)
        Node(OutletNode)%MassFlowRateMaxAvail = Node(InletNode)%MassFlowRateMaxAvail / &
                                                  ZoneSupPlenCond(ZonePlenumNum)%NumOutletNodes
      END DO
    END IF

  END IF !For FirstHVACIteration and FirstCall

  IF (FirstCall) THEN

    IF(Node(InletNode)%MassFlowRateMaxAvail == 0.0d0) THEN !For Node inlet Max Avail = 0.0

      DO NodeIndex=1,ZoneSupPlenCond(ZonePlenumNum)%NumOutletNodes
        OutletNode = ZoneSupPlenCond(ZonePlenumNum)%OutletNode(NodeIndex)
        Node(OutletNode)%MassFlowRate = 0.0d0
        Node(OutletNode)%MassFlowRateMaxAvail = 0.0d0
        Node(OutletNode)%MassFlowRateMinAvail = 0.0d0
      END DO

      Node(ZoneNodeNum)%MassFlowRate = 0.0d0
      Node(ZoneNodeNum)%MassFlowRateMaxAvail = 0.0d0
      Node(ZoneNodeNum)%MassFlowRateMinAvail = 0.0d0

    END IF !For Node inlet Max Avail = 0.0

    ! Add stuff to calculate conduction inputs to the zone plenum
    ! Now load the zone conditions
    ZoneSupPlenCond(ZonePlenumNum)%ZoneTemp         = Node(ZoneNodeNum)%Temp
    ZoneSupPlenCond(ZonePlenumNum)%ZoneHumRat       = Node(ZoneNodeNum)%HumRat
    ZoneSupPlenCond(ZonePlenumNum)%ZoneEnthalpy     = Node(ZoneNodeNum)%Enthalpy

    DO NodeIndex=1,ZoneSupPlenCond(ZonePlenumNum)%NumOutletNodes
      OutletNode = ZoneSupPlenCond(ZonePlenumNum)%OutletNode(NodeIndex)
      Node(OutletNode)%Press    = Node(InletNode)%Press
      Node(OutletNode)%Quality    = Node(InletNode)%Quality
    END DO

    Node(ZoneNodeNum)%Press    = Node(InletNode)%Press
    Node(ZoneNodeNum)%Quality    = Node(InletNode)%Quality



  ELSE  ! On the second call from the ZoneEquipManager this is where the flows are passed back to
        ! the supply plenum inlet.
    DO NodeIndex=1,ZoneSupPlenCond(ZonePlenumNum)%NumOutletNodes
      OutletNode = ZoneSupPlenCond(ZonePlenumNum)%OutletNode(NodeIndex)
      ZoneSupPlenCond(ZonePlenumNum)%OutletMassFlowRate(NodeIndex) = Node(OutletNode)%MassFlowRate
      ZoneSupPlenCond(ZonePlenumNum)%OutletMassFlowRateMaxAvail(NodeIndex) = Node(OutletNode)%MassFlowRateMaxAvail
      ZoneSupPlenCond(ZonePlenumNum)%OutletMassFlowRateMinAvail(NodeIndex) = Node(OutletNode)%MassFlowRateMinAvail
    END DO

  END IF !For FirstCall

  RETURN

END SUBROUTINE InitAirZoneSupplyPlenum

 ! End Initialization Section of the Module
!******************************************************************************


 ! Begin Algorithm Section of the Module
!******************************************************************************

SUBROUTINE CalcAirZoneReturnPlenum(ZonePlenumNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   November 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataDefineEquip, ONLY: AirDistUnit, NumAirDistUnits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZonePlenumNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: InletNodeNum = 0 ! inlet node number
  INTEGER :: IndNum = 0       ! induced air index
  INTEGER :: ADUNum = 0       ! air distribution unit number
  INTEGER :: ADUListIndex = 0 ! air distribution unit index in zone return plenum data structure
  REAL(r64) :: TotIndMassFlowRate = 0.0d0  ! total induced air mass flow rate [kg/s]


  ! Reset the totals to zero before they are summed.
  ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRate = 0.0d0
  ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRateMaxAvail = 0.0d0
  ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRateMinAvail = 0.0d0
  ZoneRetPlenCond(ZonePlenumNum)%OutletTemp = 0.0d0
  ZoneRetPlenCond(ZonePlenumNum)%OutletHumRat = 0.0d0
  ZoneRetPlenCond(ZonePlenumNum)%OutletPressure = 0.0d0
  ZoneRetPlenCond(ZonePlenumNum)%OutletEnthalpy = 0.0d0
  TotIndMassFlowRate = 0.0d0

  DO InletNodeNum = 1, ZoneRetPlenCond(ZonePlenumNum)%NumInletNodes
    ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRate = ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRate + &
                                                ZoneRetPlenCond(ZonePlenumNum)%InletMassFlowRate(InletNodeNum)
    ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRateMaxAvail = ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRateMaxAvail + &
                                                ZoneRetPlenCond(ZonePlenumNum)%InletMassFlowRateMaxAvail(InletNodeNum)
    ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRateMinAvail = ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRateMinAvail + &
                                                ZoneRetPlenCond(ZonePlenumNum)%InletMassFlowRateMinAvail(InletNodeNum)
  END DO

  IF (ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRate .GT. 0.0d0) THEN

    ! "Momentum balance" to get outlet air pressure
    DO InletNodeNum = 1, ZoneRetPlenCond(ZonePlenumNum)%NumInletNodes

      ZoneRetPlenCond(ZonePlenumNum)%OutletPressure = ZoneRetPlenCond(ZonePlenumNum)%OutletPressure + &
                                              ZoneRetPlenCond(ZonePlenumNum)%InletPressure(InletNodeNum) * &
                                              ZoneRetPlenCond(ZonePlenumNum)%InletMassFlowRate(InletNodeNum) / &
                                              ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRate
    END DO

  ELSE
    ! Mass Flow in air loop is zero and loop is not operating.
    ! Arbitrarily set the output to the first inlet leg
    ZoneRetPlenCond(ZonePlenumNum)%OutletPressure   = ZoneRetPlenCond(ZonePlenumNum)%InletPressure(1)
  END IF

  ! add in the leak flow rate, if any. Don't alter the pressure calc (it is not used anyway)
  DO ADUListIndex = 1,ZoneRetPlenCond(ZonePlenumNum)%NumADUs
    ADUNum = ZoneRetPlenCond(ZonePlenumNum)%ADUIndex(ADUListIndex)
    IF (AirDistUnit(ADUNum)%UpStreamLeak .OR. AirDistUnit(ADUNum)%DownStreamLeak) THEN
      ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRate = ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRate + &
                                                  AirDistUnit(ADUNum)%MassFlowRateUpStrLk + &
                                                  AirDistUnit(ADUNum)%MassFlowRateDnStrLk
      ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRateMaxAvail = ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRateMaxAvail + &
                                                  AirDistUnit(ADUNum)%MaxAvailDelta
      ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRateMinAvail = ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRateMinAvail + &
                                                  AirDistUnit(ADUNum)%MinAvailDelta
    END IF
  END DO
  ! Sum up induced air flow rate
  DO IndNum=1,ZoneRetPlenCond(ZonePlenumNum)%NumInducedNodes
    TotIndMassFlowRate = TotIndMassFlowRate + ZoneRetPlenCond(ZonePlenumNum)%InducedMassFlowRate(IndNum)
  END DO

  ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRate = ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRate - TotIndMassFlowRate

  ! Set the Plenum Outlet to the Zone Node conditions
  ZoneRetPlenCond(ZonePlenumNum)%OutletHumRat = ZoneRetPlenCond(ZonePlenumNum)%ZoneHumRat
  ZoneRetPlenCond(ZonePlenumNum)%OutletEnthalpy = ZoneRetPlenCond(ZonePlenumNum)%ZoneEnthalpy
  ZoneRetPlenCond(ZonePlenumNum)%OutletTemp = ZoneRetPlenCond(ZonePlenumNum)%ZoneTemp
  ! make sure the MassFlowMaxAvail >= MassFlowRate
  ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRateMaxAvail = MAX(ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRateMaxAvail, &
                                                                  ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRate)

  RETURN

END SUBROUTINE CalcAirZoneReturnPlenum


SUBROUTINE CalcAirZoneSupplyPlenum(ZonePlenumNum, FirstCall)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   March 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Similar to the Zone Splitter component but with interactions to the plenum zone.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZonePlenumNum
  LOGICAL, INTENT(IN) :: FirstCall

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NodeIndex

  ! The first time through the State properties are passed through
  IF (FirstCall) THEN
    ! Moisture balance to get outlet air humidity ratio
    DO NodeIndex=1,ZoneSupPlenCond(ZonePlenumNum)%NumOutletNodes
      ZoneSupPlenCond(ZonePlenumNum)%OutletHumRat(NodeIndex) = ZoneSupPlenCond(ZonePlenumNum)%ZoneHumRat
    END DO

    ! Energy balance to get outlet air enthalpy
    DO NodeIndex=1,ZoneSupPlenCond(ZonePlenumNum)%NumOutletNodes
      ZoneSupPlenCond(ZonePlenumNum)%OutletEnthalpy(NodeIndex) = ZoneSupPlenCond(ZonePlenumNum)%ZoneEnthalpy
    END DO

    ! Set outlet temperatures equal to inlet temperature
    DO NodeIndex=1,ZoneSupPlenCond(ZonePlenumNum)%NumOutletNodes
      ZoneSupPlenCond(ZonePlenumNum)%OutletTemp(NodeIndex) = ZoneSupPlenCond(ZonePlenumNum)%ZoneTemp
    END DO

  ELSE
    ! This is the second time through and this is where the mass flows from the outlets are
    ! summed and then assigned upstream to the inlet node.
    ZoneSupPlenCond(ZonePlenumNum)%InletMassFlowRate = 0.0d0
    ZoneSupPlenCond(ZonePlenumNum)%InletMassFlowRateMaxAvail = 0.0d0
    ZoneSupPlenCond(ZonePlenumNum)%InletMassFlowRateMinAvail = 0.0d0
    DO NodeIndex=1,ZoneSupPlenCond(ZonePlenumNum)%NumOutletNodes
      ZoneSupPlenCond(ZonePlenumNum)%InletMassFlowRate = ZoneSupPlenCond(ZonePlenumNum)%InletMassFlowRate + &
          ZoneSupPlenCond(ZonePlenumNum)%OutletMassFlowRate(NodeIndex)
      ZoneSupPlenCond(ZonePlenumNum)%InletMassFlowRateMaxAvail = ZoneSupPlenCond(ZonePlenumNum)%InletMassFlowRateMaxAvail + &
          ZoneSupPlenCond(ZonePlenumNum)%OutletMassFlowRateMaxAvail(NodeIndex)
      ZoneSupPlenCond(ZonePlenumNum)%InletMassFlowRateMinAvail = ZoneSupPlenCond(ZonePlenumNum)%InletMassFlowRateMinAvail + &
          ZoneSupPlenCond(ZonePlenumNum)%OutletMassFlowRateMinAvail(NodeIndex)
    END DO
  END IF

  RETURN

END SUBROUTINE CalcAirZoneSupplyPlenum

! End Algorithm Section of the Module
! *****************************************************************************


! Beginning of Update subroutines for the ZonePlenum Module
! *****************************************************************************

SUBROUTINE UpdateAirZoneReturnPlenum(ZonePlenumNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   November 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataContaminantBalance, ONLY: Contaminant

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZonePlenumNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: OutletNode
  INTEGER :: InletNode
  INTEGER :: ZoneNode
  INTEGER :: InletNodeNum
  INTEGER :: InducedNode  ! the node number of an induced air outlet node
  INTEGER :: IndNum       ! the induced air outlet index in ZoneRetPlenCond


  OutletNode = ZoneRetPlenCond(ZonePlenumNum)%OutletNode
  InletNode = ZoneRetPlenCond(ZonePlenumNum)%InletNode(1)
  ZoneNode = ZoneRetPlenCond(ZonePlenumNum)%ZoneNodeNum

  ! Set the outlet air nodes of the ZonePlenum
  Node(OutletNode)%MassFlowRate  = ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRate
  Node(OutletNode)%MassFlowRateMaxAvail  = ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRateMaxAvail
  Node(OutletNode)%MassFlowRateMinAvail  = ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRateMinAvail

  Node(ZoneNode)%MassFlowRate  = ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRate
  Node(ZoneNode)%MassFlowRateMaxAvail  = ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRateMaxAvail
  Node(ZoneNode)%MassFlowRateMinAvail  = ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRateMinAvail
  Node(ZoneNode)%Press = ZoneRetPlenCond(ZonePlenumNum)%OutletPressure

  Node(OutletNode)%Temp          = ZoneRetPlenCond(ZonePlenumNum)%OutletTemp
  Node(OutletNode)%HumRat        = ZoneRetPlenCond(ZonePlenumNum)%OutletHumRat
  Node(OutletNode)%Enthalpy      = ZoneRetPlenCond(ZonePlenumNum)%OutletEnthalpy
  Node(OutletNode)%Press         = ZoneRetPlenCond(ZonePlenumNum)%OutletPressure
  DO IndNum=1,ZoneRetPlenCond(ZonePlenumNum)%NumInducedNodes
    InducedNode = ZoneRetPlenCond(ZonePlenumNum)%InducedNode(IndNum)
    Node(InducedNode)%Temp = ZoneRetPlenCond(ZonePlenumNum)%InducedTemp(IndNum)
    Node(InducedNode)%HumRat = ZoneRetPlenCond(ZonePlenumNum)%InducedHumRat(IndNum)
    Node(InducedNode)%Enthalpy = ZoneRetPlenCond(ZonePlenumNum)%InducedEnthalpy(IndNum)
    Node(InducedNode)%Press = ZoneRetPlenCond(ZonePlenumNum)%InducedPressure(IndNum)
    Node(InducedNode)%Quality = Node(InletNode)%Quality
  END DO

  ! Set the outlet nodes for properties that are just pass through and not used
  Node(OutletNode)%Quality         = Node(InletNode)%Quality
  Node(ZoneNode)%Quality           = Node(InletNode)%Quality

  IF (Contaminant%CO2Simulation) Then
    If(ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRate .gt. 0.d0) Then
      ! CO2 balance to get outlet air CO2
      Node(OutletNode)%CO2 = 0.0d0
      DO InletNodeNum = 1, ZoneRetPlenCond(ZonePlenumNum)%NumInletNodes
        Node(OutletNode)%CO2 = Node(OutletNode)%CO2 + Node(ZoneRetPlenCond(ZonePlenumNum)%InletNode(InletNodeNum))%CO2 * &
           ZoneRetPlenCond(ZonePlenumNum)%InletMassFlowRate(InletNodeNum) / ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRate
      END DO
      Node(ZoneNode)%CO2 = Node(OutletNode)%CO2
    Else
      Node(OutletNode)%CO2 = Node(InletNode)%CO2
      Node(ZoneNode)%CO2   = Node(InletNode)%CO2
    End If
  End If

  IF (Contaminant%GenericContamSimulation) Then
    If(ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRate .gt. 0.d0) Then
      ! Contaminant balance to get outlet air generic contaminant
      Node(OutletNode)%GenContam = 0.0d0
      DO InletNodeNum = 1, ZoneRetPlenCond(ZonePlenumNum)%NumInletNodes
        Node(OutletNode)%GenContam =Node(OutletNode)%GenContam + &
           Node(ZoneRetPlenCond(ZonePlenumNum)%InletNode(InletNodeNum))%GenContam * &
           ZoneRetPlenCond(ZonePlenumNum)%InletMassFlowRate(InletNodeNum) / ZoneRetPlenCond(ZonePlenumNum)%OutletMassFlowRate
      END DO
      Node(ZoneNode)%GenContam = Node(OutletNode)%GenContam
    Else
      Node(OutletNode)%GenContam = Node(InletNode)%GenContam
      Node(ZoneNode)%GenContam   = Node(InletNode)%GenContam
    End If
  End If

  RETURN

END SUBROUTINE UpdateAirZoneReturnPlenum


SUBROUTINE UpdateAirZoneSupplyPlenum(ZonePlenumNum, PlenumInletChanged, FirstCall)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   March 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine needs a description.

          ! METHODOLOGY EMPLOYED:
          ! Similar to the Zone Splitter component but with interactions to the plenum zone.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataContaminantBalance, ONLY: Contaminant

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZonePlenumNum
  LOGICAL, INTENT(INOUT) :: PlenumInletChanged
  LOGICAL, INTENT(IN) :: FirstCall

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: FlowRateToler    = 0.01d0     ! Tolerance for mass flow rate convergence (in kg/s)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: OutletNode
  INTEGER :: InletNode
  INTEGER :: ZoneNode
  INTEGER :: NodeIndex


  OutletNode = ZoneSupPlenCond(ZonePlenumNum)%OutletNode(1)
  InletNode = ZoneSupPlenCond(ZonePlenumNum)%InletNode
  ZoneNode = ZoneSupPlenCond(ZonePlenumNum)%ZoneNodeNum

  ! On the FirstCall the State properties are passed through and the mass flows are not dealt with
  IF (FirstCall) THEN
    ! Set the outlet nodes for properties that just pass through and not used
    DO NodeIndex=1,ZoneSupPlenCond(ZonePlenumNum)%NumOutletNodes
      OutletNode = ZoneSupPlenCond(ZonePlenumNum)%OutletNode(NodeIndex)
      Node(OutletNode)%Temp          = ZoneSupPlenCond(ZonePlenumNum)%OutletTemp(NodeIndex)
      Node(OutletNode)%HumRat        = ZoneSupPlenCond(ZonePlenumNum)%OutletHumRat(NodeIndex)
      Node(OutletNode)%Enthalpy      = ZoneSupPlenCond(ZonePlenumNum)%OutletEnthalpy(NodeIndex)
      IF (Contaminant%CO2Simulation) Then
        Node(OutletNode)%CO2         = Node(InletNode)%CO2
      End If
      IF (Contaminant%GenericContamSimulation) Then
        Node(OutletNode)%GenContam         = Node(InletNode)%GenContam
      End If
    END DO

    IF (Contaminant%CO2Simulation) Then
      Node(ZoneNode)%CO2           = Node(InletNode)%CO2
    End If
    IF (Contaminant%GenericContamSimulation) Then
      Node(ZoneNode)%GenContam     = Node(InletNode)%GenContam
    End If

  ELSE
    ! The second time through just updates the mass flow conditions back upstream
    ! to the inlet.

   If(ABS(Node(InletNode)%MassFlowRate - ZoneSupPlenCond(ZonePlenumNum)%InletMassFlowRate).GT.FlowRateToler) Then
      PlenumInletChanged = .TRUE.
   END IF

    Node(InletNode)%MassFlowRate = ZoneSupPlenCond(ZonePlenumNum)%InletMassFlowRate
    Node(InletNode)%MassFlowRateMaxAvail = ZoneSupPlenCond(ZonePlenumNum)%InletMassFlowRateMaxAvail
    Node(InletNode)%MassFlowRateMinAvail = ZoneSupPlenCond(ZonePlenumNum)%InletMassFlowRateMinAvail

    Node(ZoneNode)%MassFlowRate = ZoneSupPlenCond(ZonePlenumNum)%InletMassFlowRate
    Node(ZoneNode)%MassFlowRateMaxAvail = ZoneSupPlenCond(ZonePlenumNum)%InletMassFlowRateMaxAvail
    Node(ZoneNode)%MassFlowRateMinAvail = ZoneSupPlenCond(ZonePlenumNum)%InletMassFlowRateMinAvail

  END IF !For FirstCall

  RETURN

END SUBROUTINE UpdateAirZoneSupplyPlenum

!        End of Update subroutines for the ZonePlenum Module
! *****************************************************************************


! Beginning of Reporting subroutines for the ZonePlenum Module
! *****************************************************************************

SUBROUTINE ReportZoneReturnPlenum(ZonePlenumNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   November 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

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
  INTEGER, INTENT(IN) :: ZonePlenumNum !unused1208

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

 ! Write(*,*)=ZoneRetPlenCond(ZonePlenumNum)%ZonePlenumPower    Still needs to report the ZonePlenum power from this component

  !ZoneRetPlenCond(ZonePlenumNum)% =

  RETURN

END SUBROUTINE ReportZoneReturnPlenum

SUBROUTINE ReportZoneSupplyPlenum(ZonePlenumNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   November 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

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
  INTEGER, INTENT(IN) :: ZonePlenumNum !unused1208

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

 ! Write(*,*)=ZoneSupPlenCond(ZonePlenumNum)%ZonePlenumPower    Still needs to report the ZonePlenum power from this component

  !ZoneSupPlenCond(ZonePlenumNum)% =

  RETURN

END SUBROUTINE ReportZoneSupplyPlenum

!        End of Reporting subroutines for the ZonePlenum Module
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

END MODULE ZonePlenum
