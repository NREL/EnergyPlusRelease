 MODULE ZoneAirLoopEquipmentManager
   ! Module containing the routines dealing with the ZoneAirLoopEquipmentManager

  ! MODULE INFORMATION:
  !       AUTHOR         Russ Taylor
  !       DATE WRITTEN   May 1997
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! Needs description

  ! METHODOLOGY EMPLOYED: none

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
    USE DataPrecisionGlobals
    USE DataGlobals, ONLY: NumOfZones, BeginEnvrnFlag, BeginDayFlag, &
                           BeginHourFlag, BeginTimeStepFlag, MaxNameLength
    USE DataInterfaces, ONLY: ShowFatalError, SetupOutputVariable, ShowSevereError, ShowContinueError

    USE DataHVACGlobals, ONLY: FirstTimeStepSysFlag
    USE DataDefineEquip

  ! Use statements for access to subroutines in other modules

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS:
LOGICAL :: GetAirDistUnitsFlag = .TRUE.  ! If TRUE, Air Distribution Data has not been read in yet

  ! DERIVED TYPE DEFINITIONS:
  ! na

  ! MODULE VARIABLE DECLARATIONS:
  ! na

  ! SUBROUTINE SPECIFICATIONS FOR MODULE ZoneAirLoopEquipmentManager
PUBLIC ManageZoneAirLoopEquipment

CONTAINS


SUBROUTINE ManageZoneAirLoopEquipment(ZoneAirLoopEquipName, FirstHVACIteration, &
                                      SysOutputProvided, NonAirSysOutput, LatOutputProvided, ActualZoneNum, &
                                      ControlledZoneNum, CompIndex)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   May 1997
          !       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calls the zone thermal control simulations and the interfaces
          ! (water-air, refrigerant-air, steam-air, electric-electric,
          ! water-water, etc)

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY:  MaxNameLength
  USE General, ONLY: TrimSigDigits
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ZoneAirLoopEquipName
  LOGICAL, INTENT(IN) :: FirstHVACIteration
  REAL(r64)           :: SysOutputProvided
  REAL(r64), INTENT(OUT) :: LatOutputProvided   ! Latent add/removal supplied by window AC (kg/s), dehumid = negative
  REAL(r64)           :: NonAirSysOutput
  INTEGER, INTENT(IN) :: ActualZoneNum
  INTEGER             :: ControlledZoneNum
  INTEGER             :: CompIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL             :: SimZone
  INTEGER :: AirDistUnitNum

  ! Beginning of Code

  CALL GetZoneAirLoopEquipment

  ! Find the correct Zone Air Distribution Unit Equipment
  IF (CompIndex == 0) THEN
    AirDistUnitNum = FindItemInList(ZoneAirLoopEquipName, AirDistUnit%Name, NumAirDistUnits)
    IF (AirDistUnitNum == 0) THEN
      CALL ShowFatalError('ManageZoneAirLoopEquipment: Unit not found='//TRIM(ZoneAirLoopEquipName))
    ENDIF
    CompIndex=AirDistUnitNum
  ELSE
    AirDistUnitNum=CompIndex
    IF (AirDistUnitNum > NumAirDistUnits .or. AirDistUnitNum < 1) THEN
      CALL ShowFatalError('ManageZoneAirLoopEquipment:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(AirDistUnitNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumAirDistUnits))//  &
                          ', Entered Unit name='//TRIM(ZoneAirLoopEquipName))
    ENDIF
    IF (ZoneAirLoopEquipName /= AirDistUnit(AirDistUnitNum)%Name) THEN
      CALL ShowFatalError('ManageZoneAirLoopEquipment: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(AirDistUnitNum))// &
                          ', Unit name='//TRIM(ZoneAirLoopEquipName)//', stored Unit Name for that index='//  &
                          TRIM(AirDistUnit(AirDistUnitNum)%Name))
    ENDIF
  ENDIF

  CALL InitZoneAirLoopEquipment(FirstHVACIteration,AirDistUnitNum)

  CALL SimZoneAirLoopEquipment(AirDistUnitNum, SysOutputProvided, NonAirSysOutput, LatOutputProvided, &
                                   FirstHVACIteration, ControlledZoneNum, ActualZoneNum)

    !  CALL RecordZoneAirLoopEquipment

    !  CALL ReportZoneAirLoopEquipment

  SimZone = .False.

  RETURN

END SUBROUTINE ManageZoneAirLoopEquipment

SUBROUTINE GetZoneAirLoopEquipment

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   June 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Get all the system related equipment which may be attached to
          ! a zone

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,        ONLY: GetNumObjectsFound, GetObjectItem, GetObjectItemNum, VerifyName, SameString
  USE NodeInputManager,      ONLY: GetOnlySingleNode
  USE DataLoopNode
  USE BranchNodeConnections, ONLY: SetUpCompSets
  USE DataZoneEquipment,     ONLY: ZoneEquipConfig,ZoneEquipList
  USE DualDuct,              ONLY: GetDualDuctOutdoorAirRecircUse
  USE SingleDuct,            ONLY: GetATMixerPriNode

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: RoutineName='GetZoneAirLoopEquipment: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: AirDistUnitNum
  INTEGER :: AirDistCompUnitNum
  INTEGER :: ZoneEqNum           ! zone equip config index
  INTEGER :: InletNum            ! zone equip config inlet node index
  INTEGER :: NumAlphas
  INTEGER :: NumNums
  INTEGER :: IOSTAT
  CHARACTER(len=MaxNameLength), DIMENSION(4) :: AlphArray
  REAL(r64), DIMENSION(2) :: NumArray
  LOGICAL :: ErrorsFound = .false.   ! If errors detected in input
  LOGICAL       :: IsNotOK               ! Flag to verify name
  LOGICAL       :: IsBlank               ! Flag for blank name
  CHARACTER(len=MaxNameLength) :: CurrentModuleObject   ! Object type for getting and error messages
  CHARACTER(len=MaxNameLength), DIMENSION(4) :: cAlphaFields   ! Alpha field names
  CHARACTER(len=MaxNameLength), DIMENSION(2) :: cNumericFields ! Numeric field names
  LOGICAL, DIMENSION(4) :: lAlphaBlanks     ! Logical array, alpha field input BLANK = .true.
  LOGICAL, DIMENSION(2) :: lNumericBlanks   ! Logical array, numeric field input BLANK = .true.
  LOGICAL :: DualDuctRecircIsUsed   !local temporary for deciding if recirc side used by dual duct terminal
  INTEGER :: ATMixerPriNode=0       ! primary air inlet node for air terminal mixers
  
    ! make sure the input data is read in only once
    IF (.not. GetAirDistUnitsFlag) THEN
      RETURN
    ELSE
      GetAirDistUnitsFlag = .FALSE.
    END IF

    CurrentModuleObject = 'ZoneHVAC:AirDistributionUnit'

    NumAirDistUnits = GetNumObjectsFound(CurrentModuleObject)

    ALLOCATE (AirDistUnit(NumAirDistUnits))       !

    IF (NumAirDistUnits .GT. 0)THEN

      DO AirDistUnitNum = 1,  NumAirDistUnits
        CALL GetObjectItem(CurrentModuleObject,AirDistUnitNum,AlphArray,NumAlphas, &
                           NumArray,NumNums,IOSTAT,NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields) !  data for one zone

        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(AlphArray(1),AirDistUnit%Name,AirDistUnitNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) AlphArray(1)='xxxxx'
        ENDIF
        AirDistUnit(AirDistUnitNum)%Name = AlphArray(1)
        !Input Outlet Node Num
        AirDistUnit(AirDistUnitNum)%OutletNodeNum    = &
             GetOnlySingleNode(AlphArray(2),ErrorsFound,TRIM(CurrentModuleObject),AlphArray(1), &
                               NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)
        AirDistUnit(AirDistUnitNum)%InletNodeNum = 0
        AirDistUnit(AirDistUnitNum)%NumComponents = 1
        AirDistCompUnitNum=1
        !Load the air Distribution Unit Equip and Name
        AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum) = AlphArray(3)
        AirDistUnit(AirDistUnitNum)%EquipName(AirDistCompUnitNum) = AlphArray(4)
        CALL ValidateComponent(AlphArray(3),AlphArray(4),IsNotOK,TRIM(CurrentModuleObject))
        IF (IsNotOK) THEN
          CALL ShowContinueError('In '//TRIM(CurrentModuleObject)//' = '//TRIM(AlphArray(1)))
          ErrorsFound=.true.
        ENDIF
        AirDistUnit(AirDistUnitNum)%UpStreamLeakFrac = NumArray(1)
        AirDistUnit(AirDistUnitNum)%DownStreamLeakFrac = NumArray(2)
        IF (AirDistUnit(AirDistUnitNum)%DownStreamLeakFrac <= 0.0d0) THEN
          AirDistUnit(AirDistUnitNum)%LeakLoadMult = 1.0d0
        ELSE IF (AirDistUnit(AirDistUnitNum)%DownStreamLeakFrac < 1.0d0 .AND. &
                 AirDistUnit(AirDistUnitNum)%DownStreamLeakFrac > 0.0d0) THEN
          AirDistUnit(AirDistUnitNum)%LeakLoadMult = 1.0d0/(1.0d0-AirDistUnit(AirDistUnitNum)%DownStreamLeakFrac)
        ELSE
          CALL ShowSevereError('Error found in '//TRIM(CurrentModuleObject)//' = '//TRIM(AirDistUnit(AirDistUnitNum)%Name))
          CALL ShowContinueError(TRIM(cNumericFields(2))//' must be less than 1.0')
          ErrorsFound=.true.
        END IF
        IF (AirDistUnit(AirDistUnitNum)%UpStreamLeakFrac > 0.0d0) THEN
          AirDistUnit(AirDistUnitNum)%UpStreamLeak = .TRUE.
        ELSE
          AirDistUnit(AirDistUnitNum)%UpStreamLeak = .FALSE.
        END IF
        IF (AirDistUnit(AirDistUnitNum)%DownStreamLeakFrac > 0.0d0) THEN
          AirDistUnit(AirDistUnitNum)%DownStreamLeak = .TRUE.
        ELSE
          AirDistUnit(AirDistUnitNum)%DownStreamLeak = .FALSE.
        END IF

        ! Validate EquipType for Air Distribution Unit
        IF (SameString(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum),'AirTerminal:DualDuct:ConstantVolume')) THEN
          AirDistUnit(AirDistUnitNum)%EquipType_Num(AirDistCompUnitNum)=DualDuctConstVolume
          IF (AirDistUnit(AirDistUnitNum)%UpStreamLeak .or. AirDistUnit(AirDistUnitNum)%DownStreamLeak) THEN
            CALL ShowSevereError('Error found in '//TRIM(CurrentModuleObject)//' = '//TRIM(AirDistUnit(AirDistUnitNum)%Name))
            CALL ShowContinueError('Simple duct leakage model not available for '//TRIM(cAlphaFields(3))//' = '//  &
                                    TRIM(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum)))
            ErrorsFound=.true.
          END IF
        ELSEIF (SameString(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum),'AirTerminal:DualDuct:VAV')) THEN
          AirDistUnit(AirDistUnitNum)%EquipType_Num(AirDistCompUnitNum)=DualDuctVAV
          IF (AirDistUnit(AirDistUnitNum)%UpStreamLeak .or. AirDistUnit(AirDistUnitNum)%DownStreamLeak) THEN
            CALL ShowSevereError('Error found in '//TRIM(CurrentModuleObject)//' = '//TRIM(AirDistUnit(AirDistUnitNum)%Name))
            CALL ShowContinueError('Simple duct leakage model not available for '//TRIM(cAlphaFields(3))//' = '//  &
                                    TRIM(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum)))
            ErrorsFound=.true.
          END IF
        ELSEIF (SameString(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum),'AirTerminal:DualDuct:VAV:OutdoorAir')) THEN
          AirDistUnit(AirDistUnitNum)%EquipType_Num(AirDistCompUnitNum)=DualDuctVAVOutdoorAir
          IF (AirDistUnit(AirDistUnitNum)%UpStreamLeak .or. AirDistUnit(AirDistUnitNum)%DownStreamLeak) THEN
            CALL ShowSevereError('Error found in '//TRIM(CurrentModuleObject)//' = '//TRIM(AirDistUnit(AirDistUnitNum)%Name))
            CALL ShowContinueError('Simple duct leakage model not available for '//TRIM(cAlphaFields(3))//' = '//  &
                                    TRIM(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum)))
            ErrorsFound=.true.
          END IF
        ELSEIF (SameString(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum),  &
                          'AirTerminal:SingleDuct:ConstantVolume:Reheat')) THEN
          AirDistUnit(AirDistUnitNum)%EquipType_Num(AirDistCompUnitNum)=SingleDuctConstVolReheat
        ELSEIF (SameString(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum),'AirTerminal:SingleDuct:VAV:Reheat')) THEN
          AirDistUnit(AirDistUnitNum)%EquipType_Num(AirDistCompUnitNum)=SingleDuctVAVReheat
        ELSEIF (SameString(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum),'AirTerminal:SingleDuct:VAV:NoReheat')) THEN
          AirDistUnit(AirDistUnitNum)%EquipType_Num(AirDistCompUnitNum)=SingleDuctVAVNoReheat
        ELSEIF (SameString(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum),  &
                           'AirTerminal:SingleDuct:VAV:HeatAndCool:Reheat')) THEN
          AirDistUnit(AirDistUnitNum)%EquipType_Num(AirDistCompUnitNum)=SingleDuctCBVAVReheat
        ELSEIF (SameString(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum),  &
                          'AirTerminal:SingleDuct:VAV:HeatAndCool:NoReheat')) THEN
          AirDistUnit(AirDistUnitNum)%EquipType_Num(AirDistCompUnitNum)=SingleDuctCBVAVNoReheat
        ELSEIF (SameString(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum),  &
                           'AirTerminal:SingleDuct:SeriesPIU:Reheat')) THEN
          AirDistUnit(AirDistUnitNum)%EquipType_Num(AirDistCompUnitNum)=SingleDuct_SeriesPIU_Reheat
          IF (AirDistUnit(AirDistUnitNum)%UpStreamLeak .or. AirDistUnit(AirDistUnitNum)%DownStreamLeak) THEN
            CALL ShowSevereError('Error found in '//TRIM(CurrentModuleObject)//' = '//TRIM(AirDistUnit(AirDistUnitNum)%Name))
            CALL ShowContinueError('Simple duct leakage model not available for '//TRIM(cAlphaFields(3))//' = '//  &
                                    TRIM(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum)))
            ErrorsFound=.true.
          END IF
        ELSEIF (SameString(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum),  &
                           'AirTerminal:SingleDuct:ParallelPIU:Reheat')) THEN
          AirDistUnit(AirDistUnitNum)%EquipType_Num(AirDistCompUnitNum)=SingleDuct_ParallelPIU_Reheat
          IF (AirDistUnit(AirDistUnitNum)%UpStreamLeak .or. AirDistUnit(AirDistUnitNum)%DownStreamLeak) THEN
            CALL ShowSevereError('Error found in '//TRIM(CurrentModuleObject)//' = '//TRIM(AirDistUnit(AirDistUnitNum)%Name))
            CALL ShowContinueError('Simple duct leakage model not available for '//TRIM(cAlphaFields(3))//' = '//  &
                                TRIM(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum)))
            ErrorsFound=.true.
          END IF
        ELSEIF (SameString(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum),  &
                           'AirTerminal:SingleDuct:ConstantVolume:FourPipeInduction')) THEN
          AirDistUnit(AirDistUnitNum)%EquipType_Num(AirDistCompUnitNum)=SingleDuct_ConstVol_4PipeInduc
          IF (AirDistUnit(AirDistUnitNum)%UpStreamLeak .or. AirDistUnit(AirDistUnitNum)%DownStreamLeak) THEN
            CALL ShowSevereError('Error found in '//TRIM(CurrentModuleObject)//' = '//TRIM(AirDistUnit(AirDistUnitNum)%Name))
            CALL ShowContinueError('Simple duct leakage model not available for '//TRIM(cAlphaFields(3))//' = '//  &
                                TRIM(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum)))
            ErrorsFound=.true.
          END IF
        ELSEIF (SameString(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum),  &
                           'AirTerminal:SingleDuct:VAV:Reheat:VariableSpeedFan')) THEN
          AirDistUnit(AirDistUnitNum)%EquipType_Num(AirDistCompUnitNum)=SingleDuctVAVReheatVSFan
          IF (AirDistUnit(AirDistUnitNum)%UpStreamLeak .or. AirDistUnit(AirDistUnitNum)%DownStreamLeak) THEN
            CALL ShowSevereError('Error found in '//TRIM(CurrentModuleObject)//' = '//TRIM(AirDistUnit(AirDistUnitNum)%Name))
            CALL ShowContinueError('Simple duct leakage model not available for '//TRIM(cAlphaFields(3))//' = '//  &
                                TRIM(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum)))
            ErrorsFound=.true.
          END IF
          ELSEIF (SameString(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum),  &
                           'AirTerminal:SingleDuct:ConstantVolume:CooledBeam')) THEN
          AirDistUnit(AirDistUnitNum)%EquipType_Num(AirDistCompUnitNum)=SingleDuctConstVolCooledBeam
          IF (AirDistUnit(AirDistUnitNum)%UpStreamLeak .or. AirDistUnit(AirDistUnitNum)%DownStreamLeak) THEN
            CALL ShowSevereError('Error found in '//TRIM(CurrentModuleObject)//' = '//TRIM(AirDistUnit(AirDistUnitNum)%Name))
            CALL ShowContinueError('Simple duct leakage model not available for '//TRIM(cAlphaFields(3))//' = '//  &
                                TRIM(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum)))
            ErrorsFound=.true.
          END IF
        ELSEIF (SameString(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum),'AirTerminal:SingleDuct:UserDefined')) THEN
          AirDistUnit(AirDistUnitNum)%EquipType_Num(AirDistCompUnitNum)=SingleDuctUserDefined
        ELSEIF (SameString(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum),'AirTerminal:SingleDuct:InletSideMixer')) THEN
          AirDistUnit(AirDistUnitNum)%EquipType_Num(AirDistCompUnitNum)=SingleDuctInletATMixer
          CALL GetATMixerPriNode(AirDistUnit(AirDistUnitNum)%EquipName(1),ATMixerPriNode)
          AirDistUnit(AirDistUnitNum)%InletNodeNum = ATMixerPriNode
          IF (AirDistUnit(AirDistUnitNum)%UpStreamLeak .or. AirDistUnit(AirDistUnitNum)%DownStreamLeak) THEN
            CALL ShowSevereError('Error found in '//TRIM(CurrentModuleObject)//' = '//TRIM(AirDistUnit(AirDistUnitNum)%Name))
            CALL ShowContinueError('Simple duct leakage model not available for '//TRIM(cAlphaFields(3))//' = '//  &
                                    TRIM(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum)))
            ErrorsFound=.true.
          END IF
        ELSEIF (SameString(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum),'AirTerminal:SingleDuct:SupplySideMixer')) THEN
          AirDistUnit(AirDistUnitNum)%EquipType_Num(AirDistCompUnitNum)=SingleDuctSupplyATMixer
          CALL GetATMixerPriNode(AirDistUnit(AirDistUnitNum)%EquipName(1),ATMixerPriNode)
          AirDistUnit(AirDistUnitNum)%InletNodeNum = ATMixerPriNode
          IF (AirDistUnit(AirDistUnitNum)%UpStreamLeak .or. AirDistUnit(AirDistUnitNum)%DownStreamLeak) THEN
            CALL ShowSevereError('Error found in '//TRIM(CurrentModuleObject)//' = '//TRIM(AirDistUnit(AirDistUnitNum)%Name))
            CALL ShowContinueError('Simple duct leakage model not available for '//TRIM(cAlphaFields(3))//' = '//  &
                                    TRIM(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum)))
            ErrorsFound=.true.
          END IF
        ELSE
          CALL ShowSevereError('Error found in '//TRIM(CurrentModuleObject)//' = '//TRIM(AirDistUnit(AirDistUnitNum)%Name))
          CALL ShowContinueError('Invalid '//TRIM(cAlphaFields(3))//' = '//  &
                                TRIM(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum)))
          ErrorsFound=.true.
        ENDIF

        ! Set up component set for air terminal unit
        IF ((AirDistUnit(AirDistUnitNum)%EquipType_Num(AirDistCompUnitNum) == DualDuctConstVolume) .OR. &
            (AirDistUnit(AirDistUnitNum)%EquipType_Num(AirDistCompUnitNum) == DualDuctVAV)) THEN
        !  For dual duct units, set up two component sets, one for heat and one for cool
          CALL SetUpCompSets(TRIM(CurrentModuleObject), AirDistUnit(AirDistUnitNum)%Name, &
                             TRIM(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum))//':HEAT', &
                             AirDistUnit(AirDistUnitNum)%EquipName(AirDistCompUnitNum), &
                             'UNDEFINED',AlphArray(2))
          CALL SetUpCompSets(TRIM(CurrentModuleObject), AirDistUnit(AirDistUnitNum)%Name, &
                             TRIM(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum))//':COOL', &
                             AirDistUnit(AirDistUnitNum)%EquipName(AirDistCompUnitNum), &
                             'UNDEFINED',AlphArray(2))
        !  For dual duct units with decoupled OA and RA, set up two component sets, one for OA (Outdoor Air)
        !  and one for RA (Recirculated Air)
        ELSEIF ((AirDistUnit(AirDistUnitNum)%EquipType_Num(AirDistCompUnitNum) == DualDuctVAVOutdoorAir)) THEN
          CALL SetUpCompSets(TRIM(CurrentModuleObject), AirDistUnit(AirDistUnitNum)%Name, &
                             TRIM(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum))//':OutdoorAir', &
                             AirDistUnit(AirDistUnitNum)%EquipName(AirDistCompUnitNum), &
                             'UNDEFINED',AlphArray(2))
          CALL GetDualDuctOutdoorAirRecircUse( AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum) , &
                                               AirDistUnit(AirDistUnitNum)%EquipName(AirDistCompUnitNum) , &
                                               DualDuctRecircIsUsed )
          IF ( DualDuctRecircIsUsed ) THEN
            CALL SetUpCompSets(TRIM(CurrentModuleObject), AirDistUnit(AirDistUnitNum)%Name, &
                             TRIM(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum))//':RecirculatedAir', &
                             AirDistUnit(AirDistUnitNum)%EquipName(AirDistCompUnitNum), &
                             'UNDEFINED',AlphArray(2))
          ENDIF
        ELSE
          CALL SetUpCompSets(TRIM(CurrentModuleObject), AirDistUnit(AirDistUnitNum)%Name, &
                             AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompUnitNum), &
                             AirDistUnit(AirDistUnitNum)%EquipName(AirDistCompUnitNum), &
                             'UNDEFINED',AlphArray(2))
        ENDIF

        ! find and save corresponding zone equip config
        DO ZoneEqNum=1,NumOfZones
          IF (.not. ZoneEquipConfig(ZoneEqNum)%IsControlled) CYCLE
          DO InletNum=1,ZoneEquipConfig(ZoneEqNum)%NumInletNodes
            IF (ZoneEquipConfig(ZoneEqNum)%InletNode(InletNum) == AirDistUnit(AirDistUnitNum)%OutletNodeNum) THEN
              AirDistUnit(AirDistUnitNum)%ZoneEqNum = ZoneEqNum
            END IF
          END DO
        END DO

        IF ( AirDistUnit(AirDistUnitNum)%UpStreamLeak .or. AirDistUnit(AirDistUnitNum)%DownStreamLeak ) THEN
          ZoneEquipConfig(AirDistUnit(AirDistUnitNum)%ZoneEqNum)%SupLeakToRetPlen = .TRUE.
        END IF

      END DO !End of Air Dist Do Loop

    END IF
    IF (ErrorsFound) THEN
      CALL ShowFatalError(RoutineName//'Errors found in getting '//TRIM(CurrentModuleObject)//' Input')
    ENDIF

  RETURN

END SUBROUTINE GetZoneAirLoopEquipment


SUBROUTINE InitZoneAirLoopEquipment(FirstHVACIteration, AirDistUnitNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   Nov 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is left for Module format consistency -- not needed in this module.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN) :: FirstHVACIteration !unused1208
  INTEGER, INTENT(IN) :: AirDistUnitNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  ! every time step
  AirDistUnit(AirDistUnitNum)%MassFlowRateDnStrLk = 0.0d0
  AirDistUnit(AirDistUnitNum)%MassFlowRateTU = 0.0d0
  AirDistUnit(AirDistUnitNum)%MassFlowRateZSup = 0.0d0
  AirDistUnit(AirDistUnitNum)%MassFlowRateSup = 0.0d0

  RETURN

END SUBROUTINE InitZoneAirLoopEquipment

SUBROUTINE SimZoneAirLoopEquipment(AirDistUnitNum, SysOutputProvided, NonAirSysOutput, LatOutputProvided, &
                                   FirstHVACIteration, ControlledZoneNum, ActualZoneNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   May 1997
          !       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulates primary system air supplied to a zone and calculates
          ! airflow requirements

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: ZoneEquipConfig, ZoneEquipList
  USE DataLoopNode, ONLY: Node
  USE DataAirLoop, ONLY: AirLoopFlow
  USE DualDuct,   ONLY: SimulateDualDuct
  USE SingleDuct, ONLY: SimulateSingleDuct
  USE PoweredInductionUnits, ONLY: SimPIU
  USE Psychrometrics, ONLY:PsyCpAirFnWTdb
  USE HVACSingleDuctInduc, ONLY: SimIndUnit
  USE HVACCooledBeam, ONLY: SimCoolBeam
  USE UserDefinedComponents, ONLY: SimAirTerminalUserDefined

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: AirDistUnitNum
  REAL(r64), INTENT(INOUT) :: SysOutputProvided
  REAL(r64), INTENT(OUT)   :: NonAirSysOutput
  REAL(r64), INTENT(OUT)   :: LatOutputProvided ! Latent add/removal provided by this unit (kg/s), dehumidify = negative
  LOGICAL :: FirstHVACIteration
  INTEGER :: ControlledZoneNum
  INTEGER, INTENT(IN) :: ActualZoneNum

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: AirDistCompNum
  INTEGER :: InNodeNum  ! air distribution unit inlet node
  INTEGER :: OutNodeNum ! air distribution unit outlet node
  INTEGER :: AirLoopNum=0 ! index of air loop
  REAL(r64) :: CpAirZn
  REAL(r64) :: CpAirSys
  REAL(r64) :: MassFlowRateMaxAvail        ! max avail mass flow rate excluding leaks [kg/s]
  REAL(r64) :: MassFlowRateMinAvail        ! min avail mass flow rate excluding leaks [kg/s]
  REAL(r64) :: MassFlowRateUpStreamLeakMax ! max upstream leak flow rate [kg/s]
  REAL(r64) :: DesFlowRatio=0.0d0            ! ratio of system to sum of zones design flow rate
  REAL(r64) :: SpecHumOut=0.0d0              ! Specific humidity ratio of outlet air (kg moisture / kg moist air)
  REAL(r64) :: SpecHumIn=0.0d0               ! Specific humidity ratio of inlet air (kg moisture / kg moist air)

      DO AirDistCompNum = 1, AirDistUnit(AirDistUnitNum)%NumComponents
         NonAirSysOutput = 0.0d0
         InNodeNum = AirDistUnit(AirDistUnitNum)%InletNodeNum
         OutNodeNum = AirDistUnit(AirDistUnitNum)%OutletNodeNum
         MassFlowRateMaxAvail = 0.0d0
         MassFlowRateMinAvail = 0.0d0
         ! check for no plenum
         ! set the max and min avail flow rates taking into acount the upstream leak
         IF (InNodeNum > 0) THEN
           IF (AirDistUnit(AirDistUnitNum)%UpStreamLeak) THEN
             MassFlowRateMaxAvail = Node(InNodeNum)%MassFlowRateMaxAvail
             MassFlowRateMinAvail = Node(InNodeNum)%MassFlowRateMinAvail
             AirLoopNum = ZoneEquipConfig(ControlledZoneNum)%AirLoopNum
             IF (AirLoopNum > 0) THEN
               DesFlowRatio = AirLoopFlow(AirLoopNum)%SysToZoneDesFlowRatio
             ELSE
               DesFlowRatio = 1.0d0
             END IF
             MassFlowRateUpStreamLeakMax = AirDistUnit(AirDistUnitNum)%UpStreamLeakFrac*Node(InNodeNum)%MassFlowRateMax*DesFlowRatio
             IF (MassFlowRateMaxAvail > MassFlowRateUpStreamLeakMax) THEN
               AirDistUnit(AirDistUnitNum)%MassFlowRateUpStrLk = MassFlowRateUpStreamLeakMax
               Node(InNodeNum)%MassFlowRateMaxAvail = MassFlowRateMaxAvail - MassFlowRateUpStreamLeakMax
             ELSE
               AirDistUnit(AirDistUnitNum)%MassFlowRateUpStrLk = MassFlowRateMaxAvail
               Node(InNodeNum)%MassFlowRateMaxAvail = 0.0d0
             END IF
             Node(InNodeNum)%MassFlowRateMinAvail = MAX(0.0d0, MassFlowRateMinAvail &
                                                             -AirDistUnit(AirDistUnitNum)%MassFlowRateUpStrLk)
           END IF
         END IF

         SELECT CASE (AirDistUnit(AirDistUnitNum)%EquipType_Num(AirDistCompNum))

          CASE (DualDuctConstVolume)
            CALL SimulateDualDuct(AirDistUnit(AirDistUnitNum)%EquipName(AirDistCompNum),FirstHVACIteration, &
                                           ActualZoneNum, ZoneEquipConfig(ControlledZoneNum)%ZoneNode,      &
                                           AirDistUnit(AirDistUnitNum)%EquipIndex(AirDistCompNum))

          CASE (DualDuctVAV)
            CALL SimulateDualDuct(AirDistUnit(AirDistUnitNum)%EquipName(AirDistCompNum),FirstHVACIteration, &
                                           ActualZoneNum, ZoneEquipConfig(ControlledZoneNum)%ZoneNode,      &
                                           AirDistUnit(AirDistUnitNum)%EquipIndex(AirDistCompNum))

          CASE (DualDuctVAVOutdoorAir)
            CALL SimulateDualDuct(AirDistUnit(AirDistUnitNum)%EquipName(AirDistCompNum),FirstHVACIteration, &
                                           ActualZoneNum, ZoneEquipConfig(ControlledZoneNum)%ZoneNode,      &
                                           AirDistUnit(AirDistUnitNum)%EquipIndex(AirDistCompNum))

          CASE (SingleDuctVAVReheat)
            CALL SimulateSingleDuct(AirDistUnit(AirDistUnitNum)%EquipName(AirDistCompNum),FirstHVACIteration, &
                                    ActualZoneNum, ZoneEquipConfig(ControlledZoneNum)%ZoneNode,      &
                                           AirDistUnit(AirDistUnitNum)%EquipIndex(AirDistCompNum))

          CASE (SingleDuctCBVAVReheat)
            CALL SimulateSingleDuct(AirDistUnit(AirDistUnitNum)%EquipName(AirDistCompNum),FirstHVACIteration, &
                                    ActualZoneNum, ZoneEquipConfig(ControlledZoneNum)%ZoneNode,      &
                                           AirDistUnit(AirDistUnitNum)%EquipIndex(AirDistCompNum))

          CASE (SingleDuctVAVNoReheat)
            CALL SimulateSingleDuct(AirDistUnit(AirDistUnitNum)%EquipName(AirDistCompNum),FirstHVACIteration, &
                                    ActualZoneNum, ZoneEquipConfig(ControlledZoneNum)%ZoneNode,      &
                                           AirDistUnit(AirDistUnitNum)%EquipIndex(AirDistCompNum))

          CASE (SingleDuctCBVAVNoReheat)
            CALL SimulateSingleDuct(AirDistUnit(AirDistUnitNum)%EquipName(AirDistCompNum),FirstHVACIteration, &
                                    ActualZoneNum, ZoneEquipConfig(ControlledZoneNum)%ZoneNode,      &
                                           AirDistUnit(AirDistUnitNum)%EquipIndex(AirDistCompNum))

          CASE (SingleDuctConstVolReheat)
            CALL SimulateSingleDuct(AirDistUnit(AirDistUnitNum)%EquipName(AirDistCompNum),FirstHVACIteration, &
                                    ActualZoneNum, ZoneEquipConfig(ControlledZoneNum)%ZoneNode,      &
                                           AirDistUnit(AirDistUnitNum)%EquipIndex(AirDistCompNum))

          CASE (SingleDuct_SeriesPIU_Reheat)
            CALL SimPIU(AirDistUnit(AirDistUnitNum)%EquipName(AirDistCompNum),FirstHVACIteration, &
                                    ActualZoneNum, ZoneEquipConfig(ControlledZoneNum)%ZoneNode,      &
                                           AirDistUnit(AirDistUnitNum)%EquipIndex(AirDistCompNum))

          CASE (SingleDuct_ParallelPIU_Reheat)
            CALL SimPIU(AirDistUnit(AirDistUnitNum)%EquipName(AirDistCompNum),FirstHVACIteration, &
                                    ActualZoneNum, ZoneEquipConfig(ControlledZoneNum)%ZoneNode,      &
                                           AirDistUnit(AirDistUnitNum)%EquipIndex(AirDistCompNum))

          CASE(SingleDuct_ConstVol_4PipeInduc)
            CALL SimIndUnit(AirDistUnit(AirDistUnitNum)%EquipName(AirDistCompNum), FirstHVACIteration, &
                            ActualZoneNum, ZoneEquipConfig(ControlledZoneNum)%ZoneNode,      &
                                           AirDistUnit(AirDistUnitNum)%EquipIndex(AirDistCompNum))

          CASE(SingleDuctVAVReheatVSFan)
            CALL SimulateSingleDuct(AirDistUnit(AirDistUnitNum)%EquipName(AirDistCompNum),FirstHVACIteration, &
                                    ActualZoneNum, ZoneEquipConfig(ControlledZoneNum)%ZoneNode,      &
                                           AirDistUnit(AirDistUnitNum)%EquipIndex(AirDistCompNum))

         CASE(SingleDuctConstVolCooledBeam)
            CALL SimCoolBeam(AirDistUnit(AirDistUnitNum)%EquipName(AirDistCompNum),FirstHVACIteration, &
                             ActualZoneNum, ZoneEquipConfig(ControlledZoneNum)%ZoneNode,      &
                             AirDistUnit(AirDistUnitNum)%EquipIndex(AirDistCompNum), NonAirSysOutput)

          CASE (SingleDuctUserDefined)
            CALL SimAirTerminalUserDefined(AirDistUnit(AirDistUnitNum)%EquipName(AirDistCompNum),FirstHVACIteration, &
                                    ActualZoneNum, ZoneEquipConfig(ControlledZoneNum)%ZoneNode,      &
                                           AirDistUnit(AirDistUnitNum)%EquipIndex(AirDistCompNum))
                                           
          CASE (SingleDuctInletATMixer)
          
          CASE (SingleDuctSupplyATMixer)

          CASE DEFAULT
            CALL ShowSevereError('Error found in ZoneHVAC:AirDistributionUnit='//TRIM(AirDistUnit(AirDistUnitNum)%Name))
            CALL ShowContinueError('Invalid Component='//TRIM(AirDistUnit(AirDistUnitNum)%EquipType(AirDistCompNum)))
            CALL ShowFatalError('Preceding condition causes termination.')

        END SELECT

        ! do leak mass flow calcs
         IF (InNodeNum > 0) THEN
           IF (AirDistUnit(AirDistUnitNum)%UpStreamLeak) THEN
             Node(InNodeNum)%MassFlowRateMaxAvail = MassFlowRateMaxAvail
             Node(InNodeNum)%MassFlowRateMinAvail = MassFlowRateMinAvail
           END IF
           IF ( (AirDistUnit(AirDistUnitNum)%UpStreamLeak .OR. AirDistUnit(AirDistUnitNum)%DownStreamLeak) .AND. &
                MassFlowRateMaxAvail > 0.0d0 ) THEN
             AirDistUnit(AirDistUnitNum)%MassFlowRateTU = Node(InNodeNum)%MassFlowRate
             AirDistUnit(AirDistUnitNum)%MassFlowRateZSup = AirDistUnit(AirDistUnitNum)%MassFlowRateTU * &
                                                            (1.0d0-AirDistUnit(AirDistUnitNum)%DownStreamLeakFrac)
             AirDistUnit(AirDistUnitNum)%MassFlowRateDnStrLk = AirDistUnit(AirDistUnitNum)%MassFlowRateTU * &
                                                              AirDistUnit(AirDistUnitNum)%DownStreamLeakFrac
             AirDistUnit(AirDistUnitNum)%MassFlowRateSup = AirDistUnit(AirDistUnitNum)%MassFlowRateTU &
                                                           + AirDistUnit(AirDistUnitNum)%MassFlowRateUpStrLk
             Node(InNodeNum)%MassFlowRate = AirDistUnit(AirDistUnitNum)%MassFlowRateSup
             Node(OutNodeNum)%MassFlowRate = AirDistUnit(AirDistUnitNum)%MassFlowRateZSup
             Node(OutNodeNum)%MassFlowRateMaxAvail = MAX(0.0d0, MassFlowRateMaxAvail &
                                                     - AirDistUnit(AirDistUnitNum)%MassFlowRateDnStrLk &
                                                     - AirDistUnit(AirDistUnitNum)%MassFlowRateUpStrLk)
             Node(OutNodeNum)%MassFlowRateMinAvail = MAX(0.0d0, MassFlowRateMinAvail &
                                                     - AirDistUnit(AirDistUnitNum)%MassFlowRateDnStrLk &
                                                     - AirDistUnit(AirDistUnitNum)%MassFlowRateUpStrLk)
             AirDistUnit(AirDistUnitNum)%MaxAvailDelta = MassFlowRateMaxAvail - Node(OutNodeNum)%MassFlowRateMaxAvail
             AirDistUnit(AirDistUnitNum)%MinAvailDelta = MassFlowRateMinAvail - Node(OutNodeNum)%MassFlowRateMinAvail
           END IF
         END IF

      END DO
      ! Sign convention: SysOutputProvided <0 Zone is cooled
      !                  SysOutputProvided >0 Zone is heated
      CpAirZn = PsyCpAirFnWTdb(Node(ZoneEquipConfig(ControlledZoneNum)%ZoneNode)%HumRat, &
                        Node(ZoneEquipConfig(ControlledZoneNum)%ZoneNode)%Temp)
      CpAirSys = PsyCpAirFnWTdb(Node(AirDistUnit(AirDistUnitNum)%OutletNodeNum)%Humrat,  &
                         Node(AirDistUnit(AirDistUnitNum)%OutletNodeNum)%Temp)
      SysOutputProvided = Node(AirDistUnit(AirDistUnitNum)%OutletNodeNum)%MassFlowRate * &
                (CpAirSys*Node(AirDistUnit(AirDistUnitNum)%OutletNodeNum)%Temp - &
                CpAirZn*Node(ZoneEquipConfig(ControlledZoneNum)%ZoneNode)%Temp)

      ! Sign convention: LatOutputProvided <0 Zone is dehumidified
      !                  LatOutputProvided >0 Zone is humidified
      ! CR9155 Remove specific humidity calculations
      SpecHumOut = Node(AirDistUnit(AirDistUnitNum)%OutletNodeNum)%HumRat
      SpecHumIn  = Node(ZoneEquipConfig(ControlledZoneNum)%ZoneNode)%HumRat
      LatOutputProvided = Node(AirDistUnit(AirDistUnitNum)%OutletNodeNum)%MassFlowRate * &
                          (SpecHumOut - SpecHumIn) ! Latent rate (kg/s), dehumid = negative

  RETURN

END SUBROUTINE SimZoneAirLoopEquipment

SUBROUTINE UpdateZoneAirLoopEquipment
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   Nov 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is left for Module format consistency -- not needed in this module.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
  RETURN

END SUBROUTINE UpdateZoneAirLoopEquipment

SUBROUTINE ReportZoneAirLoopEquipment
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   Nov 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is left for Module format consistency -- not needed in this module.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na


  RETURN
END SUBROUTINE ReportZoneAirLoopEquipment


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

END MODULE ZoneAirLoopEquipmentManager

