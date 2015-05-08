MODULE DirectAirManager

  ! Module containing the routines dealing with the DIRECT AIR
  ! component.

  ! MODULE INFORMATION:
  !       AUTHOR         Fred Buhl
  !       DATE WRITTEN   October 1999
  !       MODIFIED       Brent Griffith, May 2009 added EMS override control of flow rate
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required to
  ! simulate the direct air component  Direct air
  ! is the component used to pass supply air directly
  ! into a zone without any thermostatic control.

  ! METHODOLOGY EMPLOYED:

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals
USE DataInterfaces
USE ScheduleManager
USE DataLoopNode, ONLY: Node
Use DataEnvironment, ONLY: StdBaroPress, StdRhoAir
USE DataHVACGlobals, ONLY: SmallAirVolFlow


IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

!MODULE PARAMETER DEFINITIONS:

!Type declarations in DirectAir module
TYPE DirectAirProps
  ! Input Data
  CHARACTER(len=MaxNameLength) :: cObjectName  =' '
  CHARACTER(len=MaxNameLength) :: EquipID  =' '
  CHARACTER(len=MaxNameLength) :: Schedule =' '
  INTEGER :: ZoneSupplyAirNode             =0
  INTEGER :: SchedPtr                      =0
  REAL(r64)    :: MaxAirVolFlowRate             =0.0d0 !Max Specified Volume Flow Rate of Sys [m3/sec]
  REAL(r64)    :: AirMassFlowRateMax            =0.0d0 !Max mass flow [kg/sec]
  REAL(r64)    :: InitMaxAvailMassFlow          =0.0d0 !The Initial max mass Flow to set the Control Flow Fraction
  REAL(r64)    :: AirMassFlowFraction           =0.0d0
  Integer :: ZoneEquipAirInletNode         =0
  ! Simulation Data
  REAL(r64)    :: SensOutputProvided                =0.0d0
  LOGICAL      :: EMSOverrideAirFlow            = .FALSE.  ! if true, EMS is calling to override flow rate
  REAL(r64)    :: EMSMassFlowRateValue          = 0.0D0  ! value EMS is directing to use for flow rate [kg/s]
  !Reporting Variables
  REAL(r64)    :: HeatRate                      =0.0d0
  REAL(r64)    :: CoolRate                      =0.0d0
  REAL(r64)    :: HeatEnergy                    =0.0d0
  REAL(r64)    :: CoolEnergy                    =0.0d0
END TYPE DirectAirProps


!MODULE VARIABLE DECLARATIONS:
TYPE (DirectAirProps), ALLOCATABLE, DIMENSION(:) :: DirectAir
INTEGER                         :: NumDirectAir=0
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

!SUBROUTINE SPECIFICATIONS FOR MODULE AirLoopSplitter
PUBLIC  SimDirectAir
PRIVATE GetDirectAirInput
PRIVATE InitDirectAir
PRIVATE SizeDirectAir
PRIVATE CalcDirectAir
PRIVATE ReportDirectAir

CONTAINS

SUBROUTINE SimDirectAir(EquipName,ControlledZoneNum,FirstHVACIteration,SensOutputProvided,LatOutputProvided,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Oct 1999
          !       MODIFIED       Don Shirey, Aug 2009, LatOutputProvided
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! This subroutine manages Direct Air component simulation.
          ! It is called from SimZoneEquipment.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:

    USE InputProcessor, ONLY: FindItemInList
    USE General, ONLY: TrimSigDigits

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS
    CHARACTER(len=*), INTENT(IN) :: EquipName
    INTEGER, INTENT(IN) :: ControlledZoneNum
    REAL(r64), INTENT(INOUT) :: SensOutputProvided
    REAL(r64), INTENT(OUT)   :: LatOutputProvided ! Latent output provided (kg/s), dehumidification = negative
    LOGICAL, INTENT (IN):: FirstHVACIteration
    INTEGER, INTENT(INOUT) :: CompIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: DirectAirNum
    LOGICAL,SAVE :: GetDirectAirInputFlag = .TRUE.

IF (GetDirectAirInputFlag) THEN  !First time subroutine has been entered
  CALL GetDirectAirInput
  GetDirectAirInputFlag = .FALSE.
END IF

! Find the correct Direct Air Equipment
IF (CompIndex == 0) THEN
  DirectAirNum = FindItemInList(EquipName, DirectAir%EquipID, NumDirectAir)
  IF (DirectAirNum == 0) THEN
    CALL ShowFatalError('SimDirectAir: Unit not found='//TRIM(EquipName))
  ENDIF
  CompIndex=DirectAirNum
ELSE
  DirectAirNum=CompIndex
  IF (DirectAirNum > NumDirectAir .or. DirectAirNum < 1) THEN
    CALL ShowFatalError('SimDirectAir:  Invalid CompIndex passed='//  &
                        TRIM(TrimSigDigits(DirectAirNum))// &
                        ', Number of Units='//TRIM(TrimSigDigits(NumDirectAir))//  &
                        ', Entered Unit name='//TRIM(EquipName))
  ENDIF
  IF (CheckEquipName(DirectAirNum)) THEN
    IF (EquipName /= DirectAir(DirectAirNum)%EquipID) THEN
      CALL ShowFatalError('SimDirectAir: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(DirectAirNum))// &
                          ', Unit name='//TRIM(EquipName)//', stored Unit Name for that index='//  &
                          TRIM(DirectAir(DirectAirNum)%EquipID))
    ENDIF
    CheckEquipName(DirectAirNum)=.false.
  ENDIF
ENDIF
IF (DirectAirNum <= 0) THEN
  CALL ShowFatalError('SimDirectAir: Unit not found='//TRIM(EquipName))
ENDIF

! With the correct DirectAirNum to Initialize the system
CALL InitDirectAir(DirectAirNum,FirstHVACIteration)


CALL CalcDirectAir(DirectAirNum,ControlledZoneNum,SensOutputProvided,LatOutputProvided)

! No Update

CALL ReportDirectAir(DirectAirNum)

END SUBROUTINE SimDirectAir

SUBROUTINE GetDirectAirInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Mar 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Input the AirLoopSplitter data and store it in the AirLoopSplitter array.

          ! METHODOLOGY EMPLOYED:
          ! Use the Get routines from the InputProcessor module.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor,    ONLY: GetNumObjectsFound, GetObjectItem, VerifyName
    USE NodeInputManager,  ONLY: GetOnlySingleNode
    USE DataGlobals,       ONLY: AnyEnergyManagementSystemInModel, ScheduleAlwaysOn
    USE DataInterfaces,    ONLY: SetupOutputVariable, SetupEMSActuator, SetupEMSInternalVariable
    USE DataZoneEquipment, ONLY: ZoneEquipConfig
    USE SplitterComponent, ONLY: SplitterCond, NumSplitters
    USE DataLoopNode
    USE DataIPShortCuts


    IMPLICIT NONE

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:    INTEGER :: BaseboardNum
INTEGER :: NumNums  ! Number of REAL(r64) numbers returned by GetObjectItem
INTEGER :: NumAlphas ! Number of alphanumerics returned by GetObjectItem
INTEGER :: DirectAirNum
INTEGER :: IOSTAT
CHARACTER(len=*), PARAMETER    :: RoutineName='GetDirectAirInput: ' ! include trailing blank space
LOGICAL :: ErrorsFound=.false.
LOGICAL       :: IsNotOK               ! Flag to verify name
LOGICAL       :: IsBlank               ! Flag for blank name
INTEGER :: Loop !Do Loop Index
INTEGER :: CtrlZone                    ! controlled zome do loop index
INTEGER :: SupAirIn                    ! controlled zone supply air inlet index
INTEGER :: NodeNum
INTEGER :: SplitNum

cCurrentModuleObject='AirTerminal:SingleDuct:Uncontrolled'

NumDirectAir = GetNumObjectsFound(cCurrentModuleObject)

IF (NumDirectAir.GT.0) THEN

  ALLOCATE(DirectAir(NumDirectAir))
  ALLOCATE(CheckEquipName(NumDirectAir))
  CheckEquipName=.true.

  DO DirectAirNum=1,NumDirectAir
    DirectAir(DirectAirNum)%cObjectName = cCurrentModuleObject ! push Object Name into data array
    CALL GetObjectItem(cCurrentModuleObject,DirectAirNum,cAlphaArgs,NumAlphas,&
                       rNumericArgs,NumNums,IOSTAT,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),DirectAir%EquipID,DirectAirNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxxxxx'
    ENDIF
    DirectAir(DirectAirNum)%EquipID = cAlphaArgs(1)
    DirectAir(DirectAirNum)%Schedule = cAlphaArgs(2)
    IF (lAlphaFieldBlanks(2)) THEN
      DirectAir(DirectAirNum)%SchedPtr = ScheduleAlwaysOn
    ELSE
      DirectAir(DirectAirNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(2))
      IF (DirectAir(DirectAirNum)%SchedPtr .EQ. 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid data.')
        CALL ShowContinueError('..invalid '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
        ErrorsFound=.true.
      END IF
    END IF
                  ! Direct air is a problem for node connections since it only has a single node
                  ! make this an outlet
    DirectAir(DirectAirNum)%ZoneSupplyAirNode = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent,cAlphaFieldNames(3))
    !Load the maximum volume flow rate
    DirectAir(DirectAirNum)%MaxAirVolFlowRate = rNumericArgs(1)

  ! Fill the Zone Equipment data with the supply air inlet node number of this unit.
    DO CtrlZone = 1,NumOfZones
      IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
      DO SupAirIn = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
        IF (DirectAir(DirectAirNum)%ZoneSupplyAirNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(SupAirIn)) THEN
          ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%InNode = DirectAir(DirectAirNum)%ZoneSupplyAirNode
          ZoneEquipConfig(CtrlZone)%AirDistUnitCool(SupAirIn)%OutNode = DirectAir(DirectAirNum)%ZoneSupplyAirNode
        END IF
      END DO
    END DO

  ! Find the Zone Equipment Inlet Node from the Supply Air Path Splitter
    Do SplitNum = 1, NumSplitters
       DO NodeNum = 1, SplitterCond(SplitNum)%NumOutletNodes
         If(DirectAir(DirectAirNum)%ZoneSupplyAirNode .eq. SplitterCond(SplitNum)%OutletNode(NodeNum)) Then
            DirectAir(DirectAirNum)%ZoneEquipAirInletNode = SplitterCond(SplitNum)%InletNode
            Exit
         End If
       END DO
    End Do

  ! If no splitter, set Zone Equipment Inlet Node to the Zone Supply Air Node
    IF (NumSplitters == 0) THEN
      DirectAir(DirectAirNum)%ZoneEquipAirInletNode = DirectAir(DirectAirNum)%ZoneSupplyAirNode
    END IF

  END DO
END IF

IF (ErrorsFound) THEN
  CALL ShowFatalError(RoutineName//'Errors found in input.  Program terminates.')
ENDIF

!Setup output for the Direct Air Units.  This allows a comparison with
DO Loop=1,NumDirectAir
  CALL SetupOutputVariable('Zone Air Terminal Sensible Heating Energy [J]',DirectAir(Loop)%HeatEnergy,'System','Sum', &
                                            DirectAir(Loop)%EquipID)
  CALL SetupOutputVariable('Zone Air Terminal Sensible Cooling Energy [J]',DirectAir(Loop)%CoolEnergy,'System','Sum', &
                                            DirectAir(Loop)%EquipID)
  CALL SetupOutputVariable('Zone Air Terminal Sensible Heating Rate [W]',DirectAir(Loop)%HeatRate,'System','Average', &
                                            DirectAir(Loop)%EquipID)
  CALL SetupOutputVariable('Zone Air Terminal Sensible Cooling Rate [W]',DirectAir(Loop)%CoolRate,'System','Average', &
                                            DirectAir(Loop)%EquipID)

  IF ( AnyEnergyManagementSystemInModel ) THEN
    CALL SetupEMSActuator('AirTerminal:SingleDuct:Uncontrolled', DirectAir(Loop)%EquipID, 'Mass Flow Rate' , '[kg/s]', &
                         DirectAir(Loop)%EMSOverrideAirFlow,  DirectAir(Loop)%EMSMassFlowRateValue )
    CALL SetupEMSInternalVariable('AirTerminal:SingleDuct:Uncontrolled Maximum Mass Flow Rate',DirectAir(Loop)%EquipID, &
                 '[kg/s]',  DirectAir(Loop)%AirMassFlowRateMax  )
  ENDIF
ENDDO


RETURN
END SUBROUTINE GetDirectAirInput

! Beginning Initialization Section of the Module
!******************************************************************************

SUBROUTINE InitDirectAir(DirectAirNum,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   January 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for  initializations of the Direct Air Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger events.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use DataEnvironment, ONLY: StdBAROPRESS
  Use Psychrometrics, ONLY:PsyRhoAirFnPbTdbW
  USE DataAirflowNetwork, ONLY: SimulateAirflowNetwork,AirflowNetworkFanActivated,AirflowNetworkControlMultizone
  USE DataZoneEquipment,  ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT (IN):: FirstHVACIteration
  INTEGER, INTENT(IN) :: DirectAirNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE             :: MyOneTimeFlag = .true.
  LOGICAL,SAVE             :: ZoneEquipmentListChecked = .false.  ! True after the Zone Equipment List has been checked for items
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MySizeFlag
  Integer         :: ZoneNode
  Integer         :: Loop

          ! FLOW:
! Do the Begin Simulation initializations
  IF (MyOneTimeFlag) THEN

    ALLOCATE(MyEnvrnFlag(NumDirectAir))
    ALLOCATE(MySizeFlag(NumDirectAir))
    MyEnvrnFlag = .TRUE.
    MySizeFlag = .TRUE.

    MyOneTimeFlag = .false.

  END IF

  ! need to check all direct air units to see if they are on Zone Equipment List or issue warning
  IF (.not. ZoneEquipmentListChecked .and. ZoneEquipInputsFilled) THEN
    ZoneEquipmentListChecked=.true.
    DO Loop=1,NumDirectAir
      IF (CheckZoneEquipmentList(DirectAir(DirectAirNum)%cObjectName,DirectAir(Loop)%EquipID)) CYCLE
      CALL ShowWarningError('InitDirectAir: ['//TRIM(DirectAir(DirectAirNum)%cObjectName)//' = '//TRIM(DirectAir(Loop)%EquipID)// &
           '] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.')
    ENDDO
  ENDIF
  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(DirectAirNum) ) THEN

    CALL SizeDirectAir(DirectAirNum)

    MySizeFlag(DirectAirNum) = .FALSE.
  END IF
! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(DirectAirNum)) THEN

  ! Calculate the Max Mass flow rate using the air density at Standard Conditions
    DirectAir(DirectAirNum)%AirMassFlowRateMax = DirectAir(DirectAirNum)%MaxAirVolFlowRate * StdRhoAir

    Node(DirectAir(DirectAirNum)%ZoneSupplyAirNode)%MassFlowRateMax = &
                                   DirectAir(DirectAirNum)%AirMassFlowRateMax
    Node(DirectAir(DirectAirNum)%ZoneSupplyAirNode)%MassFlowRateMin = 0.0d0

    MyEnvrnFlag(DirectAirNum) = .FALSE.
  END IF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(DirectAirNum) = .true.
  ENDIF

 ! Set the ZoneNode number
  ZoneNode = DirectAir(DirectAirNum)%ZoneSupplyAirNode
  IF (FirstHVACIteration) THEN
     !The first time through set the mass flow rate to the Max
    IF ((Node(ZoneNode)%MassFlowRateMaxAvail > 0.0d0) .AND.  &
       (GetCurrentScheduleValue(DirectAir(DirectAirNum)%SchedPtr) .gt. 0.0d0)) THEN
      IF (.NOT. (SimulateAirflowNetwork .GT. AirflowNetworkControlMultizone .AND. AirflowNetworkFanActivated)) THEN
       Node(ZoneNode)%MassFlowRate = DirectAir(DirectAirNum)%AirMassFlowRateMax
        Node(ZoneNode)%MassFlowRateMaxAvail = DirectAir(DirectAirNum)%AirMassFlowRateMax
        IF (DirectAir(DirectAirNum)%EMSOverrideAirFlow) &
            Node(ZoneNode)%MassFlowRate = DirectAir(DirectAirNum)%EMSMassFlowRateValue
      ENDIF
      Node(ZoneNode)%MassFlowRateMinAvail = 0.0d0
    ELSE
      Node(ZoneNode)%MassFlowRate = 0.0d0
      Node(ZoneNode)%MassFlowRateMaxAvail = 0.0d0
    END IF
  ELSE
  !When not FirstHCAVIteration
    IF (.NOT. DirectAir(DirectAirNum)%EMSOverrideAirFlow) THEN
      IF ((Node(ZoneNode)%MassFlowRateMaxAvail > 0.0d0) .AND.  &
         (GetCurrentScheduleValue(DirectAir(DirectAirNum)%SchedPtr) > 0.0d0)) THEN
        IF (Node(ZoneNode)%MassFlowRateMaxAvail < Node(ZoneNode)%MassFlowRateMax) THEN
          Node(ZoneNode)%MassFlowRate = Node(ZoneNode)%MassFlowRateMaxAvail
        ELSE IF (Node(ZoneNode)%MassFlowRateMinAvail > Node(ZoneNode)%MassFlowRateMin) THEN
          Node(ZoneNode)%MassFlowRate = Node(ZoneNode)%MassFlowRateMinAvail
        ELSE
          Node(ZoneNode)%MassFlowRate = Node(ZoneNode)%MassFlowRateMaxAvail
        END IF
      ELSE
        Node(ZoneNode)%MassFlowRate = 0.0d0
        Node(ZoneNode)%MassFlowRateMaxAvail = 0.0d0
      END IF
    ELSE ! EMS override on
      Node(ZoneNode)%MassFlowRate = DirectAir(DirectAirNum)%EMSMassFlowRateValue
      ! but also apply constraints
      Node(ZoneNode)%MassFlowRate = MIN(Node(ZoneNode)%MassFlowRate , Node(ZoneNode)%MassFlowRateMaxAvail)
      Node(ZoneNode)%MassFlowRate = MIN(Node(ZoneNode)%MassFlowRate , Node(ZoneNode)%MassFlowRateMax)
      Node(ZoneNode)%MassFlowRate = MAX(Node(ZoneNode)%MassFlowRate , Node(ZoneNode)%MassFlowRateMinAvail)
      Node(ZoneNode)%MassFlowRate = MAX(Node(ZoneNode)%MassFlowRate , Node(ZoneNode)%MassFlowRateMin)

    ENDIF

  END IF

 !Set reporting varialbes to zero for the Direct Air Output
 DirectAir(DirectAirNum)%HeatRate = 0.0d0
 DirectAir(DirectAirNum)%CoolRate = 0.0d0
 DirectAir(DirectAirNum)%HeatEnergy = 0.0d0
 DirectAir(DirectAirNum)%CoolEnergy = 0.0d0


  RETURN

END SUBROUTINE InitDirectAir

SUBROUTINE SizeDirectAir(DirectAirNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   January 2002
          !       MODIFIED       August 2013 Daeho Kang, add component sizing table entries 
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Direct Air Components for which flow rates have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone arrays.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE InputProcessor
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE General,             ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: DirectAirNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)  :: MaxAirVolFlowRateDes   ! Design maximum air volume flow rate for reporting
  REAL(r64)  :: MaxAirVolFlowRateUser  ! User hard-sized maximum air volume flow rate for reporting
  LOGICAL    :: IsAutosize           ! Indicator to autosizing max air flow rate
  LOGICAL    :: SizingDesRunThisZone
  
  IsAutosize = .FALSE.
  MaxAirVolFlowRateDes = 0.0d0
  MaxAirVolFlowRateUser = 0.0d0
  SizingDesRunThisZone = .FALSE.

  IF (CurZoneEqNum > 0) THEN
      
    IF (DirectAir(DirectAirNum)%MaxAirVolFlowRate == AutoSize) THEN  
      IsAutosize = .TRUE.
    END IF
    CALL CheckThisZoneForSizing(CurZoneEqNum, SizingDesRunThisZone)

        ! Check if all are hard-sized
    IF (.NOT. IsAutosize .AND. .NOT. SizingDesRunThisZone) THEN ! simulation should continue   
      IF (DirectAir(DirectAirNum)%MaxAirVolFlowRate > 0.0d0) THEN  
        CALL ReportSizingOutput(TRIM(DirectAir(DirectAirNum)%cObjectName), DirectAir(DirectAirNum)%EquipID, &
                              'User-Specified Maximum Air Flow Rate [m3/s]', DirectAir(DirectAirNum)%MaxAirVolFlowRate)
      END IF  
    ELSE ! Autosize or hard-size with design run      
      CALL CheckZoneSizing(TRIM(DirectAir(DirectAirNum)%cObjectName), DirectAir(DirectAirNum)%EquipID)      
      MaxAirVolFlowRateDes = MAX(TermUnitFinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                                                  TermUnitFinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)      
      IF (MaxAirVolFlowRateDes < SmallAirVolFlow) THEN
      MaxAirVolFlowRateDes = 0.0d0
      END IF  
      IF (IsAutoSize) THEN      
        DirectAir(DirectAirNum)%MaxAirVolFlowRate = MaxAirVolFlowRateDes
        CALL ReportSizingOutput(DirectAir(DirectAirNum)%cObjectName, DirectAir(DirectAirNum)%EquipID, &
                              'Design Size Maximum Air Flow Rate [m3/s]', MaxAirVolFlowRateDes)
      ELSE ! Hard-size with sizing data
        IF (DirectAir(DirectAirNum)%MaxAirVolFlowRate > 0.0d0 .AND. MaxAirVolFlowRateDes > 0.0d0 &
          .AND. SizingDesRunThisZone) THEN
          MaxAirVolFlowRateUser = DirectAir(DirectAirNum)%MaxAirVolFlowRate
          CALL ReportSizingOutput(DirectAir(DirectAirNum)%cObjectName, DirectAir(DirectAirNum)%EquipID, &
                              'Design Size Maximum Air Flow Rate [m3/s]', MaxAirVolFlowRateDes,  &
                              'User-Specified Maximum Air Flow Rate [m3/s]', MaxAirVolFlowRateUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MaxAirVolFlowRateDes - MaxAirVolFlowRateUser)/MaxAirVolFlowRateUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeDirectAir: Potential issue with equipment sizing for AirTerminal:SingleDuct:Uncontrolled="' &
                                     //    TRIM(DirectAir(DirectAirNum)%EquipID)//'".')
              CALL ShowContinueError('User-Specified Maximum Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(MaxAirVolFlowRateUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Maximum Air Flow Rate of ' // &
                                      TRIM(RoundSigDigits(MaxAirVolFlowRateDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            ENDIF
          ENDIF
        END IF          
      END IF
    END IF
  END IF  

  RETURN

END SUBROUTINE SizeDirectAir

! End Initialization Section of the Module
!******************************************************************************


SUBROUTINE CalcDirectAir(DirectAirNum,ControlledZoneNum,SensOutputProvided,LatOutputProvided)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Oct 1999
          !       MODIFIED       Shirey, Aug 2009 (LatOutputProvided)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Calculate the system SensOutputProvided and LatOutputProvided by the direct supply air connection

          ! METHODOLOGY EMPLOYED:
          ! Enthalpy balance

          ! REFERENCES:

          ! USE STATEMENTS:
    USE DataZoneEquipment, ONLY: ZoneEquipConfig
    USE DataHVACGlobals, ONLY: SmallMassFlow
    USE Psychrometrics, ONLY: PsyHFnTdbW

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS
    INTEGER, INTENT(IN) :: DirectAirNum
    INTEGER, INTENT(IN) :: ControlledZoneNum
    REAL(r64), INTENT(INOUT) :: SensOutputProvided
    REAL(r64), INTENT(OUT)   :: LatOutputProvided ! Latent output provided, kg/s, dehumidification = negative

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: MassFlowRate ! Air mass flow rate in kg/s
    REAL(r64) :: SpecHumOut   ! Specific humidity ratio of outlet air (kg moisture / kg moist air)
    REAL(r64) :: SpecHumIn    ! Specific humidity ratio of inlet [zone] air (kg moisture / kg moist air)

        ! Sign convention: SysSensOutputProvided <0 Zone is cooled
        !                  SysSensOutputProvided >0 Zone is heated
        !                  SysLatOutputProvided <0 Zone is dehumidified
        !                  SysLatOutputProvided >0 Zone is humidified


  MassFlowRate = Node(DirectAir(DirectAirNum)%ZoneSupplyAirNode)%MassFlowRate

  IF (GetCurrentScheduleValue(DirectAir(DirectAirNum)%SchedPtr).GT. 0.0d0 &
      .AND. MassFlowRate.GT.SmallMassFlow) THEN

!  Change this later ... should be using minimum humidity ratio in the calculation of enthalpy
!    MinHumRat = MIN(Node(ZoneEquipConfig(ControlledZoneNum)%ZoneNode)%HumRat,   &
!                         Node(DirectAir(DirectAirNum)%ZoneSupplyAirNode)%HumRat)

    SensOutputProvided = MassFlowRate * (PsyHFnTdbW(Node(DirectAir(DirectAirNum)%ZoneSupplyAirNode)%Temp, &
                                            Node(ZoneEquipConfig(ControlledZoneNum)%ZoneNode)%HumRat) &
                                   - PsyHFnTdbW(Node(ZoneEquipConfig(ControlledZoneNum)%ZoneNode)%Temp, &
                                            Node(ZoneEquipConfig(ControlledZoneNum)%ZoneNode)%HumRat))

!   CR9155 Remove specific humidity calculations
    SpecHumOut = Node(DirectAir(DirectAirNum)%ZoneSupplyAirNode)%HumRat
    SpecHumIn  = Node(ZoneEquipConfig(ControlledZoneNum)%ZoneNode)%HumRat

    LatOutputProvided = MassFlowRate * (SpecHumOut - SpecHumIn) ! Latent rate, kg/s

  ELSE
    SensOutputProvided = 0.0d0
    LatOutputProvided  = 0.0d0
  END IF

  DirectAir(DirectAirNum)%SensOutputProvided = SensOutputProvided

  RETURN

END SUBROUTINE CalcDirectAir


SUBROUTINE ReportDirectAir(DirectAirNum)
            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Russ Taylor
            !       DATE WRITTEN:    Nov 1997

            ! PURPOSE OF THIS SUBROUTINE: This subroutine

            ! METHODOLOGY EMPLOYED:

            ! REFERENCES:

            ! USE STATEMENTS:
  Use DataHVACGlobals, ONLY: TimeStepSys

  IMPLICIT NONE

  INTEGER :: DirectAirNum

!report the Direct Air Output
 DirectAir(DirectAirNum)%HeatRate = MAX(DirectAir(DirectAirNum)%SensOutputProvided,0.0d0)
 DirectAir(DirectAirNum)%CoolRate = Abs(MIN(DirectAir(DirectAirNum)%SensOutputProvided,0.0d0))
 DirectAir(DirectAirNum)%HeatEnergy = MAX(DirectAir(DirectAirNum)%SensOutputProvided,0.0d0)*TimeStepSys*SecInHour
 DirectAir(DirectAirNum)%CoolEnergy = Abs(MIN(DirectAir(DirectAirNum)%SensOutputProvided,0.0d0)*TimeStepSys*SecInHour)


  RETURN
END SUBROUTINE ReportDirectAir

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

END MODULE DirectAirManager
