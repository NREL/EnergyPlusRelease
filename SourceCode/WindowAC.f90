MODULE WindowAC

  ! Module containing the routines dealing window air conditioner units

  ! MODULE INFORMATION:
  !       AUTHOR         Fred Buhl
  !       DATE WRITTEN   May 2000
  !       MODIFIED       Richard Raustad, FSEC Oct 2003
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms needed to simulate window air
  ! conditioner units.

  ! METHODOLOGY EMPLOYED:
  ! Units are modeled as a collection of components: outside air mixer,
  ! fan and DX coil. Control is by means of cycling: either continuous
  ! air flow with the DX compressor cycling on/off or the entire unit -
  ! fan and compressor cycling on/off. Cycling behavior is not explicitly
  ! modeled - instead cycling inefficiencies must be included in the efficiency
  ! curves of the DX module.

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataSizing
USE DataGlobals,     ONLY: BeginEnvrnFlag, BeginDayFlag, MaxNameLength, SecInHour, OutputFileDebug, SysSizingCalc, &
                               DisplayExtraWarnings
USE DataInterfaces
Use DataEnvironment, ONLY: OutBaroPress, OutDryBulbTemp, OutRelHum, StdBaroPress, StdRhoAir
USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad, FanElecPower, DXElecCoolingPower, OnOffFanPartLoadFraction, &
                           SmallAirVolFlow, CoilDX_CoolingSingleSpeed,CoilDX_CoolingHXAssisted, &
                           CycFanCycCoil, ContFanCycCoil, DrawThru, BlowThru, SingleHeatingSetPoint

  ! Use statements for access to subroutines in other modules
USE ScheduleManager
USE Psychrometrics, ONLY:PsyRhoAirFnPbTdbW,PsyCpAirFnWTdb,PsyHFnTdbW

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
INTEGER, PARAMETER :: WindowAC_UnitType = 1
CHARACTER(len=*), PARAMETER :: cWindowAC_UnitType='ZoneHVAC:WindowAirConditioner'
CHARACTER(len=*), PARAMETER, DIMENSION(1) :: cWindowAC_UnitTypes = (/  &
               cWindowAC_UnitType/)

! Compressor operation
INTEGER, PARAMETER :: On =  1              ! normal compressor operation
INTEGER, PARAMETER :: Off = 0              ! signal DXCoil that compressor shouldn't run

  ! DERIVED TYPE DEFINITIONS
TYPE WindACData
  ! input data
  CHARACTER(len=MaxNameLength) :: Name             =' '  ! name of unit
!  CHARACTER(len=MaxNameLength) :: UnitType         =' '  ! type of unit
  INTEGER                      :: UnitType         =0    ! type of unit
  CHARACTER(len=MaxNameLength) :: Sched            =' '  ! availability schedule
  INTEGER                      :: SchedPtr         =0    ! index to schedule
  INTEGER                      :: FanSchedPtr      =0    ! index to fan operating mode schedule
  INTEGER                      :: FanAvailSchedPtr = 0   ! index to fan availability schedule
  REAL(r64)                    :: MaxAirVolFlow    =0.0d0  ! m3/s
  REAL(r64)                    :: MaxAirMassFlow   =0.0d0  ! kg/s
  REAL(r64)                    :: OutAirVolFlow    =0.0d0  ! m3/s
  REAL(r64)                    :: OutAirMassFlow   =0.0d0  ! kg/s
  INTEGER                      :: AirInNode        =0    ! inlet air node number
  INTEGER                      :: AirOutNode       =0    ! outlet air node number
  INTEGER                      :: OutsideAirNode   =0    ! outside air node number
  INTEGER                      :: AirReliefNode    =0    ! relief air node number
  INTEGER                      :: MixedAirNode     =0    ! Mixed Air Node number
  CHARACTER(len=MaxNameLength) :: OAMixName        =' '  ! name of outdoor air mixer
  CHARACTER(len=MaxNameLength) :: OAMixType        =' '  ! type of outdoor air mixer
  INTEGER                      :: OAMixIndex       = 0
  CHARACTER(len=MaxNameLength) :: FanName          =' '  ! name of fan
  CHARACTER(len=MaxNameLength) :: FanType          =' '  ! type of fan
  INTEGER                      :: FanType_Num      =0    ! index to fan type
  INTEGER                      :: FanIndex         =0
  CHARACTER(len=MaxNameLength) :: DXCoilName       =' '  ! name of cooling coil
  CHARACTER(len=MaxNameLength) :: DXCoilType       =' '  ! type of cooling coil,Coil:DX:CoolingBypassFactorEmpirical or
                                                         ! 'CoilSystem:Cooling:DX:HeatExchangerAssisted'
  INTEGER                      :: DXCoilType_Num   =0    ! Numeric Equivalent for DXCoil Type
  INTEGER                      :: DXCoilIndex      =0    ! Index to DX cooling coil
  INTEGER                      :: CoilOutletNodeNum=0    ! Outlet node number of DX cooling coil
  INTEGER :: OpMode            =0 ! mode of operation; 1=cycling fan, cycling compressor,
                                  ! 2=continuous fan, cycling compresor
  INTEGER :: FanPlace          =0 ! fan placement; 1=blow through, 2=draw through
  INTEGER :: MaxIterIndex1     =0
  INTEGER :: MaxIterIndex2     =0
  REAL(r64)                    :: ConvergenceTol    =0.0d0 ! Convergence tolerance, fraction (ZoneLoad - Equip Output)/ZoneLoad
  ! Calc data
  REAL(r64)                    :: PartLoadFrac      =0.0d0 ! part load fraction for the unit
  Logical                      :: EMSOverridePartLoadFrac = .FALSE.
  REAL(r64)                    :: EMSValueForPartLoadFrac = 0.0D0 !
  ! Report data
  REAL(r64)                    :: TotCoolEnergyRate =0.0d0 ! total cooling output [W]
  REAL(r64)                    :: TotCoolEnergy     =0.0d0 ! total cooling output [J]
  REAL(r64)                    :: SensCoolEnergyRate=0.0d0 ! sensible cooling output [W]
  REAL(r64)                    :: SensCoolEnergy    =0.0d0 ! sensible cooling output [J]
  REAL(r64)                    :: LatCoolEnergyRate =0.0d0 ! sensible cooling output [W]
  REAL(r64)                    :: LatCoolEnergy     =0.0d0 ! sensible cooling output [J]
  REAL(r64)                    :: ElecPower         =0.0d0 ! electricity consumed [W]
  REAL(r64)                    :: ElecConsumption   =0.0d0 ! electricity consumed [J]
  REAL(r64)                    :: FanPartLoadRatio  =0.0d0 ! fan part-load ratio for time step
  REAL(r64)                    :: CompPartLoadRatio =0.0d0 ! compressor part-load ratio for time step
  CHARACTER(len=MaxNameLength) :: AvailManagerListName = ' ' ! Name of an availability manager list object
  INTEGER                      :: AvailStatus          = 0
END TYPE WindACData

  ! MODULE VARIABLE DECLARATIONS:
TYPE (WindACData), ALLOCATABLE, DIMENSION(:)         :: WindAC

INTEGER                            :: NumWindAC=0
INTEGER                            :: NumWindACCyc=0
LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlag
LOGICAL :: GetWindowACInputFlag = .TRUE.  ! First time, input is "gotten"
LOGICAL :: CoolingLoad          = .FALSE. ! defines a cooling load
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

  ! SUBROUTINE SPECIFICATIONS FOR MODULE

PUBLIC  SimWindowAC
PRIVATE GetWindowAC
PRIVATE InitWindowAC
PRIVATE SizeWindowAC
PRIVATE SimCyclingWindowAC
PRIVATE ControlCycWindACOutput
PRIVATE CalcWindowACOutput
PRIVATE ReportWindowAC
PUBLIC  GetWindowACOutAirNode
PUBLIC  GetWindowACReturnAirNode
PUBLIC  GetWindowACMixedAirNode
PUBLIC  GetWindowACZoneInletAirNode

CONTAINS

SUBROUTINE SimWindowAC(CompName,ZoneNum,FirstHVACIteration,PowerMet,LatOutputProvided,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Manages the simulation of a window AC unit. Called from SimZone Equipment

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,               ONLY: TrimSigDigits
  USE InputProcessor,        ONLY: FindItemInList
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand
  USE DataHeatBalFanSys,     ONLY: TempControlType

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT (IN)  :: CompName            ! name of the window AC unit
  INTEGER,          INTENT (IN)  :: ZoneNum             ! number of zone being served
  LOGICAL,          INTENT (IN)  :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep
  REAL(r64),        INTENT (OUT) :: PowerMet            ! Sensible power supplied by window AC (W)
  REAL(r64),        INTENT (OUT) :: LatOutputProvided   ! Latent add/removal supplied by window AC (kg/s), dehumid = negative
  INTEGER,          INTENT(INOUT):: CompIndex           ! component index

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: WindACNum              ! index of window AC unit being simulated
  REAL(r64)    :: QZnReq                 ! zone load (W)
  REAL(r64)    :: RemainingOutputToCoolingSP !- remaining load to cooling setpoint (W)


          ! FLOW

! First time SimWindowAC is called, get the input for all the window AC units
IF (GetWindowACInputFlag) THEN
  CALL GetWindowAC
  GetWindowACInputFlag = .FALSE.
END IF

! Find the correct Window AC Equipment
IF (CompIndex == 0) THEN
  WindACNum = FindItemInList(CompName,WindAC%Name,NumWindAC)
  IF (WindACNum == 0) THEN
    CALL ShowFatalError('SimWindowAC: Unit not found='//TRIM(CompName))
  ENDIF
  CompIndex=WindACNum
ELSE
  WindACNum=CompIndex
  IF (WindACNum > NumWindAC .or. WindACNum < 1) THEN
    CALL ShowFatalError('SimWindowAC:  Invalid CompIndex passed='//  &
                        TRIM(TrimSigDigits(WindACNum))// &
                        ', Number of Units='//TRIM(TrimSigDigits(NumWindAC))//  &
                        ', Entered Unit name='//TRIM(CompName))
  ENDIF
  IF (CheckEquipName(WindACNum)) THEN
    IF (CompName /= WindAC(WindACNum)%Name) THEN
      CALL ShowFatalError('SimWindowAC: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(WindACNum))// &
                          ', Unit name='//TRIM(CompName)//', stored Unit Name for that index='//  &
                          TRIM(WindAC(WindACNum)%Name))
    ENDIF
    CheckEquipName(WindACNum)=.false.
  ENDIF
ENDIF

RemainingOutputToCoolingSP = ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToCoolSP

IF(RemainingOutputToCoolingSP .LT. 0.0D0 .and. TempControlType(ZoneNum) .NE. SingleHeatingSetPoint)THEN
  QZnReq = RemainingOutputToCoolingSP
ELSE
  QZnReq = 0.0D0
END IF

ZoneEqDXCoil = .TRUE.
ZoneCoolingOnlyFan = .TRUE.

! Initialize the window AC unit
CALL InitWindowAC(WindACNum, QZnReq, ZoneNum, FirstHVACIteration)

CALL SimCyclingWindowAC(WindACNum,ZoneNum,FirstHVACIteration,PowerMet,QZnReq,LatOutputProvided)

! Report the result of the simulation
CALL ReportWindowAC(WindACNum)

ZoneEqDXCoil = .FALSE.
ZoneCoolingOnlyFan = .FALSE.

RETURN
END SUBROUTINE SimWindowAC

SUBROUTINE GetWindowAC

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Chandan Sharma, FSEC, March 2011: Added zone sys avail manager
          !                      Bereket Nigusse, FSEC, April 2011: eliminated input node names,
          !                                                         added OA Mixer object type
          !                                                         and fan object type
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for window AC units and stores it in window AC data structures

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,        ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString, GetObjectDefMaxArgs
  USE NodeInputManager,      ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: SetUpCompSets
  USE Fans,                  ONLY: GetFanIndex, GetFanVolFlow, GetFanAvailSchPtr, GetFanType
  USE General,               ONLY: TrimSigDigits
  USE DXCoils,               ONLY: GetDXCoilOutletNode => GetCoilOutletNode
  USE HVACHXAssistedCoolingCoil,  ONLY: GetDXHXAsstdCoilOutletNode => GetCoilOutletNode
  USE DataGlobals,           ONLY: AnyEnergyManagementSystemInModel, NumOfZones, ScheduleAlwaysOn
  USE DataInterfaces,        ONLY: SetupEMSActuator
  USE MixedAir,              ONLY: GetOAMixerIndex, GetOAMixerNodeNumbers
  USE DataHvacGlobals,       ONLY: FanType_SimpleConstVolume, FanType_SimpleVAV, FanType_SimpleOnOff, cFanTypes, ZoneComp
  USE DataZoneEquipment,     ONLY: WindowAC_Num, ZoneEquipConfig

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName = 'GetWindowAC: '  ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                        :: WindACIndex ! loop index
  INTEGER                        :: WindACNum   ! current window AC number
  CHARACTER(len=MaxNameLength)    :: CompSetFanInlet, CompSetCoolInlet, CompSetFanOutlet, CompSetCoolOutlet
  INTEGER                         :: NumAlphas  ! Number of Alphas for each GetObjectItem call
  INTEGER                         :: NumNumbers ! Number of Numbers for each GetObjectItem call
  INTEGER, DIMENSION(4)           :: OANodeNums ! Node numbers of Outdoor air mixer (OA, EA, RA, MA)
  INTEGER                         :: IOStatus   ! Used in GetObjectItem
  LOGICAL                         :: ErrorsFound=.false.  ! Set to true if errors in input, fatal at end of routine
  LOGICAL                         :: ErrFlag=.false. ! Local error flag for GetOAMixerNodeNums
  LOGICAL                         :: IsNotOK    ! Flag to verify name
  LOGICAL                         :: IsBlank    ! Flag for blank name
  LOGICAL                         :: FanErrFlag=.false. ! Error flag used in GetFanIndex call
  REAL(r64)                       :: FanVolFlow ! Fan volumetric flow rate
  LOGICAL                         :: CoilNodeErrFlag ! Used in error messages for mining coil outlet node number
  CHARACTER(len=MaxNameLength)    :: CurrentModuleObject                    ! Object type for getting and error messages
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas         ! Alpha input items for object
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers           ! Numeric input items for object
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.
  INTEGER                              :: TotalArgs=0       ! Total number of alpha and numeric arguments (max) for a
!  INTEGER                              :: FanType           ! Integer index for Fan type
  INTEGER                              :: CtrlZone    ! index to loop counter
  INTEGER                              :: NodeNum     ! index to loop counter
  LOGICAL                              :: ZoneNodeNotFound ! used in error checking


! find the number of each type of window AC unit
CurrentModuleObject = 'ZoneHVAC:WindowAirConditioner'

NumWindACCyc = GetNumObjectsFound(CurrentModuleObject)
NumWindAC = NumWindACCyc
! allocate the data structures
ALLOCATE(WindAC(NumWindAC))
ALLOCATE(CheckEquipName(NumWindAC))
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
lAlphaBlanks=.TRUE.
ALLOCATE(lNumericBlanks(NumNumbers))
lNumericBlanks=.TRUE.

! loop over window AC units; get and load the input data
DO WindACIndex = 1,NumWindACCyc

  CALL GetObjectItem(CurrentModuleObject,WindACIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                     NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                     AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

  WindACNum = WindACIndex
  IsNotOK=.FALSE.
  IsBlank=.FALSE.
  CALL VerifyName(Alphas(1),WindAC%Name,WindACNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.true.
    IF (IsBlank) Alphas(1)='xxxxx'
  ENDIF
  WindAC(WindACNum)%Name = Alphas(1)
  WindAC(WindACNum)%UnitType = WindowAC_UnitType ! 'ZoneHVAC:WindowAirConditioner'
  WindAC(WindACNum)%Sched = Alphas(2)
  IF (lAlphaBlanks(2)) THEN
    WindAC(WindACNum)%SchedPtr = ScheduleAlwaysOn
  ELSE
    WindAC(WindACNum)%SchedPtr = GetScheduleIndex(Alphas(2))  ! convert schedule name to pointer
    IF (WindAC(WindACNum)%SchedPtr .EQ. 0) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//'="'//TRIM(WindAC(WindACNum)%Name)//'" invalid data.')
      CALL ShowContinueError('invalid-not found '//TRIM(cAlphaFields(2))//'="'//TRIM(Alphas(2))//'".')
      ErrorsFound=.TRUE.
    END IF
  ENDIF
  WindAC(WindACNum)%MaxAirVolFlow = Numbers(1)
  WindAC(WindACNum)%OutAirVolFlow = Numbers(2)

  WindAC(WindACNum)%AirInNode = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)

  WindAC(WindACNum)%AirOutNode = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)

  WindAC(WindACNum)%OAMixType = Alphas(5)
  WindAC(WindACNum)%OAMixName = Alphas(6)
  ! Get outdoor air mixer node numbers
  ErrFlag = .false.
  CALL ValidateComponent(WindAC(WindACNum)%OAMixType,WindAC(WindACNum)%OAMixName,ErrFlag,TRIM(CurrentModuleObject))
  IF (ErrFlag) THEN
    CALL ShowContinueError('specified in '//TRIM(CurrentModuleObject)//' = "'//TRIM(WindAC(WindACNum)%Name)//'".')
    ErrorsFound=.TRUE.
  ELSE
    ! Get outdoor air mixer node numbers
    OANodeNums = GetOAMixerNodeNumbers(WindAC(WindACNum)%OAMixName, ErrFlag)
    IF (ErrFlag) THEN
      CALL ShowContinueError('that was specified in '//TRIM(CurrentModuleObject)//' = "'//TRIM(WindAC(WindACNum)%Name)//'"')
      CALL ShowContinueError('..OutdoorAir:Mixer is required. Enter an OutdoorAir:Mixer object with this name.')
      ErrorsFound=.true.
    ELSE
      WindAC(WindACNum)%OutsideAirNode = OANodeNums(1)
      WindAC(WindACNum)%AirReliefNode = OANodeNums(2)
      WindAC(WindACNum)%MixedAirNode = OANodeNums(4)
    ENDIF
  ENDIF

  WindAC(WindACNum)%FanType = Alphas(7)
  WindAC(WindACNum)%FanName = Alphas(8)

  FanErrFlag=.false.
  CALL ValidateComponent(WindAC(WindACNum)%FanType,WindAC(WindACNum)%FanName,FanErrFlag,TRIM(CurrentModuleObject))
  IF (FanErrFlag) THEN
    CALL ShowContinueError('specified in '//TRIM(CurrentModuleObject)//' = "'//TRIM(WindAC(WindACNum)%Name)//'".')
    ErrorsFound=.TRUE.
  ELSE
     CALL GetFanType(WindAC(WindACNum)%FanName,WindAC(WindACNum)%FanType_Num, &
                     FanErrFlag,CurrentModuleObject,WindAC(WindACNum)%Name)
     SELECT CASE (WindAC(WindACNum)%FanType_Num)
       CASE (FanType_SimpleOnOff, FanType_SimpleConstVolume)
         CALL GetFanIndex(WindAC(WindACNum)%FanName,WindAC(WindACNum)%FanIndex,FanErrFlag,TRIM(CurrentModuleObject))
         IF (FanErrFlag) THEN
           CALL ShowContinueError(' specified in '//TRIM(CurrentModuleObject)//' = "'//TRIM(WindAC(WindACNum)%Name)//'".')
           ErrorsFound=.true.
         ELSE
           CALL GetFanVolFlow(WindAC(WindACNum)%FanIndex,FanVolFlow)
           IF(FanVolFlow .NE. AutoSize)THEN
             IF(FanVolFlow .LT. WindAC(WindACNum)%MaxAirVolFlow)THEN
               CALL ShowWarningError('Air flow rate = '//TRIM(TrimSigDigits(FanVolFlow,7))// &
                     ' in fan object '//TRIM(WindAC(WindACNum)%FanName)//' is less than the maximum supply air flow'// &
                     ' rate ('//TRIM(TrimSigDigits(WindAC(WindACNum)%MaxAirVolFlow,7))//') in the '// &
                                TRIM(CurrentModuleObject)//' object.')
               CALL ShowContinueError(' The fan flow rate must be >= to the '//TRIM(cNumericFields(1))//' in the '// &
                                      TRIM(CurrentModuleObject)//' object.')
               CALL ShowContinueError(' Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(WindAC(WindACNum)%Name))
               ErrorsFound = .TRUE.
             END IF
           END IF
         ENDIF
       CASE DEFAULT
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(Alphas(1))//'".')
          CALL ShowContinueError('Fan Type must be Fan:OnOff, or Fan:ConstantVolume.')
          ErrorsFound=.TRUE.
     END SELECT
     ! Get the fan's availability schedule
     WindAC(WindACNum)%FanAvailSchedPtr = GetFanAvailSchPtr(WindAC(WindACNum)%FanType,WindAC(WindACNum)%FanName,FanErrFlag)
     IF (FanErrFlag) THEN
        CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(WindAC(WindACNum)%Name))
        ErrorsFound=.TRUE.
      ENDIF
  ENDIF

  WindAC(WindACNum)%DXCoilName = Alphas(10)

  IF(SameString(Alphas(9),'Coil:Cooling:DX:SingleSpeed') .OR. &
     SameString(Alphas(9),'CoilSystem:Cooling:DX:HeatExchangerAssisted')) THEN
     WindAC(WindACNum)%DXCoilType = Alphas(9)
     CoilNodeErrFlag = .FALSE.
     IF (SameString(Alphas(9),'Coil:Cooling:DX:SingleSpeed')) THEN
       WindAC(WindACNum)%DXCoilType_Num = CoilDX_CoolingSingleSpeed
       WindAC(WindACNum)%CoilOutletNodeNum = &
               GetDXCoilOutletNode(WindAC(WindACNum)%DXCoilType,WindAC(WindACNum)%DXCoilName,CoilNodeErrFlag)
     ELSEIF (SameString(Alphas(9),'CoilSystem:Cooling:DX:HeatExchangerAssisted')) THEN
       WindAC(WindACNum)%DXCoilType_Num = CoilDX_CoolingHXAssisted
       WindAC(WindACNum)%CoilOutletNodeNum = &
               GetDXHXAsstdCoilOutletNode(WindAC(WindACNum)%DXCoilType,WindAC(WindACNum)%DXCoilName,CoilNodeErrFlag)
     ENDIF
     IF(CoilNodeErrFlag)THEN
       CALL ShowContinueError(' that was specified in '//TRIM(CurrentModuleObject)//' = "' &
                              //TRIM(WindAC(WindACNum)%Name)//'".')
       ErrorsFound = .TRUE.
     END IF
  ELSE
    CALL ShowWarningError('Invalid '//TRIM(cAlphaFields(9))//' = '//TRIM(Alphas(9)))
    CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(WindAC(WindACNum)%Name))
    ErrorsFound = .TRUE.
  END IF

  WindAC(WindACNum)%FanSchedPtr = GetScheduleIndex(Alphas(11))

! Default to cycling fan when fan mode schedule is not present
  IF (.NOT. lAlphaBlanks(11) .AND. WindAC(WindACNum)%FanSchedPtr == 0) THEN
    CALL ShowSevereError(TRIM(CurrentModuleObject)//' "'//TRIM(WindAC(WindACNum)%Name)//'" '//&
                         TRIM(cAlphaFields(11))//' not found: '//TRIM(Alphas(11)))
    ErrorsFound=.TRUE.
  ELSEIF (lAlphaBlanks(11)) THEN
    WindAC(WindACNum)%OpMode = CycFanCycCoil
  END IF

  IF (SameString(Alphas(12),'BlowThrough'))  WindAC(WindACNum)%FanPlace = BlowThru
  IF (SameString(Alphas(12),'DrawThrough'))  WindAC(WindACNum)%FanPlace = DrawThru
  IF (WindAC(WindACNum)%FanPlace .EQ. 0) THEN
    CALL ShowSevereError('Invalid '//TRIM(cAlphaFields(12))//' = '//TRIM(Alphas(12)))
    CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(WindAC(WindACNum)%Name))
    ErrorsFound = .TRUE.
  END IF

  WindAC(WindACNum)%ConvergenceTol = Numbers(3)

  IF (.NOT. lAlphaBlanks(13)) THEN
    WindAC(WindACNum)%AvailManagerListName = Alphas(13)
    ZoneComp(WindowAC_Num)%ZoneCompAvailMgrs(WindACNum)%AvailManagerListName  = Alphas(13)
  ENDIF

  ! Add fan to component sets array
  IF (WindAC(WindACNum)%FanPlace == BlowThru) THEN

    ! Window AC air inlet node must be the same as a zone exhaust node and the OA Mixer return node
    ! check that Window AC air inlet node is the same as a zone exhaust node.
    ZoneNodeNotFound = .TRUE.
    DO CtrlZone = 1,NumOfZones
      IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
      DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumExhaustNodes
        IF (WindAC(WindACNum)%AirInNode .EQ. ZoneEquipConfig(CtrlZone)%ExhaustNode(NodeNum)) THEN
          ZoneNodeNotFound = .FALSE.
          EXIT
        END IF
      END DO
    END DO
    IF(ZoneNodeNotFound)THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(WindAC(WindACNum)%Name)//'".'//&
                           ' Window AC air inlet node name must be the same as a zone exhaust node name.')
      CALL ShowContinueError('..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.')
      CALL ShowContinueError('..Window AC air inlet node name = '//TRIM(NodeID(WindAC(WindACNum)%AirInNode)))
      ErrorsFound=.TRUE.
    END IF
    ! check that Window AC air outlet node is a zone inlet node.
    ZoneNodeNotFound = .TRUE.
    DO CtrlZone = 1,NumOfZones
      IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
      DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
        IF (WindAC(WindACNum)%AirOutNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(NodeNum)) THEN
          ZoneNodeNotFound = .FALSE.
          EXIT
        END IF
      END DO
    END DO
    IF(ZoneNodeNotFound)THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(WindAC(WindACNum)%Name)//'".'// &
                           ' Window AC air outlet node name must be the same as a zone inlet node name.')
      CALL ShowContinueError('..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.')
      CALL ShowContinueError('..Window AC air outlet node name = '//TRIM(NodeID(WindAC(WindACNum)%AirOutNode)))
      ErrorsFound=.TRUE.
    END IF
    CompSetFanInlet   = NodeID(WindAC(WindACNum)%MixedAirNode)
    CompSetFanOutlet  = 'UNDEFINED'
    CompSetCoolInlet  = 'UNDEFINED'
    CompSetCoolOutlet = NodeID(WindAC(WindACNum)%AirOutNode)
  ELSE ! draw through fan from IF (WindAC(WindACNum)%FanPlace == BlowThru) THEN
    ! check that Window AC air inlet node is the same as a zone exhaust node.
    ZoneNodeNotFound = .TRUE.
    DO CtrlZone = 1,NumOfZones
      IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
      DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumExhaustNodes
        IF (WindAC(WindACNum)%AirInNode .EQ. ZoneEquipConfig(CtrlZone)%ExhaustNode(NodeNum)) THEN
          ZoneNodeNotFound = .FALSE.
          EXIT
        END IF
      END DO
    END DO
    IF(ZoneNodeNotFound)THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(WindAC(WindACNum)%Name)//'".'// &
                           ' Window AC air inlet node name must be the same as a zone exhaust node name.')
      CALL ShowContinueError('..Zone exhaust node name is specified in ZoneHVAC:EquipmentConnections object.')
      CALL ShowContinueError('..Window AC inlet node name = '//TRIM(NodeID(WindAC(WindACNum)%AirInNode)))
      ErrorsFound=.TRUE.
    END IF
    ! check that Window AC air outlet node is the same as a zone inlet node.
    ZoneNodeNotFound = .TRUE.
    DO CtrlZone = 1,NumOfZones
      IF (.not. ZoneEquipConfig(CtrlZone)%IsControlled) CYCLE
      DO NodeNum = 1,ZoneEquipConfig(CtrlZone)%NumInletNodes
        IF (WindAC(WindACNum)%AirOutNode .EQ. ZoneEquipConfig(CtrlZone)%InletNode(NodeNum)) THEN
          ZoneNodeNotFound = .FALSE.
          EXIT
        END IF
      END DO
    END DO
    IF(ZoneNodeNotFound)THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(WindAC(WindACNum)%Name)//'".'// &
                           ' Window AC air outlet node name must be the same as a zone inlet node name.')
      CALL ShowContinueError('..Zone inlet node name is specified in ZoneHVAC:EquipmentConnections object.')
      CALL ShowContinueError('..Window AC outlet node name = '//TRIM(NodeID(WindAC(WindACNum)%AirOutNode)))
      ErrorsFound=.TRUE.
    END IF
    CompSetFanInlet   = NodeID(WindAC(WindACNum)%CoilOutletNodeNum)
    CompSetFanOutlet  = NodeID(WindAC(WindACNum)%AirOutNode)
    CompSetCoolInlet  = NodeID(WindAC(WindACNum)%MixedAirNode)
    CompSetCoolOutlet = NodeID(WindAC(WindACNum)%CoilOutletNodeNum)
  ENDIF
  ! Add fan to component sets array
  CALL SetUpCompSets(cWindowAC_UnitTypes(WindAC(WindACNum)%UnitType), WindAC(WindACNum)%Name, &
                     WindAC(WindACNum)%FanType, WindAC(WindACNum)%FanName,CompSetFanInlet,CompSetFanOutlet)

  ! Add cooling coil to component sets array
  CALL SetUpCompSets(cWindowAC_UnitTypes(WindAC(WindACNum)%UnitType), WindAC(WindACNum)%Name, &
                     WindAC(WindACNum)%DXCoilType,WindAC(WindACNum)%DXCoilName, &
                     CompSetCoolInlet,CompSetCoolOutlet)

  ! Set up component set for OA mixer - use OA node and Mixed air node
  CALL SetUpCompSets(cWindowAC_UnitTypes(WindAC(WindACNum)%UnitType), WindAC(WindACNum)%Name, &
                     WindAC(WindACNum)%OAMixType, WindAC(WindACNum)%OAMixName, &
                     NodeID(WindAC(WindACNum)%OutsideAirNode), NodeID(WindAC(WindACNum)%MixedAirNode))
END DO

DEALLOCATE(Alphas)
DEALLOCATE(cAlphaFields)
DEALLOCATE(cNumericFields)
DEALLOCATE(Numbers)
DEALLOCATE(lAlphaBlanks)
DEALLOCATE(lNumericBlanks)

IF (ErrorsFound) THEN
  CALL ShowFatalError(RoutineName//'Errors found in getting '//TRIM(CurrentModuleObject)// &
                      ' input.  Preceding condition causes termination.')
END IF

DO WindACNum=1,NumWindAC
  ! Setup Report variables for the Fan Coils
  CALL SetupOutputVariable('Zone Window Air Conditioner Total Cooling Rate [W]', &
                            WindAC(WindACNum)%TotCoolEnergyRate,'System','Average',&
                            WindAC(WindACNum)%Name)
  CALL SetupOutputVariable('Zone Window Air Conditioner Total Cooling Energy [J]', &
                            WindAC(WindACNum)%TotCoolEnergy,'System','Sum',&
                            WindAC(WindACNum)%Name)
  CALL SetupOutputVariable('Zone Window Air Conditioner Sensible Cooling Rate [W]', &
                            WindAC(WindACNum)%SensCoolEnergyRate,'System','Average',&
                            WindAC(WindACNum)%Name)
  CALL SetupOutputVariable('Zone Window Air Conditioner Sensible Cooling Energy [J]', &
                            WindAC(WindACNum)%SensCoolEnergy,'System','Sum',&
                            WindAC(WindACNum)%Name)
  CALL SetupOutputVariable('Zone Window Air Conditioner Latent Cooling Rate [W]', &
                           WindAC(WindACNum)%LatCoolEnergyRate,'System','Average',&
                           WindAC(WindACNum)%Name)
  CALL SetupOutputVariable('Zone Window Air Conditioner Latent Cooling Energy [J]', &
                           WindAC(WindACNum)%LatCoolEnergy,'System','Sum',&
                           WindAC(WindACNum)%Name)
  CALL SetupOutputVariable('Zone Window Air Conditioner Electric Power [W]', &
                            WindAC(WindACNum)%ElecPower,'System','Average',&
                            WindAC(WindACNum)%Name)
  CALL SetupOutputVariable('Zone Window Air Conditioner Electric Energy [J]', &
                            WindAC(WindACNum)%ElecConsumption,'System','Sum',&
                            WindAC(WindACNum)%Name)
  CALL SetupOutputVariable('Zone Window Air Conditioner Fan Part Load Ratio []', &
                            WindAC(WindACNum)%FanPartLoadRatio,'System','Average',&
                            WindAC(WindACNum)%Name)
  CALL SetupOutputVariable('Zone Window Air Conditioner Compressor Part Load Ratio []', &
                            WindAC(WindACNum)%CompPartLoadRatio,'System','Average',&
                            WindAC(WindACNum)%Name)
  CALL SetupOutputVariable('Zone Window Air Conditioner Fan Availability Status []', &
                            WindAC(WindACNum)%AvailStatus,'System','Average',&
                            WindAC(WindACNum)%Name)
  IF (AnyEnergyManagementSystemInModel) THEN
    CALL SetupEMSActuator('Window Air Conditioner', WindAC(WindACNum)%Name, 'Part Load Ratio' , '[fraction]', &
                           WindAC(WindACNum)%EMSOverridePartLoadFrac, WindAC(WindACNum)%EMSValueForPartLoadFrac )
  ENDIF

END DO

RETURN

END SUBROUTINE GetWindowAC

SUBROUTINE InitWindowAC(WindACNum,QZnReq,ZoneNum,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Chandan Sharma, FSEC, March 2011: Added zone sys avail manager
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Window AC Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment,     ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList, WindowAC_Num
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand, CurDeadbandOrSetback
  USE DataHvacGlobals,       ONLY: ZoneComp, ZoneCompTurnFansOn, ZoneCompTurnFansOff, SmallLoad

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT (IN)    :: WindACNum ! number of the current window AC unit being simulated
  REAL(r64), INTENT (INOUT) :: QZnReq    ! zone load (modified as needed) (W)
  INTEGER,   INTENT (IN)    :: ZoneNum   ! index to zone
  LOGICAL,   INTENT (IN)    :: FirstHVACIteration   ! TRUE when first HVAC iteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  Integer             :: InNode         ! inlet node number in window AC loop
  Integer             :: OutNode        ! outlet node number in window AC loop
  INTEGER             :: InletNode      ! inlet node number for window AC WindACNum
  INTEGER             :: OutsideAirNode ! outside air node number in window AC loop
  INTEGER             :: AirRelNode     ! relief air node number in window AC loop
  REAL(r64)           :: RhoAir         ! air density at InNode
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL,SAVE        :: ZoneEquipmentListChecked = .false.  ! True after the Zone Equipment List has been checked for items
  Integer             :: Loop           ! loop counter
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag  ! one time initialization flag
  REAL(r64)           :: QToCoolSetPt   ! sensible load to cooling setpoint (W)
  REAL(r64)           :: NoCompOutput   ! sensible load delivered with compressor off (W)

! Do the one time initializations
IF (MyOneTimeFlag) THEN

  ALLOCATE(MyEnvrnFlag(NumWindAC))
  ALLOCATE(MySizeFlag(NumWindAC))
  MyEnvrnFlag   = .TRUE.
  MySizeFlag    = .TRUE.
  MyOneTimeFlag = .false.

END IF

IF (ALLOCATED(ZoneComp)) THEN
  ZoneComp(WindowAC_Num)%ZoneCompAvailMgrs(WindACNum)%ZoneNum = ZoneNum
  WindAC(WindACNum)%AvailStatus = ZoneComp(WindowAC_Num)%ZoneCompAvailMgrs(WindACNum)%AvailStatus
ENDIF

! need to check all Window AC units to see if they are on Zone Equipment List or issue warning
IF (.not. ZoneEquipmentListChecked .and. ZoneEquipInputsFilled) THEN
  ZoneEquipmentListChecked=.true.
  DO Loop=1,NumWindAC
    IF (CheckZoneEquipmentList(cWindowAC_UnitTypes(WindAC(Loop)%UnitType),WindAC(Loop)%Name)) CYCLE
    CALL ShowSevereError('InitWindowAC: Window AC Unit=['//TRIM(cWindowAC_UnitTypes(WindAC(Loop)%UnitType))//','//  &
       TRIM(WindAC(Loop)%Name)//  &
         '] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.')
  ENDDO
ENDIF

IF ( .NOT. SysSizingCalc .AND. MySizeFlag(WindACNum) ) THEN

  CALL SizeWindowAC(WindACNum)

  MySizeFlag(WindACNum) = .FALSE.
END IF

! Do the Begin Environment initializations
IF (BeginEnvrnFlag .and. MyEnvrnFlag(WindACNum)) THEN
  InNode = WindAC(WindACNum)%AirInNode
  OutNode = WindAC(WindACNum)%AirOutNode
  OutsideAirNode = WindAC(WindACNum)%OutsideAirNode
  RhoAir = StdRhoAir
  ! set the mass flow rates from the input volume flow rates
  WindAC(WindACNum)%MaxAirMassFlow = RhoAir*WindAC(WindACNum)%MaxAirVolFlow
  WindAC(WindACNum)%OutAirMassFlow = RhoAir*WindAC(WindACNum)%OutAirVolFlow
  ! set the node max and min mass flow rates
  Node(OutsideAirNode)%MassFlowRateMax = WindAC(WindACNum)%OutAirMassFlow
  Node(OutsideAirNode)%MassFlowRateMin = 0.0d0
  Node(OutNode)%MassFlowRateMax = WindAC(WindACNum)%MaxAirMassFlow
  Node(OutNode)%MassFlowRateMin = 0.0d0
  Node(InNode)%MassFlowRateMax = WindAC(WindACNum)%MaxAirMassFlow
  Node(InNode)%MassFlowRateMin = 0.0d0
  MyEnvrnFlag(WindACNum) = .FALSE.
END IF ! end one time inits

IF (.not. BeginEnvrnFlag) THEN
  MyEnvrnFlag(WindACNum) = .true.
ENDIF

IF (WindAC(WindACNum)%FanSchedPtr .GT. 0) THEN
  IF (GetCurrentScheduleValue(WindAC(WindACNum)%FanSchedPtr) .EQ. 0.0d0) THEN
    WindAC(WindACNum)%OpMode = CycFanCycCoil
  ELSE
    WindAC(WindACNum)%OpMode = ContFanCycCoil
  END IF
END IF

! These initializations are done every iteration
InletNode = WindAC(WindACNum)%AirInNode
OutsideAirNode = WindAC(WindACNum)%OutsideAirNode
AirRelNode = WindAC(WindACNum)%AirReliefNode
! Set the inlet node mass flow rate
IF (GetCurrentScheduleValue(WindAC(WindACNum)%SchedPtr) .LE. 0.0d0 &
    .OR. (GetCurrentScheduleValue(WindAC(WindACNum)%FanAvailSchedPtr) .LE. 0.0d0 .AND. &
    .NOT. ZoneCompTurnFansOn) .OR. ZoneCompTurnFansOff) THEN
  WindAC(WindACNum)%PartLoadFrac = 0.0d0
  Node(InletNode)%MassFlowRate = 0.0d0
  Node(InletNode)%MassFlowRateMaxAvail = 0.0d0
  Node(InletNode)%MassFlowRateMinAvail = 0.0d0
  Node(OutsideAirNode)%MassFlowRate = 0.0d0
  Node(OutsideAirNode)%MassFlowRateMaxAvail = 0.0d0
  Node(OutsideAirNode)%MassFlowRateMinAvail = 0.0d0
  Node(AirRelNode)%MassFlowRate = 0.0d0
  Node(AirRelNode)%MassFlowRateMaxAvail = 0.0d0
  Node(AirRelNode)%MassFlowRateMinAvail = 0.0d0
ELSE
  WindAC(WindACNum)%PartLoadFrac = 1.0d0
  Node(InletNode)%MassFlowRate = WindAC(WindACNum)%MaxAirMassFlow
  Node(InletNode)%MassFlowRateMaxAvail = Node(InletNode)%MassFlowRate
  Node(InletNode)%MassFlowRateMinAvail = Node(InletNode)%MassFlowRate
  Node(OutsideAirNode)%MassFlowRate = WindAC(WindACNum)%OutAirMassFlow
  Node(OutsideAirNode)%MassFlowRateMaxAvail = WindAC(WindACNum)%OutAirMassFlow
  Node(OutsideAirNode)%MassFlowRateMinAvail = 0.0d0
  Node(AirRelNode)%MassFlowRate = WindAC(WindACNum)%OutAirMassFlow
  Node(AirRelNode)%MassFlowRateMaxAvail = WindAC(WindACNum)%OutAirMassFlow
  Node(AirRelNode)%MassFlowRateMinAvail = 0.0d0
END IF

! Original thermostat control logic (works only for cycling fan systems)
IF(QZnReq .LT. (-1.d0*SmallLoad) .AND. .NOT. CurDeadbandOrSetback(ZoneNum) .AND. WindAC(WindACNum)%PartLoadFrac .GT. 0.0d0)THEN
  CoolingLoad = .TRUE.
ELSE
  CoolingLoad = .FALSE.
END IF

! Constant fan systems are tested for ventilation load to determine if load to be met changes.
IF(WindAC(WindACNum)%OpMode .EQ. ContFanCycCoil .AND. WindAC(WindACNum)%PartLoadFrac .GT. 0.0d0 &
    .AND. (GetCurrentScheduleValue(WindAC(WindACNum)%FanAvailSchedPtr) .GT. 0.0d0 .OR. &
    ZoneCompTurnFansOn) .AND. .NOT. ZoneCompTurnFansOn)THEN

  CALL CalcWindowACOutput(WindACNum,FirstHVACIteration,WindAC(WindACNum)%OpMode, &
                          0.0d0,.FALSE.,NoCompOutput)

  QToCoolSetPt=ZoneSysEnergyDemand(ZoneNum)%RemainingOutputReqToCoolSP

! If the unit has a net heating capacity and the zone temp is below the Tstat cooling setpoint
  IF(NoCompOutput .GT. (-1.d0 * SmallLoad) .AND. QToCoolSetPt .GT. (-1.d0 * SmallLoad) .AND. CurDeadbandOrSetback(Zonenum) )THEN
    IF(NoCompOutput .GT. QToCoolSetPt)THEN
      QZnReq       = QToCoolSetPt
      CoolingLoad  = .TRUE.
    END IF
  END IF
END IF

RETURN
END SUBROUTINE InitWindowAC

SUBROUTINE SizeWindowAC(WindACNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   January 2002
          !       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Window AC  Unit components for which flow rates have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone or system sizing arrays

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE General,             ONLY: RoundSigDigits    

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: WindACNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: MaxAirVolFlowDes    ! Autosized maximum air flow for reproting 
  REAL(r64) :: MaxAirVolFlowUser   ! Hardsized maximum air flow for reproting
  REAL(r64) :: OutAirVolFlowDes    ! Autosized outdoor air flow for reproting
  REAL(r64) :: OutAirVolFlowUser   ! Hardsized outdoor ari flow for reporting  
  LOGICAL   :: IsAutosize          ! Indicator to autosize

  IsAutosize = .FALSE.
  MaxAirVolFlowDes = 0.0d0
  MaxAirVolFlowUser = 0.0d0
  OutAirVolFlowDes = 0.0d0
  OutAirVolFlowUser = 0.0d0

  IF (WindAC(WindACNum)%MaxAirVolFlow == AutoSize) THEN
    IsAutosize = .TRUE.  
  END IF
  IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
      IF (WindAC(WindACNum)%MaxAirVolFlow > 0.0d0) THEN  
        CALL ReportSizingOutput(cWindowAC_UnitTypes(WindAC(WindACNum)%UnitType), WindAC(WindACNum)%Name, &
                              'Maximum Supply Air Flow Rate [m3/s]', WindAC(WindACNum)%MaxAirVolFlow)
      END IF
    ELSE  
      CALL CheckZoneSizing(cWindowAC_UnitTypes(WindAC(WindACNum)%UnitType), WindAC(WindACNum)%Name)
      MaxAirVolFlowDes = FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow
      IF (MaxAirVolFlowDes < SmallAirVolFlow) THEN
        MaxAirVolFlowDes = 0.0d0
      END IF
      IF (IsAutosize) THEN
        WindAC(WindACNum)%MaxAirVolFlow = MaxAirVolFlowDes  
        CALL ReportSizingOutput(cWindowAC_UnitTypes(WindAC(WindACNum)%UnitType), WindAC(WindACNum)%Name, &
                              'Design Size Maximum Supply Air Flow Rate [m3/s]', MaxAirVolFlowDes)
      ELSE
        IF (WindAC(WindACNum)%MaxAirVolFlow > 0.0d0 .AND. MaxAirVolFlowDes > 0.0d0) THEN  
          MaxAirVolFlowUser = WindAC(WindACNum)%MaxAirVolFlow
          CALL ReportSizingOutput(cWindowAC_UnitTypes(WindAC(WindACNum)%UnitType), WindAC(WindACNum)%Name, &
                              'Design Size Maximum Supply Air Flow Rate [m3/s]', MaxAirVolFlowDes, &
                              'User-Specified Maximum Supply Air Flow Rate [m3/s]', MaxAirVolFlowUser)
          IF (DisplayExtraWarnings) THEN
            IF ((ABS(MaxAirVolFlowDes - MaxAirVolFlowUser)/MaxAirVolFlowUser) > AutoVsHardSizingThreshold) THEN
              CALL ShowMessage('SizeWindowAC: Potential issue with equipment sizing for ' &
                                         //TRIM(cWindowAC_UnitTypes(WindAC(WindACNum)%UnitType))//' '// &
                                          TRIM(WindAC(WindACNum)%Name))
              CALL ShowContinueError('User-Specified Maximum Supply Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(MaxAirVolFlowUser,5))// ' [m3/s]')
              CALL ShowContinueError('differs from Design Size Maximum Supply Air Flow Rate of ' // &
                                      TRIM(RoundSigDigits(MaxAirVolFlowDes,5))// ' [m3/s]')
              CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
              CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
            END IF
          ENDIF
        END IF   
      END IF  
    END IF
  END IF


  IF (WindAC(WindACNum)%OutAirVolFlow == AutoSize) THEN

    IF (CurZoneEqNum > 0) THEN

      CALL CheckZoneSizing(cWindowAC_UnitTypes(WindAC(WindACNum)%UnitType), WindAC(WindACNum)%Name)
      WindAC(WindACNum)%OutAirVolFlow = MIN(FinalZoneSizing(CurZoneEqNum)%MinOA,WindAC(WindACNum)%MaxAirVolFlow)
      IF (WindAC(WindACNum)%OutAirVolFlow < SmallAirVolFlow) THEN
        WindAC(WindACNum)%OutAirVolFlow = 0.0d0
      END IF
      CALL ReportSizingOutput(cWindowAC_UnitTypes(WindAC(WindACNum)%UnitType), WindAC(WindACNum)%Name, &
                              'Maximum Outdoor Air Flow Rate [m3/s]',WindAC(WindACNum)%OutAirVolFlow)

    END IF

  END IF

  IF (CurZoneEqNum > 0) THEN
    ZoneEqSizing(CurZoneEqNum)%OAVolFlow = WindAC(WindACNum)%OutAirVolFlow
    ZoneEqSizing(CurZoneEqNum)%AirVolFlow = WindAC(WindACNum)%MaxAirVolFlow
  END IF

  RETURN

END SUBROUTINE SizeWindowAC

SUBROUTINE SimCyclingWindowAC(WindACNum,ZoneNum,FirstHVACIteration,PowerMet,QZnReq,LatOutputProvided)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Buhl/Shirey Mar 2001, Shirey Aug 2009 (LatOutputProvided)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate a cycling window air conditioner unit; adjust its output to match the
          ! remaining zone load.

          ! METHODOLOGY EMPLOYED:
          ! If unit is on, calls ControlWindACOutput to obtain the desired unit output

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT (IN)  :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep
  INTEGER, INTENT (IN)  :: WindACNum           ! number of the current window AC unit being simulated
  INTEGER, INTENT (IN)  :: ZoneNum             ! number of zone being served !unused1208
  REAL(r64), INTENT (OUT) :: PowerMet          ! Sensible power supplied (W)
  REAL(r64), INTENT (IN)  :: QZnReq            ! Sensible load to be met (W)
  REAL(r64), INTENT (OUT) :: LatOutputProvided ! Latent power supplied (kg/s), negative = dehumidification

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64)    :: PartLoadFrac     ! unit part load fraction
REAL(r64)    :: QUnitOut         ! Dry air sens. cooling provided by AC unit [watts]
REAL(r64)    :: SensCoolOut      ! Moist air sensible cooling rate [W]
REAL(r64)    :: LatentOutput     ! Latent (moisture) add/removal rate, negative is dehumidification [kg/s]
LOGICAL :: UnitOn           ! TRUE if unit is on
LOGICAL :: CoilOn           ! TRUE if coil is on
INTEGER :: OutletNode       ! unit air outlet node
INTEGER :: InletNode        ! unit air inlet node
REAL(r64)    :: QTotUnitOut      ! total unit output [watts]
REAL(r64)    :: AirMassFlow      ! air mass flow rate [kg/sec]
REAL(r64)    :: CpAir            ! inlet air specific heat [J/kg-C]
REAL(r64)    :: Test
INTEGER :: OpMode           ! operating mode (fan cycling or continious; DX coil always cycles)
REAL(r64)    :: MinHumRat        ! minimum of inlet & outlet humidity ratio
LOGICAL :: HXUnitOn         ! Used to control HX heat recovery as needed
REAL(r64) :: SpecHumOut     ! Specific humidity ratio of outlet air (kg moisture / kg moist air)
REAL(r64) :: SpecHumIn      ! Specific humidity ratio of inlet air (kg moisture / kg moist air)

! zero the fan and DX coil electricity consumption
FanElecPower = 0.0d0
DXElecCoolingPower = 0.0d0
! initialize local variables
UnitOn = .TRUE.
CoilOn = .TRUE.
QUnitOut = 0.0d0
LatentOutput = 0.0d0
OutletNode = WindAC(WindACNum)%AirOutNode
InletNode = WindAC(WindACNum)%AirInNode
AirMassFlow = Node(InletNode)%MassFlowRate
test = airmassflow
CpAir = PsyCpAirFnWTdb(Node(InletNode)%HumRat,Node(InletNode)%Temp)
OpMode = WindAC(WindACNum)%OpMode

! set the on/off flags
IF (WindAC(WindACNum)%OPMode == CycFanCycCoil) THEN
  ! cycling unit: only runs if there is a load.
   IF ( .NOT. CoolingLoad .OR. AirMassFlow < SmallMassFlow ) THEN
     UnitOn = .FALSE.
     CoilOn = .FALSE.
   END IF
ELSE IF  (WindAC(WindACNum)%OPMode == ContFanCycCoil) THEN
  ! continuous unit: fan runs if scheduled on; coil runs only if cooling load
  IF (AirMassFlow.LT.SmallMassFlow) THEN
    UnitOn = .FALSE.
    CoilON = .FALSE.
  ELSE IF ( .NOT. CoolingLoad) THEN
    CoilOn = .FALSE.
  END IF
END IF

OnOffFanPartLoadFraction = 1.0d0

IF (UnitOn .AND. CoilOn) THEN
  HXUnitOn = .FALSE.
  CALL ControlCycWindACOutput(WindACNum,FirstHVACIteration,OpMode,QZnReq,PartLoadFrac,HXUnitOn)
ELSE
   PartLoadFrac = 0.0d0
   HXUnitOn = .FALSE.
END IF

WindAC(WindACNum)%PartLoadFrac = PartLoadFrac

CALL CalcWindowACOutput(WindACNum,FirstHVACIteration,OpMode,PartLoadFrac,HXUnitOn,QUnitOut)

! Reseting AirMassFlow to inlet node mass flow rate since inlet mass flow rate may be getting
! manipulated in subroutine CalcWindowACOutput

AirMassFlow = Node(InletNode)%MassFlowRate
MinHumRat = MIN(Node(InletNode)%HumRat,Node(OutletNode)%HumRat)
QUnitOut = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,MinHumRat)   &
                        - PsyHFnTdbW(Node(InletNode)%Temp,MinHumRat))

SensCoolOut = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,MinHumRat) - &
                             PsyHFnTdbW(Node(InletNode)%Temp,MinHumRat))

! CR9155 Remove specific humidity calculations
SpecHumOut = Node(OutletNode)%HumRat
SpecHumIn  = Node(InletNode)%HumRat
LatentOutput = AirMassFlow * (SpecHumOut - SpecHumIn) ! Latent rate, kg/s

QTotUnitOut = AirMassFlow * (Node(OutletNode)%Enthalpy - Node(InletNode)%Enthalpy)

! report variables
WindAC(WindACNum)%CompPartLoadRatio = WindAC(WindACNum)%PartLoadFrac
IF (WindAC(WindACNum)%OpMode .EQ. CycFanCycCoil) THEN
  WindAC(WindACNum)%FanPartLoadRatio = WindAC(WindACNum)%PartLoadFrac
ELSE
  IF (UnitOn) THEN
    WindAC(WindACNum)%FanPartLoadRatio = 1.0d0
  ELSE
    WindAC(WindACNum)%FanPartLoadRatio = 0.0d0
  END IF
END IF
WindAC(WindACNum)%SensCoolEnergyRate = ABS(MIN(0.0d0,SensCoolOut))
WindAC(WindACNum)%TotCoolEnergyRate = ABS(MIN(0.0d0,QTotUnitOut))
WindAC(WindACNum)%SensCoolEnergyRate = MIN(WindAC(WindACNum)%SensCoolEnergyRate,WindAC(WindACNum)%TotCoolEnergyRate)
WindAC(WindACNum)%LatCoolEnergyRate = WindAC(WindACNum)%TotCoolEnergyRate - WindAC(WindACNum)%SensCoolEnergyRate
WindAC(WindACNum)%ElecPower = FanElecPower + DXElecCoolingPower

PowerMet = QUnitOut
LatOutputProvided = LatentOutput

RETURN
END SUBROUTINE SimCyclingWindowAC

SUBROUTINE ReportWindowAC(WindACNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Fills some of the report variables for the window AC units

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use DataHVACGlobals, ONLY: TimeStepSys

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: WindACNum ! number of the current AC unit being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: ReportingConstant

          ! FLOW

ReportingConstant = TimeStepSys*SecInHour

WindAC(WindACNum)%SensCoolEnergy = WindAC(WindACNum)%SensCoolEnergyRate * ReportingConstant
WindAC(WindACNum)%TotCoolEnergy = WindAC(WindACNum)%TotCoolEnergyRate * ReportingConstant
WindAC(WindACNum)%LatCoolEnergy = WindAC(WindACNum)%LatCoolEnergyRate * ReportingConstant
WindAC(WindACNum)%ElecConsumption = WindAC(WindACNum)%ElecPower * ReportingConstant

RETURN
END SUBROUTINE ReportWindowAC

SUBROUTINE CalcWindowACOutput(WindACNum,FirstHVACIteration,OpMode,PartLoadFrac,HXUnitOn,LoadMet)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2000
          !       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulate the components making up the cycling window AC unit.

          ! METHODOLOGY EMPLOYED:
          ! Simulates the unit components sequentially in the air flow direction.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE MixedAir, ONLY:   SimOAMixer
USE Fans, ONLY:       SimulateFanComponents
USE DXCoils, ONLY:    SimDXCoil
USE HVACHXAssistedCoolingCoil,   ONLY: SimHXAssistedCoolingCoil
USE InputProcessor,              ONLY: SameString
USE DataHvacGlobals,             ONLY: ZoneCompTurnFansOn, ZoneCompTurnFansOff

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: WindACNum          ! Unit index in fan coil array
  LOGICAL, INTENT (IN)  :: FirstHVACIteration ! flag for 1st HVAV iteration in the time step
  INTEGER, INTENT (IN)  :: OpMode             ! operating mode: CycFanCycCoil | ContFanCycCoil
  REAL(r64)   , INTENT (IN)  :: PartLoadFrac       ! unit part load fraction
  LOGICAL, INTENT (IN)  :: HXUnitOn           ! Flag to toggle HX heat recovery as needed
  REAL(r64),    INTENT (OUT) :: LoadMet            ! load met by unit (watts)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: OutletNode       ! unit air outlet node
INTEGER :: InletNode        ! unit air inlet node
INTEGER :: OutsideAirNode   ! outside air node number in window AC loop
INTEGER :: AirRelNode       ! relief air node number in window AC loop
REAL(r64)    :: AirMassFlow      ! total mass flow through the unit
REAL(r64)    :: MinHumRat        ! minimum of inlet & outlet humidity ratio

          ! FLOW

OutletNode = WindAC(WindACNum)%AirOutNode
InletNode = WindAC(WindACNum)%AirInNode
OutsideAirNode = WindAC(WindACNum)%OutsideAirNode
AirRelNode = WindAC(WindACNum)%AirReliefNode
! for cycling fans, pretend we have VAV
IF (OpMode.EQ.CycFanCycCoil) THEN
  Node(InletNode)%MassFlowRate = Node(InletNode)%MassFlowRateMax * PartLoadFrac
! Don't let the outside air flow be > supply air flow
  Node(OutsideAirNode)%MassFlowRate = MIN(Node(OutsideAirNode)%MassFlowRateMax,Node(InletNode)%MassFlowRate)
  Node(AirRelNode)%MassFlowRate = Node(OutsideAirNode)%MassFlowRate
END IF
AirMassFlow = Node(InletNode)%MassFlowRate
CALL SimOAMixer(WindAC(WindACNum)%OAMixName,FirstHVACIteration,WindAC(WindACNum)%OAMixIndex)

! if blow through, simulate fan then coil. For draw through, simulate coil then fan.
IF (WindAC(WindACNum)%FanPlace .EQ. BlowThru) THEN
   CALL SimulateFanComponents(WindAC(WindACNum)%FanName,FirstHVACIteration,WindAC(WindACNum)%FanIndex, &
                               ZoneCompTurnFansOn = ZoneCompTurnFansOn,ZoneCompTurnFansOff = ZoneCompTurnFansOff)
END IF

IF(WindAC(WindACNum)%DXCoilType_Num == CoilDX_CoolingHXAssisted) THEN
  CALL SimHXAssistedCoolingCoil(WindAC(WindACNum)%DXCoilName,FirstHVACIteration,On,PartLoadFrac,WindAC(WindACNum)%DXCoilIndex, &
                                WindAC(WindACNum)%OpMode, HXUnitEnable=HXUnitOn)
ELSE
  CALL SimDXCoil(WindAC(WindACNum)%DXCoilName,On,FirstHVACIteration,PartLoadFrac,WindAC(WindACNum)%DXCoilIndex,  &
     WindAC(WindACNum)%OpMode)
END IF

IF (WindAC(WindACNum)%FanPlace .EQ. DrawThru) THEN
  CALL SimulateFanComponents(WindAC(WindACNum)%FanName,FirstHVACIteration,WindAC(WindACNum)%FanIndex, &
                             ZoneCompTurnFansOn = ZoneCompTurnFansOn,ZoneCompTurnFansOff = ZoneCompTurnFansOff)
END IF

MinHumRat = MIN(Node(InletNode)%HumRat,Node(OutletNode)%HumRat)
LoadMet = AirMassFlow * (PsyHFnTdbW(Node(OutletNode)%Temp,MinHumRat)  &
                       - PsyHFnTdbW(Node(InletNode)%Temp,MinHumRat))

RETURN
END SUBROUTINE CalcWindowACOutput

SUBROUTINE ControlCycWindACOutput(WindACNum,FirstHVACIteration,OpMode,QZnReq,PartLoadFrac, HXUnitOn)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Shirey, May 2001
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Determine the part load fraction of the air conditioner for this time step

          ! METHODOLOGY EMPLOYED:
          ! Linear interpolation between max and min outputs

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: WindACNum          ! Unit index in fan coil array
  LOGICAL, INTENT (IN)  :: FirstHVACIteration ! flag for 1st HVAV iteration in the time step
  INTEGER, INTENT(IN)   :: OpMode             ! operating mode: CycFanCycCoil | ContFanCycCoil
  REAL(r64)   , INTENT(IN)   :: QZnReq             ! cooling output needed by zone [W]
  REAL(r64)   , INTENT (OUT) :: PartLoadFrac       ! unit part load fraction
  LOGICAL, INTENT (INOUT) :: HXUnitOn         ! Used to control HX heat recovery as needed

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !
INTEGER, PARAMETER  ::   MaxIter = 50         !maximum number of iterations
REAL(r64), PARAMETER     ::   MinPLF = 0.0d0         !minimum part load factor allowed

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64) :: FullOutput   ! unit full output [W]
REAL(r64) :: NoCoolOutput ! output when no active cooling [W]
REAL(r64) :: ActualOutput ! output at current partloadfrac [W]
REAL(r64) :: Error        ! error between QznReq and ActualOutput [W]
REAL(r64) :: ErrorToler   ! error tolerance
INTEGER :: Iter      ! iteration counter
!CHARACTER(len=20) :: ErrNum
!INTEGER,SAVE :: ErrCount=0
REAL(r64) :: DelPLF
REAL(r64) :: Relax

! DX Cooling HX assisted coils can cycle the heat exchanger, see if coil ON, HX OFF can meet humidity setpoint if one exists
IF(WindAC(WindACNum)%DXCoilType_Num == CoilDX_CoolingHXAssisted)THEN
  ! Check for a setpoint at the HX outlet node, if it doesn't exist always run the HX
  IF(Node(WindAC(WindACNum)%CoilOutletNodeNum)%HumRatMax .EQ. SensedNodeFlagValue) THEN
    HXUnitOn = .TRUE.
  ELSE
    HXUnitOn = .FALSE.
  END IF
ELSE
  HXUnitOn = .FALSE.
END IF

IF (WindAC(WindACNum)%EMSOverridePartLoadFrac) THEN

  PartLoadFrac = WindAC(WindACNum)%EMSValueForPartLoadFrac
ENDIF

! Get result when DX coil is off
CALL CalcWindowACOutput(WindACNum,FirstHVACIteration,OpMode,0.0d0,HXUnitOn,NoCoolOutput)

! If NoCoolOutput < QZnReq, the coil needs to be off
IF (NoCoolOutput < QZnReq) THEN
  PartLoadFrac = 0.0d0
  RETURN
END IF

! Get full load result
CALL CalcWindowACOutput(WindACNum,FirstHVACIteration,OpMode,1.0d0,HXUnitOn,FullOutput)

! Since we are cooling, we expect FullOutput to be < 0 and FullOutput < NoCoolOutput
! Check that this is the case; if not set PartLoadFrac = 0.0 (off) and return
IF (FullOutput >= 0.0d0 .OR. FullOutput >= NoCoolOutput) THEN
  PartLoadFrac = 0.0d0
  RETURN
END IF

! If the QZnReq <= FullOutput the unit needs to run full out
IF (QZnReq  <=  FullOutput .AND. WindAC(WindACNum)%DXCoilType_Num /= CoilDX_CoolingHXAssisted) THEN
  PartLoadFrac = 1.0d0
  RETURN
END IF

! If the QZnReq <= FullOutput and a HXAssisted coil is used, check the node setpoint for a maximum humidity ratio set piont
! HumRatMax will either equal -999 if no setpoint exists or could be 0 if no moisture load is present
IF (QZnReq  <=  FullOutput .AND. WindAC(WindACNum)%DXCoilType_Num == CoilDX_CoolingHXAssisted .AND. &
    Node(WindAC(WindACNum)%CoilOutletNodeNum)%HumRatMax .LE. 0.0d0) THEN
  PartLoadFrac = 1.0d0
  RETURN
END IF

! QZnReq should now be greater than FullOutput and less than NoCoolOutput)
! Calculate the part load fraction

PartLoadFrac = MAX(MinPLF, ABS(QZnReq - NoCoolOutput) / ABS(FullOutput - NoCoolOutput))

  ErrorToler = WindAC(WindACNum)%ConvergenceTol !Error tolerance for convergence from input deck
  Error = 1.0d0             !initialize error value for comparison against tolerance
  Iter = 0                !initialize iteration counter
  Relax = 1.0d0

  DO WHILE ((ABS(Error) .GT. ErrorToler) .AND. (Iter .LE. MaxIter) .AND. PartLoadFrac .GT. MinPLF)
    ! Get result when DX coil is operating at partloadfrac
    CALL CalcWindowACOutput(WindACNum,FirstHVACIteration,OpMode,PartLoadFrac,HXUnitOn,ActualOutput)
    Error = (QZnReq - ActualOutput)/QZnReq
    DelPLF = (QZnReq-ActualOutput)/FullOutput
    PartLoadFrac = PartLoadFrac + Relax * DelPLF
    PartLoadFrac = MAX(MinPLF,MIN(1.0d0,PartLoadFrac))
    Iter = Iter + 1
    IF (Iter == 16) THEN
      Relax = 0.5d0
    END IF
  END DO
  IF (Iter .GT. MaxIter) THEN
    IF (WindAC(WindACNum)%MaxIterIndex1 == 0) THEN
      CALL ShowWarningMessage('ZoneHVAC:WindowAirConditioner="'//TRIM(WindAC(WindACNum)%Name)//  &
         '" -- Exceeded max iterations while adjusting compressor'// &
         ' sensible runtime to meet the zone load within the cooling convergence tolerance.')
      CALL ShowContinueErrorTimeStamp('Iterations='//TRIM(TrimSigDigits(MaxIter)))
    ENDIF
    CALL ShowRecurringWarningErrorAtEnd('ZoneHVAC:WindowAirConditioner="'//TRIM(WindAC(WindACNum)%Name)//  &
                 '"  -- Exceeded max iterations error (sensible runtime) continues...',WindAC(WindACNum)%MaxIterIndex1)
  END IF

  IF(WindAC(WindACNum)%DXCoilType_Num == CoilDX_CoolingHXAssisted .AND. &
     Node(WindAC(WindACNum)%CoilOutletNodeNum)%HumRatMax .LT. Node(WindAC(WindACNum)%CoilOutletNodeNum)%HumRat .AND. &
     Node(WindAC(WindACNum)%CoilOutletNodeNum)%HumRatMax .GT. 0.0d0) THEN

!   Run the HX to recovery energy and improve latent performance
    HXUnitOn = .TRUE.

!   Get full load result
    CALL CalcWindowACOutput(WindACNum,FirstHVACIteration,OpMode,1.0d0,HXUnitOn,FullOutput)

    IF(Node(WindAC(WindACNum)%CoilOutletNodeNum)%HumRatMax .LT. Node(WindAC(WindACNum)%CoilOutletNodeNum)%HumRat .OR. &
       QZnReq  <=  FullOutput) THEN
      PartLoadFrac = 1.0d0
      RETURN
    END IF

    Error = 1.0d0             !initialize error value for comparison against tolerance
    Iter = 0                !initialize iteration counter
    Relax = 1.0d0

    DO WHILE ((ABS(Error) .GT. ErrorToler) .AND. (Iter .LE. MaxIter) .AND. PartLoadFrac .GT. MinPLF)
      ! Get result when DX coil is operating at partloadfrac
      CALL CalcWindowACOutput(WindACNum,FirstHVACIteration,OpMode,PartLoadFrac,HXUnitOn,ActualOutput)
      Error = (QZnReq - ActualOutput)/QZnReq
      DelPLF = (QZnReq-ActualOutput)/FullOutput
      PartLoadFrac = PartLoadFrac + Relax * DelPLF
      PartLoadFrac = MAX(MinPLF,MIN(1.0d0,PartLoadFrac))
      Iter = Iter + 1
      IF (Iter == 16) THEN
        Relax = 0.5d0
      END IF
    END DO
    IF (Iter .GT. MaxIter) THEN
      IF (WindAC(WindACNum)%MaxIterIndex2 == 0) THEN
        CALL ShowWarningMessage('ZoneHVAC:WindowAirConditioner="'//TRIM(WindAC(WindACNum)%Name)//  &
           '" -- Exceeded max iterations while adjusting compressor'// &
           ' latent runtime to meet the zone load within the cooling convergence tolerance.')
        CALL ShowContinueErrorTimeStamp('Iterations='//TRIM(TrimSigDigits(MaxIter)))
      ENDIF
      CALL ShowRecurringWarningErrorAtEnd('ZoneHVAC:WindowAirConditioner="'//TRIM(WindAC(WindACNum)%Name)//  &
                 '"  -- Exceeded max iterations error (latent runtime) continues...',WindAC(WindACNum)%MaxIterIndex2)
    END IF

  END IF ! WindAC(WindACNum)%DXCoilType_Num == CoilDX_CoolingHXAssisted .AND. &

RETURN
END SUBROUTINE ControlCycWindACOutput

INTEGER FUNCTION GetWindowACZoneInletAirNode(WindACNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for zone inlet node

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: WindACNum          !

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (GetWindowACInputFlag) THEN
    CALL GetWindowAC
    GetWindowACInputFlag = .FALSE.
  END IF

  GetWindowACZoneInletAirNode = WindAC(WindACNum)%AirOutNode

  RETURN
END FUNCTION GetWindowACZoneInletAirNode

INTEGER FUNCTION GetWindowACOutAirNode(WindACNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for OA inlet node

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: WindACNum          !

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (GetWindowACInputFlag) THEN
    CALL GetWindowAC
    GetWindowACInputFlag = .FALSE.
  END IF

  GetWindowACOutAirNode = WindAC(WindACNum)%OutsideAirNode

  RETURN

END FUNCTION GetWindowACOutAirNode

INTEGER FUNCTION GetWindowACReturnAirNode(WindACNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for mixer return air node for ventilation load reporting

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE MixedAir, ONLY: GetOAMixerReturnNodeNumber

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: WindACNum          !

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (GetWindowACInputFlag) THEN
    CALL GetWindowAC
    GetWindowACInputFlag = .FALSE.
  END IF

  If (WindACNum > 0 .and. WindACNum <= NumWindAC) then
    IF (WindAC(WindACNum)%OAMixIndex > 0)  THEN
      GetWindowACReturnAirNode = GetOAMixerReturnNodeNumber(WindAC(WindACNum)%OAMixIndex)
    ELSE
      GetWindowACReturnAirNode = 0
    END IF
  ELSE
    GetWindowACReturnAirNode = 0
  END IF

  RETURN

END FUNCTION GetWindowACReturnAirNode

INTEGER FUNCTION GetWindowACMixedAirNode(WindACNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for mixed air node for ventilation rate reporting

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE MixedAir, ONLY: GetOAMixerMixedNodeNumber

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: WindACNum          !

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (GetWindowACInputFlag) THEN
    CALL GetWindowAC
    GetWindowACInputFlag = .FALSE.
  END IF

  If (WindACNum > 0 .and. WindACNum <= NumWindAC) then
    IF (WindAC(WindACNum)%OAMixIndex > 0) THEN
      GetWindowACMixedAirNode =  GetOAMixerMixedNodeNumber(WindAC(WindACNum)%OAMixIndex)
    ELSE
      GetWindowACMixedAirNode = 0
    ENDIF
  ELSE
    GetWindowACMixedAirNode = 0
  END IF

  RETURN

END FUNCTION GetWindowACMixedAirNode

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

END MODULE WindowAC
