! note that there are two modules in this file

!  HVACDXSystem is for cooling DX coils

!  HVACDXHeatPumpSystem is for heating DX coils


MODULE HVACDXSystem
  ! Module containing the DXCoolingSystem simulation routines

  ! MODULE INFORMATION:
  !       AUTHOR         Richard Liesen
  !       DATE WRITTEN   March 2001
  !                      Feb 2005 M. J. Witte, GARD Analytics, Inc.
  !                        Add dehumidification controls and support for multimode DX coil
  !                        Work supported by ASHRAE research project 1254-RP
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required to
  ! manage the DXCoolingSystem System Component

  ! METHODOLOGY EMPLOYED:
  !

  ! REFERENCES:


  ! OTHER NOTES:
  !

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals
USE DataHVACGlobals
USE DataInterfaces

  ! Use statements for access to subroutines in other modules
USE ScheduleManager

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS
REAL(r64), PARAMETER :: MinAirMassFlow = 0.001d0
! Compressor operation
INTEGER, PARAMETER :: On =  1              ! normal compressor operation
INTEGER, PARAMETER :: Off = 0              ! signal DXCoil that compressor shouldn't run
! Dehumidification control modes (DehumidControlMode)
INTEGER, PARAMETER :: DehumidControl_None       = 0
INTEGER, PARAMETER :: DehumidControl_Multimode  = 1
INTEGER, PARAMETER :: DehumidControl_CoolReheat = 2



  ! DERIVED TYPE DEFINITIONS

TYPE DXCoolingConditions
  CHARACTER(len=MaxNameLength) :: DXCoolingSystemType=' ' ! Type of DXCoolingSystem
  CHARACTER(len=MaxNameLength) :: Name               =' ' ! Name of the DXCoolingSystem
  INTEGER                      :: SchedPtr           =0
  CHARACTER(len=MaxNameLength) :: CoolingCoilType    =' ' !
  INTEGER                      :: CoolingCoilType_Num=0
  CHARACTER(len=MaxNameLength) :: CoolingCoilName    =' ' !
  INTEGER                      :: CoolingCoilIndex   =0
  INTEGER      :: DXCoolingCoilInletNodeNum          =0
  INTEGER      :: DXCoolingCoilOutletNodeNum         =0
  Integer      :: DXSystemControlNodeNum             =0   ! the node number of the node with the setpoint
  REAL(r64)    :: DesiredOutletTemp                  =0.0 ! the temperature at the unit outlet node needed
                                                          ! to meet the supply air setpoint.
  REAL(r64)    :: DesiredOutletHumRat                =1.0 ! the humidity ratio at the unit outlet node needed
                                                          ! to meet the supply air setpoint.
  REAL(r64)    :: PartLoadFrac                       =0.0 ! part load fraction for current time step (single speed)
  REAL(r64)    :: SpeedRatio                         =0.0 ! current compressor speed ratio (variable speed)
  REAL(r64)    :: CycRatio                           =0.0 ! cycling part load ratio (variable speed)
  LOGICAL      :: RunOnSensibleLoad              =.true.  ! logical determines if this system will run to
                                                          ! meet a sensible load - for future use
  LOGICAL      :: RunOnLatentLoad                =.false. ! logical determines if this system will run to
                                                          ! meet a latent-only load - for future use
  INTEGER      :: DehumidControlType                 =0   ! Dehumidification control type (currently only for multimode coil)
  INTEGER      :: DehumidificationMode               =0   ! Dehumidification mode for multimode coil,
                                                          ! 0=normal, 1+=enhanced dehumidification mode
  INTEGER      :: FanOpMode                          =0   ! Fan operating mode (see parameter above)

! Warning message variables
  INTEGER      :: HXAssistedSensPLRIter               =0   ! used in HX Assisted calculations
  INTEGER      :: HXAssistedSensPLRIterIndex          =0   ! used in HX Assisted calculations
  INTEGER      :: HXAssistedSensPLRFail               =0   ! used in HX Assisted calculations
  INTEGER      :: HXAssistedSensPLRFailIndex          =0   ! used in HX Assisted calculations
  INTEGER      :: HXAssistedSensPLRFail2              =0   ! used in HX Assisted calculations
  INTEGER      :: HXAssistedSensPLRFailIndex2         =0   ! used in HX Assisted calculations
  INTEGER      :: HXAssistedLatPLRIter                =0   ! used in HX Assisted calculations
  INTEGER      :: HXAssistedLatPLRIterIndex           =0   ! used in HX Assisted calculations
  INTEGER      :: HXAssistedLatPLRFail                =0   ! used in HX Assisted calculations
  INTEGER      :: HXAssistedLatPLRFailIndex           =0   ! used in HX Assisted calculations

  INTEGER      :: HXAssistedCRLatPLRIter              =0   ! used in HX Assisted calculations
  INTEGER      :: HXAssistedCRLatPLRIterIndex         =0   ! used in HX Assisted calculations
  INTEGER      :: HXAssistedCRLatPLRFail              =0   ! used in HX Assisted calculations
  INTEGER      :: HXAssistedCRLatPLRFailIndex         =0   ! used in HX Assisted calculations
  INTEGER      :: HXAssistedCRLatPLRFail2             =0   ! used in HX Assisted calculations
  INTEGER      :: HXAssistedCRLatPLRFailIndex2        =0   ! used in HX Assisted calculations

  INTEGER      :: DXCoilSensPLRIter                   =0   ! used in DXCoil calculations
  INTEGER      :: DXCoilSensPLRIterIndex              =0   ! used in DXCoil calculations
  INTEGER      :: DXCoilSensPLRFail                   =0   ! used in DXCoil calculations
  INTEGER      :: DXCoilSensPLRFailIndex              =0   ! used in DXCoil calculations
  INTEGER      :: DXCoilLatPLRIter                    =0   ! used in DXCoil calculations
  INTEGER      :: DXCoilLatPLRIterIndex               =0   ! used in DXCoil calculations
  INTEGER      :: DXCoilLatPLRFail                    =0   ! used in DXCoil calculations
  INTEGER      :: DXCoilLatPLRFailIndex               =0   ! used in DXCoil calculations

  INTEGER      :: MSpdSensPLRIter                     =0   ! used in MultiSpeed calculations
  INTEGER      :: MSpdSensPLRIterIndex                =0   ! used in MultiSpeed calculations
  INTEGER      :: MSpdCycSensPLRIter                  =0   ! used in MultiSpeed calculations
  INTEGER      :: MSpdCycSensPLRIterIndex             =0   ! used in MultiSpeed calculations
  INTEGER      :: MSpdLatPLRIter                      =0   ! used in MultiSpeed calculations
  INTEGER      :: MSpdLatPLRIterIndex                 =0   ! used in MultiSpeed calculations
  INTEGER      :: MSpdCycLatPLRIter                   =0   ! used in MultiSpeed calculations
  INTEGER      :: MSpdCycLatPLRIterIndex              =0   ! used in MultiSpeed calculations

  INTEGER      :: MModeSensPLRIter                    =0   ! used in MultiMode calculations
  INTEGER      :: MModeSensPLRIterIndex               =0   ! used in MultiMode calculations
  INTEGER      :: MModeLatPLRIter                     =0   ! used in MultiMode calculations
  INTEGER      :: MModeLatPLRIterIndex                =0   ! used in MultiMode calculations
  INTEGER      :: MModeLatPLRIter2                    =0   ! used in MultiMode calculations
  INTEGER      :: MModeLatPLRIterIndex2               =0   ! used in MultiMode calculations
! When the Dx system is a part of Outdoor Air Unit
  REAL(r64)    :: OAUnitSetTemp                        =0.0 ! set

END TYPE DXCoolingConditions

!MODULE VARIABLE DECLARATIONS:
INTEGER :: NumDXSystem=0   ! The Number of DXCoolingSystems found in the Input
LOGICAL :: EconomizerFlag=.FALSE. ! holds air loop economizer status

! Make this type allocatable
TYPE (DXCoolingConditions), ALLOCATABLE, DIMENSION(:) :: DXCoolingSystem
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

! Subroutine Specifications for the Module
          ! Driver/Manager Routines
Public  SimDXCoolingSystem

          ! Get Input routines for module
PRIVATE GetDXCoolingSystemInput

PRIVATE InitDXCoolingSystem

          ! Update routine to check convergence and update nodes
Private ControlDXSystem

PRIVATE DXCoilVarSpeedResidual
PRIVATE DXCoilVarSpeedHumRatResidual
PRIVATE DXCoilCyclingResidual
PRIVATE DXCoilCyclingHumRatResidual
PRIVATE DOE2DXCoilResidual
PRIVATE DOE2DXCoilHumRatResidual
PRIVATE MultiModeDXCoilResidual
PRIVATE MultiModeDXCoilHumRatResidual
PRIVATE HXAssistedCoolCoilTempResidual
PRIVATE HXAssistedCoolCoilHRResidual


CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE SimDXCoolingSystem(DXCoolingSystemName, FirstHVACIteration, AirLoopNum,CompIndex,OAUnitNum,OAUCoilOutTemp,QTotOut)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   Mar 2001
          !       MODIFIED       Richard Raustad, Sept 2003 (added HVACHXAssistedCoolingCoil)
          !                      Feb 2005 M. J. Witte, GARD Analytics, Inc.
          !                        Add support for multimode DX coil
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages DXCoolingSystem component simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DXCoils,          ONLY: SimDXCoil, SimDXCoilMultiSpeed, SimDXCoilMultiMode
  USE General,          ONLY: TrimSigDigits
  USE DataAirLoop,      ONLY: AirLoopControlInfo
  USE InputProcessor,   ONLY: FindItemInList
  USE HVACHXAssistedCoolingCoil, ONLY: SimHXAssistedCoolingCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: DXCoolingSystemName ! Name of DXSystem:Airloop object
  LOGICAL, INTENT(IN)          :: FirstHVACIteration  ! True when first HVAC iteration
  INTEGER, INTENT(IN)          :: AirLoopNum          ! Primary air loop number
  INTEGER, INTENT(INOUT)       :: CompIndex           ! Index to DXSystem:Airloop object
  INTEGER, INTENT(IN), OPTIONAL:: OAUnitNum           ! If the system is an equipment of OutdoorAirUnit
  REAL(r64), INTENT(IN), OPTIONAL :: OAUCoilOutTemp   ! the coil inlet temperature of OutdoorAirUnit
  REAL(r64), INTENT(INOUT), OPTIONAL :: QTotOut       ! the total cooling output of unit
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DEFINITIONS:
          ! na

          ! FLOW:


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength)  :: CompName              ! Name of DXSystem:Airloop object
  INTEGER                       :: DXSystemNum           ! Index to DXSystem:Airloop object
  LOGICAL,SAVE                  :: GetInputFlag = .True. ! Flag to get input only once
  LOGICAL                       :: HXUnitOn              ! Flag to control HX for HXAssisted Cooling Coil
  REAL(r64)                     :: AirMassFlow           ! DX System air mass flow rate
  INTEGER                       :: InletNodeNum          ! DX System inlet node number
  INTEGER                       :: OutletNodeNum         ! DX System outlet node number

    ! Obtains and Allocates DX Cooling System related parameters from input file
  IF (GetInputFlag) THEN  !First time subroutine has been entered
        !Get the DXCoolingSystem input
    CALL GetDXCoolingSystemInput
    GetInputFlag=.false.
  End If

    ! Find the correct DXSystemNumber
  IF (CompIndex == 0) THEN
    DXSystemNum = FindItemInList(DXCoolingSystemName,DXCoolingSystem%Name,NumDXSystem)
    IF (DXSystemNum == 0) THEN
      CALL ShowFatalError('SimDXCoolingSystem: DXUnit not found='//TRIM(DXCoolingSystemName))
    ENDIF
    CompIndex=DXSystemNum
  ELSE
    DXSystemNum=CompIndex
    IF (DXSystemNum > NumDXSystem .or. DXSystemNum < 1) THEN
      CALL ShowFatalError('SimulateDXCoolingSystem:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(DXSystemNum))// &
                          ', Number of DX Units='//TRIM(TrimSigDigits(NumDXSystem))//  &
                          ', DX Unit name='//TRIM(DXCoolingSystemName))
    ENDIF
    IF (CheckEquipName(DXSystemNum)) THEN
      IF (DXCoolingSystemName /= DXCoolingSystem(DXSystemNum)%Name) THEN
        CALL ShowFatalError('SimulateDXCoolingSystem: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(DXSystemNum))// &
                            ', DX Unit name='//TRIM(DXCoolingSystemName)//', stored DX Unit Name for that index='//  &
                            TRIM(DXCoolingSystem(DXSystemNum)%Name))
      ENDIF
      CheckEquipName(DXSystemNum)=.false.
    ENDIF
  ENDIF

  Call InitDXCoolingSystem(DXSystemNum,AirLoopNum,OAUnitNUm,OAUCoilOutTemp)

  !Call the series of components that simulate a DX Cooling System
   ! Control the DX Cooling System
  HXUnitOn = .FALSE.
  Call ControlDXSystem(DXSystemNum, FirstHVACIteration, HXUnitOn)

    ! simulate DX Cooling System
  CompName = DXCoolingSystem(DXSystemNum)%CoolingCoilName
  !Need a cooling System call here I think
    SELECT CASE(DXCoolingSystem(DXSystemNum)%CoolingCoilType_Num)

    CASE (CoilDX_CoolingSingleSpeed) ! COIL:DX:COOLINGBYPASSFACTOREMPIRICAL

      CALL SimDXCoil(CompName,On,FirstHVACIteration, DXCoolingSystem(DXSystemNum)%PartLoadFrac,  &
         DXCoolingSystem(DXSystemNum)%CoolingCoilIndex, &
         DXCoolingSystem(DXSystemNum)%FanOpMode)

    CASE (CoilDX_CoolingHXAssisted)  ! CoilSystem:Cooling:DX:HeatExchangerAssisted

      CALL SimHXAssistedCoolingCoil(CompName,FirstHVACIteration,On,DXCoolingSystem(DXSystemNum)%PartLoadFrac,  &
                                   DXCoolingSystem(DXSystemNum)%CoolingCoilIndex, DXCoolingSystem(DXSystemNum)%FanOpMode, &
                                   HXUnitEnable=HXUnitOn,EconomizerFlag=EconomizerFlag)

    CASE (CoilDX_CoolingTwoSpeed)  ! Coil:Cooling:DX:TwoSpeed 
                                   ! formerly (v3 and beyond)COIL:DX:MULTISPEED:COOLINGEMPIRICAL

      CALL SimDXCoilMultiSpeed(CompName,DXCoolingSystem(DXSystemNum)%SpeedRatio,&
                     DXCoolingSystem(DXSystemNum)%CycRatio,DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)

    CASE (CoilDX_CoolingTwoStageWHumControl)  ! Coil:Cooling:DX:TwoStageWithHumidityControlMode 
                                     ! formerly (v3 and beyond) COIL:DX:MULTIMODE:COOLINGEMPIRICAL

      CALL SimDXCoilMultiMode(CompName,On,FirstHVACIteration,DXCoolingSystem(DXSystemNum)%PartLoadFrac, &
                     DXCoolingSystem(DXSystemNum)%DehumidificationMode,DXCoolingSystem(DXSystemNum)%CoolingCoilIndex, &
                     DXCoolingSystem(DXSystemNum)%FanOpMode)

    CASE DEFAULT
      CALL ShowFatalError('SimDXCoolingSystem: Invalid DX Cooling System/Coil='//  &
                          TRIM(DXCoolingSystem(DXSystemNum)%CoolingCoilType))

  END SELECT
  ! set econo lockout flag
    ! set econo lockout flag
  IF (AirLoopNum /=-1) THEN ! IF the sysem is not an equipment of outdoor air unit

  IF ( (DXCoolingSystem(DXSystemNum)%PartLoadFrac > 0.0 .OR. &
        DXCoolingSystem(DXSystemNum)%SpeedRatio > 0.0 .OR. &
        DXCoolingSystem(DXSystemNum)%CycRatio > 0.0) .AND. &
       AirLoopControlInfo(AirLoopNum)%CanLockoutEconoWithCompressor) THEN
       AirLoopControlInfo(AirLoopNum)%ReqstEconoLockoutWithCompressor = .TRUE.
  ELSE
    AirLoopControlInfo(AirLoopNum)%ReqstEconoLockoutWithCompressor = .FALSE.
  END IF
  END IF

  IF(PRESENT(QTotOut))THEN
    InletNodeNum  = DXCoolingSystem(DXSystemNum)%DXCoolingCoilInletNodeNum
    OutletNodeNum = DXCoolingSystem(DXSystemNum)%DXCoolingCoilOutletNodeNum
    AirMassFlow = Node(OutletNodeNum)%MassFlowRate
    QTotOut = AirMassFlow * (Node(InletNodeNum)%Enthalpy - Node(OutletNodeNum)%Enthalpy)
  END IF

  RETURN

END SUBROUTINE SimDXCoolingSystem

! Get Input Section of the Module
!******************************************************************************
SUBROUTINE GetDXCoolingSystemInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   Mar 2001
          !                      Feb 2005 M. J. Witte, GARD Analytics, Inc.
          !                        Add dehumidification controls and support for multimode DX coil
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for system and stores it in System data structures

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in data.

          ! REFERENCES:

          ! USE STATEMENTS:
    USE InputProcessor
    USE NodeInputManager,      ONLY: GetOnlySingleNode
    USE DataHeatBalance,       ONLY: Zone
    USE BranchNodeConnections, ONLY: SetUpCompSets, TestCompSet
    USE HVACHXAssistedCoolingCoil,  ONLY: GetHXDXCoilName
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
    INTEGER :: DXSystemNum      ! The DXCoolingSystem that you are currently loading input into
    INTEGER :: NumAlphas
    INTEGER :: NumNums
    INTEGER :: IOSTAT
    CHARACTER(len=*), PARAMETER    :: RoutineName='GetDXCoolingSystemInput: ' ! include trailing blank space
    LOGICAL :: ErrorsFound = .false.   ! If errors detected in input
    LOGICAL :: IsNotOK               ! Flag to verify name
    LOGICAL :: IsBlank               ! Flag for blank name
    Integer :: DXCoolSysNum
    LOGICAL :: FanErrorsFound        ! flag returned on fan operating mode check
    LOGICAL :: DXErrorsFound         ! flag returned on DX coil name check
    CHARACTER(len=MaxNameLength) :: HXDXCoolCoilName     ! Name of DX cooling coil used with Heat Exchanger Assisted Cooling Coil
    CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in getting objects
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas         ! Alpha input items for object
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
    REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers           ! Numeric input items for object
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.
    INTEGER                              :: TotalArgs=0       ! Total number of alpha and numeric arguments (max) for a
                                                              !  certain object in the input file

          ! Flow

    CurrentModuleObject='CoilSystem:Cooling:DX'
    NumDXSystem = GetNumObjectsFound(TRIM(CurrentModuleObject))

    ALLOCATE(DXCoolingSystem(NumDXSystem))
    ALLOCATE(CheckEquipName(NumDXSystem))
    CheckEquipName=.true.

    CALL GetObjectDefMaxArgs('CoilSystem:Cooling:DX',TotalArgs,NumAlphas,NumNums)

    ALLOCATE(Alphas(NumAlphas))
    Alphas=' '
    ALLOCATE(cAlphaFields(NumAlphas))
    cAlphaFields=' '
    ALLOCATE(cNumericFields(NumNums))
    cNumericFields=' '
    ALLOCATE(Numbers(NumNums))
    Numbers=0.0
    ALLOCATE(lAlphaBlanks(NumAlphas))
    lAlphaBlanks=.TRUE.
    ALLOCATE(lNumericBlanks(NumNums))
    lNumericBlanks=.TRUE.


      ! Get the data for the DX Cooling System
      DO DXCoolSysNum = 1,  NumDXSystem

        CALL GetObjectItem(TRIM(CurrentModuleObject),DXCoolSysNum,Alphas,NumAlphas, &
                     Numbers,NumNums,IOSTAT,NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                     AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(Alphas(1),DXCoolingSystem%Name,DXCoolSysNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) Alphas(1) ='xxxxx'
        ENDIF
        DXCoolingSystem(DXCoolSysNum)%DXCoolingSystemType = CurrentModuleObject ! push Object Name into data array
        DXCoolingSystem(DXCoolSysNum)%Name            = Alphas(1)
        DXCoolingSystem(DXCoolSysNum)%SchedPtr        = GetScheduleIndex(Alphas(2))
        IF (DXCoolingSystem(DXCoolSysNum)%SchedPtr == 0) THEN
          IF (lAlphaBlanks(2)) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': '//TRIM(cAlphaFields(2))//  &
                 ' is required, missing for '//TRIM(cAlphaFields(1))//'='//TRIM(Alphas(1)))
          ELSE
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': invalid '//TRIM(cAlphaFields(2))//  &
               ' entered ='//TRIM(Alphas(2))// &
               ' for '//TRIM(cAlphaFields(1))//'='//TRIM(Alphas(1)))
          END IF
          ErrorsFound=.true.
        END IF

        DXCoolingSystem(DXCoolSysNum)%DXCoolingCoilInletNodeNum      = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)
        DXCoolingSystem(DXCoolSysNum)%DXCoolingCoilOutletNodeNum     = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)

        CALL TestCompSet(TRIM(CurrentModuleObject),Alphas(1),Alphas(3),Alphas(4),'Air Nodes')

        DXCoolingSystem(DXCoolSysNum)%DXSystemControlNodeNum      = &
               GetOnlySingleNode(Alphas(5),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
                            NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsParent)
        IF (DXCoolingSystem(DXCoolSysNum)%DXSystemControlNodeNum .EQ. 0) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//': control node must be input')
          CALL ShowContinueError('Error occurred in '//TRIM(cAlphaFields(1))//'='//TRIM(Alphas(1)))
          ErrorsFound=.true.
        ENDIF

        ! Get Cooling System Information if available
        IF (SameString(Alphas(6),'Coil:Cooling:DX:SingleSpeed') .OR. &
            SameString(Alphas(6),'Coil:Cooling:DX:TwoSpeed') .OR. &
            SameString(Alphas(6),'Coil:Cooling:DX:TwoStageWithHumidityControlMode') .OR. &
            SameString(Alphas(6),'CoilSystem:Cooling:DX:HeatExchangerAssisted')) THEN

          DXCoolingSystem(DXCoolSysNum)%CoolingCoilType = Alphas(6)
          IF (SameString(Alphas(6),'Coil:Cooling:DX:SingleSpeed')) THEN
            DXCoolingSystem(DXCoolSysNum)%CoolingCoilType_Num=CoilDX_CoolingSingleSpeed
          ELSEIF (SameString(Alphas(6),'Coil:Cooling:DX:TwoSpeed')) THEN
            DXCoolingSystem(DXCoolSysNum)%CoolingCoilType_Num=CoilDX_CoolingTwoSpeed
          ELSEIF (SameString(Alphas(6),'CoilSystem:Cooling:DX:HeatExchangerAssisted')) THEN
            DXCoolingSystem(DXCoolSysNum)%CoolingCoilType_Num=CoilDX_CoolingHXAssisted
          ELSEIF (SameString(Alphas(6),'Coil:Cooling:DX:TwoStageWithHumidityControlMode')) THEN
            DXCoolingSystem(DXCoolSysNum)%CoolingCoilType_Num=CoilDX_CoolingTwoStageWHumControl
          ENDIF

          DXCoolingSystem(DXCoolSysNum)%CoolingCoilName = Alphas(7)

        ELSE
          CALL ShowSevereError('Invalid entry for '//TRIM(cAlphaFields(6))//' :'//TRIM(Alphas(6)))
          CALL ShowContinueError('In '//TRIM(CurrentModuleObject)//'="'//TRIM(DXCoolingSystem(DXCoolSysNum)%Name)//'".')
          ErrorsFound=.true.
        END IF

        CALL ValidateComponent(DXCoolingSystem(DXCoolSysNum)%CoolingCoilType,DXCoolingSystem(DXCoolSysNum)%CoolingCoilName, &
                               IsNotOK,TRIM(CurrentModuleObject))
        IF (IsNotOK) THEN
          CALL ShowContinueError('In '//TRIM(CurrentModuleObject)//' = "'//TRIM(DXCoolingSystem(DXCoolSysNum)%Name)//'".')
          ErrorsFound=.true.
        ENDIF

        CALL SetUpCompSets(DXCoolingSystem(DXCoolSysNum)%DXCoolingSystemType, DXCoolingSystem(DXCoolSysNum)%Name, &
                           Alphas(6),Alphas(7),Alphas(3),Alphas(4))


        FanErrorsFound = .FALSE.
        IF(SameString(Alphas(6),'CoilSystem:Cooling:DX:HeatExchangerAssisted')) THEN
          DXErrorsFound = .FALSE.
          HXDXCoolCoilName = GetHXDXCoilName(Alphas(6),Alphas(7),DXErrorsFound)
          IF(DXErrorsFound)THEN
            CALL ShowWarningError(TRIM(CurrentModuleObject)//' = "'//TRIM(DXCoolingSystem(DXCoolSysNum)%Name)//'"')
            CALL ShowContinueError('CoilSystem:Cooling:DX:HeatExchangerAssisted "'//TRIM(Alphas(7))//'" not found.')
            ErrorsFound = .TRUE.
          END IF
        END IF

        ! Supply air fan operating mode defaulted to constant fan cycling coil/compressor
        DXCoolingSystem(DXCoolSysNum)%FanOpMode = ContFanCycCoil

        ! Dehumidification control mode
        IF (SameString(Alphas(8),'None')) THEN
          DXCoolingSystem(DXCoolSysNum)%DehumidControlType=DehumidControl_None
        ELSEIF (SameString(Alphas(8),' ')) THEN
          DXCoolingSystem(DXCoolSysNum)%DehumidControlType=DehumidControl_None
        ELSEIF (SameString(Alphas(8),'Multimode')) THEN
          IF (DXCoolingSystem(DXCoolSysNum)%CoolingCoilType_Num == CoilDX_CoolingTwoStageWHumControl) THEN
            DXCoolingSystem(DXCoolSysNum)%DehumidControlType=DehumidControl_Multimode
          ELSE IF (DXCoolingSystem(DXCoolSysNum)%CoolingCoilType_Num == CoilDX_CoolingHXAssisted) THEN
            DXCoolingSystem(DXCoolSysNum)%DehumidControlType=DehumidControl_Multimode
          ELSE
            CALL ShowWarningError('Invalid entry for '//TRIM(cAlphaFields(8))//' :'//TRIM(Alphas(8)))
            CALL ShowContinueError('In '//TRIM(CurrentModuleObject)//'="'//TRIM(DXCoolingSystem(DXCoolSysNum)%Name)//'".')
            CALL ShowContinueError('Valid only with cooling coil type = Coil:Cooling:DX:TwoStageWithHumidityControlMode or '// &
                                    'CoilSystem:Cooling:DX:HeatExchangerAssisted.')
            CALL ShowContinueError('Setting '//TRIM(cAlphaFields(8))//' to None.')
            DXCoolingSystem(DXCoolSysNum)%DehumidControlType=DehumidControl_None
          END IF
        ELSEIF (SameString(Alphas(8),'CoolReheat')) THEN
          DXCoolingSystem(DXCoolSysNum)%DehumidControlType=DehumidControl_CoolReheat
        ELSE
          CALL ShowSevereError('Invalid entry for '//TRIM(cAlphaFields(8))//' :'//TRIM(Alphas(8)))
          CALL ShowContinueError('In '//TRIM(CurrentModuleObject)//'="'//TRIM(DXCoolingSystem(DXCoolSysNum)%Name)//'".')
        END IF

        ! Run on sensible load
        IF (SameString(Alphas(9),'Yes')) THEN
          DXCoolingSystem(DXCoolSysNum)%RunOnSensibleLoad= .TRUE.
        ELSEIF (SameString(Alphas(9),' ')) THEN
          DXCoolingSystem(DXCoolSysNum)%RunOnSensibleLoad= .TRUE.
        ELSEIF (SameString(Alphas(9),'No')) THEN
          DXCoolingSystem(DXCoolSysNum)%RunOnSensibleLoad= .FALSE.
        ELSE
          CALL ShowSevereError('Invalid entry for '//TRIM(cAlphaFields(9))//' :'//TRIM(Alphas(9)))
          CALL ShowContinueError('In '//TRIM(CurrentModuleObject)//'="'//TRIM(DXCoolingSystem(DXCoolSysNum)%Name)//'".')
          CALL ShowContinueError('Must be Yes or No.')
        END IF

        ! Run on latent load
        IF (SameString(Alphas(10),'Yes')) THEN
          DXCoolingSystem(DXCoolSysNum)%RunOnLatentLoad= .TRUE.
        ELSEIF (SameString(Alphas(10),' ')) THEN
          DXCoolingSystem(DXCoolSysNum)%RunOnLatentLoad= .FALSE.
        ELSEIF (SameString(Alphas(10),'No')) THEN
          DXCoolingSystem(DXCoolSysNum)%RunOnLatentLoad= .FALSE.
        ELSE
          CALL ShowSevereError('Invalid entry for '//TRIM(cAlphaFields(10))//' :'//TRIM(Alphas(10)))
          CALL ShowContinueError('In '//TRIM(CurrentModuleObject)//'="'//TRIM(DXCoolingSystem(DXCoolSysNum)%Name)//'".')
          CALL ShowContinueError('Must be Yes or No.')
        END IF

      END DO  !End of the DX System Loop


      IF (ErrorsFound) THEN
          CALL ShowFatalError(RoutineName//'Errors found in input.  Program terminates.')
      ENDIF

      DO DXSystemNum=1,NumDXSystem
        ! Setup Report variables for the DXCoolingSystem that is not reported in the components themselves
        IF (SameString(DXCoolingSystem(DXSystemNum)%CoolingCoilType,'Coil:Cooling:DX:Twospeed') ) THEN
          CALL SetupOutputVariable('Coil System Cycling Part Load Ratio []',DXCoolingSystem(DXSystemNum)%CycRatio, &
                                'System','Average',DXCoolingSystem(DXSystemNum)%Name)
          CALL SetupOutputVariable('Coil System Compressor Speed Ratio []',DXCoolingSystem(DXSystemNum)%SpeedRatio, &
                                'System','Average',DXCoolingSystem(DXSystemNum)%Name)
        ELSE
          CALL SetupOutputVariable('Coil System Part Load Ratio []',DXCoolingSystem(DXSystemNum)%PartLoadFrac, &
                                'System','Average',DXCoolingSystem(DXSystemNum)%Name)
        END IF
      END DO

      DEALLOCATE(Alphas)
      DEALLOCATE(cAlphaFields)
      DEALLOCATE(cNumericFields)
      DEALLOCATE(Numbers)
      DEALLOCATE(lAlphaBlanks)
      DEALLOCATE(lNumericBlanks)

  RETURN

END SUBROUTINE GetDXCoolingSystemInput


! End of Get Input subroutines for the Module
!******************************************************************************

! Beginning of Initialization subroutines for the Module
! *****************************************************************************

SUBROUTINE InitDXCoolingSystem(DXSystemNum,AirLoopNum,OAUnitNum,OAUCoilOutTemp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2001
          !                      Feb 2005 M. J. Witte, GARD Analytics, Inc.
          !                        Add dehumidification controls
          !                      May 2009, B. Griffith, NREL added EMS setpoint checks
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the DX Cooling Systems.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY: DoSetPointTest
  USE DataAirLoop,     ONLY: AirLoopControlInfo
  USE EMSManager,      ONLY: iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS, iHumidityRatioMaxSetpoint
  USE DataGlobals,     ONLY: AnyEnergyManagementSystemInModel

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: DXSystemNum ! number of the current DX Sys being simulated
  INTEGER, INTENT (IN) :: AirLoopNum  ! number of the current air loop being simulated
  INTEGER, INTENT (IN), Optional :: OAUnitNum  ! number of the current outdoor air unit being simulated
  REAL(r64), INTENT(IN), OPTIONAL :: OAUCoilOutTemp    ! the coil inlet temperature of OutdoorAirUnit

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  Integer             :: OutNode ! outlet node number
  INTEGER             :: ControlNode ! control node number
  INTEGER             :: DXSysIndex
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL,SAVE        :: MySetPointCheckFlag = .TRUE.
  INTEGER             :: OutdoorAirUnitNum ! "ONLY" for ZoneHVAC:OutdoorAirUnit
  REAL(r64)           :: OAUCoilOutletTemp  ! "ONLY" for zoneHVAC:OutdoorAirUnit
          ! FLOW:

IF (MyOneTimeFlag) THEN

  MyOneTimeFlag = .false.
END IF
IF (AirLoopNum .EQ.-1) THEN ! This Dx system is component of ZoneHVAC:OutdoorAirUnit
   OutdoorAirUnitNum=OAUnitNum
   OAUCoilOutletTemp=OAUCoilOutTemp
END IF

IF ( .NOT. SysSizingCalc .AND. MySetPointCheckFlag .AND. DoSetPointTest) THEN
  DO DXSysIndex=1,NumDXSystem
    ControlNode = DXCoolingSystem(DXSysIndex)%DXSystemControlNodeNum
    IF (ControlNode > 0) THEN
     IF (AirLoopNum .EQ.-1) THEN                           ! Outdoor Air Unit
       Node(ControlNode)%TempSetPoint = OAUCoilOutletTemp  ! Set the coil outlet temperature
     ELSE IF (AirLoopNum /= -1) THEN ! Not an outdoor air unit

      IF (Node(ControlNode)%TempSetPoint == SensedNodeFlagValue) THEN
        IF (.NOT. AnyEnergyManagementSystemInModel) THEN
          CALL ShowSevereError(TRIM(DXCoolingSystem(DXSysIndex)%DXCoolingSystemType)//&
                               ': Missing temperature setpoint for DX unit= ' //TRIM(DXCoolingSystem(DXSysIndex)%Name))
          CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the unit control node.')
          SetPointErrorFlag = .TRUE.
        ELSE
          CALL CheckIfNodeSetpointManagedByEMS(ControlNode,iTemperatureSetpoint, SetpointErrorFlag)
          IF (SetpointErrorFlag) THEN
            CALL ShowSevereError(TRIM(DXCoolingSystem(DXSysIndex)%DXCoolingSystemType)//&
                               ': Missing temperature setpoint for DX unit= ' //TRIM(DXCoolingSystem(DXSysIndex)%Name))
            CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the unit control node.')
            CALL ShowContinueError('  or use an EMS actuator to establish a temperature setpoint at the unit control node.')
          ENDIF
        ENDIF
      END IF
      IF ((DXCoolingSystem(DXSysIndex)%DehumidControlType .NE. DehumidControl_None) .AND. &
          (Node(ControlNode)%HumRatMax == SensedNodeFlagValue)) THEN
        IF (.NOT. AnyEnergyManagementSystemInModel) THEN
          CALL ShowSevereError(TRIM(DXCoolingSystem(DXSysIndex)%DXCoolingSystemType)//&
                               ': Missing humidity ratio setpoint (HUMRATMAX) for DX unit= ' &
                                          //TRIM(DXCoolingSystem(DXSysIndex)%Name))
          CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the unit control node.')
          SetPointErrorFlag = .TRUE.
        ELSE
          CALL CheckIfNodeSetpointManagedByEMS(ControlNode,iHumidityRatioMaxSetpoint, SetpointErrorFlag)
          IF (SetpointErrorFlag) THEN
            CALL ShowSevereError(TRIM(DXCoolingSystem(DXSysIndex)%DXCoolingSystemType)//&
                               ': Missing maximum humidity ratio setpoint (HUMRATMAX) for DX unit= ' &
                                         //TRIM(DXCoolingSystem(DXSysIndex)%Name))
            CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the unit control node.')
            CALL ShowContinueError('  or use an EMS actuator to establish a maximum humidity ratio setpoint.')
          ENDIF
        ENDIF

      END IF
    END IF
   END IF
  END DO
  MySetPointCheckFlag = .FALSE.
END IF

! These initializations are done every iteration
IF (AirLoopNum .EQ.-1) THEN ! This IF-TEHN routine is just for ZoneHVAC:OUTDOORAIRUNIT
OutNode = DXCoolingSystem(DXSystemNum)%DXCoolingCoilOutletNodeNum
ControlNode = DXCoolingSystem(DXSystemNum)%DXSystemControlNodeNum

  IF (ControlNode.EQ.0) THEN
  DXCoolingSystem(DXSystemNum)%DesiredOutletTemp = 0.0
  DXCoolingSystem(DXSystemNum)%DesiredOutletHumRat = 1.0
  ELSE IF (ControlNode.EQ.OutNode) THEN
  DXCoolingSystem(DXSystemNum)%DesiredOutletTemp =OAUCoilOutletTemp
  END IF
 !  If the Dxsystem is an equipment of Outdoor Air Unit, the desiered coiloutlet humidity level is set to zero
    DXCoolingSystem(DXSystemNum)%DesiredOutletHumRat = 1.0


ELSE IF (AirLoopNum /=-1) THEN ! Not Outdoor Air Unit

OutNode = DXCoolingSystem(DXSystemNum)%DXCoolingCoilOutletNodeNum
ControlNode = DXCoolingSystem(DXSystemNum)%DXSystemControlNodeNum
EconomizerFlag = AirLoopControlInfo(AirLoopNum)%EconoActive
IF (ControlNode.EQ.0) THEN
  DXCoolingSystem(DXSystemNum)%DesiredOutletTemp = 0.0
  DXCoolingSystem(DXSystemNum)%DesiredOutletHumRat = 1.0
ELSE IF (ControlNode.EQ.OutNode) THEN
  DXCoolingSystem(DXSystemNum)%DesiredOutletTemp = Node(ControlNode)%TempSetPoint
  !  If HumRatMax is zero, then there is no request from SetpointManager:SingleZone:Humidity:Maximum
  IF ((DXCoolingSystem(DXSystemNum)%DehumidControlType .NE. DehumidControl_None) .AND. &
      (Node(ControlNode)%HumRatMax .GT. 0.0)) THEN
    DXCoolingSystem(DXSystemNum)%DesiredOutletHumRat = Node(ControlNode)%HumRatMax
  ELSE
    DXCoolingSystem(DXSystemNum)%DesiredOutletHumRat = 1.0
  END IF
ELSE
  DXCoolingSystem(DXSystemNum)%DesiredOutletTemp = Node(ControlNode)%TempSetPoint - &
    (Node(ControlNode)%Temp - Node(OutNode)%Temp)
  IF (DXCoolingSystem(DXSystemNum)%DehumidControlType .NE. DehumidControl_None) THEN
    DXCoolingSystem(DXSystemNum)%DesiredOutletHumRat = Node(ControlNode)%HumRatMax - &
      (Node(ControlNode)%HumRat - Node(OutNode)%HumRat)
  ELSE
    DXCoolingSystem(DXSystemNum)%DesiredOutletHumRat = 1.0
  END IF
END IF
END IF
RETURN
END SUBROUTINE InitDXCoolingSystem

! End of Initialization subroutines for the Module
! *****************************************************************************

! Beginning of Calculation subroutines for the DXCoolingSystem Module
! *****************************************************************************

SUBROUTINE ControlDXSystem(DXSystemNum, FirstHVACIteration, HXUnitOn)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   Feb 2001
          !       MODIFIED       Richard Raustad, FSEC Nov 2003
          !                      Feb 2005 M. J. Witte, GARD Analytics, Inc.
          !                        Add dehumidification controls and support for multimode DX coil
          !                      Jan 2008 R. Raustad, FSEC. Added coolreheat to all coil types
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  This subroutine updates the System outlet nodes.

          ! METHODOLOGY EMPLOYED:
          !  Data is moved from the System data structure to the System outlet nodes.

          ! REFERENCES:
          !  na

          ! USE STATEMENTS:
  USE ScheduleManager
  USE DataEnvironment, ONLY: OutBaroPress
  USE DataHVACGlobals, ONLY: TempControlTol
  USE InputProcessor,  ONLY: FindItemInList
  USE Psychrometrics , ONLY: PsyHFnTdbW, PsyTdpFnWPb
  USE General,         ONLY: SolveRegulaFalsi, RoundSigDigits
  USE DXCoils,         ONLY: SimDXCoil, SimDXCoilMultiSpeed, DXCoilOutletTemp, SimDXCoilMultiMode, DXCoilOutletHumRat
  USE HVACHXAssistedCoolingCoil, ONLY: SimHXAssistedCoolingCoil, HXAssistedCoilOutletTemp, HXAssistedCoilOutletHumRat


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,  Intent(In)    :: DXSystemNum             ! index to DXSystem
  LOGICAL,  Intent(In)    :: FirstHVACIteration      ! First HVAC iteration flag
  LOGICAL,  Intent(InOut) :: HXUnitOn                ! flag to enable heat exchanger heat recovery

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER          :: MaxIte    = 500     ! Maximum number of iterations for solver
  REAL(r64), PARAMETER :: Acc       = 1.d-3   ! Accuracy of solver result
  REAL(r64), PARAMETER :: HumRatAcc = 1.d-6   ! Accuracy of solver result

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength)  :: CompName  ! Name of the DX cooling coil
  REAL(r64)           :: NoOutput            ! Sensible capacity (outlet - inlet) when the compressor is off
  REAL(r64)           :: FullOutput          ! Sensible capacity (outlet - inlet) when the compressor is on
  REAL(r64)           :: ReqOutput           ! Sensible capacity (outlet - inlet) required to meet load or setpoint temperature
  Integer             :: InletNode           ! Inlet node number of the DX cooling coil
  Integer             :: OutletNode          ! Outlet node number of the DX cooling coil
  Integer             :: ControlNode         ! The node number where a setpoint is placed to control the DX cooling coil
  REAL(r64)           :: PartLoadFrac        ! The part-load fraction of the compressor
  REAL(r64)           :: SpeedRatio          ! SpeedRatio = (CompressorSpeed - CompressorSpeedMin) /
                                             !              (CompressorSpeedMax - CompressorSpeedMin)
                                             ! for variable speed or 2 speed compressors
  REAL(r64)           :: CycRatio            ! Cycling part-load ratio for variable speed or 2 speed compressors
  REAL(r64)           :: DesOutTemp          ! Desired outlet temperature of the DX cooling coil
  REAL(r64)           :: DesOutHumRat        ! Desired outlet humidity ratio of the DX cooling coil
  REAL(r64)           :: OutletTempDXCoil    ! Actual outlet temperature of the DX cooling coil
  REAL(r64)           :: OutletTempLS        ! Actual outlet temperature of the variable speed DX cooling coil at low speed
  REAL(r64)           :: OutletTempHS        ! Actual outlet temperature of the variable speed DX cooling coil at high speed
  REAL(r64)           :: OutletHumRatLS      ! Actual outlet humrat of the variable speed DX cooling coil at low speed
  REAL(r64)           :: OutletHumRatHS      ! Actual outlet humrat of the variable speed DX cooling coil at high speed
  REAL(r64)           :: OutletHumRatDXCoil  ! Actual outlet humidity ratio of the DX cooling coil
  INTEGER             :: SolFla              ! Flag of solver
  REAL(r64), DIMENSION(5)  :: Par                 ! Parameter array passed to solver
  LOGICAL             :: SensibleLoad        ! True if there is a sensible cooling load on this system
  LOGICAL             :: LatentLoad          ! True if there is a latent   cooling load on this system
  INTEGER             :: DehumidMode         ! Dehumidification mode (0=normal, 1=enhanced)
  INTEGER             :: FanOpMode           ! Supply air fan operating mode
  REAL(r64)           :: TempMinPLR          ! Used to find latent PLR when max iterations exceeded
  REAL(r64)           :: TempMaxPLR          ! Used to find latent PLR when max iterations exceeded
  REAL(r64)           :: TempOutletTempDXCoil   ! Used to find latent PLR when max iterations exceeded
  REAL(r64)           :: TempOutletHumRatDXCoil ! Used to find latent PLR when max iterations exceeded
  REAL(r64)           :: NoLoadHumRatOut     ! DX coil outlet air humidity ratio with comprssor off
  REAL(r64)           :: FullLoadHumRatOut   ! DX coil outlet air humidity ratio with comprssor full on

      ! Set local variables
      ! Retrieve the load on the controlled zone
  OutletNode   = DXCoolingSystem(DXSystemNum)%DXCoolingCoilOutletNodeNum
  InletNode    = DXCoolingSystem(DXSystemNum)%DXCoolingCoilInletNodeNum
  ControlNode  = DXCoolingSystem(DXSystemNum)%DXSystemControlNodeNum
  DesOutTemp   = DXCoolingSystem(DXSystemNum)%DesiredOutletTemp
  DesOutHumRat = DXCoolingSystem(DXSystemNum)%DesiredOutletHumRat
  CompName     = DXCoolingSystem(DXSystemNum)%CoolingCoilName
  FanOpMode    = DXCoolingSystem(DXSystemNum)%FanOpMode
  SpeedRatio   = 0.0
  CycRatio     = 0.0
  PartLoadFrac = 0.0
  DehumidMode  = 0
  SensibleLoad = .FALSE.
  LatentLoad   = .FALSE.

  ! If DXCoolingSystem is scheduled on and there is flow
  If((GetCurrentScheduleValue(DXCoolingSystem(DXSystemNum)%SchedPtr) .gt. 0.0) .and. &
     (Node(InletNode)%MassFlowRate .gt. MinAirMassFlow)) THEN

    ! Determine if there is a sensible load on this system
    IF((Node(InletNode)%Temp > Node(ControlNode)%TempSetPoint) .and. &
       (Node(InletNode)%Temp > DesOutTemp) .and. &
       (ABS(Node(InletNode)%Temp - DesOutTemp) .gt. TempControlTol) ) SensibleLoad = .true.

    ! Determine if there is a latent load on this system - for future use to serve latent-only loads
    IF((Node(InletNode)%HumRat > Node(ControlNode)%HumRatMax) .and. &
       (Node(InletNode)%HumRat > DesOutHumRat)) LatentLoad = .true.

    ! If DXCoolingSystem runs with a cooling load then set PartLoadFrac on Cooling System and the Mass Flow
    ! Multimode coil will switch to enhanced dehumidification if available and needed, but it
    ! still runs to meet the sensible load. Multimode applies to Multimode or HXAssistedCooling coils.
    IF ((SensibleLoad .and. DXCoolingSystem(DXSystemNum)%RunOnSensibleLoad) .OR. &
        (LatentLoad .and. DXCoolingSystem(DXSystemNum)%RunOnLatentLoad)) THEN
      SELECT CASE(DXCoolingSystem(DXSystemNum)%CoolingCoilType_Num)

        CASE (CoilDX_CoolingSingleSpeed)  ! COIL:DX:COOLINGBYPASSFACTOREMPIRICAL

          ! Get no load result
          PartLoadFrac = 0.0
          CALL SimDXCoil(CompName,On,FirstHVACIteration,PartLoadFrac,DXCoolingSystem(DXSystemNum)%CoolingCoilIndex,FanOpMode)
          NoOutput = Node(InletNode)%MassFlowRate *  &
                       (PsyHFnTdbW(Node(OutletNode)%Temp,Node(OutletNode)%HumRat)  &
                        - PsyHFnTdbW(Node(InletNode)%Temp,Node(OutletNode)%HumRat))
          NoLoadHumRatOut = DXCoilOutletHumRat(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)

          ! Get full load result
          PartLoadFrac = 1.0
          CALL SimDXCoil(CompName,On,FirstHVACIteration,PartLoadFrac,DXCoolingSystem(DXSystemNum)%CoolingCoilIndex,FanOpMode)
          FullLoadHumRatOut = DXCoilOutletHumRat(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)

          FullOutput = Node(InletNode)%MassFlowRate *  &
                       (PsyHFnTdbW(Node(OutletNode)%Temp,Node(OutletNode)%HumRat)  &
                        - PsyHFnTdbW(Node(InletNode)%Temp,Node(OutletNode)%HumRat))

          ReqOutput = Node(InletNode)%MassFlowRate *  &
                       (PsyHFnTdbW(DXCoolingSystem(DXSystemNum)%DesiredOutletTemp,Node(OutletNode)%HumRat) - &
                        PsyHFnTdbW(Node(InletNode)%Temp,Node(OutletNode)%HumRat))

!         IF NoOutput is lower than (more cooling than required) or very near the ReqOutput, do not run the compressor
          IF ((NoOutput-ReqOutput) .LT. Acc) THEN
            PartLoadFrac = 0.0
!         If the FullOutput is greater than (insufficient cooling) or very near the ReqOutput,
!         run the compressor at PartLoadFrac = 1.
          ELSE IF ((FullOutput - ReqOutput) .GT. Acc) THEN
            PartLoadFrac = 1.0
!         Else find the PLR to meet the load
          ELSE
!           OutletTempDXCoil is the full capacity outlet temperature at PartLoadFrac = 1 from the CALL above. If this temp is
!           greater than the desired outlet temp, then run the compressor at PartLoadFrac = 1, otherwise find the operating PLR.
            OutletTempDXCoil = DXCoilOutletTemp(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
            IF (OutletTempDXCoil > DesOutTemp) THEN
              PartLoadFrac = 1.0
            ELSE
              Par(1) = REAL(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex,r64)
              Par(2) = DesOutTemp
              Par(5) = REAL(FanOpMode,r64)
              CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, DOE2DXCoilResidual, 0.0d0,   &
                                            1.0d0, Par)
              IF (SolFla == -1) THEN
                IF(.NOT. WarmupFlag)THEN
                  IF(DXCoolingSystem(DXSystemNum)%DXCoilSensPLRIter .LT. 1)THEN
                    DXCoolingSystem(DXSystemNum)%DXCoilSensPLRIter = DXCoolingSystem(DXSystemNum)%DXCoilSensPLRIter+1
                    CALL ShowWarningError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                          ' - Iteration limit exceeded calculating DX unit sensible '// &
                                          'part-load ratio for unit = '//TRIM(DXCoolingSystem(DXSystemNum)%Name))
                    CALL ShowContinueError('Estimated part-load ratio  = '//RoundSigDigits((ReqOutput/FullOutput),3))
                    CALL ShowContinueError('Calculated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                    CALL ShowContinueErrorTimeStamp('The calculated part-load ratio will be used and the simulation'// &
                                                    ' continues. Occurrence info: ')
                  END IF
                  CALL ShowRecurringWarningErrorAtEnd(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' "'&
                      //TRIM(DXCoolingSystem(DXSystemNum)%Name)//'" - Iteration limit exceeded calculating'// &
                      ' sensible part-load ratio error continues. Sensible PLR statistics follow.' &
                      ,DXCoolingSystem(DXSystemNum)%DXCoilSensPLRIterIndex,PartLoadFrac,PartLoadFrac)
                END IF
              ELSE IF (SolFla == -2) THEN
                PartLoadFrac = ReqOutput/FullOutput
                IF(.NOT. WarmupFlag)THEN
                  IF(DXCoolingSystem(DXSystemNum)%DXCoilSensPLRFail .LT. 1)THEN
                    DXCoolingSystem(DXSystemNum)%DXCoilSensPLRFail = DXCoolingSystem(DXSystemNum)%DXCoilSensPLRFail+1
                    CALL ShowWarningError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' - DX unit sensible part-'// &
                                    'load ratio calculation failed: part-load ratio limits exceeded, for unit = '// &
                                     TRIM(DXCoolingSystem(DXSystemNum)%Name))
                    CALL ShowContinueError('Estimated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                    CALL ShowContinueErrorTimeStamp('The estimated part-load ratio will be used and the simulation'// &
                                                    ' continues. Occurrence info: ')
                  END IF
                  CALL ShowRecurringWarningErrorAtEnd(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' "'&
                      //TRIM(DXCoolingSystem(DXSystemNum)%Name)//'" - DX unit sensible part-load ratio calculation'// &
                      ' failed error continues. Sensible PLR statistics follow.' &
                      ,DXCoolingSystem(DXSystemNum)%DXCoilSensPLRFailIndex,PartLoadFrac,PartLoadFrac)
                END IF

              END IF
            END IF
          END IF

!         If system does not operate to meet sensible load, use no load humidity ratio to test against humidity setpoint,
!         else use operating humidity ratio to test against humidity setpoint
          IF (PartLoadFrac .EQ. 0.0)THEN
            OutletHumRatDXCoil = NoLoadHumRatOut
          ELSE
            OutletHumRatDXCoil = DXCoilOutletHumRat(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
          END IF

          ! If humidity setpoint is not satisfied and humidity control type is CoolReheat,
          ! then overcool to meet moisture load

          IF (( OutletHumRatDXCoil > DesOutHumRat) .AND. (PartLoadFrac .LT. 1.0) .AND. &
              (DXCoolingSystem(DXSystemNum)%DehumidControlType .EQ. DehumidControl_CoolReheat)) THEN

!           IF NoLoadHumRatOut is lower than (more dehumidification than required) or very near the DesOutHumRat,
!           do not run the compressor
            IF ((NoLoadHumRatOut-DesOutHumRat) .LT. HumRatAcc) THEN
              PartLoadFrac = PartLoadFrac  ! keep part-load fraction from sensible calculation
!           If the FullLoadHumRatOut is greater than (insufficient dehumidification) or very near the DesOutHumRat,
!           run the compressor at PartLoadFrac = 1.
            ELSE IF ((DesOutHumRat-FullLoadHumRatOut) .LT. HumRatAcc) THEN
              PartLoadFrac = 1.0
!           Else find the PLR to meet the load
            ELSE
              Par(1) = REAL(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex,r64)
              Par(2) = DesOutHumRat
              Par(5) = REAL(FanOpMode,r64)
              CALL SolveRegulaFalsi(HumRatAcc, MaxIte, SolFla, PartLoadFrac, DOE2DXCoilHumRatResidual, 0.0d0,   &
                                            1.0d0, Par)
              IF (SolFla == -1) THEN
                IF(.NOT. WarmupFlag)THEN
                  IF(DXCoolingSystem(DXSystemNum)%DXCoilLatPLRIter .LT. 1)THEN
                    DXCoolingSystem(DXSystemNum)%DXCoilLatPLRIter = DXCoolingSystem(DXSystemNum)%DXCoilLatPLRIter+1
                    CALL ShowWarningError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                          ' - Iteration limit exceeded calculating DX unit latent part-load'// &
                                          ' ratio for unit = '//TRIM(DXCoolingSystem(DXSystemNum)%Name))
                    CALL ShowContinueError('Estimated part-load ratio   = '//RoundSigDigits((ReqOutput/FullOutput),3))
                    CALL ShowContinueError('Calculated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                    CALL ShowContinueErrorTimeStamp('The calculated part-load ratio will be used and the simulation'// &
                                                  ' continues. Occurrence info: ')
                  END IF
                  CALL ShowRecurringWarningErrorAtEnd(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' "'&
                      //TRIM(DXCoolingSystem(DXSystemNum)%Name)//'" - Iteration limit exceeded calculating'// &
                      ' latent part-load ratio error continues. Latent PLR statistics follow.' &
                      ,DXCoolingSystem(DXSystemNum)%DXCoilLatPLRIterIndex,PartLoadFrac,PartLoadFrac)
                END IF
              ELSE IF (SolFla == -2) THEN
!               RegulaFalsi returns PLR = minPLR when a solution cannot be found, recalculate PartLoadFrac.
                IF(NoLoadHumRatOut-FullLoadHumRatOut .NE. 0.0)THEN
                  PartLoadFrac = (NoLoadHumRatOut-DesOutHumRat)/(NoLoadHumRatOut-FullLoadHumRatOut)
                ELSE
                  PartLoadFrac = 1.0
                END IF
                IF(.NOT. WarmupFlag)THEN
                  IF(DXCoolingSystem(DXSystemNum)%DXCoilLatPLRFail .LT. 1)THEN
                    DXCoolingSystem(DXSystemNum)%DXCoilLatPLRFail = DXCoolingSystem(DXSystemNum)%DXCoilLatPLRFail+1
                    CALL ShowWarningError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' - DX unit latent part-'// &
                                    'load ratio calculation failed: part-load ratio limits exceeded, for unit = '//&
                                    TRIM(DXCoolingSystem(DXSystemNum)%Name))
                    CALL ShowContinueError('Estimated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                    CALL ShowContinueErrorTimeStamp('The estimated part-load ratio will be used and the simulation'// &
                                                    ' continues. Occurrence info: ')
                  END IF
                  CALL ShowRecurringWarningErrorAtEnd(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' "'&
                      //TRIM(DXCoolingSystem(DXSystemNum)%Name)//'" - DX unit latent part-load ratio calculation'// &
                      ' failed error continues. Latent PLR statistics follow.' &
                      ,DXCoolingSystem(DXSystemNum)%DXCoilLatPLRFailIndex,PartLoadFrac,PartLoadFrac)
                END IF
              END IF
            END IF
          END IF ! End if humidity ratio setpoint not met - CoolReheat humidity control

          IF(PartLoadFrac.GT.1.0) THEN
            PartLoadFrac = 1.0
          ELSEIF(PartLoadFrac < 0.0) THEN
            PartLoadFrac = 0.0
          END IF

        CASE (CoilDX_CoolingHXAssisted)  ! CoilSystem:Cooling:DX:HeatExchangerAssisted

!         Check the dehumidification control type. If it's multimode, turn off the HX to find the sensible PLR. Then check to
!         see if the humidity load is met without the use of the HX. Always run the HX for the other modes.
          IF (DXCoolingSystem(DXSystemNum)%DehumidControlType .NE. DehumidControl_Multimode)THEN
            HXUnitOn = .TRUE.
          ELSE
            HXUnitOn = .FALSE.
          END IF

          ! Get no load result
          PartLoadFrac = 0.0
          CALL SimHXAssistedCoolingCoil(CompName,FirstHVACIteration,On,PartLoadFrac, &
                                        DXCoolingSystem(DXSystemNum)%CoolingCoilIndex, FanOpMode, &
                                        HXUnitEnable=HXUnitOn,EconomizerFlag=EconomizerFlag)
          NoOutput = Node(InletNode)%MassFlowRate *  &
                       (PsyHFnTdbW(Node(OutletNode)%Temp,Node(OutletNode)%HumRat)  &
                        - PsyHFnTdbW(Node(InletNode)%Temp,Node(OutletNode)%HumRat))
          NoLoadHumRatOut = DXCoilOutletHumRat(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)

          ! Get full load result
          PartLoadFrac = 1.0
          CALL SimHXAssistedCoolingCoil(CompName,FirstHVACIteration,On,PartLoadFrac, &
                                        DXCoolingSystem(DXSystemNum)%CoolingCoilIndex, FanOpMode, &
                                        HXUnitEnable=HXUnitOn,EconomizerFlag=EconomizerFlag)
          FullOutput = Node(InletNode)%MassFlowRate *  &
                       (PsyHFnTdbW(Node(OutletNode)%Temp,Node(OutletNode)%HumRat)  &
                        - PsyHFnTdbW(Node(InletNode)%Temp,Node(OutletNode)%HumRat))
          FullLoadHumRatOut = DXCoilOutletHumRat(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)

          ReqOutput = Node(InletNode)%MassFlowRate *  &
                       (PsyHFnTdbW(DXCoolingSystem(DXSystemNum)%DesiredOutletTemp,Node(OutletNode)%HumRat) - &
                        PsyHFnTdbW(Node(InletNode)%Temp,Node(OutletNode)%HumRat))

!         IF NoOutput is lower than (more cooling than required) or very near the ReqOutput, do not run the compressor
          IF ((NoOutput-ReqOutput) .LT. Acc) THEN
            PartLoadFrac = 0.0
!         If the FullOutput is greater than or very near the ReqOutput, then run the compressor at PartLoadFrac = 1.
          ELSE IF ((FullOutput - ReqOutput) .GT. Acc) THEN
            PartLoadFrac = 1.0
!         Else find the PLR to meet the load
          ELSE
!           OutletTempDXCoil is the full capacity outlet temperature at PartLoadFrac = 1 from the CALL above.
!           If this temp is greater than or very near the desired outlet temp, then run the compressor at PartLoadFrac = 1.
!           (i.e. HX iterates to find solution, don't allow the tolerance in solution to trip up RegulaFalsi. So if
!           solution is very near request, run compressor at PLR = 1)
            OutletTempDXCoil = HXAssistedCoilOutletTemp(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
            IF ((OutletTempDXCoil > DesOutTemp) .OR. ABS(OutletTempDXCoil - DesOutTemp) .LE. (Acc*2.0)) THEN
               PartLoadFrac = 1.0
            ELSE
              Par(1) = REAL(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex,r64)
              Par(2) = DesOutTemp
              ! FirstHVACIteration is a logical, Par is REAL(r64), so make TRUE = 1 and FALSE = 0
              IF(FirstHVACIteration)THEN
                Par(3) = 1.0
              ELSE
                Par(3) = 0.0
              END IF
              IF(HXUnitOn)THEN
                Par(4) = 1.0
              ELSE
                Par(4) = 0.0
              END IF
              Par(5) = REAL(FanOpMode,r64)
              CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, HXAssistedCoolCoilTempResidual, 0.0d0,   &
                                            1.0d0, Par)
              IF (SolFla == -1) THEN

!               RegulaFalsi may not find sensible PLR when the latent degradation model is used.
!               If iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
                TempMaxPLR = -0.1
                TempOutletTempDXCoil = Node(InletNode)%Temp
                DO WHILE((TempOutletTempDXCoil-DesOutTemp) .GT. 0.0 .AND. TempMaxPLR .LE. 1.0)
!                 find upper limit of PLR
                  TempMaxPLR = TempMaxPLR + 0.1d0
                  CALL SimHXAssistedCoolingCoil(CompName,FirstHVACIteration,On,TempMaxPLR, &
                                                DXCoolingSystem(DXSystemNum)%CoolingCoilIndex, FanOpMOde, &
                                                HXUnitEnable=HXUnitOn, EconomizerFlag=EconomizerFlag)
                  TempOutletTempDXCoil = HXAssistedCoilOutletTemp(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
                END DO
                TempMinPLR = TempMaxPLR
                DO WHILE((TempOutletTempDXCoil-DesOutTemp) .LT. 0.0 .AND. TempMinPLR .GE. 0.0)
!                 pull upper limit of PLR down to last valid limit (i.e. outlet temp still exceeds DesOutTemp)
                  TempMaxPLR = TempMinPLR
!                 find minimum limit of PLR
                  TempMinPLR = TempMinPLR - 0.01d0
                  CALL SimHXAssistedCoolingCoil(CompName,FirstHVACIteration,On,TempMinPLR, &
                                                DXCoolingSystem(DXSystemNum)%CoolingCoilIndex, FanOpMode, &
                                                HXUnitEnable=HXUnitOn, EconomizerFlag=EconomizerFlag)
                  TempOutletTempDXCoil = HXAssistedCoilOutletTemp(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
                END DO
!               Relax boundary slightly to assure a solution can be found using RegulaFalsi (i.e. one boundary may be
!               very near the desired result)
                TempMinPLR = MAX(0.0d0,(TempMinPLR - 0.01d0))
                TempMaxPLR = MIN(1.0d0,(TempMaxPLR + 0.01d0))
!               tighter boundary of solution has been found, call RegulaFalsi a second time
                CALL SolveRegulaFalsi(Acc,MaxIte,SolFla,PartLoadFrac,HXAssistedCoolCoilTempResidual,TempMinPLR,TempMaxPLR,Par)
                IF (SolFla == -1) THEN
                  IF(.NOT. WarmupFlag)THEN
                    IF(DXCoolingSystem(DXSystemNum)%HXAssistedSensPLRIter .LT. 1)THEN
                      DXCoolingSystem(DXSystemNum)%HXAssistedSensPLRIter = DXCoolingSystem(DXSystemNum)%HXAssistedSensPLRIter+1
                      CALL ShowWarningError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' - Iteration limit'// &
                                            ' exceeded calculating DX unit sensible part-load ratio for unit = '// &
                                            TRIM(DXCoolingSystem(DXSystemNum)%Name))
                      CALL ShowContinueError('Estimated part-load ratio   = '//RoundSigDigits((ReqOutput/FullOutput),3))
                      CALL ShowContinueError('Calculated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                      CALL ShowContinueErrorTimeStamp('The calculated part-load ratio will be used and the simulation'// &
                                                      ' continues. Occurrence info: ')
                    END IF
                    CALL ShowRecurringWarningErrorAtEnd(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' "'&
                        //TRIM(DXCoolingSystem(DXSystemNum)%Name)//'" - Iteration limit exceeded calculating'// &
                        ' sensible part-load ratio error continues. Sensible PLR statistics follow.' &
                        ,DXCoolingSystem(DXSystemNum)%HXAssistedSensPLRIterIndex,PartLoadFrac,PartLoadFrac)
                  END IF
                ELSE IF (SolFla == -2) THEN
                  PartLoadFrac = ReqOutput/FullOutput
                  IF(.NOT. WarmupFlag)THEN
                    IF(DXCoolingSystem(DXSystemNum)%HXAssistedSensPLRFail .LT. 1)THEN
                      DXCoolingSystem(DXSystemNum)%HXAssistedSensPLRFail = DXCoolingSystem(DXSystemNum)%HXAssistedSensPLRFail+1
                      CALL ShowWarningError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                            ' - DX unit sensible part-load ratio calculation unexpectedly failed: part-load '// &
                                            'ratio limits exceeded, for unit = '//TRIM(DXCoolingSystem(DXSystemNum)%Name))
                      CALL ShowContinueError('Estimated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                      CALL ShowContinueErrorTimeStamp('The estimated part-load ratio will be used and the simulation'// &
                                                      ' continues. Occurrence info: ')
                    END IF
                    CALL ShowRecurringWarningErrorAtEnd(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' "'&
                        //TRIM(DXCoolingSystem(DXSystemNum)%Name)//'" - DX unit sensible part-load ratio calculation'// &
                        ' unexpectedly failed error continues. Sensible PLR statistics follow.' &
                        ,DXCoolingSystem(DXSystemNum)%HXAssistedSensPLRFailIndex,PartLoadFrac,PartLoadFrac)
                  END IF
                END IF

              ELSE IF (SolFla == -2) THEN
                PartLoadFrac = ReqOutput/FullOutput
                IF(.NOT. WarmupFlag)THEN
                  IF(DXCoolingSystem(DXSystemNum)%HXAssistedSensPLRFail2 .LT. 1)THEN
                    DXCoolingSystem(DXSystemNum)%HXAssistedSensPLRFail2 = DXCoolingSystem(DXSystemNum)%HXAssistedSensPLRFail2+1
                    CALL ShowWarningError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                          ' - DX unit sensible part-load ratio calculation failed: part-load '// &
                                          'ratio limits exceeded, for unit = '//TRIM(DXCoolingSystem(DXSystemNum)%Name))
                    CALL ShowContinueError('Estimated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                    CALL ShowContinueErrorTimeStamp('The estimated part-load ratio will be used and the simulation'// &
                                                    ' continues. Occurrence info: ')
                  END IF
                  CALL ShowRecurringWarningErrorAtEnd(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' "'&
                      //TRIM(DXCoolingSystem(DXSystemNum)%Name)//'" - DX unit sensible part-load ratio calculation'// &
                      ' failed error continues. Sensible PLR statistics follow.' &
                      ,DXCoolingSystem(DXSystemNum)%HXAssistedSensPLRFailIndex2,PartLoadFrac,PartLoadFrac)
                END IF
              END IF
            END IF
          END IF

!         If system does not operate to meet sensible load, use no load humidity ratio to test against humidity setpoint,
!         else use operating humidity ratio to test against humidity setpoint
          IF (PartLoadFrac .EQ. 0.0)THEN
            OutletHumRatDXCoil = NoLoadHumRatOut
          ELSE
            OutletHumRatDXCoil = HXAssistedCoilOutletHumRat(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
          END IF

          ! If humidity setpoint is not satisfied and humidity control type is MultiMode,
          ! then enable heat exchanger and run to meet sensible load

          IF (( OutletHumRatDXCoil > DesOutHumRat) .AND. &
              (DXCoolingSystem(DXSystemNum)%DehumidControlType .EQ. DehumidControl_Multimode)) THEN

            ! Determine required part load when heat exchanger is ON
            HXUnitOn = .TRUE.
            PartLoadFrac = 1.0
            CALL SimHXAssistedCoolingCoil(CompName,FirstHVACIteration,On,PartLoadFrac, &
                                          DXCoolingSystem(DXSystemNum)%CoolingCoilIndex, FanOpMode, &
                                          HXUnitEnable=HXUnitOn, EconomizerFlag=EconomizerFlag)

            OutletTempDXCoil = HXAssistedCoilOutletTemp(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)

!           FullOutput will be different than the FullOutput determined above during sensible PLR calculations
            FullOutput = Node(InletNode)%MassFlowRate *  &
                       (PsyHFnTdbW(Node(OutletNode)%Temp,Node(OutletNode)%HumRat)  &
                        - PsyHFnTdbW(Node(InletNode)%Temp,Node(OutletNode)%HumRat))

            ReqOutput = Node(InletNode)%MassFlowRate *  &
                       (PsyHFnTdbW(DXCoolingSystem(DXSystemNum)%DesiredOutletTemp,Node(OutletNode)%HumRat) - &
                        PsyHFnTdbW(Node(InletNode)%Temp,Node(OutletNode)%HumRat))

!           Check to see if the system can meet the load with the compressor off
!           IF NoOutput is lower than (more cooling than required) or very near the ReqOutput, do not run the compressor
            IF ((NoOutput-ReqOutput) .LT. Acc) THEN
              PartLoadFrac = 0.0
!           OutletTempDXCoil is the full capacity outlet temperature at PartLoadFrac = 1 from the CALL above.
!           If this temp is greater than or very near the desired outlet temp, then run the compressor at PartLoadFrac = 1.
            ELSE IF ((OutletTempDXCoil > DesOutTemp) .OR. ABS(OutletTempDXCoil - DesOutTemp) .LE. (Acc*2.0d0)) THEN
               PartLoadFrac = 1.0
            ELSE
              Par(1) = REAL(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex,r64)
              Par(2) = DesOutTemp
              ! FirstHVACIteration is a logical, Par is REAL(r64), so make TRUE = 1.0 and FALSE = 0.0
              IF(FirstHVACIteration)THEN
                Par(3) = 1.0
              ELSE
                Par(3) = 0.0
              END IF
              IF(HXUnitOn)THEN
                Par(4) = 1.0
              ELSE
                Par(4) = 0.0
              END IF
              Par(5) = REAL(FanOpMode,r64)
              CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, HXAssistedCoolCoilTempResidual, 0.0d0,   &
                                            1.0d0, Par)
              IF (SolFla == -1) THEN
                IF(.NOT. WarmupFlag)THEN
                  IF(DXCoolingSystem(DXSystemNum)%HXAssistedLatPLRIter .LT. 1)THEN
                    DXCoolingSystem(DXSystemNum)%HXAssistedLatPLRIter = DXCoolingSystem(DXSystemNum)%HXAssistedLatPLRIter+1
                    CALL ShowWarningError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                          ' - Iteration limit exceeded calculating DX unit latent'// &
                                          ' part-load ratio for unit = '//TRIM(DXCoolingSystem(DXSystemNum)%Name))
                    CALL ShowContinueError('Estimated latent part-load ratio   = '//RoundSigDigits((ReqOutput/FullOutput),3))
                    CALL ShowContinueError('Calculated latent part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                    CALL ShowContinueErrorTimeStamp('The calculated latent part-load ratio will be used and the simulation'// &
                                                    ' continues. Occurrence info: ')
                  END IF
                  CALL ShowRecurringWarningErrorAtEnd(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' "'&
                      //TRIM(DXCoolingSystem(DXSystemNum)%Name)//'" - Iteration limit exceeded calculating'// &
                      ' latent part-load ratio error continues. Latent PLR statistics follow.' &
                      ,DXCoolingSystem(DXSystemNum)%HXAssistedLatPLRIterIndex,PartLoadFrac,PartLoadFrac)
                END IF
              ELSE IF (SolFla == -2) THEN
                PartLoadFrac = ReqOutput/FullOutput
                IF(.NOT. WarmupFlag)THEN
                  IF(DXCoolingSystem(DXSystemNum)%HXAssistedLatPLRFail .LT. 1)THEN
                    DXCoolingSystem(DXSystemNum)%HXAssistedLatPLRFail = DXCoolingSystem(DXSystemNum)%HXAssistedLatPLRFail+1
                    CALL ShowWarningError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                          ' - DX unit latent part-load ratio calculation failed: part-load '// &
                                          'ratio limits exceeded, for unit = '//TRIM(DXCoolingSystem(DXSystemNum)%Name))
                    CALL ShowContinueError('Estimated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                    CALL ShowContinueErrorTimeStamp('The estimated part-load ratio will be used and the simulation'// &
                                                    ' continues. Occurrence info: ')
                  END IF
                  CALL ShowRecurringWarningErrorAtEnd(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' "'&
                      //TRIM(DXCoolingSystem(DXSystemNum)%Name)//'" - DX unit latent part-load ratio calculation'// &
                      ' failed error continues. Latent PLR statistics follow.' &
                      ,DXCoolingSystem(DXSystemNum)%HXAssistedLatPLRFailIndex,PartLoadFrac,PartLoadFrac)
                END IF
              END IF
            END IF
          END IF ! End if humidity ratio setpoint not met - Multimode humidity control

          ! If humidity setpoint is not satisfied and humidity control type is CoolReheat, then calculate
          ! a new latent PLR

          IF (OutletHumRatDXCoil .GT. DesOutHumRat .AND. PartLoadFrac .LT. 1.0 .AND. &
              DXCoolingSystem(DXSystemNum)%DehumidControlType .EQ. DehumidControl_CoolReheat) THEN

!           IF NoLoadHumRatOut is lower than (more dehumidification than required) or very near the DesOutHumRat,
!           do not run the compressor
            IF ((NoLoadHumRatOut-DesOutHumRat) .LT. HumRatAcc*2.0d0) THEN
              PartLoadFrac = PartLoadFrac  ! keep part-load fraction from sensible calculation
!           If the FullLoadHumRatOut is greater than (insufficient dehumidification) or very near the DesOutHumRat,
!           run the compressor at PartLoadFrac = 1.
            ELSE IF ((DesOutHumRat-FullLoadHumRatOut) .LT. HumRatAcc*2.0d0) THEN
              PartLoadFrac = 1.0
!           Else find the PLR to meet the load
            ELSE
              Par(1) = REAL(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex,r64)
              Par(2) = DesOutHumRat
              ! FirstHVACIteration is a logical, Par is REAL(r64), so make TRUE = 1 and FALSE = 0
              IF(FirstHVACIteration)THEN
                Par(3) = 1.0
              ELSE
                Par(3) = 0.0
              END IF
              IF(HXUnitOn)THEN
                Par(4) = 1.0
              ELSE
                Par(4) = 0.0
              END IF
              Par(5) = REAL(FanOpMode,r64)
              CALL SolveRegulaFalsi(HumRatAcc, MaxIte, SolFla, PartLoadFrac, HXAssistedCoolCoilHRResidual, 0.0d0,   &
                                            1.0d0, Par)
              IF (SolFla == -1) THEN

!               RegulaFalsi may not find latent PLR when the latent degradation model is used.
!               If iteration limit is exceeded, find tighter boundary of solution and repeat RegulaFalsi
                TempMaxPLR = -0.1d0
                TempOutletHumRatDXCoil = OutletHumRatDXCoil
                DO WHILE((OutletHumRatDXCoil - TempOutletHumRatDXCoil) .GE. 0.0 .AND. TempMaxPLR .LE. 1.0)
!                 find upper limit of LatentPLR
                  TempMaxPLR = TempMaxPLR + 0.1d0
                  CALL SimHXAssistedCoolingCoil(CompName,FirstHVACIteration,On,TempMaxPLR, &
                                                DXCoolingSystem(DXSystemNum)%CoolingCoilIndex, FanOpMode, &
                                                HXUnitEnable=HXUnitOn, EconomizerFlag=EconomizerFlag)
                  OutletHumRatDXCoil = HXAssistedCoilOutletHumRat(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
                END DO
                TempMinPLR = TempMaxPLR
                DO WHILE((OutletHumRatDXCoil - TempOutletHumRatDXCoil) .LE. 0.0 .AND. TempMinPLR .GE. 0.0)
!                 pull upper limit of LatentPLR down to last valid limit (i.e. latent output still exceeds SystemMoisuterLoad)
                  TempMaxPLR = TempMinPLR
!                 find minimum limit of Latent PLR
                  TempMinPLR = TempMinPLR - 0.01d0
                  CALL SimHXAssistedCoolingCoil(CompName,FirstHVACIteration,On,TempMaxPLR, &
                                                DXCoolingSystem(DXSystemNum)%CoolingCoilIndex, FanOpMode, &
                                                HXUnitEnable=HXUnitOn, EconomizerFlag=EconomizerFlag)
                  OutletHumRatDXCoil = HXAssistedCoilOutletHumRat(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
                END DO
!               tighter boundary of solution has been found, call RegulaFalsi a second time
                CALL SolveRegulaFalsi(HumRatAcc,MaxIte,SolFla,PartLoadFrac,HXAssistedCoolCoilHRResidual, &
                                      TempMinPLR,TempMaxPLR,Par)
                IF (SolFla == -1) THEN
                  IF(.NOT. WarmupFlag)THEN
                    IF(DXCoolingSystem(DXSystemNum)%HXAssistedCRLatPLRIter .LT. 1)THEN
                      DXCoolingSystem(DXSystemNum)%HXAssistedCRLatPLRIter = DXCoolingSystem(DXSystemNum)%HXAssistedCRLatPLRIter+1
                      CALL ShowWarningError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                            ' - Iteration limit exceeded calculating DX unit latent'// &
                                            ' part-load ratio for unit = '//TRIM(DXCoolingSystem(DXSystemNum)%Name))
                      CALL ShowContinueError('Estimated latent part-load ratio   = '//RoundSigDigits((ReqOutput/FullOutput),3))
                      CALL ShowContinueError('Calculated latent part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                      CALL ShowContinueErrorTimeStamp('The calculated latent part-load ratio will be used and the simulation'// &
                                                      ' continues. Occurrence info: ')
                    END IF
                    CALL ShowRecurringWarningErrorAtEnd(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' "'&
                        //TRIM(DXCoolingSystem(DXSystemNum)%Name)//'" - Iteration limit exceeded calculating'// &
                        ' latent part-load ratio error continues. Latent PLR statistics follow.' &
                        ,DXCoolingSystem(DXSystemNum)%HXAssistedCRLatPLRIterIndex,PartLoadFrac,PartLoadFrac)
                  END IF

                ELSE IF (SolFla == -2) THEN
                  PartLoadFrac = ReqOutput/FullOutput
                  IF(.NOT. WarmupFlag)THEN
                    IF(DXCoolingSystem(DXSystemNum)%HXAssistedCRLatPLRFail .LT. 1)THEN
                      DXCoolingSystem(DXSystemNum)%HXAssistedCRLatPLRFail = DXCoolingSystem(DXSystemNum)%HXAssistedCRLatPLRFail+1
                      CALL ShowWarningError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                         ' - DX unit latent part-load ratio calculation failed unexpectedly:'// &
                                         ' part-load ratio limits exceeded, for unit = '//TRIM(DXCoolingSystem(DXSystemNum)%Name))
                      CALL ShowContinueError('Estimated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                      CALL ShowContinueErrorTimeStamp('The estimated part-load ratio will be used and the simulation'// &
                                                      ' continues. Occurrence info: ')
                    END IF
                    CALL ShowRecurringWarningErrorAtEnd(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' "'&
                        //TRIM(DXCoolingSystem(DXSystemNum)%Name)//'" - DX unit latent part-load ratio calculation'// &
                        ' failed unexpectedly error continues. Latent PLR statistics follow.' &
                        ,DXCoolingSystem(DXSystemNum)%HXAssistedCRLatPLRFailIndex,PartLoadFrac,PartLoadFrac)
                  END IF
                END IF
              ELSE IF (SolFla == -2) THEN
                PartLoadFrac = ReqOutput/FullOutput
                IF(.NOT. WarmupFlag)THEN
                  IF(DXCoolingSystem(DXSystemNum)%HXAssistedCRLatPLRFail2 .LT. 1)THEN
                    DXCoolingSystem(DXSystemNum)%HXAssistedCRLatPLRFail2 = DXCoolingSystem(DXSystemNum)%HXAssistedCRLatPLRFail2+1
                    CALL ShowWarningError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                          ' - DX unit latent part-load ratio calculation failed: part-load '// &
                                          'ratio limits exceeded, for unit = '//TRIM(DXCoolingSystem(DXSystemNum)%Name))
                    CALL ShowContinueError('Estimated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                    CALL ShowContinueErrorTimeStamp('The estimated part-load ratio will be used and the simulation'// &
                                                    ' continues. Occurrence info: ')
                  END IF
                  CALL ShowRecurringWarningErrorAtEnd(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' "'&
                      //TRIM(DXCoolingSystem(DXSystemNum)%Name)//'" - DX unit latent part-load ratio calculation'// &
                      ' failed error continues. Latent PLR statistics follow.' &
                      ,DXCoolingSystem(DXSystemNum)%HXAssistedCRLatPLRFailIndex2,PartLoadFrac,PartLoadFrac)
                END IF
              END IF
            END IF
          END IF ! End if humidity ratio setpoint not met - CoolReheat humidity control

          IF(PartLoadFrac.GT.1.0) THEN
            PartLoadFrac = 1.0
          ELSEIF(PartLoadFrac < 0.0) THEN
            PartLoadFrac = 0.0
          END IF

        CASE (CoilDX_CoolingTwoSpeed)  ! Coil:Cooling:DX:TwoSpeed 
                                       ! formerly (v3 and beyond)COIL:DX:MULTISPEED:COOLINGEMPIRICAL
!         SUBROUTINE SimDXCoilMultiSpeed(CompName,SpeedRatio,CycRatio,CompIndex,SpeedNum,FanMode,CompOp)
          CALL SimDXCoilMultiSpeed(CompName,0.0d0,1.0d0,DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
          OutletTempLS = DXCoilOutletTemp(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
          IF (OutletTempLS > DesOutTemp) THEN
            CycRatio = 1.0
            CALL SimDXCoilMultiSpeed(CompName,1.0d0,1.0d0,DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
            OutletTempHS = DXCoilOutletTemp(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
            IF (OutletTempHS < DesOutTemp) THEN
              Par(1) = REAL(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex,r64)
              Par(2) = DesOutTemp
              CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, SpeedRatio, DXCoilVarSpeedResidual, 0.0d0,   &
                                            1.0d0, Par)
              IF (SolFla == -1) THEN
                IF(.NOT. WarmupFlag)THEN
                  IF(DXCoolingSystem(DXSystemNum)%MSpdSensPLRIter .LT. 1)THEN
                    DXCoolingSystem(DXSystemNum)%MSpdSensPLRIter = DXCoolingSystem(DXSystemNum)%MSpdSensPLRIter+1
                    CALL ShowWarningError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                          ' - Iteration limit exceeded calculating DX unit sensible'// &
                                          ' speed ratio for unit = '//TRIM(DXCoolingSystem(DXSystemNum)%Name))
                    CALL ShowContinueError('Calculated speed ratio = '//RoundSigDigits(SpeedRatio,3))
                    CALL ShowContinueErrorTimeStamp('The calculated speed ratio will be used and the simulation'// &
                                                    ' continues. Occurrence info: ')
                  END IF
                  CALL ShowRecurringWarningErrorAtEnd(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' "'&
                      //TRIM(DXCoolingSystem(DXSystemNum)%Name)//'" - Iteration limit exceeded calculating'// &
                      ' sensible speed ratio error continues. Sensible speed ratio statistics follow.' &
                      ,DXCoolingSystem(DXSystemNum)%MSpdSensPLRIterIndex,SpeedRatio,SpeedRatio)
                END IF
              ELSE IF (SolFla == -2) THEN
                IF(.NOT. WarmupFlag) &
                  CALL ShowFatalError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                      ' - compressor speed calculation failed: speed limits exceeded, for unit='// &
                                      TRIM(DXCoolingSystem(DXSystemNum)%Name))
              END IF
            ELSE
              SpeedRatio = 1.0
            END IF
          ELSE
            SpeedRatio = 0.0
            Par(1) = REAL(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex,r64)
            Par(2) = DesOutTemp
            CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, CycRatio, DXCoilCyclingResidual, 0.0d0,   &
                                            1.0d0, Par)
            IF (SolFla == -1) THEN
              IF(.NOT. WarmupFlag)THEN
                IF(DXCoolingSystem(DXSystemNum)%MSpdCycSensPLRIter .LT. 1)THEN
                  DXCoolingSystem(DXSystemNum)%MSpdCycSensPLRIter = DXCoolingSystem(DXSystemNum)%MSpdCycSensPLRIter+1
                  CALL ShowWarningError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                        ' - Iteration limit exceeded calculating DX unit sensible'// &
                                        ' cycling ratio for unit = '//TRIM(DXCoolingSystem(DXSystemNum)%Name))
                  CALL ShowContinueError('Calculated cycling ratio = '//RoundSigDigits(CycRatio,3))
                  CALL ShowContinueErrorTimeStamp('The calculated cycling ratio will be used and the simulation'// &
                                                  ' continues. Occurrence info: ')
                END IF
                CALL ShowRecurringWarningErrorAtEnd(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' "'&
                    //TRIM(DXCoolingSystem(DXSystemNum)%Name)//'" - Iteration limit exceeded calculating'// &
                    ' sensible cycling ratio error continues. Sensible cycling ratio statistics follow.' &
                    ,DXCoolingSystem(DXSystemNum)%MSpdCycSensPLRIterIndex,CycRatio,CycRatio)
              END IF
            ELSE IF (SolFla == -2) THEN
              IF(.NOT. WarmupFlag) &
                CALL ShowFatalError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                    ' - cycling ratio calculation failed: cycling limits exceeded, for unit='// &
                                    TRIM(DXCoolingSystem(DXSystemNum)%Name))
            END IF
          END IF

          IF (DXCoolingSystem(DXSystemNum)%DehumidControlType .EQ. DehumidControl_CoolReheat) THEN

!           Simulate MultiSpeed DX coil at sensible result
            CALL SimDXCoilMultiSpeed(CompName,SpeedRatio,CycRatio,DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)

            OutletHumRatDXCoil = DXCoilOutletHumRat(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
            ! If humidity setpoint is not satisfied and humidity control type is CoolReheat,
            ! then overcool to meet moisture load

            IF (OutletHumRatDXCoil > DesOutHumRat) THEN

              CycRatio = 0.0
              SpeedRatio = 0.0

!             SUBROUTINE SimDXCoilMultiSpeed(CompName,SpeedRatio,CycRatio,CompIndex,SpeedNum,FanMode,CompOp)
              CALL SimDXCoilMultiSpeed(CompName,0.0d0,1.0d0,DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
              OutletHumRatLS = DXCoilOutletHumRat(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
              IF (OutletHumRatLS > DesOutHumRat) THEN
                CycRatio = 1.0
                CALL SimDXCoilMultiSpeed(CompName,1.0d0,1.0d0,DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
                OutletHumRatHS = DXCoilOutletHumRat(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
                IF (OutletHumRatHS < DesOutHumRat) THEN
                  Par(1) = REAL(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex,r64)
                  Par(2) = DesOutHumRat
                  CALL SolveRegulaFalsi(HumRatAcc, MaxIte, SolFla, SpeedRatio, DXCoilVarSpeedHumRatResidual, 0.0d0,   &
                                            1.0d0, Par)
                  IF (SolFla == -1) THEN
                    IF(.NOT. WarmupFlag)THEN
                      IF(DXCoolingSystem(DXSystemNum)%MSpdLatPLRIter .LT. 1)THEN
                        DXCoolingSystem(DXSystemNum)%MSpdLatPLRIter = DXCoolingSystem(DXSystemNum)%MSpdLatPLRIter+1
                        CALL ShowWarningError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                              ' - Iteration limit exceeded calculating DX unit latent'// &
                                              ' speed ratio for unit = '//TRIM(DXCoolingSystem(DXSystemNum)%Name))
                        CALL ShowContinueError('Calculated speed ratio = '//RoundSigDigits(SpeedRatio,3))
                        CALL ShowContinueErrorTimeStamp('The calculated speed ratio will be used and the simulation'// &
                                                      ' continues. Occurrence info: ')
                      END IF
                      CALL ShowRecurringWarningErrorAtEnd(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' "'&
                          //TRIM(DXCoolingSystem(DXSystemNum)%Name)//'" - Iteration limit exceeded calculating'// &
                          ' latent speed ratio error continues. Latent speed ratio statistics follow.' &
                          ,DXCoolingSystem(DXSystemNum)%MSpdLatPLRIterIndex,SpeedRatio,SpeedRatio)
                    END IF
                  ELSE IF (SolFla == -2) THEN
                    IF(.NOT. WarmupFlag) &
                      CALL ShowFatalError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' - compressor speed'// &
                                          ' calculation failed:speed limits exceeded, for unit='// &
                                          TRIM(DXCoolingSystem(DXSystemNum)%Name))
                  END IF
                ELSE
                  SpeedRatio = 1.0
                END IF
              ELSE
                SpeedRatio = 0.0
                Par(1) = REAL(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex,r64)
                Par(2) = DesOutHumRat
                CALL SolveRegulaFalsi(HumRatAcc, MaxIte, SolFla, CycRatio, DXCoilCyclingHumRatResidual, 0.0d0,   &
                                            1.0d0, Par)
                IF (SolFla == -1) THEN
                  IF(.NOT. WarmupFlag)THEN
                    IF(DXCoolingSystem(DXSystemNum)%MSpdCycLatPLRIter .LT. 1)THEN
                      DXCoolingSystem(DXSystemNum)%MSpdCycLatPLRIter = DXCoolingSystem(DXSystemNum)%MSpdCycLatPLRIter+1
                      CALL ShowWarningError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                            ' - Iteration limit exceeded calculating DX unit latent'// &
                                            ' cycling ratio for unit = '//TRIM(DXCoolingSystem(DXSystemNum)%Name))
                      CALL ShowContinueError('Calculated cycling ratio = '//RoundSigDigits(CycRatio,3))
                      CALL ShowContinueErrorTimeStamp('The calculated cycling ratio will be used and the simulation'// &
                                                    ' continues. Occurrence info: ')
                    END IF
                    CALL ShowRecurringWarningErrorAtEnd(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' "'&
                        //TRIM(DXCoolingSystem(DXSystemNum)%Name)//'" - Iteration limit exceeded calculating'// &
                        ' latent cycling ratio error continues. Latent cycling ratio statistics follow.' &
                        ,DXCoolingSystem(DXSystemNum)%MSpdCycLatPLRIterIndex,CycRatio,CycRatio)
                  END IF
                ELSE IF (SolFla == -2) THEN
                  IF(.NOT. WarmupFlag) &
                    CALL ShowFatalError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' - cycling ratio' // &
                                        ' calculation failed: cycling limits exceeded, for unit='// &
                                        TRIM(DXCoolingSystem(DXSystemNum)%Name))
                END IF
              END IF

            END IF

          END IF

        CASE (CoilDX_CoolingTwoStageWHumControl) ! Coil:Cooling:DX:TwoStageWithHumidityControlMode 
                                        ! formerly (v3 and beyond) COIL:DX:MULTIMODE:COOLINGEMPIRICAL)

          ! Get no load result
          PartLoadFrac = 0.0
          CALL SimDXCoilMultiMode(CompName,On,FirstHVACIteration,PartLoadFrac,DehumidMode,  &
                                  DXCoolingSystem(DXSystemNum)%CoolingCoilIndex,FanOpMode)
          NoOutput = Node(InletNode)%MassFlowRate *  &
                       (PsyHFnTdbW(Node(OutletNode)%Temp,Node(OutletNode)%HumRat)  &
                        - PsyHFnTdbW(Node(InletNode)%Temp,Node(OutletNode)%HumRat))
          NoLoadHumRatOut = DXCoilOutletHumRat(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)

          ! Get full load result
          PartLoadFrac = 1.0
          CALL SimDXCoilMultiMode(CompName,On,FirstHVACIteration,PartLoadFrac,DehumidMode,  &
                                  DXCoolingSystem(DXSystemNum)%CoolingCoilIndex,FanOpMode)
          FullLoadHumRatOut = DXCoilOutletHumRat(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)

          FullOutput = Node(InletNode)%MassFlowRate *  &
                       (PsyHFnTdbW(Node(OutletNode)%Temp,Node(OutletNode)%HumRat)  &
                        - PsyHFnTdbW(Node(InletNode)%Temp,Node(OutletNode)%HumRat))

          ReqOutput = Node(InletNode)%MassFlowRate *  &
                       (PsyHFnTdbW(DXCoolingSystem(DXSystemNum)%DesiredOutletTemp,Node(OutletNode)%HumRat) - &
                        PsyHFnTdbW(Node(InletNode)%Temp,Node(OutletNode)%HumRat))

!         IF NoOutput is lower than (more cooling than required) or very near the ReqOutput, do not run the compressor
          IF ((NoOutput-ReqOutput) .LT. Acc) THEN
            PartLoadFrac = 0.0
!         If the FullOutput is greater than (insufficient cooling) or very near the ReqOutput,
!         run the compressor at PartLoadFrac = 1.
          ELSE IF ((FullOutput - ReqOutput) .GT. Acc) THEN
            PartLoadFrac = 1.0
!         Else find the PLR to meet the load
          ELSE
            OutletTempDXCoil = DXCoilOutletTemp(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
            IF (OutletTempDXCoil > DesOutTemp) THEN
              PartLoadFrac = 1.0
            ELSE
              Par(1) = REAL(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex,r64)
              Par(2) = DesOutTemp
              ! Dehumidification mode = 0 for normal mode, 1+ for enhanced mode
              Par(3) = REAL(DehumidMode,r64)
              Par(4) = REAL(FanOpMode,r64)
              CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilResidual, 0.0d0,   &
                                            1.0d0, Par)
              IF (SolFla == -1) THEN
                IF(.NOT. WarmupFlag)THEN
                  IF(DXCoolingSystem(DXSystemNum)%MModeSensPLRIter .LT. 1)THEN
                    DXCoolingSystem(DXSystemNum)%MModeSensPLRIter = DXCoolingSystem(DXSystemNum)%MModeSensPLRIter+1
                    CALL ShowWarningError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                          ' - Iteration limit exceeded calculating DX unit sensible'// &
                                          ' part-load ratio for unit = '//TRIM(DXCoolingSystem(DXSystemNum)%Name))
                    CALL ShowContinueError('Estimated part-load ratio  = '//RoundSigDigits((ReqOutput/FullOutput),3))
                    CALL ShowContinueError('Calculated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                    CALL ShowContinueErrorTimeStamp('The calculated part-load ratio will be used and the simulation'// &
                                                    ' continues. Occurrence info: ')
                  END IF
                  CALL ShowRecurringWarningErrorAtEnd(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' "'&
                      //TRIM(DXCoolingSystem(DXSystemNum)%Name)//'" - Iteration limit exceeded calculating'// &
                      ' sensible part-load ratio error continues. Sensible PLR statistics follow.' &
                      ,DXCoolingSystem(DXSystemNum)%MModeSensPLRIterIndex,PartLoadFrac,PartLoadFrac)
                END IF
              ELSE IF (SolFla == -2) THEN
                IF(.NOT. WarmupFlag) THEN
                  CALL ShowSevereError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                       ' : part-load ratio calculation failed: '//  &
                                       'part-load ratio limits exceeded, for unit='// &
                                       TRIM(DXCoolingSystem(DXSystemNum)%Name))
                  CALL ShowFatalError('Program terminates due to previous condition.')
                END IF
              END IF
            END IF
          END IF

          OutletHumRatDXCoil = DXCoilOutletHumRat(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)

          ! If humidity setpoint is not satisfied and humidity control type is Multimode,
          ! then turn on enhanced dehumidification mode 1

          IF (( OutletHumRatDXCoil > DesOutHumRat) .AND. &
              (DXCoolingSystem(DXSystemNum)%DehumidControlType .EQ. DehumidControl_Multimode)) THEN

            ! Determine required part load for enhanced dehumidification mode 1

            ! Get full load result
            PartLoadFrac = 1.0
            DehumidMode  = 1
            DXCoolingSystem(DXSystemNum)%DehumidificationMode = DehumidMode
            CALL SimDXCoilMultiMode(CompName,On,FirstHVACIteration,PartLoadFrac,DehumidMode,  &
                                    DXCoolingSystem(DXSystemNum)%CoolingCoilIndex,FanOpMode)
            FullOutput = Node(InletNode)%MassFlowRate *  &
                         (PsyHFnTdbW(Node(OutletNode)%Temp,Node(InletNode)%HumRat)  &
                          - PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))

            ReqOutput = Node(InletNode)%MassFlowRate *  &
                         (PsyHFnTdbW(DXCoolingSystem(DXSystemNum)%DesiredOutletTemp,Node(InletNode)%HumRat) - &
                          PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))

            ! Since we are cooling, we expect FullOutput to be < 0 and FullOutput < NoCoolOutput
            ! Check that this is the case; if not set PartLoadFrac = 0.0 (off) and return
            ! Calculate the part load fraction
            IF (FullOutput .GE. 0) THEN
              PartLoadFrac = 0.0
            ELSE
              OutletTempDXCoil = DXCoilOutletTemp(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
              OutletHumRatDXCoil = DXCoilOutletHumRat(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
              ! if sensible load and setpoint cannot be met, set PLR = 1. If no sensible load and
              ! latent load exists and setpoint cannot be met, set PLR = 1.
              IF ((OutletTempDXCoil >= DesOutTemp .AND. SensibleLoad .and. DXCoolingSystem(DXSystemNum)%RunOnSensibleLoad) .OR. &
                  (OutletHumRatDXCoil >= DesOutHumRat .AND. &
                  .NOT. SensibleLoad .AND. LatentLoad .AND. DXCoolingSystem(DXSystemNum)%RunOnLatentLoad)) THEN
                PartLoadFrac = 1.0
              ! if no sensible load and latent load can be met, find PLR
              ELSE IF (.NOT. SensibleLoad .AND. &
                  (OutletHumRatDXCoil < DesOutHumRat .AND. LatentLoad .AND. DXCoolingSystem(DXSystemNum)%RunOnLatentLoad)) THEN
                ! is a latent load with no sensible load, iterate on humidity ratio
                Par(1) = REAL(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
                Par(2) = DesOutHumRat
                ! Dehumidification mode = 0 for normal mode, 1+ for enhanced mode
                Par(3) = REAL(DehumidMode,r64)
                Par(4) = REAL(FanOpMode,r64)
                CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilHumRatResidual, 0.0d0,   &
                                            1.0d0, Par)
                IF (SolFla == -1) THEN
                  IF(.NOT. WarmupFlag)THEN
                    IF(DXCoolingSystem(DXSystemNum)%MModeLatPLRIter .LT. 1)THEN
                      DXCoolingSystem(DXSystemNum)%MModeLatPLRIter = DXCoolingSystem(DXSystemNum)%MModeLatPLRIter+1
                      CALL ShowWarningError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                            ' - Iteration limit exceeded calculating DX unit multimode'// &
                                            ' latent (no sensible) part-load ratio for unit = '// &
                                            TRIM(DXCoolingSystem(DXSystemNum)%Name))
                      IF(NoLoadHumRatOut-OutletHumRatDXCoil > 0.d0)THEN
                        TempMinPLR = (DesOutHumRat-OutletHumRatDXCoil)/(NoLoadHumRatOut-OutletHumRatDXCoil)
                      ELSE
                        TempMinPLR = PartLoadFrac + 0.00d1
                      END IF
                      CALL ShowContinueError('Estimated part-load ratio  = '//RoundSigDigits(TempMinPLR,3))
                      CALL ShowContinueError('Calculated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                      CALL ShowContinueErrorTimeStamp('The calculated part-load ratio will be used and the simulation'// &
                                                      ' continues. Occurrence info: ')
                    END IF
                    CALL ShowRecurringWarningErrorAtEnd(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' "'&
                        //TRIM(DXCoolingSystem(DXSystemNum)%Name)//'" - Iteration limit exceeded calculating'// &
                        ' multimode latent (no sensible) part-load ratio error continues. Latent PLR statistics follow.' &
                        ,DXCoolingSystem(DXSystemNum)%MModeLatPLRIterIndex,PartLoadFrac,PartLoadFrac)
                  END IF
                ELSE IF (SolFla == -2) THEN
                  IF(.NOT. WarmupFlag) THEN
                    CALL ShowSevereError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                         ' : part-load ratio calculation failed: '//  &
                                         'part-load ratio limits exceeded, for unit='// &
                                         TRIM(DXCoolingSystem(DXSystemNum)%Name))
                    CALL ShowFatalError('Program terminates due to previous condition.')
                  END IF
                END IF

              ELSE ! must be a sensible load so find PLR
                PartLoadFrac = ReqOutput/FullOutput
                Par(1) = REAL(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
                Par(2) = DesOutTemp
                ! Dehumidification mode = 0 for normal mode, 1+ for enhanced mode
                Par(3) = REAL(DehumidMode,r64)
                Par(4) = REAL(FanOpMode,r64)
                CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilResidual, 0.0d0,   &
                                            1.0d0, Par)
                IF (SolFla == -1) THEN
                  IF(.NOT. WarmupFlag)THEN
                    IF(DXCoolingSystem(DXSystemNum)%MModeLatPLRIter .LT. 1)THEN
                      DXCoolingSystem(DXSystemNum)%MModeLatPLRIter = DXCoolingSystem(DXSystemNum)%MModeLatPLRIter+1
                      CALL ShowWarningError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                            ' - Iteration limit exceeded calculating DX unit multimode'// &
                                            ' latent part-load ratio for unit = '//TRIM(DXCoolingSystem(DXSystemNum)%Name))
                      CALL ShowContinueError('Estimated part-load ratio  = '//RoundSigDigits((ReqOutput/FullOutput),3))
                      CALL ShowContinueError('Calculated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                      CALL ShowContinueErrorTimeStamp('The calculated part-load ratio will be used and the simulation'// &
                                                      ' continues. Occurrence info: ')
                    END IF
                    CALL ShowRecurringWarningErrorAtEnd(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' "'&
                        //TRIM(DXCoolingSystem(DXSystemNum)%Name)//'" - Iteration limit exceeded calculating'// &
                        ' multimode latent part-load ratio error continues. Latent PLR statistics follow.' &
                        ,DXCoolingSystem(DXSystemNum)%MModeLatPLRIterIndex,PartLoadFrac,PartLoadFrac)
                  END IF
                ELSE IF (SolFla == -2) THEN
                  IF(.NOT. WarmupFlag) THEN
                    CALL ShowSevereError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                         ' : part-load ratio calculation failed: '//  &
                                         'part-load ratio limits exceeded, for unit='// &
                                         TRIM(DXCoolingSystem(DXSystemNum)%Name))
                    CALL ShowFatalError('Program terminates due to previous condition.')
                  END IF
                END IF
              END IF
            ENDIF         
          END IF ! End if humidity ratio setpoint not met - multimode humidity control


!         If humidity setpoint is not satisfied and humidity control type is CoolReheat, then run to meet latent load
!         If system does not operate to meet sensible load, use no load humidity ratio to test against humidity setpoint,
!         else use operating humidity ratio to test against humidity setpoint
          IF (PartLoadFrac .EQ. 0.0)THEN
              OutletHumRatDXCoil = NoLoadHumRatOut
          ELSE
              OutletHumRatDXCoil = DXCoilOutletHumRat(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex)
          END IF

          IF (( OutletHumRatDXCoil > DesOutHumRat) .AND. &
                (DXCoolingSystem(DXSystemNum)%DehumidControlType .EQ. DehumidControl_CoolReheat)) THEN

!            CoolReheat operates cooling stage 1 and/or 2 to meet DesOutHumRat. Dehumidification mode is not active.
             DehumidMode  = 0

!            IF NoLoadHumRatOut is lower than (more dehumidification than required) or very near the DesOutHumRat,
!            do not run the compressor
             IF ((NoLoadHumRatOut-DesOutHumRat) .LT. HumRatAcc) THEN
                PartLoadFrac = PartLoadFrac  ! keep part-load fraction from sensible calculation
!            If the FullLoadHumRatOut is greater than (insufficient dehumidification) or very near the DesOutHumRat,
!            run the compressor at PartLoadFrac = 1.
             ELSE IF ((DesOutHumRat-FullLoadHumRatOut) .LT. HumRatAcc) THEN
                PartLoadFrac = 1.0
!            Else find the PLR to meet the load
             ELSE
               Par(1) = REAL(DXCoolingSystem(DXSystemNum)%CoolingCoilIndex,r64)
               Par(2) = DesOutHumRat
               ! Dehumidification mode = 0 for normal mode, 1+ for enhanced mode
               Par(3) = REAL(DehumidMode,r64)
               Par(4) = REAL(FanOpMode,r64)
               CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, MultiModeDXCoilHumRatResidual, 0.0d0,   &
                                           1.0d0, Par)
               IF (SolFla == -1) THEN
                 IF(.NOT. WarmupFlag)THEN
                   IF(DXCoolingSystem(DXSystemNum)%MModeLatPLRIter2 .LT. 1)THEN
                     DXCoolingSystem(DXSystemNum)%MModeLatPLRIter2 = DXCoolingSystem(DXSystemNum)%MModeLatPLRIter2+1
                     CALL ShowWarningError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                           ' - Iteration limit exceeded calculating DX unit coolreheat'// &
                                           ' latent part-load ratio for unit = '//TRIM(DXCoolingSystem(DXSystemNum)%Name))
                     CALL ShowContinueError('Estimated part-load ratio  = '//RoundSigDigits((ReqOutput/FullOutput),3))
                     CALL ShowContinueError('Calculated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                     CALL ShowContinueErrorTimeStamp('The calculated part-load ratio will be used and the simulation'// &
                                                    ' continues. Occurrence info: ')
                   END IF
                   CALL ShowRecurringWarningErrorAtEnd(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)//' "'&
                       //TRIM(DXCoolingSystem(DXSystemNum)%Name)//'" - Iteration limit exceeded calculating'// &
                       ' coolreheat latent part-load ratio error continues. Latent PLR statistics follow.' &
                       ,DXCoolingSystem(DXSystemNum)%MModeLatPLRIterIndex2,PartLoadFrac,PartLoadFrac)
                 END IF
               ELSE IF (SolFla == -2) THEN
                 IF(.NOT. WarmupFlag) THEN
                   CALL ShowSevereError(TRIM(DXCoolingSystem(DXSystemNum)%DXCoolingSystemType)// &
                                        ' : part-load ratio calculation failed: '//  &
                                        'part-load ratio limits exceeded, for unit='// &
                                        TRIM(DXCoolingSystem(DXSystemNum)%Name))
                   CALL ShowFatalError('Program terminates due to previous condition.')
                 END IF
               END IF
             END IF
          END IF ! End if humidity ratio setpoint not met - CoolReheat humidity control

          IF(PartLoadFrac.GT.1.0) THEN
            PartLoadFrac = 1.0
          ELSEIF(PartLoadFrac < 0.0) THEN
            PartLoadFrac = 0.0
          END IF


        CASE DEFAULT
          CALL ShowFatalError('ControlDXSystem: Invalid DXCoolingSystem coil type = '//  &
                              TRIM(DXCoolingSystem(DXSystemNum)%CoolingCoilType))

      END SELECT
    END IF ! End of cooling load type (sensible or latent) if block
  END IF   ! End of If DXCoolingSystem is scheduled on and there is flow
  !Set the final results
  DXCoolingSystem(DXSystemNum)%PartLoadFrac = PartLoadFrac
  DXCoolingSystem(DXSystemNum)%SpeedRatio = SpeedRatio
  DXCoolingSystem(DXSystemNum)%CycRatio = CycRatio
  DXCoolingSystem(DXSystemNum)%DehumidificationMode = DehumidMode

RETURN
END Subroutine ControlDXSystem

FUNCTION DXCoilVarSpeedResidual(SpeedRatio, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   September 2002
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp).
          ! DX Coil output depends on the compressor speed which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls CalcMultiSpeedDXCoil to get outlet temperature at the given compressor speed
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils, ONLY: DXCoilOutletTemp, CalcMultiSpeedDXCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: SpeedRatio ! compressor speed ratio (1.0 is max, 0.0 is min)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                    ! par(2) = desired air outlet temperature [C]
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilIndex       ! index of this coil
  REAL(r64) :: OutletAirTemp   ! outlet air temperature [C]

  CoilIndex = INT(Par(1))
  CALL CalcMultiSpeedDXCoil(CoilIndex,SpeedRatio,1.0d0)
  OutletAirTemp = DXCoilOutletTemp(CoilIndex)
  Residuum = Par(2) - OutletAirTemp

  RETURN
END FUNCTION DXCoilVarSpeedResidual

FUNCTION DXCoilVarSpeedHumRatResidual(SpeedRatio, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   January 2008
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet humrat - actual outlet humrat).
          ! DX Coil output depends on the compressor speed which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls CalcMultiSpeedDXCoil to get outlet humidity ratio at the given compressor speed
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils, ONLY: DXCoilOutletHumRat, CalcMultiSpeedDXCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: SpeedRatio ! compressor speed ratio (1.0 is max, 0.0 is min)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                    ! par(2) = desired air outlet humidity ratio [kg/kg]
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilIndex        ! index of this coil
  REAL(r64) :: OutletAirHumRat  ! outlet air humidity ratio [kg/kg]

  CoilIndex = INT(Par(1))
  CALL CalcMultiSpeedDXCoil(CoilIndex,SpeedRatio,1.0d0)
  OutletAirHumRat = DXCoilOutletHumRat(CoilIndex)
  Residuum = Par(2) - OutletAirHumRat

  RETURN
END FUNCTION DXCoilVarSpeedHumRatResidual

FUNCTION DXCoilCyclingResidual(CycRatio, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   September 2002
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp)
          ! DX Coil output depends on the cycling ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls CalcMultiSpeedDXCoil to get outlet temperature at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils, ONLY: DXCoilOutletTemp, CalcMultiSpeedDXCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: CycRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                    ! par(2) = desired air outlet temperature [C]
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilIndex       ! index of this coil
  REAL(r64) :: OutletAirTemp   ! outlet air temperature [C]

  CoilIndex = INT(Par(1))
  CALL CalcMultiSpeedDXCoil(CoilIndex,0.0d0,CycRatio)
  OutletAirTemp = DXCoilOutletTemp(CoilIndex)
  Residuum = Par(2) - OutletAirTemp

  RETURN
END FUNCTION DXCoilCyclingResidual

FUNCTION DXCoilCyclingHumRatResidual(CycRatio, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   September 2002
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp)
          ! DX Coil output depends on the cycling ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls CalcMultiSpeedDXCoil to get outlet temperature at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils, ONLY: DXCoilOutletHumRat, CalcMultiSpeedDXCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: CycRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                    ! par(2) = desired air outlet humidity ratio [kg/kg]
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilIndex        ! index of this coil
  REAL(r64) :: OutletAirHumRat  ! outlet air humidity ratio [kg/kg]

  CoilIndex = INT(Par(1))
  CALL CalcMultiSpeedDXCoil(CoilIndex,0.0d0,CycRatio)
  OutletAirHumRat = DXCoilOutletHumRat(CoilIndex)
  Residuum = Par(2) - OutletAirHumRat

  RETURN
END FUNCTION DXCoilCyclingHumRatResidual

FUNCTION DOE2DXCoilResidual(PartLoadRatio, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   November 2003
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp)
          ! DX Coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls CalcDoe2DXCoil to get outlet temperature at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils, ONLY: DXCoilOutletTemp, CalcDoe2DXCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: PartLoadRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                    ! par(2) = desired air outlet temperature [C]
                                                    ! par(5) = supply air fan operating mode (ContFanCycCoil)
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilIndex       ! index of this coil
  REAL(r64) :: OutletAirTemp   ! outlet air temperature [C]
  INTEGER   :: FanOpMode       ! Supply air fan operating mode

  CoilIndex = INT(Par(1))
  FanOpMode = INT(Par(5))
  CALL CalcDoe2DXCoil(CoilIndex,On,.TRUE., PartLoadRatio,FanOpMode)
  OutletAirTemp = DXCoilOutletTemp(CoilIndex)
  Residuum = Par(2) - OutletAirTemp

  RETURN
END FUNCTION DOE2DXCoilResidual

FUNCTION DOE2DXCoilHumRatResidual(PartLoadRatio, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   January 2008
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet humrat - actual outlet humrat)
          ! DX Coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls CalcDoe2DXCoil to get outlet humidity ratio at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils, ONLY: DXCoilOutletHumRat, CalcDoe2DXCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: PartLoadRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                    ! par(2) = desired air outlet humidity ratio [kg/kg]
                                                    ! par(5) = supply air fan operating mode (ContFanCycCoil)
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilIndex       ! index of this coil
  REAL(r64) :: OutletAirHumRat ! outlet air humidity ratio [kg/kg]
  INTEGER   :: FanOpMode       ! Supply air fan operating mode

  CoilIndex = INT(Par(1))
  FanOpMode = INT(Par(5))
  CALL CalcDoe2DXCoil(CoilIndex,On,.TRUE., PartLoadRatio,FanOpMode)
  OutletAirHumRat = DXCoilOutletHumRat(CoilIndex)
  Residuum = Par(2) - OutletAirHumRat

  RETURN
END FUNCTION DOE2DXCoilHumRatResidual

FUNCTION MultiModeDXCoilResidual(PartLoadRatio, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         M. J. Witte, GARD Analytics, Inc.
          !       DATE WRITTEN   February 2005
          !                      (based on DOE2DXCoilResidual by Richard Raustad, FSEC)
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp)
          ! DX Coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls SimDXCoilMultiMode to get outlet temperature at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils, ONLY: DXCoilOutletTemp, SimDXCoilMultiMode

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: PartLoadRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                    ! par(2) = desired air outlet temperature [C]
                                                    ! par(3) = dehumidification mode (0=normal, 1=enhanced)
                                                    ! par(4) = supply air fan operating mode (ContFanCycCoil)
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilIndex       ! index of this coil
  REAL(r64) :: OutletAirTemp   ! outlet air temperature [C]
  INTEGER   :: DehumidMode     ! dehumidification mode (par3)
  INTEGER   :: FanOpMode       ! supply air fan operating mode

  CoilIndex   = INT(Par(1))
  DehumidMode = INT(Par(3))
  FanOpMode   = INT(Par(4))
  CALL SimDXCoilMultiMode('',On,.FALSE.,PartLoadRatio,DehumidMode,CoilIndex,FanOpMode)
  OutletAirTemp = DXCoilOutletTemp(CoilIndex)
  Residuum = Par(2) - OutletAirTemp

  RETURN
END FUNCTION MultiModeDXCoilResidual

FUNCTION MultiModeDXCoilHumRatResidual(PartLoadRatio, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   January 2008
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet humrat - actual outlet humrat)
          ! DX Coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls SimDXCoilMultiMode to get outlet humidity ratio at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils, ONLY: DXCoilOutletHumRat, SimDXCoilMultiMode

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: PartLoadRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                    ! par(2) = desired air outlet humidity ratio [kg/kg]
                                                    ! par(3) = dehumidification mode (0=normal, 1=enhanced)
                                                    ! par(4) = supply air fan operating mode (ContFanCycCoil)
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilIndex       ! index of this coil
  REAL(r64) :: OutletAirHumRat ! outlet air humidity ratio [kg/kg]
  INTEGER   :: DehumidMode     ! dehumidification mode (par3)
  INTEGER   :: FanOpMode       ! supply air fan operating mode

  CoilIndex   = INT(Par(1))
  DehumidMode = INT(Par(3))
  FanOpMode   = INT(Par(4))
  CALL SimDXCoilMultiMode('',On,.FALSE.,PartLoadRatio,DehumidMode,CoilIndex,FanOpMode)
  OutletAirHumRat = DXCoilOutletHumRat(CoilIndex)
  Residuum = Par(2) - OutletAirHumRat

  RETURN
END FUNCTION MultiModeDXCoilHumRatResidual

FUNCTION HXAssistedCoolCoilTempResidual(PartLoadRatio, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   November 2003
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          !  Calculates residual function (desired outlet temp - actual outlet temp)
          !  DX Coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          !  Calls CalcHXAssistedCoolingCoil to get outlet temperature at the given part load ratio
          !  and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE HVACHXAssistedCoolingCoil, ONLY: HXAssistedCoilOutletTemp, CalcHXAssistedCoolingCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)     :: PartLoadRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                  ! par(2) = desired air outlet temperature [C]
                                                  ! par(3) = FirstHVACIteration logical converted to numeric (1=TRUE,0=FALSE)
                                                  ! par(4) = HX control (On/Off)
                                                  ! par(5) = supply air fan operating mode (ContFanCycCoil)
  REAL(r64)         :: Residuum  ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          !  na

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilIndex          ! index of this coil
  REAL(r64) :: OutletAirTemp      ! outlet air temperature [C]
  LOGICAL   :: FirstHVACIteration ! FirstHVACIteration flag
  LOGICAL   :: HXUnitOn           ! flag to enable heat exchanger heat recovery
  INTEGER   :: FanOpMode          ! Supply air fan operating mode

  CoilIndex = INT(Par(1))
  ! FirstHVACIteration is a logical, Par is REAL(r64), so make 1=TRUE and 0=FALSE
  IF(Par(3) .EQ. 1.0)THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  IF(Par(4) .EQ. 1.0)THEN
    HXUnitOn = .TRUE.
  ELSE
    HXUnitOn = .FALSE.
  END IF
  FanOpMode = INT(Par(5))
  CALL CalcHXAssistedCoolingCoil(CoilIndex,FirstHVACIteration,On,PartLoadRatio, HXUnitOn, FanOpMode)
  OutletAirTemp = HXAssistedCoilOutletTemp(CoilIndex)
  Residuum = Par(2) - OutletAirTemp
  RETURN

END FUNCTION HXAssistedCoolCoilTempResidual

FUNCTION HXAssistedCoolCoilHRResidual(PartLoadRatio, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   January 2008
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          !  Calculates residual function (desired outlet humrat - actual outlet humrat)
          !  DX Coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          !  Calls CalcHXAssistedCoolingCoil to get outlet humidity ratio at the given part load ratio
          !  and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE HVACHXAssistedCoolingCoil, ONLY: HXAssistedCoilOutletHumRat, CalcHXAssistedCoolingCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)     :: PartLoadRatio ! compressor cycling ratio (1.0 is continuous, 0.0 is off)
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = DX coil number
                                                  ! par(2) = desired air outlet humidity ratio [kg/kg]
                                                  ! par(3) = FirstHVACIteration logical converted to numeric (1=TRUE,0=FALSE)
                                                  ! par(4) = HX control (On/Off)
                                                  ! par(5) = supply air fan operating mode (ContFanCycCoil)
  REAL(r64)         :: Residuum  ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          !  na

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilIndex          ! index of this coil
  REAL(r64) :: OutletAirHumRat    ! outlet air humidity ratio [kg/kg]
  LOGICAL   :: FirstHVACIteration ! FirstHVACIteration flag
  LOGICAL   :: HXUnitOn           ! flag to enable heat exchanger heat recovery
  INTEGER   :: FanOpMode          ! Supply air fan operating mode

  CoilIndex = INT(Par(1))
  ! FirstHVACIteration is a logical, Par is REAL(r64), so make 1=TRUE and 0=FALSE
  IF(Par(3) .EQ. 1.0)THEN
    FirstHVACIteration = .TRUE.
  ELSE
    FirstHVACIteration = .FALSE.
  END IF
  IF(Par(4) .EQ. 1.0)THEN
    HXUnitOn = .TRUE.
  ELSE
    HXUnitOn = .FALSE.
  END IF
  FanOpMode = INT(Par(5))
  CALL CalcHXAssistedCoolingCoil(CoilIndex,FirstHVACIteration,On,PartLoadRatio, HXUnitOn, FanOpMode, &
                                 EconomizerFlag=EconomizerFlag)
  OutletAirHumRat = HXAssistedCoilOutletHumRat(CoilIndex)
  Residuum = Par(2) - OutletAirHumRat
  RETURN

END FUNCTION HXAssistedCoolCoilHRResidual




!        End of Calculation subroutines for the DXCoolingSystem Module
! *****************************************************************************


!     NOTICE
!
!     Copyright © 1996-2012 The Board of Trustees of the University of Illinois
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

End Module HVACDXSystem

!******************************************************************************************************

MODULE HVACDXHeatPumpSystem
  ! Module containing the DXHeatPumpSystem simulation routines

  ! MODULE INFORMATION:
  !       AUTHOR         Brent Griffith (derived from HVACDXSystem.f90 by R.Liesen)
  !       DATE WRITTEN   May 2011
  !                      
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required to
  ! manage the DX Heat Pump System System Component
  ! this wraps heat pump air-heating coils in coil-only wrapper with no fans. 

  ! METHODOLOGY EMPLOYED:
  !

  ! REFERENCES:


  ! OTHER NOTES:
  !

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals
USE DataHVACGlobals
USE DataInterfaces

  ! Use statements for access to subroutines in other modules
USE ScheduleManager

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS
REAL(r64), PARAMETER :: MinAirMassFlow = 0.001d0
! Compressor operation
INTEGER, PARAMETER :: On =  1              ! normal compressor operation
INTEGER, PARAMETER :: Off = 0              ! signal DXCoil that compressor shouldn't run



  ! DERIVED TYPE DEFINITIONS

TYPE DXHeatPumpSystemStruct
  CHARACTER(len=MaxNameLength) :: DXHeatPumpSystemType=' ' ! Type of DXHeatingSystem
  CHARACTER(len=MaxNameLength) :: Name               =' ' ! Name of the DXHeatingSystem
  INTEGER                      :: SchedPtr           =0
  CHARACTER(len=MaxNameLength) :: HeatPumpCoilType    =' ' !
  INTEGER                      :: HeatPumpCoilType_Num=0
  CHARACTER(len=MaxNameLength) :: HeatPumpCoilName    =' ' !
  INTEGER                      :: HeatPumpCoilIndex   =0
  INTEGER      :: DXHeatPumpCoilInletNodeNum          =0
  INTEGER      :: DXHeatPumpCoilOutletNodeNum         =0
  Integer      :: DXSystemControlNodeNum             =0   ! the node number of the node with the set point
  REAL(r64)    :: DesiredOutletTemp                  =0.0 ! the temperature at the unit outlet node needed
                                                          ! to meet the supply air set point.

  REAL(r64)    :: PartLoadFrac                       =0.0 ! part load fraction for current time step (single speed)
  REAL(r64)    :: SpeedRatio                         =0.0 ! current compressor speed ratio (variable speed)
  REAL(r64)    :: CycRatio                           =0.0 ! cycling part load ratio (variable speed)
  INTEGER      :: FanOpMode                          =0   ! Fan operating mode (see parameter above)

! Warning message variables



  INTEGER      :: DXCoilSensPLRIter                   =0   ! used in DXCoil calculations
  INTEGER      :: DXCoilSensPLRIterIndex              =0   ! used in DXCoil calculations
  INTEGER      :: DXCoilSensPLRFail                   =0   ! used in DXCoil calculations
  INTEGER      :: DXCoilSensPLRFailIndex              =0   ! used in DXCoil calculations

! When the Dx system is a part of Outdoor Air Unit
  REAL(r64)    :: OAUnitSetTemp                        =0.0 ! set

END TYPE DXHeatPumpSystemStruct

!MODULE VARIABLE DECLARATIONS:
INTEGER :: NumDXHeatPumpSystems=0   ! The Number of DXHeatPumpSystems found in the Input
LOGICAL :: EconomizerFlag=.FALSE. ! holds air loop economizer status

! Make this type allocatable
TYPE (DXHeatPumpSystemStruct), ALLOCATABLE, DIMENSION(:) :: DXHeatPumpSystem
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

! Subroutine Specifications for the Module
          ! Driver/Manager Routines
PUBLIC  SimDXHeatPumpSystem

          ! Get Input routines for module
PRIVATE GetDXHeatPumpSystemInput

PRIVATE InitDXHeatPumpSystem

          ! Update routine to check convergence and update nodes
PRIVATE ControlDXHeatingSystem

PRIVATE DXHeatingCoilResidual



CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE SimDXHeatPumpSystem(DXHeatPumpSystemName, FirstHVACIteration, AirLoopNum,CompIndex,OAUnitNum,OAUCoilOutTemp,QTotOut)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith (derived from HVACDXSystem.f90 by R.Liesen)
          !       DATE WRITTEN   May 2011

          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages DXHeatPumpSystem component simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DXCoils,          ONLY: SimDXCoil 
  USE General,          ONLY: TrimSigDigits
  USE DataAirLoop,      ONLY: AirLoopControlInfo
  USE InputProcessor,   ONLY: FindItemInList
  USE HVACHXAssistedCoolingCoil, ONLY: SimHXAssistedCoolingCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: DXHeatPumpSystemName ! Name of DXSystem:Airloop object
  LOGICAL, INTENT(IN)          :: FirstHVACIteration  ! True when first HVAC iteration
  INTEGER, INTENT(IN)          :: AirLoopNum          ! Primary air loop number
  INTEGER, INTENT(INOUT)       :: CompIndex           ! Index to CoilSystem:Heating:DX object
  INTEGER, INTENT(IN), OPTIONAL:: OAUnitNum           ! If the system is an equipment of OutdoorAirUnit
  REAL(r64), INTENT(IN), OPTIONAL :: OAUCoilOutTemp   ! the coil inlet temperature of OutdoorAirUnit
  REAL(r64), INTENT(INOUT), OPTIONAL :: QTotOut       ! the total cooling output of unit
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DEFINITIONS:
          ! na

          ! FLOW:


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength)  :: CompName              ! Name of CoilSystem:Heating:DX object
  INTEGER                       :: DXSystemNum           ! Index to CoilSystem:Heating:DX object
  LOGICAL,SAVE                  :: GetInputFlag = .True. ! Flag to get input only once
  LOGICAL                       :: HXUnitOn              ! Flag to control HX for HXAssisted Cooling Coil
  REAL(r64)                     :: AirMassFlow           ! DX System air mass flow rate
  INTEGER                       :: InletNodeNum          ! DX System inlet node number
  INTEGER                       :: OutletNodeNum         ! DX System outlet node number

    ! Obtains and Allocates DX Cooling System related parameters from input file
  IF (GetInputFlag) THEN  !First time subroutine has been entered
        !Get the DXCoolingSystem input
    CALL GetDXHeatPumpSystemInput
    GetInputFlag=.false.
  End If

    ! Find the correct DXSystemNumber
  IF (CompIndex == 0) THEN
    DXSystemNum = FindItemInList(DXHeatPumpSystemName,DXHeatPumpSystem%Name,NumDXHeatPumpSystems)
    IF (DXSystemNum == 0) THEN
      CALL ShowFatalError('SimDXHeatPumpSystem: DXUnit not found='//TRIM(DXHeatPumpSystemName))
    ENDIF
    CompIndex=DXSystemNum
  ELSE
    DXSystemNum=CompIndex
    IF (DXSystemNum > NumDXHeatPumpSystems .or. DXSystemNum < 1) THEN
      CALL ShowFatalError('SimDXHeatPumpSystem:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(DXSystemNum))// &
                          ', Number of DX Units='//TRIM(TrimSigDigits(NumDXHeatPumpSystems))//  &
                          ', DX Unit name='//TRIM(DXHeatPumpSystemName))
    ENDIF
    IF (CheckEquipName(DXSystemNum)) THEN
      IF (DXHeatPumpSystemName /= DXHeatPumpSystem(DXSystemNum)%Name) THEN
        CALL ShowFatalError('SimDXHeatPumpSystem: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(DXSystemNum))// &
                            ', DX Unit name='//TRIM(DXHeatPumpSystemName)//', stored DX Unit Name for that index='//  &
                            TRIM(DXHeatPumpSystem(DXSystemNum)%Name))
      ENDIF
      CheckEquipName(DXSystemNum)=.false.
    ENDIF
  ENDIF

  IF (PRESENT(OAUnitNum)) THEN
    CALL InitDXHeatPumpSystem(DXSystemNum,AirLoopNum,OAUnitNum=OAUnitNum,OAUCoilOutTemp=OAUCoilOutTemp)
  ELSE
    CALL InitDXHeatPumpSystem(DXSystemNum,AirLoopNum)
  ENDIF

  !Call the series of components that simulate a DX Heating System
   ! Control the DX Heating System
  CALL ControlDXHeatingSystem(DXSystemNum, FirstHVACIteration)

    ! simulate DX Heating System
  CompName = DXHeatPumpSystem(DXSystemNum)%HeatPumpCoilName

    SELECT CASE(DXHeatPumpSystem(DXSystemNum)%HeatPumpCoilType_Num)

    CASE (CoilDX_HeatingEmpirical) ! COIL:DX:COOLINGBYPASSFACTOREMPIRICAL

      CALL SimDXCoil(CompName,On,FirstHVACIteration, DXHeatPumpSystem(DXSystemNum)%PartLoadFrac,  &
         DXHeatPumpSystem(DXSystemNum)%HeatPumpCoilIndex, &
         DXHeatPumpSystem(DXSystemNum)%FanOpMode)


    CASE DEFAULT
      CALL ShowFatalError('SimDXCoolingSystem: Invalid DX Heating System/Coil='//  &
                          TRIM(DXHeatPumpSystem(DXSystemNum)%HeatPumpCoilType))

  END SELECT
  ! set econo lockout flag
    ! set econo lockout flag
  IF (AirLoopNum /=-1) THEN ! IF the sysem is not an equipment of outdoor air unit

  IF ( (DXHeatPumpSystem(DXSystemNum)%PartLoadFrac > 0.0 ) .AND. &
       AirLoopControlInfo(AirLoopNum)%CanLockoutEconoWithCompressor) THEN
       AirLoopControlInfo(AirLoopNum)%ReqstEconoLockoutWithCompressor = .TRUE.
  ELSE
    AirLoopControlInfo(AirLoopNum)%ReqstEconoLockoutWithCompressor = .FALSE.
  END IF
  END IF

  IF(PRESENT(QTotOut))THEN
    InletNodeNum  = DXHeatPumpSystem(DXSystemNum)%DXHeatPumpCoilInletNodeNum
    OutletNodeNum = DXHeatPumpSystem(DXSystemNum)%DXHeatPumpCoilOutletNodeNum
    AirMassFlow = Node(OutletNodeNum)%MassFlowRate
    QTotOut = AirMassFlow * (Node(InletNodeNum)%Enthalpy - Node(OutletNodeNum)%Enthalpy)
  END IF

  RETURN

END SUBROUTINE SimDXHeatPumpSystem

! Get Input Section of the Module
!******************************************************************************
SUBROUTINE GetDXHeatPumpSystemInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith (derived from HVACDXSystem.f90 by R.Liesen)
          !       DATE WRITTEN   May 2011
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for system and stores it in System data structures

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in data.

          ! REFERENCES:

          ! USE STATEMENTS:
    USE InputProcessor
    USE NodeInputManager,      ONLY: GetOnlySingleNode
    USE DataHeatBalance,       ONLY: Zone
    USE BranchNodeConnections, ONLY: SetUpCompSets, TestCompSet
    USE HVACHXAssistedCoolingCoil,  ONLY: GetHXDXCoilName
    USE DataIPShortCuts
    USE DXCoils,               ONLY: GetCoilInletNode, GetCoilOutletNode, SetCoilSystemHeatingDXFlag

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
    INTEGER :: DXSystemNum      ! The DXHeatingSystem that you are currently loading input into
    INTEGER :: NumAlphas
    INTEGER :: NumNums
    INTEGER :: IOSTAT
    CHARACTER(len=*), PARAMETER    :: RoutineName='GetDXHeatPumpSystemInput: ' ! include trailing blank space
    LOGICAL :: ErrorsFound = .false.   ! If errors detected in input
    LOGICAL :: IsNotOK               ! Flag to verify name
    LOGICAL :: IsBlank               ! Flag for blank name
    Integer :: DXHeatSysNum
    LOGICAL :: FanErrorsFound        ! flag returned on fan operating mode check
    LOGICAL :: DXErrorsFound         ! flag returned on DX coil name check
    CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in getting objects
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas         ! Alpha input items for object
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
    REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers           ! Numeric input items for object
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.
    INTEGER                              :: TotalArgs=0       ! Total number of alpha and numeric arguments (max) for a
                                                              !  certain object in the input file

          ! Flow

    CurrentModuleObject='CoilSystem:Heating:DX'
    NumDXHeatPumpSystems = GetNumObjectsFound(TRIM(CurrentModuleObject))

    ALLOCATE(DXHeatPumpSystem(NumDXHeatPumpSystems))
    ALLOCATE(CheckEquipName(NumDXHeatPumpSystems))
    CheckEquipName=.true.

    CALL GetObjectDefMaxArgs('CoilSystem:Heating:DX',TotalArgs,NumAlphas,NumNums)

    ALLOCATE(Alphas(NumAlphas))
    Alphas=' '
    ALLOCATE(cAlphaFields(NumAlphas))
    cAlphaFields=' '
    ALLOCATE(cNumericFields(NumNums))
    cNumericFields=' '
    ALLOCATE(Numbers(NumNums))
    Numbers=0.0
    ALLOCATE(lAlphaBlanks(NumAlphas))
    lAlphaBlanks=.TRUE.
    ALLOCATE(lNumericBlanks(NumNums))
    lNumericBlanks=.TRUE.


      ! Get the data for the DX Cooling System
      DO DXHeatSysNum = 1,  NumDXHeatPumpSystems

        CALL GetObjectItem(TRIM(CurrentModuleObject),DXHeatSysNum,Alphas,NumAlphas, &
                     Numbers,NumNums,IOSTAT,NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                     AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(Alphas(1),DXHeatPumpSystem%Name,DXHeatSysNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) Alphas(1) ='xxxxx'
        ENDIF
        DXHeatPumpSystem(DXHeatSysNum)%DXHeatPumpSystemType = CurrentModuleObject ! push Object Name into data array
        DXHeatPumpSystem(DXHeatSysNum)%Name            = Alphas(1)
        DXHeatPumpSystem(DXHeatSysNum)%SchedPtr        = GetScheduleIndex(Alphas(2))
        IF (DXHeatPumpSystem(DXHeatSysNum)%SchedPtr == 0) THEN
          IF (lAlphaBlanks(2)) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': '//TRIM(cAlphaFields(2))//  &
                 ' is required, missing for '//TRIM(cAlphaFields(1))//'='//TRIM(Alphas(1)))
          ELSE
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': invalid '//TRIM(cAlphaFields(2))//  &
               ' entered ='//TRIM(Alphas(2))// &
               ' for '//TRIM(cAlphaFields(1))//'='//TRIM(Alphas(1)))
          END IF
          ErrorsFound=.true.
        END IF
        
        IF (SameString(Alphas(3),'Coil:Heating:DX:SingleSpeed'))  THEN

          DXHeatPumpSystem(DXHeatSysNum)%HeatPumpCoilType = Alphas(3)
          DXHeatPumpSystem(DXHeatSysNum)%HeatPumpCoilType_Num=CoilDX_HeatingEmpirical

          DXHeatPumpSystem(DXHeatSysNum)%HeatPumpCoilName = Alphas(4)

        ELSE
          CALL ShowSevereError('Invalid entry for '//TRIM(cAlphaFields(3))//' :'//TRIM(Alphas(3)))
          CALL ShowContinueError('In '//TRIM(CurrentModuleObject)//'="'//TRIM(DXHeatPumpSystem(DXHeatSysNum)%Name)//'".')
          ErrorsFound=.true.
        END IF

        DXHeatPumpSystem(DXHeatSysNum)%DXHeatPumpCoilInletNodeNum      = GetCoilInletNode(  &
                                                                           DXHeatPumpSystem(DXHeatSysNum)%HeatPumpCoilType, &
                                                                           DXHeatPumpSystem(DXHeatSysNum)%HeatPumpCoilName, &
                                                                           ErrorsFound )
        
        DXHeatPumpSystem(DXHeatSysNum)%DXHeatPumpCoilOutletNodeNum     = GetCoilOutletNode(  &
                                                                           DXHeatPumpSystem(DXHeatSysNum)%HeatPumpCoilType, &
                                                                           DXHeatPumpSystem(DXHeatSysNum)%HeatPumpCoilName, &
                                                                           ErrorsFound )

        DXHeatPumpSystem(DXHeatSysNum)%DXSystemControlNodeNum = DXHeatPumpSystem(DXHeatSysNum)%DXHeatPumpCoilOutletNodeNum
        
        CALL TestCompSet(TRIM(CurrentModuleObject),DXHeatPumpSystem(DXHeatSysNum)%Name,  &
                                                   NodeID(DXHeatPumpSystem(DXHeatSysNum)%DXHeatPumpCoilInletNodeNum), &
                                                   NodeID(DXHeatPumpSystem(DXHeatSysNum)%DXHeatPumpCoilOutletNodeNum) ,&
                                                   'Air Nodes')

        CALL ValidateComponent(DXHeatPumpSystem(DXHeatSysNum)%HeatPumpCoilType,DXHeatPumpSystem(DXHeatSysNum)%HeatPumpCoilName, &
                               IsNotOK,TRIM(CurrentModuleObject))
        IF (IsNotOK) THEN
          CALL ShowContinueError('In '//TRIM(CurrentModuleObject)//' = "'//TRIM(DXHeatPumpSystem(DXHeatSysNum)%Name)//'".')
          ErrorsFound=.true.
        ENDIF

        CALL SetUpCompSets(DXHeatPumpSystem(DXHeatSysNum)%DXHeatPumpSystemType, &
                           DXHeatPumpSystem(DXHeatSysNum)%Name, &
                           DXHeatPumpSystem(DXHeatSysNum)%HeatPumpCoilType, &
                           DXHeatPumpSystem(DXHeatSysNum)%HeatPumpCoilName, &
                           NodeID(DXHeatPumpSystem(DXHeatSysNum)%DXHeatPumpCoilInletNodeNum), &
                           NodeID(DXHeatPumpSystem(DXHeatSysNum)%DXHeatPumpCoilOutletNodeNum) )


        ! Supply air fan operating mode defaulted to constant fan cycling coil/compressor
        DXHeatPumpSystem(DXHeatSysNum)%FanOpMode = ContFanCycCoil
        
        CALL SetCoilSystemHeatingDXFlag(DXHeatPumpSystem(DXHeatSysNum)%HeatPumpCoilType, &
                                   DXHeatPumpSystem(DXHeatSysNum)%HeatPumpCoilName)

      END DO  !End of the DX System Loop


      IF (ErrorsFound) THEN
        CALL ShowFatalError(RoutineName//'Errors found in input.  Program terminates.')
      ENDIF

      DO DXHeatSysNum=1,NumDXHeatPumpSystems
        ! Setup Report variables for the DXHeatingSystem that is not reported in the components themselves
        CALL SetupOutputVariable('Coil System Part Load Ratio []',DXHeatPumpSystem(DXHeatSysNum)%PartLoadFrac, &
                                'System','Average',DXHeatPumpSystem(DXHeatSysNum)%Name)
      END DO

      DEALLOCATE(Alphas)
      DEALLOCATE(cAlphaFields)
      DEALLOCATE(cNumericFields)
      DEALLOCATE(Numbers)
      DEALLOCATE(lAlphaBlanks)
      DEALLOCATE(lNumericBlanks)

  RETURN

END SUBROUTINE GetDXHeatPumpSystemInput


! End of Get Input subroutines for the Module
!******************************************************************************

! Beginning of Initialization subroutines for the Module
! *****************************************************************************

SUBROUTINE InitDXHeatPumpSystem(DXSystemNum,AirLoopNum,OAUnitNum,OAUCoilOutTemp)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith (derived from HVACDXSystem.f90 by R.Liesen)
          !
          !       DATE WRITTEN   May 2011
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the DX heat pump Systems.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY: DoSetPointTest
  USE DataAirLoop,     ONLY: AirLoopControlInfo
  USE EMSManager,      ONLY: iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS, iHumidityRatioMaxSetpoint
  USE DataGlobals,     ONLY: AnyEnergyManagementSystemInModel

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: DXSystemNum ! number of the current DX Sys being simulated
  INTEGER, INTENT (IN) :: AirLoopNum  ! number of the current air loop being simulated
  INTEGER, INTENT (IN), OPTIONAL :: OAUnitNum  ! number of the current outdoor air unit being simulated
  REAL(r64), INTENT(IN), OPTIONAL :: OAUCoilOutTemp    ! the coil inlet temperature of OutdoorAirUnit

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  Integer             :: OutNode ! outlet node number
  INTEGER             :: ControlNode ! control node number
  INTEGER             :: DXSysIndex
!  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL,SAVE        :: MySetPointCheckFlag = .TRUE.
  INTEGER             :: OutdoorAirUnitNum ! "ONLY" for ZoneHVAC:OutdoorAirUnit
  REAL(r64)           :: OAUCoilOutletTemp  ! "ONLY" for zoneHVAC:OutdoorAirUnit
          ! FLOW:

!  IF (MyOneTimeFlag) THEN
!
!    MyOneTimeFlag = .false.
!  END IF
  IF (PRESENT(OAUnitNum)) THEN ! This Dx system is component of ZoneHVAC:OutdoorAirUnit
    OutdoorAirUnitNum=OAUnitNum
    OAUCoilOutletTemp=OAUCoilOutTemp
  END IF

  IF ( .NOT. SysSizingCalc .AND. MySetPointCheckFlag .AND. DoSetPointTest) THEN
    DO DXSysIndex=1,NumDXHeatPumpSystems
      ControlNode = DXHeatPumpSystem(DXSysIndex)%DXSystemControlNodeNum
      IF (ControlNode > 0) THEN
        IF (AirLoopNum .EQ.-1) THEN                           ! Outdoor Air Unit
          Node(ControlNode)%TempSetPoint = OAUCoilOutletTemp  ! Set the coil outlet temperature
        ELSE IF (AirLoopNum /= -1) THEN ! Not an outdoor air unit

          IF (Node(ControlNode)%TempSetPoint == SensedNodeFlagValue) THEN
            IF (.NOT. AnyEnergyManagementSystemInModel) THEN
              CALL ShowSevereError(TRIM(DXHeatPumpSystem(DXSysIndex)%DXHeatPumpSystemType)//&
                               ': Missing temperature setpoint for DX unit= ' //TRIM(DXHeatPumpSystem(DXSysIndex)%Name))
              CALL ShowContinueError('  use a Set Point Manager to establish a setpoint at the unit control node.')
              SetPointErrorFlag = .TRUE.
            ELSE
              CALL CheckIfNodeSetpointManagedByEMS(ControlNode,iTemperatureSetpoint, SetpointErrorFlag)
              IF (SetpointErrorFlag) THEN
                CALL ShowSevereError(TRIM(DXHeatPumpSystem(DXSysIndex)%DXHeatPumpSystemType)//&
                               ': Missing temperature setpoint for DX unit= ' //TRIM(DXHeatPumpSystem(DXSysIndex)%Name))
                CALL ShowContinueError('  use a Set Point Manager to establish a setpoint at the unit control node.')
                CALL ShowContinueError('  or use an EMS actuator to establish a temperature setpoint at the unit control node.')
              ENDIF
            ENDIF
          END IF
        END IF
      END IF
    END DO
    MySetPointCheckFlag = .FALSE.
  END IF

! These initializations are done every iteration
IF (AirLoopNum .EQ.-1) THEN ! This IF-Then routine is just for ZoneHVAC:OUTDOORAIRUNIT

  DXHeatPumpSystem(DXSystemNum)%DesiredOutletTemp =OAUCoilOutletTemp

ELSEIF (AirLoopNum /=-1) THEN ! Not Outdoor Air Unit
  ControlNode = DXHeatPumpSystem(DXSystemNum)%DXSystemControlNodeNum
  EconomizerFlag = AirLoopControlInfo(AirLoopNum)%EconoActive
  DXHeatPumpSystem(DXSystemNum)%DesiredOutletTemp = Node(ControlNode)%TempSetPoint

END IF
RETURN
END SUBROUTINE InitDXHeatPumpSystem

! End of Initialization subroutines for the Module
! *****************************************************************************

! Beginning of Calculation subroutines for the DXCoolingSystem Module
! *****************************************************************************

SUBROUTINE ControlDXHeatingSystem(DXSystemNum, FirstHVACIteration )
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith (derived from ControlDXSystem by Richard Liesen)
          !       DATE WRITTEN   Jan 2012
          !       MODIFIED       Richard Raustad, FSEC Nov 2003
          !                      Feb 2005 M. J. Witte, GARD Analytics, Inc.
          !                        Add dehumidification controls and support for multimode DX coil
          !                      Jan 2008 R. Raustad, FSEC. Added coolreheat to all coil types
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  This subroutine updates the System outlet nodes.

          ! METHODOLOGY EMPLOYED:
          !  Data is moved from the System data structure to the System outlet nodes.

          ! REFERENCES:
          !  na

          ! USE STATEMENTS:
  USE ScheduleManager
  USE DataEnvironment, ONLY: OutBaroPress
  USE DataHVACGlobals, ONLY: TempControlTol
  USE InputProcessor,  ONLY: FindItemInList
  USE Psychrometrics , ONLY: PsyHFnTdbW, PsyTdpFnWPb
  USE General,         ONLY: SolveRegulaFalsi, RoundSigDigits
  USE DXCoils,         ONLY: SimDXCoil, DXCoilOutletTemp


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,  INTENT(In)    :: DXSystemNum             ! index to DXSystem
  LOGICAL,  INTENT(In)    :: FirstHVACIteration      ! First HVAC iteration flag

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER          :: MaxIte    = 500     ! Maximum number of iterations for solver
  REAL(r64), PARAMETER :: Acc       = 1.d-3   ! Accuracy of solver result
  REAL(r64), PARAMETER :: HumRatAcc = 1.d-6   ! Accuracy of solver result

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength)  :: CompName  ! Name of the DX cooling coil
  REAL(r64)           :: NoOutput            ! Sensible capacity (outlet - inlet) when the compressor is off
  REAL(r64)           :: FullOutput          ! Sensible capacity (outlet - inlet) when the compressor is on
  REAL(r64)           :: ReqOutput           ! Sensible capacity (outlet - inlet) required to meet load or set point temperature
  Integer             :: InletNode           ! Inlet node number of the DX cooling coil
  Integer             :: OutletNode          ! Outlet node number of the DX cooling coil
  Integer             :: ControlNode         ! The node number where a set point is placed to control the DX cooling coil
  REAL(r64)           :: PartLoadFrac        ! The part-load fraction of the compressor

  REAL(r64)           :: DesOutTemp          ! Desired outlet temperature of the DX cooling coil
  REAL(r64)           :: OutletTempDXCoil    ! Actual outlet temperature of the DX cooling coil

  INTEGER             :: SolFla              ! Flag of solver
  REAL(r64), DIMENSION(5)  :: Par                 ! Parameter array passed to solver
  LOGICAL             :: SensibleLoad        ! True if there is a sensible cooling load on this system
  LOGICAL             :: LatentLoad          ! True if there is a latent   cooling load on this system
  INTEGER             :: FanOpMode           ! Supply air fan operating mode
  REAL(r64)           :: TempMinPLR          ! Used to find latent PLR when max iterations exceeded
  REAL(r64)           :: TempMaxPLR          ! Used to find latent PLR when max iterations exceeded
  REAL(r64)           :: TempOutletTempDXCoil   ! Used to find latent PLR when max iterations exceeded

      ! Set local variables
      ! Retrieve the load on the controlled zone
  OutletNode   = DXHeatPumpSystem(DXSystemNum)%DXHeatPumpCoilOutletNodeNum
  InletNode    = DXHeatPumpSystem(DXSystemNum)%DXHeatPumpCoilInletNodeNum
  ControlNode  = DXHeatPumpSystem(DXSystemNum)%DXSystemControlNodeNum
  DesOutTemp   = DXHeatPumpSystem(DXSystemNum)%DesiredOutletTemp
  CompName     = DXHeatPumpSystem(DXSystemNum)%HeatPumpCoilName
  FanOpMode    = DXHeatPumpSystem(DXSystemNum)%FanOpMode

  PartLoadFrac = 0.0

  SensibleLoad = .FALSE.

  ! If DXHeatingSystem is scheduled on and there is flow
  If((GetCurrentScheduleValue(DXHeatPumpSystem(DXSystemNum)%SchedPtr) > 0.d0) .AND. &
     (Node(InletNode)%MassFlowRate .gt. MinAirMassFlow)) THEN

    ! Determine if there is a sensible load on this system
    IF((Node(InletNode)%Temp < Node(ControlNode)%TempSetPoint) .AND. &
       (Node(InletNode)%Temp < DesOutTemp) .AND. &
       (ABS(Node(InletNode)%Temp - DesOutTemp) .gt. TempControlTol) ) SensibleLoad = .TRUE.


    ! If DXHeatingSystem runs with a heating load then set PartLoadFrac on Heating System 
    IF (SensibleLoad ) THEN
      SELECT CASE(DXHeatPumpSystem(DXSystemNum)%HeatPumpCoilType_Num)

        CASE (CoilDX_HeatingEmpirical)  ! Coil:Heating:DX:SingleSpeed

          ! Get no load result
          PartLoadFrac = 0.0
          CALL SimDXCoil(CompName,On,FirstHVACIteration,PartLoadFrac,DXHeatPumpSystem(DXSystemNum)%HeatPumpCoilIndex,FanOpMode)
          NoOutput = Node(InletNode)%MassFlowRate *  &
                       (PsyHFnTdbW(Node(OutletNode)%Temp,Node(OutletNode)%HumRat)  &
                        - PsyHFnTdbW(Node(InletNode)%Temp,Node(OutletNode)%HumRat))

          ! Get full load result
          PartLoadFrac = 1.0
          CALL SimDXCoil(CompName,On,FirstHVACIteration,PartLoadFrac,DXHeatPumpSystem(DXSystemNum)%HeatPumpCoilIndex,FanOpMode)

          FullOutput = Node(InletNode)%MassFlowRate *  &
                       (PsyHFnTdbW(Node(OutletNode)%Temp,Node(InletNode)%HumRat)  &
                        - PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))

          ReqOutput = Node(InletNode)%MassFlowRate *  &
                       (PsyHFnTdbW(DXHeatPumpSystem(DXSystemNum)%DesiredOutletTemp,Node(InletNode)%HumRat) - &
                        PsyHFnTdbW(Node(InletNode)%Temp,Node(InletNode)%HumRat))

!         IF NoOutput is higher than (more heating than required) or very near the ReqOutput, do not run the compressor
          IF ((NoOutput-ReqOutput) > Acc) THEN
            PartLoadFrac = 0.0
!         If the FullOutput is greater than (insufficient heating) or very near the ReqOutput,
!         run the compressor at PartLoadFrac = 1.
          ELSE IF ((FullOutput - ReqOutput) < Acc) THEN
            PartLoadFrac = 1.0
!         Else find the PLR to meet the load
          ELSE
!           OutletTempDXCoil is the full capacity outlet temperature at PartLoadFrac = 1 from the CALL above. If this temp is
!           greater than the desired outlet temp, then run the compressor at PartLoadFrac = 1, otherwise find the operating PLR.
            OutletTempDXCoil = DXCoilOutletTemp(DXHeatPumpSystem(DXSystemNum)%HeatPumpCoilIndex)
            IF (OutletTempDXCoil < DesOutTemp) THEN
              PartLoadFrac = 1.0
            ELSE
              Par(1) = REAL(DXHeatPumpSystem(DXSystemNum)%HeatPumpCoilIndex,r64)
              Par(2) = DesOutTemp
              Par(3) = 1.d0  !OnOffAirFlowFrac assume = 1.0 for continuous fan dx system
              Par(5) = REAL(FanOpMode,r64)
              CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, PartLoadFrac, DXHeatingCoilResidual, 0.0d0,   &
                                            1.0d0, Par)
              IF (SolFla == -1) THEN
                IF(.NOT. WarmupFlag)THEN
                  IF(DXHeatPumpSystem(DXSystemNum)%DXCoilSensPLRIter .LT. 1)THEN
                    DXHeatPumpSystem(DXSystemNum)%DXCoilSensPLRIter = DXHeatPumpSystem(DXSystemNum)%DXCoilSensPLRIter+1
                    CALL ShowWarningError(TRIM(DXHeatPumpSystem(DXSystemNum)%DXHeatPumpSystemType)// &
                                          ' - Iteration limit exceeded calculating DX unit sensible '// &
                                          'part-load ratio for unit = '//TRIM(DXHeatPumpSystem(DXSystemNum)%Name))
                    CALL ShowContinueError('Estimated part-load ratio  = '//RoundSigDigits((ReqOutput/FullOutput),3))
                    CALL ShowContinueError('Calculated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                    CALL ShowContinueErrorTimeStamp('The calculated part-load ratio will be used and the simulation'// &
                                                    ' continues. Occurrence info: ')
                  ELSE
                    CALL ShowRecurringWarningErrorAtEnd(TRIM(DXHeatPumpSystem(DXSystemNum)%DXHeatPumpSystemType)//' "'&
                      //TRIM(DXHeatPumpSystem(DXSystemNum)%Name)//'" - Iteration limit exceeded calculating'// &
                      ' sensible part-load ratio error continues. Sensible PLR statistics follow.' &
                      ,DXHeatPumpSystem(DXSystemNum)%DXCoilSensPLRIterIndex,PartLoadFrac,PartLoadFrac)
                  END IF
                END IF
              ELSE IF (SolFla == -2) THEN
                PartLoadFrac = ReqOutput/FullOutput
                IF(.NOT. WarmupFlag)THEN
                  IF(DXHeatPumpSystem(DXSystemNum)%DXCoilSensPLRFail .LT. 1)THEN
                    DXHeatPumpSystem(DXSystemNum)%DXCoilSensPLRFail = DXHeatPumpSystem(DXSystemNum)%DXCoilSensPLRFail+1
                    CALL ShowWarningError(TRIM(DXHeatPumpSystem(DXSystemNum)%DXHeatPumpSystemType)//' - DX unit sensible part-'// &
                                    'load ratio calculation failed: part-load ratio limits exceeded, for unit = '// &
                                     TRIM(DXHeatPumpSystem(DXSystemNum)%Name))
                    CALL ShowContinueError('Estimated part-load ratio = '//RoundSigDigits(PartLoadFrac,3))
                    CALL ShowContinueErrorTimeStamp('The estimated part-load ratio will be used and the simulation'// &
                                                    ' continues. Occurrence info: ')
                  ELSE
                    CALL ShowRecurringWarningErrorAtEnd(TRIM(DXHeatPumpSystem(DXSystemNum)%DXHeatPumpSystemType)//' "'&
                      //TRIM(DXHeatPumpSystem(DXSystemNum)%Name)//'" - DX unit sensible part-load ratio calculation'// &
                      ' failed error continues. Sensible PLR statistics follow.' &
                      ,DXHeatPumpSystem(DXSystemNum)%DXCoilSensPLRFailIndex,PartLoadFrac,PartLoadFrac)
                  END IF
                END IF

              END IF
            END IF
          END IF

          IF(PartLoadFrac.GT.1.0) THEN
            PartLoadFrac = 1.0
          ELSEIF(PartLoadFrac < 0.0) THEN
            PartLoadFrac = 0.0
          END IF


        CASE DEFAULT
          CALL ShowFatalError('ControlDXHeatingSystem: Invalid DXHeatPumpSystem coil type = '//  &
                              TRIM(DXHeatPumpSystem(DXSystemNum)%HeatPumpCoilType))

      END SELECT
    END IF ! End of cooling load type (sensible or latent) if block
  END IF   ! End of If DXheatingSystem is scheduled on and there is flow
  !Set the final results
  DXHeatPumpSystem(DXSystemNum)%PartLoadFrac = PartLoadFrac


RETURN
END Subroutine ControlDXHeatingSystem


FUNCTION DXHeatingCoilResidual(PartLoadFrac, Par) RESULT (Residuum)
          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   June 2006
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (desired outlet temp - actual outlet temp)
          ! DX Coil output depends on the part load ratio which is being varied to zero the residual.

          ! METHODOLOGY EMPLOYED:
          ! Calls CalcDoe2DXCoil to get outlet temperature at the given cycling ratio
          ! and calculates the residual as defined above

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils, ONLY: DXCoilOutletTemp, CalcDXHeatingCoil

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: PartLoadFrac               ! Compressor cycling ratio (1.0 is continuous, 0.0 is off)
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! Par(1) = DX coil number
                                                    ! Par(2) = desired air outlet temperature [C]
    REAL(r64)         :: Residuum                   ! Residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: CoilIndex        ! Index of this coil
  REAL(r64)    :: OutletAirTemp    ! Outlet air temperature [C]
  REAL(r64)    :: OnOffAirFlowFrac ! Ratio of compressor ON to compressor OFF air mass flow rate

  CoilIndex        = INT(Par(1))
  OnOffAirFlowFrac = Par(3)

  CALL CalcDXHeatingCoil(CoilIndex,PartLoadFrac,ContFanCycCoil,OnOffAirFlowFrac)

  OutletAirTemp = DXCoilOutletTemp(CoilIndex)
  Residuum      = Par(2) - OutletAirTemp

  RETURN
END FUNCTION DXHeatingCoilResidual

END MODULE HVACDXHeatPumpSystem

!        End of Calculation subroutines for the DXCoolingSystem Module
! *****************************************************************************


!     NOTICE
!
!     Copyright © 1996-2012 The Board of Trustees of the University of Illinois
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


