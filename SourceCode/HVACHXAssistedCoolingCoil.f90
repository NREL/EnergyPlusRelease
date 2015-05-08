Module HVACHXAssistedCoolingCoil
  ! Module containing the simulation routines for a heat exchanger-
  ! assisted cooling coil

  ! MODULE INFORMATION:
  !       AUTHOR         Richard Raustad, FSEC
  !       DATE WRITTEN   Sept 2003
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  !  To encapsulate the data and algorithms required to
  !  manage the heat exchanger-assisted cooling coil compound component

  ! METHODOLOGY EMPLOYED:
  !  Call the air-to-air heat exchanger and cooling coil repeatedly to converge
  !  on the solution instead of relying on the air loop manager for iterations

  ! REFERENCES:
  ! Kosar, D. 2006. Dehumidification Enhancements, ASHRAE Journal, Vol. 48, No. 2, February 2006.
  ! Kosar, D. et al. 2006. Dehumidification Enhancement of Direct Expansion Systems Through Component
  !   Augmentation of the Cooling Coil. 15th Symposium on Improving Building Systems in Hot and Humid
  !   Climates, July 24-26, 2006.


  ! OTHER NOTES:


  ! USE STATEMENTS:
  !  Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataGlobals
USE DataHVACGlobals
USE DataInterfaces
!unused0909USE DataEnvironment, ONLY: CurMnDy, EnvironmentName

IMPLICIT NONE   ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
! Compressor operation
INTEGER, PARAMETER :: On =  1              ! normal compressor operation
INTEGER, PARAMETER :: Off = 0              ! signal DXCoil that compressor shouldn't run

  ! DERIVED TYPE DEFINITIONS
TYPE HXAssistedCoilParameters
  CHARACTER(len=MaxNameLength) :: HXAssistedCoilType =' '  ! Type of HXAssistedCoolingCoil
  INTEGER                      :: HXAssistedCoilType_Num=0 ! Numeric equivalent for hx assisted coil
  CHARACTER(len=MaxNameLength) :: Name               =' '  ! Name of the HXAssistedCoolingCoil
  CHARACTER(len=MaxNameLength) :: CoolingCoilType    =' '  ! Cooling coil type must be DetailedFlatCooling
                                                           !  or Coil:DX:CoolingBypassFactorEmpirical
  INTEGER                      :: CoolingCoilType_Num=0    ! Numeric Equivalent for cooling coil
  CHARACTER(len=MaxNameLength) :: CoolingCoilName    =' '  ! Cooling coil name
  INTEGER                      :: CoolingCoilIndex   =0
  CHARACTER(len=MaxNameLength) :: HeatExchangerType  =' '  ! Heat Exchanger type must be HeatExchanger:AirToAir:FlatPlate,
                                                           ! HeatExchanger:AirToAir:SensibleAndLatent or
                                                           ! HeatExchanger:Desiccant:BalancedFlow
  INTEGER                      :: HeatExchangerType_Num =0  ! Numeric Equivalent for heat exchanger
  CHARACTER(len=MaxNameLength) :: HeatExchangerName  =' '  ! Heat Exchanger name
  INTEGER                      :: HeatExchangerIndex = 0   ! Heat Exchanger index
  INTEGER                      :: HXAssistedCoilInletNodeNum=0  ! Inlet node to HXAssistedCoolingCoil compound object
  INTEGER                      :: HXAssistedCoilOutletNodeNum=0 ! Outlet node to HXAssistedCoolingCoil compound object
  INTEGER                      :: HXExhaustAirInletNodeNum   =0   ! Inlet node number for air-to-air heat exchanger
  REAL(r64)                    :: MassFlowRate               =0.0d0 ! Mass flow rate through HXAssistedCoolingCoil compound object
  INTEGER                      :: MaxIterCounter             =0   ! used in warning messages
  INTEGER                      :: MaxIterIndex               =0   ! used in warning messages
END TYPE HXAssistedCoilParameters

  ! MODULE VARIABLE DECLARATIONS:
INTEGER :: TotalNumHXAssistedCoils=0                            ! The total number of HXAssistedCoolingCoil compound objects
TYPE (HXAssistedCoilParameters), ALLOCATABLE, DIMENSION(:) :: HXAssistedCoil
REAL(r64), PUBLIC, ALLOCATABLE, DIMENSION(:) :: HXAssistedCoilOutletTemp   ! Outlet temperature from this compound object
REAL(r64), PUBLIC, ALLOCATABLE, DIMENSION(:) :: HXAssistedCoilOutletHumRat ! Outlet humidity ratio from this compound object
                                                              ! PUBLIC so others can access this information
LOGICAL     :: GetCoilsInputFlag = .True. ! Flag to allow input data to be retrieved from idf on first call to this subroutine
LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

  ! Subroutine Specifications for the Module
          ! Driver/Manager Routines
PUBLIC  SimHXAssistedCoolingCoil

          ! Get Input routines for module
PRIVATE GetHXAssistedCoolingCoilInput

          ! Initialization routines for module
PRIVATE InitHXAssistedCoolingCoil

          ! Calculation algorithms for the module
PUBLIC CalcHXAssistedCoolingCoil
          ! Update routine to update output node information
!PRIVATE UpdateHXAssistedCoolingCoil
          ! Not required.  All updates done by the individual components
          ! (cooling coil and air-to-air heat exchanger)

          ! Reporting routines for module
!PRIVATE ReportHXAssistedCoolingCoil
          ! No reporting variables for this compound component

          ! Utility routines for module
PUBLIC GetHXDXCoilIndex
PUBLIC CheckHXAssistedCoolingCoilSchedule
PUBLIC GetCoilCapacity
PUBLIC GetCoilGroupTypeNum
PUBLIC GetCoilObjectTypeNum
PUBLIC GetCoilInletNode
PUBLIC GetCoilWaterInletNode
PUBLIC GetCoilOutletNode
PUBLIC GetHXDXCoilName
PUBLIC GetHXCoilType
PUBLIC GetHXCoilTypeAndName
PUBLIC GetCoilMaxWaterFlowRate
PUBLIC GetHXCoilAirFlowRate
PUBLIC VerifyHeatExchangerParent
PUBLIC GetActualDXCoilIndex

CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE SimHXAssistedCoolingCoil(HXAssistedCoilName,FirstHVACIteration,CompOp,PartLoadRatio,CompIndex, &
                                    FanOpMode, HXUnitEnable, OnOffAFR, EconomizerFlag,QTotOut)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   Sept 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  This subroutine manages the simulation of the
          !  cooling coil/heat exchanger combination.

          ! METHODOLOGY EMPLOYED:
          !  na

          ! REFERENCES:
          !  na

          ! USE STATEMENTS:
  USE InputProcessor,   ONLY: FindItemInList
  USE General,          ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL,             INTENT(IN) :: FirstHVACIteration  ! FirstHVACIteration flag
  CHARACTER(len=*),    INTENT(IN) :: HXAssistedCoilName  ! Name of HXAssistedCoolingCoil
  INTEGER,             INTENT(IN) :: CompOp              ! compressor operation; 1=on, 0=off
  REAL(r64),           INTENT(IN) :: PartLoadRatio       ! Part load ratio of Coil:DX:CoolingBypassFactorEmpirical
                                                         ! (not used for Coil:Water:DetailedFlatCooling)
  INTEGER,             INTENT(INOUT) :: CompIndex
  INTEGER,             INTENT(IN) :: FanOpMode         ! Allows the parent object to control fan operation
  LOGICAL,   OPTIONAL, INTENT(IN) :: HXUnitEnable      ! flag to enable heat exchanger heat recovery
  REAL(r64), OPTIONAL, INTENT(IN) :: OnOffAFR          ! Ratio of compressor ON air mass flow rate to AVERAGE over time step
  LOGICAL,   OPTIONAL, INTENT(IN) :: EconomizerFlag    ! OA sys or air loop economizer status
  REAL(r64), OPTIONAL, INTENT(INOut) :: QTotOut        ! the total cooling output of unit

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER          :: HXAssistedCoilNum     ! Index for HXAssistedCoolingCoil
  REAL(r64)        :: AirFlowRatio          ! Ratio of compressor ON air mass flow rate to AVEARAGE over time step
  LOGICAL          :: HXUnitOn              ! flag to enable heat exchanger
  REAL(r64)        :: AirMassFlow           ! HX System air mass flow rate
  INTEGER          :: InletNodeNum          ! HX System inlet node number
  INTEGER          :: OutletNodeNum         ! HX System outlet node number

  ! Obtains and allocates HXAssistedCoolingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  ! First time subroutine has been called, get input data
    ! Get the HXAssistedCoolingCoil input
    CALL GetHXAssistedCoolingCoilInput
    GetCoilsInputFlag=.false. ! Set logic flag to disallow getting the input data on future calls to this subroutine
  End If

  ! Find the correct HXAssistedCoolingCoil number
  IF (CompIndex == 0) THEN
    HXAssistedCoilNum = FindItemInList(HXAssistedCoilName,HXAssistedCoil%Name,TotalNumHXAssistedCoils)
    IF (HXAssistedCoilNum == 0) THEN
      CALL ShowFatalError('HX Assisted Coil not found='//TRIM(HXAssistedCoilName))
    ENDIF
    CompIndex=HXAssistedCoilNum
  ELSE
    HXAssistedCoilNum=CompIndex
    IF (HXAssistedCoilNum > TotalNumHXAssistedCoils .or. HXAssistedCoilNum < 1) THEN
      CALL ShowFatalError('SimHXAssistedCoolingCoil: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(HXAssistedCoilNum))// &
                          ', Number of HX Assisted Cooling Coils='//TRIM(TrimSigDigits(TotalNumHXAssistedCoils))//  &
                          ', Coil name='//TRIM(HXAssistedCoilName))
    ENDIF
    IF (CheckEquipName(HXAssistedCoilNum)) THEN
      IF (HXAssistedCoilName /= Blank .AND. HXAssistedCoilName /= HXAssistedCoil(HXAssistedCoilNum)%Name) THEN
        CALL ShowFatalError('SimHXAssistedCoolingCoil: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(HXAssistedCoilNum))// &
                            ', Coil name='//TRIM(HXAssistedCoilName)//', stored Coil Name for that index='//  &
                            TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name))
      ENDIF
      CheckEquipName(HXAssistedCoilNum)=.false.
    ENDIF
  ENDIF

  ! Initialize HXAssistedCoolingCoil Flows
  Call InitHXAssistedCoolingCoil(HXAssistedCoilNum)

  IF(PRESENT(HXUnitEnable))THEN
    HXUnitOn = HXUnitEnable
  ELSE
    HXUnitOn = .TRUE.
  END IF

  IF(CompOp .EQ. Off)THEN
    HXUnitOn = .FALSE.
  END IF

    ! Calculate the HXAssistedCoolingCoil performance and the coil outlet conditions
    IF (PRESENT(OnOffAFR)) THEN
      AirFlowRatio = OnOffAFR
      Call CalcHXAssistedCoolingCoil(HXAssistedCoilNum,FirstHVACIteration,CompOp,PartLoadRatio, HXUnitOn, FanOpMode, &
                                     OnOffAirFlow = AirFlowRatio, EconomizerFlag=EconomizerFlag)
    ELSE
      AirFlowRatio = 1.0d0
      Call CalcHXAssistedCoolingCoil(HXAssistedCoilNum,FirstHVACIteration,CompOp,PartLoadRatio, HXUnitOn, FanOpMode, &
                                     OnOffAirFlow = AirFlowRatio, EconomizerFlag=EconomizerFlag)
    END IF

  ! Update the current HXAssistedCoil output
!  Call UpdateHXAssistedCoolingCoil(HXAssistedCoilNum), not required. Updates done by the HX and cooling coil components.

  ! Report the current HXAssistedCoil output
!  Call ReportHXAssistedCoolingCoil(HXAssistedCoilNum), not required. No reporting variables for this compound component.

  IF(PRESENT(QTotOut))THEN
    InletNodeNum  = HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilInletNodeNum
    OutletNodeNum = HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilOutletNodeNum
    AirMassFlow = Node(OutletNodeNum)%MassFlowRate
    QTotOut = AirMassFlow * (Node(InletNodeNum)%Enthalpy - Node(OutletNodeNum)%Enthalpy)
  END IF

  RETURN

END SUBROUTINE SimHXAssistedCoolingCoil

! Get Input Section of the Module
!******************************************************************************
SUBROUTINE GetHXAssistedCoolingCoilInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   Sept 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Obtains input data for this compount object and stores it in data structure

          ! METHODOLOGY EMPLOYED:
          !  Uses "Get" routines to read in data.

          ! REFERENCES:

          ! USE STATEMENTS:
    USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString, GetObjectDefMaxArgs
    USE NodeInputManager, ONLY: GetOnlySingleNode
    USE DataHeatBalance, ONLY: Zone
    USE BranchNodeConnections, ONLY: SetUpCompSets, TestCompSet
    USE DXCoils,               ONLY: GetDXCoilInletNode=>GetCoilInletNode, GetDXCoilOutletNode=>GetCoilOutletNode, &
                                     GetDXCoilIndex
    USE WaterCoils,            ONLY: GetWaterCoilInletNode=>GetCoilInletNode, GetWaterCoilOutletNode=>GetCoilOutletNode
    USE HeatRecovery,          ONLY: GetSupplyInletNode, GetSupplyOutletNode, GetSecondaryInletNode, GetSecondaryOutletNode

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER  :: RoutineName = 'GetHXAssistedCoolingCoilInput: '  ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: HXAssistedCoilNum      ! Index number of the HXAssistedCoolingCoil for which input data is being read from the idf
    INTEGER :: NumAlphas              ! Number of alpha inputs
    INTEGER :: NumNums                ! Number of number inputs
    INTEGER :: IOSTAT                 ! Return status from GetObjectItem call
    LOGICAL :: ErrorsFound = .false.   ! set TRUE if errors detected in input
    LOGICAL :: IsNotOK                 ! Flag to verify name
    LOGICAL :: IsBlank                 ! Flag for blank name
    Integer :: NumHXAssistedDXCoils    ! Number of HXAssistedCoolingCoil objects using a DX coil
    Integer :: NumHXAssistedWaterCoils ! Number of HXAssistedCoolingCoil objects using a chilled water coil
!    LOGICAL :: FanErrFlag              ! Error flag for fan operating mode mining call
    LOGICAL :: HXErrFlag               ! Error flag for HX node numbers mining call
    LOGICAL :: CoolingCoilErrFlag      ! Error flag for cooling coil node numbers mining call
    INTEGER :: SupplyAirInletNode      ! supply air inlet node number mined from heat exchanger object (ExchCond structure)
    INTEGER :: SupplyAirOutletNode     ! supply air outlet node number mined from heat exchanger object (ExchCond structure)
    INTEGER :: SecondaryAirInletNode   ! secondary air inlet node number mined from heat exchanger object (ExchCond structure)
    INTEGER :: SecondaryAirOutletNode  ! secondary air outlet node number mined from heat exchanger object (ExchCond structure)
    INTEGER :: CoolingCoilInletNodeNum  ! outlet node number of cooling coil, used for warning messages
    INTEGER :: CoolingCoilOutletNodeNum ! outlet node number of cooling coil, used for warning messages
    CHARACTER(len=MaxNameLength) :: CurrentModuleObject      ! Object type for getting and error messages
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: AlphArray      ! Alpha input items for object
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
    REAL(r64), ALLOCATABLE, DIMENSION(:) :: NumArray          ! Numeric input items for object
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.
    INTEGER :: MaxNums=0                ! Maximum number of numeric input fields
    INTEGER :: MaxAlphas=0              ! Maximum number of alpha input fields
    INTEGER :: TotalArgs=0              ! Total number of alpha and numeric arguments (max) for a


    NumHXAssistedDXCoils    = GetNumObjectsFound('CoilSystem:Cooling:DX:HeatExchangerAssisted')
    NumHXAssistedWaterCoils = GetNumObjectsFound('CoilSystem:Cooling:Water:HeatExchangerAssisted')
    TotalNumHXAssistedCoils = NumHXAssistedDXCoils + NumHXAssistedWaterCoils
    IF (TotalNumHXAssistedCoils.GT.0) THEN
      ALLOCATE(HXAssistedCoil(TotalNumHXAssistedCoils))
      ALLOCATE(HXAssistedCoilOutletTemp(TotalNumHXAssistedCoils))
      ALLOCATE(HXAssistedCoilOutletHumRat(TotalNumHXAssistedCoils))
      ALLOCATE(CheckEquipName(TotalNumHXAssistedCoils))
      CheckEquipName=.true.
    ENDIF

    CALL GetObjectDefMaxArgs('CoilSystem:Cooling:DX:HeatExchangerAssisted',TotalArgs,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    CALL GetObjectDefMaxArgs('CoilSystem:Cooling:Water:HeatExchangerAssisted',TotalArgs,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)

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

      ! Get the data for the Coil:DX:CoolingHeatExchangerAssisted objects
      CurrentModuleObject = 'CoilSystem:Cooling:DX:HeatExchangerAssisted'

      DO HXAssistedCoilNum = 1, NumHXAssistedDXCoils
        CALL GetObjectItem(CurrentModuleObject,HXAssistedCoilNum,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(AlphArray(1),HXAssistedCoil%Name, &
             HXAssistedCoilNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) AlphArray(1)='xxxxx'
        ENDIF
        HXAssistedCoil(HXAssistedCoilNum)%Name = AlphArray(1)

        HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType = AlphArray(2)
        HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerName = AlphArray(3)

        IF(SameString(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType , 'HeatExchanger:AirToAir:SensibleAndLatent')) THEN
          HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType_Num = HX_AIRTOAIR_GENERIC
        ELSEIF(SameString(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType , 'HeatExchanger:AirToAir:FlatPlate')) THEN
          HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType_Num = HX_AIRTOAIR_FLATPLATE
        ELSEIF(SameString(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType , 'HeatExchanger:Desiccant:BalancedFlow')) THEN
          HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType_Num = HX_DESICCANT_BALANCED
        ELSE
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name)//'"')
          CALL ShowContinueError('Invalid '//trim(cAlphaFields(2))//'="'//  &
             TRIM(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType)//'"')
          ErrorsFound=.true.
        END IF

        HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType = AlphArray(4)
        HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilName = AlphArray(5)

        IF(SameString(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType,'Coil:Cooling:DX:SingleSpeed')) THEN
          HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType_Num=CoilDX_CoolingSingleSpeed
          HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilType = TRIM(CurrentModuleObject)
          HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilType_Num=CoilDX_CoolingHXAssisted
          CoolingCoilErrFlag = .FALSE.
          CALL GetDXCoilIndex(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilName, &
                              HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilIndex, &
                              CoolingCoilErrFlag, HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType)
          IF(CoolingCoilErrFlag)THEN
            CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)// &
                                                  '="'//TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name)//'"')
            ErrorsFound=.true.
          END IF
        ELSE
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name)//'"')
          CALL ShowContinueError('Invalid '//trim(cAlphaFields(4))//'="'//  &
             TRIM(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType)//'"')
          ErrorsFound=.true.
        END IF

        HXErrFlag = .FALSE.
        SupplyAirInletNode = GetSupplyInletNode(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerName, HXErrFlag)
        IF(HXErrFlag)THEN
          CALL ShowContinueError('...Occurs in '//TRIM(CurrentModuleObject)//'="'//  &
             TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name)//'"')
        END IF

        HXErrFlag = .FALSE.
        SupplyAirOutletNode = GetSupplyOutletNode(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerName, HXErrFlag)
        IF(HXErrFlag)THEN
          CALL ShowContinueError('...Occurs in '//TRIM(CurrentModuleObject)//'="'//  &
             TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name)//'"')
        END IF

        HXErrFlag = .FALSE.
        SecondaryAirInletNode = GetSecondaryInletNode(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerName, HXErrFlag)
        IF(HXErrFlag)THEN
          CALL ShowContinueError('...Occurs in '//TRIM(CurrentModuleObject)//'="'//  &
             TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name)//'"')
        END IF

        HXErrFlag = .FALSE.
        SecondaryAirOutletNode = GetSecondaryOutletNode(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerName, HXErrFlag)
        IF(HXErrFlag)THEN
          CALL ShowContinueError('...Occurs in '//TRIM(CurrentModuleObject)//'="'//  &
             TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name)//'"')
        END IF

        IF(SameString(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType,'Coil:Cooling:DX:SingleSpeed')) THEN
!         Check node names in heat exchanger and coil objects for consistency
          CoolingCoilErrFlag = .FALSE.
          CoolingCoilInletNodeNum = &
             GetDXCoilInletNode(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType, &
                              HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilName, CoolingCoilErrFlag)
          IF(CoolingCoilErrFlag) THEN
            CALL ShowContinueError('...Occurs in '//TRIM(CurrentModuleObject)//'="'//  &
               TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name)//'"')
          ENDIF
          IF(SupplyAirOutletNode .NE. CoolingCoilInletNodeNum)THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name)//'"')
            CALL ShowContinueError('Node names are inconsistent in heat exchanger and cooling coil object.')
            CALL ShowContinueError('The supply air outlet node name in heat exchanger = ' &
                                  //TRIM(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType)//'="' &
                                  //TRIM(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerName)//'"')
            CALL ShowContinueError('must match the cooling coil inlet node name in = ' &
                                  //TRIM(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType)//'="' &
                                  //TRIM(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilName)//'"')
            CALL ShowContinueError('Heat exchanger supply air outlet node name="'//TRIM(NodeID(SupplyAirOutletNode))//'"')
            CALL ShowContinueError('Cooling coil air inlet node name="' &
                                  //TRIM(NodeID(CoolingCoilInletNodeNum))//'"')
            ErrorsFound=.true.
          END IF
          CoolingCoilErrFlag = .FALSE.
          CoolingCoilOutletNodeNum = GetDXCoilOutletNode(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType, &
                                            HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilName, CoolingCoilErrFlag)
          IF(CoolingCoilErrFlag) THEN
            CALL ShowContinueError('...Occurs in '//TRIM(CurrentModuleObject)//'="'//  &
               TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name)//'"')
          ENDIF
          IF(SecondaryAirInletNode .NE. CoolingCoilOutletNodeNum)THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name)//'"')
            CALL ShowContinueError('Node names are inconsistent in heat exchanger and cooling coil object.')
            CALL ShowContinueError('The secondary air inlet node name in heat exchanger =' &
                                  //TRIM(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType)//'="' &
                                  //TRIM(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerName)//'"')
            CALL ShowContinueError('must match the cooling coil air outlet node name in = ' &
                                  //TRIM(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType)//'="' &
                                  //TRIM(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilName)//'".')
            CALL ShowContinueError('Heat exchanger secondary air inlet node name ="'//TRIM(NodeID(SecondaryAirInletNode))//'".')
            CALL ShowContinueError('Cooling coil air outlet node name ="' &
                                  //TRIM(NodeID(CoolingCoilOutletNodeNum))//'".')
            ErrorsFound=.true.
          END IF

        END IF

        CALL TestCompSet(HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilType,HXAssistedCoil(HXAssistedCoilNum)%Name, &
                         NodeID(SupplyAirInletNode),NodeID(SecondaryAirOutletNode),'Air Nodes')

        HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilInletNodeNum = &
               GetOnlySingleNode(NodeID(SupplyAirInletNode),ErrorsFound, &
                            TRIM(CurrentModuleObject),HXAssistedCoil(HXAssistedCoilNum)%Name, &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)
        CoolingCoilInletNodeNum = &
               GetOnlySingleNode(NodeID(SupplyAirOutletNode),ErrorsFound, &
                            TRIM(CurrentModuleObject),HXAssistedCoil(HXAssistedCoilNum)%Name, &
                            NodeType_Air,NodeConnectionType_Internal,1,ObjectIsParent)
        HXAssistedCoil(HXAssistedCoilNum)%HXExhaustAirInletNodeNum = &
               GetOnlySingleNode(NodeID(SecondaryAirInletNode),ErrorsFound, &
                            TRIM(CurrentModuleObject),HXAssistedCoil(HXAssistedCoilNum)%Name, &
                            NodeType_Air,NodeConnectionType_Internal,1,ObjectIsParent)
        HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilOutletNodeNum = &
               GetOnlySingleNode(NodeID(SecondaryAirOutletNode),ErrorsFound, &
                           TRIM(CurrentModuleObject),HXAssistedCoil(HXAssistedCoilNum)%Name, &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)

        ! Add cooling coil to component sets array
        CALL SetUpCompSets(HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilType, HXAssistedCoil(HXAssistedCoilNum)%Name, &
                   HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType,HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilName, &
                   NodeID(SupplyAirOutletNode),NodeID(SecondaryAirInletNode), 'Air Nodes')
        ! Add heat exchanger to component sets array
        CALL SetUpCompSets(HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilType, HXAssistedCoil(HXAssistedCoilNum)%Name, &
                   HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType,HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerName, &
                   NodeID(SupplyAirInletNode),NodeID(SupplyAirOutletNode), 'Process Air Nodes')
        CALL SetUpCompSets(HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilType, HXAssistedCoil(HXAssistedCoilNum)%Name, &
                   HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType,HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerName, &
                   NodeID(SecondaryAirInletNode),NodeID(SecondaryAirOutletNode), 'Secondary Air Nodes')

       END DO  ! End of the Coil:DX:CoolingHXAssisted Loop

      ! Get the data for the Coil:Water:CoolingHeatExchangerAssisted objects
      CurrentModuleObject = 'CoilSystem:Cooling:Water:HeatExchangerAssisted'

      DO HXAssistedCoilNum = NumHXAssistedDXCoils + 1, NumHXAssistedWaterCoils

        CALL GetObjectItem(CurrentModuleObject,HXAssistedCoilNum,AlphArray,NumAlphas,NumArray,NumNums,IOSTAT, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(AlphArray(1),HXAssistedCoil%Name, &
             HXAssistedCoilNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) AlphArray(1)='xxxxx'
        ENDIF
        HXAssistedCoil(HXAssistedCoilNum)%Name = AlphArray(1)

        HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType = AlphArray(2)
        HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerName = AlphArray(3)

        IF(SameString(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType,'HeatExchanger:AirToAir:SensibleAndLatent')) THEN
          HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType_Num = HX_AIRTOAIR_GENERIC
        ELSEIF(SameString(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType,'HeatExchanger:AirToAir:FlatPlate')) THEN
          HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType_Num = HX_AIRTOAIR_FLATPLATE
!       balanced desiccant HX not allowed with water coils at this time
!       ELSEIF(SameString(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType,'HeatExchanger:Desiccant:BalancedFlow')) THEN
!         HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType_Num = HX_DESICCANT_BALANCED
        ELSE
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name)//'"')
          CALL ShowContinueError('Invalid '//trim(cAlphaFields(2))//'="'//  &
             TRIM(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType)//'"')
          ErrorsFound=.true.
        END IF

        HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType = AlphArray(4)
        HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilName = AlphArray(5)

        HXErrFlag = .FALSE.
        SupplyAirInletNode = GetSupplyInletNode(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerName, HXErrFlag)
        IF(HXErrFlag)THEN
          CALL ShowContinueError('...Occurs in '//TRIM(CurrentModuleObject)//'="'//  &
             TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name)//'"')
        END IF

        HXErrFlag = .FALSE.
        SupplyAirOutletNode = GetSupplyOutletNode(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerName, HXErrFlag)
        IF(HXErrFlag)THEN
          CALL ShowContinueError('...Occurs in '//TRIM(CurrentModuleObject)//'="'//  &
             TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name))
        END IF

        HXErrFlag = .FALSE.
        SecondaryAirInletNode = GetSecondaryInletNode(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerName, HXErrFlag)
        IF(HXErrFlag)THEN
          CALL ShowContinueError('...Occurs in '//TRIM(CurrentModuleObject)//'="'//  &
             TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name)//'"')
        END IF

        HXErrFlag = .FALSE.
        SecondaryAirOutletNode = GetSecondaryOutletNode(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerName, HXErrFlag)
        IF(HXErrFlag)THEN
          CALL ShowContinueError('...Occurs in '//TRIM(CurrentModuleObject)//'="'//  &
             TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name)//'"')
        END IF

        IF(SameString(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType,'Coil:Cooling:Water') .OR. &
           SameString(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType,'Coil:Cooling:Water:DetailedGeometry')) THEN
          IF (SameString(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType,'Coil:Cooling:Water:DetailedGeometry')) THEN
            HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType_Num = Coil_CoolingWaterDetailed
          ELSEIF (SameString(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType,'Coil:Cooling:Water')) THEN
            HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType_Num = Coil_CoolingWater
          ENDIF

          HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilType     = TRIM(CurrentModuleObject)
          HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilType_Num = CoilWater_CoolingHXAssisted

!         Check node names in heat exchanger and coil objects for consistency
          CoolingCoilErrFlag = .FALSE.
          CoolingCoilInletNodeNum = GetWaterCoilInletNode(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType, &
                                           HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilName, CoolingCoilErrFlag)
          IF(CoolingCoilErrFlag)CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)// &
                                                  ' "'//TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name)//'"')
          IF(SupplyAirOutletNode .NE. CoolingCoilInletNodeNum)THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name)//'"')
            CALL ShowContinueError('Node names are inconsistent in heat exchanger and cooling coil object.')
            CALL ShowContinueError('The supply air outlet node name in heat exchanger = ' &
                                  //TRIM(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType)//'="' &
                                  //TRIM(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerName)//'"')
            CALL ShowContinueError('must match the cooling coil inlet node name in = ' &
                                  //TRIM(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType)//'="' &
                                  //TRIM(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilName)//'"')
            CALL ShowContinueError('Heat exchanger supply air outlet node name ="'//TRIM(NodeID(SupplyAirOutletNode))//'"')
            CALL ShowContinueError('Cooling coil air inlet node name = "' &
                                  //TRIM(NodeID(CoolingCoilInletNodeNum))//'"')
            ErrorsFound=.true.
          END IF
          CoolingCoilErrFlag = .FALSE.
          CoolingCoilOutletNodeNum = GetWaterCoilOutletNode(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType, &
                                            HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilName, CoolingCoilErrFlag)
          IF(CoolingCoilErrFlag)CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)// &
                                                  ' "'//TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name)//'"')
          IF(SecondaryAirInletNode .NE. CoolingCoilOutletNodeNum)THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name)//'"')
            CALL ShowContinueError('Node names are inconsistent in heat exchanger and cooling coil object.')
            CALL ShowContinueError('The secondary air inlet node name in heat exchanger = ' &
                                  //TRIM(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType)//'="' &
                                  //TRIM(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerName)//'"')
            CALL ShowContinueError('must match the cooling coil air outlet node name in = ' &
                                  //TRIM(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType)//'="' &
                                  //TRIM(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilName)//'".')
            CALL ShowContinueError('Heat exchanger secondary air inlet node name = "'//TRIM(NodeID(SecondaryAirInletNode))//'".')
            CALL ShowContinueError('Cooling coil air outlet node name = "' &
                                  //TRIM(NodeID(CoolingCoilOutletNodeNum))//'".')
            ErrorsFound=.true.
          END IF

        ELSE
          CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name)//'"')
          CALL ShowContinueError('Invalid '//trim(cAlphaFields(4))//'="'//  &
             TRIM(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType)//'"')
          ErrorsFound=.true.
        END IF

        CALL TestCompSet(HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilType,HXAssistedCoil(HXAssistedCoilNum)%Name, &
                         NodeID(SupplyAirInletNode),NodeID(SecondaryAirOutletNode),'Air Nodes')

        HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilInletNodeNum = &
               GetOnlySingleNode(NodeID(SupplyAirInletNode),ErrorsFound, &
                           TRIM(CurrentModuleObject),HXAssistedCoil(HXAssistedCoilNum)%Name, &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)
        CoolingCoilInletNodeNum = &
               GetOnlySingleNode(NodeID(SupplyAirOutletNode),ErrorsFound, &
                            TRIM(CurrentModuleObject),HXAssistedCoil(HXAssistedCoilNum)%Name, &
                            NodeType_Air,NodeConnectionType_Internal,1,ObjectIsParent)
        HXAssistedCoil(HXAssistedCoilNum)%HXExhaustAirInletNodeNum = &
               GetOnlySingleNode(NodeID(SecondaryAirInletNode),ErrorsFound, &
                            TRIM(CurrentModuleObject),HXAssistedCoil(HXAssistedCoilNum)%Name, &
                            NodeType_Air,NodeConnectionType_Internal,1,ObjectIsParent)
        HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilOutletNodeNum = &
               GetOnlySingleNode(NodeID(SecondaryAirOutletNode),ErrorsFound, &
                           TRIM(CurrentModuleObject),HXAssistedCoil(HXAssistedCoilNum)%Name, &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)

        ! Add cooling coil to component sets array
        CALL SetUpCompSets(HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilType, HXAssistedCoil(HXAssistedCoilNum)%Name, &
                   HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType,HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilName, &
                   NodeID(SupplyAirOutletNode),NodeID(SecondaryAirInletNode),'Air Nodes')
        ! Add heat exchanger to component sets array
        CALL SetUpCompSets(HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilType, HXAssistedCoil(HXAssistedCoilNum)%Name, &
                   HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType,HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerName, &
                   NodeID(SupplyAirInletNode),NodeID(SupplyAirOutletNode),'Process Air Nodes')
        CALL SetUpCompSets(HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilType, HXAssistedCoil(HXAssistedCoilNum)%Name, &
                   HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerType,HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerName, &
                   NodeID(SecondaryAirInletNode),NodeID(SecondaryAirOutletNode),'Secondary Air Nodes')

       END DO  !End of the Coil:Water:CoolingHXAssisted Loop

      DEALLOCATE(AlphArray)
      DEALLOCATE(cAlphaFields)
      DEALLOCATE(cNumericFields)
      DEALLOCATE(NumArray)
      DEALLOCATE(lAlphaBlanks)
      DEALLOCATE(lNumericBlanks)

      IF (ErrorsFound) THEN
        CALL ShowFatalError(RoutineName//'Previous error condition causes termination.')
      ENDIF

  RETURN

END SUBROUTINE GetHXAssistedCoolingCoilInput


! End of Get Input subroutines for this Module
!******************************************************************************


! Beginning Initialization Section of the Module
!******************************************************************************

SUBROUTINE InitHXAssistedCoolingCoil(HXAssistedCoilNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   Sep 2003
          !       MODIFIED       R. Raustad, June 2007 now using FullLoadOutletConditions from DX Coil data structure
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  This subroutine is for initializations of the HXAssistedCoolingCoil components

          ! METHODOLOGY EMPLOYED:
          !  Uses the status flags to trigger initializations.

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DXCoils, ONLY: DXCoilFullLoadOutAirTemp, DXCoilFullLoadOutAirHumRat

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: HXAssistedCoilNum  ! index for HXAssistedCoolingCoil

          ! SUBROUTINE PARAMETER DEFINITIONS:
          !  na

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          !  na

! Do these initializations every time
  HXAssistedCoil(HXAssistedCoilNum)%MassFlowRate = &
    Node(HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilInletNodeNum)%MassFlowRate

  IF(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType_Num == CoilDX_CoolingSingleSpeed)THEN
    DXCoilFullLoadOutAirTemp(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilIndex)   = 0.0d0
    DXCoilFullLoadOutAirHumRat(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilIndex) = 0.0d0
  END IF

 RETURN

END SUBROUTINE InitHXAssistedCoolingCoil

! End Initialization Section of the Module
!******************************************************************************


SUBROUTINE CalcHXAssistedCoolingCoil(HXAssistedCoilNum,FirstHVACIteration,CompOp,PartLoadRatio, &
                                     HXUnitOn, FanOpMode, OnOffAirFlow, EconomizerFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   Sept 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  This subroutine models the cooling coil/air-to-air heat exchanger
          !  combination. The cooling coil exiting air temperature is used as
          !  an indicator of convergence.

          ! METHODOLOGY EMPLOYED:
          !  na

          ! REFERENCES:
          !  na

          ! USE STATEMENTS:
  USE HeatRecovery,    ONLY: SimHeatRecovery
  USE DXCoils,         ONLY: SimDXCoil
  USE WaterCoils,      ONLY: SimulateWaterCoilComponents
  USE Psychrometrics,  ONlY: PsyHFnTdbW
  USE General,         ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)    :: HXAssistedCoilNum  ! Index number for HXAssistedCoolingCoil
  LOGICAL,   INTENT(IN)    :: FirstHVACIteration ! FirstHVACIteration flag
  INTEGER,   INTENT(IN)    :: CompOp             ! compressor operation; 1=on, 0=off
  REAL(r64), INTENT(IN)    :: PartLoadRatio      ! Cooling coil part load ratio
                                                 ! (used only for Coil:DX:CoolingBypassFactorEmpirical)
  LOGICAL,   INTENT(IN)    :: HXUnitOn           ! Flag to enable heat exchanger
  INTEGER,   INTENT(IN)    :: FanOpMode          ! Allows parent object to control fan operation
  REAL(r64), OPTIONAL, INTENT(IN) :: OnOffAirFlow  ! Ratio of compressor ON air mass flow to AVERAGE over time step
  LOGICAL,   OPTIONAL, INTENT(IN) :: EconomizerFlag ! OA (or airloop) econommizer status

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER     :: MaxIter = 50       ! Maximum number of iterations

          ! INTERFACE BLOCK SPECIFICATIONS
          !  na

          ! DERIVED TYPE DEFINITIONS
          !  na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64), SAVE  :: CoilOutputTempLast         ! Exiting cooling coil temperature from last iteration
  REAL(r64)        :: AirMassFlow                ! Inlet air mass flow rate
  REAL(r64)        :: Error                      ! Error (exiting coil temp from last iteration minus current coil exiting temp)
  INTEGER          :: Iter                       ! Number of iterations
  INTEGER          :: CompanionCoilIndexNum      ! Index to DX coil

  AirMassFlow = HXAssistedCoil(HXAssistedCoilNum)%MassFlowRate
  Error = 1.0d0 ! Initialize error (CoilOutputTemp last iteration minus current CoilOutputTemp)
  Iter = 0 ! Initialize iteration counter to zero

  IF(FirstHVACIteration) CoilOutputTempLast = -99.0d0 ! Initialize coil output temp

! Set mass flow rate at inlet of exhaust side of heat exchanger to supply side air mass flow rate entering this compound object
  Node(HXAssistedCoil(HXAssistedCoilNum)%HXExhaustAirInletNodeNum)%MassFlowRate = AirMassFlow

  IF(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType_Num == CoilDX_CoolingSingleSpeed)THEN
    CompanionCoilIndexNum = HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilIndex
  ELSE
    CompanionCoilIndexNum = 0
  END IF

! First call to RegulaFalsi uses PLR=0. Nodes are typically setup at full output on this call.
! A large number of iterations are required to get to result (~36 iterations to get to PLR=0 node conditions).
! Reset node data to minimize iteration. This initialization reduces the number of iterations by 50%.
! CAUTION: Do not use Node(x) = Node(y) here, this can overwrite the coil outlet node setpoint.
  IF(PartLoadRatio .EQ. 0.0d0) THEN
    Node(HXAssistedCoil(HXAssistedCoilNum)%HXExhaustAirInletNodeNum)%Temp = &
        Node(HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilInletNodeNum)%Temp
    Node(HXAssistedCoil(HXAssistedCoilNum)%HXExhaustAirInletNodeNum)%HumRat = &
        Node(HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilInletNodeNum)%HumRat
    Node(HXAssistedCoil(HXAssistedCoilNum)%HXExhaustAirInletNodeNum)%Enthalpy = &
        Node(HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilInletNodeNum)%Enthalpy
    Node(HXAssistedCoil(HXAssistedCoilNum)%HXExhaustAirInletNodeNum)%MassFlowRate = &
        Node(HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilInletNodeNum)%MassFlowRate
  END IF

! Force at least 2 iterations to pass outlet node information
  DO WHILE ((ABS(Error) .GT. 0.0005d0 .AND. Iter .LE. MaxIter) .OR. Iter .LT. 2)

    Call SimHeatRecovery(HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerName,FirstHVACIteration,  &
                         HXAssistedCoil(HXAssistedCoilNum)%HeatExchangerIndex, FanOpMode, &
                         HXPartLoadRatio = PartLoadRatio, HXUnitEnable=HXUnitOn, &
                         CompanionCoilIndex=CompanionCoilIndexNum,EconomizerFlag=EconomizerFlag)

    IF(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilType_Num == CoilDX_CoolingSingleSpeed) THEN
      CALL SimDXCoil(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilName,CompOp,FirstHVACIteration,PartLoadRatio,  &
                                    HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilIndex, FanOpMode, &
                                    OnOffAFR = OnOffAirFlow)
    ELSE
      CALL SimulateWaterCoilComponents(HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilName,FirstHVACIteration,  &
                                       HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilIndex)
    END IF

    Error = CoilOutputTempLast - Node(HXAssistedCoil(HXAssistedCoilNum)%HXExhaustAirInletNodeNum)%Temp
    CoilOutputTempLast = Node(HXAssistedCoil(HXAssistedCoilNum)%HXExhaustAirInletNodeNum)%Temp
    Iter = Iter + 1

  END DO

! Write excessive iteration warning messages
  IF (Iter .GT. MaxIter) THEN
    IF(HXAssistedCoil(HXAssistedCoilNum)%MaxIterCounter .LT. 1)THEN
      HXAssistedCoil(HXAssistedCoilNum)%MaxIterCounter = HXAssistedCoil(HXAssistedCoilNum)%MaxIterCounter + 1
      CALL ShowWarningError(TRIM(HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilType)//' "'&
           //TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name)//'" -- Exceeded max iterations ('//TRIM(TrimSigDigits(MaxIter,0))//  &
            ') while calculating operating conditions.')
      CALL ShowContinueErrorTimeStamp(' ')
    ELSE
      CALL ShowRecurringWarningErrorAtEnd(TRIM(HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilType)//' "'//  &
                            TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name)//&
                            '" -- Exceeded max iterations error continues...',HXAssistedCoil(HXAssistedCoilNum)%MaxIterIndex)
    ENDIF
  END IF

  HXAssistedCoilOutletTemp(HXAssistedCoilNum) = Node(HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilOutletNodeNum)%Temp
  HXAssistedCoilOutletHumRat(HXAssistedCoilNum) = Node(HXAssistedCoil(HXAssistedCoilNum)%HXAssistedCoilOutletNodeNum)%HumRat

  RETURN

END SUBROUTINE CalcHXAssistedCoolingCoil

!        End of Reporting subroutines for the HXAssistedCoil Module
! *****************************************************************************

SUBROUTINE GetHXDXCoilIndex(HXDXCoilName,HXDXCoilIndex,ErrorsFound,CurrentModuleObject)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   August 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets an index for a given HX Assisted Cooling Coil -- issues error message if that
          ! HX is not a legal HX Assisted Cooling Coil.


          ! METHODOLOGY EMPLOYED:
          ! na


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItem
  USE DataInterfaces,    ONLY: ShowSevereError


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: HXDXCoilName
  INTEGER, INTENT(INOUT)       :: HXDXCoilIndex
  LOGICAL, INTENT(INOUT)       :: ErrorsFound
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: CurrentModuleObject


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  ! Obtains and allocates HXAssistedCoolingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  ! First time subroutine has been called, get input data
    ! Get the HXAssistedCoolingCoil input
    CALL GetHXAssistedCoolingCoilInput
    GetCoilsInputFlag=.false. ! Set logic flag to disallow getting the input data on future calls to this subroutine
  End If

  IF(TotalNumHXAssistedCoils .GT. 0)THEN
    HXDXCoilIndex = FindItem(HXDXCoilName,HXAssistedCoil%Name,TotalNumHXAssistedCoils)
  ELSE
    HXDXCoilIndex = 0
  END IF

  IF (HXDXCoilIndex == 0) THEN
    IF (PRESENT(CurrentModuleObject)) THEN
      CALL ShowSevereError(TRIM(CurrentModuleObject)//', GetHXDXCoilIndex: HX Assisted Cooling Coil not found='//TRIM(HXDXCoilName))
    ELSE
      CALL ShowSevereError('GetHXDXCoilIndex: HX Assisted Cooling Coil not found='//TRIM(HXDXCoilName))
    ENDIF
    ErrorsFound = .TRUE.
  ENDIF


  RETURN


END SUBROUTINE GetHXDXCoilIndex

SUBROUTINE CheckHXAssistedCoolingCoilSchedule(CompType,CompName,Value,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine provides a method for outside routines to check if
          ! the hx assisted cooling coil is scheduled to be on.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItem
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CompType !unused1208
  CHARACTER(len=*), INTENT(IN) :: CompName
  REAL(r64), INTENT(OUT)            :: Value
  INTEGER, INTENT(INOUT)       :: CompIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER HXAssistedCoilNum

  ! Obtains and allocates HXAssistedCoolingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  ! First time subroutine has been called, get input data
    ! Get the HXAssistedCoolingCoil input
    CALL GetHXAssistedCoolingCoilInput
    GetCoilsInputFlag=.false. ! Set logic flag to disallow getting the input data on future calls to this subroutine
  End If

  ! Find the correct Coil number
  IF (CompIndex == 0) THEN
    IF(TotalNumHXAssistedCoils .GT. 0)THEN
      HXAssistedCoilNum = FindItem(CompName,HXAssistedCoil%Name,TotalNumHXAssistedCoils)
    ELSE
      HXAssistedCoilNum = 0
    END IF

    IF (HXAssistedCoilNum == 0) THEN
      CALL ShowFatalError('CheckHXAssistedCoolingCoilSchedule: HX Assisted Coil not found='//TRIM(CompName))
    ENDIF
    CompIndex=HXAssistedCoilNum
    Value=1.0d0  ! not scheduled?
  ELSE
    HXAssistedCoilNum=CompIndex
    IF (HXAssistedCoilNum > TotalNumHXAssistedCoils .or. HXAssistedCoilNum < 1) THEN
      CALL ShowFatalError('CheckHXAssistedCoolingCoilSchedule: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(HXAssistedCoilNum))// &
                          ', Number of Heating Coils='//TRIM(TrimSigDigits(TotalNumHXAssistedCoils))//  &
                          ', Coil name='//TRIM(CompName))
    ENDIF
    IF (CompName /= HXAssistedCoil(HXAssistedCoilNum)%Name) THEN
      CALL ShowFatalError('CheckHXAssistedCoolingCoilSchedule: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(HXAssistedCoilNum))// &
                          ', Coil name='//TRIM(CompName)//', stored Coil Name for that index='//  &
                          TRIM(HXAssistedCoil(HXAssistedCoilNum)%Name))
    ENDIF

    Value=1.0d0  ! not scheduled?
  ENDIF

  RETURN

END SUBROUTINE CheckHXAssistedCoolingCoilSchedule

FUNCTION GetCoilCapacity(CoilType,CoilName,ErrorsFound) RESULT(CoilCapacity)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the coil capacity for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and capacity is returned
          ! as negative.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem,SameString
  USE DXCoils, ONLY: GetDXCoilCapacity => GetCoilCapacity
  USE WaterCoils, ONLY: GetWaterCoilCapacity

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  REAL(r64)                    :: CoilCapacity ! returned capacity of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil
  INTEGER, SAVE :: ErrCount=0
  LOGICAL :: ErrFlag

  ! Obtains and allocates HXAssistedCoolingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  ! First time subroutine has been called, get input data
    ! Get the HXAssistedCoolingCoil input
    CALL GetHXAssistedCoolingCoilInput
    GetCoilsInputFlag=.false. ! Set logic flag to disallow getting the input data on future calls to this subroutine
  End If

  ErrFlag = .FALSE.

  IF(TotalNumHXAssistedCoils .GT. 0)THEN
    WhichCoil=FindItem(CoilName,HXAssistedCoil%Name,TotalNumHXAssistedCoils)
  ELSE
    WhichCoil=0
  END IF

  IF (SameString(CoilType,'CoilSystem:Cooling:DX:HeatExchangerAssisted')) THEN
    IF (WhichCoil /= 0) THEN
       ! coil does not have capacity in input so mine information from DX cooling coil
      CoilCapacity=GetDXCoilCapacity(HXAssistedCoil(WhichCoil)%CoolingCoilType,HXAssistedCoil(WhichCoil)%CoolingCoilName,ErrFlag)
      IF(ErrFlag)THEN
        CALL ShowRecurringWarningErrorAtEnd('Requested DX Coil from CoilSystem:Cooling:DX:HeatExchangerAssisted not found',ErrCount)
      END IF
    ENDIF
  ELSE IF (SameString(CoilType,'CoilSystem:Cooling:Water:HeatExchangerAssisted')) THEN
    IF (WhichCoil /= 0) THEN
       ! coil does not have capacity in input so mine information from DX cooling coil
      CoilCapacity=GetWaterCoilCapacity(HXAssistedCoil(WhichCoil)%CoolingCoilType,HXAssistedCoil(WhichCoil)%CoolingCoilName,ErrFlag)
      IF(ErrFlag)THEN
        CALL ShowRecurringWarningErrorAtEnd('Requested DX Coil from CoilSystem:Cooling:DX:HeatExchangerAssisted not found',ErrCount)
      END IF
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('GetCoilCapacity: Could not find Coil, Type="'//TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    CALL ShowContinueError('... Coil Capacity returned as -1000.')
    ErrorsFound=.true.
    CoilCapacity=-1000.0d0
  ENDIF

  IF(ErrFlag)ErrorsFound=.true.

  RETURN

END FUNCTION GetCoilCapacity

FUNCTION GetCoilGroupTypeNum(CoilType,CoilName,ErrorsFound,PrintWarning) RESULT(TypeNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad - FSEC
          !       DATE WRITTEN   August 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the HX coil type and returns it (CoilDX_CoolingHXAssisted, CoilWater_CoolingHXAssisted)
          ! If incorrect coil type or name is given, errorsfound is returned as true.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  LOGICAL, OPTIONAL, INTENT(IN):: PrintWarning ! prints warning message if true
  INTEGER                      :: TypeNum      ! returned integerized type of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil
  LOGICAL :: PrintMessage



  ! Obtains and allocates HXAssistedCoolingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  ! First time subroutine has been called, get input data
    ! Get the HXAssistedCoolingCoil input
    CALL GetHXAssistedCoolingCoilInput
    GetCoilsInputFlag=.false. ! Set logic flag to disallow getting the input data on future calls to this subroutine
  End If

  IF(PRESENT(PrintWarning))THEN
    PrintMessage = PrintWarning
  ELSE
    PrintMessage = .TRUE.
  END IF

  IF(TotalNumHXAssistedCoils .GT. 0)THEN
    WhichCoil=FindItem(CoilName,HXAssistedCoil%Name,TotalNumHXAssistedCoils)
  ELSE
    WhichCoil=0
  END IF

  IF (WhichCoil /= 0) THEN
       ! coil does not have capacity in input so mine information from DX cooling coil
    TypeNum=HXAssistedCoil(WhichCoil)%HXAssistedCoilType_Num
  ELSE
    IF(PrintMessage)THEN
      CALL ShowSevereError('GetCoilGroupTypeNum: Could not find Coil, Type="'//TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    END IF
    ErrorsFound=.true.
    TypeNum=0
  ENDIF

  RETURN

END FUNCTION GetCoilGroupTypeNum

FUNCTION GetCoilObjectTypeNum(CoilType,CoilName,ErrorsFound,PrintWarning) RESULT(TypeNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad - FSEC
          !       DATE WRITTEN   April 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the coil object type for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and capacity is returned
          ! as negative.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  LOGICAL, OPTIONAL, INTENT(IN):: PrintWarning ! prints warning message if true
  INTEGER                      :: TypeNum      ! returned integerized type of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil
  LOGICAL :: PrintMessage



  ! Obtains and allocates HXAssistedCoolingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  ! First time subroutine has been called, get input data
    ! Get the HXAssistedCoolingCoil input
    CALL GetHXAssistedCoolingCoilInput
    GetCoilsInputFlag=.false. ! Set logic flag to disallow getting the input data on future calls to this subroutine
  End If

  IF(PRESENT(PrintWarning))THEN
    PrintMessage = PrintWarning
  ELSE
    PrintMessage = .TRUE.
  END IF

  IF(TotalNumHXAssistedCoils .GT. 0)THEN
    WhichCoil=FindItem(CoilName,HXAssistedCoil%Name,TotalNumHXAssistedCoils)
  ELSE
    WhichCoil=0
  END IF

  IF (WhichCoil /= 0) THEN
    TypeNum=HXAssistedCoil(WhichCoil)%CoolingCoilType_Num
  ELSE
    IF(PrintMessage)THEN
      CALL ShowSevereError('GetCoilObjectTypeNum: Could not find Coil, Type="'//TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    END IF
    ErrorsFound=.true.
    TypeNum=0
  ENDIF

  RETURN

END FUNCTION GetCoilObjectTypeNum

FUNCTION GetCoilInletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the inlet node number.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and node number is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned node number of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and allocates HXAssistedCoolingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  ! First time subroutine has been called, get input data
    ! Get the HXAssistedCoolingCoil input
    CALL GetHXAssistedCoolingCoilInput
    GetCoilsInputFlag=.false. ! Set logic flag to disallow getting the input data on future calls to this subroutine
  End If

  IF(TotalNumHXAssistedCoils .GT. 0)THEN
    WhichCoil=FindItem(CoilName,HXAssistedCoil%Name,TotalNumHXAssistedCoils)
  ELSE
    WhichCoil=0
  END IF

  IF (WhichCoil /= 0) THEN
    NodeNumber=HXAssistedCoil(WhichCoil)%HXAssistedCoilInletNodeNum
  ELSE
    CALL ShowSevereError('GetCoilInletNode: Could not find Coil, Type="'//TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    NodeNumber=0
  ENDIF

  RETURN

END FUNCTION GetCoilInletNode

FUNCTION GetCoilWaterInletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   April 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the inlet node number.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and node number is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem
  USE WaterCoils,      ONLY: GetWaterCoilWaterInletNode=>GetCoilWaterInletNode

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned node number of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and allocates HXAssistedCoolingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  ! First time subroutine has been called, get input data
    ! Get the HXAssistedCoolingCoil input
    CALL GetHXAssistedCoolingCoilInput
    GetCoilsInputFlag=.false. ! Set logic flag to disallow getting the input data on future calls to this subroutine
  End If

  IF(TotalNumHXAssistedCoils .GT. 0)THEN
    WhichCoil=FindItem(CoilName,HXAssistedCoil%Name,TotalNumHXAssistedCoils)
  ELSE
    WhichCoil=0
  END IF

  IF (WhichCoil /= 0) THEN
    IF (HXAssistedCoil(WhichCoil)%CoolingCoilType_Num == Coil_CoolingWater) THEN
      NodeNumber=GetWaterCoilWaterInletNode(HXAssistedCoil(WhichCoil)%CoolingCoilType,  &
         HXAssistedCoil(WhichCoil)%CoolingCoilName,ErrorsFound)
    ELSEIF (HXAssistedCoil(WhichCoil)%CoolingCoilType_Num == Coil_CoolingWaterDetailed) THEN
      NodeNumber=GetWaterCoilWaterInletNode(HXAssistedCoil(WhichCoil)%CoolingCoilType,  &
         HXAssistedCoil(WhichCoil)%CoolingCoilName,ErrorsFound)
    ELSE  ! even though validated in Get, still check.
      CALL ShowSevereError('GetCoilWaterInletNode: Invalid Cooling Coil for HX Assisted Coil, Type="'//  &
         TRIM(HXAssistedCoil(WhichCoil)%CoolingCoilType)//'" Name="'//TRIM(CoilName)//'"')
      ErrorsFound=.true.
      NodeNumber=0 !Objexx:Return Added line to set return value
    ENDIF
  ELSE
    CALL ShowSevereError('GetCoilInletNode: Could not find Coil, Type="'//TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    NodeNumber=0
  ENDIF

  RETURN

END FUNCTION GetCoilWaterInletNode

FUNCTION GetCoilOutletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         R. Raustad
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the outlet node number.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and node number is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned node number of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and allocates HXAssistedCoolingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  ! First time subroutine has been called, get input data
    ! Get the HXAssistedCoolingCoil input
    CALL GetHXAssistedCoolingCoilInput
    GetCoilsInputFlag=.false. ! Set logic flag to disallow getting the input data on future calls to this subroutine
  End If

  IF(TotalNumHXAssistedCoils .GT. 0)THEN
    WhichCoil=FindItem(CoilName,HXAssistedCoil%Name,TotalNumHXAssistedCoils)
  ELSE
    WhichCoil=0
  END IF

  IF (WhichCoil /= 0) THEN
    NodeNumber=HXAssistedCoil(WhichCoil)%HXAssistedCoilOutletNodeNum
  ELSE
    CALL ShowSevereError('GetCoilOutletNode: Could not find Coil, Type="'//TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    NodeNumber=0
  ENDIF

  RETURN

END FUNCTION GetCoilOutletNode

FUNCTION GetHXDXCoilName(CoilType,CoilName,ErrorsFound) RESULT(DXCoilName)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the cooling coil name.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and the name
          ! is returned as blank

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  CHARACTER(len=MaxNameLength) :: DXCoilName   ! returned name of cooling coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and allocates HXAssistedCoolingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  ! First time subroutine has been called, get input data
    ! Get the HXAssistedCoolingCoil input
    CALL GetHXAssistedCoolingCoilInput
    GetCoilsInputFlag=.false. ! Set logic flag to disallow getting the input data on future calls to this subroutine
  End If

!  HXAssistedCoil(HXAssistedCoilNum)%CoolingCoilName            = AlphArray(7)
  IF(TotalNumHXAssistedCoils .GT. 0)THEN
    WhichCoil=FindItem(CoilName,HXAssistedCoil%Name,TotalNumHXAssistedCoils)
  ELSE
    WhichCoil=0
  END IF

  IF (WhichCoil /= 0) THEN
    DXCoilName=HXAssistedCoil(WhichCoil)%CoolingCoilName
  ELSE
    CALL ShowSevereError('Could not find Coil, Type="'//TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    DXCoilName = ' '
  ENDIF

  RETURN

END FUNCTION GetHXDXCoilName

FUNCTION GetActualDXCoilIndex(CoilType,CoilName,ErrorsFound) RESULT(DXCoilIndex)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the cooling coil name.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and the name
          ! is returned as blank

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: DXCoilIndex  ! returned index of DX cooling coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and allocates HXAssistedCoolingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  ! First time subroutine has been called, get input data
    ! Get the HXAssistedCoolingCoil input
    CALL GetHXAssistedCoolingCoilInput
    GetCoilsInputFlag=.false. ! Set logic flag to disallow getting the input data on future calls to this subroutine
  End If

  IF(TotalNumHXAssistedCoils .GT. 0)THEN
    WhichCoil=FindItem(CoilName,HXAssistedCoil%Name,TotalNumHXAssistedCoils)
  ELSE
    WhichCoil=0
  END IF

  IF (WhichCoil /= 0) THEN
    ! this should be the index to the DX cooling coil object, not the HXAssisted object
    DXCoilIndex=HXAssistedCoil(WhichCoil)%CoolingCoilIndex
  ELSE
    CALL ShowSevereError('Could not find Coil, Type="'//TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    DXCoilIndex = 0
  ENDIF

  RETURN

END FUNCTION GetActualDXCoilIndex

FUNCTION GetHXCoilType(CoilType,CoilName,ErrorsFound) RESULT(CoolingCoilType)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   June 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the cooling coil type.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and the cooling
          ! coil type is returned as blank.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType         ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName         ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound      ! set to true if problem
  CHARACTER(len=MaxNameLength) :: CoolingCoilType  ! returned type of cooling coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and allocates HXAssistedCoolingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  ! First time subroutine has been called, get input data
    ! Get the HXAssistedCoolingCoil input
    CALL GetHXAssistedCoolingCoilInput
    GetCoilsInputFlag=.false. ! Set logic flag to disallow getting the input data on future calls to this subroutine
  End If

  IF(TotalNumHXAssistedCoils .GT. 0)THEN
    WhichCoil=FindItem(CoilName,HXAssistedCoil%Name,TotalNumHXAssistedCoils)
  ELSE
    WhichCoil=0
  END IF

  IF (WhichCoil /= 0) THEN
    CoolingCoilType=HXAssistedCoil(WhichCoil)%CoolingCoilType
  ELSE
    CALL ShowSevereError('Could not find Coil, Type="'//TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    CoolingCoilType = ' '
  ENDIF

  RETURN

END FUNCTION GetHXCoilType

SUBROUTINE GetHXCoilTypeAndName(CoilType,CoilName,ErrorsFound,CoolingCoilType,CoolingCoilName)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   Oct 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Need to get child coil type and name.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType         ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName         ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound      ! set to true if problem
  CHARACTER(len=MaxNameLength) :: CoolingCoilType  ! returned type of cooling coil
  CHARACTER(len=MaxNameLength) :: CoolingCoilName  ! returned name of cooling coil

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and allocates HXAssistedCoolingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  ! First time subroutine has been called, get input data
    ! Get the HXAssistedCoolingCoil input
    CALL GetHXAssistedCoolingCoilInput
    GetCoilsInputFlag=.false. ! Set logic flag to disallow getting the input data on future calls to this subroutine
  End If

  IF(TotalNumHXAssistedCoils .GT. 0)THEN
    WhichCoil=FindItem(CoilName,HXAssistedCoil%Name,TotalNumHXAssistedCoils)
  ELSE
    WhichCoil=0
  END IF

  IF (WhichCoil /= 0) THEN
    CoolingCoilType=HXAssistedCoil(WhichCoil)%CoolingCoilType
    CoolingCoilName=HXAssistedCoil(WhichCoil)%CoolingCoilName
  ELSE
    CALL ShowSevereError('Could not find Coil, Type="'//TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    CoolingCoilType = ' '
    CoolingCoilName = ' '
  ENDIF

  RETURN

END SUBROUTINE GetHXCoilTypeAndName

FUNCTION GetCoilMaxWaterFlowRate(CoilType,CoilName,ErrorsFound) RESULT(MaxWaterFlowRate)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   November 2006
          !       MODIFIED       R. Raustad, April 2009 - added water coil ELSE IF
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the max water flow rate for the given coil and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and capacity is returned
          ! as negative.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem,SameString
  USE WaterCoils,      ONLY: GetWaterCoilMaxFlowRate=>GetCoilMaxWaterFlowRate

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  REAL(r64)                    :: MaxWaterFlowRate  ! returned max water flow rate of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil
  INTEGER, SAVE :: ErrCount=0

  ! Obtains and allocates HXAssistedCoolingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  ! First time subroutine has been called, get input data
    ! Get the HXAssistedCoolingCoil input
    CALL GetHXAssistedCoolingCoilInput
    GetCoilsInputFlag=.false. ! Set logic flag to disallow getting the input data on future calls to this subroutine
  End If

  IF(TotalNumHXAssistedCoils .GT. 0)THEN

    WhichCoil=FindItem(CoilName,HXAssistedCoil%Name,TotalNumHXAssistedCoils)

    IF (SameString(CoilType,'CoilSystem:Cooling:DX:HeatExchangerAssisted')) THEN
      IF (WhichCoil /= 0) THEN
        ! coil does not specify MaxWaterFlowRate
        MaxWaterFlowRate=0.0d0
        CALL ShowRecurringWarningErrorAtEnd('Requested Max Water Flow Rate from CoilSystem:Cooling:DX:HeatExchangerAssisted N/A',  &
             ErrCount)
      ENDIF
    ELSE IF (SameString(CoilType,'CoilSystem:Cooling:Water:HeatExchangerAssisted')) THEN
      IF (WhichCoil /= 0) THEN
        MaxWaterFlowRate=GetWaterCoilMaxFlowRate(cAllCoilTypes(GetCoilObjectTypeNum(CoilType,CoilName,ErrorsFound)),  &
                         GetHXDXCoilName(CoilType,CoilName,ErrorsFound),ErrorsFound)
      ENDIF
    ELSE
      WhichCoil=0
    ENDIF

    IF (WhichCoil == 0) THEN
      CALL ShowSevereError('GetCoilMaxWaterFlowRate: Could not find Coil, Type="'//TRIM(CoilType)//  &
                           '" Name="'//TRIM(CoilName)//'"')
      ErrorsFound=.true.
      MaxWaterFlowRate=-1000.d0
    ENDIF
  ELSE
    CALL ShowSevereError('GetCoilMaxWaterFlowRate: Could not find Coil, Type="'//TRIM(CoilType)//  &
                         '" Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    MaxWaterFlowRate=-1000.d0
  END IF

  RETURN

END FUNCTION GetCoilMaxWaterFlowRate

FUNCTION GetHXCoilAirFlowRate(CoilType,CoilName,ErrorsFound) RESULT(MaxAirFlowRate)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   September 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the max air flow rate for the given HX and returns it.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and capacity is returned
          ! as negative.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem,SameString
  USE HeatRecovery,    ONLY: GetSupplyAirFlowRate

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  REAL(r64)                    :: MaxAirFlowRate  ! returned max air flow rate of matched HX

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil
  INTEGER, SAVE :: ErrCount=0

  ! Obtains and allocates HXAssistedCoolingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  ! First time subroutine has been called, get input data
    ! Get the HXAssistedCoolingCoil input
    CALL GetHXAssistedCoolingCoilInput
    GetCoilsInputFlag=.false. ! Set logic flag to disallow getting the input data on future calls to this subroutine
  End If

  IF(TotalNumHXAssistedCoils .GT. 0)THEN

    WhichCoil=FindItem(CoilName,HXAssistedCoil%Name,TotalNumHXAssistedCoils)

    IF (SameString(CoilType,'CoilSystem:Cooling:DX:HeatExchangerAssisted') .OR. &
        SameString(CoilType,'CoilSystem:Cooling:Water:HeatExchangerAssisted')) THEN
      IF (WhichCoil /= 0) THEN
        MaxAirFlowRate=GetSupplyAirFlowRate(HXAssistedCoil(WhichCoil)%HeatExchangerName,ErrorsFound)
      ENDIF
    ELSE
      WhichCoil=0
    ENDIF

    IF (WhichCoil == 0) THEN
      CALL ShowSevereError('GetHXCoilAirFlowRate: Could not find HX, Type="'//TRIM(CoilType)//  &
                           '" Name="'//TRIM(CoilName)//'"')
      ErrorsFound=.true.
      MaxAirFlowRate=-1000.d0
    ENDIF
  ELSE
    CALL ShowSevereError('GetHXCoilAirFlowRate: Could not find HX, Type="'//TRIM(CoilType)//  &
                         '" Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    MaxAirFlowRate=-1000.d0
  END IF

  RETURN

END FUNCTION GetHXCoilAirFlowRate

FUNCTION VerifyHeatExchangerParent(HXType,HXName) RESULT(Found)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   January 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given heat exchanger name and type and returns true or false.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItem, SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: HXType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: HXName     ! must match coil names for the coil type
  LOGICAL  :: Found  ! set to true if found

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and allocates HXAssistedCoolingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  ! First time subroutine has been called, get input data
    ! Get the HXAssistedCoolingCoil input
    CALL GetHXAssistedCoolingCoilInput
    GetCoilsInputFlag=.false. ! Set logic flag to disallow getting the input data on future calls to this subroutine
  End If

  Found = .FALSE.

  IF(TotalNumHXAssistedCoils .GT. 0)THEN
    WhichCoil=FindItem(HXName,HXAssistedCoil%HeatExchangerName,TotalNumHXAssistedCoils)
  ELSE
    WhichCoil = 0
  END IF

  IF (WhichCoil /= 0) THEN
    If (SameString(HXAssistedCoil(WhichCoil)%HeatExchangerType,HXType)) Then
      Found = .TRUE.
    End If
  ENDIF

  RETURN

END FUNCTION VerifyHeatExchangerParent

!        End of Utility subroutines for the HXAssistedCoil Module
! *****************************************************************************

!     NOTICE

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

End Module HVACHXAssistedCoolingCoil

