Module HeatingCoils
  ! Module containing the HeatingCoil simulation routines other than the Water coils

  ! MODULE INFORMATION:
  !       AUTHOR         Richard J. Liesen
  !       DATE WRITTEN   May 2000
  !       MODIFIED       Therese Stovall June 2008 to add references to refrigation condensers
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required to
  ! manage the HeatingCoil System Component

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
USE DataInterfaces
USE DataHVACGlobals
Use DataEnvironment,   ONLY: StdRhoAir
USE DataHeatBalance,   ONLY: NumRefrigeratedRacks, HeatReclaimRefrigeratedRack, &
                             HeatReclaimRefrigCondenser, HeatReclaimDXCoil, NumRefrigCondensers, &
                             RefrigSystemTypeDetailed , RefrigSystemTypeRack, &
                             RefrigCondenserTypeAir, RefrigCondenserTypeEvap, RefrigCondenserTypeWater
USE Psychrometrics,    ONLY: PsyCpAirFnWTdb,PsyHFnTdbW,PsyRhoAirFnPbTdbW

  ! Use statements for access to subroutines in other modules
USE ScheduleManager
USE DXCoils,           ONLY: NumDXCoils, GetDXCoilIndex
USE RefrigeratedCase,  ONLY: GetRefrigeratedRackIndex

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS
REAL(r64), PARAMETER :: MinAirMassFlow                     = 0.001d0
INTEGER :: NumDesuperheaterCoil  ! Total number of desuperheater heating coil objects in input

! reclaim heat object types
INTEGER, PARAMETER :: COMPRESSORRACK_REFRIGERATEDCASE = 1
INTEGER, PARAMETER :: COIL_DX_COOLING                 = 2
INTEGER, PARAMETER :: COIL_DX_MULTISPEED              = 3
INTEGER, PARAMETER :: COIL_DX_MULTIMODE               = 4
INTEGER, PARAMETER :: CONDENSER_REFRIGERATION         = 5

  ! DERIVED TYPE DEFINITIONS
TYPE, PUBLIC                   :: HeatingCoilEquipConditions
  CHARACTER(len=MaxNameLength) :: Name             = ' ' ! Name of the HeatingCoil
  CHARACTER(len=MaxNameLength) :: HeatingCoilType  = ' ' ! Type of HeatingCoil ie. Heating or Cooling
  CHARACTER(len=MaxNameLength) :: HeatingCoilModel = ' ' ! Type of HeatingCoil ie. Simple, Detailed, etc.
  INTEGER                      :: HCoilType_Num    = 0
  CHARACTER(len=MaxNameLength) :: Schedule         = ' ' ! HeatingCoil Operation Schedule
  Integer      :: SchedPtr                         = 0   ! Pointer to the correct schedule
  Integer      :: InsuffTemperatureWarn            = 0   ! Used for recurring error message
  REAL(r64)    :: InletAirMassFlowRate             = 0.0d0 ! MassFlow through the HeatingCoil being Simulated [kg/Sec]
  REAL(r64)    :: OutletAirMassFlowRate            = 0.0d0 !
  REAL(r64)    :: InletAirTemp                     = 0.0d0 !
  REAL(r64)    :: OutletAirTemp                    = 0.0d0 !
  REAL(r64)    :: InletAirHumRat                   = 0.0d0 !
  REAL(r64)    :: OutletAirHumRat                  = 0.0d0 !
  REAL(r64)    :: InletAirEnthalpy                 = 0.0d0 !
  REAL(r64)    :: OutletAirEnthalpy                = 0.0d0 !
  REAL(r64)    :: HeatingCoilLoad                  = 0.0d0 ! Total Load on the Coil [J]
  REAL(r64)    :: HeatingCoilRate                  = 0.0d0 ! Total Coil Rate on the Coil [W]
  REAL(r64)    :: GasUseLoad                       = 0.0d0 ! Gas Usage of Coil [J]
  REAL(r64)    :: ElecUseLoad                      = 0.0d0 ! Electric Usage of Coil [J]
  REAL(r64)    :: GasUseRate                       = 0.0d0 ! Gas Usage of Coil [W]
  REAL(r64)    :: ElecUseRate                      = 0.0d0 ! Electric Usage of Coil [W]
  REAL(r64)    :: Efficiency                       = 0.0d0 ! HeatingCoil Efficiency Value
  REAL(r64)    :: NominalCapacity                  = 0.0d0 ! Nominal Capacity of Coil [W]
  REAL(r64)    :: DesiredOutletTemp                = 0.0d0 !
  REAL(r64)    :: DesiredOutletHumRat              = 0.0d0 !
  REAL(r64)    :: AvailTemperature                 = 0.0d0 ! Used in heat recovery test [C]
  INTEGER      :: AirInletNodeNum                  = 0   !
  INTEGER      :: AirOutletNodeNum                 = 0   !
  INTEGER      :: TempSetPointNodeNum              = 0   ! If applicable this is the node number that the temp setpoint exists.
  INTEGER      :: Control                          = 0   !
  INTEGER      :: PLFCurveIndex                    = 0   ! Index for part-load factor curve index for gas heating coil
  REAL(r64)    :: ParasiticElecLoad                = 0.0d0 ! parasitic electric load associated with the gas heating coil
  REAL(r64)    :: ParasiticGasLoad                 = 0.0d0 ! parasitic gas load associated with the gas heating coil
                                                         ! (standing pilot light) [J]
  REAL(r64)    :: ParasiticGasRate                 = 0.0d0 ! avg. parasitic gas consumption rate with the gas heating coil
                                                         ! (standing pilot light) [J]
  REAL(r64)    :: ParasiticGasCapacity             = 0.0d0 ! capacity of parasitic gas consumption rate, input by user [W]
  REAL(r64)    :: RTF                              = 0.0d0 ! Heater runtime fraction, including PLF curve impacts
  INTEGER      :: RTFErrorIndex                    = 0   ! used in recurring error warnings
  INTEGER      :: RTFErrorCount                    = 0   ! used in recurring error warnings
  INTEGER      :: PLFErrorIndex                    = 0   ! used in recurring error warnings
  INTEGER      :: PLFErrorCount                    = 0   ! used in recurring error warnings
  CHARACTER(len=MaxNameLength) :: ReclaimHeatingCoilName = ' '   ! Name of reclaim heating coil
  INTEGER      :: ReclaimHeatingSourceIndexNum     = 0   ! Index to reclaim heating source (condenser) of a specific type
  INTEGER      :: ReclaimHeatingSource             = 0   ! The source for the Reclaim Heating Coil
!                                                            COMPRESSOR RACK:REFRIGERATED CASE    = 1
!                                                            COIL:DX:COOLINGBYPASSFACTOREMPIRICAL = 2
!                                                            COIL:DX:MULTISPEED:COOLINGEMPIRICAL  = 3
!                                                            COIL:DX:MultiMode:CoolingEmpirical   = 4
!                                                            Refrigeration:Condenser              = 5
  INTEGER :: NumOfStages    =0   ! Number of speeds
  REAL(r64), DIMENSION(:), ALLOCATABLE :: MSNominalCapacity ! Nominal Capacity MS AC Furnace [W]
  REAL(r64), DIMENSION(:), ALLOCATABLE :: MSEfficiency    ! Efficiency for MS AC Furnace [dimensionless]
  REAL(r64), DIMENSION(:), ALLOCATABLE :: MSParasiticElecLoad    ! Parasitic elec load MS AC Furnace (gas only) [W]

END TYPE HeatingCoilEquipConditions

  !MODULE VARIABLE DECLARATIONS:
  INTEGER , PUBLIC:: NumHeatingCoils =0  ! The Number of HeatingCoils found in the Input
  TYPE (HeatingCoilEquipConditions), PUBLIC, ALLOCATABLE, DIMENSION(:) :: HeatingCoil
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: MySizeFlag
  LOGICAL, ALLOCATABLE,DIMENSION(:) :: ValidSourceType ! Used to determine if a source for a desuperheater heating coil is valid
  Logical :: GetCoilsInputFlag = .True. ! Flag set to make sure you get input once
  Logical :: CoilIsSuppHeater =  .False.! Flag set to indicate the heating coil is a supplemental heater
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

! Subroutine Specifications for the Module
          ! Driver/Manager Routines
PUBLIC  SimulateHeatingCoilComponents

          ! Get Input routines for module
PRIVATE GetHeatingCoilInput

          ! Initialization routines for module
PRIVATE InitHeatingCoil
PRIVATE SizeHeatingCoil

          ! Algorithms for the module
Private CalcElectricHeatingCoil
Private CalcGasHeatingCoil
Private CalcDesuperheaterHeatingCoil
Public  CalcMultiStageElectricHeatingCoil
Public  CalcMultiStageGasHeatingCoil

          ! Update routine to check convergence and update nodes
Private UpdateHeatingCoil

          ! Reporting routines for module
Private ReportHeatingCoil

          ! Utility routines for module
Public  CheckHeatingCoilSchedule
Public  GetCoilCapacity
Public  GetCoilInletNode
Public  GetCoilOutletNode
Public  GetCoilIndex
Public  GetHeatReclaimSourceIndex
Public  GetCoilControlNodeNum
Public  GetHeatingCoilTypeNum
Public  GetHeatingCoilIndex
Public  GetHeatingCoilPLFCurveIndex
Public  GetHeatingCoilNumberOfStages
Public  GetCoilAvailScheduleIndex

CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE SimulateHeatingCoilComponents(CompName,FirstHVACIteration,QCoilReq,CompIndex,QCoilActual,SuppHeat, &
                                         FanOpMode, PartLoadRatio, StageNum, SpeedRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages HeatingCoil component simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT (IN)                 :: FirstHVACIteration
  CHARACTER(len=*), INTENT(IN)         :: CompName
  REAL(r64),    INTENT (IN), OPTIONAL  :: QCoilReq       ! coil load to be met
  INTEGER, OPTIONAL                    :: CompIndex
  INTEGER, OPTIONAL                    :: StageNum
  REAL(r64),    INTENT (OUT), OPTIONAL :: QCoilActual    ! coil load actually delivered returned to calling component
  LOGICAL, INTENT(IN), OPTIONAL        :: SuppHeat       ! True if current heating coil is a supplemental heating coil
                                                         ! in a unitary system
  INTEGER, OPTIONAL, INTENT(IN)        :: FanOpMode      ! fan operating mode, CycFanCycCoil or ContFanCycCoil
  REAL(r64), OPTIONAL, INTENT(IN)      :: PartLoadRatio  ! part-load ratio of heating coil
  REAL(r64), OPTIONAL, INTENT(IN)      :: SpeedRatio     ! Speed ratio of MultiStage heating coil

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: CoilNum               ! The HeatingCoil that you are currently loading input into
  REAL(r64) :: QCoilActual2          ! coil load actually delivered returned from specific coil
  INTEGER   :: OpMode                ! fan operating mode
  REAL(r64) :: PartLoadFrac          ! part-load fraction of heating coil
  REAL(r64) :: QCoilRequired         ! local variable for optional argument

          ! FLOW:
  ! Obtains and Allocates HeatingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetHeatingCoilInput
    GetCoilsInputFlag=.false.
  End If

  ! Find the correct HeatingCoilNumber with the Coil Name
  IF (PRESENT(CompIndex)) THEN
    IF (CompIndex == 0) THEN
      CoilNum = FindItemInList(CompName,HeatingCoil%Name,NumHeatingCoils)
      IF (CoilNum == 0) THEN
        CALL ShowFatalError('SimulateHeatingCoilComponents: Coil not found='//TRIM(CompName))
      ENDIF
  !    CompIndex=CoilNum
    ELSE
      CoilNum=CompIndex
      IF (CoilNum > NumHeatingCoils .or. CoilNum < 1) THEN
        CALL ShowFatalError('SimulateHeatingCoilComponents: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(CoilNum))// &
                            ', Number of Heating Coils='//TRIM(TrimSigDigits(NumHeatingCoils))//  &
                            ', Coil name='//TRIM(CompName))
      ENDIF
      IF (CheckEquipName(CoilNum)) THEN
        IF (CompName /= Blank .AND. CompName /= HeatingCoil(CoilNum)%Name) THEN
          CALL ShowFatalError('SimulateHeatingCoilComponents: Invalid CompIndex passed='//  &
                              TRIM(TrimSigDigits(CoilNum))// &
                              ', Coil name='//TRIM(CompName)//', stored Coil Name for that index='//  &
                              TRIM(HeatingCoil(CoilNum)%Name))
        ENDIF
        CheckEquipName(CoilNum)=.false.
      ENDIF
    ENDIF
  ELSE
    CALL ShowSevereError('SimulateHeatingCoilComponents: CompIndex argument not used.')
    CALL ShowContinueError('..CompName = '//TRIM(CompName))
    CALL ShowFatalError('Preceding conditions cause termination.')
  ENDIF

  IF (PRESENT(SuppHeat)) THEN
    CoilIsSuppHeater = SuppHeat
  ELSE
    CoilIsSuppHeater = .FALSE.
  END IF

  IF(PRESENT(FanOpMode))THEN
    OpMode = FanOpMode
  ELSE
    OpMode = ContFanCycCoil
  END IF

  IF(PRESENT(PartLoadRatio))THEN
    PartLoadFrac = PartLoadRatio
  ELSE
    PartLoadFrac = 1.0d0
  END IF

  IF(PRESENT(QCoilReq))THEN
    QCoilRequired = QCoilReq
  ELSE
    QCoilRequired = SensedLoadFlagValue
  END IF

  ! With the correct CoilNum Initialize
  CALL InitHeatingCoil(CoilNum,FirstHVACIteration,QCoilRequired) ! Initialize all HeatingCoil related parameters

  ! Calculate the Correct HeatingCoil Model with the current CoilNum
  If(HeatingCoil(CoilNum)%HCoilType_Num == Coil_HeatingElectric) Then
       Call CalcElectricHeatingCoil(CoilNum,QCoilRequired,QCoilActual2,OpMode,PartLoadFrac)
  ElseIf(HeatingCoil(CoilNum)%HCoilType_Num == Coil_HeatingElectric_MultiStage) Then
       Call CalcMultiStageElectricHeatingCoil(CoilNum,SpeedRatio, PartLoadRatio, StageNum, OpMode) !Objexx:OPTIONAL SpeedRatio, PartLoadRatio, StageNum used without PRESENT check
  ElseIf(HeatingCoil(CoilNum)%HCoilType_Num == Coil_HeatingGas) Then
       Call CalcGasHeatingCoil(CoilNum,QCoilRequired,QCoilActual2,OpMode,PartLoadFrac)
  ElseIf(HeatingCoil(CoilNum)%HCoilType_Num == Coil_HeatingGas_MultiStage) Then
       Call CalcMultiStageGasHeatingCoil(CoilNum,SpeedRatio, PartLoadRatio, StageNum, OpMode) !Objexx:OPTIONAL SpeedRatio, PartLoadRatio, StageNum used without PRESENT check
  ElseIf(HeatingCoil(CoilNum)%HCoilType_Num == Coil_HeatingDesuperheater) Then
       Call CalcDesuperheaterHeatingCoil(CoilNum,QCoilRequired,QCoilActual2)
  Else
       QCoilActual2 = 0.0d0
  End If

  ! Update the current HeatingCoil to the outlet nodes
  Call UpdateHeatingCoil(CoilNum)

  ! Report the current HeatingCoil
  Call ReportHeatingCoil(CoilNum)

  IF (PRESENT(QCoilActual)) THEN
       QCoilActual = QCoilActual2
  ENDIF

  RETURN

END SUBROUTINE SimulateHeatingCoilComponents


! Get Input Section of the Module
!******************************************************************************
SUBROUTINE GetHeatingCoilInput


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for coils and stores it in coil data structures

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor
    USE NodeInputManager,      ONLY: GetOnlySingleNode
    USE BranchNodeConnections, ONLY: TestCompSet
    USE CurveManager,          ONLY: GetCurveIndex
    USE DataIPShortCuts
    USE GlobalNames, ONLY: VerifyUniqueCoilName

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER  :: RoutineName='GetHeatingCoilInput: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: CoilNum      ! The HeatingCoil that you are currently loading input into
    INTEGER :: NumElecCoil
    INTEGER :: NumElecCoilMultiStage
    INTEGER :: NumGasCoil
    INTEGER :: NumGasCoilMultiStage
    INTEGER :: ElecCoilNum
    INTEGER :: GasCoilNum
    INTEGER :: DesuperheaterCoilNum  ! Index to desuperheater heating coil
    INTEGER :: RemainingCoils        ! Index for error checking DO loop for desuperheater coils on remaining heating coil
    INTEGER :: SourceIndexNum  = 0   ! Index to reclaim heating source (condenser) of a specific type
    CHARACTER(len=MaxNameLength) :: SourceTypeString ! character string used in error message for desuperheating coil
    CHARACTER(len=MaxNameLength) :: SourceNameString ! character string used in error message for desuperheating coil
    CHARACTER(len=MaxNameLength) :: CurrentModuleObject  ! for ease in getting objects
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas         ! Alpha input items for object
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
    REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers           ! Numeric input items for object
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.
    INTEGER                       :: MaxNums=0                ! Maximum number of numeric input fields
    INTEGER                       :: MaxAlphas=0              ! Maximum number of alpha input fields
    INTEGER                       :: TotalArgs=0              ! Total number of alpha and numeric arguments (max) for a
                                                              !  certain object in the input file
    INTEGER :: NumAlphas
    INTEGER :: NumNums
    INTEGER :: IOSTAT
    INTEGER :: StageNum
    LOGICAL :: ErrorsFound = .false.   ! If errors detected in input
    LOGICAL :: IsNotOK                 ! Flag to verify name
    LOGICAL :: IsBlank                 ! Flag for blank name
    LOGICAL :: DXCoilErrFlag           ! Used in GetDXCoil mining functions
    LOGICAL :: errflag
          ! Flow
    NumElecCoil            = GetNumObjectsFound('Coil:Heating:Electric')
    NumElecCoilMultiStage  = GetNumObjectsFound('Coil:Heating:Electric:MultiStage')
    NumGasCoil             = GetNumObjectsFound('Coil:Heating:Gas')
    NumGasCoilMultiStage   = GetNumObjectsFound('Coil:Heating:Gas:MultiStage')
    NumDesuperheaterCoil   = GetNumObjectsFound('Coil:Heating:Desuperheater')
    NumHeatingCoils        = NumElecCoil + NumElecCoilMultiStage + NumGasCoil + NumGasCoilMultiStage + NumDesuperheaterCoil
    IF (NumHeatingCoils.GT.0) THEN
      ALLOCATE(HeatingCoil(NumHeatingCoils))
      ALLOCATE(ValidSourceType(NumHeatingCoils))
      ValidSourceType = .FALSE.
      ALLOCATE(CheckEquipName(NumHeatingCoils))
      CheckEquipName=.true.
    ENDIF

    CALL GetObjectDefMaxArgs('Coil:Heating:Electric',TotalArgs,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    CALL GetObjectDefMaxArgs('Coil:Heating:Electric:MultiStage',TotalArgs,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    CALL GetObjectDefMaxArgs('Coil:Heating:Gas',TotalArgs,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    CALL GetObjectDefMaxArgs('Coil:Heating:Gas:MultiStage',TotalArgs,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)
    CALL GetObjectDefMaxArgs('Coil:Heating:Desuperheater',TotalArgs,NumAlphas,NumNums)
    MaxNums=MAX(MaxNums,NumNums)
    MaxAlphas=MAX(MaxAlphas,NumAlphas)

    ALLOCATE(Alphas(MaxAlphas))
    Alphas=' '
    ALLOCATE(cAlphaFields(MaxAlphas))
    cAlphaFields=' '
    ALLOCATE(cNumericFields(MaxNums))
    cNumericFields=' '
    ALLOCATE(Numbers(MaxNums))
    Numbers=0.0d0
    ALLOCATE(lAlphaBlanks(MaxAlphas))
    lAlphaBlanks=.true.
    ALLOCATE(lNumericBlanks(MaxNums))
    lNumericBlanks=.true.

      ! Get the data for electric heating coils
      DO ElecCoilNum = 1,  NumElecCoil

        CoilNum= ElecCoilNum

        CurrentModuleObject='Coil:Heating:Electric'

        CALL GetObjectItem(CurrentModuleObject,ElecCoilNum,Alphas,NumAlphas,Numbers,NumNums,IOSTAT, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(Alphas(1),HeatingCoil%Name,CoilNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) Alphas(1)='xxxxx'
        ENDIF
        CALL VerifyUniqueCoilName(CurrentModuleObject,Alphas(1),errflag,TRIM(CurrentModuleObject)//' Name')
        IF (errflag) THEN
          ErrorsFound=.true.
        ENDIF
        HeatingCoil(CoilNum)%Name     = Alphas(1)
        HeatingCoil(CoilNum)%Schedule = Alphas(2)
        IF (lAlphaBlanks(2)) THEN
          HeatingCoil(CoilNum)%SchedPtr = ScheduleAlwaysOn
        ELSE
          HeatingCoil(CoilNum)%SchedPtr = GetScheduleIndex(Alphas(2))
          IF (HeatingCoil(CoilNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': Invalid '//TRIM(cAlphaFields(2))//  &
               ' entered ='//TRIM(Alphas(2))// &
               ' for '//TRIM(cAlphaFields(1))//'='//TRIM(Alphas(1)))
            ErrorsFound=.true.
          END IF
        ENDIF

        HeatingCoil(CoilNum)%HeatingCoilType  = 'Heating'
        HeatingCoil(CoilNum)%HeatingCoilModel = 'Electric'
        HeatingCoil(CoilNum)%HCoilType_Num    = Coil_HeatingElectric

        HeatingCoil(CoilNum)%Efficiency           = Numbers(1)
        HeatingCoil(CoilNum)%NominalCapacity      = Numbers(2)
        HeatingCoil(CoilNum)%AirInletNodeNum      = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1),  &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
        HeatingCoil(CoilNum)%AirOutletNodeNum     = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1),  &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

        CALL TestCompSet(TRIM(CurrentModuleObject),Alphas(1),Alphas(3),Alphas(4),'Air Nodes')

        HeatingCoil(CoilNum)%TempSetPointNodeNum     = &
               GetOnlySingleNode(Alphas(5),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1),  &
                            NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)

        ! Setup Report variables for the Electric Coils
        CALL SetupOutputVariable('Heating Coil Air Heating Energy [J]',HeatingCoil(CoilNum)%HeatingCoilLoad, &
                              'System','Sum',HeatingCoil(CoilNum)%Name, &
                               ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATINGCOILS',GroupKey='System')
        CALL SetupOutputVariable('Heating Coil Air Heating Rate [W]', HeatingCoil(CoilNum)%HeatingCoilRate, &
                              'System','Average',HeatingCoil(CoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Electric Energy [J]',HeatingCoil(CoilNum)%ElecUseLoad, &
                              'System','Sum',HeatingCoil(CoilNum)%Name,  &
                               ResourceTypeKey='Electric',EndUseKey='Heating',GroupKey='System')
        CALL SetupOutputVariable('Heating Coil Electric Power [W]',HeatingCoil(CoilNum)%ElecUseRate, &
                              'System','Average',HeatingCoil(CoilNum)%Name)

      END DO

      ! Get the data for electric heating coils
      DO ElecCoilNum = 1,  NumElecCoilMultiStage

        CoilNum= NumElecCoil + ElecCoilNum

        CurrentModuleObject='Coil:Heating:Electric:MultiStage'

        CALL GetObjectItem(CurrentModuleObject,ElecCoilNum,Alphas,NumAlphas,Numbers,NumNums,IOSTAT, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(Alphas(1),HeatingCoil%Name,CoilNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) Alphas(1)='xxxxx'
        ENDIF
        CALL VerifyUniqueCoilName(CurrentModuleObject,Alphas(1),errflag,TRIM(CurrentModuleObject)//' Name')
        IF (errflag) THEN
          ErrorsFound=.true.
        ENDIF
       HeatingCoil(CoilNum)%Name     = Alphas(1)
        HeatingCoil(CoilNum)%Schedule = Alphas(2)
        IF (lAlphaBlanks(2)) THEN
          HeatingCoil(CoilNum)%SchedPtr = ScheduleAlwaysOn
        ELSE
          HeatingCoil(CoilNum)%SchedPtr = GetScheduleIndex(Alphas(2))
          IF (HeatingCoil(CoilNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': Invalid '//TRIM(cAlphaFields(2))//  &
               ' entered ='//TRIM(Alphas(2))// &
               ' for '//TRIM(cAlphaFields(1))//'='//TRIM(Alphas(1)))
            ErrorsFound=.true.
          END IF
        ENDIF

        HeatingCoil(CoilNum)%HeatingCoilType  = 'Heating'
        HeatingCoil(CoilNum)%HeatingCoilModel = 'ElectricMultiStage'
        HeatingCoil(CoilNum)%HCoilType_Num    = Coil_HeatingElectric_MultiStage

        HeatingCoil(CoilNum)%NumOfStages          = Numbers(1)

        ALLOCATE(HeatingCoil(CoilNum)%MSEfficiency(HeatingCoil(CoilNum)%NumOfStages))
        ALLOCATE(HeatingCoil(CoilNum)%MSNominalCapacity(HeatingCoil(CoilNum)%NumOfStages))

        DO StageNum=1,HeatingCoil(CoilNum)%NumOfStages

          HeatingCoil(CoilNum)%MSEfficiency(StageNum)      = Numbers(StageNum*2)
          HeatingCoil(CoilNum)%MSNominalCapacity(StageNum) = Numbers(StageNum*2 + 1)

        END DO

        HeatingCoil(CoilNum)%AirInletNodeNum      = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1),  &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
        HeatingCoil(CoilNum)%AirOutletNodeNum     = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1),  &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

        CALL TestCompSet(TRIM(CurrentModuleObject),Alphas(1),Alphas(3),Alphas(4),'Air Nodes')

        HeatingCoil(CoilNum)%TempSetPointNodeNum     = &
               GetOnlySingleNode(Alphas(5),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1),  &
                            NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)

        ! Setup Report variables for the Electric Coils
        CALL SetupOutputVariable('Heating Coil Air Heating Energy [J]',HeatingCoil(CoilNum)%HeatingCoilLoad, &
                              'System','Sum',HeatingCoil(CoilNum)%Name, &
                               ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATINGCOILS',GroupKey='System')
        CALL SetupOutputVariable('Heating Coil Air Heating Rate [W]', HeatingCoil(CoilNum)%HeatingCoilRate, &
                              'System','Average',HeatingCoil(CoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Electric Energy [J]',HeatingCoil(CoilNum)%ElecUseLoad, &
                              'System','Sum',HeatingCoil(CoilNum)%Name,  &
                               ResourceTypeKey='Electric',EndUseKey='Heating',GroupKey='System')
        CALL SetupOutputVariable('Heating Coil Electric Power [W]',HeatingCoil(CoilNum)%ElecUseRate, &
                              'System','Average',HeatingCoil(CoilNum)%Name)

      END DO

      ! Get the data for for gas heating coils
      DO GasCoilNum = 1,  NumGasCoil

        CoilNum= NumElecCoil + NumElecCoilMultiStage + GasCoilNum

        CurrentModuleObject='Coil:Heating:Gas'

        CALL GetObjectItem(CurrentModuleObject,GasCoilNum,Alphas,NumAlphas,Numbers,NumNums,IOSTAT, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(Alphas(1),HeatingCoil%Name,CoilNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) Alphas(1)='xxxxx'
        ENDIF
        CALL VerifyUniqueCoilName(CurrentModuleObject,Alphas(1),errflag,TRIM(CurrentModuleObject)//' Name')
        IF (errflag) THEN
          ErrorsFound=.true.
        ENDIF
        HeatingCoil(CoilNum)%Name     = Alphas(1)
        HeatingCoil(CoilNum)%Schedule = Alphas(2)
        IF (lAlphaBlanks(2)) THEN
          HeatingCoil(CoilNum)%SchedPtr = ScheduleAlwaysOn
        ELSE
          HeatingCoil(CoilNum)%SchedPtr = GetScheduleIndex(Alphas(2))
          IF (HeatingCoil(CoilNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': Invalid '//TRIM(cAlphaFields(2))//  &
               ' entered ='//TRIM(Alphas(2))// &
               ' for '//TRIM(cAlphaFields(1))//'='//TRIM(Alphas(1)))
            ErrorsFound=.true.
          END IF
        ENDIF

        HeatingCoil(CoilNum)%HeatingCoilType  = 'Heating'
        HeatingCoil(CoilNum)%HeatingCoilModel = 'Gas'
        HeatingCoil(CoilNum)%HCoilType_Num    = Coil_HeatingGas

        HeatingCoil(CoilNum)%Efficiency           = Numbers(1)
        HeatingCoil(CoilNum)%NominalCapacity      = Numbers(2)
        HeatingCoil(CoilNum)%AirInletNodeNum      = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1),  &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
        HeatingCoil(CoilNum)%AirOutletNodeNum     = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1),  &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

        CALL TestCompSet(TRIM(CurrentModuleObject),Alphas(1),Alphas(3),Alphas(4),'Air Nodes')

        HeatingCoil(CoilNum)%TempSetPointNodeNum     = &
               GetOnlySingleNode(Alphas(5),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1),  &
                            NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)

        !parasitic electric load associated with the gas heating coil
        HeatingCoil(CoilNum)%ParasiticElecLoad = Numbers(3)

        HeatingCoil(CoilNum)%PLFCurveIndex = GetCurveIndex(Alphas(6)) ! convert curve name to number

        !parasitic gas load associated with the gas heating coil (standing pilot light)
        HeatingCoil(CoilNum)%ParasiticGasCapacity = Numbers(4)

        ! Setup Report variables for the Gas Coils
        CALL SetupOutputVariable('Heating Coil Air Heating Energy [J]',HeatingCoil(CoilNum)%HeatingCoilLoad, &
                              'System','Sum',HeatingCoil(CoilNum)%Name, &
                               ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATINGCOILS',GroupKey='System')
        CALL SetupOutputVariable('Heating Coil Air Heating Rate [W]', HeatingCoil(CoilNum)%HeatingCoilRate, &
                              'System','Average',HeatingCoil(CoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Gas Energy [J]',HeatingCoil(CoilNum)%GasUseLoad, &
                              'System','Sum',HeatingCoil(CoilNum)%Name,  &
                               ResourceTypeKey='Gas',EndUseKey='Heating',GroupKey='System')
        CALL SetupOutputVariable('Heating Coil Gas Rate [W]',HeatingCoil(CoilNum)%GasUseRate, &
                              'System','Average',HeatingCoil(CoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Electric Energy [J]',HeatingCoil(CoilNum)%ElecUseLoad, &
                              'System','Sum',HeatingCoil(CoilNum)%Name,  &
                               ResourceTypeKey='Electricity',EndUseKey='Heating',GroupKey='System')
        CALL SetupOutputVariable('Heating Coil Electric Power [W]',HeatingCoil(CoilNum)%ElecUseRate, &
                              'System','Average',HeatingCoil(CoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Runtime Fraction []',HeatingCoil(CoilNum)%RTF, &
                              'System','Average',HeatingCoil(CoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Ancillary Gas Rate [W]', HeatingCoil(CoilNum)%ParasiticGasRate, &
                              'System','Average',HeatingCoil(CoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Ancillary Gas Energy [J]',HeatingCoil(CoilNum)%ParasiticGasLoad, &
                              'System','Sum',HeatingCoil(CoilNum)%Name,  &
                               ResourceTypeKey='Gas',EndUseKey='Heating',GroupKey='System')

      END DO

      ! Get the data for for gas heating coils
      DO GasCoilNum = 1,  NumGasCoilMultiStage

        CoilNum= NumElecCoil + NumElecCoilMultiStage + NumGasCoil + GasCoilNum

        CurrentModuleObject='Coil:Heating:Gas:MultiStage'

        CALL GetObjectItem(CurrentModuleObject,GasCoilNum,Alphas,NumAlphas,Numbers,NumNums,IOSTAT, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(Alphas(1),HeatingCoil%Name,CoilNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) Alphas(1)='xxxxx'
        ENDIF
        CALL VerifyUniqueCoilName(CurrentModuleObject,Alphas(1),errflag,TRIM(CurrentModuleObject)//' Name')
        IF (errflag) THEN
          ErrorsFound=.true.
        ENDIF
        HeatingCoil(CoilNum)%Name     = Alphas(1)
        HeatingCoil(CoilNum)%Schedule = Alphas(2)
        IF (lAlphaBlanks(2)) THEN
          HeatingCoil(CoilNum)%SchedPtr = ScheduleAlwaysOn
        ELSE
          HeatingCoil(CoilNum)%SchedPtr = GetScheduleIndex(Alphas(2))
          IF (HeatingCoil(CoilNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': Invalid '//TRIM(cAlphaFields(2))//  &
               ' entered ='//TRIM(Alphas(2))// &
               ' for '//TRIM(cAlphaFields(1))//'='//TRIM(Alphas(1)))
            ErrorsFound=.true.
          END IF
        ENDIF

        HeatingCoil(CoilNum)%HeatingCoilType  = 'Heating'
        HeatingCoil(CoilNum)%HeatingCoilModel = 'GasMultiStage'
        HeatingCoil(CoilNum)%HCoilType_Num    = Coil_HeatingGas_MultiStage

        HeatingCoil(CoilNum)%ParasiticGasCapacity = Numbers(1)

        HeatingCoil(CoilNum)%NumOfStages          = Numbers(2)

        ALLOCATE(HeatingCoil(CoilNum)%MSEfficiency(HeatingCoil(CoilNum)%NumOfStages))
        ALLOCATE(HeatingCoil(CoilNum)%MSNominalCapacity(HeatingCoil(CoilNum)%NumOfStages))
        ALLOCATE(HeatingCoil(CoilNum)%MSParasiticElecLoad(HeatingCoil(CoilNum)%NumOfStages))

        DO StageNum=1,HeatingCoil(CoilNum)%NumOfStages

          HeatingCoil(CoilNum)%MSEfficiency(StageNum)      = Numbers(StageNum*3)
          HeatingCoil(CoilNum)%MSNominalCapacity(StageNum) = Numbers(StageNum*3 + 1)
          HeatingCoil(CoilNum)%MSParasiticElecLoad(StageNum) = Numbers(StageNum*3 + 2)

        END DO

        HeatingCoil(CoilNum)%AirInletNodeNum      = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1),  &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
        HeatingCoil(CoilNum)%AirOutletNodeNum     = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1),  &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

        CALL TestCompSet(TRIM(CurrentModuleObject),Alphas(1),Alphas(3),Alphas(4),'Air Nodes')

        HeatingCoil(CoilNum)%TempSetPointNodeNum     = &
               GetOnlySingleNode(Alphas(5),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1),  &
                            NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)

        !parasitic electric load associated with the gas heating coil
        HeatingCoil(CoilNum)%ParasiticElecLoad = Numbers(10)

        HeatingCoil(CoilNum)%PLFCurveIndex = GetCurveIndex(Alphas(6)) ! convert curve name to number

        !parasitic gas load associated with the gas heating coil (standing pilot light)

        ! Setup Report variables for the Gas Coils
        CALL SetupOutputVariable('Heating Coil Air Heating Energy [J]',HeatingCoil(CoilNum)%HeatingCoilLoad, &
                              'System','Sum',HeatingCoil(CoilNum)%Name, &
                               ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATINGCOILS',GroupKey='System')
        CALL SetupOutputVariable('Heating Coil Air Heating Rate [W]', HeatingCoil(CoilNum)%HeatingCoilRate, &
                              'System','Average',HeatingCoil(CoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Gas Energy [J]',HeatingCoil(CoilNum)%GasUseLoad, &
                              'System','Sum',HeatingCoil(CoilNum)%Name,  &
                               ResourceTypeKey='Gas',EndUseKey='Heating',GroupKey='System')
        CALL SetupOutputVariable('Heating Coil Gas Rate [W]',HeatingCoil(CoilNum)%GasUseRate, &
                              'System','Average',HeatingCoil(CoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Electric Energy [J]',HeatingCoil(CoilNum)%ElecUseLoad, &
                              'System','Sum',HeatingCoil(CoilNum)%Name,  &
                               ResourceTypeKey='Electricity',EndUseKey='Heating',GroupKey='System')
        CALL SetupOutputVariable('Heating Coil Electric Power [W]',HeatingCoil(CoilNum)%ElecUseRate, &
                              'System','Average',HeatingCoil(CoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Runtime Fraction []',HeatingCoil(CoilNum)%RTF, &
                              'System','Average',HeatingCoil(CoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Ancillary Gas Rate [W]', HeatingCoil(CoilNum)%ParasiticGasRate, &
                              'System','Average',HeatingCoil(CoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Ancillary Gas Energy [J]',HeatingCoil(CoilNum)%ParasiticGasLoad, &
                              'System','Sum',HeatingCoil(CoilNum)%Name,  &
                               ResourceTypeKey='Gas',EndUseKey='Heating',GroupKey='System')

      END DO

      ! Get the data for for desuperheater heating coils
      DO DesuperheaterCoilNum = 1,  NumDesuperheaterCoil

        CoilNum= NumElecCoil + NumElecCoilMultiStage + NumGasCoil + NumGasCoilMultiStage + DesuperheaterCoilNum

        CurrentModuleObject='Coil:Heating:Desuperheater'

        CALL GetObjectItem(CurrentModuleObject,DesuperheaterCoilNum,Alphas,NumAlphas,Numbers,NumNums,IOSTAT, &
                           NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                           AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(Alphas(1),HeatingCoil%Name,CoilNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) Alphas(1)='xxxxx'
        ENDIF
        CALL VerifyUniqueCoilName(CurrentModuleObject,Alphas(1),errflag,TRIM(CurrentModuleObject)//' Name')
        IF (errflag) THEN
          ErrorsFound=.true.
        ENDIF
        HeatingCoil(CoilNum)%Name     = Alphas(1)
        HeatingCoil(CoilNum)%Schedule = Alphas(2)
        IF (lAlphaBlanks(2)) THEN
          HeatingCoil(CoilNum)%SchedPtr = ScheduleAlwaysOn
        ELSE
          HeatingCoil(CoilNum)%SchedPtr = GetScheduleIndex(Alphas(2))
          IF (HeatingCoil(CoilNum)%SchedPtr == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': Invalid '//TRIM(cAlphaFields(2))//  &
               ' entered ='//TRIM(Alphas(2))// &
               ' for '//TRIM(cAlphaFields(1))//'='//TRIM(Alphas(1)))
            ErrorsFound=.true.
          END IF
        ENDIF

!       check availability schedule for values between 0 and 1
        IF (HeatingCoil(CoilNum)%SchedPtr .GT. 0)THEN
          IF (.NOT. CheckScheduleValueMinMax(HeatingCoil(CoilNum)%SchedPtr,'>=',0.0d0,'<=',1.0d0)) THEN
            CALL ShowSevereError(TRIM(CurrentModuleObject)//' = "'//TRIM(HeatingCoil(CoilNum)%Name)//'"')
            CALL ShowContinueError('Error found in '//TRIM(cAlphaFields(2))//' = '//TRIM(Alphas(2)))
            CALL ShowContinueError('Schedule values must be (>=0., <=1.)')
            ErrorsFound=.true.
          END IF
        END IF

        HeatingCoil(CoilNum)%HeatingCoilType  = 'Heating'
        HeatingCoil(CoilNum)%HeatingCoilModel = 'Desuperheater'
        HeatingCoil(CoilNum)%HCoilType_Num    = Coil_HeatingDesuperheater

        !HeatingCoil(CoilNum)%Efficiency       = Numbers(1)
        !(Numbers(1)) error limits checked and defaults applied on efficiency after
        !       identifying souce type.

        HeatingCoil(CoilNum)%AirInletNodeNum     = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1),  &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
        HeatingCoil(CoilNum)%AirOutletNodeNum     = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1),  &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

        CALL TestCompSet(TRIM(CurrentModuleObject),Alphas(1),Alphas(3),Alphas(4),'Air Nodes')

! Find the DX equipment index associated with the desuperheater heating coil.
! The CoilNum may not be found here when zone heating equip. exists. Check again in InitHeatingCoil.
! (when zone equipment heating coils are included in the input, the air loop DX equipment has not yet been read in)
        IF(SameString(Alphas(5),'Refrigeration:CompressorRack'))THEN
          HeatingCoil(CoilNum)%ReclaimHeatingSource = COMPRESSORRACK_REFRIGERATEDCASE
          CALL GetRefrigeratedRackIndex(Alphas(6),HeatingCoil(CoilNum)%ReclaimHeatingSourceIndexNum, &
                                        RefrigSystemTypeRack,DXCoilErrFlag,Alphas(5))
          IF(HeatingCoil(CoilNum)%ReclaimHeatingSourceIndexNum .GT. 0) ValidSourceType(CoilNum) = .TRUE.
        ELSEIF((SameString(Alphas(5),'Refrigeration:Condenser:AirCooled')).OR.&
                (SameString(Alphas(5),'Refrigeration:Condenser:EvaporativeCooled')).OR.&
                (SameString(Alphas(5),'Refrigeration:Condenser:WaterCooled')))&
        THEN
          HeatingCoil(CoilNum)%ReclaimHeatingSource = CONDENSER_REFRIGERATION
          CALL GetRefrigeratedRackIndex(Alphas(6),HeatingCoil(CoilNum)%ReclaimHeatingSourceIndexNum, &
                                        RefrigSystemTypeDetailed,DXCoilErrFlag,Alphas(5))
          IF(HeatingCoil(CoilNum)%ReclaimHeatingSourceIndexNum .GT. 0) ValidSourceType(CoilNum) = .TRUE.
        ELSEIF(SameString(Alphas(5),'Coil:Cooling:DX:SingleSpeed'))THEN
          HeatingCoil(CoilNum)%ReclaimHeatingSource = COIL_DX_COOLING
          CALL GetDXCoilIndex(Alphas(6),HeatingCoil(CoilNum)%ReclaimHeatingSourceIndexNum, DXCoilErrFlag, Alphas(5))
          IF(HeatingCoil(CoilNum)%ReclaimHeatingSourceIndexNum .GT. 0) ValidSourceType(CoilNum) = .TRUE.
        ELSEIF(SameString(Alphas(5),'Coil:Cooling:DX:TwoSpeed'))THEN
          HeatingCoil(CoilNum)%ReclaimHeatingSource = COIL_DX_MULTISPEED
          CALL GetDXCoilIndex(Alphas(6),HeatingCoil(CoilNum)%ReclaimHeatingSourceIndexNum, DXCoilErrFlag, Alphas(5))
          IF(HeatingCoil(CoilNum)%ReclaimHeatingSourceIndexNum .GT. 0) ValidSourceType(CoilNum) = .TRUE.
        ELSEIF(SameString(Alphas(5),'Coil:Cooling:DX:TwoStageWithHumidityControlMode'))THEN
          HeatingCoil(CoilNum)%ReclaimHeatingSource = COIL_DX_MULTIMODE
          CALL GetDXCoilIndex(Alphas(6),HeatingCoil(CoilNum)%ReclaimHeatingSourceIndexNum, DXCoilErrFlag, Alphas(5))
          IF(HeatingCoil(CoilNum)%ReclaimHeatingSourceIndexNum .GT. 0) ValidSourceType(CoilNum) = .TRUE.
        ELSE
          CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(HeatingCoil(CoilNum)%Name)//&
                           '" valid desuperheater heat source object type not found: '//TRIM(Alphas(5)))
          CALL ShowContinueError('Valid desuperheater heat source objects are:')
          CALL ShowContinueError('Refrigeration:CompressorRack, Coil:Cooling:DX:SingleSpeed, '// &
                                 'Refrigeration:Condenser:AirCooled, Refrigeration:Condenser:EvaporativeCooled, '//&
                                 ' Refrigeration:Condenser:WaterCooled,Coil:Cooling:DX:TwoSpeed, '// &
                                 'and Coil:Cooling:DX:TwoStageWithHumidityControlMode')
          ErrorsFound = .TRUE.
        END IF

        IF (HeatingCoil(CoilNum)%ReclaimHeatingSource == CONDENSER_REFRIGERATION) THEN
           IF (lNumericBlanks(1))THEN
             HeatingCoil(CoilNum)%Efficiency = 0.8d0
           ELSE
             HeatingCoil(CoilNum)%Efficiency       = Numbers(1)
             IF(Numbers(1) .LT. 0.0d0 .OR. Numbers(1) .GT. 0.9d0) THEN
               CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(HeatingCoil(CoilNum)%Name)//&
                           '" heat reclaim recovery efficiency must be >= 0 and <=0.9')
               ErrorsFound = .TRUE.
             END IF
           END IF
        ELSE
           IF (lNumericBlanks(1))THEN
             HeatingCoil(CoilNum)%Efficiency = 0.25d0
           ELSE
             HeatingCoil(CoilNum)%Efficiency       = Numbers(1)
             IF(Numbers(1) .LT. 0.0d0 .OR. Numbers(1) .GT. 0.3d0) THEN
               CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(HeatingCoil(CoilNum)%Name)//&
                           '" heat reclaim recovery efficiency must be >= 0 and <=0.3')
               ErrorsFound = .TRUE.
             END IF
           END IF
        END IF

        HeatingCoil(CoilNum)%ReclaimHeatingCoilName = Alphas(6)

        HeatingCoil(CoilNum)%TempSetPointNodeNum    = &
               GetOnlySingleNode(Alphas(7),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1),  &
                            NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)

        !parasitic electric load associated with the desuperheater heating coil
        HeatingCoil(CoilNum)%ParasiticElecLoad      = Numbers(2)

        IF(Numbers(2) .LT. 0.0d0) THEN
          CALL ShowSevereError(TRIM(CurrentModuleObject)//', "'//TRIM(HeatingCoil(CoilNum)%Name)//&
                           '" parasitic electric load must be >= 0')
          ErrorsFound = .TRUE.
        END IF


        ! Setup Report variables for the Desuperheater Heating Coils
        CALL SetupOutputVariable('Heating Coil Air Heating Energy [J]',HeatingCoil(CoilNum)%HeatingCoilLoad, &
                              'HVAC','Sum',HeatingCoil(CoilNum)%Name, &
                               ResourceTypeKey='ENERGYTRANSFER',EndUseKey='HEATINGCOILS',GroupKey='System')
        CALL SetupOutputVariable('Heating Coil Air Heating Rate [W]', HeatingCoil(CoilNum)%HeatingCoilRate, &
                              'HVAC','Average',HeatingCoil(CoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Electric Energy [J]',HeatingCoil(CoilNum)%ElecUseLoad, &
                              'HVAC','Sum',HeatingCoil(CoilNum)%Name,  &
                               ResourceTypeKey='Electricity',EndUseKey='Heating',GroupKey='System')
        CALL SetupOutputVariable('Heating Coil Electric Power [W]',HeatingCoil(CoilNum)%ElecUseRate, &
                              'HVAC','Average',HeatingCoil(CoilNum)%Name)
        CALL SetupOutputVariable('Heating Coil Runtime Fraction []',HeatingCoil(CoilNum)%RTF, &
                              'HVAC','Average',HeatingCoil(CoilNum)%Name)

      END DO

! perform error check to make sure duplicate heating sources are not used (i.e. 2 desuperheating coils cannot
! use the same heat source). This error check will be expanded in the future to check for duplicates in
! desuperheaters used for water heating purposed.
    DO CoilNum = NumElecCoil + NumGasCoil + 1, NumHeatingCoils
      DO RemainingCoils = CoilNum + 1, NumHeatingCoils
          IF(HeatingCoil(CoilNum)%ReclaimHeatingSource.EQ.HeatingCoil(RemainingCoils)%ReclaimHeatingSource .AND. &
            HeatingCoil(CoilNum)%ReclaimHeatingSourceIndexNum.EQ.HeatingCoil(RemainingCoils)%ReclaimHeatingSourceIndexNum)THEN
            SourceIndexNum = HeatingCoil(CoilNum)%ReclaimHeatingSourceIndexNum
            IF(HeatingCoil(CoilNum)%ReclaimHeatingSource .EQ. COMPRESSORRACK_REFRIGERATEDCASE)THEN
              SourceTypeString='Refrigeration:CompressorRack'
              SourceNameString=HeatReclaimRefrigeratedRack(SourceIndexNum)%Name
            END IF
            IF(HeatingCoil(CoilNum)%ReclaimHeatingSource .EQ. CONDENSER_REFRIGERATION)THEN
              SourceNameString=HeatReclaimRefrigCondenser(SourceIndexNum)%Name
              IF(HeatReclaimRefrigCondenser(SourceIndexNum)%SourceType==RefrigCondenserTypeAir)&
                 SourceTypeString='Refrigeration:Condenser:AirCooled'
              IF(HeatReclaimRefrigCondenser(SourceIndexNum)%SourceType==RefrigCondenserTypeEvap) &
                SourceTypeString='Refrigeration:Condenser:EvaporativeCooled'
              IF(HeatReclaimRefrigCondenser(SourceIndexNum)%SourceType==RefrigCondenserTypeWater) &
                SourceTypeString='Refrigeration:Condenser:WaterCooled'
            END IF
            IF(HeatingCoil(CoilNum)%ReclaimHeatingSource .EQ. COIL_DX_COOLING)THEN
              SourceTypeString='Coil:Cooling:DX:SingleSpeed'
              SourceNameString=HeatReclaimDXCoil(SourceIndexNum)%Name
            END IF
            IF(HeatingCoil(CoilNum)%ReclaimHeatingSource .EQ. COIL_DX_MULTISPEED)THEN
              SourceTypeString='Coil:Cooling:DX:TwoSpeed'
              SourceNameString=HeatReclaimDXCoil(SourceIndexNum)%Name
            END IF
            IF(HeatingCoil(CoilNum)%ReclaimHeatingSource .EQ. COIL_DX_MULTIMODE)THEN
              SourceTypeString='Coil:Cooling:DX:TwoStageWithHumidityControlMode'
              SourceNameString=HeatReclaimDXCoil(SourceIndexNum)%Name
            END IF
            CALL ShowSevereError('Coil:Heating:Desuperheater, "'//TRIM(HeatingCoil(CoilNum)%Name)//&
                           '" and "'//TRIM(HeatingCoil(RemainingCoils)%Name)//'" cannot use the same')
            CALL ShowContinueError(' heat source object '//TRIM(SourceTypeString)//', "'//TRIM(SourceNameString)//'"')
            ErrorsFound = .TRUE.
          END IF
      END DO
    END DO

    IF (ErrorsFound) THEN
       CALL ShowFatalError(RoutineName//'Errors found in input.  Program terminates.')
    ENDIF

    DEALLOCATE(Alphas)
    DEALLOCATE(cAlphaFields)
    DEALLOCATE(cNumericFields)
    DEALLOCATE(Numbers)
    DEALLOCATE(lAlphaBlanks)
    DEALLOCATE(lNumericBlanks)

  RETURN

END SUBROUTINE GetHeatingCoilInput

! End of Get Input subroutines for the HB Module
!******************************************************************************


 ! Beginning Initialization Section of the Module
!******************************************************************************

SUBROUTINE InitHeatingCoil(CoilNum, FirstHVACIteration, QCoilRequired)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   May 2000
          !       MODIFIED       B. Griffith, May 2009 added EMS setpoint check
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the HeatingCoil Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: SameString
  USE EMSManager,      ONLY: iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS
  USE DataGlobals,     ONLY: AnyEnergyManagementSystemInModel

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   Intent(IN) :: CoilNum
  LOGICAL,   Intent(IN) :: FirstHVACIteration
  REAL(r64), Intent(IN) :: QCoilRequired

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: AirInletNode                     ! coil air inlet node number
  INTEGER             :: AirOutletNode                    ! coil air outlet node number
  INTEGER             :: ControlNode                      ! coil control node number
  INTEGER             :: RackNum                          ! Index to refrigerated case compressor rack
  INTEGER             :: CondNum                          ! Index to refrigeration condenser
  INTEGER             :: DXCoilNum                        ! Index to DX cooling coil
  INTEGER, SAVE       :: ValidSourceTypeCounter = 0       ! Counter used to determine if desuperheater source name is valid
  LOGICAL, SAVE       :: HeatingCoilFatalError = .FALSE.  ! used for error checking
  LOGICAL, SAVE       :: MyOneTimeFlag = .true.           ! one time flag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MySPTestFlag      ! used for error checking
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: ShowSingleWarning ! Used for single warning message for desuperheater coil
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag       ! one time environment flag

  IF (MyOneTimeFlag) THEN
    ! initialize the environment and sizing flags
    ALLOCATE(MyEnvrnFlag(NumHeatingCoils))
    ALLOCATE(MySizeFlag(NumHeatingCoils))
    ALLOCATE(ShowSingleWarning(NumHeatingCoils))
    ALLOCATE(MySPTestFlag(NumHeatingCoils))
    MyEnvrnFlag       = .TRUE.
    MySizeFlag        = .TRUE.
    ShowSingleWarning = .TRUE.
    MyOneTimeFlag     = .FALSE.
    MySPTestFlag      = .TRUE.

  END IF


  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(CoilNum)) THEN
    ! for each coil, do the sizing once.
    CALL SizeHeatingCoil(CoilNum)

    MySizeFlag(CoilNum) = .FALSE.
  END IF

  ! Do the following initializations (every time step): This should be the info from
  ! the previous components outlets or the node data in this section.
  !First set the conditions for the air into the coil model
  AirInletNode = HeatingCoil(CoilNum)%AirInletNodeNum
  AirOutletNode = HeatingCoil(CoilNum)%AirOutletNodeNum
  ControlNode = HeatingCoil(CoilNum)%TempSetPointNodeNum
  HeatingCoil(CoilNum)%InletAirMassFlowRate = Node(AirInletNode)%MassFlowRate
  HeatingCoil(CoilNum)%InletAirTemp         = Node(AirInletNode)%Temp
  HeatingCoil(CoilNum)%InletAirHumRat       = Node(AirInletNode)%HumRat
  HeatingCoil(CoilNum)%InletAirEnthalpy     = Node(AirInletNode)%Enthalpy

  ! Set the reporting variables to zero at each timestep.
  HeatingCoil(CoilNum)%HeatingCoilLoad = 0.0d0
  HeatingCoil(CoilNum)%GasUseLoad=0.0d0
  HeatingCoil(CoilNum)%ElecUseLoad=0.0d0
  HeatingCoil(CoilNum)%RTF = 0.0d0

  ! If a temperature setpoint controlled coil must set the desired outlet temp everytime
  IF (ControlNode.EQ.0) THEN
    HeatingCoil(CoilNum)%DesiredOutletTemp = 0.0d0
  ELSE IF (ControlNode.EQ.AirOutletNode) THEN
    HeatingCoil(CoilNum)%DesiredOutletTemp = Node(ControlNode)%TempSetPoint
  ELSE
    HeatingCoil(CoilNum)%DesiredOutletTemp = Node(ControlNode)%TempSetPoint - &
      (Node(ControlNode)%Temp - Node(AirOutletNode)%Temp)
  END IF

  IF(QCoilRequired == SensedLoadFlagValue .AND. MySPTestFlag(CoilNum) &
      .AND. HeatingCoil(CoilNum)%HCoilType_Num .NE. Coil_HeatingElectric_MultiStage .AND. &
        HeatingCoil(CoilNum)%HCoilType_Num .NE. Coil_HeatingGas_MultiStage) THEN

!   If the coil is temperature controlled (QCoilReq == -999.0), both a control node and setpoint are required.
    IF(.NOT. SysSizingCalc .AND. DoSetPointTest)THEN
!     3 possibilities here:
!     1) TempSetPointNodeNum .GT. 0 and TempSetPoint /= SensedNodeFlagValue, this is correct
!     2) TempSetPointNodeNum .EQ. 0, this is not correct, control node is required
!     3) TempSetPointNodeNum .GT. 0 and TempSetPoint == SensedNodeFlagValue, this is not correct, missing temperature setpoint
!     test 2) here (fatal message)
      IF(ControlNode == 0)THEN
        CALL ShowSevereError(TRIM(cAllCoilTypes(HeatingCoil(CoilNum)%HCoilType_Num))// &
                                             ' "'//TRIM(HeatingCoil(CoilNum)%Name)//'"')
        CALL ShowContinueError('... Missing control node for heating coil.')
        CALL ShowContinueError('... enter a control node name in the coil temperature setpoint node field for this'// &
                                 ' heating coil.')
        CALL ShowContinueError('... use a Setpoint Manager to establish a setpoint at the coil temperature setpoint node.')
        HeatingCoilFatalError = .TRUE.
!     test 3) here (fatal message)
      ELSE !IF(ControlNode .GT. 0)THEN
        IF(Node(ControlNode)%TempSetPoint == SensedNodeFlagValue)THEN
          IF (.NOT. AnyEnergyManagementSystemInModel) THEN
            CALL ShowSevereError(TRIM(cAllCoilTypes(HeatingCoil(CoilNum)%HCoilType_Num))// &
                                               ' "'//TRIM(HeatingCoil(CoilNum)%Name)//'"')
            CALL ShowContinueError('... Missing temperature setpoint for heating coil.')
            CALL ShowContinueError('... use a Setpoint Manager to establish a setpoint at the coil temperature setpoint node.')
            HeatingCoilFatalError = .TRUE.
          ELSE
            CALL CheckIfNodeSetpointManagedByEMS(ControlNode,iTemperatureSetpoint, HeatingCoilFatalError)
            IF (HeatingCoilFatalError) THEN
              CALL ShowSevereError(TRIM(cAllCoilTypes(HeatingCoil(CoilNum)%HCoilType_Num))// &
                                                 ' "'//TRIM(HeatingCoil(CoilNum)%Name)//'"')
              CALL ShowContinueError('... Missing temperature setpoint for heating coil.')
              CALL ShowContinueError('... use a Setpoint Manager to establish a setpoint at the coil temperature setpoint node.')
              CALL ShowContinueError('... or use an EMS Actuator to establish a setpoint at the coil temperature setpoint node.')
            ENDIF
          ENDIF
        END IF
      END IF
      MySPTestFlag(CoilNum) = .FALSE.
    END IF
  ELSE IF(MySPTestFlag(CoilNum))THEN
!  If QCoilReq /= SensedLoadFlagValue, the coil is load controlled and does not require a control node
!   4 possibilities here:
!   1) TempSetPointNodeNum .EQ. 0 and TempSetPoint == SensedNodeFlagValue, this is correct
!   2) TempSetPointNodeNum .EQ. 0 and TempSetPoint /= SensedNodeFlagValue, this may be correct,
!      (if no control node specified and SP on heating coil outlet do not show warning, other SP managers may be using SP)
!   3) TempSetPointNodeNum .GT. 0 and TempSetPoint == SensedNodeFlagValue, control node not required if load based control
!   4) TempSetPointNodeNum .GT. 0 and TempSetPoint /= SensedNodeFlagValue, control node not required if load based control
!   test 3) and 4) here (warning only)
    IF(ControlNode .GT. 0)THEN
      CALL ShowWarningError(TRIM(cAllCoilTypes(HeatingCoil(CoilNum)%HCoilType_Num))// &
                                             ' "'//TRIM(HeatingCoil(CoilNum)%Name)//'"')
      CALL ShowContinueError(' The Temperature Setpoint Node Name input is not required for this heating coil because')
      CALL ShowContinueError(' this heating coil is controlled based on the load calculated by the thermostat.')
      CALL ShowContinueError('... this heating coil is not controlled by using a temperature setpoint manager.')
      CALL ShowContinueError('... if a temperature setpoint is placed at the outlet node of this heating coil, that'//&
                             ' temperature setpoint will not be used.')
      CALL ShowContinueError('... leaving the input field "Temperature Setpoint Node Name" blank will'//&
                             ' eliminate this warning.')
    END IF
    MySPTestFlag(CoilNum) = .FALSE.
  END IF

! delay fatal error until all coils are called
  IF(.NOT. FirstHVACIteration .AND. HeatingCoilFatalError)THEN
    CALL ShowFatalError('... errors found in heating coil input.')
  END IF


! Find the heating source index for the desuperheater heating coil if not already found. This occurs when zone heating
! equip. exists. (when zone equipment heating coils are included in the input, the air loop DX equipment has not yet been read)
! Issue a single warning if the coil is not found and continue the simulation
  IF(.NOT. ValidSourceType(CoilNum) .AND. (HeatingCoil(CoilNum)%HCoilType_Num .EQ. Coil_HeatingDesuperheater) .AND. &
     ShowSingleWarning(CoilNum))THEN
    ValidSourceTypeCounter = ValidSourceTypeCounter + 1
    IF(HeatingCoil(CoilNum)%ReclaimHeatingSource .EQ. COMPRESSORRACK_REFRIGERATEDCASE)THEN
      DO RackNum = 1,NumRefrigeratedRacks
        IF(.NOT. SameString(HeatReclaimRefrigeratedRack(RackNum)%Name, &
                            HeatingCoil(CoilNum)%ReclaimHeatingCoilName))CYCLE
        HeatingCoil(CoilNum)%ReclaimHeatingSourceIndexNum = RackNum
        IF(ALLOCATED(HeatReclaimRefrigeratedRack))ValidSourceType(CoilNum) = .TRUE.
        EXIT
      END DO
    ELSEIF(HeatingCoil(CoilNum)%ReclaimHeatingSource .EQ. CONDENSER_REFRIGERATION)THEN
      DO CondNum = 1,NumRefrigCondensers
        IF(.NOT. SameString(HeatReclaimRefrigCondenser(CondNum)%Name, &
                            HeatingCoil(CoilNum)%ReclaimHeatingCoilName))CYCLE
        HeatingCoil(CoilNum)%ReclaimHeatingSourceIndexNum = CondNum
        IF(ALLOCATED(HeatReclaimRefrigCondenser))ValidSourceType(CoilNum) = .TRUE.
        EXIT
      END DO
    ELSEIF(HeatingCoil(CoilNum)%ReclaimHeatingSource .EQ. COIL_DX_COOLING    .OR. &
           HeatingCoil(CoilNum)%ReclaimHeatingSource .EQ. COIL_DX_MULTISPEED .OR. &
           HeatingCoil(CoilNum)%ReclaimHeatingSource .EQ. COIL_DX_MULTIMODE) THEN
      DO DXCoilNum = 1, NumDXCoils
        IF(.NOT. SameString(HeatReclaimDXCoil(DXCoilNum)%Name, &
                            HeatingCoil(CoilNum)%ReclaimHeatingCoilName))CYCLE
        HeatingCoil(CoilNum)%ReclaimHeatingSourceIndexNum = DXCoilNum
        IF(ALLOCATED(HeatReclaimDXCoil))ValidSourceType(CoilNum) = .TRUE.
        EXIT
      END DO
    END IF
    IF((ValidSourceTypeCounter .GT. NumDesuperheaterCoil*2) .AND. ShowSingleWarning(CoilNum) .AND. &
        .NOT. ValidSourceType(CoilNum))THEN
       CALL ShowWarningError('Coil:Heating:Desuperheater, "'//TRIM(HeatingCoil(CoilNum)%Name)//&
                           '" desuperheater heat source object name not found: ' &
                           //TRIM(HeatingCoil(CoilNum)%ReclaimHeatingCoilName))
       CALL ShowContinueError(' Desuperheater heating coil is not modeled and simulation continues.')
       ShowSingleWarning(CoilNum) = .FALSE.
    END IF
  END IF

  RETURN

END SUBROUTINE InitHeatingCoil

SUBROUTINE SizeHeatingCoil(CoilNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   January 2002
          !       MODIFIED       August 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Heating Coil Components for which nominal cpacities have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains heating capacities from the zone or system sizing arrays.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataAirSystems, ONLY: PrimaryAirSystem
  USE DataAirLoop, ONLY: AirLoopControlInfo
  USE OutputReportPredefined
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE General, ONLY: TrimSigDigits, RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: CoilNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)           :: CoilInTemp
  REAL(r64)           :: CoilOutTemp
  REAL(r64)           :: CoilOutHumRat
  REAL(r64)           :: DesCoilLoad
  REAL(r64)           :: DesPriVolFlow
  REAL(r64)           :: DesVolFlow
  REAL(r64)           :: DesMassFlow
  REAL(r64)           :: CpAir
  REAL(r64)           :: OutAirFrac
  REAL(r64)           :: MinPriFlowFrac
  REAL(r64)           :: RhoAirStd
  REAL(r64)           :: CpAirStd
  INTEGER             :: StageNum
  INTEGER             :: NumOfStages
  LOGICAL             :: IsAutosize            ! Indicator to autosize for reporting
  LOGICAL             :: ThisStageAutosize     ! Indicator to autosize at each stage for reporting
  LOGICAL             :: HardSizeNoDesRun      ! Indicator to hardsize with no sizing runs for reporting
  REAL(r64)           :: NominalCapacityDes    ! Autosized nominal capacity for reporting 
  REAL(r64)           :: NominalCapacityUser   ! Hardsized nominal capacity for reporting 
!  REAL(r64)           :: MSNominalCapacityDes  ! Autosized multi stage nominal capacity for reporting
!  REAL(r64)           :: MSNominalCapacityUser ! Hardsized multi stage nominal capacity for reporting 
  LOGICAL :: SizingDesRunThisAirSys            ! true if a particular air system had a Sizing:System object and system sizing done
  LOGICAL :: SizingDesRunThisZone              ! true if a particular zone had a Sizing:Zone object and zone sizing was done

  CoilInTemp = 0.0d0
  CoilOutTemp = 0.0d0
  CoilOutHumRat = 0.0d0
  DesPriVolFlow = 0.0d0
  DesCoilLoad = 0.0d0
  DesVolFlow = 0.0d0
  DesMassFlow = 0.0d0
  CpAir = 0.0d0
  OutAirFrac = 0.0d0
  MinPriFlowFrac = 0.0d0
  CpAirStd = PsyCpAirFnWTdb(0.0d0,20.0d0)
  RhoAirStd = StdRhoAir
  NominalCapacityDes = 0.0d0
  NominalCapacityUser = 0.0d0
!  MSNominalCapacityDes = 0.0d0
!  MSNominalCapacityUser = 0.0d0
  IsAutosize = .FALSE.
  ThisStageAutosize = .FALSE.
  IF (SysSizingRunDone .OR. ZoneSizingRunDone) THEN
    HardSizeNoDesRun = .FALSE.
  ELSE
    HardSizeNoDesRun = .TRUE.
  ENDIF
  IF (CurSysNum > 0) THEN
    CALL CheckThisAirSystemForSizing(CurSysNum, SizingDesRunThisAirSys )
  ELSE
    SizingDesRunThisAirSys =  .FALSE.
  ENDIF
  IF (CurZoneEqNum > 0) THEN
    CALL CheckThisZoneForSizing(CurZoneEqNum, SizingDesRunThisZone)
  ELSE
    SizingDesRunThisZone =  .FALSE.
  ENDIF

  IF (HeatingCoil(CoilNum)%NominalCapacity == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF  

  IF (CurSysNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. SizingDesRunThisAirSys) THEN ! Simulation continue
      HardSizeNoDesRun = .TRUE.    
      IF (HeatingCoil(CoilNum)%NominalCapacity > 0.0d0) THEN
        CALL ReportSizingOutput('Coil:'//TRIM(HeatingCoil(CoilNum)%HeatingCoilType)//':' &
                            //TRIM(HeatingCoil(CoilNum)%HeatingCoilModel), &
                            HeatingCoil(CoilNum)%Name, 'User-Specified Nominal Capacity [W]', &
                            HeatingCoil(CoilNum)%NominalCapacity)  
      END IF
    ELSE ! Autosize or hardsize with sizing data  
      CALL CheckSysSizing('Coil:'//TRIM(HeatingCoil(CoilNum)%HeatingCoilType)//':'//TRIM(HeatingCoil(CoilNum)%HeatingCoilModel), &
                            HeatingCoil(CoilNum)%Name)
      IF (CurOASysNum > 0) THEN
        IF(OASysEqSizing(CurOASysNum)%AirFlow)THEN
          DesVolFlow = OASysEqSizing(CurOASysNum)%AirVolFlow
        ELSE
          DesVolFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
        END IF
      ELSE
        IF(UnitarySysEqSizing(CurSysNum)%AirFlow)THEN
          DesVolFlow = UnitarySysEqSizing(CurSysNum)%AirVolFlow
        ELSE
          SELECT CASE(CurDuctType)
            CASE(Main)
              DesVolFlow = FinalSysSizing(CurSysNum)%SysAirMinFlowRat*FinalSysSizing(CurSysNum)%DesMainVolFlow
            CASE(Cooling)
              DesVolFlow = FinalSysSizing(CurSysNum)%SysAirMinFlowRat*FinalSysSizing(CurSysNum)%DesCoolVolFlow
            CASE(Heating)
              DesVolFlow = FinalSysSizing(CurSysNum)%DesHeatVolFlow
            CASE(Other)
              DesVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
            CASE DEFAULT
              DesVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
          END SELECT
        END IF
      END IF
      DesMassFlow = RhoAirStd*DesVolFlow
      ! get the outside air fraction
      IF (CurOASysNum > 0) THEN
        OutAirFrac = 1.0d0
      ELSE IF (FinalSysSizing(CurSysNum)%HeatOAOption == MinOA) THEN
        IF (DesVolFlow > 0.0d0) THEN
          OutAirFrac = FinalSysSizing(CurSysNum)%DesOutAirVolFlow / DesVolFlow
        ELSE
          OutAirFrac = 1.0d0
        END IF
        OutAirFrac = MIN(1.0d0,MAX(0.0d0,OutAirFrac))
      ELSE
        OutAirFrac = 1.0d0
      END IF
      ! coil inlet temperature
      IF (CurOASysNum == 0 .AND. PrimaryAirSystem(CurSysNum)%NumOAHeatCoils > 0) THEN
        CoilInTemp = OutAirFrac*FinalSysSizing(CurSysNum)%PreheatTemp + &
                         (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%HeatRetTemp
      ELSE
        CoilInTemp = OutAirFrac*FinalSysSizing(CurSysNum)%HeatOutTemp + &
                         (1.0d0-OutAirFrac)*FinalSysSizing(CurSysNum)%HeatRetTemp
      END IF

      ! coil load
      IF (CurOASysNum > 0) THEN
        IF(OASysEqSizing(CurOASysNum)%Capacity)THEN
          DesCoilLoad = OASysEqSizing(CurOASysNum)%DesHeatingLoad
        ELSE
          DesCoilLoad = CpAirStd*DesMassFlow*(FinalSysSizing(CurSysNum)%PreheatTemp - CoilInTemp)
        END IF
      ELSE
        IF(UnitarySysEqSizing(CurSysNum)%Capacity)THEN
          DesCoilLoad = UnitarySysEqSizing(CurSysNum)%DesHeatingLoad
        ELSE
          DesCoilLoad = CpAirStd*DesMassFlow*(FinalSysSizing(CurSysNum)%HeatSupTemp - CoilInTemp)
        END IF
      END IF

      IF (AirLoopControlInfo(CurSysNum)%UnitarySys) THEN
        IF (CoilIsSuppHeater) THEN
          NominalCapacityDes = SuppHeatCap
        ELSE
         ! TRUE for all air loop parent equipment except UnitarySystem where flag is reset to FALSE after simulating
         ! This method allows downstream heating coils to size individually. Probably should do this for all air loop equipment
         ! ChangoverBypass model always sets AirLoopControlInfo%UnitarySys to FALSE so heating coil can individually size
          IF(AirLoopControlInfo(CurSysNum)%UnitarySysSimulating)THEN
            NominalCapacityDes = UnitaryHeatCap
          ELSE
            IF (DesCoilLoad >= SmallLoad) THEN
              NominalCapacityDes = DesCoilLoad
            ELSE
              NominalCapacityDes = 0.0d0
            END IF
          END IF
        END IF
      ELSE
        IF (DesCoilLoad >= SmallLoad) THEN
          NominalCapacityDes = DesCoilLoad
        ELSE
          NominalCapacityDes = 0.0d0
        END IF
      END IF
    END IF  

  ELSE IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. SizingDesRunThisZone) THEN ! Simulation continue
      HardSizeNoDesRun = .TRUE.    
      IF (HeatingCoil(CoilNum)%NominalCapacity > 0.0d0) THEN
        CALL ReportSizingOutput('Coil:'//TRIM(HeatingCoil(CoilNum)%HeatingCoilType)//':' &
                            //TRIM(HeatingCoil(CoilNum)%HeatingCoilModel), &
                            HeatingCoil(CoilNum)%Name, 'User-Specified Nominal Capacity [W]', &
                            HeatingCoil(CoilNum)%NominalCapacity) 
      END IF
    ELSE ! Autosize or hardsize with sizing data  

      CALL CheckZoneSizing('Coil:'//TRIM(HeatingCoil(CoilNum)%HeatingCoilType)//':'//TRIM(HeatingCoil(CoilNum)%HeatingCoilModel), &
                            HeatingCoil(CoilNum)%Name)
      IF(ZoneEqSizing(CurZoneEqNum)%Capacity)THEN
        NominalCapacityDes = ZoneEqSizing(CurZoneEqNum)%DesHeatingLoad
      ELSE IF (FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow >= SmallMassFlow) THEN
        IF (TermUnitPIU) THEN
          MinPriFlowFrac = TermUnitSizing(CurZoneEqNum)%MinFlowFrac
          IF (TermUnitSizing(CurZoneEqNum)%InducesPlenumAir) THEN
            CoilInTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTempTU * MinPriFlowFrac + &
                           FinalZoneSizing(CurZoneEqNum)%ZoneRetTempAtHeatPeak * (1.0d0 - MinPriFlowFrac)
          ELSE
            CoilInTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTempTU * MinPriFlowFrac + &
                           FinalZoneSizing(CurZoneEqNum)%ZoneTempAtHeatPeak * (1.0d0 - MinPriFlowFrac)
          END IF
        ELSE IF (TermUnitIU) THEN
          CoilInTemp = FinalZoneSizing(CurZoneEqNum)%ZoneTempAtHeatPeak
        ELSE IF (TermUnitSingDuct) THEN
          CoilInTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTempTU
        ELSE
          CoilInTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTemp
        END IF
        IF (TermUnitSingDuct .OR. TermUnitPIU) THEN
          CoilOutTemp = FinalZoneSizing(CurZoneEqNum)%HeatDesTemp
          CoilOutHumRat = FinalZoneSizing(CurZoneEqNum)%HeatDesHumRat
          CpAir = PsyCpAirFnWTdb(CoilOutHumRat, 0.5d0*(CoilInTemp+CoilOutTemp))
          DesCoilLoad = CpAir * RhoAirStd * TermUnitSizing(CurZoneEqNum)%AirVolFlow * (CoilOutTemp-CoilInTemp)
        ELSE IF (TermUnitIU) THEN
          IF (TermUnitSizing(CurZoneEqNum)%InducRat > 0.01d0) THEN
            DesPriVolFlow = TermUnitSizing(CurZoneEqNum)%AirVolFlow / TermUnitSizing(CurZoneEqNum)%InducRat
            CpAir = PsyCpAirFnWTdb(FinalZoneSizing(CurZoneEqNum)%HeatDesHumRat,FinalZoneSizing(CurZoneEqNum)%HeatDesTemp)
            ! the design heating coil load is the zone load minus whatever the central system does. Note that
            ! DesHeatCoilInTempTU is really the primary air inlet temperature for the unit.
            DesCoilLoad = FinalZoneSizing(CurZoneEqNum)%DesHeatLoad - CpAir*RhoAirStd*DesPriVolFlow* &
              (FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTempTU - FinalZoneSizing(CurZoneEqNum)%ZoneTempAtHeatPeak)
          ELSE
            DesCoilLoad = 0.0d0
          END IF
        ELSE
          CoilOutTemp = FinalZoneSizing(CurZoneEqNum)%HeatDesTemp
          CoilOutHumRat = FinalZoneSizing(CurZoneEqNum)%HeatDesHumRat
          CpAir = PsyCpAirFnWTdb(CoilOutHumRat, 0.5d0*(CoilInTemp+CoilOutTemp))
          DesCoilLoad = CpAir * FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow * (CoilOutTemp-CoilInTemp)
        END IF
        NominalCapacityDes = MAX(0.0d0,DesCoilLoad)
      ELSE
        NominalCapacityDes = 0.0d0
      END IF
    END IF 
  END IF  
  IF (.NOT. HardSizeNoDesRun) THEN
    IF (IsAutosize) THEN
      HeatingCoil(CoilNum)%NominalCapacity = NominalCapacityDes
      CALL ReportSizingOutput('Coil:'//TRIM(HeatingCoil(CoilNum)%HeatingCoilType)//':' &
                            //TRIM(HeatingCoil(CoilNum)%HeatingCoilModel), &
                            HeatingCoil(CoilNum)%Name, 'Design Size Nominal Capacity [W]', NominalCapacityDes)                                    
    ELSE  
      IF (HeatingCoil(CoilNum)%NominalCapacity > 0.0d0 .AND. NominalCapacityDes > 0.0d0) THEN            
        NominalCapacityUser = HeatingCoil(CoilNum)%NominalCapacity
        CALL ReportSizingOutput('Coil:'//TRIM(HeatingCoil(CoilNum)%HeatingCoilType)//':' &
                          //TRIM(HeatingCoil(CoilNum)%HeatingCoilModel), &
                          HeatingCoil(CoilNum)%Name, 'Design Size Nominal Capacity [W]', NominalCapacityDes, &
                          'User-Specified Nominal Capacity [W]', NominalCapacityUser) 
        IF (DisplayExtraWarnings) THEN
          IF ((ABS(NominalCapacityDes - NominalCapacityUser)/NominalCapacityUser ) > AutoVsHardSizingThreshold) THEN
            CALL ShowMessage('SizeHeatingCoil: Potential issue with equipment sizing for ' &
                                       //TRIM(HeatingCoil(CoilNum)%HeatingCoilType)//' '//  &
                                   TRIM(HeatingCoil(CoilNum)%HeatingCoilModel)//', '//TRIM(HeatingCoil(CoilNum)%Name))
            CALL ShowContinueError('User-Specified Nominal Capacity of '// &
                                  TRIM(RoundSigDigits(NominalCapacityUser,2))// ' [W]')
            CALL ShowContinueError('differs from Design Size Nominal Capacity of ' // &
                                  TRIM(RoundSigDigits(NominalCapacityDes,2))// ' [W]')
            CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
            CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
          END IF
        ENDIF
      END IF
    END IF
  END IF

  IF (HeatingCoil(CoilNum)%HCoilType_Num .EQ. Coil_HeatingElectric_MultiStage .OR. &
      HeatingCoil(CoilNum)%HCoilType_Num .EQ. Coil_HeatingGas_MultiStage ) THEN
    IsAutosize = .FALSE.  
    IF (ANY(HeatingCoil(CoilNum)%MSNominalCapacity == AutoSize)) THEN
      IsAutosize = .TRUE.
    END IF  
    IF (IsAutosize) THEN
      NumOfStages = HeatingCoil(CoilNum)%NumOfStages
      DO StageNum=NumOfStages, 1, -1
        IF (HeatingCoil(CoilNum)%MSNominalCapacity(StageNum) == AutoSize) THEN
          ThisStageAutosize = .TRUE.
        END IF  
        IF (StageNum .EQ. NumOfStages) THEN
          IF (CoilIsSuppHeater) THEN
            NominalCapacityDes = SuppHeatCap
          ELSE
            IF(AirLoopControlInfo(CurSysNum)%UnitarySysSimulating)THEN
              NominalCapacityDes = UnitaryHeatCap
            ELSE
              IF (DesCoilLoad >= SmallLoad) THEN
                NominalCapacityDes = DesCoilLoad
              ELSE
                NominalCapacityDes = 0.0d0
              END IF
            END IF
          END IF
        ELSE
          NominalCapacityDes = HeatingCoil(CoilNum)%MSNominalCapacity(NumOfStages) * &
                                 StageNum / NumOfStages
        ENDIF
        IF (ThisStageAutosize) THEN
          HeatingCoil(CoilNum)%MSNominalCapacity(StageNum) = NominalCapacityDes   
          CALL ReportSizingOutput('Coil:'//TRIM(HeatingCoil(CoilNum)%HeatingCoilType)//':' &
                  //TRIM(HeatingCoil(CoilNum)%HeatingCoilModel), HeatingCoil(CoilNum)%Name, 'Stage ' &
                  //TRIM(TrimSigDigits(StageNum))//' Design Size Nominal Capacity [W]', NominalCapacityDes)
        ELSE 
          IF (HeatingCoil(CoilNum)%MSNominalCapacity(StageNum) > 0.0d0 .AND. NominalCapacityDes > 0.0d0) THEN
            NominalCapacityUser = HeatingCoil(CoilNum)%MSNominalCapacity(StageNum)
            CALL ReportSizingOutput('Coil:'//TRIM(HeatingCoil(CoilNum)%HeatingCoilType)//':' &
                  //TRIM(HeatingCoil(CoilNum)%HeatingCoilModel), HeatingCoil(CoilNum)%Name, 'Stage ' &
                  //TRIM(TrimSigDigits(StageNum))//' Design Size Nominal Capacity [W]', NominalCapacityDes, &
                 ' User-Specified Nominal Capacity [W]', NominalCapacityUser)
            IF (DisplayExtraWarnings) THEN
              IF ((ABS(NominalCapacityDes - NominalCapacityUser)/NominalCapacityUser) > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizeHeatingCoil: Potential issue with equipment sizing for ' &
                                     //TRIM(HeatingCoil(CoilNum)%HeatingCoilType)//' '//  &
                       TRIM(HeatingCoil(CoilNum)%HeatingCoilModel)//', '//TRIM(HeatingCoil(CoilNum)%Name))
                CALL ShowContinueError('User-Specified Nominal Capacity of '// &
                                    TRIM(RoundSigDigits(NominalCapacityUser,2))// ' [W]')
                CALL ShowContinueError('differs from Design Size Nominal Capacity of ' // &
                                    TRIM(RoundSigDigits(NominalCapacityDes,2))// ' [W]')
                CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.') 
              END IF
            ENDIF
          END IF  
        ENDIF
      END DO
    ELSE ! No autosize
      NumOfStages = HeatingCoil(CoilNum)%NumOfStages  
      DO StageNum=NumOfStages, 1, -1        
        IF (HeatingCoil(CoilNum)%MSNominalCapacity(StageNum) > 0.0d0) THEN
          CALL ReportSizingOutput('Coil:'//TRIM(HeatingCoil(CoilNum)%HeatingCoilType)//':' &
                  //TRIM(HeatingCoil(CoilNum)%HeatingCoilModel), HeatingCoil(CoilNum)%Name, 'Stage ' &
                  //TRIM(TrimSigDigits(StageNum))//' User-Specified Nominal Capacity [W]', &
                  HeatingCoil(CoilNum)%MSNominalCapacity(StageNum))  
        END IF
      END DO  
    ENDIF
   ! Ensure capacity at lower Stage must be lower or equal to the capacity at higher Stage.
    !IF (IsAutosize) THEN
      Do StageNum = 1,HeatingCoil(CoilNum)%NumOfStages-1
        If (HeatingCoil(CoilNum)%MSNominalCapacity(StageNum) .GT. HeatingCoil(CoilNum)%MSNominalCapacity(StageNum+1)) Then
          CALL ShowSevereError('SizeHeatingCoil: '//TRIM(HeatingCoil(CoilNum)%HeatingCoilType)//' '// &
            TRIM(HeatingCoil(CoilNum)%Name)//', '// 'Stage '//TRIM(TrimSigDigits(StageNum)) &
            //' Nominal Capacity ('//TRIM(RoundSigDigits(HeatingCoil(CoilNum)%MSNominalCapacity(StageNum),2)) &
            //' W) must be less than or equal to '//'Stage '//TRIM(TrimSigDigits(StageNum+1)) &
            //' Nominal Capacity ('//TRIM(RoundSigDigits(HeatingCoil(CoilNum)%MSNominalCapacity(StageNum+1),2))//' W).')
          CALL ShowFatalError('Preceding conditions cause termination.')
        End If
      End Do
    !ENDIF
  END IF  

  !create predefined report entries
  SELECT CASE (HeatingCoil(CoilNum)%HCoilType_Num)
    CASE (Coil_HeatingElectric)
      CALL PreDefTableEntry(pdchHeatCoilType,HeatingCoil(CoilNum)%Name,'Coil:Heating:Electric')
      CALL PreDefTableEntry(pdchHeatCoilNomCap,HeatingCoil(CoilNum)%Name,HeatingCoil(CoilNum)%NominalCapacity)
      CALL PreDefTableEntry(pdchHeatCoilNomEff,HeatingCoil(CoilNum)%Name,HeatingCoil(CoilNum)%Efficiency)
    CASE (Coil_HeatingElectric_MultiStage)
      CALL PreDefTableEntry(pdchHeatCoilType,HeatingCoil(CoilNum)%Name,'Coil:Heating:Electric:MultiStage')
      CALL PreDefTableEntry(pdchHeatCoilNomCap,HeatingCoil(CoilNum)%Name, &
                            HeatingCoil(CoilNum)%MSNominalCapacity(HeatingCoil(CoilNum)%NumOfStages))
      CALL PreDefTableEntry(pdchHeatCoilNomEff,HeatingCoil(CoilNum)%Name, &
                            HeatingCoil(CoilNum)%MSEfficiency(HeatingCoil(CoilNum)%NumOfStages))
    CASE (Coil_HeatingGas)
      CALL PreDefTableEntry(pdchHeatCoilType,HeatingCoil(CoilNum)%Name,'Coil:Heating:Gas')
      CALL PreDefTableEntry(pdchHeatCoilNomCap,HeatingCoil(CoilNum)%Name,HeatingCoil(CoilNum)%NominalCapacity)
      CALL PreDefTableEntry(pdchHeatCoilNomEff,HeatingCoil(CoilNum)%Name,HeatingCoil(CoilNum)%Efficiency)
    CASE (Coil_HeatingGas_MultiStage)
      CALL PreDefTableEntry(pdchHeatCoilType,HeatingCoil(CoilNum)%Name,'Coil:Heating:Gas:MultiStage')
      CALL PreDefTableEntry(pdchHeatCoilNomCap,HeatingCoil(CoilNum)%Name, &
                            HeatingCoil(CoilNum)%MSNominalCapacity(HeatingCoil(CoilNum)%NumOfStages))
      CALL PreDefTableEntry(pdchHeatCoilNomEff,HeatingCoil(CoilNum)%Name, &
                            HeatingCoil(CoilNum)%MSEfficiency(HeatingCoil(CoilNum)%NumOfStages))
    CASE (Coil_HeatingDesuperheater)
      CALL PreDefTableEntry(pdchHeatCoilType,HeatingCoil(CoilNum)%Name,'Coil:Heating:Desuperheater')
      CALL PreDefTableEntry(pdchHeatCoilNomCap,HeatingCoil(CoilNum)%Name,HeatingCoil(CoilNum)%NominalCapacity)
      CALL PreDefTableEntry(pdchHeatCoilNomEff,HeatingCoil(CoilNum)%Name,HeatingCoil(CoilNum)%Efficiency)
  END SELECT

  RETURN

END SUBROUTINE SizeHeatingCoil

 ! End Initialization Section of the Module
!******************************************************************************


 ! Begin Algorithm Section of the Module
!******************************************************************************

Subroutine CalcElectricHeatingCoil(CoilNum,QCoilReq,QCoilActual,FanOpMode,PartLoadRatio)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rich Liesen
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulates a simple Electric heating coil with an efficiency

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:


          ! USE STATEMENTS:
      USE DataHVACGlobals, ONLY: TempControlTol, ElecHeatingCoilPower
      USE DataAirLoop,     ONLY: LoopHeatingCoilMaxRTF

          ! SUBROUTINE ARGUMENT DEFINITIONS:
      INTEGER,   INTENT(IN)  :: CoilNum        ! index to heating coil
      REAL(r64), INTENT(OUT) :: QCoilActual    ! coil load actually delivered (W)
      INTEGER,   INTENT(IN)  :: FanOpMode      ! fan operating mode
      REAL(r64), INTENT(IN)  :: PartLoadRatio  ! part-load ratio of heating coil

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      REAL(r64) AirMassFlow  ! [kg/sec]
      REAL(r64) TempAirIn    ! [C]
      REAL(r64) TempAirOut   ! [C]
      REAL(r64) Win
      REAL(r64) Effic
      REAL(r64) CapacitanceAir
      REAL(r64) HeatingCoilLoad
      REAL(r64) QCoilReq
      REAL(r64) QCoilCap
      REAL(r64) TempSetPoint
      Integer Control

   Effic        = HeatingCoil(CoilNum)%Efficiency
   TempAirIn    = HeatingCoil(CoilNum)%InletAirTemp
   Win          = HeatingCoil(CoilNum)%InletAirHumRat
   Control      = HeatingCoil(CoilNum)%Control
   TempSetPoint = HeatingCoil(CoilNum)%DesiredOutletTemp

!  adjust mass flow rates for cycling fan cycling coil operation
   IF(FanOpMode .EQ. CycFanCycCoil)THEN
     IF(PartLoadRatio .GT. 0.0d0)THEN
       AirMassFlow = HeatingCoil(CoilNum)%InletAirMassFlowRate / PartLoadRatio
       QCoilReq    = QCoilReq / PartLoadRatio
     ELSE
       AirMassFlow = 0.0d0
     END IF
   ELSE
     AirMassFlow   = HeatingCoil(CoilNum)%InletAirMassFlowRate
   END IF

   CapacitanceAir=PsyCpAirFnWTdb(Win,TempAirIn)*AirMassFlow

  ! If the coil is operating there should be some heating capacitance
  !  across the coil, so do the simulation. If not set outlet to inlet and no load.
  !  Also the coil has to be scheduled to be available.

  ! Control output to meet load QCoilReq (QCoilReq is passed in if load controlled, otherwise QCoilReq=-999)
  IF((AirMassFlow .GT. 0.0d0 .AND. HeatingCoil(CoilNum)%NominalCapacity > 0.0d0) .and. &
     (GetCurrentScheduleValue(HeatingCoil(CoilNum)%SchedPtr) .gt. 0.0d0) .and. &
     (QCoilReq .gt. 0.0d0)) THEN

      !check to see if the Required heating capacity is greater than the user specified capacity.
      IF(QCoilReq > HeatingCoil(CoilNum)%NominalCapacity) Then
        QCoilCap = HeatingCoil(CoilNum)%NominalCapacity
      Else
        QCoilCap = QCoilReq
      End IF

      TempAirOut=TempAirIn + QCoilCap/CapacitanceAir
      HeatingCoilLoad = QCoilCap

     !The HeatingCoilLoad is the change in the enthalpy of the Heating
      HeatingCoil(CoilNum)%ElecUseLoad = HeatingCoilLoad/Effic

   ! Control coil output to meet a setpoint temperature.
   Else IF((AirMassFlow .GT. 0.0d0 .AND. HeatingCoil(CoilNum)%NominalCapacity > 0.0d0) .and. &
     (GetCurrentScheduleValue(HeatingCoil(CoilNum)%SchedPtr) .gt. 0.0d0) .and. &
     (QCoilReq == SensedLoadFlagValue) .and. &
     (ABS(TempSetPoint-TempAirIn) .gt. TempControlTol) ) THEN

      QCoilCap = CapacitanceAir*(TempSetPoint - TempAirIn)
      ! check to see if setpoint above enetering temperature. If not, set
      ! output to zero.
      IF(QCoilCap .LE. 0.0d0) THEN
        QCoilCap = 0.0d0
        TempAirOut = TempAirIn
      !check to see if the Required heating capacity is greater than the user
      ! specified capacity.
      Else IF(QCoilCap > HeatingCoil(CoilNum)%NominalCapacity) Then
        QCoilCap = HeatingCoil(CoilNum)%NominalCapacity
        TempAirOut=TempAirIn + QCoilCap/CapacitanceAir
      Else
        TempAirOut = TempSetPoint
      End IF

      HeatingCoilLoad = QCoilCap

     !The HeatingCoilLoad is the change in the enthalpy of the Heating
      HeatingCoil(CoilNum)%ElecUseLoad = HeatingCoilLoad/Effic


  Else ! If not running Conditions do not change across coil from inlet to outlet

      TempAirOut=TempAirIn
      HeatingCoilLoad=0.0d0
      HeatingCoil(CoilNum)%ElecUseLoad = 0.0d0
  END IF

   IF(FanOpMode .EQ. CycFanCycCoil)THEN
     HeatingCoil(CoilNum)%ElecUseLoad = HeatingCoil(CoilNum)%ElecUseLoad * PartLoadRatio
     HeatingCoilLoad = HeatingCoilLoad * PartLoadRatio
   END IF

   HeatingCoil(CoilNum)%HeatingCoilLoad = HeatingCoilLoad
   ElecHeatingCoilPower = HeatingCoil(CoilNum)%ElecUseLoad

  ! Set the outlet conditions
   HeatingCoil(CoilNum)%OutletAirTemp   = TempAirOut

   ! This HeatingCoil does not change the moisture or Mass Flow across the component
   HeatingCoil(CoilNum)%OutletAirHumRat       = HeatingCoil(CoilNum)%InletAirHumRat
   HeatingCoil(CoilNum)%OutletAirMassFlowRate = HeatingCoil(CoilNum)%InletAirMassFlowRate
   !Set the outlet enthalpys for air and Heating
   HeatingCoil(CoilNum)%OutletAirEnthalpy = PsyHFnTdbW(HeatingCoil(CoilNum)%OutletAirTemp, &
                                             HeatingCoil(CoilNum)%OutletAirHumRat)

   QCoilActual = HeatingCoilLoad
   IF (ABS(HeatingCoil(CoilNum)%NominalCapacity) < 1.d-8) THEN
     LoopHeatingCoilMaxRTF = MAX(LoopHeatingCoilMaxRTF,0.0d0)
   ELSE
     LoopHeatingCoilMaxRTF = MAX(LoopHeatingCoilMaxRTF,HeatingCoilLoad/HeatingCoil(CoilNum)%NominalCapacity)
   ENDIF

RETURN
END Subroutine CalcElectricHeatingCoil

SUBROUTINE CalcMultiStageElectricHeatingCoil(CoilNum,SpeedRatio, CycRatio, StageNum, FanOpMode)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma, FSEC
          !       DATE WRITTEN   January 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the air-side performance and electrical energy use of multistage electric heating coil.

          ! METHODOLOGY EMPLOYED:
          ! Uses the same methodology as the single stage electric heating unit model (SUBROUTINE CalcelectricHeatingCoil).
          ! In addition it assumes that the unit performance is obtained by interpolating between
          ! the performance at high stage and that at low stage. If the output needed is below
          ! that produced at low stage, the coil cycles between off and low stage.

          ! USE STATEMENTS:
USE CurveManager,        ONLY: CurveValue
USE General,             ONLY: TrimSigDigits, RoundSigDigits
USE DataHVACGlobals,     ONLY: MSHPMassFlowRateLow, MSHPMassFlowRateHigh, ElecHeatingCoilPower
USE Psychrometrics,      ONLY: PsyTdbFnHW, PsyRhFnTdbWPb, PsyTsatFnHPb, PsyWFnTdbH
USE DataEnvironment,     ONLY: OutBaroPress

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER   :: CoilNum      ! the number of the electric heating coil to be simulated
REAL(r64) :: SpeedRatio   ! SpeedRatio varies between 1.0 (maximum speed) and 0.0 (minimum speed)
REAL(r64) :: CycRatio       ! cycling part load ratio
INTEGER   :: StageNum       ! Stage number
INTEGER   :: FanOpMode      ! Fan operation mode

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: RoutineName='CalcMultiStageElectricHeatingCoil'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64) :: AirMassFlow         ! dry air mass flow rate through coil [kg/s]
REAL(r64) :: InletAirDryBulbTemp ! inlet air dry bulb temperature [C]
REAL(r64) :: InletAirEnthalpy    ! inlet air enthalpy [J/kg]
REAL(r64) :: InletAirHumRat      ! inlet air humidity ratio [kg/kg]
REAL(r64) :: OutletAirEnthalpy   ! outlet air enthalpy [J/kg]
REAL(r64) :: OutletAirHumRat     ! outlet air humidity ratio [kg/kg]
REAL(r64) :: TotCapHS            ! total capacity at high stage [W]
REAL(r64) :: TotCapLS            ! total capacity at low stage [W]
REAL(r64) :: TotCap              ! total capacity at current stage [W]
REAL(r64) :: EffHS               ! total capacity at high stage [W]
REAL(r64) :: EffLS               ! total capacity at low stage [W]
REAL(r64) :: OutdoorPressure      ! Outdoor barometric pressure at condenser (Pa)
INTEGER   :: StageNumHS           ! High stage number
INTEGER   :: StageNumLS           ! Low stage number
REAL(r64) :: FullLoadOutAirEnth   ! Outlet full load enthalpy
REAL(r64) :: FullLoadOutAirHumRat ! Outlet humidity ratio at full load
REAL(r64) :: FullLoadOutAirTemp   ! Outlet temperature at full load
REAL(r64) :: FullLoadOutAirRH     ! Outler relative humidity at full load
REAL(r64) :: OutletAirTemp        ! Supply ari temperature
REAL(r64) :: LSFullLoadOutAirEnth ! Outlet full load enthalpy at low stage
REAL(r64) :: HSFullLoadOutAirEnth ! Outlet full load enthalpy at high stage
REAL(r64) :: LSElecHeatingPower   ! Full load power at low stage
REAL(r64) :: HSElecHeatingPower   ! Full load power at high stage
REAL(r64) :: PartLoadRat          ! part load ratio

! FLOW
If (StageNum > 1) Then
   StageNumLS = StageNum-1
   StageNumHS = StageNum
  If (StageNum .GT. HeatingCoil(CoilNum)%NumOfStages) Then
    StageNumLS = HeatingCoil(CoilNum)%NumOfStages-1
    StageNumHS = HeatingCoil(CoilNum)%NumOfStages
  End If
Else
  StageNumLS = 1
  StageNumHS = 1
End If

AirMassFlow         = HeatingCoil(CoilNum)%InletAirMassFlowRate
InletAirDryBulbTemp = HeatingCoil(CoilNum)%InletAirTemp
InletAirEnthalpy    = HeatingCoil(CoilNum)%InletAirEnthalpy
InletAirHumRat      = HeatingCoil(CoilNum)%InletAirHumRat

OutdoorPressure = OutBaroPress

IF((AirMassFlow .GT. 0.0d0) .AND. &
   (GetCurrentScheduleValue(HeatingCoil(CoilNum)%SchedPtr) .GT. 0.0d0) .AND. &
   ((CycRatio .GT. 0.0d0) .OR. (SpeedRatio .GT. 0.0d0))) THEN

  If (StageNum > 1) Then

    TotCapLS = HeatingCoil(CoilNum)%MSNominalCapacity(StageNumLS)
    TotCapHS = HeatingCoil(CoilNum)%MSNominalCapacity(StageNumHS)

    EffLS    = HeatingCoil(CoilNum)%MSEfficiency(StageNumLS)
    EffHS    = HeatingCoil(CoilNum)%MSEfficiency(StageNumHS)

    ! Get full load output and power
    LSFullLoadOutAirEnth = InletAirEnthalpy + TotCapLS/MSHPMassFlowRateLow
    HSFullLoadOutAirEnth = InletAirEnthalpy + TotCapHS/MSHPMassFlowRateHigh
    LSElecHeatingPower   = TotCapLS/ EffLS
    HSElecHeatingPower   = TotCapHS/EffHS
    OutletAirHumRat      = InletAirHumRat

    ! if cycling fan, send coil part-load fraction to on/off fan via HVACDataGlobals
    ! IF (FanOpMode .EQ. CycFanCycCoil) OnOffFanPartLoadFraction = 1.0d0

    ! Power calculation
    HeatingCoil(CoilNum)%ElecUseLoad = SpeedRatio*HSElecHeatingPower+(1.0d0-SpeedRatio)*LSElecHeatingPower

    ElecHeatingCoilPower = HeatingCoil(CoilNum)%ElecUseLoad
    HeatingCoil(CoilNum)%HeatingCoilLoad = MSHPMassFlowRateHigh*(HSFullLoadOutAirEnth-InletAirEnthalpy)*SpeedRatio + &
                                               MSHPMassFlowRateLow*(LSFullLoadOutAirEnth-InletAirEnthalpy)*(1.0d0-SpeedRatio)

    OutletAirEnthalpy = InletAirEnthalpy + HeatingCoil(CoilNum)%HeatingCoilLoad/HeatingCoil(CoilNum)%InletAirMassFlowRate
    OutletAirTemp = PsyTdbFnHW(OutletAirEnthalpy,OutletAirHumRat,RoutineName)
    FullLoadOutAirRH = PsyRhFnTdbWPb(OutletAirTemp,OutletAirHumRat,OutdoorPressure,RoutineName//':Averageload')

    IF (FullLoadOutAirRH .gt. 1.d0) THEN  ! Limit to saturated conditions at FullLoadOutAirEnth
      OutletAirTemp = PsyTsatFnHPb(OutletAirEnthalpy,OutdoorPressure,RoutineName)
      OutletAirHumRat = PsyWFnTdbH(OutletAirTemp,OutletAirEnthalpy,RoutineName)
    END IF

    HeatingCoil(CoilNum)%OutletAirTemp     = OutletAirTemp
    HeatingCoil(CoilNum)%OutletAirHumRat   = OutletAirHumRat
    HeatingCoil(CoilNum)%OutletAirEnthalpy = OutletAirEnthalpy
    HeatingCoil(CoilNum)%OutletAirMassFlowRate = HeatingCoil(CoilNum)%InletAirMassFlowRate

  ! Stage 1
  Else If (CycRatio > 0.0d0) Then

    PartLoadRat = MIN(1.0d0,CycRatio)

    ! for cycling fan, reset mass flow to full on rate
    IF (FanOpMode .EQ. CycFanCycCoil) AirMassFlow = AirMassFlow / PartLoadRat
    IF (FanOpMode .EQ. ContFanCycCoil) AirMassFlow = MSHPMassFlowRateLow

    TotCap = HeatingCoil(CoilNum)%MSNominalCapacity(StageNumLS)

    ! Calculate full load outlet conditions
    FullLoadOutAirEnth = InletAirEnthalpy + TotCap/AirMassFlow
    FullLoadOutAirHumRat = InletAirHumRat
    FullLoadOutAirTemp = PsyTdbFnHW(FullLoadOutAirEnth,FullLoadOutAirHumRat,RoutineName)
    FullLoadOutAirRH = PsyRhFnTdbWPb(FullLoadOutAirTemp,FullLoadOutAirHumRat,OutdoorPressure,RoutineName//':fullload')

    IF (FullLoadOutAirRH .gt. 1.d0) THEN  ! Limit to saturated conditions at FullLoadOutAirEnth
      FullLoadOutAirTemp = PsyTsatFnHPb(FullLoadOutAirEnth,OutdoorPressure,RoutineName)
      !  Eventually inlet air conditions will be used in electric Coil, these lines are commented out and marked with this comment line
      !  FullLoadOutAirTemp = PsyTsatFnHPb(FullLoadOutAirEnth,InletAirPressure)
      FullLoadOutAirHumRat  = PsyWFnTdbH(FullLoadOutAirTemp,FullLoadOutAirEnth,RoutineName)
    END IF

    ! Set outlet conditions from the full load calculation
    IF (FanOpMode .EQ. CycFanCycCoil) THEN
      OutletAirEnthalpy = FullLoadOutAirEnth
      OutletAirHumRat   = FullLoadOutAirHumRat
      OutletAirTemp     = FullLoadOutAirTemp
    ELSE
      OutletAirEnthalpy = PartLoadRat * FullLoadOutAirEnth + (1.0d0-PartLoadRat) * InletAirEnthalpy
      OutletAirHumRat   = PartLoadRat * FullLoadOutAirHumRat + (1.0d0-PartLoadRat) * InletAirHumRat
      OutletAirTemp     = PartLoadRat * FullLoadOutAirTemp + (1.0d0-PartLoadRat) * InletAirDryBulbTemp
    END IF

    EffLS = HeatingCoil(CoilNum)%MSEfficiency(StageNumLS)

!    HeatingCoil(CoilNum)%HeatingCoilLoad = TotCap
!   This would require a CR to change
    HeatingCoil(CoilNum)%HeatingCoilLoad = TotCap * PartLoadRat

    HeatingCoil(CoilNum)%ElecUseLoad = HeatingCoil(CoilNum)%HeatingCoilLoad/ EffLS

    ElecHeatingCoilPower = HeatingCoil(CoilNum)%ElecUseLoad

    HeatingCoil(CoilNum)%OutletAirTemp     = OutletAirTemp
    HeatingCoil(CoilNum)%OutletAirHumRat   = OutletAirHumRat
    HeatingCoil(CoilNum)%OutletAirEnthalpy = OutletAirEnthalpy
    HeatingCoil(CoilNum)%OutletAirMassFlowRate = HeatingCoil(CoilNum)%InletAirMassFlowRate
! this would require a CR to correct (i.e., calculate outputs when coil is off)
!  ELSE
!    ! electric coil is off; just pass through conditions
!    HeatingCoil(CoilNum)%OutletAirEnthalpy = HeatingCoil(CoilNum)%InletAirEnthalpy
!    HeatingCoil(CoilNum)%OutletAirHumRat   = HeatingCoil(CoilNum)%InletAirHumRat
!    HeatingCoil(CoilNum)%OutletAirTemp     = HeatingCoil(CoilNum)%InletAirTemp
!    HeatingCoil(CoilNum)%OutletAirMassFlowRate = HeatingCoil(CoilNum)%InletAirMassFlowRate
!
!    HeatingCoil(CoilNum)%ElecUseLoad      = 0.0
!    HeatingCoil(CoilNum)%HeatingCoilLoad  = 0.0
!    ElecHeatingCoilPower                  = 0.0
  End If

ELSE

  ! electric coil is off; just pass through conditions
  HeatingCoil(CoilNum)%OutletAirEnthalpy = HeatingCoil(CoilNum)%InletAirEnthalpy
  HeatingCoil(CoilNum)%OutletAirHumRat   = HeatingCoil(CoilNum)%InletAirHumRat
  HeatingCoil(CoilNum)%OutletAirTemp     = HeatingCoil(CoilNum)%InletAirTemp
  HeatingCoil(CoilNum)%OutletAirMassFlowRate = HeatingCoil(CoilNum)%InletAirMassFlowRate

  ! some of these are reset in Init, can be removed to speed up code
  HeatingCoil(CoilNum)%ElecUseLoad      = 0.0d0
  HeatingCoil(CoilNum)%HeatingCoilLoad  = 0.0d0
  ElecHeatingCoilPower                  = 0.0d0

END IF ! end of on/off if - else

RETURN

END SUBROUTINE CalcMultiStageElectricHeatingCoil


Subroutine CalcGasHeatingCoil(CoilNum,QCoilReq,QCoilActual,FanOpMode,PartLoadRatio)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rich Liesen
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulates a simple Gas heating coil with a burner efficiency

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
      USE DataHVACGlobals, ONLY: TempControlTol
      USE CurveManager,    ONLY : CurveValue
      USE General,         ONLY: TrimSigDigits
      USE DataAirLoop,     ONLY: LoopHeatingCoilMaxRTF

          ! SUBROUTINE ARGUMENT DEFINITIONS:
      INTEGER,   INTENT(IN)  :: CoilNum        ! index to heating coil
      REAL(r64), INTENT(OUT) :: QCoilActual    ! coil load actually delivered (W)
      INTEGER,   INTENT(IN)  :: FanOpMode      ! fan operating mode
      REAL(r64), INTENT(IN)  :: PartLoadRatio  ! part-load ratio of heating coil

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      REAL(r64) AirMassFlow  ! [kg/sec]
      REAL(r64) TempAirIn    ! [C]
      REAL(r64) TempAirOut   ! [C]
      REAL(r64) Win
      REAL(r64) Effic
      REAL(r64) CapacitanceAir
      REAL(r64) HeatingCoilLoad
      REAL(r64) QCoilReq
      REAL(r64) QCoilCap
      REAL(r64) TempSetPoint
      Integer Control
      REAL(r64) PartLoadRat
      REAL(r64) PLF

   Effic        = HeatingCoil(CoilNum)%Efficiency
   TempAirIn    = HeatingCoil(CoilNum)%InletAirTemp
   Win          = HeatingCoil(CoilNum)%InletAirHumRat
   Control      = HeatingCoil(CoilNum)%Control
   TempSetPoint = HeatingCoil(CoilNum)%DesiredOutletTemp
   AirMassFlow  = HeatingCoil(CoilNum)%InletAirMassFlowRate

   CapacitanceAir=PsyCpAirFnWTdb(Win,TempAirIn)*AirMassFlow

  ! If the coil is operating there should be some heating capacitance
  !  across the coil, so do the simulation. If not set outlet to inlet and no load.
  !  Also the coil has to be scheduled to be available.

  ! Control output to meet load QCoilReq (QCoilReq is passed in if load controlled, otherwise QCoilReq=-999)
  IF((AirMassFlow > 0.0d0 .AND. HeatingCoil(CoilNum)%NominalCapacity > 0.0d0) .and. &
     (GetCurrentScheduleValue(HeatingCoil(CoilNum)%SchedPtr) .gt. 0.0d0) .and. &
     (QCoilReq .gt. 0.0d0)) THEN

      !check to see if the Required heating capacity is greater than the user specified capacity.
      IF(QCoilReq > HeatingCoil(CoilNum)%NominalCapacity) Then
        QCoilCap = HeatingCoil(CoilNum)%NominalCapacity
      Else
        QCoilCap = QCoilReq
      End IF

      TempAirOut=TempAirIn + QCoilCap/CapacitanceAir
      HeatingCoilLoad = QCoilCap

      PartLoadRat = HeatingCoilLoad/HeatingCoil(CoilNum)%NominalCapacity

     !The HeatingCoilLoad is the change in the enthalpy of the Heating
      HeatingCoil(CoilNum)%GasUseLoad = HeatingCoilLoad/Effic
      HeatingCoil(CoilNum)%ElecUseLoad = HeatingCoil(CoilNum)%ParasiticElecLoad*PartLoadRat
      HeatingCoil(CoilNum)%ParasiticGasRate = HeatingCoil(CoilNum)%ParasiticGasCapacity * (1.0d0 - PartLoadRat)

   ! Control coil output to meet a setpoint temperature.
   Else IF((AirMassFlow > 0.0d0 .AND. HeatingCoil(CoilNum)%NominalCapacity > 0.0d0) .and. &
     (GetCurrentScheduleValue(HeatingCoil(CoilNum)%SchedPtr) .gt. 0.0d0) .and. &
     (QCoilReq == SensedLoadFlagValue) .and. &
     (ABS(TempSetPoint-TempAirIn) .gt. TempControlTol) ) THEN

      QCoilCap = CapacitanceAir*(TempSetPoint - TempAirIn)
      ! check to see if setpoint above entering temperature. If not, set
      ! output to zero.
      IF(QCoilCap .LE. 0.0d0) THEN
        QCoilCap = 0.0d0
        TempAirOut = TempAirIn
      !check to see if the Required heating capacity is greater than the user
      ! specified capacity.
      Else IF(QCoilCap > HeatingCoil(CoilNum)%NominalCapacity) Then
        QCoilCap = HeatingCoil(CoilNum)%NominalCapacity
        TempAirOut=TempAirIn + QCoilCap/CapacitanceAir
      Else
        TempAirOut = TempSetPoint
      End IF

      HeatingCoilLoad = QCoilCap

      PartLoadRat = HeatingCoilLoad/HeatingCoil(CoilNum)%NominalCapacity

      !The HeatingCoilLoad is the change in the enthalpy of the Heating
      HeatingCoil(CoilNum)%GasUseLoad = HeatingCoilLoad/Effic
      HeatingCoil(CoilNum)%ElecUseLoad = HeatingCoil(CoilNum)%ParasiticElecLoad*PartLoadRat
      HeatingCoil(CoilNum)%ParasiticGasRate = HeatingCoil(CoilNum)%ParasiticGasCapacity * (1.0d0 - PartLoadRat)

  Else ! If not running Conditions do not change across coil from inlet to outlet

      TempAirOut=TempAirIn
      HeatingCoilLoad=0.0d0
      PartLoadRat = 0.0d0
      HeatingCoil(CoilNum)%GasUseLoad = 0.0d0
      HeatingCoil(CoilNum)%ElecUseLoad = 0.0d0
      HeatingCoil(CoilNum)%ParasiticGasRate = HeatingCoil(CoilNum)%ParasiticGasCapacity
  END IF

   HeatingCoil(CoilNum)%RTF = PartLoadRat

  ! If the PLF curve is defined the gas usage needs to be modified
   If(HeatingCoil(CoilNum)%PLFCurveIndex > 0)Then
      IF (PartLoadRat == 0)THEN
        HeatingCoil(CoilNum)%GasUseLoad = 0.0d0
      ELSE
        PLF = CurveValue(HeatingCoil(CoilNum)%PLFCurveIndex, PartLoadRat)
        IF (PLF < 0.7d0) THEN
          IF (HeatingCoil(CoilNum)%PLFErrorCount < 1) THEN
            HeatingCoil(CoilNum)%PLFErrorCount=HeatingCoil(CoilNum)%PLFErrorCount+1
            CALL ShowWarningError('CalcGasHeatingCoil: '//TRIM(cAllCoilTypes(HeatingCoil(CoilNum)%HCoilType_Num))//'="'//  &
               TRIM(HeatingCoil(CoilNum)%Name)//'", PLF curve values')
            CALL ShowContinueError('The PLF curve value = '//TRIM(TrimSigDigits(PLF,5))// &
                                  ' for part-load ratio = '//TRIM(TrimSigDigits(PartLoadRat,5)))
            CALL ShowContinueError('PLF curve values must be >= 0.7. PLF has been reset to 0.7 and the simulation continues...')
            CALL ShowContinueError('Check the IO reference manual for PLF curve guidance [Coil:Heating:Gas].')
          ELSE
            CALL ShowRecurringWarningErrorAtEnd(TRIM(HeatingCoil(CoilNum)%Name)//  &
                        ', Heating coil PLF curve < 0.7 warning continues... ',  &
                        HeatingCoil(CoilNum)%PLFErrorIndex,PLF,PLF)
          END IF
          PLF = 0.7d0
        END IF
        ! Modify the Gas Coil Consumption and parasitic loads based on PLF curve
        HeatingCoil(CoilNum)%RTF = PartLoadRat/PLF
        IF (HeatingCoil(CoilNum)%RTF > 1.0d0 .and. ABS(HeatingCoil(CoilNum)%RTF-1.0d0) > .001d0) THEN
          IF (HeatingCoil(CoilNum)%RTFErrorCount < 1) THEN
            HeatingCoil(CoilNum)%RTFErrorCount=HeatingCoil(CoilNum)%RTFErrorCount+1
            CALL ShowWarningError('CalcGasHeatingCoil: '//TRIM(cAllCoilTypes(HeatingCoil(CoilNum)%HCoilType_Num))//'="'//  &
               TRIM(HeatingCoil(CoilNum)%Name)//'", runtime fraction')
            CALL ShowContinueError('The runtime fraction exceeded 1.0. ['//TRIM(TrimSigDigits(HeatingCoil(CoilNum)%RTF,4))//'].')
            CALL ShowContinueError('Runtime fraction is set to 1.0 and the simulation continues...')
            CALL ShowContinueError('Check the IO reference manual for PLF curve guidance [Coil:Heating:Gas].')
          ELSE
            CALL ShowRecurringWarningErrorAtEnd(TRIM(HeatingCoil(CoilNum)%Name)//  &
                        ', Heating coil runtime fraction > 1.0 warning continues... ',  &
                        HeatingCoil(CoilNum)%RTFErrorIndex,HeatingCoil(CoilNum)%RTF,HeatingCoil(CoilNum)%RTF)
          END IF
          HeatingCoil(CoilNum)%RTF = 1.0d0 ! Reset coil runtime fraction to 1.0
        ELSEIF (HeatingCoil(CoilNum)%RTF > 1.0d0) THEN
          HeatingCoil(CoilNum)%RTF = 1.0d0 ! Reset coil runtime fraction to 1.0
        END IF
        HeatingCoil(CoilNum)%ElecUseLoad = HeatingCoil(CoilNum)%ParasiticElecLoad * HeatingCoil(CoilNum)%RTF
        HeatingCoil(CoilNum)%GasUseLoad  = HeatingCoil(CoilNum)%NominalCapacity / Effic * HeatingCoil(CoilNum)%RTF
        HeatingCoil(CoilNum)%ParasiticGasRate = HeatingCoil(CoilNum)%ParasiticGasCapacity * (1.0d0 - HeatingCoil(CoilNum)%RTF)
        ! Fan power will also be modified by the heating coil's part load fraction
        ! OnOffFanPartLoadFraction passed to fan via DataHVACGlobals (cycling fan only)
        IF(FanOpMode .EQ. CycFanCycCoil)THEN
          OnOffFanPartLoadFraction = PLF
        END IF
      END IF
   END IF

  ! Set the outlet conditions
   HeatingCoil(CoilNum)%HeatingCoilLoad = HeatingCoilLoad
   HeatingCoil(CoilNum)%OutletAirTemp   = TempAirOut

   ! This HeatingCoil does not change the moisture or Mass Flow across the component
   HeatingCoil(CoilNum)%OutletAirHumRat       = HeatingCoil(CoilNum)%InletAirHumRat
   HeatingCoil(CoilNum)%OutletAirMassFlowRate = HeatingCoil(CoilNum)%InletAirMassFlowRate
   !Set the outlet enthalpys for air and Heating
   HeatingCoil(CoilNum)%OutletAirEnthalpy = PsyHFnTdbW(HeatingCoil(CoilNum)%OutletAirTemp, &
                                             HeatingCoil(CoilNum)%OutletAirHumRat)

   QCoilActual = HeatingCoilLoad
   LoopHeatingCoilMaxRTF = MAX(LoopHeatingCoilMaxRTF,HeatingCoil(CoilNum)%RTF)
   ElecHeatingCoilPower = HeatingCoil(CoilNum)%ElecUseLoad

RETURN
END Subroutine CalcGasHeatingCoil

SUBROUTINE CalcMultiStageGasHeatingCoil(CoilNum,SpeedRatio, CycRatio, StageNum, FanOpMode)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma, FSEC
          !       DATE WRITTEN   January 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the air-side performance and energy use of a multi stage gas heating coil.

          ! METHODOLOGY EMPLOYED:
          ! Uses the same methodology as the single speed Gas heating unit model (SUBROUTINE CalcGasHeatingCoil).
          ! In addition it assumes that the unit performance is obtained by interpolating between
          ! the performance at high stage and that at low stage. If the output needed is below
          ! that produced at low stage, the coil cycles between off and low stage.

          ! USE STATEMENTS:
USE CurveManager, ONLY: CurveValue
USE General,      ONLY: TrimSigDigits, RoundSigDigits
USE DataHVACGlobals,     ONLY: MSHPMassFlowRateLow, MSHPMassFlowRateHigh, ElecHeatingCoilPower
USE Psychrometrics,      ONLY: PsyTdbFnHW, PsyRhFnTdbWPb, PsyTsatFnHPb, PsyWFnTdbH
USE DataEnvironment,     ONLY: OutBaroPress

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER   :: CoilNum      ! the number of the Gas heating coil to be simulated
REAL(r64) :: SpeedRatio   ! SpeedRatio varies between 1.0 (maximum speed) and 0.0 (minimum speed)
REAL(r64) :: CycRatio       ! cycling part load ratio
INTEGER   :: StageNum       ! Speed number
INTEGER   :: FanOpMode      ! Fan operation mode

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER :: RoutineName='CalcMultiStageGasHeatingCoil'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64) :: AirMassFlow         ! dry air mass flow rate through coil [kg/s]
REAL(r64) :: InletAirDryBulbTemp ! inlet air dry bulb temperature [C]
REAL(r64) :: InletAirEnthalpy    ! inlet air enthalpy [J/kg]
REAL(r64) :: InletAirHumRat      ! inlet air humidity ratio [kg/kg]
REAL(r64) :: OutletAirEnthalpy   ! outlet air enthalpy [J/kg]
REAL(r64) :: OutletAirHumRat     ! outlet air humidity ratio [kg/kg]
REAL(r64) :: TotCapHS            ! total capacity at high stage [W]
REAL(r64) :: TotCapLS            ! total capacity at low stage [W]
REAL(r64) :: TotCap              ! total capacity at current stage [W]
REAL(r64) :: EffHS               ! efficiency at high stage
REAL(r64) :: EffLS               ! efficiency at low stage
REAL(r64) :: EffAvg              ! average efficiency
REAL(r64) :: OutdoorPressure      ! Outdoor barometric pressure at condenser (Pa)
INTEGER   :: StageNumHS           ! High stage number
INTEGER   :: StageNumLS           ! Low stage number
REAL(r64) :: FullLoadOutAirEnth   ! Outlet full load enthalpy
REAL(r64) :: FullLoadOutAirHumRat ! Outlet humidity ratio at full load
REAL(r64) :: FullLoadOutAirTemp   ! Outlet temperature at full load
REAL(r64) :: FullLoadOutAirRH     ! Outler relative humidity at full load
REAL(r64) :: OutletAirTemp        ! Supply ari temperature
REAL(r64) :: LSFullLoadOutAirEnth ! Outlet full load enthalpy at low stage
REAL(r64) :: HSFullLoadOutAirEnth ! Outlet full load enthalpy at high stage
REAL(r64) :: LSGasHeatingPower    ! Full load power at low stage
REAL(r64) :: HSGasHeatingPower    ! Full load power at high stage
REAL(r64) :: PartLoadRat          ! part load ratio
REAL(r64) :: PLF                  ! part load factor used to calculate RTF

! FLOW
If (StageNum > 1) Then
   StageNumLS = StageNum-1
   StageNumHS = StageNum
  If (StageNum .GT. HeatingCoil(CoilNum)%NumOfStages) Then
    StageNumLS = HeatingCoil(CoilNum)%NumOfStages-1
    StageNumHS = HeatingCoil(CoilNum)%NumOfStages
  End If
Else
  StageNumLS = 1
  StageNumHS = 1
End If

AirMassFlow         = HeatingCoil(CoilNum)%InletAirMassFlowRate
InletAirDryBulbTemp = HeatingCoil(CoilNum)%InletAirTemp
InletAirEnthalpy    = HeatingCoil(CoilNum)%InletAirEnthalpy
InletAirHumRat      = HeatingCoil(CoilNum)%InletAirHumRat

OutdoorPressure = OutBaroPress

IF((AirMassFlow .GT. 0.0d0) .AND. &
   (GetCurrentScheduleValue(HeatingCoil(CoilNum)%SchedPtr) .GT. 0.0d0) .AND. &
   ((CycRatio .GT. 0.0d0) .OR. (SpeedRatio .GT. 0.0d0))) THEN

  If (StageNum > 1) Then

    TotCapLS = HeatingCoil(CoilNum)%MSNominalCapacity(StageNumLS)
    TotCapHS = HeatingCoil(CoilNum)%MSNominalCapacity(StageNumHS)

    EffLS    = HeatingCoil(CoilNum)%MSEfficiency(StageNumLS)
    EffHS    = HeatingCoil(CoilNum)%MSEfficiency(StageNumHS)

    PartLoadRat = MIN(1.0d0,SpeedRatio)
    ! Get full load output and power
    LSFullLoadOutAirEnth = InletAirEnthalpy + TotCapLS/MSHPMassFlowRateLow
    HSFullLoadOutAirEnth = InletAirEnthalpy + TotCapHS/MSHPMassFlowRateHigh
    LSGasHeatingPower   = TotCapLS/ EffLS
    HSGasHeatingPower   = TotCapHS/EffHS
    OutletAirHumRat      = InletAirHumRat

    ! if cycling fan, send coil part-load fraction to on/off fan via HVACDataGlobals
    ! IF (FanOpMode .EQ. CycFanCycCoil) OnOffFanPartLoadFraction = 1.0d0

    ! Power calculation. If PartLoadRat (SpeedRatio) = 0, operate at LS the whole time step
    HeatingCoil(CoilNum)%ElecUseLoad = PartLoadRat*HeatingCoil(CoilNum)%MSParasiticElecLoad(StageNumHS) + &
                                       (1.0d0-PartLoadRat)*HeatingCoil(CoilNum)%MSParasiticElecLoad(StageNumLS)

    ElecHeatingCoilPower = HeatingCoil(CoilNum)%ElecUseLoad
    HeatingCoil(CoilNum)%HeatingCoilLoad = MSHPMassFlowRateHigh*(HSFullLoadOutAirEnth-InletAirEnthalpy)*PartLoadRat + &
                                               MSHPMassFlowRateLow*(LSFullLoadOutAirEnth-InletAirEnthalpy)*(1.0d0-PartLoadRat)
    EffAvg = (EffHS * PartLoadRat) + (EffLS * (1.0d0 - PartLoadRat))
    HeatingCoil(CoilNum)%GasUseLoad      = HeatingCoil(CoilNum)%HeatingCoilLoad / EffAvg
    HeatingCoil(CoilNum)%ParasiticGasRate = 0.0d0

    OutletAirEnthalpy = InletAirEnthalpy + HeatingCoil(CoilNum)%HeatingCoilLoad/HeatingCoil(CoilNum)%InletAirMassFlowRate
    OutletAirTemp = PsyTdbFnHW(OutletAirEnthalpy,OutletAirHumRat,RoutineName)
    FullLoadOutAirRH = PsyRhFnTdbWPb(OutletAirTemp,OutletAirHumRat,OutdoorPressure,RoutineName//':Averageload')

    IF (FullLoadOutAirRH .gt. 1.d0) THEN  ! Limit to saturated conditions at FullLoadOutAirEnth
      OutletAirTemp = PsyTsatFnHPb(OutletAirEnthalpy,OutdoorPressure,RoutineName)
      OutletAirHumRat = PsyWFnTdbH(OutletAirTemp,OutletAirEnthalpy,RoutineName)
    END IF

    HeatingCoil(CoilNum)%OutletAirTemp     = OutletAirTemp
    HeatingCoil(CoilNum)%OutletAirHumRat   = OutletAirHumRat
    HeatingCoil(CoilNum)%OutletAirEnthalpy = OutletAirEnthalpy
    HeatingCoil(CoilNum)%OutletAirMassFlowRate = HeatingCoil(CoilNum)%InletAirMassFlowRate

  ! Stage 1
  Else If (CycRatio > 0.0d0) Then

    ! for cycling fan, reset mass flow to full on rate
    IF (FanOpMode .EQ. CycFanCycCoil) AirMassFlow = AirMassFlow / CycRatio
    IF (FanOpMode .EQ. ContFanCycCoil) AirMassFlow = MSHPMassFlowRateLow

    TotCap = HeatingCoil(CoilNum)%MSNominalCapacity(StageNumLS)

    PartLoadRat = MIN(1.0d0,CycRatio)

    ! Calculate full load outlet conditions
    FullLoadOutAirEnth = InletAirEnthalpy + TotCap/AirMassFlow
    FullLoadOutAirHumRat = InletAirHumRat
    FullLoadOutAirTemp = PsyTdbFnHW(FullLoadOutAirEnth,FullLoadOutAirHumRat,RoutineName)
    FullLoadOutAirRH = PsyRhFnTdbWPb(FullLoadOutAirTemp,FullLoadOutAirHumRat,OutdoorPressure,RoutineName//':fullload')

    IF (FullLoadOutAirRH .gt. 1.d0) THEN  ! Limit to saturated conditions at FullLoadOutAirEnth
      FullLoadOutAirTemp = PsyTsatFnHPb(FullLoadOutAirEnth,OutdoorPressure,RoutineName)
      !  Eventually inlet air conditions will be used in Gas Coil, these lines are commented out and marked with this comment line
      !  FullLoadOutAirTemp = PsyTsatFnHPb(FullLoadOutAirEnth,InletAirPressure)
      FullLoadOutAirHumRat  = PsyWFnTdbH(FullLoadOutAirTemp,FullLoadOutAirEnth,RoutineName)
    END IF

    ! Set outlet conditions from the full load calculation
    IF (FanOpMode .EQ. CycFanCycCoil) THEN
      OutletAirEnthalpy = FullLoadOutAirEnth
      OutletAirHumRat   = FullLoadOutAirHumRat
      OutletAirTemp     = FullLoadOutAirTemp
    ELSE
      OutletAirEnthalpy = PartLoadRat * FullLoadOutAirEnth + (1.0d0-PartLoadRat) * InletAirEnthalpy
      OutletAirHumRat   = PartLoadRat * FullLoadOutAirHumRat + (1.0d0-PartLoadRat) * InletAirHumRat
      OutletAirTemp     = PartLoadRat * FullLoadOutAirTemp + (1.0d0-PartLoadRat) * InletAirDryBulbTemp
    END IF

    EffLS = HeatingCoil(CoilNum)%MSEfficiency(StageNumLS)

!    HeatingCoil(CoilNum)%HeatingCoilLoad = TotCap
!   This would require a CR to change
    HeatingCoil(CoilNum)%HeatingCoilLoad = TotCap * PartLoadRat

    HeatingCoil(CoilNum)%GasUseLoad  = HeatingCoil(CoilNum)%HeatingCoilLoad / EffLS
!   parasitics are calculated when the coil is off (1-PLR)
    HeatingCoil(CoilNum)%ElecUseLoad = HeatingCoil(CoilNum)%MSParasiticElecLoad(StageNumLS) * (1.0d0 - PartLoadRat)
    HeatingCoil(CoilNum)%ParasiticGasRate = HeatingCoil(CoilNum)%ParasiticGasCapacity * (1.0d0 - PartLoadRat)
    ElecHeatingCoilPower = HeatingCoil(CoilNum)%ElecUseLoad

    HeatingCoil(CoilNum)%OutletAirTemp     = OutletAirTemp
    HeatingCoil(CoilNum)%OutletAirHumRat   = OutletAirHumRat
    HeatingCoil(CoilNum)%OutletAirEnthalpy = OutletAirEnthalpy
    HeatingCoil(CoilNum)%OutletAirMassFlowRate = HeatingCoil(CoilNum)%InletAirMassFlowRate
! This seems unecessary (i.e., cycratio or speedratio is > 0) , and would require a CR to change
!  ELSE
!    ! Gas coil is off; just pass through conditions
!    HeatingCoil(CoilNum)%OutletAirEnthalpy = HeatingCoil(CoilNum)%InletAirEnthalpy
!    HeatingCoil(CoilNum)%OutletAirHumRat   = HeatingCoil(CoilNum)%InletAirHumRat
!    HeatingCoil(CoilNum)%OutletAirTemp     = HeatingCoil(CoilNum)%InletAirTemp
!    HeatingCoil(CoilNum)%OutletAirMassFlowRate = HeatingCoil(CoilNum)%InletAirMassFlowRate
!
!    HeatingCoil(CoilNum)%ElecUseLoad      = 0.0
!    HeatingCoil(CoilNum)%HeatingCoilLoad  = 0.0
!    ElecHeatingCoilPower                  = 0.0
  End If

! This requires a CR to correct (i.e., calculate outputs when coil is off)
ELSE

  ! Gas coil is off; just pass through conditions
  HeatingCoil(CoilNum)%OutletAirEnthalpy = HeatingCoil(CoilNum)%InletAirEnthalpy
  HeatingCoil(CoilNum)%OutletAirHumRat   = HeatingCoil(CoilNum)%InletAirHumRat
  HeatingCoil(CoilNum)%OutletAirTemp     = HeatingCoil(CoilNum)%InletAirTemp
  HeatingCoil(CoilNum)%OutletAirMassFlowRate = HeatingCoil(CoilNum)%InletAirMassFlowRate

  ! some of these are reset in Init, can be removed to speed up code
  HeatingCoil(CoilNum)%ElecUseLoad      = 0.0d0
  HeatingCoil(CoilNum)%HeatingCoilLoad  = 0.0d0
  HeatingCoil(CoilNum)%GasUseLoad       = 0.0d0
  HeatingCoil(CoilNum)%ParasiticGasRate = HeatingCoil(CoilNum)%ParasiticGasCapacity
  ElecHeatingCoilPower                  = 0.0d0
  PartLoadRat                           = 0.0d0

END IF ! end of on/off if - else

   ! If the PLF curve is defined the gas usage needs to be modified.
   ! The PLF curve is only used when the coil cycles.
   If(HeatingCoil(CoilNum)%PLFCurveIndex > 0)Then
      IF (PartLoadRat > 0 .AND. StageNum < 2)THEN
        PLF = CurveValue(HeatingCoil(CoilNum)%PLFCurveIndex, PartLoadRat)
        IF (PLF < 0.7d0) THEN
          IF (HeatingCoil(CoilNum)%PLFErrorCount < 1) THEN
            HeatingCoil(CoilNum)%PLFErrorCount=HeatingCoil(CoilNum)%PLFErrorCount+1
            CALL ShowWarningError('CalcGasHeatingCoil: '//TRIM(cAllCoilTypes(HeatingCoil(CoilNum)%HCoilType_Num))//'="'//  &
               TRIM(HeatingCoil(CoilNum)%Name)//'", PLF curve values')
            CALL ShowContinueError('The PLF curve value = '//TRIM(TrimSigDigits(PLF,5))// &
                                  ' for part-load ratio = '//TRIM(TrimSigDigits(PartLoadRat,5)))
            CALL ShowContinueError('PLF curve values must be >= 0.7. PLF has been reset to 0.7 and the simulation continues...')
            CALL ShowContinueError('Check the IO reference manual for PLF curve guidance [Coil:Heating:Gas].')
          ELSE
            CALL ShowRecurringWarningErrorAtEnd(TRIM(HeatingCoil(CoilNum)%Name)//  &
                        ', Heating coil PLF curve < 0.7 warning continues... ',  &
                        HeatingCoil(CoilNum)%PLFErrorIndex,PLF,PLF)
          END IF
          PLF = 0.7d0
        END IF
        ! Modify the Gas Coil Consumption and parasitic loads based on PLF curve
        HeatingCoil(CoilNum)%RTF = PartLoadRat/PLF
        IF (HeatingCoil(CoilNum)%RTF > 1.0d0 .and. ABS(HeatingCoil(CoilNum)%RTF-1.0d0) > .001d0) THEN
          IF (HeatingCoil(CoilNum)%RTFErrorCount < 1) THEN
            HeatingCoil(CoilNum)%RTFErrorCount=HeatingCoil(CoilNum)%RTFErrorCount+1
            CALL ShowWarningError('CalcGasHeatingCoil: '//TRIM(cAllCoilTypes(HeatingCoil(CoilNum)%HCoilType_Num))//'="'//  &
               TRIM(HeatingCoil(CoilNum)%Name)//'", runtime fraction')
            CALL ShowContinueError('The runtime fraction exceeded 1.0. ['//TRIM(TrimSigDigits(HeatingCoil(CoilNum)%RTF,4))//'].')
            CALL ShowContinueError('Runtime fraction is set to 1.0 and the simulation continues...')
            CALL ShowContinueError('Check the IO reference manual for PLF curve guidance [Coil:Heating:Gas].')
          ELSE
            CALL ShowRecurringWarningErrorAtEnd(TRIM(HeatingCoil(CoilNum)%Name)//  &
                        ', Heating coil runtime fraction > 1.0 warning continues... ',  &
                        HeatingCoil(CoilNum)%RTFErrorIndex,HeatingCoil(CoilNum)%RTF,HeatingCoil(CoilNum)%RTF)
          END IF
          HeatingCoil(CoilNum)%RTF = 1.0d0 ! Reset coil runtime fraction to 1.0
        ELSEIF (HeatingCoil(CoilNum)%RTF > 1.0d0) THEN
          HeatingCoil(CoilNum)%RTF = 1.0d0 ! Reset coil runtime fraction to 1.0
        END IF
        HeatingCoil(CoilNum)%ElecUseLoad = HeatingCoil(CoilNum)%MSParasiticElecLoad(StageNum) * HeatingCoil(CoilNum)%RTF
        HeatingCoil(CoilNum)%GasUseLoad  = (HeatingCoil(CoilNum)%MSNominalCapacity(StageNum) / EffLS) * HeatingCoil(CoilNum)%RTF
        HeatingCoil(CoilNum)%ParasiticGasRate = HeatingCoil(CoilNum)%ParasiticGasCapacity * (1.0d0 - HeatingCoil(CoilNum)%RTF)
        ! Fan power will also be modified by the heating coil's part load fraction
        ! OnOffFanPartLoadFraction passed to fan via DataHVACGlobals (cycling fan only)
        IF(FanOpMode .EQ. CycFanCycCoil)THEN
          OnOffFanPartLoadFraction = PLF
        END IF
      END IF
! This requires a CR to correct (i.e., if PLFCurveIndex = 0 do this)
!   ELSE
!     IF(CycRatio > 0.0d0 .AND. StageNum < 2)THEN
!       HeatingCoil(CoilNum)%ElecUseLoad = HeatingCoil(CoilNum)%MSParasiticElecLoad(StageNum) * CycRatio
!       HeatingCoil(CoilNum)%GasUseLoad  = HeatingCoil(CoilNum)%MSNominalCapacity(StageNum) / EffLS * CycRatio
!       HeatingCoil(CoilNum)%ParasiticGasRate = HeatingCoil(CoilNum)%ParasiticGasCapacity * (1.0d0 - CycRatio)
!     END IF
   END IF

RETURN

END SUBROUTINE CalcMultiStageGasHeatingCoil

Subroutine CalcDesuperheaterHeatingCoil(CoilNum,QCoilReq,QCoilActual)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   January 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Simulates a simple desuperheater heating coil with a heat reclaim efficiency
          ! (eff = ratio of condenser waste heat reclaimed to total condenser waste heat rejected)

          ! METHODOLOGY EMPLOYED:
          ! The available capacity of the desuperheater heating coil is determined by the
          ! amount of heat rejected at the heating source condenser multiplied by the
          ! desuperheater heat reclaim efficiency. This capacity is either applied towards
          ! a requested load (load based control) or applied to the air stream to meet a
          ! heating setpoint (temperature based control). This subroutine is similar to
          ! the electric or gas heating coil except that the NominalCapacity is variable
          ! and based on the runtime fraction and heat rejection of the heat source object.

          ! REFERENCES:


          ! USE STATEMENTS:
   USE DataHVACGlobals, ONLY: TempControlTol
   USE DXCoils

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   INTEGER,      INTENT (IN)  :: CoilNum        ! index to desuperheater heating coil
   REAL(r64),    INTENT (IN)  :: QCoilReq       ! load requested by the simulation for load based control [W]
   REAL(r64),    INTENT (OUT) :: QCoilActual    ! coil load actually delivered

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   REAL(r64) AirMassFlow     ! air mass flow through the desuperheater heating coil [kg/sec]
   REAL(r64) AvailTemp       ! Lowest temperature available from desuperheater (~T condensing)[C]
   REAL(r64) TempAirIn       ! temperature of the air entering the desuperheater heating coil [C]
   REAL(r64) TempAirOut      ! temperature of the air leaving the desuperheater heating coil [C]
   REAL(r64) Win             ! humidity ratio of the air entering the desuperheater heating coil [kg/kg]
   REAL(r64) Effic           ! ratio of condenser waste heat reclaimed to total condenser waste heat rejected
   REAL(r64) CapacitanceAir  ! MdotCp of air entering the desuperheater heating coil
   REAL(r64) HeatingCoilLoad ! actual load delivered by the desuperheater heating coil [W]
   REAL(r64) QCoilCap        ! available capacity of the desuperheater heating coil [W]
   REAL(r64) TempSetPoint    ! setpoint temperature to be met when using temperature based control [C]
   INTEGER   SourceID        ! waste heat source id number

   Effic          = HeatingCoil(CoilNum)%Efficiency
   AirMassFlow    = HeatingCoil(CoilNum)%InletAirMassFlowRate
   TempAirIn      = HeatingCoil(CoilNum)%InletAirTemp
   Win            = HeatingCoil(CoilNum)%InletAirHumRat
   CapacitanceAir = PsyCpAirFnWTdb(Win,TempAirIn)*AirMassFlow
   TempSetPoint   = HeatingCoil(CoilNum)%DesiredOutletTemp

   ! Access the appropriate structure to find the available heating capacity of the desuperheater heating coil
   ! The nominal capacity of the desuperheater heating coil varies based on the amount of heat rejected by the source
   ! Stovall 2011, add comparison to available temperature of heat reclaim source
   IF(ValidSourceType(CoilNum))THEN
      SourceID = HeatingCoil(CoilNum)%ReclaimHeatingSourceIndexNum
      IF(HeatingCoil(CoilNum)%ReclaimHeatingSource .EQ. COMPRESSORRACK_REFRIGERATEDCASE)THEN
      !Added last term to available energy equations to avoid double counting reclaimed energy
      ! because refrigeration systems are solved outside the hvac time step iterations
        HeatingCoil(CoilNum)%RTF = 1.0d0
          HeatingCoil(CoilNum)%NominalCapacity = HeatReclaimRefrigeratedRack(SourceID)%AvailCapacity * Effic - &
                  HeatReclaimRefrigeratedRack(SourceID)%UsedWaterHeater
      ELSEIF(HeatingCoil(CoilNum)%ReclaimHeatingSource .EQ. CONDENSER_REFRIGERATION)THEN
         AvailTemp = HeatReclaimRefrigCondenser(SourceID)%AvailTemperature
         HeatingCoil(CoilNum)%RTF = 1.0d0
        IF(AvailTemp .LE. TempAirIn)THEN
          HeatingCoil(CoilNum)%NominalCapacity = 0.d0
          CALL ShowRecurringWarningErrorAtEnd('Coil:Heating:Desuperheater '// &
            TRIM(HeatingCoil(CoilNum)%Name) // &
            ' - Waste heat source temperature was too low to be useful.',&
            HeatingCoil(CoilNum)%InsuffTemperatureWarn)
        ELSE
          HeatingCoil(CoilNum)%NominalCapacity = HeatReclaimRefrigCondenser(SourceID)%AvailCapacity * Effic - &
                  HeatReclaimRefrigCondenser(SourceID)%UsedWaterHeater
        END IF
      ELSEIF(HeatingCoil(CoilNum)%ReclaimHeatingSource .EQ. COIL_DX_COOLING    .OR. &
             HeatingCoil(CoilNum)%ReclaimHeatingSource .EQ. COIL_DX_MULTISPEED .OR. &
             HeatingCoil(CoilNum)%ReclaimHeatingSource .EQ. COIL_DX_MULTIMODE) THEN
        HeatingCoil(CoilNum)%RTF = DXCoil(SourceID)%CoolingCoilRuntimeFraction
        HeatingCoil(CoilNum)%NominalCapacity = HeatReclaimDXCoil(SourceID)%AvailCapacity * Effic
      END IF
   ELSE
      HeatingCoil(CoilNum)%NominalCapacity = 0.0d0
   END IF

  ! Control output to meet load (QCoilReq)
  IF((AirMassFlow .GT. 0.0d0) .and. &
     (GetCurrentScheduleValue(HeatingCoil(CoilNum)%SchedPtr) .gt. 0.0d0) .and. &
     (QCoilReq .gt. 0.0d0)) THEN

      !check to see if the Required heating capacity is greater than the available heating capacity.
      IF(QCoilReq > HeatingCoil(CoilNum)%NominalCapacity) Then
        QCoilCap = HeatingCoil(CoilNum)%NominalCapacity
      Else
        QCoilCap = QCoilReq
      End IF

      ! report the runtime fraction of the desuperheater heating coil
      IF(HeatingCoil(CoilNum)%NominalCapacity .GT. 0.0d0)THEN
        HeatingCoil(CoilNum)%RTF = HeatingCoil(CoilNum)%RTF * (QCoilCap / HeatingCoil(CoilNum)%NominalCapacity)
        TempAirOut               = TempAirIn + QCoilCap/CapacitanceAir
        HeatingCoilLoad          = QCoilCap
      ELSE
        HeatingCoil(CoilNum)%RTF = 0.0d0
        TempAirOut               = TempAirIn
        HeatingCoilLoad          = 0.0d0
      END IF

   ! Control coil output to meet a setpoint temperature.
   Else IF((AirMassFlow .GT. 0.0d0 .AND. HeatingCoil(CoilNum)%NominalCapacity > 0.0d0) .and. &
     (GetCurrentScheduleValue(HeatingCoil(CoilNum)%SchedPtr) .gt. 0.0d0) .and. &
     (QCoilReq == SensedLoadFlagValue) .and. (ABS(TempSetPoint-TempAirIn) .gt. TempControlTol) ) THEN

      QCoilCap     = CapacitanceAir*(TempSetPoint - TempAirIn)
      ! check to see if setpoint is above entering air temperature. If not, set output to zero.
      IF(QCoilCap .LE. 0.0d0) THEN
        QCoilCap   = 0.0d0
        TempAirOut = TempAirIn
      !check to see if the required heating capacity is greater than the available capacity.
      Else IF(QCoilCap > HeatingCoil(CoilNum)%NominalCapacity) Then
        QCoilCap   = HeatingCoil(CoilNum)%NominalCapacity
        TempAirOut = TempAirIn + QCoilCap/CapacitanceAir
      Else
        TempAirOut   = TempSetPoint
      End IF

      HeatingCoilLoad = QCoilCap
!     report the runtime fraction of the desuperheater heating coil
      HeatingCoil(CoilNum)%RTF = HeatingCoil(CoilNum)%RTF * (QCoilCap / HeatingCoil(CoilNum)%NominalCapacity)

  Else ! If not running, conditions do not change across heating coil from inlet to outlet

      TempAirOut                       = TempAirIn
      HeatingCoilLoad                  = 0.0d0
      HeatingCoil(CoilNum)%ElecUseLoad = 0.0d0
      HeatingCoil(CoilNum)%RTF         = 0.0d0

  END IF

  ! Set the outlet conditions
   HeatingCoil(CoilNum)%HeatingCoilLoad       = HeatingCoilLoad
   HeatingCoil(CoilNum)%OutletAirTemp         = TempAirOut

   ! This HeatingCoil does not change the moisture or Mass Flow across the component
   HeatingCoil(CoilNum)%OutletAirHumRat       = HeatingCoil(CoilNum)%InletAirHumRat
   HeatingCoil(CoilNum)%OutletAirMassFlowRate = HeatingCoil(CoilNum)%InletAirMassFlowRate
   !Set the outlet enthalpy
   HeatingCoil(CoilNum)%OutletAirEnthalpy     = PsyHFnTdbW(HeatingCoil(CoilNum)%OutletAirTemp, &
                                                HeatingCoil(CoilNum)%OutletAirHumRat)

   HeatingCoil(CoilNum)%ElecUseLoad           = HeatingCoil(CoilNum)%ParasiticElecLoad*HeatingCoil(CoilNum)%RTF
   QCoilActual                                = HeatingCoilLoad

! Update remaining waste heat (just in case multiple users of waste heat use same source)
   IF(ValidSourceType(CoilNum))THEN
     SourceID = HeatingCoil(CoilNum)%ReclaimHeatingSourceIndexNum
!   Refrigerated cases are simulated at the zone time step, do not decrement available capacity
!   (the heat reclaim available capacity will not get reinitialized as the air loop iterates)
     IF(HeatingCoil(CoilNum)%ReclaimHeatingSource .EQ. COMPRESSORRACK_REFRIGERATEDCASE)THEN
         HeatReclaimRefrigeratedRack(SourceID)%UsedHVACCoil=HeatingCoilLoad
     ELSEIF(HeatingCoil(CoilNum)%ReclaimHeatingSource .EQ. CONDENSER_REFRIGERATION)THEN
         HeatReclaimRefrigCondenser(SourceID)%UsedHVACCoil=HeatingCoilLoad
     ELSEIF(HeatingCoil(CoilNum)%ReclaimHeatingSource .EQ. COIL_DX_COOLING    .OR. &
            HeatingCoil(CoilNum)%ReclaimHeatingSource .EQ. COIL_DX_MULTISPEED .OR. &
            HeatingCoil(CoilNum)%ReclaimHeatingSource .EQ. COIL_DX_MULTIMODE) THEN
        HeatReclaimDXCoil(SourceID)%AvailCapacity = &
          HeatReclaimDXCoil(SourceID)%AvailCapacity - HeatingCoilLoad
     END IF
   END IF

RETURN
END Subroutine CalcDesuperheaterHeatingCoil

! End Algorithm Section of the Module
! *****************************************************************************

! Beginning of Update subroutines for the HeatingCoil Module
! *****************************************************************************

SUBROUTINE UpdateHeatingCoil(CoilNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the coil outlet nodes.

          ! METHODOLOGY EMPLOYED:
          ! Data is moved from the coil data structure to the coil outlet nodes.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataContaminantBalance, ONLY: Contaminant

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(In) :: CoilNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  Integer             :: AirInletNode
  Integer             :: AirOutletNode


   AirInletNode                             = HeatingCoil(CoilNum)%AirInletNodeNum
   AirOutletNode                            = HeatingCoil(CoilNum)%AirOutletNodeNum

   ! Set the outlet air nodes of the HeatingCoil
   Node(AirOutletNode)%MassFlowRate         = HeatingCoil(CoilNum)%OutletAirMassFlowRate
   Node(AirOutletNode)%Temp                 = HeatingCoil(CoilNum)%OutletAirTemp
   Node(AirOutletNode)%HumRat               = HeatingCoil(CoilNum)%OutletAirHumRat
   Node(AirOutletNode)%Enthalpy             = HeatingCoil(CoilNum)%OutletAirEnthalpy

     ! Set the outlet nodes for properties that just pass through & not used
   Node(AirOutletNode)%Quality              = Node(AirInletNode)%Quality
   Node(AirOutletNode)%Press                = Node(AirInletNode)%Press
   Node(AirOutletNode)%MassFlowRateMin      = Node(AirInletNode)%MassFlowRateMin
   Node(AirOutletNode)%MassFlowRateMax      = Node(AirInletNode)%MassFlowRateMax
   Node(AirOutletNode)%MassFlowRateMinAvail = Node(AirInletNode)%MassFlowRateMinAvail
   Node(AirOutletNode)%MassFlowRateMaxAvail = Node(AirInletNode)%MassFlowRateMaxAvail

  IF (Contaminant%CO2Simulation) Then
    Node(AirOutletNode)%CO2 = Node(AirInletNode)%CO2
  End If

  IF (Contaminant%GenericContamSimulation) Then
    Node(AirOutletNode)%GenContam = Node(AirInletNode)%GenContam
  End If

  RETURN
END Subroutine UpdateHeatingCoil

!        End of Update subroutines for the HeatingCoil Module
! *****************************************************************************


! Beginning of Reporting subroutines for the HeatingCoil Module
! *****************************************************************************

SUBROUTINE ReportHeatingCoil(CoilNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the report variable for the coils.

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use DataHVACGlobals, ONLY: TimeStepSys

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: CoilNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   REAL(r64) :: ReportingConstant

   ReportingConstant = TimeStepSys*SecInHour
 ! report the HeatingCoil energy from this component
   HeatingCoil(CoilNum)%HeatingCoilRate  = HeatingCoil(CoilNum)%HeatingCoilLoad
   HeatingCoil(CoilNum)%HeatingCoilLoad  = HeatingCoil(CoilNum)%HeatingCoilLoad*ReportingConstant

   HeatingCoil(CoilNum)%GasUseRate       = HeatingCoil(CoilNum)%GasUseLoad
   HeatingCoil(CoilNum)%ElecUseRate      = HeatingCoil(CoilNum)%ElecUseLoad
   HeatingCoil(CoilNum)%GasUseLoad       = HeatingCoil(CoilNum)%GasUseLoad*ReportingConstant
   HeatingCoil(CoilNum)%ElecUseLoad      = HeatingCoil(CoilNum)%ElecUseLoad*ReportingConstant

   HeatingCoil(CoilNum)%ParasiticGasLoad = HeatingCoil(CoilNum)%ParasiticGasRate*ReportingConstant

  RETURN
END Subroutine ReportHeatingCoil

!        End of Reporting subroutines for the HeatingCoil Module

SUBROUTINE GetCoilIndex(HeatingCoilName,HeatingCoilIndex,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   March 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets an index for a given DX Coil -- issues error message if that
          ! DX Coil is not a legal DX Coil.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItem

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: HeatingCoilName
  INTEGER, INTENT(INOUT)       :: HeatingCoilIndex
  LOGICAL, INTENT(INOUT)       :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
  ! Obtains and Allocates HeatingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetHeatingCoilInput
    GetCoilsInputFlag=.false.
  End If

  HeatingCoilIndex = FindItem(HeatingCoilName,HeatingCoil%Name,NumHeatingCoils)
  IF (HeatingCoilIndex == 0) THEN
    CALL ShowSevereError('GetCoilIndex: Heating coil not found='//TRIM(HeatingCoilName))
    ErrorsFound = .TRUE.
  ENDIF

  RETURN

END SUBROUTINE GetCoilIndex

SUBROUTINE CheckHeatingCoilSchedule(CompType,CompName,Value,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine provides a method for outside routines to check if
          ! the heating coil is scheduled to be on.

          ! METHODOLOGY EMPLOYED:
            ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItem,SameString
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
  INTEGER CoilNum

  ! Obtains and Allocates HeatingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetHeatingCoilInput
    GetCoilsInputFlag=.false.
  End If

  ! Find the correct Coil number
  IF (CompIndex == 0) THEN
    CoilNum = FindItem(CompName,HeatingCoil%Name,NumHeatingCoils)
    IF (CoilNum == 0) THEN
      CALL ShowFatalError('CheckHeatingCoilSchedule: Coil not found="'//TRIM(CompName)//'".')
    ENDIF
    IF (.not. SameString(CompType,cAllCoilTypes(HeatingCoil(CoilNum)%HCoilType_Num))) THEN
      CALL ShowSevereError('CheckHeatingCoilSchedule: Coil="'//trim(CompName)//'"')
      CALL ShowContinueError('...expected type="'//trim(CompType)//'", actual type="'//  &
         trim(cAllCoilTypes(HeatingCoil(CoilNum)%HCoilType_Num))//'".')
      CALL ShowFatalError('Program terminates due to preceding conditions.')
    ENDIF
    CompIndex=CoilNum
    Value=GetCurrentScheduleValue(HeatingCoil(CoilNum)%SchedPtr)  ! not scheduled?
  ELSE
    CoilNum=CompIndex
    IF (CoilNum > NumHeatingCoils .or. CoilNum < 1) THEN
      CALL ShowFatalError('CheckHeatingCoilSchedule: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(CoilNum))// &
                          ', Number of Heating Coils='//TRIM(TrimSigDigits(NumHeatingCoils))//  &
                          ', Coil name='//TRIM(CompName))
    ENDIF
    IF (CompName /= HeatingCoil(CoilNum)%Name) THEN
      CALL ShowSevereError('CheckHeatingCoilSchedule: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(CoilNum))// &
                          ', Coil name='//TRIM(CompName)//', stored Coil Name for that index='//  &
                          TRIM(HeatingCoil(CoilNum)%Name))
      CALL ShowContinueError('...expected type="'//trim(CompType)//'", actual type="'//  &
         trim(cAllCoilTypes(HeatingCoil(CoilNum)%HCoilType_Num))//'".')
      CALL ShowFatalError('Program terminates due to preceding conditions.')
    ENDIF
    Value=GetCurrentScheduleValue(HeatingCoil(CoilNum)%SchedPtr)  ! not scheduled?
  ENDIF

  RETURN

END SUBROUTINE CheckHeatingCoilSchedule

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
  USE InputProcessor,  ONLY: FindItem

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
  INTEGER :: FoundType ! Integer equivalent of coil type

  ! Obtains and Allocates HeatingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetHeatingCoilInput
    GetCoilsInputFlag=.false.
  End If

  FoundType = FindItem(CoilType,cAllCoilTypes,NumAllCoilTypes)
  IF (FoundType == Coil_HeatingElectric .OR. FoundType == Coil_HeatingGas .OR. FoundType == Coil_HeatingDesuperheater) THEN
    WhichCoil=FindItem(CoilName,HeatingCoil%Name,NumHeatingCoils)
    IF (WhichCoil /= 0) THEN
      CoilCapacity=HeatingCoil(WhichCoil)%NominalCapacity
    ENDIF
  ELSE IF (FoundType == Coil_HeatingElectric_MultiStage .OR. FoundType == Coil_HeatingGas_MultiStage) THEN
    WhichCoil=FindItem(CoilName,HeatingCoil%Name,NumHeatingCoils)
    IF (WhichCoil /= 0) THEN
      CoilCapacity=HeatingCoil(WhichCoil)%MSNominalCapacity(HeatingCoil(WhichCoil)%NumOfStages)
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN !Objexx:Return Reworked block to assure CoilCapacity is set before return
    IF (FoundType == 0) THEN
      CALL ShowSevereError('GetCoilCapacity: Could not find Coil, Type="'//TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    ELSEIF (FoundType > 0) THEN
      CALL ShowSevereError('GetCoilCapacity: Invalid coil type for capacity, Type="'//TRIM(CoilType)//  &
         '" Name="'//TRIM(CoilName)//'"')
      CALL ShowContinueError('...only '//trim(cAllCoilTypes(Coil_HeatingElectric))//', '//  &
         trim(cAllCoilTypes(Coil_HeatingGas))//' or '//trim(cAllCoilTypes(Coil_HeatingDesuperheater))//  &
         ' are valid in this context.')
    ENDIF
    CALL ShowContinueError('... returning Coil Capacity as -1000.')
    ErrorsFound=.true.
    CoilCapacity=-1000.0d0
  ENDIF

  RETURN

END FUNCTION GetCoilCapacity

FUNCTION GetCoilAvailScheduleIndex(CoilType,CoilName,ErrorsFound) RESULT(AvailSchIndex)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the availability schedule index.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and index is returned
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
  INTEGER                      :: AvailSchIndex   ! returned availability schedule of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil
  INTEGER :: FoundType ! Integer equivalent of coil type

  ! Obtains and Allocates HeatingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetHeatingCoilInput
    GetCoilsInputFlag=.false.
  End If

  WhichCoil=0
  AvailSchIndex=0
  FoundType = FindItem(CoilType,cAllCoilTypes,NumAllCoilTypes)
  IF (FoundType == Coil_HeatingElectric .OR. FoundType == Coil_HeatingElectric_MultiStage .OR. &
      FoundType == Coil_HeatingGas .OR. FoundType == Coil_HeatingGas_MultiStage .OR. &
      FoundType == Coil_HeatingDesuperheater) THEN
    WhichCoil=FindItem(CoilName,HeatingCoil%Name,NumHeatingCoils)
    IF (WhichCoil /= 0) THEN
      AvailSchIndex=HeatingCoil(WhichCoil)%SchedPtr
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('GetCoilAvailScheduleIndex: Could not find Coil, Type="'// &
                          TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    AvailSchIndex=0
  ENDIF

  RETURN

END FUNCTION GetCoilAvailScheduleIndex

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
  INTEGER :: FoundType ! Integer equivalent of coil type

  ! Obtains and Allocates HeatingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetHeatingCoilInput
    GetCoilsInputFlag=.false.
  End If

  WhichCoil=0
  NodeNumber=0
  FoundType = FindItem(CoilType,cAllCoilTypes,NumAllCoilTypes)
  IF (FoundType == Coil_HeatingElectric .OR. FoundType == Coil_HeatingElectric_MultiStage .OR. &
      FoundType == Coil_HeatingGas .OR. FoundType == Coil_HeatingGas_MultiStage .OR. &
      FoundType == Coil_HeatingDesuperheater) THEN
    WhichCoil=FindItem(CoilName,HeatingCoil%Name,NumHeatingCoils)
    IF (WhichCoil /= 0) THEN
      NodeNumber=HeatingCoil(WhichCoil)%AirInletNodeNum
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('GetCoilInletNode: Could not find Coil, Type="'//TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    NodeNumber=0
  ENDIF

  RETURN

END FUNCTION GetCoilInletNode

FUNCTION GetCoilOutletNode(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
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
  INTEGER :: FoundType ! Integer equivalent of coil type

  ! Obtains and Allocates HeatingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetHeatingCoilInput
    GetCoilsInputFlag=.false.
  End If

  WhichCoil=0
  NodeNumber=0
  FoundType = FindItem(CoilType,cAllCoilTypes,NumAllCoilTypes)
  IF (FoundType == Coil_HeatingElectric .OR. FoundType == Coil_HeatingElectric_MultiStage .OR. &
      FoundType == Coil_HeatingGas .OR. FoundType == Coil_HeatingGas_MultiStage .OR. &
      FoundType == Coil_HeatingDesuperheater) THEN
    WhichCoil=FindItem(CoilName,HeatingCoil%Name,NumHeatingCoils)
    IF (WhichCoil /= 0) THEN
      NodeNumber=HeatingCoil(WhichCoil)%AirOutletNodeNum
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('GetCoilOutletNode: Could not find Coil, Type="'//TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    NodeNumber=0
  ENDIF

  RETURN

END FUNCTION GetCoilOutletNode

FUNCTION GetHeatReclaimSourceIndex(CoilType,CoilName,ErrorsFound) RESULT(CoilFound)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   June 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the heating coil index number if it is a desuperheating coil.
          ! If incorrect coil type or name is given, errorsfound is returned as true and index number is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList, SameString

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType     ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName     ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: CoilFound    ! returned index number of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: GetCoilErrFlag
  LOGICAL :: SuppressWarning
  INTEGER :: NumCoil
  INTEGER :: CoilNum


  ! Obtains and Allocates HeatingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetHeatingCoilInput
    GetCoilsInputFlag=.false.
  End If

  SuppressWarning = .TRUE.
  CoilFound = 0

 !This function only used for dessicant regeneration and refrigeration desuperheat not a valid source
 !IF (SameString(CoilType,'REFRIGERATION:COMPRESSORRACK')) THEN
 !    CALL GetRefrigeratedRackIndex(CoilName, CoilNum,RefrigSystemTypeRack, GetCoilErrFlag, CoilType, SuppressWarning)
 !    DO NumCoil = 1, NumHeatingCoils
 !      IF(HeatingCoil(NumCoil)%ReclaimHeatingSource .NE. COMPRESSORRACK_REFRIGERATEDCASE .AND. &
 !         HeatingCoil(NumCoil)%ReclaimHeatingCoilName .NE. CoilName)CYCLE
 !      CoilFound = CoilNum
 !      EXIT
 !    END DO
 ! ELSEIF (SameString(CoilType,'REFRIGERATION:CONDENSER')) THEN   bbb
 !    CALL GetRefrigeratedRackIndex(CoilName, CoilNum,RefrigSystemTypeDetailed, GetCoilErrFlag, CoilType, SuppressWarning)
 !    DO NumCoil = 1, NumHeatingCoils
 !      IF(HeatingCoil(NumCoil)%ReclaimHeatingSource .NE. CONDENSER_REFRIGERATION .AND. &
 !         HeatingCoil(NumCoil)%ReclaimHeatingCoilName .NE. CoilName)CYCLE
 !      CoilFound = CoilNum
 !      EXIT
 !    END DO
 ! ELSEIF
 !note should eventually get rid of this string comparison
 IF(SameString(CoilType,'COIL:COOLING:DX:SINGLESPEED') .or.  &
          SameString(CoilType,'COIL:COOLING:DX:TWOSPEED') .or.   &
          SameString(CoilType,'COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE')) THEN
     CALL GetDXCoilIndex(CoilName,CoilNum, GetCoilErrFlag, CoilType, SuppressWarning)
     DO NumCoil = 1, NumHeatingCoils
       IF(HeatingCoil(NumCoil)%ReclaimHeatingSource .NE. COIL_DX_COOLING .AND. &
          HeatingCoil(NumCoil)%ReclaimHeatingSource .NE. COIL_DX_MULTISPEED .AND. &
          HeatingCoil(NumCoil)%ReclaimHeatingSource .NE. COIL_DX_MULTIMODE .AND. &
          HeatingCoil(NumCoil)%ReclaimHeatingCoilName .NE. CoilName)CYCLE
       CoilFound = CoilNum
       EXIT
     END DO
  ENDIF

  IF (CoilNum == 0) THEN
    ErrorsFound=.true.
  ENDIF

  RETURN

END FUNCTION GetHeatReclaimSourceIndex

FUNCTION GetCoilControlNodeNum(CoilType,CoilName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   June 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the control node number.  If
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
  INTEGER :: FoundType ! Integer equivalent of coil type

  ! Obtains and Allocates HeatingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetHeatingCoilInput
    GetCoilsInputFlag=.false.
  End If

  WhichCoil=0
  NodeNumber=0
  FoundType = FindItem(CoilType,cAllCoilTypes,NumAllCoilTypes)
  IF (FoundType == Coil_HeatingElectric .OR. FoundType == Coil_HeatingElectric_MultiStage .OR. &
      FoundType == Coil_HeatingGas .OR. FoundType == Coil_HeatingGas_MultiStage .OR. &
      FoundType == Coil_HeatingDesuperheater) THEN
    WhichCoil=FindItem(CoilName,HeatingCoil%Name,NumHeatingCoils)
    IF (WhichCoil /= 0) THEN
      NodeNumber=HeatingCoil(WhichCoil)%TempSetPointNodeNum
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('GetCoilControlNodeNum: Could not find Coil, Type="'//TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    NodeNumber=0
  ENDIF

  RETURN

END FUNCTION GetCoilControlNodeNum

FUNCTION GetHeatingCoilTypeNum(CoilType,CoilName,ErrorsFound) RESULT(TypeNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   August 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the type number.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and type number is returned
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
  INTEGER                      :: TypeNum      ! returned type number of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil
  INTEGER :: FoundType ! Integer equivalent of coil type

  ! Obtains and Allocates HeatingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetHeatingCoilInput
    GetCoilsInputFlag=.false.
  End If

  WhichCoil=0
  TypeNum=0
  FoundType = FindItem(CoilType,cAllCoilTypes,NumAllCoilTypes)
  IF (FoundType == Coil_HeatingElectric .OR. FoundType == Coil_HeatingElectric_MultiStage .OR. &
      FoundType == Coil_HeatingGas .OR. FoundType == Coil_HeatingGas_MultiStage .OR. &
      FoundType == Coil_HeatingDesuperheater) THEN
    WhichCoil=FindItem(CoilName,HeatingCoil%Name,NumHeatingCoils)
    IF (WhichCoil /= 0) THEN
      TypeNum=HeatingCoil(WhichCoil)%HCoilType_Num
    ENDIF
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('GetHeatingCoilTypeNum: Could not find Coil, Type="'//TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    TypeNum=0
  ENDIF

  RETURN

END FUNCTION GetHeatingCoilTypeNum

FUNCTION GetHeatingCoilIndex(CoilType,CoilName,ErrorsFound) RESULT(WhichCoil)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the index into the structure.  If
          ! incorrect coil type or name is given, errorsfound is returned as true and index is returned
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
  INTEGER                      :: WhichCoil    ! returned index number of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: FoundType ! Integer equivalent of coil type

  ! Obtains and Allocates HeatingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetHeatingCoilInput
    GetCoilsInputFlag=.false.
  End If

  WhichCoil=0
  FoundType = FindItem(CoilType,cAllCoilTypes,NumAllCoilTypes)
  IF (FoundType == Coil_HeatingElectric .OR. FoundType == Coil_HeatingElectric_MultiStage .OR. &
      FoundType == Coil_HeatingGas .OR. FoundType == Coil_HeatingGas_MultiStage .OR. &
      FoundType == Coil_HeatingDesuperheater) THEN
    WhichCoil=FindItem(CoilName,HeatingCoil%Name,NumHeatingCoils)
  ELSE
    WhichCoil=0
  ENDIF

  IF (WhichCoil == 0) THEN
    CALL ShowSevereError('GetHeatingCoilIndex: Could not find Coil, Type="'//TRIM(CoilType)//'" Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
  ENDIF

  RETURN

END FUNCTION GetHeatingCoilIndex

FUNCTION GetHeatingCoilPLFCurveIndex(CoilType,CoilName,ErrorsFound) RESULT(IndexNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   December 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the PLF curve index.  If
          ! incorrect coil name is given for gas or electric heating coils, errorsfound
          ! is returned as true and curve index is returned as zero.
          ! If not a gas or electric heating coil, errorsfound is unchanged and index is 0.

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
  INTEGER                      :: IndexNum     ! returned PLF curve index of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil
  INTEGER :: FoundType ! Integer equivalent of coil type

  ! Obtains and Allocates HeatingCoil related parameters from input file
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetHeatingCoilInput
    GetCoilsInputFlag=.false.
  End If

  FoundType = FindItem(CoilType,cAllCoilTypes,NumAllCoilTypes)
  IF (FoundType == Coil_HeatingElectric .OR. FoundType == Coil_HeatingElectric_MultiStage .OR. &
      FoundType == Coil_HeatingGas .OR. FoundType == Coil_HeatingGas_MultiStage .OR. &
      FoundType == Coil_HeatingDesuperheater) THEN
    WhichCoil=FindItem(CoilName,HeatingCoil%Name,NumHeatingCoils)
    IF (WhichCoil /= 0) THEN
      IndexNum=HeatingCoil(WhichCoil)%PLFCurveIndex
    ELSE
      CALL ShowSevereError('GetHeatingCoilPLFCurveIndex: Could not find Coil, Type="'//TRIM(CoilType)//  &
         '" Name="'//TRIM(CoilName)//'"')
      ErrorsFound=.TRUE.
      IndexNum=0
    ENDIF
  ELSE
    IndexNum=0
  ENDIF

  RETURN

END FUNCTION GetHeatingCoilPLFCurveIndex

FUNCTION GetHeatingCoilNumberOfStages(CoilType,CoilName,ErrorsFound) RESULT(NumberOfStages)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   February 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given coil and returns the number of speeds for multistage coils.
          ! If incorrect coil type or name is given, errorsfound is returned as true.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: CoilType       ! must match coil types in this module
  CHARACTER(len=*), INTENT(IN) :: CoilName       ! must match coil names for the coil type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound    ! set to true if problem
  INTEGER                      :: NumberOfStages ! returned the number of speed of matched coil

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichCoil

  ! Obtains and Allocates HeatingCoils
  IF (GetCoilsInputFlag) THEN  !First time subroutine has been entered
    CALL GetHeatingCoilInput
    GetCoilsInputFlag=.false.
  End If

  WhichCoil=FindItemInList(CoilName,HeatingCoil%Name,NumHeatingCoils)
  IF (WhichCoil /= 0) THEN
    NumberOfStages=HeatingCoil(WhichCoil)%NumOfStages
   ELSE
    CALL ShowSevereError('GetHeatingCoilNumberOfSpeeds: Invalid Heating Coil Type="'//TRIM(CoilType) &
                          //'" Name="'//TRIM(CoilName)//'"')
    ErrorsFound=.true.
    NumberOfStages=0
  ENDIF

  RETURN

END FUNCTION GetHeatingCoilNumberOfStages

!        End of Utility subroutines for the HeatingCoil Module

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

End Module HeatingCoils

