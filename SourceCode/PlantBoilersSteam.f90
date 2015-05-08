MODULE BoilerSteam

   ! Module containing the routines dealing with the Boilers

   ! MODULE INFORMATION:
   !    AUTHOR         Rahul Chillar
   !    DATE WRITTEN   Dec 2004
   !    MODIFIED       na
   !    RE-ENGINEERED  na
   ! PURPOSE OF THIS MODULE:
   ! Performs steam boiler simulation for plant simulation

   ! METHODOLOGY EMPLOYED:
   ! The steam boiler based on

   ! REFERENCES:
   ! none

   ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataGlobals,     ONLY: MaxNameLength, BeginEnvrnFlag, SecInHour
  USE DataInterfaces
  USE DataLoopNode
  USE DataHVACGlobals
  USE General,         ONLY: TrimSigDigits
  USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance

  IMPLICIT NONE

  PRIVATE ! All private unless defined public

   ! MODULE PARAMETER DEFINITIONS
   ! na

   ! DERIVED TYPE DEFINITIONS
  TYPE BoilerSpecs
     CHARACTER(len=MaxNameLength) :: Name     =' '      ! user identifier
     INTEGER           :: FuelType            =0        ! resource type
     LOGICAL           :: Available           =.false.  ! TRUE if machine available in current time step
     LOGICAL           :: ON                  =.false.  ! TRUE: simulate the machine at it's operating part load ratio
     LOGICAL           :: MissingSetpointErrDone = .FALSE. ! Missing outlet node setpoint message flag
     LOGICAL           :: UseLoopSetpoint        = .FALSE. ! Flag to use setpoint from loop
     REAL(r64)         :: DesMassFlowRate     =0.0d0      ! kg/s - Boiler water design mass flow rate
     REAL(r64)         :: MassFlowRate        =0.0d0      ! kg/s - Boiler water mass flow rate
     REAL(r64)         :: NomCap              =0.0d0      ! W - design nominal capacity of Boiler
     REAL(r64)         :: Effic               =0.0d0      ! boiler efficiency at design conditions
!       REAL(r64)         :: TempDesBoilerOut    =0.0d0      ! C - Boiler design outlet temperature
     REAL(r64)         :: MinPartLoadRat      =0.0d0      ! Minimum allowed operating part load ratio
     REAL(r64)         :: MaxPartLoadRat      =0.0d0      ! Maximum allowed operating part load ratio
     REAL(r64)         :: OptPartLoadRat      =0.0d0      ! Optimal operating part load ratio
     REAL(r64)         :: OperPartLoadRat     =0.0d0      ! Actual operating part load ratio
     REAL(r64)         :: TempUpLimitBoilerOut=0.0d0      ! C - Boiler outlet maximum temperature limit
     REAL(r64)         :: BoilerMaxOperPress  =0.0d0      ! Max Boiler Pressure
     REAL(r64)         :: BoilerPressCheck    =0.0d0      ! Boiler Operating Pressure at Saturation Temperature
     REAL(r64)         :: SizFac              =0.0d0      ! sizing factor
     INTEGER           :: BoilerInletNodeNum  =0        ! Node number at the boiler inlet
     INTEGER           :: BoilerOutletNodeNum =0        ! Node number at the boiler outlet
     REAL(r64),DIMENSION(3) :: FullLoadCoef   =0.0d0      ! Coefficients of the fuel consumption/part load ratio curve
     INTEGER           :: TypeNum             =0        ! Plant loop type identifier
     INTEGER           :: LoopNum             =0        ! Plant loop index number
     INTEGER           :: LoopSideNum         =0        ! Loop side index number
     INTEGER           :: BranchNum           =0        ! Branch index number
     INTEGER           :: CompNum             =0        ! Plant loop component index number
     INTEGER           :: PressErrIndex       =0   ! index pointer for recurring errors
     INTEGER           :: FluidIndex          =0   ! Steam index
  END TYPE BoilerSpecs

  TYPE ReportVars
      REAL(r64)    ::  BoilerLoad           = 0.0d0 ! W - Boiler operating load
      REAL(r64)    ::  BoilerEnergy         = 0.0d0 ! J - Boiler energy integrated over time
      REAL(r64)    ::  FuelUsed             = 0.0d0 ! W - Boiler fuel used
      REAL(r64)    ::  FuelConsumed         = 0.0d0 ! J - Boiler Fuel consumed integrated over time
      REAL(r64)    ::  BoilerInletTemp      = 0.0d0 ! C - Boiler inlet temperature
      REAL(r64)    ::  BoilerOutletTemp     = 0.0d0 ! C - Boiler outlet temperature
      REAL(r64)    ::  Mdot                 = 0.0d0 ! kg/s - Boiler mass flow rate
      REAL(r64)    ::  BoilerMaxOperPress   = 0.0d0 !
  END TYPE ReportVars

  ! MODULE VARIABLE DECLARATIONS:
  REAL(r64)    :: FuelUsed            =0.0d0      ! W - Boiler fuel used
  REAL(r64)    :: BoilerLoad          =0.0d0      ! W - Boiler Load
  REAL(r64)    :: BoilerMassFlowRate  =0.0d0      ! kg/s - Boiler mass flow rate
  REAL(r64)    :: BoilerOutletTemp    =0.0d0      ! W - Boiler outlet temperature
  REAL(r64)    :: BoilerMaxPress      =0.0d0      !
  INTEGER      :: NumBoilers          =0        ! Number of boilers
  REAL(r64)    :: BoilerMassFlowMaxAvail=0.0d0        ! kg/s - Boiler mass flow rate
  REAL(r64)    :: BoilerMassFlowMinAvail=0.0d0        ! kg/s - Boiler mass flow rate


  TYPE(BoilerSpecs), ALLOCATABLE, DIMENSION(:)  :: Boiler         !dimension to number of machines
  TYPE(ReportVars) , ALLOCATABLE, DIMENSION(:)   ::BoilerReport   !
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

   ! SUBROUTINE SPECIFICATIONS FOR MODULE Boilers
  PRIVATE    CalcBoilerModel
  PRIVATE    GetBoilerInput
  PRIVATE    InitBoiler
  PRIVATE    SizeBoiler
  PRIVATE    UpdateBoilerRecords
  PUBLIC     SimSteamBoiler

  CONTAINS
  ! MODULE SUBROUTINES:



    ! Beginning of Boiler Module Driver Subroutines
SUBROUTINE SimSteamBoiler(BoilerType,BoilerName,EquipFlowCtrl, CompIndex, RunFlag,FirstHVACIteration, &
                          InitLoopEquip,MyLoad,MaxCap,MinCap,OptCap,GetSizingFactor,SizingFactor)
  ! SUBROUTINE INFORMATION:
  !       AUTHOR         Rahul Chillar
  !       DATE WRITTEN
  !       MODIFIED
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS SUBROUTINE:
  ! This subrountine controls the boiler component simulation

  ! METHODOLOGY EMPLOYED: na

  ! REFERENCES: na

  ! USE STATEMENTS:
  USE InputProcessor,    ONLY: FindItemInList
  USE FluidProperties

  IMPLICIT NONE

  ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)  :: BoilerType      ! boiler type (used in CASE statement)
  CHARACTER(len=*), INTENT(IN)  :: BoilerName      ! boiler identifier
  INTEGER, INTENT(IN)           :: EquipFlowCtrl   ! Flow control mode for the equipment
  INTEGER, INTENT(INOUT)        :: CompIndex       ! boiler counter/identifier
  LOGICAL, INTENT(IN)           :: RunFlag         ! if TRUE run boiler simulation--boiler is ON
  LOGICAL , INTENT(IN)          :: FirstHVACIteration ! TRUE if First iteration of simulation
  LOGICAL, INTENT(INOUT)        :: InitLoopEquip   ! If not zero, calculate the max load for operating conditions
  REAL(r64), INTENT(INOUT)      :: MyLoad          ! W - Actual demand boiler must satisfy--calculated by load dist. routine
  REAL(r64)                     :: MinCap          ! W - minimum boiler operating capacity
  REAL(r64)                     :: MaxCap          ! W - maximum boiler operating capacity
  REAL(r64)                     :: OptCap          ! W - optimal boiler operating capacity
  LOGICAL, INTENT(IN)           :: GetSizingFactor ! TRUE when just the sizing factor is requested
  REAL(r64), INTENT(OUT)        :: SizingFactor    ! sizing factor

  ! SUBROUTINE PARAMETER DEFINITIONS:
  ! na

  ! DERIVED TYPE DEFINITIONS
  ! na

  ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE :: GetInput = .TRUE.        ! if TRUE read user input
  INTEGER       :: BoilerNum       ! boiler counter/identifier

  !Get Input
  IF (GetInput) THEN
    CALL GetBoilerInput
    GetInput=.false.
  END IF

  ! Find the correct Equipment
  IF (CompIndex == 0) THEN
    BoilerNum = FindItemInList(BoilerName,Boiler%Name,NumBoilers)
    IF (BoilerNum == 0) THEN
      CALL ShowFatalError('SimBoiler: Unit not found='//TRIM(BoilerName))
    ENDIF
    CompIndex=BoilerNum
  ELSE
    BoilerNum=CompIndex
    IF (BoilerNum > NumBoilers .or. BoilerNum < 1) THEN
      CALL ShowFatalError('SimBoiler:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(BoilerNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumBoilers))//  &
                          ', Entered Unit name='//TRIM(BoilerName))
    ENDIF
    IF (CheckEquipName(BoilerNum)) THEN
      IF (BoilerName /= Boiler(BoilerNum)%Name) THEN
        CALL ShowFatalError('SimBoiler: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(BoilerNum))// &
                            ', Unit name='//TRIM(BoilerName)//', stored Unit Name for that index='//  &
                            TRIM(Boiler(BoilerNum)%Name))
      ENDIF
      CheckEquipName(BoilerNum)=.false.
    ENDIF
  ENDIF

  ! Initialize Loop Equipment
  IF (InitLoopEquip) THEN
    CALL InitBoiler(BoilerNum)
    CALL SizeBoiler(BoilerNum)
    MinCap = Boiler(BoilerNum)%NomCap*Boiler(BoilerNum)%MinPartLoadRat
    MaxCap = Boiler(BoilerNum)%NomCap*Boiler(BoilerNum)%MaxPartLoadRat
    OptCap = Boiler(BoilerNum)%NomCap*Boiler(BoilerNum)%OptPartLoadRat
    IF (GetSizingFactor) THEN
      SizingFactor = Boiler(BoilerNum)%SizFac
    ENDIF
    RETURN
  END IF

  !Calculate Load
  !Select boiler type and call boiler model
  CALL InitBoiler(BoilerNum)
  CALL CalcBoilerModel(BoilerNum,MyLoad,Runflag,EquipFlowCtrl)
  CALL UpdateBoilerRecords(MyLoad,RunFlag,BoilerNum,FirstHVACIteration)

RETURN
END SUBROUTINE SimSteamBoiler

SUBROUTINE GetBoilerInput
    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Rahul Chillar
    !       DATE WRITTEN   Dec 2004
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! Get all boiler data from input file

    ! METHODOLOGY EMPLOYED:
    ! standard EnergyPlus input retrieval using input Processor

    ! REFERENCES: na

    ! USE STATEMENTS:
    USE DataGlobalConstants
    USE InputProcessor,     ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString
    USE DataIPShortCuts  ! Data for field names, blank numerics
    USE BranchNodeConnections, ONLY: TestCompSet
    USE NodeInputManager,   ONLY: GetOnlySingleNode
    USE FluidProperties,    ONLY: FindRefrigerant
    USE GlobalNames,        ONLY: VerifyUniqueBoilerName
    USE General,            ONLY: RoundSigDigits

    IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

    ! PARAMETERS
    CHARACTER(len=*), PARAMETER :: RoutineName='GetBoilerInput: '

    !LOCAL VARIABLES
    INTEGER                     :: BoilerNum ! boiler identifier
    INTEGER                     :: NumAlphas ! Number of elements in the alpha array
    INTEGER                     :: NumNums   ! Number of elements in the numeric array
    INTEGER                     :: IOStat    ! IO Status when calling get input subroutine
    LOGICAL                     :: IsNotOK   ! Flag to verify name
    LOGICAL                     :: IsBlank   ! Flag for blank name
    INTEGER                     :: SteamFluidIndex  ! Fluid Index for Steam
    LOGICAL, SAVE               :: ErrorsFound=.false.
    LOGICAL                     :: errflag
    CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: BoilerFuelTypeForOutputVariable  ! used to set up report variables

    SteamFluidIndex=0
    cCurrentModuleObject = 'Boiler:Steam'
    NumBoilers = GetNumObjectsFound(cCurrentModuleObject)

    IF (NumBoilers<=0) THEN
      CALL ShowSevereError('No '//TRIM(cCurrentModuleObject)//' equipment specified in input file')
      ErrorsFound=.true.
    END IF

    !See if load distribution manager has already gotten the input
    IF (ALLOCATED(Boiler))RETURN

    ! Boiler will have fuel input to it , that is it !
    ALLOCATE (Boiler(NumBoilers))
    ALLOCATE(CheckEquipName(NumBoilers))
    CheckEquipName=.true.
    ALLOCATE(BoilerFuelTypeForOutputVariable(NumBoilers))
    BoilerFuelTypeForOutputVariable=' '

    ALLOCATE (BoilerReport(NumBoilers))


         !LOAD ARRAYS WITH CURVE FIT Boiler DATA
    DO BoilerNum = 1 , NumBoilers
       CALL GetObjectItem(cCurrentModuleObject,BoilerNum,cAlphaArgs,NumAlphas, &
                               rNumericArgs,NumNums,IOSTAT,AlphaFieldnames=cAlphaFieldNames, &
                               NumericFieldNames=cNumericFieldNames)

       IsNotOK=.false.
       IsBlank=.false.
       CALL VerifyName(cAlphaArgs(1),Boiler%Name,BoilerNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
       IF (IsNotOK) THEN
         ErrorsFound=.true.
         IF (IsBlank) cAlphaArgs(1)='xxxxx'
       ENDIF
       CALL VerifyUniqueBoilerName(TRIM(cCurrentModuleObject),cAlphaArgs(1),errflag,TRIM(cCurrentModuleObject)//' Name')
       IF (errflag) THEN
         ErrorsFound=.true.
       ENDIF
       Boiler(BoilerNum)%Name                = cAlphaArgs(1)

       SELECT CASE (cAlphaArgs(2))

       CASE ('ELECTRICITY','ELECTRIC','ELEC')
          BoilerFuelTypeForOutputVariable(BoilerNum) = 'Electric'
          Boiler(BoilerNum)%FuelType=AssignResourceTypeNum('ELECTRICITY')

       CASE ('GAS','NATURALGAS','NATURAL GAS')
          BoilerFuelTypeForOutputVariable(BoilerNum) = 'Gas'
          Boiler(BoilerNum)%FuelType=AssignResourceTypeNum('NATURALGAS')

       CASE ('DIESEL')
          BoilerFuelTypeForOutputVariable(BoilerNum) = 'Diesel'
          Boiler(BoilerNum)%FuelType=AssignResourceTypeNum('DIESEL')

       CASE ('GASOLINE')
          BoilerFuelTypeForOutputVariable(BoilerNum) = 'Gasoline'
          Boiler(BoilerNum)%FuelType=AssignResourceTypeNum('GASOLINE')

       CASE ('COAL')
          BoilerFuelTypeForOutputVariable(BoilerNum) = 'Coal'
          Boiler(BoilerNum)%FuelType=AssignResourceTypeNum('COAL')

       CASE ('FUEL OIL #1','FUELOIL#1','FUEL OIL','DISTILLATE OIL')
          BoilerFuelTypeForOutputVariable(BoilerNum) = 'FuelOil#1'
          Boiler(BoilerNum)%FuelType=AssignResourceTypeNum('DISTILLATE OIL')

       CASE ('FUEL OIL #2','FUELOIL#2','RESIDUAL OIL')
          BoilerFuelTypeForOutputVariable(BoilerNum) = 'FuelOil#2'
          Boiler(BoilerNum)%FuelType=AssignResourceTypeNum('RESIDUAL OIL')

       CASE ('PROPANE','LPG','PROPANEGAS','PROPANE GAS')
          BoilerFuelTypeForOutputVariable(BoilerNum) = 'Propane'
          Boiler(BoilerNum)%FuelType=AssignResourceTypeNum('PROPANE')

       CASE ('OTHERFUEL1')
          BoilerFuelTypeForOutputVariable(BoilerNum) = 'OtherFuel1'
          Boiler(BoilerNum)%FuelType=AssignResourceTypeNum('OTHERFUEL1')

       CASE ('OTHERFUEL2')
          BoilerFuelTypeForOutputVariable(BoilerNum) = 'OtherFuel2'
          Boiler(BoilerNum)%FuelType=AssignResourceTypeNum('OTHERFUEL2')

       CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
          CALL ShowContinueError('Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))

          ! Set to Electric to avoid errors when setting up output variables
          BoilerFuelTypeForOutputVariable(BoilerNum) = 'Electric'
          ErrorsFound=.true.
       END SELECT


       ! INPUTS from the IDF file
       Boiler(BoilerNum)%BoilerMaxOperPress   = rNumericArgs(1)
       Boiler(BoilerNum)%Effic                = rNumericArgs(2)
       Boiler(BoilerNum)%TempUpLimitBoilerOut = rNumericArgs(3)
       Boiler(BoilerNum)%NomCap               = rNumericArgs(4)
       Boiler(BoilerNum)%MinPartLoadRat       = rNumericArgs(5)
       Boiler(BoilerNum)%MaxPartLoadRat       = rNumericArgs(6)
       Boiler(BoilerNum)%OptPartLoadRat       = rNumericArgs(7)
       Boiler(BoilerNum)%FullLoadCoef(1)      = rNumericArgs(8)
       Boiler(BoilerNum)%FullLoadCoef(2)      = rNumericArgs(9)
       Boiler(BoilerNum)%FullLoadCoef(3)      = rNumericArgs(10)
       Boiler(BoilerNum)%SizFac               = rNumericArgs(11)
       IF (Boiler(BoilerNum)%SizFac <= 0.0d0) Boiler(BoilerNum)%SizFac = 1.0d0

       IF ((rNumericArgs(8)+rNumericArgs(9)+rNumericArgs(10)) == 0.0D0) THEN
         CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
         CALL ShowContinueError(' Sum of fuel use curve coefficients = 0.0')
         ErrorsFound=.true.
       ENDIF

       IF (rNumericArgs(5) == 0.0D0) THEN
         CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
         CALL ShowContinueError('Invalid '//TRIM(cNumericFieldNames(5))//'='//TRIM(RoundSigDigits(rNumericArgs(5),3)))
         ErrorsFound=.true.
       ENDIF

       IF (rNumericArgs(3) == 0.0D0) THEN
         CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
         CALL ShowContinueError('Invalid '//TRIM(cNumericFieldNames(3))//'='//TRIM(RoundSigDigits(rNumericArgs(3),3)))
         ErrorsFound=.true.
       ENDIF
       Boiler(BoilerNum)%BoilerInletNodeNum= &
           GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeType_Steam, &
           NodeConnectionType_Inlet, 1, ObjectIsNotParent)
       Boiler(BoilerNum)%BoilerOutletNodeNum   = &
           GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeType_Steam, &
           NodeConnectionType_Outlet, 1, ObjectIsNotParent)
       CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4),'Hot Steam Nodes')

      IF (SteamFluidIndex == 0 .and. BoilerNum == 1) THEN
        SteamFluidIndex=FindRefrigerant('Steam')
        IF (SteamFluidIndex == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
          CALL ShowContinueError('Steam Properties not found; '//   &
                                 'Steam Fluid Properties must be included in the input file.')
          ErrorsFound=.true.
        ENDIF
      ENDIF

      Boiler(BoilerNum)%FluidIndex=SteamFluidIndex

    END DO

    IF (ErrorsFound) THEN
      CALL ShowFatalError(RoutineName//'Errors found in processing '//TRIM(cCurrentModuleObject)//' input.')
    ENDIF

    DO BoilerNum = 1, NumBoilers
       CALL SetupOutputVariable('Boiler Heating Rate [W]', &
            BoilerReport(BoilerNum)%BoilerLoad,'System','Average',Boiler(BoilerNum)%Name)
       CALL SetupOutputVariable('Boiler Heating Energy [J]', &
            BoilerReport(BoilerNum)%BoilerEnergy,'System','Sum',Boiler(BoilerNum)%Name,  &
                  ResourceTypeKey='ENERGYTRANSFER',EndUseKey='BOILERS',GroupKey='Plant')
       IF (SameString(BoilerFuelTypeForOutputVariable(BoilerNum), 'Electric')) THEN
         CALL SetupOutputVariable('Boiler ' // TRIM(BoilerFuelTypeForOutputVariable(BoilerNum)) //' Power [W]', &
              BoilerReport(BoilerNum)%FuelUsed,'System','Average',Boiler(BoilerNum)%Name)
       ELSE
         CALL SetupOutputVariable('Boiler ' // TRIM(BoilerFuelTypeForOutputVariable(BoilerNum)) //' Rate [W]', &
              BoilerReport(BoilerNum)%FuelUsed,'System','Average',Boiler(BoilerNum)%Name)
       ENDIF
       CALL SetupOutputVariable('Boiler ' // TRIM(BoilerFuelTypeForOutputVariable(BoilerNum)) //' Energy [J]', &
            BoilerReport(BoilerNum)%FuelConsumed,'System','Sum',Boiler(BoilerNum)%Name,  &
                  ResourceTypeKey=TRIM(BoilerFuelTypeForOutputVariable(BoilerNum)),EndUseKey='Heating',GroupKey='Plant')
       CALL SetupOutputVariable('Boiler Steam Inlet Temperature [C]', &
            BoilerReport(BoilerNum)%BoilerInletTemp,'System','Average',Boiler(BoilerNum)%Name)
       CALL SetupOutputVariable('Boiler Steam Outlet Temperature [C]', &
            BoilerReport(BoilerNum)%BoilerOutletTemp,'System','Average',Boiler(BoilerNum)%Name)
       CALL SetupOutputVariable('Boiler Steam Mass Flow Rate [kg/s]', &
            BoilerReport(BoilerNum)%Mdot,'System','Average',Boiler(BoilerNum)%Name)

    END DO

    DEALLOCATE(BoilerFuelTypeForOutputVariable)

    RETURN
END SUBROUTINE GetBoilerInput


SUBROUTINE InitBoiler(BoilerNum)
  ! SUBROUTINE INFORMATION:
  !       AUTHOR         Rahul Chillar
  !       DATE WRITTEN   Dec 2004
  !       MODIFIED       na
  !       RE-ENGINEERED  D. Shirey, rework for plant upgrade

  ! PURPOSE OF THIS SUBROUTINE:
  ! This subroutine is for initializations of the Boiler components

  ! METHODOLOGY EMPLOYED:
  ! Uses the status flags to trigger initializations.

  ! REFERENCES:
  ! Na

  ! USE STATEMENTS:

  USE FluidProperties, ONLY : GetSatDensityRefrig, GetSatEnthalpyRefrig, GetSatSpecificHeatRefrig
  USE DataPlant,       ONLY : TypeOf_Boiler_Steam, ScanPlantLoopsForObject, PlantLoop, &
                              PlantSizesOkayToFinalize, PlantSizeNotComplete, &
                              SingleSetpoint, DualSetpointDeadband
  USE PlantUtilities,  ONLY : InitComponentNodes
  USE EMSManager,      ONLY : iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS
  USE DataGlobals,     ONLY : AnyEnergyManagementSystemInModel

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

  ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: BoilerNum     ! number of the current electric chiller being simulated

  ! SUBROUTINE PARAMETER DEFINITIONS:
  ! na

  ! INTERFACE BLOCK SPECIFICATIONS
  ! na

  ! DERIVED TYPE DEFINITIONS
  ! na

  ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyFlag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL           :: FatalError
  REAL(r64)         :: TempUpLimitBoilerOut ! C - Boiler outlet maximum temperature limit
  REAL(r64)         :: EnthSteamOutWet
  REAL(r64)         :: EnthSteamOutDry
  REAL(r64)         :: LatentEnthSteam
  REAL(r64)         :: CpWater                  ! Heat capacity of condensed steam (liquid)
  INTEGER           :: BoilerInletNode          ! Boiler inlet node number
  INTEGER           :: BoilerOutletNode         ! Boiler outlet node number
  LOGICAL           :: errFlag

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyFlag(NumBoilers))
    ALLOCATE(MyEnvrnFlag(NumBoilers))
    MyFlag = .TRUE.
    MyEnvrnFlag = .TRUE.
    MyOneTimeFlag = .false.
  END IF

  ! Init more variables
  IF (MyFlag(BoilerNum)) THEN
    ! Locate the chillers on the plant loops for later usage
    errFlag=.false.
    CALL ScanPlantLoopsForObject(Boiler(BoilerNum)%Name, &
                                 TypeOf_Boiler_Steam, &
                                 Boiler(BoilerNum)%LoopNum, &
                                 Boiler(BoilerNum)%LoopSideNum, &
                                 Boiler(BoilerNum)%BranchNum, &
                                 Boiler(BoilerNum)%CompNum,  &
                                 errFlag=errFlag)
    IF (errFlag) THEN
      CALL ShowFatalError('InitBoiler: Program terminated due to previous condition(s).')
    ENDIF

    MyFlag(BoilerNum)=.FALSE.
  ENDIF

  BoilerInletNode       = Boiler(BoilerNum)%BoilerInletNodeNum
  BoilerOutletNode      = Boiler(BoilerNum)%BoilerOutletNodeNum

  IF (BeginEnvrnFlag .and. MyEnvrnFlag(BoilerNum).AND. (PlantSizesOkayToFinalize)) THEN
    IF (PlantSizeNotComplete) CALL SizeBoiler(BoilerNum)

   !BoilerOutletTemp     = Node(BoilerOutletNode)%TempSetPoint
   !TempUpLimitBoilerOut =Boiler(BoilerNum)%TempUpLimitBoilerOut
!      TempUpLimitBoilerOut = Node(BoilerOutletNode)%TempSetPoint
    TempUpLimitBoilerOut = Boiler(BoilerNum)%TempUpLimitBoilerOut ! Design Outlet Steam Temperature
    EnthSteamOutDry=   GetSatEnthalpyRefrig('STEAM',TempUpLimitBoilerOut,1.0d0,Boiler(BoilerNum)%FluidIndex,'InitBoiler')
    EnthSteamOutWet=   GetSatEnthalpyRefrig('STEAM',TempUpLimitBoilerOut,0.0d0,Boiler(BoilerNum)%FluidIndex,'InitBoiler')
    LatentEnthSteam=   EnthSteamOutDry-EnthSteamOutWet

    CpWater = GetSatSpecificHeatRefrig('STEAM',TempUpLimitBoilerOut,0.0d0,Boiler(BoilerNum)%FluidIndex,'InitBoiler')

    Boiler(BoilerNum)%DesMassFlowRate = Boiler(BoilerNum)%NomCap/(LatentEnthSteam +  &
                                        CpWater * (TempUpLimitBoilerOut-Node(BoilerInletNode)%Temp))

    CALL InitComponentNodes(0.d0,Boiler(BoilerNum)%DesMassFlowRate,     &
                                  Boiler(BoilerNum)%BoilerInletNodeNum,  &
                                  Boiler(BoilerNum)%BoilerOutletNodeNum,  &
                                  Boiler(BoilerNum)%LoopNum,             &
                                  Boiler(BoilerNum)%LoopSideNum,         &
                                  Boiler(BoilerNum)%BranchNum,           &
                                  Boiler(BoilerNum)%CompNum)

    Boiler(BoilerNum)%BoilerPressCheck = 0.0d0
    FuelUsed = 0.0d0
    BoilerLoad = 0.0d0
!         BoilerMassFlowRate = 0.0
    BoilerOutletTemp = 0.0d0
    BoilerMaxPress = 0.0d0
!        BoilerMassFlowMaxAvail = 0.0
!        BoilerMassFlowMinAvail = 0.0

    IF ((Node(Boiler(BoilerNum)%BoilerOutletNodeNum)%TempSetPoint == SensedNodeFlagValue) .AND. &
        (Node(Boiler(BoilerNum)%BoilerOutletNodeNum)%TempSetPointLo == SensedNodeFlagValue)) THEN
      IF (.NOT. AnyEnergyManagementSystemInModel) THEN
        IF (.NOT. Boiler(BoilerNum)%MissingSetpointErrDone) THEN
          CALL ShowWarningError('Missing temperature setpoint for Boiler:Steam = ' // &
                                          TRIM(Boiler(BoilerNum)%Name) )
          CALL ShowContinueError(' A temperature setpoint is needed at the outlet node of the boiler,' // &
                                         ' use a SetpointManager')
          CALL ShowContinueError(' The overall loop setpoint will be assumed for this boiler. The simulation continues ...')
          Boiler(BoilerNum)%MissingSetpointErrDone = .TRUE.
        ENDIF
      ELSE
     ! need call to EMS to check node
        FatalError = .FALSE. ! but not really fatal yet, but should be.
        CALL CheckIfNodeSetpointManagedByEMS(Boiler(BoilerNum)%BoilerOutletNodeNum,iTemperatureSetpoint, FatalError)
        IF (FatalError) THEN
          IF (.NOT. Boiler(BoilerNum)%MissingSetpointErrDone) THEN
            CALL ShowWarningError('Missing temperature setpoint for VariableFlow mode Boiler named ' // &
                                          TRIM(Boiler(BoilerNum)%Name) )
            CALL ShowContinueError(' A temperature setpoint is needed at the outlet node of the boiler.')
            CALL ShowContinueError(' Use a Setpoint Manager to establish a setpoint at the boiler outlet node ')
            CALL ShowContinueError(' or use an EMS actuator to establish a setpoint at the boiler outlet node.')
            CALL ShowContinueError(' The overall loop setpoint will be assumed for this boiler. The simulation continues...')
            Boiler(BoilerNum)%MissingSetpointErrDone = .TRUE.
          ENDIF
        ENDIF
      ENDIF
      Boiler(BoilerNum)%UseLoopSetpoint = .TRUE. ! this is for backward compatibility and could be removed
    ENDIF

    MyEnvrnFlag(BoilerNum) = .FALSE.

  END IF  ! End If for the Begin Environment initializations

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(BoilerNum)=.true.
  ENDIF

  IF (Boiler(BoilerNum)%UseLoopSetpoint) THEN
!  At some point, need to circle back and get from plant data structure instead of node
! fix for clumsy old input that worked because loop setpoint was spread.
!  could be removed with transition, testing , model change, period of being obsolete.
    SELECT CASE (PlantLoop(Boiler(BoilerNum)%LoopNum)%LoopDemandCalcScheme)
    CASE (SingleSetPoint)
      Node(BoilerOutletNode)%TempSetPoint =                        &
           Node(PlantLoop(Boiler(BoilerNum)%LoopNum)%TempSetPointNodeNum)%TempSetPoint
    CASE (DualSetPointDeadBand)
      Node(BoilerOutletNode)%TempSetPointLo =                        &
           Node(PlantLoop(Boiler(BoilerNum)%LoopNum)%TempSetPointNodeNum)%TempSetPointLo
    END SELECT

  END IF

  RETURN

END SUBROUTINE InitBoiler

SUBROUTINE SizeBoiler(BoilerNum)

  ! SUBROUTINE INFORMATION:
  !       AUTHOR         Rahul Chillar
  !       DATE WRITTEN   Dec 2004
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS SUBROUTINE:
  ! This subroutine is for sizing Boiler Components for which capacities and flow rates
  ! have not been specified in the input.

  ! METHODOLOGY EMPLOYED:
  ! Obtains Steam flow rate from the plant sizing array. Calculates nominal capacity from
  ! the hot water flow rate and the hot water loop design delta T.

  ! REFERENCES:
  ! na

  ! USE STATEMENTS:
  USE DataSizing
  USE DataPlant,         ONLY: PlantLoop,  PlantSizesOkayToFinalize
  USE FluidProperties,   ONLY: GetSatDensityRefrig,GetSatEnthalpyRefrig,GetSatSpecificHeatRefrig
  USE OutputReportPredefined
  USE ReportSizingManager, ONLY: ReportSizingOutput


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

  ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: BoilerNum

  ! SUBROUTINE PARAMETER DEFINITIONS:
  ! na

  ! INTERFACE BLOCK SPECIFICATIONS
  ! na

  ! DERIVED TYPE DEFINITIONS
  ! na

  ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PltSizNum     ! Plant Sizing index corresponding to CurLoopNum
  LOGICAL             :: ErrorsFound   ! If errors detected in input
  REAL(r64)           :: SteamDensity
  REAL(r64)           :: EnthSteamOutWet
  REAL(r64)           :: EnthSteamOutDry
  REAL(r64)           :: LatentEnthSteam
  REAL(r64)           :: SizingTemp
  REAL(r64)           :: CpWater   ! Heat capacity of condensed steam
  CHARACTER(len=MaxNameLength) :: equipName
  REAL(r64)           :: tmpNomCap ! local nominal capacity cooling power

  PltSizNum = 0
  ErrorsFound = .FALSE.
  tmpNomCap = Boiler(BoilerNum)%NomCap

  ! Find the appropriate Plant Sizing object
  PltSizNum = PlantLoop(Boiler(BoilerNum)%LoopNum)%PlantSizNum

  IF (Boiler(BoilerNum)%NomCap  == AutoSize) THEN
    IF (PltSizNum > 0) THEN
      IF (PlantSizData(PltSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        SizingTemp =Boiler(BoilerNum)%TempUpLimitBoilerOut
        SteamDensity   =   GetSatDensityRefrig('STEAM',SizingTemp,1.0d0,Boiler(BoilerNum)%FluidIndex,'SizeBoiler')
        EnthSteamOutDry=   GetSatEnthalpyRefrig('STEAM',SizingTemp,1.0d0,Boiler(BoilerNum)%FluidIndex,'SizeBoiler')
        EnthSteamOutWet=   GetSatEnthalpyRefrig('STEAM',SizingTemp,0.0d0,Boiler(BoilerNum)%FluidIndex,'SizeBoiler')
        LatentEnthSteam=   EnthSteamOutDry-EnthSteamOutWet
        CpWater        =   GetSatSpecificHeatRefrig('STEAM',SizingTemp,0.0d0,Boiler(BoilerNum)%FluidIndex,'SizeBoiler')
        tmpNomCap = (CpWater * SteamDensity * Boiler(BoilerNum)%SizFac * &
                                   PlantSizData(PltSizNum)%DeltaT* PlantSizData(PltSizNum)%DesVolFlowRate + &
                                   PlantSizData(PltSizNum)%DesVolFlowRate*SteamDensity*LatentEnthSteam)
        IF (PlantSizesOkayToFinalize) Boiler(BoilerNum)%NomCap =tmpNomCap
      ELSE
        tmpNomCap = 0.d0
        IF (PlantSizesOkayToFinalize) Boiler(BoilerNum)%NomCap = tmpNomCap
      END IF
        IF (PlantSizesOkayToFinalize) CALL ReportSizingOutput('Boiler:Steam', Boiler(BoilerNum)%Name, &
                              'Nominal Capacity [W]', Boiler(BoilerNum)%NomCap)
    ELSE
      CALL ShowSevereError('Autosizing of Boiler nominal capacity requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in Boiler:Steam object='//TRIM(Boiler(BoilerNum)%Name))
      ErrorsFound = .TRUE.
    END IF
  END IF

!  model has no volume flow rate, may need something else for steam loop sizing DSU??
!DSU?      CALL RegisterPlantCompDesignFlow(Boiler(BoilerNum)%BoilerInletNodeNum,Boiler(BoilerNum)%VolFlowRate)

  IF (PlantSizesOkayToFinalize) THEN
    !create predefined report
    equipName = Boiler(BoilerNum)%Name
    CALL PreDefTableEntry(pdchMechType,equipName,'Boiler:Steam')
    CALL PreDefTableEntry(pdchMechNomEff,equipName,Boiler(BoilerNum)%Effic)
    CALL PreDefTableEntry(pdchMechNomCap,equipName,Boiler(BoilerNum)%NomCap)
  ENDIF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN
END SUBROUTINE SizeBoiler

SUBROUTINE CalcBoilerModel(BoilerNum,MyLoad,Runflag,EquipFlowCtrl)
      ! SUBROUTINE INFORMATION:
      !       AUTHOR         Rahul Chillar
      !       DATE WRITTEN   Dec 2004
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS SUBROUTINE:
      ! This subroutine calculates the boiler fuel consumption and the associated
      ! hot water demand met by the boiler

      ! METHODOLOGY EMPLOYED:
      ! The model is based on a single combustion efficiency (=1 for electric)
      ! and a second order polynomial fit of performance data to obtain part
      ! load performance

      ! REFERENCES:
      ! na

      ! USE STATEMENTS:
       USE General, ONLY: RoundSigDigits
       USE FluidProperties, ONLY: GetSatPressureRefrig, GetSatSpecificHeatRefrig, GetSatEnthalpyRefrig
       USE DataPlant, ONLY: PlantLoop, SingleSetpoint, DualSetpointDeadband
       USE DataBranchAirLoopPlant, ONLY : ControlType_SeriesActive
       USE PlantUtilities, ONLY: SetComponentFlowRate

       IMPLICIT NONE


      ! SUBROUTINE ARGUMENT DEFINITIONS:
       INTEGER                :: BoilerNum       ! boiler identifier
       REAL(r64)              :: MyLoad          ! W - hot water demand to be met by boiler
       LOGICAL                :: RunFlag         ! TRUE if boiler operating
       INTEGER, INTENT(IN)    :: EquipFlowCtrl  ! Flow control mode for the equipment


      ! SUBROUTINE PARAMETER DEFINITIONS:
      ! na

      ! DERIVED TYPE DEFINITIONS
      ! na

      ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      REAL(r64)              :: BoilerEFF                ! boiler efficiency
      REAL(r64)              :: BoilerNomCap             ! W - boiler nominal capacity
      REAL(r64)              :: BoilerMaxPLR             ! boiler maximum part load ratio
      REAL(r64)              :: BoilerMinPLR             ! boiler minimum part load ratio
      REAL(r64)              :: TheorFuelUse             ! Theoretical (stoichiometric) fuel use
      REAL(r64)              :: OperPLR                  ! operating part load ratio
      REAL(r64)              :: BoilerDeltaTemp          ! C - boiler inlet to outlet temperature difference
      REAL(r64)              :: TempUpLimitBout          ! C - boiler high temperature limit
      REAL(r64)              :: BoilerMassFlowRateMax    ! Max Design Boiler Mass Flow Rate converted from Volume Flow Rate
      REAL(r64)              :: EnthSteamOutDry          !
      REAL(r64)              :: EnthSteamOutWet          !
      REAL(r64)              :: LatentEnthSteam          !
      REAL(r64)              :: QualitySteam             !
      REAL(r64), DIMENSION(3)     :: LoadCoef                 ! coefficients of the fuel use/part load curve
      REAL(r64)              :: CpWater                  ! Heat capacity of condensed steam
      INTEGER                :: BoilerInletNode          ! Boiler inlet node number
      INTEGER                :: BoilerOutletNode         ! Boiler outlet node number
!      CHARACTER(len=25) CErrCount                        !
!      INTEGER,SAVE :: PressErrCount=0                    !
      INTEGER :: LoopNum
      INTEGER :: LoopSideNum


     !Loading the variables derived type in to local variables
      BoilerLoad            = 0.0d0
      BoilerMassFlowRate    = 0.0d0
      BoilerInletNode       = Boiler(BoilerNum)%BoilerInletNodeNum
      BoilerOutletNode      = Boiler(BoilerNum)%BoilerOutletNodeNum
      BoilerNomCap          = Boiler(BoilerNum)%NomCap
      BoilerMaxPLR          = Boiler(BoilerNum)%MaxPartLoadRat
      BoilerMinPLR          = Boiler(BoilerNum)%MinPartLoadRat
      LoadCoef              = Boiler(BoilerNum)%FullLoadCoef
      TempUpLimitBout       = Boiler(BoilerNum)%TempUpLimitBoilerOut
      BoilerMassFlowRateMax = Boiler(BoilerNum)%DesMassFlowRate
      BoilerMaxPress        = Boiler(BoilerNum)%BoilerMaxOperPress
      BoilerEff             = Boiler(BoilerNum)%Effic

      QualitySteam          = Node(BoilerInletNode)%Quality
      LoopNum               = Boiler(BoilerNum)%LoopNum
      LoopSideNum           = Boiler(BoilerNum)%LoopSideNum
      SELECT CASE (PlantLoop(LoopNum)%LoopDemandCalcScheme)
      CASE (SingleSetPoint)
        BoilerOutletTemp      = Node(BoilerOutletNode)%TempSetPoint
      CASE (DualSetPointDeadBand)
        BoilerOutletTemp      = Node(BoilerOutletNode)%TempSetPointLo
      END SELECT
    !If the specified load is 0.0 or the boiler should not run then we leave this subroutine.Before leaving
    !if the component control is SERIESACTIVE we set the component flow to inlet flow so that flow resolver
    !will not shut down the branch
      IF(MyLoad <= 0.0d0 .OR. .NOT. RunFlag) THEN
        IF(EquipFlowCtrl == ControlType_SeriesActive) BoilerMassFlowRate = Node(BoilerInletNode)%MassFlowrate
        RETURN
      END IF

      !Set the current load equal to the boiler load
      BoilerLoad = MyLoad

      Boiler(BoilerNum)%BoilerPressCheck=  &
         GetSatPressureRefrig('STEAM',BoilerOutletTemp,Boiler(BoilerNum)%FluidIndex,'CalcBoilerModel')

      IF((Boiler(BoilerNum)%BoilerPressCheck).GT.BoilerMaxPress)THEN
        IF (Boiler(BoilerNum)%PressErrIndex == 0) THEN
         CALL ShowSevereError('Boiler:Steam="'//TRIM(Boiler(BoilerNum)%Name)//  &
           '", Saturation Pressure is greater than Maximum Operating Pressure,')
         CALL ShowContinueError('Lower Input Temperature')
         CALL ShowContinueError('Steam temperature=['//TRIM(RoundSigDigits(BoilerOutletTemp,2))//'] C')
         CALL ShowContinueError('Refrigerant Saturation Pressure =['//  &
                 trim(RoundSigDigits(Boiler(BoilerNum)%BoilerPressCheck,0))//'] Pa')
        ENDIF
        CALL ShowRecurringSevereErrorAtEnd('Boiler:Steam="'//TRIM(Boiler(BoilerNum)%Name)//  &
           '", Saturation Pressure is greater than Maximum Operating Pressure..continues',Boiler(BoilerNum)%PressErrIndex,   &
           ReportMinOf=Boiler(BoilerNum)%BoilerPressCheck,ReportMinUnits='[Pa]',  &
           ReportMaxOf=Boiler(BoilerNum)%BoilerPressCheck,ReportMaxUnits='[Pa]')
      END IF

      CpWater = GetSatSpecificHeatRefrig('STEAM',Node(BoilerInletNode)%Temp,0.0d0,Boiler(BoilerNum)%FluidIndex,'CalcBoilerModel')

      IF (PlantLoop(LoopNum)%Loopside(LoopSideNum)%FlowLock==0) THEN
        ! Calculate the flow for the boiler

        SELECT CASE (PlantLoop(Boiler(BoilerNum)%LoopNum)%LoopDemandCalcScheme)
        CASE (SingleSetPoint)
          BoilerDeltaTemp = Node(BoilerOutletNode)%TempSetPoint-Node(BoilerInletNode)%Temp
        CASE (DualSetPointDeadBand)
          BoilerDeltaTemp = Node(BoilerOutletNode)%TempSetPointLo-Node(BoilerInletNode)%Temp
        END SELECT
        BoilerOutletTemp = BoilerDeltaTemp + Node(BoilerInletNode)%Temp

        EnthSteamOutDry=GetSatEnthalpyRefrig('STEAM',BoilerOutletTemp,1.0d0,Boiler(BoilerNum)%FluidIndex,'CalcBoilerModel')

        EnthSteamOutWet=GetSatEnthalpyRefrig('STEAM',BoilerOutletTemp,0.0d0,Boiler(BoilerNum)%FluidIndex,'CalcBoilerModel')

        LatentEnthSteam=EnthSteamOutDry-EnthSteamOutWet

        BoilerMassFlowRate =BoilerLoad/(LatentEnthSteam+(CpWater*BoilerDeltaTemp))
        !Check to see if the Maximum is exceeded, if so set to maximum
 !       BoilerMassFlowRate = MIN(BoilerMassFlowRateMax, BoilerMassFlowRate)
 !       BoilerMassFlowRate = MIN(BoilerMassFlowRate,Node(BoilerInletNode)%MassFlowRateMaxAvail)  !CRBranchPump
 !       BoilerMassFlowRate = MAX(BoilerMassFlowRate,Node(BoilerInletNode)%MassFlowRateMinAvail)     !CRBranchPump

        CALL SetComponentFlowRate(BoilerMassFlowRate, BoilerInletNode, BoilerOutletNode, &
                                                      Boiler(BoilerNum)%LoopNum,     &
                                                      Boiler(BoilerNum)%LoopSideNum, &
                                                      Boiler(BoilerNum)%BranchNum,   &
                                                      Boiler(BoilerNum)%CompNum)

      ELSE ! If FlowLock is True
        ! Set the boiler flow rate from inlet node and then check performance
        BoilerMassFlowRate = Node(BoilerInletNode)%MassFlowRate
        ! Assume that it can meet the setpoint
        SELECT CASE (PlantLoop(Boiler(BoilerNum)%LoopNum)%LoopDemandCalcScheme)
        CASE (SingleSetPoint)
          BoilerDeltaTemp = Node(BoilerOutletNode)%TempSetPoint-Node(BoilerInletNode)%Temp
        CASE (DualSetPointDeadBand)
          BoilerDeltaTemp = Node(BoilerOutletNode)%TempSetPointLo-Node(BoilerInletNode)%Temp
        END SELECT
        !If boiler outlet temp is already greater than setpoint than it does not need to operate this iteration
        IF(BoilerDeltaTemp < 0.0d0) THEN
          SELECT CASE (PlantLoop(Boiler(BoilerNum)%LoopNum)%LoopDemandCalcScheme)
          CASE (SingleSetPoint)
            BoilerOutletTemp=Node(BoilerOutletNode)%TempSetPoint
          CASE (DualSetPointDeadBand)
            BoilerOutletTemp=Node(BoilerOutletNode)%TempSetPointLo
          END SELECT
          EnthSteamOutDry=GetSatEnthalpyRefrig('STEAM',BoilerOutletTemp,1.0d0,Boiler(BoilerNum)%FluidIndex,'CalcBoilerModel')
          EnthSteamOutWet=GetSatEnthalpyRefrig('STEAM',BoilerOutletTemp,0.0d0,Boiler(BoilerNum)%FluidIndex,'CalcBoilerModel')

          LatentEnthSteam=EnthSteamOutDry-EnthSteamOutWet

          BoilerLoad = (BoilerMassFlowRate*LatentEnthSteam)

        ELSE

          SELECT CASE (PlantLoop(Boiler(BoilerNum)%LoopNum)%LoopDemandCalcScheme)
          CASE (SingleSetPoint)
            BoilerOutletTemp=Node(BoilerOutletNode)%TempSetPoint
          CASE (DualSetPointDeadBand)
            BoilerOutletTemp=Node(BoilerOutletNode)%TempSetPointLo
          END SELECT

          EnthSteamOutDry=GetSatEnthalpyRefrig('STEAM',BoilerOutletTemp,1.0d0,Boiler(BoilerNum)%FluidIndex,'CalcBoilerModel')
          EnthSteamOutWet=GetSatEnthalpyRefrig('STEAM',BoilerOutletTemp,0.0d0,Boiler(BoilerNum)%FluidIndex,'CalcBoilerModel')

          LatentEnthSteam=EnthSteamOutDry-EnthSteamOutWet

          ! Calculate the boiler load with the specified flow rate.
          BoilerLoad = ABS(BoilerMassFlowRate*LatentEnthSteam)+  &
                           ABS(BoilerMassFlowRate*CpWater*BoilerDeltaTemp)

        END IF

        ! If load exceeds the distributed load set to the distributed load
        IF(BoilerLoad > MyLoad) THEN
          BoilerLoad = MyLoad

          ! Reset later , here just for calculating latent heat
          SELECT CASE (PlantLoop(Boiler(BoilerNum)%LoopNum)%LoopDemandCalcScheme)
          CASE (SingleSetPoint)
            BoilerOutletTemp=Node(BoilerOutletNode)%TempSetPoint
          CASE (DualSetPointDeadBand)
            BoilerOutletTemp=Node(BoilerOutletNode)%TempSetPointLo
          END SELECT

          EnthSteamOutDry=GetSatEnthalpyRefrig('STEAM',BoilerOutletTemp,1.0d0,Boiler(BoilerNum)%FluidIndex,'CalcBoilerModel')

          EnthSteamOutWet=GetSatEnthalpyRefrig('STEAM',BoilerOutletTemp,0.0d0,Boiler(BoilerNum)%FluidIndex,'CalcBoilerModel')

          LatentEnthSteam=EnthSteamOutDry-EnthSteamOutWet

          BoilerDeltaTemp =BoilerOutletTemp-Node(BoilerInletNode)%Temp

          BoilerMassFlowRate=BoilerLoad/(LatentEnthSteam+CpWater*BoilerDeltaTemp)

          CALL SetComponentFlowRate(BoilerMassFlowRate, BoilerInletNode, BoilerOutletNode, &
                                                        Boiler(BoilerNum)%LoopNum,     &
                                                        Boiler(BoilerNum)%LoopSideNum, &
                                                        Boiler(BoilerNum)%BranchNum,   &
                                                        Boiler(BoilerNum)%CompNum)
        END IF

        ! Checks Boiler Load on the basis of the machine limits.
        IF(BoilerLoad > BoilerNomCap)THEN
          IF(BoilerMassFlowRate > MassFlowTolerance) THEN
            BoilerLoad = BoilerNomCap

            EnthSteamOutDry=GetSatEnthalpyRefrig('STEAM',BoilerOutletTemp,1.0d0,Boiler(BoilerNum)%FluidIndex,'CalcBoilerModel')
            EnthSteamOutWet=GetSatEnthalpyRefrig('STEAM',BoilerOutletTemp,0.0d0,Boiler(BoilerNum)%FluidIndex,'CalcBoilerModel')

            LatentEnthSteam=EnthSteamOutDry-EnthSteamOutWet

            BoilerDeltaTemp = BoilerOutletTemp-Node(BoilerInletNode)%Temp

            BoilerMassFlowRate=BoilerLoad/(LatentEnthSteam+CpWater*BoilerDeltaTemp)

            CALL SetComponentFlowRate(BoilerMassFlowRate, BoilerInletNode, BoilerOutletNode, &
                                                          Boiler(BoilerNum)%LoopNum,     &
                                                          Boiler(BoilerNum)%LoopSideNum, &
                                                          Boiler(BoilerNum)%BranchNum,   &
                                                          Boiler(BoilerNum)%CompNum)
          ELSE
            BoilerLoad = 0.0d0
            BoilerOutletTemp = Node(BoilerInletNode)%Temp
          END IF
        END IF

      END IF  !End of the FlowLock If block

      ! Limit BoilerOutletTemp.  If > max temp, trip boiler.
      IF(BoilerOutletTemp > TempUpLimitBout) THEN
        BoilerDeltaTemp = 0.0d0
        BoilerLoad = 0.0d0
        BoilerOutletTemp = Node(BoilerInletNode)%Temp
        !  Does BoilerMassFlowRate need to be set????
      END IF

      OperPLR      = BoilerLoad/BoilerNomCap
      OperPLR      = MIN(OperPLR,BoilerMaxPLR)
      OperPLR      = MAX(OperPLR,BoilerMinPLR)
      TheorFuelUse = BoilerLoad/BoilerEff

      ! Calculate fuel used
      FuelUsed=TheorFuelUse/(LoadCoef(1) + LoadCoef(2)*OperPLR + LoadCoef(3)*OperPLR**2)

  RETURN
END SUBROUTINE CalcBoilerModel

! Beginning of Record Keeping subroutines for the BOILER:SIMPLE Module
SUBROUTINE UpdateBoilerRecords(MyLoad,RunFlag,Num,FirstHVACIteration)
      ! SUBROUTINE INFORMATION:
      !       AUTHOR         Rahul Chillar
      !       DATE WRITTEN   Dec 2004
      !       MODIFIED       na
      !       RE-ENGINEERED  na

      ! PURPOSE OF THIS SUBROUTINE:
      ! Boiler simulation reporting

      ! METHODOLOGY EMPLOYED:
      ! na

      ! REFERENCES:
      ! na

      ! USE STATEMENTS:
!     USE DataPlant, ONLY : PlantLoop
     USE PlantUtilities, ONLY : SafeCopyPlantNode

       IMPLICIT NONE

      ! SUBROUTINE ARGUMENT DEFINITIONS:
       REAL(r64),INTENT(IN)   :: MyLoad        !boiler operating load
       LOGICAL,  INTENT(IN)   :: RunFlag       !boiler on when TRUE
       INTEGER,  INTENT(IN)   :: Num           !boiler number
       LOGICAL , INTENT(IN)   :: FirstHVACIteration ! TRUE if First iteration of simulation

      ! SUBROUTINE PARAMETER DEFINITIONS:
      ! na

      ! INTERFACE BLOCK SPECIFICATIONS
      ! na

      ! DERIVED TYPE DEFINITIONS
      ! na

      ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      INTEGER                :: BoilerInletNode ! Boiler inlet node number
      INTEGER                :: BoilerOutletNode ! Boiler outlet node number
      REAL(r64) :: ReportingConstant
      INTEGER :: LoopNum
      INTEGER :: LoopSideNum

      ReportingConstant = TimeStepSys*SecInHour

      BoilerInletNode     = Boiler(Num)%BoilerInletNodeNum
      BoilerOutletNode    = Boiler(Num)%BoilerOutletNodeNum

    IF (MyLoad<=0.0d0 .OR. .NOT. RunFlag)THEN
      !set node temperatures
      CALL SafeCopyPlantNode(BoilerInletNode, BoilerOutletNode)
      Node(BoilerOutletNode)%Temp           = Node(BoilerInletNode)%Temp
      BoilerReport(Num)%BoilerOutletTemp    = Node(BoilerInletNode)%Temp
      BoilerReport(Num)%BoilerLoad          = 0.0d0
      BoilerReport(Num)%FuelUsed            = 0.0d0
      Node(BoilerInletNode)%Press           = Boiler(Num)%BoilerPressCheck
      Node(BoilerOutletNode)%Press          = Node(BoilerInletNode)%Press
      Node(BoilerInletNode)%Quality         = 0.0d0
      Node(BoilerOutletNode)%Quality        = Node(BoilerInletNode)%Quality

    ELSE
      !set node temperatures
      CALL SafeCopyPlantNode(BoilerInletNode, BoilerOutletNode)
      Node(BoilerOutletNode)%Temp           = BoilerOutletTemp
      BoilerReport(Num)%BoilerOutletTemp    = BoilerOutletTemp
      BoilerReport(Num)%BoilerLoad          = BoilerLoad
      BoilerReport(Num)%FuelUsed            = FuelUsed
      Node(BoilerInletNode)%Press           = Boiler(Num)%BoilerPressCheck !???
      Node(BoilerOutletNode)%Press          = Node(BoilerInletNode)%Press
      Node(BoilerOutletNode)%Quality        = 1.0d0 ! Model assumes saturated steam exiting the boiler

    END IF

      BoilerReport(Num)%BoilerInletTemp           = Node(BoilerInletNode)%Temp
      BoilerReport(Num)%Mdot                      = Node(BoilerOutletNode)%MassFlowRate
      LoopNum = Boiler(Num)%LoopNum
      LoopSideNum = Boiler(Num)%LoopSideNum

      BoilerReport(Num)%BoilerEnergy   = BoilerReport(Num)%BoilerLoad*ReportingConstant
      BoilerReport(Num)%FuelConsumed   = BoilerReport(Num)%FuelUsed*ReportingConstant


  RETURN

END SUBROUTINE UpdateBoilerRecords
! End of Record Keeping subroutines for the BOILER:STEAM Module



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

END MODULE BoilerSteam
