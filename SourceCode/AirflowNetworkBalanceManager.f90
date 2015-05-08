MODULE AirflowNetworkBalanceManager


          ! MODULE INFORMATION:
          !       AUTHOR         Lixing Gu, Don Shirey, and Muthusamy V. Swami
          !       DATE WRITTEN   July 28, 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS MODULE:
          ! This module is used to simulate performance of air distribution system with a single HVAC system and a constant
          ! volume supply fan.


          ! USE STATEMENTS:
    USE DataPrecisionGLobals

    USE General, ONLY: RoundSigDigits
    USE DataGlobals, ONLY: MaxNameLength, BeginEnvrnFlag, OutputFileBNDetails, NumOfZones, PI, DegToRadians, &
                           OutputFileInits, CurrentTime, WarmUpFlag, SecInHour, DisplayExtraWarnings
    USE DataInterfaces, ONLY:ShowFatalError, ShowWarningError, ShowSevereError, ShowMessage, ShowContinueError,   &
                             SetupOutputVariable, ShowContinueErrorTimeStamp, ShowRecurringWarningErrorAtEnd
    USE DataAirflowNetwork
    USE DataAirLoop, ONLY: AirToZoneNodeInfo
    USE DataLoopNode, ONLY: NumOfNodes,NodeID,Node
    USE DataEnvironment, ONLY: StdBaroPress, OutBaroPress, OutDryBulbTempAt, OutHumRat, CurMnDy, EnvironmentName, &
                               OutAirDensity, WindSpeed, WindDir, OutEnthalpy, StdRhoAir, WindSpeedAt
    USE DataSurfaces, ONLY: ExternalEnvironment,Surface,TotSurfaces,WorldCoordSystem,  &
                            SurfaceWindow,SurfaceClass_Window,SurfaceClass_Door,SurfaceClass_GlassDoor
    USE DataHeatBalance,   ONLY: Zone, TotInfiltration,TotVentilation,TotMixing,TotCrossMixing,TotZoneAirBalance
    USE DataHeatBalFanSys, ONLY: ZoneAirHumRat, MAT, XMAT, WZoneTimeMinus1, ZoneAirHumRatAvg
    USE Psychrometrics, ONLY: PsyRhoAirFnPbTdbW,PsyCpAirFnWTdb,PsyHFnTdbW,PsyRhoAirFnPbTdbW
    USE ScheduleManager, ONLY: GetScheduleIndex, GetCurrentScheduleValue
    USE AirflowNetworkSolver, ONLY: SETSKY, AIRMOV,AllocateAirflowNetworkData,InitAirflowNetworkData, &
                                        NetworkNumOfLinks, NetworkNumOfNodes
    USE Fans, ONLY: GetFanVolFlow,GetFanIndex,GetFanInletNode,GetFanOutletNode,GetFanType
    USE DataZoneEquipment, ONLY: ZoneEquipConfig
    USE DataHVACGlobals, ONLY: FanType_SimpleConstVolume,FanType_SimpleOnOff,FanType_ZoneExhaust,OnOffFanPartLoadFraction, &
                               NumHybridVentSysAvailMgrs, CycFanCycCoil, ContFanCycCoil, FanType_SimpleVAV
    USE DataContaminantBalance, ONLY: Contaminant, OutdoorCO2, ZoneAirCO2, CO2ZoneTimeMinus1, OutdoorGC, ZoneAirGC, &
                                      GCZoneTimeMinus1

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: VentCtrNum_None = 0      ! Wrong input
INTEGER, PARAMETER :: VentCtrNum_Temp = 1      ! Temperature venting control
INTEGER, PARAMETER :: VentCtrNum_Enth = 2      ! Enthalpy venting control
INTEGER, PARAMETER :: VentCtrNum_Const = 3     ! Constant venting control
INTEGER, PARAMETER :: VentCtrNum_ASH55 = 4     !
INTEGER, PARAMETER :: VentCtrNum_CEN15251 = 5  !
INTEGER, PARAMETER :: VentCtrNum_Novent = 6    ! No venting
INTEGER, PARAMETER :: VentCtrNum_ZoneLevel = 7 ! ZoneLevel control for a heat transfer subsurface
INTEGER, PARAMETER :: VentCtrNum_AdjTemp = 8   ! Temperature venting control based on adjacent zone conditions
INTEGER, PARAMETER :: VentCtrNum_AdjEnth = 9   ! Enthalpy venting control based on adjacent zone conditions

          ! DERIVED TYPE DEFINITIONS:
! Report variables

Type AirflowNetworkReportVars
  REAL(r64) :: MeanAirTemp         =0.0d0 ! Mean Air Temperature {C}
  REAL(r64) :: OperativeTemp       =0.0d0 ! Average of Mean Air Temperature {C} and Mean Radiant Temperature {C}
  REAL(r64) :: InfilHeatGain       =0.0d0 ! Heat Gain {W} due to infiltration
  REAL(r64) :: InfilHeatLoss       =0.0d0 ! Heat Loss {W} due to infiltration
  REAL(r64) :: InfilVolume         =0.0d0 ! Volume of Air {m3} due to infiltration
  REAL(r64) :: InfilMass           =0.0d0 ! Mass of Air {kg} due to infiltration
  REAL(r64) :: InfilAirChangeRate  =0.0d0 ! Infiltration air change rate {ach}
  REAL(r64) :: VentilHeatLoss      =0.0d0 ! Heat Gain {W} due to ventilation
  REAL(r64) :: VentilHeatGain      =0.0d0 ! Heat Loss {W} due to ventilation
  REAL(r64) :: VentilVolume        =0.0d0 ! Volume of Air {m3} due to ventilation
  REAL(r64) :: VentilMass          =0.0d0 ! Mass of Air {kg} due to ventilation
  REAL(r64) :: VentilFanElec       =0.0d0 ! Fan Electricity {W} due to ventilation
  REAL(r64) :: VentilAirTemp       =0.0d0 ! Air Temp {C} of ventilation
  REAL(r64) :: MixVolume           =0.0d0 ! Mixing volume of Air {m3}
  REAL(r64) :: MixMass             =0.0d0 ! Mixing mass of air {kg}
END TYPE AirflowNetworkReportVars

          ! MODULE VARIABLE DECLARATIONS:
! Report variables
REAL(r64), ALLOCATABLE, DIMENSION(:) :: PZ
! Inverse matrix
REAL(r64), ALLOCATABLE, DIMENSION(:) :: MA
REAL(r64), ALLOCATABLE, DIMENSION(:) :: MV
INTEGER, ALLOCATABLE, DIMENSION(:) :: IVEC
INTEGER, DIMENSION(:), ALLOCATABLE :: SplitterNodeNumbers

logical :: AirflowNetworkGetInputFlag = .True.
INTEGER :: VentilationCtrl = 0    ! Hybrid ventilation control type
INTEGER :: NumOfExhaustFans = 0   ! Number of exhaust fans



TYPE (AirflowNetworkReportVars), ALLOCATABLE, DIMENSION(:) :: AirflowNetworkZnRpt

INTEGER :: NumAirflowNetwork = 0
INTEGER :: AirflowNetworkNumOfDetOpenings = 0
INTEGER :: AirflowNetworkNumOfSimOpenings = 0
INTEGER :: AirflowNetworkNumOfHorOpenings = 0
INTEGER :: AirflowNetworkNumOfStdCndns = 0
INTEGER :: AirflowNetworkNumOfSurCracks = 0
INTEGER :: AirflowNetworkNumOfSurELA = 0
INTEGER :: AirflowNetworkNumOfExtNode = 0
INTEGER :: AirflowNetworkNumOfCPArray = 0
INTEGER :: AirflowNetworkNumOfCPValue = 0
INTEGER :: DisSysNumOfNodes = 0
INTEGER :: DisSysNumOfLeaks = 0
INTEGER :: DisSysNumOfELRs = 0
INTEGER :: DisSysNumOfDucts = 0
INTEGER :: DisSysNumOfDampers = 0
INTEGER :: DisSysNumOfCVFs = 0
INTEGER :: DisSysNumOfDetFans = 0
INTEGER :: DisSysNumOfCoils = 0
INTEGER :: DisSysNumOfHXs = 0
INTEGER :: DisSysNumOfCPDs = 0
INTEGER :: DisSysNumOfTermUnits = 0
INTEGER :: DisSysNumOfLinks = 0
INTEGER :: NumOfExtNodes = 0
INTEGER :: AirflowNetworkNumOfExtSurfaces = 0

INTEGER :: SupplyFanInletNode = 0          ! Supply air fan inlet node number
INTEGER :: SupplyFanOutletNode = 0         ! Supply air fan outlet node number
INTEGER :: SupplyFanType = 0               ! Supply air fan type
REAL(r64)    :: OnOffFanRunTimeFraction   = 0.0d0 ! Run time fraction for an On/Off fan flow rate

          ! SUBROUTINE SPECIFICATIONS FOR MODULE AirflowNetworkBalanceManager:
  ! Name Public routines, optionally name Private routines within this module
PRIVATE GetAirflowNetworkInput
PUBLIC ManageAirflowNetworkBalance
PRIVATE InitAirflowNetwork
PRIVATE UpdateAirflowNetwork
PUBLIC ReportAirflowNetwork
PRIVATE ValidateDistributionSystem
PRIVATE ValidateExhaustFanInput
PRIVATE HybridVentilationControl
PUBLIC GetZoneInfilAirChangeRate
PRIVATE CalcAirflowNetworkAirBalance
PRIVATE CalcAirflowNetworkHeatBalance
PRIVATE CalcAirflowNetworkMoisBalance
PRIVATE CalcAirflowNetworkCO2Balance
PRIVATE CalcAirflowNetworkGCBalance

CONTAINS

SUBROUTINE ManageAirflowNetworkBalance(FirstHVACIteration, Iter, ResimulateAirZone)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   July 28, 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine performs simulation of air distribution system.


          ! METHODOLOGY EMPLOYED:
          ! na


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY: TurnFansOn, TurnFansOff, VerySmallMassFlow
  USE DataAirLoop,     ONLY: LoopHeatingCoilMaxRTF,LoopONOffFanRTF,LoopDXCoilRTF,LoopOnOffFanPartLoadRatio, &
                             LoopSystemOnMassFlowrate,LoopFanOperationMode
  USE DataAirSystems,  ONLY: PrimaryAirSystem

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na
  LOGICAL, INTENT(IN), OPTIONAL :: FirstHVACIteration ! True when solution technique on first iteration
  INTEGER, INTENT(IN), OPTIONAL :: Iter               ! Iteration number
  LOGICAL, INTENT(INOUT), OPTIONAL :: ResimulateAirZone ! True when solution technique on third iteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

   if (AirflowNetworkGetInputFlag) then
      CALL GetAirflowNetworkInput
      AirflowNetworkGetInputFlag=.false.
      Return
   endif

   If (PRESENT(ResimulateAirZone)) then
     ResimulateAirZone = .FALSE.
   End If

   if (SimulateAirflowNetwork .lt. AirflowNetworkControlMultizone) return

   IF (BeginEnvrnFlag) THEN
     TurnFansOn = .FALSE. ! The FAN should be off when BeginEnvrnFlag = .True.
   End If

   CALL InitAirflowNetwork

   NetworkNumOfNodes = NumOfNodesMultiZone
   NetworkNumOfLinks = NumOfLinksMultiZone

   AirflowNetworkFanActivated = .FALSE.
   IF (PRESENT(FirstHVACIteration) .AND. SupplyFanInletNode > 0) THEN
     If (FirstHVACIteration) then
       LoopHeatingCoilMaxRTF =0.0d0
       LoopONOffFanRTF =0.0d0
       LoopDXCoilRTF =0.0d0
       LoopOnOffFanPartLoadRatio =0.0d0
     End If
!    Revised to meet heat exchanger requirement
     If ((Node(SupplyFanOutletNode)%MassFlowRate > VerySmallMassFlow) .AND. (.NOT. FirstHVACIteration)) then
       If (LoopFanOperationMode .EQ. CycFanCycCoil) Then
         If (LoopSystemOnMassFlowrate > 0) AirflowNetworkFanActivated = .TRUE.
       Else If (SupplyFanType .EQ. FanType_SimpleVAV) Then
         If (PRESENT(Iter) .AND. Iter > 1) AirflowNetworkFanActivated = .TRUE.
       Else If (AirflowNetworkUnitarySystem) Then
         If (PRESENT(Iter) .AND. Iter > 1) AirflowNetworkFanActivated = .TRUE.
       Else
         AirflowNetworkFanActivated = .TRUE.
       End If
     End If
   End If
   If (ALLOCATED(ZoneEquipConfig) .AND. NumHybridVentSysAvailMgrs > 0 .AND. ALLOCATED(PrimaryAirSystem)) &
      CALL HybridVentilationControl
   If (VentilationCtrl == 1 .AND. NumHybridVentSysAvailMgrs > 0) AirflowNetworkFanActivated = .FALSE.

   If (PRESENT(Iter) .AND. PRESENT(ResimulateAirZone)) then
     IF (AirflowNetworkFanActivated .and. Iter .LT. 3 .and. SupplyFanType .EQ. FanType_SimpleOnOff) then
       ResimulateAirZone = .TRUE.
     End If
     IF (SupplyFanType .EQ. FanType_SimpleVAV) Then
       IF (.NOT. AirflowNetworkFanActivated .AND. Iter .LT. 3) ResimulateAirZone = .TRUE.
     End If
     If (AirflowNetworkUnitarySystem) Then
       IF (.NOT. AirflowNetworkFanActivated .AND. Iter .LT. 3) ResimulateAirZone = .TRUE.
     End If
   End If
   if (AirflowNetworkFanActivated .and. SimulateAirflowNetwork > AirflowNetworkControlMultizone) then
      NetworkNumOfNodes = AirflowNetworkNumOfNodes
      NetworkNumOfLinks = AirflowNetworkNumOfLinks
   End if

   If (ALLOCATED(ZoneEquipConfig)) CALL ValidateExhaustFanInput

   ! VAV terminal set only
   If (PRESENT(FirstHVACIteration) .AND. FirstHVACIteration) VAVTerminalRatio = 0.d0

   if (AirflowNetworkFanActivated .and. SimulateAirflowNetwork > AirflowNetworkControlMultizone) then
     CALL ValidateDistributionSystem
   End If
   Call CalcAirflowNetworkAirBalance

   if (AirflowNetworkFanActivated .and. SimulateAirflowNetwork > AirflowNetworkControlMultizone) then
     Call CalcAirflowNetworkHeatBalance
     Call CalcAirflowNetworkMoisBalance
     IF (Contaminant%CO2Simulation) Call CalcAirflowNetworkCO2Balance
     IF (Contaminant%GenericContamSimulation) Call CalcAirflowNetworkGCBalance
   endif

   Call UpdateAirflownetwork(FirstHVACIteration)

  RETURN

END SUBROUTINE ManageAirflowNetworkBalance


SUBROUTINE GetAirflowNetworkInput


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Aug. 2003
          !       MODIFIED       Aug. 2005
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reads inputs of air distribution system


          ! METHODOLOGY EMPLOYED:
          ! na


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindIteminList, GetObjectItemNum, VerifyName,  &
                            GetObjectDefMaxArgs,SameString, MakeUPPERCase
  USE HVACHXAssistedCoolingCoil, ONLY: VerifyHeatExchangerParent
  USE DataHeatBalance, ONLY: People, TotPeople
  USE ThermalComfort, ONLY: ThermalComfortData

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=1), PARAMETER :: Blank=' '
    CHARACTER(len=*), PARAMETER :: fmta='(A)'
    CHARACTER(len=*), PARAMETER :: RoutineName='GetAirflowNetworkInput: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER i,n,j
    INTEGER count
    LOGICAL NodeFound, CompFound,ErrorsFound, found, FanErrorFound,NodeFound1,NodeFound2
    Integer NumAPL
    LOGICAL IsNotOK, IsBlank
    CHARACTER (len=MaxNameLength) CompName(2)
    CHARACTER(len=MaxNameLength) SimAirNetworkKey
    LOGICAL SimObjectError
    CHARACTER(len=MaxNameLength * 2) :: StringOut
    INTEGER ZoneNum
    Integer FanIndex, FanType_Num

! Declare variables used in this subroutine for debug purpose
    LOGICAL AirflowNetworkInitFlag
    INTEGER, ALLOCATABLE, DIMENSION(:) :: ZoneCheck
    INTEGER, ALLOCATABLE, DIMENSION(:) :: ZoneBCCheck
    LOGICAL SurfaceFound

    INTEGER                        :: NumAlphas               ! Number of Alphas for each GetObjectItem call
    INTEGER                        :: NumNumbers              ! Number of Numbers for each GetObjectItem call
    INTEGER                        :: IOStatus                ! Used in GetObjectItem
    CHARACTER(Len=MaxNameLength)   :: CurrentModuleObject
    CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas         ! Alpha input items for object
    CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
    CHARACTER(Len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
    REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers           ! Numeric input items for object
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
    LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.
    INTEGER                        :: MaxNums=0               ! Maximum number of numeric input fields
    INTEGER                        :: MaxAlphas=0             ! Maximum number of alpha input fields
    INTEGER                        :: TotalArgs=0             ! Total number of alpha and numeric arguments (max) for a

  ! Set the maximum numbers of input fields
  CALL GetObjectDefMaxArgs('AirflowNetwork:SimulationControl',TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs('AirflowNetwork:MultiZone:Zone',TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs('AirflowNetwork:MultiZone:Surface',TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs('AirflowNetwork:MultiZone:ReferenceCrackConditions',TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs('AirflowNetwork:MultiZone:Surface:Crack',TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs('AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea',TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs('AirflowNetwork:MultiZone:Component:DetailedOpening',TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs('AirflowNetwork:MultiZone:Component:SimpleOpening',TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs('AirflowNetwork:MultiZone:Component:ZoneExhaustFan',TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs('AirflowNetwork:MultiZone:ExternalNode',TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs('AirflowNetwork:MultiZone:WindPressureCoefficientArray',TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs('AirflowNetwork:MultiZone:WindPressureCoefficientValues',TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs('AirflowNetwork:Distribution:Node',TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs('AirflowNetwork:Distribution:Component:Leak',TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs('AirflowNetwork:Distribution:Component:LeakageRatio',TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs('AirflowNetwork:Distribution:Component:Duct',TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs('AirflowNetwork:Distribution:Component:Fan',TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs('AirflowNetwork:Distribution:Component:Coil',TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs('AirflowNetwork:Distribution:Component:TerminalUnit',TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs('AirflowNetwork:Distribution:Component:ConstantPressureDrop',TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs('AirflowNetwork:Distribution:Linkage',TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
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

  ErrorsFound = .FALSE.
  AirflowNetworkInitFlag = .FALSE.

! *** Read AirflowNetwork simulation parameters
  CurrentModuleObject='AirflowNetwork:SimulationControl'
  NumAirflowNetwork = GetNumObjectsFound(CurrentModuleObject)
  if (NumAirflowNetwork .EQ. 0) then
     SimulateAirflowNetwork = AirflowNetworkControlSimple
     WRITE(OutputFileInits,110)
     WRITE(OutputFileInits,120) 'NoMultizoneOrDistribution'
     Return
  end if
  if (NumAirflowNetwork .GT. 1) then
     CALL ShowFatalError(RoutineName//'Currently only one ("1") '//TRIM(CurrentModuleObject)//  &
                         ' object per simulation is allowed.')
  end if

  SimObjectError=.false.
  CALL GetObjectItem(CurrentModuleObject,NumAirflowNetwork,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                     NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                     AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

  AirflowNetworkSimu%AirflowNetworkSimuName = Alphas(1)
  AirflowNetworkSimu%Control     = Alphas(2)
  AirflowNetworkSimu%WPCCntr     = Alphas(3)
  AirflowNetworkSimu%CpArrayName = Alphas(4)
  AirflowNetworkSimu%HeightOption = Alphas(5)
  AirflowNetworkSimu%BldgType    = Alphas(6)

  ! Find a flag for possible combination of vent and distribution system
  SELECT CASE(MakeUPPERCase(AirflowNetworkSimu%Control))
    CASE ('NOMULTIZONEORDISTRIBUTION')
      SimulateAirflowNetwork = AirflowNetworkControlSimple
      SimAirNetworkKey = 'NoMultizoneOrDistribution'
    CASE ('MULTIZONEWITHOUTDISTRIBUTION')
      SimulateAirflowNetwork = AirflowNetworkControlMultizone
      SimAirNetworkKey = 'MultizoneWithoutDistribution'
    CASE ('MULTIZONEWITHDISTRIBUTIONONLYDURINGFANOPERATION')
      SimulateAirflowNetwork = AirflowNetworkControlSimpleADS
      SimAirNetworkKey = 'MultizoneWithDistributionOnlyDuringFanOperation'
    CASE ('MULTIZONEWITHDISTRIBUTION')
      SimulateAirflowNetwork = AirflowNetworkControlMultiADS
      SimAirNetworkKey = 'MultizoneWithDistribution'
    CASE DEFAULT  ! Error
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object, '//  &
                           'The entered choice for '//TRIM(cAlphaFields(2))//' is not valid = "'//AirflowNetworkSimu%Control//'"')
      CALL ShowContinueError('Valid choices are "NO MULTIZONE OR DISTRIBUTION",'//  &
                             '"MULTIZONE WITH DISTRIBUTION ONLY DURING FAN OPERATION"')
      CALL ShowContinueError('"MULTIZONE WITH DISTRIBUTION", or "MULTIZONE WITHOUT DISTRIBUTION"')
      CALL ShowContinueError('..specified in '//TRIM(CurrentModuleObject)//' '//TRIM(cAlphaFields(1))//' = '//  &
         Trim(AirflowNetworkSimu%AirflowNetworkSimuName))
      ErrorsFound=.true.
  END SELECT

! Check the number of primary air loops
  if (SimulateAirFlowNetwork == AirflowNetworkControlSimpleADS .or. SimulateAirFlowNetwork == AirflowNetworkControlMultiADS) THEN
     NumAPL = GetNumObjectsFound('AirLoopHVAC')
     if (NumAPL .NE. 1) then
       if (NumAPL .EQ. 0) then
         CALL ShowSevereError(RoutineName//'No AirLoopHVAC is found when '// TRIM(cAlphaFields(2))// &
                             ' = '//Trim(SimAirNetworkKey))
         CALL ShowContinueError('Please select a choice of MultizoneWithoutDistribution for '//TRIM(cAlphaFields(2)))
       else
         CALL ShowSevereError(RoutineName//'More AirLoopHVACs are found. Currently only one ("1") AirLoopHVAC'// &
                             ' object per simulation is allowed when using AirflowNetwork Distribution Systems')
       end if
       CALL ShowFatalError(RoutineName//'Errors found getting '//TRIM(CurrentModuleObject)//' object.'// &
                         ' Previous error(s) cause program termination.')
    end if
  end if

  WRITE(OutputFileInits,110)
  WRITE(OutputFileInits,120) Trim(SimAirNetworkKey)

110 Format('! <AirflowNetwork Model:Control>, No Multizone or Distribution/Multizone with Distribution/', &
           'Multizone without Distribution/Multizone with Distribution only during Fan Operation')
120 Format('AirflowNetwork Model:Control,',A)

  ! Check whether there are any objects from infiltration, ventilation, mixing and cross mixing
  If (SimulateAirflowNetwork == AirflowNetworkControlSimple .or. SimulateAirflowNetwork == AirflowNetworkControlSimpleADS) then
    If (TotInfiltration+TotVentilation+TotMixing+TotCrossMixing+TotZoneAirBalance+GetNumObjectsFound('ZoneEarthtube')+  &
        GetNumObjectsFound('ZoneThermalChimney')+GetNumObjectsFound('ZoneCoolTower:Shower') == 0) then
      CALL ShowWarningError(RoutineName//TRIM(cAlphaFields(2))//' = "'//Trim(SimAirNetworkKey)//'".')
      CALL ShowContinueError('..but there are no Infiltration, Ventilation, Mixing, Cross Mixing or ZoneAirBalance objects.'//  &
                             ' The simulation continues...')
    End If
  end if

  ! Check whether a user wants to perform SIMPLE calculation only or not
  If (SimulateAirflowNetwork == AirflowNetworkControlSimple) Return

  If (SimulateAirflowNetwork == AirflowNetworkControlMultizone .or. SimulateAirflowNetwork == AirflowNetworkControlMultiADS) then
    If (TotInfiltration > 0) then
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object, ')
        CALL ShowContinueError('..Specified '//TRIM(cAlphaFields(2))//' = "'//  &
                              TRIM(SimAirNetworkKey)//'" and ZoneInfiltration:* objects are present.')
        CALL ShowContinueError('..ZoneInfiltration objects will not be simulated.')
    End If
    If (TotVentilation > 0) then
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object, ')
        CALL ShowContinueError('..Specified '//TRIM(cAlphaFields(2))//' = "'//  &
                              TRIM(SimAirNetworkKey)//'" and ZoneVentilation:* objects are present.')
        CALL ShowContinueError('..ZoneVentilation objects will not be simulated.')
    End If
    If (TotMixing > 0) then
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object, ')
        CALL ShowContinueError('..Specified '//TRIM(cAlphaFields(2))//' = "'//  &
                              TRIM(SimAirNetworkKey)//'" and ZoneMixing objects are present.')
        CALL ShowContinueError('..ZoneMixing objects will not be simulated.')
    End If
    If (TotCrossMixing > 0) then
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object, ')
        CALL ShowContinueError('..Specified '//TRIM(cAlphaFields(2))//' = "'//  &
                              TRIM(SimAirNetworkKey)//'" and ZoneCrossMixing objects are present.')
        CALL ShowContinueError('..ZoneCrossMixing objects will not be simulated.')
    End If
    If (TotZoneAirBalance > 0) then
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object, ')
        CALL ShowContinueError('..Specified '//TRIM(cAlphaFields(2))//' = "'//  &
                              TRIM(SimAirNetworkKey)//'" and ZoneAirBalance:OutdoorAir objects are present.')
        CALL ShowContinueError('..ZoneAirBalance:OutdoorAir objects will not be simulated.')
    End If
    If (GetNumObjectsFound('ZoneEarthtube') > 0) then
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object, ')
        CALL ShowContinueError('..Specified '//TRIM(cAlphaFields(2))//' = "'//  &
                              TRIM(SimAirNetworkKey)//'" and ZoneEarthtube objects are present.')
        CALL ShowContinueError('..ZoneEarthtube objects will not be simulated.')
    End If
    If (GetNumObjectsFound('ZoneThermalChimney') > 0) then
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object, ')
        CALL ShowContinueError('..Specified '//TRIM(cAlphaFields(2))//' = "'//  &
                              TRIM(SimAirNetworkKey)//'" and ZoneThermalChimney objects are present.')
        CALL ShowContinueError('..ZoneThermalChimney objects will not be simulated.')
    End If
    If (GetNumObjectsFound('ZoneCoolTower:Shower') > 0) then
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object, ')
        CALL ShowContinueError('..Specified '//TRIM(cAlphaFields(2))//' = "'//  &
                              TRIM(SimAirNetworkKey)//'" and ZoneCoolTower:Shower objects are present.')
        CALL ShowContinueError('..ZoneCoolTower:Shower objects will not be simulated.')
    End If
  End If

  if (SameString(AirflowNetworkSimu%WPCCntr,'Input')) then
     AirflowNetworkSimu%iWPCCntr=iWPCCntr_Input
     if (lAlphaBlanks(4)) then
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object, '// TRIM(cAlphaFields(3))//' = INPUT.')
        CALL ShowContinueError('..'//TRIM(cAlphaFields(4))//' was not entered.')
        ErrorsFound=.true.
        SimObjectError=.true.
     end if
     if (lAlphaBlanks(5)) then
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object, '// TRIM(cAlphaFields(3))//' = INPUT.')
        CALL ShowContinueError('..'//TRIM(cAlphaFields(5))//' was not entered.')
        ErrorsFound=.true.
        SimObjectError=.true.
     Else
       if (.NOT. (SameString(AirflowNetworkSimu%HeightOption,'ExternalNode') .or. &
                  SameString(AirflowNetworkSimu%HeightOption,'OpeningHeight'))) then
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object, '//TRIM(cAlphaFields(5))//' = '// &
                               TRIM(Alphas(5))//' is invalid.')
          CALL ShowContinueError('Valid choices are ExternalNode or OpeningHeight. '//TRIM(CurrentModuleObject)//': '// &
                               TRIM(cAlphaFields(1))//' = '//TRIM(AirflowNetworkSimu%AirflowNetworkSimuName))
          ErrorsFound=.true.
          SimObjectError=.true.
       end if
     end if
!     if (AirflowNetworkSimu%BldgType /= ' ') then
!        CALL ShowMessage('GetAirflowNetworkInput: AirflowNetwork Wind Pressure Coefficient Type = INPUT.'// &
!         ' Building type = '//TRIM(AirflowNetworkSimu%BldgType)//' was entered but will not be used.')
!        AirflowNetworkSimu%BldgType    = ' '
!     end if
  else if (SameString(AirflowNetworkSimu%WPCCntr,'SurfaceAverageCalculation')) then
     AirflowNetworkSimu%iWPCCntr=iWPCCntr_SurfAvg
     if (.NOT. lAlphaBlanks(4)) then
        AirflowNetworkSimu%CpArrayName = ' '
!        CALL ShowWarningError('GetAirflowNetworkInput: AirflowNetwork Wind Pressure Coefficient Type '// &
!             '= SURFACE-AVERAGE CALCULATION.'// &
!             ' CP ARRAY NAME was entered but will not be used. The simulation continues...')
     end if
     if (.NOT. (SameString(AirflowNetworkSimu%BldgType,'LowRise') .or. &
                SameString(AirflowNetworkSimu%BldgType,'HighRise'))) then
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object, '//TRIM(cAlphaFields(6))//' = '// &
                             TRIM(Alphas(6))//' is invalid.')
        CALL ShowContinueError('Valid choices are LowRise or HighRise. '//TRIM(CurrentModuleObject)//': '// &
                               TRIM(cAlphaFields(1))//' = '//TRIM(AirflowNetworkSimu%AirflowNetworkSimuName))
        ErrorsFound=.true.
        SimObjectError=.true.
     end if
  else
     CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object, '//TRIM(cAlphaFields(3))//' = '// &
                          TRIM(AirflowNetworkSimu%WPCCntr)//' is not valid.')
     CALL ShowContinueError('Valid choices are Input or SurfaceAverageCalculation. '//TRIM(CurrentModuleObject)//' = ' &
                            //TRIM(AirflowNetworkSimu%AirflowNetworkSimuName))
     ErrorsFound=.true.
     SimObjectError=.true.
  end if

  AirflowNetworkSimu%InitType = Alphas(7)
  if (SameString(AirflowNetworkSimu%InitType,'LinearInitializationMethod')) then
    AirflowNetworkSimu%InitFlag = 0
  Else IF (SameString(AirflowNetworkSimu%InitType,'ZeroNodePressures')) then
    AirflowNetworkSimu%InitFlag = 1
  Else IF (SameString(AirflowNetworkSimu%InitType,'0')) then
    AirflowNetworkSimu%InitFlag = 0
  Else IF (SameString(AirflowNetworkSimu%InitType,'1')) then
    AirflowNetworkSimu%InitFlag = 1
  Else
    CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object, '//  &
                         ' '//TRIM(cAlphaFields(7))//' = '//TRIM(Alphas(7))//' is invalid.')
    CALL ShowContinueError('Valid choices are LinearInitializationMethod or ZeroNodePressures. '// &
                           TRIM(CurrentModuleObject)//' = '//TRIM(AirflowNetworkSimu%AirflowNetworkSimuName))
    ErrorsFound=.true.
    SimObjectError=.true.
  End If

  IF (SimObjectError) THEN
     CALL ShowFatalError(RoutineName//'Errors found getting '//TRIM(CurrentModuleObject)//' object.'// &
                         ' Previous error(s) cause program termination.')
  ENDIF

  AirflowNetworkSimu%MaxIteration = Numbers(1)
  AirflowNetworkSimu%RelTol = Numbers(2)
  AirflowNetworkSimu%AbsTol = Numbers(3)
  AirflowNetworkSimu%ConvLimit = Numbers(4)
  AirflowNetworkSimu%Azimuth = Numbers(5)
  AirflowNetworkSimu%AspectRatio = Numbers(6)
  AirflowNetworkSimu%MaxPressure = 500.0d0 ! Maximum pressure difference by default


! *** Read AirflowNetwork simulation zone data
  CurrentModuleObject='AirflowNetwork:MultiZone:Zone'
  AirflowNetworkNumOfZones = GetNumObjectsFound(CurrentModuleObject)
  if (AirflowNetworkNumOfZones > 0) then
   Allocate(MultizoneZoneData(AirflowNetworkNumOfZones))
   ALLOCATE(AirflowNetworkZoneFlag(NumOfZones)) ! AirflowNetwork zone flag
   AirflowNetworkZoneFlag = .False.
   Do i=1,AirflowNetworkNumOfZones
     CALL GetObjectItem(CurrentModuleObject,i,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                        NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                        AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
     IsNotOK=.false.
     IsBlank=.false.
     CALL VerifyName(Alphas(1),MultizoneZoneData%ZoneName,i-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
     IF (IsNotOK) THEN
       ErrorsFound=.true.
       IF (IsBlank) Alphas(1)='xxxxx'
     ENDIF
     MultizoneZoneData(i)%ZoneName     = Alphas(1)              ! Name of Associated EnergyPlus Thermal Zone
     If (.NOT. lAlphaBlanks(2)) &
       MultizoneZoneData(i)%VentControl  = Alphas(2)            ! Ventilation Control Mode: "Temperature", "Enthalpy",
                                                                ! "ASHRAE55ADAPTIVE", "CEN15251AdaptiveComfort,
                                                                ! "Constant", or "NoVent"
     MultizoneZoneData(i)%VentSchName  = Alphas(3)              ! Name of ventilation temperature control schedule
     MultizoneZoneData(i)%OpenFactor   = Numbers(1)             ! Limit Value on Multiplier for Modulating Venting Open Factor,
                                                                ! Not applicable if Vent Control Mode = CONSTANT or NOVENT
     MultizoneZoneData(i)%LowValueTemp = Numbers(2)             ! Lower Value on Inside/Outside Temperature Difference
                                                                ! for Modulating the Venting Open Factor with temp control
     MultizoneZoneData(i)%UpValueTemp  = Numbers(3)             ! Upper Value on Inside/Outside Temperature Difference
                                                                ! for Modulating the Venting Open Factor with temp control
     MultizoneZoneData(i)%LowValueEnth = Numbers(4)             ! Lower Value on Inside/Outside Temperature Difference
                                                                ! for Modulating the Venting Open Factor with Enthalpy control
     MultizoneZoneData(i)%UpValueEnth  = Numbers(5)             ! Upper Value on Inside/Outside Temperature Difference
                                                                ! for Modulating the Venting Open Factor with Enthalpy control
     MultizoneZoneData(i)%VentCtrNum = VentCtrNum_None
     if (SameString(MultizoneZoneData(i)%VentControl,'Temperature')) MultizoneZoneData(i)%VentCtrNum = VentCtrNum_Temp
     if (SameString(MultizoneZoneData(i)%VentControl,'Enthalpy')) MultizoneZoneData(i)%VentCtrNum = VentCtrNum_Enth
     if (SameString(MultizoneZoneData(i)%VentControl,'Constant')) MultizoneZoneData(i)%VentCtrNum = VentCtrNum_Const
     if (SameString(MultizoneZoneData(i)%VentControl,'ASHRAE55Adaptive')) MultizoneZoneData(i)%VentCtrNum = VentCtrNum_ASH55
     if (SameString(MultizoneZoneData(i)%VentControl,'CEN15251Adaptive')) MultizoneZoneData(i)%VentCtrNum = VentCtrNum_CEN15251
     if (SameString(MultizoneZoneData(i)%VentControl,'NoVent')) MultizoneZoneData(i)%VentCtrNum = VentCtrNum_Novent

     If (MultizoneZoneData(i)%VentCtrNum < 4) then
       If (NumAlphas == 4 .and. (.NOT. lAlphaBlanks(4))) then
         MultizoneZoneData(i)%VentingSchName = Alphas(4)
         MultizoneZoneData(i)%VentingSchNum = GetScheduleIndex(MultizoneZoneData(i)%VentingSchName)
         If (MultizoneZoneData(i)%VentingSchNum == 0) then
           CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object, '//Trim(cAlphaFields(4))//' not found = '//  &
                                Trim(MultizoneZoneData(i)%VentingSchName))
           CALL ShowContinueError('..for specified '//Trim(cAlphaFields(1))//' = '//TRIM(Alphas(1)))
           ErrorsFound = .true.
         end if
       End if
     Else
       MultizoneZoneData(i)%VentingSchName = Blank
       MultizoneZoneData(i)%VentingSchNum = 0
     End If
   end do
  Else
   CALL ShowSevereError(RoutineName//'For an AirflowNetwork Simulation, '//  &
                        'at least one '//TRIM(CurrentModuleObject)//' object is required but none were found.')
   ErrorsFound=.true.
  End If

! ==> Zone data validation
  Do i=1,AirflowNetworkNumOfZones
     ! Zone name validation
     MultizoneZoneData(i)%ZoneNum = FindIteminList(MultizoneZoneData(i)%ZoneName,Zone%Name,NumOfZones)
     IF (MultizoneZoneData(i)%ZoneNum == 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object, invalid '//TRIM(cAlphaFields(1))//' given.')
        CALL ShowContinueError('..invalid '//TRIM(cAlphaFields(1))//' = "' //TRIM(MultizoneZoneData(i)%ZoneName)//'"')
        ErrorsFound=.true.
     Else
        AirflowNetworkZoneFlag(MultizoneZoneData(i)%ZoneNum) = .True.
        MultizoneZoneData(i)%Height = Zone(MultizoneZoneData(i)%ZoneNum)%CENTROID%Z    ! Nodal height
     END IF
     if (MultizoneZoneData(i)%VentCtrNum == VentCtrNum_None) then
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object, invalid '//TRIM(cAlphaFields(2))//' = ' &
                             //TRIM(MultizoneZoneData(i)%VentControl))
        CALL ShowContinueError('Valid choices are Temperature, Enthalpy, Constant, or NoVent')
        CALL ShowContinueError('.. in '//TRIM(cAlphaFields(1))//' = "' //TRIM(MultizoneZoneData(i)%ZoneName)//'"')
        ErrorsFound=.true.
     end if
     if (SameString(MultizoneZoneData(i)%VentControl,'Temperature') .or. &
         SameString(MultizoneZoneData(i)%VentControl,'Enthalpy')) then
         ! .or. &
         !SameString(MultizoneZoneData(i)%VentControl,'ASHRAE55Adaptive') .or. &
         !SameString(MultizoneZoneData(i)%VentControl,'CEN15251Adaptive')) then
        MultizoneZoneData(i)%VentSchNum = GetScheduleIndex(MultizoneZoneData(i)%VentSchName)
        if (MultizoneZoneData(i)%VentSchName == Blank) THEN
           CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object, '//  &
                                'No '//TRIM(cAlphaFields(3))//' was found, but is required when '//TRIM(cAlphaFields(2))// &
                                ' is Temperature or Enthalpy.')
           CALL ShowContinueError('..for '//TRIM(cAlphaFields(1))//' = "'//TRIM(MultizoneZoneData(i)%ZoneName)// &
                                  '", with '//TRIM(cAlphaFields(2))//' = "'//TRIM(MultizoneZoneData(i)%VentControl)//'"')
           ErrorsFound=.true.
        elseif (MultizoneZoneData(i)%VentSchNum == 0) then
           CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object, invalid '//Trim(cAlphaFields(3))//', '// &
                                ' required when '//TRIM(cAlphaFields(2))//' is Temperature or Enthalpy.')
           CALL ShowContinueError('..'//TRIM(cAlphaFields(3))//' in error = '//TRIM(MultizoneZoneData(i)%VentSchName))
           CALL ShowContinueError('..for '//TRIM(cAlphaFields(1))//' = "'//TRIM(MultizoneZoneData(i)%ZoneName)//'", with '// &
                                  TRIM(cAlphaFields(2))//' = "'//TRIM(MultizoneZoneData(i)%VentControl)//'"')
          ErrorsFound = .true.
        end if
     else
        MultizoneZoneData(i)%VentSchNum = GetScheduleIndex(MultizoneZoneData(i)%VentSchName)
        IF (MultizoneZoneData(i)%VentSchNum > 0) THEN
           CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object, '//TRIM(cAlphaFields(3))//  &
                                 ' not required, when '//TRIM(cAlphaFields(2))//' is neither Temperature nor Enthalpy.')
           CALL ShowContinueError('..'//TRIM(cAlphaFields(3))//' specified = '//TRIM(MultizoneZoneData(i)%VentSchName))
           CALL ShowContinueError('..for '//TRIM(cAlphaFields(1))//' = "'//TRIM(MultizoneZoneData(i)%ZoneName)//'", with '//&
                                  TRIM(cAlphaFields(2))//' = "'//TRIM(MultizoneZoneData(i)%VentControl)//'"')
           MultizoneZoneData(i)%VentSchNum = 0
           MultizoneZoneData(i)%VentSchName = Blank
        End if
     end if
     if (MultizoneZoneData(i)%OpenFactor > 1.0d0 .or. MultizoneZoneData(i)%OpenFactor < 0.0d0) then
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object, '//  &
                              TRIM(cNumericFields(1))//' is out of range [0.0,1.0]')
        CALL ShowContinueError('..Input value = '//TRIM(RoundSigDigits(MultizoneZoneData(i)%OpenFactor,2))//  &
                               ', Value will be set to 1.0')
        MultizoneZoneData(i)%OpenFactor = 1.0d0
     end if

     SELECT CASE (MakeUPPERCase(MultizoneZoneData(i)%VentControl))
       CASE ('TEMPERATURE')  ! checks on Temperature control
        if (MultizoneZoneData(i)%LowValueTemp < 0.0d0) then
           CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object, '//  &
                                 TRIM(cNumericFields(2))//' < 0.0')
           CALL ShowContinueError('..Input value = '//TRIM(RoundSigDigits(MultizoneZoneData(i)%LowValueTemp,1))//  &
                                  ', Value will be set to 0.0')
           CALL ShowContinueError('..for '//TRIM(cAlphaFields(1))//' = "'//TRIM(MultizoneZoneData(i)%ZoneName))
           MultizoneZoneData(i)%LowValueTemp = 0.0d0
        end if
        if (MultizoneZoneData(i)%LowValueTemp >= 100.0d0) then
           CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object, '//  &
                TRIM(cNumericFields(2))//' >= 100.0')
           CALL ShowContinueError('..Input value = '//TRIM(RoundSigDigits(MultizoneZoneData(i)%LowValueTemp,1))//  &
                                  ', Value will be reset to 0.0')
           CALL ShowContinueError('..for '//TRIM(cAlphaFields(1))//' = "'//TRIM(MultizoneZoneData(i)%ZoneName))
           MultizoneZoneData(i)%LowValueTemp = 0.0d0
        end if
        if (MultizoneZoneData(i)%UpValueTemp <= MultizoneZoneData(i)%LowValueTemp) then
           CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object, '//  &
                TRIM(cNumericFields(3))//' <= '//Trim(cNumericFields(2)))
           CALL ShowContinueError('..Input value for '//TRIM(cNumericFields(3))//' = '//  &
                                  TRIM(RoundSigDigits(MultizoneZoneData(i)%UpValueTemp,1))//', Value will be reset to 100.0')
           CALL ShowContinueError('..for '//TRIM(cAlphaFields(1))//' = "'//TRIM(MultizoneZoneData(i)%ZoneName))
           MultizoneZoneData(i)%UpValueTemp = 100.0d0
        end if

      CASE ('ENTHALPY')  ! checks for Enthalpy control
        if (MultizoneZoneData(i)%LowValueEnth < 0.0d0) then
           CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object, '//  &
                TRIM(cNumericFields(4))//' < 0.0')
           CALL ShowContinueError('..Input value = '//TRIM(RoundSigDigits(MultizoneZoneData(i)%LowValueEnth,1))//  &
                                  ', Value will be reset to 0.0')
           CALL ShowContinueError('..for '//TRIM(cAlphaFields(1))//' = "'//TRIM(MultizoneZoneData(i)%ZoneName))
           MultizoneZoneData(i)%LowValueEnth = 0.0d0
        end if
        if (MultizoneZoneData(i)%LowValueEnth >= 300000.0d0) then
           CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object, '//  &
                                 TRIM(cNumericFields(4))//' >= 300000.0')
           CALL ShowContinueError('..Input value = '//TRIM(RoundSigDigits(MultizoneZoneData(i)%LowValueEnth,1))//  &
                                  ', Value will be reset to 0.0.')
           CALL ShowContinueError('..for '//TRIM(cAlphaFields(1))//' = "'//TRIM(MultizoneZoneData(i)%ZoneName))
           MultizoneZoneData(i)%LowValueEnth = 0.0d0
        end if
        if (MultizoneZoneData(i)%UpValueEnth <= MultizoneZoneData(i)%LowValueEnth) then
           CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object, '//  &
                                 TRIM(cNumericFields(5))//' <= '//TRIM(cNumericFields(4)))
           CALL ShowContinueError('..Input value for '//TRIM(cNumericFields(5))//'= '//  &
                                  TRIM(RoundSigDigits(MultizoneZoneData(i)%UpValueEnth,1))//', Value will be reset to 300000.0')
           CALL ShowContinueError('..for '//TRIM(cAlphaFields(1))//' = "'//TRIM(MultizoneZoneData(i)%ZoneName))
           MultizoneZoneData(i)%UpValueEnth = 300000.0d0
        end if
      CASE ('ASHRAE55ADAPTIVE')
        ! Check that for the given zone, there is a people object for which ASHRAE 55 calculations are carried out
        ZoneNum = MultizoneZoneData(i)%ZoneNum
        DO j=1,TotPeople
          IF (ZoneNum == People(j)%ZonePtr .and. People(j)%AdaptiveASH55) THEN
            MultizoneZoneData(i)%ASH55PeopleInd = j
          END IF
        END DO
        IF (MultizoneZoneData(i)%ASH55PeopleInd==0) THEN
          CALL ShowFatalError('ASHRAE55 ventilation control for zone '//TRIM(MultizoneZoneData(i)%ZoneName)// &
                              ' requires a people object with respective model calculations.')
        END IF
      CASE ('CEN15251ADAPTIVE')
      ! Check that for the given zone, there is a people object for which CEN-15251 calculations are carried out
        ZoneNum = MultizoneZoneData(i)%ZoneNum
        DO j=1,TotPeople
          IF (ZoneNum == People(j)%ZonePtr .and. People(j)%AdaptiveCEN15251) THEN
            MultizoneZoneData(i)%CEN15251PeopleInd = j
            EXIT
          END IF
        END DO
        IF (MultizoneZoneData(i)%CEN15251PeopleInd==0) THEN
          CALL ShowFatalError('CEN15251 ventilation control for zone '//TRIM(MultizoneZoneData(i)%ZoneName)// &
                              ' requires a people object with respective model calculations.')
        END IF
      CASE DEFAULT
      END SELECT

  End Do

! *** Read AirflowNetwork external node
  if (AirflowNetworkSimu%iWPCCntr == iWPCCntr_Input) then
   ! Wind coefficient == Surface-Average does not need inputs of external nodes
   CurrentModuleObject='AirflowNetwork:MultiZone:ExternalNode'
   AirflowNetworkNumOfExtNode = GetNumObjectsFound(CurrentModuleObject)
   If (AirflowNetworkNumOfExtNode > 0) then
    Allocate(MultizoneExternalNodeData(AirflowNetworkNumOfExtNode))
    Do i=1,AirflowNetworkNumOfExtNode
     CALL GetObjectItem(CurrentModuleObject,i,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
     IsNotOK=.false.
     IsBlank=.false.
     CALL VerifyName(Alphas(1),MultizoneExternalNodeData%Name,i-1,IsNotOK,IsBlank, &
                    TRIM(CurrentModuleObject)//' Name')
     IF (IsNotOK) THEN
       ErrorsFound=.true.
       IF (IsBlank) Alphas(1)='xxxxx'
     ENDIF
     MultizoneExternalNodeData(i)%Name   = Alphas(1)       ! Name of external node
     MultizoneExternalNodeData(i)%height = Numbers(1)      ! Nodal height
     If (SameString(AirflowNetworkSimu%HeightOption,'ExternalNode') .AND. lNumericBlanks(1)) Then
       CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object ='//TRIM(Alphas(1))// &
          '. The input of ' //TRIM(cNumericFields(1))//' is required, but a blank is found.')
       CALL ShowContinueError('The default value is assigned as '//TRIM(RoundSigDigits(Numbers(1),1)))
     End If
     MultizoneExternalNodeData(i)%ExtNum = AirflowNetworkNumOfZones+i  ! External node number
     MultizoneExternalNodeData(i)%WPCName = Alphas(2)      ! Name of Wind Pressure Coefficient Values Object
    End Do
   else
     CALL ShowSevereError(RoutineName//'An '//TRIM(CurrentModuleObject)// &
                          ' object is required but not found when Wind Pressure Coefficient Type = Input.')
     ErrorsFound=.true.
   end if
  end if


! *** Read AirflowNetwork simulation surface data
  CurrentModuleObject='AirflowNetwork:MultiZone:Surface'
  AirflowNetworkNumOfSurfaces = GetNumObjectsFound(CurrentModuleObject)
  if (AirflowNetworkNumOfSurfaces > 0) then
   Allocate(MultizoneSurfaceData(AirflowNetworkNumOfSurfaces))
   Do i=1,AirflowNetworkNumOfSurfaces
     CALL GetObjectItem(CurrentModuleObject,i,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                        NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                        AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
     IsNotOK=.false.
     IsBlank=.false.
     CALL VerifyName(Alphas(1),MultizoneSurfaceData%SurfName,i-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
     IF (IsNotOK) THEN
       CALL ShowContinueError('..only 1 crack per surface is allowed, opening/crack component = '//TRIM(Alphas(2)))
       ErrorsFound=.true.
       IF (IsBlank) Alphas(1)='xxxxx'
     ENDIF
     MultizoneSurfaceData(i)%SurfName      = Alphas(1)             ! Name of Associated EnergyPlus surface
     MultizoneSurfaceData(i)%OpeningName   = Alphas(2)             ! Name of crack or opening component,
                                                                   ! either simple or detailed large opening, or crack
     MultizoneSurfaceData(i)%ExternalNodeName = Alphas(3)          ! Name of external node, but not used at WPC="INPUT"
     MultizoneSurfaceData(i)%Factor        = Numbers(1)            ! Crack Actual Value or Window Open Factor for Ventilation
     if (MultizoneSurfaceData(i)%Factor > 1.0d0 .or. MultizoneSurfaceData(i)%Factor .LE. 0.0d0) then
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object='//TRIM(MultizoneSurfaceData(i)%SurfName)//', '  &
                              //TRIM(cNumericFields(1))//' is out of range (0.0,1.0]')
        CALL ShowContinueError('..Input value = '//TRIM(RoundSigDigits(MultizoneSurfaceData(i)%Factor,2))//  &
                               ', Value will be set to 1.0')
        MultizoneSurfaceData(i)%Factor = 1.0d0
     end if
     ! Get input of ventilation control and associated data
     If (NumAlphas >= 4) then
       ! Ventilation Control Mode: "TEMPERATURE", "ENTHALPY",
       !   "CONSTANT", "ZONELEVEL", "NOVENT", "ADJACENTTEMPERATURE",
       !   or "ADJACENTENTHALPY"
       IF (.not. lAlphaBlanks(4)) MultizoneSurfaceData(i)%VentControl = Alphas(4)
       ! Name of ventilation temperature control schedule
       IF (.not. lAlphaBlanks(5)) MultizoneSurfaceData(i)%VentSchName = Alphas(5)
       SELECT CASE (MakeUPPERCase(MultizoneSurfaceData(i)%VentControl))
         CASE ('TEMPERATURE')
           MultizoneSurfaceData(i)%VentSurfCtrNum = VentCtrNum_Temp
           MultizoneSurfaceData(i)%IndVentControl = .TRUE.
         CASE ('ENTHALPY')
           MultizoneSurfaceData(i)%VentSurfCtrNum = VentCtrNum_Enth
           MultizoneSurfaceData(i)%IndVentControl = .TRUE.
         CASE ('CONSTANT')
           MultizoneSurfaceData(i)%VentSurfCtrNum = VentCtrNum_Const
           MultizoneSurfaceData(i)%IndVentControl = .TRUE.
         CASE ('ASHRAE55ADAPTIVE')
           MultizoneSurfaceData(i)%VentSurfCtrNum = VentCtrNum_ASH55
           MultizoneSurfaceData(i)%IndVentControl = .TRUE.
         CASE('CEN15251ADAPTIVE')
           MultizoneSurfaceData(i)%VentSurfCtrNum = VentCtrNum_CEN15251
           MultizoneSurfaceData(i)%IndVentControl = .TRUE.
         CASE ('NOVENT')
           MultizoneSurfaceData(i)%VentSurfCtrNum = VentCtrNum_Novent
           MultizoneSurfaceData(i)%IndVentControl = .TRUE.
         CASE ('ZONELEVEL')
           MultizoneSurfaceData(i)%VentSurfCtrNum = VentCtrNum_ZoneLevel
           MultizoneSurfaceData(i)%IndVentControl = .FALSE.
         CASE ('ADJACENTTEMPERATURE')
           MultizoneSurfaceData(i)%VentSurfCtrNum = VentCtrNum_AdjTemp
           MultizoneSurfaceData(i)%IndVentControl = .TRUE.
         CASE ('ADJACENTENTHALPY')
           MultizoneSurfaceData(i)%VentSurfCtrNum = VentCtrNum_AdjEnth
           MultizoneSurfaceData(i)%IndVentControl = .TRUE.
         CASE DEFAULT
           CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object, '//  &
                                'Invalid '//TRIM(cAlphaFields(4)))
           CALL ShowContinueError('..'//TRIM(cAlphaFields(1))//' = '//TRIM(MultizoneSurfaceData(i)%SurfName)//', Specified '// &
                                  TRIM(cAlphaFields(4))//' = '//TRIM(Alphas(4)))
           CALL ShowContinueError('..The valid choices are "Temperature", "Enthalpy", "Constant", "NoVent", "ZoneLevel", '// &
                                  '"AdjancentTemperature" or "AdjacentEnthalpy"')
           ErrorsFound=.true.
        END SELECT
     End if
     MultizoneSurfaceData(i)%ModulateFactor = Numbers(2)        ! Limit Value on Multiplier for Modulating Venting Open Factor
     MultizoneSurfaceData(i)%LowValueTemp   = Numbers(3)        ! Lower temperature value for modulation of temperature control
     MultizoneSurfaceData(i)%UpValueTemp    = Numbers(4)        ! Upper temperature value for modulation of temperature control
     MultizoneSurfaceData(i)%LowValueEnth   = Numbers(5)        ! Lower Enthalpy value for modulation of Enthalpy control
     MultizoneSurfaceData(i)%UpValueEnth    = Numbers(6)        ! Lower Enthalpy value for modulation of Enthalpy control
     If (MultizoneSurfaceData(i)%VentSurfCtrNum < 4) then
       If (.NOT. lAlphaBlanks(6)) then
         MultizoneSurfaceData(i)%VentingSchName = Alphas(6)     ! Name of ventilation availability schedule
       End If
     End if
   end do
  Else
   CALL ShowSevereError(RoutineName//'An '//TRIM(CurrentModuleObject)//' object is required but not found.')
   ErrorsFound=.true.
  End if


! ==> Validate AirflowNetwork simulation surface data
  NumOfExtNodes = 0
  Do i=1,AirflowNetworkNumOfSurfaces
     ! Check a valid surface defined earlier
     MultizoneSurfaceData(i)%SurfNum = FindIteminList(MultizoneSurfaceData(i)%SurfName,Surface%Name,TotSurfaces)
     if (MultizoneSurfaceData(i)%SurfNum == 0) then
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object, Invalid '//TRIM(cAlphaFields(1))//' given = ' &
                             //TRIM(MultizoneSurfaceData(i)%SurfName))
        ErrorsFound=.true.
        cycle
     end if
     if (.NOT. Surface(MultizoneSurfaceData(i)%SurfNum)%HeatTransSurf) then
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object')
        CALL ShowContinueError('..The surface specified must be a heat transfer surface. Invalid '// &
                               TRIM(cAlphaFields(1))//' = '//TRIM(MultizoneSurfaceData(i)%SurfName))
        ErrorsFound=.true.
        cycle
     end if
     ! Ensure an interior surface does not face itself
     If (Surface(MultizoneSurfaceData(i)%SurfNum)%ExtBoundCond >= 1) then
       ! Check the surface is a subsurface or not
       If (Surface(MultizoneSurfaceData(i)%SurfNum)%BaseSurf == MultizoneSurfaceData(i)%SurfNum) then
         If (MultizoneSurfaceData(i)%SurfNum == Surface(MultizoneSurfaceData(i)%SurfNum)%ExtBoundCond) then
           CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object')
           CALL ShowContinueError('..The surface facing itself is not allowed. Invalid '// &
                                  TRIM(cAlphaFields(1))//' = '//TRIM(MultizoneSurfaceData(i)%SurfName))
           ErrorsFound=.true.
         End If
       Else
         If (Surface(MultizoneSurfaceData(i)%SurfNum)%BaseSurf == &
             Surface(Surface(MultizoneSurfaceData(i)%SurfNum)%BaseSurf)%ExtBoundCond) then
           CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object')
           CALL ShowContinueError('..The base surface facing itself is not allowed. Invalid '// &
                                  TRIM(cAlphaFields(1))//' = '//TRIM(MultizoneSurfaceData(i)%SurfName))
           ErrorsFound=.true.
         End If
       End If
     End If
     ! Ensure zones defined in inside and outside environment are used in the object of AIRFLOWNETWORK:MULTIZONE:ZONE
     found = .FALSE.
     n = Surface(MultizoneSurfaceData(i)%SurfNum)%Zone
     Do j=1,AirflowNetworkNumOfZones
        If (MultizoneZoneData(j)%ZoneNum .eq. n) then
           Found = .TRUE.
           Exit
        End if
     End do
     ! find a surface geometry
     MultizoneSurfaceData(i)%Height = Surface(MultizoneSurfaceData(i)%SurfNum)%Height
     MultizoneSurfaceData(i)%Width = Surface(MultizoneSurfaceData(i)%SurfNum)%Width
     MultizoneSurfaceData(i)%CHeight = Surface(MultizoneSurfaceData(i)%SurfNum)%Centroid%Z
     If (found) then
       MultizoneSurfaceData(i)%NodeNums(1) = j
     Else
        CALL ShowSevereError(RoutineName// TRIM(CurrentModuleObject)//' object, '//TRIM(cAlphaFields(1))//' = '// &
                             TRIM(MultizoneSurfaceData(i)%SurfName))
        CALL ShowContinueError('..Zone for inside surface must be defined in a AirflowNetwork:MultiZone:Zone object.  '//  &
                               'Could not find Zone = '//TRIM(Zone(Surface(MultizoneSurfaceData(i)%SurfNum)%Zone)%Name))
        ErrorsFound=.true.
        Cycle
     End if

     ! Get the number of external surfaces
     If (Surface(MultizoneSurfaceData(i)%SurfNum)%ExtBoundCond == ExternalEnvironment) Then
       AirflowNetworkNumOfExtSurfaces = AirflowNetworkNumOfExtSurfaces + 1
     End If

     ! Outside face environment
     if (AirflowNetworkSimu%iWPCCntr == iWPCCntr_Input) then
      n = Surface(MultizoneSurfaceData(i)%SurfNum)%ExtBoundCond
      if (n == ExternalEnvironment) then
        NumOfExtNodes = NumOfExtNodes+1
        if (AirflowNetworkNumOfExtNode > 0) then
           found = .False.
           do j=1,AirflowNetworkNumOfExtNode
              if (SameString(MultizoneSurfaceData(i)%ExternalNodeName,MultizoneExternalNodeData(j)%Name)) then
                 MultizoneSurfaceData(i)%NodeNums(2) = MultizoneExternalNodeData(j)%ExtNum
                 found = .True.
                 Exit
              end if
           end do
           if (.NOT. found) then
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': Invalid '//TRIM(cAlphaFields(3))//' = ' &
                                   //TRIM(MultizoneSurfaceData(i)%ExternalNodeName))
              CALL ShowContinueError('A valid '//TRIM(cAlphaFields(3))//' is required when Wind Pressure Coefficient Type = Input')
              ErrorsFound=.true.
           end if
        else
!          MultizoneSurfaceData(i)%NodeNums(2) = AirflowNetworkNumOfZones+NumOfExtNodes
        end if
        cycle
      else
        if (n < ExternalEnvironment) then
           CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': Invalid '//TRIM(cAlphaFields(1))//' = '// &
                                TRIM(MultizoneSurfaceData(i)%SurfName))
           CALL ShowContinueError('This type of surface (has ground, etc exposure) cannot be used in the AiflowNetwork model.')
           ErrorsFound=.true.
        end if
      end if
      found = .FALSE.
      Do j=1,AirflowNetworkNumOfZones
        If (MultizoneZoneData(j)%ZoneNum .eq. Surface(n)%Zone) then
           Found = .TRUE.
           Exit
        End if
      End do
      If (found) then
        MultizoneSurfaceData(i)%NodeNums(2) = j
      Else
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object, '//TRIM(cAlphaFields(1))//' = '// &
                             TRIM(MultizoneSurfaceData(i)%SurfName))
        CALL ShowContinueError('..Zone for outside surface must be defined in a AirflowNetwork:MultiZone:Zone object.  '//  &
                               'Could not find Zone = '//TRIM(Zone(Surface(MultizoneSurfaceData(i)%SurfNum)%Zone)%Name))
        ErrorsFound=.true.
        Cycle
      End if
     End IF
     if (SameString(AirflowNetworkSimu%WPCCntr,'SurfaceAverageCalculation')) then
      n = Surface(MultizoneSurfaceData(i)%SurfNum)%ExtBoundCond
      if (n >= 1) then ! exterior boundary condition is a surface
        found = .FALSE.
        Do j=1,AirflowNetworkNumOfZones
          If (MultizoneZoneData(j)%ZoneNum .eq. Surface(n)%Zone) then
            Found = .TRUE.
            Exit
          End if
        End do
        If (found) then
          MultizoneSurfaceData(i)%NodeNums(2) = j
        Else
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(MultizoneSurfaceData(i)%SurfName))
          CALL ShowContinueError('..Zone = '//TRIM(Zone(Surface(MultizoneSurfaceData(i)%SurfNum)%Zone)%Name)//  &
                 ' is not described in AIRFLOWNETWORK:MULTIZONE:ZONE')
          ErrorsFound=.true.
          Cycle
        End if
      End if
     end if
  end do

  ! Validate adjacent temperature and Enthalpy control for an interior surface only
  Do i=1,AirflowNetworkNumOfSurfaces
     If (MultizoneSurfaceData(i)%VentSurfCtrNum == VentCtrNum_AdjTemp) then
        If (.not. Surface(MultizoneSurfaceData(i)%SurfNum)%ExtBoundCond >= 1) then
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object, '//TRIM(cAlphaFields(1))//' = '// &
                               TRIM(MultizoneSurfaceData(i)%SurfName))
          CALL ShowContinueError('..AdjacentTemperature venting control must be defined for an interzone surface.')
          ErrorsFound=.true.
        End If
     End If
     If (MultizoneSurfaceData(i)%VentSurfCtrNum == VentCtrNum_AdjEnth) then
        If (.not. Surface(MultizoneSurfaceData(i)%SurfNum)%ExtBoundCond >= 1) then
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object, '//TRIM(cAlphaFields(1))//' = '// &
                               TRIM(MultizoneSurfaceData(i)%SurfName))
          CALL ShowContinueError('..AdjacentEnthalpy venting control must be defined for an interzone surface.')
          ErrorsFound=.true.
        End If
     End If
  end do

! Ensure the number of external node = the number of external surface with HeightOption choice = OpeningHeight
  If (SameString(AirflowNetworkSimu%HeightOption,'OpeningHeight') .AND. AirflowNetworkSimu%iWPCCntr == iWPCCntr_Input) Then
    If (AirflowNetworkNumOfExtSurfaces .NE. AirflowNetworkNumOfExtNode) Then
      CALL ShowSevereError(RoutineName// 'When the choice of Height Selection for Local Wind Speed Calculation is OpeningHeight,' &
      //' the number of external surfaces defined in '//Trim(CurrentModuleObject)//' objects ')
      CALL ShowContinueError('has to be equal to the number of AirflowNetwork:MultiZone:ExternalNode objects.')
      CALL ShowContinueError('The entered number of external nodes is '//TRIM(RoundSigDigits(AirflowNetworkNumOfExtNode,0))// &
                   '. The entered number of external surfaces is '//TRIM(RoundSigDigits(AirflowNetworkNumOfExtSurfaces,0))//'.')
      ErrorsFound=.true.
    End If
  End If

! Read AirflowNetwork simulation detailed openings
  CurrentModuleObject='AirflowNetwork:MultiZone:Component:DetailedOpening'
  AirflowNetworkNumOfDetOpenings = GetNumObjectsFound(CurrentModuleObject)
  If (AirflowNetworkNumOfDetOpenings > 0) then
   Allocate(MultizoneCompDetOpeningData(AirflowNetworkNumOfDetOpenings))
   Do i=1,AirflowNetworkNumOfDetOpenings
     CALL GetObjectItem(CurrentModuleObject,i,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                        NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                        AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
     IsNotOK=.false.
     IsBlank=.false.
     CALL VerifyName(Alphas(1),MultizoneCompDetOpeningData%Name,i-1,IsNotOK,IsBlank, &
                     TRIM(CurrentModuleObject)//' Name')
     IF (IsNotOK) THEN
       ErrorsFound=.true.
       IF (IsBlank) Alphas(1)='xxxxx'
     ENDIF
     MultizoneCompDetOpeningData(i)%Name     = Alphas(1)       ! Name of large detailed opening component
     MultizoneCompDetOpeningData(i)%FlowCoef = Numbers(1)      ! Air Mass Flow Coefficient When Window or Door Is Closed
     MultizoneCompDetOpeningData(i)%FlowExpo = Numbers(2)      ! Air Mass Flow exponent When Window or Door Is Closed
     MultizoneCompDetOpeningData(i)%TypeName = Alphas(2)       ! Large vertical opening type
     if (SameString(Alphas(2),'NonPivoted') .or. SameString(Alphas(2),'1')) then
       MultizoneCompDetOpeningData(i)%LVOType = 1              ! Large vertical opening type number
     Else If (SameString(Alphas(2),'HorizontallyPivoted') .or. SameString(Alphas(2),'2')) then
       MultizoneCompDetOpeningData(i)%LVOType = 2              ! Large vertical opening type number
     Else
       CALL ShowSevereError(RoutineName//'Invalid '//TRIM(cAlphaFields(2))//' = '//TRIM(Alphas(2))//  &
                            'in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
       CALL ShowContinueError('Valid choices are NonPivoted and HorizontallyPivoted.')
       ErrorsFound=.true.
     End If
     MultizoneCompDetOpeningData(i)%LVOValue = Numbers(3)      ! Extra crack length for LVO type 1 with multiple openable parts,
                                                               ! or Height of pivoting axis for LVO type 2
     If (MultizoneCompDetOpeningData(i)%LVOValue <0) then
       CALL ShowSevereError(RoutineName//'Negative values are not allowed for '//TRIM(cNumericFields(3))// &
                            ' in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
       CALL ShowContinueError('The input value is '//TRIM(RoundSigDigits(Numbers(3),2)))
       ErrorsFound=.true.
     End If
     MultizoneCompDetOpeningData(i)%NumFac = Numbers(4)        ! Number of Opening Factor Values

     MultizoneCompDetOpeningData(i)%OpenFac1 = Numbers(5)      ! Opening factor #1
     If (MultizoneCompDetOpeningData(i)%OpenFac1> 0.0d0) then
       CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
       CALL ShowContinueError('..The value of '//TRIM(cNumericFields(5))//' is reset to 0.0')
       CALL ShowContinueError('..Input value was '//TRIM(RoundSigDigits(MultizoneCompDetOpeningData(i)%OpenFac1,2)))
       MultizoneCompDetOpeningData(i)%OpenFac1 = 0.0d0
     End If
     MultizoneCompDetOpeningData(i)%DischCoeff1 = Numbers(6)   ! Discharge coefficient for opening factor #1
     If (MultizoneCompDetOpeningData(i)%DischCoeff1 .le. 0.0d0) then
       CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
       CALL ShowContinueError('The value of '//TRIM(cNumericFields(6))//' is less than or equal to 0. '// &
                              'A value greater than zero is required.')
       ErrorsFound=.true.
     End If
     MultizoneCompDetOpeningData(i)%WidthFac1  = Numbers(7)    ! Width factor for for Opening factor #1
     MultizoneCompDetOpeningData(i)%HeightFac1 = Numbers(8)    ! Height factor for opening factor #1
     MultizoneCompDetOpeningData(i)%StartHFac1 = Numbers(9)    ! Start height factor for opening factor #1
     MultizoneCompDetOpeningData(i)%OpenFac2   = Numbers(10)   ! Opening factor #2
     If (MultizoneCompDetOpeningData(i)%OpenFac2 .NE. 1.0d0 .and. MultizoneCompDetOpeningData(i)%NumFac == 2) then
       CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
       CALL ShowContinueError('..This object specifies that only 2 opening factors will be used. So, the value '// &
                              'of '//TRIM(cNumericFields(10))//' is reset to 1.0.')
       CALL ShowContinueError('..Input value was '//TRIM(RoundSigDigits(MultizoneCompDetOpeningData(i)%OpenFac2,2)))
       MultizoneCompDetOpeningData(i)%OpenFac2 = 1.0d0
     End If
     MultizoneCompDetOpeningData(i)%DischCoeff2 = Numbers(11)  ! Discharge coefficient for opening factor #2
     MultizoneCompDetOpeningData(i)%WidthFac2   = Numbers(12)  ! Width factor for for Opening factor #2
     MultizoneCompDetOpeningData(i)%HeightFac2  = Numbers(13)  ! Height factor for opening factor #2
     MultizoneCompDetOpeningData(i)%StartHFac2  = Numbers(14)  ! Start height factor for opening factor #2
     If (MultizoneCompDetOpeningData(i)%DischCoeff2 .le. 0.0d0) then
       CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
       CALL ShowContinueError('The value of '//TRIM(cNumericFields(11))//' is less than or equal to 0. '// &
                              'A value greater than zero is required.')
       ErrorsFound=.true.
     End If
     MultizoneCompDetOpeningData(i)%OpenFac3    = 0.0d0  ! Opening factor #3
     MultizoneCompDetOpeningData(i)%DischCoeff3 = 0.0d0  ! Discharge coefficient for opening factor #3
     MultizoneCompDetOpeningData(i)%WidthFac3   = 0.0d0  ! Width factor for for Opening factor #3
     MultizoneCompDetOpeningData(i)%HeightFac3  = 0.0d0  ! Height factor for opening factor #3
     MultizoneCompDetOpeningData(i)%StartHFac3  = 0.0d0  ! Start height factor for opening factor #3
     MultizoneCompDetOpeningData(i)%OpenFac4    = 0.0d0  ! Opening factor #4
     MultizoneCompDetOpeningData(i)%DischCoeff4 = 0.0d0  ! Discharge coefficient for opening factor #4
     MultizoneCompDetOpeningData(i)%WidthFac4   = 0.0d0  ! Width factor for for Opening factor #4
     MultizoneCompDetOpeningData(i)%HeightFac4  = 0.0d0  ! Height factor for opening factor #4
     MultizoneCompDetOpeningData(i)%StartHFac4  = 0.0d0  ! Start height factor for opening factor #4
     If (MultizoneCompDetOpeningData(i)%NumFac > 2) then
       If (NumNumbers .GE. 19) then
         MultizoneCompDetOpeningData(i)%OpenFac3    = Numbers(15)  ! Opening factor #3
         MultizoneCompDetOpeningData(i)%DischCoeff3 = Numbers(16)  ! Discharge coefficient for opening factor #3
         MultizoneCompDetOpeningData(i)%WidthFac3   = Numbers(17)  ! Width factor for for Opening factor #3
         MultizoneCompDetOpeningData(i)%HeightFac3  = Numbers(18)  ! Height factor for opening factor #3
         MultizoneCompDetOpeningData(i)%StartHFac3  = Numbers(19)  ! Start height factor for opening factor #3
         If (MultizoneCompDetOpeningData(i)%DischCoeff3 .le. 0.0d0) then
           CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
           CALL ShowContinueError('The value of '//TRIM(cNumericFields(16))//' is equal to 0. '// &
                                  'A value greater than zero is required.')
           ErrorsFound=.true.
         End If
         If (MultizoneCompDetOpeningData(i)%OpenFac3 .NE. 1.0d0 .AND. MultizoneCompDetOpeningData(i)%NumFac == 3) then
           CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
           CALL ShowContinueError('..This object specifies that only 3 opening factors will be used. So, the value '// &
                                  'of '//TRIM(cNumericFields(15))//' is set to 1.0.')
           CALL ShowContinueError('..Input value was '//TRIM(RoundSigDigits(MultizoneCompDetOpeningData(i)%OpenFac3,2)))
           MultizoneCompDetOpeningData(i)%OpenFac3 = 1.0d0
         End If
       Else
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': '// &
                              TRIM(Alphas(1))// '. The number of opening fields is less than required (21).')
         ErrorsFound=.true.
       End If
     End IF
     If (MultizoneCompDetOpeningData(i)%NumFac == 4) then
       If (NumNumbers .EQ. 24) then
         MultizoneCompDetOpeningData(i)%OpenFac4    = Numbers(20)  ! Opening factor #4
         MultizoneCompDetOpeningData(i)%DischCoeff4 = Numbers(21)  ! Discharge coefficient for opening factor #4
         MultizoneCompDetOpeningData(i)%WidthFac4   = Numbers(22)  ! Width factor for for Opening factor #4
         MultizoneCompDetOpeningData(i)%HeightFac4  = Numbers(23)  ! Height factor for opening factor #4
         MultizoneCompDetOpeningData(i)%StartHFac4  = Numbers(24)  ! Start height factor for opening factor #4
         If (MultizoneCompDetOpeningData(i)%DischCoeff4 .le. 0.0d0) then
           CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
           CALL ShowContinueError('The value of '//TRIM(cNumericFields(21))//' is equal to 0. '// &
                                  'A value greater than zero is required.')
           ErrorsFound=.true.
         End If
         If (MultizoneCompDetOpeningData(i)%OpenFac4 .NE. 1.0d0) then
           CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
           CALL ShowContinueError('..This object specifies that 4 opening factors will be used. So, the value '// &
                                    'of '//TRIM(cNumericFields(20))//' is set to 1.0.')
           CALL ShowContinueError('..Input value was '//TRIM(RoundSigDigits(MultizoneCompDetOpeningData(i)%OpenFac3,2)))
           MultizoneCompDetOpeningData(i)%OpenFac4 = 1.0d0
         End If
       Else
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': '// &
                              TRIM(Alphas(1))//'. The number of opening fields is less than required (26).')
         ErrorsFound=.true.
       End If
     End IF
     If (MultizoneCompDetOpeningData(i)%NumFac > 2) then
       If (MultizoneCompDetOpeningData(i)%OpenFac2 .GE. MultizoneCompDetOpeningData(i)%OpenFac3) then
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
         CALL ShowContinueError('..The value of '//TRIM(cNumericFields(10))//' >= the value of '//TRIM(cNumericFields(15)))
         ErrorsFound=.true.
       end IF
     End If
     If (MultizoneCompDetOpeningData(i)%NumFac == 4) then
       If (MultizoneCompDetOpeningData(i)%OpenFac3 .GE. MultizoneCompDetOpeningData(i)%OpenFac4) then
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
         CALL ShowContinueError('..The value of '//TRIM(cNumericFields(15))//' >= the value of '//TRIM(cNumericFields(20)))
         ErrorsFound=.true.
       end IF
     End If
     ! Check values to meet requirements
     If (MultizoneCompDetOpeningData(i)%NumFac .GE. 2) Then
       ! Check width factor
       If (MultizoneCompDetOpeningData(i)%WidthFac1 .lt. 0.0d0) Then
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
         CALL ShowContinueError('..The value of '//TRIM(cNumericFields(7))//' must be greater than or equal to zero.')
         ErrorsFound=.true.
       End If
       If (MultizoneCompDetOpeningData(i)%WidthFac2 .le. 0.0d0) Then
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
         CALL ShowContinueError('..The value of '//TRIM(cNumericFields(12))//' must be greater than zero.')
         ErrorsFound=.true.
       End If
       ! Check height factor
       If (MultizoneCompDetOpeningData(i)%HeightFac1 .lt. 0.0d0) Then
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
         CALL ShowContinueError('..The value of '//TRIM(cNumericFields(8))//' must be greater than or equal to zero.')
         ErrorsFound=.true.
       End If
       If (MultizoneCompDetOpeningData(i)%HeightFac2 .le. 0.0d0) Then
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
         CALL ShowContinueError('..The value of '//TRIM(cNumericFields(13))//' must be greater than zero.')
         ErrorsFound=.true.
       End If
     End If
     If (MultizoneCompDetOpeningData(i)%NumFac .GE. 3) Then
       ! Check width factor
       If (MultizoneCompDetOpeningData(i)%WidthFac3 .le. 0.0d0) Then
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
         CALL ShowContinueError('..The value of '//TRIM(cNumericFields(17))//' must be greater than zero.')
         ErrorsFound=.true.
       End If
       ! Check height factor
       If (MultizoneCompDetOpeningData(i)%HeightFac3 .le. 0.0d0) Then
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
         CALL ShowContinueError('..The value of '//TRIM(cNumericFields(18))//' must be greater than zero.')
         ErrorsFound=.true.
       End If
       If (MultizoneCompDetOpeningData(i)%DischCoeff3 .le. 0.0d0) then
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
         CALL ShowContinueError('The value of '//TRIM(cNumericFields(16))//' is less than or equal to 0. '// &
                                'A value greater than zero is required.')
         ErrorsFound=.true.
       End If
     End If
     If (MultizoneCompDetOpeningData(i)%NumFac .GE. 4) Then
       ! Check width factor
       If (MultizoneCompDetOpeningData(i)%WidthFac4 .le. 0.0d0) Then
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
         CALL ShowContinueError('..The value of '//TRIM(cNumericFields(22))//' must be greater than zero.')
         ErrorsFound=.true.
       End If
       ! Check height factor
       If (MultizoneCompDetOpeningData(i)%HeightFac4 .le. 0.0d0) Then
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
         CALL ShowContinueError('..The value of '//TRIM(cNumericFields(23))//' must be greater than zero.')
         ErrorsFound=.true.
       End If
       If (MultizoneCompDetOpeningData(i)%DischCoeff4 .le. 0.0d0) then
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
         CALL ShowContinueError('The value of '//TRIM(cNumericFields(21))//' is less than or equal to 0. '// &
                                'A value greater than zero is required.')
         ErrorsFound=.true.
       End If
     End If
     ! Check sum of Height Factor and the Start Height Factor
     If (MultizoneCompDetOpeningData(i)%HeightFac1+MultizoneCompDetOpeningData(i)%StartHFac1 .GT. 1.0d0) Then
       CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
       CALL ShowContinueError('..The sum of '//TRIM(cNumericFields(8))//' and '//TRIM(cNumericFields(9))//' is greater than 1.0')
       ErrorsFound=.true.
     End If
     If (MultizoneCompDetOpeningData(i)%HeightFac2+MultizoneCompDetOpeningData(i)%StartHFac2 .GT. 1.0d0) Then
       CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
       CALL ShowContinueError('..The sum of '//TRIM(cNumericFields(13))//' and '//TRIM(cNumericFields(14))//' is greater than 1.0')
       ErrorsFound=.true.
     End If
     If (MultizoneCompDetOpeningData(i)%HeightFac3+MultizoneCompDetOpeningData(i)%StartHFac3 .GT. 1.0d0) Then
       CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
       CALL ShowContinueError('..The sum of '//TRIM(cNumericFields(18))//' and '//TRIM(cNumericFields(19))//' is greater than 1.0')
       ErrorsFound=.true.
     End If
     If (MultizoneCompDetOpeningData(i)%HeightFac4+MultizoneCompDetOpeningData(i)%StartHFac4 .GT. 1.0d0) Then
       CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
       CALL ShowContinueError('..The sum of '//TRIM(cNumericFields(23))//' and '//TRIM(cNumericFields(24))//' is greater than 1.0')
       ErrorsFound=.true.
     End If
   End do
  End If

! Validate opening component and assign opening dimension
  If (AirflowNetworkNumOfDetOpenings > 0) then
   Do i=1,AirflowNetworkNumOfDetOpenings
     found = .False.
     Do j=1,AirflowNetworkNumOfSurfaces
        if (MultizoneCompDetOpeningData(i)%Name == MultizoneSurfaceData(j)%OpeningName) then
!           MultizoneCompDetOpeningData(i)%Width = Surface(MultizoneSurfaceData(j)%SurfNum)%Width
!           MultizoneCompDetOpeningData(i)%Height = Surface(MultizoneSurfaceData(j)%SurfNum)%Height
           found = .True.
        end if
     end do
   end do
  end if

! Read AirflowNetwork simulation simple openings
  CurrentModuleObject='AirflowNetwork:MultiZone:Component:SimpleOpening'
  AirflowNetworkNumOfSimOpenings = GetNumObjectsFound(CurrentModuleObject)
  If (AirflowNetworkNumOfSimOpenings > 0) then
   Allocate(MultizoneCompSimpleOpeningData(AirflowNetworkNumOfSimOpenings))
   Do i=1,AirflowNetworkNumOfSimOpenings
     CALL GetObjectItem(CurrentModuleObject,i,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                        NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                        AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
     IsNotOK=.false.
     IsBlank=.false.
     CALL VerifyName(Alphas(1),MultizoneCompSimpleOpeningData%Name,i-1,IsNotOK,IsBlank, &
                     TRIM(CurrentModuleObject)//' Name')
     IF (IsNotOK) THEN
       ErrorsFound=.true.
       IF (IsBlank) Alphas(1)='xxxxx'
     ENDIF
     MultizoneCompSimpleOpeningData(i)%Name       = Alphas(1)     ! Name of large simple opening component
     MultizoneCompSimpleOpeningData(i)%FlowCoef   = Numbers(1)    ! Air Mass Flow Coefficient When Window or Door Is Closed
     MultizoneCompSimpleOpeningData(i)%FlowExpo   = Numbers(2)    ! Air Mass Flow exponent When Window or Door Is Closed
     MultizoneCompSimpleOpeningData(i)%MinRhoDiff = Numbers(3)    ! Minimum density difference for two-way flow
     MultizoneCompSimpleOpeningData(i)%DischCoeff = Numbers(4)    ! Discharge coefficient at full opening
   End do
  End If

! Read AirflowNetwork simulation horizontal openings
  CurrentModuleObject='AirflowNetwork:MultiZone:Component:HorizontalOpening'
  AirflowNetworkNumOfHorOpenings = GetNumObjectsFound(CurrentModuleObject)
  If (AirflowNetworkNumOfHorOpenings > 0) then
   Allocate(MultizoneCompHorOpeningData(AirflowNetworkNumOfHorOpenings))
   Do i=1,AirflowNetworkNumOfHorOpenings
     CALL GetObjectItem(CurrentModuleObject,i,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                        NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                        AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
     IsNotOK=.false.
     IsBlank=.false.
     CALL VerifyName(Alphas(1),MultizoneCompHorOpeningData%Name,i-1,IsNotOK,IsBlank, &
                     TRIM(CurrentModuleObject)//' Name')
     IF (IsNotOK) THEN
       ErrorsFound=.true.
       IF (IsBlank) Alphas(1)='xxxxx'
     ENDIF
     MultizoneCompHorOpeningData(i)%Name       = Alphas(1)     ! Name of large simple opening component
     MultizoneCompHorOpeningData(i)%FlowCoef   = Numbers(1)    ! Air Mass Flow Coefficient When Window or Door Is Closed
     If (Numbers(1) .LE. 0.0d0) Then
       CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
       CALL ShowContinueError('The value of '//TRIM(cNumericFields(1))//' is less than or equal to 0. '// &
                              'A value greater than zero is required.')
       ErrorsFound=.true.
     End If
     MultizoneCompHorOpeningData(i)%FlowExpo   = Numbers(2)    ! Air Mass Flow exponent When Window or Door Is Closed
     If (Numbers(2) .GT. 1.0d0 .OR. Numbers(2) .LT. 0.5d0) Then
       CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
       CALL ShowContinueError('The value of '//TRIM(cNumericFields(2))//' is beyond the boundary. '// &
                              'A value between 0.5 and 1.0 is required.')
       ErrorsFound=.true.
     End If
     MultizoneCompHorOpeningData(i)%Slope      = Numbers(3)    ! Sloping plane angle
     If (Numbers(3) .GT. 90.0d0 .OR. Numbers(3) .LT. 0.0d0) Then
       CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
       CALL ShowContinueError('The value of '//TRIM(cNumericFields(3))//' is beyond the boundary. '// &
                              'A value between 0 and 90.0 is required.')
       ErrorsFound=.true.
     End If
     MultizoneCompHorOpeningData(i)%DischCoeff = Numbers(4)    ! Discharge coefficient at full opening
     If (Numbers(4) .LE. 0.0d0) Then
       CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
       CALL ShowContinueError('The value of '//TRIM(cNumericFields(4))//' is less than or equal to 0. '// &
                              'A value greater than zero is required.')
       ErrorsFound=.true.
     End If
   End do
  End If

! Check status of control level for each surface with an opening
  j=0
  CurrentModuleObject='AirflowNetwork:MultiZone:Surface'
  Do i=1,AirflowNetworkNumOfSurfaces
    if (MultizoneSurfaceData(i)%SurfNum == 0) CYCLE
    if (AirflowNetworkNumOfDetOpenings > 0) &
      j=FindItemInList(MultizoneSurfaceData(i)%OpeningName,MultizoneCompDetOpeningData%Name, &
              AirflowNetworkNumOfDetOpenings)
    If (j == 0 .and. AirflowNetworkNumOfSimOpenings > 0)   &
      j=FindItemInList(MultizoneSurfaceData(i)%OpeningName,MultizoneCompSimpleOpeningData%Name, &
              AirflowNetworkNumOfSimOpenings)
    ! Obtain schedule number and check surface shape
    If (j > 0) then
      If (Surface(MultizoneSurfaceData(i)%SurfNum)%Sides .eq. 3) then
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(MultizoneSurfaceData(i)%SurfName)//'".')
        CALL ShowContinueError('The opening is a Triangular subsurface. A rectangular subsurface should be used.')
      End If
      If (MultizoneSurfaceData(i)%VentingSchName /= Blank) then
        MultizoneSurfaceData(i)%VentingSchNum = GetScheduleIndex(MultizoneSurfaceData(i)%VentingSchName)
        If (MultizoneSurfaceData(i)%VentingSchNum == 0) then
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//TRIM(MultizoneSurfaceData(i)%SurfName)//  &
             '", invalid schedule.')
          CALL ShowContinueError('Venting Schedule not found="'//TRIM(MultizoneSurfaceData(i)%VentingSchName)//'".')
          ErrorsFound = .true.
        end if
      Else
        MultizoneSurfaceData(i)%VentingSchName = ' '
        MultizoneSurfaceData(i)%VentingSchNum = 0
      End if
      Select Case (MultizoneSurfaceData(i)%VentSurfCtrNum)
        Case (VentCtrNum_Temp, VentCtrNum_AdjTemp)
          MultizoneSurfaceData(i)%VentSchNum = GetScheduleIndex(MultizoneSurfaceData(i)%VentSchName)
          if (MultizoneSurfaceData(i)%VentSchName == Blank) THEN
             CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object, '//  &
                           'No Ventilation Schedule was found, but is required when ventilation control is Temperature.')
             CALL ShowContinueError('..for Surface = "'//TRIM(MultizoneSurfaceData(i)%SurfName)//'"')
             ErrorsFound=.true.
          elseif (MultizoneSurfaceData(i)%VentSchNum == 0) then
             CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object, '//  &
                           'Invalid Ventilation Schedule, required when ventilation control is Temperature.')
             CALL ShowContinueError('..Schedule name in error = '//TRIM(MultizoneSurfaceData(i)%VentSchName))
             CALL ShowContinueError('..for Surface = "'//TRIM(MultizoneSurfaceData(i)%SurfName)//'"')
            ErrorsFound = .true.
          end if
          if (MultizoneSurfaceData(i)%LowValueTemp < 0.0d0) then
             CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object, '//  &
                          ' Low Temperature difference value < 0.0d0')
             CALL ShowContinueError('..Input value='//TRIM(RoundSigDigits(MultizoneSurfaceData(i)%LowValueTemp,1))//  &
                                    ', Value will be reset to 0.0.')
             CALL ShowContinueError('..for Surface = "'//TRIM(MultizoneSurfaceData(i)%SurfName)//'"')
             MultizoneSurfaceData(i)%LowValueTemp = 0.0d0
          end if
          if (MultizoneSurfaceData(i)%LowValueTemp >= 100.0d0) then
             CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object, '//  &
                                   ' Low Temperature difference value >= 100.0d0')
             CALL ShowContinueError('..Input value = '//TRIM(RoundSigDigits(MultizoneSurfaceData(i)%LowValueTemp,1))//  &
                                    ', Value will be reset to 0.0')
             CALL ShowContinueError('..for Surface = "'//TRIM(MultizoneSurfaceData(i)%SurfName)//'"')
             MultizoneZoneData(i)%LowValueTemp = 0.0d0
          end if
          if (MultizoneSurfaceData(i)%UpValueTemp <= MultizoneSurfaceData(i)%LowValueTemp) then
            CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object, '//  &
                                  ' Upper Temperature <= Lower Temperature difference value.')
            CALL ShowContinueError('..Input value = '//TRIM(RoundSigDigits(MultizoneSurfaceData(i)%UpValueTemp,1))//  &
                                   ', Value will be reset to 100.0')
            CALL ShowContinueError('..for Surface = "'//TRIM(MultizoneSurfaceData(i)%SurfName)//'"')
            MultizoneSurfaceData(i)%UpValueTemp = 100.0d0
          end if

        Case (VentCtrNum_Enth, VentCtrNum_AdjEnth)
          MultizoneSurfaceData(i)%VentSchNum = GetScheduleIndex(MultizoneSurfaceData(i)%VentSchName)
          if (MultizoneSurfaceData(i)%VentSchName == Blank) THEN
             CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object, '//  &
                'No Ventilation Schedule was found, but is required when ventilation control is Enthalpy.')
             CALL ShowContinueError('..for Surface = "'//TRIM(MultizoneSurfaceData(i)%SurfName)//'"')
             ErrorsFound=.true.
          elseif (MultizoneSurfaceData(i)%VentSchNum == 0) then
             CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' object, '//  &
                 'Invalid Ventilation Schedule, required when ventilation control is Enthalpy.')
             CALL ShowContinueError('..Schedule name in error = '//TRIM(MultizoneSurfaceData(i)%VentSchName))
             CALL ShowContinueError('..for Surface = "'//TRIM(MultizoneSurfaceData(i)%SurfName)//'"')
            ErrorsFound = .true.
          end if
          if (MultizoneSurfaceData(i)%LowValueEnth < 0.0d0) then
             CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object, '//  &
                  ' Low Enthalpy difference value < 0.0d0')
             CALL ShowContinueError('..Input value = '//TRIM(RoundSigDigits(MultizoneSurfaceData(i)%LowValueEnth,1))//  &
                    ', Value will be reset to 0.0')
             CALL ShowContinueError('..for Surface = "'//TRIM(MultizoneSurfaceData(i)%SurfName)//'"')
             MultizoneSurfaceData(i)%LowValueEnth = 0.0d0
          end if
          if (MultizoneSurfaceData(i)%LowValueEnth >= 300000.0d0) then
             CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object, '//  &
                  'Low Enthalpy difference value >= 300000.0')
             CALL ShowContinueError('..Input value = '//TRIM(RoundSigDigits(MultizoneSurfaceData(i)%LowValueEnth,1))//  &
                    ', Value will be reset to 0.0')
             CALL ShowContinueError('..for Surface = "'//TRIM(MultizoneSurfaceData(i)%SurfName)//'"')
             MultizoneZoneData(i)%LowValueEnth = 0.0d0
          end if
          if (MultizoneSurfaceData(i)%UpValueEnth <= MultizoneSurfaceData(i)%LowValueEnth) then
            CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' object, '//  &
                   ' Upper Enthalpy <= Lower Enthalpy difference value.')
            CALL ShowContinueError('..Input value = '//TRIM(RoundSigDigits(MultizoneSurfaceData(i)%UpValueEnth,1))//  &
                   ', Value will be set to 300000.0')
            CALL ShowContinueError('..for Surface = "'//TRIM(MultizoneSurfaceData(i)%SurfName)//'"')
            MultizoneSurfaceData(i)%UpValueEnth = 300000.0d0
          end if

        Case (VentCtrNum_Const)
          MultizoneSurfaceData(i)%VentSchNum = 0
          MultizoneSurfaceData(i)%VentSchName = ' '

        Case (VentCtrNum_ASH55)
          MultizoneSurfaceData(i)%VentSchNum = 0
          MultizoneSurfaceData(i)%VentSchName = ' '

        Case (VentCtrNum_CEN15251)
          MultizoneSurfaceData(i)%VentSchNum = 0
          MultizoneSurfaceData(i)%VentSchName = ' '

        Case (VentCtrNum_Novent)
          MultizoneSurfaceData(i)%VentSchNum = 0
          MultizoneSurfaceData(i)%VentSchName = ' '

        Case (VentCtrNum_ZoneLevel)
          MultizoneSurfaceData(i)%VentSchNum = 0
          MultizoneSurfaceData(i)%VentSchName = ' '

        Case Default
      End Select

    End If
  End Do

! Validate opening component and assign opening dimension
  If (AirflowNetworkNumOfSimOpenings > 0) then
   Do i=1,AirflowNetworkNumOfSimOpenings
     found = .False.
     Do j=1,AirflowNetworkNumOfSurfaces
        if (MultizoneCompSimpleOpeningData(i)%Name == MultizoneSurfaceData(j)%OpeningName) then
!           MultizoneCompSimpleOpeningData(i)%Width = Surface(MultizoneSurfaceData(j)%SurfNum)%Width
!           MultizoneCompSimpleOpeningData(i)%Height = Surface(MultizoneSurfaceData(j)%SurfNum)%Height
           found = .True.
        end if
     end do
   end do
  end if

! *** Read AirflowNetwork simulation reference crack conditions
  CurrentModuleObject='AirflowNetwork:MultiZone:ReferenceCrackConditions'
  AirflowNetworkNumOfStdCndns = GetNumObjectsFound(CurrentModuleObject)
  IF (AirflowNetworkNumOfStdCndns > 0) THEN
    Allocate(MultizoneSurfaceStdConditionsCrackData(0:AirflowNetworkNumOfStdCndns))
    Do i=1,AirflowNetworkNumOfStdCndns
      CALL GetObjectItem(CurrentModuleObject,i,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                         NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                         AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
      IsNotOK=.false.
      IsBlank=.false.
      CALL VerifyName(Alphas(1),MultizoneSurfaceStdConditionsCrackData%Name,i-1,IsNotOK,IsBlank,  &
                      TRIM(CurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        CYCLE
      ENDIF
      MultizoneSurfaceStdConditionsCrackData(i)%Name      = Alphas(1)
      MultizoneSurfaceStdConditionsCrackData(i)%StandardT = Numbers(1)      ! Reference temperature for crack data
      MultizoneSurfaceStdConditionsCrackData(i)%StandardP = Numbers(2)      ! Reference barometric pressure for crack data
      IF (ABS((MultizoneSurfaceStdConditionsCrackData(i)%StandardP-StdBaroPress)/StdBaroPress) > 0.1d0) THEN  ! 10% off
        CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//': Pressure = '//  &
                           TRIM(RoundSigDigits(MultizoneSurfaceStdConditionsCrackData(i)%StandardP,0))//   &
                           ' differs by more than 10% from Standard Barometric Pressure = '//  &
                           TRIM(RoundSigDigits(StdBaroPress,0))//'.')
        CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
      ENDIF
      If (MultizoneSurfaceStdConditionsCrackData(i)%StandardP <= 31000.0d0) then
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': '//TRIM(Alphas(1))// &
                            '. '//TRIM(cNumericFields(2))//' must be greater than 31000 Pa.')
        ErrorsFound=.true.
      end IF
      MultizoneSurfaceStdConditionsCrackData(i)%StandardW = Numbers(3)      ! Reference humidity ratio for crack data
    ENDDO
  ELSE
    AirflowNetworkNumOfStdCndns=0
    Allocate(MultizoneSurfaceStdConditionsCrackData(0:1))
    MultizoneSurfaceStdConditionsCrackData(0)%Name = '*'
    MultizoneSurfaceStdConditionsCrackData(0)%StandardT = 20.d0
    MultizoneSurfaceStdConditionsCrackData(0)%StandardP = 101325.d0
    MultizoneSurfaceStdConditionsCrackData(0)%StandardW = 0.0d0
  ENDIF

! *** Read AirflowNetwork simulation surface crack component
  CurrentModuleObject='AirflowNetwork:MultiZone:Surface:Crack'
  AirflowNetworkNumOfSurCracks = GetNumObjectsFound(CurrentModuleObject)
  If (AirflowNetworkNumOfSurCracks > 0) then
   Allocate(MultizoneSurfaceCrackData(AirflowNetworkNumOfSurCracks))
   Do i=1,AirflowNetworkNumOfSurCracks
     CALL GetObjectItem(CurrentModuleObject,i,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                        NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                        AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
     IsNotOK=.false.
     IsBlank=.false.
     CALL VerifyName(Alphas(1),MultizoneSurfaceCrackData%Name,i-1,IsNotOK,IsBlank, &
                    TRIM(CurrentModuleObject)//' Name')
     IF (IsNotOK) THEN
       ErrorsFound=.true.
       IF (IsBlank) Alphas(1)='xxxxx'
     ENDIF
     MultizoneSurfaceCrackData(i)%Name     = Alphas(1)       ! Name of surface crack component
     MultizoneSurfaceCrackData(i)%FlowCoef = Numbers(1)      ! Air Mass Flow Coefficient
     MultizoneSurfaceCrackData(i)%FlowExpo = Numbers(2)      ! Air Mass Flow exponent
     IF (lAlphaBlanks(2)) THEN
       IF (AirflowNetworkNumOfStdCndns == 1) THEN
         MultiZoneSurfaceCrackData(i)%StandardT=MultizoneSurfaceStdConditionsCrackData(1)%StandardT
         MultiZoneSurfaceCrackData(i)%StandardP=MultizoneSurfaceStdConditionsCrackData(1)%StandardP
         MultiZoneSurfaceCrackData(i)%StandardW=MultizoneSurfaceStdConditionsCrackData(1)%StandardW
       ELSE
         MultiZoneSurfaceCrackData(i)%StandardT=MultizoneSurfaceStdConditionsCrackData(0)%StandardT
         MultiZoneSurfaceCrackData(i)%StandardP=MultizoneSurfaceStdConditionsCrackData(0)%StandardP
         MultiZoneSurfaceCrackData(i)%StandardW=MultizoneSurfaceStdConditionsCrackData(0)%StandardW
       ENDIF
     ELSE
       j=FindItemInList(Alphas(2),MultizoneSurfaceStdConditionsCrackData(1:AirflowNetworkNumOfStdCndns)%Name,  &
                          AirflowNetworkNumOfStdCndns)
       IF (j == 0) THEN
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1))//  &
                              '. Specified '//TRIM(cAlphaFields(2))//' = '//TRIM(Alphas(2))//' not found.')
         ErrorsFound=.true.
       ELSE
         MultiZoneSurfaceCrackData(i)%StandardT=MultizoneSurfaceStdConditionsCrackData(j)%StandardT
         MultiZoneSurfaceCrackData(i)%StandardP=MultizoneSurfaceStdConditionsCrackData(j)%StandardP
         MultiZoneSurfaceCrackData(i)%StandardW=MultizoneSurfaceStdConditionsCrackData(j)%StandardW
       ENDIF
     ENDIF
   End do
  End If

! *** Read AirflowNetwork simulation surface effective leakage area component
  CurrentModuleObject='AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea'
  AirflowNetworkNumOfSurELA = GetNumObjectsFound(CurrentModuleObject)
  If (AirflowNetworkNumOfSurELA > 0) then
   Allocate(MultizoneSurfaceELAData(AirflowNetworkNumOfSurELA))
   Do i=1,AirflowNetworkNumOfSurELA
     CALL GetObjectItem(CurrentModuleObject,i,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                        NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                        AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
     IsNotOK=.false.
     IsBlank=.false.
     CALL VerifyName(Alphas(1),MultizoneSurfaceELAData%Name,i-1,IsNotOK,IsBlank, &
                    TRIM(CurrentModuleObject)//' Name')
     IF (IsNotOK) THEN
       ErrorsFound=.true.
       IF (IsBlank) Alphas(1)='xxxxx'
     ENDIF
     MultizoneSurfaceELAData(i)%Name  = Alphas(1)         ! Name of surface effective leakage area component
     MultizoneSurfaceELAData(i)%ELA   = Numbers(1)        ! Effective leakage area
     MultizoneSurfaceELAData(i)%DischCoeff  = Numbers(2)  ! Discharge coefficient
     MultizoneSurfaceELAData(i)%RefDeltaP   = Numbers(3)  ! Reference pressure difference
     MultizoneSurfaceELAData(i)%FlowExpo    = Numbers(4)  ! Air Mass Flow exponent
     MultizoneSurfaceELAData(i)%TestDeltaP  = 0.0d0       ! Testing pressure difference
     MultizoneSurfaceELAData(i)%TestDisCoef = 0.0d0       ! Testing Discharge coefficient
   End do
  End If

! *** Read AirflowNetwork simulation zone exhaust fan component
  CurrentModuleObject='AirflowNetwork:MultiZone:Component:ZoneExhaustFan'
  AirflowNetworkNumOfExhFan = GetNumObjectsFound(CurrentModuleObject)
  NumOfExhaustFans = GetNumObjectsFound('Fan:ZoneExhaust')
  If (AirflowNetworkNumOfExhFan > 0) then
   Allocate(MultizoneCompExhaustFanData(AirflowNetworkNumOfExhFan))
   Do i=1,AirflowNetworkNumOfExhFan
     CALL GetObjectItem(CurrentModuleObject,i,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                        NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                        AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
     IsNotOK=.false.
     IsBlank=.false.
     CALL VerifyName(Alphas(1),MultizoneCompExhaustFanData%Name,i-1,IsNotOK,IsBlank, &
                    TRIM(CurrentModuleObject)//' Name')
     IF (IsNotOK) THEN
       ErrorsFound=.true.
       IF (IsBlank) Alphas(1)='xxxxx'
     ENDIF
     MultizoneCompExhaustFanData(i)%Name     = Alphas(1)  ! Name of zone exhaust fan component
     MultizoneCompExhaustFanData(i)%FlowCoef = Numbers(1) ! flow coefficient
     MultizoneCompExhaustFanData(i)%FlowExpo = Numbers(2) ! Flow exponent
     FanErrorFound=.false.
     CALL GetFanIndex(MultizoneCompExhaustFanData(i)%Name,FanIndex,FanErrorFound)
     If (FanErrorFound) THEN
       CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1))//  &
                            ' is not found in Fan:ZoneExhaust objects.')
       ErrorsFound=.true.
     ENDIF
     CALL GetFanVolFlow(FanIndex,MultizoneCompExhaustFanData(i)%FlowRate)
     MultizoneCompExhaustFanData(i)%FlowRate   = StdRhoAir*MultizoneCompExhaustFanData(i)%FlowRate
     MultizoneCompExhaustFanData(i)%InletNode  = GetFanInletNode('Fan:ZoneExhaust',Alphas(1),ErrorsFound)
     MultizoneCompExhaustFanData(i)%OutletNode = GetFanOutletNode('Fan:ZoneExhaust',Alphas(1),ErrorsFound)
     CALL GetFanType(Alphas(1),FanType_Num,FanErrorFound)
     IF (FanType_Num .NE. FanType_ZoneExhaust) then
       CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1))//  &
                           '. The specified '//TRIM(cAlphaFields(1))//' is not found as a valid Fan:ZoneExhaust object.')
       ErrorsFound=.true.
     ENDIF
     IF (lAlphaBlanks(2)) THEN
       IF (AirflowNetworkNumOfStdCndns == 1) THEN
         MultizoneCompExhaustFanData(i)%StandardT=MultizoneSurfaceStdConditionsCrackData(1)%StandardT
         MultizoneCompExhaustFanData(i)%StandardP=MultizoneSurfaceStdConditionsCrackData(1)%StandardP
         MultizoneCompExhaustFanData(i)%StandardW=MultizoneSurfaceStdConditionsCrackData(1)%StandardW
       ELSE
         MultizoneCompExhaustFanData(i)%StandardT=MultizoneSurfaceStdConditionsCrackData(0)%StandardT
         MultizoneCompExhaustFanData(i)%StandardP=MultizoneSurfaceStdConditionsCrackData(0)%StandardP
         MultizoneCompExhaustFanData(i)%StandardW=MultizoneSurfaceStdConditionsCrackData(0)%StandardW
       ENDIF
     ELSE
       j=FindItemInList(Alphas(2),MultizoneSurfaceStdConditionsCrackData(1:AirflowNetworkNumOfStdCndns)%Name,  &
                        AirflowNetworkNumOfStdCndns)
       IF (j == 0) THEN
         CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1))//  &
                              '. Specified '//TRIM(cAlphaFields(2))//' = '//TRIM(Alphas(2))//' not found.')
         ErrorsFound=.true.
       ELSE
         MultizoneCompExhaustFanData(i)%StandardT=MultizoneSurfaceStdConditionsCrackData(j)%StandardT
         MultizoneCompExhaustFanData(i)%StandardP=MultizoneSurfaceStdConditionsCrackData(j)%StandardP
         MultizoneCompExhaustFanData(i)%StandardW=MultizoneSurfaceStdConditionsCrackData(j)%StandardW
       ENDIF
     ENDIF
   end do
  End If

! *** Read AirflowNetwork CP Array
 if (AirflowNetworkSimu%iWPCCntr == iWPCCntr_Input) then ! Surface-Average does not need inputs of external nodes
  CurrentModuleObject='AirflowNetwork:MultiZone:WindPressureCoefficientArray'
  AirflowNetworkNumOfCPArray = GetNumObjectsFound(CurrentModuleObject)

  if (AirflowNetworkNumOfCPArray .NE. 1) then
    CALL ShowSevereError(RoutineName//'Currently only one ("1") '//TRIM(CurrentModuleObject)// &
                         ' object per simulation allowed when using the AirflowNetwork model.')
    ErrorsFound=.true.
  end if

  If (AirflowNetworkNumOfCPArray > 0 .AND. AirflowNetworkSimu%iWPCCntr == iWPCCntr_Input) then
   Allocate(MultizoneCPArrayData(AirflowNetworkNumOfCPArray))
   Do i=1,AirflowNetworkNumOfCPArray

     CALL GetObjectItem(CurrentModuleObject,i,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                        NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                        AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

     MultizoneCPArrayData(i)%Name       = Alphas(1)     ! Name of CP array
     MultizoneCPArrayData(i)%NumWindDir = NumNumbers

     Allocate(MultizoneCPArrayData(i)%WindDir(NumNumbers))
     DO j=1, NumNumbers ! Wind direction
        MultizoneCPArrayData(i)%WindDir(j) = Numbers(j)
        If (j > 1) THEN
          If (MultizoneCPArrayData(i)%WindDir(j-1) .GE. MultizoneCPArrayData(i)%WindDir(j)) Then
            CALL ShowSevereError(RoutineName//'An '//TRIM(CurrentModuleObject)//' object ')
            CALL ShowContinueError('has either the same values for two consecutive wind directions, or a lower wind direction'// &
                                   ' value after a higher wind direction value.')
            CALL ShowContinueError('Wind direction values must be entered in ascending order.')
            CALL ShowContinueError(TRIM(cNumericFields(j))//' = ' &
              //TRIM(RoundSigDigits(MultizoneCPArrayData(i)%WindDir(j-1),2))//' '//TRIM(cNumericFields(j+1))//' = ' &
              //TRIM(RoundSigDigits(MultizoneCPArrayData(i)%WindDir(j),2)))
            ErrorsFound=.true.
          End If
       End If
     end do
   End Do
  Else
   if (AirflowNetworkSimu%iWPCCntr == iWPCCntr_Input) then ! Wind coefficient == Surface-Average does not need inputs of CP Array
     CALL ShowSevereError(RoutineName//'An '//TRIM(CurrentModuleObject)//' object is required.')
     CALL ShowContinueError('..but not found with Wind Pressure Coefficient Type = INPUT')
     ErrorsFound=.true.
   end if
  End If
 End If

! Get the number of wind directions
  If (AirflowNetworkSimu%iWPCCntr == iWPCCntr_Input) then
    AirflowNetworkSimu%NWind = NumNumbers
  else
!    AirflowNetworkSimu%NWind = 4
  end if

! Read AirflowNetwork CP Value
 if (AirflowNetworkSimu%iWPCCntr == iWPCCntr_Input) then ! Surface-Average does not need inputs of external nodes
  CurrentModuleObject='AirflowNetwork:MultiZone:WindPressureCoefficientValues'
  AirflowNetworkNumOfCPValue = GetNumObjectsFound(CurrentModuleObject)
  If (AirflowNetworkNumOfCPValue > 0 .AND. AirflowNetworkSimu%iWPCCntr == iWPCCntr_Input) then
   Allocate(MultizoneCPValueData(AirflowNetworkNumOfCPValue))

   Do i=1,AirflowNetworkNumOfCPValue
     CALL GetObjectItem(CurrentModuleObject,i,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                        NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                        AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
     IsNotOK=.false.
     IsBlank=.false.
     CALL VerifyName(Alphas(1),MultizoneCPValueData%Name,i-1,IsNotOK,IsBlank, &
                     TRIM(CurrentModuleObject)//' Name')
     IF (IsNotOK) THEN
       ErrorsFound=.true.
       IF (IsBlank) Alphas(2)='xxxxx'
     ENDIF
     MultizoneCPValueData(i)%Name        = Alphas(1)    ! Name of CP value
     MultizoneCPValueData(i)%CPArrayName = Alphas(2)    ! CP array Name
     ! Ensure the CP array name should be the same as the name of AirflowNetwork:MultiZone:WindPressureCoefficientArray
     if (.NOT. SameString(Alphas(2),MultizoneCPArrayData(1)%Name)) then
       CALL ShowSevereError(RoutineName//'Invalid '//TRIM(cAlphaFields(2))//' = '//TRIM(Alphas(2))//  &
                            ' in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
       CALL ShowContinueError('The valid name is '//Trim(MultizoneCPArrayData(1)%Name))
       ErrorsFound=.true.
     End If
     Allocate(MultizoneCPValueData(i)%CPValue(NumNumbers))
     If (NumNumbers .LT. AirflowNetworkSimu%NWind) Then
       CALL ShowSevereError(RoutineName//'The number of WPC Values ('// &
                            TRIM(RoundSigDigits(NumNumbers,0))//') in the '//TRIM(CurrentModuleObject)//' object ')
       CALL ShowContinueError(TRIM(Alphas(1))//' with '//TRIM(cAlphaFields(2))//' = '//TRIM(Alphas(2))// &
                              ' is less than the number of Wind Directions ('// &
                              TRIM(RoundSigDigits(MultizoneCPArrayData(1)%NumWindDir,0))//') defined in the ')
       CALL ShowContinueError(TRIM(CurrentModuleObject)//' object.')
       CALL ShowFatalError(RoutineName//'Errors found getting inputs. Previous error(s) cause program termination.')
     End If
     DO j=1, NumNumbers ! CP Value
        MultizoneCPValueData(i)%CPValue(j) = Numbers(j)
     end do
   End Do

  Else
   if (AirflowNetworkSimu%iWPCCntr == iWPCCntr_Input) then ! Wind coefficient == Surface-Average does not need inputs of CP Array
     CALL ShowSevereError(RoutineName//'An '//TRIM(CurrentModuleObject)//' object is required and not found' &
                          // ' with Wind Pressure Coefficient Type = INPUT')
     ErrorsFound=.true.
   end if
  End If
 End If

! Calculate CP values
  If (SameString(AirflowNetworkSimu%WPCCntr,'SurfaceAverageCalculation')) then
     Call CalcWindPressureCoeffs
     ! Ensure automatic generation is OK
     n = 0
     Do j=1,5
       found = .FALSE.
       Do i=1,AirflowNetworkNumOfExtNode
         If (MultizoneExternalNodeData(i)%CPVNum == j) Then
           found = .TRUE.
           Exit
         End If
       End Do
       If (found) n = n+1
       If (j == 5 .AND. (.NOT. found)) Then
         found = .TRUE.
         If (DisplayExtraWarnings) Then
           CALL ShowWarningError(RoutineName//'SurfaceAverageCalculation is entered for field = Wind Pressure Coefficient ' &
                             //'Type, but no roof surface is defined using an AirflowNetwork:MultiZone:Surface object.')
           CALL ShowContinueError('Reconsider if this is your modeling intent. Simulation continues.')
         End If
       End If
     End Do
     If (n .LT. 5 .AND. DisplayExtraWarnings) Then
       CALL ShowWarningError(RoutineName//'SurfaceAverageCalculation is entered for field = Wind Pressure Coefficient Type.')
       CALL ShowContinueError('The AirflowNetwork model provides wind pressure coefficients for 4 vertical exterior ' &
                             //'orientations and 1 horizontal roof.')
       CALL ShowContinueError('There are only '//TRIM(RoundSigDigits(n,0))//' exterior surface orientations defined' &
                              //' in this input file using AirflowNetwork:MultiZone:Surface objects.')
       CALL ShowContinueError('Reconsider if this is your modeling intent. Simulation continues.')
     End If
  End If

! Assign external node height
  If (SameString(AirflowNetworkSimu%WPCCntr,'SurfaceAverageCalculation') .OR. &
      SameString(AirflowNetworkSimu%HeightOption,'OpeningHeight')) then
    Do i=1,AirflowNetworkNumOfExtNode
      Do j=1,AirflowNetworkNumOfSurfaces
        If (Surface(MultizoneSurfaceData(j)%SurfNum)%ExtBoundCond == ExternalEnvironment) Then
          If (SameString(MultizoneSurfaceData(j)%ExternalNodeName,MultizoneExternalNodeData(i)%Name)) Then
            MultizoneExternalNodeData(i)%height = Surface(MultizoneSurfaceData(j)%SurfNum)%Centroid%Z
            Exit
          End If
        End If
      End DO
    End Do
  End If

  IF (ErrorsFound) CALL ShowFatalError(RoutineName//'Errors found getting inputs. Previous error(s) cause program termination.')

  ! Write wind pressure coefficients in the EIO file
  WRITE(OutputFileInits,fmta) '! <AirflowNetwork Model:Wind Direction>, Wind Direction #1 to n (degree)'
  WRITE(OutputFileInits,fmta,advance='No') 'AirflowNetwork Model:Wind Direction, '
  DO I=1,AirflowNetworkSimu%NWind-1
    StringOut=RoundSigDigits(MultizoneCPArrayData(1)%WindDir(I),1)
    WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
  END DO
  StringOut=RoundSigDigits(MultizoneCPArrayData(1)%WindDir(AirflowNetworkSimu%NWind),1)
  WRITE(OutputFileInits,fmta) TRIM(StringOut)

  WRITE(OutputFileInits,fmta,advance='No') '! <AirflowNetwork Model:Wind Pressure Coefficients>, Name, '
  WRITE(OutputFileInits,fmta) 'Wind Pressure Coefficients #1 to n (dimensionless)'
  Do I=1,AirflowNetworkNumOfCPValue
    WRITE(OutputFileInits,fmta,advance='No') 'AirflowNetwork Model:Wind Pressure Coefficients, '
    WRITE(OutputFileInits,fmta,advance='No') TRIM(MultizoneCpValueData(i)%Name)//', '
    DO J=1,AirflowNetworkSimu%NWind-1
      StringOut=RoundSigDigits(MultizoneCpValueData(i)%CPValue(J),2)
      WRITE(OutputFileInits,fmta,advance='No') TRIM(StringOut)//','
    END DO
    StringOut=RoundSigDigits(MultizoneCpValueData(i)%CPValue(AirflowNetworkSimu%NWind),2)
    WRITE(OutputFileInits,fmta) TRIM(StringOut)
  End Do

  ! If no zone object, exit
  If (AirflowNetworkNumOfZones .eq. 0) Then
    CALL ShowFatalError(RoutineName//'Errors found getting inputs. Previous error(s) cause program termination.')
  End If
  ! If zone node number =0, exit.
  Do j=1,AirflowNetworkNumOfSurfaces
    If (MultizoneSurfaceData(j)%NodeNums(1) .EQ. 0 .AND. ErrorsFound) Then
      CALL ShowFatalError(RoutineName//'Errors found getting inputs. Previous error(s) cause program termination.')
    End If
    If (MultizoneSurfaceData(j)%NodeNums(2) .EQ. 0 .AND. ErrorsFound) Then
      CALL ShowFatalError(RoutineName//'Errors found getting inputs. Previous error(s) cause program termination.')
    End If
  End Do

  ! Ensure at least two surfaces are exposed to a zone
  ALLOCATE(ZoneCheck(AirflowNetworkNumOfZones))
  ALLOCATE(ZoneBCCheck(AirflowNetworkNumOfZones))
  ZoneCheck = 0
  ZoneBCCheck = 0
  CurrentModuleObject='AirflowNetwork:MultiZone:Surface'
  Do j=1,AirflowNetworkNumOfSurfaces
    If (MultizoneSurfaceData(j)%NodeNums(1) .LE. AirflowNetworkNumOfZones) then
      ZoneCheck(MultizoneSurfaceData(j)%NodeNums(1)) = ZoneCheck(MultizoneSurfaceData(j)%NodeNums(1))+1
      ZoneBCCheck(MultizoneSurfaceData(j)%NodeNums(1)) = MultizoneSurfaceData(j)%NodeNums(2)
    End If
    If (MultizoneSurfaceData(j)%NodeNums(2) .LE. AirflowNetworkNumOfZones) then
      ZoneCheck(MultizoneSurfaceData(j)%NodeNums(2)) = ZoneCheck(MultizoneSurfaceData(j)%NodeNums(2))+1
      ZoneBCCheck(MultizoneSurfaceData(j)%NodeNums(2)) = MultizoneSurfaceData(j)%NodeNums(1)
    End If
  End Do
  Do i=1,AirflowNetworkNumOfZones
    If (ZoneCheck(i) .EQ. 0) then
      CALL ShowSevereError(RoutineName//'AirflowNetwork:Multizone:Zone = '//TRIM(MultizoneZoneData(i)%ZoneName))
      CALL ShowContinueError(' does not have any surfaces defined in '//TRIM(CurrentModuleObject))
      CALL ShowContinueError('Each zone should have at least two surfaces defined in '//TRIM(CurrentModuleObject))
      ErrorsFound=.true.
    End If
    If (ZoneCheck(i) .EQ. 1) then
      CALL ShowSevereError(RoutineName//'AirflowNetwork:Multizone:Zone = '//TRIM(MultizoneZoneData(i)%ZoneName))
      CALL ShowContinueError(' has only one surface defined in '//TRIM(CurrentModuleObject))
      CALL ShowContinueError(' Each zone should have at least two surfaces defined in '//TRIM(CurrentModuleObject))
      ErrorsFound=.true.
   End If
    If (ZoneCheck(i) > 1) then
      SurfaceFound = .FALSE.
      Do j=1,AirflowNetworkNumOfSurfaces
        If (MultizoneSurfaceData(j)%NodeNums(1) == i) then
          If (ZoneBCCheck(i) .NE. MultizoneSurfaceData(j)%NodeNums(2)) then
            SurfaceFound = .TRUE.
            Exit
          End If
        End If
        If (MultizoneSurfaceData(j)%NodeNums(2) == i) then
          If (ZoneBCCheck(i) .NE. MultizoneSurfaceData(j)%NodeNums(1)) then
            SurfaceFound = .TRUE.
            Exit
          End If
        End If
      End Do
      If (.NOT. SurfaceFound) then
        CALL ShowWarningError(RoutineName//'AirflowNetwork:Multizone:Zone = '//TRIM(MultizoneZoneData(i)%ZoneName))
        CALL ShowContinueError('has more than one surface defined in '//TRIM(CurrentModuleObject)//', but has the same ' &
                               //'boundary conditions')
        CALL ShowContinueError('Please check inputs of '//TRIM(CurrentModuleObject))
      End If
    End If
  End Do
  DEALLOCATE(ZoneCheck)
  DEALLOCATE(ZoneBCCheck)

! Validate CP Value number
 if (AirflowNetworkSimu%iWPCCntr == iWPCCntr_Input) then ! Surface-Average does not need inputs of external nodes
! Ensure no duplicated external names in CP Value
  CurrentModuleObject='AirflowNetwork:MultiZone:WindPressureCoefficientValues'
  Do j=1,AirflowNetworkNumOfExtNode
    found = .False.
    Do i=1,AirflowNetworkNumOfCPValue
      if (SameString(MultizoneExternalNodeData(j)%WPCName,MultizoneCPValueData(i)%Name)) then
        MultizoneExternalNodeData(j)%CPVNum = i
        Exit
      end if
    end do
    If (MultizoneExternalNodeData(j)%CPVNum == 0) then
      CALL ShowSevereError(RoutineName//'AirflowNetwork:MultiZone:ExternalNode: Wind Pressure Coefficient ' &
                       //'Values Object Name is not found in ' //Trim(MultizoneExternalNodeData(j)%Name))
      CALL ShowContinueError('Please ensure there is a WindPressureCoefficientValues name defined as '// &
           TRIM(MultizoneExternalNodeData(j)%WPCName) //' in '//TRIM(CurrentModuleObject))
      ErrorsFound=.true.
    End If
  End do
  ! Ensure different CPVNum is used to avoid a single side boundary condition
  found = .False.
  Do j=2,AirflowNetworkNumOfExtNode
    If (MultizoneExternalNodeData(j-1)%CPVNum .NE. MultizoneExternalNodeData(j)%CPVNum) Then
      found = .True.
      Exit
    End If
  End do
  If (.NOT. found) then
    CALL ShowSevereError('The same Wind Pressure Coefficient Values Object name is used in all ' &
                        //'AirflowNetwork:MultiZone:ExternalNode objects.')
    CALL ShowContinueError('Please input at least two different Wind Pressure Coefficient Values Object names'&
                          //' to avoid single side boundary condition.')
    ErrorsFound=.true.
  End If

 End If


! Read AirflowNetwork Distribution system node
  CurrentModuleObject='AirflowNetwork:Distribution:Node'
  DisSysNumOfNodes = GetNumObjectsFound(CurrentModuleObject)
  if (DisSysNumOfNodes > 0) then
   Allocate(DisSysNodeData(DisSysNumOfNodes))
   Do i=1,DisSysNumOfNodes
     CALL GetObjectItem(CurrentModuleObject,i,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                        NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                        AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
     IsNotOK=.false.
     IsBlank=.false.
     CALL VerifyName(Alphas(1),DisSysNodeData%Name,i-1,IsNotOK,IsBlank, &
                    TRIM(CurrentModuleObject)//' Name')
     IF (IsNotOK) THEN
       ErrorsFound=.true.
       IF (IsBlank) Alphas(1)='xxxxx'
     ENDIF
     DisSysNodeData(i)%Name      = Alphas(1)       ! Name of node
     DisSysNodeData(i)%EPlusName = Alphas(2)       ! Name of associated EnergyPlus node
     DisSysNodeData(i)%EPlusType = Alphas(3)       ! Name of associated EnergyPlus type
     DisSysNodeData(i)%Height    = Numbers(1)      ! Nodal height
     DisSysNodeData(i)%EPlusNodeNum = 0            ! EPlus node number
     ! verify EnergyPlus object type
     if (SameString(Alphas(3),'AirLoopHVAC:ZoneMixer') .or. SameString(Alphas(3),'AirLoopHVAC:ZoneSplitter') .or. &
         SameString(Alphas(3),'AirLoopHVAC:OutdoorAirSystem') .or. SameString(Alphas(3),'OAMixerOutdoorAirStreamNode') .or. &
         SameString(Alphas(3),'OutdoorAir:NodeList') .or. SameString(Alphas(3),'OutdoorAir:Node') .or. &
         SameString(Alphas(3),'Other') .or. lAlphaBlanks(3)) then
       cycle
     else
       CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//'="'//trim(Alphas(1))//'" invalid '//  &
         trim(cAlphaFields(3))//'="'//trim(Alphas(3))//'" illegal key.')
       CALL ShowContinueError('Valid keys are: AirLoopHVAC:ZoneMixer, AirLoopHVAC:ZoneSplitter, AirLoopHVAC:OutdoorAirSystem, '//  &
         'OAMixerOutdoorAirStreamNode, OutdoorAir:NodeList, OutdoorAir:Node or Other.')
       ErrorsFound=.true.
     endif
   end do
  Else
   if (SimulateAirflowNetwork > AirflowNetworkControlMultizone+1) then
     CALL ShowSevereError(RoutineName//'An '//TRIM(CurrentModuleObject)//' object is required but not found.')
     ErrorsFound=.true.
   End If
  End If

! Read AirflowNetwork Distribution system component: duct leakage
  CurrentModuleObject='AirflowNetwork:Distribution:Component:Leak'
  DisSysNumOfLeaks = GetNumObjectsFound(CurrentModuleObject)
  If (DisSysNumOfLeaks > 0) then
   Allocate(DisSysCompLeakData(DisSysNumOfLeaks))
   Do i=1,DisSysNumOfLeaks
     CALL GetObjectItem(CurrentModuleObject,i,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                        NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                        AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
     IsNotOK=.false.
     IsBlank=.false.
     CALL VerifyName(Alphas(1),DisSysCompLeakData%Name,i-1,IsNotOK,IsBlank, &
                     TRIM(CurrentModuleObject)//' Name')
     IF (IsNotOK) THEN
       ErrorsFound=.true.
       IF (IsBlank) Alphas(1)='xxxxx'
     ENDIF
     DisSysCompLeakData(i)%Name     = Alphas(1)     ! Name of duct leak component
     DisSysCompLeakData(i)%FlowCoef = Numbers(1)    ! Air Mass Flow Coefficient
     DisSysCompLeakData(i)%FlowExpo = Numbers(2)    ! Air Mass Flow exponent
   end do
  Else
!   if (AirflowNetworkSimu%DistControl == "DISTRIBUTIONSYSTEM") &
!     CALL ShowMessage('GetAirflowNetworkInput: AirflowNetwork:Distribution:Component Leak: This object is not used')
  End If


! Read AirflowNetwork Distribution system component: duct effective leakage ratio
  CurrentModuleObject='AirflowNetwork:Distribution:Component:LeakageRatio'
  DisSysNumOfELRs = GetNumObjectsFound(CurrentModuleObject)
  If (DisSysNumOfELRs > 0) then
   Allocate(DisSysCompELRData(DisSysNumOfELRs))
   Do i=1,DisSysNumOfELRs
     CALL GetObjectItem(CurrentModuleObject,i,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                        NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                        AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
     IsNotOK=.false.
     IsBlank=.false.
     CALL VerifyName(Alphas(1),DisSysCompELRData%Name,i-1,IsNotOK,IsBlank, &
                     TRIM(CurrentModuleObject)//' Name')
     IF (IsNotOK) THEN
       ErrorsFound=.true.
       IF (IsBlank) Alphas(1)='xxxxx'
     ENDIF
     DisSysCompELRData(i)%Name     = Alphas(1)     ! Name of duct effective leakage ratio component
     DisSysCompELRData(i)%ELR      = Numbers(1)    ! Value of effective leakage ratio
     DisSysCompELRData(i)%FlowRate = Numbers(2)    ! Maximum airflow rate
     DisSysCompELRData(i)%RefPres  = Numbers(3)    ! Reference pressure difference
     DisSysCompELRData(i)%FlowExpo = Numbers(4)    ! Air Mass Flow exponent
     DisSysCompELRData(i)%FlowRate = Numbers(2)*StdRhoAir
   end do
  Else
!   if (AirflowNetworkSimu%DistControl == "DISTRIBUTIONSYSTEM") &
!     CALL ShowMessage('GetAirflowNetworkInput: AirflowNetwork:Distribution:Component Leakage Ratio: This object is not used')
  End If

! Read AirflowNetwork Distribution system component: duct
  CurrentModuleObject='AirflowNetwork:Distribution:Component:Duct'
  DisSysNumOfDucts = GetNumObjectsFound(CurrentModuleObject)
  If (DisSysNumOfDucts > 0) then
   Allocate(DisSysCompDuctData(DisSysNumOfDucts))
   Do i=1,DisSysNumOfDucts
     CALL GetObjectItem(CurrentModuleObject,i,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                        NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                        AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
     IsNotOK=.false.
     IsBlank=.false.
     CALL VerifyName(Alphas(1),DisSysCompDuctData%Name,i-1,IsNotOK,IsBlank, &
                     TRIM(CurrentModuleObject)//' Name')
     IF (IsNotOK) THEN
       ErrorsFound=.true.
       IF (IsBlank) Alphas(1)='xxxxx'
     ENDIF
     DisSysCompDuctData(i)%Name        = Alphas(1)    ! Name of duct effective leakage ratio component
     DisSysCompDuctData(i)%L           = Numbers(1)   ! Duct length [m]
     DisSysCompDuctData(i)%D           = Numbers(2)   ! Hydrolic diameter [m]
     DisSysCompDuctData(i)%A           = Numbers(3)   ! Cross section area [m2]
     DisSysCompDuctData(i)%Rough       = Numbers(4)   ! Surface roughness [m]
     DisSysCompDuctData(i)%TurDynCoef  = Numbers(5)   ! Turbulent dynamic loss coefficient
     DisSysCompDuctData(i)%UThermal    = Numbers(6)   ! Overall heat transmittance [W/m2.K]
     DisSysCompDuctData(i)%UMoisture   = Numbers(7)   ! Overall moisture transmittance [kg/m2]
     DisSysCompDuctData(i)%MThermal    = 0.0d0        ! Thermal capacity [J/K]
     DisSysCompDuctData(i)%MMoisture   = 0.0d0        ! Mositure capacity [kg]
     DisSysCompDuctData(i)%LamDynCoef  = 64.0d0       ! Laminar dynamic loss coefficient
     DisSysCompDuctData(i)%LamFriCoef  = Numbers(5)   ! Laminar friction loss coefficient
     DisSysCompDuctData(i)%InitLamCoef = 128.0d0      ! Coefficient of linear initialization
     DisSysCompDuctData(i)%RelRough = Numbers(4)/Numbers(2)    ! e/D: relative roughness
     DisSysCompDuctData(i)%RelL     = Numbers(1)/Numbers(2)    ! L/D: relative length
     DisSysCompDuctData(i)%A1 = 1.14d0 - 0.868589d0*LOG(DisSysCompDuctData(i)%RelRough)    ! 1.14 - 0.868589*ln(e/D)
     DisSysCompDuctData(i)%g = DisSysCompDuctData(i)%A1        ! 1/sqrt(Darcy friction factor)
   end do
  Else
   if (SimulateAirflowNetwork > AirflowNetworkControlMultizone+1) then
     CALL ShowSevereError(RoutineName//'An '//TRIM(CurrentModuleObject)//' object is required but not found.')
     ErrorsFound=.true.
   End If
  End If

! Read AirflowNetwork Distribution system component: Damper
!  CurrentModuleObject='AIRFLOWNETWORK:DISTRIBUTION:COMPONENT DAMPER'
! Deleted on Aug. 13, 2008

! Read AirflowNetwork Distribution system component: constant volume fan
  CurrentModuleObject='AirflowNetwork:Distribution:Component:Fan'
  DisSysNumOfCVFs = GetNumObjectsFound(CurrentModuleObject)
  If (DisSysNumOfCVFs > 0) then
   Allocate(DisSysCompCVFData(DisSysNumOfCVFs))
   Do i=1,DisSysNumOfCVFs
     CALL GetObjectItem(CurrentModuleObject,i,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                        NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                        AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
     DisSysCompCVFData(i)%Name = Alphas(1)           ! Name of duct effective leakage ratio component
     DisSysCompCVFData(i)%Ctrl = 1.0d0               ! Control ratio
     FanErrorFound=.false.
     CALL GetFanIndex(DisSysCompCVFData(i)%Name,FanIndex,FanErrorFound)
     DisSysCompCVFData(i)%FanIndex = FanIndex
     If (FanErrorFound) THEN
       CALL ShowSevereError('...occurs in '//TRIM(CurrentModuleObject)//' = '//  &
                            TRIM(DisSysCompCVFData(i)%Name))
       ErrorsFound=.true.
     ENDIF
     CALL GetFanVolFlow(FanIndex,DisSysCompCVFData(i)%FlowRate)
     DisSysCompCVFData(i)%FlowRate = StdRhoAir*DisSysCompCVFData(i)%FlowRate

     CALL GetFanType(Alphas(1),FanType_Num,FanErrorFound)
     DisSysCompCVFData(i)%FanTypeNum = FanType_Num
     SupplyFanType = FanType_Num
     If (.NOT. (FanType_Num .EQ. FanType_SimpleConstVolume .or. FanType_Num .EQ. FanType_SimpleOnOff .or. &
         FanType_Num .EQ. FanType_SimpleVAV)) then
       CALL ShowSevereError(RoutineName//'The '//TRIM(cAlphaFields(2))//' in '//TRIM(CurrentModuleObject)//  &
                            ' = '//TRIM(Alphas(1))//' is not a valid fan type.')
       CALL ShowContinueError('Valid fan types are  Fan:ConstantVolume or Fan:OnOff')
       ErrorsFound=.true.
     Else
       If (SameString(Alphas(2),'Fan:ConstantVolume') .AND. FanType_Num .EQ. FanType_SimpleOnOff) then
         CALL ShowSevereError('The '//TRIM(cAlphaFields(2))//' defined in '//TRIM(CurrentModuleObject)//  &
                              ' is '//TRIM(Alphas(2)))
         CALL ShowContinueError('The '//TRIM(cAlphaFields(2))//' defined in an AirLoopHVAC is Fan:OnOff')
         ErrorsFound=.true.
       End If
       If (SameString(Alphas(2),'Fan:OnOff') .AND. FanType_Num .EQ. FanType_SimpleConstVolume) then
         CALL ShowSevereError('The '//TRIM(cAlphaFields(2))//' defined in '//TRIM(CurrentModuleObject)//  &
                              ' is '//TRIM(Alphas(2)))
         CALL ShowContinueError('The '//TRIM(cAlphaFields(2))//' defined in an AirLoopHVAC is Fan:ConstantVolume')
         ErrorsFound=.true.
       End If
     End If
     If (FanType_Num .EQ. FanType_SimpleConstVolume) then
       SupplyFanInletNode = GetFanInletNode('Fan:ConstantVolume',Alphas(1),ErrorsFound)
       DisSysCompCVFData(i)%InletNode = SupplyFanInletNode
       DisSysCompCVFData(i)%OutletNode = GetFanOutletNode('Fan:ConstantVolume',Alphas(1),ErrorsFound)
       SupplyFanOutletNode = DisSysCompCVFData(i)%OutletNode
     End If
     If (FanType_Num .EQ. FanType_SimpleOnOff) then
       SupplyFanInletNode = GetFanInletNode('Fan:OnOff',Alphas(1),ErrorsFound)
       DisSysCompCVFData(i)%InletNode = SupplyFanInletNode
       DisSysCompCVFData(i)%OutletNode = GetFanOutletNode('Fan:OnOff',Alphas(1),ErrorsFound)
       SupplyFanOutletNode = DisSysCompCVFData(i)%OutletNode
     End If
     If (FanType_Num .EQ. FanType_SimpleVAV) then
       SupplyFanInletNode = GetFanInletNode('Fan:VariableVolume',Alphas(1),ErrorsFound)
       DisSysCompCVFData(i)%InletNode = SupplyFanInletNode
       DisSysCompCVFData(i)%OutletNode = GetFanOutletNode('Fan:VariableVolume',Alphas(1),ErrorsFound)
       SupplyFanOutletNode = DisSysCompCVFData(i)%OutletNode
       VAVSystem = .TRUE.
     End If
   end do
  Else
   if (SimulateAirflowNetwork > AirflowNetworkControlMultizone+1) then
     CALL ShowSevereError(RoutineName//'An '//TRIM(CurrentModuleObject)//' object is required but not found.')
     ErrorsFound=.true.
   End If
  End If

  ! Check AirTerminal:SingleDuct:Uncontrolled. This object is not allowed
  If (VAVSystem) Then
    i = GetNumObjectsFound('AirTerminal:SingleDuct:Uncontrolled')
    If (i .GT. 0) Then
      CALL ShowSevereError(RoutineName//'Invalid terminal type for a VAV system = AirTerminal:SingleDuct:Uncontrolled')
      CALL ShowContinueError('A VAV system requires all ternimal units with type = AirTerminal:SingleDuct:VAV:Reheat')
      ErrorsFound=.true.
    End If
  End If

! Read AirflowNetwork Distribution system component: Detailed fan
!  CurrentModuleObject='AIRFLOWNETWORK:DISTRIBUTION:COMPONENT DETAILED FAN'
! Deleted on Aug. 13, 2008

! Read AirflowNetwork Distribution system component: coil
  CurrentModuleObject='AirflowNetwork:Distribution:Component:Coil'
  DisSysNumOfCoils = GetNumObjectsFound(CurrentModuleObject)
  If (DisSysNumOfCoils > 0) then
   Allocate(DisSysCompCoilData(DisSysNumOfCoils))
   Do i=1,DisSysNumOfCoils
     CALL GetObjectItem(CurrentModuleObject,i,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                        NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                        AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
     IsNotOK=.false.
     IsBlank=.false.
     CALL VerifyName(Alphas(1),DisSysCompCoilData%Name,i-1,IsNotOK,IsBlank, &
                     TRIM(CurrentModuleObject)//' Name')
     IF (IsNotOK) THEN
       ErrorsFound=.true.
       IF (IsBlank) Alphas(1)='xxxxx'
     ENDIF
     DisSysCompCoilData(i)%Name      = Alphas(1)    ! Name of associated EPlus coil component
     DisSysCompCoilData(i)%EPlusType = Alphas(2)    ! coil type
     DisSysCompCoilData(i)%L         = Numbers(1)   ! Air path length
     DisSysCompCoilData(i)%D         = Numbers(2)   ! Air path hydraulic diameter
   end do
  Else
   if (SimulateAirflowNetwork > AirflowNetworkControlMultizone+1) then
!     CALL ShowMessage(RoutineName//TRIM(CurrentModuleObject)//': This object is not used')
   End If
  End If

! Read AirflowNetwork Distribution system component: heat exchanger
  CurrentModuleObject='AirflowNetwork:Distribution:Component:HeatExchanger'
  DisSysNumOfHXs = GetNumObjectsFound(CurrentModuleObject)
  If (DisSysNumOfHXs > 0) then
   Allocate(DisSysCompHXData(DisSysNumOfHXs))
   Do i=1,DisSysNumOfHXs
     CALL GetObjectItem(CurrentModuleObject,i,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                        NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                        AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
     IsNotOK=.false.
     IsBlank=.false.
     CALL VerifyName(Alphas(1),DisSysCompHXData%Name,i-1,IsNotOK,IsBlank, &
                     TRIM(CurrentModuleObject)//' Name')
     IF (IsNotOK) THEN
       ErrorsFound=.true.
       IF (IsBlank) Alphas(1)='xxxxx'
     ENDIF
     DisSysCompHXData(i)%Name      = Alphas(1)    ! Name of associated EPlus heat exchange component
     DisSysCompHXData(i)%EPlusType = Alphas(2)    ! coil type
     DisSysCompHXData(i)%L         = Numbers(1)   ! Air path length
     DisSysCompHXData(i)%D         = Numbers(2)   ! Air path hydraulic diameter
     DisSysCompHXData(i)%CoilParentExists = VerifyHeatExchangerParent(DisSysCompHXData(i)%EPlusType,DisSysCompHXData(i)%Name)
   end do
  End If

! Read AirflowNetwork Distribution system component: terminal unit
  CurrentModuleObject='AirflowNetwork:Distribution:Component:TerminalUnit'
  DisSysNumOfTermUnits = GetNumObjectsFound(CurrentModuleObject)
  if (DisSysNumOfTermUnits > 0) then
   Allocate(DisSysCompTermUnitData(DisSysNumOfTermUnits))
   Do i=1,DisSysNumOfTermUnits
     CALL GetObjectItem(CurrentModuleObject,i,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                        NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                        AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
     IsNotOK=.false.
     IsBlank=.false.
     CALL VerifyName(Alphas(1),DisSysCompTermUnitData%Name,i-1,IsNotOK,IsBlank, &
                     TRIM(CurrentModuleObject)//' Name')
     IF (IsNotOK) THEN
       ErrorsFound=.true.
       IF (IsBlank) Alphas(1)='xxxxx'
     ENDIF
     DisSysCompTermUnitData(i)%Name      = Alphas(1)  ! Name of associated EPlus coil component
     DisSysCompTermUnitData(i)%EPlusType = Alphas(2)  ! Terminal unit type
     DisSysCompTermUnitData(i)%L         = Numbers(1) ! Air path length
     DisSysCompTermUnitData(i)%D         = Numbers(2) ! Air path hydraulic diameter
   end do
  Else
!     CALL ShowMessage(RoutineName//TRIM(CurrentModuleObject)//': This object is not used')
  End If

! Get input data of constant pressure drop component
  CurrentModuleObject='AirflowNetwork:Distribution:Component:ConstantPressureDrop'
  DisSysNumOfCPDs = GetNumObjectsFound(CurrentModuleObject)
  if (DisSysNumOfCPDs > 0) then
   Allocate(DisSysCompCPDData(DisSysNumOfCPDs))
   Do i=1,DisSysNumOfCPDs
     CALL GetObjectItem(CurrentModuleObject,i,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                        NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                        AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
     IsNotOK=.false.
     IsBlank=.false.
     CALL VerifyName(Alphas(1),DisSysCompCPDData%Name,i-1,IsNotOK,IsBlank, &
                     TRIM(CurrentModuleObject)//' Name')
     IF (IsNotOK) THEN
       ErrorsFound=.true.
       IF (IsBlank) Alphas(1)='xxxxx'
     ENDIF
     DisSysCompCPDData(i)%Name = Alphas(1)           ! Name of constant pressure drop component
     DisSysCompCPDData(i)%A    = 1.0d0               ! cross section area
     DisSysCompCPDData(i)%DP   = Numbers(1)          ! Pressure difference across the component
   end do
  Else
   if (SimulateAirflowNetwork > AirflowNetworkControlMultizone+1) then
!     CALL ShowMessage(RoutineName//TRIM(CurrentModuleObject)//': This object is not used')
   End If
  End If

! Assign numbers of nodes and linkages
   if (SimulateAirflowNetwork > AirflowNetworkControlSimple) then
      if (AirflowNetworkSimu%iWPCCntr == iWPCCntr_Input) then
         NumOfNodesMultiZone = AirflowNetworkNumOfZones+AirflowNetworkNumOfExtNode
      Else
         NumOfNodesMultiZone = AirflowNetworkNumOfZones+NumOfExtNodes
      end if
      NumOfLinksMultiZone = AirflowNetworkNumOfSurfaces
      AirflowNetworkNumOfNodes = NumOfNodesMultiZone
      AirflowNetworkNumOfLinks = NumOfLinksMultiZone
   end if
   if (SimulateAirflowNetwork > AirflowNetworkControlMultizone+1) then
      AirflowNetworkNumOfNodes = NumOfNodesMultiZone+DisSysNumOfNodes
   end if

! Assign node data
  Allocate(AirflowNetworkNodeData(AirflowNetworkNumOfNodes))
  ! Zone node
  Do I=1,AirflowNetworkNumOfZones
     AirflowNetworkNodeData(i)%Name = MultizoneZoneData(i)%ZoneName
     AirflowNetworkNodeData(i)%NodeTypeNum = 0
     AirflowNetworkNodeData(i)%EPlusZoneNum =MultizoneZoneData(i)%ZoneNum
     AirflowNetworkNodeData(i)%NodeHeight =MultizoneZoneData(i)%Height
  End Do
  ! External node
  if (AirflowNetworkSimu%iWPCCntr == iWPCCntr_Input) then
   Do I=AirflowNetworkNumOfZones+1,NumOfNodesMultiZone
     AirflowNetworkNodeData(i)%Name = MultizoneExternalNodeData(i-AirflowNetworkNumOfZones)%Name
     AirflowNetworkNodeData(i)%NodeTypeNum = 1
     AirflowNetworkNodeData(i)%EPlusZoneNum =0
     AirflowNetworkNodeData(i)%NodeHeight =MultizoneExternalNodeData(i-AirflowNetworkNumOfZones)%Height
     AirflowNetworkNodeData(i)%ExtNodeNum = i-AirflowNetworkNumOfZones
   End Do
  Else ! Surface-Average input
   Do I=AirflowNetworkNumOfZones+1,NumOfNodesMultiZone
     n = I-AirflowNetworkNumOfZones
     AirflowNetworkNodeData(i)%Name = MultizoneExternalNodeData(n)%Name
     AirflowNetworkNodeData(i)%NodeTypeNum = 1
     AirflowNetworkNodeData(i)%EPlusZoneNum =0
     AirflowNetworkNodeData(i)%ExtNodeNum = n
   End Do
  End If
  ! Check whether Distribution system is simulated
  If (AirflowNetworkNumOfNodes > NumOfNodesMultiZone) then
    ! Search node types: OAMixerOutdoorAirStreamNode, OutdoorAir:NodeList, and OutdoorAir:Node
    J = 0
    Do I=NumOfNodesMultiZone+1,AirflowNetworkNumOfNodes
     If (SameString(DisSysNodeData(i-NumOfNodesMultiZone)%EPlusType,'OAMixerOutdoorAirStreamNode')) then
       J = J+1
     End If
     If (SameString(DisSysNodeData(i-NumOfNodesMultiZone)%EPlusType,'OutdoorAir:NodeList')) then
       J = J+1
     End If
     If (SameString(DisSysNodeData(i-NumOfNodesMultiZone)%EPlusType,'OutdoorAir:Node')) then
       J = J+1
     End If
    End Do

    Do I=NumOfNodesMultiZone+1,AirflowNetworkNumOfNodes
     AirflowNetworkNodeData(i)%Name = DisSysNodeData(i-NumOfNodesMultiZone)%Name
     AirflowNetworkNodeData(i)%NodeTypeNum = 0
     AirflowNetworkNodeData(i)%EPlusZoneNum =0
     AirflowNetworkNodeData(i)%NodeHeight = DisSysNodeData(i-NumOfNodesMultiZone)%Height
     AirflowNetworkNodeData(i)%EPlusNodeNum = DisSysNodeData(i-NumOfNodesMultiZone)%EPlusNodeNum
     ! Get mixer information
     If (SameString(DisSysNodeData(i-NumOfNodesMultiZone)%EPlusType,'AirLoopHVAC:ZoneMixer')) then
       AirflowNetworkNodeData(i)%EPlusTypeNum = EPlusTypeNum_MIX
     End If
     ! Get splitter information
     If (SameString(DisSysNodeData(i-NumOfNodesMultiZone)%EPlusType,'AirLoopHVAC:ZoneSplitter')) then
       AirflowNetworkNodeData(i)%EPlusTypeNum = EPlusTypeNum_SPL
     End If
     ! Get outside air system information
     If (SameString(DisSysNodeData(i-NumOfNodesMultiZone)%EPlusType,'AirLoopHVAC:OutdoorAirSystem')) then
       AirflowNetworkNodeData(i)%EPlusTypeNum = EPlusTypeNum_OAN
     End If
     ! Get OA system inlet information 'OAMixerOutdoorAirStreamNode' was specified as an outdoor air node implicitly
     If (SameString(DisSysNodeData(i-NumOfNodesMultiZone)%EPlusType,'OAMixerOutdoorAirStreamNode') .AND. J==1) then
       AirflowNetworkNodeData(i)%EPlusTypeNum = EPlusTypeNum_EXT
       AirflowNetworkNodeData(i)%ExtNodeNum = AirflowNetworkNumOfExtNode+1
       AirflowNetworkNodeData(i)%NodeTypeNum = 1
     End If
     If (SameString(DisSysNodeData(i-NumOfNodesMultiZone)%EPlusType,'OutdoorAir:NodeList') .OR. &
          SameString(DisSysNodeData(i-NumOfNodesMultiZone)%EPlusType,'OutdoorAir:Node')) then
       If (J > 1) Then
         AirflowNetworkNodeData(i)%EPlusTypeNum = EPlusTypeNum_EXT
         AirflowNetworkNodeData(i)%ExtNodeNum = AirflowNetworkNumOfExtNode+1
         AirflowNetworkNodeData(i)%NodeTypeNum = 1
       Else
         CALL ShowSevereError(RoutineName//'AirflowNetwork:Distribution:Node: The outdoor air node is found at ' &
                            //AirflowNetworkNodeData(i)%Name)
         Call ShowContinueError('The node with Component Object Type = '//  &
            'OAMixerOutdoorAirStreamNode is not found. Please check inputs.')
         ErrorsFound=.true.
       End If
     End If
    End Do
  end if

! Start to assembly AirflowNetwork Components
  AirflowNetworkNumOfComps = AirflowNetworkNumOfDetOpenings+AirflowNetworkNumOfSimOpenings+AirflowNetworkNumOfSurCracks+ &
                             AirflowNetworkNumOfSurELA+DisSysNumOfLeaks+DisSysNumOfELRs+DisSysNumOfDucts+DisSysNumOfDampers+ &
                             DisSysNumOfCVFs+DisSysNumOfDetFans+DisSysNumOfCPDs+DisSysNumOfCoils+DisSysNumOfTermUnits+ &
                             AirflowNetworkNumOfExhFan+DisSysNumOfHXs+AirflowNetworkNumOfHorOpenings
  Allocate(AirflowNetworkCompData(AirflowNetworkNumOfComps))

  Do I=1,AirflowNetworkNumOfDetOpenings ! Detailed opening component
     AirflowNetworkCompData(i)%Name = MultizoneCompDetOpeningData(i)%Name
     AirflowNetworkCompData(i)%CompTypeNum = CompTypeNum_DOP
     AirflowNetworkCompData(i)%TypeNum = i
     AirflowNetworkCompData(i)%EPlusName = ' '
     AirflowNetworkCompData(i)%EPlusCompName = ' '
     AirflowNetworkCompData(i)%EPlusType = ' '
     AirflowNetworkCompData(i)%CompNum = i
  End Do

  J = AirflowNetworkNumOfDetOpenings
  Do I=1+J, AirflowNetworkNumOfSimOpenings+J ! Simple opening component
     n = I-J
     AirflowNetworkCompData(i)%Name = MultizoneCompSimpleOpeningData(n)%Name
     AirflowNetworkCompData(i)%CompTypeNum = CompTypeNum_SOP
     AirflowNetworkCompData(i)%TypeNum = n
     AirflowNetworkCompData(i)%EPlusName = ' '
     AirflowNetworkCompData(i)%EPlusCompName = ' '
     AirflowNetworkCompData(i)%EPlusType = ' '
     AirflowNetworkCompData(i)%CompNum = i
  End Do

  J = J+AirflowNetworkNumOfSimOpenings
  Do I=1+J, AirflowNetworkNumOfSurCracks+J ! Surface crack component
     n = I-J
     AirflowNetworkCompData(i)%Name = MultizoneSurfaceCrackData(n)%Name
     AirflowNetworkCompData(i)%CompTypeNum = CompTypeNum_SCR
     AirflowNetworkCompData(i)%TypeNum = n
     AirflowNetworkCompData(i)%EPlusName = ' '
     AirflowNetworkCompData(i)%EPlusCompName = ' '
     AirflowNetworkCompData(i)%EPlusType = ' '
     AirflowNetworkCompData(i)%CompNum = i
  End Do

  J = J+AirflowNetworkNumOfSurCracks
  Do I=1+J, AirflowNetworkNumOfSurELA+J ! Surface crack component
     n = I-J
     AirflowNetworkCompData(i)%Name = MultizoneSurfaceELAData(n)%Name
     AirflowNetworkCompData(i)%CompTypeNum = CompTypeNum_SEL
     AirflowNetworkCompData(i)%TypeNum = n
     AirflowNetworkCompData(i)%EPlusName = ' '
     AirflowNetworkCompData(i)%EPlusCompName = ' '
     AirflowNetworkCompData(i)%EPlusType = ' '
     AirflowNetworkCompData(i)%CompNum = i
  End Do

  J = J+AirflowNetworkNumOfSurELA
  Do I=1+J, AirflowNetworkNumOfExhFan+J ! Zone exhaust fan component
     n = I-J
     AirflowNetworkCompData(i)%Name = MultizoneCompExhaustFanData(n)%Name
     AirflowNetworkCompData(i)%CompTypeNum = CompTypeNum_EXF
     AirflowNetworkCompData(i)%TypeNum = n
     AirflowNetworkCompData(i)%EPlusName = ' '
     AirflowNetworkCompData(i)%EPlusCompName = ' '
     AirflowNetworkCompData(i)%EPlusType = ' '
     AirflowNetworkCompData(i)%CompNum = i
  End Do

  J = J+AirflowNetworkNumOfExhFan
  Do I=1+J, AirflowNetworkNumOfHorOpenings+J ! Distribution system crack component
     n = I-J
     AirflowNetworkCompData(i)%Name = MultizoneCompHorOpeningData(n)%Name
     AirflowNetworkCompData(i)%CompTypeNum = CompTypeNum_HOP
     AirflowNetworkCompData(i)%TypeNum = n
     AirflowNetworkCompData(i)%EPlusName = ' '
     AirflowNetworkCompData(i)%EPlusCompName = ' '
     AirflowNetworkCompData(i)%EPlusType = ' '
     AirflowNetworkCompData(i)%CompNum = i
  End Do

  J = J+AirflowNetworkNumOfHorOpenings
  Do I=1+J, DisSysNumOfLeaks+J ! Distribution system crack component
     n = I-J
     AirflowNetworkCompData(i)%Name = DisSysCompLeakData(n)%Name
     AirflowNetworkCompData(i)%CompTypeNum = CompTypeNum_PLR
     AirflowNetworkCompData(i)%TypeNum = n
     AirflowNetworkCompData(i)%EPlusName = ' '
     AirflowNetworkCompData(i)%EPlusCompName = ' '
     AirflowNetworkCompData(i)%EPlusType = ' '
     AirflowNetworkCompData(i)%CompNum = i
  End Do

  J = J+DisSysNumOfLeaks
  Do I=1+J, DisSysNumOfELRs+J ! Distribution system effective leakage ratio component
     n = I-J
     AirflowNetworkCompData(i)%Name = DisSysCompELRData(n)%Name
     AirflowNetworkCompData(i)%CompTypeNum = CompTypeNum_ELR
     AirflowNetworkCompData(i)%TypeNum = n
     AirflowNetworkCompData(i)%EPlusName = ' '
     AirflowNetworkCompData(i)%EPlusCompName = ' '
     AirflowNetworkCompData(i)%EPlusType = ' '
     AirflowNetworkCompData(i)%CompNum = i
  End Do

  J = J+DisSysNumOfELRs
  Do I=1+J, DisSysNumOfDucts+J ! Distribution system effective leakage ratio component
     n = I-J
     AirflowNetworkCompData(i)%Name = DisSysCompDuctData(n)%Name
     AirflowNetworkCompData(i)%CompTypeNum = CompTypeNum_DWC
     AirflowNetworkCompData(i)%TypeNum = n
     AirflowNetworkCompData(i)%EPlusName = ' '
     AirflowNetworkCompData(i)%EPlusCompName = ' '
     AirflowNetworkCompData(i)%EPlusType = ' '
     AirflowNetworkCompData(i)%CompNum = i
  End Do

  J = J+DisSysNumOfDucts
  Do I=1+J, DisSysNumOfDampers+J ! Distribution system effective leakage ratio component
     n = I-J
     AirflowNetworkCompData(i)%Name = DisSysCompDamperData(n)%Name
     AirflowNetworkCompData(i)%CompTypeNum = CompTypeNum_DMP
     AirflowNetworkCompData(i)%TypeNum = n
     AirflowNetworkCompData(i)%EPlusName = ' '
     AirflowNetworkCompData(i)%EPlusCompName = ' '
     AirflowNetworkCompData(i)%EPlusType = ' '
     AirflowNetworkCompData(i)%CompNum = i
  End Do

  J = J+DisSysNumOfDampers
  Do I=1+J, DisSysNumOfCVFs+J ! Distribution system constant volume fan component
     n = I-J
     AirflowNetworkCompData(i)%Name = DisSysCompCVFData(n)%Name
     AirflowNetworkCompData(i)%CompTypeNum = CompTypeNum_CVF
     AirflowNetworkCompData(i)%TypeNum = n
     AirflowNetworkCompData(i)%EPlusName = ' '
     AirflowNetworkCompData(i)%EPlusCompName = ' '
     AirflowNetworkCompData(i)%EPlusType = ' '
     AirflowNetworkCompData(i)%CompNum = i
     AirflowNetworkCompData(i)%EPlusTypeNum = EPlusTypeNum_FAN
  End Do

  J = J+DisSysNumOfCVFs
  Do I=1+J, DisSysNumOfDetFans+J ! Distribution system fan component
     n = I-J
     AirflowNetworkCompData(i)%Name = DisSysCompDetFanData(n)%Name
     AirflowNetworkCompData(i)%CompTypeNum = CompTypeNum_FAN
     AirflowNetworkCompData(i)%TypeNum = n
     AirflowNetworkCompData(i)%EPlusName = ' '
     AirflowNetworkCompData(i)%EPlusCompName = ' '
     AirflowNetworkCompData(i)%EPlusType = ' '
     AirflowNetworkCompData(i)%CompNum = i
     AirflowNetworkCompData(i)%EPlusTypeNum = EPlusTypeNum_FAN
  End Do

  J = J+DisSysNumOfDetFans
  Do I=1+J, DisSysNumOfCPDs+J ! Distribution system constant pressure drop component
     n = I-J
     AirflowNetworkCompData(i)%Name = DisSysCompCPDData(n)%Name
     AirflowNetworkCompData(i)%CompTypeNum = CompTypeNum_CPD
     AirflowNetworkCompData(i)%TypeNum = n
     AirflowNetworkCompData(i)%EPlusName = ' '
     AirflowNetworkCompData(i)%EPlusCompName = ' '
     AirflowNetworkCompData(i)%EPlusType = ' '
     AirflowNetworkCompData(i)%CompNum = i
  End Do

  J = J+DisSysNumOfCPDs
  Do I=1+J, DisSysNumOfCoils+J ! Distribution system coil component
     n = I-J
     AirflowNetworkCompData(i)%Name = DisSysCompCoilData(n)%Name
     AirflowNetworkCompData(i)%CompTypeNum = CompTypeNum_COI
     AirflowNetworkCompData(i)%TypeNum = n
     AirflowNetworkCompData(i)%EPlusName = ' '
     AirflowNetworkCompData(i)%EPlusCompName = ' '
     AirflowNetworkCompData(i)%EPlusType = ' '
     AirflowNetworkCompData(i)%CompNum = i
     AirflowNetworkCompData(i)%EPlusTypeNum = EPlusTypeNum_COI
  End Do

  J = J+DisSysNumOfCoils
  Do I=1+J, DisSysNumOfTermUnits+J ! Terminal unit component
     n = I-J
     AirflowNetworkCompData(i)%Name = DisSysCompTermUnitData(n)%Name
     AirflowNetworkCompData(i)%CompTypeNum = CompTypeNum_TMU
     AirflowNetworkCompData(i)%TypeNum = n
     AirflowNetworkCompData(i)%EPlusName = ' '
     AirflowNetworkCompData(i)%EPlusCompName = ' '
     AirflowNetworkCompData(i)%EPlusType = ' '
     AirflowNetworkCompData(i)%CompNum = i
     AirflowNetworkCompData(i)%EPlusTypeNum = EPlusTypeNum_RHT
  End Do

  J = J+DisSysNumOfTermUnits
  Do I=1+J, DisSysNumOfHXs+J ! Distribution system heat exchanger component
     n = I-J
     AirflowNetworkCompData(i)%Name = DisSysCompHXData(n)%Name
     AirflowNetworkCompData(i)%CompTypeNum = CompTypeNum_HEX
     AirflowNetworkCompData(i)%TypeNum = n
     AirflowNetworkCompData(i)%EPlusName = ' '
     AirflowNetworkCompData(i)%EPlusCompName = ' '
     AirflowNetworkCompData(i)%EPlusType = ' '
     AirflowNetworkCompData(i)%CompNum = i
     AirflowNetworkCompData(i)%EPlusTypeNum = EPlusTypeNum_HEX
  End Do

! Assign linkage data

! Read AirflowNetwork linkage data
  CurrentModuleObject='AirflowNetwork:Distribution:Linkage'
  DisSysNumOfLinks = GetNumObjectsFound(CurrentModuleObject)
  If (DisSysNumOfLinks > 0 .AND. SimulateAirflowNetwork .GT. AirflowNetworkControlMultizone) then ! Multizone + Distribution
   AirflowNetworkNumOfLinks = NumOfLinksMultiZone+DisSysNumOfLinks
   Allocate(AirflowNetworkLinkageData(DisSysNumOfLinks+AirflowNetworkNumOfSurfaces))
  Else ! Multizone only
   Allocate(AirflowNetworkLinkageData(AirflowNetworkNumOfSurfaces))
  End If

  ! Assign Mutilzone linkage based on surfaces, by assuming every surface has a crack or opening
  J=0
  Do count=1, AirflowNetworkNumOfSurfaces
     if (MultizoneSurfaceData(count)%SurfNum == 0) CYCLE
     AirflowNetworkLinkageData(count)%Name = MultizoneSurfaceData(count)%SurfName
     AirflowNetworkLinkageData(count)%NodeNums(1) = MultizoneSurfaceData(count)%NodeNums(1)
     AirflowNetworkLinkageData(count)%NodeNums(2) = MultizoneSurfaceData(count)%NodeNums(2)
     AirflowNetworkLinkageData(count)%CompName = MultizoneSurfaceData(count)%OpeningName
     AirflowNetworkLinkageData(count)%ZoneNum = 0
     AirflowNetworkLinkageData(count)%LinkNum = count
     AirflowNetworkLinkageData(count)%NodeHeights(1) = MultizoneSurfaceData(count)%CHeight
     AirflowNetworkLinkageData(count)%NodeHeights(2) = MultizoneSurfaceData(count)%CHeight
     If (.NOT. WorldCoordSystem) then
       If (AirflowNetworkNodeData(AirflowNetworkLinkageData(count)%NodeNums(1))%EPlusZoneNum>0) then
         AirflowNetworkLinkageData(count)%NodeHeights(1)=AirflowNetworkLinkageData(count)%NodeHeights(1)- &
          Zone(AirflowNetworkNodeData(AirflowNetworkLinkageData(count)%NodeNums(1))%EPlusZoneNum)%OriginZ
       End If
       If (AirflowNetworkNodeData(AirflowNetworkLinkageData(count)%NodeNums(2))%EPlusZoneNum>0) then
         AirflowNetworkLinkageData(count)%NodeHeights(2)=AirflowNetworkLinkageData(count)%NodeHeights(2)- &
          Zone(AirflowNetworkNodeData(AirflowNetworkLinkageData(count)%NodeNums(2))%EPlusZoneNum)%OriginZ
       End If
     End If
     ! Find component number
     found = .FALSE.
     Do i=1,AirflowNetworkNumOfComps
        if (AirflowNetworkLinkageData(count)%CompName == AirflowNetworkCompData(i)%Name) then
           AirflowNetworkLinkageData(count)%CompNum = i
           found = .TRUE.
           if (AirflowNetworkCompData(i)%CompTypeNum == CompTypeNum_DOP) then
             J = J+1
             AirflowNetworkLinkageData(count)%DetOpenNum = J
             MultizoneSurfaceData(count)%Multiplier = Surface(MultizoneSurfaceData(count)%SurfNum)%Multiplier
             IF (Surface(MultizoneSurfaceData(count)%SurfNum)%Tilt < 10.0d0 .OR. &
                 Surface(MultizoneSurfaceData(count)%SurfNum)%Tilt > 170.0d0) then
                Call ShowWarningError('An AirflowNetwork:Multizone:Surface object has an air-flow opening corresponding to')
                Call ShowContinueError('window or door = '//Trim(MultizoneSurfaceData(count)%SurfName)//', which is within ')
                Call ShowContinueError('10 deg of being horizontal. Airflows through large horizontal openings are poorly')
                Call ShowContinueError('modeled in the AirflowNetwork model resulting in only one-way airflow.')
             End If
             If (.NOT. (SurfaceWindow(MultizoneSurfaceData(count)%SurfNum)%OriginalClass == SurfaceClass_Window    &
                 .OR. SurfaceWindow(MultizoneSurfaceData(count)%SurfNum)%OriginalClass == SurfaceClass_GlassDoor    &
                 .OR. SurfaceWindow(MultizoneSurfaceData(count)%SurfNum)%OriginalClass == SurfaceClass_Door)) then
                CALL ShowSevereError(RoutineName//'AirflowNetworkComponent: The opening must be ' &
                            //'assigned to a window, door or glassdoor at '//AirflowNetworkLinkageData(count)%Name)
                ErrorsFound=.true.
             End If
             If (SurfaceWindow(MultizoneSurfaceData(count)%SurfNum)%OriginalClass == SurfaceClass_Door  &
                 .OR. SurfaceWindow(MultizoneSurfaceData(count)%SurfNum)%OriginalClass == SurfaceClass_GlassDoor) then
               If (MultizoneCompDetOpeningData(AirflowNetworkCompData(i)%TypeNum)%LVOType == 2) then
                 CALL ShowSevereError(RoutineName//'AirflowNetworkComponent: The opening with horizontally pivoted ' &
                            //'type must be assigned to a window surface at '//AirflowNetworkLinkageData(count)%Name)
                 ErrorsFound=.true.
               End If
             End If
           end if
           if (AirflowNetworkCompData(i)%CompTypeNum == CompTypeNum_SOP) then
             MultizoneSurfaceData(count)%Multiplier = Surface(MultizoneSurfaceData(count)%SurfNum)%Multiplier
             IF (Surface(MultizoneSurfaceData(count)%SurfNum)%Tilt < 10.0d0 .OR. &
                 Surface(MultizoneSurfaceData(count)%SurfNum)%Tilt > 170.0d0) then
                Call ShowSevereError('An AirflowNetwork:Multizone:Surface object has an air-flow opening corresponding to')
                Call ShowContinueError('window or door = '//Trim(MultizoneSurfaceData(count)%SurfName)//', which is within')
                Call ShowContinueError('10 deg of being horizontal. Airflows through horizontal openings are not allowed.')
                Call ShowContinueError('AirflowNetwork:Multizone:Component:SimpleOpening = ' &
                     //Trim(AirflowNetworkCompData(i)%Name))
                ErrorsFound=.true.
             End If
             If (.NOT. (SurfaceWindow(MultizoneSurfaceData(count)%SurfNum)%OriginalClass == SurfaceClass_Window     &
                 .OR.   SurfaceWindow(MultizoneSurfaceData(count)%SurfNum)%OriginalClass == SurfaceClass_GlassDoor  &
                 .OR.   SurfaceWindow(MultizoneSurfaceData(count)%SurfNum)%OriginalClass == SurfaceClass_Door)) then
                CALL ShowSevereError(RoutineName//'AirflowNetworkComponent: The opening must be ' &
                            //'assigned to a window, door or glassdoor at '//AirflowNetworkLinkageData(count)%Name)
                ErrorsFound=.true.
             End If
           end if
           if (AirflowNetworkCompData(i)%CompTypeNum == CompTypeNum_HOP) then
             MultizoneSurfaceData(count)%Multiplier = Surface(MultizoneSurfaceData(count)%SurfNum)%Multiplier
             ! Get linkage height from upper and lower zones
             If (MultizoneZoneData(AirflowNetworkLinkageData(count)%NodeNums(1))%ZoneNum > 0) Then
               AirflowNetworkLinkageData(count)%NodeHeights(1) = &
                Zone(MultizoneZoneData(AirflowNetworkLinkageData(count)%NodeNums(1))%ZoneNum)%CENTROID%Z
             End If
             If (AirflowNetworkLinkageData(count)%NodeNums(2) .LE. AirflowNetworkNumOfZones) Then
               If (MultizoneZoneData(AirflowNetworkLinkageData(count)%NodeNums(2))%ZoneNum > 0) Then
                 AirflowNetworkLinkageData(count)%NodeHeights(2) = &
                  Zone(MultizoneZoneData(AirflowNetworkLinkageData(count)%NodeNums(2))%ZoneNum)%CENTROID%Z
               End If
             End If
             If (AirflowNetworkLinkageData(count)%NodeNums(2) .GT. AirflowNetworkNumOfZones) Then
                CALL ShowSevereError(RoutineName//'AirflowNetworkComponent: The horizontal opening must be ' &
                            //'located between two thermal zones at '//AirflowNetworkLinkageData(count)%Name)
                Call ShowContinueError('This component is exposed to outdoors.')
                ErrorsFound=.true.
             Else
               If (.NOT. (MultizoneZoneData(AirflowNetworkLinkageData(count)%NodeNums(1))%ZoneNum > 0 .AND. &
                        MultizoneZoneData(AirflowNetworkLinkageData(count)%NodeNums(2))%ZoneNum > 0)) Then
                  CALL ShowSevereError(RoutineName//'AirflowNetworkComponent: The horizontal opening must be ' &
                            //'located between two thermal zones at '//AirflowNetworkLinkageData(count)%Name)
                 ErrorsFound=.true.
               End If
             End If
             IF (.NOT. (Surface(MultizoneSurfaceData(count)%SurfNum)%Tilt > 170.0d0 .AND. &
                 Surface(MultizoneSurfaceData(count)%SurfNum)%Tilt < 190.0d0) .AND. &
                 .NOT. (Surface(MultizoneSurfaceData(count)%SurfNum)%Tilt > -10.0d0 .AND. &
                 Surface(MultizoneSurfaceData(count)%SurfNum)%Tilt < 10.0d0)) then
                Call ShowWarningError('An AirflowNetwork:Multizone:Surface object has an air-flow opening corresponding to')
                Call ShowContinueError('window or door = '//Trim(MultizoneSurfaceData(count)%SurfName)//', which is above')
                Call ShowContinueError('10 deg of being horizontal. Airflows through non-horizontal openings are not modeled')
                Call ShowContinueError('with the object of AirflowNetwork:Multizone:Component:HorizontalOpening = ' &
                     //Trim(AirflowNetworkCompData(i)%Name))
             End If
             If (.NOT. (SurfaceWindow(MultizoneSurfaceData(count)%SurfNum)%OriginalClass == SurfaceClass_Window     &
                 .OR.   SurfaceWindow(MultizoneSurfaceData(count)%SurfNum)%OriginalClass == SurfaceClass_GlassDoor  &
                 .OR.   SurfaceWindow(MultizoneSurfaceData(count)%SurfNum)%OriginalClass == SurfaceClass_Door)) then
                CALL ShowSevereError(RoutineName//'AirflowNetworkComponent: The opening must be ' &
                            //'assigned to a window, door or glassdoor at '//AirflowNetworkLinkageData(count)%Name)
                ErrorsFound=.true.
             End If
           end if
           Exit
        end if
     end do
     if (.NOT. found) then
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': The component is not defined in '// &
                             AirflowNetworkLinkageData(count)%Name )
        ErrorsFound=.true.
     end if
  End Do

  If (DisSysNumOfLinks > 0 .AND. SimulateAirflowNetwork .GT. AirflowNetworkControlMultizone) then ! Distribution

   AirflowNetworkLinkageData%ZoneNum = 0

   do count=AirflowNetworkNumOfSurfaces+1,AirflowNetworkNumOfLinks

     CALL GetObjectItem(CurrentModuleObject,count-AirflowNetworkNumOfSurfaces,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                        NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                        AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)
     IsNotOK=.false.
     IsBlank=.false.
     CALL VerifyName(Alphas(1),AirflowNetworkLinkageData%Name,count-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
     IF (IsNotOK) THEN
       ErrorsFound=.true.
       IF (IsBlank) Alphas(1)='xxxxx'
     ENDIF
     AirflowNetworkLinkageData(count)%Name           = Alphas(1)
     AirflowNetworkLinkageData(count)%NodeNames(1)   = Alphas(2)
     AirflowNetworkLinkageData(count)%NodeHeights(1) = 0.0d0
     AirflowNetworkLinkageData(count)%NodeNames(2)   = Alphas(3)
     AirflowNetworkLinkageData(count)%NodeHeights(2) = 0.0d0
     AirflowNetworkLinkageData(count)%CompName       = Alphas(4)
     AirflowNetworkLinkageData(count)%ZoneName       = Alphas(5)
     AirflowNetworkLinkageData(count)%LinkNum        = count
     if (.NOT. lAlphaBlanks(5)) then
        AirflowNetworkLinkageData(count)%ZoneNum = FindIteminList(AirflowNetworkLinkageData(count)%ZoneName,Zone%Name,NumOfZones)
        IF (AirflowNetworkLinkageData(count)%ZoneNum == 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': Invalid '//TRIM(cAlphaFields(5))//' given = ' &
                               //TRIM(AirflowNetworkLinkageData(count)%ZoneName))
          ErrorsFound=.true.
        ENDIF
     end if
     if (Alphas(2) == Alphas(3)) then
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//', '//TRIM(cAlphaFields(2))//' = '//TRIM(cAlphaFields(3))// &
                             ' in '//TRIM(AirflowNetworkLinkageData(count)%Name))
        ErrorsFound = .TRUE.
     end if
     ! Find component number
     found = .FALSE.
     Do i=1,AirflowNetworkNumOfComps
        if (AirflowNetworkLinkageData(count)%CompName == AirflowNetworkCompData(i)%Name) then
           AirflowNetworkLinkageData(count)%CompNum = i
           found = .TRUE.
           Exit
        end if
     end do
     if (.NOT. found) then
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': The '//TRIM(cAlphaFields(4))//' is not defined in ' &
                             //AirflowNetworkLinkageData(count)%Name )
        ErrorsFound=.true.
     end if
     ! Find Node number
     found = .FALSE.
     Do i=1,AirflowNetworkNumOfNodes
        if (AirflowNetworkLinkageData(count)%NodeNames(1) == AirflowNetworkNodeData(i)%Name) then
           AirflowNetworkLinkageData(count)%NodeNums(1) = i
           AirflowNetworkLinkageData(count)%NodeHeights(1) = AirflowNetworkLinkageData(count)%NodeHeights(1)+ &
                                                             AirflowNetworkNodeData(i)%NodeHeight
           found = .TRUE.
           Exit
        end if
     end do
     if (.NOT. found) then
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': The '//TRIM(cAlphaFields(2))//' is not ' &
                             //'found in the node data ' //AirflowNetworkLinkageData(count)%Name )
        ErrorsFound=.true.
     end if
     Do i=1,AirflowNetworkNumOfNodes
        if (AirflowNetworkLinkageData(count)%NodeNames(2) == AirflowNetworkNodeData(i)%Name) then
           AirflowNetworkLinkageData(count)%NodeNums(2) = i
           AirflowNetworkLinkageData(count)%NodeHeights(2) = AirflowNetworkLinkageData(count)%NodeHeights(2)+ &
                                                             AirflowNetworkNodeData(i)%NodeHeight
           found = .TRUE.
           Exit
        end if
     end do
     if (.NOT. found) then
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': The '//TRIM(cAlphaFields(3))//' is not ' &
                             //'found in the node data ' //AirflowNetworkLinkageData(count)%Name )
        ErrorsFound=.true.
     end if
   end do

  Else

   if (SimulateAirflowNetwork > AirflowNetworkControlMultizone+1) then
     CALL ShowSevereError(RoutineName//'An '//TRIM(CurrentModuleObject)//' object is required but not found.')
     ErrorsFound=.true.
   End If

  End If

! Ensure no duplicated names in AirflowNetwork component objects
  Do i=1,AirflowNetworkNumOfComps
    Do J=i+1,AirflowNetworkNumOfComps
      If (SameString(AirflowNetworkCompData(i)%Name,AirflowNetworkCompData(j)%Name)) then
        ! SurfaceAirflowLeakageNames
        If (i .le. 4 .and. j .le. 4) then
          If (AirflowNetworkCompData(i)%CompTypeNum == CompTypeNum_DOP) &
             CompName(1) = 'AirflowNetwork:MultiZone:Component:DetailedOpening'
          If (AirflowNetworkCompData(i)%CompTypeNum == CompTypeNum_SOP) &
             CompName(1) = 'AirflowNetwork:MultiZone:Component:SimpleOpening'
          If (AirflowNetworkCompData(i)%CompTypeNum == CompTypeNum_SCR) &
             CompName(1) = 'AirflowNetwork:MultiZone:Surface:Crack'
          If (AirflowNetworkCompData(i)%CompTypeNum == CompTypeNum_SEL) &
             CompName(1) = 'AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea'
          If (AirflowNetworkCompData(j)%CompTypeNum == CompTypeNum_DOP) &
             CompName(2) = 'AirflowNetwork:MultiZone:Component:DetailedOpening'
          If (AirflowNetworkCompData(j)%CompTypeNum == CompTypeNum_SOP) &
             CompName(2) = 'AirflowNetwork:MultiZone:Component:SimpleOpening'
          If (AirflowNetworkCompData(j)%CompTypeNum == CompTypeNum_SCR) &
             CompName(2) = 'AirflowNetwork:MultiZone:Surface:Crack'
          If (AirflowNetworkCompData(j)%CompTypeNum == CompTypeNum_SEL) &
             CompName(2) = 'AirflowNetwork:MultiZone:Surface:EffectiveLeakageArea'
          CALL ShowSevereError(RoutineName//'Duplicated component names are found = ' &
                                //Trim(AirflowNetworkCompData(i)%Name))
          CALL ShowContinueError('A unique component name is required in both objects '//Trim(CompName(1)) &
                                 //' and '//Trim(CompName(2)))
          ErrorsFound=.true.
        End If
        ! Distribution component
        If (i > 4 .and. j > 4) then
          If (AirflowNetworkCompData(i)%CompTypeNum == CompTypeNum_PLR) &
             CompName(1) = 'AirflowNetwork:Distribution:Component:Leak'
          If (AirflowNetworkCompData(i)%CompTypeNum == CompTypeNum_DWC) &
             CompName(1) = 'AirflowNetwork:Distribution:Component:Duct'
          If (AirflowNetworkCompData(i)%CompTypeNum == CompTypeNum_ELR) &
             CompName(1) = 'AirflowNetwork:Distribution:Component:LeakageRatio'
          If (AirflowNetworkCompData(i)%CompTypeNum == CompTypeNum_DMP) &
             CompName(1) = 'AIRFLOWNETWORK:DISTRIBUTION:COMPONENT DAMPER'
          If (AirflowNetworkCompData(i)%CompTypeNum == CompTypeNum_CVF) &
             CompName(1) = 'AirflowNetwork:Distribution:Component:Fan'
          If (AirflowNetworkCompData(i)%CompTypeNum == CompTypeNum_CPD) &
             CompName(1) = 'AirflowNetwork:Distribution:Component:ConstantPressureDrop'
          If (AirflowNetworkCompData(i)%CompTypeNum == CompTypeNum_COI) &
             CompName(1) = 'AirflowNetwork:Distribution:Component:Coil'
          If (AirflowNetworkCompData(i)%CompTypeNum == CompTypeNum_TMU) &
             CompName(1) = 'AirflowNetwork:Distribution:Component:TerminalUnit'
          If (AirflowNetworkCompData(i)%CompTypeNum == CompTypeNum_HEX) &
             CompName(1) = 'AirflowNetwork:Distribution:Component:HeatExchanger'
          If (AirflowNetworkCompData(j)%CompTypeNum == CompTypeNum_PLR) &
             CompName(2) = 'AirflowNetwork:Distribution:Component:Leak'
          If (AirflowNetworkCompData(j)%CompTypeNum == CompTypeNum_DWC) &
             CompName(2) = 'AirflowNetwork:Distribution:Component:Duct'
          If (AirflowNetworkCompData(j)%CompTypeNum == CompTypeNum_ELR) &
             CompName(2) = 'AirflowNetwork:Distribution:Component:LeakageRatio'
          If (AirflowNetworkCompData(j)%CompTypeNum == CompTypeNum_DMP) &
             CompName(2) = 'AIRFLOWNETWORK:DISTRIBUTION:COMPONENT DAMPER'
          If (AirflowNetworkCompData(j)%CompTypeNum == CompTypeNum_CVF) &
             CompName(2) = 'AirflowNetwork:Distribution:Component:Fan'
          If (AirflowNetworkCompData(j)%CompTypeNum == CompTypeNum_CPD) &
             CompName(2) = 'AirflowNetwork:Distribution:Component:ConstantPressureDrop'
          If (AirflowNetworkCompData(j)%CompTypeNum == CompTypeNum_COI) &
             CompName(2) = 'AirflowNetwork:Distribution:Component:Coil'
          If (AirflowNetworkCompData(j)%CompTypeNum == CompTypeNum_TMU) &
             CompName(2) = 'AirflowNetwork:Distribution:Component:TerminalUnit'
          If (AirflowNetworkCompData(j)%CompTypeNum == CompTypeNum_HEX) &
             CompName(2) = 'AirflowNetwork:Distribution:Component:HeatExchanger'
          CALL ShowSevereError(RoutineName//'Duplicated component names are found = ' &
                                //Trim(AirflowNetworkCompData(i)%Name))
          CALL ShowContinueError('A unique component name is required in both objects '//Trim(CompName(1)) &
                                 //' and '//Trim(CompName(2)))
          ErrorsFound=.true.
        End If
      End IF
    End Do
  End Do

! Node and component validation
  do count=1,AirflowNetworkNumOfLinks
     NodeFound = .FALSE.
     do i=1,AirflowNetworkNumOfNodes
        if (i == AirflowNetworkLinkageData(count)%NodeNums(1)) then
           NodeFound = .TRUE.
           Exit
        end if
     end do
     if (.NOT. NodeFound) then
       If (count .le. AirflowNetworkNumOfSurfaces) then
        CALL ShowSevereError(RoutineName//TRIM(AirflowNetworkLinkageData(count)%NodeNames(1))// &
                             ' in AIRFLOWNETWORK:MULTIZONE:SURFACE = ' &
                             //TRIM(AirflowNetworkLinkageData(count)%Name)//' is not found')
       Else
         CALL ShowSevereError(RoutineName//TRIM(AirflowNetworkLinkageData(count)%NodeNames(1))// &
                              ' in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE = '//TRIM(AirflowNetworkLinkageData(count)%Name)// &
                              ' is not found in AIRFLOWNETWORK:DISTRIBUTION:NODE objects.')
       End If
       ErrorsFound = .TRUE.
     end if
     NodeFound = .FALSE.
     do i=1,AirflowNetworkNumOfNodes
        if (i == AirflowNetworkLinkageData(count)%NodeNums(2)) then
           NodeFound = .TRUE.
           Exit
        end if
     end do
     if (.NOT. NodeFound) then
       If (count .le. AirflowNetworkNumOfSurfaces) then
        CALL ShowSevereError(RoutineName//TRIM(AirflowNetworkLinkageData(count)%NodeNames(1))// &
                             ' in AIRFLOWNETWORK:MULTIZONE:SURFACE = ' &
                             //TRIM(AirflowNetworkLinkageData(count)%Name)//' is not found')
       Else
         CALL ShowSevereError(RoutineName//TRIM(AirflowNetworkLinkageData(count)%NodeNames(2))// &
                              ' in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE = '//TRIM(AirflowNetworkLinkageData(count)%Name)// &
                              ' is not found in AIRFLOWNETWORK:DISTRIBUTION:NODE objects.')
       End If
       ErrorsFound = .TRUE.
     end if
     CompFound = .FALSE.
     do i=1,AirflowNetworkNumOfComps
        if (i == AirflowNetworkLinkageData(count)%CompNum) then
           CompFound = .TRUE.
        end if
     end do
     if (.NOT. CompFound) then
        CALL ShowSevereError(RoutineName//'Component = '//TRIM(AirflowNetworkLinkageData(count)%CompName)//&
                             ' in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE = '//TRIM(AirflowNetworkLinkageData(count)%Name)// &
                             ' is not found in AirflowNetwork Component Data objects.')
        ErrorsFound = .TRUE.
     end if
  end do

  ! Ensure every AirflowNetworkNode is used in AirflowNetworkLinkage
  do count=1,AirflowNetworkNumOfNodes
     NodeFound1 = .FALSE.
     NodeFound2 = .FALSE.
     do i=1,AirflowNetworkNumOfLinks
        if (count .EQ. AirflowNetworkLinkageData(i)%NodeNums(1)) then
           NodeFound1 = .True.
        end if
        if (count .EQ. AirflowNetworkLinkageData(i)%NodeNums(2)) then
           NodeFound2 = .True.
        end if
     end do
     if ((.NOT. NodeFound1) .AND. Count > NumOfNodesMultiZone .AND. AirflowNetworkNodeData(count)%ExtNodeNum == 0) then
        CALL ShowSevereError(RoutineName//'AIRFLOWNETWORK:DISTRIBUTION:NODE = '//TRIM(AirflowNetworkNodeData(count)%Name) &
                             //' is not found as Node 1 Name in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE')
        Call ShowContinueError('Each non-external AIRFLOWNETWORK:DISTRIBUTION:NODE has to be defined as Node 1 once in ' &
                               //'AIRFLOWNETWORK:DISTRIBUTION:LINKAGE')
        ErrorsFound=.true.
     end if
     if ((.NOT. NodeFound2) .AND. Count > NumOfNodesMultiZone .AND. AirflowNetworkNodeData(count)%ExtNodeNum == 0) then
        CALL ShowSevereError(RoutineName//'AIRFLOWNETWORK:DISTRIBUTION:NODE = '//TRIM(AirflowNetworkNodeData(count)%Name) &
                             //' is not found as Node 2 Name in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE')
        Call ShowContinueError('Each non-external AIRFLOWNETWORK:DISTRIBUTION:NODE has to be defined as Node 2 once in ' &
                               //'AIRFLOWNETWORK:DISTRIBUTION:LINKAGE')
        ErrorsFound=.true.
     end if
     if ((.NOT. NodeFound1) .AND. (.NOT. NodeFound2) .AND. Count > NumOfNodesMultiZone &
        .AND. AirflowNetworkNodeData(count)%ExtNodeNum > 0) then
        CALL ShowSevereError(RoutineName//'AIRFLOWNETWORK:DISTRIBUTION:NODE = '//TRIM(AirflowNetworkNodeData(count)%Name) &
                             //' is not found as Node 1 Name or Node 2 Name in AIRFLOWNETWORK:DISTRIBUTION:LINKAGE')
        Call ShowContinueError('This external AIRFLOWNETWORK:DISTRIBUTION:NODE has to be defined in ' &
                               //'AIRFLOWNETWORK:DISTRIBUTION:LINKAGE')
        ErrorsFound=.true.
     end if
  end do

  ! Ensure there is at least one node defined as EXTERNAL node
  NodeFound = .FALSE.
  do count=1,AirflowNetworkNumOfNodes
     if (AirflowNetworkNodeData(count)%ExtNodeNum > 0) then
        NodeFound = .True.
     end if
  end do
  if (.NOT. NodeFound) then
     CALL ShowSevereError(RoutineName//'No External Nodes found in AirflowNetwork:Multizone:ExternalNode. '// &
                          'There must be at least 1 external node defined.')
     ErrorsFound=.true.
  end if

  if (AirflowNetworkSimu%iWPCCntr == iWPCCntr_Input) then
    do count=1,AirflowNetworkNumOfSurfaces
      If (AirflowNetworkLinkageData(count)%NodeNums(1) == 0) then
        CALL ShowSevereError('The surface is not found '// &
                             'in AIRFLOWNETWORK:MULTIZONE:SURFACE = '//TRIM(AirflowNetworkLinkageData(count)%Name))
        ErrorsFound=.true.
      End If
      If (AirflowNetworkLinkageData(count)%NodeNums(2) == 0) then
        CALL ShowSevereError('The external node is not found '// &
                             'in AIRFLOWNETWORK:MULTIZONE:SURFACE = '//TRIM(AirflowNetworkLinkageData(count)%Name))
        ErrorsFound=.true.
      End If
    end do
  End If

  ! Provide a warning when a door component is assigned as envelope leakage
  if (.Not. ErrorsFound) THEN
    do count=1,AirflowNetworkNumOfSurfaces
      if (AirflowNetworkNodeData(AirflowNetworkLinkageData(count)%NodeNums(1))%ExtNodeNum > 0 .AND. &
         AirflowNetworkNodeData(AirflowNetworkLinkageData(count)%NodeNums(2))%EPlusZoneNum > 0 .AND. &
         AirflowNetworkLinkageData(count)%CompNum > 0) then
        if (AirflowNetworkCompData(AirflowNetworkLinkageData(count)%CompNum)%CompTypeNum == CompTypeNum_SOP) then
!            CALL ShowWarningError('A door component is assigned between an external node and a thermal zone ' &
!                 //'in AirflowNetwork linkage data = '//TRIM(AirflowNetworkLinkageData(count)%Name))
!            CALL ShowContinueError('This represents a large opening between indoor and outdoors. You may want to ' &
!                                   //'reconsider your input.')
        end if
      end if
      if (AirflowNetworkNodeData(AirflowNetworkLinkageData(count)%NodeNums(2))%ExtNodeNum > 0 .AND. &
         AirflowNetworkNodeData(AirflowNetworkLinkageData(count)%NodeNums(1))%EPlusZoneNum > 0 .AND. &
         AirflowNetworkLinkageData(count)%CompNum > 0) then
        if (AirflowNetworkCompData(AirflowNetworkLinkageData(count)%CompNum)%CompTypeNum == CompTypeNum_SOP) then
!            CALL ShowWarningError('A door component is assigned between an external node and a thermal zone ' &
!                 //'in AirflowNetwork linkage data = '//TRIM(AirflowNetworkLinkageData(count)%Name))
!            CALL ShowContinueError('This represents a large opening between indoor and outdoors. You may want to ' &
!                                   //'reconsider your input.')
        end if
      end if
    end do
  End If

  ! Ensure the name of each heat exchanger is shown either once ot twice in the field of
  If (SimulateAirflowNetwork == AirflowNetworkControlSimpleADS .OR. SimulateAirflowNetwork == AirflowNetworkControlMultiADS) Then
    Do I=1,DisSysNumOfHXs
      count = 0
      Do j=1,AirflowNetworkNumOfLinks
        If (Samestring(AirflowNetworkLinkageData(j)%CompName,DisSysCompHXData(i)%Name)) Then
          count = count+1
        End If
      End Do

      If (DisSysCompHXData(i)%CoilParentExists .AND. count .NE. 2) Then
        CALL ShowSevereError(RoutineName//'The inputs of component name field as a heat exchanger in ' &
                          //'AIRFLOWNETWORK:DISTRIBUTION:LINKAGE is not correct')
        Call ShowContinueError('The entered name of heat enchanger is '//Trim(DisSysCompHXData(i)%Name)// &
                          ' in AirflowNetwork:Distribution:Component:HeatExchanger objects')
        Call ShowContinueError('The correct apperance number is 2. The entered apperance number is '//TRIM(RoundSigDigits(count,0)))
        ErrorsFound=.true.
      End If
      If ((.NOT. DisSysCompHXData(i)%CoilParentExists).AND. count .NE. 1) Then
        CALL ShowSevereError(RoutineName//'The inputs of component name field as a heat exchanger in ' &
                          //'AIRFLOWNETWORK:DISTRIBUTION:LINKAGE is not correct')
        Call ShowContinueError('The entered name of heat enchanger is '//Trim(DisSysCompHXData(i)%Name)// &
                          ' in AirflowNetwork:Distribution:Component:HeatExchanger objects')
        Call ShowContinueError('The correct apperance number is 1. The entered apperance number is '//TRIM(RoundSigDigits(count,0)))
        ErrorsFound=.true.
      End If
    End Do
  End If

  IF (ErrorsFound) THEN
     CALL ShowFatalError(RoutineName//'Errors found getting inputs. Previous error(s) cause program termination.')
  END IF

  DEALLOCATE(Alphas)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(Numbers)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(lNumericBlanks)

  IF (.Not. ErrorsFound) THEN
    CALL AllocateAndInitData
  ENDIF

  RETURN

END SUBROUTINE GetAirflowNetworkInput


SUBROUTINE InitAirflowNetwork
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Aug. 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes variables of additional zone loads caused by ADS.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

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
    LOGICAL,SAVE :: OneTimeFlag = .True.
    LOGICAL, SAVE :: MyEnvrnFlag = .true.
    INTEGER I

    if (OneTimeFlag) then
       ALLOCATE(AirflowNetworkExchangeData(NumOfZones)) ! AirflowNetwork exchange data due to air-forced system
       If (SupplyFanType .EQ. FanType_SimpleOnOff) then
         ALLOCATE(AirflowNetworkMultiExchangeData(NumOfZones))
       End If
       OneTimeFlag = .False.
       IF (Contaminant%CO2Simulation) Then
         DO i=1, NumOfZOnes
           CALL SetupOutputVariable('AFN Zone Outdoor Air Mass Flow Rate [kg/s]',AirflowNetworkExchangeData(i)%SumMHr, &
              'System','Average',Zone(i)%Name)
           CALL SetupOutputVariable('AFN Zone Mixing Mass Flow Rate [kg/s]',AirflowNetworkExchangeData(i)%SumMMHr, &
              'System','Average',Zone(i)%Name)
           CALL SetupOutputVariable('AFN Zone Outdoor Air CO2 Mass Flow Rate [kg/s]',AirflowNetworkExchangeData(i)%SumMHrCO, &
              'System','Average',Zone(i)%Name)
           CALL SetupOutputVariable('AFN Zone Mixing CO2 Mass Flow Rate [kg/s]',AirflowNetworkExchangeData(i)%SumMMHrCO, &
              'System','Average',Zone(i)%Name)
           CALL SetupOutputVariable('AFN Zone Total CO2 Mass Flow Rate [kg/s]',AirflowNetworkExchangeData(i)%TotalCO2, &
              'System','Average',Zone(i)%Name)
         END DO
       END IF
       IF (Contaminant%GenericContamSimulation) Then
         DO i=1, NumOfZOnes
           IF (.NOT. Contaminant%CO2Simulation) Then
             CALL SetupOutputVariable('AFN Zone Outdoor Air Mass Flow Rate [kg/s]',AirflowNetworkExchangeData(i)%SumMHr, &
              'System','Average',Zone(i)%Name)
             CALL SetupOutputVariable('AFN Zone Mixing Mass Flow Rate [kg/s]',AirflowNetworkExchangeData(i)%SumMMHr, &
              'System','Average',Zone(i)%Name)
           End If
           CALL SetupOutputVariable('AFN Zone Outdoor Air Generic Air Contaminant Mass Flow Rate [kg/s]', &
              AirflowNetworkExchangeData(i)%SumMHrGC,'System','Average',Zone(i)%Name)
           CALL SetupOutputVariable('AFN Zone Mixing Generic Air Contaminant Mass Flow Rate [kg/s]', &
              AirflowNetworkExchangeData(i)%SumMMHrGC,'System','Average',Zone(i)%Name)
           CALL SetupOutputVariable('AFN Zone Total Generic Air Contaminant Mass Flow Rate [kg/s]', &
              AirflowNetworkExchangeData(i)%TotalGC,'System','Average',Zone(i)%Name)
         END DO
       END IF
    end if

   IF (BeginEnvrnFlag .and. MyEnvrnFlag) THEN
     ! Assign node values
     DO I=1,AirflowNetworkNumOfNodes
        AirflowNetworkNodeSimu(I)%TZ = 23.0d0
        AirflowNetworkNodeSimu(I)%WZ = 0.00084d0
        AirflowNetworkNodeSimu(I)%PZ = 0.0d0
        IF (Contaminant%CO2Simulation) AirflowNetworkNodeSimu(I)%CO2Z = OutdoorCO2
        IF (Contaminant%GenericContamSimulation) AirflowNetworkNodeSimu(I)%GCZ = OutdoorGC
     END DO

     DO I=1,AirflowNetworkNumOfLinks
        AirflowNetworkLinkSimu(I)%flow = 0.0d0
        AirflowNetworkLinkSimu(I)%flow2 = 0.0d0
     END DO

     DO I=1,NumOfZones
        ANZT(I) = MAT(I)
        ANZW(I) = ZoneAirHumRat(I)
        IF (Contaminant%CO2Simulation) ANCO(I) = ZoneAirCO2(I)
        IF (Contaminant%GenericContamSimulation) ANGC(I) = ZoneAirGC(I)
     End Do
     MyEnvrnFlag=.false.
   ENDIF
   IF (.not. BeginEnvrnFlag) THEN
     MyEnvrnFlag=.true.
     if (SimulateAirflowNetwork .gt. AirflowNetworkControlSimple) then
       If (RollBackFlag) then
         DO I=1,NumOfZones
           ANZT(I) = XMAT(I)
           ANZW(I) = WZoneTimeMinus1(I)
           IF (Contaminant%CO2Simulation) ANCO(I) = CO2ZoneTimeMinus1(I)
           IF (Contaminant%GenericContamSimulation) ANGC(I) = GCZoneTimeMinus1(I)
         End Do
       else
         DO I=1,NumOfZones
           ANZT(I) = MAT(I)
           ANZW(I) = ZoneAirHumRat(I)
           IF (Contaminant%CO2Simulation) ANCO(I) = ZoneAirCO2(I)
           IF (Contaminant%GenericContamSimulation) ANGC(I) = ZoneAirGC(I)
         End Do
       end if

       DO I=1,AirflowNetworkNumOfNodes
         if (AirflowNetworkNodeData(i)%EPlusZoneNum > 0) then
           AirflowNetworkNodeSimu(I)%TZ = ANZT(AirflowNetworkNodeData(i)%EPlusZoneNum)
           AirflowNetworkNodeSimu(I)%WZ = ANZW(AirflowNetworkNodeData(i)%EPlusZoneNum)
           IF (Contaminant%CO2Simulation) AirflowNetworkNodeSimu(I)%CO2Z = ANCO(AirflowNetworkNodeData(i)%EPlusZoneNum)
           IF (Contaminant%GenericContamSimulation) AirflowNetworkNodeSimu(I)%GCZ = ANGC(AirflowNetworkNodeData(i)%EPlusZoneNum)
         end if
         if (AirflowNetworkNodeData(i)%ExtNodeNum > 0) then
           AirflowNetworkNodeSimu(I)%TZ = OutDryBulbTempAt(AirflowNetworkNodeData(i)%NodeHeight)
           AirflowNetworkNodeSimu(I)%WZ = OutHumRat
           IF (Contaminant%CO2Simulation) AirflowNetworkNodeSimu(I)%CO2Z = OutdoorCO2
           IF (Contaminant%GenericContamSimulation) AirflowNetworkNodeSimu(I)%GCZ = OutdoorGC
         end if
       END DO
     End If
   ENDIF

   AirflowNetworkExchangeData%TotalSen = 0.0d0
   AirflowNetworkExchangeData%TotalLat = 0.0d0
   AirflowNetworkExchangeData%MultiZoneSen = 0.0d0
   AirflowNetworkExchangeData%MultiZoneLat = 0.0d0
   AirflowNetworkExchangeData%LeakSen = 0.0d0
   AirflowNetworkExchangeData%LeakLat = 0.0d0
   AirflowNetworkExchangeData%CondSen = 0.0d0
   AirflowNetworkExchangeData%DiffLat = 0.0d0
   IF (Contaminant%CO2Simulation) AirflowNetworkExchangeData%TotalCO2 = 0.0d0
   IF (Contaminant%GenericContamSimulation) AirflowNetworkExchangeData%TotalGC = 0.0d0

   RETURN
END SUBROUTINE InitAirflowNetwork

SUBROUTINE AllocateAndInitData


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Aug. 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes variables and allocates dynamic arrays.


          ! METHODOLOGY EMPLOYED:
          ! na


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
  USE DataGlobals, ONLY: AnyEnergyManagementSystemInModel
  USE DataInterfaces, ONLY: SetupEMSActuator

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
INTEGER i, ZoneNum, N, SurfNum
INTEGER, EXTERNAL :: GetNewUnitNumber  ! External  function to "get" a unit number

   ALLOCATE(AirflowNetworkNodeSimu(AirflowNetworkNumOfNodes)) ! Node simulation variable in air distribution system
   ALLOCATE(AirflowNetworkLinkSimu(AirflowNetworkNumOfLinks)) ! Link simulation variable in air distribution system
   ALLOCATE(AirflowNetworkLinkReport(AirflowNetworkNumOfLinks)) ! Report link simulation variable in air distribution system

   If (SupplyFanType .EQ. FanType_SimpleOnOff) then
     ALLOCATE(AirflowNetworkNodeReport(AirflowNetworkNumOfZones))
     ALLOCATE(AirflowNetworkLinkReport1(AirflowNetworkNumOfSurfaces))
   End If

   ALLOCATE(MA(AirflowNetworkNumOfNodes*AirflowNetworkNumOfNodes))
   ALLOCATE(MV(AirflowNetworkNumOfNodes))
   ALLOCATE(IVEC(AirflowNetworkNumOfNodes+20))

   ALLOCATE(AirflowNetworkReportData(NumOfZones)) ! Report variables
   ALLOCATE(AirflowNetworkZnRpt(NumOfZones)) ! Report variables

   ALLOCATE(ANZT(NumOfZones))              ! Local zone air temperature for rollback use
   ALLOCATE(ANZW(NumOfZones))              ! Local zone humidity ratio for rollback use
   IF (Contaminant%CO2Simulation) ALLOCATE(ANCO(NumOfZones)) ! Local zone CO2 for rollback use
   IF (Contaminant%GenericContamSimulation) ALLOCATE(ANGC(NumOfZones)) ! Local zone generic contaminant for rollback use

   CALL AllocateAirflowNetworkData

   ! CurrentModuleObject='AirflowNetwork Simulations'
    DO I=1,AirflowNetworkNumOfNodes
       CALL SetupOutputVariable('AFN Node Temperature [C]',AirflowNetworkNodeSimu(I)%TZ,'System','Average', &
                                AirflowNetworkNodeData(i)%Name)
       CALL SetupOutputVariable('AFN Node Humidity Ratio [kgWater/kgDryAir]',AirflowNetworkNodeSimu(I)%WZ,  &
          'System','Average',AirflowNetworkNodeData(i)%Name)
       IF (Contaminant%CO2Simulation)  CALL SetupOutputVariable('AFN Node CO2 Concentration [ppm]' &
               ,AirflowNetworkNodeSimu(I)%CO2Z,'System','Average', AirflowNetworkNodeData(i)%Name)
       IF (Contaminant%GenericContamSimulation)  CALL SetupOutputVariable('AFN Node Generic Air Contaminant Concentration [ppm]' &
               ,AirflowNetworkNodeSimu(I)%GCZ,'System','Average', AirflowNetworkNodeData(i)%Name)
       If (.NOT. (SupplyFanType .EQ. FanType_SimpleOnOff .AND. i .LE. AirflowNetworkNumOfZones)) &
       CALL SetupOutputVariable('AFN Node Total Pressure [Pa]',AirflowNetworkNodeSimu(I)%PZ,'System','Average', &
                                AirflowNetworkNodeData(i)%Name)
       If (AirflowNetworkNodeData(i)%ExtNodeNum > 0) then
          CALL SetupOutputVariable('AFN Node Wind Pressure [Pa]',AirflowNetworkNodeSimu(I)%PZ,'System','Average', &
                                AirflowNetworkNodeData(i)%Name)
       End If
    END DO

    DO i=1,AirflowNetworkNumOfLinks
     If (.NOT. (SupplyFanType .EQ. FanType_SimpleOnOff .AND. i .LE. AirflowNetworkNumOfSurfaces)) Then
       CALL SetupOutputVariable('AFN Linkage Node 1 to Node 2 Mass Flow Rate [kg/s]',AirflowNetworkLinkReport(I)%FLOW, &
            'System','Average',AirflowNetworkLinkageData(i)%Name)
       CALL SetupOutputVariable('AFN Linkage Node 2 to Node 1 Mass Flow Rate [kg/s]',AirflowNetworkLinkReport(I)%FLOW2, &
            'System','Average',AirflowNetworkLinkageData(i)%Name)
       CALL SetupOutputVariable('AFN Linkage Node 1 to Node 2 Volume Flow Rate [m3/s]',AirflowNetworkLinkReport(I)%VolFLOW, &
            'System','Average',AirflowNetworkLinkageData(i)%Name)
       CALL SetupOutputVariable('AFN Linkage Node 2 to Node 1 Volume Flow Rate [m3/s]',AirflowNetworkLinkReport(I)%VolFLOW2, &
            'System','Average',AirflowNetworkLinkageData(i)%Name)
       CALL SetupOutputVariable('AFN Linkage Node 1 to Node 2 Pressure Difference [Pa]',AirflowNetworkLinkSimu(I)%DP, &
            'System','Average',AirflowNetworkLinkageData(i)%Name)
     End If
    END DO

    DO i=1,AirflowNetworkNumOfSurfaces
       N = AirflowNetworkLinkageData(i)%CompNum
       IF (AirflowNetworkCompData(N)%CompTypeNum==CompTypeNum_DOP .OR. &
          AirflowNetworkCompData(N)%CompTypeNum==CompTypeNum_SOP .OR. &
          AirflowNetworkCompData(N)%CompTypeNum==CompTypeNum_HOP) THEN
          SurfNum = MultizoneSurfaceData(i)%SurfNum
          CALL SetupOutputVariable('AFN Surface Venting Window or Door Opening Factor []',  &
             MultizoneSurfaceData(I)%OpenFactor,'System','Average', &
            MultizoneSurfaceData(I)%SurfName)
          IF (AnyEnergyManagementSystemInModel) THEN
            CALL SetupEMSActuator('AirFlow Network Window/Door Opening', MultizoneSurfaceData(I)%SurfName, &
                                  'Venting Opening Factor' , '[Fraction]', &
                                   MultizoneSurfaceData(I)%EMSOpenFactorActuated, MultizoneSurfaceData(I)%EMSOpenFactor )
          ENDIF
          CALL SetupOutputVariable('AFN Surface Venting Window or Door Opening Modulation Multiplier []',  &
               SurfaceWindow(SurfNum)%VentingOpenFactorMultRep,'System','Average',Surface(SurfNum)%Name)
          CALL SetupOutputVariable('AFN Surface Venting Inside Setpoint Temperature [C]',  &
               SurfaceWindow(SurfNum)%InsideTempForVentingRep,'System','Average',Surface(SurfNum)%Name)
          CALL SetupOutputVariable('AFN Surface Venting Availability Status []',  &
               SurfaceWindow(SurfNum)%VentingAvailabilityRep,'System','Average',Surface(SurfNum)%Name)
       END IF
    END DO

    DO I=1, NumOfZones
       ! Multizone losses due to force air systems
       CALL SetupOutputVariable('AFN Zone Infiltration Sensible Heat Gain Rate [W]',  &
                                AirflowNetworkReportData(i)%MultiZoneInfiSenGainW,  &
                                'System','Average',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Infiltration Sensible Heat Gain Energy [J]',  &
          AirflowNEtworkReportData(i)%MultiZoneInfiSenGainJ, &
                                'System','Sum',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Mixing Sensible Heat Gain Rate [W]',AirflowNetworkReportData(i)%MultiZoneMixSenGainW,  &
                                'System','Average',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Mixing Sensible Heat Gain Energy [J]',AirflowNEtworkReportData(i)%MultiZoneMixSenGainJ, &
                                'System','Sum',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Infiltration Sensible Heat Loss Rate [W]',  &
                                AirflowNEtworkReportData(i)%MultiZoneInfiSenLossW,  &
                                'System','Average',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Infiltration Sensible Heat Loss Energy [J]',  &
          AirflowNEtworkReportData(i)%MultiZoneInfiSenLossJ, &
                                'System','Sum',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Mixing Sensible Heat Loss Rate [W]',AirflowNEtworkReportData(i)%MultiZoneMixSenLossW,  &
                                'System','Average',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Mixing Sensible Heat Loss Energy [J]',AirflowNEtworkReportData(i)%MultiZoneMixSenLossJ, &
                                'System','Sum',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Infiltration Latent Heat Gain Rate [W]',  &
                                AirflowNEtworkReportData(i)%MultiZoneInfiLatGainW,  &
                                'System','Average',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Infiltration Latent Heat Gain Energy [J]',  &
          AirflowNEtworkReportData(i)%MultiZoneInfiLatGainJ, &
                                'System','Sum',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Infiltration Latent Heat Loss Rate [W]',  &
                                AirflowNEtworkReportData(i)%MultiZoneInfiLatLossW,  &
                                'System','Average',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Infiltration Latent Heat Loss Energy [J]',  &
          AirflowNEtworkReportData(i)%MultiZoneInfiLatLossJ, &
                                'System','Sum',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Mixing Latent Heat Gain Rate [W]',AirflowNEtworkReportData(i)%MultiZoneMixLatGainW,  &
                                'System','Average',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Mixing Latent Heat Gain Energy [J]',AirflowNEtworkReportData(i)%MultiZoneMixLatGainJ, &
                                'System','Sum',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Mixing Latent Heat Loss Rate [W]',AirflowNEtworkReportData(i)%MultiZoneMixLatLossW,  &
                                'System','Average',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Mixing Latent Heat Loss Energy [J]',AirflowNEtworkReportData(i)%MultiZoneInfiLatLossJ, &
                                'System','Sum',Zone(i)%Name)
       ! Supply leak losses due to force air systems
       CALL SetupOutputVariable('AFN Zone Duct Leaked Air Sensible Heat Gain Rate [W]',AirflowNEtworkReportData(i)%LeakSenGainW, &
                                'System','Average',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Duct Leaked Air Sensible Heat Gain Energy [J]',AirflowNEtworkReportData(i)%LeakSenGainJ, &
                                'System','Sum',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Duct Leaked Air Sensible Heat Loss Rate [W]',AirflowNEtworkReportData(i)%LeakSenLossW, &
                                'System','Average',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Duct Leaked Air Sensible Heat Loss Energy [J]',AirflowNEtworkReportData(i)%LeakSenLossJ, &
                                'System','Sum',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Duct Leaked Air Latent Heat Gain Rate [W]',AirflowNEtworkReportData(i)%LeakLatGainW, &
                                'System','Average',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Duct Leaked Air Latent Heat Gain Energy [J]',AirflowNEtworkReportData(i)%LeakLatGainJ, &
                                'System','Sum',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Duct Leaked Air Latent Heat Loss Rate [W]',AirflowNEtworkReportData(i)%LeakLatLossW, &
                                'System','Average',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Duct Leaked Air Latent Heat Loss Energy [J]',AirflowNEtworkReportData(i)%LeakLatLossJ, &
                                'System','Sum',Zone(i)%Name)
       ! Conduction losses due to force air systems
       CALL SetupOutputVariable('AFN Zone Duct Conduction Sensible Heat Gain Rate [W]',AirflowNEtworkReportData(i)%CondSenGainW,  &
                                'System','Average',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Duct Conduction Sensible Heat Gain Energy [J]',AirflowNEtworkReportData(i)%CondSenGainJ, &
                                'System','Sum',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Duct Conduction Sensible Heat Loss Rate [W]',AirflowNEtworkReportData(i)%CondSenLossW,  &
                                'System','Average',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Duct Conduction Sensible Heat Loss Energy [J]',AirflowNEtworkReportData(i)%CondSenLossJ, &
                                'System','Sum',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Duct Diffusion Latent Heat Gain Rate [W]',AirflowNEtworkReportData(i)%DiffLatGainW, &
                                'System','Average',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Duct Diffusion Latent Heat Gain Energy [J]',AirflowNEtworkReportData(i)%DiffLatGainJ, &
                                'System','Sum',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Duct Diffusion Latent Heat Loss Rate [W]',AirflowNEtworkReportData(i)%DiffLatLossW, &
                                'System','Average',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Duct Diffusion Latent Heat Loss Energy [J]',AirflowNEtworkReportData(i)%DiffLatLossJ, &
                                'System','Sum',Zone(i)%Name)
       ! Total losses due to force air systems
       CALL SetupOutputVariable('AFN Distribution Sensible Heat Gain Rate [W]',AirflowNetworkReportData(i)%TotalSenGainW, &
                                'System','Average',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Distribution Sensible Heat Gain Energy [J]',AirflowNEtworkReportData(i)%TotalSenGainJ, &
                                'System','Sum',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Distribution Sensible Heat Loss Rate [W]',AirflowNEtworkReportData(i)%TotalSenLossW, &
                                'System','Average',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Distribution Sensible Heat Loss Energy [J]',AirflowNEtworkReportData(i)%TotalSenLossJ, &
                                'System','Sum',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Distribution Latent Heat Gain Rate [W]',AirflowNEtworkReportData(i)%TotalLatGainW, &
                                'System','Average',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Distribution Latent Heat Gain Energy [J]',AirflowNEtworkReportData(i)%TotalLatGainJ, &
                                'System','Sum',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Distribution Latent Heat Loss Rate [W]',AirflowNEtworkReportData(i)%TotalLatLossW, &
                                'System','Average',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Distribution Latent Heat Loss Energy [J]',AirflowNEtworkReportData(i)%TotalLatLossJ, &
                                'System','Sum',Zone(i)%Name)
    END DO

    DO i=1,NumOfZones
       CALL SetupOutputVariable('AFN Zone Infiltration Volume [m3]',AirflowNetworkZnRpt(i)%InfilVolume, &
                                'System','Sum',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Infiltration Mass [kg]',AirflowNetworkZnRpt(i)%InfilMass, &
                                'System','Sum',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Infiltration Air Change Rate [ach]',  &
                                AirflowNetworkZnRpt(i)%InfilAirChangeRate,  &
                                'System','Average',Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Mixing Volume [m3]',AirflowNetworkZnRpt(i)%MixVolume,'System','Sum',  &
                                 Zone(i)%Name)
       CALL SetupOutputVariable('AFN Zone Mixing Mass [kg]',AirflowNetworkZnRpt(i)%MixMass,'System','Sum',Zone(i)%Name)
    ENDDO

    If (SupplyFanType .EQ. FanType_SimpleOnOff) then
      DO i=1,AirflowNetworkNumOfZones
        CALL SetupOutputVariable('AFN Zone Average Pressure [Pa]',AirflowNetworkNodeReport(i)%PZ, &
                                'System','Average',Zone(i)%Name)
        CALL SetupOutputVariable('AFN Zone On Cycle Pressure [Pa]',AirflowNetworkNodeReport(i)%PZON, &
                                'System','Average',Zone(i)%Name)
        CALL SetupOutputVariable('AFN Zone Off Cycle Pressure [Pa]',AirflowNetworkNodeReport(i)%PZOFF, &
                                'System','Average',Zone(i)%Name)
      ENDDO
      Do I=1,AirflowNetworkNumOfSurfaces
        CALL SetupOutputVariable('AFN Linkage Node 1 to 2 Average Mass Flow Rate [kg/s]', &
          AirflowNetworkLinkReport1(I)%FLOW, 'System','Average',MultizoneSurfaceData(I)%SurfName)
        CALL SetupOutputVariable('AFN Linkage Node 2 to 1 Average Mass Flow Rate [kg/s]', &
          AirflowNetworkLinkReport1(I)%FLOW2, 'System','Average',MultizoneSurfaceData(I)%SurfName)
        CALL SetupOutputVariable('AFN Linkage Node 1 to 2 Average Volume Flow Rate [m3/s]', &
          AirflowNetworkLinkReport1(I)%VolFLOW, 'System','Average',MultizoneSurfaceData(I)%SurfName)
        CALL SetupOutputVariable('AFN Linkage Node 2 to 1 Average Volume Flow Rate [m3/s]', &
          AirflowNetworkLinkReport1(I)%VolFLOW2, 'System','Average',MultizoneSurfaceData(I)%SurfName)
        CALL SetupOutputVariable('AFN Surface Average Pressure Difference [Pa]',AirflowNetworkLinkReport1(I)%DP, &
            'System','Average',MultizoneSurfaceData(I)%SurfName)
        CALL SetupOutputVariable('AFN Surface On Cycle Pressure Difference [Pa]',  &
             AirflowNetworkLinkReport1(I)%DPON, &
            'System','Average',MultizoneSurfaceData(I)%SurfName)
        CALL SetupOutputVariable('AFN Surface Off Cycle Pressure Difference [Pa]',  &
             AirflowNetworkLinkReport1(I)%DPOFF, &
            'System','Average',MultizoneSurfaceData(I)%SurfName)
      End Do
    End If

! Assign node reference height
    Do i=1,AirflowNetworkNumOfNodes
       AirflowNetworkNodeData(i)%NodeHeight = 0.0d0
       ZoneNum = AirflowNetworkNodeData(i)%EPlusZoneNum
       If (ZoneNum > 0) then
         If (WorldCoordSystem) then
           AirflowNetworkNodeData(i)%NodeHeight = 0.0d0
         Else
           AirflowNetworkNodeData(i)%NodeHeight = Zone(ZoneNum)%OriginZ
         End If
       end if
    end do

900 Format(1X,i2)
901 Format(1X,2I4,4F9.4)
902 Format(1X,2I4,4F9.4)
903 Format(9X,4F9.4)
904 Format(1X,2I4,1F9.4)
910 Format(1X,I4,2(I4,F9.4),I4,2F4.1)


RETURN
END SUBROUTINE AllocateAndInitData

SUBROUTINE CalcAirflowNetworkAirBalance
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Oct. 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine performs simulations of nodal pressures and linkage airflows.


          ! METHODOLOGY EMPLOYED:
          ! na


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
   Use DataAirLoop , Only: AirToZoneNodeInfo
   USE DataHVACGlobals, ONLY: TurnFansOn, TurnFansOff
   USE InputProcessor, ONLY: SameString  ! NEEDS TO BE CHANGED after V1.3 release!!!


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
integer I,j,N
REAL(r64) Vref
LOGICAL,SAVE :: OneTimeFlag = .True.
LOGICAL :: ErrorsFound=.false.
REAL(r64)  GlobalOpenFactor

   ! Validate supply and return connections
if (OneTimeFlag) then
  OneTimeFlag = .False.
  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetAirflowNetworkInput: Program terminates for preceding reason(s).')
  ENDIF
end if

   DO N=1,NetworkNumOfNodes
     if (AirflowNetworkNodeData(N)%NodeTypeNum.EQ.0) then
       AirflowNetworkNodeSimu(N)%PZ = 0.0d0
     else
       ! Assing ambient conditions to external nodes
       i = AirflowNetworkNodeData(N)%ExtNodeNum
       if (i > 0) then
         If (i .LE. AirflowNetworkNumOfExtNode) then
           Vref = WindSpeedAt(MultizoneExternalNodeData(i)%height)
           AirflowNetworkNodeSimu(N)%PZ = CalcWindPressure(MultizoneExternalNodeData(i)%CPVNum,Vref, &
                                          AirflowNetworkNodeData(N)%NodeHeight)
         End if
         AirflowNetworkNodeSimu(N)%TZ = OutDryBulbTempAt(AirflowNetworkNodeData(N)%NodeHeight)
         AirflowNetworkNodeSimu(N)%WZ = OutHumRat
       else
         CALL ShowSevereError('GetAirflowNetworkInput: AIRFLOWNETWORK:DISTRIBUTION:NODE: Invalid external node = ' &
                             //TRIM(AirflowNetworkNodeData(N)%Name))
         ErrorsFound=.true.
       end if
     end if
   END DO

  Do i=1,AirflowNetworkNumOfSurfaces
    MultizoneSurfaceData(i)%OpenFactor = 0.0d0
    j = MultizoneSurfaceData(i)%SurfNum
    If (SurfaceWindow(j)%OriginalClass == SurfaceClass_Window .OR. SurfaceWindow(j)%OriginalClass == SurfaceClass_Door &
        .OR. SurfaceWindow(j)%OriginalClass == SurfaceClass_GlassDoor) then
      Call AirflowNetworkVentingControl(i,MultizoneSurfaceData(i)%OpenFactor)
      MultizoneSurfaceData(i)%OpenFactor = MultizoneSurfaceData(i)%OpenFactor*MultizoneSurfaceData(i)%WindModifier
      If (MultizoneSurfaceData(i)%HybridVentClose) MultizoneSurfaceData(i)%OpenFactor = 0.0d0
      if (AirflowNetworkFanActivated .AND. (SimulateAirflowNetwork > AirflowNetworkControlMultizone).AND. &
          MultizoneSurfaceData(i)%OpenFactor .GT. 0.0d0 .AND. Surface(j)%ExtBoundCond == ExternalEnvironment .AND.   &
          .NOT. WarmUpFlag) then
        ! Exterior Large opening only
        MultizoneSurfaceData(i)%ExtLargeOpeningErrCount = MultizoneSurfaceData(i)%ExtLargeOpeningErrCount + 1
        if (MultizoneSurfaceData(i)%ExtLargeOpeningErrCount < 2) then
          CALL ShowWarningError('AirflowNetwork: The window or door is open during HVAC system operation ' &
                                //TRIM(MultizoneSurfaceData(i)%SurfName))
          CALL ShowContinueError('The window or door opening factor is ' &
               //Trim(RoundSigDigits(MultizoneSurfaceData(i)%OpenFactor,2)))
          CALL ShowContinueErrorTimeStamp(' ')
        else
          CALL ShowRecurringWarningErrorAtEnd('AirFlowNetwork: '//TRIM(MultizoneSurfaceData(i)%SurfName)//&
             ' The window or door is open during HVAC system operation error continues...', &
             MultizoneSurfaceData(i)%ExtLargeOpeningErrIndex, MultizoneSurfaceData(i)%OpenFactor,   &
             MultizoneSurfaceData(i)%OpenFactor)
        end if
      End If
      If (MultizoneSurfaceData(i)%OpenFactor .GT. 1.0d0) then
        MultizoneSurfaceData(i)%OpenFactorErrCount = MultizoneSurfaceData(i)%OpenFactorErrCount + 1
        if (MultizoneSurfaceData(i)%OpenFactorErrCount < 2) then
          CALL ShowWarningError('AirflowNetwork: The window or door opening factor is greater than 1.0 ' &
                                //TRIM(MultizoneSurfaceData(i)%SurfName))
          CALL ShowContinueErrorTimeStamp(' ')
        else
          CALL ShowRecurringWarningErrorAtEnd('AirFlowNetwork: '//TRIM(MultizoneSurfaceData(i)%SurfName)//&
             ' The window or door opening factor is greater than 1.0 error continues...' &
             , MultizoneSurfaceData(i)%OpenFactorErrIndex, MultizoneSurfaceData(i)%OpenFactor, MultizoneSurfaceData(i)%OpenFactor)
        end if
      End If
    end if
  end do

  ! Check if the global ventilation control is applied or not
  GlobalOpenFactor = -1.0d0
  Do i=1,AirflowNetworkNumOfSurfaces
    If (MultizoneSurfaceData(i)%HybridCtrlMaster) Then
      GlobalOpenFactor = MultizoneSurfaceData(i)%OpenFactor
      Exit
    End If
  End Do
  If (GlobalOpenFactor .GE. 0.0d0) Then
    Do i=1,AirflowNetworkNumOfSurfaces
      j = MultizoneSurfaceData(i)%SurfNum
      If (SurfaceWindow(j)%OriginalClass == SurfaceClass_Window .OR. SurfaceWindow(j)%OriginalClass == SurfaceClass_Door &
        .OR. SurfaceWindow(j)%OriginalClass == SurfaceClass_GlassDoor) then
        If (MultizoneSurfaceData(i)%HybridCtrlGlobal) Then
          MultizoneSurfaceData(i)%OpenFactor = GlobalOpenFactor
        End If
      End If
    End Do
  End IF

  CALL InitAirflowNetworkData
  CALL AIRMOV

  RETURN

END SUBROUTINE CalcAirflowNetworkAirBalance

SUBROUTINE CalcWindPressureCoeffs

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   May 2003
          !       MODIFIED       Revised by L. Gu, Nov. 2005, to meet requirements of AirflowNetwork
          !       MODIFIED       Revised by L. Gu, Dec. 2008, to set the number of external nodes based on
          !                      the number of external surfaces
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates surface-average wind pressure coefficients for
          ! the walls and roof of a rectangular building.

          ! METHODOLOGY EMPLOYED:
          ! Interpolates correlations between surface-average wind pressure coefficient and wind direction based on
          ! measurements (see REFERENCES). Applicable only to rectangular buildings.

          ! REFERENCES:
          ! For low-rise buildings: M.V. Swami and S. Chandra, Correlations for Pressure Distribution
          ! on Buildings and Calculation of Natural-Ventilation Airflow. ASHRAE Transactions 94 (1): 243-266.
          ! For high-rise buildings: 2001 ASHRAE Fundamentals Handbook, p. 16.5, Fig. 7, "Surface Averaged
          ! Wall Pressure Coefficients for Tall Buildings" and p.16.6, Fig. 9, "Surface Averaged Roof Pressure
          ! Coefficients for Tall Buildings; from R.E. Akins, J.A. Peterka, and J.E. Cermak. 1979.
          ! Averaged Pressure Coefficients for Rectangular Buildings. Wind Engineering. Proc. Fifth
          ! International Conference 7:369-80, Fort Collins, CO. Pergamon Press, NY.

          ! USE STATEMENTS:
  USE DataSurfaces
  USE InputProcessor, ONLY: SameString

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:na
          ! SUBROUTINE PARAMETER DEFINITIONS:na
          ! INTERFACE BLOCK SPECIFICATIONS:na
          ! DERIVED TYPE DEFINITIONS:na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: FacadeNum             ! Facade number
    INTEGER :: ExtNum                ! External number
    REAL(r64)    :: FacadeAng(5)     ! Facade azimuth angle (for walls, angle of outward normal
                                     ! to facade measured clockwise from North) (deg)
    REAL(r64)    :: SideRatio        ! For vertical facades, width of facade / width of adjacent facade
    REAL(r64)    :: SR               ! SideRatio restricted to 0.25 to 4.0 range
    REAL(r64)    :: SideRatioFac     ! Log(SideRatio)
    INTEGER :: WindDirNum            ! Wind direction number
    REAL(r64)    :: WindAng          ! Wind direction angle (degrees clockwise from North)
    REAL(r64)    :: IncAng           ! Wind incidence angle relative to facade normal (deg)
    REAL(r64)    :: IncRad           ! IncAng in radians
    INTEGER :: IAng                  ! Incidence angle index; used in interpolation
    REAL(r64)    :: DelAng           ! Incidence angle difference; used in interpolation
    REAL(r64)    :: WtAng            ! Incidence angle weighting factor; used in interpolation
    INTEGER :: ISR                   ! Side ratio index, for interpolation
    REAL(r64)    :: WtSR             ! Side ratio weighting factor; used in interpolation
    INTEGER :: SurfNum               ! Surface number
    INTEGER :: SurfDatNum            ! Surface data number
    REAL(r64)    :: SurfAng          ! Azimuth angle of surface normal (degrees clockwise from North)
    INTEGER :: FacadeNumThisSurf     ! Facade number for a particular surface
    REAL(r64)    :: AngDiff          ! Angle difference between wind and surface direction (deg)
    REAL(r64)    :: AngDiffMin       ! Minimum angle difference between wind and surface direction (deg)
    REAL(r64)    :: CPHighRiseWall(12,3)  ! Surface-averaged wind-pressure coefficient array for walls;
                                     !  index 1 is wind incidence angle (0,30,60,...,300,330 deg)
                                     !  index 2 is side ratio (0.25,1.0,4.0),
    REAL(r64)    :: CPHighRiseRoof(12,3)  ! Surface-averaged wind-pressure coefficient array for roof;
                                     !  index 1 is wind incidence angle (0,30,60,...,300,330 deg)
                                     !  index 2 is side ratio (0.25,0.5,1.0),
    CHARACTER(15) :: Name            ! External node name

    DATA CPHighRiseWall / 0.60d0, 0.54d0, 0.23d0,-0.25d0,-0.61d0,-0.55d0,-0.51d0,-0.55d0,-0.61d0,-0.25d0, 0.23d0, 0.54d0, &
                          0.60d0, 0.48d0, 0.04d0,-0.56d0,-0.56d0,-0.42d0,-0.37d0,-0.42d0,-0.56d0,-0.56d0, 0.04d0, 0.48d0, &
                          0.60d0, 0.44d0,-0.26d0,-0.70d0,-0.53d0,-0.32d0,-0.22d0,-0.32d0,-0.53d0,-0.70d0,-0.26d0, 0.44d0 /

    DATA CPHighRiseRoof /-0.28d0,-0.69d0,-0.72d0,-0.76d0,-0.72d0,-0.69d0,-0.28d0,-0.69d0,-0.72d0,-0.76d0,-0.72d0,-0.69d0, &
                         -0.47d0,-0.52d0,-0.70d0,-0.76d0,-0.70d0,-0.52d0,-0.47d0,-0.52d0,-0.70d0,-0.76d0,-0.70d0,-0.52d0, &
                         -0.70d0,-0.55d0,-0.55d0,-0.70d0,-0.55d0,-0.55d0,-0.70d0,-0.55d0,-0.55d0,-0.70d0,-0.55d0,-0.55d0 /

  ! Create five AirflowNetwork external node objects -- one for each of the four facades and one for the roof

  ALLOCATE(MultizoneExternalNodeData(AirflowNetworkNumOfExtSurfaces))
  AirflowNetworkNumOfExtNode = AirflowNetworkNumOfExtSurfaces
  NumOfExtNodes = AirflowNetworkNumOfExtSurfaces
  DO ExtNum = 1,NumOfExtNodes
    MultizoneExternalNodeData(ExtNum)%ExtNum  = AirflowNetworkNumOfZones+ExtNum
    Write(Name,'("ExtNode",I4)') ExtNum
    MultizoneExternalNodeData(ExtNum)%Name = ADJUSTL(Name)
  END DO

  ! Facade azimuth angle
  DO FacadeNum = 1,4
    FacadeAng(FacadeNum) = AirflowNetworkSimu%Azimuth + (FacadeNum-1)*90.0d0
    IF(FacadeAng(FacadeNum) .GE. 360.0d0) FacadeAng(FacadeNum) = FacadeAng(FacadeNum) - 360.0d0
  END DO

  FacadeAng(5) = AirflowNetworkSimu%Azimuth + 90.0d0

  ! Associate each SurfaceData with an external node

  ExtNum = 0
  DO SurfDatNum = 1,AirflowNetworkNumOfSurfaces
    SurfNum = MultizoneSurfaceData(SurfDatNum)%SurfNum
    IF (SurfNum == 0) CYCLE   ! Error caught earlier
    IF(Surface(SurfNum)%ExtBoundCond == ExternalEnvironment) THEN
      ExtNum = ExtNum + 1
      IF(Surface(SurfNum)%Tilt >= 45.0d0) THEN  ! "Vertical" surface
        SurfAng = Surface(SurfNum)%Azimuth
        FacadeNumThisSurf = 1
        AngDiffMin = ABS(SurfAng - FacadeAng(1))
        If (AngDiffMin .GT. 359.d0) AngDiffMin = ABS(AngDiffMin - 360.d0)
        DO FacadeNum = 2,4
          AngDiff = ABS(SurfAng-FacadeAng(FacadeNum))
          If (AngDiff .GT. 359.d0) AngDiff = ABS(AngDiff - 360.d0)
          IF(AngDiff < AngDiffMin) THEN
            AngDiffMin = AngDiff
            FacadeNumThisSurf = FacadeNum
          END IF
        END DO
        Write(Name,'("FacadeNum",I1)') FacadeNumThisSurf
        MultizoneExternalNodeData(ExtNum)%CPVNum = FacadeNumThisSurf
      ELSE                                    ! "Roof" surface
        Write(Name,'("FacadeNum",I1)') 5
        MultizoneExternalNodeData(ExtNum)%CPVNum = 5
      END IF
      MultizoneExternalNodeData(ExtNum)%WPCName = ADJUSTL(Name)
      MultizoneSurfaceData(SurfDatNum)%NodeNums(2) = MultizoneExternalNodeData(ExtNum)%ExtNum
      MultizoneSurfaceData(SurfDatNum)%ExternalNodeName = MultizoneExternalNodeData(ExtNum)%Name
    ELSE  ! Not an exterior surface
!       MultizoneSurfaceData(SurfDatNum)%ExternalNodeName = ' '
    END IF
  END DO

  ! Create the CP Array of wind directions

  ALLOCATE(MultizoneCPArrayData(1))
  AirflowNetworkNumOfCPArray = 1
  MultizoneCPArrayData(1)%Name='EVERY30DEGREES'
  AirflowNetworkSimu%CpArrayName = 'EVERY30DEGREESNAME'
  MultizoneCPArrayData(1)%NumWindDir = 12
  AirflowNetworkSimu%NWind = 12
  ALLOCATE(MultizoneCPArrayData(1)%WindDir(MultizoneCPArrayData(1)%NumWindDir))
  MultizoneCPArrayData(1)%WindDir = 0.0d0
  DO WindDirNum = 1,12
    MultizoneCPArrayData(1)%WindDir(WindDirNum) = (WindDirNum-1)*30.0d0
  END DO

  ! Calculate the wind pressure coefficients vs. wind direction for each external node

  ALLOCATE(MultizoneCpValueData(5))
  AirflowNetworkNumOfCPValue = 5
  DO FacadeNum = 1,5
    Write(Name,'("FacadeNum",I1)') FacadeNum
    MultizoneCpValueData(FacadeNum)%Name = ADJUSTL(Name)
    MultizoneCpValueData(FacadeNum)%CPArrayName = 'EVERY30DEGREES'
    ALLOCATE(MultizoneCpValueData(FacadeNum)%CpValue(MultizoneCPArrayData(1)%NumWindDir))
    MultizoneCpValueData(FacadeNum)%CpValue = 0.0d0
  END DO

  DO FacadeNum = 1,5
    IF(FacadeNum == 1 .OR. FacadeNum == 3 .OR. FacadeNum == 5) THEN
      SideRatio = AirflowNetworkSimu%AspectRatio
    ELSE  ! FacadeNum = 2 or 4
      SideRatio = 1.0/AirflowNetworkSimu%AspectRatio
    END IF
    If (SameString(AirflowNetworkSimu%BldgType,'HighRise') .AND. FacadeNum .NE. 5) SideRatio = 1.d0/SideRatio
    SideRatioFac = LOG(SideRatio)
    DO WindDirNum = 1,12
      WindAng = (WindDirNum-1)*30.0d0
      IncAng = ABS(WindAng - FacadeAng(FacadeNum))
      IF(IncAng > 180.0d0) IncAng = 360.0d0 - IncAng
      IAng   = INT(IncAng/30.0d0) + 1
      DelAng = MOD(IncAng,30.0d0)
      WtAng  = 1.0d0 - DelAng/30.0d0

      ! Wind-pressure coefficients for vertical facades, low-rise building

      IF(SameString(AirflowNetworkSimu%BldgType,'LowRise') .AND. FacadeNum <= 4) THEN
        IncRad = IncAng*DegToRadians
        MultizoneCpValueData(FacadeNum)%CpValue(WindDirNum) = 0.6d0 * LOG(  &
          1.248d0 - 0.703d0*SIN(IncRad/2.d0) - 1.175d0*SIN(IncRad)**2 + 0.131d0*SIN(2.d0*IncRad*SideRatioFac)**3 +  &
          0.769d0*COS(IncRad/2.d0) + 0.07d0*(SideRatioFac*SIN(IncRad/2.d0))**2 + 0.717d0*COS(IncRad/2.d0)**2 )
      END IF

      ! Wind-pressure coefficients for vertical facades, high-rise building

      IF(SameString(AirflowNetworkSimu%BldgType,'HighRise') .AND. FacadeNum <=4) THEN
        SR     = MIN(MAX(SideRatio,0.25d0),4.0d0)
        IF(SR >= 0.25d0 .AND. SR < 1.0d0) THEN
          ISR = 1
          WtSR = (1.0d0 - SR)/0.75d0
        ELSE  ! 1.0 <= SR <= 4.0
          ISR = 2
          WtSR = (4.0d0 - SR)/3.0d0
        END IF
        MultizoneCpValueData(FacadeNum)%CpValue(WindDirNum) = &
          WtSR     * (WtAng*CPHighRiseWall(IAng,ISR)   + (1.0d0-WtAng)*CPHighRiseWall(IAng+1,ISR)) +  &
          (1.0d0-WtSR)* (WtAng*CPHighRiseWall(IAng,ISR+1) + (1.0d0-WtAng)*CPHighRiseWall(IAng+1,ISR+1))
      END IF

      ! Wind-pressure coefficients for roof (assumed same for low-rise and high-rise buildings)

      IF( (SameString(AirflowNetworkSimu%BldgType,'HighRise') .OR. SameString(AirflowNetworkSimu%BldgType,'LowRise')) &
         .AND. FacadeNum == 5) THEN
        SR = MIN(MAX(SideRatio,0.25d0),1.0d0)
        IF(SR >= 0.25d0 .AND. SR < 0.5d0) THEN
          ISR = 1
          WtSR = (0.5d0 - SR)/0.25d0
        ELSE  ! 0.5 <= SR <= 1.0
          ISR = 2
          WtSR = (1.0d0 - SR)/0.5d0
        END IF
        MultizoneCpValueData(FacadeNum)%CpValue(WindDirNum) = &
          WtSR     * (WtAng*CPHighRiseRoof(IAng,ISR)   + (1.0d0-WtAng)*CPHighRiseRoof(IAng+1,ISR)) +  &
          (1.0d0-WtSR)* (WtAng*CPHighRiseRoof(IAng,ISR+1) + (1.0d0-WtAng)*CPHighRiseRoof(IAng+1,ISR+1))
      END IF

    END DO  ! End of wind direction loop
  END DO  ! End of facade number loop

END SUBROUTINE CalcWindPressureCoeffs

REAL(r64) FUNCTION CalcWindPressure(CPVNum,Vref,Height)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Oct. 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates surface wind pressure based on given CP values

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          ! COMIS Fundamentals

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
      INTEGER, intent(in) :: CPVNum   ! CP Value number
      REAL(r64), intent(in)    :: Vref     ! Velocity at reference height
      REAL(r64), intent(in)    :: Height   ! Node height for outdoor temperature calculation
! Output is Wind Pressure [Pa]
          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
      INTEGER i,NWind
      REAL(r64) RhoOut ! Outdoor air density
      REAL(r64) CPV    ! CP value at given wind direction
      LOGICAL FoundCPV

!     CODE  ************************************************************
      ! Calculate outdoor density
      RhoOut = PsyRhoAirFnPbTdbW(OutBaroPress,OutDryBulbTempAt(Height),OutHumRat)

      NWind = AirflowNetworkSimu%NWind
      ! Calculate dynamic pressure
      FoundCPV = .FALSE.
      Do i=2,NWind
         If (MultizoneCPArrayData(1)%WindDir(i).GE.WindDir) then
            CPV = MultizoneCPValueData(CPVNum)%CPValue(i-1)+(WindDir-MultizoneCPArrayData(1)%WindDir(i-1)) &
                  *(MultizoneCPValueData(CPVNum)%CPValue(i)-MultizoneCPValueData(CPVNum)%CPValue(i-1)) &
                  /(MultizoneCPArrayData(1)%WindDir(i)-MultizoneCPArrayData(1)%WindDir(i-1))
            FoundCPV = .TRUE.
            Exit
         end if
      end do
      if (.NOT. FoundCPV) then
         CPV = MultizoneCPValueData(CPVNum)%CPValue(NWind)+(WindDir-MultizoneCPArrayData(1)%WindDir(NWind)) &
               *(MultizoneCPValueData(CPVNum)%CPValue(1)-MultizoneCPValueData(CPVNum)%CPValue(NWind)) &
               /(MultizoneCPArrayData(1)%WindDir(1)-MultizoneCPArrayData(1)%WindDir(NWind)+360)
      end if
      CalcWindPressure = CPV*0.5d0*RhoOut*Vref*Vref

      RETURN
END FUNCTION CalcWindPressure

SUBROUTINE CalcAirflowNetworkHeatBalance
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Oct. 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  Revised based on Subroutine CalcADSHeatBalance


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine performs AirflowNetwork thermal simulations.


          ! METHODOLOGY EMPLOYED:
          ! na


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
          ! na


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na


          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank=' '


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER i,j
INTEGER LF,LT,CompNum,NF,NT
INTEGER CompTypeNum,TypeNum
Character(len=MaxNameLength) CompName
REAL(r64) Ei,DirSign,Tamb
REAL(r64) CpAir, TZON, load
INTEGER ZoneNum
LOGICAL found,OANode


  MA = 0.0d0
  MV = 0.0d0
  DO I=1,AirflowNetworkNumOfLinks
     CompNum = AirflowNetworkLinkageData(i)%CompNum
     CompTypeNum = AirflowNetworkCompData(CompNum)%CompTypeNum
     CompName = AirflowNetworkCompData(CompNum)%EPlusName
     CpAir = PsyCpAirFnWTdb((AirflowNetworkNodeSimu(AirflowNetworkLinkageData(i)%NodeNums(1))%WZ+ &
                             AirflowNetworkNodeSimu(AirflowNetworkLinkageData(i)%NodeNums(2))%WZ)/2.0d0, &
                            (AirflowNetworkNodeSimu(AirflowNetworkLinkageData(i)%NodeNums(1))%TZ+ &
                             AirflowNetworkNodeSimu(AirflowNetworkLinkageData(i)%NodeNums(2))%TZ)/2.0d0)
     ! Calculate duct conduction loss
     if (CompTypeNum == CompTypeNum_DWC .AND. CompName == Blank) then ! Duct element only
        TypeNum = AirflowNetworkCompData(CompNum)%TypeNum
        IF (AirflowNetworkLinkSimu(I)%FLOW .GT. 0) then ! flow direction is tha same as input from node 1 to node 2
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
           DirSign = 1.0d0
        else ! flow direction is tha opposite as input from node 2 to node 1
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
           DirSign = -1.0d0
        end if
        ! Fatal error when return flow is opposite to the desired direction
        If (AirflowNetworkLinkSimu(I)%FLOW .EQ. 0.0d0 .AND. AirflowNetworkLinkSimu(I)%FLOW2 > 0.0d0) Then
          CALL ShowSevereError('AirflowNetwork: The airflow direction is opposite to the intended direction ' &
           //'(from node 1 to node 2) in AirflowNetwork:Distribution:Linkage = '//Trim(AirflowNetworkLinkageData(i)%Name))
          CALL ShowContinueErrorTimeStamp(' ')
          CALL ShowContinueError('The sum of the airflows entering the zone is greater than the airflows leaving the zone '// &
               '(e.g., wind and stack effect).')
          CALL ShowContinueError('Please check wind speed or reduce values of "Window/Door Opening Factor, '// &
             'or Crack Factor" defined in AirflowNetwork:MultiZone:Surface objects.')
          CALL ShowFatalError('AirflowNetwork: The previous error causes termination.')
        End If

        Ei = exp(-DisSysCompDuctData(TypeNum)%UThermal*DisSysCompDuctData(TypeNum)%L*DisSysCompDuctData(TypeNum)%D*pi/ &
             (DirSign*AirflowNetworkLinkSimu(I)%FLOW*CpAir))
        if (AirflowNetworkLinkageData(i)%ZoneNum < 0) then
           Tamb = OutDryBulbTempAt(AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%NodeHeight)
        else if (AirflowNetworkLinkageData(i)%ZoneNum == 0) then
           Tamb = AirflowNetworkNodeSimu(LT)%TZ
        else
           Tamb = ANZT(AirflowNetworkLinkageData(i)%ZoneNum)
        end if
        MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+ &
                                                 Abs(AirflowNetworkLinkSimu(I)%FLOW)*CpAir
        MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW)*CpAir*Ei
        MV(LT) = MV(LT)+abs(AirflowNetworkLinkSimu(I)%FLOW)*Tamb*(1.0d0-Ei)*CpAir
     end if
     if (CompTypeNum == CompTypeNum_TMU) then ! Reheat unit: SINGLE DUCT:CONST VOLUME:REHEAT
        TypeNum = AirflowNetworkCompData(CompNum)%TypeNum
        IF (AirflowNetworkLinkSimu(I)%FLOW .GT. 0.d0) then ! flow direction is tha same as input from node 1 to node 2
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
           DirSign = 1.0d0
        else ! flow direction is tha opposite as input from node 2 to node 1
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
           DirSign = -1.0d0
        end if
        Ei = exp(-0.001d0*DisSysCompTermUnitData(TypeNum)%L*DisSysCompTermUnitData(TypeNum)%D*pi/ &
             (DirSign*AirflowNetworkLinkSimu(I)%FLOW*CpAir))
        Tamb = AirflowNetworkNodeSimu(LT)%TZ
        MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+ &
                                                 Abs(AirflowNetworkLinkSimu(I)%FLOW)*CpAir
        MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW)*CpAir*Ei
        MV(LT) = MV(LT)+abs(AirflowNetworkLinkSimu(I)%FLOW)*Tamb*(1.0d0-Ei)*CpAir
     end if
     if (CompTypeNum == CompTypeNum_COI) then ! heating or cooling coil
        TypeNum = AirflowNetworkCompData(CompNum)%TypeNum
        IF (AirflowNetworkLinkSimu(I)%FLOW .GT. 0.d0) then ! flow direction is tha same as input from node 1 to node 2
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
           DirSign = 1.0d0
        else ! flow direction is tha opposite as input from node 2 to node 1
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
           DirSign = -1.0d0
        end if
!        Ei = exp(-0.001*DisSysCompCoilData(TypeNum)%L*DisSysCompCoilData(TypeNum)%D*pi/ &
!             (DirSign*AirflowNetworkLinkSimu(I)%FLOW*CpAir))
!        Tamb = AirflowNetworkNodeSimu(LT)%TZ
!        MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+ &
!                                                 Abs(AirflowNetworkLinkSimu(I)%FLOW)*CpAir
!        MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW)*CpAir*Ei
!        MV(LT) = MV(LT)+abs(AirflowNetworkLinkSimu(I)%FLOW)*Tamb*(1.0-Ei)*CpAir
     end if
     ! Calculate temp in a constant pressure drop element
     if (CompTypeNum == CompTypeNum_CPD .AND. CompName == Blank) then ! constant pressure element only
        IF (AirflowNetworkLinkSimu(I)%FLOW .GT. 0) then ! flow direction is tha same as input from node 1 to node 2
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
        else ! flow direction is tha opposite as input from node 2 to node 1
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
        end if
        MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+ &
                                                 Abs(AirflowNetworkLinkSimu(I)%FLOW)*CpAir
        MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW)*CpAir
        MV(LT) = 0.0d0
     end if
     ! Calculate return leak
     if ((CompTypeNum == CompTypeNum_PLR .OR. CompTypeNum == CompTypeNum_ELR) .AND. CompName == Blank) then
        ! Return leak element only
        if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusZoneNum > 0) .AND. &
            (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusZoneNum == 0) .AND. &
            (AirflowNetworkLinkSimu(I)%FLOW .GT. 0.0d0)) then
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
           MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+Abs(AirflowNetworkLinkSimu(I)%FLOW)*CpAir
           MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW)*CpAir
        end if
        if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%ExtNodeNum > 0) .AND. &
            (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusZoneNum == 0) .AND. &
            (AirflowNetworkLinkSimu(I)%FLOW .GT. 0.0d0)) then
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
           MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+  &
                                                      Abs(AirflowNetworkLinkSimu(I)%FLOW)*CpAir
           MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW)*CpAir
        end if
        if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusZoneNum > 0) .AND. &
            (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusZoneNum == 0) .AND. &
            (AirflowNetworkLinkSimu(I)%FLOW2 .GT. 0.0d0)) then
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
           MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+  &
                                                      Abs(AirflowNetworkLinkSimu(I)%FLOW2)*CpAir
           MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW2)*CpAir
        end if
        if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%ExtNodeNum > 0) .AND. &
            (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusZoneNum == 0) .AND. &
            (AirflowNetworkLinkSimu(I)%FLOW2 .GT. 0.0d0)) then
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
           MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+  &
                                                      Abs(AirflowNetworkLinkSimu(I)%FLOW2)*CpAir
           MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW2)*CpAir
        end if
     end if
     ! Check reheat unit or coil
     if (AirflowNetworkCompData(CompNum)%EPlusTypeNum .EQ. EPlusTypeNum_RHT .AND. &
        (.NOT. AirflowNetworkLinkageData(i)%VAVTermDamper)) then
        NF = 0
        NT = 0
        if (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusNodeNum > 0) then
           NF = AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusNodeNum
        end if
        if (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusNodeNum > 0) then
           NT = AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusNodeNum
        end if
        if ((NF .EQ. 0) .OR. (NT .EQ. 0)) then
           CALL ShowFatalError('Node number in the primary air loop is not found in AIRFLOWNETWORK:DISTRIBUTION:NODE = ' &
                               //AirflowNetworkLinkageData(i)%Name)
        end if
        if (AirflowNetworkLinkSimu(I)%FLOW .GT. 0.0d0) then
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
           load = Node(NT)%Temp-Node(NF)%Temp
        else
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
           load = Node(NF)%Temp-Node(NT)%Temp
        end if
        CpAir = PsyCpAirFnWTdb(Node(NT)%HumRat,Node(NT)%Temp)
        MV(LT) = MV(LT)+AirflowNetworkLinkSimu(I)%FLOW*CpAir*load
     end if
  END DO

  ! Prescribe temperature for EPlus nodes
  DO I=1,AirflowNetworkNumOfNodes
     found = .FALSE.
     OANode = .FALSE.
     DO J=1,AirflowNetworkNumOfLinks
        if (AirflowNetworkLinkageData(J)%NodeNums(1) == I .OR. AirflowNetworkLinkageData(J)%NodeNums(2) == I) then
           CompNum = AirflowNetworkLinkageData(j)%CompNum
           if (AirflowNetworkCompData(CompNum)%EPlusTypeNum .EQ. EPlusTypeNum_RHT .AND. &
              (.NOT. AirflowNetworkLinkageData(J)%VAVTermDamper)) then
              found = .TRUE.
              Exit
           end if
           ! Overwrite fan outlet node
           if (AirflowNetworkCompData(CompNum)%EPlusTypeNum .EQ. EPlusTypeNum_FAN .AND. &
               AirflowNetworkLinkageData(J)%NodeNums(2) == I) then
              found = .FALSE.
              Exit
           end if
           ! Overwrite return connection outlet
           if (AirflowNetworkLinkageData(j)%ConnectionFlag .EQ. EPlusTypeNum_RCN ) then ! Modified on 9/2/09
              found = .TRUE.
              Exit
           end if
           if (AirflowNetworkLinkageData(j)%ConnectionFlag .EQ. EPlusTypeNum_SCN .AND. & ! Modified on 9/2/09
               AirflowNetworkLinkageData(J)%NodeNums(2) == I) then
              found = .TRUE.
              Exit
           end if
        end if
        if (AirflowNetworkLinkageData(J)%NodeNums(2) == I .AND. &
           AirflowNetworkNodeData(AirflowNetworkLinkageData(J)%NodeNums(1))%EPlusTypeNum == EPlusTypeNum_OAN) then
           OANode = .TRUE.
           Exit
        End if
     END DO
     if (found) cycle
     if (AirflowNetworkNodeData(I)%EPlusZoneNum .eq. 0 .AND. &
         AirflowNetworkNodeData(I)%EPlusTypeNum .EQ. EPlusTypeNum_ZIN) cycle
     J = AirflowNetworkNodeData(I)%EPlusNodeNum

     if (J > 0 .AND. (AirflowNetworkNodeData(I)%EPlusZoneNum > 0 .OR. &
                      AirflowNetworkNodeData(I)%EPlusTypeNum == EPlusTypeNum_FOU .OR. &
                      AirflowNetworkNodeData(I)%EPlusTypeNum == EPlusTypeNum_COU .OR. &
                      AirflowNetworkNodeData(I)%EPlusTypeNum == EPlusTypeNum_HXO)) then
         MA((I-1)*AirflowNetworkNumOfNodes+I) = 1.0d10
         MV(I) = Node(j)%Temp*1.0d10
     end if
     if (J > 0 .AND. OANode) then
         MA((I-1)*AirflowNetworkNumOfNodes+I) = 1.0d10
         MV(I) = Node(j)%Temp*1.0d10
     end if
     if (AirflowNetworkNodeData(I)%EPlusZoneNum > 0 .AND. &
        MA((I-1)*AirflowNetworkNumOfNodes+I) .LT. 0.9d10 ) then
        ZoneNum = AirflowNetworkNodeData(I)%EPlusZoneNum
        MA((I-1)*AirflowNetworkNumOfNodes+I) = 1.0d10
        MV(I) = ANZT(ZoneNum)*1.0d10
     end if
     if (AirflowNetworkNodeData(I)%ExtNodeNum > 0 .AND. &
        MA((I-1)*AirflowNetworkNumOfNodes+I) .LT. 0.9d10 ) then
        MA((I-1)*AirflowNetworkNumOfNodes+I) = 1.0d10
        MV(I) = OutDryBulbTempAt(AirflowNetworkNodeData(I)%NodeHeight)*1.0d10
     end if
  END DO

  ! Check singularity
  DO I=1,AirflowNetworkNumOfNodes
     if (MA((I-1)*AirflowNetworkNumOfNodes+I) .LT. 1.0d-6) then
        CALL ShowFatalError('CalcAirflowNetworkHeatBalance: A diagonal entity is zero in AirflowNetwork matrix at node '//  &
                              TRIM(AirflowNetworkNodeData(I)%Name))
     end if
  END DO


  ! Get an inverse matrix
  Call MRXINV(AirflowNetworkNumOfNodes)


  ! Calculate node temperatures
  DO I=1,AirflowNetworkNumOfNodes
     TZON = 0.0d0
     DO J=1, AirflowNetworkNumOfNodes
        TZON = TZON +MA((I-1)*AirflowNetworkNumOfNodes+J)*MV(J)
     END DO
     AirflowNetworkNodeSimu(I)%TZ = TZON
  END DO


RETURN
END SUBROUTINE CalcAirflowNetworkHeatBalance

SUBROUTINE CalcAirflowNetworkMoisBalance
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Oct. 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  Revised based on Subroutine CalcADSMoistureBalance


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine performs AirflowNetwork mositure simulations.


          ! METHODOLOGY EMPLOYED:
          ! na


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
          ! na


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na


          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank=' '


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER i,j
INTEGER LF,LT,CompNum,NF,NT
INTEGER CompTypeNum,TypeNum
Character(len=MaxNameLength) CompName
REAL(r64) Ei,DirSign,Wamb
REAL(r64) WZON, load
INTEGER ZoneNum
LOGICAL found,OANode


  MA = 0.0d0
  MV = 0.0d0
  DO I=1,AirflowNetworkNumOfLinks
     CompNum = AirflowNetworkLinkageData(i)%CompNum
     CompTypeNum = AirflowNetworkCompData(CompNum)%CompTypeNum
     CompName = AirflowNetworkCompData(CompNum)%EPlusName
     ! Calculate duct moisture diffusion loss
     if (CompTypeNum == CompTypeNum_DWC .AND. CompName == Blank) then ! Duct component only
        TypeNum = AirflowNetworkCompData(CompNum)%TypeNum
        IF (AirflowNetworkLinkSimu(I)%FLOW .GT. 0.d0) then ! flow direction is tha same as input from node 1 to node 2
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
           DirSign = 1.0d0
        else ! flow direction is tha opposite as input from node 2 to node 1
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
           DirSign = -1.0d0
        end if
        Ei = exp(-DisSysCompDuctData(TypeNum)%UMoisture*DisSysCompDuctData(TypeNum)%L*DisSysCompDuctData(TypeNum)%D*pi/ &
             (DirSign*AirflowNetworkLinkSimu(I)%FLOW))
        if (AirflowNetworkLinkageData(i)%ZoneNum < 0) then
           Wamb = OutHumRat
        else if (AirflowNetworkLinkageData(i)%ZoneNum == 0) then
           Wamb = AirflowNetworkNodeSimu(LT)%WZ
        else
           Wamb = ANZW(AirflowNetworkLinkageData(i)%ZoneNum)
        end if
        MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+ &
                                                 Abs(AirflowNetworkLinkSimu(I)%FLOW)
        MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW)*Ei
        MV(LT) = MV(LT)+abs(AirflowNetworkLinkSimu(I)%FLOW)*Wamb*(1.0d0-Ei)
     end if
     if (CompTypeNum == CompTypeNum_TMU) then ! Reheat unit: SINGLE DUCT:CONST VOLUME:REHEAT
        TypeNum = AirflowNetworkCompData(CompNum)%TypeNum
        IF (AirflowNetworkLinkSimu(I)%FLOW .GT. 0) then ! flow direction is tha same as input from node 1 to node 2
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
           DirSign = 1.0d0
        else ! flow direction is tha opposite as input from node 2 to node 1
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
           DirSign = -1.0d0
        end if
        Ei = exp(-0.0001d0*DisSysCompTermUnitData(TypeNum)%L*DisSysCompTermUnitData(TypeNum)%D*PI/ &
             (DirSign*AirflowNetworkLinkSimu(I)%FLOW))
        Wamb = AirflowNetworkNodeSimu(LT)%WZ
        MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+ &
                                                 Abs(AirflowNetworkLinkSimu(I)%FLOW)
        MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW)*Ei
        MV(LT) = MV(LT)+abs(AirflowNetworkLinkSimu(I)%FLOW)*Wamb*(1.0d0-Ei)
     end if
     if (CompTypeNum == CompTypeNum_COI) then ! heating or cooling coil
        TypeNum = AirflowNetworkCompData(CompNum)%TypeNum
        IF (AirflowNetworkLinkSimu(I)%FLOW .GT. 0) then ! flow direction is tha same as input from node 1 to node 2
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
           DirSign = 1.0d0
        else ! flow direction is tha opposite as input from node 2 to node 1
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
           DirSign = -1.0d0
        end if
!        Ei = exp(-0.0001*DisSysCompCoilData(TypeNum)%L*DisSysCompCoilData(TypeNum)%D*pi/ &
!             (DirSign*AirflowNetworkLinkSimu(I)%FLOW))
!        Wamb = AirflowNetworkNodeSimu(LT)%WZ
!        MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+ &
!                                                 Abs(AirflowNetworkLinkSimu(I)%FLOW)
!        MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW)*Ei
!        MV(LT) = MV(LT)+abs(AirflowNetworkLinkSimu(I)%FLOW)*Wamb*(1.0-Ei)
     end if
     ! Calculate temp in a constant pressure drop component
     if (CompTypeNum == CompTypeNum_CPD .AND. CompName == Blank) then ! constant pressure element only
        IF (AirflowNetworkLinkSimu(I)%FLOW .GT. 0) then ! flow direction is tha same as input from node 1 to node 2
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
        else ! flow direction is tha opposite as input from node 2 to node 1
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
        end if
        MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+ &
                                                 Abs(AirflowNetworkLinkSimu(I)%FLOW)
        MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW)
        MV(LT) = 0.0d0
     end if
     ! Calculate return leak
     if ((CompTypeNum == CompTypeNum_PLR .OR. CompTypeNum == CompTypeNum_ELR) .AND. CompName == Blank) then
        ! Return leak component only
        if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusZoneNum > 0) .AND. &
            (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusZoneNum == 0) .AND. &
            (AirflowNetworkLinkSimu(I)%FLOW .GT. 0.0d0)) then
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
           MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+Abs(AirflowNetworkLinkSimu(I)%FLOW)
           MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW)
        end if
        if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%ExtNodeNum > 0) .AND. &
            (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusZoneNum == 0) .AND. &
            (AirflowNetworkLinkSimu(I)%FLOW .GT. 0.0d0)) then
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
           MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+Abs(AirflowNetworkLinkSimu(I)%FLOW)
           MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW)
        end if
        if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusZoneNum > 0) .AND. &
            (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusZoneNum == 0) .AND. &
            (AirflowNetworkLinkSimu(I)%FLOW2 .GT. 0.0d0)) then
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
           MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+Abs(AirflowNetworkLinkSimu(I)%FLOW2)
           MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW2)
        end if
        if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%ExtNodeNum > 0) .AND. &
            (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusZoneNum == 0) .AND. &
            (AirflowNetworkLinkSimu(I)%FLOW2 .GT. 0.0d0)) then
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
           MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+Abs(AirflowNetworkLinkSimu(I)%FLOW2)
           MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW2)
        end if
     end if
     ! Check reheat unit
     if (AirflowNetworkCompData(CompNum)%EPlusTypeNum .EQ. EPlusTypeNum_RHT .AND. &
        (.NOT. AirflowNetworkLinkageData(i)%VAVTermDamper)) then
        NF = 0
        NT = 0
        if (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusNodeNum > 0) then
           NF = AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusNodeNum
        end if
        if (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusNodeNum > 0) then
           NT = AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusNodeNum
        end if
        if ((NF .EQ. 0) .OR. (NT .EQ. 0)) then
           CALL ShowFatalError('Node number in the primary air loop is not found in AIRFLOWNETWORK:DISTRIBUTION:NODE = ' &
                               //AirflowNetworkLinkageData(i)%Name)
        end if
        if (AirflowNetworkLinkSimu(I)%FLOW .GT. 0.0d0) then
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
           load = Node(NT)%HumRat-Node(NF)%HumRat
        else
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
           load = Node(NF)%HumRat-Node(NT)%HumRat
        end if
        MV(LT) = MV(LT)+AirflowNetworkLinkSimu(I)%FLOW*load
     end if
  END DO

  ! Prescribe temperature for EPlus nodes
  DO I=1,AirflowNetworkNumOfNodes
     found = .FALSE.
     OANode = .FALSE.
     DO J=1,AirflowNetworkNumOfLinks
        if (AirflowNetworkLinkageData(J)%NodeNums(1) == I .OR. AirflowNetworkLinkageData(J)%NodeNums(2) == I) then
           CompNum = AirflowNetworkLinkageData(j)%CompNum
           if (AirflowNetworkCompData(CompNum)%EPlusTypeNum .EQ. EPlusTypeNum_RHT .AND. &
              (.NOT. AirflowNetworkLinkageData(J)%VAVTermDamper)) then
              found = .TRUE.
              Exit
           end if
           ! Overwrite fan outlet node
           if (AirflowNetworkCompData(CompNum)%EPlusTypeNum .EQ. EPlusTypeNum_FAN .AND. &
               AirflowNetworkLinkageData(J)%NodeNums(2) == I) then
              found = .FALSE.
              Exit
           end if
           ! Overwrite return connection outlet
           if (AirflowNetworkLinkageData(j)%ConnectionFlag .EQ. EPlusTypeNum_RCN ) then ! Modified on 9/2/09
              found = .TRUE.
              Exit
           end if
           if (AirflowNetworkLinkageData(j)%ConnectionFlag .EQ. EPlusTypeNum_SCN .AND. & ! Modified on 9/2/09
               AirflowNetworkLinkageData(J)%NodeNums(2) == I) then
              found = .TRUE.
              Exit
           end if
        end if
        if (AirflowNetworkLinkageData(J)%NodeNums(2) == I .AND. &
           AirflowNetworkNodeData(AirflowNetworkLinkageData(J)%NodeNums(1))%EPlusTypeNum == EPlusTypeNum_OAN) then
           OANode = .TRUE.
           Exit
        End if
     END DO
     if (found) cycle
     if (AirflowNetworkNodeData(I)%EPlusZoneNum .eq. 0 .AND. &
         AirflowNetworkNodeData(I)%EPlusTypeNum .EQ. EPlusTypeNum_ZIN) cycle
     J = AirflowNetworkNodeData(I)%EPlusNodeNum
     if (J > 0 .AND. (AirflowNetworkNodeData(I)%EPlusZoneNum > 0 .OR. &
                      AirflowNetworkNodeData(I)%EPlusTypeNum == EPlusTypeNum_FOU .OR. &
                      AirflowNetworkNodeData(I)%EPlusTypeNum == EPlusTypeNum_COU .OR. &
                      AirflowNetworkNodeData(I)%EPlusTypeNum == EPlusTypeNum_HXO)) then
         MA((I-1)*AirflowNetworkNumOfNodes+I) = 1.0d10
         MV(I) = Node(j)%HumRat*1.0d10
     end if
     if (J > 0 .AND. OANode) then
         MA((I-1)*AirflowNetworkNumOfNodes+I) = 1.0d10
         MV(I) = Node(j)%HumRat*1.0d10
     end if
     if (AirflowNetworkNodeData(I)%EPlusZoneNum > 0 .AND. &
        MA((I-1)*AirflowNetworkNumOfNodes+I) .LT. 0.9d10 ) then
        ZoneNum = AirflowNetworkNodeData(I)%EPlusZoneNum
        MA((I-1)*AirflowNetworkNumOfNodes+I) = 1.0d10
        MV(I) = ANZW(ZoneNum)*1.0d10
     end if
     if (AirflowNetworkNodeData(I)%ExtNodeNum > 0 ) then
        MA((I-1)*AirflowNetworkNumOfNodes+I) = 1.0d10
        MV(I) = OutHumRat*1.0d10
     end if
  END DO

  ! Check singularity
  DO I=1,AirflowNetworkNumOfNodes
     if (MA((I-1)*AirflowNetworkNumOfNodes+I) .LT. 1.0d-6) then
        CALL ShowFatalError('CalcAirflowNetworkMoisBalance: A diagonal entity is zero in AirflowNetwork matrix at node '//  &
                              TRIM(AirflowNetworkNodeData(I)%Name))
     end if
  END DO

  ! Get an inverse matrix
  Call MRXINV(AirflowNetworkNumOfNodes)

  ! Calculate node temperatures
  DO I=1,AirflowNetworkNumOfNodes
     WZON = 0.0d0
     DO J=1, AirflowNetworkNumOfNodes
        WZON = WZON +MA((I-1)*AirflowNetworkNumOfNodes+J)*MV(J)
     END DO
     AirflowNetworkNodeSimu(I)%WZ = WZON
  END DO


RETURN
END SUBROUTINE CalcAirflowNetworkMoisBalance

SUBROUTINE CalcAirflowNetworkCO2Balance
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   June. 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  Revised based on Subroutine CalcAirflowNetworkMoisBalance


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine performs AirflowNetwork CO2 simulations.


          ! METHODOLOGY EMPLOYED:
          ! na


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
          ! na


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na


          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank=' '


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER i,j
INTEGER LF,LT,CompNum,NF,NT
INTEGER CompTypeNum,TypeNum
Character(len=MaxNameLength) CompName
REAL(r64) DirSign
REAL(r64) COZN
INTEGER ZoneNum
LOGICAL found,OANode


  MA = 0.0d0
  MV = 0.0d0
  DO I=1,AirflowNetworkNumOfLinks
     CompNum = AirflowNetworkLinkageData(i)%CompNum
     CompTypeNum = AirflowNetworkCompData(CompNum)%CompTypeNum
     CompName = AirflowNetworkCompData(CompNum)%EPlusName
     ! Calculate duct moisture diffusion loss
     if (CompTypeNum == CompTypeNum_DWC .AND. CompName == Blank) then ! Duct component only
        TypeNum = AirflowNetworkCompData(CompNum)%TypeNum
        IF (AirflowNetworkLinkSimu(I)%FLOW .GT. 0.d0) then ! flow direction is tha same as input from node 1 to node 2
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
           DirSign = 1.0d0
        else ! flow direction is tha opposite as input from node 2 to node 1
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
           DirSign = -1.0d0
        end if
        MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+ &
                                                 Abs(AirflowNetworkLinkSimu(I)%FLOW)
        MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW)
     end if
     if (CompTypeNum == CompTypeNum_TMU) then ! Reheat unit: SINGLE DUCT:CONST VOLUME:REHEAT
        TypeNum = AirflowNetworkCompData(CompNum)%TypeNum
        IF (AirflowNetworkLinkSimu(I)%FLOW .GT. 0) then ! flow direction is tha same as input from node 1 to node 2
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
           DirSign = 1.0d0
        else ! flow direction is tha opposite as input from node 2 to node 1
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
           DirSign = -1.0d0
        end if
        MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+ &
                                                 Abs(AirflowNetworkLinkSimu(I)%FLOW)
        MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW)
     end if
     if (CompTypeNum == CompTypeNum_COI) then ! heating or cooling coil
        TypeNum = AirflowNetworkCompData(CompNum)%TypeNum
        IF (AirflowNetworkLinkSimu(I)%FLOW .GT. 0) then ! flow direction is tha same as input from node 1 to node 2
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
           DirSign = 1.0d0
        else ! flow direction is tha opposite as input from node 2 to node 1
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
           DirSign = -1.0d0
        end if
     end if
     ! Calculate temp in a constant pressure drop component
     if (CompTypeNum == CompTypeNum_CPD .AND. CompName == Blank) then ! constant pressure element only
        IF (AirflowNetworkLinkSimu(I)%FLOW .GT. 0) then ! flow direction is tha same as input from node 1 to node 2
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
        else ! flow direction is tha opposite as input from node 2 to node 1
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
        end if
        MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+ &
                                                 Abs(AirflowNetworkLinkSimu(I)%FLOW)
        MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW)
        MV(LT) = 0.0d0
     end if
     ! Calculate return leak
     if ((CompTypeNum == CompTypeNum_PLR .OR. CompTypeNum == CompTypeNum_ELR) .AND. CompName == Blank) then
        ! Return leak component only
        if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusZoneNum > 0) .AND. &
            (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusZoneNum == 0) .AND. &
            (AirflowNetworkLinkSimu(I)%FLOW .GT. 0.0d0)) then
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
           MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+Abs(AirflowNetworkLinkSimu(I)%FLOW)
           MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW)
        end if
        if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%ExtNodeNum > 0) .AND. &
            (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusZoneNum == 0) .AND. &
            (AirflowNetworkLinkSimu(I)%FLOW .GT. 0.0d0)) then
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
           MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+Abs(AirflowNetworkLinkSimu(I)%FLOW)
           MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW)
        end if
        if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusZoneNum > 0) .AND. &
            (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusZoneNum == 0) .AND. &
            (AirflowNetworkLinkSimu(I)%FLOW2 .GT. 0.0d0)) then
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
           MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+Abs(AirflowNetworkLinkSimu(I)%FLOW2)
           MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW2)
        end if
        if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%ExtNodeNum > 0) .AND. &
            (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusZoneNum == 0) .AND. &
            (AirflowNetworkLinkSimu(I)%FLOW2 .GT. 0.0d0)) then
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
           MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+Abs(AirflowNetworkLinkSimu(I)%FLOW2)
           MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW2)
        end if
     end if
  END DO

  ! Prescribe temperature for EPlus nodes
  DO I=1,AirflowNetworkNumOfNodes
     found = .FALSE.
     OANode = .FALSE.
     DO J=1,AirflowNetworkNumOfLinks
        if (AirflowNetworkLinkageData(J)%NodeNums(1) == I .OR. AirflowNetworkLinkageData(J)%NodeNums(2) == I) then
           CompNum = AirflowNetworkLinkageData(j)%CompNum
           if (AirflowNetworkCompData(CompNum)%EPlusTypeNum .EQ. EPlusTypeNum_RHT .AND. &
              (.NOT. AirflowNetworkLinkageData(J)%VAVTermDamper)) then
              found = .TRUE.
              Exit
           end if
           ! Overwrite fan outlet node
           if (AirflowNetworkCompData(CompNum)%EPlusTypeNum .EQ. EPlusTypeNum_FAN .AND. &
               AirflowNetworkLinkageData(J)%NodeNums(2) == I) then
              found = .FALSE.
              Exit
           end if
           ! Overwrite return connection outlet
           if (AirflowNetworkLinkageData(j)%ConnectionFlag .EQ. EPlusTypeNum_RCN ) then ! Modified on 9/2/09
              found = .TRUE.
              Exit
           end if
           if (AirflowNetworkLinkageData(j)%ConnectionFlag .EQ. EPlusTypeNum_SCN .AND. & ! Modified on 9/2/09
               AirflowNetworkLinkageData(J)%NodeNums(2) == I) then
              found = .TRUE.
              Exit
           end if
        end if
        if (AirflowNetworkLinkageData(J)%NodeNums(2) == I .AND. &
           AirflowNetworkNodeData(AirflowNetworkLinkageData(J)%NodeNums(1))%EPlusTypeNum == EPlusTypeNum_OAN) then
           OANode = .TRUE.
           Exit
        End if
     END DO
     if (found) cycle
     if (AirflowNetworkNodeData(I)%EPlusZoneNum .eq. 0 .AND. &
         AirflowNetworkNodeData(I)%EPlusTypeNum .EQ. EPlusTypeNum_ZIN) cycle
     J = AirflowNetworkNodeData(I)%EPlusNodeNum
     if (J > 0 .AND. (AirflowNetworkNodeData(I)%EPlusZoneNum > 0 .OR. &
                      AirflowNetworkNodeData(I)%EPlusTypeNum == EPlusTypeNum_FOU .OR. &
                      AirflowNetworkNodeData(I)%EPlusTypeNum == EPlusTypeNum_COU .OR. &
                      AirflowNetworkNodeData(I)%EPlusTypeNum == EPlusTypeNum_HXO)) then
         MA((I-1)*AirflowNetworkNumOfNodes+I) = 1.0d10
         MV(I) = Node(j)%CO2*1.0d10
     end if
     if (J > 0 .AND. OANode) then
         MA((I-1)*AirflowNetworkNumOfNodes+I) = 1.0d10
         MV(I) = Node(j)%CO2*1.0d10
     end if
     if (AirflowNetworkNodeData(I)%EPlusZoneNum > 0 .AND. &
        MA((I-1)*AirflowNetworkNumOfNodes+I) .LT. 0.9d10 ) then
        ZoneNum = AirflowNetworkNodeData(I)%EPlusZoneNum
        MA((I-1)*AirflowNetworkNumOfNodes+I) = 1.0d10
        MV(I) = ANCO(ZoneNum)*1.0d10
     end if
     if (AirflowNetworkNodeData(I)%ExtNodeNum > 0 ) then
        MA((I-1)*AirflowNetworkNumOfNodes+I) = 1.0d10
        MV(I) = OutdoorCO2*1.0d10
     end if
  END DO

  ! Check singularity
  DO I=1,AirflowNetworkNumOfNodes
     if (MA((I-1)*AirflowNetworkNumOfNodes+I) .LT. 1.0d-6) then
        CALL ShowFatalError('CalcAirflowNetworkCO2Balance: A diagonal entity is zero in AirflowNetwork matrix at node '//  &
                              TRIM(AirflowNetworkNodeData(I)%Name))
     end if
  END DO

  ! Get an inverse matrix
  Call MRXINV(AirflowNetworkNumOfNodes)

  ! Calculate node temperatures
  DO I=1,AirflowNetworkNumOfNodes
     COZN = 0.0d0
     DO J=1, AirflowNetworkNumOfNodes
        COZN = COZN +MA((I-1)*AirflowNetworkNumOfNodes+J)*MV(J)
     END DO
     AirflowNetworkNodeSimu(I)%CO2Z = COZN
  END DO


RETURN
END SUBROUTINE CalcAirflowNetworkCO2Balance

SUBROUTINE CalcAirflowNetworkGCBalance
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Jan. 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  Revised based on Subroutine CalcAirflowNetworkCO2Balance


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine performs AirflowNetwork generic contaminant simulations.


          ! METHODOLOGY EMPLOYED:
          ! na


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
          ! na


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na


          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank=' '


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER i,j
INTEGER LF,LT,CompNum,NF,NT
INTEGER CompTypeNum,TypeNum
Character(len=MaxNameLength) CompName
REAL(r64) DirSign
REAL(r64) COZN
INTEGER ZoneNum
LOGICAL found,OANode


  MA = 0.0d0
  MV = 0.0d0
  DO I=1,AirflowNetworkNumOfLinks
     CompNum = AirflowNetworkLinkageData(i)%CompNum
     CompTypeNum = AirflowNetworkCompData(CompNum)%CompTypeNum
     CompName = AirflowNetworkCompData(CompNum)%EPlusName
     ! Calculate duct moisture diffusion loss
     if (CompTypeNum == CompTypeNum_DWC .AND. CompName == Blank) then ! Duct component only
        TypeNum = AirflowNetworkCompData(CompNum)%TypeNum
        IF (AirflowNetworkLinkSimu(I)%FLOW .GT. 0.d0) then ! flow direction is tha same as input from node 1 to node 2
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
           DirSign = 1.0d0
        else ! flow direction is tha opposite as input from node 2 to node 1
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
           DirSign = -1.0d0
        end if
        MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+ &
                                                 Abs(AirflowNetworkLinkSimu(I)%FLOW)
        MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW)
     end if
     if (CompTypeNum == CompTypeNum_TMU) then ! Reheat unit: SINGLE DUCT:CONST VOLUME:REHEAT
        TypeNum = AirflowNetworkCompData(CompNum)%TypeNum
        IF (AirflowNetworkLinkSimu(I)%FLOW .GT. 0) then ! flow direction is tha same as input from node 1 to node 2
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
           DirSign = 1.0d0
        else ! flow direction is tha opposite as input from node 2 to node 1
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
           DirSign = -1.0d0
        end if
        MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+ &
                                                 Abs(AirflowNetworkLinkSimu(I)%FLOW)
        MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW)
     end if
     if (CompTypeNum == CompTypeNum_COI) then ! heating or cooling coil
        TypeNum = AirflowNetworkCompData(CompNum)%TypeNum
        IF (AirflowNetworkLinkSimu(I)%FLOW .GT. 0) then ! flow direction is tha same as input from node 1 to node 2
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
           DirSign = 1.0d0
        else ! flow direction is tha opposite as input from node 2 to node 1
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
           DirSign = -1.0d0
        end if
     end if
     ! Calculate temp in a constant pressure drop component
     if (CompTypeNum == CompTypeNum_CPD .AND. CompName == Blank) then ! constant pressure element only
        IF (AirflowNetworkLinkSimu(I)%FLOW .GT. 0) then ! flow direction is tha same as input from node 1 to node 2
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
        else ! flow direction is tha opposite as input from node 2 to node 1
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
        end if
        MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+ &
                                                 Abs(AirflowNetworkLinkSimu(I)%FLOW)
        MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW)
        MV(LT) = 0.0d0
     end if
     ! Calculate return leak
     if ((CompTypeNum == CompTypeNum_PLR .OR. CompTypeNum == CompTypeNum_ELR) .AND. CompName == Blank) then
        ! Return leak component only
        if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusZoneNum > 0) .AND. &
            (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusZoneNum == 0) .AND. &
            (AirflowNetworkLinkSimu(I)%FLOW .GT. 0.0d0)) then
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
           MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+Abs(AirflowNetworkLinkSimu(I)%FLOW)
           MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW)
        end if
        if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%ExtNodeNum > 0) .AND. &
            (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusZoneNum == 0) .AND. &
            (AirflowNetworkLinkSimu(I)%FLOW .GT. 0.0d0)) then
           LF = AirflowNetworkLinkageData(i)%NodeNums(1)
           LT = AirflowNetworkLinkageData(i)%NodeNums(2)
           MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+Abs(AirflowNetworkLinkSimu(I)%FLOW)
           MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW)
        end if
        if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusZoneNum > 0) .AND. &
            (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusZoneNum == 0) .AND. &
            (AirflowNetworkLinkSimu(I)%FLOW2 .GT. 0.0d0)) then
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
           MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+Abs(AirflowNetworkLinkSimu(I)%FLOW2)
           MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW2)
        end if
        if ((AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%ExtNodeNum > 0) .AND. &
            (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusZoneNum == 0) .AND. &
            (AirflowNetworkLinkSimu(I)%FLOW2 .GT. 0.0d0)) then
           LF = AirflowNetworkLinkageData(i)%NodeNums(2)
           LT = AirflowNetworkLinkageData(i)%NodeNums(1)
           MA((LT-1)*AirflowNetworkNumOfNodes+LT) = MA((LT-1)*AirflowNetworkNumOfNodes+LT)+Abs(AirflowNetworkLinkSimu(I)%FLOW2)
           MA((LT-1)*AirflowNetworkNumOfNodes+LF) = -Abs(AirflowNetworkLinkSimu(I)%FLOW2)
        end if
     end if
  END DO

  ! Prescribe temperature for EPlus nodes
  DO I=1,AirflowNetworkNumOfNodes
     found = .FALSE.
     OANode = .FALSE.
     DO J=1,AirflowNetworkNumOfLinks
        if (AirflowNetworkLinkageData(J)%NodeNums(1) == I .OR. AirflowNetworkLinkageData(J)%NodeNums(2) == I) then
           CompNum = AirflowNetworkLinkageData(j)%CompNum
           if (AirflowNetworkCompData(CompNum)%EPlusTypeNum .EQ. EPlusTypeNum_RHT .AND. &
              (.NOT. AirflowNetworkLinkageData(J)%VAVTermDamper)) then
              found = .TRUE.
              Exit
           end if
           ! Overwrite fan outlet node
           if (AirflowNetworkCompData(CompNum)%EPlusTypeNum .EQ. EPlusTypeNum_FAN .AND. &
               AirflowNetworkLinkageData(J)%NodeNums(2) == I) then
              found = .FALSE.
              Exit
           end if
           ! Overwrite return connection outlet
           if (AirflowNetworkLinkageData(j)%ConnectionFlag .EQ. EPlusTypeNum_RCN ) then ! Modified on 9/2/09
              found = .TRUE.
              Exit
           end if
           if (AirflowNetworkLinkageData(j)%ConnectionFlag .EQ. EPlusTypeNum_SCN .AND. & ! Modified on 9/2/09
               AirflowNetworkLinkageData(J)%NodeNums(2) == I) then
              found = .TRUE.
              Exit
           end if
        end if
        if (AirflowNetworkLinkageData(J)%NodeNums(2) == I .AND. &
           AirflowNetworkNodeData(AirflowNetworkLinkageData(J)%NodeNums(1))%EPlusTypeNum == EPlusTypeNum_OAN) then
           OANode = .TRUE.
           Exit
        End if
     END DO
     if (found) cycle
     if (AirflowNetworkNodeData(I)%EPlusZoneNum .eq. 0 .AND. &
         AirflowNetworkNodeData(I)%EPlusTypeNum .EQ. EPlusTypeNum_ZIN) cycle
     J = AirflowNetworkNodeData(I)%EPlusNodeNum
     if (J > 0 .AND. (AirflowNetworkNodeData(I)%EPlusZoneNum > 0 .OR. &
                      AirflowNetworkNodeData(I)%EPlusTypeNum == EPlusTypeNum_FOU .OR. &
                      AirflowNetworkNodeData(I)%EPlusTypeNum == EPlusTypeNum_COU .OR. &
                      AirflowNetworkNodeData(I)%EPlusTypeNum == EPlusTypeNum_HXO)) then
         MA((I-1)*AirflowNetworkNumOfNodes+I) = 1.0d10
         MV(I) = Node(j)%GenContam*1.0d10
     end if
     if (J > 0 .AND. OANode) then
         MA((I-1)*AirflowNetworkNumOfNodes+I) = 1.0d10
         MV(I) = Node(j)%GenContam*1.0d10
     end if
     if (AirflowNetworkNodeData(I)%EPlusZoneNum > 0 .AND. &
        MA((I-1)*AirflowNetworkNumOfNodes+I) .LT. 0.9d10 ) then
        ZoneNum = AirflowNetworkNodeData(I)%EPlusZoneNum
        MA((I-1)*AirflowNetworkNumOfNodes+I) = 1.0d10
        MV(I) = ANGC(ZoneNum)*1.0d10
     end if
     if (AirflowNetworkNodeData(I)%ExtNodeNum > 0 ) then
        MA((I-1)*AirflowNetworkNumOfNodes+I) = 1.0d10
        MV(I) = OutdoorGC*1.0d10
     end if
  END DO

  ! Check singularity
  DO I=1,AirflowNetworkNumOfNodes
     if (MA((I-1)*AirflowNetworkNumOfNodes+I) .LT. 1.0d-6) then
        CALL ShowFatalError('CalcAirflowNetworkGCBalance: A diagonal entity is zero in AirflowNetwork matrix at node '//  &
                              TRIM(AirflowNetworkNodeData(I)%Name))
     end if
  END DO

  ! Get an inverse matrix
  Call MRXINV(AirflowNetworkNumOfNodes)

  ! Calculate node temperatures
  DO I=1,AirflowNetworkNumOfNodes
     COZN = 0.0d0
     DO J=1, AirflowNetworkNumOfNodes
        COZN = COZN +MA((I-1)*AirflowNetworkNumOfNodes+J)*MV(J)
     END DO
     AirflowNetworkNodeSimu(I)%GCZ = COZN
  END DO


RETURN
END SUBROUTINE CalcAirflowNetworkGCBalance

SUBROUTINE MRXINV (NORDER)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Oct. 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  Revised based on Subroutine ADSINV


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine inverses a matrix


          ! METHODOLOGY EMPLOYED:
          ! na


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
          ! na


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT (IN) :: NORDER

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER I, J, K, M
REAL(r64) R1, S
!   ############################################## MATRIX INVERSION
    IVEC = 0
    DO I=1,NORDER
      IVEC(I+20) = I
    END DO
    DO I=1,NORDER
       R1 = 0.0d0
       M  = I
       DO J=I,NORDER
          IF (ABS(R1) .LT. ABS(MA((I-1)*NORDER+J))) THEN
             M  = J
             R1 = MA((I-1)*NORDER+J)
          END IF
       END DO
       IF (I .NE. M) THEN
          K = IVEC(M+20)
          IVEC(M+20) = IVEC(I+20)
          IVEC(I+20) = K
          DO J=1,NORDER
             S         = MA((J-1)*NORDER+I)
             MA((J-1)*NORDER+I) = MA((J-1)*NORDER+M)
             MA((J-1)*NORDER+M) = S
          END DO
       END IF
       MA((I-1)*NORDER+I) = 1.0d0
       DO J=1,NORDER
          MA((I-1)*NORDER+J) = MA((I-1)*NORDER+J)/R1
       END DO
       DO J=1,NORDER
          IF (I .EQ. J) CYCLE
          R1 = MA((J-1)*NORDER+I)
          IF (ABS(R1) .LE. 1.0D-20) CYCLE
          MA((J-1)*NORDER+I) = 0.0d0
          DO K=1,NORDER
             MA((J-1)*NORDER+K) = MA((J-1)*NORDER+K)-R1*MA((I-1)*NORDER+K)
          END DO
60     END DO
    END DO
    DO I=1,NORDER
       IF (IVEC(I+20) .EQ. I) CYCLE
       M = I
       DO WHILE (NORDER .GT. M)
70       M = M+1
         IF (IVEC(M+20) .EQ. I) EXIT
       END DO
80     IVEC(M+20) = IVEC(I+20)
       DO J=1,NORDER
          R1        = MA((I-1)*NORDER+J)
          MA((I-1)*NORDER+J) = MA((M-1)*NORDER+J)
          MA((M-1)*NORDER+J) = R1
       END DO
       IVEC(I+20) = I
90  END DO
    RETURN
!   ########################################################### END
    END SUBROUTINE MRXINV

SUBROUTINE ReportAirflowNetwork


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   2/1/04
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reports outputs of air distribution systems

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHeatBalance, ONLY: MRT, ZonePreDefRep
  Use DataHVACGlobals, ONLY: TimeStepSys, TurnFansOn, TurnFansOff

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: Lam  = 2.5d6    ! Heat of vaporization (J/kg)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  Integer I, N, M, ZN1, ZN2
  REAL(r64) AirDensity, CpAir, Tamb
  REAL(r64) :: ReportingConstant
  REAL(r64) :: ReportingFraction

   if (SimulateAirflowNetwork .lt. AirflowNetworkControlMultizone) return

  ReportingConstant=TimeStepSys*SecInHour

  AirflowNetworkReportData%MultiZoneInfiSenGainW = 0.0d0
  AirflowNetworkReportData%MultiZoneInfiSenGainJ = 0.0d0
  AirflowNetworkReportData%MultiZoneInfiSenLossW = 0.0d0
  AirflowNetworkReportData%MultiZoneInfiSenLossJ = 0.0d0
  AirflowNetworkReportData%MultiZoneInfiLatGainW = 0.0d0
  AirflowNetworkReportData%MultiZoneInfiLatGainJ = 0.0d0
  AirflowNetworkReportData%MultiZoneInfiLatLossW = 0.0d0
  AirflowNetworkReportData%MultiZoneInfiLatLossJ = 0.0d0
  AirflowNetworkReportData%MultiZoneMixSenGainW = 0.0d0
  AirflowNetworkReportData%MultiZoneMixSenGainJ = 0.0d0
  AirflowNetworkReportData%MultiZoneMixSenLossW = 0.0d0
  AirflowNetworkReportData%MultiZoneMixSenLossJ = 0.0d0
  AirflowNetworkReportData%MultiZoneMixLatGainW = 0.0d0
  AirflowNetworkReportData%MultiZoneMixLatGainJ = 0.0d0
  AirflowNetworkReportData%MultiZoneMixLatLossW = 0.0d0
  AirflowNetworkReportData%MultiZoneMixLatLossJ = 0.0d0

  AirflowNetworkReportData%LeakSenGainW = 0.0d0
  AirflowNetworkReportData%LeakSenGainJ = 0.0d0
  AirflowNetworkReportData%LeakSenLossW = 0.0d0
  AirflowNetworkReportData%LeakSenLossJ = 0.0d0
  AirflowNetworkReportData%LeakLatGainW = 0.0d0
  AirflowNetworkReportData%LeakLatGainJ = 0.0d0
  AirflowNetworkReportData%LeakLatLossW = 0.0d0
  AirflowNetworkReportData%LeakLatLossJ = 0.0d0
  AirflowNetworkReportData%CondSenGainW = 0.0d0
  AirflowNetworkReportData%CondSenGainJ = 0.0d0
  AirflowNetworkReportData%CondSenLossW = 0.0d0
  AirflowNetworkReportData%CondSenLossJ = 0.0d0
  AirflowNetworkReportData%DiffLatGainW = 0.0d0
  AirflowNetworkReportData%DiffLatGainJ = 0.0d0
  AirflowNetworkReportData%DiffLatLossW = 0.0d0
  AirflowNetworkReportData%DiffLatLossJ = 0.0d0
  AirflowNetworkReportData%TotalSenGainW = 0.0d0
  AirflowNetworkReportData%TotalSenGainJ = 0.0d0
  AirflowNetworkReportData%TotalSenLossW = 0.0d0
  AirflowNetworkReportData%TotalSenLossJ = 0.0d0
  AirflowNetworkReportData%TotalLatGainW = 0.0d0
  AirflowNetworkReportData%TotalLatGainJ = 0.0d0
  AirflowNetworkReportData%TotalLatLossW = 0.0d0
  AirflowNetworkReportData%TotalLatLossJ = 0.0d0

  ! Calculate sensible and latent loads in each zone from multizone airflows
  If (SimulateAirflowNetwork == AirflowNetworkControlMultizone .OR. SimulateAirflowNetwork == AirflowNetworkControlMultiADS .OR. &
     (SimulateAirflowNetwork == AirflowNetworkControlSimpleADS .AND. AirflowNetworkFanActivated)) then
    Do i=1, AirflowNetworkNumOfSurfaces ! Multizone airflow energy
      N = AirflowNetworkLinkageData(i)%NodeNums(1)
      M = AirflowNetworkLinkageData(i)%NodeNums(2)
      ZN1 = AirflowNetworkNodeData(N)%EPlusZoneNum
      ZN2 = AirflowNetworkNodeData(M)%EPlusZoneNum
      ! Find a linkage from a zone to outdoors
      If (ZN1 > 0 .and. ZN2 == 0) then
        Tamb = Zone(ZN1)%OutDryBulbTemp
        CpAir   = PsyCpAirFnWTdb(OutHumRat, Tamb)
        If (Tamb > MAT(ZN1)) then
          AirflowNetworkReportData(ZN1)%MultiZoneInfiSenGainW = AirflowNetworkReportData(ZN1)%MultiZoneInfiSenGainW + &
             (AirflowNetworkLinkSimu(i)%FLOW2*CpAir*(Tamb-MAT(ZN1)))
          AirflowNetworkReportData(ZN1)%MultiZoneInfiSenGainJ = AirflowNetworkReportData(ZN1)%MultiZoneInfiSenGainJ + &
             (AirflowNetworkLinkSimu(i)%FLOW2*CpAir*(Tamb-MAT(ZN1)))*ReportingConstant
        else
          AirflowNetworkReportData(ZN1)%MultiZoneInfiSenLossW = AirflowNetworkReportData(ZN1)%MultiZoneInfiSenLossW + &
             (AirflowNetworkLinkSimu(i)%FLOW2*CpAir*(MAT(ZN1)-Tamb))
          AirflowNetworkReportData(ZN1)%MultiZoneInfiSenLossJ = AirflowNetworkReportData(ZN1)%MultiZoneInfiSenLossJ + &
             (AirflowNetworkLinkSimu(i)%FLOW2*CpAir*(MAT(ZN1)-Tamb))*ReportingConstant
        end if
        If (OutHumRat > ZoneAirHumRat(ZN1)) then
          AirflowNetworkReportData(ZN1)%MultiZoneInfiLatGainW = AirflowNetworkReportData(ZN1)%MultiZoneInfiLatGainW + &
             (AirflowNetworkLinkSimu(i)%FLOW2*(OutHumRat-ZoneAirHumRat(ZN1)))
          AirflowNetworkReportData(ZN1)%MultiZoneInfiLatGainJ = AirflowNetworkReportData(ZN1)%MultiZoneInfiLatGainJ + &
             (AirflowNetworkLinkSimu(i)%FLOW2*(OutHumRat-ZoneAirHumRat(ZN1)))*ReportingConstant
        else
          AirflowNetworkReportData(ZN1)%MultiZoneInfiLatLossW = AirflowNetworkReportData(ZN1)%MultiZoneInfiLatLossW + &
             (AirflowNetworkLinkSimu(i)%FLOW2*(ZoneAirHumRat(ZN1)-OutHumRat))
          AirflowNetworkReportData(ZN1)%MultiZoneInfiLatLossJ = AirflowNetworkReportData(ZN1)%MultiZoneInfiLatLossJ + &
             (AirflowNetworkLinkSimu(i)%FLOW2*(ZoneAirHumRat(ZN1)-OutHumRat))*ReportingConstant
        end if
      end if
      If (ZN1 == 0 .and. ZN2 > 0) then
        Tamb = Zone(ZN2)%OutDryBulbTemp
        CpAir   = PsyCpAirFnWTdb(OutHumRat, Tamb)
        If (Tamb > MAT(ZN2)) then
          AirflowNetworkReportData(ZN2)%MultiZoneInfiSenGainW = AirflowNetworkReportData(ZN2)%MultiZoneInfiSenGainW + &
             (AirflowNetworkLinkSimu(i)%FLOW*CpAir*(Tamb-MAT(ZN2)))
          AirflowNetworkReportData(ZN2)%MultiZoneInfiSenGainJ = AirflowNetworkReportData(ZN2)%MultiZoneInfiSenGainJ + &
             (AirflowNetworkLinkSimu(i)%FLOW*CpAir*(Tamb-MAT(ZN2)))*ReportingConstant
        else
          AirflowNetworkReportData(ZN2)%MultiZoneInfiSenLossW = AirflowNetworkReportData(ZN2)%MultiZoneInfiSenLossW + &
             (AirflowNetworkLinkSimu(i)%FLOW*CpAir*(MAT(ZN2)-Tamb))
          AirflowNetworkReportData(ZN2)%MultiZoneInfiSenLossJ = AirflowNetworkReportData(ZN2)%MultiZoneInfiSenLossJ + &
             (AirflowNetworkLinkSimu(i)%FLOW*CpAir*(MAT(ZN2)-Tamb))*ReportingConstant
        end if
        If (OutHumRat > ZoneAirHumRat(ZN2)) then
          AirflowNetworkReportData(ZN2)%MultiZoneInfiLatGainW = AirflowNetworkReportData(ZN2)%MultiZoneInfiLatGainW + &
             (AirflowNetworkLinkSimu(i)%FLOW*(OutHumRat-ZoneAirHumRat(ZN2)))
          AirflowNetworkReportData(ZN2)%MultiZoneInfiLatGainJ = AirflowNetworkReportData(ZN2)%MultiZoneInfiLatGainJ + &
             (AirflowNetworkLinkSimu(i)%FLOW*(OutHumRat-ZoneAirHumRat(ZN2)))*ReportingConstant
        else
          AirflowNetworkReportData(ZN2)%MultiZoneInfiLatLossW = AirflowNetworkReportData(ZN2)%MultiZoneInfiLatLossW + &
             (AirflowNetworkLinkSimu(i)%FLOW*(ZoneAirHumRat(ZN2)-OutHumRat))
          AirflowNetworkReportData(ZN2)%MultiZoneInfiLatLossJ = AirflowNetworkReportData(ZN2)%MultiZoneInfiLatLossJ + &
             (AirflowNetworkLinkSimu(i)%FLOW*(ZoneAirHumRat(ZN2)-OutHumRat))*ReportingConstant
        end if
      end if

      If (ZN1 > 0 .and. ZN2 > 0) then
        CpAir   = PsyCpAirFnWTdb(ZoneAirHumRat(ZN1), MAT(ZN1))
        If (MAT(ZN1) > MAT(ZN2)) then
          AirflowNetworkReportData(ZN2)%MultiZoneMixSenGainW = AirflowNetworkReportData(ZN2)%MultiZoneMixSenGainW + &
             (AirflowNetworkLinkSimu(i)%FLOW*CpAir*(MAT(ZN1)-MAT(ZN2)))
          AirflowNetworkReportData(ZN2)%MultiZoneMixSenGainJ = AirflowNetworkReportData(ZN2)%MultiZoneMixSenGainJ + &
             (AirflowNetworkLinkSimu(i)%FLOW*CpAir*(MAT(ZN1)-MAT(ZN2)))*ReportingConstant
        else
          AirflowNetworkReportData(ZN2)%MultiZoneMixSenLossW = AirflowNetworkReportData(ZN2)%MultiZoneMixSenLossW + &
             (AirflowNetworkLinkSimu(i)%FLOW*CpAir*(MAT(ZN2)-MAT(ZN1)))
          AirflowNetworkReportData(ZN2)%MultiZoneMixSenLossJ = AirflowNetworkReportData(ZN2)%MultiZoneMixSenLossJ + &
             (AirflowNetworkLinkSimu(i)%FLOW*CpAir*(MAT(ZN2)-MAT(ZN1)))*ReportingConstant
        end if
        If (ZoneAirHumRat(ZN1) > ZoneAirHumRat(ZN2)) then
          AirflowNetworkReportData(ZN2)%MultiZoneMixLatGainW = AirflowNetworkReportData(ZN2)%MultiZoneMixLatGainW + &
             (AirflowNetworkLinkSimu(i)%FLOW*(ZoneAirHumRat(ZN1)-ZoneAirHumRat(ZN2)))
          AirflowNetworkReportData(ZN2)%MultiZoneMixLatGainJ = AirflowNetworkReportData(ZN2)%MultiZoneMixLatGainJ + &
             (AirflowNetworkLinkSimu(i)%FLOW*(ZoneAirHumRat(ZN1)-ZoneAirHumRat(ZN2)))*ReportingConstant
        else
          AirflowNetworkReportData(ZN2)%MultiZoneMixLatLossW = AirflowNetworkReportData(ZN2)%MultiZoneMixLatLossW + &
             (AirflowNetworkLinkSimu(i)%FLOW*(ZoneAirHumRat(ZN2)-ZoneAirHumRat(ZN1)))
          AirflowNetworkReportData(ZN2)%MultiZoneMixLatLossJ = AirflowNetworkReportData(ZN2)%MultiZoneMixLatLossJ + &
             (AirflowNetworkLinkSimu(i)%FLOW*(ZoneAirHumRat(ZN2)-ZoneAirHumRat(ZN1)))*ReportingConstant
        end if
        CpAir   = PsyCpAirFnWTdb(ZoneAirHumRat(ZN2), MAT(ZN2))
        If (MAT(ZN2) > MAT(ZN1)) then
          AirflowNetworkReportData(ZN1)%MultiZoneMixSenGainW = AirflowNetworkReportData(ZN1)%MultiZoneMixSenGainW + &
             (AirflowNetworkLinkSimu(i)%FLOW2*CpAir*(MAT(ZN2)-MAT(ZN1)))
          AirflowNetworkReportData(ZN1)%MultiZoneMixSenGainJ = AirflowNetworkReportData(ZN1)%MultiZoneMixSenGainJ + &
             (AirflowNetworkLinkSimu(i)%FLOW2*CpAir*(MAT(ZN2)-MAT(ZN1)))*ReportingConstant
        else
          AirflowNetworkReportData(ZN1)%MultiZoneMixSenLossW = AirflowNetworkReportData(ZN1)%MultiZoneMixSenLossW + &
             (AirflowNetworkLinkSimu(i)%FLOW2*CpAir*(MAT(ZN1)-MAT(ZN2)))
          AirflowNetworkReportData(ZN1)%MultiZoneMixSenLossJ = AirflowNetworkReportData(ZN1)%MultiZoneMixSenLossJ + &
             (AirflowNetworkLinkSimu(i)%FLOW2*CpAir*(MAT(ZN1)-MAT(ZN2)))*ReportingConstant
        end if
        If (ZoneAirHumRat(ZN2) > ZoneAirHumRat(ZN1)) then
          AirflowNetworkReportData(ZN1)%MultiZoneMixLatGainW = AirflowNetworkReportData(ZN1)%MultiZoneMixLatGainW + &
             (AirflowNetworkLinkSimu(i)%FLOW2*(ZoneAirHumRat(ZN2)-ZoneAirHumRat(ZN1)))
          AirflowNetworkReportData(ZN1)%MultiZoneMixLatGainJ = AirflowNetworkReportData(ZN1)%MultiZoneMixLatGainJ + &
             (AirflowNetworkLinkSimu(i)%FLOW2*(ZoneAirHumRat(ZN2)-ZoneAirHumRat(ZN1)))*ReportingConstant
        else
          AirflowNetworkReportData(ZN1)%MultiZoneMixLatLossW = AirflowNetworkReportData(ZN1)%MultiZoneMixLatLossW + &
             ABS(AirflowNetworkLinkSimu(i)%FLOW2*(ZoneAirHumRat(ZN1)-ZoneAirHumRat(ZN2)))
          AirflowNetworkReportData(ZN1)%MultiZoneMixLatLossJ = AirflowNetworkReportData(ZN1)%MultiZoneMixLatLossJ + &
             (AirflowNetworkLinkSimu(i)%FLOW2*(ZoneAirHumRat(ZN1)-ZoneAirHumRat(ZN2)))*ReportingConstant
        end if
      end if
    End Do
  End If

  ! Assign data for report
  If (SimulateAirflowNetwork > AirflowNetworkControlMultizone) then
    Do I=1,NumOfZones
      if (AirflowNetworkExchangeData(I)%LeakSen > 0.0d0) then
        AirflowNetworkReportData(I)%LeakSenGainW = AirflowNetworkExchangeData(I)%LeakSen
        AirflowNetworkReportData(I)%LeakSenGainJ = AirflowNetworkExchangeData(I)%LeakSen*ReportingConstant
      else
        AirflowNetworkReportData(I)%LeakSenLossW = -AirflowNetworkExchangeData(I)%LeakSen
        AirflowNetworkReportData(I)%LeakSenLossJ = -AirflowNetworkExchangeData(I)%LeakSen*ReportingConstant
      end if
      if (AirflowNetworkExchangeData(I)%LeakLat > 0.0d0) then
        AirflowNetworkReportData(I)%LeakLatGainW = AirflowNetworkExchangeData(I)%LeakLat*Lam
        AirflowNetworkReportData(I)%LeakLatGainJ = AirflowNetworkExchangeData(I)%LeakLat*Lam*ReportingConstant
      else
        AirflowNetworkReportData(I)%LeakLatLossW = -AirflowNetworkExchangeData(I)%LeakLat*Lam
        AirflowNetworkReportData(I)%LeakLatLossJ = -AirflowNetworkExchangeData(I)%LeakLat*Lam*ReportingConstant
      end if
      if (AirflowNetworkExchangeData(I)%CondSen > 0.0d0) then
        AirflowNetworkReportData(I)%CondSenGainW = AirflowNetworkExchangeData(I)%CondSen
        AirflowNetworkReportData(I)%CondSenGainJ = AirflowNetworkExchangeData(I)%CondSen*ReportingConstant
      else
        AirflowNEtworkReportData(I)%CondSenLossW = -AirflowNetworkExchangeData(I)%CondSen
        AirflowNEtworkReportData(I)%CondSenLossJ = -AirflowNetworkExchangeData(I)%CondSen*ReportingConstant
      end if
      if (AirflowNetworkExchangeData(I)%DiffLat > 0.0d0) then
        AirflowNEtworkReportData(I)%DiffLatGainW = AirflowNetworkExchangeData(I)%DiffLat*Lam
        AirflowNEtworkReportData(I)%DiffLatGainJ = AirflowNetworkExchangeData(I)%DiffLat*Lam*ReportingConstant
      else
        AirflowNEtworkReportData(I)%DiffLatLossW = -AirflowNetworkExchangeData(I)%DiffLat*Lam
        AirflowNEtworkReportData(I)%DiffLatLossJ = -AirflowNetworkExchangeData(I)%DiffLat*Lam*ReportingConstant
      end if

      if (AirflowNetworkExchangeData(I)%TotalSen > 0.0d0) then
        AirflowNEtworkReportData(I)%TotalSenGainW = AirflowNetworkExchangeData(I)%TotalSen
        AirflowNEtworkReportData(I)%TotalSenGainJ = AirflowNetworkExchangeData(I)%TotalSen*ReportingConstant
      else
        AirflowNEtworkReportData(I)%TotalSenLossW = -AirflowNetworkExchangeData(I)%TotalSen
        AirflowNEtworkReportData(I)%TotalSenLossJ = -AirflowNetworkExchangeData(I)%TotalSen*ReportingConstant
      end if
      if (AirflowNetworkExchangeData(I)%TotalLat > 0.0d0) then
        AirflowNEtworkReportData(I)%TotalLatGainW = AirflowNetworkExchangeData(I)%TotalLat*Lam
        AirflowNEtworkReportData(I)%TotalLatGainJ = AirflowNetworkExchangeData(I)%TotalLat*Lam*ReportingConstant
      else
        AirflowNEtworkReportData(I)%TotalLatLossW = -AirflowNetworkExchangeData(I)%TotalLat*Lam
        AirflowNEtworkReportData(I)%TotalLatLossJ = -AirflowNetworkExchangeData(I)%TotalLat*Lam*ReportingConstant
      end if
    End Do
  End If

  ! Zone report

  AirflowNetworkZnRpt%MeanAirTemp = 0.0d0
  AirflowNetworkZnRpt%OperativeTemp = 0.0d0
  AirflowNetworkZnRpt%InfilHeatGain=0.0d0
  AirflowNetworkZnRpt%InfilHeatLoss=0.0d0
  AirflowNetworkZnRpt%InfilVolume=0.0d0
  AirflowNetworkZnRpt%InfilMass=0.0d0
  AirflowNetworkZnRpt%InfilAirChangeRate  =0.0d0
  AirflowNetworkZnRpt%MixVolume =0.0d0
  AirflowNetworkZnRpt%MixMass =0.0d0

  If (SupplyFanType .EQ. FanType_SimpleOnOff .AND. OnOffFanRunTimeFraction < 1.0d0 .AND. OnOffFanRunTimeFraction > 0.0d0) then
    ! ON Cycle calculation
    DO I=1, NumOfZones
      AirflowNetworkReportData(I)%MultiZoneInfiSenGainW=AirflowNetworkReportData(I)%MultiZoneInfiSenGainW*OnOffFanRunTimeFraction
      AirflowNetworkReportData(I)%MultiZoneInfiSenGainJ=AirflowNetworkReportData(I)%MultiZoneInfiSenGainJ*OnOffFanRunTimeFraction
      AirflowNetworkReportData(I)%MultiZoneInfiSenLossW=AirflowNetworkReportData(I)%MultiZoneInfiSenLossW*OnOffFanRunTimeFraction
      AirflowNetworkReportData(I)%MultiZoneInfiSenLossJ=AirflowNetworkReportData(I)%MultiZoneInfiSenLossJ*OnOffFanRunTimeFraction
      AirflowNetworkReportData(I)%MultiZoneInfiLatGainW=AirflowNetworkReportData(I)%MultiZoneInfiLatGainW*OnOffFanRunTimeFraction
      AirflowNetworkReportData(I)%MultiZoneInfiLatGainJ=AirflowNetworkReportData(I)%MultiZoneInfiLatGainJ*OnOffFanRunTimeFraction
      AirflowNetworkReportData(I)%MultiZoneInfiLatLossW=AirflowNetworkReportData(I)%MultiZoneInfiLatLossW*OnOffFanRunTimeFraction
      AirflowNetworkReportData(I)%MultiZoneInfiLatLossJ=AirflowNetworkReportData(I)%MultiZoneInfiLatLossJ*OnOffFanRunTimeFraction
      AirflowNetworkReportData(I)%MultiZoneMixSenGainW=AirflowNetworkReportData(I)%MultiZoneMixSenGainW*OnOffFanRunTimeFraction
      AirflowNetworkReportData(I)%MultiZoneMixSenGainJ=AirflowNetworkReportData(I)%MultiZoneMixSenGainJ*OnOffFanRunTimeFraction
      AirflowNetworkReportData(I)%MultiZoneMixSenLossW=AirflowNetworkReportData(I)%MultiZoneMixSenLossW*OnOffFanRunTimeFraction
      AirflowNetworkReportData(I)%MultiZoneMixSenLossJ=AirflowNetworkReportData(I)%MultiZoneMixSenLossJ*OnOffFanRunTimeFraction
      AirflowNetworkReportData(I)%MultiZoneMixLatGainW=AirflowNetworkReportData(I)%MultiZoneMixLatGainW*OnOffFanRunTimeFraction
      AirflowNetworkReportData(I)%MultiZoneMixLatGainJ=AirflowNetworkReportData(I)%MultiZoneMixLatGainJ*OnOffFanRunTimeFraction
      AirflowNetworkReportData(I)%MultiZoneMixLatLossW=AirflowNetworkReportData(I)%MultiZoneMixLatLossW*OnOffFanRunTimeFraction
      AirflowNetworkReportData(I)%MultiZoneMixLatLossJ=AirflowNetworkReportData(I)%MultiZoneMixLatLossJ*OnOffFanRunTimeFraction
    End Do
    ! Off Cycle addon
    Do i=1, AirflowNetworkNumOfSurfaces ! Multizone airflow energy
      N = AirflowNetworkLinkageData(i)%NodeNums(1)
      M = AirflowNetworkLinkageData(i)%NodeNums(2)
      ZN1 = AirflowNetworkNodeData(N)%EPlusZoneNum
      ZN2 = AirflowNetworkNodeData(M)%EPlusZoneNum
      ReportingFraction=(1.0d0-OnOffFanRunTimeFraction)
      ! Find a linkage from a zone to outdoors
      If (ZN1 > 0 .and. ZN2 == 0) then
        Tamb = Zone(ZN1)%OutDryBulbTemp
        CpAir   = PsyCpAirFnWTdb(OutHumRat, Tamb)
        If (Tamb > MAT(ZN1)) then
          AirflowNetworkReportData(ZN1)%MultiZoneInfiSenGainW = AirflowNetworkReportData(ZN1)%MultiZoneInfiSenGainW + &
             (AirflowNetworkLinkReport1(i)%FLOW2OFF*CpAir*(Tamb-MAT(ZN1)))*(1.0d0-OnOffFanRunTimeFraction)
          AirflowNetworkReportData(ZN1)%MultiZoneInfiSenGainJ = AirflowNetworkReportData(ZN1)%MultiZoneInfiSenGainJ + &
             (AirflowNetworkLinkReport1(i)%FLOW2OFF*CpAir*(Tamb-MAT(ZN1)))*ReportingConstant*ReportingFraction
        else
          AirflowNetworkReportData(ZN1)%MultiZoneInfiSenLossW = AirflowNetworkReportData(ZN1)%MultiZoneInfiSenLossW + &
             (AirflowNetworkLinkReport1(i)%FLOW2OFF*CpAir*(MAT(ZN1)-Tamb))*(1.0-OnOffFanRunTimeFraction)
          AirflowNetworkReportData(ZN1)%MultiZoneInfiSenLossJ = AirflowNetworkReportData(ZN1)%MultiZoneInfiSenLossJ + &
             (AirflowNetworkLinkReport1(i)%FLOW2OFF*CpAir*(MAT(ZN1)-Tamb))*ReportingConstant*ReportingFraction
        end if
        If (OutHumRat > ZoneAirHumRat(ZN1)) then
          AirflowNetworkReportData(ZN1)%MultiZoneInfiLatGainW = AirflowNetworkReportData(ZN1)%MultiZoneInfiLatGainW + &
             (AirflowNetworkLinkReport1(i)%FLOW2OFF*(OutHumRat-ZoneAirHumRat(ZN1)))*ReportingFraction
          AirflowNetworkReportData(ZN1)%MultiZoneInfiLatGainJ = AirflowNetworkReportData(ZN1)%MultiZoneInfiLatGainJ + &
             (AirflowNetworkLinkReport1(i)%FLOW2OFF*(OutHumRat-ZoneAirHumRat(ZN1)))*ReportingConstant*ReportingFraction
        else
          AirflowNetworkReportData(ZN1)%MultiZoneInfiLatLossW = AirflowNetworkReportData(ZN1)%MultiZoneInfiLatLossW + &
             (AirflowNetworkLinkReport1(i)%FLOW2OFF*(ZoneAirHumRat(ZN1)-OutHumRat))*ReportingFraction
          AirflowNetworkReportData(ZN1)%MultiZoneInfiLatLossJ = AirflowNetworkReportData(ZN1)%MultiZoneInfiLatLossJ + &
             (AirflowNetworkLinkReport1(i)%FLOW2OFF*(ZoneAirHumRat(ZN1)-OutHumRat))*ReportingConstant*ReportingFraction
        end if
      end if
      If (ZN1 == 0 .and. ZN2 > 0) then
        Tamb = Zone(ZN2)%OutDryBulbTemp
        CpAir   = PsyCpAirFnWTdb(OutHumRat, Tamb)
        If (Tamb > MAT(ZN2)) then
          AirflowNetworkReportData(ZN2)%MultiZoneInfiSenGainW = AirflowNetworkReportData(ZN2)%MultiZoneInfiSenGainW + &
             (AirflowNetworkLinkReport1(i)%FLOWOFF*CpAir*(Tamb-MAT(ZN2)))*ReportingFraction
          AirflowNetworkReportData(ZN2)%MultiZoneInfiSenGainJ = AirflowNetworkReportData(ZN2)%MultiZoneInfiSenGainJ + &
             (AirflowNetworkLinkReport1(i)%FLOWOFF*CpAir*(Tamb-MAT(ZN2)))*ReportingConstant*ReportingFraction
        else
          AirflowNetworkReportData(ZN2)%MultiZoneInfiSenLossW = AirflowNetworkReportData(ZN2)%MultiZoneInfiSenLossW + &
             (AirflowNetworkLinkReport1(i)%FLOWOFF*CpAir*(MAT(ZN2)-Tamb))*ReportingFraction
          AirflowNetworkReportData(ZN2)%MultiZoneInfiSenLossJ = AirflowNetworkReportData(ZN2)%MultiZoneInfiSenLossJ + &
             (AirflowNetworkLinkReport1(i)%FLOWOFF*CpAir*(MAT(ZN2)-Tamb))*ReportingConstant*ReportingFraction
        end if
        If (OutHumRat > ZoneAirHumRat(ZN2)) then
          AirflowNetworkReportData(ZN2)%MultiZoneInfiLatGainW = AirflowNetworkReportData(ZN2)%MultiZoneInfiLatGainW + &
             (AirflowNetworkLinkReport1(i)%FLOWOFF*(OutHumRat-ZoneAirHumRat(ZN2)))*ReportingFraction
          AirflowNetworkReportData(ZN2)%MultiZoneInfiLatGainJ = AirflowNetworkReportData(ZN2)%MultiZoneInfiLatGainJ + &
             (AirflowNetworkLinkReport1(i)%FLOWOFF*(OutHumRat-ZoneAirHumRat(ZN2)))*ReportingConstant*ReportingFraction
        else
          AirflowNetworkReportData(ZN2)%MultiZoneInfiLatLossW = AirflowNetworkReportData(ZN2)%MultiZoneInfiLatLossW + &
             (AirflowNetworkLinkReport1(i)%FLOWOFF*(ZoneAirHumRat(ZN2)-OutHumRat))*ReportingFraction
          AirflowNetworkReportData(ZN2)%MultiZoneInfiLatLossJ = AirflowNetworkReportData(ZN2)%MultiZoneInfiLatLossJ + &
             (AirflowNetworkLinkReport1(i)%FLOWOFF*(ZoneAirHumRat(ZN2)-OutHumRat))*ReportingConstant*ReportingFraction
        end if
      end if

      If (ZN1 > 0 .and. ZN2 > 0) then
        CpAir   = PsyCpAirFnWTdb(ZoneAirHumRat(ZN1), MAT(ZN1))
        If (MAT(ZN1) > MAT(ZN2)) then
          AirflowNetworkReportData(ZN2)%MultiZoneMixSenGainW = AirflowNetworkReportData(ZN2)%MultiZoneMixSenGainW + &
             (AirflowNetworkLinkReport1(i)%FLOWOFF*CpAir*(MAT(ZN1)-MAT(ZN2)))*ReportingFraction
          AirflowNetworkReportData(ZN2)%MultiZoneMixSenGainJ = AirflowNetworkReportData(ZN2)%MultiZoneMixSenGainJ + &
             (AirflowNetworkLinkReport1(i)%FLOWOFF*CpAir*(MAT(ZN1)-MAT(ZN2)))*ReportingConstant*ReportingFraction
        else
          AirflowNetworkReportData(ZN2)%MultiZoneMixSenLossW = AirflowNetworkReportData(ZN2)%MultiZoneMixSenLossW + &
             (AirflowNetworkLinkReport1(i)%FLOWOFF*CpAir*(MAT(ZN2)-MAT(ZN1)))*ReportingFraction
          AirflowNetworkReportData(ZN2)%MultiZoneMixSenLossJ = AirflowNetworkReportData(ZN2)%MultiZoneMixSenLossJ + &
             (AirflowNetworkLinkReport1(i)%FLOWOFF*CpAir*(MAT(ZN2)-MAT(ZN1)))*ReportingConstant*ReportingFraction
        end if
        If (ZoneAirHumRat(ZN1) > ZoneAirHumRat(ZN2)) then
          AirflowNetworkReportData(ZN2)%MultiZoneMixLatGainW = AirflowNetworkReportData(ZN2)%MultiZoneMixLatGainW + &
             (AirflowNetworkLinkReport1(i)%FLOWOFF*(ZoneAirHumRat(ZN1)-ZoneAirHumRat(ZN2)))*ReportingFraction
          AirflowNetworkReportData(ZN2)%MultiZoneMixLatGainJ = AirflowNetworkReportData(ZN2)%MultiZoneMixLatGainJ + &
             (AirflowNetworkLinkReport1(i)%FLOWOFF*(ZoneAirHumRat(ZN1)-ZoneAirHumRat(ZN2)))*ReportingConstant*ReportingFraction
        else
          AirflowNetworkReportData(ZN2)%MultiZoneMixLatLossW = AirflowNetworkReportData(ZN2)%MultiZoneMixLatLossW + &
             (AirflowNetworkLinkReport1(i)%FLOWOFF*(ZoneAirHumRat(ZN2)-ZoneAirHumRat(ZN1)))*ReportingFraction
          AirflowNetworkReportData(ZN2)%MultiZoneMixLatLossJ = AirflowNetworkReportData(ZN2)%MultiZoneMixLatLossJ + &
             (AirflowNetworkLinkReport1(i)%FLOWOFF*(ZoneAirHumRat(ZN2)-ZoneAirHumRat(ZN1)))*ReportingConstant*ReportingFraction
        end if
        CpAir   = PsyCpAirFnWTdb(ZoneAirHumRat(ZN2), MAT(ZN2))
        If (MAT(ZN2) > MAT(ZN1)) then
          AirflowNetworkReportData(ZN1)%MultiZoneMixSenGainW = AirflowNetworkReportData(ZN1)%MultiZoneMixSenGainW + &
             (AirflowNetworkLinkReport1(i)%FLOW2OFF*CpAir*(MAT(ZN2)-MAT(ZN1)))*ReportingFraction
          AirflowNetworkReportData(ZN1)%MultiZoneMixSenGainJ = AirflowNetworkReportData(ZN1)%MultiZoneMixSenGainJ + &
             (AirflowNetworkLinkReport1(i)%FLOW2OFF*CpAir*(MAT(ZN2)-MAT(ZN1)))*ReportingConstant*ReportingFraction
        else
          AirflowNetworkReportData(ZN1)%MultiZoneMixSenLossW = AirflowNetworkReportData(ZN1)%MultiZoneMixSenLossW + &
             (AirflowNetworkLinkReport1(i)%FLOW2OFF*CpAir*(MAT(ZN1)-MAT(ZN2)))*ReportingFraction
          AirflowNetworkReportData(ZN1)%MultiZoneMixSenLossJ = AirflowNetworkReportData(ZN1)%MultiZoneMixSenLossJ + &
             (AirflowNetworkLinkReport1(i)%FLOW2OFF*CpAir*(MAT(ZN1)-MAT(ZN2)))*ReportingConstant*ReportingFraction
        end if
        If (ZoneAirHumRat(ZN2) > ZoneAirHumRat(ZN1)) then
          AirflowNetworkReportData(ZN1)%MultiZoneMixLatGainW = AirflowNetworkReportData(ZN1)%MultiZoneMixLatGainW + &
             (AirflowNetworkLinkReport1(i)%FLOW2OFF*(ZoneAirHumRat(ZN2)-ZoneAirHumRat(ZN1)))*ReportingFraction
          AirflowNetworkReportData(ZN1)%MultiZoneMixLatGainJ = AirflowNetworkReportData(ZN1)%MultiZoneMixLatGainJ + &
             (AirflowNetworkLinkReport1(i)%FLOW2OFF*(ZoneAirHumRat(ZN2)-ZoneAirHumRat(ZN1)))*ReportingConstant*ReportingFraction
        else
          AirflowNetworkReportData(ZN1)%MultiZoneMixLatLossW = AirflowNetworkReportData(ZN1)%MultiZoneMixLatLossW + &
             ABS(AirflowNetworkLinkReport1(i)%FLOW2OFF*(ZoneAirHumRat(ZN1)-ZoneAirHumRat(ZN2)))*ReportingFraction
          AirflowNetworkReportData(ZN1)%MultiZoneMixLatLossJ = AirflowNetworkReportData(ZN1)%MultiZoneMixLatLossJ + &
             (AirflowNetworkLinkReport1(i)%FLOW2OFF*(ZoneAirHumRat(ZN1)-ZoneAirHumRat(ZN2)))*ReportingConstant*ReportingFraction
        end if
      end if
    End Do
  End If

  If (.NOT. (SimulateAirflowNetwork == AirflowNetworkControlMultizone .OR. &
             SimulateAirflowNetwork == AirflowNetworkControlMultiADS)) Return

  DO i = 1, NumOfZones
    AirflowNetworkZnRpt(i)%MeanAirTemp = MAT(i)
    AirflowNetworkZnRpt(i)%OperativeTemp = 0.5d0*(MAT(i)+MRT(i))
  END DO

  DO i = 1, NumOfZones   ! Start of zone loads report variable update loop ...
    Tamb = Zone(i)%OutDryBulbTemp
    CpAir = PsyCpAirFnWTdb(ZoneAirHumRatAvg(i),MAT(i))
    AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,MAT(i),ZoneAirHumRatAvg(i))
    IF (MAT(i) > Tamb) THEN
      AirflowNetworkZnRpt(i)%InfilHeatLoss = AirflowNetworkExchangeData(i)%SumMCp*(MAT(i)-Tamb)*ReportingConstant
      AirflowNetworkZnRpt(i)%InfilHeatGain=0.0d0
    ELSE IF (MAT(i) <= Tamb) THEN
      AirflowNetworkZnRpt(i)%InfilHeatGain = AirflowNetworkExchangeData(i)%SumMCp*(Tamb-MAT(i))*ReportingConstant
      AirflowNetworkZnRpt(i)%InfilHeatLoss =0.0d0
    END IF
    AirflowNetworkZnRpt(i)%InfilVolume    = (AirflowNetworkExchangeData(i)%SumMCp/CpAir/AirDensity)*ReportingConstant
    AirflowNetworkZnRpt(i)%InfilAirChangeRate = AirflowNetworkZnRpt(i)%InfilVolume/(TimeStepSys*Zone(i)%Volume)
    AirflowNetworkZnRpt(i)%InfilMass      = (AirflowNetworkExchangeData(i)%SumMCp/CpAir)*ReportingConstant
    AirflowNetworkZnRpt(i)%MixVolume      = (AirflowNetworkExchangeData(i)%SumMMCp/CpAir/AirDensity)*ReportingConstant
    AirflowNetworkZnRpt(i)%MixMass        = (AirflowNetworkExchangeData(i)%SumMMCp/CPAir)*ReportingConstant
    !save values for predefined report
    IF (ZonePreDefRep(i)%isOccupied) THEN
      ZonePreDefRep(i)%AFNInfilVolTotal = ZonePreDefRep(i)%AFNInfilVolTotal + &
        AirflowNetworkZnRpt(i)%InfilVolume
      IF (AirflowNetworkZnRpt(i)%InfilVolume .LT. ZonePreDefRep(i)%AFNInfilVolMin) THEN
        ZonePreDefRep(i)%AFNInfilVolMin = AirflowNetworkZnRpt(i)%InfilVolume
      END IF
    END IF
  END DO    ! ... end of zone loads report variable update loop.

  ! Rewrite AirflowNetwork airflow rate
  do i=1,NumOfLinksMultiZone
     Tamb = OutDryBulbTempAt(AirflowNetworkLinkageData(i)%NodeHeights(1))
     AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,Tamb,OutHumRat)
     If (SupplyFanType .EQ. FanType_SimpleOnOff .AND. OnOffFanRunTimeFraction < 1.0d0 .AND. OnOffFanRunTimeFraction > 0.0d0) then
       AirflowNetworkLinkReport(i)%VolFlow = AirflowNetworkLinkReport1(i)%Flow/AirDensity
       AirflowNetworkLinkReport(i)%VolFlow2 = AirflowNetworkLinkReport1(i)%Flow2/AirDensity
     Else
       AirflowNetworkLinkReport(i)%VolFlow = AirflowNetworkLinkReport(i)%Flow/AirDensity
       AirflowNetworkLinkReport(i)%VolFlow2 = AirflowNetworkLinkReport(i)%Flow2/AirDensity
     End If
  end do

  If (AirflowNetworkNumOfLinks .gt. NumOfLinksMultiZone) Then
    DO i = NumOfLinksMultiZone+1, AirflowNetworkNumOfLinks
      N = AirflowNetworkLinkageData(i)%NodeNums(1)
      M = AirflowNetworkLinkageData(i)%NodeNums(2)
      AirDensity = PsyRhoAirFnPbTdbW((AirflowNetworkNodeSimu(N)%PZ+AirflowNetworkNodeSimu(M)%PZ)/2.d0+OutBaroPress, &
                 (AirflowNetworkNodeSimu(N)%TZ+AirflowNetworkNodeSimu(M)%TZ)/2.d0,   &
                 (AirflowNetworkNodeSimu(N)%WZ+AirflowNetworkNodeSimu(M)%WZ)/2.d0)
      If (SupplyFanType .EQ. FanType_SimpleOnOff .AND. OnOffFanRunTimeFraction < 1.0d0 .AND. OnOffFanRunTimeFraction > 0.0d0) then
        AirflowNetworkLinkReport(i)%VolFlow  = AirflowNetworkLinkReport(i)%Flow/AirDensity*(1.0d0-OnOffFanRunTimeFraction)
        AirflowNetworkLinkReport(i)%VolFlow2 = AirflowNetworkLinkReport(i)%Flow2/AirDensity*(1.0d0-OnOffFanRunTimeFraction)
      Else
        AirflowNetworkLinkReport(i)%VolFlow  = AirflowNetworkLinkReport(i)%Flow/AirDensity
        AirflowNetworkLinkReport(i)%VolFlow2 = AirflowNetworkLinkReport(i)%Flow2/AirDensity
      End If
    End Do
  End If

  RETURN

END SUBROUTINE ReportAirflowNetwork

SUBROUTINE UpdateAirflowNetwork(FirstHVACIteration)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   12/10/05
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine update varaibles used in the AirflowNetwork model.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use DataHVACGlobals, ONLY: TimeStepSys
  USE DataHVACGlobals, ONLY: TurnFansOn, TurnFansOff, VerySmallMassFlow
  USE DataAirLoop,     ONLY: LoopSystemOnMassFlowrate,LoopSystemOffMassFlowrate,LoopFanOperationMode, &
                             LoopOnOffFanPartLoadRatio,LoopHeatingCoilMaxRTF,LoopONOffFanRTF,LoopDXCoilRTF

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN), OPTIONAL :: FirstHVACIteration ! True when solution technique on first iteration

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  Integer I, J, N, M, ZN1, ZN2, Node1, Node2, Node3
  REAL(r64) CpAir, Qsen, Qlat, AirDensity, Tamb
  REAL(r64) PartLoadRatio, OnOffRatio,NodeMass, AFNMass
  LOGICAL WriteFlag
  LOGICAL,SAVE :: MyOneTimeFlag = .TRUE.
  LOGICAL,SAVE :: MyOneTimeFlag1 = .TRUE.

  AirflowNetworkExchangeData%SumMCp = 0.0d0
  AirflowNetworkExchangeData%SumMCpT = 0.0d0
  AirflowNetworkExchangeData%SumMHr = 0.0d0
  AirflowNetworkExchangeData%SumMHrW = 0.0d0
  AirflowNetworkExchangeData%SumMMCp = 0.0d0
  AirflowNetworkExchangeData%SumMMCpT = 0.0d0
  AirflowNetworkExchangeData%SumMMHr = 0.0d0
  AirflowNetworkExchangeData%SumMMHrW = 0.0d0
  IF (Contaminant%CO2Simulation) Then
    AirflowNetworkExchangeData%SumMHrCO = 0.0d0
    AirflowNetworkExchangeData%SumMMHrCO = 0.0d0
  End If
  IF (Contaminant%GenericContamSimulation) Then
    AirflowNetworkExchangeData%SumMHrGC = 0.0d0
    AirflowNetworkExchangeData%SumMMHrGC = 0.0d0
  End If

  ! Calculate sensible and latent loads in each zone from multizone airflows
  If (SimulateAirflowNetwork == AirflowNetworkControlMultizone .OR. SimulateAirflowNetwork == AirflowNetworkControlMultiADS .OR. &
     (SimulateAirflowNetwork == AirflowNetworkControlSimpleADS .AND. AirflowNetworkFanActivated)) then
    Do i=1, NumOfLinksMultiZone ! Multizone airflow energy
      N = AirflowNetworkLinkageData(i)%NodeNums(1)
      M = AirflowNetworkLinkageData(i)%NodeNums(2)
      ZN1 = AirflowNetworkNodeData(N)%EPlusZoneNum
      ZN2 = AirflowNetworkNodeData(M)%EPlusZoneNum
      If (ZN1 > 0 .AND. ZN2 == 0) then
        ! Find a linkage from outdoors to this zone
        Tamb = Zone(ZN1)%OutDryBulbTemp
        CpAir = PsyCpAirFnWTdb(OutHumRat, Tamb)
        AirflowNetworkExchangeData(ZN1)%SumMCp = AirflowNetworkExchangeData(ZN1)%SumMCp + &
                                                 AirflowNetworkLinkSimu(i)%FLOW2*CpAir
        AirflowNetworkExchangeData(ZN1)%SumMCpT = AirflowNetworkExchangeData(ZN1)%SumMCpT + &
                                                  AirflowNetworkLinkSimu(i)%FLOW2*CpAir*Tamb
        AirflowNetworkExchangeData(ZN1)%SumMHr = AirflowNetworkExchangeData(ZN1)%SumMHr + &
                                                 AirflowNetworkLinkSimu(i)%FLOW2
        AirflowNetworkExchangeData(ZN1)%SumMHrW = AirflowNetworkExchangeData(ZN1)%SumMHrW + &
                                                  AirflowNetworkLinkSimu(i)%FLOW2*OutHumRat
        IF (Contaminant%CO2Simulation) Then
          AirflowNetworkExchangeData(ZN1)%SumMHrCO = AirflowNetworkExchangeData(ZN1)%SumMHrCO + &
                                                  AirflowNetworkLinkSimu(i)%FLOW2*OutdoorCO2
        End If
        IF (Contaminant%GenericContamSimulation) Then
          AirflowNetworkExchangeData(ZN1)%SumMHrGC = AirflowNetworkExchangeData(ZN1)%SumMHrGC + &
                                                  AirflowNetworkLinkSimu(i)%FLOW2*OutdoorGC
        End If
      end if
      If (ZN1 == 0 .AND. ZN2 > 0) then
        ! Find a linkage from outdoors to this zone
        Tamb = Zone(ZN2)%OutDryBulbTemp
        CpAir   = PsyCpAirFnWTdb(OutHumRat, Tamb)
        AirflowNetworkExchangeData(ZN2)%SumMCp = AirflowNetworkExchangeData(ZN2)%SumMCp + &
                                                 AirflowNetworkLinkSimu(i)%FLOW*CpAir
        AirflowNetworkExchangeData(ZN2)%SumMCpT = AirflowNetworkExchangeData(ZN2)%SumMCpT + &
                                                  AirflowNetworkLinkSimu(i)%FLOW*CpAir*Tamb
        AirflowNetworkExchangeData(ZN2)%SumMHr = AirflowNetworkExchangeData(ZN2)%SumMHr + &
                                                 AirflowNetworkLinkSimu(i)%FLOW
        AirflowNetworkExchangeData(ZN2)%SumMHrW = AirflowNetworkExchangeData(ZN2)%SumMHrW + &
                                                  AirflowNetworkLinkSimu(i)%FLOW*OutHumRat
        IF (Contaminant%CO2Simulation) Then
          AirflowNetworkExchangeData(ZN2)%SumMHrCO = AirflowNetworkExchangeData(ZN2)%SumMHrCO + &
                                                  AirflowNetworkLinkSimu(i)%FLOW*OutdoorCO2
        End If
        IF (Contaminant%GenericContamSimulation) Then
          AirflowNetworkExchangeData(ZN2)%SumMHrGC = AirflowNetworkExchangeData(ZN2)%SumMHrGC + &
                                                  AirflowNetworkLinkSimu(i)%FLOW*OutdoorGC
        End If
      end if
      If (ZN1 > 0 .AND. ZN2 > 0) then
        ! Find a linkage from outdoors to this zone
        CpAir   = PsyCpAirFnWTdb(ANZW(ZN1), ANZT(ZN1))
        AirflowNetworkExchangeData(ZN2)%SumMMCp = AirflowNetworkExchangeData(ZN2)%SumMMCp + &
                                                  AirflowNetworkLinkSimu(i)%FLOW*CpAir
        AirflowNetworkExchangeData(ZN2)%SumMMCpT = AirflowNetworkExchangeData(ZN2)%SumMMCpT + &
                                                   AirflowNetworkLinkSimu(i)%FLOW*CpAir*ANZT(ZN1)
        AirflowNetworkExchangeData(ZN2)%SumMMHr = AirflowNetworkExchangeData(ZN2)%SumMMHr + &
                                                  AirflowNetworkLinkSimu(i)%FLOW
        AirflowNetworkExchangeData(ZN2)%SumMMHrW = AirflowNetworkExchangeData(ZN2)%SumMMHrW + &
                                                   AirflowNetworkLinkSimu(i)%FLOW*ANZW(ZN1)
        IF (Contaminant%CO2Simulation) Then
          AirflowNetworkExchangeData(ZN2)%SumMMHrCO = AirflowNetworkExchangeData(ZN2)%SumMMHrCO + &
                                                   AirflowNetworkLinkSimu(i)%FLOW*ANCO(ZN1)
        End If
        IF (Contaminant%GenericContamSimulation) Then
          AirflowNetworkExchangeData(ZN2)%SumMMHrGC = AirflowNetworkExchangeData(ZN2)%SumMMHrGC + &
                                                   AirflowNetworkLinkSimu(i)%FLOW*ANGC(ZN1)
        End If
        CpAir = PsyCpAirFnWTdb(ANZW(ZN2), ANZT(ZN2))
        AirflowNetworkExchangeData(ZN1)%SumMMCp = AirflowNetworkExchangeData(ZN1)%SumMMCp + &
                                                  AirflowNetworkLinkSimu(i)%FLOW2*CpAir
        AirflowNetworkExchangeData(ZN1)%SumMMCpT = AirflowNetworkExchangeData(ZN1)%SumMMCpT + &
                                                   AirflowNetworkLinkSimu(i)%FLOW2*CpAir*ANZT(ZN2)
        AirflowNetworkExchangeData(ZN1)%SumMMHr = AirflowNetworkExchangeData(ZN1)%SumMMHr + &
                                                  AirflowNetworkLinkSimu(i)%FLOW2
        AirflowNetworkExchangeData(ZN1)%SumMMHrW = AirflowNetworkExchangeData(ZN1)%SumMMHrW + &
                                                   AirflowNetworkLinkSimu(i)%FLOW2*ANZW(ZN2)
        IF (Contaminant%CO2Simulation) Then
          AirflowNetworkExchangeData(ZN1)%SumMMHrCO = AirflowNetworkExchangeData(ZN1)%SumMMHrCO + &
                                                   AirflowNetworkLinkSimu(i)%FLOW2*ANCO(ZN2)
        End If
        IF (Contaminant%GenericContamSimulation) Then
          AirflowNetworkExchangeData(ZN1)%SumMMHrGC = AirflowNetworkExchangeData(ZN1)%SumMMHrGC + &
                                                   AirflowNetworkLinkSimu(i)%FLOW2*ANGC(ZN2)
        End If
      end if
    End Do
  End If
  ! End of update of multizone airflow calculations

  ! Initialize these values
  AirflowNetworkExchangeData%LeakSen = 0.0d0
  AirflowNetworkExchangeData%CondSen = 0.0d0
  AirflowNetworkExchangeData%LeakLat = 0.0d0
  AirflowNetworkExchangeData%DiffLat = 0.0d0
  AirflowNetworkExchangeData%MultiZoneSen = 0.0d0
  AirflowNetworkExchangeData%MultiZoneLat = 0.0d0

  ! Rewrite AirflowNetwork airflow rate
  do i=1,NumOfLinksMultiZone
     Tamb = OutDryBulbTempAt(AirflowNetworkLinkageData(i)%NodeHeights(1))
     AirDensity = PsyRhoAirFnPbTdbW(OutBaroPress,Tamb,OutHumRat)
     AirflowNetworkLinkSimu(i)%VolFlow = AirflowNetworkLinkSimu(i)%Flow/AirDensity
     AirflowNetworkLinkSimu(i)%VolFlow2 = AirflowNetworkLinkSimu(i)%Flow2/AirDensity
  end do

  AirflowNetworkLinkReport%FLOW = AirflowNetworkLinkSimu%FLOW
  AirflowNetworkLinkReport%FLOW2 = AirflowNetworkLinkSimu%FLOW2
  AirflowNetworkLinkReport%VolFlow = AirflowNetworkLinkSimu%VolFlow
  AirflowNetworkLinkReport%VolFlow2 = AirflowNetworkLinkSimu%VolFlow2

  ! Save zone loads from multizone calculation for later summation
  If (PRESENT(FirstHVACIteration)) then
    If (FirstHVACIteration .AND. SupplyFanType .EQ. FanType_SimpleOnOff) then
      AirflowNetworkMultiExchangeData = AirflowNetworkExchangeData
      Do I=1,AirflowNetworkNumOfZones
        AirflowNetworkNodeReport(i)%PZ = AirflowNetworkNodeSimu(i)%PZ
        AirflowNetworkNodeReport(i)%PZOFF  = AirflowNetworkNodeSimu(i)%PZ
        AirflowNetworkNodeReport(i)%PZON = 0.0d0
      End Do
      Do I=1,AirflowNetworkNumOfSurfaces
        AirflowNetworkLinkReport1(I)%FLOW = AirflowNetworkLinkSimu(I)%FLOW
        AirflowNetworkLinkReport1(I)%FLOW2 = AirflowNetworkLinkSimu(I)%FLOW2
        AirflowNetworkLinkReport1(I)%VolFLOW = AirflowNetworkLinkSimu(I)%VolFLOW
        AirflowNetworkLinkReport1(I)%VolFLOW2 = AirflowNetworkLinkSimu(I)%VolFLOW2
        AirflowNetworkLinkReport1(I)%FLOWOFF = AirflowNetworkLinkSimu(I)%FLOW
        AirflowNetworkLinkReport1(I)%FLOW2OFF = AirflowNetworkLinkSimu(I)%FLOW2
        AirflowNetworkLinkReport1(I)%VolFLOWOFF = AirflowNetworkLinkSimu(I)%VolFLOW
        AirflowNetworkLinkReport1(I)%VolFLOW2OFF = AirflowNetworkLinkSimu(I)%VolFLOW2
        AirflowNetworkLinkReport1(I)%DP = AirflowNetworkLinkSimu(I)%DP
        AirflowNetworkLinkReport1(I)%DPOFF = AirflowNetworkLinkSimu(I)%DP
        AirflowNetworkLinkReport1(I)%DPON = 0.0d0
      End Do
    End If
  End If

  if (.NOT. (AirflowNetworkFanActivated .and. SimulateAirflowNetwork > AirflowNetworkControlMultizone)) RETURN

  If (SimulateAirflowNetwork > AirflowNetworkControlMultizone+1) then
    Do i=1, AirflowNetworkNumOfSurfaces ! Multizone airflow energy
      N = AirflowNetworkLinkageData(i)%NodeNums(1)
      M = AirflowNetworkLinkageData(i)%NodeNums(2)
      ZN1 = AirflowNetworkNodeData(N)%EPlusZoneNum
      ZN2 = AirflowNetworkNodeData(M)%EPlusZoneNum
      ! Find a linkage from a zone to outdoors
      If (ZN1 > 0 .and. ZN2 == 0) then
        Tamb = Zone(ZN1)%OutDryBulbTemp
        CpAir   = PsyCpAirFnWTdb(OutHumRat, Tamb)
        AirflowNetworkExchangeData(ZN1)%MultiZoneSen = AirflowNetworkExchangeData(ZN1)%MultiZoneSen + &
          AirflowNetworkLinkSimu(i)%FLOW2*CpAir*(Tamb-ANZT(ZN1))
        AirflowNetworkExchangeData(ZN1)%MultiZoneLat = AirflowNetworkExchangeData(ZN1)%MultiZoneLat + &
          AirflowNetworkLinkSimu(i)%FLOW2*(OutHumRat-ANZW(ZN1))
      end if
      If (ZN1 == 0 .and. ZN2 > 0) then
        Tamb = Zone(ZN2)%OutDryBulbTemp
        CpAir   = PsyCpAirFnWTdb(OutHumRat, Tamb)
        AirflowNetworkExchangeData(ZN2)%MultiZoneSen = AirflowNetworkExchangeData(ZN2)%MultiZoneSen + &
          AirflowNetworkLinkSimu(i)%FLOW*CpAir*(Tamb-ANZT(ZN2))
        AirflowNetworkExchangeData(ZN2)%MultiZoneLat = AirflowNetworkExchangeData(ZN2)%MultiZoneLat + &
          AirflowNetworkLinkSimu(i)%FLOW*(OutHumRat-ANZW(ZN2))
      end if

      If (ZN1 > 0 .and. ZN2 > 0) then
        If (AirflowNetworkLinkSimu(i)%FLOW .GT. 0) then ! Flow from ZN1 to ZN2
          CpAir   = PsyCpAirFnWTdb(ANZW(ZN1), ANZT(ZN1))
          AirflowNetworkExchangeData(ZN2)%MultiZoneSen = AirflowNetworkExchangeData(ZN2)%MultiZoneSen + &
            AirflowNetworkLinkSimu(i)%FLOW*CpAir*(ANZT(ZN1)-ANZT(ZN2))
          AirflowNetworkExchangeData(ZN2)%MultiZoneLat = AirflowNetworkExchangeData(ZN2)%MultiZoneLat + &
            AirflowNetworkLinkSimu(i)%FLOW*(ANZW(ZN1)-ANZW(ZN2))
          CpAir   = PsyCpAirFnWTdb(ANZW(ZN2), ANZT(ZN2))
          AirflowNetworkExchangeData(ZN1)%MultiZoneSen = AirflowNetworkExchangeData(ZN1)%MultiZoneSen + &
            ABS(AirflowNetworkLinkSimu(i)%FLOW2)*CpAir*(ANZT(ZN2)-ANZT(ZN1))
          AirflowNetworkExchangeData(ZN1)%MultiZoneLat = AirflowNetworkExchangeData(ZN1)%MultiZoneLat + &
            ABS(AirflowNetworkLinkSimu(i)%FLOW2)*(ANZW(ZN2)-ANZW(ZN1))
        Else
!          CpAir   = PsyCpAirFnWTdb(ZoneAirHumRat(ZN2), MAT(ZN2))
!          AirflowNetworkExchangeData(ZN1)%MultiZoneSen = AirflowNetworkExchangeData(ZN1)%MultiZoneSen + &
!            AirflowNetworkLinkSimu(i)%FLOW*CpAir*(MAT(ZN1)-MAT(ZN2))
!          AirflowNetworkExchangeData(ZN1)%MultiZoneLat = AirflowNetworkExchangeData(ZN1)%MultiZoneLat + &
!            AirflowNetworkLinkSimu(i)%FLOW*(ZoneAirHumRat(ZN1)-ZoneAirHumRat(ZN2))
!          CpAir   = PsyCpAirFnWTdb(ZoneAirHumRat(ZN1), MAT(ZN1))
!          AirflowNetworkExchangeData(ZN2)%MultiZoneSen = AirflowNetworkExchangeData(ZN2)%MultiZoneSen + &
!            ABS(AirflowNetworkLinkSimu(i)%FLOW2)*CpAir*(MAT(ZN1)-MAT(ZN2))
!          AirflowNetworkExchangeData(ZN2)%MultiZoneLat = AirflowNetworkExchangeData(ZN2)%MultiZoneLat + &
!            ABS(AirflowNetworkLinkSimu(i)%FLOW2)*(ZoneAirHumRat(ZN1)-ZoneAirHumRat(ZN2))
          CpAir   = PsyCpAirFnWTdb(ANZW(ZN2), ANZT(ZN2))
          AirflowNetworkExchangeData(ZN1)%MultiZoneSen = AirflowNetworkExchangeData(ZN1)%MultiZoneSen + &
            ABS(AirflowNetworkLinkSimu(i)%FLOW2)*CpAir*(ANZT(ZN2)-ANZT(ZN1))
          AirflowNetworkExchangeData(ZN1)%MultiZoneLat = AirflowNetworkExchangeData(ZN1)%MultiZoneLat + &
            ABS(AirflowNetworkLinkSimu(i)%FLOW2)*(ANZW(ZN2)-ANZW(ZN1))
        End If
      end if
    End Do
  End If

  PartLoadRatio = 1.0d0
  OnOffFanRunTimeFraction = 1.0d0
  ! Calculate the part load ratio, can't be greater than 1 for a simple ONOFF fan
  If (SupplyFanType .EQ. FanType_SimpleOnOff .AND. Node(SupplyFanInletNode)%MassFlowRate > VerySmallMassFlow .AND. &
      LoopFanOperationMode .EQ. CycFanCycCoil) then
    PartLoadRatio= LoopOnOffFanPartLoadRatio
    OnOffFanRunTimeFraction = Max(LoopHeatingCoilMaxRTF,LoopONOffFanRTF,LoopDXCoilRTF)
  End If
  LoopHeatingCoilMaxRTF = 0.0d0

  If (SupplyFanType .EQ. FanType_SimpleOnOff .AND. PartLoadRatio < 1.0d0) then
    AirflowNetworkLinkReport%FLOW = AirflowNetworkLinkSimu%FLOW*PartLoadRatio
    AirflowNetworkLinkReport%FLOW2 = AirflowNetworkLinkSimu%FLOW2*PartLoadRatio
    AirflowNetworkLinkReport%VolFlow = AirflowNetworkLinkSimu%VolFlow*PartLoadRatio
    AirflowNetworkLinkReport%VolFlow2 = AirflowNetworkLinkSimu%VolFlow2*PartLoadRatio
  End If

  ! One time warning
  If (MyOneTimeFlag) then
    If (SupplyFanType .EQ. FanType_SimpleOnOff .AND. LoopFanOperationMode .EQ. ContFanCycCoil) then
      OnOffRatio = ABS((LoopSystemOnMassFlowrate-LoopSystemOffMassFlowrate)/LoopSystemOnMassFlowrate)
      If (OnOffRatio > 0.1d0) then
        CALL ShowWarningError('The absolute percent difference of supply air mass flow rate between HVAC operation ' &
          //'and No HVAC operation is above 10% with fan operation mode = ContFanCycCoil.')
        CALL ShowContinueError('The added zone loads using the AirflowNetwork model may not be accurate,' &
          //'because the zone loads are calculated based on the mass flow rate during HVAC operation.')
        CALL ShowContinueError('The mass flow rate during HVAC operation = '//TRIM(RoundSigDigits(LoopSystemOnMassFlowrate,2)) &
          //' The mass flow rate during no HVAC operation = '//TRIM(RoundSigDigits(LoopSystemOffMassFlowrate,2)))
        MyOneTimeFlag = .FALSE.
      End If
    End If
  End If

  ! Check mass flow differences in the zone inlet zones and splitter nodes between node and AFN links
  If (MyOneTimeFlag1) then
    If ((.NOT. VAVSystem) .AND. DisplayExtraWarnings) Then
      WriteFlag = .FALSE.
      Do I=1,AirflowNetworkNumOfLinks
        Node1 = AirflowNetworkLinkageData(i)%NodeNums(1)
        Node2 = AirflowNetworkLinkageData(i)%NodeNums(2)
        If (AirflowNetworkNodeData(Node1)%EPlusTypeNum == EPlusTypeNum_SPI .OR. &
            AirflowNetworkNodeData(Node2)%EPlusTypeNum == EPlusTypeNum_SPO .OR. &
            AirflowNetworkNodeData(Node2)%EPlusTypeNum == EPlusTypeNum_ZIN) then
          If (AirflowNetworkNodeData(Node1)%EPlusTypeNum == EPlusTypeNum_SPI) Then
            Node3 = Node1
          Else
            Node3 = Node2
          End If
          IF (AirflowNetworkNodeData(Node2)%EPlusTypeNum == EPlusTypeNum_ZIN) Then
            If (AirflowNetworkCompData(AirflowNetworkLinkageData(i)%CompNum)%EPlusTypeNum == 0) Cycle
          End If
          NodeMass = Node(AirflowNetworkNodeData(Node3)%EPlusNodeNum)%MassFlowRate
          AFNMass = AirflowNetworkLinkSimu(I)%FLOW
          If (NodeMass .GT. 0.0 .AND. AFNMass .GT. NodeMass + 0.01d0) Then
            CALL ShowWarningError('The mass flow rate difference is found between System Node = ' // &
               Trim(NodeID(AirflowNetworkNodeData(Node3)%EPlusNodeNum)) &
               //' and AFN Link = ' // Trim(AirflowNetworkLinkageData(I)%Name)//'.')
            CALL ShowContinueError('The system node max mass flow rate = '//TRIM(RoundSigDigits(NodeMass,3)) &
               //' kg/s. The AFN node mass flow rate = '//TRIM(RoundSigDigits(AFNMass,3)) // ' kg.s.')
               WriteFlag = .TRUE.
          End If
        End If
      End Do
      MyOneTimeFlag1 = .FALSE.
      If (WriteFlag) Then
        CALL ShowWarningError('Please adjust the rate of Maximum Air Flow Rate field in the terminal objects or ' // &
                        'duct pressure resistance.')
      End If
    Else
      MyOneTimeFlag1 = .FALSE.
    End If
  End If

  ! Assign airflows to EPLus nodes
  DO I=1,AirflowNetworkNumOfLinks
     if (AirflowNetworkCompData(AirflowNetworkLinkageData(i)%CompNum)%CompTypeNum .EQ. CompTypeNum_DWC .OR. &
       AirflowNetworkLinkageData(i)%VAVTermDamper) then
       ! Exclude envelope leakage Crack element
        Node1 = AirflowNetworkLinkageData(i)%NodeNums(1)
        Node2 = AirflowNetworkLinkageData(i)%NodeNums(2)

        j=AirflowNetworkNodeData(Node1)%EPlusNodeNum
        if (j > 0 .AND. AirflowNetworkNodeData(Node1)%EPlusZoneNum .EQ. 0) then
           Node(j)%MassFlowRate = AirflowNetworkLinkSimu(I)%FLOW*PartLoadRatio
           If (.NOT. (AirflowNetworkNodeData(Node1)%EPlusTypeNum == EPlusTypeNum_DIN .OR. &
             AirflowNetworkNodeData(Node1)%EPlusTypeNum == EPlusTypeNum_DOU)) Then
             Node(j)%MassFlowRateMaxAvail = AirflowNetworkLinkSimu(I)%FLOW*PartLoadRatio
             Node(j)%MassFlowRateMax = AirflowNetworkLinkSimu(I)%FLOW
           End If
        end if

        j=AirflowNetworkNodeData(Node2)%EPlusNodeNum
        if (j > 0) then
           Node(j)%MassFlowRate = AirflowNetworkLinkSimu(I)%FLOW*PartLoadRatio
           If (.NOT. (AirflowNetworkNodeData(Node2)%EPlusTypeNum == EPlusTypeNum_DIN .OR. &
             AirflowNetworkNodeData(Node2)%EPlusTypeNum == EPlusTypeNum_DOU)) Then
             Node(j)%MassFlowRateMaxAvail = AirflowNetworkLinkSimu(I)%FLOW*PartLoadRatio
             Node(j)%MassFlowRateMax = AirflowNetworkLinkSimu(I)%FLOW
           End If
        end if
     end if
  END DO

! Assign AirflowNetwork nodal values to Node array
  DO I=1,AirflowNetworkNumOfNodes
     j=AirflowNetworkNodeData(I)%EPlusNodeNum
     if (j > 0) then
        Node(j)%Enthalpy = PsyHFnTdbW(AirflowNetworkNodeSimu(I)%TZ, AirflowNetworkNodeSimu(I)%WZ)
        Node(j)%Temp = AirflowNetworkNodeSimu(I)%TZ
        Node(j)%HumRat = AirflowNetworkNodeSimu(I)%WZ
        IF (Contaminant%CO2Simulation) THEN
          Node(j)%CO2 = AirflowNetworkNodeSimu(I)%CO2Z
        END IF
        IF (Contaminant%GenericContamSimulation) THEN
          Node(j)%GenContam = AirflowNetworkNodeSimu(I)%GCZ
        END IF
     end if
  END DO

! Calculate sensible loads from forced air flow
  DO I=1,AirflowNetworkNumOfLinks
    Node1 = AirflowNetworkLinkageData(i)%NodeNums(1)
    Node2 = AirflowNetworkLinkageData(i)%NodeNums(2)
    CpAir = PsyCpAirFnWTdb((AirflowNetworkNodeSimu(Node1)%WZ+AirflowNetworkNodeSimu(Node2)%WZ)/2.0d0, &
                           (AirflowNetworkNodeSimu(Node1)%TZ+AirflowNetworkNodeSimu(Node2)%TZ)/2.0d0)
    ! Calculate sensible loads from duct conduction losses
    if (AirflowNetworkLinkageData(i)%ZoneNum > 0 .AND. &
        AirflowNetworkCompData(AirflowNetworkLinkageData(i)%CompNum)%CompTypeNum == CompTypeNum_DWC) then
      Qsen = AirflowNetworkLinkSimu(I)%FLOW*CpAir*(AirflowNetworkNodeSimu(Node2)%TZ-AirflowNetworkNodeSimu(Node1)%TZ)
      AirflowNetworkExchangeData(AirflowNetworkLinkageData(i)%ZoneNum)%CondSen = &
                                 AirflowNetworkExchangeData(AirflowNetworkLinkageData(i)%ZoneNum)%CondSen-Qsen
    end if
    ! Calculate sensible leakage losses
    if (AirflowNetworkCompData(AirflowNetworkLinkageData(i)%CompNum)%CompTypeNum == CompTypeNum_PLR .OR. &
        AirflowNetworkCompData(AirflowNetworkLinkageData(i)%CompNum)%CompTypeNum == CompTypeNum_ELR) then
      ! Calculate supply leak sensible losses
      if ((AirflowNetworkNodeData(Node2)%EPlusZoneNum > 0) .AND. &
         (AirflowNetworkNodeData(Node1)%EPlusNodeNum == 0) .AND. &
         (AirflowNetworkLinkSimu(I)%FLOW .GT. 0.0d0)) then
        ZN2=AirflowNetworkNodeData(Node2)%EPlusZoneNum
        Qsen = AirflowNetworkLinkSimu(I)%FLOW*CpAir*(AirflowNetworkNodeSimu(Node1)%TZ-AirflowNetworkNodeSimu(Node2)%TZ)
        AirflowNetworkExchangeData(ZN2)%LeakSen = AirflowNetworkExchangeData(ZN2)%LeakSen+Qsen
      end if
      if ((AirflowNetworkNodeData(Node1)%EPlusZoneNum > 0) .AND. &
          (AirflowNetworkNodeData(Node2)%EPlusNodeNum == 0) .AND. &
          (AirflowNetworkLinkSimu(I)%FLOW2 .GT. 0.0d0)) then
        ZN1=AirflowNetworkNodeData(Node1)%EPlusZoneNum
        Qsen = AirflowNetworkLinkSimu(I)%FLOW2*CpAir*(AirflowNetworkNodeSimu(Node2)%TZ-AirflowNetworkNodeSimu(Node1)%TZ)
        AirflowNetworkExchangeData(ZN1)%LeakSen = AirflowNetworkExchangeData(ZN1)%LeakSen+Qsen
      end if
    end if
  END DO

! Calculate latent loads from forced air flow
  DO I=1,AirflowNetworkNumOfLinks
    Node1 = AirflowNetworkLinkageData(i)%NodeNums(1)
    Node2 = AirflowNetworkLinkageData(i)%NodeNums(2)
    ! Calculate latent loads from duct conduction losses
    if (AirflowNetworkLinkageData(i)%ZoneNum > 0 .AND. &
        AirflowNetworkCompData(AirflowNetworkLinkageData(i)%CompNum)%CompTypeNum == CompTypeNum_DWC) then
      Qlat = AirflowNetworkLinkSimu(I)%FLOW*(AirflowNetworkNodeSimu(Node2)%WZ-AirflowNetworkNodeSimu(Node1)%WZ)
      AirflowNetworkExchangeData(AirflowNetworkLinkageData(i)%ZoneNum)%DiffLat = &
                                 AirflowNetworkExchangeData(AirflowNetworkLinkageData(i)%ZoneNum)%DiffLat-Qlat
    end if
    ! Calculate latent leakage losses
    if (AirflowNetworkCompData(AirflowNetworkLinkageData(i)%CompNum)%CompTypeNum == CompTypeNum_PLR .OR. &
        AirflowNetworkCompData(AirflowNetworkLinkageData(i)%CompNum)%CompTypeNum == CompTypeNum_ELR) then
      ! Calculate supply leak latent losses
      if ((AirflowNetworkNodeData(Node2)%EPlusZoneNum > 0) .AND. &
          (AirflowNetworkNodeData(Node1)%EPlusNodeNum == 0) .AND. &
          (AirflowNetworkLinkSimu(I)%FLOW .GT. 0.0d0)) then
        ZN2=AirflowNetworkNodeData(Node2)%EPlusZoneNum
        Qlat = AirflowNetworkLinkSimu(I)%FLOW*(AirflowNetworkNodeSimu(Node1)%WZ-AirflowNetworkNodeSimu(Node2)%WZ)
        AirflowNetworkExchangeData(ZN2)%LeakLat = AirflowNetworkExchangeData(ZN2)%LeakLat+Qlat
        IF (Contaminant%CO2Simulation) Then
          AirflowNetworkExchangeData(ZN2)%TotalCO2 = AirflowNetworkExchangeData(ZN2)%TotalCO2+ &
             AirflowNetworkLinkSimu(I)%FLOW*(AirflowNetworkNodeSimu(Node1)%CO2Z-AirflowNetworkNodeSimu(Node2)%CO2Z)
        End If
        IF (Contaminant%GenericContamSimulation) Then
          AirflowNetworkExchangeData(ZN2)%TotalGC = AirflowNetworkExchangeData(ZN2)%TotalGC+ &
             AirflowNetworkLinkSimu(I)%FLOW*(AirflowNetworkNodeSimu(Node1)%GCZ-AirflowNetworkNodeSimu(Node2)%GCZ)
        End If
      end if
      if ((AirflowNetworkNodeData(Node1)%EPlusZoneNum > 0) .AND. &
          (AirflowNetworkNodeData(Node2)%EPlusNodeNum == 0) .AND. &
          (AirflowNetworkLinkSimu(I)%FLOW2 .GT. 0.0d0)) then
        ZN1=AirflowNetworkNodeData(Node1)%EPlusZoneNum
        Qlat = AirflowNetworkLinkSimu(I)%FLOW2*(AirflowNetworkNodeSimu(Node2)%WZ-AirflowNetworkNodeSimu(Node1)%WZ)
        AirflowNetworkExchangeData(ZN1)%LeakLat = AirflowNetworkExchangeData(ZN1)%LeakLat+Qlat
        IF (Contaminant%CO2Simulation) Then
          AirflowNetworkExchangeData(ZN1)%TotalCO2 = AirflowNetworkExchangeData(ZN1)%TotalCO2+ &
             AirflowNetworkLinkSimu(I)%FLOW2*(AirflowNetworkNodeSimu(Node2)%CO2Z-AirflowNetworkNodeSimu(Node1)%CO2Z)
        End If
        IF (Contaminant%GenericContamSimulation) Then
          AirflowNetworkExchangeData(ZN1)%TotalGC = AirflowNetworkExchangeData(ZN1)%TotalGC+ &
             AirflowNetworkLinkSimu(I)%FLOW2*(AirflowNetworkNodeSimu(Node2)%GCZ-AirflowNetworkNodeSimu(Node1)%GCZ)
        End If
      end if
    end if
  END DO

  ! Sum all the loads
  DO I=1, NumOfZones
     AirflowNetworkExchangeData(i)%TotalSen =  &
             AirflowNetworkExchangeData(i)%LeakSen+AirflowNetworkExchangeData(i)%CondSen
     AirflowNetworkExchangeData(i)%TotalLat =  &
             AirflowNetworkExchangeData(i)%LeakLat+AirflowNetworkExchangeData(i)%DiffLat
  END DO

  ! Simple ONOFF fan
  If (SupplyFanType .EQ. FanType_SimpleOnOff .AND. OnOffFanRunTimeFraction < 1.0d0) then
    DO I=1, NumOfZones
       AirflowNetworkExchangeData(i)%MultiZoneSen = AirflowNetworkExchangeData(i)%MultiZoneSen*OnOffFanRuntimeFraction
       AirflowNetworkExchangeData(i)%MultiZoneLat = AirflowNetworkExchangeData(i)%MultiZoneLat*OnOffFanRuntimeFraction
       AirflowNetworkExchangeData(i)%LeakSen = AirflowNetworkExchangeData(i)%LeakSen*OnOffFanRuntimeFraction
       AirflowNetworkExchangeData(i)%LeakLat = AirflowNetworkExchangeData(i)%LeakLat*OnOffFanRuntimeFraction
       AirflowNetworkExchangeData(i)%CondSen = AirflowNetworkExchangeData(i)%CondSen*OnOffFanRuntimeFraction
       AirflowNetworkExchangeData(i)%DiffLat = AirflowNetworkExchangeData(i)%DiffLat*OnOffFanRuntimeFraction
       AirflowNetworkExchangeData(i)%TotalSen = AirflowNetworkExchangeData(i)%TotalSen*OnOffFanRuntimeFraction
       AirflowNetworkExchangeData(i)%TotalLat = AirflowNetworkExchangeData(i)%TotalLat*OnOffFanRuntimeFraction
       AirflowNetworkExchangeData(i)%SumMCp = AirflowNetworkExchangeData(i)%SumMCp*OnOffFanRuntimeFraction
       AirflowNetworkExchangeData(i)%SumMCpT = AirflowNetworkExchangeData(i)%SumMCpT*OnOffFanRuntimeFraction
       AirflowNetworkExchangeData(i)%SumMHr = AirflowNetworkExchangeData(i)%SumMHr*OnOffFanRuntimeFraction
       AirflowNetworkExchangeData(i)%SumMHrW = AirflowNetworkExchangeData(i)%SumMHrW*OnOffFanRuntimeFraction
       AirflowNetworkExchangeData(i)%SumMMCp = AirflowNetworkExchangeData(i)%SumMMCp*OnOffFanRuntimeFraction
       AirflowNetworkExchangeData(i)%SumMMCpT = AirflowNetworkExchangeData(i)%SumMMCpT*OnOffFanRuntimeFraction
       AirflowNetworkExchangeData(i)%SumMMHr = AirflowNetworkExchangeData(i)%SumMMHr*OnOffFanRuntimeFraction
       AirflowNetworkExchangeData(i)%SumMMHrW = AirflowNetworkExchangeData(i)%SumMMHrW*OnOffFanRuntimeFraction
       IF (Contaminant%CO2Simulation) Then
         AirflowNetworkExchangeData(i)%SumMHrCO = AirflowNetworkExchangeData(i)%SumMHrCO*OnOffFanRuntimeFraction
         AirflowNetworkExchangeData(i)%SumMMHrCO = AirflowNetworkExchangeData(i)%SumMMHrCO*OnOffFanRuntimeFraction
       End If
       IF (Contaminant%GenericContamSimulation) Then
         AirflowNetworkExchangeData(i)%SumMHrGC = AirflowNetworkExchangeData(i)%SumMHrGC*OnOffFanRuntimeFraction
         AirflowNetworkExchangeData(i)%SumMMHrGC = AirflowNetworkExchangeData(i)%SumMMHrGC*OnOffFanRuntimeFraction
       End If
    END DO
    If (LoopFanOperationMode .EQ. CycFanCycCoil) Then
      DO I=1, NumOfZones
        AirflowNetworkExchangeData(i)%SumMCp = AirflowNetworkExchangeData(i)%SumMCp + &
                                              AirflowNetworkMultiExchangeData(i)%SumMCp*(1.0-OnOffFanRuntimeFraction)
        AirflowNetworkExchangeData(i)%SumMCpT = AirflowNetworkExchangeData(i)%SumMCpT + &
                                              AirflowNetworkMultiExchangeData(i)%SumMCpT*(1.0-OnOffFanRuntimeFraction)
        AirflowNetworkExchangeData(i)%SumMHr = AirflowNetworkExchangeData(i)%SumMHr + &
                                              AirflowNetworkMultiExchangeData(i)%SumMHr*(1.0-OnOffFanRuntimeFraction)
        AirflowNetworkExchangeData(i)%SumMHrW = AirflowNetworkExchangeData(i)%SumMHrW + &
                                              AirflowNetworkMultiExchangeData(i)%SumMHrW*(1.0-OnOffFanRuntimeFraction)
        AirflowNetworkExchangeData(i)%SumMMCp = AirflowNetworkExchangeData(i)%SumMMCp + &
                                              AirflowNetworkMultiExchangeData(i)%SumMMCp*(1.0-OnOffFanRuntimeFraction)
        AirflowNetworkExchangeData(i)%SumMMCpT = AirflowNetworkExchangeData(i)%SumMMCpT + &
                                              AirflowNetworkMultiExchangeData(i)%SumMMCpT*(1.0-OnOffFanRuntimeFraction)
        AirflowNetworkExchangeData(i)%SumMMHr = AirflowNetworkExchangeData(i)%SumMMHr + &
                                              AirflowNetworkMultiExchangeData(i)%SumMMHr*(1.0-OnOffFanRuntimeFraction)
        AirflowNetworkExchangeData(i)%SumMMHrW = AirflowNetworkExchangeData(i)%SumMMHrW + &
                                              AirflowNetworkMultiExchangeData(i)%SumMMHrW*(1.0-OnOffFanRuntimeFraction)
        IF (Contaminant%CO2Simulation) Then
          AirflowNetworkExchangeData(i)%SumMHrCO = AirflowNetworkExchangeData(i)%SumMHrCO + &
                                              AirflowNetworkMultiExchangeData(i)%SumMHrCO*(1.0-OnOffFanRuntimeFraction)
          AirflowNetworkExchangeData(i)%SumMMHrCO = AirflowNetworkExchangeData(i)%SumMMHrCO + &
                                              AirflowNetworkMultiExchangeData(i)%SumMMHrCO*(1.0-OnOffFanRuntimeFraction)
        End If
        IF (Contaminant%GenericContamSimulation) Then
          AirflowNetworkExchangeData(i)%SumMHrGC = AirflowNetworkExchangeData(i)%SumMHrGC + &
                                              AirflowNetworkMultiExchangeData(i)%SumMHrGC*(1.0-OnOffFanRuntimeFraction)
          AirflowNetworkExchangeData(i)%SumMMHrGC = AirflowNetworkExchangeData(i)%SumMMHrGC + &
                                              AirflowNetworkMultiExchangeData(i)%SumMMHrGC*(1.0-OnOffFanRuntimeFraction)
        End If
      END DO
    End If
  End If

  If (SupplyFanType .EQ. FanType_SimpleOnOff) then
    Do I=1,AirflowNetworkNumOfZones
      AirflowNetworkNodeReport(i)%PZ = AirflowNetworkNodeSimu(i)%PZ*PartLoadRatio + &
                                       AirflowNetworkNodeReport(i)%PZOFF*(1.0-PartLoadRatio)
      AirflowNetworkNodeReport(i)%PZON = AirflowNetworkNodeSimu(i)%PZ
    End Do
    Do I=1,AirflowNetworkNumOfSurfaces
      AirflowNetworkLinkReport1(I)%FLOW = AirflowNetworkLinkSimu(I)%FLOW*PartLoadRatio + &
                                       AirflowNetworkLinkReport1(I)%FLOWOFF*(1.0-PartLoadRatio)
      AirflowNetworkLinkReport1(I)%FLOW2 = AirflowNetworkLinkSimu(I)%FLOW2*PartLoadRatio + &
                                       AirflowNetworkLinkReport1(I)%FLOW2OFF*(1.0-PartLoadRatio)
      AirflowNetworkLinkReport1(I)%VolFLOW = AirflowNetworkLinkSimu(I)%VolFLOW*PartLoadRatio + &
                                       AirflowNetworkLinkReport1(I)%VolFLOWOFF*(1.0-PartLoadRatio)
      AirflowNetworkLinkReport1(I)%VolFLOW2 = AirflowNetworkLinkSimu(I)%VolFLOW2*PartLoadRatio + &
                                       AirflowNetworkLinkReport1(I)%VolFLOW2OFF*(1.0-PartLoadRatio)
      AirflowNetworkLinkReport1(I)%DP = AirflowNetworkLinkSimu(I)%DP*PartLoadRatio + &
                                       AirflowNetworkLinkReport1(I)%DPOFF*(1.0-PartLoadRatio)
      AirflowNetworkLinkReport1(I)%DPON = AirflowNetworkLinkSimu(I)%DP
    End Do
  End If

  RETURN
END SUBROUTINE UpdateAirflowNetwork

SUBROUTINE AirflowNetworkVentingControl (I,OpenFactor)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   April 2003
          !       MODIFIED       Feb 2004, FCW: allow venting control of interior window/door
          !       MODIFIED       Nov. 2005, LG: to fit the requirement for AirflowNetwork Model
          !       RE-ENGINEERED

          ! PURPOSE OF THIS SUBROUTINE:
          ! Determines the venting opening factor for an exterior or interior window or door
          ! as determined by the venting control method.

          ! METHODOLOGY EMPLOYED:na
          ! REFERENCES:na

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE DataSurfaces, ONLY: SurfaceWindow
  USE ThermalComfort, ONLY: ThermalComfortData

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   INTEGER, INTENT(IN) :: I             ! AirflowNetwork surface number
   REAL(r64), INTENT(OUT)   :: OpenFactor    ! Window or door opening factor (used to calculate airflow)

          ! SUBROUTINE PARAMETER DEFINITIONS:na
          ! INTERFACE BLOCK SPECIFICATIONS:na
          ! DERIVED TYPE DEFINITIONS:na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

   REAL(r64) VentTemp                 ! Venting temperature (C)
   REAL(r64) ZoneAirEnthalpy          ! Enthalpy of zone air (J/kg)
   REAL(r64) OpenFactorMult           ! Window/door opening modulation multiplier on venting open factor
   REAL(r64) DelTemp                  ! Inside-outside air temperature difference (K)
   REAL(r64) DelEnthal                ! Inside-outsdie air enthalpy difference (J/kg)
   INTEGER IZ                    ! AirflowNetwork zone number
   INTEGER ZoneNum               ! EnergyPlus zone number
   INTEGER SurfNum               ! Heat transfer surface number
   REAL(r64) LimValVentOpenFacMult    ! Limiting value of venting opening factor multiplier
   REAL(r64) LowerValInOutTempDiff    ! Lower value of inside/outside temperature difference for opening factor modulation
   REAL(r64) UpperValInOutTempDiff    ! Upper value of inside/outside temperature difference for opening factor modulation
   REAL(r64) LowerValInOutEnthalDiff  ! Lower value of inside/outside enthalpy difference for opening factor modulation
   REAL(r64) UpperValInOutEnthalDiff  ! Upper value of inside/outside enthalpy difference for opening factor modulation
   LOGICAL VentingAllowed        ! True if venting schedule allows venting
   INTEGER VentCtrlNum           ! Venting control strategy 1: Temperature contro; 2: Enthalpy control
   REAL(r64) VentingSchVal            ! Current time step value of venting schedule
   REAL(r64) Tamb                     ! Outdoor dry bulb temperature at surface centroid height
   INTEGER PeopleInd


   IF (MultizoneSurfaceData(I)%EMSOpenFactorActuated) Then ! EMS sets value to use
     OpenFactor = MultizoneSurfaceData(I)%EMSOpenFactor
     SurfNum = MultizoneSurfaceData(I)%SurfNum
     If (MultizoneSurfaceData(i)%Factor > 0.0D0) THEN
       SurfaceWindow(SurfNum)%VentingOpenFactorMultRep = OpenFactor / MultizoneSurfaceData(i)%Factor
     ELSE
       SurfaceWindow(SurfNum)%VentingOpenFactorMultRep = OpenFactor
     ENDIF
     RETURN
   ENDIF

   SurfNum = MultizoneSurfaceData(I)%SurfNum

   SurfaceWindow(SurfNum)%VentingOpenFactorMultRep = -1.0d0

   ! Get venting temperature and venting strategy for exterior window or door
   ! and determine whether venting is allowed

   SurfaceWindow(SurfNum)%VentingAvailabilityRep = 1.0d0
   VentingAllowed = .TRUE.
   IZ = MultizoneSurfaceData(I)%NodeNums(1)
   ZoneNum = MultizoneZoneData(IZ)%ZoneNum

   ! Note in the following that individual venting control for a window/door takes
   ! precedence over zone-level control
   If (MultizoneSurfaceData(I)%IndVentControl) then
     VentTemp = GetCurrentScheduleValue(MultizoneSurfaceData(I)%VentSchNum)
     VentCtrlNum = MultizoneSurfaceData(I)%VentSurfCtrNum
     If (MultizoneSurfaceData(I)%VentingSchNum > 0) then
       VentingSchVal = GetCurrentScheduleValue(MultizoneSurfaceData(I)%VentingSchNum)
       If (VentingSchVal <= 0.0d0) then
         VentingAllowed = .FALSE.
         SurfaceWindow(SurfNum)%VentingAvailabilityRep = 0.0d0
       End If
     End If
   Else
     ! Zone level only by Gu on Nov. 8, 2005
     VentTemp = GetCurrentScheduleValue(MultizoneZoneData(IZ)%VentSchNum)
     VentCtrlNum = MultizoneZoneData(Iz)%VentCtrNum
     If (MultizoneZoneData(IZ)%VentingSchNum > 0) then
       VentingSchVal = GetCurrentScheduleValue(MultizoneZoneData(IZ)%VentingSchNum)
       If (VentingSchVal <= 0.0d0) then
         VentingAllowed = .FALSE.
         SurfaceWindow(SurfNum)%VentingAvailabilityRep = 0.0d0
       End If
     End If
   End If

     SurfaceWindow(SurfNum)%InsideTempForVentingRep = VentTemp
     OpenFactor = 0.0d0

     ! Venting based on inside-outside air temperature difference

     if ((VentCtrlNum == VentCtrNum_Temp .or. VentCtrlNum == VentCtrNum_AdjTemp) .AND. VentingAllowed) then
       Tamb = Surface(SurfNum)%OutDryBulbTemp
       ! Check whether this surface is an interior wall or not. If Yes, use adjacent zone conditions
       If (VentCtrlNum == VentCtrNum_AdjTemp .and. MultizoneSurfaceData(I)%IndVentControl) then
         Tamb = ANZT(MultizoneZoneData(MultizoneSurfaceData(I)%NodeNums(2))%ZoneNum)
       End If
       if (ANZT(ZoneNum) > Tamb .AND. ANZT(ZoneNum) > VentTemp) then
         OpenFactor = MultizoneSurfaceData(i)%Factor
         SurfaceWindow(SurfNum)%VentingOpenFactorMultRep = 1.0d0
         ! Modulation of OpenFactor
         If (MultizoneSurfaceData(I)%IndVentControl) then
           LimValVentOpenFacMult = MultizoneSurfaceData(i)%ModulateFactor
           LowerValInOutTempDiff = MultizoneSurfaceData(i)%LowValueTemp
           UpperValInOutTempDiff = MultizoneSurfaceData(i)%UpValueTemp
         Else
           LimValVentOpenFacMult = MultizoneZoneData(IZ)%OpenFactor
           LowerValInOutTempDiff = MultizoneZoneData(IZ)%LowValueTemp
           UpperValInOutTempDiff = MultizoneZoneData(IZ)%UpValueTemp
         End If
         if(LimValVentOpenFacMult /= 1.0d0) then
           DelTemp = ANZT(ZoneNum)-Tamb
           if(DelTemp <= LowerValInOutTempDiff) then
             OpenFactorMult = 1.0d0
           else if (DelTemp >= UpperValInOutTempDiff) then
             OpenFactorMult = LimValVentOpenFacMult
           else
             OpenFactorMult = LimValVentOpenFacMult + &
               ((UpperValInOutTempDiff-DelTemp)/(UpperValInOutTempDiff-LowerValInOutTempDiff))* &
                (1-LimValVentOpenFacMult)
           endif
           OpenFactor = OpenFactorMult * OpenFactor
           SurfaceWindow(SurfNum)%VentingOpenFactorMultRep = OpenFactorMult
         end if
       else
         OpenFactor = 0.0d0
         SurfaceWindow(SurfNum)%VentingOpenFactorMultRep = -1.0d0
       endif
     endif

     ! Venting based on inside-outside air enthalpy difference

     if ((VentCtrlNum == VentCtrNum_Enth .or. VentCtrlNum == VentCtrNum_AdjEnth) .AND. VentingAllowed) then
       ZoneAirEnthalpy = PsyHFnTdbW(ANZT(ZoneNum),ANZW(ZoneNum))
       ! Check whether this surface is an interior wall or not. If Yes, use adjacent zone conditions
       If (VentCtrlNum == VentCtrNum_AdjEnth .AND. MultizoneSurfaceData(I)%IndVentControl) then
         OutEnthalpy = PsyHFnTdbW(ANZT(MultizoneZoneData(MultizoneSurfaceData(I)%NodeNums(2))%ZoneNum), &
                                  ANZW(MultizoneZoneData(MultizoneSurfaceData(I)%NodeNums(2))%ZoneNum))
       End If
       if (ZoneAirEnthalpy > OutEnthalpy .AND. ANZT(ZoneNum) > VentTemp) then
         OpenFactor = MultizoneSurfaceData(i)%Factor
         ! Modulation of OpenFactor
         If (MultizoneSurfaceData(I)%IndVentControl) then
           LimValVentOpenFacMult   = MultizoneSurfaceData(i)%ModulateFactor
           LowerValInOutEnthalDiff = MultizoneSurfaceData(i)%LowValueEnth
           UpperValInOutEnthalDiff = MultizoneSurfaceData(i)%UpValueEnth
         Else
           LimValVentOpenFacMult   = MultizoneZoneData(IZ)%OpenFactor
           LowerValInOutEnthalDiff = MultizoneZoneData(IZ)%LowValueEnth
           UpperValInOutEnthalDiff = MultizoneZoneData(IZ)%UpValueEnth
         End If
         SurfaceWindow(SurfNum)%VentingOpenFactorMultRep = 1.0d0

         if (LimValVentOpenFacMult /= 1.0d0) then
           DelEnthal = ZoneAirEnthalpy - OutEnthalpy
           if (DelEnthal <= LowerValInOutEnthalDiff) then
             OpenFactorMult = 1.0d0
           else if (DelEnthal >= UpperValInOutEnthalDiff) then
             OpenFactorMult = LimValVentOpenFacMult
           else
             OpenFactorMult = LimValVentOpenFacMult + &
               ((UpperValInOutEnthalDiff-DelEnthal)/(UpperValInOutEnthalDiff-LowerValInOutEnthalDiff))* &
                (1-LimValVentOpenFacMult)
           endif
           OpenFactor = OpenFactorMult * OpenFactor
           SurfaceWindow(SurfNum)%VentingOpenFactorMultRep = OpenFactorMult
         end if
       else
         OpenFactor = 0.0d0
         SurfaceWindow(SurfNum)%VentingOpenFactorMultRep = -1.0d0
       endif
     endif

     ! Constant venting (opening factor as specified in IDF) - C-PH - added by Philip Haves 3/8/01
     ! subject to venting availability

     if (VentCtrlNum == VentCtrNum_Const .AND. VentingAllowed) then ! Constant
       OpenFactor = MultizoneSurfaceData(i)%Factor
       SurfaceWindow(SurfNum)%VentingOpenFactorMultRep = 1.0d0
     endif

     IF (VentCtrlNum == VentCtrNum_ASH55) THEN
       IF (VentingAllowed .AND. (.NOT. BeginEnvrnFlag) .AND. (.NOT. WarmupFlag)) THEN
         PeopleInd = MultizoneZoneData(IZ)%ASH55PeopleInd
         IF (PeopleInd > 0 .AND. ThermalComfortData(PeopleInd)%ThermalComfortAdaptiveASH5590 /= -1 ) THEN
           IF (ThermalComfortData(PeopleInd)%ThermalComfortOpTemp > ThermalComfortData(PeopleInd)%TComfASH55) THEN
             OpenFactor = MultizoneSurfaceData(i)%Factor
             SurfaceWindow(SurfNum)%VentingOpenFactorMultRep = 1.0d0
           ELSE
             OpenFactor = 0.0d0
           END IF
         ELSE
           OpenFactor = 0.0d0
         END IF
       ELSE
         OpenFactor = 0.0d0
       END IF
     END IF

     IF (VentCtrlNum == VentCtrNum_CEN15251) THEN
       IF (VentingAllowed .AND. (.NOT. BeginEnvrnFlag) .AND. (.NOT. WarmupFlag)) THEN
         PeopleInd = MultizoneZoneData(IZ)%CEN15251PeopleInd
         IF (PeopleInd > 0 .AND. ThermalComfortData(PeopleInd)%ThermalComfortAdaptiveCEN15251CatI /= -1 ) THEN
           IF (ThermalComfortData(PeopleInd)%ThermalComfortOpTemp > ThermalComfortData(PeopleInd)%TComfCEN15251) THEN
             OpenFactor = MultizoneSurfaceData(i)%Factor
             SurfaceWindow(SurfNum)%VentingOpenFactorMultRep = 1.0d0
           ELSE
             OpenFactor = 0.0d0
           END IF
         ELSE
           OpenFactor = 0.0d0
         END IF
       ELSE
         OpenFactor = 0.0d0
       END IF
     END IF

     ! No venting, i.e, window/door always closed - added YJH 8 Aug 02

     if (VentCtrlNum == VentCtrNum_Novent ) then ! Novent
       OpenFactor = 0.0d0
       SurfaceWindow(SurfNum)%VentingOpenFactorMultRep = -1.0d0
     endif

  RETURN
END SUBROUTINE AirflowNetworkVentingControl

SUBROUTINE ValidateDistributionSystem

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Oct. 2005
          !       MODIFIED       L. Gu, Jan. 2009: allow a desuperheater coil and three heat exchangers
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine validates the inputs of distribution system, since node data from a pimary airloop
          ! are nor available in the first call during reading input data of airflownetwrok objects.


          ! METHODOLOGY EMPLOYED:
          ! na


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
  Use DataAirLoop , Only: AirToZoneNodeInfo,AirToOANodeInfo
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  Use MixedAir, Only: GetNumOAMixers, GetOAMixerReliefNodeNumber, GetOAMixerInletNodeNumber
  USE HeatingCoils, ONLY: HeatingCoil, NumHeatingCoils
  USE SingleDuct, ONLY: GetHVACSingleDuctSysIndex
  USE InputProcessor, ONLY: SameString,MakeUPPERCase ! NEEDS TO BE CHANGED AFTER V1.3 RELEASE
  USE BranchNodeConnections, ONLY: GetNodeConnectionType
  USE DataLoopNode
  USE DataBranchNodeConnections, ONLY: NodeConnections,NumOfNodeConnections
  USE ZoneDehumidifier, ONLY: GetZoneDehumidifierNodeNumber
  USE SplitterComponent, ONLY: GetSplitterNodeNumbers, GetSplitterOutletNumber

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na


          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER (len=*), PARAMETER   :: RoutineName='ValidateDistributionSystem: ' ! include trailing blank space


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
integer I,j,k,N, S1,S2,R1,R2
LOGICAL,SAVE :: OneTimeFlag = .True.
LOGICAL :: ErrorsFound=.false.
LOGICAL LocalError
LOGICAL, ALLOCATABLE, DIMENSION(:) :: NodeFound
REAL(r64) FanFlow
LOGICAL :: IsNotOk=.false.
LOGICAL :: ErrFlag=.false.
INTEGER, ALLOCATABLE, DIMENSION(:) :: NodeConnectionType ! Specifies the type of node connection
CHARACTER(len=MaxNameLength) :: CurrentModuleObject


! Validate supply and return connections
if (OneTimeFlag) then
  Allocate(NodeFound(NumOfNodes))
  NodeFound = .False.
! Validate inlet and outlet nodes for zone exhaust fans
  Do i=1,AirflowNetworkNumOfExhFan
    NodeFound(MultizoneCompExhaustFanData(i)%InletNode) = .True.
    NodeFound(MultizoneCompExhaustFanData(i)%OutletNode) = .True.
  End Do
! Validate EPlus Node names and types
  do i=1,DisSysNumOfNodes
    if (SameString(DisSysNodeData(i)%EPlusName,' ') .or. &
        SameString(DisSysNodeData(i)%EPlusName,'Other')) cycle
    LocalError=.false.
    do j=1,NumOfNodes ! NodeID
      if (DisSysNodeData(i)%EPlusName == NodeID(j)) then
        DisSysNodeData(i)%EPlusNodeNum = j
        AirflowNetworkNodeData(NumOfNodesMultiZone+i)%EPlusNodeNum = j
        NodeFound(j) = .True.
        LocalError=.True.
        Exit
      end if
    end do
    ! Check outdoor air node
    If (SameString(DisSysNodeData(i)%EPlusType,'OutdoorAir:NodeList') .OR. &
        SameString(DisSysNodeData(i)%EPlusType,'OutdoorAir:Node')) then
      If (.NOT. LocalError) Then
        CALL ShowSevereError(RoutineName //'The Node or Component Name defined in '//Trim(DisSysNodeData(i)%Name) // &
             ' is not found in the '//Trim(DisSysNodeData(i)%EPlusType))
        CALL ShowContinueError('The entered name is '//Trim(DisSysNodeData(i)%EPlusName) &
                               //' in an AirflowNetwork:Distribution:Node object.')
        ErrorsFound=.true.
      End If
    End If
    if (DisSysNodeData(i)%EPlusNodeNum .EQ. 0) then
      CALL ShowSevereError(RoutineName//'Primary Air Loop Node is not found in AIRFLOWNETWORK:DISTRIBUTION:NODE = ' &
                           //TRIM(DisSysNodeData(i)%Name))
      ErrorsFound=.true.
    end if
  end do

! Determine node numbers for zone inlets
  DO i=1,NumOfZones
    IF (.not. ZoneEquipConfig(i)%IsControlled) CYCLE
    Do j=1, ZoneEquipConfig(i)%NumInletNodes
      Do k=1,AirflowNetworkNumOfNodes
        If (ZoneEquipConfig(i)%InletNode(j) == AirflowNetworkNodeData(k)%EPlusNodeNum) then
           AirflowNetworkNodeData(k)%EPlusTypeNum = EPlusTypeNum_ZIN
           Exit
        End If
      end do
    End Do
  end do

  ! Eliminate node not related to AirLoopHVAC
  DO k=1,NumOfNodeConnections
    If (NodeFound(NodeConnections(k)%NodeNumber)) cycle
    If (NodeConnections(k)%FluidStream == 2) then
      NodeFound(NodeConnections(k)%NodeNumber) = .True.
    End If
  End Do

  ! Eliminate nodes with fluidtype = water
  DO k=1,NumOfNodes
    If (NodeFound(k)) cycle
    If (Node(k)%FluidType == 2) then
      NodeFound(k) = .True.
    End If
  End Do

! Ensure all the nodes used in Eplus are a subset of AirflowNetwork Nodes
  Do i=1,NumOfNodes
    If (NodeFound(i)) cycle
    ! Skip the inlet and outlet nodes of zone dehumidifiers
    If (GetZoneDehumidifierNodeNumber(i)) NodeFound(i) = .TRUE.

    Do j=1,NumOfZones
      IF (.not. ZoneEquipConfig(j)%IsControlled) CYCLE
      If (ZoneEquipConfig(j)%ZoneNode == i) then
         NodeFound(i) = .True.
         AirflowNetworkNodeData(ZoneEquipConfig(j)%ActualZoneNum)%EPlusNodeNum = i
         Exit
      end if
    End Do

!   skip nodes that are not part of an airflow network

!     DX COIL CONDENSER NODE TEST:
!
!     Outside air nodes are used for DX coil condenser inlet nodes, these are specified in an outside air node or
!     OutdoorAir:NodeList object (and classified with NodeConnectionType as OutsideAir). In addition,
!     this same node is specified in a Coil:DX:CoolingBypassFactorEmpirical object (and classified with
!     NodeConnectionType as OutsideAirReference). In the NodeConnectionType structure, both of these nodes have a
!     unique index but have the same node number. The Outside Air Node will usually be listed first. Search for all
!     indexs with the same node number and check if it is classified as NodeConnectionType = OutsideAirReference.
!     Mark this node as found since it is not used in an airflownetwork simulation.
!
!     Example (using AirflowNetwork_MultiZone_SmallOffice.idf with a single OA Mixer):
!             (the example shown below is identical to AirflowNetwork_SimpleHouse.idf with no OA Mixer except
!              that the NodeConnections indexs are (7) and (31), respectively and the NodeNumber = 6)
!
!   The GetNodeConnectionType CALL below returns NodeConnectionType_OutsideAir = 7 and NodeConnectionType_OutsideAirReference = 14.
!
!     NodeConnections info from OUTSIDE AIR NODE object read:
!     NodeConnections(9)NodeNumber      = 10
!     NodeConnections(9)NodeName        = ACDXCOIL 1 CONDENSER NODE
!     NodeConnections(9)ObjectType      = OUTSIDE AIR NODE
!     NodeConnections(9)ObjectName      = OUTSIDE AIR NODE
!     NodeConnections(9)ConnectionType  = OutsideAir
!
!     NodeConnections info from Coil:DX:CoolingBypassFactorEmpirical object read:
!     NodeConnections(64)NodeNumber     = 10
!     NodeConnections(64)NodeName       = ACDXCOIL 1 CONDENSER NODE
!     NodeConnections(64)ObjectType     = COIL:DX:COOLINGBYPASSFACTOREMPIRICAL
!     NodeConnections(64)ObjectName     = ACDXCOIL 1
!     NodeConnections(64)ConnectionType = OutsideAirReference


    ErrFlag = .FALSE.
    CALL GetNodeConnectionType(i, NodeConnectionType, ErrFlag) ! Gets all connection types for a given node number
    IF(ErrFlag)THEN
      CALL ShowContinueError('...occurs in Airflow Network simulation.')
    ELSE
!   skip nodes for air cooled condensers
      DO j = 1, SIZE(NodeConnectionType)
        if (NodeConnectionType(j) .EQ. NodeConnectionType_OutsideAirReference) then
          NodeFound(i) = .TRUE.
        end if
      END DO
    END IF

    If (.NOT. NodeFound(i)) then
      ! Check if this node is the OA relief node. For the time being, OA relief node is not used
      If (GetNumOAMixers() .GT. 1) then
        CALL ShowSevereError(RoutineName//'Only one OutdoorAir:Mixer is allowed in the AirflowNetwork model.')
        ErrorsFound=.true.
      ElseIf (GetNumOAMixers() .EQ. 0) Then
        CALL ShowSevereError(RoutineName //Trim(NodeID(I)) //' is not defined as an ' &
                               //'AirflowNetwork:Distribution:Node object.')
        ErrorsFound=.true.
      Else
        if (i .EQ. GetOAMixerReliefNodeNumber(1)) then
          NodeFound(i) = .TRUE.
        ElseIf (i .EQ. GetOAMixerInletNodeNumber(1)) Then
          NodeFound(i) = .TRUE.
        Else
          CALL ShowSevereError(RoutineName //Trim(NodeID(I)) //' is not defined as an ' &
                               //'AirflowNetwork:Distribution:Node object.')
          ErrorsFound=.true.
        end if
      end if
    End If
  End Do
  Deallocate(NodeFound)

 ! Validate coil name and type
  CurrentModuleObject = 'AirflowNetwork:Distribution:Component:Coil'
  MultiSpeedHPIndicator = 0
  Do i=1,DisSysNumOfCoils
    SELECT CASE(MakeUPPERCase(DisSysCompCoilData(i)%EPlusType))

      CASE ('COIL:COOLING:DX:SINGLESPEED')
        CALL ValidateComponent('Coil:Cooling:DX:SingleSpeed',DisSysCompCoilData(i)%Name,IsNotOK,  &
                               RoutineName//TRIM(CurrentModuleObject))
        If (IsNotOk) then
          ErrorsFound=.true.
        end if

      CASE ('COIL:HEATING:DX:SINGLESPEED')
        CALL ValidateComponent('Coil:Heating:DX:SingleSpeed',DisSysCompCoilData(i)%Name,IsNotOK,  &
                               RoutineName//TRIM(CurrentModuleObject))
        If (IsNotOk) then
          ErrorsFound=.true.
        end if

      CASE ('COIL:HEATING:GAS')
        CALL ValidateComponent('Coil:Heating:Gas',DisSysCompCoilData(i)%Name,IsNotOK,  &
                               RoutineName//TRIM(CurrentModuleObject))
        If (IsNotOk) then
          ErrorsFound=.true.
        end if

      CASE ('COIL:HEATING:ELECTRIC')
        CALL ValidateComponent('Coil:Heating:Electric',DisSysCompCoilData(i)%Name,IsNotOK,  &
                               RoutineName//TRIM(CurrentModuleObject))
        If (IsNotOk) then
          ErrorsFound=.true.
        end if

      CASE ('COIL:COOLING:WATER')
        CALL ValidateComponent('Coil:Cooling:Water',DisSysCompCoilData(i)%Name,IsNotOK,  &
                               RoutineName//TRIM(CurrentModuleObject))
        If (IsNotOk) then
          ErrorsFound=.true.
        end if

      CASE ('COIL:HEATING:WATER')
        CALL ValidateComponent('Coil:Heating:Water',DisSysCompCoilData(i)%Name,IsNotOK,  &
                               RoutineName//TRIM(CurrentModuleObject))
        If (IsNotOk) then
          ErrorsFound=.true.
        end if

      CASE ('COIL:COOLING:WATER:DETAILEDGEOMETRY')
        CALL ValidateComponent('Coil:Cooling:Water:DetailedGeometry',DisSysCompCoilData(i)%Name,IsNotOK,  &
                               RoutineName//TRIM(CurrentModuleObject))
        If (IsNotOk) then
          ErrorsFound=.true.
        end if

      CASE ('COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE')
        CALL ValidateComponent('Coil:Cooling:DX:TwoStageWithHumidityControlMode',DisSysCompCoilData(i)%Name,IsNotOK,  &
                               RoutineName//TRIM(CurrentModuleObject))
        If (IsNotOk) then
          ErrorsFound=.true.
        end if

      CASE ('COIL:COOLING:DX:MULTISPEED')
        CALL ValidateComponent('Coil:Cooling:DX:MultiSpeed',DisSysCompCoilData(i)%Name,IsNotOK,  &
                               RoutineName//TRIM(CurrentModuleObject))
        MultiSpeedHPIndicator = MultiSpeedHPIndicator+1
        If (IsNotOk) then
          ErrorsFound=.true.
        end if

      CASE ('COIL:HEATING:DX:MULTISPEED')
        CALL ValidateComponent('Coil:Heating:DX:MultiSpeed',DisSysCompCoilData(i)%Name,IsNotOK,  &
                               RoutineName//TRIM(CurrentModuleObject))
        MultiSpeedHPIndicator = MultiSpeedHPIndicator+1
        If (IsNotOk) then
          ErrorsFound=.true.
        end if

      CASE ('COIL:HEATING:DESUPERHEATER')
        CALL ValidateComponent('Coil:Heating:Desuperheater',DisSysCompCoilData(i)%Name,IsNotOK,  &
                               RoutineName//TRIM(CurrentModuleObject))
        If (IsNotOk) then
          ErrorsFound=.true.
        end if

        CASE ('COIL:COOLING:DX:TWOSPEED')
        CALL ValidateComponent('Coil:Cooling:DX:TwoSpeed',DisSysCompCoilData(i)%Name,IsNotOK,  &
                               RoutineName//TRIM(CurrentModuleObject))
        If (IsNotOk) then
          ErrorsFound=.true.
        end if

      CASE DEFAULT
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' Invalid coil type = ' &
                             //DisSysCompCoilData(i)%Name)
        ErrorsFound=.true.
      END SELECT
  end do

! Validate ternimal unit name and type
  Do i=1,DisSysNumOfTermUnits
    if (SameString(DisSysCompTermUnitData(i)%EPlusType,'AirTerminal:SingleDuct:ConstantVolume:Reheat') .OR. &
        SameString(DisSysCompTermUnitData(i)%EPlusType,'AirTerminal:SingleDuct:VAV:Reheat') ) then
      LocalError=.false.
      If (SameString(DisSysCompTermUnitData(i)%EPlusType,'AirTerminal:SingleDuct:ConstantVolume:Reheat')) &
        CALL GetHVACSingleDuctSysIndex(DisSysCompTermUnitData(i)%Name,n,LocalError,  &
                     'AirflowNetwork:Distribution:Component:TerminalUnit')
      If (SameString(DisSysCompTermUnitData(i)%EPlusType,'AirTerminal:SingleDuct:VAV:Reheat')) &
        CALL GetHVACSingleDuctSysIndex(DisSysCompTermUnitData(i)%Name,n,LocalError,  &
                     'AirflowNetwork:Distribution:Component:TerminalUnit',DisSysCompTermUnitData(i)%DamperInletNode, &
                     DisSysCompTermUnitData(i)%DamperOutletNode)
      if (LocalError) ErrorsFound = .True.
      If (VAVSystem) Then
        If (.NOT. SameString(DisSysCompTermUnitData(i)%EPlusType,'AirTerminal:SingleDuct:VAV:Reheat')) Then
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' Invalid terminal type for a VAV system = ' &
                             //DisSysCompTermUnitData(i)%Name)
          CALL ShowContinueError('The input type = '//TRIM(DisSysCompTermUnitData(i)%EPlusType))
          CALL ShowContinueError('A VAV system requires all ternimal units with type = AirTerminal:SingleDuct:VAV:Reheat')
          ErrorsFound=.true.
        End If
      End If
    else
      CALL ShowSevereError(RoutineName//'AIRFLOWNETWORK:DISTRIBUTION:COMPONENT TERMINAL UNIT: ' &
                           //'Invalid Terminal unit type = '//DisSysCompTermUnitData(i)%Name)
      ErrorsFound=.true.
    end if
  end do

 ! Validate heat exchanger name and type
  CurrentModuleObject = 'AirflowNetwork:Distribution:Component:HeatExchanger'
  Do i=1,DisSysNumOfHXs
    SELECT CASE(MakeUPPERCase(DisSysCompHXData(i)%EPlusType))

      CASE ('HEATEXCHANGER:AIRTOAIR:FLATPLATE')
        CALL ValidateComponent('HeatExchanger:AirToAir:FlatPlate',DisSysCompHXData(i)%Name,IsNotOK,  &
                               RoutineName//TRIM(CurrentModuleObject))
        If (IsNotOk) then
          ErrorsFound=.true.
        end if

      CASE ('HEATEXCHANGER:AIRTOAIR:SENSIBLEANDLATENT')
        CALL ValidateComponent('HeatExchanger:AirToAir:SensibleAndLatent',DisSysCompHXData(i)%Name,IsNotOK,  &
                               RoutineName//TRIM(CurrentModuleObject))
        If (IsNotOk) then
          ErrorsFound=.true.
        end if

      CASE ('HEATEXCHANGER:DESICCANT:BALANCEDFLOW')
        CALL ValidateComponent('HeatExchanger:Desiccant:BalancedFlow',DisSysCompHXData(i)%Name,IsNotOK,  &
                               RoutineName//TRIM(CurrentModuleObject))
        If (IsNotOk) then
          ErrorsFound=.true.
        end if

      CASE DEFAULT
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' Invalid heat exchanger type = ' &
                             //DisSysCompHXData(i)%EPlusType)
        ErrorsFound=.true.
      END SELECT
  end do

! Assign supply and return connection
  S1 = 0
  S2 = 0
  R1 = 0
  R2 = 0
  Do I=1,AirflowNetworkNumOfNodes
    If (AirflowNetworkNodeData(i)%EPlusNodeNum .EQ. AirToZoneNodeInfo(1)%AirLoopSupplyNodeNum(1)) S1=I
    If (AirflowNetworkNodeData(i)%EPlusNodeNum .EQ. AirToZoneNodeInfo(1)%ZoneEquipSupplyNodeNum(1)) S2=I
    If (AirflowNetworkNodeData(i)%EPlusNodeNum .EQ. AirToZoneNodeInfo(1)%ZoneEquipReturnNodeNum(1)) R1=I
    If (AirflowNetworkNodeData(i)%EPlusNodeNum .EQ. AirToZoneNodeInfo(1)%AirLoopReturnNodeNum(1)) R2=I
  End Do
  Do i=1,AirflowNetworkNumOfLinks
    If (AirflowNetworkLinkageData(i)%NodeNums(1) .eq. R1 .and.AirflowNetworkLinkageData(i)%NodeNums(2) .eq. R2) then
      AirflowNetworkLinkageData(i)%ConnectionFlag = EPlusTypeNum_RCN
    end if
    If (AirflowNetworkLinkageData(i)%NodeNums(1) .eq. S1 .and.AirflowNetworkLinkageData(i)%NodeNums(2) .eq. S2) then
      AirflowNetworkLinkageData(i)%ConnectionFlag = EPlusTypeNum_SCN
    end if
  End Do

! Assign fan inlet and outlet node, and coil outlet
  Do i=1,AirflowNetworkNumOfLinks
    J = AirflowNetworkLinkageData(i)%CompNum
    If (AirflowNetworkCompData(J)%CompTypeNum == CompTypeNum_CVF) then
      If (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusTypeNum == 0) &
        AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusTypeNum = EPlusTypeNum_FIN
      AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusTypeNum = EPlusTypeNum_FOU
    end if
    If (AirflowNetworkCompData(J)%EPlusTypeNum == EPlusTypeNum_COI) then
      AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusTypeNum = EPlusTypeNum_COU
    end if
    If (AirflowNetworkCompData(J)%EPlusTypeNum == EPlusTypeNum_HEX) then
      AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusTypeNum = EPlusTypeNum_HXO
    end if
    If (AirflowNetworkCompData(J)%CompTypeNum == CompTypeNum_TMU) Then
      If (DisSysCompTermUnitData(AirflowNetworkCompData(j)%TypeNum)%DamperInletNode > 0) Then
        If (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusNodeNum == &
           DisSysCompTermUnitData(AirflowNetworkCompData(j)%TypeNum)%DamperInletNode .AND. &
           AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusNodeNum == &
           DisSysCompTermUnitData(AirflowNetworkCompData(j)%TypeNum)%DamperOutletNode) Then
          AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusTypeNum = EPlusTypeNum_DIN
          AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusTypeNum = EPlusTypeNum_DOU
          AirflowNetworkLinkageData(i)%VAVTermDamper = .TRUE.
        End If
      End If
    End If
  End Do

  ! Validate the position of constant pressure drop component
  CurrentModuleObject = 'AirflowNetwork:Distribution:Component:ConstantPressureDrop'
  do i=1,AirflowNetworkNumOfLinks
    If (AirflowNetworkCompData(AirflowNetworkLinkageData(i)%CompNum)%CompTypeNum == CompTypeNum_CPD) then
      Do j=1,AirflowNetworkNumOfLinks
        If (AirflowNetworkLinkageData(i)%NodeNums(1) == AirflowNetworkLinkageData(j)%NodeNums(2)) then
          If (AirflowNetworkCompData(AirflowNetworkLinkageData(j)%CompNum)%CompTypeNum /= CompTypeNum_DWC) then
            CALL ShowSevereError(RoutineName//'An '//TRIM(CurrentModuleObject) &
                                 //' object ('//TRIM(AirflowNetworkLinkageData(i)%CompName)//')')
            CALL ShowContinueError('must connect a duct component upstream and not ' &
                                   //TRIM(AirflowNetworkLinkageData(j)%Name))
            ErrorsFound=.true.
          End If
        End If
      End Do
      If (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusTypeNum == EPlusTypeNum_SPL) then
        CALL ShowSevereError(RoutineName//'An '//TRIM(CurrentModuleObject) &
                             //' object ('//TRIM(AirflowNetworkLinkageData(i)%CompName)//')')
        CALL ShowContinueError('does not allow a AirLoopHVAC:ZoneSplitter node = ' &
                               //TRIM(AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%Name))
        ErrorsFound=.true.
      End If
      If (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusTypeNum == EPlusTypeNum_SPL) then
        CALL ShowSevereError(RoutineName//'An '//TRIM(CurrentModuleObject) &
                             //' object ('//TRIM(AirflowNetworkLinkageData(i)%CompName)//')')
        CALL ShowContinueError('does not allow a AirLoopHVAC:ZoneSplitter node = ' &
                               //TRIM(AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%Name))
        ErrorsFound=.true.
      End If
      If (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusTypeNum == EPlusTypeNum_MIX) then
        CALL ShowSevereError(RoutineName//'An '//TRIM(CurrentModuleObject) &
                             //' object ('//TRIM(AirflowNetworkLinkageData(i)%CompName)//')')
        CALL ShowContinueError('does not allow a AirLoopHVAC:ZoneMixer node = ' &
                               //Trim(AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%Name))
        ErrorsFound=.true.
      End If
      If (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusTypeNum == EPlusTypeNum_MIX) then
        CALL ShowSevereError(RoutineName//'An '//TRIM(CurrentModuleObject) &
                             //' object ('//TRIM(AirflowNetworkLinkageData(i)%CompName)//')')
        CALL ShowContinueError('does not allow a AirLoopHVAC:ZoneMixer node = ' &
                               //TRIM(AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%Name))
        ErrorsFound=.true.
      End If
      If (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusNodeNum >0) then
        CALL ShowSevereError(RoutineName//'An '//TRIM(CurrentModuleObject) &
                             //' object ('//TRIM(AirflowNetworkLinkageData(i)%CompName)//')')
        CALL ShowContinueError('does not allow to connect an EnergyPlus node = ' &
                               //TRIM(AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%Name))
        ErrorsFound=.true.
      End If
      If (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusNodeNum >0) then
        CALL ShowSevereError(RoutineName//'An '//TRIM(CurrentModuleObject) &
                             //' object ('//TRIM(AirflowNetworkLinkageData(i)%CompName)//')')
        CALL ShowContinueError('does not allow to connect an EnergyPlus node = ' &
                               //TRIM(AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%Name))
        ErrorsFound=.true.
      End If
      If (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%EPlusZoneNum >0) then
        CALL ShowSevereError(RoutineName//'An '//TRIM(CurrentModuleObject) &
                             //' object ('//TRIM(AirflowNetworkLinkageData(i)%CompName)//')')
        CALL ShowContinueError('does not allow to connect an EnergyPlus zone = ' &
                               //TRIM(AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(1))%Name))
        ErrorsFound=.true.
      End If
      If (AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusZoneNum >0) then
        CALL ShowSevereError(RoutineName//'An '//TRIM(CurrentModuleObject) &
                             //' object ('//TRIM(AirflowNetworkLinkageData(i)%CompName)//')')
        CALL ShowContinueError('does not allow to connect an EnergyPlus zone = ' &
                               //TRIM(AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%Name))
        ErrorsFound=.true.
      End If
    End If
  End Do

  Do I=NumOfNodesMultiZone+1,AirflowNetworkNumOfNodes
    If (AirflowNetworkNodeData(i)%EPlusTypeNum == EPlusTypeNum_SPL) then
      LocalError=.false.
      j = GetSplitterOutletNumber('',1,LocalError)
      ALLOCATE(SplitterNodeNumbers(j+2))
      SplitterNodeNumbers = GetSplitterNodeNumbers('',1,LocalError)
      if (LocalError) ErrorsFound = .True.
    End If
  End Do

  ! Assing inlet and oulet nodes for a splitter
  DO I=1,AirflowNetworkNumOfNodes
    If (AirflowNetworkNodeData(I)%EplusNodeNum == SplitterNodeNumbers(1)) Then
      If (AirflowNetworkNodeData(I)%EPlusTypeNum .EQ. 0) AirflowNetworkNodeData(I)%EPlusTypeNum = EPlusTypeNum_SPI
    End If
    Do j=1,SplitterNodeNumbers(2)
      If (AirflowNetworkNodeData(I)%EplusNodeNum == SplitterNodeNumbers(j+2)) Then
        If (AirflowNetworkNodeData(I)%EPlusTypeNum .EQ. 0) AirflowNetworkNodeData(I)%EPlusTypeNum = EPlusTypeNum_SPO
      End If
    End Do
  End Do

  OneTimeFlag = .False.
  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Program terminates for preceding reason(s).')
  ENDIF
end if

  ! Catch a fan flow rate from EPlus input file and add a flag for VAV teminal damper
  DO I=1,AirflowNetworkNumOfLinks
    select case (AirflowNetworkCompData(AirflowNetworkLinkageData(i)%CompNum)%CompTypeNum)
      CASE (CompTypeNum_CVF) ! 'CVF'
        j=AirflowNetworkNodeData(AirflowNetworkLinkageData(i)%NodeNums(2))%EPlusNodeNum
        k = AirflowNetworkCompData(AirflowNetworkLinkageData(i)%CompNum)%TypeNum
        FanFlow=Node(j)%MassFlowRate
        If (DisSysCompCVFData(k)%FanTypeNum .eq. FanType_SimpleVAV) Then
          Call GetFanVolFlow(DisSysCompCVFData(k)%FanIndex, FanFlow)
          DisSysCompCVFData(k)%MaxAirMassFlowRate = FanFlow*StdRhoAir
        End If
      CASE (CompTypeNum_FAN) !'FAN'
      ! Check ventilation status for large openings
      CASE (CompTypeNum_SOP) !'Simple opening'
      CASE (CompTypeNum_TMU) ! Terminal unit
      CASE Default
    end select
  END DO

END SUBROUTINE ValidateDistributionSystem

SUBROUTINE ValidateExhaustFanInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Dec. 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine validate zone exhaust fan and associated surface

          ! METHODOLOGY EMPLOYED:
          ! na


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
          USE InputProcessor, ONLY: SameString
          USE DataZoneEquipment,  ONLY: ZoneEquipList,ZoneExhaustFan_Num

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na


          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER (len=*), PARAMETER   :: RoutineName='ValidateExhaustFanInput: ' ! include trailing blank space


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
integer I,j,k
LOGICAL,SAVE :: OneTimeFlag = .True.
LOGICAL :: ErrorsFound=.false.
LOGICAL found
INTEGER EquipTypeNum         ! Equipment type number
CHARACTER(len=MaxNameLength) :: CurrentModuleObject

  ! Validate supply and return connections
  if (OneTimeFlag) then
    CurrentModuleObject = 'AirflowNetwork:MultiZone:Component:ZoneExhaustFan'
    If (ANY(ZoneEquipConfig%IsControlled)) then
      ALLOCATE(AirflowNetworkZoneExhaustFan(NumOfZones))
      AirflowNetworkZoneExhaustFan = .FALSE.
    End If
    ! Ensure the number of exhaust fan defined in the AirflowNetwork model matches the number of Zone Exhaust Fan objects
    If (NumOfExhaustFans .NE. AirflowNetworkNumOfExhFan) Then
      CALL ShowSevereError(RoutineName//'The number of '//TRIM(CurrentModuleObject) &
         //' is not equal to the number of Fan:ZoneExhaust fans defined in ZoneHVAC:EquipmentConnections')
      CALL ShowContinueError('The number of '//TRIM(CurrentModuleObject)//' is ' &
         //TRIM(RoundSigDigits(AirflowNetworkNumOfExhFan)))
      CALL ShowContinueError('The number of Zone exhaust fans defined in ZoneHVAC:EquipmentConnections is ' &
         //TRIM(RoundSigDigits(NumOfExhaustFans)))
      ErrorsFound=.true.
    END IF

    Do i=1,AirflowNetworkNumOfExhFan
      ! Get zone number
      DO j=1,NumOfZones
        IF (.not. ZoneEquipConfig(j)%IsControlled) CYCLE
        DO k=1,ZoneEquipConfig(j)%NumExhaustNodes
          If (ZoneEquipConfig(j)%ExhaustNode(k) .EQ. MultizoneCompExhaustFanData(i)%InletNode) then
            MultizoneCompExhaustFanData(i)%EPlusZoneNum = ZoneEquipConfig(j)%ActualZoneNum
            Exit
          End If
        End Do
      End Do
      If (MultizoneCompExhaustFanData(i)%EPlusZoneNum == 0) then
        CALL ShowSevereError(RoutineName//'Zone name in '//TRIM(CurrentModuleObject)//'  = ' &
         //TRIM(MultizoneCompExhaustFanData(i)%Name)//' does not match the zone name in ZoneHVAC:EquipmentConnections')
        ErrorsFound=.true.
      End If
      ! Ensure a surface using zone exhaust fan to expose to the same zone
      found = .FALSE.
      Do j=1,AirflowNetworkNumOfSurfaces
        If (SameString(MultizoneSurfaceData(j)%OpeningName, MultizoneCompExhaustFanData(i)%Name)) then
          found = .TRUE.
          If (Surface(MultizoneSurfaceData(j)%SurfNum)%ExtBoundCond /= ExternalEnvironment) then
            CALL ShowSevereError(RoutineName//'The surface using '//TRIM(CurrentModuleObject) &
                                 //' is not an exterior surface: '//TRIM(MultizoneSurfaceData(j)%SurfName))
            ErrorsFound=.true.
          End If
          Exit
        End If
      End Do
      If (.NOT. found) then
        CALL ShowSevereError(TRIM(CurrentModuleObject)//'  = ' &
                             //TRIM(MultizoneCompExhaustFanData(i)%Name)//' is defined and never used.')
        ErrorsFound=.true.
      Else
        If (MultizoneCompExhaustFanData(i)%EPlusZoneNum .NE. Surface(MultizoneSurfaceData(j)%SurfNum)%Zone) then
          CALL ShowSevereError(RoutineName//'Zone name in '//TRIM(CurrentModuleObject)//'  = ' &
                               //TRIM(MultizoneCompExhaustFanData(i)%Name)//' does not match the zone name')
          CALL ShowContinueError('the surface is exposed to ' //Trim(Surface(MultizoneSurfaceData(j)%SurfNum)%Name))
          ErrorsFound=.true.
        Else
          AirflowNetworkZoneExhaustFan(MultizoneCompExhaustFanData(i)%EPlusZoneNum) = .TRUE.
        End If
      End If
    End Do

    ! Ensure all zone exhaust fans are defined
    DO j=1,NumOfZones
      IF (.not. ZoneEquipConfig(j)%IsControlled) CYCLE
      DO EquipTypeNum = 1, ZoneEquipList(j)%NumOfEquipTypes
        If (ZoneEquipList(j)%EquipType_Num(EquipTypeNum) == ZoneExhaustFan_Num) Then
          found = .FALSE.
          Do k=1,ZoneEquipConfig(j)%NumExhaustNodes
            Do i=1,AirflowNetworkNumOfExhFan
              If (ZoneEquipConfig(j)%ExhaustNode(k) .EQ. MultizoneCompExhaustFanData(i)%InletNode) then
                MultizoneCompExhaustFanData(i)%EPlusZoneNum = ZoneEquipConfig(j)%ActualZoneNum
                found = .TRUE.
              End If
            End Do
            If (.NOT. found) then
              CALL ShowSevereError(RoutineName//'Fan:ZoneExhaust is not defined in '//TRIM(CurrentModuleObject))
              CALL ShowContinueError('Zone Air Exhaust Node in ZoneHVAC:EquipmentConnections =' &
                                     //TRIM(NodeID(ZoneEquipConfig(j)%ExhaustNode(k))))
              ErrorsFound=.true.
            End If
          End Do
        End If
      End Do
    End Do

    OneTimeFlag = .False.
    IF (ErrorsFound) THEN
      CALL ShowFatalError(RoutineName//'Program terminates for preceding reason(s).')
    ENDIF
  end if

END SUBROUTINE ValidateExhaustFanInput

SUBROUTINE HybridVentilationControl

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   Dec. 2006
          !       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone hybrid ventilation managers
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine performs hybrid ventilation control

          ! METHODOLOGY EMPLOYED:
          ! na


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
  USE InputProcessor, ONLY: SameString
  USE DataHVACGlobals, ONLY: NumHybridVentSysAvailMgrs,HybridVentSysAvailAirLoopNum,HybridVentSysAvailVentCtrl, &
                             HybridVentSysAvailANCtrlStatus,HybridVentSysAvailMaster,HybridVentSysAvailWindModifier, &
                             HybridVentSysAvailActualZoneNum
  USE DataZoneEquipment, ONLY: ZoneEquipConfig


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na


          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: HybridVentCtrl_Close     = 2  ! Open windows or doors
  INTEGER, PARAMETER :: IndividualCtrlType       = 0  ! Individual window or door control
  INTEGER, PARAMETER :: GlobalCtrlType           = 1  ! GLobal window or door control
  CHARACTER (len=*), PARAMETER   :: RoutineName='HybridVentilationControl: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER SysAvailNum        ! Hybrid ventilation control number
  INTEGER AirLoopNum         ! Airloop number
  INTEGER ControlledZoneNum  ! Controlled zone number
  INTEGER ActualZoneNum      ! Actual zone number
  INTEGER ANSurfaceNum       ! AirflowNetwork Surface Number
  INTEGER SurfNum            ! Surface number
  INTEGER ControlType        ! Hybrid ventilation control type: 0 individual; 1 global
  LOGICAL Found              ! Logical to indicate whether a master surface is found or not
  INTEGER, SAVE :: HybridGlobalErrIndex = 0
  INTEGER, SAVE :: HybridGlobalErrCount = 0

  MultizoneSurfaceData%HybridVentClose = .FALSE.
  MultizoneSurfaceData%HybridCtrlGlobal = .FALSE.
  MultizoneSurfaceData%HybridCtrlMaster = .FALSE.
  MultizoneSurfaceData%WindModifier = 1.0d0
  ControlType = IndividualCtrlType

  Do SysAvailNum=1,NumHybridVentSysAvailMgrs
    AirLoopNum = HybridVentSysAvailAirLoopNum(SysAvailNum)
    VentilationCtrl = HybridVentSysAvailVentCtrl(SysAvailNum)
    If (HybridVentSysAvailANCtrlStatus(SysAvailNum) > 0) Then
      ControlType = GetCurrentScheduleValue(HybridVentSysAvailANCtrlStatus(SysAvailNum))
    End If
    Found = .FALSE.
    ActualZoneNum = 0
    Do ControlledZoneNum=1,NumOfZones
      IF (.not. ZoneEquipConfig(ControlledZoneNum)%IsControlled) CYCLE
      ! Ensure all the zones served by this AirLoopHVAC to be controlled by the hybrid ventilation
      If (AirLoopNum .GT. 0) THEN
        If (AirLoopNum == ZoneEquipConfig(ControlledZoneNum)%AirLoopNum) Then
          ActualZoneNum = ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum
        End If
      Else
        If (HybridVentSysAvailActualZoneNum(SysAvailNum) == ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum) THEN
          ActualZoneNum = HybridVentSysAvailActualZoneNum(SysAvailNum)
        Endif
      Endif
      If (ActualZoneNum .GT. 0) Then
        Do ANSurfaceNum=1,AirflowNetworkNumOfSurfaces
          SurfNum = MultizoneSurfaceData(ANSurfaceNum)%SurfNum
          If (Surface(SurfNum)%Zone == ActualZoneNum) Then
            If (VentilationCtrl == HybridVentCtrl_Close) Then
              MultizoneSurfaceData(ANSurfaceNum)%HybridVentClose = .TRUE.
            Else
              If (HybridVentSysAvailWindModifier(SysAvailNum) .GE. 0) Then
                MultizoneSurfaceData(ANSurfaceNum)%WindModifier = HybridVentSysAvailWindModifier(SysAvailNum)
              End If
              If (ControlType .eq. GlobalCtrlType) Then
                MultizoneSurfaceData(ANSurfaceNum)%HybridCtrlGlobal = .TRUE.
                If (HybridVentSysAvailMaster(SysAvailNum) .EQ. ActualZoneNum) Then
                  If ((SurfaceWindow(SurfNum)%OriginalClass == SurfaceClass_Window .OR.   &
                       SurfaceWindow(SurfNum)%OriginalClass == SurfaceClass_Door .OR.   &
                       SurfaceWindow(SurfNum)%OriginalClass == SurfaceClass_GlassDoor) .AND.   &
                       Surface(SurfNum)%ExtBoundCond == ExternalEnvironment) then
                    MultizoneSurfaceData(ANSurfaceNum)%HybridCtrlMaster = .TRUE.
                    Found = .TRUE.
                  End If
                End If
              End If
            End If
          End If
        End Do
      End If
    End Do
    If (ControlType .eq. GlobalCtrlType .AND. .Not. Found .AND. .NOT. WarmupFlag .AND. VentilationCtrl/=HybridVentCtrl_Close) Then
      HybridGlobalErrCount = HybridGlobalErrCount + 1
      if (HybridGlobalErrCount < 2) then
        CALL ShowWarningError(RoutineName//'The hybrid ventilation control schedule value indicates global control in the '&
                              //'controlled zone = '//TRIM(Zone(HybridVentSysAvailMaster(SysAvailNum))%Name))
        CALL ShowContinueError('The exterior surface containing an opening component in the controlled zone is not found. ' &
                               //' No global control will not be modeled.')
        CALL ShowContinueError('The individual control is assumed.')
        CALL ShowContinueErrorTimeStamp(' ')
      else
        CALL ShowRecurringWarningErrorAtEnd(RoutineName//'The hybrid ventilation control requires a global control.' &
                                            //' The individual control continues...', &
                                            HybridGlobalErrIndex,  REAL(controlType,r64),  REAL(ControlType,r64))
      end if
    End If
  End Do

END SUBROUTINE HybridVentilationControl

FUNCTION GetZoneInfilAirChangeRate(ZoneNum) RESULT(ACH)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   May. 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! This function outputs air change per hour in a given zone

          ! METHODOLOGY EMPLOYED:
          ! na


          ! REFERENCES:
          ! na


          ! USE STATEMENTS:
          Use DataHVACGlobals, ONLY: TimeStepSys

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)       :: ZoneNum  ! hybrid ventilation system controlled zone number


          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na


          ! DERIVED TYPE DEFINITIONS
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    ACH                ! Zone air change rate [ACH]
  REAL(r64)    InfilVolume        ! Zone infiltration volume
  REAL(r64)    RhoAir             ! Zone air density [kg/m3]
  REAL(r64)    CpAir              ! Zone air specific heat

    CpAir   = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), MAT(ZoneNum))
    RhoAir = PsyRhoAirFnPbTdbW(OutBaroPress,MAT(ZoneNum),ZoneAirHumRat(ZoneNum))
    InfilVolume    = (AirflowNetworkExchangeData(ZoneNum)%SumMCp/CpAir/RhoAir)*TimeStepSys*SecInHour
    ACH = InfilVolume/(TimeStepSys*Zone(ZoneNum)%Volume)

END FUNCTION GetZoneInfilAirChangeRate

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


END MODULE AirflowNetworkBalanceManager
