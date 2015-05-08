Module Fans
  ! Module containing the fan simulation routines

  ! MODULE INFORMATION:
  !       AUTHOR         Richard J. Liesen
  !       DATE WRITTEN   April 1998
  !       MODIFIED       Shirey, May 2001
  !                      Griffith, May 2009, EMS changes
  !                      Craig Wray 22Aug2010 Added Fan Component Model
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required to
  ! manage the Fan System Component

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataHVACGlobals, ONLY: TurnFansOn, TurnFansOff, Main, Cooling, Heating, Other, OnOffFanPartLoadFraction, &
                           SmallAirVolFlow, UnbalExhMassFlow, BalancedExhMassFlow, NightVentOn, cFanTypes, &
                           FanType_SimpleConstVolume, FanType_SimpleVAV, FanType_SimpleOnOff, FanType_ZoneExhaust, &
                           FanType_ComponentModel, MinFrac, FixedMin !cpw22Aug2010 Added FanType_ComponentModel
USE DataGlobals,     ONLY: BeginEnvrnFlag, MaxNameLength, WarmupFlag, SysSizingCalc, emsCallFromComponentGetInput, &
                           DisplayExtraWarnings
USE EMSManager,      ONLY: ManageEMS
USE DataInterfaces
Use DataEnvironment, ONLY: StdRhoAir
USE Psychrometrics,  ONLY: PsyRhoAirFnPbTdbW, PsyTdbFnHW, PsyCpAirFnWTdb

  ! Use statements for access to subroutines in other modules
USE ScheduleManager

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS
  ! parameters describing fan types are contained in DataHVACGlobals (see USE statement above)

INTEGER, PARAMETER :: ExhaustFanCoupledToAvailManagers     = 150
INTEGER, PARAMETER :: ExhaustFanDecoupledFromAvailManagers = 151

  !na

  ! DERIVED TYPE DEFINITIONS
TYPE FanEquipConditions
  CHARACTER(len=MaxNameLength) :: FanName  =' '  ! Name of the fan
  CHARACTER(len=MaxNameLength) :: FanType  =' '  ! Type of Fan ie. Simple, Vane axial, Centrifugal, etc.
  CHARACTER(len=MaxNameLength) :: AvailSchedName =' '  ! Fan Operation Schedule
  INTEGER      :: FanType_Num              =0    ! DataHVACGlobals fan type
  INTEGER      :: AvailSchedPtrNum         =0    ! Pointer to the availability schedule
  REAL(r64)    :: InletAirMassFlowRate     =0.0d0  !MassFlow through the Fan being Simulated [kg/Sec]

  REAL(r64)    :: OutletAirMassFlowRate    =0.0d0
  REAL(r64)    :: MaxAirFlowRate           =0.0d0  !Max Specified Volume Flow Rate of Fan [m3/sec]
  LOGICAL      :: MaxAirFlowRateIsAutosizable = .FALSE. ! if true, then this type of fan could be autosize
  LOGICAL      :: MaxAirFlowRateEMSOverrideOn = .FALSE. ! if true, EMS wants to override fan size for Max Volume Flow Rate
  REAL(r64)    :: MaxAirFlowRateEMSOverrideValue = 0.d0 ! EMS value to use for override of  Max Volume Flow Rate
  REAL(r64)    :: MinAirFlowRate           =0.0d0  !Min Specified Volume Flow Rate of Fan [m3/sec]
  REAL(r64)    :: MaxAirMassFlowRate       =0.0d0  ! Max flow rate of fan in kg/sec
  REAL(r64)    :: MinAirMassFlowRate       =0.0d0  ! Min flow rate of fan in kg/sec
  INTEGER      :: FanMinAirFracMethod      = MinFrac   ! parameter for what method is used for min flow fraction
  REAL(r64)    :: FanMinFrac               = 0.0D0 ! Minimum fan air flow fraction
  REAL(r64)    :: FanFixedMin              = 0.0D0 ! Absolute minimum fan air flow [m3/s]
  LOGICAL      :: EMSMaxMassFlowOverrideOn = .FALSE. ! if true, then EMS is calling to override mass flow
  REAL(r64)    :: EMSAirMassFlowValue      = 0.0D0 ! value EMS is directing to use [kg/s]
  REAL(r64)    :: InletAirTemp             =0.0d0
  REAL(r64)    :: OutletAirTemp            =0.0d0
  REAL(r64)    :: InletAirHumRat           =0.0d0
  REAL(r64)    :: OutletAirHumRat          =0.0d0
  REAL(r64)    :: InletAirEnthalpy         =0.0d0
  REAL(r64)    :: OutletAirEnthalpy        =0.0d0
  REAL(r64)    :: FanPower                 =0.0d0  !Power of the Fan being Simulated [kW]
  REAL(r64)    :: FanEnergy                =0.0d0  !Fan energy in [kJ]
  REAL(r64)    :: FanRuntimeFraction       =0.0d0  !Fraction of the timestep that the fan operates
  REAL(r64)    :: DeltaTemp                =0.0d0  !Temp Rise across the Fan [C]
  REAL(r64)    :: DeltaPress               =0.0d0  !Delta Pressure Across the Fan [N/m2]
  LOGICAL      :: EMSFanPressureOverrideOn = .FALSE. ! if true, then EMS is calling to override
  REAL(r64)    :: EMSFanPressureValue      = 0.0D0 ! EMS value for Delta Pressure Across the Fan [Pa]
! cpw22Aug2010 Clarify meaning of "fan efficiency"
!  REAL(r64)    :: FanEff                   =0.0d0  !Fan total efficiency; motor and mechanical
  REAL(r64)    :: FanEff                   =0.0d0 !Fan total system efficiency (fan*belt*motor*VFD)
  LOGICAL      :: EMSFanEffOverrideOn      = .FALSE. ! if true, then EMS is calling to override
  REAL(r64)    :: EMSFanEffValue           = 0.0D0 ! EMS value for total efficiency of the Fan, fraction on 0..1
  REAL(r64)    :: MotEff                   =0.0d0  !Fan motor efficiency
  REAL(r64)    :: MotInAirFrac             =0.0d0  !Fraction of motor heat entering air stream
  REAL(r64), Dimension(5):: FanCoeff            =0.0d0  !Fan Part Load Coefficients to match fan type
  ! Mass Flow Rate Control Variables
  REAL(r64)    :: MassFlowRateMaxAvail     =0.0d0
  REAL(r64)    :: MassFlowRateMinAvail     =0.0d0
  REAL(r64)    :: RhoAirStdInit            =0.0d0
  INTEGER      :: InletNodeNum             =0
  INTEGER      :: OutletNodeNum            =0
  INTEGER      :: NVPerfNum                =0
  INTEGER      :: FanPowerRatAtSpeedRatCurveIndex  =0
  INTEGER      :: FanEffRatioCurveIndex    =0
  CHARACTER(len=MaxNameLength) :: EndUseSubcategoryName=' '
  LOGICAL      :: OneTimePowerRatioCheck = .TRUE. ! one time flag used for error message
  LOGICAL      :: OneTimeEffRatioCheck = .TRUE.   ! one time flag used for error message

  !cpw22Aug2010 Following added to support Fan Component Model input
  REAL(r64)    :: FanWheelDia              =0.0d0   !Fan wheel outer diameter [m]
  REAL(r64)    :: FanOutletArea            =0.0d0   !Fan outlet area [m2]
  REAL(r64)    :: FanMaxEff                =0.0d0   !Fan maximum static efficiency [-]
  REAL(r64)    :: EuMaxEff                 =0.0d0   !Euler number at fan maximum static efficiency [-]
  REAL(r64)    :: FanMaxDimFlow            =0.0d0   !Fan maximum dimensionless airflow [-]
  REAL(r64)    :: FanShaftPwrMax           =0.0d0   !Fan shaft maximum input power [W]
  REAL(r64)    :: FanSizingFactor          =0.0d0   !Fan sizing factor [-] cpw31Aug2010
  REAL(r64)    :: PulleyDiaRatio           =0.0d0   !Motor/fan pulley diameter ratio [-]
  REAL(r64)    :: BeltMaxTorque            =0.0d0   !Belt maximum torque [N-m]
  REAL(r64)    :: BeltSizingFactor         =0.0d0   !Belt sizing factor [-]
  REAL(r64)    :: BeltTorqueTrans          =0.0d0   !Belt fractional torque transition Region 1-2 [-]
  REAL(r64)    :: MotorMaxSpd              =0.0d0   !Motor maximum speed [rpm]
  REAL(r64)    :: MotorMaxOutPwr           =0.0d0   !Motor maximum output power [W]
  REAL(r64)    :: MotorSizingFactor        =0.0d0   !Motor sizing factor [-]
  CHARACTER(len=MaxNameLength) :: VFDEffType =' '   !VFD efficiency type [Speed or Power]
  REAL(r64)    :: VFDMaxOutPwr             =0.0d0   !VFD maximum output power [W]
  REAL(r64)    :: VFDSizingFactor          =0.0d0   !VFD sizing factor [-] cpw31Aug2010
  INTEGER      :: PressRiseCurveIndex      =0       !Fan pressure rise curve index
  INTEGER      :: PressResetCurveIndex     =0       !Duct static pressure reset curve index
  INTEGER      :: PLFanEffNormCurveIndex   =0       !Fan part-load efficiency (normal) curve index
  INTEGER      :: PLFanEffStallCurveIndex  =0       !Fan part-load efficiency (stall) curve index
  INTEGER      :: DimFlowNormCurveIndex    =0       !Fan dimensionless airflow (normal) curve index
  INTEGER      :: DimFlowStallCurveIndex   =0       !Fan dimensionless airflow (stall) curve index
  INTEGER      :: BeltMaxEffCurveIndex     =0       !Belt maximum efficiency curve index
  INTEGER      :: PLBeltEffReg1CurveIndex  =0       !Belt part-load efficiency (Region 1) curve index
  INTEGER      :: PLBeltEffReg2CurveIndex  =0       !Belt part-load efficiency (Region 2) curve index
  INTEGER      :: PLBeltEffReg3CurveIndex  =0       !Belt part-load efficiency (Region 3) curve index
  INTEGER      :: MotorMaxEffCurveIndex    =0       !Motor maximum efficiency curve index
  INTEGER      :: PLMotorEffCurveIndex     =0       !Motor part-load efficiency curve index
  INTEGER      :: VFDEffCurveIndex         =0       !VFD efficiency curve index

  !cpw22Aug2010 Following added to support Fan Component Model calculated values
  REAL(r64)    :: DeltaPressTot            =0.0d0   !Total pressure rise across fan [N/m2]
  REAL(r64)    :: FanAirPower              =0.0d0   !Air power for fan being Simulated [W]
  REAL(r64)    :: FanSpd                   =0.0d0   !Fan shaft rotational speed [rpm]
  REAL(r64)    :: FanTrq                   =0.0d0   !Fan shaft torque [N-m]
  REAL(r64)    :: FanWheelEff              =0.0d0   !Fan efficiency (mechanical)
  REAL(r64)    :: FanShaftPower            =0.0d0   !Shaft input power for fan being Simulated [W]
  REAL(r64)    :: BeltMaxEff               =0.0d0   !Belt maximum efficiency (mechanical) cpw31Aug2010
  REAL(r64)    :: BeltEff                  =0.0d0   !Belt efficiency (mechanical)
  REAL(r64)    :: BeltInputPower           =0.0d0   !Belt input power for fan being Simulated [W]
  REAL(r64)    :: MotorMaxEff              =0.0d0   !Motor maximum efficiency (electrical) cpw31Aug2010
  REAL(r64)    :: MotorInputPower          =0.0d0   !Motor input power for fan being Simulated [W]
  REAL(r64)    :: VFDEff                   =0.0d0   !VFD efficiency (electrical)
  REAL(r64)    :: VFDInputPower            =0.0d0   !VFD input power for fan being Simulated [W]
  REAL(r64)    :: MaxFanPowerEncountered   =0.0d0   !Maximum VFD input power encountered [W]

  !zone exhaust fan
  INTEGER      :: FlowFractSchedNum        =0    ! schedule index flow rate modifier schedule
  INTEGER      :: AvailManagerMode         =0    ! mode for how exhaust fan should react to availability managers
  INTEGER      :: MinTempLimitSchedNum     =0    ! schedule index minimum temperature limit
  INTEGER      :: BalancedFractSchedNum   =0    ! schedule index portion recirculated
  REAL(r64)    :: UnbalancedOutletMassFlowRate = 0.d0
  REAL(r64)    :: BalancedOutletMassFlowRate = 0.d0

END TYPE FanEquipConditions

TYPE NightVentPerfData
  CHARACTER(len=MaxNameLength) :: FanName  =' ' ! Name of the fan that will use this data
  REAL(r64)    :: FanEff                   =0.0d0 !Fan total efficiency; motor and mechanical
  REAL(r64)    :: DeltaPress               =0.0d0 !Delta Pressure Across the Fan [N/m2]
  REAL(r64)    :: MaxAirFlowRate           =0.0d0 !Max Specified Volume Flow Rate of Fan [m3/s]
  REAL(r64)    :: MaxAirMassFlowRate       =0.0d0 ! Max flow rate of fan in kg/sec
  REAL(r64)    :: MotEff                   =0.0d0 !Fan motor efficiency
  REAL(r64)    :: MotInAirFrac             =0.0d0 !Fraction of motor heat entering air stream
END TYPE NightVentPerfData



  !MODULE VARIABLE DECLARATIONS:
  INTEGER :: NumFans     =0 ! The Number of Fans found in the Input
  INTEGER :: NumNightVentPerf =0 ! number of FAN:NIGHT VENT PERFORMANCE objects found in the input
  TYPE (FanEquipConditions), ALLOCATABLE, DIMENSION(:) :: Fan
  TYPE (NightVentPerfData), ALLOCATABLE, DIMENSION(:)  :: NightVentPerf
  LOGICAL :: GetFanInputFlag = .True.  ! Flag set to make sure you get input once
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName
  LOGICAL  :: LocalTurnFansOn  = .FALSE.  ! If True, overrides fan schedule and cycles ZoneHVAC component fans on
  LOGICAL  :: LocalTurnFansOff = .FALSE.  ! If True, overrides fan schedule and LocalTurnFansOn and
                                          ! forces ZoneHVAC comp fans off

! Subroutine Specifications for the Module
          ! Driver/Manager Routines
Public  SimulateFanComponents

          ! Get Input routines for module
Private GetFanInput

          ! Initialization routines for module
Private InitFan
Private SizeFan

          ! Algorithms for the module
Private SimSimpleFan
Private SimVariableVolumeFan
Private SimZoneExhaustFan
Private SimComponentModelFan !cpw22Aug2010


          ! Update routine to check convergence and update nodes
Private UpdateFan

          ! Reporting routines for module
Private ReportFan

          ! Utility routines for module
Public  GetFanDesignVolumeFlowRate
Public  GetFanInletNode
Public  GetFanOutletNode
Public  GetFanIndex
Public  GetFanType
Public  GetFanVolFlow
PUBLIC  GetFanPower
Public  GetFanAvailSchPtr
Public  SetFanData
Public  GetFanSpeedRatioCurveIndex

CONTAINS

! MODULE SUBROUTINES:
!*************************************************************************
SUBROUTINE SimulateFanComponents(CompName,FirstHVACIteration,CompIndex,SpeedRatio,ZoneCompTurnFansOn,ZoneCompTurnFansOff, &
                                 PressureRise)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   February 1998
          !       MODIFIED       Chandan Sharma, March 2011 - FSEC: Added logic for ZoneHVAC sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages Fan component simulation.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL,      INTENT (IN):: FirstHVACIteration
  CHARACTER(len=*), INTENT(IN) :: CompName
  INTEGER :: CompIndex
  REAL(r64), OPTIONAL, INTENT(IN) :: SpeedRatio
  LOGICAL,   OPTIONAL, INTENT(IN) :: ZoneCompTurnFansOn   ! Turn fans ON signal from ZoneHVAC component
  LOGICAL,   OPTIONAL, INTENT(IN) :: ZoneCompTurnFansOff  ! Turn Fans OFF signal from ZoneHVAC component
  REAL(r64), OPTIONAL, INTENT(IN) :: PressureRise         ! Pressure difference to use for DeltaPress

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank = ' '

          ! INTERFACE BLOCK SPECIFICATIONS

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: FanNum     ! current fan number

          ! FLOW:

  ! Obtains and Allocates fan related parameters from input file
  IF (GetFanInputFlag) THEN  !First time subroutine has been entered
    CALL GetFanInput
    GetFanInputFlag=.false.
  End If

  IF (CompIndex == 0) THEN
    FanNum = FindItemInList(CompName,Fan%FanName,NumFans)
    IF (FanNum == 0) THEN
      CALL ShowFatalError('SimulateFanComponents: Fan not found='//TRIM(CompName))
    ENDIF
    CompIndex=FanNum
  ELSE
    FanNum=CompIndex
    IF (FanNum > NumFans .or. FanNum < 1) THEN
      CALL ShowFatalError('SimulateFanComponents: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(FanNum))// &
                          ', Number of Fans='//TRIM(TrimSigDigits(NumFans))//  &
                          ', Fan name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(FanNum)) THEN
      IF (CompName /= Blank .AND. CompName /= Fan(FanNum)%FanName) THEN
        CALL ShowFatalError('SimulateFanComponents: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(FanNum))// &
                            ', Fan name='//TRIM(CompName)//', stored Fan Name for that index='//  &
                            TRIM(Fan(FanNum)%FanName))
      ENDIF
      CheckEquipName(FanNum)=.false.
    ENDIF
  ENDIF

  LocalTurnFansOn   = .FALSE.
  LocalTurnFansOff  = .FALSE.
  ! With the correct FanNum Initialize
  CALL InitFan(FanNum,FirstHVACIteration)  ! Initialize all fan related parameters

  IF (PRESENT(ZoneCompTurnFansOn) .AND. PRESENT(ZoneCompTurnFansOff)) THEN
    ! Set module-level logic flags equal to ZoneCompTurnFansOn and ZoneCompTurnFansOff values passed into this routine
    ! for ZoneHVAC components with system availability managers defined.
    ! The module-level flags get used in the other subroutines (e.g., SimSimpleFan,SimVariableVolumeFan and SimOnOffFan)
    LocalTurnFansOn  = ZoneCompTurnFansOn
    LocalTurnFansOff = ZoneCompTurnFansOff
  ELSE
    ! Set module-level logic flags equal to the global LocalTurnFansOn and LocalTurnFansOff variables for all other cases.
    LocalTurnFansOn  = TurnFansOn
    LocalTurnFansOff = TurnFansOff
  ENDIF

  ! Calculate the Correct Fan Model with the current FanNum
  IF (Fan(FanNum)%FanType_Num == FanType_SimpleConstVolume) THEN
    Call SimSimpleFan(FanNum)
  ELSE IF (Fan(FanNum)%FanType_Num == FanType_SimpleVAV) THEN
    IF (PRESENT(PressureRise)) THEN
      Call SimVariableVolumeFan(FanNum, PressureRise)
    ELSE
      Call SimVariableVolumeFan(FanNum)
    ENDIF
  ELSE IF (Fan(FanNum)%FanType_Num == FanType_SimpleOnOff) THEN
    Call SimOnOffFan(FanNum, SpeedRatio)
  ELSE IF (Fan(FanNum)%FanType_Num == FanType_ZoneExhaust) THEN
    Call SimZoneExhaustFan(FanNum)
  ! cpw22Aug2010 Add call for Component Model fan
  ELSE IF (Fan(FanNum)%FanType_Num == FanType_ComponentModel) THEN
    Call SimComponentModelFan(FanNum)
  END IF

  ! Update the current fan to the outlet nodes
  Call UpdateFan(FanNum)

  ! Report the current fan
  Call ReportFan(FanNum)

  RETURN

END SUBROUTINE SimulateFanComponents


! Get Input Section of the Module
!******************************************************************************
SUBROUTINE GetFanInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   April 1998
          !       MODIFIED       Shirey, May 2001
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for fans and stores it in fan data structures

          ! METHODOLOGY EMPLOYED:
          ! Uses "Get" routines to read in data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor
    USE NodeInputManager,      ONLY: GetOnlySingleNode
    USE CurveManager,          ONLY: GetCurveIndex
    USE BranchNodeConnections, ONLY: TestCompSet

!    USE DataIPShortCuts
    USE DataGlobals,           ONLY: AnyEnergyManagementSystemInModel, ScheduleAlwaysOn
    USE DataInterfaces,        ONLY: SetupEMSActuator, SetupEMSInternalVariable

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
    INTEGER :: FanNum      ! The fan that you are currently loading input into
    INTEGER :: NumSimpFan  ! The number of Simple Const Vol Fans
    INTEGER :: NumVarVolFan ! The number of Simple Variable Vol Fans
    INTEGER :: NumOnOff     ! The number of Simple on-off Fans
    INTEGER :: NumZoneExhFan
    INTEGER :: SimpFanNum
    INTEGER :: OnOffFanNum
    INTEGER :: VarVolFanNum
    INTEGER :: ExhFanNum
    INTEGER :: NVPerfNum
    LOGICAL :: NVPerfFanFound
    INTEGER :: NumCompModelFan ! cpw22Aug2010 The number of Component Model Fans
    INTEGER :: CompModelFanNum ! cpw22Aug2010 Component Model Fan index
    INTEGER :: NumAlphas
    INTEGER :: NumNums
    INTEGER :: checkNum
    INTEGER :: IOSTAT
    LOGICAL :: ErrorsFound = .false.   ! If errors detected in input
    LOGICAL :: IsNotOK               ! Flag to verify name
    LOGICAL :: IsBlank               ! Flag for blank name
    CHARACTER(len=*), PARAMETER    :: RoutineName='GetFanInput: ' ! include trailing blank space
    CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cAlphaFieldNames
    CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cNumericFieldNames
    LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericFieldBlanks
    LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaFieldBlanks
    CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: cAlphaArgs
    REAL(r64),ALLOCATABLE, DIMENSION(:) :: rNumericArgs
    CHARACTER(len=MaxNameLength) :: cCurrentModuleObject
    INTEGER :: NumParams
    INTEGER :: MaxAlphas
    INTEGER :: MaxNumbers

          ! Flow
    MaxAlphas=0
    MaxNumbers=0
    NumSimpFan   = GetNumObjectsFound('Fan:ConstantVolume')
    IF (NumSimpFan > 0) THEN
      CALL GetObjectDefMaxArgs('Fan:ConstantVolume',NumParams,NumAlphas,NumNums)
      MaxAlphas=MAX(MaxAlphas,NumAlphas)
      MaxNumbers=MAX(MaxNumbers,NumNums)
    ENDIF
    NumVarVolFan = GetNumObjectsFound('Fan:VariableVolume')
    IF (NumVarVolFan > 0) THEN
      CALL GetObjectDefMaxArgs('Fan:VariableVolume',NumParams,NumAlphas,NumNums)
      MaxAlphas=MAX(MaxAlphas,NumAlphas)
      MaxNumbers=MAX(MaxNumbers,NumNums)
    ENDIF
    NumOnOff = GetNumObjectsFound('Fan:OnOff')
    IF (NumOnOff > 0) THEN
      CALL GetObjectDefMaxArgs('Fan:OnOff',NumParams,NumAlphas,NumNums)
      MaxAlphas=MAX(MaxAlphas,NumAlphas)
      MaxNumbers=MAX(MaxNumbers,NumNums)
    ENDIF
    NumZoneExhFan = GetNumObjectsFound('Fan:ZoneExhaust')
    IF (NumZoneExhFan > 0) THEN
      CALL GetObjectDefMaxArgs('Fan:ZoneExhaust',NumParams,NumAlphas,NumNums)
      MaxAlphas=MAX(MaxAlphas,NumAlphas)
      MaxNumbers=MAX(MaxNumbers,NumNums)
    ENDIF
    NumNightVentPerf = GetNumObjectsFound('FanPerformance:NightVentilation')
    IF (NumNightVentPerf > 0) THEN
      CALL GetObjectDefMaxArgs('FanPerformance:NightVentilation',NumParams,NumAlphas,NumNums)
      MaxAlphas=MAX(MaxAlphas,NumAlphas)
      MaxNumbers=MAX(MaxNumbers,NumNums)
    ENDIF

    ! cpw22Aug2010 Added get max alphas and numbers for ComponentModel fan
    NumCompModelFan = GetNumObjectsFound('Fan:ComponentModel')
    IF (NumCompModelFan > 0) THEN
      CALL GetObjectDefMaxArgs('Fan:ComponentModel',NumParams,NumAlphas,NumNums)
      MaxAlphas=MAX(MaxAlphas,NumAlphas)
      MaxNumbers=MAX(MaxNumbers,NumNums)
    ENDIF

    ALLOCATE(cAlphaArgs(MaxAlphas))
    cAlphaArgs=' '
    ALLOCATE(cAlphaFieldNames(MaxAlphas))
    cAlphaFieldNames=' '
    ALLOCATE(lAlphaFieldBlanks(MaxAlphas))
    lAlphaFieldBlanks=.false.
    ALLOCATE(cNumericFieldNames(MaxNumbers))
    cNumericFieldNames=' '
    ALLOCATE(lNumericFieldBlanks(MaxNumbers))
    lNumericFieldBlanks=.false.
    ALLOCATE(rNumericArgs(MaxNumbers))
    rNumericArgs=0.0d0

    NumFans = NumSimpFan + NumVarVolFan + NumZoneExhFan + NumOnOff + NumCompModelFan ! cpw1Mar2010 Add NumCompModelFan
    IF (NumFans > 0) THEN
      ALLOCATE(Fan(NumFans))
    ENDIF
    ALLOCATE(CheckEquipName(NumFans))
    CheckEquipName=.true.

      DO SimpFanNum = 1,  NumSimpFan
        FanNum = SimpFanNum
        cCurrentModuleObject= 'Fan:ConstantVolume'
        CALL GetObjectItem(cCurrentModuleObject,SimpFanNum,cAlphaArgs,NumAlphas, &
                           rNumericArgs,NumNums,IOSTAT, &
                           NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                           AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(cAlphaArgs(1),Fan%FanName,FanNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) cAlphaArgs(1)='xxxxx'
        ENDIF
        Fan(FanNum)%FanName  = cAlphaArgs(1)
        Fan(FanNum)%FanType =  cCurrentModuleObject
        Fan(FanNum)%AvailSchedName = cAlphaArgs(2)
        IF (lAlphaFieldBlanks(2)) THEN
          Fan(FanNum)%AvailSchedPtrNum = ScheduleAlwaysOn
        ELSE
          Fan(FanNum)%AvailSchedPtrNum = GetScheduleIndex(cAlphaArgs(2))
          IF (Fan(FanNum)%AvailSchedPtrNum == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(2))//  &
               ' entered ='//TRIM(cAlphaArgs(2))// &
               ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
            ErrorsFound=.true.
          END IF
        END IF
        Fan(FanNum)%FanType_Num=FanType_SimpleConstVolume

        Fan(FanNum)%FanEff        = rNumericArgs(1)
        Fan(FanNum)%DeltaPress    = rNumericArgs(2)
        Fan(FanNum)%MaxAirFlowRate= rNumericArgs(3)
        IF (Fan(FanNum)%MaxAirFlowRate == 0.0d0) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(Fan(FanNum)%FanName)//  &
             '" has specified 0.0 max air flow rate. It will not be used in the simulation.')
        ENDIF
        Fan(FanNum)%MaxAirFlowRateIsAutosizable = .TRUE.
        Fan(FanNum)%MotEff        = rNumericArgs(4)
        Fan(FanNum)%MotInAirFrac  = rNumericArgs(5)
        Fan(FanNum)%MinAirFlowRate= 0.0d0

        Fan(FanNum)%InletNodeNum  = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
        Fan(FanNum)%OutletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

        IF (NumAlphas > 4) THEN
          Fan(FanNum)%EndUseSubcategoryName = cAlphaArgs(5)
        ELSE
          Fan(FanNum)%EndUseSubcategoryName = 'General'
        END IF

        CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4),'Air Nodes')


      END DO   ! end Number of Simple FAN Loop


      DO VarVolFanNum = 1,  NumVarVolFan
        FanNum = NumSimpFan + VarVolFanNum
        cCurrentModuleObject= 'Fan:VariableVolume'
        CALL GetObjectItem(cCurrentModuleObject,VarVolFanNum,cAlphaArgs,NumAlphas, &
                           rNumericArgs,NumNums,IOSTAT, &
                           NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                           AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(cAlphaArgs(1),Fan%FanName,FanNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) cAlphaArgs(1)='xxxxx'
        ENDIF
        Fan(FanNum)%FanName = cAlphaArgs(1)
        Fan(FanNum)%FanType = cCurrentModuleObject
        Fan(FanNum)%AvailSchedName = cAlphaArgs(2)
        IF (lAlphaFieldBlanks(2)) THEN
          Fan(FanNum)%AvailSchedPtrNum = ScheduleAlwaysOn
        ELSE
          Fan(FanNum)%AvailSchedPtrNum = GetScheduleIndex(cAlphaArgs(2))
          IF (Fan(FanNum)%AvailSchedPtrNum == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(2))//  &
               ' entered ='//TRIM(cAlphaArgs(2))// &
               ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
            ErrorsFound=.true.
          END IF
        END IF
        Fan(FanNum)%FanType_Num=FanType_SimpleVAV

        Fan(FanNum)%FanEff        = rNumericArgs(1)
        Fan(FanNum)%DeltaPress    = rNumericArgs(2)
        Fan(FanNum)%MaxAirFlowRate= rNumericArgs(3)
        IF (Fan(FanNum)%MaxAirFlowRate == 0.0d0) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(Fan(FanNum)%FanName)//  &
             '" has specified 0.0 max air flow rate. It will not be used in the simulation.')
        ENDIF
        Fan(FanNum)%MaxAirFlowRateIsAutosizable = .TRUE.
        IF (SameString(cAlphaArgs(3) , 'Fraction')) THEN
          Fan(FanNum)%FanMinAirFracMethod = MinFrac
        ELSEIF (SameString(cAlphaArgs(3), 'FixedFlowRate')) THEN
          Fan(FanNum)%FanMinAirFracMethod = FixedMin
        ELSE
          CALL ShowSevereError(TRIM(cAlphaFieldNames(3))//' should be either Fraction or FixedFlowRate.')
          CALL ShowContinueError('Occurs in '//trim(Fan(FanNum)%FanName)//' object.')
          ErrorsFound=.true.
        ENDIF
!        Fan(FanNum)%MinAirFlowRate= rNumericArgs(4)
        Fan(FanNum)%FanMinFrac    = rNumericArgs(4)
        Fan(FanNum)%FanFixedMin   = rNumericArgs(5)
        Fan(FanNum)%MotEff        = rNumericArgs(6)
        Fan(FanNum)%MotInAirFrac  = rNumericArgs(7)
        Fan(FanNum)%FanCoeff(1)   = rNumericArgs(8)
        Fan(FanNum)%FanCoeff(2)   = rNumericArgs(9)
        Fan(FanNum)%FanCoeff(3)   = rNumericArgs(10)
        Fan(FanNum)%FanCoeff(4)   = rNumericArgs(11)
        Fan(FanNum)%FanCoeff(5)   = rNumericArgs(12)
        IF (Fan(FanNum)%FanCoeff(1) == 0.0d0 .and. Fan(FanNum)%FanCoeff(2) == 0.0d0 .and.  &
            Fan(FanNum)%FanCoeff(3) == 0.0d0 .and. Fan(FanNum)%FanCoeff(4) == 0.0d0 .and.  &
            Fan(FanNum)%FanCoeff(5) == 0.0d0)  THEN
            CALL ShowWarningError('Fan Coefficients are all zero.  No Fan power will be reported.')
            CALL ShowContinueError('For '//TRIM(cCurrentModuleObject)//', Fan='//TRIM(cAlphaArgs(1)))
        ENDIF
        Fan(FanNum)%InletNodeNum  = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
        Fan(FanNum)%OutletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

        IF (NumAlphas > 5) THEN
          Fan(FanNum)%EndUseSubcategoryName = cAlphaArgs(6)
        ELSE
          Fan(FanNum)%EndUseSubcategoryName = 'General'
        END IF

        CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(4),cAlphaArgs(5),'Air Nodes')

      END DO   ! end Number of Variable Volume FAN Loop

      DO ExhFanNum = 1,  NumZoneExhFan
        FanNum = NumSimpFan + NumVarVolFan + ExhFanNum
        cCurrentModuleObject= 'Fan:ZoneExhaust'
        CALL GetObjectItem(cCurrentModuleObject,ExhFanNum,cAlphaArgs,NumAlphas, &
                           rNumericArgs,NumNums,IOSTAT, &
                           NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                           AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(cAlphaArgs(1),Fan%FanName,FanNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) cAlphaArgs(1)='xxxxx'
        ENDIF
        Fan(FanNum)%FanName = cAlphaArgs(1)
        Fan(FanNum)%FanType = cCurrentModuleObject
        Fan(FanNum)%AvailSchedName = cAlphaArgs(2)
        IF (lAlphaFieldBlanks(2)) THEN
          Fan(FanNum)%AvailSchedPtrNum = ScheduleAlwaysOn
        ELSE
          Fan(FanNum)%AvailSchedPtrNum = GetScheduleIndex(cAlphaArgs(2))
          IF (Fan(FanNum)%AvailSchedPtrNum == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(2))//  &
               ' entered ='//TRIM(cAlphaArgs(2))// &
               ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
            ErrorsFound=.true.
          ELSE
            IF (HasFractionalScheduleValue(Fan(FanNum)%AvailSchedPtrNum)) THEN
              CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(Fan(FanNum)%FanName)//  &
                '" has fractional values in Schedule='//TRIM(cAlphaArgs(2))//'. Only 0.0 in the schedule value turns the fan off.')
            ENDIF
          END IF
        END IF
        Fan(FanNum)%FanType_Num=FanType_ZoneExhaust

        Fan(FanNum)%FanEff        = rNumericArgs(1)
        Fan(FanNum)%DeltaPress    = rNumericArgs(2)
        Fan(FanNum)%MaxAirFlowRate= rNumericArgs(3)
        Fan(FanNum)%MaxAirFlowRateIsAutosizable = .FALSE.
        Fan(FanNum)%MotEff        = 1.0d0
        Fan(FanNum)%MotInAirFrac  = 1.0d0
        Fan(FanNum)%MinAirFlowRate= 0.0d0
        Fan(FanNum)%RhoAirStdInit = StdRhoAir
        Fan(FanNum)%MaxAirMassFlowRate = Fan(FanNum)%MaxAirFlowRate * Fan(FanNum)%RhoAirStdInit

        IF (Fan(FanNum)%MaxAirFlowRate == 0.0d0) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(Fan(FanNum)%FanName)//  &
              '" has specified 0.0 max air flow rate. It will not be used in the simulation.')
        ENDIF

        Fan(FanNum)%InletNodeNum  = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
        Fan(FanNum)%OutletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)


        IF (NumAlphas > 4 .AND. .NOT. lAlphaFieldBlanks(5)) THEN
          Fan(FanNum)%EndUseSubcategoryName = cAlphaArgs(5)
        ELSE
          Fan(FanNum)%EndUseSubcategoryName = 'General'
        END IF

        IF (NumAlphas > 5 .AND. .NOT. lAlphaFieldBlanks(6)) THEN
          Fan(FanNum)%FlowFractSchedNum = GetScheduleIndex(cAlphaArgs(6))
          IF (Fan(FanNum)%FlowFractSchedNum == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(6))//  &
               ' entered ='//TRIM(cAlphaArgs(6))// &
               ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
            ErrorsFound=.true.
          ELSEIF (Fan(FanNum)%FlowFractSchedNum > 0) THEN
            IF (.NOT.CheckScheduleValueMinMax(Fan(FanNum)%FlowFractSchedNum,'>=',0.0D0,'<=',1.0D0)) THEN
              CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(6))//  &
                   ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
              CALL ShowContinueError('Error found in '//TRIM(cAlphaFieldNames(6))//' = '//TRIM(cAlphaArgs(6)) )
              CALL ShowContinueError('Schedule values must be (>=0., <=1.)')
              ErrorsFound=.true.
            ENDIF
          ENDIF
        ELSE
          Fan(FanNum)%FlowFractSchedNum = ScheduleAlwaysOn
        ENDIF

        IF (NumAlphas > 6 .AND. .NOT. lAlphaFieldBlanks(7)) THEN
          SELECT CASE ( TRIM(cAlphaArgs(7)) )
          CASE ( 'COUPLED' )
            Fan(FanNum)%AvailManagerMode = ExhaustFanCoupledToAvailManagers
          CASE ( 'DECOUPLED')
            Fan(FanNum)%AvailManagerMode = ExhaustFanDecoupledFromAvailManagers
          CASE DEFAULT
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(7))//  &
               ' entered ='//TRIM(cAlphaArgs(7))// &
               ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
            ErrorsFound=.true.
          END SELECT
        ELSE
          Fan(FanNum)%AvailManagerMode = ExhaustFanCoupledToAvailManagers
        ENDIF

        IF (NumAlphas > 7 .AND. .NOT. lAlphaFieldBlanks(8)) THEN
          Fan(FanNum)%MinTempLimitSchedNum = GetScheduleIndex(cAlphaArgs(8))
          IF (Fan(FanNum)%MinTempLimitSchedNum == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(8))//  &
               ' entered ='//TRIM(cAlphaArgs(8))// &
               ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
            ErrorsFound=.true.
          ENDIF
        ELSE
          Fan(FanNum)%MinTempLimitSchedNum = 0
        ENDIF

        IF (NumAlphas > 8 .AND. .NOT. lAlphaFieldBlanks(9)) THEN
          Fan(FanNum)%BalancedFractSchedNum = GetScheduleIndex(cAlphaArgs(9))
          IF (Fan(FanNum)%BalancedFractSchedNum == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(9))//  &
               ' entered ='//TRIM(cAlphaArgs(9))// &
               ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
            ErrorsFound=.true.
          ELSEIF (Fan(FanNum)%BalancedFractSchedNum > 0) THEN
            IF (.NOT.CheckScheduleValueMinMax(Fan(FanNum)%BalancedFractSchedNum,'>=',0.0D0,'<=',1.0D0)) THEN
              CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(9))//  &
                   ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
              CALL ShowContinueError('Error found in '//TRIM(cAlphaFieldNames(9))//' = '//TRIM(cAlphaArgs(9)) )
              CALL ShowContinueError('Schedule values must be (>=0., <=1.)')
              ErrorsFound=.true.
            ENDIF
          ENDIF
        ELSE
          Fan(FanNum)%BalancedFractSchedNum = 0
        ENDIF

        ! Component sets not setup yet for zone equipment
        ! CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4),'Air Nodes')

      END DO   ! end of Zone Exhaust Fan loop

      DO OnOffFanNum = 1,  NumOnOff
        FanNum = NumSimpFan + NumVarVolFan + NumZoneExhFan + OnOffFanNum
        cCurrentModuleObject= 'Fan:OnOff'
        CALL GetObjectItem(cCurrentModuleObject,OnOffFanNum,cAlphaArgs,NumAlphas, &
                           rNumericArgs,NumNums,IOSTAT, &
                           NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                           AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(cAlphaArgs(1),Fan%FanName,FanNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) cAlphaArgs(1)='xxxxx'
        ENDIF
        Fan(FanNum)%FanName  = cAlphaArgs(1)
        Fan(FanNum)%FanType  = cCurrentModuleObject
        Fan(FanNum)%AvailSchedName = cAlphaArgs(2)
        IF (lAlphaFieldBlanks(2)) THEN
          Fan(FanNum)%AvailSchedPtrNum = ScheduleAlwaysOn
        ELSE
          Fan(FanNum)%AvailSchedPtrNum = GetScheduleIndex(cAlphaArgs(2))
          IF (Fan(FanNum)%AvailSchedPtrNum == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(2))//  &
               ' entered ='//TRIM(cAlphaArgs(2))// &
               ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
            ErrorsFound=.true.
          END IF
        END IF
        Fan(FanNum)%FanType_Num=FanType_SimpleOnOff

        Fan(FanNum)%FanEff        = rNumericArgs(1)
        Fan(FanNum)%DeltaPress    = rNumericArgs(2)
        Fan(FanNum)%MaxAirFlowRate= rNumericArgs(3)
        IF (Fan(FanNum)%MaxAirFlowRate == 0.0d0) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(Fan(FanNum)%FanName)//  &
              '" has specified 0.0 max air flow rate. It will not be used in the simulation.')
        ENDIF
        Fan(FanNum)%MaxAirFlowRateIsAutosizable = .TRUE.
!       the following two structure variables are set here, as well as in InitFan, for the Heat Pump:Water Heater object
!       (Standard Rating procedure may be called before BeginEnvirFlag is set to TRUE, if so MaxAirMassFlowRate = 0)
        Fan(FanNum)%RhoAirStdInit = StdRhoAir
        Fan(FanNum)%MaxAirMassFlowRate = Fan(FanNum)%MaxAirFlowRate * Fan(FanNum)%RhoAirStdInit

        Fan(FanNum)%MotEff        = rNumericArgs(4)
        Fan(FanNum)%MotInAirFrac  = rNumericArgs(5)
        Fan(FanNum)%MinAirFlowRate= 0.0d0

        Fan(FanNum)%InletNodeNum  = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
        Fan(FanNum)%OutletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

        IF (NumAlphas > 4 .AND. .NOT. lAlphaFieldBlanks(5)) THEN
          Fan(FanNum)%FanPowerRatAtSpeedRatCurveIndex  = GetCurveIndex(cAlphaArgs(5))
        END IF

        IF (NumAlphas > 5 .AND. .NOT. lAlphaFieldBlanks(6)) THEN
          Fan(FanNum)%FanEffRatioCurveIndex  = GetCurveIndex(cAlphaArgs(6))
        END IF

        IF (NumAlphas > 6 .AND. .NOT. lAlphaFieldBlanks(7)) THEN
          Fan(FanNum)%EndUseSubcategoryName = cAlphaArgs(7)
        ELSE
          Fan(FanNum)%EndUseSubcategoryName = 'General'
        END IF

        CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(3),cAlphaArgs(4),'Air Nodes')


      END DO   ! end Number of Simple  ON-OFF FAN Loop


      cCurrentModuleObject= 'FanPerformance:NightVentilation'
      NumNightVentPerf = GetNumObjectsFound(cCurrentModuleObject)

      IF (NumNightVentPerf > 0) THEN
        ALLOCATE(NightVentPerf(NumNightVentPerf))
        NightVentPerf%FanName = ' '
        NightVentPerf%FanEff = 0.0d0
        NightVentPerf%DeltaPress = 0.0d0
        NightVentPerf%MaxAirFlowRate = 0.0d0
        NightVentPerf%MotEff = 0.0d0
        NightVentPerf%MotInAirFrac = 0.0d0
        NightVentPerf%MaxAirMassFlowRate = 0.0d0
      END IF
      ! input the night ventilation performance objects
      DO NVPerfNum=1,NumNightVentPerf
         CALL GetObjectItem(cCurrentModuleObject,NVPerfNum,cAlphaArgs,NumAlphas, &
                           rNumericArgs,NumNums,IOSTAT, &
                           NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                           AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(cAlphaArgs(1),NightVentPerf%FanName,NVPerfNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) cAlphaArgs(1)='xxxxx'
        ENDIF
        NightVentPerf(NVPerfNum)%FanName        = cAlphaArgs(1)
        NightVentPerf(NVPerfNum)%FanEff         = rNumericArgs(1)
        NightVentPerf(NVPerfNum)%DeltaPress     = rNumericArgs(2)
        NightVentPerf(NVPerfNum)%MaxAirFlowRate = rNumericArgs(3)
        NightVentPerf(NVPerfNum)%MotEff         = rNumericArgs(4)
        NightVentPerf(NVPerfNum)%MotInAirFrac   = rNumericArgs(5)
        ! find the corresponding fan
        NVPerfFanFound = .FALSE.
        DO FanNum=1,NumFans
          IF (NightVentPerf(NVPerfNum)%FanName == Fan(FanNum)%FanName) THEN
            NVPerfFanFound = .TRUE.
            Fan(FanNum)%NVPerfNum = NVPerfNum
            EXIT
          END IF
        END DO
        IF ( .NOT. NVPerfFanFound) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', fan name not found='//TRIM(cAlphaArgs(1)))
          ErrorsFound=.true.
        END IF

      END DO

      !cpw22Aug2010 Added get input for Component Fan Model
      DO CompModelFanNum = 1,  NumCompModelFan
        FanNum = NumSimpFan + NumVarVolFan + NumZoneExhFan + NumOnOff + CompModelFanNum

        cCurrentModuleObject= 'Fan:ComponentModel'
        CALL GetObjectItem(cCurrentModuleObject,CompModelFanNum,cAlphaArgs,NumAlphas, &
                           rNumericArgs,NumNums,IOSTAT, &
                           NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                           AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
        IsNotOK=.false.
        IsBlank=.false.
        CALL VerifyName(cAlphaArgs(1),Fan%FanName,FanNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
        IF (IsNotOK) THEN
          ErrorsFound=.true.
          IF (IsBlank) cAlphaArgs(1)='xxxxx'
        ENDIF
        Fan(FanNum)%FanName = cAlphaArgs(1) ! Fan name
        Fan(FanNum)%FanType = cCurrentModuleObject

        Fan(FanNum)%InletNodeNum  = &
               GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent) ! Air inlet node name
        Fan(FanNum)%OutletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                            NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent) ! Air outlet node name

        CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(2),cAlphaArgs(3),'Air Nodes')

        Fan(FanNum)%AvailSchedName = cAlphaArgs(4) ! Availability schedule name
        IF (lAlphaFieldBlanks(4)) THEN
          Fan(FanNum)%AvailSchedPtrNum =0
        ELSE
          Fan(FanNum)%AvailSchedPtrNum =GetScheduleIndex(cAlphaArgs(4))
          IF (Fan(FanNum)%AvailSchedPtrNum == 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(4))//  &
               ' entered ='//TRIM(cAlphaArgs(4))// &
               ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
            ErrorsFound=.true.
          END IF
        ENDIF

        Fan(FanNum)%FanType_Num=FanType_ComponentModel

        Fan(FanNum)%MaxAirFlowRate= rNumericArgs(1)
        IF (Fan(FanNum)%MaxAirFlowRate == 0.0d0) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(Fan(FanNum)%FanName)//  &
             '" has specified 0.0 max air flow rate. It will not be used in the simulation.')
        ENDIF
        Fan(FanNum)%MaxAirFlowRateIsAutosizable = .TRUE.
        Fan(FanNum)%MinAirFlowRate= rNumericArgs(2)

        Fan(FanNum)%FanSizingFactor = rNumericArgs(3)    ! Fan max airflow sizing factor [-] cpw31Aug2010
        Fan(FanNum)%FanWheelDia = rNumericArgs(4)        ! Fan wheel outer diameter [m]
        Fan(FanNum)%FanOutletArea = rNumericArgs(5)      ! Fan outlet area [m2]
        Fan(FanNum)%FanMaxEff = rNumericArgs(6)          ! Fan maximum static efficiency [-]
        Fan(FanNum)%EuMaxEff = rNumericArgs(7)           ! Euler number at Fan maximum static efficiency [-]
        Fan(FanNum)%FanMaxDimFlow = rNumericArgs(8)      ! Fan maximum dimensionless airflow [-]
        Fan(FanNum)%PulleyDiaRatio = rNumericArgs(9)     ! Motor/fan pulley diameter ratio [-]
        Fan(FanNum)%BeltMaxTorque = rNumericArgs(10)     ! Belt maximum torque [N-m, autosizable]
        Fan(FanNum)%BeltSizingFactor = rNumericArgs(11)  ! Belt sizing factor [-]
        Fan(FanNum)%BeltTorqueTrans  = rNumericArgs(12)  ! Belt fractional torque transition Region 1-2 [-]
        Fan(FanNum)%MotorMaxSpd = rNumericArgs(13)       ! Motor maximum speed [rpm]
        Fan(FanNum)%MotorMaxOutPwr = rNumericArgs(14)    ! Motor maximum output power [W, autosizable]
        Fan(FanNum)%MotorSizingFactor = rNumericArgs(15) ! Motor sizing factor [-]
        Fan(FanNum)%MotInAirFrac  = rNumericArgs(16)     ! Fraction of fan and motor losses to airstream [-]
        Fan(FanNum)%VFDEffType = cAlphaArgs(5)           ! VFD efficiency type [Speed or Power]
        Fan(FanNum)%VFDMaxOutPwr = rNumericArgs(17)      ! VFD maximum output power [W, autosizable]
        Fan(FanNum)%VFDSizingFactor = rNumericArgs(18)   ! VFD sizing factor [-] cpw31Aug2010
        Fan(FanNum)%PressRiseCurveIndex = GetCurveIndex(cAlphaArgs(6))      ! Fan pressure rise curve
        Fan(FanNum)%PressResetCurveIndex = GetCurveIndex(cAlphaArgs(7))     ! Duct static pressure reset curve
        Fan(FanNum)%PLFanEffNormCurveIndex = GetCurveIndex(cAlphaArgs(8))   ! Fan part-load eff (normal) curve
        Fan(FanNum)%PLFanEffStallCurveIndex = GetCurveIndex(cAlphaArgs(9))  ! Fan part-load eff (stall) curve
        Fan(FanNum)%DimFlowNormCurveIndex = GetCurveIndex(cAlphaArgs(10))   ! Fan dim airflow (normal) curve
        Fan(FanNum)%DimFlowStallCurveIndex = GetCurveIndex(cAlphaArgs(11))  ! Fan dim airflow (stall) curve
        Fan(FanNum)%BeltMaxEffCurveIndex = GetCurveIndex(cAlphaArgs(12))    ! Belt max eff curve
        Fan(FanNum)%PLBeltEffReg1CurveIndex = GetCurveIndex(cAlphaArgs(13)) ! Belt part-load eff Region 1 curve
        Fan(FanNum)%PLBeltEffReg2CurveIndex = GetCurveIndex(cAlphaArgs(14)) ! Belt part-load eff Region 2 curve
        Fan(FanNum)%PLBeltEffReg3CurveIndex = GetCurveIndex(cAlphaArgs(15)) ! Belt part-load eff Region 3 curve
        Fan(FanNum)%MotorMaxEffCurveIndex = GetCurveIndex(cAlphaArgs(16))   ! Motor max eff curve
        Fan(FanNum)%PLMotorEffCurveIndex = GetCurveIndex(cAlphaArgs(17))    ! Motor part-load eff curve
        Fan(FanNum)%VFDEffCurveIndex = GetCurveIndex(cAlphaArgs(18))        ! VFD eff curve

        IF (NumAlphas > 18) THEN
          Fan(FanNum)%EndUseSubcategoryName = cAlphaArgs(19)
        ELSE
          Fan(FanNum)%EndUseSubcategoryName = 'General'
        END IF

      END DO   ! end Number of Component Model FAN Loop

      DEALLOCATE(cAlphaArgs)
      DEALLOCATE(cAlphaFieldNames)
      DEALLOCATE(lAlphaFieldBlanks)
      DEALLOCATE(cNumericFieldNames)
      DEALLOCATE(lNumericFieldBlanks)
      DEALLOCATE(rNumericArgs)

      ! Check Fans
      DO FanNum=1,NumFans
        DO checkNum=FanNum+1,NumFans
          IF (Fan(FanNum)%InletNodeNum == Fan(checkNum)%InletNodeNum) THEN
            ErrorsFound=.true.
            CALL ShowSevereError('GetFanInput, duplicate fan inlet node names, must be unique for fans.')
            CALL ShowContinueError('Fan='//trim(Fan(FanNum)%FanType)//':'//trim(Fan(FanNum)%FanName)//  &
               ' and Fan='//trim(Fan(checkNum)%FanType)//':'//trim(Fan(checkNum)%FanName)//'.')
            CALL ShowContinueError('Inlet Node Name="'//trim(NodeID(Fan(FanNum)%InletNodeNum))//'".')
          ENDIF
          IF (Fan(FanNum)%OutletNodeNum == Fan(checkNum)%OutletNodeNum) THEN
            ErrorsFound=.true.
            CALL ShowSevereError('GetFanInput, duplicate fan outlet node names, must be unique for fans.')
            CALL ShowContinueError('Fan='//trim(Fan(FanNum)%FanType)//':'//trim(Fan(FanNum)%FanName)//  &
               ' and Fan='//trim(Fan(checkNum)%FanType)//':'//trim(Fan(checkNum)%FanName)//'.')
            CALL ShowContinueError('Outlet Node Name="'//trim(NodeID(Fan(FanNum)%OutletNodeNum))//'".')
          ENDIF
        ENDDO
      ENDDO

      IF (ErrorsFound) THEN
            CALL ShowFatalError(RoutineName//'Errors found in input.  Program terminates.')
      ENDIF

      Do FanNum=1,NumFans
             ! Setup Report variables for the Fans  CurrentModuleObject='Fans'
       CALL SetupOutputVariable('Fan Electric Power [W]', Fan(FanNum)%FanPower, 'System','Average',Fan(FanNum)%FanName)
       CALL SetupOutputVariable('Fan Rise in Air Temperature [deltaC]', Fan(FanNum)%DeltaTemp,  &
                                'System','Average',Fan(FanNum)%FanName)
       CALL SetupOutputVariable('Fan Electric Energy [J]', Fan(FanNum)%FanEnergy, 'System','Sum',Fan(FanNum)%FanName, &
                                 ResourceTypeKey='Electric',GroupKey='System', &
                                 EndUseKey='Fans',EndUseSubKey=Fan(FanNum)%EndUseSubcategoryName)

       IF ((Fan(FanNum)%FanType_Num == FanType_ZoneExhaust) .and.  (Fan(FanNum)%BalancedFractSchedNum > 0)) THEN
         CALL SetupOutputVariable('Fan Unbalanced Air Mass Flow Rate [kg/s]',   &
                  Fan(FanNum)%UnbalancedOutletMassFlowRate, 'System','Average',Fan(FanNum)%FanName)
         CALL SetupOutputVariable('Fan Balanced Air Mass Flow Rate [kg/s]',   &
                  Fan(FanNum)%BalancedOutletMassFlowRate, 'System','Average',Fan(FanNum)%FanName)
       ENDIF

       IF (AnyEnergyManagementSystemInModel) THEN

         CALL SetupEMSInternalVariable('Fan Maximum Mass Flow Rate', Fan(FanNum)%FanName, '[kg/s]', &
                                  Fan(FanNum)%MaxAirMassFlowRate  )
         CALL SetupEMSActuator('Fan', Fan(FanNum)%FanName, 'Fan Air Mass Flow Rate' , '[kg/s]', &
                                  Fan(FanNum)%EMSMaxMassFlowOverrideOn, Fan(FanNum)%EMSAirMassFlowValue )
         CALL SetupEMSInternalVariable('Fan Nominal Pressure Rise', Fan(FanNum)%FanName, '[Pa]', &
                                  Fan(FanNum)%DeltaPress  )
         CALL SetupEMSActuator('Fan', Fan(FanNum)%FanName, 'Fan Pressure Rise' , '[Pa]', &
                                  Fan(FanNum)%EMSFanPressureOverrideON, Fan(FanNum)%EMSFanPressureValue )
         CALL SetupEMSInternalVariable('Fan Nominal Total Efficiency', Fan(FanNum)%FanName, '[fraction]', &
                                  Fan(FanNum)%FanEff  )
         CALL SetupEMSActuator('Fan', Fan(FanNum)%FanName, 'Fan Total Efficiency' , '[fraction]', &
                                  Fan(FanNum)%EMSFanEffOverrideOn, Fan(FanNum)%EMSFanEffValue )

         CALL SetupEMSActuator('Fan', Fan(FanNum)%FanName, 'Fan Autosized Air Flow Rate' , '[m3/s]', &
                                  Fan(FanNum)%MaxAirFlowRateEMSOverrideOn, Fan(FanNum)%MaxAirFlowRateEMSOverrideValue )
       ENDIF
      END DO

      DO OnOffFanNum = 1,  NumOnOff
       FanNum = NumSimpFan + NumVarVolFan + NumZoneExhFan + OnOffFanNum
       CALL SetupOutputVariable('Fan Runtime Fraction []', Fan(FanNum)%FanRuntimeFraction, 'System','Average', &
                                 Fan(FanNum)%FanName)
      END DO

  CALL ManageEMS(emsCallFromComponentGetInput)

  RETURN

END SUBROUTINE GetFanInput

! End of Get Input subroutines for the HB Module
!******************************************************************************



! Beginning Initialization Section of the Module
!******************************************************************************

SUBROUTINE InitFan(FanNum,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   February 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Fan Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing, ONLY: CurSysNum
  USE DataAirLoop, ONLY: AirLoopControlInfo
  USE DataZoneEquipment, ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList
  USE InputProcessor, ONLY: SameString

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT (IN):: FirstHVACIteration !unused1208
  Integer, Intent(IN) :: FanNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  Integer             :: InletNode
  Integer             :: OutletNode
!unused0909  Integer             :: InNode
  Integer             :: OutNode
  LOGICAL,SAVE        :: MyOneTimeFlag = .true.
  LOGICAL,SAVE        :: ZoneEquipmentListChecked = .false.  ! True after the Zone Equipment List has been checked for items
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MySizeFlag
  INTEGER :: Loop

          ! FLOW:

  IF (MyOneTimeFlag) THEN

    ALLOCATE(MyEnvrnFlag(NumFans))
    ALLOCATE(MySizeFlag(NumFans))
    MyEnvrnFlag = .TRUE.
    MySizeFlag = .TRUE.

    MyOneTimeFlag = .false.

  END IF

  ! need to check all fans to see if they are on Zone Equipment List or issue warning
  IF (.not. ZoneEquipmentListChecked .and. ZoneEquipInputsFilled) THEN
    ZoneEquipmentListChecked=.true.
    DO Loop=1,NumFans
      IF (.NOT. SameString(Fan(Loop)%FanType , 'Fan:ZoneExhaust')) CYCLE
      IF (CheckZoneEquipmentList(Fan(Loop)%FanType,Fan(Loop)%FanName)) CYCLE
      CALL ShowSevereError('InitFans: Fan=['//TRIM(Fan(Loop)%FanType)//','//TRIM(Fan(Loop)%FanName)//  &
           '] is not on any ZoneHVAC:EquipmentList.  It will not be simulated.')
    ENDDO
  ENDIF

  IF ( .NOT. SysSizingCalc .AND. MySizeFlag(FanNum)) THEN

    CALL SizeFan(FanNum)
    ! Set the loop cycling flag
    IF (Fan(FanNum)%FanType_Num == FanType_SimpleOnOff) THEN
      IF (CurSysNum > 0) THEN
        AirLoopControlInfo(CurSysNum)%CyclingFan = .TRUE.
      END IF
    END IF

    MySizeFlag(FanNum) = .FALSE.
  END IF


! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(FanNum)) THEN

    !For all Fan inlet nodes convert the Volume flow to a mass flow
!unused0909    InNode = Fan(FanNum)%InletNodeNum
    OutNode = Fan(FanNum)%OutletNodeNum
    Fan(FanNum)%RhoAirStdInit = StdRhoAir

    !Change the Volume Flow Rates to Mass Flow Rates

    Fan(FanNum)%MaxAirMassFlowRate = Fan(FanNum)%MaxAirFlowRate *  Fan(FanNum)%RhoAirStdInit
    If (Fan(FanNum)%FanMinAirFracMethod == MinFrac) Then
        Fan(FanNum)%MinAirFlowRate = Fan(FanNum)%MaxAirFlowRate * Fan(FanNum)%FanMinFrac
        Fan(FanNum)%MinAirMassFlowRate = Fan(FanNum)%MinAirFlowRate * Fan(FanNum)%RhoAirStdInit
    ELSE IF (Fan(FanNum)%FanMinAirFracMethod == FixedMin) Then
        Fan(FanNum)%MinAirFlowRate = Fan(FanNum)%FanFixedMin
        Fan(FanNum)%MinAirMassFlowRate = Fan(FanNum)%MinAirFlowRate * Fan(FanNum)%RhoAirStdInit
    END IF
    IF (Fan(FanNum)%NVPerfNum > 0) THEN
      NightVentPerf(Fan(FanNum)%NVPerfNum)%MaxAirMassFlowRate = NightVentPerf(Fan(FanNum)%NVPerfNum)%MaxAirFlowRate &
                                                                  * Fan(FanNum)%RhoAirStdInit
    END IF


    !Init the Node Control variables
    Node(OutNode)%MassFlowRateMax      = Fan(FanNum)%MaxAirMassFlowRate
    Node(OutNode)%MassFlowRateMin      = Fan(FanNum)%MinAirMassFlowRate


    !Initialize all report variables to a known state at beginning of simulation
    Fan(FanNum)%FanPower = 0.0d0
    Fan(FanNum)%DeltaTemp = 0.0d0
    Fan(FanNum)%FanEnergy = 0.0d0

    MyEnvrnFlag(FanNum) = .FALSE.
  END IF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(FanNum) = .true.
  ENDIF

  ! Do the Begin Day initializations
    ! none

  ! Do the begin HVAC time step initializations
    ! none

  ! Do the following initializations (every time step): This should be the info from
  ! the previous components outlets or the node data in this section.

  ! Do a check and make sure that the max and min available(control) flow is
  ! between the physical max and min for the Fan while operating.

  InletNode = Fan(FanNum)%InletNodeNum
  OutletNode = Fan(FanNum)%OutletNodeNum

  Fan(FanNum)%MassFlowRateMaxAvail = MIN(Node(OutletNode)%MassFlowRateMax, &
                                              Node(InletNode)%MassFlowRateMaxAvail)
  Fan(FanNum)%MassFlowRateMinAvail = MIN(MAX(Node(OutletNode)%MassFlowRateMin, &
                                             Node(InletNode)%MassFlowRateMinAvail), &
                                             Node(InletNode)%MassFlowRateMaxAvail)

  ! Load the node data in this section for the component simulation
  !
  !First need to make sure that the massflowrate is between the max and min avail.
  IF (Fan(FanNum)%FanType_Num /= FanType_ZoneExhaust ) THEN
    Fan(FanNum)%InletAirMassFlowRate = Min(Node(InletNode)%MassFlowRate, &
                                           Fan(FanNum)%MassFlowRateMaxAvail)
    Fan(FanNum)%InletAirMassFlowRate = Max(Fan(FanNum)%InletAirMassFlowRate, &
                                           Fan(FanNum)%MassFlowRateMinAvail)
  ELSE  ! zone exhaust fans
    Fan(FanNum)%MassFlowRateMaxAvail = Fan(FanNum)%MaxAirMassFlowRate
    Fan(FanNum)%MassFlowRateMinAvail = 0.0d0
    IF (Fan(FanNum)%FlowFractSchedNum > 0) THEN ! modulate flow
      Fan(FanNum)%InletAirMassFlowRate = Fan(FanNum)%MassFlowRateMaxAvail  &
                                * GetCurrentScheduleValue(Fan(FanNum)%FlowFractSchedNum)
      Fan(FanNum)%InletAirMassFlowRate = MAX(0.d0, Fan(FanNum)%InletAirMassFlowRate)
    ELSE ! always run at max
      Fan(FanNum)%InletAirMassFlowRate = Fan(FanNum)%MassFlowRateMaxAvail
    ENDIF
    IF (Fan(FanNum)%EMSMaxMassFlowOverrideOn) Fan(FanNum)%InletAirMassFlowRate = &
       MIN(Fan(FanNum)%EMSAirMassFlowValue,Fan(FanNum)%MassFlowRateMaxAvail)
  END IF

  !Then set the other conditions
  Fan(FanNum)%InletAirTemp         = Node(InletNode)%Temp
  Fan(FanNum)%InletAirHumRat       = Node(InletNode)%HumRat
  Fan(FanNum)%InletAirEnthalpy     = Node(InletNode)%Enthalpy

  RETURN

END SUBROUTINE InitFan

SUBROUTINE SizeFan(FanNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   September 2001
          !       MODIFIED       Craig Wray August 2010 - added fan, belt, motor, and VFD component sizing
          !                      August 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing fans for which flow rates have not been
          ! specified in the input, or when fan component sizes have not been specified

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone or system sizing arrays.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE OutputReportPredefined
  USE CurveManager, ONLY: CurveValue
  USE CurveManager, ONLY: GetCurveIndex
  USE General, ONLY: RoundSigDigits
  USE ReportSizingManager, ONLY: ReportSizingOutput

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: FanNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: FanMinAirFlowRate  ! minimum air flow rate [m3/s]
  Integer :: NVPerfNum            ! Index to night ventialation performance object
  CHARACTER(len=MaxNameLength) :: equipName ! Equipment name
  REAL(r64) :: RatedPower         ! Rated fan power [W]
  REAL(r64) :: RhoAir             ! Air density [kg/m3]
  REAL(r64) :: FanVolFlow         ! Fan volumetric airflow [m3/s]
  REAL(r64) :: DuctStaticPress    ! Duct static pressure setpoint [Pa]
  REAL(r64) :: DeltaPressTot      ! Total pressure rise across fan [N/m2 = Pa]
  REAL(r64) :: FanOutletVelPress  ! Fan outlet velocity pressure [Pa]
  REAL(r64) :: EulerNum           ! Fan Euler number [-]
  REAL(r64) :: NormalizedEulerNum ! Normalized Fan Euler number [-]
  REAL(r64) :: FanDimFlow         ! Fan dimensionless airflow [-]
  REAL(r64) :: FanSpdRadS         ! Fan shaft rotational speed [rad/s]
  REAL(r64) :: MotorSpeed         ! Motor shaft rotational speed [rpm]
  REAL(r64) :: XbeltMax           ! Factor for belt max eff curve [ln hp]
  REAL(r64) :: FanTrqRatio        ! Ratio of fan torque to max fan torque [-]
  REAL(r64) :: BeltPLEff          ! Belt normalized (part-load) efficiency [-]
  REAL(r64) :: XmotorMax          ! Factor for motor max eff curve [ln hp]
  REAL(r64) :: MotorOutPwrRatio   ! Ratio of motor output power to max motor output power [-]
  REAL(r64) :: MotorPLEff         ! Motor normalized (part-load) efficiency [-]
  REAL(r64) :: VFDSpdRatio    = 0.d0 ! Ratio of motor speed to motor max speed [-]
  REAL(r64) :: VFDOutPwrRatio = 0.d0 ! Ratio of VFD output power to max VFD output power [-]
  LOGICAL   :: OASysFlag          ! Logical flag determines if parent object set OA Sys coil property
  LOGICAL   :: AirLoopSysFlag     ! Logical flag determines if parent object set air loop coil property
  REAL(r64) :: MaxAirFlowRateDes     ! Design maximum air flow rate for reporting
  REAL(r64) :: MaxAirFlowRateUser    ! User hard-sized maximum air flow rate for reproting
  REAL(r64) :: MinAirFlowRateDes     ! Design minimum air flow rate for reporting
  REAL(r64) :: MinAirFlowRateUser    ! User hard-sized minimum air flow rate for reproting  
  LOGICAL   :: IsAutosize            ! Indicator to autosize
  LOGICAL   :: HardSizeNoDesRun      ! Indicator to hardsize with no disign run
  LOGICAL :: SizingDesRunThisAirSys            ! true if a particular air system had a Sizing:System object and system sizing done
  LOGICAL :: SizingDesRunThisZone              ! true if a particular zone had a Sizing:Zone object and zone sizing was done

  FanMinAirFlowRate = 0.0d0
  NVPerfNum  = Fan(FanNum)%NVPerfNum
  MaxAirFlowRateDes = 0.0d0
  MaxAirFlowRateUser = 0.0d0
  IsAutosize = .FALSE.
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

  IF (Fan(FanNum)%MaxAirFlowRate == AutoSize) THEN
    IsAutosize = .TRUE.
  END IF  

  IF (CurSysNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. SizingDesRunThisAirSys) THEN ! Simulation continue
      HardSizeNoDesRun = .TRUE.  
      IF (Fan(FanNum)%MaxAirFlowRate > 0.0d0) THEN
        CALL ReportSizingOutput(Fan(FanNum)%FanType, Fan(FanNum)%FanName, &
                   'User-Specified Maximum Flow Rate [m3/s]', Fan(FanNum)%MaxAirFlowRate)
      END IF  
    ELSE ! Autosize or hardsize with sizing run 

      OASysFlag = .FALSE.
      AirLoopSysFlag = .FALSE.
      ! logicals used when parent sizes fan
      IF (CurOASysNum > 0)OASysFlag = OASysEqSizing(CurOASysNum)%AirFlow
      IF (CurSysNum > 0)AirLoopSysFlag = UnitarySysEqSizing(CurSysNum)%AirFlow

      CALL CheckSysSizing(TRIM(Fan(FanNum)%FanType) , &
                           Fan(FanNum)%FanName)

      IF(OASysFlag)THEN
        MaxAirFlowRateDes = OASysEqSizing(CurOASysNum)%AirVolFlow
      ELSE IF(AirLoopSysFlag)THEN
        MaxAirFlowRateDes = UnitarySysEqSizing(CurSysNum)%AirVolFlow
      ELSE
        SELECT CASE(CurDuctType)
          CASE(Main)
            MaxAirFlowRateDes = FinalSysSizing(CurSysNum)%DesMainVolFlow
          CASE(Cooling)
            ! MaxAirFlowRateDes = FinalSysSizing(CurSysNum)%DesCoolVolFlow
            MaxAirFlowRateDes = FinalSysSizing(CurSysNum)%DesMainVolFlow
          CASE(Heating)
            ! MaxAirFlowRateDes = FinalSysSizing(CurSysNum)%DesHeatVolFlow
            MaxAirFlowRateDes = FinalSysSizing(CurSysNum)%DesMainVolFlow
          CASE(Other)
            MaxAirFlowRateDes = FinalSysSizing(CurSysNum)%DesMainVolFlow
          CASE DEFAULT
            MaxAirFlowRateDes = FinalSysSizing(CurSysNum)%DesMainVolFlow
        END SELECT
      END IF

      FanMinAirFlowRate = Fan(FanNum)%MinAirFlowRate
    END IF
  ELSE IF (CurZoneEqNum > 0) THEN
    IF (.NOT. IsAutosize .AND. .NOT. SizingDesRunThisZone) THEN ! Simulation continue
      HardSizeNoDesRun = .TRUE.  
      IF (Fan(FanNum)%MaxAirFlowRate > 0.0d0) THEN
        CALL ReportSizingOutput(Fan(FanNum)%FanType, Fan(FanNum)%FanName, &
                   'User-Specified Maximum Flow Rate [m3/s]', Fan(FanNum)%MaxAirFlowRate)
      END IF  
    ELSE ! Autosize or hardsize with sizing run 
      CALL CheckZoneSizing(TRIM(Fan(FanNum)%FanType) , &
                           Fan(FanNum)%FanName)
      IF(ZoneEqSizing(CurZoneEqNum)%AirFlow)THEN
        MaxAirFlowRateDes = ZoneEqSizing(CurZoneEqNum)%AirVolFlow
      ELSE
        IF(ZoneCoolingOnlyFan)THEN
          MaxAirFlowRateDes = FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow
        ELSE IF (ZoneHeatingOnlyFan) THEN
          MaxAirFlowRateDes = FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow
        ELSE
          MaxAirFlowRateDes = MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                                  FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)
        END IF
      END IF
    END IF    
  END IF
  
  IF (MaxAirFlowRateDes < SmallAirVolFlow) THEN
    MaxAirFlowRateDes = 0.0d0
  END IF

  IF (Fan(FanNum)%MaxAirFlowRateEMSOverrideOn) THEN
    MaxAirFlowRateDes = Fan(FanNum)%MaxAirFlowRateEMSOverrideValue
  ENDIF
  
  IF (.NOT. HardSizeNoDesRun) THEN
    IF (IsAutosize) THEN
      Fan(FanNum)%MaxAirFlowRate = MaxAirFlowRateDes  
      CALL ReportSizingOutput(Fan(FanNum)%FanType, &
                          Fan(FanNum)%FanName, 'Design Size Maximum Flow Rate [m3/s]', MaxAirFlowRateDes)
    ELSE
      IF (Fan(FanNum)%MaxAirFlowRate > 0.0d0 .AND. Fan(FanNum)%MaxAirFlowRateIsAutosizable .AND. MaxAirFlowRateDes > 0.0d0) THEN
        MaxAirFlowRateUser = Fan(FanNum)%MaxAirFlowRate
        CALL ReportSizingOutput(Fan(FanNum)%FanType, Fan(FanNum)%FanName, &
                             'Design Size Maximum Flow Rate [m3/s]', MaxAirFlowRateDes, &
                             'User-Specified Maximum Flow Rate [m3/s]', MaxAirFlowRateUser)
        IF (DisplayExtraWarnings) THEN
          IF ((ABS(MaxAirFlowRateDes - MaxAirFlowRateUser)/MaxAirFlowRateUser) > AutoVsHardSizingThreshold) THEN
            CALL ShowMessage('SizeHVACFans: Potential issue with equipment sizing for '// &
                                    TRIM(Fan(FanNum)%FanType)//' = "'//TRIM(Fan(FanNum)%FanName)//'".')
            CALL ShowContinueError('User-Specified Maximum Flow Rate of '// &
                                    TRIM(RoundSigDigits(MaxAirFlowRateUser,5))// ' [m3/s]')
            CALL ShowContinueError('differs from Design Size Maximum Flow Rate of ' // &
                                    TRIM(RoundSigDigits(MaxAirFlowRateDes,5))// ' [m3/s]')
            CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
            CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
          END IF
        ENDIF
      END IF      
    END IF  
  END IF  
  

  !cpw31Aug2010 Add fan, belt, motor and VFD component autosizing and maximum efficiency calculations
  FanVolFlow = Fan(FanNum)%MaxAirFlowRate !Maximum volumetric airflow through fan [m3/s at standard conditions]
  IF (Fan(FanNum)%FanType_Num == FanType_ComponentModel) THEN
    ! Get air density at standard conditions and get mass airflow through fan
    ! From WeatherManager:
    !   StdBaroPress=(101.325d0*(1.d0-2.25577d-05*WeatherFileElevation)**5.2559d0)*1000.d0
    !   StdRhoAir=PsyRhoAirFnPbTdbW(StdBaroPress,20,0)
    ! From PsychRoutines:
    !   w=MAX(dw,1.0d-5)
    !   rhoair = pb/(287.d0*(tdb+KelvinConv)*(1.d0+1.6077687d0*w))
    RhoAir = StdRhoAir

    ! Adjust max fan volumetric airflow using fan sizing factor
    FanVolFlow = FanVolFlow * Fan(FanNum)%FanSizingFactor ![m3/s at standard conditions]

    ! Calculate max fan static pressure rise using max fan volumetric flow, std air density, air-handling system characteristics,
    !   and Sherman-Wray system curve model (assumes static pressure surrounding air distribution system is zero)
    DuctStaticPress = CurveValue(Fan(FanNum)%PressResetCurveIndex,FanVolFlow) !Duct static pressure setpoint [Pa]
    DeltaPressTot = CurveValue(Fan(FanNum)%PressRiseCurveIndex,FanVolFlow,DuctStaticPress) !Max fan total pressure rise [Pa]
    FanOutletVelPress = 0.5d0 * RhoAir*(FanVolFlow / Fan(FanNum)%FanOutletArea)**2 !Max fan outlet velocity pressure [Pa]
    !Outlet velocity pressure cannot exceed total pressure rise
    FanOutletVelPress = MIN(FanOutletVelPress, DeltaPressTot)
    Fan(FanNum)%DeltaPress = DeltaPressTot - FanOutletVelPress !Max fan static pressure rise [Pa]

    ! Calculate max fan air power using volumetric flow abd corresponding fan static pressure rise
    Fan(FanNum)%FanAirPower = FanVolFlow * Fan(FanNum)%DeltaPress ![W]

    ! Calculate fan wheel efficiency at max fan volumetric flow and corresponding fan static pressure rise,
    !   using fan characteristics and Wray dimensionless fan static efficiency model
    EulerNum = (Fan(FanNum)%DeltaPress * Fan(FanNum)%FanWheelDia**4) / (RhoAir * FanVolFlow**2) ![-]
    NormalizedEulerNum = LOG10(EulerNum / Fan(FanNum)%EuMaxEff)
    IF (NormalizedEulerNum <= 0.d0) THEN
      Fan(FanNum)%FanWheelEff = CurveValue(Fan(FanNum)%PLFanEffNormCurveIndex,NormalizedEulerNum)
    ELSE
      Fan(FanNum)%FanWheelEff = CurveValue(Fan(FanNum)%PLFanEffStallCurveIndex,NormalizedEulerNum)
    END IF
    Fan(FanNum)%FanWheelEff = Fan(FanNum)%FanWheelEff * Fan(FanNum)%FanMaxEff ! [-]
    Fan(FanNum)%FanWheelEff = MAX(Fan(FanNum)%FanWheelEff,0.01d0) !Minimum efficiency is 1% to avoid numerical errors

    ! Calculate max fan shaft power using fan air power and fan efficiency
    ! at max fan static pressure rise and max fan volumetric flow
    Fan(FanNum)%FanShaftPower = (Fan(FanNum)%FanAirPower / Fan(FanNum)%FanWheelEff) ![W]
    Fan(FanNum)%FanShaftPwrMax = Fan(FanNum)%FanShaftPower ![W]

    ! Calculate fan shaft speed, motor speed, and fan torque using Wray dimensionless fan airflow model
    IF (NormalizedEulerNum <= 0.d0) THEN
      FanDimFlow = CurveValue(Fan(FanNum)%DimFlowNormCurveIndex,NormalizedEulerNum) ![-]
    ELSE
      FanDimFlow = CurveValue(Fan(FanNum)%DimFlowStallCurveIndex,NormalizedEulerNum) ![-]
    END IF
    FanSpdRadS = FanVolFlow / &
     (FanDimFlow * Fan(FanNum)%FanMaxDimFlow * Fan(FanNum)%FanWheelDia**3) ![rad/s]
    Fan(FanNum)%FanSpd =  FanSpdRadS * 9.549296586d0 ![rpm, conversion factor is 30/PI]

    IF (Fan(FanNum)%PulleyDiaRatio == AutoSize) THEN
      !WRITE(*,*) 'Autosizing pulley drive ratio'
      Fan(FanNum)%PulleyDiaRatio = Fan(FanNum)%FanSpd / Fan(FanNum)%MotorMaxSpd ![-]
    END IF

    ! For direct-drive, should have PulleyDiaRatio = 1
    MotorSpeed = Fan(FanNum)%FanSpd / Fan(FanNum)%PulleyDiaRatio ![rpm]

    ! Check for inconsistent drive ratio and motor speed, and report design fan speed with warning cpw14Sep2010
    IF (MotorSpeed > (Fan(FanNum)%MotorMaxSpd + 1.d-5)) THEN
      CALL ShowWarningError('Drive ratio for '//TRIM(Fan(FanNum)%FanType)//': '//TRIM(Fan(FanNum)%FanName)// &
         ' is too low at design conditions -- check motor speed and drive ratio inputs')
      CALL ShowContinueError('...Design fan speed [rev/min]: '// &
         TRIM(RoundSigDigits(Fan(FanNum)%FanSpd,2)))
    END IF

    Fan(FanNum)%FanTrq = Fan(FanNum)%FanShaftPower / FanSpdRadS ![N-m]

    IF (Fan(FanNum)%BeltMaxTorque == AutoSize) THEN
      !WRITE(*,*) 'Autosizing fan belt'
      Fan(FanNum)%BeltMaxTorque = Fan(FanNum)%FanTrq ![N-m]
    END IF
    ! Adjust max belt torque using belt sizing factor
    Fan(FanNum)%BeltMaxTorque = Fan(FanNum)%BeltMaxTorque * Fan(FanNum)%BeltSizingFactor ![N-m]

    ! Check for undersized belt and report design size with warning cpw14Sep2010
    IF (Fan(FanNum)%FanTrq > (Fan(FanNum)%BeltMaxTorque + 1.d-5)) THEN
      CALL ShowWarningError('Belt for '//TRIM(Fan(FanNum)%FanType)//': '//TRIM(Fan(FanNum)%FanName)// &
         ' is undersized at design conditions -- check belt inputs')
      CALL ShowContinueError('...Design belt output torque (without oversizing) [Nm]: '// &
         TRIM(RoundSigDigits(Fan(FanNum)%FanTrq,2)))
    END IF

   ! Calculate belt max efficiency using correlations and coefficients based on AMCA data
   ! Direct-drive is represented using curve coefficients such that "belt" max eff and PL eff = 1.0
    XbeltMax = LOG(Fan(FanNum)%FanShaftPwrMax / 746.d0)  !Natural log of belt output power in hp
    IF (Fan(FanNum)%BeltMaxEffCurveIndex /= 0) THEN
      Fan(FanNum)%BeltMaxEff = EXP(CurveValue(Fan(FanNum)%BeltMaxEffCurveIndex,XbeltMax)) ![-]
    ELSE
      Fan(FanNum)%BeltMaxEff = 1.d0 !No curve specified - use constant efficiency
    END IF

   ! Calculate belt part-load drive efficiency and input power using correlations and coefficients based on ACEEE data
    FanTrqRatio = Fan(FanNum)%FanTrq / Fan(FanNum)%BeltMaxTorque ![-]
    IF ((FanTrqRatio <= Fan(FanNum)%BeltTorqueTrans).AND.(Fan(FanNum)%PLBeltEffReg1CurveIndex /= 0)) THEN
      BeltPLEff = CurveValue(Fan(FanNum)%PLBeltEffReg1CurveIndex,FanTrqRatio) ![-]
    ELSE
      IF ((FanTrqRatio > Fan(FanNum)%BeltTorqueTrans).AND.(FanTrqRatio <= 1.d0) &
          .AND.(Fan(FanNum)%PLBeltEffReg2CurveIndex /= 0)) THEN
        BeltPLEff = CurveValue(Fan(FanNum)%PLBeltEffReg2CurveIndex,FanTrqRatio) ![-]
      ELSE
        IF ((FanTrqRatio > 1.d0).AND.(Fan(FanNum)%PLBeltEffReg3CurveIndex /= 0)) THEN
          BeltPLEff = CurveValue(Fan(FanNum)%PLBeltEffReg3CurveIndex,FanTrqRatio) ![-]
        ELSE
           BeltPLEff = 1.d0 !Direct drive or no curve specified - use constant efficiency
        END IF
      END IF
    END IF
    Fan(FanNum)%BeltEff = Fan(FanNum)%BeltMaxEff * BeltPLEff ![-]
    Fan(FanNum)%BeltEff = MAX(Fan(FanNum)%BeltEff,0.01d0)!Minimum efficiency is 1% to avoid numerical errors
    Fan(FanNum)%BeltInputPower = Fan(FanNum)%FanShaftPower / Fan(FanNum)%BeltEff ![W]

    IF (Fan(FanNum)%MotorMaxOutPwr == AutoSize) THEN
      !WRITE(*,*) 'Autosizing fan motor'
      Fan(FanNum)%MotorMaxOutPwr = Fan(FanNum)%BeltInputPower
    END IF
    ! Adjust max motor output power using motor sizing factor
    Fan(FanNum)%MotorMaxOutPwr = Fan(FanNum)%MotorMaxOutPwr * Fan(FanNum)%MotorSizingFactor ![W]

    ! Check for undersized motor and report design size with warning cpw14Sep2010
    IF (Fan(FanNum)%BeltInputPower > (Fan(FanNum)%MotorMaxOutPwr + 1.d-5)) THEN
      CALL ShowWarningError('Motor for '//TRIM(Fan(FanNum)%FanType)//': '//TRIM(Fan(FanNum)%FanName)// &
         ' is undersized at design conditions -- check motor inputs')
      CALL ShowContinueError('...Design motor output power (without oversizing) [W]: '// &
         TRIM(RoundSigDigits(Fan(FanNum)%BeltInputPower,2)))
    END IF

   ! Calculate motor max efficiency using correlations and coefficients based on MotorMaster+ data
    XmotorMax = LOG(Fan(FanNum)%MotorMaxOutPwr / 746.d0)  !Natural log of motor output power in hp
    IF (Fan(FanNum)%MotorMaxEffCurveIndex /= 0) THEN
      Fan(FanNum)%MotorMaxEff = CurveValue(Fan(FanNum)%MotorMaxEffCurveIndex,XmotorMax) ![-]
    ELSE
      Fan(FanNum)%MotorMaxEff = 1.d0 !No curve specified - use constant efficiency
    END IF

   ! Calculate motor part-load efficiency and input power using correlations and coefficients based on MotorMaster+ data
    MotorOutPwrRatio = Fan(FanNum)%BeltInputPower / Fan(FanNum)%MotorMaxOutPwr ![-]
    IF (Fan(FanNum)%PLMotorEffCurveIndex /= 0) THEN
      MotorPLEff = CurveValue(Fan(FanNum)%PLMotorEffCurveIndex,MotorOutPwrRatio) ![-]
    ELSE
      MotorPLEff = 1.d0 !No curve specified - use constant efficiency
    END IF
    Fan(FanNum)%MotEff = Fan(FanNum)%MotorMaxEff * MotorPLEff ![-]
    Fan(FanNum)%MotEff = MAX(Fan(FanNum)%MotEff,0.01d0)!Minimum efficiency is 1% to avoid numerical errors

   ! Calculate motor input power using belt input power and motor efficiency
    Fan(FanNum)%MotorInputPower = Fan(FanNum)%BeltInputPower / Fan(FanNum)%MotEff ![W]

    ! Calculate max VFD efficiency and input power using correlations and coefficients based on VFD type
    IF ((TRIM(Fan(FanNum)%VFDEffType) == 'SPEED').AND.(Fan(FanNum)%VFDEffCurveIndex /= 0)) THEN
      VFDSpdRatio = MotorSpeed / Fan(FanNum)%MotorMaxSpd ![-]
      Fan(FanNum)%VFDEff = CurveValue(Fan(FanNum)%VFDEffCurveIndex,VFDSpdRatio) ![-]
    ELSE
      IF ((TRIM(Fan(FanNum)%VFDEffType) == 'POWER').AND.(Fan(FanNum)%VFDEffCurveIndex /= 0)) THEN
        IF (Fan(FanNum)%VFDMaxOutPwr == AutoSize) THEN
          !WRITE(*,*) 'Autosizing fan VFD'
          Fan(FanNum)%VFDMaxOutPwr = Fan(FanNum)%MotorInputPower
        END IF
        ! Adjust max VFD output power using VFD sizing factor
        Fan(FanNum)%VFDMaxOutPwr = Fan(FanNum)%VFDMaxOutPwr * Fan(FanNum)%VFDSizingFactor ![W]

        ! Check for undersized VFD and report design size with warning cpw14Sep2010
        IF (Fan(FanNum)%MotorInputPower > (Fan(FanNum)%VFDMaxOutPwr + 1.d-5)) THEN
          CALL ShowWarningError('VFD for '//TRIM(Fan(FanNum)%FanType)//': '//TRIM(Fan(FanNum)%FanName)// &
            ' is undersized at design conditions -- check VFD inputs')
          CALL ShowContinueError('...Design VFD output power (without oversizing) [W]: '// &
            TRIM(RoundSigDigits(Fan(FanNum)%MotorInputPower,2)))
        END IF

        VFDOutPwrRatio = Fan(FanNum)%MotorInputPower / Fan(FanNum)%VFDMaxOutPwr ![-]
        Fan(FanNum)%VFDEff = CurveValue(Fan(FanNum)%VFDEffCurveIndex,VFDOutPwrRatio) ![-]
      ELSE
        ! No curve specified - use constant efficiency
        Fan(FanNum)%VFDMaxOutPwr = 0.d0
        Fan(FanNum)%VFDEff = 0.97d0
      END IF
    ENDIF
    Fan(FanNum)%VFDEff = MAX(Fan(FanNum)%VFDEff,0.01d0)!Minimum efficiency is 1% to avoid numerical errors

   ! Calculate VFD "rated" input power using motor input power and VFD efficiency
    RatedPower = Fan(FanNum)%MotorInputPower / Fan(FanNum)%VFDEff ![W]

    ! Calculate combined fan system efficiency: includes fan, belt, motor, and VFD
    ! Equivalent to Fan(FanNum)%FanAirPower / Fan(FanNum)%FanPower
    Fan(FanNum)%FanEff = Fan(FanNum)%FanWheelEff * Fan(FanNum)%BeltEff * Fan(FanNum)%MotEff * Fan(FanNum)%VFDEff

    ! Report fan, belt, motor, and VFD characteristics at design condition to .eio file cpw14Sep2010
     CALL ReportSizingOutput(TRIM(Fan(FanNum)%FanType), &
            Fan(FanNum)%FanName, 'Design Fan Airflow [m3/s]', FanVolFlow)
     CALL ReportSizingOutput(TRIM(Fan(FanNum)%FanType), &
            Fan(FanNum)%FanName, 'Design Fan Static Pressure Rise [Pa]', Fan(FanNum)%DeltaPress)
     CALL ReportSizingOutput(TRIM(Fan(FanNum)%FanType), &
            Fan(FanNum)%FanName, 'Design Fan Shaft Power [W]', Fan(FanNum)%FanShaftPower)
     CALL ReportSizingOutput(TRIM(Fan(FanNum)%FanType), &
            Fan(FanNum)%FanName, 'Design Motor Output Power [W]', Fan(FanNum)%MotorMaxOutPwr)
     CALL ReportSizingOutput(TRIM(Fan(FanNum)%FanType), &
            Fan(FanNum)%FanName, 'Design VFD Output Power [W]', Fan(FanNum)%VFDMaxOutPwr)
     CALL ReportSizingOutput(TRIM(Fan(FanNum)%FanType), &
            Fan(FanNum)%FanName, 'Rated Power [W]', RatedPower)
     CALL ReportSizingOutput(TRIM(Fan(FanNum)%FanType), &
            Fan(FanNum)%FanName, 'Drive Ratio [-]', Fan(FanNum)%PulleyDiaRatio)
     CALL ReportSizingOutput(TRIM(Fan(FanNum)%FanType), &
            Fan(FanNum)%FanName, 'Design Belt Output Torque [Nm]', Fan(FanNum)%BeltMaxTorque)
     CALL ReportSizingOutput(TRIM(Fan(FanNum)%FanType), &
            Fan(FanNum)%FanName, 'Design Fan Efficiency  [-]', Fan(FanNum)%FanWheelEff)
     CALL ReportSizingOutput(TRIM(Fan(FanNum)%FanType), &
            Fan(FanNum)%FanName, 'Maximum Belt Efficiency [-]', Fan(FanNum)%BeltMaxEff)
     CALL ReportSizingOutput(TRIM(Fan(FanNum)%FanType), &
            Fan(FanNum)%FanName, 'Design Belt Efficiency [-]', Fan(FanNum)%BeltEff)
     CALL ReportSizingOutput(TRIM(Fan(FanNum)%FanType), &
            Fan(FanNum)%FanName, 'Maximum Motor Efficiency [-]', Fan(FanNum)%MotorMaxEff)
     CALL ReportSizingOutput(TRIM(Fan(FanNum)%FanType), &
            Fan(FanNum)%FanName, 'Design Motor Efficiency [-]', Fan(FanNum)%MotEff)
     CALL ReportSizingOutput(TRIM(Fan(FanNum)%FanType), &
            Fan(FanNum)%FanName, 'Design VFD Efficiency [-]', Fan(FanNum)%VFDEff)
     CALL ReportSizingOutput(TRIM(Fan(FanNum)%FanType), &
            Fan(FanNum)%FanName, 'Design Combined Efficiency [-]', Fan(FanNum)%FanEff)

    !cpw31Aug2010 Temporary code for debugging fan component model
!    WRITE(300,*) TRIM(RoundSigDigits(RhoAir,4))//','//TRIM(RoundSigDigits(FanVolFlow,4)) &
!    //','//TRIM(RoundSigDigits(FanOutletVelPress,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%DeltaPress,4)) &
!    //','//TRIM(RoundSigDigits(Fan(FanNum)%FanAirPower,4))//','//TRIM(RoundSigDigits(EulerNum,4)) &
!    //','//TRIM(RoundSigDigits(NormalizedEulerNum,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%FanWheelEff,4))
!    WRITE(301,*) TRIM(RoundSigDigits(Fan(FanNum)%FanShaftPower,4))//','//TRIM(RoundSigDigits(FanDimFlow,4)) &
!    //','//TRIM(RoundSigDigits(Fan(FanNum)%FanTrq,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%FanSpd,4)) &
!    //','//TRIM(RoundSigDigits(Fan(FanNum)%FanShaftPwrMax,4))//','//TRIM(RoundSigDigits(XbeltMax,4)) &
!    //','//TRIM(RoundSigDigits(Fan(FanNum)%BeltMaxEff,4))//','//TRIM(RoundSigDigits(FanTrqRatio,4))
!    WRITE(302,*) TRIM(RoundSigDigits(BeltPLEff,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%BeltEff,4)) &
!    //','//TRIM(RoundSigDigits(Fan(FanNum)%BeltInputPower,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%MotorMaxOutPwr,4)) &
!    //','//TRIM(RoundSigDigits(XmotorMax,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%MotorMaxEff,4)) &
!    //','//TRIM(RoundSigDigits(MotorOutPwrRatio,4))//','//TRIM(RoundSigDigits(MotorPLEff,4))
!    WRITE(303,*) TRIM(RoundSigDigits(Fan(FanNum)%MotEff,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%MotorInputPower,4)) &
!    //','//TRIM(RoundSigDigits(VFDOutPwrRatio,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%VFDEff,4)) &
!    //','//TRIM(RoundSigDigits(RatedPower,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%FanEff,4)) &
!    //','//TRIM(RoundSigDigits(0.d0,4))//','//TRIM(RoundSigDigits(0.d0,4))
!    WRITE(304,*) TRIM("Fan")//','//TRIM("Sizing")

    !cpw31Aug2010 Temporary code to write headers for component fan model debug files
!    WRITE(300,*) 'Rho,VolFlow,dPvOut,dP,AirPwr,Eu,NrmEu,FWEff'
!    WRITE(301,*) 'ShftPwr,DimFlow,Trq,FSpd,BPwrOut,XBmax,BMaxEff,TrqRat'
!    WRITE(302,*) 'BPLEff,BEff,BPwrIn,MPwrOut,XMmax,MMaxEff,MPwrRat,MPLEff'
!    WRITE(303,*) 'MEff,MPwrIn,VPwrRat,VEff,FanPwr,FanEff,PwrLoss,dEnthalpy'
!    WRITE(304,*) 'Date,Period'
  END IF  !End fan component sizing

  equipName = Fan(FanNum)%FanName

  !cpw31Aug2010 Rearrange order to match table and use FanVolFlow to calculate RatedPower
  !ALSO generates values if Component Model fan, for which DeltaPress and FanEff vary with flow
  CALL PreDefTableEntry(pdchFanType,equipName,Fan(FanNum)%FanType)
  CALL PreDefTableEntry(pdchFanTotEff,equipName,Fan(FanNum)%FanEff)
  CALL PreDefTableEntry(pdchFanDeltaP,equipName,Fan(FanNum)%DeltaPress)
  CALL PreDefTableEntry(pdchFanVolFlow,equipName,FanVolFlow)
  RatedPower =  FanVolFlow * Fan(FanNum)%DeltaPress / Fan(FanNum)%FanEff ! total fan power
  CALL PreDefTableEntry(pdchFanPwr,equipName,RatedPower)
  IF (FanVolFlow .NE. 0.0d0) THEN
    CALL PreDefTableEntry(pdchFanPwrPerFlow,equipName,RatedPower/FanVolFlow)
  END IF
  CALL PreDefTableEntry(pdchFanMotorIn,equipName,Fan(FanNum)%MotInAirFrac)
  CALL PreDefTableEntry(pdchFanEndUse,equipName,Fan(FanNum)%EndUseSubcategoryName)

  IF (NVPerfNum > 0) THEN
    IF (NightVentPerf(NVPerfNum)%MaxAirFlowRate == AutoSize) THEN
      NightVentPerf(NVPerfNum)%MaxAirFlowRate = Fan(FanNum)%MaxAirFlowRate
    END IF
  END IF

  RETURN

END SUBROUTINE SizeFan

! End Initialization Section of the Module
!******************************************************************************


! Begin Algorithm Section of the Module
!******************************************************************************
SUBROUTINE SimSimpleFan(FanNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Unknown
          !       DATE WRITTEN   Unknown
          !       MODIFIED       Brent Griffith, May 2009, added EMS override
          !                      Chandan Sharma, March 2011, FSEC: Added LocalTurnFansOn and LocalTurnFansOff
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the simple constant volume fan.

          ! METHODOLOGY EMPLOYED:
          ! Converts design pressure rise and efficiency into fan power and temperature rise
          ! Constant fan pressure rise is assumed.

          ! REFERENCES:
          ! ASHRAE HVAC 2 Toolkit, page 2-3 (FANSIM)

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   Integer, Intent(IN) :: FanNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      REAL(r64) RhoAir
      REAL(r64) DeltaPress  ! [N/m2]
      REAL(r64) FanEff
      REAL(r64) MotInAirFrac
      REAL(r64) MotEff
      REAL(r64) MassFlow    ! [kg/sec]
!unused0909      REAL(r64) Tin         ! [C]
!unused0909      REAL(r64) Win
      REAL(r64) FanShaftPower ! power delivered to fan shaft
      REAL(r64) PowerLossToAir ! fan and motor loss to air stream (watts)
      Integer NVPerfNum

   NVPerfNum  = Fan(FanNum)%NVPerfNum

   IF (NightVentOn .AND. NVPerfNum > 0) THEN
     DeltaPress = NightVentPerf(NVPerfNum)%DeltaPress
     FanEff = NightVentPerf(NVPerfNum)%FanEff
     MotEff = NightVentPerf(NVPerfNum)%MotEff
     MotInAirFrac = NightVentPerf(NVPerfNum)%MotInAirFrac
   ELSE
     DeltaPress = Fan(FanNum)%DeltaPress
     FanEff     = Fan(FanNum)%FanEff
     MotEff     = Fan(FanNum)%MotEff
     MotInAirFrac = Fan(FanNum)%MotInAirFrac
   END IF

   IF (Fan(FanNum)%EMSFanPressureOverrideOn) DeltaPress = Fan(FanNum)%EMSFanPressureValue
   IF (Fan(FanNum)%EMSFanEffOverrideOn) FanEff = Fan(FanNum)%EMSFanEffValue

   ! For a Constant Volume Simple Fan the Max Flow Rate is the Flow Rate for the fan
!unused0909   Tin        = Fan(FanNum)%InletAirTemp
!unused0909   Win        = Fan(FanNum)%InletAirHumRat
   RhoAir     = Fan(FanNum)%RhoAirStdInit
   MassFlow   = Fan(FanNum)%InletAirMassFlowRate
   IF (Fan(FanNum)%EMSMaxMassFlowOverrideOn) MassFlow = Fan(FanNum)%EMSAirMassFlowValue
   MassFlow   = MIN(MassFlow,Fan(FanNum)%MaxAirMassFlowRate)
   MassFlow   = MAX(MassFlow,Fan(FanNum)%MinAirMassFlowRate)
   !
   !Determine the Fan Schedule for the Time step
  If( ( GetCurrentScheduleValue(Fan(FanNum)%AvailSchedPtrNum)>0.0d0 .or. LocalTurnFansOn) &
        .and. .NOT.LocalTurnFansOff  .and. Massflow>0.0d0) Then
   !Fan is operating
   Fan(FanNum)%FanPower = MassFlow*DeltaPress/(FanEff*RhoAir) ! total fan power
   FanShaftPower = MotEff * Fan(FanNum)%FanPower  ! power delivered to shaft
   PowerLossToAir = FanShaftPower + (Fan(FanNum)%FanPower - FanShaftPower) * MotInAirFrac
   Fan(FanNum)%OutletAirEnthalpy = Fan(FanNum)%InletAirEnthalpy + PowerLossToAir/MassFlow
   ! This fan does not change the moisture or Mass Flow across the component
   Fan(FanNum)%OutletAirHumRat       = Fan(FanNum)%InletAirHumRat
   Fan(FanNum)%OutletAirMassFlowRate = MassFlow
   Fan(FanNum)%OutletAirTemp = PsyTdbFnHW(Fan(FanNum)%OutletAirEnthalpy,Fan(FanNum)%OutletAirHumRat)

 Else
   !Fan is off and not operating no power consumed and mass flow rate.
   Fan(FanNum)%FanPower = 0.0d0
   FanShaftPower = 0.0d0
   PowerLossToAir = 0.0d0
   Fan(FanNum)%OutletAirMassFlowRate = 0.0d0
   Fan(FanNum)%OutletAirHumRat       = Fan(FanNum)%InletAirHumRat
   Fan(FanNum)%OutletAirEnthalpy     = Fan(FanNum)%InletAirEnthalpy
   Fan(FanNum)%OutletAirTemp = Fan(FanNum)%InletAirTemp
   ! Set the Control Flow variables to 0.0 flow when OFF.
   Fan(FanNum)%MassFlowRateMaxAvail = 0.0d0
   Fan(FanNum)%MassFlowRateMinAvail = 0.0d0

 End If

 RETURN
END SUBROUTINE SimSimpleFan


SUBROUTINE SimVariableVolumeFan(FanNum, PressureRise)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Unknown
          !       DATE WRITTEN   Unknown
          !       MODIFIED       Phil Haves
          !                      Brent Griffith, May 2009 for EMS
          !                      Chandan Sharma, March 2011, FSEC: Added LocalTurnFansOn and LocalTurnFansOff
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the simple variable volume fan.

          ! METHODOLOGY EMPLOYED:
          ! Converts design pressure rise and efficiency into fan power and temperature rise
          ! Constant fan pressure rise is assumed.
          ! Uses curves of fan power fraction vs. fan part load to determine fan power at
          ! off design conditions.

          ! REFERENCES:
          ! ASHRAE HVAC 2 Toolkit, page 2-3 (FANSIM)

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   Integer, Intent(IN) :: FanNum
   REAL(r64), INTENT(IN), OPTIONAL :: PressureRise

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      REAL(r64) RhoAir
      REAL(r64) DeltaPress  ! [N/m2 = Pa]
      REAL(r64) FanEff      ! Total fan efficiency - combined efficiency of fan, drive train,
                       ! motor and variable speed controller (if any)
      REAL(r64) MaxAirMassFlowRate
      REAL(r64) MotInAirFrac
      REAL(r64) MotEff
      REAL(r64) MassFlow    ! [kg/sec]
!unused0909      REAL(r64) Tin         ! [C]
!unused0909      REAL(r64) Win
      REAL(r64) PartLoadFrac
!unused0909      REAL(r64) MaxFlowFrac   !Variable Volume Fan Max Flow Fraction [-]
      REAL(r64) MinFlowFrac   !Variable Volume Fan Min Flow Fraction [-]
      REAL(r64) :: FlowFracForPower = 0.d0   !Variable Volume Fan Flow Fraction for power calcs[-]
      REAL(r64) :: FlowFracActual  = 0.d0  ! actual VAV fan flow fraction
      REAL(r64) FanShaftPower ! power delivered to fan shaft
      REAL(r64) PowerLossToAir ! fan and motor loss to air stream (watts)
      Integer NVPerfNum

      ! added to address the fan heat issue during low air flow conditions
      REAL(r64) MinFlowFracLimitFanHeat   ! Minimum Fan Flow Fraction Limit for Fan Heat at Low Airflow [-]
      REAL(r64) FanPoweratLowMinimum      ! Fan Power at Low Minimum Airflow [W]
      REAL(r64) PartLoadFracatLowMin
      REAL(r64) DeltaTAcrossFan           ! Air temperature rise across the fan due to fan heat [C]

! Simple Variable Volume Fan - default values from DOE-2
! Type of Fan          Coeff1       Coeff2       Coeff3        Coeff4      Coeff5
! INLET VANE DAMPERS   0.35071223   0.30850535   -0.54137364   0.87198823  0.000
! DISCHARGE DAMPERS    0.37073425   0.97250253   -0.34240761   0.000       0.000
! VARIABLE SPEED MOTOR 0.0015302446 0.0052080574  1.1086242   -0.11635563  0.000

  NVPerfNum  = Fan(FanNum)%NVPerfNum

  IF (NightVentOn .AND. NVPerfNum > 0) THEN
    DeltaPress = NightVentPerf(NVPerfNum)%DeltaPress
    FanEff = NightVentPerf(NVPerfNum)%FanEff
    MotEff = NightVentPerf(NVPerfNum)%MotEff
    MotInAirFrac = NightVentPerf(NVPerfNum)%MotInAirFrac
    MaxAirMassFlowRate = NightVentPerf(NVPerfNum)%MaxAirMassFlowRate
  ELSE
    IF (PRESENT(PressureRise)) THEN
      DeltaPress = PressureRise
    ELSE
      DeltaPress = Fan(FanNum)%DeltaPress
    ENDIF
    FanEff     = Fan(FanNum)%FanEff
    MotEff     = Fan(FanNum)%MotEff
    MotInAirFrac = Fan(FanNum)%MotInAirFrac
    MaxAirMassFlowRate = Fan(FanNum)%MaxAirMassFlowRate
  END IF

  IF (Fan(FanNum)%EMSFanPressureOverrideOn) DeltaPress = Fan(FanNum)%EMSFanPressureValue
  IF (Fan(FanNum)%EMSFanEffOverrideOn) FanEff = Fan(FanNum)%EMSFanEffValue

!unused0909  Tin         = Fan(FanNum)%InletAirTemp
!unused0909  Win         = Fan(FanNum)%InletAirHumRat
  RhoAir      = Fan(FanNum)%RhoAirStdInit
  MassFlow    = Fan(FanNum)%InletAirMassFlowRate
  IF (Fan(FanNum)%EMSMaxMassFlowOverrideOn) MassFlow = Fan(FanNum)%EMSAirMassFlowValue
  MassFlow    = MIN(MassFlow,Fan(FanNum)%MaxAirMassFlowRate)

   !Determine the Fan Schedule for the Time step
  If( ( GetCurrentScheduleValue(Fan(FanNum)%AvailSchedPtrNum)>0.0d0 .or. LocalTurnFansOn) &
        .and. .NOT. LocalTurnFansOff .and. Massflow>0.0d0) Then
    !Fan is operating - calculate power loss and enthalpy rise
    !  Fan(FanNum)%FanPower = PartLoadFrac*FullMassFlow*DeltaPress/(FanEff*RhoAir) ! total fan power
   ! Calculate and check limits on fraction of system flow
!unused0909    MaxFlowFrac = 1.0
    ! MinFlowFrac is calculated from the ration of the volume flows and is non-dimensional
    MinFlowFrac = Fan(FanNum)%MinAirFlowRate/Fan(FanNum)%MaxAirFlowRate
    ! The actual flow fraction is calculated from MassFlow and the MaxVolumeFlow * AirDensity
    FlowFracActual = MassFlow/(Fan(FanNum)%MaxAirMassFlowRate)

    ! Calculate the part Load Fraction             (PH 7/13/03)

    FlowFracForPower = MAX(MinFlowFrac,MIN(FlowFracActual,1.0d0))  ! limit flow fraction to allowed range
    IF (NightVentOn .AND. NVPerfNum > 0) THEN
      PartLoadFrac = 1.0d0
    ELSE
      PartLoadFrac=Fan(FanNum)%FanCoeff(1) + Fan(FanNum)%FanCoeff(2)*FlowFracForPower +  &
                       Fan(FanNum)%FanCoeff(3)*FlowFracForPower**2 + Fan(FanNum)%FanCoeff(4)*FlowFracForPower**3 + &
                       Fan(FanNum)%FanCoeff(5)*FlowFracForPower**4
    END IF

    Fan(FanNum)%FanPower = PartLoadFrac*MaxAirMassFlowRate*DeltaPress/(FanEff*RhoAir) ! total fan power (PH 7/13/03)

    FanShaftPower = MotEff * Fan(FanNum)%FanPower  ! power delivered to shaft
    PowerLossToAir = FanShaftPower + (Fan(FanNum)%FanPower - FanShaftPower) * MotInAirFrac
    Fan(FanNum)%OutletAirEnthalpy = Fan(FanNum)%InletAirEnthalpy + PowerLossToAir/MassFlow
    ! This fan does not change the moisture or Mass Flow across the component
    Fan(FanNum)%OutletAirHumRat       = Fan(FanNum)%InletAirHumRat
    Fan(FanNum)%OutletAirMassFlowRate = MassFlow
    Fan(FanNum)%OutletAirTemp = PsyTdbFnHW(Fan(FanNum)%OutletAirEnthalpy,Fan(FanNum)%OutletAirHumRat)

    ! KHL/FB, 2/10/2011. NFP implemented as CR 8338.
    ! When fan air flow is less than 10%, the fan power curve is linearized between the 10% to 0% to
    !  avoid the unrealistic high temperature rise across the fan.
    ! TH, 2/15/2011
    ! This change caused diffs for VAV systems when fan runs at less than 10% flow conditions.
    !  A potential way to improve is to check the temperature rise across the fan first,
    !  if it is too high (say > 20C) then applies the code.
    DeltaTAcrossFan = Fan(FanNum)%OutletAirTemp - Fan(FanNum)%InletAirTemp
    IF (DeltaTAcrossFan > 20.0d0) THEN
        MinFlowFracLimitFanHeat = 0.10d0
        IF (FlowFracForPower < MinFlowFracLimitFanHeat) THEN
          PartLoadFracatLowMin=Fan(FanNum)%FanCoeff(1) + Fan(FanNum)%FanCoeff(2)*MinFlowFracLimitFanHeat +  &
                    Fan(FanNum)%FanCoeff(3)*MinFlowFracLimitFanHeat**2 + Fan(FanNum)%FanCoeff(4)*MinFlowFracLimitFanHeat**3 + &
                    Fan(FanNum)%FanCoeff(5)*MinFlowFracLimitFanHeat**4
          FanPoweratLowMinimum = PartLoadFracatLowMin*MaxAirMassFlowRate*DeltaPress/(FanEff*RhoAir)
          Fan(FanNum)%FanPower = FlowFracForPower * FanPoweratLowMinimum / MinFlowFracLimitFanHeat
        ELSEIF (FlowFracActual < MinFlowFracLimitFanHeat) THEN
          PartLoadFracatLowMin=Fan(FanNum)%FanCoeff(1) + Fan(FanNum)%FanCoeff(2)*MinFlowFracLimitFanHeat +  &
                    Fan(FanNum)%FanCoeff(3)*MinFlowFracLimitFanHeat**2 + Fan(FanNum)%FanCoeff(4)*MinFlowFracLimitFanHeat**3 + &
                    Fan(FanNum)%FanCoeff(5)*MinFlowFracLimitFanHeat**4
          FanPoweratLowMinimum = PartLoadFracatLowMin*MaxAirMassFlowRate*DeltaPress/(FanEff*RhoAir)
          Fan(FanNum)%FanPower = FlowFracActual * FanPoweratLowMinimum / MinFlowFracLimitFanHeat
        END IF
        FanShaftPower = MotEff * Fan(FanNum)%FanPower  ! power delivered to shaft
        PowerLossToAir = FanShaftPower + (Fan(FanNum)%FanPower - FanShaftPower) * MotInAirFrac
        Fan(FanNum)%OutletAirEnthalpy = Fan(FanNum)%InletAirEnthalpy + PowerLossToAir/MassFlow
        ! This fan does not change the moisture or Mass Flow across the component
        Fan(FanNum)%OutletAirHumRat       = Fan(FanNum)%InletAirHumRat
        Fan(FanNum)%OutletAirMassFlowRate = MassFlow
        Fan(FanNum)%OutletAirTemp = PsyTdbFnHW(Fan(FanNum)%OutletAirEnthalpy,Fan(FanNum)%OutletAirHumRat)
    ENDIF

  Else
    !Fan is off and not operating no power consumed and mass flow rate.
    Fan(FanNum)%FanPower = 0.0d0
    FanShaftPower = 0.0d0
    PowerLossToAir = 0.0d0
    Fan(FanNum)%OutletAirMassFlowRate = 0.0d0
    Fan(FanNum)%OutletAirHumRat       = Fan(FanNum)%InletAirHumRat
    Fan(FanNum)%OutletAirEnthalpy     = Fan(FanNum)%InletAirEnthalpy
    Fan(FanNum)%OutletAirTemp = Fan(FanNum)%InletAirTemp
    ! Set the Control Flow variables to 0.0 flow when OFF.
    Fan(FanNum)%MassFlowRateMaxAvail = 0.0d0
    Fan(FanNum)%MassFlowRateMinAvail = 0.0d0
  End If

  RETURN
END SUBROUTINE SimVariableVolumeFan

SUBROUTINE SimOnOffFan(FanNum, SpeedRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Unknown
          !       DATE WRITTEN   Unknown
          !       MODIFIED       Shirey, May 2001
          !                      R. Raustad - FSEC, Jan 2009 - added SpeedRatio for multi-speed fans
          !                      Brent Griffith, May 2009 for EMS
          !                      Chandan Sharma, March 2011, FSEC: Added LocalTurnFansOn and LocalTurnFansOff
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the simple on/off fan.

          ! METHODOLOGY EMPLOYED:
          ! Converts design pressure rise and efficiency into fan power and temperature rise
          ! Constant fan pressure rise is assumed.
          ! Uses curves of fan power fraction vs. fan part load to determine fan power at
          ! off design conditions.
          ! Same as simple (constant volume) fan, except added part-load curve input

          ! REFERENCES:
          ! ASHRAE HVAC 2 Toolkit, page 2-3 (FANSIM)

          ! USE STATEMENTS:
  USE CurveManager, ONLY: CurveValue
  USE General,      ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   Integer, Intent(IN)        :: FanNum
   REAL(r64), OPTIONAL, INTENT(IN) :: SpeedRatio

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      REAL(r64) RhoAir
      REAL(r64) DeltaPress  ! [N/m2]
      REAL(r64) FanEff
      REAL(r64) MassFlow    ! [kg/sec]
!unused0909      REAL(r64) Tin         ! [C]
!unused0909      REAL(r64) Win
      REAL(r64) PartLoadRatio !Ratio of actual mass flow rate to max mass flow rate
      REAL(r64) FlowFrac      !Actual Fan Flow Fraction = actual mass flow rate / max air mass flow rate
      REAL(r64) FanShaftPower ! power delivered to fan shaft
      REAL(r64) PowerLossToAir ! fan and motor loss to air stream (watts)
      REAL(r64) SpeedRaisedToPower ! Result of the speed ratio raised to the power of n (Curve object)
      REAL(r64) EffRatioAtSpeedRatio ! Efficeincy ratio at current speed ratio (Curve object)
      Integer, SAVE :: ErrCount=0

   DeltaPress = Fan(FanNum)%DeltaPress
   IF (Fan(FanNum)%EMSFanPressureOverrideOn) DeltaPress = Fan(FanNum)%EMSFanPressureValue
   FanEff     = Fan(FanNum)%FanEff
   IF (Fan(FanNum)%EMSFanEffOverrideOn) FanEff = Fan(FanNum)%EMSFanEffValue
!unused0909   Tin        = Fan(FanNum)%InletAirTemp
!unused0909   Win        = Fan(FanNum)%InletAirHumRat
   RhoAir     = Fan(FanNum)%RhoAirStdInit
   MassFlow   = Fan(FanNum)%InletAirMassFlowRate
   IF (Fan(FanNum)%EMSMaxMassFlowOverrideOn) MassFlow = Fan(FanNum)%EMSAirMassFlowValue
   MassFlow   = MIN(MassFlow,Fan(FanNum)%MaxAirMassFlowRate)
   MassFlow   = MAX(MassFlow,Fan(FanNum)%MinAirMassFlowRate)
   Fan(FanNum)%FanRuntimeFraction = 0.0d0

  ! Determine the Fan Schedule for the Time step
  IF( ( GetCurrentScheduleValue(Fan(FanNum)%AvailSchedPtrNum)>0.0d0 .or. LocalTurnFansOn) &
        .and. .NOT. LocalTurnFansOff .and. Massflow>0.0d0 .and. Fan(FanNum)%MaxAirMassFlowRate > 0.0d0) THEN
    ! The actual flow fraction is calculated from MassFlow and the MaxVolumeFlow * AirDensity
   FlowFrac = MassFlow/(Fan(FanNum)%MaxAirMassFlowRate)

    ! Calculate the part load ratio, can't be greater than 1
   PartLoadRatio= MIN(1.0d0,FlowFrac)
   ! Fan is operating
   IF (OnOffFanPartLoadFraction <= 0.0d0) THEN
     CALL ShowRecurringWarningErrorAtEnd('Fan:OnOff, OnOffFanPartLoadFraction <= 0.0, Reset to 1.0',ErrCount)
     OnOffFanPartLoadFraction = 1.0d0 ! avoid divide by zero or negative PLF
   END IF

   IF (OnOffFanPartLoadFraction < 0.7d0) THEN
        OnOffFanPartLoadFraction = 0.7d0 ! a warning message is already issued from the DX coils or gas heating coil
   END IF

   ! Keep fan runtime fraction between 0.0 and 1.0, and RTF >= PLR
   IF(OnOffFanPartLoadFraction .GE. 1.0d0)THEN
     Fan(FanNum)%FanRuntimeFraction = PartLoadRatio
   ELSE
     Fan(FanNum)%FanRuntimeFraction = MAX(0.0d0,MIN(1.0d0,PartLoadRatio/OnOffFanPartLoadFraction))
   END IF
   ! The fan speed ratio (passed from parent) determines the fan power according to fan laws
   IF(PRESENT(SpeedRatio))THEN
!    Fan(FanNum)%FanPower = MassFlow*DeltaPress/(FanEff*RhoAir*OnOffFanPartLoadFraction)! total fan power
     Fan(FanNum)%FanPower = Fan(FanNum)%MaxAirMassFlowRate*Fan(FanNum)%FanRuntimeFraction*DeltaPress/(FanEff*RhoAir)

!    Do not modify fan power calculation unless fan power vs speed ratio curve is used.
     IF(Fan(FanNum)%FanPowerRatAtSpeedRatCurveIndex .GT. 0)THEN

!      adjust RTF to be in line with speed ratio (i.e., MaxAirMassFlowRate is not MAX when SpeedRatio /= 1)
!      PLR = Mdot/MAXFlow => Mdot/(MAXFlow * SpeedRatio), RTF = PLR/PLF => PLR/SpeedRatio/PLF = RTF / SpeedRatio
       IF(SpeedRatio .GT. 0.0d0)Fan(FanNum)%FanRuntimeFraction = MIN(1.0d0,Fan(FanNum)%FanRuntimeFraction/SpeedRatio)

       SpeedRaisedToPower   = CurveValue(Fan(FanNum)%FanPowerRatAtSpeedRatCurveIndex,SpeedRatio)
       IF(SpeedRaisedToPower .LT. 0.0d0)THEN
         IF(Fan(FanNum)%OneTimePowerRatioCheck .AND. .NOT. WarmupFlag)THEN
           CALL ShowSevereError(TRIM(cFanTypes(Fan(FanNum)%FanType_Num))//' = '//TRIM(Fan(FanNum)%FanName)//'"')
           CALL ShowContinueError('Error in Fan Power Ratio curve. Curve output less than 0.0.')
           CALL ShowContinueError('Curve output = '//TRIM(TrimSigDigits(SpeedRaisedToPower,5))// &
                                  ', fan speed ratio = '//TRIM(TrimSigDigits(SpeedRatio,5)))
           CALL ShowContinueError('Check curve coefficients to ensure proper power ratio as a function of fan speed ratio.')
           CALL ShowContinueError('Resetting Fan Power Ratio curve output to 0.0 and the simulation continues.')
           CALL ShowContinueErrorTimeStamp(' Occurrence info: ')
           Fan(FanNum)%OneTimePowerRatioCheck = .FALSE.
         END IF
         SpeedRaisedToPower = 0.0d0
       END IF
       IF(Fan(FanNum)%FanEffRatioCurveIndex .GT. 0 .AND. .NOT. WarmupFlag)THEN
         EffRatioAtSpeedRatio = CurveValue(Fan(FanNum)%FanEffRatioCurveIndex,SpeedRatio)
         IF(EffRatioAtSpeedRatio .LT. 0.01d0)THEN
           IF(Fan(FanNum)%OneTimeEffRatioCheck .AND. .NOT. WarmupFlag)THEN
             CALL ShowSevereError(TRIM(cFanTypes(Fan(FanNum)%FanType_Num))//' = '//TRIM(Fan(FanNum)%FanName)//'"')
             CALL ShowContinueError('Error in Fan Efficiency Ratio curve. Curve output less than 0.01.')
             CALL ShowContinueError('Curve output = '//TRIM(TrimSigDigits(EffRatioAtSpeedRatio,5))// &
                                    ', fan speed ratio = '//TRIM(TrimSigDigits(SpeedRatio,5)))
             CALL ShowContinueError('Check curve coefficients to ensure proper efficiency ratio as a function of fan speed ratio.')
             CALL ShowContinueError('Resetting Fan Efficiency Ratio curve output to 0.01 and the simulation continues.')
             CALL ShowContinueErrorTimeStamp(' Occurrence info: ')
             Fan(FanNum)%OneTimeEffRatioCheck = .FALSE.
           END IF
           EffRatioAtSpeedRatio = 0.01d0
         END IF
       ELSE
         EffRatioAtSpeedRatio = 1.0d0
       END IF
       Fan(FanNum)%FanPower = Fan(FanNum)%FanPower * SpeedRaisedToPower/EffRatioAtSpeedRatio
     END IF
   ELSE
     Fan(FanNum)%FanPower = Fan(FanNum)%MaxAirMassFlowRate*Fan(FanNum)%FanRuntimeFraction*DeltaPress/(FanEff*RhoAir)!total fan power
   END IF

   ! OnOffFanPartLoadFraction is passed via DataHVACGlobals from the cooling or heating coil that is
   !   requesting the fan to operate in cycling fan/cycling coil mode
   OnOffFanPartLoadFraction = 1.0d0 ! reset to 1 in case other on/off fan is called without a part load curve
   FanShaftPower = Fan(FanNum)%MotEff * Fan(FanNum)%FanPower  ! power delivered to shaft
   PowerLossToAir = FanShaftPower + (Fan(FanNum)%FanPower - FanShaftPower) * Fan(FanNum)%MotInAirFrac
   Fan(FanNum)%OutletAirEnthalpy = Fan(FanNum)%InletAirEnthalpy + PowerLossToAir/MassFlow
   ! This fan does not change the moisture or Mass Flow across the component
   Fan(FanNum)%OutletAirHumRat       = Fan(FanNum)%InletAirHumRat
   Fan(FanNum)%OutletAirMassFlowRate = MassFlow
!   Fan(FanNum)%OutletAirTemp = Tin + PowerLossToAir/(MassFlow*PsyCpAirFnWTdb(Win,Tin))
   Fan(FanNum)%OutletAirTemp = PsyTdbFnHW(Fan(FanNum)%OutletAirEnthalpy,Fan(FanNum)%OutletAirHumRat)
  ELSE
   ! Fan is off and not operating no power consumed and mass flow rate.
   Fan(FanNum)%FanPower = 0.0d0
   FanShaftPower = 0.0d0
   PowerLossToAir = 0.0d0
   Fan(FanNum)%OutletAirMassFlowRate = 0.0d0
   Fan(FanNum)%OutletAirHumRat       = Fan(FanNum)%InletAirHumRat
   Fan(FanNum)%OutletAirEnthalpy     = Fan(FanNum)%InletAirEnthalpy
   Fan(FanNum)%OutletAirTemp = Fan(FanNum)%InletAirTemp
   ! Set the Control Flow variables to 0.0 flow when OFF.
   Fan(FanNum)%MassFlowRateMaxAvail = 0.0d0
   Fan(FanNum)%MassFlowRateMinAvail = 0.0d0
  END IF

  RETURN
END SUBROUTINE SimOnOffFan


SUBROUTINE SimZoneExhaustFan(FanNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   Jan 2000
          !       MODIFIED       Brent Griffith, May 2009 for EMS
          !                      Brent Griffith, Feb 2013 controls upgrade
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the Zone Exhaust Fan

          ! METHODOLOGY EMPLOYED:
          ! Converts design pressure rise and efficiency into fan power and temperature rise
          ! Constant fan pressure rise is assumed.

          ! REFERENCES:
          ! ASHRAE HVAC 2 Toolkit, page 2-3 (FANSIM)

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: FanNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: RhoAir
  REAL(r64) :: DeltaPress  ! [N/m2]
  REAL(r64) :: FanEff
  REAL(r64) :: MassFlow    ! [kg/sec]
  REAL(r64) :: Tin         ! [C]
  REAL(r64) :: PowerLossToAir ! fan and motor loss to air stream (watts)
  LOGICAL   :: FanIsRunning

  DeltaPress = Fan(FanNum)%DeltaPress
  IF (Fan(FanNum)%EMSFanPressureOverrideOn) DeltaPress = Fan(FanNum)%EMSFanPressureValue

  FanEff     = Fan(FanNum)%FanEff
  IF (Fan(FanNum)%EMSFanEffOverrideOn) FanEff = Fan(FanNum)%EMSFanEffValue

   ! For a Constant Volume Simple Fan the Max Flow Rate is the Flow Rate for the fan
  Tin        = Fan(FanNum)%InletAirTemp
  RhoAir     = Fan(FanNum)%RhoAirStdInit
  MassFlow   = Fan(FanNum)%InletAirMassFlowRate

!  When the AvailManagerMode == ExhaustFanCoupledToAvailManagers then the
!  Exhaust Fan is  interlocked with air loop availability via global TurnFansOn and TurnFansOff variables.
!  There is now the option to control if user wants to decouple air loop operation and exhaust fan operation
!  (zone air mass balance issues). If in the future want to allow for zone level local availability manager
!  then the optional arguments ZoneCompTurnFansOn and ZoneCompTurnFansOff will need
!  to be passed to SimulateFanComponents, and TurnFansOn must be changed to LocalTurnFansOn
!  and TurnFansOff to LocalTurnFansOff in the IF statement below.

! apply controls to determine if operating
  IF (Fan(FanNum)%AvailManagerMode == ExhaustFanCoupledToAvailManagers) THEN
    IF ( (( GetCurrentScheduleValue(Fan(FanNum)%AvailSchedPtrNum) > 0.0d0) .OR. TurnFansOn ) &
            .AND. .NOT. TurnFansOff .AND. MassFlow > 0.0d0 ) THEN ! available
      IF (Fan(FanNum)%MinTempLimitSchedNum > 0) THEN
        IF (Tin >= GetCurrentScheduleValue(Fan(FanNum)%MinTempLimitSchedNum)) THEN
          FanIsRunning = .TRUE.
        ELSE
          FanIsRunning = .FALSE.
        ENDIF
      ELSE
        FanIsRunning = .TRUE.
      ENDIF
    ELSE
      FanIsRunning = .FALSE.
    ENDIF

  ELSEIF (Fan(FanNum)%AvailManagerMode == ExhaustFanDecoupledFromAvailManagers) THEN
    IF ( GetCurrentScheduleValue(Fan(FanNum)%AvailSchedPtrNum) > 0.0d0 .AND. MassFlow > 0.0d0 ) THEN
      IF (Fan(FanNum)%MinTempLimitSchedNum > 0) THEN
        IF (Tin >= GetCurrentScheduleValue(Fan(FanNum)%MinTempLimitSchedNum)) THEN
          FanIsRunning = .TRUE.
        ELSE
          FanIsRunning = .FALSE.
        ENDIF
      ELSE
        FanIsRunning = .TRUE.
      ENDIF
    ELSE
      FanIsRunning = .FALSE.
    ENDIF
  ENDIF


  IF ( FanIsRunning ) THEN
    !Fan is operating
    Fan(FanNum)%FanPower = MassFlow*DeltaPress/(FanEff*RhoAir) ! total fan power
    PowerLossToAir = Fan(FanNum)%FanPower
    Fan(FanNum)%OutletAirEnthalpy = Fan(FanNum)%InletAirEnthalpy + PowerLossToAir/MassFlow
    ! This fan does not change the moisture or Mass Flow across the component
    Fan(FanNum)%OutletAirHumRat       = Fan(FanNum)%InletAirHumRat
    Fan(FanNum)%OutletAirMassFlowRate = MassFlow
    Fan(FanNum)%OutletAirTemp = PsyTdbFnHW(Fan(FanNum)%OutletAirEnthalpy,Fan(FanNum)%OutletAirHumRat)

  ELSE
    !Fan is off and not operating no power consumed and mass flow rate.
    Fan(FanNum)%FanPower = 0.0d0
    PowerLossToAir = 0.0d0
    Fan(FanNum)%OutletAirMassFlowRate = 0.0d0
    Fan(FanNum)%OutletAirHumRat       = Fan(FanNum)%InletAirHumRat
    Fan(FanNum)%OutletAirEnthalpy     = Fan(FanNum)%InletAirEnthalpy
    Fan(FanNum)%OutletAirTemp = Fan(FanNum)%InletAirTemp
   ! Set the Control Flow variables to 0.0 flow when OFF.
    Fan(FanNum)%MassFlowRateMaxAvail = 0.0d0
    Fan(FanNum)%MassFlowRateMinAvail = 0.0d0
    Fan(FanNum)%InletAirMassFlowRate = 0.0d0

   END IF

   RETURN
END SUBROUTINE SimZoneExhaustFan

!cpw22Aug2010 Added Component Model fan algorithm
SUBROUTINE SimComponentModelFan(FanNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Craig Wray, LBNL
          !       DATE WRITTEN   Feb 2010
          !       MODIFIED       Chandan Sharma, March 2011, FSEC: Added LocalTurnFansOn and LocalTurnFansOff
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the component model fan.

          ! METHODOLOGY EMPLOYED:
          ! Calculate fan volumetric flow and corresponding fan static pressure rise,
          !    using air-handling system characteristics and Sherman-Wray system curve model
          ! Calculate fan air power using volumetric flow and fan static pressure rise
          ! Calculate fan wheel efficiency using fan volumetric flow, fan static pressure rise,
          !   fan characteristics, and Wray dimensionless fan static efficiency model
          ! Calculate fan shaft power using fan air power and fan static efficiency
          ! Calculate fan shaft speed and torque using Wray dimensionless fan airflow model
          ! Calculate belt part-load efficiency using correlations and coefficients based on ACEEE data
          ! Calculate belt input power using fan shaft power and belt efficiency
          ! Calculate motor part-load efficiency using correlations and coefficients based on MotorMaster+ data
          ! Calculate motor input power using belt input power and motor efficiency
          ! Calculate VFD efficiency using correlations and coefficients based on DOE data
          ! Calculate VFD input power using motor input power and VFD efficiency
          ! Calculate combined efficiency of fan, belt, motor, and VFD
          ! Calculate air temperature rise due to fan (and belt+motor if in airstream) power entering air-handler airflow
          ! Calculate output node conditions

          ! REFERENCES:
          ! TBD

          ! USE STATEMENTS:
  USE CurveManager, ONLY: CurveValue
  USE CurveManager, ONLY: GetCurveIndex
  USE OutputReportPredefined
  USE General, ONLY: CreateSysTimeIntervalString,RoundSigDigits
  USE DataEnvironment, ONLY : EnvironmentName,CurMnDy

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: FanNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER NVPerfNum

  REAL(r64) MaxAirMassFlowRate ! Fan Max mass airflow [kg/s]
!unused062011  REAL(r64) MinFlowFrac        ! Fan Min Volumetric airflow Fraction [-]
!unused062011  REAL(r64) FlowFrac           ! Fan Volumetric airflow Fraction [-]

!unused062011  REAL(r64) DeltaPress         ! Delta Pressure Across the Fan (Fan Static Pressure Rise) [N/m2 = Pa]
!unused062011  REAL(r64) FanAirPower        ! Air power for Fan being Simulated [W]
!unused062011  REAL(r64) FanSpd             ! Fan shaft rotational speed [rpm]
!unused062011  REAL(r64) FanTrq             ! Fan shaft torque [N-m]
!unused062011  REAL(r64) FanWheelEff        ! Fan efficiency (mechanical) [-]
!unused062011  REAL(r64) FanShaftPower      ! Shaft input power for Fan being Simulated [W]
!unused062011  REAL(r64) BeltEff            ! Belt efficiency (mechanical) [-]
!unused062011  REAL(r64) BeltInputPower     ! Belt input power for Fan being Simulated [W]
!unused062011  REAL(r64) MotEff             ! Fan motor efficiency [-]
!unused062011  REAL(r64) MotorInputPower    ! Motor input power for Fan being Simulated [W]
!unused062011  REAL(r64) VFDEff             ! VFD efficiency (electrical) [-]
!unused062011  REAL(r64) VFDInputPower      ! VFD input power for Fan being Simulated [W]
!unused062011  REAL(r64) FanEff             ! Fan total system efficiency (fan*belt*motor*VFD) [-]
  REAL(r64) MotInAirFrac       ! Fraction of fan power input to airstream

  ! Local variables
  REAL(r64) RhoAir             ! Air density [kg/m3]
  REAL(r64) MassFlow           ! Fan mass airflow [kg/s]
  REAL(r64) FanVolFlow         ! Fan volumetric airflow [m3/s]
  REAL(r64) DuctStaticPress    ! Duct static pressure setpoint [Pa]
  REAL(r64) DeltaPressTot      ! Total pressure rise across fan [N/m2 = Pa]
  REAL(r64) FanOutletVelPress  ! Fan outlet velocity pressure [Pa]
  REAL(r64) EulerNum           ! Fan Euler number [-]
  REAL(r64) NormalizedEulerNum ! Normalized Fan Euler number [-]
  REAL(r64) FanDimFlow         ! Fan dimensionless airflow [-]
  REAL(r64) FanSpdRadS         ! Fan shaft rotational speed [rad/s]
  REAL(r64) MotorSpeed         ! Motor shaft rotational speed [rpm]
  REAL(r64) FanTrqRatio        ! Ratio of fan torque to max fan torque [-]
  REAL(r64) BeltPLEff          ! Belt normalized (part-load) efficiency [-]
  REAL(r64) MotorOutPwrRatio   ! Ratio of motor output power to max motor output power [-]
  REAL(r64) MotorPLEff         ! Motor normalized (part-load) efficiency [-]
  REAL(r64) :: VFDSpdRatio    = 0.d0 ! Ratio of motor speed to motor max speed [-]
  REAL(r64) :: VFDOutPwrRatio = 0.d0 ! Ratio of VFD output power to max VFD output power [-]
  REAL(r64) PowerLossToAir     ! Energy input to air stream (W)
  REAL(r64) FanEnthalpyChange  ! Air enthalpy change due to fan, belt, and motor losses [kJ/kg]

  ! Get inputs for night ventilation option
  NVPerfNum  = Fan(FanNum)%NVPerfNum

  IF (NightVentOn .AND. NVPerfNum > 0) THEN
    MotInAirFrac = NightVentPerf(NVPerfNum)%MotInAirFrac
    MaxAirMassFlowRate = NightVentPerf(NVPerfNum)%MaxAirMassFlowRate
  ELSE
    MotInAirFrac = Fan(FanNum)%MotInAirFrac
    MaxAirMassFlowRate = Fan(FanNum)%MaxAirMassFlowRate
  END IF

!  IF (Fan(FanNum)%EMSFanPressureOverrideOn) DeltaPress = Fan(FanNum)%EMSFanPressureValue
!  IF (Fan(FanNum)%EMSFanEffOverrideOn) FanEff = Fan(FanNum)%EMSFanEffValue

  ! Get air density at standard conditions and get mass airflow through fan
  ! From WeatherManager:
  !   StdBaroPress=(101.325d0*(1.d0-2.25577d-05*WeatherFileElevation)**5.2559d0)*1000.d0
  !   StdRhoAir=PsyRhoAirFnPbTdbW(StdBaroPress,20,0)
  ! From PsychRoutines:
  !   w=MAX(dw,1.0d-5)
  !   rhoair = pb/(287.d0*(tdb+KelvinConv)*(1.d0+1.6077687d0*w))
  RhoAir = Fan(FanNum)%RhoAirStdInit
  MassFlow = MIN(Fan(FanNum)%InletAirMassFlowRate,Fan(FanNum)%MaxAirMassFlowRate)

!  IF (Fan(FanNum)%EMSMaxMassFlowOverrideOn) MassFlow   = Fan(FanNum)%EMSAirMassFlowValue

  !Determine the Fan Schedule for the Time step
  If((GetCurrentScheduleValue(Fan(FanNum)%AvailSchedPtrNum)>0.0d0 .or. LocalTurnFansOn) &
        .and. .NOT.LocalTurnFansOff .and. Massflow>0.0d0) Then
    !Fan is operating - calculate fan pressure rise, component efficiencies and power, and also air enthalpy rise

   ! Calculate fan static pressure rise using fan volumetric flow, std air density, air-handling system characteristics,
   !   and Sherman-Wray system curve model (assumes static pressure surrounding air distribution system is zero)
    FanVolFlow = MassFlow / RhoAir ![m3/s at standard conditions]
    DuctStaticPress = CurveValue(Fan(FanNum)%PressResetCurveIndex,FanVolFlow) !Duct static pressure setpoint [Pa]
    DeltaPressTot = CurveValue(Fan(FanNum)%PressRiseCurveIndex,FanVolFlow,DuctStaticPress) !Fan total pressure rise [Pa]
    FanOutletVelPress = 0.5d0 * RhoAir * (FanVolFlow / Fan(FanNum)%FanOutletArea)**2 !Fan outlet velocity pressure [Pa]
    !Outlet velocity pressure cannot exceed total pressure rise
    FanOutletVelPress = MIN(FanOutletVelPress, DeltaPressTot)
    Fan(FanNum)%DeltaPress = DeltaPressTot - FanOutletVelPress !Fan static pressure rise [Pa]

!    IF (Fan(FanNum)%EMSFanPressureOverrideOn) DeltaPress = Fan(FanNum)%EMSFanPressureValue

   ! Calculate fan static air power using volumetric flow and fan static pressure rise
    Fan(FanNum)%FanAirPower = FanVolFlow * Fan(FanNum)%DeltaPress ![W]

   ! Calculate fan wheel efficiency using fan volumetric flow, fan static pressure rise,
   !   fan characteristics, and Wray dimensionless fan static efficiency model
    EulerNum = (Fan(FanNum)%DeltaPress * Fan(FanNum)%FanWheelDia**4) / (RhoAir * FanVolFlow**2) ![-]
    NormalizedEulerNum = LOG10(EulerNum / Fan(FanNum)%EuMaxEff)
    IF (NormalizedEulerNum <= 0.d0) THEN
      Fan(FanNum)%FanWheelEff = CurveValue(Fan(FanNum)%PLFanEffNormCurveIndex,NormalizedEulerNum)
    ELSE
      Fan(FanNum)%FanWheelEff = CurveValue(Fan(FanNum)%PLFanEffStallCurveIndex,NormalizedEulerNum)
    END IF
    Fan(FanNum)%FanWheelEff = Fan(FanNum)%FanWheelEff * Fan(FanNum)%FanMaxEff ! [-]
    Fan(FanNum)%FanWheelEff = MAX(Fan(FanNum)%FanWheelEff,0.01d0) !Minimum efficiency is 1% to avoid numerical errors

   ! Calculate fan shaft power using fan static air power and fan static efficiency
    Fan(FanNum)%FanShaftPower = Fan(FanNum)%FanAirPower / Fan(FanNum)%FanWheelEff ![W]

   ! Calculate fan shaft speed, fan torque, and motor speed using Wray dimensionless fan airflow model
    IF (NormalizedEulerNum <= 0.d0) THEN
      FanDimFlow = CurveValue(Fan(FanNum)%DimFlowNormCurveIndex,NormalizedEulerNum) ![-]
    ELSE
      FanDimFlow = CurveValue(Fan(FanNum)%DimFlowStallCurveIndex,NormalizedEulerNum) ![-]
    END IF
    FanSpdRadS = FanVolFlow / &
       (FanDimFlow * Fan(FanNum)%FanMaxDimFlow * Fan(FanNum)%FanWheelDia**3) ![rad/s]
    Fan(FanNum)%FanTrq = Fan(FanNum)%FanShaftPower / FanSpdRadS ![N-m]
    Fan(FanNum)%FanSpd =  FanSpdRadS * 9.549296586d0 ![rpm, conversion factor is 30/PI]
    MotorSpeed = Fan(FanNum)%FanSpd * Fan(FanNum)%PulleyDiaRatio ![rpm]

   ! Calculate belt part-load drive efficiency using correlations and coefficients based on ACEEE data
   ! Direct-drive is represented using curve coefficients such that "belt" max eff and PL eff = 1.0
    FanTrqRatio = Fan(FanNum)%FanTrq / Fan(FanNum)%BeltMaxTorque ![-]
    IF ((FanTrqRatio <= Fan(FanNum)%BeltTorqueTrans).AND.(Fan(FanNum)%PLBeltEffReg1CurveIndex /= 0)) THEN
      BeltPLEff = CurveValue(Fan(FanNum)%PLBeltEffReg1CurveIndex,FanTrqRatio) ![-]
    ELSE
      IF ((FanTrqRatio > Fan(FanNum)%BeltTorqueTrans).AND.(FanTrqRatio <= 1.d0) &
          .AND.(Fan(FanNum)%PLBeltEffReg2CurveIndex /= 0)) THEN
        BeltPLEff = CurveValue(Fan(FanNum)%PLBeltEffReg2CurveIndex,FanTrqRatio) ![-]
      ELSE
        IF ((FanTrqRatio > 1.d0).AND.(Fan(FanNum)%PLBeltEffReg3CurveIndex /= 0)) THEN
          BeltPLEff = CurveValue(Fan(FanNum)%PLBeltEffReg3CurveIndex,FanTrqRatio) ![-]
        ELSE
          BeltPLEff = 1.d0 !Direct drive or no curve specified - use constant efficiency
        END IF
      END IF
    END IF
    Fan(FanNum)%BeltEff = Fan(FanNum)%BeltMaxEff * BeltPLEff ![-]
    Fan(FanNum)%BeltEff = MAX(Fan(FanNum)%BeltEff,0.01d0)!Minimum efficiency is 1% to avoid numerical errors

   ! Calculate belt input power using fan shaft power and belt efficiency
    Fan(FanNum)%BeltInputPower = Fan(FanNum)%FanShaftPower/Fan(FanNum)%BeltEff ![W]

   ! Calculate motor part-load efficiency using correlations and coefficients based on MotorMaster+ data
    MotorOutPwrRatio = Fan(FanNum)%BeltInputPower / Fan(FanNum)%MotorMaxOutPwr ![-]
    IF (Fan(FanNum)%PLMotorEffCurveIndex /= 0) THEN
      MotorPLEff = CurveValue(Fan(FanNum)%PLMotorEffCurveIndex,MotorOutPwrRatio) ![-]
    ELSE
      MotorPLEff = 1.d0 !No curve specified - use constant efficiency
    END IF
    Fan(FanNum)%MotEff = Fan(FanNum)%MotorMaxEff * MotorPLEff ![-]
    Fan(FanNum)%MotEff = MAX(Fan(FanNum)%MotEff,0.01d0)!Minimum efficiency is 1% to avoid numerical errors

   ! Calculate motor input power using belt input power and motor efficiency
    Fan(FanNum)%MotorInputPower = Fan(FanNum)%BeltInputPower / Fan(FanNum)%MotEff ![W]

   ! Calculate VFD efficiency using correlations and coefficients based on VFD type
    IF ((TRIM(Fan(FanNum)%VFDEffType) == 'SPEED').AND.(Fan(FanNum)%VFDEffCurveIndex /= 0)) THEN
      VFDSpdRatio = MotorSpeed / Fan(FanNum)%MotorMaxSpd ![-]
      Fan(FanNum)%VFDEff = CurveValue(Fan(FanNum)%VFDEffCurveIndex,VFDSpdRatio) ![-]
    ELSE
      IF ((TRIM(Fan(FanNum)%VFDEffType) == 'POWER').AND.(Fan(FanNum)%VFDEffCurveIndex /= 0)) THEN
        VFDOutPwrRatio = Fan(FanNum)%MotorInputPower / Fan(FanNum)%VFDMaxOutPwr ![-]
        Fan(FanNum)%VFDEff = CurveValue(Fan(FanNum)%VFDEffCurveIndex,VFDOutPwrRatio) ![-]
      ELSE
        ! No curve specified - use constant efficiency
        Fan(FanNum)%VFDMaxOutPwr = 0.d0
        Fan(FanNum)%VFDEff = 0.97d0
      END IF
    ENDIF
    Fan(FanNum)%VFDEff = MAX(Fan(FanNum)%VFDEff,0.01d0)!Minimum efficiency is 1% to avoid numerical errors

   ! Calculate VFD input power using motor input power and VFD efficiency
    Fan(FanNum)%VFDInputPower = Fan(FanNum)%MotorInputPower / Fan(FanNum)%VFDEff ![W]
    Fan(FanNum)%FanPower = Fan(FanNum)%VFDInputPower ![W]

    ! Calculate combined fan system efficiency: includes fan, belt, motor, and VFD
    ! Equivalent to Fan(FanNum)%FanAirPower / Fan(FanNum)%FanPower
    Fan(FanNum)%FanEff = Fan(FanNum)%FanWheelEff * Fan(FanNum)%BeltEff * Fan(FanNum)%MotEff * Fan(FanNum)%VFDEff

!    IF (Fan(FanNum)%EMSFanEffOverrideOn) FanEff = Fan(FanNum)%EMSFanEffValue

   ! Calculate air enthalpy and temperature rise from power entering air stream from fan wheel, belt, and motor
   ! Assumes MotInAirFrac applies to belt and motor but NOT to VFD
    PowerLossToAir = Fan(FanNum)%FanShaftPower &
                     + (Fan(FanNum)%MotorInputPower - Fan(FanNum)%FanShaftPower) * Fan(FanNum)%MotInAirFrac ![W]
    FanEnthalpyChange = PowerLossToAir / MassFlow ![kJ/kg]
    Fan(FanNum)%OutletAirEnthalpy = Fan(FanNum)%InletAirEnthalpy + FanEnthalpyChange ![kJ/kg]

    ! This fan does not change the moisture or mass flow across the component
    Fan(FanNum)%OutletAirHumRat       = Fan(FanNum)%InletAirHumRat ![-]
    Fan(FanNum)%OutletAirMassFlowRate = MassFlow ![kg/s]
    Fan(FanNum)%OutletAirTemp = PsyTdbFnHW(Fan(FanNum)%OutletAirEnthalpy,Fan(FanNum)%OutletAirHumRat)

    !cpw31Aug2010 Temporary code for debugging fan component model
!    WRITE(300,*) TRIM(RoundSigDigits(Fan(FanNum)%RhoAirStdInit,4))//','//TRIM(RoundSigDigits(FanVolFlow,4)) &
!    //','//TRIM(RoundSigDigits(FanOutletVelPress,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%DeltaPress,4)) &
!    //','//TRIM(RoundSigDigits(Fan(FanNum)%FanAirPower,4))//','//TRIM(RoundSigDigits(EulerNum,4)) &
!    //','//TRIM(RoundSigDigits(NormalizedEulerNum,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%FanWheelEff,4))
!    WRITE(301,*) TRIM(RoundSigDigits(Fan(FanNum)%FanShaftPower,4))//','//TRIM(RoundSigDigits(FanDimFlow,4)) &
!    //','//TRIM(RoundSigDigits(Fan(FanNum)%FanTrq,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%FanSpd,4)) &
!    //','//TRIM(RoundSigDigits(Fan(FanNum)%FanShaftPwrMax,4))//','//TRIM(" ") &
!    //','//TRIM(RoundSigDigits(Fan(FanNum)%BeltMaxEff,4))//','//TRIM(RoundSigDigits(FanTrqRatio,4))
!    WRITE(302,*) TRIM(RoundSigDigits(BeltPLEff,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%BeltEff,4)) &
!    //','//TRIM(RoundSigDigits(Fan(FanNum)%BeltInputPower,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%MotorMaxOutPwr,4)) &
!    //','//TRIM(" ")//','//TRIM(RoundSigDigits(Fan(FanNum)%MotorMaxEff,4)) &
!    //','//TRIM(RoundSigDigits(MotorOutPwrRatio,4))//','//TRIM(RoundSigDigits(MotorPLEff,4))
!    WRITE(303,*) TRIM(RoundSigDigits(Fan(FanNum)%MotEff,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%MotorInputPower,4)) &
!    //','//TRIM(RoundSigDigits(VFDOutPwrRatio,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%VFDEff,4)) &
!    //','//TRIM(RoundSigDigits(Fan(FanNum)%FanPower,4))//','//TRIM(RoundSigDigits(Fan(FanNum)%FanEff,4)) &
!    //','//TRIM(RoundSigDigits(PowerLossToAir,4))//','//TRIM(RoundSigDigits(FanEnthalpyChange,4))
!    WRITE(304,*) TRIM(CurMnDy)//','//TRIM(CreateSysTimeIntervalString())

  Else
    !Fan is OFF and not operating -- no power consumed and zero mass flow rate
    Fan(FanNum)%FanPower = 0.0d0
    Fan(FanNum)%FanShaftPower = 0.0d0
    PowerLossToAir = 0.0d0
    Fan(FanNum)%OutletAirMassFlowRate = 0.0d0
    Fan(FanNum)%OutletAirHumRat = Fan(FanNum)%InletAirHumRat
    Fan(FanNum)%OutletAirEnthalpy = Fan(FanNum)%InletAirEnthalpy
    Fan(FanNum)%OutletAirTemp = Fan(FanNum)%InletAirTemp
    ! Set the Control Flow variables to 0.0 flow when OFF.
    Fan(FanNum)%MassFlowRateMaxAvail = 0.0d0
    Fan(FanNum)%MassFlowRateMinAvail = 0.0d0

    Fan(FanNum)%DeltaPress = 0.0d0
    Fan(FanNum)%FanAirPower = 0.0d0
    Fan(FanNum)%FanWheelEff = 0.0d0
    Fan(FanNum)%FanSpd = 0.0d0
    Fan(FanNum)%FanTrq = 0.0d0
    Fan(FanNum)%BeltEff = 0.0d0
    Fan(FanNum)%BeltInputPower = 0.0d0
    Fan(FanNum)%MotEff = 0.0d0
    Fan(FanNum)%MotorInputPower = 0.0d0
    Fan(FanNum)%VFDEff = 0.0d0
    Fan(FanNum)%VFDInputPower = 0.0d0
    Fan(FanNum)%FanEff = 0.0d0
  END IF

  RETURN
END SUBROUTINE SimComponentModelFan

! End Algorithm Section of the Module
! *****************************************************************************

! Beginning of Update subroutines for the Fan Module
! *****************************************************************************

SUBROUTINE UpdateFan(FanNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   April 1998
          !       MODIFIED       L. Gu, Feb. 1, 2007, No unbalance airflow when Zone Exhaust Fans are used in the AirflowNetwork
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the fan outlet nodes.

          ! METHODOLOGY EMPLOYED:
          ! Data is moved from the fan data structure to the fan outlet nodes.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          USE DataAirflowNetwork, ONLY: AirflowNetworkNumOfExhFan
          USE DataContaminantBalance, ONLY: Contaminant

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   Integer, Intent(IN) :: FanNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  Integer             :: OutletNode
  Integer             :: InletNode


   OutletNode = Fan(FanNum)%OutletNodeNum
   InletNode = Fan(FanNum)%InletNodeNum

   ! Set the outlet air nodes of the fan
   Node(OutletNode)%MassFlowRate  = Fan(FanNum)%OutletAirMassFlowRate
   Node(OutletNode)%Temp          = Fan(FanNum)%OutletAirTemp
   Node(OutletNode)%HumRat        = Fan(FanNum)%OutletAirHumRat
   Node(OutletNode)%Enthalpy      = Fan(FanNum)%OutletAirEnthalpy
   ! Set the outlet nodes for properties that just pass through & not used
   Node(OutletNode)%Quality         = Node(InletNode)%Quality
   Node(OutletNode)%Press           = Node(InletNode)%Press

   ! Set the Node Flow Control Variables from the Fan Control Variables
   Node(OutletNode)%MassFlowRateMaxAvail = Fan(FanNum)%MassFlowRateMaxAvail
   Node(OutletNode)%MassFlowRateMinAvail = Fan(FanNum)%MassFlowRateMinAvail

   IF (Fan(FanNum)%FanType_Num == FanType_ZoneExhaust) THEN
     Node(InletNode)%MassFlowRate = Fan(FanNum)%InletAirMassFlowRate
     IF (AirflowNetworkNumOfExhFan .EQ. 0) THEN
       UnbalExhMassFlow = Fan(FanNum)%InletAirMassFlowRate
       IF (Fan(FanNum)%BalancedFractSchedNum > 0) THEN
         BalancedExhMassFlow = UnbalExhMassFlow * GetCurrentScheduleValue(Fan(FanNum)%BalancedFractSchedNum)
       ELSE
         BalancedExhMassFlow = 0.d0
       ENDIF
     ELSE
       UnbalExhMassFlow = 0.d0
       BalancedExhMassFlow = 0.d0
     END IF
     Fan(FanNum)%UnbalancedOutletMassFlowRate = UnbalExhMassFlow - BalancedExhMassFlow
     Fan(FanNum)%BalancedOutletMassFlowRate   = BalancedExhMassFlow
   END IF

  IF (Contaminant%CO2Simulation) Then
    Node(OutletNode)%CO2 = Node(InletNode)%CO2
  End If

  IF (Contaminant%GenericContamSimulation) Then
    Node(OutletNode)%GenContam = Node(InletNode)%GenContam
  End If

  RETURN
END Subroutine UpdateFan

!        End of Update subroutines for the Fan Module
! *****************************************************************************


! Beginning of Reporting subroutines for the Fan Module
! *****************************************************************************

SUBROUTINE ReportFan(FanNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   April 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine updates the report variables for the fans.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use DataHVACGlobals, ONLY: TimeStepSys, FanElecPower
  USE DataAirLoop,     ONLY: LoopOnOffFanRTF
  USE DataGlobals,     ONLY: SecInHour

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
   Integer, Intent(IN) :: FanNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

    Fan(FanNum)%FanEnergy=Fan(FanNum)%FanPower*TimeStepSys*SecInHour
    Fan(FanNum)%DeltaTemp=Fan(FanNum)%OutletAirTemp - Fan(FanNum)%InletAirTemp
    FanElecPower = Fan(FanNum)%FanPower

    If (Fan(FanNum)%FanType_Num == FanType_SimpleOnOff) Then
      LoopOnOffFanRTF = Fan(FanNum)%FanRunTimeFraction
    End If

  RETURN
END Subroutine ReportFan

!        End of Reporting subroutines for the Fan Module
! *****************************************************************************

! Beginning of Utility subroutines for the Fan Module
! *****************************************************************************
SUBROUTINE GetFanIndex(FanName,FanIndex,ErrorsFound,ThisObjectType)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   June 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets an index for a given fan -- issues error message if that fan
          ! is not legal fan.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: FanName
  INTEGER, INTENT(INOUT)       :: FanIndex
  LOGICAL, INTENT(INOUT)       :: ErrorsFound
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ThisObjectType

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (GetFanInputFlag) THEN  !First time subroutine has been entered
    CALL GetFanInput
    GetFanInputFlag=.false.
  End If

  FanIndex = FindItemInList(FanName,Fan%FanName,NumFans)
  IF (FanIndex == 0) THEN
    IF (PRESENT(ThisObjectType)) THEN
      CALL ShowSevereError(TRIM(ThisObjectType)//', GetFanIndex: Fan not found='//TRIM(FanName))
    ELSE
      CALL ShowSevereError('GetFanIndex: Fan not found='//TRIM(FanName))
    ENDIF
    ErrorsFound=.TRUE.
  ENDIF

  RETURN

END SUBROUTINE GetFanIndex

SUBROUTINE GetFanVolFlow(FanIndex, FanVolFlow)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   August 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the fan volumetric flow for use by zone equipment (e.g. Packaged Terminal Heat Pump)
          ! Zone equipment must ensure that a properly sized fan is used to meet the maximum supply air flow rate

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: FanIndex
  REAL(r64),    INTENT(INOUT)       :: FanVolFlow

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  IF(FanIndex .EQ. 0)THEN
    FanVolFlow = 0.0d0
  ELSE
    FanVolFlow = Fan(FanIndex)%MaxAirFlowRate
  END IF

  RETURN

END SUBROUTINE GetFanVolFlow

SUBROUTINE GetFanPower(FanIndex, FanPower)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   July 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the fan power draw

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: FanIndex
  REAL(r64),    INTENT(OUT)       :: FanPower

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  IF(FanIndex .EQ. 0)THEN
    FanPower = 0.d0
  ELSE
    FanPower = Fan(FanIndex)%FanPower
  END IF

  RETURN

END SUBROUTINE GetFanPower

SUBROUTINE GetFanType(FanName,FanType,ErrorsFound,ThisObjectType,ThisObjectName)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   August 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets an integer type for a given fan -- issues error message if that fan
          ! is not a legal fan.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: FanName      ! Fan name
  INTEGER, INTENT(INOUT)       :: FanType      ! returned fantype number
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! error indicator
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ThisObjectType  ! parent object type (for error message)
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: ThisObjectName  ! parent object name (for error message)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: FanIndex

  IF (GetFanInputFlag) THEN  !First time subroutine has been entered
    CALL GetFanInput
    GetFanInputFlag=.false.
  End If

  FanIndex = FindItemInList(FanName,Fan%FanName,NumFans)
  IF (FanIndex == 0) THEN
    IF (PRESENT(ThisObjectType) .and. PRESENT(ThisObjectName)) THEN
      CALL ShowSevereError('GetFanType: '//TRIM(ThisObjectType)//'="'//trim(ThisObjectName)//'",'//  &
           ' invalid Fan specified="'//TRIM(FanName)//'".')
    ELSEIF (PRESENT(ThisObjectType)) THEN
      CALL ShowSevereError(TRIM(ThisObjectType)//', GetFanType: Fan not found='//TRIM(FanName))
    ELSE
      CALL ShowSevereError('GetFanType: Fan not found='//TRIM(FanName))
    ENDIF
    FanType     = 0
    ErrorsFound = .TRUE.
  ELSE
    FanType     = Fan(FanIndex)%FanType_Num
  ENDIF

  RETURN

END SUBROUTINE GetFanType

FUNCTION GetFanDesignVolumeFlowRate(FanType,FanName,ErrorsFound,FanIndex) RESULT(DesignVolumeFlowRate)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2006
          !       MODIFIED       R. Raustad, Aug 2007 - added optional fan index
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the design volume flow rate for the given fan and returns it.  If
          ! incorrect fan type or name is given, errorsfound is returned as true and value is returned
          ! as negative.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: FanType      ! must match fan types in this module
  CHARACTER(len=*), INTENT(IN) :: FanName      ! must match fan names for the fan type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER, OPTIONAL, INTENT(IN):: FanIndex     ! index to fan
  REAL(r64)                    :: DesignVolumeFlowRate ! returned flow rate of matched fan

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichFan

  ! Obtains and Allocates fan related parameters from input file
  IF (GetFanInputFlag) THEN  !First time subroutine has been entered
    CALL GetFanInput
    GetFanInputFlag=.false.
  End If

  IF(PRESENT(FanIndex))THEN
    DesignVolumeFlowRate=Fan(FanIndex)%MaxAirFlowRate
  ELSE
    WhichFan=FindItemInList(FanName,Fan%FanName,NumFans)
    IF (WhichFan /= 0) THEN
      DesignVolumeFlowRate=Fan(WhichFan)%MaxAirFlowRate
    ELSE
      CALL ShowSevereError('GetFanDesignVolumeFlowRate: Could not find Fan, Type="'//TRIM(FanType)//  &
                  '" Name="'//TRIM(FanName)//'"')
      CALL ShowContinueError('... Design Volume Flow rate returned as -1000.')
      ErrorsFound=.true.
      DesignVolumeFlowRate=-1000.d0
    ENDIF
  END IF

  RETURN

END FUNCTION GetFanDesignVolumeFlowRate

FUNCTION GetFanInletNode(FanType,FanName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given fan and returns the inlet node.  If
          ! incorrect fan type or name is given, errorsfound is returned as true and value is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: FanType      ! must match fan types in this module
  CHARACTER(len=*), INTENT(IN) :: FanName      ! must match fan names for the fan type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned outlet node of matched fan

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichFan

  ! Obtains and Allocates fan related parameters from input file
  IF (GetFanInputFlag) THEN  !First time subroutine has been entered
    CALL GetFanInput
    GetFanInputFlag=.false.
  End If

  WhichFan=FindItemInList(FanName,Fan%FanName,NumFans)
  IF (WhichFan /= 0) THEN
    NodeNumber=Fan(WhichFan)%InletNodeNum
  ELSE
    CALL ShowSevereError('GetFanInletNode: Could not find Fan, Type="'//TRIM(FanType)//'" Name="'//TRIM(FanName)//'"')
    ErrorsFound=.true.
    NodeNumber=0
  ENDIF

  RETURN

END FUNCTION GetFanInletNode

FUNCTION GetFanOutletNode(FanType,FanName,ErrorsFound) RESULT(NodeNumber)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given fan and returns the outlet node.  If
          ! incorrect fan type or name is given, errorsfound is returned as true and value is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: FanType      ! must match fan types in this module
  CHARACTER(len=*), INTENT(IN) :: FanName      ! must match fan names for the fan type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: NodeNumber   ! returned outlet node of matched fan

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichFan

  ! Obtains and Allocates fan related parameters from input file
  IF (GetFanInputFlag) THEN  !First time subroutine has been entered
    CALL GetFanInput
    GetFanInputFlag=.false.
  End If

  WhichFan=FindItemInList(FanName,Fan%FanName,NumFans)
  IF (WhichFan /= 0) THEN
    NodeNumber=Fan(WhichFan)%OutletNodeNum
  ELSE
    CALL ShowSevereError('GetFanOutletNode: Could not find Fan, Type="'//TRIM(FanType)//'" Name="'//TRIM(FanName)//'"')
    ErrorsFound=.true.
    NodeNumber=0
  ENDIF

  RETURN

END FUNCTION GetFanOutletNode

FUNCTION GetFanAvailSchPtr(FanType,FanName,ErrorsFound) RESULT(FanAvailSchPtr)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   September 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given fan and returns the availability schedule pointer.  If
          ! incorrect fan type or name is given, errorsfound is returned as true and value is returned
          ! as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: FanType      ! must match fan types in this module
  CHARACTER(len=*), INTENT(IN) :: FanName      ! must match fan names for the fan type
  LOGICAL, INTENT(INOUT)       :: ErrorsFound  ! set to true if problem
  INTEGER                      :: FanAvailSchPtr   ! returned availability schedule pointer of matched fan

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichFan

  ! Obtains and Allocates fan related parameters from input file
  IF (GetFanInputFlag) THEN  !First time subroutine has been entered
    CALL GetFanInput
    GetFanInputFlag=.false.
  End If

  WhichFan=FindItemInList(FanName,Fan%FanName,NumFans)
  IF (WhichFan /= 0) THEN
    FanAvailSchPtr=Fan(WhichFan)%AvailSchedPtrNum
  ELSE
    CALL ShowSevereError('GetFanAvailSchPtr: Could not find Fan, Type="'//TRIM(FanType)//'" Name="'//TRIM(FanName)//'"')
    ErrorsFound=.true.
    FanAvailSchPtr=0
  ENDIF

  RETURN

END FUNCTION GetFanAvailSchPtr

FUNCTION GetFanSpeedRatioCurveIndex(FanType,FanName,IndexIn) RESULT(FanSpeedRatioCurveIndex)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   September 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given fan and returns the fan speed curve pointer.  If
          ! incorrect fan type or name is given, errorsfound is returned as true and value is returned
          ! as zero. If optional index argument is passed along with fan type and name, the index is set.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(INOUT) :: FanType   ! must match fan types in this module (set if nonzero index passed)
  CHARACTER(len=*), INTENT(INOUT) :: FanName   ! must match fan names for the fan type (set if nonzero index passed)
  INTEGER, OPTIONAL, INTENT(INOUT):: IndexIn   ! optional fan index if fan type and name are unknown or index needs setting
  INTEGER      :: FanSpeedRatioCurveIndex      ! index to fan speed ratio curve object

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichFan

  ! Obtains and Allocates fan related parameters from input file
  IF (GetFanInputFlag) THEN  !First time subroutine has been entered
    CALL GetFanInput
    GetFanInputFlag=.false.
  End If

  IF(PRESENT(IndexIn))THEN
    IF(IndexIn .GT. 0)THEN
      WhichFan = IndexIn
      FanType = Fan(WhichFan)%FanType
      FanName = Fan(WhichFan)%FanName
    ELSE
      WhichFan=FindItemInList(FanName,Fan%FanName,NumFans)
      IndexIn = WhichFan
    END IF
  ELSE
    WhichFan=FindItemInList(FanName,Fan%FanName,NumFans)
  END IF

  IF (WhichFan /= 0) THEN
    FanSpeedRatioCurveIndex=Fan(WhichFan)%FanPowerRatAtSpeedRatCurveIndex
  ELSE
    CALL ShowSevereError('GetFanSpeedRatioCurveIndex: Could not find Fan, Type="'//TRIM(FanType)//'" Name="'//TRIM(FanName)//'"')
    FanSpeedRatioCurveIndex=0
  ENDIF

  RETURN

END FUNCTION GetFanSpeedRatioCurveIndex

SUBROUTINE SetFanData(FanNum, ErrorsFound, FanName, MaxAirVolFlow, MinAirVolFlow)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   October 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine was designed for to autosize the HeatExchanger:AirToAir:SensibleAndLatent using
          ! information from the ZoneHVAC:EnergyRecoveryVentilator object.
          ! This is an illustration of setting data from an outside source.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)         :: FanNum              ! Index of fan
  LOGICAL, INTENT(INOUT)      :: ErrorsFound         ! Set to true if certain errors found
  CHARACTER(len=*),INTENT(IN) :: FanName             ! Name of fan
  REAL(r64), OPTIONAL         :: MaxAirVolFlow       ! Fan air volumetric flow rate    [m3/s]
  REAL(r64), OPTIONAL         :: MinAirVolFlow       ! Fan air volumetric flow rate    [m3/s]

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichFan   ! index to generic HX

  ! Obtains and Allocates fan related parameters from input file
  IF (GetFanInputFlag) THEN  !First time subroutine has been entered
    CALL GetFanInput
    GetFanInputFlag=.false.
  End If

  IF(FanNum .EQ. 0)THEN
    WhichFan = FindItemInList(FanName,Fan%FanName,NumFans)
  ELSE
    WhichFan = FanNum
  END IF

  IF (WhichFan <= 0 .OR. WhichFan .GT. NumFans) THEN
    CALL ShowSevereError('SetFanData: Could not find fan = "'//TRIM(FanName)//'"')
    ErrorsFound=.TRUE.
    RETURN
  ENDIF

  IF(PRESENT(MaxAirVolFlow))THEN
    Fan(WhichFan)%MaxAirFlowRate = MaxAirVolFlow
  END IF

  IF(PRESENT(MinAirVolFlow))THEN
    Fan(WhichFan)%MinAirFlowRate = MinAirVolFlow
  END IF

  RETURN

END SUBROUTINE SetFanData

! End of Utility subroutines for the Fan Module
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

End Module Fans


