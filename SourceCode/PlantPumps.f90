MODULE Pumps

          ! MODULE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Sept 1998
          !       MODIFIED       July 2001, Richard Liesen
          !                      July 2001, Rick Strand (new "local" pump control method)
          !                      Feb 2005, Rahul Chillar(added condensate pump for steam systems)
          !                      Jan 2006, Sankaranarayanan (Added pump banks to the library of pumps)
          !                      May 2009, Brent Griffith (added support for EMS override of massflow)
          !                      Aug 2010, Edwin Lee (refactored code, significant clean-up)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Encapsulates the data and algorithms to simulate pumps.

          ! REFERENCES:
          ! HVAC 2 Toolkit:  A Toolkit for Secondary HVAC System
          ! Energy Calculations, ASHRAE, 1993, pp2-10 to 2-15

          ! USE STATEMENTS:
 USE DataPrecisionGlobals, ONLY: r64
 USE DataGlobals,          ONLY: InitConvTemp, AnyEnergyManagementSystemInModel, SecInHour, BeginEnvrnFlag
 USE DataHVACGlobals,      ONLY: MaxNameLength, SmallWaterVolFlow, NumPlantLoops, NumCondLoops, ForceOff, CycleOn, TimeStepSys
 USE DataLoopNode,         ONLY: NodeID, Node, NodeType_Water, NodeType_Steam, NodeConnectionType_Inlet,   &
                                 NodeConnectionType_Outlet, ObjectIsNotParent
 USE DataInterfaces,       ONLY: ShowRecurringWarningErrorAtEnd, ShowRecurringContinueErrorAtEnd, &
                                 ShowFatalError, ShowWarningError, ShowContinueError, ShowSevereError, &
                                 ShowContinueErrorTimeStamp, ShowWarningMessage, SetupEMSInternalVariable, &
                                 SetupEMSActuator

 IMPLICIT NONE

 PRIVATE
          !MODULE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: Continuous   = 1  ! Pump control type (pump always running)
  INTEGER, PARAMETER :: Intermittent = 2  ! Pump control type (pump runs only when there is a demand)

  INTEGER, PARAMETER :: VFDManual       = 1  ! VFD control type (Scheduled RPM)
  INTEGER, PARAMETER :: VFDAutomatic    = 2  ! VFD control type (Variable RPM according to flow request)

  INTEGER, PARAMETER :: OptimalScheme = 1    ! Control sequencing for pump bank
  INTEGER, PARAMETER :: SequentialScheme = 2 ! Control sequencing for pump bank
  INTEGER, PARAMETER :: UserDefined = 3      ! Control sequencing for pump bank

  CHARACTER(len=*), PARAMETER :: cPump_VarSpeed     = 'Pump:VariableSpeed           '
  INTEGER,          PARAMETER :: Pump_VarSpeed      = 101
  CHARACTER(len=*), PARAMETER :: cPump_ConSpeed     = 'Pump:ConstantSpeed           '
  INTEGER,          PARAMETER :: Pump_ConSpeed      = 102
  CHARACTER(len=*), PARAMETER :: cPump_Cond         = 'Pump:VariableSpeed:Condensate'
  INTEGER,          PARAMETER :: Pump_Cond          = 103
  CHARACTER(len=*), PARAMETER :: cPumpBank_VarSpeed = 'HeaderedPumps:VariableSpeed  '
  INTEGER,          PARAMETER :: PumpBank_VarSpeed  = 104
  CHARACTER(len=*), PARAMETER :: cPumpBank_ConSpeed = 'HeaderedPumps:ConstantSpeed  '
  INTEGER,          PARAMETER :: PumpBank_ConSpeed  = 105
  CHARACTER(len=*), PARAMETER, DIMENSION(101:105) :: cPumpTypes=  &
    (/cPump_VarSpeed,      &
      cPump_ConSpeed,      &
      cPump_Cond,          &
      cPumpBank_VarSpeed,  &
      cPumpBank_ConSpeed/)

          ! DERIVED TYPE DEFINITIONS
  TYPE PumpVFDControlData
    CHARACTER(len=MaxNameLength) :: Name               = '  '
    CHARACTER(len=MaxNameLength) :: ManualRPMSchedName  = '  '
    INTEGER                      :: ManualRPMSchedIndex = 0
    CHARACTER(len=MaxNameLength) :: LowerPsetSchedName = '  '
    INTEGER                      :: LowerPsetSchedIndex= 0
    CHARACTER(len=MaxNameLength) :: UpperPsetSchedName = '  '
    INTEGER                      :: UpperPsetSchedIndex= 0
    CHARACTER(len=MaxNameLength) :: MinRPMSchedName  = '  '
    INTEGER                      :: MinRPMSchedIndex= 0
    CHARACTER(len=MaxNameLength) :: MaxRPMSchedName  = '  '
    INTEGER                      :: MaxRPMSchedIndex= 0
    INTEGER           :: VFDControlType               = 0    !Integer equivalent of VFDControlType
    REAL(r64)         :: MaxRPM                       = 0.0d0  !Maximum RPM range value - schedule limit
    REAL(r64)         :: MinRPM                       = 0.0d0  !Minimum RPM range value - schedule limit
    REAL(r64)         :: PumpActualRPM                = 0.0d0  !RPM recalculated from final flow through the loop
  END TYPE PumpVFDControlData


  TYPE, PUBLIC :: PumpSpecs
    CHARACTER(len=MaxNameLength) :: Name               = ' '  ! user identifier
    CHARACTER(len=MaxNameLength) :: PumpSchedule       = ' '  ! Schedule to modify the design nominal capacity of the pump
    CHARACTER(len=MaxNameLength) :: PressureCurve_Name = ' '  !- placeholder for pump curve name
    INTEGER           :: PumpType               = 0    ! pump type integer, based on local parameter values, used to identify
                                                       ! index in the cPumpTypes string array to do error reporting
    INTEGER           :: TypeOf_Num             = 0    ! pump type of number in reference to the dataplant values
    INTEGER           :: LoopNum                = 0    ! loop where pump is located
    INTEGER           :: LoopSideNum            = 0    ! loopside index on loop where pump is located
    INTEGER           :: BranchNum              = 0    ! branch index on loopside where pump is located
    INTEGER           :: CompNum                = 0    ! component index on branch where pump is located
    INTEGER           :: PumpControl            = 0    ! Integer equivalent of PumpControlType
    INTEGER           :: PumpScheduleIndex      = 0    ! Schedule Pointer
    INTEGER           :: InletNodeNum           = 0    ! Node number on the inlet side of the plant
    INTEGER           :: OutletNodeNum          = 0    ! Node number on the outlet side of the plant
    INTEGER           :: SequencingScheme       = 0    ! Optimal, Sequential, User-Defined
    INTEGER           :: FluidIndex             = 0    ! Index for Fluid Properties
    INTEGER           :: NumPumpsInBank         = 0    ! Node number on the inlet side of the plant
    INTEGER           :: PowerErrIndex1         = 0    ! for recurring errors
    INTEGER           :: PowerErrIndex2         = 0    ! for recurring errors
    REAL(r64)         :: MinVolFlowRateFrac     = 0.0d0  ! minimum schedule value fraction modifier
    REAL(r64)         :: NomVolFlowRate         = 0.0d0  ! design nominal capacity of Pump
    REAL(r64)         :: MassFlowRateMax        = 0.0d0  ! design nominal capacity of Pump
    LOGICAL           :: EMSMassFlowOverrideOn  = .FALSE. ! if true, then EMS is calling to override flow requests.
    REAL(r64)         :: EMSMassFlowValue       = 0.0D0 ! EMS value to use for mass flow rate [kg/s]
    REAL(r64)         :: NomSteamVolFlowRate    = 0.0d0   ! For Steam Pump
    REAL(r64)         :: MinVolFlowRate         = 0.0d0   ! For a Variable Flow Pump this is the minimum capacity during operation.
    REAL(r64)         :: MassFlowRateMin        = 0.0d0   ! For a Variable Flow Pump this is the minimum capacity during operation.
    REAL(r64)         :: NomPumpHead            = 0.0d0   ! design nominal head pressure of Pump, [Pa]
    LOGICAL           :: EMSPressureOverrideOn  = .FALSE. ! if true, EMS is calling to override pump pressure
    REAL(r64)         :: EMSPressureOverrideValue = 0.0d0 ! EMS value to use for pressure [Pa]
    REAL(r64)         :: NomPowerUse            = 0.0d0   ! design nominal capacity of Pump
    REAL(r64)         :: MotorEffic             = 0.0d0   ! efficiency of the motor
    REAL(r64)         :: PumpEffic              = 0.0d0   ! efficiency of the pump
    REAL(r64)         :: FracMotorLossToFluid   = 0.0d0   ! ?????
    REAL(r64)         :: Energy                 = 0.0d0   ! Energy consumed
    REAL(r64)         :: Power                  = 0.0d0   ! Power used
    REAL(r64), DIMENSION(4):: PartLoadCoef      = 0.0d0   ! Pump Curve Coefficients
    INTEGER           :: PressureCurve_Index    = 0     ! Pointer to a pump coefficient curve
    REAL(r64)         :: PumpMassFlowRateMaxRPM = 0.0d0 ! Mass flow rate calculated from maximum rpm
    REAL(r64)         :: PumpMassFlowRateMinRPM = 0.0d0 ! Mass flow rate calculated from minimum rpm
    REAL(r64)         :: MinPhiValue            = 0.0d0 ! Minimum value of Phi (from CurveManager)
    REAL(r64)         :: MaxPhiValue            = 0.0d0 ! Maximum value of Phi (from CurveManager)
    REAL(r64)         :: ImpellerDiameter       = 0.0d0 ! Pump Impeller Diameter [m]
    REAL(r64)         :: RotSpeed_RPM           = 0.0d0 ! Rotational speed used for input in revs/min
    REAL(r64)         :: RotSpeed               = 0.0d0 ! Rotational speed for calculations in revs/sec
    LOGICAL           :: PumpInitFlag           = .TRUE.
    LOGICAL           :: PumpOneTimeFlag        = .TRUE.
    LOGICAL           :: CheckEquipName         = .TRUE.
    LOGICAL           :: HasVFD                 = .FALSE.
    TYPE(PumpVFDControlData) :: VFD
    LOGICAL           :: OneTimePressureWarning = .TRUE.
    LOGICAL           :: HeatLossesToZone       = .FALSE. ! if true then pump losses added to surrounding zone
    INTEGER           :: ZoneNum                = 0       ! index for zone surrounding pump
    REAL(r64)         :: SkinLossRadFraction    = 0.d0    ! radiative split for skin losses to zone
  END TYPE PumpSpecs

  TYPE ReportVars
    INTEGER           :: NumPumpsOperating            = 0    !Used in pump bank. reports how many pumps are ON
    REAL(r64)         :: PumpMassFlowRate             = 0.0d0  !Mass flow rate of the pump
    REAL(r64)         :: PumpHeattoFluid              = 0.0d0  !Heat transfer from pump to fluid (W)
    REAL(r64)         :: PumpHeattoFluidEnergy        = 0.0d0  !Pump Energy dissipated into fluid stream
    REAL(r64)         :: OutletTemp                   = 0.0d0  !Pump outlet temperature
    REAL(r64)         :: ShaftPower                   = 0.0d0  !Power input at the shaft
    REAL(r64)         :: ZoneTotalGainRate            = 0.d0 ! total pump skin losses to zone (W)
    REAL(r64)         :: ZoneTotalGainEnergy          = 0.d0 ! total pump skin losses to zone energy (J)
    REAL(r64)         :: ZoneConvGainRate             = 0.d0 ! pump skin losses convecting to zone air (W)
    REAL(r64)         :: ZoneRadGainRate              = 0.d0 ! pump skin losses radiating to inside of zone (W)
  END TYPE ReportVars

            ! MODULE VARIABLE DECLARATIONS:
  INTEGER       :: NumPumps                           = 0    !Num Pumps (used in pump bank)
  INTEGER       :: NumPumpsRunning                    = 0    !Num of pumps ON (used in pump bank)
  INTEGER       :: NumPumpsFullLoad                   = 0    !Num pumps running at full load (used in pump bank)
!  INTEGER       :: NumPumpsPartLoad                   = 0    !Num pumps running at part load (used in pump bank)

       !simulation and reporting variables
!  REAL(r64)           :: OutletTemp                   = 0.0d0  ! pump outlet temperature
  REAL(r64)           :: PumpMassFlowRate             = 0.0d0  ! mass flow rate at pump inlet node
!  REAL(r64)           :: PumpPress                    = 0.0d0  ! For Passing around the steam loops
!  REAL(r64)           :: PumpQuality                  = 0.0d0  ! For Passing around the steam loops=0.0 here
  REAL(r64)           :: PumpHeattoFluid              = 0.0d0  ! Pump Power dissipated in fluid stream
  REAL(r64)           :: Power                        = 0.0d0  ! Pump Electric power
  REAL(r64)           :: ShaftPower                   = 0.0d0  ! Power passing through pump shaft

  TYPE(PumpSpecs),  PUBLIC, ALLOCATABLE, DIMENSION(:) :: PumpEquip
  TYPE(ReportVars), ALLOCATABLE, DIMENSION(:) :: PumpEquipReport

          ! SUBROUTINE SPECIFICATIONS FOR MODULE PrimaryPlantLoops
 PUBLIC     SimPumps
 PRIVATE    GetPumpInput
 PRIVATE    InitializePumps
 PRIVATE    SetupPumpMinMaxFlows
 PRIVATE    CalcPumps
 PRIVATE    SizePump
 PRIVATE    ReportPumps
 PRIVATE    GetRequiredMassFlowRate
 CONTAINS

!*************************************************************************!
SUBROUTINE SimPumps(PumpName, LoopNum, FlowRequest, PumpRunning, PumpIndex, PumpHeat)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   July 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages the pump operation based on the type of
          ! pump and the pump controls (continuous, intermittent, etc.).  The
          ! result of this subroutine is that the pump has been simulated for
          ! the necessary loop and the PumpRunning has been correctly set.

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General,        ONLY: TrimSigDigits
  USE DataPlant,      ONLY: PlantLoop, FlowPumpQuery

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN)     :: PumpName  ! Name of pump to be managed
  INTEGER,          INTENT(IN)     :: LoopNum ! Plant loop number
  REAL(r64),        INTENT(IN)     :: FlowRequest ! requested flow from adjacent demand side
  LOGICAL,          INTENT(OUT)    :: PumpRunning  ! .TRUE. if the loop pump is actually operating
  INTEGER,          INTENT(IN OUT) :: PumpIndex
  REAL(r64),        INTENT(OUT)    :: PumpHeat

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL, SAVE   :: GetInputFlag = .TRUE.  ! Get input once and once only
  INTEGER         :: PumpNum                ! Pump index within PumpEquip derived type

  ! Get input from IDF one time
  IF (GetInputFlag) THEN
    CALL GetPumpInput
    GetInputFlag=.false.
  END IF

  ! Exit early if no pumps found
  IF (NumPumps == 0) THEN
    PumpHeat=0.0d0
    RETURN
  ENDIF

  ! Setup pump component index if needed
  IF (PumpIndex == 0) THEN
    PumpNum = FindItemInList(PumpName,PumpEquip%Name,NumPumps)  ! Determine which pump to simulate
    IF (PumpNum == 0) THEN
      CALL ShowFatalError('ManagePumps: Pump requested not found ='//TRIM(PumpName)) ! Catch any bad names before crashing
    ENDIF
    PumpIndex=PumpNum
  ELSE
    PumpNum=PumpIndex
    IF (PumpEquip(PumpNum)%CheckEquipName) THEN
      IF (PumpNum > NumPumps .or. PumpNum < 1) THEN
        CALL ShowFatalError('ManagePumps: Invalid PumpIndex passed='//TRIM(TrimSigDigits(PumpNum))// &
                            ', Number of Pumps='//TRIM(TrimSigDigits(NumPumps))//', Pump name='//TRIM(PumpName))
      ENDIF
      IF (PumpName /= PumpEquip(PumpNum)%Name) THEN
        CALL ShowFatalError('ManagePumps: Invalid PumpIndex passed='//TRIM(TrimSigDigits(PumpNum))// &
                            ', Pump name='//TRIM(PumpName)//', stored Pump Name for that index='//TRIM(PumpEquip(PumpNum)%Name))
      ENDIF
      PumpEquip(PumpNum)%CheckEquipName=.FALSE.
    ENDIF
  ENDIF

  ! Perform one-time and begin-environment initialization
  CALL InitializePumps(PumpNum)

  ! If all we need is to set outlet min/max avail, then just do it and get out.  Also, we only do min/max avail on flow query
  IF (PlantLoop(LoopNum)%LoopSide(PumpEquip(PumpNum)%LoopSideNum)%FlowLock == FlowPumpQuery) THEN
    CALL SetupPumpMinMaxFlows(LoopNum, PumpNum)
    RETURN
  END IF

  ! Set pump flow rate and calculate power
  CALL CalcPumps(PumpNum, FlowRequest, PumpRunning)

  ! Update pump reporting data
  CALL ReportPumps(PumpNum)

  ! Send this up to the calling routine
  PumpHeat = PumpHeattoFluid

 RETURN

END SUBROUTINE SimPumps
!*************************************************************************!

!*************************************************************************!
SUBROUTINE GetPumpInput()

            ! SUBROUTINE INFORMATION:
            !       AUTHOR:          Dan Fisher
            !       DATE WRITTEN:    April 1998
            !       MODIFIED:        July 2001, Rick Strand (addition of pump controls)
            !                        May 2009, Brent Griffith (added EMS calls)

            ! PURPOSE OF THIS SUBROUTINE:
            ! This routine will get the input
            ! required by the pump simulation.

            ! PUMP:VARIABLE SPEED,
            ! This pump model is described in the ASHRAE secondary HVAC toolkit.

            ! REFERENCES:
            ! HVAC 2 Toolkit:  A Toolkit for Secondary HVAC System
            !  Energy Calculations, ASHRAE, 1993, pp2-10 to 2-15

            ! USE STATEMENTS:
  USE InputProcessor,        ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString, FindItemInList
  USE DataIPShortCuts,       ONLY: lAlphaFieldBlanks, cAlphaFieldNames, cNumericFieldNames, lNumericFieldBlanks, &
                                   cCurrentModuleObject, cAlphaArgs, rNumericArgs
  USE NodeInputManager,      ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet
  USE FluidProperties,       ONLY: GetSatDensityRefrig, GetDensityGlycol
  USE DataSizing,            ONLY: Autosize
  USE DataInterfaces
  USE CurveManager,          ONLY: GetCurveType, GetCurveIndex, GetCurveMinMaxValues
  USE DataPlant,             ONLY: TypeOf_PumpVariableSpeed, TypeOf_PumpConstantSpeed, TypeOf_PumpCondensate, &
                                   TypeOf_PumpBankVariableSpeed, TypeOf_PumpBankConstantSpeed
  USE ScheduleManager,       ONLY: GetScheduleIndex,CheckScheduleValueMinMax
  USE DataHeatBalance,       ONLY: IntGainTypeOf_Pump_VarSpeed, IntGainTypeOf_Pump_ConSpeed, &
                                   IntGainTypeOf_Pump_Cond, IntGainTypeOf_PumpBank_VarSpeed, &
                                   IntGainTypeOf_PumpBank_ConSpeed, Zone
  USE DataGlobals,           ONLY: NumOfZones

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
    REAL(r64),        PARAMETER :: StartTemp   = 100.0d0   ! Standard Temperature across code to calculated Steam density
    CHARACTER(len=*), PARAMETER :: RoutineName = 'GetPumpInput: '

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER           :: PumpNum
    INTEGER           :: NumAlphas ! Number of elements in the alpha array
    INTEGER           :: NumNums   ! Number of elements in the numeric array
    INTEGER           :: IOStat    ! IO Status when calling get input subroutine
    LOGICAL           :: ErrorsFound
    LOGICAL           :: IsNotOK               ! Flag to verify name
    LOGICAL           :: IsBlank               ! Flag for blank name
    INTEGER           :: TempCurveIndex
    CHARACTER(len=32) :: TempCurveType
    INTEGER           :: NumVarSpeedPumps
    INTEGER           :: NumConstSpeedPumps
    INTEGER           :: NumCondensatePumps
    INTEGER           :: NumVarPump
    INTEGER           :: NumConstPump
    INTEGER           :: NumCondPump
    INTEGER           :: NumPumpBankSimpleVar
    INTEGER           :: NumPumpBankSimpleConst
    INTEGER           :: NumVarPumpBankSimple
    INTEGER           :: NumConstPumpBankSimple
    REAL(r64)         :: SteamDensity
    REAL(r64)         :: TempWaterDensity
    INTEGER           :: DummyWaterIndex =1

  ErrorsFound = .FALSE.

            !GET NUMBER OF ALL EQUIPMENT TYPES
  NumVarSpeedPumps      = GetNumObjectsFound(cPump_VarSpeed)
  NumConstSpeedPumps    = GetNumObjectsFound(cPump_ConSpeed)
  NumCondensatePumps    = GetNumObjectsFound(cPump_Cond)
  NumPumpBankSimpleVar  = GetNumObjectsFound(cPumpBank_VarSpeed)
  NumPumpBankSimpleConst= GetNumObjectsFound(cPumpBank_ConSpeed)
  NumPumps = NumVarSpeedPumps + NumConstSpeedPumps + NumCondensatePumps + NumPumpBankSimpleVar + NumPumpBankSimpleConst

  IF(NumPumps<=0)THEN
    CALL ShowWarningError('No Pumping Equipment Found')
    RETURN
  END IF

  ALLOCATE (PumpEquip(NumPumps))
  ALLOCATE (PumpEquipReport(NumPumps))

             !LOAD ARRAYS WITH VARIABLE SPEED CURVE FIT PUMP DATA

  cCurrentModuleObject = cPump_VarSpeed

  DO NumVarPump = 1 , NumVarSpeedPumps
    PumpNum = NumVarPump
    CALL GetObjectItem(cCurrentModuleObject,NumVarPump,cAlphaArgs,NumAlphas, &
                       rNumericArgs,NumNums,IOSTAT, &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),PumpEquip%Name,PumpNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    PumpEquip(PumpNum)%Name                = cAlphaArgs(1)
    PumpEquip(PumpNum)%PumpType            = Pump_VarSpeed    !'Pump:VariableSpeed'
    PumpEquip(PumpNum)%TypeOf_Num          = TypeOf_PumpVariableSpeed

    PumpEquip(PumpNum)%InletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)

    PumpEquip(PumpNum)%OutletNodeNum       = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(2),cAlphaArgs(3),'Water Nodes')

!    PumpEquip(PumpNum)%PumpControlType = cAlphaArgs(4)
    IF (SameString(cAlphaArgs(4),'Continuous')) THEN
      PumpEquip(PumpNum)%PumpControl = Continuous
    ELSE IF (SameString(cAlphaArgs(4),'Intermittent')) THEN
      PumpEquip(PumpNum)%PumpControl = Intermittent
    ELSE
      CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(PumpEquip(PumpNum)%Name)//  &
           '", Invalid '//trim(cAlphaFieldNames(4)))
      CALL ShowContinueError('Entered Value=['//trim(cAlphaArgs(4))//']. '//  &
         trim(cAlphaFieldNames(4))//' has been set to Continuous for this pump.')
      PumpEquip(PumpNum)%PumpControl = Continuous
    END IF

    ! Input the optional schedule for the pump
    PumpEquip(PumpNum)%PumpSchedule = cAlphaArgs(5)
    PumpEquip(PumpNum)%PumpScheduleIndex =GetScheduleIndex(cAlphaArgs(5))
    IF (.NOT. lAlphaFieldBlanks(5) .AND. .NOT. PumpEquip(PumpNum)%PumpScheduleIndex > 0) THEN
      CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(PumpEquip(PumpNum)%Name)//  &
           '", Invalid '//trim(cAlphaFieldNames(5)))
      CALL ShowContinueError('Schedule named =['//trim(cAlphaArgs(5))//']. '//  &
         ' was not found and will not be used.')
    ENDIF

    PumpEquip(PumpNum)%NomVolFlowRate      = rNumericArgs(1)
    PumpEquip(PumpNum)%NomPumpHead         = rNumericArgs(2)
    PumpEquip(PumpNum)%NomPowerUse         = rNumericArgs(3)
    PumpEquip(PumpNum)%MotorEffic          = rNumericArgs(4)
    PumpEquip(PumpNum)%FracMotorLossToFluid= rNumericArgs(5)
    PumpEquip(PumpNum)%PartLoadCoef(1)     = rNumericArgs(6)
    PumpEquip(PumpNum)%PartLoadCoef(2)     = rNumericArgs(7)
    PumpEquip(PumpNum)%PartLoadCoef(3)     = rNumericArgs(8)
    PumpEquip(PumpNum)%PartLoadCoef(4)     = rNumericArgs(9)
    PumpEquip(PumpNum)%MinVolFlowRate      = rNumericArgs(10)
!Probably the following two lines will be used if the team agrees on changing the F10 value from min flow rate to
!minimum flow as a fraction of nominal flow.
!    PumpEquip(PumpNum)%MinVolFlowRateFrac  = rNumericArgs(10)
!    PumpEquip(PumpNum)%MinVolFlowRate      = PumpEquip(PumpNum)%NomVolFlowRate * PumpEquip(PumpNum)%MinVolFlowRateFrac

   ! Input pressure related data such as pressure curve and impeller size/rotational speed
    PumpEquip(PumpNum)%PressureCurve_Name = cAlphaArgs(6)
    IF (TRIM(PumpEquip(PumpNum)%PressureCurve_Name) .EQ. '') THEN
      PumpEquip(PumpNum)%PressureCurve_Index = -1
    ELSE
      TempCurveIndex = GetCurveIndex(PumpEquip(PumpNum)%PressureCurve_Name)
      IF (TempCurveIndex .EQ. 0) THEN
        PumpEquip(PumpNum)%PressureCurve_Index = -1
      ELSE
        TempCurveType = GetCurveType(TempCurveIndex)
        SELECT CASE (TRIM(TempCurveType))
          CASE ('LINEAR','QUADRATIC','CUBIC','QUARTIC')
            PumpEquip(PumpNum)%PressureCurve_Index = TempCurveIndex
            CALL GetCurveMinMaxvalues(TempCurveIndex, PumpEquip(PumpNum)%MinPhiValue, PumpEquip(PumpNum)%MaxPhiValue)
          CASE DEFAULT
            ErrorsFound = .TRUE.
        END SELECT
      END IF
    END IF

    !read in the rest of the pump pressure characteristics
    PumpEquip(PumpNum)%ImpellerDiameter    = rNumericArgs(11)

    ! Input VFD related data
    IF (lAlphaFieldBlanks(7)) THEN
      PumpEquip(PumpNum)%HasVFD=.False.
    ELSE
      PumpEquip(PumpNum)%HasVFD=.True.
      IF (trim(cAlphaArgs(7)) .EQ. 'MANUALCONTROL') THEN
        PumpEquip(PumpNum)%VFD%VFDControlType = VFDManual
        PumpEquip(PumpNum)%VFD%ManualRPMSchedName  = cAlphaArgs(8)
        PumpEquip(PumpNum)%VFD%ManualRPMSchedIndex = GetScheduleIndex(cAlphaArgs(8))
        IF (PumpEquip(PumpNum)%VFD%ManualRPMSchedIndex <= 0) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(PumpEquip(PumpNum)%Name)//'", '// &
                                 'At least one scheduled VFD schedule input was invalid.')
            CALL ShowContinueError('Verify that all of the pressure and rpm schedules referenced '//  &
               'in the input fields actually exist.')
            ErrorsFound = .TRUE.
        ELSEIF ( .not. CheckScheduleValueMinMax(PumpEquip(PumpNum)%VFD%ManualRPMSchedIndex, '>', 0.0d0) .OR. &
                 .not. CheckScheduleValueMinMax(PumpEquip(PumpNum)%VFD%ManualRPMSchedIndex, '>', 0.0d0) ) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(PumpEquip(PumpNum)%Name)//'", '// &
                                 'A pump rpm schedule had zero value.  Ensure all entries in the schedule are greater than zero.')
            ErrorsFound = .TRUE.
        END IF
      ELSE IF (trim(cAlphaArgs(7)) .EQ. 'PRESSURESETPOINTCONTROL') THEN
        PumpEquip(PumpNum)%VFD%VFDControlType = VFDAutomatic
        PumpEquip(PumpNum)%VFD%LowerPsetSchedName = cAlphaArgs(9)
        PumpEquip(PumpNum)%VFD%LowerPsetSchedIndex= GetScheduleIndex(cAlphaArgs(9))
        PumpEquip(PumpNum)%VFD%UpperPsetSchedName = cAlphaArgs(10)
        PumpEquip(PumpNum)%VFD%UpperPsetSchedIndex= GetScheduleIndex(cAlphaArgs(10))
        PumpEquip(PumpNum)%VFD%MinRPMSchedName    = cAlphaArgs(11)
        PumpEquip(PumpNum)%VFD%MinRPMSchedIndex   = GetScheduleIndex(cAlphaArgs(11))
        PumpEquip(PumpNum)%VFD%MaxRPMSchedName    = cAlphaArgs(12)
        PumpEquip(PumpNum)%VFD%MaxRPMSchedIndex   = GetScheduleIndex(cAlphaArgs(12))
        IF (ANY((/PumpEquip(PumpNum)%VFD%LowerPsetSchedIndex, PumpEquip(PumpNum)%VFD%UpperPsetSchedIndex, &
                 PumpEquip(PumpNum)%VFD%MinRPMSchedIndex, PumpEquip(PumpNum)%VFD%MaxRPMSchedIndex/) <= 0)) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(PumpEquip(PumpNum)%Name)//'", '// &
                                 'At least one scheduled VFD schedule input was invalid.')
            CALL ShowContinueError('Verify that all of the pressure and rpm schedules referenced '//  &
               'in the input fields actually exist.')
            ErrorsFound = .TRUE.
        ELSEIF ( .not. CheckScheduleValueMinMax(PumpEquip(PumpNum)%VFD%MinRPMSchedIndex, '>', 0.0d0) .OR. &
                 .not. CheckScheduleValueMinMax(PumpEquip(PumpNum)%VFD%MaxRPMSchedIndex, '>', 0.0d0) ) THEN
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(PumpEquip(PumpNum)%Name)//'", '// &
                                 'A pump rpm schedule had zero value.  Ensure all entries in the schedule are greater than zero.')
            ErrorsFound = .TRUE.
        END IF
      ELSE
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(PumpEquip(PumpNum)%Name)//'", '// &
                             'VFD Control type entered is invalid.  Use one of the key choice entries.')
        ErrorsFound = .TRUE.
      END IF
    END IF

    IF (.NOT. lAlphaFieldBlanks(13)) THEN ! zone named for pump skin losses
      PumpEquip(PumpNum)%ZoneNum=FindItemInList(cAlphaArgs(13),Zone%Name,NumOfZones)
      IF (PumpEquip(PumpNum)%ZoneNum > 0) THEN
        PumpEquip(PumpNum)%HeatLossesToZone = .TRUE.
        IF (.NOT. lNumericFieldBlanks(12) ) THEN
          PumpEquip(PumpNum)%SkinLossRadFraction = rNumericArgs(12)
        ENDIF
      ELSE
        CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
           trim(cAlphaFieldNames(13))//'="'//trim(cAlphaArgs(13))//'" not found.')
        ErrorsFound = .TRUE.
      ENDIF
    ENDIF

    ! Is this really necessary for each pump GetInput loop?
    PumpEquip(PumpNum)%Energy = 0.0d0
    PumpEquip(PumpNum)%Power = 0.0d0
  END DO

  cCurrentModuleObject = TRIM(cPump_ConSpeed)

  DO NumConstPump = 1, NumConstSpeedPumps
    PumpNum = NumVarSpeedPumps + NumConstPump
    CALL GetObjectItem(cCurrentModuleObject,NumConstPump,cAlphaArgs,NumAlphas, &
                       rNumericArgs,NumNums,IOSTAT,  &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),PumpEquip%Name,PumpNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    PumpEquip(PumpNum)%Name                = cAlphaArgs(1)
    PumpEquip(PumpNum)%PumpType            = Pump_ConSpeed   !'Pump:ConstantSpeed'
    PumpEquip(PumpNum)%TypeOf_Num          = TypeOf_PumpConstantSpeed

    PumpEquip(PumpNum)%InletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)

    PumpEquip(PumpNum)%OutletNodeNum       = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(2),cAlphaArgs(3),'Water Nodes')

    PumpEquip(PumpNum)%NomVolFlowRate      = rNumericArgs(1)
    PumpEquip(PumpNum)%NomPumpHead         = rNumericArgs(2)
    PumpEquip(PumpNum)%NomPowerUse         = rNumericArgs(3)
    PumpEquip(PumpNum)%MotorEffic          = rNumericArgs(4)
    PumpEquip(PumpNum)%FracMotorLossToFluid= rNumericArgs(5)
    PumpEquip(PumpNum)%PartLoadCoef(1)     = 1.0d0
    PumpEquip(PumpNum)%PartLoadCoef(2)     = 0.0d0
    PumpEquip(PumpNum)%PartLoadCoef(3)     = 0.0d0
    PumpEquip(PumpNum)%PartLoadCoef(4)     = 0.0d0
    !DSU In a constant volume pump we previously set the minimum to the nominal capacity
    !DSU Now we model the pump as constant speed and set flow by riding the pump curve.
    !DSU PumpEquip(PumpNum)%MinVolFlowRate      = rNumericArgs(1)
    PumpEquip(PumpNum)%MinVolFlowRate      = 0.0d0
    PumpEquip(PumpNum)%Energy = 0.0d0
    PumpEquip(PumpNum)%Power = 0.0d0

!    PumpEquip(PumpNum)%PumpControlType = cAlphaArgs(4)
    IF (SameString(cAlphaArgs(4),'Continuous')) THEN
      PumpEquip(PumpNum)%PumpControl = Continuous
    ELSE IF (SameString(cAlphaArgs(4),'Intermittent')) THEN
      PumpEquip(PumpNum)%PumpControl = Intermittent
    ELSE
      CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(PumpEquip(PumpNum)%Name)//  &
           '", Invalid '//trim(cAlphaFieldNames(4)))
      CALL ShowContinueError('Entered Value=['//trim(cAlphaArgs(4))//']. '//  &
         trim(cAlphaFieldNames(4))//' has been set to Continuous for this pump.')
      PumpEquip(PumpNum)%PumpControl = Continuous
    END IF

    ! Input the optional schedule for the pump
    PumpEquip(PumpNum)%PumpSchedule = cAlphaArgs(5)
    PumpEquip(PumpNum)%PumpScheduleIndex =GetScheduleIndex(cAlphaArgs(5))
    IF (.NOT. lAlphaFieldBlanks(5) .AND. .NOT. PumpEquip(PumpNum)%PumpScheduleIndex > 0) THEN
      CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(PumpEquip(PumpNum)%Name)//  &
           '", Invalid '//trim(cAlphaFieldNames(5)))
      CALL ShowContinueError('Schedule named =['//trim(cAlphaArgs(5))//']. '//  &
         ' was not found and will not be used.')
    ENDIF

    ! Input pressure related data such as pressure curve and impeller size/rotational speed
    PumpEquip(PumpNum)%PressureCurve_Name = cAlphaArgs(6)
    IF (TRIM(PumpEquip(PumpNum)%PressureCurve_Name) .EQ. '') THEN
      PumpEquip(PumpNum)%PressureCurve_Index = -1
    ELSE
      TempCurveIndex = GetCurveIndex(PumpEquip(PumpNum)%PressureCurve_Name)
      IF (TempCurveIndex .EQ. 0) THEN
        PumpEquip(PumpNum)%PressureCurve_Index = -1
      ELSE
        TempCurveType = GetCurveType(TempCurveIndex)
        SELECT CASE (TRIM(TempCurveType))
          CASE ('LINEAR','QUADRATIC','CUBIC','QUARTIC')
            PumpEquip(PumpNum)%PressureCurve_Index = TempCurveIndex
            CALL GetCurveMinMaxvalues(TempCurveIndex, PumpEquip(PumpNum)%MinPhiValue, PumpEquip(PumpNum)%MaxPhiValue)
          CASE DEFAULT
            ErrorsFound = .TRUE.
        END SELECT
      END IF
    END IF

    !read in the rest of the pump pressure characteristics
    PumpEquip(PumpNum)%ImpellerDiameter    = rNumericArgs(6)
    PumpEquip(PumpNum)%RotSpeed_RPM        = rNumericArgs(7) ! retrieve the input rotational speed, in revs/min
    PumpEquip(PumPNum)%RotSpeed            = PumpEquip(PumpNum)%RotSpeed_RPM/60.0d0 !convert input[rpm] to calculation units[rps]

    IF (.NOT. lAlphaFieldBlanks(7)) THEN ! zone named for pump skin losses
      PumpEquip(PumpNum)%ZoneNum=FindItemInList(cAlphaArgs(7),Zone%Name,NumOfZones)
      IF (PumpEquip(PumpNum)%ZoneNum > 0) THEN
        PumpEquip(PumpNum)%HeatLossesToZone = .TRUE.
        IF (.NOT. lNumericFieldBlanks(8) ) THEN
          PumpEquip(PumpNum)%SkinLossRadFraction = rNumericArgs(8)
        ENDIF
      ELSE
        CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
           trim(cAlphaFieldNames(7))//'="'//trim(cAlphaArgs(7))//'" not found.')
        ErrorsFound = .TRUE.
      ENDIF
    ENDIF

  END DO

        ! pumps for steam system pumping condensate
  cCurrentModuleObject = cPump_Cond
  DO NumCondPump = 1 , NumCondensatePumps
    PumpNum = NumCondPump + NumVarSpeedPumps + NumConstSpeedPumps
    CALL GetObjectItem(cCurrentModuleObject ,NumCondPump,cAlphaArgs,NumAlphas, &
                       rNumericArgs,NumNums,IOSTAT,  &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),PumpEquip%Name,PumpNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//'  Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    PumpEquip(PumpNum)%Name                = cAlphaArgs(1)
    PumpEquip(PumpNum)%PumpType            = Pump_Cond     !'Pump:VariableSpeed:Condensate'
    PumpEquip(PumpNum)%TypeOf_Num          = TypeOf_PumpCondensate

    PumpEquip(PumpNum)%InletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject) ,cAlphaArgs(1), &
               NodeType_Steam,NodeConnectionType_Inlet, 1, ObjectIsNotParent)

    PumpEquip(PumpNum)%OutletNodeNum       = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject) ,cAlphaArgs(1), &
               NodeType_Steam,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject) ,cAlphaArgs(1),cAlphaArgs(2),cAlphaArgs(3),'Water Nodes')

    ! PumpEquip(PumpNum)%PumpControlType == 'Intermittent'
    PumpEquip(PumpNum)%PumpControl = Intermittent

    ! Input the optional schedule for the pump
    PumpEquip(PumpNum)%PumpSchedule = cAlphaArgs(4)
    PumpEquip(PumpNum)%PumpScheduleIndex =GetScheduleIndex(cAlphaArgs(4))
    IF (.NOT. lAlphaFieldBlanks(4) .AND. .NOT. PumpEquip(PumpNum)%PumpScheduleIndex > 0) THEN
      CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(PumpEquip(PumpNum)%Name)//  &
           '", Invalid '//trim(cAlphaFieldNames(4)))
      CALL ShowContinueError('Schedule named =['//trim(cAlphaArgs(4))//']. '//  &
         ' was not found and will not be used.')
    ENDIF


    PumpEquip(PumpNum)%NomSteamVolFlowRate = rNumericArgs(1)
    PumpEquip(PumpNum)%NomPumpHead         = rNumericArgs(2)
    PumpEquip(PumpNum)%NomPowerUse         = rNumericArgs(3)
    PumpEquip(PumpNum)%MotorEffic          = rNumericArgs(4)
    PumpEquip(PumpNum)%FracMotorLossToFluid= rNumericArgs(5)
    PumpEquip(PumpNum)%PartLoadCoef(1)     = rNumericArgs(6)
    PumpEquip(PumpNum)%PartLoadCoef(2)     = rNumericArgs(7)
    PumpEquip(PumpNum)%PartLoadCoef(3)     = rNumericArgs(8)
    PumpEquip(PumpNum)%PartLoadCoef(4)     = rNumericArgs(9)

    IF (.NOT. lAlphaFieldBlanks(5)) THEN ! zone named for pump skin losses
      PumpEquip(PumpNum)%ZoneNum=FindItemInList(cAlphaArgs(5),Zone%Name,NumOfZones)
      IF (PumpEquip(PumpNum)%ZoneNum > 0) THEN
        PumpEquip(PumpNum)%HeatLossesToZone = .TRUE.
        IF (.NOT. lNumericFieldBlanks(10) ) THEN
          PumpEquip(PumpNum)%SkinLossRadFraction = rNumericArgs(10)
        ENDIF
      ELSE
        CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
           trim(cAlphaFieldNames(5))//'="'//trim(cAlphaArgs(5))//'" not found.')
        ErrorsFound = .TRUE.
      ENDIF
    ENDIF

    PumpEquip(PumpNum)%MinVolFlowRate      = 0.0d0
    PumpEquip(PumpNum)%Energy = 0.0d0
    PumpEquip(PumpNum)%Power = 0.0d0

    IF (PumpEquip(PumpNum)%NomSteamVolFlowRate == Autosize)THEN
       PumpEquip(PumpNum)%NomVolFlowRate=Autosize
    ELSE
        ! Calc Condensate Pump Water Volume Flow Rate
      SteamDensity=GetSatDensityRefrig('STEAM',StartTemp,1.0d0,PumpEquip(PumpNum)%FluidIndex,'GetPumpInput')
      TempWaterDensity = GetDensityGlycol('WATER', InitConvTemp, DummyWaterIndex, RoutineName)
      PumpEquip(PumpNum)%NomVolFlowRate= (PumpEquip(PumpNum)%NomSteamVolFlowRate*SteamDensity)/TempWaterDensity
    ENDIF
  END DO

             !LOAD Variable Speed Pump Bank ARRAYS WITH VARIABLE SPEED CURVE FIT PUMP DATA
  cCurrentModuleObject = cPumpBank_VarSpeed
  DO NumVarPumpBankSimple = 1 , NumPumpBankSimpleVar
    PumpNum = NumVarPumpBankSimple + NumVarSpeedPumps + NumConstSpeedPumps + NumCondensatePumps
    CALL GetObjectItem(cCurrentModuleObject,NumVarPumpBankSimple,cAlphaArgs,NumAlphas, &
                       rNumericArgs,NumNums,IOSTAT,  &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),PumpEquip%Name,PumpNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    PumpEquip(PumpNum)%Name                = cAlphaArgs(1)
    PumpEquip(PumpNum)%PumpType            = PumpBank_VarSpeed     !'HeaderedPumps:VariableSpeed'
    PumpEquip(PumpNum)%TypeOf_Num          = TypeOf_PumpBankVariableSpeed

    PumpEquip(PumpNum)%InletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)

    PumpEquip(PumpNum)%OutletNodeNum       = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(2),cAlphaArgs(3),'Water Nodes')

!    PumpEquip(PumpNum)%PumpBankFlowSeqControl = cAlphaArgs(4)
    IF (SameString(cAlphaArgs(4),'Optimal')) THEN
      PumpEquip(PumpNum)%SequencingScheme = OptimalScheme
    ELSE IF (SameString(cAlphaArgs(4),'Sequential')) THEN
      PumpEquip(PumpNum)%SequencingScheme = SequentialScheme
    ELSE IF (SameString(cAlphaArgs(4),'SupplyEquipmentAssigned')) THEN
      PumpEquip(PumpNum)%SequencingScheme = UserDefined
    ELSE
      CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(PumpEquip(PumpNum)%Name)//  &
           '", Invalid '//trim(cAlphaFieldNames(4)))
      CALL ShowContinueError('Entered Value=['//trim(cAlphaArgs(4))//']. '//  &
         trim(cAlphaFieldNames(4))//' has been set to Sequential for this pump.')
      PumpEquip(PumpNum)%SequencingScheme = SequentialScheme
    END IF

!    PumpEquip(PumpNum)%PumpControlType = cAlphaArgs(5)
    IF (SameString(cAlphaArgs(5),'Continuous')) THEN
      PumpEquip(PumpNum)%PumpControl = Continuous
    ELSE IF (SameString(cAlphaArgs(5),'Intermittent')) THEN
      PumpEquip(PumpNum)%PumpControl = Intermittent
    ELSE
      CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(PumpEquip(PumpNum)%Name)//  &
           '", Invalid '//trim(cAlphaFieldNames(5)))
      CALL ShowContinueError('Entered Value=['//trim(cAlphaArgs(5))//']. '//  &
         trim(cAlphaFieldNames(5))//' has been set to Continuous for this pump.')
      PumpEquip(PumpNum)%PumpControl = Continuous
    END IF

    ! Input the optional schedule for the pump
    PumpEquip(PumpNum)%PumpSchedule = cAlphaArgs(6)
    PumpEquip(PumpNum)%PumpScheduleIndex =GetScheduleIndex(cAlphaArgs(6))
    IF (.NOT. lAlphaFieldBlanks(6) .AND. .NOT. PumpEquip(PumpNum)%PumpScheduleIndex > 0) THEN
      CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(PumpEquip(PumpNum)%Name)//  &
           '", Invalid '//trim(cAlphaFieldNames(6)))
      CALL ShowContinueError('Schedule named =['//trim(cAlphaArgs(6))//']. '//  &
         ' was not found and will not be used.')
    ENDIF

    PumpEquip(PumpNum)%NomVolFlowRate      = rNumericArgs(1)
    PumpEquip(PumpNum)%NumPumpsInBank      = rNumericArgs(2)
    PumpEquip(PumpNum)%NomPumpHead         = rNumericArgs(3)
    PumpEquip(PumpNum)%NomPowerUse         = rNumericArgs(4)
    PumpEquip(PumpNum)%MotorEffic          = rNumericArgs(5)
    PumpEquip(PumpNum)%FracMotorLossToFluid= rNumericArgs(6)
    PumpEquip(PumpNum)%PartLoadCoef(1)     = rNumericArgs(7)
    PumpEquip(PumpNum)%PartLoadCoef(2)     = rNumericArgs(8)
    PumpEquip(PumpNum)%PartLoadCoef(3)     = rNumericArgs(9)
    PumpEquip(PumpNum)%PartLoadCoef(4)     = rNumericArgs(10)
    PumpEquip(PumpNum)%MinVolFlowRateFrac  = rNumericArgs(11)
    PumpEquip(PumpNum)%MinVolFlowRate      = PumpEquip(PumpNum)%NomVolFlowRate * PumpEquip(PumpNum)%MinVolFlowRateFrac

    IF (.NOT. lAlphaFieldBlanks(7)) THEN ! zone named for pump skin losses
      PumpEquip(PumpNum)%ZoneNum=FindItemInList(cAlphaArgs(7),Zone%Name,NumOfZones)
      IF (PumpEquip(PumpNum)%ZoneNum > 0) THEN
        PumpEquip(PumpNum)%HeatLossesToZone = .TRUE.
        IF (.NOT. lNumericFieldBlanks(12) ) THEN
          PumpEquip(PumpNum)%SkinLossRadFraction = rNumericArgs(12)
        ENDIF
      ELSE
        CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
           trim(cAlphaFieldNames(7))//'="'//trim(cAlphaArgs(7))//'" not found.')
        ErrorsFound = .TRUE.
      ENDIF
    ENDIF

    PumpEquip(PumpNum)%Energy = 0.0d0
    PumpEquip(PumpNum)%Power = 0.0d0
  END DO

  cCurrentModuleObject = TRIM(cPumpBank_ConSpeed)
  DO NumConstPumpBankSimple = 1 , NumPumpBankSimpleConst
    PumpNum = NumConstPumpBankSimple + NumVarSpeedPumps + NumConstSpeedPumps + NumCondensatePumps + NumPumpBankSimpleVar
    CALL GetObjectItem(cCurrentModuleObject,NumConstPumpBankSimple,cAlphaArgs,NumAlphas, &
                       rNumericArgs,NumNums,IOSTAT,  &
                       NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),PumpEquip%Name,PumpNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    PumpEquip(PumpNum)%Name                = cAlphaArgs(1)
    PumpEquip(PumpNum)%PumpType            = PumpBank_ConSpeed        !'HeaderedPumps:ConstantSpeed'
    PumpEquip(PumpNum)%TypeOf_Num          = TypeOf_PumpBankConstantSpeed

    PumpEquip(PumpNum)%InletNodeNum    = &
               GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Inlet, 1, ObjectIsNotParent)

    PumpEquip(PumpNum)%OutletNodeNum       = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Water,NodeConnectionType_Outlet, 1, ObjectIsNotParent)
    CALL TestCompSet(TRIM(cCurrentModuleObject),cAlphaArgs(1),cAlphaArgs(2),cAlphaArgs(3),'Water Nodes')

!    PumpEquip(PumpNum)%PumpBankFlowSeqControl = cAlphaArgs(4)
    IF (SameString(cAlphaArgs(4),'Optimal')) THEN
      PumpEquip(PumpNum)%SequencingScheme = OptimalScheme
    ELSE IF (SameString(cAlphaArgs(4),'Sequential')) THEN
      PumpEquip(PumpNum)%SequencingScheme = SequentialScheme
    ELSE
      CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(PumpEquip(PumpNum)%Name)//  &
           '", Invalid '//trim(cAlphaFieldNames(4)))
      CALL ShowContinueError('Entered Value=['//trim(cAlphaArgs(4))//']. '//  &
         trim(cAlphaFieldNames(4))//' has been set to Sequential for this pump.')
      PumpEquip(PumpNum)%SequencingScheme = SequentialScheme
!      PumpEquip(PumpNum)%PumpBankFlowSeqControl = 'Optimal'
    END IF

!    PumpEquip(PumpNum)%PumpControlType = cAlphaArgs(5)
    IF (SameString(cAlphaArgs(5),'Continuous')) THEN
      PumpEquip(PumpNum)%PumpControl = Continuous
    ELSE IF (SameString(cAlphaArgs(5),'Intermittent')) THEN
      PumpEquip(PumpNum)%PumpControl = Intermittent
    ELSE
      CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(PumpEquip(PumpNum)%Name)//  &
           '", Invalid '//trim(cAlphaFieldNames(5)))
      CALL ShowContinueError('Entered Value=['//trim(cAlphaArgs(5))//']. '//  &
         trim(cAlphaFieldNames(5))//' has been set to Continuous for this pump.')
      PumpEquip(PumpNum)%PumpControl = Continuous
    END IF

    ! Input the optional schedule for the pump
    PumpEquip(PumpNum)%PumpSchedule = cAlphaArgs(6)
    PumpEquip(PumpNum)%PumpScheduleIndex =GetScheduleIndex(cAlphaArgs(6))
    IF (.NOT. lAlphaFieldBlanks(6) .AND. .NOT. PumpEquip(PumpNum)%PumpScheduleIndex > 0) THEN
      CALL ShowWarningError(RoutineName//trim(cCurrentModuleObject)//'="'//TRIM(PumpEquip(PumpNum)%Name)//  &
           '", Invalid '//trim(cAlphaFieldNames(6)))
      CALL ShowContinueError('Schedule named =['//trim(cAlphaArgs(6))//']. '//  &
         ' was not found and will not be used.')
    ENDIF

    PumpEquip(PumpNum)%NomVolFlowRate      = rNumericArgs(1)
    PumpEquip(PumpNum)%NumPumpsInBank      = rNumericArgs(2)
    PumpEquip(PumpNum)%NomPumpHead         = rNumericArgs(3)
    PumpEquip(PumpNum)%NomPowerUse         = rNumericArgs(4)
    PumpEquip(PumpNum)%MotorEffic          = rNumericArgs(5)
    PumpEquip(PumpNum)%FracMotorLossToFluid= rNumericArgs(6)
    PumpEquip(PumpNum)%PartLoadCoef(1)     = 1.0d0
    PumpEquip(PumpNum)%PartLoadCoef(2)     = 0.0d0
    PumpEquip(PumpNum)%PartLoadCoef(3)     = 0.0d0
    PumpEquip(PumpNum)%PartLoadCoef(4)     = 0.0d0
!    PumpEquip(PumpNum)%MinVolFlowRateFrac  = rNumericArgs(11)
!    PumpEquip(PumpNum)%MinVolFlowRate      = PumpEquip(PumpNum)%NomVolFlowRate * PumpEquip(PumpNum)%MinVolFlowRateFrac
   !DSU?  need a value set for %MinVolFlowRate ?? zero? NomVolFlowRate?

    IF (.NOT. lAlphaFieldBlanks(7)) THEN ! zone named for pump skin losses
      PumpEquip(PumpNum)%ZoneNum=FindItemInList(cAlphaArgs(7),Zone%Name,NumOfZones)
      IF (PumpEquip(PumpNum)%ZoneNum > 0) THEN
        PumpEquip(PumpNum)%HeatLossesToZone = .TRUE.
        IF (.NOT. lNumericFieldBlanks(7) ) THEN
          PumpEquip(PumpNum)%SkinLossRadFraction = rNumericArgs(7)
        ENDIF
      ELSE
        CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'" invalid '//  &
           trim(cAlphaFieldNames(7))//'="'//trim(cAlphaArgs(7))//'" not found.')
        ErrorsFound = .TRUE.
      ENDIF
    ENDIF

    PumpEquip(PumpNum)%MinVolFlowRate      = 0.0d0
    PumpEquip(PumpNum)%Energy = 0.0d0
    PumpEquip(PumpNum)%Power = 0.0d0
  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in getting Pump input')
  ENDIF

  DO PumpNum = 1 , NumPumps    !CurrentModuleObject='Pumps'
   If(PumpEquip(PumpNum)%PumpType == Pump_VarSpeed .OR. &
      PumpEquip(PumpNum)%PumpType == Pump_ConSpeed .OR. &
      PumpEquip(PumpNum)%PumpType == Pump_Cond) THEN

    CALL SetupOutputVariable('Pump Electric Energy [J]',PumpEquip(PumpNum)%Energy, &
                             'System','Sum',PumpEquip(PumpNum)%Name,  &
                              ResourceTypeKey='Electric',EndUseKey='Pumps',GroupKey='Plant')
    CALL SetupOutputVariable('Pump Electric Power [W]',PumpEquip(PumpNum)%Power, &
                             'System','Average',PumpEquip(PumpNum)%Name)
    CALL SetupOutputVariable('Pump Shaft Power [W]', &
          PumpEquipReport(PumpNum)%ShaftPower,'System','Average',PumpEquip(PumpNum)%Name)
    CALL SetupOutputVariable('Pump Fluid Heat Gain Rate [W]', &
          PumpEquipReport(PumpNum)%PumpHeattoFluid,'System','Average',PumpEquip(PumpNum)%Name)
    CALL SetupOutputVariable('Pump Fluid Heat Gain Energy [J]', &
          PumpEquipReport(PumpNum)%PumpHeattoFluidEnergy,'System','Sum',PumpEquip(PumpNum)%Name)
    CALL SetupOutputVariable('Pump Outlet Temperature [C]', &
          PumpEquipReport(PumpNum)%OutletTemp,'System','Average',PumpEquip(PumpNum)%Name)
    CALL SetupOutputVariable('Pump Mass Flow Rate [kg/s]', &
          PumpEquipReport(PumpNum)%PumpMassFlowRate,'System','Average',PumpEquip(PumpNum)%Name)

   End If
   If(PumpEquip(PumpNum)%PumpType == PumpBank_VarSpeed .OR. &   ! CurrentModuleObject='HeaderedPumps'
      PumpEquip(PumpNum)%PumpType == PumpBank_ConSpeed) THEN

    CALL SetupOutputVariable('Pump Electric Energy [J]',PumpEquip(PumpNum)%Energy, &
                             'System','Sum',PumpEquip(PumpNum)%Name,  &
                              ResourceTypeKey='Electric',EndUseKey='Pumps',GroupKey='Plant')
    CALL SetupOutputVariable('Pump Electric Power [W]',PumpEquip(PumpNum)%Power, &
                             'System','Average',PumpEquip(PumpNum)%Name)
    CALL SetupOutputVariable('Pump Shaft Power [W]', &
          PumpEquipReport(PumpNum)%ShaftPower,'System','Average',PumpEquip(PumpNum)%Name)
    CALL SetupOutputVariable('Pump Fluid Heat Gain Rate [W]', &
          PumpEquipReport(PumpNum)%PumpHeattoFluid,'System','Average',PumpEquip(PumpNum)%Name)
    CALL SetupOutputVariable('Pump Fluid Heat Gain Energy [J]', &
          PumpEquipReport(PumpNum)%PumpHeattoFluidEnergy,'System','Sum',PumpEquip(PumpNum)%Name)
    CALL SetupOutputVariable('Pump Outlet Temperature [C]', &
          PumpEquipReport(PumpNum)%OutletTemp,'System','Average',PumpEquip(PumpNum)%Name)
    CALL SetupOutputVariable('Pump Mass Flow Rate [kg/s]', &
          PumpEquipReport(PumpNum)%PumpMassFlowRate,'System','Average',PumpEquip(PumpNum)%Name)
    CALL SetupOutputVariable('Pump Operating Pumps Count []', &
          PumpEquipReport(PumpNum)%NumPumpsOperating,'System','Average',PumpEquip(PumpNum)%Name)
   End If

   IF (AnyEnergyManagementSystemInModel) THEN
     CALL SetupEMSInternalVariable('Pump Maximum Mass Flow Rate', PumpEquip(PumpNum)%Name, '[kg/s]', &
                                  PumpEquip(PumpNum)%MassFlowRateMax  )
     CALL SetupEMSActuator('Pump', PumpEquip(PumpNum)%Name, 'Pump Mass Flow Rate' , '[kg/s]', &
                                  PumpEquip(PumpNum)%EMSMassFlowOverrideOn, PumpEquip(PumpNum)%EMSMassFlowValue )
     CALL SetupEMSActuator('Pump',  PumpEquip(PumpNum)%Name, 'Pump Pressure Rise', '[Pa]', &
                                  PumpEquip(PumpNum)%EMSPressureOverrideOn, PumpEquip(PumpNum)%EMSPressureOverrideValue )
   ENDIF

   IF (PumpEquip(PumpNum)%HeatLossesToZone) THEN
     ! setup skin loss output vars
     CALL SetupOutputVariable('Pump Zone Total Heating Rate [W]', &
            PumpEquipReport(PumpNum)%ZoneTotalGainRate,'System','Average',PumpEquip(PumpNum)%Name)
     CALL SetupOutputVariable('Pump Zone Total Heating Energy [J]', &
            PumpEquipReport(PumpNum)%ZoneTotalGainEnergy,'System','Sum',PumpEquip(PumpNum)%Name)
     CALL SetupOutputVariable('Pump Zone Convective Heating Rate [W]', &
            PumpEquipReport(PumpNum)%ZoneConvGainRate,'System','Average',PumpEquip(PumpNum)%Name)
     CALL SetupOutputVariable('Pump Zone Radiative Heating Rate [W]', &
            PumpEquipReport(PumpNum)%ZoneRadGainRate,'System','Average',PumpEquip(PumpNum)%Name)

     ! setup internal gains
     SELECT CASE (PumpEquip(PumpNum)%PumpType)
     CASE (Pump_VarSpeed)
       CALL SetupZoneInternalGain(PumpEquip(PumpNum)%ZoneNum, &
             'Pump:VariableSpeed', &
             PumpEquip(PumpNum)%Name, &
             IntGainTypeOf_Pump_VarSpeed, &
             ConvectionGainRate          = PumpEquipReport(PumpNum)%ZoneConvGainRate,&
             ThermalRadiationGainRate    = PumpEquipReport(PumpNum)%ZoneRadGainRate)
     CASE (Pump_ConSpeed)
       CALL SetupZoneInternalGain(PumpEquip(PumpNum)%ZoneNum, &
             'Pump:ConstantSpeed', &
             PumpEquip(PumpNum)%Name, &
             IntGainTypeOf_Pump_ConSpeed, &
             ConvectionGainRate          = PumpEquipReport(PumpNum)%ZoneConvGainRate,&
             ThermalRadiationGainRate    = PumpEquipReport(PumpNum)%ZoneRadGainRate)
     CASE (Pump_Cond)
       CALL SetupZoneInternalGain(PumpEquip(PumpNum)%ZoneNum, &
             'Pump:VariableSpeed:Condensate', &
             PumpEquip(PumpNum)%Name, &
             IntGainTypeOf_Pump_Cond, &
             ConvectionGainRate          = PumpEquipReport(PumpNum)%ZoneConvGainRate,&
             ThermalRadiationGainRate    = PumpEquipReport(PumpNum)%ZoneRadGainRate)
     CASE (PumpBank_VarSpeed)
       CALL SetupZoneInternalGain(PumpEquip(PumpNum)%ZoneNum, &
             'HeaderedPumps:VariableSpeed', &
             PumpEquip(PumpNum)%Name, &
             IntGainTypeOf_PumpBank_VarSpeed, &
             ConvectionGainRate          = PumpEquipReport(PumpNum)%ZoneConvGainRate,&
             ThermalRadiationGainRate    = PumpEquipReport(PumpNum)%ZoneRadGainRate)
     CASE (PumpBank_ConSpeed)
       CALL SetupZoneInternalGain(PumpEquip(PumpNum)%ZoneNum, &
             'HeaderedPumps:ConstantSpeed', &
             PumpEquip(PumpNum)%Name, &
             IntGainTypeOf_PumpBank_ConSpeed, &
             ConvectionGainRate          = PumpEquipReport(PumpNum)%ZoneConvGainRate,&
             ThermalRadiationGainRate    = PumpEquipReport(PumpNum)%ZoneRadGainRate)
     END SELECT

   ENDIF

  END DO


RETURN
END SUBROUTINE GetPumpInput
!*************************************************************************!

!*************************************************************************!
SUBROUTINE InitializePumps(PumpNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:        Edwin Lee
          !       DATE WRITTEN:  August 2010
          !       MODIFIED       Based on the INIT section of InitSimVars, credits here:
          !                        Author:
          !                          Oct 1998 Dan Fisher
          !                        Modifications:
          !                          Jul 2001 Richard Liesen
          !                          July 2001, Rick Strand (implemented new pump controls)
          !                          May 2009, Brent Griffith (added EMS override capability)
          !                          Nov 2010, Brent Griffith (call InitComponentNodes, generalize fluid props)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine does one-time and begin-envrn inits for the pump

          ! USE STATEMENTS:
  USE General,             ONLY: RoundSigDigits
  USE DataPlant,           ONLY: ScanPlantLoopsForObject, PlantLoop, LoopFlowStatus_NeedyAndTurnsLoopOn
  USE FluidProperties,     ONLY: GetSatDensityRefrig, GetDensityGlycol
  USE PlantUtilities,      ONLY: InitComponentNodes

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,   INTENT(IN)    :: PumpNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64),        PARAMETER :: StartTemp    = 100.0d0   ! Standard Temperature across code to calculated Steam density
  REAL(r64),        PARAMETER :: ZeroPowerTol = 0.0000001d0
  CHARACTER(len=*), PARAMETER :: RoutineName  = 'PlantPumps::InitializePumps '

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER        :: InletNode             !pump inlet node number
  INTEGER        :: OutletNode            !pump outlet node number
  REAL(r64)      :: TotalEffic
  REAL(r64)      :: SteamDensity          ! Density of working fluid
  INTEGER        :: DummyWaterIndex = 1
  REAL(r64)      :: TempWaterDensity
  LOGICAL        :: errFlag
  REAL(r64)      :: mdotMax ! local fluid mass flow rate maximum
  REAL(r64)      :: mdotMin ! local fluid mass flow rate minimum
  INTEGER        :: plloopnum
  INTEGER        :: lsnum
  INTEGER        :: brnum
  INTEGER        :: cpnum

  ! Set some variables for convenience
  InletNode         = PumpEquip(PumpNum)%InletNodeNum
  OutletNode        = PumpEquip(PumpNum)%OutletNodeNum

  ! One time inits
  IF (PumpEquip(PumpNum)%PumpOneTimeFlag) THEN

    errFlag=.false.
    CALL ScanPlantLoopsForObject(                                &
                                 PumpEquip(PumpNum)%Name,        &
                                 PumpEquip(PumpNum)%TypeOf_Num,  &
                                 PumpEquip(PumpNum)%LoopNum,     &
                                 PumpEquip(PumpNum)%LoopSideNum, &
                                 PumpEquip(PumpNum)%BranchNum,   &
                                 PumpEquip(PumpNum)%CompNum,     &
                                 errFlag=errFlag)
    plloopnum=PumpEquip(PumpNum)%LoopNum
    lsnum=PumpEquip(PumpNum)%LoopSideNum
    brnum=PumpEquip(PumpNum)%BranchNum
    cpnum=PumpEquip(PumpNum)%CompNum
    IF (plloopnum > 0 .and. lsnum > 0 .and. brnum > 0 .and. cpnum > 0) THEN
      IF (PlantLoop(plloopnum)%LoopSide(lsnum)%Branch(brnum)%Comp(cpnum)%NodeNumIn /= InletNode .or.  &
          PlantLoop(plloopnum)%LoopSide(lsnum)%Branch(brnum)%Comp(cpnum)%NodeNumOut /= OutletNode) THEN
        CALL ShowSevereError('InitializePumps: '//trim(cPumpTypes(PumpEquip(PumpNum)%PumpType))//'="'//  &
          trim(PumpEquip(PumpNum)%Name)//'", non-matching nodes.')
        CALL ShowContinueError('...in Branch="'//trim(PlantLoop(plloopnum)%LoopSide(lsnum)%Branch(brnum)%Name)//  &
           '", Component referenced with:')
        CALL ShowContinueError('...Inlet Node="'//  &
           trim(NodeID(PlantLoop(plloopnum)%LoopSide(lsnum)%Branch(brnum)%Comp(cpnum)%NodeNumIn)))
        CALL ShowContinueError('...Outlet Node="'//  &
           trim(NodeID(PlantLoop(plloopnum)%LoopSide(lsnum)%Branch(brnum)%Comp(cpnum)%NodeNumOut)))
        CALL ShowContinueError('...Pump Inlet Node="'//trim(NodeID(InletNode)))
        CALL ShowContinueError('...Pump Outlet Node="'//trim(NodeID(OutletNode)))
        errflag=.true.
      ENDIF
    ELSE ! CR9292
      CALL ShowSevereError('InitializePumps: '//trim(cPumpTypes(PumpEquip(PumpNum)%PumpType))//'="'//  &
          trim(PumpEquip(PumpNum)%Name)//'", component missing.')
      errflag=.true.  ! should have received warning/severe earlier, will reiterate
    ENDIF

    IF (errFlag) THEN
      CALL ShowFatalError('InitializePumps: Program terminated due to previous condition(s).')
    ENDIF
    PlantLoop(PumpEquip(PumpNum)%LoopNum)%LoopSide(PumpEquip(PumpNum)%LoopSideNum)&
       %Branch(PumpEquip(PumpNum)%BranchNum)%Comp(PumpEquip(PumpNum)%CompNum)%CompNum = PumpNum

    CALL SizePump(PumpNum)

    CALL PumpDataForTable(PumpNum)

          ! calculate the efficiency for each pump
          ! by calculating the efficiency for each pump being simulated.  The calculation
          ! is based on the PMPSIM code in the ASHRAE Secondary Toolkit
    IF (PumpEquip(PumpNum)%NomPowerUse > ZeroPowerTol .AND. PumpEquip(PumpNum)%MotorEffic > ZeroPowerTol)THEN
      TotalEffic =   PumpEquip(PumpNum)%NomVolFlowRate * PumpEquip(PumpNum)%NomPumpHead  &
                     / PumpEquip(PumpNum)%NomPowerUse
      PumpEquip(PumpNum)%PumpEffic =  TotalEffic / PumpEquip(PumpNum)%MotorEffic
      IF (PumpEquip(PumpNum)%PumpEffic < .50d0)THEN
        CALL ShowWarningError('Check input. Calculated Pump Efficiency='//  &
                              TRIM(RoundSigDigits(PumpEquip(PumpNum)%PumpEffic*100.0d0 ,2))// &
                              '% which is less than 50%, for pump='//TRIM(PumpEquip(PumpNum)%Name))
        CALL ShowContinueError('Calculated Pump_Efficiency % =Total_Efficiency % ['//TRIM(RoundSigDigits(TotalEffic*100.,1))//  &
                               '] / Motor_Efficiency % ['//TRIM(RoundSigDigits(PumpEquip(PumpNum)%MotorEffic*100.,1))//']')
        CALL ShowContinueError('Total_Efficiency % =(Rated_Volume_Flow_Rate ['//  &
                               TRIM(RoundSigDigits(PumpEquip(PumpNum)%NomVolFlowRate,1))//  &
                               '] * Rated_Pump_Head ['//TRIM(RoundSigDigits(PumpEquip(PumpNum)%NomPumpHead,1))//  &
                               '] / Rated_Power_Use ['//TRIM(RoundSigDigits(PumpEquip(PumpNum)%NomPowerUse,1))//']) * 100.')
      Else IF ((PumpEquip(PumpNum)%PumpEffic > 0.95d0) .and. (PumpEquip(PumpNum)%PumpEffic <= 1.0d0))THEN
        CALL ShowWarningError('Check input.  Calculated Pump Efficiency='// &
                              TRIM(RoundSigDigits(PumpEquip(PumpNum)%PumpEffic*100.0d0 ,2))//  &
                              '% is approaching 100%, for pump='//TRIM(PumpEquip(PumpNum)%Name))
        CALL ShowContinueError('Calculated Pump_Efficiency % =Total_Efficiency % ['//TRIM(RoundSigDigits(TotalEffic*100.,1))//  &
                               '] / Motor_Efficiency % ['//TRIM(RoundSigDigits(PumpEquip(PumpNum)%MotorEffic*100.,1))//']')
        CALL ShowContinueError('Total_Efficiency % =(Rated_Volume_Flow_Rate ['//  &
                               TRIM(RoundSigDigits(PumpEquip(PumpNum)%NomVolFlowRate,1))//  &
                               '] * Rated_Pump_Head ['//TRIM(RoundSigDigits(PumpEquip(PumpNum)%NomPumpHead,1))//  &
                               '] / Rated_Power_Use ['//TRIM(RoundSigDigits(PumpEquip(PumpNum)%NomPowerUse,1))//']) * 100.')
      Else IF (PumpEquip(PumpNum)%PumpEffic > 1.0d0)THEN
        CALL ShowSevereError('Check input.  Calculated Pump Efficiency='//  &
                              TRIM(RoundSigDigits(PumpEquip(PumpNum)%PumpEffic*100.0d0 ,3))//  &
                              '% which is bigger than 100%, for pump='//TRIM(PumpEquip(PumpNum)%Name))
        CALL ShowContinueError('Calculated Pump_Efficiency % =Total_Efficiency % ['//TRIM(RoundSigDigits(TotalEffic*100.,1))//  &
                               '] / Motor_Efficiency % ['//TRIM(RoundSigDigits(PumpEquip(PumpNum)%MotorEffic*100.,1))//']')
        CALL ShowContinueError('Total_Efficiency % =(Rated_Volume_Flow_Rate ['//  &
                               TRIM(RoundSigDigits(PumpEquip(PumpNum)%NomVolFlowRate,1))//  &
                               '] * Rated_Pump_Head ['//TRIM(RoundSigDigits(PumpEquip(PumpNum)%NomPumpHead,1))//  &
                               '] / Rated_Power_Use ['//TRIM(RoundSigDigits(PumpEquip(PumpNum)%NomPowerUse,1))//']) * 100.')
        CALL ShowFatalError('Errors found in Pump input')
      END IF
    ELSE
      CALL ShowWarningError('Check input. Pump nominal power or motor efficiency is set to 0, for pump='//  &
                            TRIM(PumpEquip(PumpNum)%Name))
    END IF

    IF (PumpEquip(PumpNum)%NomVolFlowRate <= SmallWaterVolFlow)THEN
      CALL ShowWarningError('Check input. Pump nominal flow rate is set or calculated = 0, for pump='//  &
                            TRIM(PumpEquip(PumpNum)%Name))
    End IF

    IF (PumpEquip(PumpNum)%PumpControl == Continuous) THEN
      ! reset flow priority appropriately (default was for Intermittent)
      PlantLoop(PumpEquip(PumpNum)%LoopNum)%LoopSide(PumpEquip(PumpNum)%LoopSideNum)%  &
       Branch(PumpEquip(PumpNum)%BranchNum)%Comp(PumpEquip(PumpNum)%CompNum)%FlowPriority = LoopFlowStatus_NeedyAndTurnsLoopOn
    ENDIF

    PumpEquip(PumpNum)%PumpOneTimeFlag = .FALSE.
  END IF

  ! Begin environment inits
  !DSU? Still need to clean this up and update condensate pump stuff -
  !     BG cleaned to call initComponentnodes, not sure what else may be needed if anything
  IF (PumpEquip(PumpNum)%PumpInitFlag .and. BeginEnvrnFlag) THEN
    IF (PumpEquip(PumpNum)%PumpType == Pump_Cond) THEN

      TempWaterDensity  = GetDensityGlycol('WATER', InitConvTemp, DummyWaterIndex, RoutineName)
      SteamDensity=GetSatDensityRefrig('STEAM',StartTemp,1.0d0,PumpEquip(PumpNum)%FluidIndex,RoutineName)
      PumpEquip(PumpNum)%NomVolFlowRate= (PumpEquip(PumpNum)%NomSteamVolFlowRate*SteamDensity)/TempWaterDensity

          !set the maximum flow rate on the outlet node
      mdotMax = PumpEquip(PumpNum)%NomSteamVolFlowRate * SteamDensity
      !mdotMin = PumpEquip(PumpNum)%MinVolFlowRate      * SteamDensity
      !On a pump the 'hardware min' (MassFlowRateMin) must be defined as zero and not
      !confused with the desired pump operating scheme or the user specified
      !'minimum flow rate'.  The user specified 'minimum flow rate' determines the minumum
      !flow rate under normal operating conditions.  For cases when 'MaxAvail' on the pump
      !inlet node actually less than the 'minimum flow rate' specified by the user, than a
      !loop shutdown must  be triggered.
      mdotMin = 0.0d0
      CALL InitComponentNodes(mdotMin, mdotMax, InletNode, OutletNode, &
                                PumpEquip(PumpNum)%LoopNum           , &
                                PumpEquip(PumpNum)%LoopSideNum, &
                                PumpEquip(PumpNum)%BranchNum, &
                                PumpEquip(PumpNum)%CompNum )
      PumpEquip(PumpNum)%MassFlowRateMax = mdotMax
      PumpEquip(PumpNum)%MassFlowRateMin = PumpEquip(PumpNum)%MinVolFlowRate * SteamDensity


    Else
      TempWaterDensity = GetDensityGlycol(PlantLoop(PumpEquip(PumpNum)%LoopNum)%FluidName, &
                                          InitConvTemp, &
                                          PlantLoop(PumpEquip(PumpNum)%LoopNum)%FluidIndex, RoutineName)
      mdotMax = PumpEquip(PumpNum)%NomVolFlowRate * TempWaterDensity
      !mdotMin = PumpEquip(PumpNum)%MinVolFlowRate * TempWaterDensity
      !see note above
      mdotMin = 0.0d0
      CALL InitComponentNodes(mdotMin, mdotMax, InletNode, OutletNode, &
                                PumpEquip(PumpNum)%LoopNum           , &
                                PumpEquip(PumpNum)%LoopSideNum, &
                                PumpEquip(PumpNum)%BranchNum, &
                                PumpEquip(PumpNum)%CompNum )
      PumpEquip(PumpNum)%MassFlowRateMax = mdotMax
      PumpEquip(PumpNum)%MassFlowRateMin = PumpEquip(PumpNum)%MinVolFlowRate * TempWaterDensity

    End If
      !zero out report variables
    PumpEquip(PumpNum)%Energy                = 0.d0
    PumpEquip(PumpNum)%Power                 = 0.d0
    PumpEquipReport(PumpNum)%ShaftPower      = 0.d0
    PumpEquipReport(PumpNum)%PumpHeattoFluid = 0.d0
    PumpEquipReport(PumpNum)%PumpHeattoFluidEnergy = 0.d0
    PumpEquipReport(PumpNum)%OutletTemp      = 0.d0
    PumpEquipReport(PumpNum)%PumpMassFlowRate = 0.d0
    PumpEquipReport(PumpNum)%NumPumpsOperating = 0
    PumpEquipReport(PumpNum)%ZoneTotalGainRate = 0.d0
    PumpEquipReport(PumpNum)%ZoneTotalGainEnergy = 0.d0
    PumpEquipReport(PumpNum)%ZoneConvGainRate = 0.d0
    PumpEquipReport(PumpNum)%ZoneRadGainRate  = 0.d0

    PumpEquip(PumpNum)%PumpInitFlag = .FALSE.

  END IF

  ! Reset the local environment flag for the next environment
  IF (.NOT. BeginEnvrnFlag) PumpEquip(PumpNum)%PumpInitFlag = .TRUE.

  ! zero out module level working variables
  PumpMassFlowRate = 0.d0
  PumpHeattoFluid  = 0.d0
  Power            = 0.d0
  ShaftPower       = 0.d0

 RETURN

END SUBROUTINE InitializePumps
!*************************************************************************!

!*************************************************************************!
SUBROUTINE SetupPumpMinMaxFlows(LoopNum, PumpNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:        Edwin Lee
          !       DATE WRITTEN:  Aug 2010
          !       MODIFIED       Based on the Flow control portion of what was previously Pumps::InitSimVars, by:
          !                        Dan Fisher October 1998
          !                        Richard Liesen July 2001
          !                        July 2001, Rick Strand (implemented new pump controls)
          !                        May 2009, Brent Griffith (added EMS override capability)
          !                        B. Griffith, Nov 2011 Pump control: Intermittent vs Continuous
          !       RE-ENGINEERED

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes the pump minAvail and maxAvail flow rates, and assigns them to the
          !  outlet min/max avail according to inlet min/max constraints and zero flow request
          ! The loop solver then uses this information to set up the flow bounds for the loop side
          !  for the current iteration.

          ! METHODOLOGY EMPLOYED:
          !  Design flow rate and user specified minimum flow rate is compared in the inlet node
          !  min/maxavail.  The pump output is appropriately constrained.
          !
          !  Design flow is rated flow times schedule fraction
          !  Inlet node max will represent the rated flow rate according to pump init routines.
          !  These values are bounded by hardware min constraints on the inlet node, which is likely zero.
          !  These values are also bounded by EMS overridable limit of max flow rate.

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE FluidProperties, ONLY: GetDensityGlycol
  USE DataPlant,       ONLY: PlantLoop, Press_FlowCorrection, PlantAvailMgr
  USE PlantPressureSystem, ONLY: ResolveLoopFlowVsPressure
  USE PlantUtilities,  ONLY: BoundValueToWithinTwoValues

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: LoopNum
  INTEGER, INTENT(IN)  :: PumpNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName = 'PlantPumps:SetupPumpMinMaxFlows: '

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER    :: InletNode             !pump inlet node number
  INTEGER    :: OutletNode            !pump outlet node number
  REAL(r64)  :: InletNodeMax
  REAL(r64)  :: InletNodeMin
  REAL(r64)  :: PumpMassFlowRateMax   !max allowable flow rate at the pump
  REAL(r64)  :: PumpMassFlowRateMin   !min allowable flow rate at the pump
  REAL(r64)  :: PumpSchedFraction
  REAL(r64)  :: PumpOverridableMaxLimit
  REAL(r64)  :: PumpMassFlowRateMinLimit
  REAL(r64)  :: PumpSchedRPM          !Pump RPM Optional Input

    !Inlet/Outlet Node Numbers
  InletNode         = PumpEquip(PumpNum)%InletNodeNum
  OutletNode        = PumpEquip(PumpNum)%OutletNodeNum

    !Inlet node Min/MaxAvail
  InletNodeMax      = Node(InletNode)%MassFlowRateMaxAvail
  InletNodeMin      = Node(InletNode)%MassFlowRateMinAvail

    !Retrive the pump speed fraction from the pump schedule
  IF (PumpEquip(PumpNum)%PumpScheduleIndex .NE. 0) THEN
    PumpSchedFraction = GetCurrentScheduleValue(PumpEquip(PumpNum)%PumpScheduleIndex)
    PumpSchedFraction = BoundValueToWithinTwoValues(PumpSchedFraction, 0.0d0, 1.0d0)
  ELSE
    PumpSchedFraction = 1.0d0
  END IF

    !User specified min/max mass flow rates for pump
  PumpOverridableMaxLimit  = PumpEquip(PumpNum)%MassFlowRateMax
  PumpMassFlowRateMinLimit = PumpEquip(PumpNum)%MassFlowRateMin

    !The pump outlet node Min/MaxAvail
  PumpMassFlowRateMin = MAX(InletNodeMin, PumpMassFlowRateMinLimit)
  PumpMassFlowRateMax = MIN(InletNodeMax, PumpOverridableMaxLimit * PumpSchedFraction)

    !Check for conflicts (MaxAvail < MinAvail)
  IF(PumpMassFlowRateMin > PumpMassFlowRateMax)THEN !the demand side wants to operate outside of the pump range
        !shut the pump (and the loop) down
    PumpMassFlowRateMin = 0.0d0
    PumpMassFlowRateMax = 0.0d0
        !Let the user know that his input file is overconstrained
    !DSU? Call one-time warning...with a counter
  ENDIF

    !DSU? IF (EMS ACTIVE) THEN...
    !DSU?         PumpMassFlowRateMax = MIN(PumpMassFlowRateMax, PumpOverridableMaxLimit) !Allow override by EMS

  SELECT CASE (PumpEquip(PumpNum)%PumpType)

    CASE (Pump_VarSpeed)

      IF (PumpEquip(PumpNum)%HasVFD) THEN
        SELECT CASE (PumpEquip(PumpNum)%VFD%VFDControlType)
        CASE (VFDManual)

          !Evaluate the schedule if it exists and put the fraction into a local variable
          PumpSchedRPM = GetCurrentScheduleValue(PumpEquip(PumpNum)%VFD%ManualRPMSchedIndex)
          !Convert the RPM to rot/sec for calculation routine
          PumpEquip(PumpNum)%RotSpeed = PumpSchedRPM / 60.0d0
          !Resolve the new mass flow rate based on current pressure characteristics
          IF (PlantLoop(PumpEquip(PumpNum)%LoopNum)%UsePressureForPumpCalcs .AND. &
              PlantLoop(PumpEquip(PumpNum)%LoopNum)%PressureSimType == Press_FlowCorrection .AND. &
              PlantLoop(PumpEquip(PumpNum)%LoopNum)%PressureDrop.GT.0.0d0) THEN

            PumpMassFlowRate    = ResolveLoopFlowVsPressure(PumpEquip(PumpNum)%LoopNum, &
                                  Node(PumpEquip(PumpNum)%InletNodeNum)%MassFlowRate, &
                                  PumpEquip(PumpNum)%PressureCurve_Index, &
                                  PumpEquip(PumpNum)%RotSpeed, &
                                  PumpEquip(PumpNum)%ImpellerDiameter, &
                                  PumpEquip(PumpNum)%MinPhiValue, &
                                  PumpEquip(PumpNum)%MaxPhiValue)

            PumpMassFlowRateMax = PumpMassFlowRate
            PumpMassFlowRateMin = PumpMassFlowRate

          END IF

        CASE (VFDAutomatic)

          IF (PlantLoop(PumpEquip(PumpNum)%LoopNum)%UsePressureForPumpCalcs .AND. &
              PlantLoop(PumpEquip(PumpNum)%LoopNum)%PressureSimType == Press_FlowCorrection .AND. &
              PlantLoop(PumpEquip(PumpNum)%LoopNum)%PressureDrop.GT.0.0d0) THEN

            Call GetRequiredMassFlowRate(LoopNum, PumpNum, Node(PumpEquip(PumpNum)%InletNodeNum)%MassFlowRate, &
                                         PumpMassFlowRate, PumpMassFlowRateMin, PumpMassFlowRateMax)

          END IF

        END SELECT !VFDControlType

      END IF

      IF (PumpEquip(PumpNum)%PumpControl == Continuous) THEN
        Node(InletNode)%MassFlowRateRequest = PumpMassFlowRateMin
      ENDIF

    CASE (Pump_ConSpeed)

      IF (PumpEquip(PumpNum)%PumpControl == Continuous) THEN
        PumpMassFlowRateMin = PumpMassFlowRateMax
        Node(InletNode)%MassFlowRateRequest = PumpMassFlowRateMin
      ENDIF

      ! Override (lock down flow) for pressure drop if applicable
      IF (PumpEquip(PumpNum)%LoopNum .GT. 0) THEN
        IF (PlantLoop(PumpEquip(PumpNum)%LoopNum)%UsePressureForPumpCalcs .AND. &
            PlantLoop(PumpEquip(PumpNum)%LoopNum)%PressureSimType == Press_FlowCorrection .AND. &
            PlantLoop(PumpEquip(PumpNum)%LoopNum)%PressureDrop.GT.0.0d0) THEN
          PumpMassFlowRate    = ResolveLoopFlowVsPressure(PumpEquip(PumpNum)%LoopNum, &
                                Node(PumpEquip(PumpNum)%InletNodeNum)%MassFlowRate, &  !DSU? Is this still valid?
                                PumpEquip(PumpNum)%PressureCurve_Index, &
                                PumpEquip(PumpNum)%RotSpeed, &
                                PumpEquip(PumpNum)%ImpellerDiameter, &
                                PumpEquip(PumpNum)%MinPhiValue, &
                                PumpEquip(PumpNum)%MaxPhiValue)
          PumpMassFlowRateMax = PumpMassFlowRate
          PumpMassFlowRateMin = PumpMassFlowRate
        END IF
      END IF

  END SELECT

  ! Override pump operation based on System Availability Managers, should be done elsewhere?  I suppose this should be OK though
  IF(ALLOCATED(PlantAvailMgr))THEN
    IF (PlantAvailMgr(LoopNum)%AvailStatus == ForceOff) THEN
      PumpMassFlowRateMax = 0.0d0
      PumpMassFlowRateMin = 0.0d0
    END IF
  END IF

  ! Check if EMS is overriding flow
  IF (PumpEquip(PumpNum)%EMSMassFlowOverrideOn) THEN
    PumpMassFlowRateMax = PumpEquip(PumpNum)%EMSMassFlowValue
    PumpMassFlowRateMin = PumpEquip(PumpNum)%EMSMassFlowValue
  ENDIF

  ! Update outlet node to allow loop solver to get data
  !DSU?  could avoid this by passing data in/out to avoid putting things on nodes
  Node(OutletNode)%MassFlowRateMinAvail = PumpMassFlowRateMin
  Node(OUtletNode)%MassFlowRateMaxAvail = PumpMassFlowRateMax

 RETURN

END SUBROUTINE SetupPumpMinMaxFlows
!*************************************************************************!

!*************************************************************************!
SUBROUTINE CalcPumps(PumpNum, FlowRequest, PumpRunning)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Sept. 1998
          !       MODIFIED       July 2001, Rick Strand
          !       RE-ENGINEERED  Sept 2010, Edwin Lee

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutines simulates a pump following
          ! the methodology oulined in ASHRAE's secondary toolkit.

          ! METHODOLOGY EMPLOYED:
          ! Calculates power and updates other pump things.

          ! REFERENCES:
          ! HVAC 2 Toolkit:  A Toolkit for Secondary HVAC System
          ! Energy Calculations, ASHRAE, 1993, pp2-10 to 2-15

          ! USE STATEMENTS:
  USE PlantUtilities,         ONLY: SetComponentFlowRate
  USE FluidProperties,        ONLY: GetDensityGlycol, GetSpecificHeatGlycol
  USE DataPlant,              ONLY: PlantLoop
  USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance
  USE General,                ONLY: RoundSigDigits
  USE ScheduleManager,        ONLY: GetCurrentScheduleValue
  USE DataConvergParams,      ONLY: PlantFlowRateToler
  USE DataBranchAirLoopPlant, ONLY: ControlType_SeriesActive

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)     :: PumpNum
  REAL(r64), INTENT(IN)   :: FlowRequest
  LOGICAL, INTENT(IN OUT) :: PumpRunning

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: RotSpeed_Tol = 0.01d0
  CHARACTER(len=*), PARAMETER :: RoutineName = 'PlantPumps:CalcPumps: '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: InletNode
  INTEGER   :: OutletNode
  REAL(r64) :: LoopDensity
!  INTEGER   :: DummyWaterIndex = 1
  REAL(r64) :: VolFlowRate
  REAL(r64) :: PartLoadRatio
  REAL(r64) :: FracFullLoadPower
  REAL(r64) :: FullLoadVolFlowRate
  REAL(r64) :: PartLoadVolFlowRate
  REAL(r64) :: FullLoadPower
  REAL(r64) :: FullLoadPowerRatio
  REAL(r64) :: TotalEffic
  INTEGER   :: PumpType
  REAL(r64) :: RotSpeed_Min
  REAL(r64) :: RotSpeed_Max
  REAL(r64) :: PumpActualRPMValueOne
  REAL(r64) :: PumpActualRPMValueTwo
  INTEGER   :: NumBranchesOnThisLoopSide

  InletNode   = PumpEquip(PumpNum)%InletNodeNum
  OutletNode  = PumpEquip(PumpNum)%OutletNodeNum
  PumpType    = PumpEquip(PumpNum)%PumpType


 !****************************!
 !** SETTING PUMP FLOW RATE **!
 !****************************!
  ! So the loop solver always passes in the full loop side flow request to each pump called
  ! The pump will try to use this value according to its inlet conditions via the SetComponentFlowRate routine.
  ! If the loop solver is doing branch pumps, then individual parallel branch inlet nodes would have been previously
  ! constrained, so even though we pass in a full flow request, each pump will "pull down" to the min/max avail.
  ! Also, on flowlock == locked, we will just use the inlet node flow rate
  ! The flow resolver can take care of argument resolution beyond that.
  ! For a typical situation, the flow request should be within the values of min/max avail, so the pump will get this flow rate.
  IF (FlowRequest > MassFlowTolerance) THEN
      PumpMassFlowRate = FlowRequest
  ELSE
      PumpMassFlowRate = 0.0d0
  END IF

  ! For variable speed branch pumps, with other components
  !  on the branch, we are not going to assign a request.
  ! Other components on this branch will request flow for this branch

!  ! If this is a variable speed pump
  IF ((PumpEquip(PumpNum)%PumpType == Pump_VarSpeed)     .OR. &
      (PumpEquip(PumpNum)%PumpType == PumpBank_VarSpeed) .OR. &
      (PumpEquip(PumpNum)%PumpType == Pump_Cond)) THEN

    IF (PlantLoop(PumpEquip(PumpNum)%LoopNum)%LoopSide(PumpEquip(PumpNum)%LoopSideNum)% &
         Branch(PumpEquip(PumpNum)%BranchNum)%Comp(PumpEquip(PumpNum)%CompNum)%FlowCtrl == ControlType_SeriesActive) THEN
      PumpMassFlowRate = 0.0d0
    ENDIF

  END IF

! bound flow request by pump max limit, the Flow Request is total loop flow and if this is a branch pump that is not appropriate
  PumpMassFlowRate = MIN(PumpEquip(PumpNum)%MassFlowRateMax, PumpMassFlowRate)
  PumpMassFlowRate = MAX(PumpEquip(PumpNum)%MassFlowRateMin, PumpMassFlowRate)

  CALL SetComponentFlowRate(                                &
                            PumpMassFlowRate,               &
                            InletNode,                      &
                            OutletNode,                     &
                            PumpEquip(PumpNum)%LoopNum,     &
                            PumpEquip(PumpNum)%LoopSideNum, &
                            PumpEquip(PumpNum)%BranchNum,   &
                            PumpEquip(PumpNum)%CompNum      &
                           )

  !Get RPM value for reporting as output
  !RPM is calculated using pump affinity laws for rotation speed
  IF (PlantLoop(PumpEquip(PumpNum)%LoopNum)%UsePressureForPumpCalcs .AND. PumpEquip(PumpNum)%HasVFD) THEN
    RotSpeed_Min =  GetCurrentScheduleValue(PumpEquip(PumpNum)%VFD%MinRPMSchedIndex)
    RotSpeed_Max =  GetCurrentScheduleValue(PumpEquip(PumpNum)%VFD%MaxRPMSchedIndex)
    IF (PumpEquip(PumpNum)%PumpMassFlowRateMaxRPM < MassFlowTolerance &
            .OR. PumpEquip(PumpNum)%PumpMassFlowRateMinRPM < MassFlowTolerance) THEN
        PumpEquip(PumpNum)%VFD%PumpActualRPM = 0.0d0
    ELSE
        PumpActualRPMValueOne = (PumpMassFlowRate/PumpEquip(PumpNum)%PumpMassFlowRateMaxRPM) * RotSpeed_Max
        PumpActualRPMValueTwo = (PumpMassFlowRate/PumpEquip(PumpNum)%PumpMassFlowRateMinRPM) * RotSpeed_Min
        PumpEquip(PumpNum)%VFD%PumpActualRPM = (PumpActualRPMValueOne + PumpActualRPMValueTwo) / 2
    END IF
  END IF

 !****************************!
 !** DETERMINE IF PUMP IS ON *!
 !****************************!
 ! Since we don't allow series pumping, if there is ANY flow rate for this pump, THIS PUMP is driving the flow!  Therefore...
 PumpRunning = (PumpMassFlowRate .GT. MassFlowTolerance)

 !****************************!
 !** UPDATE PUMP BANK USAGE **!
 !****************************!
 SELECT CASE (PumpEquip(PumpNum)%PumpType)
 CASE (PumpBank_VarSpeed, PumpBank_ConSpeed)
    ! previously, pumps did whatever they wanted
    ! because of this a constant speed pump bank could adjust the flow rate as-desired
    !  even if it was not allowed
    ! since pumps now must behave nicely like all other components, the calculation of number
    !  of running pumps in a pump bank is the same for both bank types
    ! the pumps are loaded sequentially, and the last pump can have full or non-full part load
    !  status...this is just how it works now.  The pump cannot *bump* up the flow on the loop
    !  to make sure the last running pump is fully loaded anymore for constant speed pumps...sorry
    IF (PumpMassFlowRate >= PumpEquip(PumpNum)%MassFlowRateMax) THEN
        !running full on
        NumPumpsRunning = PumpEquip(PumpNum)%NumPumpsInBank
    ELSE
        !running at some sort of part load
        NumPumpsRunning = CEILING((PumpMassFlowRate / (PumpEquip(PumpNum)%MassFlowRateMax) * PumpEquip(PumpNum)%NumPumpsInBank) )
        NumPumpsRunning = MIN(NumPumpsRunning, PumpEquip(PumpNum)%NumPumpsInBank)
    END IF
 END SELECT

 !****************************!
 !***** EXIT IF NO FLOW ******!
 !****************************!
  IF (PumpMassFlowRate <= MassFlowTolerance) THEN
    Node(OutletNode)%Temp         = Node(InletNode)%Temp
    Node(OutletNode)%Press        = Node(InletNode)%Press
    Node(OutletNode)%Quality      = Node(InletNode)%Quality
    RETURN
  END IF

! density used for volumetric flow calculations
  LoopDensity = GetDensityGlycol(PlantLoop(PumpEquip(PumpNum)%LoopNum)%FluidName, &
                                 Node(InletNode)%Temp, &
                                 PlantLoop(PumpEquip(PumpNum)%LoopNum)%FluidIndex, &
                                 RoutineName)

 !****************************!
 !***** CALCULATE POWER (1) **!
 !****************************!
  IF(PumpType == Pump_ConSpeed .OR. PumpType == Pump_VarSpeed .OR. PumpType == Pump_Cond) THEN

   VolFlowRate       = PumpMassFlowRate / LoopDensity
   PartLoadRatio     = MIN(1.0d0,(VolFlowRate / PumpEquip(PumpNum)%NomVolFlowRate))
   FracFullLoadPower =   PumpEquip(PumpNum)%PartLoadCoef(1)                     &
                       + PumpEquip(PumpNum)%PartLoadCoef(2) * PartLoadRatio     &
                       + PumpEquip(PumpNum)%PartLoadCoef(3) * PartLoadRatio**2  &
                       + PumpEquip(PumpNum)%PartLoadCoef(4) * PartLoadRatio**3
   Power             = FracFullLoadPower * PumpEquip(PumpNum)%NomPowerUse

  ELSE IF(PumpType == PumpBank_ConSpeed .OR. PumpType == PumpBank_VarSpeed) THEN

    ! now just assume the last one is (or is not) running at part load
    ! if it is actually at full load, the calculations work out to PLR = 1
    ! for the last pump, so all is OK
    NumPumpsFullLoad = NumPumpsRunning - 1
    FullLoadVolFlowRate = PumpEquip(PumpNum)%NomVolFlowRate/PumpEquip(PumpNum)%NumPumpsInBank
    PartLoadVolFlowrate = PumpMassFlowRate/LoopDensity - FullLoadVolFlowRate * NumPumpsFullLoad
    FullLoadPower       = PumpEquip(PumpNum)%NomPowerUse/PumpEquip(PumpNum)%NumPumpsInBank
    FullLoadPowerRatio  = PumpEquip(PumpNum)%PartLoadCoef(1)  &
                         +PumpEquip(PumpNum)%PartLoadCoef(2)  &
                         +PumpEquip(PumpNum)%PartLoadCoef(3)  &
                         +PumpEquip(PumpNum)%PartLoadCoef(4)
    PartLoadRatio       = MIN(1.0d0,(PartLoadVolFlowrate / FullLoadVolFlowRate))
    FracFullLoadPower   =  PumpEquip(PumpNum)%PartLoadCoef(1)                     &
                         + PumpEquip(PumpNum)%PartLoadCoef(2) * PartLoadRatio     &
                         + PumpEquip(PumpNum)%PartLoadCoef(3) * PartLoadRatio**2  &
                         + PumpEquip(PumpNum)%PartLoadCoef(4) * PartLoadRatio**3
    Power = (FullLoadPowerRatio * NumPumpsFullLoad + FracFullLoadPower) * FullLoadPower

  END IF

 !****************************!
 !***** CALCULATE POWER (2) **!
 !****************************!
  IF (Power < 0.0d0) THEN
    IF (PumpEquip(PumpNum)%PowerErrIndex1 == 0) THEN
      CALL ShowWarningMessage(RoutineName//' Calculated Pump Power < 0, Type='//  &
        trim(cPumpTypes(PumpType))//', Name="'//trim(PumpEquip(PumpNum)%Name)//'".')
      CALL ShowContinueErrorTimeStamp(' ')
      CALL ShowContinueError('...PartLoadRatio=['//trim(RoundSigDigits(PartLoadRatio,4))//'], '// &
        'Fraction Full Load Power='//trim(RoundSigDigits(FracFullLoadPower,4))//']')
      CALL ShowContinueError('...Power is set to 0 for continuing the simulation.')
      CALL ShowContinueError('...Pump coefficients should be checked for producing this negative value.')
    ENDIF
    Power=0.0d0
    CALL ShowRecurringWarningErrorAtEnd(RoutineName//' Calculated Pump Power < 0, '// &
        trim(cPumpTypes(PumpType))//', Name="'//trim(PumpEquip(PumpNum)%Name)//'", PLR=', &
        PumpEquip(PumpNum)%PowerErrIndex1,ReportMinOf=PartLoadRatio,ReportMaxOf=PartLoadRatio)
    CALL ShowRecurringContinueErrorAtEnd('...Fraction Full Load Power=', &
        PumpEquip(PumpNum)%PowerErrIndex2,ReportMinOf=FracFullLoadPower,ReportMaxOf=FracFullLoadPower)
  ENDIF

 !****************************!
 !***** CALCULATE POWER (3) **!
 !****************************!
  !Now if we are doing pressure-based simulation, then we have a means to calculate power exactly based on current
  ! simulation conditions (flow rate and pressure drop) along with knowledge about pump impeller and motor efficiencies
  !Thus we will override the power that was calculated based on nominal values with the corrected pressure-based power
  IF (PumpEquip(PumpNum)%LoopNum .GT. 0) THEN
    IF (PlantLoop(PumpEquip(PumpNum)%LoopNum)%UsePressureForPumpCalcs) THEN
      TotalEffic = PumpEquip(PumpNum)%PumpEffic * PumpEquip(PumpNum)%MotorEffic
      !Efficiency errors are caught previously, but it doesn't hurt to add another catch before dividing by zero!!!
      IF (TotalEffic == 0.0d0) THEN
        CALL ShowSevereError(RoutineName//' Plant pressure simulation encountered a pump with zero efficiency: '// &
                             PumpEquip(PumpNum)%Name)
        CALL ShowContinueError('Check efficiency inputs for this pump component.')
        CALL ShowFatalError('Errors in plant calculation would result in divide-by-zero cause program termination.')
      END IF
      Power = VolFlowRate * PlantLoop(PumpEquip(PumpNum)%LoopNum)%PressureDrop / TotalEffic
    END IF
  END IF

  ! if user has specified a pressure value, then use it, same as for pressure-based simulation
  IF (PumpEquip(PumpNum)%EMSPressureOverrideOn) THEN
    TotalEffic = PumpEquip(PumpNum)%PumpEffic * PumpEquip(PumpNum)%MotorEffic
    !Efficiency errors are caught previously, but it doesn't hurt to add another catch before dividing by zero!!!
    IF (TotalEffic == 0.0d0) THEN
      CALL ShowSevereError(RoutineName//' Plant pump simulation encountered a pump with zero efficiency: '// &
                            PumpEquip(PumpNum)%Name)
      CALL ShowContinueError('Check efficiency inputs for this pump component.')
      CALL ShowFatalError('Errors in plant calculation would result in divide-by-zero cause program termination.')
    END IF
    Power = VolFlowRate * PumpEquip(PumpNum)%EMSPressureOverrideValue / TotalEffic
  ENDIF

 !****************************!
 !***** CALCULATE POWER (4) **!
 !****************************!
  ! This adds the pump heat based on User input for the pump
  ! We assume that all of the heat ends up in the fluid eventually since this is a closed loop
  ShaftPower = Power * PumpEquip(PumpNum)%MotorEffic
  PumpHeattoFluid = ShaftPower + (Power - ShaftPower)*PumpEquip(PumpNum)%FracMotorLossToFluid

 !****************************!
 !***** UPDATE INFORMATION ***!
 !****************************!
  ! Update data structure variables
  PumpEquip(PumpNum)%Power      = Power

  ! Update outlet node conditions
  Node(OutletNode)%Temp         = Node(InletNode)%Temp
  Node(OutletNode)%Press        = Node(InletNode)%Press
  Node(OutletNode)%Quality      = Node(InletNode)%Quality

 RETURN

END SUBROUTINE CalcPumps
!*************************************************************************!

!*************************************************************************!
SUBROUTINE SizePump(PumpNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   December 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Pump Components for which flow rates have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the plant sizing array.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing,      ONLY: AutoSize, PlantSizData
  USE DataPlant,       ONLY: PlantLoop
  USE General,         ONLY: RoundSigDigits
  USE ReportSizingManager, ONLY: ReportSizingOutput
  USE FluidProperties, ONLY: GetSatDensityRefrig, GetDensityGlycol

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: PumpNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: StartTemp =100.0d0   ! Standard Temperature across code to calculated Steam density
  CHARACTER(len=*), PARAMETER :: RoutineName = 'PlantPumps::InitSimVars '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: PlantSizNum        ! index of Plant Sizing array
  LOGICAL   :: ErrorsFound
  REAL(r64) :: TotalEffic    ! pump total efficiency
  INTEGER   :: Side               ! half loop index
  INTEGER   :: BranchNum          ! index of branch
  INTEGER   :: CompNum             ! index of component on branch
  REAL(r64) :: PumpSizFac    ! pump sizing factor
  REAL(r64) :: SteamDensity
  REAL(r64) :: TempWaterDensity
  INTEGER   :: DummyWaterIndex = 1
  REAL(r64) :: DesVolFlowRatePerBranch ! local temporary for split of branch pumps

  ! Calculate density at InitConvTemp once here, to remove RhoH2O calls littered throughout
  IF (PumpEquip(PumpNum)%LoopNum > 0) THEN
    TempWaterDensity = GetDensityGlycol(PlantLoop(PumpEquip(PumpNum)%LoopNum)%FluidName, &
                                      InitConvTemp, &
                                      PlantLoop(PumpEquip(PumpNum)%LoopNum)%FluidIndex, RoutineName)
  ELSE
    TempWaterDensity = GetDensityGlycol('WATER', InitConvTemp, DummyWaterIndex, RoutineName)
  ENDIF

  ! note: we assume pump impeller efficiency is 78% for autosizing
  TotalEffic = 0.78d0 * PumpEquip(PumpNum)%MotorEffic
  PlantSizNum = 0
  PumpSizFac = 1.0d0
  ErrorsFound = .FALSE.
  ! CurLoopNum > 0 only for Plant Loops; condenser loops not sized yet
  IF (PumpEquip(PumpNum)%LoopNum > 0) THEN
    PlantSizNum = PlantLoop(PumpEquip(PumpNum)%LoopNum)%PlantSizNum
!  ELSE IF (CurCondLoopNum > 0) THEN
!    PlantSizNum = CondSupplySide(CurCondLoopNum)%PlantSizNum
  END IF
! look for pump sizing factor on branch
  IF (PumpEquip(PumpNum)%LoopNum > 0) THEN
    SideLoop: DO Side=1,2
      DO BranchNum = 1,PlantLoop(PumpEquip(PumpNum)%LoopNum)%LoopSide(Side)%TotalBranches
        DO CompNum = 1, PlantLoop(PumpEquip(PumpNum)%LoopNum)%LoopSide(Side)%Branch(BranchNum)%TotalComponents
          IF (PumpEquip(PumpNum)%InletNodeNum ==   &
                PlantLoop(PumpEquip(PumpNum)%LoopNum)%LoopSide(Side)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn .AND. &
              PumpEquip(PumpNum)%OutletNodeNum ==   &
                 PlantLoop(PumpEquip(PumpNum)%LoopNum)%LoopSide(Side)%Branch(BranchNum)%Comp(CompNum)%NodeNumOut) THEN
            IF (PlantLoop(PumpEquip(PumpNum)%LoopNum)%LoopSide(Side)%Branch(BranchNum)%PumpSizFac > 0.0d0) THEN
              PumpSizFac = PlantLoop(PumpEquip(PumpNum)%LoopNum)%LoopSide(Side)%Branch(BranchNum)%PumpSizFac
            ELSE
              PumpSizFac = 1.0d0
            END IF
            EXIT SideLoop
          END IF
        END DO
      END DO
    END DO SideLoop
  END IF

  IF (PumpEquip(PumpNum)%NomVolFlowRate == AutoSize) THEN

    IF (PlantSizNum > 0) THEN
      IF (PlantSizData(PlantSizNum)%DesVolFlowRate >= SmallWaterVolFlow) THEN
        IF (.NOT. PlantLoop(PumpEquip(PumpNum)%LoopNum)%LoopSide(Side)%BranchPumpsExist) THEN
          ! size pump to full flow of plant loop
          IF(PumpEquip(PumpNum)%PumpType == Pump_Cond)THEN
            TempWaterDensity = GetDensityGlycol('WATER', InitConvTemp, DummyWaterIndex, RoutineName)
            SteamDensity=GetSatDensityRefrig('STEAM',StartTemp,1.0d0,PumpEquip(PumpNum)%FluidIndex,'SizePumps')
            PumpEquip(PumpNum)%NomSteamVolFlowRate= PlantSizData(PlantSizNum)%DesVolFlowRate * PumpSizFac
            PumpEquip(PumpNum)%NomVolFlowRate = PumpEquip(PumpNum)%NomSteamVolFlowRate*SteamDensity/TempWaterDensity
          ELSE
            PumpEquip(PumpNum)%NomVolFlowRate = PlantSizData(PlantSizNum)%DesVolFlowRate * PumpSizFac
          END IF
        ELSE
        ! Distribute sizes evenly across all branch pumps
          DesVolFlowRatePerBranch =  PlantSizData(PlantSizNum)%DesVolFlowRate &
                                      /  PlantLoop(PumpEquip(PumpNum)%LoopNum)%LoopSide(Side)%TotalPumps
          IF(PumpEquip(PumpNum)%PumpType == Pump_Cond)THEN
            TempWaterDensity = GetDensityGlycol('WATER', InitConvTemp, DummyWaterIndex, RoutineName)
            SteamDensity=GetSatDensityRefrig('STEAM',StartTemp,1.0d0,PumpEquip(PumpNum)%FluidIndex,'SizePumps')
            PumpEquip(PumpNum)%NomSteamVolFlowRate= DesVolFlowRatePerBranch * PumpSizFac
            PumpEquip(PumpNum)%NomVolFlowRate = PumpEquip(PumpNum)%NomSteamVolFlowRate*SteamDensity/TempWaterDensity
          ELSE
            PumpEquip(PumpNum)%NomVolFlowRate = DesVolFlowRatePerBranch * PumpSizFac
         ENDIF
        ENDIF

      ELSE
        PumpEquip(PumpNum)%NomVolFlowRate = 0.0d0
        CALL ShowWarningError('SizePump: Calculated Pump Nominal Volume Flow Rate=['//  &
             TRIM(RoundSigDigits(PlantSizData(PlantSizNum)%DesVolFlowRate, 2))//'] is too small. Set to 0.0')
        CALL ShowContinueError('..occurs for Pump='//TRIM(PumpEquip(PumpNum)%Name))
      END IF
      CALL ReportSizingOutput(cPumpTypes(PumpEquip(PumpNum)%PumpType), PumpEquip(PumpNum)%Name, &
                              'Rated Flow Rate [m3/s]', PumpEquip(PumpNum)%NomVolFlowRate)
    ELSE
      CALL ShowSevereError('Autosizing of plant loop pump flow rate requires a loop Sizing:Plant object')
      CALL ShowContinueError('Occurs in plant pump object='//TRIM(PumpEquip(PumpNum)%Name))
      ErrorsFound = .TRUE.
    END IF

  END IF

  ! Note that autocalculation of power is based on nominal volume flow, regardless of whether the flow was
  !  auto sized or manually sized.  Thus, this must go after the flow sizing block above.
  IF (PumpEquip(PumpNum)%NomPowerUse == AutoSize) THEN
      IF (PumpEquip(PumpNum)%NomVolFlowRate >= SmallWaterVolFlow) THEN
        PumpEquip(PumpNum)%NomPowerUse = (PumpEquip(PumpNum)%NomPumpHead * PumpEquip(PumpNum)%NomVolFlowRate) / &
                                         TotalEffic
      ELSE
        PumpEquip(PumpNum)%NomPowerUse = 0.0d0
      END IF
      CALL ReportSizingOutput(cPumpTypes(PumpEquip(PumpNum)%PumpType), PumpEquip(PumpNum)%Name, &
                              'Rated Power Consumption [W]', PumpEquip(PumpNum)%NomPowerUse)
  END IF

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Preceding sizing errors cause program termination')
  END IF

  RETURN

END SUBROUTINE SizePump
!*************************************************************************!

!*************************************************************************!
SUBROUTINE ReportPumps(PumpNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Dan Fisher
          !       DATE WRITTEN:    October 1998
          !       MODIFIED         July 2001, Rick Strand (revision of pump module)
          !       RE-ENGINEERED    na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets the pump reporting variables.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataBranchAirLoopPlant, ONLY: MassFlowTolerance

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: PumpNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: InletNode  ! pump inlet node number
  INTEGER :: OutletNode ! pump outlet node number
  INTEGER :: PumpType    !Current pump type

          ! FLOW:
  PumpType   = PumpEquip(PumpNum)%PumpType
  InletNode  = PumpEquip(PumpNum)%InletNodeNum
  OutletNode = PumpEquip(PumpNum)%OutletNodeNum

  IF (PumpMassFlowRate <= MassFlowTolerance) THEN
   PumpEquipReport(PumpNum)%PumpMassFlowRate = 0.0d0
   PumpEquipReport(PumpNum)%PumpHeattoFluid  = 0.0d0
   PumpEquipReport(PumpNum)%OutletTemp       = Node(OutletNode)%Temp
   PumpEquip(PumpNum)%Power                  = 0.0d0
   PumpEquip(PumpNum)%Energy                 = 0.0d0
   PumpEquipReport(PumpNum)%ShaftPower       = 0.0d0
   PumpEquipReport(PumpNum)%PumpHeattoFluidEnergy = 0.0d0
   PumpEquipReport(PumpNum)%ZoneTotalGainRate     = 0.d0
   PumpEquipReport(PumpNum)%ZoneTotalGainEnergy   = 0.d0
   PumpEquipReport(PumpNum)%ZoneConvGainRate      = 0.d0
   PumpEquipReport(PumpNum)%ZoneRadGainRate       = 0.d0
   PumpEquipReport(PumpNum)%NumPumpsOperating = 0
  ELSE
   PumpEquipReport(PumpNum)%PumpMassFlowRate = PumpMassFlowRate
   PumpEquipReport(PumpNum)%PumpHeattoFluid  = PumpHeattoFluid
   PumpEquipReport(PumpNum)%OutletTemp       = Node(OutletNode)%Temp
   PumpEquip(PumpNum)%Power                  = Power
   PumpEquip(PumpNum)%Energy                 = PumpEquip(PumpNum)%Power * TimeStepSys * SecInHour
   PumpEquipReport(PumpNum)%ShaftPower       = ShaftPower
   PumpEquipReport(PumpNum)%PumpHeattoFluidEnergy = PumpHeattoFluid * TimeStepSys * SecInHour
   IF(PumpType == Pump_ConSpeed .OR. PumpType == Pump_VarSpeed .OR. PumpType == Pump_Cond) THEN
    PumpEquipReport(PumpNum)%NumPumpsOperating = 1
   ELSE IF(PumpType == PumpBank_ConSpeed .OR. PumpType == PumpBank_VarSpeed) THEN
    PumpEquipReport(PumpNum)%NumPumpsOperating = NumPumpsRunning
   END IF
   PumpEquipReport(PumpNum)%ZoneTotalGainRate     = Power - PumpHeattoFluid
   PumpEquipReport(PumpNum)%ZoneTotalGainEnergy   = PumpEquipReport(PumpNum)%ZoneTotalGainRate * TimeStepSys * SecInHour
   PumpEquipReport(PumpNum)%ZoneConvGainRate      = (1 - PumpEquip(PumpNum)%SkinLossRadFraction) &
                                                      * PumpEquipReport(PumpNum)%ZoneTotalGainRate
   PumpEquipReport(PumpNum)%ZoneRadGainRate       = PumpEquip(PumpNum)%SkinLossRadFraction &
                                                      * PumpEquipReport(PumpNum)%ZoneTotalGainRate
  END IF

  RETURN

END SUBROUTINE ReportPumps
!*************************************************************************!

!*************************************************************************!
SUBROUTINE PumpDataForTable(NumPump)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR:          Jason Glazer
          !       DATE WRITTEN:    September 2006
          !       MODIFIED         na
          !       RE-ENGINEERED    na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Pull data together for predefined tables.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  USE OutputReportPredefined

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: NumPump

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength) :: equipName

  equipName = PumpEquip(NumPump)%Name
  CALL PreDefTableEntry(pdchPumpType,equipName,cPumpTypes(PumpEquip(NumPump)%PumpType))
  IF (PumpEquip(NumPump)%PumpControl == Continuous) THEN
    CALL PreDefTableEntry(pdchPumpControl,equipName,'Continuous')
  ELSEIF (PumpEquip(NumPump)%PumpControl == Intermittent) THEN
    CALL PreDefTableEntry(pdchPumpControl,equipName,'Intermittent')
  ELSE
    CALL PreDefTableEntry(pdchPumpControl,equipName,'Unknown')
  ENDIF
  CALL PreDefTableEntry(pdchPumpHead,equipName,PumpEquip(NumPump)%NomPumpHead)
  CALL PreDefTableEntry(pdchPumpFlow,equipName,PumpEquip(NumPump)%NomVolFlowRate,4)
  CALL PreDefTableEntry(pdchPumpPower,equipName,PumpEquip(NumPump)%NomPowerUse)
  IF (PumpEquip(NumPump)%NomVolFlowRate .NE. 0) THEN
    CALL PreDefTableEntry(pdchPumpPwrPerFlow,equipName,PumpEquip(NumPump)%NomPowerUse / PumpEquip(NumPump)%NomVolFlowRate)
  ELSE
    CALL PreDefTableEntry(pdchPumpPwrPerFlow,equipName,'-')
  END IF
  CALL PreDefTableEntry(pdchMotEff,equipName,PumpEquip(NumPump)%MotorEffic)

END SUBROUTINE PumpDataForTable


!*************************************************************************!
Subroutine GetRequiredMassFlowRate(LoopNum, PumpNum, InletNodeMassFlowRate, ActualFlowRate, &
                                   PumpMinMassFlowRateVFDRange,PumpMaxMassFlowRateVFDRange)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR
          !       DATE WRITTEN
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS SUBROUTINE:
          !

          ! METHODOLOGY EMPLOYED:
          ! General EnergyPlus Methodology

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:



  USE PlantUtilities,      ONLY: SetComponentFlowRate
  USE FluidProperties,  ONLY: GetDensityGlycol, GetSpecificHeatGlycol
  USE DataPlant, ONLY: Press_FlowCorrection, PlantLoop
  USE General, ONLY: RoundSigDigits
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE PlantPressureSystem, ONLY: ResolveLoopFlowVsPressure
  USE DataLoopNode,     ONLY: Node

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)     :: LoopNum
  INTEGER, INTENT(IN)     :: PumpNum
  REAL(r64), INTENT(IN)   :: InletNodeMassFlowRate
  REAL(r64), INTENT(IN OUT)    ::ActualFlowRate
  REAL(r64), INTENT(IN OUT)    ::PumpMinMassFlowRateVFDRange
  REAL(r64), INTENT(IN OUT)    ::PumpMaxMassFlowRateVFDRange

  REAL(r64)          :: PumpMassFlowRateMaxPress       = 0.0d0  ! Maximum mass flow rate associated with maximum pressure limit
  REAL(r64)          :: PumpMassFlowRateMinPress       = 0.0d0  ! Minimum mass flow rate associated with minimum pressure limit
  REAL(r64)          :: RotSpeed_Max                   = 0.0d0  ! Maximum rotaional speed in rps
  REAL(r64)          :: RotSpeed_Min                   = 0.0d0  ! Minimum rotaional speed in rps
  REAL(r64)          :: MinPress                   = 0.0d0  ! Minimum pressure
  REAL(r64)          :: MaxPress                   = 0.0d0  ! Maximum pressure

  RotSpeed_Min = GetCurrentScheduleValue(PumpEquip(PumpNum)%VFD%MinRPMSchedIndex)
  RotSpeed_Max = GetCurrentScheduleValue(PumpEquip(PumpNum)%VFD%MaxRPMSchedIndex)
  MinPress     = GetCurrentScheduleValue(PumpEquip(PumpNum)%VFD%LowerPsetSchedIndex)
  MaxPress     = GetCurrentScheduleValue(PumpEquip(PumpNum)%VFD%UpperPsetSchedIndex)

    !Calculate maximum and minimum mass flow rate associated with maximun and minimum RPM
    IF (PumpEquip(PumpNum)%LoopNum .GT. 0) THEN
        IF (PlantLoop(PumpEquip(PumpNum)%LoopNum)%UsePressureForPumpCalcs .AND. &
            PlantLoop(PumpEquip(PumpNum)%LoopNum)%PressureSimType == Press_FlowCorrection .AND. &
            PlantLoop(PumpEquip(PumpNum)%LoopNum)%PressureDrop.GT.0.0d0) THEN
            PumpEquip(PumpNum)%PumpMassFlowRateMaxRPM    = ResolveLoopFlowVsPressure(PumpEquip(PumpNum)%LoopNum, &
                                InletNodeMassFlowRate, &  !DSU? Is this still valid?
                                PumpEquip(PumpNum)%PressureCurve_Index, &
                                RotSpeed_Max, &
                                PumpEquip(PumpNum)%ImpellerDiameter, &
                                PumpEquip(PumpNum)%MinPhiValue, &
                                PumpEquip(PumpNum)%MaxPhiValue)
            PumpEquip(PumpNum)%PumpMassFlowRateMinRPM    = ResolveLoopFlowVsPressure(PumpEquip(PumpNum)%LoopNum, &
                                InletNodeMassFlowRate, &  !DSU? Is this still valid?
                                PumpEquip(PumpNum)%PressureCurve_Index, &
                                RotSpeed_Min, &
                                PumpEquip(PumpNum)%ImpellerDiameter, &
                                PumpEquip(PumpNum)%MinPhiValue, &
                                PumpEquip(PumpNum)%MaxPhiValue)
        END IF
    END IF

    !Not correct necessarily, but values are coming out way wrong here, maxRPMmdot~3, minRPMmdot~62!
    IF(PumpEquip(PumpNum)%PumpMassFlowRateMaxRPM < PumpEquip(PumpNum)%PumpMassFlowRateMinRPM) THEN
      PumpEquip(PumpNum)%PumpMassFlowRateMaxRPM = PumpEquip(PumpNum)%PumpMassFlowRateMinRPM
    END IF

    !Calculate maximum and minimum mass flow rate associated with operating pressure range
    IF (PumpEquip(PumpNum)%LoopNum .GT. 0) THEN
      If(PlantLoop(LoopNum)%PressureEffectiveK .GT. 0.0d0) Then
        PumpMassFlowRateMaxPress = ((MaxPress) / (PlantLoop(LoopNum)%PressureEffectiveK))**0.5d0
        PumpMassFlowRateMinPress = ((MinPress) / (PlantLoop(LoopNum)%PressureEffectiveK))**0.5d0
      End If
    End If

    !Decide operating range for mass flow rate
    !Maximum mass flow rate value of the range
    If(PumpEquip(PumpNum)%PumpMassFlowRateMaxRPM .GT. PumpMassFlowRateMaxPress) Then
      !Maximum pressure value governs maximum VFD range value
      PumpMaxMassFlowRateVFDRange = PumpMassFlowRateMaxPress
    Else
      !Maximum RPM value governs maximum VFD range value
      PumpMaxMassFlowRateVFDRange = PumpEquip(PumpNum)%PumpMassFlowRateMaxRPM
    End IF

    !Minimum mass flow rate value of the range
    If(PumpEquip(PumpNum)%PumpMassFlowRateMinRPM .GT. PumpMassFlowRateMinPress) Then
      !Minimum pressure value governs minimum VFD range value
      PumpMinMassFlowRateVFDRange = PumpEquip(PumpNum)%PumpMassFlowRateMinRPM
    Else
      !Minimum pressure range value governs minimum VFD range value
      PumpMinMassFlowRateVFDRange = PumpMassFlowRateMinPress
    End IF

  !Set the mass flow rate within VFD operating range
  If(InletNodeMassFlowRate .GT. PumpMinMassFlowRateVFDRange) Then
    If(InletNodeMassFlowRate .LT. PumpMaxMassFlowRateVFDRange) Then
      !Flow request is within VFD operating range
      ActualFlowRate = InletNodeMassFlowRate
    Else
      !Flow request is outside VFD operating range
      !Flow is set to maximum VFD operating range
      ActualFlowRate = PumpMaxMassFlowRateVFDRange
    End If
  Else
    !Flow request is outside VFD operating range
    !Flow is set to minimum VFD operating Range
    ActualFlowRate = PumpMinMassFlowRateVFDRange
  End If

  RETURN

END SUBROUTINE

!=================================================================================================!

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


END MODULE Pumps

