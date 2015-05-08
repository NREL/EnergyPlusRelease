MODULE DesiccantDehumidifiers

  ! Module containing the routines dealing with dehumidifiers

  ! MODULE INFORMATION:
  !       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
  !                      for Gas Research Institute
  !       DATE WRITTEN   March 2001
  !       MODIFIED       Jan 2005 M. J. Witte, GARD Analytics, Inc.
  !                        Add new control type option:
  !                          NODE LEAVING HUMRAT SETPOINT:BYPASS
  !                        Change existing control type to:
  !                          FIXED LEAVING HUMRAT SETPOINT:BYPASS
  !                        Work supported by ASHRAE research project 1254-RP
  !                      June 2007 R. Raustad, FSEC
  !                        Added new dehumidifier type -- DESICCANT DEHUMIDIFIER
  !                      Jan 2012  B. Nigusse, FSEC
  !                        Added steam and hot water heating coils

  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and routines required to model desiccant dehumidifier
  ! components in the EnergyPlus HVAC simulation

  ! METHODOLOGY EMPLOYED:
  ! The desiccant dehumidifier emcompasses not just the component but also its
  ! control. The desiccant dehumidifier removes moisture from its air inlet to meet
  ! the HumRatMax setpoint at its exit node. The HumRatMax is set by
  ! an external setpoint manager or is a fixed user input.

  ! REFERENCES: na

  ! OTHER NOTES: This module is based substantially on the Humidifiers module.
  !              authored by Fred Buhl.
  !
  !              Development of portions of this module was funded by the Gas Research Institute.
  !              (Please see copyright and disclaimer information at end of module)

  ! USE STATEMENTS:
  ! Use statements for data only modules
  USE DataPrecisionGlobals
  USE DataGlobals
  USE DataLoopNode
  USE DataEnvironment, ONLY: OutBaroPress,StdRhoAir
  USE DataHVACGlobals, ONLY: SmallMassFlow, OnOffFanPartLoadFraction, ContFanCycCoil, &
                             BlowThru, DrawThru, Coil_HeatingWater, Coil_HeatingSteam, &
                             Coil_HeatingGas, Coil_HeatingElectric
  USE DataHeatBalance, ONLY: HeatReclaimDXCoil
  USE DataInterfaces
  ! Use statements for access to subroutines in other modules
  USE ScheduleManager
  USE HeatingCoils
  USE Fans
  USE CurveManager
  USE Psychrometrics
  USE General, ONLY: TrimSigDigits, RoundSigDigits
  USE FluidProperties,       ONLY: GetSatDensityRefrig

  IMPLICIT NONE         ! Enforce explicit typing of all variables

  PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
  ! Desiccant dehumidifier type
  INTEGER, PARAMETER :: Solid = 1                 ! DESICCANT DEHUMIDIFIER:SOLID = 1
  INTEGER, PARAMETER :: Generic = 2               ! DESICCANT DEHUMIDIFIER = 2
  !  Desiccant heat exchanger type
  INTEGER, PARAMETER :: BalancedHX = 1            ! HeatExchanger:Desiccant:BalancedFlow = 1
  ! Desiccant control type
  INTEGER, PARAMETER :: FixedHumratBypass = 1     ! FIXED LEAVING HUMRAT SETPOINT:BYPASS = 1
  INTEGER, PARAMETER :: NodeHumratBypass  = 2     ! NODE LEAVING HUMRAT SETPOINT:BYPASS  = 2
  ! Preheat selection
  INTEGER, PARAMETER :: No = 0                    ! Condenser waste heat NOT reclaimed for desiccant regeneration
  INTEGER, PARAMETER :: Yes = 1                   ! Condenser waste heat reclaimed for desiccant regeneration
  ! Performance Model
  INTEGER, PARAMETER :: PM_Default = 1            ! Performance Model = default
  INTEGER, PARAMETER :: PM_UserCurves = 2          ! Performance Model = user curve

  ! DERIVED TYPE DEFINITIONS
  TYPE DesiccantDehumidifierData
  ! User Input data
    CHARACTER(len=MaxNameLength) :: Name             =' '  ! unique name of component
    CHARACTER(len=MaxNameLength) :: Sched            =' '  ! name of availability schedule
    CHARACTER(len=MaxNameLength) :: RegenCoilType    =' '  ! type of regen coil
    CHARACTER(len=MaxNameLength) :: RegenCoilName    =' '  ! name of regen coil
    CHARACTER(len=MaxNameLength) :: RegenFanType     =' '  ! type of regen fan
    CHARACTER(len=MaxNameLength) :: RegenFanName     =' '  ! name of regen fan
    INTEGER                      :: PerformanceModel_Num =0 ! type of performance model, default or user curves
    INTEGER                      :: ProcAirInNode    =0    ! process air inlet node of dehumidifier
    INTEGER                      :: ProcAirOutNode   =0    ! process air outlet node of dehumidifier
    INTEGER                      :: RegenAirInNode   =0    ! regen air inlet node of dehumidifier
                                                           ! (initially set to conditions entering regen heating coil)
    INTEGER                      :: RegenAirOutNode  =0    ! regen air outlet node of dehumidifier
    INTEGER                      :: RegenFanInNode   =0    ! regen fan inlet node
    INTEGER                      :: ControlType      =0    ! type of controls
    REAL(r64)                    :: HumRatSet        =0.0d0  ! humidity ratio setpoint [kg water / kg air]
    REAL(r64)                    :: NomProcAirVolFlow=0.0d0  ! nominal process air flow rate [m3/s]
    REAL(r64)                    :: NomProcAirVel    =0.0d0  ! nominal process air velocity [m/s]
    REAL(r64)                    :: NomRotorPower    =0.0d0  ! rotor power consumption at full output [W]
    INTEGER                      :: RegenCoilIndex   =0    ! Index for regen coil
    INTEGER                      :: RegenFanIndex    =0    ! Index for regen fan
    INTEGER                      :: ProcDryBulbCurvefTW=0  ! number of process leaving dry bulb f(edb,ew) curve
    INTEGER                      :: ProcDryBulbCurvefV =0  ! number of process leaving dry bulb f(v) curve
    INTEGER                      :: ProcHumRatCurvefTW =0  ! number of process leaving humidity ratio f(edb,ew) curve
    INTEGER                      :: ProcHumRatCurvefV  =0  ! number of process leaving humidity ratio f(v) curve
    INTEGER                      :: RegenEnergyCurvefTW=0  ! number of regen energy f(edb,ew) curve
    INTEGER                      :: RegenEnergyCurvefV =0  ! number of regen energy f(v) curve
    INTEGER                      :: RegenVelCurvefTW   =0  ! number of regen velocity f(edb,ew) curve
    INTEGER                      :: RegenVelCurvefV    =0  ! number of regen velocity f(v) curve
    REAL(r64)                    :: NomRegenTemp  = 121.0d0  ! nominal regen temperature for regen energy curve [C]

  ! Possible future inputs, hardwired for now depending on which performance model is in use, unit off if out of bounds
    REAL(r64)            :: MinProcAirInTemp   = -73.3d0   ! min allowable process inlet air temperature [C]
    REAL(r64)            :: MaxProcAirInTemp   = 65.6d0    ! max allowable process inlet air temperature [C]
    REAL(r64)            :: MinProcAirInHumRat = 0.0d0     ! min allowable process inlet air humidity ratio [kg water / kg air]
    REAL(r64)            :: MaxProcAirInHumRat = 0.21273d0 ! max allowable process inlet air humidity ratio [kg water / kg air]

  ! Internal Data
    INTEGER              :: SchedPtr              =0   ! index of availability schedule
    REAL(r64)            :: NomProcAirMassFlow    =0.0d0 ! nominal process air mass flow rate [kg/s]
    REAL(r64)            :: NomRegenAirMassFlow   =0.0d0 ! nominal regeneration air mass flow rate [kg/s]
    REAL(r64)            :: ProcAirInTemp         =0.0d0 ! process inlet air temperature [C]
    REAL(r64)            :: ProcAirInHumRat       =0.0d0 ! process inlet air humidity ratio [kg water / kg air]
    REAL(r64)            :: ProcAirInEnthalpy     =0.0d0 ! process inlet air specific enthalpy [J/kg]
    REAL(r64)            :: ProcAirInMassFlowRate =0.0d0 ! process inlet air mass flow rate [kg/s]
    REAL(r64)            :: ProcAirOutTemp        =0.0d0 ! process outlet air temperature [C]
    REAL(r64)            :: ProcAirOutHumRat      =0.0d0 ! process outlet air humidity ratio [kg water / kg air]
    REAL(r64)            :: ProcAirOutEnthalpy    =0.0d0 ! process outlet air specific enthalpy [J/kg]
    REAL(r64)            :: ProcAirOutMassFlowRate=0.0d0 ! process outlet air mass flow rate [kg/s]

    REAL(r64)            :: RegenAirInTemp        =0.0d0 ! regen inlet air temperature [C]
    REAL(r64)            :: RegenAirInHumRat      =0.0d0 ! regen inlet air humidity ratio [kg water / kg air]
    REAL(r64)            :: RegenAirInEnthalpy    =0.0d0 ! regen inlet air specific enthalpy [J/kg]
    REAL(r64)            :: RegenAirInMassFlowRate=0.0d0 ! regen inlet air mass flow rate [kg/s]
    REAL(r64)            :: RegenAirVel           =0.0d0 ! regen air velocity [m/s]

    CHARACTER(len=MaxNameLength) :: DehumType         =' '  ! Type of desiccant dehumidifier
    INTEGER                      :: DehumTypeCode     =0    ! Type of desiccant dehumidifier, integer code
    REAL(r64)                    :: WaterRemove       =0.0d0  ! water removed [kg]
    REAL(r64)                    :: WaterRemoveRate   =0.0d0  ! water removal rate [kg/s]
    REAL(r64)                    :: SpecRegenEnergy   =0.0d0  ! specific regen energy [J/kg of water removed]
    REAL(r64)                    :: QRegen            =0.0d0  ! regen energy rate requested from regen coil [W]
    REAL(r64)                    :: RegenEnergy       =0.0d0  ! regen energy requested from regen coil [J]
    REAL(r64)                    :: ElecUseEnergy     =0.0d0  ! electricity consumption [J]
    REAL(r64)                    :: ElecUseRate       =0.0d0  ! electricity consumption rate [W]
    REAL(r64)                    :: PartLoad          =0.0d0  ! fraction of dehumidification capacity required to meet setpoint
    INTEGER                      :: RegenCapErrorIndex1 =0  ! recurring error message index for insufficient regen coil capacity
    INTEGER                      :: RegenCapErrorIndex2 =0  ! recurring error message index for insufficient regen coil capacity
    INTEGER                      :: RegenCapErrorIndex3 =0  ! recurring error message index for insufficient regen coil capacity
    INTEGER                      :: RegenCapErrorIndex4 =0  ! recurring error message index for insufficient regen coil capacity
    INTEGER                      :: RegenFanErrorIndex1 =0  ! recurring error message index for incorrect regen fan flow
    INTEGER                      :: RegenFanErrorIndex2 =0  ! recurring error message index for incorrect regen fan flow
    INTEGER                      :: RegenFanErrorIndex3 =0  ! recurring error message index for incorrect regen fan flow
    INTEGER                      :: RegenFanErrorIndex4 =0  ! recurring error message index for incorrect regen fan flow

! structure elements unique to generic desiccant dehumidifier
    CHARACTER(len=MaxNameLength) :: HXType              =' '  ! type of desiccant heat exchanger
    CHARACTER(len=MaxNameLength) :: HXName              =' '  ! name of desiccant heat exchanger
    INTEGER                      :: HXTypeNum           = 0   ! parameter number of desiccant heat exchanger
    CHARACTER(len=MaxNameLength) :: ExhaustFanCurveObject=' ' ! exhaust fan curve object
    CHARACTER(len=MaxNameLength) :: CoolingCoilType     =' '  ! type of cooling coil used with desiccant heat exchanger
    CHARACTER(len=MaxNameLength) :: CoolingCoilName     =' '  ! name of cooling coil used with desiccant heat exchanger
    INTEGER                      :: Preheat             =0    ! determine condenser waste heat usage for pre heating regen air
    REAL(r64)                    :: RegenSetPointTemp   =0.0d0  ! heating set-point for regeneration air [C]
    REAL(r64)                    :: ExhaustFanMaxVolFlowRate=0.0d0 ! exhaust fan maximum allowable air flow rate [m3/s]
    REAL(r64)                    :: ExhaustFanMaxMassFlowRate=0.0d0 ! exhaust fan maximum allowable air mass flow rate [kg/s]
    REAL(r64)                    :: ExhaustFanMaxPower  =0.0d0  ! exhaust fan maximum allowable power [W]
    REAL(r64)                    :: ExhaustFanPower     =0.0d0  ! exhaust fan power for reporting [W]
    REAL(r64)                    :: ExhaustFanElecConsumption = 0.0d0 ! exhaust fan electric consumption for reporting [J]
    REAL(r64)                    :: CompanionCoilCapacity=0.0d0 ! DX coil capacity for dehumidifier companion cooling coil [W]
    INTEGER                      :: RegenFanPlacement     =0  ! placement of the fan used for regeneration air flow
    INTEGER                      :: ControlNodeNum        =0  ! node number of control node
    INTEGER                      :: ExhaustFanCurveIndex  =0  ! exhaust fan curve object index
    INTEGER                      :: CompIndex             =0  ! index of HX component to call simheatrecovery
    INTEGER                      :: CoolingCoilOutletNode =0  ! node number of cooling coil outlet node
    INTEGER                      :: RegenFanOutNode       =0  ! fan outlet node number mined from regen fan object
    INTEGER                      :: RegenCoilInletNode    =0  ! regen heating coil inlet node number mined from regen heater object
    INTEGER                      :: RegenCoilOutletNode   =0  ! regen heating coil outlet node number mined from regen heater object
    INTEGER                      :: HXProcInNode          =0  ! process inlet node num mined from desiccant heat exchanger object
    INTEGER                      :: HXProcOutNode         =0  ! process outlet node num mined from desiccant heat exchanger object
    INTEGER                      :: HXRegenInNode         =0  ! regen inlet node number mined from desiccant heat exchanger object
    INTEGER                      :: HXRegenOutNode        =0  ! regen outlet node number mined from desiccant heat exchanger object
    INTEGER                      :: CondenserInletNode    =0  ! regen outlet node number mined from desiccant heat exchanger object
    INTEGER                      :: DXCoilIndex           =0  ! DX Coil index mined from coil object
    INTEGER                      :: ErrCount              =0  ! error count
    INTEGER                      :: ErrIndex1             =0  ! error index
    INTEGER                      :: CoilUpstreamOfProcessSide=0 ! used to determine if process inlet is pre-cooled
    LOGICAL                      :: RegenInletIsOutsideAirNode = .FALSE. ! regen inlet is connected to an outside air node

    INTEGER                      :: RegenCoilType_Num         = 0  ! type number of regen coil
    INTEGER                      :: CoilControlNode           = 0  ! heating coil hot water or steam inlet node
    INTEGER                      :: CoilOutletNode       = 0  ! outlet node for water coil
    INTEGER                      :: LoopNum                   = 0  ! plant loop index for water heating coil
    INTEGER                      :: LoopSide                  = 0  ! plant loop side  index for water heating coil
    INTEGER                      :: BranchNum                 = 0  ! plant loop branch index for water heating coil
    INTEGER                      :: CompNum                   = 0  ! plant loop component index for water heating coil
    Integer                      :: HotWaterCoilMaxIterIndex  = 0  ! Index to recurring warning message
    Integer                      :: HotWaterCoilMaxIterIndex2 = 0  ! Index to recurring warning message
    REAL(r64)                    :: MaxCoilFluidFlow          = 0.0d0  ! hot water or steam mass flow rate regen. heating coil [kg/s]
    REAL(r64)                    :: RegenCoilCapacity         = 0.0d0  ! hot water or steam coil operating capacity [W]

  END TYPE DesiccantDehumidifierData

  ! MODULE VARIABLE DECLARATIONS:
  INTEGER :: NumDesicDehums   ! number of desiccant dehumidifiers of all types
  INTEGER :: NumSolidDesicDehums         ! number of solid desiccant dehumidifiers
  INTEGER :: NumGenericDesicDehums   ! number of generic desiccant dehumidifiers
  TYPE (DesiccantDehumidifierData), ALLOCATABLE, DIMENSION(:) :: DesicDehum
  REAL(r64)                       :: TempSteamIn = 100.0d0            ! steam coil steam inlet temperature

  ! SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>

  ! Name Public routines, optionally name Private routines within this module

  PUBLIC  SimDesiccantDehumidifier
  PRIVATE ControlDesiccantDehumidifier
  PRIVATE GetDesiccantDehumidifierInput
  PRIVATE InitDesiccantDehumidifier
  PRIVATE CalcSolidDesiccantDehumidifier
  PRIVATE CalcGenericDesiccantDehumidifier
  PRIVATE UpdateDesiccantDehumidifier
  PRIVATE ReportDesiccantDehumidifier
  PRIVATE CalcNonDXHeatingCoils


CONTAINS

SUBROUTINE SimDesiccantDehumidifier(CompName,FirstHVACIteration,CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
          !                      for Gas Research Institute
          !       DATE WRITTEN   March 2001
          !       MODIFIED       June 2007, R. Raustad, Added new dehumidifier type -- DESICCANT DEHUMIDIFIER
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Manage the simulation of an air dehumidifier

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! NA

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT (IN)  :: CompName            ! name of the dehumidifier unit
  LOGICAL,          INTENT (IN)  :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep
  INTEGER,          INTENT(INOUT):: CompIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: DesicDehumNum             ! index of solid desiccant unit being simulated
  LOGICAL,SAVE :: GetInputFlag = .true.     ! First time, input is "gotten"
  REAL(r64)    :: HumRatNeeded              ! process air leaving humidity ratio set by controller [kg water/kg air]


  IF (GetInputFlag) THEN
    CALL GetDesiccantDehumidifierInput
    GetInputFlag=.false.
  ENDIF

  ! Get the desiccant dehumidifier unit index
  IF (CompIndex == 0) THEN
    DesicDehumNum = FindItemInList(CompName,DesicDehum%Name,NumDesicDehums)
    IF (DesicDehumNum == 0) THEN
      CALL ShowFatalError('SimDesiccantDehumidifier: Unit not found='//TRIM(CompName))
    ENDIF
    CompIndex=DesicDehumNum
  ELSE
    DesicDehumNum=CompIndex
    IF (DesicDehumNum > NumDesicDehums .or. DesicDehumNum < 1) THEN
      CALL ShowFatalError('SimDesiccantDehumidifier:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(DesicDehumNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumDesicDehums))//  &
                          ', Entered Unit name='//TRIM(CompName))
    ENDIF
    IF (CompName /= DesicDehum(DesicDehumNum)%Name) THEN
      CALL ShowFatalError('SimDesiccantDehumidifier: Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(DesicDehumNum))// &
                          ', Unit name='//TRIM(CompName)//', stored Unit Name for that index='//  &
                          TRIM(DesicDehum(DesicDehumNum)%Name))
    ENDIF
  ENDIF

  CALL InitDesiccantDehumidifier(DesicDehumNum,FirstHVACIteration)

  CALL ControlDesiccantDehumidifier(DesicDehumNum,HumRatNeeded,FirstHVACIteration)

  ! call the correct dehumidifier calculation routine
  SELECT CASE(DesicDehum(DesicDehumNum)%DehumTypeCode)

    CASE (Solid)

      CALL CalcSolidDesiccantDehumidifier(DesicDehumNum,HumRatNeeded,FirstHVACIteration)

    CASE (Generic)

      CALL CalcGenericDesiccantDehumidifier(DesicDehumNum,HumRatNeeded,FirstHVACIteration)

    CASE DEFAULT
      CALL ShowFatalError('Invalid type, Desiccant Dehumidifer='//TRIM(DesicDehum(DesicDehumNum)%DehumType))

  END SELECT

  CALL UpdateDesiccantDehumidifier(DesicDehumNum)

  CALL ReportDesiccantDehumidifier(DesicDehumNum)

  RETURN

END SUBROUTINE SimDesiccantDehumidifier

SUBROUTINE GetDesiccantDehumidifierInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
          !                      for Gas Research Institute
          !       DATE WRITTEN   March 2001
          !       MODIFIED       Jan 2005 M. J. Witte, GARD Analytics, Inc.
          !                        Add new control type option:
          !                          NODE LEAVING HUMRAT SETPOINT:BYPASS
          !                        Change existing control type to:
          !                          FIXED LEAVING HUMRAT SETPOINT:BYPASS
          !                        Work supported by ASHRAE research project 1254-RP
          !                      June 2007 R. Raustad, FSEC
          !                        Added new dehumidifier type -- DESICCANT DEHUMIDIFIER
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for humidifiers and stores it in dehumidifier data structures.

          ! METHODOLOGY EMPLOYED:
          ! Uses InputProcessor "Get" routines to obtain data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,    ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString, GetObjectDefMaxArgs
  USE NodeInputManager,  ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: SetUpCompSets, TestCompSet
  USE DXCoils,           ONLY: GetDXCoilOutletNode=>GetCoilOutletNode,GetCoilCondenserInletNode, GetDXCoilBypassedFlowFrac, &
                               GetDXCoilIndex, GetDXCoilCapacity=>GetCoilCapacity
  USE HeatRecovery,      ONLY: GetSupplyOutletNode, GetSupplyInletNode, &
                               GetSecondaryInletNode, GetSecondaryOutletNode
  USE HeatingCoils,      ONLY: GetHeatingCoilInletNode=>GetCoilInletNode, GetHeatingCoilOutletNode=>GetCoilOutletNode, &
                               GetHeatReclaimSourceIndexNum=>GetHeatReclaimSourceIndex, GetHeatingCoilIndex=>GetCoilIndex, &
                               GetHeatingCoilControlNodeNum=>GetCoilControlNodeNum
  USE WaterCoils,        ONLY: GetCoilWaterInletNode, GetCoilMaxWaterFlowRate, GetWaterCoilIndex, &
                               GetWaterCoilInletNode=>GetCoilInletNode,GetWaterCoilOutletNode=>GetCoilOutletNode
  USE SteamCoils,        ONLY: GetSteamCoilAirInletNode=>GetCoilAirInletNode, GetSteamCoilIndex, &
                               GetSteamCoilAirOutletNode=>GetCoilAirOutletNode, &
                               GetSteamCoilSteamInletNode=>GetCoilSteamInletNode, &
                               GetCoilMaxSteamFlowRate=>GetCoilMaxSteamFlowRate, GetTypeOfCoil, ZoneLoadControl, &
                               GetSteamCoilControlNodeNum
  USE OutAirNodeManager, ONLY: CheckOutAirNodeNumber, CheckAndAddAirNodeNumber
  USE DataIPShortCuts

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER (len=*), PARAMETER   :: RoutineName='GetDesiccantDehumidifierInput: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                        :: DesicDehumIndex         ! Loop index
  INTEGER                        :: DesicDehumNum           ! Current desiccant dehumidifier number
  INTEGER                        :: NumAlphas               ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers              ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: IOStatus                ! Used in GetObjectItem
  LOGICAL                        :: ErrorsFound=.false.     ! Set to true if errors in input, fatal at end of routine
  LOGICAL                        :: ErrorsFound2=.false.    ! Set to true if errors in input, fatal at end of routine
  LOGICAL                        :: ErrorsFoundGeneric=.false. ! Set to true if errors in input, fatal at end of routine
  LOGICAL                        :: IsNotOK                 ! Flag to verify name
  LOGICAL                        :: IsBlank                 ! Flag for blank name
  LOGICAL                        :: OANodeError             ! Flag for check on outside air node
  CHARACTER (len=MaxNameLength)  :: RegenFanInlet           ! Desiccant system regeneration air fan inlet node
  CHARACTER (len=MaxNameLength)  :: RegenFanOutlet          ! Desiccant system regeneration air fan outlet node
  CHARACTER (len=MaxNameLength)  :: RegenCoilInlet          ! Desiccant system regeneration air heater inlet node
  CHARACTER (len=MaxNameLength)  :: RegenCoilOutlet         ! Desiccant system regeneration air heater outlet node
  CHARACTER (len=MaxNameLength)  :: ProcAirInlet            ! HX process air inlet node
  CHARACTER (len=MaxNameLength)  :: ProcAirOutlet           ! HX process air outlet node
  CHARACTER (len=MaxNameLength)  :: RegenAirInlet           ! HX regeneration air inlet node
  CHARACTER (len=MaxNameLength)  :: RegenAirOutlet          ! HX regeneration air outlet node
  CHARACTER (len=MaxNameLength)  :: CurrentModuleObject     ! for ease in getting objects
  INTEGER                        :: DesuperHeaterIndex      ! Index of desuperheater heating coil
  INTEGER                        :: RegenCoilControlNodeNum ! Control node number of regen heating coil
  REAL(r64)                      :: CoilBypassedFlowFrac    ! Bypass air fraction for multimode DX coils
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Alphas         ! Alpha input items for object
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAlphaFields   ! Alpha field names
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cNumericFields ! Numeric field names
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: Numbers           ! Numeric input items for object
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lAlphaBlanks      ! Logical array, alpha field input BLANK = .true.
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: lNumericBlanks    ! Logical array, numeric field input BLANK = .true.
  INTEGER                        :: MaxNums=0               ! Maximum number of numeric input fields
  INTEGER                        :: MaxAlphas=0             ! Maximum number of alpha input fields
  INTEGER                        :: TotalArgs=0             ! Total number of alpha and numeric arguments (max) for a
                                                            !  certain object in the input file
  INTEGER                        :: RegenCoilAirInletNode   ! regen heating coil air inlet node number
  INTEGER                        :: RegenCoilAirOutletNode  ! regen heating coil air outlet node number
  LOGICAL                        :: ErrFlag                 ! local error flag
  CHARACTER(len=MaxNameLength)   :: RegenCoilType           ! Regen heating coil type
  CHARACTER(len=MaxNameLength)   :: RegenCoilName           ! Regen heating coil name
  REAL(r64)                      :: SteamDensity  = 0.0d0     ! density of steam at 100C
  INTEGER                        :: SteamIndex              ! steam coil Index

  NumSolidDesicDehums = GetNumObjectsFound('Dehumidifier:Desiccant:NoFans')
  NumGenericDesicDehums = GetNumObjectsFound('Dehumidifier:Desiccant:System')
  NumDesicDehums = NumSolidDesicDehums + NumGenericDesicDehums
  ! allocate the data array
  ALLOCATE(DesicDehum(NumDesicDehums))

  CALL GetObjectDefMaxArgs('Dehumidifier:Desiccant:NoFans',TotalArgs,NumAlphas,NumNumbers)
  MaxNums=MAX(MaxNums,NumNumbers)
  MaxAlphas=MAX(MaxAlphas,NumAlphas)
  CALL GetObjectDefMaxArgs('Dehumidifier:Desiccant:System',TotalArgs,NumAlphas,NumNumbers)
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

  ! loop over solid desiccant dehumidifiers and load the input data
  CurrentModuleObject = 'Dehumidifier:Desiccant:NoFans'
  DO DesicDehumIndex = 1,NumSolidDesicDehums
    RegenCoilAirInletNode = 0
    RegenCoilAirOutletNode = 0
    CALL GetObjectItem(CurrentModuleObject,DesicDehumIndex,Alphas,NumAlphas,Numbers,NumNumbers,IOStatus, &
                       NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

    DesicDehumNum = DesicDehumIndex
    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(Alphas(1),DesicDehum%Name,DesicDehumNum-1,IsNotOK,IsBlank,TRIM(CurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    DesicDehum(DesicDehumNum)%Name = Alphas(1)
    DesicDehum(DesicDehumNum)%DehumType = TRIM(CurrentModuleObject)
    DesicDehum(DesicDehumNum)%DehumTypeCode = Solid
    DesicDehum(DesicDehumNum)%Sched = Alphas(2)
    IF (lAlphaBlanks(2)) THEN
      DesicDehum(DesicDehumNum)%SchedPtr = ScheduleAlwaysOn
    ELSE
      DesicDehum(DesicDehumNum)%SchedPtr = GetScheduleIndex(Alphas(2))  ! convert schedule name to pointer
      IF (DesicDehum(DesicDehumNum)%SchedPtr .EQ. 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': invalid '//TRIM(cAlphaFields(2))//  &
                             ' entered ='//TRIM(Alphas(2))// &
                             ' for '//TRIM(cAlphaFields(1))//'='//TRIM(Alphas(1)))
        ErrorsFound=.true.
      END IF
    END IF
          ! For node connections, this object is both a parent and a non-parent, because the
          ! Desiccant wheel is not called out as a separate component, its nodes must be connected
          ! as ObjectIsNotParent.  But for the Regen fan, the nodes are connected as ObjectIsParent
    DesicDehum(DesicDehumNum)%ProcAirInNode = &
               GetOnlySingleNode(Alphas(3),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)

    DesicDehum(DesicDehumNum)%ProcAirOutNode = &
               GetOnlySingleNode(Alphas(4),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)

    DesicDehum(DesicDehumNum)%RegenAirInNode = &
               GetOnlySingleNode(Alphas(5),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Air,NodeConnectionType_Inlet,2,ObjectIsNotParent)

    DesicDehum(DesicDehumNum)%RegenFanInNode = &
               GetOnlySingleNode(Alphas(6),ErrorsFound,TRIM(CurrentModuleObject),Alphas(1), &
               NodeType_Air,NodeConnectionType_Internal,2,ObjectIsParent)

    IF (SameString(Alphas(7),'LEAVING HUMRAT:BYPASS')) THEN
      CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(DesicDehum(DesicDehumNum)%Name))
      CALL ShowContinueError('Obsolete '//TRIM(cAlphaFields(7))//' = '//TRIM(Alphas(7)))
      CALL ShowContinueError('setting to LeavingMaximumHumidityRatioSetpoint')
      DesicDehum(DesicDehumNum)%ControlType      = FixedHumratBypass
    END IF
    IF (SameString(Alphas(7),'LeavingMaximumHumidityRatioSetpoint'))  &
               DesicDehum(DesicDehumNum)%ControlType = FixedHumratBypass
    IF (SameString(Alphas(7),'SystemNodeMaximumHumidityRatioSetpoint'))  &
               DesicDehum(DesicDehumNum)%ControlType = NodeHumratBypass
    IF (DesicDehum(DesicDehumNum)%ControlType == 0) THEN
      CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(DesicDehum(DesicDehumNum)%Name))
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFields(7))//' = '//TRIM(Alphas(7)))
      CALL ShowContinueError('setting to LeavingMaximumHumidityRatioSetpoint')
      DesicDehum(DesicDehumNum)%ControlType      = FixedHumratBypass
    END IF
    DesicDehum(DesicDehumNum)%HumRatSet          = Numbers(1)
    DesicDehum(DesicDehumNum)%NomProcAirVolFlow  = Numbers(2)
    DesicDehum(DesicDehumNum)%NomProcAirVel      = Numbers(3)

    DesicDehum(DesicDehumNum)%RegenCoilType      = Alphas(8)
    DesicDehum(DesicDehumNum)%RegenCoilName      = Alphas(9)
    RegenCoilType = Alphas(8)
    RegenCoilName = Alphas(9)

    IF (SameString(DesicDehum(DesicDehumNum)%RegenCoilType,'Coil:Heating:Electric') .OR. &
        SameString(DesicDehum(DesicDehumNum)%RegenCoilType,'Coil:Heating:Gas')) THEN
      IF (SameString(DesicDehum(DesicDehumNum)%RegenCoilType,'Coil:Heating:Electric')) &
                        DesicDehum(DesicDehumNum)%RegenCoilType_Num = Coil_HeatingElectric
      IF (SameString(DesicDehum(DesicDehumNum)%RegenCoilType,'Coil:Heating:Gas')) &
                        DesicDehum(DesicDehumNum)%RegenCoilType_Num = Coil_HeatingGas
      CALL ValidateComponent(DesicDehum(DesicDehumNum)%RegenCoilType,DesicDehum(DesicDehumNum)%RegenCoilName,  &
                             ErrorsFound2,TRIM(CurrentModuleObject)//'='//TRIM(Alphas(1)))
      IF (ErrorsFound2) ErrorsFound = .TRUE.
      CALL GetHeatingCoilIndex(DesicDehum(DesicDehumNum)%RegenCoilName,DesicDehum(DesicDehumNum)%RegenCoilIndex,&
                        ErrorsFound2)
      IF (ErrorsFound2) ErrorsFound = .TRUE.

    ELSEIF (SameString(DesicDehum(DesicDehumNum)%RegenCoilType,'Coil:Heating:Water')) THEN
      DesicDehum(DesicDehumNum)%RegenCoilType_Num = Coil_HeatingWater
      CALL ValidateComponent(RegenCoilType,RegenCoilName,IsNotOK,TRIM(CurrentModuleObject))
      IF (IsNotOK) THEN
          CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          ErrorsFound=.TRUE.
      ELSE ! mine data from heating coil object
          ErrFlag = .FALSE.
          DesicDehum(DesicDehumNum)%RegenCoilIndex = GetWaterCoilIndex('COIL:HEATING:WATER',RegenCoilName,ErrFlag)
          IF (DesicDehum(DesicDehumNum)%RegenCoilIndex .EQ. 0) THEN
              CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(9))//' = ' &
                              //TRIM(RegenCoilName))
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(DesicDehum(DesicDehumNum)%Name))
              ErrorsFound = .TRUE.
          END IF

           ! Get the Heating Coil Hot water Inlet or control Node number
          ErrFlag = .FALSE.
          DesicDehum(DesicDehumNum)%CoilControlNode = GetCoilWaterInletNode('Coil:Heating:Water', &
                                                         RegenCoilName,ErrFlag)
          IF(ErrFlag)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(DesicDehum(DesicDehumNum)%Name))
              ErrorsFound = .TRUE.
          END IF

          ! Get the Regeneration Heating Coil hot water max volume flow rate
          ErrFlag = .FALSE.
          DesicDehum(DesicDehumNum)%MaxCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                       RegenCoilName,ErrFlag)
          IF(ErrFlag)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(DesicDehum(DesicDehumNum)%Name))
              ErrorsFound = .TRUE.
          END IF

          ! Get the Regeneration Heating Coil Inlet Node
          ErrFlag = .FALSE.
          RegenCoilAirInletNode =  GetWaterCoilInletNode('Coil:Heating:Water',RegenCoilName,ErrFlag)
          DesicDehum(DesicDehumNum)%RegenCoilInletNode = RegenCoilAirInletNode
          IF(ErrFlag)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(DesicDehum(DesicDehumNum)%Name))
              ErrorsFound = .TRUE.
          END IF

          ! Get the Regeneration Heating Coil Outlet Node
          ErrFlag = .FALSE.
          RegenCoilAirOutletNode = GetWaterCoilOutletNode('Coil:Heating:Water',RegenCoilName,ErrFlag)
          DesicDehum(DesicDehumNum)%RegenCoilOutletNode = RegenCoilAirOutletNode
          IF(ErrFlag)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(DesicDehum(DesicDehumNum)%Name))
              ErrorsFound = .TRUE.
          END IF

        ENDIF
    ELSEIF (SameString(DesicDehum(DesicDehumNum)%RegenCoilType,'Coil:Heating:Steam')) THEN
        DesicDehum(DesicDehumNum)%RegenCoilType_Num = Coil_HeatingSteam
        CALL ValidateComponent(Alphas(8),RegenCoilName,IsNotOK,TRIM(CurrentModuleObject))
        IF (IsNotOK) THEN
             CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
             ErrorsFound=.TRUE.
        ELSE ! mine data from the regeneration heating coil object

            ErrFlag = .FALSE.
            DesicDehum(DesicDehumNum)%RegenCoilIndex = GetSTeamCoilIndex('COIL:HEATING:STEAM',RegenCoilName,ErrFlag)
            IF (DesicDehum(DesicDehumNum)%RegenCoilIndex .EQ. 0) THEN
                CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(9))//' = ' &
                                //TRIM(RegenCoilName))
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(DesicDehum(DesicDehumNum)%Name))
                ErrorsFound = .TRUE.
            END IF

            ! Get the regeneration Heating Coil steam inlet node number
            ErrFlag = .FALSE.
            DesicDehum(DesicDehumNum)%CoilControlNode = GetSteamCoilSteamInletNode('Coil:Heating:Steam',RegenCoilName,ErrFlag)
            IF(ErrFlag)THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(DesicDehum(DesicDehumNum)%Name))
                ErrorsFound = .TRUE.
            END IF

            ! Get the regeneration heating Coil steam max volume flow rate
            DesicDehum(DesicDehumNum)%MaxCoilFluidFlow = &
                                    GetCoilMaxSteamFlowRate(DesicDehum(DesicDehumNum)%RegenCoilIndex,ErrFlag)
            IF (DesicDehum(DesicDehumNum)%MaxCoilFluidFlow .GT. 0.0d0)THEN
                SteamIndex = 0      ! Function GetSatDensityRefrig will look up steam index if 0 is passed
                SteamDensity=GetSatDensityRefrig("STEAM",TempSteamIn,1.0d0,SteamIndex,'Dehumidifier:Desiccant:NoFans')
                DesicDehum(DesicDehumNum)%MaxCoilFluidFlow = DesicDehum(DesicDehumNum)%MaxCoilFluidFlow * SteamDensity
            END IF

            ! Get the regeneration heating Coil Inlet Node
            ErrFlag = .FALSE.
            RegenCoilAirInletNode = &
                        GetSteamCoilAirInletNode(DesicDehum(DesicDehumNum)%RegenCoilIndex,RegenCoilName,ErrFlag)
            DesicDehum(DesicDehumNum)%RegenCoilInletNode = RegenCoilAirInletNode
            IF(ErrFlag)THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(DesicDehum(DesicDehumNum)%Name))
                ErrorsFound = .TRUE.
            END IF

            ! Get the regeneration heating Coil Outlet Node
            ErrFlag = .FALSE.
            RegenCoilAirOutletNode = &
                        GetSteamCoilAirOutletNode(DesicDehum(DesicDehumNum)%RegenCoilIndex,RegenCoilName,ErrFlag)
            DesicDehum(DesicDehumNum)%RegenCoilOutletNode = RegenCoilAirOutletNode
            IF(ErrFlag)THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(DesicDehum(DesicDehumNum)%Name))
                ErrorsFound = .TRUE.
            END IF

        ENDIF
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
      CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(8))//' = '//TRIM(DesicDehum(DesicDehumNum)%RegenCoilType))
      ErrorsFound = .TRUE.
    ENDIF

    DesicDehum(DesicDehumNum)%NomRotorPower      = Numbers(4)
    DesicDehum(DesicDehumNum)%RegenFanType       = Alphas(10)
    DesicDehum(DesicDehumNum)%RegenFanName       = Alphas(11)

    CALL TestCompSet(DesicDehum(DesicDehumNum)%DehumType,DesicDehum(DesicDehumNum)%Name, &
                     Alphas(3), Alphas(4),'Process Air Nodes')

    ! Set up component set for regen coil
    CALL SetUpCompSets(DesicDehum(DesicDehumNum)%DehumType, DesicDehum(DesicDehumNum)%Name, &
                       Alphas(8),Alphas(9),'UNDEFINED','UNDEFINED')

    ! Set up component set for regen fan
    CALL SetUpCompSets(DesicDehum(DesicDehumNum)%DehumType, DesicDehum(DesicDehumNum)%Name, &
                       Alphas(10),Alphas(11),Alphas(6),'UNDEFINED')

    IF ((.not. SameString(Alphas(12),'Default'))&
        .AND. (SameString(Alphas(12),'UserCurves')) ) THEN
      CALL ShowWarningError(RoutineName//TRIM(CurrentModuleObject)//': Invalid'//TRIM(cAlphaFields(12))//' = '//TRIM(Alphas(12)))
      CALL ShowContinueError('resetting to Default')
      DesicDehum(DesicDehumNum)%PerformanceModel_Num = PM_Default
    END IF

    IF (SameString(Alphas(12),'UserCurves')) THEN
        DesicDehum(DesicDehumNum)%PerformanceModel_Num = PM_UserCurves
        DesicDehum(DesicDehumNum)%ProcDryBulbCurvefTW= GetCurveIndex(Alphas(13))
        IF (DesicDehum(DesicDehumNum)%ProcDryBulbCurvefTW .EQ. 0) THEN
          CALL ShowSevereError(RoutineName//'Curve object='//TRIM(Alphas(13))//' not found.')
          ErrorsFound2 = .TRUE.
        ENDIF
        DesicDehum(DesicDehumNum)%ProcDryBulbCurvefV = GetCurveIndex(Alphas(14))
        IF (DesicDehum(DesicDehumNum)%ProcDryBulbCurvefV .EQ. 0) THEN
          CALL ShowSevereError(RoutineName//'Curve object='//TRIM(Alphas(14))//' not found.')
          ErrorsFound2 = .TRUE.
        ENDIF
        DesicDehum(DesicDehumNum)%ProcHumRatCurvefTW = GetCurveIndex(Alphas(15))
        IF (DesicDehum(DesicDehumNum)%ProcHumRatCurvefTW .EQ. 0) THEN
          CALL ShowSevereError(RoutineName//'Curve object='//TRIM(Alphas(15))//' not found.')
          ErrorsFound2 = .TRUE.
        ENDIF
        DesicDehum(DesicDehumNum)%ProcHumRatCurvefV  = GetCurveIndex(Alphas(16))
        IF (DesicDehum(DesicDehumNum)%ProcHumRatCurvefV .EQ. 0) THEN
          CALL ShowSevereError(RoutineName//'Curve object='//TRIM(Alphas(16))//' not found.')
          ErrorsFound2 = .TRUE.
        ENDIF
        DesicDehum(DesicDehumNum)%RegenEnergyCurvefTW= GetCurveIndex(Alphas(17))
        IF (DesicDehum(DesicDehumNum)%RegenEnergyCurvefTW .EQ. 0) THEN
          CALL ShowSevereError(RoutineName//'Curve object='//TRIM(Alphas(17))//' not found.')
          ErrorsFound2 = .TRUE.
        ENDIF
        DesicDehum(DesicDehumNum)%RegenEnergyCurvefV = GetCurveIndex(Alphas(18))
        IF (DesicDehum(DesicDehumNum)%RegenEnergyCurvefV .EQ. 0) THEN
          CALL ShowSevereError(RoutineName//'Curve object='//TRIM(Alphas(18))//' not found.')
          ErrorsFound2 = .TRUE.
        ENDIF
        DesicDehum(DesicDehumNum)%RegenVelCurvefTW   = GetCurveIndex(Alphas(19))
        IF (DesicDehum(DesicDehumNum)%RegenVelCurvefTW .EQ. 0) THEN
          CALL ShowSevereError(RoutineName//'Curve object='//TRIM(Alphas(19))//' not found.')
          ErrorsFound2 = .TRUE.
        ENDIF
        DesicDehum(DesicDehumNum)%RegenVelCurvefV    = GetCurveIndex(Alphas(20))
        IF (DesicDehum(DesicDehumNum)%RegenVelCurvefV .EQ. 0) THEN
          CALL ShowSevereError(RoutineName//'Curve object='//TRIM(Alphas(20))//' not found.')
          ErrorsFound2 = .TRUE.
        ENDIF
        IF (ErrorsFound2) THEN
          CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Errors found in getting performance curves.')
          ErrorsFound = .TRUE.
        END IF
        DesicDehum(DesicDehumNum)%NomRegenTemp      = Numbers(5)
           ! Validate regen fan type, for user defined curves, can be constant or variable volume
        IF ((SameString(DesicDehum(DesicDehumNum)%RegenFanType,'FAN:CONSTANTVOLUME')) .OR. &
            (SameString(DesicDehum(DesicDehumNum)%RegenFanType,'FAN:VARIABLEVOLUME'))) THEN
          CALL ValidateComponent(DesicDehum(DesicDehumNum)%RegenFanType,DesicDehum(DesicDehumNum)%RegenFanName,  &
                                 ErrorsFound2,TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          IF (ErrorsFound2) ErrorsFound = .TRUE.
        ELSE
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(10))//' = '//TRIM(DesicDehum(DesicDehumNum)%RegenFanType))
          ErrorsFound = .TRUE.
        ENDIF
    ELSE
           ! If DEFAULT performance model, set operating limits curves.  Unit is off outside this range
      DesicDehum(DesicDehumNum)%PerformanceModel_Num = PM_Default
      DesicDehum%MinProcAirInTemp        =  1.67d0     !  35 F
      DesicDehum%MaxProcAirInTemp        = 48.89d0     ! 120 F
      DesicDehum%MinProcAirInHumRat      =  0.002857d0 !  20 gr/lb
      DesicDehum%MaxProcAirInHumRat      =  0.02857d0  ! 200 gr/lb
           !  If DEFAULT performance model, warn if curve names and nominal regen temp have values
      IF ((.NOT. lAlphaBlanks(13)) .OR. (.NOT. lAlphaBlanks(14)) .OR. (.NOT. lAlphaBlanks(15)) .OR. &
          (.NOT. lAlphaBlanks(16)) .OR. (.NOT. lAlphaBlanks(17)) .OR. (.NOT. lAlphaBlanks(18)) .OR. &
          (.NOT. lAlphaBlanks(19)) .OR. (.NOT. lAlphaBlanks(20))) THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
        CALL ShowContinueError('DEFAULT performance selected, curve names and nominal regen temp will be ignored.')
      ENDIF
      IF (DesicDehum(DesicDehumNum)%NomProcAirVel > 4.064d0) THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
        CALL ShowContinueError(TRIM(cNumericFields(3))//' > 4.064 m/s.; Value in input='//  &
                          TRIM(RoundSigDigits(DesicDehum(DesicDehumNum)%NomProcAirVel,3)))
        CALL ShowContinueError('DEFAULT performance curves not valid outside 2.032 to 4.064 m/s (400 to 800 fpm).')
      ENDIF
      IF (DesicDehum(DesicDehumNum)%NomProcAirVel < 2.032d0) THEN
        CALL ShowWarningError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
        CALL ShowContinueError(TRIM(cNumericFields(3))//' < 2.032 m/s.; Value in input='//  &
                          TRIM(RoundSigDigits(DesicDehum(DesicDehumNum)%NomProcAirVel,3)))
        CALL ShowContinueError('DEFAULT performance curves not valid outside 2.032 to 4.064 m/s (400 to 800 fpm).')
      ENDIF
           ! Validate regen fan type, for default curves, can only variable volume
        IF (DesicDehum(DesicDehumNum)%RegenFanType == 'FAN:VARIABLEVOLUME') THEN
          CALL ValidateComponent(DesicDehum(DesicDehumNum)%RegenFanType,DesicDehum(DesicDehumNum)%RegenFanName,  &
                                 ErrorsFound2,TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          IF (ErrorsFound2) ErrorsFound = .TRUE.
        ELSE
          CALL ShowSevereError(TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(10))//' = '//TRIM(DesicDehum(DesicDehumNum)%RegenFanType))
          CALL ShowContinueError('For DEFAULT performance model, the regen fan type must be Fan:VariableVolume')
          ErrorsFound = .TRUE.
        ENDIF
    ENDIF
  ENDDO

  DO DesicDehumIndex = 1,NumGenericDesicDehums
    RegenCoilAirInletNode = 0
    RegenCoilAirOutletNode = 0

    CurrentModuleObject = 'Dehumidifier:Desiccant:System'

    DesicDehumNum = DesicDehumIndex + NumSolidDesicDehums
    DesicDehum(DesicDehumNum)%DehumType = TRIM(CurrentModuleObject)
    DesicDehum(DesicDehumNum)%DehumTypeCode = Generic
    CALL GetObjectItem(DesicDehum(DesicDehumNum)%DehumType,DesicDehumIndex,Alphas,NumAlphas,Numbers,NumNumbers, &
                       IOStatus,NumBlank=lNumericBlanks,AlphaBlank=lAlphaBlanks, &
                       AlphaFieldNames=cAlphaFields,NumericFieldNames=cNumericFields)

    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(Alphas(1),DesicDehum%Name,DesicDehumNum-1,IsNotOK,IsBlank,DesicDehum(DesicDehumNum)%DehumType//' Name')

    IF (IsNotOK) THEN
      ErrorsFoundGeneric=.true.
      IF (IsBlank) Alphas(1)='xxxxx'
    ENDIF
    DesicDehum(DesicDehumNum)%Name = Alphas(1)

    ErrorsFound2 = .FALSE.
    CALL ValidateComponent(DesicDehum(DesicDehumNum)%DehumType,DesicDehum(DesicDehumNum)%Name, &
                             ErrorsFound2,DesicDehum(DesicDehumNum)%DehumType//' = "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
    IF (ErrorsFound2)  THEN
      CALL ShowSevereError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "' &
                         //TRIM(DesicDehum(DesicDehumNum)%Name)//'" is not unique')
      ErrorsFoundGeneric=.TRUE.
    ENDIF

    DesicDehum(DesicDehumNum)%Sched = Alphas(2)
    IF (lAlphaBlanks(2)) THEN
      DesicDehum(DesicDehumNum)%SchedPtr = ScheduleAlwaysOn
    ELSE
      DesicDehum(DesicDehumNum)%SchedPtr = GetScheduleIndex(Alphas(2))  ! convert schedule name to pointer
      IF (DesicDehum(DesicDehumNum)%SchedPtr .EQ. 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(CurrentModuleObject)//': invalid '//TRIM(cAlphaFields(2))//  &
                             ' entered ='//TRIM(Alphas(2))// &
                             ' for '//TRIM(cAlphaFields(1))//'='//TRIM(Alphas(1)))
        ErrorsFound=.true.
      END IF
    END IF

    DesicDehum(DesicDehumNum)%HXType = Alphas(3)
    DesicDehum(DesicDehumNum)%HXName = Alphas(4)

    IF (.NOT. SameString(DesicDehum(DesicDehumNum)%HXType,'HeatExchanger:Desiccant:BalancedFlow')) THEN
      CALL ShowWarningError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' = "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
      CALL ShowContinueError('Invalid '//TRIM(cAlphaFields(3))//' = '//TRIM(DesicDehum(DesicDehumNum)%HXType))
      ErrorsFoundGeneric=.TRUE.
    ELSE
      DesicDehum(DesicDehumNum)%HXTypeNum = BalancedHX
    END IF

    ErrorsFound2 = .FALSE.
    CALL ValidateComponent(DesicDehum(DesicDehumNum)%HXType,DesicDehum(DesicDehumNum)%HXName, &
                           ErrorsFound2,DesicDehum(DesicDehumNum)%DehumType//' = "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
    IF (ErrorsFound2) ErrorsFoundGeneric = .TRUE.

    ErrorsFound2 = .FALSE.
    DesicDehum(DesicDehumNum)%HXProcInNode = &
                    GetSecondaryInletNode(DesicDehum(DesicDehumNum)%HXName, ErrorsFound2)
    IF(ErrorsFound2)THEN
      CALL ShowContinueError('...occurs in '//TRIM(DesicDehum(DesicDehumNum)%DehumType)// &
                                       ' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
      ErrorsFoundGeneric=.TRUE.
    END IF

    ProcAirInlet = NodeID(DesicDehum(DesicDehumNum)%HXProcInNode)

    DesicDehum(DesicDehumNum)%ProcAirInNode = GetOnlySingleNode(ProcAirInlet,ErrorsFound,&
                   DesicDehum(DesicDehumNum)%DehumType,DesicDehum(DesicDehumNum)%Name, &
                   NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)

    ErrorsFound2 = .FALSE.
    DesicDehum(DesicDehumNum)%HXProcOutNode = &
                    GetSecondaryOutletNode(DesicDehum(DesicDehumNum)%HXName, ErrorsFound2)
    IF(ErrorsFound2)THEN
      CALL ShowContinueError('...occurs in '//TRIM(DesicDehum(DesicDehumNum)%DehumType)// &
                                       ' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
      ErrorsFoundGeneric=.TRUE.
    END IF

    ProcAirOutlet = NodeID(DesicDehum(DesicDehumNum)%HXProcOutNode)

    DesicDehum(DesicDehumNum)%ProcAirOutNode = GetOnlySingleNode(ProcAirOutlet,ErrorsFound,&
                   DesicDehum(DesicDehumNum)%DehumType,DesicDehum(DesicDehumNum)%Name, &
                   NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)

    CALL TestCompSet(DesicDehum(DesicDehumNum)%DehumType,DesicDehum(DesicDehumNum)%Name, &
                     ProcAirInlet,ProcAirOutlet,'Process Air Nodes')

    ErrorsFound2 = .FALSE.
    DesicDehum(DesicDehumNum)%HXRegenInNode = &
                    GetSupplyInletNode(DesicDehum(DesicDehumNum)%HXName, ErrorsFound2)
    IF(ErrorsFound2)THEN
      CALL ShowContinueError('...occurs in '//TRIM(DesicDehum(DesicDehumNum)%DehumType)// &
                                       ' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
      ErrorsFoundGeneric=.TRUE.
    END IF

    ErrorsFound2 = .FALSE.
    DesicDehum(DesicDehumNum)%HXRegenOutNode = &
                    GetSupplyOutletNode(DesicDehum(DesicDehumNum)%HXName, ErrorsFound2)
    IF(ErrorsFound2)THEN
      CALL ShowContinueError('...occurs in '//TRIM(DesicDehum(DesicDehumNum)%DehumType)// &
                                       ' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
      ErrorsFoundGeneric=.TRUE.
    END IF

    DesicDehum(DesicDehumNum)%ControlNodeNum =        &
        GetOnlySingleNode(Alphas(5),ErrorsFound,DesicDehum(DesicDehumNum)%DehumType,DesicDehum(DesicDehumNum)%Name, &
        NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)

    IF (DesicDehum(DesicDehumNum)%ControlNodeNum .EQ. 0) THEN
      CALL ShowContinueError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' = "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
      CALL ShowSevereError(TRIM(cAlphaFields(5))//' must be specified.')
      ErrorsFoundGeneric=.TRUE.
    ENDIF


    DesicDehum(DesicDehumNum)%RegenFanType = Alphas(6)
    DesicDehum(DesicDehumNum)%RegenFanName = Alphas(7)

    IF (SameString(DesicDehum(DesicDehumNum)%RegenFanType,'Fan:OnOff') .OR. &
           SameString(DesicDehum(DesicDehumNum)%RegenFanType,'Fan:ConstantVolume')) THEN
      ErrorsFound2 = .FALSE.
      CALL ValidateComponent(DesicDehum(DesicDehumNum)%RegenFanType,DesicDehum(DesicDehumNum)%RegenFanName, &
                             ErrorsFound2,DesicDehum(DesicDehumNum)%DehumType// ' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
      IF (ErrorsFound2) ErrorsFoundGeneric = .TRUE.
    ELSE
      CALL ShowSevereError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
      CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(6))//' = '//TRIM(DesicDehum(DesicDehumNum)%RegenFanType))
           ErrorsFoundGeneric = .TRUE.
    ENDIF

    IF (SameString(Alphas(8),'DrawThrough') ) THEN
       DesicDehum(DesicDehumNum)%RegenFanPlacement = DrawThru
    ELSEIF (SameString(Alphas(8),'BlowThrough') ) THEN
       DesicDehum(DesicDehumNum)%RegenFanPlacement = BlowThru
    ELSE
       CALL ShowWarningError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
       CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(8))//' = '//TRIM(Alphas(8)))
       CALL ShowContinueError('...resetting to DEFAULT of DRAW THROUGH')
       DesicDehum(DesicDehumNum)%RegenFanPlacement = DrawThru
    END IF

    ErrorsFound2 = .FALSE.
    DesicDehum(DesicDehumNum)%RegenFanInNode = &
                          GetFanInletNode(DesicDehum(DesicDehumNum)%RegenFanType, &
                          DesicDehum(DesicDehumNum)%RegenFanName,ErrorsFound2)
    IF(ErrorsFound2)THEN
      CALL ShowContinueError('...occurs in '//TRIM(DesicDehum(DesicDehumNum)%DehumType)// &
                                       ' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
      ErrorsFoundGeneric=.TRUE.
    END IF

    ErrorsFound2 = .FALSE.
    DesicDehum(DesicDehumNum)%RegenFanOutNode = &
                          GetFanOutletNode(DesicDehum(DesicDehumNum)%RegenFanType, &
                          DesicDehum(DesicDehumNum)%RegenFanName,ErrorsFound2)
    CALL GetFanIndex(DesicDehum(DesicDehumNum)%RegenFanName,DesicDehum(DesicDehumNum)%RegenFanIndex,ErrorsFound2, &
                                        DesicDehum(DesicDehumNum)%RegenFanType)
    IF(ErrorsFound2)THEN
      CALL ShowContinueError('...occurs in '//TRIM(DesicDehum(DesicDehumNum)%DehumType)// &
                                       ' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
      ErrorsFoundGeneric = .TRUE.
    END IF

    DesicDehum(DesicDehumNum)%RegenCoilType = Alphas(9)
    DesicDehum(DesicDehumNum)%RegenCoilName = Alphas(10)
    RegenCoilType = Alphas(9)
    RegenCoilName = Alphas(10)
    DesicDehum(DesicDehumNum)%RegenSetPointTemp = Numbers(1)

    IF (.NOT. lAlphaBlanks(10))Then
      IF (SameString(DesicDehum(DesicDehumNum)%RegenCoilType,'Coil:Heating:Electric') .OR. &
          SameString(DesicDehum(DesicDehumNum)%RegenCoilType,'Coil:Heating:Gas')) THEN
      IF (SameString(DesicDehum(DesicDehumNum)%RegenCoilType,'Coil:Heating:Electric')) &
                        DesicDehum(DesicDehumNum)%RegenCoilType_Num = Coil_HeatingElectric
      IF (SameString(DesicDehum(DesicDehumNum)%RegenCoilType,'Coil:Heating:Gas')) &
                        DesicDehum(DesicDehumNum)%RegenCoilType_Num = Coil_HeatingGas
        ErrorsFound2 = .FALSE.
        CALL ValidateComponent(RegenCoilType,RegenCoilName,ErrorsFound2, &
                               DesicDehum(DesicDehumNum)%DehumType//' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
        IF (ErrorsFound2) ErrorsFoundGeneric = .TRUE.

        IF(DesicDehum(DesicDehumNum)%RegenSetPointTemp .LE. 0.0d0)THEN
          CALL ShowSevereError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
          CALL ShowContinueError(TRIM(cNumericFields(1))//' must be greater than 0.')
          ErrorsFoundGeneric = .TRUE.
        END IF

        ErrorsFound2 = .FALSE.
        DesicDehum(DesicDehumNum)%RegenCoilInletNode = GetHeatingCoilInletNode(RegenCoilType,RegenCoilName,ErrorsFound2)
        IF(ErrorsFound2)THEN
          CALL ShowContinueError('...occurs in '//TRIM(DesicDehum(DesicDehumNum)%DehumType)// &
                                            ' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
          ErrorsFoundGeneric=.TRUE.
        END IF

        ErrorsFound2 = .FALSE.
        DesicDehum(DesicDehumNum)%RegenCoilOutletNode = GetHeatingCoilOutletNode(RegenCoilType,RegenCoilName,ErrorsFound2)
        IF(ErrorsFound2)THEN
          CALL ShowContinueError('...occurs in '//TRIM(DesicDehum(DesicDehumNum)%DehumType)// &
                                            ' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
          ErrorsFoundGeneric=.TRUE.
        END IF

        ErrorsFound2=.FALSE.
        CALL GetHeatingCoilIndex(RegenCoilName,DesicDehum(DesicDehumNum)%RegenCoilIndex,ErrorsFound2)
        IF(ErrorsFound2)THEN
          CALL ShowContinueError('...occurs in '//TRIM(DesicDehum(DesicDehumNum)%DehumType)// &
                                            ' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
          ErrorsFoundGeneric=.TRUE.
        END IF

        ErrorsFound2=.FALSE.
        RegenCoilControlNodeNum = GetHeatingCoilControlNodeNum(RegenCoilType,RegenCoilName,ErrorsFound2)
        IF(ErrorsFound2)THEN
          CALL ShowContinueError('...occurs in '//TRIM(DesicDehum(DesicDehumNum)%DehumType)// &
                                            ' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
          ErrorsFoundGeneric=.TRUE.
        END IF

        IF(RegenCoilControlNodeNum .GT. 0)THEN
          CALL ShowSevereError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
          CALL ShowContinueError(TRIM(cNumericFields(1))//' is specified as '// &
                                 TRIM(RoundSigDigits(DesicDehum(DesicDehumNum)%RegenSetPointTemp,3))//' C in this object.')
          CALL ShowContinueError(' Do not specify a coil temperature setpoint node name in the regeneration air heater object.')
          CALL ShowContinueError('...'//TRIM(cAlphaFields(9))//' = '//TRIM(DesicDehum(DesicDehumNum)%RegenCoilType))
          CALL ShowContinueError('...'//TRIM(cAlphaFields(10))//' = '//TRIM(DesicDehum(DesicDehumNum)%RegenCoilName))
          CALL ShowContinueError('...heating coil temperature setpoint node = '//TRIM(NodeID(RegenCoilControlNodeNum)))
          CALL ShowContinueError('...leave the heating coil temperature setpoint node name blank in the regen heater object.')
          ErrorsFoundGeneric = .TRUE.
        END IF

    ELSEIF (SameString(DesicDehum(DesicDehumNum)%RegenCoilType,'Coil:Heating:Water')) THEN
      DesicDehum(DesicDehumNum)%RegenCoilType_Num = Coil_HeatingWater
      CALL ValidateComponent(RegenCoilType,RegenCoilName,IsNotOK,TRIM(CurrentModuleObject))
      IF (IsNotOK) THEN
          CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
          ErrorsFound=.TRUE.
      ELSE ! mine data from heating coil object
          ErrFlag = .FALSE.
          DesicDehum(DesicDehumNum)%RegenCoilIndex = GetWaterCoilIndex('COIL:HEATING:WATER',RegenCoilName,ErrFlag)
          IF (DesicDehum(DesicDehumNum)%RegenCoilIndex .EQ. 0) THEN
              CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(9))//' = ' &
                              //TRIM(RegenCoilName))
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(DesicDehum(DesicDehumNum)%Name))
              ErrorsFound = .TRUE.
          END IF

          IF(DesicDehum(DesicDehumNum)%RegenSetPointTemp .LE. 0.0d0)THEN
            CALL ShowSevereError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
            CALL ShowContinueError(TRIM(cNumericFields(1))//' must be greater than 0.')
            ErrorsFoundGeneric = .TRUE.
          END IF

           ! Get the Heating Coil Hot water Inlet or control Node number
          ErrFlag = .FALSE.
          DesicDehum(DesicDehumNum)%CoilControlNode = GetCoilWaterInletNode('Coil:Heating:Water', &
                                                         RegenCoilName,ErrFlag)
          IF(ErrFlag)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(DesicDehum(DesicDehumNum)%Name))
              ErrorsFound = .TRUE.
          END IF

          ! Get the Regeneration Heating Coil hot water max volume flow rate
          ErrFlag = .FALSE.
          DesicDehum(DesicDehumNum)%MaxCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                       RegenCoilName,ErrFlag)
          IF(ErrFlag)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(DesicDehum(DesicDehumNum)%Name))
              ErrorsFound = .TRUE.
          END IF

          ! Get the Regeneration Heating Coil Inlet Node
          ErrFlag = .FALSE.
          RegenCoilAirInletNode =  GetWaterCoilInletNode('Coil:Heating:Water',RegenCoilName,ErrFlag)
          DesicDehum(DesicDehumNum)%RegenCoilInletNode = RegenCoilAirInletNode
          IF(ErrFlag)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(DesicDehum(DesicDehumNum)%Name))
              ErrorsFound = .TRUE.
          END IF

          ! Get the Regeneration Heating Coil Outlet Node
          ErrFlag = .FALSE.
          RegenCoilAirOutletNode = GetWaterCoilOutletNode('Coil:Heating:Water',RegenCoilName,ErrFlag)
          DesicDehum(DesicDehumNum)%RegenCoilOutletNode = RegenCoilAirOutletNode
          IF(ErrFlag)THEN
              CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(DesicDehum(DesicDehumNum)%Name))
              ErrorsFound = .TRUE.
          END IF

        ENDIF
    ELSEIF (SameString(DesicDehum(DesicDehumNum)%RegenCoilType,'Coil:Heating:Steam')) THEN
        DesicDehum(DesicDehumNum)%RegenCoilType_Num = Coil_HeatingSteam
        CALL ValidateComponent(RegenCoilType,RegenCoilName,IsNotOK,TRIM(CurrentModuleObject))
        IF (IsNotOK) THEN
             CALL ShowContinueError('...occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(Alphas(1)))
             ErrorsFound=.TRUE.
        ELSE ! mine data from the regeneration heating coil object
            IF(DesicDehum(DesicDehumNum)%RegenSetPointTemp .LE. 0.0d0)THEN
              CALL ShowSevereError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
              CALL ShowContinueError(TRIM(cNumericFields(1))//' must be greater than 0.')
              ErrorsFoundGeneric = .TRUE.
            END IF

            ErrFlag = .FALSE.
            DesicDehum(DesicDehumNum)%RegenCoilIndex = GetSTeamCoilIndex('COIL:HEATING:STEAM',RegenCoilName,ErrFlag)
            IF (DesicDehum(DesicDehumNum)%RegenCoilIndex .EQ. 0) THEN
                CALL ShowSevereError(TRIM(CurrentModuleObject)//' illegal '//TRIM(cAlphaFields(9))//' = ' &
                                //TRIM(RegenCoilName))
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(DesicDehum(DesicDehumNum)%Name))
                ErrorsFound = .TRUE.
            END IF

            ! Get the regeneration Heating Coil steam inlet node number
            ErrFlag = .FALSE.
            DesicDehum(DesicDehumNum)%CoilControlNode = GetSteamCoilSteamInletNode('Coil:Heating:Steam',RegenCoilName,ErrFlag)
            IF(ErrFlag)THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(DesicDehum(DesicDehumNum)%Name))
                ErrorsFound = .TRUE.
            END IF

            ! Get the regeneration heating Coil steam max volume flow rate
            DesicDehum(DesicDehumNum)%MaxCoilFluidFlow = &
                                    GetCoilMaxSteamFlowRate(DesicDehum(DesicDehumNum)%RegenCoilIndex,ErrFlag)
            IF (DesicDehum(DesicDehumNum)%MaxCoilFluidFlow .GT. 0.0d0)THEN
                SteamIndex = 0      ! Function GetSatDensityRefrig will look up steam index if 0 is passed
                SteamDensity=GetSatDensityRefrig("STEAM",TempSteamIn,1.0d0,SteamIndex,'Dehumidifier:Desiccant:NoFans')
                DesicDehum(DesicDehumNum)%MaxCoilFluidFlow = DesicDehum(DesicDehumNum)%MaxCoilFluidFlow * SteamDensity
            END IF

            ! Get the regeneration heating Coil Inlet Node
            ErrFlag = .FALSE.
            RegenCoilAirInletNode = &
                        GetSteamCoilAirInletNode(DesicDehum(DesicDehumNum)%RegenCoilIndex,RegenCoilName,ErrFlag)
            DesicDehum(DesicDehumNum)%RegenCoilInletNode = RegenCoilAirInletNode
            IF(ErrFlag)THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(DesicDehum(DesicDehumNum)%Name))
                ErrorsFound = .TRUE.
            END IF

            ! Get the regeneration heating Coil Outlet Node
            ErrFlag = .FALSE.
            RegenCoilAirOutletNode = &
                        GetSteamCoilAirOutletNode(DesicDehum(DesicDehumNum)%RegenCoilIndex,RegenCoilName,ErrFlag)
            DesicDehum(DesicDehumNum)%RegenCoilOutletNode = RegenCoilAirOutletNode
            IF(ErrFlag)THEN
                CALL ShowContinueError('Occurs in '//TRIM(CurrentModuleObject)//' = '//TRIM(DesicDehum(DesicDehumNum)%Name))
                ErrorsFound = .TRUE.
            END IF
        ENDIF

        ErrorsFound2=.FALSE.
        RegenCoilControlNodeNum = GetSteamCoilControlNodeNum(RegenCoilType,RegenCoilName,ErrorsFound2)

        IF(ErrorsFound2)THEN
          CALL ShowContinueError('...occurs in '//TRIM(DesicDehum(DesicDehumNum)%DehumType)// &
                                            ' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
          ErrorsFoundGeneric=.TRUE.
        END IF

        IF(RegenCoilControlNodeNum .GT. 0)THEN
          CALL ShowSevereError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
          CALL ShowContinueError(TRIM(cNumericFields(1))//' is specified as '// &
                                 TRIM(RoundSigDigits(DesicDehum(DesicDehumNum)%RegenSetPointTemp,3))//' C in this object.')
          CALL ShowContinueError(' Do not specify a coil temperature setpoint node name in the regeneration air heater object.')
          CALL ShowContinueError('...'//TRIM(cAlphaFields(9))//' = '//TRIM(DesicDehum(DesicDehumNum)%RegenCoilType))
          CALL ShowContinueError('...'//TRIM(cAlphaFields(10))//' = '//TRIM(DesicDehum(DesicDehumNum)%RegenCoilName))
          CALL ShowContinueError('...heating coil temperature setpoint node = '//TRIM(NodeID(RegenCoilControlNodeNum)))
          CALL ShowContinueError('...leave the heating coil temperature setpoint node name blank in the regen heater object.')
          ErrorsFoundGeneric = .TRUE.
        END IF

      ELSE
        CALL ShowSevereError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
        CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(9))//' = '//TRIM(DesicDehum(DesicDehumNum)%RegenCoilType))
        ErrorsFoundGeneric = .TRUE.
      END IF

    ENDIF

    RegenAirInlet =  NodeID(DesicDehum(DesicDehumNum)%HXRegenInNode)

    RegenAirOutlet = NodeID(DesicDehum(DesicDehumNum)%HXRegenOutNode)

    RegenFanInlet = NodeID(DesicDehum(DesicDehumNum)%RegenFanInNode)

    RegenFanOutlet = NodeID(DesicDehum(DesicDehumNum)%RegenFanOutNode)

    IF (.NOT. lAlphaBlanks(10))Then
      RegenCoilInlet = NodeID(DesicDehum(DesicDehumNum)%RegenCoilInletNode)

      RegenCoilOutlet = NodeID(DesicDehum(DesicDehumNum)%RegenCoilOutletNode)
    ENDIF

    CALL SetUpCompSets(DesicDehum(DesicDehumNum)%DehumType, DesicDehum(DesicDehumNum)%Name, &
           DesicDehum(DesicDehumNum)%HXType,DesicDehum(DesicDehumNum)%HXName, &
           ProcAirInlet,ProcAirOutlet)

    CALL SetUpCompSets(DesicDehum(DesicDehumNum)%DehumType, DesicDehum(DesicDehumNum)%Name, &
           DesicDehum(DesicDehumNum)%RegenFanType,DesicDehum(DesicDehumNum)%RegenFanName, &
           RegenFanInlet,RegenFanOutlet)

    IF (.NOT. lAlphaBlanks(10))THEN
      CALL SetUpCompSets(DesicDehum(DesicDehumNum)%DehumType, DesicDehum(DesicDehumNum)%Name, &
           DesicDehum(DesicDehumNum)%RegenCoilType,DesicDehum(DesicDehumNum)%RegenCoilName, &
           RegenCoilInlet,RegenCoilOutlet)
    ENDIF

    IF (DesicDehum(DesicDehumNum)%RegenFanPlacement == BlowThru)THEN
      DesicDehum(DesicDehumNum)%RegenAirInNode = GetOnlySingleNode(RegenFanInlet,ErrorsFound,&
                 DesicDehum(DesicDehumNum)%DehumType,DesicDehum(DesicDehumNum)%Name, &
                 NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)
      DesicDehum(DesicDehumNum)%RegenAirOutNode = GetOnlySingleNode(RegenAirOutlet,ErrorsFound,&
                 DesicDehum(DesicDehumNum)%DehumType,DesicDehum(DesicDehumNum)%Name, &
                 NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)
      IF (.NOT. lAlphaBlanks(10))THEN
        IF (DesicDehum(DesicDehumNum)%RegenFanOutNode /= DesicDehum(DesicDehumNum)%RegenCoilInletNode)THEN
          CALL ShowSevereError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
          CALL ShowContinueError('Regen fan outlet node name and regen heater inlet node name do not match for fan ' &
                 //'placement: Blow Through')
          CALL ShowContinueError('...Regen fan outlet node   = '//TRIM(NodeID(DesicDehum(DesicDehumNum)%RegenFanOutNode)))
          CALL ShowContinueError('...Regen heater inlet node = '//TRIM(NodeID(DesicDehum(DesicDehumNum)%RegenCoilInletNode)))
          ErrorsFoundGeneric = .TRUE.
        ENDIF
        IF (DesicDehum(DesicDehumNum)%RegenCoilOutletNode /= DesicDehum(DesicDehumNum)%HXRegenInNode)THEN
          CALL ShowSevereError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
          CALL ShowContinueError('Regen heater outlet node name and desiccant heat exchanger regen inlet node name do not ' &
                 //'match for fan placement: Blow Through')
          CALL ShowContinueError('...Regen heater outlet node = '//TRIM(NodeID(DesicDehum(DesicDehumNum)%RegenCoilOutletNode)))
          CALL ShowContinueError('...HX regen inlet node      = '//TRIM(NodeID(DesicDehum(DesicDehumNum)%HXRegenInNode)))
          ErrorsFoundGeneric = .TRUE.
        ENDIF
      ELSE
        IF (DesicDehum(DesicDehumNum)%RegenFanOutNode /= DesicDehum(DesicDehumNum)%HXRegenInNode)THEN
          CALL ShowSevereError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
          CALL ShowContinueError('Regen fan outlet node name and desiccant heat exchanger inlet node name do not match for fan ' &
                 //'placement: Blow Through')
          CALL ShowContinueError('...Regen fan outlet node   = '//TRIM(NodeID(DesicDehum(DesicDehumNum)%RegenFanOutNode)))
          CALL ShowContinueError('...Desiccant HX inlet node = '//TRIM(NodeID(DesicDehum(DesicDehumNum)%HXRegenInNode)))
          ErrorsFoundGeneric = .TRUE.
        ENDIF
      ENDIF
    ELSE ! ELSE for IF (DesicDehum(DesicDehumNum)%RegenFanPlacement == BlowThru)THEN
      DesicDehum(DesicDehumNum)%RegenAirOutNode = GetOnlySingleNode(RegenFanOutlet,ErrorsFound,&
                 DesicDehum(DesicDehumNum)%DehumType,DesicDehum(DesicDehumNum)%Name, &
                 NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsParent)
      IF (.NOT. lAlphaBlanks(10)) THEN
        DesicDehum(DesicDehumNum)%RegenAirInNode =  GetOnlySingleNode(RegenCoilInlet,ErrorsFound,&
                      DesicDehum(DesicDehumNum)%DehumType,DesicDehum(DesicDehumNum)%Name, &
                      NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)
        IF (DesicDehum(DesicDehumNum)%RegenCoilOutletNode /= DesicDehum(DesicDehumNum)%HXRegenInNode)THEN
          CALL ShowSevereError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
          CALL ShowContinueError('Regen heater outlet node name and desiccant heat exchanger regen inlet node name do not ' &
                  //'match for fan placement: Draw Through')
          CALL ShowContinueError('...Regen heater outlet node = '//TRIM(NodeID(DesicDehum(DesicDehumNum)%RegenCoilOutletNode)))
          CALL ShowContinueError('...HX regen inlet node      = '//TRIM(NodeID(DesicDehum(DesicDehumNum)%HXRegenInNode)))
          ErrorsFoundGeneric = .TRUE.
        ENDIF
      ELSE
        DesicDehum(DesicDehumNum)%RegenAirInNode =  GetOnlySingleNode(RegenAirInlet,ErrorsFound,&
                      DesicDehum(DesicDehumNum)%DehumType,DesicDehum(DesicDehumNum)%Name, &
                      NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsParent)
      ENDIF
      IF (DesicDehum(DesicDehumNum)%RegenFanInNode /= DesicDehum(DesicDehumNum)%HXRegenOutNode)THEN
        CALL ShowSevereError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
        CALL ShowContinueError('Regen fan inlet node name and desiccant heat exchanger regen outlet node name do not match ' &
                //'for fan placement: Draw Through')
        CALL ShowContinueError('...Regen fan inlet node = '//TRIM(NodeID(DesicDehum(DesicDehumNum)%RegenFanInNode)))
        CALL ShowContinueError('...HX regen outlet node = '//TRIM(NodeID(DesicDehum(DesicDehumNum)%HXRegenOutNode)))
        ErrorsFoundGeneric = .TRUE.
      ENDIF
    ENDIF

    DesicDehum(DesicDehumNum)%CoolingCoilType = Alphas(11)
    DesicDehum(DesicDehumNum)%CoolingCoilName = Alphas(12)

    IF (.NOT. lAlphaBlanks(12))THEN
      IF ((SameString(DesicDehum(DesicDehumNum)%CoolingCoilType,'COIL:COOLING:DX:SINGLESPEED')) .OR. &
          (SameString(DesicDehum(DesicDehumNum)%CoolingCoilType,'COIL:COOLING:DX:TWOSTAGEWITHHUMIDITYCONTROLMODE'))) THEN
        ErrorsFound2 = .FALSE.
        CALL ValidateComponent(DesicDehum(DesicDehumNum)%CoolingCoilType,DesicDehum(DesicDehumNum)%CoolingCoilName, &
                               ErrorsFound2,DesicDehum(DesicDehumNum)%DehumType//' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
        IF (ErrorsFound2) ErrorsFoundGeneric = .TRUE.
      ELSE !ELSE for IF (DesicDehum(DesicDehumNum)%CoolingCoilType == 'COIL:COOLING:DX:SINGLESPEED' or MultiMode)THEN
        CALL ShowSevereError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//'='//TRIM(DesicDehum(DesicDehumNum)%Name))
        CALL ShowContinueError('Illegal '//TRIM(cAlphaFields(11))//' = '//TRIM(DesicDehum(DesicDehumNum)%CoolingCoilType))
           ErrorsFoundGeneric = .TRUE.
      END IF

      ErrorsFound2 = .FALSE.
      DesicDehum(DesicDehumNum)%CoolingCoilOutletNode = &
                    GetDXCoilOutletNode(DesicDehum(DesicDehumNum)%CoolingCoilType, &
                     DesicDehum(DesicDehumNum)%CoolingCoilName,ErrorsFound2)
      DesicDehum(DesicDehumNum)%CompanionCoilCapacity = GetDXCoilCapacity(DesicDehum(DesicDehumNum)%CoolingCoilType, &
                     DesicDehum(DesicDehumNum)%CoolingCoilName,ErrorsFound2)
      IF(ErrorsFound2)CALL ShowContinueError('...occurs in '//TRIM(DesicDehum(DesicDehumNum)%DehumType)// &
                                       ' "'//TRIM(DesicDehum(DesicDehumNum)%CoolingCoilName)//'"')

      ErrorsFound2 = .FALSE.
      CALL GetDXCoilIndex(DesicDehum(DesicDehumNum)%CoolingCoilName,DesicDehum(DesicDehumNum)%DXCoilIndex, &
                              ErrorsFound2, DesicDehum(DesicDehumNum)%CoolingCoilType)
      IF(ErrorsFound2)CALL ShowContinueError('...occurs in '//TRIM(DesicDehum(DesicDehumNum)%DehumType)// &
                                                        ' "'//TRIM(DesicDehum(DesicDehumNum)%CoolingCoilName)//'"')

    ENDIF !  (DesicDehum(DesicDehumNum)%CoolingCoilName /= Blank)THEN

    IF (SameString(Alphas(13),'Yes')) THEN
      DesicDehum(DesicDehumNum)%CoilUpstreamOfProcessSide = Yes
    ELSE IF (lAlphaBlanks(13)) THEN
      DesicDehum(DesicDehumNum)%CoilUpstreamOfProcessSide = No
    ELSEIF (SameString(Alphas(13),'No')) THEN
      DesicDehum(DesicDehumNum)%CoilUpstreamOfProcessSide = No
    ELSE
      CALL ShowWarningError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
      CALL ShowContinueError('Invalid choice for '//TRIM(cAlphaFields(13))//' = ' &
                              //TRIM(Alphas(13)))
      CALL ShowContinueError('...resetting to the default value of No')
      DesicDehum(DesicDehumNum)%CoilUpstreamOfProcessSide = No
    END IF

    IF (SameString(Alphas(14),'Yes')) THEN
      DesicDehum(DesicDehumNum)%Preheat = Yes
    ELSEIF (SameString(Alphas(14),'No')) THEN
      DesicDehum(DesicDehumNum)%Preheat = No
    ELSEIF (lAlphaBlanks(14)) THEN
      DesicDehum(DesicDehumNum)%Preheat = No
    ELSE
      CALL ShowWarningError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
      CALL ShowContinueError('Invalid choice for '//TRIM(cAlphaFields(14))//' = '//TRIM(Alphas(14)))
      CALL ShowContinueError('...resetting to the default value of NO')
      DesicDehum(DesicDehumNum)%Preheat = No
    END IF

    IF(DesicDehum(DesicDehumNum)%DXCoilIndex .GT. 0)THEN

      IF(DesicDehum(DesicDehumNum)%Preheat == Yes)THEN ! Companion coil waste heat used for regeneration of desiccant
        ErrorsFound2 = .FALSE.
        DesuperHeaterIndex = GetHeatReclaimSourceIndexNum(DesicDehum(DesicDehumNum)%CoolingCoilType, &
                                    DesicDehum(DesicDehumNum)%CoolingCoilName,ErrorsFound2)
        IF (ErrorsFound2)THEN
          CALL ShowContinueError('...occurs in '//TRIM(DesicDehum(DesicDehumNum)%DehumType)// &
                                 ' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
          ErrorsFoundGeneric = .TRUE.
        END IF

        IF(DesuperHeaterIndex .GT. 0) THEN
          CALL ShowWarningError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//'='//TRIM(DesicDehum(DesicDehumNum)%Name))
          CALL ShowContinueError('A Coil:Heating:Desuperheater object should not be used when condenser waste heat is '// &
                               'reclaimed for desiccant regeneration.')
          CALL ShowContinueError('A Coil:Heating:Desuperheater object was found using waste heat from the ' &
                              //TRIM(DesicDehum(DesicDehumNum)%CoolingCoilType)//' "'//&
                                TRIM(DesicDehum(DesicDehumNum)%CoolingCoilName)//'" object.')
!          ErrorsFoundGeneric = .TRUE.
        END IF
      END IF

      ErrorsFound2 = .FALSE.
      DesicDehum(DesicDehumNum)%CondenserInletNode = GetCoilCondenserInletNode(DesicDehum(DesicDehumNum)%CoolingCoilType, &
                   DesicDehum(DesicDehumNum)%CoolingCoilName,ErrorsFound2)
      IF(DesicDehum(DesicDehumNum)%CondenserInletNode .EQ. 0 .AND. DesicDehum(DesicDehumNum)%Preheat .EQ. Yes)THEN
         DesicDehum(DesicDehumNum)%CondenserInletNode = GetOnlySingleNode(&
             TRIM(DesicDehum(DesicDehumNum)%CoolingCoilName)//' Condenser Inlet Node', &
             ErrorsFound,TRIM(DesicDehum(DesicDehumNum)%DehumType),TRIM(DesicDehum(DesicDehumNum)%Name), &
             NodeType_Air,NodeConnectionType_OutsideAirReference, 2, ObjectIsNotParent)
        CALL CheckAndAddAirNodeNumber(DesicDehum(DesicDehumNum)%CondenserInletNode,OANodeError)
        IF(.NOT. OANodeError)THEN
          CALL ShowWarningError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
          CALL ShowContinueError('The '//TRIM(cAlphaFields(14))//' input is specified as Yes and a condenser air'// &
                                 ' inlet node name was not specified for the companion cooling coil.')
          CALL ShowContinueError('Adding condenser inlet air node for '//TRIM(DesicDehum(DesicDehumNum)%CoolingCoilType)//' "'// &
                                                                         TRIM(DesicDehum(DesicDehumNum)%CoolingCoilName)//'"')
          CALL ShowContinueError('...condenser inlet air node name = '// &
                                 TRIM(NodeID(DesicDehum(DesicDehumNum)%CondenserInletNode)))
          CALL ShowContinueError('...this node name will be specified as an outdoor air node.')
        END IF
      ELSE IF(DesicDehum(DesicDehumNum)%Preheat .EQ. Yes)THEN
        IF (.NOT. CheckOutAirNodeNumber(DesicDehum(DesicDehumNum)%CondenserInletNode)) THEN
          CALL ShowSevereError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
          CALL ShowContinueError('The regeneration air inlet node must be specified as an outdoor air node '// &
                                 'when '//TRIM(cAlphaFields(14))//' is specified as Yes.')
          ErrorsFoundGeneric = .TRUE.
        END IF
      END IF
    END IF

    IF (CheckOutAirNodeNumber(DesicDehum(DesicDehumNum)%RegenAirInNode)) THEN
      DesicDehum(DesicDehumNum)%RegenInletIsOutsideAirNode = .TRUE.
    END IF

    IF(DesicDehum(DesicDehumNum)%DXCoilIndex .EQ. 0 .AND. DesicDehum(DesicDehumNum)%Preheat == Yes) THEN
      CALL ShowWarningError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//'='//TRIM(DesicDehum(DesicDehumNum)%Name))
      CALL ShowContinueError('A valid '//TRIM(cAlphaFields(12))//' must be used when condenser waste heat is '// &
                             'reclaimed for desiccant regeneration.')
      CALL ShowContinueError('... '//TRIM(cAlphaFields(11))//' = '//TRIM(DesicDehum(DesicDehumNum)%CoolingCoilType))
      CALL ShowContinueError('... '//TRIM(cAlphaFields(12))//' = '//TRIM(DesicDehum(DesicDehumNum)%CoolingCoilName))
      ErrorsFoundGeneric = .TRUE.
    END IF

    IF(DesicDehum(DesicDehumNum)%DXCoilIndex .GT. 0 .AND. DesicDehum(DesicDehumNum)%CoilUpstreamOfProcessSide == Yes) THEN
      ErrorsFound2 = .FALSE.
      CoilBypassedFlowFrac = GetDXCoilBypassedFlowFrac(DesicDehum(DesicDehumNum)%CoolingCoilType, &
                                                       DesicDehum(DesicDehumNum)%CoolingCoilName, ErrorsFound2)
      IF(ErrorsFound2)CALL ShowContinueError('...occurs in '//TRIM(DesicDehum(DesicDehumNum)%DehumType)// &
                                                          ' "'//TRIM(DesicDehum(DesicDehumNum)%CoolingCoilName)//'"')
      IF(CoilBypassedFlowFrac .GT. 0.0d0)THEN
        CALL ShowWarningError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//'='//TRIM(DesicDehum(DesicDehumNum)%Name))
        CALL ShowContinueError('A DX coil bypassed air flow fraction greater than 0 may not be used when the input for '// &
                               TRIM(cAlphaFields(13))//' is specified as Yes.')
        CALL ShowContinueError('A DX coil with a bypassed air flow fraction greater than 0 may be upstream of the process '// &
                               'inlet however the input for '//TRIM(cAlphaFields(13))//' must be specified as No.')
        CALL ShowContinueError('... '//TRIM(cAlphaFields(11))//' = '//TRIM(DesicDehum(DesicDehumNum)%CoolingCoilType))
        CALL ShowContinueError('... '//TRIM(cAlphaFields(12))//' = '//TRIM(DesicDehum(DesicDehumNum)%CoolingCoilName))
        ErrorsFoundGeneric = .TRUE.
      END IF
    ELSE IF(DesicDehum(DesicDehumNum)%DXCoilIndex .EQ. 0 .AND. DesicDehum(DesicDehumNum)%CoilUpstreamOfProcessSide == Yes) THEN
        CALL ShowSevereError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
        CALL ShowContinueError('A valid companion coil must be specified when '// &
                               TRIM(cAlphaFields(13))//' is specified as Yes.')
        ErrorsFoundGeneric = .TRUE.
    END IF

    IF(.NOT. DesicDehum(DesicDehumNum)%RegenInletIsOutsideAirNode .AND. DesicDehum(DesicDehumNum)%Preheat == Yes) THEN
      CALL ShowWarningError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//'='//TRIM(DesicDehum(DesicDehumNum)%Name))
      CALL ShowContinueError('The desiccant dehumidifier regeneration air inlet must be specified as an outdoor air node'// &
                             ' when '//TRIM(cAlphaFields(14))//' is specified as Yes.')
      CALL ShowContinueError('... desiccant dehumidifier regeneration air inlet node name = '// &
                                  TRIM(NodeID(DesicDehum(DesicDehumNum)%RegenAirInNode)))
      ErrorsFoundGeneric = .TRUE.
    END IF

    IF(DesicDehum(DesicDehumNum)%CoilUpstreamOfProcessSide == Yes)THEN
      IF(DesicDehum(DesicDehumNum)%ProcAirInNode .NE. DesicDehum(DesicDehumNum)%CoolingCoilOutletNode)THEN
         CALL ShowSevereError('For '//TRIM(DesicDehum(DesicDehumNum)%DehumType)// &
                     ' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
         CALL ShowContinueError('Node names are inconsistent in companion cooling coil and desiccant heat exchanger objects.')
         CALL ShowContinueError('For companion cooling coil = '//TRIM(DesicDehum(DesicDehumNum)%CoolingCoilType)//' "'// &
                                                    TRIM(DesicDehum(DesicDehumNum)%CoolingCoilName)//'"')
         CALL ShowContinueError('The outlet node name in cooling coil = '// &
                              TRIM(NodeID(DesicDehum(DesicDehumNum)%CoolingCoilOutletNode)))
         CALL ShowContinueError('For desiccant heat exchanger = '//TRIM(DesicDehum(DesicDehumNum)%HXType)//' "'// &
                                                    TRIM(DesicDehum(DesicDehumNum)%HXName)//'"')
         CALL ShowContinueError('The process air inlet node name = '// &
                              TRIM(NodeID(DesicDehum(DesicDehumNum)%ProcAirInNode)))
         CALL ShowFatalError('...previous error causes program termination.')
      END IF
    END IF

    !Exhaust Fan input
    DesicDehum(DesicDehumNum)%ExhaustFanMaxVolFlowRate = Numbers(2)
    DesicDehum(DesicDehumNum)%ExhaustFanMaxPower = Numbers(3)
    DesicDehum(DesicDehumNum)%ExhaustFanCurveIndex = GetCurveIndex(Alphas(15))

    IF(.NOT. SameString(GetCurveType(DesicDehum(DesicDehumNum)%ExhaustFanCurveIndex),' '))THEN
      SELECT CASE(GetCurveType(DesicDehum(DesicDehumNum)%ExhaustFanCurveIndex))
        CASE('CUBIC')

        CASE('QUADRATIC')

        CASE DEFAULT
          CALL ShowSevereError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//', "'//TRIM(DesicDehum(DesicDehumNum)%Name)// &
              '" illegal Part Load Fraction Correlation Curve (function of part-load ratio) type for this object = '// &
             TRIM(GetCurveType(DesicDehum(DesicDehumNum)%ExhaustFanCurveIndex)))
          ErrorsFoundGeneric=.true.
      END SELECT
    END IF

    IF(DesicDehum(DesicDehumNum)%Preheat == Yes)THEN
      ErrorsFound2 = .FALSE.
      IF (DesicDehum(DesicDehumNum)%ExhaustFanMaxVolFlowRate <= 0)THEN
         ErrorsFound2 =.TRUE.
      ENDIF
      IF (DesicDehum(DesicDehumNum)%ExhaustFanMaxPower <= 0)THEN
         ErrorsFound2 =.TRUE.
      ENDIF
      IF(ErrorsFound2)THEN
        CALL ShowSevereError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "'//TRIM(DesicDehum(DesicDehumNum)%Name//'"'))
        CALL ShowContinueError(TRIM(cNumericFields(2))//' and '//TRIM(cNumericFields(3))// &
                               ' must be defined if '//TRIM(cAlphaFields(14))//' field is "Yes".')
      END IF
    ELSE IF(DesicDehum(DesicDehumNum)%Preheat == No)THEN
      IF(DesicDehum(DesicDehumNum)%ExhaustFanMaxVolFlowRate .GT. 0.0d0)THEN
        CALL ShowWarningError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "'//TRIM(DesicDehum(DesicDehumNum)%Name//'"'))
        CALL ShowContinueError(TRIM(cNumericFields(2))// &
                               ' should be 0 if '//TRIM(cAlphaFields(14))//' field is "No".')
        CALL ShowContinueError('...'//TRIM(cNumericFields(2))//' will not be used and is reset to 0.')
        DesicDehum(DesicDehumNum)%ExhaustFanMaxVolFlowRate = 0.0d0
      END IF
    END IF

  ENDDO

! SET UP OUTPUTS
  DO DesicDehumNum=1,NumSolidDesicDehums
    ! Setup Report variables for the Desiccant Dehumidifiers
    CALL SetupOutputVariable('Dehumidifier Removed Water Mass [kg]',DesicDehum(DesicDehumNum)%WaterRemove,&
                             'System','Sum',DesicDehum(DesicDehumNum)%Name)
    CALL SetupOutputVariable('Dehumidifier Removed Water Mass Flow Rate [kg/s]',DesicDehum(DesicDehumNum)%WaterRemoveRate,&
                             'System','Average',DesicDehum(DesicDehumNum)%Name)
    CALL SetupOutputVariable('Dehumidifier Part Load Ratio []',DesicDehum(DesicDehumNum)%PartLoad,'System',&
                             'Average',DesicDehum(DesicDehumNum)%Name)
    CALL SetupOutputVariable('Dehumidifier Electric Power [W]',DesicDehum(DesicDehumNum)%ElecUseRate,'System',&
                             'Average',DesicDehum(DesicDehumNum)%Name)
    CALL SetupOutputVariable('Dehumidifier Electric Energy [J]',DesicDehum(DesicDehumNum)%ElecUseEnergy,'System',&
                             'Sum',DesicDehum(DesicDehumNum)%Name, &
                             ResourceTypeKey='Electricity',GroupKey='System',EndUseKey='Cooling')
    CALL SetupOutputVariable('Dehumidifier Regeneration Specific Energy [J/kgWater]',&
                             DesicDehum(DesicDehumNum)%SpecRegenEnergy,&
                             'System','Average',DesicDehum(DesicDehumNum)%Name)
    CALL SetupOutputVariable('Dehumidifier Regeneration Rate [W]',DesicDehum(DesicDehumNum)%QRegen,'System','Average',&
                             DesicDehum(DesicDehumNum)%Name)
    CALL SetupOutputVariable('Dehumidifier Regeneration Energy [J]',DesicDehum(DesicDehumNum)%RegenEnergy,'System','Sum',&
                             DesicDehum(DesicDehumNum)%Name)
    CALL SetupOutputVariable('Dehumidifier Regeneration Air Speed [m/s]',DesicDehum(DesicDehumNum)%RegenAirVel,&
                             'System','Average',DesicDehum(DesicDehumNum)%Name)
    CALL SetupOutputVariable('Dehumidifier Regeneration Air Mass Flow Rate [kg/s]',&
                             DesicDehum(DesicDehumNum)%RegenAirInMassFlowRate,'System','Average',&
                             DesicDehum(DesicDehumNum)%Name)
    CALL SetupOutputVariable('Dehumidifier Process Air Mass Flow Rate [kg/s]',&
                             DesicDehum(DesicDehumNum)%ProcAirInMassFlowRate,'System','Average',&
                             DesicDehum(DesicDehumNum)%Name)
  END DO

  DO DesicDehumNum=1,NumGenericDesicDehums
    ! Setup Report variables for the Desiccant Dehumidifiers
    CALL SetupOutputVariable('Dehumidifier Removed Water Mass [kg]',DesicDehum(DesicDehumNum)%WaterRemove,&
                             'System','Sum',DesicDehum(DesicDehumNum)%Name)
    CALL SetupOutputVariable('Dehumidifier Removed Water Mass Flow Rate [kg/s]',DesicDehum(DesicDehumNum)%WaterRemoveRate,&
                             'System','Average',DesicDehum(DesicDehumNum)%Name)
    CALL SetupOutputVariable('Dehumidifier Part Load Ratio []',DesicDehum(DesicDehumNum)%PartLoad,'System',&
                             'Average',DesicDehum(DesicDehumNum)%Name)
    IF(DesicDehum(DesicDehumNum)%ExhaustFanMaxVolFlowRate .GT. 0)THEN
      CALL SetupOutputVariable('Dehumidifier Exhaust Fan Electric Power [W]',DesicDehum(DesicDehumNum)%ExhaustFanPower,&
                               'System','Average',DesicDehum(DesicDehumNum)%Name)
      CALL SetupOutputVariable('Dehumidifier Exhaust Fan Electric Energy [J]', &
                                DesicDehum(DesicDehumNum)%ExhaustFanElecConsumption,'System',&
                               'Sum',DesicDehum(DesicDehumNum)%Name, &
                                ResourceTypeKey='Electricity',GroupKey='System',EndUseKey='Cooling')
    END IF
  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError('Errors found in getting Dehumidifier:Desiccant:NoFans input')
  ELSEIF (ErrorsFoundGeneric) THEN
    CALL ShowFatalError('Errors found in getting DESICCANT DEHUMIDIFIER input')
  END IF

  DEALLOCATE(Alphas)
  DEALLOCATE(cAlphaFields)
  DEALLOCATE(cNumericFields)
  DEALLOCATE(Numbers)
  DEALLOCATE(lAlphaBlanks)
  DEALLOCATE(lNumericBlanks)

  RETURN

END SUBROUTINE GetDesiccantDehumidifierInput

SUBROUTINE InitDesiccantDehumidifier(DesicDehumNum,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
          !                      for Gas Research Institute
          !       DATE WRITTEN   March 2001
          !       MODIFIED       Jan 2005 M. J. Witte, GARD Analytics, Inc.
          !                        Add setpoint validation for new control type option:
          !                          NODE LEAVING HUMRAT SETPOINT:BYPASS
          !                        Work supported by ASHRAE research project 1254-RP
          !                      June 2007 R. Raustad, FSEC
          !                        Added new dehumidifier type -- DESICCANT DEHUMIDIFIER
          !                      May 2009, B. Griffith, NREL. added EMS node setpoint checks
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the dehumidifier Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY: DoSetPointTest, SetPointErrorFlag
!unused  USE DataEnvironment, ONLY: StdBaroPress
  USE Psychrometrics,  ONLY: PsyRhoAirFnPbTdbW
  USE EMSManager ,     ONLY: CheckIfNodeSetpointManagedByEMS, iHumidityRatioMaxSetpoint
  USE SteamCoils,           ONLY: SimulateSteamCoilComponents, GetCoilMaxSteamFlowRate=>GetCoilMaxSteamFlowRate, &
                                  GetSteamCoilCapacity=>GetCoilCapacity
  USE WaterCoils,           ONLY: GetCoilMaxWaterFlowRate, SimulateWaterCoilComponents
  USE DataPlant,            ONLY: TypeOf_CoilSteamAirHeating, ScanPlantLoopsForObject, TypeOf_CoilWaterSimpleHeating, &
                                  PlantLoop
  USE FluidProperties,      ONLY: GetDensityGlycol, GetSatDensityRefrig
  USE PlantUtilities,       ONLY: SetComponentFlowRate, InitComponentNodes
  USE DataGlobals,          ONLY: InitConvTemp, AnyPlantInModel
  USE DataSizing,           ONLY: AutoSize

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: DesicDehumNum ! number of the current dehumidifier being simulated
  LOGICAL, INTENT (IN) :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: ProcInNode ! inlet node number
  INTEGER             :: RegenInNode ! inlet node number
  INTEGER             :: ControlNode ! control node number
  LOGICAL,SAVE        :: MySetPointCheckFlag = .TRUE.
  LOGICAL,SAVE        :: MyOneTimeFlag = .TRUE.
  LOGICAL, ALLOCATABLE,SAVE, DIMENSION(:) :: MyEnvrnFlag
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: MyPlantScanFlag  ! Used for init plant component for heating coils

  LOGICAL                        :: ErrorsFound=.false.  ! Set to true if errors in input, fatal at end of routine
  INTEGER                        :: SteamIndex           ! steam coil index
  REAL(r64)                      :: FluidDensity         ! steam or water coil fluid density
  REAL(r64)                      :: CoilMaxVolFlowRate   ! water or steam max volumetric water flow rate
  REAL(r64)                      :: QCoilActual          ! actual CBVAV steam heating coil load met (W)
  LOGICAL                        :: ErrorFlag            ! local error flag returned from data mining
!unused  REAL(r64)                      :: mdot                 ! heating coil fluid mass flow rate, kg/s
!unused  REAL(r64)                      :: QDelivered           ! regen heat actually delivered by regen coil [W]

  IF (MyOneTimeFlag) THEN

    ! initialize the environment and sizing flags
    ALLOCATE(MyEnvrnFlag(NumDesicDehums))
    ALLOCATE(MyPlantScanFlag(NumDesicDehums))
    MyEnvrnFlag = .TRUE.

    MyOneTimeFlag = .false.
    MyPlantScanFlag = .TRUE.

  END IF

  IF (MyPlantScanFlag(DesicDehumNum) .AND. ALLOCATED(PlantLoop)) THEN
    IF ( (DesicDehum(DesicDehumNum)%RegenCoilType_Num == Coil_HeatingWater) .OR. &
         (DesicDehum(DesicDehumNum)%RegenCoilType_Num == Coil_HeatingSteam) ) THEN
      IF (DesicDehum(DesicDehumNum)%RegenCoilType_Num == Coil_HeatingWater) THEN
        ErrorFlag=.false.
        CALL ScanPlantLoopsForObject( DesicDehum(DesicDehumNum)%RegenCoilName, &
                                          TypeOf_CoilWaterSimpleHeating , &
                                          DesicDehum(DesicDehumNum)%LoopNum, &
                                          DesicDehum(DesicDehumNum)%LoopSide, &
                                          DesicDehum(DesicDehumNum)%BranchNum, &
                                          DesicDehum(DesicDehumNum)%CompNum,   &
                                          ErrFlag=ErrorFlag)
        IF (ErrorFlag) THEN
          CALL ShowFatalError('InitDesiccantDehumidifier: Program terminated for previous conditions.')
        ENDIF

        ErrorFlag=.false.
        DesicDehum(DesicDehumNum)%MaxCoilFluidFlow = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                     DesicDehum(DesicDehumNum)%RegenCoilName,ErrorFlag)
        IF(DesicDehum(DesicDehumNum)%MaxCoilFluidFlow .GT. 0.0d0)THEN
          FluidDensity = GetDensityGlycol(PlantLoop(DesicDehum(DesicDehumNum)%LoopNum)%FluidName, &
                                InitConvTemp, &
                                PlantLoop(DesicDehum(DesicDehumNum)%LoopNum)%FluidIndex, &
                                'InitCBVAV')
          DesicDehum(DesicDehumNum)%MaxCoilFluidFlow = DesicDehum(DesicDehumNum)%MaxCoilFluidFlow * FluidDensity
        END IF

      ELSEIF (DesicDehum(DesicDehumNum)%RegenCoilType_Num == Coil_HeatingSteam) THEN

        ErrorFlag=.false.
        CALL ScanPlantLoopsForObject( DesicDehum(DesicDehumNum)%RegenCoilName, &
                                      TypeOf_CoilSteamAirHeating , &
                                      DesicDehum(DesicDehumNum)%LoopNum, &
                                      DesicDehum(DesicDehumNum)%LoopSide, &
                                      DesicDehum(DesicDehumNum)%BranchNum, &
                                      DesicDehum(DesicDehumNum)%CompNum,   &
                                      ErrFlag=ErrorFlag)

        IF (ErrorFlag) THEN
          CALL ShowFatalError('InitDesiccantDehumidifier: Program terminated for previous conditions.')
        ENDIF
        ErrorFlag=.false.
        DesicDehum(DesicDehumNum)%MaxCoilFluidFlow = &
                            GetCoilMaxSteamFlowRate(DesicDehum(DesicDehumNum)%RegenCoilIndex,ErrorFlag)

        IF(DesicDehum(DesicDehumNum)%MaxCoilFluidFlow .GT. 0.0d0)THEN
          SteamIndex = 0 ! Function GetSatDensityRefrig will look up steam index if 0 is passed
          FluidDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamIndex,'InitDesiccantDehumidifier')
          DesicDehum(DesicDehumNum)%MaxCoilFluidFlow = DesicDehum(DesicDehumNum)%MaxCoilFluidFlow  * FluidDensity
        END IF

      ENDIF

      ! fill outlet node for regenartion hot water or steam heating coil
      DesicDehum(DesicDehumNum)%CoilOutletNode =  &
            PlantLoop(DesicDehum(DesicDehumNum)%LoopNum)%LoopSide(DesicDehum(DesicDehumNum)%LoopSide) &
                      %Branch(DesicDehum(DesicDehumNum)%BranchNum)%Comp(DesicDehum(DesicDehumNum)%CompNum)%NodeNumOut
      MyPlantScanFlag(DesicDehumNum) = .FALSE.

    ELSE ! DesicDehum is not connected to plant
      MyPlantScanFlag(DesicDehumNum) = .FALSE.
    ENDIF
  ELSEIF (MyPlantScanFlag(DesicDehumNum) .AND. .NOT. AnyPlantInModel) THEN
    MyPlantScanFlag(DesicDehumNum) = .FALSE.
  ENDIF

  SELECT CASE ((DesicDehum(DesicDehumNum)%DehumTypeCode))

    CASE (Solid)

       IF ( .NOT. SysSizingCalc .AND. MySetPointCheckFlag .AND. DoSetPointTest) THEN
        IF (DesicDehum(DesicDehumNum)%ControlType == NodeHumratBypass) THEN
        ControlNode = DesicDehum(DesicDehumNum)%ProcAirOutNode
         IF (ControlNode > 0) THEN
           IF (Node(ControlNode)%HumRatMax == SensedNodeFlagValue) THEN
             IF (.NOT. AnyEnergyManagementSystemInModel) THEN
               CALL ShowSevereError('Missing humidity ratio setpoint (HumRatMax) for ')
               CALL ShowContinueError('Dehumidifier:Desiccant:NoFans: '//TRIM(DesicDehum(DesicDehumNum)%Name))
               CALL ShowContinueError('Node Referenced='//TRIM(NodeID(ControlNode)))
               CALL ShowContinueError('use a Setpoint Manager to establish a setpoint at the process air outlet node.')
               SetPointErrorFlag = .TRUE.
             ELSE
               CALL CheckIfNodeSetpointManagedByEMS(ControlNode,iHumidityRatioMaxSetpoint, SetpointErrorFlag)
               IF (SetPointErrorFlag) THEN
                 CALL ShowSevereError('Missing humidity ratio setpoint (HumRatMax) for ')
                 CALL ShowContinueError('Dehumidifier:Desiccant:NoFans: '//TRIM(DesicDehum(DesicDehumNum)%Name))
                 CALL ShowContinueError('Node Referenced='//TRIM(NodeID(ControlNode)))
                 CALL ShowContinueError('use a Setpoint Manager to establish a setpoint at the process air outlet node.')
                 CALL ShowContinueError('Or use EMS Actuator to establish a setpoint at the process air outlet node.')
               ENDIF
             ENDIF
           END IF
         END IF
        END IF
        MySetPointCheckFlag = .FALSE.
       END IF
       ! always do these initializations every iteration
       ProcInNode  = DesicDehum(DesicDehumNum)%ProcAirInNode
       DesicDehum(DesicDehumNum)%ProcAirInTemp = Node(ProcInNode)%Temp
       DesicDehum(DesicDehumNum)%ProcAirInHumRat = Node(ProcInNode)%HumRat
       DesicDehum(DesicDehumNum)%ProcAirInEnthalpy = Node(ProcInNode)%Enthalpy
       DesicDehum(DesicDehumNum)%ProcAirInMassFlowRate = Node(ProcInNode)%MassFlowRate

      !  Determine heating coil inlet conditions by calling it with zero load
      !  Not sure if this is really a good way to do this, should revisit for next release.
      CALL CalcNonDXHeatingCoils(DesicDehumNum,FirstHVACIteration,0.0d0)

      RegenInNode  = DesicDehum(DesicDehumNum)%RegenAirInNode
      DesicDehum(DesicDehumNum)%RegenAirInTemp = Node(RegenInNode)%Temp
      DesicDehum(DesicDehumNum)%RegenAirInHumRat = Node(RegenInNode)%HumRat
      DesicDehum(DesicDehumNum)%RegenAirInEnthalpy = Node(RegenInNode)%Enthalpy

      DesicDehum(DesicDehumNum)%WaterRemove = 0.0d0
      DesicDehum(DesicDehumNum)%ElecUseEnergy = 0.0d0
      DesicDehum(DesicDehumNum)%ElecUseRate = 0.0d0

    CASE (Generic)

!      Do the Begin Environment initializations
       IF (BeginEnvrnFlag .and. MyEnvrnFlag(DesicDehumNum)) THEN
         !Change the Volume Flow Rates to Mass Flow Rates
          DesicDehum(DesicDehumNum)%ExhaustFanMaxMassFlowRate = DesicDehum(DesicDehumNum)%ExhaustFanMaxVolFlowRate * &
                                          StdRhoAir

         !   set fluid-side hardware limits
         IF(DesicDehum(DesicDehumNum)%CoilControlNode .GT. 0)THEN
           !    If water coil max water flow rate is autosized, simulate once in order to mine max water flow rate
           IF(DesicDehum(DesicDehumNum)%MaxCoilFluidFlow .EQ. Autosize)THEN
             IF (DesicDehum(DesicDehumNum)%RegenCoilType_Num == Coil_HeatingWater) THEN
                CALL SimulateWaterCoilComponents(DesicDehum(DesicDehumNum)%RegenCoilName,FirstHVACIteration, &
                                                 DesicDehum(DesicDehumNum)%RegenCoilIndex)
                ErrorFlag = .FALSE.
                CoilMaxVolFlowRate = GetCoilMaxWaterFlowRate('Coil:Heating:Water',  &
                                                             DesicDehum(DesicDehumNum)%RegenCoilName,ErrorFlag)
                IF (ErrorFlag) Then
                  ErrorsFound = .TRUE.
                ENDIF
                IF(CoilMaxVolFlowRate .NE. Autosize) THEN
                  FluidDensity = GetDensityGlycol(PlantLoop(DesicDehum(DesicDehumNum)%LoopNum)%fluidName, &
                                                  InitConvTemp, &
                                                  PlantLoop(DesicDehum(DesicDehumNum)%LoopNum)%fluidIndex, &
                                                  'InitDesiccantDehumidifier')
                  DesicDehum(DesicDehumNum)%MaxCoilFluidFlow = CoilMaxVolFlowRate * FluidDensity
                ENDIF
            ENDIF
            IF (DesicDehum(DesicDehumNum)%RegenCoilType_Num == Coil_HeatingSteam) THEN
                CALL SimulateSteamCoilComponents(DesicDehum(DesicDehumNum)%RegenCoilName, &
                                                 FirstHVACIteration,    &
                                                 1.0d0, & !simulate any load > 0 to get max capacity of steam coil
                                                 DesicDehum(DesicDehumNum)%RegenCoilIndex, QCoilActual)
                ErrorFlag = .FALSE.
                CoilMaxVolFlowRate = GetCoilMaxSteamFlowRate(DesicDehum(DesicDehumNum)%RegenCoilIndex,ErrorFlag)
                IF (ErrorFlag) Then
                  ErrorsFound = .TRUE.
                ENDIF
                IF(CoilMaxVolFlowRate .NE. Autosize) THEN
                  SteamIndex = 0             ! Function GetSatDensityRefrig will look up steam index if 0 is passed
                  FluidDensity=GetSatDensityRefrig('STEAM',TempSteamIn,1.0d0,SteamIndex,'InitDesiccantDehumidifier')
                  DesicDehum(DesicDehumNum)%MaxCoilFluidFlow = CoilMaxVolFlowRate * FluidDensity
                ENDIF
            ENDIF
           ENDIF
           Call InitComponentNodes(0.d0, DesicDehum(DesicDehumNum)%MaxCoilFluidFlow, &
                                        DesicDehum(DesicDehumNum)%CoilControlNode, &
                                        DesicDehum(DesicDehumNum)%CoilOutletNode, &
                                        DesicDehum(DesicDehumNum)%LoopNum, &
                                        DesicDehum(DesicDehumNum)%LoopSide, &
                                        DesicDehum(DesicDehumNum)%BranchNum, &
                                        DesicDehum(DesicDehumNum)%CompNum)
         END IF

          MyEnvrnFlag(DesicDehumNum) = .FALSE.
       END IF

       IF ( .NOT. SysSizingCalc .AND. MySetPointCheckFlag .AND. DoSetPointTest) THEN
         ControlNode = DesicDehum(DesicDehumNum)%ControlNodeNum
         IF (ControlNode > 0) THEN
           IF (Node(ControlNode)%HumRatMax == SensedNodeFlagValue) THEN
             IF (.NOT. AnyEnergyManagementSystemInModel) THEN
               CALL ShowSevereError('Missing maximum humidity ratio setpoint (MaxHumRat) for ')
               CALL ShowContinueError(TRIM(DesicDehum(DesicDehumNum)%DehumType)// ': '//TRIM(DesicDehum(DesicDehumNum)%Name))
               CALL ShowContinueError('Node Referenced='//TRIM(NodeID(ControlNode)))
               CALL ShowContinueError('use a Setpoint Manager to establish a "MaxHumRat" setpoint'// &
                                      ' at the process air control node.')
               SetPointErrorFlag = .TRUE.
             ELSE
               CALL CheckIfNodeSetpointManagedByEMS(ControlNode,iHumidityRatioMaxSetpoint, SetpointErrorFlag)
               IF (SetPointErrorFlag) THEN
                 CALL ShowSevereError('Missing maximum humidity ratio setpoint (MaxHumRat) for ')
                 CALL ShowContinueError(TRIM(DesicDehum(DesicDehumNum)%DehumType)// ': '//TRIM(DesicDehum(DesicDehumNum)%Name))
                 CALL ShowContinueError('Node Referenced='//TRIM(NodeID(ControlNode)))
                 CALL ShowContinueError('use a Setpoint Manager to establish a "MaxHumRat" setpoint'// &
                                        ' at the process air control node.')
                 CALL ShowContinueError('Or use EMS Actuator to establish a setpoint at the process air outlet node.')
               ENDIF
             ENDIF


           END IF
         END IF
         MySetPointCheckFlag = .FALSE.
       END IF
       RegenInNode  = DesicDehum(DesicDehumNum)%RegenAirInNode
       DesicDehum(DesicDehumNum)%RegenAirInTemp = Node(RegenInNode)%Temp
       DesicDehum(DesicDehumNum)%RegenAirInMassFlowRate = Node(RegenInNode)%MassFlowRate

       DesicDehum(DesicDehumNum)%ExhaustFanPower = 0.0d0
       DesicDehum(DesicDehumNum)%WaterRemoveRate = 0.0d0

  END SELECT


  RETURN

END SUBROUTINE InitDesiccantDehumidifier

SUBROUTINE ControlDesiccantDehumidifier(DesicDehumNum,HumRatNeeded,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
          !                      for Gas Research Institute
          !       DATE WRITTEN   March 2001
          !       MODIFIED       Jan 2005 M. J. Witte, GARD Analytics, Inc.
          !                        Add new control type option:
          !                          NODE LEAVING HUMRAT SETPOINT:BYPASS
          !                        Change existing control type to:
          !                          FIXED LEAVING HUMRAT SETPOINT:BYPASS
          !                        Work supported by ASHRAE research project 1254-RP
          !                      June 2007 R. Raustad, FSEC
          !                        Added new dehumidifier type -- DESICCANT DEHUMIDIFIER
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets the output required from the dehumidifier

          ! METHODOLOGY EMPLOYED:
          ! Uses a maximum humidity ratio setpoint to calculate required process
          ! leaving humidity ratio

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: DesicDehumNum      ! number of the current dehumidifier being simulated
  REAL(r64),    INTENT(OUT) :: HumRatNeeded       ! process air leaving humidity ratio set by controller [kg water/kg air]
  LOGICAL, INTENT (IN) :: FirstHVACIteration ! TRUE if 1st HVAC simulation of system timestep !unused1208

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: UnitOn               ! unit on flag
  REAL(r64)    :: ProcAirMassFlowRate  ! process air mass flow rate [kg/s]
  REAL(r64)    :: RegenAirMassFlowRate ! regen air mass flow rate [kg/s]

  ProcAirMassFlowRate = 0.0d0
  RegenAirMassFlowRate = 0.0d0
  UnitOn = .TRUE.

  SELECT CASE ((DesicDehum(DesicDehumNum)%DehumTypeCode))

    CASE (Solid)

       IF (DesicDehum(DesicDehumNum)%HumRatSet .LE. 0.0d0) UnitOn = .FALSE.
       ProcAirMassFlowRate = DesicDehum(DesicDehumNum)%ProcAirInMassFlowRate
       IF (ProcAirMassFlowRate .LE. SmallMassFlow) UnitOn = .FALSE.

       IF (GetCurrentScheduleValue(DesicDehum(DesicDehumNum)%SchedPtr) .LE. 0.0d0)  UnitOn = .FALSE.

       ! If incoming conditions are outside valid range for curve fits, then shut unit off, do not issue warnings

       IF (UnitOn) THEN
         IF((DesicDehum(DesicDehumNum)%ProcAirInTemp   .LT. DesicDehum(DesicDehumNum)%MinProcAirInTemp) .OR. &
            (DesicDehum(DesicDehumNum)%ProcAirInTemp   .GT. DesicDehum(DesicDehumNum)%MaxProcAirInTemp)) THEN
            UnitOn = .FALSE.
         ENDIF
         IF((DesicDehum(DesicDehumNum)%ProcAirInHumRat .LT. DesicDehum(DesicDehumNum)%MinProcAirInHumRat) .OR. &
            (DesicDehum(DesicDehumNum)%ProcAirInHumRat .GT. DesicDehum(DesicDehumNum)%MaxProcAirInHumRat)) THEN
            UnitOn = .FALSE.
         ENDIF
       ENDIF

       IF (UnitOn) THEN

    ! perform the correct dehumidifier control strategy
         SELECT CASE(DesicDehum(DesicDehumNum)%ControlType)

           CASE (FixedHumratBypass)

             HumRatNeeded = DesicDehum(DesicDehumNum)%HumRatSet
             IF (HumRatNeeded <= 0.0d0) THEN
               CALL ShowSevereError('Dehumidifier:Desiccant:NoFans: '//TRIM(DesicDehum(DesicDehumNum)%Name))
               CALL ShowContinueError('Invalid Leaving Max Humidity Ratio Setpoint='//TRIM(TrimSigDigits(HumRatNeeded,8)))
               CALL ShowFatalError('must be > 0.0')
             END IF

           CASE (NodeHumratBypass)

            HumRatNeeded = Node(DesicDehum(DesicDehumNum)%ProcAirOutNode)%HumRatMax

           CASE DEFAULT

             CALL ShowFatalError('Invalid control type in desiccant dehumidifier = '//TRIM(DesicDehum(DesicDehumNum)%Name))

         END SELECT

          ! Setpoint of zero indicates no load from setpoint manager max hum
         IF ((HumRatNeeded == 0.0d0) .OR. &
             (DesicDehum(DesicDehumNum)%ProcAirInHumRat .LE. HumRatNeeded)) THEN
             UnitOn = .FALSE.
             HumRatNeeded = DesicDehum(DesicDehumNum)%ProcAirInHumRat
         ENDIF
       ELSE
         HumRatNeeded = DesicDehum(DesicDehumNum)%ProcAirInHumRat
       END IF

    CASE (Generic)

       ProcAirMassFlowRate = Node(DesicDehum(DesicDehumNum)%ProcAirInNode)%MassFlowRate
       IF (ProcAirMassFlowRate .LE. SmallMassFlow) UnitOn = .FALSE.

       IF (GetCurrentScheduleValue(DesicDehum(DesicDehumNum)%SchedPtr) .LE. 0.0d0)  UnitOn = .FALSE.

       IF (UnitOn) THEN
         IF(DesicDehum(DesicDehumNum)%ControlNodeNum .EQ. DesicDehum(DesicDehumNum)%ProcAirOutNode)THEN
           HumRatNeeded = Node(DesicDehum(DesicDehumNum)%ControlNodeNum)%HumRatMax
         ELSE
           IF(Node(DesicDehum(DesicDehumNum)%ControlNodeNum)%HumRatMax .GT. 0.0d0)THEN
             HumRatNeeded = Node(DesicDehum(DesicDehumNum)%ControlNodeNum)%HumRatMax - &
                (Node(DesicDehum(DesicDehumNum)%ControlNodeNum)%HumRat - Node(DesicDehum(DesicDehumNum)%ProcAirOutNode)%HumRat)
           ELSE
             HumRatNeeded = 0.0d0
           END IF
         END IF

         ! Setpoint of zero indicates no load from setpoint manager max hum
         IF ((HumRatNeeded == 0.0d0) .OR. &
             (Node(DesicDehum(DesicDehumNum)%ProcAirInNode)%HumRat .LE. HumRatNeeded)) THEN
             HumRatNeeded = Node(DesicDehum(DesicDehumNum)%ProcAirInNode)%HumRat
         ENDIF
       ELSE
         HumRatNeeded = Node(DesicDehum(DesicDehumNum)%ProcAirInNode)%HumRat
       END IF

    CASE DEFAULT


  END SELECT

  RETURN

END SUBROUTINE ControlDesiccantDehumidifier

SUBROUTINE CalcSolidDesiccantDehumidifier(DesicDehumNum,HumRatNeeded,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
          !                      for Gas Research Institute
          !       DATE WRITTEN   March 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate the electricity consumption, regen heat requirements and the outlet
          ! conditions for a solid desiccant dehumidifier, given the inlet conditions and
          ! and the needed process leaving humidity ratio.

          ! METHODOLOGY EMPLOYED:
          ! Given the entering conditions, the full-load outlet conditions are calculated.
          ! Adjust for part-load if required.
          ! Caclulate required regen energy and call regen coil and regen fan.
          ! Desiccant wheel leaving conditions and regen energy requirements are calculated
          ! from empirical curve fits.  The user can select either default built-in
          ! performance curves, or use custom user-defined curves.

          ! REFERENCES:
          ! The default performance curves represent a commerical-grade solid desiccant
          ! wheel typical of HVAC applications in the early 1990's.  These curves were
          ! developed for Gas Research Institute by William W. Worek, University of Illinois
          ! at Chicago.

          ! USE STATEMENTS:
  USE Psychrometrics, ONLY:PsyHFnTdbW, PsyRhoAirFnPbTdbW
!unused  USE DataEnvironment, ONLY: StdBaroPress

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: DesicDehumNum  ! number of the current dehumidifier being simulated
  REAL(r64), INTENT(IN)    :: HumRatNeeded ! process air leaving humidity ratio set by controller [kgWater/kgDryAir]
  LOGICAL,          INTENT (IN)  :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64) :: ProcAirInHumRat        ! process inlet air humidity ratio [kgWater/kgDryAir]
  REAL(r64) :: ProcAirInTemp          ! process inlet air temperature [C]
  REAL(r64) :: ProcAirOutHumRat        ! process outlet air humidity ratio [kgWater/kgDryAir]
  REAL(r64) :: MinProcAirOutHumRat    ! minimum available process outlet air humidity ratio [kgWater/kgDryAir]
  REAL(r64) :: ProcAirOutTemp          ! process outlet air temperature [C]
  REAL(r64) :: ProcAirVel              ! process air velocity [m/s]
  REAL(r64) :: QRegen                  ! regen heat input rate requested from regen coil [W]
  REAL(r64) :: QDelivered              ! regen heat actually delivered by regen coil [W]
  !REAL(r64) :: RegenAirInHumRat        ! regen inlet air humidity ratio [kgWater/kgDryAir]
  REAL(r64) :: RegenAirInTemp          ! regen inlet air temperature [C]
  REAL(r64) :: RegenAirVel             ! regen air velocity [m/s]
  REAL(r64) :: ProcAirMassFlowRate     ! process air mass flow rate [kg/s]
  REAL(r64) :: RegenAirMassFlowRate    ! regen air mass flow rate [kg/s]
  REAL(r64) :: SpecRegenEnergy         ! specific regen energy [J/kg of water removed]
  REAL(r64) :: NomRegenTemp            ! nominal regen temperature for regen energy curve
  REAL(r64) :: ElecUseRate             ! electricity consumption rate [W]
  REAL(r64) :: PartLoad            ! fraction of dehumidification capacity required to meet setpoint
  LOGICAL :: UnitOn       ! unit on flag

  LOGICAL, SAVE :: MyOneTimeFlag = .TRUE.     ! one time flag
  REAL(r64), SAVE    :: RhoAirStdInit

! Variables for hardwired coefficients for default performance model

  REAL(r64) :: TC0
  REAL(r64) :: TC1
  REAL(r64) :: TC2
  REAL(r64) :: TC3
  REAL(r64) :: TC4
  REAL(r64) :: TC5
  REAL(r64) :: TC6
  REAL(r64) :: TC7
  REAL(r64) :: TC8
  REAL(r64) :: TC9
  REAL(r64) :: TC10
  REAL(r64) :: TC11
  REAL(r64) :: TC12
  REAL(r64) :: TC13
  REAL(r64) :: TC14
  REAL(r64) :: TC15

  REAL(r64) :: WC0
  REAL(r64) :: WC1
  REAL(r64) :: WC2
  REAL(r64) :: WC3
  REAL(r64) :: WC4
  REAL(r64) :: WC5
  REAL(r64) :: WC6
  REAL(r64) :: WC7
  REAL(r64) :: WC8
  REAL(r64) :: WC9
  REAL(r64) :: WC10
  REAL(r64) :: WC11
  REAL(r64) :: WC12
  REAL(r64) :: WC13
  REAL(r64) :: WC14
  REAL(r64) :: WC15

  REAL(r64) :: QC0
  REAL(r64) :: QC1
  REAL(r64) :: QC2
  REAL(r64) :: QC3
  REAL(r64) :: QC4
  REAL(r64) :: QC5
  REAL(r64) :: QC6
  REAL(r64) :: QC7
  REAL(r64) :: QC8
  REAL(r64) :: QC9
  REAL(r64) :: QC10
  REAL(r64) :: QC11
  REAL(r64) :: QC12
  REAL(r64) :: QC13
  REAL(r64) :: QC14
  REAL(r64) :: QC15

  REAL(r64) :: RC0
  REAL(r64) :: RC1
  REAL(r64) :: RC2
  REAL(r64) :: RC3
  REAL(r64) :: RC4
  REAL(r64) :: RC5
  REAL(r64) :: RC6
  REAL(r64) :: RC7
  REAL(r64) :: RC8
  REAL(r64) :: RC9
  REAL(r64) :: RC10
  REAL(r64) :: RC11
  REAL(r64) :: RC12
  REAL(r64) :: RC13
  REAL(r64) :: RC14
  REAL(r64) :: RC15

! Setup internal variables for calculations

  ProcAirInTemp         = DesicDehum(DesicDehumNum)%ProcAirInTemp
  ProcAirInHumRat       = DesicDehum(DesicDehumNum)%ProcAirInHumRat
  ProcAirMassFlowRate   = DesicDehum(DesicDehumNum)%ProcAirInMassFlowRate
  ProcAirVel            = DesicDehum(DesicDehumNum)%NomProcAirVel
  PartLoad              = 0.0d0

  RegenAirInTemp        = DesicDehum(DesicDehumNum)%RegenAirInTemp
  NomRegenTemp          = DesicDehum(DesicDehumNum)%NomRegenTemp

! Calculate min available process out humrat
UnitOn = .FALSE.
MinProcAirOutHumRat = 0.0d0 ! max(MinProcAirOutHumRat,0.000857)

IF (HumRatNeeded .lt. ProcAirInHumRat) THEN

  UnitOn = .TRUE.

  SELECT CASE(DesicDehum(DesicDehumNum)%PerformanceModel_Num) ! Performance Model Part A

    CASE (PM_Default)

      WC0  = 0.0148880824323806d0
      WC1  = -0.000283393198398211d0
      WC2  = -0.87802168940547d0
      WC3  = -0.000713615831236411d0
      WC4  = 0.0311261188874622d0
      WC5  = 1.51738892142485d-06
      WC6  = 0.0287250198281021d0
      WC7  = 4.94796903231558d-06
      WC8  = 24.0771139652826d0
      WC9  = 0.000122270283927978d0
      WC10 = -0.0151657189566474d0
      WC11 = 3.91641393230322d-08
      WC12 = 0.126032651553348d0
      WC13 = 0.000391653854431574d0
      WC14 = 0.002160537360507d0
      WC15 = 0.00132732844211593d0

      MinProcAirOutHumRat   =  WC0 &
        + WC1*ProcAirInTemp + WC2*ProcAirInHumRat + WC3*ProcAirVel &
        + WC4*ProcAirInTemp*ProcAirInHumRat + WC5*ProcAirInTemp*ProcAirVel &
        + WC6*ProcAirInHumRat*ProcAirVel + WC7*ProcAirInTemp*ProcAirInTemp &
        + WC8*ProcAirInHumRat*ProcAirInHumRat + WC9*ProcAirVel*ProcAirVel &
        + WC10*ProcAirInTemp*ProcAirInTemp*ProcAirInHumRat*ProcAirInHumRat &
        + WC11*ProcAirInTemp*ProcAirInTemp*ProcAirVel*ProcAirVel &
        + WC12*ProcAirInHumRat*ProcAirInHumRat*ProcAirVel*ProcAirVel &
        + WC13*LOG(ProcAirInTemp) + WC14*LOG(ProcAirInHumRat) &
        + WC15*LOG(ProcAirVel)

      ! limit to 6 grains/lb (0.000857 kg/kg)

    CASE (PM_UserCurves)

      MinProcAirOutHumRat = CurveValue(DesicDehum(DesicDehumNum)%ProcHumRatCurvefTW,ProcAirInTemp,ProcAirInHumRat)&
         * CurveValue(DesicDehum(DesicDehumNum)%ProcHumRatCurvefV,ProcAirVel)


    CASE DEFAULT

      CALL ShowFatalError('Invalid performance model in desiccant dehumidifier = '&
           //TRIM(TrimSigDigits(DesicDehum(DesicDehumNum)%PerformanceModel_Num)))

  END SELECT ! Performance Model Part A

  MinProcAirOutHumRat = max(MinProcAirOutHumRat,0.000857d0)

ENDIF

IF (MinProcAirOutHumRat .GE. ProcAirInHumRat) UnitOn=.FALSE.

IF (UnitOn) THEN

  ! Calculate partload fraction of dehumidification capacity required to meet setpoint
  PartLoad = 1.0d0
  IF (MinProcAirOutHumRat .LT. HumRatNeeded) &
    PartLoad = (ProcAirInHumRat - HumRatNeeded) / (ProcAirInHumRat - MinProcAirOutHumRat)
  PartLoad = MAX(0.0d0,PartLoad)
  PartLoad = MIN(1.0d0,PartLoad)

  SELECT CASE(DesicDehum(DesicDehumNum)%PerformanceModel_Num) ! Performance Model Part B

    CASE (PM_Default)

      ! Calculate leaving conditions
      TC0  = -38.7782841989449d0
      TC1  = 2.0127655837628d0
      TC2  = 5212.49360216097d0
      TC3  = 15.2362536782665d0
      TC4  = -80.4910419759181d0
      TC5  = -0.105014122001509d0
      TC6  = -229.668673645144d0
      TC7  = -0.015424703743461d0
      TC8  = -69440.0689831847d0
      TC9  = -1.6686064694322d0
      TC10 = 38.5855718977592d0
      TC11 = 0.000196395381206009d0
      TC12 = 386.179386548324d0
      TC13 = -0.801959614172614d0
      TC14 = -3.33080986818745d0
      TC15 = -15.2034386065714d0

      ProcAirOutTemp   =  TC0 &
        + TC1*ProcAirInTemp + TC2*ProcAirInHumRat + TC3*ProcAirVel &
        + TC4*ProcAirInTemp*ProcAirInHumRat + TC5*ProcAirInTemp*ProcAirVel &
        + TC6*ProcAirInHumRat*ProcAirVel + TC7*ProcAirInTemp*ProcAirInTemp &
        + TC8*ProcAirInHumRat*ProcAirInHumRat + TC9*ProcAirVel*ProcAirVel &
        + TC10*ProcAirInTemp*ProcAirInTemp*ProcAirInHumRat*ProcAirInHumRat &
        + TC11*ProcAirInTemp*ProcAirInTemp*ProcAirVel*ProcAirVel &
        + TC12*ProcAirInHumRat*ProcAirInHumRat*ProcAirVel*ProcAirVel &
        + TC13*LOG(ProcAirInTemp) + TC14*LOG(ProcAirInHumRat) &
        + TC15*LOG(ProcAirVel)

      ! Regen energy
      QC0  = -27794046.6291107d0
      QC1  = -235725.171759615d0
      QC2  = 975461343.331328d0
      QC3  = -686069.373946731d0
      QC4  = -17717307.3766266d0
      QC5  = 31482.2539662489d0
      QC6  = 55296552.8260743d0
      QC7  = 6195.36070023868d0
      QC8  = -8304781359.40435d0
      QC9  = -188987.543809419d0
      QC10 = 3933449.40965846d0
      QC11 = -6.66122876558634d0
      QC12 = -349102295.417547d0
      QC13 = 83672.179730172d0
      QC14 = -6059524.33170538d0
      QC15 = 1220523.39525162d0

      SpecRegenEnergy   =  QC0 &
        + QC1*ProcAirInTemp + QC2*ProcAirInHumRat + QC3*ProcAirVel &
        + QC4*ProcAirInTemp*ProcAirInHumRat + QC5*ProcAirInTemp*ProcAirVel &
        + QC6*ProcAirInHumRat*ProcAirVel + QC7*ProcAirInTemp*ProcAirInTemp &
        + QC8*ProcAirInHumRat*ProcAirInHumRat + QC9*ProcAirVel*ProcAirVel &
        + QC10*ProcAirInTemp*ProcAirInTemp*ProcAirInHumRat*ProcAirInHumRat &
        + QC11*ProcAirInTemp*ProcAirInTemp*ProcAirVel*ProcAirVel &
        + QC12*ProcAirInHumRat*ProcAirInHumRat*ProcAirVel*ProcAirVel &
        + QC13*LOG(ProcAirInTemp) + QC14*LOG(ProcAirInHumRat) &
        + QC15*LOG(ProcAirVel)

      ! Regen face velocity
      RC0 = -4.67358908091488d0
      RC1 = 0.0654323095468338d0
      RC2 = 396.950518702316d0
      RC3 = 1.52610165426736d0
      RC4 = -11.3955868430328d0
      RC5 = 0.00520693906104437d0
      RC6 = 57.783645385621d0
      RC7 = -0.000464800668311693d0
      RC8 = -5958.78613212602d0
      RC9 = -0.205375818291012d0
      RC10 = 5.26762675442845d0
      RC11 = -8.88452553055039d-05
      RC12 = -182.382479369311d0
      RC13 = -0.100289774002047d0
      RC14 = -0.486980507964251d0
      RC15 = -0.972715425435447d0

      RegenAirVel   =  RC0 &
        + RC1*ProcAirInTemp + RC2*ProcAirInHumRat + RC3*ProcAirVel &
        + RC4*ProcAirInTemp*ProcAirInHumRat + RC5*ProcAirInTemp*ProcAirVel &
        + RC6*ProcAirInHumRat*ProcAirVel + RC7*ProcAirInTemp*ProcAirInTemp &
        + RC8*ProcAirInHumRat*ProcAirInHumRat + RC9*ProcAirVel*ProcAirVel &
        + RC10*ProcAirInTemp*ProcAirInTemp*ProcAirInHumRat*ProcAirInHumRat &
        + RC11*ProcAirInTemp*ProcAirInTemp*ProcAirVel*ProcAirVel &
        + RC12*ProcAirInHumRat*ProcAirInHumRat*ProcAirVel*ProcAirVel &
        + RC13*LOG(ProcAirInTemp) + RC14*LOG(ProcAirInHumRat) &
        + RC15*LOG(ProcAirVel)

    CASE (PM_UserCurves)

      ProcAirOutTemp = CurveValue(DesicDehum(DesicDehumNum)%ProcDryBulbCurvefTW,ProcAirInTemp,ProcAirInHumRat)&
         * CurveValue(DesicDehum(DesicDehumNum)%ProcDryBulbCurvefV,ProcAirVel)

      SpecRegenEnergy = CurveValue(DesicDehum(DesicDehumNum)%RegenEnergyCurvefTW,ProcAirInTemp,ProcAirInHumRat)&
         * CurveValue(DesicDehum(DesicDehumNum)%RegenEnergyCurvefV,ProcAirVel)

      RegenAirVel = CurveValue(DesicDehum(DesicDehumNum)%RegenVelCurvefTW,ProcAirInTemp,ProcAirInHumRat)&
         * CurveValue(DesicDehum(DesicDehumNum)%RegenVelCurvefV,ProcAirVel)


    CASE DEFAULT

      CALL ShowFatalError('Invalid performance model in desiccant dehumidifier = '&
            //TRIM(TrimSigDigits(DesicDehum(DesicDehumNum)%PerformanceModel_Num)))

  END SELECT ! Performance Model Part B


  ProcAirOutTemp = (1-PartLoad)*ProcAirInTemp + (PartLoad)*ProcAirOutTemp

  ProcAirOutHumRat =  (1-PartLoad)*ProcAirInHumRat + (PartLoad)*MinProcAirOutHumRat

  ! Calculate water removal
  DesicDehum(DesicDehumNum)%WaterRemoveRate = ProcAirMassFlowRate * (ProcAirInHumRat - ProcAirOutHumRat)

  ! Adjust for regen inlet temperature
  SpecRegenEnergy = SpecRegenEnergy * (NomRegenTemp - RegenAirInTemp) / (NomRegenTemp - ProcAirInTemp)
  SpecRegenEnergy = MAX(SpecRegenEnergy, 0.0d0)
  QRegen = SpecRegenEnergy*DesicDehum(DesicDehumNum)%WaterRemoveRate

  ! Above curves are based on a 90deg regen angle and 245deg process air angle
  RegenAirMassFlowRate = ProcAirMassFlowRate * 90.d0/245.d0 * RegenAirVel/ProcAirVel

  ElecUseRate = DesicDehum(DesicDehumNum)%NomRotorPower

ELSE  ! Unit is off

  ProcAirOutTemp   = ProcAirInTemp
  ProcAirOutHumRat = ProcAirInHumRat
  SpecRegenEnergy = 0.0d0
  QRegen = 0.0d0
  ElecUseRate = 0.0d0
  RegenAirVel = 0.0d0
  RegenAirMassFlowRate = 0.0d0
  DesicDehum(DesicDehumNum)%WaterRemoveRate = 0.0d0
  PartLoad = 0.0d0

ENDIF ! UnitOn/Off

! Set regen mass flow
  Node(DesicDehum(DesicDehumNum)%RegenFanInNode)%MassFlowRate = RegenAirMassFlowRate
  Node(DesicDehum(DesicDehumNum)%RegenFanInNode)%MassFlowRateMaxAvail = RegenAirMassFlowRate
! Call regen fan
  CALL SimulateFanComponents(DesicDehum(DesicDehumNum)%RegenFanName,FirstHVACIteration, &
                             DesicDehum(DesicDehumNum)%RegenFanIndex)
! Call regen heating coil
  CALL CalcNonDXHeatingCoils(DesicDehumNum,FirstHVACIteration,QRegen,QDelivered)

! Verify is requestd flow was delivered (must do after heating coil has executed to pass flow to RegenAirInNode)
  IF (Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate .NE. RegenAirMassFlowRate) THEN
    ! Initialize standard air density
    IF (MyOneTimeFlag) THEN
      RhoAirStdInit = StdRhoAir
    ENDIF
    CALL ShowRecurringSevereErrorAtEnd( &
           'Improper flow delivered by desiccant regen fan - RESULTS INVALID! Check regen fan capacity and schedule.', &
           DesicDehum(DesicDehumNum)%RegenFanErrorIndex1)
    CALL ShowRecurringContinueErrorAtEnd( &
           TRIM(DesicDehum(DesicDehumNum)%DehumType)//'='//TRIM(DesicDehum(DesicDehumNum)%Name), &
           DesicDehum(DesicDehumNum)%RegenFanErrorIndex2)
    RhoAirStdInit = StdRhoAir
    CALL ShowRecurringContinueErrorAtEnd( &
           TRIM('Flow requested [m3/s] from '//DesicDehum(DesicDehumNum)%RegenFanType)//'='// &
           TRIM(DesicDehum(DesicDehumNum)%RegenFanName), &
           DesicDehum(DesicDehumNum)%RegenFanErrorIndex3, &
           ReportMaxOf=(RegenAirMassFlowRate / RhoAirStdInit))
    CALL ShowRecurringContinueErrorAtEnd( &
           'Flow request varied from delivered by [m3/s]', &
           DesicDehum(DesicDehumNum)%RegenFanErrorIndex4, &
           ReportMaxOf=((RegenAirMassFlowRate - Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate)/ RhoAirStdInit), &
           ReportMinOf=((RegenAirMassFlowRate - Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate)/ RhoAirStdInit))
  ENDIF

! Verify is requestd heating was delivered
  IF (QDelivered .LT. QRegen) THEN
    CALL ShowRecurringSevereErrorAtEnd( &
           'Inadequate heat delivered by desiccant regen coil - RESULTS INVALID! Check regen coil capacity and schedule.', &
           DesicDehum(DesicDehumNum)%RegenCapErrorIndex1)
    CALL ShowRecurringContinueErrorAtEnd( &
           TRIM(DesicDehum(DesicDehumNum)%DehumType)//'='//TRIM(DesicDehum(DesicDehumNum)%Name), &
           DesicDehum(DesicDehumNum)%RegenCapErrorIndex2)
    CALL ShowRecurringContinueErrorAtEnd( &
           TRIM('Load requested [W] from '//DesicDehum(DesicDehumNum)%RegenCoilType)//'='// &
           TRIM(DesicDehum(DesicDehumNum)%RegenCoilName), &
           DesicDehum(DesicDehumNum)%RegenCapErrorIndex3,ReportMaxOf=QRegen)
    CALL ShowRecurringContinueErrorAtEnd( &
           'Load request exceeded delivered by [W]', &
           DesicDehum(DesicDehumNum)%RegenCapErrorIndex4,ReportMaxOf=(QRegen-QDelivered))
  ENDIF

  DesicDehum(DesicDehumNum)%SpecRegenEnergy = SpecRegenEnergy
  DesicDehum(DesicDehumNum)%QRegen = QRegen
  DesicDehum(DesicDehumNum)%ElecUseRate = ElecUseRate
  DesicDehum(DesicDehumNum)%PartLoad = PartLoad

  DesicDehum(DesicDehumNum)%ProcAirOutMassFlowRate = ProcAirMassFlowRate
  DesicDehum(DesicDehumNum)%ProcAirOutTemp         = ProcAirOutTemp
  DesicDehum(DesicDehumNum)%ProcAirOutHumRat       = ProcAirOutHumRat
  DesicDehum(DesicDehumNum)%ProcAirOutEnthalpy     = PsyHFnTdbW(ProcAirOutTemp,ProcAirOutHumRat)
  DesicDehum(DesicDehumNum)%RegenAirInMassFlowRate = RegenAirMassFlowRate
  DesicDehum(DesicDehumNum)%RegenAirVel = RegenAirVel

!  DesicDehum(DesicDehumNum)%RegenAirOutTemp        = -999.
!  DesicDehum(DesicDehumNum)%RegenAirOutHumRat      = -999.
!  DesicDehum(DesicDehumNum)%RegenAirOutEnthalpy    = -999.
  RETURN
!

END SUBROUTINE CalcSolidDesiccantDehumidifier

SUBROUTINE CalcGenericDesiccantDehumidifier(DesicDehumNum,HumRatNeeded,FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Mangesh Basarkar, FSEC
          !       DATE WRITTEN   May 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate the electricity consumption, regen heat requirements and the outlet
          ! conditions for a desiccant dehumidifier, given the inlet conditions,
          ! DX coil part-load ratio, and/or the needed process leaving humidity ratio.

          ! METHODOLOGY EMPLOYED:
          ! Given the entering conditions, the full-load outlet conditions are calculated.
          ! Adjust for part-load if required.
          ! Calculate the required regen energy and call the regen coil and the regen fan.

          ! REFERENCES:
          ! Kosar, D. 2006. Dehumidification Enhancements, ASHRAE Journal, Vol. 48, No. 2, February 2006.
          ! Kosar, D. et al. 2006. Dehumidification Enhancement of Direct Expansion Systems Through Component
          !   Augmentation of the Cooling Coil. 15th Symposium on Improving Building Systems in Hot and Humid
          !   Climates, July 24-26, 2006.

          ! USE STATEMENTS:
  USE Psychrometrics,  ONLY: PsyHFnTdbW, PsyRhoAirFnPbTdbW
!unused  USE DataEnvironment, ONLY: StdBaroPress
  USE HeatRecovery,    ONLY: SimHeatRecovery
  USE DXCoils,         ONLY: DXCoilPartLoadRatio, DXCoilFanOpMode

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: DesicDehumNum  ! number of the current dehumidifier being simulated
  REAL(r64),    INTENT (IN) :: HumRatNeeded ! process air leaving humidity ratio set by controller [kg water/kg air]
  LOGICAL, INTENT (IN) :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: MinVolFlowPerRatedTotQ = 0.00002684d0 ! m3/s per W = 200 cfm/ton,
                                                                ! min vol flow per rated evaporator capacity

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: DDPartLoadRatio        ! fraction of dehumidification capacity required to meet setpoint
  REAL(r64) :: QRegen = 0.0d0           ! required coil load passed to sim heating coil routine (W)
  REAL(r64) :: MassFlowRateNew        ! new required mass flow rate calculated to keep regen setpoint temperature (kg/s)
  REAL(r64) :: CondenserWasteHeat     ! Condenser waste heat (W)
  REAL(r64) :: CpAir                  ! Specific heat of air (J/kg-K)
  REAL(r64) :: NewRegenInTemp         ! new temp calculated from condenser waste heat (C)
  REAL(r64) :: ExhaustFanMassFlowRate ! exhaust fan mass flow rate (kg/s)
  REAL(r64) :: ExhaustFanPLR          ! exhaust fan run time fraction calculated from new mass flow rate for regen side
  REAL(r64) :: ExhaustFanPowerMod     ! used to calculate exhaust fan power from flow fraction
  REAL(r64) :: VolFlowPerRatedTotQ    ! flow rate per rated total cooling capacity of the companion coil (m3/s/W)
  REAL(r64) :: FanDeltaT                           ! used to account for fan heat when calculating regeneration heater energy (C)
  REAL(r64) :: OnOffFanPLF                         ! save air loop fan part load fracton while calculating exhaust fan power
  REAL(r64) :: RegenSetPointTemp                   ! regeneration temperature setpoint (C)
  INTEGER :: RegenCoilIndex                   ! index to regeneration heating coil, 0 when not used
  INTEGER :: CompanionCoilIndexNum            ! index for companion DX cooling coil, 0 when DX coil is not used
  CHARACTER(len=MaxNameLength) :: MinVol      ! character string used for error messages
  CHARACTER(len=MaxNameLength) :: VolFlowChar ! character string used for error messages
  LOGICAL, SAVE :: MyOneTimeFlag = .TRUE.     ! one time flag
  REAL(r64), SAVE    :: RhoAirStdInit              ! standard air density (kg/m3)
  LOGICAL       :: UnitOn                     ! unit on flag
!  LOGICAL       :: SimFlag                    ! used to turn off additional simulation if DX Coil is off
  REAL(r64)     :: QRegen_OASysFanAdjust      ! temporary variable used to adjust regen heater load during iteration

UnitOn = .FALSE.
DDPartLoadRatio = 0.0d0
RegenCoilIndex = DesicDehum(DesicDehumNum)%RegenCoilIndex
FanDeltaT = 0.0d0
RegenSetPointTemp = DesicDehum(DesicDehumNum)%RegenSetPointTemp
ExhaustFanMassFlowRate = 0.0d0

! Save OnOffFanPartLoadFraction while performing exhaust fan calculations
OnOffFanPLF = OnOffFanPartLoadFraction
OnOffFanPartLoadFraction = 1.0d0

IF(DesicDehum(DesicDehumNum)%CoilUpstreamOfProcessSide == Yes)THEN
! Cooling coil directly upstream of desiccant dehumidifier, dehumidifier runs in tandem with DX coil
  CompanionCoilIndexNum = DesicDehum(DesicDehumNum)%DXCoilIndex
ELSE
! desiccant dehumidifier determines its own PLR
  CompanionCoilIndexNum = 0
END IF

IF(MyOneTimeFlag)THEN
  RhoAirStdInit = StdRhoAir
  MyOneTimeFlag = .FALSE.
END IF

IF (HumRatNeeded .LT. Node(DesicDehum(DesicDehumNum)%ProcAirInNode)%HumRat)THEN
  UnitOn = .TRUE.
ENDIF

IF(DesicDehum(DesicDehumNum)%CoilUpstreamOfProcessSide == Yes) THEN
  IF (DXCoilPartLoadRatio(DesicDehum(DesicDehumNum)%DXCoilIndex) .EQ. 0.0d0)THEN
    UnitOn = .FALSE.
  ENDIF
ENDIF

IF (UnitOn) THEN

  IF (DesicDehum(DesicDehumNum)%RegenInletIsOutsideAirNode) THEN
    IF (DesicDehum(DesicDehumNum)%HXTypeNum == BalancedHX) THEN
      Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate = &
                       Node(DesicDehum(DesicDehumNum)%ProcAirInNode)%MassFlowRate
      Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRateMaxAvail = &
                       Node(DesicDehum(DesicDehumNum)%ProcAirInNode)%MassFlowRate
    END IF
  END IF

! Get conditions from DX Coil condenser if present (DXCoilIndex verified > 0 in GetInput)
  IF(DesicDehum(DesicDehumNum)%Preheat == Yes)THEN

!     condenser waste heat is proportional to DX coil PLR
      CondenserWasteHeat = HeatReclaimDXCoil(DesicDehum(DesicDehumNum)%DXCoilIndex)%AvailCapacity
      HeatReclaimDXCoil(DesicDehum(DesicDehumNum)%DXCoilIndex)%AvailCapacity = 0.0d0

      CpAir = PsyCpAirFnWTdb(Node(DesicDehum(DesicDehumNum)%CondenserInletNode)%HumRat, &
                             Node(DesicDehum(DesicDehumNum)%CondenserInletNode)%Temp)

      IF (DesicDehum(DesicDehumNum)%RegenFanPlacement == BlowThru)THEN
        CALL SimulateFanComponents(DesicDehum(DesicDehumNum)%RegenFanName,FirstHVACIteration, &
                                   DesicDehum(DesicDehumNum)%RegenFanIndex)
        FanDeltaT = Node(DesicDehum(DesicDehumNum)%RegenFanOutNode)%Temp - &
                    Node(DesicDehum(DesicDehumNum)%RegenFanInNode)%Temp
!       Adjust setpoint to account for fan heat
        RegenSetPointTemp = RegenSetPointTemp - FanDeltaT
      ENDIF

!     CompanionCoilIndexNum .GT. 0 means the same thing as DesicDehum(DesicDehumNum)%CoilUpstreamOfProcessSide == Yes
      IF(CompanionCoilIndexNum .GT. 0)THEN

        DDPartLoadRatio = DXCoilPartLoadRatio(DesicDehum(DesicDehumNum)%DXCoilIndex)

      END IF

!     calculate actual condenser outlet node (regen inlet node) temperature
      IF(CompanionCoilIndexNum .GT. 0) THEN
        IF(DXCoilFanOpMode(DesicDehum(DesicDehumNum)%DXCoilIndex) == ContFanCycCoil) THEN
          NewRegenInTemp = Node(DesicDehum(DesicDehumNum)%CondenserInletNode)%Temp + &
                    CondenserWasteHeat/(CpAir*(Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate)* DDPartLoadRatio)
          CondenserWasteHeat = CondenserWasteHeat / DDPartLoadRatio
        ELSE
          NewRegenInTemp = Node(DesicDehum(DesicDehumNum)%CondenserInletNode)%Temp + &
                  CondenserWasteHeat/(CpAir*(Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate))
        END IF
      ELSE
        NewRegenInTemp = Node(DesicDehum(DesicDehumNum)%CondenserInletNode)%Temp + &
                CondenserWasteHeat/(CpAir*(Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate))
      END IF

      Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%Temp     = NewRegenInTemp
      Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%Enthalpy = PsyHFnTdbW(Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%Temp, &
                                                                           Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%HumRat)
      MassFlowRateNew = 0.0d0

      IF (DesicDehum(DesicDehumNum)%ExhaustFanMaxVolFlowRate .GT. 0)THEN

!       calculate mass flow rate required to maintain regen inlet setpoint temp
        IF(NewRegenInTemp .GT. RegenSetPointTemp)THEN
          IF(RegenSetPointTemp - Node(DesicDehum(DesicDehumNum)%CondenserInletNode)%Temp .NE. 0.0d0)THEN
            MassFlowRateNew =  MAX(0.0d0, CondenserWasteHeat / &
                (CpAir*(RegenSetPointTemp - Node(DesicDehum(DesicDehumNum)%CondenserInletNode)%Temp)))
          ELSE
            MassFlowRateNew =  Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate
          ENDIF
        ENDIF

!       calculate exhaust fan mass flow rate and new regen inlet temperature (may not be at setpoint)
        IF (MassFlowRateNew > Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate)THEN
          ExhaustFanMassFlowRate = MassFlowRateNew - Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate
          ExhaustFanMassFlowRate = MAX(0.0d0,MIN(ExhaustFanMassFlowRate,DesicDehum(DesicDehumNum)%ExhaustFanMaxMassFlowRate))

          Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%Temp = Node(DesicDehum(DesicDehumNum)%CondenserInletNode)%Temp &
           + CondenserWasteHeat/(CpAir*(Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate+ExhaustFanMassFlowRate))
          Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%HumRat = Node(DesicDehum(DesicDehumNum)%CondenserInletNode)%HumRat
          Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%Enthalpy = &
                 PsyHFnTdbW(Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%Temp, &
                 Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%HumRat)
        END IF

      ENDIF

      IF(RegenCoilIndex .GT. 0)THEN
        IF(NewRegenInTemp .LT. RegenSetPointTemp)THEN
          CpAir = PsyCpAirFnWTdb(Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%HumRat, &
                           Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%Temp)
        END IF
        QRegen = MAX(0.0d0, (CpAir * Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate * &
                          (RegenSetPointTemp-Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%Temp)))
        IF(QRegen .EQ. 0.0d0) QRegen = -1.0d0
      END IF

!     CompanionCoilIndexNum .EQ. 0 means the same thing as DesicDehum(DesicDehumNum)%CoilUpstreamOfProcessSide == No
      IF(CompanionCoilIndexNum .EQ. 0)THEN

        IF(RegenCoilIndex .GT. 0)THEN

          QRegen_OASysFanAdjust = QRegen
          IF (DesicDehum(DesicDehumNum)%RegenFanPlacement == BlowThru)THEN
            IF(Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate .GT. 0.0d0)THEN
!             For VAV systems, fan may restrict air flow during iteration. Adjust QRegen proportional to Mdot reduction through fan
              QRegen_OASysFanAdjust = QRegen_OASysFanAdjust * Node(DesicDehum(DesicDehumNum)%RegenFanOutNode)%MassFlowRate / &
                                                              Node(DesicDehum(DesicDehumNum)%RegenFanInNode)%MassFlowRate
            END IF
          END IF

          CALL CalcNonDXHeatingCoils(DesicDehumNum,FirstHVACIteration,QRegen_OASysFanAdjust)
        END IF

        CALL SimHeatRecovery(DesicDehum(DesicDehumNum)%HXName,FirstHVACIteration,DesicDehum(DesicDehumNum)%CompIndex, &
                 ContFanCycCoil, HXPartLoadRatio=1.0d0, HXUnitEnable=.TRUE., CompanionCoilIndex=CompanionCoilIndexNum, &
                 RegenInletIsOANode=DesicDehum(DesicDehumNum)%RegenInletIsOutsideAirNode)

!       calculate desiccant part-load ratio
        IF(Node(DesicDehum(DesicDehumNum)%ProcAirInNode)%HumRat .NE. Node(DesicDehum(DesicDehumNum)%ProcAirOutNode)%HumRat) THEN
          DDPartLoadRatio = (Node(DesicDehum(DesicDehumNum)%ProcAirInNode)%HumRat - HumRatNeeded) / &
                (Node(DesicDehum(DesicDehumNum)%ProcAirInNode)%HumRat - Node(DesicDehum(DesicDehumNum)%ProcAirOutNode)%HumRat)
          DDPartLoadRatio = MAX(0.0d0, MIN(1.0d0, DDPartLoadRatio))
        ELSE
          DDPartLoadRatio = 1.0d0
        END IF

      END IF

      IF(ExhaustFanMassFlowRate .GT. 0.0d0)THEN

!       calculate exhaust fan mass flow rate due to desiccant system operation
        ExhaustFanMassFlowRate = ExhaustFanMassFlowRate * DDPartLoadRatio

!       calculate exhaust fan PLR due to desiccant system operation
        ExhaustFanPLR = ExhaustFanMassFlowRate/DesicDehum(DesicDehumNum)%ExhaustFanMaxMassFlowRate

!       find exhaust fan power multiplier using exhaust fan part-load ratio
        IF(DesicDehum(DesicDehumNum)%ExhaustFanCurveIndex .GT. 0) THEN
           ExhaustFanPowerMod = MIN(1.0d0,MAX(0.0d0,CurveValue(DesicDehum(DesicDehumNum)%ExhaustFanCurveIndex,ExhaustFanPLR)))
        ELSE
           ExhaustFanPowerMod = 1.0d0
        END IF

!       calculate exhaust fan power due to desiccant operation
        DesicDehum(DesicDehumNum)%ExhaustFanPower = DesicDehum(DesicDehumNum)%ExhaustFanMaxPower * ExhaustFanPowerMod

      END IF

  ELSE ! ELSE for IF(DesicDehum(DesicDehumNum)%Preheat == Yes)THEN

      IF(Node(DesicDehum(DesicDehumNum)%ProcAirInNode)%HumRat .GT. HumRatNeeded)THEN

!       Get Full load output of desiccant wheel
        IF (DesicDehum(DesicDehumNum)%RegenFanPlacement == BlowThru)THEN
          CALL SimulateFanComponents(DesicDehum(DesicDehumNum)%RegenFanName,FirstHVACIteration, &
                                     DesicDehum(DesicDehumNum)%RegenFanIndex)
          FanDeltaT = Node(DesicDehum(DesicDehumNum)%RegenFanOutNode)%Temp - &
                      Node(DesicDehum(DesicDehumNum)%RegenFanInNode)%Temp
          RegenSetPointTemp = RegenSetPointTemp - FanDeltaT
        ENDIF

        IF(RegenCoilIndex .GT. 0)THEN
          CpAir = PsyCpAirFnWTdb(Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%HumRat, &
                                 Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%Temp)
          QRegen = MAX(0.0d0, (CpAir * Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate * &
                            (RegenSetPointTemp-Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%Temp)))

          QRegen_OASysFanAdjust = QRegen
          IF (DesicDehum(DesicDehumNum)%RegenFanPlacement == BlowThru)THEN
            IF(Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate .GT. 0.0d0)THEN
!             For VAV systems, fan may restrict air flow during iteration. Adjust QRegen proportional to Mdot reduction through fan
              QRegen_OASysFanAdjust = QRegen_OASysFanAdjust * Node(DesicDehum(DesicDehumNum)%RegenFanOutNode)%MassFlowRate / &
                                                              Node(DesicDehum(DesicDehumNum)%RegenFanInNode)%MassFlowRate
            END IF
          END IF

          IF(QRegen_OASysFanAdjust .EQ. 0.0d0) QRegen_OASysFanAdjust = -1.0d0
          CALL CalcNonDXHeatingCoils(DesicDehumNum,FirstHVACIteration,QRegen_OASysFanAdjust)
        END IF

!       CompanionCoilIndexNum .EQ. 0 means the same thing as DesicDehum(DesicDehumNum)%CoilUpstreamOfProcessSide == No
        IF(CompanionCoilIndexNum .EQ. 0)THEN
          CALL SimHeatRecovery(DesicDehum(DesicDehumNum)%HXName,FirstHVACIteration,DesicDehum(DesicDehumNum)%CompIndex, &
                 ContFanCycCoil, HXPartLoadRatio=1.0d0, HXUnitEnable=.TRUE., CompanionCoilIndex=CompanionCoilIndexNum, &
                 RegenInletIsOANode=DesicDehum(DesicDehumNum)%RegenInletIsOutsideAirNode)

!         calculate desiccant part-load ratio
          IF(Node(DesicDehum(DesicDehumNum)%ProcAirInNode)%HumRat .NE. Node(DesicDehum(DesicDehumNum)%ProcAirOutNode)%HumRat) THEN
            DDPartLoadRatio = (Node(DesicDehum(DesicDehumNum)%ProcAirInNode)%HumRat - HumRatNeeded) / &
                (Node(DesicDehum(DesicDehumNum)%ProcAirInNode)%HumRat - Node(DesicDehum(DesicDehumNum)%ProcAirOutNode)%HumRat)
            DDPartLoadRatio = MAX(0.0d0, MIN(1.0d0, DDPartLoadRatio))
          ELSE
            DDPartLoadRatio = 1.0d0
          END IF
        ELSE
          DDPartLoadRatio = DXCoilPartLoadRatio(DesicDehum(DesicDehumNum)%DXCoilIndex)
        END IF
      ELSE ! ELSE for IF(Node(DesicDehum(DesicDehumNum)%ProcAirInNode)%HumRat .GT. HumRatNeeded)THEN
        DDPartLoadRatio = 0.0d0
      END IF ! END IF for IF(Node(DesicDehum(DesicDehumNum)%ProcAirInNode)%HumRat .GT. HumRatNeeded)THEN

  ENDIF ! END IF for IF(DesicDehum(DesicDehumNum)%Preheat == Yes)THEN

  DesicDehum(DesicDehumNum)%PartLoad = DDPartLoadRatio
  QRegen_OASysFanAdjust = QRegen

! set average regeneration air mass flow rate based on desiccant cycling ratio (DDPartLoadRatio)
  IF (DesicDehum(DesicDehumNum)%RegenInletIsOutsideAirNode) THEN
    Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate = &
      Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate * DDPartLoadRatio

! **RR moved to here, only adjust regen heater load if mass flow rate is changed
!   adjust regen heating coil capacity based on desiccant cycling ratio (PLR)
    QRegen_OASysFanAdjust = QRegen_OASysFanAdjust * DDPartLoadRatio

  END IF

! Call regen fan, balanced desiccant HX and heating coil
  IF (DesicDehum(DesicDehumNum)%RegenFanPlacement == BlowThru)THEN
    CALL SimulateFanComponents(DesicDehum(DesicDehumNum)%RegenFanName,FirstHVACIteration, &
                                      DesicDehum(DesicDehumNum)%RegenFanIndex)
  ENDIF

  IF(RegenCoilIndex .GT. 0)THEN

!!   adjust regen heating coil capacity based on desiccant cycling ratio (PLR)
!    QRegen_OASysFanAdjust = QRegen * DDPartLoadRatio

    IF (DesicDehum(DesicDehumNum)%RegenFanPlacement == BlowThru)THEN
      IF(Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate .GT. 0.0d0)THEN
!       For VAV systems, fan may restrict air flow during iteration. Adjust QRegen proportional to Mdot reduction through fan
        QRegen_OASysFanAdjust = QRegen_OASysFanAdjust * Node(DesicDehum(DesicDehumNum)%RegenFanOutNode)%MassFlowRate / &
                                                        Node(DesicDehum(DesicDehumNum)%RegenFanInNode)%MassFlowRate
      END IF
    END IF

    IF(QRegen_OASysFanAdjust .EQ. 0.0d0) QRegen_OASysFanAdjust = -1.0d0
    CALL CalcNonDXHeatingCoils(DesicDehumNum,FirstHVACIteration,QRegen_OASysFanAdjust)
  END IF

  CALL SimHeatRecovery(DesicDehum(DesicDehumNum)%HXName,FirstHVACIteration,DesicDehum(DesicDehumNum)%CompIndex, &
           ContFanCycCoil, HXPartLoadRatio=DDPartLoadRatio, HXUnitEnable=.TRUE., CompanionCoilIndex=CompanionCoilIndexNum, &
           RegenInletIsOANode=DesicDehum(DesicDehumNum)%RegenInletIsOutsideAirNode)

  IF (DesicDehum(DesicDehumNum)%RegenFanPlacement == DrawThru)THEN
    CALL SimulateFanComponents(DesicDehum(DesicDehumNum)%RegenFanName,FirstHVACIteration, &
                                      DesicDehum(DesicDehumNum)%RegenFanIndex)
  ENDIF

! Calculate water removal
  DesicDehum(DesicDehumNum)%WaterRemoveRate = Node(DesicDehum(DesicDehumNum)%ProcAirInNode)%MassFlowRate* &
                     (Node(DesicDehum(DesicDehumNum)%ProcAirInNode)%HumRat - &
                       Node(DesicDehum(DesicDehumNum)%ProcAirOutNode)%HumRat)

! If preheat is Yes, exhaust fan is condenser fan, if CoilUpstreamOfProcessSide is No, DD runs an its own PLR
  IF(DesicDehum(DesicDehumNum)%Preheat == Yes .AND. DesicDehum(DesicDehumNum)%CoilUpstreamOfProcessSide == No)THEN
!    should actually use DX coil RTF instead of PLR since fan power is being calculated
     DesicDehum(DesicDehumNum)%ExhaustFanPower = DesicDehum(DesicDehumNum)%ExhaustFanPower + &
                                  MAX(0.0d0,(DesicDehum(DesicDehumNum)%ExhaustFanMaxPower * &
                                  (DXCoilPartLoadRatio(DesicDehum(DesicDehumNum)%DXCoilIndex)-DDPartLoadRatio)))
  END IF

ELSE ! unit must be off

  DesicDehum(DesicDehumNum)%PartLoad = 0.0d0

  IF(DesicDehum(DesicDehumNum)%RegenInletIsOutsideAirNode) THEN
    Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate = 0.0d0
    Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRateMaxAvail = 0.0d0
  END IF

  IF (DesicDehum(DesicDehumNum)%RegenFanPlacement == BlowThru)THEN
    CALL SimulateFanComponents(DesicDehum(DesicDehumNum)%RegenFanName,FirstHVACIteration, &
                                        DesicDehum(DesicDehumNum)%RegenFanIndex)
  ENDIF

  IF(RegenCoilIndex .GT. 0)THEN
    CALL CalcNonDXHeatingCoils(DesicDehumNum,FirstHVACIteration,-1.0d0)
  END IF

  CALL SimHeatRecovery(DesicDehum(DesicDehumNum)%HXName,FirstHVACIteration,DesicDehum(DesicDehumNum)%CompIndex, &
                       ContFanCycCoil,HXPartLoadRatio=0.0d0,HXUnitEnable=.FALSE.,CompanionCoilIndex=CompanionCoilIndexNum, &
                       RegenInletIsOANode=DesicDehum(DesicDehumNum)%RegenInletIsOutsideAirNode)

  IF (DesicDehum(DesicDehumNum)%RegenFanPlacement == DrawThru)THEN
    CALL SimulateFanComponents(DesicDehum(DesicDehumNum)%RegenFanName,FirstHVACIteration, &
                                        DesicDehum(DesicDehumNum)%RegenFanIndex)
  ENDIF

! Turn on exhaust fan if DX Coil is operating
  IF(DesicDehum(DesicDehumNum)%ExhaustFanMaxVolFlowRate .GT. 0)THEN
    IF(DesicDehum(DesicDehumNum)%DXCoilIndex .GT. 0)THEN
      DDPartLoadRatio = DXCoilPartLoadRatio(DesicDehum(DesicDehumNum)%DXCoilIndex)
      DesicDehum(DesicDehumNum)%ExhaustFanPower = DesicDehum(DesicDehumNum)%ExhaustFanMaxPower * DDPartLoadRatio
      ExhaustFanMassFlowRate = DesicDehum(DesicDehumNum)%ExhaustFanMaxMassFlowRate * DDPartLoadRatio
    END IF
  END IF

ENDIF  ! UnitOn/Off

! check condenser minimum flow per rated total capacity
IF(DDPartLoadRatio .GT. 0.0d0 .AND. DesicDehum(DesicDehumNum)%ExhaustFanMaxVolFlowRate .GT. 0.0d0) THEN
  VolFlowperRatedTotQ = (Node(DesicDehum(DesicDehumNum)%RegenAirInNode)%MassFlowRate+ExhaustFanMassFlowRate)/ &
                        MAX(0.00001d0,(DesicDehum(DesicDehumNum)%CompanionCoilCapacity*DDPartLoadRatio*RhoAirStdInit))
  IF(.NOT. WarmupFlag .AND. (VolFlowperRatedTotQ .LT. MinVolFlowPerRatedTotQ)) THEN
    WRITE(VolFlowChar,*) VolFlowperRatedTotQ
    DesicDehum(DesicDehumNum)%ErrCount=DesicDehum(DesicDehumNum)%ErrCount+1
    IF (DesicDehum(DesicDehumNum)%ErrCount < 2) THEN
      CALL ShowWarningError(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "'//TRIM(DesicDehum(DesicDehumNum)%Name)//&
      '" - Air volume flow rate per watt of total condenser waste heat is below the minimum recommended at ' &
             //TRIM(VolFlowChar)//' m3/s/W.')
      CALL ShowContinueErrorTimeStamp(' ')
      WRITE(MinVol,*) MinVolFlowPerRatedTotQ
      CALL ShowContinueError('Expected minimum for VolumeFlowperRatedTotalCondenserWasteHeat = ['//TRIM(MinVol)//']')
      CALL ShowContinueError('Possible causes include inconsistent air flow rates in system components ')
      CALL ShowContinueError('on the regeneration side of the desiccant dehumidifier.')
    ELSE
      CALL ShowRecurringWarningErrorAtEnd(TRIM(DesicDehum(DesicDehumNum)%DehumType)//' "' &
             //TRIM(DesicDehum(DesicDehumNum)%Name)// &
        '" - Air volume flow rate per watt of rated total cooling capacity is out ' //&
        'of range error continues...',DesicDehum(DesicDehumNum)%ErrIndex1,VolFlowperRatedTotQ,VolFlowperRatedTotQ)
    END IF
  END IF ! flow per rated total capacity check ends
END IF

! Reset OnOffFanPartLoadFraction for process side fan calculations
OnOffFanPartLoadFraction = OnOffFanPLF


RETURN

END SUBROUTINE CalcGenericDesiccantDehumidifier

SUBROUTINE UpdateDesiccantDehumidifier(DesicDehumNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
          !                      for Gas Research Institute
          !       DATE WRITTEN   March 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Moves dehumidifier output to the outlet nodes.

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: DesicDehumNum ! number of the current dehumidifier being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: ProcInNode   ! process air inlet node number
  INTEGER             :: ProcOutNode  ! process air outlet node number

Select Case(DesicDehum(DesicDehumNum)%DehumTypeCode)

  Case (Solid)
   ProcInNode =DesicDehum(DesicDehumNum)%ProcAirInNode
   ProcOutNode = DesicDehum(DesicDehumNum)%ProcAirOutNode
   ! Set the process outlet air node of the dehumidifier
   Node(ProcOutNode)%Temp         = DesicDehum(DesicDehumNum)%ProcAirOutTemp
   Node(ProcOutNode)%HumRat       = DesicDehum(DesicDehumNum)%ProcAirOutHumRat
   Node(ProcOutNode)%Enthalpy     = DesicDehum(DesicDehumNum)%ProcAirOutEnthalpy

   ! Set the process outlet nodes for properties that just pass through & not used
   Node(ProcOutNode)%Quality             = Node(ProcInNode)%Quality
   Node(ProcOutNode)%Press               = Node(ProcInNode)%Press
   Node(ProcOutNode)%MassFlowRate        = Node(ProcInNode)%MassFlowRate
   Node(ProcOutNode)%MassFlowRateMin     = Node(ProcInNode)%MassFlowRateMin
   Node(ProcOutNode)%MassFlowRateMax     = Node(ProcInNode)%MassFlowRateMax
   Node(ProcOutNode)%MassFlowRateMinAvail= Node(ProcInNode)%MassFlowRateMinAvail
   Node(ProcOutNode)%MassFlowRateMaxAvail= Node(ProcInNode)%MassFlowRateMaxAvail

!   RegenInNode =DesicDehum(DesicDehumNum)%RegenAirInNode
!   RegenOutNode = DesicDehum(DesicDehumNum)%RegenAirOutNode
   ! Set the regen outlet air node of the dehumidifier
!   Node(RegenOutNode)%Temp         = DesicDehum(DesicDehumNum)%RegenAirOutTemp
!   Node(RegenOutNode)%HumRat       = DesicDehum(DesicDehumNum)%RegenAirOutHumRat
!   Node(RegenOutNode)%Enthalpy     = DesicDehum(DesicDehumNum)%RegenAirOutEnthalpy

   ! Set the regen outlet nodes for properties that just pass through & not used
!   Node(RegenOutNode)%Quality             = Node(RegenInNode)%Quality
!   Node(RegenOutNode)%Press               = Node(RegenInNode)%Press
!   Node(RegenOutNode)%MassFlowRate        = Node(RegenInNode)%MassFlowRate
!   Node(RegenOutNode)%MassFlowRateMin     = Node(RegenInNode)%MassFlowRateMin
!   Node(RegenOutNode)%MassFlowRateMax     = Node(RegenInNode)%MassFlowRateMax
!   Node(RegenOutNode)%MassFlowRateMinAvail= Node(RegenInNode)%MassFlowRateMinAvail
!   Node(RegenOutNode)%MassFlowRateMaxAvail= Node(RegenInNode)%MassFlowRateMaxAvail

   Case (Generic)

    RETURN

  END SELECT

RETURN

END SUBROUTINE UpdateDesiccantDehumidifier

SUBROUTINE ReportDesiccantDehumidifier(DesicDehumNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael J. Witte, GARD Analytics, Inc.
          !                      for Gas Research Institute
          !       DATE WRITTEN   March 2001
          !       MODIFIED       June 2007, R. Raustad, Added new dehumidifier type -- DESICCANT DEHUMIDIFIER
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Fill remaining report variables

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use DataHVACGlobals, ONLY: TimeStepSys

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: DesicDehumNum ! number of the current dehumidifier being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    REAL(r64) :: ReportingConstant

ReportingConstant=TimeStepSys*SecInHour

SELECT CASE (DesicDehum(DesicDehumNum)%DehumTypeCode)

 Case(Solid)
   DesicDehum(DesicDehumNum)%WaterRemove   = DesicDehum(DesicDehumNum)%WaterRemoveRate*ReportingConstant
   DesicDehum(DesicDehumNum)%RegenEnergy   = DesicDehum(DesicDehumNum)%QRegen*ReportingConstant
   DesicDehum(DesicDehumNum)%ElecUseEnergy = DesicDehum(DesicDehumNum)%ElecUseRate*ReportingConstant
 Case(Generic)
   DesicDehum(DesicDehumNum)%WaterRemove     = DesicDehum(DesicDehumNum)%WaterRemoveRate*ReportingConstant
   DesicDehum(DesicDehumNum)%ExhaustFanElecConsumption = DesicDehum(DesicDehumNum)%ExhaustFanPower*ReportingConstant

END SELECT

RETURN
END SUBROUTINE ReportDesiccantDehumidifier

SUBROUTINE CalcNonDXHeatingCoils(DesicDehumNum,FirstHVACIteration,RegenCoilLoad,RegenCoilLoadmet)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket Nigusse, FSEC/UCF
          !       DATE WRITTEN   January 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine simulates the four non dx heating coil types: Gas, Electric, hot water and steam.

          ! METHODOLOGY EMPLOYED:
          ! Simply calls the different heating coil component.  The hot water flow rate matching the coil load
          ! is calculated iteratively.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  USE HeatingCoils,              ONLY: SimulateHeatingCoilComponents
  USE WaterCoils,                ONLY: SimulateWaterCoilComponents
  USE SteamCoils,                ONLY: SimulateSteamCoilComponents
  USE PlantUtilities,            ONLY: SetComponentFlowRate
  USE General,                   ONLY: SolveRegulaFalsi,RoundSigDigits
  USE DataHVACGlobals,           ONLY: SmallLoad

  IMPLICIT NONE     ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,      INTENT(IN)             :: DesicDehumNum        ! Desiccant dehumidifier unit index
  LOGICAL,      INTENT(IN)             :: FirstHVACIteration   ! flag for first HVAC iteration in the time step
  REAL(r64),    INTENT(IN)             :: RegenCoilLoad        ! heating coil load to be met (Watts)
  REAL(r64),    INTENT(OUT), OPTIONAL  :: RegenCoilLoadmet     ! heating load met

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: ErrTolerance = 0.001d0    ! convergence limit for hotwater coil
  INTEGER, PARAMETER :: SolveMaxIter=50             ! Max iteration for SolveRegulaFalsi

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)      :: RegenCoilActual   ! actual heating load met
  REAL(r64)      :: mdot              ! heating coil steam or hot water mass flow rate
  REAL(r64)      :: MinWaterFlow      ! minimum hot water mass flow rate
!unused  REAL(r64)      :: PartLoadFraction  ! heating or cooling part load fraction
  REAL(r64)      :: MaxHotWaterFlow   ! maximum hot water mass flow rate, kg/s
  REAL(r64)      :: HotWaterMdot      ! actual hot water mass flow rate
  REAL(r64), DIMENSION(3) :: Par
  INTEGER        :: SolFlag

  RegenCoilActual=0.0d0
  IF (RegenCoilLoad > SmallLoad) THEN
     Select Case (DesicDehum(DesicDehumNum)%RegenCoilType_Num)
        Case (Coil_HeatingGas, Coil_HeatingElectric)
          CALL SimulateHeatingCoilComponents(DesicDehum(DesicDehumNum)%RegenCoilName,FirstHVACIteration, &
                                             RegenCoilLoad, DesicDehum(DesicDehumNum)%RegenCoilIndex, &
                                             RegenCoilActual)
        Case (Coil_HeatingWater)
          MaxHotWaterFlow = DesicDehum(DesicDehumNum)%MaxCoilFluidFlow
          Call SetComponentFlowRate(MaxHotWaterFlow, &
                                    DesicDehum(DesicDehumNum)%CoilControlNode, &
                                    DesicDehum(DesicDehumNum)%CoilOutletNode, &
                                    DesicDehum(DesicDehumNum)%LoopNum, &
                                    DesicDehum(DesicDehumNum)%LoopSide, &
                                    DesicDehum(DesicDehumNum)%BranchNum, &
                                    DesicDehum(DesicDehumNum)%CompNum)
          RegenCoilActual = RegenCoilLoad
          ! simulate the regenerator hot water heating coil
          CALL SimulateWaterCoilComponents(DesicDehum(DesicDehumNum)%RegenCoilName,FirstHVACIteration, &
                                            DesicDehum(DesicDehumNum)%RegenCoilIndex, RegenCoilActual)

          IF ( RegenCoilActual > (RegenCoilLoad + SmallLoad) ) THEN
              ! control water flow to obtain output matching RegenCoilLoad
              SolFlag = 0
              MinWaterFlow = 0.0d0
              Par(1) = REAL(DesicDehumNum,r64)
              IF (FirstHVACIteration) THEN
                Par(2) = 1.0d0
              ELSE
                Par(2) = 0.0d0
              END IF
              Par(3) = RegenCoilLoad
              CALL SolveRegulaFalsi(ErrTolerance, SolveMaxIter, SolFlag, HotWaterMdot, HotWaterCoilResidual, &
                                    MinWaterFlow, MaxHotWaterFlow, Par)
              IF (SolFlag == -1) THEN
               IF (DesicDehum(DesicDehumNum)%HotWaterCoilMaxIterIndex == 0) THEN
                 CALL ShowWarningMessage('CalcNonDXHeatingCoils: Hot water coil control failed for '//  &
                    trim(DesicDehum(DesicDehumNum)%DehumType)//'="'// &
                    TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
                 CALL ShowContinueErrorTimeStamp(' ')
                 CALL ShowContinueError('...Iteration limit ['//trim(RoundSigDigits(SolveMaxIter))//  &
                    '] exceeded in calculating hot water mass flow rate')
               ENDIF
              CALL ShowRecurringWarningErrorAtEnd('CalcNonDXHeatingCoils: Hot water coil control failed (iteration limit ['//  &
                 trim(RoundSigDigits(SolveMaxIter))//']) for '//trim(DesicDehum(DesicDehumNum)%DehumType)//'="' &
                 //TRIM(DesicDehum(DesicDehumNum)%Name)//'"',DesicDehum(DesicDehumNum)%HotWaterCoilMaxIterIndex)
              ELSE IF (SolFlag == -2) THEN
               IF (DesicDehum(DesicDehumNum)%HotWaterCoilMaxIterIndex2 == 0) THEN
                 CALL ShowWarningMessage('CalcNonDXHeatingCoils: Hot water coil control failed (maximum flow limits) for '//  &
                    trim(DesicDehum(DesicDehumNum)%DehumType)//'="'// &
                    TRIM(DesicDehum(DesicDehumNum)%Name)//'"')
                 CALL ShowContinueErrorTimeStamp(' ')
                 CALL ShowContinueError('...Bad hot water maximum flow rate limits')
                 CALL ShowContinueError('...Given minimum water flow rate='//trim(RoundSigDigits(MinWaterFlow,3))//' kg/s')
                 CALL ShowContinueError('...Given maximum water flow rate='//trim(RoundSigDigits(MaxHotWaterFlow,3))//' kg/s')
               ENDIF
               CALL ShowRecurringWarningErrorAtEnd('CalcNonDXHeatingCoils: Hot water coil control failed (flow limits) for '//  &
                  trim(DesicDehum(DesicDehumNum)%DehumType)//'="'// &
                  TRIM(DesicDehum(DesicDehumNum)%Name)//'"', &
                  DesicDehum(DesicDehumNum)%HotWaterCoilMaxIterIndex2,  &
                  ReportMinOf=MinWaterFlow,ReportMaxOf=MaxHotWaterFlow,ReportMinUnits='[kg/s]',ReportMaxUnits='[kg/s]')
              END IF

              RegenCoilActual = RegenCoilLoad
              ! simulate the regenerator hot water heating coil
              CALL SimulateWaterCoilComponents(DesicDehum(DesicDehumNum)%RegenCoilName,FirstHVACIteration, &
                                               DesicDehum(DesicDehumNum)%RegenCoilIndex, RegenCoilActual)
          ENDIF
        Case (Coil_HeatingSteam)
          mdot = DesicDehum(DesicDehumNum)%MaxCoilFluidFlow
          Call SetComponentFlowRate(mdot, &
                                    DesicDehum(DesicDehumNum)%CoilControlNode, &
                                    DesicDehum(DesicDehumNum)%CoilOutletNode, &
                                    DesicDehum(DesicDehumNum)%LoopNum, &
                                    DesicDehum(DesicDehumNum)%LoopSide, &
                                    DesicDehum(DesicDehumNum)%BranchNum, &
                                    DesicDehum(DesicDehumNum)%CompNum)
          ! simulate the regenerator steam heating coil
          CALL SimulateSteamCoilComponents(DesicDehum(DesicDehumNum)%RegenCoilName, FirstHVACIteration, &
                                           RegenCoilLoad, DesicDehum(DesicDehumNum)%RegenCoilIndex, &
                                           RegenCoilActual)
     END Select
  ELSE
     Select Case (DesicDehum(DesicDehumNum)%RegenCoilType_Num)
        Case (Coil_HeatingGas, Coil_HeatingElectric)
          CALL SimulateHeatingCoilComponents(DesicDehum(DesicDehumNum)%RegenCoilName,FirstHVACIteration, &
                                             RegenCoilLoad, DesicDehum(DesicDehumNum)%RegenCoilIndex, &
                                             RegenCoilActual)
        Case (Coil_HeatingWater)
          mdot = 0.0d0
          Call SetComponentFlowRate(mdot, &
                                    DesicDehum(DesicDehumNum)%CoilControlNode, &
                                    DesicDehum(DesicDehumNum)%CoilOutletNode, &
                                    DesicDehum(DesicDehumNum)%LoopNum, &
                                    DesicDehum(DesicDehumNum)%LoopSide, &
                                    DesicDehum(DesicDehumNum)%BranchNum, &
                                    DesicDehum(DesicDehumNum)%CompNum)
          RegenCoilActual = RegenCoilLoad
          ! simulate the regenerator hot water heating coil
          CALL SimulateWaterCoilComponents(DesicDehum(DesicDehumNum)%RegenCoilName,FirstHVACIteration, &
                                           DesicDehum(DesicDehumNum)%RegenCoilIndex, RegenCoilActual)
        Case (Coil_HeatingSteam)
          mdot = 0.0d0
          Call SetComponentFlowRate(mdot, &
                                    DesicDehum(DesicDehumNum)%CoilControlNode, &
                                    DesicDehum(DesicDehumNum)%CoilOutletNode, &
                                    DesicDehum(DesicDehumNum)%LoopNum, &
                                    DesicDehum(DesicDehumNum)%LoopSide, &
                                    DesicDehum(DesicDehumNum)%BranchNum, &
                                    DesicDehum(DesicDehumNum)%CompNum)
          ! simulate the regenerator steam heating coil
          CALL SimulateSteamCoilComponents(DesicDehum(DesicDehumNum)%RegenCoilName, FirstHVACIteration, &
                                           RegenCoilLoad, DesicDehum(DesicDehumNum)%RegenCoilIndex, &
                                           RegenCoilActual)
     END Select
  ENDIF
  IF (PRESENT(RegenCoilLoadmet)) RegenCoilLoadmet = RegenCoilActual
 RETURN

END SUBROUTINE CalcNonDXHeatingCoils

FUNCTION HotWaterCoilResidual(HWFlow, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Bereket Nigusse, FSEC/UCF
          !       DATE WRITTEN   January 2012
          !       MODIFIED
          !       RE-ENGINEERED

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates residual function (RegenCoilActual - RegenCoilHeatLoad) / RegenCoilHeatLoad
          ! coil actual output depends on the hot water flow rate which is varied to minimize the residual
          !

          ! METHODOLOGY EMPLOYED:
          ! Calls HotWaterCoilResidual, and calculates the residual as defined above.
          !

          ! REFERENCES:

          ! USE STATEMENTS:
  USE WaterCoils,     ONLY: SimulateWaterCoilComponents
  USE PlantUtilities, ONLY: SetComponentFlowRate


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)                         :: HWFlow   ! hot water flow rate in kg/s
  REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par      ! Par(5) is the requested coil load
  REAL(r64)                                     :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER               :: DesicDehumNum
  LOGICAL               :: FirstHVACSoln
  REAL(r64)             :: RegenCoilActual             ! delivered coild load, W
  REAL(r64)             :: RegenCoilHeatLoad           ! requested coild load, W
  REAL(r64)             :: mdot

  DesicDehumNum = INT(Par(1))
  IF (Par(2) > 0.0d0) THEN
    FirstHVACSoln = .TRUE.
  ELSE
    FirstHVACSoln = .FALSE.
  END IF
  RegenCoilHeatLoad =  Par(3)
  RegenCoilActual = RegenCoilHeatLoad
  mdot = HWFlow
  Call SetComponentFlowRate(mdot, &
                            DesicDehum(DesicDehumNum)%CoilControlNode, &
                            DesicDehum(DesicDehumNum)%CoilOutletNode, &
                            DesicDehum(DesicDehumNum)%LoopNum, &
                            DesicDehum(DesicDehumNum)%LoopSide, &
                            DesicDehum(DesicDehumNum)%BranchNum, &
                            DesicDehum(DesicDehumNum)%CompNum)

    ! simulate the hot water regenerator heating coil
  CALL SimulateWaterCoilComponents(DesicDehum(DesicDehumNum)%RegenCoilName,FirstHVACSoln, &
                                   DesicDehum(DesicDehumNum)%RegenCoilIndex, RegenCoilActual)
  IF (RegenCoilHeatLoad /= 0.0d0) THEN
    Residuum = (RegenCoilActual - RegenCoilHeatLoad)/ RegenCoilHeatLoad
  ELSE !Objexx:Return ELSE added to assure return value is set
    Residuum = 0.0d0
  ENDIF
  RETURN
END FUNCTION HotWaterCoilResidual


!        End of Reporting subroutines for the SimAir Module
! *****************************************************************************


!                                 COPYRIGHT NOTICE
!
!     Portions Copyright © Gas Research Institute 2001.  All rights reserved.
!
!     GRI LEGAL NOTICE
!     Neither GRI, members of GRI nor any person or organization acting on behalf
!     of either:
!
!     A. Makes any warranty of representation, express or implied with respect to
!        the accuracy, completness, or usefulness of the information contained in
!        in this program, including any warranty of merchantability or fitness of
!        any purpose with respoect to the program, or that the use of any
!        information disclosed in this program may not infringe privately-owned
!        rights, or
!
!     B.  Assumes any liability with respoct to the use of, or for any and all
!         damages resulting from the use of the program or any portion thereof or
!         any information disclosed therein.
!
!
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


END MODULE DesiccantDehumidifiers

