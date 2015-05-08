MODULE PurchasedAirManager

  ! Module containing data and routines dealing with Ideal Loads Air System (formerly PURCHASED AIR).

  ! MODULE INFORMATION:
  !       AUTHOR         Russ Taylor
  !       DATE WRITTEN   May 1997
  !       MODIFIED       Fred Buhl Dec 1999
  !                      B. Griffith Dec 2006. added OA lookup function, moved getinputflag up to Module
  !                      M. Witte June 2011, add new features including DCV, economizer, dehumidification and humidification
  !                      NOTE: MJW Sep 13, 2011:  Still need to review checks for negative loads and impossible supply temps???
  !                           There are no Deallocate statements in here - should there be?
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required to simulate the
  ! Zone Ideal Loads Air System component. This component supplies hot or cold air
  ! at a fixed or variable temperature to a zone to meet the zone load.
  ! With the June 2011 enhancements it will also supply outdoor air with optional demand-controlled ventilation
  ! and economizer controls, plus new options for controlling zone humidity.

  ! METHODOLOGY EMPLOYED:
  ! The user can choose via input the max/min hot and cold supply air
  ! temperature and humidity ratio. The air mass flow rate is chosen
  ! to meet the (remaining) zone load or based on the outdoor air flow requirement.
  ! If the outdoor air flow sets the flow rate, the supply air temperature and
  ! humidity ratio are adjusted to meet the zone load.

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
    USE DataPrecisionGlobals
    USE DataGlobals
    USE DataHVACGlobals
    USE DataHeatBalFanSys, ONLY: ZoneAirHumRat, ZoneThermostatSetPointHi, ZoneThermostatSetPointLo
    Use DataEnvironment, ONLY: StdBaroPress, OutBaroPress, OutHumRat, OutEnthalpy, StdRhoAir
    USE DataInterfaces

  ! Use statements for access to subroutines in other modules
    USE ScheduleManager
    USE Psychrometrics,  ONLY: PsyRhoAirFnPbTdbW,PsyHFnTdbW,PsyTdbFnHW,PsyCpAirFnWTdb,PsyTsatFnHPb,PsyWFnTdbH,PsyWFnTdbRhPb

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS:
          ! MODULE PARAMETER DEFINITIONS:
  ! Heating and Cooling Limit type parameters
    INTEGER, PARAMETER :: NoLimit                     = 1
    INTEGER, PARAMETER :: LimitFlowRate               = 2
    INTEGER, PARAMETER :: LimitCapacity               = 3
    INTEGER, PARAMETER :: LimitFlowRateAndCapacity    = 4
    CHARACTER(len=*), DIMENSION(4), PARAMETER :: cLimitType=  &
       (/'NoLimit                 ',  &
         'LimitFlowRate           ',  &
         'LimitCapacity           ',  &
         'LimitFlowRateAndCapacity'/)
  ! Dehumidification and Humidification control type parameters
    INTEGER, PARAMETER :: None                        = 1
    INTEGER, PARAMETER :: ConstantSensibleHeatRatio   = 2
    INTEGER, PARAMETER :: Humidistat                  = 3
    INTEGER, PARAMETER :: ConstantSupplyHumidityRatio = 4
  ! Demand controlled ventilation type parameters
    INTEGER, PARAMETER :: NoDCV                       = 1
    INTEGER, PARAMETER :: OccupancySchedule           = 2
    INTEGER, PARAMETER :: CO2Setpoint                 = 3
  ! Outdoor air economizer type parameters
    INTEGER, PARAMETER :: NoEconomizer                = 1
    INTEGER, PARAMETER :: DifferentialDryBulb         = 2
    INTEGER, PARAMETER :: DifferentialEnthalpy        = 3
  ! Heat recovery type parameters
    INTEGER, PARAMETER :: NoHeatRecovery              = 1
    INTEGER, PARAMETER :: Sensible                    = 2
    INTEGER, PARAMETER :: Enthalpy                    = 3
  ! Operating mode parameters
    INTEGER, PARAMETER :: Off                       = 0
    INTEGER, PARAMETER :: Heat                      = 1
    INTEGER, PARAMETER :: Cool                      = 2
    INTEGER, PARAMETER :: Deadband                  = 3
  ! Delta humidity ratio limit, 0.00025 equals delta between 45F dewpoint and 46F dewpoint
  ! used to prevent dividing by near zero
    REAL(r64), PARAMETER :: SmallDeltaHumRat = 0.00025d0

  ! DERIVED TYPE DEFINITIONS:
    TYPE ZonePurchasedAir
      CHARACTER(len=MaxNameLength) :: cObjectName  =' ' ! Name of the object from IDD
      CHARACTER(len=MaxNameLength) :: Name = ' '        ! Name or identifier of this piece of equipment
      CHARACTER(len=MaxNameLength) :: AvailSched   = ' ' ! System availablity schedule
      INTEGER   :: AvailSchedPtr            = 0          ! Index to system availability schedule
      INTEGER   :: ZoneSupplyAirNodeNum    = 0          ! Node number of zone supply air node for purchased air
      INTEGER   :: ZoneExhaustAirNodeNum   = 0          ! Node number of zone exhaust air node for purchased air
      INTEGER   :: ZoneRecircAirNodeNum    = 0          ! Node number of recirculation air node for purchased air
                                                        !   same as exhaust node if specified, otherwise zone return node
      REAL(r64) :: MaxHeatSuppAirTemp      = 0.0d0      ! Maximum supply air temperature for heating [C]
      REAL(r64) :: MinCoolSuppAirTemp      = 0.0d0      ! Minimum supply air temperature for cooling [C]
      REAL(r64) :: MaxHeatSuppAirHumRat    = 0.0d0      ! Maximum supply heating air humidity ratio [kg water/kg dry air]
      REAL(r64) :: MinCoolSuppAirHumRat    = 0.0d0      ! Minimum supply cooling air humidity ratio [kg water/kg dry air]
      INTEGER   :: HeatingLimit            = 0          ! Heating capacity limit type - NoLimit, LimitFlowRate, LimitCapacity,
                                                        !       or LimitFlowRateAndCapacity
      REAL(r64) :: MaxHeatVolFlowRate      = 0.0d0      ! Maximum heating supply air flow[m3/s]
      REAL(r64) :: MaxHeatSensCap          = 0.0d0      ! Maximum heating sensible capacity [W]
      INTEGER   :: CoolingLimit            = 0          ! Cooling capacity limit type - NoLimit, LimitFlowRate, LimitCapacity,
                                                        !       or LimitFlowRateAndCapacity
      REAL(r64) :: MaxCoolVolFlowRate      = 0.0d0      ! Maximum cooling supply air flow [m3/s]
      REAL(r64) :: MaxCoolTotCap           = 0.0d0      ! Maximum cooling total capacity [W]
      CHARACTER(len=MaxNameLength) :: HeatSched   = ' ' ! Heating availablity schedule
      INTEGER   :: HeatSchedPtr            = 0          ! Index to heating availability schedule
      CHARACTER(len=MaxNameLength) :: CoolSched   = ' ' ! Cooling availability schedule
      INTEGER   :: CoolSchedPtr            = 0          ! Index to the cooling availability schedule
      INTEGER   :: DehumidCtrlType         = 0          ! Dehumidification control type - ConstantSensibleHeatRatio,
                                                        !      Humidistat, or ConstantSupplyHumidityRatio
      REAL(r64) :: CoolSHR                 = 0.0d0      ! Cooling sensible heat ratio
      INTEGER   :: HumidCtrlType           = 0          ! Humidification control type - None,
                                                        !      Humidistat, or ConstantSupplyHumidityRatio
      INTEGER   :: OARequirementsPtr       = 0          ! Index to DesignSpecification:OutdoorAir object
      INTEGER   :: DCVType                 = 0          ! Demand controlled ventilation type - None,
                                                        !      OccupancySchedule, or CO2Setpoint
      INTEGER   :: EconomizerType          = 0          ! Outdoor air economizer type - NoEconomizer,
                                                        !      DifferentialDryBulb, or DifferentialEnthalpy
      LOGICAL   :: OutdoorAir              = .false.    ! Is there outdoor air?
      INTEGER   :: OutdoorAirNodeNum       = 0          ! Node number of the outdoor air inlet node
      INTEGER   :: HtRecType               = 0          ! Outdoor air heat recovery type - None, Sensible, Enthalpy
      REAL(r64) :: HtRecSenEff             = 0.0d0      ! Sensible heat recovery effectiveness
      REAL(r64) :: HtRecLatEff             = 0.0d0      ! Latent heat recovery effectiveness

      INTEGER   :: OAFlowFracSchPtr        = 0          ! Fraction schedule applied to total OA requirement

      REAL(r64) :: MaxHeatMassFlowRate     = 0.0d0      ! The maximum heating air mass flow rate [kg/s]
      REAL(r64) :: MaxCoolMassFlowRate     = 0.0d0      ! The maximum cooling air mass flow rate [kg/s]
      LOGICAL   :: EMSOverrideMdotOn       = .FALSE.    ! if true, then EMS is calling to override supply mass flow rate
      REAL(r64) :: EMSValueMassFlowRate    = 0.0d0      ! Value EMS is directing to use for supply mass flow rate [kg/s]
      LOGICAL   :: EMSOverrideOAMdotOn     = .FALSE.    ! if true, then EMS is calling to override OA mass flow rate
      REAL(r64) :: EMSValueOAMassFlowRate  = 0.0d0      ! Value EMS is directing to use for OA mass flow rate [kg/s]
      LOGICAL   :: EMSOverrideSupplyTempOn = .FALSE.    ! if true, then EMS is calling to override supply temperature
      REAL(r64) :: EMSValueSupplyTemp      = 0.0d0      ! Value EMS is directing to use for supply temperature [C]
      LOGICAL   :: EMSOverrideSupplyHumRatOn = .FALSE.  ! if true, then EMS is calling to override supply humidity ratio
      REAL(r64) :: EMSValueSupplyHumRat    = 0.0d0      ! Value EMS is directing to use for supply humidity ratio [kg-H2O/kg-dryair]
      REAL(r64) :: MinOAMassFlowRate       = 0.0d0      ! The minimum required outdoor air mass flow rate [kg/s]
      REAL(r64) :: OutdoorAirMassFlowRate  = 0.0d0      ! The outdoor air mass flow rate [kg/s]
      ! Intermediate results
      REAL(r64) :: FinalMixedAirTemp        = 0.0d0      ! Dry-bulb temperature of the mixed air, saved for system ventilation load reporting [C]
      REAL(r64) :: FinalMixedAirHumRat      = 0.0d0      ! Humidity ratio of the mixed air, saved for system ventilation load reporting [kg-H2O/kg-dryair]
      REAL(r64) :: HtRecSenOutput           = 0.0d0      ! Sensible heating/cooling rate from heat recovery (<0 means cooling) [W]
      REAL(r64) :: HtRecLatOutput           = 0.0d0      ! Latent heating/cooling rate from heat recovery (<0 means cooling or dehumidfying) [W]
      REAL(r64) :: OASenOutput              = 0.0d0      ! Outdoor air sensible output relative to zone conditions [W], <0 means OA is cooler than zone air
      REAL(r64) :: OALatOutput              = 0.0d0      ! Outdoor air latent output relative to zone conditions [W], <0 means OA is drier than zone air
      REAL(r64) :: SenOutputToZone          = 0.0d0      ! Ideal Loads System sensible output to zone [W], <0 means supply is cooler than zone air
      REAL(r64) :: LatOutputToZone          = 0.0d0      ! Ideal Loads System latent heat output to zone [W], <0 means supply is drier than zone air
      REAL(r64) :: SenCoilLoad              = 0.0d0      ! Ideal Loads System sensible load on "coils" (<0 means cooling) [W]
      REAL(r64) :: LatCoilLoad              = 0.0d0      ! Ideal Loads System latent load on "coils" (<0 means cooling or dehumidfying) [W]
      INTEGER   :: OAFlowMaxCoolOutputError = 0          ! Counter for OAFlow > Max Cooling Flow error
      INTEGER   :: OAFlowMaxHeatOutputError = 0          ! Counter for OAFlow > Max Heating Flow error
      INTEGER   :: SaturationOutputError    = 0          ! Counter for OAFlow > Max Heating Flow error
      INTEGER   :: OAFlowMaxCoolOutputIndex = 0          ! Recurring warning index for OAFlow > Max Cooling Flow error
      INTEGER   :: OAFlowMaxHeatOutputIndex = 0          ! Recurring warning index for OAFlow > Max Heating Flow error
      INTEGER   :: SaturationOutputIndex    = 0          ! Recurring warning index for OAFlow > Max Heating Flow error
      INTEGER   :: AvailStatus              = 0
      INTEGER   :: CoolErrIndex             = 0          ! Cooling setpoint error index (recurring errors)
      INTEGER   :: HeatErrIndex             = 0          ! Heating setpoint error index (recurring errors)

      ! Output variables
      REAL(r64) :: SenHeatEnergy           = 0.0d0      ! Sensible heating energy consumed [J]
      REAL(r64) :: LatHeatEnergy           = 0.0d0      ! Latent   heating energy consumed [J]
      REAL(r64) :: TotHeatEnergy           = 0.0d0      ! Total    heating energy consumed [J]
      REAL(r64) :: SenCoolEnergy           = 0.0d0      ! Sensible cooling energy consumed [J]
      REAL(r64) :: LatCoolEnergy           = 0.0d0      ! Latent   cooling energy consumed [J]
      REAL(r64) :: TotCoolEnergy           = 0.0d0      ! Total    cooling energy consumed [J]
      REAL(r64) :: ZoneSenHeatEnergy       = 0.0d0      ! Sensible heating energy supplied to the zone [J]
      REAL(r64) :: ZoneLatHeatEnergy       = 0.0d0      ! Latent   heating energy supplied to the zone [J]
      REAL(r64) :: ZoneTotHeatEnergy       = 0.0d0      ! Total    heating energy supplied to the zone [J]
      REAL(r64) :: ZoneSenCoolEnergy       = 0.0d0      ! Sensible cooling energy supplied to the zone [J]
      REAL(r64) :: ZoneLatCoolEnergy       = 0.0d0      ! Latent   cooling energy supplied to the zone [J]
      REAL(r64) :: ZoneTotCoolEnergy       = 0.0d0      ! Total    cooling energy supplied to the zone [J]
      REAL(r64) :: OASenHeatEnergy         = 0.0d0      ! Sensible heating energy required for OA to equal zone air [J]
      REAL(r64) :: OALatHeatEnergy         = 0.0d0      ! Latent   heating energy required for OA to equal zone air [J]
      REAL(r64) :: OATotHeatEnergy         = 0.0d0      ! Total    heating energy required for OA to equal zone air [J]
      REAL(r64) :: OASenCoolEnergy         = 0.0d0      ! Sensible cooling energy required for OA to equal zone air [J]
      REAL(r64) :: OALatCoolEnergy         = 0.0d0      ! Latent   cooling energy required for OA to equal zone air [J]
      REAL(r64) :: OATotCoolEnergy         = 0.0d0      ! Total    cooling energy required for OA to equal zone air [J]
      REAL(r64) :: HtRecSenHeatEnergy      = 0.0d0      ! Sensible heating energy from heat reocovery [J]
      REAL(r64) :: HtRecLatHeatEnergy      = 0.0d0      ! Latent   heating energy from heat reocovery [J]
      REAL(r64) :: HtRecTotHeatEnergy      = 0.0d0      ! Total    heating energy from heat reocovery [J]
      REAL(r64) :: HtRecSenCoolEnergy      = 0.0d0      ! Sensible cooling energy from heat reocovery [J]
      REAL(r64) :: HtRecLatCoolEnergy      = 0.0d0      ! Latent   cooling energy from heat reocovery [J]
      REAL(r64) :: HtRecTotCoolEnergy      = 0.0d0      ! Total    cooling energy from heat reocovery [J]
      REAL(r64) :: SenHeatRate             = 0.0d0      ! Sensible heating rate consumed [W]
      REAL(r64) :: LatHeatRate             = 0.0d0      ! Latent   heating rate consumed [W]
      REAL(r64) :: TotHeatRate             = 0.0d0      ! Total    heating rate consumed [W]
      REAL(r64) :: SenCoolRate             = 0.0d0      ! Sensible cooling rate consumed [W]
      REAL(r64) :: LatCoolRate             = 0.0d0      ! Latent   cooling rate consumed [W]
      REAL(r64) :: TotCoolRate             = 0.0d0      ! Total    cooling rate consumed [W]
      REAL(r64) :: ZoneSenHeatRate         = 0.0d0      ! Sensible heating rate supplied to the zone [W]
      REAL(r64) :: ZoneLatHeatRate         = 0.0d0      ! Latent   heating rate supplied to the zone [W]
      REAL(r64) :: ZoneTotHeatRate         = 0.0d0      ! Total    heating rate supplied to the zone [W]
      REAL(r64) :: ZoneSenCoolRate         = 0.0d0      ! Sensible cooling rate supplied to the zone [W]
      REAL(r64) :: ZoneLatCoolRate         = 0.0d0      ! Latent   cooling rate supplied to the zone [W]
      REAL(r64) :: ZoneTotCoolRate         = 0.0d0      ! Total    cooling rate supplied to the zone [W]
      REAL(r64) :: OASenHeatRate           = 0.0d0      ! Sensible heating rate required for OA to equal zone air [W]
      REAL(r64) :: OALatHeatRate           = 0.0d0      ! Latent   heating rate required for OA to equal zone air [W]
      REAL(r64) :: OATotHeatRate           = 0.0d0      ! Total    heating rate required for OA to equal zone air [W]
      REAL(r64) :: OASenCoolRate           = 0.0d0      ! Sensible cooling rate required for OA to equal zone air [W]
      REAL(r64) :: OALatCoolRate           = 0.0d0      ! Latent   cooling rate required for OA to equal zone air [W]
      REAL(r64) :: OATotCoolRate           = 0.0d0      ! Total    cooling rate required for OA to equal zone air [W]
      REAL(r64) :: HtRecSenHeatRate        = 0.0d0      ! Sensible heating rate from heat reocovery [W]
      REAL(r64) :: HtRecLatHeatRate        = 0.0d0      ! Latent   heating rate from heat reocovery [W]
      REAL(r64) :: HtRecTotHeatRate        = 0.0d0      ! Total    heating rate from heat reocovery [W]
      REAL(r64) :: HtRecSenCoolRate        = 0.0d0      ! Sensible cooling rate from heat reocovery [W]
      REAL(r64) :: HtRecLatCoolRate        = 0.0d0      ! Latent   cooling rate from heat reocovery [W]
      REAL(r64) :: HtRecTotCoolRate        = 0.0d0      ! Total    cooling rate from heat reocovery [W]
      REAL(r64) :: TimeEconoActive         = 0.0d0      ! Time economizer is active [hrs]
      REAL(r64) :: TimeHtRecActive         = 0.0d0      ! Time heat reocovery is active [hrs]
    END TYPE ZonePurchasedAir


  !MODULE VARIABLE DECLARATIONS:
    TYPE (ZonePurchasedAir), ALLOCATABLE, DIMENSION(:) :: PurchAir   ! Used to specify purchased air parameters

    INTEGER, SAVE :: NumPurchAir
    LOGICAL, SAVE :: GetPurchAirInputFlag = .TRUE.
    LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName
  !SUBROUTINE SPECIFICATIONS FOR MODULE PurchasedAir:

PUBLIC  SimPurchasedAir
PRIVATE GetPurchasedAir
PRIVATE InitPurchasedAir
PRIVATE CalcPurchAirLoads
PRIVATE UpdatePurchasedAir
PRIVATE ReportPurchasedAir
PUBLIC  GetPurchasedAirOutAirMassFlow
PUBLIC  GetPurchasedAirZoneInletAirNode
PUBLIC  GetPurchasedAirMixedAirTemp
PUBLIC  GetPurchasedAirMixedAirHumRat
PUBLIC  GetPurchasedAirReturnAirNode
PRIVATE CalcPurchAirMinOAMassFlow
PRIVATE CalcPurchAirMixedAir

CONTAINS

SUBROUTINE SimPurchasedAir(PurchAirName, SysOutputProvided, MoistOutputProvided, FirstHVACIteration, &
                                  ControlledZoneNum, ActualZoneNum, CompIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   May 1997
          !       MODIFIED       Don Shirey, Aug 2009 (LatOutputProvided - now MoistOutputProvided)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages Purchased Air component simulation.
          ! It is called from SimZoneEquipment in the ZoneEquipmentManager
          ! at the system time step.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
      USE DataGlobals, ONLY:  MaxNameLength
      USE InputProcessor, ONLY: FindItemInList
      USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
      CHARACTER(len=*), INTENT(IN) :: PurchAirName
      REAL(r64), INTENT(INOUT) :: SysOutputProvided
      REAL(r64), INTENT(OUT)   :: MoistOutputProvided ! Moisture output provided (kg/s), dehumidification = negative
      LOGICAL, INTENT(IN) :: FirstHVACIteration
      INTEGER, INTENT(IN) :: ControlledZoneNum
      INTEGER, INTENT(IN) :: ActualZoneNum
      INTEGER, INTENT(INOUT) :: CompIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

      INTEGER :: PurchAirNum

      ! Beginning of Code

      IF (GetPurchAirInputFlag ) THEN
        CALL GetPurchasedAir
        GetPurchAirInputFlag = .FALSE.
      END IF

      ! Find the correct PurchasedAir Equipment
      IF (CompIndex == 0) THEN
        PurchAirNum = FindItemInList(PurchAirName, PurchAir%Name, NumPurchAir)
        IF (PurchAirNum == 0) THEN
          CALL ShowFatalError('SimPurchasedAir: Unit not found='//TRIM(PurchAirName))
        ENDIF
        CompIndex=PurchAirNum
      ELSE
        PurchAirNum=CompIndex
        IF (PurchAirNum > NumPurchAir .or. PurchAirNum < 1) THEN
          CALL ShowFatalError('SimPurchasedAir:  Invalid CompIndex passed='//  &
                              TRIM(TrimSigDigits(PurchAirNum))// &
                              ', Number of Units='//TRIM(TrimSigDigits(NumPurchAir))//  &
                              ', Entered Unit name='//TRIM(PurchAirName))
        ENDIF
        IF (CheckEquipName(PurchAirNum)) THEN
          IF (PurchAirName /= PurchAir(PurchAirNum)%Name) THEN
            CALL ShowFatalError('SimPurchasedAir: Invalid CompIndex passed='//  &
                                TRIM(TrimSigDigits(PurchAirNum))// &
                                ', Unit name='//TRIM(PurchAirName)//', stored Unit Name for that index='//  &
                                TRIM(PurchAir(PurchAirNum)%Name))
          ENDIF
          CheckEquipName(PurchAirNum)=.false.
        ENDIF
      ENDIF

      CALL InitPurchasedAir(PurchAirNum, FirstHVACIteration, ControlledZoneNum, ActualZoneNum)

      CALL CalcPurchAirLoads(PurchAirNum, SysOutputProvided, MoistOutputProvided, ControlledZoneNum, ActualZoneNum)

      CALL UpdatePurchasedAir(PurchAirNum)

      CALL ReportPurchasedAir(PurchAirNum)

      RETURN

    END SUBROUTINE SimPurchasedAir

    SUBROUTINE GetPurchasedAir

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   June 1997
          !       MODIFIED       M. Witte, June 2011, add new features including DCV, economizer, dehumidification
          !                                           and humidification controls
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Get the input data for the Purchased Air objects.
          ! Set up output variables.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,   ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, SameString, FindIteminList
  USE NodeInputManager, ONLY: GetOnlySingleNode,InitUniqueNodeCheck,CheckUniqueNodes,EndUniqueNodeCheck
  USE OutAirNodeManager, ONLY: CheckAndAddAirNodeNumber
  USE DataLoopNode
  USE DataIPShortCuts
  USE DataSizing, ONLY: OARequirements, NumOARequirements   ! to find DesignSpecification:OutdoorAir pointer
  USE DataContaminantBalance, ONLY: Contaminant

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      INTEGER :: PurchAirNum
      INTEGER :: NumAlphas
      INTEGER :: NumNums
      INTEGER :: IOSTAT
      CHARACTER(len=*), PARAMETER  :: RoutineName='GetPurchasedAir: ' ! include trailing blank space
      LOGICAL :: ErrorsFound = .false.   ! If errors detected in input
      LOGICAL :: IsNotOK                 ! Flag to verify name
      LOGICAL :: IsBlank                 ! Flag for blank name
      LOGICAL :: IsOANodeListed          ! Flag for OA node name listed in OutdoorAir:Node or Nodelist
      LOGICAL :: UniqueNodeError         ! Flag for non-unique node error(s)

      cCurrentModuleObject='ZoneHVAC:IdealLoadsAirSystem'

      NumPurchAir = GetNumObjectsFound(cCurrentModuleObject)

      ALLOCATE (PurchAir(NumPurchAir))
      ALLOCATE(CheckEquipName(NumPurchAir))
      CheckEquipName=.true.

      IF (NumPurchAir .GT. 0)THEN
        CALL InitUniqueNodeCheck(cCurrentModuleObject)
        DO PurchAirNum = 1,  NumPurchAir
          PurchAir(PurchAirNum)%cObjectName = cCurrentModuleObject

          CALL GetObjectItem(cCurrentModuleObject,PurchAirNum,cAlphaArgs,NumAlphas,rNumericArgs, &
                             NumNums,IOSTAT, NumBlank=lNumericFieldBlanks, AlphaBlank=lAlphaFieldBlanks, &
                             AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
          IsNotOK=.false.
          IsBlank=.false.
          CALL VerifyName(cAlphaArgs(1),PurchAir%Name,PurchAirNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
          IF (IsNotOK) THEN
            ErrorsFound=.true.
            IF (IsBlank) cAlphaArgs(1)='xxxxx'
          ENDIF
          PurchAir(PurchAirNum)%Name       = cAlphaArgs(1)
          ! get optional  availability schedule
          PurchAir(PurchAirNum)%AvailSched = cAlphaArgs(2)
          IF (lAlphaFieldBlanks(2)) THEN
            PurchAir(PurchAirNum)%AvailSchedPtr = ScheduleAlwaysOn
          ELSE
            PurchAir(PurchAirNum)%AvailSchedPtr = GetScheduleIndex(cAlphaArgs(2))
            IF (PurchAir(PurchAirNum)%AvailSchedPtr == 0) THEN
              CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid data')
              CALL ShowContinueError('Invalid-not found '//TRIM(cAlphaFieldNames(2))//  &
                 '="'//TRIM(cAlphaArgs(2))//'".')
              ErrorsFound=.true.
            END IF
          END IF
                  ! Purchased air supply air node is an outlet node
          PurchAir(PurchAirNum)%ZoneSupplyAirNodeNum  = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)
          UniqueNodeError=.false.
          CALL CheckUniqueNodes(cAlphaFieldNames(3),'NodeName',UniqueNodeError,CheckName=cAlphaArgs(3),ObjectName=cAlphaArgs(1))
          IF (UniqueNodeError) ErrorsFound=.true.
                  ! If new (optional) exhaust air node name is present, then register it as inlet
          IF (.NOT.lAlphaFieldBlanks(4)) THEN
            PurchAir(PurchAirNum)%ZoneExhaustAirNodeNum  = &
                 GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                 NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
            UniqueNodeError=.false.
            CALL CheckUniqueNodes(cAlphaFieldNames(4),'NodeName',UniqueNodeError,CheckName=cAlphaArgs(4),ObjectName=cAlphaArgs(1))
            IF (UniqueNodeError) ErrorsFound=.true.
          ENDIF
          PurchAir(PurchAirNum)%MaxHeatSuppAirTemp    = rNumericArgs(1)
          PurchAir(PurchAirNum)%MinCoolSuppAirTemp    = rNumericArgs(2)
          PurchAir(PurchAirNum)%MaxHeatSuppAirHumRat  = rNumericArgs(3)
          PurchAir(PurchAirNum)%MinCoolSuppAirHumRat  = rNumericArgs(4)

          IF (SameString(cAlphaArgs(5),'NoLimit')) THEN
            PurchAir(PurchAirNum)%HeatingLimit = NoLimit
          ELSEIF (SameString(cAlphaArgs(5),'LimitFlowRate')) THEN
            IF (lNumericFieldBlanks(5)) THEN
              PurchAir(PurchAirNum)%HeatingLimit = NoLimit
            ELSE
              PurchAir(PurchAirNum)%HeatingLimit = LimitFlowRate
            ENDIF
          ELSEIF (SameString(cAlphaArgs(5),'LimitCapacity')) THEN
            IF (lNumericFieldBlanks(6)) THEN
              PurchAir(PurchAirNum)%HeatingLimit = NoLimit
            ELSE
              PurchAir(PurchAirNum)%HeatingLimit = LimitCapacity
            ENDIF
          ELSEIF (SameString(cAlphaArgs(5),'LimitFlowRateAndCapacity')) THEN
            IF (lNumericFieldBlanks(5) .AND. lNumericFieldBlanks(6)) THEN
              PurchAir(PurchAirNum)%HeatingLimit = NoLimit
            ELSEIF (lNumericFieldBlanks(5)) THEN
              PurchAir(PurchAirNum)%HeatingLimit = LimitCapacity
            ELSEIF (lNumericFieldBlanks(6)) THEN
              PurchAir(PurchAirNum)%HeatingLimit = LimitFlowRate
            ELSE
              PurchAir(PurchAirNum)%HeatingLimit = LimitFlowRateAndCapacity
            ENDIF
          ELSE
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid data')
            CALL ShowContinueError('Invalid-entry '//TRIM(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
            CALL ShowContinueError('Valid entries are NoLimit, LimitFlowRate, LimitCapacity, or LimitFlowRateAndCapacity')
            ErrorsFound=.true.
          END IF
          PurchAir(PurchAirNum)%MaxHeatVolFlowRate = rNumericArgs(5)
          PurchAir(PurchAirNum)%MaxHeatSensCap     = rNumericArgs(6)

          IF (SameString(cAlphaArgs(6),'NoLimit')) THEN
            PurchAir(PurchAirNum)%CoolingLimit = NoLimit
          ELSEIF (SameString(cAlphaArgs(6),'LimitFlowRate')) THEN
            IF (lNumericFieldBlanks(7)) THEN
              PurchAir(PurchAirNum)%CoolingLimit = NoLimit
            ELSE
              PurchAir(PurchAirNum)%CoolingLimit = LimitFlowRate
            ENDIF
          ELSEIF (SameString(cAlphaArgs(6),'LimitCapacity')) THEN
            IF (lNumericFieldBlanks(8)) THEN
              PurchAir(PurchAirNum)%CoolingLimit = NoLimit
            ELSE
              PurchAir(PurchAirNum)%CoolingLimit = LimitCapacity
            ENDIF
          ELSEIF (SameString(cAlphaArgs(6),'LimitFlowRateAndCapacity')) THEN
            IF (lNumericFieldBlanks(7) .AND. lNumericFieldBlanks(8)) THEN
              PurchAir(PurchAirNum)%CoolingLimit = NoLimit
            ELSEIF (lNumericFieldBlanks(7)) THEN
              PurchAir(PurchAirNum)%CoolingLimit = LimitCapacity
            ELSEIF (lNumericFieldBlanks(8)) THEN
              PurchAir(PurchAirNum)%CoolingLimit = LimitFlowRate
            ELSE
              PurchAir(PurchAirNum)%CoolingLimit = LimitFlowRateAndCapacity
            ENDIF
          ELSE
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid data')
            CALL ShowContinueError('Invalid-entry '//TRIM(cAlphaFieldNames(6))//'="'//TRIM(cAlphaArgs(6))//'".')
            CALL ShowContinueError('Valid entries are NoLimit, LimitFlowRate, LimitCapacity, or LimitFlowRateAndCapacity')
            ErrorsFound=.true.
          END IF
          PurchAir(PurchAirNum)%MaxCoolVolFlowRate    = rNumericArgs(7)
          PurchAir(PurchAirNum)%MaxCoolTotCap         = rNumericArgs(8)

          ! get optional heating availability schedule
          PurchAir(PurchAirNum)%HeatSched = cAlphaArgs(7)
          IF (lAlphaFieldBlanks(7)) THEN
            PurchAir(PurchAirNum)%HeatSchedPtr = ScheduleAlwaysOn
          ELSE
            PurchAir(PurchAirNum)%HeatSchedPtr = GetScheduleIndex(cAlphaArgs(7))
            IF (PurchAir(PurchAirNum)%HeatSchedPtr == 0) THEN
              CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid data')
              CALL ShowContinueError('Invalid-not found '//TRIM(cAlphaFieldNames(7))//'="'//TRIM(cAlphaArgs(7))//'".')
              ErrorsFound=.true.
            END IF
          END IF
          ! get optional cooling availability schedule
          PurchAir(PurchAirNum)%CoolSched = cAlphaArgs(8)
          IF (lAlphaFieldBlanks(8)) THEN
            PurchAir(PurchAirNum)%CoolSchedPtr = ScheduleAlwaysOn
          ELSE
            PurchAir(PurchAirNum)%CoolSchedPtr = GetScheduleIndex(cAlphaArgs(8))
            IF (PurchAir(PurchAirNum)%CoolSchedPtr == 0) THEN
              CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid data')
              CALL ShowContinueError('Invalid-not found '//TRIM(cAlphaFieldNames(8))//'="'//TRIM(cAlphaArgs(8))//'".')
              ErrorsFound=.true.
            END IF
          END IF
          ! get Dehumidification control type
          IF (SameString(cAlphaArgs(9),'None')) THEN
                  PurchAir(PurchAirNum)%DehumidCtrlType = None
          ELSEIF (SameString(cAlphaArgs(9),'ConstantSensibleHeatRatio')) THEN
                  PurchAir(PurchAirNum)%DehumidCtrlType = ConstantSensibleHeatRatio
          ELSEIF(SameString(cAlphaArgs(9),'Humidistat')) THEN
                  PurchAir(PurchAirNum)%DehumidCtrlType = Humidistat
          ELSEIF(SameString(cAlphaArgs(9),'ConstantSupplyHumidityRatio')) THEN
                  PurchAir(PurchAirNum)%DehumidCtrlType = ConstantSupplyHumidityRatio
          ELSE
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid data')
            CALL ShowContinueError('Invalid-entry '//TRIM(cAlphaFieldNames(9))//'="'//TRIM(cAlphaArgs(9))//'".')
            CALL ShowContinueError('Valid entries are ConstantSensibleHeatRatio, Humidistat, or ConstantSupplyHumidityRatio')
            ErrorsFound=.true.
          END IF
          PurchAir(PurchAirNum)%CoolSHR = rNumericArgs(9)

          ! get Humidification control type
          IF (SameString(cAlphaArgs(10),'None')) THEN
                  PurchAir(PurchAirNum)%HumidCtrlType = None
          ELSEIF(SameString(cAlphaArgs(10),'Humidistat')) THEN
                  PurchAir(PurchAirNum)%HumidCtrlType = Humidistat
          ELSEIF(SameString(cAlphaArgs(10),'ConstantSupplyHumidityRatio')) THEN
                  PurchAir(PurchAirNum)%HumidCtrlType = ConstantSupplyHumidityRatio
          ELSE
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid data')
            CALL ShowContinueError('Invalid-entry '//TRIM(cAlphaFieldNames(10))//'="'//TRIM(cAlphaArgs(10))//'".')
            CALL ShowContinueError('Valid entries are None, Humidistat, or ConstantSupplyHumidityRatio')
            ErrorsFound=.true.
          END IF

          ! get Design specification outdoor air object
          IF(.NOT. lAlphaFieldBlanks(11))THEN
            PurchAir(PurchAirNum)%OARequirementsPtr = FindItemInList(cAlphaArgs(11),OARequirements%Name,NumOARequirements)
            IF(PurchAir(PurchAirNum)%OARequirementsPtr .EQ. 0)THEN
              CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//' invalid data')
              CALL ShowContinueError('Invalid-not found'//TRIM(cAlphaFieldNames(11))//'="'//TRIM(cAlphaArgs(11))//'".')
              ErrorsFound=.true.
            ELSE
              PurchAir(PurchAirNum)%OutdoorAir = .true.
            END IF
          END IF

          ! If outdoor air specified, then get Outdoor air inlet node and other outdoor air inputs
          IF (PurchAir(PurchAirNum)%OutdoorAir) THEN
            IF (lAlphaFieldBlanks(12)) THEN
                ! If there is outdoor air and outdoor air inlet node is blank, then create one
                IF (LEN_TRIM(cAlphaArgs(1)) < (MaxNameLength - 23) ) THEN ! protect against long name leading to > 100 chars
                    cAlphaArgs(12) = TRIM(cAlphaArgs(1))//' OUTDOOR AIR INLET NODE'
                ELSE
                    cAlphaArgs(12) = TRIM(cAlphaArgs(1)(1:75))//' OUTDOOR AIR INLET NODE'
                ENDIF
                IF (DisplayExtraWarnings) THEN
                    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//  &
                       trim(cAlphaArgs(1))//' blank field')
                    CALL ShowContinueError(TRIM(cAlphaFieldNames(12))// &
                          ' is blank, but there is outdoor air requested for this system.')
                    CALL ShowContinueError('Creating node name ='//TRIM(cAlphaArgs(12)))
                ENDIF
            ENDIF
                ! Register OA node
            PurchAir(PurchAirNum)%OutdoorAirNodeNum  = &
                  GetOnlySingleNode(cAlphaArgs(12),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
                  NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)
                ! Check if OA node is initialized in OutdoorAir:Node or OutdoorAir:Nodelist
            CALL CheckAndAddAirNodeNumber(PurchAir(PurchAirNum)%OutdoorAirNodeNum,IsOANodeListed)
            IF ((.not. IsOANodeListed) .AND. DisplayExtraWarnings) THEN
                CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//  &
                       trim(cAlphaArgs(1))//' missing data')
                CALL ShowContinueError(TRIM(cAlphaArgs(12))// &
                      ' does not appear in an OutdoorAir:NodeList or as an OutdoorAir:Node.')
                CALL ShowContinueError('Adding OutdoorAir:Node='//TRIM(cAlphaArgs(12)))
            ENDIF
            UniqueNodeError=.false.
            CALL CheckUniqueNodes(cAlphaFieldNames(12),'NodeName',UniqueNodeError,CheckName=cAlphaArgs(12),ObjectName=cAlphaArgs(1))
            IF (UniqueNodeError) ErrorsFound=.true.

            ! get Demand controlled ventilation type
            IF (SameString(cAlphaArgs(13),'None')) THEN
                    PurchAir(PurchAirNum)%DCVType = NoDCV
            ELSEIF(SameString(cAlphaArgs(13),'OccupancySchedule')) THEN
                    PurchAir(PurchAirNum)%DCVType = OccupancySchedule
            ELSEIF(SameString(cAlphaArgs(13),'CO2Setpoint')) THEN
              IF(Contaminant%CO2Simulation) THEN
                    PurchAir(PurchAirNum)%DCVType = CO2Setpoint
              ELSE
                    PurchAir(PurchAirNum)%DCVType = NoDCV
                    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//  &
                       trim(cAlphaArgs(1))//' invalid data')
                    CALL ShowContinueError(TRIM(cAlphaFieldNames(13))//'='//TRIM(cAlphaArgs(13))// &
                         ' but CO2 simulation is not active.')
                    CALL ShowContinueError('Resetting '//TRIM(cAlphaFieldNames(13))//' to NoDCV')
                    CALL ShowContinueError('To activate CO2 simulation, use ZoneAirContaminantBalance object'// &
                         ' and specify "Carbon Dioxide Concentration"="Yes".')
              END IF
            ELSE
              CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//  &
                       trim(cAlphaArgs(1))//' invalid data')
              CALL ShowContinueError('Invalid-entry '//TRIM(cAlphaFieldNames(13))//'='//TRIM(cAlphaArgs(13)))
              CALL ShowContinueError('Valid entries are None, OccupancySchedule, or CO2Setpoint')
              ErrorsFound=.true.
            END IF
            ! get Outdoor air economizer type
            IF (SameString(cAlphaArgs(14),'NoEconomizer')) THEN
                    PurchAir(PurchAirNum)%EconomizerType = NoEconomizer
            ELSEIF(SameString(cAlphaArgs(14),'DifferentialDryBulb')) THEN
                    PurchAir(PurchAirNum)%EconomizerType = DifferentialDryBulb
            ELSEIF(SameString(cAlphaArgs(14),'DifferentialEnthalpy')) THEN
                    PurchAir(PurchAirNum)%EconomizerType = DifferentialEnthalpy
            ELSE
              CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//  &
                       trim(cAlphaArgs(1))//' invalid data')
              CALL ShowContinueError('Invalid-entry '//TRIM(cAlphaFieldNames(14))//'='//TRIM(cAlphaArgs(14)))
              CALL ShowContinueError('Valid entries are NoEconomizer, DifferentialDryBulb, or DifferentialEnthalpy')
              ErrorsFound=.true.
            END IF
            ! get Outdoor air heat recovery type and effectiveness
            IF (SameString(cAlphaArgs(15),'None')) THEN
                    PurchAir(PurchAirNum)%HtRecType = NoHeatRecovery
            ELSEIF(SameString(cAlphaArgs(15),'Sensible')) THEN
                    PurchAir(PurchAirNum)%HtRecType = Sensible
            ELSEIF(SameString(cAlphaArgs(15),'Enthalpy')) THEN
                    PurchAir(PurchAirNum)%HtRecType = Enthalpy
            ELSE
              CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//  &
                       trim(cAlphaArgs(1))//' invalid data')
              CALL ShowContinueError('Invalid-entry '//TRIM(cAlphaFieldNames(15))//'='//TRIM(cAlphaArgs(15)))
              CALL ShowContinueError('Valid entries are None, Sensible, or Enthalpy')
              ErrorsFound=.true.
            END IF
          ELSE !No outdoorair
            PurchAir(PurchAirNum)%DCVType = NoDCV
            PurchAir(PurchAirNum)%EconomizerType = NoEconomizer
            PurchAir(PurchAirNum)%HtRecType = NoHeatRecovery
          END IF

          PurchAir(PurchAirNum)%HtRecSenEff = rNumericArgs(10)
          PurchAir(PurchAirNum)%HtRecLatEff = rNumericArgs(11)


           ! initialize the calculated and report values
          PurchAir(PurchAirNum)%MaxHeatMassFlowRate  = 0.0d0
          PurchAir(PurchAirNum)%MaxCoolMassFlowRate  = 0.0d0
          PurchAir(PurchAirNum)%SenHeatEnergy        = 0.0d0
          PurchAir(PurchAirNum)%LatHeatEnergy        = 0.0d0
          PurchAir(PurchAirNum)%TotHeatEnergy        = 0.0d0
          PurchAir(PurchAirNum)%SenCoolEnergy        = 0.0d0
          PurchAir(PurchAirNum)%LatCoolEnergy        = 0.0d0
          PurchAir(PurchAirNum)%TotCoolEnergy        = 0.0d0
          PurchAir(PurchAirNum)%ZoneSenHeatEnergy    = 0.0d0
          PurchAir(PurchAirNum)%ZoneLatHeatEnergy    = 0.0d0
          PurchAir(PurchAirNum)%ZoneTotHeatEnergy    = 0.0d0
          PurchAir(PurchAirNum)%ZoneSenCoolEnergy    = 0.0d0
          PurchAir(PurchAirNum)%ZoneLatCoolEnergy    = 0.0d0
          PurchAir(PurchAirNum)%ZoneTotCoolEnergy    = 0.0d0
          PurchAir(PurchAirNum)%OASenHeatEnergy      = 0.0d0
          PurchAir(PurchAirNum)%OALatHeatEnergy      = 0.0d0
          PurchAir(PurchAirNum)%OATotHeatEnergy      = 0.0d0
          PurchAir(PurchAirNum)%OASenCoolEnergy      = 0.0d0
          PurchAir(PurchAirNum)%OALatCoolEnergy      = 0.0d0
          PurchAir(PurchAirNum)%OATotCoolEnergy      = 0.0d0
          PurchAir(PurchAirNum)%HtRecSenHeatEnergy   = 0.0d0
          PurchAir(PurchAirNum)%HtRecLatHeatEnergy   = 0.0d0
          PurchAir(PurchAirNum)%HtRecTotHeatEnergy   = 0.0d0
          PurchAir(PurchAirNum)%HtRecSenCoolEnergy   = 0.0d0
          PurchAir(PurchAirNum)%HtRecLatCoolEnergy   = 0.0d0
          PurchAir(PurchAirNum)%HtRecTotCoolEnergy   = 0.0d0
          PurchAir(PurchAirNum)%SenHeatRate          = 0.0d0
          PurchAir(PurchAirNum)%LatHeatRate          = 0.0d0
          PurchAir(PurchAirNum)%TotHeatRate          = 0.0d0
          PurchAir(PurchAirNum)%SenCoolRate          = 0.0d0
          PurchAir(PurchAirNum)%LatCoolRate          = 0.0d0
          PurchAir(PurchAirNum)%TotCoolRate          = 0.0d0
          PurchAir(PurchAirNum)%ZoneSenHeatRate      = 0.0d0
          PurchAir(PurchAirNum)%ZoneLatHeatRate      = 0.0d0
          PurchAir(PurchAirNum)%ZoneTotHeatRate      = 0.0d0
          PurchAir(PurchAirNum)%ZoneSenCoolRate      = 0.0d0
          PurchAir(PurchAirNum)%ZoneLatCoolRate      = 0.0d0
          PurchAir(PurchAirNum)%ZoneTotCoolRate      = 0.0d0
          PurchAir(PurchAirNum)%OASenHeatRate        = 0.0d0
          PurchAir(PurchAirNum)%OALatHeatRate        = 0.0d0
          PurchAir(PurchAirNum)%OATotHeatRate        = 0.0d0
          PurchAir(PurchAirNum)%OASenCoolRate        = 0.0d0
          PurchAir(PurchAirNum)%OALatCoolRate        = 0.0d0
          PurchAir(PurchAirNum)%OATotCoolRate        = 0.0d0
          PurchAir(PurchAirNum)%HtRecSenHeatRate     = 0.0d0
          PurchAir(PurchAirNum)%HtRecLatHeatRate     = 0.0d0
          PurchAir(PurchAirNum)%HtRecTotHeatRate     = 0.0d0
          PurchAir(PurchAirNum)%HtRecSenCoolRate     = 0.0d0
          PurchAir(PurchAirNum)%HtRecLatCoolRate     = 0.0d0
          PurchAir(PurchAirNum)%HtRecTotCoolRate     = 0.0d0

        END DO
        CALL EndUniqueNodeCheck(cCurrentModuleObject)
      END IF

      DO PurchAirNum = 1,NumPurchAir

        ! Setup Output variables
        !    energy variables
        CALL SetupOutputVariable('Zone Ideal Loads Supply Air Sensible Heating Energy [J]', PurchAir(PurchAirNum)%SenHeatEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Supply Air Latent Heating Energy [J]', PurchAir(PurchAirNum)%LatHeatEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Supply Air Total Heating Energy [J]', PurchAir(PurchAirNum)%TotHeatEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name,  &
                               ResourceTypeKey='DISTRICTHEATING',EndUseKey='Heating',GroupKey='System')
        CALL SetupOutputVariable('Zone Ideal Loads Supply Air Sensible Cooling Energy [J]', PurchAir(PurchAirNum)%SenCoolEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Supply Air Latent Cooling Energy [J]', PurchAir(PurchAirNum)%LatCoolEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Supply Air Total Cooling Energy [J]', PurchAir(PurchAirNum)%TotCoolEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name,  &
                               ResourceTypeKey='DISTRICTCOOLING',EndUseKey='Cooling',GroupKey='System')
        CALL SetupOutputVariable('Zone Ideal Loads Zone Sensible Heating Energy [J]', PurchAir(PurchAirNum)%ZoneSenHeatEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Zone Latent Heating Energy [J]', PurchAir(PurchAirNum)%ZoneLatHeatEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Zone Total Heating Energy [J]', PurchAir(PurchAirNum)%ZoneTotHeatEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Zone Sensible Cooling Energy [J]', PurchAir(PurchAirNum)%ZoneSenCoolEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Zone Latent Cooling Energy [J]', PurchAir(PurchAirNum)%ZoneLatCoolEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Zone Total Cooling Energy [J]', PurchAir(PurchAirNum)%ZoneTotCoolEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Outdoor Air Sensible Heating Energy [J]', &
                                PurchAir(PurchAirNum)%OASenHeatEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Outdoor Air Latent Heating Energy [J]', PurchAir(PurchAirNum)%OALatHeatEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Outdoor Air Total Heating Energy [J]', PurchAir(PurchAirNum)%OATotHeatEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Outdoor Air Sensible Cooling Energy [J]', &
                                  PurchAir(PurchAirNum)%OASenCoolEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Outdoor Air Latent Cooling Energy [J]', PurchAir(PurchAirNum)%OALatCoolEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Outdoor Air Total Cooling Energy [J]', PurchAir(PurchAirNum)%OATotCoolEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Heat Recovery Sensible Heating Energy [J]', &
                                  PurchAir(PurchAirNum)%HtRecSenHeatEnergy,&
                               'System','Sum',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Heat Recovery Latent Heating Energy [J]', &
                                  PurchAir(PurchAirNum)%HtRecLatHeatEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Heat Recovery Total Heating Energy [J]', &
                                  PurchAir(PurchAirNum)%HtRecTotHeatEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Heat Recovery Sensible Cooling Energy [J]',   &
                               PurchAir(PurchAirNum)%HtRecSenCoolEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Heat Recovery Latent Cooling Energy [J]', &
                                  PurchAir(PurchAirNum)%HtRecLatCoolEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Heat Recovery Total Cooling Energy [J]', &
                                  PurchAir(PurchAirNum)%HtRecTotCoolEnergy, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)

        !    rate variables
        CALL SetupOutputVariable('Zone Ideal Loads Supply Air Sensible Heating Rate [W]', PurchAir(PurchAirNum)%SenHeatRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Supply Air Latent Heating Rate [W]', PurchAir(PurchAirNum)%LatHeatRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Supply Air Total Heating Rate [W]', PurchAir(PurchAirNum)%TotHeatRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Supply Air Sensible Cooling Rate [W]', PurchAir(PurchAirNum)%SenCoolRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Supply Air Latent Cooling Rate [W]', PurchAir(PurchAirNum)%LatCoolRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Supply Air Total Cooling Rate [W]', PurchAir(PurchAirNum)%TotCoolRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Zone Sensible Heating Rate [W]', PurchAir(PurchAirNum)%ZoneSenHeatRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Zone Latent Heating Rate [W]', PurchAir(PurchAirNum)%ZoneLatHeatRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Zone Total Heating Rate [W]', PurchAir(PurchAirNum)%ZoneTotHeatRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Zone Sensible Cooling Rate [W]', PurchAir(PurchAirNum)%ZoneSenCoolRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Zone Latent Cooling Rate [W]', PurchAir(PurchAirNum)%ZoneLatCoolRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Zone Total Cooling Rate [W]', PurchAir(PurchAirNum)%ZoneTotCoolRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Outdoor Air Sensible Heating Rate [W]', PurchAir(PurchAirNum)%OASenHeatRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Outdoor Air Latent Heating Rate [W]', PurchAir(PurchAirNum)%OALatHeatRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Outdoor Air Total Heating Rate [W]', PurchAir(PurchAirNum)%OATotHeatRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Outdoor Air Sensible Cooling Rate [W]', PurchAir(PurchAirNum)%OASenCoolRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Outdoor Air Latent Cooling Rate [W]', PurchAir(PurchAirNum)%OALatCoolRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Outdoor Air Total Cooling Rate [W]', PurchAir(PurchAirNum)%OATotCoolRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Heat Recovery Sensible Heating Rate [W]', &
                                  PurchAir(PurchAirNum)%HtRecSenHeatRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Heat Recovery Latent Heating Rate [W]', PurchAir(PurchAirNum)%HtRecLatHeatRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Heat Recovery Total Heating Rate [W]', PurchAir(PurchAirNum)%HtRecTotHeatRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Heat Recovery Sensible Cooling Rate [W]', &
                                  PurchAir(PurchAirNum)%HtRecSenCoolRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Heat Recovery Latent Cooling Rate [W]', PurchAir(PurchAirNum)%HtRecLatCoolRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Heat Recovery Total Cooling Rate [W]', PurchAir(PurchAirNum)%HtRecTotCoolRate, &
                               'System','Average',PurchAir(PurchAirNum)%Name)

        CALL SetupOutputVariable('Zone Ideal Loads Economizer Active Time [hr]', PurchAir(PurchAirNum)%TimeEconoActive, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)
        CALL SetupOutputVariable('Zone Ideal Loads Heat Recovery Active Time [hr]', PurchAir(PurchAirNum)%TimeHtRecActive, &
                               'System','Sum',PurchAir(PurchAirNum)%Name)

        CALL SetupOutputVariable('Zone Ideal Loads Hybrid Ventilation Available Status []',PurchAir(PurchAirNum)%AvailStatus,&
                               'System','Average',PurchAir(PurchAirNum)%Name)

        IF (AnyEnergyManagementSystemInModel) THEN
          CALL SetupEMSActuator('Ideal Loads Air System', PurchAir(PurchAirNum)%Name, 'Air Mass Flow Rate' , '[kg/s]', &
                                  PurchAir(PurchAirNum)%EMSOverrideMdotOn, PurchAir(PurchAirNum)%EMSValueMassFlowRate )
          CALL SetupEMSActuator('Ideal Loads Air System', PurchAir(PurchAirNum)%Name, 'Outdoor Air Mass Flow Rate' , '[kg/s]', &
                                  PurchAir(PurchAirNum)%EMSOverrideOAMdotOn, PurchAir(PurchAirNum)%EMSValueOAMassFlowRate )
          CALL SetupEMSActuator('Ideal Loads Air System', PurchAir(PurchAirNum)%Name, 'Air Temperature' , '[C]', &
                                  PurchAir(PurchAirNum)%EMSOverrideSupplyTempOn, PurchAir(PurchAirNum)%EMSValueSupplyTemp )
          CALL SetupEMSActuator('Ideal Loads Air System', PurchAir(PurchAirNum)%Name, 'Air Humidity Ratio' , '[kgWater/kgDryAir]', &
                                  PurchAir(PurchAirNum)%EMSOverrideSupplyHumRatOn, PurchAir(PurchAirNum)%EMSValueSupplyHumRat )

        ENDIF
      END DO


      IF (ErrorsFound) THEN
        CALL ShowFatalError(RoutineName//'Errors found in input. Preceding conditions cause termination.')
      ENDIF

      RETURN

    END SUBROUTINE GetPurchasedAir


    SUBROUTINE InitPurchasedAir(PurchAirNum,FirstHVACIteration,ControlledZoneNum,ActualZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   Nov 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Initialize the PurchAir data structure.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
     USE DataHeatBalance, ONLY: Zone
     USE General, ONLY: RoundSigDigits
     USE DataZoneEquipment,  ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList,ZoneEquipConfig
     USE DataSizing, ONLY: OARequirements   ! to access DesignSpecification:OutdoorAir inputs
     USE DataHeatBalance,  ONLY: Zone ! to access zone area, volume, and multipliers
     USE General, ONLY: FindNumberinList
     USE DataLoopNode, ONLY: NodeID

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
     LOGICAL, INTENT(IN) :: FirstHVACIteration !unused1208
     INTEGER, INTENT(IN) :: PurchAirNum
     INTEGER, INTENT(IN) :: ControlledZoneNum
     INTEGER, INTENT(IN) :: ActualZoneNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      LOGICAL,SAVE        :: MyOneTimeFlag = .true.
      LOGICAL,SAVE        :: ZoneEquipmentListChecked = .false.  ! True after the Zone Equipment List has been checked for items
      Integer             :: Loop
      LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag
      LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MySizeFlag
      LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: OneTimeUnitInitsDone ! True if one-time inits for PurchAirNum are completed
!      LOGICAL :: ErrorsFound = .false.   ! If errors detected in input
      LOGICAL :: UnitOn   ! simple checks for error
      LOGICAL :: CoolOn   ! simple checks for error
      LOGICAL :: HeatOn   ! simple checks for error
      Integer             :: SupplyNodeNum    ! Node number for ideal loads supply node
      Integer             :: ExhaustNodeNum    ! Node number for ideal loads exhaust node
      Integer             :: NodeIndex  ! Array index of zone inlet or zone exhaust node that matches ideal loads node
      LOGICAL :: UseReturnNode   ! simple checks for error


      ! Do the Begin Simulation initializations
      IF (MyOneTimeFlag) THEN

        ALLOCATE(MyEnvrnFlag(NumPurchAir))
        ALLOCATE(MySizeFlag(NumPurchAir))
        ALLOCATE(OneTimeUnitInitsDone(NumPurchAir))
        MyEnvrnFlag = .TRUE.
        MySizeFlag = .TRUE.
        OneTimeUnitInitsDone = .false.
        MyOneTimeFlag = .false.

      END IF

      ! need to check all units to see if they are on Zone Equipment List or issue warning
      IF (.not. ZoneEquipmentListChecked .and. ZoneEquipInputsFilled) THEN
        ZoneEquipmentListChecked=.true.
        DO Loop=1,NumPurchAir
          IF (CheckZoneEquipmentList(PurchAir(Loop)%cObjectName,PurchAir(Loop)%Name)) CYCLE
          CALL ShowSevereError('InitPurchasedAir: '//TRIM(PurchAir(Loop)%cObjectName)//' = '// &
                               TRIM(PurchAir(Loop)%Name)//' is not on any ZoneHVAC:EquipmentList.  It will not be simulated.')
        ENDDO
      ENDIF

      ! one time inits for each unit - links PurchAirNum with static input data from ControlledZoneNum and ActualZoneNum
      IF (.not. OneTimeUnitInitsDone(PurchAirNum)) THEN
        OneTimeUnitInitsDone(PurchAirNum) = .true.

        ! Is the supply node really a zone inlet node?
        ! this check has to be done here because of SimPurchasedAir passing in ControlledZoneNum
        SupplyNodeNum = PurchAir(PurchAirNum)%ZoneSupplyAirNodeNum
        IF (SupplyNodeNum .GT. 0) THEN
          NodeIndex = FindNumberinList(SupplyNodeNum, ZoneEquipConfig(ControlledZoneNum)%InletNode, &
                                           ZoneEquipConfig(ControlledZoneNum)%NumInletNodes)
          IF (NodeIndex == 0) THEN
            CALL ShowSevereError('InitPurchasedAir: In '//TRIM(PurchAir(PurchAirNum)%cObjectName)//' = '// &
                  TRIM(PurchAir(PurchAirNum)%Name))
            CALL ShowContinueError('Zone Supply Air Node Name='//TRIM(NodeID(SupplyNodeNum))//' is not a zone inlet node.')
            CALL ShowContinueError('Check ZoneHVAC:EquipmentConnections for zone='// &
                                   TRIM(ZoneEquipConfig(ControlledZoneNum)%ZoneName))
            CALL ShowFatalError('Preceding condition causes termination.')
          END IF
        END IF

        ! Set recirculation node number
        ! If exhaust node is specified, then recirculation is exhaust node, otherwise use zone return node
        ! this check has to be done here because of SimPurchasedAir passing in ControlledZoneNum
        UseReturnNode = .false.
        IF (PurchAir(PurchAirNum)%ZoneExhaustAirNodeNum .GT. 0) THEN
          ExhaustNodeNum = PurchAir(PurchAirNum)%ZoneExhaustAirNodeNum
          NodeIndex = FindNumberinList(ExhaustNodeNum, ZoneEquipConfig(ControlledZoneNum)%ExhaustNode, &
                                           ZoneEquipConfig(ControlledZoneNum)%NumExhaustNodes)
          IF (NodeIndex == 0) THEN
            CALL ShowSevereError('InitPurchasedAir: In '//TRIM(PurchAir(PurchAirNum)%cObjectName)//' = '// &
                  TRIM(PurchAir(PurchAirNum)%Name))
            CALL ShowContinueError('Zone Exhaust Air Node Name='//TRIM(NodeID(ExhaustNodeNum))//' is not a zone exhaust node.')
            CALL ShowContinueError('Check ZoneHVAC:EquipmentConnections for zone='// &
                                   TRIM(ZoneEquipConfig(ControlledZoneNum)%ZoneName))
            CALL ShowContinueError('Zone return air node will be used for ideal loads recirculation air.')
            UseReturnNode = .true.
          ELSE
            PurchAir(PurchAirNum)%ZoneRecircAirNodeNum = PurchAir(PurchAirNum)%ZoneExhaustAirNodeNum
          END IF
        ELSE
          UseReturnNode = .true.
        END IF
        IF(UseReturnNode) THEN
          IF (ZoneEquipConfig(ControlledZoneNum)%ReturnAirNode .GT. 0) THEN
            PurchAir(PurchAirNum)%ZoneRecircAirNodeNum = ZoneEquipConfig(ControlledZoneNum)%ReturnAirNode
          ELSE
            CALL ShowFatalError('InitPurchasedAir: In '//TRIM(PurchAir(PurchAirNum)%cObjectName)//' = '// &
                  TRIM(PurchAir(PurchAirNum)%Name))
            CALL ShowContinueError(' Invalid recirculation node. No exhaust or return node has been'// &
                  ' specified for this zone in ZoneHVAC:EquipmentConnections.')
            CALL ShowFatalError('Preceding condition causes termination.')
          END IF
        END IF
        ! If there is OA and economizer is active, then there must be a limit on cooling flow rate
        IF (PurchAir(PurchAirNum)%OutdoorAir .AND. (PurchAir(PurchAirNum)%EconomizerType /= NoEconomizer)) THEN
          IF ((PurchAir(PurchAirNum)%CoolingLimit == NoLimit) .OR. (PurchAir(PurchAirNum)%CoolingLimit == LimitCapacity)) THEN
            CALL ShowSevereError('InitPurchasedAir: In '//TRIM(PurchAir(PurchAirNum)%cObjectName)//' = '// &
                  TRIM(PurchAir(PurchAirNum)%Name))
            CALL ShowContinueError('There is outdoor air with economizer active but there is no limit on cooling air flow rate.')
            CALL ShowContinueError('Cooling Limit must be set to LimitFlowRate or LimitFlowRateAndCapacity, and '// &
                                   'Maximum Cooling Air Flow Rate must be set to a value or autosize.')
            CALL ShowContinueError('Simulation will proceed with no limit on outdoor air flow rate.')
          END IF
        END IF
      END IF

      IF ( .NOT. SysSizingCalc .AND. MySizeFlag(PurchAirNum) ) THEN

        CALL SizePurchasedAir(PurchAirNum)

        MySizeFlag(PurchAirNum) = .FALSE.
      END IF

      ! Do the Begin Environment initializations
      IF (BeginEnvrnFlag .and. MyEnvrnFlag(PurchAirNum)) THEN

        IF ((PurchAir(PurchAirNum)%HeatingLimit == LimitFlowRate) .OR. &
            (PurchAir(PurchAirNum)%HeatingLimit == LimitFlowRateAndCapacity)) THEN
          PurchAir(PurchAirNum)%MaxHeatMassFlowRate = StdRhoAir * PurchAir(PurchAirNum)%MaxHeatVolFlowRate
        ELSE
          PurchAir(PurchAirNum)%MaxHeatMassFlowRate = 0.0d0
        END IF
        IF ((PurchAir(PurchAirNum)%CoolingLimit == LimitFlowRate) .OR. &
            (PurchAir(PurchAirNum)%CoolingLimit == LimitFlowRateAndCapacity)) THEN
          PurchAir(PurchAirNum)%MaxCoolMassFlowRate = StdRhoAir * PurchAir(PurchAirNum)%MaxCoolVolFlowRate
        ELSE
          PurchAir(PurchAirNum)%MaxCoolMassFlowRate = 0.0d0
        END IF
        MyEnvrnFlag(PurchAirNum) = .FALSE.
      END IF

      IF (.not. BeginEnvrnFlag) THEN
        MyEnvrnFlag(PurchAirNum) = .TRUE.
      ENDIF

      ! These initializations are done every iteration
      ! check that supply air temps can meet the zone thermostat setpoints
      IF (PurchAir(PurchAirNum)%MinCoolSuppAirTemp > ZoneThermostatSetPointHi(ActualZoneNum) .AND. &
          ZoneThermostatSetPointHi(ActualZoneNum) .NE. 0 .and. PurchAir(PurchAirNum)%CoolingLimit == NoLimit) THEN
            ! Check if the unit is scheduled off
        UnitOn = .true.
!        IF (PurchAir(PurchAirNum)%AvailSchedPtr > 0) THEN
          IF (GetCurrentScheduleValue(PurchAir(PurchAirNum)%AvailSchedPtr) <= 0) THEN
            UnitOn = .FALSE.
          END IF
!        END IF
            ! Check if cooling available
        CoolOn = .TRUE.
!        IF (PurchAir(PurchAirNum)%CoolSchedPtr > 0) THEN
          IF (GetCurrentScheduleValue(PurchAir(PurchAirNum)%CoolSchedPtr) <= 0) THEN
            CoolOn = .FALSE.
          END IF
!        END IF
        IF (UnitOn .and. CoolOn) THEN
          IF (PurchAir(PurchAirNum)%CoolErrIndex == 0) THEN
            CALL ShowSevereError('InitPurchasedAir: For '//TRIM(PurchAir(PurchAirNum)%cObjectName)//' = '// &
                               TRIM(PurchAir(PurchAirNum)%Name) //' serving Zone ' // TRIM(Zone(ActualZoneNum)%Name) )
            CALL ShowContinueError('..the minimum supply air temperature for cooling ['//  &
                TRIM(RoundSigDigits(PurchAir(PurchAirNum)%MinCoolSuppAirTemp,2))// &
                '] is greater than the zone cooling mean air temperature (MAT) setpoint ['//  &
                TRIM(RoundSigDigits(ZoneThermostatSetPointHi(ActualZoneNum),2))//'].')
            CALL ShowContinueError('..For operative and comfort thermostat controls, the MAT setpoint is computed.')
            CALL ShowContinueError('..This error may indicate that the mean radiant temperature '//  &
               'or another comfort factor is too warm.')
            CALL ShowContinueError('Unit availability is nominally ON and Cooling availability is nominally ON.')
            CALL ShowContinueError('Limit Cooling Capacity Type='//trim(cLimitType(PurchAir(PurchAirNum)%CoolingLimit)))
! could check for optemp control or comfort control here
            CALL ShowContinueErrorTimeStamp(' ')
          ENDIF
          CALL ShowRecurringSevereErrorAtEnd('InitPurchasedAir: For '//TRIM(PurchAir(PurchAirNum)%cObjectName)//' = '// &
                             TRIM(PurchAir(PurchAirNum)%Name) //' serving Zone ' // TRIM(Zone(ActualZoneNum)%Name)// &
                             ', the minimum supply air temperature for cooling error continues',                     &
                             PurchAir(PurchAirNum)%CoolErrIndex,ReportMinOf=PurchAir(PurchAirNum)%MinCoolSuppAirTemp, &
                             ReportMaxOf=PurchAir(PurchAirNum)%MinCoolSuppAirTemp,ReportMinUnits='C',ReportMaxUnits='C')
        ENDIF
      END IF
      IF (PurchAir(PurchAirNum)%MaxHeatSuppAirTemp < ZoneThermostatSetPointLo(ActualZoneNum) .AND. &
          ZoneThermostatSetPointLo(ActualZoneNum) .NE. 0 .and. PurchAir(PurchAirNum)%HeatingLimit == NoLimit) THEN
            ! Check if the unit is scheduled off
        UnitOn = .true.
!        IF (PurchAir(PurchAirNum)%AvailSchedPtr > 0) THEN
          IF (GetCurrentScheduleValue(PurchAir(PurchAirNum)%AvailSchedPtr) <= 0) THEN
            UnitOn = .FALSE.
          END IF
!        END IF
            ! Check if heating and cooling available
        HeatOn = .TRUE.
!        IF (PurchAir(PurchAirNum)%HeatSchedPtr > 0) THEN
          IF (GetCurrentScheduleValue(PurchAir(PurchAirNum)%HeatSchedPtr) <= 0) THEN
            HeatOn = .FALSE.
          END IF
!        END IF
        IF (UnitOn .and. HeatOn) THEN
          IF (PurchAir(PurchAirNum)%HeatErrIndex == 0) THEN
            CALL ShowSevereMessage('InitPurchasedAir: For '//TRIM(PurchAir(PurchAirNum)%cObjectName)//' = '//&
                                 TRIM(PurchAir(PurchAirNum)%Name) // ' serving Zone ' // TRIM(Zone(ActualZoneNum)%Name) )
            CALL ShowContinueError('..the maximum supply air temperature for heating ['//  &
                TRIM(RoundSigDigits(PurchAir(PurchAirNum)%MaxHeatSuppAirTemp,2))// &
                '] is less than the zone mean air temperature heating setpoint ['//  &
                TRIM(RoundSigDigits(ZoneThermostatSetPointLo(ActualZoneNum),2))//'].')
            CALL ShowContinueError('..For operative and comfort thermostat controls, the MAT setpoint is computed.')
            CALL ShowContinueError('..This error may indicate that the mean radiant temperature '//  &
               'or another comfort factor is too cold.')
            CALL ShowContinueError('Unit availability is nominally ON and Heating availability is nominally ON.')
            CALL ShowContinueError('Limit Heating Capacity Type='//trim(cLimitType(PurchAir(PurchAirNum)%HeatingLimit)))
! could check for optemp control or comfort control here
            CALL ShowContinueErrorTimeStamp(' ')
          ENDIF
          CALL ShowRecurringSevereErrorAtEnd('InitPurchasedAir: For '//TRIM(PurchAir(PurchAirNum)%cObjectName)//' = '// &
                             TRIM(PurchAir(PurchAirNum)%Name) //' serving Zone ' // TRIM(Zone(ActualZoneNum)%Name)// &
                             ', maximum supply air temperature for heating error continues',                         &
                             PurchAir(PurchAirNum)%HeatErrIndex,ReportMinOf=PurchAir(PurchAirNum)%MaxHeatSuppAirTemp, &
                             ReportMaxOf=PurchAir(PurchAirNum)%MaxHeatSuppAirTemp,ReportMinUnits='C',ReportMaxUnits='C')
        ENDIF
      END IF
!      IF (ErrorsFound .and. .not. WarmupFlag) THEN
!        CALL ShowFatalError('Preceding conditions cause termination.')
!      ENDIF

      RETURN

    END SUBROUTINE InitPurchasedAir

    SUBROUTINE SizePurchasedAir(PurchAirNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   April 2003
          !       MODIFIED       M. Witte, June 2011, add sizing for new capacity fields
          !                      August 2013 Daeho Kang, add component sizing table entries
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Purchased Air Components for which flow rates have not been
          ! specified in the input.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the zone sizing arrays.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
      USE DataSizing
      USE InputProcessor
      USE ReportSizingManager, ONLY: ReportSizingOutput
      USE Psychrometrics, ONLY: PsyCpAirFnWTdb, RhoH2O, CpHw, CpCw, PsyHFnTdbW
      USE General,             ONLY: RoundSigDigits

      IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
      Integer, Intent(IN) :: PurchAirNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      REAL(r64)           :: MixedAirTemp
      REAL(r64)           :: OutletTemp
      REAL(r64)           :: MixedAirHumRat
      REAL(r64)           :: OutletHumRat
      REAL(r64)           :: DesignLoad
      LOGICAL             :: IsAutosize              ! Indicator to autosize
      REAL(r64)           :: MaxHeatVolFlowRateDes   ! Autosized maximum heating air flow for reporting
      REAL(r64)           :: MaxHeatVolFlowRateUser  ! Hardsized maximum heating air flow for reporting
      REAL(r64)           :: MaxCoolVolFlowRateDes   ! Autosized maximum cooling air flow for reporting
      REAL(r64)           :: MaxCoolVolFlowRateUser  ! Hardsized maximum cooling air flow for reporting
      REAL(r64)           :: MaxHeatSensCapDes       ! Autosized maximum sensible heating capacity for reporting
      REAL(r64)           :: MaxHeatSensCapUser      ! Hardsized maximum sensible heating capacity for reporting
      REAL(r64)           :: MaxCoolTotCapDes        ! Autosized maximum sensible cooling capacity for reporting
      REAL(r64)           :: MaxCoolTotCapUser       ! Hardsized maximum sensible cooling capacity for reporting

      IsAutosize = .FALSE.
      MaxHeatVolFlowRateDes = 0.0d0
      MaxHeatVolFlowRateUser = 0.0d0
      MaxCoolVolFlowRateDes = 0.0d0
      MaxCoolVolFlowRateUser = 0.0d0
      MaxHeatSensCapDes = 0.0d0
      MaxHeatSensCapUser = 0.0d0
      MaxCoolTotCapDes = 0.0d0
      MaxCoolTotCapUser = 0.0d0

      IF ((PurchAir(PurchAirNum)%MaxHeatVolFlowRate == AutoSize) .AND. &
          ((PurchAir(PurchAirNum)%HeatingLimit == LimitFlowRate) .OR. &
           (PurchAir(PurchAirNum)%HeatingLimit == LimitFlowRateAndCapacity))) THEN
        IsAutosize = .TRUE.
      END IF

      IF (CurZoneEqNum > 0) THEN
        IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
          IF (PurchAir(PurchAirNum)%MaxHeatVolFlowRate > 0.0d0) THEN
            CALL ReportSizingOutput(PurchAir(PurchAirNum)%cObjectName, PurchAir(PurchAirNum)%Name, &
                        'User-Specified Maximum Heating Air Flow Rate [m3/s]', PurchAir(PurchAirNum)%MaxHeatVolFlowRate)
          END IF
        ELSE
          CALL CheckZoneSizing(TRIM(PurchAir(PurchAirNum)%cObjectName), PurchAir(PurchAirNum)%Name)
          MaxHeatVolFlowRateDes = FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow
          IF (MaxHeatVolFlowRateDes < SmallAirVolFlow) THEN
            MaxHeatVolFlowRateDes = 0.0d0
          END IF
          IF (IsAutosize) THEN
            PurchAir(PurchAirNum)%MaxHeatVolFlowRate = MaxHeatVolFlowRateDes
            CALL ReportSizingOutput(PurchAir(PurchAirNum)%cObjectName, PurchAir(PurchAirNum)%Name, &
                              'Design Size Maximum Heating Air Flow Rate [m3/s]', MaxHeatVolFlowRateDes)
          ELSE
            IF (PurchAir(PurchAirNum)%MaxHeatVolFlowRate > 0.0d0 .AND. MaxHeatVolFlowRateDes > 0.0d0) THEN
              MaxHeatVolFlowRateUser = PurchAir(PurchAirNum)%MaxHeatVolFlowRate
              CALL ReportSizingOutput(PurchAir(PurchAirNum)%cObjectName, PurchAir(PurchAirNum)%Name, &
                              'Design Size Maximum Heating Air Flow Rate [m3/s]', MaxHeatVolFlowRateDes, &
                              'User-Specified Maximum Heating Air Flow Rate [m3/s]', MaxHeatVolFlowRateUser)
              IF (DisplayExtraWarnings) THEN
                IF ((ABS(MaxHeatVolFlowRateDes - MaxHeatVolFlowRateUser)/ MaxHeatVolFlowRateUser) > AutoVsHardSizingThreshold) THEN
                  CALL ShowMessage('SizePurchasedAir: Potential issue with equipment sizing for ' &
                                      //TRIM(PurchAir(PurchAirNum)%cObjectName)//' '// &
                                      TRIM(PurchAir(PurchAirNum)%Name))
                  CALL ShowContinueError('User-Specified Maximum Heating Air Flow Rate of '// &
                                      TRIM(RoundSigDigits(MaxHeatVolFlowRateUser,5))// ' [m3/s]')
                  CALL ShowContinueError('differs from Design Size Maximum Heating Air Flow Rate of ' // &
                                      TRIM(RoundSigDigits(MaxHeatVolFlowRateDes,5))// ' [m3/s]')
                  CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                  CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                END IF
              ENDIF
            END IF
          END IF
        END IF
      END IF

      IsAutosize = .FALSE.
      IF ((PurchAir(PurchAirNum)%MaxCoolVolFlowRate == AutoSize) .AND. &
          ((PurchAir(PurchAirNum)%CoolingLimit == LimitFlowRate) .OR. &
           (PurchAir(PurchAirNum)%CoolingLimit == LimitFlowRateAndCapacity) .OR. &
           (PurchAir(PurchAirNum)%OutdoorAir .AND. PurchAir(PurchAirNum)%EconomizerType .NE. NoEconomizer))) THEN
        IsAutosize = .TRUE.
      END IF

      IF (CurZoneEqNum > 0) THEN
        IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
          IF (PurchAir(PurchAirNum)%MaxCoolVolFlowRate > 0.0d0) THEN
            CALL ReportSizingOutput(PurchAir(PurchAirNum)%cObjectName, PurchAir(PurchAirNum)%Name, &
                        'User-Specified Maximum Cooling Air Flow Rate [m3/s]', PurchAir(PurchAirNum)%MaxCoolVolFlowRate)
          END IF
        ELSE
          CALL CheckZoneSizing(TRIM(PurchAir(PurchAirNum)%cObjectName), PurchAir(PurchAirNum)%Name)
          MaxCoolVolFlowRateDes = FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow
          IF (MaxCoolVolFlowRateDes < SmallAirVolFlow) THEN
            MaxCoolVolFlowRateDes = 0.0d0
          END IF
          IF (IsAutosize) THEN
            PurchAir(PurchAirNum)%MaxCoolVolFlowRate = MaxCoolVolFlowRateDes
            CALL ReportSizingOutput(PurchAir(PurchAirNum)%cObjectName, PurchAir(PurchAirNum)%Name, &
                              'Design Size Maximum Cooling Air Flow Rate [m3/s]', MaxCoolVolFlowRateDes)
          ELSE
            IF (PurchAir(PurchAirNum)%MaxCoolVolFlowRate > 0.0d0 .AND. MaxCoolVolFlowRateDes > 0.0d0) THEN
              MaxCoolVolFlowRateUser = PurchAir(PurchAirNum)%MaxCoolVolFlowRate
              CALL ReportSizingOutput(PurchAir(PurchAirNum)%cObjectName, PurchAir(PurchAirNum)%Name, &
                              'Design Size Maximum Cooling Air Flow Rate [m3/s]', MaxCoolVolFlowRateDes, &
                              'User-Specified Maximum Cooling Air Flow Rate [m3/s]', MaxCoolVolFlowRateUser)
              IF (DisplayExtraWarnings) THEN
              IF ((ABS(MaxCoolVolFlowRateDes - MaxCoolVolFlowRateUser)/MaxCoolVolFlowRateUser) > AutoVsHardSizingThreshold) THEN
                CALL ShowMessage('SizePurchasedAir: Potential issue with equipment sizing for ' &
                                    //TRIM(PurchAir(PurchAirNum)%cObjectName)//' '// &
                                    TRIM(PurchAir(PurchAirNum)%Name))
                CALL ShowContinueError('User-Specified Maximum Cooling Air Flow Rate of '// &
                                    TRIM(RoundSigDigits(MaxCoolVolFlowRateUser,5))// ' [m3/s]')
                CALL ShowContinueError('differs from Design Size Maximum Cooling Air Flow Rate of ' // &
                                    TRIM(RoundSigDigits(MaxCoolVolFlowRateDes,5))// ' [m3/s]')
                CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
              END IF
              ENDIF
            END IF
          END IF
        END IF
      END IF

      IsAutosize = .FALSE.
      IF ((PurchAir(PurchAirNum)%MaxHeatSensCap == AutoSize) .AND. &
          ((PurchAir(PurchAirNum)%HeatingLimit == LimitCapacity) .OR. &
           (PurchAir(PurchAirNum)%HeatingLimit == LimitFlowRateAndCapacity))) THEN
        IsAutosize = .TRUE.
      END IF

      IF (CurZoneEqNum > 0) THEN
        IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
          IF (PurchAir(PurchAirNum)%MaxHeatSensCap > 0.0d0) THEN
            CALL ReportSizingOutput(PurchAir(PurchAirNum)%cObjectName, PurchAir(PurchAirNum)%Name, &
                        'User-Specified Maximum Sensible Heating Capacity [W]', PurchAir(PurchAirNum)%MaxHeatSensCap)
          END IF
        ELSE
          CALL CheckZoneSizing(TRIM(PurchAir(PurchAirNum)%cObjectName), PurchAir(PurchAirNum)%Name)
          MixedAirTemp = FinalZoneSizing(CurZoneEqNum)%DesHeatCoilInTemp
          OutletTemp   = FinalZoneSizing(CurZoneEqNum)%HeatDesTemp
          OutletHumRat = FinalZoneSizing(CurZoneEqNum)%HeatDesHumRat
          DesignLoad   = PsyCpAirFnWTdb(OutletHumRat, 0.5d0*(MixedAirTemp+OutletTemp), 'SizePurchasedAir') &
                          * FinalZoneSizing(CurZoneEqNum)%DesHeatMassFlow &
                          * (OutletTemp-MixedAirTemp)
          MaxHeatSensCapDes = DesignLoad
          IF (MaxHeatSensCapDes < SmallLoad) THEN
            MaxHeatSensCapDes = 0.0d0
          END IF
          IF (IsAutosize) THEN
            PurchAir(PurchAirNum)%MaxHeatSensCap = MaxHeatSensCapDes
            CALL ReportSizingOutput(PurchAir(PurchAirNum)%cObjectName, PurchAir(PurchAirNum)%Name, &
                              'Design Size Maximum Sensible Heating Capacity [W]', MaxHeatSensCapDes)
            ! If there is OA, check if sizing calcs have OA>0, throw warning if not
            IF ((PurchAir(PurchAirNum)%OutdoorAir) .AND. (FinalZoneSizing(CurZoneEqNum)%MinOA == 0.0)) THEN
              CALL ShowWarningError('InitPurchasedAir: In '//TRIM(PurchAir(PurchAirNum)%cObjectName)//' = '// &
                    TRIM(PurchAir(PurchAirNum)%Name))
              CALL ShowContinueError('There is outdoor air specified in this object, '//  &
                 'but the design outdoor air flow rate for this ')
              CALL ShowContinueError('zone is zero. The Maximum Sensible Heating Capacity will be '//  &
                 'autosized for zero outdoor air flow. ')
              CALL ShowContinueError('Check the outdoor air specifications in the Sizing:Zone object for zone '// &
                                     TRIM(FinalZoneSizing(CurZoneEqNum)%ZoneName)//'.')
            END IF
          ELSE
            IF (PurchAir(PurchAirNum)%MaxHeatSensCap > 0.0d0 .AND. MaxHeatSensCapDes > 0.0d0) THEN
              MaxHeatSensCapUser = PurchAir(PurchAirNum)%MaxHeatSensCap
              CALL ReportSizingOutput(PurchAir(PurchAirNum)%cObjectName, PurchAir(PurchAirNum)%Name, &
                              'Design Size Maximum Sensible Heating Capacity [W]', MaxHeatSensCapDes, &
                              'User-Specified Maximum Sensible Heating Capacity [W]', MaxHeatSensCapUser)
              IF (DisplayExtraWarnings) THEN
                IF ((ABS(MaxHeatSensCapDes - MaxHeatSensCapUser)/MaxHeatSensCapUser) > AutoVsHardSizingThreshold) THEN
                  CALL ShowMessage('SizePurchasedAir: Potential issue with equipment sizing for ' &
                                      //TRIM(PurchAir(PurchAirNum)%cObjectName)//' '// &
                                      TRIM(PurchAir(PurchAirNum)%Name))
                  CALL ShowContinueError('...User-Specified Maximum Sensible Heating Capacity of '// &
                                      TRIM(RoundSigDigits(MaxHeatSensCapUser,2))// ' [W]')
                  CALL ShowContinueError('...differs from Design Size Maximum Sensible Heating Capacity of ' // &
                                      TRIM(RoundSigDigits(MaxHeatSensCapDes,2))// ' [W]')
                  CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                  CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                END IF
              ENDIF
            END IF
          END IF
        END IF
      END IF

      IsAutosize = .FALSE.
      IF ((PurchAir(PurchAirNum)%MaxCoolTotCap == AutoSize) .AND. &
          ((PurchAir(PurchAirNum)%CoolingLimit == LimitCapacity) .OR. &
           (PurchAir(PurchAirNum)%CoolingLimit == LimitFlowRateAndCapacity))) THEN
        IsAutosize = .TRUE.
      END IF

      IF (CurZoneEqNum > 0) THEN
        IF (.NOT. IsAutosize .AND. .NOT. ZoneSizingRunDone) THEN ! Simulation continue
          IF (PurchAir(PurchAirNum)%MaxCoolTotCap > 0.0d0) THEN
            CALL ReportSizingOutput(PurchAir(PurchAirNum)%cObjectName, PurchAir(PurchAirNum)%Name, &
                        'User-Specified Maximum Heating Air Flow Rate [m3/s]', PurchAir(PurchAirNum)%MaxCoolTotCap)
          END IF
        ELSE
          CALL CheckZoneSizing(TRIM(PurchAir(PurchAirNum)%cObjectName), PurchAir(PurchAirNum)%Name)
          MixedAirTemp = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInTemp
          OutletTemp = FinalZoneSizing(CurZoneEqNum)%CoolDesTemp
          OutletHumRat = FinalZoneSizing(CurZoneEqNum)%CoolDesHumRat
          MixedAirHumRat = FinalZoneSizing(CurZoneEqNum)%DesCoolCoilInHumRat
          DesignLoad = FinalZoneSizing(CurZoneEqNum)%DesCoolMassFlow &
                      * (PsyHFnTdbW(MixedAirTemp, MixedAirHumRat, 'SizePurchasedAir') &
                      -PsyHFnTdbW(OutletTemp, OutletHumRat, 'SizePurchasedAir'))
          MaxCoolTotCapDes = DesignLoad
          IF (MaxCoolTotCapDes < SmallLoad) THEN
            MaxCoolTotCapDes = 0.0d0
          END IF
          IF (IsAutosize) THEN
            PurchAir(PurchAirNum)%MaxCoolTotCap = MaxCoolTotCapDes
            CALL ReportSizingOutput(PurchAir(PurchAirNum)%cObjectName, PurchAir(PurchAirNum)%Name, &
                              'Design Size Maximum Total Cooling Capacity [W]', MaxCoolTotCapDes)
            ! If there is OA, check if sizing calcs have OA>0, throw warning if not
            IF ((PurchAir(PurchAirNum)%OutdoorAir) .AND. (FinalZoneSizing(CurZoneEqNum)%MinOA == 0.0)) THEN
              CALL ShowWarningError('SizePurchasedAir: In '//TRIM(PurchAir(PurchAirNum)%cObjectName)//' = '// &
                    TRIM(PurchAir(PurchAirNum)%Name))
              CALL ShowContinueError('There is outdoor air specified in this object, '//  &
                 'but the design outdoor air flow rate for this ')
              CALL ShowContinueError('zone is zero. The Maximum Total Cooling Capacity will be autosized '//  &
                 'for zero outdoor air flow. ')
              CALL ShowContinueError('Check the outdoor air specifications in the Sizing:Zone object for zone '// &
                                     TRIM(FinalZoneSizing(CurZoneEqNum)%ZoneName)//'.')
            END IF
          ELSE
            IF (PurchAir(PurchAirNum)%MaxCoolTotCap > 0.0d0 .AND. MaxCoolTotCapDes > 0.0d0) THEN
              MaxCoolTotCapUser = PurchAir(PurchAirNum)%MaxCoolTotCap
              CALL ReportSizingOutput(TRIM(PurchAir(PurchAirNum)%cObjectName), PurchAir(PurchAirNum)%Name, &
                              'Design Size Maximum Total Cooling Capacity [W]', MaxCoolTotCapDes, &
                              'User-Specified Maximum Total Cooling Capacity [W]', MaxCoolTotCapUser)
              IF (DisplayExtraWarnings) THEN
                IF ((ABS(MaxCoolTotCapDes - MaxCoolTotCapUser)/MaxCoolTotCapUser) > AutoVsHardSizingThreshold) THEN
                  CALL ShowMessage('SizePurchasedAir: Potential issue with equipment sizing for ' &
                                      //TRIM(PurchAir(PurchAirNum)%cObjectName)//' '// &
                                      TRIM(PurchAir(PurchAirNum)%Name))
                  CALL ShowContinueError('User-Specified Maximum Total Cooling Capacity of '// &
                                      TRIM(RoundSigDigits(MaxCoolTotCapUser,2))// ' [W]')
                  CALL ShowContinueError('differs from Design Size Maximum Total Cooling Capacity of ' // &
                                      TRIM(RoundSigDigits(MaxCoolTotCapDes,2))// ' [W]')
                  CALL ShowContinueError('This may, or may not, indicate mismatched component sizes.')
                  CALL ShowContinueError('Verify that the value entered is intended and is consistent with other components.')
                END IF
              ENDIF
            END IF
          END IF
        END IF
      END IF

!      IF (PurchAir(PurchAirNum)%OutdoorAir .AND. PurchAir(PurchAirNum)%OutsideAirVolFlowRate == AutoSize) THEN
!
!        IF (CurZoneEqNum > 0) THEN
!
!          CALL CheckZoneSizing(TRIM(PurchAir(PurchAirNum)%cObjectName), PurchAir(PurchAirNum)%Name)
!          PurchAir(PurchAirNum)%OutsideAirVolFlowRate = FinalZoneSizing(CurZoneEqNum)%MinOA
!          IF (PurchAir(PurchAirNum)%OutsideAirVolFlowRate < SmallAirVolFlow) THEN
!            PurchAir(PurchAirNum)%OutsideAirVolFlowRate = 0.0
!          END IF
!          CALL ReportSizingOutput(TRIM(PurchAir(PurchAirNum)%cObjectName), PurchAir(PurchAirNum)%Name, &
!                              'Outdoor Air Flow Rate [m3/s]', PurchAir(PurchAirNum)%OutsideAirVolFlowRate )
!        END IF
!
!      END IF

    RETURN

    END SUBROUTINE SizePurchasedAir

    SUBROUTINE CalcPurchAirLoads(PurchAirNum, SysOutputProvided, MoistOutputProvided, ControlledZoneNum, ActualZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   Nov 1997
          !       MODIFIED       Shirey, Aug 2009 (LatOutputProvided - now MoistOutputProvided)
          !                      M. Witte June 2011, add new features including DCV, economizer, dehumidification
          !                          and humidification,
          !                      July 2012, Chandan Sharma - FSEC: Added hybrid ventilation manager
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Needs description.

          ! METHODOLOGY EMPLOYED:
          ! Needs description, as appropriate.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
         USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand, ZoneSysMoistureDemand
         USE DataLoopNode, ONLY: Node
         USE DataZoneEquipment, ONLY: ZoneEquipConfig
         USE DataHVACGlobals, ONLY: SmallLoad, ZoneComp, ForceOff
         USE DataHeatBalance, ONLY: Zone
         USE DataHeatBalFanSys, ONLY:TempControlType
         USE General, ONLY: TrimSigDigits
         USE DataContaminantBalance, ONLY: Contaminant
         USE DataZoneEquipment, ONLY: PurchasedAir_Num

        IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
         REAL(r64), INTENT(INOUT) :: SysOutputProvided ! Sensible output provided [W] cooling = negative
         REAL(r64), INTENT(OUT)   :: MoistOutputProvided ! Moisture output provided [kg/s] dehumidification = negative
         INTEGER, INTENT(IN) :: ActualZoneNum
         INTEGER, INTENT(IN) :: ControlledZoneNum
         INTEGER, INTENT(IN) :: PurchAirNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
         INTEGER   :: InNodeNum         ! Ideal loads supply node to zone
!         INTEGER   :: ExhNodeNum        ! Ideal loads exhaust node from zone
         INTEGER   :: ZoneNodeNum       ! Zone air node
         INTEGER   :: OANodeNum         ! Outdoor air inlet node
         INTEGER   :: RecircNodeNum     ! Return air or zone exhaust node
         INTEGER   :: OperatingMode     ! current operating mode, Off, Heat, Cool, or Deadband
         REAL(r64) :: SupplyMassFlowRate  ! System supply air mass flow rate [kg/s]
         REAL(r64) :: SupplyMassFlowRateForHumid  ! System supply air mass flow rate required to meet humdification load [kg/s]
         REAL(r64) :: SupplyMassFlowRateForDehum  ! System supply air mass flow rate required to meet dehumidification load [kg/s]
         REAL(r64) :: SupplyMassFlowRateForCool   ! System supply air mass flow rate required to meet sensible cooling load[kg/s]
         REAL(r64) :: SupplyMassFlowRateForHeat   ! System supply air mass flow rate required to meet sensible heating load[kg/s]
         REAL(r64) :: SupplyHumRatForHumid ! Supply air humidity ratio require to meet the humidification load [kgH2O/kgAir]
         REAL(r64) :: SupplyHumRatForDehum ! Supply air humidity ratio require to meet the dehumidification load [kgH2O/kgAir]
         REAL(r64) :: OAMassFlowRate    ! Outdoor air mass flow rate [kg/s]
         REAL(r64) :: OAVolFlowRate     ! Outdoor air volume flow rate at standard density [m3/s]
         REAL(r64) :: MinOASensOutput   ! Minimum Outdoor air sensible output [W], <0 means OA is cooler than zone air
         REAL(r64) :: MinOALatOutput    ! Minimum Outdoor air moisture load [kg/s]
         REAL(r64) :: SensOutput        ! Sensible output [W] (psitive means heating, negative means cooling)
         REAL(r64) :: HeatSensOutput    ! Heating sensible output [W]
         REAL(r64) :: CoolSensOutput    ! Cooling sensible output [W] (positive value menas cooling)
         REAL(r64) :: LatOutput         ! Latent output [W] (positive value means hudmification, negative means dehumidification)
         REAL(r64) :: CoolLatOutput     ! Cooling latent output [W] (positive value means dehumidification)
         REAL(r64) :: CoolTotOutput     ! Cooling total output [W] (positive value means cooling)
         REAL(r64) :: DeltaT            ! Delta temperature - reused in multiple places
         REAL(r64) :: DeltaHumRat       ! Delta humidity ratio - reused in multiple places
         REAL(r64) :: QZnHeatSP         ! Load required to meet heating setpoint [W] (>0 is a heating load)
         REAL(r64) :: QZnCoolSP         ! Load required to meet cooling setpoint [W] (<0 is a cooling load)
         REAL(r64) :: MdotZnHumidSP     ! Load required to meet humidifying setpoint [kg H2O/s] (>0 = a humidify load)
         REAL(r64) :: MdotZnDehumidSP   ! Load required to meet dehumidifying setpoint [kg H2O/s] (<0 = a dehumidify load)
         LOGICAL   :: UnitOn
         LOGICAL   :: HeatOn            ! Flag for heating and humidification availbility schedule, true if heating is on
         LOGICAL   :: CoolOn            ! Flag for cooling and dehumidification availbility schedule, true if cooling is on
         LOGICAL   :: EconoOn           ! Flag for economizer operation, true if economizer is on
         REAL(r64) :: SupplyTemp        ! Supply inlet to zone dry bulb temperature [C]
         REAL(r64) :: SupplyHumRat      ! Supply inlet to zone humidity ratio [kg H2O/kg Air]
         REAL(r64) :: SupplyHumRatOrig  ! Supply inlet to zone humidity ratio before saturation check [kg H2O/kg Air]
         REAL(r64) :: SupplyHumRatSat   ! Supply inlet to zone humidity ratio saturation at SupplyTemp [kg H2O/kg Air]
         REAL(r64) :: SupplyEnthalpy    ! Supply inlet to zone enthalpy [J/kg]
         REAL(r64) :: MixedAirTemp      ! Mixed air dry bulb temperature [C]
         REAL(r64) :: MixedAirHumRat    ! Mixed air humidity ratio [kg H2O/kg Air]
         REAL(r64) :: MixedAirEnthalpy  ! Mixed air enthalpy [J/kg]
         REAL(r64) :: CpAir        ! Specific heat [J/kg-C] reused in multiple places
!         REAL(r64) :: SpecHumOut   ! Specific humidity ratio of outlet air (kg moisture / kg moist air)
!         REAL(r64) :: SpecHumIn    ! Specific humidity ratio of inlet [zone] air (kg moisture / kg moist air)

           ! Sign convention: SysOutputProvided <0 Supply air is heated on entering zone (zone is cooled)
           !                  SysOutputProvided >0 Supply air is cooled on entering zone (zone is heated)
         InNodeNum = PurchAir(PurchAirNum)%ZoneSupplyAirNodeNum
         ZoneNodeNum = ZoneEquipConfig(ControlledZoneNum)%ZoneNode
         OANodeNum = PurchAir(PurchAirNum)%OutdoorAirNodeNum
         RecircNodeNum = PurchAir(PurchAirNum)%ZoneRecircAirNodeNum
         SupplyMassFlowRate = 0.0d0
         OAMassFlowRate = 0.0d0
         PurchAir(PurchAirNum)%MinOAMassFlowRate = 0.0d0
         PurchAir(PurchAirNum)%TimeEconoActive = 0.0d0
         PurchAir(PurchAirNum)%TimeHtRecActive = 0.0d0
         SysOutputProvided = 0.0d0
         MoistOutputProvided = 0.0d0
         CoolSensOutput = 0.0d0
         CoolLatOutput = 0.0d0
         CoolTotOutput = 0.0d0
         HeatSensOutput = 0.0d0
         LatOutput = 0.0d0


            ! default unit to ON
         UnitOn = .TRUE.
         EconoOn = .FALSE.
            ! get current zone requirements
         QZnHeatSP       = ZoneSysEnergyDemand(ActualZoneNum)%RemainingOutputReqToHeatSP
         QZnCoolSP       = ZoneSysEnergyDemand(ActualZoneNum)%RemainingOutputReqToCoolSP

         IF (ALLOCATED(ZoneComp)) THEN
           ZoneComp(PurchasedAir_Num)%ZoneCompAvailMgrs(PurchAirNum)%ZoneNum = ActualZoneNum
           PurchAir(PurchAirNum)%AvailStatus = ZoneComp(PurchasedAir_Num)%ZoneCompAvailMgrs(PurchAirNum)%AvailStatus
            ! Check if the hybrid ventilation availability manager is turning the unit off
           IF (PurchAir(PurchAirNum)%AvailStatus .EQ. ForceOFf) THEN
             UnitOn = .FALSE.
           END IF
         ENDIF

            ! Check if the unit is scheduled off
!         IF (PurchAir(PurchAirNum)%AvailSchedPtr > 0) THEN
             IF (GetCurrentScheduleValue(PurchAir(PurchAirNum)%AvailSchedPtr) <= 0) THEN
                 UnitOn = .FALSE.
             END IF
!         END IF
            ! Check if heating and cooling available
         HeatOn = .TRUE.
!         IF (PurchAir(PurchAirNum)%HeatSchedPtr > 0) THEN
             IF (GetCurrentScheduleValue(PurchAir(PurchAirNum)%HeatSchedPtr) <= 0) THEN
                 HeatOn = .FALSE.
             END IF
!         END IF
         CoolOn = .TRUE.
!         IF (PurchAir(PurchAirNum)%CoolSchedPtr > 0) THEN
             IF (GetCurrentScheduleValue(PurchAir(PurchAirNum)%CoolSchedPtr) <= 0) THEN
                 CoolOn = .FALSE.
             END IF
!         END IF

         IF (UnitON) THEN
             ! Calculate current minimum outdoor air flow rate based on design OA specifications and DCV or CO2 control
             CALL CalcPurchAirMinOAMassFlow(PurchAirNum, ActualZoneNum, OAMassFlowRate)

             ! EMS override point  Purch air outdoor air massflow rate.....
             IF (PurchAir(PurchAirNum)%EMSOverrideOAMdotOn) THEN
               OAMassFlowRate = PurchAir(PurchAirNum)%EMSValueOAMassFlowRate
             ENDIF

             ! Calculate minimum outdoor air sensible and latent load
             IF (PurchAir(PurchAirNum)%OutdoorAir) THEN
               CpAir = PsyCpAirFnWTdb(Node(OANodeNum)%HumRat,Node(OANodeNum)%Temp, 'CalcPurchAirLoads')
               MinOASensOutput = OAMassFlowRate * CpAir * (Node(OANodeNum)%Temp - Node(ZoneNodeNum)%Temp)
               MinOALatOutput  = OAMassFlowRate * (Node(OANodeNum)%HumRat - Node(ZoneNodeNum)%HumRat)
             ELSE
               MinOASensOutput = 0.0d0
               MinOALatOutput  = 0.0d0
             ENDIF
             SupplyMassFlowRate = OAMassFlowRate

             ! Check if cooling of the supply air stream is required

                                     ! Cooling operation
           IF ((MinOASensOutput >= QZnCoolSP) .AND. (TempControlType(ActualZoneNum) .NE. SingleHeatingSetPoint)) THEN
             OperatingMode = Cool
             ! Calculate supply mass flow, temp and humidity with the following constraints:
             !  Min cooling supply temp
             !  Max total cooling capacity
             !  Max cooling airflow
             !  Min cooling supply humrat  (and Max heating supply humrat)
             !  Min OA mass flow rate

             ! Check if OA flow rate greater than max cooling airflow limit
             IF (((PurchAir(PurchAirNum)%CoolingLimit == LimitFlowRate) .OR. &
                  (PurchAir(PurchAirNum)%CoolingLimit == LimitFlowRateAndCapacity)) &
                  .AND. (OAMassFlowRate .GT. PurchAir(PurchAirNum)%MaxCoolMassFlowRate)) THEN
               OAVolFlowRate = OAMassFlowRate / StdRhoAir
               IF (PurchAir(PurchAirNum)%OAFlowMaxCoolOutputError < 1) THEN
                 PurchAir(PurchAirNum)%OAFlowMaxCoolOutputError = PurchAir(PurchAirNum)%OAFlowMaxCoolOutputError + 1
                 Call ShowWarningError(TRIM(PurchAir(PurchAirNum)%cObjectName)//' "'//TRIM(PurchAir(PurchAirNum)%Name)//'"'&
                                       //' Requested outdoor air flow rate = '//TRIM(TrimSigDigits(OAVolFlowRate,5)) &
                                       //' [m3/s] exceeds limit.')
                 CALL ShowContinueError(' Will be reduced to the Maximum Cooling Air Flow Rate = ' &
                                        //TRIM(TrimSigDigits(PurchAir(PurchAirNum)%MaxCoolVolFlowRate,5))//' [m3/s]')
                 CALL ShowContinueErrorTimeStamp(' ')
               ELSE
                 CALL ShowRecurringWarningErrorAtEnd(TRIM(PurchAir(PurchAirNum)%cObjectName)//' "'&
                       //TRIM(PurchAir(PurchAirNum)%Name)//'"'//&
                       ' Requested outdoor air flow rate [m3/s] reduced to Maximum Cooling Air Flow Rate warning continues...' &
                       , PurchAir(PurchAirNum)%OAFlowMaxCoolOutputIndex, OAVolFlowRate)
               END IF
               OAMassFlowRate = PurchAir(PurchAirNum)%MaxCoolMassFlowRate

             ELSE
               ! Model economizer
               IF (PurchAir(PurchAirNum)%EconomizerType /= NoEconomizer) THEN
                 IF (((PurchAir(PurchAirNum)%EconomizerType == DifferentialDryBulb) &
                       .AND. (Node(OANodeNum)%Temp < Node(PurchAir(PurchAirNum)%ZoneRecircAirNodeNum)%Temp)) .OR. &
                     ((PurchAir(PurchAirNum)%EconomizerType == DifferentialEnthalpy) &
                       .AND. (Node(OANodeNum)%Enthalpy < Node(PurchAir(PurchAirNum)%ZoneRecircAirNodeNum)%Enthalpy))) THEN

                    ! Calculate supply MassFlowRate based on sensible load but limit to Max Cooling Supply Air Flow Rate if specified
                   CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ActualZoneNum),Node(ZoneNodeNum)%Temp, 'CalcPurchAirLoads')
                   DeltaT = (Node(OANodeNum)%Temp - Node(ZoneNodeNum)%Temp)
                   IF (DeltaT < -SmallTempDiff) THEN
                     SupplyMassFlowRate = QZnCoolSP/CPAir/DeltaT
                     IF (((PurchAir(PurchAirNum)%CoolingLimit == LimitFlowRate) .OR. &
                          (PurchAir(PurchAirNum)%CoolingLimit == LimitFlowRateAndCapacity)) &
                          .AND. (PurchAir(PurchAirNum)%MaxCoolMassFlowRate .GT. 0.0d0)) THEN
                       SupplyMassFlowRate = MIN(MAX(SupplyMassFlowRate,0.0d0),PurchAir(PurchAirNum)%MaxCoolMassFlowRate)
                     END IF
                     IF (SupplyMassFlowRate > OAMassFlowRate) THEN
                       EconoOn = .TRUE.
                       OAMassFlowRate = SupplyMassFlowRate
                       PurchAir(PurchAirNum)%TimeEconoActive = TimeStepSys
                     END IF
                   END IF
                 END IF
               END IF
             END IF

             ! Determine supply mass flow rate
                 ! Mass flow rate to meet sensible load, at Minimum Cooling Supply Air Temperature
             SupplyMassFlowRateForCool = 0.0d0
             IF (CoolOn) THEN
               CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ActualZoneNum),Node(ZoneNodeNum)%Temp, 'CalcPurchAirLoads')
               DeltaT = (PurchAir(PurchAirNum)%MinCoolSuppAirTemp - Node(ZoneNodeNum)%Temp)
               IF (DeltaT < -SmallTempDiff) THEN
                 SupplyMassFlowRateForCool = QZnCoolSP/CPAir/DeltaT
               ENDIF
             ENDIF

                 ! Mass flow rate to meet dehumidification load, if applicable, at Minimum Cooling Supply Humidity Ratio
             SupplyMassFlowRateForDehum = 0.0d0
             IF (CoolOn) THEN
               IF (PurchAir(PurchAirNum)%DehumidCtrlType .EQ. Humidistat) THEN
                 MdotZnDehumidSP= ZoneSysMoistureDemand(ActualZoneNum)%RemainingOutputReqToDehumidSP
                 DeltaHumRat = (PurchAir(PurchAirNum)%MinCoolSuppAirHumRat - Node(ZoneNodeNum)%HumRat)
                 IF ((DeltaHumRat < -SmallDeltaHumRat) .AND. (MdotZnDehumidSP .LT. 0.0d0)) THEN
                   SupplyMassFlowRateForDehum = MdotZnDehumidSP/DeltaHumRat
                 ENDIF
               ENDIF
             ENDIF

                 ! Mass flow rate to meet humidification load, if applicable, at Maximum Heating Supply Humidity Ratio
                 ! This section is the cooling section, so humidification should activate only if humidification control = humidistat
                 !   and if dehumidification control = humidistat or none
             SupplyMassFlowRateForHumid = 0.0d0
             IF (HeatOn) THEN
               IF (PurchAir(PurchAirNum)%HumidCtrlType .EQ. Humidistat) THEN
                 IF ((PurchAir(PurchAirNum)%DehumidCtrlType .EQ. Humidistat) .OR. &
                     (PurchAir(PurchAirNum)%DehumidCtrlType .EQ. None)) THEN
                   MdotZnHumidSP = ZoneSysMoistureDemand(ActualZoneNum)%RemainingOutputReqToHumidSP
                   DeltaHumRat = (PurchAir(PurchAirNum)%MaxHeatSuppAirHumRat - Node(ZoneNodeNum)%HumRat)
                   IF ((DeltaHumRat > SmallDeltaHumRat) .AND. (MdotZnHumidSP .GT. 0.0d0)) THEN
                     SupplyMassFlowRateForHumid = MdotZnHumidSP/DeltaHumRat
                   ENDIF
                 ENDIF
               ENDIF
             ENDIF

                 ! Supply mass flow is greatest of these, but limit to cooling max flow rate, if applicable
             SupplyMassFlowRate = MAX(0.0d0, OAMassFlowRate, SupplyMassFlowRateForCool, &
                                     SupplyMassFlowRateForDehum, SupplyMassFlowRateForHumid)
             ! EMS override point  Purch air massflow rate..... but only if unit is on, i.e. SupplyMassFlowRate>0.0
             IF ((PurchAir(PurchAirNum)%EMSOverrideMdotOn) .AND. (SupplyMassFlowRate > 0.0d0)) THEN
               SupplyMassFlowRate = PurchAir(PurchAirNum)%EMSValueMassFlowRate
               OAMassFlowRate = MIN(OAMassFlowRate,SupplyMassFlowRate)
             ENDIF
             IF (((PurchAir(PurchAirNum)%CoolingLimit == LimitFlowRate) .OR. &
                  (PurchAir(PurchAirNum)%CoolingLimit == LimitFlowRateAndCapacity)) &
                  .AND. (PurchAir(PurchAirNum)%MaxCoolMassFlowRate .GT. 0.0d0)) THEN
               SupplyMassFlowRate = MIN(SupplyMassFlowRate,PurchAir(PurchAirNum)%MaxCoolMassFlowRate)
             END IF

             IF (SupplyMassFlowRate <= VerySmallMassFlow) SupplyMassFlowRate = 0.0d0

             ! Calculate mixed air conditions
             CALL CalcPurchAirMixedAir(PurchAirNum, OAMassFlowRate, SupplyMassFlowRate, &
                                         MixedAirTemp, MixedAirHumRat, MixedAirEnthalpy, OperatingMode)

             ! Calculate supply air conditions using final massflow rate, imposing capacity limits if specified
             ! If capacity limits are exceeded, keep massflow rate where it is and adjust supply temp
             ! In general, in the cooling section, don't let SupplyTemp be set to something that results in heating
             IF (SupplyMassFlowRate .GT. 0.0d0) THEN
               ! Calculate supply temp at SupplyMassFlowRate and recheck limit on Minimum Cooling Supply Air Temperature
               CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ActualZoneNum),Node(ZoneNodeNum)%Temp, 'CalcPurchAirLoads')
               SupplyTemp = QZnCoolSP/(CPAir*SupplyMassFlowRate) + Node(ZoneNodeNum)%Temp
               SupplyTemp = MAX(SupplyTemp,PurchAir(PurchAirNum)%MinCoolSuppAirTemp)
               ! This is the cooling mode, so SupplyTemp can't be more than MixedAirTemp
               SupplyTemp = MIN(SupplyTemp,MixedAirTemp)
               SupplyHumRat = MixedAirHumRat
               SupplyEnthalpy = PsyHFnTdbW(SupplyTemp,SupplyHumRat, 'CalcPurchAirLoads')

               ! Check sensible load vs max total cooling capacity, if specified, and adjust supply temp before applying humidity controls
               ! Will check again later, too
               IF ((PurchAir(PurchAirNum)%CoolingLimit == LimitCapacity) .OR. &
                   (PurchAir(PurchAirNum)%CoolingLimit == LimitFlowRateAndCapacity)) THEN
                 CpAir = PsyCpAirFnWTdb(MixedAirHumRat,MixedAirTemp, 'CalcPurchAirLoads')
                 CoolSensOutput = SupplyMassFlowRate * (MixedAirEnthalpy - SupplyEnthalpy)
                 IF (CoolSensOutput >= PurchAir(PurchAirNum)%MaxCoolTotCap) THEN
                   CoolSensOutput = PurchAir(PurchAirNum)%MaxCoolTotCap
                   SupplyEnthalpy = MixedAirEnthalpy - CoolSensOutput/SupplyMassFlowRate
                   SupplyTemp = PsyTdbFnHW(SupplyEnthalpy,SupplyHumRat, 'CalcPurchAirLoads')
                   ! This is the cooling mode, so SupplyTemp can't be more than MixedAirTemp
                   SupplyTemp = MIN(SupplyTemp,MixedAirTemp)
                 END IF ! Capacity limit exceeded
               END IF

               ! Set supply humidity ratio for cooling/dehumidification
               SupplyHumRat = MixedAirHumRat
               SELECT CASE(PurchAir(PurchAirNum)%DehumidCtrlType)
                 CASE(None)
                   SupplyHumRat = MixedAirHumRat
                 CASE(ConstantSensibleHeatRatio)
                   ! SHR = CoolSensOutput/CoolTotOutput
                   ! CoolTotOutput = CoolSensOutput/SHR
                   CpAir = PsyCpAirFnWTdb(MixedAirHumRat,MixedAirTemp, 'CalcPurchAirLoads')
                   CoolSensOutput = SupplyMassFlowRate * CpAir * (MixedAirTemp - SupplyTemp)
                   CoolTotOutput = CoolSensOutput/PurchAir(PurchAirNum)%CoolSHR
                   SupplyEnthalpy = MixedAirEnthalpy - CoolTotOutput/SupplyMassFlowRate
                   !  Limit for overdrying (avoid Pysch errors which occur if SupplyEnthalpy is too low for SupplyTemp)
                   SupplyEnthalpy = MAX(SupplyEnthalpy,PsyHFnTdbW(SupplyTemp,0.00001D0, 'CalcPurchAirLoads'))
                   SupplyHumRat = MIN(SupplyHumRat,PsyWFnTdbH(SupplyTemp,SupplyEnthalpy, 'CalcPurchAirLoads'))
                   ! Apply min cooling humidity ratio limit
                   SupplyHumRat = MAX(SupplyHumRat, PurchAir(PurchAirNum)%MinCoolSuppAirHumRat)
                   ! But don't let it be higher than incoming MixedAirHumRat
                   SupplyHumRat = MIN(SupplyHumRat,MixedAirHumRat)
                 CASE(Humidistat)
                   MdotZnDehumidSP= ZoneSysMoistureDemand(ActualZoneNum)%RemainingOutputReqToDehumidSP
                   SupplyHumRatForDehum = MdotZnDehumidSP/SupplyMassFlowRate + Node(ZoneNodeNum)%HumRat
                   SupplyHumRatForDehum = MIN(SupplyHumRatForDehum,PurchAir(PurchAirNum)%MinCoolSuppAirHumRat)
                   SupplyHumRat = MIN(MixedAirHumRat,SupplyHumRatForDehum)
                 CASE(ConstantSupplyHumidityRatio)
                   SupplyHumRat = PurchAir(PurchAirNum)%MinCoolSuppAirHumRat
                 CASE DEFAULT
                   SupplyHumRat = MixedAirHumRat
               END SELECT

                   ! Check supply humidity ratio for humidification (SupplyHumRatForHum should always be < SupplyHumRatForDehum)
                   ! This section is the cooling section, so humidification should activate only if humidification control = humidistat
                   !   and if dehumidification control = humidistat or none
               IF (HeatOn) THEN
                 IF (PurchAir(PurchAirNum)%HumidCtrlType .EQ. Humidistat) THEN
                   IF ((PurchAir(PurchAirNum)%DehumidCtrlType .EQ. Humidistat) .OR. &
                       (PurchAir(PurchAirNum)%DehumidCtrlType .EQ. None)) THEN
                     MdotZnHumidSP = ZoneSysMoistureDemand(ActualZoneNum)%RemainingOutputReqToHumidSP
                     SupplyHumRatForHumid = MdotZnHumidSP/SupplyMassFlowRate + Node(ZoneNodeNum)%HumRat
                     SupplyHumRatForHumid = MIN(SupplyHumRatForHumid,PurchAir(PurchAirNum)%MaxHeatSuppAirHumRat)
                     SupplyHumRat = MAX(SupplyHumRat,SupplyHumRatForHumid)
                   END IF
                 END IF
               END IF

               !   Limit supply humidity ratio to saturation at supply outlet temp
               SupplyHumRatOrig = SupplyHumRat
               SupplyHumRatSat  = PsyWFnTdbRhPb(SupplyTemp,1.0d0,OutBaroPress, 'CalcPurchAirLoads')
               SupplyHumRat = MIN(SupplyHumRatOrig,SupplyHumRatSat)
               SupplyEnthalpy = PsyHFnTdbW(SupplyTemp,SupplyHumRat, 'CalcPurchAirLoads')

               ! Check max total Cooling capacity, if specified
               IF ((PurchAir(PurchAirNum)%CoolingLimit == LimitCapacity) .OR. &
                   (PurchAir(PurchAirNum)%CoolingLimit == LimitFlowRateAndCapacity)) THEN
                 ! If dehumidifying, compare total cooling to the limit
                 IF (SupplyHumRat < MixedAirHumRat) THEN ! Dehumidifying
                   CoolTotOutput = SupplyMassFlowRate * (MixedAirEnthalpy - SupplyEnthalpy)
                   IF ((CoolTotOutput) > PurchAir(PurchAirNum)%MaxCoolTotCap) THEN
                     CoolTotOutput = PurchAir(PurchAirNum)%MaxCoolTotCap
                     SupplyEnthalpy = MixedAirEnthalpy - CoolTotOutput/SupplyMassFlowRate
                     ! Adjust output based on dehumidification control type
                     SELECT CASE(PurchAir(PurchAirNum)%DehumidCtrlType)
                       CASE(ConstantSensibleHeatRatio)
                         ! Adjust both supply temp and humidity ratio to maintain SHR
                         ! SHR = CoolSensOutput/CoolTotOutput
                         ! CoolSensOutput = SHR*CoolTotOutput
                         CpAir = PsyCpAirFnWTdb(MixedAirHumRat,MixedAirTemp, 'CalcPurchAirLoads')
                         CoolSensOutput = CoolTotOutput * PurchAir(PurchAirNum)%CoolSHR
                         SupplyTemp = MixedAirTemp - CoolSensOutput/(CPAir*SupplyMassFlowRate)
                         ! This is the cooling mode, so SupplyTemp can't be more than MixedAirTemp
                         SupplyTemp = MIN(SupplyTemp,MixedAirTemp)
                         !  Limit for overdrying (avoid Pysch errors which occur if SupplyEnthalpy is too low for SupplyTemp)
                         SupplyEnthalpy = MAX(SupplyEnthalpy,PsyHFnTdbW(SupplyTemp,0.00001D0, 'CalcPurchAirLoads'))
                         SupplyHumRat = PsyWFnTdbH(SupplyTemp,SupplyEnthalpy, 'CalcPurchAirLoads')
                       CASE(Humidistat)
                         ! Keep supply temp and adjust humidity ratio to reduce load
                         SupplyHumRat = PsyWFnTdbH(SupplyTemp,SupplyEnthalpy, 'CalcPurchAirLoads')
                       CASE(None, ConstantSupplyHumidityRatio)
                         ! Keep humidity ratio and adjust supply temp
                         ! Check if latent output exceeds capacity
                         CpAir = PsyCpAirFnWTdb(MixedAirHumRat,MixedAirTemp, 'CalcPurchAirLoads')
                         CoolSensOutput = SupplyMassFlowRate * CpAir * (MixedAirTemp - SupplyTemp)
                         CoolLatOutput = CoolTotOutput - CoolSensOutput
                         IF (CoolLatOutput >= PurchAir(PurchAirNum)%MaxCoolTotCap) THEN
                           SupplyTemp   = MixedAirTemp
                           SupplyHumRat = PsyWFnTdbH(SupplyTemp,SupplyEnthalpy, 'CalcPurchAirLoads')
                           CoolLatOutput = PurchAir(PurchAirNum)%MaxCoolTotCap
                         ELSE
                           SupplyTemp = PsyTdbFnHW(SupplyEnthalpy,SupplyHumRat, 'CalcPurchAirLoads')
                           ! This is the cooling mode, so SupplyTemp can't be more than MixedAirTemp
                           SupplyTemp = MIN(SupplyTemp,MixedAirTemp)
                         END IF
                     END SELECT
                     ! Limit supply humidity ratio to saturation at supply outlet temp
                     ! If saturation exceeded, then honor capacity limit and set to dew point at supplyenthalpy
                     SupplyHumRatOrig = SupplyHumRat
                     SupplyHumRatSat  = PsyWFnTdbRhPb(SupplyTemp,1.0d0,OutBaroPress, 'CalcPurchAirLoads')
                     IF (SupplyHumRatSat < SupplyHumRatOrig) THEN
                       SupplyTemp = PsyTsatFnHPb(SupplyEnthalpy,OutBaroPress,'CalcPurchAirLoads')
                       ! This is the cooling mode, so SupplyTemp can't be more than MixedAirTemp
                       SupplyTemp = MIN(SupplyTemp,MixedAirTemp)
                       SupplyHumRat = PsyWFnTdbH(SupplyTemp,SupplyEnthalpy,'CalcPurchAirLoads')
                       SupplyEnthalpy = PsyHFnTdbW(SupplyTemp,SupplyHumRat, 'CalcPurchAirLoads')
                       ! CpAir = PsyCpAirFnWTdb(MixedAirHumRat,MixedAirTemp, 'CalcPurchAirLoads')
                       ! CoolSensOutput = SupplyMassFlowRate * CpAir * (MixedAirTemp - SupplyTemp)
                       ! CoolTotOutput = SupplyMassFlowRate * (MixedAirEnthalpy - SupplyEnthalpy)
                     END IF
                   END IF ! Capacity limit exceeded
                 ELSE  ! Not dehumidifying
                   ! If not dehumidifying, compare sensible cooling to the limit
                   ! This section will only increase supply temp, so no need to recheck for super-saturation
                   CpAir = PsyCpAirFnWTdb(MixedAirHumRat,MixedAirTemp, 'CalcPurchAirLoads')
                   CoolSensOutput = SupplyMassFlowRate * CpAir * (MixedAirTemp - SupplyTemp)
                   IF (CoolSensOutput >= PurchAir(PurchAirNum)%MaxCoolTotCap) THEN
                     CoolSensOutput = PurchAir(PurchAirNum)%MaxCoolTotCap
                     SupplyTemp = MixedAirTemp - CoolSensOutput/(SupplyMassFlowRate * CpAir)
                   END IF ! Capacity limit exceeded
                 END IF  ! Dehumidifying or not
               END IF  ! Capacity limit active

             ELSE ! SupplyMassFlowRate is zero
               SupplyEnthalpy = MixedAirEnthalpy
               SupplyHumRat   = MixedAirHumRat
               SupplyTemp     = MixedAirTemp
               CoolSensOutput = 0.0d0
               CoolTotOutput  = 0.0d0
             END IF
                                     ! Heating or no-load operation
           ELSE  ! Heating or no-load case
             IF ((MinOASensOutput .LT. QZnHeatSP) .AND. (TempControlType(ActualZoneNum) .NE. SingleCoolingSetPoint)) THEN
               OperatingMode = Heat
             ELSE ! Deadband mode shuts off heat recovery and economizer
               OperatingMode = Deadband
             END IF
             ! Calculate supply mass flow, temp and humidity with the following constraints:
             !  Max heating supply temp
             !  Max sensible heating capacity
             !  Max heating airflow
             !  Max heating supply humrat (and Min cooling supply humrat)
             !  Min OA mass flow rate

             ! Check if OA flow rate greater than max heating airflow limit
             IF (((PurchAir(PurchAirNum)%HeatingLimit == LimitFlowRate) .OR. &
                  (PurchAir(PurchAirNum)%HeatingLimit == LimitFlowRateAndCapacity)) &
                  .AND. (OAMassFlowRate .GT. PurchAir(PurchAirNum)%MaxHeatMassFlowRate)) THEN
               OAVolFlowRate = OAMassFlowRate / StdRhoAir
               IF (PurchAir(PurchAirNum)%OAFlowMaxHeatOutputError < 1) THEN
                 PurchAir(PurchAirNum)%OAFlowMaxHeatOutputError = PurchAir(PurchAirNum)%OAFlowMaxHeatOutputError + 1
                 Call ShowWarningError(TRIM(PurchAir(PurchAirNum)%cObjectName)//' "'//TRIM(PurchAir(PurchAirNum)%Name)//'"'&
                                       //' Requested outdoor air flow rate = '//TRIM(TrimSigDigits(OAVolFlowRate,5)) &
                                       //' [m3/s] exceeds limit.')
                 CALL ShowContinueError(' Will be reduced to the Maximum Heating Air Flow Rate = ' &
                                        //TRIM(TrimSigDigits(PurchAir(PurchAirNum)%MaxHeatVolFlowRate,5))//' [m3/s]')
                 CALL ShowContinueErrorTimeStamp(' ')
               ELSE
                 CALL ShowRecurringWarningErrorAtEnd(TRIM(PurchAir(PurchAirNum)%cObjectName)//' "'&
                       //TRIM(PurchAir(PurchAirNum)%Name)//'"'//&
                       ' Requested outdoor air flow rate [m3/s] reduced to Maximum Heating Air Flow Rate warning continues...' &
                       , PurchAir(PurchAirNum)%OAFlowMaxHeatOutputIndex, OAVolFlowRate)
               END IF
               OAMassFlowRate = PurchAir(PurchAirNum)%MaxHeatMassFlowRate
             ENDIF

             SupplyMassFlowRate = OAMassFlowRate

             ! Determine supply mass flow rate
                 ! Mass flow rate to meet sensible load, at Minimum Cooling Supply Air Temperature
             SupplyMassFlowRateForHeat = 0.0d0
             IF ((HeatOn) .AND. (OperatingMode == Heat)) THEN
               CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ActualZoneNum),Node(ZoneNodeNum)%Temp, 'CalcPurchAirLoads')
               DeltaT = (PurchAir(PurchAirNum)%MaxHeatSuppAirTemp - Node(ZoneNodeNum)%Temp)
               IF (DeltaT > SmallTempDiff) THEN
                 SupplyMassFlowRateForHeat = QZnHeatSP/CPAir/DeltaT
               ENDIF
             ENDIF

                 ! Mass flow rate to meet dehumidification load, if applicable, at Minimum Cooling Supply Humidity Ratio
                 ! This section is the heating/deadband section, so dehumidification should activate
                 !   only if dehumidification control = humidistat
                 !   and if humidification control = humidistat or none or if operating in deadband mode
             SupplyMassFlowRateForDehum = 0.0d0
             IF (CoolOn) THEN
               IF (PurchAir(PurchAirNum)%DehumidCtrlType .EQ. Humidistat) THEN
                 IF ((PurchAir(PurchAirNum)%HumidCtrlType .EQ. Humidistat) .OR. &
                     (PurchAir(PurchAirNum)%HumidCtrlType .EQ. None) .OR. &
                     (OperatingMode == Deadband)) THEN
                   MdotZnDehumidSP= ZoneSysMoistureDemand(ActualZoneNum)%RemainingOutputReqToDehumidSP
                   DeltaHumRat = (PurchAir(PurchAirNum)%MinCoolSuppAirHumRat - Node(ZoneNodeNum)%HumRat)
                   IF ((DeltaHumRat < -SmallDeltaHumRat) .AND. (MdotZnDehumidSP .LT. 0.0d0)) THEN
                     SupplyMassFlowRateForDehum = MdotZnDehumidSP/DeltaHumRat
                   ENDIF
                 ENDIF
               ENDIF
             ENDIF

                 ! Mass flow rate to meet humidification load, if applicable, at Maximum Heating Supply Humidity Ratio
             SupplyMassFlowRateForHumid = 0.0d0
             IF (HeatOn) THEN
               IF (PurchAir(PurchAirNum)%HumidCtrlType .EQ. Humidistat) THEN
                 MdotZnHumidSP = ZoneSysMoistureDemand(ActualZoneNum)%RemainingOutputReqToHumidSP
                 DeltaHumRat = (PurchAir(PurchAirNum)%MaxHeatSuppAirHumRat - Node(ZoneNodeNum)%HumRat)
                 IF ((DeltaHumRat > SmallDeltaHumRat) .AND. (MdotZnHumidSP .GT. 0.0d0)) THEN
                   SupplyMassFlowRateForHumid = MdotZnHumidSP/DeltaHumRat
                 ENDIF
               ENDIF
             ENDIF

                 ! Supply mass flow is greatest of these, but limit to heating max flow rate, if applicable
             SupplyMassFlowRate = MAX(0.0d0, OAMassFlowRate, SupplyMassFlowRateForHeat, &
                                     SupplyMassFlowRateForDehum, SupplyMassFlowRateForHumid)
             ! EMS override point  Purch air massflow rate..... but only if unit is on, i.e. SupplyMassFlowRate>0.0
             IF ((PurchAir(PurchAirNum)%EMSOverrideMdotOn) .AND. (SupplyMassFlowRate > 0.0d0)) THEN
               SupplyMassFlowRate = PurchAir(PurchAirNum)%EMSValueMassFlowRate
               OAMassFlowRate = MIN(OAMassFlowRate,SupplyMassFlowRate)
             ENDIF
             IF (((PurchAir(PurchAirNum)%HeatingLimit == LimitFlowRate) .OR. &
                  (PurchAir(PurchAirNum)%HeatingLimit == LimitFlowRateAndCapacity)) &
                  .AND. (PurchAir(PurchAirNum)%MaxHeatMassFlowRate .GT. 0.0d0)) THEN
               SupplyMassFlowRate = MIN(SupplyMassFlowRate,PurchAir(PurchAirNum)%MaxHeatMassFlowRate)
             END IF

             IF (SupplyMassFlowRate <= VerySmallMassFlow) SupplyMassFlowRate = 0.0d0


             ! Calculate mixed air conditions
             CALL CalcPurchAirMixedAir(PurchAirNum, OAMassFlowRate, SupplyMassFlowRate, &
                                         MixedAirTemp, MixedAirHumRat, MixedAirEnthalpy, OperatingMode)

             ! Calculate supply air conditions using final massflow rate, imposing capacity limits if specified
             ! If capacity limits are exceeded, keep massflow rate where it is and adjust supply temp
             IF (SupplyMassFlowRate .GT. 0.0d0) THEN
               IF ((HeatOn) .AND. (OperatingMode == Heat)) THEN
                 ! Calculate supply temp at SupplyMassFlowRate and check limit on Maximum Heating Supply Air Temperature
                 CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ActualZoneNum),Node(ZoneNodeNum)%Temp, 'CalcPurchAirLoads')
                 SupplyTemp = QZnHeatSP/(CPAir*SupplyMassFlowRate) + Node(ZoneNodeNum)%Temp
                 SupplyTemp = MIN(SupplyTemp,PurchAir(PurchAirNum)%MaxHeatSuppAirTemp)
                 ! This is the heating mode, so SupplyTemp can't be less than MixedAirTemp
                 SupplyTemp = MAX(SupplyTemp,MixedAirTemp)
                 ! Check max heating capacity, if specified
                 IF ((PurchAir(PurchAirNum)%HeatingLimit == LimitCapacity) .OR. &
                   (PurchAir(PurchAirNum)%HeatingLimit == LimitFlowRateAndCapacity)) THEN
                   CpAir = PsyCpAirFnWTdb(MixedAirHumRat,MixedAirTemp, 'CalcPurchAirLoads')
                   HeatSensOutput = SupplyMassFlowRate * CpAir * (SupplyTemp - MixedAirTemp)
                   IF (HeatSensOutput > PurchAir(PurchAirNum)%MaxHeatSensCap) THEN
                     SupplyTemp = PurchAir(PurchAirNum)%MaxHeatSensCap/(SupplyMassFlowRate * CpAir) + MixedAirTemp
                     HeatSensOutput = PurchAir(PurchAirNum)%MaxHeatSensCap
                   END IF
                 END IF
               ELSE ! Heat is off or operating mode is deadband (i.e. don't do any heating)
                 SupplyTemp = MixedAirTemp
               END IF

               ! Set supply humidity ratio first for heating/humidification
               SupplyHumRat = MixedAirHumRat
               SELECT CASE(PurchAir(PurchAirNum)%HumidCtrlType)
                 CASE(None)
                   SupplyHumRat = MixedAirHumRat
                 CASE(Humidistat)
                   MdotZnHumidSP = ZoneSysMoistureDemand(ActualZoneNum)%RemainingOutputReqToHumidSP
                   SupplyHumRatForHumid = MdotZnHumidSP/SupplyMassFlowRate + Node(ZoneNodeNum)%HumRat
                   SupplyHumRatForHumid = MIN(SupplyHumRatForHumid,PurchAir(PurchAirNum)%MaxHeatSuppAirHumRat)
                   SupplyHumRat = MAX(SupplyHumRat,SupplyHumRatForHumid)
                 CASE(ConstantSupplyHumidityRatio)
                   IF (OperatingMode == Heat) THEN
                     ! If this results in dehumidification, must check cooling capacity limit
                     IF (MixedAirHumRat > PurchAir(PurchAirNum)%MaxHeatSuppAirHumRat) THEN
                       IF ((PurchAir(PurchAirNum)%CoolingLimit == LimitCapacity) .OR. &
                           (PurchAir(PurchAirNum)%CoolingLimit == LimitFlowRateAndCapacity)) THEN
                         SupplyHumRat = PurchAir(PurchAirNum)%MaxHeatSuppAirHumRat
                         SupplyEnthalpy = PsyHFnTdbW(SupplyTemp,SupplyHumRat, 'CalcPurchAirLoads')
                         CoolTotOutput = SupplyMassFlowRate * (MixedAirEnthalpy - SupplyEnthalpy)
                         CpAir = PsyCpAirFnWTdb(MixedAirHumRat,MixedAirTemp, 'CalcPurchAirLoads')
                         CoolSensOutput = SupplyMassFlowRate * CpAir * (MixedAirTemp - SupplyTemp)
                         CoolLatOutput = CoolTotOutput - CoolSensOutput
                         IF (CoolLatOutput >= PurchAir(PurchAirNum)%MaxCoolTotCap) THEN
                           CoolLatOutput = PurchAir(PurchAirNum)%MaxCoolTotCap
                           CoolTotOutput = CoolSensOutput + CoolLatOutput
                           SupplyEnthalpy = MixedAirEnthalpy - CoolTotOutput/SupplyMassFlowRate
                           SupplyHumRat = PsyWFnTdbH(SupplyTemp,SupplyEnthalpy, 'CalcPurchAirLoads')
                         END IF
                       ELSE
                         SupplyHumRat = PurchAir(PurchAirNum)%MaxHeatSuppAirHumRat
                       END IF
                     ELSE
                       SupplyHumRat = PurchAir(PurchAirNum)%MaxHeatSuppAirHumRat
                     END IF
                   ELSE
                     SupplyHumRat = MixedAirHumRat
                   END IF
                 CASE DEFAULT
                   SupplyHumRat = MixedAirHumRat
               END SELECT
               SupplyEnthalpy = PsyHFnTdbW(SupplyTemp,SupplyHumRat, 'CalcPurchAirLoads')

                 ! Check supply humidity ratio for dehumidification (SupplyHumRatForHumid should always be < SupplyHumRatForDehum)
                 ! This section is the heating/deadband section, so dehumidification should activate
                 !   only if dehumidification control = humidistat
                 !   and if humidification control = humidistat or none or if operating in deadband mode
               IF (CoolOn) THEN
                 IF (PurchAir(PurchAirNum)%DehumidCtrlType .EQ. Humidistat) THEN
                   IF ((PurchAir(PurchAirNum)%HumidCtrlType .EQ. Humidistat) .OR. &
                       (PurchAir(PurchAirNum)%HumidCtrlType .EQ. None) .OR. &
                       (OperatingMode == Deadband)) THEN
                     MdotZnDehumidSP= ZoneSysMoistureDemand(ActualZoneNum)%RemainingOutputReqToDehumidSP
                     SupplyHumRatForDehum = MdotZnDehumidSP/SupplyMassFlowRate + Node(ZoneNodeNum)%HumRat
                     SupplyHumRatForDehum = MAX(SupplyHumRatForDehum,PurchAir(PurchAirNum)%MinCoolSuppAirHumRat)
                     SupplyHumRat = MIN(SupplyHumRat, SupplyHumRatForDehum)
                     SupplyEnthalpy = PsyHFnTdbW(SupplyTemp,SupplyHumRat, 'CalcPurchAirLoads')
                     IF (SupplyHumRat < MixedAirHumRat) THEN
                       ! At this point, the system is heating or deadband but dehumidifying, check max cooling cap limit
                       CpAir = PsyCpAirFnWTdb(MixedAirHumRat,MixedAirTemp)
                       SensOutput = SupplyMassFlowRate * CpAir * (SupplyTemp - MixedAirTemp)
                       LatOutput = SupplyMassFlowRate * (SupplyEnthalpy - MixedAirEnthalpy) - SensOutput
                       IF ((PurchAir(PurchAirNum)%CoolingLimit == LimitCapacity) .OR. &
                           (PurchAir(PurchAirNum)%CoolingLimit == LimitFlowRateAndCapacity)) THEN
                         IF (LatOutput > PurchAir(PurchAirNum)%MaxCoolTotCap) THEN
                           LatOutput = PurchAir(PurchAirNum)%MaxCoolTotCap
                           SupplyEnthalpy = MixedAirEnthalpy + (LatOutput + SensOutput)/SupplyMassFlowRate
                           SupplyHumRat = PsyWFnTdbH(SupplyTemp,SupplyEnthalpy, 'CalcPurchAirLoads')
                         END IF
                       END IF
                     END IF
                   END IF
                 END IF
               END IF


                 !   Limit supply humidity ratio to saturation at supply outlet temp
               SupplyHumRatOrig = SupplyHumRat
               SupplyHumRat = MIN(SupplyHumRat,PsyWFnTdbRhPb(SupplyTemp,1.0d0,OutBaroPress, 'CalcPurchAirLoads'))
               SupplyEnthalpy = PsyHFnTdbW(SupplyTemp,SupplyHumRat, 'CalcPurchAirLoads')

             ELSE ! SupplyMassFlowRate is zero
               SupplyEnthalpy = MixedAirEnthalpy
               SupplyHumRat   = MixedAirHumRat
               SupplyTemp     = MixedAirTemp
               HeatSensOutput = 0.0d0
             END IF

           END IF ! Cooling or heating required

             ! EMS override point  Purch air supply temp and humidty ratio ..... but only if unit is on, SupplyMassFlowRate>0.0
           IF ((PurchAir(PurchAirNum)%EMSOverrideSupplyTempOn) .AND. (SupplyMassFlowRate > 0.0d0)) THEN
             SupplyTemp = PurchAir(PurchAirNum)%EMSValueSupplyTemp
           ENDIF
           IF ((PurchAir(PurchAirNum)%EMSOverrideSupplyHumRatOn) .AND. (SupplyMassFlowRate > 0.0d0)) THEN
             SupplyHumRat = PurchAir(PurchAirNum)%EMSValueSupplyHumRat
           ENDIF

           IF (SupplyMassFlowRate > 0.0d0) THEN
             PurchAir(PurchAirNum)%FinalMixedAirTemp = MixedAirtemp
             PurchAir(PurchAirNum)%FinalMixedAirHumRat = MixedAirHumRat
             ! compute coil loads
             IF ((SupplyHumRat == MixedAirHumRat) .AND. (SupplyTemp == MixedAirTemp)) THEN
               ! If no change in humrat or temp, then set loads to zero
               PurchAir(PurchAirNum)%SenCoilLoad = 0.0d0
               PurchAir(PurchAirNum)%LatCoilLoad = 0.0d0
             ELSE IF ((SupplyHumRat == MixedAirHumRat) .AND. (SupplyTemp .NE. MixedAirTemp)) THEN
               ! If no change in humrat, then set latent load to zero and use enthalpies to calculate sensible load
               PurchAir(PurchAirNum)%SenCoilLoad = SupplyMassFlowRate * (SupplyEnthalpy - MixedAirEnthalpy)
               PurchAir(PurchAirNum)%LatCoilLoad = 0.0d0
             ELSE
               CpAir = PsyCpAirFnWTdb(MixedAirHumRat,MixedAirTemp)
               PurchAir(PurchAirNum)%SenCoilLoad = SupplyMassFlowRate * CpAir * (SupplyTemp - MixedAirTemp)
               PurchAir(PurchAirNum)%LatCoilLoad = SupplyMassFlowRate * (SupplyEnthalpy - MixedAirEnthalpy) &
                                                         - PurchAir(PurchAirNum)%SenCoilLoad
             END IF

             ! Apply heating and cooling availability schedules to sensible load
             IF (((PurchAir(PurchAirNum)%SenCoilLoad > 0.0d0) .AND. .NOT. HeatOn) &
                                  .OR.                                                       &
                 ((PurchAir(PurchAirNum)%SenCoilLoad < 0.0d0) .AND. .NOT. CoolOn)) THEN
               ! Coil is off
               PurchAir(PurchAirNum)%SenCoilLoad = 0.0d0
               SupplyTemp = MixedAirTemp
             END IF

             ! Apply heating and cooling availability schedules to latent load
             IF (((PurchAir(PurchAirNum)%LatCoilLoad > 0.0d0) .AND. .NOT. HeatOn) &
                                  .OR.                                                       &
                 ((PurchAir(PurchAirNum)%LatCoilLoad < 0.0d0) .AND. .NOT. CoolOn)) THEN
               ! Coil is off
               PurchAir(PurchAirNum)%LatCoilLoad = 0.0d0
               SupplyHumRat = MixedAirHumRat
             END IF

             ! Double-check if saturation exceeded, then thow warning, shouldn't happen here, don't reset, just warn
             SupplyHumRatOrig = SupplyHumRat
             SupplyHumRatSat  = PsyWFnTdbRhPb(SupplyTemp,1.0d0,OutBaroPress, 'CalcPurchAirLoads')
             DeltaHumRat = SupplyHumRatOrig - SupplyHumRatSat
             IF (DeltaHumRat > SmallDeltaHumRat) THEN
               IF (PurchAir(PurchAirNum)%SaturationOutputError < 1) THEN
                 PurchAir(PurchAirNum)%SaturationOutputError = PurchAir(PurchAirNum)%SaturationOutputError + 1
                 Call ShowWarningError(TRIM(PurchAir(PurchAirNum)%cObjectName)//' "'//TRIM(PurchAir(PurchAirNum)%Name)//'"'&
                                       //' Supply humidity ratio = '//TRIM(TrimSigDigits(SupplyHumRatOrig,5)) &
                                       //' exceeds saturation limit '//TRIM(TrimSigDigits(SupplyHumRatSat,5))//  &
                                       ' [kgWater/kgDryAir]')
                 CALL ShowContinueError(' Simulation continuing . . . ')
                 CALL ShowContinueErrorTimeStamp(' ')
               ELSE
                 CALL ShowRecurringWarningErrorAtEnd(TRIM(PurchAir(PurchAirNum)%cObjectName)//' "'&
                       //TRIM(PurchAir(PurchAirNum)%Name)//'"'//&
                       ' Supply humidity ratio exceeds saturation limit warning continues, delta max/min [kgWater/kgDryAir]...' &
                       , PurchAir(PurchAirNum)%SaturationOutputIndex, DeltaHumRat, DeltaHumRat)
               END IF
             END IF

             SupplyEnthalpy = PsyHFnTdbW(SupplyTemp,SupplyHumRat, 'CalcPurchAirLoads')

             CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ActualZoneNum),Node(ZoneNodeNum)%Temp)
             SysOutputProvided = SupplyMassFlowRate * CpAir * (SupplyTemp - Node(ZoneNodeNum)%Temp)
             MoistOutputProvided = SupplyMassFlowRate * (SupplyHumRat - Node(ZoneNodeNum)%HumRat) ! Latent rate, kg/s

             PurchAir(PurchAirNum)%SenOutputToZone = SysOutputProvided
             PurchAir(PurchAirNum)%LatOutputToZone = SupplyMassFlowRate * (SupplyEnthalpy - Node(ZoneNodeNum)%Enthalpy) &
                                                       - PurchAir(PurchAirNum)%SenOutputToZone

             CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ActualZoneNum),Node(ZoneNodeNum)%Temp)
             IF (PurchAir(PurchAirNum)%OutdoorAir) THEN
               PurchAir(PurchAirNum)%OASenOutput = OAMassFlowRate * CpAir * (Node(OANodeNum)%Temp - Node(ZoneNodeNum)%Temp)
               PurchAir(PurchAirNum)%OALatOutput = OAMassFlowRate * (Node(OANodeNum)%Enthalpy - Node(ZoneNodeNum)%Enthalpy) &
                                                       - PurchAir(PurchAirNum)%OASenOutput
             ELSE
               PurchAir(PurchAirNum)%OASenOutput = 0.0d0
               PurchAir(PurchAirNum)%OALatOutput = 0.0d0
             ENDIF
             IF (Contaminant%CO2Simulation) THEN
               IF (PurchAir(PurchAirNum)%OutdoorAir) THEN
                 Node(InNodeNum)%CO2 = ((SupplyMassFlowRate - OAMassFlowRate)*Node(RecircNodeNum)%CO2 + &
                    OAMassFlowRate*Node(OANodeNum)%CO2) / SupplyMassFlowRate
               Else
                 Node(InNodeNum)%CO2 = Node(RecircNodeNum)%CO2
               End If
             END IF
             IF (Contaminant%GenericContamSimulation) THEN
               IF (PurchAir(PurchAirNum)%OutdoorAir) THEN
                 Node(InNodeNum)%GenContam = ((SupplyMassFlowRate - OAMassFlowRate)*Node(RecircNodeNum)%GenContam + &
                    OAMassFlowRate*Node(OANodeNum)%GenContam) / SupplyMassFlowRate
               Else
                 Node(InNodeNum)%GenContam = Node(RecircNodeNum)%GenContam
               End If
             END IF
           ELSE ! SupplyMassFlowRate = 0.0
             SysOutputProvided  = 0.0d0
             MoistOutputProvided  = 0.0d0

             PurchAir(PurchAirNum)%SenOutputToZone = 0.0d0
             PurchAir(PurchAirNum)%LatOutputToZone = 0.0d0
             PurchAir(PurchAirNum)%SenCoilLoad = 0.0d0
             PurchAir(PurchAirNum)%LatCoilLoad = 0.0d0
             PurchAir(PurchAirNum)%OASenOutput = 0.0d0
             PurchAir(PurchAirNum)%OALatOutput = 0.0d0
             PurchAir(PurchAirNum)%FinalMixedAirTemp = Node(RecircNodeNum)%Temp
             PurchAir(PurchAirNum)%FinalMixedAirHumRat = Node(RecircNodeNum)%HumRat
             IF (Contaminant%CO2Simulation) THEN
               Node(InNodeNum)%CO2        = Node(ZoneNodeNum)%CO2
             END IF
             IF (Contaminant%GenericContamSimulation) THEN
               Node(InNodeNum)%GenContam  = Node(ZoneNodeNum)%GenContam
             END IF
           END IF

           Node(InNodeNum)%Temp         = SupplyTemp
           Node(InNodeNum)%HumRat       = SupplyHumRat
           Node(InNodeNum)%Enthalpy     = SupplyEnthalpy
           Node(InNodeNum)%MassFlowRate = SupplyMassFlowRate
           IF (PurchAir(PurchAirNum)%OutdoorAir) Node(OANodeNum)%MassFlowRate = OAMassFlowRate

         ELSE ! purchased air OFF

           SysOutputProvided  = 0.0d0
           MoistOutputProvided  = 0.0d0
           SupplyMassFlowRate = 0.0d0
           OAMassFlowRate     = 0.0d0
           Node(InNodeNum)%Temp         = Node(ZoneNodeNum)%Temp
           Node(InNodeNum)%HumRat       = Node(ZoneNodeNum)%HumRat
           Node(InNodeNum)%Enthalpy     = Node(ZoneNodeNum)%Enthalpy
           IF (Contaminant%CO2Simulation) THEN
             Node(InNodeNum)%CO2        = Node(ZoneNodeNum)%CO2
           END IF
           IF (Contaminant%GenericContamSimulation) THEN
             Node(InNodeNum)%GenContam  = Node(ZoneNodeNum)%GenContam
           END IF

           Node(InNodeNum)%MassFlowRate = 0.0d0
           IF (PurchAir(PurchAirNum)%OutdoorAir) Node(OANodeNum)%MassFlowRate = 0.0d0
           PurchAir(PurchAirNum)%SenHeatRate = 0.0d0
           PurchAir(PurchAirNum)%SenCoolRate = 0.0d0
           PurchAir(PurchAirNum)%TotCoolRate = 0.0d0

           PurchAir(PurchAirNum)%SenOutputToZone = 0.0d0
           PurchAir(PurchAirNum)%LatOutputToZone = 0.0d0
           PurchAir(PurchAirNum)%SenCoilLoad = 0.0d0
           PurchAir(PurchAirNum)%LatCoilLoad = 0.0d0
           PurchAir(PurchAirNum)%OASenOutput = 0.0d0
           PurchAir(PurchAirNum)%OALatOutput = 0.0d0
           PurchAir(PurchAirNum)%FinalMixedAirTemp = Node(RecircNodeNum)%Temp
           PurchAir(PurchAirNum)%FinalMixedAirHumRat = Node(RecircNodeNum)%HumRat

         END IF

         PurchAir(PurchAirNum)%OutdoorAirMassFlowRate = OAMassFlowRate

         RETURN

    END SUBROUTINE CalcPurchAirLoads

    SUBROUTINE CalcPurchAirMinOAMassFlow(PurchAirNum, ActualZoneNum, OAMassFlowRate)

              ! SUBROUTINE INFORMATION:
              !       AUTHOR         M. Witte (GARD)
              !       DATE WRITTEN   Jun 2011 (taken from HVACSingleDuctSystem.f90 and adapted for Ideal Loads System)
              !       MODIFIED       na
              !       RE-ENGINEERED  na

              ! PURPOSE OF THIS SUBROUTINE:
              ! Calculates the amount of outside air required based on optional user input.
              ! Zone multipliers have been applied in GetInput.

              ! METHODOLOGY EMPLOYED:
              ! User input defines method used to calculate OA.

              ! REFERENCES:

              ! USE STATEMENTS:
        USE DataHeatBalance,   ONLY: ZoneIntGain, Zone
        USE DataEnvironment,   ONLY: StdRhoAir
        USE DataSizing, ONLY: OAFlowPPer, OAFlow, OAFlowPerArea, OAFlowACH, OAFlowSum, OAFlowMax
        USE DataContaminantBalance, ONLY: ZoneSysContDemand
        USE DataZoneEquipment, ONLY: CalcDesignSpecificationOutdoorAir

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

              ! SUBROUTINE ARGUMENT DEFINITIONS:
        INTEGER, INTENT(IN)      :: PurchAirNum    ! index to ideal loads unit
        INTEGER, INTENT(IN)      :: ActualZoneNum  ! index to actual zone number
        REAL(r64), INTENT(OUT)   :: OAMassFlowRate ! outside air mass flow rate [kg/s] from volume flow using std density

              ! FUNCTION PARAMETER DEFINITIONS:
        LOGICAL, PARAMETER   :: UseMinOASchFlag = .TRUE. ! Always use min OA schedule in calculations.

              ! INTERFACE BLOCK SPECIFICATIONS
              ! na

              ! DERIVED TYPE DEFINITIONS
              ! na

              ! FUNCTION LOCAL VARIABLE DECLARATIONS:
        LOGICAL   :: UseOccSchFlag     ! TRUE = use actual occupancy, FALSE = use total zone people
        REAL(r64) :: OAVolumeFlowRate  ! outside air flow rate (m3/s)

        IF (PurchAir(PurchAirNum)%OutdoorAir) THEN

          IF(PurchAir(PurchAirNum)%DCVType == OccupancySchedule)THEN
            UseOccSchFlag = .TRUE.
          ELSE
            UseOccSchFlag = .FALSE.
          END IF
          OAVolumeFlowRate = CalcDesignSpecificationOutdoorAir(PurchAir(PurchAirNum)%OARequirementsPtr, &
                                                                ActualZoneNum, UseOccSchFlag, UseMinOASchFlag)
          OAMassFlowRate = OAVolumeFlowRate * StdRhoAir

          ! If DCV with CO2Setpoint then check required OA flow to meet CO2 setpoint
          IF (PurchAir(PurchAirNum)%DCVType == CO2Setpoint) THEN
            OAMassFlowRate = Max(OAMassFlowRate, ZoneSysContDemand(ActualZoneNum)%OutputRequiredToCO2SP)
          END IF

          IF (OAMassFlowRate <= VerySmallMassFlow) OAMassFlowRate = 0.0d0

        ELSE !No outdoor air
          OAMassFlowRate = 0.0d0
        END IF
        PurchAir(PurchAirNum)%MinOAMassFlowRate = OAMassFlowRate

        RETURN

    END SUBROUTINE CalcPurchAirMinOAMassFlow


    SUBROUTINE CalcPurchAirMixedAir(PurchAirNum, OAMassFlowRate, SupplyMassFlowRate, &
                                         MixedAirTemp, MixedAirHumRat, MixedAirEnthalpy, OperatingMode)

              ! SUBROUTINE INFORMATION:
              !       AUTHOR         M. Witte (GARD)
              !       DATE WRITTEN   Sep 2011
              !       MODIFIED       na
              !       RE-ENGINEERED  na

              ! PURPOSE OF THIS SUBROUTINE:
              ! Calculates the mixed air conditions, accounting for heat recovery.

              ! METHODOLOGY EMPLOYED:
              ! na

              ! REFERENCES:

              ! USE STATEMENTS:
        USE DataLoopNode, ONLY: Node

        IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

              ! SUBROUTINE ARGUMENT DEFINITIONS:
        INTEGER, INTENT(IN)      :: PurchAirNum        ! index to ideal loads unit
        INTEGER, INTENT(IN)      :: OperatingMode      ! current operating mode, Off, Heating, Cooling, or Deadband
        REAL(r64), INTENT(IN)    :: OAMassFlowRate     ! outside air mass flow rate [kg/s]
        REAL(r64), INTENT(IN)    :: SupplyMassFlowRate ! supply air mass flow rate [kg/s]
        REAL(r64), INTENT(OUT)   :: MixedAirTemp       ! Mixed air dry bulb temperature [C]
        REAL(r64), INTENT(OUT)   :: MixedAirHumRat     ! Mixed air humidity ratio [kg H2O/kg Air]
        REAL(r64), INTENT(OUT)   :: MixedAirEnthalpy   ! Mixed air enthalpy [J/kg]

              ! SUBROUTINE PARAMETER DEFINITIONS:
              ! na

              ! INTERFACE BLOCK SPECIFICATIONS
              ! na

              ! DERIVED TYPE DEFINITIONS
              ! na

              ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
         INTEGER   :: RecircNodeNum     ! Zone return air node
         INTEGER   :: OANodeNum         ! Outdoor air inlet node
         REAL(r64) :: RecircTemp        ! Recirculated air from zone dry bulb temperature [C]
         REAL(r64) :: RecircHumRat      ! Recirculated air from zone humidity ratio [kg H2O/kg Air]
         REAL(r64) :: RecircEnthalpy    ! Recirculated air from zone enthalpy [J/kg]
         REAL(r64) :: RecircMassFlowRate ! Recirculated air mass flow rate [kg/s]
         REAL(r64) :: OAInletTemp       ! Outdoor air inlet dry bulb temperature [C]
         REAL(r64) :: OAInletHumRat     ! Outdoor air inlet humidity ratio [kg H2O/kg Air]
         REAL(r64) :: OAInletEnthalpy   ! Outdoor air inlet enthalpy [J/kg]
         REAL(r64) :: OAAfterHtRecTemp     ! Outdoor air after heat recovery to mixing box dry bulb temperature [C]
         REAL(r64) :: OAAfterHtRecHumRat   ! Outdoor air after heat recovery to mixing box humidity ratio [kg H2O/kg Air]
         REAL(r64) :: OAAfterHtRecEnthalpy ! Outdoor air after heat recovery to mixing box enthalpy [J/kg]
         LOGICAL   :: HeatRecOn
         REAL(r64) :: CpAir             ! Specific heat [J/kg-C] reused in multiple places

             ! Initializations
         OANodeNum     = PurchAir(PurchAirNum)%OutdoorAirNodeNum
         RecircNodeNum = PurchAir(PurchAirNum)%ZoneRecircAirNodeNum

         RecircMassFlowRate = 0.0d0
         RecircTemp      = Node(RecircNodeNum)%Temp
         RecircHumRat    = Node(RecircNodeNum)%HumRat
         RecircEnthalpy  = Node(RecircNodeNum)%Enthalpy
         IF (PurchAir(PurchAirNum)%OutdoorAir) THEN
           OAInletTemp     = Node(OANodeNum)%Temp
           OAInletHumRat   = Node(OANodeNum)%HumRat
           OAInletEnthalpy = Node(OANodeNum)%Enthalpy
           OAAfterHtRecTemp     = OAInletTemp
           OAAfterHtRecHumRat   = OAInletHumRat
           OAAfterHtRecEnthalpy = OAInletEnthalpy
         ELSE
           OAInletTemp     = 0.0d0
           OAInletHumRat   = 0.0d0
           OAInletEnthalpy = 0.0d0
           OAAfterHtRecTemp     = OAInletTemp
           OAAfterHtRecHumRat   = OAInletHumRat
           OAAfterHtRecEnthalpy = OAInletEnthalpy
         ENDIF
         HeatRecOn = .FALSE.

         IF (PurchAir(PurchAirNum)%OutdoorAir .AND. (OAMassFlowRate > 0.0d0)) THEN
             ! Determine if heat recovery is beneficial
             IF ((PurchAir(PurchAirNum)%HtRecType == Sensible) ) THEN
               IF ((OperatingMode == Heat) .AND. (RecircTemp > OAInletTemp)) HeatRecOn = .TRUE.
               IF ((OperatingMode == Cool) .AND. (RecircTemp < OAInletTemp)) HeatRecOn = .TRUE.
             END IF
             IF ((PurchAir(PurchAirNum)%HtRecType == Enthalpy)) THEN
               IF ((OperatingMode == Heat) .AND. (RecircEnthalpy > OAInletEnthalpy)) HeatRecOn = .TRUE.
               IF ((OperatingMode == Cool) .AND. (RecircEnthalpy < OAInletEnthalpy)) HeatRecOn = .TRUE.
             END IF
             ! Calculate heat recovery if active
             IF (HeatRecOn) THEN
               PurchAir(PurchAirNum)%TimeHtRecActive = TimeStepSys
               OAAfterHtRecTemp = OAInletTemp + PurchAir(PurchAirNum)%HtRecSenEff*(RecircTemp - OAInletTemp)
               IF (PurchAir(PurchAirNum)%HtRecType == Enthalpy) &
                 OAAfterHtRecHumRat = OAInletHumRat + PurchAir(PurchAirNum)%HtRecLatEff*(RecircHumRat - OAInletHumRat)
               OAAfterHtRecEnthalpy = PsyHFnTdbW(OAAfterHtRecTemp,OAAfterHtRecHumRat, 'CalcPurchAirMixedAir')
               !   Check for saturation in supply outlet and reset temp, then humidity ratio at constant enthalpy
               IF (PsyTsatFnHPb(OAAfterHtRecEnthalpy,OutBaroPress, 'CalcPurchAirMixedAir') > OAAfterHtRecTemp) THEN
                 OAAfterHtRecTemp   = PsyTsatFnHPb(OAAfterHtRecEnthalpy,OutBaroPress, 'CalcPurchAirMixedAir')
                 OAAfterHtRecHumRat = PsyWFnTdbH(OAAfterHtRecTemp,OAAfterHtRecEnthalpy, 'CalcPurchAirMixedAir')
               END IF
             END IF

             IF (SupplyMassFlowRate .GT. OAMassFlowRate) THEN
               RecircMassFlowRate = SupplyMassFlowRate - OAMassFlowRate
               MixedAirEnthalpy = (RecircMassFlowRate*Node(RecircNodeNum)%Enthalpy + OAMassFlowRate*OAAfterHtRecEnthalpy) / &
                                    SupplyMassFlowRate
               MixedAirHumRat   = (RecircMassFlowRate*Node(RecircNodeNum)%HumRat + OAMassFlowRate*OAAfterHtRecHumRat) / &
                                    SupplyMassFlowRate
               ! Mixed air temperature is calculated from the mixed air enthalpy and humidity ratio.
               MixedAirTemp = PsyTdbFnHW(MixedAirEnthalpy,MixedAirHumRat, 'CalcPurchAirMixedAir')
             ELSE
               RecircMassFlowRate   = 0.0d0
               MixedAirEnthalpy = OAAfterHtRecEnthalpy
               MixedAirHumRat   = OAAfterHtRecHumRat
               MixedAirTemp     = OAAfterHtRecTemp
             END IF

               ! Calculate OA and heat recovery sensible and latent rates
             CpAir = PsyCpAirFnWTdb(OAInletHumRat,OAInletTemp)
             PurchAir(PurchAirNum)%HtRecSenOutput = OAMassFlowRate * CPAir * (OAAfterHtRecTemp - OAInletTemp)
             PurchAir(PurchAirNum)%HtRecLatOutput = OAMassFlowRate * (OAAfterHtRecEnthalpy - OAInletEnthalpy) &
                                                       - PurchAir(PurchAirNum)%HtRecSenOutput

         ELSE ! No outdoor air
           RecircMassFlowRate = SupplyMassFlowRate
           MixedAirTemp     = RecircTemp
           MixedAirHumRat   = RecircHumRat
           MixedAirEnthalpy = RecircEnthalpy
           PurchAir(PurchAirNum)%HtRecSenOutput = 0.0d0
           PurchAir(PurchAirNum)%HtRecLatOutput = 0.0d0
         END IF
        ! If exhaust node is specified, then set massflow on exhaust node, otherwise return node sets its own massflow
        IF (PurchAir(PurchAirNum)%ZoneExhaustAirNodeNum .GT. 0) THEN
          Node(RecircNodeNum)%MassFlowRate = RecircMassFlowRate
        END IF

        RETURN

    END SUBROUTINE CalcPurchAirMixedAir


    SUBROUTINE UpdatePurchasedAir(PurchAirNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         M. J. Witte
          !       DATE WRITTEN   Sep 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Update node data for Ideal Loads (purchased air) system

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

      IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
         INTEGER, INTENT(IN) :: PurchAirNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

      RETURN

    END SUBROUTINE UpdatePurchasedAir

    SUBROUTINE ReportPurchasedAir(PurchAirNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor
          !       DATE WRITTEN   Nov 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate values of report variables, if necessary.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
      USE DataHVACGlobals, ONLY: TimeStepSys

      IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
      INTEGER, INTENT(IN) :: PurchAirNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      REAL(r64) :: ReportingConstant

          ! Sort out heating and cooling rates
      PurchAir(PurchAirNum)%SenHeatRate = MAX(PurchAir(PurchAirNum)%SenCoilLoad,0.0d0)
      PurchAir(PurchAirNum)%SenCoolRate = ABS(MIN(PurchAir(PurchAirNum)%SenCoilLoad,0.0d0))
      PurchAir(PurchAirNum)%LatHeatRate = MAX(PurchAir(PurchAirNum)%LatCoilLoad,0.0d0)
      PurchAir(PurchAirNum)%LatCoolRate = ABS(MIN(PurchAir(PurchAirNum)%LatCoilLoad,0.0d0))
      PurchAir(PurchAirNum)%TotHeatRate = PurchAir(PurchAirNum)%SenHeatRate + PurchAir(PurchAirNum)%LatHeatRate
      PurchAir(PurchAirNum)%TotCoolRate = PurchAir(PurchAirNum)%SenCoolRate + PurchAir(PurchAirNum)%LatCoolRate

      PurchAir(PurchAirNum)%ZoneSenHeatRate = MAX(PurchAir(PurchAirNum)%SenOutputToZone,0.0d0)
      PurchAir(PurchAirNum)%ZoneSenCoolRate = ABS(MIN(PurchAir(PurchAirNum)%SenOutputToZone,0.0d0))
      PurchAir(PurchAirNum)%ZoneLatHeatRate = MAX(PurchAir(PurchAirNum)%LatOutputToZone,0.0d0)
      PurchAir(PurchAirNum)%ZoneLatCoolRate = ABS(MIN(PurchAir(PurchAirNum)%LatOutputToZone,0.0d0))
      PurchAir(PurchAirNum)%ZoneTotHeatRate = PurchAir(PurchAirNum)%ZoneSenHeatRate + PurchAir(PurchAirNum)%ZoneLatHeatRate
      PurchAir(PurchAirNum)%ZoneTotCoolRate = PurchAir(PurchAirNum)%ZoneSenCoolRate + PurchAir(PurchAirNum)%ZoneLatCoolRate

         ! Sort out outdoor air "loads"
         ! OASenOutput = Outdoor air sensible output relative to zone conditions [W], <0 means OA is cooler than zone air
         ! OALatOutput  = Outdoor air latent output relative to zone conditions [W], <0 means OA is drier than zone air
      IF (PurchAir(PurchAirNum)%SenCoilLoad > 0.0d0) THEN ! Heating is active
        PurchAir(PurchAirNum)%OASenHeatRate = ABS(MIN(PurchAir(PurchAirNum)%OASenOutput,0.0d0))
      ELSE
        PurchAir(PurchAirNum)%OASenHeatRate = 0.0d0
      END IF
      IF (PurchAir(PurchAirNum)%SenCoilLoad < 0.0d0) THEN ! Cooling is active
        PurchAir(PurchAirNum)%OASenCoolRate = MAX(PurchAir(PurchAirNum)%OASenOutput,0.0d0)
      ELSE
        PurchAir(PurchAirNum)%OASenCoolRate = 0.0d0
      END IF
      IF (PurchAir(PurchAirNum)%LatCoilLoad > 0.0d0) THEN ! Humidification is active
        PurchAir(PurchAirNum)%OALatHeatRate =  ABS(MIN(PurchAir(PurchAirNum)%OALatOutput,0.0d0))
      ELSE
        PurchAir(PurchAirNum)%OALatHeatRate = 0.0d0
      END IF
      IF (PurchAir(PurchAirNum)%LatCoilLoad < 0.0d0) THEN ! Dehumidification is active
        PurchAir(PurchAirNum)%OALatCoolRate = MAX(PurchAir(PurchAirNum)%OALatOutput,0.0d0)
      ELSE
        PurchAir(PurchAirNum)%OALatCoolRate = 0.0d0
      ENDIF

      PurchAir(PurchAirNum)%OATotHeatRate = PurchAir(PurchAirNum)%OASenHeatRate + PurchAir(PurchAirNum)%OALatHeatRate
      PurchAir(PurchAirNum)%OATotCoolRate = PurchAir(PurchAirNum)%OASenCoolRate + PurchAir(PurchAirNum)%OALatCoolRate

      PurchAir(PurchAirNum)%HtRecSenHeatRate = MAX(PurchAir(PurchAirNum)%HtRecSenOutput,0.0d0)
      PurchAir(PurchAirNum)%HtRecSenCoolRate = ABS(MIN(PurchAir(PurchAirNum)%HtRecSenOutput,0.0d0))
      PurchAir(PurchAirNum)%HtRecLatHeatRate = MAX(PurchAir(PurchAirNum)%HtRecLatOutput,0.0d0)
      PurchAir(PurchAirNum)%HtRecLatCoolRate = ABS(MIN(PurchAir(PurchAirNum)%HtRecLatOutput,0.0d0))
      PurchAir(PurchAirNum)%HtRecTotHeatRate = PurchAir(PurchAirNum)%HtRecSenHeatRate+PurchAir(PurchAirNum)%HtRecLatHeatRate
      PurchAir(PurchAirNum)%HtRecTotCoolRate = PurchAir(PurchAirNum)%HtRecSenCoolRate+PurchAir(PurchAirNum)%HtRecLatCoolRate

      ReportingConstant = TimeStepSys*SecInHour

      PurchAir(PurchAirNum)%SenHeatEnergy = PurchAir(PurchAirNum)%SenHeatRate * ReportingConstant
      PurchAir(PurchAirNum)%SenCoolEnergy = PurchAir(PurchAirNum)%SenCoolRate * ReportingConstant
      PurchAir(PurchAirNum)%LatHeatEnergy = PurchAir(PurchAirNum)%LatHeatRate * ReportingConstant
      PurchAir(PurchAirNum)%LatCoolEnergy = PurchAir(PurchAirNum)%LatCoolRate * ReportingConstant
      PurchAir(PurchAirNum)%TotHeatEnergy = PurchAir(PurchAirNum)%TotHeatRate * ReportingConstant
      PurchAir(PurchAirNum)%TotCoolEnergy = PurchAir(PurchAirNum)%TotCoolRate * ReportingConstant

      PurchAir(PurchAirNum)%ZoneSenHeatEnergy = PurchAir(PurchAirNum)%ZoneSenHeatRate * ReportingConstant
      PurchAir(PurchAirNum)%ZoneSenCoolEnergy = PurchAir(PurchAirNum)%ZoneSenCoolRate * ReportingConstant
      PurchAir(PurchAirNum)%ZoneLatHeatEnergy = PurchAir(PurchAirNum)%ZoneLatHeatRate * ReportingConstant
      PurchAir(PurchAirNum)%ZoneLatCoolEnergy = PurchAir(PurchAirNum)%ZoneLatCoolRate * ReportingConstant
      PurchAir(PurchAirNum)%ZoneTotHeatEnergy = PurchAir(PurchAirNum)%ZoneTotHeatRate * ReportingConstant
      PurchAir(PurchAirNum)%ZoneTotCoolEnergy = PurchAir(PurchAirNum)%ZoneTotCoolRate * ReportingConstant

      PurchAir(PurchAirNum)%OASenHeatEnergy = PurchAir(PurchAirNum)%OASenHeatRate * ReportingConstant
      PurchAir(PurchAirNum)%OASenCoolEnergy = PurchAir(PurchAirNum)%OASenCoolRate * ReportingConstant
      PurchAir(PurchAirNum)%OALatHeatEnergy = PurchAir(PurchAirNum)%OALatHeatRate * ReportingConstant
      PurchAir(PurchAirNum)%OALatCoolEnergy = PurchAir(PurchAirNum)%OALatCoolRate * ReportingConstant
      PurchAir(PurchAirNum)%OATotHeatEnergy = PurchAir(PurchAirNum)%OATotHeatRate * ReportingConstant
      PurchAir(PurchAirNum)%OATotCoolEnergy = PurchAir(PurchAirNum)%OATotCoolRate * ReportingConstant

      PurchAir(PurchAirNum)%HtRecSenHeatEnergy = PurchAir(PurchAirNum)%HtRecSenHeatRate * ReportingConstant
      PurchAir(PurchAirNum)%HtRecSenCoolEnergy = PurchAir(PurchAirNum)%HtRecSenCoolRate * ReportingConstant
      PurchAir(PurchAirNum)%HtRecLatHeatEnergy = PurchAir(PurchAirNum)%HtRecLatHeatRate * ReportingConstant
      PurchAir(PurchAirNum)%HtRecLatCoolEnergy = PurchAir(PurchAirNum)%HtRecLatCoolRate * ReportingConstant
      PurchAir(PurchAirNum)%HtRecTotHeatEnergy = PurchAir(PurchAirNum)%HtRecTotHeatRate * ReportingConstant
      PurchAir(PurchAirNum)%HtRecTotCoolEnergy = PurchAir(PurchAirNum)%HtRecTotCoolRate * ReportingConstant

      RETURN
    END SUBROUTINE ReportPurchasedAir

FUNCTION GetPurchasedAirOutAirMassFlow(PurchAirNum) RESULT(OutAirMassFlow)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for OA inlet mass flow for ventilation rate reporting

          ! METHODOLOGY EMPLOYED:
          ! most analagous functions look up an outside air node but this function
          ! gets the actual mass flow of outdoor air, following the features of the model

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: PurchAirNum          !
  REAL(r64)             :: OutAirMassFlow

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (GetPurchAirInputFlag ) THEN
    CALL GetPurchasedAir
    GetPurchAirInputFlag = .FALSE.
  END IF

  OutAirMassFlow = PurchAir(PurchAirNum)%OutdoorAirMassFlowRate

  RETURN

END FUNCTION GetPurchasedAirOutAirMassFlow

INTEGER FUNCTION GetPurchasedAirZoneInletAirNode(PurchAirNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       Adapted for purchased air by M.J. Witte, Oct 2013
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for zone inlet node for ventilation rate reporting

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: PurchAirNum          !

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetPurchAirInputFlag ) THEN
    CALL GetPurchasedAir
    GetPurchAirInputFlag = .FALSE.
  END IF

  GetPurchasedAirZoneInletAirNode = 0
  IF (PurchAirNum > 0 .and. PurchAirNum <= NumPurchAir) THEN
    GetPurchasedAirZoneInletAirNode =  PurchAir(PurchAirNum)%ZoneSupplyAirNodeNum
  ENDIF

  RETURN

END FUNCTION GetPurchasedAirZoneInletAirNode


INTEGER FUNCTION GetPurchasedAirReturnAirNode(PurchAirNum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       Adapted for purchased air by M.J. Witte, Oct 2013
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for recirculation air node for ventilation rate reporting

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: PurchAirNum          !

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (GetPurchAirInputFlag ) THEN
    CALL GetPurchasedAir
    GetPurchAirInputFlag = .FALSE.
  END IF

  GetPurchasedAirReturnAirNode = 0
  IF (PurchAirNum > 0 .and. PurchAirNum <= NumPurchAir) THEN
    GetPurchasedAirReturnAirNode =  PurchAir(PurchAirNum)%ZoneRecircAirNodeNum
  ENDIF

  RETURN

END FUNCTION GetPurchasedAirReturnAirNode

FUNCTION GetPurchasedAirMixedAirTemp(PurchAirNum) RESULT(MixedAirTemp)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       Adapted for purchased air by M.J. Witte, Oct 2013
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for mixed air Temp for ventilation rate reporting

          ! METHODOLOGY EMPLOYED:
          ! most analagous functions look up an outside air node but this function
          ! gets the actual mass flow of outdoor air, following the features of the model

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: PurchAirNum          !
  REAL(r64)             :: MixedAirTemp

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (GetPurchAirInputFlag ) THEN
    CALL GetPurchasedAir
    GetPurchAirInputFlag = .FALSE.
  END IF

  MixedAirTemp = PurchAir(PurchAirNum)%FinalMixedAirTemp

  RETURN

END FUNCTION GetPurchasedAirMixedAirTemp

FUNCTION GetPurchasedAirMixedAirHumRat(PurchAirNum) RESULT(MixedAirHumRat)

          ! FUNCTION INFORMATION:
          !       AUTHOR         B Griffith
          !       DATE WRITTEN   Dec  2006
          !       MODIFIED       Adapted for purchased air by M.J. Witte, Oct 2013
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! lookup function for mixed air HumRat for ventilation rate reporting

          ! METHODOLOGY EMPLOYED:
          ! most analagous functions look up an outside air node but this function
          ! gets the actual mass flow of outdoor air, following the features of the model

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: PurchAirNum          !
  REAL(r64)             :: MixedAirHumRat

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (GetPurchAirInputFlag ) THEN
    CALL GetPurchasedAir
    GetPurchAirInputFlag = .FALSE.
  END IF

  MixedAirHumRat = PurchAir(PurchAirNum)%FinalMixedAirHumRat

  RETURN

END FUNCTION GetPurchasedAirMixedAirHumRat

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

END MODULE PurchasedAirManager


