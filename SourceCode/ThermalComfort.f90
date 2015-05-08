MODULE ThermalComfort

  ! Module containing the routines dealing with the CalcThermalComfortFanger,
  ! CalcThermalComfortPierce, and CalcThermalComfortKSU

  ! MODULE INFORMATION:
  !       AUTHOR         Jaewook Lee
  !       DATE WRITTEN   January 2000
  !       MODIFIED       Rick Strand (for E+ implementation February 2000)
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To calculate thermal comfort indices based on the
  ! three thermal comfort prediction models (Fanger, Pierce, KSU)

  ! METHODOLOGY EMPLOYED:
  ! For each thermal comfort model type, the subroutines will loop through
  ! the people statements and perform the requested thermal comfort evaluations

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals
USE DataHeatBalance,   ONLY: MRT, People, Zone, ZoneAveraged, SurfaceWeighted, AngleFactor, TotPeople
USE DataEnvironment,   ONLY: OutBaroPress, OutDryBulbTemp
USE DataHeatBalFanSys, ONLY: MAT, ZTAV, ZoneAirHumRat,ZoneComfortControlsFanger, ZoneAirHumRatAvg, &
                             ZTAVComf, ZoneAirHumRatAvgComf
USE ScheduleManager,   ONLY: GetCurrentScheduleValue
USE DataRoomAirModel,  ONLY: IsZoneDV, TCMF, IsZoneCV, ZTREC,ZTJET,Ujet,Urec,ZoneUCSDCV, IsZoneUI,   &
                             VComfort_Jet, VComfort_Recirculation
USE DataInterfaces

  !Use statements for access to subroutines in other modules
USE Psychrometrics, ONlY:PsyRhFnTdbWPb

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
REAL(r64), PARAMETER :: TAbsConv = KelvinConv      ! Converter for absolute temperature
REAL(r64), PARAMETER :: ActLevelConv = 58.2d0    ! Converter for activity level (1Met = 58.2 W/m2)
REAL(r64), PARAMETER :: BodySurfArea = 1.8d0     ! Dubois body surface area of the human body (m2)
REAL(r64), PARAMETER :: RadSurfEff = 0.72d0      ! Fraction of surface effective for radiation
REAL(r64), PARAMETER :: StefanBoltz = 5.67d-8  ! Stefan-Boltzmann constant (W/m2K4)

  ! DERIVED TYPE DEFINITIONS
TYPE, PUBLIC :: ThermalComfortDataType
  REAL(r64) :: FangerPMV = 0.0d0
  REAL(r64) :: FangerPPD = 0.0d0
  REAL(r64) :: CloSurfTemp = 0.0d0  ! clothing surface temp from iteration in FANGER calcs
  REAL(r64) :: PiercePMVET = 0.0d0
  REAL(r64) :: PiercePMVSET = 0.0d0
  REAL(r64) :: PierceDISC = 0.0d0
  REAL(r64) :: PierceTSENS = 0.0d0
  REAL(r64) :: KsuTSV = 0.0d0
  REAL(r64) :: ThermalComfortMRT = 0.0d0
  REAL(r64) :: ThermalComfortOpTemp = 0.0d0
  REAL(r64) :: ClothingValue = 0.0d0
  INTEGER :: ThermalComfortAdaptiveASH5590 = 0
  INTEGER :: ThermalComfortAdaptiveASH5580 = 0
  INTEGER :: ThermalComfortAdaptiveCEN15251CatI = 0
  INTEGER :: ThermalComfortAdaptiveCEN15251CatII = 0
  INTEGER :: ThermalComfortAdaptiveCEN15251CatIII = 0
  REAL(r64) :: TComfASH55 = 0.0d0
  REAL(r64) :: TComfCEN15251 = 0.0d0
  REAL(r64) :: ASHRAE55RunningMeanOutdoorTemp = 0.0d0
  REAL(r64) :: CEN15251RunningMeanOutdoorTemp = 0.0d0
END TYPE

TYPE ThermalComfortInASH55Type
  ! for debugging
  !REAL(r64)    :: dCurAirTemp
  !REAL(r64)    :: dCurMeanRadiantTemp
  !REAL(r64)    :: dOperTemp
  !REAL(r64)    :: dHumidRatio

  REAL(r64)    :: timeNotSummer = 0.0d0 !time when not in summer comfort range based on ASHRAE 55 simplified
  REAL(r64)    :: timeNotWinter = 0.0d0 !time when not in winter comfort range based on ASHRAE 55 simplified
  REAL(r64)    :: timeNotEither = 0.0d0 !time when  not in summer or winter comfort range based on ASHRAE 55 simplified
  REAL(r64)    :: totalTimeNotSummer = 0.0d0 !sum for simulation for summer
  REAL(r64)    :: totalTimeNotWinter = 0.0d0 !sum for simulation for winter
  REAL(r64)    :: totalTimeNotEither = 0.0d0 !sum for simulation for either
  LOGICAL :: ZoneIsOccupied = .false. !flag if zone has people
  INTEGER :: warningIndex =  0   !variable to store pointer to the recurring warning
  INTEGER :: warningIndex2 =  0   !variable to store pointer to the recurring warning
  LOGICAL :: Enable55Warning = .false. !flag if the warning should be able to be shown if appropriate
END TYPE

TYPE ThermalComfortSetpointType
  REAL(r64)    :: notMetHeating = 0.0d0
  REAL(r64)    :: notMetCooling = 0.0d0
  REAL(r64)    :: notMetHeatingOccupied = 0.0d0
  REAL(r64)    :: notMetCoolingOccupied = 0.0d0
  REAL(r64)    :: totalNotMetHeating = 0.0d0
  REAL(r64)    :: totalNotMetCooling = 0.0d0
  REAL(r64)    :: totalNotMetHeatingOccupied = 0.0d0
  REAL(r64)    :: totalNotMetCoolingOccupied = 0.0d0
END TYPE

TYPE AngleFactorData
  REAL(r64), ALLOCATABLE, DIMENSION(:)    :: AngleFactor         ! Angle factor of each surface
  CHARACTER(len=MaxNameLength)   :: Name = ' '                ! Angle factor list name
  CHARACTER(len=MaxNameLength), &
      ALLOCATABLE, DIMENSION(:)      :: SurfaceName         ! Names of the Surfces
  INTEGER, ALLOCATABLE, DIMENSION(:) :: SurfacePtr          ! ALLOCATABLE to the names of the Surfces
  INTEGER                        :: TotAngleFacSurfaces = 0 ! Total number of surfaces
  CHARACTER(len=MaxNameLength)   :: ZoneName= ' '            ! Name of zone the system is serving
  INTEGER                        :: ZonePtr= 0             ! Point to this zone in the Zone derived type
END TYPE

TYPE(ThermalComfortInASH55Type), DIMENSION(:), ALLOCATABLE :: ThermalComfortInASH55
TYPE(ThermalComfortSetpointType), DIMENSION(:), ALLOCATABLE :: ThermalComfortSetpoint
TYPE(ThermalComfortDataType), DIMENSION(:), ALLOCATABLE, PUBLIC :: ThermalComfortData
TYPE (AngleFactorData), DIMENSION(:), ALLOCATABLE :: AngleFactorList   ! Angle Factor List data for each Angle Factor List
  ! MODULE VARIABLE DECLARATIONS:
REAL(r64) :: AbsAirTemp        = 0.0d0 ! Absolute air temperature; K
REAL(r64) :: AbsCloSurfTemp    = 0.0d0 ! Absolute clothing surface temperature; K
REAL(r64) :: AbsRadTemp        = 0.0d0 ! Absolute radiant temperature; K
REAL(r64) :: AcclPattern       = 0.0d0 ! The pattern of acclimation
REAL(r64) :: ActLevel          = 0.0d0 ! Metabolic rate; w/m2
REAL(r64) :: AirVel            = 0.0d0 ! Air velocity; m/s
REAL(r64) :: AirTemp           = 0.0d0 ! Air temperature; C
REAL(r64) :: CloBodyRat        = 0.0d0 ! Ratio of clothed body
REAL(r64) :: CloInsul          = 0.0d0 ! Clothing insulation
REAL(r64) :: CloPermeatEff     = 0.0d0 ! Clothing permeation efficiency
REAL(r64) :: CloSurfTemp       = 0.0d0 ! Clothing surface temperature; K
REAL(r64) :: CloThermEff       = 0.0d0 ! The Burton thermal efficiency factor for clothing
REAL(r64) :: CloUnit           = 0.0d0 ! Clothing unit; CLO
REAL(r64) :: ConvHeatLoss      = 0.0d0 ! Convective heat loss
REAL(r64) :: CoreTempChange    = 0.0d0 ! Temperature change of core in 1 minute
REAL(r64) :: CoreTemp          = 0.0d0 ! Body core temperature
REAL(r64) :: CoreTempNeut      = 0.0d0 ! Body core temperature of neutral state
REAL(r64) :: CoreThermCap      = 0.0d0 ! Thermal capacity of core
REAL(r64) :: DryHeatLoss       = 0.0d0 ! Heat loss from clothing surface due to both convection and radiation
REAL(r64) :: DryRespHeatLoss   = 0.0d0 ! Dry respiration heat loss
REAL(r64) :: EvapHeatLoss      = 0.0d0 ! Evaporative heat loss from skin
REAL(r64) :: EvapHeatLossDiff  = 0.0d0 ! Evaporative heat loss due to moisture diffusion through skin
REAL(r64) :: EvapHeatLossMax   = 0.0d0 ! Maximum evaporative heat loss
REAL(r64) :: EvapHeatLossRegComf   = 0.0d0 ! Evaporative heat loss due to regulatory sweating at the state of comfort
REAL(r64) :: EvapHeatLossRegSweat  = 0.0d0 ! Evaporative heat loss from regulatory sweating
REAL(r64) :: EvapHeatLossSweat     = 0.0d0 ! Evaporative heat loss from the sweat secreted
REAL(r64) :: EvapHeatLossSweatPrev = 0.0d0 ! Old value of evaporative heat loss from the sweat secreted (KSU)
REAL(r64) :: H                 = 0.0d0 ! Combined heat transfer coefficient
REAL(r64) :: Hc                = 0.0d0 ! Convective heat transfer coeffiency
REAL(r64) :: HcFor             = 0.0d0 ! Convective heat transfer coeffiency - Forced
REAL(r64) :: HcNat             = 0.0d0 ! Convective heat transfer coeffiency - Natural
REAL(r64) :: HeatFlow          = 0.0d0 ! Heat flow from core to skin
REAL(r64) :: Hr                = 0.0d0 ! Radiant heat transfer coeffiency
REAL(r64) :: IntHeatProd       = 0.0d0 ! Internal heat production
INTEGER :: IterNum        = 0   ! Number of iteration
REAL(r64) :: LatRespHeatLoss   = 0.0d0 ! Latent respiration heat loss
INTEGER :: MaxZoneNum     = 0   ! Number of zones
INTEGER :: MRTCalcType    = 0   ! The type of MRT calculation (ZoneAveraged or SurfaceWeighted)
REAL(r64) :: OpTemp            = 0.0d0 ! Operative temperature
INTEGER :: PeopleNum      = 0   ! People number
REAL(r64) :: RadHeatLoss       = 0.0d0 ! Radiant heat loss
REAL(r64) :: RadTemp           = 0.0d0 ! Radiant temperature; C
REAL(r64) :: RelHum            = 0.0d0 ! Relative humidity; Fraction
REAL(r64) :: RespHeatLoss      = 0.0d0 ! The rate of respiratory heat loss
REAL(r64) :: SatSkinVapPress   = 0.0d0 ! Saturated vapor pressure at skin temperature
REAL(r64) :: ShivResponse      = 0.0d0 ! Metalbolic heat production due to shivering
REAL(r64) :: SkinComfTemp      = 0.0d0 ! Skin temperature required to achieve thermal comfort; C
REAL(r64) :: SkinComfVPress    = 0.0d0 ! Saturated water vapor pressure at required skin temperature; Torr
REAL(r64) :: SkinTemp          = 0.0d0 ! Skin temperature
REAL(r64) :: SkinTempChange    = 0.0d0 ! Temperature change of skin in 1 minute
REAL(r64) :: SkinTempNeut      = 0.0d0 ! Skin temperature at neutral state
REAL(r64) :: SkinThermCap      = 0.0d0 ! Thermal capacity of Skin
REAL(r64) :: SkinWetDiff       = 0.0d0 ! Skin wettedness for nonsweating portion of skin
REAL(r64) :: SkinWetSweat      = 0.0d0 ! Skin wettedness required to evaporate regulatory sweat
REAL(r64) :: SkinWetTot        = 0.0d0 ! Total skin wettedness
REAL(r64) :: SkinVapPress      = 0.0d0 ! Vapor pressure at skin
REAL(r64) :: SurfaceTemp       = 0.0d0 ! Surface temperature when MRTType is 'SurfaceWeighted'
REAL(r64) :: ThermCndct        = 0.0d0 ! Thermal conductance of skin
REAL(r64) :: ThermSensTransCoef = 0.0d0 ! Theraml sensation coefficient for PMV
REAL(r64) :: Time              = 0.0d0 ! Time, hr
REAL(r64) :: TimeChange        = 0.0d0 ! Change of time, hr
REAL(r64) :: VapPress          = 0.0d0 ! Vapor pressure; Torr  ?? BG Oct 2005 humm, this should be kPa
REAL(r64) :: VasoconstrictFac  = 0.0d0 ! Constriction factor of blood vessel
REAL(r64) :: VasodilationFac   = 0.0d0 ! Dilation factor of blood vessel
REAL(r64) :: WorkEff           = 0.0d0 ! Energy cosumption by external work; w/m2
INTEGER :: ZoneNum        = 0   ! Zone number
REAL(r64) :: TemporarySixAMTemperature           = 0.0d0 ! Temperature at 6am

!time that any zone is not comfortable based on simple ASHRAE 55 using summer clothes
REAL(r64) :: AnyZoneTimeNotSimpleASH55Summer = 0.0d0
!time that any zone is not comfortable based on simple ASHRAE 55 using winter clothes
REAL(r64) :: AnyZoneTimeNotSimpleASH55Winter = 0.0d0
!time that any zone is not comfortable based on simple ASHRAE 55 using summer or winter clothes
REAL(r64) :: AnyZoneTimeNotSimpleASH55Either = 0.0d0

!time that any zone has unmet met loads
REAL(r64)    :: AnyZoneNotMetHeating = 0.0d0
REAL(r64)    :: AnyZoneNotMetCooling = 0.0d0
REAL(r64)    :: AnyZoneNotMetHeatingOccupied = 0.0d0
REAL(r64)    :: AnyZoneNotMetCoolingOccupied = 0.0d0
REAL(r64)    :: AnyZoneNotMetOccupied = 0.0d0
!total time from beginning of simulation AnyZoneTimeNotSimpleASH55
REAL(r64) :: TotalAnyZoneTimeNotSimpleASH55Summer = 0.0d0
REAL(r64) :: TotalAnyZoneTimeNotSimpleASH55Winter = 0.0d0
REAL(r64) :: TotalAnyZoneTimeNotSimpleASH55Either = 0.0d0
!total time from beginning of simulation any zone not met
REAL(r64)    :: TotalAnyZoneNotMetHeating = 0.0d0
REAL(r64)    :: TotalAnyZoneNotMetCooling = 0.0d0
REAL(r64)    :: TotalAnyZoneNotMetHeatingOccupied = 0.0d0
REAL(r64)    :: TotalAnyZoneNotMetCoolingOccupied = 0.0d0
REAL(r64)    :: TotalAnyZoneNotMetOccupied = 0.0d0
REAL(r64),ALLOCATABLE,DIMENSION(:) :: ZoneOccHrs



! Subroutine Specifications for the Thermal Comfort module
PUBLIC  ManageThermalComfort
PRIVATE InitThermalComfort
PUBLIC  CalcThermalComfortFanger
PRIVATE CalcThermalComfortPierce
PRIVATE CalcThermalComfortKSU
PRIVATE CalcThermalComfortSimpleASH55
PUBLIC  CalcThermalComfortAdaptiveASH55
PUBLIC  CalcThermalComfortAdaptiveCEN15251
PRIVATE CalcIfSetpointMet
PRIVATE DERIV
PRIVATE RKG
PRIVATE CalcSatVapPressFromTemp
PRIVATE CalcRadTemp
PRIVATE GetAngleFactorList
PRIVATE CalcAngleFactorMRT
PRIVATE DynamicClothingModel

CONTAINS

SUBROUTINE ManageThermalComfort(InitializeOnly)

          ! SUBROUTINE INFORMATION:
          !     AUTHOR         Rick Strand
          !     DATE WRITTEN   February 2000
          !     MODIFIED       na
          !     RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages the various thermal comfort calculations.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus manager methodology.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN) :: InitializeOnly   ! when called from ZTPC and calculations aren't needed

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE :: FirstTimeFlag = .TRUE. ! Flag set to make sure you get input once
  LOGICAL,SAVE :: ASH55Flag = .false.
  LOGICAL,SAVE :: CEN15251Flag = .false.

          ! FLOW:
  ! No input to get because this is already done by other heat balance routines

  IF (FirstTimeFlag) THEN
    CALL InitThermalComfort ! Mainly sets up output stuff
    FirstTimeFlag = .FALSE.
    IF (TotPeople > 0) THEN
      IF (ANY(People%AdaptiveASH55)) ASH55Flag  = .true.
      IF (ANY(People%AdaptiveCEN15251)) CEN15251Flag = .true.
    ENDIF
  END IF

 IF (DayOfSim == 1) THEN
  IF (HourOfDay < 7) THEN
  TemporarySixAMTemperature = 1.868132
  ELSE IF (HourOfDay == 7) THEN
    IF (TimeStep == 1) THEN
      TemporarySixAMTemperature = OutDryBulbTemp
    END IF
  END IF
 ELSE
  IF (HourOfDay == 7) THEN
     IF (TimeStep == 1) THEN
        TemporarySixAMTemperature = OutDryBulbTemp
     END IF
   END IF
 END IF

  IF (InitializeOnly) RETURN

  IF (BeginEnvrnFlag) THEN
    ZoneOccHrs=0.0d0
  ENDIF

  IF (.not. DoingSizing .and. .not. WarmupFlag) THEN
    CALL CalcThermalComfortFanger
    CALL CalcThermalComfortPierce
    CALL CalcThermalComfortKSU
    CALL CalcThermalComfortSimpleASH55
    CALL CalcIfSetpointMet
    IF(ASH55Flag) CALL CalcThermalComfortAdaptiveASH55(.false.)
    IF(CEN15251Flag) CALL CalcThermalComfortAdaptiveCEN15251(.false.)
  ENDIF

  ! No updating needed

  ! No other reporting needed

  RETURN

END SUBROUTINE ManageThermalComfort

SUBROUTINE InitThermalComfort

          ! SUBROUTINE INFORMATION:
          !     AUTHOR         Rick Strand
          !     DATE WRITTEN   February 2000
          !     MODIFIED       na
          !     RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine allocates the proper arrays, sets all values to zero,
          ! and sets up the output stuff.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus manager methodology.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

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
  INTEGER :: Loop   ! DO loop counter
  CHARACTER(len=MaxNameLength) :: CurrentGroupName

          ! FLOW:

  ALLOCATE (ThermalComfortData(TotPeople))

  DO Loop = 1, TotPeople

    CurrentGroupName = People(Loop)%Name

    ! CurrentModuleObject='People'
    IF (People(Loop)%Fanger) THEN
      CALL SetupOutputVariable('Zone Thermal Comfort Fanger Model PMV []',ThermalComfortData(Loop)%FangerPMV, &
                               'Zone','State',People(Loop)%Name)
      CALL SetupOutputVariable('Zone Thermal Comfort Fanger Model PPD [%]',ThermalComfortData(Loop)%FangerPPD, &
                               'Zone','State',People(Loop)%Name)
      CALL SetupOutputVariable('Zone Thermal Comfort Clothing Surface Temperature [C]',ThermalComfortData(Loop)%CloSurfTemp, &
                               'Zone','State',People(Loop)%Name)
    END IF

    IF (People(Loop)%Pierce) THEN
      CALL SetupOutputVariable('Zone Thermal Comfort Pierce Model Effective Temperature PMV []', &
                                ThermalComfortData(Loop)%PiercePMVET, &
                               'Zone','State',People(Loop)%Name)
      CALL SetupOutputVariable('Zone Thermal Comfort Pierce Model Standard Effective Temperature PMV []', &
                                ThermalComfortData(Loop)%PiercePMVSET, &
                               'Zone','State',People(Loop)%Name)
      CALL SetupOutputVariable('Zone Thermal Comfort Pierce Model Discomfort Index []',ThermalComfortData(Loop)%PierceDISC, &
                               'Zone','State',People(Loop)%Name)
      CALL SetupOutputVariable('Zone Thermal Comfort Pierce Model Thermal Sensation Index []', &
                                ThermalComfortData(Loop)%PierceTSENS, &
                               'Zone','State',People(Loop)%Name)
    END IF

    IF (People(Loop)%Ksu) THEN
      CALL SetupOutputVariable('Zone Thermal Comfort KSU Model Thermal Sensation Vote []',ThermalComfortData(Loop)%KsuTSV, &
                               'Zone','State',People(Loop)%Name)
    END IF

    IF ((People(Loop)%Fanger).OR.(People(Loop)%Pierce).OR.(People(Loop)%Ksu)) THEN
      CALL SetupOutputVariable('Zone Thermal Comfort Mean Radiant Temperature [C]',ThermalComfortData(Loop)%ThermalComfortMRT, &
                              'Zone','State',People(Loop)%Name)
      CALL SetupOutputVariable('Zone Thermal Comfort Operative Temperature [C]',ThermalComfortData(Loop)%ThermalComfortOpTemp, &
                              'Zone','State',People(Loop)%Name)
      CALL SetupOutputVariable('Zone Thermal Comfort Clothing Value [clo]',ThermalComfortData(Loop)%ClothingValue, &
                              'Zone','State',People(Loop)%Name)
    END IF

    IF (People(Loop)%AdaptiveASH55) THEN
      CALL SetupOutputVariable('Zone Thermal Comfort ASHRAE 55 Adaptive Model 90% Acceptability Status []', &
                               ThermalComfortData(Loop)%ThermalComfortAdaptiveASH5590, &
                               'Zone','State',People(Loop)%Name)
      CALL SetupOutputVariable('Zone Thermal Comfort ASHRAE 55 Adaptive Model 80% Acceptability Status []', &
                               ThermalComfortData(Loop)%ThermalComfortAdaptiveASH5580, &
                               'Zone','State',People(Loop)%Name)
      CALL SetupOutputVariable('Zone Thermal Comfort ASHRAE 55 Adaptive Model Running Average Outdoor Air Temperature [C]', &
                               ThermalComfortData(Loop)%ASHRAE55RunningMeanOutdoorTemp, &
                               'Zone','State',People(Loop)%Name)
      CALL SetupOutputVariable('Zone Thermal Comfort ASHRAE 55 Adaptive Model Temperature [C]', &
                               ThermalComfortData(Loop)%TComfASH55, &
                               'Zone','State',People(Loop)%Name)
    END IF

    IF (People(Loop)%AdaptiveCEN15251) THEN
      CALL SetupOutputVariable('Zone Thermal Comfort CEN 15251 Adaptive Model Category I Status []', &
                               ThermalComfortData(Loop)%ThermalComfortAdaptiveCEN15251CatI, &
                               'Zone','State',People(Loop)%Name)
      CALL SetupOutputVariable('Zone Thermal Comfort CEN 15251 Adaptive Model Category II Status []', &
                               ThermalComfortData(Loop)%ThermalComfortAdaptiveCEN15251CatII, &
                               'Zone','State',People(Loop)%Name)
      CALL SetupOutputVariable('Zone Thermal Comfort CEN 15251 Adaptive Model Category III Status []', &
                               ThermalComfortData(Loop)%ThermalComfortAdaptiveCEN15251CatIII, &
                               'Zone','State',People(Loop)%Name)
      CALL SetupOutputVariable('Zone Thermal Comfort CEN 15251 Adaptive Model Running Average Outdoor Air Temperature [C]', &
                               ThermalComfortData(Loop)%CEN15251RunningMeanOutdoorTemp, &
                               'Zone','State',People(Loop)%Name)
      CALL SetupOutputVariable('Zone Thermal Comfort CEN 15251 Adaptive Model Temperature [C]', &
                               ThermalComfortData(Loop)%TComfCEN15251, &
                               'Zone','State',People(Loop)%Name)
    END IF


  END DO
  ALLOCATE (ThermalComfortInASH55(NumOfZones))

  ! ASHRAE 55 Warning. If any people statement for a zone is true, set that zone to true
  DO Loop = 1, TotPeople
    IF (People(Loop)%Show55Warning) THEN
      ThermalComfortInASH55(People(Loop)%ZonePtr)%Enable55Warning =  .TRUE.
    END IF
  END DO

  ! CurrentModuleObject='Zone'
  DO Loop = 1, NumOfZones
    CALL SetupOutputVariable('Zone Thermal Comfort ASHRAE 55 Simple Model Summer Clothes Not Comfortable Time [hr]', &
                              ThermalComfortInASH55(Loop)%timeNotSummer, &
                              'Zone','Sum',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Thermal Comfort ASHRAE 55 Simple Model Winter Clothes Not Comfortable Time [hr]', &
                              ThermalComfortInASH55(Loop)%timeNotWinter, &
                              'Zone','Sum',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Thermal Comfort ASHRAE 55 Simple Model Summer or Winter Clothes Not Comfortable Time [hr]', &
                              ThermalComfortInASH55(Loop)%timeNotEither, &
                              'Zone','Sum',Zone(Loop)%Name)
  END DO
  CALL SetupOutputVariable('Facility Thermal Comfort ASHRAE 55 Simple Model Summer Clothes Not Comfortable Time [hr]', &
                            AnyZoneTimeNotSimpleASH55Summer, &
                              'Zone','Sum','Facility')
  CALL SetupOutputVariable('Facility Thermal Comfort ASHRAE 55 Simple Model Winter Clothes Not Comfortable Time [hr]', &
                             AnyZoneTimeNotSimpleASH55Winter, &
                              'Zone','Sum','Facility')
  CALL SetupOutputVariable('Facility Thermal Comfort ASHRAE 55 Simple Model Summer or Winter Clothes Not Comfortable Time [hr]', &
                             AnyZoneTimeNotSimpleASH55Either, &
                              'Zone','Sum','Facility')

  ALLOCATE (ThermalComfortSetpoint(NumOfZones))
  DO Loop = 1, NumOfZones
    CALL SetupOutputVariable('Zone Heating Setpoint Not Met Time [hr]',ThermalComfortSetpoint(Loop)%notMetHeating, &
                              'Zone','Sum',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Heating Setpoint Not Met While Occupied Time [hr]',  &
                              ThermalComfortSetpoint(Loop)%notMetHeatingOccupied, &
                              'Zone','Sum',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Cooling Setpoint Not Met Time [hr]',ThermalComfortSetpoint(Loop)%notMetCooling, &
                              'Zone','Sum',Zone(Loop)%Name)
    CALL SetupOutputVariable('Zone Cooling Setpoint Not Met While Occupied Time [hr]',  &
                              ThermalComfortSetpoint(Loop)%notMetCoolingOccupied, &
                              'Zone','Sum',Zone(Loop)%Name)
  END DO

  CALL SetupOutputVariable('Facility Heating Setpoint Not Met Time [hr]',AnyZoneNotMetHeating, &
                              'Zone','Sum','Facility')
  CALL SetupOutputVariable('Facility Cooling Setpoint Not Met Time [hr]',AnyZoneNotMetCooling, &
                              'Zone','Sum','Facility')
  CALL SetupOutputVariable('Facility Heating Setpoint Not Met While Occupied Time [hr]',AnyZoneNotMetHeatingOccupied, &
                              'Zone','Sum','Facility')
  CALL SetupOutputVariable('Facility Cooling Setpoint Not Met While Occupied Time [hr]',AnyZoneNotMetCoolingOccupied, &
                              'Zone','Sum','Facility')

  CALL GetAngleFactorList

  ALLOCATE(ZoneOccHrs(NumOfZones))
  ZoneOccHrs=0.0d0

  RETURN

END SUBROUTINE InitThermalComfort

SUBROUTINE CalcThermalComfortFanger(PNum,Tset,PMVResult)

          ! SUBROUTINE INFORMATION:
          !     AUTHOR         Jaewook Lee
          !     DATE WRITTEN   January 2000
          !     MODIFIED       Rick Strand (for E+ implementation February 2000)
          !                    Brent Griffith modifications for CR 5641 (October 2005)
          !                    L. Gu, Added optional arguments for thermal comfort control (May 2006)
          !                    T. Hong, added Fanger PPD (April 2009)
          !     RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates PMV(Predicted Mean Vote) using the Fanger thermal
          ! comfort model. This subroutine is also used for thermal comfort control by determining
          ! the temperature at which the PMV is equal to a PMV setpoint specified by the user.

          ! METHODOLOGY EMPLOYED:
          ! This subroutine is based heavily upon the work performed by Dan Maloney for
          ! the BLAST program.  Many of the equations are based on the original Fanger
          ! development.  See documentation for further details and references.

          ! REFERENCES:
          ! Maloney, Dan, M.S. Thesis, University of Illinois at Urbana-Champaign
          !
          ! BG note (10/21/2005),  This formulation is based on the the BASIC program
          ! that is included in ASHRAE Standard 55 Normative Appendix D.
          !

          ! USE STATEMENTS:
  USE Psychrometrics, ONLY:PsyPsatFnTemp

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          INTEGER, INTENT(IN), OPTIONAL :: PNum      ! People number for thermal comfort control
          REAL(r64), INTENT(IN), OPTIONAL    :: Tset      ! Temperature setpoint for thermal comfort control
          REAL(r64), INTENT(OUT), OPTIONAL   :: PMVResult ! PMV value for thermal comfort control

          ! SUBROUTINE PARAMETER DEFINITIONS:
    INTEGER, PARAMETER :: MaxIter = 150       ! Limit of iteration
    REAL(r64), PARAMETER :: StopIterCrit = 0.00015d0 ! Stop criteria for iteration
    REAL(r64), PARAMETER :: SkinEmiss = 0.97d0       ! Emissivity of clothing-skin surface
          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

    REAL(r64) :: P1   ! Intermediate variables to calculate clothed body ratio and clothing temperature
    REAL(r64) :: P2   ! Intermediate variables to calculate clothed body ratio and clothing temperature
    REAL(r64) :: P3   ! Intermediate variables to calculate clothed body ratio and clothing temperature
    REAL(r64) :: P4   ! Intermediate variables to calculate clothed body ratio and clothing temperature
    REAL(r64) :: XF   ! Intermediate variables to calculate clothed body ratio and clothing temperature
    REAL(r64) :: XN   ! Intermediate variables to calculate clothed body ratio and clothing temperature
    REAL(r64) :: IntermediateClothing
!    REAL(r64) :: SkinTempComf        ! Skin temperature required to achieve thermal comfort; C

    REAL(r64) :: PMV  ! temporary variable to store calculated Fanger PMV value
    REAL(r64) :: PPD  ! temporary variable to store calculated Fanger PPD value

      DO PeopleNum = 1, TotPeople

          ! Optional argument is used to access people object when thermal comfort control is used
          If (PRESENT(PNum)) then
            If (PeopleNum .NE. PNum) Cycle
          End If

          ! If optional argument is used do not cycle regardless of thermal comfort reporting type
          IF((.NOT. People(PeopleNum)%Fanger) .AND. (.Not. PRESENT(PNum))) CYCLE

          ZoneNum = People(PeopleNum)%ZonePtr
          IF (IsZoneDV(ZoneNum) .or. IsZoneUI(ZoneNum)) THEN
              AirTemp = TCMF(ZoneNum)               !PH 3/7/04
          ! UCSD-CV
          ELSEIF (IsZoneCV(ZoneNum)) THEN
              IF (ZoneUCSDCV(ZoneNum)%VforComfort == VComfort_Jet) THEN
                AirTemp = ZTJET(ZoneNum)
              ELSEIF (ZoneUCSDCV(ZoneNum)%VforComfort== VComfort_Recirculation) THEN
                 AirTemp = ZTJET(ZoneNum)
              ELSE
                ! Thermal comfort control uses Tset to determine PMV setpoint value, otherwise use zone temp
                If (PRESENT(PNum)) then
                  AirTemp = Tset
                Else
                  AirTemp = ZTAV(ZoneNum)
                End If
              ENDIF
          ELSE
            If (PRESENT(PNum)) then
              AirTemp = Tset
            Else
              AirTemp = ZTAVComf(ZoneNum)
            End If
          ENDIF
          RadTemp = CalcRadTemp(PeopleNum)
          ! Use mean air temp for calculating RH when thermal comfort control is used
          If (PRESENT(PNum)) then
            RelHum = PsyRhFnTdbWPb(MAT(ZoneNum),ZoneAirHumRat(ZoneNum),OutBaroPress)
          Else
            RelHum = PsyRhFnTdbWPb(ZTAVComf(ZoneNum),ZoneAirHumRatAvgComf(ZoneNum),OutBaroPress)
          End If
          People(PeopleNum)%TemperatureInZone =  AirTemp
          People(PeopleNum)%RelativeHumidityInZone =  RelHum * 100.0d0

          ! Metabolic rate of body (W/m2)
          ActLevel = GetCurrentScheduleValue(People(PeopleNum)%ActivityLevelPtr)/BodySurfArea
          ! Energy consumption by external work (W/m2)
          WorkEff = GetCurrentScheduleValue(People(PeopleNum)%WorkEffPtr)*ActLevel
          ! Clothing unit
          SELECT CASE (People(PeopleNum)%ClothingType)
            CASE (1)
              CloUnit = GetCurrentScheduleValue(People(PeopleNum)%ClothingPtr)
            CASE (2)
              ThermalComfortData(PeopleNum)%ThermalComfortOpTemp = (RadTemp+AirTemp)/2.0d0
              ThermalComfortData(PeopleNum)%ClothingValue = CloUnit
              CALL DynamicClothingModel
              CloUnit = ThermalComfortData(PeopleNum)%ClothingValue
            CASE (3)
              IntermediateClothing = GetCurrentScheduleValue(People(PeopleNum)%ClothingMethodPtr)
              IF(IntermediateClothing .EQ. 1.0d0) THEN
                CloUnit = GetCurrentScheduleValue(People(PeopleNum)%ClothingPtr)
                ThermalComfortData(PeopleNum)%ClothingValue = CloUnit
              ELSE IF(IntermediateClothing .EQ. 2.0d0) THEN
                ThermalComfortData(PeopleNum)%ThermalComfortOpTemp = (RadTemp+AirTemp)/2.0d0
                ThermalComfortData(PeopleNum)%ClothingValue = CloUnit
                CALL DynamicClothingModel
                CloUnit = ThermalComfortData(PeopleNum)%ClothingValue
              ELSE
                CloUnit = GetCurrentScheduleValue(People(PeopleNum)%ClothingPtr)
                CALL ShowWarningError('PEOPLE="'//TRIM(People(PeopleNum)%Name)// &
                     '", Scheduled clothing value will be used rather than clothing calculation method.')
              ENDIF
            CASE DEFAULT
              CALL ShowSevereError('PEOPLE="'//TRIM(People(PeopleNum)%Name)// &
                        '", Incorrect Clothing Type')
          END SELECT

          IF (IsZoneCV(ZoneNum)) THEN
            IF (ZoneUCSDCV(ZoneNum)%VforComfort == VComfort_Jet) THEN
                AirVel = Ujet(ZoneNum)
              ELSEIF (ZoneUCSDCV(ZoneNum)%VforComfort== VComfort_Recirculation) THEN
                 AirVel = Urec(ZoneNum)
              ELSE
                 AirVel = 0.2d0
              ENDIF
          ELSE
            AirVel = GetCurrentScheduleValue(People(PeopleNum)%AirVelocityPtr)
            ! Ensure air velocity within the reasonable range. Otherwise reccusive warnings is provided
            If (PRESENT(PNum) .AND. (AirVel < 0.1d0 .OR. AirVel > 0.5d0)) then
              if (People(PeopleNum)%AirVelErrIndex == 0) then
                CALL ShowWarningMessage('PEOPLE="'//TRIM(People(PeopleNum)%Name)// &
                     '", Air velocity is beyond the reasonable range (0.1,0.5) for thermal comfort control.')
                CALL ShowContinueErrorTimeStamp(' ')
              end if
              CALL ShowRecurringWarningErrorAtEnd('PEOPLE="'//TRIM(People(PeopleNum)%Name)//  &
                '",Air velocity is still beyond the reasonable range (0.1,0.5)', &
                People(PeopleNum)%AirVelErrIndex, ReportMinOf=AirVel,ReportMinUnits='[m/s]',   &
                ReportMaxOf=AirVel,ReportMaxUnits='[m/s]')
            End If

          ENDIF

          ! VapPress    = CalcSatVapPressFromTemp(AirTemp)  !original
          ! VapPress    = RelHum*VapPress                   !original might be in torrs

          VapPress    = PsyPsatFnTemp(AirTemp) ! use psych routines inside E+ , returns Pa

          VapPress    = RelHum*VapPress ! in units of [Pa]

          IntHeatProd = ActLevel - WorkEff

          ! Compute the Corresponding Clothed Body Ratio
          CloBodyRat = 1.05d0 + 0.1d0*CloUnit ! The ratio of the surface area of the clothed body
                                          ! to the surface area of nude body

          IF(CloUnit < 0.5d0) CloBodyRat = CloBodyRat - 0.05d0 + 0.1d0*CloUnit

          AbsRadTemp = RadTemp + TAbsConv
          AbsAirTemp = AirTemp + TAbsConv

          CloInsul = CloUnit*CloBodyRat*0.155d0 ! Thermal resistance of the clothing

          P2 = CloInsul*3.96d0
          P3 = CloInsul*100.d0
          P1 = CloInsul*AbsAirTemp
          P4 = 308.7d0 - 0.028d0*IntHeatProd + P2*(AbsRadTemp/100.d0)**4

          ! First guess for clothed surface tempeature
          AbsCloSurfTemp = AbsAirTemp + (35.5d0-AirTemp)/(3.5d0*(CloUnit + 0.1d0))
          XN = AbsCloSurfTemp/100.d0
          HcFor = 12.1d0*SQRT(AirVel) ! Heat transfer coefficient by forced convection
          IterNum = 0
          XF = XN

          ! COMPUTE SURFACE TEMPERATURE OF CLOTHING BY ITERATIONS
            DO WHILE (( (ABS(XN - XF) > StopIterCrit) .OR. (IterNum == 0) ) &
                                                  .AND. (IterNum < MaxIter))
              XF = (XF + XN)/2.d0
              HcNat = 2.38d0*ABS(100.*XF - AbsAirTemp)**0.25d0 ! Heat transfer coefficient by natural convection
              Hc = MAX(HcFor, HcNat) ! Determination of convective heat transfer coefficient
              XN = (P4+P1*Hc - P2*XF**4)/(100.d0 + P3*Hc)
              IterNum = IterNum + 1
              IF (IterNum > MaxIter) THEN
                CALL ShowWarningError('Max iteration exceeded in CalcThermalFanger')
              END IF
            END DO
          AbsCloSurfTemp = 100.d0*XN
          CloSurfTemp = AbsCloSurfTemp - TAbsConv

          ! COMPUTE PREDICTED MEAN VOTE
          ! Sensible heat loss
          ! RadHeatLoss = RadSurfEff*CloBodyRat*SkinEmiss*StefanBoltz* &   !original
          !                            (AbsCloSurfTemp**4 - AbsRadTemp**4) ! Heat loss by radiation

          ! following line is ln 480 in ASHRAE 55 append. D
          RadHeatLoss = 3.96d0*CloBodyRat*((AbsCloSurfTemp/100.d0)**4.0d0 - (AbsRadTemp/100.d0)**4.0d0)

          ConvHeatLoss = CloBodyRat*Hc*(CloSurfTemp - AirTemp) ! Heat loss by convection

          DryHeatLoss = RadHeatLoss + ConvHeatLoss

          ! Evaporative heat loss
          ! Heat loss by regulatory sweating
          EvapHeatLossRegComf = 0.0d0
          IF (IntHeatProd > 58.2d0) THEN
            EvapHeatLossRegComf = 0.42d0*(IntHeatProd - ActLevelConv)
          END IF
          ! SkinTempComf = 35.7 - 0.028*IntHeatProd ! Skin temperature required to achieve thermal comfort
          ! SatSkinVapPress = 1.92*SkinTempComf - 25.3 ! Water vapor pressure at required skin temperature
          ! Heat loss by diffusion
          ! EvapHeatLossDiff = 0.4148*(SatSkinVapPress - VapPress) !original
          EvapHeatLossDiff = 3.05d0 *0.001d0*(5733.d0 -6.99d0*IntHeatProd-VapPress) ! ln 440 in ASHRAE 55 Append. D

          EvapHeatLoss = EvapHeatLossRegComf + EvapHeatLossDiff
          ! Heat loss by respiration
          ! original: LatRespHeatLoss = 0.0023*ActLevel*(44. - VapPress) ! Heat loss by latent respiration
          LatRespHeatLoss = 1.7d0 * 0.00001d0 * ActLevel * (5867.d0 - VapPress) !ln 460 in ASHRAE 55 Append. D

         ! LatRespHeatLoss = 0.017251*ActLevel*(5.8662 - VapPress)
             ! V-1.2.2 'fix' BG 3/2005 5th term in LHS Eq (58)  in 2001 HOF Ch. 8
             ! this was wrong because VapPress needed to be kPa

          DryRespHeatLoss = 0.0014d0*ActLevel*(34.d0- AirTemp) ! Heat loss by dry respiration.

          RespHeatLoss = LatRespHeatLoss + DryRespHeatLoss

          ThermSensTransCoef = 0.303d0*EXP(-0.036d0*ActLevel) + 0.028d0 ! Thermal transfer coefficient to calculate PMV

          PMV = ThermSensTransCoef*(IntHeatProd - EvapHeatLoss - RespHeatLoss - DryHeatLoss)

          ThermalComfortData(PeopleNum)%FangerPMV = PMV

          ! Pass resulting PMV based on temperature setpoint (Tset) when using thermal comfort control
          If (PRESENT(PNum)) then
            PMVResult = PMV
          End If
          ThermalComfortData(PeopleNum)%ThermalComfortMRT = RadTemp
          ThermalComfortData(PeopleNum)%ThermalComfortOpTemp = (RadTemp+AirTemp)/2.0d0
          ThermalComfortData(PeopleNum)%CloSurfTemp       = CloSurfTemp

          ! Calculate the Fanger PPD (Predicted Percentage of Dissatisfied), as a %
          PPD = 100.0d0 - 95.0d0*EXP(-0.03353d0*PMV**4 - 0.2179d0*PMV**2)
          IF (PPD < 0.0d0 ) PPD = 0.0d0
          IF (PPD > 100.0d0 ) PPD = 100.0d0

          ThermalComfortData(PeopleNum)%FangerPPD = PPD
  END DO

  RETURN

END SUBROUTINE CalcThermalComfortFanger

SUBROUTINE CalcThermalComfortPierce

          ! SUBROUTINE INFORMATION:
          !     AUTHOR         Jaewook Lee
          !     DATE WRITTEN   January 2000
          !     MODIFIED       Rick Strand (for E+ implementation February 2000)
          !     RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates PMVET, PMVSET, DISC, and TSENS using the Pierce
          ! 2 Node model.

          ! METHODOLOGY EMPLOYED:
          ! This subroutine is based heavily upon the work performed by Dan Maloney for
          ! the BLAST program.  Many of the equations are based on the original Pierce
          ! development.  See documentation for further details and references.

          ! REFERENCES:
          ! Maloney, Dan, M.S. Thesis, University of Illinois at Urbana-Champaign

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: CloFac = 0.25d0             ! Clothing factor determined experimentally
  REAL(r64), PARAMETER :: EvapEff = 0.9d0             ! Evaporative efficiency
  REAL(r64), PARAMETER :: MaxSkinBloodFlow = 90.d0    ! Max. value of skin blood flow
  REAL(r64), PARAMETER :: RegSweatMax = 670.d0        ! Max. value of regulatory sweating; w/m2
  REAL(r64), PARAMETER :: SkinBloodFlowConst = 200.d0 ! Skin blood flow coefficient for average person; l/m2.hr.k
  REAL(r64), PARAMETER :: STdAtm = 1.d0               ! Standard Atmospheres
  REAL(r64), PARAMETER :: Str = 0.1d0                 ! Constriction constant of skin blood flow for average person
  REAL(r64), PARAMETER :: SweatContConst = 170.d0     ! Proportionality constant for sweat control; g/m2.hr
  REAL(r64), PARAMETER :: VapPressConv = 0.1333227d0 ! Vapor pressure converter from torr to Kpa

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64) :: AirEvapHeatResist    ! Evaporative heat resistance of air
  REAL(r64) :: ActMet               ! Metalbolic rate in MET
  REAL(r64) :: ActLevelStart        ! Activity level at the start of the minute-by-minute iterations
  REAL(r64) :: AvgBodyTemp          ! Average body temperature
  REAL(r64) :: AvgBodyTempHigh      ! Average body temperature when HSI(Belding's classic heat sterss index) is 100
  REAL(r64) :: AvgBodyTempLow       ! Average body temperature when DISC is 0
  REAL(r64) :: AvgBodyTempSet       ! Setpoint for average body temperature
  REAL(r64) :: BodyThermSigCold     ! Temperature difference of Body when BodyTempSet is higher than BodyTemp
  REAL(r64) :: BodyTempChange       ! Temperature change of body in 1 minute
  REAL(r64) :: BodyThermSigWarm     ! Temperature difference of Body when BodyTemp is higher than BodyTempSet
  REAL(r64) :: CloCond              ! The conductance of the clothing
  REAL(r64) :: CloEvapHeatResist    ! Evaporative heat resistance of clothing
  REAL(r64) :: CloSurfTempOld       ! Old value of clothing surface temperature
  REAL(r64) :: CoreThermSigCold     ! Temperature difference of core when CoreTempSet is higher than CoreTemp
  REAL(r64) :: CoreHeatStorage      ! Heat storage in core compartment
  REAL(r64) :: CoreTempSet          ! Setpoint for body core temperature
  REAL(r64) :: CoreThermSigWarm     ! Temperature difference of core when CoreTemp is higher than CoreTempSet
  REAL(r64) :: DryHeatLossET        ! Heat loss from clothing surface due to both convection and radiation at ET
  REAL(r64) :: DryHeatLossSET       ! Heat loss from clothing surface due to both convection and radiation at SET
  REAL(r64) :: EffectCloThermEff    ! Effective clothing thermal efficiency
  REAL(r64) :: EffectCloUnit        ! Effective clothing unit; clo
  REAL(r64) :: EnergyBalErrET       ! Stop criterion for iteration to solve energy balance
  REAL(r64) :: EnergyBalErrSET      ! Stop criterion for iteration to solve energy balance
  REAL(r64) :: ET                   ! Effective temperature
  REAL(r64) :: EvapHeatLossStart    ! Starting value of evaporative heat loss
  LOGICAL :: FirstMinIter
  REAL(r64) :: HcAct                ! Convective heat transfer coefficient at high activity
  REAL(r64) :: HcStd                ! Standard convective heat transfer coefficient
  REAL(r64) :: HrStd                ! Standard radiant heat transfer coefficient
  REAL(r64) :: HStd                 ! Standard combined heat transfer coefficient
  INTEGER :: IterMin                ! Time period for the ieterative calculation
  REAL(r64) :: LewisRat             ! Lewis ratio
  REAL(r64) :: RegSweat             ! The rate of regulatory sweating
  REAL(r64) :: SET                  ! Standard effective temperature
  REAL(r64) :: SkinBloodFlow        ! The skin blood flow
  REAL(r64) :: SkinThermSigCold     ! Temperature difference of skin when SkinTempSet is higher than SkinTemp
  REAL(r64) :: SkinHeatLoss         ! Heat loss from skin
  REAL(r64) :: SkinHeatStorage      ! Heat storage in skin compartment
  REAL(r64) :: SkinMassRat          ! Actual skin mass to total body mass ratio
  REAL(r64) :: SkinMassRatSet       ! Setpoint for skin mass to total body mass ratio
  REAL(r64) :: SkinRelHum           ! Relative humidity at skin
  REAL(r64) :: SkinTempSet          ! Setpoint for skin temperature
  REAL(r64) :: SkinThermSigWarm     ! Temperature difference of skin when SkinTemp is higher than SkinTempSet
  REAL(r64) :: StdCloBodyRat        ! Standard ratio of clothed body
  REAL(r64) :: StdCloFac            ! Clothing factor determined experimentally at standard environment
  REAL(r64) :: StdCloPermeatEff     ! Standard clothing permeation efficiency
  REAL(r64) :: StdCloUnit           ! standard clothing unit
  REAL(r64) :: StdEffectCloThermEff ! Standard effective clothing theraml efficiency
  REAL(r64) :: StdEffectCloUnit     ! standard effective clothing unit
  REAL(r64) :: StdVapPressET        ! Standard vapor pressure at effective temperature
  REAL(r64) :: StdVapPressSET       ! Standard vapor pressure at standar effective temperature
  REAL(r64) :: TotEvapHeatResist    ! Total evaporative heat resistance
  REAL(r64) :: UnevapSweat          ! Unevaporated sweat; g/m2/hr
  REAL(r64) :: IntermediateClothing

        ! FLOW:

  DO PeopleNum = 1, TotPeople

    IF(.NOT. People(PeopleNum)%Pierce) CYCLE

    ZoneNum = People(PeopleNum)%ZonePtr
    IF (IsZoneDV(ZoneNum) .or. IsZoneUI(ZoneNum)) THEN
        AirTemp = TCMF(ZoneNum)               !PH 3/7/04
    ELSE
        AirTemp = ZTAV(ZoneNum)
    ENDIF
    RadTemp = CalcRadTemp(PeopleNum)
    RelHum  = PsyRhFnTdbWPb(ZTAV(ZoneNum),ZoneAirHumRat(ZoneNum),OutBaroPress)
    ! Metabolic rate of body (W/m2)
    ActLevel = GetCurrentScheduleValue(People(PeopleNum)%ActivityLevelPtr)/BodySurfArea
    ! Energy consumption by external work (W/m2)
    WorkEff = GetCurrentScheduleValue(People(PeopleNum)%WorkEffPtr)*ActLevel
    ! Clothing unit
      SELECT CASE (People(PeopleNum)%ClothingType)
        CASE (1)
          CloUnit = GetCurrentScheduleValue(People(PeopleNum)%ClothingPtr)
        CASE (2)
          ThermalComfortData(PeopleNum)%ThermalComfortOpTemp = (RadTemp+AirTemp)/2.0d0
          ThermalComfortData(PeopleNum)%ClothingValue = CloUnit
          CALL DynamicClothingModel
          CloUnit = ThermalComfortData(PeopleNum)%ClothingValue
        CASE (3)
          IntermediateClothing = GetCurrentScheduleValue(People(PeopleNum)%ClothingMethodPtr)
          IF(IntermediateClothing .EQ. 1.0d0) THEN
            CloUnit = GetCurrentScheduleValue(People(PeopleNum)%ClothingPtr)
            ThermalComfortData(PeopleNum)%ClothingValue = CloUnit
          ELSE IF(IntermediateClothing .EQ. 2.0d0) THEN
            ThermalComfortData(PeopleNum)%ThermalComfortOpTemp = (RadTemp+AirTemp)/2.0d0
            ThermalComfortData(PeopleNum)%ClothingValue = CloUnit
            CALL DynamicClothingModel
            CloUnit = ThermalComfortData(PeopleNum)%ClothingValue
          ELSE
            CloUnit = GetCurrentScheduleValue(People(PeopleNum)%ClothingPtr)
            CALL ShowWarningError('Scheduled clothing value will be used rather than clothing calculation method.')
          ENDIF
        CASE DEFAULT
          CALL ShowSevereError('Incorrect Clothing Type')
      END SELECT

    AirVel  = GetCurrentScheduleValue(People(PeopleNum)%AirVelocityPtr)

    VapPress = CalcSatVapPressFromTemp(AirTemp)
    VapPress = RelHum*VapPress
    VapPress = VapPress*VapPressConv ! Torr to KPa (5.8662 kPa=44 mmHg; .017251=.0023*760 mmHg/101.325 kPa)
    IntHeatProd = ActLevel - WorkEff
    ActMet = ActLevel/ActLevelConv
    ! CALCULATE VARIABLESS THAT REMAIN CONSTANT FOR AN HOUR
    CloBodyRat = 1.0d0 + CloFac*CloUnit

    IF(CloUnit < .01d0) CloUnit=.01d0

    CloCond = 1.d0/(CloUnit*0.155d0)

    ! INITIALIZE THE POLLOWING VARIABLES
    IF(AirVel < .137d0) AirVel = .137d0

    Hc = 8.6d0*AirVel**0.53d0
    IF(ActMet > .9d0) THEN
      HcAct = 5.66d0*(ActMet - 0.85d0)**0.39d0
      Hc = MAX(HcAct, Hc)
    ENDIF

    ! Definition of vascular control signals
    ! CoreTempSet, SkinTempSet, and AvgBodyTempSet are the setpoints for core, skin and
    ! average body temperatures corresponding to physiol.  neutrality
    ! SkinMassRatSet is the ratio of skin mass to total body mass (skin+core)
    ! Typical values for CoreTempSet, SkinTempSet and SkinMassRatSet are 36.8, 33.7 and 0.10
    ! SkinMassRat is the actual skin to total body mass ratio
    SkinTempSet = 33.7d0
    CoreTempSet = 36.8d0
    SkinMassRatSet = 0.10d0
    AvgBodyTempSet = SkinMassRatSet*SkinTempSet + (1.d0-SkinMassRatSet)*CoreTempSet

    ! APPROXIMATE THE FOLLOWING VALUES TO START
    SkinTemp = 33.7d0
    CoreTemp = 36.8d0
    SkinBloodFlow = 6.3d0
    EvapHeatLossStart = 5.0d0
    LatRespHeatLoss = 0.017251d0*ActLevel*(5.8662d0 - VapPress)
    EvapHeatLoss = (EvapHeatLossStart - LatRespHeatLoss)
    SkinMassRat = 0.0417737d0 + 0.7451832d0/(SkinBloodFlow + 0.585417d0)

    ! GUESS CloSurfTemp TO START
    CloSurfTemp = (SkinTemp + AirTemp)/2.d0

    ! SIMULATION OF TEMPERATURE REGULATION.
    ! This SECTION simulates the temperature regulation over 1 minute.
    ! Inputs are the physiological data from the previous time step and
    ! the current environmental conditions.

    ! BEGIN MINUTE BY MINUTE CALCULATIONS FOR ONE HOUR
    ActLevelStart = ActLevel    ! ActLevel gets increased by shivering in the following DO
                                ! loop and must be increased from the start level, not
                                ! perpetually increased
    DO IterMin = 1, 60

      ! Dry heat balance:  solve  for CloSurfTemp and Hr
      FirstMinIter = .TRUE.
      CloSurfTempOld=0.0d0
      DO WHILE ((ABS(CloSurfTemp-CloSurfTempOld) > 0.01d0) .OR. FirstMinIter)
        FirstMinIter = .FALSE.
        CloSurfTempOld = CloSurfTemp
        Hr = 4.d0*RadSurfEff*StefanBoltz*((CloSurfTemp + RadTemp)/2.d0 + TAbsConv)**3
        CloSurfTemp = (CloCond*SkinTemp + CloBodyRat*(Hc*AirTemp + Hr*RadTemp))/(CloCond + CloBodyRat*(Hc + Hr))
      END DO

      ! CALCULATE THE COMBINED HEAT TRANSFER COEFF. (H)
      H = Hr+Hc
      ! Heat flow from Clothing surface to environment
      DryHeatLoss = CloBodyRat*(Hc*(CloSurfTemp - AirTemp) + Hr*(CloSurfTemp - RadTemp))
      ! dry and latent respiratory heat losses
      LatRespHeatLoss = 0.017251d0*ActLevel*(5.8662d0 - VapPress)
      DryRespHeatLoss = 0.0014d0*ActLevel*(34.d0 - AirTemp)*StdAtm
      RespHeatLoss = LatRespHeatLoss + DryRespHeatLoss
      ! Heat flows to skin and core:
      HeatFlow = (CoreTemp-SkinTemp)*(5.28d0 + 1.163d0*SkinBloodFlow)
      ! 5.28 is skin conductance in the
      ! absence of skin blood flow
      SkinHeatStorage = HeatFlow - DryHeatLoss - EvapHeatLoss
      CoreHeatStorage = ActLevel - (CoreTemp - SkinTemp)*(5.28d0+1.163d0*SkinBloodFlow) - &
                        RespHeatLoss - WorkEff

      ! Thermal capacities (average man: 70 kg, 1.8 square meter).
      CoreThermCap = ActLevelConv*(1.d0 - SkinMassRat)*70.d0
      SkinThermCap = ActLevelConv*SkinMassRat*70.d0

      ! Temperature changes in 1 minute
      SkinTempChange = (SkinHeatStorage*1.8d0)/SkinThermCap
      CoreTempChange = (CoreHeatStorage*1.8d0)/CoreThermCap
      BodyTempChange = SkinMassRat*SkinTempChange + (1.d0 - SkinMassRat)*CoreTempChange
      SkinTemp    = SkinTemp + SkinTempChange
      CoreTemp    = CoreTemp + CoreTempChange
      AvgBodyTemp = SkinMassRat*SkinTemp + (1.d0 - SkinMassRat)*CoreTemp

      IF(SkinTemp > SkinTempSet) THEN
        SkinThermSigWarm = SkinTemp - SkinTempSet
        SkinThermSigCold = 0.d0
      ELSE
        SkinThermSigCold = SkinTempSet - SkinTemp
        SkinThermSigWarm = 0.d0
      END IF

      IF(CoreTemp > CoreTempSet) THEN
        CoreThermSigWarm = CoreTemp - CoreTempSet
        CoreThermSigCold = 0.d0
      ELSE
        CoreThermSigCold = CoreTempSet - CoreTemp
        CoreThermSigWarm = 0.d0
      END IF

      IF(AvgBodyTemp > AvgBodyTempSet) THEN
        BodyThermSigWarm = AvgBodyTemp - AvgBodyTempSet
        BodyThermSigCold = 0.d0
      ELSE
        BodyThermSigCold = AvgBodyTempSet-AvgBodyTemp
        BodyThermSigWarm = 0.d0
      END IF

      VasodilationFac = SkinBloodFlowConst*CoreThermSigWarm
      VasoconstrictFac = Str*SkinThermSigCold
      SkinBloodFlow = (6.3d0 + VasodilationFac)/(1.d0 + VasoconstrictFac)

      ! SkinBloodFlow is never below 0.5 liter/(m2.hr) nor above MaxSkinBloodFlow
      IF(SkinBloodFlow < 0.5d0) SkinBloodFlow = 0.5d0
      IF(SkinBloodFlow > MaxSkinBloodFlow) SkinBloodFlow = MaxSkinBloodFlow

      ! ratio of skin-core masses change with SkinBloodFlow
      ! (SkinMassRat,SkinBloodFlow) = (.15,6.3),(.45,1.24),(.05,90)
      SkinMassRat = 0.0417737d0 + 0.7451832d0/(SkinBloodFlow + 0.585417d0)

      ! control of regulatory sweating
      RegSweat = SweatContConst*BodyThermSigWarm*EXP(SkinThermSigWarm/10.7d0)

      IF(RegSweat > RegSweatMax) RegSweat = RegSweatMax

      EvapHeatLossRegSweat = 0.68d0*RegSweat

      ! adjustment of metabolic heat due to shivering (Stolwijk, Hardy)
      ShivResponse = 19.4d0*SkinThermSigCold*CoreThermSigCold
      ActLevel = ActLevelStart + ShivResponse

      ! Evaluation of heat transfer by evaporation at skin surface
      ! LewisRat varies with SkinTemp.
      ! LewisRat=2.02 C/mmHg or 15.1512 C/kPa at 0 C (lr=2.2 at 25 C)
      LewisRat = 15.1512d0*(SkinTemp + TAbsConv)/TAbsConv

      ! Mass transfer equation between skin and environment
      ! TotEvapHeatResist is total vapor resistance of CloUnitthing + air layer
      ! CloInsul is efficiency of mass transfer for CloUnitthing
      ! CloInsul IS SET TO .45 (FOR WOVEN MATERIAL)
      ! Reference:  Woodcock, Breckenridge and Goldman
      CloInsul = 0.45d0
      CloThermEff = 1.d0/(1.d0 + 0.155d0*CloBodyRat*H*CloUnit)

      AirEvapHeatResist = 1.d0/(LewisRat*CloBodyRat*Hc)
      CloEvapHeatResist = 0.155d0*CloUnit/(LewisRat*CloInsul)
      TotEvapHeatResist = AirEvapHeatResist + CloEvapHeatResist

      SatSkinVapPress = CalcSatVapPressFromTemp(SkinTemp)
      SatSkinVapPress = SatSkinVapPress*0.1333227d0
      EvapHeatLossMax = (1.d0/TotEvapHeatResist)*(SatSkinVapPress - VapPress)
      SkinWetSweat = EvapHeatLossRegSweat/EvapHeatLossMax

      ! 0.06 if SkinWetDiff for nonsweating skin --- Kerslake
      SkinWetDiff = (1.d0-SkinWetSweat)*.06d0
      EvapHeatLossDiff = SkinWetDiff*EvapHeatLossMax
      EvapHeatLoss = EvapHeatLossRegSweat + EvapHeatLossDiff
      SkinWetTot = EvapHeatLoss/EvapHeatLossMax

      ! Beginning of dripping (Sweat not evaporated on skin surface)
      IF((SkinWetTot >= EvapEff).AND.(EvapHeatLossMax >= 0)) THEN
        SkinWetTot = EvapEff
        SkinWetSweat = (EvapEff - 0.06d0)/.94d0
        EvapHeatLossRegSweat = SkinWetSweat*EvapHeatLossMax
        SkinWetDiff = (1.d0 - SkinWetSweat)*.06d0
        EvapHeatLossDiff = SkinWetDiff*EvapHeatLossMax
        EvapHeatLoss = EvapHeatLossRegSweat + EvapHeatLossDiff
      END IF

      ! When EvapHeatLossMax<0. condensation on skin occurs.
      IF(EvapHeatLossMax <= 0.0d0) THEN
        SkinWetDiff = 0.0d0
        EvapHeatLossDiff = 0.0d0
        EvapHeatLoss = EvapHeatLossMax
        SkinWetTot = EvapEff
        SkinWetSweat = EvapEff
        EvapHeatLossRegSweat = 0.0d0
      END IF

      ! UnevapSweat = unevaporated sweat in grams/sq.m/hr
      UnevapSweat = (RegSweat*.68d0 - SkinWetSweat*EvapHeatLossMax)/0.68d0
      IF(UnevapSweat <= 0.0d0) UnevapSweat=0.0d0

      ! Vapor pressure at skin (as measured by dewpoint sensors)
      SkinVapPress=SkinWetTot*SatSkinVapPress + (1.d0 - SkinWetTot)*VapPress

      ! SkinRelHum is skin relative humidity
      SkinRelHum = SkinVapPress/SatSkinVapPress

    END DO ! END OF MINUTE BY MINUTE TEMPERATURE REGULATION LOOP

    ! Computation of comfort indices.
    ! Inputs to this SECTION are the physiological data from the simulation of
    ! temperature regulation loop

    ! PART I: Heat transfer indices in real environment
    OpTemp = (Hr*RadTemp + Hc*AirTemp)/H
    EffectCloUnit = CloUnit - (CloBodyRat-1.d0)/(.155d0*CloBodyRat*H)
    EffectCloThermEff = 1.d0/(1.d0 + .155d0*H*EffectCloUnit)
    CloPermeatEff = 1.d0/(1.d0 + (.155d0/CloInsul)*Hc*EffectCloUnit)

    ! PART II: ET*(standardization humidity/REAL(r64) CloUnit, StdAtm and Hc)
    ! calculation of skin heat Loss (SkinHeatLoss)
    SkinHeatLoss = H*EffectCloThermEff*(SkinTemp - OpTemp) + &
                   SkinWetTot*LewisRat*Hc*CloPermeatEff*(SatSkinVapPress - VapPress)
    ! Get a low approximation for ET* and solve balance
    ! equation by iteration
    ET = SkinTemp - SkinHeatLoss/(H*EffectCloThermEff)
    ! THE STANDARD VAPOR PRESSURE AT THE EFFECTIVE TEMP : StdVapPressET

    DO
      StdVapPressET = CalcSatVapPressFromTemp(ET)
      StdVapPressET = StdVapPressET*VapPressConv
      EnergyBalErrET = SkinHeatLoss - H*EffectCloThermEff*(SkinTemp - ET) - &
                       SkinWetTot*LewisRat*Hc*CloPermeatEff*(SatSkinVapPress - StdVapPressET/2.0d0)
      IF (EnergyBalErrET >= 0.0d0) EXIT
      ET = ET + 0.1d0
    END DO

    ! Part III: Standard effective temperature SET*
    ! standardized humidity.  Hc, CloUnit, StdAtm
    ! normalized for given ActLeAirVelivity

    ! Standard environment
    HrStd = Hr
    ! HcStd = standard conv. heat tr. coeff. (level walking/still air)
    IF(ActMet <= 0.86d0) ActMet = 0.86d0
    HcStd = 5.66d0*(ActMet - 0.85d0)**0.39d0

    ! minimum value of Hc at sea leAirVel = 3.0 (AirVel = .137 m/s)
    IF(HcStd <= 3.d0) HcStd = 3.d0

    ! standard MET - StdCloUnit relation gives SET* = 24 C when PMV = 0
    StdCloUnit = 1.3264d0/((ActLevel-WorkEff)/ActLevelConv + 0.7383d0) - 0.0953d0
    StdCloFac = CloFac
    StdCloBodyRat = 1.d0 + StdCloFac*StdCloUnit
    HStd = HrStd + HcStd
    StdEffectCloUnit = StdCloUnit - (StdCloBodyRat - 1.d0)/(0.155d0*StdCloBodyRat*HStd)
    StdEffectCloThermEff = 1.d0/(1.d0 + 0.155d0*HStd*StdEffectCloUnit)
    StdCloPermeatEff = 1.d0/(1.d0+(0.155d0/.45d0)*HcStd*StdEffectCloUnit)

    ! Get a low approximation for SET*
    ! and solve balance equ. by iteration
    SET = SkinTemp - SkinHeatLoss/(HStd*StdEffectCloThermEff)

    DO
      StdVapPressSET = CalcSatVapPressFromTemp(SET)
      StdVapPressSET = StdVapPressSET*VapPressConv
      EnergyBalErrSET = SkinHeatLoss - HStd*StdEffectCloThermEff*(SkinTemp - SET) - &
                        SkinWetTot*LewisRat*HcStd*StdCloPermeatEff*(SatSkinVapPress - StdVapPressSET/2.0d0)
      IF (EnergyBalErrSET >= 0.0d0) EXIT
      SET = SET + 0.1d0
    END DO

    ! Part IV:  Fanger's comfort equation.
    ! Thermal transfer coefficient to calculate PMV
    ThermSensTransCoef = 0.303d0*EXP(-0.036d0*ActLevel) + 0.028d0
    ! Fanger's reg. sweating at comfort threshold (PMV=0) is:
    EvapHeatLossRegComf = (IntHeatProd - ActLevelConv)*0.42d0

    ! PMV*(PMVET in prgm) uses ET instead of OpTemp
    DryHeatLossET = HStd*StdEffectCloThermEff*(SkinTemp - ET)
    ThermalComfortData(PeopleNum)%PiercePMVET = ThermSensTransCoef*(IntHeatProd - RespHeatLoss - &
                                                DryHeatLossET - EvapHeatLossDiff - EvapHeatLossRegComf)

    ! SPMV*(PMVSET in prgm) uses SET instead of OpTemp
    DryHeatLossSET = HStd*StdEffectCloThermEff*(SkinTemp - SET)
    ThermalComfortData(PeopleNum)%PiercePMVSET = ThermSensTransCoef*(IntHeatProd - RespHeatLoss - &
                                                 DryHeatLossSET - EvapHeatLossDiff - EvapHeatLossRegComf)

    ! Part V:  Heat stress and heat strain indices derived from EvapHeatLoss,
    ! EvapHeatLossMax and W (skin wettedness)

    ! EvapHeatLossMax is readjusted for EvapEff
    EvapHeatLossMax = EvapHeatLossMax*EvapEff
    ! DISC (discomfort) varies with relative thermoregulatory strain
    ThermalComfortData(PeopleNum)%PierceDISC = 5.d0*(EvapHeatLossRegSweat - EvapHeatLossRegComf)/(EvapHeatLossMax - &
                                               EvapHeatLossRegComf - EvapHeatLossDiff)

    ! Part VI:  Thermal sensation TSENS as function of mean body temp.-
    ! AvgBodyTempLow is AvgBodyTemp when DISC is 0. (lower limit of zone of evap. regul.)
    AvgBodyTempLow = (0.185d0/ActLevelConv)*(ActLevel - WorkEff) + 36.313d0
    ! AvgBodyTempHigh is AvgBodyTemp when HSI=100 (upper limit of zone of evap. regul.)
    AvgBodyTempHigh = (0.359d0/ActLevelConv)*(ActLevel - WorkEff) + 36.664d0

    ! TSENS=DISC=4.7 when HSI =1 00 (HSI is Belding's classic heat stress index)
    ! In cold, DISC &TSENS are the same and neg. fct of AvgBodyTemp
    IF(AvgBodyTemp > AvgBodyTempLow) THEN
      ThermalComfortData(PeopleNum)%PierceTSENS = 4.7d0*(AvgBodyTemp - AvgBodyTempLow)/(AvgBodyTempHigh - &
                                                  AvgBodyTempLow)

    ELSE
      ThermalComfortData(PeopleNum)%PierceTSENS = .68175d0*(AvgBodyTemp - AvgBodyTempLow)
      ThermalComfortData(PeopleNum)%PierceDISC = ThermalComfortData(PeopleNum)%PierceTSENS
    END IF

    ThermalComfortData(PeopleNum)%ThermalComfortMRT = RadTemp
    ThermalComfortData(PeopleNum)%ThermalComfortOpTemp = (RadTemp+AirTemp)/2.0d0

  END DO

  RETURN

END SUBROUTINE CalcThermalComfortPierce

SUBROUTINE CalcThermalComfortKSU

          ! SUBROUTINE INFORMATION:
          !     AUTHOR         Jaewook Lee
          !     DATE WRITTEN   January 2000
          !     MODIFIED       Rick Strand (for E+ implementation February 2000)
          !     RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates TSV using the KSU 2 Node model.
          ! METHODOLOGY EMPLOYED:
          ! This subroutine is based heavily upon the work performed by Dan Maloney for
          ! the BLAST program.  Many of the equations are based on the original Pierce
          ! development.  See documentation for further details and references.

          ! REFERENCES:
          ! Maloney, Dan, M.S. Thesis, University of Illinois at Urbana-Champaign

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: CloEmiss = 0.8d0             ! Clothing Emissivity

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64) :: BodyWt                ! Weight of body, kg
  REAL(r64), DIMENSION(2) :: Coeff   ! Coefficients used in Range-Kutta's Method
  REAL(r64) :: DayNum                ! Number of days of acclimation
  INTEGER :: NumDay             ! Loop counter for DayNum
  REAL(r64) :: EmissAvg              ! Average emissivity
  INTEGER :: IncreDayNum        ! Number of days of increment in the outputs as desired
  REAL(r64) :: IntHeatProdMet        ! Internal heat production in MET
  REAL(r64) :: IntHeatProdMetMax     ! Maximum value of internal heat production in MET
  INTEGER :: LastDayNum         ! Number of days for the last print out
  REAL(r64) :: SkinWetFac            ! Skin wettedness factor
  REAL(r64) :: SkinWetNeut           ! Skin wettedness at neutral state
  INTEGER :: StartDayNum        ! Number of days for the first print out
                                ! Unacclimated man = 1, Acclimated man = 14
  REAL(r64) :: SweatSuppFac          ! Sweat suppression factor due to skin wettedness
  REAL(r64), DIMENSION(2) :: Temp       ! Temperature
  REAL(r64), DIMENSION(2) :: TempChange ! Change of temperature
  REAL(r64) :: TempDiffer            ! Temperature difference between the rectal and esophageal temperatures
                                ! If not measured, set it to be 0.5 Deg. C.
  INTEGER :: TempIndiceNum      ! Number of temperature indices
  REAL(r64) :: ThermCndctMin         ! Minimum value of thermal conductance
  REAL(r64) :: ThermCndctNeut        ! Thermal conductance at neutral state
  REAL(r64) :: TimeExpos             ! Time period in the exposure, hr
  REAL(r64) :: TimeInterval          ! Time interval of outputs desired, hr
  REAL(r64) :: TSVMax                ! Maximum value of thermal sensation vote
  REAL(r64) :: IntermediateClothing

! FLOW:

  TempIndiceNum = 2

! NEXT GROUP OF VARIABLE ARE FIXED FOR BLAST PROGRAM - UNACCLIMATED MAN
! THE TSV MODEL CAN BE APPLIED TO UNACCLIMATED MAN ONLY.
  TimeInterval = 1.d0
  TSVMax = 4.d0
  StartDayNum = 1
  LastDayNum = 1
  IncreDayNum = 1
  TimeExpos = 1.d0
  TempDiffer = 0.5d0

  DO PeopleNum = 1, TotPeople
! THE NEXT SIX VARIABLES WILL BE READ IN FROM INPUT DECK
    IF(.NOT. People(PeopleNum)%KSU) CYCLE

    ZoneNum = People(PeopleNum)%ZonePtr
    IF (IsZoneDV(ZoneNum) .or. IsZoneUI(ZoneNum)) THEN
        AirTemp = TCMF(ZoneNum)               !PH 3/7/04
    ELSE
        AirTemp = ZTAV(ZoneNum)
    ENDIF
    RadTemp = CalcRadTemp(PeopleNum)
    RelHum  = PsyRhFnTdbWPb(ZTAV(ZoneNum),ZoneAirHumRat(ZoneNum),OutBaroPress)
    ActLevel = GetCurrentScheduleValue(People(PeopleNum)%ActivityLevelPtr)/BodySurfArea
    WorkEff = GetCurrentScheduleValue(People(PeopleNum)%WorkEffPtr)*ActLevel
    SELECT CASE (People(PeopleNum)%ClothingType)
      CASE (1)
        CloUnit = GetCurrentScheduleValue(People(PeopleNum)%ClothingPtr)
      CASE (2)
        ThermalComfortData(PeopleNum)%ThermalComfortOpTemp = (RadTemp+AirTemp)/2.0d0
        ThermalComfortData(PeopleNum)%ClothingValue = CloUnit
        CALL DynamicClothingModel
        CloUnit = ThermalComfortData(PeopleNum)%ClothingValue
      CASE (3)
        IntermediateClothing = GetCurrentScheduleValue(People(PeopleNum)%ClothingMethodPtr)
        IF(IntermediateClothing .EQ. 1.0d0) THEN
          CloUnit = GetCurrentScheduleValue(People(PeopleNum)%ClothingPtr)
          ThermalComfortData(PeopleNum)%ClothingValue = CloUnit
        ELSE IF(IntermediateClothing .EQ. 2.0d0) THEN
          ThermalComfortData(PeopleNum)%ThermalComfortOpTemp = (RadTemp+AirTemp)/2.0d0
          ThermalComfortData(PeopleNum)%ClothingValue = CloUnit
          CALL DynamicClothingModel
          CloUnit = ThermalComfortData(PeopleNum)%ClothingValue
        ELSE
          CloUnit = GetCurrentScheduleValue(People(PeopleNum)%ClothingPtr)
          CALL ShowWarningError('PEOPLE="'//TRIM(People(PeopleNum)%Name)// &
                     '", Scheduled clothing value will be used rather than clothing calculation method.')
        ENDIF
      CASE DEFAULT
        CALL ShowSevereError('PEOPLE="'//TRIM(People(PeopleNum)%Name)//'", Incorrect Clothing Type')
    END SELECT

    AirVel  = GetCurrentScheduleValue(People(PeopleNum)%AirVelocityPtr)
    IntHeatProd = ActLevel - WorkEff
! THE FOLLOWING ARE TYPICAL VALUES SET FOR BLAST RUNS
! STANDARD MAN: 70. KG WEIGHT, 1.8 M2 SURFACE AREA
    BodyWt = 70.d0
    CoreTemp = 37.d0
    SkinTemp = 31.d0

!   CALCULATIONS NEEDED FOR THE PASSIVE STATE EQUATIONS
    CoreThermCap = 0.9d0*BodyWt*0.97d0/BodySurfArea
    SkinThermCap = 0.1d0*BodyWt*0.97d0/BodySurfArea
!   KERSLAKE'S FORMULA (0.05<AirVel<5. M/S)
    IF(AirVel < 0.137d0) AirVel = 0.137d0
    Hc = 8.3d0*SQRT(AirVel)
    EmissAvg = RadSurfEff*CloEmiss + (1.d0 - RadSurfEff)*1.d0
!   IBERALL EQUATION
    Hr = EmissAvg*(3.87d0 + 0.031d0*RadTemp)
    H = Hr+Hc
    OpTemp = (Hc*AirTemp + Hr*RadTemp)/H
    VapPress = CalcSatVapPressFromTemp(AirTemp)
    VapPress = RelHum*VapPress
    CloBodyRat = 1.0d0+0.2d0*CloUnit
    CloThermEff = 1.d0/(1.d0 + 0.155d0*H*CloBodyRat*CloUnit)
    CloPermeatEff = 1.d0/(1.d0 + 0.143d0*Hc*CloUnit)
!  CALCULATE THE PHYSIOLOGICAL REACTIONS OF AN UNACCLIMATED
!  MAN (LastDayNum = 1), OR AN ACCLIMATED MAN (LastDayNum = 14, IncreDayNum = 13),
    DO NumDay = StartDayNum,LastDayNum,IncreDayNum
!  INITIAL CONDITIONS IN AN EXPOSURE
      DayNum=REAL(NumDay,r64)
      Time = 0.0d0
      TimeChange = .01d0
      SweatSuppFac = 1.d0
      Temp(1) = CoreTemp
      Temp(2) = SkinTemp
      Coeff(1) = 0.0d0
      Coeff(2) = 0.0d0
!  PHYSIOLOGICAL ADJUSTMENTS IN HEAT ACCLIMATION.
      AcclPattern = 1.d0 - EXP(-0.12d0*(DayNum - 1.0d0))
      CoreTempNeut = 36.9d0 - 0.6d0*AcclPattern
      SkinTempNeut = 33.8d0 - 1.6d0*AcclPattern
      ActLevel = ActLevel - 0.07d0*ActLevel*AcclPattern
!  BASIC INFORMATION FOR THERMAL SENSATION.
      IntHeatProdMet = IntHeatProd/ActLevelConv
      IntHeatProdMetMax = MAX(1.d0,IntHeatProdMet)
      ThermCndctNeut = 12.05d0*EXP(0.2266d0*(IntHeatProdMetMax - 1.0d0))
      SkinWetNeut = 0.02d0 + 0.4d0*(1.d0-EXP(-0.6d0*(IntHeatProdMetMax - 1.0d0)))
      ThermCndctMin = (ThermCndctNeut - 5.3d0)*0.26074074d0 + 5.3d0
!  CALCULATION OF CoreTempChange/TempChange & SkinTempChange/TempChange
      CALL DERIV(TempIndiceNum,Temp,TempChange)
      DO
!  CALCULATION OF THERMAL SENSATION VOTE (TSV).
!  THE TSV MODEL CAN BE APPLIED TO UNACCLIMATED MAN ONLY.
        SkinWetFac = (SkinWetSweat - SkinWetNeut)/(1.d0 - SkinWetNeut)
        VasodilationFac = (ThermCndct - ThermCndctNeut)/(75.d0 - ThermCndctNeut)
        VasoconstrictFac = (ThermCndctNeut - ThermCndct)/(ThermCndctNeut - ThermCndctMin)
!  IF VasodilationFac < 0.0, VASOCONSTRICTION OCCURS AND RESULTS IN COLD SENSATION.
!  OTHERWISE NORMAL BLOOD FLOW OR VASODILATION OCCURS AND RESULTS IN
!  THERMAL NEUTRALITY OR WARM SENSATION.
        IF (VasodilationFac < 0) THEN
         ThermalComfortData(PeopleNum)%KsuTSV = -1.46153d0*VasoconstrictFac + 3.74721d0*VasoconstrictFac**2 - &
                                                6.168856d0*VasoconstrictFac**3
        ELSE
         ThermalComfortData(PeopleNum)%KsuTSV = (5.d0 - 6.56d0*(RelHum - 0.50d0))*SkinWetFac
         IF (ThermalComfortData(PeopleNum)%KsuTSV > TSVMax) ThermalComfortData(PeopleNum)%KsuTSV = TSVMax
        END IF

      ThermalComfortData(PeopleNum)%ThermalComfortMRT = RadTemp
      ThermalComfortData(PeopleNum)%ThermalComfortOpTemp = (RadTemp+AirTemp)/2.0d0

      CoreTemp = Temp(1)
      SkinTemp = Temp(2)
      EvapHeatLossSweatPrev = EvapHeatLossSweat

      CALL RKG (TempIndiceNum,TimeChange,Time,Temp,TempChange,Coeff)

      IF (Time > TimeExpos) EXIT

      END DO

    END DO

  END DO

  RETURN

END SUBROUTINE CalcThermalComfortKSU

SUBROUTINE DERIV(TempIndiceNum,Temp,TempChange)

          ! SUBROUTINE INFORMATION:
          !     AUTHOR         Jaewook Lee
          !     DATE WRITTEN   January 2000
          !     MODIFIED       Rick Strand (for E+ implementation February 2000)
          !     RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! THIS SUBROUTINE CALCULATES HEAT TRANSFER TERMS INVOLVED IN THE
          ! THERMOREGULATORY SYSTEM TO OBTAIN THE RATES OF CHANGE OF CoreTemp & SkinTemp
          ! VIZ., CoreTempChange/TempChange & SkinTempChange/TempChange RESPECTIVELY.

          ! METHODOLOGY EMPLOYED:
          ! This subroutine is based heavily upon the work performed by Dan Maloney for
          ! the BLAST program.  Many of the equations are based on the original Pierce
          ! development.  See documentation for further details and references.

          ! REFERENCES:
          ! Maloney, Dan, M.S. Thesis, University of Illinois at Urbana-Champaign

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER :: TempIndiceNum        ! Number of temperature indices  unused1208
  REAL(r64), DIMENSION(2) :: Temp       ! Temperature unused1208
  REAL(r64), DIMENSION(2) :: TempChange ! Change of temperature

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: ActLevelTot             ! Total activity level
  REAL(r64) :: CoreSignalShiv          ! Core signal when shivering occurs
  REAL(r64) :: CoreSignalShivMax       ! Maximum value of core signal when shivering occurs
  REAL(r64) :: CoreSignalSkinSens      ! The sensitivity of the skin signal increases
  REAL(r64) :: CoreSignalSweatMax      ! Maximum value of core signal when sweating occurs
  REAL(r64) :: CoreSignalSweatWarm     ! Core signal when sweating occurs
  REAL(r64) :: CoreTempSweat           ! Core temperature when sweating occurs
  REAL(r64) :: CoreSignalWarm          ! Warm core signal
  REAL(r64) :: CoreSignalWarmMax       ! Maximum value of warm core signal
  REAL(r64) :: EvapHeatLossDrySweat    ! Evaporative heat loss by sweating when total skin wettedness < 0.4
  REAL(r64) :: Err                     ! Stop criteria for iteration
  REAL(r64) :: ErrPrev                 ! Previous value of stop criteria for iteration
  REAL(r64) :: EvapHeatLossSweatEst    ! Estimated evaporative heat loss by sweating
  REAL(r64) :: EvapHeatLossSweatEstNew ! New value of estimated evaporative heat loss by sweating
  REAL(r64) :: IntHeatProdTot          ! Total internal heat production
  REAL(r64) :: SkinCndctMax            ! Maximum value of skin conductance
  REAL(r64) :: SkinSignalCold          ! Cold skin signal
  REAL(r64) :: SkinSignalColdMax       ! Maximum value of cold skin signal
  REAL(r64) :: SkinSignalSweatCold     ! Cold skin signal for sweat inhibition
  REAL(r64) :: SkinSignalSweatColdMax  ! Maximum value of cold skin signal for sweat inhibition
  REAL(r64) :: SkinCndctDilation       ! Overall skin conductance due to vasodilation
  REAL(r64) :: SkinCndctConstriction   ! Overall skin conductance due to vasoconstriction
  REAL(r64) :: SkinSignalShiv          ! Skin signal when shivering occurs
  REAL(r64) :: SkinSignalShivMax       ! Maximum value of skin signal when shivering occurs
  REAL(r64) :: SkinSignalSweatMax      ! Skin signal when sweating occurs
  REAL(r64) :: SkinSignalSweatWarm     ! Maximum value of skin signal when sweating occurs
  REAL(r64) :: SkinSignalWarm          ! Warm skin signal
  REAL(r64) :: SkinSignalWarmMax       ! Maximum value of warm skin signal
  REAL(r64) :: SkinTempSweat           ! Skin temperature when sweating occurs
  REAL(r64) :: SkinWetSignal           ! Skin wettedness signal
  REAL(r64) :: SweatCtrlFac            ! Sweat control factor
  REAL(r64) :: SweatSuppFac            ! Sweat suppression factor due to skin wettedness
  REAL(r64) :: WeighFac                ! Weighting factor of core siganl

  ! THE CONTROLLING SYSTEM.
  ! THE CONTROLLING SIGNALS :
  ! SIGNALS FOR KS.
  CoreSignalWarm = CoreTemp - 36.98d0
  SkinSignalWarm = SkinTemp - 33.8d0
  SkinSignalCold = 32.1d0 - SkinTemp
  CoreSignalSkinSens = CoreTemp - 35.15d0
  CoreSignalWarmMax = MAX(0.d0,CoreSignalWarm)
  SkinSignalWarmMax = MAX(0.d0,SkinSignalWarm)
  SkinSignalColdMax = MAX(0.d0,SkinSignalCold)

  ! SIGNALS FOR EvapHeatLossSweat.
  CoreTempSweat = CoreTemp
  IF(CoreTempSweat > 38.29d0) CoreTempSweat = 38.29d0
  CoreSignalSweatWarm = CoreTempSweat - CoreTempNeut
  SkinTempSweat = SkinTemp
  IF(SkinTempSweat > 36.1d0) SkinTempSweat = 36.1d0
  SkinSignalSweatWarm = SkinTempSweat - SkinTempNeut
  CoreSignalSweatMax = MAX(0.d0,CoreSignalSweatWarm)
  SkinSignalSweatMax = MAX(0.d0,SkinSignalSweatWarm)
  SkinSignalSweatCold = 33.37d0 - SkinTemp
  IF(SkinTempNeut < 33.37d0) SkinSignalSweatCold = SkinTempNeut - SkinTemp
  SkinSignalSweatColdMax = MAX(0.d0,SkinSignalSweatCold)

  ! SIGNALS FOR SHIVERING.
  CoreSignalShiv = 36.9d0 - CoreTemp
  SkinSignalShiv = 32.5d0 - SkinTemp
  CoreSignalShivMax = MAX(0.d0,CoreSignalShiv)
  SkinSignalShivMax = MAX(0.d0,SkinSignalShiv)

  ! CONTROLLING FUNCTIONS :
  ! SHIVERING RESPONSE IN W/M**2.
  ShivResponse = 20.d0*CoreSignalShivMax*SkinSignalShivMax + 5.d0*SkinSignalShivMax
  IF(CoreTemp >= 37.1d0) ShivResponse = 0.0d0

  ! SWEAT FUNCTION IN W/M**2.
  WeighFac = 260.d0+70.d0*AcclPattern
  SweatCtrlFac = 1.d0 + 0.05d0*SkinSignalSweatColdMax**2.4d0

  ! EvapHeatLossDrySweat = SWEAT WHEN SkinWetTot < 0.4.
  EvapHeatLossDrySweat = ((WeighFac*CoreSignalSweatMax + 0.1d0*WeighFac*SkinSignalSweatMax) &
                         *EXP(SkinSignalSweatMax/8.5d0))/SweatCtrlFac

  ! MAXIMUM EVAPORATIVE POWER, EvapHeatLossMax, IN W/M**2.
  SkinVapPress = CalcSatVapPressFromTemp(SkinTemp)
  EvapHeatLossMax = 2.2d0*Hc*(SkinVapPress - VapPress)*CloPermeatEff
  IF(EvapHeatLossMax > 0.0d0) THEN
    SkinWetSweat = EvapHeatLossDrySweat/EvapHeatLossMax
    EvapHeatLossDiff = 0.408d0*(SkinVapPress - VapPress)
    EvapHeatLoss = SkinWetSweat*EvapHeatLossMax+(1.d0 - SkinWetSweat)*EvapHeatLossDiff
    SkinWetTot = EvapHeatLoss/EvapHeatLossMax
    IF(Time == 0.0d0) THEN
      EvapHeatLossSweat = EvapHeatLossDrySweat
      EvapHeatLossSweatPrev = EvapHeatLossDrySweat
    END IF
    IF(SkinWetTot > 0.4d0) THEN

  ! ITERATION  FOR SWEAT WHEN SkinWetTot IS GREATER THAT 0.4.
      IterNum = 0
      IF(SkinWetSweat > 1.0d0) SkinWetSweat = 1.d0
      DO
        EvapHeatLossSweatEst = EvapHeatLossSweatPrev
        SkinWetSweat = EvapHeatLossSweatEst/EvapHeatLossMax

        IF(SkinWetSweat > 1.0d0) SkinWetSweat = 1.d0

        EvapHeatLossDiff = 0.408d0*(SkinVapPress - VapPress)
        EvapHeatLoss = (1.d0 - SkinWetTot)*EvapHeatLossDiff + EvapHeatLossSweat
        SkinWetTot = EvapHeatLoss/EvapHeatLossMax

        IF(SkinWetTot > 1.0d0) SkinWetTot = 1.d0

        SkinWetSignal = MAX(0.d0,SkinWetTot - .4d0)
        SweatSuppFac = 0.5d0 + 0.5d0*EXP(-5.6d0*SkinWetSignal)
        EvapHeatLossSweatEstNew = SweatSuppFac*EvapHeatLossDrySweat

        IF(IterNum == 0) EvapHeatLossSweat = EvapHeatLossSweatEstNew

        Err = EvapHeatLossSweatEst - EvapHeatLossSweatEstNew

        IF (IterNum /= 0) THEN
          IF((ErrPrev*Err) < 0.0d0) EvapHeatLossSweat = (EvapHeatLossSweatEst + EvapHeatLossSweatEstNew)/2.0d0
          IF((ErrPrev*Err) >= 0.0d0) EvapHeatLossSweat = EvapHeatLossSweatEstNew
        END IF

  ! STOP CRITERION FOR THE ITERATION.
        IF((ABS(Err) <= 0.5d0) .OR. (IterNum >= 10)) EXIT
        IterNum = IterNum + 1
        EvapHeatLossSweatPrev = EvapHeatLossSweat
        ErrPrev = Err

      END DO

    ELSE
      EvapHeatLossSweat = EvapHeatLossDrySweat
    END IF

  ELSE
    SkinWetSweat = 1.d0
    SkinWetTot = 1.d0
    EvapHeatLossSweat = 0.5d0*EvapHeatLossDrySweat
    EvapHeatLoss = EvapHeatLossSweat
  END IF

  ! OVERALL SKIN CONDUCTANCE, KS, IN W/M**2/C.
  ! SkinCndctDilation = EFFECT DUE TO VASODILATION.
  ! SkinCndctConstriction = EFFECT DUE TO VASOCONSTRICTION.
  SkinCndctDilation = 42.45d0*CoreSignalWarmMax + 8.15d0*CoreSignalSkinSens**0.8d0*SkinSignalWarmMax
  SkinCndctConstriction = 1.0d0 + 0.4d0*SkinSignalColdMax
  ! ThermCndct IS EQUIVALENT TO KS
  ThermCndct = 5.3d0+(6.75d0+SkinCndctDilation)/SkinCndctConstriction
  SkinCndctMax = 75.d0+10.d0*AcclPattern
  IF(ThermCndct > SkinCndctMax) ThermCndct = SkinCndctMax

  ! PASSIVE ENERGY BALANCE EQUATIONS.
  ! TOTAL METABOLIC HEAT PRODUCTION RATE, ActLevel, IN W/M**2.
  ActLevelTot = ActLevel + ShivResponse
  IntHeatProdTot = ActLevelTot - WorkEff
  ! RESPIRATION HEAT LOSS, RespHeatLoss, IN W/M**0.
  LatRespHeatLoss = 0.0023d0*ActLevelTot*(44.d0 - VapPress)
  DryRespHeatLoss = 0.0014d0*ActLevelTot*(34.d0 - AirTemp)
  RespHeatLoss = LatRespHeatLoss + DryRespHeatLoss
  ! HEAT FLOW FROM CORE TO SKIN, HeatFlow, IN W/M**2.
  HeatFlow = ThermCndct*(CoreTemp - SkinTemp)
  ! TempChange(1) = CoreTempChange/TempChange, IN C/HR.
  TempChange(1) = (IntHeatProdTot - RespHeatLoss - HeatFlow)/CoreThermCap
  IF(EvapHeatLoss > EvapHeatLossMax) EvapHeatLoss = EvapHeatLossMax

  ! DRY HEAT EXCHANGE BY RADIATION & CONVECTION, R+C, IN W/M**2.
  DryHeatLoss = H*CloBodyRat*CloThermEff*(SkinTemp - OpTemp)
  ! TempChange(2) = SkinTempChange/TempChange, IN C/HR.
  TempChange(2) = (HeatFlow - EvapHeatLoss - DryHeatLoss)/SkinThermCap

  RETURN

END SUBROUTINE DERIV


SUBROUTINE RKG(NEQ,H,X,Y,DY,C)

! SUBROUTINE INFORMATION:
          !     AUTHOR         Jaewook Lee
          !     DATE WRITTEN   January 2000
          !     MODIFIED       Rick Strand (for E+ implementation February 2000)
          !     RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This is a subroutine for integration by Runga-Kutta's method.

          ! METHODOLOGY EMPLOYED:
          ! This subroutine is based heavily upon the work performed by Dan Maloney for
          ! the BLAST program.  Many of the equations are based on the original Pierce
          ! development.  See documentation for further details and references.

          ! REFERENCES:
          ! Maloney, Dan, M.S. Thesis, University of Illinois at Urbana-Champaign

          ! USE STATEMENTS:
          ! na

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
  INTEGER :: I
  INTEGER :: J
  INTEGER :: NEQ
  REAL(r64) :: B
  REAL(r64) :: H
  REAL(r64) :: H2
  REAL(r64) :: X
  REAL(r64), DIMENSION(2)   :: A
  REAL(r64), DIMENSION(NEQ) :: C
  REAL(r64), DIMENSION(NEQ) :: DY
  REAL(r64), DIMENSION(NEQ) :: Y

  A(1) = 0.29289321881345d0
  A(2) = 1.70710678118654d0
  H2 = .5d0*H

  CALL DERIV (NEQ,Y,DY)
  DO I = 1,NEQ
    B = H2*DY(I) - C(I)
    Y(I) = Y(I) + B
    C(I) = C(I) + 3.d0*B - H2*DY(I)
  END DO

  X = X + H2

  DO J = 1,2
    CALL DERIV (NEQ,Y,DY)
    DO I = 1,NEQ
      B = A(J)*(H*DY(I) - C(I))
      Y(I) = Y(I) + B
      C(I) = C(I) + 3.d0*B - A(J)*H*DY(I)
    END DO
  END DO

  X = X + H2
  CALL DERIV (NEQ,Y,DY)

  DO I = 1,NEQ
    B = (H*DY(I) - 2.d0*C(I))/6.0d0
    Y(I) = Y(I) + B
    C(I) = C(I) + 3.d0*B - H2*DY(I)
  END DO

  CALL DERIV (NEQ,Y,DY)

  RETURN

END SUBROUTINE RKG

SUBROUTINE GetAngleFactorList

          ! SUBROUTINE INFORMATION:
          !     AUTHOR         Jaewook Lee
          !     DATE WRITTEN   July 2001
          !     MODIFIED       na
          !     RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals
  USE DataHeatBalance
  USE DataSurfaces,     ONLY : Surface, TotSurfaces
  USE InputProcessor,   ONLY : GetNumObjectsFound, GetObjectItem, FindItemInList
  USE DataIPShortCuts
  USE General,          ONLY : RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER        :: AngleFacLimit = 0.01d0     ! To set the limit of sum of angle factors
  CHARACTER(len=*), PARAMETER :: Blank = ' '
  INTEGER,          PARAMETER :: MaxSurfaces = 20         ! Maximum number of surfaces in each AngleFactor List

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!unused1208  CHARACTER(len=MaxNameLength),  &
!        DIMENSION(22)       :: Alphas                  ! Alpha strings from Input Processor
  REAL(r64)                 :: AllAngleFacSummed       ! Sum of angle factors in each zone
  LOGICAL                   :: ErrorsFound=.false.     ! Set to true if errors in input, fatal at end of routine
  INTEGER                   :: IOStatus
  INTEGER                   :: Item                    ! Item to be "gotten"
  INTEGER                   :: NumAlphas               ! Number of Alphas from InputProcessor
!unused1208  REAL(r64), DIMENSION(20)       :: Numbers                 ! Numbers from Input Processor
  INTEGER                   :: NumNumbers              ! Number of Numbers from Input Processor
  INTEGER                   :: NumOfAngleFactorLists   ! Number of Angle Factor Lists found in IDF
  INTEGER                   :: SurfNum                 ! Surface number DO loop counter
  INTEGER                   :: WhichAFList             ! Used in validating AngleFactorList


  cCurrentModuleObject='ComfortViewFactorAngles'
  NumOfAngleFactorLists = GetNumObjectsFound(cCurrentModuleObject)
  ALLOCATE(AngleFactorList(NumOfAngleFactorLists))
  AngleFactorList%Name              = Blank
  AngleFactorList%ZoneName          = Blank
  AngleFactorList%ZonePtr           = 0

  DO Item = 1, NumOfAngleFactorLists

    AllAngleFacSummed = 0.0d0

    CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus,  &
                          NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                          AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    AngleFactorList(Item)%Name = cAlphaArgs(1)  ! no need for verification/uniqueness.
    AngleFactorList(Item)%ZoneName = cAlphaArgs(2)
    AngleFactorList(Item)%ZonePtr  = FindIteminList(cAlphaArgs(2),Zone%Name,NumOfZones)
    IF (AngleFactorList(Item)%ZonePtr == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'", '//  &
                          'invalid - not found')
      CALL ShowContinueError('...invalid '//TRIM(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'".')
      ErrorsFound=.true.
    END IF

    AngleFactorList(Item)%TotAngleFacSurfaces = NumNumbers
    IF (AngleFactorList(Item)%TotAngleFacSurfaces > MaxSurfaces) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Too many surfaces specified in '//TRIM(cAlphaFieldNames(1))//  &
         '='//TRIM(cAlphaArgs(1)))
      ErrorsFound=.true.
    END IF

    ALLOCATE(AngleFactorList(Item)%SurfaceName(AngleFactorList(Item)%TotAngleFacSurfaces))
    ALLOCATE(AngleFactorList(Item)%SurfacePtr(AngleFactorList(Item)%TotAngleFacSurfaces))
    ALLOCATE(AngleFactorList(Item)%AngleFactor(AngleFactorList(Item)%TotAngleFacSurfaces))

    DO SurfNum = 1, AngleFactorList(Item)%TotAngleFacSurfaces
      AngleFactorList(Item)%SurfaceName(SurfNum)       = cAlphaArgs(SurfNum+2)
      AngleFactorList(Item)%SurfacePtr(SurfNum)        = FindIteminList(cAlphaArgs(SurfNum+2),Surface%Name,TotSurfaces)
      AngleFactorList(Item)%AngleFactor(SurfNum)       = rNumericArgs(SurfNum)
          ! Error trap for surfaces that do not exist or surfaces not in the zone
      IF (AngleFactorList(Item)%SurfacePtr(SurfNum) == 0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(SurfNum+2))//   &
           ', entered value='//TRIM(cAlphaArgs(SurfNum+2)))
        CALL ShowContinueError('ref '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1))//  &
           ' not found in '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
        ErrorsFound=.true.
      ELSEIF (AngleFactorList(Item)%ZonePtr /= 0) THEN  ! don't look at invalid zones
        ! Found Surface, is it in same zone tagged for Angle Factor List?
        IF (AngleFactorList(Item)%ZonePtr /= Surface(AngleFactorList(Item)%SurfacePtr(SurfNum))%Zone) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'", invalid - mismatch '//  &
                TRIM(cAlphaFieldNames(2))//'="'//trim(cAlphaArgs(2))//'"')
          CALL ShowContinueError('... does not match '//trim(cAlphaFieldNames(2))//'="'//  &
             trim(Zone(Surface(AngleFactorList(Item)%SurfacePtr(SurfNum))%Zone)%Name)//  &
             '" for '//trim(cAlphaFieldNames(SurfNum+2))//'="'//trim(cAlphaArgs(SurfNum+2))//'".')
          ErrorsFound=.true.
        ENDIF
      END IF

      AllAngleFacSummed = AllAngleFacSummed + AngleFactorList(Item)%AngleFactor(SurfNum)

    END DO

    IF (ABS(AllAngleFacSummed-1.0d0) > AngleFacLimit) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'", invalid - Sum[AngleFactors]')
        CALL ShowContinueError('...Sum of Angle Factors ['//trim(RoundSigDigits(AllAngleFacSummed,3))//  &
           '] exceed expected sum [1.0] by more than limit ['//TRIM(RoundSigDigits(AngleFacLimit,3))//'].')
        ErrorsFound=.true.
    END IF

  END DO

  IF (ErrorsFound) THEN
     CALL ShowFatalError('GetAngleFactorList: Program terminated due to preceding errors.')
  END IF

  DO Item = 1, TotPeople
    IF(People(Item)%MRTCalcType /= AngleFactor) CYCLE
    People(Item)%AngleFactorListPtr = FindIteminList(People(Item)%AngleFactorListName,AngleFactorList%Name,NumOfAngleFactorLists)
    WhichAFList=People(Item)%AngleFactorListPtr
    IF (WhichAFList == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//trim(People(Item)%AngleFactorListName)//'", invalid')
      CALL ShowSevereError('... Angle Factor List Name not found for PEOPLE= '//TRIM(People(Item)%Name))
      ErrorsFound=.true.
    ELSEIF(People(Item)%ZonePtr /= AngleFactorList(WhichAFList)%ZonePtr) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(AngleFactorList(WhichAFList)%Name)//' mismatch Zone Name')
      CALL ShowContinueError('...Zone="'//trim(AngleFactorList(WhichAFList)%ZoneName)//  &
           ' does not match Zone="'//trim(Zone(People(Item)%ZonePtr)%Name)//'" in PEOPLE="'//  &
           TRIM(People(Item)%Name)//'".')
      ErrorsFound=.true.
    END IF
  END DO

  IF (ErrorsFound) THEN
     CALL ShowFatalError('GetAngleFactorList: Program terminated due to preceding errors.')
  ENDIF

END SUBROUTINE GetAngleFactorList

REAL(r64) FUNCTION CalcAngleFactorMRT(AngleFacNum)

          ! SUBROUTINE INFORMATION:
          !     AUTHOR         Jaewook Lee
          !     DATE WRITTEN   July 2001
          !     MODIFIED       na
          !     RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! THIS IS A SUBROUTINE TO CALCULATE ANGLE FACTOR MRT

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHeatBalSurface, ONLY : TH

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this SUBROUTINE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: AngleFacNum
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: SurfaceTemp
  INTEGER :: SurfNum
  REAL(r64)    :: SurfTempAngleFacSummed

          ! FLOW:

  SurfTempAngleFacSummed = 0.0d0

  DO SurfNum = 1, AngleFactorList(AngleFacNum)%TotAngleFacSurfaces

    SurfaceTemp = TH(AngleFactorList(AngleFacNum)%SurfacePtr(SurfNum),1,2)
    SurfTempAngleFacSummed = SurfTempAngleFacSummed + SurfaceTemp * AngleFactorList(AngleFacNum)%AngleFactor(SurfNum)

  END DO

  CalcAngleFactorMRT = SurfTempAngleFacSummed

  RETURN

END FUNCTION CalcAngleFactorMRT

REAL(r64) FUNCTION CalcSatVapPressFromTemp(Temp)

          ! FUNCTION INFORMATION:
          !     AUTHOR         Jaewook Lee
          !     DATE WRITTEN   January 2000
          !     MODIFIED       Rick Strand (for E+ implementation February 2000)
          !     RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! THIS IS A FUNCTION TO CALCULATE THE SATURATED VAPOR PRESSURE
          ! FROM AIR TEMPERATURE

          ! METHODOLOGY EMPLOYED:
          ! This function is based upon the work performed by Dan Maloney for
          ! the BLAST program.
          ! REFERENCES:
          ! Maloney, Dan, M.S. Thesis, University of Illinois at Urbana-Champaign

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this function

          ! FUNCTION ARGUMENT DEFINITIONS:
          ! na

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64), INTENT(IN) :: Temp
  REAL(r64) :: XT

          ! FLOW

  XT = Temp/100.d0
  CalcSatVapPressFromTemp = 6.16796d0 + 358.1855d0*XT**2 - 550.3543d0*XT**3 + 1048.8115d0*XT**4

  RETURN

END FUNCTION CalcSatVapPressFromTemp

REAL(r64) FUNCTION CalcRadTemp(PeopleListNum)

          ! FUNCTION INFORMATION:
          !     AUTHOR         Jaewook Lee
          !     DATE WRITTEN   November 2000
          !     MODIFIED       Rick Strand (for E+ implementation November 2000)
          !                    Rick Strand (for high temperature radiant heaters March 2001)
          !     RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! THIS IS A FUNCTION TO CALCULATE EITHER ZONE AVERAGED MRT OR
          ! SURFACE WEIGHTED MRT

          ! METHODOLOGY EMPLOYED:
          ! The method here is fairly straight-forward.  If the user has selected
          ! a zone average MRT calculation, then there is nothing to do other than
          ! to assign the function value because the zone MRT has already been
          ! calculated.  Note that this value is an "area-emissivity" weighted value.
          ! If the user wants to place the occupant "near" a particular surface,
          ! then at the limit half of the radiant field will be from this surface.
          ! As a result, an average of the zone MRT and the surface temperature
          ! is taken to arrive at an approximate radiant temperature.
          ! If a high temperature radiant heater is present, then this must also be
          ! taken into account.  The equation used to account for this factor is
          ! based on equation 49 on page 150 of Fanger's text (see reference below).
          ! The additional assumptions for EnergyPlus are that the radiant energy
          ! from the heater must be spread over the average area of a human being
          ! (see parameter below) and that the emissivity and absorptivity of the
          ! occupant are equivalent for the dominant wavelength of radiant energy
          ! from the heater.  These assumptions might be off slightly, but it does
          ! allow for an approximation of the effects of surfaces and heaters
          ! within a space.  Future additions might include the effect of direct
          ! solar energy on occupants.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHeatBalFanSys,  ONLY : QHTRadSysToPerson, QHWBaseboardToPerson, QSteamBaseboardToPerson, QElecBaseboardToPerson
  USE DataHeatBalSurface, ONLY : TH

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this function

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: PeopleListNum    ! Type of MRT calculation (zone averaged or surface weighted)
          ! FUNCTION PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: AreaEff = 1.8d0                      ! Effective area of a "standard" person in meters squared
!  REAL(r64), PARAMETER :: KelvinConv = KelvinConv                ! Conversion from Celsius to Kelvin
  REAL(r64), PARAMETER :: StefanBoltzmannConst = 5.6697d-8   ! Stefan-Boltzmann constant in W/(m2*K4)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: SurfaceTemp
  REAL(r64) :: ZoneRadTemp

          ! FLOW:
  SELECT CASE (People(PeopleListNum)%MRTCalcType)

    CASE (ZoneAveraged)
      RadTemp = MRT(ZoneNum)
    CASE (SurfaceWeighted)
      ZoneRadTemp = MRT(ZoneNum)
      SurfaceTemp = TH(People(PeopleListNum)%SurfacePtr,1,2)
      RadTemp = (ZoneRadTemp + SurfaceTemp)/2.0d0
    CASE (AngleFactor)
      RadTemp = CalcAngleFactorMRT(People(PeopleListNum)%AngleFactorListPtr)

  END SELECT

  ! If high temperature radiant heater present and on, then must account for this in MRT calculation
  IF (QHTRadSysToPerson(ZoneNum) > 0.0d0 .OR. QHWBaseboardToPerson(ZoneNum) > 0.0d0 .OR. &
        QSteamBaseboardToPerson(ZoneNum) > 0.0d0 .OR. QElecBaseboardToPerson(ZoneNum) > 0.0d0) THEN
    RadTemp = RadTemp + KelvinConv  ! Convert to Kelvin
    RadTemp = ((RadTemp**4)+((QHTRadSysToPerson(ZoneNum)+QHWBaseboardToPerson(ZoneNum)+ &
                QSteamBaseboardToPerson(ZoneNum) + QElecBaseboardToPerson(ZoneNum))/ &
                AreaEff/StefanBoltzmannConst))**(1.0d0/4.0d0)
    RadTemp = RadTemp - KelvinConv  ! Convert back to Celsius
  END IF

  CalcRadTemp = RadTemp

  RETURN

END FUNCTION CalcRadTemp

SUBROUTINE CalcThermalComfortSimpleASH55
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   June 2005
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Determines if the space is within the ASHRAE 55-2004 comfort region
          !   based on operative temperature and humidity ratio

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
USE OutputReportTabular, Only: isInQuadrilateral
USE General, ONLY: RoundSigDigits
USE DataEnvironment, Only: EnvironmentName, RunPeriodEnvironment, EnvironmentStartEnd
USE OutputReportPredefined

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64) :: OperTemp
REAL(r64) :: HumidRatio
REAL(r64) :: CurAirTemp
REAL(r64) :: CurMeanRadiantTemp
REAL(r64) :: NumberOccupants
LOGICAL :: isComfortableWithSummerClothes
LOGICAL :: isComfortableWithWinterClothes
INTEGER :: iPeople
INTEGER :: iZone
REAL(r64) :: allowedHours
LOGICAL :: showWarning

AnyZoneTimeNotSimpleASH55Summer = 0.0d0
AnyZoneTimeNotSimpleASH55Winter = 0.0d0
AnyZoneTimeNotSimpleASH55Either = 0.0d0

!assume the zone is unoccupied
ThermalComfortInASH55%ZoneIsOccupied = .FALSE.
!loop through the people objects and determine if the zone is currently occupied
DO iPeople = 1, TotPeople
  ZoneNum = People(iPeople)%ZonePtr
  NumberOccupants = People(iPeople)%NumberOfPeople * GetCurrentScheduleValue(People(iPeople)%NumberOfPeoplePtr)
  IF (NumberOccupants .GT. 0) THEN
    ThermalComfortInASH55(ZoneNum)%ZoneIsOccupied =  .TRUE.
  END IF
END DO
!loop through the zones and determine if in simple ashrae 55 comfort regions
DO iZone = 1, NumOfZones
  IF (ThermalComfortInASH55(iZone)%ZoneIsOccupied) THEN
    !keep track of occupied hours
    ZoneOccHrs(iZone) = ZoneOccHrs(iZone) + TimeStepZone
    IF (IsZoneDV(iZone) .or. IsZoneUI(iZone)) THEN
      CurAirTemp = TCMF(iZone)
    ELSE
      CurAirTemp = ZTAV(iZone)
    ENDIF
    CurMeanRadiantTemp = MRT(iZone)
    OperTemp = CurAirTemp * 0.5d0 + CurMeanRadiantTemp * 0.5d0
    HumidRatio = ZoneAirHumRat(iZone)
    !for debugging
    !ThermalComfortInASH55(iZone)%dCurAirTemp = CurAirTemp
    !ThermalComfortInASH55(iZone)%dCurMeanRadiantTemp = CurMeanRadiantTemp
    !ThermalComfortInASH55(iZone)%dOperTemp = OperTemp
    !ThermalComfortInASH55(iZone)%dHumidRatio = HumidRatio
    !
    ! From ASHRAE Standard 55-2004 Appendix D
    !  Run    AirTemp(C)   RH(%)  Season  HumidRatio
    !   1       19.6        86    Winter    0.012
    !   2       23.9        66    Winter    0.012
    !   3       25.7        15    Winter    0.003
    !   4       21.2        20    Winter    0.003
    !   5       23.6        67    Summer    0.012
    !   6       26.8        56    Summer    0.012
    !   7       27.9        13    Summer    0.003
    !   8       24.7        16    Summer    0.003
    !
    ! But the standard says "no recommended lower humidity limit" so it should
    ! really extend down to the 0.0 Humidity ratio line.  Extrapolating we get
    ! the values that are shown in the following table
    !
    !  Run    AirTemp(C)    Season  HumidRatio
    !   1       19.6        Winter    0.012
    !   2       23.9        Winter    0.012
    !   3       26.3        Winter    0.000
    !   4       21.7        Winter    0.000
    !   5       23.6        Summer    0.012
    !   6       26.8        Summer    0.012
    !   7       28.3        Summer    0.000
    !   8       25.1        Summer    0.000
    !
    !check summer clothing conditions
    isComfortableWithSummerClothes = isInQuadrilateral(OperTemp,HumidRatio,  &
                         25.1d0, 0.0d0,    &
                         23.6d0, 0.012d0,  &
                         26.8d0, 0.012d0,  &
                         28.3d0, 0.0d0)
    !check winter clothing conditions
    isComfortableWithWinterClothes = isInQuadrilateral(OperTemp,HumidRatio,  &
                         21.7d0, 0.0d0,    &
                         19.6d0, 0.012d0,  &
                         23.9d0, 0.012d0,  &
                         26.3d0, 0.0d0)
    IF (isComfortableWithSummerClothes) THEN
      ThermalComfortInASH55(iZone)%timeNotSummer = 0.0d0
    ELSE
      ThermalComfortInASH55(iZone)%timeNotSummer = TimeStepZone
      ThermalComfortInASH55(iZone)%totalTimeNotSummer = &
         ThermalComfortInASH55(iZone)%totalTimeNotSummer + TimeStepZone
      AnyZoneTimeNotSimpleASH55Summer = TimeStepZone
    END IF
    IF (isComfortableWithWinterClothes) THEN
      ThermalComfortInASH55(iZone)%timeNotWinter = 0.0d0
    ELSE
      ThermalComfortInASH55(iZone)%timeNotWinter = TimeStepZone
      ThermalComfortInASH55(iZone)%totalTimeNotWinter = &
         ThermalComfortInASH55(iZone)%totalTimeNotWinter + TimeStepZone
      AnyZoneTimeNotSimpleASH55Winter = TimeStepZone
    END IF
    IF (isComfortableWithSummerClothes .OR. isComfortableWithWinterClothes) THEN
      ThermalComfortInASH55(iZone)%timeNotEither = 0.0d0
    ELSE
      ThermalComfortInASH55(iZone)%timeNotEither = TimeStepZone
      ThermalComfortInASH55(iZone)%totalTimeNotEither = &
         ThermalComfortInASH55(iZone)%totalTimeNotEither + TimeStepZone
      AnyZoneTimeNotSimpleASH55Either = TimeStepZone
    END IF
  ELSE
    !when no one present in that portion of the zone then no one can be uncomfortable
    ThermalComfortInASH55(iZone)%timeNotSummer = 0.0d0
    ThermalComfortInASH55(iZone)%timeNotWinter = 0.0d0
    ThermalComfortInASH55(iZone)%timeNotEither = 0.0d0
  END IF
END DO
! accumulate total time
TotalAnyZoneTimeNotSimpleASH55Summer = TotalAnyZoneTimeNotSimpleASH55Summer + AnyZoneTimeNotSimpleASH55Summer
TotalAnyZoneTimeNotSimpleASH55Winter = TotalAnyZoneTimeNotSimpleASH55Winter + AnyZoneTimeNotSimpleASH55Winter
TotalAnyZoneTimeNotSimpleASH55Either = TotalAnyZoneTimeNotSimpleASH55Either + AnyZoneTimeNotSimpleASH55Either
!was EndEnvrnsFlag prior to CR7562
IF (EndDesignDayEnvrnsFlag) THEN
  allowedHours = REAL(NumOfDayInEnvrn,r64) * 24.d0 * 0.04d0
  !first check if warning should be printed
  showWarning = .FALSE.
  DO iZone = 1, NumOfZones
    IF (ThermalComfortInASH55(iZone)%Enable55Warning) THEN
      IF (ThermalComfortInASH55(iZone)%totalTimeNotEither .GT. allowedHours) THEN
        showWarning = .TRUE.
      END IF
    END IF
  END DO
  !if any zones should be warning print it out
  IF (showWarning) THEN
    CALL ShowWarningError('More than 4% of time (' // Trim(RoundSigDigits(allowedHours,1)) // &
         ' hours) uncomfortable in one or more zones ')
    CALL ShowContinueError('Based on ASHRAE 55-2004 graph (Section 5.2.1.1)')
    IF (RunPeriodEnvironment) THEN
      CALL ShowContinueError('During Environment ['//TRIM(EnvironmentStartEnd)//']: ' // &
        Trim(EnvironmentName))
    ELSE
      CALL ShowContinueError('During SizingPeriod Environment ['//TRIM(EnvironmentStartEnd)//']: ' &
        // Trim(EnvironmentName))
    ENDIF
    DO iZone = 1, NumOfZones
      IF (ThermalComfortInASH55(iZone)%Enable55Warning) THEN
        IF (ThermalComfortInASH55(iZone)%totalTimeNotEither .GT. allowedHours) THEN
          CALL ShowContinueError(Trim(RoundSigDigits( &
            ThermalComfortInASH55(iZone)%totalTimeNotEither,1)) &
            // ' hours were uncomfortable in zone: ' // TRIM(Zone(iZone)%Name))
        END IF
      END IF
    END DO
  END IF
  ! put in predefined reports
  DO iZone = 1, NumOfZones
    CALL PreDefTableEntry(pdchSCwinterClothes,Zone(iZone)%Name,ThermalComfortInASH55(iZone)%totalTimeNotWinter)
    CALL PreDefTableEntry(pdchSCsummerClothes,Zone(iZone)%Name,ThermalComfortInASH55(iZone)%totalTimeNotSummer)
    CALL PreDefTableEntry(pdchSCeitherClothes,Zone(iZone)%Name,ThermalComfortInASH55(iZone)%totalTimeNotEither)
  END DO
  CALL PreDefTableEntry(pdchSCwinterClothes,'Facility',TotalAnyZoneTimeNotSimpleASH55Winter)
  CALL PreDefTableEntry(pdchSCsummerClothes,'Facility',TotalAnyZoneTimeNotSimpleASH55Summer)
  CALL PreDefTableEntry(pdchSCeitherClothes,'Facility',TotalAnyZoneTimeNotSimpleASH55Either)
  !set value for ABUPS report
  TotalTimeNotSimpleASH55EitherForABUPS = TotalAnyZoneTimeNotSimpleASH55Either
  !reset accumulation for new environment
  DO iZone = 1, NumOfZones
    ThermalComfortInASH55(iZone)%totalTimeNotWinter = 0.0d0
    ThermalComfortInASH55(iZone)%totalTimeNotSummer = 0.0d0
    ThermalComfortInASH55(iZone)%totalTimeNotEither = 0.0d0
  END DO
  TotalAnyZoneTimeNotSimpleASH55Winter = 0.0d0
  TotalAnyZoneTimeNotSimpleASH55Summer = 0.0d0
  TotalAnyZoneTimeNotSimpleASH55Either = 0.0d0
  ! report how the aggregation is conducted
  SELECT CASE (kindOfSim)
    CASE(ksDesignDay)
      CALL addFootNoteSubTable(pdstSimpleComfort,'Aggregated over the Design Days')
    CASE(ksRunPeriodDesign)
      CALL addFootNoteSubTable(pdstSimpleComfort,'Aggregated over the RunPeriods for Design')
    CASE(ksRunPeriodWeather)
      CALL addFootNoteSubTable(pdstSimpleComfort,'Aggregated over the RunPeriods for Weather')
  END SELECT
  !report number of occupied hours per week for LEED report
  DO iZone = 1, NumOfZones
    CALL PreDefTableEntry(pdchLeedSutHrsWeek,Zone(iZone)%Name,7 * 24 * (ZoneOccHrs(iZone)/ (NumOfDayInEnvrn * 24)))
  END DO
END IF
END SUBROUTINE CalcThermalComfortSimpleASH55

SUBROUTINE CalcIfSetpointMet
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Jason Glazer
          !       DATE WRITTEN   July 2005
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   Report if the setpoint temperature has been met.
          !   Add calculation of how far away from setpoint and if setpoint was not met
          !   during all times and during occupancy.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:

USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand
USE DataHeatBalFanSys, ONLY: ZoneThermostatSetPointHi, ZoneThermostatSetPointLo, TempTstatAir,TempControlType
USE OutputReportPredefined
USE DataHVACGlobals, ONLY: SingleHeatingSetPoint, SingleCoolingSetPoint, SingleHeatCoolSetPoint, DualSetPointWithDeadBand,   &
                           deviationFromSetPtThresholdHtg, deviationFromSetPtThresholdClg
USE DataRoomAirModel, ONLY: AirModel, RoomAirModel_Mixing

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
REAL(r64) :: SensibleLoadPredictedNoAdj
REAL(r64) :: deltaT
INTEGER :: iZone
LOGICAL :: testHeating
LOGICAL :: testCooling

! Get the load predicted - the sign will indicate if heating or cooling
! was called for
AnyZoneNotMetHeating = 0.0d0
AnyZoneNotMetCooling = 0.0d0
AnyZoneNotMetOccupied = 0.0d0
AnyZoneNotMetHeatingOccupied = 0.0d0
AnyZoneNotMetCoolingOccupied = 0.0d0
DO iZone = 1, NumOfZones
  SensibleLoadPredictedNoAdj = ZoneSysEnergyDemand(iZone)%TotalOutputRequired
  ThermalComfortSetpoint(iZone)%notMetCooling = 0.0d0
  ThermalComfortSetpoint(iZone)%notMetHeating = 0.0d0
  ThermalComfortSetpoint(iZone)%notMetCoolingOccupied = 0.0d0
  ThermalComfortSetpoint(iZone)%notMetHeatingOccupied = 0.0d0
  SELECT CASE (TempControlType(iZone))
    CASE (SingleHeatingSetPoint)
      testHeating = .TRUE.
      testCooling = .FALSE.
    CASE (SingleCoolingSetPoint)
      testHeating = .FALSE.
      testCooling = .TRUE.
    CASE (SingleHeatCoolSetPoint)
      testHeating = .TRUE.
      testCooling = .TRUE.
    CASE (DualSetPointWithDeadBand)
      testHeating = .TRUE.
      testCooling = .TRUE.
    CASE DEFAULT
      testHeating = .TRUE.
      testCooling = .TRUE.
  END SELECT
  IF (testHeating .AND. (SensibleLoadPredictedNoAdj .GT. 0)) THEN !heating
    IF (AirModel(iZone)%AirModelType /= RoomAirModel_Mixing) THEN
      deltaT = TempTstatAir(iZone) - ZoneThermostatSetPointLo(iZone)
    ELSE
      deltaT = ZTAV(iZone) - ZoneThermostatSetPointLo(iZone)
    ENDIF
    IF (deltaT .LT. deviationFromSetPtThresholdHtg) THEN
      ThermalComfortSetpoint(iZone)%notMetHeating = TimeStepZone
      ThermalComfortSetpoint(iZone)%totalNotMetHeating = &
          ThermalComfortSetpoint(iZone)%totalNotMetHeating + TimeStepZone
      IF (AnyZoneNotMetHeating .EQ. 0.0d0) AnyZoneNotMetHeating = TimeStepZone
      IF (ThermalComfortInASH55(iZone)%ZoneIsOccupied) THEN
        ThermalComfortSetpoint(iZone)%notMetHeatingOccupied = TimeStepZone
        ThermalComfortSetpoint(iZone)%totalNotMetHeatingOccupied = &
            ThermalComfortSetpoint(iZone)%totalNotMetHeatingOccupied + TimeStepZone
        IF (AnyZoneNotMetHeatingOccupied .EQ. 0.0d0) AnyZoneNotMetHeatingOccupied = TimeStepZone
        IF (AnyZoneNotMetOccupied .EQ. 0.0d0) AnyZoneNotMetOccupied = TimeStepZone
      END IF
    END IF
  ELSEIF (testCooling .AND. (SensibleLoadPredictedNoAdj .LT. 0)) THEN !cooling
    IF (AirModel(iZone)%AirModelType /= RoomAirModel_Mixing) THEN
      deltaT = TempTstatAir(iZone) - ZoneThermostatSetPointHi(iZone)
    ELSE
      deltaT = ZTAV(iZone) - ZoneThermostatSetPointHi(iZone)
    ENDIF
    IF (deltaT .GT. deviationFromSetPtThresholdClg) THEN
      ThermalComfortSetpoint(iZone)%notMetCooling = TimeStepZone
      ThermalComfortSetpoint(iZone)%totalNotMetCooling = &
          ThermalComfortSetpoint(iZone)%totalNotMetCooling + TimeStepZone
      IF (AnyZoneNotMetCooling .EQ. 0.0d0) AnyZoneNotMetCooling = TimeStepZone
      IF (ThermalComfortInASH55(iZone)%ZoneIsOccupied) THEN
        ThermalComfortSetpoint(iZone)%notMetCoolingOccupied = TimeStepZone
        ThermalComfortSetpoint(iZone)%totalNotMetCoolingOccupied = &
            ThermalComfortSetpoint(iZone)%totalNotMetCoolingOccupied + TimeStepZone
        IF (AnyZoneNotMetCoolingOccupied .EQ. 0.0d0) AnyZoneNotMetCoolingOccupied = TimeStepZone
        IF (AnyZoneNotMetOccupied .EQ. 0.0d0) AnyZoneNotMetOccupied = TimeStepZone
      END IF
    END IF
  ENDIF
END DO
TotalAnyZoneNotMetHeating = TotalAnyZoneNotMetHeating + AnyZoneNotMetHeating
TotalAnyZoneNotMetCooling = TotalAnyZoneNotMetCooling + AnyZoneNotMetCooling
TotalAnyZoneNotMetHeatingOccupied = TotalAnyZoneNotMetHeatingOccupied + AnyZoneNotMetHeatingOccupied
TotalAnyZoneNotMetCoolingOccupied = TotalAnyZoneNotMetCoolingOccupied + AnyZoneNotMetCoolingOccupied
TotalAnyZoneNotMetOccupied = TotalAnyZoneNotMetOccupied + AnyZoneNotMetOccupied

!was EndEnvrnsFlag prior to CR7562
IF (EndDesignDayEnvrnsFlag) THEN
  DO iZone = 1, NumOfZones
    CALL PreDefTableEntry(pdchULnotMetHeat,Zone(iZone)%Name,ThermalComfortSetpoint(iZone)%totalNotMetHeating)
    CALL PreDefTableEntry(pdchULnotMetCool,Zone(iZone)%Name,ThermalComfortSetpoint(iZone)%totalNotMetCooling)
    CALL PreDefTableEntry(pdchULnotMetHeatOcc,Zone(iZone)%Name,ThermalComfortSetpoint(iZone)%totalNotMetHeatingOccupied)
    CALL PreDefTableEntry(pdchULnotMetCoolOcc,Zone(iZone)%Name,ThermalComfortSetpoint(iZone)%totalNotMetCoolingOccupied)
  END DO
  CALL PreDefTableEntry(pdchULnotMetHeat,'Facility',TotalAnyZoneNotMetHeating)
  CALL PreDefTableEntry(pdchULnotMetCool,'Facility',TotalAnyZoneNotMetCooling)
  CALL PreDefTableEntry(pdchULnotMetHeatOcc,'Facility',TotalAnyZoneNotMetHeatingOccupied)
  CALL PreDefTableEntry(pdchULnotMetCoolOcc,'Facility',TotalAnyZoneNotMetCoolingOccupied)
  !set value for ABUPS report
  TotalNotMetHeatingOccupiedForABUPS = TotalAnyZoneNotMetHeatingOccupied
  TotalNotMetCoolingOccupiedForABUPS = TotalAnyZoneNotMetCoolingOccupied
  TotalNotMetOccupiedForABUPS = TotalAnyZoneNotMetOccupied
  !reset counters
  DO iZone = 1, NumOfZones
    ThermalComfortSetpoint(iZone)%totalNotMetHeating = 0.0d0
    ThermalComfortSetpoint(iZone)%totalNotMetCooling = 0.0d0
    ThermalComfortSetpoint(iZone)%totalNotMetHeatingOccupied = 0.0d0
    ThermalComfortSetpoint(iZone)%totalNotMetCoolingOccupied = 0.0d0
  END DO
  TotalAnyZoneNotMetHeating = 0.0d0
  TotalAnyZoneNotMetCooling = 0.0d0
  TotalAnyZoneNotMetHeatingOccupied = 0.0d0
  TotalAnyZoneNotMetCoolingOccupied = 0.0d0
  ! report how the aggregation is conducted
  SELECT CASE (kindOfSim)
    CASE(ksDesignDay)
      CALL addFootNoteSubTable(pdstUnmetLoads,'Aggregated over the Design Days')
    CASE(ksRunPeriodDesign)
      CALL addFootNoteSubTable(pdstUnmetLoads,'Aggregated over the RunPeriods for Design')
    CASE(ksRunPeriodWeather)
      CALL addFootNoteSubTable(pdstUnmetLoads,'Aggregated over the RunPeriods for Weather')
  END SELECT
END IF
END SUBROUTINE CalcIfSetpointMet

SUBROUTINE CalcThermalComfortAdaptiveASH55(initiate,wthrsim,avgdrybulb)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Tyler Hoyt
          !       DATE WRITTEN   July 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Sets up and carries out ASHRAE55-2010 adaptive comfort model calculations.
          ! Output provided are state variables for the 80% and 90% acceptability limits
          ! in the model, the comfort temperature, and the 30-day running average or
          ! monthly average outdoor air temperature as parsed from the .STAT file.

          ! METHODOLOGY EMPLOYED:
          ! In order for the calculations to be possible the user must provide either
          ! a .STAT file or .EPW file for the purpose of computing a monthly average
          ! temperature or thirty-day running average. The subroutine need only open
          ! the relevant file once to initialize, and then operates within the loop.

USE DataHVACGlobals, ONLY: SysTimeElapsed
USE General, ONLY: InvJulianDay
USE DataEnvironment, ONLY: OutDryBulbTemp, DayOfYear, Month
USE OutputReportTabular, ONLY: GetColumnUsingTabs, StrToReal

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
LOGICAL, INTENT(IN) :: initiate  ! true if supposed to initiate
LOGICAL, INTENT(IN), OPTIONAL :: wthrsim   ! true if this is a weather simulation
REAL(r64), INTENT(IN), OPTIONAL :: avgdrybulb  ! approximate avg drybulb for design day.  will be used as previous period in design day

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER, EXTERNAL :: GetNewUnitNumber

CHARACTER(len=200) :: lineIn
CHARACTER(len=200) :: lineAvg
CHARACTER(len=200) :: epwLine
CHARACTER(len=52) :: ioerrmsg
REAL(r64), SAVE :: avgDryBulbASH=0.0d0
REAL(r64) :: dryBulb
REAL(r64), SAVE :: runningAverageASH=0.0d0
REAL(r64), DIMENSION(12), SAVE :: monthlyTemp=0.0d0
REAL(r64) :: tComf
REAL(r64) :: numOccupants
INTEGER :: statFile
INTEGER :: epwFile
INTEGER :: lnPtr
INTEGER :: pMonth
INTEGER :: pDay
LOGICAL :: statFileExists
LOGICAL :: epwFileExists
LOGICAL, SAVE :: useStatData = .false.
LOGICAL, SAVE :: useEpwData = .false.
INTEGER :: readStat
INTEGER :: jStartDay
INTEGER :: calcStartDay
INTEGER :: calcStartHr
INTEGER :: calcEndDay
INTEGER :: calcEndHr
INTEGER :: pos
INTEGER :: ind
INTEGER :: i
INTEGER :: j
LOGICAL :: weathersimulation
REAL(r64) :: inavgdrybulb

IF (initiate) THEN  ! not optional on initiate=true.  would otherwise check for presence
  weathersimulation=wthrsim
  avgDryBulbASH=0.0d0
  runningAverageASH=0.0d0
  monthlyTemp=0.0d0
  inavgdrybulb=avgdrybulb
ELSE
  weathersimulation=.false.
  inavgdrybulb=0.0d0
ENDIF

IF (initiate .and. weathersimulation) THEN
  INQUIRE(file='in.stat',EXIST=statFileExists)
  INQUIRE(file='in.epw',EXIST=epwFileExists)
  readStat=0
  IF (statFileExists) THEN
    statFile = GetNewUnitNumber()
    OPEN (unit=statFile, file='in.stat', action='READ', iostat=readStat)
    IF (readStat /= 0) THEN
      CALL ShowFatalError('CalcThermalComfortAdaptiveASH55: Could not open file "in.stat" for input (read).')
    ENDIF
    DO WHILE (readStat == 0)
      READ(unit=statFile,fmt='(A)',iostat=readStat) lineIn
      lnPtr = INDEX(lineIn,'Monthly Statistics for Dry Bulb temperatures')
      IF (lnPtr > 0) THEN
        DO i = 1, 7
          READ(unit=statFile,fmt='(A)',iostat=readStat)
        END DO
        READ(unit=statFile,fmt='(A)',iostat=readStat) lineAvg
        EXIT
      ENDIF
    END DO
    CLOSE(unit=statFile)
    DO i = 1, 12
      monthlyTemp(i) =  StrToReal(GetColumnUsingTabs(lineAvg,i+2))
    END DO
    useStatData = .true.
  ELSE IF (epwFileExists) THEN
    epwFile = GetNewUnitNumber()
    OPEN (unit=epwFile, file='in.epw', action='READ', iostat=readStat)
    IF (readStat /= 0) THEN
      CALL ShowFatalError('CalcThermalComfortAdaptiveASH55: Could not open file "in.epw" for input (read).')
    ENDIF
    DO i = 1, 9 ! Headers
      READ(unit=epwFile,fmt='(A)',iostat=readStat)
    END DO
    jStartDay =  DayOfYear - 1
    calcStartDay = jStartDay - 30
    IF (calcStartDay > 0) THEN
      calcStartHr  = 24 * (calcStartDay - 1) + 1
      DO i = 1, calcStartHr-1
        READ(unit=epwFile,fmt='(A)',iostat=readStat)
      END DO
      DO i = 1, 30
        avgDryBulbASH = 0.0d0
        DO j = 1, 24
          READ(unit=epwFile,fmt='(A)',iostat=readStat) epwLine
          DO ind = 1, 6
            pos = INDEX(epwLine,',')
            epwLine = epwLine(pos+1:)
          END DO
          pos = INDEX(epwLine,',')
          dryBulb = StrToReal(epwLine(1:pos-1))
          avgDryBulbASH = avgDryBulbASH + (dryBulb / 24.0d0)
        END DO
        runningAverageASH = (29.0d0 * runningAverageASH + avgDryBulbASH) / 30.0d0
      END DO
    ELSE  ! Do special things for wrapping the epw
      calcEndDay = jStartDay
      calcStartDay = calcStartDay + 365
      calcEndHr = 24 * calcEndDay
      calcStartHr  = 24 * (calcStartDay - 1) + 1
      DO i = 1, calcEndDay
        avgDryBulbASH = 0.0d0
        DO j = 1, 24
          READ(unit=epwFile,fmt='(A)',iostat=readStat) epwLine
          DO ind = 1, 6
            pos = INDEX(epwLine,',')
            epwLine = epwLine(pos+1:)
          END DO
          pos = INDEX(epwLine,',')
          dryBulb = StrToReal(epwLine(1:pos-1))
          avgDryBulbASH = avgDryBulbASH + (dryBulb / 24.0d0)
        END DO
        runningAverageASH = (29.0d0 * runningAverageASH + avgDryBulbASH) / 30.0d0
      END DO
      DO i = calcEndHr+1, calcStartHr-1
        READ(unit=epwFile,fmt='(A)',iostat=readStat)
      END DO
      DO i = 1, 30-calcEndDay
        avgDryBulbASH = 0.0d0
        DO j = 1, 24
          READ(unit=epwFile,fmt='(A)',iostat=readStat) epwLine
          DO ind = 1, 6
            pos = INDEX(epwLine,',')
            epwLine = epwLine(pos+1:)
          END DO
          pos = INDEX(epwLine,',')
          dryBulb = StrToReal(epwLine(1:pos-1))
          avgDryBulbASH = avgDryBulbASH + (dryBulb / 24.0d0)
        END DO
        runningAverageASH = (29.0d0 * runningAverageASH + avgDryBulbASH) / 30.0d0
      END DO
    END IF
    CLOSE(unit=epwFile)
    useEpwData = .true.
  END IF
ELSEIF (initiate .and. .not. weathersimulation) THEN
  runningAverageASH=inavgdrybulb
  monthlyTemp=inavgdrybulb
  avgDryBulbASH = 0.0d0
END IF

IF (initiate) RETURN

IF (BeginDayFlag .and. useEpwData) THEN
  ! Update the running average, reset the daily avg
  runningAverageASH = (29.0d0 * runningAverageASH + avgDryBulbASH) / 30.0d0
  avgDryBulbASH = 0.0d0
END IF

! If exists BeginMonthFlag we can use it to call InvJulianDay once per month.
IF (BeginDayFlag .and. useStatData) THEN
!  CALL InvJulianDay(DayOfYear,pMonth,pDay,0)
!  runningAverageASH = monthlyTemp(pMonth)
  runningAverageASH = monthlyTemp(Month)
END IF

! Update the daily average
!IF (BeginHourFlag .and. useEpwData) THEN
IF (BeginHourFlag) THEN
  avgDryBulbASH = avgDryBulbASH + (OutDryBulbTemp / 24.0d0)
END IF

DO PeopleNum = 1, TotPeople
  IF(.NOT. People(PeopleNum)%AdaptiveASH55) CYCLE
  ZoneNum = People(PeopleNum)%ZonePtr
  IF (IsZoneDV(ZoneNum) .or. IsZoneUI(ZoneNum)) THEN
    AirTemp = TCMF(ZoneNum)
  ELSE
    AirTemp = ZTAV(ZoneNum)
  ENDIF
  RadTemp = CalcRadTemp(PeopleNum)
  OpTemp = (AirTemp + RadTemp) / 2.0d0
  ThermalComfortData(PeopleNum)%ThermalComfortOpTemp = OpTemp
  ThermalComfortData(PeopleNum)%ASHRAE55RunningMeanOutdoorTemp = runningAverageASH
  IF (runningAverageASH >= 10.0d0 .and. runningAverageASH <= 33.5d0) THEN
    ! Calculate the comfort here  (people/output handling loop)
    numOccupants = People(PeopleNum)%NumberOfPeople * GetCurrentScheduleValue(People(PeopleNum)%NumberOfPeoplePtr)
    tComf = 0.31d0 * runningAverageASH + 17.8d0
    ThermalComfortData(PeopleNum)%TComfASH55 = tComf
    IF (numOccupants > 0) THEN
      IF (OpTemp < tComf+2.5d0 .and. OpTemp > tComf-2.5d0) THEN
        ! 80% and 90% limits okay
        ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveASH5590 = 1
        ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveASH5580 = 1
      ELSE IF (OpTemp < tComf+3.5d0 .and. OpTemp > tComf-3.5d0) THEN
        ! 80% only
        ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveASH5590 = 0
        ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveASH5580 = 1
        People(PeopleNum)%TimeNotMetASH5590 = People(PeopleNum)%TimeNotMetASH5590 + SysTimeElapsed
      ELSE
        ! Neither
        ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveASH5590 = 0
        ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveASH5580 = 0
        People(PeopleNum)%TimeNotMetASH5580 = People(PeopleNum)%TimeNotMetASH5580 + SysTimeElapsed
        People(PeopleNum)%TimeNotMetASH5590 = People(PeopleNum)%TimeNotMetASH5590 + SysTimeElapsed
      END IF
    ELSE
      ! Unoccupied
      ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveASH5590 = -1
      ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveASH5580 = -1
    END IF
  ELSE
    ! Monthly temp out of range
    ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveASH5590 = -1
    ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveASH5580 = -1
    ThermalComfortData(PeopleNum)%TComfASH55 = -1.0d0
  END IF
END DO

END SUBROUTINE CalcThermalComfortAdaptiveASH55


SUBROUTINE CalcThermalComfortAdaptiveCEN15251(initiate,wthrsim,avgdrybulb)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Tyler Hoyt
          !       DATE WRITTEN   July 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Sets up and carries out CEN-15251 adaptive comfort model calculations.
          ! Output provided are state variables for the Category I, II, and III
          ! limits of the model, the comfort temperature, and the 5-day weighted
          ! moving average of the outdoor air temperature.

          ! METHODOLOGY EMPLOYED:
          !   na

USE DataHVACGlobals, ONLY: SysTimeElapsed
USE DataEnvironment, ONLY: OutDryBulbTemp, DayOfYear, Month
USE OutputReportTabular, ONLY: GetColumnUsingTabs, StrToReal

IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
LOGICAL, INTENT(IN) :: initiate  ! true if supposed to initiate
LOGICAL, INTENT(IN), OPTIONAL :: wthrsim   ! true if this is a weather simulation
REAL(r64), INTENT(IN), OPTIONAL :: avgdrybulb  ! approximate avg drybulb for design day.  will be used as previous period in design day

          ! SUBROUTINE PARAMETER DEFINITIONS:
REAL(r64),PARAMETER :: alpha = 0.8d0

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER, EXTERNAL :: GetNewUnitNumber
CHARACTER(len=200) :: epwLine
REAL(r64), SAVE :: avgDryBulbCEN=0.0d0
REAL(r64) :: dryBulb
REAL(r64) :: tComf
REAL(r64) :: tComfLow
REAL(r64), SAVE :: runningAverageCEN=0.0d0
REAL(r64) :: numOccupants
INTEGER :: epwFile
LOGICAL :: epwFileExists
LOGICAL, SAVE :: useEpwData = .false.
LOGICAL, SAVE :: firstDaySet=.false.   ! first day is set with initiate -- so do not update
INTEGER :: readStat
INTEGER :: jStartDay
INTEGER :: calcStartDay
INTEGER :: calcStartHr
INTEGER :: calcEndDay
INTEGER :: calcEndHr
INTEGER :: pos
INTEGER :: ind
INTEGER :: i
INTEGER :: j
LOGICAL :: weathersimulation
REAL(r64) :: inavgdrybulb

IF (initiate) THEN  ! not optional on initiate=true.  would otherwise check for presence
  weathersimulation=wthrsim
  inavgdrybulb=avgdrybulb
  avgDryBulbCEN=0.0d0
  runningAverageCEN=0.0d0
ELSE
  weathersimulation=.false.
  inavgdrybulb=0.0d0
ENDIF

IF (initiate .and. weathersimulation) THEN
  INQUIRE(file='in.epw',EXIST=epwFileExists)
  readStat=0
  IF (epwFileExists) THEN
    epwFile = GetNewUnitNumber()
    OPEN (unit=epwFile, file='in.epw', action='READ', iostat=readStat)
    IF (readStat /= 0) THEN
      CALL ShowFatalError('CalcThermalComfortAdaptiveCEN15251: Could not open file "in.epw" for input (read).')
    ENDIF
    DO i = 1, 9 ! Headers
      READ(unit=epwFile,fmt='(A)',iostat=readStat)
    END DO
    jStartDay =  DayOfYear - 1
    calcStartDay = jStartDay - 7
    IF (calcStartDay > 0) THEN
      calcStartHr  = 24 * (calcStartDay - 1) + 1
      DO i = 1, calcStartHr - 1
        READ(unit=epwFile,fmt='(A)',iostat=readStat)
      END DO
      runningAverageCEN = 0.0d0
      DO i = 1, 7
        avgDryBulbCEN = 0.0d0
        DO j = 1, 24
          READ(unit=epwFile,fmt='(A)',iostat=readStat) epwLine
          DO ind = 1, 6
            pos = INDEX(epwLine,',')
            epwLine = epwLine(pos+1:)
          END DO
          pos = INDEX(epwLine,',')
          dryBulb = StrToReal(epwLine(1:pos-1))
          avgDryBulbCEN = avgDryBulbCEN + (dryBulb / 24.0d0)
        END DO
        runningAverageCEN = runningAverageCEN + alpha**(7-i)*avgDryBulbCEN
      END DO
    ELSE  ! Do special things for wrapping the epw
      calcEndDay = jStartDay
      calcStartDay = calcStartDay + 365
      calcEndHr = 24 * calcEndDay
      calcStartHr  = 24 * (calcStartDay - 1) + 1
      DO i = 1, calcEndDay
        avgDryBulbCEN = 0.0d0
        DO j = 1, 24
          READ(unit=epwFile,fmt='(A)',iostat=readStat) epwLine
          DO ind = 1, 6
            pos = INDEX(epwLine,',')
            epwLine = epwLine(pos+1:)
          END DO
          pos = INDEX(epwLine,',')
          dryBulb = StrToReal(epwLine(1:pos-1))
          avgDryBulbCEN = avgDryBulbCEN + (dryBulb / 24.0d0)
        END DO
        runningAverageCEN = runningAverageCEN + alpha**(calcEndDay-i)*avgDryBulbCEN
      END DO
      DO i = calcEndHr+1, calcStartHr-1
        READ(unit=epwFile,fmt='(A)',iostat=readStat)
      END DO
      DO i = 1, 7-calcEndDay
        avgDryBulbCEN = 0.0d0
        DO j = 1, 24
          READ(unit=epwFile,fmt='(A)',iostat=readStat) epwLine
          DO ind = 1, 6
            pos = INDEX(epwLine,',')
            epwLine = epwLine(pos+1:)
          END DO
          pos = INDEX(epwLine,',')
          dryBulb = StrToReal(epwLine(1:pos-1))
          avgDryBulbCEN = avgDryBulbCEN + (dryBulb / 24.0d0)
        END DO
        runningAverageCEN = runningAverageCEN + alpha**(7-i)*avgDryBulbCEN
      END DO
    END IF
    runningAverageCEN = (1.0d0-alpha) * runningAverageCEN
    avgDryBulbCEN = 0.0d0
    CLOSE(unit=epwFile)
    useEpwData = .true.
    firstDaySet=.true.
  END IF
ELSEIF (initiate .and. .not. weathersimulation) THEN
  runningAverageCEN = inavgdrybulb
  avgDryBulbCEN = 0.0d0
ENDIF
IF (initiate) RETURN

IF (BeginDayFlag .and. .not. firstDaySet) THEN
  ! Update the running average, reset the daily avg
  runningAverageCEN = 0.2d0 * runningAverageCEN + 0.8d0 * avgDryBulbCEN
  avgDryBulbCEN = 0.0d0
END IF

firstDaySet=.false.

! Update the daily average
IF (BeginHourFlag) THEN
  avgDryBulbCEN = avgDryBulbCEN + (OutDryBulbTemp / 24.0d0)
ENDIF


DO PeopleNum = 1, TotPeople
  IF(.NOT. People(PeopleNum)%AdaptiveCEN15251) CYCLE
  ZoneNum = People(PeopleNum)%ZonePtr
  IF (IsZoneDV(ZoneNum) .or. IsZoneUI(ZoneNum)) THEN
    AirTemp = TCMF(ZoneNum)
  ELSE
    AirTemp = ZTAV(ZoneNum)
  END IF
  RadTemp = CalcRadTemp(PeopleNum)
  OpTemp = (AirTemp + RadTemp) / 2.0d0
  ThermalComfortData(PeopleNum)%ThermalComfortOpTemp = OpTemp
  ThermalComfortData(PeopleNum)%CEN15251RunningMeanOutdoorTemp = runningAverageCEN
  IF (runningAverageCEN >= 10.0d0 .and. runningAverageCEN <= 30.0d0) THEN
    ! Calculate the comfort here (people/output handling loop)
    numOccupants = People(PeopleNum)%NumberOfPeople * GetCurrentScheduleValue(People(PeopleNum)%NumberOfPeoplePtr)
    tComf = 0.33d0 * runningAverageCEN + 18.8d0
    ThermalComfortData(PeopleNum)%TComfCEN15251 = tComf
    IF (numOccupants > 0) THEN
      IF (runningAverageCEN < 15) THEN
        tComfLow = 23.75d0 ! Lower limit is constant in this region
      ELSE
        tComfLow = tComf
      END IF
      IF (OpTemp < tComf+2.0d0 .and. OpTemp > tComfLow-2.0d0) THEN
        ! Within Cat I, II, III Limits
        ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveCEN15251CatI = 1
        ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveCEN15251CatII = 1
        ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveCEN15251CatIII = 1
      ELSE IF (OpTemp < tComf+3.0d0 .and. OpTemp > tComfLow-3.0d0) THEN
        ! Within Cat II, III Limits
        ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveCEN15251CatI = 0
        ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveCEN15251CatII = 1
        ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveCEN15251CatIII = 1
        People(PeopleNum)%TimeNotMetCEN15251CatI = People(PeopleNum)%TimeNotMetCEN15251CatI + SysTimeElapsed
      ELSE IF (OpTemp < tComf+4.0d0 .and. OpTemp > tComfLow-4.0d0) THEN
        ! Within Cat III Limits
        ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveCEN15251CatI = 0
        ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveCEN15251CatII = 0
        ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveCEN15251CatIII = 1
        People(PeopleNum)%TimeNotMetCEN15251CatI = People(PeopleNum)%TimeNotMetCEN15251CatI + SysTimeElapsed
        People(PeopleNum)%TimeNotMetCEN15251CatII = People(PeopleNum)%TimeNotMetCEN15251CatII + SysTimeElapsed
      ELSE
        ! None
        ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveCEN15251CatI = 0
        ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveCEN15251CatII = 0
        ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveCEN15251CatIII = 0
        People(PeopleNum)%TimeNotMetCEN15251CatI = People(PeopleNum)%TimeNotMetCEN15251CatI + SysTimeElapsed
        People(PeopleNum)%TimeNotMetCEN15251CatII = People(PeopleNum)%TimeNotMetCEN15251CatII + SysTimeElapsed
        People(PeopleNum)%TimeNotMetCEN15251CatIII = People(PeopleNum)%TimeNotMetCEN15251CatIII + SysTimeElapsed
      END IF
    ELSE
      ! Unoccupied
      ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveCEN15251CatI = -1
      ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveCEN15251CatII = -1
      ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveCEN15251CatIII = -1
    END IF
  ELSE
    ! Monthly temp out of range
    ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveCEN15251CatI = -1
    ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveCEN15251CatII = -1
    ThermalComfortData(PeopleNum)%ThermalComfortAdaptiveCEN15251CatIII = -1
    ThermalComfortData(PeopleNum)%TComfCEN15251 = -1.0d0
  END IF
END DO

END SUBROUTINE CalcThermalComfortAdaptiveCEN15251


SUBROUTINE DynamicClothingModel
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Kwang Ho Lee
          !       DATE WRITTEN   June 2013
          !       MODIFIED
          !       RE-ENGINEERED  na

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:

! USE DataRoomAirModel, ONLY: AirModel, RoomAirModel_Mixing

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
REAL(r64) :: TemporaryVariable

 IF (TemporarySixAMTemperature < -5.0d0) THEN
    ThermalComfortData(PeopleNum)%ClothingValue = 1.0d0
 ELSE IF ((TemporarySixAMTemperature >= -5.0d0).AND.(TemporarySixAMTemperature < 5.0d0)) THEN
    ThermalComfortData(PeopleNum)%ClothingValue = 0.818d0 - 0.0364d0 * TemporarySixAMTemperature
 ELSE IF ((TemporarySixAMTemperature >= 5.0d0).AND.(TemporarySixAMTemperature < 26.0d0)) THEN
    TemporaryVariable = -0.1635d0 - 0.0066d0 * TemporarySixAMTemperature
    ThermalComfortData(PeopleNum)%ClothingValue = 10.0d0**(TemporaryVariable)
 ELSE IF (TemporarySixAMTemperature >= 26.0d0) THEN
    ThermalComfortData(PeopleNum)%ClothingValue = 0.46d0
 END IF

END SUBROUTINE DynamicClothingModel

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

END MODULE ThermalComfort


