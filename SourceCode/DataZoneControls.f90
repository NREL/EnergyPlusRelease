MODULE DataZoneControls

          ! Module containing the routines dealing with the zone controls.

          ! MODULE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module has the data and structures for various types of controls
          ! (humidity, temperature, comfort) within the zones.  This data was formerly
          ! public data in ZoneTempPredictorCorrector.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataGlobals, ONLY: MaxNameLength
          ! <use statements for access to subroutines in other modules>

IMPLICIT NONE ! Enforce explicit typing of all variables

PUBLIC ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
  TYPE :: ZoneTempControls
    CHARACTER(len=MaxNameLength) :: Name                 =' ' ! Name of the thermostat
    CHARACTER(len=MaxNameLength) :: ZoneName             =' ' ! Name of the zone
    INTEGER                      :: ActualZoneNum        =0
    CHARACTER(len=MaxNameLength) :: ControlTypeSchedName =' ' ! Name of the schedule which determines the zone temp setpoint
    INTEGER :: CTSchedIndex                              =0   ! Index for this schedule
    INTEGER :: NumControlTypes                           =0
    CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: ControlType
    CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: ControlTypeName
    INTEGER, DIMENSION(:), ALLOCATABLE :: ControlTypeSchIndx
    INTEGER :: SchIndx_SingleHeatSetPoint                =0
    INTEGER :: SchIndx_SingleCoolSetPoint                =0
    INTEGER :: SchIndx_SingleHeatCoolSetPoint            =0
    INTEGER :: SchIndx_DualSetPointWDeadBand             =0


    LOGICAL :: ManageDemand                          =.FALSE. ! Flag to indicate whether to use demand limiting
    REAL(r64) :: HeatingResetLimit                       =0.0d0 ! Lowest heating setpoint that can be set by demand manager [C]
    REAL(r64) :: CoolingResetLimit                       =0.0d0 ! Highest cooling setpoint that can be set by demand manager [C]
    LOGICAL   :: EMSOverrideHeatingSetpointOn          = .FALSE. ! EMS is calling to override heating setpoint
    REAL(r64) :: EMSOverrideHeatingSetpointValue       = 0.0D0   ! value EMS is directing to use for heating setpoint [C]
    LOGICAL   :: EMSOverrideCoolingSetpointOn          = .FALSE. ! EMS is calling to override cooling setpoint
    REAL(r64) :: EMSOverrideCoolingSetpointValue       = 0.0D0   ! value EMS is directing to use for cooling setpoint [C]

    LOGICAL :: OperativeTempControl                      = .FALSE.  ! flag to indicate whether control based on Operative Temp
    LOGICAL :: OpTempCntrlModeScheduled                  = .FALSE.  ! flag to indicate if radiative fraction is scheduled,
                                                                    ! else constant
    REAL(r64) :: FixedRadiativeFraction                  = 0.0d0  ! weighting factor for mean radiant temp for Operative temperature
    INTEGER :: OpTempRadiativeFractionSched              = 0 ! index of schedule for when fraction is scheduled


    REAL(r64) :: ZoneOvercoolRange                     = 0.0d0   ! Zone overcool temperature range (max), deg C
    LOGICAL   :: ZoneOvercoolControl                   = .FALSE. ! Flag to indicate whether control is based on overcool
    LOGICAL   :: OvercoolCntrlModeScheduled            = .FALSE. ! Flag to indicate if zone overcool range is scheduled
                                                                 !   or constant
    REAL(r64) :: ZoneOvercoolConstRange                = 0.0d0   ! Overcool Range for Zone Air Setpoint Temperature [deltaC]
    INTEGER   :: ZoneOvercoolRangeSchedIndex           = 0       ! Index for Overcool Range Schedule
    REAL(r64) :: ZoneOvercoolControlRatio              = 0.0d0   ! Zone relative humidity shift per dry-bulb temperature overcooling
                                                                 !      below the original cooling setpoint, %RH/deltaC
    CHARACTER(len=MaxNameLength) :: DehumidifyingSched =' '  ! Name of the schedule to determine the zone dehumidifying setpoint
    INTEGER                 :: DehumidifyingSchedIndex =0    ! Index for dehumidifying schedule

  END TYPE ZoneTempControls

  TYPE :: ZoneHumidityControls
    CHARACTER(len=MaxNameLength) :: ControlName           =' ' ! Name of this humidity controller
    CHARACTER(len=MaxNameLength) :: ZoneName              =' ' ! Name of the zone
    CHARACTER(len=MaxNameLength) :: HumidifyingSched    =' '   ! Name of the schedule to determine the zone humidifying setpoint
    CHARACTER(len=MaxNameLength) :: DehumidifyingSched  =' '   ! Name of the schedule to determine the zone dehumidifying setpoint
    INTEGER                      :: ActualZoneNum         =0
    INTEGER                      :: HumidifyingSchedIndex   =0 ! Index for humidifying schedule
    INTEGER                      :: DehumidifyingSchedIndex =0 ! Index for dehumidifying schedule
    INTEGER                      :: ErrorIndex = 0             ! Error index when LowRH setpoint > HighRH setpoint
    LOGICAL   :: EMSOverrideHumidifySetpointOn          = .FALSE. ! EMS is calling to override humidifying setpoint
    REAL(r64) :: EMSOverrideHumidifySetpointValue       = 0.0D0   ! value EMS is directing to use for humidifying setpoint
    LOGICAL   :: EMSOverrideDehumidifySetpointOn        = .FALSE. ! EMS is calling to override dehumidifying setpoint
    REAL(r64) :: EMSOverrideDehumidifySetpointValue     = 0.0D0   ! value EMS is directing to use for dehumidifying setpoint
  END TYPE ZoneHumidityControls


  TYPE :: ZoneComfortControls
    CHARACTER(len=MaxNameLength) :: Name                 =' ' ! Name of the thermostat
    CHARACTER(len=MaxNameLength) :: ZoneName             =' ' ! Name of the zone
    INTEGER                      :: ActualZoneNum        =0   ! Index number of zone
    CHARACTER(len=MaxNameLength) :: ControlTypeSchedName =' ' ! Name of the schedule which determines the zone temp setpoint
    INTEGER :: ComfortSchedIndex                              =0   ! Index for this schedule
    INTEGER :: NumControlTypes                           =0   ! Number of control types in ZoneControl:ThermalComfort object
    CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: ControlType     ! Type of control
    CHARACTER(len=MaxNameLength), DIMENSION(:), ALLOCATABLE :: ControlTypeName ! Name of control type
    INTEGER, DIMENSION(:), ALLOCATABLE :: ControlTypeSchIndx  ! Index to control type schedule
    INTEGER :: SchIndx_SglHeatSetPointFanger             =0   ! Index to fanger single heating setpoint schedule
    INTEGER :: SchIndx_SglCoolSetPointFanger             =0   ! Index to fanger single cooling setpoint schedule
    INTEGER :: SchIndx_SglHCSetPointFanger               =0   ! Index to fanger single heating/cooling setpoint schedule
    INTEGER :: SchIndx_DualSetPointFanger                =0   ! Index to fanger dual setpoint schedule
    INTEGER :: SchIndx_SglHeatSetPointPierce             =0   ! Index to pierce single heating setpoint schedule
    INTEGER :: SchIndx_SglCoolSetPointPierce             =0   ! Index to pierce single cooling setpoint schedule
    INTEGER :: SchIndx_SglHCSetPointPierce               =0   ! Index to pierce single heating/cooling setpoint schedule
    INTEGER :: SchIndx_DualSetPointPierce                =0   ! Index to pierce dual setpoint schedule
    INTEGER :: SchIndx_SglHeatSetPointKSU                =0   ! Index to KSU single heating setpoint schedule
    INTEGER :: SchIndx_SglCoolSetPointKSU                =0   ! Index to KSU single cooling setpoint schedule
    INTEGER :: SchIndx_SglHCSetPointKSU                  =0   ! Index to KSU single heating/cooling setpoint schedule
    INTEGER :: SchIndx_DualSetPointKSU                   =0   ! Index to KSU dual setpoint schedule


    LOGICAL :: ManageDemand                          =.FALSE. ! Flag to indicate whether to use demand limiting
    REAL(r64) :: HeatingResetLimit                            =0.0d0 ! Lowest heating setpoint that can be set by demand manager [C]
    REAL(r64) :: CoolingResetLimit                            =0.0d0 ! Highest cooling setpoint that can be set by demand manager [C]
    LOGICAL   :: EMSOverrideHeatingSetpointOn          = .FALSE. ! EMS is calling to override heating setpoint
    REAL(r64) :: EMSOverrideHeatingSetpointValue       = 0.0D0   ! value EMS is directing to use for heating setpoint
    LOGICAL   :: EMSOverrideCoolingSetpointOn          = .FALSE. ! EMS is calling to override cooling setpoint
    REAL(r64) :: EMSOverrideCoolingSetpointValue       = 0.0D0   ! value EMS is directing to use for cooling setpoint

    REAL(r64) :: TdbMaxSetPoint                              =50.0d0 ! Maximum dry-bulb temperature setpoint [C]
    REAL(r64) :: TdbMinSetPoint                              = 0.0d0 ! Minimum dry-bulb temperature setpoint [C]
    CHARACTER(len=MaxNameLength) :: AverageMethodName ='PEOPLE AVERGAE' ! Averaging Method for Zones with Multiple People Objects
    CHARACTER(len=MaxNameLength) :: AverageObjectName =' '    ! Object Name for Specific Object Average
    INTEGER :: AverageMethodNum                         =0    ! Numerical value for averaging method
    INTEGER :: SpecificObjectNum                        =0    ! People Object number used for Specific people object choice
    INTEGER :: PeopleAverageErrIndex =0                       ! People average error index
    INTEGER :: TdbMaxErrIndex =0                              ! Single cooling setpoint error index
    INTEGER :: TdbMinErrIndex =0                              ! Single heating setpoint error index
    INTEGER :: TdbHCErrIndex =0                               ! Single heating cooling setpoint error index
    INTEGER :: TdbDualMaxErrIndex =0                          ! Dual cooling setpoint error index
    INTEGER :: TdbDualMinErrIndex =0                          ! Dual heating setpoint error index
  END TYPE ZoneComfortControls

  TYPE :: ZoneSatgedControls
    CHARACTER(len=MaxNameLength) :: Name                 =' ' ! Name of the thermostat
    CHARACTER(len=MaxNameLength) :: ZoneName             =' ' ! Name of the zone
    INTEGER                      :: ActualZoneNum        =0   ! Index number of zone
    CHARACTER(len=MaxNameLength) :: HeatSetBaseSchedName =' ' ! Name of the schedule which provides zone heating setpoint base
    INTEGER :: HSBchedIndex                              =0   ! Index for this schedule
    CHARACTER(len=MaxNameLength) :: CoolSetBaseSchedName =' ' ! Name of the schedule which provides zone cooling setpoint base
    INTEGER :: CSBchedIndex                              =0   ! Index for this schedule
    INTEGER :: NumOfHeatStages                           =0   ! Number of heating stages
    INTEGER :: NumOfCoolStages                           =0   ! Number of cooling stages
    REAL(r64) :: HeatThroRange                           =0.0 ! Heating throttling tempeature range
    REAL(r64) :: CoolThroRange                           =0.0 ! Cooling throttling tempeature range
    REAL(r64), DIMENSION(:), ALLOCATABLE :: HeatTOffset       ! Heating temperature offset
    REAL(r64), DIMENSION(:), ALLOCATABLE :: CoolTOffset       ! Cooling temperature offset
    REAL(r64) :: HeatSetpoint                            =0.0 ! Heating throttling tempeature range
    REAL(r64) :: CoolSetpoint                            =0.0 ! Cooling throttling tempeature range
    INTEGER :: StageErrCount                             =0   ! Staged setpoint erro count
    INTEGER :: StageErrIndex                             =0   ! Staged setpoint erro index
  END TYPE ZoneSatgedControls
  
  TYPE TStatObject
    CHARACTER(len=MaxNameLength) :: Name  =' '
    INTEGER :: ZoneOrZoneListPtr          =0
    INTEGER :: NumOfZones                 =0
    INTEGER :: TempControlledZoneStartPtr =0
    INTEGER :: ComfortControlledZoneStartPtr =0
    INTEGER :: StageControlledZoneStartPtr =0
    LOGICAL :: ZoneListActive             =.false.
  END TYPE


          ! MODULE VARIABLE DECLARATIONS:
  TYPE (ZoneHumidityControls), ALLOCATABLE, DIMENSION(:) :: HumidityControlZone
  TYPE (ZoneTempControls), ALLOCATABLE, DIMENSION(:)     :: TempControlledZone
  TYPE (ZoneComfortControls), ALLOCATABLE, DIMENSION(:)  :: ComfortControlledZone
  TYPE (TStatObject), ALLOCATABLE, DIMENSION(:) :: TStatObjects
  TYPE (TStatObject), ALLOCATABLE, DIMENSION(:) :: ComfortTStatObjects
  TYPE (TStatObject), ALLOCATABLE, DIMENSION(:) :: StagedTStatObjects
  TYPE (ZoneSatgedControls), ALLOCATABLE, DIMENSION(:)     :: StageControlledZone
  INTEGER :: NumTempControlledZones  =0
  INTEGER :: NumHumidityControlZones =0
  INTEGER :: NumComfortControlledZones  =0
  INTEGER :: NumTStatStatements = 0
  INTEGER :: NumComfortTStatStatements = 0
  INTEGER :: NumOpTempControlledZones =0             ! number of zones with operative temp control
  INTEGER :: NumTempAndHumidityControlledZones = 0   ! number of zones with over cool control
  LOGICAL :: AnyOpTempControl = .FALSE.              ! flag set true if any zones have op temp control
  LOGICAL :: AnyZoneTempAndHumidityControl = .FALSE. ! flag set true if any zones have over cool control
  LOGICAL, ALLOCATABLE, DIMENSION(:)   :: StageZoneLogic      ! Logical array, A zone with staged thermostat = .true.
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: OccRoomTSetPointHeat ! occupied heating set point for optimum start period
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: OccRoomTSetPointCool ! occupied cooling set point for optimum start period
  LOGICAL :: GetZoneAirStatsInputFlag = .TRUE.  ! True when need to get input
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

END MODULE DataZoneControls

