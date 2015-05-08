MODULE SystemAvailabilityManager

  ! Module containing the System Availability Manager routines

  ! MODULE INFORMATION:
  !       AUTHOR         Fred Buhl
  !       DATE WRITTEN   August 2001
  !       MODIFIED       February 2004, PGE: Added plant managers.
  !       MODIFIED       March 2007, LG: Added hybrid ventilation control.
  !                      August 2008, R. Raustad - FSEC: added 2 new scheduled sys avail managers
  !                      March 2011, Chandan Sharma - FSEC: Added zone sys avail managers
  !                      August 2013, Xiufeng Pang (XP) - added algorithms for optimal start
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE
  ! To encapsulate the data and algorithms required to
  ! determine system (loop) availability and "cycle on" status.

  ! METHODOLOGY EMPLOYED:
  ! Previous time step node data and current zone thermostat setpoints are used
  ! in a set of fixed, precoded algorithms to determine the current time step
  ! on/off status of systems and loops.

  ! USE STATEMENTS:
  ! Use statements for data only modules
  USE DataPrecisionGlobals
  USE DataGlobals
  USE DataHVACGlobals
  USE DataInterfaces

  ! Use statements for access to subroutines in other modules
  USE ScheduleManager
  USE DataAirSystems, ONLY : PrimaryAirSystem
  USE DataHeatBalance, ONLY: ZoneList

  IMPLICIT NONE ! Enforce explicit typing of all variables

  PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS
  INTEGER, PARAMETER :: MaxDayTypes=12
  INTEGER, PARAMETER :: StayOff = 0
  INTEGER, PARAMETER :: CycleOnAny = 1
  INTEGER, PARAMETER :: CycleOnControlZone = 2
  INTEGER, PARAMETER :: ZoneFansOnly = 3

  ! Optimum start parameter definations
  INTEGER, PARAMETER :: ControlZone = 4
  INTEGER, PARAMETER :: MaximumOfZoneList = 5

  INTEGER, PARAMETER :: ConstantTemperatureGradient = 0
  INTEGER, PARAMETER :: AdaptiveTemperatureGradient = 1
  INTEGER, PARAMETER :: AdaptiveASHRAE = 2
  INTEGER, PARAMETER :: ConstantStartTime = 3

  ! Hybrid Ventilation parameters
  INTEGER, PARAMETER :: HybridVentMode_No       = 0  ! No hybrid ventilation control
  INTEGER, PARAMETER :: HybridVentMode_Temp     = 1  ! Temperature control
  INTEGER, PARAMETER :: HybridVentMode_Enth     = 2  ! Enthalpy control
  INTEGER, PARAMETER :: HybridVentMode_DewPoint = 3  ! Dew point control
  INTEGER, PARAMETER :: HybridVentMode_OA       = 4  ! Outdoor air control

  INTEGER, PARAMETER :: HybridVentCtrl_Noaction = 0  ! No hybrid ventilation control
  INTEGER, PARAMETER :: HybridVentCtrl_Open     = 1  ! Open windows or doors
  INTEGER, PARAMETER :: HybridVentCtrl_Close    = 2  ! Close windows or doors

  INTEGER, PARAMETER :: NumValidSysAvailManagerTypes=12
  CHARACTER(len=*), PARAMETER, DIMENSION(NumValidSysAvailManagerTypes) :: cValidSysAvailManagerTypes=    &
                 (/'AvailabilityManager:Scheduled                ',  &
                   'AvailabilityManager:ScheduledOn              ',  &
                   'AvailabilityManager:ScheduledOff             ',  &
                   'AvailabilityManager:NightCycle               ',  &
                   'AvailabilityManager:DifferentialThermostat   ',  &
                   'AvailabilityManager:HighTemperatureTurnOff   ',  &
                   'AvailabilityManager:HighTemperatureTurnOn    ',  &
                   'AvailabilityManager:LowTemperatureTurnOff    ',  &
                   'AvailabilityManager:LowTemperatureTurnOn     ',  &
                   'AvailabilityManager:NightVentilation         ',  &
                   'AvailabilityManager:HybridVentilation        ',  &
                   'AvailabilityManager:OptimumStart             '/)
  INTEGER, PARAMETER :: SysAvailMgr_Scheduled    = 1
  INTEGER, PARAMETER :: SysAvailMgr_ScheduledOn  = 2
  INTEGER, PARAMETER :: SysAvailMgr_ScheduledOff = 3
  INTEGER, PARAMETER :: SysAvailMgr_NightCycle   = 4
  INTEGER, PARAMETER :: SysAvailMgr_DiffThermo   = 5
  INTEGER, PARAMETER :: SysAvailMgr_HiTempTOff   = 6
  INTEGER, PARAMETER :: SysAvailMgr_HiTempTOn    = 7
  INTEGER, PARAMETER :: SysAvailMgr_LoTempTOff   = 8
  INTEGER, PARAMETER :: SysAvailMgr_LoTempTOn    = 9
  INTEGER, PARAMETER :: SysAvailMgr_NightVent    = 10
  INTEGER, PARAMETER :: SysAvailMgr_HybridVent   = 11

  INTEGER, PARAMETER :: SysAvailMgr_OptimumStart = 12
  INTEGER, PARAMETER, DIMENSION(NumValidSysAvailManagerTypes) :: ValidSysAvailManagerTypes=  (/  &
           SysAvailMgr_Scheduled,    &
           SysAvailMgr_ScheduledOn,  &
           SysAvailMgr_ScheduledOff, &
           SysAvailMgr_NightCycle,   &
           SysAvailMgr_DiffThermo,   &
           SysAvailMgr_HiTempTOff,   &
           SysAvailMgr_HiTempTOn,    &
           SysAvailMgr_LoTempTOff,   &
           SysAvailMgr_LoTempTOn,    &
           SysAvailMgr_NightVent,    &
           SysAvailMgr_HybridVent,   &
           SysAvailMgr_OptimumStart/)
  ! DERIVED TYPE DEFINITIONS
  TYPE DefineSchedSysAvailManager                  ! Derived type for Scheduled Sys Avail Managers
    CHARACTER(len=MaxNameLength) :: Name              = ' ' ! Name of the manager object
    INTEGER                      :: MgrType           = 0   ! Integer equivalent of availability manager type
    INTEGER                      :: SchedPtr          = 0   ! Schedule pointer
    INTEGER                      :: AvailStatus       = 0   ! reports status of availability manager
  END TYPE DefineSchedSysAvailManager

  TYPE DefineSchedOnSysAvailManager                ! Derived type for Scheduled On Sys Avail Managers
    CHARACTER(len=MaxNameLength) :: Name              = ' ' ! Name of the manager object
    INTEGER                      :: MgrType           = 0   ! Integer equivalent of availability manager type
    INTEGER                      :: SchedPtr          = 0   ! Schedule pointer
    INTEGER                      :: AvailStatus       = 0   ! reports status of availability manager
  END TYPE DefineSchedOnSysAvailManager

  TYPE DefineSchedOffSysAvailManager               ! Derived type for Scheduled Off Sys Avail Managers
    CHARACTER(len=MaxNameLength) :: Name              = ' ' ! Name of the manager object
    INTEGER                      :: MgrType           = 0   ! Integer equivalent of availability manager type
    INTEGER                      :: SchedPtr          = 0   ! Schedule pointer
    INTEGER                      :: AvailStatus       = 0   ! reports status of availability manager
  END TYPE DefineSchedOffSysAvailManager

  TYPE DefineNightCycSysAvailManager               ! Derived type for Night Cycle Sys Avail Managers
    CHARACTER(len=MaxNameLength) :: Name              = ' '  ! Name of the manager object
    INTEGER                      :: MgrType           = 0   ! Integer equivalent of availability manager type
    INTEGER                      :: SchedPtr          = 0    ! Applicability schedule pointer
    CHARACTER(len=MaxNameLength) :: FanSched          = ' '  ! Fan schedule name
    INTEGER                      :: FanSchedPtr       = 0    ! Fan schedule pointer
    INTEGER                      :: CtrlType          = 0    ! type of control: Stay Off, Cycle On Any,
                                                            !   Cycle On Control Zone, or Cycle On Any - Zone Fans Only
    REAL(r64)                    :: TempTolRange      = 1.0d0  ! range in degrees C of thermostat tolerance
    INTEGER                      :: CyclingTimeSteps  = 1    ! period (in Loads time steps) system will cycle on.
    CHARACTER(len=MaxNameLength) :: CtrlZoneName      = ' '  ! Name of the control zone
    INTEGER                      :: ZoneNum           = 0    ! zone number of control zone
    INTEGER                      :: ControlledZoneNum = 0    ! controlled zone number of control zone
    INTEGER                      :: AvailStatus       = 0    ! reports status of availability manager
  END TYPE DefineNightCycSysAvailManager

  TYPE DefineOptStartSysAvailManager               ! Derived type for Optimal Start Sys Avail Managers
    CHARACTER(len=MaxNameLength) :: Name              = ' '  ! Name of the manager object
    INTEGER                      :: MgrType           = 0    ! Integer equivalent of availability manager type
    INTEGER                      :: SchedPtr          = 0    ! Applicability schedule pointer
    CHARACTER(len=MaxNameLength) :: FanSched          = ' '  ! Fan schedule name
    INTEGER                      :: FanSchedPtr       = 0    ! Fan schedule pointer
    INTEGER                      :: CtrlType          = 0    ! Type of control: Stay Off, ControlZone, MaximumofZoneList
    CHARACTER(len=MaxNameLength) :: CtrlZoneName      = ' '  ! Name of the control zone
    INTEGER                      :: ZoneNum           = 0    ! zone number of control zone
    INTEGER                      :: ControlledZoneNum = 0    ! controlled zone number of control zone
    CHARACTER(len=MaxNameLength) :: ZoneListName      = ' '  ! Zone List name
    INTEGER                      :: NumOfZones    = 0        ! Number of zones in the list
    INTEGER, ALLOCATABLE, DIMENSION(:) :: ZonePtrs           ! Pointers to zones in the list
    REAL(r64)                    :: MaxOptStartTime   = 6.0d0  ! Maximum value of start time in hours
    INTEGER                      :: CtrlAlgType       = 0    ! Control algorithm: ConstantTemperatureGradient,
                                                             ! AdaptiveTemperatureGradient, AdaptiveASHRAE, ConstantStartTime
    REAL(r64)                    :: ConstTGradCool    = 1.0d0  ! Constant temperature gradient in cooling mode, unit: degC per hour
    REAL(r64)                    :: ConstTGradHeat    = 1.0d0  ! Constant temperature gradient in heating mode, unit: degC per hour
    REAL(r64)                    :: InitTGradCool     = 1.0d0  ! Initial value for temperature gradient in cooling mode, unit: degC per hour
    REAL(r64)                    :: InitTGradHeat     = 1.0d0  ! Initial value for temperature gradient in heating mode, unit: degC per hour
    REAL(r64)                    :: AdaptiveTGradCool = 1.0d0  ! Calculated adaptive temperature gradient in cooling mode, unit: degC per hour
    REAL(r64)                    :: AdaptiveTGradHeat = 1.0d0  ! Calculated adaptive temperature gradient in heating mode, unit: degC per hour
    REAL(r64)                    :: ConstStartTime    = 2.0d0  ! Constant start time in hours
    INTEGER                      :: NumPreDays        = 1    ! Number of previous days for adaptive control
    INTEGER                      :: AvailStatus       = 0    ! reports status of availability manager
    REAL(r64)                    :: NumHoursBeforeOccupancy = 0.0d0
  END TYPE DefineOptStartSysAvailManager

!Not used yet
  TYPE DefineASHRAEAdaptiveOptimumStartCoeffs               ! Derived type for Differential Thermostat Sys Avail Managers
    CHARACTER(len=MaxNameLength) :: Name              = ' ' ! Name of the object
    REAL(r64)                    :: Coeff1            = 0.0d0 ! 1st Coefficient of the equation
    REAL(r64)                    :: Coeff2            = 0.0d0 ! 2nd Coefficient of the equation
    REAL(r64)                    :: Coeff3            = 0.0d0 ! 3rd Coefficient of the equation
    REAL(r64)                    :: Coeff4            = 0.0d0 ! 4th Coefficient of the equation
  END TYPE DefineASHRAEAdaptiveOptimumStartCoeffs

  TYPE DefineDiffTSysAvailManager                  ! Derived type for Differential Thermostat Sys Avail Managers
    CHARACTER(len=MaxNameLength) :: Name              = ' ' ! Name of the manager object
    INTEGER                      :: MgrType           = 0   ! Integer equivalent of availability manager type
    INTEGER                      :: HotNode           = 0   ! "Hot" sensor node
    INTEGER                      :: ColdNode          = 0   ! "Cold" sensor node
    REAL(r64)                    :: TempDiffOn        = 0.0d0 ! Temperature difference for turn on (delta C)
    REAL(r64)                    :: TempDiffOff       = 0.0d0 ! Temperature difference for turn off (delta C)
    INTEGER                      :: AvailStatus       = 0   ! reports status of availability manager
  END TYPE DefineDiffTSysAvailManager

  TYPE DefineHiLoSysAvailManager                   ! Derived type for High/Low Temperature On/Off Sys Avail Managers
    CHARACTER(len=MaxNameLength) :: Name              = ' ' ! Name of the manager object
    INTEGER                      :: MgrType           = 0   ! Integer equivalent of availability manager type
    INTEGER                      :: Node              = 0   ! Sensor node
    REAL(r64)                    :: Temp              = 0.0d0 ! Temperature for on/off (C)
    INTEGER                      :: SchedPtr          = 0   ! Applicability schedule pointer
    INTEGER                      :: AvailStatus       = 0   ! reports status of availability manager
  END TYPE DefineHiLoSysAvailManager

  TYPE DefineNightVentSysAvailManager
    CHARACTER(len=MaxNameLength) :: Name              = ' ' ! Name of the manager object
    INTEGER                      :: MgrType           = 0   ! Integer equivalent of availability manager type
    INTEGER                      :: SchedPtr          = 0   ! Applicability schedule pointer
    CHARACTER(len=MaxNameLength) :: FanSched          = ' ' ! Fan schedule name
    INTEGER                      :: FanSchedPtr       = 0   ! Fan schedule pointer
    CHARACTER(len=MaxNameLength) :: VentTempSched     = ' ' ! Ventilation temperature schedule
    INTEGER                      :: VentTempSchedPtr  = 0   ! Ventilation temperature schedule pointer
    REAL(r64)                    :: VentDelT          = 0.0d0 ! Ventilation delta T [deltaC]
    REAL(r64)                    :: VentTempLowLim    = 0.0d0 ! ventilation temperature low limit
    CHARACTER(len=MaxNameLength) :: CtrlZoneName      = ' ' ! Name of the control zone
    INTEGER                      :: ZoneNum           = 0   ! zome number of control zone
    INTEGER                      :: ControlledZoneNum = 0   ! controlled zone number of control zone
    REAL(r64)                    :: VentFlowFrac      = 0.0d0 ! the night venting flow fraction
    INTEGER                      :: AvailStatus       = 0   ! reports status of availability manager
  END TYPE DefineNightVentSysAvailManager

  TYPE DefineHybridVentSysAvailManager
    CHARACTER(len=MaxNameLength) :: Name                   = ' ' ! Name of the object
    INTEGER                      :: MgrType                = 0   ! Integer equivalent of availability manager type
    CHARACTER(len=MaxNameLength) :: AirLoopName            = ' ' ! Name of HVAC Air Loop
    INTEGER                      :: AirLoopNum             = 0   ! HVAC Air Loop number
    CHARACTER(len=MaxNameLength) :: ControlZoneName        = ' ' ! Controlled zone name
    INTEGER                      :: NodeNumofControlledZone =0   ! Controlled zone node number
    INTEGER                      :: ActualZoneNum          = 0   ! Actual zone number
    INTEGER                      :: ControlledZoneNum      = 0   ! Controlled zone number
    INTEGER                      :: ControlModeSchedPtr    = 0   ! Ventilation control mode schedule pointer
    INTEGER                      :: ControlMode            = 0   ! hybrid ventilation control mode
    INTEGER                      :: VentilationCtrl        = 0   ! Ventilation control type: Noaction, Close, Open
    REAL(r64)                    :: MinOutdoorTemp    = -100.0d0   ! Minimum Outdoor Temperature [C]
    REAL(r64)                    :: MaxOutdoorTemp     = 100.0d0   ! Maximum Outdoor Temperature [C]
    REAL(r64)                    :: MinOutdoorEnth       = 0.1d0   ! Minimum Outdoor Enthalpy [J/kg]
    REAL(r64)                    :: MaxOutdoorEnth  = 300000.0d0   ! Maximum Outdoor Enthalpy [J/kg]
    REAL(r64)                    :: MinOutdoorDewPoint =-100.0d0   ! Minimum Outdoor Dew point temperature [C]
    REAL(r64)                    :: MaxOutdoorDewPoint = 100.0d0   ! Maximum Outdoor Dew Point Temperature [C]
    REAL(r64)                    :: MaxWindSpeed         = 0.0d0   ! Maximum Wind speed [m/s]
    LOGICAL                      :: UseRainIndicator  = .TRUE.   ! Use WeatherFile Rain Indicators
    CHARACTER(len=MaxNameLength) :: MinOASched             = ' ' ! Minimum Outdoor Ventilation Air Schedule Name
    INTEGER                      :: MinOASchedPtr          = 0   ! Minimum Outdoor Ventilation Air Schedule pointer
    INTEGER                      :: DewPointNoRHErrCount   = 0   ! Dewpoint control mode error count without a humidistat
    INTEGER                      :: DewPointNoRHErrIndex   = 0   ! Dewpoint control mode error index without a humidistat
    INTEGER                      :: DewPointErrCount       = 0   ! Dewpoint control mode error count without a valid humidistat
    INTEGER                      :: DewPointErrIndex       = 0   ! Dewpoint control mode error index without a valid humidistat
    INTEGER                      :: SingleHCErrCount       = 0   ! Temperature and enthalpy control mode error count
                                                                 ! with a singleHeatingCooling setpoint
    INTEGER                      :: SingleHCErrIndex       = 0   ! Temperature and enthalpy control mode error index
                                                                 ! with a singleHeatingCooling setpoint
    INTEGER                      :: OpeningFactorFWS       = 0   ! Opening factor modifier as a function of wind speed
    INTEGER                      :: ANControlTypeSchedPtr  = 0   ! AirflowNetwork control type schedule pointer
    INTEGER                      :: SimpleControlTypeSchedPtr = 0 ! Simple airflow object control type schedule pointer
    INTEGER                      :: VentilationPtr         = 0   ! Ventilation object name pointer
    INTEGER                      :: AvailStatus            = 0   ! reports status of availability manager
    CHARACTER(len=MaxNameLength) :: VentilationName        = ' ' ! Ventilation object name
    LOGICAL                      :: HybridVentMgrConnectedToAirLoop = .TRUE. ! Flag to check whether hybrid ventilation
                                                                 ! manager is connected to air loop
    LOGICAL                      :: SimHybridVentSysAvailMgr  = .FALSE. ! Set to false when a zone has two hybrid ventilation
                                                                 ! managers, one with air loop and one without
  END TYPE DefineHybridVentSysAvailManager

  TYPE SysAvailManagerList
    CHARACTER(len=MaxNameLength) :: Name =' '            ! Availability Manager List Name
    INTEGER                      :: NumItems=0
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: AvailManagerName
    CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: cAvailManagerType
    INTEGER,                      ALLOCATABLE, DIMENSION(:) :: AvailManagerType
  END TYPE

  ! MODULE VARIABLE DECLARATIONS
  TYPE (DefineSchedSysAvailManager),      ALLOCATABLE, DIMENSION(:) :: SchedSysAvailMgrData
  TYPE (DefineSchedOnSysAvailManager),    ALLOCATABLE, DIMENSION(:) :: SchedOnSysAvailMgrData
  TYPE (DefineSchedOffSysAvailManager),   ALLOCATABLE, DIMENSION(:) :: SchedOffSysAvailMgrData
  TYPE (DefineNightCycSysAvailManager),   ALLOCATABLE, DIMENSION(:) :: NCycSysAvailMgrData
  TYPE (DefineDiffTSysAvailManager),      ALLOCATABLE, DIMENSION(:) :: DiffTSysAvailMgrData
  TYPE (DefineHiLoSysAvailManager),       ALLOCATABLE, DIMENSION(:) :: HiTurnOffSysAvailMgrData
  TYPE (DefineHiLoSysAvailManager),       ALLOCATABLE, DIMENSION(:) :: HiTurnOnSysAvailMgrData
  TYPE (DefineHiLoSysAvailManager),       ALLOCATABLE, DIMENSION(:) :: LoTurnOffSysAvailMgrData
  TYPE (DefineHiLoSysAvailManager),       ALLOCATABLE, DIMENSION(:) :: LoTurnOnSysAvailMgrData
  TYPE (DefineNightVentSysAvailManager),  ALLOCATABLE, DIMENSION(:) :: NVentSysAvailMgrData
  TYPE (DefineHybridVentSysAvailManager), ALLOCATABLE, DIMENSION(:) :: HybridVentSysAvailMgrData
  TYPE (SysAvailManagerList),             ALLOCATABLE, DIMENSION(:) :: SysAvailMgrListData
  TYPE (DefineOptStartSysAvailManager),   ALLOCATABLE, DIMENSION(:) :: OptStartSysAvailMgrData
  TYPE (DefineASHRAEAdaptiveOptimumStartCoeffs),      ALLOCATABLE, DIMENSION(:) :: ASHRAEOptSCoeffCooling
  TYPE (DefineASHRAEAdaptiveOptimumStartCoeffs),      ALLOCATABLE, DIMENSION(:) :: ASHRAEOptSCoeffHeating

  INTEGER :: NumSchedSysAvailMgrs     = 0
  INTEGER :: NumSchedOnSysAvailMgrs   = 0
  INTEGER :: NumSchedOffSysAvailMgrs  = 0
  INTEGER :: NumNCycSysAvailMgrs      = 0
  INTEGER :: NumDiffTSysAvailMgrs     = 0
  INTEGER :: NumHiTurnOffSysAvailMgrs = 0
  INTEGER :: NumHiTurnOnSysAvailMgrs  = 0
  INTEGER :: NumLoTurnOffSysAvailMgrs = 0
  INTEGER :: NumLoTurnOnSysAvailMgrs  = 0
  INTEGER :: NumNVentSysAvailMgrs     = 0
  INTEGER :: NumAvailManagerLists     = 0
  LOGICAL :: GetAvailListsInput       = .TRUE.
  LOGICAL :: GetAvailMgrInputFlag     = .TRUE.     ! First time, input is "gotten"
  LOGICAL :: GetHybridInputFlag = .True.  ! Flag set to make sure you get input once
  INTEGER :: NumOptStartSysAvailMgrs  = 0
  ! SUBROUTINE SPECIFICATIONS FOR MODULE

  PUBLIC  ManageSystemAvailability
  PRIVATE GetSysAvailManagerInputs
  PRIVATE GetSysAvailManagerListInputs
  PUBLIC  GetPlantAvailabilityManager
  PUBLIC  GetAirLoopAvailabilityManager
  PUBLIC  GetZoneEqAvailabilityManager
  PRIVATE InitSysAvailManagers
  PRIVATE SimSysAvailManager
  PRIVATE CalcSchedSysAvailMgr
  PRIVATE CalcSchedOnSysAvailMgr
  PRIVATE CalcSchedOffSysAvailMgr
  PRIVATE CalcNCycSysAvailMgr
  PRIVATE CalcDiffTSysAvailMgr
  PRIVATE CalcHiTurnOffSysAvailMgr
  PRIVATE CalcHiTurnOnSysAvailMgr
  PRIVATE CalcLoTurnOffSysAvailMgr
  PRIVATE CalcLoTurnOnSysAvailMgr
  PRIVATE CalcNVentSysAvailMgr
  PRIVATE CalcOptStartSysAvailMgr
  PUBLIC  ValidateAndSetSysAvailabilityManagerType
  PUBLIC  ManageHybridVentilation
  PRIVATE GetHybridVentilationInputs
  PRIVATE CalcHybridVentSysAvailMgr
  PRIVATE InitHybridVentSysAvailMgr
  PUBLIC  GetHybridVentilationControlStatus

  CONTAINS

SUBROUTINE ManageSystemAvailability

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   August 2001
          !       MODIFIED       L. Gu, April, 2007. Added hybrid ventilation control
          !                      Chandan Sharma, March 2011/July 2012 - FSEC: Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Manage the simulation of the System Availability Managers

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! NA

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: ZoneEquipAvail, NumValidSysAvailZoneComponents
  USE DataLoopNode
  USE DataAirLoop
  USE DataPlant

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! None

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: PriAirSysNum              ! Primary Air System index
  INTEGER      :: PriAirSysAvailMgrNum      ! Index of Sys Avail Manager in a Primary Air System
  INTEGER      :: PlantNum                  ! Plant Loop index
  INTEGER      :: PlantAvailMgrNum          ! Index of Plant Avail Manager in a Plant Loop
  INTEGER      :: AvailStatus
  INTEGER      :: PreviousStatus
  INTEGER      :: ZoneInSysNum
  INTEGER      :: CtrldZoneNum
  INTEGER      :: HybridVentNum             ! Hybrid ventilation control number
  INTEGER      :: ZoneEquipType             ! Type of ZoneHVAC:* component
  INTEGER      :: CompNum                   ! Index of ZoneHVAC:* component
  INTEGER      :: ZoneCompAvailMgrNum       ! Index of availability manager associated with the ZoneHVAC:* component
  INTEGER      :: DummyArgument=1           ! This variable is used when SimSysAvailManager is called for a ZoneHVAC:* component

  IF (GetAvailMgrInputFlag) THEN
    CALL GetSysAvailManagerInputs
    GetAvailMgrInputFlag=.FALSE.
    RETURN
  ENDIF

  CALL InitSysAvailManagers

  DO PriAirSysNum=1,NumPrimaryAirSys  ! loop over the primary air systems

    PreviousStatus = PriAirSysAvailMgr(PriAirSysNum)%AvailStatus ! Save the previous status for differential thermostat
    PriAirSysAvailMgr(PriAirSysNum)%AvailStatus = NoAction ! initialize the availability to "take no action"

    DO PriAirSysAvailMgrNum=1,PriAirSysAvailMgr(PriAirSysNum)%NumAvailManagers ! loop over the avail managers in system

      CALL SimSysAvailManager(PriAirSysAvailMgr(PriAirSysNum)%AvailManagerType(PriAirSysAvailMgrNum), &
                              PriAirSysAvailMgr(PriAirSysNum)%AvailManagerName(PriAirSysAvailMgrNum), &
                              PriAirSysAvailMgr(PriAirSysNum)%AvailManagerNum(PriAirSysAvailMgrNum),  &
                              PriAirSysNum, PreviousStatus, AvailStatus)

      IF (AvailStatus .EQ. Forceoff) THEN
        PriAirSysAvailMgr(PriAirSysNum)%AvailStatus = ForceOff
        EXIT  ! Fans forced off takes precedence
      ELSE IF (AvailStatus .EQ. CycleOnZoneFansOnly) THEN
        PriAirSysAvailMgr(PriAirSysNum)%AvailStatus = CycleOnZoneFansOnly ! zone fans only takes next precedence
      ELSE IF ( (AvailStatus .EQ. CycleOn) .AND. &
                (PriAirSysAvailMgr(PriAirSysNum)%AvailStatus .EQ. NoAction) ) THEN
        PriAirSysAvailMgr(PriAirSysNum)%AvailStatus = CycleOn ! cycle on is lowest precedence
      END IF

    END DO ! end of availability manager loop

    ! Add hybrid ventilation control
    IF (NumHybridVentSysAvailMgrs > 0) THEN
      DO HybridVentNum = 1, NumHybridVentSysAvailMgrs
        IF (HybridVentSysAvailMgrData(HybridVentNum)%AirLoopNum == PriAirSysNum .AND. &
            HybridVentSysAvailMgrData(HybridVentNum)%VentilationCtrl == HybridVentCtrl_Open) THEN
          PriAirSysAvailMgr(PriAirSysNum)%AvailStatus = ForceOff ! Force the system off
        END IF
      END DO
    END IF

    ! loop over the zones served by the system and set the zone equipment availability
    DO ZoneInSysNum=1,AirToZoneNodeInfo(PriAirSysNum)%NumZonesCooled

      CtrldZoneNum = AirToZoneNodeInfo(PriAirSysNum)%CoolCtrlZoneNums(ZoneInSysNum)
      ZoneEquipAvail(CtrldZoneNum) = PriAirSysAvailMgr(PriAirSysNum)%AvailStatus

    END DO

  END DO ! end of primary air system loop


  DO PlantNum=1,NumPlantLoops

    PreviousStatus = PlantAvailMgr(PlantNum)%AvailStatus ! Save the previous status for differential thermostat
    PlantAvailMgr(PlantNum)%AvailStatus = NoAction ! Initialize the availability to "take no action"

    DO PlantAvailMgrNum=1,PlantAvailMgr(PlantNum)%NumAvailManagers ! loop over the avail managers in plant

      CALL SimSysAvailManager(PlantAvailMgr(PlantNum)%AvailManagerType(PlantAvailMgrNum), &
                              PlantAvailMgr(PlantNum)%AvailManagerName(PlantAvailMgrNum), &
                              PlantAvailMgr(PlantNum)%AvailManagerNum(PlantAvailMgrNum),  &
                              PlantNum, PreviousStatus, AvailStatus)

      IF (AvailStatus /= NoAction) THEN
        PlantAvailMgr(PlantNum)%AvailStatus = AvailStatus
        EXIT ! First manager to do anything other than "NoAction" gets to set the availability
      END IF

    END DO ! end of availability manager loop

  END DO ! end of plant loop

  DO ZoneEquipType = 1,NumValidSysAvailZoneComponents  ! loop over the zone equipment types which allow system avail managers
    IF(ALLOCATED(ZoneComp))THEN
     IF(ZoneComp(ZoneEquipType)%TotalNumComp .GT. 0)THEN
      DO CompNum = 1, ZoneComp(ZoneEquipType)%TotalNumComp
       IF(ALLOCATED(ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs)) THEN
        IF(ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%NumAvailManagers .GT. 0)THEN
           ! Save the previous status for differential thermostat
          PreviousStatus = ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%AvailStatus
           ! initialize the availability to "take no action"
          ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%AvailStatus = NoAction
          DO ZoneCompAvailMgrNum=1,ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%NumAvailManagers
           ! loop over the avail managers in ZoneHVAC:* components
            CALL SimSysAvailManager(ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%AvailManagerType(ZoneCompAvailMgrNum), &
                                    ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%AvailManagerName(ZoneCompAvailMgrNum), &
                                    ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%AvailManagerNum(ZoneCompAvailMgrNum),  &
                                    DummyArgument, PreviousStatus, AvailStatus, ZoneEquipType, CompNum)
            IF (AvailStatus .EQ. Forceoff) THEN
              ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%AvailStatus = ForceOff
              EXIT  ! Fans forced off takes precedence
            ELSE IF ( (AvailStatus .EQ. CycleOn) .AND. &
                      (ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%AvailStatus .EQ. NoAction) ) THEN
              ! cycle on is next precedence
              ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%AvailStatus = CycleOn
            END IF
          END DO ! end of availability manager loop
         ENDIF
        ELSE
          ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%AvailStatus = NoAction
        ENDIF
        IF (ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%ZoneNum .GT. 0) THEN
          IF (NumHybridVentSysAvailMgrs > 0) THEN
            DO HybridVentNum = 1, NumHybridVentSysAvailMgrs
              IF (.NOT. HybridVentSysAvailMgrData(HybridVentNum)%HybridVentMgrConnectedToAirLoop) THEN
                IF (HybridVentSysAvailMgrData(HybridVentNum)%ActualZoneNum == &
                    ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%ZoneNum) THEN
                  IF (HybridVentSysAvailMgrData(HybridVentNum)%VentilationCtrl == HybridVentCtrl_Open) THEN
                    ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%AvailStatus = ForceOff
                  END IF
                END IF
              ENDIF
            END DO
          END IF
        ENDIF
      END DO
     ENDIF
    ENDIF
  END DO ! end of zone equip types

  RETURN

END SUBROUTINE ManageSystemAvailability

SUBROUTINE GetSysAvailManagerInputs

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   August 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for System Availability Managers and stores it in
          ! appropriate data structures.

          ! METHODOLOGY EMPLOYED:
          ! Uses InputProcessor "Get" routines to obtain data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,   ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, FindIteminList, SameString,   &
                              MakeUPPERCase, GetObjectDefMaxArgs
  USE NodeInputManager, ONLY: GetOnlySingleNode, MarkNode
  USE DataHeatBalance,  ONLY: Zone, ZoneList, NumOfZoneLists
  USE DataLoopNode
  USE DataZoneEquipment, ONLY: NumValidSysAvailZoneComponents, cValidSysAvailManagerCompTypes

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetSysAvailManagerInputs: ' ! include trailing blank

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cAlphaFieldNames
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cNumericFieldNames
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericFieldBlanks
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaFieldBlanks
  CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: cAlphaArgs
  REAL(r64),ALLOCATABLE, DIMENSION(:) :: rNumericArgs
  CHARACTER(len=MaxNameLength) :: cCurrentModuleObject
  INTEGER       :: NumAlphas            ! Number of Alphas for each GetObjectItem call
  INTEGER       :: NumNumbers           ! Number of Numbers for each GetObjectItem call
  INTEGER       :: maxAlphas            ! maximum number of alphas for this set of objects
  INTEGER       :: maxNumbers           ! maximum number of numbers for this set of objects
  INTEGER       :: numArgs              ! maximum number of arguments for this set of objects
  INTEGER       :: IOStatus             ! Used in GetObjectItem
  LOGICAL       :: ErrorsFound=.FALSE.  ! Set to true if errors in input, fatal at end of routine
  LOGICAL       :: IsNotOK              ! Flag to verify name
  LOGICAL       :: IsBlank              ! Flag for blank name
  INTEGER       :: SysAvailNum          ! DO loop index for all System Availability Managers
  INTEGER       :: CyclingTimeSteps
  INTEGER       :: ZoneEquipType
  INTEGER       :: TotalNumComp
  INTEGER       :: ZoneListNum
  INTEGER       :: ZoneNumInList

  ! Get the number of occurences of each type of manager and read in data
  cCurrentModuleObject = 'AvailabilityManager:Scheduled'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,numArgs,NumAlphas,NumNumbers)
  maxNumbers=NumNumbers
  maxAlphas=NumAlphas
  cCurrentModuleObject = 'AvailabilityManager:ScheduledOn'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,numArgs,NumAlphas,NumNumbers)
  maxNumbers=MAX(maxNumbers,NumNumbers)
  maxAlphas=MAX(maxAlphas,NumAlphas)
  cCurrentModuleObject = 'AvailabilityManager:ScheduledOff'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,numArgs,NumAlphas,NumNumbers)
  maxNumbers=MAX(maxNumbers,NumNumbers)
  maxAlphas=MAX(maxAlphas,NumAlphas)
  cCurrentModuleObject = 'AvailabilityManager:NightCycle'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,numArgs,NumAlphas,NumNumbers)
  maxNumbers=MAX(maxNumbers,NumNumbers)
  maxAlphas=MAX(maxAlphas,NumAlphas)
  cCurrentModuleObject = 'AvailabilityManager:DifferentialThermostat'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,numArgs,NumAlphas,NumNumbers)
  maxNumbers=MAX(maxNumbers,NumNumbers)
  maxAlphas=MAX(maxAlphas,NumAlphas)
  cCurrentModuleObject = 'AvailabilityManager:HighTemperatureTurnOff'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,numArgs,NumAlphas,NumNumbers)
  maxNumbers=MAX(maxNumbers,NumNumbers)
  maxAlphas=MAX(maxAlphas,NumAlphas)
  cCurrentModuleObject = 'AvailabilityManager:HighTemperatureTurnOn'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,numArgs,NumAlphas,NumNumbers)
  maxNumbers=MAX(maxNumbers,NumNumbers)
  maxAlphas=MAX(maxAlphas,NumAlphas)
  cCurrentModuleObject = 'AvailabilityManager:LowTemperatureTurnOff'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,numArgs,NumAlphas,NumNumbers)
  maxNumbers=MAX(maxNumbers,NumNumbers)
  maxAlphas=MAX(maxAlphas,NumAlphas)
  cCurrentModuleObject = 'AvailabilityManager:LowTemperatureTurnOn'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,numArgs,NumAlphas,NumNumbers)
  maxNumbers=MAX(maxNumbers,NumNumbers)
  maxAlphas=MAX(maxAlphas,NumAlphas)
  cCurrentModuleObject = 'AvailabilityManager:NightVentilation'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,numArgs,NumAlphas,NumNumbers)
  maxNumbers=MAX(maxNumbers,NumNumbers)
  maxAlphas=MAX(maxAlphas,NumAlphas)
  cCurrentModuleObject = 'AvailabilityManager:OptimumStart'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,numArgs,NumAlphas,NumNumbers)
  maxNumbers=MAX(maxNumbers,NumNumbers)
  maxAlphas=MAX(maxAlphas,NumAlphas)

  ALLOCATE(cAlphaFieldNames(maxAlphas))
  cAlphaFieldNames=' '
  ALLOCATE(cAlphaArgs(maxAlphas))
  cAlphaArgs=' '
  ALLOCATE(lAlphaFieldBlanks(maxAlphas))
  lAlphaFieldBlanks=.false.
  ALLOCATE(cNumericFieldNames(maxNumbers))
  cNumericFieldNames=' '
  ALLOCATE(rNumericArgs(maxNumbers))
  rNumericArgs=0.0d0
  ALLOCATE(lNumericFieldBlanks(maxNumbers))
  lNumericFieldBlanks=.false.

  IF (.not. ALLOCATED(ZoneComp)) THEN
    ALLOCATE(ZoneComp(NumValidSysAvailZoneComponents))
  ENDIF

  DO ZoneEquipType = 1, NumValidSysAvailZoneComponents
    IF (.not. ALLOCATED(ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs)) THEN
      TotalNumComp = GetNumObjectsFound(cValidSysAvailManagerCompTypes(ZoneEquipType))
      ZoneComp(ZoneEquipType)%TotalNumComp = TotalNumComp
      IF (TotalNumComp .GT. 0) THEN
        ALLOCATE(ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(TotalNumComp))
      ENDIF
    ENDIF
  ENDDO

  cCurrentModuleObject = 'AvailabilityManager:Scheduled'
  NumSchedSysAvailMgrs = GetNumObjectsFound(cCurrentModuleObject)

  IF(NumSchedSysAvailMgrs > 0)THEN

    ALLOCATE(SchedSysAvailMgrData(NumSchedSysAvailMgrs))

    DO SysAvailNum = 1,NumSchedSysAvailMgrs

      CALL GetObjectItem(cCurrentModuleObject,SysAvailNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus, &
                         AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK=.FALSE.
      IsBlank=.FALSE.
      CALL VerifyName(cAlphaArgs(1),SchedSysAvailMgrData%Name,SysAvailNum-1,IsNotOK,IsBlank,&
                      TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.TRUE.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      SchedSysAvailMgrData(SysAvailNum)%Name = cAlphaArgs(1)
      SchedSysAvailMgrData(SysAvailNum)%MgrType = SysAvailMgr_Scheduled

      SchedSysAvailMgrData(SysAvailNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(2))
      IF (SchedSysAvailMgrData(SysAvailNum)%SchedPtr .EQ. 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid')
        CALL ShowContinueError('not found: '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
        ErrorsFound = .TRUE.
      END IF

      CALL SetupOutputVariable('Availability Manager Scheduled Control Status []', &
                                SchedSysAvailMgrData(SysAvailNum)%AvailStatus, &
                               'System','Average',SchedSysAvailMgrData(SysAvailNum)%Name)

    END DO ! SysAvailNum

  END IF

  cCurrentModuleObject = 'AvailabilityManager:ScheduledOn'
  NumSchedOnSysAvailMgrs   = GetNumObjectsFound(cCurrentModuleObject)

  IF(NumSchedOnSysAvailMgrs > 0)THEN

    ALLOCATE(SchedOnSysAvailMgrData(NumSchedOnSysAvailMgrs))

    DO SysAvailNum = 1,NumSchedOnSysAvailMgrs

      CALL GetObjectItem(cCurrentModuleObject,SysAvailNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus, &
                         AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK=.FALSE.
      IsBlank=.FALSE.
      CALL VerifyName(cAlphaArgs(1),SchedOnSysAvailMgrData%Name,SysAvailNum-1,IsNotOK,IsBlank,&
                      TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.TRUE.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      SchedOnSysAvailMgrData(SysAvailNum)%Name = cAlphaArgs(1)
      SchedOnSysAvailMgrData(SysAvailNum)%MgrType = SysAvailMgr_ScheduledOn

      SchedOnSysAvailMgrData(SysAvailNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(2))
      IF (SchedOnSysAvailMgrData(SysAvailNum)%SchedPtr .EQ. 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' = "'//TRIM(cAlphaArgs(1))//'", invalid')
        CALL ShowContinueError('not found: '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
        ErrorsFound = .TRUE.
      END IF

      CALL SetupOutputVariable('Availability Manager Scheduled On Control Status []', &
                                SchedOnSysAvailMgrData(SysAvailNum)%AvailStatus, &
                               'System','Average',SchedOnSysAvailMgrData(SysAvailNum)%Name)

    END DO ! SysAvailNum

  END IF

  cCurrentModuleObject = 'AvailabilityManager:ScheduledOff'
  NumSchedOffSysAvailMgrs  = GetNumObjectsFound(cCurrentModuleObject)

  IF(NumSchedOffSysAvailMgrs > 0)THEN

    ALLOCATE(SchedOffSysAvailMgrData(NumSchedOffSysAvailMgrs))

    DO SysAvailNum = 1,NumSchedOffSysAvailMgrs

      CALL GetObjectItem(cCurrentModuleObject,SysAvailNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus, &
                         AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK=.FALSE.
      IsBlank=.FALSE.
      CALL VerifyName(cAlphaArgs(1),SchedOffSysAvailMgrData%Name,SysAvailNum-1,IsNotOK,IsBlank,&
                      TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.TRUE.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      SchedOffSysAvailMgrData(SysAvailNum)%Name = cAlphaArgs(1)
      SchedOffSysAvailMgrData(SysAvailNum)%MgrType = SysAvailMgr_ScheduledOff

      SchedOffSysAvailMgrData(SysAvailNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(2))
      IF (SchedOffSysAvailMgrData(SysAvailNum)%SchedPtr .EQ. 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' = "'//TRIM(cAlphaArgs(1))//'", invalid')
        CALL ShowContinueError('not found: '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
        ErrorsFound = .TRUE.
      END IF

      CALL SetupOutputVariable('Availability Manager Scheduled Off Control Status []', &
                                SchedOffSysAvailMgrData(SysAvailNum)%AvailStatus, &
                               'System','Average',SchedOffSysAvailMgrData(SysAvailNum)%Name)

    END DO ! SysAvailNum

  END IF

  cCurrentModuleObject = 'AvailabilityManager:NightCycle'
  NumNCycSysAvailMgrs  = GetNumObjectsFound(cCurrentModuleObject)
  CyclingTimeSteps     = 0

  IF(NumNCycSysAvailMgrs > 0)THEN

    ALLOCATE(NCycSysAvailMgrData(NumNCycSysAvailMgrs))

    DO SysAvailNum = 1,NumNCycSysAvailMgrs

      CALL GetObjectItem(cCurrentModuleObject,SysAvailNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus, &
                         AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK=.FALSE.
      IsBlank=.FALSE.
      CALL VerifyName(cAlphaArgs(1),NCycSysAvailMgrData%Name,SysAvailNum-1,IsNotOK,IsBlank,&
                      TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.TRUE.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      NCycSysAvailMgrData(SysAvailNum)%Name = cAlphaArgs(1)
      NCycSysAvailMgrData(SysAvailNum)%MgrType = SysAvailMgr_NightCycle

      NCycSysAvailMgrData(SysAvailNum)%TempTolRange = rNumericArgs(1)
      CyclingTimeSteps = NINT( (rNumericArgs(2)/SecInHour)*REAL(NumOfTimeStepInHour,r64) )
      CyclingTimeSteps = MAX(1, CyclingTimeSteps)
      NCycSysAvailMgrData(SysAvailNum)%CyclingTimeSteps = CyclingTimeSteps
      NCycSysAvailMgrData(SysAvailNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(2))
      IF (NCycSysAvailMgrData(SysAvailNum)%SchedPtr .EQ. 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' = "'//TRIM(cAlphaArgs(1))//'", invalid')
        CALL ShowContinueError('not found: '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
        ErrorsFound = .TRUE.
      END IF
      NCycSysAvailMgrData(SysAvailNum)%FanSched = cAlphaArgs(3)
      NCycSysAvailMgrData(SysAvailNum)%FanSchedPtr = GetScheduleIndex(cAlphaArgs(3))
      IF (NCycSysAvailMgrData(SysAvailNum)%FanSchedPtr .EQ. 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' = "'//TRIM(cAlphaArgs(1))//'", invalid')
        CALL ShowContinueError('not found: '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
        ErrorsFound = .TRUE.
      END IF

      SELECT CASE(MakeUPPERCase(cAlphaArgs(4)))
        CASE('STAYOFF')
          NCycSysAvailMgrData(SysAvailNum)%CtrlType = StayOff
        CASE('CYCLEONANY')
          NCycSysAvailMgrData(SysAvailNum)%CtrlType = CycleOnAny
        CASE('CYCLEONCONTROLZONE')
          NCycSysAvailMgrData(SysAvailNum)%CtrlType = CycleOnControlZone
        CASE('CYCLEONANYZONEFANSONLY')
          NCycSysAvailMgrData(SysAvailNum)%CtrlType = ZoneFansOnly
        CASE DEFAULT
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid')
          CALL ShowSevereError(RoutineName//'incorrect value: '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
          ErrorsFound=.TRUE.
      END SELECT
      IF (NCycSysAvailMgrData(SysAvailNum)%CtrlType .EQ. CycleOnControlZone) THEN
        NCycSysAvailMgrData(SysAvailNum)%CtrlZoneName = cAlphaArgs(5)
        NCycSysAvailMgrData(SysAvailNum)%ZoneNum = FindItemInList(cAlphaArgs(5),Zone%Name,NumOfZones)
        IF (NCycSysAvailMgrData(SysAvailNum)%ZoneNum .EQ. 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid')
          CALL ShowSevereError('not found: '//TRIM(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
          ErrorsFound = .TRUE.
        END IF
      END IF

      CALL SetupOutputVariable('Availability Manager Night Cycle Control Status []', &
                                NCycSysAvailMgrData(SysAvailNum)%AvailStatus, &
                               'System','Average',NCycSysAvailMgrData(SysAvailNum)%Name)

    END DO ! SysAvailNum
 END IF

  cCurrentModuleObject = 'AvailabilityManager:OptimumStart'
  NumOptStartSysAvailMgrs  = GetNumObjectsFound(cCurrentModuleObject)
  CyclingTimeSteps     = 0

  IF(NumOptStartSysAvailMgrs > 0)THEN
    ! Array size of variable type OptStartSysAvailMgrData is updated
    ALLOCATE(OptStartSysAvailMgrData(NumOptStartSysAvailMgrs))

    DO SysAvailNum = 1,NumOptStartSysAvailMgrs

      CALL GetObjectItem(cCurrentModuleObject,SysAvailNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus, &
                         AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK=.FALSE.
      IsBlank=.FALSE.
      CALL VerifyName(cAlphaArgs(1),OptStartSysAvailMgrData%Name,SysAvailNum-1,IsNotOK,IsBlank,&
                      TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.TRUE.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      OptStartSysAvailMgrData(SysAvailNum)%Name = cAlphaArgs(1)
      OptStartSysAvailMgrData(SysAvailNum)%MgrType = SysAvailMgr_OptimumStart
      OptStartSysAvailMgrData(SysAvailNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(2))
      IF (OptStartSysAvailMgrData(SysAvailNum)%SchedPtr .EQ. 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' = "'//TRIM(cAlphaArgs(1))//'", invalid')
        CALL ShowContinueError('not found: '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
        ErrorsFound = .TRUE.
      END IF
      OptStartSysAvailMgrData(SysAvailNum)%FanSched = cAlphaArgs(3)
      OptStartSysAvailMgrData(SysAvailNum)%FanSchedPtr = GetScheduleIndex(cAlphaArgs(3))
      IF (OptStartSysAvailMgrData(SysAvailNum)%FanSchedPtr .EQ. 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' = "'//TRIM(cAlphaArgs(1))//'", invalid')
        CALL ShowContinueError('not found: '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
        ErrorsFound = .TRUE.
      END IF

      OptStartSysAvailMgrData(SysAvailNum)%MaxOptStartTime = rNumericArgs(1)

      SELECT CASE(MakeUPPERCase(TRIM(cAlphaArgs(4))))
        CASE('STAYOFF')
          OptStartSysAvailMgrData(SysAvailNum)%CtrlType = StayOff
        CASE('CONTROLZONE')
          OptStartSysAvailMgrData(SysAvailNum)%CtrlType = ControlZone
        CASE('MAXIMUMOFZONELIST')
          OptStartSysAvailMgrData(SysAvailNum)%CtrlType = MaximumOfZoneList
        CASE DEFAULT
          OptStartSysAvailMgrData(SysAvailNum)%CtrlType = ControlZone
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid')
          CALL ShowSevereError(RoutineName//'incorrect value: '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
          ErrorsFound=.TRUE.
      END SELECT

      IF (OptStartSysAvailMgrData(SysAvailNum)%CtrlType .EQ. ControlZone) THEN
        OptStartSysAvailMgrData(SysAvailNum)%CtrlZoneName = cAlphaArgs(5)
        OptStartSysAvailMgrData(SysAvailNum)%ZoneNum = FindItemInList(cAlphaArgs(5),Zone%Name,NumOfZones)
        IF (OptStartSysAvailMgrData(SysAvailNum)%ZoneNum .EQ. 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid')
          CALL ShowSevereError('not found: '//TRIM(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
          ErrorsFound = .TRUE.
        END IF
      END IF

      IF (OptStartSysAvailMgrData(SysAvailNum)%CtrlType .EQ. MaximumOfZoneList) THEN
        OptStartSysAvailMgrData(SysAvailNum)%ZoneListName = cAlphaArgs(6)
        Do ZoneListNum=1, NumOfZoneLists
           IF (ZoneList(ZoneListNum)%Name == cAlphaArgs(6)) THEN
             OptStartSysAvailMgrData(SysAvailNum)%NumOfZones=ZoneList(ZoneListNum)%NumOfZones
             ALLOCATE(OptStartSysAvailMgrData(SysAvailNum)%ZonePtrs(ZoneList(ZoneListNum)%NumOfZones))
             DO ZoneNumInList=1, ZoneList(ZoneListNum)%NumOfZones
                OptStartSysAvailMgrData(SysAvailNum)%ZonePtrs(ZoneNumInList)= &
                ZoneList(ZoneListNum)%Zone(ZoneNumInList)
             END DO
           END IF
        END DO
        OptStartSysAvailMgrData(SysAvailNum)%NumOfZones = FindItemInList(cAlphaArgs(6),ZoneList%Name,NumOfZoneLists)
        IF (OptStartSysAvailMgrData(SysAvailNum)%NumOfZones .EQ. 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid')
          CALL ShowSevereError('not found: '//TRIM(cAlphaFieldNames(6))//'="'//TRIM(cAlphaArgs(6))//'".')
          ErrorsFound = .TRUE.
        END IF
      END IF

      SELECT CASE(MakeUPPERCase(TRIM(cAlphaArgs(7))))
        CASE('CONSTANTTEMPERATUREGRADIENT')
          OptStartSysAvailMgrData(SysAvailNum)%CtrlAlgType = ConstantTemperatureGradient
        CASE('ADAPTIVETEMPERATUREGRADIENT')
          OptStartSysAvailMgrData(SysAvailNum)%CtrlAlgType = AdaptiveTemperatureGradient
        CASE('ADAPTIVEASHRAE')
          OptStartSysAvailMgrData(SysAvailNum)%CtrlAlgType = AdaptiveASHRAE
        CASE('CONSTANTSTARTTIME')
          OptStartSysAvailMgrData(SysAvailNum)%CtrlAlgType = ConstantStartTime
        CASE DEFAULT
          OptStartSysAvailMgrData(SysAvailNum)%CtrlAlgType = AdaptiveASHRAE
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid')
          CALL ShowSevereError(RoutineName//'incorrect value: '//TRIM(cAlphaFieldNames(7))//'="'//TRIM(cAlphaArgs(7))//'".')
          ErrorsFound=.TRUE.
      END SELECT

      IF (OptStartSysAvailMgrData(SysAvailNum)%CtrlAlgType .EQ. ConstantTemperatureGradient) THEN
        OptStartSysAvailMgrData(SysAvailNum)%ConstTGradCool = rNumericArgs(2)
      END IF

      IF (OptStartSysAvailMgrData(SysAvailNum)%CtrlAlgType .EQ. ConstantTemperatureGradient) THEN
        OptStartSysAvailMgrData(SysAvailNum)%ConstTGradHeat = rNumericArgs(3)
      END IF

      IF (OptStartSysAvailMgrData(SysAvailNum)%CtrlAlgType .EQ. AdaptiveTemperatureGradient) THEN
        OptStartSysAvailMgrData(SysAvailNum)%InitTGradCool = rNumericArgs(4)
      END IF

      IF (OptStartSysAvailMgrData(SysAvailNum)%CtrlAlgType .EQ. AdaptiveTemperatureGradient) THEN
        OptStartSysAvailMgrData(SysAvailNum)%InitTGradHeat = rNumericArgs(5)
      END IF

      IF (OptStartSysAvailMgrData(SysAvailNum)%CtrlAlgType .EQ. ConstantStartTime) THEN
        OptStartSysAvailMgrData(SysAvailNum)%ConstStartTime = rNumericArgs(6)
      END IF

      IF (OptStartSysAvailMgrData(SysAvailNum)%CtrlAlgType .EQ. AdaptiveTemperatureGradient) THEN
        OptStartSysAvailMgrData(SysAvailNum)%NumPreDays = rNumericArgs(7)
       END IF

      CALL SetupOutputVariable('Availability Manager Optimum Start Control Status []', &
                                OptStartSysAvailMgrData(SysAvailNum)%AvailStatus, &
                               'System','Average',OptStartSysAvailMgrData(SysAvailNum)%Name)

      ! add
      CALL SetupOutputVariable('Availability Manager Optimum Start Hours Before Occupancy []', &
                                OptStartSysAvailMgrData(SysAvailNum)%NumHoursBeforeOccupancy, &
                               'System','Average',OptStartSysAvailMgrData(SysAvailNum)%Name, 'Daily')

    END DO

  END IF

  cCurrentModuleObject = 'AvailabilityManager:DifferentialThermostat'
  NumDiffTSysAvailMgrs     = GetNumObjectsFound(cCurrentModuleObject)

  IF(NumDiffTSysAvailMgrs > 0)THEN

    ALLOCATE(DiffTSysAvailMgrData(NumDiffTSysAvailMgrs))

    DO SysAvailNum = 1,NumDiffTSysAvailMgrs

      CALL GetObjectItem(cCurrentModuleObject,SysAvailNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus, &
                         AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK=.FALSE.
      IsBlank=.FALSE.
      CALL VerifyName(cAlphaArgs(1),DiffTSysAvailMgrData%Name,SysAvailNum-1,IsNotOK,IsBlank,&
                      TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      DiffTSysAvailMgrData(SysAvailNum)%Name = cAlphaArgs(1)
      DiffTSysAvailMgrData(SysAvailNum)%MgrType = SysAvailMgr_DiffThermo

      DiffTSysAvailMgrData(SysAvailNum)%HotNode = &
        GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
        NodeType_Unknown,NodeConnectionType_Sensor,1,ObjectIsNotParent)
      CALL MarkNode(DiffTSysAvailMgrData(SysAvailNum)%HotNode,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                          'Hot Node')
      DiffTSysAvailMgrData(SysAvailNum)%ColdNode = &
        GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
        NodeType_Unknown,NodeConnectionType_Sensor,1,ObjectIsNotParent)
      CALL MarkNode(DiffTSysAvailMgrData(SysAvailNum)%ColdNode,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                          'Cold Node')

      DiffTSysAvailMgrData(SysAvailNum)%TempDiffOn = rNumericArgs(1)

      IF (NumNumbers > 1) THEN
        DiffTSysAvailMgrData(SysAvailNum)%TempDiffOff = rNumericArgs(2)
      ELSE
        DiffTSysAvailMgrData(SysAvailNum)%TempDiffOff = DiffTSysAvailMgrData(SysAvailNum)%TempDiffOn
      END IF

      IF (DiffTSysAvailMgrData(SysAvailNum)%TempDiffOff > DiffTSysAvailMgrData(SysAvailNum)%TempDiffOn) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//' = "'//TRIM(cAlphaArgs(1))//'", invalid')
        CALL ShowContinueError('The '//TRIM(cNumericFieldNames(2))//' is greater than the '//TRIM(cNumericFieldNames(1))//'.')
        ErrorsFound = .TRUE.
      END IF

      CALL SetupOutputVariable('Availability Manager Differential Thermostat Control Status []', &
                                DiffTSysAvailMgrData(SysAvailNum)%AvailStatus, &
                               'System','Average',DiffTSysAvailMgrData(SysAvailNum)%Name)

    END DO ! SysAvailNum

  END IF

  cCurrentModuleObject = 'AvailabilityManager:HighTemperatureTurnOff'
  NumHiTurnOffSysAvailMgrs = GetNumObjectsFound(cCurrentModuleObject)

  IF(NumHiTurnOffSysAvailMgrs > 0)THEN
    ALLOCATE(HiTurnOffSysAvailMgrData(NumHiTurnOffSysAvailMgrs))

    DO SysAvailNum = 1,NumHiTurnOffSysAvailMgrs

      CALL GetObjectItem(cCurrentModuleObject,SysAvailNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus, &
                         AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK=.FALSE.
      IsBlank=.FALSE.
      CALL VerifyName(cAlphaArgs(1),HiTurnOffSysAvailMgrData%Name,SysAvailNum-1,IsNotOK,IsBlank,&
                      TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      HiTurnOffSysAvailMgrData(SysAvailNum)%Name = cAlphaArgs(1)
      HiTurnOffSysAvailMgrData(SysAvailNum)%MgrType = SysAvailMgr_HiTempTOff

      HiTurnOffSysAvailMgrData(SysAvailNum)%Node = &
        GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
        NodeType_Unknown,NodeConnectionType_Sensor,1,ObjectIsNotParent)
      CALL MarkNode(HiTurnOffSysAvailMgrData(SysAvailNum)%Node,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                         'Sensor Node')

      HiTurnOffSysAvailMgrData(SysAvailNum)%Temp = rNumericArgs(1)

      CALL SetupOutputVariable('Availability Manager High Temperature Turn Off Control Status []', &
                                HiTurnOffSysAvailMgrData(SysAvailNum)%AvailStatus, &
                               'System','Average',HiTurnOffSysAvailMgrData(SysAvailNum)%Name)

    END DO ! SysAvailNum

  END IF


  cCurrentModuleObject = 'AvailabilityManager:HighTemperatureTurnOn'
  NumHiTurnOnSysAvailMgrs  = GetNumObjectsFound(cCurrentModuleObject)

  IF(NumHiTurnOnSysAvailMgrs > 0)THEN

    ALLOCATE(HiTurnOnSysAvailMgrData(NumHiTurnOnSysAvailMgrs))

    DO SysAvailNum = 1,NumHiTurnOnSysAvailMgrs

      CALL GetObjectItem(cCurrentModuleObject,SysAvailNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus, &
                         AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK=.FALSE.
      IsBlank=.FALSE.
      CALL VerifyName(cAlphaArgs(1),HiTurnOnSysAvailMgrData%Name,SysAvailNum-1,IsNotOK,IsBlank,&
                      TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      HiTurnOnSysAvailMgrData(SysAvailNum)%Name = cAlphaArgs(1)
      HiTurnOnSysAvailMgrData(SysAvailNum)%MgrType = SysAvailMgr_HiTempTOn

      HiTurnOnSysAvailMgrData(SysAvailNum)%Node = &
        GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
        NodeType_Unknown,NodeConnectionType_Sensor,1,ObjectIsNotParent)
      CALL MarkNode(HiTurnOnSysAvailMgrData(SysAvailNum)%Node,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                          'Sensor Node')

      HiTurnOnSysAvailMgrData(SysAvailNum)%Temp = rNumericArgs(1)

      CALL SetupOutputVariable('Availability Manager High Temperature Turn On Control Status []', &
                                HiTurnOnSysAvailMgrData(SysAvailNum)%AvailStatus, &
                               'System','Average',HiTurnOnSysAvailMgrData(SysAvailNum)%Name)

    END DO ! SysAvailNum

  END IF

  cCurrentModuleObject = 'AvailabilityManager:LowTemperatureTurnOff'
  NumLoTurnOffSysAvailMgrs = GetNumObjectsFound(cCurrentModuleObject)

  IF(NumLoTurnOffSysAvailMgrs > 0)THEN

    ALLOCATE(LoTurnOffSysAvailMgrData(NumLoTurnOffSysAvailMgrs))

    DO SysAvailNum = 1,NumLoTurnOffSysAvailMgrs

      CALL GetObjectItem(cCurrentModuleObject,SysAvailNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus, &
                         AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK=.FALSE.
      IsBlank=.FALSE.
      CALL VerifyName(cAlphaArgs(1),LoTurnOffSysAvailMgrData%Name,SysAvailNum-1,IsNotOK,IsBlank,&
                      TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      LoTurnOffSysAvailMgrData(SysAvailNum)%Name = cAlphaArgs(1)
      LoTurnOffSysAvailMgrData(SysAvailNum)%MgrType = SysAvailMgr_LoTempTOff

      LoTurnOffSysAvailMgrData(SysAvailNum)%Node = &
        GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
        NodeType_Unknown,NodeConnectionType_Sensor,1,ObjectIsNotParent)
      CALL MarkNode(LoTurnOffSysAvailMgrData(SysAvailNum)%Node,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                               'Sensor Node')

      LoTurnOffSysAvailMgrData(SysAvailNum)%Temp = rNumericArgs(1)

      IF (.NOT. lAlphaFieldBlanks(3)) THEN
        LoTurnOffSysAvailMgrData(SysAvailNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(3))
        IF (LoTurnOffSysAvailMgrData(SysAvailNum)%SchedPtr .EQ. 0) THEN
          CALL ShowSevereError(RoutineName//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'" not found.')
          CALL ShowContinueError('Occurs in '//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'".')
          ErrorsFound = .TRUE.
        END IF
      ELSE
        LoTurnOffSysAvailMgrData(SysAvailNum)%SchedPtr = 0
      END IF

      CALL SetupOutputVariable('Availability Manager Low Temperature Turn Off Control Status []', &
                                LoTurnOffSysAvailMgrData(SysAvailNum)%AvailStatus, &
                               'System','Average',LoTurnOffSysAvailMgrData(SysAvailNum)%Name)

    END DO ! SysAvailNum

  END IF

  cCurrentModuleObject = 'AvailabilityManager:LowTemperatureTurnOn'
  NumLoTurnOnSysAvailMgrs  = GetNumObjectsFound(cCurrentModuleObject)

  IF(NumLoTurnOnSysAvailMgrs > 0)THEN

    ALLOCATE(LoTurnOnSysAvailMgrData(NumLoTurnOnSysAvailMgrs))

    DO SysAvailNum = 1,NumLoTurnOnSysAvailMgrs

      CALL GetObjectItem(cCurrentModuleObject,SysAvailNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus, &
                         AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK=.FALSE.
      IsBlank=.FALSE.
      CALL VerifyName(cAlphaArgs(1),LoTurnOnSysAvailMgrData%Name,SysAvailNum-1,IsNotOK,IsBlank,&
                      TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      LoTurnOnSysAvailMgrData(SysAvailNum)%Name = cAlphaArgs(1)
      LoTurnOnSysAvailMgrData(SysAvailNum)%MgrType = SysAvailMgr_LoTempTOn

      LoTurnOnSysAvailMgrData(SysAvailNum)%Node = &
        GetOnlySingleNode(cAlphaArgs(2),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
        NodeType_Unknown,NodeConnectionType_Sensor,1,ObjectIsNotParent)
      CALL MarkNode(LoTurnOnSysAvailMgrData(SysAvailNum)%Node,TRIM(cCurrentModuleObject),cAlphaArgs(1),  &
                                'Sensor Node')

      LoTurnOnSysAvailMgrData(SysAvailNum)%Temp = rNumericArgs(1)

      CALL SetupOutputVariable('Availability Manager Low Temperature Turn On Control Status []', &
                                LoTurnOnSysAvailMgrData(SysAvailNum)%AvailStatus, &
                               'System','Average',LoTurnOnSysAvailMgrData(SysAvailNum)%Name)

    END DO ! SysAvailNum

  END IF

  cCurrentModuleObject = 'AvailabilityManager:NightVentilation'
  NumNVentSysAvailMgrs = GetNumObjectsFound(cCurrentModuleObject)

  IF(NumNVentSysAvailMgrs > 0)THEN

    ALLOCATE(NVentSysAvailMgrData(NumNVentSysAvailMgrs))

    DO SysAvailNum = 1,NumNVentSysAvailMgrs

      CALL GetObjectItem(cCurrentModuleObject,SysAvailNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus, &
                         AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK=.FALSE.
      IsBlank=.FALSE.
      CALL VerifyName(cAlphaArgs(1),NVentSysAvailMgrData%Name,SysAvailNum-1,IsNotOK,IsBlank,&
                      TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.TRUE.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      NVentSysAvailMgrData(SysAvailNum)%Name = cAlphaArgs(1)
      NVentSysAvailMgrData(SysAvailNum)%MgrType = SysAvailMgr_NightVent

      NVentSysAvailMgrData(SysAvailNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(2))
      IF (NVentSysAvailMgrData(SysAvailNum)%SchedPtr .EQ. 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid')
        CALL ShowContinueError('not found: '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
        ErrorsFound = .TRUE.
      END IF
      NVentSysAvailMgrData(SysAvailNum)%FanSched = cAlphaArgs(3)
      NVentSysAvailMgrData(SysAvailNum)%FanSchedPtr = GetScheduleIndex(cAlphaArgs(3))
      IF (NVentSysAvailMgrData(SysAvailNum)%FanSchedPtr .EQ. 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid')
        CALL ShowContinueError('not found: '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
        ErrorsFound = .TRUE.
      END IF
      NVentSysAvailMgrData(SysAvailNum)%VentTempSched = cAlphaArgs(4)
      NVentSysAvailMgrData(SysAvailNum)%VentTempSchedPtr = GetScheduleIndex(cAlphaArgs(4))
      IF (NVentSysAvailMgrData(SysAvailNum)%VentTempSchedPtr .EQ. 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid')
        CALL ShowContinueError('not found: '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
        ErrorsFound = .TRUE.
      END IF
      NVentSysAvailMgrData(SysAvailNum)%VentDelT = rNumericArgs(1)
      NVentSysAvailMgrData(SysAvailNum)%VentTempLowLim = rNumericArgs(2)
      NVentSysAvailMgrData(SysAvailNum)%VentFlowFrac = rNumericArgs(3)
      NVentSysAvailMgrData(SysAvailNum)%CtrlZoneName = cAlphaArgs(5)
      NVentSysAvailMgrData(SysAvailNum)%ZoneNum = FindItemInList(cAlphaArgs(5),Zone%Name,NumOfZones)
      IF (NVentSysAvailMgrData(SysAvailNum)%ZoneNum .EQ. 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid')
        CALL ShowContinueError('not found: '//TRIM(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
        ErrorsFound = .TRUE.
      END IF

      CALL SetupOutputVariable('Availability Manager Night Ventilation Control Status []', &
                                NVentSysAvailMgrData(SysAvailNum)%AvailStatus, &
                               'System','Average',NVentSysAvailMgrData(SysAvailNum)%Name)

    END DO ! SysAvailNum

  END IF

  DEALLOCATE(cAlphaFieldNames)
  DEALLOCATE(cAlphaArgs)
  DEALLOCATE(lAlphaFieldBlanks)
  DEALLOCATE(cNumericFieldNames)
  DEALLOCATE(rNumericArgs)
  DEALLOCATE(lNumericFieldBlanks)

  IF (ErrorsFound) THEN
      CALL ShowFatalError(RoutineName//'Errors found in input.  Preceding condition(s) cause termination.')
  END IF

  RETURN

END SUBROUTINE GetSysAvailManagerInputs

SUBROUTINE GetSysAvailManagerListInputs

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine gets the System Availability Manager List object input and stores
          ! it for later retrieval of items from the Plant and Air Loops.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cAlphaFieldNames
  CHARACTER(len=MaxNameLength+40),ALLOCATABLE, DIMENSION(:) :: cNumericFieldNames
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericFieldBlanks
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaFieldBlanks
  CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: cAlphaArgs
  REAL(r64),ALLOCATABLE, DIMENSION(:) :: rNumericArgs
  CHARACTER(len=MaxNameLength) :: cCurrentModuleObject
  integer :: NumAlphas
  integer :: NumNumbers
  integer :: numArgs
  integer :: Item
  integer :: IOstatus
  logical :: IsNotOK
  logical :: IsBlank
  logical :: ErrorsFound
  integer :: list
  integer :: itemnum

  IF (GetAvailMgrInputFlag) THEN
    CALL GetSysAvailManagerInputs
    GetAvailMgrInputFlag=.FALSE.
  ENDIF

  ErrorsFound=.FALSE.

  cCurrentModuleObject ='AvailabilityManagerAssignmentList'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,numArgs,NumAlphas,NumNumbers)
  ALLOCATE(cAlphaFieldNames(NumAlphas))
  cAlphaFieldNames=' '
  ALLOCATE(cAlphaArgs(NumAlphas))
  cAlphaArgs=' '
  ALLOCATE(lAlphaFieldBlanks(NumAlphas))
  lAlphaFieldBlanks=.false.
  ALLOCATE(cNumericFieldNames(NumNumbers))
  cNumericFieldNames=' '
  ALLOCATE(rNumericArgs(NumNumbers))
  rNumericArgs=0.0d0
  ALLOCATE(lNumericFieldBlanks(NumNumbers))
  lNumericFieldBlanks=.false.


  cCurrentModuleObject ='AvailabilityManagerAssignmentList'
  NumAvailManagerLists = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumAvailManagerLists > 0) THEN

    ALLOCATE(SysAvailMgrListData(NumAvailManagerLists))

    DO Item=1,NumAvailManagerLists
      CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

      IsNotOK=.FALSE.
      IsBlank=.FALSE.
      CALL VerifyName(cAlphaArgs(1),SysAvailMgrListData%Name,Item-1,IsNotOK,IsBlank,&
                      TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      SysAvailMgrListData(Item)%Name=cAlphaArgs(1)

      SysAvailMgrListData(Item)%NumItems = (NumAlphas-1)/2  ! Subtract off the list name first
      ALLOCATE(SysAvailMgrListData(Item)%AvailManagerName(SysAvailMgrListData(Item)%NumItems))
      SysAvailMgrListData(Item)%AvailManagerName=' '
      ALLOCATE(SysAvailMgrListData(Item)%cAvailManagerType(SysAvailMgrListData(Item)%NumItems))
      SysAvailMgrListData(Item)%cAvailManagerType=' '
      ALLOCATE(SysAvailMgrListData(Item)%AvailManagerType(SysAvailMgrListData(Item)%NumItems))
      SysAvailMgrListData(Item)%AvailManagerType=0

      ! retrieve data

      itemnum=1
      DO list=1,SysAvailMgrListData(Item)%NumItems
        itemnum=itemnum+1
        SysAvailMgrListData(Item)%cAvailManagerType(list)= cAlphaArgs(itemnum)
        SysAvailMgrListData(Item)%AvailManagerType(list) = ValidateAndSetSysAvailabilityManagerType(cAlphaArgs(itemnum))
        ! these are validated individually in the GetPlant, GetSystem and GetZoneEq lists
        itemnum=itemnum+1
        SysAvailMgrListData(Item)%AvailManagerName(list) = cAlphaArgs(itemnum)
      ENDDO  ! End of retrieving items
    ENDDO

  ENDIF

  DEALLOCATE(cAlphaFieldNames)
  DEALLOCATE(cAlphaArgs)
  DEALLOCATE(lAlphaFieldBlanks)
  DEALLOCATE(cNumericFieldNames)
  DEALLOCATE(rNumericArgs)
  DEALLOCATE(lNumericFieldBlanks)

  IF (ErrorsFound) THEN
    CALL ShowFatalError('GetSysAvailManagerListInputs: Program terminates due to preceding conditions.')
  ENDIF

 RETURN

END SUBROUTINE GetSysAvailManagerListInputs

SUBROUTINE GetPlantAvailabilityManager(AvailabilityListName,Loop,NumPlantLoops,ErrorsFound)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the plant availability manager data for the indicated
          ! loop.  If the PlantAvailMgr structure has not been allocated, it will be allocated
          ! to "number of plant loops".

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: AvailabilityListName  ! name that should be an Availability Manager List Name
  INTEGER, INTENT(IN)          :: Loop                  ! which loop this is
  INTEGER, INTENT(IN)          :: NumPlantLoops         ! Total number of plant loops
  LOGICAL, INTENT(INOUT)       :: ErrorsFound           ! true if certain errors are detected here

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Found
  INTEGER Num

  IF (GetAvailListsInput) THEN
    CALL GetSysAvailManagerListInputs
    GetAvailListsInput=.FALSE.
  ENDIF

  IF (.not. ALLOCATED(PlantAvailMgr)) THEN
    ALLOCATE(PlantAvailMgr(NumPlantLoops))
  ENDIF

  Found=0
  IF (NumAvailManagerLists > 0) &
    Found=FindItemInList(AvailabilityListName,SysAvailMgrListData%Name,NumAvailManagerLists)

  IF (Found /= 0) THEN
    PlantAvailMgr(Loop)%NumAvailManagers = SysAvailMgrListData(Found)%NumItems
    PlantAvailMgr(Loop)%AvailStatus = NoAction
    PlantAvailMgr(Loop)%StartTime = 0
    PlantAvailMgr(Loop)%StopTime = 0
    ALLOCATE(PlantAvailMgr(Loop)%AvailManagerName(PlantAvailMgr(Loop)%NumAvailManagers))
    ALLOCATE(PlantAvailMgr(Loop)%AvailManagerType(PlantAvailMgr(Loop)%NumAvailManagers))
    ALLOCATE(PlantAvailMgr(Loop)%AvailManagerNum(PlantAvailMgr(Loop)%NumAvailManagers))
    DO Num=1,PlantAvailMgr(Loop)%NumAvailManagers
      PlantAvailMgr(Loop)%AvailManagerName(Num) = SysAvailMgrListData(Found)%AvailManagerName(Num)
      PlantAvailMgr(Loop)%AvailManagerNum(Num)  = 0
      PlantAvailMgr(Loop)%AvailManagerType(Num) = SysAvailMgrListData(Found)%AvailManagerType(Num)
      IF (PlantAvailMgr(Loop)%AvailManagerType(Num) == 0) THEN
        CALL ShowSevereError('GetPlantLoopData/GetPlantAvailabilityManager: '//  &
           'Invalid System Availability Manager Type entered="'//  &
           TRIM(SysAvailMgrListData(Found)%cAvailManagerType(Num))//'".')
        CALL ShowContinueError('Occurs in AvailabilityManagerAssignmentList="'//  &
           TRIM(AvailabilityListName)//'".')
        ErrorsFound=.TRUE.
      ENDIF
      IF (SysAvailMgrListData(Found)%AvailManagerType(Num) == SysAvailMgr_DiffThermo .and.  &
          Num /= PlantAvailMgr(Loop)%NumAvailManagers) THEN
        CALL ShowWarningError('GetPlantLoopData/GetPlantAvailabilityManager: '//  &
           'AvailabilityManager:DifferentialThermostat="'//  &
           TRIM(SysAvailMgrListData(Found)%AvailManagerName(Num))//'".')
        CALL ShowContinueError('...is not the last manager on the AvailabilityManagerAssignmentList.  '//  &
                               'Any remaining managers will not be used.')
        CALL ShowContinueError('Occurs in AvailabilityManagerAssignmentList ="'//  &
           TRIM(AvailabilityListName)//'".')
      ENDIF
      IF (SysAvailMgrListData(Found)%AvailManagerType(Num) == SysAvailMgr_NightVent .OR. &
          SysAvailMgrListData(Found)%AvailManagerType(Num) == SysAvailMgr_NightCycle) THEN
        CALL ShowSevereError('GetPlantLoopData/GetPlantAvailabilityManager: '//  &
           'Invalid System Availability Manager Type entered="'//  &
           TRIM(SysAvailMgrListData(Found)%cAvailManagerType(Num))//'".')
        CALL ShowContinueError('...this manager is not used in a Plant Loop.')
        CALL ShowContinueError('Occurs in AvailabilityManagerAssignmentList="'//  &
           TRIM(AvailabilityListName)//'".')
        ErrorsFound=.TRUE.
      ENDIF
    END DO  !End of Num Loop

  ELSE
    IF (AvailabilityListName /= ' ') THEN
      CALL ShowWarningError('GetPlantLoopData/GetPlantAvailabilityManager: AvailabilityManagerAssignmentList='//  &
         TRIM(AvailabilityListName)//' not found in lists.  No availability will be used.')
    ENDIF
    PlantAvailMgr(Loop)%NumAvailManagers = 0
    PlantAvailMgr(Loop)%AvailStatus = NoAction
    ALLOCATE(PlantAvailMgr(Loop)%AvailManagerName(PlantAvailMgr(Loop)%NumAvailManagers))
    ALLOCATE(PlantAvailMgr(Loop)%AvailManagerType(PlantAvailMgr(Loop)%NumAvailManagers))
    ALLOCATE(PlantAvailMgr(Loop)%AvailManagerNum(PlantAvailMgr(Loop)%NumAvailManagers))
  END IF

  RETURN

END SUBROUTINE GetPlantAvailabilityManager

SUBROUTINE GetAirLoopAvailabilityManager(AvailabilityListName,Loop,NumAirLoops,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the availability manager data for the indicated air
          ! loop or for the indicated type of zone equipment component.
          ! If the PriAirSysAvailMgr structure has not been allocated, it will be allocated
          ! to "number of air loops".

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataAirLoop
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: AvailabilityListName  ! name that should be an Availability Manager List Name
  INTEGER, INTENT(IN)          :: Loop                  ! which loop this is
  INTEGER, INTENT(IN)          :: NumAirLoops           ! Total number of air loops
  LOGICAL, INTENT(INOUT)       :: ErrorsFound           ! true if certain errors are detected here

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Found
  INTEGER :: Num
!  INTEGER :: CompNumAvailManagers ! Number of availability managers associated with a ZoneHVAC:* component

  IF (GetAvailListsInput) THEN
    CALL GetSysAvailManagerListInputs
    GetAvailListsInput=.FALSE.
  ENDIF

  IF (.not. ALLOCATED(PriAirSysAvailMgr)) THEN
    ALLOCATE(PriAirSysAvailMgr(NumAirLoops))
  ENDIF

  Found=0
  IF (NumAvailManagerLists > 0) &
    Found=FindItemInList(AvailabilityListName,SysAvailMgrListData%Name,NumAvailManagerLists)

  IF (Found /= 0) THEN
    PriAirSysAvailMgr(Loop)%NumAvailManagers = SysAvailMgrListData(Found)%NumItems
    PriAirSysAvailMgr(Loop)%AvailStatus = NoAction
    PriAirSysAvailMgr(Loop)%StartTime = 0
    PriAirSysAvailMgr(Loop)%StopTime = 0
    PriAirSysAvailMgr(Loop)%ReqSupplyFrac = 1.0d0
    ALLOCATE(PriAirSysAvailMgr(Loop)%AvailManagerName(PriAirSysAvailMgr(Loop)%NumAvailManagers))
    ALLOCATE(PriAirSysAvailMgr(Loop)%AvailManagerType(PriAirSysAvailMgr(Loop)%NumAvailManagers))
    ALLOCATE(PriAirSysAvailMgr(Loop)%AvailManagerNum(PriAirSysAvailMgr(Loop)%NumAvailManagers))
    DO Num=1,PriAirSysAvailMgr(Loop)%NumAvailManagers
      PriAirSysAvailMgr(Loop)%AvailManagerName(Num) = SysAvailMgrListData(Found)%AvailManagerName(Num)
      PriAirSysAvailMgr(Loop)%AvailManagerNum(Num)  = 0
      PriAirSysAvailMgr(Loop)%AvailManagerType(Num) = SysAvailMgrListData(Found)%AvailManagerType(Num)
      IF (PriAirSysAvailMgr(Loop)%AvailManagerType(Num) == 0) THEN
        CALL ShowSevereError('GetAirPathData/GetAirLoopAvailabilityManager: '//  &
           'Invalid AvailabilityManagerAssignmentList Type entered="'//  &
           TRIM(SysAvailMgrListData(Found)%cAvailManagerType(Num))//'".')
        CALL ShowContinueError('Occurs in AvailabilityManagerAssignmentList="'//  &
          TRIM(SysAvailMgrListData(Found)%AvailManagerName(Num))//'".')
         ErrorsFound=.TRUE.
      ENDIF
      IF (SysAvailMgrListData(Found)%AvailManagerType(Num) == SysAvailMgr_DiffThermo .and.  &
          Num /= PriAirSysAvailMgr(Loop)%NumAvailManagers) THEN
        CALL ShowWarningError('GetAirPathData/GetAirLoopAvailabilityManager: '//  &
           'AvailabilityManager:DifferentialThermostat="'//  &
           TRIM(SysAvailMgrListData(Found)%AvailManagerName(Num))//'".')
        CALL ShowContinueError('...is not the last manager on the AvailabilityManagerAssignmentList.  '//  &
                               'Any remaining managers will not be used.')
        CALL ShowContinueError('Occurs in AvailabilityManagerAssignmentList="'//  &
           TRIM(SysAvailMgrListData(Found)%AvailManagerName(Num))//'".')
      ENDIF
    END DO  !End of Num Loop

  ELSE
    IF (AvailabilityListName /= ' ') THEN
      CALL ShowWarningError('GetAirPathData/GetAirLoopAvailabilityManager: AvailabilityManagerAssignmentList='//  &
         TRIM(AvailabilityListName)//' not found in lists.  No availability will be used.')
    ENDIF
    PriAirSysAvailMgr(Loop)%NumAvailManagers = 0
    PriAirSysAvailMgr(Loop)%AvailStatus = NoAction
    ALLOCATE(PriAirSysAvailMgr(Loop)%AvailManagerName(PriAirSysAvailMgr(Loop)%NumAvailManagers))
    ALLOCATE(PriAirSysAvailMgr(Loop)%AvailManagerType(PriAirSysAvailMgr(Loop)%NumAvailManagers))
    ALLOCATE(PriAirSysAvailMgr(Loop)%AvailManagerNum(PriAirSysAvailMgr(Loop)%NumAvailManagers))
  END IF

  RETURN

END SUBROUTINE GetAirLoopAvailabilityManager

SUBROUTINE GetZoneEqAvailabilityManager(ZoneEquipType, CompNum, ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   April 2011
          !       MODIFIED       Chandan Sharma, March 2011/July 2012 - FSEC: Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the availability manager data for the indicated type of zone
          ! equipment component.
          ! If not allocated, ZoneComp structure will be allocated to "Total num of zone equip types" and
          ! ZoneCompAvailMgrs structure will be allocated to "Total number of components of the indicated type".

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT)       :: ErrorsFound           ! true if certain errors are detected here
  INTEGER, INTENT(IN)          :: ZoneEquipType     ! Type of ZoneHVAC:* component
  INTEGER, INTENT(IN)          :: CompNum           ! Index of a particular ZoneHVAC:* component

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength) :: AvailabilityListName  ! name that should be an Availability Manager List Name
  INTEGER :: Found
  INTEGER :: Num
  INTEGER :: CompNumAvailManagers ! Number of availability managers associated with a ZoneHVAC:* component

  IF (GetAvailListsInput) THEN
    CALL GetSysAvailManagerListInputs
    GetAvailListsInput=.FALSE.
  ENDIF

  IF (ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%Input) THEN
    AvailabilityListName = ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%AvailManagerListName
    Found=0
    IF (NumAvailManagerLists > 0) &
      Found = FindItemInList( AvailabilityListName, SysAvailMgrListData%Name,NumAvailManagerLists)
    IF (Found /= 0) THEN
      ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%NumAvailManagers = SysAvailMgrListData(Found)%NumItems
      CompNumAvailManagers = ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%NumAvailManagers
      ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%AvailStatus = NoAction
      ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%StartTime = 0
      ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%StopTime = 0
      IF (.NOT. ALLOCATED(ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%AvailManagerName)) THEN
        ALLOCATE(ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%AvailManagerName(CompNumAvailManagers))
        ALLOCATE(ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%AvailManagerType(CompNumAvailManagers))
        ALLOCATE(ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%AvailManagerNum(CompNumAvailManagers))
      ENDIF
      DO Num=1,ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%NumAvailManagers
        ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%AvailManagerName(Num) = &
                                                                     SysAvailMgrListData(Found)%AvailManagerName(Num)
        ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%AvailManagerNum(Num)  = 0
        ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%AvailManagerType(Num) = &
                                                                     SysAvailMgrListData(Found)%AvailManagerType(Num)
        IF (ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%AvailManagerType(Num) == 0) THEN
          CALL ShowSevereError('GetZoneEqAvailabilityManager: '//  &
             'Invalid AvailabilityManagerAssignmentList Type entered="'//  &
             TRIM(SysAvailMgrListData(Found)%cAvailManagerType(Num))//'".')
          CALL ShowContinueError('Occurs in AvailabilityManagerAssignmentList="'//  &
            TRIM(SysAvailMgrListData(Found)%AvailManagerName(Num))//'".')
           ErrorsFound=.TRUE.
        ENDIF
        IF (SysAvailMgrListData(Found)%AvailManagerType(Num) == SysAvailMgr_DiffThermo .and.  &
            Num /= ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%NumAvailManagers) THEN
          CALL ShowWarningError('GetZoneEqAvailabilityManager: '//  &
             'AvailabilityManager:DifferentialThermostat="'//  &
             TRIM(SysAvailMgrListData(Found)%AvailManagerName(Num))//'".')
          CALL ShowContinueError('...is not the last manager on the AvailabilityManagerAssignmentList.  '//  &
                                 'Any remaining managers will not be used.')
          CALL ShowContinueError('Occurs in AvailabilityManagerAssignmentList="'//  &
             TRIM(SysAvailMgrListData(Found)%AvailManagerName(Num))//'".')
        ENDIF
      END DO  !End of Num Loop
    END IF
    ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%Input = .FALSE.
  ENDIF

  RETURN

END SUBROUTINE GetZoneEqAvailabilityManager

SUBROUTINE InitSysAvailManagers

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   August 2001
          !       MODIFIED       Brent Griffith, CR8376 initialize to NoAction every timestep
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the System Availability Manager objects.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: ZoneEquipConfig, NumValidSysAvailZoneComponents
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! NA

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE  :: MyOneTimeFlag = .TRUE.    ! One time flag
  INTEGER       :: SysAvailNum               ! DO loop indes for Sys Avail Manager objects
  INTEGER       :: ControlledZoneNum         ! Index into the ZoneEquipConfig array
  INTEGER       :: ZoneEquipType
  INTEGER       :: ZoneListNum
  INTEGER       :: ScanZoneListNum
  INTEGER       :: ZoneNum
  INTEGER       :: NumOfZoneLists=1
  ! One time initializations
  IF (MyOneTimeFlag) THEN

    DO SysAvailNum = 1,NumNCycSysAvailMgrs
      IF (NCycSysAvailMgrData(SysAvailNum)%CtrlType .EQ. CycleOnControlZone) THEN
        ! set the controlled zone numbers
        DO ControlledZoneNum = 1,NumOfZones
          IF (ALLOCATED(ZoneEquipConfig)) THEN
            IF (ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum .EQ. NCycSysAvailMgrData(SysAvailNum)%ZoneNum ) THEN
              NCycSysAvailMgrData(SysAvailNum)%ControlledZoneNum = ControlledZoneNum
              EXIT
            END IF
          ENDIF
        END DO
      END IF
    END DO


    DO SysAvailNum = 1,NumOptStartSysAvailMgrs
      SELECT CASE (OptStartSysAvailMgrData(SysAvailNum)%CtrlType)
      CASE (ControlZone)
        ! set the controlled zone numbers
        DO ControlledZoneNum = 1,NumOfZones
          IF (ALLOCATED(ZoneEquipConfig)) THEN
            IF (ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum .EQ. OptStartSysAvailMgrData(SysAvailNum)%ZoneNum ) THEN
              OptStartSysAvailMgrData(SysAvailNum)%ControlledZoneNum = ControlledZoneNum
              EXIT
            END IF
          ENDIF
        END DO
      CASE (MaximumofZoneList)
        !a zone list
        ZoneListNum = FindItemInList(OptStartSysAvailMgrData(SysAvailNum)%ZoneListName,ZoneList%Name,NumOfZoneLists)
        IF(ZoneListNum .GT. 0)THEN
          OptStartSysAvailMgrData(SysAvailNum)%NumOfZones = ZoneList(ZoneListNum)%NumofZones
          IF (.NOT. ALLOCATED(OptStartSysAvailMgrData(SysAvailNum)%ZonePtrs)) THEN
            ALLOCATE (OptStartSysAvailMgrData(SysAvailNum)%ZonePtrs(1:ZoneList(ZoneListNum)%NumofZones))
          ENDIF
          DO ScanZoneListNum = 1, ZoneList(ZoneListNum)%NumofZones
            ZoneNum = ZoneList(ZoneListNum)%Zone(ScanZoneListNum)
            OptStartSysAvailMgrData(SysAvailNum)%ZonePtrs(ScanZoneListNum) = ZoneNum
          END DO
      END IF
    END SELECT
  END DO

    DO SysAvailNum = 1,NumNVentSysAvailMgrs
      ! set the controlled zone numbers
      DO ControlledZoneNum = 1,NumOfZones
        IF (ALLOCATED(ZoneEquipConfig)) THEN
          IF (ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum .EQ. NVentSysAvailMgrData(SysAvailNum)%ZoneNum ) THEN
            NVentSysAvailMgrData(SysAvailNum)%ControlledZoneNum = ControlledZoneNum
            EXIT
          END IF
        ENDIF
      END DO
    END DO

    MyOneTimeFlag = .FALSE.

  END IF ! end 1 time initializations

  ! initialize individual availability managers to no action (CR 8376 reporting issue)
  IF (ALLOCATED(SchedSysAvailMgrData))     SchedSysAvailMgrData%AvailStatus     = NoAction
  IF (ALLOCATED(SchedOnSysAvailMgrData))   SchedOnSysAvailMgrData%AvailStatus   = NoAction
  IF (ALLOCATED(SchedOffSysAvailMgrData))  SchedOffSysAvailMgrData%AvailStatus  = NoAction
  IF (ALLOCATED(NCycSysAvailMgrData))      NCycSysAvailMgrData%AvailStatus      = NoAction
  IF (ALLOCATED(NVentSysAvailMgrData))     NVentSysAvailMgrData%AvailStatus     = NoAction
  IF (ALLOCATED(DiffTSysAvailMgrData))     DiffTSysAvailMgrData%AvailStatus     = NoAction
  IF (ALLOCATED(HiTurnOffSysAvailMgrData)) HiTurnOffSysAvailMgrData%AvailStatus = NoAction
  IF (ALLOCATED(HiTurnOnSysAvailMgrData))  HiTurnOnSysAvailMgrData%AvailStatus  = NoAction
  IF (ALLOCATED(LoTurnOffSysAvailMgrData)) LoTurnOffSysAvailMgrData%AvailStatus = NoAction
  IF (ALLOCATED(LoTurnOnSysAvailMgrData))  LoTurnOnSysAvailMgrData%AvailStatus  = NoAction
  IF (ALLOCATED(OptStartSysAvailMgrData))  OptStartSysAvailMgrData%AvailStatus  = NoAction
!  HybridVentSysAvailMgrData%AvailStatus= NoAction
  DO ZoneEquipType = 1,NumValidSysAvailZoneComponents  ! loop over the zone equipment types
    IF(ALLOCATED(ZoneComp))THEN
      IF(ZoneComp(ZoneEquipType)%TotalNumComp .GT. 0) &
        ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs%AvailStatus = NoAction
    ENDIF
  ENDDO

  RETURN

END SUBROUTINE InitSysAvailManagers

SUBROUTINE SimSysAvailManager(SysAvailType,SysAvailName,SysAvailNum,PriAirSysNum,PreviousStatus,AvailStatus,ZoneEquipType, CompNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   August 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Loop over all the System Availability Managers and invoke the correct
          ! System Availability Manager algorithm.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General,        ONLY: TrimSigDigits

  IMPLICIT NONE

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,          INTENT(IN) :: SysAvailType
!  CHARACTER(len=*), INTENT(IN) :: SysAvailType
  CHARACTER(len=*), INTENT(IN) :: SysAvailName
  INTEGER,       INTENT(INOUT) :: SysAvailNum
  INTEGER,          INTENT(IN) :: PriAirSysNum ! Primary Air System index. If being called for a ZoneHVAC:* component
                                               ! then a dummyvariable is passed in to this subroutine.
  INTEGER,          INTENT(IN) :: PreviousStatus
  INTEGER,         INTENT(OUT) :: AvailStatus
  INTEGER,  OPTIONAL,  INTENT(IN)  :: ZoneEquipType   ! Type of ZoneHVAC:* equipment component
  INTEGER,  OPTIONAL,  INTENT(IN)  :: CompNum         ! Index of ZoneHVAC:* equipment component
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  SELECT CASE(SysAvailType)
    CASE(SysAvailMgr_Scheduled) ! 'AvailabilityManager:Scheduled'
      IF (SysAvailNum == 0) THEN
        SysAvailNum = FindItemInList(SysAvailName,SchedSysAvailMgrData%Name,NumSchedSysAvailMgrs)
      ENDIF
      IF (SysAvailNum > 0) THEN
        CALL CalcSchedSysAvailMgr(SysAvailNum,AvailStatus)
      ELSE
        CALL ShowFatalError('SimSysAvailManager: AvailabilityManager:Scheduled not found: '//TRIM(SysAvailName))
      ENDIF

    CASE(SysAvailMgr_ScheduledOn) ! 'AvailabilityManager:ScheduledOn'
      IF (SysAvailNum == 0) THEN
        SysAvailNum = FindItemInList(SysAvailName,SchedOnSysAvailMgrData%Name,NumSchedOnSysAvailMgrs)
      ENDIF
      IF (SysAvailNum > 0) THEN
        CALL CalcSchedOnSysAvailMgr(SysAvailNum,AvailStatus)
      ELSE
        CALL ShowFatalError('SimSysAvailManager: AvailabilityManager:ScheduledOn not found: '//TRIM(SysAvailName))
      ENDIF

    CASE(SysAvailMgr_ScheduledOff) ! 'AvailabilityManager:ScheduledOff'
      IF (SysAvailNum == 0) THEN
        SysAvailNum = FindItemInList(SysAvailName,SchedOffSysAvailMgrData%Name,NumSchedOffSysAvailMgrs)
      ENDIF
      IF (SysAvailNum > 0) THEN
        CALL CalcSchedOffSysAvailMgr(SysAvailNum,AvailStatus)
      ELSE
        CALL ShowFatalError('SimSysAvailManager: AvailabilityManager:ScheduledOff not found: '//TRIM(SysAvailName))
      ENDIF

    CASE(SysAvailMgr_NightCycle) ! 'AvailabilityManager:NightCycle'
      IF (SysAvailNum == 0) THEN
        SysAvailNum = FindItemInList(SysAvailName,NCycSysAvailMgrData%Name,NumNCycSysAvailMgrs)
      ENDIF
      IF (SysAvailNum > 0) THEN
        CALL CalcNCycSysAvailMgr(SysAvailNum,PriAirSysNum,AvailStatus, ZoneEquipType, CompNum)
      ELSE
        CALL ShowFatalError('SimSysAvailManager: AvailabilityManager:NightCycle not found: '//TRIM(SysAvailName))
      ENDIF

    CASE(SysAvailMgr_OptimumStart) ! 'AvailabilityManager:OptimumStart'
      IF (SysAvailNum == 0) THEN
        SysAvailNum = FindItemInList(SysAvailName,OptStartSysAvailMgrData%Name,NumOptStartSysAvailMgrs)
      ENDIF
      IF (SysAvailNum > 0) THEN
        CALL CalcOptStartSysAvailMgr(SysAvailNum,PriAirSysNum,AvailStatus, ZoneEquipType, CompNum)
      ELSE
        CALL ShowFatalError('SimSysAvailManager: AvailabilityManager:OptimumStart not found: '//TRIM(SysAvailName))
      ENDIF

    CASE(SysAvailMgr_NightVent) ! 'AvailabilityManager:NightVentilation'
      IF (SysAvailNum == 0) THEN
        SysAvailNum = FindItemInList(SysAvailName,NVentSysAvailMgrData%Name,NumNVentSysAvailMgrs)
      ENDIF
      IF (SysAvailNum > 0) THEN
        CALL CalcNVentSysAvailMgr(SysAvailNum,PriAirSysNum,AvailStatus,ZoneEquipType)
      ELSE
        CALL ShowFatalError('SimSysAvailManager: AvailabilityManager:NightVentilation not found: '//TRIM(SysAvailName))
      ENDIF

    CASE(SysAvailMgr_DiffThermo) ! 'AvailabilityManager:DifferentialThermostat'
      IF (SysAvailNum == 0) THEN
        SysAvailNum = FindItemInList(SysAvailName,DiffTSysAvailMgrData%Name,NumDiffTSysAvailMgrs)
      ENDIF
      IF (SysAvailNum > 0) THEN
        CALL CalcDiffTSysAvailMgr(SysAvailNum,PreviousStatus,AvailStatus)
      ELSE
        CALL ShowFatalError('SimSysAvailManager: AvailabilityManager:DifferentialThermostat not found: '//TRIM(SysAvailName))
      ENDIF

    CASE(SysAvailMgr_HiTempTOff)  ! 'AvailabilityManager:HighTemperatureTurnOff'
      IF (SysAvailNum == 0) THEN
        SysAvailNum = FindItemInList(SysAvailName,HiTurnOffSysAvailMgrData%Name,NumHiTurnOffSysAvailMgrs)
      ENDIF
      IF (SysAvailNum > 0) THEN
        CALL CalcHiTurnOffSysAvailMgr(SysAvailNum,AvailStatus)
      ELSE
        CALL ShowFatalError('SimSysAvailManager: AvailabilityManager:HighTemperatureTurnOff not found: '//TRIM(SysAvailName))
      ENDIF

    CASE(SysAvailMgr_HiTempTOn)  ! 'AvailabilityManager:HighTemperatureTurnOn'
      IF (SysAvailNum == 0) THEN
        SysAvailNum = FindItemInList(SysAvailName,HiTurnOnSysAvailMgrData%Name,NumHiTurnOnSysAvailMgrs)
      ENDIF
      IF (SysAvailNum > 0) THEN
        CALL CalcHiTurnOnSysAvailMgr(SysAvailNum,AvailStatus)
      ELSE
        CALL ShowFatalError('SimSysAvailManager: AvailabilityManager:HighTemperatureTurnOn not found: '//TRIM(SysAvailName))
      ENDIF

    CASE(SysAvailMgr_LoTempTOff)  ! 'AvailabilityManager:LowTemperatureTurnOff'
      IF (SysAvailNum == 0) THEN
        SysAvailNum = FindItemInList(SysAvailName,LoTurnOffSysAvailMgrData%Name,NumLoTurnOffSysAvailMgrs)
      ENDIF
      IF (SysAvailNum > 0) THEN
        CALL CalcLoTurnOffSysAvailMgr(SysAvailNum,AvailStatus)
      ELSE
        CALL ShowFatalError('SimSysAvailManager: AvailabilityManager:LowTemperatureTurnOff not found: '//TRIM(SysAvailName))
      ENDIF

    CASE(SysAvailMgr_LoTempTOn)  ! 'AvailabilityManager:LowTemperatureTurnOn'
      IF (SysAvailNum == 0) THEN
        SysAvailNum = FindItemInList(SysAvailName,LoTurnOnSysAvailMgrData%Name,NumLoTurnOnSysAvailMgrs)
      ENDIF
      IF (SysAvailNum > 0) THEN
        CALL CalcLoTurnOnSysAvailMgr(SysAvailNum,AvailStatus)
      ELSE
        CALL ShowFatalError('SimSysAvailManager: AvailabilityManager:LowTemperatureTurnOn not found: '//TRIM(SysAvailName))
      ENDIF

    CASE DEFAULT
      CALL ShowSevereError('AvailabilityManager Type not found: '//TRIM(TrimSigDigits(SysAvailType)))
      CALL ShowContinueError('Occurs in Manager='//TRIM(SysAvailName))
      CALL ShowFatalError('Preceding condition causes termination.')

  END SELECT

  RETURN

END SUBROUTINE SimSysAvailManager

SUBROUTINE CalcSchedSysAvailMgr(SysAvailNum,AvailStatus)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   August 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set AvailStatus indicator for a primary air loop, plant loop or ZoneHVAC component.

          ! METHODOLOGY EMPLOYED:
          ! Looks at the System Availability Manager schedule and sets the
          ! AvailStatus indicator accordingly. Mostly a useless algorithm
          ! since the fan schedules can do the same thing.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SysAvailNum         ! number of the current scheduled system availability manager
  INTEGER, INTENT (OUT) :: AvailStatus         ! System status indicator

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:


          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  IF (GetCurrentScheduleValue(SchedSysAvailMgrData(SysAvailNum)%SchedPtr) .GT. 0.0d0) THEN
    AvailStatus = CycleOn
  ELSE
    AvailStatus = ForceOff
  END IF

  SchedSysAvailMgrData(SysAvailNum)%AvailStatus = AvailStatus

  RETURN

END SUBROUTINE CalcSchedSysAvailMgr

SUBROUTINE CalcSchedOnSysAvailMgr(SysAvailNum,AvailStatus)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         R. Raustad - FSEC
          !       DATE WRITTEN   August 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set AvailStatus indicator for a primary air loop, plant loop or ZoneHVAC component.

          ! METHODOLOGY EMPLOYED:
          ! Looks at the System Availability Manager schedule and sets the
          ! AvailStatus indicator accordingly. If the schedule value is > 0
          ! the availability status is CycleOn, ELSE the status is NoAction.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SysAvailNum         ! number of the current scheduled on system availability manager
  INTEGER, INTENT (OUT) :: AvailStatus         ! System status indicator

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:


          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  IF (GetCurrentScheduleValue(SchedOnSysAvailMgrData(SysAvailNum)%SchedPtr) .GT. 0.0d0) THEN
    AvailStatus = CycleOn
  ELSE
    AvailStatus = NoAction
  END IF

  SchedOnSysAvailMgrData(SysAvailNum)%AvailStatus = AvailStatus

  RETURN

END SUBROUTINE CalcSchedOnSysAvailMgr

SUBROUTINE CalcSchedOffSysAvailMgr(SysAvailNum,AvailStatus)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         R. Raustad - FSEC
          !       DATE WRITTEN   August 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set AvailStatus indicator for a primary air loop, plant loop or ZoneHVAC component.

          ! METHODOLOGY EMPLOYED:
          ! Looks at the System Availability Manager schedule and sets the
          ! AvailStatus indicator accordingly.  If the schedule value is = 0
          ! the availability status is ForceOff, ELSE the status is NoAction.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SysAvailNum         ! number of the current scheduled off system availability manager
  INTEGER, INTENT (OUT) :: AvailStatus         ! System status indicator

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:


          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  IF (GetCurrentScheduleValue(SchedOffSysAvailMgrData(SysAvailNum)%SchedPtr) .EQ. 0.0d0) THEN
    AvailStatus = ForceOff
  ELSE
    AvailStatus = NoAction
  END IF

  SchedOffSysAvailMgrData(SysAvailNum)%AvailStatus = AvailStatus

  RETURN

END SUBROUTINE CalcSchedOffSysAvailMgr

SUBROUTINE CalcNCycSysAvailMgr(SysAvailNum,PriAirSysNum,AvailStatus,ZoneEquipType, CompNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   August 2001
          !       MODIFIED       March 2011, Chandan Sharma - FSEC: Allowed night cycle
          !                             availability manager to work for ZoneHVAC component
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set AvailStatus indicator for a primary air loop or ZoneHVAC component.

          ! METHODOLOGY EMPLOYED:
          ! For air loop, depending on the type of control, looks at 1 named zone or all the zones
          ! attached to a primary air system, compares zone temperature to the setup
          ! or setback thermostat setpoint, and sets the AvailStaus indicator according
          ! to whether the system needs to be cycled on or not.
          ! For ZoneHVAC component, uses the exact same method as above but only looks at the
          ! zone where component is located.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataAirLoop
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE DataHeatBalFanSys, ONLY: TempZoneThermostatSetpoint, ZoneThermostatSetPointHi, &
                               ZoneThermostatSetPointLo, TempControlType, TempTstatAir


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SysAvailNum         ! number of the current scheduled system availability manager
  INTEGER, INTENT (IN)  :: PriAirSysNum        ! number of the primary air system affected by this Avail. Manager
  INTEGER, INTENT (OUT) :: AvailStatus         ! System status indicator
  INTEGER,  OPTIONAL,  INTENT(IN)  :: ZoneEquipType  ! Type of ZoneHVAC equipment component
  INTEGER,  OPTIONAL,  INTENT(IN)  :: CompNum    ! Index of ZoneHVAC equipment component
          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: StartTime
  INTEGER :: StopTime
  INTEGER :: ZoneInSysNum
  INTEGER :: CtrldZoneNum
  INTEGER :: ZoneNum
  REAL(r64)    :: TempTol
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: ZoneCompNCControlType
  LOGICAL, SAVE :: OneTimeFlag = .TRUE.

  TempTol = 0.5d0*NCycSysAvailMgrData(SysAvailNum)%TempTolRange
  IF (PRESENT(ZoneEquipType)) THEN
    StartTime = ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%StartTime
    StopTime  = ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%StopTime
    IF (OneTimeFlag) THEN
      ALLOCATE(ZoneCompNCControlType(NumNCycSysAvailMgrs))
      ZoneCompNCControlType = .TRUE.
      OneTimeFlag = .FALSE.
    ENDIF
  ELSE
    StartTime = PriAirSysAvailMgr(PriAirSysNum)%StartTime
    StopTime = PriAirSysAvailMgr(PriAirSysNum)%StopTime
  ENDIF
! CR 7913 changed to allow during warmup
  IF ( (GetCurrentScheduleValue(NCycSysAvailMgrData(SysAvailNum)%SchedPtr) <= 0.0d0) .OR. &
       (GetCurrentScheduleValue(NCycSysAvailMgrData(SysAvailNum)%FanSchedPtr) > 0.0d0) )  THEN
    AvailStatus = NoAction
    NCycSysAvailMgrData(SysAvailNum)%AvailStatus = AvailStatus   ! CR 8358
    RETURN
  END IF

  IF (PRESENT(ZoneEquipType)) THEN
    IF ( SimTimeSteps >= StartTime .AND. SimTimeSteps < StopTime ) THEN ! if cycled on
      AvailStatus = CycleOn
    ELSE IF ( SimTimeSteps == StopTime ) THEN ! if end of cycle run time, shut down if fan off
      AvailStatus = NoAction
    ELSE

      SELECT CASE(NCycSysAvailMgrData(SysAvailNum)%CtrlType) ! select type of night cycle control

        CASE(StayOff)
          AvailStatus = NoAction

        CASE(CycleOnControlZone)

          ZoneNum = NCycSysAvailMgrData(SysAvailNum)%ZoneNum

          SELECT CASE(TempControlType(ZoneNum)) ! select on thermostat control

            CASE(SingleHeatingSetPoint)
              IF (TempTstatAir(ZoneNum) < TempZoneThermostatSetpoint(ZoneNum) - TempTol) THEN
                AvailStatus = CycleOn
              ELSE
                AvailStatus = NoAction
              END IF

            CASE(SingleCoolingSetPoint)
              IF (TempTstatAir(ZoneNum) > TempZoneThermostatSetpoint(ZoneNum) + TempTol) THEN
                AvailStatus = CycleOn
              ELSE
                AvailStatus = NoAction
              END IF

            CASE(SingleHeatCoolSetPoint)
              IF ( (TempTstatAir(ZoneNum) < TempZoneThermostatSetpoint(ZoneNum) - TempTol) .OR. &
                   (TempTstatAir(ZoneNum) > TempZoneThermostatSetpoint(ZoneNum) + TempTol) ) THEN

                AvailStatus = CycleOn
              ELSE
                AvailStatus = NoAction
              END IF

            CASE(DualSetPointWithDeadBand)
              IF ( (TempTstatAir(ZoneNum) < ZoneThermostatSetPointLo(ZoneNum) - TempTol) .OR. &
                   (TempTstatAir(ZoneNum) > ZoneThermostatSetPointHi(ZoneNum) + TempTol) ) THEN
                AvailStatus = CycleOn
              ELSE
                AvailStatus = NoAction
              END IF

            CASE DEFAULT
              AvailStatus = NoAction

          END SELECT ! end select on thermostat control

        CASE(CycleOnAny, ZoneFansOnly)
          IF (ZoneCompNCControlType(SysAvailNum)) THEN
            CALL ShowWarningError('AvailabilityManager:NightCycle = '//TRIM(NCycSysAvailMgrData(SysAvailNum)%Name)// &
                                 ', is specified for a ZoneHVAC component.')
            CALL ShowContinueError('The only valid Control Types for ZoneHVAC components are CycleOnControlZone and StayOff.')
            CALL ShowContinueError('Night Cycle operation will not be modeled for ZoneHVAC components that reference this '// &
                                   'manager.')
            ZoneCompNCControlType(SysAvailNum) = .FALSE.
          ENDIF
          AvailStatus = NoAction

        CASE DEFAULT
          AvailStatus = NoAction

      END SELECT ! end select type of night cycle control

      IF (AvailStatus == CycleOn) THEN ! reset the start and stop times
        ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%StartTime = SimTimeSteps
        ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs(CompNum)%StopTime = SimTimeSteps + &
                                          NCycSysAvailMgrData(SysAvailNum)%CyclingTimeSteps
      END IF

    END IF
  ELSE
    IF ( SimTimeSteps >= StartTime .AND. SimTimeSteps < StopTime ) THEN ! if cycled on
      AvailStatus = CycleOn
      IF (NCycSysAvailMgrData(SysAvailNum)%CtrlType == ZoneFansOnly) AvailStatus = CycleOnZoneFansOnly
    ELSE IF ( SimTimeSteps == StopTime ) THEN ! if end of cycle run time, shut down if fan off
      AvailStatus = NoAction
    ELSE

      SELECT CASE(NCycSysAvailMgrData(SysAvailNum)%CtrlType) ! select type of night cycle control

        CASE(StayOff)
          AvailStatus = NoAction

        CASE(CycleOnAny, ZoneFansOnly)

          ! If no zones cooled, Availstatus could be "unknown"
          AvailStatus = NoAction

          DO ZoneInSysNum=1,AirToZoneNodeInfo(PriAirSysNum)%NumZonesCooled ! loop over zones in system

            CtrldZoneNum = AirToZoneNodeInfo(PriAirSysNum)%CoolCtrlZoneNums(ZoneInSysNum)
            ZoneNum = ZoneEquipConfig(CtrldZoneNum)%ActualZoneNum

            SELECT CASE(TempControlType(ZoneNum)) ! select on thermostat control

              CASE(SingleHeatingSetPoint)
                IF (TempTstatAir(ZoneNum) < TempZoneThermostatSetpoint(ZoneNum) - TempTol) THEN
                  AvailStatus = CycleOn
                  EXIT
                ELSE
                  AvailStatus = NoAction
                END IF

              CASE(SingleCoolingSetPoint)
                IF (TempTstatAir(ZoneNum) > TempZoneThermostatSetpoint(ZoneNum) + TempTol) THEN
                  AvailStatus = CycleOn
                  EXIT
                ELSE
                  AvailStatus = NoAction
                END IF

              CASE(SingleHeatCoolSetPoint)
                IF ( (TempTstatAir(ZoneNum) < TempZoneThermostatSetpoint(ZoneNum) - TempTol) .OR. &
                     (TempTstatAir(ZoneNum) > TempZoneThermostatSetpoint(ZoneNum) + TempTol) ) THEN
                  AvailStatus = CycleOn
                  EXIT
                ELSE
                  AvailStatus = NoAction
                END IF

              CASE(DualSetPointWithDeadBand)
                IF ( (TempTstatAir(ZoneNum) < ZoneThermostatSetPointLo(ZoneNum) - TempTol) .OR. &
                     (TempTstatAir(ZoneNum) > ZoneThermostatSetPointHi(ZoneNum) + TempTol) ) THEN
                  AvailStatus = CycleOn
                  EXIT
                ELSE
                  AvailStatus = NoAction
                END IF

              CASE DEFAULT
                AvailStatus = NoAction

            END SELECT ! end select on thermostat control

          END DO ! end loop over zones in system

        CASE(CycleOnControlZone)

          ZoneNum = NCycSysAvailMgrData(SysAvailNum)%ZoneNum

          SELECT CASE(TempControlType(ZoneNum)) ! select on thermostat control

            CASE(SingleHeatingSetPoint)
              IF (TempTstatAir(ZoneNum) < TempZoneThermostatSetpoint(ZoneNum) - TempTol) THEN
                AvailStatus = CycleOn
              ELSE
                AvailStatus = NoAction
              END IF

            CASE(SingleCoolingSetPoint)
              IF (TempTstatAir(ZoneNum) > TempZoneThermostatSetpoint(ZoneNum) + TempTol) THEN
                AvailStatus = CycleOn
              ELSE
                AvailStatus = NoAction
              END IF

            CASE(SingleHeatCoolSetPoint)
              IF ( (TempTstatAir(ZoneNum) < TempZoneThermostatSetpoint(ZoneNum) - TempTol) .OR. &
                   (TempTstatAir(ZoneNum) > TempZoneThermostatSetpoint(ZoneNum) + TempTol) ) THEN

                AvailStatus = CycleOn
              ELSE
                AvailStatus = NoAction
              END IF

            CASE(DualSetPointWithDeadBand)
              IF ( (TempTstatAir(ZoneNum) < ZoneThermostatSetPointLo(ZoneNum) - TempTol) .OR. &
                   (TempTstatAir(ZoneNum) > ZoneThermostatSetPointHi(ZoneNum) + TempTol) ) THEN
                AvailStatus = CycleOn
              ELSE
                AvailStatus = NoAction
              END IF

            CASE DEFAULT
              AvailStatus = NoAction

          END SELECT ! end select on thermostat control

        CASE DEFAULT
          AvailStatus = NoAction

      END SELECT ! end select type of night cycle control

      IF (AvailStatus == CycleOn) THEN ! reset the start and stop times
        PriAirSysAvailMgr(PriAirSysNum)%StartTime = SimTimeSteps
        PriAirSysAvailMgr(PriAirSysNum)%StopTime = SimTimeSteps + NCycSysAvailMgrData(SysAvailNum)%CyclingTimeSteps
        IF (NCycSysAvailMgrData(SysAvailNum)%CtrlType == ZoneFansOnly) AvailStatus = CycleOnZoneFansOnly
      END IF

    END IF
  ENDIF
  NCycSysAvailMgrData(SysAvailNum)%AvailStatus = AvailStatus

  RETURN

END SUBROUTINE CalcNCycSysAvailMgr

SUBROUTINE CalcOptStartSysAvailMgr(SysAvailNum,PriAirSysNum,AvailStatus,ZoneEquipType, CompNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR            Xiufeng Pang (XP)
          !       DATE WRITTEN      August 2013
          !       MODIFIED
          !
          !       RE-ENGINEERED

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set AvailStatus indicator for a primary air loop, plant loop or ZoneHVAC component

          ! METHODOLOGY EMPLOYED:
          ! Sets the AvailStatus indicator according to the
          ! optimum start algorithm

          ! REFERENCES:
          !

          ! USE STATEMENTS:
  USE DataAirLoop
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE DataHeatBalFanSys, ONLY: TempZoneThermostatSetpoint, ZoneThermostatSetPointHi, &
                               ZoneThermostatSetPointLo, TempControlType, TempTstatAir
  USE DataEnvironment, ONLY:   DSTIndicator, DayOfYear, DayOfWeekTomorrow, DayOfWeek
  USE DataZoneControls, ONLY: OccRoomTSetPointHeat, OccRoomTSetPointCool


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SysAvailNum         ! number of the current scheduled system availability manager
  INTEGER, INTENT (IN)  :: PriAirSysNum        ! number of the primary air system affected by this Avail. Manager
  INTEGER, INTENT (OUT) :: AvailStatus         ! System status indicator
  INTEGER,  OPTIONAL,  INTENT(IN)  :: ZoneEquipType  ! Type of ZoneHVAC equipment component
  INTEGER,  OPTIONAL,  INTENT(IN)  :: CompNum    ! Index of ZoneHVAC equipment component
          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER :: ScheduleIndex
  INTEGER :: DayScheduleIndex
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: DayValues
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: DayValuesTmr
  INTEGER :: JDay
  INTEGER :: TmrJDay
  INTEGER :: CurDayofWeek
  INTEGER :: TmrDayOfWeek
  INTEGER :: ZoneNum
  REAL(r64)    :: FanStartTime
  REAL(r64)    :: FanStartTimeTmr
  REAL(r64)    :: PreStartTime
  REAL(r64)    :: PreStartTimeTmr
  REAL(r64)    :: DeltaTime
  INTEGER      :: I
  INTEGER      :: J
  REAL(r64)    :: TempDiff
  REAL(r64), SAVE    :: TempDiffHi = 0.0d0
  REAL(r64), SAVE    :: TempDiffLo = 0.0d0
!  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: ZoneCompOptStartControlType
  LOGICAL       :: FirstTimeATGFlag = .TRUE.
  LOGICAL       :: OverNightStartFlag = .False. ! Flag to indicate the optimum start starts before mid night.
  LOGICAL       :: CycleOnFlag = .False.
  LOGICAL       :: OSReportVarFlag = .True.
  INTEGER       :: NumPreDays
  INTEGER       :: NumOfZonesInList
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: AdaTempGradTrdHeat !Heating temp gradient for previous days
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: AdaTempGradTrdCool !Cooling temp gradient for previous days
  REAL(r64), SAVE    :: AdaTempGradHeat
  REAL(r64), SAVE    :: AdaTempGradCool
  REAL(r64)    :: ATGUpdateTime1 = 0.0d0
  REAL(r64)    :: ATGUpdateTime2 = 0.0d0
  REAL(r64)    :: ATGUpdateTemp1 = 0.0d0
  REAL(r64)    :: ATGUpdateTemp2 = 0.0d0
  LOGICAL      :: ATGUpdateFlag1 = .False.
  LOGICAL      :: ATGUpdateFlag2 = .False.
  INTEGER       :: ATGCounter
  INTEGER       :: ATGWCZoneNumHi
  INTEGER       :: ATGWCZoneNumLo
  REAL(r64)    :: NumHoursBeforeOccupancy = 0.0d0 !Variable to store the number of hours before occupancy in optimum start period

! add or use a new variable OptStartSysAvailMgrData(SysAvailNum)%FanSchIndex
  IF (KickOffSimulation) THEN
      AvailStatus = NoAction
  ELSE
  ScheduleIndex=GetScheduleIndex(OptStartSysAvailMgrData(SysAvailNum)%FanSched)
  JDay = DayOfYear
  TmrJDay = JDay + 1
  TmrDayOfWeek = DayOfWeekTomorrow

  ALLOCATE (DayValues(24,NumOfTimeStepInHour))
  ALLOCATE (DayValuesTmr(24,NumOfTimeStepInHour))
  IF (.not.allocated(OptStartData%OptStartFlag)) THEN
  ALLOCATE (OptStartData%OptStartFlag(NumOfZones))
  ALLOCATE (OptStartData%OccStartTime(NumOfZones))
  END IF
  IF (.NOT.ALLOCATED(OptStartData%ActualZoneNum)) ALLOCATE(OptStartData%ActualZoneNum(NumOfZones))
  OptStartData%OptStartFlag = .FALSE.
  OptStartData%OccStartTime = 99.99d0   !initialize the zone occupancy start time
  Call GetScheduleValuesForDay(ScheduleIndex,DayValues)
  Call GetScheduleValuesForDay(ScheduleIndex,DayValuesTmr, TmrJDay, TmrDayOfWeek)

  FanStartTime = 0.0d0
  FanStartTimeTmr = 0.0d0
Loop1:  DO I = 1, 24
Loop2:    DO J = 1, NumOfTimeStepInHour
        IF (DayValues(I,J)> 0.0d0) THEN
          FanStartTime = I-1 + 1/NumOfTimeStepInHour * J
          EXIT Loop1
        ENDIF
    END DO Loop2
END DO Loop1

Loop3:  DO I = 1, 24
Loop4:    DO J = 1, NumOfTimeStepInHour
        IF (DayValuesTmr(I,J)> 0.0d0) THEN
          FanStartTimeTmr = I-1 + 1/NumOfTimeStepInHour * J
          EXIT Loop3
        ENDIF
    END DO Loop4
END DO Loop3

IF (FanStartTimeTmr == 0.0d0) FanStartTimeTmr = 24.0d0

! Pass the start time to ZoneTempPredictorCorrector
DO I=1, NumOfZones
  If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
    OptStartData%OccStartTime(ZoneEquipConfig(I)%ActualZoneNum)=FanStartTime
    OptStartData%ActualZoneNum(ZoneEquipConfig(I)%ActualZoneNum)=ZoneEquipConfig(I)%ActualZoneNum
  END IF
END DO

If (DSTIndicator>0) then
  FanStartTime = FanStartTime - 1.d0
  FanStartTimeTmr = FanStartTimeTmr - 1.d0
End If

IF (BeginDayFlag) THEN
  NumHoursBeforeOccupancy = 0.0 !Initialize the hours of optimum start period. This variable is for reporting purpose.
END IF

SELECT CASE(OptStartSysAvailMgrData(SysAvailNum)%CtrlAlgType)
CASE(ConstantStartTime)
  IF (OptStartSysAvailMgrData(SysAvailNum)%CtrlType == StayOff) THEN
    AvailStatus = NoAction
  ELSE
    DeltaTime = OptStartSysAvailMgrData(SysAvailNum)%ConstStartTime
    IF (DeltaTime>OptStartSysAvailMgrData(SysAvailNum)%MaxOptStartTime) THEN
      DeltaTime=OptStartSysAvailMgrData(SysAvailNum)%MaxOptStartTime
    END IF
    PreStartTime = FanStartTime - DeltaTime
    if (PreStartTime<0.0d0) PreStartTime = -.1d0
    PreStartTimeTmr = FanStartTimeTmr - DeltaTime
    If (PreStartTimeTmr<0.0d0) then
      PreStartTimeTmr = PreStartTimeTmr + 24.0d0
      OverNightStartFlag = .True.
    else
      OverNightStartFlag = .False.
    end if
    IF (.NOT. OverNightStartFlag) THEN
      IF (FanStartTime == 0.0d0 .OR. PreviousHour .GT. FanStartTime) THEN
        AvailStatus = NoAction
        OSReportVarFlag = .True.
      ELSE IF (PreStartTime .LT. CurrentTime) THEN
        IF (OSReportVarFlag) THEN
          NumHoursBeforeOccupancy=DeltaTime
          OSReportVarFlag = .False.
        END IF
          AvailStatus = CycleOn
          DO I=1, NumOfZones
            If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
              OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
            END IF
          END DO
      ELSE
        AvailStatus = NoAction
        OSReportVarFlag = .True.
      END IF
    ELSE
      IF (FanStartTime == 0.0d0 .OR. (HourOfDay .GT. FanStartTime .AND. CurrentTime .LE. PreStartTimeTmr)) THEN
        AvailStatus = NoAction
        OSReportVarFlag = .True.
      ELSE IF (PreStartTime .LT. CurrentTime .OR. PreStartTimeTmr .LT. CurrentTime) THEN
        IF (OSReportVarFlag) THEN
          NumHoursBeforeOccupancy=DeltaTime
          OSReportVarFlag = .False.
        END IF
        AvailStatus = CycleOn
        DO I=1, NumOfZones
          If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
            OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
          END IF
        END DO
      ELSE
        AvailStatus = NoAction
        OSReportVarFlag = .True.
      END IF
    END IF
  END IF

CASE(ConstantTemperatureGradient)
  IF (OptStartSysAvailMgrData(SysAvailNum)%CtrlType == ControlZone) THEN
    ZoneNum = OptStartSysAvailMgrData(SysAvailNum)%ZoneNum
    IF ((.Not.allocated(TempTstatAir)) .OR. (.Not.allocated(ZoneThermostatSetPointLo)) .OR. &
      (.Not.allocated(ZoneThermostatSetPointHi))) THEN
      TempDiff = 0.0d0
    ELSE
      IF (.NOT. CycleOnFlag) THEN
        IF (ALLOCATED(OccRoomTSetPointHeat) .AND. ALLOCATED(OccRoomTSetPointCool)) THEN
          TempDiffHi = TempTstatAir(ZoneNum) - OccRoomTSetPointCool(ZoneNum)
          TempDiffLo = TempTstatAir(ZoneNum) - OccRoomTSetPointHeat(ZoneNum)
        ELSE
          TempDiffHi = 0.0d0
          TempDiffLo = 0.0d0
        END IF
      END IF
    END IF

    IF (TempDiffHi<0.0d0) THEN
      TempDiff = TempDiffLo
      IF (TempDiff .LT. 0.0d0) THEN !Heating Mode
        TempDiff = ABS(TempDiff)
        DeltaTime = TempDiff/OptStartSysAvailMgrData(SysAvailNum)%ConstTGradHeat
        IF (DeltaTime>OptStartSysAvailMgrData(SysAvailNum)%MaxOptStartTime) THEN
          DeltaTime=OptStartSysAvailMgrData(SysAvailNum)%MaxOptStartTime
        END IF
        PreStartTime = FanStartTime - DeltaTime
        IF (PreStartTime<0) PreStartTime = -.1d0
        PreStartTimeTmr = FanStartTimeTmr - DeltaTime
        IF (PreStartTimeTmr<0) THEN
          PreStartTimeTmr = PreStartTimeTmr + 24.0d0
          OverNightStartFlag = .True.
        ELSE
          OverNightStartFlag = .False.
        END IF
        IF (.NOT. OverNightStartFlag) THEN
          IF (FanStartTime == 0.0d0 .OR. CurrentTime .GT. FanStartTime) THEN
              CycleOnFlag = .False.
              OSReportVarFlag = .True.
            ELSE IF (CyCleOnFlag) THEN
              AvailStatus = CycleOn
              DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
              END DO
              IF (CurrentTime .GT. FanStartTime) CycleOnFlag = .False.
            ELSE IF (PreStartTime .LT. CurrentTime) THEN
              AvailStatus = CycleOn
              CycleOnFlag = .True.
              IF (OSReportVarFlag) THEN
                NumHoursBeforeOccupancy=DeltaTime
                OSReportVarFlag = .False.
              END IF
              DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
              END DO
            ELSE
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
            END IF
          ELSE
          IF (FanStartTime == 0.0d0 .OR. (CurrentTime .GT. FanStartTime .AND. CurrentTime .LE. PreStartTimeTmr)) THEN
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
          ELSE IF (CycleOnFlag) THEN
            AvailStatus = CycleOn
            DO I=1, NumOfZones
              If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
              END IF
            END DO
            IF (CurrentTime .GT. FanStartTime .AND. CurrentTime .LT. PreStartTimeTmr) CycleOnFlag=.False.
          ELSE IF (PreStartTime .LT. CurrentTime .OR. PreStartTimeTmr .LT. CurrentTime) THEN
            IF (OSReportVarFlag) THEN
              NumHoursBeforeOccupancy=DeltaTime
              OSReportVarFlag = .False.
            END IF
            AvailStatus = CycleOn
            CycleOnFlag = .True.
            DO I=1, NumOfZones
              If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
              END IF
            END DO
          ELSE
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
          END IF
          END IF
        ELSE
          AvailStatus = NoAction
          CycleOnFlag = .False.
        END IF
      ELSE IF (OccRoomTSetPointCool(ZoneNum) .LT. 50.0d0) THEN ! Cooling Mode
          TempDiff = TempDiffHi
          DeltaTime = TempDiff/OptStartSysAvailMgrData(SysAvailNum)%ConstTGradCool
          IF (DeltaTime>OptStartSysAvailMgrData(SysAvailNum)%MaxOptStartTime) THEN
            DeltaTime=OptStartSysAvailMgrData(SysAvailNum)%MaxOptStartTime
          END IF
          PreStartTime = FanStartTime - DeltaTime
          IF (PreStartTime<0) PreStartTime = -.1d0
          PreStartTimeTmr = FanStartTimeTmr - DeltaTime
          IF (PreStartTimeTmr<0) THEN
            PreStartTimeTmr = PreStartTimeTmr + 24.0d0
            OverNightStartFlag = .True.
          ELSE
            OverNightStartFlag = .False.
          END IF
          IF (.NOT. OverNightStartFlag) THEN
            IF (FanStartTime == 0.0d0 .OR. CurrentTime .GT. FanStartTime) THEN
              AvailStatus = NoAction
              CycleOnFlag = .False.
              OSReportVarFlag = .True.
            ELSE IF (CyCleOnFlag) THEN
              AvailStatus = CycleOn
              DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
              END DO
            ELSE IF (PreStartTime .LT. CurrentTime) THEN
              IF (OSReportVarFlag) THEN
                NumHoursBeforeOccupancy=DeltaTime
                OSReportVarFlag = .False.
              END IF
              AvailStatus = CycleOn
              CycleOnFlag = .True.
              DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
              END DO
            ELSE
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
            END IF
          ELSE
          IF (FanStartTime == 0.0d0 .OR. (CurrentTime .GT. FanStartTime .AND. CurrentTime .LE. PreStartTimeTmr)) THEN
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
          ELSE IF (CycleOnFlag) THEN
            AvailStatus = CycleOn
            DO I=1, NumOfZones
              If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
              END IF
            END DO
          ELSE IF (PreStartTime .LT. CurrentTime .OR. PreStartTimeTmr .LT. CurrentTime) THEN
            IF (OSReportVarFlag) THEN
              NumHoursBeforeOccupancy=DeltaTime
              OSReportVarFlag = .False.
            END IF
            AvailStatus = CycleOn
            CycleOnFlag = .True.
            DO I=1, NumOfZones
              If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
              END IF
            END DO
          ELSE
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
          END IF
        END IF
      ELSE
        AvailStatus = NoAction
        CycleOnFlag = .False.
      END IF
  ELSE IF (OptStartSysAvailMgrData(SysAvailNum)%CtrlType == MaximumOfZoneList) THEN
    NumOfZonesInList = OptStartSysAvailMgrData(SysAvailNum)%NumOfZones
    IF ((.Not.allocated(TempTstatAir)) .OR. (.Not.allocated(ZoneThermostatSetPointLo)) .OR. &
      (.Not.allocated(ZoneThermostatSetPointHi))) THEN
      TempDiff = 0.0d0
    ELSE
      IF (.NOT. CycleOnFlag) THEN
        IF (ALLOCATED(OccRoomTSetPointHeat) .AND. ALLOCATED(OccRoomTSetPointCool)) THEN
          TempDiffHi = 0.0d0
          TempDiffLo = 0.0d0
          DO ZoneNum=1, NumOfZonesInList
            TempDiff=TempTstatAir(OptStartSysAvailMgrData(SysAvailNum)%ZonePtrs(ZoneNum)) - &
                     OccRoomTSetPointCool(OptStartSysAvailMgrData(SysAvailNum)%ZonePtrs(ZoneNum))
            TempDiffHi = Max(TempDiffHi, TempDiff)
            TempDiff=TempTstatAir(OptStartSysAvailMgrData(SysAvailNum)%ZonePtrs(ZoneNum)) - &
                     OccRoomTSetPointHeat(OptStartSysAvailMgrData(SysAvailNum)%ZonePtrs(ZoneNum))
            TempDiffLo = Min(TempDiffLo, TempDiff)
          END DO
        ELSE
          TempDiffHi = 0.0d0
          TempDiffLo = 0.0d0
        END IF
      END IF
    END IF
      IF ((TempDiffHi<0.0d0 .AND. TempDiffLo<0.0d0) .OR. (abs(TempDiffLo)>abs(TempDiffHi) &
        .AND. TempDiffLo<0)) THEN !Heating Mode
        TempDiff = TempDiffLo
        TempDiff = ABS(TempDiff)
        DeltaTime = TempDiff/OptStartSysAvailMgrData(SysAvailNum)%ConstTGradHeat
        IF (DeltaTime>OptStartSysAvailMgrData(SysAvailNum)%MaxOptStartTime) THEN
          DeltaTime=OptStartSysAvailMgrData(SysAvailNum)%MaxOptStartTime
        END IF
        PreStartTime = FanStartTime - DeltaTime
        IF (PreStartTime<0) PreStartTime = -.1d0
        PreStartTimeTmr = FanStartTimeTmr - DeltaTime
        IF (PreStartTimeTmr<0) THEN
          PreStartTimeTmr = PreStartTimeTmr + 24.0d0
          OverNightStartFlag = .True.
        ELSE
          OverNightStartFlag = .False.
        END IF
        IF (.NOT. OverNightStartFlag) THEN
          IF (FanStartTime == 0.0d0 .OR. CurrentTime .GT. FanStartTime) THEN
              AvailStatus = NoAction
              CycleOnFlag = .False.
              OSReportVarFlag = .True.
            ELSE IF (CyCleOnFlag) THEN
              AvailStatus = CycleOn
              DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
              END DO
              IF (CurrentTime .GT. FanStartTime) CycleOnFlag = .False.
            ELSE IF (PreStartTime .LT. CurrentTime) THEN
              IF (OSReportVarFlag) THEN
                NumHoursBeforeOccupancy=DeltaTime
                OSReportVarFlag = .False.
              END IF
              AvailStatus = CycleOn
              CycleOnFlag = .True.
              DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
              END DO
            ELSE
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
            END IF
          ELSE
          IF (FanStartTime == 0.0d0 .OR. (CurrentTime .GT. FanStartTime .AND. CurrentTime .LE. PreStartTimeTmr)) THEN
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
          ELSE IF (CycleOnFlag) THEN
            AvailStatus = CycleOn
            DO I=1, NumOfZones
              If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
              END IF
            END DO
            IF (CurrentTime .GT. FanStartTime .AND. CurrentTime .LT. PreStartTimeTmr) CycleOnFlag=.False.
          ELSE IF (PreStartTime .LT. CurrentTime .OR. PreStartTimeTmr .LT. CurrentTime) THEN
            IF (OSReportVarFlag) THEN
              NumHoursBeforeOccupancy=DeltaTime
              OSReportVarFlag = .False.
            END IF
            AvailStatus = CycleOn
            CycleOnFlag = .True.
            DO I=1, NumOfZones
              If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
              END IF
            END DO
          ELSE
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
          END IF
        END IF
      ELSE IF (TempDiffHi<=0.0d0 .AND. TempDiffLo>=0.0d0) THEN ! not heating and not cooling
        AvailStatus = NoAction
        CycleOnFlag = .False.
        TempDiffHi = 0.0d0
        TempDiffLo = 0.0d0
      ELSE IF (TempDiffHi < 30.0d0) THEN ! Cooling Mode
        TempDiff = TempDiffHi
        DeltaTime = TempDiff/OptStartSysAvailMgrData(SysAvailNum)%ConstTGradCool
        IF (DeltaTime>OptStartSysAvailMgrData(SysAvailNum)%MaxOptStartTime) THEN
          DeltaTime=OptStartSysAvailMgrData(SysAvailNum)%MaxOptStartTime
        END IF
        PreStartTime = FanStartTime - DeltaTime
        IF (PreStartTime<0) PreStartTime = -.1d0
        PreStartTimeTmr = FanStartTimeTmr - DeltaTime
        IF (PreStartTimeTmr<0) THEN
          PreStartTimeTmr = PreStartTimeTmr + 24.0d0
          OverNightStartFlag = .True.
        ELSE
          OverNightStartFlag = .False.
        END IF
        IF (.NOT. OverNightStartFlag) THEN
          IF (FanStartTime == 0.0d0 .OR. CurrentTime .GT. FanStartTime) THEN
            AvailStatus = NoAction
              CycleOnFlag = .False.
              OSReportVarFlag = .True.
            ELSE IF (CyCleOnFlag) THEN
              AvailStatus = CycleOn
              DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
              END DO
            ELSE IF (PreStartTime .LT. CurrentTime) THEN
              IF (OSReportVarFlag) THEN
                NumHoursBeforeOccupancy=DeltaTime
                OSReportVarFlag = .False.
              END IF
              AvailStatus = CycleOn
              CycleOnFlag = .True.
              DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
              END DO
            ELSE
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
            END IF
          ELSE
          IF (FanStartTime == 0.0d0 .OR. (CurrentTime .GT. FanStartTime .AND. CurrentTime .LE. PreStartTimeTmr)) THEN
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
          ELSE IF (CycleOnFlag) THEN
            AvailStatus = CycleOn
            DO I=1, NumOfZones
              If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
              END IF
            END DO
          ELSE IF (PreStartTime .LT. CurrentTime .OR. PreStartTimeTmr .LT. CurrentTime) THEN
            IF (OSReportVarFlag) THEN
              NumHoursBeforeOccupancy=DeltaTime
              OSReportVarFlag = .False.
            END IF
            AvailStatus = CycleOn
            CycleOnFlag = .True.
            DO I=1, NumOfZones
              If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
              END IF
            END DO
          ELSE
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
          END IF
        END IF
      ELSE
        AvailStatus = NoAction
        CycleOnFlag = .False.
      END IF
  ELSE
    AvailStatus = NoAction
  END IF

CASE(AdaptiveTemperatureGradient)
    NumPreDays = OptStartSysAvailMgrData(SysAvailNum)%NumPreDays
  IF (OptStartSysAvailMgrData(SysAvailNum)%CtrlType == ControlZone) THEN
    IF (.NOT.ALLOCATED(AdaTempGradTrdHeat)) THEN
      ALLOCATE(AdaTempGradTrdHeat(NumPreDays))
      ALLOCATE(AdaTempGradTrdCool(NumPreDays))
    END IF
    ZoneNum = OptStartSysAvailMgrData(SysAvailNum)%ZoneNum
    IF ((.Not.allocated(TempTstatAir)) .OR. (.Not.allocated(ZoneThermostatSetPointLo)) .OR. &
      (.Not.allocated(ZoneThermostatSetPointHi))) THEN
      TempDiff = 0.0d0
    ELSE
      IF (.NOT. CycleOnFlag) THEN
        IF (ALLOCATED(OccRoomTSetPointHeat) .AND. ALLOCATED(OccRoomTSetPointCool)) THEN
          TempDiffHi = TempTstatAir(ZoneNum) - OccRoomTSetPointCool(ZoneNum)
          TempDiffLo = TempTstatAir(ZoneNum) - OccRoomTSetPointHeat(ZoneNum)
        ELSE
          TempDiffHi = 0.0d0
          TempDiffLo = 0.0d0
        END IF
      END IF
    END IF
    !Store adaptive temperature gradients for previous days and calculate the adaptive temp gradients
    !-----------------------------------------------------------------------------
    IF (WarmupFlag) THEN
        AdaTempGradHeat = OptStartSysAvailMgrData(SysAvailNum)%InitTGradHeat
        AdaTempGradCool = OptStartSysAvailMgrData(SysAvailNum)%InitTGradCool
    ELSE IF (DayOfSim == BeginDay .AND. BeginDayFlag) THEN
        AdaTempGradTrdHeat = OptStartSysAvailMgrData(SysAvailNum)%InitTGradHeat
        AdaTempGradHeat = OptStartSysAvailMgrData(SysAvailNum)%InitTGradHeat
        AdaTempGradTrdCool = OptStartSysAvailMgrData(SysAvailNum)%InitTGradHeat
        AdaTempGradCool = OptStartSysAvailMgrData(SysAvailNum)%InitTGradHeat
    ELSE
        IF (BeginDayFlag .AND. FirstTimeATGFlag) THEN
            FirstTimeATGFlag = .False.
            AdaTempGradHeat = AdaTempGradHeat + AdaTempGradTrdHeat(NumPreDays)/NumPreDays - &
                        AdaTempGradTrdHeat(1)/NumPreDays
            AdaTempGradCool = AdaTempGradCool + AdaTempGradTrdCool(NumPreDays)/NumPreDays - &
                        AdaTempGradTrdCool(1)/NumPreDays
            IF (FanStartTime>0) THEN
                DO ATGCounter=1, NumPreDays-1
                AdaTempGradTrdHeat(ATGCounter) = AdaTempGradTrdHeat(ATGCounter+1)
                AdaTempGradTrdCool(ATGCounter) = AdaTempGradTrdCool(ATGCounter+1)
                END DO
            END IF
        END IF
    END IF

    IF (CurrentTime >= 1.0d0) FirstTimeATGFlag = .True.
    !------------------------------------------------------------------------------

    IF (TempDiffHi<0.0d0) THEN
        TempDiff = TempDiffLo
        IF (TempDiff .LT. 0.0d0) THEN !Heating Mode
          TempDiff = ABS(TempDiff)
          DeltaTime = TempDiff/AdaTempGradHeat
          IF (DeltaTime>OptStartSysAvailMgrData(SysAvailNum)%MaxOptStartTime) THEN
            DeltaTime=OptStartSysAvailMgrData(SysAvailNum)%MaxOptStartTime
          END IF
          PreStartTime = FanStartTime - DeltaTime
          IF (PreStartTime<0.0d0) PreStartTime = -.1d0
          PreStartTimeTmr = FanStartTimeTmr - DeltaTime
          IF (PreStartTimeTmr<0.0d0) THEN
            PreStartTimeTmr = PreStartTimeTmr + 24.0d0
            OverNightStartFlag = .True.
          ELSE
            OverNightStartFlag = .False.
          END IF
          IF (.NOT. OverNightStartFlag) THEN
            IF (FanStartTime == 0.0d0 .OR. CurrentTime .GT. FanStartTime) THEN
              AvailStatus = NoAction
              CycleOnFlag = .False.
              OSReportVarFlag = .True.
            ELSE IF (CyCleOnFlag) THEN
              AvailStatus = CycleOn
              DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
              END DO
              IF (CurrentTime .GT. FanStartTime) CycleOnFlag = .False.
              ! Calculate the current day actual temperature gradient --------------------------
              IF (.NOT. WarmupFlag) THEN
                  IF (ATGUpdateFlag1) THEN
                    ATGUpdateTime1 = CurrentTime
                    ATGUpdateTemp1 = TempTstatAir(ZoneNum)
                    ATGUpdateFlag1 = .False.
                  END IF
                  IF (TempTstatAir(ZoneNum) .GE. OccRoomTSetPointHeat(ZoneNum) .AND. ATGUpdateFlag2) THEN
                    ATGUpdateTime2 = CurrentTime
                    ATGUpdateTemp2 = TempTstatAir(ZoneNum)
                    ATGUpdateFlag2 = .False.
                IF (ABS(ATGUpdateTime2-ATGUpdateTime1) > 1.d-10) THEN
                  AdaTempGradTrdHeat(NumPreDays)=(ATGUpdateTemp2-ATGUpdateTemp1)/(ATGUpdateTime2-ATGUpdateTime1)
                ELSE
                  AdaTempGradTrdHeat(NumPreDays)=(ATGUpdateTemp2-ATGUpdateTemp1)*NumOfTimeStepInHour
                END IF
                  END IF
              END IF
              !---------------------------------------------------------------------------------
            ELSE IF (PreStartTime .LT. CurrentTime) THEN
              IF (OSReportVarFlag) THEN
                NumHoursBeforeOccupancy=DeltaTime
                OSReportVarFlag = .False.
              END IF
              AvailStatus = CycleOn
              CycleOnFlag = .True.
              ATGUpdateFlag1 = .True.
              ATGUpdateFlag2 = .True.
              DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
              END DO
            ELSE
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
            END IF
          ELSE
          IF (FanStartTime == 0.0d0 .OR. (CurrentTime .GT. FanStartTime .AND. CurrentTime .LE. PreStartTimeTmr)) THEN
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
          ELSE IF (CycleOnFlag) THEN
            AvailStatus = CycleOn
            DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
            END DO
            IF (CurrentTime .GT. FanStartTime .AND. CurrentTime .LT. PreStartTimeTmr) CycleOnFlag=.False.
            ! Calculate the current day actual temperature gradient --------------------------
              IF (.NOT. WarmupFlag) THEN
                IF (ATGUpdateFlag1) THEN
                  ATGUpdateTime1 = CurrentTime
                  ATGUpdateTemp1 = TempTstatAir(ZoneNum)
                  ATGUpdateFlag1 = .False.
                END IF
                IF (TempTstatAir(ZoneNum) .GE. OccRoomTSetPointHeat(ZoneNum) .AND. ATGUpdateFlag2) THEN
                  ATGUpdateTime2 = CurrentTime
                  ATGUpdateTemp2 = TempTstatAir(ZoneNum)
                  ATGUpdateFlag2 = .False.
                IF (ABS(ATGUpdateTime2-ATGUpdateTime1+24.0d0) > 1.d-10) THEN
                  AdaTempGradTrdHeat(NumPreDays)=(ATGUpdateTemp2-ATGUpdateTemp1)/(ATGUpdateTime2-ATGUpdateTime1+24.0d0)
                ELSE
                  AdaTempGradTrdHeat(NumPreDays)=(ATGUpdateTemp2-ATGUpdateTemp1)*NumOfTimeStepInHour
                END IF
                END IF
              END IF
              !---------------------------------------------------------------------------------
          ELSE IF (PreStartTime .LT. CurrentTime .OR. PreStartTimeTmr .LT. CurrentTime) THEN
            IF (OSReportVarFlag) THEN
                NumHoursBeforeOccupancy=DeltaTime
                OSReportVarFlag = .False.
            END IF
            AvailStatus = CycleOn
            CycleOnFlag = .True.
            ATGUpdateFlag1 = .True.
            ATGUpdateFlag2 = .True.
            DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
            END DO
          ELSE
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
          END IF
          END IF
        ELSE
          AvailStatus = NoAction
          CycleOnFlag = .False.
        END IF
      ELSE IF (OccRoomTSetPointCool(ZoneNum) .LT. 50.0d0) THEN ! Cooling Mode
          TempDiff = TempDiffHi
          DeltaTime = TempDiff/AdaTempGradCool
          IF (DeltaTime>OptStartSysAvailMgrData(SysAvailNum)%MaxOptStartTime) THEN
            DeltaTime=OptStartSysAvailMgrData(SysAvailNum)%MaxOptStartTime
          END IF
          PreStartTime = FanStartTime - DeltaTime
          IF (PreStartTime<0.0d0) PreStartTime = -.1d0
          PreStartTimeTmr = FanStartTimeTmr - DeltaTime
          IF (PreStartTimeTmr<0.0d0) THEN
            PreStartTimeTmr = PreStartTimeTmr + 24.0d0
            OverNightStartFlag = .True.
          ELSE
            OverNightStartFlag = .False.
          END IF
          IF (.NOT. OverNightStartFlag) THEN
            IF (FanStartTime == 0.0d0 .OR. CurrentTime .GT. FanStartTime) THEN
              AvailStatus = NoAction
              CycleOnFlag = .False.
              OSReportVarFlag = .True.
            ELSE IF (CyCleOnFlag) THEN
              IF (OSReportVarFlag) THEN
                NumHoursBeforeOccupancy=DeltaTime
                OSReportVarFlag = .False.
              END IF
              AvailStatus = CycleOn
              DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
              END DO
              IF (.NOT. WarmupFlag) THEN
                  IF (ATGUpdateFlag1) THEN
                    ATGUpdateTime1 = CurrentTime
                    ATGUpdateTemp1 = TempTstatAir(ZoneNum)
                    ATGUpdateFlag1 = .False.
                  END IF
                  IF (TempTstatAir(ZoneNum) .LE. OccRoomTSetPointCool(ZoneNum) .AND. ATGUpdateFlag2) THEN
                    ATGUpdateTime2 = CurrentTime
                    ATGUpdateTemp2 = TempTstatAir(ZoneNum)
                    ATGUpdateFlag2 = .False.
                IF (ABS(ATGUpdateTime2-ATGUpdateTime1) > 1.d-10) THEN
                  AdaTempGradTrdCool(NumPreDays)=(ATGUpdateTemp1-ATGUpdateTemp2)/(ATGUpdateTime2-ATGUpdateTime1)
                ELSE
                  AdaTempGradTrdCool(NumPreDays)=(ATGUpdateTemp1-ATGUpdateTemp2)*NumOfTimeStepInHour
                END IF
                  END IF
              END IF
            ELSE IF (PreStartTime .LT. CurrentTime) THEN
              AvailStatus = CycleOn
              CycleOnFlag = .True.
              ATGUpdateFlag1 = .True.
              ATGUpdateFlag2 = .True.
              DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
              END DO
            ELSE
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
            END IF
          ELSE
          IF (FanStartTime == 0.0d0 .OR. (CurrentTime .GT. FanStartTime .AND. CurrentTime .LE. PreStartTimeTmr)) THEN
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
          ELSE IF (CycleOnFlag) THEN
            AvailStatus = CycleOn
            IF (.NOT. WarmupFlag) THEN
              IF (ATGUpdateFlag1) THEN
                ATGUpdateTime1 = CurrentTime
                ATGUpdateTemp1 = TempTstatAir(ZoneNum)
                ATGUpdateFlag1 = .False.
              END IF
              IF (TempTstatAir(ZoneNum) .LE. OccRoomTSetPointCool(ZoneNum) .AND. ATGUpdateFlag2) THEN
                ATGUpdateTime2 = CurrentTime
                ATGUpdateTemp2 = TempTstatAir(ZoneNum)
                ATGUpdateFlag2 = .False.
                IF (ABS(ATGUpdateTime2-ATGUpdateTime1+24.0d0) > 1.d-10) THEN
                  AdaTempGradTrdCool(NumPreDays)=(ATGUpdateTemp1-ATGUpdateTemp2)/(ATGUpdateTime2-ATGUpdateTime1+24.0d0)
                ELSE
                  AdaTempGradTrdCool(NumPreDays)=(ATGUpdateTemp1-ATGUpdateTemp2)*NumOfTimeStepInHour
                END IF
              END IF
            END IF
            DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
            END DO
          ELSE IF (PreStartTime .LT. CurrentTime .OR. PreStartTimeTmr .LT. CurrentTime) THEN
            IF (OSReportVarFlag) THEN
                NumHoursBeforeOccupancy=DeltaTime
                OSReportVarFlag = .False.
            END IF
            AvailStatus = CycleOn
            CycleOnFlag = .True.
            ATGUpdateFlag1 = .True.
            ATGUpdateFlag2 = .True.
            DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
            END DO
          ELSE
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
          END IF
          END IF
        ELSE !Not heating nor cooling mode
          AvailStatus = NoAction
          CycleOnFlag = .False.
        END IF
ELSE IF (OptStartSysAvailMgrData(SysAvailNum)%CtrlType == MaximumOfZoneList) THEN
    IF (.NOT. ALLOCATED(AdaTempGradTrdHeat)) THEN
      ALLOCATE(AdaTempGradTrdHeat(NumPreDays))
      ALLOCATE(AdaTempGradTrdCool(NumPreDays))
    END IF
    NumOfZonesInList = OptStartSysAvailMgrData(SysAvailNum)%NumOfZones
    ATGWCZoneNumHi = OptStartSysAvailMgrData(SysAvailNum)%ZonePtrs(1)
    ATGWCZoneNumLo = OptStartSysAvailMgrData(SysAvailNum)%ZonePtrs(1)
    IF ((.Not.allocated(TempTstatAir)) .OR. (.Not.allocated(ZoneThermostatSetPointLo)) .OR. &
        (.Not.allocated(ZoneThermostatSetPointHi))) THEN
      TempDiff = 0.0d0
    ELSE
      IF (.NOT. CycleOnFlag) THEN
        IF (ALLOCATED(OccRoomTSetPointHeat) .AND. ALLOCATED(OccRoomTSetPointCool)) THEN
          TempDiffHi = 0.0d0
          TempDiffLo = 0.0d0
          DO ZoneNum=1, NumOfZonesInList
            TempDiff=TempTstatAir(OptStartSysAvailMgrData(SysAvailNum)%ZonePtrs(ZoneNum)) - &
                     OccRoomTSetPointCool(OptStartSysAvailMgrData(SysAvailNum)%ZonePtrs(ZoneNum))
            TempDiffHi = Max(TempDiffHi, TempDiff)
            !Store the worse case zone number for actual temperature gradient calculation
            IF (TempDiff == TempDiffHi) THEN
              ATGWCZoneNumHi = OptStartSysAvailMgrData(SysAvailNum)%ZonePtrs(ZoneNum)
            ELSE
              ATGWCZoneNumHi = OptStartSysAvailMgrData(SysAvailNum)%ZonePtrs(1)
            END IF
            TempDiff=TempTstatAir(OptStartSysAvailMgrData(SysAvailNum)%ZonePtrs(ZoneNum)) - &
                     OccRoomTSetPointHeat(OptStartSysAvailMgrData(SysAvailNum)%ZonePtrs(ZoneNum))
            TempDiffLo = Min(TempDiffLo, TempDiff)
            IF (TempDiff == TempDiffLo) THEN
              ATGWCZoneNumLo = OptStartSysAvailMgrData(SysAvailNum)%ZonePtrs(ZoneNum)
            ELSE
              ATGWCZoneNumLo = OptStartSysAvailMgrData(SysAvailNum)%ZonePtrs(1)
            END IF
          END DO
        ELSE
          TempDiffHi = 0.0d0
          TempDiffLo = 0.0d0
        END IF
      END IF
    END IF
    !Store adaptive temperature gradients for previous days and calculate the adaptive temp gradients
    !-----------------------------------------------------------------------------
    IF (WarmupFlag) THEN
        AdaTempGradHeat = OptStartSysAvailMgrData(SysAvailNum)%InitTGradHeat
        AdaTempGradCool = OptStartSysAvailMgrData(SysAvailNum)%InitTGradCool
    ELSE IF (DayOfSim == BeginDay .AND. BeginDayFlag) THEN
        AdaTempGradTrdHeat = OptStartSysAvailMgrData(SysAvailNum)%InitTGradHeat
        AdaTempGradHeat = OptStartSysAvailMgrData(SysAvailNum)%InitTGradHeat
        AdaTempGradTrdCool = OptStartSysAvailMgrData(SysAvailNum)%InitTGradHeat
        AdaTempGradCool = OptStartSysAvailMgrData(SysAvailNum)%InitTGradHeat
    ELSE
        IF (BeginDayFlag .AND. FirstTimeATGFlag) THEN
            FirstTimeATGFlag = .False.
            AdaTempGradHeat = AdaTempGradHeat + AdaTempGradTrdHeat(NumPreDays)/NumPreDays - &
                        AdaTempGradTrdHeat(1)/NumPreDays
            AdaTempGradCool = AdaTempGradCool + AdaTempGradTrdCool(NumPreDays)/NumPreDays - &
                        AdaTempGradTrdCool(1)/NumPreDays
            IF (FanStartTime>0) THEN
                DO ATGCounter=1, NumPreDays-1
                AdaTempGradTrdHeat(ATGCounter) = AdaTempGradTrdHeat(ATGCounter+1)
                AdaTempGradTrdCool(ATGCounter) = AdaTempGradTrdCool(ATGCounter+1)
                END DO
            END IF
        END IF
    END IF

    IF (CurrentTime >= 1.0d0) FirstTimeATGFlag = .True.
    !------------------------------------------------------------------------------

      IF ((TempDiffHi<0.0d0 .AND. TempDiffLo<0.0d0) .OR. (abs(TempDiffLo)>abs(TempDiffHi) &
         .AND. TempDiffLo<0.0d0)) THEN !Heating Mode
          TempDiff = TempDiffLo
          TempDiff = ABS(TempDiff)
          DeltaTime = TempDiff/AdaTempGradHeat
          IF (DeltaTime>OptStartSysAvailMgrData(SysAvailNum)%MaxOptStartTime) THEN
            DeltaTime=OptStartSysAvailMgrData(SysAvailNum)%MaxOptStartTime
          END IF
          PreStartTime = FanStartTime - DeltaTime
          IF (PreStartTime<0.0d0) PreStartTime = -.1d0
          PreStartTimeTmr = FanStartTimeTmr - DeltaTime
          IF (PreStartTimeTmr<0.0d0) THEN
            PreStartTimeTmr = PreStartTimeTmr + 24.0d0
            OverNightStartFlag = .True.
          ELSE
            OverNightStartFlag = .False.
          END IF
          IF (.NOT. OverNightStartFlag) THEN
            IF (FanStartTime == 0.0d0 .OR. CurrentTime .GT. FanStartTime) THEN
              OSReportVarFlag = .True.
              AvailStatus = NoAction
              CycleOnFlag = .False.
            ELSE IF (CyCleOnFlag) THEN
              AvailStatus = CycleOn
              DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
              END DO
              IF (CurrentTime .GT. FanStartTime) CycleOnFlag = .False.
              ! Calculate the current day actual temperature gradient --------------------------
              IF (.NOT. WarmupFlag) THEN
                  IF (ATGUpdateFlag1) THEN
                    ATGUpdateTime1 = CurrentTime
                    ATGUpdateTemp1 = TempTstatAir(ATGWCZoneNumLo)
                    ATGUpdateFlag1 = .False.
                  END IF
                  IF (TempTstatAir(ATGWCZoneNumLo) .GE. OccRoomTSetPointHeat(ATGWCZoneNumLo) .AND. &
                  ATGUpdateFlag2) THEN
                    ATGUpdateTime2 = CurrentTime
                    ATGUpdateTemp2 = TempTstatAir(ATGWCZoneNumLo)
                    ATGUpdateFlag2 = .False.
                IF (ABS(ATGUpdateTime2-ATGUpdateTime1) > 1.d-10) THEN
                  AdaTempGradTrdHeat(NumPreDays)=(ATGUpdateTemp2-ATGUpdateTemp1)/(ATGUpdateTime2-ATGUpdateTime1)
                ELSE
                  AdaTempGradTrdHeat(NumPreDays)=(ATGUpdateTemp2-ATGUpdateTemp1)*NumOfTimeStepInHour
                END IF
                  END IF
              END IF
              !---------------------------------------------------------------------------------
            ELSE IF (PreStartTime .LT. CurrentTime) THEN
              IF (OSReportVarFlag) THEN
                NumHoursBeforeOccupancy=DeltaTime
                OSReportVarFlag = .False.
              END IF
              AvailStatus = CycleOn
              CycleOnFlag = .True.
              ATGUpdateFlag1 = .True.
              ATGUpdateFlag2 = .True.
              DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
              END DO
            ELSE
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
            END IF
          ELSE
          IF (FanStartTime == 0.0d0 .OR. (CurrentTime .GT. FanStartTime .AND. CurrentTime .LE. PreStartTimeTmr)) THEN
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
          ELSE IF (CycleOnFlag) THEN
            AvailStatus = CycleOn
            ! Calculate the current day actual temperature gradient --------------------------
            IF (.NOT. WarmupFlag) THEN
                IF (ATGUpdateFlag1) THEN
                ATGUpdateTime1 = CurrentTime
                ATGUpdateTemp1 = TempTstatAir(ATGWCZoneNumLo)
                ATGUpdateFlag1 = .False.
                END IF
                IF (TempTstatAir(ATGWCZoneNumLo) .GE. OccRoomTSetPointHeat(ATGWCZoneNumLo) .AND. &
                ATGUpdateFlag2) THEN
                ATGUpdateTime2 = CurrentTime
                ATGUpdateTemp2 = TempTstatAir(ATGWCZoneNumLo)
                ATGUpdateFlag2 = .False.
                IF (ABS(ATGUpdateTime2-ATGUpdateTime1+24.0d0) > 1.d-10) THEN
                  AdaTempGradTrdHeat(NumPreDays)=(ATGUpdateTemp2-ATGUpdateTemp1)/(ATGUpdateTime2-ATGUpdateTime1+24.0d0)
                ELSE
                  AdaTempGradTrdHeat(NumPreDays)=(ATGUpdateTemp2-ATGUpdateTemp1)*NumOfTimeStepInHour
                END IF
                END IF
            END IF
            !---------------------------------------------------------------------------------
            DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
            END DO
            IF (CurrentTime .GT. FanStartTime .AND. CurrentTime .LT. PreStartTimeTmr) CycleOnFlag=.False.
          ELSE IF (PreStartTime .LT. CurrentTime .OR. PreStartTimeTmr .LT. CurrentTime) THEN
            IF (OSReportVarFlag) THEN
                NumHoursBeforeOccupancy=DeltaTime
                OSReportVarFlag = .False.
            END IF
            AvailStatus = CycleOn
            CycleOnFlag = .True.
            ATGUpdateFlag1 = .True.
            ATGUpdateFlag2 = .True.
            DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
            END DO
          ELSE
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
          END IF
          END IF
      ELSE IF (TempDiffHi<=0.0d0 .AND. TempDiffLo>=0.0d0) THEN ! not heating and not cooling
            AvailStatus = NoAction
            CycleOnFlag = .False.
            TempDiffHi = 0.0d0
            TempDiffLo = 0.0d0
      ELSE IF (TempDiffHi < 30.0d0) THEN ! Cooling Mode
          TempDiff = TempDiffHi
          DeltaTime = TempDiff/AdaTempGradCool
          IF (DeltaTime>OptStartSysAvailMgrData(SysAvailNum)%MaxOptStartTime) THEN
            DeltaTime=OptStartSysAvailMgrData(SysAvailNum)%MaxOptStartTime
          END IF
          PreStartTime = FanStartTime - DeltaTime
          IF (PreStartTime<0) PreStartTime = -.1d0
          PreStartTimeTmr = FanStartTimeTmr - DeltaTime
          IF (PreStartTimeTmr<0) THEN
            PreStartTimeTmr = PreStartTimeTmr + 24.0d0
            OverNightStartFlag = .True.
          ELSE
            OverNightStartFlag = .False.
          END IF
          IF (.NOT. OverNightStartFlag) THEN
            IF (FanStartTime == 0.0d0 .OR. CurrentTime .GT. FanStartTime) THEN
              AvailStatus = NoAction
              CycleOnFlag = .False.
              OSReportVarFlag = .True.
            ELSE IF (CyCleOnFlag) THEN
              AvailStatus = CycleOn
            ! Calculate the current day actual temperature gradient --------------------------
            IF (.NOT. WarmupFlag) THEN
                IF (ATGUpdateFlag1) THEN
                ATGUpdateTime1 = CurrentTime
                ATGUpdateTemp1 = TempTstatAir(ATGWCZoneNumHi)
                ATGUpdateFlag1 = .False.
                END IF
                IF (TempTstatAir(ATGWCZoneNumHi) .LE. OccRoomTSetPointCool(ATGWCZoneNumHi) .AND. &
                ATGUpdateFlag2) THEN
                ATGUpdateTime2 = CurrentTime
                ATGUpdateTemp2 = TempTstatAir(ATGWCZoneNumHi)
                ATGUpdateFlag2 = .False.
                IF (ABS(ATGUpdateTime2-ATGUpdateTime1) > 1.d-10) THEN
                  AdaTempGradTrdCool(NumPreDays)=(ATGUpdateTemp1-ATGUpdateTemp2)/(ATGUpdateTime2-ATGUpdateTime1)
                ELSE
                  AdaTempGradTrdCool(NumPreDays)=(ATGUpdateTemp1-ATGUpdateTemp2)*NumOfTimeStepInHour
                ENDIF

                END IF
            END IF
            !---------------------------------------------------------------------------------
              DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
              END DO
            ELSE IF (PreStartTime .LT. CurrentTime) THEN
              IF (OSReportVarFlag) THEN
                NumHoursBeforeOccupancy=DeltaTime
                OSReportVarFlag = .False.
              END IF
              AvailStatus = CycleOn
              CycleOnFlag = .True.
              ATGUpdateFlag1 = .True.
              ATGUpdateFlag2 = .True.
              DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
              END DO
            ELSE
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
            END IF
          ELSE
          IF (FanStartTime == 0.0d0 .OR. (CurrentTime .GT. FanStartTime .AND. CurrentTime .LE. PreStartTimeTmr)) THEN
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
          ELSE IF (CycleOnFlag) THEN
            AvailStatus = CycleOn
            ! Calculate the current day actual temperature gradient --------------------------
            IF (.NOT. WarmupFlag) THEN
                IF (ATGUpdateFlag1) THEN
                ATGUpdateTime1 = CurrentTime
                ATGUpdateTemp1 = TempTstatAir(ATGWCZoneNumHi)
                ATGUpdateFlag1 = .False.
                END IF
                IF (TempTstatAir(ATGWCZoneNumHi) .LE. OccRoomTSetPointCool(ATGWCZoneNumHi) .AND. &
                ATGUpdateFlag2) THEN
                ATGUpdateTime2 = CurrentTime
                ATGUpdateTemp2 = TempTstatAir(ATGWCZoneNumHi)
                ATGUpdateFlag2 = .False.
                IF (ABS(ATGUpdateTime2-ATGUpdateTime1+24.0d0) > 1.d-10) THEN
                  AdaTempGradTrdCool(NumPreDays)=(ATGUpdateTemp1-ATGUpdateTemp2)/(ATGUpdateTime2-ATGUpdateTime1+24.0d0)
                ELSE
                  AdaTempGradTrdCool(NumPreDays)=(ATGUpdateTemp1-ATGUpdateTemp2)*NumOfTimeStepInHour
                END IF
                END IF
            END IF
            !---------------------------------------------------------------------------------
            DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
            END DO
          ELSE IF (PreStartTime .LT. CurrentTime .OR. PreStartTimeTmr .LT. CurrentTime) THEN
            IF (OSReportVarFlag) THEN
                NumHoursBeforeOccupancy=DeltaTime
                OSReportVarFlag = .False.
            END IF
            AvailStatus = CycleOn
            CycleOnFlag = .True.
            ATGUpdateFlag2 = .True.
            ATGUpdateFlag1 = .True.
            DO I=1, NumOfZones
                If (ZoneEquipConfig(I)%AirLoopNum==PriAirSysNum) THEN
                    OptStartData%OptStartFlag(ZoneEquipConfig(I)%ActualZoneNum)=.TRUE.
                END IF
            END DO
          ELSE
            AvailStatus = NoAction
            CycleOnFlag = .False.
            OSReportVarFlag = .True.
          END IF
          END IF
        ELSE
          AvailStatus = NoAction
          CycleOnFlag = .False.
        END IF
  ELSE
    AvailStatus = NoAction
  END IF

CASE(AdaptiveASHRAE)
    AvailStatus = NoAction
END SELECT
END IF

  OptStartSysAvailMgrData(SysAvailNum)%AvailStatus = AvailStatus
  OptStartSysAvailMgrData(SysAvailNum)%NumHoursBeforeOccupancy = NumHoursBeforeOccupancy

  RETURN

END SUBROUTINE CalcOptStartSysAvailMgr

SUBROUTINE CalcNVentSysAvailMgr(SysAvailNum,PriAirSysNum,AvailStatus,ZoneEquipType)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   December 2004
          !       MODIFIED       March 2011, Chandan Sharma - FSEC: Allowed night ventilation
          !                             availability manager to work for zone component
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set AvailStatus indicator for a primary air loop and ZoneHVAC component and sets a specified flow
          ! rate fraction for the air loop for use during night ventilation.

          ! METHODOLOGY EMPLOYED:
          ! Looks at outside and indoor conditions to determine if night ventilation
          ! is beneficial. If it is and it is scheduled on the AvailStatus is set to cycle
          ! on and the loop flow rate fractionis set to the specified night ventilation
          ! value.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataAirLoop
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE DataHeatBalFanSys, ONLY: TempZoneThermostatSetpoint, ZoneThermostatSetPointHi, &
                               ZoneThermostatSetPointLo, TempControlType, TempTstatAir
  USE DataEnvironment, ONLY:   OutDryBulbTemp


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SysAvailNum         ! number of the current scheduled system availability manager
  INTEGER, INTENT (IN)  :: PriAirSysNum        ! number of the primary air system affected by this Avail. Manager
  INTEGER, INTENT (OUT) :: AvailStatus         ! System status indicator
  INTEGER,  OPTIONAL,  INTENT(IN)  :: ZoneEquipType  ! Type of zone equipment component
          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ZoneInSysNum
  INTEGER :: CtrldZoneNum
  INTEGER :: ZoneNum
  LOGICAL :: TempCheck ! TRUE if one zone's temperature is above the value of the vent temp sched
  LOGICAL :: DelTCheck ! TRUE if the control zone temperature - outside temperature > VentDelT
  LOGICAL :: LowLimCheck ! TRUE if one zones's air temperature is below this value
  REAL(r64) :: VentTemp       ! value of the ventilation temperature schedule
  INTEGER :: ControlZoneNum ! actual zone number of the control zone

  TempCheck = .FALSE.
  DelTCheck = .FALSE.
  LowLimCheck = .FALSE.
! check if night venting allowed: not allowed if avail sched is off or fan sched is on
! CR 7913 changed to allow during warmup
  IF ( (GetCurrentScheduleValue(NVentSysAvailMgrData(SysAvailNum)%SchedPtr) <= 0.0d0) .OR. &
       (GetCurrentScheduleValue(NVentSysAvailMgrData(SysAvailNum)%FanSchedPtr) > 0.0d0)  )  THEN
    AvailStatus = NoAction
  ELSE

    VentTemp = GetCurrentScheduleValue(NVentSysAvailMgrData(SysAvailNum)%VentTempSchedPtr)
    ControlZoneNum = NVentSysAvailMgrData(SysAvailNum)%ZoneNum

    IF (PRESENT(ZoneEquipType)) THEN
      ! if the room temperature is greater than the vent temp sched value, set the vent temp check to TRUE
      IF (TempTstatAir(ControlZoneNum) > VentTemp) THEN
        TempCheck = .TRUE.
      END IF
      ! if the room temperature is less than the low limit set the low limit check to TRUE
      IF (TempTstatAir(ControlZoneNum) < NVentSysAvailMgrData(SysAvailNum)%VentTempLowLim) THEN
        LowLimCheck = .TRUE.
      END IF
    ELSE
      DO ZoneInSysNum=1,AirToZoneNodeInfo(PriAirSysNum)%NumZonesCooled ! loop over zones in system

         CtrldZoneNum = AirToZoneNodeInfo(PriAirSysNum)%CoolCtrlZoneNums(ZoneInSysNum)
         ZoneNum = ZoneEquipConfig(CtrldZoneNum)%ActualZoneNum
         ! if the room temperature is greater than the vent temp sched value, set the vent temp check to TRUE
         IF (TempTstatAir(ZoneNum) > VentTemp) THEN
           TempCheck = .TRUE.
         END IF
         ! if the room temperature is less than the low limit set the low limit check to TRUE
         IF (TempTstatAir(ZoneNum) < NVentSysAvailMgrData(SysAvailNum)%VentTempLowLim) THEN
           LowLimCheck = .TRUE.
         END IF

      END DO
    ENDIF
    ! If the difference between the control zone temperature and the outside temperature is greater than
    ! the specified night venting delta T then set the delta T check to TRUE
    IF ( (TempTstatAir(ControlZoneNum) - OutDryBulbTemp)  > NVentSysAvailMgrData(SysAvailNum)%VentDelT ) THEN
      DelTCheck = .TRUE.
    END IF
    ! If the limit requirements are met turn on night ventilation
    IF (TempCheck .AND. DelTCheck .AND. .NOT. LowLimCheck) THEN
      AvailStatus = CycleOn
    ELSE
      AvailStatus = NoAction
    END IF

  END IF

  IF (.NOT. PRESENT(ZoneEquipType)) THEN
    IF (AvailStatus == CycleOn) THEN
      AirLoopControlInfo(PriAirSysNum)%LoopFlowRateSet = .TRUE.
      AirLoopControlInfo(PriAirSysNum)%NightVent = .TRUE.
      AirLoopFlow(PriAirSysNum)%ReqSupplyFrac = NVentSysAvailMgrData(SysAvailNum)%VentFlowFrac
    END IF
  ENDIF

  NVentSysAvailMgrData(SysAvailNum)%AvailStatus = AvailStatus

  RETURN

END SUBROUTINE CalcNVentSysAvailMgr

SUBROUTINE CalcDiffTSysAvailMgr(SysAvailNum,PreviousStatus,AvailStatus)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   February 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set AvailStatus indicator for a plant loop, primary air loop or ZoneHVAC component.

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:
  USE DataLoopNode, ONLY: Node
  USE DataPlant, ONLY: PlantAvailMgr

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SysAvailNum         ! Number of the current scheduled system availability manager
  INTEGER, INTENT (IN)  :: PreviousStatus      ! System status for the previous timestep
  INTEGER, INTENT (OUT) :: AvailStatus         ! System status indicator

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: DeltaTemp

          ! FLOW:
  DeltaTemp = Node(DiffTSysAvailMgrData(SysAvailNum)%HotNode)%Temp - Node(DiffTSysAvailMgrData(SysAvailNum)%ColdNode)%Temp

  IF (DeltaTemp >= DiffTSysAvailMgrData(SysAvailNum)%TempDiffOn) THEN
    AvailStatus = CycleOn
  ELSE IF (DeltaTemp <= DiffTSysAvailMgrData(SysAvailNum)%TempDiffOff) THEN
    AvailStatus = ForceOff
  ELSE

    IF (PreviousStatus == NoAction) THEN
      AvailStatus = ForceOff
    ELSE
      AvailStatus = PreviousStatus ! No change, but not "NoAction"; it should always be on or off.
    END IF

  END IF

  DiffTSysAvailMgrData(SysAvailNum)%AvailStatus = AvailStatus

  RETURN

END SUBROUTINE CalcDiffTSysAvailMgr


SUBROUTINE CalcHiTurnOffSysAvailMgr(SysAvailNum,AvailStatus)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   February 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set AvailStatus indicator for a plant loop, primary air loop or ZoneHVAC component.

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:
  USE DataLoopNode, ONLY: Node

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SysAvailNum         ! Number of the current scheduled system availability manager
  INTEGER, INTENT (OUT) :: AvailStatus         ! System status indicator

          ! FLOW:
  IF (Node(HiTurnOffSysAvailMgrData(SysAvailNum)%Node)%Temp >= HiTurnOffSysAvailMgrData(SysAvailNum)%Temp) THEN
    AvailStatus = ForceOff
  ELSE
    AvailStatus = NoAction
  END IF

  HiTurnOffSysAvailMgrData(SysAvailNum)%AvailStatus = AvailStatus

  RETURN

END SUBROUTINE CalcHiTurnOffSysAvailMgr


SUBROUTINE CalcHiTurnOnSysAvailMgr(SysAvailNum,AvailStatus)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   February 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set AvailStatus indicator for a plant loop, primary air loop or ZoneHVAC component.

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:
  USE DataLoopNode, ONLY: Node

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SysAvailNum         ! Number of the current scheduled system availability manager
  INTEGER, INTENT (OUT) :: AvailStatus         ! System status indicator

          ! FLOW:
  IF (Node(HiTurnOnSysAvailMgrData(SysAvailNum)%Node)%Temp >= HiTurnOnSysAvailMgrData(SysAvailNum)%Temp) THEN
    AvailStatus = CycleOn
  ELSE
    AvailStatus = NoAction
  END IF

  HiTurnOnSysAvailMgrData(SysAvailNum)%AvailStatus = AvailStatus

  RETURN

END SUBROUTINE CalcHiTurnOnSysAvailMgr


SUBROUTINE CalcLoTurnOffSysAvailMgr(SysAvailNum,AvailStatus)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   February 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set AvailStatus indicator for a plant loop, primary air loop or ZoneHVAC component.

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:
  USE DataLoopNode, ONLY: Node

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SysAvailNum         ! Number of the current scheduled system availability manager
  INTEGER, INTENT (OUT) :: AvailStatus         ! System status indicator

          ! FLOW:

          ! If applicability schedule is off, then availability manager is inactive, return no action
  IF (LoTurnOffSysAvailMgrData(SysAvailNum)%SchedPtr > 0) THEN
    IF(GetCurrentScheduleValue(LoTurnOffSysAvailMgrData(SysAvailNum)%SchedPtr) <= 0.0d0) THEN
      AvailStatus = NoAction
      LoTurnOffSysAvailMgrData(SysAvailNum)%AvailStatus = AvailStatus
      RETURN
    END IF
  END IF

          ! Availability manager is active, check temperature limit
  IF (Node(LoTurnOffSysAvailMgrData(SysAvailNum)%Node)%Temp <= LoTurnOffSysAvailMgrData(SysAvailNum)%Temp) THEN
    AvailStatus = ForceOff
  ELSE
    AvailStatus = NoAction
  END IF

  LoTurnOffSysAvailMgrData(SysAvailNum)%AvailStatus = AvailStatus

  RETURN

END SUBROUTINE CalcLoTurnOffSysAvailMgr


SUBROUTINE CalcLoTurnOnSysAvailMgr(SysAvailNum,AvailStatus)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   February 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set AvailStatus indicator for a plant loop, primary air loop or ZoneHVAC component.

          ! METHODOLOGY EMPLOYED:
          !

          ! USE STATEMENTS:
  USE DataLoopNode, ONLY: Node

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SysAvailNum         ! Number of the current scheduled system availability manager
  INTEGER, INTENT (OUT) :: AvailStatus         ! System status indicator

          ! FLOW:
  IF (Node(LoTurnOnSysAvailMgrData(SysAvailNum)%Node)%Temp <= LoTurnOnSysAvailMgrData(SysAvailNum)%Temp) THEN
    AvailStatus = CycleOn
  ELSE
    AvailStatus = NoAction
  END IF

  LoTurnOnSysAvailMgrData(SysAvailNum)%AvailStatus = AvailStatus

  RETURN

END SUBROUTINE CalcLoTurnOnSysAvailMgr

FUNCTION ValidateAndSetSysAvailabilityManagerType(AvailMgrName) RESULT(ValidType)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   April 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function returns true for a valid System Availability Manager Type
          ! and false if not.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItem

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: AvailMgrName  ! name to validate
  INTEGER                      :: ValidType     ! result of validation

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Found

  Found=FindItem(AvailMgrName,cValidSysAvailManagerTypes,NumValidSysAvailManagerTypes)
  IF (Found > 0 ) THEN
!   Hybrid ventilation must not be specified in a list
    IF(ValidSysAvailManagerTypes(Found) .NE. SysAvailMgr_HybridVent) THEN
      ValidType=ValidSysAvailManagerTypes(Found)
    ELSE
      ValidType=0
    END IF
  ELSE
    ValidType=0
  ENDIF

  RETURN

END FUNCTION ValidateAndSetSysAvailabilityManagerType

SUBROUTINE ManageHybridVentilation

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   March 2007
          !       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Manage the simulation of the Hybrid Ventilation Control System Availability Managers

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! NA

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE DataLoopNode
  USE DataAirLoop

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! None

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: PriAirSysNum              ! Primary Air System index
  INTEGER      :: SysAvailNum
  INTEGER      :: ZoneNum

  IF (GetHybridInputFlag) THEN
    CALL GetHybridVentilationInputs
    GetHybridInputFlag=.FALSE.
  ENDIF

  IF (NumHybridVentSysAvailMgrs == 0) RETURN

  CALL InitHybridVentSysAvailMgr

  DO SysAvailNum = 1,NumHybridVentSysAvailMgrs
    IF (HybridVentSysAvailMgrData(SysAvailNum)%HybridVentMgrConnectedToAirLoop) THEN
      DO PriAirSysNum=1,NumPrimaryAirSys
        IF (HybridVentSysAvailMgrData(SysAvailNum)%AirLoopNum .EQ. PriAirSysNum) &
        CALL CalcHybridVentSysAvailMgr(SysAvailNum,PriAirSysNum)
      END DO
    ELSE
      ! Hybrid ventilation manager is applied to zone component
      IF (HybridVentSysAvailMgrData(SysAvailNum)%SimHybridVentSysAvailMgr) THEN
        CALL CalcHybridVentSysAvailMgr(SysAvailNum)
      ENDIF
    ENDIF
  END DO

  RETURN

END SUBROUTINE ManageHybridVentilation

SUBROUTINE GetHybridVentilationInputs

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   March 2007
          !       MODIFIED       L. GU, 6/23/08, Added more controls, including simple airflow objects
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for Hybrid Ventilation Control System Availability Managers and stores it in
          ! appropriate data structures.

          ! METHODOLOGY EMPLOYED:
          ! Uses InputProcessor "Get" routines to obtain data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,   ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, FindIteminList, SameString, MakeUPPERCase
  USE NodeInputManager, ONLY: GetOnlySingleNode, MarkNode
  USE DataHeatBalance,  ONLY: Zone, TotVentilation, Ventilation
  USE DataLoopNode
  USE General, ONLY: TrimSigDigits
  USE DataAirflowNetwork, ONLY: SimulateAirflowNetwork,AirflowNetworkControlSimple,AirflowNetworkControlSimpleADS
  USE DataIPShortCuts
  USE CurveManager,       ONLY: GetCurveIndex, GetCurveMinMaxValues, CurveValue, GetCurveType

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='GetHybridVentilationInputs: ' ! include trailing blank


          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                         :: NumAlphas  ! Number of Alphas for each GetObjectItem call
  INTEGER                         :: NumNumbers ! Number of Numbers for each GetObjectItem call
  INTEGER                         :: IOStatus   ! Used in GetObjectItem
  LOGICAL                         :: ErrorsFound=.FALSE.  ! Set to true if errors in input, fatal at end of routine
  LOGICAL       :: IsNotOK                      ! Flag to verify name
  LOGICAL       :: IsBlank                      ! Flag for blank name
  INTEGER       :: SysAvailNum                  ! DO loop index for all System Availability Managers
  REAL(r64)     :: SchedMin                     ! Minimum value specified in a schedule
  REAL(r64)     :: SchedMax                     ! Maximum value specified in a schedule
  REAL(r64)     :: CurveMin                     ! Minimum value specified in a curve
  REAL(r64)     :: CurveMax                     ! Maximum value specified in a curve
  REAL(r64)     :: CurveVal                     ! Curve value


  ! Get the number of occurences of each type of System Availability Manager
  cCurrentModuleObject ='AvailabilityManager:HybridVentilation'
  NumHybridVentSysAvailMgrs = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumHybridVentSysAvailMgrs == 0) RETURN

  ! Allocate the data arrays
  ALLOCATE(HybridVentSysAvailMgrData(NumHybridVentSysAvailMgrs))
  ALLOCATE(HybridVentSysAvailAirLoopNum(NumHybridVentSysAvailMgrs))
  ALLOCATE(HybridVentSysAvailActualZoneNum(NumHybridVentSysAvailMgrs))
  ALLOCATE(HybridVentSysAvailVentCtrl(NumHybridVentSysAvailMgrs))
  ALLOCATE(HybridVentSysAvailANCtrlStatus(NumHybridVentSysAvailMgrs))
  ALLOCATE(HybridVentSysAvailMaster(NumHybridVentSysAvailMgrs))
  ALLOCATE(HybridVentSysAvailWindModifier(NumHybridVentSysAvailMgrs))
  HybridVentSysAvailANCtrlStatus = 0
  HybridVentSysAvailMaster = 0

  DO SysAvailNum = 1,NumHybridVentSysAvailMgrs

    CALL GetObjectItem(cCurrentModuleObject,SysAvailNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus,  &
                 AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                 AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(cAlphaArgs(1),HyBridVentSysAvailMgrData%AirLoopName,SysAvailNum-1,IsNotOK,IsBlank,&
                    TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.TRUE.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    HybridVentSysAvailMgrData(SysAvailNum)%Name = cAlphaArgs(1)
    HybridVentSysAvailMgrData(SysAvailNum)%MgrType = SysAvailMgr_HybridVent

    HybridVentSysAvailMgrData(SysAvailNum)%AirLoopName = cAlphaArgs(2)

    IF (lAlphaFieldBlanks(2)) THEN ! Hybrid ventilation manager applied to zone
      HybridVentSysAvailMgrData(SysAvailNum)%HybridVentMgrConnectedToAirLoop = .FALSE.
    ENDIF
    HybridVentSysAvailMgrData(SysAvailNum)%ControlZoneName = cAlphaArgs(3)
    ! Check zone number
    HybridVentSysAvailMgrData(SysAvailNum)%ActualZoneNum = FindItemInList(cAlphaArgs(3),Zone%Name,NumOfZones)
    IF (HybridVentSysAvailMgrData(SysAvailNum)%ActualZoneNum .EQ. 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'" invalid')
      CALL ShowContinueError('not found: '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
      ErrorsFound = .TRUE.
    END IF

    HybridVentSysAvailMgrData(SysAvailNum)%ControlModeSchedPtr = GetScheduleIndex(cAlphaArgs(4))
    IF (HybridVentSysAvailMgrData(SysAvailNum)%ControlModeSchedPtr .EQ. 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'" invalid')
      CALL ShowContinueError('not found: '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
      ErrorsFound = .TRUE.
    END IF

    ! Check schedule values
    SchedMin=GetScheduleMinValue(HybridVentSysAvailMgrData(SysAvailNum)%ControlModeSchedPtr)
    SchedMax=GetScheduleMaxValue(HybridVentSysAvailMgrData(SysAvailNum)%ControlModeSchedPtr)
    IF (SchedMin == 0 .and. SchedMax == 0) THEN
      CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
      CALL ShowContinueError(TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))// &
                         '" specifies control mode 0 for all entries.')
      CALL ShowContinueError('All zones using this '//TRIM(cAlphaFieldNames(4))//' have no hybrid ventilation control.')
    END IF
    IF (SchedMax > 4.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
      CALL ShowContinueError(TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))// &
                         '", the maximum schedule value should be 4. However, ')
      CALL ShowContinueError('the maximum entered value in the schedule is '//TRIM(TrimSigDigits(SchedMax,1)))
      ErrorsFound = .TRUE.
    END IF
    IF (SchedMin < 0.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
      CALL ShowContinueError(TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))// &
                              'the minimum schedule value should be 0. However, ')
      CALL ShowContinueError('the minimum entered value in the schedule is '//TRIM(TrimSigDigits(SchedMin,1)))
      ErrorsFound = .TRUE.
    END IF

    ! Read use weather rain indicator
    IF (SameString(cAlphaArgs(5), 'YES')) THEN
      HybridVentSysAvailMgrData(SysAvailNum)%UseRainIndicator = .TRUE.
    ELSEIF (SameString(cAlphaArgs(5), 'NO')) THEN
      HybridVentSysAvailMgrData(SysAvailNum)%UseRainIndicator = .FALSE.
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
      CALL ShowContinueError('..invalid value: '//TRIM(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
      CALL ShowContinueError('Valid choices are Yes or No.')
      ErrorsFound = .TRUE.
    END IF

    ! Check max wind speed
    IF (NumNumbers > 0) THEN
      HybridVentSysAvailMgrData(SysAvailNum)%MaxWindSpeed = rNumericArgs(1)
      IF (rNumericArgs(1) > 40.0d0 .OR. rNumericArgs(1) < 0.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError(TRIM(cNumericFieldNames(1))//' is beyond the range.')
        CALL ShowContinueError('The input value is '//TRIM(TrimSigDigits(rNumericArgs(1),0))// &
                               '. The allowed value must be >= 0 and <= 40 m/s')
        ErrorsFound = .TRUE.
      END IF
    END IF

    ! Read Max and Min outdoor temperature
    IF (NumNumbers > 1) THEN
      HybridVentSysAvailMgrData(SysAvailNum)%MinOutdoorTemp = rNumericArgs(2)
      IF (rNumericArgs(2) > 100.0d0 .OR. rNumericArgs(2) < -100.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError(TRIM(cNumericFieldNames(2))//' is beyond the range.')
        CALL ShowContinueError('The input value is '//TRIM(TrimSigDigits(rNumericArgs(2),0))// &
                               '. The allowed value must be between -100 C and +100 C')
        ErrorsFound = .TRUE.
      END IF
    END IF
    IF (NumNumbers > 2) THEN
      HybridVentSysAvailMgrData(SysAvailNum)%MaxOutdoorTemp = rNumericArgs(3)
      IF (rNumericArgs(3) > 100.0d0 .OR. rNumericArgs(3) < -100.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError(TRIM(cNumericFieldNames(3))//' is beyond the range.')
        CALL ShowContinueError('The input value is '//TRIM(TrimSigDigits(rNumericArgs(3),0))// &
                               '. The allowed value must be between -100 C and +100 C')
        ErrorsFound = .TRUE.
      END IF
    END IF
    ! Ensure MaxTemp >= MinTemp
    IF (rNumericArgs(2) >= rNumericArgs(3)) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '" The '//TRIM(cNumericFieldNames(2))//' must be less than the '//  &
         TRIM(cNumericFieldNames(3)))
      CALL ShowContinueError('The '//TRIM(cNumericFieldNames(2))//' is '//TRIM(TrimSigDigits(rNumericArgs(2),0))// &
                             '. The '//TRIM(cNumericFieldNames(3))//' is '//TRIM(TrimSigDigits(rNumericArgs(3),0))//'.')
      ErrorsFound = .TRUE.
    END IF

    ! Read Max and Min outdoor enthalpy
    IF (NumNumbers > 3) THEN
      HybridVentSysAvailMgrData(SysAvailNum)%MinOutdoorEnth = rNumericArgs(4)
      IF (rNumericArgs(4) > 300000.0d0 .OR. rNumericArgs(4) < 0.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError(TRIM(cNumericFieldNames(4))//' is beyond the range.')
        CALL ShowContinueError('The input value is '//TRIM(TrimSigDigits(rNumericArgs(4),0))// &
                               '. The allowed value must be between 0 and 300000 J/kg')
        ErrorsFound = .TRUE.
      END IF
    END IF
    IF (NumNumbers > 4) THEN
      HybridVentSysAvailMgrData(SysAvailNum)%MaxOutdoorEnth = rNumericArgs(5)
      IF (rNumericArgs(5) > 300000.0d0 .OR. rNumericArgs(5) < 0.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError(TRIM(cNumericFieldNames(5))//' is beyond the range.')
        CALL ShowContinueError('The input value is '//TRIM(TrimSigDigits(rNumericArgs(5),0))// &
                               '. The allowed value must be between 0 and 300000 J/kg')
        ErrorsFound = .TRUE.
      END IF
    END IF
    ! Ensure MaxEnth >= MiniEnth
    IF (rNumericArgs(4) >= rNumericArgs(5)) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '" The '//TRIM(cNumericFieldNames(4))//' must be less than the '//  &
         TRIM(cNumericFieldNames(5)))
      CALL ShowContinueError('The '//TRIM(cNumericFieldNames(4))//' is '//TRIM(TrimSigDigits(rNumericArgs(4),0))// &
                             '. The '//TRIM(cNumericFieldNames(5))//' is '//TRIM(TrimSigDigits(rNumericArgs(5),0))//'.')
      ErrorsFound = .TRUE.
    END IF

    ! Read Max and Min outdoor dew point
    IF (NumNumbers > 5) THEN
      HybridVentSysAvailMgrData(SysAvailNum)%MinOutdoorDewPoint = rNumericArgs(6)
      IF (rNumericArgs(6) > 100.0d0 .OR. rNumericArgs(6) < -100.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError(TRIM(cNumericFieldNames(6))//' is beyond the range.')
        CALL ShowContinueError('The input value is '//TRIM(TrimSigDigits(rNumericArgs(6),0))// &
                               '. The allowed value must be between -100 C and +100 C')
        ErrorsFound = .TRUE.
      END IF
    END IF
    IF (NumNumbers > 6) THEN
      HybridVentSysAvailMgrData(SysAvailNum)%MaxOutdoorDewPoint = rNumericArgs(7)
      IF (rNumericArgs(7) > 100.0d0 .OR. rNumericArgs(7) < -100.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError(TRIM(cNumericFieldNames(7))//' is beyond the range.')
        CALL ShowContinueError('The input value is '//TRIM(TrimSigDigits(rNumericArgs(7),0))// &
                               '. The allowed value must be between -100 C and +100 C')
        ErrorsFound = .TRUE.
      END IF
    END IF
    ! Ensure MaxTemp >= MinTemp
    IF (rNumericArgs(6) >= rNumericArgs(7)) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '" The '//TRIM(cNumericFieldNames(6))//' must be less than the '//  &
         TRIM(cNumericFieldNames(7)))
      CALL ShowContinueError('The '//TRIM(cNumericFieldNames(6))//' is '//TRIM(TrimSigDigits(rNumericArgs(6),0))// &
                             '. The '//TRIM(cNumericFieldNames(7))//' is '//TRIM(TrimSigDigits(rNumericArgs(7),0))//'.')
      ErrorsFound = .TRUE.
    END IF

    HybridVentSysAvailMgrData(SysAvailNum)%MinOASched = cAlphaArgs(6)
    HybridVentSysAvailMgrData(SysAvailNum)%MinOASchedPtr = GetScheduleIndex(cAlphaArgs(6))
    IF (HybridVentSysAvailMgrData(SysAvailNum)%MinOASchedPtr .EQ. 0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid')
      CALL ShowContinueError('..not found: '//TRIM(cAlphaFieldNames(6))//'="'//TRIM(cAlphaArgs(6))//'".')
      ErrorsFound = .TRUE.
    END IF
    SchedMin=GetScheduleMinValue(HybridVentSysAvailMgrData(SysAvailNum)%MinOASchedPtr)
    IF (SchedMin < 0.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="' //TRIM(cAlphaArgs(1))// &
                           '", Schedule value must be >= 0 in '// &
                            TRIM(cAlphaFieldNames(6))//'="'//TRIM(cAlphaArgs(6))//'".')
      CALL ShowContinueError('The minimum schedule value is '//TRIM(TrimSigDigits(SchedMin,1)))
      ErrorsFound = .TRUE.
    END IF

    IF (.NOT. lAlphaFieldBlanks(7)) THEN
      HybridVentSysAvailMgrData(SysAvailNum)%OpeningFactorFWS = GetCurveIndex(cAlphaArgs(7))
      IF (HybridVentSysAvailMgrData(SysAvailNum)%OpeningFactorFWS .LE. 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError(' not found: '//TRIM(cAlphaFieldNames(7))//'="'//TRIM(cAlphaArgs(7))//'".')
        ErrorsFound = .TRUE.
      ELSE
        CALL GetCurveMinMaxValues(HybridVentSysAvailMgrData(SysAvailNum)%OpeningFactorFWS,CurveMin,CurveMax)
        IF (CurveMin < 0.0d0) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
          CALL ShowContinueError('The minimum wind speed used in '//TRIM(cAlphaFieldNames(7))//'="'//TRIM(cAlphaArgs(7))// &
                           'should be greater than or equal to 0.0 (m/s)')
          CALL ShowContinueError('Curve minimum value appears to be less than 0.')
          ErrorsFound = .TRUE.
        END IF
        CurveVal = CurveValue(HybridVentSysAvailMgrData(SysAvailNum)%OpeningFactorFWS,CurveMin)
        IF(CurveVal .LT. 0.0d0)THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
          CALL ShowContinueError('The minimum value of '//TRIM(cAlphaFieldNames(7))//' must be greater ' &
                                 //'than or equal to 0.0 at the minimum value of wind speed.')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(7))//'="'//TRIM(cAlphaArgs(7))//'".')
          CALL ShowContinueError('Curve output at the minimum wind speed = '//TRIM(TrimSigDigits(CurveVal,3)))
          ErrorsFound = .TRUE.
        END IF
        CurveVal = CurveValue(HybridVentSysAvailMgrData(SysAvailNum)%OpeningFactorFWS,CurveMax)
        IF(CurveVal .GT. 1.0d0)THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
          CALL ShowContinueError('The maximum value of '//TRIM(cAlphaFieldNames(7))//' must be less ' &
                                 //'than or equal to 1.0 at the maximum value of wind speed.')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(7))//'="'//TRIM(cAlphaArgs(7))//'".')
          CALL ShowContinueError('Curve output at the maximum wind speed = '//TRIM(TrimSigDigits(CurveVal,3)))
          ErrorsFound = .TRUE.
        END IF
        ! Check curve type
        SELECT CASE(GetCurveType(HybridVentSysAvailMgrData(SysAvailNum)%OpeningFactorFWS))

          CASE('QUADRATIC')
          CASE('LINEAR')

          CASE DEFAULT
            CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
            CALL ShowContinueError('Illegal curve type for '//TRIM(cAlphaFieldNames(7))//'="'//TRIM(cAlphaArgs(7))//'".')
            ErrorsFound = .TRUE.
        END SELECT
     END IF
    END IF

    HybridVentSysAvailMgrData(SysAvailNum)%ANControlTypeSchedPtr = GetScheduleIndex(cAlphaArgs(8))
    IF (HybridVentSysAvailMgrData(SysAvailNum)%ANControlTypeSchedPtr > 0) THEN
      HybridVentSysAvailMaster(SysAvailNum) = HybridVentSysAvailMgrData(SysAvailNum)%ActualZoneNum
      ! Check schedule values
      SchedMin=GetScheduleMinValue(HybridVentSysAvailMgrData(SysAvailNum)%ANControlTypeSchedPtr)
      SchedMax=GetScheduleMaxValue(HybridVentSysAvailMgrData(SysAvailNum)%ANControlTypeSchedPtr)
      HybridVentSysAvailANCtrlStatus(SysAvailNum) = HybridVentSysAvailMgrData(SysAvailNum)%ANControlTypeSchedPtr
      IF (SchedMax > 1.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError(' For '//TRIM(cAlphaFieldNames(8))//'="'//TRIM(cAlphaArgs(8))//'",')
        CALL ShowContinueError('the maximum schedule value should be 1. However, ')
        CALL ShowContinueError('the maximum entered value in the schedule is '//TRIM(TrimSigDigits(SchedMax,1)))
        ErrorsFound = .TRUE.
      END IF
      IF (SchedMin < 0.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError('For '//TRIM(cAlphaFieldNames(8))//'="'//TRIM(cAlphaArgs(8))//'",')
        CALL ShowContinueError('the minimum schedule value should be 0. However, ')
        CALL ShowContinueError('the minimum entered value in the schedule is '//TRIM(TrimSigDigits(SchedMin,1)))
        ErrorsFound = .TRUE.
      END IF
    END IF

    HybridVentSysAvailMgrData(SysAvailNum)%SimpleControlTypeSchedPtr = GetScheduleIndex(cAlphaArgs(9))
    IF (HybridVentSysAvailMgrData(SysAvailNum)%SimpleControlTypeSchedPtr > 0 .AND. &
        HybridVentSysAvailMgrData(SysAvailNum)%ANControlTypeSchedPtr > 0) THEN
      CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
      CALL ShowContinueError('The inputs for'//TRIM(cAlphaFieldNames(8))// &
                            ' and '//TRIM(cAlphaFieldNames(9))//' are valid.')
      CALL ShowContinueError('But both objects cannot work at the same time. The Simple Airflow Control is disabled')
      HybridVentSysAvailMgrData(SysAvailNum)%SimpleControlTypeSchedPtr = 0
    ELSE IF (HybridVentSysAvailMgrData(SysAvailNum)%SimpleControlTypeSchedPtr > 0) THEN
      ! Check schedule values
      SchedMin=GetScheduleMinValue(HybridVentSysAvailMgrData(SysAvailNum)%SimpleControlTypeSchedPtr)
      SchedMax=GetScheduleMaxValue(HybridVentSysAvailMgrData(SysAvailNum)%SimpleControlTypeSchedPtr)
      IF (SchedMax > 1.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError('For '//TRIM(cAlphaFieldNames(9))//'="'//TRIM(cAlphaArgs(9))//'",')
        CALL ShowContinueError('the maximum schedule value should be 1. However, ')
        CALL ShowContinueError('the maximum entered value in the schedule is '//TRIM(TrimSigDigits(SchedMax,1)))
        ErrorsFound = .TRUE.
      END IF
      IF (SchedMin < 0.0d0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError('For '//TRIM(cAlphaFieldNames(9))//'="'//TRIM(cAlphaArgs(9))//'",')
        CALL ShowContinueError('the minimum schedule value should be 0. However, ')
        CALL ShowContinueError('the minimum entered value in the schedule is '//TRIM(TrimSigDigits(SchedMin,1)))
        ErrorsFound = .TRUE.
      END IF
    END IF

    IF (HybridVentSysAvailMgrData(SysAvailNum)%SimpleControlTypeSchedPtr > 0) THEN
      HybridVentSysAvailMgrData(SysAvailNum)%VentilationName = cAlphaArgs(10)
      If (TotVentilation .GT. 0) Then
        HybridVentSysAvailMgrData(SysAvailNum)%VentilationPtr = FindItemInList(cAlphaArgs(10),Ventilation%Name,TotVentilation)
        HybridVentSysAvailMaster(SysAvailNum) = HybridVentSysAvailMgrData(SysAvailNum)%VentilationPtr
        SchedMax=GetScheduleMaxValue(HybridVentSysAvailMgrData(SysAvailNum)%SimpleControlTypeSchedPtr)
        IF (HybridVentSysAvailMgrData(SysAvailNum)%VentilationPtr .LE. 0 .AND. INT(SchedMax) == 1) THEN
          CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(10))//'="'//TRIM(cAlphaArgs(10))//'" is required and not found.')
          ErrorsFound = .TRUE.
        End If ! Otherwise check later
      END IF
    END IF

    ! Check simple airflow object
    IF (HybridVentSysAvailMgrData(SysAvailNum)%SimpleControlTypeSchedPtr > 0 .AND. &
        HybridVentSysAvailMgrData(SysAvailNum)%VentilationPtr > 0) THEN
      IF (HybridVentSysAvailMgrData(SysAvailNum)%ActualZoneNum .NE. &
          Ventilation(HybridVentSysAvailMgrData(SysAvailNum)%VentilationPtr)%ZonePtr) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError('The Zone name specified in the Ventilation ' &
             //'object '//TRIM(Zone(Ventilation(HybridVentSysAvailMgrData(SysAvailNum)%VentilationPtr)%ZonePtr)%Name))
        CALL ShowContinueError('is not equal to the '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
        ErrorsFound = .TRUE.
      END IF
    END IF

    IF (HybridVentSysAvailMgrData(SysAvailNum)%SimpleControlTypeSchedPtr > 0 .AND. &
        SimulateAirflowNetwork > AirflowNetworkControlSimple) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(HybridVentSysAvailMgrData(SysAvailNum)%Name)//'"')
      CALL ShowContinueError('The simple airflow objects are used for natural ventilation calculation.')
      CALL ShowContinueError('The Airflow Network model is not allowed to perform. Please set the control type = ' &
           //'NoMultizoneOrDistribution')
      ErrorsFound = .TRUE.
    END IF

    IF (HybridVentSysAvailMgrData(SysAvailNum)%SimpleControlTypeSchedPtr == 0) THEN
      IF (SimulateAirflowNetwork <= AirflowNetworkControlSimple) THEN
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(HybridVentSysAvailMgrData(SysAvailNum)%Name)//'"')
        CALL ShowContinueError ('The Airflow Network model is not available for Hybrid Ventilation Control.')
      ELSE IF (SimulateAirflowNetwork == AirflowNetworkControlSimpleADS) THEN
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(HybridVentSysAvailMgrData(SysAvailNum)%Name)//'"')
        CALL ShowContinueError('Please check the AirflowNetwork Control field in the AirflowNetwork:SimulationControl object.')
        CALL ShowContinueError('The suggested choices are MultizoneWithDistribution or MultizoneWithoutDistribution.')
      END IF
    END IF

    ! Disallow combination of simple control and OA control mode
    SchedMax=GetScheduleMaxValue(HybridVentSysAvailMgrData(SysAvailNum)%ControlModeSchedPtr)
    IF (HybridVentSysAvailMgrData(SysAvailNum)%SimpleControlTypeSchedPtr > 0 .AND. SchedMax .EQ. 4.0d0) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
      CALL ShowContinueError('The outdoor ventilation air control type defined in '// TRIM(cAlphaArgs(4))//    &
                        ' cannot work together with '//TRIM(cAlphaFieldNames(9)))
      ErrorsFound = .TRUE.
    END IF

  END DO ! SysAvailNum

  IF (NumHybridVentSysAvailMgrs > 1) THEN
    DO SysAvailNum = 2,NumHybridVentSysAvailMgrs
      IF (HybridVentSysAvailMgrData(SysAvailNum-1)%ANControlTypeSchedPtr > 0) THEN
        IF (HybridVentSysAvailMgrData(SysAvailNum)%SimpleControlTypeSchedPtr > 0) THEN
          CALL ShowSevereError('The AirflowNetwork model is used for natural ventilation calculation in ' &
             //TRIM(cCurrentModuleObject)//'="' //TRIM(HybridVentSysAvailMgrData(SysAvailNum-1)%Name)//'"')
          CALL ShowContinueError('The simple airflow objects are used for natural ventilation calculation in ' &
             //TRIM(cCurrentModuleObject)//'="' //TRIM(HybridVentSysAvailMgrData(SysAvailNum)%Name)//'"')
          CALL ShowContinueError('The hybrid ventilation control requires the same models to calculate natural ventilation')
          ErrorsFound = .TRUE.
        END IF
     END IF
      IF (HybridVentSysAvailMgrData(SysAvailNum-1)%SimpleControlTypeSchedPtr > 0) THEN
        IF (HybridVentSysAvailMgrData(SysAvailNum)%ANControlTypeSchedPtr > 0) THEN
          CALL ShowSevereError('The Airflow Network model is used for natural ventilation calculation in ' &
             //TRIM(cCurrentModuleObject)//'="' //TRIM(HybridVentSysAvailMgrData(SysAvailNum)%Name)//'"')
          CALL ShowContinueError('The simple airflow objects are used for natural ventilation calculation in ' &
             //TRIM(cCurrentModuleObject)//'="' //TRIM(HybridVentSysAvailMgrData(SysAvailNum-1)%Name)//'"')
          CALL ShowContinueError('The hybrid ventilation control requires the same models to calculate natural ventilation')
          ErrorsFound = .TRUE.
        END IF
     END IF
    END DO ! SysAvailNum
  END IF

  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Errors found in input.  Preceding condition(s) cause termination.')
  END IF

  ! Set up output variables
  DO SysAvailNum = 1,NumHybridVentSysAvailMgrs
    IF (HybridVentSysAvailMgrData(SysAvailNum)%HybridVentMgrConnectedToAirLoop) THEN
      CALL SetupOutputVariable('Availability Manager Hybrid Ventilation Control Status []', &
                                 HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl, &
                                'System','Average',HybridVentSysAvailMgrData(SysAvailNum)%AirLoopName)
      CALL SetupOutputVariable('Availability Manager Hybrid Ventilation Control Mode []', &
                                 HybridVentSysAvailMgrData(SysAvailNum)%ControlMode, &
                                'System','Average',HybridVentSysAvailMgrData(SysAvailNum)%AirLoopName)
    ELSE
      CALL SetupOutputVariable('Availability Manager Hybrid Ventilation Control Status []', &
                                 HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl, &
                                'System','Average',HybridVentSysAvailMgrData(SysAvailNum)%ControlZoneName)
      CALL SetupOutputVariable('Availability Manager Hybrid Ventilation Control Mode []', &
                                 HybridVentSysAvailMgrData(SysAvailNum)%ControlMode, &
                                'System','Average',HybridVentSysAvailMgrData(SysAvailNum)%ControlZoneName)
    ENDIF
  END DO

  RETURN

END SUBROUTINE GetHybridVentilationInputs

SUBROUTINE InitHybridVentSysAvailMgr

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   March 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Hybrid Ventilation Control System Availability Manager

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: ZoneEquipConfig, NumValidSysAvailZoneComponents
  USE InputProcessor, ONLY : SameString
  USE DataHeatBalance,  ONLY: TotVentilation, Ventilation
  USE InputProcessor,   ONLY: FindIteminList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! NA

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE  :: MyOneTimeFlag = .TRUE.    ! One time flag
  INTEGER       :: SysAvailNum               ! DO loop index for Sys Avail Manager objects
  INTEGER       :: ControlledZoneNum         ! Index into the ZoneEquipConfig array
  LOGICAL       :: ErrorsFound = .FALSE.     ! Set to true if errors in input, fatal at end of routine
  INTEGER       :: AirLoopNum                ! Air loop number
  INTEGER       :: ControlMode               ! Hybrid control mode
  INTEGER       :: AirLoopCount              ! Air loop name count
  REAL(r64)     :: SchedMax                  ! Maximum value specified in a schedule
  INTEGER       :: SysAvailIndex             ! Hybrid Ventilation Sys Avail Manager index
  INTEGER       :: ZoneEquipType
  INTEGER       :: HybridVentNum

  ! One time initializations
  IF (MyOneTimeFlag .AND. ALLOCATED(ZoneEquipConfig) .AND. ALLOCATED(PrimaryAirSystem)) THEN

    ! Ensure the controlled zone is listed and defined in an HVAC Air Loop
    DO SysAvailNum = 1,NumHybridVentSysAvailMgrs
      IF (HybridVentSysAvailMgrData(SysAvailNum)%SimpleControlTypeSchedPtr > 0 .AND. TotVentilation > 0 .AND. &
          HybridVentSysAvailMgrData(SysAvailNum)%VentilationPtr .EQ. 0 ) THEN
        HybridVentSysAvailMgrData(SysAvailNum)%VentilationPtr = &
          FindItemInList(HybridVentSysAvailMgrData(SysAvailNum)%VentilationName,Ventilation%Name,TotVentilation)
        HybridVentSysAvailMaster(SysAvailNum) = HybridVentSysAvailMgrData(SysAvailNum)%VentilationPtr
        SchedMax=GetScheduleMaxValue(HybridVentSysAvailMgrData(SysAvailNum)%SimpleControlTypeSchedPtr)
        IF (HybridVentSysAvailMgrData(SysAvailNum)%VentilationPtr .LE. 0 .AND. INT(SchedMax) == 1) THEN
          CALL ShowSevereError(TRIM('ZoneVentilation Object Name')//'="'// &
             TRIM(HybridVentSysAvailMgrData(SysAvailNum)%VentilationName)//'" is required and not found.')
          CALL ShowContinueError('Occurs in '//TRIM('AvailabilityManager:HybridVentilation')//'="' // &
            TRIM(HybridVentSysAvailMgrData(SysAvailNum)%Name)//'".')
          ErrorsFound = .TRUE.
        End If
      End If
      ! Check air loop number
      DO AirLoopNum=1,NumPrimaryAirSys  ! loop over the primary air systems
        IF (SameString(PrimaryAirSystem(AirLoopNum)%Name,HybridVentSysAvailMgrData(SysAvailNum)%AirLoopName)) THEN
          HybridVentSysAvailMgrData(SysAvailNum)%AirLoopNum = AirLoopNum
        END IF
      END DO
      HybridVentSysAvailAirLoopNum(SysAvailNum)    = HybridVentSysAvailMgrData(SysAvailNum)%AirLoopNum
      HybridVentSysAvailActualZoneNum(SysAvailNum) = HybridVentSysAvailMgrData(SysAvailNum)%ActualZoneNum

      ! set the controlled zone numbers
      DO ControlledZoneNum = 1,NumOfZones
        IF (ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum .EQ. HybridVentSysAvailMgrData(SysAvailNum)%ActualZoneNum ) THEN
          HybridVentSysAvailMgrData(SysAvailNum)%ControlledZoneNum = ControlledZoneNum
          IF (HybridVentSysAvailMgrData(SysAvailNum)%HybridVentMgrConnectedToAirLoop) THEN
            IF (HybridVentSysAvailMgrData(SysAvailNum)%ControlledZoneNum > 0) THEN
              IF (ZoneEquipConfig(ControlledZoneNum)%AirLoopNum /=HybridVentSysAvailMgrData(SysAvailNum)%AirLoopNum) THEN
                CALL ShowSevereError(TRIM(cValidSysAvailManagerTypes(HybridVentSysAvailMgrData(SysAvailNum)%MgrType))// &
                    ', The controlled zone ='// TRIM(HybridVentSysAvailMgrData(SysAvailNum)%ControlZoneName)// &
                    ' is not served by this Air Loop='//TRIM(HybridVentSysAvailMgrData(SysAvailNum)%AirLoopName))
                ErrorsFound = .TRUE.
              END IF
            END IF
            EXIT
          ENDIF
        END IF
        IF(ANY(HybridVentSysAvailMgrData%HybridVentMgrConnectedToAirLoop))THEN
          IF (ZoneEquipConfig(ControlledZoneNum)%AirLoopNum .EQ. HybridVentSysAvailMgrData(SysAvailNum)%AirLoopNum .AND. &
              HybridVentSysAvailMgrData(SysAvailNum)%AirLoopNum .GT. 0 ) THEN
            DO HybridVentNum = 1, NumHybridVentSysAvailMgrs
              IF (.NOT. HybridVentSysAvailMgrData(HybridVentNum)%HybridVentMgrConnectedToAirLoop .AND. &
                  (HybridVentNum .NE. SysAvailNum)) THEN
                IF (ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum .EQ. &
                    HybridVentSysAvailMgrData(HybridVentNum)%ActualZoneNum .AND. &
                    ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum .GT. 0) THEN
                  CALL ShowWarningError(TRIM('AvailabilityManager:HybridVentilation')//' = "' // &
                          TRIM(HybridVentSysAvailMgrData(HybridVentNum)%Name)//'" has the controlled zone name = "' // &
                          TRIM(HybridVentSysAvailMgrData(HybridVentNum)%ControlZoneName)//'".')
                  CALL ShowContinueError('This controlled zone already has hybrid ventilation control through this ' //&
                          'air loop = "'//TRIM(HybridVentSysAvailMgrData(SysAvailNum)%AirLoopName)//'".')
                  CALL ShowContinueError('Only ' // TRIM('AvailabilityManager:HybridVentilation')//' = "' // &
                          TRIM(HybridVentSysAvailMgrData(SysAvailNum)%Name)// &
                          '" will be simulated. Simulation continues...')
                ELSE
                  HybridVentSysAvailMgrData(HybridVentNum)%SimHybridVentSysAvailMgr = .TRUE.
                END IF
              ENDIF
            ENDDO
          ENDIF
        ELSE
          HybridVentSysAvailMgrData%SimHybridVentSysAvailMgr = .TRUE.
        ENDIF
      END DO

      IF (HybridVentSysAvailMgrData(SysAvailNum)%ControlledZoneNum == 0) THEN
        CALL ShowSevereError(TRIM(cValidSysAvailManagerTypes(HybridVentSysAvailMgrData(SysAvailNum)%MgrType))// &
                             ', The controlled zone is not defined' &
                           //' correctly ='//TRIM(HybridVentSysAvailMgrData(SysAvailNum)%ControlZoneName))
        ErrorsFound = .TRUE.
      END IF
    END DO

    ! Ensure an airloop name is not used more than once in the hybrid ventilation control objects
    DO AirLoopNum=1,NumPrimaryAirSys  ! loop over the primary air systems
      AirLoopCount = 0
      DO SysAvailNum = 1,NumHybridVentSysAvailMgrs
        IF (SameString(PrimaryAirSystem(AirLoopNum)%Name,HybridVentSysAvailMgrData(SysAvailNum)%AirLoopName)) THEN
          AirLoopCount = AirLoopCount+1
          IF (AirLoopCount .GT. 1) SysAvailIndex = SysAvailNum
        END IF
      END DO
      IF (AirLoopCount .GT. 1) THEN
        CALL ShowSevereError(TRIM(cValidSysAvailManagerTypes(HybridVentSysAvailMgrData(SysAvailIndex)%MgrType))// &
            ', The AirLoopHVAC name found more' &
          //' than once=' //TRIM(PrimaryAirSystem(AirLoopNum)%Name))
        CALL ShowContinueError('Each AirLoopHVAC allows one hybrid ventilation control object.')
        ErrorsFound = .TRUE.
      END IF
    END DO

    IF (ErrorsFound) THEN
      CALL ShowFatalError('Errors found in getting AvailabilityManager:* inputs')
    END IF

    MyOneTimeFlag = .FALSE.

  END IF ! end 1 time initializations

  DO SysAvailNum = 1,NumHybridVentSysAvailMgrs
    ControlMode = GetCurrentScheduleValue(HybridVentSysAvailMgrData(SysAvailNum)%ControlModeSchedPtr)
    HybridVentSysAvailMgrData(SysAvailNum)%ControlMode = ControlMode
    ! -1 means that the value will be determined inside CalcHybridVentSysAvailMgr.
    ! IF the value is still -1, the program will stop.
    HybridVentSysAvailVentCtrl(SysAvailNum) = -1
    HybridVentSysAvailWindModifier(SysAvailNum) = -1.0d0
  END DO

  IF (ALLOCATED(HybridVentSysAvailMgrData))  HybridVentSysAvailMgrData%AvailStatus  = NoAction

  DO ZoneEquipType = 1,NumValidSysAvailZoneComponents  ! loop over the zone equipment types
    IF(ALLOCATED(ZoneComp))THEN
      IF(ZoneComp(ZoneEquipType)%TotalNumComp .GT. 0) &
        ZoneComp(ZoneEquipType)%ZoneCompAvailMgrs%AvailStatus = NoAction
    ENDIF
  ENDDO
  RETURN

END SUBROUTINE InitHybridVentSysAvailMgr

SUBROUTINE CalcHybridVentSysAvailMgr(SysAvailNum,PriAirSysNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   March 2007
          !       MODIFIED       July 2012, Chandan Sharma - FSEC: Added zone sys avail managers
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set AvailStatus indicator for a primary air loop and AirflowNetwork model to prevent
          ! windows or doors open during HVAC system operation

          ! METHODOLOGY EMPLOYED:
          ! Looks at outside and indoor conditions to determine if hybrid ventilation
          ! is beneficial. If it is and it is scheduled on the AvailStatus is set to cycle
          ! on and open windows or doors.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataAirLoop
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE DataHeatBalFanSys, ONLY: TempZoneThermostatSetpoint, ZoneThermostatSetPointHi, &
                               ZoneThermostatSetPointLo, TempControlType
  USE DataEnvironment,   ONLY: OutEnthalpy, OutDewPointTemp, OutBaroPress, IsRain, OutHumRat
  USE DataHeatBalFanSys, ONLY: ZoneAirHumRat, MAT
  USE Psychrometrics,    ONLY: PsyHFnTdbW, PsyTdpFnWPb, PsyRhFnTdbWPb, PsyWFnTdbRhPb
  USE DataHeatBalance,   ONLY: Zone, TotVentilation, Ventilation, TotMixing, Mixing, HybridControlTypeIndiv, &
                               HybridControlTypeClose, HybridControlTypeGlobal
  USE DataZoneControls,  ONLY: HumidityControlZone, NumHumidityControlZones
  USE AirflowNetworkBalanceManager, ONLY: GetZoneInfilAirChangeRate, ManageAirflowNetworkBalance
  USE CurveManager,       ONLY: CurveValue
  USE DataAirflowNetwork, ONLY: SimulateAirflowNetwork,AirflowNetworkControlSimple

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SysAvailNum         ! number of the current scheduled system availability manager
  INTEGER,  OPTIONAL,  INTENT(IN)  :: PriAirSysNum        ! number of the primary air system affected by this Avail. Manager

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS:

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: ZoneNum         ! actual zone number of the control zone
  INTEGER      :: ControlMode     ! Hybrid control mode
  INTEGER      :: HstatZoneNum    ! Humidity control zone number
  REAL(r64)    :: ZoneAirEnthalpy ! Zone air enthalpy
  REAL(r64)    :: ZoneAirDewpoint ! Zone air dew point temperature
  REAL(r64)    :: ZoneAirRH       ! Zone air relative humidity
  REAL(r64)    :: TempExt         ! Outdoor dry bulb temperature at zone height
  REAL(r64)    :: WindExt         ! Outdoor wind spped at zone height
!unused  REAL(r64)    :: RHSetPoint      ! RH setpoint from a given schedule
  REAL(r64)    :: WSetPoint       ! Humidity ratio setpoint from a given RH setpoint schedule
  REAL(r64)    :: OASetPoint      ! Outdoor air setpoint from a given OA setpoint schedule
  REAL(r64)    :: ACH             ! Zone air change per hour
  LOGICAL      :: found           ! Used for humidistat object
  LOGICAL      :: HybridVentModeOA  ! USed to check whether HybridVentModeOA is allowed
  REAL(r64)    :: ZoneRHHumidifyingSetPoint   ! Zone humidifying setpoint (%)
  REAL(r64)    :: ZoneRHDehumidifyingSetPoint ! Zone dehumidifying setpoint (%)
  INTEGER      :: ControlledZoneNum           ! Index into the ZoneEquipConfig array
  INTEGER      :: SimpleControlType           ! Simple control type from a schedule: 0 individual, 1 global
  INTEGER      :: i                ! Array index

  ControlMode = HybridVentSysAvailMgrData(SysAvailNum)%ControlMode

  ZoneNum = HybridVentSysAvailMgrData(SysAvailNum)%ActualZoneNum
  HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl = HybridVentCtrl_NoAction
  TempExt = Zone(ZoneNum)%OutDryBulbTemp
  WindExt = Zone(ZoneNum)%WindSpeed


  SELECT CASE(ControlMode)

      CASE(HybridVentMode_No)
        HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl = HybridVentCtrl_NoAction

      ! Temperature control
      CASE(HybridVentMode_Temp)
        IF (TempExt >= HybridVentSysAvailMgrData(SysAvailNum)%MinOutdoorTemp .AND. &
            TempExt <= HybridVentSysAvailMgrData(SysAvailNum)%MaxOutdoorTemp) THEN
          HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl = HybridVentCtrl_Open
        ELSE
          HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl = HybridVentCtrl_Close
        END IF

      ! Enthalpy control
      CASE(HybridVentMode_Enth)
        ZoneAirEnthalpy = PsyHFnTdbW(MAT(ZoneNum),ZoneAirHumRat(ZoneNum))
        IF (OutEnthalpy >= HybridVentSysAvailMgrData(SysAvailNum)%MinOutdoorEnth .AND. &
            OutEnthalpy <= HybridVentSysAvailMgrData(SysAvailNum)%MaxOutdoorEnth) THEN
          HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl = HybridVentCtrl_Open
        ELSE
          HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl = HybridVentCtrl_Close
        END IF

      ! Dew point control
      CASE(HybridVentMode_DewPoint)
        IF (OutDewPointTemp >= HybridVentSysAvailMgrData(SysAvailNum)%MinOutdoorDewPoint .AND. &
            OutDewPointTemp <= HybridVentSysAvailMgrData(SysAvailNum)%MaxOutdoorDewPoint) THEN
          HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl = HybridVentCtrl_Open
        ELSE
          HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl = HybridVentCtrl_Close
        END IF

      CASE(HybridVentMode_OA)
        OASetPoint = GetCurrentScheduleValue(HybridVentSysAvailMgrData(SysAvailNum)%MinOASchedPtr)
        ACH=0.0d0
        HybridVentModeOA = .TRUE.
        IF(.NOT. HybridVentSysAvailMgrData(SysAvailNum)%HybridVentMgrConnectedToAirLoop) THEN
          IF (SimulateAirflowNetwork <= AirflowNetworkControlSimple) THEN
            HybridVentModeOA = .FALSE.
          ENDIF
        ENDIF

        If (HybridVentSysAvailMgrData(SysAvailNum)%ANControlTypeSchedPtr > 0 .AND. HybridVentModeOA) Then
          CALL ManageAirflowNetworkBalance(.TRUE.)
          ACH = GetZoneInfilAirChangeRate(ZoneNum)
        End If
        IF (ACH > OASetpoint) THEN
          HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl = HybridVentCtrl_Open
        ELSE
          HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl = HybridVentCtrl_Close
        END IF

      CASE DEFAULT
        CALL ShowSevereError(TRIM(cValidSysAvailManagerTypes(HybridVentSysAvailMgrData(SysAvailNum)%MgrType))// &
                 ': incorrect Control Type: '//TRIM(HybridVentSysAvailMgrData(SysAvailNum)%AirLoopName))
        CALL ShowFatalError('Errors found in getting '// &
                            TRIM(cValidSysAvailManagerTypes(HybridVentSysAvailMgrData(SysAvailNum)%MgrType))// &
                            ' Control mode value')

  END SELECT

  IF (HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl == HybridVentCtrl_Open) THEN

    ! Temperature and enthalpy control
    IF (HybridVentSysAvailMgrData(SysAvailNum)%ControlMode == HybridVentMode_Temp .OR. &
        HybridVentSysAvailMgrData(SysAvailNum)%ControlMode == HybridVentMode_Enth) THEN

      SELECT CASE(TempControlType(ZoneNum)) ! select on thermostat control

        CASE(SingleHeatingSetPoint)
          IF (MAT(ZoneNum) < TempZoneThermostatSetpoint(ZoneNum)) THEN
            HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl = HybridVentCtrl_Close
          END IF

        CASE(SingleCoolingSetPoint)
          IF (MAT(ZoneNum) > TempZoneThermostatSetpoint(ZoneNum)) THEN
            HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl = HybridVentCtrl_Close
          END IF

        CASE(SingleHeatCoolSetPoint)
          HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl = HybridVentCtrl_Close
          HybridVentSysAvailMgrData(SysAvailNum)%SingleHCErrCount = &
          HybridVentSysAvailMgrData(SysAvailNum)%SingleHCErrCount + 1
          if (HybridVentSysAvailMgrData(SysAvailNum)%SingleHCErrCount < 2) THEN
            CALL ShowWarningError('Hybrid ventilation control: ' //TRIM(HybridVentSysAvailMgrData(SysAvailNum)%AirLoopName)// &
              ': The zone temperature control type is ThermostatSetpoint:SingleHeatingOrCooling.'//  &
              ' Natural ventilation is not allowed.')
            CALL ShowContinueErrorTimeStamp(' ')
          else
            CALL ShowRecurringWarningErrorAtEnd('Hybrid ventilation control: ' &
             //TRIM(HybridVentSysAvailMgrData(SysAvailNum)%AirLoopName)//&
             ': No natural ventilation continues with a ThermostatSetpoint:SingleHeatingOrCooling type...', &
             HybridVentSysAvailMgrData(SysAvailNum)%SingleHCErrIndex, &
             REAL(HybridVentSysAvailMgrData(SysAvailNum)%ControlMode,r64),  &
             REAL(HybridVentSysAvailMgrData(SysAvailNum)%ControlMode,r64))
          END IF

        CASE(DualSetPointWithDeadBand)
          IF ((MAT(ZoneNum) < ZoneThermostatSetPointLo(ZoneNum)) .OR. &
              (MAT(ZoneNum) > ZoneThermostatSetPointHi(ZoneNum)) ) THEN
            HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl = HybridVentCtrl_Close
          END IF

        CASE DEFAULT
      END SELECT ! end select on thermostat control
    END IF

    ! Dew point control mode
    IF (HybridVentSysAvailMgrData(SysAvailNum)%ControlMode == HybridVentMode_Dewpoint) THEN
      ZoneAirRH = PsyRhFnTdbWPb(MAT(ZoneNum),ZoneAirHumRat(ZoneNum),OutBaroPress)*100.0d0
      ZoneAirDewpoint = PsyTdpFnWPb(ZoneAirHumRat(ZoneNum),OutBaroPress)
      IF (NumHumidityControlZones == 0) THEN
        HybridVentSysAvailMgrData(SysAvailNum)%DewPointNoRHErrCount = &
          HybridVentSysAvailMgrData(SysAvailNum)%DewPointNoRHErrCount + 1
        IF (HybridVentSysAvailMgrData(SysAvailNum)%DewPointNoRHErrCount < 2) THEN
          CALL ShowWarningError('Hybrid ventilation control: Dew point control mode is selected, '&
            //'but no ZoneControl:Humidistat object=' &
                                //TRIM(HybridVentSysAvailMgrData(SysAvailNum)%AirLoopName))
          CALL ShowContinueError('The hybrid ventilation control is triggered by outdoor min and max dewpoint only.')
          CALL ShowContinueError('HVAC system may turn off when outdoor dewpoint is between min and max dewpoint.')
          CALL ShowContinueErrorTimeStamp(' ')
        else
          CALL ShowRecurringWarningErrorAtEnd('Hybrid ventilation control: ' &
             //TRIM(HybridVentSysAvailMgrData(SysAvailNum)%AirLoopName)//&
             ': no ZoneControl:Humidistat object continues...', &
             HybridVentSysAvailMgrData(SysAvailNum)%DewPointNoRHErrIndex, &
             REAL(HybridVentSysAvailMgrData(SysAvailNum)%ControlMode,r64),  &
             REAL(HybridVentSysAvailMgrData(SysAvailNum)%ControlMode,r64))
        END IF
      END IF
      found = .FALSE.
      DO HstatZoneNum = 1, NumHumidityControlZones
        IF(HumidityControlZone(HstatZoneNum)%ActualZoneNum .EQ. ZoneNum) THEN
          found = .TRUE.
          ZoneRHHumidifyingSetPoint = GetCurrentScheduleValue(HumidityControlZone(HstatZoneNum)%HumidifyingSchedIndex)
          ZoneRHDehumidifyingSetPoint = GetCurrentScheduleValue(HumidityControlZone(HstatZoneNum)%DehumidifyingSchedIndex)
          IF (ZoneAirRH > ZoneRHDehumidifyingSetPoint) THEN ! Need dehumidification
            WSetPoint = PsyWFnTdbRhPb(MAT(ZoneNum),(ZoneRHDehumidifyingSetPoint / 100.0d0),OutBaroPress)
            IF (WSetPoint < OutHumRat) &
              HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl = HybridVentCtrl_Close
          ELSE IF (ZoneAirRH < ZoneRHHumidifyingSetPoint) THEN ! Need humidification
            WSetPoint = PsyWFnTdbRhPb(MAT(ZoneNum),(ZoneRHHumidifyingSetPoint / 100.0d0),OutBaroPress)
            IF (WSetPoint > OutHumRat) &
              HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl = HybridVentCtrl_Close
          ELSE
            HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl = HybridVentCtrl_Close
          END IF
        END IF
      END DO
      IF (.NOT. found .AND. NumHumidityControlZones > 0) THEN
        HybridVentSysAvailMgrData(SysAvailNum)%DewPointErrCount = &
          HybridVentSysAvailMgrData(SysAvailNum)%DewPointErrCount + 1
        IF (HybridVentSysAvailMgrData(SysAvailNum)%DewPointErrCount < 2) THEN
          CALL ShowWarningError('Hybrid ventilation control: The zone for dew point control mode is different from ' &
            //'the zone for ZoneControl:Humidistat=' &
                                //TRIM(HybridVentSysAvailMgrData(SysAvailNum)%AirLoopName))
          CALL ShowContinueError('The Zone name for hybrid control is '//Trim(Zone(ZoneNum)%Name)//'. Humidistat has no impact')
          CALL ShowContinueError('HVAC system may turn off when outdoor dewpoint is between min and max dewpoint.')
          CALL ShowContinueErrorTimeStamp(' ')
        else
          CALL ShowRecurringWarningErrorAtEnd('Hybrid ventilation control: ' &
            //TRIM(HybridVentSysAvailMgrData(SysAvailNum)%AirLoopName)//&
             ' No humidistat control impact continues...', &
             HybridVentSysAvailMgrData(SysAvailNum)%DewPointErrIndex, &
             REAL(HybridVentSysAvailMgrData(SysAvailNum)%ControlMode,r64),   &
             REAL(HybridVentSysAvailMgrData(SysAvailNum)%ControlMode,r64))
        END IF
      END IF
    END IF

    ! Outdoor ventilation air control mode
    IF (HybridVentSysAvailMgrData(SysAvailNum)%ControlMode == HybridVentMode_OA) THEN

    END IF
  END IF

  IF (WindExt > HybridVentSysAvailMgrData(SysAvailNum)%MaxWindSpeed) THEN
    HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl = HybridVentCtrl_Close
  END IF

  IF (IsRain .AND. HybridVentSysAvailMgrData(SysAvailNum)%UseRainIndicator) THEN
    HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl = HybridVentCtrl_Close
  END IF
  ! Sent a signal to the AirflowNetwork to ensure large onpenings are close or open based on this logic
  HybridVentSysAvailVentCtrl(SysAvailNum) = HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl
  IF (HybridVentSysAvailVentCtrl(SysAvailNum) < 0) THEN
  ! Fatal error
    CALL ShowFatalError('Hybrid ventilation control: the ventilation control status is beyond the range. ' &
      //'Please check input of control mode schedule')
  END IF

  IF (HybridVentSysAvailMgrData(SysAvailNum)%HybridVentMgrConnectedToAirLoop) THEN
    IF (HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl == HybridVentCtrl_Close) THEN
      PriAirSysAvailMgr(PriAirSysNum)%AvailStatus = CycleOn
    END IF
  ENDIF

  IF (HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl == HybridVentCtrl_Open .AND. &
      HybridVentSysAvailMgrData(SysAvailNum)%ANControlTypeSchedPtr > 0 .AND. &
      HybridVentSysAvailMgrData(SysAvailNum)%OpeningFactorFWS .GT. 0) THEN
    HybridVentSysAvailWindModifier(SysAvailNum) =  &
      CurveValue(HybridVentSysAvailMgrData(SysAvailNum)%OpeningFactorFWS,WindExt)
  END IF

  ! Set up flags to control simple airflow objects
  IF (HybridVentSysAvailMgrData(SysAvailNum)%AirLoopNum > 0 .AND. &
      HybridVentSysAvailMgrData(SysAvailNum)%SimpleControlTypeSchedPtr > 0) THEN
    SimpleControlType = GetCurrentScheduleValue(HybridVentSysAvailMgrData(SysAvailNum)%SimpleControlTypeSchedPtr)
    DO ControlledZoneNum = 1,NumOfZones
      IF (HybridVentSysAvailMgrData(SysAvailNum)%AirLoopNum == ZoneEquipConfig(ControlledZoneNum)%AirLoopNum) THEN
        ! Setup flag for ventilation objects
        DO i=1,TotVentilation
          IF (Ventilation(i)%ZonePtr == ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum) THEN
            Ventilation(i)%HybridControlType=HybridControlTypeIndiv
            IF (HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl == HybridVentCtrl_Close) THEN
              Ventilation(i)%HybridControlType = HybridControlTypeClose
            ELSE
              IF (SimpleControlType == 1) THEN
                Ventilation(i)%HybridControlType=HybridControlTypeGlobal
                Ventilation(i)%HybridControlMasterNum=HybridVentSysAvailMgrData(SysAvailNum)%VentilationPtr
              END IF
            END IF
          END IF
        END DO
        ! Setup flag for Mixing objects
        DO i=1,TotMixing
          IF (Mixing(i)%ZonePtr == ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum) THEN
            Mixing(i)%HybridControlType=HybridControlTypeIndiv
            IF (HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl == HybridVentCtrl_Close) THEN
              Mixing(i)%HybridControlType = HybridControlTypeClose
            ELSE
              IF (SimpleControlType == 1) THEN
                Mixing(i)%HybridControlType=HybridControlTypeGlobal
                Mixing(i)%HybridControlMasterNum=HybridVentSysAvailMgrData(SysAvailNum)%VentilationPtr
              END IF
            END IF
          END IF
        END DO
      END IF
    END DO
  ELSEIF(HybridVentSysAvailMgrData(SysAvailNum)%SimpleControlTypeSchedPtr > 0) THEN
    SimpleControlType = GetCurrentScheduleValue(HybridVentSysAvailMgrData(SysAvailNum)%SimpleControlTypeSchedPtr)
    ! Hybrid ventilation manager is applied to zone component
    ! setup flag for ventilation objects
    DO i=1,TotVentilation
      IF (Ventilation(i)%ZonePtr == HybridVentSysAvailMgrData(SysAvailNum)%ActualZoneNum) THEN
        Ventilation(i)%HybridControlType=HybridControlTypeIndiv
        IF (HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl == HybridVentCtrl_Close) THEN
          Ventilation(i)%HybridControlType = HybridControlTypeClose
        ELSE
          IF (SimpleControlType == 1) THEN
            Ventilation(i)%HybridControlType=HybridControlTypeGlobal
            Ventilation(i)%HybridControlMasterNum=HybridVentSysAvailMgrData(SysAvailNum)%VentilationPtr
          END IF
        END IF
      END IF
    END DO
    ! Setup flag for Mixing objects
    DO i=1,TotMixing
      IF (Mixing(i)%ZonePtr == HybridVentSysAvailMgrData(SysAvailNum)%ActualZoneNum) THEN
        Mixing(i)%HybridControlType=HybridControlTypeIndiv
        IF (HybridVentSysAvailMgrData(SysAvailNum)%VentilationCtrl == HybridVentCtrl_Close) THEN
          Mixing(i)%HybridControlType = HybridControlTypeClose
        ELSE
          IF (SimpleControlType == 1) THEN
            Mixing(i)%HybridControlType=HybridControlTypeGlobal
            Mixing(i)%HybridControlMasterNum=HybridVentSysAvailMgrData(SysAvailNum)%VentilationPtr
          END IF
        END IF
      END IF
    END DO
  END IF

  RETURN

END SUBROUTINE CalcHybridVentSysAvailMgr

FUNCTION GetHybridVentilationControlStatus(ZoneNum) Result(VentControl)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   July 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine was designed to find whether this zone is controlled by hybrid ventilation
          ! ventilation control option.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)         :: ZoneNum              ! Index of zone
  INTEGER :: SysAvailNum   ! index to system availability manager number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL :: VentControl         ! Set to true if ventilation control in the same zone

  ! Obtains inputs of hybrid ventilation objects
  IF (GetHybridInputFlag) THEN  !First time subroutine has been entered
    CALL GetHybridVentilationInputs
    GetHybridInputFlag=.false.
  End If

  VentControl = .FALSE.

  DO SysAvailNum = 1,NumHybridVentSysAvailMgrs
    If (HybridVentSysAvailMgrData(SysAvailNum)%ActualZoneNum == ZoneNum) Then
      If (HybridVentSysAvailMgrData(SysAvailNum)%SimpleControlTypeSchedPtr > 0) Then
        VentControl = .TRUE.
      End If
    End If
  End Do

  RETURN

END FUNCTION GetHybridVentilationControlStatus

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

END MODULE SystemAvailabilityManager
