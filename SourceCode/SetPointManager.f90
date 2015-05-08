MODULE SetPointManager

  ! Module containing the SetPoint Manager routines

  ! MODULE INFORMATION:
  !       AUTHOR         Fred Buhl
  !       DATE WRITTEN   July 1998
  !       MODIFIED       Shirey/Raustad (FSEC), Jan 2004
  !                      Nov 2004 - Jan 2005 M. J. Witte, GARD Analytics, Inc.
  !                        Add new setpoint managers:
  !                          SET POINT MANAGER:SINGLE ZONE HEATING and
  !                          SET POINT MANAGER:SINGLE ZONE COOLING
  !                          SET POINT MANAGER:OUTSIDE AIR PRETREAT
  !                        Work supported by ASHRAE research project 1254-RP
  !                      Phil Haves Oct 2004
  !                      B. Griffith Aug. 2006.
  !                      R. Raustad - FSEC: added AllSetPtMgr used for node conflict checks
  !                      July 2010 B.A. Nigusse, FSEC/UCF
  !                        Added new setpoint managers:
  !                          SetpointManager:MultiZone:Heating:Average
  !                          SetpointManager:MultiZone:Cooling:Average
  !                          SetpointManager:MultiZone:MinimumHumidity:Average
  !                          SetpointManager:MultiZone:MaximumHumidity:Average
  !                       22Aug2010 Craig Wray - added Fan:ComponentModel
  !                      Aug 2010 B.A. Nigusse, FSEC/UCF
  !                        Added new setpoint managers:
  !                          SetpointManager:MultiZone:Humidity:Minimum
  !                          SetpointManager:MultiZone:Humidity:Maximum
  !                      July 2011 Chandan Sharma, FSEC/UCF
  !                        Added new setpoint managers:
  !                          SetpointManager:FollowOutdoorAirTemperature
  !                          SetpointManager:FollowSystemNodeTemperature
  !                          SetpointManager:FollowGroundTemperature
  !                      March 2012, Atefe Makhmalbaf and Heejin Cho, PNNL
  !                        Added new setpoint manager:
  !                          SetpointManager:CondenserEnteringReset
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and algorithms required to
  ! determine all the controller setpoints in the problem.

  ! METHODOLOGY EMPLOYED:
  ! Previous time step node data will be used, in a set of fixed, precoded algorithms,
  ! to determine the current time step's controller setpoints.

  ! REFERENCES:


  ! OTHER NOTES:
  !

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataLoopNode
USE DataAirLoop
USE DataGlobals,     ONLY: BeginEnvrnFlag, BeginDayFlag, NumOfZones, MaxNameLength, &
                           MetersHaveBeenInitialized, RunOptCondEntTemp
USE DataInterfaces,  ONLY: ShowFatalError, ShowSevereError, ShowContinueError, ShowWarningError, &
                           SetupOutputVariable, GetInternalVariableValue
USE DataEnvironment, ONLY: OutDryBulbTemp, OutWetBulbTemp, OutBaroPress, OutHumRat
USE ScheduleManager
USE DataHVACGlobals, ONLY: NumPrimaryAirSys
USE CurveManager

  ! USE STATEMENTS
USE Psychrometrics, ONLY:PsyHFnTdbW,PsyCpAirFnWTdb

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

!MODULE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: MaxTemp   = 1
INTEGER, PARAMETER :: MinTemp   = 2
INTEGER, PARAMETER :: TempFirst = 1
INTEGER, PARAMETER :: FlowFirst = 2
INTEGER, PARAMETER :: iRefTempType_WetBulb = 1
INTEGER, PARAMETER :: iRefTempType_DryBulb = 2
INTEGER, PARAMETER :: iRefGroundTempObjType_BuildingSurface = 1
INTEGER, PARAMETER :: iRefGroundTempObjType_Shallow = 2
INTEGER, PARAMETER :: iRefGroundTempObjType_Deep = 3
INTEGER, PARAMETER :: iRefGroundTempObjType_FCfactorMethod = 4

! following are used to reduce string comparisons related to CtrlVarType
INTEGER, PUBLIC, PARAMETER :: iCtrlVarType_Temp = 1  ! control type 'Temperature'
INTEGER, PARAMETER :: iCtrlVarType_MaxTemp      = 2  ! control type 'MaximumTemperature'
INTEGER, PARAMETER :: iCtrlVarType_MinTemp      = 3  ! control type 'MinimumTemperature'
INTEGER, PUBLIC, PARAMETER :: iCtrlVarType_HumRat  = 4  ! control Type 'HumidityRatio'
INTEGER, PUBLIC, PARAMETER :: iCtrlVarType_MaxHumRat    = 5  ! control Type 'MaximumHumidityRatio'
INTEGER, PARAMETER :: iCtrlVarType_MinHumRat    = 6  ! control Type 'MinimumHumidityRatio'
INTEGER, PARAMETER :: iCtrlVarType_MassFlow     = 7  ! control type 'MassFlowRate'
INTEGER, PARAMETER :: iCtrlVarType_MaxMassFlow  = 8  ! control Type 'MaximumMassFlowRate'
INTEGER, PARAMETER :: iCtrlVarType_MinMassFlow  = 9  ! control Type 'MinimumMassFlowRate'

INTEGER, PARAMETER :: NumValidCtrlTypes = 9
CHARACTER(len=*), PARAMETER, DIMENSION(NumValidCtrlTypes) :: cValidCtrlTypes =    &
                 (/'Temperature           ',  &
                   'MaximumTemperature    ',  &
                   'MinimumTemperature    ',  &
                   'HumidityRatio         ',  &
                   'MaximumHumidityRatio  ',  &
                   'MinimumHumidityRatio  ',  &
                   'MassFlowRate          ',  &
                   'MaximumMassFlowRate   ',  &
                   'MinimumMassFlowRate   '/)

! following are used to reduce string comparisons related to CtrlVarType
INTEGER, PARAMETER :: iSPMType_Scheduled          = 1
INTEGER, PARAMETER :: iSPMType_ScheduledDual      = 2
INTEGER, PARAMETER :: iSPMType_OutsideAir         = 3
INTEGER, PARAMETER :: iSPMType_SZReheat           = 4
INTEGER, PARAMETER :: iSPMType_SZHeating          = 5
INTEGER, PARAMETER :: iSPMType_SZCooling          = 6
INTEGER, PARAMETER :: iSPMType_SZMinHum           = 7
INTEGER, PARAMETER :: iSPMType_SZMaxHum           = 8
INTEGER, PARAMETER :: iSPMType_MixedAir           = 9
INTEGER, PARAMETER :: iSPMType_OutsideAirPretreat = 10
INTEGER, PARAMETER :: iSPMType_Warmest            = 11
INTEGER, PARAMETER :: iSPMType_Coldest            = 12
INTEGER, PARAMETER :: iSPMType_WarmestTempFlow    = 13
INTEGER, PARAMETER :: iSPMType_RAB                = 14
INTEGER, PARAMETER :: iSPMType_MZCoolingAverage   = 15
INTEGER, PARAMETER :: iSPMType_MZHeatingAverage   = 16
INTEGER, PARAMETER :: iSPMType_MZMinHumAverage    = 17
INTEGER, PARAMETER :: iSPMType_MZMaxHumAverage    = 18
INTEGER, PARAMETER :: iSPMType_MZMinHum           = 19
INTEGER, PARAMETER :: iSPMType_MZMaxHum           = 20
INTEGER, PARAMETER :: iSPMType_FollowOATemp       = 21
INTEGER, PARAMETER :: iSPMType_FollowSysNodeTemp  = 22
INTEGER, PARAMETER :: iSPMType_GroundTemp         = 23
INTEGER, PARAMETER :: iSPMType_CondEntReset       = 24
INTEGER, PARAMETER :: iSPMType_IdealCondEntReset  = 25
INTEGER, PARAMETER :: iSPMType_SZOneStageCooling  = 26
INTEGER, PARAMETER :: iSPMType_SZOneStageHeating  = 27

INTEGER, PARAMETER :: NumValidSPMTypes = 27
CHARACTER(len=*), PARAMETER, DIMENSION(NumValidSPMTypes) :: cValidSPMTypes =    &
                 (/'SetpointManager:Scheduled                        ',  &
                   'SetpointManager:Scheduled:DualSetpoint           ',  &
                   'SetpointManager:OutdoorAirReset                  ',  &
                   'SetpointManager:SingleZone:Reheat                ',  &
                   'SetpointManager:SingleZone:Heating               ',  &
                   'SetpointManager:SingleZone:Cooling               ',  &
                   'SetpointManager:SingleZone:Humidity:Minimum      ',  &
                   'SetpointManager:SingleZone:Humidity:Maximum      ',  &
                   'SetpointManager:MixedAir                         ',  &
                   'SetpointManager:OutdoorAirPretreat               ',  &
                   'SetpointManager:Warmest                          ',  &
                   'SetpointManager:Coldest                          ',  &
                   'SetpointManager:WarmestTemperatureFlow           ',  &
                   'SetpointManager:ReturnAirBypassFlow              ',  &
                   'SetpointManager:MultiZone:Cooling:Average        ',  &
                   'SetpointManager:MultiZone:Heating:Average        ',  &
                   'SetpointManager:MultiZone:MinimumHumidity:Average',  &
                   'SetpointManager:MultiZone:MaximumHumidity:Average',  &
                   'SetpointManager:MultiZone:Humidity:Minimum       ',  &
                   'SetpointManager:MultiZone:Humidity:Maximum       ',  &
                   'SetpointManager:FollowOutdoorAirTemperature      ',  &
                   'SetpointManager:FollowSystemNodeTemperature      ',  &
                   'SetpointManager:FollowGroundTemperature          ',  &
                   'SetpointManager:CondenserEnteringReset           ',  &
                   'SetpointManager:CondenserEnteringReset:Ideal     ',  &
                   'SetpointManager:SingleZone:OneStageCooling       ',  &
                   'SetpointManager:SingleZone:OneStageHeating       '/)

!Type declarations in SetPointManager module

! This one is used for conflicting node checks and is DEALLOCATED at the end of VerifySetPointManagers
TYPE DataSetPointManager ! Derived type for all Setpoint Managers
  CHARACTER(len=MaxNameLength) :: Name          = ' ' ! name of setpoint manager
  INTEGER                      :: SPMType       = 0   ! integer representing type of setpoint manager
  INTEGER                      :: CtrlTypeMode  = 0   ! set to iCtrlVarType_xxxx
  INTEGER                      :: NumCtrlNodes  = 0   ! number of control nodes
  INTEGER, DIMENSION(:), ALLOCATABLE :: CtrlNodes     ! index to control node
  INTEGER                      :: AirLoopNum    = 0   ! index to air loop
  CHARACTER(len=MaxNameLength) :: AirLoopName   = ' ' ! name of air loop
END TYPE DataSetPointManager

TYPE DefineScheduledSetPointManager ! Derived type for Scheduled Setpoint Manager data
  CHARACTER(len=MaxNameLength) :: Name         =' '
  CHARACTER(len=MaxNameLength) :: CtrlVarType  =' '
  INTEGER                      :: CtrlTypeMode = 0 ! set to iCtrlVarType_xxxx
  CHARACTER(len=MaxNameLength) :: Sched        =' '
  INTEGER      :: SchedPtr                     =0
  INTEGER      :: NumCtrlNodes                 =0
  CHARACTER(len=MaxNameLength) :: CtrlNodeListName=' '
  INTEGER, DIMENSION(:), ALLOCATABLE      :: CtrlNodes
  REAL(r64)    :: SetPt                        =0.0d0
END TYPE DefineScheduledSetPointManager

TYPE DefineSchedDualSetPointManager ! Derived type for Scheduled Dual Setpoint Manager
  CHARACTER(len=MaxNameLength) :: Name        =' '
  CHARACTER(len=MaxNameLength) :: CtrlVarType =' '
  INTEGER                      :: CtrlTypeMode = 0 ! set to iCtrlVarType_xxxx
  CHARACTER(len=MaxNameLength) :: SchedHi     =' '
  CHARACTER(len=MaxNameLength) :: SchedLo     =' '
  INTEGER      :: SchedPtrHi                  =0
  INTEGER      :: SchedPtrLo                  =0
  INTEGER      :: NumCtrlNodes                =0
  CHARACTER(len=MaxNameLength) :: CtrlNodeListName=' '
  INTEGER, DIMENSION(:), ALLOCATABLE      :: CtrlNodes
  REAL(r64)    :: SetPtHi                     =0.0d0
  REAL(r64)    :: SetPtLo                     =0.0d0
END TYPE DefineSchedDualSetPointManager

TYPE DefineOutsideAirSetPointManager ! Derived type for Outside Air Setpoint Manager Data
  CHARACTER(len=MaxNameLength) :: Name         =' '
  CHARACTER(len=MaxNameLength) :: CtrlVarType  =' ' ! type of variable to be set
  INTEGER                      :: CtrlTypeMode =0   ! set to iCtrlVarType_xxxx
  REAL(r64)    :: OutLowSetPt1                 =0.0d0 ! 1st setpoint at outside low
  REAL(r64)    :: OutLow1                      =0.0d0 ! 1st Outside low
  REAL(r64)    :: OutHighSetPt1                =0.0d0 ! 1st setpoint at outside high
  REAL(r64)    :: OutHigh1                     =0.0d0 ! 1st Outside high
  CHARACTER(len=MaxNameLength) :: Sched        =' ' ! Optional schedule
  INTEGER      :: SchedPtr                     =0   ! Schedule index
  REAL(r64)    :: OutLowSetPt2                 =0.0d0 ! 2nd setpoint at outside low (optional)
  REAL(r64)    :: OutLow2                      =0.0d0 ! 2nd Outside low (optional)
  REAL(r64)    :: OutHighSetPt2                =0.0d0 ! 2nd setpoint at outside high (optional)
  REAL(r64)    :: OutHigh2                     =0.0d0 ! 2nd Outside high (optional)
  INTEGER      :: NumCtrlNodes                 =0
  CHARACTER(len=MaxNameLength) :: CtrlNodeListName=' '
  INTEGER, DIMENSION(:), ALLOCATABLE      :: CtrlNodes
  REAL(r64)    :: SetPt                        =0.0d0
END TYPE DefineOutsideAirSetPointManager

TYPE DefineSZReheatSetPointManager ! Derived type for the Single Zone Reheat Setpoint Manager data
  CHARACTER(len=MaxNameLength) :: Name             =' '
  CHARACTER(len=MaxNameLength) :: CtrlVarType      =' ' ! type of variable to be set
  INTEGER                      :: CtrlTypeMode     =0   ! set to iCtrlVarType_xxxx
  CHARACTER(len=MaxNameLength) :: ControlZoneName  =' ' ! name of the control zone (zone with main thermostat)
  INTEGER                      :: ControlZoneNum   =0   ! number (index into Zone array) of control zone
  INTEGER                      :: ZoneNodeNum      =0   ! zone node number
  INTEGER                      :: ZoneInletNodeNum =0   ! inlet node number for the SZRH air
  REAL(r64)                    :: MinSetTemp       =0.0d0 ! minimum supply air setpoint temperature
  REAL(r64)                    :: MaxSetTemp       =0.0d0 ! maximum supply air setpoint temperature
  INTEGER                      :: MixedAirNode     =0   ! mixed air node number
  INTEGER                      :: FanNodeIn        =0   ! fan inlet node number
  INTEGER                      :: FanNodeOut       =0   ! fan outlet node number
  INTEGER                      :: AirLoopNum       =0   ! air loop index of air loop associated with this setpoint manager
  INTEGER                      :: OAInNode         =0   ! outside airstream inlet node to the OA mixer
  INTEGER                      :: RetNode          =0   ! return node inlet to OA mixer
  INTEGER                      :: LoopInNode       =0   ! Primary Air System inlet node
  INTEGER      :: NumCtrlNodes                     =0
  INTEGER, DIMENSION(:), ALLOCATABLE      :: CtrlNodes  ! node numbers of nodes where setpoint is to be set
  REAL(r64)                    :: SetPt            =0.0d0 ! the setpoint
END TYPE DefineSZReheatSetPointManager

TYPE DefineSZHeatingSetPointManager ! Derived type for the Single Zone Heating Setpoint Manager data
  CHARACTER(len=MaxNameLength) :: Name             =' '
  CHARACTER(len=MaxNameLength) :: CtrlVarType      =' ' ! type of variable to be set
  INTEGER                      :: CtrlTypeMode     =0   ! set to iCtrlVarType_xxxx
  CHARACTER(len=MaxNameLength) :: ControlZoneName  =' ' ! name of the control zone (zone with main thermostat)
  INTEGER                      :: ControlZoneNum   =0   ! number (index into Zone array) of control zone
  INTEGER                      :: ZoneNodeNum      =0   ! zone node number
  INTEGER                      :: ZoneInletNodeNum =0   ! inlet node number for the supply air
  REAL(r64)                    :: MinSetTemp       =0.0d0 ! minimum supply air setpoint temperature
  REAL(r64)                    :: MaxSetTemp       =0.0d0 ! maximum supply air setpoint temperature
  INTEGER      :: NumCtrlNodes                     =0
  INTEGER, DIMENSION(:), ALLOCATABLE      :: CtrlNodes  ! node numbers of nodes where setpoint is to be set
  REAL(r64)                    :: SetPt            =0.0d0 ! the setpoint
END TYPE DefineSZHeatingSetPointManager

TYPE DefineSZCoolingSetPointManager ! Derived type for the Single Zone Cooling Setpoint Manager data
  CHARACTER(len=MaxNameLength) :: Name             =' '
  CHARACTER(len=MaxNameLength) :: CtrlVarType      =' ' ! type of variable to be set
  INTEGER                      :: CtrlTypeMode     =0   ! set to iCtrlVarType_xxxx
  CHARACTER(len=MaxNameLength) :: ControlZoneName  =' ' ! name of the control zone (zone with main thermostat)
  INTEGER                      :: ControlZoneNum   =0   ! number (index into Zone array) of control zone
  INTEGER                      :: ZoneNodeNum      =0   ! zone node number
  INTEGER                      :: ZoneInletNodeNum =0   ! inlet node number for the supply air
  REAL(r64)                    :: MinSetTemp       =0.0d0 ! minimum supply air setpoint temperature
  REAL(r64)                    :: MaxSetTemp       =0.0d0 ! maximum supply air setpoint temperature
  INTEGER      :: NumCtrlNodes                     =0
  INTEGER, DIMENSION(:), ALLOCATABLE      :: CtrlNodes  ! node numbers of nodes where setpoint is to be set
  REAL(r64)                    :: SetPt            =0.0d0 ! the setpoint
END TYPE DefineSZCoolingSetPointManager

TYPE DefineSZMinHumSetPointManager ! Derived Type for Single Zone Minimum Humidity Setpoint Manager data
  CHARACTER(len=MaxNameLength)   :: Name           =' '
  CHARACTER(len=MaxNameLength)   :: CtrlVarType    =' ' ! type of variable to be set
  INTEGER                        :: CtrlTypeMode   =0   ! set to iCtrlVarType_xxxx
  INTEGER                        :: NumZones       =0   ! number of zones whose humidity is being controlled
  INTEGER                        :: NumCtrlNodes   =0   ! number of nodes whose humidity ratio is being set
  INTEGER, DIMENSION(:), ALLOCATABLE :: ZoneNodes       ! zone node numbers of zones being controlled
  INTEGER, DIMENSION(:), ALLOCATABLE :: ZoneNum         ! actual zone number ( index into Zone array)
  INTEGER, DIMENSION(:), ALLOCATABLE :: CtrlZoneNum     ! index into ZoneEquipConfig
  INTEGER, DIMENSION(:), ALLOCATABLE :: CtrlNodes       ! nodes where humidity ratio is being set
  REAL(r64)                      :: SetPt          =0.0d0 ! the setpoint
END TYPE DefineSZMinHumSetPointManager

TYPE DefineSZMaxHumSetPointManager ! Derived Type for Single Zone Maximum Humidity Setpoint Manager data
  CHARACTER(len=MaxNameLength)   :: Name           =' '
  CHARACTER(len=MaxNameLength)   :: CtrlVarType    =' ' ! type of variable to be set
  INTEGER                        :: CtrlTypeMode   =0   ! set to iCtrlVarType_xxxx
  INTEGER                        :: NumZones       =0   ! number of zones whose humidity is being controlled
  INTEGER                        :: NumCtrlNodes   =0   ! number of nodes whose humidity ratio is being set
  INTEGER, DIMENSION(:), ALLOCATABLE :: ZoneNodes       ! zone node numbers of zones being controlled
  INTEGER, DIMENSION(:), ALLOCATABLE :: ZoneNum         ! actual zone number (index into Zone array)
  INTEGER, DIMENSION(:), ALLOCATABLE :: CtrlZoneNum     ! index into ZoneEquipConfig
  INTEGER, DIMENSION(:), ALLOCATABLE :: CtrlNodes       ! nodes where humidity ratio is being set
  REAL(r64)                      :: SetPt          =0.0d0 ! the setpoint
END TYPE DefineSZMaxHumSetPointManager

TYPE DefineMixedAirSetPointManager
  CHARACTER(len=MaxNameLength)   :: Name           =' '
  CHARACTER(len=MaxNameLength)   :: CtrlVarType    =' ' ! type of variable to be set
  INTEGER                        :: CtrlTypeMode   =0   ! set to iCtrlVarType_xxxx
  INTEGER                        :: RefNode        =0   ! reference node number
  INTEGER                        :: FanInNode      =0   ! supply fan inlet node number
  INTEGER                        :: FanOutNode     =0   ! Supplt fan outlet node number
  INTEGER                        :: NumCtrlNodes   =0   ! number of nodes whose humidity ratio is being set
  INTEGER, DIMENSION(:), ALLOCATABLE :: CtrlNodes       ! node numbers of nodes where setpoint is to be set
  REAL(r64)                      :: SetPt          =0.0d0 ! the setpoint
  LOGICAL                        :: MySetPointCheckFlag = .TRUE. ! used for mixed air SPM test for missing SP
END TYPE DefineMixedAirSetPointManager

TYPE DefineOAPretreatSetPointManager
  CHARACTER(len=MaxNameLength)   :: Name           =' '
  CHARACTER(len=MaxNameLength)   :: CtrlVarType    =' ' ! type of variable to be set
  INTEGER                        :: CtrlTypeMode   =0   ! set to iCtrlVarType_xxxx
  INTEGER                        :: RefNode        =0   ! reference node number
  INTEGER                        :: MixedOutNode   =0   ! mixed air outlet node number
  INTEGER                        :: OAInNode       =0   ! outside air inlet node number
  INTEGER                        :: ReturnInNode   =0   ! return air inlet node number
  REAL(r64)                      :: MinSetTemp     =0.0d0 ! minimum supply air setpoint temperature [C]
  REAL(r64)                      :: MaxSetTemp     =0.0d0 ! maximum supply air setpoint temperature [C]
  REAL(r64)                      :: MinSetHumRat   =0.0d0 ! minimum supply air setpoint humidity ratio [kg/kg]
  REAL(r64)                      :: MaxSetHumRat   =0.0d0 ! maximum supply air setpoint humidity ratio [kg/kg]
  INTEGER                        :: NumCtrlNodes   =0   ! number of nodes whose humidity ratio is being set
  INTEGER, DIMENSION(:), ALLOCATABLE :: CtrlNodes       ! node numbers of nodes where setpoint is to be set
  REAL(r64)                      :: SetPt          =0.0d0 ! the setpoint
  LOGICAL                        :: MySetPointCheckFlag = .TRUE. ! used for DOAS SPM test for missing SP
END TYPE DefineOAPretreatSetPointManager

TYPE DefineWarmestSetPointManager
  CHARACTER(len=MaxNameLength)   :: Name           =' '
  CHARACTER(len=MaxNameLength)   :: CtrlVarType    =' ' ! type of variable to be set
  INTEGER                        :: CtrlTypeMode = 0    ! set to iCtrlVarType_xxxx
  CHARACTER(len=MaxNameLength)   :: AirLoopName    =' ' ! name of air loop that will use "warmest zone" strategy
  INTEGER                        :: AirLoopNum     =0   ! index of named air loop
  REAL(r64)                      :: MinSetTemp     =0.0d0 ! minimum supply air setpoint temperature
  REAL(r64)                      :: MaxSetTemp     =0.0d0 ! maximum supply air setpoint temperature
  INTEGER                        :: Strategy       =0   ! supply flow and temperature set strategy
                                                        ! 1 = MaxTemp
  INTEGER                        :: NumCtrlNodes   =0   ! number of nodes whose temperature is being set
  INTEGER, DIMENSION(:), ALLOCATABLE :: CtrlNodes       ! nodes where temperature is being set
  REAL(r64)                      :: SetPt          =0.0d0 ! the setpoint
END TYPE DefineWarmestSetPointManager

TYPE DefineColdestSetPointManager
  CHARACTER(len=MaxNameLength)   :: Name           =' '
  CHARACTER(len=MaxNameLength)   :: CtrlVarType    =' ' ! type of variable to be set
  INTEGER                        :: CtrlTypeMode   = 0  ! set to iCtrlVarType_xxxx
  CHARACTER(len=MaxNameLength)   :: AirLoopName    =' ' ! name of air loop that will use "coldest zone" strategy
  INTEGER                        :: AirLoopNum     =0   ! index of named air loop
  REAL(r64)                      :: MinSetTemp     =0.0d0 ! minimum supply air setpoint temperature
  REAL(r64)                      :: MaxSetTemp     =0.0d0 ! maximum supply air setpoint temperature
  INTEGER                        :: Strategy       =0   ! supply flow and temperature set strategy
                                                        ! 2 = MinTemp
  INTEGER                        :: NumCtrlNodes   =0   ! number of nodes whose temperature is being set
  INTEGER, DIMENSION(:), ALLOCATABLE :: CtrlNodes       ! nodes where temperature is being set
  REAL(r64)                      :: SetPt          =0.0d0 ! the setpoint
END TYPE DefineColdestSetPointManager

TYPE DefWarmestSetPtManagerTempFlow
  CHARACTER(len=MaxNameLength)   :: Name           =' '
  CHARACTER(len=MaxNameLength)   :: CtrlVarType    =' ' ! type of variable to be set
  INTEGER                        :: CtrlTypeMode   =0   ! set to iCtrlVarType_xxxx
  CHARACTER(len=MaxNameLength)   :: AirLoopName    =' ' ! name of air loop that will use "warmest zone" strategy
  INTEGER                        :: AirLoopNum     =0   ! index of named air loop
  REAL(r64)                      :: MinSetTemp     =0.0d0 ! minimum supply air setpoint temperature
  REAL(r64)                      :: MaxSetTemp     =0.0d0 ! maximum supply air setpoint temperature
  INTEGER                        :: Strategy       =0   ! supply flow and temperature set strategy
                                                        ! 1 = TempFirst, 2 = FlowFirst
  INTEGER                        :: NumCtrlNodes   =0   ! number of nodes whose temperature is being set
  INTEGER, DIMENSION(:), ALLOCATABLE :: CtrlNodes       ! nodes where temperature is being set
  REAL(r64)                      :: SetPt          =0.0d0 ! the setpoint
  REAL(r64)                      :: MinTurndown    =0.0d0 ! minimum fractional flow rate
  REAL(r64)                      :: Turndown       =0.0d0 ! fractional flow rate
  INTEGER                        :: CritZoneNum    =0
  LOGICAL                        :: SimReady       =.false.
END TYPE DefWarmestSetPtManagerTempFlow

TYPE DefRABFlowSetPointManager
  CHARACTER(len=MaxNameLength)   :: Name           =' '
  CHARACTER(len=MaxNameLength)   :: CtrlVarType    =' ' ! type of variable to be set
  INTEGER                        :: CtrlTypeMode   = 0  ! set to iCtrlVarType_xxxx
  INTEGER                        :: NumCtrlNodes   = 0  ! number of nodes whose temperature is being set
  INTEGER, DIMENSION(:), ALLOCATABLE :: CtrlNodes       ! nodes where temperature is being set
  CHARACTER(len=MaxNameLength)   :: AirLoopName    =' ' ! name of air loop that will use "warmest zone" strategy
  INTEGER                        :: AirLoopNum     = 0  ! index of named air loop
  CHARACTER(len=MaxNameLength)   :: Sched          =' ' ! name of a schedule of supply air setpoint temperatures
  INTEGER                        :: SchedPtr       = 0  ! index of the above schedule
  REAL(r64)                      :: FlowSetPt      = 0.d0 ! mass flow rate setpoint (kg/s)
  INTEGER                        :: RABMixInNode   = 0
  INTEGER                        :: SupMixInNode   = 0
  INTEGER                        :: MixOutNode     = 0
  INTEGER                        :: RABSplitOutNode= 0
  INTEGER                        :: SysOutNode     = 0
  INTEGER                        :: AllSetPtMgrIndex = 0 ! index of RAB SP manager in AllSetPtMgr structure
END TYPE DefRABFlowSetPointManager

TYPE DefMultiZoneAverageCoolingSetPointManager ! derived type for SetpointManager:Multizone:Cooling:Average data
  CHARACTER(len=MaxNameLength)   :: Name           =' '
  CHARACTER(len=MaxNameLength)   :: CtrlVarType    =' ' ! type of variable to be set
  INTEGER                        :: CtrlTypeMode = 0    ! set to iCtrlVarType_xxxx
  CHARACTER(len=MaxNameLength)   :: AirLoopName    =' ' ! name of air loop that will use "MultiZone:Cooling:Average" strategy
  INTEGER                        :: AirLoopNum     =0   ! index of named air loop
  REAL(r64)                      :: MinSetTemp     =0.0d0 ! minimum supply air setpoint temperature [C]
  REAL(r64)                      :: MaxSetTemp     =0.0d0 ! maximum supply air setpoint temperature [C]
  INTEGER                        :: NumCtrlNodes   =0   ! number of nodes whose temperature is being set
  INTEGER, DIMENSION(:), ALLOCATABLE :: CtrlNodes       ! nodes where temperature is being set
  REAL(r64)                      :: SetPt          =0.0d0 ! the temperature setpoint [C]
END TYPE DefMultiZoneAverageCoolingSetPointManager

TYPE DefMultiZoneAverageHeatingSetPointManager ! derived type for SetpointManager:Multizone:Heating:Average data
  CHARACTER(len=MaxNameLength)   :: Name           =' '
  CHARACTER(len=MaxNameLength)   :: CtrlVarType    =' ' ! type of variable to be set
  INTEGER                        :: CtrlTypeMode = 0    ! set to iCtrlVarType_xxxx
  CHARACTER(len=MaxNameLength)   :: AirLoopName    =' ' ! name of air loop that will use "MultiZone:Heating:Average" strategy
  INTEGER                        :: AirLoopNum     =0   ! index of named air loop
  REAL(r64)                      :: MinSetTemp     =0.0d0 ! minimum supply air setpoint temperature [C]
  REAL(r64)                      :: MaxSetTemp     =0.0d0 ! maximum supply air setpoint temperature [C]
  INTEGER                        :: NumCtrlNodes   =0   ! number of nodes whose temperature is being set
  INTEGER, DIMENSION(:), ALLOCATABLE :: CtrlNodes       ! nodes where temperature is being set
  REAL(r64)                      :: SetPt          =0.0d0 ! the temperature setpoint [C]
END TYPE DefMultiZoneAverageHeatingSetPointManager

TYPE DefMultiZoneAverageMinHumSetPointManager ! derived type for SetpointManager:MultiZone:MinimumHumidity:Average data
  CHARACTER(len=MaxNameLength)   :: Name           =' '
  CHARACTER(len=MaxNameLength)   :: CtrlVarType    =' ' ! type of variable to be set
  INTEGER                        :: CtrlTypeMode   =0   ! set to iCtrlVarType_xxxx
  CHARACTER(len=MaxNameLength)   :: AirLoopName    =' ' ! name of air loop using MultiZone:MinimumHumidity:Average strategy
  INTEGER                        :: AirLoopNum     =0   ! index of named air loop
  REAL(r64)                      :: MinSetHum      =0.0d0 ! minimum supply air humidity ratio [kg/kg]
  REAL(r64)                      :: MaxSetHum      =0.0d0 ! maximum supply air humidity ratio [kg/kg]
  INTEGER                        :: NumCtrlNodes   =0   ! number of nodes whose humidity ratio is being set
  INTEGER, DIMENSION(:), ALLOCATABLE :: CtrlNodes       ! nodes where humidity ratio is being set
  REAL(r64)                      :: SetPt          =0.0d0 ! the humidity ratio setpoint [kg/kg]
END TYPE DefMultiZoneAverageMinHumSetPointManager

TYPE DefMultiZoneAverageMaxHumSetPointManager ! derived type for SetpointManager:MultiZone:MaximumHumidity:Average data
  CHARACTER(len=MaxNameLength)   :: Name           =' '
  CHARACTER(len=MaxNameLength)   :: CtrlVarType    =' ' ! type of variable to be set
  INTEGER                        :: CtrlTypeMode   =0   ! set to iCtrlVarType_xxxx
  CHARACTER(len=MaxNameLength)   :: AirLoopName    =' ' ! name of air loop using MultiZone:MaximumHumidity:Average strategy
  INTEGER                        :: AirLoopNum     =0   ! index of named air loop
  REAL(r64)                      :: MinSetHum      =0.0d0 ! minimum supply air humidity ratio [kg/kg]
  REAL(r64)                      :: MaxSetHum      =0.0d0 ! maximum supply air humidity ratio [kg/kg]
  INTEGER                        :: NumCtrlNodes   =0   ! number of nodes whose humidity ratio is being set
  INTEGER, DIMENSION(:), ALLOCATABLE :: CtrlNodes       ! nodes where humidity ratio is being set
  REAL(r64)                      :: SetPt          =0.0d0 ! the humidity ratio setpoint [kg/kg]
END TYPE DefMultiZoneAverageMaxHumSetPointManager

TYPE DefMultiZoneMinHumSetPointManager    ! derived type for SetpointManager:MultiZone:Humidity:Minimum data
  CHARACTER(len=MaxNameLength)   :: Name           =' '
  CHARACTER(len=MaxNameLength)   :: CtrlVarType    =' ' ! type of variable to be set
  INTEGER                        :: CtrlTypeMode   =0   ! set to iCtrlVarType_xxxx
  CHARACTER(len=MaxNameLength)   :: AirLoopName    =' ' ! name of air loop using SetpointManager:MultiZone:Humidity:Minimum
  INTEGER                        :: AirLoopNum     =0   ! index of named air loop
  REAL(r64)                      :: MinSetHum      =0.0d0 ! minimum supply air humidity ratio [kg/kg]
  REAL(r64)                      :: MaxSetHum      =0.0d0 ! maximum supply air humidity ratio [kg/kg]
  INTEGER                        :: NumCtrlNodes   =0   ! number of nodes whose humidity ratio is being set
  INTEGER, DIMENSION(:), ALLOCATABLE :: CtrlNodes       ! nodes where humidity ratio is being set
  REAL(r64)                      :: SetPt          =0.0d0 ! the humidity ratio setpoint [kg/kg]
END TYPE DefMultiZoneMinHumSetPointManager

TYPE DefMultiZoneMaxHumSetPointManager    ! derived type for SetpointManager:MultiZone:Humidity:Maximum data
  CHARACTER(len=MaxNameLength)   :: Name           =' '
  CHARACTER(len=MaxNameLength)   :: CtrlVarType    =' ' ! type of variable to be set
  INTEGER                        :: CtrlTypeMode   =0   ! set to iCtrlVarType_xxxx
  CHARACTER(len=MaxNameLength)   :: AirLoopName    =' ' ! name of air loop using SetpointManager:MultiZone:Humidity:Maximum
  INTEGER                        :: AirLoopNum     =0   ! index of named air loop
  REAL(r64)                      :: MinSetHum      =0.0d0 ! minimum supply air humidity ratio [kg/kg]
  REAL(r64)                      :: MaxSetHum      =0.0d0 ! maximum supply air humidity ratio [kg/kg]
  INTEGER                        :: NumCtrlNodes   =0   ! number of nodes whose humidity ratio is being set
  INTEGER, DIMENSION(:), ALLOCATABLE :: CtrlNodes       ! nodes where humidity ratio is being set
  REAL(r64)                      :: SetPt          =0.0d0 ! the humidity ratio setpoint [kg/kg]
END TYPE DefMultiZoneMaxHumSetPointManager

TYPE DefineFollowOATempSetPointManager
  CHARACTER(len=MaxNameLength)   :: Name           =' '
  CHARACTER(len=MaxNameLength)   :: CtrlVarType    =' ' ! type of variable to be set
  INTEGER                        :: CtrlTypeMode = 0    ! set to iCtrlVarType_xxxx
  CHARACTER(len=MaxNameLength)   :: RefTempType    =' ' ! Reference Temperature type (choice OutdoorAirWetBulb/OutdoorAirDryBulb)
  INTEGER                        :: RefTypeMode    = 0  ! set to iRefTempType_WetBulb or iRefTempType_DryBulb
  REAL(r64)                      :: Offset         =0.0d0 ! Offset temperature difference
  REAL(r64)                      :: MinSetTemp     =0.0d0 ! Minimum supply air setpoint temperature
  REAL(r64)                      :: MaxSetTemp     =0.0d0 ! Maximum supply air setpoint temperature
  INTEGER                        :: NumCtrlNodes   =0   ! number of nodes whose temperature is being set
  INTEGER, DIMENSION(:), ALLOCATABLE :: CtrlNodes       ! nodes where temperature is being set
  REAL(r64)                      :: SetPt          =0.0d0 ! the setpoint
END TYPE DefineFollowOATempSetPointManager

TYPE DefineFollowSysNodeTempSetPointManager
  CHARACTER(len=MaxNameLength)   :: Name           =' '
  CHARACTER(len=MaxNameLength)   :: CtrlVarType    =' ' ! type of variable to be set
  INTEGER                        :: CtrlTypeMode   =0   ! set to iCtrlVarType_xxxx
  INTEGER                        :: RefNodeNum     =0   ! reference node number
  CHARACTER(len=MaxNameLength)   :: RefTempType    =' ' ! Reference Temperature type (choice OutdoorAirWetBulb/OutdoorAirDryBulb)
  INTEGER                        :: RefTypeMode    = 0  ! set to iRefTempType_WetBulb or iRefTempType_DryBulb
  REAL(r64)                      :: Offset         =0.0d0 ! Offset temperature difference
  REAL(r64)                      :: MinSetTemp     =0.0d0 ! Minimum supply air setpoint temperature
  REAL(r64)                      :: MaxSetTemp     =0.0d0 ! Maximum supply air setpoint temperature
  INTEGER                        :: NumCtrlNodes   =0   ! number of nodes whose temperature is being set
  INTEGER, DIMENSION(:), ALLOCATABLE :: CtrlNodes       ! nodes where temperature is being set
  REAL(r64)                      :: SetPt          =0.0d0 ! the setpoint
END TYPE DefineFollowSysNodeTempSetPointManager

TYPE DefineGroundTempSetPointManager
  CHARACTER(len=MaxNameLength)   :: Name           =' '
  CHARACTER(len=MaxNameLength)   :: CtrlVarType    =' ' ! type of variable to be set
  INTEGER                        :: CtrlTypeMode = 0    ! set to iCtrlVarType_xxxx
  CHARACTER(len=MaxNameLength)   :: RefGroundTempObjType    =' ' ! Reference Temperature type (Available choices are listed below)
                                                                 ! Site:GroundTemperature:BuildingSurface
                                                                 ! Site:GroundTemperature:Shallow
                                                                 ! Site:GroundTemperature:Deep
                                                                 ! Site:GroundTemperature:FCfactorMethod
  INTEGER                        :: RefTypeMode    = 0  ! set to iRefGroundTempObjType_xxxx based on RefGroundTempObjType
  REAL(r64)                      :: Offset         =0.0d0 ! Offset temperature difference
  REAL(r64)                      :: MinSetTemp     =0.0d0 ! Minimum supply air setpoint temperature
  REAL(r64)                      :: MaxSetTemp     =0.0d0 ! Maximum supply air setpoint temperature
  INTEGER                        :: NumCtrlNodes   =0   ! number of nodes whose temperature is being set
  INTEGER, DIMENSION(:), ALLOCATABLE :: CtrlNodes       ! nodes where temperature is being set
  REAL(r64)                      :: SetPt          =0.0d0 ! the setpoint
END TYPE DefineGroundTempSetPointManager

TYPE DefineCondEntSetPointManager   ! derived type for SetpointManager:CondenserEnteringReset data
  CHARACTER(len=MaxNameLength) :: Name                       = ' '
  CHARACTER(len=MaxNameLength) :: CtrlVarType                = ' '   ! type of variable to be set
  INTEGER                      :: CtrlTypeMode               = 0     ! set to iCtrlVarType_xxxx
  CHARACTER(len=MaxNameLength) :: CondEntTempSched           = ' '   ! Optional schedule
  INTEGER                      :: CondEntTempSchedPtr        = 0     ! default condenser entering water temperature schedule Index
  REAL(r64)                    :: TowerDsnInletAirWetBulb    = 0.0d0     ! cooling tower design inlet air wetbulb temperature
  INTEGER                      :: MinTwrWbCurve              = 0     ! minimum design wetbulb temperature curve name
  INTEGER                      :: MinOaWbCurve               = 0     ! minimum outside air wetbulb temperature curve name
  INTEGER                      :: OptCondEntCurve            = 0     ! optimized condenser entering water temperature curve name
  REAL(r64)                    :: MinimumLiftTD              = 0.0d0     ! minimum lift
  REAL(r64)                    :: MaxCondEntTemp             = 0.0d0     ! maximum condenser entering water temp
  INTEGER                      :: NumCtrlNodes               = 0     ! number of nodes whose temperature is being set
  CHARACTER(len=MaxNameLength) :: CtrlNodeListName           = ' '
  INTEGER, DIMENSION(:), ALLOCATABLE :: CtrlNodes                    ! nodes where temperature is being set
  REAL(r64)                    :: SetPt                      =0.0d0    ! the temperature set point [C]
  INTEGER                      :: ChillerIndexPlantSide      = 0     ! plant side chiller index
  INTEGER                      :: ChillerIndexDemandSide     = 0     ! demand side chiller index
  INTEGER                      :: BranchIndexPlantSide       = 0     ! plant side branch index
  INTEGER                      :: BranchIndexDemandSide      = 0     ! demand side branch index
  INTEGER                      :: LoopIndexPlantSide         = 0     ! plant side loop index
  INTEGER                      :: LoopIndexDemandSide        = 0     ! deand side loop index
  INTEGER                      :: TypeNum                    = 0     ! chiller type number
END TYPE DefineCondEntSetPointManager

TYPE DefineIdealCondEntSetPointManager   ! derived type for SetpointManager:CondenserEnteringReset:Ideal data
  CHARACTER(len=MaxNameLength) :: Name                       = ' '
  CHARACTER(len=MaxNameLength) :: CtrlVarType                = ' '   ! type of variable to be set
  INTEGER                      :: CtrlTypeMode               = 0     ! set to iCtrlVarType_xxxx
  REAL(r64)                    :: MinimumLiftTD              = 0.0d0     ! minimum lift
  REAL(r64)                    :: MaxCondEntTemp             = 0.0d0     ! maximum condenser entering water temp
  INTEGER                      :: NumCtrlNodes               = 0     ! number of nodes whose temperature is being set
  CHARACTER(len=MaxNameLength) :: CtrlNodeListName           = ' '
  INTEGER, DIMENSION(:), ALLOCATABLE :: CtrlNodes                    ! nodes where temperature is being set
  REAL(r64)                    :: SetPt                      = 0.0d0   ! the temperature set point [C]
  INTEGER                      :: ChillerIndexPlantSide      = 0     ! plant side chiller index
  INTEGER                      :: BranchIndexPlantSide       = 0     ! plant side branch index
  INTEGER                      :: LoopIndexPlantSide         = 0     ! plant side loop index
  INTEGER                      :: ChllrVarType               = 0     ! report variable type
  INTEGER                      :: ChllrVarIndex              = 0     ! report variable index
  INTEGER                      :: ChlPumpVarType             = 0     ! report variable type
  INTEGER                      :: ChlPumpVarIndex            = 0     ! report variable index
  INTEGER                      :: ClTowerVarType             = 0     ! report variable type
  INTEGER                      :: ClTowerVarIndex            = 0     ! report variable index
  INTEGER                      :: CndPumpVarType             = 0     ! report variable type
  INTEGER                      :: CndPumpVarIndex            = 0     ! report variable index
  INTEGER                      :: TypeNum                    = 0     ! chiller type number
  INTEGER                      :: TowerNum                   = 0     ! cooling tower number
  INTEGER                      :: CondLoopNum                = 0     ! condenser loop number
  INTEGER                      :: CondBranchNum              = 0     ! condenser branch number
  INTEGER                      :: CondPumpNum                = 0     ! condenser pump number
  INTEGER                      :: CondPumpBranchNum          = 0     ! condenser branch number for pump
  INTEGER                      :: ChilledPumpNum             = 0     ! chilled water pump number
  INTEGER                      :: ChilledPumpBranchNum       = 0     ! chilled water branch number for pump
END TYPE DefineIdealCondEntSetPointManager

TYPE DefineSZOneStageCoolinggSetPointManager ! Derived type for the Single Zone One Stage Cooling Setpoint Manager data
  CHARACTER(len=MaxNameLength) :: Name             =' '
  CHARACTER(len=MaxNameLength) :: CtrlVarType      =' ' ! type of variable to be set
  INTEGER                      :: CtrlTypeMode     =0   ! set to iCtrlVarType_xxxx
  CHARACTER(len=MaxNameLength) :: ControlZoneName  =' ' ! name of the control zone (zone with main thermostat)
  INTEGER                      :: ControlZoneNum   =0   ! number (index into Zone array) of control zone
  INTEGER                      :: ZoneNodeNum      =0   ! zone node number
  REAL(r64)                    :: CoolingOnTemp    =0.0d0 ! minimum supply air setpoint temperature
  REAL(r64)                    :: CoolingOffTemp   =0.0d0 ! maximum supply air setpoint temperature
  INTEGER      :: NumCtrlNodes                     =0
  INTEGER, DIMENSION(:), ALLOCATABLE      :: CtrlNodes  ! node numbers of nodes where setpoint is to be set
  REAL(r64)                    :: SetPt            =0.0d0 ! the setpoint
END TYPE DefineSZOneStageCoolinggSetPointManager

TYPE DefineSZOneStageHeatingSetPointManager ! Derived type for the Single Zone One Stage Heating Setpoint Manager data
  CHARACTER(len=MaxNameLength) :: Name             =' '
  CHARACTER(len=MaxNameLength) :: CtrlVarType      =' ' ! type of variable to be set
  INTEGER                      :: CtrlTypeMode     =0   ! set to iCtrlVarType_xxxx
  CHARACTER(len=MaxNameLength) :: ControlZoneName  =' ' ! name of the control zone (zone with main thermostat)
  INTEGER                      :: ControlZoneNum   =0   ! number (index into Zone array) of control zone
  INTEGER                      :: ZoneNodeNum      =0   ! zone node number
  REAL(r64)                    :: HeatingOnTemp    =0.0d0 ! minimum supply air setpoint temperature
  REAL(r64)                    :: HeatingOffTemp   =0.0d0 ! maximum supply air setpoint temperature
  INTEGER      :: NumCtrlNodes                     =0
  INTEGER, DIMENSION(:), ALLOCATABLE      :: CtrlNodes  ! node numbers of nodes where setpoint is to be set
  REAL(r64)                    :: SetPt            =0.0d0 ! the setpoint
END TYPE DefineSZOneStageHeatingSetPointManager


!MODULE VARIABLE DECLARATIONS:
  INTEGER :: NumAllSetPtMgrs             = 0   ! Number of all Setpoint Managers found in input
  INTEGER :: NumSchSetPtMgrs             = 0   ! Number of Scheduled Setpoint Managers found in input
  INTEGER :: NumDualSchSetPtMgrs         = 0   ! Number of Scheduled Dual Setpoint Managers found in input
  INTEGER :: NumOutAirSetPtMgrs          = 0   ! Number of Outside Air Setpoint Managers found in input
  INTEGER :: NumSZRhSetPtMgrs            = 0   ! number of single zone reheat setpoint managers
  INTEGER :: NumSZHtSetPtMgrs            = 0   ! number of single zone heating setpoint managers
  INTEGER :: NumSZClSetPtMgrs            = 0   ! number of single zone cooling setpoint managers
  INTEGER :: NumSZMinHumSetPtMgrs        = 0   ! number of Single Zone Minimum Humidity Setpoint Managers
  INTEGER :: NumSZMaxHumSetPtMgrs        = 0   ! number of Single Zone Maximum Humidity Setpoint Managers
  INTEGER :: NumMixedAirSetPtMgrs        = 0   ! number of mixed air setpoint managers
  INTEGER :: NumOAPretreatSetPtMgrs      = 0   ! number of outside air pretreat setpoint managers
  INTEGER :: NumWarmestSetPtMGrs         = 0   ! number of Warmest setpoint managers
  INTEGER :: NumColdestSetPtMGrs         = 0   ! number of Coldest setpoint managers
  INTEGER :: NumWarmestSetPtMGrsTempFlow = 0   ! number of Warmest Temp Flow setpoint managers
  INTEGER :: NumRABFlowSetPtMgrs         = 0   ! number of return air bypass temperature-based flow setpoint manager
  INTEGER :: NumMZClgAverageSetPtMGrs    = 0   ! number of Multizone:Cooling:Average setpoint managers
  INTEGER :: NumMZHtgAverageSetPtMGrs    = 0   ! number of Multizone:Heating:Average setpoint managers
  INTEGER :: NumMZAverageMinHumSetPtMgrs = 0   ! number of MultiZone:MinimumHumidity:Average setpoint managers
  INTEGER :: NumMZAverageMaxHumSetPtMgrs = 0   ! number of MultiZone:MaximumHumidity:Average setpoint managers
  INTEGER :: NumMZMinHumSetPtMgrs        = 0   ! number of MultiZone:Humidity:Minimum setpoint managers
  INTEGER :: NumMZMaxHumSetPtMgrs        = 0   ! number of MultiZone:Humidity:Maximum setpoint managers
  INTEGER :: NumFollowOATempSetPtMgrs    = 0   ! number of SetpointManager:FollowOutdoorAirTemperature setpoint managers
  INTEGER :: NumFollowSysNodeTempSetPtMgrs   = 0   ! number of SetpointManager:FollowSystemNodeTemperature setpoint managers
  INTEGER :: NumGroundTempSetPtMgrs      = 0   ! number of SetpointManager:FollowGroundTemperature setpoint managers
  INTEGER :: NumCondEntSetPtMgrs         = 0   ! number of Condenser Entering Reset setpoint managers
  INTEGER :: NumIdealCondEntSetPtMgrs    = 0   ! number of Ideal Condenser Entering Temperature setpoint managers
  INTEGER :: NumSZOneStageCoolingSetPtMgrs = 0 ! number of single zone one stage cooling setpoint managers
  INTEGER :: NumSZOneStageHeatingSetPtMgrs = 0 ! number of singel zone one stage heating setpoint managers

  LOGICAL :: ManagerOn=.false.
  LOGICAL,SAVE :: GetInputFlag = .TRUE.        ! First time, input is "gotten"

  TYPE (DataSetPointManager),             ALLOCATABLE, DIMENSION(:) :: AllSetPtMgr ! Array for all Setpoint Manager data(warnings)
  TYPE (DefineScheduledSetPointManager),  ALLOCATABLE, DIMENSION(:) :: SchSetPtMgr ! Array for Scheduled Setpoint Manager data
  TYPE (DefineSchedDualSetPointManager),  ALLOCATABLE, DIMENSION(:) :: DualSchSetPtMgr ! Dual Scheduled Setpoint Manager data
  TYPE (DefineOutsideAirSetPointManager), ALLOCATABLE, DIMENSION(:) :: OutAirSetPtMgr !Array for Outside Air Setpoint Manager data
  TYPE (DefineSZReheatSetPointManager),   ALLOCATABLE, DIMENSION(:) :: SingZoneRhSetPtMgr ! Array for SZRH Set Pt Mgr
  TYPE (DefineSZHeatingSetPointManager),  ALLOCATABLE, DIMENSION(:) :: SingZoneHtSetPtMgr ! Array for SZ Heating Set Pt Mgr
  TYPE (DefineSZCoolingSetPointManager),  ALLOCATABLE, DIMENSION(:) :: SingZoneClSetPtMgr ! Array for SZ Cooling Set Pt Mgr
  TYPE (DefineSZMinHumSetPointManager),   ALLOCATABLE, DIMENSION(:) :: SZMinHumSetPtMgr ! Array for SZ Min Hum Set Pt Mgr
  TYPE (DefineSZMaxHumSetPointManager),   ALLOCATABLE, DIMENSION(:) :: SZMaxHumSetPtMgr ! Array for SZ Max Hum Set Pt Mgr
  TYPE (DefineMixedAirSetPointManager),   ALLOCATABLE, DIMENSION(:) :: MixedAirSetPtMgr ! Array for Mixed Air Set Pt Mgr
  TYPE (DefineOAPretreatSetPointManager), ALLOCATABLE, DIMENSION(:) :: OAPretreatSetPtMgr ! Array for OA Pretreat Set Pt Mgr
  TYPE (DefineWarmestSetPointManager) ,   ALLOCATABLE, DIMENSION(:) :: WarmestSetPtMgr  ! Array for Warmest Set Pt Mgr
  TYPE (DefineColdestSetPointManager) ,   ALLOCATABLE, DIMENSION(:) :: ColdestSetPtMgr  ! Array for Coldest Set Pt Mgr
  TYPE (DefWarmestSetPtManagerTempFlow) , ALLOCATABLE, DIMENSION(:) :: WarmestSetPtMgrTempFlow ! Array for Warmest Set Pt Mgr
  TYPE (DefRABFlowSetPointManager),       ALLOCATABLE, DIMENSION(:) :: RABFlowSetPtMgr ! Array for return air bypass
                                                                     ! temperature-based flow control manager
  TYPE (DefMultiZoneAverageCoolingSetPointManager), ALLOCATABLE, DIMENSION(:) :: MZAverageCoolingSetPtMgr  ! Array for MultiZone
                                                                                 ! Average Cooling Set Pt Mgr
  TYPE (DefMultiZoneAverageHeatingSetPointManager), ALLOCATABLE, DIMENSION(:) :: MZAverageHeatingSetPtMgr  ! Array for MultiZone
                                                                                 ! Average Heating Set Pt Mgr
  TYPE (DefMultiZoneAverageMinHumSetPointManager), ALLOCATABLE, DIMENSION(:) :: MZAverageMinHumSetPtMgr  ! Array for MultiZone
                                                                                 ! Average Minimum humidity ratio Set Pt Mgr
  TYPE (DefMultiZoneAverageMaxHumSetPointManager), ALLOCATABLE, DIMENSION(:) :: MZAverageMaxHumSetPtMgr  ! Array for MultiZone
                                                                                 ! Average Maximum humidity ratio Set Pt Mgr

  TYPE (DefMultiZoneMinHumSetPointManager), ALLOCATABLE, DIMENSION(:) :: MZMinHumSetPtMgr  ! Multizone min humidity rat Set Pt Mgr
  TYPE (DefMultiZoneMaxHumSetPointManager), ALLOCATABLE, DIMENSION(:) :: MZMaxHumSetPtMgr  ! Multizone max humidity rat Set Pt Mgr
  TYPE (DefineFollowOATempSetPointManager), ALLOCATABLE, DIMENSION(:) :: FollowOATempSetPtMgr ! Array for Follow Outdoor Air
                                                                                              ! Temperature Setpoint Manager data
  TYPE (DefineFollowSysNodeTempSetPointManager), ALLOCATABLE, DIMENSION(:) :: FollowSysNodeTempSetPtMgr ! Array for Follow System
                                                                                              ! Node Temp Setpoint Manager data
  TYPE (DefineGroundTempSetPointManager), ALLOCATABLE, DIMENSION(:) :: GroundTempSetPtMgr ! Array for Ground Temp Setpoint
                                                                                          ! Manager data
  TYPE (DefineCondEntSetPointManager), ALLOCATABLE, DIMENSION(:) :: CondEntSetPtMgr   ! Condenser Entering Water Set Pt Mgr
  TYPE (DefineIdealCondEntSetPointManager), ALLOCATABLE, DIMENSION(:) :: IdealCondEntSetPtMgr !Ideal Condenser Entering Set Pt Mgr
  TYPE (DefineSZOneStageCoolinggSetPointManager), ALLOCATABLE, DIMENSION(:) :: SZOneStageCoolingSetPtMgr !single zone 1 stage cool
  TYPE (DefineSZOneStageHeatingSetPointManager), ALLOCATABLE, DIMENSION(:) :: SZOneStageHeatingSetPtMgr ! single zone 1 stage heat

!SUBROUTINE SPECIFICATIONS FOR MODULE SetPointManager
PUBLIC     ManageSetPoints
PRIVATE    GetSetPointManagerInputs
PRIVATE    VerifySetPointManagers
PRIVATE    InitSetPointManagers
PRIVATE    SimSetPointManagers
PRIVATE    CalcScheduledSetPoint
PRIVATE    CalcScheduledDualSetPoint
PRIVATE    CalcOutsideAirSetPoint
PRIVATE    CalcSingZoneRhSetPoint
PRIVATE    CalcSingZoneHtSetPoint
PRIVATE    CalcSingZoneClSetPoint
PRIVATE    CalcSingZoneMinHumSetPoint
PRIVATE    CalcSingZoneMaxHumSetPoint
PRIVATE    CalcMixedAirSetPoint
PRIVATE    CalcOAPretreatSetPoint
PRIVATE    CalcWarmestSetPoint
PRIVATE    CalcColdestSetPoint
PRIVATE    CalcWarmestSetPointTempFlow
PRIVATE    CalcRABFlowSetPoint
PRIVATE    UpdateSetPointManagers
PRIVATE    UpdateMixedAirSetPoints
PRIVATE    UpdateOAPretreatSetPoints
PUBLIC     IsNodeOnSetPtManager
PUBLIC     NodeHasSPMCtrlVarType
PRIVATE    CalcMultiZoneAverageCoolingSetPoint
PRIVATE    CalcMultiZoneAverageHeatingSetPoint
PRIVATE    CalcMultiZoneAverageMinHumSetPoint
PRIVATE    CalcMultiZoneAverageMaxHumSetPoint
PRIVATE    CalcMultiZoneMinHumSetPoint
PRIVATE    CalcMultiZoneMaxHumSetPoint
PRIVATE    CalcFollowOATempSetPoint
PRIVATE    CalcFollowSysNodeTempSetPoint
PRIVATE    CalcGroundTempSetPoint
PRIVATE    CalcCondEntSetPoint
PRIVATE    CalcIdealCondEntSetPoint
PUBLIC     CheckIFAnyIdealCondEntSetPoint

CONTAINS

SUBROUTINE ManageSetPoints
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Russ Taylor, Rick Strand
          !       DATE WRITTEN   May 1998
          !       MODIFIED       Fred Buhl May 2000
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:

          ! METHODOLOGY EMPLOYED:
          ! Each flag is checked and the appropriate manager is then called.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: SetPtMgrNum ! loop index

! First time ManageSetPoints is called, get the input for all the setpoint managers
IF (GetInputFlag) THEN
  CALL GetSetPointManagerInputs
  GetInputFlag = .FALSE.
END IF

CALL InitSetPointManagers

IF (ManagerOn) THEN
  CALL SimSetPointManagers
  CALL UpdateSetPointManagers
  ! The Mixed Air Setpoint Managers (since they depend on other setpoints, they must be calculated
  ! and updated next to last).
  DO SetPtMgrNum=1,NumMixedAirSetPtMgrs
    CALL CalcMixedAirSetPoint(SetPtMgrNum)
  END DO
  CALL UpdateMixedAirSetPoints
  ! The Outside Air Pretreat Setpoint Managers (since they depend on other setpoints, they must be calculated
  ! and updated last).
  DO SetPtMgrNum=1,NumOAPretreatSetPtMgrs
    CALL CalcOAPretreatSetPoint(SetPtMgrNum)
  END DO
  CALL UpdateOAPretreatSetPoints
END IF

RETURN
END SUBROUTINE ManageSetPoints

SUBROUTINE GetSetPointManagerInputs

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   July 1998
          !       MODIFIED       Shirey/Raustad (FSEC), Jan 2004
          !                      Nov 2004 - Jan 2005 M. J. Witte, GARD Analytics, Inc.
          !                        Add new setpoint managers:
          !                          SET POINT MANAGER:SINGLE ZONE HEATING and
          !                          SET POINT MANAGER:SINGLE ZONE COOLING
          !                          SET POINT MANAGER:OUTSIDE AIR PRETREAT
          !                        Work supported by ASHRAE research project 1254-RP
          !                      Haves October 2004
          !                      Witte (GARD), Sep 2006
          !                      July 2010 B.A. Nigusse, FSEC/UCF
          !                        Added new setpoint managers:
          !                          SetpointManager:MultiZone:Heating:Average
          !                          SetpointManager:MultiZone:Cooling:Average
          !                          SetpointManager:MultiZone:MinimumHumidity:Average
          !                          SetpointManager:MultiZone:MaximumHumidity:Average
          !                      Aug 2010 B.A. Nigusse, FSEC/UCF
          !                        Added new setpoint managers:
          !                          SetpointManager:MultiZone:Humidity:Minimum
          !                          SetpointManager:MultiZone:Humidity:Maximum

          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Input the SetPointManager data and store it in the SetPtMgrIn array.
          ! Examine the Controllers in the input data and determine which ones
          ! will have their setpoints set by a particular Setpoint Manager.

          ! METHODOLOGY EMPLOYED:
          ! Use the Get routines from the InputProcessor module.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindIteminList, GetObjectItemNum, VerifyName, SameString,  &
                              MakeUPPERCase, GetObjectDefMaxArgs
    USE NodeInputManager, ONLY: GetOnlySingleNode, GetNodeNums
    USE DataHeatBalance, ONLY: Zone
    USE ScheduleManager, ONLY: GetScheduleIndex, CheckScheduleValueMinMax
!    USE DataIPShortCuts
    USE General, ONLY: RoundSigDigits, FindNumberinList
    USE DataEnvironment, ONLY: GroundTemp_DeepObjInput, GroundTempObjInput, GroundTemp_SurfaceObjInput, FCGroundTemps, &
                               GroundTemp_Deep, GroundTemp,GroundTemp_Surface, GroundTempFC
    USE DataZoneEquipment, ONLY: GetSystemNodeNumberForZone
    USE DataZoneControls,  ONLY: StageZoneLogic
    IMPLICIT NONE

          ! SUBROUTINE PARAMETER DEFINITIONS:
CHARACTER(len=*), PARAMETER    :: RoutineName='GetSetPointManagerInputs: ' ! include trailing blank space

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: cAlphaFieldNames
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: cNumericFieldNames
LOGICAL, ALLOCATABLE, DIMENSION(:) :: lNumericFieldBlanks
LOGICAL, ALLOCATABLE, DIMENSION(:) :: lAlphaFieldBlanks
CHARACTER(len=MaxNameLength),ALLOCATABLE, DIMENSION(:) :: cAlphaArgs
REAL(r64),ALLOCATABLE, DIMENSION(:) :: rNumericArgs
CHARACTER(len=MaxNameLength) :: cCurrentModuleObject
INTEGER :: MaxNumAlphas = 0 !argument for call to GetObjectDefMaxArgs
INTEGER :: MaxNumNumbers = 0 !argument for call to GetObjectDefMaxArgs

INTEGER :: NumNums                                    ! Number of real numbers returned by GetObjectItem
INTEGER :: NumAlphas                                  ! Number of alphanumerics returned by GetObjectItem
INTEGER :: NumParams
INTEGER :: SetPtMgrNum                                ! Setpoint Manager index
INTEGER :: AllSetPtMgrNum                             ! Setpoint Manager index to ALL setpoint managers in single TYPE
INTEGER :: IOStat                                     ! Status flag from GetObjectItem
INTEGER :: NumNodesCtrld                              ! number of controlled nodes in input node list
INTEGER :: CtrldNodeNum                               ! index of the items in the controlled node node list
INTEGER :: NumZones                                   ! number of zone nodes in input node list
INTEGER :: ZoneNum                                    ! loop index for zone nodes
INTEGER :: NumNodes
INTEGER,ALLOCATABLE, DIMENSION(:)  :: NodeNums
LOGICAL :: ErrorsFound=.false.
LOGICAL :: IsNotOK                                    ! Flag to verify name
LOGICAL :: IsBlank                                    ! Flag for blank name
LOGICAL :: NodeListError=.false.
LOGICAL :: ErrInList
INTEGER :: Found
LOGICAL :: NoSurfaceGroundTempObjWarning = .TRUE.  ! This will cause a warning to be issued if no "surface" ground
                                                   ! temperature object was input.
LOGICAL :: NoShallowGroundTempObjWarning = .TRUE.  ! This will cause a warning to be issued if no "shallow" ground
                                                   ! temperature object was input.
LOGICAL :: NoDeepGroundTempObjWarning = .TRUE.     ! This will cause a warning to be issued if no "deep" ground
                                                   ! temperature object was input.
LOGICAL :: NoFCGroundTempObjWarning = .TRUE.       ! This will cause a warning to be issued if no ground
                                                   ! temperature object was input for FC Factor method

NumNodesCtrld = 0
CtrldNodeNum = 0
NumZones = 0
ZoneNum = 0

  cCurrentModuleObject = 'SetpointManager:Scheduled'
  NumSchSetPtMgrs             = GetNumObjectsFound(cCurrentModuleObject) ! 'SetpointManager:Scheduled'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=NumNums
  MaxNumAlphas=NumAlphas

  cCurrentModuleObject = 'SetpointManager:Scheduled:DualSetpoint'
  NumDualSchSetPtMgrs         = GetNumObjectsFound(cCurrentModuleObject) ! 'SetpointManager:Scheduled:DualSetpoint'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:OutdoorAirReset'
  NumOutAirSetPtMgrs          = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:OutdoorAirReset'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:SingleZone:Reheat'
  NumSZRhSetPtMgrs            = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:SingleZone:Reheat'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:SingleZone:Heating'
  NumSZHtSetPtMgrs            = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:SingleZone:Heating'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:SingleZone:Cooling'
  NumSZClSetPtMgrs            = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:SingleZone:Cooling'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:SingleZone:Humidity:Minimum'
  NumSZMinHumSetPtMgrs        = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:SingleZone:Humidity:Minimum'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:SingleZone:Humidity:Maximum'
  NumSZMaxHumSetPtMgrs        = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:SingleZone:Humidity:Maximum'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:MixedAir'
  NumMixedAirSetPtMgrs        = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:MixedAir'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:OutdoorAirPretreat'
  NumOAPretreatSetPtMgrs      = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:OutdoorAirPretreat'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:Warmest'
  NumWarmestSetPtMgrs         = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:Warmest'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:Coldest'
  NumColdestSetPtMgrs         = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:Coldest'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:WarmestTemperatureFlow'
  NumWarmestSetPtMgrsTempFlow = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:WarmestTemperatureFlow'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:ReturnAirBypassFlow'
  NumRABFlowSetPtMgrs         = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:ReturnAirBypassFlow'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:MultiZone:Cooling:Average'
  NumMZClgAverageSetPtMgrs    = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:MultiZone:Cooling:Average'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:MultiZone:Heating:Average'
  NumMZHtgAverageSetPtMgrs    = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:MultiZone:Heating:Average'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:MultiZone:MinimumHumidity:Average'
  NumMZAverageMinHumSetPtMgrs = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:MultiZone:MinimumHumidity:Average'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:MultiZone:MaximumHumidity:Average'
  NumMZAverageMaxHumSetPtMgrs = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:MultiZone:MaximumHumidity:Average'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:MultiZone:Humidity:Minimum'
  NumMZMinHumSetPtMgrs        = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:MultiZone:Humidity:Minimum'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:MultiZone:Humidity:Maximum'
  NumMZMaxHumSetPtMgrs        = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:MultiZone:Humidity:Maximum'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:FollowOutdoorAirTemperature'
  NumFollowOATempSetPtMgrs    = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:FollowOutdoorAirTemperature'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:FollowSystemNodeTemperature'
  NumFollowSysNodeTempSetPtMgrs   = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:FollowSystemNodeTemperature'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:FollowGroundTemperature'
  NumGroundTempSetPtMgrs      = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:FollowGroundTemperature'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:CondenserEnteringReset'
  NumCondEntSetPtMgrs         = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:CondenserEnteringReset'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:CondenserEnteringReset:Ideal'
  NumIdealCondEntSetPtMgrs    = GetNumObjectsFound(cCurrentModuleObject)  ! 'SetpointManager:CondenserEnteringReset:Ideal'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:SingleZone:OneStageCooling'
  NumSZOneStageCoolingSetPtMgrs = GetNumObjectsFound(cCurrentModuleObject)
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)

  cCurrentModuleObject = 'SetpointManager:SingleZone:OneStageHeating'
  NumSZOneStageHeatingSetPtMgrs = GetNumObjectsFound(cCurrentModuleObject)
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,NumParams,NumAlphas,NumNums)
  MaxNumNumbers=MAX(MaxNumNumbers,NumNums)
  MaxNumAlphas=MAX(MaxNumAlphas,NumAlphas)


NumAllSetPtMgrs             = NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                              NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + &
                              NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + &
                              NumColdestSetPtMgrs + NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + &
                              NumMZClgAverageSetPtMgrs + NumMZHtgAverageSetPtMgrs + NumMZAverageMinHumSetPtMgrs + &
                              NumMZAverageMaxHumSetPtMgrs + NumMZMinHumSetPtMgrs + NumMZMaxHumSetPtMgrs + &
                              NumFollowOATempSetPtMgrs + NumFollowSysNodeTempSetPtMgrs + NumGroundTempSetPtMgrs + &
                              NumCondEntSetPtMgrs + NumIdealCondEntSetPtMgrs + NumSZOneStageCoolingSetPtMgrs + &
                              NumSZOneStageHeatingSetPtMgrs

ALLOCATE(cAlphaFieldNames(MaxNumAlphas))
cAlphaFieldNames=' '
ALLOCATE(cAlphaArgs(MaxNumAlphas))
cAlphaArgs=' '
ALLOCATE(lAlphaFieldBlanks(MaxNumAlphas))
lAlphaFieldBlanks=.false.
ALLOCATE(cNumericFieldNames(MaxNumNumbers))
cNumericFieldNames=' '
ALLOCATE(rNumericArgs(MaxNumNumbers))
rNumericArgs=0.0d0
ALLOCATE(lNumericFieldBlanks(MaxNumNumbers))
lNumericFieldBlanks=.false.

CALL GetObjectDefMaxArgs('NodeList',NumParams,NumAlphas,NumNums)
ALLOCATE(NodeNums(NumParams))
NodeNums=0

IF (NumAllSetPtMgrs.GT.0) ALLOCATE(AllSetPtMgr(NumAllSetPtMgrs)) ! Allocate the entire Setpoint Manager input data array

! Input the Scheduled Setpoint Managers

IF (NumSchSetPtMgrs.GT.0) ALLOCATE(SchSetPtMgr(NumSchSetPtMgrs)) ! Allocate the Setpoint Manager input data array

! Input the data for each Setpoint Manager

cCurrentModuleObject='SetpointManager:Scheduled'

DO SetPtMgrNum = 1,NumSchSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1),SchSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  SchSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  SchSetPtMgr(SetPtMgrNum)%CtrlVarType = cAlphaArgs(2)
  ! setup program flow control integers
  IF (SameString(SchSetPtMgr(SetPtMgrNum)%CtrlVarType,'Temperature')) THEN
    SchSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_Temp
  ELSEIF (SameString(SchSetPtMgr(SetPtMgrNum)%CtrlVarType,'MaximumTemperature')) THEN
    SchSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_MaxTemp
  ELSEIF (SameString(SchSetPtMgr(SetPtMgrNum)%CtrlVarType,'MinimumTemperature')) THEN
    SchSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_MinTemp
  ELSEIF (SameString(SchSetPtMgr(SetPtMgrNum)%CtrlVarType,'HumidityRatio')) THEN
    SchSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_HumRat
  ELSEIF (SameString(SchSetPtMgr(SetPtMgrNum)%CtrlVarType,'MaximumHumidityRatio')) THEN
    SchSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_MaxHumRat
  ELSEIF (SameString(SchSetPtMgr(SetPtMgrNum)%CtrlVarType,'MinimumHumidityRatio')) THEN
    SchSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_MinHumRat
  ELSEIF (SameString(SchSetPtMgr(SetPtMgrNum)%CtrlVarType,'MassFlowRate')) THEN
    SchSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_MassFlow
  ELSEIF (SameString(SchSetPtMgr(SetPtMgrNum)%CtrlVarType,'MaximumMassFlowRate')) THEN
    SchSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_MaxMassFlow
  ELSEIF (SameString(SchSetPtMgr(SetPtMgrNum)%CtrlVarType,'MinimumMassFlowRate')) THEN
    SchSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_MinMassFlow
  ELSE
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
    CALL ShowContinueError('..Valid values are "Temperature","MaximumTemperature","MinimumTemperature",')
    CALL ShowContinueError('     "HumidityRatio","MaximumHumidityRatio","MinimumHumidityRatio","MassFlowRate",')
    CALL ShowContinueError('     "MaximumMassFlowRate" or "MinimumMassFlowRate"')
    ErrorsFound = .TRUE.
  ENDIF

  SchSetPtMgr(SetPtMgrNum)%Sched = cAlphaArgs(3)
  SchSetPtMgr(SetPtMgrNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(3))
  IF (SchSetPtMgr(SetPtMgrNum)%SchedPtr == 0) THEN
    IF (lAlphaFieldBlanks(3)) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '", blank required field.')
      Call ShowContinueError('..required field '//trim(cAlphaFieldNames(3)))
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '", invalid field.')
      Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
    END IF
    ErrorsFound=.TRUE.
  ENDIF
  SchSetPtMgr(SetPtMgrNum)%CtrlNodeListName = cAlphaArgs(4)
  NodeListError=.false.
  CALL GetNodeNums(SchSetPtMgr(SetPtMgrNum)%CtrlNodeListName,NumNodes,NodeNums,NodeListError, &
       NodeType_Unknown,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(4))

  IF (.not. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(SchSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    SchSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    SchSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      SchSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
    ErrorsFound=.TRUE.
  ENDIF

  AllSetPtMgrNum = SetPtMgrNum

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = SchSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = SchSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_Scheduled
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = SchSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = SchSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

! Input the Scheduled Setpoint Managers DUAL SETPOINT

IF (NumDualSchSetPtMgrs.GT.0) ALLOCATE(DualSchSetPtMgr(NumDualSchSetPtMgrs)) ! Allocate the Setpoint Manager input data array

  ! Input the data for each Setpoint Manager
cCurrentModuleObject='SetpointManager:Scheduled:DualSetpoint'

DO SetPtMgrNum = 1,NumDualSchSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1),DualSchSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  DualSchSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  DualSchSetPtMgr(SetPtMgrNum)%CtrlVarType = cAlphaArgs(2)
  IF (SameString(DualSchSetPtMgr(SetPtMgrNum)%CtrlVarType,'Temperature')) THEN
    DualSchSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_Temp
  ELSE
    ! should not come here if idd type choice and key list is working
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
    CALL ShowContinueError('..Valid value is "Temperature".')
    ErrorsFound = .TRUE.
  ENDIF
  DualSchSetPtMgr(SetPtMgrNum)%SchedHi = cAlphaArgs(3)
  DualSchSetPtMgr(SetPtMgrNum)%SchedPtrHi = GetScheduleIndex(cAlphaArgs(3))
  IF (DualSchSetPtMgr(SetPtMgrNum)%SchedPtrHi == 0) THEN
    IF (lAlphaFieldBlanks(3)) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '", blank required field.')
      Call ShowContinueError('..required field '//trim(cAlphaFieldNames(3)))
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '", invalid field.')
      Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
    END IF
    ErrorsFound=.TRUE.
  ENDIF
  DualSchSetPtMgr(SetPtMgrNum)%SchedLo = cAlphaArgs(4)
  DualSchSetPtMgr(SetPtMgrNum)%SchedPtrLo = GetScheduleIndex(cAlphaArgs(4))
  IF (DualSchSetPtMgr(SetPtMgrNum)%SchedPtrLo == 0) THEN
    IF (lAlphaFieldBlanks(4)) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '", blank required field.')
      Call ShowContinueError('..required field '//trim(cAlphaFieldNames(4)))
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '", invalid field.')
      Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
    END IF
    ErrorsFound=.TRUE.
  ENDIF
  DualSchSetPtMgr(SetPtMgrNum)%CtrlNodeListName = cAlphaArgs(5)
  NodeListError=.false.
  CALL GetNodeNums(DualSchSetPtMgr(SetPtMgrNum)%CtrlNodeListName,NumNodes,NodeNums,NodeListError, &
       NodeType_Unknown,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(5))

  IF (.not. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(DualSchSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    DualSchSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    DualSchSetPtMgr(SetPtMgrNum)%SetPtHi = 0.0d0
    DualSchSetPtMgr(SetPtMgrNum)%SetPtLo = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      DualSchSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE  ! check getnodenums/nodelist
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
    ErrorsFound=.TRUE.
  ENDIF

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = DualSchSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = DualSchSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_ScheduledDual
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = DualSchSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = DualSchSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

! Input the Outside Air Setpoint Managers

IF (NumOutAirSetPtMgrs.GT.0) ALLOCATE(OutAirSetPtMgr(NumOutAirSetPtMgrs)) ! Allocate the Setpoint Manager input data array

  ! Input the data for each Setpoint Manager
cCurrentModuleObject='SetpointManager:OutdoorAirReset'

DO SetPtMgrNum = 1,NumOutAirSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1),OutAirSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  OutAirSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  OutAirSetPtMgr(SetPtMgrNum)%CtrlVarType = cAlphaArgs(2)
  IF (SameString(OutAirSetPtMgr(SetPtMgrNum)%CtrlVarType,'Temperature')) THEN
    OutAirSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_Temp
  ELSE
    ! should not come here if idd type choice and key list is working
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
    CALL ShowContinueError('..Valid value is "Temperature".')
    ErrorsFound = .TRUE.
  ENDIF
  OutAirSetPtMgr(SetPtMgrNum)%OutLowSetPt1     = rNumericArgs(1)
  OutAirSetPtMgr(SetPtMgrNum)%OutLow1          = rNumericArgs(2)
  OutAirSetPtMgr(SetPtMgrNum)%OutHighSetPt1    = rNumericArgs(3)
  OutAirSetPtMgr(SetPtMgrNum)%OutHigh1         = rNumericArgs(4)
  OutAirSetPtMgr(SetPtMgrNum)%CtrlNodeListName = cAlphaArgs(3)
  IF (OutAirSetPtMgr(SetPtMgrNum)%OutHigh1 < OutAirSetPtMgr(SetPtMgrNum)%OutLow1) THEN
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid setpoints.')
    CALL ShowContinueError('...'//trim(cNumericFieldNames(4))//  &
         '=['//trim(RoundSigDigits(OutAirSetPtMgr(SetPtMgrNum)%OutHigh1,1))//'] is less than '//  &
         trim(cNumericFieldNames(2))//  &
         '=['//trim(RoundSigDigits(OutAirSetPtMgr(SetPtMgrNum)%OutLow1,1))//'].')
  ENDIF
  ! Get optional input: schedule and 2nd reset rule
  IF (NumAlphas.EQ.4 .AND. NumNums.EQ.8) THEN
    OutAirSetPtMgr(SetPtMgrNum)%Sched         = cAlphaArgs(4)
    OutAirSetPtMgr(SetPtMgrNum)%SchedPtr      = GetScheduleIndex(cAlphaArgs(4))
    ! Schedule is optional here, so no check on SchedPtr
    OutAirSetPtMgr(SetPtMgrNum)%OutLowSetPt2  = rNumericArgs(5)
    OutAirSetPtMgr(SetPtMgrNum)%OutLow2       = rNumericArgs(6)
    OutAirSetPtMgr(SetPtMgrNum)%OutHighSetPt2 = rNumericArgs(7)
    OutAirSetPtMgr(SetPtMgrNum)%OutHigh2      = rNumericArgs(8)
    IF (OutAirSetPtMgr(SetPtMgrNum)%OutHigh2 < OutAirSetPtMgr(SetPtMgrNum)%OutLow2) THEN
      CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid setpoints.')
      CALL ShowContinueError('...'//trim(cNumericFieldNames(8))//  &
           '=['//trim(RoundSigDigits(OutAirSetPtMgr(SetPtMgrNum)%OutHigh2,1))//'] is less than '//  &
           trim(cNumericFieldNames(6))//  &
           '=['//trim(RoundSigDigits(OutAirSetPtMgr(SetPtMgrNum)%OutLow2,1))//'].')
    ENDIF
  ELSE
    OutAirSetPtMgr(SetPtMgrNum)%Sched         = '  '
    OutAirSetPtMgr(SetPtMgrNum)%SchedPtr      = 0
    OutAirSetPtMgr(SetPtMgrNum)%OutLowSetPt2  = 0.0d0
    OutAirSetPtMgr(SetPtMgrNum)%OutLow2       = 0.0d0
    OutAirSetPtMgr(SetPtMgrNum)%OutHighSetPt2 = 0.0d0
    OutAirSetPtMgr(SetPtMgrNum)%OutHigh2      = 0.0d0
  END IF
  NodeListError=.false.
  CALL GetNodeNums(OutAirSetPtMgr(SetPtMgrNum)%CtrlNodeListName,NumNodes,NodeNums,NodeListError, &
       NodeType_Unknown,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(3))
  IF (.not. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(OutAirSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    OutAirSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    OutAirSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      OutAirSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
    ErrorsFound=.TRUE.
  ENDIF

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = OutAirSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = OutAirSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_OutsideAir
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = OutAirSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = OutAirSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

! Input the Single Zone Reheat Setpoint Managers

IF (NumSZRhSetPtMgrs.GT.0) ALLOCATE(SingZoneRhSetPtMgr(NumSZRhSetPtMgrs)) ! Allocate the Setpoint Manager input data array

  ! Input the data for each Setpoint Manager
cCurrentModuleObject='SetpointManager:SingleZone:Reheat'

DO SetPtMgrNum = 1,NumSZRhSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1),SingZoneRhSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  SingZoneRhSetPtMgr(SetPtMgrNum)%Name        = cAlphaArgs(1)
  SingZoneRhSetPtMgr(SetPtMgrNum)%CtrlVarType = cAlphaArgs(2)
  IF (SameString(SingZoneRhSetPtMgr(SetPtMgrNum)%CtrlVarType,'Temperature')) THEN
    SingZoneRhSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_Temp
  ELSE
    ! should not come here if idd type choice and key list is working
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
    CALL ShowContinueError('..Valid value is "Temperature".')
    ErrorsFound = .TRUE.
  ENDIF
  SingZoneRhSetPtMgr(SetPtMgrNum)%ControlZoneName = cAlphaArgs(3)
  SingZoneRhSetPtMgr(SetPtMgrNum)%MinSetTemp = rNumericArgs(1)
  SingZoneRhSetPtMgr(SetPtMgrNum)%MaxSetTemp = rNumericArgs(2)
  IF (SingZoneRhSetPtMgr(SetPtMgrNum)%MaxSetTemp < SingZoneRhSetPtMgr(SetPtMgrNum)%MinSetTemp) THEN
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
    CALL ShowContinueError('...'//trim(cNumericFieldNames(2))//  &
         '=['//trim(RoundSigDigits(SingZoneRhSetPtMgr(SetPtMgrNum)%MaxSetTemp,1))//'] is less than '//  &
         trim(cNumericFieldNames(1))//  &
         '=['//trim(RoundSigDigits(SingZoneRhSetPtMgr(SetPtMgrNum)%MinSetTemp,1))//'].')
  ENDIF
  SingZoneRhSetPtMgr(SetPtMgrNum)%ZoneNodeNum = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)
  SingZoneRhSetPtMgr(SetPtMgrNum)%ZoneInletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)
  NodeListError=.false.
  CALL GetNodeNums(cAlphaArgs(6),NumNodes,NodeNums,NodeListError, & ! setpoint nodes
       NodeType_Unknown,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(6))
  IF (.NOT. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(SingZoneRhSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    SingZoneRhSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    SingZoneRhSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      SingZoneRhSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(6))//'="'//TRIM(cAlphaArgs(6))//'".')
    ErrorsFound=.TRUE.
  ENDIF

  ! get the actual zone number of the control zone
  SingZoneRhSetPtMgr(SetPtMgrNum)%ControlZoneNum = FindItemInList(cAlphaArgs(3),Zone%Name,NumOfZones)
  IF (SingZoneRhSetPtMgr(SetPtMgrNum)%ControlZoneNum == 0) THEN
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
    ErrorsFound=.TRUE.
  ENDIF
  SingZoneRhSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = SingZoneRhSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = SingZoneRhSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_SZReheat
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = SingZoneRhSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = SingZoneRhSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

! Input the Single Zone Heating Setpoint Managers

IF (NumSZHtSetPtMgrs.GT.0) ALLOCATE(SingZoneHtSetPtMgr(NumSZHtSetPtMgrs)) ! Allocate the Setpoint Manager input data array

  ! Input the data for each Setpoint Manager
cCurrentModuleObject='SetpointManager:SingleZone:Heating'

DO SetPtMgrNum = 1,NumSZHtSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1),SingZoneHtSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  SingZoneHtSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  SingZoneHtSetPtMgr(SetPtMgrNum)%CtrlVarType = cAlphaArgs(2)
  IF (SameString(SingZoneHtSetPtMgr(SetPtMgrNum)%CtrlVarType,'Temperature')) THEN
    SingZoneHtSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_Temp
  ELSE
    ! should not come here if idd type choice and key list is working
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
    CALL ShowContinueError('..Valid value is "Temperature".')
    ErrorsFound = .TRUE.
  ENDIF
  SingZoneHtSetPtMgr(SetPtMgrNum)%ControlZoneName = cAlphaArgs(3)
  SingZoneHtSetPtMgr(SetPtMgrNum)%MinSetTemp = rNumericArgs(1)
  SingZoneHtSetPtMgr(SetPtMgrNum)%MaxSetTemp = rNumericArgs(2)
  IF (SingZoneHtSetPtMgr(SetPtMgrNum)%MaxSetTemp < SingZoneHtSetPtMgr(SetPtMgrNum)%MinSetTemp) THEN
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
    CALL ShowContinueError('...'//trim(cNumericFieldNames(2))//  &
         '=['//trim(RoundSigDigits(SingZoneHtSetPtMgr(SetPtMgrNum)%MaxSetTemp,1))//'] is less than '//  &
         trim(cNumericFieldNames(1))//  &
         '=['//trim(RoundSigDigits(SingZoneHtSetPtMgr(SetPtMgrNum)%MinSetTemp,1))//'].')
  ENDIF
  SingZoneHtSetPtMgr(SetPtMgrNum)%ZoneNodeNum = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)
  SingZoneHtSetPtMgr(SetPtMgrNum)%ZoneInletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)
  NodeListError=.false.
  CALL GetNodeNums(cAlphaArgs(6),NumNodes,NodeNums,NodeListError, & ! setpoint nodes
       NodeType_Unknown,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(6))
  IF (.NOT. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(SingZoneHtSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    SingZoneHtSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    SingZoneHtSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      SingZoneHtSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
    ErrorsFound=.TRUE.
  ENDIF

  ! get the actual zone number of the control zone
  SingZoneHtSetPtMgr(SetPtMgrNum)%ControlZoneNum = FindItemInList(cAlphaArgs(3),Zone%Name,NumOfZones)
  IF (SingZoneHtSetPtMgr(SetPtMgrNum)%ControlZoneNum == 0) THEN
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
    ErrorsFound=.TRUE.
  ENDIF
  SingZoneHtSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = SingZoneHtSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = SingZoneHtSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_SZHeating
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = SingZoneHtSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = SingZoneHtSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

! Input the Single Zone Cooling Setpoint Managers

IF (NumSZClSetPtMgrs.GT.0) ALLOCATE(SingZoneClSetPtMgr(NumSZClSetPtMgrs)) ! Allocate the Setpoint Manager input data array

  ! Input the data for each Setpoint Manager
cCurrentModuleObject='SetpointManager:SingleZone:Cooling'
DO SetPtMgrNum = 1,NumSZClSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1),SingZoneClSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  SingZoneClSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  SingZoneClSetPtMgr(SetPtMgrNum)%CtrlVarType = cAlphaArgs(2)
  IF (SameString(SingZoneClSetPtMgr(SetPtMgrNum)%CtrlVarType,'Temperature')) Then
    SingZoneClSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_Temp
  ELSE
    ! should not come here if idd type choice and key list is working
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
    CALL ShowContinueError('..Valid value is "Temperature".')
    ErrorsFound = .TRUE.
  ENDIF
  SingZoneClSetPtMgr(SetPtMgrNum)%ControlZoneName = cAlphaArgs(3)
  SingZoneClSetPtMgr(SetPtMgrNum)%MinSetTemp = rNumericArgs(1)
  SingZoneClSetPtMgr(SetPtMgrNum)%MaxSetTemp = rNumericArgs(2)
  IF (SingZoneClSetPtMgr(SetPtMgrNum)%MaxSetTemp < SingZoneClSetPtMgr(SetPtMgrNum)%MinSetTemp) THEN
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
    CALL ShowContinueError('...'//trim(cNumericFieldNames(2))//  &
         '=['//trim(RoundSigDigits(SingZoneClSetPtMgr(SetPtMgrNum)%MaxSetTemp,1))//'] is less than '//  &
         trim(cNumericFieldNames(1))//  &
         '=['//trim(RoundSigDigits(SingZoneClSetPtMgr(SetPtMgrNum)%MinSetTemp,1))//'].')
  ENDIF
  SingZoneClSetPtMgr(SetPtMgrNum)%ZoneNodeNum = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)
  SingZoneClSetPtMgr(SetPtMgrNum)%ZoneInletNodeNum = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)
  NodeListError=.false.
  CALL GetNodeNums(cAlphaArgs(6),NumNodes,NodeNums,NodeListError, & ! setpoint nodes
       NodeType_Unknown,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(6))
  IF (.NOT. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(SingZoneClSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    SingZoneClSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    SingZoneClSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      SingZoneClSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(6))//'="'//TRIM(cAlphaArgs(6))//'".')
    ErrorsFound=.TRUE.
  ENDIF

  ! get the actual zone number of the control zone
  SingZoneClSetPtMgr(SetPtMgrNum)%ControlZoneNum = FindItemInList(cAlphaArgs(3),Zone%Name,NumOfZones)
  IF (SingZoneClSetPtMgr(SetPtMgrNum)%ControlZoneNum == 0) THEN
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
    ErrorsFound=.TRUE.
  ENDIF
  SingZoneClSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                   NumSZHtSetPtMgrs

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = SingZoneClSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = SingZoneClSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_SZCooling
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = SingZoneClSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = SingZoneClSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

! Input the Single Zone Minimum Humidity Setpoint Managers

IF (NumSZMinHumSetPtMgrs.GT.0) ALLOCATE(SZMinHumSetPtMgr(NumSZMinHumSetPtMgrs))

  ! Input the data for each Setpoint Manager
cCurrentModuleObject='SetpointManager:SingleZone:Humidity:Minimum'
DO SetPtMgrNum = 1,NumSZMinHumSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1),SZMinHumSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  SZMinHumSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  SZMinHumSetPtMgr(SetPtMgrNum)%CtrlVarType = 'MinimumHumidityRatio'
  SZMinHumSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_MinHumRat

  IF(cAlphaArgs(2) .NE. '')THEN
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
    CALL ShowContinueError('Deprecated Field in Object.  Please leave blank.')
    Call ShowContinueError('Please note that this field in this object will be deleted in future versions.')
  END IF
  IF(cAlphaArgs(3) .NE. '')THEN
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
    CALL ShowContinueError('Deprecated Field in Object.  Please leave blank.')
    Call ShowContinueError('Please note that this field in this object will be deleted in future versions.')
  END IF

  NodeListError=.false.
  CALL GetNodeNums(cAlphaArgs(4),NumNodes,NodeNums,NodeListError, & ! nodes whose min humidity ratio will be set
       NodeType_Air,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(4))
  IF (.NOT. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(SZMinHumSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    SZMinHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    SZMinHumSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      SZMinHumSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
    ErrorsFound=.TRUE.
  ENDIF

  ErrInList=.false.
  CALL GetNodeNums(cAlphaArgs(5),NumNodes,NodeNums,ErrInList, & ! nodes of zones whose humidity is being controlled
       NodeType_Air,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Sensor,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(5))
  IF (ErrInList) THEN
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
    ErrorsFound=.TRUE.
  ENDIF
  NumZones = NumNodes
  SZMinHumSetPtMgr(SetPtMgrNum)%NumZones = NumZones
  ! only allow one control zone for now
  IF (NumNodes > 1) THEN
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", entered nodelist.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
    Call ShowContinueError('..only one control zone is allowed.')
    ErrorsFound=.TRUE.
  END IF
  ALLOCATE(SZMinHumSetPtMgr(SetPtMgrNum)%ZoneNodes(NumZones))
  ALLOCATE(SZMinHumSetPtMgr(SetPtMgrNum)%ZoneNum(NumZones))
  ALLOCATE(SZMinHumSetPtMgr(SetPtMgrNum)%CtrlZoneNum(NumZones))

  DO ZoneNum = 1,NumZones
    SZMinHumSetPtMgr(SetPtMgrNum)%ZoneNodes(ZoneNum) = NodeNums(ZoneNum)
    SZMinHumSetPtMgr(SetPtMgrNum)%ZoneNum(ZoneNum)   = 0
    SZMinHumSetPtMgr(SetPtMgrNum)%CtrlZoneNum(ZoneNum) = 0
  END DO

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                   NumSZHtSetPtMgrs + NumSZClSetPtMgrs

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = SZMinHumSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = SZMinHumSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_SZMinHum
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = SZMinHumSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = SZMinHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

! Input the Single Zone Maximum Humidity Setpoint Managers

IF (NumSZMaxHumSetPtMgrs.GT.0) ALLOCATE(SZMaxHumSetPtMgr(NumSZMaxHumSetPtMgrs))

  ! Input the data for each Setpoint Manager
cCurrentModuleObject='SetpointManager:SingleZone:Humidity:Maximum'
DO SetPtMgrNum = 1,NumSZMaxHumSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1),SZMaxHumSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  SZMaxHumSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  SZMaxHumSetPtMgr(SetPtMgrNum)%CtrlVarType = 'MaximumHumidityRatio'
  SZMaxHumSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_MaxHumRat

  IF(cAlphaArgs(2) .NE. '')THEN
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
    CALL ShowContinueError('Deprecated Field in Object.  Please leave blank.')
    Call ShowContinueError('Please note that this field in this object will be deleted in future versions.')
  END IF
  IF(cAlphaArgs(3) .NE. '')THEN
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
    CALL ShowContinueError('Deprecated Field in Object.  Please leave blank.')
    Call ShowContinueError('Please note that this field in this object will be deleted in future versions.')
  END IF

  NodeListError=.false.
  CALL GetNodeNums(cAlphaArgs(4),NumNodes,NodeNums,NodeListError, & ! nodes whose max humidity ratio will be set
       NodeType_Air,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(4))
  IF (.NOT. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(SZMaxHumSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    SZMaxHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    SZMaxHumSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      SZMaxHumSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
    ErrorsFound=.TRUE.
  ENDIF

  ErrInList=.false.
  CALL GetNodeNums(cAlphaArgs(5),NumNodes,NodeNums,ErrInList, & ! nodes of zones whose humidity is being controlled
       NodeType_Air,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Sensor,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(5))
  IF (ErrInList) THEN
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
    ErrorsFound=.TRUE.
  ENDIF
  NumZones = NumNodes
  SZMaxHumSetPtMgr(SetPtMgrNum)%NumZones = NumZones
  ! only allow one control zone for now
  IF (NumNodes > 1) THEN
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", entered nodelist.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
    Call ShowContinueError('..only one control zone is allowed.')
    ErrorsFound=.TRUE.
  END IF
  ALLOCATE(SZMaxHumSetPtMgr(SetPtMgrNum)%ZoneNodes(NumZones))
  ALLOCATE(SZMaxHumSetPtMgr(SetPtMgrNum)%ZoneNum(NumZones))
  ALLOCATE(SZMaxHumSetPtMgr(SetPtMgrNum)%CtrlZoneNum(NumZones))

  DO ZoneNum = 1,NumZones
    SZMaxHumSetPtMgr(SetPtMgrNum)%ZoneNodes(ZoneNum) = NodeNums(ZoneNum)
!   Actual zone node and controlled zone numbers set in Init subroutine
    SZMaxHumSetPtMgr(SetPtMgrNum)%ZoneNum(ZoneNum)   = 0
    SZMaxHumSetPtMgr(SetPtMgrNum)%CtrlZoneNum(ZoneNum) = 0
  END DO

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                   NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = SZMaxHumSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = SZMaxHumSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_SZMaxHum
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = SZMaxHumSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = SZMaxHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

! Input the Mixed Air Setpoint Managers

IF (NumMixedAirSetPtMgrs.GT.0) ALLOCATE(MixedAirSetPtMgr(NumMixedAirSetPtMgrs))

  ! Input the data for each Setpoint Manager
cCurrentModuleObject='SetpointManager:MixedAir'
DO SetPtMgrNum = 1,NumMixedAirSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1),MixedAirSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF

  MixedAirSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  MixedAirSetPtMgr(SetPtMgrNum)%CtrlVarType = cAlphaArgs(2)
  IF (SameString(MixedAirSetPtMgr(SetPtMgrNum)%CtrlVarType,'Temperature')) THEN
    MixedAirSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_Temp
  ELSE
    ! should not come here if idd type choice and key list is working
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
    CALL ShowContinueError('..Valid value is "Temperature".')
    ErrorsFound = .TRUE.
  ENDIF
  MixedAirSetPtMgr(SetPtMgrNum)%RefNode = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)
  MixedAirSetPtMgr(SetPtMgrNum)%FanInNode = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)
  MixedAirSetPtMgr(SetPtMgrNum)%FanOutNode = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)
  NodeListError=.false.
  CALL GetNodeNums(cAlphaArgs(6),NumNodes,NodeNums,NodeListError, & ! setpoint nodes
       NodeType_Air,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(6))
  IF (.NOT. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(MixedAirSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    MixedAirSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    MixedAirSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      MixedAirSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(6))//'="'//TRIM(cAlphaArgs(6))//'".')
    ErrorsFound=.TRUE.
  ENDIF

  Found=FindNumberInList(MixedAirSetPtMgr(SetPtMgrNum)%RefNode,  &
           MixedAirSetPtMgr(SetPtMgrNum)%CtrlNodes,MixedAirSetPtMgr(SetPtMgrNum)%NumCtrlNodes)
  IF (Found > 0) THEN
    IF (MixedAirSetPtMgr(SetPtMgrNum)%NumCtrlNodes > 1) THEN
      CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", reference node.')
      CALL ShowContinueError('..Reference Node is the same as one of the nodes in SetPoint NodeList')
    ELSE
      CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", reference node.')
      CALL ShowContinueError('..Reference Node is the same as the SetPoint Node')
    ENDIF
    CALL ShowContinueError('Reference Node Name="'//TRIM(NodeID(MixedAirSetPtMgr(SetPtMgrNum)%RefNode))//'".')
  ENDIF

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                   NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = MixedAirSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = MixedAirSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_MixedAir
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = MixedAirSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = MixedAirSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

! Input the Outside Air Pretreat Setpoint Managers

IF (NumOAPretreatSetPtMgrs.GT.0) ALLOCATE(OAPretreatSetPtMgr(NumOAPretreatSetPtMgrs))

  ! Input the data for each Setpoint Manager
cCurrentModuleObject='SetpointManager:OutdoorAirPretreat'
DO SetPtMgrNum = 1,NumOAPretreatSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1),OAPretreatSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF

  OAPretreatSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  OAPretreatSetPtMgr(SetPtMgrNum)%CtrlVarType = cAlphaArgs(2)
  ! setup program flow control integers.
  SELECT CASE(MakeUPPERCase(OAPretreatSetPtMgr(SetPtMgrNum)%CtrlVarType))

    CASE ('TEMPERATURE')
      OAPretreatSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_Temp
    CASE ('HUMIDITYRATIO')
      OAPretreatSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_HumRat
    CASE ('MAXIMUMHUMIDITYRATIO')
      OAPretreatSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_MaxHumRat
    CASE ('MINIMUMHUMIDITYRATIO')
      OAPretreatSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_MinHumRat
    CASE DEFAULT
      ! should not come here if idd type choice and key list is working
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '", invalid field.')
      Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
      CALL ShowContinueError('..Valid values are "Temperature","HumidityRatio","MaximumHumidityRatio" or "MinimumHumidityRatio".')
      ErrorsFound = .TRUE.
  END SELECT

  OAPretreatSetPtMgr(SetPtMgrNum)%MinSetTemp   = rNumericArgs(1)
  OAPretreatSetPtMgr(SetPtMgrNum)%MaxSetTemp   = rNumericArgs(2)
  IF (OAPretreatSetPtMgr(SetPtMgrNum)%MaxSetTemp < OAPretreatSetPtMgr(SetPtMgrNum)%MinSetTemp) THEN
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
    CALL ShowContinueError('...'//trim(cNumericFieldNames(2))//  &
         '=['//trim(RoundSigDigits(OAPretreatSetPtMgr(SetPtMgrNum)%MaxSetTemp,1))//'] is less than '//  &
         trim(cNumericFieldNames(1))//  &
         '=['//trim(RoundSigDigits(OAPretreatSetPtMgr(SetPtMgrNum)%MinSetTemp,1))//'].')
  ENDIF
  OAPretreatSetPtMgr(SetPtMgrNum)%MinSetHumRat = rNumericArgs(3)
  OAPretreatSetPtMgr(SetPtMgrNum)%MaxSetHumRat = rNumericArgs(4)
  IF (OAPretreatSetPtMgr(SetPtMgrNum)%MaxSetHumRat < OAPretreatSetPtMgr(SetPtMgrNum)%MinSetHumRat) THEN
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
    CALL ShowContinueError('...'//trim(cNumericFieldNames(4))//  &
         '=['//trim(RoundSigDigits(OAPretreatSetPtMgr(SetPtMgrNum)%MaxSetHumRat,1))//'] is less than '//  &
         trim(cNumericFieldNames(3))//  &
         '=['//trim(RoundSigDigits(OAPretreatSetPtMgr(SetPtMgrNum)%MinSetHumRat,1))//'].')
  ENDIF

  ! Because a zero humidity ratio setpoint is a special value indicating "off" or "no load"
  ! must not allow MinSetHumRat or MaxSetHumRat to be <=0.0
  IF (OAPretreatSetPtMgr(SetPtMgrNum)%MinSetHumRat .LE. 0.0d0) THEN
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid value.')
    CALL ShowContinueError('Minimum setpoint humidity ratio <=0.0, resetting to 0.00001')
    OAPretreatSetPtMgr(SetPtMgrNum)%MinSetHumRat = 0.00001d0
  ENDIF
  IF (OAPretreatSetPtMgr(SetPtMgrNum)%MaxSetHumRat .LE. 0.0d0) THEN
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'", invalid value.')
    CALL ShowContinueError('Maximum setpoint humidity ratio <=0.0, resetting to 0.00001')
    OAPretreatSetPtMgr(SetPtMgrNum)%MaxSetHumRat = 0.00001d0
  ENDIF

  OAPretreatSetPtMgr(SetPtMgrNum)%RefNode = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)
  OAPretreatSetPtMgr(SetPtMgrNum)%MixedOutNode = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)
  OAPretreatSetPtMgr(SetPtMgrNum)%OAInNode = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)
  OAPretreatSetPtMgr(SetPtMgrNum)%ReturnInNode = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Sensor,1,ObjectIsNotParent)
  NodeListError=.false.
  CALL GetNodeNums(cAlphaArgs(7),NumNodes,NodeNums,NodeListError, & ! setpoint nodes
       NodeType_Air,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(7))
  IF (.NOT. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(OAPretreatSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    OAPretreatSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    OAPretreatSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      OAPretreatSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(7))//'="'//TRIM(cAlphaArgs(7))//'".')
    ErrorsFound=.TRUE.
  ENDIF

  Found=FindNumberInList(OAPretreatSetPtMgr(SetPtMgrNum)%RefNode,  &
           OAPretreatSetPtMgr(SetPtMgrNum)%CtrlNodes,OAPretreatSetPtMgr(SetPtMgrNum)%NumCtrlNodes)
  IF (Found > 0) THEN
    IF (OAPretreatSetPtMgr(SetPtMgrNum)%NumCtrlNodes > 1) THEN
      CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", reference node.')
      CALL ShowContinueError('..Reference Node is the same as one of the nodes in SetPoint NodeList')
    ELSE
      CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", reference node.')
      CALL ShowContinueError('..Reference Node is the same as the SetPoint Node')
    ENDIF
    CALL ShowContinueError('Reference Node Name="'//TRIM(NodeID(OAPretreatSetPtMgr(SetPtMgrNum)%RefNode))//'".')
  ENDIF

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                   NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + &
                   NumMixedAirSetPtMgrs

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = OAPretreatSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = OAPretreatSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_OutsideAirPretreat
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = OAPretreatSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = OAPretreatSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

! Input the Warmest Setpoint Managers

IF (NumWarmestSetPtMgrs.GT.0) ALLOCATE(WarmestSetPtMgr(NumWarmestSetPtMgrs))

  ! Input the data for each Setpoint Manager
cCurrentModuleObject='SetpointManager:Warmest'
DO SetPtMgrNum = 1,NumWarmestSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1),WarmestSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  WarmestSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  WarmestSetPtMgr(SetPtMgrNum)%CtrlVarType = cAlphaArgs(2)
  IF (SameString(WarmestSetPtMgr(SetPtMgrNum)%CtrlVarType,'Temperature')) THEN
    WarmestSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_Temp
  ELSE
    ! should not come here if idd type choice and key list is working
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
    CALL ShowContinueError('..Valid value is "Temperature".')
    ErrorsFound = .TRUE.
  ENDIF
  WarmestSetPtMgr(SetPtMgrNum)%AirLoopName = cAlphaArgs(3)
  WarmestSetPtMgr(SetPtMgrNum)%AirLoopNum = 0
  WarmestSetPtMgr(SetPtMgrNum)%MinSetTemp = rNumericArgs(1)
  WarmestSetPtMgr(SetPtMgrNum)%MaxSetTemp = rNumericArgs(2)
  IF (WarmestSetPtMgr(SetPtMgrNum)%MaxSetTemp < WarmestSetPtMgr(SetPtMgrNum)%MinSetTemp) THEN
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
    CALL ShowContinueError('...'//trim(cNumericFieldNames(2))//  &
         '=['//trim(RoundSigDigits(WarmestSetPtMgr(SetPtMgrNum)%MaxSetTemp,1))//'] is less than '//  &
         trim(cNumericFieldNames(1))//  &
         '=['//trim(RoundSigDigits(WarmestSetPtMgr(SetPtMgrNum)%MinSetTemp,1))//'].')
  ENDIF

  SELECT CASE(MakeUPPERCase(cAlphaArgs(4)))
    CASE('MAXIMUMTEMPERATURE')
        WarmestSetPtMgr(SetPtMgrNum)%Strategy = MaxTemp
    CASE DEFAULT
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '", invalid field.')
      Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
      CALL ShowContinueError('..Valid value is "MaximumTemperature".')
      ErrorsFound=.TRUE.
  END SELECT

  NodeListError=.false.
  CALL GetNodeNums(cAlphaArgs(5),NumNodes,NodeNums,NodeListError, &  ! setpoint nodes
       NodeType_Air,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(5))
  IF (.NOT. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(WarmestSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    WarmestSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    WarmestSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      WarmestSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
    ErrorsFound=.TRUE.
  ENDIF

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                   NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + &
                   NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = WarmestSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = WarmestSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_Warmest
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = WarmestSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = WarmestSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

! Input the Coldest Setpoint Managers

IF (NumColdestSetPtMgrs.GT.0) ALLOCATE(ColdestSetPtMgr(NumColdestSetPtMgrs))

  ! Input the data for each Setpoint Manager
cCurrentModuleObject='SetpointManager:Coldest'
DO SetPtMgrNum = 1,NumColdestSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1),ColdestSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  ColdestSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  ColdestSetPtMgr(SetPtMgrNum)%CtrlVarType = cAlphaArgs(2)
  IF (SameString(ColdestSetPtMgr(SetPtMgrNum)%CtrlVarType,'Temperature')) THEN
    ColdestSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_Temp
  ELSE
    ! should not come here if idd type choice and key list is working
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
    CALL ShowContinueError('..Valid value is "Temperature".')
    ErrorsFound = .TRUE.
  ENDIF
  ColdestSetPtMgr(SetPtMgrNum)%AirLoopName = cAlphaArgs(3)
  ColdestSetPtMgr(SetPtMgrNum)%AirLoopNum = 0
  ColdestSetPtMgr(SetPtMgrNum)%MinSetTemp = rNumericArgs(1)
  ColdestSetPtMgr(SetPtMgrNum)%MaxSetTemp = rNumericArgs(2)
  IF (ColdestSetPtMgr(SetPtMgrNum)%MaxSetTemp < ColdestSetPtMgr(SetPtMgrNum)%MinSetTemp) THEN
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
    CALL ShowContinueError('...'//trim(cNumericFieldNames(2))//  &
         '=['//trim(RoundSigDigits(ColdestSetPtMgr(SetPtMgrNum)%MaxSetTemp,1))//'] is less than '//  &
         trim(cNumericFieldNames(1))//  &
         '=['//trim(RoundSigDigits(ColdestSetPtMgr(SetPtMgrNum)%MinSetTemp,1))//'].')
  ENDIF

  SELECT CASE(MakeUPPERCase(cAlphaArgs(4)))
    CASE('MINIMUMTEMPERATURE')
        ColdestSetPtMgr(SetPtMgrNum)%Strategy = MinTemp
    CASE DEFAULT
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '", invalid field.')
      Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
      CALL ShowContinueError('..Valid value is "MinimumTemperature".')
      ErrorsFound=.TRUE.
  END SELECT

  NodeListError=.false.
  CALL GetNodeNums(cAlphaArgs(5),NumNodes,NodeNums,NodeListError, & ! setpoint nodes
       NodeType_Air,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(5))
  IF (.NOT. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(ColdestSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    ColdestSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    ColdestSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      ColdestSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
    ErrorsFound=.TRUE.
  ENDIF

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                   NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + &
                   NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = ColdestSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = ColdestSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_Coldest
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = ColdestSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = ColdestSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

! Input the Warmest Temp Flow Setpoint Managers

IF (NumWarmestSetPtMgrsTempFlow.GT.0) ALLOCATE(WarmestSetPtMgrTempFlow(NumWarmestSetPtMgrsTempFlow))

  ! Input the data for each Setpoint Manager
cCurrentModuleObject='SetpointManager:WarmestTemperatureFlow'
DO SetPtMgrNum = 1,NumWarmestSetPtMgrsTempFlow
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1),WarmestSetPtMgrTempFlow%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  WarmestSetPtMgrTempFlow(SetPtMgrNum)%Name = cAlphaArgs(1)
  WarmestSetPtMgrTempFlow(SetPtMgrNum)%CtrlVarType = cAlphaArgs(2)
  IF (SameString(WarmestSetPtMgrTempFlow(SetPtMgrNum)%CtrlVarType,'Temperature')) THEN
    WarmestSetPtMgrTempFlow(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_Temp
  ELSE
    ! should not come here if idd type choice and key list is working
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
    CALL ShowContinueError('..Valid value is "Temperature".')
    Errorsfound = .TRUE.
  ENDIF
  WarmestSetPtMgrTempFlow(SetPtMgrNum)%AirLoopName = cAlphaArgs(3)
  WarmestSetPtMgrTempFlow(SetPtMgrNum)%AirLoopNum  = 0
  WarmestSetPtMgrTempFlow(SetPtMgrNum)%MinSetTemp  = rNumericArgs(1)
  WarmestSetPtMgrTempFlow(SetPtMgrNum)%MaxSetTemp  = rNumericArgs(2)
  IF (WarmestSetPtMgrTempFlow(SetPtMgrNum)%MaxSetTemp < WarmestSetPtMgrTempFlow(SetPtMgrNum)%MinSetTemp) THEN
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
    CALL ShowContinueError('...'//trim(cNumericFieldNames(2))//  &
         '=['//trim(RoundSigDigits(WarmestSetPtMgrTempFlow(SetPtMgrNum)%MaxSetTemp,1))//'] is less than '//  &
         trim(cNumericFieldNames(1))//  &
         '=['//trim(RoundSigDigits(WarmestSetPtMgrTempFlow(SetPtMgrNum)%MinSetTemp,1))//'].')
  ENDIF
  WarmestSetPtMgrTempFlow(SetPtMgrNum)%MinTurndown = rNumericArgs(3)
  IF (WarmestSetPtMgrTempFlow(SetPtMgrNum)%MinTurndown >= 0.8d0) THEN
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
    CALL ShowContinueError('...'//trim(cNumericFieldNames(3))//  &
    '=['//trim(RoundSigDigits(WarmestSetPtMgrTempFlow(SetPtMgrNum)%MinTurndown,2))//'] is greater than 0.8;')
    CALL ShowContinueError('...typical values for '//trim(cNumericFieldNames(3))//' are less than 0.8.')
  END IF
  SELECT CASE(MakeUPPERCase(cAlphaArgs(4)))
    CASE('TEMPERATUREFIRST')
        WarmestSetPtMgrTempFlow(SetPtMgrNum)%Strategy = TempFirst
    CASE('FLOWFIRST')
        WarmestSetPtMgrTempFlow(SetPtMgrNum)%Strategy = FlowFirst
    CASE DEFAULT
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '", invalid field.')
      Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
      CALL ShowContinueError('..Valid values are "TemperatureFirst" or "FlowFirst".')
      ErrorsFound=.TRUE.
  END SELECT

  NodeListError=.false.
  CALL GetNodeNums(cAlphaArgs(5),NumNodes,NodeNums,NodeListError, &  ! setpoint nodes
       NodeType_Air,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(5))
  IF (.NOT. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(WarmestSetPtMgrTempFlow(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    WarmestSetPtMgrTempFlow(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    WarmestSetPtMgrTempFlow(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      WarmestSetPtMgrTempFlow(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
    ErrorsFound=.TRUE.
  ENDIF

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                   NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + &
                   NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = WarmestSetPtMgrTempFlow(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = WarmestSetPtMgrTempFlow(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_WarmestTempFlow
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = WarmestSetPtMgrTempFlow(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = WarmestSetPtMgrTempFlow(SetPtMgrNum)%NumCtrlNodes

END DO

! Input the Return Air Bypass Flow Setpoint Managers

IF (NumRABFlowSetPtMgrs.GT.0) ALLOCATE(RABFlowSetPtMgr(NumRABFlowSetPtMgrs))

  ! Input the data for each Setpoint Manager
cCurrentModuleObject='SetpointManager:ReturnAirBypassFlow'
DO SetPtMgrNum = 1,NumRABFlowSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1),RABFlowSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  RABFlowSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  RABFlowSetPtMgr(SetPtMgrNum)%CtrlVarType = cAlphaArgs(2)
  RABFlowSetPtMgr(SetPtMgrNum)%NumCtrlNodes = 1
  NumNodesCtrld = 1

  IF (SameString(RABFlowSetPtMgr(SetPtMgrNum)%CtrlVarType,'Flow')) THEN
    RABFlowSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_MassFlow
  ELSE
    ! should not come here if idd type choice and key list is working
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
    CALL ShowContinueError('..Valid value is "Temperature".')
    ErrorsFound = .TRUE.
  ENDIF
  RABFlowSetPtMgr(SetPtMgrNum)%AirLoopName = cAlphaArgs(3)
  RABFlowSetPtMgr(SetPtMgrNum)%AirLoopNum  = 0
  RABFlowSetPtMgr(SetPtMgrNum)%Sched       = cAlphaArgs(4)
  RABFlowSetPtMgr(SetPtMgrNum)%SchedPtr    = GetScheduleIndex(cAlphaArgs(4))
  IF (RABFlowSetPtMgr(SetPtMgrNum)%SchedPtr == 0) THEN
    IF (lAlphaFieldBlanks(4)) THEN
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '", blank required field.')
      Call ShowContinueError('..required field '//trim(cAlphaFieldNames(4)))
    ELSE
      CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '", invalid field.')
      Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
    END IF
    ErrorsFound=.TRUE.
  ENDIF

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                   NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + &
                   NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + &
                   NumWarmestSetPtMgrsTempFlow

  RABFlowSetPtMgr(SetPtMgrNum)%AllSetPtMgrIndex = AllSetPtMgrNum
  ALLOCATE(RABFlowSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
  RABFlowSetPtMgr(SetPtMgrNum)%CtrlNodes   = 0
  ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
! need to reset this to the control node (RABSplitOutNode) in Init, will be 0 here
  AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes    = RABFlowSetPtMgr(SetPtMgrNum)%CtrlNodes
  AllSetPtMgr(AllsetPtMgrNum)%Name         = RABFlowSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_RAB
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = RABFlowSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = RABFlowSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

! Input the MultiZone Average Cooling Setpoint Managers
IF (NumMZClgAverageSetPtMGrs.GT.0) ALLOCATE(MZAverageCoolingSetPtMgr(NumMZClgAverageSetPtMGrs))

  ! Input the data for each setpoint manager
cCurrentModuleObject= 'SetpointManager:MultiZone:Cooling:Average'
DO SetPtMgrNum = 1, NumMZClgAverageSetPtMGrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1), MZAverageCoolingSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  MZAverageCoolingSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  MZAverageCoolingSetPtMgr(SetPtMgrNum)%AirLoopName = cAlphaArgs(2)
  MZAverageCoolingSetPtMgr(SetPtMgrNum)%AirLoopNum = 0
  MZAverageCoolingSetPtMgr(SetPtMgrNum)%MinSetTemp = rNumericArgs(1)
  MZAverageCoolingSetPtMgr(SetPtMgrNum)%MaxSetTemp = rNumericArgs(2)
  MZAverageCoolingSetPtMgr(SetPtMgrNum)%CtrlVarType = 'Temperature'
  MZAverageCoolingSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_Temp

  IF (MZAverageCoolingSetPtMgr(SetPtMgrNum)%MaxSetTemp < MZAverageCoolingSetPtMgr(SetPtMgrNum)%MinSetTemp) THEN
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
    CALL ShowContinueError('...'//TRIM(cNumericFieldNames(2))//  &
         '=['//TRIM(RoundSigDigits(MZAverageCoolingSetPtMgr(SetPtMgrNum)%MaxSetTemp,1))//'] is less than '//  &
         TRIM(cNumericFieldNames(1))//  &
         '=['//TRIM(RoundSigDigits(MZAverageCoolingSetPtMgr(SetPtMgrNum)%MinSetTemp,1))//'].')
  ENDIF

  NodeListError=.false.
  CALL GetNodeNums(cAlphaArgs(3),NumNodes,NodeNums,NodeListError, &  ! setpoint nodes
       NodeType_Air,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(3))
  IF (.NOT. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(MZAverageCoolingSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    MZAverageCoolingSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    MZAverageCoolingSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      MZAverageCoolingSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
    ErrorsFound=.TRUE.
  ENDIF

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                   NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + &
                   NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + &
                   NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = MZAverageCoolingSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = MZAverageCoolingSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_MZCoolingAverage
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = MZAverageCoolingSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = MZAverageCoolingSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO


! Input the MultiZone Average Heating Setpoint Managers
IF (NumMZHtgAverageSetPtMGrs.GT.0) ALLOCATE(MZAverageHeatingSetPtMgr(NumMZHtgAverageSetPtMGrs))

  ! Input the data for each setpoint manager
cCurrentModuleObject= 'SetpointManager:MultiZone:Heating:Average'
DO SetPtMgrNum = 1, NumMZHtgAverageSetPtMGrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1), MZAverageHeatingSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  MZAverageHeatingSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  MZAverageHeatingSetPtMgr(SetPtMgrNum)%AirLoopName = cAlphaArgs(2)
  MZAverageHeatingSetPtMgr(SetPtMgrNum)%AirLoopNum = 0
  MZAverageHeatingSetPtMgr(SetPtMgrNum)%MinSetTemp = rNumericArgs(1)
  MZAverageHeatingSetPtMgr(SetPtMgrNum)%MaxSetTemp = rNumericArgs(2)
  MZAverageHeatingSetPtMgr(SetPtMgrNum)%CtrlVarType = 'Temperature'
  MZAverageHeatingSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_Temp

  IF (MZAverageHeatingSetPtMgr(SetPtMgrNum)%MaxSetTemp < MZAverageHeatingSetPtMgr(SetPtMgrNum)%MinSetTemp) THEN
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
    CALL ShowContinueError('...'//TRIM(cNumericFieldNames(2))//  &
         '=['//TRIM(RoundSigDigits(MZAverageHeatingSetPtMgr(SetPtMgrNum)%MaxSetTemp,1))//'] is less than '//  &
         TRIM(cNumericFieldNames(1))//  &
         '=['//TRIM(RoundSigDigits(MZAverageHeatingSetPtMgr(SetPtMgrNum)%MinSetTemp,1))//'].')
  ENDIF

  NodeListError=.false.
  CALL GetNodeNums(cAlphaArgs(3),NumNodes,NodeNums,NodeListError, &  ! setpoint nodes
       NodeType_Air,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(3))
  IF (.NOT. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(MZAverageHeatingSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    MZAverageHeatingSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    MZAverageHeatingSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      MZAverageHeatingSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
    ErrorsFound=.TRUE.
  ENDIF

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                   NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + &
                   NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + &
                   NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMGrs

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = MZAverageHeatingSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = MZAverageHeatingSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_MZHeatingAverage
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = MZAverageHeatingSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = MZAverageHeatingSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

! Input the MultiZone Average Minimum Humidity Setpoint Managers
IF (NumMZAverageMinHumSetPtMgrs.GT.0) ALLOCATE(MZAverageMinHumSetPtMgr(NumMZAverageMinHumSetPtMgrs))

  ! Input the data for each setpoint manager
cCurrentModuleObject= 'SetpointManager:MultiZone:MinimumHumidity:Average'
DO SetPtMgrNum = 1, NumMZAverageMinHumSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1), MZAverageMinHumSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  MZAverageMinHumSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  MZAverageMinHumSetPtMgr(SetPtMgrNum)%AirLoopName = cAlphaArgs(2)
  MZAverageMinHumSetPtMgr(SetPtMgrNum)%AirLoopNum = 0
  MZAverageMinHumSetPtMgr(SetPtMgrNum)%MinSetHum = rNumericArgs(1)
  MZAverageMinHumSetPtMgr(SetPtMgrNum)%MaxSetHum = rNumericArgs(2)
  MZAverageMinHumSetPtMgr(SetPtMgrNum)%CtrlVarType = 'MinimumHumidityRatio'
  MZAverageMinHumSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_MinHumRat

  IF (MZAverageMinHumSetPtMgr(SetPtMgrNum)%MaxSetHum < MZAverageMinHumSetPtMgr(SetPtMgrNum)%MinSetHum) THEN
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
    CALL ShowContinueError('...'//TRIM(cNumericFieldNames(2))//  &
         '=['//TRIM(RoundSigDigits(MZAverageMinHumSetPtMgr(SetPtMgrNum)%MaxSetHum,3))//'] is less than '//  &
         TRIM(cNumericFieldNames(1))//  &
         '=['//TRIM(RoundSigDigits(MZAverageMinHumSetPtMgr(SetPtMgrNum)%MinSetHum,3))//'].')
  ENDIF

  NodeListError=.false.
  CALL GetNodeNums(cAlphaArgs(3),NumNodes,NodeNums,NodeListError, &  ! setpoint nodes
       NodeType_Air,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(3))
  IF (.NOT. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(MZAverageMinHumSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    MZAverageMinHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    MZAverageMinHumSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      MZAverageMinHumSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
    ErrorsFound=.TRUE.
  ENDIF

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                   NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + &
                   NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + &
                   NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMGrs + NumMZHtgAverageSetPtMGrs

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = MZAverageMinHumSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = MZAverageMinHumSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_MZMinHumAverage
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = MZAverageMinHumSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = MZAverageMinHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

! Input the MultiZone Average Maximum Humidity SetPoint Managers
IF (NumMZAverageMaxHumSetPtMgrs.GT.0) ALLOCATE(MZAverageMaxHumSetPtMgr(NumMZAverageMaxHumSetPtMgrs))

  ! Input the data for each setpoint manager
cCurrentModuleObject= 'SetpointManager:MultiZone:MaximumHumidity:Average'
DO SetPtMgrNum = 1, NumMZAverageMaxHumSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1), MZAverageMaxHumSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  MZAverageMaxHumSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  MZAverageMaxHumSetPtMgr(SetPtMgrNum)%AirLoopName = cAlphaArgs(2)
  MZAverageMaxHumSetPtMgr(SetPtMgrNum)%AirLoopNum = 0
  MZAverageMaxHumSetPtMgr(SetPtMgrNum)%MinSetHum = rNumericArgs(1)
  MZAverageMaxHumSetPtMgr(SetPtMgrNum)%MaxSetHum = rNumericArgs(2)
  MZAverageMaxHumSetPtMgr(SetPtMgrNum)%CtrlVarType = 'MaximumHumidityRatio'
  MZAverageMaxHumSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_MaxHumRat

  IF (MZAverageMaxHumSetPtMgr(SetPtMgrNum)%MaxSetHum < MZAverageMaxHumSetPtMgr(SetPtMgrNum)%MinSetHum) THEN
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
    CALL ShowContinueError('...'//TRIM(cNumericFieldNames(2))//  &
         '=['//TRIM(RoundSigDigits(MZAverageMaxHumSetPtMgr(SetPtMgrNum)%MaxSetHum,3))//'] is less than '//  &
         TRIM(cNumericFieldNames(1))//  &
         '=['//TRIM(RoundSigDigits(MZAverageMaxHumSetPtMgr(SetPtMgrNum)%MinSetHum,3))//'].')
  ENDIF

  NodeListError=.false.
  CALL GetNodeNums(cAlphaArgs(3),NumNodes,NodeNums,NodeListError, &  ! setpoint nodes
       NodeType_Air,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(3))
  IF (.NOT. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(MZAverageMaxHumSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    MZAverageMaxHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    MZAverageMaxHumSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      MZAverageMaxHumSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
    ErrorsFound=.TRUE.
  ENDIF

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                   NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + &
                   NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + &
                   NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMGrs + NumMZHtgAverageSetPtMGrs + &
                   NumMZAverageMinHumSetPtMgrs

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = MZAverageMaxHumSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = MZAverageMaxHumSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_MZMaxHumAverage
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = MZAverageMaxHumSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = MZAverageMaxHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

! Input the Multizone Minimum Humidity Ratio SetPoint Managers
IF (NumMZMinHumSetPtMgrs.GT.0) ALLOCATE(MZMinHumSetPtMgr(NumMZMinHumSetPtMgrs))

! Input the data for each setpoint manager
cCurrentModuleObject= 'SetpointManager:MultiZone:Humidity:Minimum'
DO SetPtMgrNum = 1, NumMZMinHumSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1), MZMinHumSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  MZMinHumSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  MZMinHumSetPtMgr(SetPtMgrNum)%AirLoopName = cAlphaArgs(2)
  MZMinHumSetPtMgr(SetPtMgrNum)%AirLoopNum = 0
  MZMinHumSetPtMgr(SetPtMgrNum)%MinSetHum = rNumericArgs(1)
  MZMinHumSetPtMgr(SetPtMgrNum)%MaxSetHum = rNumericArgs(2)
  MZMinHumSetPtMgr(SetPtMgrNum)%CtrlVarType = 'MinimumHumidityRatio'
  MZMinHumSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_MinHumRat

  IF (MZMinHumSetPtMgr(SetPtMgrNum)%MaxSetHum < MZMinHumSetPtMgr(SetPtMgrNum)%MinSetHum) THEN
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
    CALL ShowContinueError('...'//TRIM(cNumericFieldNames(2))//  &
         '=['//TRIM(RoundSigDigits(MZMinHumSetPtMgr(SetPtMgrNum)%MaxSetHum,3))//'] is less than '//  &
         TRIM(cNumericFieldNames(1))//  &
         '=['//TRIM(RoundSigDigits(MZMinHumSetPtMgr(SetPtMgrNum)%MinSetHum,3))//'].')
  ENDIF

  NodeListError=.false.
  CALL GetNodeNums(cAlphaArgs(3),NumNodes,NodeNums,NodeListError, &  ! setpoint nodes
       NodeType_Air,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(3))
  IF (.NOT. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(MZMinHumSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    MZMinHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    MZMinHumSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      MZMinHumSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
    ErrorsFound=.TRUE.
  ENDIF

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                   NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + &
                   NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + &
                   NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMGrs + NumMZHtgAverageSetPtMGrs + &
                   NumMZAverageMinHumSetPtMgrs + NumMZAverageMaxHumSetPtMgrs

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = MZMinHumSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = MZMinHumSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_MZMinHum
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = MZMinHumSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = MZMinHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

! Input the Multizone Maximum Humidity Ratio SetPoint Managers
IF (NumMZMaxHumSetPtMgrs.GT.0) ALLOCATE(MZMaxHumSetPtMgr(NumMZMaxHumSetPtMgrs))

! Input the data for each setpoint manager
cCurrentModuleObject= 'SetpointManager:MultiZone:Humidity:Maximum'
DO SetPtMgrNum = 1, NumMZMaxHumSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1), MZMaxHumSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  MZMaxHumSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  MZMaxHumSetPtMgr(SetPtMgrNum)%AirLoopName = cAlphaArgs(2)
  MZMaxHumSetPtMgr(SetPtMgrNum)%AirLoopNum = 0
  MZMaxHumSetPtMgr(SetPtMgrNum)%MinSetHum = rNumericArgs(1)
  MZMaxHumSetPtMgr(SetPtMgrNum)%MaxSetHum = rNumericArgs(2)
  MZMaxHumSetPtMgr(SetPtMgrNum)%CtrlVarType = 'MaximumHumidityRatio'
  MZMaxHumSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_MaxHumRat

  IF (MZMaxHumSetPtMgr(SetPtMgrNum)%MaxSetHum < MZMaxHumSetPtMgr(SetPtMgrNum)%MinSetHum) THEN
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
    CALL ShowContinueError('...'//TRIM(cNumericFieldNames(2))//  &
         '=['//TRIM(RoundSigDigits(MZMaxHumSetPtMgr(SetPtMgrNum)%MaxSetHum,3))//'] is less than '//  &
         TRIM(cNumericFieldNames(1))//  &
         '=['//TRIM(RoundSigDigits(MZMaxHumSetPtMgr(SetPtMgrNum)%MinSetHum,3))//'].')
  ENDIF

  NodeListError=.false.
  CALL GetNodeNums(cAlphaArgs(3),NumNodes,NodeNums,NodeListError, &  ! setpoint nodes
       NodeType_Air,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(3))
  IF (.NOT. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(MZMaxHumSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    MZMaxHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    MZMaxHumSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      MZMaxHumSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
    ErrorsFound=.TRUE.
  ENDIF

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                   NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + &
                   NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + &
                   NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMGrs + NumMZHtgAverageSetPtMGrs + &
                   NumMZAverageMinHumSetPtMgrs + NumMZAverageMaxHumSetPtMgrs + NumMZMinHumSetPtMgrs

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = MZMaxHumSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = MZMaxHumSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_MZMaxHum
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = MZMaxHumSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = MZMaxHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

! Input the Follow Outdoor Air Temperature Setpoint Managers

IF (NumFollowOATempSetPtMgrs.GT.0) ALLOCATE(FollowOATempSetPtMgr(NumFollowOATempSetPtMgrs))

  ! Input the data for each Setpoint Manager
cCurrentModuleObject='SetpointManager:FollowOutdoorAirTemperature'
DO SetPtMgrNum = 1,NumFollowOATempSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1),FollowOATempSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  FollowOATempSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  FollowOATempSetPtMgr(SetPtMgrNum)%CtrlVarType = cAlphaArgs(2)
  IF (SameString(FollowOATempSetPtMgr(SetPtMgrNum)%CtrlVarType,'Temperature')) THEN
    FollowOATempSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_Temp
  ELSEIF (SameString(FollowOATempSetPtMgr(SetPtMgrNum)%CtrlVarType,'MaximumTemperature')) THEN
    FollowOATempSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_MaxTemp
  ELSEIF (SameString(FollowOATempSetPtMgr(SetPtMgrNum)%CtrlVarType,'MinimumTemperature')) THEN
    FollowOATempSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_MinTemp
  ELSE
     ! should not come here if idd type choice and key list is working
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
    CALL ShowContinueError('..Valid values are "Temperature","MaximumTemperature" or "MinimumTemperature".')
    ErrorsFound = .TRUE.
  ENDIF
  FollowOATempSetPtMgr(SetPtMgrNum)%RefTempType = cAlphaArgs(3)
  IF (SameString(FollowOATempSetPtMgr(SetPtMgrNum)%RefTempType,'OutdoorAirWetBulb')) THEN
    FollowOATempSetPtMgr(SetPtMgrNum)%RefTypeMode = iRefTempType_WetBulb
  ELSEIF (SameString(FollowOATempSetPtMgr(SetPtMgrNum)%RefTempType,'OutdoorAirDryBulb')) THEN
    FollowOATempSetPtMgr(SetPtMgrNum)%RefTypeMode = iRefTempType_DryBulb
  ELSE
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
    CALL ShowContinueError('..Valid values are "OutdoorAirWetBulb" or "OutdoorAirDryBulb".')
    ErrorsFound = .TRUE.
  ENDIF
  FollowOATempSetPtMgr(SetPtMgrNum)%Offset     = rNumericArgs(1)
  FollowOATempSetPtMgr(SetPtMgrNum)%MaxSetTemp = rNumericArgs(2)
  FollowOATempSetPtMgr(SetPtMgrNum)%MinSetTemp = rNumericArgs(3)
  IF (FollowOATempSetPtMgr(SetPtMgrNum)%MaxSetTemp < FollowOATempSetPtMgr(SetPtMgrNum)%MinSetTemp) THEN
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
    CALL ShowContinueError('...'//trim(cNumericFieldNames(2))//  &
         '=['//trim(RoundSigDigits(FollowOATempSetPtMgr(SetPtMgrNum)%MaxSetTemp,1))//'] is less than '//  &
         trim(cNumericFieldNames(3))//  &
         '=['//trim(RoundSigDigits(FollowOATempSetPtMgr(SetPtMgrNum)%MinSetTemp,1))//'].')
  ENDIF

  NodeListError=.false.
  CALL GetNodeNums(cAlphaArgs(4),NumNodes,NodeNums,NodeListError, &  ! setpoint nodes
       NodeType_Unknown,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(4))
  IF (.NOT. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(FollowOATempSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    FollowOATempSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    FollowOATempSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      FollowOATempSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
    ErrorsFound=.TRUE.
  ENDIF

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                   NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + &
                   NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + &
                   NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMGrs + NumMZHtgAverageSetPtMGrs + &
                   NumMZAverageMinHumSetPtMgrs + NumMZAverageMaxHumSetPtMgrs + NumMZMinHumSetPtMgrs + NumMZMaxHumSetPtMgrs

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = FollowOATempSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = FollowOATempSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_FollowOATemp
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = FollowOATempSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = FollowOATempSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

! Input the Follow System Node Temperature Setpoint Managers

IF (NumFollowSysNodeTempSetPtMgrs.GT.0) ALLOCATE(FollowSysNodeTempSetPtMgr(NumFollowSysNodeTempSetPtMgrs))

  ! Input the data for each Setpoint Manager
cCurrentModuleObject='SetpointManager:FollowSystemNodeTemperature'
DO SetPtMgrNum = 1,NumFollowSysNodeTempSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1),FollowSysNodeTempSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  FollowSysNodeTempSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlVarType = cAlphaArgs(2)
  IF (SameString(FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlVarType,'Temperature')) THEN
    FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_Temp
  ELSEIF (SameString(FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlVarType,'MaximumTemperature')) THEN
    FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_MaxTemp
  ELSEIF (SameString(FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlVarType,'MinimumTemperature')) THEN
    FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_MinTemp
  ELSE
     ! should not come here if idd type choice and key list is working
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
    CALL ShowContinueError('..Valid values are "Temperature","MaximumTemperature" or "MinimumTemperature".')
    ErrorsFound = .TRUE.
  ENDIF
  FollowSysNodeTempSetPtMgr(SetPtMgrNum)%RefNodeNum = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Unknown,NodeConnectionType_Sensor, 1, ObjectIsNotParent)
  FollowSysNodeTempSetPtMgr(SetPtMgrNum)%RefTempType = cAlphaArgs(4)
  IF (SameString(FollowSysNodeTempSetPtMgr(SetPtMgrNum)%RefTempType,'NodeWetBulb')) THEN
    FollowSysNodeTempSetPtMgr(SetPtMgrNum)%RefTypeMode = iRefTempType_WetBulb
  ELSEIF (SameString(FollowSysNodeTempSetPtMgr(SetPtMgrNum)%RefTempType,'NodeDryBulb')) THEN
    FollowSysNodeTempSetPtMgr(SetPtMgrNum)%RefTypeMode = iRefTempType_DryBulb
  ELSE
     ! should not come here if idd type choice and key list is working
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
    CALL ShowContinueError('..Valid values are "NodeWetBulb" or "NodeDryBulb".')
    ErrorsFound = .TRUE.
  ENDIF
  FollowSysNodeTempSetPtMgr(SetPtMgrNum)%Offset     = rNumericArgs(1)
  FollowSysNodeTempSetPtMgr(SetPtMgrNum)%MaxSetTemp = rNumericArgs(2)
  FollowSysNodeTempSetPtMgr(SetPtMgrNum)%MinSetTemp = rNumericArgs(3)
  IF (FollowSysNodeTempSetPtMgr(SetPtMgrNum)%MaxSetTemp < FollowSysNodeTempSetPtMgr(SetPtMgrNum)%MinSetTemp) THEN
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
    CALL ShowContinueError('...'//trim(cNumericFieldNames(2))//  &
         '=['//trim(RoundSigDigits(FollowSysNodeTempSetPtMgr(SetPtMgrNum)%MaxSetTemp,1))//'] is less than '//  &
         trim(cNumericFieldNames(3))//  &
         '=['//trim(RoundSigDigits(FollowSysNodeTempSetPtMgr(SetPtMgrNum)%MinSetTemp,1))//'].')
  ENDIF

  NodeListError=.false.
  CALL GetNodeNums(cAlphaArgs(5),NumNodes,NodeNums,NodeListError, &  ! setpoint nodes
       NodeType_Unknown,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(5))
  IF (.NOT. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    FollowSysNodeTempSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    FollowSysNodeTempSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
    ErrorsFound=.TRUE.
  ENDIF

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                   NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + &
                   NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + &
                   NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMGrs + NumMZHtgAverageSetPtMGrs + &
                   NumMZAverageMinHumSetPtMgrs + NumMZAverageMaxHumSetPtMgrs + NumMZMinHumSetPtMgrs + NumMZMaxHumSetPtMgrs + &
                   NumFollowOATempSetPtMgrs

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = FollowSysNodeTempSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_FollowSysNodeTemp
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = FollowSysNodeTempSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

! Input the Ground Temperature Setpoint Managers

IF (NumGroundTempSetPtMgrs.GT.0) ALLOCATE(GroundTempSetPtMgr(NumGroundTempSetPtMgrs))

  ! Input the data for each Setpoint Manager
cCurrentModuleObject='SetpointManager:FollowGroundTemperature'
DO SetPtMgrNum = 1,NumGroundTempSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1),GroundTempSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  GroundTempSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  GroundTempSetPtMgr(SetPtMgrNum)%CtrlVarType = cAlphaArgs(2)
  IF (SameString(GroundTempSetPtMgr(SetPtMgrNum)%CtrlVarType,'Temperature')) THEN
    GroundTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_Temp
  ELSEIF (SameString(GroundTempSetPtMgr(SetPtMgrNum)%CtrlVarType,'MaximumTemperature')) THEN
    GroundTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_MaxTemp
  ELSEIF (SameString(GroundTempSetPtMgr(SetPtMgrNum)%CtrlVarType,'MinimumTemperature')) THEN
    GroundTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_MinTemp
  ELSE
     ! should not come here if idd type choice and key list is working
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
    CALL ShowContinueError('..Valid values are "Temperature","MaximumTemperature" or "MinimumTemperature".')
    ErrorsFound = .TRUE.
  ENDIF
  GroundTempSetPtMgr(SetPtMgrNum)%RefGroundTempObjType = cAlphaArgs(3)
  IF (SameString(GroundTempSetPtMgr(SetPtMgrNum)%RefGroundTempObjType,'Site:GroundTemperature:BuildingSurface')) THEN
    GroundTempSetPtMgr(SetPtMgrNum)%RefTypeMode = iRefGroundTempObjType_BuildingSurface
    IF (NoSurfaceGroundTempObjWarning) THEN
      IF (.not. GroundTempObjInput) THEN
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'" requires '//  &
                              '"Site:GroundTemperature:BuildingSurface" in the input.')
        CALL ShowContinueError('Defaults, constant throughout the year of ('//TRIM(RoundSigDigits(GroundTemp,1))// &
                           ') will be used.')
      ENDIF
      NoSurfaceGroundTempObjWarning=.false.
    ENDIF
  ELSEIF (SameString(GroundTempSetPtMgr(SetPtMgrNum)%RefGroundTempObjType,'Site:GroundTemperature:Shallow')) THEN
    GroundTempSetPtMgr(SetPtMgrNum)%RefTypeMode = iRefGroundTempObjType_Shallow
    IF (NoShallowGroundTempObjWarning) THEN
      IF (.not. GroundTemp_SurfaceObjInput) THEN
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'" requires '//  &
                              '"Site:GroundTemperature:Shallow" in the input.')
        CALL ShowContinueError('Defaults, constant throughout the year of ('//TRIM(RoundSigDigits(GroundTemp_Surface,1))// &
                           ') will be used.')
      ENDIF
      NoShallowGroundTempObjWarning=.false.
    ENDIF
  ELSEIF (SameString(GroundTempSetPtMgr(SetPtMgrNum)%RefGroundTempObjType,'Site:GroundTemperature:Deep')) THEN
    GroundTempSetPtMgr(SetPtMgrNum)%RefTypeMode = iRefGroundTempObjType_Deep
    IF (NoDeepGroundTempObjWarning) THEN
      IF (.not. GroundTemp_DeepObjInput) THEN
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'" requires '//  &
                              '"Site:GroundTemperature:Deep" in the input.')
        CALL ShowContinueError('Defaults, constant throughout the year of ('//TRIM(RoundSigDigits(GroundTemp_Deep,1))// &
                           ') will be used.')
      ENDIF
      NoDeepGroundTempObjWarning=.false.
    ENDIF
  ELSEIF (SameString(GroundTempSetPtMgr(SetPtMgrNum)%RefGroundTempObjType,'Site:GroundTemperature:FCfactorMethod')) THEN
    GroundTempSetPtMgr(SetPtMgrNum)%RefTypeMode = iRefGroundTempObjType_FCfactorMethod
    IF (NoFCGroundTempObjWarning) THEN
      IF (.not. FCGroundTemps) THEN
        CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'" requires '//  &
                              '"Site:GroundTemperature:FCfactorMethod" in the input.')
        CALL ShowContinueError('Defaults, constant throughout the year of ('//TRIM(RoundSigDigits(GroundTempFC,1))// &
                           ') will be used.')
      ENDIF
      NoFCGroundTempObjWarning=.false.
    ENDIF
  ELSE
     ! should not come here if idd type choice and key list is working
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
    CALL ShowContinueError('..Valid values are "Site:GroundTemperature:BuildingSurface", "Site:GroundTemperature:Shallow",')
    CALL ShowContinueError('     "Site:GroundTemperature:Deep" or "Site:GroundTemperature:FCfactorMethod".')
    ErrorsFound = .TRUE.
  ENDIF
  GroundTempSetPtMgr(SetPtMgrNum)%Offset     = rNumericArgs(1)
  GroundTempSetPtMgr(SetPtMgrNum)%MaxSetTemp = rNumericArgs(2)
  GroundTempSetPtMgr(SetPtMgrNum)%MinSetTemp = rNumericArgs(3)
  IF (GroundTempSetPtMgr(SetPtMgrNum)%MaxSetTemp < GroundTempSetPtMgr(SetPtMgrNum)%MinSetTemp) THEN
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
    CALL ShowContinueError('...'//trim(cNumericFieldNames(2))//  &
         '=['//trim(RoundSigDigits(GroundTempSetPtMgr(SetPtMgrNum)%MaxSetTemp,1))//'] is less than '//  &
         trim(cNumericFieldNames(3))//  &
         '=['//trim(RoundSigDigits(GroundTempSetPtMgr(SetPtMgrNum)%MinSetTemp,1))//'].')
  ENDIF

  NodeListError=.false.
  CALL GetNodeNums(cAlphaArgs(4),NumNodes,NodeNums,NodeListError, &  ! setpoint nodes
       NodeType_Unknown,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(4))
  IF (.NOT. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(GroundTempSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    GroundTempSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    GroundTempSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      GroundTempSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
!       '", invalid field.')
!    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
    ErrorsFound=.TRUE.
  ENDIF

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                   NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + &
                   NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + &
                   NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMGrs + NumMZHtgAverageSetPtMGrs + &
                   NumMZAverageMinHumSetPtMgrs + NumMZAverageMaxHumSetPtMgrs + NumMZMinHumSetPtMgrs + NumMZMaxHumSetPtMgrs + &
                   NumFollowOATempSetPtMgrs + NumFollowSysNodeTempSetPtMgrs

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = GroundTempSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = GroundTempSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_GroundTemp
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = GroundTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = GroundTempSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Errors found in input.  Program terminates.')
ENDIF

DO SetPtMgrNum = 1,NumWarmestSetPtMgrsTempFlow
    CALL SetupOutputVariable('Setpoint Manager Warmest Temperature Critical Zone Number []', &
                             WarmestSetPtMgrTempFlow(SetPtMgrNum)%CritZoneNum,'System',&
                             'Average',WarmestSetPtMgrTempFlow(SetPtMgrNum)%Name)
    CALL SetupOutputVariable('Setpoint Manager Warmest Temperature Turndown Flow Fraction []', &
                             WarmestSetPtMgrTempFlow(SetPtMgrNum)%Turndown,'System',&
                             'Average',WarmestSetPtMgrTempFlow(SetPtMgrNum)%Name)
END DO

! Input the Condenser Entering Set Point Managers

IF (NumCondEntSetPtMgrs.GT.0) ALLOCATE(CondEntSetPtMgr(NumCondEntSetPtMgrs)) ! Allocate the Set Point Manager input data array

  ! Input the data for each Set Point Manager
cCurrentModuleObject='SetpointManager:CondenserEnteringReset'

DO SetPtMgrNum = 1,NumCondEntSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1),CondEntSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  CondEntSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  CondEntSetPtMgr(SetPtMgrNum)%CtrlVarType = cAlphaArgs(2)
  IF (SameString(CondEntSetPtMgr(SetPtMgrNum)%CtrlVarType,'Temperature')) THEN
    CondEntSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_Temp
  ELSE
    ! should not come here if idd type choice and key list is working
    CALL ShowSevereError(' found invalid control type of '//TRIM(cAlphaArgs(2)) &
                         //' in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
    ErrorsFound = .TRUE.
  ENDIF
  CondEntSetPtMgr(SetPtMgrNum)%CondEntTempSched       = cAlphaArgs(3)
  CondEntSetPtMgr(SetPtMgrNum)%CondEntTempSchedPtr    = GetScheduleIndex(cAlphaArgs(3))
  CondEntSetPtMgr(SetPtMgrNum)%MinTwrWbCurve       = GetCurveIndex(cAlphaArgs(4))
  CondEntSetPtMgr(SetPtMgrNum)%MinOaWbCurve        = GetCurveIndex(cAlphaArgs(5))
  CondEntSetPtMgr(SetPtMgrNum)%OptCondEntCurve     = GetCurveIndex(cAlphaArgs(6))
  CondEntSetPtMgr(SetPtMgrNum)%MinimumLiftTD       = rNumericArgs(1)
  CondEntSetPtMgr(SetPtMgrNum)%MaxCondEntTemp      = rNumericArgs(2)
  CondEntSetPtMgr(SetPtMgrNum)%TowerDsnInletAirWetBulb   = rNumericArgs(3)
  CondEntSetPtMgr(SetPtMgrNum)%CtrlNodeListName = cAlphaArgs(7)
  IF (CondEntSetPtMgr(SetPtMgrNum)%MaxCondEntTemp < CondEntSetPtMgr(SetPtMgrNum)%TowerDsnInletAirWetBulb) THEN
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
    CALL ShowContinueError('...'//trim(cNumericFieldNames(2))//  &
         '=['//trim(RoundSigDigits(CondEntSetPtMgr(SetPtMgrNum)%MaxCondEntTemp,1))//'] is less than '//  &
         trim(cNumericFieldNames(1))//  &
         '=['//trim(RoundSigDigits(CondEntSetPtMgr(SetPtMgrNum)%TowerDsnInletAirWetBulb,1))//'].')
  ENDIF


  NodeListError=.false.
  CALL GetNodeNums(CondEntSetPtMgr(SetPtMgrNum)%CtrlNodeListName,NumNodes,NodeNums,NodeListError, &
       NodeType_Unknown,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(7))
  IF (.not. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(CondEntSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    CondEntSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    CondEntSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      CondEntSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowContinueError('Invalid '//trim(cAlphaFieldNames(3))//' in '//TRIM(cCurrentModuleObject)//' = '// &
!                           TRIM(CondEntSetPtMgr(SetPtMgrNum)%Name))
    ErrorsFound=.TRUE.
  ENDIF


    AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                   NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + &
                   NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + &
                   NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMGrs + NumMZHtgAverageSetPtMGrs + &
                   NumMZAverageMinHumSetPtMgrs + NumMZAverageMaxHumSetPtMgrs + NumMZMinHumSetPtMgrs + NumMZMaxHumSetPtMgrs + &
                   NumFollowOATempSetPtMgrs + NumFollowSysNodeTempSetPtMgrs + NumGroundTempSetPtMgrs



  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = CondEntSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = CondEntSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_CondEntReset
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = CondEntSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = CondEntSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

! Input the Ideal Condenser Entering Set Point Managers

 ! Allocate the Set Point Manager input data array
IF (NumIdealCondEntSetPtMgrs.GT.0) ALLOCATE(IdealCondEntSetPtMgr(NumIdealCondEntSetPtMgrs))

  ! Input the data for each Set Point Manager
cCurrentModuleObject='SetpointManager:CondenserEnteringReset:Ideal'

DO SetPtMgrNum = 1,NumIdealCondEntSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1),IdealCondEntSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  IdealCondEntSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  IdealCondEntSetPtMgr(SetPtMgrNum)%CtrlVarType = cAlphaArgs(2)
  IF (SameString(IdealCondEntSetPtMgr(SetPtMgrNum)%CtrlVarType,'Temperature')) THEN
    IdealCondEntSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_Temp
  ELSE
    CALL ShowSevereError(' found invalid control type of '//TRIM(cAlphaArgs(2)) &
                         //' in '//TRIM(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) )
    ErrorsFound = .TRUE.
  ENDIF
  IdealCondEntSetPtMgr(SetPtMgrNum)%MinimumLiftTD = rNumericArgs(1)
  IdealCondEntSetPtMgr(SetPtMgrNum)%MaxCondEntTemp = rNumericArgs(2)
  IdealCondEntSetPtMgr(SetPtMgrNum)%CtrlNodeListName = cAlphaArgs(3)

  NodeListError=.false.
  CALL GetNodeNums(IdealCondEntSetPtMgr(SetPtMgrNum)%CtrlNodeListName,NumNodes,NodeNums,NodeListError, &
       NodeType_Unknown,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(3))
  IF (.not. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(IdealCondEntSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    IdealCondEntSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    IdealCondEntSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      IdealCondEntSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
!    CALL ShowContinueError('Invalid '//trim(cAlphaFieldNames(3))//' in '//TRIM(cCurrentModuleObject)//' = '// &
!                           TRIM(IdealCondEntSetPtMgr(SetPtMgrNum)%Name))
    ErrorsFound=.TRUE.
  ENDIF

    AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                   NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + &
                   NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + &
                   NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMGrs + NumMZHtgAverageSetPtMGrs + &
                   NumMZAverageMinHumSetPtMgrs + NumMZAverageMaxHumSetPtMgrs + NumMZMinHumSetPtMgrs + NumMZMaxHumSetPtMgrs + &
                   NumFollowOATempSetPtMgrs + NumFollowSysNodeTempSetPtMgrs + NumGroundTempSetPtMgrs + NumCondEntSetPtMgrs

  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = IdealCondEntSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = IdealCondEntSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_IdealCondEntReset
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = IdealCondEntSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = IdealCondEntSetPtMgr(SetPtMgrNum)%NumCtrlNodes

END DO

IF (NumSZOneStageCoolingSetPtMgrs > 0) ALLOCATE ( SZOneStageCoolingSetPtMgr(NumSZOneStageCoolingSetPtMgrs))

cCurrentModuleObject='SetpointManager:SingleZone:OneStageCooling'
DO SetPtMgrNum = 1,NumSZOneStageCoolingSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1),SZOneStageCoolingSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  SZOneStageCoolingSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  SZOneStageCoolingSetPtMgr(SetPtMgrNum)%CtrlVarType = 'Temperature'
  SZOneStageCoolingSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_Temp
  SZOneStageCoolingSetPtMgr(SetPtMgrNum)%CoolingOnTemp  = rNumericArgs(1)
  SZOneStageCoolingSetPtMgr(SetPtMgrNum)%CoolingOffTemp = rNumericArgs(2)


  IF (SZOneStageCoolingSetPtMgr(SetPtMgrNum)%CoolingOffTemp < SZOneStageCoolingSetPtMgr(SetPtMgrNum)%CoolingOnTemp ) THEN
    ! throw warning, off must be warmer than on
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
    CALL ShowContinueError('...'//trim(cNumericFieldNames(2))//  &
         '=['//trim(RoundSigDigits( SZOneStageCoolingSetPtMgr(SetPtMgrNum)%CoolingOffTemp,1))//'] is less than '//  &
         trim(cNumericFieldNames(1))//  &
         '=['//trim(RoundSigDigits(SZOneStageCoolingSetPtMgr(SetPtMgrNum)%CoolingOnTemp,1))//'].')
  ENDIF

  SZOneStageCoolingSetPtMgr(SetPtMgrNum)%ControlZoneName = cAlphaArgs(2)
  SZOneStageCoolingSetPtMgr(SetPtMgrNum)%ZoneNodeNum = GetSystemNodeNumberForZone(cAlphaArgs(2))
  ! get the actual zone number of the control zone
  SZOneStageCoolingSetPtMgr(SetPtMgrNum)%ControlZoneNum = FindItemInList(cAlphaArgs(2),Zone%Name,NumOfZones)
  IF (SZOneStageCoolingSetPtMgr(SetPtMgrNum)%ControlZoneNum == 0) THEN
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
    ErrorsFound=.TRUE.
  ELSE
    If (ALLOCATED(StageZoneLogic)) THEN
      If (.NOT. StageZoneLogic(SZOneStageCoolingSetPtMgr(SetPtMgrNum)%ControlZoneNum)) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '", invalid field.')
        CALL ShowContinueError('..invalid '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
        CALL ShowContinueError('Zone thermostat must use ZoneControl:Thermostat:StagedDualSetpoint.')
        ErrorsFound=.TRUE.
      ENDIF
    ENDIF

  ENDIF

  NodeListError=.false.
  CALL GetNodeNums(cAlphaArgs(3),NumNodes,NodeNums,NodeListError, & ! setpoint nodes
       NodeType_Unknown,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(3))
  IF (.NOT. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(SZOneStageCoolingSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    SZOneStageCoolingSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    SZOneStageCoolingSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      SZOneStageCoolingSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
    ErrorsFound=.TRUE.
  ENDIF

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                     NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + &
                     NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + &
                     NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMGrs + NumMZHtgAverageSetPtMGrs + &
                     NumMZAverageMinHumSetPtMgrs + NumMZAverageMaxHumSetPtMgrs + NumMZMinHumSetPtMgrs + NumMZMaxHumSetPtMgrs + &
                     NumFollowOATempSetPtMgrs + NumFollowSysNodeTempSetPtMgrs + NumGroundTempSetPtMgrs + NumCondEntSetPtMgrs + &
                     NumIdealCondEntSetPtMgrs
  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = SZOneStageCoolingSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = SZOneStageCoolingSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_SZOneStageCooling
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = SZOneStageCoolingSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = SZOneStageCoolingSetPtMgr(SetPtMgrNum)%NumCtrlNodes

ENDDO

IF (NumSZOneStageHeatingSetPtMgrs > 0) ALLOCATE ( SZOneStageHeatingSetPtMgr(NumSZOneStageHeatingSetPtMgrs))

cCurrentModuleObject='SetpointManager:SingleZone:OneStageHeating'
DO SetPtMgrNum = 1,NumSZOneStageHeatingSetPtMgrs
  CALL GetObjectItem(cCurrentModuleObject,SetPtMgrNum,cAlphaArgs,NumAlphas,&
                     rNumericArgs,NumNums,IOStat,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                     AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

  IsNotOK=.false.
  IsBlank=.false.
  CALL VerifyName(cAlphaArgs(1),SZOneStageHeatingSetPtMgr%Name,SetPtMgrNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
  IF (IsNotOK) THEN
    ErrorsFound=.TRUE.
    IF (IsBlank) cAlphaArgs(1)='xxxxx'
  ENDIF
  SZOneStageHeatingSetPtMgr(SetPtMgrNum)%Name = cAlphaArgs(1)
  SZOneStageHeatingSetPtMgr(SetPtMgrNum)%CtrlVarType = 'Temperature'
  SZOneStageHeatingSetPtMgr(SetPtMgrNum)%CtrlTypeMode = iCtrlVarType_Temp
  SZOneStageHeatingSetPtMgr(SetPtMgrNum)%HeatingOnTemp  = rNumericArgs(1)
  SZOneStageHeatingSetPtMgr(SetPtMgrNum)%HeatingOffTemp = rNumericArgs(2)


  IF (SZOneStageHeatingSetPtMgr(SetPtMgrNum)%HeatingOffTemp > SZOneStageHeatingSetPtMgr(SetPtMgrNum)%HeatingOnTemp ) THEN
    ! throw warning, off must be cooler than on
    CALL ShowWarningError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'",')
    CALL ShowContinueError('...'//trim(cNumericFieldNames(2))//  &
         '=['//trim(RoundSigDigits( SZOneStageHeatingSetPtMgr(SetPtMgrNum)%HeatingOnTemp,1))//'] is less than '//  &
         trim(cNumericFieldNames(1))//  &
         '=['//trim(RoundSigDigits(SZOneStageHeatingSetPtMgr(SetPtMgrNum)%HeatingOffTemp,1))//'].')
  ENDIF

  SZOneStageHeatingSetPtMgr(SetPtMgrNum)%ControlZoneName = cAlphaArgs(2)
  SZOneStageHeatingSetPtMgr(SetPtMgrNum)%ZoneNodeNum = GetSystemNodeNumberForZone(cAlphaArgs(2))
  ! get the actual zone number of the control zone
  SZOneStageHeatingSetPtMgr(SetPtMgrNum)%ControlZoneNum = FindItemInList(cAlphaArgs(2),Zone%Name,NumOfZones)
  IF (SZOneStageHeatingSetPtMgr(SetPtMgrNum)%ControlZoneNum == 0) THEN
    CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
       '", invalid field.')
    Call ShowContinueError('..invalid '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
    ErrorsFound=.TRUE.
  ELSE
    If (ALLOCATED(StageZoneLogic)) THEN
      If (.NOT. StageZoneLogic(SZOneStageHeatingSetPtMgr(SetPtMgrNum)%ControlZoneNum)) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '", invalid field.')
        CALL ShowContinueError('..invalid '//trim(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
        CALL ShowContinueError('Zone thermostat must use ZoneControl:Thermostat:StagedDualSetpoint.')
        ErrorsFound=.TRUE.
      ENDIF
    ENDIF
  ENDIF

  NodeListError=.false.
  CALL GetNodeNums(cAlphaArgs(3),NumNodes,NodeNums,NodeListError, & ! setpoint nodes
       NodeType_Unknown,TRIM(cCurrentModuleObject),cAlphaArgs(1),NodeConnectionType_Setpoint,1,ObjectIsNotParent,  &
       InputFieldName=cAlphaFieldNames(3))
  IF (.NOT. NodeListError) THEN
    NumNodesCtrld = NumNodes
    ALLOCATE(SZOneStageHeatingSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    SZOneStageHeatingSetPtMgr(SetPtMgrNum)%NumCtrlNodes = NumNodesCtrld
    SZOneStageHeatingSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0

    DO CtrldNodeNum = 1,NumNodesCtrld
      SZOneStageHeatingSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) = NodeNums(CtrldNodeNum)
    END DO
  ELSE
    ErrorsFound=.TRUE.
  ENDIF

  AllSetPtMgrNum = SetPtMgrNum + NumSchSetPtMgrs + NumDualSchSetPtMgrs + NumOutAirSetPtMgrs + NumSZRhSetPtMgrs + &
                     NumSZHtSetPtMgrs + NumSZClSetPtMgrs + NumSZMinHumSetPtMgrs + NumSZMaxHumSetPtMgrs + &
                     NumMixedAirSetPtMgrs + NumOAPretreatSetPtMgrs + NumWarmestSetPtMgrs + NumColdestSetPtMgrs + &
                     NumWarmestSetPtMgrsTempFlow + NumRABFlowSetPtMgrs + NumMZClgAverageSetPtMGrs + NumMZHtgAverageSetPtMGrs + &
                     NumMZAverageMinHumSetPtMgrs + NumMZAverageMaxHumSetPtMgrs + NumMZMinHumSetPtMgrs + NumMZMaxHumSetPtMgrs + &
                     NumFollowOATempSetPtMgrs + NumFollowSysNodeTempSetPtMgrs + NumGroundTempSetPtMgrs + NumCondEntSetPtMgrs + &
                     NumIdealCondEntSetPtMgrs + NumSZOneStageCoolingSetPtMgrs
  IF (.not. NodeListError) THEN
    ALLOCATE(AllSetPtMgr(AllSetPtMgrNum)%CtrlNodes(NumNodesCtrld))
    AllSetPtMgr(AllsetPtMgrNum)%CtrlNodes  = SZOneStageHeatingSetPtMgr(SetPtMgrNum)%CtrlNodes
  END IF
  AllSetPtMgr(AllsetPtMgrNum)%Name         = SZOneStageHeatingSetPtMgr(SetPtMgrNum)%Name
  AllSetPtMgr(AllsetPtMgrNum)%SPMType      = iSPMType_SZOneStageHeating
  AllSetPtMgr(AllsetPtMgrNum)%CtrlTypeMode = SZOneStageHeatingSetPtMgr(SetPtMgrNum)%CtrlTypeMode
  AllSetPtMgr(AllsetPtMgrNum)%NumCtrlNodes = SZOneStageHeatingSetPtMgr(SetPtMgrNum)%NumCtrlNodes

ENDDO



DEALLOCATE(cAlphaFieldNames)
DEALLOCATE(cAlphaArgs)
DEALLOCATE(lAlphaFieldBlanks)
DEALLOCATE(cNumericFieldNames)
DEALLOCATE(rNumericArgs)
DEALLOCATE(lNumericFieldBlanks)

IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Errors found in input.  Program terminates.')
ENDIF

RETURN

END SUBROUTINE GetSetPointManagerInputs

SUBROUTINE VerifySetPointManagers(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   July 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Check the SetPointManager data to eliminate conflicts.

          ! METHODOLOGY EMPLOYED:
          ! 1) Check for duplicate names in individual setpoint managers.
          !
          ! Control nodes = A B C D
          ! Check A with B, C, and D
          ! Check B with C and D
          ! Check C with D
          !
          ! 2) Check for duplicate names in all other setpoint managers
          !    Verify setpoint managers use same control type (e.g. TEMP) and then check for duplicate nodes
          !
          ! SPM 1 - Control nodes A - D, SPM 2 - Control nodes E - H, SPM 3 - Control nodes I - L
          ! If SPM 1 has same control type as SPM 2 and SPM 3 (e.g. all use SPM%CtrlTypeMode = iCtrlVarType_Temp) then:
          ! Check A with E-H and I-L
          ! Check B with E-H and I-L
          ! Check C with E-H and I-L
          ! Check D with E-H and I-L
          !
          ! Then check SPM 2 nodes with SPM 3. Check E with I-L, F with I-L, etc.
          !
          ! 3) For SET POINT MANAGER:RETURN AIR BYPASS FLOW
          !    check for duplicate air loop names.
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE InputProcessor, ONLY: SameString

    IMPLICIT NONE

          ! SUBROUTINE PARAMETER DEFINITIONS:
LOGICAL, INTENT(INOUT) :: ErrorsFound ! flag to denote node conflicts in input. !unused1208

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER :: SetPtMgrNum         ! Setpoint Manager index
INTEGER :: TempSetPtMgrNum     ! Setpoint Manager index for warning messages
INTEGER :: CtrldNodeNum        ! index of the items in the controlled node node list
INTEGER :: TempCtrldNodeNum    ! index of the items in the controlled node node list, used for warning messages

DO SetPtMgrNum = 1, NumAllSetPtMgrs

! check for duplicate nodes in each setpoint managers control node list (node lists of size 1 do not need verification)
! issue warning only since duplicate node names within a setpoint manager does not cause a conflict (i.e., same
! value written to node) but may indicate an error in the node name.
  DO CtrldNodeNum = 1, AllSetPtMgr(SetPtMgrNum)%NumCtrlNodes - 1
    DO TempCtrldNodeNum = CtrldNodeNum+1, AllSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      IF(AllSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) /= AllSetPtMgr(SetPtMgrNum)%CtrlNodes(TempCtrldNodeNum))CYCLE
      CALL ShowWarningError(TRIM(cValidSPMTypes(AllSetPtMgr(SetPtMgrNum)%SPMType))//'="'//TRIM(AllSetPtMgr(SetPtMgrNum)%Name)//'"')
      CALL ShowContinueError('...duplicate node specified = '//TRIM(NodeID(AllSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum))))
      CALL ShowContinueError('...control type variable    = '//TRIM(cValidCtrlTypes(AllSetPtMgr(SetPtMgrNum)%CtrlTypeMode)))
    END DO
  END DO

! check for node conflicts in all other setpoint managers
  DO TempSetPtMgrNum = SetPtMgrNum+1, NumAllSetPtMgrs

!   check the air loop name in addition to the node names for these SP manager types
!    IF((AllSetPtMgr(SetPtMgrNum)%SPMType == iSPMType_WarmestTempFlow .AND. &
!        AllSetPtMgr(TempSetPtMgrNum)%SPMType == iSPMType_WarmestTempFlow) .OR. &
!       (AllSetPtMgr(SetPtMgrNum)%SPMType == iSPMType_RAB .AND. &
!        AllSetPtMgr(TempSetPtMgrNum)%SPMType == iSPMType_RAB) .OR. &
!       (AllSetPtMgr(SetPtMgrNum)%SPMType == iSPMType_Coldest .AND. &
!        AllSetPtMgr(TempSetPtMgrNum)%SPMType == iSPMType_Coldest) .OR. &
!       (AllSetPtMgr(SetPtMgrNum)%SPMType == iSPMType_Warmest .AND. &
!        AllSetPtMgr(TempSetPtMgrNum)%SPMType == iSPMType_Warmest))THEN
    IF((AllSetPtMgr(SetPtMgrNum)%SPMType == iSPMType_RAB .AND. &
        AllSetPtMgr(TempSetPtMgrNum)%SPMType == iSPMType_RAB))THEN

!     check the air loop name for duplicates in this SP manager type
      IF(ALLSetPtMgr(SetPtMgrNum)%AirLoopNum == AllSetPtMgr(TempSetPtMgrNum)%AirLoopNum)THEN
        CALL ShowWarningError(TRIM(cValidSPMTypes(AllSetPtMgr(SetPtMgrNum)%SPMType))//'="'// &
                             TRIM(AllSetPtMgr(SetPtMgrNum)%Name)//'"')
        CALL ShowContinueError('...air loop name conflicts with another setpoint manager.')
        CALL ShowContinueError('...conflicting setpoint manager = '// &
                             TRIM(cValidSPMTypes(AllSetPtMgr(TempSetPtMgrNum)%SPMType))//' "'// &
                             TRIM(AllSetPtMgr(TempSetPtMgrNum)%Name)//'"')
        CALL ShowContinueError('...conflicting air loop name = '// &
                             TRIM(AllSetPtMgr(SetPtMgrNum)%AirLoopName))
!        ErrorsFound=.TRUE.
      END IF

!     check for duplicate control nodes
      IF(AllSetPtMgr(SetPtMgrNum)%CtrlTypeMode /= AllSetPtMgr(TempSetPtMgrNum)%CtrlTypeMode)CYCLE

      DO CtrldNodeNum = 1, AllSetPtMgr(SetPtMgrNum)%NumCtrlNodes
        DO TempCtrldNodeNum = 1, AllSetPtMgr(TempSetPtMgrNum)%NumCtrlNodes
          IF((AllSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) == &
             AllSetPtMgr(TempSetPtMgrNum)%CtrlNodes(TempCtrldNodeNum)) .AND. &
             AllSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) .NE. 0)THEN
            CALL ShowWarningError(TRIM(cValidSPMTypes(AllSetPtMgr(SetPtMgrNum)%SPMType))//'="'// &
                                 TRIM(AllSetPtMgr(SetPtMgrNum)%Name)//'"')
            CALL ShowContinueError('...setpoint node conflicts with another setpoint manager.')
            CALL ShowContinueError('...conflicting setpoint manager = '// &
                                 TRIM(cValidSPMTypes(AllSetPtMgr(TempSetPtMgrNum)%SPMType))//' "'// &
                                 TRIM(AllSetPtMgr(TempSetPtMgrNum)%Name)//'"')
            CALL ShowContinueError('...conflicting node name = '// &
                                 TRIM(NodeID(AllSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum))))
            CALL ShowContinueError('...control type variable = '//TRIM(cValidCtrlTypes(AllSetPtMgr(SetPtMgrNum)%CtrlTypeMode)))
!            ErrorsFound=.TRUE.
          END IF
        END DO
      END DO

    ELSE ! not a RAB setpoint manager

!     check just the control nodes for other types of SP managers
      IF(AllSetPtMgr(SetPtMgrNum)%CtrlTypeMode /= AllSetPtMgr(TempSetPtMgrNum)%CtrlTypeMode)CYCLE

      DO CtrldNodeNum = 1, AllSetPtMgr(SetPtMgrNum)%NumCtrlNodes
        DO TempCtrldNodeNum = 1, AllSetPtMgr(TempSetPtMgrNum)%NumCtrlNodes

          IF(AllSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum) /= &
             AllSetPtMgr(TempSetPtMgrNum)%CtrlNodes(TempCtrldNodeNum))CYCLE

!         only warn if scheduled setpoint manager is setting mass flow rate on the same node used by RAB
          IF(AllSetPtMgr(SetPtMgrNum)%SPMType == iSPMType_RAB .OR. AllSetPtMgr(TempSetPtMgrNum)%SPMType == iSPMType_RAB)THEN
            CALL ShowWarningError(TRIM(cValidSPMTypes(AllSetPtMgr(SetPtMgrNum)%SPMType))//'="'// &
                                  TRIM(AllSetPtMgr(SetPtMgrNum)%Name)//'"')
            CALL ShowContinueError('...setpoint node conflicts with another setpoint manager.')
            CALL ShowContinueError('...conflicting setpoint manager ='// &
                                  TRIM(cValidSPMTypes(AllSetPtMgr(TempSetPtMgrNum)%SPMType))//':"'// &
                                  TRIM(AllSetPtMgr(TempSetPtMgrNum)%Name)//'"')
            CALL ShowContinueError('...conflicting node name = '//TRIM(NodeID(AllSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum))))
            CALL ShowContinueError('...control type variable = '//TRIM(cValidCtrlTypes(AllSetPtMgr(SetPtMgrNum)%CtrlTypeMode)))
            CALL ShowContinueError('...return air bypass flow setpoint manager will have priority setting mass flow rate'// &
                                   ' on this node.')
          ELSE ! severe error for other SP manager types
            CALL ShowWarningError(TRIM(cValidSPMTypes(AllSetPtMgr(SetPtMgrNum)%SPMType))//'="'// &
                                 TRIM(AllSetPtMgr(SetPtMgrNum)%Name)//'"')
            CALL ShowContinueError('...setpoint node conflicts with another setpoint manager.')
            CALL ShowContinueError('...conflicting setpoint manager = '// &
                                 TRIM(cValidSPMTypes(AllSetPtMgr(TempSetPtMgrNum)%SPMType))//':"'// &
                                 TRIM(AllSetPtMgr(TempSetPtMgrNum)%Name)//'"')
            CALL ShowContinueError('...conflicting node name = '//TRIM(NodeID(AllSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrldNodeNum))))
            CALL ShowContinueError('...control type variable = '//TRIM(cValidCtrlTypes(AllSetPtMgr(SetPtMgrNum)%CtrlTypeMode)))
!            ErrorsFound=.TRUE.
          END IF
        END DO
      END DO
    END IF

  END DO ! DO TempSetPtMgrNum = SetPtMgrNum+1, AllSetPtMgrs

END DO ! DO SetPtMgrNum = 1, AllSetPtMgrs

IF (ALLOCATED(AllSetPtMgr)) DEALLOCATE(AllSetPtMgr)

RETURN

END SUBROUTINE VerifySetPointManagers

SUBROUTINE InitSetPointManagers

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   October 2000
          !       MODIFIED       Shirey/Raustad (FSEC), Jan 2004
          !                      Nov 2004 - Jan 2005 M. J. Witte, GARD Analytics, Inc.
          !                        Add new setpoint managers:
          !                          SET POINT MANAGER:SINGLE ZONE HEATING and
          !                          SET POINT MANAGER:SINGLE ZONE COOLING
          !                          SET POINT MANAGER:OUTSIDE AIR PRETREAT
          !                        Work supported by ASHRAE research project 1254-RP
          !                      Haves Oct 2004
          !                      July 2010 B.A. Nigusse, FSEC/UCF
          !                        Added new setpoint managers:
          !                          SetpointManager:MultiZone:Heating:Average
          !                          SetpointManager:MultiZone:Cooling:Average
          !                          SetpointManager:MultiZone:MinimumHumidity:Average
          !                          SetpointManager:MultiZone:MaximumHumidity:Average
          !                      Aug 2010 B.A. Nigusse, FSEC/UCF
          !                        Added new setpoint managers:
          !                          SetpointManager:MultiZone:Humidity:Minimum
          !                          SetpointManager:MultiZone:Humidity:Maximum
          !                      Sep 2010 B.A. Nigusse, FSEC/UCF
          !                         Added control varibles for SetpointManage:Scheduled
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Setpoint Manager objects.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: ZoneEquipConfig, ZoneEquipInputsFilled
  USE DataZoneControls, ONLY: HumidityControlZone, NumHumidityControlZones
  USE InputProcessor, ONLY: FindItemInList
  USE DataAirSystems, ONLY: PrimaryAirSystem
  USE DataHeatBalance, ONLY: Zone
  USE DataHVACGlobals,  ONLY : NumPlantLoops, NumCondLoops
  USE DataPlant
  USE InputProcessor, ONLY: SameString
  USE DataEnvironment, ONLY: GroundTemp_Deep, GroundTemp,GroundTemp_Surface, GroundTempFC
  USE OutAirNodeManager,     ONLY: CheckOutAirNodeNumber

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! NA

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
LOGICAL,SAVE  :: MyOneTimeFlag = .TRUE.
Logical,SAVE  :: MyEnvrnFlag = .TRUE.   ! flag for init once at start of environment
LOGICAL,SAVE  :: MyOneTimeFlag2 = .TRUE.

INTEGER  :: SetZoneNum
INTEGER  :: ControlledZoneNum
INTEGER  :: ZoneNode
INTEGER  :: ZoneInletNode
INTEGER  :: SetPtMgrNum
INTEGER  :: ZoneIndex
INTEGER  :: CtrlNodeIndex
INTEGER  :: NodeNum
INTEGER  :: AirLoopNum
INTEGER  :: LoopNum
INTEGER  :: LoopNum2
LOGICAL  :: ErrorsFound=.false.
INTEGER  :: ConZoneNum
INTEGER  :: MixedAirNode
INTEGER  :: BranchNum
INTEGER  :: BranchNum2
INTEGER  :: InletBranchNum
INTEGER  :: CompNum
INTEGER  :: CompNum2
Logical  :: LookForFan = .FALSE.
CHARACTER(len=MaxNameLength) :: CompType
CHARACTER(len=MaxNameLength) :: cSetPointManagerType
INTEGER  :: FanNodeIn
INTEGER  :: FanNodeOut
INTEGER  :: LoopInNode
INTEGER  :: HstatZoneNum
LOGICAL  :: HstatZoneFound
INTEGER  :: ZonesCooledIndex         ! Cooled zones index in an air loop
INTEGER  :: TotalBranches
INTEGER  :: TotalComponents
INTEGER  :: BranchNumPlantSide
INTEGER  :: CompNumPlantSide
INTEGER  :: VarNum
!INTEGER  :: ChillerIndexPlantSide    = 0
!INTEGER  :: ChillerIndexDemandSide   = 0
!INTEGER  :: BranchIndexPlantSide     = 0
!INTEGER  :: BranchIndexDemandSide    = 0
!INTEGER  :: LoopIndexPlantSide       = 0
!INTEGER  :: LoopIndexDemandSide      = 0
INTEGER  :: TypeNum                  = 0
INTEGER  :: TowerNum                 = 0
INTEGER  :: CondLoopNum              = 0
INTEGER  :: CondBranchNum            = 0
INTEGER  :: NumChiller               = 0
INTEGER  :: NumCT                    = 0
INTEGER  :: TypeOf_Num               =0
ManagerOn = .TRUE.

! One time initializations

IF (ZoneEquipInputsFilled .and. AirLoopInputsFilled) THEN ! check that the zone equipment and air loop data has been read in

  IF (MyOneTimeFlag) THEN

    ! Minimum humidity setpoint managers
    cSetPointManagerType = cValidSPMTypes(iSPMType_SZMinHum)
    DO SetPtMgrNum=1,NumSZMinHumSetPtMgrs
      DO SetZoneNum = 1,SZMinHumSetPtMgr(SetPtMgrNum)%NumZones
        ! set the actual and controlled zone numbers
        DO ControlledZoneNum = 1,NumOfZones
          IF (ZoneEquipConfig(ControlledZoneNum)%ZoneNode .EQ. SZMinHumSetPtMgr(SetPtMgrNum)%ZoneNodes(SetZoneNum) ) THEN
            SZMinHumSetPtMgr(SetPtMgrNum)%CtrlZoneNum(SetZoneNum) = ControlledZoneNum
            SZMinHumSetPtMgr(SetPtMgrNum)%ZoneNum(SetZoneNum) = ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum
            EXIT
          END IF
        END DO
        ! still need to validate...
        IF (SZMinHumSetPtMgr(SetPtMgrNum)%CtrlZoneNum(SetZoneNum) == 0) THEN  ! didn't find
          CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//  &
             trim(SZMinHumSetPtMgr(SetPtMgrNum)%Name)//'", invalid zone')
          CALL ShowContinueError('could not find Controlled Zone='// &
               TRIM(Zone(SZMinHumSetPtMgr(SetPtMgrNum)%ZoneNum(SetZoneNum))%Name))
          ErrorsFound=.TRUE.
        ELSE
          ! make sure humidity controlled zone
          HstatZoneFound=.false.
          DO HstatZoneNum = 1, NumHumidityControlZones
            IF(HumidityControlZone(HstatZoneNum)%ActualZoneNum .NE. SZMinHumSetPtMgr(SetPtMgrNum)%ZoneNum(SetZoneNum))CYCLE
            HstatZoneFound=.TRUE.
            EXIT
          END DO
          IF (.not. HstatZoneFound) THEN
            CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//  &
               trim(SZMinHumSetPtMgr(SetPtMgrNum)%Name)//'", invalid humidistat specification')
            CALL ShowContinueError('could not locate Humidistat in Zone='// &
               TRIM(Zone(SZMinHumSetPtMgr(SetPtMgrNum)%ZoneNum(SetZoneNum))%Name))
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF
      END DO
    END DO

    ! Maximum humidity setpoint managers
    cSetPointManagerType = cValidSPMTypes(iSPMType_SZMaxHum)
    DO SetPtMgrNum=1,NumSZMaxHumSetPtMgrs
      DO SetZoneNum = 1,SZMaxHumSetPtMgr(SetPtMgrNum)%NumZones
        ! set the actual and controlled zone numbers
        DO ControlledZoneNum = 1,NumOfZones
          IF (ZoneEquipConfig(ControlledZoneNum)%ZoneNode .EQ. SZMaxHumSetPtMgr(SetPtMgrNum)%ZoneNodes(SetZoneNum) ) THEN
            SZMaxHumSetPtMgr(SetPtMgrNum)%CtrlZoneNum(SetZoneNum) = ControlledZoneNum
            SZMaxHumSetPtMgr(SetPtMgrNum)%ZoneNum(SetZoneNum) = ZoneEquipConfig(ControlledZoneNum)%ActualZoneNum
            EXIT
          END IF
        END DO
        ! still need to validate...
        IF (SZMaxHumSetPtMgr(SetPtMgrNum)%CtrlZoneNum(SetZoneNum) == 0) THEN  ! didn't find
          CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//  &
             trim(SZMaxHumSetPtMgr(SetPtMgrNum)%Name)//'", invalid zone')
          CALL ShowContinueError('could not find Controlled Zone='// &
               TRIM(Zone(SZMaxHumSetPtMgr(SetPtMgrNum)%ZoneNum(SetZoneNum))%Name))
          ErrorsFound=.TRUE.
        ELSE
          ! make sure humidity controlled zone
          HstatZoneFound=.false.
          DO HstatZoneNum = 1, NumHumidityControlZones
            IF(HumidityControlZone(HstatZoneNum)%ActualZoneNum .NE. SZMaxHumSetPtMgr(SetPtMgrNum)%ZoneNum(SetZoneNum))CYCLE
            HstatZoneFound=.TRUE.
            EXIT
          END DO
          IF (.not. HstatZoneFound) THEN
            CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//  &
               trim(SZMaxHumSetPtMgr(SetPtMgrNum)%Name)//'", invalid humidistat specification')
            CALL ShowContinueError('could not locate Humidistat in Zone='// &
               TRIM(Zone(SZMaxHumSetPtMgr(SetPtMgrNum)%ZoneNum(SetZoneNum))%Name))
            ErrorsFound=.TRUE.
          ENDIF
        ENDIF
      END DO
    END DO

    ! single zone reheat setpoint manager
    cSetPointManagerType = cValidSPMTypes(iSPMType_SZReheat)
    DO SetPtMgrNum=1,NumSZRhSetPtMgrs
      FanNodeIn = 0
      FanNodeOut = 0
      MixedAirNode = 0
      AirLoopNum = 0
      InletBranchNum = 0
      LoopInNode = 0
      LookForFan = .FALSE.
      ZoneInletNode = SingZoneRhSetPtMgr(SetPtMgrNum)%ZoneInletNodeNum
      ZoneNode = SingZoneRhSetPtMgr(SetPtMgrNum)%ZoneNodeNum
      ! find the index in the ZoneEquipConfig array of the control zone (the one with the main or only thermostat)
      ConZoneNum=0
      DO ControlledZoneNum = 1,NumOfZones
        IF (ZoneEquipConfig(ControlledZoneNum)%ZoneNode .EQ. ZoneNode ) THEN
          ConZoneNum = ControlledZoneNum
        END IF
      END DO
      IF (ConZoneNum == 0) THEN
        CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(SingZoneRhSetPtMgr(SetPtMgrNum)%Name)//  &
           '", Zone Node not found:')
        CALL ShowContinueError('Node="'//TRIM(NodeID(SingZoneRhSetPtMgr(SetPtMgrNum)%ZoneNodeNum))//  &
           '", not found in any controlled Zone')
        ErrorsFound=.TRUE.
      ELSE
        AirLoopNum = ZoneEquipConfig(ConZoneNum)%AirLoopNum
        IF (AirLoopNum == 0) THEN
          CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(SingZoneRhSetPtMgr(SetPtMgrNum)%Name)//  &
             '", Zone not on air loop:')
          CALL ShowContinueError('Controlled Zone not on air loop, Zone='// &
                                   TRIM(ZoneEquipConfig(ConZoneNum)%ZoneName))
          ErrorsFound=.TRUE.
          CYCLE
        ENDIF
        MixedAirNode = PrimaryAirSystem(AirLoopNum)%OASysOutletNodeNum
        InletBranchNum = PrimaryAirSystem(AirLoopNum)%InletBranchNum(1)
        LoopInNode = PrimaryAirSystem(AirLoopNum)%Branch(InletBranchNum)%NodeNumIn
        ! get the supply fan inlet and outlet nodes
        IF (MixedAirNode > 0) THEN
          DO BranchNum = 1,PrimaryAirSystem(AirLoopNum)%NumBranches
            DO CompNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%TotalComponents
              CompType = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf
              IF (MixedAirNode == PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn) THEN
                LookForFan = .TRUE.
              END IF
              IF (LookForFan) THEN
              !cpw22Aug2010 Add Fan:ComponentModel (new)
                IF (SameString(CompType , 'Fan:ConstantVolume') .OR. SameString(CompType , 'Fan:VariableVolume') .OR. &
                    SameString(CompType , 'Fan:OnOff') .OR. SameString(CompType , 'Fan:ComponentModel')) THEN
                  FanNodeIn = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn
                  FanNodeOut = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumOut
                  EXIT
                END IF
              END IF
            END DO
          END DO
        ELSE
          DO BranchNum = 1,PrimaryAirSystem(AirLoopNum)%NumBranches
            DO CompNum = 1, PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%TotalComponents
              CompType = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%TypeOf
              !cpw22Aug2010 Add Fan:ComponentModel (new)
              IF (SameString(CompType , 'Fan:ConstantVolume') .OR. SameString(CompType , 'Fan:VariableVolume') .OR. &
                  SameString(CompType , 'Fan:OnOff') .OR. SameString(CompType , 'Fan:ComponentModel')) THEN
                FanNodeIn = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumIn
                FanNodeOut = PrimaryAirSystem(AirLoopNum)%Branch(BranchNum)%Comp(CompNum)%NodeNumOut
              END IF
            END DO
          END DO
        END IF
        SingZoneRhSetPtMgr(SetPtMgrNum)%FanNodeIn = FanNodeIn
        SingZoneRhSetPtMgr(SetPtMgrNum)%FanNodeOut = FanNodeOut
        SingZoneRhSetPtMgr(SetPtMgrNum)%MixedAirNode = MixedAirNode
        SingZoneRhSetPtMgr(SetPtMgrNum)%AirLoopNum = AirLoopNum
        SingZoneRhSetPtMgr(SetPtMgrNum)%OAInNode = PrimaryAirSystem(AirLoopNum)%OAMixOAInNodeNum
        SingZoneRhSetPtMgr(SetPtMgrNum)%RetNode = PrimaryAirSystem(AirLoopNum)%OASysInletNodeNum
        SingZoneRhSetPtMgr(SetPtMgrNum)%OAInNode = PrimaryAirSystem(AirLoopNum)%OAMixOAInNodeNum
        SingZoneRhSetPtMgr(SetPtMgrNum)%LoopInNode = LoopInNode
      ENDIF
    END DO

    ! Warmest Setpoint Managers
    cSetPointManagerType = cValidSPMTypes(iSPMType_Warmest)
    DO SetPtMgrNum = 1,NumWarmestSetPtMgrs
      IF (NumPrimaryAirSys > 0) THEN
        AirLoopNum = FindItemInList(WarmestSetPtMgr(SetPtMgrNum)%AirLoopName, &
                                    AirToZoneNodeInfo%AirLoopName,NumPrimaryAirSys)
        IF (AirLoopNum == 0) THEN
          CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(WarmestSetPtMgr(SetPtMgrNum)%Name)//  &
             '", invalid Air Loop specified:')
          CALL ShowContinueError('Air Loop not found ="'//&
                               TRIM(WarmestSetPtMgr(SetPtMgrNum)%AirLoopName)//'".')
          ErrorsFound = .TRUE.
        ELSE
          WarmestSetPtMgr(SetPtMgrNum)%AirLoopNum = AirLoopNum
        END IF
        IF (AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled == 0) THEN
          CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(WarmestSetPtMgr(SetPtMgrNum)%Name)//  &
             '", no zones with cooling found:')
          CALL ShowContinueError('Air Loop provides no cooling, Air Loop="'// &
                               TRIM(WarmestSetPtMgr(SetPtMgrNum)%AirLoopName)//'".')
          ErrorsFound = .TRUE.
        END IF
      ELSE
        CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(WarmestSetPtMgr(SetPtMgrNum)%Name)//  &
             '", no AirLoopHVAC objects found:')
        CALL ShowContinueError('Setpoint Manager needs an AirLoopHVAC to operate.')
        ErrorsFound = .TRUE.
      END IF
    END DO

    ! Coldest Setpoint Managers
    cSetPointManagerType = cValidSPMTypes(iSPMType_Coldest)
    DO SetPtMgrNum = 1,NumColdestSetPtMgrs
      IF (NumPrimaryAirSys > 0) THEN
        AirLoopNum = FindItemInList(ColdestSetPtMgr(SetPtMgrNum)%AirLoopName, &
                                    AirToZoneNodeInfo%AirLoopName,NumPrimaryAirSys)
        IF (AirLoopNum == 0) THEN
          CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(ColdestSetPtMgr(SetPtMgrNum)%Name)//  &
             '", invalid Air Loop specified:')
          CALL ShowContinueError('Air Loop not found ="'//&
                               TRIM(ColdestSetPtMgr(SetPtMgrNum)%AirLoopName)//'".')
          ErrorsFound = .TRUE.
        ELSE
          ColdestSetPtMgr(SetPtMgrNum)%AirLoopNum = AirLoopNum
        END IF
        IF (AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated == 0) THEN
          CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(ColdestSetPtMgr(SetPtMgrNum)%Name)//  &
             '", no zones with heating found:')
          CALL ShowContinueError('Air Loop provides no heating, Air Loop="'// &
                               TRIM(ColdestSetPtMgr(SetPtMgrNum)%AirLoopName)//'".')
          ErrorsFound = .TRUE.
        END IF
      ELSE
        CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(ColdestSetPtMgr(SetPtMgrNum)%Name)//  &
             '", no AirLoopHVAC objects found:')
        CALL ShowContinueError('Setpoint Manager needs an AirLoopHVAC to operate.')
        ErrorsFound = .TRUE.
      END IF
    END DO

    ! Warmest Temp Flow Setpoint Managers
    cSetPointManagerType = cValidSPMTypes(iSPMType_WarmestTempFlow)
    DO SetPtMgrNum = 1,NumWarmestSetPtMgrsTempFlow
      IF (NumPrimaryAirSys > 0) THEN
        AirLoopNum = FindItemInList(WarmestSetPtMgrTempFlow(SetPtMgrNum)%AirLoopName, &
                                    AirToZoneNodeInfo%AirLoopName,NumPrimaryAirSys)
        IF (AirLoopNum == 0) THEN
          CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(WarmestSetPtMgrTempFlow(SetPtMgrNum)%Name)//  &
             '", invalid Air Loop specified:')
          CALL ShowContinueError('Air Loop not found ="'//&
                               TRIM(WarmestSetPtMgrTempFlow(SetPtMgrNum)%AirLoopName)//'".')
          ErrorsFound = .TRUE.
        ELSE
          WarmestSetPtMgrTempFlow(SetPtMgrNum)%AirLoopNum = AirLoopNum
          WarmestSetPtMgrTempFlow(SetPtMgrNum)%SimReady = .TRUE.
        END IF
        IF (AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled == 0) THEN
          CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(WarmestSetPtMgrTempFlow(SetPtMgrNum)%Name)//  &
             '", no zones with cooling found:')
          CALL ShowContinueError('Air Loop provides no cooling, Air Loop="'// &
                               TRIM(WarmestSetPtMgrTempFlow(SetPtMgrNum)%AirLoopName)//'".')
          ErrorsFound = .TRUE.
        END IF
      ELSE
        CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(WarmestSetPtMgrTempFlow(SetPtMgrNum)%Name)//  &
             '", no AirLoopHVAC objects found:')
        CALL ShowContinueError('Setpoint Manager needs an AirLoopHVAC to operate.')
        ErrorsFound = .TRUE.
      END IF
    END DO

    ! return air bypass flow set manager
    cSetPointManagerType = cValidSPMTypes(iSPMType_RAB)
    DO SetPtMgrNum=1,NumRABFlowSetPtMgrs
      IF (NumPrimaryAirSys > 0) THEN
        AirLoopNum = FindItemInList(RABFlowSetPtMgr(SetPtMgrNum)%AirLoopName, &
                                    AirToZoneNodeInfo%AirLoopName,NumPrimaryAirSys)
        AllSetPtMgr(RABFlowSetPtMgr(SetPtMgrNum)%AllSetPtMgrIndex)%AirLoopNum = AirLoopNum
        AllSetPtMgr(RABFlowSetPtMgr(SetPtMgrNum)%AllSetPtMgrIndex)%AirLoopName = RABFlowSetPtMgr(SetPtMgrNum)%AirLoopName
        IF (AirLoopNum == 0) THEN
          CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(RABFlowSetPtMgr(SetPtMgrNum)%Name)//  &
             '", invalid Air Loop specified:')
          CALL ShowContinueError('Air Loop not found ="'//&
                               TRIM(RABFlowSetPtMgr(SetPtMgrNum)%AirLoopName)//'".')
          ErrorsFound = .TRUE.
        ELSE
          RABFlowSetPtMgr(SetPtMgrNum)%AirLoopNum = AirLoopNum
          IF (PrimaryAirSystem(AirLoopNum)%RABExists) THEN
            RABFlowSetPtMgr(SetPtMgrNum)%RABMixInNode = PrimaryAirSystem(AirLoopNum)%RABMixInNode
            RABFlowSetPtMgr(SetPtMgrNum)%SupMixInNode = PrimaryAirSystem(AirLoopNum)%SupMixInNode
            RABFlowSetPtMgr(SetPtMgrNum)%MixOutNode = PrimaryAirSystem(AirLoopNum)%MixOutNode
            RABFlowSetPtMgr(SetPtMgrNum)%RABSplitOutNode = PrimaryAirSystem(AirLoopNum)%RABSplitOutNode
            RABFlowSetPtMgr(SetPtMgrNum)%SysOutNode = AirToZoneNodeInfo(AirLoopNum)%AirLoopSupplyNodeNum(1)
            RABFlowSetPtMgr(SetPtMgrNum)%CtrlNodes(1) = RABFlowSetPtMgr(SetPtMgrNum)%RABSplitOutNode
            AllSetPtMgr(RABFlowSetPtMgr(SetPtMgrNum)%AllSetPtMgrIndex)%CtrlNodes(1) = RABFlowSetPtMgr(SetPtMgrNum)%RABSplitOutNode
          ELSE
            CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(RABFlowSetPtMgr(SetPtMgrNum)%Name)//  &
               '", no RAB in air loop found:')
            CALL ShowContinueError('Air Loop="'//&
                               TRIM(RABFlowSetPtMgr(SetPtMgrNum)%AirLoopName)//'".')
            ErrorsFound = .TRUE.
          END IF
        END IF
      ELSE
        CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(RABFlowSetPtMgr(SetPtMgrNum)%Name)//  &
             '", no AirLoopHVAC objects found:')
        CALL ShowContinueError('Setpoint Manager needs an AirLoopHVAC to operate.')
        ErrorsFound = .TRUE.
      END IF
    END DO

    ! MultiZone Average Cooling Setpoint Managers
    cSetPointManagerType = cValidSPMTypes(iSPMType_MZCoolingAverage)
    DO SetPtMgrNum = 1, NumMZClgAverageSetPtMGrs
      IF (NumPrimaryAirSys > 0) THEN
        AirLoopNum = FindItemInList(MZAverageCoolingSetPtMgr(SetPtMgrNum)%AirLoopName, &
                                    AirToZoneNodeInfo%AirLoopName,NumPrimaryAirSys)
        IF (AirLoopNum == 0) THEN
          CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(MZAverageCoolingSetPtMgr(SetPtMgrNum)%Name)//  &
             '", invalid Air Loop specified:')
          CALL ShowContinueError('Air Loop not found ="'//&
                               TRIM(MZAverageCoolingSetPtMgr(SetPtMgrNum)%AirLoopName)//'".')
          ErrorsFound = .TRUE.
        ELSE
          MZAverageCoolingSetPtMgr(SetPtMgrNum)%AirLoopNum = AirLoopNum
        END IF
        IF (AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled == 0) THEN
          CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(MZAverageCoolingSetPtMgr(SetPtMgrNum)%Name)//  &
             '", no zones with cooling found:')
          CALL ShowContinueError('Air Loop provides no cooling, Air Loop="'// &
                               TRIM(MZAverageCoolingSetPtMgr(SetPtMgrNum)%AirLoopName)//'".')
          ErrorsFound = .TRUE.
        END IF
      ELSE
        CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(MZAverageCoolingSetPtMgr(SetPtMgrNum)%Name)//  &
             '", no AirLoopHVAC objects found:')
        CALL ShowContinueError('Setpoint Manager needs an AirLoopHVAC to operate.')
        ErrorsFound = .TRUE.
      END IF
    END DO

    ! MultiZone Average Heating Setpoint Managers
    cSetPointManagerType = cValidSPMTypes(iSPMType_MZHeatingAverage)
    DO SetPtMgrNum = 1, NumMZHtgAverageSetPtMGrs
      IF (NumPrimaryAirSys > 0) THEN
        AirLoopNum = FindItemInList(MZAverageHeatingSetPtMgr(SetPtMgrNum)%AirLoopName, &
                                    AirToZoneNodeInfo%AirLoopName,NumPrimaryAirSys)
        IF (AirLoopNum == 0) THEN
          CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(MZAverageHeatingSetPtMgr(SetPtMgrNum)%Name)//  &
             '", invalid Air Loop specified:')
          CALL ShowContinueError('Air Loop not found ="'//&
                               TRIM(MZAverageHeatingSetPtMgr(SetPtMgrNum)%AirLoopName)//'".')
          ErrorsFound = .TRUE.
        ELSE
          MZAverageHeatingSetPtMgr(SetPtMgrNum)%AirLoopNum = AirLoopNum
        END IF
        ! Commented out as we are using %NumZonesCooled instead of %NumZonesHeated for all systems for now
        !IF (AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated == 0) THEN
        !  CALL ShowSevereError(TRIM(cSetPointManagerType)//': Air Loop provides no heating ' // &
        !                       TRIM(MZAverageHeatingSetPtMgr(SetPtMgrNum)%Name))
        !  CALL ShowContinueError('Occurs in Setpoint Manager='//TRIM(MZAverageHeatingSetPtMgr(SetPtMgrNum)%Name))
        !  ErrorsFound = .TRUE.
        !END IF
      ELSE
        CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(MZAverageHeatingSetPtMgr(SetPtMgrNum)%Name)//  &
             '", no AirLoopHVAC objects found:')
        CALL ShowContinueError('Setpoint Manager needs an AirLoopHVAC to operate.')
        ErrorsFound = .TRUE.
      END IF
    END DO

    ! MultiZone Average Minimum Humidity Setpoint Managers
    cSetPointManagerType = cValidSPMTypes(iSPMType_MZMinHumAverage)
    DO SetPtMgrNum = 1, NumMZAverageMinHumSetPtMgrs
      IF (NumPrimaryAirSys > 0) THEN
        AirLoopNum = FindItemInList(MZAverageMinHumSetPtMgr(SetPtMgrNum)%AirLoopName, &
                                    AirToZoneNodeInfo%AirLoopName,NumPrimaryAirSys)
        IF (AirLoopNum == 0) THEN
          CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(MZAverageMinHumSetPtMgr(SetPtMgrNum)%Name)//  &
             '", invalid Air Loop specified:')
          CALL ShowContinueError('Air Loop not found ="'//&
                               TRIM(MZAverageMinHumSetPtMgr(SetPtMgrNum)%AirLoopName)//'".')
          ErrorsFound = .TRUE.
        ELSE
          MZAverageMinHumSetPtMgr(SetPtMgrNum)%AirLoopNum = AirLoopNum
          ! make sure humidity controlled zone
             HstatZoneFound=.false.
          DO HstatZoneNum = 1, NumHumidityControlZones
             DO ZonesCooledIndex=1,AirToZoneNodeInfo(MZAverageMinHumSetPtMgr(SetPtMgrNum)%AirLoopNum)%NumZonesCooled
                IF(HumidityControlZone(HstatZoneNum)%ActualZoneNum .NE. &
                   AirToZoneNodeInfo(MZAverageMinHumSetPtMgr(SetPtMgrNum)%AirLoopNum)%CoolCtrlZoneNums(ZonesCooledIndex))CYCLE
                   HstatZoneFound=.TRUE.
                   EXIT
             END DO
          END DO
          IF (.not. HstatZoneFound) THEN
            CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//  &
               trim(MZAverageMinHumSetPtMgr(SetPtMgrNum)%Name)//'", invalid humidistat specification')
            CALL ShowContinueError('could not locate Humidistat in any of the zones'//  &
               ' served by the Air loop='//TRIM(PrimaryAirSystem(AirLoopNum)%Name))
            ErrorsFound=.TRUE.
          ENDIF
        END IF
      ELSE
        CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(MZAverageMinHumSetPtMgr(SetPtMgrNum)%Name)//  &
             '", no AirLoopHVAC objects found:')
        CALL ShowContinueError('Setpoint Manager needs an AirLoopHVAC to operate.')
        ErrorsFound = .TRUE.
      END IF
    END DO

    ! MultiZone Average Maximum Humidity Setpoint Managers
    cSetPointManagerType = cValidSPMTypes(iSPMType_MZMaxHumAverage)
    DO SetPtMgrNum = 1, NumMZAverageMaxHumSetPtMgrs
      IF (NumPrimaryAirSys > 0) THEN
        AirLoopNum = FindItemInList(MZAverageMaxHumSetPtMgr(SetPtMgrNum)%AirLoopName, &
                                    AirToZoneNodeInfo%AirLoopName,NumPrimaryAirSys)
        IF (AirLoopNum == 0) THEN
          CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(MZAverageMaxHumSetPtMgr(SetPtMgrNum)%Name)//  &
             '", invalid Air Loop specified:')
          CALL ShowContinueError('Air Loop not found ="'//&
                               TRIM(MZAverageMaxHumSetPtMgr(SetPtMgrNum)%AirLoopName)//'".')
          ErrorsFound = .TRUE.
        ELSE
          MZAverageMaxHumSetPtMgr(SetPtMgrNum)%AirLoopNum = AirLoopNum
          ! make sure humidity controlled zone
          HstatZoneFound=.false.
          DO HstatZoneNum = 1, NumHumidityControlZones
             DO ZonesCooledIndex=1,AirToZoneNodeInfo(MZAverageMaxHumSetPtMgr(SetPtMgrNum)%AirLoopNum)%NumZonesCooled
                IF(HumidityControlZone(HstatZoneNum)%ActualZoneNum .NE. &
                   AirToZoneNodeInfo(MZAverageMaxHumSetPtMgr(SetPtMgrNum)%AirLoopNum)%CoolCtrlZoneNums(ZonesCooledIndex))CYCLE
                   HstatZoneFound=.TRUE.
                   EXIT
             END DO
          END DO
          IF (.not. HstatZoneFound) THEN
            CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//  &
               trim(MZAverageMaxHumSetPtMgr(SetPtMgrNum)%Name)//'", invalid humidistat specification')
            CALL ShowContinueError('could not locate Humidistat in any of the zones'//  &
               ' served by the Air loop='//TRIM(PrimaryAirSystem(AirLoopNum)%Name))
            ErrorsFound=.TRUE.
          ENDIF
        END IF
      ELSE
        CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(MZAverageMaxHumSetPtMgr(SetPtMgrNum)%Name)//  &
             '", no AirLoopHVAC objects found:')
        CALL ShowContinueError('Setpoint Manager needs an AirLoopHVAC to operate.')
        ErrorsFound = .TRUE.
      END IF
    END DO

    ! Multizone Minimum Humidity Ratio Setpoint Managers
    cSetPointManagerType = cValidSPMTypes(iSPMType_MZMinHum)
    DO SetPtMgrNum = 1, NumMZMinHumSetPtMgrs
      IF (NumPrimaryAirSys > 0) THEN
        AirLoopNum = FindItemInList(MZMinHumSetPtMgr(SetPtMgrNum)%AirLoopName, &
                                    AirToZoneNodeInfo%AirLoopName,NumPrimaryAirSys)
        IF (AirLoopNum == 0) THEN
          CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(MZMinHumSetPtMgr(SetPtMgrNum)%Name)//  &
             '", invalid Air Loop specified:')
          CALL ShowContinueError('Air Loop not found ="'//&
                               TRIM(MZMinHumSetPtMgr(SetPtMgrNum)%AirLoopName)//'".')
          ErrorsFound = .TRUE.
        ELSE
          MZMinHumSetPtMgr(SetPtMgrNum)%AirLoopNum = AirLoopNum
          ! make sure humidity controlled zone
             HstatZoneFound=.false.
          DO HstatZoneNum = 1, NumHumidityControlZones
             DO ZonesCooledIndex=1,AirToZoneNodeInfo(MZMinHumSetPtMgr(SetPtMgrNum)%AirLoopNum)%NumZonesCooled
                IF(HumidityControlZone(HstatZoneNum)%ActualZoneNum .NE. &
                   AirToZoneNodeInfo(MZMinHumSetPtMgr(SetPtMgrNum)%AirLoopNum)%CoolCtrlZoneNums(ZonesCooledIndex))CYCLE
                   HstatZoneFound=.TRUE.
                   EXIT
             END DO
          END DO
          IF (.not. HstatZoneFound) THEN
            CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//  &
               trim(MZMinHumSetPtMgr(SetPtMgrNum)%Name)//'", invalid humidistat specification')
            CALL ShowContinueError('could not locate Humidistat in any of the zones'//  &
               ' served by the Air loop='//TRIM(PrimaryAirSystem(AirLoopNum)%Name))
            ErrorsFound=.TRUE.
          ENDIF
        END IF
      ELSE
        CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(MZMinHumSetPtMgr(SetPtMgrNum)%Name)//  &
             '", no AirLoopHVAC objects found:')
        CALL ShowContinueError('Setpoint Manager needs an AirLoopHVAC to operate.')
        ErrorsFound = .TRUE.
      END IF
    END DO

    ! Multizone Maximum Humidity Ratio Setpoint Managers
    cSetPointManagerType = cValidSPMTypes(iSPMType_MZMaxHum)
    DO SetPtMgrNum = 1, NumMZMaxHumSetPtMgrs
      IF (NumPrimaryAirSys > 0) THEN
        AirLoopNum = FindItemInList(MZMaxHumSetPtMgr(SetPtMgrNum)%AirLoopName, &
                                    AirToZoneNodeInfo%AirLoopName,NumPrimaryAirSys)
        IF (AirLoopNum == 0) THEN
          CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(MZMaxHumSetPtMgr(SetPtMgrNum)%Name)//  &
             '", invalid Air Loop specified:')
          CALL ShowContinueError('Air Loop not found ="'//&
                               TRIM(MZMaxHumSetPtMgr(SetPtMgrNum)%AirLoopName)//'".')
          ErrorsFound = .TRUE.
        ELSE
          MZMaxHumSetPtMgr(SetPtMgrNum)%AirLoopNum = AirLoopNum
          ! make sure humidity controlled zone
             HstatZoneFound=.false.
          DO HstatZoneNum = 1, NumHumidityControlZones
             DO ZonesCooledIndex=1,AirToZoneNodeInfo(MZMaxHumSetPtMgr(SetPtMgrNum)%AirLoopNum)%NumZonesCooled
                IF(HumidityControlZone(HstatZoneNum)%ActualZoneNum .NE. &
                   AirToZoneNodeInfo(MZMaxHumSetPtMgr(SetPtMgrNum)%AirLoopNum)%CoolCtrlZoneNums(ZonesCooledIndex))CYCLE
                   HstatZoneFound=.TRUE.
                   EXIT
             END DO
          END DO
          IF (.not. HstatZoneFound) THEN
            CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//  &
               trim(MZMaxHumSetPtMgr(SetPtMgrNum)%Name)//'", invalid humidistat specification')
            CALL ShowContinueError('could not locate Humidistat in any of the zones'//  &
               ' served by the Air loop='//TRIM(PrimaryAirSystem(AirLoopNum)%Name))
               ErrorsFound=.TRUE.
          ENDIF
        END IF
      ELSE
        CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//TRIM(MZMaxHumSetPtMgr(SetPtMgrNum)%Name)//  &
             '", no AirLoopHVAC objects found:')
        CALL ShowContinueError('Setpoint Manager needs an AirLoopHVAC to operate.')
        ErrorsFound = .TRUE.
      END IF
    END DO

    ! condenser entering water temperature reset setpoint manager
    NumCT = 0
    cSetPointManagerType = cValidSPMTypes(iSPMType_CondEntReset)
    DO SetPtMgrNum=1,NumCondEntSetPtMgrs
       ! Scan loops and find the loop index that includes the condenser cooling tower node used as setpoint
       DO LoopNum = 1, NumCondLoops + NumPlantLoops ! Begin demand side loops ... When condenser is added becomes NumLoops
          DO CtrlNodeIndex=1,CondEntSetPtMgr(SetPtMgrNum)%NumCtrlNodes
             IF (PlantLoop(LoopNum)%TempSetPointNodeNum == CondEntSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex)) THEN
               DO BranchNum = 1, PlantLoop(LoopNum)%LoopSide(SupplySide)%TotalBranches
                 DO CompNum = 1, PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%TotalComponents
                   ! Check if cooling tower is single speed and generate and error
                   TypeOf_Num = PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num
                   IF (TypeOf_Num == TypeOf_CoolingTower_SingleSpd) THEN
                      CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//  &
                        trim(CondEntSetPtMgr(SetPtMgrNum)%Name)//'", invalid tower found')
                      CALL ShowContinueError('Found SingleSpeed Cooling Tower, Cooling Tower='// &
                        TRIM(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%Name))
                      CALL ShowContinueError('SingleSpeed cooling towers cannot be used with this setpoint manager on each loop')
                      ErrorsFound=.true.
                   END IF
                   ! Check if there are more than 1 cooling tower on the plant and generate error
                   IF (TypeOf_Num == TypeOf_CoolingTower_TwoSpd  .or. &
                       TypeOf_Num == TypeOf_CoolingTower_VarSpdMerkel .OR.  &
                       TypeOf_Num == TypeOf_CoolingTower_VarSpd) THEN
                        NumCT = NumCT + 1
                      IF (NumCT .GT. 1 )THEN
                        CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//  &
                          TRIM(CondEntSetPtMgr(SetPtMgrNum)%Name)//'", too many towers found')
                        CALL ShowContinueError('only one cooling tower can be used with this setpoint manager on each loop')
                        CALL ShowContinueError('Found more than one cooling tower, Cooling Tower='//&
                          TRIM(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%Name))
                        ErrorsFound=.true.
                      END IF
                   END IF
                 END DO
               END DO
               NumCT = 0
               ! Scan all attached chillers in the condenser loop index found to find the chiller index
               DO BranchNum = 1, PlantLoop(LoopNum)%LoopSide(DemandSide)%TotalBranches
                 DO CompNum = 1, PlantLoop(LoopNum)%LoopSide(DemandSide)%Branch(BranchNum)%TotalComponents
                   TypeOf_Num = PlantLoop(LoopNum)%LoopSide(DemandSide)%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num
                   IF (TypeOf_Num == TypeOf_Chiller_Absorption .or. &
                       TypeOf_Num == TypeOf_Chiller_Indirect_Absorption .or. &
                       TypeOf_Num == TypeOf_Chiller_CombTurbine .or. &
                       TypeOf_Num == TypeOf_Chiller_ConstCOP  .or. &
                       TypeOf_Num == TypeOf_Chiller_Electric   .or. &
                       TypeOf_Num == TypeOf_Chiller_ElectricEIR .or. &
                       TypeOf_Num == TypeOf_Chiller_DFAbsorption .or. &
                       TypeOf_Num == TypeOf_Chiller_ElectricReformEIR .or. &
                       TypeOf_Num == TypeOf_Chiller_EngineDriven) THEN

                      ! Scan the supply side to find the chiller index and branch index on plantloop
                      TypeNum = PlantLoop(LoopNum)%LoopSide(DemandSide)%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num
                      DO LoopNum2 = 1, NumCondLoops + NumPlantLoops
                        DO BranchNumPlantSide = 1, PlantLoop(LoopNum2)%LoopSide(SupplySide)%TotalBranches
                          DO CompNumPlantSide = 1,   &
                             PlantLoop(LoopNum2)%LoopSide(SupplySide)%Branch(BranchNumPlantSide)%TotalComponents
                            IF(PlantLoop(LoopNum2)%LoopSide(SupplySide)%Branch(BranchNumPlantSide)%  &
                               Comp(CompNumPlantSide)%TypeOf_Num == TypeNum) THEN
                              CondEntSetPtMgr(SetPtMgrNum)%LoopIndexPlantSide = LoopNum2
                              CondEntSetPtMgr(SetPtMgrNum)%ChillerIndexPlantSide = CompNumPlantSide
                              CondEntSetPtMgr(SetPtMgrNum)%BranchIndexPlantSide = BranchNumPlantSide
                            END IF
                          END DO
                        END DO
                      END DO
                      CondEntSetPtMgr(SetPtMgrNum)%TypeNum = TypeNum
                      CondEntSetPtMgr(SetPtMgrNum)%LoopIndexDemandSide = LoopNum
                      CondEntSetPtMgr(SetPtMgrNum)%ChillerIndexDemandSide = CompNum
                      CondEntSetPtMgr(SetPtMgrNum)%BranchIndexDemandSide = BranchNum
                   END IF
                 END DO
               END DO
             END IF
          END DO
       END DO
    END DO

    ! Ideal condenser entering water temperature reset setpoint manager
    cSetPointManagerType = cValidSPMTypes(iSPMType_IdealCondEntReset)
    NumCT = 0
    NumChiller = 0
    DO SetPtMgrNum=1,NumIdealCondEntSetPtMgrs
       ! Scan loops and find the loop index that includes the condenser cooling tower node used as setpoint
       DO LoopNum = 1, NumCondLoops + NumPlantLoops ! Begin demand side loops ... When condenser is added becomes NumLoops
          DO CtrlNodeIndex=1,IdealCondEntSetPtMgr(SetPtMgrNum)%NumCtrlNodes
             IF (PlantLoop(LoopNum)%TempSetPointNodeNum == IdealCondEntSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex)) THEN
               DO BranchNum = 1, PlantLoop(LoopNum)%LoopSide(SupplySide)%TotalBranches
                 DO CompNum = 1, PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%TotalComponents
                   ! Check if cooling tower is single speed and generate and error
                   TypeOf_Num = PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num
                   IF (TypeOf_Num == TypeOf_CoolingTower_SingleSpd) THEN
                      CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//  &
                        TRIM(IdealCondEntSetPtMgr(SetPtMgrNum)%Name)//'", invalid cooling tower found')
                      CALL ShowContinueError('Found Single Speed Cooling Tower, Cooling Tower='// &
                        TRIM(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%Name))
                      CALL ShowContinueError('SingleSpeed cooling towers cannot be used with this setpoint manager on each loop')
                      ErrorsFound=.true.
                   END IF
                   ! Check if there are more than 1 cooling tower on the plant and generate error
                   IF (TypeOf_Num == TypeOf_CoolingTower_TwoSpd .or. &
                       TypeOf_Num == TypeOf_CoolingTower_VarSpdMerkel .or. &
                       TypeOf_Num == TypeOf_CoolingTower_VarSpd) THEN
                     NumCT = NumCT + 1
                     IF (NumCT .GT. 1 )THEN
                       CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//  &
                         TRIM(IdealCondEntSetPtMgr(SetPtMgrNum)%Name)//'", too many cooling towers found')
                       CALL ShowContinueError('only one cooling tower can be used with this setpoint manager on each loop')
                       CALL ShowContinueError('Found more than one cooling tower, Cooling Tower='//&
                         TRIM(PlantLoop(LoopNum)%LoopSide(SupplySide)%Branch(BranchNum)%Comp(CompNum)%Name))
                       ErrorsFound=.true.
                     END IF
                   END IF
                   ! Scan the pump on the condenser water loop
                   IF (TypeOf_Num == TypeOf_PumpVariableSpeed .or. &
                       TypeOf_Num == TypeOf_PumpConstantSpeed) THEN
                     IdealCondEntSetPtMgr(SetPtMgrNum)%CondPumpNum = CompNum
                     IdealCondEntSetPtMgr(SetPtMgrNum)%CondPumpBranchNum = BranchNum
                   END IF
                 END DO
               END DO
               NumCT = 0
               ! Scan all attached chillers in the condenser loop index found to find the chiller index
               DO BranchNum = 1, PlantLoop(LoopNum)%LoopSide(DemandSide)%TotalBranches
                 DO CompNum = 1, PlantLoop(LoopNum)%LoopSide(DemandSide)%Branch(BranchNum)%TotalComponents
                   TypeOf_Num = PlantLoop(LoopNum)%LoopSide(DemandSide)%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num
                   IF (TypeOf_Num ==  TypeOf_Chiller_Absorption .or. &
                       TypeOf_Num ==  TypeOf_Chiller_Indirect_Absorption .or. &
                       TypeOf_Num ==  TypeOf_Chiller_CombTurbine .or. &
                       TypeOf_Num ==  TypeOf_Chiller_ConstCOP  .or. &
                       TypeOf_Num ==  TypeOf_Chiller_Electric   .or. &
                       TypeOf_Num ==  TypeOf_Chiller_ElectricEIR .or. &
                       TypeOf_Num ==  TypeOf_Chiller_DFAbsorption .or. &
                       TypeOf_Num ==  TypeOf_Chiller_ElectricReformEIR .or. &
                       TypeOf_Num ==  TypeOf_Chiller_EngineDriven) THEN

                      ! Scan the supply side to find the chiller index and branch index on plantloop
                      TypeNum = PlantLoop(LoopNum)%LoopSide(DemandSide)%Branch(BranchNum)%Comp(CompNum)%TypeOf_Num
                      DO LoopNum2 = 1, NumCondLoops + NumPlantLoops
                        DO BranchNumPlantSide = 1, PlantLoop(LoopNum2)%LoopSide(SupplySide)%TotalBranches
                          DO CompNumPlantSide = 1,  &
                             PlantLoop(LoopNum2)%LoopSide(SupplySide)%Branch(BranchNumPlantSide)%TotalComponents
                            TypeOf_Num = PlantLoop(LoopNum2)%LoopSide(SupplySide)%Branch(BranchNumPlantSide)%  &
                               Comp(CompNumPlantSide)%TypeOf_Num
                            IF(TypeOf_Num == TypeNum) THEN
                              NumChiller = NumChiller + 1
                              IdealCondEntSetPtMgr(SetPtMgrNum)%LoopIndexPlantSide = LoopNum2
                              IdealCondEntSetPtMgr(SetPtMgrNum)%ChillerIndexPlantSide = CompNumPlantSide
                              IdealCondEntSetPtMgr(SetPtMgrNum)%BranchIndexPlantSide = BranchNumPlantSide
                              ! Scan the pump on the chilled water loop
                              DO BranchNum2 = 1, PlantLoop(LoopNum2)%LoopSide(SupplySide)%TotalBranches
                                DO CompNum2 = 1, PlantLoop(LoopNum2)%LoopSide(SupplySide)%Branch(BranchNum2)%TotalComponents
                                  TypeOf_Num =PlantLoop(LoopNum2)%LoopSide(SupplySide)%Branch(BranchNum2)%Comp(CompNum2)%TypeOf_Num
                                  IF (TypeOf_Num == TypeOf_PumpVariableSpeed .or. &
                                      TypeOf_Num == TypeOf_PumpConstantSpeed) THEN
                                    IdealCondEntSetPtMgr(SetPtMgrNum)%ChilledPumpNum = CompNum2
                                    IdealCondEntSetPtMgr(SetPtMgrNum)%ChilledPumpBranchNum = BranchNum2
                                  END IF
                                END DO
                              END DO
                            END IF
                          END DO
                        END DO
                      END DO
                      IF (NumChiller .GT. 1 )THEN
                        CALL ShowSevereError(TRIM(cSetPointManagerType)//'="'//  &
                          TRIM(IdealCondEntSetPtMgr(SetPtMgrNum)%Name)//'", too many chillers found')
                        CALL ShowContinueError('only one chiller can be used with this setpoint manager on each loop')
                        CALL ShowContinueError('Found more than one chiller, chiller ='// &
                          TRIM(PlantLoop(LoopNum)%LoopSide(DemandSide)%Branch(BranchNum)%Comp(CompNum)%Name))
                        ErrorsFound=.true.
                      END IF
                      IdealCondEntSetPtMgr(SetPtMgrNum)%TypeNum = TypeNum
                      IdealCondEntSetPtMgr(SetPtMgrNum)%CondLoopNum = LoopNum
                      IdealCondEntSetPtMgr(SetPtMgrNum)%TowerNum = CompNum
                      IdealCondEntSetPtMgr(SetPtMgrNum)%CondBranchNum = BranchNum
                   END IF
                 END DO
               END DO
               NumChiller = 0
             END IF
          END DO
       END DO
    END DO

    CALL VerifySetPointManagers(ErrorsFound)

  END IF

  MyOneTimeFlag = .FALSE.

  IF (ErrorsFound) THEN
    CALL ShowFatalError('InitSetPointManagers: Errors found in getting SetPointManager input.')
  ENDIF

END IF

IF ( (BeginEnvrnFlag .and. MyEnvrnFlag) .or. MyOneTimeFlag2) THEN

  ManagerOn = .FALSE.

  DO SetPtMgrNum=1,NumSchSetPtMgrs
    DO CtrlNodeIndex=1,SchSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = SchSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
       ! Initialize scheduled setpoints
       SELECT CASE (SchSetPtMgr(SetPtMgrNum)%CtrlTypeMode)
        CASE(iCtrlVarType_Temp)
              Node(NodeNum)%TempSetPoint = &
                       GetCurrentScheduleValue(SchSetPtMgr(SetPtMgrNum)%SchedPtr)
        CASE(iCtrlVarType_MaxTemp)
              Node(NodeNum)%TempSetPointHi = &
                       GetCurrentScheduleValue(SchSetPtMgr(SetPtMgrNum)%SchedPtr)
        CASE(iCtrlVarType_MinTemp)
              Node(NodeNum)%TempSetPointLo = &
                       GetCurrentScheduleValue(SchSetPtMgr(SetPtMgrNum)%SchedPtr)
        CASE(iCtrlVarType_HumRat)
              Node(NodeNum)%HumRatSetPoint = &
                       GetCurrentScheduleValue(SchSetPtMgr(SetPtMgrNum)%SchedPtr)
        CASE(iCtrlVarType_MaxHumRat)
              Node(NodeNum)%HumRatMax = &
                       GetCurrentScheduleValue(SchSetPtMgr(SetPtMgrNum)%SchedPtr)
        CASE(iCtrlVarType_MinHumRat)
              Node(NodeNum)%HumRatMin = &
                       GetCurrentScheduleValue(SchSetPtMgr(SetPtMgrNum)%SchedPtr)
        CASE(iCtrlVarType_MassFlow)
              Node(NodeNum)%MassFlowRateSetPoint = &
                       GetCurrentScheduleValue(SchSetPtMgr(SetPtMgrNum)%SchedPtr)
        CASE(iCtrlVarType_MaxMassFlow)
              Node(NodeNum)%MassFlowRateMax = &
                       GetCurrentScheduleValue(SchSetPtMgr(SetPtMgrNum)%SchedPtr)
        CASE(iCtrlVarType_MinMassFlow)
              Node(NodeNum)%MassFlowRateMin = &
                       GetCurrentScheduleValue(SchSetPtMgr(SetPtMgrNum)%SchedPtr)
       END SELECT
    END DO
  END DO

  DO SetPtMgrNum=1,NumDualSchSetPtMgrs
    DO CtrlNodeIndex=1,DualSchSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = DualSchSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
      IF (DualSchSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
        Node(NodeNum)%TempSetPointHi =  GetCurrentScheduleValue(DualSchSetPtMgr(SetPtMgrNum)%SchedPtrHi)
        Node(NodeNum)%TempSetPointLo =  GetCurrentScheduleValue(DualSchSetPtMgr(SetPtMgrNum)%SchedPtrLo)
        Node(NodeNum)%TempSetPoint =  (Node(NodeNum)%TempSetPointHi + Node(NodeNum)%TempSetPointLo)/2.0d0
      END IF
    END DO
  END DO

  DO SetPtMgrNum=1,NumOutAirSetPtMgrs
    DO CtrlNodeIndex=1,OutAirSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = OutAirSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
      IF (OutAirSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
        ! Call the CALC routine, with an optional argument to only set
        ! the initialization NODE(:)% setpoint, and not the OutAirSetPtMgr(:)%SetPt
        CALL CalcOutsideAirSetPoint(SetPtMgrNum, NodeNum, .TRUE.)
      END IF
    END DO
  END DO

  DO SetPtMgrNum=1,NumSZMinHumSetPtMgrs ! Minimum humidity setpoint managers
    DO ZoneIndex=1,SZMinHumSetPtMgr(SetPtMgrNum)%NumZones
      ZoneNode = SZMinHumSetPtMgr(SetPtMgrNum)%ZoneNodes(ZoneIndex)
      Node(ZoneNode)%MassFlowRate = 0.0d0
    END DO
    DO CtrlNodeIndex=1,SZMinHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = SZMinHumSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
      Node(NodeNum)%HumRatMin = 0.007d0 ! Set the setpoint
    END DO
  END DO

  DO SetPtMgrNum=1,NumSZMaxHumSetPtMgrs ! Maximum humidity setpoint managers
    DO ZoneIndex=1,SZMaxHumSetPtMgr(SetPtMgrNum)%NumZones
      ZoneNode = SZMaxHumSetPtMgr(SetPtMgrNum)%ZoneNodes(ZoneIndex)
      Node(ZoneNode)%MassFlowRate = 0.0d0
    END DO
    DO CtrlNodeIndex=1,SZMaxHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = SZMaxHumSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
      Node(NodeNum)%HumRatMax = 0.011d0 ! Set the setpoint
    END DO
  END DO

  DO SetPtMgrNum=1,NumSZRhSetPtMgrs ! single zone reheat setpoint managers
    ZoneInletNode = SingZoneRhSetPtMgr(SetPtMgrNum)%ZoneInletNodeNum
    ZoneNode = SingZoneRhSetPtMgr(SetPtMgrNum)%ZoneNodeNum
    Node(ZoneInletNode)%MassFlowRate = 0.0d0
    Node(ZoneNode)%MassFlowRate = 0.0d0
    DO CtrlNodeIndex=1,SingZoneRhSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = SingZoneRhSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
     IF (SingZoneRhSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
        Node(NodeNum)%TempSetPoint = 20.d0 ! Set the setpoint
      END IF
    END DO
  END DO

  DO SetPtMgrNum=1,NumSZHtSetPtMgrs ! single zone heating setpoint managers
    ZoneInletNode = SingZoneHtSetPtMgr(SetPtMgrNum)%ZoneInletNodeNum
    ZoneNode = SingZoneHtSetPtMgr(SetPtMgrNum)%ZoneNodeNum
    Node(ZoneInletNode)%MassFlowRate = 0.0d0
    Node(ZoneNode)%MassFlowRate = 0.0d0
    DO CtrlNodeIndex=1,SingZoneHtSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = SingZoneHtSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
     IF (SingZoneHtSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
        Node(NodeNum)%TempSetPoint = 20.d0 ! Set the setpoint
      END IF
    END DO
  END DO

  DO SetPtMgrNum=1,NumSZClSetPtMgrs ! single zone cooling setpoint managers
    ZoneInletNode = SingZoneClSetPtMgr(SetPtMgrNum)%ZoneInletNodeNum
    ZoneNode = SingZoneClSetPtMgr(SetPtMgrNum)%ZoneNodeNum
    Node(ZoneInletNode)%MassFlowRate = 0.0d0
    Node(ZoneNode)%MassFlowRate = 0.0d0
    DO CtrlNodeIndex=1,SingZoneClSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = SingZoneClSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
     IF (SingZoneClSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
        Node(NodeNum)%TempSetPoint = 20.d0 ! Set the setpoint
      END IF
    END DO
  END DO

  DO SetPtMgrNum=1,NumMixedAirSetPtMgrs ! mixed air setpoint managers

    Node(MixedAirSetPtMgr(SetPtMgrNum)%RefNode)%MassFlowRate = 0.0d0
    Node(MixedAirSetPtMgr(SetPtMgrNum)%FanInNode)%MassFlowRate = 0.0d0
    Node(MixedAirSetPtMgr(SetPtMgrNum)%FanOutNode)%MassFlowRate = 0.0d0
    Node(MixedAirSetPtMgr(SetPtMgrNum)%RefNode)%Temp = 20.d0
    Node(MixedAirSetPtMgr(SetPtMgrNum)%FanInNode)%Temp = 20.d0
    Node(MixedAirSetPtMgr(SetPtMgrNum)%FanOutNode)%Temp = 20.d0
    Node(MixedAirSetPtMgr(SetPtMgrNum)%RefNode)%HumRat = OutHumRat
    Node(MixedAirSetPtMgr(SetPtMgrNum)%FanInNode)%HumRat = OutHumRat
    Node(MixedAirSetPtMgr(SetPtMgrNum)%FanOutNode)%HumRat = OutHumRat
    Node(MixedAirSetPtMgr(SetPtMgrNum)%RefNode)%Quality = 1.0d0
    Node(MixedAirSetPtMgr(SetPtMgrNum)%FanInNode)%Quality = 1.0d0
    Node(MixedAirSetPtMgr(SetPtMgrNum)%FanOutNode)%Quality = 1.0d0
    Node(MixedAirSetPtMgr(SetPtMgrNum)%RefNode)%Press = OutBaroPress
    Node(MixedAirSetPtMgr(SetPtMgrNum)%FanInNode)%Press = OutBaroPress
    Node(MixedAirSetPtMgr(SetPtMgrNum)%FanOutNode)%Press = OutBaroPress
    Node(MixedAirSetPtMgr(SetPtMgrNum)%RefNode)%Enthalpy = PsyHFnTdbW(constant_twenty,OutHumRat)
    Node(MixedAirSetPtMgr(SetPtMgrNum)%FanInNode)%Enthalpy = PsyHFnTdbW(constant_twenty,OutHumRat)
    Node(MixedAirSetPtMgr(SetPtMgrNum)%FanOutNode)%Enthalpy = PsyHFnTdbW(constant_twenty,OutHumRat)
    DO CtrlNodeIndex=1,MixedAirSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = MixedAirSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
      IF (MixedAirSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
        Node(NodeNum)%TempSetPoint = 20.d0 ! Set the setpoint
      END IF
    END DO
  END DO

  DO SetPtMgrNum=1,NumOAPretreatSetPtMgrs ! Outside Air Pretreat setpoint managers

    Node(OAPretreatSetPtMgr(SetPtMgrNum)%RefNode)%MassFlowRate = 0.0d0
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%MixedOutNode)%MassFlowRate = 0.0d0
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%OAInNode)%MassFlowRate = 0.0d0
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%ReturnInNode)%MassFlowRate = 0.0d0
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%RefNode)%Temp = 20.d0
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%MixedOutNode)%Temp = 20.d0
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%OAInNode)%Temp = 20.d0
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%ReturnInNode)%Temp = 20.d0
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%RefNode)%HumRat = OutHumRat
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%MixedOutNode)%HumRat = OutHumRat
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%OAInNode)%HumRat = OutHumRat
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%ReturnInNode)%HumRat = OutHumRat
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%RefNode)%Quality = 1.0d0
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%MixedOutNode)%Quality = 1.0d0
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%OAInNode)%Quality = 1.0d0
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%ReturnInNode)%Quality = 1.0d0
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%RefNode)%Press = OutBaroPress
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%MixedOutNode)%Press = OutBaroPress
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%OAInNode)%Press = OutBaroPress
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%ReturnInNode)%Press = OutBaroPress
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%RefNode)%Enthalpy = PsyHFnTdbW(constant_twenty,OutHumRat)
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%MixedOutNode)%Enthalpy = PsyHFnTdbW(constant_twenty,OutHumRat)
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%OAInNode)%Enthalpy = PsyHFnTdbW(constant_twenty,OutHumRat)
    Node(OAPretreatSetPtMgr(SetPtMgrNum)%ReturnInNode)%Enthalpy = PsyHFnTdbW(constant_twenty,OutHumRat)
    DO CtrlNodeIndex=1,OAPretreatSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = OAPretreatSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
      IF (OAPretreatSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
        Node(NodeNum)%TempSetPoint = 20.d0 ! Set the setpoint
      END IF
      IF (OAPretreatSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MaxHumRat) THEN
        Node(NodeNum)%HumRatMax = OutHumRat ! Set the setpoint
      END IF
      IF (OAPretreatSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MinHumRat) THEN
        Node(NodeNum)%HumRatMin = OutHumRat ! Set the setpoint
      END IF
      IF (OAPretreatSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_HumRat) THEN
        Node(NodeNum)%HumRatSetPoint = OutHumRat ! Set the setpoint
      END IF
    END DO
  END DO

  DO SetPtMgrNum=1,NumWarmestSetPtMgrs
    DO CtrlNodeIndex=1,WarmestSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = WarmestSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
      IF (WarmestSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
        Node(NodeNum)%TempSetPoint = 20.d0 ! Set the setpoint
      END IF
    END DO
  END DO

  DO SetPtMgrNum=1,NumColdestSetPtMgrs
    DO CtrlNodeIndex=1,ColdestSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = ColdestSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
      IF (ColdestSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
        Node(NodeNum)%TempSetPoint = 20.d0 ! Set the setpoint
      END IF
    END DO
  END DO

  DO SetPtMgrNum=1,NumWarmestSetPtMgrsTempFlow
    DO CtrlNodeIndex=1,WarmestSetPtMgrTempFlow(SetPtMgrNum)%NumCtrlNodes
      NodeNum = WarmestSetPtMgrTempFlow(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
      IF (WarmestSetPtMgrTempFlow(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
        Node(NodeNum)%TempSetPoint = 20.d0 ! Set the temperature setpoint
        IF (WarmestSetPtMgrTempFlow(SetPtMgrNum)%AirLoopNum /= 0) THEN
          AirLoopFlow(WarmestSetPtMgrTempFlow(SetPtMgrNum)%AirLoopNum)%ReqSupplyFrac = 1.d0 ! PH 10/09/04 Set the flow
          AirLoopControlInfo(WarmestSetPtMgrTempFlow(SetPtMgrNum)%AirLoopNum)%LoopFlowRateSet = .TRUE.  ! PH 10/09/04 Set the flag
        ENDIF
      END IF
    END DO
  END DO

  IF (ZoneEquipInputsFilled .and. AirLoopInputsFilled) THEN
    DO SetPtMgrNum=1,NumRABFlowSetPtMgrs
      NodeNum = RABFlowSetPtMgr(SetPtMgrNum)%RABSplitOutNode
      IF (RABFlowSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MassFlow) THEN
        Node(NodeNum)%MassFlowRateSetPoint = 0.0d0
      END IF
    END DO
  END IF

  DO SetPtMgrNum=1,NumMZClgAverageSetPtMGrs
    DO CtrlNodeIndex=1,MZAverageCoolingSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = MZAverageCoolingSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
      IF (MZAverageCoolingSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
        Node(NodeNum)%TempSetPoint = 20.d0 ! Set the setpoint
      END IF
    END DO
  END DO

  DO SetPtMgrNum=1,NumMZHtgAverageSetPtMGrs
    DO CtrlNodeIndex=1,MZAverageHeatingSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = MZAverageHeatingSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
      IF (MZAverageHeatingSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
        Node(NodeNum)%TempSetPoint = 20.0d0 ! Set the setpoint
      END IF
    END DO
  END DO

  DO SetPtMgrNum=1,NumMZAverageMinHumSetPtMgrs
    DO CtrlNodeIndex=1,MZAverageMinHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = MZAverageMinHumSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
      Node(NodeNum)%HumRatMin = 0.007d0 ! Set the setpoint
    END DO
  END DO

  DO SetPtMgrNum=1,NumMZAverageMaxHumSetPtMgrs
    DO CtrlNodeIndex=1,MZAverageMaxHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = MZAverageMaxHumSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
      Node(NodeNum)%HumRatMax = 0.011d0 ! Set the setpoint
    END DO
  END DO

  DO SetPtMgrNum=1,NumMZMinHumSetPtMgrs
    DO CtrlNodeIndex=1,MZMinHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = MZMinHumSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
      Node(NodeNum)%HumRatMin = 0.007d0 ! Set the setpoint
    END DO
  END DO

  DO SetPtMgrNum=1,NumMZMaxHumSetPtMgrs
    DO CtrlNodeIndex=1,MZMaxHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = MZMaxHumSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
      Node(NodeNum)%HumRatMax = 0.011d0 ! Set the setpoint
    END DO
  END DO

  DO SetPtMgrNum=1,NumFollowOATempSetPtMgrs
    DO CtrlNodeIndex=1,FollowOATempSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = FollowOATempSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
      IF (FollowOATempSetPtMgr(SetPtMgrNum)%RefTypeMode == iRefTempType_WetBulb) THEN
        IF (FollowOATempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
          Node(NodeNum)%TempSetPoint = OutWetBulbTemp ! Set the setpoint
        ELSEIF (FollowOATempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MaxTemp) THEN
          Node(NodeNum)%TempSetPointHi = OutWetBulbTemp ! Set the setpoint
        ELSEIF (FollowOATempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MinTemp) THEN
          Node(NodeNum)%TempSetPointLo = OutWetBulbTemp ! Set the setpoint
        END IF
      ELSEIF (FollowOATempSetPtMgr(SetPtMgrNum)%RefTypeMode == iRefTempType_DryBulb) THEN
        IF (FollowOATempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
          Node(NodeNum)%TempSetPoint = OutDryBulbTemp ! Set the setpoint
        ELSEIF (FollowOATempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MaxTemp) THEN
          Node(NodeNum)%TempSetPointHi = OutDryBulbTemp ! Set the setpoint
        ELSEIF (FollowOATempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MinTemp) THEN
          Node(NodeNum)%TempSetPointLo = OutDryBulbTemp ! Set the setpoint
        END IF
      END IF
    END DO
  END DO

  DO SetPtMgrNum=1,NumFollowSysNodeTempSetPtMgrs
    DO CtrlNodeIndex=1,FollowSysNodeTempSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
      IF (CheckOutAirNodeNumber(FollowSysNodeTempSetPtMgr(SetPtMgrNum)%RefNodeNum)) THEN
        IF (FollowSysNodeTempSetPtMgr(SetPtMgrNum)%RefTypeMode == iRefTempType_WetBulb) THEN
          Node(FollowSysNodeTempSetPtMgr(SetPtMgrNum)%RefNodeNum)%SPMNodeWetbulbRepReq = .TRUE.
          IF (FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
            Node(NodeNum)%TempSetPoint = OutWetBulbTemp ! Set the setpoint
          ELSEIF (FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MaxTemp) THEN
            Node(NodeNum)%TempSetPointHi = OutWetBulbTemp ! Set the setpoint
          ELSEIF (FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MinTemp) THEN
            Node(NodeNum)%TempSetPointLo = OutWetBulbTemp ! Set the setpoint
          END IF
        ELSEIF (FollowSysNodeTempSetPtMgr(SetPtMgrNum)%RefTypeMode == iRefTempType_DryBulb) THEN
          IF (FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
            Node(NodeNum)%TempSetPoint = OutDryBulbTemp ! Set the setpoint
          ELSEIF (FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MaxTemp) THEN
            Node(NodeNum)%TempSetPointHi = OutDryBulbTemp ! Set the setpoint
          ELSEIF (FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MinTemp) THEN
            Node(NodeNum)%TempSetPointLo = OutDryBulbTemp ! Set the setpoint
          END IF
        ENDIF
      ELSE
        ! If reference node is a water node, then set RefTypeMode to NodeDryBulb
        IF (Node(FollowSysNodeTempSetPtMgr(SetPtMgrNum)%RefNodeNum)%FluidType .EQ. NodeType_Water) THEN
          FollowSysNodeTempSetPtMgr(SetPtMgrNum)%RefTypeMode = iRefTempType_DryBulb
        ELSEIF (Node(FollowSysNodeTempSetPtMgr(SetPtMgrNum)%RefNodeNum)%FluidType .EQ. NodeType_Air) THEN
          IF (FollowSysNodeTempSetPtMgr(SetPtMgrNum)%RefTypeMode == iRefTempType_WetBulb) THEN
            Node(FollowSysNodeTempSetPtMgr(SetPtMgrNum)%RefNodeNum)%SPMNodeWetbulbRepReq = .TRUE.
          ENDIF
        ENDIF
        IF (FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
          Node(NodeNum)%TempSetPoint = 20.0d0 ! Set the setpoint
        ELSEIF (FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MaxTemp) THEN
          Node(NodeNum)%TempSetPointHi = 20.0d0 ! Set the setpoint
        ELSEIF (FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MinTemp) THEN
          Node(NodeNum)%TempSetPointLo = 20.0d0 ! Set the setpoint
        END IF
      END IF
    END DO
  END DO

  DO SetPtMgrNum=1,NumGroundTempSetPtMgrs
    DO CtrlNodeIndex=1,GroundTempSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = GroundTempSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
      IF (GroundTempSetPtMgr(SetPtMgrNum)%RefTypeMode == iRefGroundTempObjType_BuildingSurface) THEN
        IF (GroundTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
          Node(NodeNum)%TempSetPoint = GroundTemp ! Set the setpoint
        ELSEIF (GroundTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MaxTemp) THEN
          Node(NodeNum)%TempSetPointHi = GroundTemp ! Set the setpoint
        ELSEIF (GroundTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MinTemp) THEN
          Node(NodeNum)%TempSetPointLo = GroundTemp ! Set the setpoint
        END IF
      ELSEIF (GroundTempSetPtMgr(SetPtMgrNum)%RefTypeMode == iRefGroundTempObjType_Shallow) THEN
        IF (GroundTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
          Node(NodeNum)%TempSetPoint = GroundTemp_Surface ! Set the setpoint
        ELSEIF (GroundTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MaxTemp) THEN
          Node(NodeNum)%TempSetPointHi = GroundTemp_Surface ! Set the setpoint
        ELSEIF (GroundTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MinTemp) THEN
          Node(NodeNum)%TempSetPointLo = GroundTemp_Surface ! Set the setpoint
        END IF
      ELSEIF (GroundTempSetPtMgr(SetPtMgrNum)%RefTypeMode == iRefGroundTempObjType_Deep) THEN
        IF (GroundTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
          Node(NodeNum)%TempSetPoint = GroundTemp_Deep ! Set the setpoint
        ELSEIF (GroundTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MaxTemp) THEN
          Node(NodeNum)%TempSetPointHi = GroundTemp_Deep ! Set the setpoint
        ELSEIF (GroundTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MinTemp) THEN
          Node(NodeNum)%TempSetPointLo = GroundTemp_Deep ! Set the setpoint
        END IF
      ELSEIF (GroundTempSetPtMgr(SetPtMgrNum)%RefTypeMode == iRefGroundTempObjType_FCfactorMethod) THEN
        IF (GroundTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
          Node(NodeNum)%TempSetPoint = GroundTempFC ! Set the setpoint
        ELSEIF (GroundTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MaxTemp) THEN
          Node(NodeNum)%TempSetPointHi = GroundTempFC ! Set the setpoint
        ELSEIF (GroundTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MinTemp) THEN
          Node(NodeNum)%TempSetPointLo = GroundTempFC ! Set the setpoint
        END IF
      END IF
    END DO
  END DO

  DO SetPtMgrNum=1,NumCondEntSetPtMgrs    ! Condenser entering water Set point managers
    DO CtrlNodeIndex=1,CondEntSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = CondEntSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
      IF (CondEntSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
         Node(NodeNum)%TempSetPoint = GetCurrentScheduleValue(CondEntSetPtMgr(SetPtMgrNum)%CondEntTempSchedPtr)
      END IF
    END DO
  END DO

  DO SetPtMgrNum=1,NumIdealCondEntSetPtMgrs    ! Ideal Condenser entering water Set point managers
    DO CtrlNodeIndex=1,IdealCondEntSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = IdealCondEntSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
      IF (IdealCondEntSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
         Node(NodeNum)%TempSetPoint = IdealCondEntSetPtMgr(SetPtMgrNum)%MaxCondEntTemp
      END IF
    END DO
  END DO


  DO SetPtMgrNum=1, NumSZOneStageCoolingSetPtMgrs
    DO CtrlNodeIndex=1,SZOneStageCoolingSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = SZOneStageCoolingSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
      IF (SZOneStageCoolingSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
         Node(NodeNum)%TempSetPoint = SZOneStageCoolingSetPtMgr(SetPtMgrNum)%CoolingOffTemp
      END IF
    END DO
  ENDDO


  DO SetPtMgrNum=1,NumSZOneStageHeatingSetPtMgrs
    DO CtrlNodeIndex=1,SZOneStageHeatingSetPtMgr(SetPtMgrNum)%NumCtrlNodes
      NodeNum = SZOneStageHeatingSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
      IF (SZOneStageHeatingSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
         Node(NodeNum)%TempSetPoint = SZOneStageHeatingSetPtMgr(SetPtMgrNum)%HeatingOffTemp
      END IF
    END DO
  ENDDO
  MyEnvrnFlag = .FALSE.
  IF ( .not. MyOneTimeFlag) MyOneTimeFlag2 = .FALSE.

  IF (ErrorsFound) THEN
    CALL ShowFatalError('InitSetPointManagers: Errors found. Program Terminates.')
  ENDIF

END IF ! end begin environment inits
IF (.not. BeginEnvrnFlag) THEN
  MyEnvrnFlag = .TRUE.
ENDIF

RETURN
END  SUBROUTINE InitSetPointManagers


SUBROUTINE SimSetPointManagers

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   July 1998
          !       MODIFIED       Shirey/Raustad (FSEC), Jan 2004
          !                      Nov 2004 M. J. Witte, GARD Analytics, Inc.
          !                        Add new setpoint managers:
          !                          SET POINT MANAGER:SINGLE ZONE HEATING and
          !                          SET POINT MANAGER:SINGLE ZONE COOLING
          !                        Work supported by ASHRAE research project 1254-RP
          !                      Haves Oct 2004
          !                      July 2010 B.A. Nigusse, FSEC/UCF
          !                        Added new setpoint managers
          !                          SetpointManager:MultiZone:Heating:Average
          !                          SetpointManager:MultiZone:Cooling:Average
          !                          SetpointManager:MultiZone:MinimumHumidity:Average
          !                          SetpointManager:MultiZone:MaximumHumidity:Average
          !                      Aug 2010 B.A. Nigusse, FSEC/UCF
          !                        Added new setpoint managers:
          !                          SetpointManager:MultiZone:Humidity:Minimum
          !                          SetpointManager:MultiZone:Humidity:Maximum
          !
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Loop over all the Setpoint Managers and invoke the correct
          ! Setpoint Manager algorithm.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

    IMPLICIT NONE

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: SetPtMgrNum

! Execute all the Setpoint Managers

! The Scheduled Setpoint Managers

DO SetPtMgrNum=1,NumSchSetPtMgrs

  CALL CalcScheduledSetPoint(SetPtMgrNum)

END DO

! The Scheduled Dual Setpoint Managers

DO SetPtMgrNum=1,NumDualSchSetPtMgrs

  CALL CalcScheduledDualSetPoint(SetPtMgrNum)

END DO

! The Outside Air Setpoint Managers

DO SetPtMgrNum=1,NumOutAirSetPtMgrs

  CALL CalcOutsideAirSetPoint(SetPtMgrNum)

END DO

! The Single Zone Reheat Setpoint Managers

DO SetPtMgrNum=1,NumSZRhSetPtMgrs

  CALL CalcSingZoneRhSetPoint(SetPtMgrNum)

END DO

! The Single Zone Heating Setpoint Managers

DO SetPtMgrNum=1,NumSZHtSetPtMgrs

  CALL CalcSingZoneHtSetPoint(SetPtMgrNum)

END DO

! The Single Zone Cooling Setpoint Managers

DO SetPtMgrNum=1,NumSZClSetPtMgrs

  CALL CalcSingZoneClSetPoint(SetPtMgrNum)

END DO

! The Single Zone Minimum Humidity Setpoint Managers

DO SetPtMgrNum=1,NumSZMinHumSetPtMgrs

  CALL CalcSingZoneMinHumSetPoint(SetPtMgrNum)

END DO

! The Single Zone Maximum Humidity Setpoint Managers

DO SetPtMgrNum=1,NumSZMaxHumSetPtMgrs

  CALL CalcSingZoneMaxHumSetPoint(SetPtMgrNum)

END DO

! The Warmest Setpoint Managers

DO SetPtMgrNum=1,NumWarmestSetPtMgrs

  CALL CalcWarmestSetPoint(SetPtMgrNum)

END DO

! The Coldest Setpoint Managers

DO SetPtMgrNum=1,NumColdestSetPtMgrs

  CALL CalcColdestSetPoint(SetPtMgrNum)

END DO

! The Warmest Temp Flow Setpoint Managers

DO SetPtMgrNum=1,NumWarmestSetPtMgrsTempFlow

  CALL CalcWarmestSetPointTempFlow(SetPtMgrNum)

END DO

! The RAB Temp Flow Setpoint Managers

DO SetPtMgrNum=1,NumRABFlowSetPtMgrs

  CALL CalcRABFlowSetPoint(SetPtMgrNum)

END DO

! The Multizone Average Cooling Setpoint Managers

DO SetPtMgrNum=1,NumMZClgAverageSetPtMGrs

  CALL CalcMultiZoneAverageCoolingSetPoint(SetPtMgrNum)

END DO

! The Multizone Average Heating Setpoint Managers

DO SetPtMgrNum=1,NumMZHtgAverageSetPtMGrs

  CALL CalcMultiZoneAverageHeatingSetPoint(SetPtMgrNum)

END DO

! The Multizone Average Minimum Humidity Setpoint Managers

DO SetPtMgrNum=1,NumMZAverageMinHumSetPtMgrs

  CALL CalcMultiZoneAverageMinHumSetPoint(SetPtMgrNum)

END DO

! The Multizone Average Maximum Humidity Setpoint Managers

DO SetPtMgrNum=1,NumMZAverageMaxHumSetPtMgrs

  CALL CalcMultiZoneAverageMaxHumSetPoint(SetPtMgrNum)

END DO

! The Multizone Minimum Humidity Ratio Setpoint Managers
DO SetPtMgrNum=1,NumMZMinHumSetPtMgrs

  CALL CalcMultiZoneMinHumSetPoint(SetPtMgrNum)

END DO

! The Multizone Maximum Humidity Ratio Setpoint Managers
DO SetPtMgrNum=1,NumMZMaxHumSetPtMgrs

  CALL CalcMultiZoneMaxHumSetPoint(SetPtMgrNum)

END DO

! The Follow Outdoor Air  Temperature Setpoint Managers
DO SetPtMgrNum=1,NumFollowOATempSetPtMgrs

  CALL CalcFollowOATempSetPoint(SetPtMgrNum)

END DO

! The Follow System Node Temp Setpoint Managers
DO SetPtMgrNum=1,NumFollowSysNodeTempSetPtMgrs

  CALL CalcFollowSysNodeTempSetPoint(SetPtMgrNum)

END DO

! The Ground Temp Setpoint Managers
DO SetPtMgrNum=1,NumGroundTempSetPtMgrs

  CALL CalcGroundTempSetPoint(SetPtMgrNum)

END DO

! The Condenser Entering Water Temperature Set Point Managers
DO SetPtMgrNum=1,NumCondEntSetPtMgrs

 CALL CalcCondEntSetPoint(SetPtMgrNum)

END DO

! The Ideal Condenser Entering Water Temperature Set Point Managers
DO SetPtMgrNum=1,NumIdealCondEntSetPtMgrs

 CALL CalcIdealCondEntSetPoint(SetPtMgrNum)

END DO

! the single zone cooling on/off staged control setpoint managers
DO SetPtMgrNum=1, NumSZOneStageCoolingSetPtMgrs
  CALL CalcSZOneStageCoolingSetPt(SetPtMgrNum)
ENDDO

! the single zone heating on/off staged control setpoint managers
DO SetPtMgrNum=1, NumSZOneStageHeatingSetPtMgrs
  CALL CalcSZOneStageHeatingSetPt(SetPtMgrNum)
ENDDO

RETURN

END SUBROUTINE SimSetPointManagers

SUBROUTINE CalcScheduledSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   July 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set the setpoint using a simple schedule.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENTS:
INTEGER :: SetPtMgrNum

SchSetPtMgr(SetPtMgrNum)%SetPt = GetCurrentScheduleValue(SchSetPtMgr(SetPtMgrNum)%SchedPtr)

RETURN
END SUBROUTINE CalcScheduledSetPoint


SUBROUTINE CalcScheduledDualSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   May 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set the both setpoint using a simple schedule.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

    IMPLICIT NONE

          ! SUBROUTINE ARGUMENTS:
INTEGER :: SetPtMgrNum

DualSchSetPtMgr(SetPtMgrNum)%SetPtHi = GetCurrentScheduleValue(DualSchSetPtMgr(SetPtMgrNum)%SchedPtrHi)
DualSchSetPtMgr(SetPtMgrNum)%SetPtLo = GetCurrentScheduleValue(DualSchSetPtMgr(SetPtMgrNum)%SchedPtrLo)

RETURN
END SUBROUTINE CalcScheduledDualSetPoint


SUBROUTINE CalcOutsideAirSetPoint(SetPtMgrNum, NodeNum, InitFlag)

          ! SUBROUTINE ARGUMENTS:
INTEGER :: SetPtMgrNum
INTEGER, INTENT(IN), OPTIONAL :: NodeNum  !When Init Calls this routine, it passes the cur node number
LOGICAL, INTENT(IN), OPTIONAL :: InitFlag !When Init Calls this routine, it passes True

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64) :: SchedVal
REAL(r64) :: OutLowTemp
REAL(r64) :: OutHighTemp
REAL(r64) :: SetTempAtOutLow
REAL(r64) :: SetTempAtOutHigh
INTEGER :: SchedPtr
REAL(r64) :: SetPt

SchedPtr = OutAirSetPtMgr(SetPtMgrNum)%SchedPtr

IF (SchedPtr.GT.0) THEN
  SchedVal = GetCurrentScheduleValue(SchedPtr)
ELSE
  SchedVal = 0.0d0
END IF

IF (SchedVal.EQ.2.d0) THEN
  OutLowTemp = OutAirSetPtMgr(SetPtMgrNum)%OutLow2
  OutHighTemp = OutAirSetPtMgr(SetPtMgrNum)%OutHigh2
  SetTempAtOutLow = OutAirSetPtMgr(SetPtMgrNum)%OutLowSetPt2
  SetTempAtOutHigh = OutAirSetPtMgr(SetPtMgrNum)%OutHighSetPt2
ELSE
  OutLowTemp = OutAirSetPtMgr(SetPtMgrNum)%OutLow1
  OutHighTemp = OutAirSetPtMgr(SetPtMgrNum)%OutHigh1
  SetTempAtOutLow = OutAirSetPtMgr(SetPtMgrNum)%OutLowSetPt1
  SetTempAtOutHigh = OutAirSetPtMgr(SetPtMgrNum)%OutHighSetPt1
END IF

IF (OutLowTemp.LT.OutHighTemp .AND. SetTempAtOutLow.GT.SetTempAtOutHigh) THEN

  IF (OutDryBulbTemp.LE.OutLowTemp) THEN
    SetPt = SetTempAtOutLow
  ELSE IF (OutDryBulbTemp.GE.OutHighTemp) THEN
    SetPt = SetTempAtOutHigh
  ELSE
    SetPt = SetTempAtOutLow - ((OutDryBulbTemp - OutLowTemp)/(OutHighTemp-OutLowTemp))*(SetTempAtOutLow - SetTempAtOutHigh)
  END IF

ELSE
  SetPt = 0.5d0*(SetTempAtOutLow + SetTempAtOutHigh)
END IF

IF(PRESENT(InitFlag))THEN
  Node(NodeNum)%TempSetPoint = SetPt       !Setpoint for Initial Routine
ELSE
  OutAirSetPtMgr(SetPtMgrNum)%SetPt = SetPt !Setpoint for Calc Routine
ENDIF

RETURN

END SUBROUTINE CalcOutsideAirSetPoint

SUBROUTINE CalcSingZoneRhSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! From the heating or cooling load of the control zone, calculate the supply air setpoint
          ! needed to meet that zone load

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands
  USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad
  USE Psychrometrics, ONLY:PsyTdbFnHW

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum         ! number of the current setpoint manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS


          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)      :: ZoneLoad         ! required zone load [W]
  REAL(r64)      :: ZoneMassFlow     ! zone inlet mass flow rate [kg/s]
  REAL(r64)      :: CpAir            ! inlet air specific heat [J/kg-C]
  INTEGER        :: ZoneInletNode
  INTEGER        :: ZoneNode
  INTEGER        :: ZoneNum
  REAL(r64)      :: ZoneTemp
  REAL(r64)      :: ZoneLoadToCoolSetPt
  REAL(r64)      :: ZoneLoadToHeatSetPt
  REAL(r64)      :: TSetPt
  REAL(r64)      :: TSetPt1
  REAL(r64)      :: TSetPt2
  Logical        :: DeadBand
  INTEGER        :: FanNodeIn
  INTEGER        :: FanNodeOut
  INTEGER        :: RetNode
  INTEGER        :: OAMixOAInNode
  REAL(r64)      :: FanDeltaT
  REAL(r64)      :: TSupNoHC = 0.0d0     ! supply temperature with no heating or cooling
  REAL(r64)      :: TMixAtMinOA
  REAL(r64)      :: EnthMixAtMinOA
  REAL(r64)      :: HumRatMixAtMinOA
  INTEGER        :: AirLoopNum
  REAL(r64)      :: MinOAFrac
  INTEGER        :: LoopInNode
  REAL(r64)      :: ExtrRateNoHC = 0.0d0 ! the heating (>0) or cooling (<0) that can be done by supply air at TSupNoHC [W]

ZoneInletNode = SingZoneRhSetPtMgr(SetPtMgrNum)%ZoneInletNodeNum
ZoneNum = SingZoneRhSetPtMgr(SetPtMgrNum)%ControlZoneNum
ZoneNode = SingZoneRhSetPtMgr(SetPtMgrNum)%ZoneNodeNum
FanNodeIn = SingZoneRhSetPtMgr(SetPtMgrNum)%FanNodeIn
FanNodeOut = SingZoneRhSetPtMgr(SetPtMgrNum)%FanNodeOut
RetNode = SingZoneRhSetPtMgr(SetPtMgrNum)%RetNode
OAMixOAInNode = SingZoneRhSetPtMgr(SetPtMgrNum)%OAInNode
AirLoopNum = SingZoneRhSetPtMgr(SetPtMgrNum)%AirLoopNum
MinOAFrac = AirLoopFlow(AirLoopNum)%OAMinFrac
ZoneMassFlow = Node(ZoneInletNode)%MassFlowRate
ZoneLoad = ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired
ZoneLoadToCoolSetPt = ZoneSysEnergyDemand(ZoneNum)%OutputRequiredToCoolingSP
ZoneLoadToHeatSetPt = ZoneSysEnergyDemand(ZoneNum)%OutputRequiredToHeatingSP
DeadBand = DeadbandOrSetback(ZoneNum)
ZoneTemp = Node(ZoneNode)%Temp
LoopInNode = SingZoneRhSetPtMgr(SetPtMgrNum)%LoopInNode
IF (OAMixOAInNode > 0) THEN
  HumRatMixAtMinOA = (1.0d0-MinOAFrac)*Node(RetNode)%HumRat + MinOAFrac*Node(OAMixOAInNode)%HumRat
  EnthMixAtMinOA = (1.0d0-MinOAFrac)*Node(RetNode)%Enthalpy + MinOAFrac*Node(OAMixOAInNode)%Enthalpy
  TMixAtMinOA = PsyTdbFnHW(EnthMixAtMinOA,HumRatMixAtMinOA)
ELSE
  TMixAtMinOA = Node(LoopInNode)%Temp
END IF
IF (FanNodeOut > 0 .and. FanNodeIn > 0) THEN
  FanDeltaT = Node(FanNodeOut)%Temp - Node(FanNodeIn)%Temp
ELSE
  FanDeltaT = 0.0d0
END IF
TSupNoHC = TMixAtMinOA + FanDeltaT
CpAir = PsyCpAirFnWTdb(Node(ZoneInletNode)%HumRat,Node(ZoneInletNode)%Temp)
ExtrRateNoHC = CpAir*ZoneMassFlow*(TSupNoHC - ZoneTemp)
IF (ZoneMassFlow.LE.SmallMassFlow) THEN
  TSetPt = TSupNoHC
ELSE IF (Deadband .OR. ABS(ZoneLoad) < SmallLoad) THEN
  ! if air with no active heating or cooling provides cooling
  IF (ExtrRateNoHC < 0.0d0) THEN
    ! if still in deadband, do no active heating or cooling;
    ! if below heating setpoint, set a supply temp that will cool to the heating setpoint
    IF (ExtrRateNoHC >= ZoneLoadToHeatSetPt) THEN
      TSetPt = TSupNoHC
    ELSE
      TSetPt = ZoneTemp + ZoneLoadToHeatSetPt/(CpAir*ZoneMassFlow)
    END IF
  ! if air with no active heating or cooling provides heating
  ELSE IF (ExtrRateNoHC > 0.0d0) THEN
    ! if still in deadband, do no active heating or cooling;
    ! if above cooling setpoint, set a supply temp that will heat to the cooling setpoint
    IF (ExtrRateNoHC <= ZoneLoadToCoolSetPt) THEN
      TSetPt = TSupNoHC
    ELSE
      TSetPt = ZoneTemp + ZoneLoadToCoolSetPt/(CpAir*ZoneMassFlow)
    END IF
  ELSE
    TSetPt = TSupNoHC
  END IF
ELSE IF (ZoneLoad < (-1.d0*SmallLoad)) THEN
  TSetPt1 = ZoneTemp + ZoneLoad/(CpAir*ZoneMassFlow)
  TSetPt2 = ZoneTemp + ZoneLoadToHeatSetPt/(CpAir*ZoneMassFlow)
  IF (TSetPt1 > TSupNoHC) THEN
    IF (TSetPt2 > TSupNoHC) THEN
      TSetPt = TSetPt2
    ELSE
      TSetPt = TSupNoHC
    END IF
  ELSE
    TSetPt = TSetPt1
  END IF
ELSE IF (ZoneLoad > SmallLoad) THEN
  TSetPt1 = ZoneTemp + ZoneLoad/(CpAir*ZoneMassFlow)
  TSetPt2 = ZoneTemp + ZoneLoadToCoolSetPt/(CpAir*ZoneMassFlow)
  IF (TSetPt1 < TSupNoHC) THEN
    IF (TSetPt2 < TSupNoHC) THEN
      TSetPt = TSetPt2
    ELSE
      TSetPt =TSupNoHC
    END IF
  ELSE
    TSetPt = TSetPt1
  END IF
ELSE
  TSetPt = TSupNoHC
END IF

TSetPt = MAX(MIN(TSetPt,SingZoneRhSetPtMgr(SetPtMgrNum)%MaxSetTemp),SingZoneRhSetPtMgr(SetPtMgrNum)%MinSetTemp)
SingZoneRhSetPtMgr(SetPtMgrNum)%SetPt = TSetPt

RETURN
END SUBROUTINE CalcSingZoneRhSetPoint

SUBROUTINE CalcSingZoneHtSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         M. J. Witte based on CalcSingZoneRhSetPoint by Fred Buhl,
          !                        Work supported by ASHRAE research project 1254-RP
          !       DATE WRITTEN   November 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! From the heating load of the control zone, calculate the supply air setpoint
          ! needed to meet that zone load (based on CalcSingZoneRhSetPoint)

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand
  USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum         ! number of the current setpoint manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS


          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)      :: ZoneLoadtoHeatSP ! required zone load to zone heating setpoint [W]
  REAL(r64)      :: ZoneMassFlow     ! zone inlet mass flow rate [kg/s]
  REAL(r64)      :: CpAir            ! inlet air specific heat [J/kg-C]
  INTEGER        :: ZoneInletNode
  INTEGER        :: ZoneNode
  INTEGER        :: ZoneNum
  REAL(r64)      :: ZoneTemp

ZoneInletNode = SingZoneHtSetPtMgr(SetPtMgrNum)%ZoneInletNodeNum
ZoneNum = SingZoneHtSetPtMgr(SetPtMgrNum)%ControlZoneNum
ZoneNode = SingZoneHtSetPtMgr(SetPtMgrNum)%ZoneNodeNum
ZoneMassFlow = Node(ZoneInletNode)%MassFlowRate
ZoneLoadtoHeatSP = ZoneSysEnergyDemand(ZoneNum)%OutputRequiredToHeatingSP
ZoneTemp = Node(ZoneNode)%Temp
!CR7654 IF (ZoneLoadtoHeatSP.GT.0.0) THEN
  IF (ZoneMassFlow.LE.SmallMassFlow) THEN
    SingZoneHtSetPtMgr(SetPtMgrNum)%SetPt = SingZoneHtSetPtMgr(SetPtMgrNum)%MaxSetTemp
  ELSE
    CpAir = PsyCpAirFnWTdb(Node(ZoneInletNode)%HumRat,Node(ZoneInletNode)%Temp)
    SingZoneHtSetPtMgr(SetPtMgrNum)%SetPt = ZoneTemp + ZoneLoadtoHeatSP/(CpAir*ZoneMassFlow)
    SingZoneHtSetPtMgr(SetPtMgrNum)%SetPt = &
      MAX(SingZoneHtSetPtMgr(SetPtMgrNum)%SetPt, SingZoneHtSetPtMgr(SetPtMgrNum)%MinSetTemp)
    SingZoneHtSetPtMgr(SetPtMgrNum)%SetPt = &
      MIN(SingZoneHtSetPtMgr(SetPtMgrNum)%SetPt, SingZoneHtSetPtMgr(SetPtMgrNum)%MaxSetTemp)
  END IF
!CR7654 ELSE
!CR7654   SingZoneHtSetPtMgr(SetPtMgrNum)%SetPt = SingZoneHtSetPtMgr(SetPtMgrNum)%MinSetTemp
!CR7654 END IF

RETURN
END SUBROUTINE CalcSingZoneHtSetPoint

SUBROUTINE CalcSingZoneClSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         M. J. Witte based on CalcSingZoneRhSetPoint by Fred Buhl,
          !                        Work supported by ASHRAE research project 1254-RP
          !       DATE WRITTEN   November 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! From the Cooling load of the control zone, calculate the supply air setpoint
          ! needed to meet that zone load (based on CalcSingZoneRhSetPoint)

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand
  USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum         ! number of the current setpoint manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS


          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)      :: ZoneLoadtoCoolSP ! required zone load to zone Cooling setpoint [W]
  REAL(r64)      :: ZoneMassFlow     ! zone inlet mass flow rate [kg/s]
  REAL(r64)      :: CpAir            ! inlet air specific Cool [J/kg-C]
  INTEGER        :: ZoneInletNode
  INTEGER        :: ZoneNode
  INTEGER        :: ZoneNum
  REAL(r64)      :: ZoneTemp

ZoneInletNode = SingZoneClSetPtMgr(SetPtMgrNum)%ZoneInletNodeNum
ZoneNum = SingZoneClSetPtMgr(SetPtMgrNum)%ControlZoneNum
ZoneNode = SingZoneClSetPtMgr(SetPtMgrNum)%ZoneNodeNum
ZoneMassFlow = Node(ZoneInletNode)%MassFlowRate
ZoneLoadtoCoolSP = ZoneSysEnergyDemand(ZoneNum)%OutputRequiredToCoolingSP
ZoneTemp = Node(ZoneNode)%Temp
!CR7654 IF (ZoneLoadtoCoolSP.LT.0.0) THEN
  IF (ZoneMassFlow.LE.SmallMassFlow) THEN
    SingZoneClSetPtMgr(SetPtMgrNum)%SetPt = SingZoneClSetPtMgr(SetPtMgrNum)%MinSetTemp
  ELSE
    CpAir = PsyCpAirFnWTdb(Node(ZoneInletNode)%HumRat,Node(ZoneInletNode)%Temp)
    SingZoneClSetPtMgr(SetPtMgrNum)%SetPt = ZoneTemp + ZoneLoadtoCoolSP/(CpAir*ZoneMassFlow)
    SingZoneClSetPtMgr(SetPtMgrNum)%SetPt = &
      MAX(SingZoneClSetPtMgr(SetPtMgrNum)%SetPt, SingZoneClSetPtMgr(SetPtMgrNum)%MinSetTemp)
    SingZoneClSetPtMgr(SetPtMgrNum)%SetPt = &
      MIN(SingZoneClSetPtMgr(SetPtMgrNum)%SetPt, SingZoneClSetPtMgr(SetPtMgrNum)%MaxSetTemp)
  END IF
!CR7654 ELSE
!CR7654   SingZoneClSetPtMgr(SetPtMgrNum)%SetPt = SingZoneClSetPtMgr(SetPtMgrNum)%MaxSetTemp
!CR7654 END IF

RETURN
END SUBROUTINE CalcSingZoneClSetPoint

SUBROUTINE CalcSZOneStageCoolingSetPt(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   August 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! calculate the setpoint for staged on/off cooling

          ! METHODOLOGY EMPLOYED:
          ! Evaluate stage in zone energy demand structure and choose setpoint accordingly

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum         ! number of the current setpoint manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (ZoneSysEnergyDemand(SZOneStageCoolingSetPtMgr(SetPtMgrNum)%ControlZoneNum)%StageNum >= 0) THEN
    SZOneStageCoolingSetPtMgr(SetPtMgrNum)%SetPt = SZOneStageCoolingSetPtMgr(SetPtMgrNum)%CoolingOffTemp
  ELSE ! negative so a cooling stage is set
    SZOneStageCoolingSetPtMgr(SetPtMgrNum)%SetPt = SZOneStageCoolingSetPtMgr(SetPtMgrNum)%CoolingOnTemp
  ENDIF

  RETURN

END SUBROUTINE CalcSZOneStageCoolingSetPt

SUBROUTINE CalcSZOneStageHeatingSetPt(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   August 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! calculate the setpoint for staged on/off control

          ! METHODOLOGY EMPLOYED:
          ! Evaluate stage in zone energy demand structure and choose setpoint accordingly

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum         ! number of the current setpoint manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
  IF (ZoneSysEnergyDemand(SZOneStageHeatingSetPtMgr(SetPtMgrNum)%ControlZoneNum)%StageNum <= 0) THEN
    SZOneStageHeatingSetPtMgr(SetPtMgrNum)%SetPt = SZOneStageHeatingSetPtMgr(SetPtMgrNum)%HeatingOffTemp
  ELSE ! positive so a heating stage is set
    SZOneStageHeatingSetPtMgr(SetPtMgrNum)%SetPt = SZOneStageHeatingSetPtMgr(SetPtMgrNum)%HeatingOnTemp
  ENDIF

  RETURN

END SUBROUTINE CalcSZOneStageHeatingSetPt

SUBROUTINE CalcSingZoneMinHumSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   October 2000
          !       MODIFIED       Shirey/Raustad Jan 2002
          !                      Gu, Dec 2007
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! From humidity load of the control zone, calculate the supply air humidity
          ! needed to meet the minimum humidity setpoint

          ! METHODOLOGY EMPLOYED:
          ! Zone moisture load from ZoneTempPredictorCorrector (via DataZoneEnergyDemands)
          ! is used to calculate the minimum supply air humidity ratio
          ! needed to meet minimum zone relative humidity requirement

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY: SmallMassFlow
  USE DataZoneEnergyDemands, ONLY: ZoneSysMoistureDemand
  USE Psychrometrics, ONLY:PsyWFnTdbRhPb

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum         ! number of the current setpoint manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS


          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER  :: ZoneNode
REAL(r64)     :: ZoneMassFlow
!REAL(r64)     :: RelHumSet
!REAL(r64)     :: ZoneHumRatSet
INTEGER  :: ZoneNum
REAL(r64)     :: MoistureLoad    ! Zone moisture load (kg moisture/second) required to meet the relative humidity setpoint
                            ! Value obtained from ZoneTempPredictorCorrector (via ZoneSysMoistureDemand in DataZoneEnergyDemands)
REAL(r64)     :: SupplyAirHumRat ! Desired air humidity ratio

SZMinHumSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0
! Only use one zone for now
ZoneNode = SZMinHumSetPtMgr(SetPtMgrNum)%ZoneNodes(1)
ZoneMassFlow = Node(ZoneNode)%MassFlowRate
ZoneNum = SZMinHumSetPtMgr(SetPtMgrNum)%ZoneNum(1)

IF (ZoneMassFlow.GT.SmallMassFlow) THEN

  MoistureLoad= ZoneSysMoistureDemand(SZMinHumSetPtMgr(SetPtMgrNum)%ZoneNum(1))%OutputRequiredToHumidifyingSP

  SupplyAirHumRat = MAX(0.0d0, Node(ZoneNode)%HumRat + MoistureLoad/ZoneMassFlow)

! Positive Humidity Ratio MoistureLoad means a humidification load and only humidifying can raise up to a minimum
!  IF(MoistureLoad .GT. 0.0) SZMinHumSetPtMgr(SetPtMgrNum)%SetPt = SupplyAirHumRat
  SZMinHumSetPtMgr(SetPtMgrNum)%SetPt = SupplyAirHumRat

END IF

RETURN

END SUBROUTINE CalcSingZoneMinHumSetPoint

SUBROUTINE CalcSingZoneMaxHumSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Raustad/Shirey, FSEC
          !       DATE WRITTEN   January 2004
          !       MODIFIED       Gu, Dec. 2007
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! From humidity load of the control zone, calculate the supply air humidity
          ! needed to meet the maximum humidity setpoint

          ! METHODOLOGY EMPLOYED:
          ! Zone moisture load from ZoneTempPredictorCorrector (via DataZoneEnergyDemands)
          ! is used to calculate the maximum supply air humidity ratio
          ! needed to meet maximum zone relative humidity requirement

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataHVACGlobals, ONLY: SmallMassFlow
  USE DataZoneEnergyDemands, ONLY: ZoneSysMoistureDemand
  USE Psychrometrics, ONLY:PsyWFnTdbRhPb

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum         ! number of the current setpoint manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS


          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER  :: ZoneNode        ! Control zone air node number
REAL(r64)     :: ZoneMassFlow    ! Zone air mass flow rate (kg/s)
!REAL(r64)     :: RelHumSet       ! Zone air relative humidity setpoint for this time step (fraction)
!REAL(r64)     :: ZoneHumRatSet   ! Zone air humidity ratio setpoint for this time step (kg/kg)
REAL(r64)     :: MoistureLoad    ! Zone moisture load (kg moisture/sec) required to meet the relative humidity setpoint
                            ! Value obtained from ZoneTempPredictorCorrector (via ZoneSysMoistureDemand in DataZoneEnergyDemands)
REAL(r64)     :: SupplyAirHumRat ! Desired air humidity ratio
REAL(r64)     :: SystemMassFlow !

SZMaxHumSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0
! Only use one zone for now
ZoneNode = SZMaxHumSetPtMgr(SetPtMgrNum)%ZoneNodes(1)
ZoneMassFlow = Node(ZoneNode)%MassFlowRate

IF (ZoneMassFlow.GT.SmallMassFlow) THEN

  MoistureLoad= ZoneSysMoistureDemand(SZMaxHumSetPtMgr(SetPtMgrNum)%ZoneNum(1))%OutputRequiredToDehumidifyingSP

  SystemMassFlow = Node(SZMaxHumSetPtMgr(SetPtMgrNum)%CtrlNodes(1))%MassFlowRate

! MoistureLoad (negative for dehumidification) may be so large that a negative humrat results, cap at 0.00001
  SupplyAirHumRat = MAX(0.00001d0,Node(ZoneNode)%HumRat + MoistureLoad/ZoneMassFlow)

! This hum rat is currently used in Controller:Simple, control variable "TEMPandHUMRAT" (Jan 2004)
! Negative MoistureLoad means a dehumidification load
  IF(MoistureLoad .LT. 0.0d0) SZMaxHumSetPtMgr(SetPtMgrNum)%SetPt = SupplyAirHumRat

END IF

RETURN

END SUBROUTINE CalcSingZoneMaxHumSetPoint

SUBROUTINE CalcMixedAirSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Starting with the setpoint at the reference node, subtract the supply fan
          ! temperature rise and set the resulting temperature at the mixed air node.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: SysSizingCalc, AnyEnergyManagementSystemInModel
  USE DataHVACGlobals, ONLY: SetPointErrorFlag
  USE EMSManager,      ONLY: iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum         ! number of the current setpoint manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS


          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: FanInNode   ! supply fan inlet node number
  INTEGER :: FanOutNode  ! supply fan outlet node number
  INTEGER :: RefNode     ! setpoint reference node number

FanInNode = MixedAirSetPtMgr(SetPtMgrNum)%FanInNode
FanOutNode = MixedAirSetPtMgr(SetPtMgrNum)%FanOutNode
RefNode = MixedAirSetPtMgr(SetPtMgrNum)%RefNode

IF ( .NOT. SysSizingCalc .AND. MixedAirSetPtMgr(SetPtMgrNum)%MySetPointCheckFlag) THEN

  RefNode = MixedAirSetPtMgr(SetPtMgrNum)%RefNode
  IF (Node(RefNode)%TempSetPoint == SensedNodeFlagValue) THEN
    IF (.NOT. AnyEnergyManagementSystemInModel) THEN
      CALL ShowSevereError('CalcMixedAirSetPoint: Missing reference temperature setpoint for Mixed Air Setpoint Manager '//  &
                           TRIM(MixedAirSetPtMgr(SetPtMgrNum)%Name))
      CALL ShowContinueError('Node Referenced ='//TRIM(NodeID(RefNode)))
      CALL ShowContinueError('  use an additional Setpoint Manager with Control Variable = "Temperature" to establish a '//  &
          'setpoint at this node.')
      SetPointErrorFlag = .TRUE.
    ELSE
      ! need call to check if this is the target of an EnergyManagementSystem:Actuator object
      CALL CheckIfNodeSetpointManagedByEMS(RefNode,iTemperatureSetpoint, SetpointErrorFlag)
      IF (SetpointErrorFlag) THEN
        CALL ShowSevereError('CalcMixedAirSetPoint: Missing reference temperature setpoint for Mixed Air Setpoint Manager '//  &
                             TRIM(MixedAirSetPtMgr(SetPtMgrNum)%Name))
        CALL ShowContinueError('Node Referenced ='//TRIM(NodeID(RefNode)))
        CALL ShowContinueError('  use an additional Setpoint Manager with Control Variable = "Temperature" to establish a '//  &
            'setpoint at this node.')
        CALL ShowContinueError('Or add EMS Actuator to provide temperature setpoint at this node')
      ENDIF
    ENDIF
  END IF

  MixedAirSetPtMgr(SetPtMgrNum)%MySetPointCheckFlag = .FALSE.
END IF

MixedAirSetPtMgr(SetPtMgrNum)%SetPt = Node(RefNode)%TempSetPoint - (Node(FanOutNode)%Temp - Node(FanInNode)%Temp)

RETURN

END SUBROUTINE CalcMixedAirSetPoint

SUBROUTINE CalcOAPretreatSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         M. J. Witte based on CalcMixedAirSetPoint by Fred Buhl,
          !                        Work supported by ASHRAE research project 1254-RP
          !       DATE WRITTEN   January 2005
          !       MODIFIED       Witte (GARD), Sep 2006
          !                      Griffith( NREL), May 2009, added EMS setpoint checks
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Starting with the setpoint at the reference node, determine the required
          ! outside air inlet conditions which when mixed with return air result in
          ! the reference setpoint at the mixed air node.
          ! (based on CalcMixedAirSetPoint)

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: SysSizingCalc, AnyEnergyManagementSystemInModel
  USE EMSManager,  ONLY: iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS, &
                         iHumidityRatioSetpoint, iHumidityRatioMinSetpoint, iHumidityRatioMaxSetpoint

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum         ! number of the current setpoint manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS


          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: RefNode         ! setpoint reference node number
  INTEGER :: MixedOutNode    ! mixed air outlet node number
  INTEGER :: OAInNode        ! outside air inlet node number
  INTEGER :: ReturnInNode    ! return air inlet node number
  REAL(r64)    :: OAFraction      ! outside air fraction of mixed flow rate
  REAL(r64)    :: ReturnInValue   ! return air inlet node mass flow rate
  REAL(r64)    :: RefNodeSetPoint ! setpoint at reference node
  REAL(r64)    :: MinSetPoint     ! minimum allowed setpoint
  REAL(r64)    :: MaxSetPoint     ! maximum allowed setpoint
  LOGICAL :: HumiditySetPoint ! logical to indicate if this is a humidity setpoint
  LOGICAL :: LocalSetpointCheckFailed = .FALSE.

RefNode = OAPretreatSetPtMgr(SetPtMgrNum)%RefNode
MixedOutNode = OAPretreatSetPtMgr(SetPtMgrNum)%MixedOutNode
OAInNode = OAPretreatSetPtMgr(SetPtMgrNum)%OAInNode
ReturnInNode = OAPretreatSetPtMgr(SetPtMgrNum)%ReturnInNode
HumiditySetPoint = .FALSE.

SELECT CASE(OAPretreatSetPtMgr(SetPtMgrNum)%CtrlTypeMode)
  CASE(iCtrlVarType_Temp)  ! 'Temperature'
     RefNodeSetPoint = Node(RefNode)%TempSetPoint
     ReturnInValue = Node(ReturnInNode)%Temp
     MinSetPoint = OAPretreatSetPtMgr(SetPtMgrNum)%MinSetTemp
     MaxSetPoint = OAPretreatSetPtMgr(SetPtMgrNum)%MaxSetTemp
  CASE(iCtrlVarType_MaxHumRat)  ! 'HUMRATMAX'
     RefNodeSetPoint = Node(RefNode)%HumRatMax
     ReturnInValue = Node(ReturnInNode)%HumRat
     MinSetPoint = OAPretreatSetPtMgr(SetPtMgrNum)%MinSetHumRat
     MaxSetPoint = OAPretreatSetPtMgr(SetPtMgrNum)%MaxSetHumRat
     HumiditySetPoint = .TRUE.
  CASE(iCtrlVarType_MinHumRat) ! 'HUMRATMIN'
     RefNodeSetPoint = Node(RefNode)%HumRatMin
     ReturnInValue = Node(ReturnInNode)%HumRat
     MinSetPoint = OAPretreatSetPtMgr(SetPtMgrNum)%MinSetHumRat
     MaxSetPoint = OAPretreatSetPtMgr(SetPtMgrNum)%MaxSetHumRat
     HumiditySetPoint = .TRUE.
  CASE(iCtrlVarType_HumRat) ! 'HumidityRatio'
     RefNodeSetPoint = Node(RefNode)%HumRatSetPoint
     ReturnInValue = Node(ReturnInNode)%HumRat
     MinSetPoint = OAPretreatSetPtMgr(SetPtMgrNum)%MinSetHumRat
     MaxSetPoint = OAPretreatSetPtMgr(SetPtMgrNum)%MaxSetHumRat
     HumiditySetPoint = .TRUE.
END SELECT

IF ( .NOT. SysSizingCalc .AND. OAPretreatSetPtMgr(SetPtMgrNum)%MySetPointCheckFlag) THEN
  OAPretreatSetPtMgr(SetPtMgrNum)%MySetPointCheckFlag = .FALSE.
  IF (RefNodeSetPoint == SensedNodeFlagValue) THEN
    IF (.NOT. AnyEnergyManagementSystemInModel) THEN
      CALL ShowSevereError('CalcOAPretreatSetPoint: Missing reference setpoint for Outdoor Air Pretreat Setpoint Manager '//  &
                           TRIM(OAPretreatSetPtMgr(SetPtMgrNum)%Name))
      CALL ShowContinueError('Node Referenced ='//TRIM(NodeID(RefNode)))
      CALL ShowContinueError('use a Setpoint Manager to establish a setpoint at this node.')
      CALL ShowFatalError('Missing reference setpoint.')
    ELSE
      LocalSetpointCheckFailed = .FALSE.
      SELECT CASE(OAPretreatSetPtMgr(SetPtMgrNum)%CtrlTypeMode)
      CASE(iCtrlVarType_Temp)       ! 'Temperature'
        CALL CheckIfNodeSetpointManagedByEMS(RefNode,iTemperatureSetpoint, LocalSetpointCheckFailed)
      CASE(iCtrlVarType_MaxHumRat)  ! 'HUMRATMAX'
        CALL CheckIfNodeSetpointManagedByEMS(RefNode,iHumidityRatioMaxSetpoint, LocalSetpointCheckFailed)
      CASE(iCtrlVarType_MinHumRat)  ! 'HUMRATMIN'
        CALL CheckIfNodeSetpointManagedByEMS(RefNode,iHumidityRatioMinSetpoint, LocalSetpointCheckFailed)
      CASE(iCtrlVarType_HumRat)     ! 'HumidityRatio'
        CALL CheckIfNodeSetpointManagedByEMS(RefNode,iHumidityRatioSetpoint, LocalSetpointCheckFailed)
      END SELECT
      IF (LocalSetpointCheckFailed) THEN
        CALL ShowSevereError('CalcOAPretreatSetPoint: Missing reference setpoint for Outdoor Air Pretreat Setpoint Manager '//  &
                             TRIM(OAPretreatSetPtMgr(SetPtMgrNum)%Name))
        CALL ShowContinueError('Node Referenced ='//TRIM(NodeID(RefNode)))
        CALL ShowContinueError('use a Setpoint Manager to establish a setpoint at this node.')
        CALL ShowContinueError('Or use an EMS actuator to control a setpoint at this node.')
        CALL ShowFatalError('Missing reference setpoint.')
      ENDIF
    ENDIF
  END IF
END IF
IF ((Node(MixedOutNode)%MassFlowRate .LE. 0.0d0) .OR. (Node(OAInNode)%MassFlowRate .LE. 0.0d0)) THEN
  OAPretreatSetPtMgr(SetPtMgrNum)%SetPt = RefNodeSetPoint
ELSEIF (HumiditySetPoint .AND. (RefNodeSetPoint == 0.0d0)) THEN
  ! For humidity setpoints, zero is special meaning "off" or "no load"
  ! so pass through zero setpoints without enforcing the max/min setpoint limits
  OAPretreatSetPtMgr(SetPtMgrNum)%SetPt = 0.0d0
ELSE
  OAFraction = Node(OAInNode)%MassFlowRate / Node(MixedOutNode)%MassFlowRate
  OAPretreatSetPtMgr(SetPtMgrNum)%SetPt = ReturnInValue + (RefNodeSetPoint - ReturnInValue)/OAFraction
  ! Apply maximum and minimum values
  OAPretreatSetPtMgr(SetPtMgrNum)%SetPt = MAX(OAPretreatSetPtMgr(SetPtMgrNum)%SetPt, MinSetPoint)
  OAPretreatSetPtMgr(SetPtMgrNum)%SetPt = MIN(OAPretreatSetPtMgr(SetPtMgrNum)%SetPt, MaxSetPoint)
END IF

RETURN

END SUBROUTINE CalcOAPretreatSetPoint

SUBROUTINE CalcWarmestSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate the "warmest" supply air setpoint temperature that will satisfy the cooling
          ! requirements of all the zones served by a central air system.

          ! METHODOLOGY EMPLOYED:
          ! Zone sensible heat balance

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand
  USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum         ! number of the current setpoint manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)      :: ZoneLoad         ! required zone load [W]
  REAL(r64)      :: ZoneMassFlowMax  ! zone inlet maximum mass flow rate [kg/s]
  REAL(r64)      :: CpAir            ! inlet air specific heat [J/kg-C]
  INTEGER        :: AirLoopNum       ! the index of the air loop served by this setpoint manager
  REAL(r64)      :: TotCoolLoad      ! sum of the zone cooling loads for this air loop [W]
  INTEGER        :: ZonesCooledIndex ! DO loop index for zones cooled by the air loop
  INTEGER        :: CtrlZoneNum      ! the controlled zone index
  INTEGER        :: ZoneInletNode    ! the zone inlet node number
  REAL(r64)      :: ZoneTemp         ! zone temperature [C]
  REAL(r64)      :: ZoneSetPointTemp ! zone supply air temperature [C]
  REAL(r64)      :: SetPointTemp     ! the system setpoint temperature [C]
  INTEGER        :: ZoneNode         ! the zone node number of the current zone
  INTEGER        :: ZoneNum          ! the actual zone number

  AirLoopNum =  WarmestSetPtMgr(SetPtMgrNum)%AirLoopNum
  TotCoolLoad = 0.0d0
  SetPointTemp = WarmestSetPtMgr(SetPtMgrNum)%MaxSetTemp

  DO ZonesCooledIndex=1,AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled
    CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledIndex)
    ZoneInletNode = AirToZoneNodeInfo(AirLoopNum)%CoolZoneInletNodes(ZonesCooledIndex)
    ZoneNode = ZoneEquipConfig(CtrlZoneNum)%ZoneNode
    ZoneNum = ZoneEquipConfig(CtrlZoneNum)%ActualZoneNum
    ZoneMassFlowMax = Node(ZoneInletNode)%MassFlowRateMax
    ZoneLoad = ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired
    ZoneTemp = Node(ZoneNode)%Temp
    ZoneSetPointTemp = WarmestSetPtMgr(SetPtMgrNum)%MaxSetTemp
    IF (ZoneLoad < 0.0d0) THEN
      TotCoolLoad = TotCoolLoad + ABS(ZoneLoad)
      CpAir = PsyCpAirFnWTdb(Node(ZoneInletNode)%HumRat,Node(ZoneInletNode)%Temp)
      IF (ZoneMassFlowMax > SmallMassFlow) THEN
        ZoneSetPointTemp = ZoneTemp + ZoneLoad/(CpAir*ZoneMassFlowMax)
      END IF
    END IF
    SetPointTemp = MIN(SetPointTemp,ZoneSetPointTemp)
  END DO

  SetPointTemp = MAX(WarmestSetPtMgr(SetPtMgrNum)%MinSetTemp,MIN(SetPointTemp,WarmestSetPtMgr(SetPtMgrNum)%MaxSetTemp))
  IF (TotCoolLoad < SmallLoad) THEN
    SetPointTemp = WarmestSetPtMgr(SetPtMgrNum)%MaxSetTemp
  END IF

  WarmestSetPtMgr(SetPtMgrNum)%SetPt = SetPointTemp

  RETURN

END SUBROUTINE CalcWarmestSetPoint

SUBROUTINE CalcColdestSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate the "coldest" supply air setpoint temperature that will satisfy the heating
          ! requirements of all the zones served by a central air system.

          ! METHODOLOGY EMPLOYED:
          ! Zone sensible heat balance

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand
  USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum         ! number of the current setpoint manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)      :: ZoneLoad         ! required zone load [W]
  REAL(r64)      :: ZoneMassFlowMax  ! zone inlet maximum mass flow rate [kg/s]
  REAL(r64)      :: CpAir            ! inlet air specific heat [J/kg-C]
  INTEGER        :: AirLoopNum       ! the index of the air loop served by this setpoint manager
  REAL(r64)      :: TotHeatLoad      ! sum of the zone heating loads for this air loop [W]
  INTEGER        :: ZonesHeatedIndex ! DO loop index for zones heated by the air loop
  INTEGER        :: CtrlZoneNum      ! the controlled zone index
  INTEGER        :: ZoneInletNode    ! the zone inlet node number
  REAL(r64)      :: ZoneTemp         ! zone temperature [C]
  REAL(r64)      :: ZoneSetPointTemp ! zone supply air temperature [C]
  REAL(r64)      :: SetPointTemp     ! the system setpoint temperature [C]
  INTEGER        :: ZoneNode         ! the zone node number of the current zone
  INTEGER        :: ZoneNum          ! the actual zone number

  AirLoopNum =  ColdestSetPtMgr(SetPtMgrNum)%AirLoopNum
  TotHeatLoad = 0.0d0
  SetPointTemp = ColdestSetPtMgr(SetPtMgrNum)%MinSetTemp

  DO ZonesHeatedIndex=1,AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated
    CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%HeatCtrlZoneNums(ZonesHeatedIndex)
    ZoneInletNode = AirToZoneNodeInfo(AirLoopNum)%HeatZoneInletNodes(ZonesHeatedIndex)
    ZoneNode = ZoneEquipConfig(CtrlZoneNum)%ZoneNode
    ZoneNum = ZoneEquipConfig(CtrlZoneNum)%ActualZoneNum
    ZoneMassFlowMax = Node(ZoneInletNode)%MassFlowRateMax
    ZoneLoad = ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired
    ZoneTemp = Node(ZoneNode)%Temp
    ZoneSetPointTemp = ColdestSetPtMgr(SetPtMgrNum)%MinSetTemp
    IF (ZoneLoad > 0.0d0) THEN
      TotHeatLoad = TotHeatLoad + ZoneLoad
      CpAir = PsyCpAirFnWTdb(Node(ZoneInletNode)%HumRat,Node(ZoneInletNode)%Temp)
      IF (ZoneMassFlowMax > SmallMassFlow) THEN
        ZoneSetPointTemp = ZoneTemp + ZoneLoad/(CpAir*ZoneMassFlowMax)
      END IF
    END IF
    SetPointTemp = MAX(SetPointTemp,ZoneSetPointTemp)
  END DO

  SetPointTemp = MIN(ColdestSetPtMgr(SetPtMgrNum)%MaxSetTemp,MAX(SetPointTemp,ColdestSetPtMgr(SetPtMgrNum)%MinSetTemp))
  IF (TotHeatLoad < SmallLoad) THEN
    SetPointTemp = ColdestSetPtMgr(SetPtMgrNum)%MinSetTemp
  END IF

  ColdestSetPtMgr(SetPtMgrNum)%SetPt = SetPointTemp

  RETURN

END SUBROUTINE CalcColdestSetPoint

SUBROUTINE CalcWarmestSetPointTempFlow(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2002
          !       MODIFIED       Haves, Oct 2004
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate the "warmest" supply air setpoint temperature that will satisfy the cooling
          ! requirements of all the zones served by a central air system.

          ! METHODOLOGY EMPLOYED:
          ! Zone sensible heat balance

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand
  USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad
  USE DataAirLoop, ONLY: AirLoopControlInfo, AirLoopFlow

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum         ! number of the current setpoint manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)      :: ZoneLoad         ! required zone load [W]
  REAL(r64)      :: ZoneMassFlowMax  ! zone inlet maximum mass flow rate [kg/s]
  REAL(r64)      :: CpAir            ! inlet air specific heat [J/kg-C]
  INTEGER        :: AirLoopNum       ! the index of the air loop served by this setpoint manager
  REAL(r64)      :: TotCoolLoad      ! sum of the zone cooling loads for this air loop [W]
  INTEGER        :: ZonesCooledIndex ! DO loop index for zones cooled by the air loop
  INTEGER        :: CtrlZoneNum      ! the controlled zone index
  INTEGER        :: ZoneInletNode    ! the zone inlet node number
  REAL(r64)      :: ZoneTemp         ! zone temperature [C]
  REAL(r64)      :: ZoneSetPointTemp ! zone supply air temperature [C]
  REAL(r64)      :: SetPointTemp     ! the system setpoint temperature [C]
  INTEGER        :: ZoneNode         ! the zone node number of the current zone
  INTEGER        :: ZoneNum          ! the actual zone number
  REAL(r64)      :: MinFracFlow
  REAL(r64)      :: ZoneFracFlow
  REAL(r64)      :: FracFlow
  REAL(r64)      :: MaxSetPointTemp
  REAL(r64)      :: MinSetPointTemp
  INTEGER        :: CritZoneNumTemp
  INTEGER        :: CritZoneNumFlow
  INTEGER        :: ControlStrategy

  IF (.not. WarmestSetPtMgrTempFlow(SetPtMgrNum)%SimReady) RETURN
  AirLoopNum =  WarmestSetPtMgrTempFlow(SetPtMgrNum)%AirLoopNum
  TotCoolLoad = 0.0d0
  MaxSetPointTemp = WarmestSetPtMgrTempFlow(SetPtMgrNum)%MaxSetTemp
  SetPointTemp = MaxSetPointTemp
  MinSetPointTemp = WarmestSetPtMgrTempFlow(SetPtMgrNum)%MinSetTemp
  MinFracFlow = WarmestSetPtMgrTempFlow(SetPtMgrNum)%MinTurndown
  FracFlow = MinFracFlow
  CritZoneNumTemp = 0
  CritZoneNumFlow = 0
  ControlStrategy = WarmestSetPtMgrTempFlow(SetPtMgrNum)%Strategy

  DO ZonesCooledIndex=1,AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled
    CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledIndex)
    ZoneInletNode = AirToZoneNodeInfo(AirLoopNum)%CoolZoneInletNodes(ZonesCooledIndex)
    ZoneNode = ZoneEquipConfig(CtrlZoneNum)%ZoneNode
    ZoneNum = ZoneEquipConfig(CtrlZoneNum)%ActualZoneNum
    ZoneMassFlowMax = Node(ZoneInletNode)%MassFlowRateMax
    ZoneLoad = ZoneSysEnergyDemand(ZoneNum)%TotalOutputRequired
    ZoneTemp = Node(ZoneNode)%Temp
    ZoneSetPointTemp = MaxSetPointTemp
    ZoneFracFlow = MinFracFlow
    IF (ZoneLoad < 0.0d0) THEN
      TotCoolLoad = TotCoolLoad + ABS(ZoneLoad)
      CpAir = PsyCpAirFnWTdb(Node(ZoneInletNode)%HumRat,Node(ZoneInletNode)%Temp)
      IF (ZoneMassFlowMax > SmallMassFlow) THEN
        IF (ControlStrategy == TempFirst) THEN
! First find supply air temperature required to meet the load at minimum flow. If this is
! below the minimum supply air temperature, calculate the fractional flow rate required to meet the
! load at the minimum supply air temperature.
          ZoneSetPointTemp = ZoneTemp + ZoneLoad/(CpAir*ZoneMassFlowMax*MinFracFlow)
          IF (ZoneSetPointTemp < MinSetPointTemp) THEN
            ZoneFracFlow = (ZoneLoad/(CpAir*(MinSetPointTemp-ZoneTemp)) ) / ZoneMassFlowMax
          ELSE
            ZoneFracFlow = MinFracFlow
          END IF
        ELSE ! ControlStrategy = FlowFirst
! First find supply air flow rate required to meet the load at maximum supply air temperature. If this
! is above the maximum supply air flow rate, calculate the supply air temperature required to meet the
! load at the maximum flow.
          ZoneFracFlow = (ZoneLoad/(CpAir*(MaxSetPointTemp-ZoneTemp)) ) / ZoneMassFlowMax
          IF (ZoneFracFlow > 1.0d0 .OR. ZoneFracFlow < 0.0d0) THEN
            ZoneSetPointTemp = ZoneTemp + ZoneLoad/(CpAir*ZoneMassFlowMax)
          ELSE
            ZoneSetPointTemp = MaxSetPointTemp
          END IF
        END IF
      END IF
    END IF
    IF (ZoneSetPointTemp < SetPointTemp) THEN
      SetPointTemp = ZoneSetPointTemp
      CritZoneNumTemp = ZoneNum
    END IF
    IF (ZoneFracFlow > FracFlow) THEN
      FracFlow = ZoneFracFlow
      CritZoneNumFlow = ZoneNum
    END IF
  END DO

  SetPointTemp = MAX(MinSetPointTemp,MIN(SetPointTemp,MaxSetPointTemp))
  FracFlow = MAX(MinFracFlow,MIN(FracFlow,1.0d0))
  IF (TotCoolLoad < SmallLoad) THEN
    SetPointTemp = MaxSetPointTemp
    FracFlow = MinFracFlow
  END IF

  WarmestSetPtMgrTempFlow(SetPtMgrNum)%SetPt = SetPointTemp
  WarmestSetPtMgrTempFlow(SetPtMgrNum)%Turndown = FracFlow
  IF (ControlStrategy == TempFirst) THEN
    IF (CritZoneNumFlow /= 0) THEN
      WarmestSetPtMgrTempFlow(SetPtMgrNum)%CritZoneNum = CritZoneNumFlow
    ELSE
      WarmestSetPtMgrTempFlow(SetPtMgrNum)%CritZoneNum = CritZoneNumTemp
    END IF
  ELSE ! ControlStrategy = FlowFirst
  IF (CritZoneNumTemp /= 0) THEN
      WarmestSetPtMgrTempFlow(SetPtMgrNum)%CritZoneNum = CritZoneNumTemp
    ELSE
      WarmestSetPtMgrTempFlow(SetPtMgrNum)%CritZoneNum = CritZoneNumFlow
    END IF
  END IF

  RETURN

END SUBROUTINE CalcWarmestSetPointTempFlow

SUBROUTINE CalcRABFlowSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   July 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Given the desired setpoint temperature, calulate the flow rate through the
          ! return asir branch that will deliver the desired temperature at the loop outlet
          ! node.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum         ! number of the current setpoint manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS


          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: MixerRABInNode     ! Mixer RAB inlet node number
  INTEGER :: MixerSupInNode     ! Mixer supply inlet node number
  INTEGER :: MixerOutNode       ! Mixer outlet node number
  INTEGER :: LoopOutNode        ! loop outlet node number
  REAL(r64)    :: TempSetPt          ! the setpoint temperature (from schedule) [C]
  REAL(r64)    :: TempSetPtMod       ! the setpoint temperature modified for fan heat gain [C]
  REAL(r64)    :: SupFlow            ! supply flow rate before mixing [kg/s]
  REAL(r64)    :: RABFlow            ! Return Air Bypass flow rate [kg/s]
  REAL(r64)    :: TotSupFlow         ! supply air flow after mixing [kg/s]
  REAL(r64)    :: TempSup            ! temperature of supply air before mixing [kg/s]
  REAL(r64)    :: TempRAB            ! temperature of return bypass air

  MixerRABInNode = RABFlowSetPtMgr(SetPtMgrNum)%RABMixInNode
  MixerSupInNode = RABFlowSetPtMgr(SetPtMgrNum)%SupMixInNode
  MixerOutNode = RABFlowSetPtMgr(SetPtMgrNum)%MixOutNode
  LoopOutNode = RABFlowSetPtMgr(SetPtMgrNum)%SysOutNode
  TempSetPt = GetCurrentScheduleValue(RABFlowSetPtMgr(SetPtMgrNum)%SchedPtr)
  TempSetPtMod = TempSetPt - (Node(LoopOutNode)%Temp - Node(MixerOutNode)%Temp)
  SupFlow = Node(MixerSupInNode)%MassFlowRate
  TempSup = Node(MixerSupInNode)%Temp
  TotSupFlow = Node(MixerOutNode)%MassFlowRate
  TempRAB = Node(MixerRABInNode)%Temp
  RABFlow = (TotSupFlow*TempSetPtMod - SupFlow*TempSup) / Max(TempRAB,1.0d0)
  RABFlow = Max(0.0d0,RABFlow)
  RABFlowSetPtMgr(SetPtMgrNum)%FlowSetPt = RABFlow

  RETURN

END SUBROUTINE CalcRABFlowSetPoint

SUBROUTINE CalcMultiZoneAverageHeatingSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket Nigusse, FSEC
          !       DATE WRITTEN   July 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the "Average" supply air setpoint temperature that will satisfy the heating
          ! requirements of multizones served by a central air system.

          ! METHODOLOGY EMPLOYED:
          ! Zone sensible (heating load) heat balance around the zones served by a central air system

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand
  USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum         ! number of the current setpoint manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)      :: ZoneLoad         ! zone load predicted to the setpoint [W]
  REAL(r64)      :: ZoneMassFlowRate ! zone inlet node actual mass flow rate lagged by system one time step[kg/s]
  REAL(r64)      :: CpAir            ! inlet air specific heat [J/kg-C]
  INTEGER        :: AirLoopNum       ! the index of the air loop served by this setpoint manager
  REAL(r64)      :: SumHeatLoad      ! sum of the zone's predicted heating loads for this air loop [W]
  REAL(r64)      :: SumProductMdotCpTZoneTot  ! sum of the product of zone inlet node actual mass flow rate,
                                              ! Cp of air at zone air node and zone air node temperature for
                                              ! all zones in the air loop [W]
  REAL(r64)      :: SumProductMdotCp     ! sum of the product of zone inlet node actual mass flow rate, and
                                         ! Cp of air at zone inlet node for all heated zones in the airloop [W/C]
  REAL(r64)      :: SumProductMdotCpTot  ! sum of the product of zone inlet node actual mass flow rate, and
                                         ! Cp of air at zone air node for all zones in the airloop [W/C]
  REAL(r64)      :: ZoneAverageTemp      ! multizone average zone air node temperature [C]
  INTEGER        :: ZonesHeatedIndex     ! DO loop index for zones cooled by the air loop
  INTEGER        :: CtrlZoneNum          ! the controlled zone index
  INTEGER        :: ZoneInletNode        ! the zone inlet node number
  REAL(r64)      :: ZoneTemp             ! zone air node temperature [C]
  REAL(r64)      :: SetPointTemp         ! the system setpoint temperature [C]
  INTEGER        :: ZoneNode             ! the zone node number of the current zone


  SumHeatLoad = 0.0d0
  ZoneAverageTemp = 0.0d0
  SumProductMdotCp = 0.0d0
  SumProductMdotCpTot = 0.0d0
  SumProductMdotCpTzoneTot = 0.0d0
  AirLoopNum = MZAverageHeatingSetPtMgr(SetPtMgrNum)%AirLoopNum
  SetPointTemp = MZAverageHeatingSetPtMgr(SetPtMgrNum)%MinSetTemp

  DO ZonesHeatedIndex=1,AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled
  ! DO ZonesHeatedIndex=1,AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated
  ! Using AirToZoneNodeInfo(AirLoopNum)%Cool* structure variables since they include heating and cooling.

  ! The data for number of zones heated is included in the data structure of the variable
  ! "AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled" for all systems.  The data structure
  ! "AirToZoneNodeInfo(AirLoopNum)%NumZonesHeated" applies to Dual Duct System only and
  ! if used will limit the application of this setpoint manager to other systems.  Thus,
  ! the "AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled" data is used instead.

     CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesHeatedIndex)
     ZoneInletNode = AirToZoneNodeInfo(AirLoopNum)%CoolZoneInletNodes(ZonesHeatedIndex)
     ZoneNode = ZoneEquipConfig(CtrlZoneNum)%ZoneNode
     ZoneMassFlowRate = Node(ZoneInletNode)%MassFlowRate
     ZoneLoad = ZoneSysEnergyDemand(CtrlZoneNum)%TotalOutputRequired
     ZoneTemp = Node(ZoneNode)%Temp
     CpAir = PsyCpAirFnWTdb(Node(ZoneNode)%HumRat,ZoneTemp)
     SumProductMdotCpTot = SumProductMdotCpTot + ZoneMassFlowRate * CpAir
     SumProductMdotCpTzoneTot = SumProductMdotCpTzoneTot + ZoneMassFlowRate * CpAir * ZoneTemp
     IF (ZoneLoad > 0.0d0) THEN
         CpAir = PsyCpAirFnWTdb(Node(ZoneInletNode)%HumRat,Node(ZoneInletNode)%Temp)
         SumHeatLoad = SumHeatLoad + ZoneLoad
         SumProductMdotCp = SumProductMdotCp + ZoneMassFlowRate * CpAir
     END IF
  END DO
  IF (SumProductMdotCpTot > 0.0d0) ZoneAverageTemp = SumProductMdotCpTzoneTot / SumProductMdotCpTot
  IF (SumProductMdotCp > 0.0d0) SetPointTemp = ZoneAverageTemp + SumHeatLoad / SumProductMdotCp

  SetPointTemp = MIN(MZAverageHeatingSetPtMgr(SetPtMgrNum)%MaxSetTemp,MAX(SetPointTemp,  &
                     MZAverageHeatingSetPtMgr(SetPtMgrNum)%MinSetTemp))
  IF (SumHeatLoad < SmallLoad) THEN
    SetPointTemp = MZAverageHeatingSetPtMgr(SetPtMgrNum)%MinSetTemp
  END IF
  MZAverageHeatingSetPtMgr(SetPtMgrNum)%SetPt = SetPointTemp
  RETURN

END SUBROUTINE CalcMultiZoneAverageHeatingSetPoint

SUBROUTINE CalcMultiZoneAverageCoolingSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket Nigusse, FSEC
          !       DATE WRITTEN   July 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate the "Average" supply air setpoint temperature that will satisfy the cooling
          ! requirements of all the zones served by a central air system.

          ! METHODOLOGY EMPLOYED:
          ! Zone sensible (cooling load) heat balance around the zones served by a central air system

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE DataZoneEnergyDemands, ONLY: ZoneSysEnergyDemand
  USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum         ! number of the current setpoint manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)      :: ZoneLoad         ! zone load predicted to the setpoint [W]
  REAL(r64)      :: ZoneMassFlowRate ! zone inlet node actual mass flow rate lagged by system one time step[kg/s]
  REAL(r64)      :: CpAir            ! inlet air specific heat [J/kg-C]
  INTEGER        :: AirLoopNum       ! the index of the air loop served by this setpoint manager
  REAL(r64)      :: SumCoolLoad      ! sum of the zone cooling loads for this air loop [W]
  REAL(r64)      :: SumProductMdotCpTZoneTot ! sum of the product of zone inlet node actual mass flow rate,
                                             ! Cp of air at zone air node and zone air node temperature for
                                             ! all zones in the air loop [W]
  REAL(r64)      :: SumProductMdotCp         ! sum of the product of zone inlet node actual mass flow rate, and
                                             ! Cp of air at zone inlet node for cooled zones in the airloop [W/C]
  REAL(r64)      :: SumProductMdotCpTot      ! sum of the product of zone inlet node actual mass flow rate, and
                                             ! Cp of air at zone air node for all zones in the airloop [W/C]
  REAL(r64)      :: ZoneAverageTemp          ! multizone average zone Air node temperature [C]
  INTEGER        :: ZonesCooledIndex         ! DO loop index for zones cooled by the air loop
  INTEGER        :: CtrlZoneNum              ! the controlled zone index
  INTEGER        :: ZoneInletNode            ! the zone inlet node number
  REAL(r64)      :: ZoneTemp                 ! zone air node temperature [C]
  REAL(r64)      :: SetPointTemp             ! the system setpoint temperature [C]
  INTEGER        :: ZoneNode                 ! the zone node number of the current zone

  SumCoolLoad = 0.0d0
  ZoneAverageTemp = 0.0d0
  SumProductMdotCp = 0.0d0
  SumProductMdotCpTot = 0.0d0
  SumProductMdotCpTzoneTot = 0.0d0
  AirLoopNum = MZAverageCoolingSetPtMgr(SetPtMgrNum)%AirLoopNum
  SetPointTemp = MZAverageCoolingSetPtMgr(SetPtMgrNum)%MaxSetTemp

  DO ZonesCooledIndex=1,AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled
     CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledIndex)
     ZoneInletNode = AirToZoneNodeInfo(AirLoopNum)%CoolZoneInletNodes(ZonesCooledIndex)
     ZoneNode = ZoneEquipConfig(CtrlZoneNum)%ZoneNode
     ZoneMassFlowRate = Node(ZoneInletNode)%MassFlowRate
     ZoneLoad = ZoneSysEnergyDemand(CtrlZoneNum)%TotalOutputRequired
     ZoneTemp = Node(ZoneNode)%Temp
     CpAir = PsyCpAirFnWTdb(Node(ZoneNode)%HumRat,ZoneTemp)
     SumProductMdotCpTot = SumProductMdotCpTot + ZoneMassFlowRate * CpAir
     SumProductMdotCpTzoneTot = SumProductMdotCpTzoneTot + ZoneMassFlowRate * CpAir * ZoneTemp
     IF (ZoneLoad < 0.0d0) THEN
         CpAir = PsyCpAirFnWTdb(Node(ZoneInletNode)%HumRat,Node(ZoneInletNode)%Temp)
         SumCoolLoad = SumCoolLoad + ZoneLoad
         SumProductMdotCp = SumProductMdotCp + ZoneMassFlowRate * CpAir
     END IF
  END DO
  IF (SumProductMdotCpTot > 0.0d0) ZoneAverageTemp = SumProductMdotCpTzoneTot / SumProductMdotCpTot
  IF (SumProductMdotCp > 0.0d0) SetPointTemp = ZoneAverageTemp + SumCoolLoad / SumProductMdotCp

  SetPointTemp = MAX(MZAverageCoolingSetPtMgr(SetPtMgrNum)%MinSetTemp,MIN(SetPointTemp,  &
                     MZAverageCoolingSetPtMgr(SetPtMgrNum)%MaxSetTemp))

  IF (ABS(SumCoolLoad) < SmallLoad) THEN
    SetPointTemp = MZAverageCoolingSetPtMgr(SetPtMgrNum)%MaxSetTemp
  END IF

  MZAverageCoolingSetPtMgr(SetPtMgrNum)%SetPt = SetPointTemp
  RETURN

END SUBROUTINE CalcMultiZoneAverageCoolingSetPoint

SUBROUTINE CalcMultiZoneAverageMinHumSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket Nigusse, FSEC
          !       DATE WRITTEN   July 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate the "Average" supply air minimum humidity setpoint that will satisfy the minimum
          ! humidity ratio requirements of multiple zones served by a central air system.

          ! METHODOLOGY EMPLOYED:
          ! Zone latent load balance around the zones served by a central air system

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE DataZoneEnergyDemands, ONLY: ZoneSysMoistureDemand
  USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum        ! number of the current setpoint manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)      :: MoistureLoad         ! zone's moisture load predicted to the setpoint [kgH20/s]
  REAL(r64)      :: ZoneMassFlowRate     ! zone inlet node actual mass flow rate lagged by system one time step[kg/s]
  INTEGER        :: AirLoopNum           ! the index of the air loop served by this setpoint manager
  REAL(r64)      :: SumMoistureLoad      ! sum of the zone moisture loads for this air loop [W]
  REAL(r64)      :: SumMdot              ! sum of the actual mass flow rate for controlled zones in the air loop [kg/s]
  REAL(r64)      :: SumMdotTot           ! sum of the actual mass flow rate for this air loop [kg/s]
  REAL(r64)      :: SumProductMdotHumTot ! sum of product of actual mass flow rate at the zone inlet node,
                                         ! and humidity ratio at zones air node for all zones in the airloop [kgH20/s]
  REAL(r64)      :: AverageZoneHum       ! multizone average zone air node humidity ratio of all zones in the air loop [kg/kg]
  INTEGER        :: ZonesCooledIndex     ! DO loop index for zones cooled by the air loop
  INTEGER        :: CtrlZoneNum          ! the controlled zone index
  INTEGER        :: ZoneInletNode        ! the zone inlet node number
  REAL(r64)      :: ZoneHum              ! zone air node humidity ratio [kg/kg]
  REAL(r64)      :: SetPointHum          ! system setpoint humidity ratio [kg/kg]
  INTEGER        :: ZoneNode             ! the zone node number of the current zone

  SumMdot = 0.0d0
  SumMdotTot = 0.0d0
  AverageZoneHum = 0.0d0
  SumMoistureLoad = 0.0d0
  SumProductMdotHumTot = 0.0d0
  AirLoopNum = MZAverageMinHumSetPtMgr(SetPtMgrNum)%AirLoopNum
  SetPointHum = MZAverageMinHumSetPtMgr(SetPtMgrNum)%MinSetHum

  DO ZonesCooledIndex=1,AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled
     CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledIndex)
     ZoneInletNode = AirToZoneNodeInfo(AirLoopNum)%CoolZoneInletNodes(ZonesCooledIndex)
     ZoneNode = ZoneEquipConfig(CtrlZoneNum)%ZoneNode
     ZoneMassFlowRate = Node(ZoneInletNode)%MassFlowRate
     MoistureLoad = ZoneSysMoistureDemand(CtrlZoneNum)%OutputRequiredToHumidifyingSP
     ZoneHum = Node(ZoneNode)%HumRat
     SumMdotTot =  SumMdotTot + ZoneMassFlowRate
     SumProductMdotHumTot = SumProductMdotHumTot + ZoneMassFlowRate * ZoneHum
     ! For humidification the mositure load is positive
     IF (MoistureLoad > 0.0d0) THEN
        SumMdot =  SumMdot + ZoneMassFlowRate
        SumMoistureLoad = SumMoistureLoad + MoistureLoad
     ENDIF
  END DO
  IF (SumMdotTot > SmallMassFlow) AverageZoneHum = SumProductMdotHumTot / SumMdotTot
  IF (SumMdot > SmallMassFlow) SetPointHum = MAX(0.0d0, AverageZoneHum + SumMoistureLoad / SumMdot)

  SetPointHum = MIN(MZAverageMinHumSetPtMgr(SetPtMgrNum)%MaxSetHum,MAX(SetPointHum,  &
                    MZAverageMinHumSetPtMgr(SetPtMgrNum)%MinSetHum))

  MZAverageMinHumSetPtMgr(SetPtMgrNum)%SetPt = SetPointHum

  RETURN

END SUBROUTINE CalcMultiZoneAverageMinHumSetPoint

SUBROUTINE CalcMultiZoneAverageMaxHumSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket Nigusse, FSEC
          !       DATE WRITTEN   July 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate the "Average" supply air maximum humidity setpoint that will satisfy the maximum
          ! himudity ratio requirements of multiple zones served by a central air system.

          ! METHODOLOGY EMPLOYED:
          ! Zone latent load balance around the zones served by a central air system

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE DataZoneEnergyDemands, ONLY: ZoneSysMoistureDemand
  USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum       ! number of the current setpoint manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)      :: MoistureLoad         ! zone's moisture load predicted to the setpoint [kgH20/s]
  REAL(r64)      :: ZoneMassFlowRate     ! zone inlet node actual mass flow rate lagged by system one time step[kg/s]
  INTEGER        :: AirLoopNum           ! the index of the air loop served by this setpoint manager
  REAL(r64)      :: SumMoistureLoad      ! sum of the zone moisture loads for this air loop [W]
  REAL(r64)      :: SumMdot              ! sum of the actual mass flow rate for controlled zones in the air loop [kg/s]
  REAL(r64)      :: SumMdotTot           ! sum of the actual mass flow rate for this air loop [kg/s]
  REAL(r64)      :: SumProductMdotHumTot ! sum of product of actual mass flow rate at the zone inlet node,
                                         ! and humidity ratio at zones air node for all zones in the airloop [kgH20/s]
  REAL(r64)      :: AverageZoneHum       ! multizone average zone air node humidity ratio of all zones in the air loop [kg/kg]
  INTEGER        :: ZonesCooledIndex     ! DO loop index for zones cooled by the air loop
  INTEGER        :: CtrlZoneNum          ! the controlled zone index
  INTEGER        :: ZoneInletNode        ! the zone inlet node number
  REAL(r64)      :: ZoneHum              ! zone air node humidity ratio [kg/kg]
!  REAL(r64)      :: AverageSetPointHum   ! Supply air humidity ratio [kg/kg]
  REAL(r64)      :: SetPointHum          ! system setpoint humidity ratio [kg/kg]
  INTEGER        :: ZoneNode             ! the zone node number of the current zone

  SumMdot = 0.0d0
  SumMdotTot = 0.0d0
  AverageZoneHum = 0.0d0
  SumMoistureLoad = 0.0d0
  SumProductMdotHumTot = 0.0d0
  AirLoopNum = MZAverageMaxHumSetPtMgr(SetPtMgrNum)%AirLoopNum
  SetPointHum = MZAverageMaxHumSetPtMgr(SetPtMgrNum)%MaxSetHum

  DO ZonesCooledIndex=1,AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled
     CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledIndex)
     ZoneInletNode = AirToZoneNodeInfo(AirLoopNum)%CoolZoneInletNodes(ZonesCooledIndex)
     ZoneNode = ZoneEquipConfig(CtrlZoneNum)%ZoneNode
     ZoneMassFlowRate = Node(ZoneInletNode)%MassFlowRate
     MoistureLoad = ZoneSysMoistureDemand(CtrlZoneNum)%OutputRequiredToDehumidifyingSP
     ZoneHum = Node(ZoneNode)%HumRat
     SumMdotTot =  SumMdotTot + ZoneMassFlowRate
     SumProductMdotHumTot = SumProductMdotHumTot + ZoneMassFlowRate * ZoneHum
     ! For dehumidification the mositure load is negative
     IF (MoistureLoad < 0.0d0) THEN
        SumMdot =  SumMdot + ZoneMassFlowRate
        SumMoistureLoad = SumMoistureLoad + MoistureLoad
     ENDIF
  END DO
  IF (SumMdotTot > SmallMassFlow) AverageZoneHum = SumProductMdotHumTot / SumMdotTot
  IF (SumMdot > SmallMassFlow) SetPointHum = MAX(0.0d0, AverageZoneHum + SumMoistureLoad / SumMdot)

  SetPointHum = MAX(MZAverageMaxHumSetPtMgr(SetPtMgrNum)%MinSetHum,MIN(SetPointHum,  &
                MZAverageMaxHumSetPtMgr(SetPtMgrNum)%MaxSetHum))
  MZAverageMaxHumSetPtMgr(SetPtMgrNum)%SetPt = SetPointHum
  RETURN

END SUBROUTINE CalcMultiZoneAverageMaxHumSetPoint

SUBROUTINE CalcMultiZoneMinHumSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket Nigusse, FSEC/UCF
          !       DATE WRITTEN   Aug 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the minimum supply air humidity ratio based on humidification requirements of
          ! a controlled zone with critical humidification need (i.e., a zone with the highest
          ! humidity ratio setpoint) in an air loop served by a central air-conditioner.
          !
          !
          ! METHODOLOGY EMPLOYED:
          ! Uses moisture mass balance to calculate the humidity ratio setpoint. The algorithm loops
          ! over all the zones that a central air system can humidify and calculates the setpoint based
          ! on a zone with the highest humidity ratio setpoint requirement:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE DataZoneEnergyDemands, ONLY: ZoneSysMoistureDemand
  USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum        ! number of the current setpoint manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: AirLoopNum                 ! the index of the air loop served by this setpoint manager
  INTEGER     :: ZonesCooledIndex           ! DO loop index for zones cooled by the air loop
  INTEGER     :: CtrlZoneNum                ! the controlled zone index
  INTEGER     :: ZoneInletNode              ! the zone inlet node number
  INTEGER     :: ZoneNode                   ! the zone node number of the current zone
  REAL(r64)   :: ZoneHum                    ! zone air node humidity ratio [kg/kg]
  REAL(r64)   :: SetPointHum                ! system setpoint humidity ratio [kg/kg]
  REAL(r64)   :: ZoneSetPointHum            ! Zone setpoint humidity ratio [kg/kg]
  REAL(r64)   :: MoistureLoad               ! zone's moisture load predicted to the setpoint [kgH20/s]
  REAL(r64)   :: ZoneMassFlowRate           ! zone inlet node actual supply air mass flow rate [kg/s]
  REAL(r64)   :: SumMoistureLoad = 0.0d0    ! sum of the zone moisture loads for this air loop [W]
  REAL(r64)   :: SmallMoistureLoad = 0.0001d0 ! small moisture load [kgH2O/s]

  AirLoopNum = MZMinHumSetPtMgr(SetPtMgrNum)%AirLoopNum
  SetPointHum = MZMinHumSetPtMgr(SetPtMgrNum)%MinSetHum

  DO ZonesCooledIndex=1,AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled
     CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledIndex)
     ZoneInletNode = AirToZoneNodeInfo(AirLoopNum)%CoolZoneInletNodes(ZonesCooledIndex)
     ZoneNode = ZoneEquipConfig(CtrlZoneNum)%ZoneNode
     ZoneMassFlowRate = Node(ZoneInletNode)%MassFlowRate
     MoistureLoad = ZoneSysMoistureDemand(CtrlZoneNum)%OutputRequiredToHumidifyingSP
     ZoneHum = Node(ZoneNode)%HumRat
     ZoneSetPointHum = MZMinHumSetPtMgr(SetPtMgrNum)%MinSetHum
     ! For humidification the mositure load is positive
     IF (MoistureLoad > 0.0d0) THEN
        SumMoistureLoad = SumMoistureLoad + MoistureLoad
        IF (ZoneMassFlowRate > SmallMassFlow) THEN
            ZoneSetPointHum = MAX(0.0d0, ZoneHum + MoistureLoad/ZoneMassFlowRate)
        ENDIF
     ENDIF
     SetPointHum = MAX(SetPointHum, ZoneSetPointHum)
  END DO
  SetPointHum = MIN(MZMinHumSetPtMgr(SetPtMgrNum)%MaxSetHum,MAX(SetPointHum,  &
                    MZMinHumSetPtMgr(SetPtMgrNum)%MinSetHum))
  IF (SumMoistureLoad < SmallMoistureLoad) THEN
    SetPointHum = MZMinHumSetPtMgr(SetPtMgrNum)%MinSetHum
  END IF
  MZMinHumSetPtMgr(SetPtMgrNum)%SetPt = SetPointHum

  RETURN
END SUBROUTINE CalcMultiZoneMinHumSetPoint

SUBROUTINE CalcMultiZoneMaxHumSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Bereket Nigusse, FSEC/UCF
          !       DATE WRITTEN   August 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the maximum supply air humidity ratio based on dehumidification requirements of
          ! a controlled zone with critical dehumidification need (i.e., a zone with the lowest
          ! humidity ratio setpoint) in an air loop served by a central air-conditioner.

          ! METHODOLOGY EMPLOYED:
          ! Uses moisture mass balance to calculate the humidity ratio setpoint. The algorithm loops
          ! over all the zones that a central air system can dehumidify and calculates the setpoint
          ! based on a zone with the lowest humidity ratio setpoint requirement:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataZoneEquipment, ONLY: ZoneEquipConfig
  USE DataZoneEnergyDemands, ONLY: ZoneSysMoistureDemand
  USE DataHVACGlobals, ONLY: SmallMassFlow, SmallLoad

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum       ! number of the current setpoint manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: AirLoopNum                 ! the index of the air loop served by this setpoint manager
  INTEGER     :: ZonesCooledIndex           ! DO loop index for zones cooled by the air loop
  INTEGER     :: CtrlZoneNum                ! the controlled zone index
  INTEGER     :: ZoneInletNode              ! the zone inlet node number
  INTEGER     :: ZoneNode                   ! the zone node number of the current zone
  REAL(r64)   :: ZoneHum                    ! zone air node humidity ratio [kg/kg]
  REAL(r64)   :: SetPointHum                ! system setpoint humidity ratio [kg/kg]
  REAL(r64)   :: ZoneSetPointHum            ! Zone setpoint humidity ratio [kg/kg]
  REAL(r64)   :: MoistureLoad               ! zone's moisture load predicted to the setpoint [kgH20/s]
  REAL(r64)   :: ZoneMassFlowRate           ! zone inlet node actual supply air mass flow rate [kg/s]
  REAL(r64)   :: SumMoistureLoad = 0.0d0    ! sum of the zone moisture loads for this air loop [W]
  REAL(r64)   :: SmallMoistureLoad = 0.00001d0 ! small moisture load [kgH2O/s]

  AirLoopNum = MZMaxHumSetPtMgr(SetPtMgrNum)%AirLoopNum
  SetPointHum = MZMaxHumSetPtMgr(SetPtMgrNum)%MaxSetHum

  DO ZonesCooledIndex=1,AirToZoneNodeInfo(AirLoopNum)%NumZonesCooled
     CtrlZoneNum = AirToZoneNodeInfo(AirLoopNum)%CoolCtrlZoneNums(ZonesCooledIndex)
     ZoneInletNode = AirToZoneNodeInfo(AirLoopNum)%CoolZoneInletNodes(ZonesCooledIndex)
     ZoneNode = ZoneEquipConfig(CtrlZoneNum)%ZoneNode
     ZoneMassFlowRate = Node(ZoneInletNode)%MassFlowRate
     MoistureLoad = ZoneSysMoistureDemand(CtrlZoneNum)%OutputRequiredToDehumidifyingSP
     ZoneHum = Node(ZoneNode)%HumRat
     ZoneSetPointHum = MZMaxHumSetPtMgr(SetPtMgrNum)%MaxSetHum

     ! For dehumidification the mositure load is negative
     IF (MoistureLoad < 0.0d0) THEN
        SumMoistureLoad = SumMoistureLoad + MoistureLoad
        IF (ZoneMassFlowRate > SmallMassFlow) THEN
            ZoneSetPointHum = MAX(0.0d0, ZoneHum + MoistureLoad/ZoneMassFlowRate)
        ENDIF
     ENDIF
     SetPointHum = MIN(SetPointHum, ZoneSetPointHum)
  END DO
  SetPointHum = MAX(MZMaxHumSetPtMgr(SetPtMgrNum)%MinSetHum,MIN(SetPointHum,  &
                MZMaxHumSetPtMgr(SetPtMgrNum)%MaxSetHum))

  IF (ABS(SumMoistureLoad) < SmallMoistureLoad) THEN
    SetPointHum = MZMaxHumSetPtMgr(SetPtMgrNum)%MaxSetHum
  END IF

  MZMaxHumSetPtMgr(SetPtMgrNum)%SetPt = SetPointHum
  RETURN

END SUBROUTINE CalcMultiZoneMaxHumSetPoint

SUBROUTINE CalcFollowOATempSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma, FSEC
          !       DATE WRITTEN   July 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set the setpoint based on outdoor air dry-bulb/wet-bulb temperature

          ! METHODOLOGY EMPLOYED:
          ! Based on reference temperature type specifed in the setpoint manager,
          ! the setpoint is calculated as OutWetBulbTemp(Or OutDryBulbTemp) + Offset.
          ! The sign convention is that a positive Offset will increase the resulting setpoint.
          ! Final value of the setpoint is limited by the Max and Min limit specified in the setpoint manager.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum         ! number of the current setpoint manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  INTEGER      :: CtrldNodeNum    ! index of the items in the controlled node list
  REAL(r64)    :: MinSetPoint     ! minimum allowed setpoint
  REAL(r64)    :: MaxSetPoint     ! maximum allowed setpoint

MaxSetPoint = FollowOATempSetPtMgr(SetPtMgrNum)%MaxSetTemp
MinSetPoint = FollowOATempSetPtMgr(SetPtMgrNum)%MinSetTemp

SELECT CASE(FollowOATempSetPtMgr(SetPtMgrNum)%RefTypeMode)
  CASE(iRefTempType_WetBulb)
    FollowOATempSetPtMgr(SetPtMgrNum)%SetPt = OutWetBulbTemp + FollowOATempSetPtMgr(SetPtMgrNum)%OffSet
  CASE(iRefTempType_DryBulb)
    FollowOATempSetPtMgr(SetPtMgrNum)%SetPt = OutDryBulbTemp + FollowOATempSetPtMgr(SetPtMgrNum)%OffSet
END SELECT

! Apply maximum and minimum values
FollowOATempSetPtMgr(SetPtMgrNum)%SetPt = MAX(FollowOATempSetPtMgr(SetPtMgrNum)%SetPt, MinSetPoint)
FollowOATempSetPtMgr(SetPtMgrNum)%SetPt = MIN(FollowOATempSetPtMgr(SetPtMgrNum)%SetPt, MaxSetPoint)

RETURN

END SUBROUTINE CalcFollowOATempSetPoint

SUBROUTINE CalcFollowSysNodeTempSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma, FSEC
          !       DATE WRITTEN   July 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set the setpoint based on current temperatures at a separate system node.

          ! METHODOLOGY EMPLOYED:
          ! The current value of the temperature at a reference node are obtained and used
          ! to generate setpoint on a second system node.  If the reference node is also designated
          ! to be an outdoor air (intake) node, then this setpoint manager can be used to follow
          ! outdoor air conditions that are adjusted for altitude.
          ! Also, based on reference temperature type specifed in the setpoint manager, the out door air wet-bulb
          ! or dry-bulb temperature at the reference node could be used.
          ! A temperature offset will be applied to the value obtained from the reference system node.
          ! If this value is zero, and the limits are met, then the resulting setpoint will be exactly the same
          ! as the reference system node temperature.  The sign convention is that a positive offset will increase
          ! the resulting setpoint.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum         ! number of the current setpoint manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: RefNode         ! setpoint reference node number
  REAL(r64)    :: RefNodeTemp     ! setpoint at reference node
  REAL(r64)    :: MinSetPoint     ! minimum allowed setpoint
  REAL(r64)    :: MaxSetPoint     ! maximum allowed setpoint

  RefNodeTemp  = 0.d0

  MaxSetPoint  = FollowSysNodeTempSetPtMgr(SetPtMgrNum)%MaxSetTemp
  MinSetPoint  = FollowSysNodeTempSetPtMgr(SetPtMgrNum)%MinSetTemp

  RefNode = FollowSysNodeTempSetPtMgr(SetPtMgrNum)%RefNodeNum

  SELECT CASE(FollowSysNodeTempSetPtMgr(SetPtMgrNum)%RefTypeMode)
    CASE(iRefTempType_WetBulb)
       IF (Allocated(MoreNodeInfo)) THEN
         RefNodeTemp = MoreNodeInfo(RefNode)%WetbulbTemp
       ENDIF
    CASE(iRefTempType_DryBulb)
       RefNodeTemp = Node(RefNode)%Temp
  END SELECT

  FollowSysNodeTempSetPtMgr(SetPtMgrNum)%SetPt = RefNodeTemp + FollowSysNodeTempSetPtMgr(SetPtMgrNum)%OffSet

  ! Apply maximum and minimum values
  FollowSysNodeTempSetPtMgr(SetPtMgrNum)%SetPt = MAX(FollowSysNodeTempSetPtMgr(SetPtMgrNum)%SetPt, MinSetPoint)
  FollowSysNodeTempSetPtMgr(SetPtMgrNum)%SetPt = MIN(FollowSysNodeTempSetPtMgr(SetPtMgrNum)%SetPt, MaxSetPoint)

RETURN

END SUBROUTINE CalcFollowSysNodeTempSetPoint

SUBROUTINE CalcGroundTempSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Chandan Sharma, FSEC
          !       DATE WRITTEN   July 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Set the setpoint based on current ground temperature

          ! METHODOLOGY EMPLOYED:
          ! Based on reference ground temperature object type specifed in the setpoint manager,
          ! the setpoint is calculated as GroundTemperature + Offset.
          ! The sign convention is that a positive Offset will increase the resulting setpoint.
          ! Final value of the setpoint is limited by the Max and Min limit specified in the setpoint manager.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataEnvironment, ONLY: GroundTemp_Deep, GroundTemp,GroundTemp_Surface, GroundTempFC

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum         ! number of the current setpoint manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  INTEGER      :: CtrldNodeNum    ! index of the items in the controlled node list
  REAL(r64)    :: MinSetPoint     ! minimum allowed setpoint
  REAL(r64)    :: MaxSetPoint     ! maximum allowed setpoint

MaxSetPoint  = GroundTempSetPtMgr(SetPtMgrNum)%MaxSetTemp
MinSetPoint  = GroundTempSetPtMgr(SetPtMgrNum)%MinSetTemp

SELECT CASE(GroundTempSetPtMgr(SetPtMgrNum)%RefTypeMode)
  CASE(iRefGroundTempObjType_BuildingSurface)
    GroundTempSetPtMgr(SetPtMgrNum)%SetPt = GroundTemp + GroundTempSetPtMgr(SetPtMgrNum)%OffSet
  CASE(iRefGroundTempObjType_Shallow)
    GroundTempSetPtMgr(SetPtMgrNum)%SetPt = GroundTemp_Surface + GroundTempSetPtMgr(SetPtMgrNum)%OffSet
  CASE(iRefGroundTempObjType_Deep)
    GroundTempSetPtMgr(SetPtMgrNum)%SetPt = GroundTemp_Deep + GroundTempSetPtMgr(SetPtMgrNum)%OffSet
  CASE(iRefGroundTempObjType_FCfactorMethod)
    GroundTempSetPtMgr(SetPtMgrNum)%SetPt = GroundTempFC + GroundTempSetPtMgr(SetPtMgrNum)%OffSet
END SELECT

! Apply maximum and minimum values
GroundTempSetPtMgr(SetPtMgrNum)%SetPt = MAX(GroundTempSetPtMgr(SetPtMgrNum)%SetPt, MinSetPoint)
GroundTempSetPtMgr(SetPtMgrNum)%SetPt = MIN(GroundTempSetPtMgr(SetPtMgrNum)%SetPt, MaxSetPoint)

RETURN

END SUBROUTINE CalcGroundTempSetPoint

SUBROUTINE CalcCondEntSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Atefe Makhmalbaf and Heejin Cho, PNNL
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate the optimal condenser water temperature set point for a chiller plant
          ! with one or more chillers.  The condenser water leaving the tower should be at this temperature
          ! for optimal operation of the chiller plant.

          ! METHODOLOGY EMPLOYED:
          ! using one curve to determine the optimum condenser entering water temperature for a given timestep
          ! and two other curves to place boundary conditions on the optimal setpoint value.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use DataEnvironment,  ONLY: CurMnDy, OutDryBulbTemp, OutWetBulbTemp
  USE CurveManager,     ONLY: CurveValue
  USE ScheduleManager,  ONLY: GetCurrentScheduleValue
  USE DataPlant
  USE DataLoopNode,     ONLY: Node

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum       !number of the current set point manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS

          ! DERIVED TYPE DEFINITIONS
          ! na
  ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  REAL(r64) :: NormDsnCondFlow          = 0.0d0           !Normalized design condenser flow for cooling towers, m3/s per watt
  REAL(r64) :: Twr_DesignWB             = 0.0d0           !The cooling tower design inlet air wet bulb temperature, C
  REAL(r64):: Dsn_EntCondTemp           = 0.0d0           !The chiller design entering condenser temp, C; e.g. 29.44C {85F}
  REAL(r64):: Dsn_CondMinThisChiller    = 0.0d0           !Design Minimum Condenser Entering for current chillers this timestep
  REAL(r64):: Dsn_MinCondSetpt          = 0.0d0           !The design minimum condenser water temp, C; e.g. 18.33C {65 F}
  REAL(r64):: Cur_MinLiftTD             = 0.0d0           !Minimum lift (TCond entering - Tevap leaving) TD this timestep
  REAL(r64):: temp_MinLiftTD            = 0.0d0           !Intermeidate variable associated with lift (TCond entering - Tevap leaving) TD
  REAL(r64):: Des_Load                  = 0.0d0           !array of chiller design loads
  REAL(r64):: Act_Load                  = 0.0d0           !array of chiller actual loads
  REAL(r64):: ALW                       = 0.0d0           !Actual load weighting of each chiller, W
  REAL(r64):: DLW                       = 0.0d0           !Design capacity of each chiller, W
  REAL(r64):: Design_Load_Sum           = 0.0d0           !the design load of the chillers, W
  REAL(r64) :: Actual_Load_Sum          = 0.0d0           !the actual load of the chillers, W
  REAL(r64) :: Weighted_Actual_Load_Sum = 0.0d0           !Intermediate weighted value of actual load on plant, W
  REAL(r64) :: Weighted_Design_Load_Sum = 0.0d0           !Intermediate weighted value of design load on plant, W
  REAL(r64) :: Weighted_Ratio           = 0.0d0           !Weighted part load ratio of chillers
  REAL(r64) :: Min_DesignWB             = 0.0d0           !Minimum design twr wet bulb allowed, C
  REAL(r64) :: Min_ActualWb             = 0.0d0           !Minimum actual oa wet bulb allowed, C
  REAL(r64) :: SetPoint                 = 0.0d0           !Condenser entering water temperature setpoint this timestep, C
  REAL(r64) :: Opt_CondEntTemp          = 0.0d0           !Optimized Condenser entering water temperature setpoint this timestep, C
  REAL(r64) :: CondWaterSetpoint        = 0.0d0           !Condenser entering water temperature setpoint this timestep, C
  REAL(r64) :: DesignClgCapacity_Watts  = 0.0d0
  REAL(r64) :: CurrentLoad_Watts        = 0.0d0
  REAL(r64) :: CondInletTemp            = 0.0d0           ! Condenser water inlet temperature (C)
  REAL(r64) :: TempDesCondIn            = 0.0d0           ! Design condenser inlet temp. C , or 25.d0
  REAL(r64) :: EvapOutletTemp           = 0.0d0           ! Evaporator water outlet temperature (C)
  REAL(r64) :: TempEvapOutDesign        = 0.0d0           ! design evaporator outlet temperature, water side
  REAL(r64) :: CurLoad                  = 0.0d0
  INTEGER   :: ChillerIndexPlantSide    = 0
  INTEGER   :: ChillerIndexDemandSide   = 0
  INTEGER   :: BranchIndexPlantSide     = 0
  INTEGER   :: BranchIndexDemandSide    = 0
  INTEGER   :: LoopIndexPlantSide       = 0
  INTEGER   :: LoopIndexDemandSide      = 0
  INTEGER   :: TypeNum                  = 0

  ! Get from tower design values
  NormDsnCondFlow = 5.38d-8   !m3/s per watt (typically 3 gpm/ton)=(Volume of condenser fluid)/(ton of heat rejection)

  ! Grab tower design inlet air wet bulb from setpoint manager
  Twr_DesignWB = CondEntSetPtMgr(SetPtMgrNum)%TowerDsnInletAirWetBulb

  ! Current timestep's condenser water entering setpoint
  CondWaterSetpoint = GetCurrentScheduleValue(CondEntSetPtMgr(SetPtMgrNum)%CondEntTempSchedPtr)
  LoopIndexPlantSide = CondEntSetPtMgr(SetPtMgrNum)%LoopIndexPlantSide
  ChillerIndexPlantSide = CondEntSetPtMgr(SetPtMgrNum)%ChillerIndexPlantSide
  BranchIndexPlantSide = CondEntSetPtMgr(SetPtMgrNum)%BranchIndexPlantSide
  TypeNum = CondEntSetPtMgr(SetPtMgrNum)%TypeNum
  LoopIndexDemandSide = CondEntSetPtMgr(SetPtMgrNum)%LoopIndexDemandSide
  ChillerIndexDemandSide = CondEntSetPtMgr(SetPtMgrNum)%ChillerIndexDemandSide
  BranchIndexDemandSide = CondEntSetPtMgr(SetPtMgrNum)%BranchIndexDemandSide

  ! If chiller is on
  CurLoad = ABS(PlantLoop(LoopIndexPlantSide)%LoopSide(SupplySide)%Branch(BranchIndexPlantSide)%  &
     Comp(ChillerIndexPlantSide)%MyLoad)
  IF (CurLoad > 0) THEN
    IF (TypeNum == TypeOf_Chiller_Absorption .or. TypeNum == TypeOf_Chiller_CombTurbine       .or.  &
        TypeNum == TypeOf_Chiller_Electric   .or. TypeNum == TypeOf_Chiller_ElectricReformEIR .or.  &
        TypeNum == TypeOf_Chiller_EngineDriven) THEN
       TempDesCondIn = PlantLoop(LoopIndexPlantSide)%LoopSide(SupplySide)%Branch(BranchIndexPlantSide)%  &
          Comp(ChillerIndexPlantSide)%TempDesCondIn
       CondInletTemp = Node(PlantLoop(LoopIndexDemandSide)%LoopSide(DemandSide)%Branch(BranchIndexDemandSide)%  &
          Comp(ChillerIndexDemandSide)%NodeNumIn)%temp
       EvapOutletTemp = Node(PlantLoop(LoopIndexPlantSide)%LoopSide(SupplySide)%Branch(BranchIndexPlantSide)%  &
          Comp(ChillerIndexPlantSide)%NodeNumOut)%temp
       TempEvapOutDesign = PlantLoop(LoopIndexPlantSide)%LoopSide(SupplySide)%Branch(BranchIndexPlantSide)%  &
          Comp(ChillerIndexPlantSide)%TempDesEvapOut
       DesignClgCapacity_Watts = PlantLoop(LoopIndexPlantSide)%LoopSide(SupplySide)%Branch(BranchIndexPlantSide)%  &
          Comp(ChillerIndexPlantSide)%MaxLoad
       CurrentLoad_Watts = PlantReport(LoopIndexPlantSide)%CoolingDemand
    ELSE IF (TypeNum == TypeOf_Chiller_Indirect_Absorption .or. TypeNum == TypeOf_Chiller_DFAbsorption) THEN
       TempDesCondIn = PlantLoop(LoopIndexPlantSide)%LoopSide(SupplySide)%Branch(BranchIndexPlantSide)%  &
          Comp(ChillerIndexPlantSide)%TempDesCondIn
       TempEvapOutDesign = 6.666d0
    ELSE
       TempDesCondIn     = 25.0d0
       TempEvapOutDesign = 6.666d0
    END IF

    ! for attached chillers (that are running this timestep) find their Dsn_MinCondSetpt and Dsn_EntCondTemp
    Dsn_MinCondSetpt = 999.0d0
    Dsn_EntCondTemp = 0.0d0

    ! Design Minimum Condenser Entering as a function of the minimum lift and TEvapLvg
    ! for chillers operating on current cond loop this timestep
    Dsn_CondMinThisChiller = TempEvapOutDesign + (CondEntSetPtMgr(SetPtMgrNum)%MinimumLiftTD)
    Dsn_MinCondSetpt = MIN(Dsn_MinCondSetpt, Dsn_CondMinThisChiller)

    ! Design entering condenser water temperature for chillers operating
    ! on current cond loop this timestep
    Dsn_EntCondTemp = MAX(Dsn_EntCondTemp,TempDesCondIn)

    ! Load this array with the design capacity and actual load of each chiller this timestep
    Des_Load = 0.0d0
    Act_Load = 0.0d0
    Des_Load = DesignClgCapacity_Watts
    Act_Load = CurrentLoad_Watts

    ! ***** Load Calculations *****
    ! In this section the sum of the actual load (watts) and design load (watts)
    ! of the chillers that are on is calculated.
    Actual_Load_Sum = Actual_Load_Sum + Act_Load
    Design_Load_Sum = Design_Load_Sum + Des_Load

    ! Exit if the chillers are all off this hour
    If (Actual_Load_Sum <= 0) Then
      CondWaterSetpoint = Dsn_EntCondTemp
      RETURN
    End If

    ! ***** Weighted Ratio Calculation *****
    ! This section first calculates the actual (ALW) and design (DLW) individual
    ! weights. Then the weighted actual and design loads are computed. Finally
    ! the Weighted Ratio is found.
    If (Actual_Load_Sum .NE. 0 .AND. Design_Load_Sum .NE. 0) Then
      ALW = ((Act_Load/Actual_Load_Sum)*Act_Load)
      DLW = ((Des_Load/Design_Load_Sum)*Des_Load)
    Else
      ALW = 0.0d0
      DLW = 0.0d0
    End If
    Weighted_Actual_Load_Sum = Weighted_Actual_Load_Sum + ALW
    Weighted_Design_Load_Sum = Weighted_Design_Load_Sum + DLW
    Weighted_Ratio = Weighted_Actual_Load_Sum/Weighted_Design_Load_Sum

    ! ***** Optimal Temperature Calculation *****
    ! In this section the optimal temperature is computed along with the minimum
    ! design wet bulb temp and the mimimum actual wet bulb temp.
    ! Min_DesignWB = ACoef1 + ACoef2*OaWb + ACoef3*WPLR + ACoef4*TwrDsnWB + ACoef5*NF
    Min_DesignWB = CurveValue(CondEntSetPtMgr(SetPtMgrNum)%MinTwrWbCurve,OutWetBulbTemp,  &
       Weighted_Ratio,Twr_DesignWB,NormDsnCondFlow)

    ! Min_ActualWb = BCoef1 + BCoef2*MinDsnWB + BCoef3*WPLR + BCoef4*TwrDsnWB + BCoef5*NF
    Min_ActualWb = CurveValue(CondEntSetPtMgr(SetPtMgrNum)%MinOaWbCurve,Min_DesignWB,  &
       Weighted_Ratio,Twr_DesignWB,NormDsnCondFlow)

    ! Opt_CondEntTemp = CCoef1 + CCoef2*OaWb + CCoef3*WPLR + CCoef4*TwrDsnWB + CCoef5*NF
    Opt_CondEntTemp = CurveValue(CondEntSetPtMgr(SetPtMgrNum)%OptCondEntCurve,OutWetBulbTemp,  &
       Weighted_Ratio,Twr_DesignWB,NormDsnCondFlow)

    ! ***** Calculate (Cond ent - Evap lvg) Section *****
    ! In this section we find the worst case of (Cond ent - Evap lvg) for the
    ! chillers that are running.
    Cur_MinLiftTD = 9999.0d0
    temp_MinLiftTD = 20.d0/1.8d0
    temp_MinLiftTD = CondInletTemp - EvapOutletTemp
    Cur_MinLiftTD = MIN(Cur_MinLiftTD, temp_MinLiftTD)
  END IF

  ! ***** Limit conditions Section *****
  ! Check for limit conditions and control to the proper value.
  If ((Weighted_Ratio >= 0.90d0) .AND. (Opt_CondEntTemp >= (Dsn_EntCondTemp + 1.0d0))) Then
  ! Optimized value exceeds the design condenser entering condition or chillers
  ! near full load condition; reset condenser entering setpoint to its design value
  Setpoint = Dsn_EntCondTemp + 1.0d0
  Else
    If ((OutWetBulbTemp >= Min_ActualWb) .AND. (Twr_DesignWB >= Min_DesignWB) &
       .and. (Cur_MinLiftTD > CondEntSetPtMgr(SetPtMgrNum)%MinimumLiftTD)) Then
       ! Boundaries are satified; use optimized condenser entering water temp
       Setpoint = Opt_CondEntTemp
    Else
      !Boundaries violated; Reset to scheduled value of condenser water entering setpoint
      Setpoint = CondWaterSetpoint
    End If
  End If
  ! Do not allow new setpoint to be less than the design condenser minimum entering condition,
  ! i.e., TCondWaterEnt not allowed to be less than DsnEvapWaterLvg + MinimumLiftTD
  CondWaterSetpoint = Max (Setpoint, Dsn_MinCondSetpt)
  CondEntSetPtMgr(SetPtMgrNum)%SetPt = CondWaterSetpoint

END SUBROUTINE CalcCondEntSetPoint

SUBROUTINE CalcIdealCondEntSetPoint(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Heejin Cho, PNNL
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate the optimal condenser water entering temperature set point for a chiller plant.


          ! METHODOLOGY EMPLOYED:
          ! The “ideal” chiller-tower optimization scheme uses a search algorithm to find the ideal optimal setpoint
          ! at a given timestep. This requires resimulating HVAC systems at each timestep until finding
          ! an “optimal” condenser water entering setpoint (OptSetpoint) which gives the minimum total chiller,
          ! cooling tower, chilled water pump and condenser water pump power consumption.
          ! The OptSetpoint falls between realistic minimum and maximum boundaries, which are set by the user.
          ! The minimum boundary is determined based on the minimum lift (user input)
          ! and evaporator leaving water temperature. The maximum boundary is specified by the user.
          ! It is assumed that a single minimum point exists between these boundaries.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant
  USE DataLoopNode,   ONLY: Node

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)  :: SetPtMgrNum       !number of the current set point manager being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS

          ! DERIVED TYPE DEFINITIONS
          ! na

  ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: CondWaterSetpoint        = 0.0d0           ! Condenser entering water temperature setpoint this timestep, C
  REAL(r64) :: InitCondWaterSetpoint    = 0.0d0           ! Initial condenser entering water temperature setpoint this timestep, C
  REAL(r64) :: EvapOutletTemp           = 0.0d0           ! Evaporator water outlet temperature (C)
  REAL(r64) :: CondTempLimit            = 0.0d0           ! Condenser entering water temperature setpoint lower limit
  REAL(r64) :: CurLoad                  = 0.0d0           ! Current cooling load, W
  REAL(r64) :: MinLiftTD                = 0.0d0           ! Minimum lift (Tcond entering - Tevap leaving) TD this timestep
  INTEGER   :: ChillerTypeNum           = 0             ! Chiller type number
  INTEGER   :: ChillerLoopNum           = 0             ! Chiller loop number
  INTEGER   :: ChillerBranchNum         = 0             ! Chiller branch number
  INTEGER   :: ChillerNum               = 0             ! Chiller number
  INTEGER   :: TowerLoopNum             = 0             ! Tower loop number
  INTEGER   :: CondLoopNum              = 0             ! Condenser loop number
  INTEGER   :: TowerBranchNum           = 0             ! Tower branch number
  INTEGER   :: TowerNum                 = 0             ! Tower number
  INTEGER   :: ChilledPumpBranchNum     = 0             ! Chilled water pump branch number
  INTEGER   :: ChilledPumpNum           = 0             ! Chilled water pump number
  INTEGER   :: CondPumpBranchNum        = 0             ! Condenser water pump branch number
  INTEGER   :: CondPumpNum              = 0             ! Condenser pump number
!  INTEGER   :: VarNum                   = 0             ! Metered variable number
!  INTEGER   :: VarType                  = 0             ! Metered variable type number
!  INTEGER   :: VarIndex                 = 0             ! Metered variable index
  REAL(r64) :: DeltaTotEnergy           = 0.0d0           ! Difference between total energy consumptions at this time step
                                                        ! and at the previous time step
  REAL(r64) :: ChillerEnergy            = 0.0d0           ! Chiller energy consumption
  REAL(r64) :: ChilledPumpEnergy        = 0.0d0           ! Chilled water pump energy consumption
  REAL(r64) :: TowerFanEnergy           = 0.0d0           ! Colling tower fan energy consumption
  REAL(r64) :: CondPumpEnergy           = 0.0d0           ! Condenser water pump energy consumption
  REAL(r64) :: TotEnergy                = 0.0d0           ! Totoal energy consumptions at this time step
  REAL(r64) :: TotEnergyPre             = 0.0d0           ! Totoal energy consumptions at the previous time step
  LOGICAL   :: RunSubOptCondEntTemp     =.FALSE.
  LOGICAL   :: RunFinalOptCondEntTemp   =.FALSE.
  LOGICAL   :: FirstTime                =.true.
  LOGICAL, SAVE, DIMENSION(:), ALLOCATABLE :: SetupIdealCondEntSetPtVars

  !! Current timestep's condenser water entering setpoint
  IF (FirstTime) THEN
    ALLOCATE(SetupIdealCondEntSetPtVars(NumIdealCondEntSetPtMgrs))
    SetupIdealCondEntSetPtVars=.true.
    FirstTime=.false.
  ENDIF

  InitCondWaterSetpoint = IdealCondEntSetPtMgr(SetPtMgrNum)%MaxCondEntTemp
  MinLiftTD = IdealCondEntSetPtMgr(SetPtMgrNum)%MinimumLiftTD
  ChillerTypeNum = IdealCondEntSetPtMgr(SetPtMgrNum)%TypeNum
  ChillerLoopNum = IdealCondEntSetPtMgr(SetPtMgrNum)%LoopIndexPlantSide
  ChillerBranchNum = IdealCondEntSetPtMgr(SetPtMgrNum)%BranchIndexPlantSide
  ChillerNum = IdealCondEntSetPtMgr(SetPtMgrNum)%ChillerIndexPlantSide
  TowerLoopNum = IdealCondEntSetPtMgr(SetPtMgrNum)%CondLoopNum
  CondLoopNum = IdealCondEntSetPtMgr(SetPtMgrNum)%CondLoopNum
  TowerBranchNum = IdealCondEntSetPtMgr(SetPtMgrNum)%CondBranchNum
  TowerNum = IdealCondEntSetPtMgr(SetPtMgrNum)%TowerNum
  ChilledPumpBranchNum = IdealCondEntSetPtMgr(SetPtMgrNum)%ChilledPumpBranchNum
  ChilledPumpNum = IdealCondEntSetPtMgr(SetPtMgrNum)%ChilledPumpNum
  CondPumpBranchNum = IdealCondEntSetPtMgr(SetPtMgrNum)%CondPumpBranchNum
  CondPumpNum = IdealCondEntSetPtMgr(SetPtMgrNum)%CondPumpNum

  IF (MetersHaveBeenInitialized) THEN
    ! Setup meter vars
    IF (SetupIdealCondEntSetPtVars(SetPtMgrNum)) THEN
      CALL SetupMeteredVarsForSetPt(SetPtMgrNum)
      SetupIdealCondEntSetPtVars(SetPtMgrNum)=.false.
    ENDIF
  ENDIF

  IF (MetersHaveBeenInitialized .and. RunOptCondEntTemp) THEN

    ! If chiller is on
    CurLoad = ABS(PlantLoop(ChillerLoopNum)%LoopSide(SupplySide)%Branch(ChillerBranchNum)%Comp(ChillerNum)%MyLoad)

    IF (CurLoad > 0) THEN
      ! Calculate the minimum condenser inlet temperature boundry for set point
      IF (ChillerTypeNum == TypeOf_Chiller_Absorption .or. ChillerTypeNum == TypeOf_Chiller_CombTurbine       .or.  &
          ChillerTypeNum == TypeOf_Chiller_Electric   .or. ChillerTypeNum == TypeOf_Chiller_ElectricReformEIR .or.  &
          ChillerTypeNum == TypeOf_Chiller_EngineDriven) THEN
          EvapOutletTemp = Node(PlantLoop(ChillerLoopNum)%LoopSide(SupplySide)%Branch(ChillerBranchNum)%  &
            Comp(ChillerNum)%NodeNumOut)%temp
      ELSE
          EvapOutletTemp = 6.666d0
      END IF
      CondTempLimit = MinLiftTD + EvapOutletTemp

      ! Energy consumption metered variable number = 1

      ! Get the chiller energy consumption
      ChillerEnergy = GetInternalVariableValue(IdealCondEntSetPtMgr(SetPtMgrNum)%ChllrVarType,  &
         IdealCondEntSetPtMgr(SetPtMgrNum)%ChllrVarIndex)

      ! Get the chilled water pump energy consumption
      ChilledPumpEnergy = GetInternalVariableValue(IdealCondEntSetPtMgr(SetPtMgrNum)%ChlPumpVarType,  &
         IdealCondEntSetPtMgr(SetPtMgrNum)%ChlPumpVarIndex)

      ! Get the cooling tower fan energy consumption
      TowerFanEnergy = GetInternalVariableValue(IdealCondEntSetPtMgr(SetPtMgrNum)%ClTowerVarType,  &
         IdealCondEntSetPtMgr(SetPtMgrNum)%ClTowerVarIndex)

      ! Get the condenser pump energy consumption
      CondPumpEnergy = GetInternalVariableValue(IdealCondEntSetPtMgr(SetPtMgrNum)%CndPumpVarType,  &
         IdealCondEntSetPtMgr(SetPtMgrNum)%CndPumpVarIndex)

      ! Calculate the total energy consumption
      TotEnergy = ChillerEnergy + ChilledPumpEnergy + TowerFanEnergy + CondPumpEnergy

      IF (TotEnergyPre /= 0.0d0) THEN
        DeltaTotEnergy = 0.0d0
        ! Calculate the total energy consumption difference
        DeltaTotEnergy = TotEnergyPre - TotEnergy
        ! Search for the minimum total energy consumption
        IF ((DeltaTotEnergy > 0) .and. (CondWaterSetpoint >= CondTempLimit) .and.  &
            (.not. RunFinalOptCondEntTemp)) THEN
          IF (.not. RunSubOptCondEntTemp) THEN
            CondWaterSetpoint = CondWaterSetpoint - 1.0d0
            RunOptCondEntTemp = .TRUE.
          ELSE
            CondWaterSetpoint = CondWaterSetpoint - 0.2d0
            RunOptCondEntTemp = .TRUE.
          END IF
          TotEnergyPre = TotEnergy
        ! Set smaller set point (0.2 degC) decrease
        ELSE IF ((DeltaTotEnergy < 0) .and. (.not. RunSubOptCondEntTemp) .and. &
           (CondWaterSetpoint > CondTempLimit) .and. (.not. RunFinalOptCondEntTemp)) THEN
          CondWaterSetpoint = CondWaterSetpoint + 0.8d0
          RunOptCondEntTemp = .TRUE.
          RunSubOptCondEntTemp = .TRUE.
        ELSE
          IF (.not. RunFinalOptCondEntTemp) THEN
            CondWaterSetpoint = CondWaterSetpoint + 0.2d0
            RunOptCondEntTemp = .TRUE.
            RunSubOptCondEntTemp = .FALSE.
            RunFinalOptCondEntTemp = .TRUE.
          ELSE
            CondWaterSetpoint = CondWaterSetpoint
            TotEnergyPre = 0.0d0
            RunOptCondEntTemp = .FALSE.
            RunSubOptCondEntTemp = .FALSE.
            RunFinalOptCondEntTemp = .FALSE.
          ENDIF
        END IF
      ELSE
        CondWaterSetpoint = InitCondWaterSetpoint - 1.0d0
        TotEnergyPre = TotEnergy
        RunOptCondEntTemp = .TRUE.
        RunSubOptCondEntTemp = .FALSE.
      ENDIF
    ELSE
    CondWaterSetpoint = InitCondWaterSetpoint
    TotEnergyPre = 0.0d0
    RunOptCondEntTemp = .FALSE.
    RunSubOptCondEntTemp = .FALSE.
    END IF
  ELSE
    CondWaterSetpoint = InitCondWaterSetpoint
    RunOptCondEntTemp = .FALSE.
    RunSubOptCondEntTemp = .FALSE.
  END IF

  IdealCondEntSetPtMgr(SetPtMgrNum)%SetPt = CondWaterSetpoint

END SUBROUTINE CalcIdealCondEntSetPoint

SUBROUTINE SetupMeteredVarsForSetPt(SetPtMgrNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   Sep 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! For the Ideal Cond reset setpoint manager, this sets up the
          ! report variables used during the calculation.

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPlant

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: SetPtMgrNum  ! number of this setpoint manager (only Ideal Cond Reset)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
  INTERFACE GetNumMeteredVariables
    FUNCTION GetNumMeteredVariables(ComponentType,ComponentName) RESULT(NumVariables)
      CHARACTER(len=*), INTENT(IN) :: ComponentType  ! Given Component Type
      CHARACTER(len=*), INTENT(IN) :: ComponentName  ! Given Component Name (user defined)
      INTEGER                      :: NumVariables
    END FUNCTION
  END INTERFACE

  INTERFACE GetMeteredVariables
    SUBROUTINE GetMeteredVariables(ComponentType,ComponentName,VarIndexes,VarTypes,IndexTypes,  &
                                   UnitsStrings,ResourceTypes,EndUses,Groups,Names,NumFound,VarIDs)
      CHARACTER(len=*),      INTENT(IN)            :: ComponentType  ! Given Component Type
      CHARACTER(len=*),      INTENT(IN)            :: ComponentName  ! Given Component Name (user defined)
      INTEGER, DIMENSION(:), INTENT(OUT)           :: VarIndexes     ! Variable Numbers
      INTEGER, DIMENSION(:), INTENT(OUT)           :: VarTypes       ! Variable Types (1=integer, 2=real, 3=meter)
      INTEGER, DIMENSION(:), INTENT(OUT)           :: IndexTypes     ! Variable Index Types (1=Zone,2=HVAC)
      CHARACTER(len=*), DIMENSION(:), INTENT(OUT)  :: UnitsStrings   ! UnitsStrings for each variable
      INTEGER, DIMENSION(:), INTENT(OUT)  :: ResourceTypes  ! ResourceTypes for each variable
      CHARACTER(len=*), DIMENSION(:),   &
                            OPTIONAL, INTENT(OUT)  :: EndUses        ! EndUses for each variable
      CHARACTER(len=*), DIMENSION(:),   &
                            OPTIONAL, INTENT(OUT)  :: Groups         ! Groups for each variable
      CHARACTER(len=*), DIMENSION(:),   &
                            OPTIONAL, INTENT(OUT)  :: Names          ! Variable Names for each variable
      INTEGER, OPTIONAL, INTENT(OUT)               :: NumFound       ! Number Found
      INTEGER, DIMENSION(:), OPTIONAL, INTENT(OUT) :: VarIDs         ! Variable Report Numbers
    END SUBROUTINE
  END INTERFACE


          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength) :: TypeOfComp
  CHARACTER(len=MaxNameLength) :: NameOfComp

  INTEGER, ALLOCATABLE, DIMENSION(:)        :: VarIndexes     ! Variable Numbers
  INTEGER, ALLOCATABLE, DIMENSION(:)        :: VarTypes       ! Variable Types (1=integer, 2=real, 3=meter)
  INTEGER, ALLOCATABLE, DIMENSION(:)        :: IndexTypes     ! Variable Index Types (1=Zone,2=HVAC)
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: UnitsStrings   ! UnitsStrings for each variable
  INTEGER, ALLOCATABLE, DIMENSION(:) :: ResourceTypes  ! ResourceTypes for each variable
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: EndUses        ! EndUses for each variable
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Groups         ! Groups for each variable
  CHARACTER(len=MaxNameLength), ALLOCATABLE, DIMENSION(:) :: Names          ! Variable Names for each variable
  INTEGER :: NumVariables
  INTEGER :: NumFound

  INTEGER   :: ChillerTypeNum           = 0             ! Chiller type number
  INTEGER   :: ChillerLoopNum           = 0             ! Chiller loop number
  INTEGER   :: ChillerBranchNum         = 0             ! Chiller branch number
  INTEGER   :: ChillerNum               = 0             ! Chiller number
  INTEGER   :: TowerLoopNum             = 0             ! Tower loop number
  INTEGER   :: TowerBranchNum           = 0             ! Tower branch number
  INTEGER   :: CondLoopNum              = 0             ! Condenser loop number
  INTEGER   :: TowerNum                 = 0             ! Tower number
  INTEGER   :: ChilledPumpBranchNum     = 0             ! Chilled water pump branch number
  INTEGER   :: ChilledPumpNum           = 0             ! Chilled water pump number
  INTEGER   :: CondPumpBranchNum        = 0             ! Condenser water pump branch number
  INTEGER   :: CondPumpNum              = 0             ! Condenser pump number

  ChillerTypeNum = IdealCondEntSetPtMgr(SetPtMgrNum)%TypeNum
  ChillerLoopNum = IdealCondEntSetPtMgr(SetPtMgrNum)%LoopIndexPlantSide
  ChillerBranchNum = IdealCondEntSetPtMgr(SetPtMgrNum)%BranchIndexPlantSide
  ChillerNum = IdealCondEntSetPtMgr(SetPtMgrNum)%ChillerIndexPlantSide
  TowerLoopNum = IdealCondEntSetPtMgr(SetPtMgrNum)%CondLoopNum
  CondLoopNum = IdealCondEntSetPtMgr(SetPtMgrNum)%CondLoopNum
  TowerBranchNum = IdealCondEntSetPtMgr(SetPtMgrNum)%CondBranchNum
  TowerNum = IdealCondEntSetPtMgr(SetPtMgrNum)%TowerNum
  ChilledPumpBranchNum = IdealCondEntSetPtMgr(SetPtMgrNum)%ChilledPumpBranchNum
  ChilledPumpNum = IdealCondEntSetPtMgr(SetPtMgrNum)%ChilledPumpNum
  CondPumpBranchNum = IdealCondEntSetPtMgr(SetPtMgrNum)%CondPumpBranchNum
  CondPumpNum = IdealCondEntSetPtMgr(SetPtMgrNum)%CondPumpNum

  TypeOfComp = PlantLoop(ChillerLoopNum)%LoopSide(SupplySide)%Branch(ChillerBranchNum)%Comp(ChillerNum)%TypeOf
  NameOfComp = PlantLoop(ChillerLoopNum)%LoopSide(SupplySide)%Branch(ChillerBranchNum)%Comp(ChillerNum)%Name
  NumVariables=GetNumMeteredVariables(TypeOfComp,NameOfComp)
  ALLOCATE (VarIndexes(NumVariables))
  ALLOCATE (VarTypes(NumVariables))
  ALLOCATE (IndexTypes(NumVariables))
  ALLOCATE (UnitsStrings(NumVariables))
  ALLOCATE (ResourceTypes(NumVariables))
  ALLOCATE (EndUses(NumVariables))
  ALLOCATE (Groups(NumVariables))
  ALLOCATE (Names(NumVariables))

  CALL GetMeteredVariables(TypeOfComp, NameOfComp, &
                           VarIndexes, VarTypes, &
                           IndexTypes, UnitsStrings, &
                           ResourceTypes, EndUses, Groups, Names, NumFound)
  IdealCondEntSetPtMgr(SetPtMgrNum)%ChllrVarType  = VarTypes(1)
  IdealCondEntSetPtMgr(SetPtMgrNum)%ChllrVarIndex = VarIndexes(1)

  DEALLOCATE (VarIndexes)
  DEALLOCATE (VarTypes)
  DEALLOCATE (IndexTypes)
  DEALLOCATE (UnitsStrings)
  DEALLOCATE (ResourceTypes)
  DEALLOCATE (EndUses)
  DEALLOCATE (Groups)
  DEALLOCATE (Names)

  TypeOfComp = PlantLoop(ChillerLoopNum)%LoopSide(SupplySide)%Branch(ChilledPumpBranchNum)%Comp(ChilledPumpNum)%TypeOf
  NameOfComp = PlantLoop(ChillerLoopNum)%LoopSide(SupplySide)%Branch(ChilledPumpBranchNum)%Comp(ChilledPumpNum)%Name
  NumVariables=GetNumMeteredVariables(TypeOfComp,NameOfComp)
  ALLOCATE (VarIndexes(NumVariables))
  ALLOCATE (VarTypes(NumVariables))
  ALLOCATE (IndexTypes(NumVariables))
  ALLOCATE (UnitsStrings(NumVariables))
  ALLOCATE (ResourceTypes(NumVariables))
  ALLOCATE (EndUses(NumVariables))
  ALLOCATE (Groups(NumVariables))
  ALLOCATE (Names(NumVariables))

  CALL GetMeteredVariables(TypeOfComp, NameOfComp, &
                           VarIndexes, VarTypes, &
                           IndexTypes, UnitsStrings, &
                           ResourceTypes, EndUses, Groups, Names, NumFound)
  IdealCondEntSetPtMgr(SetPtMgrNum)%ChlPumpVarType  = VarTypes(1)
  IdealCondEntSetPtMgr(SetPtMgrNum)%ChlPumpVarIndex = VarIndexes(1)

  DEALLOCATE (VarIndexes)
  DEALLOCATE (VarTypes)
  DEALLOCATE (IndexTypes)
  DEALLOCATE (UnitsStrings)
  DEALLOCATE (ResourceTypes)
  DEALLOCATE (EndUses)
  DEALLOCATE (Groups)
  DEALLOCATE (Names)

  TypeOfComp = PlantLoop(TowerLoopNum)%LoopSide(SupplySide)%Branch(TowerBranchNum)%Comp(TowerNum)%TypeOf
  NameOfComp = PlantLoop(TowerLoopNum)%LoopSide(SupplySide)%Branch(TowerBranchNum)%Comp(TowerNum)%Name
  NumVariables=GetNumMeteredVariables(TypeOfComp,NameOfComp)
  ALLOCATE (VarIndexes(NumVariables))
  ALLOCATE (VarTypes(NumVariables))
  ALLOCATE (IndexTypes(NumVariables))
  ALLOCATE (UnitsStrings(NumVariables))
  ALLOCATE (ResourceTypes(NumVariables))
  ALLOCATE (EndUses(NumVariables))
  ALLOCATE (Groups(NumVariables))
  ALLOCATE (Names(NumVariables))

  CALL GetMeteredVariables(TypeOfComp, NameOfComp, &
                           VarIndexes, VarTypes, &
                           IndexTypes, UnitsStrings, &
                           ResourceTypes, EndUses, Groups, Names, NumFound)
  IdealCondEntSetPtMgr(SetPtMgrNum)%ClTowerVarType   = VarTypes(1)
  IdealCondEntSetPtMgr(SetPtMgrNum)%ClTowerVarIndex  = VarIndexes(1)

  DEALLOCATE (VarIndexes)
  DEALLOCATE (VarTypes)
  DEALLOCATE (IndexTypes)
  DEALLOCATE (UnitsStrings)
  DEALLOCATE (ResourceTypes)
  DEALLOCATE (EndUses)
  DEALLOCATE (Groups)
  DEALLOCATE (Names)

  TypeOfComp = PlantLoop(CondLoopNum)%LoopSide(SupplySide)%Branch(CondPumpBranchNum)%Comp(CondPumpNum)%TypeOf
  NameOfComp = PlantLoop(CondLoopNum)%LoopSide(SupplySide)%Branch(CondPumpBranchNum)%Comp(CondPumpNum)%Name
  NumVariables=GetNumMeteredVariables(TypeOfComp,NameOfComp)
  ALLOCATE (VarIndexes(NumVariables))
  ALLOCATE (VarTypes(NumVariables))
  ALLOCATE (IndexTypes(NumVariables))
  ALLOCATE (UnitsStrings(NumVariables))
  ALLOCATE (ResourceTypes(NumVariables))
  ALLOCATE (EndUses(NumVariables))
  ALLOCATE (Groups(NumVariables))
  ALLOCATE (Names(NumVariables))

  CALL GetMeteredVariables(TypeOfComp, NameOfComp, &
                           VarIndexes, VarTypes, &
                           IndexTypes, UnitsStrings, &
                           ResourceTypes, EndUses, Groups, Names, NumFound)
  IdealCondEntSetPtMgr(SetPtMgrNum)%CndPumpVarType   = VarTypes(1)
  IdealCondEntSetPtMgr(SetPtMgrNum)%CndPumpVarIndex  = VarIndexes(1)

  DEALLOCATE (VarIndexes)
  DEALLOCATE (VarTypes)
  DEALLOCATE (IndexTypes)
  DEALLOCATE (UnitsStrings)
  DEALLOCATE (ResourceTypes)
  DEALLOCATE (EndUses)
  DEALLOCATE (Groups)
  DEALLOCATE (Names)

  RETURN

END SUBROUTINE SetupMeteredVarsForSetPt

SUBROUTINE UpdateSetPointManagers

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   July 1998
          !       MODIFIED       Shirey/Raustad (FSEC), Jan 2004
          !                      P. Haves Oct 2004
          !                        Add new setpoint managers:
          !                          SET POINT MANAGER:WARMEST TEMP FLOW and
          !                          SET POINT MANAGER:COLDEST TEMP FLOW
          !                      Nov 2004 M. J. Witte, GARD Analytics, Inc.
          !                        Add new setpoint managers:
          !                          SET POINT MANAGER:SINGLE ZONE HEATING and
          !                          SET POINT MANAGER:SINGLE ZONE COOLING
          !                        Work supported by ASHRAE research project 1254-RP
          !                      B. Griffith Aug. 2006.  Allow HUMRAT for scheduled setpoint manager
          !                      P. Haves Aug 2007
          !                        SET POINT MANAGER:WARMEST TEMP FLOW:
          !                          Set AirLoopControlInfo()%LoopFlowRateSet every call not just on
          !                          initialization (flag now reset in SUBROUTINE ResetHVACControl)
          !                        Removed SET POINT MANAGER:COLDEST TEMP FLOW
          !                      July 2010 B.A. Nigusse, FSEC/UCF
          !                        Added new setpoint managers
          !                          SetpointManager:MultiZone:Heating:Average
          !                          SetpointManager:MultiZone:Cooling:Average
          !                          SetpointManager:MultiZone:MinimumHumidity:Average
          !                          SetpointManager:MultiZone:MaximumHumidity:Average
          !                      Aug 2010 B.A. Nigusse, FSEC/UCF
          !                        Added new setpoint managers:
          !                          SetpointManager:MultiZone:Humidity:Minimum
          !                          SetpointManager:MultiZone:Humidity:Maximum

          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Loop over all the Setpoint Managers and use their output arrays
          ! to set the node setpoints.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: SysSizingCalc, AnyEnergyManagementSystemInModel
  USE DataHVACGlobals, ONLY: DoSetPointTest, SetPointErrorFlag
  USE EMSManager,      ONLY: iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS

    IMPLICIT NONE

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: SetPtMgrNum
INTEGER :: CtrlNodeIndex
INTEGER :: NodeNum

! Loop over all the Scheduled Setpoint Managers

DO SetPtMgrNum=1,NumSchSetPtMgrs

  DO CtrlNodeIndex=1,SchSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
                                                           ! setpoints from this setpoint manager
    NodeNum = SchSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number

    Select Case (SchSetPtMgr(SetPtMgrNum)%CtrlTypeMode)
    ! set the setpoint depending on the type of variable being controlled
    CASE (iCtrlVarType_Temp)
      Node(NodeNum)%TempSetPoint         = SchSetPtMgr(SetPtMgrNum)%SetPt
    CASE (iCtrlVarType_MaxTemp)
      Node(NodeNum)%TempSetPointHi              = SchSetPtMgr(SetPtMgrNum)%SetPt
    CASE (iCtrlVarType_MinTemp)
      Node(NodeNum)%TempSetPointLo              = SchSetPtMgr(SetPtMgrNum)%SetPt
    CASE (iCtrlVarType_HumRat)
      Node(NodeNum)%HumRatSetPoint       = SchSetPtMgr(SetPtMgrNum)%SetPt
    CASE (iCtrlVarType_MaxHumRat)
      Node(NodeNum)%HumRatMax            = SchSetPtMgr(SetPtMgrNum)%SetPt
    CASE (iCtrlVarType_MinHumRat)
      Node(NodeNum)%HumRatMin            = SchSetPtMgr(SetPtMgrNum)%SetPt
    CASE (iCtrlVarType_MassFlow)
      Node(NodeNum)%MassFlowRateSetPoint = SchSetPtMgr(SetPtMgrNum)%SetPt
    CASE (iCtrlVarType_MaxMassFlow)
      Node(NodeNum)%MassFlowRateMax      = SchSetPtMgr(SetPtMgrNum)%SetPt
    CASE (iCtrlVarType_MinMassFlow)
      Node(NodeNum)%MassFlowRateMin      = SchSetPtMgr(SetPtMgrNum)%SetPt
    END SELECT

  END DO !nodes in list

END DO ! setpoint manger:scheduled

! Loop over all the Scheduled Dual Setpoint Managers

DO SetPtMgrNum=1,NumDualSchSetPtMgrs

  DO CtrlNodeIndex=1,DualSchSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
          ! setpoints from this setpoint manager
    NodeNum = DualSchSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number

    IF (DualSchSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
      Node(NodeNum)%TempSetPointHi = DualSchSetPtMgr(SetPtMgrNum)%SetPtHi ! Set the setpoint High
      Node(NodeNum)%TempSetPointLo = DualSchSetPtMgr(SetPtMgrNum)%SetPtLo ! Set the setpoint Low
      Node(NodeNum)%TempSetPoint = (Node(NodeNum)%TempSetPointHi + Node(NodeNum)%TempSetPointLo)/2.0d0 ! average of the high and low
    END IF

  END DO

END DO


! Loop over all the Outside Air Setpoint Managers

DO SetPtMgrNum=1,NumOutAirSetPtMgrs

  DO CtrlNodeIndex=1,OutAirSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
                                                           ! setpoints from this setpoint manager
    NodeNum = OutAirSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number

    IF (OutAirSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
      Node(NodeNum)%TempSetPoint = OutAirSetPtMgr(SetPtMgrNum)%SetPt ! Set the setpoint
    END IF

  END DO

END DO

! Loop over all the Single Zone Reheat Setpoint Managers

DO SetPtMgrNum=1,NumSZRhSetPtMgrs

  DO CtrlNodeIndex=1,SingZoneRhSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
                                                           ! setpoints from this setpoint manager
    NodeNum = SingZoneRhSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number

    IF (SingZoneRhSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
      Node(NodeNum)%TempSetPoint = SingZoneRhSetPtMgr(SetPtMgrNum)%SetPt ! Set the setpoint
    END IF

  END DO

END DO

! Loop over all the Single Zone Heating Setpoint Managers

DO SetPtMgrNum=1,NumSZHtSetPtMgrs

  DO CtrlNodeIndex=1,SingZoneHtSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
                                                           ! setpoints from this setpoint manager
    NodeNum = SingZoneHtSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number

    IF (SingZoneHtSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
      Node(NodeNum)%TempSetPoint = SingZoneHtSetPtMgr(SetPtMgrNum)%SetPt ! Set the setpoint
    END IF

  END DO

END DO

! Loop over all the Single Zone Cooling Setpoint Managers

DO SetPtMgrNum=1,NumSZClSetPtMgrs

  DO CtrlNodeIndex=1,SingZoneClSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
                                                           ! setpoints from this setpoint manager
    NodeNum = SingZoneClSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number

    IF (SingZoneClSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
      Node(NodeNum)%TempSetPoint = SingZoneClSetPtMgr(SetPtMgrNum)%SetPt ! Set the setpoint
    END IF

  END DO

END DO

! Loop over all the Single Zone Minimum Humidity Setpoint Managers

DO SetPtMgrNum=1,NumSZMinHumSetPtMgrs

  DO CtrlNodeIndex=1,SZMinHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
                                                                ! setpoints from this setpoint manager
    NodeNum = SZMinHumSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number

    Node(NodeNum)%HumRatMin = SZMinHumSetPtMgr(SetPtMgrNum)%SetPt ! Set the setpoint

  END DO

END DO

! Loop over all the Single Zone Maximum Humidity Setpoint Managers

DO SetPtMgrNum=1,NumSZMaxHumSetPtMgrs

  DO CtrlNodeIndex=1,SZMaxHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
                                                                ! setpoints from this setpoint manager
    NodeNum = SZMaxHumSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number

    Node(NodeNum)%HumRatMax = SZMaxHumSetPtMgr(SetPtMgrNum)%SetPt ! Set the setpoint

  END DO

END DO

! Loop over all the Warmest Setpoint Managers

DO SetPtMgrNum=1,NumWarmestSetPtMgrs

  DO CtrlNodeIndex=1,WarmestSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
                                                               ! setpoints from this setpoint manager
    NodeNum = WarmestSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number

    IF (WarmestSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
      Node(NodeNum)%TempSetPoint = WarmestSetPtMgr(SetPtMgrNum)%SetPt ! Set the setpoint
    END IF

  END DO

END DO

! Loop over all the Coldest Setpoint Managers

DO SetPtMgrNum=1,NumColdestSetPtMgrs

  DO CtrlNodeIndex=1,ColdestSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
                                                               ! setpoints from this setpoint manager
    NodeNum = ColdestSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number

    IF (ColdestSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
      Node(NodeNum)%TempSetPoint = ColdestSetPtMgr(SetPtMgrNum)%SetPt ! Set the setpoint
    END IF

  END DO

END DO

! Loop over all the Warmest Temp Flow Setpoint Managers

DO SetPtMgrNum=1,NumWarmestSetPtMgrsTempFlow

  DO CtrlNodeIndex=1,WarmestSetPtMgrTempFlow(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
                                                               ! setpoints from this setpoint manager
    NodeNum = WarmestSetPtMgrTempFlow(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number

    IF (WarmestSetPtMgrTempFlow(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
      Node(NodeNum)%TempSetPoint = WarmestSetPtMgrTempFlow(SetPtMgrNum)%SetPt ! Set the supply air temperature setpoint
      AirLoopFlow(WarmestSetPtMgrTempFlow(SetPtMgrNum)%AirLoopNum)%ReqSupplyFrac &
                                 = WarmestSetPtMgrTempFlow(SetPtMgrNum)%Turndown ! Set the supply air flow rate
      AirLoopControlInfo(WarmestSetPtMgrTempFlow(SetPtMgrNum)%AirLoopNum)%LoopFlowRateSet = .TRUE.  ! PH 8/17/07
    END IF

  END DO

END DO

DO SetPtMgrNum=1,NumRABFlowSetPtMgrs

  NodeNum = RABFlowSetPtMgr(SetPtMgrNum)%RABSplitOutNode ! Get the node number

  IF (RABFlowSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MassFlow) THEN
    Node(NodeNum)%MassFlowRateSetPoint = RABFlowSetPtMgr(SetPtMgrNum)%FlowSetPt ! Set the flow setpoint
  END IF

END DO

! Loop over all the MultiZone Average Cooling Setpoint Managers
DO SetPtMgrNum=1,NumMZClgAverageSetPtMGrs
  DO CtrlNodeIndex=1,MZAverageCoolingSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
                                                                        !  setpoints from this setpoint manager
    NodeNum = MZAverageCoolingSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
    IF (MZAverageCoolingSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
      Node(NodeNum)%TempSetPoint = MZAverageCoolingSetPtMgr(SetPtMgrNum)%SetPt ! Set the temperature setpoint
    END IF
  END DO
END DO

! Loop over all the MultiZone Average Heating Setpoint Managers
DO SetPtMgrNum=1,NumMZHtgAverageSetPtMGrs
  DO CtrlNodeIndex=1,MZAverageHeatingSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
                                                                        !  setpoints from this setpoint manager
    NodeNum = MZAverageHeatingSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
    IF (MZAverageHeatingSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
      Node(NodeNum)%TempSetPoint = MZAverageHeatingSetPtMgr(SetPtMgrNum)%SetPt ! Set the temperature setpoint
    END IF
  END DO
END DO

! Loop over all the MultiZone Average Minimum Humidity Setpoint Managers
DO SetPtMgrNum=1, NumMZAverageMinHumSetPtMgrs
  DO CtrlNodeIndex=1,MZAverageMinHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
                                                                       !  setpoints from this setpoint manager
    NodeNum = MZAverageMinHumSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
    IF (MZAverageMinHumSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MinHumRat) THEN
      Node(NodeNum)%HumRatMin = MZAverageMinHumSetPtMgr(SetPtMgrNum)%SetPt ! Set the humidity ratio setpoint
    END IF
  END DO
END DO

! Loop over all the MultiZone Average Maxiumum Humidity Setpoint Managers
DO SetPtMgrNum=1, NumMZAverageMaxHumSetPtMgrs
  DO CtrlNodeIndex=1,MZAverageMaxHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
                                                                       !  setpoints from this setpoint manager
    NodeNum = MZAverageMaxHumSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
    IF (MZAverageMaxHumSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MaxHumRat) THEN
      Node(NodeNum)%HumRatMax = MZAverageMaxHumSetPtMgr(SetPtMgrNum)%SetPt ! Set the humidity ratio setpoint
    END IF
  END DO
END DO

! Loop over all the Multizone Minimum Humidity Ratio Setpoint Managers
DO SetPtMgrNum=1, NumMZMinHumSetPtMgrs
  DO CtrlNodeIndex=1,MZMinHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
                                                              !  setpoints from this setpoint manager
    NodeNum = MZMinHumSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex)     ! Get the node number
    IF (MZMinHumSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MinHumRat) THEN
      Node(NodeNum)%HumRatMin = MZMinHumSetPtMgr(SetPtMgrNum)%SetPt      ! Set the humidity ratio setpoint
    END IF
  END DO
END DO

! Loop over all the Multizone Maximum Humidity Ratio Setpoint Managers
DO SetPtMgrNum=1, NumMZMaxHumSetPtMgrs
  DO CtrlNodeIndex=1,MZMaxHumSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
                                                               !  setpoints from this setpoint manager
    NodeNum = MZMaxHumSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex)     ! Get the node number
    IF (MZMaxHumSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MaxHumRat) THEN
      Node(NodeNum)%HumRatMax = MZMaxHumSetPtMgr(SetPtMgrNum)%SetPt      ! Set the humidity ratio setpoint
    END IF
  END DO
END DO

! Loop over all the Follow Outdoor Air Setpoint Managers
DO SetPtMgrNum=1,NumFollowOATempSetPtMgrs
  DO CtrlNodeIndex=1,FollowOATempSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
                                                                        !  setpoints from this setpoint manager
    NodeNum = FollowOATempSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
    IF (FollowOATempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
      Node(NodeNum)%TempSetPoint = FollowOATempSetPtMgr(SetPtMgrNum)%SetPt ! Set the temperature setpoint
    ELSEIF (FollowOATempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MaxTemp) THEN
      Node(NodeNum)%TempSetPointHi = FollowOATempSetPtMgr(SetPtMgrNum)%SetPt ! Set the temperature setpoint
    ELSEIF (FollowOATempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MinTemp) THEN
      Node(NodeNum)%TempSetPointLo = FollowOATempSetPtMgr(SetPtMgrNum)%SetPt ! Set the temperature setpoint
    END IF
  END DO
END DO

! Loop over all the Follow System Node Temperature Setpoint Managers
DO SetPtMgrNum=1,NumFollowSysNodeTempSetPtMgrs
  DO CtrlNodeIndex=1,FollowSysNodeTempSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
                                                                        !  setpoints from this setpoint manager
    NodeNum = FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
    IF (FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
      Node(NodeNum)%TempSetPoint = FollowSysNodeTempSetPtMgr(SetPtMgrNum)%SetPt ! Set the temperature setpoint
    ELSEIF (FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MaxTemp) THEN
      Node(NodeNum)%TempSetPointHi = FollowSysNodeTempSetPtMgr(SetPtMgrNum)%SetPt ! Set the temperature setpoint
    ELSEIF (FollowSysNodeTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MinTemp) THEN
      Node(NodeNum)%TempSetPointLo = FollowSysNodeTempSetPtMgr(SetPtMgrNum)%SetPt ! Set the temperature setpoint
    END IF
  END DO
END DO

! Loop over all the Ground Tempearture Setpoint Managers
DO SetPtMgrNum=1,NumGroundTempSetPtMgrs
  DO CtrlNodeIndex=1,GroundTempSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
                                                                        !  setpoints from this setpoint manager
    NodeNum = GroundTempSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
    IF (GroundTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
      Node(NodeNum)%TempSetPoint = GroundTempSetPtMgr(SetPtMgrNum)%SetPt ! Set the temperature setpoint
    ELSEIF (GroundTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MaxTemp) THEN
      Node(NodeNum)%TempSetPointHi = GroundTempSetPtMgr(SetPtMgrNum)%SetPt ! Set the temperature setpoint
    ELSEIF (GroundTempSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_MinTemp) THEN
      Node(NodeNum)%TempSetPointLo = GroundTempSetPtMgr(SetPtMgrNum)%SetPt ! Set the temperature setpoint
    END IF
  END DO
END DO

! Loop over all Condenser Entering Set Point Managers

DO SetPtMgrNum=1,NumCondEntSetPtMgrs
  DO CtrlNodeIndex=1,CondEntSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
                                                                        !  set points from this set point manager
    NodeNum = CondEntSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
    IF (CondEntSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
      Node(NodeNum)%TempSetPoint = CondEntSetPtMgr(SetPtMgrNum)%SetPt ! Set the temperature setpoint
    END IF
  END DO
END DO

! Loop over all Ideal Condenser Entering Set Point Managers

DO SetPtMgrNum=1,NumIdealCondEntSetPtMgrs
  DO CtrlNodeIndex=1,IdealCondEntSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
    ! set points from this set point manager
    NodeNum = IdealCondEntSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
    IF (IdealCondEntSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
      Node(NodeNum)%TempSetPoint = IdealCondEntSetPtMgr(SetPtMgrNum)%SetPt ! Set the temperature setpoint
    END IF
  END DO
END DO

!loop over all single zone on/off cooling setpoint managers
DO SetPtMgrNum=1,NumSZOneStageCoolingSetPtMgrs
  DO CtrlNodeIndex=1,SZOneStageCoolingSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
    ! set points from this set point manager
    NodeNum = SZOneStageCoolingSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
    IF (SZOneStageCoolingSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
      Node(NodeNum)%TempSetPoint = SZOneStageCoolingSetPtMgr(SetPtMgrNum)%SetPt ! Set the temperature setpoint
    END IF
  END DO
ENDDO

!loop over all single zone on/off heating setpoint managers
DO SetPtMgrNum=1,NumSZOneStageHeatingSetPtMgrs
  DO CtrlNodeIndex=1,SZOneStageHeatingSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
    ! set points from this set point manager
    NodeNum = SZOneStageHeatingSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number
    IF (SZOneStageHeatingSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
      Node(NodeNum)%TempSetPoint = SZOneStageHeatingSetPtMgr(SetPtMgrNum)%SetPt ! Set the temperature setpoint
    END IF
  END DO
ENDDO

RETURN

END SUBROUTINE UpdateSetPointManagers

SUBROUTINE UpdateMixedAirSetPoints

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Buhl
          !       DATE WRITTEN   May 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Loop over all the Mixed Air Managers and use their output arrays
          ! to set the node setpoints.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

    IMPLICIT NONE

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: SetPtMgrNum
INTEGER :: CtrlNodeIndex
INTEGER :: NodeNum

! Loop over all the Mixed Air Setpoint Managers

DO SetPtMgrNum=1,NumMixedAirSetPtMgrs

  DO CtrlNodeIndex=1,MixedAirSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
                                                                ! setpoints from this setpoint manager
    NodeNum = MixedAirSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number

    IF (MixedAirSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType_Temp) THEN
      Node(NodeNum)%TempSetPoint = MixedAirSetPtMgr(SetPtMgrNum)%SetPt ! Set the setpoint
    END IF

  END DO

END DO

RETURN

END SUBROUTINE UpdateMixedAirSetPoints

SUBROUTINE UpdateOAPretreatSetPoints

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         M. J. Witte based on UpdateMixedAirSetPoints by Fred Buhl,
          !                        Work supported by ASHRAE research project 1254-RP
          !       DATE WRITTEN   January 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE
          ! Loop over all the Outside Air Pretreat Managers and use their output arrays
          ! to set the node setpoints.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

    IMPLICIT NONE

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER :: SetPtMgrNum
INTEGER :: CtrlNodeIndex
INTEGER :: NodeNum

! Loop over all the Mixed Air Setpoint Managers

DO SetPtMgrNum=1,NumOAPretreatSetPtMgrs

  DO CtrlNodeIndex=1,OAPretreatSetPtMgr(SetPtMgrNum)%NumCtrlNodes ! Loop over the list of nodes wanting
                                                                ! setpoints from this setpoint manager
    NodeNum = OAPretreatSetPtMgr(SetPtMgrNum)%CtrlNodes(CtrlNodeIndex) ! Get the node number

    SELECT CASE(OAPretreatSetPtMgr(SetPtMgrNum)%CtrlTypeMode)
      CASE(iCtrlVarType_Temp)  ! 'Temperature'
        Node(NodeNum)%TempSetPoint = OAPretreatSetPtMgr(SetPtMgrNum)%SetPt ! Set the setpoint
      CASE(iCtrlVarType_MaxHumRat) ! 'MaximumHumidityRatio'
        Node(NodeNum)%HumRatMax = OAPretreatSetPtMgr(SetPtMgrNum)%SetPt ! Set the setpoint
      CASE(iCtrlVarType_MinHumRat) ! 'MinimumHumidityRatio'
        Node(NodeNum)%HumRatMin = OAPretreatSetPtMgr(SetPtMgrNum)%SetPt ! Set the setpoint
      CASE(iCtrlVarType_HumRat) ! 'HumidityRatio'
        Node(NodeNum)%HumRatSetPoint = OAPretreatSetPtMgr(SetPtMgrNum)%SetPt ! Set the setpoint
    END SELECT

  END DO

END DO

RETURN

END SUBROUTINE UpdateOAPretreatSetPoints

LOGICAL FUNCTION IsNodeOnSetPtManager(NodeNum,SetPtType)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Sankaranarayanan K P
          !       DATE WRITTEN   January 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Determines if a particular node is acted upon by a specific setpoint manager

          ! METHODOLOGY EMPLOYED:
          ! Cycle through all setpoint managers and find if the node passed in has a setpoint manager of passed
          ! in type associated to it.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:


INTEGER, INTENT(IN)  ::  NodeNum
INTEGER, INTENT(IN)  ::  SetPtType

INTEGER SetPtMgrNum
INTEGER NumNode

 ! First time called, get the input for all the setpoint managers
 IF (GetInputFlag) THEN
   CALL GetSetPointManagerInputs
   GetInputFlag = .FALSE.
 END IF

 IsNodeOnSetPtManager = .FALSE.

 DO SetPtMgrNum = 1, NumSchSetPtMgrs
  IF(SetPtType == SchSetPtMgr(SetPtMgrNum)%CtrlTypeMode) THEN
   DO NumNode = 1, SchSetPtMgr(SetPtMgrNum)%NumCtrlNodes
    IF(NodeNum == SchSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNode)) THEN
     IsNodeOnSetPtManager = .TRUE.
     EXIT
    END IF
   END DO
  END IF
 END DO

 RETURN

END FUNCTION IsNodeOnSetPtManager

LOGICAL FUNCTION NodeHasSPMCtrlVarType(NodeNum,iCtrlVarType)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Chandan Sharma
          !       DATE WRITTEN   March 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Determines if a particular node is acted upon by a specific setpoint manager

          ! METHODOLOGY EMPLOYED:
          ! Cycle through all setpoint managers and find if the node has a specific control type

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER, INTENT(IN) ::  NodeNum
INTEGER, INTENT(IN) ::  iCtrlVarType

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER SetPtMgrNum           ! loop counter for each set point manager
INTEGER NumNode               ! loop counter for each node and specific control type

          ! FLOW:

! First time called, get the input for all the setpoint managers
IF (GetInputFlag) THEN
  CALL GetSetPointManagerInputs
  GetInputFlag = .FALSE.
END IF

! Initialize to false that node is not controlled by set point manager
 NodeHasSPMCtrlVarType = .FALSE.

 SPMLoop: DO SetPtMgrNum = 1, NumAllSetPtMgrs
   DO NumNode = 1, AllSetPtMgr(SetPtMgrNum)%NumCtrlNodes
    IF(NodeNum == AllSetPtMgr(SetPtMgrNum)%CtrlNodes(NumNode)) THEN
      IF(AllSetPtMgr(SetPtMgrNum)%CtrlTypeMode == iCtrlVarType)THEN
!       If specific control type is found, it doesn't matter if there are other of same type.
        NodeHasSPMCtrlVarType = .TRUE.
        EXIT SPMLoop
      END IF
    END IF
   END DO
 END DO SPMLoop

 RETURN

END FUNCTION NodeHasSPMCtrlVarType

SUBROUTINE CheckIFAnyIdealCondEntSetPoint

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Heejin Cho, PNNL
          !       DATE WRITTEN   March 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Determine if ideal condenser entering set point manager is used in model and set flag


          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor , ONLY: GetNumObjectsFound
  USE DataGlobals ,    ONLY: AnyIdealCondEntSetPointInModel

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

  INTEGER, EXTERNAL :: GetNewUnitNumber
  INTEGER :: write_stat
  CHARACTER(len=MaxNameLength) :: cCurrentModuleObject

  cCurrentModuleObject = 'SetpointManager:CondenserEnteringReset:Ideal'
  NumIdealCondEntSetPtMgrs  = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumIdealCondEntSetPtMgrs > 0 ) THEN
    AnyIdealCondEntSetPointInModel = .TRUE.
  ELSE
    AnyIdealCondEntSetPointInModel = .FALSE.
  ENDIF

  RETURN

END SUBROUTINE CheckIFAnyIdealCondEntSetPoint

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

END MODULE SetPointManager
