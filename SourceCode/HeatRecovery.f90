MODULE HeatRecovery

  ! Module containing the routines dealing with heat recovery from exhaust or relief air

  ! MODULE INFORMATION:
  !       AUTHOR         Michael Wetter
  !       DATE WRITTEN   March 1999
  !       MODIFIED       F Buhl Nov 2000, D Shirey Feb 2003, R. Raustad April 2003
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! To encapsulate the data and routines required to model heat
  ! recovery components in the EnergyPlus HVAC simulation

  ! METHODOLOGY EMPLOYED:
  ! Heat exchanger effectiveness - NTU models are used.

  ! REFERENCES:
  ! M. Wetter, Simulation Model Air-to-Air Plate Heat Exchanger,LBNL Report 42354, 1999.
  !
  ! ARI Standard 1060-2001,Rating Air-to-Air Heat Exchangers for Energy Recovery Ventilation Equipment, www.ari.org
  ! ASHRAE Standard 84, Method of Testing Air-To-Air Heat Exchangers, www.ashrae.org
  ! U.S. Environmental Protection Agency software "SAVES" -
  !  School Advanced Ventilation Engineering Software http://www.epa.gov/iaq/schooldesign/saves.html


  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
  USE DataPrecisionGlobals
  USE DataHVACGlobals
  USE DataGlobals, ONLY: WarmupFlag, MaxNameLength, BeginEnvrnFlag, SysSizingCalc, SecInHour, ScheduleAlwaysOn
  USE DataInterfaces, ONLY: SetupOutputVariable, ShowWarningError, ShowSevereError, ShowFatalError,  &
                         ShowRecurringWarningErrorAtEnd, ShowContinueError, ShowContinueErrorTimeStamp
  USE DataLoopNode
  USE DataEnvironment, ONLY: OutBaroPress, StdBaroPress, CurMnDy, EnvironmentName, StdRhoAir
  USE InputProcessor,  ONLY: SameString

  ! Use statements for access to subroutines in other modules
  USE ScheduleManager
  USE General, ONLY: SolveRegulaFalsi, RoundSigDigits
  USE Psychrometrics

  IMPLICIT NONE         ! Enforce explicit typing of all variables

  PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: KELVZERO = 273.16d0
  REAL(r64), PARAMETER :: SMALL = 1.d-10

! Heat exchanger performance data type
  INTEGER, PARAMETER :: BALANCEDHX_PERFDATATYPE1 = 1

! Heat exchanger configurations
  INTEGER, PARAMETER :: Counter_Flow = 1
  INTEGER, PARAMETER :: Parallel_Flow = 2
  INTEGER, PARAMETER :: Cross_Flow_Both_Unmixed = 3
  INTEGER, PARAMETER :: Cross_Flow_Other = 4

! Heat exchanger configuration types
  INTEGER, PARAMETER :: Plate = 1
  INTEGER, PARAMETER :: Rotary = 2

! Economizer lockout operation
  INTEGER, PARAMETER :: EconoLockOut_No  = 0
  INTEGER, PARAMETER :: EconoLockOut_Yes = 1

  ! DERIVED TYPE DEFINITIONS:

  TYPE HeatExchCond
    CHARACTER(len=MaxNameLength) :: Name     =' '            ! name of component
    INTEGER                      :: ExchTypeNum = 0            ! Integer equivalent to ExchType
    INTEGER                      :: HeatExchPerfTypeNum = 0  ! Desiccant balanced heat exchanger performance data type num
    CHARACTER(len=MaxNameLength) :: HeatExchPerfName  = ' ' ! Desiccant balanced heat exchanger performance data name

    INTEGER :: SchedPtr=0                  ! index of schedule
    INTEGER :: FlowArr=0                   ! flow Arrangement:
                                           ! 1: COUNTER_FLOW
                                           ! 2: PARALLEL_FLOW
                                           ! 3: CROSS_FLOW_BOTH_UNMIXED
    INTEGER :: EconoLockOut       =0       ! 1: Yes;  0: No
    REAL(r64)    :: hARatio            =0.0d0     ! ratio of supply side h*A to secondary side h*A
    REAL(r64)    :: NomSupAirVolFlow   =0.0d0     ! nominal supply air volume flow rate (m3/s)
    REAL(r64)    :: NomSupAirInTemp    =0.0d0     ! nominal supply air inlet temperature (C)
    REAL(r64)    :: NomSupAirOutTemp   =0.0d0     ! nominal supply air outlet temperature (C)
    REAL(r64)    :: NomSecAirVolFlow   =0.0d0     ! nominal secondary air volume flow rate (m3/s)
    REAL(r64)    :: NomSecAirInTemp    =0.0d0     ! nominal secondary air inlet temperature (C)
    REAL(r64)    :: NomElecPower       =0.0d0     ! nominal electric power consumption [W]

    ! values describing nominal condition (derived from input parameters)
    REAL(r64)    :: UA0                =0.0d0     ! (Uavg*A) at nominal condition
    REAL(r64)    :: mTSup0             =0.0d0     ! product mDot*Tabs, supply  air, nominal cond.
    REAL(r64)    :: mTSec0             =0.0d0     ! product mDot*Tabs, exhaust air, nominal cond
    REAL(r64)    :: NomSupAirMassFlow  =0.0d0     ! nominal supply air mass flow rate (kg/s)
    REAL(r64)    :: NomSecAirMassFlow  =0.0d0     ! nominal secondary air mass flow rate (kg/s)

    ! Nodes
    INTEGER :: SupInletNode       =0       ! supply air inlet node number
    INTEGER :: SupOutletNode      =0       ! supply air outlet node number
    INTEGER :: SecInletNode       =0       ! secondary air inlet node number
    INTEGER :: SecOutletNode      =0       ! secondary air outlet node number

    ! inlet conditions
    REAL(r64)    :: SupInTemp          =0.0d0     ! supply air inlet temperature (C)
    REAL(r64)    :: SupInHumRat        =0.0d0     ! supply air inlet humidity ratio (kg water/kg dry air)
    REAL(r64)    :: SupInEnth          =0.0d0     ! supply air inlet enthalpy (J/kg)
    REAL(r64)    :: SupInMassFlow      =0.0d0     ! supply air inlet mass flow rate (kg/s)
    REAL(r64)    :: SecInTemp          =0.0d0     ! secondary air inlet temperature (C)
    REAL(r64)    :: SecInHumRat        =0.0d0     ! secondary air inlet humidity ratio (kg water/kg dry air)
    REAL(r64)    :: SecInEnth          =0.0d0     ! secondary air inlet enthalpy (J/kg)
    REAL(r64)    :: SecInMassFlow      =0.0d0     ! secondary air inlet mass flow rate (kg/s)

    ! balanced desiccant inputs
    INTEGER :: PerfDataIndex    =0            ! Performance data index allocating performance data number to heat exchanger
    REAL(r64)    :: FaceArea         =0.0d0          ! face area of balanced desiccant heat exchangers to determine face velocity [m2]
    LOGICAL :: UnbalancedWarningFlag = .TRUE. ! Used to print one-time warning when unbalanced flow exists (then set to FALSE)

    ! generic hx performance inputs
    REAL(r64)    :: HeatEffectSensible100=0.0d0           ! heating sensible effectiveness at 100% rated air flow
    REAL(r64)    :: HeatEffectSensible75 =0.0d0           ! heating sensible effectiveness at 75% rated air flow
    REAL(r64)    :: HeatEffectLatent100  =0.0d0           ! heating latent effectiveness at 100% rated air flow
    REAL(r64)    :: HeatEffectLatent75   =0.0d0           ! heating latent effectiveness at 75% rated air flow
    REAL(r64)    :: CoolEffectSensible100=0.0d0           ! cooling sensible effectiveness at 100% rated air flow
    REAL(r64)    :: CoolEffectSensible75 =0.0d0           ! cooling sensible effectiveness at 75% rated air flow
    REAL(r64)    :: CoolEffectLatent100  =0.0d0           ! cooling latent effectiveness at 100% rated air flow
    REAL(r64)    :: CoolEffectLatent75   =0.0d0           ! cooling latent effectiveness at 75% rated air flow
    INTEGER :: HeatExchEconoMode    =0             ! generic heat exchanger economize mode option
                                                   ! 1 = None, 2 = Bypass, 3 = Stop Rotary HX Rotation
    INTEGER :: ExchConfigNum        = 0            ! parameter equivalent of HX configuration, plate or rotary

    ! frost control parameters
    Character(len=MaxNameLength) :: FrostControlType=' ' ! type of frost control used if any
    REAL(r64)    :: ThresholdTemperature      =0.0d0            ! threshold temperature for frost control
    REAL(r64)    :: InitialDefrostTime        =0.0d0            ! initial defrost time
    REAL(r64)    :: RateofDefrostTimeIncrease =0.0d0            ! rate of change of defrost time
    REAL(r64)    :: DefrostFraction           =0.0d0            ! fraction of time HX is in frost control mode
    LOGICAL :: ControlToTemperatureSetpoint=.FALSE.      ! temperature control flag for generic HX

    ! outlet conditions
    REAL(r64)    :: SupOutTemp       =0.0d0       ! supply air outlet temperature (C)
    REAL(r64)    :: SupOutHumRat     =0.0d0       ! supply air outlet humidity ratio (kg water/kg dry air)
    REAL(r64)    :: SupOutEnth       =0.0d0       ! supply air outlet enthalpy (J/kg)
    REAL(r64)    :: SupOutMassFlow   =0.0d0       ! supply air outlet mass flow rate (kg/s)
    REAL(r64)    :: SecOutTemp       =0.0d0       ! secondary air outlet temperature (C)
    REAL(r64)    :: SecOutHumRat     =0.0d0       ! secondary air outlet humidity ratio (kg water/kg dry air)
    REAL(r64)    :: SecOutEnth       =0.0d0       ! secondary air outlet enthalpy (J/kg)
    REAL(r64)    :: SecOutMassFlow   =0.0d0       ! secondary air outlet mass flow rate (kg/s)

    ! report values
    REAL(r64)    :: SensHeatingRate  =0.0d0       ! rate of sensible heat being added to the supply (primary) air [W]
    REAL(r64)    :: SensHeatingEnergy=0.0d0       ! sensible heat added to the supply (primary) air [J]
    REAL(r64)    :: LatHeatingRate   =0.0d0       ! rate of latent heat being added to the supply (primary) air [W]
    REAL(r64)    :: LatHeatingEnergy =0.0d0       ! latent heat added to the supply (primary) air [J]
    REAL(r64)    :: TotHeatingRate   =0.0d0       ! rate of total heat being added to the supply (primary) air [W]
    REAL(r64)    :: TotHeatingEnergy =0.0d0       ! total heat added to the supply (primary) air [J]
    REAL(r64)    :: SensCoolingRate  =0.0d0       ! rate of sensible heat being removed from the supply (primary) air [W]
    REAL(r64)    :: SensCoolingEnergy=0.0d0       ! sensible heat removed from the supply (primary) air [J]
    REAL(r64)    :: LatCoolingRate   =0.0d0       ! rate of latent heat being removed from the supply (primary) air [W]
    REAL(r64)    :: LatCoolingEnergy =0.0d0       ! latent heat removed from the supply (primary) air [J]
    REAL(r64)    :: TotCoolingRate   =0.0d0       ! rate of total heat being removed from the supply (primary) air [W]
    REAL(r64)    :: TotCoolingEnergy =0.0d0       ! total heat removed from the supply (primary) air [J]
    REAL(r64)    :: ElecUseEnergy    =0.0d0       ! electricity consumption [J]
    REAL(r64)    :: ElecUseRate      =0.0d0       ! electricity consumption rate [W]
    REAL(r64)    :: SensEffectiveness=0.0d0       ! heat exchanger sensible effectiveness [-]
    REAL(r64)    :: LatEffectiveness =0.0d0       ! heat exchanger latent effectiveness [-]
    REAL(r64)    :: SupBypassMassFlow=0.0d0       ! supply air mass flow rate bypassing the heat exchanger [kg/s]
    REAL(r64)    :: SecBypassMassFlow=0.0d0       ! secondary air mass flow rate bypassing the heat exchanger [kg/s]
    INTEGER :: LowFlowErrCount       =0         ! Counter for recurring warning message
    INTEGER :: LowFlowErrIndex       =0         ! Index to recurring warning message
    INTEGER :: UnBalancedErrCount    =0         ! Counter for recurring warning message
    INTEGER :: UnBalancedErrIndex    =0         ! Index to recurring warning message

  END TYPE HeatExchCond

  TYPE BalancedDesDehumPerfData
  ! User Input data
    CHARACTER(len=MaxNameLength) :: Name                   =' '  ! unique name of balanced desiccant performance data type object
    CHARACTER(len=MaxNameLength) :: PerfType               =' '  ! Type of performance data set
    REAL(r64)                    :: NomSupAirVolFlow       =0.0d0  ! nominal supply air volumetric flow rate m^3/s
    REAL(r64)                    :: NomProcAirFaceVel      =0.0d0  ! nominal process air face velocity m/s
    REAL(r64)                    :: NomElecPower           =0.0d0  ! nominal electric power consumption [W]

 ! regeneration outlet temperature equation coefficients and limits
    REAL(r64)                    :: B1                     =0.0d0  ! constant coefficient for outlet regeneration temprature equation
    REAL(r64)                    :: B2                     =0.0d0  ! regen inlet humrat coeff for outlet regen temperature equation
    REAL(r64)                    :: B3                     =0.0d0  ! regen inlet temp coeff for outlet regen temprature equation
    REAL(r64)                    :: B4                     =0.0d0  ! (regen in humrat/regen in temp) coeff for outlet regen temp eq
    REAL(r64)                    :: B5                     =0.0d0  ! process inlet humrat coeff for outlet regen temp equation
    REAL(r64)                    :: B6                     =0.0d0  ! process inlet temp coeff for outlet regen temp equation
    REAL(r64)                    :: B7                     =0.0d0  ! (process in humrat/proc in temp) coeff for outlet regen temp eq
    REAL(r64)                    :: B8                     =0.0d0  ! process, regen face velocity coeff for outlet regen temp eq
    REAL(r64)                    :: T_MinRegenAirInTemp    =0.0d0  ! min allowable regen inlet air temperature [C]
    REAL(r64)                    :: T_MaxRegenAirInTemp    =0.0d0  ! max allowable regen inlet air temperature [C]
    REAL(r64)                    :: T_MinRegenAirInHumRat  =0.0d0  ! min allowable regen inlet air humidity ratio [kg water / kg air]
    REAL(r64)                    :: T_MaxRegenAirInHumRat  =0.0d0  ! max allowable regen inlet air humidity ratio [kg water / kg air]
    REAL(r64)                    :: T_MinProcAirInTemp     =0.0d0  ! min allowable process inlet air temperature [C]
    REAL(r64)                    :: T_MaxProcAirInTemp     =0.0d0  ! max allowable process inlet air temperature [C]
    REAL(r64)                    :: T_MinProcAirInHumRat   =0.0d0  ! min allowable process inlet air humidity ratio [kg water/kg air]
    REAL(r64)                    :: T_MaxProcAirInHumRat   =0.0d0  ! max allowable process inlet air humidity ratio [kg water/kg air]
    REAL(r64)                    :: T_MinFaceVel           =0.0d0  ! min allowable process, regen face velocity [m/s]
    REAL(r64)                    :: T_MaxFaceVel           =0.0d0  ! max allowable process, regen face velocity [m/s]
    REAL(r64)                    :: MinRegenAirOutTemp     =0.0d0  ! min allowable regen outlet air temperature [C]
    REAL(r64)                    :: MaxRegenAirOutTemp     =0.0d0  ! max allowable regen outlet air temperature [C]
    REAL(r64)                    :: T_MinRegenAirInRelHum  =0.0d0  ! min allowable regen inlet air relative humidity [%]
    REAL(r64)                    :: T_MaxRegenAirInRelHum  =0.0d0  ! max allowable regen inlet air relative humidity [%]
    REAL(r64)                    :: T_MinProcAirInRelHum   =0.0d0  ! min allowable process inlet air relative humidity [%]
    REAL(r64)                    :: T_MaxProcAirInRelHum   =0.0d0  ! max allowable process inlet air relative humidity [%]

 ! regeneration outlet humidity ratio equation coefficients and limits
    REAL(r64)                    :: C1                     =0.0d0  ! constant coeff for outlet regen humidity ratio equation
    REAL(r64)                    :: C2                     =0.0d0  ! regen inlet humrat coeff for outlet regen humidity ratio eq
    REAL(r64)                    :: C3                     =0.0d0  ! regen inlet temp coeff for outlet regen humidity ratio equation
    REAL(r64)                    :: C4                     =0.0d0  ! (regen in humrat/regen in temp) coeff for outlet regen humrat eq
    REAL(r64)                    :: C5                     =0.0d0  ! process inlet humrat coeff for outlet regen humidity ratio eq
    REAL(r64)                    :: C6                     =0.0d0  ! process inlet temp coeff for outlet regen humidity ratio eq
    REAL(r64)                    :: C7                     =0.0d0  ! (proc in humrat/proc in temp) coeff for outlet regen humrat eq
    REAL(r64)                    :: C8                     =0.0d0  ! process, regen face velocity coeff for outlet regen humrat eq
    REAL(r64)                    :: H_MinRegenAirInTemp    =0.0d0  ! min allowable regen inlet air temperature [C]
    REAL(r64)                    :: H_MaxRegenAirInTemp    =0.0d0  ! max allowable regen inlet air temperature [C]
    REAL(r64)                    :: H_MinRegenAirInHumRat  =0.0d0  ! min allowable regen inlet air humidity ratio [kg water / kg air]
    REAL(r64)                    :: H_MaxRegenAirInHumRat  =0.0d0  ! max allowable regen inlet air humidity ratio [kg water / kg air]
    REAL(r64)                    :: H_MinProcAirInTemp     =0.0d0  ! min allowable process inlet air temperature [C]
    REAL(r64)                    :: H_MaxProcAirInTemp     =0.0d0  ! max allowable process inlet air temperature [C]
    REAL(r64)                    :: H_MinProcAirInHumRat   =0.0d0  ! min allowable process inlet air humidity ratio [kg water/kg air]
    REAL(r64)                    :: H_MaxProcAirInHumRat   =0.0d0  ! max allowable process inlet air humidity ratio [kg water/kg air]
    REAL(r64)                    :: H_MinFaceVel           =0.0d0  ! min allowable process, regen face velocity [m/s]
    REAL(r64)                    :: H_MaxFaceVel           =0.0d0  ! max allowable process, regen face velocity [m/s]
    REAL(r64)                    :: MinRegenAirOutHumRat   =0.0d0  ! min allowable regen outlet air temperature [C]
    REAL(r64)                    :: MaxRegenAirOutHumRat   =0.0d0  ! max allowable regen outlet air temperature [C]
    REAL(r64)                    :: H_MinRegenAirInRelHum  =0.0d0  ! min allowable regen inlet air relative humidity [%]
    REAL(r64)                    :: H_MaxRegenAirInRelHum  =0.0d0  ! max allowable regen inlet air relative humidity [%]
    REAL(r64)                    :: H_MinProcAirInRelHum   =0.0d0  ! min allowable process inlet air relative humidity [%]
    REAL(r64)                    :: H_MaxProcAirInRelHum   =0.0d0  ! max allowable process inlet air relative humidity [%]

   ! for model bound checking
    ! regen inlet relative humidity for temperature equation
    LOGICAL                      :: PrintRegenInRelHumTempMess   = .FALSE. !- flag to print regen in RH error message for temp eq
    INTEGER                      :: RegenInRelHumTempErrIndex    = 0    !- index to recurring error struc for regen outlet hum rat
    INTEGER                      :: RegenInRelHumTempErrorCount  = 0    !- counter if regen outlet temp limits are exceeded
    CHARACTER(len=400)           :: RegenInRelHumTempBuffer1     = ' '  !- buffer for RegenOutHumRat warn mess on following timstep
    CHARACTER(len=400)           :: RegenInRelHumTempBuffer2     = ' '  !- buffer for RegenOutHumRat warn mess on following timstep
    CHARACTER(len=400)           :: RegenInRelHumTempBuffer3     = ' '  !- buffer for RegenOutHumRat warn mess on following timstep
    REAL(r64)                    :: RegenInRelHumTempLast        = 0.0d0  !- last value of regen outlet humidity ratio

    ! process inlet relative humidity for temperature equation
    LOGICAL                      :: PrintProcInRelHumTempMess   = .FALSE. !- flag to print regen in RH error message for temp eq
    INTEGER                      :: ProcInRelHumTempErrIndex    = 0    !- index to recurring error struc for regen outlet hum rat
    INTEGER                      :: ProcInRelHumTempErrorCount  = 0    !- counter if regen outlet temp limits are exceeded
    CHARACTER(len=400)           :: ProcInRelHumTempBuffer1     = ' '  !- buffer for RegenOutHumRat warn mess on following timstep
    CHARACTER(len=400)           :: ProcInRelHumTempBuffer2     = ' '  !- buffer for RegenOutHumRat warn mess on following timstep
    CHARACTER(len=400)           :: ProcInRelHumTempBuffer3     = ' '  !- buffer for RegenOutHumRat warn mess on following timstep
    REAL(r64)                    :: ProcInRelHumTempLast        = 0.0d0  !- last value of regen outlet humidity ratio

    ! regen inlet relative humidity for humidity ratio equation
    LOGICAL                      :: PrintRegenInRelHumHumRatMess = .FALSE. !- flag to print regen in RH error message for temp eq
    INTEGER                      :: RegenInRelHumHumRatErrIndex  = 0    !- index to recurring error struc for regen outlet hum rat
    INTEGER                      :: RegenInRelHumHumRatErrorCount= 0    !- counter if regen outlet temp limits are exceeded
    CHARACTER(len=400)           :: RegenInRelHumHumRatBuffer1   = ' '  !- buffer for RegenOutHumRat warn mess on following timstep
    CHARACTER(len=400)           :: RegenInRelHumHumRatBuffer2   = ' '  !- buffer for RegenOutHumRat warn mess on following timstep
    CHARACTER(len=400)           :: RegenInRelHumHumRatBuffer3   = ' '  !- buffer for RegenOutHumRat warn mess on following timstep
    REAL(r64)                    :: RegenInRelHumHumRatLast      = 0.0d0  !- last value of regen outlet humidity ratio

    ! process inlet relative humidity for humidity ratio equation
    LOGICAL                      :: PrintProcInRelHumHumRatMess  = .FALSE. !- flag to print regen in RH error message for temp eq
    INTEGER                      :: ProcInRelHumHumRatErrIndex   = 0    !- index to recurring error struc for regen outlet hum rat
    INTEGER                      :: ProcInRelHumHumRatErrorCount = 0    !- counter if regen outlet temp limits are exceeded
    CHARACTER(len=400)           :: ProcInRelHumHumRatBuffer1    = ' '  !- buffer for RegenOutHumRat warn mess on following timstep
    CHARACTER(len=400)           :: ProcInRelHumHumRatBuffer2    = ' '  !- buffer for RegenOutHumRat warn mess on following timstep
    CHARACTER(len=400)           :: ProcInRelHumHumRatBuffer3    = ' '  !- buffer for RegenOutHumRat warn mess on following timstep
    REAL(r64)                    :: ProcInRelHumHumRatLast       = 0.0d0  !- last value of regen outlet humidity ratio

    ! regen outlet temp variables
    LOGICAL                      :: PrintT_RegenInTempMessage    = .FALSE. !- flag to print regen in temp error message for temp eq
    LOGICAL                      :: PrintT_RegenInHumRatMessage  = .FALSE. !- flag to print regen in humrat err message for temp eq
    LOGICAL                      :: PrintT_ProcInTempMessage     = .FALSE. !- flag to print proc inlet temp err message for temp eq
    LOGICAL                      :: PrintT_ProcInHumRatMessage   = .FALSE. !- flag to print process hum rat err message for temp eq
    LOGICAL                      :: PrintT_FaceVelMessage        = .FALSE. !- flag to print face velocity error message
    LOGICAL                      :: PrintRegenOutTempMessage     = .FALSE. !- flag to print regen outlet temp error message
    LOGICAL                      :: PrintRegenOutTempFailedMessage = .FALSE. !- flag to print regen outlet temp error message

    ! regen outlet hum rat variables
    LOGICAL                      :: PrintH_RegenInTempMessage    = .FALSE. !- flag to print regen in temp err message for humrat eq
    LOGICAL                      :: PrintH_RegenInHumRatMessage  = .FALSE. !- flag for regen in humrat err message for humrat eq
    LOGICAL                      :: PrintH_ProcInTempMessage     = .FALSE. !- flag for process inlet temp err message for humrat eq
    LOGICAL                      :: PrintH_ProcInHumRatMessage   = .FALSE. !- flag for process hum rat error message for hum rat eq
    LOGICAL                      :: PrintH_FaceVelMessage        = .FALSE. !- flag for face velocity error message
    LOGICAL                      :: PrintRegenOutHumRatMessage   = .FALSE. !- flag for regen outlet hum rat error message
    LOGICAL                      :: PrintRegenInHumRatMessage    = .FALSE. !- flag for regen outlet hum rat error message

    ! used when regen outlet humrat is below regen inlet humrat, verify coefficients warning issued
    LOGICAL                      :: PrintRegenOutHumRatFailedMess = .FALSE. !- flag for regen outlet hum rat error message
    INTEGER                      :: RegenOutHumRatFailedErrIndex  = 0   !- index to recurring error struc for regen outlet hum rat
    INTEGER                      :: RegenOutHumRatFailedErrorCount = 0  !- counter if regen outlet temp limits are exceeded
    CHARACTER(len=400)           :: RegenOutHumRatFailedBuffer1  = ' '  !- buffer for RegenOutHumRat warn mess on following timstep
    CHARACTER(len=400)           :: RegenOutHumRatFailedBuffer2  = ' '  !- buffer for RegenOutHumRat warn mess on following timstep
    CHARACTER(len=400)           :: RegenOutHumRatFailedBuffer3  = ' '  !- buffer for RegenOutHumRat warn mess on following timstep
    REAL(r64)                    :: RegenOutHumRatFailedLast     = 0.0d0  !- last value of regen outlet humidity ratio

    ! used when regen and process mass flow rates are not equal to within 2%
    LOGICAL                      :: PrintImbalancedMassFlowMess  = .FALSE. !- flag for imbalanced regen and process mass flow rate
    INTEGER                      :: ImbalancedFlowErrIndex   = 0        !- index to recurring error struc for imbalanced flow
    INTEGER                      :: ImbalancedMassFlowErrorCount = 0    !- counter for imbalanced regen and process mass flow rate
    CHARACTER(len=400)           :: ImbalancedMassFlowBuffer1    = ' '  !- buffer for imbalanced regen and process mass flow rate
    CHARACTER(len=400)           :: ImbalancedMassFlowBuffer2    = ' '  !- buffer for imbalanced regen and process mass flow rate
    CHARACTER(len=400)           :: ImbalancedMassFlowBuffer3    = ' '  !- buffer for imbalanced regen and process mass flow rate
    REAL(r64)                    :: ABSImbalancedFlow            = 0.0d0  !- last value of heat exchanger mass flow rate imbalance

    ! regen outlet temp eqn
    INTEGER                      :: T_RegenInTempErrorCount      = 0    !- counter if regen inlet temp limits are exceeded
    INTEGER                      :: T_RegenInHumRatErrorCount    = 0    !- counter if regen inlet hum rat limits are exceeded
    INTEGER                      :: T_ProcInTempErrorCount       = 0    !- counter if process inlet temperature limits are exceeded
    INTEGER                      :: T_ProcInHumRatErrorCount     = 0    !- counter if process inlet hum rat limits are exceeded
    INTEGER                      :: T_FaceVelErrorCount          = 0    !- counter if regen and proc face vel limits are exceeded
    INTEGER                      :: T_RegenInTempErrIndex        = 0    !- index to recurring error structure for regen inlet temp
    INTEGER                      :: T_RegenInHumRatErrIndex      = 0    !- index to recurring error structure for regen in humrat
    INTEGER                      :: T_ProcInTempErrIndex         = 0    !- index to recurring error structure for process in temp
    INTEGER                      :: T_ProcInHumRatErrIndex       = 0    !- index to recurring error structure for process in humrat
    INTEGER                      :: T_FaceVelocityErrIndex       = 0    !- index to recurring err struc for proc and regen face vel
    INTEGER                      :: RegenOutTempErrorCount       = 0    !- counter if regen outlet temp limits are exceeded
    INTEGER                      :: RegenOutTempErrIndex         = 0    !- index to recurring error structure for regen outlet temp

    ! used when regen outlet temperature is above regen inlet temperature, verify coefficients warning issued
    INTEGER                      :: RegenOutTempFailedErrorCount = 0    !- counter if regen outlet temp limits are exceeded
    INTEGER                      :: RegenOutTempFailedErrIndex   = 0    !- index to recurring error structure for regen outlet temp
    CHARACTER(len=400)           :: RegenOutTempFailedBuffer1 = ' '  !- buffer for RegenOutTemp warn messages on following timestep
    CHARACTER(len=400)           :: RegenOutTempFailedBuffer2 = ' '  !- buffer for RegenOutTemp warn messages on following timestep
    CHARACTER(len=400)           :: RegenOutTempFailedBuffer3 = ' '  !- buffer for RegenOutTemp warn messages on following timestep
    REAL(r64)                    :: RegenOutTempFailedLast    = 0.0d0  !- last value of regen outlet temp


    ! regen outlet hum rat eqn
    INTEGER                      :: H_RegenInTempErrorCount      = 0    !- counter if regen inlet temp limits are exceeded
    INTEGER                      :: H_RegenInHumRatErrorCount    = 0    !- counter if regen inlet hum rat limits are exceeded
    INTEGER                      :: H_ProcInTempErrorCount       = 0    !- counter if process inlet temperature limits are exceeded
    INTEGER                      :: H_ProcInHumRatErrorCount     = 0    !- counter if process inlet hum rat limits are exceeded
    INTEGER                      :: H_FaceVelErrorCount          = 0    !- counter if regen and proc face vel limits are exceeded
    INTEGER                      :: H_RegenInTempErrIndex        = 0    !- index to recurring error structure for regen inlet temp
    INTEGER                      :: H_RegenInHumRatErrIndex      = 0    !- index to recurring error struc for regen inlet humrat
    INTEGER                      :: H_ProcInTempErrIndex         = 0    !- index to recurring error struc for process inlet temp
    INTEGER                      :: H_ProcInHumRatErrIndex       = 0    !- index to recurring error struc for process inlet hum rat
    INTEGER                      :: H_FaceVelocityErrIndex       = 0    !- index to recurring err struc for proc and regen face vel
    INTEGER                      :: RegenOutHumRatErrorCount     = 0    !- counter if regen outlet temp limits are exceeded
    INTEGER                      :: RegenOutHumRatErrIndex       = 0    !- index to recurring error struc for regen outlet hum rat
    INTEGER                      :: RegenInHumRatErrorCount      = 0    !- counter if regen outlet temp limits are exceeded
    INTEGER                      :: RegenInHumRatErrIndex        = 0    !- index to recurring error struc for regen outlet hum rat

    ! regen outlet temp variables                                   !- T_RegenInTemp = Regen inlet temperature
    CHARACTER(len=400)           :: T_RegenInTempBuffer1     = ' '  !- buffer for T_RegenInTemp warn message on following timestep
    CHARACTER(len=400)           :: T_RegenInTempBuffer2     = ' '  !- buffer for T_RegenInTemp warn message on following timestep
    CHARACTER(len=400)           :: T_RegenInTempBuffer3     = ' '  !- buffer for T_RegenInTemp warn message on following timestep
    REAL(r64)                    :: T_RegenInTempLast        = 0.0d0  !- last value of regen inlet temp
                                                                    !- T_RegenInHumRat = Regen inlet humidity ratio
    CHARACTER(len=400)           :: T_RegenInHumRatBuffer1   = ' '  !- buffer for T_RegenInHumRat warn messag on following timestep
    CHARACTER(len=400)           :: T_RegenInHumRatBuffer2   = ' '  !- buffer for T_RegenInHumRat warn messag on following timestep
    CHARACTER(len=400)           :: T_RegenInHumRatBuffer3   = ' '  !- buffer for T_RegenInHumRat warn messag on following timestep
    REAL(r64)                    :: T_RegenInHumRatLast      = 0.0d0  !- last value of regen inlet humidity ratio
                                                                    !- T_ProcInTemp = Process inlet temperature
    CHARACTER(len=400)           :: T_ProcInTempBuffer1      = ' '  !- buffer for T_ProcInTemp warning messag on following timestep
    CHARACTER(len=400)           :: T_ProcInTempBuffer2      = ' '  !- buffer for T_ProcInTemp warning messag on following timestep
    CHARACTER(len=400)           :: T_ProcInTempBuffer3      = ' '  !- buffer for T_ProcInTemp warning messag on following timestep
    REAL(r64)                    :: T_ProcInTempLast         = 0.0d0  !- last value of process inlet temp
                                                                    !- T_ProcInHumRat = Process inlet humidity ratio
    CHARACTER(len=400)           :: T_ProcInHumRatBuffer1    = ' '  !- buffer for T_ProcInHumRat warn message on following timestep
    CHARACTER(len=400)           :: T_ProcInHumRatBuffer2    = ' '  !- buffer for T_ProcInHumRat warn message on following timestep
    CHARACTER(len=400)           :: T_ProcInHumRatBuffer3    = ' '  !- buffer for T_ProcInHumRat warn message on following timestep
    REAL(r64)                    :: T_ProcInHumRatLast       = 0.0d0  !- last value of process inlet humidity ratio
                                                                    !- T_FaceVel = Process and regen face velocity
    CHARACTER(len=400)           :: T_FaceVelBuffer1         = ' '  !- buffer for T_FaceVel warning messages on following time step
    CHARACTER(len=400)           :: T_FaceVelBuffer2         = ' '  !- buffer for T_FaceVel warning messages on following time step
    CHARACTER(len=400)           :: T_FaceVelBuffer3         = ' '  !- buffer for T_FaceVel warning messages on following time step
    REAL(r64)                    :: T_FaceVelLast            = 0.0d0  !- last value of process and regen face velocity
                                                                    !- T_RegenOutTemp = Regen outlet temperature
    CHARACTER(len=400)           :: RegenOutTempBuffer1      = ' '  !- buffer for RegenOutTemp warn messages on following timestep
    CHARACTER(len=400)           :: RegenOutTempBuffer2      = ' '  !- buffer for RegenOutTemp warn messages on following timestep
    CHARACTER(len=400)           :: RegenOutTempBuffer3      = ' '  !- buffer for RegenOutTemp warn messages on following timestep
    REAL(r64)                    :: RegenOutTempLast         = 0.0d0  !- last value of regen outlet temp

    ! regen outlet humidity ratio variables                         !- H_RegenInTemp = Regen inlet temperature
    CHARACTER(len=400)           :: H_RegenInTempBuffer1     = ' '  !- buffer for H_RegenInTemp warn message on following time step
    CHARACTER(len=400)           :: H_RegenInTempBuffer2     = ' '  !- buffer for H_RegenInTemp warn message on following time step
    CHARACTER(len=400)           :: H_RegenInTempBuffer3     = ' '  !- buffer for H_RegenInTemp warn message on following time step
    REAL(r64)                    :: H_RegenInTempLast        = 0.0d0  !- last value of regen inlet temp
                                                                    !- H_RegenInHumRat = Regen inlet humidity ratio
    CHARACTER(len=400)           :: H_RegenInHumRatBuffer1   = ' '  !- buffer for H_RegenInHumRat warn messag on following timestep
    CHARACTER(len=400)           :: H_RegenInHumRatBuffer2   = ' '  !- buffer for H_RegenInHumRat warn messag on following timestep
    CHARACTER(len=400)           :: H_RegenInHumRatBuffer3   = ' '  !- buffer for H_RegenInHumRat warn messag on following timestep
    REAL(r64)                    :: H_RegenInHumRatLast      = 0.0d0  !- last value of regen inlet humidity ratio
                                                                    !- H_ProcInTemp = Process inlet temperature
    CHARACTER(len=400)           :: H_ProcInTempBuffer1      = ' '  !- buffer for H_ProcInTemp warn messages on following time step
    CHARACTER(len=400)           :: H_ProcInTempBuffer2      = ' '  !- buffer for H_ProcInTemp warn messages on following time step
    CHARACTER(len=400)           :: H_ProcInTempBuffer3      = ' '  !- buffer for H_ProcInTemp warn messages on following time step
    REAL(r64)                    :: H_ProcInTempLast         = 0.0d0  !- last value of process inlet temp
                                                                    !- H_ProcInHumRat = Process inlet humidity ratio
    CHARACTER(len=400)           :: H_ProcInHumRatBuffer1    = ' '  !- buffer for H_ProcInHumRat warn message on following timestep
    CHARACTER(len=400)           :: H_ProcInHumRatBuffer2    = ' '  !- buffer for H_ProcInHumRat warn message on following timestep
    CHARACTER(len=400)           :: H_ProcInHumRatBuffer3    = ' '  !- buffer for H_ProcInHumRat warn message on following timestep
    REAL(r64)                    :: H_ProcInHumRatLast       = 0.0d0  !- last value of process inlet humidity ratio
                                                                    !- H_FaceVel = Process and regen face velocity
    CHARACTER(len=400)           :: H_FaceVelBuffer1         = ' '  !- buffer for H_FaceVel warning messages on following time step
    CHARACTER(len=400)           :: H_FaceVelBuffer2         = ' '  !- buffer for H_FaceVel warning messages on following time step
    CHARACTER(len=400)           :: H_FaceVelBuffer3         = ' '  !- buffer for H_FaceVel warning messages on following time step
    REAL(r64)                    :: H_FaceVelLast            = 0.0d0  !- last value of process and regen face velocity
                                                                    !- H_RegenOutTemp = Regen outlet temperature
    CHARACTER(len=400)           :: RegenOutHumRatBuffer1    = ' '  !- buffer for RegenOutHumRat warn message on following timestep
    CHARACTER(len=400)           :: RegenOutHumRatBuffer2    = ' '  !- buffer for RegenOutHumRat warn message on following timestep
    CHARACTER(len=400)           :: RegenOutHumRatBuffer3    = ' '  !- buffer for RegenOutHumRat warn message on following timestep
    REAL(r64)                    :: RegenOutHumRatLast       = 0.0d0  !- last value of regen outlet humidity ratio
  END TYPE BalancedDesDehumPerfData

  ! MODULE VARIABLE DECLARATIONS:
  INTEGER :: NumHeatExchangers           =0 ! number of heat exchangers
  INTEGER :: NumAirToAirPlateExchs       =0 ! number of air to air plate heat exchangers
  INTEGER :: NumAirToAirGenericExchs     =0 ! number of air to air generic heat exchangers
  INTEGER :: NumDesiccantBalancedExchs   =0 ! number of desiccant balanced heat exchangers
  INTEGER :: NumDesBalExchsPerfDataType1 =0 ! number of desiccant balanced heat exchanger performance data maps
  REAL(r64)    :: FullLoadOutAirTemp          =0.0d0 ! Used with desiccant HX empirical model, water coils use inlet node condition
                                              ! DX coils use DXCoilFullLoadOutAirTemp when coil is ON otherwise inlet node
  REAL(r64)    :: FullLoadOutAirHumRat        =0.0d0 ! Used with desiccant HX empirical model, water coils use inlet node condition
                                              ! DX coils use DXCoilFullLoadOutAirHumRat when coil is ON otherwise inlet node
  LOGICAL :: GetInputFlag            = .TRUE. ! First time, input is "gotten"
  LOGICAL :: CalledFromParentObject  = .TRUE. ! Indicates that HX is called from parent object (this object is not on a branch)
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckEquipName

  TYPE (HeatExchCond),             ALLOCATABLE, DIMENSION(:) :: ExchCond
  TYPE (BalancedDesDehumPerfData), ALLOCATABLE, DIMENSION(:) :: BalDesDehumPerfData

  ! SUBROUTINE SPECIFICATIONS FOR MODULE:

          ! Driver/Manager Routines
  PUBLIC  SimHeatRecovery

          ! Get Input routines for module
  PRIVATE GetHeatRecoveryInput

          ! Initialization routines for module
  PRIVATE InitHeatRecovery

     ! Sizing routine for the module
  PRIVATE SizeHeatRecovery

          ! Update routines to check convergence and update nodes
  PRIVATE CalcAirToAirPlateHeatExch
  PRIVATE CalcAirToAirGenericHeatExch
  PRIVATE CalcDesiccantBalancedHeatExch
  PRIVATE UpdateHeatRecovery
  PRIVATE ReportHeatRecovery

          ! Common routines
  PRIVATE SafeDiv
  PRIVATE CalculateEpsFromNTUandZ
  PRIVATE CalculateNTUfromEpsAndZ
  PRIVATE GetNTUforCrossFlowBothUnmixed
  PRIVATE GetResidCrossFlowBothUnmixed
  PRIVATE CheckModelBoundsTempEq
  PRIVATE CheckModelBoundsHumRatEq
  PRIVATE CheckModelBoundOutput_Temp
  PRIVATE CheckModelBoundOutput_HumRat
  PRIVATE CheckModelBoundsRH_TempEq
  PRIVATE CheckModelBoundsRH_HumRatEq
  PRIVATE CheckForBalancedFlow

          ! External function calls
  PUBLIC  GetSupplyInletNode
  PUBLIC  GetSupplyOutletNode
  PUBLIC  GetSecondaryInletNode
  PUBLIC  GetSecondaryOutletNode
  PUBLIC  GetSupplyAirFlowRate
  PUBLIC  GetHeatExchangerObjectTypeNum
  PUBLIC  SetHeatExchangerData

CONTAINS

SUBROUTINE SimHeatRecovery(CompName,FirstHVACIteration,CompIndex,FanOpMode,HXPartLoadRatio,HXUnitEnable, &
                           CompanionCoilIndex,RegenInletIsOANode,EconomizerFlag,HighHumCtrlFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   March 1999
          !       MODIFIED       Fred Buhl November 2000, R. Raustad FSEC - Feb 2009
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Manage the simulation of a heat recovery unit

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! NA

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General,        ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*),    INTENT (IN)   :: CompName            ! name of the heat exchanger unit
  LOGICAL,             INTENT (IN)   :: FirstHVACIteration  ! TRUE if 1st HVAC simulation of system timestep
  INTEGER,             INTENT(INOUT) :: CompIndex           ! Pointer to Component
  INTEGER,             INTENT (IN)   :: FanOpMode           ! Supply air fan operating mode
  REAL(r64), OPTIONAL, INTENT (IN)   :: HXPartLoadRatio     ! Part load ratio requested of DX compressor
  LOGICAL,   OPTIONAL, INTENT (IN)   :: HXUnitEnable        ! Flag to operate heat exchanger
  INTEGER,   OPTIONAL, INTENT (IN)   :: CompanionCoilIndex  ! index of companion cooling coil
  LOGICAL,   OPTIONAL, INTENT (IN)   :: RegenInletIsOANode  ! flag to determine if supply inlet is OA node, if so air flow cycles
  LOGICAL,   OPTIONAL, INTENT (IN)   :: EconomizerFlag      ! economizer operation flag passed by airloop or OA sys
  LOGICAL,   OPTIONAL, INTENT (IN)   :: HighHumCtrlFlag     ! high humidity control flag passed by airloop or OA sys

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      :: HeatExchNum               ! index of unit being simulated
  LOGICAL      :: HXUnitOn                  ! flag to enable heat exchanger
!unused0509  INTEGER      :: FanModeOperation          ! supply air fan operating mode
  REAL(r64)    :: PartLoadRatio             ! Part load ratio requested of DX compressor
  LOGICAL      :: RegInIsOANode             ! local variable to set RegenInletIsOANode optional argument
  INTEGER      :: CompanionCoilNum          ! Index to companion cooling coil

  IF (GetInputFlag) THEN
    CALL GetHeatRecoveryInput
    GetInputFlag=.FALSE.
  ENDIF

  ! Find the correct unit index
  IF (CompIndex == 0) THEN
    HeatExchNum = FindItemInList(CompName,ExchCond%Name,NumHeatExchangers)
    IF (HeatExchNum == 0) THEN
      CALL ShowFatalError('SimHeatRecovery: Unit not found='//TRIM(CompName))
    ENDIF
    CompIndex=HeatExchNum
  ELSE
    HeatExchNum=CompIndex
    IF (HeatExchNum > NumHeatExchangers .or. HeatExchNum < 1) THEN
      CALL ShowFatalError('SimHeatRecovery:  Invalid CompIndex passed='//  &
                          TRIM(TrimSigDigits(HeatExchNum))// &
                          ', Number of Units='//TRIM(TrimSigDigits(NumHeatExchangers))//  &
                          ', Entered Unit name='//TRIM(CompName))
    ENDIF
    IF (CheckEquipName(HeatExchNum)) THEN
      IF (CompName /= ExchCond(HeatExchNum)%Name) THEN
        CALL ShowFatalError('SimHeatRecovery: Invalid CompIndex passed='//  &
                            TRIM(TrimSigDigits(HeatExchNum))// &
                            ', Unit name='//TRIM(CompName)//', stored Unit Name for that index='//  &
                            TRIM(ExchCond(HeatExchNum)%Name))
      ENDIF
      CheckEquipName(HeatExchNum)=.false.
    ENDIF
  ENDIF

  IF(PRESENT(CompanionCoilIndex))THEN
    CompanionCoilNum = CompanionCoilIndex
  ELSE
    CompanionCoilNum = 0
  END IF

  IF( PRESENT(HXUnitEnable))THEN
    HXUnitOn = HXUnitEnable
!   When CalledFromParentObject is TRUE, this SIM routine was called by a parent object that passed in HXUnitEnable.
!   HX will use the DX coil part-load ratio (optional CompanionCoilIndex must be present) or PLR passed in if
!   not used with DX coil (optional CompanionCoilIndex must not be present).
    CalledFromParentObject = .TRUE.
  ELSE
!   HX is placed on a BRANCH, optional arguments are not passed in from SimAirServingZones.
!   HX will calculate its own part-load ratio if optional HXUnitEnable flag is not present
    HXUnitOn = .TRUE.
    CalledFromParentObject = .FALSE.
  END IF

  CALL InitHeatRecovery(HeatExchNum, CompanionCoilNum)

  ! call the correct heat exchanger calculation routine
  SELECT CASE(ExchCond(HeatExchNum)%ExchTypeNum)

    CASE (HX_AIRTOAIR_FLATPLATE)

      CALL CalcAirToAirPlateHeatExch(HeatExchNum, HXUnitOn, EconomizerFlag, HighHumCtrlFlag)

    CASE (HX_AIRTOAIR_GENERIC)

      CALL CalcAirToAirGenericHeatExch(HeatExchNum, HXUnitOn, FirstHVACIteration, EconomizerFlag, HighHumCtrlFlag)

    CASE (HX_DESICCANT_BALANCED)

      IF(PRESENT(HXPartLoadRatio))THEN
        PartLoadRatio = HXPartLoadRatio
      ELSE
        PartLoadRatio = 1.0d0
      END IF

      IF(PRESENT(RegenInletIsOANode))THEN
        RegInIsOANode = RegenInletIsOANode
      ELSE
        RegInIsOANode = .FALSE.
      END IF

      CALL CalcDesiccantBalancedHeatExch(HeatExchNum, HXUnitOn, FirstHVACIteration, FanOpMode, &
                                         PartLoadRatio, CompanionCoilNum, RegInIsOANode, EconomizerFlag, HighHumCtrlFlag)

  END SELECT

  CALL UpdateHeatRecovery(HeatExchNum)

  CALL ReportHeatRecovery(HeatExchNum)

RETURN
END SUBROUTINE SimHeatRecovery

SUBROUTINE GetHeatRecoveryInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   March 1999
          !       MODIFIED       F Buhl Nov 2000, D Shirey Feb 2003, R. Raustad FSEC - Feb 2009 (EconoLockout inputs)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtains input data for heat recovery units and stores it in
          ! appropriate data structures.

          ! METHODOLOGY EMPLOYED:
          ! Uses InputProcessor "Get" routines to obtain data.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName
  USE NodeInputManager, ONLY: GetOnlySingleNode
  USE BranchNodeConnections, ONLY: TestCompSet
  USE DataIPShortCuts

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
  INTEGER                        :: ExchIndex      ! loop index
  INTEGER                        :: ExchNum        ! current heat exchanger number
  INTEGER                        :: PerfDataIndex  ! desiccant balance heat exchanger performance data loop index
  INTEGER                        :: PerfDataNum    ! current desiccant balanced heat exchanger performance data set number
  INTEGER                        :: NumAlphas     ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers    ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: IOStatus      ! Used in GetObjectItem
  LOGICAL                 :: ErrorsFound=.false.  ! Set to true if errors in input, fatal at end of routine
  LOGICAL                        :: IsNotOK       ! Flag to verify name
  LOGICAL                        :: IsBlank       ! Flag for blank name
  CHARACTER(len=MaxNameLength)   :: HeatExchPerfType =' ' ! Desiccant balanced heat exchanger performance data type
  CHARACTER(len=*), PARAMETER    :: RoutineName='GetHeatRecoveryInput: ' ! include trailing blank space


  NumAirToAirPlateExchs = GetNumObjectsFound('HeatExchanger:AirToAir:FlatPlate')
  NumAirToAirGenericExchs = GetNumObjectsFound('HeatExchanger:AirToAir:SensibleAndLatent')
  NumDesiccantBalancedExchs = GetNumObjectsFound('HeatExchanger:Desiccant:BalancedFlow')
  NumDesBalExchsPerfDataType1 = GetNumObjectsFound('HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1')
  NumHeatExchangers = NumAirToAirPlateExchs + NumAirToAirGenericExchs + NumDesiccantBalancedExchs

  ! allocate the data array
  ALLOCATE(ExchCond(NumHeatExchangers))
  ALLOCATE(CheckEquipName(NumHeatExchangers))
  CheckEquipName=.true.

  IF (NumDesBalExchsPerfDataType1 .GT. 0) THEN
    ALLOCATE(BalDesDehumPerfData(NumDesBalExchsPerfDataType1))
  END IF

  ! loop over the air to air plate heat exchangers and load their input data
  DO ExchIndex = 1,NumAirToAirPlateExchs
    cCurrentModuleObject='HeatExchanger:AirToAir:FlatPlate'
    CALL GetObjectItem(cCurrentModuleObject,ExchIndex,cAlphaArgs,NumAlphas,&
                       rNumericArgs,NumNumbers,IOStatus,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    ExchNum = ExchIndex
    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(cAlphaArgs(1),ExchCond%Name,ExchNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    ExchCond(ExchNum)%Name = cAlphaArgs(1)
    ExchCond(ExchNum)%ExchTypeNum = HX_AIRTOAIR_FLATPLATE
    IF (lAlphaFieldBlanks(2)) THEN
      ExchCond(ExchNum)%SchedPtr = ScheduleAlwaysOn
    ELSE
      ExchCond(ExchNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(2))
      IF (ExchCond(ExchNum)%SchedPtr .EQ. 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(2))//  &
           ' entered ='//TRIM(cAlphaArgs(2))// &
           ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.TRUE.
      END IF
    END IF
    SELECT CASE(TRIM(cAlphaArgs(3)))
      CASE('COUNTERFLOW')
        ExchCond(ExchNum)%FlowArr = Counter_Flow
      CASE('PARALLELFLOW')
        ExchCond(ExchNum)%FlowArr = Parallel_Flow
      CASE('CROSSFLOWBOTHUNMIXED')
        ExchCond(ExchNum)%FlowArr = Cross_Flow_Both_Unmixed
      CASE DEFAULT
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': incorrect flow arrangement: '//TRIM(cAlphaArgs(3)))
        ErrorsFound=.true.
    END SELECT
    SELECT CASE(TRIM(cAlphaArgs(4)))
      CASE('YES')
        ExchCond(ExchNum)%EconoLockOut = EconoLockOut_Yes
      CASE('NO')
        ExchCond(ExchNum)%EconoLockOut = EconoLockOut_No
      CASE DEFAULT
        IF(lAlphaFieldBlanks(4))THEN
          ExchCond(ExchNum)%EconoLockOut = EconoLockOut_Yes
        ELSE
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//': incorrect econo lockout: '//TRIM(cAlphaArgs(4)))
          ErrorsFound=.true.
        END IF
    END SELECT
    ExchCond(ExchNum)%hARatio           = rNumericArgs(1)
    ExchCond(ExchNum)%NomSupAirVolFlow  = rNumericArgs(2)
    ExchCond(ExchNum)%NomSupAirInTemp   = rNumericArgs(3)
    ExchCond(ExchNum)%NomSupAirOutTemp  = rNumericArgs(4)
    ExchCond(ExchNum)%NomSecAirVolFlow  = rNumericArgs(5)
    ExchCond(ExchNum)%NomSecAirInTemp   = rNumericArgs(6)
    ExchCond(ExchNum)%NomElecPower      = rNumericArgs(7)
    ExchCond(ExchNum)%SupInletNode = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
    ExchCond(ExchNum)%SupOutletNode = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)
    ExchCond(ExchNum)%SecInletNode = &
               GetOnlySingleNode(cAlphaArgs(7),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Inlet,2,ObjectIsNotParent)
    ExchCond(ExchNum)%SecOutletNode = &
               GetOnlySingleNode(cAlphaArgs(8),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Outlet,2,ObjectIsNotParent)

    CALL TestCompSet(cHXTypes(ExchCond(ExchNum)%ExchTypeNum),ExchCond(ExchNum)%Name,cAlphaArgs(5), &
                     cAlphaArgs(6),'Process Air Nodes')

  END DO ! end of input loop over air to air plate heat exchangers

  ! loop over the air to air generic heat exchangers and load their input data
  DO ExchIndex = 1,NumAirToAirGenericExchs
    cCurrentModuleObject='HeatExchanger:AirToAir:SensibleAndLatent'
    CALL GetObjectItem(cCurrentModuleObject,ExchIndex,cAlphaArgs,NumAlphas,&
                       rNumericArgs,NumNumbers,IOStatus,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    ExchNum = ExchIndex+NumAirToAirPlateExchs
    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(cAlphaArgs(1),ExchCond%Name,ExchNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    ExchCond(ExchNum)%Name = cAlphaArgs(1)
    ExchCond(ExchNum)%ExchTypeNum = HX_AIRTOAIR_GENERIC
    IF (lAlphaFieldBlanks(2)) THEN
      ExchCond(ExchNum)%SchedPtr = ScheduleAlwaysOn
    ELSE
      ExchCond(ExchNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(2))
      IF (ExchCond(ExchNum)%SchedPtr .EQ. 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(2))//  &
           ' entered ='//TRIM(cAlphaArgs(2))// &
           ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.TRUE.
      END IF
    END IF
    ExchCond(ExchNum)%NomSupAirVolFlow      = rNumericArgs(1)
    ExchCond(ExchNum)%HeatEffectSensible100 = rNumericArgs(2)
    ExchCond(ExchNum)%HeatEffectLatent100   = rNumericArgs(3)
    ExchCond(ExchNum)%HeatEffectSensible75  = rNumericArgs(4)
    ExchCond(ExchNum)%HeatEffectLatent75    = rNumericArgs(5)
    IF (ExchCond(ExchNum)%HeatEffectSensible75 .LT. ExchCond(ExchNum)%HeatEffectSensible100) THEN
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//' "'//TRIM(ExchCond(ExchNum)%Name)//&
                             '" sensible heating effectiveness at 75% rated flow is less than at 100% rated flow.')
      CALL ShowContinueError('Sensible heating effectiveness at 75% rated flow is usually greater than at 100% rated flow.')
    END IF
    IF (ExchCond(ExchNum)%HeatEffectLatent75 .LT. ExchCond(ExchNum)%HeatEffectLatent100) THEN
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//' "'//TRIM(ExchCond(ExchNum)%Name)//&
                             '" latent heating effectiveness at 75% rated flow is less than at 100% rated flow.')
      CALL ShowContinueError('Latent heating effectiveness at 75% rated flow is usually greater than at 100% rated flow.')
    END IF
    ExchCond(ExchNum)%CoolEffectSensible100 = rNumericArgs(6)
    ExchCond(ExchNum)%CoolEffectLatent100   = rNumericArgs(7)
    ExchCond(ExchNum)%CoolEffectSensible75  = rNumericArgs(8)
    ExchCond(ExchNum)%CoolEffectLatent75    = rNumericArgs(9)
    IF (ExchCond(ExchNum)%CoolEffectSensible75 .LT. ExchCond(ExchNum)%CoolEffectSensible100) THEN
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//' "'//TRIM(ExchCond(ExchNum)%Name)//&
                             '" sensible cooling effectiveness at 75% rated flow is less than at 100% rated flow.')
      CALL ShowContinueError('Sensible cooling effectiveness at 75% rated flow is usually greater than at 100% rated flow.')
    END IF
    IF (ExchCond(ExchNum)%CoolEffectLatent75 .LT. ExchCond(ExchNum)%CoolEffectLatent100) THEN
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//' "'//TRIM(ExchCond(ExchNum)%Name)//&
                             '" latent cooling effectiveness at 75% rated flow is less than at 100% rated flow.')
      CALL ShowContinueError('Latent cooling effectiveness at 75% rated flow is usually greater than at 100% rated flow.')
    END IF
    ExchCond(ExchNum)%SupInletNode = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
    ExchCond(ExchNum)%SupOutletNode = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)
    ExchCond(ExchNum)%SecInletNode = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Inlet,2,ObjectIsNotParent)
    ExchCond(ExchNum)%SecOutletNode = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Outlet,2,ObjectIsNotParent)

    ExchCond(ExchNum)%NomElecPower = rNumericArgs(10)

    IF(SameString(cAlphaArgs(7),'Yes')) THEN
     ExchCond(ExchNum)%ControlToTemperatureSetpoint = .TRUE.
    ELSE
     IF(.NOT. SameString(cAlphaArgs(7),'No')) THEN
       CALL ShowSevereError('Rotary HX Speed Modulation or Plate Bypass for Temperature Control for ')
       CALL ShowContinueError(TRIM(ExchCond(ExchNum)%Name)//' must be set to Yes or No')
       ErrorsFound = .TRUE.
     END IF
    END IF

    IF (SameString(cAlphaArgs(8),'Plate')) THEN
      ExchCond(ExchNum)%ExchConfigNum = Plate
    ELSE IF(SameString(cAlphaArgs(8),'Rotary')) THEN
      ExchCond(ExchNum)%ExchConfigNum = Rotary
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' configuration not found= '//TRIM(cAlphaArgs(8)))
      CALL ShowContinueError('HX configuration must be either Plate or Rotary')
        ErrorsFound=.TRUE.
    END IF

! Added additional inputs for frost control
    ExchCond(ExchNum)%FrostControlType = cAlphaArgs(9)
    IF(.NOT. SameString(ExchCond(ExchNum)%FrostControlType,'None')) THEN
     IF(.NOT. SameString(ExchCond(ExchNum)%FrostControlType,'ExhaustOnly')) THEN
      IF(.NOT. SameString(ExchCond(ExchNum)%FrostControlType,'ExhaustAirRecirculation')) THEN
       IF(.NOT. SameString(ExchCond(ExchNum)%FrostControlType,'MinimumExhaustTemperature')) THEN
        CALL ShowSevereError('Invalid Frost Control method for '//TRIM(ExchCond(ExchNum)%Name)//' =  '//TRIM(cAlphaArgs(9)))
        ErrorsFound = .TRUE.
       END IF
      END IF
     END IF
    END IF

    IF(.not. SameString(cAlphaArgs(9),'None'))THEN
     ExchCond(ExchNum)%ThresholdTemperature      = rNumericArgs(11)
     ExchCond(ExchNum)%InitialDefrostTime        = rNumericArgs(12)
     ExchCond(ExchNum)%RateofDefrostTimeIncrease = rNumericArgs(13)
    END IF

    SELECT CASE(TRIM(cAlphaArgs(10)))
      CASE('YES')
        ExchCond(ExchNum)%EconoLockOut = EconoLockOut_Yes
      CASE('NO')
        ExchCond(ExchNum)%EconoLockOut = EconoLockOut_No
      CASE DEFAULT
        IF(lAlphaFieldBlanks(10))THEN
          ExchCond(ExchNum)%EconoLockOut = EconoLockOut_Yes
        ELSE
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//': incorrect econo lockout: '//TRIM(cAlphaArgs(10)))
          ErrorsFound=.true.
        END IF
    END SELECT

    CALL TestCompSet(cHXTypes(ExchCond(ExchNum)%ExchTypeNum),ExchCond(ExchNum)%Name,cAlphaArgs(3), &
                     cAlphaArgs(4),'Process Air Nodes')
  END DO ! end of input loop over air to air generic heat exchangers

! loop over the desiccant balanced heat exchangers and load their input data
  DO ExchIndex = 1,NumDesiccantBalancedExchs
    cCurrentModuleObject = 'HeatExchanger:Desiccant:BalancedFlow'
    CALL GetObjectItem(cCurrentModuleObject,ExchIndex,cAlphaArgs,NumAlphas,&
                       rNumericArgs,NumNumbers,IOStatus,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                       AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    ExchNum = ExchIndex+NumAirToAirPlateExchs+NumAirToAirGenericExchs
    IsNotOK=.FALSE.
    IsBlank=.FALSE.
    CALL VerifyName(cAlphaArgs(1),ExchCond%Name,ExchNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (IsNotOK) THEN
      ErrorsFound=.true.
      IF (IsBlank) cAlphaArgs(1)='xxxxx'
    ENDIF
    ExchCond(ExchNum)%Name = cAlphaArgs(1)
    ExchCond(ExchNum)%ExchTypeNum = HX_DESICCANT_BALANCED
    IF (lAlphaFieldBlanks(2)) THEN
      ExchCond(ExchNum)%SchedPtr = ScheduleAlwaysOn
    ELSE
      ExchCond(ExchNum)%SchedPtr = GetScheduleIndex(cAlphaArgs(2))
      IF (ExchCond(ExchNum)%SchedPtr .EQ. 0) THEN
        CALL ShowSevereError(RoutineName//TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(2))//  &
           ' entered ='//TRIM(cAlphaArgs(2))// &
           ' for '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1)))
        ErrorsFound=.TRUE.
      END IF
    END IF
    ! desiccant HX's usually refer to process and regeneration air streams
    ! In this module, Sup = Regeneration nodes and Sec = Process nodes
    ! regeneration air inlet and outlet nodes
    ExchCond(ExchNum)%SupInletNode = &
               GetOnlySingleNode(cAlphaArgs(3),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Inlet,1,ObjectIsNotParent)
    ExchCond(ExchNum)%SupOutletNode = &
               GetOnlySingleNode(cAlphaArgs(4),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Outlet,1,ObjectIsNotParent)
    ! process air inlet and outlet nodes
    ExchCond(ExchNum)%SecInletNode = &
               GetOnlySingleNode(cAlphaArgs(5),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Inlet,2,ObjectIsNotParent)
    ExchCond(ExchNum)%SecOutletNode = &
               GetOnlySingleNode(cAlphaArgs(6),ErrorsFound,TRIM(cCurrentModuleObject),cAlphaArgs(1), &
               NodeType_Air,NodeConnectionType_Outlet,2,ObjectIsNotParent)

    ! Set up the component set for the process side of the HX (Sec = Process)
    CALL TestCompSet(cHXTypes(ExchCond(ExchNum)%ExchTypeNum),ExchCond(ExchNum)%Name,NodeID(ExchCond(ExchNum)%SecInletNode), &
                     NodeID(ExchCond(ExchNum)%SecOutletNode),'Process Air Nodes')

    HeatExchPerfType = cAlphaArgs(7)
    IF(SameString(HeatExchPerfType,'HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1'))THEN
       ExchCond(ExchNum)%HeatExchPerfTypeNum = BALANCEDHX_PERFDATATYPE1
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(ExchCond(ExchNum)%Name)//'"')
      CALL ShowContinueError('Invalid performance data type selected.')
      CALL ShowContinueError('...performance data type selected = '//TRIM(HeatExchPerfType))
      ErrorsFound=.TRUE.
    END IF

    ExchCond(ExchNum)%HeatExchPerfName = cAlphaArgs(8)

    SELECT CASE(TRIM(cAlphaArgs(9)))
      CASE('YES')
        ExchCond(ExchNum)%EconoLockOut = EconoLockOut_Yes
      CASE('NO')
        ExchCond(ExchNum)%EconoLockOut = EconoLockOut_No
      CASE DEFAULT
        IF(lAlphaFieldBlanks(9))THEN
          ExchCond(ExchNum)%EconoLockOut = EconoLockOut_No
        ELSE
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//': incorrect econo lockout: '//TRIM(cAlphaArgs(9)))
          ErrorsFound=.true.
        END IF
    END SELECT

  END DO ! end of input loop over desiccant balanced heat exchangers

  ! get performance data set for balanced desiccant heat exchanger

    DO PerfDataIndex = 1,NumDesBalExchsPerfDataType1
      cCurrentModuleObject = 'HeatExchanger:Desiccant:BalancedFlow:PerformanceDataType1'
      CALL GetObjectItem(cCurrentModuleObject,PerfDataIndex,cAlphaArgs,NumAlphas,&
                         rNumericArgs,NumNumbers,IOStatus,NumBlank=lNumericFieldBlanks,AlphaBlank=lAlphaFieldBlanks, &
                         AlphaFieldNames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      PerfDataNum = PerfDataIndex
      IsNotOK=.FALSE.
      IsBlank=.FALSE.
      CALL VerifyName(cAlphaArgs(1),BalDesDehumPerfData%Name,PerfDataNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound=.true.
        IF (IsBlank) cAlphaArgs(1)='xxxxx'
      ENDIF
      BalDesDehumPerfData(PerfDataNum)%Name     = cAlphaArgs(1)
      BalDesDehumPerfData(PerfDataNum)%PerfType = cCurrentModuleObject
      BalDesDehumPerfData(PerfDataNum)%NomSupAirVolFlow = rNumericArgs(1)
       ! check validity
      IF (BalDesDehumPerfData(PerfDataNum)%NomSupAirVolFlow .LE. 0.0d0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Nominal air flow rate must be greater than zero.')
        CALL ShowContinueError('... value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%NomSupAirVolFlow,6))
        ErrorsFound = .TRUE.
      END IF

      BalDesDehumPerfData(PerfDataNum)%NomProcAirFaceVel = rNumericArgs(2)
      ! check validity
      IF (BalDesDehumPerfData(PerfDataNum)%NomProcAirFaceVel .LE. 0.0d0 .OR. &
                                            BalDesDehumPerfData(PerfDataNum)%NomProcAirFaceVel .GT. 6.0d0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Nominal air face velocity cannot be less than or equal to zero or greater than 6 m/s.')
        CALL ShowContinueError('... value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%NomProcAirFaceVel,6))
        ErrorsFound = .TRUE.
      END IF
      BalDesDehumPerfData(PerfDataNum)%NomElecPower = rNumericArgs(3)
      ! check validity
      IF (BalDesDehumPerfData(PerfDataNum)%NomElecPower < 0.0d0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Nominal electric power cannot be less than zero.')
        CALL ShowContinueError('... value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%NomElecPower,6))
        ErrorsFound = .TRUE.
      END IF

      ! regen outlet temp variables
      BalDesDehumPerfData(PerfDataNum)%B1 = rNumericArgs(4)
      BalDesDehumPerfData(PerfDataNum)%B2 = rNumericArgs(5)
      BalDesDehumPerfData(PerfDataNum)%B3 = rNumericArgs(6)
      BalDesDehumPerfData(PerfDataNum)%B4 = rNumericArgs(7)
      BalDesDehumPerfData(PerfDataNum)%B5 = rNumericArgs(8)
      BalDesDehumPerfData(PerfDataNum)%B6 = rNumericArgs(9)
      BalDesDehumPerfData(PerfDataNum)%B7 = rNumericArgs(10)
      BalDesDehumPerfData(PerfDataNum)%B8 = rNumericArgs(11)

!     Check that the minimum is not greater than or equal to the maximum for each of the following model boundaries
      BalDesDehumPerfData(PerfDataNum)%T_MinRegenAirInHumRat = rNumericArgs(12)
      BalDesDehumPerfData(PerfDataNum)%T_MaxRegenAirInHumRat = rNumericArgs(13)
      IF(BalDesDehumPerfData(PerfDataNum)%T_MinRegenAirInHumRat .GE. BalDesDehumPerfData(PerfDataNum)%T_MaxRegenAirInHumRat)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min/max boundary for the regen outlet air temperature equation.')
        CALL ShowContinueError('... the minimum value of regeneration inlet air humidity ratio must be less than the maximum.')
        CALL ShowContinueError('... minimum value entered by user = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%T_MinRegenAirInHumRat,6))
        CALL ShowContinueError('... maximum value entered by user = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%T_MaxRegenAirInHumRat,6))
        ErrorsFound = .TRUE.
      END IF
      IF(BalDesDehumPerfData(PerfDataNum)%T_MinRegenAirInHumRat .LT. 0.0d0)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min boundary for the regen outlet air temperature equation.')
        CALL ShowContinueError('... the minimum value of regeneration inlet air humidity ratio must be greater than'// &
                               ' or equal to 0.')
        CALL ShowContinueError('... minimum value entered by user = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%T_MinRegenAirInHumRat,6))
        ErrorsFound = .TRUE.
      END IF
      IF(BalDesDehumPerfData(PerfDataNum)%T_MaxRegenAirInHumRat .GT. 1.0d0)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in max boundary for the regen outlet air temperature equation.')
        CALL ShowContinueError('... the maximum value of regeneration inlet air humidity ratio must be less than or equal to 1.')
        CALL ShowContinueError('... maximum value entered by user = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%T_MaxRegenAirInHumRat,6))
        ErrorsFound = .TRUE.
      END IF

      BalDesDehumPerfData(PerfDataNum)%T_MinRegenAirInTemp   = rNumericArgs(14)
      BalDesDehumPerfData(PerfDataNum)%T_MaxRegenAirInTemp   = rNumericArgs(15)
      IF(BalDesDehumPerfData(PerfDataNum)%T_MinRegenAirInTemp .GE. BalDesDehumPerfData(PerfDataNum)%T_MaxRegenAirInTemp)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min/max boundary for the regen outlet air temperature equation.')
        CALL ShowContinueError('... the minimum value of regeneration inlet air temperature must be less than the maximum.')
        CALL ShowContinueError('... minimum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%T_MinRegenAirInTemp,6))
        CALL ShowContinueError('... maximum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%T_MaxRegenAirInTemp,6))
        ErrorsFound = .TRUE.
      END IF

      BalDesDehumPerfData(PerfDataNum)%T_MinProcAirInHumRat  = rNumericArgs(16)
      BalDesDehumPerfData(PerfDataNum)%T_MaxProcAirInHumRat  = rNumericArgs(17)
      IF(BalDesDehumPerfData(PerfDataNum)%T_MinProcAirInHumRat .GE. BalDesDehumPerfData(PerfDataNum)%T_MaxProcAirInHumRat)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min/max boundary for the regen outlet air temperature equation.')
        CALL ShowContinueError('... the minimum value of process inlet air humidity ratio must be less than the maximum.')
        CALL ShowContinueError('... minimum value entered by user = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%T_MinProcAirInHumRat,6))
        CALL ShowContinueError('... maximum value entered by user = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%T_MaxProcAirInHumRat,6))
        ErrorsFound = .TRUE.
      END IF
      IF(BalDesDehumPerfData(PerfDataNum)%T_MinProcAirInHumRat .LT. 0.0d0)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min boundary for the regen outlet air temperature equation.')
        CALL ShowContinueError('... the minimum value of process inlet air humidity ratio must be greater than or equal to 0.')
        CALL ShowContinueError('... minimum value entered by user = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%T_MinProcAirInHumRat,6))
        ErrorsFound = .TRUE.
      END IF
      IF(BalDesDehumPerfData(PerfDataNum)%T_MaxProcAirInHumRat .GT. 1.0d0)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in max boundary for the regen outlet air temperature equation.')
        CALL ShowContinueError('... the maximum value of process inlet air humidity ratio must be less than or equal to 1.')
        CALL ShowContinueError('... maximum value entered by user = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%T_MaxProcAirInHumRat,6))
        ErrorsFound = .TRUE.
      END IF

      BalDesDehumPerfData(PerfDataNum)%T_MinProcAirInTemp    = rNumericArgs(18)
      BalDesDehumPerfData(PerfDataNum)%T_MaxProcAirInTemp    = rNumericArgs(19)
      IF(BalDesDehumPerfData(PerfDataNum)%T_MinProcAirInTemp .GE. BalDesDehumPerfData(PerfDataNum)%T_MaxProcAirInTemp)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min/max boundary for the regen outlet air temperature equation.')
        CALL ShowContinueError('... the minimum value of process inlet air temperature must be less than the maximum.')
        CALL ShowContinueError('... minimum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%T_MinProcAirInTemp,6))
        CALL ShowContinueError('... maximum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%T_MaxProcAirInTemp,6))
        ErrorsFound = .TRUE.
      END IF

      BalDesDehumPerfData(PerfDataNum)%T_MinFaceVel          = rNumericArgs(20)
      BalDesDehumPerfData(PerfDataNum)%T_MaxFaceVel          = rNumericArgs(21)
      IF(BalDesDehumPerfData(PerfDataNum)%T_MinFaceVel .GE. BalDesDehumPerfData(PerfDataNum)%T_MaxFaceVel)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min/max boundary for the regen outlet air temperature equation.')
        CALL ShowContinueError('... the minimum value of regen air velocity must be less than the maximum.')
        CALL ShowContinueError('... minimum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%T_MinFaceVel,6))
        CALL ShowContinueError('... maximum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%T_MaxFaceVel,6))
        ErrorsFound = .TRUE.
      END IF

      BalDesDehumPerfData(PerfDataNum)%MinRegenAirOutTemp    = rNumericArgs(22)
      BalDesDehumPerfData(PerfDataNum)%MaxRegenAirOutTemp    = rNumericArgs(23)
      IF(BalDesDehumPerfData(PerfDataNum)%MinRegenAirOutTemp .GE. BalDesDehumPerfData(PerfDataNum)%MaxRegenAirOutTemp)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min/max boundary for the regen outlet air temperature equation.')
        CALL ShowContinueError('... the minimum value of regen outlet air temperature must be less than the maximum.')
        CALL ShowContinueError('... minimum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%MinRegenAirOutTemp,6))
        CALL ShowContinueError('... maximum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%MaxRegenAirOutTemp,6))
        ErrorsFound = .TRUE.
      END IF

      BalDesDehumPerfData(PerfDataNum)%T_MinRegenAirInRelHum = rNumericArgs(24)/100.0d0
      BalDesDehumPerfData(PerfDataNum)%T_MaxRegenAirInRelHum = rNumericArgs(25)/100.0d0
      IF(BalDesDehumPerfData(PerfDataNum)%T_MinRegenAirInRelHum .GE. BalDesDehumPerfData(PerfDataNum)%T_MaxRegenAirInRelHum)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min/max boundary for the regen outlet air temperature equation.')
        CALL ShowContinueError('... the minimum value of regen inlet air relative humidity must be less than the maximum.')
        CALL ShowContinueError('... minimum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%T_MinRegenAirInRelHum*100.0d0,6))
        CALL ShowContinueError('... maximum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%T_MaxRegenAirInRelHum*100.0d0,6))
        ErrorsFound = .TRUE.
      END IF
      IF(BalDesDehumPerfData(PerfDataNum)%T_MinRegenAirInRelHum .LT. 0.0d0)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min boundary for the regen outlet air temperature equation.')
        CALL ShowContinueError('... the minimum value of regen inlet air relative humidity must be greater than or equal to 0.')
        CALL ShowContinueError('... minimum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%T_MinRegenAirInRelHum*100.0d0,6))
        ErrorsFound = .TRUE.
      END IF
      IF(BalDesDehumPerfData(PerfDataNum)%T_MaxRegenAirInRelHum .GT. 1.0d0)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in max boundary for the regen outlet air temperature equation.')
        CALL ShowContinueError('... the maximum value of regen inlet air relative humidity must be less than or equal to 100.')
        CALL ShowContinueError('... maximum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%T_MaxRegenAirInRelHum*100.0d0,6))
        ErrorsFound = .TRUE.
      END IF

      BalDesDehumPerfData(PerfDataNum)%T_MinProcAirInRelHum  = rNumericArgs(26)/100.0d0
      BalDesDehumPerfData(PerfDataNum)%T_MaxProcAirInRelHum  = rNumericArgs(27)/100.0d0
      IF(BalDesDehumPerfData(PerfDataNum)%T_MinProcAirInRelHum .GE. BalDesDehumPerfData(PerfDataNum)%T_MaxProcAirInRelHum)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min/max boundary for the regen outlet air temperature equation.')
        CALL ShowContinueError('... the minimum value of process inlet air relative humidity must be less than the maximum.')
        CALL ShowContinueError('... minimum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%T_MinProcAirInRelHum*100.0d0,6))
        CALL ShowContinueError('... maximum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%T_MaxProcAirInRelHum*100.0d0,6))
        ErrorsFound = .TRUE.
      END IF
      IF(BalDesDehumPerfData(PerfDataNum)%T_MinProcAirInRelHum .LT. 0.0d0)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min boundary for the regen outlet air temperature equation.')
        CALL ShowContinueError('... the minimum value of process inlet air relative humidity must be greater than or equal to 0.')
        CALL ShowContinueError('... minimum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%T_MinProcAirInRelHum*100.0d0,6))
        ErrorsFound = .TRUE.
      END IF
      IF(BalDesDehumPerfData(PerfDataNum)%T_MaxProcAirInRelHum .GT. 1.0d0)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in max boundary for the regen outlet air temperature equation.')
        CALL ShowContinueError('... the maximum value of process inlet air relative humidity must be less than or equal to 100.')
        CALL ShowContinueError('... maximum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%T_MaxProcAirInRelHum*100.0d0,6))
        ErrorsFound = .TRUE.
      END IF

      ! regen outlet humidity ratio variables
      BalDesDehumPerfData(PerfDataNum)%C1 = rNumericArgs(28)
      BalDesDehumPerfData(PerfDataNum)%C2 = rNumericArgs(29)
      BalDesDehumPerfData(PerfDataNum)%C3 = rNumericArgs(30)
      BalDesDehumPerfData(PerfDataNum)%C4 = rNumericArgs(31)
      BalDesDehumPerfData(PerfDataNum)%C5 = rNumericArgs(32)
      BalDesDehumPerfData(PerfDataNum)%C6 = rNumericArgs(33)
      BalDesDehumPerfData(PerfDataNum)%C7 = rNumericArgs(34)
      BalDesDehumPerfData(PerfDataNum)%C8 = rNumericArgs(35)

!     Check that the minimum is not greater than or equal to the maximum for each of the following model boundaries
      BalDesDehumPerfData(PerfDataNum)%H_MinRegenAirInHumRat = rNumericArgs(36)
      BalDesDehumPerfData(PerfDataNum)%H_MaxRegenAirInHumRat = rNumericArgs(37)
      IF(BalDesDehumPerfData(PerfDataNum)%H_MinRegenAirInHumRat .GE. BalDesDehumPerfData(PerfDataNum)%H_MaxRegenAirInHumRat)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min/max boundary for the regen outlet air humidity ratio equation.')
        CALL ShowContinueError('... the minimum value of regeneration inlet air humidity ratio must be less than the maximum.')
        CALL ShowContinueError('... minimum value entered by user = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%H_MinRegenAirInHumRat,6))
        CALL ShowContinueError('... maximum value entered by user = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%H_MaxRegenAirInHumRat,6))
        ErrorsFound = .TRUE.
      END IF
      IF(BalDesDehumPerfData(PerfDataNum)%H_MinRegenAirInHumRat .LT. 0.0d0)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min boundary for the regen outlet air humidity ratio equation.')
        CALL ShowContinueError('... the minimum value of regeneration inlet air humidity ratio must be greater than'// &
                               ' or equal to 0.')
        CALL ShowContinueError('... minimum value entered by user = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%H_MinRegenAirInHumRat,6))
        ErrorsFound = .TRUE.
      END IF
      IF(BalDesDehumPerfData(PerfDataNum)%H_MaxRegenAirInHumRat .GT. 1.0d0)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in max boundary for the regen outlet air humidity ratio equation.')
        CALL ShowContinueError('... the maximum value of regeneration inlet air humidity ratio must be less than or equal to 1.')
        CALL ShowContinueError('... maximum value entered by user = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%H_MaxRegenAirInHumRat,6))
        ErrorsFound = .TRUE.
      END IF

      BalDesDehumPerfData(PerfDataNum)%H_MinRegenAirInTemp   = rNumericArgs(38)
      BalDesDehumPerfData(PerfDataNum)%H_MaxRegenAirInTemp   = rNumericArgs(39)
      IF(BalDesDehumPerfData(PerfDataNum)%H_MinRegenAirInTemp .GE. BalDesDehumPerfData(PerfDataNum)%H_MaxRegenAirInTemp)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min/max boundary for the regen outlet air humidity ratio equation.')
        CALL ShowContinueError('... the minimum value of regeneration inlet air temperature must be less than the maximum.')
        CALL ShowContinueError('... minimum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%H_MinRegenAirInTemp,6))
        CALL ShowContinueError('... maximum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%H_MaxRegenAirInTemp,6))
        ErrorsFound = .TRUE.
      END IF

      BalDesDehumPerfData(PerfDataNum)%H_MinProcAirInHumRat  = rNumericArgs(40)
      BalDesDehumPerfData(PerfDataNum)%H_MaxProcAirInHumRat  = rNumericArgs(41)
      IF(BalDesDehumPerfData(PerfDataNum)%H_MinProcAirInHumRat .GE. BalDesDehumPerfData(PerfDataNum)%H_MaxProcAirInHumRat)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min/max boundary for the regen outlet air humidity ratio equation.')
        CALL ShowContinueError('... the minimum value of process inlet air humidity ratio must be less than the maximum.')
        CALL ShowContinueError('... minimum value entered by user = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%H_MinProcAirInHumRat,6))
        CALL ShowContinueError('... maximum value entered by user = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%H_MaxProcAirInHumRat,6))
        ErrorsFound = .TRUE.
      END IF
      IF(BalDesDehumPerfData(PerfDataNum)%H_MinProcAirInHumRat .LT. 0.0d0)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min boundary for the regen outlet air humidity ratio equation.')
        CALL ShowContinueError('... the minimum value of process inlet air humidity ratio must be greater than or equal to 0.')
        CALL ShowContinueError('... minimum value entered by user = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%H_MinProcAirInHumRat,6))
        ErrorsFound = .TRUE.
      END IF
      IF(BalDesDehumPerfData(PerfDataNum)%H_MaxProcAirInHumRat .GT. 1.0d0)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in max boundary for the regen outlet air humidity ratio equation.')
        CALL ShowContinueError('... the maximum value of process inlet air humidity ratio must be less than or equal to 1.')
        CALL ShowContinueError('... maximum value entered by user = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%H_MaxProcAirInHumRat,6))
        ErrorsFound = .TRUE.
      END IF

      BalDesDehumPerfData(PerfDataNum)%H_MinProcAirInTemp    = rNumericArgs(42)
      BalDesDehumPerfData(PerfDataNum)%H_MaxProcAirInTemp    = rNumericArgs(43)
      IF(BalDesDehumPerfData(PerfDataNum)%H_MinProcAirInTemp .GE. BalDesDehumPerfData(PerfDataNum)%H_MaxProcAirInTemp)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min/max boundary for the regen outlet air humidity ratio equation.')
        CALL ShowContinueError('... the minimum value of process inlet air temperature must be less than the maximum.')
        CALL ShowContinueError('... minimum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%H_MinProcAirInTemp,6))
        CALL ShowContinueError('... maximum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%H_MaxProcAirInTemp,6))
        ErrorsFound = .TRUE.
      END IF

      BalDesDehumPerfData(PerfDataNum)%H_MinFaceVel          = rNumericArgs(44)
      BalDesDehumPerfData(PerfDataNum)%H_MaxFaceVel          = rNumericArgs(45)
      IF(BalDesDehumPerfData(PerfDataNum)%H_MinFaceVel .GE. BalDesDehumPerfData(PerfDataNum)%H_MaxFaceVel)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min/max boundary for the regen outlet air humidity ratio equation.')
        CALL ShowContinueError('... the minimum value of regen air velocity must be less than the maximum.')
        CALL ShowContinueError('... minimum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%H_MinFaceVel,6))
        CALL ShowContinueError('... maximum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%H_MaxFaceVel,6))
        ErrorsFound = .TRUE.
      END IF

      BalDesDehumPerfData(PerfDataNum)%MinRegenAirOutHumRat  = rNumericArgs(46)
      BalDesDehumPerfData(PerfDataNum)%MaxRegenAirOutHumRat  = rNumericArgs(47)
      IF(BalDesDehumPerfData(PerfDataNum)%MinRegenAirOutHumRat .GE. BalDesDehumPerfData(PerfDataNum)%MaxRegenAirOutHumRat)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min/max boundary for the regen outlet air humidity ratio equation.')
        CALL ShowContinueError('... the minimum value of regen outlet air humidity ratio must be less than the maximum.')
        CALL ShowContinueError('... minimum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%MinRegenAirOutHumRat,6))
        CALL ShowContinueError('... maximum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%MaxRegenAirOutHumRat,6))
        ErrorsFound = .TRUE.
      END IF
      IF(BalDesDehumPerfData(PerfDataNum)%MinRegenAirOutHumRat .LT. 0.0d0)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min boundary for the regen outlet air humidity ratio equation.')
        CALL ShowContinueError('... the minimum value of regen outlet air humidity ratio must be greater than or equal to 0.')
        CALL ShowContinueError('... minimum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%MinRegenAirOutHumRat,6))
        ErrorsFound = .TRUE.
      END IF
      IF(BalDesDehumPerfData(PerfDataNum)%MaxRegenAirOutHumRat .GT. 1.0d0)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in max boundary for the regen outlet air humidity ratio equation.')
        CALL ShowContinueError('... the maximum value of regen outlet air humidity ratio must be less or equal to 1.')
        CALL ShowContinueError('... maximum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%MaxRegenAirOutHumRat,6))
        ErrorsFound = .TRUE.
      END IF

      BalDesDehumPerfData(PerfDataNum)%H_MinRegenAirInRelHum = rNumericArgs(48)/100.0d0
      BalDesDehumPerfData(PerfDataNum)%H_MaxRegenAirInRelHum = rNumericArgs(49)/100.0d0
      IF(BalDesDehumPerfData(PerfDataNum)%H_MinRegenAirInRelHum .GE. BalDesDehumPerfData(PerfDataNum)%H_MaxRegenAirInRelHum)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min/max boundary for the regen outlet air humidity ratio equation.')
        CALL ShowContinueError('... the minimum value of regen inlet air relative humidity must be less than the maximum.')
        CALL ShowContinueError('... minimum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%H_MinRegenAirInRelHum*100.0d0,6))
        CALL ShowContinueError('... maximum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%H_MaxRegenAirInRelHum*100.0d0,6))
        ErrorsFound = .TRUE.
      END IF
      IF(BalDesDehumPerfData(PerfDataNum)%H_MinRegenAirInRelHum .LT. 0.0d0)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min boundary for the regen outlet air humidity ratio equation.')
        CALL ShowContinueError('... the minimum value of regen inlet air relative humidity must be greater than or equal to 0.')
        CALL ShowContinueError('... minimum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%H_MinRegenAirInRelHum*100.0d0,6))
        ErrorsFound = .TRUE.
      END IF
      IF(BalDesDehumPerfData(PerfDataNum)%H_MaxRegenAirInRelHum .GT. 1.0d0)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in max boundary for the regen outlet air humidity ratio equation.')
        CALL ShowContinueError('... the maximum value of regen inlet air relative humidity must be less or equal to 100.')
        CALL ShowContinueError('... maximum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%H_MaxRegenAirInRelHum*100.0d0,6))
        ErrorsFound = .TRUE.
      END IF

      BalDesDehumPerfData(PerfDataNum)%H_MinProcAirInRelHum  = rNumericArgs(50)/100.0d0
      BalDesDehumPerfData(PerfDataNum)%H_MaxProcAirInRelHum  = rNumericArgs(51)/100.0d0
      IF(BalDesDehumPerfData(PerfDataNum)%H_MinProcAirInRelHum .GE. BalDesDehumPerfData(PerfDataNum)%H_MaxProcAirInRelHum)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min/max boundary for the regen outlet air humidity ratio equation.')
        CALL ShowContinueError('... the minimum value of process inlet air relative humidity must be less than the maximum.')
        CALL ShowContinueError('... minimum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%H_MinProcAirInRelHum*100.0d0,6))
        CALL ShowContinueError('... maximum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%H_MaxProcAirInRelHum*100.0d0,6))
        ErrorsFound = .TRUE.
      END IF
      IF(BalDesDehumPerfData(PerfDataNum)%H_MinProcAirInRelHum .LT. 0.0d0)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in min boundary for the regen outlet air humidity ratio equation.')
        CALL ShowContinueError('... the minimum value of process inlet air relative humidity must be greater than or equal to 0.')
        CALL ShowContinueError('... minimum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%H_MinProcAirInRelHum*100.0d0,6))
        ErrorsFound = .TRUE.
      END IF
      IF(BalDesDehumPerfData(PerfDataNum)%H_MaxProcAirInRelHum .GT. 1.0d0)THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//' "'//TRIM(BalDesDehumPerfData(PerfDataNum)%Name)//'"')
        CALL ShowContinueError('Error found in max boundary for the regen outlet air humidity ratio equation.')
        CALL ShowContinueError('... the maximum value of process inlet air relative humidity must be less than or equal to 100.')
        CALL ShowContinueError('... maximum value entered = ' &
                             //RoundSigDigits(BalDesDehumPerfData(PerfDataNum)%H_MaxProcAirInRelHum*100.0d0,6))
        ErrorsFound = .TRUE.
      END IF

    END DO
  ! getting performance data set for balanced desiccant heat exchanger ends

  ! match desiccant heat exchanger index to performance data index
    DO ExchIndex = 1, NumDesiccantBalancedExchs
      ExchNum = ExchIndex + NumAirToAirPlateExchs + NumAirToAirGenericExchs
      DO PerfDataNum = 1, NumDesBalExchsPerfDataType1
        IF (SameString(ExchCond(ExchNum)%HeatExchPerfName,BalDesDehumPerfData(PerfDataNum)%Name)) THEN
          ExchCond(ExchNum)%PerfDataIndex = PerfDataNum
          EXIT
        END IF
      END DO
      IF(ExchCond(ExchNum)%PerfDataIndex .EQ. 0)THEN
        CALL ShowSevereError(TRIM(cHXTypes(ExchCond(ExchNum)%ExchTypeNum))//' "'//TRIM(ExchCond(ExchNum)%Name)//'"')
        CALL ShowContinueError('... Performance data set not found = '//TRIM(ExchCond(ExchNum)%HeatExchPerfName))
        ErrorsFound = .TRUE.
      ELSE
        IF(.NOT. ErrorsFound)THEN
          ExchCond(ExchNum)%FaceArea = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%NomSupAirVolFlow/ &
                                      (BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%NomProcAirFaceVel)
        END IF
      END IF
    END DO
  ! matching done

  ! setup common report variables for heat exchangers
  DO ExchIndex = 1,NumHeatExchangers
    ExchNum = ExchIndex
    ! CurrentModuleObject='HeatExchanger:AirToAir:FlatPlate/AirToAir:SensibleAndLatent/Desiccant:BalancedFlow')
    CALL SetupOutputVariable('Heat Exchanger Sensible Heating Rate [W]',ExchCond(ExchNum)%SensHeatingRate,'System','Average',&
                             ExchCond(ExchNum)%Name)
    CALL SetupOutputVariable('Heat Exchanger Sensible Heating Energy [J]',ExchCond(ExchNum)%SensHeatingEnergy,'System','Sum',&
                             ExchCond(ExchNum)%Name)
    CALL SetupOutputVariable('Heat Exchanger Latent Gain Rate [W]',ExchCond(ExchNum)%LatHeatingRate,'System','Average',&
                             ExchCond(ExchNum)%Name)
    CALL SetupOutputVariable('Heat Exchanger Latent Gain Energy [J]',ExchCond(ExchNum)%LatHeatingEnergy,'System','Sum',&
                             ExchCond(ExchNum)%Name)
    CALL SetupOutputVariable('Heat Exchanger Total Heating Rate [W]',ExchCond(ExchNum)%TotHeatingRate,'System','Average',&
                             ExchCond(ExchNum)%Name)
    CALL SetupOutputVariable('Heat Exchanger Total Heating Energy [J]',ExchCond(ExchNum)%TotHeatingEnergy,'System','Sum',&
                             ExchCond(ExchNum)%Name, &
                             ResourceTypeKey='ENERGYTRANSFER',EndUseKey = 'HEAT RECOVERY FOR HEATING',GroupKey = 'System')
    CALL SetupOutputVariable('Heat Exchanger Sensible Cooling Rate [W]',ExchCond(ExchNum)%SensCoolingRate,'System','Average',&
                             ExchCond(ExchNum)%Name)
    CALL SetupOutputVariable('Heat Exchanger Sensible Cooling Energy [J]',ExchCond(ExchNum)%SensCoolingEnergy,'System','Sum',&
                             ExchCond(ExchNum)%Name)
    CALL SetupOutputVariable('Heat Exchanger Latent Cooling Rate [W]',ExchCond(ExchNum)%LatCoolingRate,'System','Average',&
                             ExchCond(ExchNum)%Name)
    CALL SetupOutputVariable('Heat Exchanger Latent Cooling Energy [J]',ExchCond(ExchNum)%LatCoolingEnergy,'System','Sum',&
                             ExchCond(ExchNum)%Name)
    CALL SetupOutputVariable('Heat Exchanger Total Cooling Rate [W]',ExchCond(ExchNum)%TotCoolingRate,'System','Average',&
                             ExchCond(ExchNum)%Name)
    CALL SetupOutputVariable('Heat Exchanger Total Cooling Energy [J]',ExchCond(ExchNum)%TotCoolingEnergy,'System','Sum',&
                             ExchCond(ExchNum)%Name, &
                             ResourceTypeKey='ENERGYTRANSFER',EndUseKey = 'HEAT RECOVERY FOR COOLING',GroupKey = 'System')

    CALL SetupOutputVariable('Heat Exchanger Electric Power [W]',ExchCond(ExchNum)%ElecUseRate,'System','Average',&
                             ExchCond(ExchNum)%Name)
    CALL SetupOutputVariable('Heat Exchanger Electric Energy [J]',ExchCond(ExchNum)%ElecUseEnergy,&
                             'System','Sum',ExchCond(ExchNum)%Name, &
                             ResourceTypeKey='ELECTRICITY',EndUseKey = 'HEATRECOVERY',GroupKey = 'System')
  END DO

  ! setup additional report variables for generic heat exchangers
  DO ExchIndex = 1,NumAirToAirGenericExchs
    ! generic heat exchangers are read in after flat plate heat exchanger objects (index needs to be set correctly)
    ! CurrentModuleObject=HeatExchanger:AirToAir:SensibleAndLatent
    ExchNum = ExchIndex + NumAirToAirPlateExchs
    CALL SetupOutputVariable('Heat Exchanger Sensible Effectiveness []',ExchCond(ExchNum)%SensEffectiveness,'System',&
                             'Average',ExchCond(ExchNum)%Name)
    CALL SetupOutputVariable('Heat Exchanger Latent Effectiveness []',ExchCond(ExchNum)%LatEffectiveness,'System',&
                             'Average',ExchCond(ExchNum)%Name)
    CALL SetupOutputVariable('Heat Exchanger Supply Air Bypass Mass Flow Rate [kg/s]',ExchCond(ExchNum)%SupBypassMassFlow,'System',&
                             'Average',ExchCond(ExchNum)%Name)
    CALL SetupOutputVariable('Heat Exchanger Exhaust Air Bypass Mass Flow Rate [kg/s]',ExchCond(ExchNum)%SecBypassMassFlow, &
                             'System','Average',ExchCond(ExchNum)%Name)
    CALL SetupOutputVariable('Heat Exchanger Defrost Time Fraction []',ExchCond(ExchNum)%DefrostFraction, &
                             'System','Average',ExchCond(ExchNum)%Name)

  END DO

  IF (ErrorsFound) THEN
    CALL ShowFatalError(RoutineName//'Errors found in input.  Program terminates.')
  END IF

  RETURN
END SUBROUTINE GetHeatRecoveryInput

SUBROUTINE InitHeatRecovery(ExchNum, CompanionCoilIndex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   March 1999
          !       MODIFIED       F Buhl Nov 2000, D Shirey Feb 2003
          !                      B Griffith May 2009, EMS setpoint check
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for initializations of the Heat Recovery Components.

          ! METHODOLOGY EMPLOYED:
          ! Uses the status flags to trigger initializations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits
  USE DXCoils, ONLY: DXCoilFullLoadOutAirTemp, DXCoilFullLoadOutAirHumRat
!  USE DataZoneEquipment,  ONLY: ZoneEquipInputsFilled,CheckZoneEquipmentList
  USE EMSManager,      ONLY: iTemperatureSetpoint, CheckIfNodeSetpointManagedByEMS, iHumidityRatioMaxSetpoint
  USE DataGLobals,     ONLY: AnyEnergyManagementSystemInModel

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: ExchNum ! number of the current heat exchanger being simulated
  INTEGER, INTENT (IN) :: CompanionCoilIndex

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: ExIndex       ! do loop index
  INTEGER             :: SupInNode     ! supply air inlet node number
  INTEGER             :: SecInNode     ! secondary air inlet node number
  REAL(r64)           :: CMIN0         ! minimum capacity flow
  REAL(r64)           :: CMAX0         ! maximum capacity flow
  REAL(r64)           :: Eps0          ! effectiveness at rated conditions
  REAL(r64)           :: NTU0          ! NTU at rated conditions
  REAL(r64)           :: RhoAir        ! air density at outside pressure & standard temperature and humidity
  REAL(r64)           :: CpAir         ! heat capacity of air
                                       ! of humidity ratio and temperature
  LOGICAL,SAVE        :: MyEnvrnFlag=.TRUE.
  LOGICAL,SAVE        :: MyOneTimeAllocate=.TRUE.
  LOGICAL, DIMENSION(:), ALLOCATABLE, SAVE  :: MySetPointTest
  LOGICAL, DIMENSION(:), ALLOCATABLE, SAVE  :: MySizeFlag
  INTEGER             :: ErrStat       ! error status returned by CalculateNTUfromEpsAndZ
  LOGICAL             :: FatalError    ! fatal error flag
  LOGICAL             :: LocalWarningError ! warning error flag
  REAL(r64)           :: Z             ! Min/max flow ratio
!  LOGICAL,SAVE        :: ZoneEquipmentListChecked = .false.  ! True after the Zone Equipment List has been checked for items

  IF(MyOneTimeAllocate)THEN
    ALLOCATE(MySetPointTest(NumHeatExchangers))
    ALLOCATE(MySizeFlag(NumHeatExchangers))
    MySetPointTest = .TRUE.
    MySizeFlag = .TRUE.
    MyOneTimeAllocate = .FALSE.
  END IF

IF ( .NOT. SysSizingCalc .AND. MySizeFlag(ExchNum) ) THEN

  CALL SizeHeatRecovery(ExchNum)
  MySizeFlag(ExchNum) = .FALSE.

END IF


  FatalError = .FALSE.
  LocalWarningError = .false.

  ! Do the Begin Environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag) THEN
    !I believe that all of these initializations should be taking place at the SCFM conditions
    RhoAir = StdRhoAir
!    RhoAir = PsyRhoAirFnPbTdbW(101325.0,20.0,0.0)  do we want standard air density at sea level for generic ERVs per ARI 1060?
    CpAir = PsyCpAirFnWTdb(0.0d0,20.0d0)
    DO ExIndex=1,NumHeatExchangers

      SELECT CASE(ExchCond(ExIndex)%ExchTypeNum)

      CASE(HX_AIRTOAIR_FLATPLATE)

        ExchCond(ExIndex)%NomSupAirMassFlow = RhoAir * ExchCond(ExIndex)%NomSupAirVolFlow
        ExchCond(ExIndex)%NomSecAirMassFlow = RhoAir * ExchCond(ExIndex)%NomSecAirVolFlow
        ! Note: the capacity stream is here simply the mass flow
        !       since the thermal capacity can be assumed to be
        !       equal for both streams
        IF (ExchCond(ExIndex)%NomSupAirMassFlow > ExchCond(ExIndex)%NomSecAirMassFlow) THEN
          CMIN0 = ExchCond(ExIndex)%NomSecAirMassFlow
          CMAX0 = ExchCond(ExIndex)%NomSupAirMassFlow
        ELSE
          CMIN0 = ExchCond(ExIndex)%NomSupAirMassFlow
          CMAX0 = ExchCond(ExIndex)%NomSecAirMassFlow
        END IF

        Eps0 = ExchCond(ExIndex)%NomSupAirMassFlow *  &
                 SafeDiv( ExchCond(ExIndex)%NomSupAirOutTemp - ExchCond(ExIndex)%NomSupAirInTemp, &
                 CMin0*(ExchCond(ExIndex)%NomSecAirInTemp - ExchCond(ExIndex)%NomSupAirInTemp) )
        Z = CMIN0/CMAX0

        ErrStat=0
        CALL CalculateNTUfromEpsAndZ(NTU0, ErrStat, Z, ExchCond(ExIndex)%FlowArr, Eps0)

        IF (ErrStat == 1) THEN

          FatalError = .TRUE.
          CALL ShowSevereError('In the HeatExchanger:AirToAir:FlatPlate component ' // TRIM(ExchCond(ExIndex)%Name) )
          CALL ShowContinueError('  the mass flow ratio is out of bounds')
          CALL ShowContinueError('The mass flow ratio is (Min_Mass_Flow_Rate / Max_Mass_Flow_Rate) = ' // &
                                 TRIM(RoundSigDigits(Z,2)))
          CALL ShowContinueError('The mass flow ratio should be >= 0.0 and <= 1.0')
          CALL ShowContinueError('Min_Mass_Flow_Rate = ' // TRIM(RoundSigDigits(RhoAir,2)) // ' [air density] * ' // &
                                 TRIM(RoundSigDigits(MIN(ExchCond(ExIndex)%NomSupAirVolFlow, &
                                                         ExchCond(ExIndex)%NomSecAirVolFlow),1)) // ' [Min_Vol_Flow_Rate]' )
          CALL ShowContinueError('Max_Mass_Flow_Rate = ' // TRIM(RoundSigDigits(RhoAir,2)) // ' [air density] * ' // &
                                 TRIM(RoundSigDigits(MAX(ExchCond(ExIndex)%NomSupAirVolFlow, &
                                                         ExchCond(ExIndex)%NomSecAirVolFlow),1)) // ' [Max_Vol_Flow_Rate]' )
        ELSE IF (ErrStat == 2) THEN
          FatalError = .TRUE.
          CALL ShowSevereError('In the HeatExchanger:AirToAir:FlatPlate component ' // TRIM(ExchCond(ExIndex)%Name) )
          CALL ShowContinueError('  the calculated nominal effectiveness is out of bounds')
          CALL ShowContinueError('The effectiveness is ' // TRIM(RoundSigDigits(Eps0,3)))
          CALL ShowContinueError('The effectiveness should be >= 0.0 and <= ' // &
                                 TRIM(RoundSigDigits(1.0d0/(1.0d0+Z),3)))
          CALL ShowContinueError('Eff = (Nom_Sup_Mass_Flow_Rate/Min_Mass_Flow_Rate)*(T_nom_sup_out-T_nom_sup_in)' &
                                  // '/(T_nom_sec_in-T_nom_sup_in)')
          CALL ShowContinueError('The temperatures are user inputs. The mass flow rates are user input volume flow rates')
          CALL ShowContinueError('  times the density of air [' // TRIM(RoundSigDigits(RhoAir,2)) // ' kg/m3]' )
          CALL ShowContinueError('Change these inputs to obtain a physically realizable heat exchanger effectiveness')
        ELSE IF (ErrStat == 3) THEN
          FatalError = .TRUE.
          CALL ShowSevereError('In the HeatExchanger:AirToAir:FlatPlate component ' // TRIM(ExchCond(ExIndex)%Name) )
          CALL ShowContinueError('  the calculated nominal effectiveness is out of bounds')
          CALL ShowContinueError('The effectiveness is ' // TRIM(RoundSigDigits(Eps0,3)))
          CALL ShowContinueError('The effectiveness should be >= 0.0 and <= ' // &
                                 TRIM(RoundSigDigits((1.0d0-EXP(-Z))/Z,3)))
          CALL ShowContinueError('Eff = (Nom_Sup_Mass_Flow_Rate/Min_Mass_Flow_Rate)*(T_nom_sup_out-T_nom_sup_in)' &
                                  // '/(T_nom_sec_in-T_nom_sup_in)')
          CALL ShowContinueError('The temperatures are user inputs. The mass flow rates are user input volume flow rates')
          CALL ShowContinueError('  times the density of air [' // TRIM(RoundSigDigits(RhoAir,2)) // ' kg/m3]' )
          CALL ShowContinueError('Change these inputs to obtain a physically realizable heat exchanger effectiveness')
        ELSE IF (ErrStat == 4) THEN
          FatalError = .TRUE.
          CALL ShowSevereError('In the HeatExchanger:AirToAir:FlatPlate component ' // TRIM(ExchCond(ExIndex)%Name))
          CALL ShowContinueError('  the quantity Eff_nom*(Min_Mass_Flow_Rate / Max_Mass_Flow_Rate) is out of bounds')
          CALL ShowContinueError('The value is ' // TRIM(RoundSigDigits(Eps0*Z,3)))
          CALL ShowContinueError('The value should be >= 0.0 and <= ' // &
                                 TRIM(RoundSigDigits(1.0d0-EXP(Z*(SMALL-1.0d0)),3)))
          CALL ShowContinueError('Eff_nom = (Nom_Sup_Mass_Flow_Rate/Min_Mass_Flow_Rate) * (T_nom_sup_out - T_nom_sup_in)' &
                                  // '/(T_nom_sec_in - T_nom_sup_in)')
          CALL ShowContinueError('The temperatures are user inputs. The mass flow rates are user input volume flow rates')
          CALL ShowContinueError('  times the density of air [' // TRIM(RoundSigDigits(RhoAir,2)) // ' kg/m3]' )
          CALL ShowContinueError('Change these inputs to obtain a physically realizable product of effectiveness' &
                                  // 'times min/max mass ratio for this heat exchanger')
        ELSE IF (ErrStat == 5) THEN
          FatalError = .TRUE.
          CALL ShowSevereError('In the HeatExchanger:AirToAir:FlatPlate component ' // TRIM(ExchCond(ExIndex)%Name) )
          CALL ShowContinueError('  the calculated nominal effectiveness is out of bounds')
          CALL ShowContinueError('The effectiveness is ' // TRIM(RoundSigDigits(Eps0,3)))
          CALL ShowContinueError('The effectiveness should be >= 0.0 and <= 1.0')
          CALL ShowContinueError('Eff = (Nom_Sup_Mass_Flow_Rate/Min_Mass_Flow_Rate)*(T_nom_sup_out-T_nom_sup_in)' &
                                  // '/(T_nom_sec_in-T_nom_sup_in)')
          CALL ShowContinueError('The temperatures are user inputs. The mass flow rates are user input volume flow rates')
          CALL ShowContinueError('  times the density of air [' // TRIM(RoundSigDigits(RhoAir,2)) // ' kg/m3]' )
          CALL ShowContinueError('Change these inputs to obtain a physically realizable heat exchanger effectiveness')

        END IF

        IF (FatalError) THEN
          CALL ShowFatalError('Heat exchanger design calculation caused fatal error: program terminated.')
        END IF

        ExchCond(ExIndex)%UA0    = NTU0 * CMin0 *CpAir
        ExchCond(ExIndex)%mTSup0 = ExchCond(ExIndex)%NomSupAirMassFlow * (ExchCond(ExIndex)%NomSupAirInTemp + KELVZERO)
        ExchCond(ExIndex)%mTSec0 = ExchCond(ExIndex)%NomSecAirMassFlow * (ExchCond(ExIndex)%NomSecAirInTemp + KELVZERO)

        ! check validity
        IF (ExchCond(ExIndex)%NomSupAirMassFlow * ExchCond(ExIndex)%NomSecAirMassFlow < SmallMassFlow*SmallMassFlow) THEN
          CALL ShowFatalError("Mass flow in HeatExchanger:AirToAir:FlatPlate too small in initialization.")
        END IF

        IF (ExchCond(ExIndex)%mTSup0 < SmallMassFlow) THEN
          CALL ShowFatalError("(m*T)Sup,in in HeatExchanger:AirToAir:FlatPlate too small in initialization.")
        END IF

        IF (ExchCond(ExIndex)%mTSec0 < SmallMassFlow) THEN
          CALL ShowFatalError("(m*T)Sec,in in HeatExchanger:AirToAir:FlatPlate too small in initialization.")
        END IF

        IF (CMin0 < SmallMassFlow) THEN
          CALL ShowFatalError("CMin0 in HeatExchanger:AirToAir:FlatPlate too small in initialization.")
        END IF

      CASE(HX_AIRTOAIR_GENERIC)

        IF (ExchCond(ExIndex)%SupOutletNode > 0 .AND. ExchCond(ExIndex)%ControlToTemperatureSetPoint) THEN
          IF (Node(ExchCond(ExIndex)%SupOutletNode)%TempSetPoint == SensedNodeFlagValue) THEN
            IF (.NOT. AnyEnergyManagementSystemInModel) THEN
              CALL ShowSevereError('Missing temperature setpoint for ' // &
                              TRIM(cHXTypes(ExchCond(ExIndex)%ExchTypeNum)) //' "'//TRIM(ExchCond(ExIndex)%Name) // '" :')
              CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the supply air outlet node ' // &
                               'of the Heat Exchanger.')
              CALL ShowFatalError(' Previous condition causes program termination.')
            ELSE
             ! need call to EMS to check node
              CALL CheckIfNodeSetpointManagedByEMS(ExchCond(ExIndex)%SupOutletNode,iTemperatureSetpoint, FatalError)
              IF (FatalError) THEN
                CALL ShowSevereError('Missing temperature setpoint for ' // &
                                TRIM(cHXTypes(ExchCond(ExIndex)%ExchTypeNum)) //' "'//TRIM(ExchCond(ExIndex)%Name) // '" :')
                CALL ShowContinueError('  use a Setpoint Manager to establish a setpoint at the supply air outlet node ' // &
                                 'of the Heat Exchanger.')
                CALL ShowContinueError('  or use an EMS actuator to establish a setpoint at the supply air outlet node ' // &
                                 'of the Heat Exchanger.')
                CALL ShowFatalError(' Previous condition causes program termination.')
              ENDIF
            ENDIF
          END IF
        END IF

      CASE(HX_DESICCANT_BALANCED)


      CASE DEFAULT
!       Will never get here

      END SELECT

    END DO
    MyEnvrnFlag = .FALSE.

  END IF

  IF (.NOT. BeginEnvrnFlag) THEN
    MyEnvrnFlag=.TRUE.
  ENDIF

  ! Do these initializations every time step
  SupInNode = ExchCond(ExchNum)%SupInletNode
  SecInNode = ExchCond(ExchNum)%SecInletNode

  ! Get information from inlet nodes
  ExchCond(ExchNum)%SupInTemp   = Node(SupInNode)%Temp
  ExchCond(ExchNum)%SupInHumRat = Node(SupInNode)%HumRat
  ExchCond(ExchNum)%SupInEnth   = Node(SupInNode)%Enthalpy
  ExchCond(ExchNum)%SupInMassFlow = Node(SupInNode)%MassFlowRate
  ExchCond(ExchNum)%SecInTemp   = Node(SecInNode)%Temp
  ExchCond(ExchNum)%SecInHumRat = Node(SecInNode)%HumRat
  ExchCond(ExchNum)%SecInEnth   = Node(SecInNode)%Enthalpy
  ExchCond(ExchNum)%SecInMassFlow = Node(SecInNode)%MassFlowRate

  ! initialize the output variables
  ExchCond(ExchNum)%SensHeatingRate   = 0.0d0
  ExchCond(ExchNum)%SensHeatingEnergy = 0.0d0
  ExchCond(ExchNum)%LatHeatingRate    = 0.0d0
  ExchCond(ExchNum)%LatHeatingEnergy  = 0.0d0
  ExchCond(ExchNum)%TotHeatingRate    = 0.0d0
  ExchCond(ExchNum)%TotHeatingEnergy  = 0.0d0
  ExchCond(ExchNum)%SensCoolingRate   = 0.0d0
  ExchCond(ExchNum)%SensCoolingEnergy = 0.0d0
  ExchCond(ExchNum)%LatCoolingRate    = 0.0d0
  ExchCond(ExchNum)%LatCoolingEnergy  = 0.0d0
  ExchCond(ExchNum)%TotCoolingRate    = 0.0d0
  ExchCond(ExchNum)%TotCoolingEnergy  = 0.0d0
  ExchCond(ExchNum)%ElecUseRate       = 0.0d0
  ExchCond(ExchNum)%ElecUseEnergy     = 0.0d0
  ExchCond(ExchNum)%SensEffectiveness = 0.0d0
  ExchCond(ExchNum)%LatEffectiveness  = 0.0d0
  ExchCond(ExchNum)%SupBypassMassFlow = 0.0d0
  ExchCond(ExchNum)%SecBypassMassFlow = 0.0d0


!  Initialize inlet conditions

  SELECT CASE(ExchCond(ExchNum)%ExchTypeNum)

  CASE(HX_AIRTOAIR_FLATPLATE)

  CASE(HX_AIRTOAIR_GENERIC)

  CASE(HX_DESICCANT_BALANCED)

    IF(MySetPointTest(ExchNum))THEN
      IF ( .NOT. SysSizingCalc .AND. DoSetPointTest) THEN
        IF(.NOT. CalledFromParentObject)THEN
          IF (Node(ExchCond(ExchNum)%SecOutletNode)%HumRatMax == SensedNodeFlagValue) THEN
            IF (.NOT. AnyEnergyManagementSystemInModel) THEN
              CALL ShowWarningError('Missing optional HumRatMax setpoint for ' // &
                        TRIM(cHXTypes(ExchCond(ExchNum)%ExchTypeNum)) //' "'//TRIM(ExchCond(ExchNum)%Name) // '"')
              CALL ShowContinueError('...the simulation will continue without control of the desiccant heat exchanger to'// &
                                 ' a maximum humidity ratio setpoint.')
              CALL ShowContinueError('...use a Setpoint Manager to establish a setpoint at the process air outlet node ' // &
                               'of the desiccant Heat Exchanger if control is desired.')
            ELSE
             ! need call to EMS to check node
              CALL CheckIfNodeSetpointManagedByEMS(ExchCond(ExchNum)%SecOutletNode,iHumidityRatioMaxSetpoint, LocalWarningError)
              IF (LocalWarningError) THEN
                CALL ShowWarningError('Missing optional HumRatMax setpoint for ' // &
                        TRIM(cHXTypes(ExchCond(ExchNum)%ExchTypeNum)) //' "'//TRIM(ExchCond(ExchNum)%Name) // '"')
                CALL ShowContinueError('...the simulation will continue without control of the desiccant heat exchanger to'// &
                                 ' a maximum humidity ratio setpoint.')
                CALL ShowContinueError('...use a Setpoint Manager to establish a setpoint at the process air outlet node ' // &
                               'of the desiccant Heat Exchanger if control is desired.')
                CALL ShowContinueError('...or use an EMS Actuator to establish a maximum humidity ratio setpoint at the ' &
                               //'process air outlet node of the desiccant Heat Exchanger if control is desired.')
              ENDIF
            ENDIF
          END IF
        END IF
        MySetPointTest(ExchNum) = .FALSE.
      END IF
    END IF

    IF(CompanionCoilIndex .GT. 0) THEN

      IF(DXCoilFullLoadOutAirTemp(CompanionCoilIndex) == 0.0d0 .OR. DXCoilFullLoadOutAirHumRat(CompanionCoilIndex) == 0.0d0)THEN
!       DX Coil is OFF, read actual inlet conditions
        FullLoadOutAirTemp   = ExchCond(ExchNum)%SecInTemp
        FullLoadOutAirHumRat = ExchCond(ExchNum)%SecInHumRat
      ELSE
!       DX Coil is ON, read full load DX coil outlet conditions (conditions HX sees when ON)
        FullLoadOutAirTemp   = DXCoilFullLoadOutAirTemp(CompanionCoilIndex)
        FullLoadOutAirHumRat = DXCoilFullLoadOutAirHumRat(CompanionCoilIndex)
      END IF

    ELSE

!     HX only (not used in conjunction with DX coil), read inlet conditions
      FullLoadOutAirTemp   = ExchCond(ExchNum)%SecInTemp
      FullLoadOutAirHumRat = ExchCond(ExchNum)%SecInHumRat

    END IF

  CASE DEFAULT
!   Will never get here

  END SELECT

  RETURN

END SUBROUTINE InitHeatRecovery

SUBROUTINE SizeHeatRecovery(ExchNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   October 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is for sizing Heat Exchanger components for which flow rates have not been
          ! specified in the input. Currently, only nominal supply air flow rate for the generic HX can be autosized.

          ! METHODOLOGY EMPLOYED:
          ! Obtains flow rates from the system or OA system sizing arrays

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSizing
  USE DataHVACGlobals, ONLY: SmallAirVolFlow, Main, Cooling, Heating, Other
  USE ReportSizingManager, ONLY: ReportSizingOutput

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Integer, Intent(IN) :: ExchNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (ExchCond(ExchNum)%NomSupAirVolFlow == AutoSize) THEN

    IF (CurZoneEqNum > 0) THEN

      CALL CheckZoneSizing(cHXTypes(ExchCond(ExchNum)%ExchTypeNum), ExchCond(ExchNum)%Name)
      ExchCond(ExchNum)%NomSupAirVolFlow = MAX(FinalZoneSizing(CurZoneEqNum)%DesCoolVolFlow, &
                                              FinalZoneSizing(CurZoneEqNum)%DesHeatVolFlow)

    END IF

    IF (CurSysNum > 0) THEN

      CALL CheckSysSizing(cHXTypes(ExchCond(ExchNum)%ExchTypeNum), ExchCond(ExchNum)%Name)

        IF (CurOASysNum > 0) THEN
          ! size to outdoor air volume flow rate if available
          IF(FinalSysSizing(CurSysNum)%DesOutAirVolFlow .GT. 0.d0)THEN
            ExchCond(ExchNum)%NomSupAirVolFlow = FinalSysSizing(CurSysNum)%DesOutAirVolFlow
          ELSE
            ! ELSE size to supply air duct flow rate
            SELECT CASE(CurDuctType)
              CASE(Main)
                ExchCond(ExchNum)%NomSupAirVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
              CASE(Cooling)
                ExchCond(ExchNum)%NomSupAirVolFlow = FinalSysSizing(CurSysNum)%DesCoolVolFlow
              CASE(Heating)
                ExchCond(ExchNum)%NomSupAirVolFlow = FinalSysSizing(CurSysNum)%DesHeatVolFlow
              CASE(Other)
                ExchCond(ExchNum)%NomSupAirVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
              CASE DEFAULT
                ExchCond(ExchNum)%NomSupAirVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
            END SELECT
          END IF
        ELSE
          SELECT CASE(CurDuctType)
            CASE(Main)
              ExchCond(ExchNum)%NomSupAirVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
            CASE(Cooling)
              ExchCond(ExchNum)%NomSupAirVolFlow = FinalSysSizing(CurSysNum)%DesCoolVolFlow
            CASE(Heating)
              ExchCond(ExchNum)%NomSupAirVolFlow = FinalSysSizing(CurSysNum)%DesHeatVolFlow
            CASE(Other)
              ExchCond(ExchNum)%NomSupAirVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
            CASE DEFAULT
              ExchCond(ExchNum)%NomSupAirVolFlow = FinalSysSizing(CurSysNum)%DesMainVolFlow
          END SELECT
        END IF

    END IF

    IF (ExchCond(ExchNum)%NomSupAirVolFlow < SmallAirVolFlow) THEN
        ExchCond(ExchNum)%NomSupAirVolFlow = 0.0d0
        ! Generic HX will be turned off if nominal air flow rate is 0, even if simulated air flow through
        ! HX is greater than 0. Avoids a divide by 0 in Sub CalcAirToAirGenericHeatExch.
        IF(ExchCond(ExchNum)%ExchTypeNum == HX_AIRTOAIR_GENERIC) THEN
          CALL ShowWarningError(TRIM(cHXTypes(ExchCond(ExchNum)%ExchTypeNum))//': "'//TRIM(ExchCond(ExchNum)%Name)//'"')
          CALL ShowContinueError('... nominal supply air volume flow rate through the heat exchanger is sized to 0'// &
                                 ', see eio file for sizing results.')
          CALL ShowContinueError('... HX will not be enabled and the simulation continues.')
          CALL ShowContinueError('... To eliminate this warning, check sizing and HX inputs to correct HX sizing issue.')
        END IF
    END IF

    CALL ReportSizingOutput(cHXTypes(ExchCond(ExchNum)%ExchTypeNum), ExchCond(ExchNum)%Name, &
                           'Nominal Supply Air Flow Rate [m3/s]', ExchCond(ExchNum)%NomSupAirVolFlow)

  END IF

  RETURN

END SUBROUTINE SizeHeatRecovery

SUBROUTINE CalcAirToAirPlateHeatExch(ExNum, HXUnitOn, EconomizerFlag, HighHumCtrlFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   March 1999
          !       MODIFIED       F. Buhl Nov 2000, R. Raustad - FSEC, Feb 2009 - added economizer flags
          !                      Both the economizer and high humidity control flags can disable the HX
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate the outlet conditions for an air to air plate heat
          ! exchanger given the inlet conditions.

          ! METHODOLOGY EMPLOYED:
          ! This is a static heat exchanger model. No geometrical input data
          ! is needed. No knowledge of h*A values is needed except the ratio
          ! of the primary side to the secondary side convective heat transfer
          ! coefficient times the exchanger surface area. Effectiveness - NTU
          ! heat exchanger formulas are used.

          ! The time varying load is calculated based on the variation of the
          ! convective heat transfer coefficient.The variation is a function of
          ! mass flow rate and inlet temperature. An iterative solution is only
          ! required during initialization in one specific flow arrangement. During
          ! the time steps the solution is explicit. The iteration is done with
          ! the Regula Falsi algorithm. Convergence is always achieved.

          ! REFERENCES:
          ! M. Wetter, Simulation Model Air-to-Air Plate Heat Exchanger
          ! LBNL Report 42354, 1999.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: ExNum ! number of the current heat exchanger being simulated
  LOGICAL, INTENT (IN) :: HXUnitOn ! flag to simulate heat exchager heat recovery
  LOGICAL, OPTIONAL, INTENT (IN) :: EconomizerFlag ! economizer flag pass by air loop or OA sys
  LOGICAL, OPTIONAL, INTENT (IN) :: HighHumCtrlFlag     ! high humidity control flag passed by airloop or OA sys

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL      :: UnitOn              ! unit on flag
  REAL(r64)    :: SupByPassMassFlow   ! supply air mass flow rate bypassing unit [kg/s]
  REAL(r64)    :: UnitSupMassFlow     ! supply air mass flow rate passing through the unit [kg/s]
  REAL(r64)    :: SecByPassMassFlow   ! secondary air mass flow rate bypassing unit [kg/s]
  REAL(r64)    :: UnitSecMassFlow     ! secondary air mass flow rate passing through the unit [kg/s]
  REAL(r64)    :: QuotSup             ! ratio of supply nominal m*T to actual m*T
  REAL(r64)    :: QuotExh             ! ratio of secondary nominal m*T to actual m*T
  REAL(r64)    :: Deno                ! denominator of UA calculation
  REAL(r64)    :: CSup                ! supply air capacitance rate [J/C/s]
  REAL(r64)    :: CSec                ! secondary air capacitance rate [J/C/s]
  REAL(r64)    :: CMin                ! minimum air capacitance rate [J/C/s]
  REAL(r64)    :: Z                   ! Ratio of minimum air capacitance rate to maximum air capacitance rate
  REAL(r64)    :: NTU                 ! Number of heat transfer units
  REAL(r64)    :: Eps                 ! epsilon, the unit effectiveness
  REAL(r64)    :: UA                  ! present UA
  REAL(r64)    :: TempSupOut          ! unit supply outlet temperature [C]
  REAL(r64)    :: HumRatSupOut        ! unit supply outlet humidity ratio [kg water / kg dry air]
  REAL(r64)    :: EnthSupOut          ! unit supply outlet enthalpy [J/kg]
  REAL(r64)    :: TempSupOutSat       ! unit supply outlet temperature at saturation (at EnthSupOut) [C]
  REAL(r64)    :: QTrans              ! heat transferred in the heat exchanger [W]
  REAL(r64)    :: ElecCons            ! electricity consumption rate [W]
  REAL(r64)    :: TempSecOut          ! unit secondary outlet temperature [C]
  REAL(r64)    :: HumRatSecOut        ! unit secondary outlet humidity ratio [kg water / kg dry air]
  REAL(r64)    :: EnthSecOut          ! unit secondary outlet enthalpy [J/kgC]
  REAL(r64)    :: TempSecOutSat       ! unit secondary outlet temperature at saturation (at EnthsSecOut) [C]
  REAL(r64)    :: SensHeatRecRate     ! sensible heat recovery rate to supply air (heating +, cooling -)
  REAL(r64)    :: LatHeatRecRate      ! latent heat recovery rate to supply air (heating [humidify] +, cooling [dehumidify] -)
  REAL(r64)    :: TotHeatRecRate      ! total heat recovery rate to supply air (heating +, cooling -)
  LOGICAL      :: EconomizerActiveFlag  ! local representing the economizer status when PRESENT
  LOGICAL      :: HighHumCtrlActiveFlag ! local representing high humidity control when PRESENT

  UnitOn = .TRUE.
  QTrans = 0.0d0
  ElecCons = 0.0d0

  IF(PRESENT(EconomizerFlag))THEN
    EconomizerActiveFlag = EconomizerFlag
  ELSE
    EconomizerActiveFlag = .FALSE.
  END IF

  IF(PRESENT(HighHumCtrlFlag))THEN
    HighHumCtrlActiveFlag = HighHumCtrlFlag
  ELSE
    HighHumCtrlActiveFlag = .FALSE.
  END IF

  IF((EconomizerActiveFlag .OR. HighHumCtrlActiveFlag) .AND. &
      ExchCond(ExNum)%EconoLockOut .EQ. EconoLockOut_Yes)THEN
    UnitSupMassFlow = 0.0d0 ! set HX supply flow to 0, all supply air will go through supply bypass
    UnitSecMassFlow = 0.0d0 ! set HX secondary flow to 0, all secondary air will got through secondary bypass
    UnitOn = .FALSE.        ! turn off HX calculations when in economizer mode
  ELSE
    ! if economizer operation is not allowed, air always passes through HX
    ! if CompanionCoilNum > 0, air always passes through HX (no economizer operation allowed)
    UnitSupMassFlow = MIN(ExchCond(ExNum)%NomSupAirMassFlow,ExchCond(ExNum)%SupInMassFlow)
    UnitSecMassFlow = MIN(ExchCond(ExNum)%NomSecAirMassFlow,ExchCond(ExNum)%SecInMassFlow)
  END IF

  SupByPassMassFlow =  MAX(0.0d0,ExchCond(ExNum)%SupInMassFlow - UnitSupMassFlow)
  SecByPassMassFlow =  MAX(0.0d0,ExchCond(ExNum)%SecInMassFlow - UnitSecMassFlow)
  IF (GetCurrentScheduleValue(ExchCond(ExNum)%SchedPtr) .LE. 0.0d0) UnitOn = .FALSE.
  IF (ExchCond(ExNum)%SupInMassFlow .LE. SmallMassFlow) UnitOn = .FALSE.
  IF (ExchCond(ExNum)%SecInMassFlow .LE. SmallMassFlow) UnitOn = .FALSE.
  IF (.NOT. HXUnitOn) UnitOn = .FALSE.

  IF (UnitOn) THEN
    ! unit is on
    ! calculate the UA for this time step
    QuotSup = SafeDiv( ExchCond(ExNum)%mTSup0, &
                       UnitSupMassFlow * ( ExchCond(ExNum)%SupInTemp + KELVZERO) )
    QuotExh = SafeDiv( ExchCond(ExNum)%mTSec0, &
                       UnitSecMassFlow * ( ExchCond(ExNum)%SecInTemp + KELVZERO) )
    Deno = QuotSup**0.78d0 + ExchCond(ExNum)%hARatio * QuotExh**0.78d0
    UA = ExchCond(ExNum)%UA0 * ( ExchCond(ExNum)%hARatio + 1.d0 ) / Deno
    ! calculate the NTU
    CSup = UnitSupMassFlow * PsyCpAirFnWTdb(ExchCond(ExNum)%SupInHumRat,ExchCond(ExNum)%SupInTemp)
    CSec = UnitSecMassFlow * PsyCpAirFnWTdb(ExchCond(ExNum)%SecInHumRat,ExchCond(ExNum)%SecInTemp)
    ! note: no C can be zero since otherwise we wouldn't be here
    IF (CSup < CSec) THEN
      CMin = CSup
      Z = CMin / CSec
    ELSE
      CMin = CSec
      Z = CMin / CSup
    END IF
    NTU = UA / CMin
    ! Get the effectiveness
    CALL CalculateEpsFromNTUandZ(NTU, Z, ExchCond(ExNum)%FlowArr, Eps)
    ! use the effectiveness to calculate the unit outlet conditions
    TempSupOut = ExchCond(ExNum)%SupInTemp + Eps * CMin / CSup * (ExchCond(ExNum)%SecInTemp - ExchCond(ExNum)%SupInTemp)
    QTrans = CSup * (TempSupOut - ExchCond(ExNum)%SupInTemp)
    TempSecOut = ExchCond(ExNum)%SecInTemp - QTrans / CSec
    HumRatSupOut = ExchCond(ExNum)%SupInHumRat
    EnthSupOut = PsyHFnTdbW(TempSupOut,HumRatSupOut)
    ! check for saturation in supply outlet
    TempSupOutSat = PsyTsatFnHPb(EnthSupOut,OutBaroPress)
    IF (TempSupOutSat.GT.TempSupOut) THEN
      TempSupOut = TempSupOutSat
      HumRatSupOut = PsyWFnTdbH(TempSupOut,EnthSupOut)
    END IF
    HumRatSecOut = ExchCond(ExNum)%SecInHumRat
    EnthSecOut = PsyHFnTdbW(TempSecOut,HumRatSecOut)
    ! check for saturation in secondary outlet
    TempSecOutSat = PsyTsatFnHPb(EnthSecOut,OutBaroPress)
    IF (TempSecOutSat.GT.TempSecOut) THEN
      TempSecOut = TempSecOutSat
      HumRatSecOut = PsyWFnTdbH(TempSecOut,EnthSecOut)
    END IF
    ! calculate outlet conditions by mixing bypass air stream with air that went through the
    ! heat exchanger core.
    ExchCond(ExNum)%SupOutEnth = (UnitSupMassFlow*EnthSupOut + SupByPassMassFlow*ExchCond(ExNum)%SupInEnth) &
                                   / ExchCond(ExNum)%SupInMassFlow
    ExchCond(ExNum)%SupOutHumRat = (UnitSupMassFlow*HumRatSupOut + SupByPassMassFlow*ExchCond(ExNum)%SupInHumRat) &
                                   / ExchCond(ExNum)%SupInMassFlow
    ExchCond(ExNum)%SupOutTemp = PsyTdbFnHW(ExchCond(ExNum)%SupOutEnth,ExchCond(ExNum)%SupOutHumRat)
    ExchCond(ExNum)%SupOutMassFlow = ExchCond(ExNum)%SupInMassFlow
    ExchCond(ExNum)%SecOutEnth = (UnitSecMassFlow*EnthSecOut + SecByPassMassFlow*ExchCond(ExNum)%SecInEnth) &
                                   / ExchCond(ExNum)%SecInMassFlow
    ExchCond(ExNum)%SecOutHumRat = (UnitSecMassFlow*HumRatSecOut + SecByPassMassFlow*ExchCond(ExNum)%SecInHumRat) &
                                   / ExchCond(ExNum)%SecInMassFlow
    ExchCond(ExNum)%SecOutTemp = PsyTdbFnHW(ExchCond(ExNum)%SecOutEnth,ExchCond(ExNum)%SecOutHumRat)
    ExchCond(ExNum)%SecOutMassFlow = ExchCond(ExNum)%SecInMassFlow
    ElecCons = ExchCond(ExNum)%NomElecPower

  ELSE
    ! the unit is off. Pass through the air streams with no change
    ExchCond(ExNum)%SupOutEnth = ExchCond(ExNum)%SupInEnth
    ExchCond(ExNum)%SupOutHumRat = ExchCond(ExNum)%SupInHumRat
    ExchCond(ExNum)%SupOutTemp = ExchCond(ExNum)%SupInTemp
    ExchCond(ExNum)%SupOutMassFlow = ExchCond(ExNum)%SupInMassFlow
    ExchCond(ExNum)%SecOutEnth = ExchCond(ExNum)%SecInEnth
    ExchCond(ExNum)%SecOutHumRat = ExchCond(ExNum)%SecInHumRat
    ExchCond(ExNum)%SecOutTemp = ExchCond(ExNum)%SecinTemp
    ExchCond(ExNum)%SecOutMassFlow = ExchCond(ExNum)%SecInMassFlow

  END IF
  CSup = ExchCond(ExNum)%SupInMassFlow*PsyCpAirFnWTdb(ExchCond(ExNum)%SupInHumRat,ExchCond(ExNum)%SupInTemp)
  SensHeatRecRate = CSup * (ExchCond(ExNum)%SupOutTemp - ExchCond(ExNum)%SupInTemp)
  TotHeatRecRate = ExchCond(ExNum)%SupOutMassFlow * ( ExchCond(ExNum)%SupOutEnth - &
                                     ExchCond(ExNum)%SupInEnth )
  LatHeatRecRate = TotHeatRecRate - SensHeatRecRate


  IF (SensHeatRecRate .GT. 0.0d0) THEN
    ExchCond(ExNum)%SensHeatingRate = SensHeatRecRate
    ExchCond(ExNum)%SensCoolingRate = 0.0d0
  ELSE
    ExchCond(ExNum)%SensHeatingRate = 0.0d0
    ExchCond(ExNum)%SensCoolingRate = ABS(SensHeatRecRate)
  END IF
  IF (LatHeatRecRate .GT. 0.0d0) THEN
    ExchCond(ExNum)%LatHeatingRate = LatHeatRecRate
    ExchCond(ExNum)%LatCoolingRate = 0.0d0
  ELSE
    ExchCond(ExNum)%LatHeatingRate = 0.0d0
    ExchCond(ExNum)%LatCoolingRate = ABS(LatHeatRecRate)
  END IF
  IF (TotHeatRecRate .GT. 0.0d0) THEN
    ExchCond(ExNum)%TotHeatingRate = TotHeatRecRate
    ExchCond(ExNum)%TotCoolingRate = 0.0d0
  ELSE
    ExchCond(ExNum)%TotHeatingRate = 0.0d0
    ExchCond(ExNum)%TotCoolingRate = ABS(TotHeatRecRate)
  END IF


  ExchCond(ExNum)%ElecUseRate = ElecCons

RETURN
END SUBROUTINE CalcAirToAirPlateHeatExch

SUBROUTINE CalcAirToAirGenericHeatExch(ExNum, HXUnitOn, FirstHVACIteration, EconomizerFlag, HighHumCtrlFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Don Shirey
          !       DATE WRITTEN   February 2003
          !       MODIFIED       R. Raustad - FSEC, Feb 2009 - added economizer flags
          !                      Both the economizer and high humidity control flags can disable the HX
          !       RE-ENGINEERED  Richard Raustad, June 2003

          ! PURPOSE OF THIS SUBROUTINE:
          !  Calculate the outlet conditions for an air to air generic heat
          !  exchanger given the inlet conditions.

          ! METHODOLOGY EMPLOYED:
          !  This is a standard heat exchanger effectiveness model. No geometrical input data
          !  is needed. The model uses heat exchanger effectiveness performance data
          !  to calculate the air temperature and humidity ratio of the leaving
          !  supply and secondary air streams. Linear interpolation (or extrapolation)
          !  is assumed to obtain heat exchanger effectiveness at off-rated conditions.
          !
          !  Economizer operation is allowed through the use of a Controller: Outside Air
          !  object.

          ! REFERENCES:
          !  ARI Standard 1060-2001,Rating Air-to-Air Heat Exchangers for Energy Recovery Ventilation Equipment, www.ari.org
          !  ASHRAE Standard 84, Method of Testing Air-To-Air Heat Exchangers, www.ashrae.org
          !  U.S. Environmental Protection Agency software "SAVES" -
          !   School Advanced Ventilation Engineering Software http://www.epa.gov/iaq/schooldesign/saves.html

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: ExNum    ! number of the current heat exchanger being simulated
  LOGICAL, INTENT (IN) :: HXUnitOn ! flag to simulate heat exchanger heat recovery
  LOGICAL, INTENT (IN) :: FirstHVACIteration ! first HVAC iteration flag
  LOGICAL, OPTIONAL, INTENT (IN) :: EconomizerFlag ! economizer flag pass by air loop or OA sys
  LOGICAL, OPTIONAL, INTENT (IN) :: HighHumCtrlFlag ! high humidity control flag passed by airloop or OA sys

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER  ::  ErrorTol = 0.001d0         !error tolerence

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          !
  LOGICAL :: UnitOn               ! unit on flag
  LOGICAL :: FrostControlFlag     ! unit is in frost control mode when TRUE
  INTEGER :: SupOutNode
  REAL(r64)    :: Error               ! iteration loop error variable
  REAL(r64)    :: Iter                ! iteration counter
  REAL(r64)    :: ControlFraction     ! fraction of effectiveness when rotary HX speed or plate bypass modulation is used for
                                 ! temperature control
  REAL(r64)    :: RhoSup              ! supply air density at actual pressure, temperature and humidity conditions [kg/m3]
  REAL(r64)    :: RhoSec              ! secondary air density at actual pressure, temperature and humidity conditions [kg/m3]
  REAL(r64)    :: RhoStd              ! standard air density at actual pressure, 20C dry-bulb temp and 0.0 absolute humidity [kg/m3]
  REAL(r64)    :: CSup                ! supply air heat capacity rate [W/K]
  REAL(r64)    :: CSec                ! secondary air heat capacity rate [W/K]
  REAL(r64)    :: CMin                ! minimum air heat capacity rate [W/K]
  REAL(r64)    :: QSensTrans          ! sensible heat transferred by the heat exchanger [W]
  REAL(r64)    :: QTotTrans           ! total heat (sensible + latent) transferred by the heat exchanger [W]
  REAL(r64)    :: TempSecOutSat       ! secondary air outlet temperature at saturation (at EnthsSecOut) [C]
  REAL(r64)    :: HXSecAirVolFlowRate ! air volume flow rate of the secondary air stream through the heat exchanger [m3/sec]
  REAL(r64)    :: HXSupAirVolFlowRate ! air volume flow rate of the supply air stream through the heat exchanger [m3/sec]
  REAL(r64)    :: HXAvgAirVolFlowRate ! average air volume flow rate through the heat exchanger [m3/sec]
  REAL(r64)    :: HXAirVolFlowRatio   ! ratio of avg actual air volume flow through HX to nominal HX air volume flow [-]
  REAL(r64)    :: HXTempSetPoint      ! setpoint temperature at supply outlet node of HX when ControlToTemperatureSetpoint = Yes
  REAL(r64)    :: MassFlowSecIn       ! secondary air mass flow rate at HX inlet
!  REAL(r64)    :: MassFlowSecOut      ! secondary air mass flow rate at HX outlet
  REAL(r64)    :: MassFlowSupIn       ! supply air mass flow rate at HX inlet
  REAL(r64)    :: MassFlowSupOut      ! supply air mass flow rate through HX core outlet
  REAL(r64)    :: MassFlowSupBypass   ! supply air bypass mass flow rate around HX core
  REAL(r64)    :: TempSupIn           ! supply side temperature of air entering HX
  REAL(r64)    :: TempSupOut          ! supply side temperature of air leaving HX core
  REAL(r64)    :: HumRatSupIn         ! supply side humidity ratio of air entering HX
  REAL(r64)    :: TempSecIn           ! secondary side temperature of air entering HX
  REAL(r64)    :: SensHeatRecRate     ! sensible heat recovery rate to supply air (heating +, cooling -)
  REAL(r64)    :: LatHeatRecRate      ! latent heat recovery rate to supply air (heating [humidify] +, cooling [dehumidify] -)
  REAL(r64)    :: TotHeatRecRate      ! total heat recovery rate to supply air (heating +, cooling -)
  LOGICAL      :: EconomizerActiveFlag  ! local representing the economizer status when PRESENT
  LOGICAL      :: HighHumCtrlActiveFlag ! local representing high humidity control when PRESENT

! Initialize local variables
  UnitOn = .TRUE.
  FrostControlFlag = .FALSE.
  QSensTrans = 0.0d0
  QTotTrans = 0.0d0
  ExchCond(ExNum)%DefrostFraction = 0.0d0
  ExchCond(ExNum)%SensEffectiveness = 0.0d0
  ExchCond(ExNum)%LatEffectiveness = 0.0d0
  ExchCond(ExNum)%ElecUseRate = 0.0d0
  ExchCond(ExNum)%SupOutTemp = ExchCond(ExNum)%SupInTemp
  ExchCond(ExNum)%SecOutTemp = ExchCond(ExNum)%SecInTemp
  ExchCond(ExNum)%SupOutHumRat = ExchCond(ExNum)%SupInHumRat
  ExchCond(ExNum)%SecOutHumRat = ExchCond(ExNum)%SecInHumRat
  ExchCond(ExNum)%SupOutEnth = ExchCond(ExNum)%SupInEnth
  ExchCond(ExNum)%SecOutEnth = ExchCond(ExNum)%SecInEnth
  SupOutNode = ExchCond(ExNum)%SupOutletNode
  HXTempSetPoint = Node(SupOutNode)%TempSetPoint

  IF(PRESENT(EconomizerFlag))THEN
    EconomizerActiveFlag = EconomizerFlag
  ELSE
    EconomizerActiveFlag = .FALSE.
  END IF

  IF(PRESENT(HighHumCtrlFlag))THEN
    HighHumCtrlActiveFlag = HighHumCtrlFlag
  ELSE
    HighHumCtrlActiveFlag = .FALSE.
  END IF

! Determine mass flow through heat exchanger and mass flow being bypassed (only flat plate bypasses flow)
  IF (((EconomizerActiveFlag .OR. HighHumCtrlActiveFlag) .AND. &
        ExchCond(ExNum)%EconoLockOut .EQ. EconoLockOut_Yes) .AND. &
        ExchCond(ExNum)%ExchConfigNum == PLATE) THEN
    ExchCond(ExNum)%SupBypassMassFlow = ExchCond(ExNum)%SupInMassFlow
    ExchCond(ExNum)%SupOutMassFlow = ExchCond(ExNum)%SupInMassFlow
    ExchCond(ExNum)%SecBypassMassFlow = ExchCond(ExNum)%SecInMassFlow
    ExchCond(ExNum)%SecOutMassFlow = ExchCond(ExNum)%SecInMassFlow
  ELSE ! No bypass mass flow
    ExchCond(ExNum)%SupOutMassFlow = ExchCond(ExNum)%SupInMassFlow
    ExchCond(ExNum)%SecOutMassFlow = ExchCond(ExNum)%SecInMassFlow
    ExchCond(ExNum)%SupBypassMassFlow =  0.0d0
    ExchCond(ExNum)%SecBypassMassFlow =  0.0d0
  END IF
! Unit is scheduled OFF, so bypass heat exchange calcs
  IF (GetCurrentScheduleValue(ExchCond(ExNum)%SchedPtr) .LE. 0.0d0) UnitOn = .FALSE.
!! Economizer is active, so bypass heat exchange calcs. This applies to both flat plate and rotary HX's
  IF ((EconomizerActiveFlag .OR. HighHumCtrlActiveFlag) .AND. &
       ExchCond(ExNum)%EconoLockOut .EQ. EconoLockOut_Yes)THEN
    UnitOn = .FALSE.
  END IF
! Determine if unit is ON or OFF based on air mass flow through the supply and secondary airstreams and operation flag
  IF (ExchCond(ExNum)%SupInMassFlow .LE. SmallMassFlow) UnitOn = .FALSE.
  IF (ExchCond(ExNum)%SecInMassFlow .LE. SmallMassFlow) UnitOn = .FALSE.
  IF (.NOT. HXUnitOn) UnitOn = .FALSE.
  IF(ExchCond(ExNum)%NomSupAirVolFlow == 0.d0)UnitOn = .FALSE.

  IF (UnitOn) THEN
    ! Unit is on.
    ! In the future, use actual node pressures in the following air density calls
    RhoStd = PsyRhoAirFnPbTdbW(OutBaroPress,20.0d0, 0.0d0)
    HXSupAirVolFlowRate = ExchCond(ExNum)%SupOutMassFlow/RhoStd ! volume flow using standard density
    HXSecAirVolFlowRate = ExchCond(ExNum)%SecOutMassFlow/RhoStd
    ! Limit unbalanced volumetric flow ratio to 2:1
    IF(.NOT. WarmupFlag .AND. .NOT. FirstHVACIteration)THEN
      IF (HXSupAirVolFlowRate .NE. 0.0d0 .AND. HXSecAirVolFlowRate .NE. 0.0d0) THEN
        IF (((HXSupAirVolFlowRate/HXSecAirVolFlowRate) .GT. 2.0d0).OR.((HXSecAirVolFlowRate/HXSupAirVolFlowRate) .GT. 2.0d0)) THEN
          ExchCond(ExNum)%UnBalancedErrCount = ExchCond(ExNum)%UnBalancedErrCount + 1
          IF (ExchCond(ExNum)%UnBalancedErrCount .LE. 2) THEN
             CALL ShowSevereError(TRIM(cHXTypes(ExchCond(ExNum)%ExchTypeNum))//': "'//TRIM(ExchCond(ExNum)%Name)// &
              '" unbalanced air volume flow ratio through the heat exchanger is greater than 2:1.')
             CALL ShowContinueErrorTimeStamp('...HX Supply air to Exhaust air flow ratio = '// &
                TRIM(RoundSigDigits(HXSupAirVolFlowRate/HXSecAirVolFlowRate,5))//'.')
          ELSE
            CALL ShowRecurringWarningErrorAtEnd(TRIM(cHXTypes(ExchCond(ExNum)%ExchTypeNum))// &
            ' "'//TRIM(ExchCond(ExNum)%Name)//'":  Unbalanced air volume flow ratio exceeds '// &
            ' 2:1 warning continues. HX flow ratio statistics follow.' &
            , ExchCond(ExNum)%UnBalancedErrIndex, HXSupAirVolFlowRate/HXSecAirVolFlowRate, &
              HXSupAirVolFlowRate/HXSecAirVolFlowRate)
          END IF
        END IF
      END IF
    END IF
    ! Calculate average volumetric flow rate of the two air streams
    HXAvgAirVolFlowRate = (HXSecAirVolFlowRate + HXSupAirVolFlowRate)/2.0d0
    HXAirVolFlowRatio = HXAvgAirVolFlowRate/ExchCond(ExNum)%NomSupAirVolFlow
    ! Average air volume flow rate must be between 50% and 130% of nominal supply air volume flow
    IF (HXAirVolFlowRatio .GT. 1.3d0 .OR. HXAirVolFlowRatio .LT. 0.5d0) THEN
      IF(.NOT. WarmupFlag .AND. .NOT. FirstHVACIteration)THEN
         ExchCond(ExNum)%LowFlowErrCount = ExchCond(ExNum)%LowFlowErrCount + 1
         IF (ExchCond(ExNum)%LowFlowErrCount .EQ. 1) THEN
           CALL ShowWarningError(TRIM(cHXTypes(ExchCond(ExNum)%ExchTypeNum))//' "'//TRIM(ExchCond(ExNum)%Name)//'"')
           CALL ShowContinueError('Average air volume flow rate is <50% or >130% of the nominal HX supply air volume flow rate.')
           CALL ShowContinueErrorTimeStamp('Air volume flow rate ratio = '//TRIM(RoundSigDigits(HXAirVolFlowRatio,3))//'.')
         ELSE
           CALL ShowRecurringWarningErrorAtEnd(TRIM(cHXTypes(ExchCond(ExNum)%ExchTypeNum))//' "'//TRIM(ExchCond(ExNum)%Name)//&
           '":  Average air volume flow rate is <50% or >130% warning continues. Air flow rate ratio statistics follow.' &
           , ExchCond(ExNum)%LowFlowErrIndex, HXAirVolFlowRatio, HXAirVolFlowRatio)
         END IF
       END IF
    END IF

    ! Determine heat exchanger effectiveness using avg air volume flow rate based on actual inlet air density
    ! Linearly interpolate and extrapolate (within limits) from effectiveness input values
    RhoSup = PsyRhoAirFnPbTdbW(OutBaroPress,ExchCond(ExNum)%SupInTemp,ExchCond(ExNum)%SupInHumRat)
    RhoSec = PsyRhoAirFnPbTdbW(OutBaroPress,ExchCond(ExNum)%SecInTemp,ExchCond(ExNum)%SecInHumRat)
    HXSupAirVolFlowRate = ExchCond(ExNum)%SupOutMassFlow/RhoSup
    HXSecAirVolFlowRate = ExchCond(ExNum)%SecOutMassFlow/RhoSec
    HXAvgAirVolFlowRate = (HXSecAirVolFlowRate + HXSupAirVolFlowRate)/2.0d0
    HXAirVolFlowRatio = HXAvgAirVolFlowRate/ExchCond(ExNum)%NomSupAirVolFlow

    IF (ExchCond(ExNum)%SupInTemp .LT. ExchCond(ExNum)%SecInTemp) THEN
     ! Use heating effectiveness values
      ExchCond(ExNum)%SensEffectiveness = ExchCond(ExNum)%HeatEffectSensible75 + &
                     (ExchCond(ExNum)%HeatEffectSensible100 - ExchCond(ExNum)%HeatEffectSensible75)* &
                     (HXAirVolFlowRatio - 0.75d0)/(1.d0 - 0.75d0)
      ExchCond(ExNum)%LatEffectiveness = ExchCond(ExNum)%HeatEffectLatent75 + &
                    (ExchCond(ExNum)%HeatEffectLatent100 - ExchCond(ExNum)%HeatEffectLatent75)* &
                    (HXAirVolFlowRatio - 0.75d0)/(1.d0 - 0.75d0)
    ELSE
      ! Use cooling effectiveness values
      ExchCond(ExNum)%SensEffectiveness = ExchCond(ExNum)%CoolEffectSensible75 + &
                     (ExchCond(ExNum)%CoolEffectSensible100 - ExchCond(ExNum)%CoolEffectSensible75)* &
                     (HXAirVolFlowRatio - 0.75d0)/(1.d0 - 0.75d0)
      ExchCond(ExNum)%LatEffectiveness = ExchCond(ExNum)%CoolEffectLatent75 + &
                     (ExchCond(ExNum)%CoolEffectLatent100 - ExchCond(ExNum)%CoolEffectLatent75)* &
                     (HXAirVolFlowRatio - 0.75d0)/(1.d0 - 0.75d0)
    END IF

!     Keep effectiveness between 0 and 1.0 ??
!     HXOpSensEffect = MAX(MIN(HXOpSensEffect,1.0),0.0)
!     HXOpLatEffect =  MAX(MIN(HXOpLatEffect,1.0),0.0)

!   The model should at least guard against negative numbers
    ExchCond(ExNum)%SensEffectiveness = MAX(0.d0,ExchCond(ExNum)%SensEffectiveness)
    ExchCond(ExNum)%LatEffectiveness = MAX(0.d0,ExchCond(ExNum)%LatEffectiveness)

    ! Use the effectiveness to calculate the air conditions exiting the heat exchanger (all air flow through the HX)
    !
    ! Include EATR and OACF in the following calculations at some point

    CSup = ExchCond(ExNum)%SupOutMassFlow * PsyCpAirFnWTdb(ExchCond(ExNum)%SupInHumRat,ExchCond(ExNum)%SupInTemp)
    CSec = ExchCond(ExNum)%SecOutMassFlow * PsyCpAirFnWTdb(ExchCond(ExNum)%SecInHumRat,ExchCond(ExNum)%SecInTemp)
    CMin = MIN(CSup, CSec)

    ExchCond(ExNum)%SupOutTemp = ExchCond(ExNum)%SupInTemp + &
        ExchCond(ExNum)%SensEffectiveness * CMin / CSup * (ExchCond(ExNum)%SecInTemp - ExchCond(ExNum)%SupInTemp)
    ExchCond(ExNum)%SupOutHumRat = ExchCond(ExNum)%SupInHumRat + &
        ExchCond(ExNum)%LatEffectiveness * CMin / CSup * (ExchCond(ExNum)%SecInHumRat - ExchCond(ExNum)%SupInHumRat)
    ExchCond(ExNum)%SupOutEnth = PsyHFnTdbW(ExchCond(ExNum)%SupOutTemp,ExchCond(ExNum)%SupOutHumRat)

!   Check for saturation in supply outlet and reset temp, then humidity ratio at constant enthalpy
    IF (PsyTsatFnHPb(ExchCond(ExNum)%SupOutEnth,OutBaroPress).GT.ExchCond(ExNum)%SupOutTemp) THEN
      ExchCond(ExNum)%SupOutTemp = PsyTsatFnHPb(ExchCond(ExNum)%SupOutEnth,OutBaroPress)
      ExchCond(ExNum)%SupOutHumRat = PsyWFnTdbH(ExchCond(ExNum)%SupOutTemp,ExchCond(ExNum)%SupOutEnth)
    END IF
    QSensTrans = CSup * (ExchCond(ExNum)%SupInTemp - ExchCond(ExNum)%SupOutTemp)
    ExchCond(ExNum)%SecOutTemp = ExchCond(ExNum)%SecInTemp + QSensTrans / CSec
    QTotTrans = ExchCond(ExNum)%SupOutMassFlow * (ExchCond(ExNum)%SupInEnth - ExchCond(ExNum)%SupOutEnth)
    ExchCond(ExNum)%SecOutEnth = ExchCond(ExNum)%SecInEnth + QTotTrans/ExchCond(ExNum)%SecOutMassFlow
    ExchCond(ExNum)%SecOutHumRat = PsyWFnTdbH(ExchCond(ExNum)%SecOutTemp,ExchCond(ExNum)%SecOutEnth)
!
!   Control the supply air outlet temperature to a setpoint for Heating Mode only
!   (ControlFraction = 0 HX fully bypassed, ControlFraction = 1 air passed entirely through HX)
!   (supply air stream bypass mass flow rate proportional to ControlFraction except when frost control is active)
    IF(ExchCond(ExNum)%ControlToTemperatureSetPoint .AND. &
     ExchCond(ExNum)%SupInTemp .LT. HXTempSetPoint)THEN
!     IF secondary inlet temperature is above the supply inlet temperature, control to SP
      IF(ExchCond(ExNum)%SecInTemp .GT. ExchCond(ExNum)%SupInTemp .AND. &
        (ExchCond(ExNum)%SupInTemp - ExchCond(ExNum)%SupOutTemp).NE. 0.0d0)THEN
          ControlFraction = MAX(0.0d0,MIN(1.0d0,(ExchCond(ExNum)%SupInTemp - HXTempSetPoint)/ &
                                (ExchCond(ExNum)%SupInTemp - ExchCond(ExNum)%SupOutTemp)))
      ELSE
!     ELSE fully bypass HX to maintain supply outlet temp as high as possible
        ControlFraction = 0.0d0
      ENDIF
      IF (ExchCond(ExNum)%ExchConfigNum == ROTARY) THEN
!       Rotory HX's never get bypassed, rotational speed is modulated
        ExchCond(ExNum)%SensEffectiveness = ControlFraction * ExchCond(ExNum)%SensEffectiveness
        ExchCond(ExNum)%LatEffectiveness = ControlFraction * ExchCond(ExNum)%LatEffectiveness
      ELSE ! HX is a plate heat exchanger, bypass air to control SA temperature
        Error = 1.0d0
        Iter = 0.0d0
        MassFlowSupIn = ExchCond(ExNum)%SupInMassFlow
        MassFlowSupOut = ExchCond(ExNum)%SupOutMassFlow
        MassFlowSupBypass = ExchCond(ExNum)%SupBypassMassFlow
        MassFlowSecIn = ExchCond(ExNum)%SecInMassFlow
        TempSupIn = ExchCond(ExNum)%SupInTemp
        TempSupOut = ExchCond(ExNum)%SupOutTemp
        HumRatSupIn = ExchCond(ExNum)%SupInHumRat
        TempSecIn = ExchCond(ExNum)%SecInTemp
        DO WHILE ((ABS(Error).GT.ErrorTol .AND. Iter.lt.10 .AND. ControlFraction.LT.1.0d0) &
                  .OR. Iter .eq. 1)
          MassFlowSupOut = MassFlowSupIn*ControlFraction
          MassFlowSupBypass = MassFlowSupIn*(1.d0-ControlFraction)
          HXSupAirVolFlowRate = MassFlowSupOut/RhoSup
          HXAvgAirVolFlowRate = (HXSecAirVolFlowRate + HXSupAirVolFlowRate)/2.0d0
          HXAirVolFlowRatio = HXAvgAirVolFlowRate/ExchCond(ExNum)%NomSupAirVolFlow
          CSup = MassFlowSupOut * PsyCpAirFnWTdb(HumRatSupIn,TempSupIn)
          CMin = MIN(CSup,CSec)
          IF (TempSupIn .LT. TempSecIn) THEN
!          Use heating effectiveness values
            ExchCond(ExNum)%SensEffectiveness = ExchCond(ExNum)%HeatEffectSensible75 + &
             (ExchCond(ExNum)%HeatEffectSensible100 - ExchCond(ExNum)%HeatEffectSensible75)* &
             (HXAirVolFlowRatio - 0.75d0)/(1.d0 - 0.75d0)
            ExchCond(ExNum)%LatEffectiveness = ExchCond(ExNum)%HeatEffectLatent75 + &
             (ExchCond(ExNum)%HeatEffectLatent100 - ExchCond(ExNum)%HeatEffectLatent75)* &
             (HXAirVolFlowRatio - 0.75d0)/(1.d0 - 0.75d0)
          ELSE
!          Use cooling effectiveness values
            ExchCond(ExNum)%SensEffectiveness = ExchCond(ExNum)%CoolEffectSensible75 + &
             (ExchCond(ExNum)%CoolEffectSensible100 - ExchCond(ExNum)%CoolEffectSensible75)* &
             (HXAirVolFlowRatio - 0.75d0)/(1.d0 - 0.75d0)
            ExchCond(ExNum)%LatEffectiveness = ExchCond(ExNum)%CoolEffectLatent75 + &
             (ExchCond(ExNum)%CoolEffectLatent100 - ExchCond(ExNum)%CoolEffectLatent75)* &
             (HXAirVolFlowRatio - 0.75d0)/(1.d0 - 0.75d0)
          END IF
          IF(CSup .EQ. 0.0d0)THEN
!          IF CSup = 0, then supply air mass flow rate = 0 and HX is fully bypassed. Fix divide by 0 error below DO loop.
            CSup = 1.0d0
            CMin = 0.0d0
            EXIT
          END IF
          TempSupOut = (MassFlowSupOut * (TempSupIn + &
            ExchCond(ExNum)%SensEffectiveness * CMin / CSup * (TempSecIn - TempSupIn)) +&
            MassFlowSupBypass * TempSupIn)/MassFlowSupIn
          Error =  (TempSupOut - HXTempSetPoint)
!         IF supply inlet temp = supply outlet temp, fully bypass HX - ELSE control to SP
          IF(TempSupIn .NE. TempSupOut)THEN
            ControlFraction = MAX(0.0d0,MIN(1.0d0,ControlFraction * (TempSupIn - HXTempSetPoint)/&
                            (TempSupIn - TempSupOut)))
          ELSE IF(ABS(TempSupOut - HXTempSetPoint) .LT. ErrorTol)THEN
!           IF TempSupIn = TempSupOut then TempSecIn = TempSupIn (ControlFraction = ?)
!           Do nothing, variables in ELSE below have already been calculated
            EXIT
          ELSE
!           or HX is fully bypassed (ControlFraction = 0) which actually should be caught in IF(CSup .EQ. 0.0)THEN above.
            ControlFraction = 0.0d0
            MassFlowSupOut = MassFlowSupIn*ControlFraction
            MassFlowSupBypass = MassFlowSupIn*(1.d0-ControlFraction)
            CSup = 1.0d0
            CMin = 0.0d0
            EXIT
          END IF
          Iter = Iter + 1
        END DO

        ExchCond(ExNum)%SupInMassFlow = MassFlowSupIn
        ExchCond(ExNum)%SupOutMassFlow = MassFlowSupOut
        ExchCond(ExNum)%SupBypassMassFlow = MassFlowSupBypass
        ExchCond(ExNum)%SecInMassFlow = MassFlowSecIn
        ExchCond(ExNum)%SupInTemp = TempSupIn
        ExchCond(ExNum)%SupOutTemp = TempSupOut
        ExchCond(ExNum)%SupInHumRat = HumRatSupIn
        ExchCond(ExNum)%SecInTemp = TempSecIn

      END IF ! ENDIF for "IF (ExchCond(ExNum)%ExchConfig == 'ROTARY') THEN"
      ExchCond(ExNum)%SupOutTemp = ExchCond(ExNum)%SupInTemp + &
        ExchCond(ExNum)%SensEffectiveness*CMin/CSup*(ExchCond(ExNum)%SecInTemp-ExchCond(ExNum)%SupInTemp)
      ExchCond(ExNum)%SupOutHumRat = ExchCond(ExNum)%SupInHumRat + &
        ExchCond(ExNum)%LatEffectiveness*CMin/CSup*(ExchCond(ExNum)%SecInHumRat-ExchCond(ExNum)%SupInHumRat)
      ExchCond(ExNum)%SupOutEnth = PsyHFnTdbW(ExchCond(ExNum)%SupOutTemp,ExchCond(ExNum)%SupOutHumRat)

!     Check for saturation in supply outlet and reset temp, then humidity ratio at constant enthalpy
      IF (PsyTsatFnHPb(ExchCond(ExNum)%SupOutEnth,OutBaroPress).GT.ExchCond(ExNum)%SupOutTemp) THEN
        ExchCond(ExNum)%SupOutTemp = PsyTsatFnHPb(ExchCond(ExNum)%SupOutEnth,OutBaroPress)
        ExchCond(ExNum)%SupOutHumRat = PsyWFnTdbH(ExchCond(ExNum)%SupOutTemp,ExchCond(ExNum)%SupOutEnth)
      END IF

      QSensTrans = CSup * (ExchCond(ExNum)%SupInTemp - ExchCond(ExNum)%SupOutTemp)
      ExchCond(ExNum)%SecOutTemp = ExchCond(ExNum)%SecInTemp + QSensTrans / CSec
      QTotTrans = ExchCond(ExNum)%SupOutMassFlow * (ExchCond(ExNum)%SupInEnth - ExchCond(ExNum)%SupOutEnth)
      ExchCond(ExNum)%SecOutEnth = ExchCond(ExNum)%SecInEnth + QTotTrans/ExchCond(ExNum)%SecOutMassFlow
      ExchCond(ExNum)%SecOutHumRat = PsyWFnTdbH(ExchCond(ExNum)%SecOutTemp,ExchCond(ExNum)%SecOutEnth)

    END IF !ENDIF for "IF(ExchCond(ExNum)%ControlToTemperatureSetPoint .AND... THEN, ELSE"

    IF( (ExchCond(ExNum)%FrostControlType == 'MINIMUMEXHAUSTTEMPERATURE' .AND. &
      ExchCond(ExNum)%SecOutTemp .LT. ExchCond(ExNum)%ThresholdTemperature) .OR. &
        (ExchCond(ExNum)%FrostControlType == 'EXHAUSTAIRRECIRCULATION' .AND. &
      ExchCond(ExNum)%SupInTemp .LE. ExchCond(ExNum)%ThresholdTemperature) .OR. &
        (ExchCond(ExNum)%FrostControlType == 'EXHAUSTONLY' .AND. &
      ExchCond(ExNum)%SupInTemp .LE. ExchCond(ExNum)%ThresholdTemperature))THEN
      CALL FrostControl(ExNum)
      FrostControlFlag = .TRUE.
    END IF

     ! check for saturation in secondary outlet
    TempSecOutSat = PsyTsatFnHPb(ExchCond(ExNum)%SecOutEnth,OutBaroPress)
    IF (TempSecOutSat.GT.ExchCond(ExNum)%SecOutTemp) THEN
      ExchCond(ExNum)%SecOutTemp = TempSecOutSat
      ExchCond(ExNum)%SecOutHumRat = PsyWFnTdbH(ExchCond(ExNum)%SecOutTemp,ExchCond(ExNum)%SecOutEnth)
    END IF

    ! calculate outlet conditions by mixing bypass air stream with air that went through the
    ! heat exchanger core.  Perform this mixing only when no frost control is used or
    ! heat exchanger is not in frost control mode.  Mixing similar to this is performed
    ! in the frost control subroutine when in frost control mode.
    IF(.NOT. FrostControlFlag)THEN
      ExchCond(ExNum)%SupOutEnth = (ExchCond(ExNum)%SupOutMassFlow*ExchCond(ExNum)%SupOutEnth &
                                 + ExchCond(ExNum)%SupBypassMassFlow*ExchCond(ExNum)%SupInEnth) &
                                   / ExchCond(ExNum)%SupInMassFlow
      ExchCond(ExNum)%SupOutHumRat = (ExchCond(ExNum)%SupOutMassFlow*ExchCond(ExNum)%SupOutHumRat &
                                 + ExchCond(ExNum)%SupBypassMassFlow*ExchCond(ExNum)%SupInHumRat) &
                                   / ExchCond(ExNum)%SupInMassFlow
      ExchCond(ExNum)%SupOutTemp = PsyTdbFnHW(ExchCond(ExNum)%SupOutEnth,ExchCond(ExNum)%SupOutHumRat)
      ExchCond(ExNum)%SupOutMassFlow = ExchCond(ExNum)%SupInMassFlow
      ExchCond(ExNum)%SecOutEnth = (ExchCond(ExNum)%SecOutMassFlow*ExchCond(ExNum)%SecOutEnth &
                                 + ExchCond(ExNum)%SecBypassMassFlow*ExchCond(ExNum)%SecInEnth) &
                                   / ExchCond(ExNum)%SecInMassFlow
      ExchCond(ExNum)%SecOutHumRat = (ExchCond(ExNum)%SecOutMassFlow*ExchCond(ExNum)%SecOutHumRat &
                                 + ExchCond(ExNum)%SecBypassMassFlow*ExchCond(ExNum)%SecInHumRat) &
                                   / ExchCond(ExNum)%SecInMassFlow
      ExchCond(ExNum)%SecOutTemp = PsyTdbFnHW(ExchCond(ExNum)%SecOutEnth,ExchCond(ExNum)%SecOutHumRat)
      ExchCond(ExNum)%SecOutMassFlow = ExchCond(ExNum)%SecInMassFlow
    END IF

    ExchCond(ExNum)%ElecUseRate = ExchCond(ExNum)%NomElecPower

  END IF !ENDIF for "IF (UnitOn) THEN"

! Calculate heat transfer from the unit using the final supply inlet and supply outlet air conditions
  CSup = ExchCond(ExNum)%SupOutMassFlow*PsyCpAirFnWTdb(ExchCond(ExNum)%SupInHumRat,ExchCond(ExNum)%SupInTemp)
  SensHeatRecRate = CSup * (ExchCond(ExNum)%SupOutTemp - ExchCond(ExNum)%SupInTemp)
  TotHeatRecRate = ExchCond(ExNum)%SupOutMassFlow * ( ExchCond(ExNum)%SupOutEnth - &
                                   ExchCond(ExNum)%SupInEnth )
  LatHeatRecRate = TotHeatRecRate - SensHeatRecRate

! Set report variables based on sign of recovery rate
  IF (SensHeatRecRate .GT. 0.0d0) THEN
    ExchCond(ExNum)%SensHeatingRate = SensHeatRecRate
    ExchCond(ExNum)%SensCoolingRate = 0.0d0
  ELSE
    ExchCond(ExNum)%SensHeatingRate = 0.0d0
    ExchCond(ExNum)%SensCoolingRate = ABS(SensHeatRecRate)
  END IF
  IF (LatHeatRecRate .GT. 0.0d0) THEN
    ExchCond(ExNum)%LatHeatingRate = LatHeatRecRate
    ExchCond(ExNum)%LatCoolingRate = 0.0d0
  ELSE
    ExchCond(ExNum)%LatHeatingRate = 0.0d0
    ExchCond(ExNum)%LatCoolingRate = ABS(LatHeatRecRate)
  END IF
  IF (TotHeatRecRate .GT. 0.0d0) THEN
    ExchCond(ExNum)%TotHeatingRate = TotHeatRecRate
    ExchCond(ExNum)%TotCoolingRate = 0.0d0
  ELSE
    ExchCond(ExNum)%TotHeatingRate = 0.0d0
    ExchCond(ExNum)%TotCoolingRate = ABS(TotHeatRecRate)
  END IF


RETURN
END SUBROUTINE CalcAirToAirGenericHeatExch

SUBROUTINE CalcDesiccantBalancedHeatExch(ExNum, HXUnitOn, FirstHVACIteration, FanOpMode, PartLoadRatio, &
                                         CompanionCoilIndex, RegenInletIsOANode, EconomizerFlag, HighHumCtrlFlag)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Mangesh Basarkar, FSEC
          !       DATE WRITTEN   January 2007
          !       MODIFIED       R. Raustad - FSEC, Feb 2009 - added economizer flags
          !                      Both the economizer and high humidity control flags can disable the HX
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !  Calculate the outlet conditions for a balanced air-to-air desiccant heat exchanger
          !  given the inlet conditions and face velocity. Performance map is provided by user.

          ! METHODOLOGY EMPLOYED:
          !  This is an empirical heat exchanger model. The model uses heat exchanger performance data to
          !  calculate the air temperature and humidity ratio of the leaving upply and secondary air streams.
          !
          !  Humidity control can enable/disable heat recovery through the use of the HXUnitOn Subroutine argument.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DXCoils,      ONLY: DXCoilPartLoadRatio
  USE DataLoopNode, ONLY: SensedNodeFlagValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)   :: ExNum                  ! number of the current heat exchanger being simulated
  LOGICAL, INTENT (IN)   :: HXUnitOn               ! flag to simulate heat exchager heat recovery
  LOGICAL, INTENT (IN)   :: FirstHVACIteration     ! First HVAC iteration flag
  INTEGER, INTENT (IN)   :: FanOpMode              ! Supply air fan operating mode (1=cycling, 2=constant)
  REAL(r64), INTENT (IN) :: PartLoadRatio          ! Part load ratio requested of DX compressor
  INTEGER, INTENT (IN)   :: CompanionCoilIndex     ! index of companion cooling coil
  LOGICAL, INTENT (IN)   :: RegenInletIsOANode     ! Flag to determine if regen side inlet is OANode, if so this air stream cycles
  LOGICAL, OPTIONAL, INTENT (IN) :: EconomizerFlag ! economizer flag pass by air loop or OA sys
  LOGICAL, OPTIONAL, INTENT (IN) :: HighHumCtrlFlag ! high humidity control flag passed by airloop or OA sys

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER  ::  ErrorTol = 0.001d0         !error tolerence

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          !
  LOGICAL :: UnitOn              ! unit on flag
  REAL(r64)    :: RhoStd              ! standard air density at actual pressure, 20C dry-bulb temp and 0.0 absolute humidity [kg/m3]
  REAL(r64)    :: CSup                ! supply air heat capacity rate [W/K]
  REAL(r64)    :: CSec                ! secondary air heat capacity rate [W/K]
  REAL(r64)    :: TempSecOutSat       ! secondary air outlet temperature at saturation (at EnthsSecOut) [C]
  REAL(r64)    :: SensHeatRecRate     ! sensible heat recovery rate to supply air (heating +, cooling -)
  REAL(r64)    :: TotHeatRecRate      ! total heat recovery rate to supply air (heating +, cooling -)
  REAL(r64)    :: ProcessSensHeatRecRate ! process sensible heat recovery rate (heating +, cooling -)
  REAL(r64)    :: ProcessTotHeatRecRate  ! process total heat recovery rate (heating +, cooling -)
  REAL(r64)    :: ProcessLatHeatRecRate  ! process latent heat recovery rate (heating [humidify] +, cooling [dehumidify] -)
  REAL(r64)    :: SupInMassFlow       ! Supply side HX mass flow rate
  REAL(r64)    :: SecInMassFlow       ! Secondary side HX mass flow rate

  REAL(r64) :: Coeff1                 ! coefficient1 to empirical model (used for both temperature and humidity ratio equations)
  REAL(r64) :: Coeff2                 ! coefficient2 to empirical model (used for both temperature and humidity ratio equations)
  REAL(r64) :: Coeff3                 ! coefficient3 to empirical model (used for both temperature and humidity ratio equations)
  REAL(r64) :: Coeff4                 ! coefficient4 to empirical model (used for both temperature and humidity ratio equations)
  REAL(r64) :: Coeff5                 ! coefficient5 to empirical model (used for both temperature and humidity ratio equations)
  REAL(r64) :: Coeff6                 ! coefficient6 to empirical model (used for both temperature and humidity ratio equations)
  REAL(r64) :: Coeff7                 ! coefficient7 to empirical model (used for both temperature and humidity ratio equations)
  REAL(r64) :: Coeff8                 ! coefficient8 to empirical model (used for both temperature and humidity ratio equations)
  REAL(r64) :: BalFaceVelActual       ! operating face velocity [m/s]
  REAL(r64) :: FullLoadSupOutTemp     ! empirical model supply outlet temperature [C]
  REAL(r64) :: FullLoadSupOutHumRat   ! empirical model supply outlet humidity ratio [kg/kg]
  REAL(r64) :: FullLoadDeltaT         ! empirical model heat exchanger delta temperature [C]
  REAL(r64) :: FullLoadDeltaW         ! empirical model heat exchanger delta humidity ratio [kg/kg]
  REAL(r64) :: T_RegenInTemp          ! empirical model supply (regen) inlet temperature for temperature equation [C]
  REAL(r64) :: T_RegenInHumRat        ! empirical model supply (regen) inlet humidity ratio for temperature equation [kg/kg]
  REAL(r64) :: T_ProcInTemp           ! empirical model secondary (process) inlet temperature for temperature equation [C]
  REAL(r64) :: T_ProcInHumRat         ! empirical model secondary (process) inlet humidity ratio for temperature equation [kg/kg]
  REAL(r64) :: T_FaceVel              ! empirical model face velocity for temperature equation [m/s]
  REAL(r64) :: H_RegenInTemp          ! empirical model supply (regen) inlet temperature for humidity ratio equation [C]
  REAL(r64) :: H_RegenInHumRat        ! empirical model supply (regen) inlet humidity ratio for humidity ratio equation [kg/kg]
  REAL(r64) :: H_ProcInTemp           ! empirical model secondary (process) inlet temperature for humidity ratio equation [C]
  REAL(r64) :: H_ProcInHumRat         ! empirical model secondary (process) inlet humidity ratio for humidity ratio equation [kg/kg]
  REAL(r64) :: H_FaceVel              ! empirical model face velocity for humidity ratio equation [m/s]
  REAL(r64) :: MaxHumRatNeeded        ! maximum humidity ratio setpoint for balanced desiccant HX [kg/kg]
  REAL(r64) :: MinHumRatNeeded        ! minimum humidity ratio setpoint for balanced desiccant HX [kg/kg]
  REAL(r64) :: HXPartLoadRatio        ! local heat exchanger part-load ratio
  REAL(r64) :: TestSaturationEnthalpy ! enthalpy used to test for regeneration outlet condition over saturation curve (J/kg)
  CHARACTER(len=32), SAVE :: ThisSub = 'CalcDesiccantBalancedHeatExch:'! Used to pass to Psyc routines
  REAL(r64) :: AverageMassFlowRate    ! average of supply (regen) and secondary (process) mass flow rates [kg/s]
  LOGICAL   :: EconomizerActiveFlag   ! local representing the economizer status when PRESENT
  LOGICAL   :: HighHumCtrlActiveFlag  ! local representing high humidity control when PRESENT

! Initialize local variables
  UnitOn = .TRUE.
  SensHeatRecRate = 0.0d0
  TotHeatRecRate = 0.0d0
  HXPartLoadRatio = PartLoadRatio
  ExchCond(ExNum)%DefrostFraction = 0.0d0
  ExchCond(ExNum)%ElecUseRate  = 0.0d0
  ExchCond(ExNum)%SupOutTemp   = ExchCond(ExNum)%SupInTemp
  ExchCond(ExNum)%SecOutTemp   = ExchCond(ExNum)%SecInTemp
  ExchCond(ExNum)%SupOutHumRat = ExchCond(ExNum)%SupInHumRat
  ExchCond(ExNum)%SecOutHumRat = ExchCond(ExNum)%SecInHumRat
  ExchCond(ExNum)%SupOutEnth   = ExchCond(ExNum)%SupInEnth
  ExchCond(ExNum)%SecOutEnth   = ExchCond(ExNum)%SecInEnth
  ExchCond(ExNum)%SupOutMassFlow = ExchCond(ExNum)%SupInMassFlow
  ExchCond(ExNum)%SecOutMassFlow = ExchCond(ExNum)%SecInMassFlow
  AverageMassFlowRate = (ExchCond(ExNum)%SupOutMassFlow + ExchCond(ExNum)%SecOutMassFlow) / 2.0d0

  IF(PRESENT(EconomizerFlag))THEN
    EconomizerActiveFlag = EconomizerFlag
  ELSE
    EconomizerActiveFlag = .FALSE.
  END IF

  IF(PRESENT(HighHumCtrlFlag))THEN
    HighHumCtrlActiveFlag = HighHumCtrlFlag
  ELSE
    HighHumCtrlActiveFlag = .FALSE.
  END IF

! Unit is scheduled OFF, so bypass heat exchange calcs
  IF (GetCurrentScheduleValue(ExchCond(ExNum)%SchedPtr) .LE. 0.0d0) UnitOn = .FALSE.
! Determine if unit is ON or OFF based on air mass flow through the supply and secondary airstreams and operation flag
  IF (ExchCond(ExNum)%SupInMassFlow .LE. SmallMassFlow) UnitOn = .FALSE.
  IF (ExchCond(ExNum)%SecInMassFlow .LE. SmallMassFlow) UnitOn = .FALSE.
  IF (HXPartLoadRatio .EQ. 0.0d0) UnitOn = .FALSE.
  IF (.NOT. HXUnitOn) UnitOn = .FALSE.
  IF ((EconomizerActiveFlag .OR. HighHumCtrlActiveFlag) .AND. &
       ExchCond(ExNum)%EconoLockOut .EQ. EconoLockOut_Yes) UnitOn = .FALSE.

  IF (UnitOn) THEN

!   Use local variables to perform checks
    SecInMassFlow = ExchCond(ExNum)%SecInMassFlow
    SupInMassFlow = ExchCond(ExNum)%SupInMassFlow

    ! In constant fan mode, process air mass flow rate is full flow and supply (regen) air cycles based on PLR.
    ! If supply (regen) inlet is OA node, regen mass flow rate is proportional to PLR.
    ! If both of the above is true then boost local variable up to full flow
    IF((FanOpMode .EQ. ContFanCycCoil) .AND. RegenInletIsOANode) THEN
      SupInMassFlow = SupInMassFlow / HXPartLoadRatio
    END IF
    ! for cycling fan case, boost both local variables up to full flow
    IF(FanOpMode .EQ. CycFanCycCoil) THEN
      SupInMassFlow = SupInMassFlow / HXPartLoadRatio ! supply = regen
      SecInMassFlow = SecInMassFlow / HXPartLoadRatio ! secondary = process
    END IF

    ! Check for balanced flow condition
    CALL CheckForBalancedFlow(ExNum, SecInMassFlow, SupInMassFlow, FirstHVACIteration)

    SELECT CASE (ExchCond(ExNum)%HeatExchPerfTypeNum)

    CASE(BALANCEDHX_PERFDATATYPE1)

      Coeff1 = BalDesDehumPerfData(ExchCond(ExNum)%PerfDataIndex)%B1
      Coeff2 = BalDesDehumPerfData(ExchCond(ExNum)%PerfDataIndex)%B2
      Coeff3 = BalDesDehumPerfData(ExchCond(ExNum)%PerfDataIndex)%B3
      Coeff4 = BalDesDehumPerfData(ExchCond(ExNum)%PerfDataIndex)%B4
      Coeff5 = BalDesDehumPerfData(ExchCond(ExNum)%PerfDataIndex)%B5
      Coeff6 = BalDesDehumPerfData(ExchCond(ExNum)%PerfDataIndex)%B6
      Coeff7 = BalDesDehumPerfData(ExchCond(ExNum)%PerfDataIndex)%B7
      Coeff8 = BalDesDehumPerfData(ExchCond(ExNum)%PerfDataIndex)%B8

      T_ProcInTemp    = FullLoadOutAirTemp
      T_ProcInHumRat  = FullLoadOutAirHumRat
      T_RegenInTemp   = ExchCond(ExNum)%SupInTemp
      T_RegenInHumRat = ExchCond(ExNum)%SupInHumRat

      ! Must use the same density used to convert volumetric flow rate to mass flow rate to get back to velocity
      RhoStd           = StdRhoAir  !PsyRhoAirFnPbTdbW(StdBaroPress,20.0d0, 0.0d0)
      BalFaceVelActual = SupInMassFlow / (RhoStd*ExchCond(ExNum)%FaceArea)

      T_FaceVel = BalFaceVelActual

!     Call model check routines only when HX is active, if coil is off these checks do not apply (no potential for heat transfer)
!     Check RH limits and warn user if out of bounds (T_* not modified in subroutine)

      CALL CheckModelBoundsRH_TempEq(ExNum, T_RegenInTemp, T_RegenInHumRat, T_ProcInTemp, T_ProcInHumRat, FirstHVACIteration)
!     Check model boundaries and cap empirical model independent variables as needed (T_* may be modified on return from sub)
      CALL CheckModelBoundsTempEq(ExNum, T_RegenInTemp, T_RegenInHumRat, T_ProcInTemp, T_ProcInHumRat, T_FaceVel, &
                                    FirstHVACIteration)

      IF(T_ProcInTemp .NE. 0.0d0 .AND. T_RegenInTemp .NE. 0.0d0)THEN
        FullLoadSupOutTemp = Coeff1 + Coeff2 *  T_RegenInHumRat                + &
                                      Coeff3 *  T_RegenInTemp                  + &
                                      Coeff4 * (T_RegenInHumRat/T_RegenInTemp) + &
                                      Coeff5 *  T_ProcInHumRat                 + &
                                      Coeff6 *  T_ProcInTemp                   + &
                                      Coeff7 * (T_ProcInHumRat/T_ProcInTemp)   + &
                                      Coeff8 *  T_FaceVel

        ! Check model boundary for supply (regen) temp and do not cap value if out of bounds, check that supply in temp > out temp
        Call CheckModelBoundOutput_Temp(ExNum, ExchCond(ExNum)%SupInTemp, FullLoadSupOutTemp, FirstHVACIteration)
        FullLoadDeltaT = FullLoadSupOutTemp - ExchCond(ExNum)%SupInTemp
      ELSE
        FullLoadDeltaT = 0.0d0
      END IF

      Coeff1 = BalDesDehumPerfData(ExchCond(ExNum)%PerfDataIndex)%C1
      Coeff2 = BalDesDehumPerfData(ExchCond(ExNum)%PerfDataIndex)%C2
      Coeff3 = BalDesDehumPerfData(ExchCond(ExNum)%PerfDataIndex)%C3
      Coeff4 = BalDesDehumPerfData(ExchCond(ExNum)%PerfDataIndex)%C4
      Coeff5 = BalDesDehumPerfData(ExchCond(ExNum)%PerfDataIndex)%C5
      Coeff6 = BalDesDehumPerfData(ExchCond(ExNum)%PerfDataIndex)%C6
      Coeff7 = BalDesDehumPerfData(ExchCond(ExNum)%PerfDataIndex)%C7
      Coeff8 = BalDesDehumPerfData(ExchCond(ExNum)%PerfDataIndex)%C8

      H_ProcInTemp    = FullLoadOutAirTemp
      H_ProcInHumRat  = FullLoadOutAirHumRat
      H_RegenInTemp   = ExchCond(ExNum)%SupInTemp
      H_RegenInHumRat = ExchCond(ExNum)%SupInHumRat
      H_FaceVel       = BalFaceVelActual

!     Call model check routines only when HX is active, if coil is off these checks do not apply (no potential for heat transfer)
!     Check RH limits and warn user if out of bounds (H_* not modified in subroutine)

      CALL CheckModelBoundsRH_HumRatEq(ExNum, H_RegenInTemp, H_RegenInHumRat, H_ProcInTemp, H_ProcInHumRat, FirstHVACIteration)
!     Check model boundaries and cap empirical model independent variables as needed (H_* may be modified on return from sub)
      CALL CheckModelBoundsHumRatEq(ExNum, H_RegenInTemp, H_RegenInHumRat, H_ProcInTemp, H_ProcInHumRat, H_FaceVel, &
                                    FirstHVACIteration)

!     Calc curve
      IF(H_ProcInTemp .NE. 0.0d0 .AND. H_RegenInTemp .NE. 0.0d0)THEN
        FullLoadSupOutHumRat = Coeff1 + Coeff2 *  H_RegenInHumRat                + &
                                        Coeff3 *  H_RegenInTemp                  + &
                                        Coeff4 * (H_RegenInHumRat/H_RegenInTemp) + &
                                        Coeff5 *  H_ProcInHumRat                 + &
                                        Coeff6 *  H_ProcInTemp                   + &
                                        Coeff7 * (H_ProcInHumRat/H_ProcInTemp)   + &
                                        Coeff8 *  H_FaceVel

        ! Check model boundary for supply (regen) hum rat and do not cap value if out of bounds, check that supply in HR < out HR
        Call CheckModelBoundOutput_HumRat(ExNum, ExchCond(ExNum)%SupInHumRat, FullLoadSupOutHumRat, FirstHVACIteration)
        FullLoadDeltaW =  FullLoadSupOutHumRat - ExchCond(ExNum)%SupInHumRat
      ELSE
        FullLoadDeltaW = 0.0d0
      END IF

!     Check for saturation in the model's calculated supply outlet and reset temp, then humidity ratio at constant enthalpy
!     Reset delta T and delta W such that the model does not allow an outlet condition over saturation
      TestSaturationEnthalpy = PsyHFnTdbW(FullLoadSupOutTemp,FullLoadSupOutHumRat,ThisSub//' TestSatSup')
      IF (PsyTsatFnHPb(TestSaturationEnthalpy,OutBaroPress,ThisSub//' TSat').GT.FullLoadSupOutTemp) THEN
        FullLoadSupOutTemp = PsyTsatFnHPb(TestSaturationEnthalpy,OutBaroPress,ThisSub//' TSat-FullLoadOutTemp')
        FullLoadSupOutHumRat = PsyWFnTdbH(FullLoadSupOutTemp,TestSaturationEnthalpy,ThisSub//' TSat-FullLoadOutHumRat')
        FullLoadDeltaT = FullLoadSupOutTemp - ExchCond(ExNum)%SupInTemp
        FullLoadDeltaW = FullLoadSupOutHumRat - ExchCond(ExNum)%SupInHumRat
      END IF

      IF(.NOT. CalledFromParentObject)THEN
!       calculate part-load ratio for HX
        MaxHumRatNeeded = Node(ExchCond(ExNum)%SecOutletNode)%HumRatMax
        MinHumRatNeeded = Node(ExchCond(ExNum)%SecOutletNode)%HumRatMin
        ! Calculate partload fraction of dehumidification capacity required to meet setpoint

!       check the model output, if the regen delta W is positive, the process air stream is dehumidified
        IF(FullLoadDeltaW .GT. 0)THEN
!         check for a setpoint, if no setpoint then PLR remains at 1
          IF(MaxHumRatNeeded .NE. SensedNodeFlagValue)THEN
            IF (ExchCond(ExNum)%SecInHumRat .GT. MaxHumRatNeeded .AND. MaxHumRatNeeded .GT. 0.0d0) THEN
              HXPartLoadRatio = (ExchCond(ExNum)%SecInHumRat - MaxHumRatNeeded) / FullLoadDeltaW
            ELSE
              HXPartLoadRatio = 0.0d0
            END IF
          END IF
!       check the model output, if the regen delta W is negative, the process air stream is humidified
        ELSE IF(FullLoadDeltaW .LT. 0)THEN
!         check for a setpoint, if no setpoint then PLR remains at 1
          IF(MinHumRatNeeded .NE. SensedNodeFlagValue)THEN
            IF (ExchCond(ExNum)%SecInHumRat .LT. MinHumRatNeeded .AND. MinHumRatNeeded .GT. 0.0d0) THEN
              HXPartLoadRatio = (ExchCond(ExNum)%SecInHumRat - MinHumRatNeeded) / FullLoadDeltaW
            ELSE
              HXPartLoadRatio = 0.0d0
            END IF
          END IF
        END IF

        HXPartLoadRatio = MAX(0.0d0,HXPartLoadRatio)
        HXPartLoadRatio = MIN(1.0d0,HXPartLoadRatio)

      ELSE IF (CompanionCoilIndex .GT. 0) THEN
        HXPartLoadRatio = DXCoilPartLoadRatio(CompanionCoilIndex)
      ENDIF

      IF(FanOpMode .EQ. CycFanCycCoil .OR. RegenInletIsOANode)THEN
!       Supply (regen) air stream mass flow rate is cycling and proportional to PLR, outlet conditions are full load conditions
        ExchCond(ExNum)%SupOutTemp   = ExchCond(ExNum)%SupInTemp + FullLoadDeltaT
        ExchCond(ExNum)%SupOutHumRat = MIN(1.0d0,MAX(1.d-5,ExchCond(ExNum)%SupInHumRat + FullLoadDeltaW))
      ELSE
!       Supply (regen) air stream mass flow rate is constant and outlet conditions are averaged
        ExchCond(ExNum)%SupOutTemp   = ExchCond(ExNum)%SupInTemp + (FullLoadDeltaT * HXPartLoadRatio)
        ExchCond(ExNum)%SupOutHumRat = MIN(1.0d0,MAX(1.d-5,ExchCond(ExNum)%SupInHumRat + (FullLoadDeltaW * HXPartLoadRatio)))
      END IF

!     for a balanced flow HX, use average mass flow rate and actual node conditions to calculate CSup and CSec
!     the mass flow rate on the process and secondary side of HX may be imbalanced when the HX is used in the OA branch
!     use the average mass flow rate to avoid psych warnings, mass flow rates will converge at the end of the iteration
!     if the air mass flow rates do not converge, this model should not be used
      CSup = AverageMassFlowRate * PsyCpAirFnWTdb(ExchCond(ExNum)%SupInHumRat,ExchCond(ExNum)%SupInTemp,ThisSub//' CSup')
      CSec = AverageMassFlowRate * PsyCpAirFnWTdb(ExchCond(ExNum)%SecInHumRat,ExchCond(ExNum)%SecInTemp,ThisSub//' CSec')

      ExchCond(ExNum)%SupOutEnth   = PsyHFnTdbW(ExchCond(ExNum)%SupOutTemp,ExchCond(ExNum)%SupOutHumRat,ThisSub//' SupOutEnth')

      SensHeatRecRate = CSup * (ExchCond(ExNum)%SupOutTemp - ExchCond(ExNum)%SupInTemp)

      TotHeatRecRate = AverageMassFlowRate * (ExchCond(ExNum)%SupOutEnth - ExchCond(ExNum)%SupInEnth)

!     now calculate process side heat transfer

      ExchCond(ExNum)%SecOutEnth = ExchCond(ExNum)%SecInEnth - TotHeatRecRate/AverageMassFlowRate

      ExchCond(ExNum)%SecOutTemp = ExchCond(ExNum)%SecInTemp - SensHeatRecRate/CSec

      ExchCond(ExNum)%SecOutHumRat = PsyWFnTdbH(ExchCond(ExNum)%SecOutTemp,ExchCond(ExNum)%SecOutEnth,ThisSub//' SecOutHumRat')

      ! check for saturation in process (secondary) outlet
      ! The process outlet conditions should never be over the saturation curve for the balanced desiccant model
      ! although this may occur during warmup. This check is included here for consistency.
      TempSecOutSat = PsyTsatFnHPb(ExchCond(ExNum)%SecOutEnth,OutBaroPress,ThisSub//' TestSatSec')
      IF (TempSecOutSat.GT.ExchCond(ExNum)%SecOutTemp) THEN
        ExchCond(ExNum)%SecOutTemp = TempSecOutSat
        ExchCond(ExNum)%SecOutHumRat = &
                  PsyWFnTdbH(ExchCond(ExNum)%SecOutTemp,ExchCond(ExNum)%SecOutEnth,ThisSub//' TSat-SecOutHumRat')
      END IF

      ExchCond(ExNum)%ElecUseRate = BalDesDehumPerfData(ExchCond(ExNum)%PerfDataIndex)%NomElecPower * HXPartLoadRatio

    CASE DEFAULT

    END SELECT

  END IF !ENDIF for "IF (UnitOn) THEN"

! Report the process side heat transfer
  CSec = AverageMassFlowRate * PsyCpAirFnWTdb(ExchCond(ExNum)%SecInHumRat,ExchCond(ExNum)%SecInTemp,ThisSub)
  ProcessSensHeatRecRate = CSec * (ExchCond(ExNum)%SecOutTemp - ExchCond(ExNum)%SecInTemp)

  ProcessTotHeatRecRate = ExchCond(ExNum)%SecOutMassFlow * (ExchCond(ExNum)%SecOutEnth - ExchCond(ExNum)%SecInEnth)

  ProcessLatHeatRecRate = ProcessTotHeatRecRate - ProcessSensHeatRecRate

! Set report variables based on sign of recovery rate
  IF (ProcessSensHeatRecRate .GT. 0.0d0) THEN
    ExchCond(ExNum)%SensHeatingRate = ProcessSensHeatRecRate
    ExchCond(ExNum)%SensCoolingRate = 0.0d0
  ELSE
    ExchCond(ExNum)%SensHeatingRate = 0.0d0
    ExchCond(ExNum)%SensCoolingRate = ABS(ProcessSensHeatRecRate)
  END IF
  IF (ProcessLatHeatRecRate .GT. 0.0d0) THEN
    ExchCond(ExNum)%LatHeatingRate = ProcessLatHeatRecRate
    ExchCond(ExNum)%LatCoolingRate = 0.0d0
  ELSE
    ExchCond(ExNum)%LatHeatingRate = 0.0d0
    ExchCond(ExNum)%LatCoolingRate = ABS(ProcessLatHeatRecRate)
  END IF
  IF (ProcessTotHeatRecRate .GT. 0.0d0) THEN
    ExchCond(ExNum)%TotHeatingRate = ProcessTotHeatRecRate
    ExchCond(ExNum)%TotCoolingRate = 0.0d0
  ELSE
    ExchCond(ExNum)%TotHeatingRate = 0.0d0
    ExchCond(ExNum)%TotCoolingRate = ABS(ProcessTotHeatRecRate)
  END IF

RETURN
END SUBROUTINE CalcDesiccantBalancedHeatExch

SUBROUTINE FrostControl(ExNum)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   June 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates fraction of timestep necessary to eliminate frost on ERV surface
          ! by comparing secondary outlet or outdoor temperature to a frost control threshold
          ! temperature.  Supply air and secondary air outlet conditions are calculated
          ! based on frost control method selected.

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: ExNum ! number of the current heat exchanger being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER  ::  ErrorTol = 0.001d0         ! error tolerence for iteration loop
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: DFFraction          ! fraction of timestep ERV is in frost control mode
  REAL(r64)    :: RhoSup              ! density of supply air [kg/m3]
  REAL(r64)    :: RhoSec              ! density of secondary air [kg/m3]
  REAL(r64)    :: Error               ! iteration loop error variable
  REAL(r64)    :: Iter                ! iteration counter
  REAL(r64)    :: CSup                ! mdot Cp of supply air [W/K]
  REAL(r64)    :: CSec                ! mdot Cp of secondary air [W/K]
  REAL(r64)    :: CMin                ! minimum mdot Cp of supply or secondary air [W/K]
  REAL(r64)    :: QTotTrans           ! total heat transfer by ERV [W]
  REAL(r64)    :: QSensTrans          ! sensible heat transfer by ERV [W]
  REAL(r64)    :: HXSecAirVolFlowRate ! air volume flow rate of the secondary air stream through the heat exchanger [m3/sec]
  REAL(r64)    :: HXSupAirVolFlowRate ! air volume flow rate of the supply air stream through the heat exchanger [m3/sec]
  REAL(r64)    :: HXAvgAirVolFlowRate ! average air volume flow rate through the heat exchanger [m3/sec]
  REAL(r64)    :: HXAirVolFlowRatio   ! nominal to actual air volume flow ratio
  REAL(r64)    :: MassFlowSupIn       ! supply air mass flow rate at HX inlet
  REAL(r64)    :: MassFlowSupOut      ! supply air mass flow rate through HX core outlet
  REAL(r64)    :: MassFlowSupBypass   ! supply air bypass mass flow rate around HX core
  REAL(r64)    :: TempSupIn           ! supply side temperature of air entering HX
  REAL(r64)    :: TempSupOut          ! supply side temperature of air leaving HX core
  REAL(r64)    :: HumRatSupIn         ! supply side humidity ratio of air entering HX
  REAL(r64)    :: TempSecIn           ! secondary side temperature of air entering HX
  REAL(r64)    :: TempSecOut          ! secondary side temperature of air leaving HX core
  REAL(r64)    :: TempThreshold       ! threshold temperature below which frost control is active


  ExchCond(ExNum)%SupOutMassFlow = ExchCond(ExNum)%SupInMassFlow
  ExchCond(ExNum)%SecOutMassFlow = ExchCond(ExNum)%SecInMassFlow
  ExchCond(ExNum)%SupBypassMassFlow =  0.0d0
  ExchCond(ExNum)%SecBypassMassFlow =  0.0d0
  RhoSup = PsyRhoAirFnPbTdbW(OutBaroPress,ExchCond(ExNum)%SupInTemp,ExchCond(ExNum)%SupInHumRat)
  RhoSec = PsyRhoAirFnPbTdbW(OutBaroPress,ExchCond(ExNum)%SecInTemp,ExchCond(ExNum)%SecInHumRat)
  CSup = ExchCond(ExNum)%SupOutMassFlow * PsyCpAirFnWTdb(ExchCond(ExNum)%SupInHumRat,ExchCond(ExNum)%SupInTemp)
  CSec = ExchCond(ExNum)%SecOutMassFlow * PsyCpAirFnWTdb(ExchCond(ExNum)%SecInHumRat,ExchCond(ExNum)%SecInTemp)
  CMin = MIN(CSup, CSec)
  TempThreshold = ExchCond(ExNum)%ThresholdTemperature

  IF (ExchCond(ExNum)%ControlToTemperatureSetPoint) THEN
  ! Recalculate HX outlet conditions as if control to temperature setpoint was not activated,
  ! because defrost will override those results

    HXSupAirVolFlowRate = ExchCond(ExNum)%SupOutMassFlow/RhoSup
    HXSecAirVolFlowRate = ExchCond(ExNum)%SecOutMassFlow/RhoSec
    HXAvgAirVolFlowRate = (HXSecAirVolFlowRate + HXSupAirVolFlowRate)/2.0d0
    HXAirVolFlowRatio = HXAvgAirVolFlowRate/ExchCond(ExNum)%NomSupAirVolFlow
    ExchCond(ExNum)%SensEffectiveness = ExchCond(ExNum)%HeatEffectSensible75 + &
      (ExchCond(ExNum)%HeatEffectSensible100 - ExchCond(ExNum)%HeatEffectSensible75)* &
      (HXAirVolFlowRatio - 0.75d0)/(1.d0 - 0.75d0)
    ExchCond(ExNum)%LatEffectiveness = ExchCond(ExNum)%HeatEffectLatent75 + &
      (ExchCond(ExNum)%HeatEffectLatent100 - ExchCond(ExNum)%HeatEffectLatent75)* &
      (HXAirVolFlowRatio - 0.75d0)/(1.d0 - 0.75d0)
    ExchCond(ExNum)%SupOutTemp = ExchCond(ExNum)%SupInTemp + &
      ExchCond(ExNum)%SensEffectiveness*CMin/CSup*(ExchCond(ExNum)%SecInTemp-ExchCond(ExNum)%SupInTemp)
    ExchCond(ExNum)%SupOutHumRat = ExchCond(ExNum)%SupInHumRat + &
      ExchCond(ExNum)%LatEffectiveness*CMin/CSup*(ExchCond(ExNum)%SecInHumRat-ExchCond(ExNum)%SupInHumRat)
    ExchCond(ExNum)%SupOutEnth = PsyHFnTdbW(ExchCond(ExNum)%SupOutTemp,ExchCond(ExNum)%SupOutHumRat)

!   Check for saturation in supply outlet and reset temp, then humidity ratio at constant enthalpy
    IF (PsyTsatFnHPb(ExchCond(ExNum)%SupOutEnth,OutBaroPress).GT.ExchCond(ExNum)%SupOutTemp) THEN
      ExchCond(ExNum)%SupOutTemp = PsyTsatFnHPb(ExchCond(ExNum)%SupOutEnth,OutBaroPress)
      ExchCond(ExNum)%SupOutHumRat = PsyWFnTdbH(ExchCond(ExNum)%SupOutTemp,ExchCond(ExNum)%SupOutEnth)
    END IF

    QSensTrans = CSup * (ExchCond(ExNum)%SupInTemp - ExchCond(ExNum)%SupOutTemp)
    ExchCond(ExNum)%SecOutTemp = ExchCond(ExNum)%SecInTemp + QSensTrans / CSec
    QTotTrans = ExchCond(ExNum)%SupOutMassFlow * (ExchCond(ExNum)%SupInEnth - ExchCond(ExNum)%SupOutEnth)
    ExchCond(ExNum)%SecOutEnth = ExchCond(ExNum)%SecInEnth + QTotTrans/ExchCond(ExNum)%SecOutMassFlow
    ExchCond(ExNum)%SecOutHumRat = PsyWFnTdbH(ExchCond(ExNum)%SecOutTemp,ExchCond(ExNum)%SecOutEnth)
  END IF

! Check frost control by type

  IF(ExchCond(ExNum)%FrostControlType == 'MINIMUMEXHAUSTTEMPERATURE') THEN
!   A plate HX will bypass air on the supply side to keep exhaust temp above a
!   threshold temperature and requires recalculating effectiveness based on
!   the reduced air flow rate. A rotary HX modulates rotational speed to try to keep the
!   exhaust air temperature above the threshold temperature. Assume that
!   sensible and latent effectiveness decrease proportionally with rotary HX speed.

    DFFraction = MAX(0.0d0,MIN(1.0d0,SafeDiv((TempThreshold - ExchCond(ExNum)%SecOutTemp), &
                                 (ExchCond(ExNum)%SecInTemp - ExchCond(ExNum)%SecOutTemp))))
    IF (ExchCond(ExNum)%ExchConfigNum == ROTARY) THEN
      ExchCond(ExNum)%SensEffectiveness = (1.d0-DFFraction) * ExchCond(ExNum)%SensEffectiveness
      ExchCond(ExNum)%LatEffectiveness = (1.d0-DFFraction) * ExchCond(ExNum)%LatEffectiveness
    ELSE ! HX is a plate heat exchanger, bypass air to eliminate frost
      Error = 1.0d0
      Iter = 0.0d0
      MassFlowSupIn = ExchCond(ExNum)%SupInMassFlow
      MassFlowSupOut = ExchCond(ExNum)%SupOutMassFlow
      MassFlowSupBypass = ExchCond(ExNum)%SupBypassMassFlow
      TempSupIn = ExchCond(ExNum)%SupInTemp
      HumRatSupIn = ExchCond(ExNum)%SupInHumRat
      TempSecIn = ExchCond(ExNum)%SecInTemp

      DO WHILE (ABS(Error) .GT. ErrorTol .AND. Iter .lt. 10)
        MassFlowSupOut = MassFlowSupIn*(1.d0-DFFraction)
        MassFlowSupBypass = MassFlowSupIn*DFFraction
        HXSupAirVolFlowRate = MassFlowSupOut/RhoSup
        HXSecAirVolFlowRate = ExchCond(ExNum)%SecOutMassFlow/RhoSec
        HXAvgAirVolFlowRate = (HXSecAirVolFlowRate + HXSupAirVolFlowRate)/2.0d0
        HXAirVolFlowRatio = HXAvgAirVolFlowRate/ExchCond(ExNum)%NomSupAirVolFlow
        CSup = MassFlowSupOut * PsyCpAirFnWTdb(HumRatSupIn,TempSupIn)
        CMin = MIN(CSup,CSec)
        IF (TempSupIn .LT. TempSecIn) THEN
!         Use heating effectiveness values
          ExchCond(ExNum)%SensEffectiveness = ExchCond(ExNum)%HeatEffectSensible75 + &
              (ExchCond(ExNum)%HeatEffectSensible100 - ExchCond(ExNum)%HeatEffectSensible75)* &
              (HXAirVolFlowRatio - 0.75d0)/(1.d0 - 0.75d0)
          ExchCond(ExNum)%LatEffectiveness = ExchCond(ExNum)%HeatEffectLatent75 + &
              (ExchCond(ExNum)%HeatEffectLatent100 - ExchCond(ExNum)%HeatEffectLatent75)* &
              (HXAirVolFlowRatio - 0.75d0)/(1.d0 - 0.75d0)
        ELSE
!         Use cooling effectiveness values
          ExchCond(ExNum)%SensEffectiveness = ExchCond(ExNum)%CoolEffectSensible75 + &
              (ExchCond(ExNum)%CoolEffectSensible100 - ExchCond(ExNum)%CoolEffectSensible75)* &
              (HXAirVolFlowRatio - 0.75d0)/(1.d0 - 0.75d0)
          ExchCond(ExNum)%LatEffectiveness = ExchCond(ExNum)%CoolEffectLatent75 + &
              (ExchCond(ExNum)%CoolEffectLatent100 - ExchCond(ExNum)%CoolEffectLatent75)* &
              (HXAirVolFlowRatio - 0.75d0)/(1.d0 - 0.75d0)
        END IF
!         calculation of local variable Csup can be 0, gaurd against divide by 0.
          TempSupOut = TempSupIn + &
              ExchCond(ExNum)%SensEffectiveness * SafeDiv(CMin,CSup) * (TempSecIn - TempSupIn)
          QSensTrans = CSup * (TempSupIn - TempSupOut)
!         Csec cannot be 0 in this subroutine
          TempSecOut = TempSecIn + QSensTrans / CSec
          Error =  (TempSecOut - TempThreshold)
!         recalculate DFFraction until convergence, gaurd against divide by 0 (unlikely).
          DFFraction = MAX(0.0d0,MIN(1.0d0,DFFraction * SafeDiv((TempSecIn - TempSecOut),(TempSecIn - TempThreshold))))
          Iter = Iter + 1
      END DO
      ExchCond(ExNum)%SupInMassFlow = MassFlowSupIn
      ExchCond(ExNum)%SupOutMassFlow = MassFlowSupOut
      ExchCond(ExNum)%SupBypassMassFlow = MassFlowSupBypass
    END IF
    ExchCond(ExNum)%SupOutTemp = ExchCond(ExNum)%SupInTemp + &
        ExchCond(ExNum)%SensEffectiveness*SafeDiv(CMin,CSup)*(ExchCond(ExNum)%SecInTemp-ExchCond(ExNum)%SupInTemp)
    ExchCond(ExNum)%SupOutHumRat = ExchCond(ExNum)%SupInHumRat + &
        ExchCond(ExNum)%LatEffectiveness*SafeDiv(CMin,CSup)*(ExchCond(ExNum)%SecInHumRat-ExchCond(ExNum)%SupInHumRat)
    ExchCond(ExNum)%SupOutEnth = PsyHFnTdbW(ExchCond(ExNum)%SupOutTemp,ExchCond(ExNum)%SupOutHumRat)

!   Check for saturation in supply outlet and reset temp, then humidity ratio at constant enthalpy
    IF (PsyTsatFnHPb(ExchCond(ExNum)%SupOutEnth,OutBaroPress).GT.ExchCond(ExNum)%SupOutTemp) THEN
        ExchCond(ExNum)%SupOutTemp = PsyTsatFnHPb(ExchCond(ExNum)%SupOutEnth,OutBaroPress)
        ExchCond(ExNum)%SupOutHumRat = PsyWFnTdbH(ExchCond(ExNum)%SupOutTemp,ExchCond(ExNum)%SupOutEnth)
    END IF

    QSensTrans = CSup * (ExchCond(ExNum)%SupInTemp - ExchCond(ExNum)%SupOutTemp)
    ExchCond(ExNum)%SecOutTemp = ExchCond(ExNum)%SecInTemp + QSensTrans / CSec
    QTotTrans = ExchCond(ExNum)%SupOutMassFlow * (ExchCond(ExNum)%SupInEnth - ExchCond(ExNum)%SupOutEnth)
    ExchCond(ExNum)%SecOutEnth = ExchCond(ExNum)%SecInEnth + QTotTrans/ExchCond(ExNum)%SecOutMassFlow
    ExchCond(ExNum)%SecOutHumRat = PsyWFnTdbH(ExchCond(ExNum)%SecOutTemp,ExchCond(ExNum)%SecOutEnth)

!   Perform mixing of core air stream and bypass air stream and set mass flow rates at outlet nodes
    ExchCond(ExNum)%SupOutEnth = (ExchCond(ExNum)%SupOutMassFlow*ExchCond(ExNum)%SupOutEnth &
                                 + ExchCond(ExNum)%SupBypassMassFlow*ExchCond(ExNum)%SupInEnth) &
                                   / ExchCond(ExNum)%SupInMassFlow
    ExchCond(ExNum)%SupOutHumRat = (ExchCond(ExNum)%SupOutMassFlow*ExchCond(ExNum)%SupOutHumRat &
                                 + ExchCond(ExNum)%SupBypassMassFlow*ExchCond(ExNum)%SupInHumRat) &
                                   / ExchCond(ExNum)%SupInMassFlow
    ExchCond(ExNum)%SupOutTemp = PsyTdbFnHW(ExchCond(ExNum)%SupOutEnth,ExchCond(ExNum)%SupOutHumRat)
    ExchCond(ExNum)%SupOutMassFlow = ExchCond(ExNum)%SupInMassFlow
    ExchCond(ExNum)%SecOutEnth = (ExchCond(ExNum)%SecOutMassFlow*ExchCond(ExNum)%SecOutEnth &
                                 + ExchCond(ExNum)%SecBypassMassFlow*ExchCond(ExNum)%SecInEnth) &
                                   / ExchCond(ExNum)%SecInMassFlow
    ExchCond(ExNum)%SecOutHumRat = (ExchCond(ExNum)%SecOutMassFlow*ExchCond(ExNum)%SecOutHumRat &
                                 + ExchCond(ExNum)%SecBypassMassFlow*ExchCond(ExNum)%SecInHumRat) &
                                   / ExchCond(ExNum)%SecInMassFlow
    ExchCond(ExNum)%SecOutTemp = PsyTdbFnHW(ExchCond(ExNum)%SecOutEnth,ExchCond(ExNum)%SecOutHumRat)
    ExchCond(ExNum)%SecOutMassFlow = ExchCond(ExNum)%SecInMassFlow

  END IF  ! End of IF (Minimum Exhaust Temperature)


  IF(ExchCond(ExNum)%FrostControlType == 'EXHAUSTAIRRECIRCULATION') Then
!
! Directing exhaust outlet air back across the HX core on the supply side
! Assume no heat exchange when in frost control mode, full heat exchange otherwise
!
     DFFraction = MAX(0.0d0,MIN((ExchCond(ExNum)%InitialDefrostTime + &
                  ExchCond(ExNum)%RateofDefrostTimeIncrease * &
                  (TempThreshold-ExchCond(ExNum)%SupInTemp)),1.0d0))

!    Calculate derated heat transfer using outlet air conditions assuming no defrost (calculated earlier)
!    and (1-DefrostFraction)
     QSensTrans = (1.d0-DFFraction) * CSup * (ExchCond(ExNum)%SupInTemp - ExchCond(ExNum)%SupOutTemp)
     QTotTrans = (1.d0-DFFraction) * ExchCond(ExNum)%SupOutMassFlow * (ExchCond(ExNum)%SupInEnth - ExchCond(ExNum)%SupOutEnth)

     ExchCond(ExNum)%SupOutMassFlow = (1.d0-DFFraction) * ExchCond(ExNum)%SupInMassFlow + &
                                       DFFraction    * ExchCond(ExNum)%SecInMassFlow

!    Blend supply outlet condition of HX core with exhaust air inlet to get final supply air outlet conditions
     ExchCond(ExNum)%SupOutTemp = ((1.d0-DFFraction) * ExchCond(ExNum)%SupInMassFlow * ExchCond(ExNum)%SupOutTemp + &
                                  DFFraction * ExchCond(ExNum)%SecInMassFlow * ExchCond(ExNum)%SecInTemp) / &
                                  ExchCond(ExNum)%SupOutMassFlow

     ExchCond(ExNum)%SupOutHumRat = ((1.d0-DFFraction) * ExchCond(ExNum)%SupInMassFlow * ExchCond(ExNum)%SupOutHumRat + &
                                  DFFraction * ExchCond(ExNum)%SecInMassFlow * ExchCond(ExNum)%SecInHumRat) / &
                                  ExchCond(ExNum)%SupOutMassFlow

     ExchCond(ExNum)%SupOutEnth = PsyHFnTdbW(ExchCond(ExNum)%SupOutTemp,ExchCond(ExNum)%SupOutHumRat)
!    No need to check for saturation after SA out and EA inlet are blended


!    Derate effectiveness based on frost control time fraction for reporting purposes
     ExchCond(ExNum)%SensEffectiveness = (1.d0-DFFraction) * ExchCond(ExNum)%SensEffectiveness
     ExchCond(ExNum)%LatEffectiveness = (1.d0-DFFraction) * ExchCond(ExNum)%LatEffectiveness

!    Secondary air outlet conditions are previously calculated as the conditions when not
!    in defrost, and this is what we want to report so no changes here.
!
!    Average SupInMassFlow and SecOutMassFlow rates have been reduced due to frost control
!      Equipment attached to the supply inlet node may have problems with our setting the
!      mass flow rate in the next statement. This is done only to simulate exhaust air recirc.
     Node(ExchCond(ExNum)%SupInletNode)%MassFlowRate = ExchCond(ExNum)%SupInMassFlow * (1.d0-DFFraction)
     ExchCond(ExNum)%SecOutMassFlow = ExchCond(ExNum)%SecOutMassFlow * (1.d0-DFFraction)

  END IF  ! End of IF (Exhaust Air Recirculation)

  IF(ExchCond(ExNum)%FrostControlType == 'EXHAUSTONLY') Then

!   Perform frost control by bypassing the supply air around the HX core during the defrost
!   time period. HX heat transfer is reduced proportionally to (1 - defrosttimefraction)

    DFFraction = MAX(0.0d0,MIN((ExchCond(ExNum)%InitialDefrostTime + &
                 ExchCond(ExNum)%RateofDefrostTimeIncrease * &
                 (TempThreshold-ExchCond(ExNum)%SupInTemp)),1.0d0))

!   Calculate derated heat transfer based on defrost time
    QSensTrans = (1.d0-DFFraction) * CSup * (ExchCond(ExNum)%SupInTemp - ExchCond(ExNum)%SupOutTemp)
    QTotTrans = (1.d0-DFFraction) * ExchCond(ExNum)%SupOutMassFlow * (ExchCond(ExNum)%SupInEnth - ExchCond(ExNum)%SupOutEnth)

!   Calculate the air conditions leaving heat exchanger unit
!   Heat exchanger effectiveness is not derated, HX is fully bypassed during frost control

    ExchCond(ExNum)%SupBypassMassFlow = ExchCond(ExNum)%SupInMassFlow * DFFraction
    ExchCond(ExNum)%SupOutTemp = ExchCond(ExNum)%SupInTemp - QSensTrans / CSup
    ExchCond(ExNum)%SupOutEnth = ExchCond(ExNum)%SupInEnth - QTotTrans/ExchCond(ExNum)%SupOutMassFlow
    ExchCond(ExNum)%SupOutHumRat = PsyWFnTdbH(ExchCond(ExNum)%SupOutTemp,ExchCond(ExNum)%SupOutEnth)

    IF (PsyTsatFnHPb(ExchCond(ExNum)%SupOutEnth,OutBaroPress).GT.ExchCond(ExNum)%SupOutTemp) THEN
      ExchCond(ExNum)%SupOutTemp = PsyTsatFnHPb(ExchCond(ExNum)%SupOutEnth,OutBaroPress)
      ExchCond(ExNum)%SupOutHumRat = PsyWFnTdbH(ExchCond(ExNum)%SupOutTemp,ExchCond(ExNum)%SupOutEnth)
      QSensTrans = CSup * (ExchCond(ExNum)%SupInTemp - ExchCond(ExNum)%SupOutTemp)
      ! Should we be updating the sensible and latent effectiveness values also?
    END IF

    ExchCond(ExNum)%SecOutEnth = ExchCond(ExNum)%SecInEnth + QTotTrans/ExchCond(ExNum)%SecOutMassFlow
    ExchCond(ExNum)%SecOutTemp = ExchCond(ExNum)%SecInTemp + QSensTrans / CSec
    ExchCond(ExNum)%SecOutHumRat = PsyWFnTdbH(ExchCond(ExNum)%SecOutTemp,ExchCond(ExNum)%SecOutEnth)
  END IF  ! End of IF (Exhaust Only)

  ExchCond(ExNum)%DefrostFraction = DFFraction

RETURN
END SUBROUTINE FrostControl

SUBROUTINE UpdateHeatRecovery(ExNum)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   March 1999
          !       MODIFIED       Fred Buhl November 2000
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Moves heat exchanger output to the outlet nodes.

          ! METHODOLOGY EMPLOYED:
          ! NA

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataContaminantBalance, ONLY: Contaminant

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: ExNum ! number of the current heat exchanger being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: SupInNode  ! supply inlet node number
  INTEGER             :: SupOutNode ! supply outlet node number
  INTEGER             :: SecInNode  ! secondary inlet node number
  INTEGER             :: SecOutNode ! secondary outlet node number

  SupInNode = ExchCond(ExNum)%SupInletNode
  SupOutNode = ExchCond(ExNum)%SupOutletNode
  SecInNode = ExchCond(ExNum)%SecInletNode
  SecOutNode = ExchCond(ExNum)%SecOutletNode

  ! Set the outlet air nodes of the heat exchanger
  Node(SupOutNode)%Temp = ExchCond(ExNum)%SupOutTemp
  Node(SupOutNode)%HumRat = ExchCond(ExNum)%SupOutHumRat
  Node(SupOutNode)%Enthalpy = ExchCond(ExNum)%SupOutEnth
  Node(SupOutNode)%MassFlowRate = ExchCond(ExNum)%SupOutMassFlow
  Node(SecOutNode)%Temp = ExchCond(ExNum)%SecOutTemp
  Node(SecOutNode)%HumRat = ExchCond(ExNum)%SecOutHumRat
  Node(SecOutNode)%Enthalpy = ExchCond(ExNum)%SecOutEnth
  Node(SecOutNode)%MassFlowRate = ExchCond(ExNum)%SecOutMassFlow

  ! Set the outlet nodes for properties that just pass through & not used
  Node(SupOutNode)%Quality             = Node(SupInNode)%Quality
  Node(SupOutNode)%Press               = Node(SupInNode)%Press
  Node(SupOutNode)%MassFlowRateMin     = Node(SupInNode)%MassFlowRateMin
  Node(SupOutNode)%MassFlowRateMax     = Node(SupInNode)%MassFlowRateMax
  Node(SupOutNode)%MassFlowRateMinAvail= Node(SupInNode)%MassFlowRateMinAvail
  Node(SupOutNode)%MassFlowRateMaxAvail= Node(SupInNode)%MassFlowRateMaxAvail
  Node(SecOutNode)%Quality             = Node(SecInNode)%Quality
  Node(SecOutNode)%Press               = Node(SecInNode)%Press
  Node(SecOutNode)%MassFlowRateMin     = Node(SecInNode)%MassFlowRateMin
  Node(SecOutNode)%MassFlowRateMax     = Node(SecInNode)%MassFlowRateMax
  Node(SecOutNode)%MassFlowRateMinAvail= Node(SecInNode)%MassFlowRateMinAvail
  Node(SecOutNode)%MassFlowRateMaxAvail= Node(SecInNode)%MassFlowRateMaxAvail

  IF (Contaminant%CO2Simulation) Then
    Node(SupOutNode)%CO2 = Node(SupInNode)%CO2
    Node(SecOutNode)%CO2 = Node(SecInNode)%CO2
  End If
  IF (Contaminant%GenericContamSimulation) Then
    Node(SupOutNode)%GenContam = Node(SupInNode)%GenContam
    Node(SecOutNode)%GenContam = Node(SecInNode)%GenContam
  End If

RETURN
END SUBROUTINE UpdateHeatRecovery

SUBROUTINE ReportHeatRecovery(ExNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   March 1999
          !       MODIFIED       F Buhl Nov 2000, D Shirey Feb/June 2003
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Fill remaining report variables

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use DataHVACGlobals, ONLY: TimeStepSys, AirToAirHXElecPower

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN) :: ExNum ! number of the current heat exchanger being simulated

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: ReportingConstant

  ReportingConstant = TimeStepSys*SecInHour
  ExchCond(ExNum)%ElecUseEnergy = ExchCond(ExNum)%ElecUseRate*ReportingConstant
  ExchCond(ExNum)%SensHeatingEnergy = ExchCond(ExNum)%SensHeatingRate*ReportingConstant
  ExchCond(ExNum)%LatHeatingEnergy = ExchCond(ExNum)%LatHeatingRate*ReportingConstant
  ExchCond(ExNum)%TotHeatingEnergy = ExchCond(ExNum)%TotHeatingRate*ReportingConstant
  ExchCond(ExNum)%SensCoolingEnergy = ExchCond(ExNum)%SensCoolingRate*ReportingConstant
  ExchCond(ExNum)%LatCoolingEnergy = ExchCond(ExNum)%LatCoolingRate*ReportingConstant
  ExchCond(ExNum)%TotCoolingEnergy = ExchCond(ExNum)%TotCoolingRate*ReportingConstant

  AirToAirHXElecPower = ExchCond(ExNum)%ElecUseRate

RETURN

END SUBROUTINE ReportHeatRecovery

FUNCTION SafeDiv(a, b) RESULT (c)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   March 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Returns a / b while preventing division by zero

          ! METHODOLOGY EMPLOYED:
          ! Check for small or zero values before performing division

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: a, b

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: c

  IF (ABS(b) < SMALL) THEN
    c = a / SIGN(SMALL, b)
  ELSE
    c = a / b
  END IF

  RETURN

END FUNCTION SafeDiv

SUBROUTINE CalculateEpsFromNTUandZ(NTU, Z, FlowArr, Eps)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   March 1999
          !       MODIFIED       Fred Buhl November 2000
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates eps, the exchanger effectiveness,
          ! from NTU, the number of transfer units,
          ! from Z, the capacity rate ratio, and
          ! from the flow arrangement

          ! METHODOLOGY EMPLOYED:
          ! Uses the effectiveness - NTU heat exchanger formulas

          ! REFERENCES:
          ! M. Wetter, Simulation Model Air-to-Air Plate Heat Exchanger
          ! LBNL Report 42354, 1999.
          ! Also see:
          ! ASHRAE HVAC 2 Toolkit, pages 4-3 through 4-5

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)     :: NTU     ! number of transfer units
  REAL(r64), INTENT(IN)     :: Z       ! capacity rate ratio
  INTEGER, INTENT(IN)  :: FlowArr ! flow arrangement
                                  ! 1: COUNTER FLOW
                                  ! 2: PARALLEL FLOW
                                  ! 3: CROSS FLOW BOTH UNMIXED
                                  ! 4: CROSS FLOW, Cmax MIXED, Cmin UNMIXED
                                  !    (coil with one row)
  REAL(r64), INTENT(OUT)    :: Eps     ! heat exchanger effectiveness

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: Temp   ! temporary variable

  ! check input validity
  IF (Z .LT. 0.0d0 .OR. Z.GT.1.0d0) THEN
    CALL ShowFatalError('Variable Z ('//TRIM(RoundSigDigits(Z,2))//') out of range [0.0,1.0] in CalculateEpsFromNTUandZ')
  END IF

  ! effectiveness
  IF (NTU < SMALL) THEN
    Eps = 0.0d0
  ELSE IF (Z < SMALL) THEN ! Eps independent of flow arrangement
    Eps = 1.d0 - exp(-NTU)
  ELSE
    SELECT CASE (FlowArr)
      CASE (Counter_Flow)   ! COUNTER FLOW
        IF (ABS(Z - 1.0d0) < SMALL) THEN
          Eps = NTU/(NTU+1.0d0)
        ELSE
          Temp = EXP(-NTU*(1.d0-Z))
          Eps = (1.d0-Temp) / (1.d0-Z*Temp)
        END IF
      CASE (Parallel_Flow)   ! PARALLEL FLOW
        Temp = (1.d0+Z)
        Eps = (1.d0 - EXP(-NTU*Temp)) / Temp
      CASE (Cross_Flow_Both_Unmixed)   ! CROSS FLOW BOTH UNMIXED
        Temp = Z * NTU**(-0.22d0)
        Eps = 1.d0 - EXP( ( EXP(-NTU*Temp) -1.0d0)/Temp )
      CASE (Cross_Flow_Other)   ! CROSS FLOW, Cmax MIXED, Cmin UNMIXED
        Eps = (1.d0 - EXP(-Z * (1.d0-EXP(-NTU)) ) ) / Z
      CASE DEFAULT
        CALL ShowFatalError('HeatRecovery: Illegal flow arrangement in CalculateEpsFromNTUandZ, Value='//  &
                                        TRIM(RoundSigDigits(FlowArr)))
    END SELECT
  END IF

RETURN

END SUBROUTINE CalculateEpsFromNTUandZ

SUBROUTINE CalculateNTUfromEpsAndZ(NTU, Err, Z, FlowArr, Eps)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   March 1999
          !       MODIFIED       Fred Buhl November 2000
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates NTU, the number of transfer units,
          ! based on eps, the exchanger effectiveness,
          ! Z, the capacity rate ratio, and
          ! from the flow arrangement

          ! METHODOLOGY EMPLOYED:
          ! Uses the effectiveness - NTU heat exchanger formulas

          ! REFERENCES:
          ! M. Wetter, Simulation Model Air-to-Air Plate Heat Exchanger
          ! LBNL Report 42354, 1999.
          ! Also see:
          ! ASHRAE HVAC 2 Toolkit, pages 4-3 through 4-5

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)     :: Eps     ! heat exchanger effectiveness
  REAL(r64), INTENT(IN)     :: Z       ! capacity rate ratio
  INTEGER, INTENT(IN)  :: FlowArr ! flow arrangement
                                  ! 1: COUNTER FLOW
                                  ! 2: PARALLEL FLOW
                                  ! 3: CROSS FLOW BOTH UNMIXED
                                  ! 4: CROSS FLOW, Cmax MIXED, Cmin UNMIXED
                                  !    (coil with one row)
  REAL(r64), INTENT(OUT)     :: NTU    ! number of transfer units
  INTEGER, INTENT(INOUT)  :: Err    ! error indicator

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  NTU = 0.0d0
  ! check input validity
  IF (Z .LT. 0.0d0 .OR. Z.GT.1.0d0) THEN
    Err = 1
    RETURN
  END IF

  IF (FlowArr == Parallel_Flow) THEN
    IF ( Eps<0.0d0 .OR. Eps>1.d0/(1.d0+Z) ) THEN
      Err = 2
      RETURN
    END IF
  ELSE IF (FlowArr == Cross_Flow_Other) THEN
    IF ( Eps<0.0d0 .OR. Eps>(1.d0-EXP(-Z))/Z ) THEN
      Err = 3
      RETURN
    END IF
    ! check product (Eps*Z)
    IF (Eps*Z < 0.0d0 .OR. Eps*Z > 1.d0-EXP(Z*(SMALL-1.d0)) ) THEN
      Err = 4
      RETURN
    END IF
          ! check product (Eps*Z)
  ELSE
    IF ( Eps<0.0d0 .OR. Eps>1.0d0 ) THEN
      Err = 5
      RETURN
    END IF
  END IF

  IF (Eps < SMALL) THEN ! no effectiveness. Set NTU = 0
  NTU = 0.0d0
  ELSE IF (Z < SMALL) THEN ! Eps independent of flow arrangement
    NTU = - LOG(1.d0-Eps)
  ELSE
    ! calculate based on configuration
    SELECT CASE (FlowArr)
      CASE (Counter_Flow)   ! COUNTER FLOW
        IF (ABS(Z - 1.0d0) < SMALL) THEN
          NTU = Eps / (1.d0 - Eps)
        ELSE
          NTU = 1.d0 / (Z-1.d0) * LOG( (1.d0-Eps)/(1.d0-Eps*Z) )
        END IF
      CASE (Parallel_Flow)   ! PARALLEL FLOW
        NTU = - LOG( -Eps - Eps * Z + 1.d0) / (Z+1.d0)
      CASE (Cross_Flow_Both_Unmixed)   ! CROSS FLOW BOTH UNMIXED
        NTU = GetNTUforCrossFlowBothUnmixed(Eps, Z)
      CASE (Cross_Flow_Other)   ! CROSS FLOW, Cmax MIXED, Cmin UNMIXED
        NTU = -LOG(1.d0 + LOG(1.d0-eps*Z)/Z )
      CASE DEFAULT
        CALL ShowFatalError('HeatRecovery: Illegal flow arrangement in CalculateNTUfromEpsAndZ, Value='//  &
                                        TRIM(RoundSigDigits(FlowArr)))
    END SELECT
  END IF

RETURN

END SUBROUTINE CalculateNTUfromEpsAndZ

FUNCTION GetNTUforCrossFlowBothUnmixed(Eps, Z) RESULT (NTU)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   March 1999
          !       MODIFIED       Fred Buhl November 2000
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! Calculates the NTU value based on the exchanger effectiveness
          ! and the capacity ratio for cross flow exchanger, both
          ! streams unmixed

          ! METHODOLOGY EMPLOYED:
          ! Uses a Regula Falsi solver function to numerically invert the formula
          ! giving effectiveness as a function of NTU and Z..

          ! REFERENCES:
          ! M. Wetter, Simulation Model Air-to-Air Plate Heat Exchanger
          ! LBNL Report 42354, 1999.
          ! Also see:
          ! ASHRAE HVAC 2 Toolkit, pages 4-3 through 4-5

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)     :: Eps     ! heat exchanger effectiveness
  REAL(r64), INTENT(IN)     :: Z       ! capacity rate ratio

  REAL(r64)            :: NTU     ! result variable; number of transfer units

          ! FUNCTION PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER  :: Acc =  0.0001d0       ! Accuracy of result
  INTEGER, PARAMETER    :: MaxIte = 500        ! Maximum number of iterations

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

  INTEGER    :: SolFla              ! Flag of solver
  REAL(r64)  :: NTU0 = 0.d0           ! lower bound for NTU
  REAL(r64)  :: NTU1 = 50.d0          ! upper bound for NTU
  REAL(r64), DIMENSION(2) :: Par

  Par(1) = Eps
  Par(2) = Z

  CALL SolveRegulaFalsi(Acc, MaxIte, SolFla, NTU, &
    GetResidCrossFlowBothUnmixed, NTU0, NTU1, Par)

  IF (SolFla == -2) THEN
    CALL ShowFatalError('HeatRecovery: Bad initial bounds for NTU in GetNTUforCrossFlowBothUnmixed')
  ELSE IF (SolFla == -1) THEN
    CALL ShowFatalError('HeatRecovery: No convergence in solving for NTU in GetNTUforCrossFlowBothUnmixed')
  END IF

RETURN
END FUNCTION GetNTUforCrossFlowBothUnmixed

FUNCTION GetResidCrossFlowBothUnmixed(NTU, Par) RESULT (Residuum)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Michael Wetter
          !       DATE WRITTEN   March 1999
          !       MODIFIED       Fred Buhl November 2000
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! From the formula Eps = f(NTU,Z) this function finds the
          ! residual of f(NTU,Z) - Eps for a cross flow heat exchanger,
          ! both streams unmixed.

          ! METHODOLOGY EMPLOYED:
          ! Uses the effectiveness - NTU heat exchanger formula for cross
          ! flow, both streams unmixed.

          ! REFERENCES:
          ! M. Wetter, Simulation Model Air-to-Air Plate Heat Exchanger
          ! LBNL Report 42354, 1999.
          ! Also see:
          ! ASHRAE HVAC 2 Toolkit, pages 4-3 through 4-5

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
    REAL(r64), INTENT(IN)  :: NTU ! number of transfer units
    REAL(r64), INTENT(IN), DIMENSION(:), OPTIONAL :: Par ! par(1) = Eps, par(2) = Z
    REAL(r64)         :: Residuum ! residual to be minimized to zero

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:

    Residuum = 1.0d0- EXP( ( EXP(-NTU**0.78d0 * Par(2)) - 1.0d0) /  &
      Par(2)*NTU**0.22d0) - Par(1)

RETURN

END FUNCTION GetResidCrossFlowBothUnmixed

SUBROUTINE CheckModelBoundsTempEq(ExchNum, T_RegenInTemp, T_RegenInHumRat, T_ProcInTemp, T_ProcInHumRat, T_FaceVel, &
                                  FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Mangesh Basarkar, FSEC
          !       DATE WRITTEN   January 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !
          ! To verify that the empirical model's independent variables are within the limits used during the
          ! developement of the empirical model.

          ! METHODOLOGY EMPLOYED:
          ! The empirical models used for simulating a desiccant enhanced cooling coil are based on a limited data set.
          ! Extrapolation of empirical models can cause instability and the independent variables may need to be limited.
          ! The range of each independent variable is provided by the user and are based on the limits of the
          ! empirical model. These limits are tested in this subroutine each time step and returned for use by the calling
          ! routine.
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,         ONLY: CreateSysTimeIntervalString, RoundSigDigits
  USE DataGlobals,     ONLY: CurrentTime
  USE DataHVACGlobals, ONLY: SysTimeElapsed, TimeStepSys

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

  ! SUBROUTINE ARGUMENT DEFINITIONS
   ! regen outlet temp equation
  INTEGER, INTENT(IN)    :: ExchNum ! number of the current heat exchanger being simulated
  REAL(r64),    INTENT(INOUT) :: T_RegenInTemp             ! current regen inlet temperature (C) for regen outlet temp eqn
  REAL(r64),    INTENT(INOUT) :: T_RegenInHumRat           ! current regen inlet hum rat for regen outlet temp eqn
  REAL(r64),    INTENT(INOUT) :: T_ProcInTemp              ! current process inlet temperature (C) for regen outlet temp eqn
  REAL(r64),    INTENT(INOUT) :: T_ProcInHumRat            ! current process inlet hum rat for regen outlet temp eqn
  REAL(r64),    INTENT(INOUT) :: T_FaceVel                 ! current process and regen face velocity (m/s)
  LOGICAL, INTENT(IN)    :: FirstHVACIteration        ! First HVAC iteration flag

  ! SUBROUTINE PARAMETER DEFINITIONS:
!  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F10.6)'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
 CHARACTER(len=32)        :: OutputChar         = ' '     ! character string for warning messages
 CHARACTER(len=32)        :: OutputCharLo       = ' '     ! character string for warning messages
 CHARACTER(len=32)        :: OutputCharHi       = ' '     ! character string for warning messages
 CHARACTER(len=32)        :: CharValue          = ' '     ! character string for warning messages
 REAL(r64),SAVE    :: TimeStepSysLast    = 0.0d0     ! last system time step (used to check for downshifting)
 REAL(r64)    :: CurrentEndTime     = 0.0d0     ! end time of time step for current simulation time step
 REAL(r64),SAVE    :: CurrentEndTimeLast = 0.0d0     ! end time of time step for last simulation time step
                                                          ! current end time is compared with last to see if time step changed


!   calculate end time of current time step
    CurrentEndTime = CurrentTime + SysTimeElapsed

!   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
!   Wait for next time step to print warnings. If simulation iterates, print out
!   the warning for the last iteration only. Must wait for next time step to accomplish this.
!   If a warning occurs and the simulation down shifts, the warning is not valid.
    IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

      ! print error for variables of regeneration outlet temperature equation
      ! Regen inlet temp for temp eqn
      IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_RegenInTempMessage)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInTempErrorCount = &
                        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInTempErrorCount + 1
        IF (BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInTempErrorCount < 2) THEN
           CALL ShowWarningError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInTempBuffer1))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInTempBuffer2))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInTempBuffer3))
           CALL ShowContinueError('...Using regeneration inlet air temperatures that are outside the regeneration outlet ' &
                             //'air temperature equation model boundaries may adversely affect desiccant model performance.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
                                              TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// &
             '" - Regeneration inlet air temp used in regen outlet air temperature equation is out of range error continues...', &
                  BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInTempErrIndex, &
                        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInTempLast, &
                                 BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInTempLast)
        END IF
      END IF
      ! Regen inlet humidity ratio for temp eqn
      IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_RegenInHumRatMessage)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInHumRatErrorCount = &
                        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInHumRatErrorCount + 1
        IF (BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInHumRatErrorCount < 2) THEN
           CALL ShowWarningError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInHumRatBuffer1))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInHumRatBuffer2))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInHumRatBuffer3))
           CALL ShowContinueError('...Using regeneration inlet air humidity ratios that are outside the regeneration outlet ' &
                             //'air temperature equation model boundaries may adversely affect desiccant model performance.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
                            TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)//'" - Regeneration inlet air '// &
           'humidity ratio used in regen outlet temperature equation is out of range error continues...', &
            BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInHumRatErrIndex, &
                               BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInHumRatLast, &
                                BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInHumRatLast)
        END IF
      END IF
      ! Process inlet temp for temp eqn
      IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_ProcInTempMessage)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInTempErrorCount = &
                        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInTempErrorCount + 1
        IF (BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInTempErrorCount < 2) THEN
           CALL ShowWarningError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInTempBuffer1))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInTempBuffer2))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInTempBuffer3))
           CALL ShowContinueError('...Using process inlet air temperatures that are outside the regeneration outlet ' &
                           //'air temperature equation model boundaries may adversely affect desiccant model performance.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
                                              TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// &
              '" - Process inlet air temperature used in regen outlet temperature equation is out of range error continues...', &
               BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInTempErrIndex, &
                                       BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInTempLast, &
                                       BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInTempLast)
        END IF
      END IF
      ! Process inlet humidity ratio for temp eqn
      IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_ProcInHumRatMessage)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInHumRatErrorCount = &
                        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInHumRatErrorCount + 1
        IF (BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInHumRatErrorCount < 2) THEN
           CALL ShowWarningError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInHumRatBuffer1))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInHumRatBuffer2))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInHumRatBuffer3))
           CALL ShowContinueError('...Using process inlet air humidity ratios that are outside the regeneratoin outlet ' &
                           //'air temperature equation model boundaries may adversely affect desiccant model performance.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
                                              TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// &
             '" - Process inlet air humidity ratio used in regen outlet temperature equation is out of range error continues...', &
              BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInHumRatErrIndex, &
                               BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInHumRatLast, &
                                BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInHumRatLast)
        END IF
      END IF
      ! Process and regeneration face velocity for temp eqn
      IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_FaceVelMessage)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_FaceVelErrorCount = &
                        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_FaceVelErrorCount + 1
        IF (BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_FaceVelErrorCount < 2) THEN
           CALL ShowWarningError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_FaceVelBuffer1))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_FaceVelBuffer2))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_FaceVelBuffer3))
           CALL ShowContinueError('...Using process and regeneration face velocities that are outside the regeneration outlet ' &
                       //'air temperature equation model boundaries may adversely affect desiccant model performance.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
                                              TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)//'" - Process '// &
               'and regen inlet air face velocity used in regen outlet temperature equation is out of range error continues...', &
               BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_FaceVelocityErrIndex, &
                                 BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_FaceVelLast, &
                                  BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_FaceVelLast)
        END IF
      END IF
    END IF ! IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

!   save last system time step and last end time of current time step (used to determine if warning is valid)
    TimeStepSysLast    = TimeStepSys
    CurrentEndTimeLast = CurrentEndTime

!   If regen and procees inlet temperatures are the same the coil is off, do not print out of bounds warning for this case
    IF(ABS(T_RegenInTemp-T_ProcInTemp) .LT. Small)THEN
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_RegenInTempMessage = .FALSE.
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_RegenInHumRatMessage = .FALSE.
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_ProcInTempMessage = .FALSE.
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_ProcInHumRatMessage = .FALSE.
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_FaceVelMessage = .FALSE.
      RETURN
    END IF

!   check boundaries of independent variables and post warnings to individual buffers to print at end of time step
    ! checking model bounds for variables of regeneration outlet temperature equation
    ! Regen inlet temp
    IF(T_RegenInTemp .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinRegenAirInTemp .OR. &
      T_RegenInTemp .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxRegenAirInTemp)THEN
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInTempLast = T_RegenInTemp
      OutputChar=RoundSigDigits(T_RegenInTemp,2)
      OutputCharLo=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinRegenAirInTemp,2)
      OutputCharHi=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxRegenAirInTemp,2)
      IF(T_RegenInTemp .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinRegenAirInTemp)THEN
        T_RegenInTemp = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinRegenAirInTemp
      END IF
      IF(T_RegenInTemp .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxRegenAirInTemp)THEN
        T_RegenInTemp = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxRegenAirInTemp
      END IF
      IF(.NOT. WarmupFlag .AND. .NOT. FirstHVACIteration)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_RegenInTempMessage = .TRUE.
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInTempBuffer1 = &
           TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
            TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// '" - Regeneration inlet air temperature ' &
          //'used in regen outlet air temperature equation is outside model boundaries at '//TRIM(OutputChar)//'.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInTempBuffer2 = '...Valid range = '//TRIM(OutputCharLo)//&
           ' to ' //TRIM(OutputCharHi)//'. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
           //TRIM(CreateSysTimeIntervalString())
        CharValue=RoundSigDigits(T_RegenInTemp,6)
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInTempBuffer3 = '...Regeneration outlet air temperature ' &
            //'equation: regeneration inlet air temperature passed to the model = '//TRIM(CharValue)
      ELSE
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_RegenInTempMessage = .FALSE.
      END IF
    ELSE
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_RegenInTempMessage = .FALSE.
    END IF
    ! regen inlet humidity ratio
    IF(T_RegenInHumRat .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinRegenAirInHumRat .OR. &
      T_RegenInHumRat .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxRegenAirInHumRat)THEN
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInHumRatLast = T_RegenInHumRat
      OutputChar=RoundSigDigits(T_RegenInHumRat,6)
      OutputCharLo=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinRegenAirInHumRat,6)
      OutputCharHi=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxRegenAirInHumRat,6)
      IF(T_RegenInHumRat .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinRegenAirInHumRat)THEN
        T_RegenInHumRat = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinRegenAirInHumRat
      END IF
      IF(T_RegenInHumRat .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxRegenAirInHumRat)THEN
        T_RegenInHumRat = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxRegenAirInHumRat
      END IF
      IF(.NOT. WarmupFlag .AND. .NOT. FirstHVACIteration)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_RegenInHumRatMessage = .TRUE.
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInHumRatBuffer1 = &
           TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
           TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)//'" - Regeneration inlet air' &
          //' humidity ratio used in regen outlet air temperature equation is outside model boundaries at '//TRIM(OutputChar)//'.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInHumRatBuffer2 = '...Valid range = '//TRIM(OutputCharLo) &
                     //' to '// TRIM(OutputCharHi)//'. Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//' '&
                       //TRIM(CreateSysTimeIntervalString())
        CharValue=RoundSigDigits(T_RegenInHumRat,6)
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_RegenInHumRatBuffer3 = '...Regeneration outlet air temperature ' &
                       //'equation: regeneration inlet air humidity ratio passed to the model = '//TRIM(CharValue)
      ELSE
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_RegenInHumRatMessage = .FALSE.
      END IF
    ELSE
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_RegenInHumRatMessage = .FALSE.
    END IF
    ! process inlet temp
    IF(T_ProcInTemp .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinProcAirInTemp .OR. &
      T_ProcInTemp .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxProcAirInTemp)THEN
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInTempLast = T_ProcInTemp
      OutputChar=RoundSigDigits(T_ProcInTemp,2)
      OutputCharLo=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinProcAirInTemp,2)
      OutputCharHi=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxProcAirInTemp,2)
      IF(T_ProcInTemp .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinProcAirInTemp)THEN
        T_ProcInTemp = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinProcAirInTemp
      END IF
      IF(T_ProcInTemp .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxProcAirInTemp)THEN
        T_ProcInTemp = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxProcAirInTemp
      END IF
      IF(.NOT. WarmupFlag .AND. .NOT. FirstHVACIteration)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_ProcInTempMessage = .TRUE.
!       Suppress warning message when process inlet temperature = 0 (DX coil is off)
        IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInTempLast == 0.0d0) &
            BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_ProcInTempMessage = .FALSE.
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInTempBuffer1 = &
           TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
                   TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)//'" - Process inlet air ' &
          //'temperature used in regen outlet air temperature equation is outside model boundaries at '//TRIM(OutputChar)//'.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInTempBuffer2 = '...Valid range = ' &
           //TRIM(OutputCharLo)//' to '//TRIM(OutputCharHi)//'. Occurrence info = '//TRIM(EnvironmentName)//',' &
           //Trim(CurMnDy)//' '//TRIM(CreateSysTimeIntervalString())
        CharValue=RoundSigDigits(T_ProcInTemp,6)
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInTempBuffer3 = &
         '...Regeneration outlet air temperature equation: process inlet air temperature passed to the model = '//TRIM(CharValue)
      ELSE
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_ProcInTempMessage = .FALSE.
      END IF
    ELSE
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_ProcInTempMessage = .FALSE.
    END IF
    ! process inlet humidity ratio
    IF(T_ProcInHumRat .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinProcAirInHumRat .OR. &
      T_ProcInHumRat .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxProcAirInHumRat)THEN
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInHumRatLast = T_ProcInHumRat
      OutputChar=RoundSigDigits(T_ProcInHumRat,6)
      OutputCharLo=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinProcAirInHumRat,6)
      OutputCharHi=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxProcAirInHumRat,6)
      IF(T_ProcInHumRat .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinProcAirInHumRat)THEN
        T_ProcInHumRat = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinProcAirInHumRat
      END IF
      IF(T_ProcInHumRat .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxProcAirInHumRat)THEN
        T_ProcInHumRat = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxProcAirInHumRat
      END IF
      IF(.NOT. WarmupFlag .AND. .NOT. FirstHVACIteration)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_ProcInHumRatMessage = .TRUE.
!       Suppress warning message when process inlet humrat = 0 (DX coil is off)
        IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInHumRatLast == 0.0d0) &
            BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_ProcInHumRatMessage = .FALSE.
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInHumRatBuffer1 = &
           TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
           TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)//'" - Process inlet air' &
          //' humidity ratio used in regen outlet air temperature equation is outside model boundaries at '//TRIM(OutputChar)//'.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInHumRatBuffer2 = &
          '...Valid range = '//TRIM(OutputCharLo)//' to ' //TRIM(OutputCharHi)//'. Occurrence info = '//TRIM(EnvironmentName) &
          //', '//Trim(CurMnDy)//' ' //TRIM(CreateSysTimeIntervalString())
        CharValue=RoundSigDigits(T_ProcInHumRat,6)
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInHumRatBuffer3 = '...Regeneration outlet air '// &
          'temperature equation: process inlet air humidity ratio passed to the model = '//TRIM(CharValue)
      ELSE
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_ProcInHumRatMessage = .FALSE.
      END IF
    ELSE
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_ProcInHumRatMessage = .FALSE.
    END IF
    ! regeneration and process face velocity
    IF(T_FaceVel .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinFaceVel .OR. &
      T_FaceVel .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxFaceVel)THEN
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_FaceVelLast = T_FaceVel
      OutputChar=RoundSigDigits(T_FaceVel,6)
      OutputCharLo=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinFaceVel,6)
      OutputCharHi=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxFaceVel,6)
      IF(T_FaceVel .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinFaceVel)THEN
        T_FaceVel = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinFaceVel
      END IF
      IF(T_FaceVel .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxFaceVel)THEN
        T_FaceVel = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxFaceVel
      END IF
      IF(.NOT. WarmupFlag .AND. .NOT. FirstHVACIteration)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_FaceVelMessage = .TRUE.
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_FaceVelBuffer1 = &
           TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
           TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// '" - Process and regen inlet air face ' &
           //'velocity used in regen outlet air temperature equation is outside model boundaries at '//TRIM(OutputChar)//'.'
           BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_FaceVelBuffer2 = '...Valid range = ' &
           // TRIM(OutputCharLo)//' to '// TRIM(OutputCharHi)//'. Occurrence info = '//TRIM(EnvironmentName)//', ' &
           // Trim(CurMnDy)//' ' //TRIM(CreateSysTimeIntervalString())
        CharValue=RoundSigDigits(T_FaceVel,6)
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_FaceVelBuffer3 = &
         '...Regeneration outlet air temperature equation: process and regen face velocity passed to the model = '//TRIM(CharValue)
      ELSE
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_FaceVelMessage = .FALSE.
      END IF
    ELSE
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintT_FaceVelMessage = .FALSE.
    END IF

END SUBROUTINE CheckModelBoundsTempEq

SUBROUTINE CheckModelBoundsHumRatEq(ExchNum, H_RegenInTemp, H_RegenInHumRat, H_ProcInTemp, H_ProcInHumRat, H_FaceVel, &
                                    FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Mangesh Basarkar, FSEC
          !       DATE WRITTEN   January 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !
          ! To verify that the empirical model's independent variables are within the limits used during the
          ! developement of the empirical model.

          ! METHODOLOGY EMPLOYED:
          ! The empirical models used for simulating a desiccant enhanced cooling coil are based on a limited data set.
          ! Extrapolation of empirical models can cause instability and the independent variables may need to be limited.
          ! The range of each independent variable is provided by the user and are based on the limits of the
          ! empirical model. These limits are tested in this subroutine each time step and returned for use by the calling
          ! routine.
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,         ONLY: CreateSysTimeIntervalString,RoundSigDigits
  USE DataGlobals,     ONLY: CurrentTime
  USE DataHVACGlobals, ONLY: SysTimeElapsed, TimeStepSys

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

  ! SUBROUTINE ARGUMENT DEFINITIONS:
   ! regen outlet humidity ratio equation
  INTEGER, INTENT(IN)    :: ExchNum                   ! number of the current heat exchanger being simulated
  REAL(r64),    INTENT(INOUT) :: H_RegenInTemp             ! current regen inlet temperature (C) for regen outlet hum rat eqn
  REAL(r64),    INTENT(INOUT) :: H_RegenInHumRat           ! current regen inlet hum rat for regen outlet hum rat eqn
  REAL(r64),    INTENT(INOUT) :: H_ProcInTemp              ! current process inlet temperature (C) for regen outlet hum rat eqn
  REAL(r64),    INTENT(INOUT) :: H_ProcInHumRat            ! current process inlet hum rat for regen outlet hum rat eqn
  REAL(r64),    INTENT(INOUT) :: H_FaceVel                 ! current process and regen face velocity (m/s)
  LOGICAL, INTENT(IN)    :: FirstHVACIteration        ! First HVAC iteration flag

      ! SUBROUTINE PARAMETER DEFINITIONS:
!  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F10.6)'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
 CHARACTER(len=32)        :: OutputChar         = ' '     ! character string for warning messages
 CHARACTER(len=32)        :: OutputCharLo       = ' '     ! character string for warning messages
 CHARACTER(len=32)        :: OutputCharHi       = ' '     ! character string for warning messages
 CHARACTER(len=32)        :: CharValue          = ' '     ! character string for warning messages
 REAL(r64),SAVE    :: TimeStepSysLast    = 0.0d0     ! last system time step (used to check for downshifting)
 REAL(r64)    :: CurrentEndTime     = 0.0d0     ! end time of time step for current simulation time step
 REAL(r64),SAVE    :: CurrentEndTimeLast = 0.0d0     ! end time of time step for last simulation time step
                                                          ! current end time is compared with last to see if time step changed


!   calculate end time of current time step
    CurrentEndTime = CurrentTime + SysTimeElapsed

!   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
!   Wait for next time step to print warnings. If simulation iterates, print out
!   the warning for the last iteration only. Must wait for next time step to accomplish this.
!   If a warning occurs and the simulation down shifts, the warning is not valid.
    IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

    ! print error for variables of regeneration outlet humidity ratio equation
      ! Regen inlet temp for humidity ratio eqn
     IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_RegenInTempMessage)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInTempErrorCount = &
                        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInTempErrorCount + 1
        IF (BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInTempErrorCount < 2) THEN
           CALL ShowWarningError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInTempBuffer1))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInTempBuffer2))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInTempBuffer3))
           CALL ShowContinueError('...Using regeneration inlet air temperatures that are outside the regeneration inlet ' &
                       //'air temperature equation model boundaries may adversely affect desiccant model performance.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
                         TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)//'" - Regeneration inlet air '//&
           'temperature used in regen outlet air humidity ratio equation is out of range error continues...',&
            BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInTempErrIndex, &
                                      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInTempLast, &
                                       BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInTempLast)
        END IF
      END IF
      ! Regen inlet humidity ratio for humidity ratio eqn
      IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_RegenInHumRatMessage)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInHumRatErrorCount = &
                        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInHumRatErrorCount + 1
        IF (BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInHumRatErrorCount < 2) THEN
           CALL ShowWarningError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInHumRatBuffer1))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInHumRatBuffer2))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInHumRatBuffer3))
           CALL ShowContinueError('...Using regeneration inlet air humidity ratios that are outside the regeneration ' &
                     //'outlet air humidity ratio equation model boundaries may adversely affect desiccant model performance.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
                                              TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// &
              '" - Regeneration inlet air humidity ratio used in regen outlet air humidity ratio equation is out of range ' &
              //'error continues...', BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInHumRatErrIndex, &
                                      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInHumRatLast, &
                                      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInHumRatLast)
        END IF
      END IF
      ! Process inlet temp for humidity ratio eqn
      IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_ProcInTempMessage)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInTempErrorCount = &
                        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInTempErrorCount + 1
        IF (BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInTempErrorCount < 2) THEN
           CALL ShowWarningError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInTempBuffer1))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInTempBuffer2))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInTempBuffer3))
           CALL ShowContinueError('...Using process inlet air temperatures that are outside the regeneration outlet ' &
                       //'air humidity ratio equation model may adversely affect desiccant model performance.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
              TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)//'" - Process inlet air '// &
              'temperature used in regen outlet air humidity ratio equation is out of range error continues...', &
                BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInTempErrIndex, &
                BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInTempLast, &
                BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInTempLast)
        END IF
      END IF
      ! Process inlet humidity ratio for humidity ratio eqn
      IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_ProcInHumRatMessage)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInHumRatErrorCount = &
                        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInHumRatErrorCount + 1
        IF (BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInHumRatErrorCount < 2) THEN
           CALL ShowWarningError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInHumRatBuffer1))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInHumRatBuffer2))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInHumRatBuffer3))
           CALL ShowContinueError('...Using process inlet air humidity ratios that are outside the regeneration outlet ' &
                      //'humidity ratio equation model boundaries may adversely affect desiccant model performance.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
              TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)//'" - Process inlet air '// &
              'humidity ratio used in regen outlet air humidity ratio equation is out of range error continues...', &
                BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_ProcInHumRatErrIndex, &
                BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInHumRatLast, &
                BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInHumRatLast)
        END IF
      END IF
      ! Process and regeneration face velocity for humidity ratio eqn
      IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_FaceVelMessage)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_FaceVelErrorCount = &
                        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_FaceVelErrorCount + 1
        IF (BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_FaceVelErrorCount < 2) THEN
           CALL ShowWarningError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_FaceVelBuffer1))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_FaceVelBuffer2))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_FaceVelBuffer3))
           CALL ShowContinueError('...Using process and regeneration face velocities that are outside the regeneration outlet ' &
                       //'air humidity ratio equation model boundaries may adversely affect desiccant model performance.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
            TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)//'" - Process and regen face '// &
            'velocity used in regen outlet air humidity ratio equation is out of range error continues...', &
            BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_FaceVelocityErrIndex, &
                                      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_FaceVelLast, &
                                       BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_FaceVelLast)
        END IF
      END IF
    END IF ! IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

!   save last system time step and last end time of current time step (used to determine if warning is valid)
    TimeStepSysLast    = TimeStepSys
    CurrentEndTimeLast = CurrentEndTime

!   If regen and procees inlet temperatures are the same the coil is off, do not print out of bounds warning for this case
    IF(ABS(H_RegenInTemp-H_ProcInTemp) .LT. Small)THEN
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_RegenInTempMessage = .FALSE.
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_RegenInHumRatMessage = .FALSE.
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_ProcInTempMessage = .FALSE.
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_ProcInHumRatMessage = .FALSE.
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_FaceVelMessage = .FALSE.
      RETURN
    END IF

!   check boundaries of independent variables and post warnings to individual buffers to print at end of time step
    ! checking model bounds for variables of regeneration outlet humidity ratio equation
    ! Regen inlet temp
    IF(H_RegenInTemp .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinRegenAirInTemp .OR. &
      H_RegenInTemp .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxRegenAirInTemp)THEN
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInTempLast = H_RegenInTemp
      OutputChar=RoundSigDigits(H_RegenInTemp,2)
      OutputCharLo=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinRegenAirInTemp,2)
      OutputCharHi=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxRegenAirInTemp,2)
      IF(H_RegenInTemp .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinRegenAirInTemp)THEN
        H_RegenInTemp = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinRegenAirInTemp
      END IF
      IF(H_RegenInTemp .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxRegenAirInTemp)THEN
        H_RegenInTemp = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxRegenAirInTemp
      END IF
      IF(.NOT. WarmupFlag .AND. .NOT. FirstHVACIteration)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_RegenInTempMessage = .TRUE.
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInTempBuffer1 = &
           TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
           TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// '" - Regeneration inlet air ' &
          //'temperature used in regen outlet air humidity ratio equation is outside model boundaries at ' //TRIM(OutputChar)//'.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInTempBuffer2 = '...Valid range = ' &
          //TRIM(OutputCharLo)//' to ' //TRIM(OutputCharHi)//'. Occurrence info = '//TRIM(EnvironmentName)//', ' &
          //Trim(CurMnDy)//' , ' //TRIM(CreateSysTimeIntervalString())
        CharValue=RoundSigDigits(H_RegenInTemp,2)
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInTempBuffer3 = '...Regeneration outlet air humidity ratio ' &
                         //'equation: regeneration inlet air temperature passed to the model = '//TRIM(CharValue)
      ELSE
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_RegenInTempMessage = .FALSE.
      END IF
    ELSE
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_RegenInTempMessage = .FALSE.
    END IF
    ! regen inlet humidity ratio
    IF(H_RegenInHumRat .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinRegenAirInHumRat .OR. &
      H_RegenInHumRat .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxRegenAirInHumRat)THEN
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInHumRatLast = H_RegenInHumRat
      OutputChar=RoundSigDigits(H_RegenInHumRat,6)
      OutputCharLo=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinRegenAirInHumRat,6)
      OutputCharHi=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxRegenAirInHumRat,6)
      IF(H_RegenInHumRat .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinRegenAirInHumRat)THEN
        H_RegenInHumRat = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinRegenAirInHumRat
      END IF
      IF(H_RegenInHumRat .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxRegenAirInHumRat)THEN
        H_RegenInHumRat = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxRegenAirInHumRat
      END IF
      IF(.NOT. WarmupFlag .AND. .NOT. FirstHVACIteration)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_RegenInHumRatMessage = .TRUE.
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInHumRatBuffer1 = &
          TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
          TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// '" - Regeneration inlet air humidity ' &
          // 'ratio used in regen outlet air humidity ratio equation is outside model boundaries at ' //TRIM(OutputChar)//'.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInHumRatBuffer2 = '...Valid range = ' &
          //TRIM(OutputCharLo)//' to ' //TRIM(OutputCharHi)//'. Occurrence info = '//TRIM(EnvironmentName)//', ' &
          //Trim(CurMnDy)//' ' //TRIM(CreateSysTimeIntervalString())
        CharValue=RoundSigDigits(H_RegenInHumRat,6)
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_RegenInHumRatBuffer3 = '...Regeneration outlet air humidity ' &
          //'ratio equation: regeneration inlet air humidity ratio passed to the model = '//TRIM(CharValue)
      ELSE
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_RegenInHumRatMessage = .FALSE.
      END IF
    ELSE
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_RegenInHumRatMessage = .FALSE.
    END IF
    ! process inlet temp
    IF(H_ProcInTemp .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinProcAirInTemp .OR. &
      H_ProcInTemp .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxProcAirInTemp)THEN
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInTempLast = H_ProcInTemp
      OutputChar=RoundSigDigits(H_ProcInTemp,2)
      OutputCharLo=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinProcAirInTemp,2)
      OutputCharHi=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxProcAirInTemp,2)
      IF(H_ProcInTemp .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinProcAirInTemp)THEN
        H_ProcInTemp = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinProcAirInTemp
      END IF
      IF(H_ProcInTemp .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxProcAirInTemp)THEN
        H_ProcInTemp = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxProcAirInTemp
      END IF
      IF(.NOT. WarmupFlag .AND. .NOT. FirstHVACIteration)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_ProcInTempMessage = .TRUE.
!       Suppress warning message when process inlet temperature = 0 (DX coil is off)
        IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInTempLast == 0.0d0) &
            BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_ProcInTempMessage = .FALSE.
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInTempBuffer1 = &
          TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
          TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// '" - Process inlet air temperature ' &
          // 'used in regen outlet air humidity ratio equation is outside model boundaries at '//TRIM(OutputChar)//'.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInTempBuffer2 = '...Valid range = ' &
          //TRIM(OutputCharLo)//' to ' //TRIM(OutputCharHi)//'. Occurrence info = '//TRIM(EnvironmentName)//', ' &
          //Trim(CurMnDy)//' ' //TRIM(CreateSysTimeIntervalString())
        CharValue=RoundSigDigits(H_ProcInTemp,6)
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInTempBuffer3 = '...Regeneration outlet air humidity ratio ' &
             //'equation: process inlet air temperature passed to the model = '//TRIM(CharValue)
      ELSE
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_ProcInTempMessage = .FALSE.
      END IF
    ELSE
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_ProcInTempMessage = .FALSE.
    END IF
    ! process inlet humidity ratio
    IF(H_ProcInHumRat .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinProcAirInHumRat .OR. &
      H_ProcInHumRat .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxProcAirInHumRat)THEN
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInHumRatLast = H_ProcInHumRat
      OutputChar=RoundSigDigits(H_ProcInHumRat,6)
      OutputCharLo=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinProcAirInHumRat,6)
      OutputCharHi=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxProcAirInHumRat,6)
      IF(H_ProcInHumRat .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinProcAirInHumRat)THEN
        H_ProcInHumRat = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinProcAirInHumRat
      END IF
      IF(H_ProcInHumRat .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxProcAirInHumRat)THEN
        H_ProcInHumRat = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxProcAirInHumRat
      END IF
      IF(.NOT. WarmupFlag .AND. .NOT. FirstHVACIteration)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_ProcInHumRatMessage = .TRUE.
!       Suppress warning message when process inlet humrat = 0 (DX coil is off)
        IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInHumRatLast == 0.0d0) &
            BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_ProcInHumRatMessage = .FALSE.
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInHumRatBuffer1 = &
          TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
          TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// '" - Process inlet air humidity ratio ' &
          // 'used in regen outlet air humidity ratio equation is outside model boundaries at '//TRIM(OutputChar)//'.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInHumRatBuffer2 = '...Valid range = ' &
          //TRIM(OutputCharLo)//' to ' //TRIM(OutputCharHi)//'. Occurrence info = '//TRIM(EnvironmentName)//', ' &
          //Trim(CurMnDy)//', ' //TRIM(CreateSysTimeIntervalString())
        CharValue=RoundSigDigits(H_ProcInHumRat,6)
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_ProcInHumRatBuffer3 = '...Regeneration outlet air humidity ' &
            //'ratio equation: process inlet air humidity ratio passed to the model = '//TRIM(CharValue)
      ELSE
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_ProcInHumRatMessage = .FALSE.
      END IF
    ELSE
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_ProcInHumRatMessage = .FALSE.
    END IF
    ! regeneration and process face velocity
    IF(H_FaceVel .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinFaceVel .OR. &
      H_FaceVel .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxFaceVel)THEN
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_FaceVelLast = H_FaceVel
      OutputChar=RoundSigDigits(H_FaceVel,6)
      OutputCharLo=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinFaceVel,6)
      OutputCharHi=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxFaceVel,6)
      IF(H_FaceVel .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinFaceVel)THEN
        H_FaceVel = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinFaceVel
      END IF
      IF(H_FaceVel .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxFaceVel)THEN
        H_FaceVel = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxFaceVel
      END IF
      IF(.NOT. WarmupFlag .AND. .NOT. FirstHVACIteration)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_FaceVelMessage = .TRUE.
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_FaceVelBuffer1 = &
          TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
          TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// '" - Process and regen inlet air face ' &
          // 'velocity used in regen outlet air humidity ratio equation is outside model boundaries at '//TRIM(OutputChar)//'.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_FaceVelBuffer2 = '...Valid range = ' &
          //TRIM(OutputCharLo)//' to ' //TRIM(OutputCharHi)//'. Occurrence info = '//TRIM(EnvironmentName)//', ' &
          //Trim(CurMnDy)//', ' //TRIM(CreateSysTimeIntervalString())
        CharValue=RoundSigDigits(H_FaceVel,6)
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_FaceVelBuffer3 = '...Regeneration outlet air humidity ratio ' &
             //'equation: process and regeneration face velocity passed to the model = '//TRIM(CharValue)
      ELSE
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_FaceVelMessage = .FALSE.
      END IF
    ELSE
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintH_FaceVelMessage = .FALSE.
    END IF

END SUBROUTINE CheckModelBoundsHumRatEq

SUBROUTINE CheckModelBoundOutput_Temp(ExchNum, RegenInTemp, RegenOutTemp, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Mangesh Basarkar, FSEC
          !       DATE WRITTEN   January 2007
          !       MODIFIED       June 2007, R. Raustad, changed requirement that regen outlet temp be less than inlet temp
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !
          ! To verify that the empirical model's independent variables are within the limits used during the
          ! developement of the empirical model.

          ! METHODOLOGY EMPLOYED:
          ! The empirical models used for simulating a desiccant enhanced cooling coil are based on a limited data set.
          ! Extrapolation of empirical models can cause instability and the independent variables may need to be limited.
          ! The range of each independent variable is provided by the user and are based on the limits of the
          ! empirical model. These limits are tested in this subroutine each time step and returned for use by the calling
          ! routine.
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,         ONLY: CreateSysTimeIntervalString,RoundSigDigits
  USE DataGlobals,     ONLY: CurrentTime
  USE DataHVACGlobals, ONLY: SysTimeElapsed, TimeStepSys

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

  ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT (IN)   :: ExchNum            ! number of the current heat exchanger being simulated
  REAL(r64),    INTENT (IN)   :: RegenInTemp        ! current regen inlet temp passed to eqn
  REAL(r64),    INTENT(INOUT) :: RegenOutTemp       ! current regen outlet temp from eqn
  LOGICAL, INTENT (IN)   :: FirstHVACIteration ! First HVAC iteration flag

        ! SUBROUTINE PARAMETER DEFINITIONS:
!  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F10.6)'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
 CHARACTER(len=32)        :: OutputChar         = ' '     ! character string for warning messages
 CHARACTER(len=32)        :: OutputCharLo       = ' '     ! character string for warning messages
 CHARACTER(len=32)        :: OutputCharHi       = ' '     ! character string for warning messages
 CHARACTER(len=32)        :: CharValue          = ' '     ! character string for warning messages
 REAL(r64),SAVE    :: TimeStepSysLast    = 0.0d0     ! last system time step (used to check for downshifting)
 REAL(r64)    :: CurrentEndTime     = 0.0d0     ! end time of time step for current simulation time step
 REAL(r64),SAVE    :: CurrentEndTimeLast = 0.0d0     ! end time of time step for last simulation time step
                                                          ! current end time is compared with last to see if time step changed


!   calculate end time of current time step
    CurrentEndTime = CurrentTime + SysTimeElapsed

!   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
!   Wait for next time step to print warnings. If simulation iterates, print out
!   the warning for the last iteration only. Must wait for next time step to accomplish this.
!   If a warning occurs and the simulation down shifts, the warning is not valid.
    IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

      ! print error when regeneration outlet temperature is greater than regen inlet temperature
      IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenOutTempFailedMessage)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempFailedErrorCount = &
                        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempFailedErrorCount + 1
        IF (BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempFailedErrorCount < 2) THEN
           CALL ShowWarningError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempFailedBuffer1))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempFailedBuffer2))
           CALL ShowContinueError('...Regeneration outlet air temperature should always be less than or equal ' &
                                  //'to regen inlet air temperature. Verify correct model coefficients.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
                                              TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// &
              '" - Regeneration outlet air temperature above regen inlet air temperature error continues...', &
              BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempFailedErrIndex, &
                                      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempFailedLast, &
                                       BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempFailedLast)
        END IF
      END IF

      ! print error for variables of regeneration outlet temperature
      IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenOutTempMessage)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempErrorCount = &
                        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempErrorCount + 1
        IF (BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempErrorCount < 2) THEN
           CALL ShowWarningError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempBuffer1))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempBuffer2))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempBuffer3))
           CALL ShowContinueError('...Regeneration outlet air temperature should always be less than or equal ' &
                                  //'to regen inlet air temperature. Verify correct model coefficients.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
                                              TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// &
              '" - Regeneration outlet air temperature should be less than regen inlet air temperature error continues...', &
              BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempErrIndex, &
                                      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempLast, &
                                       BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempLast)
        END IF
      END IF
    END IF ! IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

!   save last system time step and last end time of current time step (used to determine if warning is valid)
    TimeStepSysLast    = TimeStepSys
    CurrentEndTimeLast = CurrentEndTime

    ! checking model regeneration outlet temperature to always be less than or equal to regeneration inlet temperature
    IF(RegenOutTemp .GT. RegenInTemp)THEN
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempFailedLast = RegenOutTemp
      OutputChar=RoundSigDigits(RegenOutTemp,2)
      OutputCharHi=RoundSigDigits(RegenInTemp,2)
!      IF(RegenOutTemp .GT. RegenInTemp)THEN
!        RegenOutTemp = RegenInTemp
!      END IF
      IF(.NOT. WarmupFlag .AND. .NOT. FirstHVACIteration)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenOutTempFailedMessage = .TRUE.
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempFailedBuffer1 = &
          TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
          TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// '" - Regeneration outlet air ' &
          // 'temperature is greater than inlet temperature at ' //TRIM(OutputChar)//'.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempFailedBuffer2 = &
           '...Regen inlet air temperature = '//TRIM(OutputCharHi)// &
          '. Occurrence info = '//TRIM(EnvironmentName)//', ' &
          //Trim(CurMnDy)//', ' //TRIM(CreateSysTimeIntervalString())
        CharValue=RoundSigDigits(RegenOutTemp,6)
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempFailedBuffer3 = '...Regen outlet air temperature ' &
             //'equation: regeneration outlet air temperature allowed from the model = ' //TRIM(CharValue)
      ELSE
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenOutTempMessage = .FALSE.
      END IF
    ELSE
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenOutTempMessage = .FALSE.
    END IF

!   check boundaries of regen outlet temperature and post warnings to individual buffers to print at end of time step
    ! checking model bounds for regeneration outlet temperature
    IF(RegenOutTemp .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%MinRegenAirOutTemp .OR. &
      RegenOutTemp .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%MaxRegenAirOutTemp)THEN
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempLast = RegenOutTemp
      OutputChar=RoundSigDigits(RegenOutTemp,2)
      OutputCharLo=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%MinRegenAirOutTemp,2)
      OutputCharHi=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%MaxRegenAirOutTemp,2)
      IF(RegenOutTemp .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%MinRegenAirOutTemp)THEN
        RegenOutTemp = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%MinRegenAirOutTemp
      END IF
      IF(RegenOutTemp .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%MaxRegenAirOutTemp)THEN
        RegenOutTemp = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%MaxRegenAirOutTemp
      END IF
      IF(.NOT. WarmupFlag .AND. .NOT. FirstHVACIteration)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenOutTempMessage = .TRUE.
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempBuffer1 = &
          TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
          TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// '" - Regeneration outlet air ' &
          // 'temperature equation is outside model boundaries at ' //TRIM(OutputChar)//'.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempBuffer2 = '...Valid range = ' &
          //TRIM(OutputCharLo)//' to ' //TRIM(OutputCharHi)//'. Occurrence info = '//TRIM(EnvironmentName)//', ' &
          //Trim(CurMnDy)//', ' //TRIM(CreateSysTimeIntervalString())
        CharValue=RoundSigDigits(RegenOutTemp,6)
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutTempBuffer3 = '...Regen outlet air temperature equation: ' &
              //'regeneration outlet air temperature allowed from the model = '//TRIM(CharValue)
      ELSE
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenOutTempMessage = .FALSE.
      END IF
    ELSE
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenOutTempMessage = .FALSE.
    END IF

END SUBROUTINE CheckModelBoundOutput_Temp

SUBROUTINE CheckModelBoundOutput_HumRat(ExchNum, RegenInHumRat, RegenOutHumRat, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Mangesh Basarkar, FSEC
          !       DATE WRITTEN   January 2007
          !       MODIFIED       June 2007, R. Raustad, changed requirement that regen outlet temp be less than inlet temp
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !
          ! To verify that the empirical model's independent variables are within the limits used during the
          ! developement of the empirical model.

          ! METHODOLOGY EMPLOYED:
          ! The empirical models used for simulating a desiccant enhanced cooling coil are based on a limited data set.
          ! Extrapolation of empirical models can cause instability and the independent variables may need to be limited.
          ! The range of each independent variable is provided by the user and are based on the limits of the
          ! empirical model. These limits are tested in this subroutine each time step and returned for use by the calling
          ! routine.
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,         ONLY: CreateSysTimeIntervalString,RoundSigDigits
  USE DataGlobals,     ONLY: CurrentTime
  USE DataHVACGlobals, ONLY: SysTimeElapsed, TimeStepSys

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

  ! SUBROUTINE ARGUMENT DEFINITIONS
  INTEGER, INTENT(IN)    :: ExchNum            ! number of the current heat exchanger being simulated
  REAL(r64),    INTENT(IN)    :: RegenInHumRat      ! current regen inlet hum rat passed to eqn
  REAL(r64),    INTENT(INOUT) :: RegenOutHumRat     ! current regen outlet hum rat from eqn
  LOGICAL, INTENT (IN)   :: FirstHVACIteration ! First HVAC iteration flag

         ! SUBROUTINE PARAMETER DEFINITIONS:
!  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F10.6)'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
 CHARACTER(len=32)        :: OutputChar         = ' '     ! character string for warning messages
 CHARACTER(len=32)        :: OutputCharLo       = ' '     ! character string for warning messages
 CHARACTER(len=32)        :: OutputCharHi       = ' '     ! character string for warning messages
 CHARACTER(len=32)        :: CharValue          = ' '     ! character string for warning messages
 REAL(r64),SAVE    :: TimeStepSysLast    = 0.0d0     ! last system time step (used to check for downshifting)
 REAL(r64)    :: CurrentEndTime     = 0.0d0     ! end time of time step for current simulation time step
 REAL(r64),SAVE    :: CurrentEndTimeLast = 0.0d0     ! end time of time step for last simulation time step
                                                          ! current end time is compared with last to see if time step changed


!   calculate end time of current time step
    CurrentEndTime = CurrentTime + SysTimeElapsed

!   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
!   Wait for next time step to print warnings. If simulation iterates, print out
!   the warning for the last iteration only. Must wait for next time step to accomplish this.
!   If a warning occurs and the simulation down shifts, the warning is not valid.
    IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

     ! print error when regeneration outlet humidity ratio is less than regeneration inlet humidity ratio
     IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenOutHumRatFailedMess)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatFailedErrorCount = &
                        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatFailedErrorCount + 1
        IF (BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatFailedErrorCount < 2) THEN
           CALL ShowWarningError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatFailedBuffer1))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatFailedBuffer2))
           CALL ShowContinueError('...Regeneration outlet air humidity ratio should always be greater than or equal ' &
                                  //'to regen inlet air humidity ratio. Verify correct model coefficients.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
            TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// &
            '" - Regeneration outlet air humidity ratio should be greater than regen inlet air humidity ratio error continues...',&
            BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatFailedErrIndex, &
            BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatFailedLast, &
            BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatFailedLast)
        END IF
      END IF

     ! print error for regeneration outlet humidity ratio
     IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenOutHumRatMessage)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatErrorCount = &
                        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatErrorCount + 1
        IF (BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatErrorCount < 2) THEN
           CALL ShowWarningError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatBuffer1))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatBuffer2))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatBuffer3))
           CALL ShowContinueError('...Regeneration outlet air humidity ratio outside model boundaries may ' &
                                  //'adversely affect desiccant model performance.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
              TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// &
              '" - Regeneration outlet air humidity ratio is out of range error continues...', &
              BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatErrIndex, &
                                      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatLast, &
                                       BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatLast)
        END IF
      END IF
    END IF ! IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

!   save last system time step and last end time of current time step (used to determine if warning is valid)
    TimeStepSysLast    = TimeStepSys
    CurrentEndTimeLast = CurrentEndTime

    ! checking for regeneration outlet humidity ratio less than or equal to regeneration inlet humidity ratio
    IF(RegenOutHumRat .LT. RegenInHumRat)THEN
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatFailedLast = RegenOutHumRat
      OutputChar=RoundSigDigits(RegenOutHumRat,6)
      OutputCharHi=RoundSigDigits(RegenInHumRat,6)
!      IF(RegenOutHumRat .LT. RegenInHumRat)THEN
!        RegenOutHumRat = RegenInHumRat
!      END IF
      IF(.NOT. WarmupFlag .AND. .NOT. FirstHVACIteration)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenOutHumRatFailedMess = .TRUE.
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatFailedBuffer1 = &
         TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
         TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// '" - Regeneration outlet air ' &
         // 'humidity ratio is less than the inlet air humidity ratio at '//TRIM(OutputChar)//'.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatFailedBuffer2 = '...Regen inlet air humidity ' &
         //'ratio = '//TRIM(OutputCharHi)//'. Occurrence info = '//TRIM(EnvironmentName)//', ' &
         //Trim(CurMnDy)//', ' //TRIM(CreateSysTimeIntervalString())
        CharValue=RoundSigDigits(RegenOutHumRat,6)
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatFailedBuffer3 = '...Regen outlet air humidity ' &
             //'ratio equation: regeneration outlet air humidity ratio allowed from the model = ' //TRIM(CharValue)
      ELSE
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenOutHumRatFailedMess = .FALSE.
      END IF
    ELSE
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenOutHumRatFailedMess = .FALSE.
    END IF

!   check boundaries of regen outlet humrat and post warnings to individual buffers to print at end of time step
    ! checking model bounds for regeneration outlet humidity ratio
    IF(RegenOutHumRat .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%MinRegenAirOutHumRat .OR. &
      RegenOutHumRat .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%MaxRegenAirOutHumRat)THEN
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatLast = RegenOutHumRat
      OutputChar=RoundSigDigits(RegenOutHumRat,6)
      OutputCharLo=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%MinRegenAirOutHumRat,6)
      OutputCharHi=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%MaxRegenAirOutHumRat,6)
      IF(RegenOutHumRat .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%MinRegenAirOutHumRat)THEN
        RegenOutHumRat = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%MinRegenAirOutHumRat
      END IF
      IF(RegenOutHumRat .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%MaxRegenAirOutHumRat)THEN
        RegenOutHumRat = BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%MaxRegenAirOutHumRat
      END IF
      IF(.NOT. WarmupFlag .AND. .NOT. FirstHVACIteration)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenOutHumRatMessage = .TRUE.
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatBuffer1 = &
         TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
         TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// '" - Regeneration outlet air ' &
         // 'humidity ratio is outside model boundaries at '//TRIM(OutputChar)//'.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatBuffer2 = '...Valid range = ' &
         //TRIM(OutputCharLo)//' to ' //TRIM(OutputCharHi)//'. Occurrence info = '//TRIM(EnvironmentName)//', ' &
         //Trim(CurMnDy)//', ' //TRIM(CreateSysTimeIntervalString())
        CharValue=RoundSigDigits(RegenOutHumRat,6)
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenOutHumRatBuffer3 = '...Regen outlet air humidity ' &
             //'ratio equation: regeneration outlet air humidity ratio allowed from the model = ' //TRIM(CharValue)
      ELSE
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenOutHumRatMessage = .FALSE.
      END IF
    ELSE
      BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenOutHumRatMessage = .FALSE.
    END IF

END SUBROUTINE CheckModelBoundOutput_HumRat

SUBROUTINE CheckModelBoundsRH_TempEq(ExchNum, T_RegenInTemp, T_RegenInHumRat, T_ProcInTemp, T_ProcInHumRat, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   January 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !
          ! To verify that the empirical model's independent variables result in a relative humidity that is within the range
          ! of relative humidities used when creating the empirical model. Both the regeneration and process inlet are tested.

          ! METHODOLOGY EMPLOYED:
          ! The empirical models used for simulating a desiccant enhanced cooling coil are based on a limited data set.
          ! Extrapolation of empirical models can cause instability and the independent variables may need to be limited.
          ! In addition, the range of relative humidities in the original data set may influence the output of the
          ! empirical model. This subroutine tests the relative humidities passed to the empirical model and warns the
          ! user if these relative humidities are out of bounds based on the limits set by the user.
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,         ONLY: CreateSysTimeIntervalString,RoundSigDigits
  USE DataGlobals,     ONLY: CurrentTime
  USE DataHVACGlobals, ONLY: SysTimeElapsed, TimeStepSys
  USE Psychrometrics,  ONLY: PsyRhFnTdbWPb

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

  ! SUBROUTINE ARGUMENT DEFINITIONS
  INTEGER, INTENT(IN) :: ExchNum            ! number of the current heat exchanger being simulated
  REAL(r64),    INTENT(IN) :: T_RegenInTemp      ! current regen inlet temperature passed to eqn
  REAL(r64),    INTENT(IN) :: T_RegenInHumRat    ! current regen inlet hum rat passed to eqn
  REAL(r64),    INTENT(IN) :: T_ProcInTemp       ! current process inlet temperature passed to eqn
  REAL(r64),    INTENT(IN) :: T_ProcInHumRat     ! current regen outlet hum rat from eqn
  LOGICAL, INTENT(IN) :: FirstHVACIteration ! first HVAC iteration flag

         ! SUBROUTINE PARAMETER DEFINITIONS:
!  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F10.6)'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
 REAL(r64)                :: RegenInletRH       = 0.0d0     ! Regeneration inlet air relative humidity
 REAL(r64)                :: ProcInletRH        = 0.0d0     ! Process inlet air relative humidity
 CHARACTER(len=32)        :: OutputChar         = ' '     ! character string for warning messages
 CHARACTER(len=32)        :: OutputCharLo       = ' '     ! character string for warning messages
 CHARACTER(len=32)        :: OutputCharHi       = ' '     ! character string for warning messages
 REAL(r64),SAVE    :: TimeStepSysLast    = 0.0d0     ! last system time step (used to check for downshifting)
 REAL(r64)    :: CurrentEndTime     = 0.0d0     ! end time of time step for current simulation time step
 REAL(r64),SAVE    :: CurrentEndTimeLast = 0.0d0     ! end time of time step for last simulation time step
                                                          ! current end time is compared with last to see if time step changed
    IF(WarmupFlag .OR. FirstHVACIteration)RETURN

!   calculate end time of current time step
    CurrentEndTime = CurrentTime + SysTimeElapsed

!   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
!   Wait for next time step to print warnings. If simulation iterates, print out
!   the warning for the last iteration only. Must wait for next time step to accomplish this.
!   If a warning occurs and the simulation down shifts, the warning is not valid.
    IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

      ! print error when regeneration inlet relative humidity is outside model boundaries
      IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenInRelHumTempMess)THEN
         BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumTempErrorCount = &
                        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumTempErrorCount + 1
        IF (BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumTempErrorCount < 2) THEN
           CALL ShowWarningError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumTempBuffer1))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumTempBuffer2))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumTempBuffer3))
           CALL ShowContinueError('...Using regeneration inlet air relative humidities that are outside the regeneration '&
                 //'outlet temperature equation model boundaries may adversely affect desiccant model performance. '&
                 //'Verify correct model coefficients.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
                                              TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// &
                 '" - Regeneration inlet air relative humidity related to regen outlet air temperature equation is outside '// &
                 'model boundaries error continues...', &
                 BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumTempErrIndex, &
                 BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumTempLast, &
                 BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumTempLast)
        END IF
      END IF

      ! print error when process inlet relative humidity is outside model boundaries
      IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintProcInRelHumTempMess)THEN
         BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumTempErrorCount = &
                        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumTempErrorCount + 1
        IF (BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumTempErrorCount < 2) THEN
           CALL ShowWarningError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumTempBuffer1))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumTempBuffer2))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumTempBuffer3))
           CALL ShowContinueError('...Using process inlet air relative humidities that are outside the regeneration '&
                 //'outlet temperature equation model boundaries may adversely affect desiccant model performance. '&
                 //'Verify correct model coefficients.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
                                              TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// &
                 '" - Process inlet air relative humidity related to regen outlet air temperature equation is outside '// &
                 'model boundaries error continues...', &
                 BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumTempErrIndex, &
                 BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumTempLast, &
                 BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumTempLast)
        END IF
      END IF

    END IF ! IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

!     save last system time step and last end time of current time step (used to determine if warning is valid)
      TimeStepSysLast    = TimeStepSys
      CurrentEndTimeLast = CurrentEndTime

!     Check that condition is not above saturation curve prior to next calc (PsyRhFnTdbWPb) to avoid psyc routine errors
!
!                           *
!                          *
!                  x------*---------- T_HumRat
!                  |    *
!                  |  *
!                  *----------------- PsyWFnTdpPb(Tdp,Pb)
!               *  |
!                  |
!
!                T_Temp
!
      IF(T_RegenInHumRat .GT. PsyWFnTdpPb(T_RegenInTemp,OutBaroPress) .OR. &
         T_ProcInHumRat  .GT. PsyWFnTdpPb(T_ProcInTemp ,OutBaroPress)) THEN
!       reset RH print flags just in case
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenInRelHumTempMess = .FALSE.
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintProcInRelHumTempMess  = .FALSE.
        RETURN
      END IF

!     If regen and procees inlet temperatures are the same the coil is off, do not print out of bounds warning for this case
      IF(ABS(T_RegenInTemp-T_ProcInTemp) .LT. Small)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenInRelHumTempMess = .FALSE.
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintProcInRelHumTempMess = .FALSE.
        RETURN
      END IF

      RegenInletRH = PsyRhFnTdbWPb(T_RegenInTemp, T_RegenInHumRat,OutBaroPress)
      ProcInletRH  = MIN(1.0d0,PsyRhFnTdbWPb(T_ProcInTemp,  T_ProcInHumRat, OutBaroPress))

      ! checking if regeneration inlet relative humidity is within model boundaries
      IF(RegenInletRH .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinRegenAirInRelHum .OR. &
        RegenInletRH .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxRegenAirInRelHum)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumTempLast = RegenInletRH*100.0d0
        OutputChar=RoundSigDigits(RegenInletRH*100.0d0,1)
        OutputCharLo=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinRegenAirInRelHum*100.0d0,1)
        OutputCharHi=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxRegenAirInRelHum*100.0d0,1)
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenInRelHumTempMess = .TRUE.

        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumTempBuffer1 = &
           TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
           TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// '" - Regeneration inlet air relative ' &
           //'humidity related to regen outlet air temperature equation is outside model boundaries at '//TRIM(OutputChar)//'.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumTempBuffer2 = &
           '...Model limit on regeneration inlet air relative humidity is '//TRIM(OutputCharLo)//' to '//TRIM(OutputCharHi)//'.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumTempBuffer3 = &
           '...Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//', ' //TRIM(CreateSysTimeIntervalString())
      ELSE
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenInRelHumTempMess = .FALSE.
      END IF

      ! checking if process inlet relative humidity is within model boundaries
      IF(ProcInletRH .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinProcAirInRelHum .OR. &
        ProcInletRH .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxProcAirInRelHum)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumTempLast = ProcInletRH*100.0d0
        OutputChar=RoundSigDigits(ProcInletRH*100.0d0,1)
        OutputCharLo=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MinProcAirInRelHum*100.0d0,1)
        OutputCharHi=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%T_MaxProcAirInRelHum*100.0d0,1)
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintProcInRelHumTempMess = .TRUE.

        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumTempBuffer1 = &
           TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
           TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// '" - Process inlet air relative ' &
           //'humidity related to regen outlet air temperature equation is outside model boundaries at '//TRIM(OutputChar)//'.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumTempBuffer2 = &
           '...Model limit on process inlet air relative humidity is '//TRIM(OutputCharLo)//' to '//TRIM(OutputCharHi)//'.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumTempBuffer3 = &
           '...Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//', ' //TRIM(CreateSysTimeIntervalString())
      ELSE
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintProcInRelHumTempMess = .FALSE.
      END IF

END SUBROUTINE CheckModelBoundsRH_TempEq

SUBROUTINE CheckModelBoundsRH_HumRatEq(ExchNum, H_RegenInTemp, H_RegenInHumRat, H_ProcInTemp, H_ProcInHumRat, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   January 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !
          ! To verify that the empirical model's independent variables result in a relative humidity that is within the range
          ! of relative humidities used when creating the empirical model. Both the regeneration and process inlet are tested.

          ! METHODOLOGY EMPLOYED:
          ! The empirical models used for simulating a desiccant enhanced cooling coil are based on a limited data set.
          ! Extrapolation of empirical models can cause instability and the independent variables may need to be limited.
          ! In addition, the range of relative humidities in the original data set may influence the output of the
          ! empirical model. This subroutine tests the relative humidities passed to the empirical model and warns the
          ! user if these relative humidities are out of bounds based on the limits set by the user.
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,         ONLY: CreateSysTimeIntervalString,RoundSigDigits
  USE DataGlobals,     ONLY: CurrentTime
  USE DataHVACGlobals, ONLY: SysTimeElapsed, TimeStepSys
  USE Psychrometrics,  ONLY: PsyRhFnTdbWPb

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

  ! SUBROUTINE ARGUMENT DEFINITIONS
  INTEGER, INTENT(IN) :: ExchNum            ! number of the current heat exchanger being simulated
  REAL(r64),    INTENT(IN) :: H_RegenInTemp      ! current regen inlet temperature passed to eqn
  REAL(r64),    INTENT(IN) :: H_RegenInHumRat    ! current regen inlet hum rat passed to eqn
  REAL(r64),    INTENT(IN) :: H_ProcInTemp       ! current process inlet temperature passed to eqn
  REAL(r64),    INTENT(IN) :: H_ProcInHumRat     ! current process inlet hum rat passed to eqn
  LOGICAL, INTENT(IN) :: FirstHVACIteration ! first HVAC iteration flag

         ! SUBROUTINE PARAMETER DEFINITIONS:
!  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F10.6)'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
 REAL(r64)                :: RegenInletRH       = 0.0d0     ! Regeneration inlet air relative humidity
 REAL(r64)                :: ProcInletRH        = 0.0d0     ! Process inlet air relative humidity
 CHARACTER(len=32)        :: OutputChar         = ' '     ! character string for warning messages
 CHARACTER(len=32)        :: OutputCharLo       = ' '     ! character string for warning messages
 CHARACTER(len=32)        :: OutputCharHi       = ' '     ! character string for warning messages
 REAL(r64),SAVE    :: TimeStepSysLast    = 0.0d0     ! last system time step (used to check for downshifting)
 REAL(r64)    :: CurrentEndTime     = 0.0d0     ! end time of time step for current simulation time step
 REAL(r64),SAVE    :: CurrentEndTimeLast = 0.0d0     ! end time of time step for last simulation time step
                                                          ! current end time is compared with last to see if time step changed
    IF(WarmupFlag .OR. FirstHVACIteration)RETURN

!   calculate end time of current time step
    CurrentEndTime = CurrentTime + SysTimeElapsed

!   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
!   Wait for next time step to print warnings. If simulation iterates, print out
!   the warning for the last iteration only. Must wait for next time step to accomplish this.
!   If a warning occurs and the simulation down shifts, the warning is not valid.
    IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

      ! print error when regeneration inlet relative humidity is outside model boundaries
      IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenInRelHumHumRatMess)THEN
         BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumHumRatErrorCount = &
                        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumHumRatErrorCount + 1
        IF (BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumHumRatErrorCount < 2) THEN
           CALL ShowWarningError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumHumRatBuffer1))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumHumRatBuffer2))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumHumRatBuffer3))
           CALL ShowContinueError('...Using regeneration inlet air relative humidities that are outside the regeneration '&
                 //'outlet humidity ratio equation model boundaries may adversely affect desiccant model performance. '&
                 //'Verify correct model coefficients.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
                                              TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// &
                 '" - Regeneration inlet air relative humidity related to regen outlet air humidity ratio equation is '// &
                 'outside model boundaries error continues...', &
                 BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumHumRatErrIndex, &
                 BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumHumRatLast, &
                 BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumHumRatLast)
        END IF
      END IF

      ! print error when process inlet relative humidity is outside model boundaries
      IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintProcInRelHumHumRatMess)THEN
         BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumHumRatErrorCount = &
                        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumHumRatErrorCount + 1
        IF (BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumHumRatErrorCount < 2) THEN
           CALL ShowWarningError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumHumRatBuffer1))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumHumRatBuffer2))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumHumRatBuffer3))
           CALL ShowContinueError('...Using process inlet air relative humidities that are outside the regeneration '&
                 //'outlet humidity ratio equation model boundaries may adversely affect desiccant model performance. '&
                 //'Verify correct model coefficients.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
                                              TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// &
                 '" - Process inlet air relative humidity related to regen outlet air humidity ratio equation is outside '// &
                 'model boundaries error continues...', &
                 BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumHumRatErrIndex, &
                 BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumHumRatLast, &
                 BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumHumRatLast)
        END IF
      END IF

    END IF ! IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

!     save last system time step and last end time of current time step (used to determine if warning is valid)
      TimeStepSysLast    = TimeStepSys
      CurrentEndTimeLast = CurrentEndTime

!     Check that condition is not above saturation curve prior to next calc (PsyRhFnTdbWPb) to avoid psyc routine errors
!
!                           *
!                          *
!                  x------*---------- H_HumRat
!                  |    *
!                  |  *
!                  *----------------- PsyWFnTdpPb(Tdp,Pb)
!               *  |
!                  |
!
!                H_Temp
!
      IF(H_RegenInHumRat .GT. PsyWFnTdpPb(H_RegenInTemp,OutBaroPress) .OR. &
         H_ProcInHumRat  .GT. PsyWFnTdpPb(H_ProcInTemp ,OutBaroPress)) THEN
!       reset RH print flags just in case
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenInRelHumHumRatMess = .FALSE.
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintProcInRelHumHumRatMess  = .FALSE.
        RETURN
      END IF

!     If regen and procees inlet temperatures are the same the coil is off, do not print out of bounds warning for this case
      IF(ABS(H_RegenInTemp-H_ProcInTemp) .LT. Small)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenInRelHumHumRatMess = .FALSE.
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintProcInRelHumHumRatMess = .FALSE.
        RETURN
      END IF

      RegenInletRH = PsyRhFnTdbWPb(H_RegenInTemp, H_RegenInHumRat,OutBaroPress)
      ProcInletRH  = MIN(1.0d0,PsyRhFnTdbWPb(H_ProcInTemp,  H_ProcInHumRat, OutBaroPress))

      ! checking if regeneration inlet relative humidity is within model boundaries
      IF(RegenInletRH .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinRegenAirInRelHum .OR. &
        RegenInletRH .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxRegenAirInRelHum)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumHumRatLast = RegenInletRH*100.0d0
        OutputChar=RoundSigDigits(RegenInletRH*100.0d0,1)
        OutputCharLo=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinRegenAirInRelHum*100.0d0,1)
        OutputCharHi=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxRegenAirInRelHum*100.0d0,1)
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenInRelHumHumRatMess = .TRUE.

        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumHumRatBuffer1 = &
           TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
           TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// '" - Regeneration inlet air relative ' &
           //'humidity related to regen outlet air humidity ratio equation is outside model boundaries at '//TRIM(OutputChar)//'.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumHumRatBuffer2 = &
           '...Model limit on regeneration inlet air relative humidity is '//TRIM(OutputCharLo)//' to '//TRIM(OutputCharHi)//'.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%RegenInRelHumHumRatBuffer3 = &
           '...Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//', ' //TRIM(CreateSysTimeIntervalString())
      ELSE
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintRegenInRelHumHumRatMess = .FALSE.
      END IF

      ! checking if process inlet relative humidity is within model boundaries
      IF(ProcInletRH .LT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinProcAirInRelHum .OR. &
        ProcInletRH .GT. BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxProcAirInRelHum)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumHumRatLast = ProcInletRH*100.0d0
        OutputChar=RoundSigDigits(ProcInletRH*100.0d0,1)
        OutputCharLo=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MinProcAirInRelHum*100.0d0,1)
        OutputCharHi=RoundSigDigits(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%H_MaxProcAirInRelHum*100.0d0,1)
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintProcInRelHumHumRatMess = .TRUE.

        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumHumRatBuffer1 = &
           TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PerfType)//' "'// &
           TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%Name)// '" - Process inlet air relative ' &
           //'humidity related to regen outlet air humidity ratio equation is outside model boundaries at '//TRIM(OutputChar)//'.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumHumRatBuffer2 = &
           '...Model limit on process inlet air relative humidity is '//TRIM(OutputCharLo)//' to '//TRIM(OutputCharHi)//'.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ProcInRelHumHumRatBuffer3 = &
           '...Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//', ' //TRIM(CreateSysTimeIntervalString())
      ELSE
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintProcInRelHumHumRatMess = .FALSE.
      END IF

END SUBROUTINE CheckModelBoundsRH_HumRatEq

SUBROUTINE CheckForBalancedFlow(ExchNum, ProcessInMassFlow, RegenInMassFlow, FirstHVACIteration)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Raustad, FSEC
          !       DATE WRITTEN   June 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !
          ! To verify that the balanced flow desiccant heat exchanger has the same regeneration and process air flow rates.

          ! METHODOLOGY EMPLOYED:
          ! Check that the regeneration and process air mass flow rates are within 2%.
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,         ONLY: CreateSysTimeIntervalString,RoundSigDigits
  USE DataGlobals,     ONLY: CurrentTime
  USE DataHVACGlobals, ONLY: SysTimeElapsed, TimeStepSys

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

  ! SUBROUTINE ARGUMENT DEFINITIONS
  INTEGER, INTENT(IN) :: ExchNum            ! number of the current heat exchanger being simulated
  REAL(r64),    INTENT(IN) :: ProcessInMassFlow  ! current process inlet air mass flow rate (m3/s)
  REAL(r64),    INTENT(IN) :: RegenInMassFlow    ! current regeneration inlet air mass flow rate (m3/s)
  LOGICAL, INTENT(IN) :: FirstHVACIteration ! first HVAC iteration flag

          ! SUBROUTINE PARAMETER DEFINITIONS:
!  CHARACTER(len=*), PARAMETER :: OutputFormat  ='(F10.6)'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
 CHARACTER(len=32)        :: OutputCharProc     = ' '     ! character string for warning messages
 CHARACTER(len=32)        :: OutputCharRegen    = ' '     ! character string for warning messages
 REAL(r64),SAVE    :: TimeStepSysLast    = 0.0d0     ! last system time step (used to check for downshifting)
 REAL(r64)    :: CurrentEndTime     = 0.0d0     ! end time of time step for current simulation time step
 REAL(r64),SAVE    :: CurrentEndTimeLast = 0.0d0     ! end time of time step for last simulation time step
                                                          ! current end time is compared with last to see if time step changed
 REAL(r64)                :: ABSImbalancedFlow            ! absolute value of process and regeneration air flow imbalance fraction
    IF(WarmupFlag .OR. FirstHVACIteration)RETURN

!   calculate end time of current time step
    CurrentEndTime = CurrentTime + SysTimeElapsed

!   Print warning messages only when valid and only for the first ocurrance. Let summary provide statistics.
!   Wait for next time step to print warnings. If simulation iterates, print out
!   the warning for the last iteration only. Must wait for next time step to accomplish this.
!   If a warning occurs and the simulation down shifts, the warning is not valid.
    IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

      ! print error when regeneration inlet relative humidity is outside model boundaries
      IF(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintImbalancedMassFlowMess)THEN
         BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ImbalancedMassFlowErrorCount = &
                        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ImbalancedMassFlowErrorCount + 1
        IF (BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ImbalancedMassFlowErrorCount < 2) THEN
           CALL ShowWarningError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ImbalancedMassFlowBuffer1))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ImbalancedMassFlowBuffer2))
           CALL ShowContinueError(TRIM(BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ImbalancedMassFlowBuffer3))
!           CALL ShowContinueError('...Using regeneration inlet air relative humidities that are outside the regeneration '&
!                 //'outlet humidity ratio equation model boundaries may adversely affect desiccant model performance. '&
!                 //'Verify correct model coefficients.')
        ELSE
          CALL ShowRecurringWarningErrorAtEnd(TRIM(cHXTypes(ExchCond(ExchNum)%ExchTypeNum))//' "'//TRIM(ExchCond(ExchNum)%Name)// &
          '" - unbalanced air flow rate is limited to 2% error continues with the imbalanced fraction statistics reported...', &
                 BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ImbalancedFlowErrIndex, &
                 BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ABSImbalancedFlow, &
                 BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ABSImbalancedFlow)
        END IF
      END IF

    END IF ! IF(CurrentEndTime .GT. CurrentEndTimeLast .AND. TimeStepSys .GE. TimeStepSysLast)THEN

!     save last system time step and last end time of current time step (used to determine if warning is valid)
      TimeStepSysLast    = TimeStepSys
      CurrentEndTimeLast = CurrentEndTime

      ! checking if regeneration inlet relative humidity is within model boundaries
      ABSImbalancedFlow = ABS(RegenInMassFlow - ProcessInMassFlow)/RegenInMassFlow
      IF(ABSImbalancedFlow .GT. 0.02d0)THEN
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ABSImbalancedFlow   = ABSImbalancedFlow
        OutputCharRegen=RoundSigDigits(RegenInMassFlow,6)
        OutputCharProc=RoundSigDigits(ProcessInMassFlow,6)
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintImbalancedMassFlowMess = .TRUE.

        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ImbalancedMassFlowBuffer1 = &
           TRIM(cHXTypes(ExchCond(ExchNum)%ExchTypeNum))//' "'// &
           TRIM(ExchCond(ExchNum)%Name)// '" - unbalanced air flow rate is limited to 2%.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ImbalancedMassFlowBuffer2 = '...Regeneration air ' &
           //'mass flow rate is '//TRIM(OutputCharRegen)//' and process air mass flow rate is '//TRIM(OutputCharProc)//'.'
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%ImbalancedMassFlowBuffer3 = &
           '...Occurrence info = '//TRIM(EnvironmentName)//', '//Trim(CurMnDy)//', ' //TRIM(CreateSysTimeIntervalString())
      ELSE
        BalDesDehumPerfData(ExchCond(ExchNum)%PerfDataIndex)%PrintImbalancedMassFlowMess = .FALSE.
      END IF

END SUBROUTINE CheckForBalancedFlow

FUNCTION GetSupplyInletNode(HXName,ErrorsFound)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   February 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given HX and returns the supply air inlet node number.
          ! If incorrect HX name is given, errorsfound is returned as true and node number as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=MaxNameLength), INTENT(IN) :: HXName             ! must match HX names for the ExchCond type
  LOGICAL, INTENT(INOUT)                   :: ErrorsFound        ! set to true if problem
  INTEGER                                  :: GetSupplyInletNode ! node number returned

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichHX

  ! Obtains and Allocates heat exchanger related parameters from input file
  IF (GetInputFlag) THEN  !First time subroutine has been entered
    CALL GetHeatRecoveryInput
    GetInputFlag=.FALSE.
  End If

  WhichHX=FindItemInList(HXName,ExchCond%Name,NumHeatExchangers)
  IF (WhichHX /= 0) THEN
    GetSupplyInletNode = ExchCond(WhichHX)%SupInletNode
  ELSE
    CALL ShowSevereError('GetSupplyInletNode: Could not find heat exchanger = "'//TRIM(HXName)//'"')
    ErrorsFound=.TRUE.
    GetSupplyInletNode = 0
  ENDIF

  RETURN

END FUNCTION GetSupplyInletNode

FUNCTION GetSupplyOutletNode(HXName,ErrorsFound)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   February 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given HX and returns the supply air outlet node number.
          ! If incorrect HX name is given, errorsfound is returned as true and node number as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=MaxNameLength), INTENT(IN) :: HXName              ! must match HX names for the ExchCond type
  LOGICAL, INTENT(INOUT)                   :: ErrorsFound         ! set to true if problem
  INTEGER                                  :: GetSupplyOutletNode ! node number returned

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichHX

  ! Obtains and Allocates heat exchanger related parameters from input file
  IF (GetInputFlag) THEN  !First time subroutine has been entered
    CALL GetHeatRecoveryInput
    GetInputFlag=.FALSE.
  End If

  WhichHX=FindItemInList(HXName,ExchCond%Name,NumHeatExchangers)
  IF (WhichHX /= 0) THEN
    GetSupplyOutletNode = ExchCond(WhichHX)%SupOutletNode
  ELSE
    CALL ShowSevereError('GetSupplyOutletNode: Could not find heat exchanger = "'//TRIM(HXName)//'"')
    ErrorsFound=.TRUE.
    GetSupplyOutletNode = 0
  ENDIF

  RETURN

END FUNCTION GetSupplyOutletNode

FUNCTION GetSecondaryInletNode(HXName,ErrorsFound)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   February 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given HX and returns the secondary air inlet node number.
          ! If incorrect HX name is given, errorsfound is returned as true and node number as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=MaxNameLength), INTENT(IN) :: HXName                ! must match HX names for the ExchCond type
  LOGICAL, INTENT(INOUT)                   :: ErrorsFound           ! set to true if problem
  INTEGER                                  :: GetSecondaryInletNode ! node number returned

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichHX

  ! Obtains and Allocates heat exchanger related parameters from input file
  IF (GetInputFlag) THEN  !First time subroutine has been entered
    CALL GetHeatRecoveryInput
    GetInputFlag=.FALSE.
  End If

  WhichHX=FindItemInList(HXName,ExchCond%Name,NumHeatExchangers)
  IF (WhichHX /= 0) THEN
    GetSecondaryInletNode = ExchCond(WhichHX)%SecInletNode
  ELSE
    CALL ShowSevereError('GetSecondaryInletNode: Could not find heat exchanger = "'//TRIM(HXName)//'"')
    ErrorsFound=.TRUE.
    GetSecondaryInletNode = 0
  ENDIF

  RETURN

END FUNCTION GetSecondaryInletNode

FUNCTION GetSecondaryOutletNode(HXName,ErrorsFound)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   February 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given HX assisted cooling coil and returns the secondary air outlet node number.
          ! If incorrect HX name is given, errorsfound is returned as true and node number as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=MaxNameLength), INTENT(IN) :: HXName                 ! must match HX names for the ExchCond type
  LOGICAL, INTENT(INOUT)                   :: ErrorsFound            ! set to true if problem
  INTEGER                                  :: GetSecondaryOutletNode ! node number returned

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichHX

  ! Obtains and Allocates heat exchanger related parameters from input file
  IF (GetInputFlag) THEN  !First time subroutine has been entered
    CALL GetHeatRecoveryInput
    GetInputFlag=.FALSE.
  End If

  WhichHX=FindItemInList(HXName,ExchCond%Name,NumHeatExchangers)
  IF (WhichHX /= 0) THEN
    GetSecondaryOutletNode = ExchCond(WhichHX)%SecOutletNode
  ELSE
    CALL ShowSevereError('GetSecondaryOutletNode: Could not find heat exchanger = "'//TRIM(HXName)//'"')
    ErrorsFound=.TRUE.
    GetSecondaryOutletNode = 0
  ENDIF

  RETURN

END FUNCTION GetSecondaryOutletNode

FUNCTION GetSupplyAirFlowRate(HXName,ErrorsFound)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   October 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given Generic HX and the voluetric air flow rate.
          ! If incorrect HX name is given, errorsfound is returned as true and air flow rate as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=MaxNameLength), INTENT(IN) :: HXName                 ! must match HX names for the ExchCond type
  LOGICAL, INTENT(INOUT)                   :: ErrorsFound            ! set to true if problem
  REAL(r64)                                :: GetSupplyAirFlowRate  ! air flow rate returned

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichHX

  ! Obtains and Allocates heat exchanger related parameters from input file
  IF (GetInputFlag) THEN  !First time subroutine has been entered
    CALL GetHeatRecoveryInput
    GetInputFlag=.FALSE.
  End If

  WhichHX=FindItemInList(HXName,ExchCond%Name,NumHeatExchangers)
  IF (WhichHX /= 0) THEN
    GetSupplyAirFlowRate = ExchCond(WhichHX)%NomSupAirVolFlow
  ELSE
    CALL ShowSevereError('GetSupplyAirFlowRate: Could not find heat exchanger = "'//TRIM(HXName)//'"')
    CALL ShowContinueError('... Supply Air Flow Rate returned as 0.')
    ErrorsFound=.TRUE.
    GetSupplyAirFlowRate = 0.0d0
  ENDIF

  RETURN

END FUNCTION GetSupplyAirFlowRate

FUNCTION GetHeatExchangerObjectTypeNum(HXName,ErrorsFound)

          ! FUNCTION INFORMATION:
          !       AUTHOR         Richard Raustad
          !       DATE WRITTEN   October 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS FUNCTION:
          ! This function looks up the given Generic HX and the voluetric air flow rate.
          ! If incorrect HX name is given, errorsfound is returned as true and air flow rate as zero.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor,  ONLY: FindItemInList

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  CHARACTER(len=MaxNameLength), INTENT(IN) :: HXName                 ! must match HX names for the ExchCond type
  LOGICAL, INTENT(INOUT)                   :: ErrorsFound            ! set to true if problem
  INTEGER                                  :: GetHeatExchangerObjectTypeNum  ! object type parameter returned

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichHX

  ! Obtains and Allocates heat exchanger related parameters from input file
  IF (GetInputFlag) THEN  !First time subroutine has been entered
    CALL GetHeatRecoveryInput
    GetInputFlag=.FALSE.
  End If

  WhichHX=FindItemInList(HXName,ExchCond%Name,NumHeatExchangers)
  IF (WhichHX /= 0) THEN
    GetHeatExchangerObjectTypeNum = ExchCond(WhichHX)%ExchTypeNum
  ELSE
    CALL ShowSevereError('GetHeatExchangerObjectTypeNum: Could not find heat exchanger = "'//TRIM(HXName)//'"')
    ErrorsFound=.TRUE.
    GetHeatExchangerObjectTypeNum = 0
  ENDIF

  RETURN

END FUNCTION GetHeatExchangerObjectTypeNum

SUBROUTINE SetHeatExchangerData(HXNum, ErrorsFound, HXName, SupplyAirVolFlow, SecondaryAirVolFlow)

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
  INTEGER, INTENT(IN)         :: HXNum               ! Index of HX
  LOGICAL, INTENT(INOUT)      :: ErrorsFound         ! Set to true if certain errors found
  CHARACTER(len=*),INTENT(IN) :: HXName              ! Name of HX
  REAL(r64), OPTIONAL              :: SupplyAirVolFlow    ! HX supply air flow rate    [m3/s]
  REAL(r64), OPTIONAL              :: SecondaryAirVolFlow ! HX secondary air flow rate [m3/s]

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WhichHX   ! index to generic HX

  ! Obtains and Allocates heat exchanger related parameters from input file
  IF (GetInputFlag) THEN  !First time subroutine has been entered
    CALL GetHeatRecoveryInput
    GetInputFlag=.FALSE.
  End If

  IF (HXNum .EQ. 0)THEN
    WhichHX=FindItemInList(HXName,ExchCond%Name,NumHeatExchangers)
  ELSE
    WhichHX=HXNum
  END IF

  IF (WhichHX <= 0 .OR. WhichHX .GT. NumHeatExchangers) THEN
    CALL ShowSevereError('SetHeatExchangerData: Could not find heat exchanger = "'//TRIM(HXName)//'"')
    ErrorsFound=.TRUE.
    RETURN
  ENDIF

  IF(PRESENT(SupplyAirVolFlow))THEN
    ExchCond(WhichHX)%NomSupAirVolFlow = SupplyAirVolFlow
  END IF

  IF(PRESENT(SecondaryAirVolFlow))THEN
    ExchCond(WhichHX)%NomSecAirVolFlow = SecondaryAirVolFlow
  END IF

  RETURN

END SUBROUTINE SetHeatExchangerData


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

END MODULE HeatRecovery
