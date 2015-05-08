MODULE DataHeatBalFanSys      ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Richard J. Liesen
          !       DATE WRITTEN   February 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module should contains the information that is needed to pass from the
          ! Heat Balance Module to the Fan Systems

          ! USE STATEMENTS:
USE DataPrecisionGlobals

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.


          ! MODULE PARAMETER DEFINITIONS:
INTEGER,   PARAMETER :: UseSimpleAirFlow = 1
REAL(r64), PARAMETER :: MaxRadHeatFlux   = 4000.0D0 ! [W/m2] max limit for radiant heat flux at a surface due to HVAC equipment

! Controls for PredictorCorrector
INTEGER, PARAMETER :: iGetZoneSetpoints             = 1
INTEGER, PARAMETER :: iPredictStep                  = 2
INTEGER, PARAMETER :: iCorrectStep                  = 3
INTEGER, PARAMETER :: iRevertZoneTimestepHistories  = 4
INTEGER, PARAMETER :: iPushZoneTimestepHistories    = 5
INTEGER, PARAMETER :: iPushSystemTimestepHistories  = 6

          ! DERIVED TYPE DEFINITIONS:

TYPE :: ZoneComfortControlsFangerData
  INTEGER :: FangerType                                =0   ! Index for Fanger type
  REAL(r64)    :: LowPMV                               =0.0d0 ! Low PMV value
  REAL(r64)    :: HighPMV                              =0.0d0 ! High PMV Value
  INTEGER :: DualPMVErrCount                           =0   ! Dual PMV setpoint error count
  INTEGER :: DualPMVErrIndex                           =0   ! Dual PMV setpoint error index
END TYPE ZoneComfortControlsFangerData


          ! MODULE VARIABLE DECLARATIONS:
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: SumConvHTRadSys ! Sum of convection to zone air from hi temp radiant heaters
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: SumLatentHTRadSys ! Sum of latent gains from hi temp radiant heaters
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QHTRadSysToPerson ! Sum of radiant gains to people from hi temp radiant heaters
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QHWBaseboardToPerson ! Sum of radiant gains to people from hot water baseboard heaters
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QSteamBaseboardToPerson ! Sum of radiant gains to people from steam baseboard heaters
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: QElecBaseboardToPerson ! Sum of radiant gains to people from electric baseboard heaters
!Zone air drybulb conditions variables
REAL(r64), Allocatable, Dimension(:)   :: ZTAV      !Zone Air Temperature Averaged over the Zone Time step
REAL(r64), Allocatable, Dimension(:)   :: MAT       !MEAN AIR TEMPARATURE (C)
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: TempTstatAir      ! temperature of air near the thermo stat
REAL(r64), Allocatable, Dimension(:)   :: ZT              !Zone Air Temperature Averaged over the System Time Increment
REAL(r64), Allocatable, Dimension(:)   :: XMAT            !TEMPORARY ZONE TEMPERATURE TO TEST CONVERGENCE
REAL(r64), Allocatable, Dimension(:)   :: XM2T            !
REAL(r64), Allocatable, Dimension(:)   :: XM3T            !
REAL(r64), Allocatable, Dimension(:)   :: XM4T            !
Real(r64), Allocatable, Dimension(:)   :: DSXMAT          ! Down Stepped MAT history storage
Real(r64), Allocatable, Dimension(:)   :: DSXM2T          ! Down Stepped MAT history storage
Real(r64), Allocatable, Dimension(:)   :: DSXM3T          ! Down Stepped MAT history storage
Real(r64), Allocatable, Dimension(:)   :: DSXM4T          ! Down Stepped MAT history storage
REAL(r64), Allocatable, Dimension(:)   :: XMPT            ! Zone air temperature at previous time step

REAL(r64), Allocatable, Dimension(:)   :: ZTAVComf   ! Zone Air Temperature Averaged over the Zone Time step used
                                                     ! in thermal comfort models (currently Fang model only)
REAL(r64), Allocatable, Dimension(:)   :: ZoneAirHumRatAvgComf  !AIR Humidity Ratio averaged over the zone time
                                                     ! step used in thermal comfort models (currently Fang model only)

! Zone Air moisture conditions variables
REAL(r64), Allocatable, Dimension(:)   :: ZoneAirHumRatAvg   !AIR Humidity Ratio averaged over the zone time step
REAL(r64), Allocatable, Dimension(:)   :: ZoneAirHumRat   !AIR Humidity Ratio
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: WZoneTimeMinus1     ! Humidity ratio history terms for 3rd order derivative
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: WZoneTimeMinus2     ! Time Minus 2 Zone Time Steps Term
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: WZoneTimeMinus3     ! Time Minus 3 Zone Time Steps Term
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: WZoneTimeMinus4     ! Time Minus 4 Zone Time Steps Term
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: DSWZoneTimeMinus1   ! DownStepped Humidity ratio history terms for 3rd order derivative
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: DSWZoneTimeMinus2   ! DownStepped Time Minus 2 Zone Time Steps Term
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: DSWZoneTimeMinus3   ! DownStepped Time Minus 3 Zone Time Steps Term
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: DSWZoneTimeMinus4   ! DownStepped Time Minus 4 Zone Time Steps Term
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: WZoneTimeMinusP     ! Humidity ratio history terms at previous time step

REAL(r64), ALLOCATABLE, DIMENSION(:)   :: ZoneAirHumRatTemp   ! Temp zone air humidity ratio at time plus 1
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: WZoneTimeMinus1Temp ! Zone air humidity ratio at previous timestep
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: WZoneTimeMinus2Temp ! Zone air humidity ratio at timestep T-2
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: WZoneTimeMinus3Temp ! Zone air humidity ratio at timestep T-3
REAL(r64), Allocatable, Dimension(:)   :: ZoneAirHumRatOld ! Last Time Steps Zone AIR Humidity Ratio


REAL(r64), Allocatable, Dimension(:)   :: MCPI            !INFILTRATION MASS FLOW * AIR SPECIFIC HEAT
REAL(r64), Allocatable, Dimension(:)   :: MCPTI           !INFILTRATION MASS FLOW * AIR CP * AIR TEMPERATURE
REAL(r64), Allocatable, Dimension(:)   :: MCPV            !VENTILATION MASS FLOW * AIR SPECIFIC HEAT
REAL(r64), Allocatable, Dimension(:)   :: MCPTV           !VENTILATION MASS FLOW * AIR CP * AIR TEMPERATURE
REAL(r64), Allocatable, Dimension(:)   :: MCPM            !Mixing MASS FLOW * AIR SPECIFIC HEAT
REAL(r64), Allocatable, Dimension(:)   :: MCPTM           !Mixing MASS FLOW * AIR CP * AIR TEMPERATURE
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: MCPE            ! EARTHTUBE MASS FLOW * AIR SPECIFIC HEAT
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: EAMFL           ! OUTDOOR AIR MASS FLOW for EarthTube
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: MCPTE           ! EARTHTUBE MASS FLOW * AIR CP * AIR TEMPERATURE
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: MCPC            ! COOLTOWER MASS FLOW * AIR SPECIFIC HEAT
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: CTMFL           ! OUTDOOR AIR MASS FLOW for cooltower
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: MCPTC           ! COOLTOWER MASS FLOW * AIR CP * AIR TEMPERATURE
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: ThermChimAMFL   ! OUTDOOR AIR MASS FLOW for THERMALCHIMNEY
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: MCPTThermChim   ! THERMALCHIMNEY MASS FLOW * AIR SPECIFIC HEAT
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: MCPThermChim    ! THERMALCHIMNEY MASS FLOW * AIR CP * AIR TEMPERATURE
REAL(r64), Allocatable, Dimension(:)   :: ZoneLatentGain    !Latent Energy from each Zone (People, equipment)
REAL(r64), Allocatable, Dimension(:)   :: OAMFL           !OUTDOOR AIR MASS FLOW (M**3/SEC) for infiltration
REAL(r64), Allocatable, Dimension(:)   :: VAMFL           !OUTDOOR AIR MASS FLOW (M**3/SEC) for ventilation
REAL(r64), Allocatable, Dimension(:)   :: NonAirSystemResponse ! Convective heat addition rate from non forced air
                                                          ! equipment such as baseboards plus heat from lights to
REAL(r64), Allocatable, Dimension(:) :: SysDepZoneLoads   ! Convective heat addition or subtraction rate from sources that
                                                          ! depend on what is happening with the HVAC system. Such as:
                                                          ! heat gain from lights to return air when return flow = 0; heat gain
                                                          ! from air flow windows to return air when return air flow = 0;
                                                          ! and heat removed by return air from refrigeration cases when
                                                          ! return air flow = 0.
REAL(r64), Allocatable, Dimension(:) :: SysDepZoneLoadsLagged  ! SysDepZoneLoads saved to be added to zone heat balance next
                                                               ! HVAC time step
REAL(r64), Allocatable, Dimension(:)   :: MDotCPOA        !Airbalance MASS FLOW * AIR SPECIFIC HEAT
REAL(r64), Allocatable, Dimension(:)   :: MDotOA          !Airbalance MASS FLOW rate

REAL(r64), Allocatable, Dimension(:)   :: MixingMassFlowZone    !Mixing MASS FLOW
REAL(r64), Allocatable, Dimension(:)   :: MixingMassFlowXHumRat !Mixing MASS FLOW * Humidity Ratio

          !REAL Variables for the Heat Balance Simulation


REAL(r64), ALLOCATABLE, DIMENSION(:) :: QRadSysSource       ! Current source/sink for a particular surface (radiant sys)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: TCondFDSourceNode   ! Temperature of sourc/sink location in surface from CondFD algo
REAL(r64), ALLOCATABLE, DIMENSION(:) :: QPVSysSource        ! Current source/sink for a surface (integrated PV sys)

REAL(r64), ALLOCATABLE, DIMENSION(:) :: CTFTsrcConstPart    !Constant Outside Portion of the CTF calculation of
                                                                   ! temperature at source
REAL(r64), ALLOCATABLE, DIMENSION(:) :: QHTRadSysSurf       ! Current radiant heat flux at a surface due to the presence
                                                            ! of high temperature radiant heaters
REAL(r64), ALLOCATABLE, DIMENSION(:) :: QHWBaseboardSurf    ! Current radiant heat flux at a surface due to the presence
                                                            ! of hot water baseboard heaters
REAL(r64), ALLOCATABLE, DIMENSION(:) :: QSteamBaseboardSurf ! Current radiant heat flux at a surface due to the presence
                                                            ! of steam baseboard heaters
REAL(r64), ALLOCATABLE, DIMENSION(:) :: QElecBaseboardSurf ! Current radiant heat flux at a surface due to the presence
                                                            ! of electric baseboard heaters
REAL(r64), ALLOCATABLE, DIMENSION(:) :: RadSysTiHBConstCoef !Inside heat balance coefficient that is constant
REAL(r64), ALLOCATABLE, DIMENSION(:) :: RadSysTiHBToutCoef  !Inside heat balance coefficient that modifies Toutside
REAL(r64), ALLOCATABLE, DIMENSION(:) :: RadSysTiHBQsrcCoef  !Inside heat balance coefficient that modifies source/sink
REAL(r64), ALLOCATABLE, DIMENSION(:) :: RadSysToHBConstCoef !Outside heat balance coefficient that is constant
REAL(r64), ALLOCATABLE, DIMENSION(:) :: RadSysToHBTinCoef   !Outside heat balance coefficient that modifies Toutside
REAL(r64), ALLOCATABLE, DIMENSION(:) :: RadSysToHBQsrcCoef  !Outside heat balance coefficient that modifies source/sink


!Moisture variables to carry info from HB to the Zone Temp Predictor-Corrector for Fan System
REAL(r64), Allocatable, Dimension(:)   :: SumHmAW   !SUM OF ZONE AREA*Moist CONVECTION COEFF*INSIDE Humidity Ratio
REAL(r64), Allocatable, Dimension(:)   :: SumHmARa  !SUM OF ZONE AREA*Moist CONVECTION COEFF*Rho Air
REAL(r64), Allocatable, Dimension(:)   :: SumHmARaW !SUM OF ZONE AREA*Moist CONVECTION COEFF*Rho Air* Inside Humidity Ration

REAL(r64), ALLOCATABLE, DIMENSION(:) :: TempZoneThermostatSetpoint
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneThermostatSetPointHi
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneThermostatSetPointLo

REAL(r64), ALLOCATABLE, DIMENSION(:) :: LoadCorrectionFactor  !PH 3/3/04

LOGICAL, ALLOCATABLE, DIMENSION(:) :: CrossMixingFlag ! TRUE when a zone is mixing

REAL(r64),DIMENSION(:),ALLOCATABLE  :: AIRRAT     !"air power capacity"  PH 3/5/04
REAL(r64),DIMENSION(:),ALLOCATABLE  :: ZTM1       !zone air temperature at previous timestep
REAL(r64),DIMENSION(:),ALLOCATABLE  :: ZTM2       !zone air temperature at timestep T-2
REAL(r64),DIMENSION(:),ALLOCATABLE  :: ZTM3       !zone air temperature at previous T-3
! Exact and Euler solutions
REAL(r64),DIMENSION(:),ALLOCATABLE  :: ZoneTMX    ! TEMPORARY ZONE TEMPERATURE TO TEST CONVERGENCE in Exact and Euler method
REAL(r64),DIMENSION(:),ALLOCATABLE  :: ZoneTM2    ! TEMPORARY ZONE TEMPERATURE at timestep t-2 in Exact and Euler method
REAL(r64),DIMENSION(:),ALLOCATABLE  :: ZoneT1     ! Zone temperature at the previous time step used in Exact and Euler method
REAL(r64),DIMENSION(:),ALLOCATABLE  :: ZoneWMX    ! TEMPORARY ZONE TEMPERATURE TO TEST CONVERGENCE in Exact and Euler method
REAL(r64),DIMENSION(:),ALLOCATABLE  :: ZoneWM2    ! TEMPORARY ZONE TEMPERATURE at timestep t-2 in Exact and Euler method
REAL(r64),DIMENSION(:),ALLOCATABLE  :: ZoneW1     ! Zone temperature at the previous time step used in Exact and Euler method

REAL(r64) :: ZoneVolCapMultpSens  ! This is a multiplier used on the zone volume to make the capacitance more realistic
                              ! for the calculation of the zone temp in the predictor and corrector step
REAL(r64) :: ZoneVolCapMultpMoist  ! This is a multiplier used on the zone volume to make the capacitance more realistic
                              ! for the calculation of the zone humidity ratio in the predictor and corrector step
REAL(r64) :: ZoneVolCapMultpCO2  ! This is a multiplier used on the zone volume to make the capacitance more realistic
                              ! for the calculation of the zone CO2 concentration in the predictor and corrector step
REAL(r64) :: ZoneVolCapMultpGenContam  ! This is a multiplier used on the zone volume to make the capacitance more realistic
                              ! for the calculation of the zone generic contaminant concentration in the predictor
                              ! and corrector step

INTEGER, ALLOCATABLE, DIMENSION(:) :: TempControlType
INTEGER, ALLOCATABLE, DIMENSION(:) :: ComfortControlType

TYPE (ZoneComfortControlsFangerData), ALLOCATABLE, DIMENSION(:) :: ZoneComfortControlsFanger

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

END MODULE DataHeatBalFanSys
