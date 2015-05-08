MODULE DataContaminantBalance      ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   May 2010
          !       MODIFIED       Added generic contaminant in Jan. 2012 by L. Gu
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module is revised from humidity ratio data straucture and contains the information
          ! that is needed to pass from the Contaminant Balance Module.

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals,  ONLY: MaxNameLength,AutoCalculate,DegToRadians
USE DataSurfaces, ONLY: MaxSlatAngs

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC    ! By definition, all variables which are placed in this data-only
          ! module should be available to other modules and routines.  Thus,
          ! all variables in this module must be PUBLIC.

          ! MODULE PARAMETER DEFINITIONS:

TYPE ContaminantData
  LOGICAL :: SimulateContaminants        = .false.   ! A logical flag to determine whether any contaminants are simulated or not
  LOGICAL :: CO2Simulation               = .false.   ! CO2 simulation flag
  INTEGER :: CO2OutdoorSchedPtr          = 0         ! CO2 outdoor level schedule pointer
  LOGICAL :: GenericContamSimulation     = .false.   ! Generic contaminant simulation flag
  INTEGER :: GenericContamOutdoorSchedPtr = 0        ! Generic contaminant outdoor level schedule pointer
END TYPE

TYPE :: ZoneContControls
  CHARACTER(len=MaxNameLength) :: Name                 =' ' ! Name of the contaminant controller
  CHARACTER(len=MaxNameLength) :: ZoneName             =' ' ! Name of the zone
  INTEGER                      :: ActualZoneNum        =0
  CHARACTER(len=MaxNameLength) :: AvaiSchedule         =' ' ! Availability Schedule name
  INTEGER  :: AvaiSchedPtr                             =0   ! Pointer to the correct schedule
  CHARACTER(len=MaxNameLength) :: SetPointSchedName =' '    ! Name of the schedule which determines the CO2 setpoint
  INTEGER :: SPSchedIndex                              =0   ! Index for this schedule
  LOGICAL   :: EMSOverrideCO2SetpointOn          = .FALSE.  ! EMS is calling to override CO2 setpoint
  REAL(r64) :: EMSOverrideCO2SetpointValue       = 0.0D0    ! value EMS is directing to use for CO2 setpoint
  INTEGER :: NumOfZones                                =0   ! Number of controlled zones in the same airloop
  INTEGER,DIMENSION(:),ALLOCATABLE  :: ControlZoneNum       ! Controlled zone number
  CHARACTER(len=MaxNameLength) :: ZoneMinCO2SchedName =' '    ! Name of the schedule which determines minimum CO2 concentration
  INTEGER :: ZoneMinCO2SchedIndex                              =0   ! Index for this schedule
  INTEGER :: ZoneContamControllerSchedIndex                    =0   ! Index for this schedule
  CHARACTER(len=MaxNameLength) :: GCAvaiSchedule       =' ' ! Availability Schedule name for generic contamiant
  INTEGER  :: GCAvaiSchedPtr                           =0   ! Pointer to the correct generic contaminant availability schedule
  CHARACTER(len=MaxNameLength) :: GCSetPointSchedName =' '  ! Name of the schedule which determines the generic contaminant setpoint
  INTEGER :: GCSPSchedIndex                            =0   ! Index for this schedule
  LOGICAL   :: EMSOverrideGCSetpointOn           = .FALSE.  ! EMS is calling to override generic contaminant setpoint
  REAL(r64) :: EMSOverrideGCSetpointValue        = 0.0D0    ! value EMS is directing to use for generic contaminant setpoint
END TYPE ZoneContControls

TYPE ZoneSystemContaminantDemandData                   ! Contaminent loads to be met (kg air per second)
  REAL(r64) :: OutputRequiredToCO2SP              =0.0d0 ! Load required to meet CO2 setpoint
  REAL(r64) :: RemainingOutputReqToCO2SP          =0.0d0 ! Remaining load required to meet CO2 setpoint
  REAL(r64) :: OutputRequiredToGCSP              =0.0d0 ! Load required to meet generic contaminant setpoint
  REAL(r64) :: RemainingOutputReqToGCSP          =0.0d0 ! Remaining load required to meet generic contaminant setpoint
END TYPE ZoneSystemContaminantDemandData

TYPE ZoneContamGenericDataConstant
  CHARACTER(len=MaxNameLength) :: Name                 =' ' ! Name of the constant generic contaminant source and sink
  CHARACTER(len=MaxNameLength) :: ZoneName             =' ' ! Name of the zone
  INTEGER                      :: ActualZoneNum        =0   ! Zone number
  REAL(r64) :: GCGenerateRate                          =0.0d0 ! Generic contaminant design generation rate [m3/s]
  INTEGER :: GCGenerateRateSchedPtr                    = 0  ! Generic contaminant design generation rate schedule pointer
  REAL(r64) :: GCRemovalCoef                           =0.0d0 ! Generic contaminant design removal coefficient [m3/s]
  INTEGER :: GCRemovalCoefSchedPtr                     = 0  ! Generic contaminant design removal coefficient schedule pointer
  REAL(r64) :: GCGenRate                               =0.0d0 ! Generic contaminant design generation rate [m3/s] for reporting
END TYPE

TYPE ZoneContamGenericDataPDriven
  CHARACTER(len=MaxNameLength) :: Name                 =' ' ! Name of the pressure driven generic contaminant source and sink
  CHARACTER(len=MaxNameLength) :: SurfName             =' ' ! Name of the surface
  INTEGER                      :: SurfNum              =0   ! Surface number
  REAL(r64) :: GCGenRateCoef                           =0.0d0 ! Generic contaminant design generation rate coefficeint [m3/s]
  INTEGER :: GCGenRateCoefSchedPtr                     = 0  ! Generic contaminant design generation rate schedule pointer
  REAL(r64) :: GCExpo                                  =0.0d0 ! Generic contaminant exponent []
  REAL(r64) :: GCGenRate                               =0.0d0 ! Generic contaminant design generation rate [m3/s] for reporting
END TYPE

TYPE ZoneContamGenericDataCutoff
  CHARACTER(len=MaxNameLength) :: Name                 =' ' ! Name of the cutoff generic contaminant source and sink
  CHARACTER(len=MaxNameLength) :: ZoneName             =' ' ! Name of the zone
  INTEGER                      :: ActualZoneNum        =0   ! Zone number
  REAL(r64) :: GCGenerateRate                          =0.0d0 ! Generic contaminant design generation rate [m3/s]
  INTEGER :: GCGenerateRateSchedPtr                    = 0  ! Generic contaminant design generation rate schedule pointer
  REAL(r64) :: GCCutoffValue                           =0.0d0 ! Cutoff value [ppm]
  REAL(r64) :: GCGenRate                               =0.0d0 ! Generic contaminant design generation rate [m3/s] for reporting
END TYPE

TYPE ZoneContamGenericDataDecay
  CHARACTER(len=MaxNameLength) :: Name                 =' ' ! Name of the decay generic contaminant source and sink
  CHARACTER(len=MaxNameLength) :: ZoneName             =' ' ! Name of the zone
  INTEGER                      :: ActualZoneNum        =0   ! Zone number
  REAL(r64) :: GCInitEmiRate                           =0.0d0 ! Generic contaminant design generation rate [m3/s]
  INTEGER :: GCEmiRateSchedPtr                         = 0  ! Generic contaminant emission rate schedule pointer
  REAL(r64) :: GCTime                                  =0.0d0 ! Time since the styart of emission [s]
  REAL(r64) :: GCDelayTime                             =0.0d0 ! Delay time constant [s]
  REAL(r64) :: GCGenRate                               =0.0d0 ! Generic contaminant design generation rate [m3/s] for reporting
END TYPE

TYPE ZoneContamGenericDataBLDiff
  CHARACTER(len=MaxNameLength) :: Name                 =' ' ! Name of the boundary layer diffusion generic contaminant source
                                                            ! and sink
  CHARACTER(len=MaxNameLength) :: SurfName             =' ' ! Name of the surface
  INTEGER                      :: SurfNum              =0   ! Surface number
  REAL(r64) :: GCTranCoef                              =0.0d0 ! Generic contaminant mass transfer coefficeint [m/s]
  INTEGER :: GCTranCoefSchedPtr                        = 0  ! Generic contaminant mass transfer coefficeint schedule pointer
  REAL(r64) :: GCHenryCoef                             =0.0d0 ! Generic contaminant Henry adsorption constant or
                                                            ! partition coefficient []
  REAL(r64) :: GCGenRate                               =0.0d0 ! Generic contaminant design generation rate [m3/s] for reporting
END TYPE

TYPE ZoneContamGenericDataDVS
  CHARACTER(len=MaxNameLength) :: Name                 =' ' ! Name of the deposition velocity generic contaminant sink
  CHARACTER(len=MaxNameLength) :: SurfName             =' ' ! Name of the surface
  INTEGER                      :: SurfNum              =0   ! Surface number
  REAL(r64) :: GCDepoVelo                              =0.0d0 ! Generic contaminant deposition velocity [m/s]
  INTEGER :: GCDepoVeloPtr                             = 0  ! Generic contaminant deposition velocity sink schedule pointer
  REAL(r64) :: GCGenRate                               =0.0d0 ! Generic contaminant design generation rate [m3/s] for reporting
END TYPE

TYPE ZoneContamGenericDataDRS
  CHARACTER(len=MaxNameLength) :: Name                 =' ' ! Name of the deposition rate generic contaminant sink
  CHARACTER(len=MaxNameLength) :: ZoneName             =' ' ! Name of the zone
  INTEGER                      :: ActualZoneNum        =0   ! Zone number
  REAL(r64) :: GCDepoRate                              =0.0d0 ! Generic contaminant deposition rate [m/s]
  INTEGER :: GCDepoRatePtr                             = 0  ! Generic contaminant deposition rate sink schedule pointer
  REAL(r64) :: GCGenRate                               =0.0d0 ! Generic contaminant design generation rate [m3/s] for reporting
END TYPE

TYPE (ZoneSystemContaminantDemandData), ALLOCATABLE, DIMENSION(:)     :: ZoneSysContDemand


! MODULE VARIABLE Type DECLARATIONS:
TYPE (ContaminantData), SAVE :: Contaminant=ContaminantData  &
   (.false.,  &   ! A logical flag to determine whether any contaminants are simulated or not
    .false.,  &   ! CO2 simulation flag
    0,  &         ! CO2 outdoor level schedule pointer
    .false.,  &   ! Generic contaminant simulation flag
    0)            ! Generic contaminant outdoor level schedule pointer

TYPE (ZoneContControls), ALLOCATABLE, DIMENSION(:)     :: ContaminantControlledZone

TYPE (ZoneContamGenericDataConstant), ALLOCATABLE, DIMENSION(:)     :: ZoneContamGenericConstant
TYPE (ZoneContamGenericDataPDriven), ALLOCATABLE, DIMENSION(:)      :: ZoneContamGenericPDriven
TYPE (ZoneContamGenericDataCutoff), ALLOCATABLE, DIMENSION(:)       :: ZoneContamGenericCutoff
TYPE (ZoneContamGenericDataDecay), ALLOCATABLE, DIMENSION(:)        :: ZoneContamGenericDecay
TYPE (ZoneContamGenericDataBLDiff), ALLOCATABLE, DIMENSION(:)       :: ZoneContamGenericBLDiff
TYPE (ZoneContamGenericDataDVS), ALLOCATABLE, DIMENSION(:)          :: ZoneContamGenericDVS
TYPE (ZoneContamGenericDataDRS), ALLOCATABLE, DIMENSION(:)          :: ZoneContamGenericDRS

REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneCO2Setpoint
REAL(r64), ALLOCATABLE, DIMENSION(:) :: CO2PredictedRate

REAL(r64), Allocatable, Dimension(:)   :: ZoneCO2Gain           !CO2 gain from each Zone (People, equipment)
REAL(r64), Allocatable, Dimension(:)   :: ZoneCO2GainFromPeople !CO2 gain from each Zone (From People only)

! Zone Air Contaminant conditions variables
REAL(r64), Allocatable, Dimension(:)   :: ZoneAirCO2Avg         ! AIR CO2 averaged over the zone time step
REAL(r64), Allocatable, Dimension(:)   :: ZoneAirCO2            ! AIR CO2
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: CO2ZoneTimeMinus1     ! CO2 history terms for 3rd order derivative
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: CO2ZoneTimeMinus2     ! Time Minus 2 Zone Time Steps Term
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: CO2ZoneTimeMinus3     ! Time Minus 3 Zone Time Steps Term
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: CO2ZoneTimeMinus4     ! Time Minus 4 Zone Time Steps Term
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: DSCO2ZoneTimeMinus1   ! DownStepped CO2 history terms for 3rd order derivative
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: DSCO2ZoneTimeMinus2   ! DownStepped Time Minus 2 Zone Time Steps Term
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: DSCO2ZoneTimeMinus3   ! DownStepped Time Minus 3 Zone Time Steps Term
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: DSCO2ZoneTimeMinus4   ! DownStepped Time Minus 4 Zone Time Steps Term

REAL(r64), ALLOCATABLE, DIMENSION(:)   :: ZoneAirCO2Temp   ! Temp zone air CO2 at time plus 1
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: CO2ZoneTimeMinus1Temp ! Zone air CO2 at previous timestep
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: CO2ZoneTimeMinus2Temp ! Zone air CO2 at timestep T-2
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: CO2ZoneTimeMinus3Temp ! Zone air CO2 at timestep T-3
REAL(r64), Allocatable, Dimension(:)   :: ZoneAirCO2Old ! Last Time Steps Zone AIR Humidity Ratio

REAL(r64),DIMENSION(:),ALLOCATABLE  :: ZoneCO2MX    ! TEMPORARY ZONE CO2 TO TEST CONVERGENCE in Exact and Euler method
REAL(r64),DIMENSION(:),ALLOCATABLE  :: ZoneCO2M2    ! TEMPORARY ZONE CO2 at timestep t-2 in Exact and Euler method
REAL(r64),DIMENSION(:),ALLOCATABLE  :: ZoneCO21     ! Zone CO2 at the previous time step used in Exact and Euler method

REAL(r64),DIMENSION(:),ALLOCATABLE  :: CONTRAT      ! Zone CO2 at the previous time step used in Exact and Euler method

REAL(r64), Allocatable, Dimension(:)   :: MixingMassFlowCO2 !Mixing MASS FLOW * CO2

INTEGER :: NumContControlledZones  =0

REAL(r64)  ::  OutdoorCO2 = 0.0d0                   ! Outdoor CO2 level

REAL(r64), Allocatable, Dimension(:)   :: ZoneAirDensityCO !Mixing MASS FLOW * CO2
REAL(r64), Allocatable, Dimension(:)   :: AZ
REAL(r64), Allocatable, Dimension(:)   :: BZ
REAL(r64), Allocatable, Dimension(:)   :: CZ

! Generic contaminant

REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneGCSetpoint
REAL(r64), ALLOCATABLE, DIMENSION(:) :: GCPredictedRate

REAL(r64), Allocatable, Dimension(:)   :: ZoneGCGain           ! Generic contaminant gain from each Zone (People, equipment)

! Zone Air Contaminant conditions variables
REAL(r64), Allocatable, Dimension(:)   :: ZoneAirGCAvg         ! AIR generic contaminant averaged over the zone time step
REAL(r64), Allocatable, Dimension(:)   :: ZoneAirGC            ! AIR generic contaminant
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: GCZoneTimeMinus1     ! Generic contaminant history terms for 3rd order derivative
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: GCZoneTimeMinus2     ! Time Minus 2 Zone Time Steps Term
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: GCZoneTimeMinus3     ! Time Minus 3 Zone Time Steps Term
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: GCZoneTimeMinus4     ! Time Minus 4 Zone Time Steps Term
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: DSGCZoneTimeMinus1   ! DownStepped generic contaminant history terms for 3rd order
                                                               ! derivative
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: DSGCZoneTimeMinus2   ! DownStepped Time Minus 2 Zone Time Steps Term
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: DSGCZoneTimeMinus3   ! DownStepped Time Minus 3 Zone Time Steps Term
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: DSGCZoneTimeMinus4   ! DownStepped Time Minus 4 Zone Time Steps Term

REAL(r64), ALLOCATABLE, DIMENSION(:)   :: ZoneAirGCTemp   ! Temp zone air generic contaminant at time plus 1
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: GCZoneTimeMinus1Temp ! Zone air generic contaminant at previous timestep
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: GCZoneTimeMinus2Temp ! Zone air generic contaminant at timestep T-2
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: GCZoneTimeMinus3Temp ! Zone air generic contaminant at timestep T-3
REAL(r64), Allocatable, Dimension(:)   :: ZoneAirGCOld ! Last Time Steps Zone AIR generic contaminant

REAL(r64),DIMENSION(:),ALLOCATABLE  :: ZoneGCMX    ! TEMPORARY ZONE CO2 TO TEST CONVERGENCE in Exact and Euler method
REAL(r64),DIMENSION(:),ALLOCATABLE  :: ZoneGCM2    ! TEMPORARY ZONE CO2 at timestep t-2 in Exact and Euler method
REAL(r64),DIMENSION(:),ALLOCATABLE  :: ZoneGC1     ! Zone CO2 at the previous time step used in Exact and Euler method

REAL(r64),DIMENSION(:),ALLOCATABLE  :: CONTRATGC      ! Zone generic contaminant at the previous time step used in
                                                      ! Exact and Euler method

REAL(r64), Allocatable, Dimension(:)   :: MixingMassFlowGC !Mixing MASS FLOW * generic contaminant

REAL(r64)  ::  OutdoorGC = 0.0d0                   ! Outdoor generic contaminant level

REAL(r64), Allocatable, Dimension(:)   :: ZoneAirDensityGC !Mixing MASS FLOW * generic contaminant
REAL(r64), Allocatable, Dimension(:)   :: AZGC
REAL(r64), Allocatable, Dimension(:)   :: BZGC
REAL(r64), Allocatable, Dimension(:)   :: CZGC


          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:




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

END MODULE DataContaminantBalance
