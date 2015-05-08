MODULE DataContaminantBalance      ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Lixing Gu
          !       DATE WRITTEN   May 2010
          !       MODIFIED       na
          !       MODIFIED       na
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
END TYPE

TYPE :: ZoneContControls
  CHARACTER(len=MaxNameLength) :: Name                 =' ' ! Name of the contaminant controller
  CHARACTER(len=MaxNameLength) :: ZoneName             =' ' ! Name of the zone
  INTEGER                      :: ActualZoneNum        =0
  CHARACTER(len=MaxNameLength) :: AvaiSchedule         =' ' ! Availability Schedule name
  INTEGER  :: AvaiSchedPtr                             =0   ! Pointer to the correct schedule
  CHARACTER(len=MaxNameLength) :: SetPointSchedName =' '    ! Name of the schedule which determines the contaminant setpoint
  INTEGER :: SPSchedIndex                              =0   ! Index for this schedule
  LOGICAL   :: EMSOverrideCO2SetpointOn          = .FALSE.  ! EMS is calling to override CO2 setpoint
  REAL(r64) :: EMSOverrideCO2SetpointValue       = 0.0D0    ! value EMS is directing to use for CO2 setpoint
  INTEGER :: NumOfZones                                =0   ! Number of controlled zones in the same airloop
  INTEGER,DIMENSION(:),ALLOCATABLE  :: ControlZoneNum       ! Controlled zone number
  CHARACTER(len=MaxNameLength) :: ZoneMinCO2SchedName =' '    ! Name of the schedule which determines minimum CO2 concentration
  INTEGER :: ZoneMinCO2SchedIndex                              =0   ! Index for this schedule
  INTEGER :: ZoneContamControllerSchedIndex                    =0   ! Index for this schedule
  
END TYPE ZoneContControls

TYPE ZoneSystemContaminantDemandData                   ! Contaminent loads to be met (kg air per second)
  REAL(r64) :: OutputRequiredToCO2SP              =0.0 ! Load required to meet CO2 setpoint 
  REAL(r64) :: RemainingOutputReqToCO2SP          =0.0 ! Remaining load required to meet CO2 setpoint
END TYPE ZoneSystemContaminantDemandData

TYPE (ZoneSystemContaminantDemandData), ALLOCATABLE, DIMENSION(:)     :: ZoneSysContDemand


! MODULE VARIABLE Type DECLARATIONS:
TYPE (ContaminantData) :: Contaminant=ContaminantData(.false.,.false.,0)

TYPE (ZoneContControls), ALLOCATABLE, DIMENSION(:)     :: ContaminantControlledZone

REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneCO2Setpoint
REAL(r64), ALLOCATABLE, DIMENSION(:) :: CO2PredictedRate

REAL(r64), Allocatable, Dimension(:)   :: ZoneCO2Gain           !CO2 gain from each Zone (People, equipment)
REAL(r64), Allocatable, Dimension(:)   :: ZoneCO2GainFromPeople  !CO2 gain from each Zone (From People only)

! Zone Air Contaminant conditions variables
REAL(r64), Allocatable, Dimension(:)   :: ZoneAirCO2Avg         !AIR Humidity Ratio averaged over the zone time step
REAL(r64), Allocatable, Dimension(:)   :: ZoneAirCO2            !AIR Humidity Ratio
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: CO2ZoneTimeMinus1     ! Humidity ratio history terms for 3rd order derivative
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: CO2ZoneTimeMinus2     ! Time Minus 2 Zone Time Steps Term
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: CO2ZoneTimeMinus3     ! Time Minus 3 Zone Time Steps Term
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: CO2ZoneTimeMinus4     ! Time Minus 4 Zone Time Steps Term
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: DSCO2ZoneTimeMinus1   ! DownStepped Humidity ratio history terms for 3rd order derivative
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: DSCO2ZoneTimeMinus2   ! DownStepped Time Minus 2 Zone Time Steps Term
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: DSCO2ZoneTimeMinus3   ! DownStepped Time Minus 3 Zone Time Steps Term
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: DSCO2ZoneTimeMinus4   ! DownStepped Time Minus 4 Zone Time Steps Term

REAL(r64), ALLOCATABLE, DIMENSION(:)   :: ZoneAirCO2Temp   ! Temp zone air humidity ratio at time plus 1
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: CO2ZoneTimeMinus1Temp ! Zone air humidity ratio at previous timestep
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: CO2ZoneTimeMinus2Temp ! Zone air humidity ratio at timestep T-2
REAL(r64), ALLOCATABLE, DIMENSION(:)   :: CO2ZoneTimeMinus3Temp ! Zone air humidity ratio at timestep T-3
REAL(r64), Allocatable, Dimension(:)   :: ZoneAirCO2Old ! Last Time Steps Zone AIR Humidity Ratio

REAL(r64),DIMENSION(:),ALLOCATABLE  :: ZoneCO2MX    ! TEMPORARY ZONE CO2 TO TEST CONVERGENCE in Exact and Euler method
REAL(r64),DIMENSION(:),ALLOCATABLE  :: ZoneCO2M2    ! TEMPORARY ZONE CO2 at timestep t-2 in Exact and Euler method
REAL(r64),DIMENSION(:),ALLOCATABLE  :: ZoneCO21     ! Zone CO2 at the previous time step used in Exact and Euler method

REAL(r64),DIMENSION(:),ALLOCATABLE  :: CONTRAT     ! Zone CO2 at the previous time step used in Exact and Euler method

REAL(r64), Allocatable, Dimension(:)   :: MixingMassFlowCO2 !Mixing MASS FLOW * CO2

INTEGER :: NumContControlledZones  =0
! Outdoor CO2 level
REAL(r64)                       ::  OutdoorCO2=0.0d0

REAL(r64), Allocatable, Dimension(:)   :: ZoneAirDensityCO !Mixing MASS FLOW * CO2
REAL(r64), Allocatable, Dimension(:)   :: AZ 
REAL(r64), Allocatable, Dimension(:)   :: BZ 
REAL(r64), Allocatable, Dimension(:)   :: CZ 


          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:




!     NOTICE
!
!     Copyright © 1996-2011 The Board of Trustees of the University of Illinois
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
