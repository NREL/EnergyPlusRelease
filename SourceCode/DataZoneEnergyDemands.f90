MODULE DataZoneEnergyDemands      ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION
          !             AUTHOR:  Russ Taylor
          !       DATE WRITTEN:  Oct 1997


          ! PURPOSE OF THIS MODULE:
          ! This module  contains the essential coil information that is needed by water and air
          !loop managers as well as the coil simulations

USE DataPrecisionGlobals

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.


          ! MODULE VARIABLE DECLARATIONS:

TYPE ZoneSystemDemandData ! Sensible cooling/heating loads to be met (watts)
  REAL(r64) :: RemainingOutputRequired            =0.0d0 !
  REAL(r64) :: TotalOutputRequired                =0.0d0 !
  REAL(r64) :: OutputRequiredToHeatingSP          =0.0d0 ! Load required to meet heating setpoint (>0 is a heating load)
  REAL(r64) :: OutputRequiredToCoolingSP          =0.0d0 ! Load required to meet cooling setpoint (<0 is a cooling load)
  REAL(r64) :: RemainingOutputReqToHeatSP         =0.0d0 ! Remaining load required to meet heating setpoint (>0 is a heating load)
  REAL(r64) :: RemainingOutputReqToCoolSP         =0.0d0 ! Remaining load required to meet cooling setpoint (<0 is a cooling load)
  INTEGER   :: NumZoneEquipment                   = 0  ! count of zone equipment for this zone, from ZoneHVAC:EquipmentList
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: SequencedOutputRequired
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: SequencedOutputRequiredToHeatingSP !load required to meet heating setpoint by sequence
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: SequencedOutputRequiredToCoolingSP !load required to meet cooling setpoint by sequence
  REAL(r64) :: SupplyAirAdjustFactor              = 1.0D0 ! supply air adjustment factor due to the cap of
                                                          ! zone maximum outdoor air fraction
  INTEGER   :: StageNum                           = 0  ! The stage number when staged thermostate is used: 
                                                       ! 0 no load, >0 Heating stage, <0 Cooling stage
END TYPE ZoneSystemDemandData

TYPE ZoneSystemMoistureDemand  ! Humidification/dehumidification loads to be met (kg water per second)
  REAL(r64) :: RemainingOutputRequired            =0.0d0 !
  REAL(r64) :: TotalOutputRequired                =0.0d0 !
  REAL(r64) :: OutputRequiredToHumidifyingSP      =0.0d0 ! Load required to meet humidifying setpoint (>0 = a humidify load)
  REAL(r64) :: OutputRequiredToDehumidifyingSP    =0.0d0 ! Load required to meet dehumidifying setpoint (<0 = a dehumidify load)
  REAL(r64) :: RemainingOutputReqToHumidSP        =0.0d0 ! Remaining load required to meet humidifying setpoint
                                                       ! (>0 is a humidify load)
  REAL(r64) :: RemainingOutputReqToDehumidSP      =0.0d0 ! Remaining load required to meet dehumidifying setpoint
                                                       ! (<0 is a dehumidify load)
  INTEGER   :: NumZoneEquipment                   = 0  ! count of zone equipment for this zone, from ZoneHVAC:EquipmentList
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: SequencedOutputRequired
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: SequencedOutputRequiredToHumidSP !load required to meet humidify setpoint by sequence
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: SequencedOutputRequiredToDehumidSP !load required to meet dehumidify setpoint by sequenc
END TYPE ZoneSystemMoistureDemand

TYPE (ZoneSystemDemandData), ALLOCATABLE, DIMENSION(:)     :: ZoneSysEnergyDemand
TYPE (ZoneSystemMoistureDemand), ALLOCATABLE, DIMENSION(:) :: ZoneSysMoistureDemand

LOGICAL, ALLOCATABLE :: DeadbandOrSetback(:)    ! true if zone temperature is in the thermostat deadband
                                                ! before any heating / cooling done
LOGICAL, ALLOCATABLE :: Setback(:)              ! true if zone temperature has increased
                                                ! from previous setting
LOGICAL, ALLOCATABLE :: CurDeadbandOrSetback(:) ! same as above except updated after each piece of zone equipment
                                                ! in a zone is simulated


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

END MODULE DataZoneEnergyDemands


