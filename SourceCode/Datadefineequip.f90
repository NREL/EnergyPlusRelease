MODULE DataDefineEquip      ! EnergyPlus Data-Only Module

  ! MODULE INFORMATION
  !             AUTHOR:  Russ Taylor
  !       DATE WRITTEN:  Sept 1997


  ! PURPOSE OF THIS MODULE:
  ! This module  contains the essential coil information that is needed by water and air
  ! loop managers as well as the coil simulations

  ! METHODOLOGY EMPLOYED:
  ! Needs description, as appropriate.

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.

  !MODULE PARAMETER DEFINITIONS
INTEGER, PARAMETER :: MaxZoneAirComponents = 1
!INTEGER, PARAMETER :: MaxZoneAirControls = 4
! Equipment Types covered by ZoneAirLoopEquipment:
INTEGER, PARAMETER :: DualDuctConstVolume=1
INTEGER, PARAMETER :: DualDuctVAV=2
INTEGER, PARAMETER :: SingleDuctVAVReheat=3
INTEGER, PARAMETER :: SingleDuctConstVolReheat=4
INTEGER, PARAMETER :: SingleDuctVAVNoReheat=5
INTEGER, PARAMETER :: SingleDuct_SeriesPIU_Reheat=6
INTEGER, PARAMETER :: SingleDuct_ParallelPIU_Reheat=7
INTEGER, PARAMETER :: SingleDuct_ConstVol_4PipeInduc=8
INTEGER, PARAMETER :: SingleDuctVAVReheatVSFan = 9
INTEGER, PARAMETER :: SingleDuctCBVAVReheat=10
INTEGER, PARAMETER :: SingleDuctCBVAVNoReheat=11
INTEGER, PARAMETER :: SingleDuctConstVolCooledBeam=12
INTEGER, PARAMETER :: DualDuctVAVOutdoorAir=13
INTEGER, PARAMETER :: SingleDuctUserDefined=14
INTEGER, PARAMETER :: SingleDuctInletATMixer=15
INTEGER, PARAMETER :: SingleDuctSupplyATMixer=16

  ! DERIVED TYPE DEFINITIONS
TYPE ZoneAirEquip
  CHARACTER(len=MaxNameLength) :: Name=' '     ! Name or identifier of this piece of equipment
  INTEGER      :: OutletNodeNum      =0   ! index of outlet node
  INTEGER      :: NumComponents      =0   ! number of subcomponents (=1)
  INTEGER      :: NumControls        =0   ! number of controls (not used; =0)
  CHARACTER(len=MaxNameLength), DIMENSION(MaxZoneAirComponents) :: EquipType=' '   ! Pointer indentifying type of subcomponent
  INTEGER, DIMENSION(MaxZoneAirComponents) :: EquipType_Num=0
  CHARACTER(len=MaxNameLength), DIMENSION(MaxZoneAirComponents) :: EquipName=' '   ! name of subcomponent
  INTEGER, DIMENSION(MaxZoneAirComponents) :: EquipIndex=0
  REAL(r64)    :: UpStreamLeakFrac   =0.0d0 ! upstream nominal leakage fraction
  REAL(r64)    :: DownStreamLeakFrac =0.0d0 ! downstream constant leakage fraction
  REAL(r64)    :: MassFlowRateUpStrLk=0.0d0 ! current air mass flow rate of the upstream leak [kg/s]
  REAL(r64)    :: MassFlowRateDnStrLk=0.0d0 ! current air mass flow rate of the downstream leak [kg/s]
  REAL(r64)    :: MassFlowRateTU     =0.0d0 ! current air mass flow rate tjrough the terminal unit [kg/s]
  REAL(r64)    :: MassFlowRateZSup   =0.0d0 ! current air mass flow rate of zone supply air [kg/s]
  REAL(r64)    :: MassFlowRateSup    =0.0d0 ! current air mass flow rate of supply air upstream of upstream leak [kg/s]
  REAL(r64)    :: MaxAvailDelta      =0.0d0 ! change in max avail mass low rate due to leaks [kg/s]
  REAL(r64)    :: MinAvailDelta      =0.0d0 ! change in min avail mass low rate due to leaks [kg/s]
  INTEGER      :: InletNodeNum       =0   ! index of inlet node
  INTEGER      :: ZoneEqNum          =0   ! index of zone equipment object for this terminal unit
  REAL(r64)    :: LeakLoadMult       =0.0d0 ! zome load multiplier to adjust for downstream leak
  LOGICAL      :: UpStreamLeak       =.FALSE. ! if true, there is an upstream leak
  LOGICAL      :: DownStreamLeak     =.FALSE. ! if true, there is an downstream leak
END TYPE ZoneAirEquip


  !MODULE VARIABLE DECLARATIONS:
TYPE (ZoneAirEquip), ALLOCATABLE, DIMENSION(:) :: AirDistUnit ! Used to specify zone related
                                                              ! components of air system
INTEGER                                        :: NumAirDistUnits=0

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

END MODULE DataDefineEquip


