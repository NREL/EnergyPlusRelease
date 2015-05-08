MODULE NonZoneEquipmentManager

          ! MODULE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   January 2004
          !       MODIFIED       Hudson, ORNL July 2007
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          !

          ! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na
          ! OTHER NOTES: na
          ! USE STATEMENTS: na

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS: na
          ! MODULE VARIABLE DECLARATIONS: na

          ! SUBROUTINE SPECIFICATIONS:
PUBLIC ManageNonZoneEquipment

CONTAINS

          ! MODULE SUBROUTINES:
SUBROUTINE ManageNonZoneEquipment(FirstHVACIteration,SimNonZoneEquipment)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Dan Fisher
          !       DATE WRITTEN   Sept. 2000
          !       RE-ENGINEERED  Richard Liesen
          !       DATE MODIFIED  February 2003
          !       MODIFIED       Hudson, ORNL July 2007
          !       MODIFIED       B. Grifffith, NREL, April 2008,
          !                      added calls for just heat recovery part of chillers
          !       MODIFIED       Removed much for plant upgrade, 2011

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine checks the input file for any non-zone equipment objects and gets their input.
          ! Zone equipment objects are generally triggered to "get input" when they are called for simulation
          ! by the ZoneEquipmentManager because they are referenced by a Zone Equipment List.  In the case of
          ! the NonZoneEquipmentManager, it does not yet have a list of non-zone equipment, so it must make
          ! one here before it knows what to call for simulation.

          ! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE DataGlobals,            ONLY: ZoneSizingCalc
  USE InputProcessor,         ONLY: GetNumObjectsFound
  USE WaterThermalTanks,      ONLY: SimulateWaterHeaterStandAlone
  USE WaterUse,               ONLY: SimulateWaterUse
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(IN)    :: FirstHVACIteration
  LOGICAL, INTENT(INOUT) :: SimNonZoneEquipment ! Simulation convergence flag

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WaterHeaterNum          ! Water heater object number
  INTEGER, SAVE :: NumOfWaterHeater
  LOGICAL, SAVE :: CountNonZoneEquip = .TRUE.

          ! FLOW:
  IF (CountNonZoneEquip) THEN
    NumOfWaterHeater               = GetNumObjectsFound('WaterHeater:Mixed') + GetNumObjectsFound('WaterHeater:Stratified')
    CountNonZoneEquip = .FALSE.
  END IF

  CALL SimulateWaterUse(FirstHVACIteration) ! simulate non-plant loop water use.

  IF (.not. ZoneSizingCalc) THEN
    DO WaterHeaterNum = 1, NumOfWaterHeater
      CALL SimulateWaterHeaterStandAlone(WaterHeaterNum,FirstHVACIteration)
    END DO
  ENDIF

  IF (FirstHVACIteration) THEN
    SimNonZoneEquipment = .TRUE.
  ELSE
    SimNonZoneEquipment = .FALSE.
  END IF

  RETURN

END SUBROUTINE ManageNonZoneEquipment

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

END MODULE NonZoneEquipmentManager
