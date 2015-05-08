MODULE DataMoistureBalanceEMPD      ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Muthusamy V. Swami and Lixing Gu
          !       DATE WRITTEN   Aug. 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This module should contain the information that is needed to calculate
          ! moisture level at interior surfaces

          ! USE STATEMENTS:
USE DataPrecisionGlobals

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC    ! By definition, all variables which are placed in this data-only
          ! module should be available to other modules and routines.  Thus,
          ! all variables in this module must be PUBLIC.

          ! MODULE PARAMETER DEFINITIONS

          ! Parameters for the definition and limitation of arrays:

REAL(r64), Parameter :: Lam = 2500000.0d0 ! heat of adsorption for building materials

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! MODULE VARIABLE DECLARATIONS:
! Variables that are used in both the Surface Heat Balance and the Moisture Balance
REAL(r64), ALLOCATABLE, DIMENSION(:) :: MoistEMPDOld ! Moisture level at interior surfaces at previous time step
REAL(r64), ALLOCATABLE, DIMENSION(:) :: MoistEMPDInt ! Moisture level at interior surfaces at previous interation
                                                            ! and current time step
REAL(r64), ALLOCATABLE, DIMENSION(:) :: MoistEMPDNew ! Moisture level at interior surfaces at current interation
                                                            ! and current time step
REAL(r64), ALLOCATABLE, DIMENSION(:) :: MoistEMPDFlux ! Moisture flux at interior surfaces [W]

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

END MODULE DataMoistureBalanceEMPD
