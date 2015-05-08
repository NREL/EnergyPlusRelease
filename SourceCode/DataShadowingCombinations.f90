MODULE DataShadowingCombinations

          ! Module containing the data dealing with shadowing combinations

          ! MODULE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   July 2007; Moved from SolarShading
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
          ! <use statements for data only modules>
          ! <use statements for access to subroutines in other modules>

IMPLICIT NONE ! Enforce explicit typing of all variables

PUBLIC ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
TYPE ShadowingCombinations
  LOGICAL :: UseThisSurf                     = .false. ! True when this surface should be used in calculations
  INTEGER :: NumGenSurf                      = 0       ! Number of General surfaces for this surf
  INTEGER, ALLOCATABLE, DIMENSION(:) :: GenSurf        ! Array of General Surface Numbers
  INTEGER :: NumBackSurf                     = 0       ! Number of Back (Interior) surfaces for this surf
  INTEGER, ALLOCATABLE, DIMENSION(:) :: BackSurf       ! Array of Back (Interior) surface numbers
  INTEGER :: NumSubSurf                      = 0       ! Number of SubSurfaces for this surf
  INTEGER, ALLOCATABLE, DIMENSION(:) :: SubSurf        ! Array of SubSurface surface Numbers
END TYPE


          ! MODULE VARIABLE DECLARATIONS:
TYPE (ShadowingCombinations), ALLOCATABLE, DIMENSION(:)   :: ShadowComb

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

END MODULE DataShadowingCombinations

