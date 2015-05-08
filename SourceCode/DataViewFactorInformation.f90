MODULE DataViewFactorInformation

          ! Module containing the data dealing with view factor information for ScriptF
          ! and Diffuse Solar distribution calculations

          ! MODULE INFORMATION:
          !       AUTHOR         Rob Hitchcock
          !       DATE WRITTEN   September 2007; Moved from HeatBalanceIntRadExchange
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
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength

          ! <use statements for access to subroutines in other modules>

IMPLICIT NONE ! Enforce explicit typing of all variables

PUBLIC ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
TYPE ZoneViewFactorInformation
  CHARACTER(len=MaxNameLength)   :: Name          =' ' ! Zone name
  INTEGER                        :: NumOfSurfaces =0   ! Number of surfaces in the zone
  REAL(r64), DIMENSION(:,:),  ALLOCATABLE :: F             ! View Factors
  REAL(r64), DIMENSION(:,:),  ALLOCATABLE :: ScriptF       ! Hottel's Script F
  REAL(r64), DIMENSION(:),    ALLOCATABLE :: Area          ! Surface area
  REAL(r64), DIMENSION(:),    ALLOCATABLE :: Emissivity    ! Surface emissivity
  REAL(r64), DIMENSION(:),    ALLOCATABLE :: Azimuth       ! Azimuth angle of the surface (in degrees)
  REAL(r64), DIMENSION(:),    ALLOCATABLE :: Tilt          ! Tilt angle of the surface (in degrees)
  INTEGER, DIMENSION(:), ALLOCATABLE :: SurfacePtr    ! Surface ALLOCATABLE (to Surface derived type)
  Character(len=MaxNameLength),DIMENSION(:),ALLOCATABLE :: Class        ! Class of surface (Wall, Roof, etc.)
END TYPE ZoneViewFactorInformation


          ! MODULE VARIABLE DECLARATIONS:
TYPE(ZoneViewFactorInformation), ALLOCATABLE, DIMENSION(:) :: ZoneInfo

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

END MODULE DataViewFactorInformation

