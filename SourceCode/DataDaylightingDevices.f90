MODULE DataDaylightingDevices      ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   May 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for variables used in daylighting devices which
          ! are shared by several modules.

          ! METHODOLOGY EMPLOYED: na
          ! REFERENCES: na
          ! OTHER NOTES: na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.

          ! MODULE PARAMETER DEFINITIONS:
INTEGER, PARAMETER :: MaxTZones = 10     ! Maximum number of transition zones
INTEGER, PARAMETER :: NumOfAngles = 19   ! Number of data points on transmittance vs. angle curve

INTEGER, PARAMETER :: VisibleBeam = 1    ! Constant for radiation type
INTEGER, PARAMETER :: SolarBeam   = 2    ! Constant for radiation type
INTEGER, PARAMETER :: SolarAniso  = 3    ! Constant for radiation type
INTEGER, PARAMETER :: SolarIso    = 4    ! Constant for radiation type

          ! DERIVED TYPE DEFINITIONS:
TYPE TDDPipeData
  ! Input variables
  CHARACTER(len=MaxNameLength)   :: Name = ''               ! Name of TDD pipe
  INTEGER                        :: Dome = 0                ! Pointer to the dome object
  INTEGER                        :: Diffuser = 0            ! Pointer to the diffuser object
  INTEGER                        :: Construction = 0        ! Pointer to the construction object
  REAL(r64)                      :: Diameter = 0.0d0          ! Pipe diameter
  REAL(r64)                      :: TotLength = 0.0d0         ! Total length of pipe, including exterior
  REAL(r64)                      :: Reff = 0.0d0              ! Effective R value between TDD:DOME and TDD:DIFFUSER
  INTEGER                        :: NumOfTZones = 0         ! Number of transition zone
  INTEGER, ALLOCATABLE, DIMENSION(:) :: TZone                   ! Pointers to transition zones
  REAL(r64), ALLOCATABLE, DIMENSION(:)    :: TZoneLength             ! Length of pipe in each transition zone

  ! Calculated variables
  REAL(r64)                      :: AspectRatio = 0.0d0       ! Aspect ratio, length / diameter
  REAL(r64)                      :: ReflectVis = 0.0d0        ! Visible reflectance of surface
  REAL(r64)                      :: ReflectSol = 0.0d0        ! Solar reflectance of surface
  REAL(r64), DIMENSION(NumOfAngles)   :: PipeTransVisBeam = 0.0d0  ! Table of beam visible transmittance vs. cosine angle
  REAL(r64), DIMENSION(NumOfAngles)   :: PipeTransSolBeam = 0.0d0  ! Table of beam solar transmittance vs. cosine angle
  REAL(r64)                      :: TransSolIso = 0.0d0       ! Diffuse isotropic solar transmittance (constant)
  REAL(r64)                      :: TransSolHorizon = 0.0d0   ! Diffuse horizon solar transmittance (constant)
  REAL(r64)                      :: ExtLength = 0.0d0         ! Exterior exposed length of pipe
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: TZoneHeatGain     ! convection gain to transition zones

  ! Report variables
  REAL(r64)                      :: TransmittedSolar = 0.0d0  ! Solar transmitted by the TDD [W]
  REAL(r64)                      :: PipeAbsorbedSolar = 0.0d0 ! Solar absorbed in the walls of the pipe [W]
  REAL(r64)                      :: HeatGain = 0.0d0          ! Solar heat gain [W]
  REAL(r64)                      :: HeatLoss = 0.0d0          ! Solar heat loss [W]

  REAL(r64)                      :: TransVisBeam = 0.0d0      ! TDD visible transmittance
  REAL(r64)                      :: TransSolBeam = 0.0d0      ! TDD beam solar transmittance
  REAL(r64)                      :: TransVisDiff = 0.0d0      ! TDD diffuse visible transmittance
  REAL(r64)                      :: TransSolDiff = 0.0d0      ! TDD diffuse solar transmittance
END TYPE TDDPipeData

TYPE ShelfData
  ! Input variables
  CHARACTER(len=MaxNameLength)   :: Name = ''               ! Name of daylighting shelf
  INTEGER                        :: Window = 0              ! Pointer to the window object
  INTEGER                        :: InSurf = 0              ! Pointer to the inside shelf heat transfer surface
  INTEGER                        :: OutSurf = 0             ! Pointer to the outside shelf attached shading surface
  INTEGER                        :: Construction = 0        ! Pointer to the outside shelf construction object

  ! Calculated variables
  REAL(r64)                      :: OutReflectVis = 0.0d0     ! Outside shelf visible reflectance
  REAL(r64)                      :: OutReflectSol = 0.0d0     ! Outside shelf solar reflectance
  REAL(r64)                      :: ViewFactor = 0.0d0        ! Outside shelf view factor to window

  ! Report variables

END TYPE ShelfData

          ! MODULE VARIABLE TYPE DECLARATIONS:
TYPE (TDDPipeData), ALLOCATABLE, DIMENSION(:) :: TDDPipe
TYPE (ShelfData), ALLOCATABLE, DIMENSION(:) :: Shelf

          ! INTERFACE BLOCK SPECIFICATIONS: na

          ! MODULE VARIABLE DECLARATIONS:
INTEGER :: NumOfTDDPipes=0  ! Number of TDD pipes in the input file
INTEGER :: NumOfShelf   =0  ! Number of daylighting shelves in the input file

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

END MODULE DataDaylightingDevices
