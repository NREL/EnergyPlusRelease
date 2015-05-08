MODULE DataVectorTypes      ! EnergyPlus Data-Only Module

          ! MODULE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for the "vector" and "plane equation"
          ! derived types.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.


          ! MODULE PARAMETER DEFINITIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS
  TYPE :: vector  ! This is used to specify a point in 3D space
                  ! Right Handed Coordinate system is used
    REAL(r64) :: x
    REAL(r64) :: y
    REAL(r64) :: z
  END TYPE

  TYPE :: PlaneEq ! This is used to specify a plane based on vectors in that plane
    REAL(r64) :: x
    REAL(r64) :: y
    REAL(r64) :: z
    REAL(r64) :: w
  END TYPE

  TYPE :: Face  ! Used to specify the face of a polyhedron
    INTEGER :: NSides=0 ! Number of Sides for this Face
    INTEGER :: SurfNum  ! ALLOCATABLE to actual surface number
    TYPE (vector), ALLOCATABLE, DIMENSION(:) :: FacePoints
    TYPE (vector) :: NewellAreaVector
  END TYPE


  TYPE :: Polyhedron ! This is used to specify a polyhedron based on the vectors that comprise it (a zone).
    INTEGER :: NumSurfaceFaces=0
    TYPE (Face), ALLOCATABLE, DIMENSION(:) :: SurfaceFace
  END TYPE

  ! the following two derived types are used in triangulation (for DXF outputs)
  !'Points (Vertices)
  Type Vector_2d
     REAL(r64) x
     REAL(r64) y
  End Type

  !'Created Triangles, vv# are the vertex pointers
  Type dTriangle
    integer vv0
    integer vv1
    integer vv2
  End Type


          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

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

END MODULE DataVectorTypes
