!  Multiple modules are contained in this file
!  Vectors -- Vector utilities
!  DXFEarClipping -- Triangulate >4 sided surfaces

MODULE vectors
  ! Module containing the routines dealing with Vector operations

  ! MODULE INFORMATION:
  !       AUTHOR         Linda Lawrie
  !       DATE WRITTEN   April 2000
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! This module uses a global "vector" data structure and defines
  ! operations (using the F90 "operator" features) and other
  ! manipulations used with Vectors.

  ! Vectors should allow for more modern interpretations of the
  ! calculations used in the shadowing and other surface manipulations.

  ! METHODOLOGY EMPLOYED:
  ! Uses the "Vector" derived type variables to allow for more readable
  ! calculations.

  ! REFERENCES: Original idea from F90 for Scientists and
  ! Engineers, S. J. Chapman, 1996.
  ! The module defines 8 operations which can be performed on vectors:
  !
  !               Operation                     Operator
  !               =========                     ========
  !   1.  Creation from a real array               =
  !   2.  Conversion to real array                 =
  !   3.  Vector addition                          +
  !   4.  Vector subtraction                       -
  !   5.  Vector-scalar multiplication (4 cases)   *
  !   6.  Vector-scalar division (2 cases)         /
  !   7.  Dot product                            .dot.
  !   8.  Cross product                            *
  !   9.  2d dot product                         .twoddot.
  !  10.  2d Cross product                       .twodcross.
  !
  ! It contains a total of 12 procedures to implement those
  ! operations:  array_to_vector, vector_to_array, vector_add,
  ! vector_subtract, vector_times_real, real_times_vector,
  ! vector_times_int, int_times_vector, vector_div_real,
  ! vector_div_int, dot_product, and cross_product.

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals, ONLY: DegToRadians, PI, PIOvr2, outputfiledebug
USE DataVectorTypes

IMPLICIT NONE         ! Enforce explicit typing of all variables

PUBLIC ! For this module, most things are public, private declarations
       ! are made explicitly

  ! INTERFACE DEFINITIONS
INTERFACE ASSIGNMENT (=)
   MODULE PROCEDURE array_to_vector
   MODULE PROCEDURE vector_to_array
   MODULE PROCEDURE value_to_vector
END INTERFACE

INTERFACE OPERATOR (+)
   MODULE PROCEDURE vector_add
END INTERFACE

INTERFACE OPERATOR (-)
   MODULE PROCEDURE vector_subtract
END INTERFACE

INTERFACE OPERATOR (*)
!   MODULE PROCEDURE vector_times_double
!   MODULE PROCEDURE double_times_vector
   MODULE PROCEDURE vector_times_real
   MODULE PROCEDURE real_times_vector
   MODULE PROCEDURE vector_times_int
   MODULE PROCEDURE int_times_vector
   MODULE PROCEDURE vec_cross_product
END INTERFACE

INTERFACE OPERATOR (/)
   MODULE PROCEDURE vector_div_real
   MODULE PROCEDURE vector_div_int
END INTERFACE

INTERFACE OPERATOR (.dot.)
   MODULE PROCEDURE vec_dot_product
END INTERFACE

INTERFACE OPERATOR (.twoddot.)
   MODULE PROCEDURE vec2d_dot_product
END INTERFACE

INTERFACE OPERATOR (.twodcross.)
   MODULE PROCEDURE vec2d_cross_product
END INTERFACE

  !MODULE PARAMETER DEFINITIONS
TYPE (vector), PRIVATE, PARAMETER :: XUnit=vector(1.0d0,0.0d0,0.0d0 )
TYPE (vector), PRIVATE, PARAMETER :: YUnit=vector(0.0d0,1.0d0,0.0d0 )
TYPE (vector), PRIVATE, PARAMETER :: ZUnit=vector(0.0d0,0.0d0,1.0d0 )

  ! DERIVED TYPE DEFINITIONS
  ! na

  !MODULE VARIABLE DECLARATIONS:
  ! na

  !SUBROUTINE SPECIFICATIONS FOR MODULE <module_name>

CONTAINS

!**** Following routines are referenced from the previous operators.

   SUBROUTINE array_to_vector(vec_result, array)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine allows for the assignment of a 3-element
     ! array to a vector (x,y,z).

     ! METHODOLOGY EMPLOYED:
     ! Uses the assignment operator (=) to allow for this
     ! operation.  As in:
     ! vector_data=XYZ  (where XYZ is a 3-element array with x,y,z values)

      TYPE (vector), INTENT(OUT) :: vec_result
!      REAL, DIMENSION(3), INTENT(IN) :: array
      REAL(r64), DIMENSION(3), INTENT(IN) :: array
      vec_result%x = array(1)
      vec_result%y = array(2)
      vec_result%z = array(3)
      RETURN
   END SUBROUTINE array_to_vector

   SUBROUTINE vector_to_array(array_result, vec_1)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine allows for the assignment of a vector (x,y,z)
     ! to a 3 element array.

     ! METHODOLOGY EMPLOYED:
     ! Uses the assignment operator (=) to allow for this
     ! operation.  As in:
     ! XYZ=vector_data  (where XYZ is a 3-element array with x,y,z values)

!      REAL, DIMENSION(3), INTENT(OUT) :: array_result
      REAL(r64), DIMENSION(3), INTENT(OUT) :: array_result
      TYPE (vector), INTENT(IN) :: vec_1
      array_result(1) = vec_1%x
      array_result(2) = vec_1%y
      array_result(3) = vec_1%z
      RETURN
   END SUBROUTINE vector_to_array

   SUBROUTINE value_to_vector(vec_result, value)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine allows for the assignment of a 3-element
     ! array to a vector (x,y,z).

     ! METHODOLOGY EMPLOYED:
     ! Uses the assignment operator (=) to allow for this
     ! operation.  As in:
     ! vector_data=XYZ  (where XYZ is a 3-element array with x,y,z values)

      TYPE (vector), INTENT(OUT) :: vec_result
!      REAL, INTENT(IN) :: value
      REAL(r64), INTENT(IN) :: value
      vec_result%x = value
      vec_result%y = value
      vec_result%z = value
      RETURN
   END SUBROUTINE value_to_vector

   FUNCTION vector_add(vec_1, vec_2)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine allows for the addition of two vectors.

     ! METHODOLOGY EMPLOYED:
     ! Uses the addition operator (+) to allow for this
     ! operation.  As in:
     ! Vector=Vector1+Vector2

      TYPE (vector) :: vector_add
      TYPE (vector), INTENT(IN) :: vec_1, vec_2
      vector_add%x = vec_1%x + vec_2%x
      vector_add%y = vec_1%y + vec_2%y
      vector_add%z = vec_1%z + vec_2%z
      RETURN
   END FUNCTION vector_add

   FUNCTION vector_subtract(vec_1, vec_2)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine allows for the subtraction of two vectors.

     ! METHODOLOGY EMPLOYED:
     ! Uses the subtraction operator (-) to allow for this
     ! operation.  As in:
     ! Vector=Vector1-Vector2

      TYPE (vector) :: vector_subtract
      TYPE (vector), INTENT(IN) :: vec_1, vec_2
      vector_subtract%x = vec_1%x - vec_2%x
      vector_subtract%y = vec_1%y - vec_2%y
      vector_subtract%z = vec_1%z - vec_2%z
      RETURN
   END FUNCTION vector_subtract

!   FUNCTION vector_times_double(vec_1, double_2)
!
!     ! PURPOSE OF THIS SUBROUTINE:
!     ! This subroutine allows for a vector to be multiplied by a
!     ! scalar (double precision) value.  Note, this provides a convenient way
!     ! to initialize a vector (i.e. Vector*0.0)
!
!     ! METHODOLOGY EMPLOYED:
!     ! Uses the multiplication operator (*) to allow for this
!     ! operation.  As in:
!     ! Vector=Vector1*Double_Value
!
!      TYPE (vector) :: vector_times_double
!      TYPE (vector), INTENT(IN) :: vec_1
!      DOUBLE PRECISION, INTENT(IN) :: double_2
!      vector_times_double%x = vec_1%x * double_2
!      vector_times_double%y = vec_1%y * double_2
!      vector_times_double%z = vec_1%z * double_2
!      RETURN
!   END FUNCTION vector_times_double
!
!   FUNCTION double_times_vector(double_1, vec_2)
!
!     ! PURPOSE OF THIS SUBROUTINE:
!     ! This subroutine allows for a vector to be multiplied by a
!     ! scalar (double precision) value.
!
!     ! METHODOLOGY EMPLOYED:
!     ! Uses the multiplication operator (*) to allow for this
!     ! operation.  As in:
!     ! Vector=Double_Value*Vector1
!
!      TYPE (vector) :: double_times_vector
!      DOUBLE PRECISION, INTENT(IN) :: double_1
!      TYPE (vector), INTENT(IN) :: vec_2
!      double_times_vector%x = double_1 * vec_2%x
!      double_times_vector%y = double_1 * vec_2%y
!      double_times_vector%z = double_1 * vec_2%z
!      RETURN
!   END FUNCTION double_times_vector

   FUNCTION vector_times_real(vec_1, real_2)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine allows for a vector to be multiplied by a
     ! scalar (real) value.  Note, this provides a convenient way
     ! to initialize a vector (i.e. Vector*0.0)

     ! METHODOLOGY EMPLOYED:
     ! Uses the multiplication operator (*) to allow for this
     ! operation.  As in:
     ! Vector=Vector1*Real_Value

      TYPE (vector) :: vector_times_real
      TYPE (vector), INTENT(IN) :: vec_1
!      REAL, INTENT(IN) :: real_2
      REAL(r64), INTENT(IN) :: real_2
      vector_times_real%x = vec_1%x * real_2
      vector_times_real%y = vec_1%y * real_2
      vector_times_real%z = vec_1%z * real_2
      RETURN
   END FUNCTION vector_times_real

   FUNCTION real_times_vector(real_1, vec_2)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine allows for a vector to be multiplied by a
     ! scalar (real) value.

     ! METHODOLOGY EMPLOYED:
     ! Uses the multiplication operator (*) to allow for this
     ! operation.  As in:
     ! Vector=Real_Value*Vector1

      TYPE (vector) :: real_times_vector
!      REAL, INTENT(IN) :: real_1
      REAL(r64), INTENT(IN) :: real_1
      TYPE (vector), INTENT(IN) :: vec_2
      real_times_vector%x = real_1 * vec_2%x
      real_times_vector%y = real_1 * vec_2%y
      real_times_vector%z = real_1 * vec_2%z
      RETURN
   END FUNCTION real_times_vector

   FUNCTION vector_times_int(vec_1, int_2)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine allows for a vector to be multiplied by a
     ! scalar (integer) value.

     ! METHODOLOGY EMPLOYED:
     ! Uses the multiplication operator (*) to allow for this
     ! operation.  As in:
     ! Vector=Vector1*Integer_Value

      TYPE (vector) :: vector_times_int
      TYPE (vector), INTENT(IN) :: vec_1
      INTEGER, INTENT(IN) :: int_2
      vector_times_int%x = vec_1%x * REAL(int_2,r64)
      vector_times_int%y = vec_1%y * REAL(int_2,r64)
      vector_times_int%z = vec_1%z * REAL(int_2,r64)
      RETURN
   END FUNCTION vector_times_int

   FUNCTION int_times_vector(int_1, vec_2)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine allows for a vector to be multiplied by a
     ! scalar (integer) value.

     ! METHODOLOGY EMPLOYED:
     ! Uses the multiplication operator (*) to allow for this
     ! operation.  As in:
     ! Vector=Integer_Value*Vector1

      TYPE (vector) :: int_times_vector
      INTEGER, INTENT(IN) :: int_1
      TYPE (vector), INTENT(IN) :: vec_2
      int_times_vector%x = REAL(int_1,r64) * vec_2%x
      int_times_vector%y = REAL(int_1,r64) * vec_2%y
      int_times_vector%z = REAL(int_1,r64) * vec_2%z
      RETURN
   END FUNCTION int_times_vector

   FUNCTION vector_div_real(vec_1, real_2)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine allows for a vector to be divided by a
     ! scalar (real) value.

     ! METHODOLOGY EMPLOYED:
     ! Uses the division operator (/) to allow for this
     ! operation.  As in:
     ! Vector=Vector1/Real_Value

      TYPE (vector) :: vector_div_real
      TYPE (vector), INTENT(IN) :: vec_1
!      REAL, INTENT(IN) :: real_2
      REAL(r64), INTENT(IN) :: real_2
      vector_div_real%x = vec_1%x / real_2
      vector_div_real%y = vec_1%y / real_2
      vector_div_real%z = vec_1%z / real_2
      RETURN
   END FUNCTION vector_div_real

   FUNCTION vector_div_int(vec_1, int_2)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine allows for a vector to be divided by a
     ! scalar (integer) value.

     ! METHODOLOGY EMPLOYED:
     ! Uses the division operator (/) to allow for this
     ! operation.  As in:
     ! Vector=Vector1/Integer_Value

      TYPE (vector) :: vector_div_int
      TYPE (vector), INTENT(IN) :: vec_1
      INTEGER, INTENT(IN) :: int_2
      vector_div_int%x = vec_1%x / REAL(int_2,r64)
      vector_div_int%y = vec_1%y / REAL(int_2,r64)
      vector_div_int%z = vec_1%z / REAL(int_2,r64)
      RETURN
   END FUNCTION vector_div_int

   FUNCTION vec_dot_product(vec_1, vec_2)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine allows for the dot product of two vectors.

     ! METHODOLOGY EMPLOYED:
     ! Uses a special operator (.dot.) to allow for this
     ! operation.  As in:
     ! Vector=Vector1.dot.Vector2

      REAL(r64) :: vec_dot_product
      TYPE (vector), INTENT(IN) :: vec_1, vec_2
      vec_dot_product = vec_1%x*vec_2%x + vec_1%y*vec_2%y &
                  + vec_1%z*vec_2%z
      RETURN
   END FUNCTION vec_dot_product

   FUNCTION vec2d_dot_product(vec_1, vec_2)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine allows for the dot product of two vectors.

     ! METHODOLOGY EMPLOYED:
     ! Uses a special operator (.dot.) to allow for this
     ! operation.  As in:
     ! Vector=Vector1.dot.Vector2

      REAL(r64) :: vec2d_dot_product
      TYPE (vector_2d), INTENT(IN) :: vec_1, vec_2
      vec2d_dot_product = vec_1%x*vec_2%x + vec_1%y*vec_2%y
      RETURN
   END FUNCTION vec2d_dot_product

   FUNCTION vec_cross_product(vec_1, vec_2)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine allows for the cross product of two vectors.

     ! METHODOLOGY EMPLOYED:
     ! Uses the multiplication operator (*) to allow for this
     ! operation.  As in:
     ! Vector=Vector1*Vector2

      TYPE (vector) :: vec_cross_product
      TYPE (vector), INTENT(IN) :: vec_1, vec_2
      vec_cross_product%x = vec_1%y*vec_2%z - vec_1%z*vec_2%y
      vec_cross_product%y = vec_1%z*vec_2%x - vec_1%x*vec_2%z
      vec_cross_product%z = vec_1%x*vec_2%y - vec_1%y*vec_2%x
      RETURN
   END FUNCTION vec_cross_product

   FUNCTION vec2d_cross_product(vec_1, vec_2)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine allows for the cross product of two 2d vectors.

     ! METHODOLOGY EMPLOYED:
     ! Uses the special operator (.2dcross.) to allow for this
     ! operation.  As in:
     ! Vector=Vector1 .2dcross. Vector2

      REAL(r64) :: vec2d_cross_product
      TYPE (vector_2d), INTENT(IN) :: vec_1, vec_2
      vec2d_cross_product = vec_1%x*vec_2%y - vec_2%x*vec_1%y
      RETURN
   END FUNCTION vec2d_cross_product

!**** Remainder of routines are called from other parts using their names.
   FUNCTION distance(vec_1, vec_2) RESULT(rdistance)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine calculates the distance between two vectors (points in 3D space).

     ! METHODOLOGY EMPLOYED:
     ! Standard Function operation
     ! Dist_Calc=Distance(Vector1,Vector2)

   IMPLICIT NONE

     ! SUBROUTINE ARGUMENT DEFINITIONS:
      TYPE (vector), INTENT(IN) :: vec_1, vec_2
      REAL(r64) :: rdistance

     ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
      REAL(r64) :: dx, dy, dz

      dx=vec_1%x-vec_2%x
      dy=vec_1%y-vec_2%y
      dz=vec_1%z-vec_2%z

      rdistance=sqrt((dx*dx)+(dy*dy)+(dz*dz))

      RETURN
   END FUNCTION distance

   FUNCTION AreaPolygon (n, p) RESULT(areap)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine calculates the area of a polygon defined by the
     ! input vectors.

     ! REFERENCE:
     ! Graphic Gems.

   implicit none

     ! SUBROUTINE ARGUMENT DEFINITIONS:
   integer n
   type (vector) :: p(0:n-1)
   real(r64) :: areap

     ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   type (vector) :: edge0, edge1, nor, edgex
   integer i
   type (vector) :: csum

     edge0=p(1)-p(0)
     edge1=p(2)-p(0)

     edgex=edge0*edge1
     nor =VecNormalize(edge0*edge1)

    !  Initialize csum
     csum=0.0d0

     do i=0,n-2
       csum=csum+p(i)*p(i+1)
     enddo
     csum=csum+p(n-1)*p(0)

     areap=0.5d0*abs(nor.dot.csum)

     return
   END FUNCTION AreaPolygon


   FUNCTION VecSquaredLength(vec) RESULT(vecsqlen)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine calculates the squared length of the input vector.

     ! REFERENCE:
     ! Graphic Gems.

   IMPLICIT NONE

     ! SUBROUTINE ARGUMENT DEFINITIONS:
   type(vector) :: vec
   real(r64) :: vecsqlen

     ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
     ! na

     vecsqlen=(vec%x*vec%x+vec%y*vec%y+vec%z*vec%z)

     return
   END FUNCTION VecSquaredLength

   FUNCTION VecLength(vec) RESULT(veclen)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine calculates the length of the input vector.

     ! REFERENCE:
     ! Graphic Gems.

   IMPLICIT NONE

     ! SUBROUTINE ARGUMENT DEFINITIONS:
   type (vector) :: vec
   real(r64) :: veclen

     ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
     ! na

     veclen=SQRT(VecSquaredLength(vec))

     return
   END FUNCTION VecLength

   FUNCTION VecNegate(vec)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine negates the input vector and returns that vector.

     ! REFERENCE:
     ! Graphic Gems.

   IMPLICIT NONE

     ! SUBROUTINE ARGUMENT DEFINITIONS:
   type(vector) vec
   type(vector) VecNegate

     ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
     ! na

     VecNegate%x=-vec%x
     VecNegate%y=-vec%y
     VecNegate%z=-vec%z

     return
   END FUNCTION VecNegate

   FUNCTION VecNormalize(vec)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine normalizes the input vector and returns the normalized
     ! vector

     ! REFERENCE:
     ! Graphic Gems.

   IMPLICIT NONE

     ! SUBROUTINE ARGUMENT DEFINITIONS:
   type (vector) VecNormalize
   type (vector) vec

     ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
   real(r64) :: veclen

     veclen=VecLength(vec)
     if (veclen /= 0.0d0) then
       VecNormalize%x=vec%x/veclen
       VecNormalize%y=vec%y/veclen
       VecNormalize%z=vec%z/veclen
     else
       VecNormalize%x=0.0d0
       VecNormalize%y=0.0d0
       VecNormalize%z=0.0d0
     endif

   return
   END FUNCTION VecNormalize

   SUBROUTINE VecRound(vec,roundto)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine rounds the input vector to a specified "rounding" value.

     ! REFERENCE:
     ! Graphic Gems.

   IMPLICIT NONE

     ! SUBROUTINE ARGUMENT DEFINITIONS:
   type (vector) vec
   real(r64) :: roundto

     ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
     ! na

     vec%x=ANINT(vec%x*roundto,r64)/roundto
     vec%y=ANINT(vec%y*roundto,r64)/roundto
     vec%z=ANINT(vec%z*roundto,r64)/roundto

   return
   END SUBROUTINE VecRound

   Subroutine DetermineAzimuthAndTilt(Surf,NSides,Azimuth,Tilt,lcsx,lcsy,lcsz,surfaceArea,NewellSurfaceNormalVector)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine determines the Azimuth (outward normal) angle,
     ! Tilt angle of a given surface defined by the set of input vectors.

     ! REFERENCE:
     ! Discussions and examples from Bill Carroll, LBNL.

   IMPLICIT NONE

     ! SUBROUTINE ARGUMENT DEFINITIONS:
   type (vector), dimension(:) :: Surf  ! Surface Definition
   integer, intent(in) :: NSides        ! Number of sides to surface
   real(r64), intent(out)   :: Azimuth       ! Outward Normal Azimuth Angle
   real(r64), intent(out)   :: Tilt          ! Tilt angle of surface
   type (vector) :: lcsx
   type (vector) :: lcsy
   type (vector) :: lcsz
   real(r64), intent(in) :: surfaceArea
   type (vector), intent(in) :: NewellSurfaceNormalVector

   character(len=*), parameter :: fmt3="(A,3(1x,f18.13))"

     ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!  type(vector) :: x3,y3,z3,v12
  type(vector) :: x2
   type(vector) :: x3a,v12a
  type(vector) :: v0
  type(vector) :: v1
  type(vector) :: v2
  type(vector) :: cs3_2
  type(vector) :: cs3_0
  type(vector) :: cs3_1
!  type(vector) :: y2
  type(vector) :: z3
  real(r64) costheta
  real(r64) rotang_0
!  real(r64) rotang_2

   real(r64) az
!   real(r64) azm
   real(r64) tlt
!  real(r64) newtlt
!  real(r64) roundval
!   real(r64) xcomp
!   real(r64) ycomp
!   real(r64) zcomp
!   real(r64) proj
!   integer :: scount
!   integer :: nvert1
!  real(r64) :: tltcos

!!!     x3=VecNormalize(Surf(2)-Surf(1))
!!!     v12=Surf(3)-Surf(2)
!!!
!!!     z3=VecNormalize(x3*v12)
!!!     y3=z3*x3
!!!     roundval=10000.d0
!!!     CALL VecRound(x3,roundval)
!!!     CALL VecRound(y3,roundval)
!!!     CALL VecRound(z3,roundval)
!!!
!!!!  Direction cosines, local coordinates.
!!!!      write(OUTPUT,*) 'lcs:'
!!!!      write(OUTPUT,*) 'x=',x3
!!!!      write(OUTPUT,*) 'y=',y3
!!!!      write(OUTPUT,*) 'z=',z3
     x3a=VecNormalize(Surf(3)-Surf(2))
     v12a=Surf(1)-Surf(2)

!!!     lcsx=x3a
!!!     lcsz=VecNormalize(x3a*v12a)
!!!     lcsy=lcsz*x3a

     lcsx=x3a
     lcsz=NewellSurfaceNormalVector
     lcsy=lcsz*x3a

!!!

!    Vec3d    v0(p1 - p0);  ! BGL has different conventions...p0=surf(2), etc
   v0=Surf(3)-Surf(2)
!    Vec3d    v1(p2 - p0);
   v1=Surf(1)-Surf(2)

!    Vec3d    v2 = cross(v0,v1);
   v2=v0*v1
!    cs3[2] = norm(v2); // z
   cs3_2=VecNormalize(v2)
!    cs3[0] = norm(v0); // x
   cs3_0=VecNormalize(v0)
!    cs3[1] = cross(cs3[2],cs3[0]); // y
   cs3_1=cs3_2*cs3_0
!    Vec3d    z3 = cs3[2];
   z3=cs3_2
!    double costheta = dot(z3,Ref_CS[2]);
   costheta = z3 .dot. zunit

!    if ( fabs(costheta) < 1.0d0) { // normal cases
   if (abs(costheta) < 1.0d0) then
!    // azimuth
!    Vec3d    x2 = cross(Ref_CS[2],z3); // order is important; x2 = x1
!    RotAng[0] = atan2(dot(x2,Ref_CS[1]),dot(x2,Ref_CS[0]));
     x2= zunit*z3
     rotang_0 = atan2(x2 .dot. yunit, x2 .dot. xunit)

  else

!    }
!    else { // special cases: tilt angle theta = 0, PI
!      // azimuth
!      RotAng[0] = atan2(dot(cs3[0],Ref_CS[1]),dot(cs3[0],Ref_CS[0]) );
      rotang_0=atan2(cs3_0 .dot. yunit, cs3_0 .dot. xunit)
!
     endif

   tlt=acos(NewellSurfaceNormalVector%z)
   tlt=tlt/degtoradians

     az=rotang_0

     az=az/degtoradians
     az=MOD(450.d0 - az, 360.d0)
     az=az+90.d0
     if (az < 0.0d0) az=az+360.d0
     az=mod(az,360.d0)

     ! Normalize the azimuth angle so it is positive
     if (abs(az-360.d0) < 1.d-3) az=0.0d0
     Azimuth=az
     Tilt=tlt

     RETURN
   END Subroutine DetermineAzimuthAndTilt

   SUBROUTINE PlaneEquation(verts,nverts,plane,error)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine calculates the plane equation for a given
     ! surface (which should be planar).

     ! REFERENCE:
     ! Graphic Gems

   IMPLICIT NONE

     ! SUBROUTINE ARGUMENT DEFINITIONS:
     integer :: nverts       ! Number of vertices in the surface
     type(vector),target :: verts(0:)  ! Structure of the surface !Objexx:Arg Changed verts(0:nverts-1) to assumed shape for compatibility with passed allocatable
     type(planeeq) :: plane  ! Equation of plane from inputs
     logical :: error ! returns true for degenerate surface

     ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
     type(vector) :: normal
     INTEGER :: i
     type(vector) :: refpt
     real(r64) :: lenvec
     type(vector),POINTER :: u,v

   ! - - - begin - - -
     normal=vector(0.0d0,0.0d0,0.0d0)
     refpt=vector(0.0d0,0.0d0,0.0d0)
     DO i = 0,nverts-1
       u => verts(i)
       if (i < nverts-1) then
         v => verts(i+1)
       else
         v => verts(0)
       endif
       normal%x=normal%x + (u%y-v%y)*(u%z+v%z)
       normal%y=normal%y + (u%z-v%z)*(u%x+v%x)
       normal%z=normal%z + (u%x-v%x)*(u%y+v%y)
       refpt=refpt+u
     END DO
     ! /* normalize the polygon normal to obtain the first
     !    three coefficients of the plane equation
     ! */
     lenvec=VecLength(normal)
     error=.false.
     if (lenvec /= 0.0d0) then   ! should this be >0
       plane%x = normal%x / lenvec
       plane%y = normal%y / lenvec
       plane%z = normal%z / lenvec
       ! /* compute the last coefficient of the plane equation */
       lenvec=lenvec * nverts
       plane%w = -(refpt.dot.normal) / lenvec
     else
       error=.true.
     endif

   END SUBROUTINE PlaneEquation

   FUNCTION Pt2Plane(pt,pleq)  RESULT (PtDist)

     ! PURPOSE OF THIS SUBROUTINE:
     ! This subroutine calculates the distance from a point
     ! to the plane (of a surface).  Used to determine the reveal
     ! of a heat transfer subsurface.

     ! REFERENCE:
     ! Graphic Gems

   IMPLICIT NONE

     ! SUBROUTINE ARGUMENT DEFINITIONS:
     REAL(r64) :: PtDist     ! Distance of the point to the plane
     TYPE (PlaneEq) :: pleq  ! Equation of the plane
     TYPE (Vector) :: pt     ! Point for determining the distance

     ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
     ! na

     PtDist = (pleq%x*pt%x) + (pleq%y*pt%y) + (pleq%z*pt%z) + pleq%w

     RETURN

   END FUNCTION Pt2Plane

   SUBROUTINE CreateNewellAreaVector(VList,NSides,OutNewellAreaVector)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine creates a "Newell" vector from the vector list for a surface
          ! face.  Also the Newell Area vector.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! Collaboration with Bill Carroll, LBNL.

          ! USE STATEMENTS:
          ! na

     IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
     TYPE (Vector), DIMENSION(:), INTENT(IN) :: VList
     INTEGER, INTENT(IN) :: NSides
     TYPE (Vector), INTENT(INOUT) :: OutNewellAreaVector

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
     TYPE (Vector) :: V1
     TYPE (Vector) :: V2
     INTEGER Vert

     OutNewellAreaVector=0.0d0

     V1=VList(2)-Vlist(1)
     DO Vert=3,NSides
       V2=VList(Vert)-VList(1)
       OutNewellAreaVector=OutNewellAreaVector+V1*V2
       V1=V2
     ENDDO
!     do vert=1,nsides
!       write(outputfiledebug,*) vlist(vert)
!     enddo

     OutNewellAreaVector=OutNewellAreaVector/2.d0

     RETURN

   END SUBROUTINE CreateNewellAreaVector

   SUBROUTINE CreateNewellSurfaceNormalVector(VList,NSides,OutNewellSurfaceNormalVector)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   Jan 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine creates a "Newell" surface normal vector from the vector list
          ! for a surface face.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! September 2010: from OpenGL.org
          ! Begin Function CalculateSurfaceNormal (Input Polygon) Returns Vector
          !
          !    Set Vertex Normal to (0, 0, 0)
          !
          !    Begin Cycle for Index in [0, Polygon.vertexNumber)
          !
          !       Set Vertex Current to Polygon.verts[Index]
          !       Set Vertex Next    to Polygon.verts[(Index plus 1) mod Polygon.vertexNumber]
          !
          !       Set Normal.x to Sum of Normal.x and (multiply (Current.y minus Next.y) by (Current.z plus Next.z)
          !       Set Normal.y to Sum of Normal.y and (multiply (Current.z minus Next.z) by (Current.x plus Next.x)
          !       Set Normal.z to Sum of Normal.z and (multiply (Current.x minus Next.x) by (Current.y plus Next.y)
          !
          !    End Cycle
          !
          !    Returning Normalize(Normal)
          !
          ! End Function

          ! USE STATEMENTS:
          ! na

     IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
     TYPE (Vector), DIMENSION(:), INTENT(IN) :: VList
     INTEGER, INTENT(IN) :: NSides
     TYPE (Vector), INTENT(INOUT) :: OutNewellSurfaceNormalVector

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!     TYPE (Vector) :: U
!     TYPE (Vector) :: V
     INTEGER Side
     INTEGER curVert
     INTEGER nextVert
     real(r64) :: xvalue
     real(r64) :: yvalue
     real(r64) :: zvalue

     OutNewellSurfaceNormalVector=0.0d0
     xvalue=0.0d0
     yvalue=0.0d0
     zvalue=0.0d0

!     IF (NSides > 3) THEN
       DO Side=1,NSides
         curVert=Side
         nextVert=Side+1
         if (nextVert > NSides) nextVert=1
         xvalue=xvalue+ (VList(curVert)%y-VList(nextVert)%y) * (VList(curVert)%z+VList(nextVert)%z)
         yvalue=yvalue+ (VList(curVert)%z-VList(nextVert)%z) * (VList(curVert)%x+VList(nextVert)%x)
         zvalue=zvalue+ (VList(curVert)%x-VList(nextVert)%x) * (VList(curVert)%y+VList(nextVert)%y)
       ENDDO
!     ELSE  ! Triangle
!       U=VList(2)-VList(1)
!       V=VList(3)-VList(1)
!       xvalue=(U%y*V%z)-(U%z*V%y)
!       yvalue=(U%z*V%x)-(U%x*V%z)
!       zvalue=(U%x*V%y)-(U%y*V%x)
!     ENDIF

     OutNewellSurfaceNormalVector%x=xvalue
     OutNewellSurfaceNormalVector%y=yvalue
     OutNewellSurfaceNormalVector%z=zvalue
     OutNewellSurfaceNormalVector=VecNormalize(OutNewellSurfaceNormalVector)

     RETURN

   END SUBROUTINE CreateNewellSurfaceNormalVector

   SUBROUTINE CompareTwoVectors(vector1,vector2,areSame,tolerance)

             ! SUBROUTINE INFORMATION:
             !       AUTHOR         Linda Lawrie
             !       DATE WRITTEN   February 2012
             !       MODIFIED       na
             !       RE-ENGINEERED  na

             ! PURPOSE OF THIS SUBROUTINE:
             ! This routine will provide the ability to compare two vectors (e.g. surface normals)
             ! to be the same within a specified tolerance.

             ! METHODOLOGY EMPLOYED:
             ! compare each element (x,y,z)

             ! REFERENCES:
             ! na

             ! USE STATEMENTS:
             ! na

     IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

             ! SUBROUTINE ARGUMENT DEFINITIONS:
     TYPE(vector), INTENT(IN) :: vector1   ! standard vector
     TYPE(vector), INTENT(IN) :: vector2   ! standard vector
     LOGICAL, INTENT(INOUT)   :: areSame   ! true if the two vectors are the same within specified tolerance
     REAL(r64), INTENT(IN)    :: tolerance ! specified tolerance

             ! SUBROUTINE PARAMETER DEFINITIONS:
             ! na

             ! INTERFACE BLOCK SPECIFICATIONS:
             ! na

             ! DERIVED TYPE DEFINITIONS:
             ! na

             ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
             ! na
     areSame=.true.
     IF (ABS(vector1%x-vector2%x) > tolerance) areSame=.false.
     IF (ABS(vector1%y-vector2%y) > tolerance) areSame=.false.
     IF (ABS(vector1%z-vector2%z) > tolerance) areSame=.false.

     RETURN

   END SUBROUTINE CompareTwoVectors

   SUBROUTINE CalcCoPlanarNess(Surf,NSides,IsCoPlanar,MaxDist,ErrorVertex)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   June 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine provides the calculation to determine if the
          ! surface is planar or not.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! Eric W. Weisstein. "Coplanar." From MathWorld--A Wolfram Web Resource.
          !   http://mathworld.wolfram.com/Coplanar.html

          ! USE STATEMENTS:
          ! na

     IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
     type (vector), dimension(:) :: Surf
     integer, intent(in)         :: Nsides
     logical, intent(out)        :: IsCoPlanar
     real(r64), intent(out)           :: MaxDist
     integer, intent(out)        :: ErrorVertex

          ! SUBROUTINE PARAMETER DEFINITIONS:
     real(r64), PARAMETER :: DistTooSmall=1.d-4

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
     integer vert
     type (planeeq) NewellPlane
     logical plerror
     real(r64) dist

     IsCoPlanar=.true.
     MaxDist=0.0d0
     ErrorVertex=0

     ! Use first three to determine plane
     CALL PlaneEquation(Surf,Nsides,NewellPlane,plerror)

     do vert=1,Nsides
       dist=Pt2Plane(surf(vert),NewellPlane)
       if (abs(dist) > MaxDist) then
         MaxDist=ABS(dist)
         ErrorVertex=vert
       endif
     enddo

     if (abs(MaxDist) > DistTooSmall) IsCoPlanar=.false.

     RETURN

   END SUBROUTINE CalcCoPlanarNess

   SUBROUTINE CalcPolyhedronVolume(Poly,Volume)

             ! SUBROUTINE INFORMATION:
             !       AUTHOR         Linda Lawrie
             !       DATE WRITTEN   June 2004
             !       MODIFIED       na
             !       RE-ENGINEERED  na

             ! PURPOSE OF THIS SUBROUTINE:
             ! This subroutine provides the volume calculation for a polyhedron
             ! (i.e. Zone).

             ! METHODOLOGY EMPLOYED:
             ! na

             ! REFERENCES:
             ! Conversations with Bill Carroll, LBNL.

             ! USE STATEMENTS:
             ! na

     IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

             ! SUBROUTINE ARGUMENT DEFINITIONS:
     TYPE (polyhedron) :: Poly
     real(r64), INTENT(OUT) :: Volume

             ! SUBROUTINE PARAMETER DEFINITIONS:
             ! na

             ! INTERFACE BLOCK SPECIFICATIONS
             ! na

             ! DERIVED TYPE DEFINITIONS
             ! na

             ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
     TYPE (vector) :: p0=vector(0.0d0,0.0d0,0.0d0)
     TYPE (vector) :: p3FaceOrigin
     INTEGER :: NFace
     real(r64) PyramidVolume

     Volume=0.0d0

     DO NFace=1,Poly%NumSurfaceFaces
       p3FaceOrigin=Poly%SurfaceFace(NFace)%FacePoints(2)
       PyramidVolume=Poly%SurfaceFace(NFace)%NewellAreaVector .dot. (p3FaceOrigin - p0)
       Volume=Volume+PyramidVolume/3.d0
     ENDDO

     RETURN

   END SUBROUTINE CalcPolyhedronVolume

END MODULE vectors

Module DXFEarClipping

          ! Module containing the routines dealing with triangulating a polygon of >4 sides

          ! Module information:
          !       Author         Linda Lawrie
          !       Date written   October 2005
          !       Modified       na
          !       Re-engineered  na

          ! Purpose of this module:
          ! This module provides the techniques and back up procedures for producing a triangulated
          ! polygon from a >4 sided figure.  It is only used for DXF output.

          ! Methodology employed:
          ! Ear clipping has turned out to be the simplest, most robust technique.

          ! References:
          ! na

          ! Other notes:
          ! na

          ! Use statements:
use DataPrecisionGlobals
use DataVectorTypes
use DataGlobals, ONLY: Pi,DegToRadians,outputfiledebug

implicit none ! Enforce explicit typing of all variables

Private ! Everything private unless explicitly made public

          ! Module parameter definitions:
real(r64), parameter :: twopi=Pi*2.0D0  !6.283185307179586476925287D0
real(r64), parameter :: radtodeg=1.0D0/DegToRadians !57.2957795

          ! Derived type definitions:
          ! na

          ! Module variable declarations:
          ! na
logical :: trackit=.false.
          ! Subroutine specifications for module <module_name>:
PUBLIC  Triangulate

! rest of routines are private.

contains

Logical Function InPolygon(point, poly, nsides)
! this routine is not used in the current scheme
implicit none

!'Return TRUE if the point (xp,yp) lies inside the circumcircle
!'made up by points (x1,y1) (x2,y2) (x3,y3)
!'The circumcircle centre is returned in (xc,yc) and the radius r
!'NOTE: A point on the edge is inside the circumcircle
integer nsides
type(vector) :: point
type(vector), dimension(nsides) :: poly

real(r64), parameter :: epsilon=0.0000001d0
real(r64) :: anglesum
real(r64) :: costheta
integer :: vert
type(vector) :: p1
type(vector) :: p2
real(r64) :: m1
real(r64) :: m2
real(r64) :: acosval

InPolygon = .False.

anglesum=0.0d0

do vert=1,nsides-1

  p1%x = poly(vert)%x - point%x
  p1%y = poly(vert)%y - point%y
  p1%z = poly(vert)%z - point%z

  p2%x = poly(vert+1)%x - point%x
  p2%y = poly(vert+1)%y - point%y
  p2%z = poly(vert+1)%z - point%z

  m1=modulus(p1)
  m2=modulus(p2)

  if (m1*m2 <= epsilon) then
    InPolygon = .true.
    exit
  else
    costheta = (p1%x*p2%x + p1%y*p2%y + p1%z*p2%z) / (m1*m2)
    acosval=acos(costheta)
    anglesum=anglesum+acosval
  endif
enddo

if (abs(anglesum-twopi) <= epsilon) then
  InPolygon=.true.
endif

return

End Function InPolygon

Function Modulus(point) RESULT(rModulus)
! this routine is not used in the current scheme

implicit none

type (vector) point
real(r64) :: rModulus

rModulus=SQRT(point%x*point%x + point%y*point%y + point%z*point%z)

End Function Modulus

Integer Function Triangulate(nsides,polygon,outtriangles,surfazimuth,surftilt,surfname,surfclass)

          ! Subroutine information:
          !       Author         Linda Lawrie
          !       Date written   October 2005
          !       Modified       na
          !       Re-engineered  na

          ! Purpose of this subroutine:
          ! This routine is a self-contained triangulation calculation from a polygon
          ! of 3D vertices, nsides, to a returned set (as possible) of triangles -- noted
          ! by vertex numbers.

          ! Methodology employed:
          ! <Description>

          ! References:
          ! na

          ! Use statements:
  USE DataInterfaces, ONLY: ShowWarningError,ShowMessage,ShowContinueError
  USE DataGlobals, ONLY: DisplayExtraWarnings
  USE DataSurfaces, ONLY: cSurfaceClass,SurfaceClass_Floor,SurfaceClass_Roof,SurfaceClass_Overhang
  USE General, ONLY: RoundSigDigits

  implicit none ! Enforce explicit typing of all variables in this routine

          ! Subroutine argument definitions:
  integer nsides                ! number of sides to polygon
  type (vector), dimension(nsides) :: polygon
  type (dTriangle), allocatable, dimension(:) :: outtriangles
  real(r64) :: surftilt              ! surface tilt angle
  real(r64) :: surfazimuth           ! surface azimuth angle (outward facing normal)
  character(len=*) :: surfname  ! surface name (for error messages)
  integer :: surfclass ! surface class

          ! Subroutine parameter definitions:
  real(r64) , parameter :: point_tolerance=.00001d0
  real(r64) , parameter :: twopiang=(180.d0/radtodeg)

          ! Interface block specifications:
          ! na

          ! Derived type definitions:
          ! na

          ! Subroutine local variable declarations:
  logical          :: errflag
  integer ears(nsides)
  integer r_angles(nsides)
  real(r64)  rangles(nsides)
  integer c_vertices(nsides)
  integer earvert(3,nsides)
  logical removed(nsides)
  type(Vector_2d), dimension(nsides) :: vertex
!unused  type(Vector_2d), dimension(3) :: testtri
!unused  type(Vector_2d) :: point
  integer earverts(3)
  real(r64) xvt(nsides)
  real(r64) yvt(nsides)
  real(r64) zvt(nsides)
  type (dTriangle), dimension(nsides) :: Triangle

  !'General Variables
  integer i
  integer j
!unused  integer k
  integer ntri
!unused  logical inpoly
  integer nvertcur
  integer ncount
  integer svert
  integer mvert
  integer evert
!unused  integer tvert
  integer nears
  integer nrangles
  integer ncverts
!unused  double precision :: ang
!unused  double precision :: val
  character(len=200) :: line
  integer, save :: errcount=0

  errflag=.false.
!  vertex=polygon
!  if (surfname == 'BOTTOM:OFFICE_E_3') THEN
!    trackit=.true.
!  else
!    trackit=.false.
!  endif
  if (surfclass == SurfaceClass_Floor .or. surfclass == SurfaceClass_Roof .or. &
      surfclass == SurfaceClass_Overhang) then
    CALL CalcRfFlrCoordinateTransformation(nsides,polygon,surfazimuth,surftilt,xvt,yvt,zvt)
    do svert=1,nsides
      do mvert=svert+1,nsides
        if (abs(xvt(svert)-xvt(mvert)) <= point_tolerance) xvt(svert)=xvt(mvert)
        if (abs(zvt(svert)-zvt(mvert)) <= point_tolerance) zvt(svert)=zvt(mvert)
      enddo
    enddo
    do svert=1,nsides
      vertex(svert)%x=xvt(svert)
      vertex(svert)%y=zvt(svert)
!      if (trackit) write(outputfiledebug,*) 'x=',xvt(svert),' y=',zvt(svert)
    enddo
  else
    CALL CalcWallCoordinateTransformation(nsides,polygon,surfazimuth,surftilt,xvt,yvt,zvt)
    do svert=1,nsides
      do mvert=svert+1,nsides
        if (abs(xvt(svert)-xvt(mvert)) <= point_tolerance) xvt(svert)=xvt(mvert)
        if (abs(zvt(svert)-zvt(mvert)) <= point_tolerance) zvt(svert)=zvt(mvert)
      enddo
    enddo
    do svert=1,nsides
      vertex(svert)%x=xvt(svert)
      vertex(svert)%y=zvt(svert)
    enddo
  endif

! find ears
  nvertcur=nsides
  ncount=0
  svert=1
  mvert=2
  evert=3
  removed=.false.
  do while(nvertcur > 3)
    call generate_ears(nsides, vertex, ears, nears, r_angles, nrangles, c_vertices, ncverts, removed, earverts,rangles)
    if (.not. any(ears > 0)) then
      call showwarningerror('DXFOut: Could not triangulate surface="'//trim(surfname)//  &
               '", type="'//trim(cSurfaceClass(surfclass))//'", check surface vertex order(entry)')
      errcount=errcount+1
      if (errcount == 1 .and. .not. DisplayExtraWarnings) then
        call showcontinueerror('...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual surfaces.')
      endif
      if (DisplayExtraWarnings) then
        write(line,*) 'surface=',trim(surfname),' class=',trim(cSurfaceClass(surfclass))
        call showmessage(trim(line))
        do j=1,nsides
!          write(line,"(' side=',i2,' (',2(f6.1,','),f6.1,')')") j,polygon(j)
          line=' side='//trim(roundsigdigits(j))//' ('//trim(roundsigdigits(polygon(j)%x,1))//','//  &
            trim(roundsigdigits(polygon(j)%y,1))//','//trim(roundsigdigits(polygon(j)%z,1))//')'
          call showmessage(trim(line))
        enddo
        write(line,*) 'number of triangles found=',ncount
        call showmessage(trim(line))
        do j=1,nrangles
!          write(line,"(' r angle=',i2,' vert=',i2,' deg=',f6.1)") j,r_angles(j),rangles(j)*radtodeg
          line=' r angle='//trim(roundsigdigits(j))//' vert='//trim(roundsigdigits(r_angles(j)))//' deg='//  &
            trim(roundsigdigits(rangles(j)*radtodeg,1))
          call showmessage(trim(line))
        enddo
      endif
      exit  ! while loop
    endif
    if (nears > 0) then
      svert=earverts(1)
      mvert=earverts(2)
      evert=earverts(3)
      ! remove ear
      ncount=ncount+1
      removed(mvert)=.true.
      earvert(1,ncount)=svert
      earvert(2,ncount)=mvert
      earvert(3,ncount)=evert
      nvertcur=nvertcur-1
    endif
    if (nvertcur == 3) then
      j=1
      ncount=ncount+1
      do i=1,nsides
        if (removed(i)) cycle
        earvert(j,ncount)=i
        j=j+1
      enddo
    endif
  enddo

  ntri = ncount

  do i=1,ntri
    Triangle(i)%vv0=earvert(1,i)
    Triangle(i)%vv1=earvert(2,i)
    Triangle(i)%vv2=earvert(3,i)
  enddo

  allocate(outtriangles(ntri))
  do i=1,ntri
    outtriangles(i)=triangle(i)
  enddo

  Triangulate = ntri

End Function Triangulate

function angle_2dvector ( xa, ya, xb, yb, xc, yc ) result (angle)

          ! Function information:
          !       Author         Linda Lawrie
          !       Date written   October 2005
          !       Modified       na
          !       Re-engineered  na

          ! Purpose of this function:
          ! This function calculates the angle between two sides of a 2d polygon.
          ! It computes the interior angle in radians at vertex
          ! (XB,YB) of the chain formed by the directed edges from
          ! (XA,YA) to (XB,YB) to (XC,YC).  The interior is to the
          ! left of the two directed edges.

          ! Methodology employed:
          ! <Description>

          ! References:
          ! Geometry Tools for Computer Graphics

          ! Use statements:
          ! na

  Implicit none ! Enforce explicit typing of all variables in this routine

          ! Function argument definitions:
  real(r64) , intent(in)  :: xa  ! vertex coordinate
  real(r64) , intent(in)  :: ya  ! vertex coordinate
  real(r64) , intent(in)  :: xb  ! vertex coordinate
  real(r64) , intent(in)  :: yb  ! vertex coordinate
  real(r64) , intent(in)  :: xc  ! vertex coordinate
  real(r64) , intent(in)  :: yc  ! vertex coordinate
  real(r64)  angle               ! the angle, between 0 and 2*PI.
                                       ! angle is set to PI/2 in the degenerate case.


          ! Function parameter definitions:
  real(r64) , parameter :: epsilon=0.0000001d0

          ! Interface block specifications:
          ! na

          ! Derived type definitions:
          ! na

          ! Function local variable declarations:
  real(r64)  :: t
  real(r64)  :: x1
  real(r64)  :: x2
  real(r64)  :: y1
  real(r64)  :: y2

  x1 = xa - xb
  y1 = ya - yb
  x2 = xc - xb
  y2 = yc - yb

  t = sqrt ( ( x1 * x1 + y1 * y1 ) * ( x2 * x2 + y2 * y2 ) )
  if ( t == 0.0D+00 ) t = 1.0D+00

  t = ( x1 * x2 + y1 * y2 ) / t

  if ( (1.0D+00 - epsilon) < abs ( t ) ) then
    t = sign ( 1.0D+00, t )
  end if

  angle = acos ( t )

  if ( x2 * y1 - y2 * x1 < 0.0D+00 ) then
    angle = 2.0D+00 * pi - angle
  end if

  return
end function angle_2dvector

function polygon_contains_point_2d ( nsides, polygon, point) result(inside)

          ! Function information:
          !       Author         Linda Lawrie
          !       Date written   October 2005
          !       Modified       na
          !       Re-engineered  na

          ! Purpose of this function:
          ! Determine if a point is inside a simple 2d polygon.  For a simple polygon (one whose
          ! boundary never crosses itself).  The polygon does not need to be convex.

          ! Methodology employed:
          ! <Description>

          ! References:
          ! M Shimrat, Position of Point Relative to Polygon, ACM Algorithm 112,
          ! Communications of the ACM, Volume 5, Number 8, page 434, August 1962.

          ! Use statements:
          ! na

  Implicit none ! Enforce explicit typing of all variables in this routine

          ! Function argument definitions:
  integer :: nsides      ! number of sides (vertices)
  type(Vector_2d) :: polygon(nsides)  ! points of polygon
  type(Vector_2d) :: point  ! point to be tested
  logical :: inside  ! return value, true=inside, false = not inside

          ! Function parameter definitions:
  real(r64), parameter :: point_tolerance=.00001d0

          ! Interface block specifications:
          ! na

          ! Derived type definitions:
          ! na

          ! Function local variable declarations:
  integer i
  integer ip1

  inside = .false.

  do i = 1, nsides

    if ( i < nsides ) then
      ip1 = i + 1
    else
      ip1 = 1
    end if

    if ( ( polygon(i)%y   <  point%y .and. point%y <= polygon(ip1)%y   ) .or. &
         ( point%y <= polygon(i)%y   .and. polygon(ip1)%y   < point%y ) ) then
      if ( ( point%x - polygon(i)%x ) - ( point%y - polygon(i)%y ) &
         * ( polygon(ip1)%x - polygon(i)%x ) / ( polygon(ip1)%y - polygon(i)%y ) < 0 ) then
        inside = .not. inside
      end if
    end if

  end do

  return

end function polygon_contains_point_2d

subroutine generate_ears(nvert, vertex, ears, nears, r_vertices, nrverts, c_vertices, ncverts, removed, earvert, rangles)

          ! Subroutine information:
          !       Author         Linda Lawrie
          !       Date written   October 2005
          !       Modified       na
          !       Re-engineered  na

          ! Purpose of this subroutine:
          ! This routine generates "ears", "reflex angles" and "convex angles" of the polygon
          ! based on the method set for in the reference.

          ! Methodology employed:
          ! No elegance used here.  Always starts with first vertex in polygon.

          ! References:
          ! Geometric Tools for Computer Graphics, Philip Schneider, David Eberly. 2003.  Ear
          ! clipping for triangulation is described in Chapter 13 on Polygon Partitioning.  Also
          ! described in a small article "Triangulation by Ear Clipping", David Eberly, http://www.geometrictools.com

          ! Use statements:
          ! na

  implicit none ! Enforce explicit typing of all variables in this routine

          ! Subroutine argument definitions:
  integer :: nvert             ! number of vertices in polygon
  type(Vector_2d), dimension(nvert) :: vertex
  integer :: ears(nvert)       ! number of ears possible (dimensioned to nvert)
  integer :: nears             ! number of ears found
  integer :: r_vertices(nvert) ! number of reflex vertices (>180) possible
  integer :: nrverts           ! number of reflex vertices found (>=180)
  integer :: c_vertices(nvert) ! number of convex vertices
  integer :: ncverts           ! number of convex vertices found (< 180)
  logical :: removed(nvert)    ! array that shows if a vertex has been removed (calling routine)
  integer :: earvert(3)        ! vertex indicators for first ear
  real(r64)    :: rangles(nvert)

          ! Subroutine parameter definitions:
  real(r64),parameter :: twopi_rad=(180.d0/radtodeg)

          ! Interface block specifications:
          ! na

          ! Derived type definitions:
          ! na

          ! Subroutine local variable declarations:
  integer :: svert    ! starting vertex
  integer :: mvert    ! "middle" vertex (this will be an ear, if calculated)
  integer :: evert    ! ending vertex
  real(r64) :: ang  ! ang between
  integer tvert       ! test vertex, intermediate use
  logical inpoly      ! in polygon or not
  type(Vector_2d) point      ! structure for point
  type(Vector_2d) testtri(3) ! structure for triangle
  integer j           ! loop counter

  ! initialize, always recalculate
  ears=0
  r_vertices=0
  rangles=0.0d0
  nears=0
  nrverts=0
  c_vertices=0
  ncverts=0

  do svert=1,nvert
    if (removed(svert)) cycle
    !  have starting vertex.  now need middle and end
    mvert=svert+1
    do j=1,nvert
      if (mvert > nvert) mvert=1
      if (removed(mvert)) then
        mvert=mvert+1
        if (mvert > nvert) mvert=1
      else
        exit
      endif
    enddo
    evert=mvert+1
    do j=1,nvert
      if (evert > nvert) evert=1
      if (removed(evert)) then
        evert=evert+1
        if (evert > nvert) evert=1
      else
        exit
      endif
    enddo

    ! have gotten start, middle and ending vertices.  test for reflex angle

    ang=angle_2dvector(vertex(svert)%x,vertex(svert)%y,vertex(mvert)%x,vertex(mvert)%y,vertex(evert)%x,vertex(evert)%y)

    if (ang > twopi_rad) then  ! sufficiently close to 180 degrees.
      nrverts=nrverts+1
      r_vertices(nrverts)=mvert
      rangles(nrverts)=ang
      cycle
    else
      ncverts=ncverts+1
      c_vertices(ncverts)=mvert
    endif

    ! convex angle, see if it's an ear
    testtri(1)=vertex(svert)
    testtri(2)=vertex(mvert)
    testtri(3)=vertex(evert)
    tvert=evert
    do j=4,nvert
      tvert=tvert+1
      if (tvert > nvert) tvert=1
      if (removed(tvert)) cycle
      point=vertex(tvert)
      inpoly=polygon_contains_point_2d ( 3, testtri, point)
      if (.not. inpoly) cycle
      exit
    enddo
!    if (trackit) then
!      write(outputfiledebug,*) ' triangle=',svert,mvert,evert
!      write(outputfiledebug,*) ' vertex1=',vertex(svert)%x,vertex(svert)%y
!      write(outputfiledebug,*) ' vertex2=',vertex(mvert)%x,vertex(mvert)%y
!      write(outputfiledebug,*) ' vertex3=',vertex(evert)%x,vertex(evert)%y
!      write(outputfiledebug,*) ' inpoly=',inpoly
!    endif
    if (.not. inpoly) then
  ! found an ear
      nears=nears+1
      ears(nears)=mvert
      if (nears == 1) then
        earvert(1)=svert
        earvert(2)=mvert
        earvert(3)=evert
      endif
      if (trackit) then
        write(outputfiledebug,*) 'ear=',nears,' triangle=',svert,mvert,evert
      endif
    endif
  enddo

  return

end subroutine generate_ears

Subroutine CalcWallCoordinateTransformation(nsides,polygon,surfazimuth,surftilt,xvt,yvt,zvt)

          ! Subroutine information:
          !       Author         Linda Lawrie
          !       Date written   October 2005
          !       Modified       na
          !       Re-engineered  na

          ! Purpose of this subroutine:
          ! This routine transforms a "wall" (normally vertical polygon) to a south facing (180 deg outward
          ! normal) polygon in 2 d (y vertices are then ignored).

          ! Methodology employed:
          ! Standard angle rotation

          ! References:
          ! na

          ! Use statements:
          ! na

  implicit none    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  integer :: nsides
  type(vector), dimension(nsides) :: polygon
  real(r64) :: surfazimuth
  real(r64) :: surftilt !unused1208
  real(r64) :: xvt(nsides)
  real(r64) :: yvt(nsides)
  real(r64) :: zvt(nsides)

          ! Subroutine parameter definitions:
          ! na

          ! Interface block specifications
          ! na

          ! Derived type definitions
          ! na

          ! Subroutine local variable declarations:
  integer :: i           ! Loop Control
  real(r64) :: alpha
  real(r64) :: alphrad
  real(r64) :: alpha180

 ! convert surface (wall) to facing 180 (outward normal)

  alpha=surfazimuth

  alpha180=180.d0-alpha  ! amount to rotate
  alphrad=alpha180/radtodeg

  do i=1,nsides
    xvt(i)=cos(alphrad)*polygon(i)%x + sin(alphrad)*polygon(i)%y
    yvt(i)=-sin(alphrad)*polygon(i)%x + cos(alphrad)*polygon(i)%y
    zvt(i)=polygon(i)%z
  enddo

  return

end subroutine CalcWallCoordinateTransformation

Subroutine CalcRfFlrCoordinateTransformation(nsides,polygon,surfazimuth,surftilt,xvt,yvt,zvt)

          ! Subroutine information:
          !       Author         Linda Lawrie
          !       Date written   October 2005
          !       Modified       na
          !       Re-engineered  na

          ! Purpose of this subroutine:
          ! This routine transforms a roof/floor (normally flat polygon) to a flat
          ! polygon in 2 d (z vertices are then ignored).

          ! Methodology employed:
          ! Standard angle rotation

          ! References:
          ! na

          ! Use statements:
          ! na

  implicit none    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  integer :: nsides
  type(vector), dimension(nsides) :: polygon
  real(r64) :: surfazimuth !unused1208
  real(r64) :: surftilt
  real(r64) :: xvt(nsides)
  real(r64) :: yvt(nsides)
  real(r64) :: zvt(nsides)

          ! Subroutine parameter definitions:
          ! na

          ! Interface block specifications
          ! na

          ! Derived type definitions
          ! na

          ! Subroutine local variable declarations:
  integer :: i           ! Loop Control
  real(r64) :: alpha
  real(r64) :: alphrad

  alpha=-surftilt

  alphrad=alpha/radtodeg

  do i=1,nsides
    xvt(i)=polygon(i)%x
    yvt(i)=cos(alphrad)*polygon(i)%x + sin(alphrad)*polygon(i)%y
    zvt(i)=-sin(alphrad)*polygon(i)%x + cos(alphrad)*polygon(i)%y
  enddo

  return

end subroutine CalcRfFlrCoordinateTransformation

subroutine reorder(nvert)
implicit none
integer nvert !unused1208
!type (Vector_2d) nvertex(nvert)
!integer i
!type (Vector_2d) point
!integer nrep

!  Vertex, nverts is in cw order, reorder for calc

!nrep=1
!nvertex(1)=vertex(1)
!do i=nvert,1,-1
!  nvertex(nrep)=vertex(i)
!  nrep=nrep+1
!enddo
!
!do i=1,nvert
!  vertex(i)=nvertex(i)
!enddo

return

end subroutine
End Module DXFEarClipping

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

