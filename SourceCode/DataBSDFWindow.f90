MODULE DataBSDFWindow

          ! Module containing the data definitions dealing with calculating window optical
          ! properties from BSDF data

          ! MODULE INFORMATION:
          !       AUTHOR         Joseph Klems, Brent Griffith
          !       DATE WRITTEN   August 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! define the data structures to be used in calculating solar
          !  transmittance and absorptance and associated arsenal
          !  of geometry and window state information necessary

          ! METHODOLOGY EMPLOYED:
          ! Matrix representation of bidirectional transmittance of radiance

          ! REFERENCES:
          ! to be added--Complex glazing pubs, WINDOW writeup(s)(?)

          ! OTHER NOTES:
          ! see Joe's draft "Including Non-Specular Fenestrations in EnergyPlus"

          ! USE STATEMENTS:
          ! <use statements for data only modules>
USE DataPrecisionGlobals
USE DataVectorTypes
USE DataGlobals, ONLY: MaxNameLength

          ! <use statements for access to subroutines in other modules>

IMPLICIT NONE ! Enforce explicit typing of all variables

PUBLIC ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS:


INTEGER, PARAMETER :: BasisType_WINDOW      = 1
INTEGER, PARAMETER :: BasisType_Custom       = 2

INTEGER, PARAMETER :: BasisSymmetry_Axisymmetric = 1
INTEGER, PARAMETER :: BasisSymmetry_None         = 2

! Thermal calculations for complex fenestration can be used to generate reports for standard cases
! noCondition is used when performing timestep calculations
! summerCondtion will override certain parameters so that produced results are matching standard summer WINDOW (software) results
! winterCondition will override certain parameters so that produced resuls are matching standard winter WINDOW (software) results
INTEGER, PARAMETER :: noCondition = 0
INTEGER, PARAMETER :: summerCondition = 1
INTEGER, PARAMETER :: winterCondition = 2


          ! DERIVED TYPE DEFINITIONS:

TYPE BasisElemDescr
                !The following are in the local coordinate system corresponding to the matrix
  REAL(r64)    :: Theta  !Centroid Theta value
  REAL(r64)    :: Phi    !Centroid Phi value
  REAL(r64)    :: dTheta  !Element width, Theta
  REAL(r64)    :: dPhi    !Element width, Phi
  REAL(r64)    :: UpprTheta  !Patch upper edge, Theta
  REAL(r64)    :: LwrTheta  !Patch lower edge, Theta
  REAL(r64)    :: UpprPhi  !Patch upper edge, Phi
  REAL(r64)    :: LwrPhi  !Patch lower edge, Phi
   !Note: The dimension index of the BasisElementDescription object corresponds to
   !the position (index) of this element in the row or column of property matrix

   !Note:  the following are intended to be used for interpolating directions among basis elements
  INTEGER      :: INNbInL  !Index of inward (lower Theta) neighbor, lower phi
  INTEGER      :: INNbInH  !Index of inward (lower Theta) neighbor, higher phi
  INTEGER      :: INNbOutL  !Index of outward (higher Theta) neighbor, lower phi
  INTEGER      :: INNbOutH  !Index of outward (higher Theta) neighbor, higher phi
  INTEGER      :: INNbLft  !Index of leftward (higher Phi) neighbor (same Theta)
  INTEGER      :: INNbRt  !Index of rightward (lower Phi) neighbor (same Theta)
              !These indices are in the BasisElement array, which matches the row/column of the matrix
END TYPE BasisElemDescr

TYPE BSDFDaylghtPosition
  REAL(r64)  :: Altitude   ! Altitude range is from -pi/2 to pi/2. Horizontal vector have altitude of zero
  REAL(r64)  :: Azimuth    ! Azimuth is measured from positive x counter clockwise. Its range is from -pi to pi
END TYPE BSDFDaylghtPosition

TYPE BasisStruct
  INTEGER   :: BasisType = 0 ! BasisType_WINDOW or BasisType_Custom  (see HeatBalanceManager)
  INTEGER   :: BasisSymmetryType = 0 ! BasisSymmetry_Axisymmetric or BasisSymmetry_None  (see HeatBalanceManager)
  INTEGER   :: BasisMatIndex = 0 !pointer to matrix for basis
  INTEGER   :: NBasis = 0  ! No. elements in basis
  REAL(r64), DIMENSION(:), ALLOCATABLE   :: Lamda  !Vector of diagonal Lamda matrix elems for grid
  REAL(r64), DIMENSION(:), ALLOCATABLE  :: SolAng  !Vector of basis element solid angles for grid
  INTEGER                 :: NThetas = 0  ! No. Theta values in basis
  REAL(r64), DIMENSION(:), ALLOCATABLE   :: Thetas  !List of basis theta values
  INTEGER, DIMENSION(:), ALLOCATABLE     :: NPhis    !No. basis phi values for each theta
  REAL(r64), DIMENSION(:,:), ALLOCATABLE    :: Phis  !List of basis phi values for each theta
  INTEGER, DIMENSION(:,:), ALLOCATABLE      :: BasisIndex  !Index of basis element for theta, phi
  TYPE(BasisElemDescr), DIMENSION(:), ALLOCATABLE :: Grid  ! actual basis (to be constructed from matrix)
END TYPE BasisStruct

TYPE BSDFGeomDescr
  TYPE (BasisStruct)  ::  Inc    !Basis for incident hemisphere
  TYPE (Vector), DIMENSION(:), ALLOCATABLE  :: sInc  !Central direction vectors of incident grid (World coords)
  TYPE (BSDFDaylghtPosition), DIMENSION(:), ALLOCATABLE :: pInc ! azimuth and altitude of incidence vectors
  REAL(r64), DIMENSION(:), ALLOCATABLE :: CosInc ! cosine of incident angle
  REAL(r64), DIMENSION(:), ALLOCATABLE :: DAInc ! cosine of incident angle times delta theta time delta phi 
                                              ! (used in daylighting calculations)
  INTEGER  :: NSkyUnobs  !Number of Inc basis rays from unobstructed sky
  INTEGER  :: NGndUnobs  !Number of Inc basis rays from unobstructed ground
  INTEGER  :: NSky    !Number of Inc basis rays from sky
  INTEGER  :: NGnd    !Number of Inc basis rays from gnd
  INTEGER  :: NReflSurf  !Number of Inc basis rays from (potentially reflecting) surfaces
  INTEGER, DIMENSION(:), ALLOCATABLE  ::  SkyIndex  !list of sky basis indices
  INTEGER, DIMENSION(:), ALLOCATABLE  ::  GndIndex  !list of gnd basis indices
  TYPE (Vector), DIMENSION(:), ALLOCATABLE  ::  GndPt  !gnd intersection pt of gnd basis ray (z=0)
  INTEGER, DIMENSION(:), ALLOCATABLE  ::  RefSurfIndex  !list of basis indices of rays striking exterior surf
  INTEGER, DIMENSION(:), ALLOCATABLE  ::  RefRayNHits  !for a given ray striking a surface, no. of surfaces pierced
  INTEGER, DIMENSION(:,:), ALLOCATABLE  ::  HitSurfNo  !for a given ray striking surface, list of intersected surf nos
  REAL(r64), DIMENSION(:,:), ALLOCATABLE  ::  HitSurfDSq  ! for a given ray striking surface, list of distance^2
                                                                                                                !  from window
  TYPE (Vector), DIMENSION(:,:), ALLOCATABLE    ::  HitPt  ! for a given ray striking surface, list of hit pts
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::  SolSkyWt  !Sky intensity weights
  REAL(r64), DIMENSION(:), ALLOCATABLE    ::  SolSkyGndWt  !Wts for sky rad refl from grn
  REAL(r64), DIMENSION(: , : , : ), ALLOCATABLE    ::  SolBmGndWt  !Wts for beam rad refl from gnd (hour, timestep)
  INTEGER, DIMENSION( : , : ), ALLOCATABLE  ::  SolBmIndex  !Basis index corresponding to beam dir (hour, timestep)
                !Note this is zero if sun is not in incident hemisphere
                !otherwise in range 1..NBasis
  REAL(r64), DIMENSION(: , :), ALLOCATABLE :: ThetaBm ! Theta angle corresponging to beam dir (hour, timestep) (rad)
  REAL(r64), DIMENSION(: , :), ALLOCATABLE :: PhiBm ! Theta angle corresponging to beam dir (hour, timestep) (rad)
  TYPE (BasisStruct)  ::  Trn
  TYPE (Vector), DIMENSION(:), ALLOCATABLE    :: sTrn  !Central direction vectors of Outgoing grid (World coords)
  TYPE (BSDFDaylghtPosition), DIMENSION(:), ALLOCATABLE :: pTrn ! azimuth and altitude of incidence vectors
  INTEGER, DIMENSION(:), ALLOCATABLE    ::  NSurfInt  !No. of basis rays intersecting back surface (dim from
                !NBkSurf in BSDF State Descr)
  INTEGER, DIMENSION(:,:), ALLOCATABLE    ::  SurfInt  !Basis index (IBkSurf, j) of the jth ray intersecting IBkSurf
  REAL(r64), DIMENSION (: , : ), ALLOCATABLE  ::  SjdotN  !dot product (IBksurf, j) of the jth ray direction with
                                                          ! the normal to the back surface
  REAL(r64), DIMENSION(:,:), ALLOCATABLE :: AOverlap ! Overlap areas for each outgoing
                                                     ! direction (Trn) (no of outgoing dir, NBKSurf)
  REAL(r64), DIMENSION(:,:), ALLOCATABLE :: ARhoVisOverlap ! Overlap areas multiplied with surface reflectance for each outgoing 
                                                           ! direction (Trn) (no of outgoing dir, NBKSurf)
  REAL(r64), DIMENSION(:), ALLOCATABLE   :: AveRhoVisOverlap ! Average visible reflectance from overlap surface which 
                                                             ! originates from one outgoing direction
  LOGICAL :: InitState = .TRUE.  ! Flag for marking that state needs to be initalized
END TYPE BSDFGeomDescr

! Structure to keep reference points coefficients for different reference points and illuminance maps
TYPE BSDFRefPoints
  INTEGER, DIMENSION(:), ALLOCATABLE  ::  NSky  ! number of sky elements for each window element (# window el)
  INTEGER, DIMENSION(:), ALLOCATABLE  ::  NGnd  ! number of ground elements for each window element (# window el)
  INTEGER, DIMENSION(:), ALLOCATABLE  ::  NReflSurf  ! number of Inc basis rays from reflecting surfaces (# window el)
  INTEGER, DIMENSION(:,:), ALLOCATABLE  ::  SkyIndex  !list of sky basis indices (# window el, NSky)
  INTEGER, DIMENSION(:,:), ALLOCATABLE  ::  GndIndex  !list of gnd basis indices (# window el, NGnd)
  TYPE (Vector), DIMENSION(:,:), ALLOCATABLE  ::  GndPt  !gnd intersection pt of gnd basis ray (z=0) (# window el, NGnd)
  REAL(r64), DIMENSION(:,:), ALLOCATABLE  ::  GndObstrMultiplier !ground obstruction multiplier used in reflection calculatations
                                                                    ! (# window el, NGnd)
  INTEGER, DIMENSION(:,:), ALLOCATABLE  ::  RefSurfIndex  ! list of basis indices of rays striking exterior surf 
                                                            ! (# window el, NReflSurf)
  INTEGER, DIMENSION(:,:), ALLOCATABLE  ::  RefRayNHits  ! for a given ray striking a surface, no. of surfaces pierced
                                                            ! (# window el, NReflSurf)
  REAL(r64), DIMENSION(:,:), ALLOCATABLE  ::  TransOutSurf  ! total transmittance of exterior obstructions for given incoming 
                                                            ! basis direction. (# window el, NReflSurf)
  INTEGER, DIMENSION(:,:,:), ALLOCATABLE  ::  HitSurfNo  ! for a given ray striking surface, list of intersected surf nos
                                                            ! (# window el, NReflSurf, RefRayNHits)
  REAL(r64), DIMENSION(:,:,:), ALLOCATABLE  ::  HitSurfDSq  ! for a given ray striking surface, list of distance^2 from window
                                                            ! (# window el, NReflSurf, RefRayNHits)
  TYPE (Vector), DIMENSION(:,:,:), ALLOCATABLE    ::  HitPt  ! for a given ray striking surface, list of hit pts
                                                             ! (# window el, NReflSurf, RefRayNHits)
  INTEGER, DIMENSION(:), ALLOCATABLE  :: RefPointIndex  ! outgoing direction which containts reference point 
                                                          ! (# window el)
  LOGICAL, DIMENSION(:), ALLOCATABLE :: RefPointIntersection ! determines if reference point is laying in light tube of bsdf
                                                                ! outgoing direction
                                                                ! (NTrnBasis)
  REAL(r64), DIMENSION(:), ALLOCATABLE :: RefPtIntPosFac  ! position factors for intersections from reference point to window for
                                                            ! each outgoing direction
                                                            ! (NTrnBasis)
END TYPE BSDFRefPoints

TYPE BSDFDaylghtGeomDescr
  TYPE(BSDFRefPoints), DIMENSION(:,:), ALLOCATABLE :: IlluminanceMap    ! array to keep bsdf coefficients for different 
                                                                              ! illuminance maps
                                                          ! (# of illuminance maps, # of reference points)
  TYPE(BSDFRefPoints), DIMENSION(:), ALLOCATABLE :: RefPoint     ! keep reference points daylight coefficients
                                                          ! (# of reference points)
END TYPE BSDFDaylghtGeomDescr

TYPE BSDFBkSurfDescr

 REAL(r64), DIMENSION (: , : ), ALLOCATABLE      ::  WinDHBkRefl    !Back directional hemispherical reflectance
                 ! (hour, timestep)
                ! of this window for radiation from the
                ! back surface window
  REAL(r64), DIMENSION (: , : , : ), ALLOCATABLE         :: WinDirBkAbs  !back absorptance (layer, hr, timestep)
                !   for beam radiation absorbed in this
                !   window that comes from the back surface window
  !Note:  WinDHBkRefl and WinDirBkAbs are the same for all hours & timesteps if the back surface window is a
  ! Complex Fenestration; they depend on the sun direction if the back surface window is a regular window
END TYPE BSDFBkSurfDescr


TYPE BSDFStateDescr
  INTEGER      :: Konst =0    !pointer to construction for this state; property matrices are in the construction
  !INTEGER      :: ThermConst =0  ! pointer to thermal construction for this state
  REAL(r64)                   ::  WinDiffTrans  !Window hemispherical ave diff trans
                ! for use in corrections requiring a diffuse trans
                ! that have not been redone in detail for Compex Fen
  REAL(r64)                   ::  WinDiffVisTrans  !Window hemispherical ave diff trans for visible spectrum
  REAL(r64), DIMENSION (:,:), ALLOCATABLE     :: WinDirHemiTrans    !Directional-hemispherical transmittance(hr,ts)
  REAL(r64), DIMENSION (:,:), ALLOCATABLE     :: WinDirSpecTrans    !Directional specular transmittance(hr,ts)
  REAL(r64)                  :: WinSkyTrans  =0.0d0  !Transmittance for sky radiation (weighted average over sky viewed)
  REAL(r64)                  :: WinSkyGndTrans  =0.0d0  !Transmittance for sky radiation reflected from ground (average over
               !viewed part of ground)
  REAL(r64), DIMENSION (:,:), ALLOCATABLE    :: WinBmGndTrans    !Transmittance (hour, timestep) for beam radiation reflected
            !from ground (average over unshaded ground viewed)
  REAL(r64)                   ::  WinBkHemRefl  =0.0d0  !Window back hemispherical reflectance
  REAL(r64)                   ::  WinBkHemVisRefl  =0.0d0  !Window back hemispherical reflectance (visible spectrum)
                !(for reflection of interior diffuse radiation)
  INTEGER                 ::  NLayers  =0  !Number of absorbing layers in this window
  REAL(r64), DIMENSION (:,:,:), ALLOCATABLE  ::  WinBmFtAbs    !Front directional absorptance (layer, hour, timestep)
  REAL(r64), DIMENSION (:), ALLOCATABLE      ::  WinSkyFtAbs    !Front absorptance (layer) averaged over sky
  REAL(r64), DIMENSION (:), ALLOCATABLE      ::  WinSkyGndAbs    !Front absorptance (layer) averaged over ground
                ! viewed part of gnd  (for ground-reflected sky radiation)
  REAL(r64), DIMENSION (:,:,:), ALLOCATABLE  ::  WinBmGndAbs   !Front absorptance (layer, hour, timestep) averaged
                  !over unshaded ground viewed by beam
  REAL(r64), DIMENSION (:), ALLOCATABLE     ::  WinFtHemAbs    !Front hemispherical absorptance (layers)
  REAL(r64), DIMENSION (:), ALLOCATABLE     ::  WinBkHemAbs    !Back hemispherical absorptance (layers)
  REAL(r64), DIMENSION (: , : ,: ), ALLOCATABLE  ::  WinToSurfBmTrans    !Beam transmittance (bk surf no, hour, timestep)
                    !to back surface
        !Note: the following will be evaluated only if the given back surface is a  window
  TYPE (BSDFBkSurfDescr),DIMENSION(:), ALLOCATABLE  ::  BkSurf  !Structure dimensioned (bk surface no)

  ! Integrated beam values at front and back sides of window.  It is used in calculations of how much of the energy is
  ! leaving throught the window to other zone or to the outside for certain beam direction
  REAL(r64), DIMENSION (:), ALLOCATABLE   :: IntegratedFtAbs ! Sum of all back layer absorptances (for each back direction)
  REAL(r64), DIMENSION (:), ALLOCATABLE   :: IntegratedFtRefl ! Integrated back layer reflectance (for each back direction)
  REAL(r64), DIMENSION (:), ALLOCATABLE   :: IntegratedFtTrans ! Integrated back layer transmittance (for each back direction)
  REAL(r64), DIMENSION (:), ALLOCATABLE   :: IntegratedBkAbs ! Sum of all back layer absorptances (for each back direction)
  REAL(r64), DIMENSION (:), ALLOCATABLE   :: IntegratedBkRefl ! Integrated back layer reflectance (for each back direction)
  REAL(r64), DIMENSION (:), ALLOCATABLE   :: IntegratedBkTrans ! Integrated back layer transmittance (for each back direction)

END TYPE BSDFStateDescr

TYPE BSDFRefPointsGeomDescr
   REAL(r64), DIMENSION(:), ALLOCATABLE :: SolidAngle ! Solid angle from daylighting reference point to each window element
                                                      ! (# window el)                                                    
   TYPE(vector), DIMENSION(:), ALLOCATABLE :: SolidAngleVec ! unit vector from reference point towards center of window element
                                                      ! (# window el)  
END TYPE

TYPE BSDFWindowGeomDescr
        !This contains all the geometry info that we don't want to carry around in SurfaceWindow
        !This is dimensioned like SurfaceWindow, but only surfaces that are complex windows
        !will have the structure below allocated
   INTEGER    :: NumStates      !Number of states for this window
   TYPE (BSDFGeomDescr), DIMENSION(:), ALLOCATABLE   :: Geom  !This is dimensioned with number of states
   TYPE (BSDFDaylghtGeomDescr), DIMENSION(:), ALLOCATABLE   :: DaylghtGeom  !This is dimensioned with number of states
   LOGICAL :: DaylightingInitialized = .FALSE. ! used for one time initialization only
   INTEGER  ::NBkSurf  =0      !Number of back (interior) surfaces viewed by this window
   TYPE (Vector), DIMENSION(:), ALLOCATABLE    ::  sWinSurf  !Unit vector from window center to center of IBkSurf
   REAL(r64), DIMENSION(:), ALLOCATABLE  ::  sdotN  !Dot product of unit vector s with back surface normal
               ! here s is vector from center of window to center of back surface
                                         !Function of the following subsumed by using an index of 0 if no beam incidence
   !REAL(r64), DIMENSION(: , : ), ALLOCATABLE  ::  SolBmWt  !Intensity wt for beam radiation (Hour, timestep)
   TYPE(BSDFRefPointsGeomDescr), DIMENSION(:,:), ALLOCATABLE :: IlluminanceMap    ! array to keep bsdf coefficients for 
                                                                                !different illuminance maps
                                                          ! (# of illuminance maps, # of reference points)
   TYPE(BSDFRefPointsGeomDescr), DIMENSION(:), ALLOCATABLE :: RefPoint     ! keep reference points daylight coefficients
                                                            ! (# of reference points)
END TYPE BSDFWindowGeomDescr

TYPE BSDFWindowDescript
  INTEGER    ::   NumStates      !Number of states for this window
  INTEGER    ::  CurrentState   = 1  !Current state of this window
  REAL(r64), DIMENSION(: , :), ALLOCATABLE  ::  ResultAllStates  !Array to hold calculated
              !quantities for all states.
              !Currently unallocated.  To be defined when control
              !scheme worked out.  This is an array (nvar, nstates)
              !to be set up for some number of variables, and calculated
              !for all states 1...NumStates each time step.  e.g., one variable could be
              !total beam transmitted solar, another total transmitted diffuse
              !The idea is that for a given time step when one has the
              !actual result (total cooling load or whatever), one needs to have
              !some information about all the states to decide where to
              !set the state variable for the next time step
  TYPE (BSDFStateDescr), DIMENSION(:),  ALLOCATABLE :: State  !State description, dimensioned with number of states
END TYPE BSDFWindowDescript  !This structure is located in SurfaceWindow as SurfaceWindow(ISurf)%ComplexFen

  !Allocation of complex fenestration data:  SurfaceWindow(:)%ComplexFen is a structure of type BSDFWindowDescript
    !defined in DataSurfaces.  ComplexWind(:) is an array of type BSDF WindowGeomDescr defined as a module
    !variable in WindowComplexManager

TYPE BSDFLayerAbsorpStruct
  INTEGER        :: MaterialIndex = 0 ! pointer to material layer
  INTEGER        :: FrtAbsIndex = 0 !pointer to matrix for Front directional absorptance vector
  INTEGER        :: AbsNcols    = 0 !Number of elements (columns) in each of the absorption (row) vectors
  REAL(r64), DIMENSION(:,:), ALLOCATABLE :: FrtAbs  !Front directional absorptance vector
  INTEGER        :: BkAbsIndex = 0 !pointer to matrix for Back directional absorptance vector
  REAL(r64), DIMENSION(:,:), ALLOCATABLE :: BkAbs  !Back directional absorptance vector
END TYPE BSDFLayerAbsorpStruct

TYPE BSDFWindowInputStruct
  !nested data for Construction
  INTEGER   :: BasisType = 0 !
  INTEGER   :: BasisSymmetryType = 0 !
  INTEGER    :: ThermalModel = 0    ! Pointer to thermal model
  INTEGER   :: BasisMatIndex = 0 !pointer to matrix for basis
  INTEGER   :: BasisMatNrows = 0 !No. rows in matrix
  INTEGER   :: BasisMatNcols = 0 !No. columns in matrix
  INTEGER   :: NBasis = 0  !No. elements in basis
  REAL(r64), DIMENSION(:,:), ALLOCATABLE :: BasisMat  ! basis matrix
  INTEGER   :: SolFrtTransIndex = 0 !pointer to matrix for Front optical transmittance matrix
  INTEGER   :: SolFrtTransNrows = 0 !No. rows in matrix
  INTEGER   :: SolFrtTransNcols = 0 !No. columns in matrix
  REAL(r64), DIMENSION(:,:), ALLOCATABLE :: SolFrtTrans  !Front optical transmittance matrix
  INTEGER   :: SolBkReflIndex = 0 !pointer to matrix for Back optical reflectance matrix
  INTEGER   :: SolBkReflNrows = 0 !No. rows in matrix
  INTEGER   :: SolBkReflNcols = 0 !No. columns in matrix
  REAL(r64), DIMENSION(:,:), ALLOCATABLE :: SolBkRefl  !Back optical reflectance matrix
  INTEGER   :: VisFrtTransIndex = 0 !pointer to matrix for Front visible transmittance matrix
  INTEGER   :: VisFrtTransNrows = 0 !No. rows in matrix
  INTEGER   :: VisFrtTransNcols = 0 !No. columns in matrix
  REAL(r64), DIMENSION(:,:), ALLOCATABLE :: VisFrtTrans  !Front visible transmittance matrix
  INTEGER   :: VisBkReflIndex  = 0 !pointer to matrix for Back visible reflectance matrix
    INTEGER   :: VisBkReflNrows = 0 !No. rows in matrix
  INTEGER   :: VisBkReflNcols = 0 !No. columns in matrix
  REAL(r64), DIMENSION(:,:), ALLOCATABLE :: VisBkRefl  !Back visible reflectance matrix
  !INTEGER   :: ThermalConstruction  !Pointer to location in Construct array of thermal construction for the state
          ! (to be implemented)
  INTEGER  :: NumLayers  = 0 !
  TYPE(BSDFLayerAbsorpStruct), DIMENSION(:), ALLOCATABLE :: Layer

END TYPE BSDFWindowInputStruct



          ! MODULE VARIABLE DECLARATIONS:

INTEGER :: TotComplexFenStates = 0   ! Number of complex fenestration construction definitions
INTEGER :: FirstBSDF = 0       ! Location of first complex fenestration construction definition in Constr array
INTEGER   :: MaxBkSurf = 20   !was 20    Maximum number of back surfaces in solar overlap & interior solar distribution
INTEGER :: TotThermalModels = 0  ! Number of thermal models
                                                      !calculation
REAL(r64), DIMENSION(3,24,60)    ::  SUNCOSTS  !Timestep values of solar direction cosines
TYPE (BSDFWindowGeomDescr), DIMENSION(:), ALLOCATABLE  ::  ComplexWind   !Window geometry structure                                                                                               !set in CalcPerSolarBeam/SolarShading
REAL(r64), DIMENSION(: , :), ALLOCATABLE  :: BSDFTempMtrx  !Temporary matrix for holding axisymmetric input




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

END MODULE DataBSDFWindow












































