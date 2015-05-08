MODULE SolarReflectionManager

          ! MODULE INFORMATION
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   September 2003
          !       MODIFIED       May 2004, FCW: modify calculation of receiving point location on a
          !                        receiving surface so can handle surface of any number of vertices
          !                        (previously restricted to 3- or 4-sided surfaces).
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Manages the calculation of factors for solar reflected from obstructions and ground.

          ! METHODOLOGY EMPLOYED:
          !
          ! REFERENCES: na

          ! OTHER NOTES: na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals
USE DataHeatBalance
USE DataSurfaces
USE ScheduleManager
USE DataEnvironment
USE DataInterfaces

USE DataVectorTypes

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE  ! Only certain items will be declared public

          ! MODULE PARAMETER DEFINITIONS:na

          ! DERIVED TYPE DEFINITIONS:

TYPE,PUBLIC :: SolReflRecSurfData
  INTEGER                           :: SurfNum      =0   ! Number of heat transfer surface
  CHARACTER(len=MaxNameLength)      :: SurfName     =' ' ! Name of heat transfer surface
  INTEGER                           :: NumRecPts    =0   ! Number of receiving points
  REAL(r64),ALLOCATABLE,DIMENSION(:,:)   :: RecPt        ! Coordinates of receiving point on receiving surface in global CS (m)
  REAL(r64),DIMENSION(3)                 :: NormVec      =0.0d0 ! Unit outward normal to receiving surface
  REAL(r64)                         :: ThetaNormVec =0.0d0 ! Azimuth of surface normal (radians)
  REAL(r64)                         :: PhiNormVec   =0.0d0 ! Altitude of surface normal (radians)
  INTEGER                           :: NumReflRays  =0   ! Number of rays from this receiving surface
  REAL(r64),ALLOCATABLE,DIMENSION(:,:)   :: RayVec       ! Unit vector in direction of ray from receiving surface
  REAL(r64),ALLOCATABLE,DIMENSION(:)     :: CosIncAngRay ! Cosine of angle between ray and receiving surface outward normal
  REAL(r64),ALLOCATABLE,DIMENSION(:)     :: dOmegaRay    ! Delta solid angle associated with ray
  REAL(r64),ALLOCATABLE,DIMENSION(:,:,:) :: HitPt        ! For each receiving point and ray, coords of hit point on obstruction
                                                    ! that is closest to receiving point (m)
  INTEGER,ALLOCATABLE,DIMENSION(:,:):: HitPtSurfNum ! Number of surface containing the hit point for a ray, except:
                                                    !  0 => ray does not hit an obstruction, but hits sky
                                                    !  -1 => ray does not hit an obstruction, but hits ground
  REAL(r64),ALLOCATABLE,DIMENSION(:,:)   :: HitPtSolRefl ! Beam-to-diffuse solar reflectance at hit point
  REAL(r64),ALLOCATABLE,DIMENSION(:,:)   :: RecPtHitPtDis ! Distance from receiving point to hit point (m)
  REAL(r64),ALLOCATABLE,DIMENSION(:,:,:) :: HitPtNormVec ! Hit point's surface normal unit vector pointing into hemisphere
                                                    !  containing the receiving point
  INTEGER,ALLOCATABLE,DIMENSION(:)  :: PossibleObsSurfNums ! Surface numbers of possible obstructions for a receiving surf
  INTEGER                           :: NumPossibleObs      =0 ! Number of possible obstructions for a receiving surface
END TYPE SolReflRecSurfData

TYPE(SolReflRecSurfData), PUBLIC, ALLOCATABLE, DIMENSION(:) :: SolReflRecSurf

          ! MODULE VARIABLE DECLARATIONS:
INTEGER :: TotSolReflRecSurf      = 0   ! Total number of exterior surfaces that can receive reflected solar
INTEGER :: TotPhiReflRays         = 0   ! Number of rays in altitude angle (-90 to 90 deg) for diffuse refl calc
INTEGER :: TotThetaReflRays       = 0   ! Number of rays in azimuth angle (0 to 180 deg) for diffuse refl calc

          ! SUBROUTINE SPECIFICATIONS FOR MODULE ExteriorSolarReflectionManager
PUBLIC  InitSolReflRecSurf
PUBLIC  CalcBeamSolDiffuseReflFactors
PRIVATE FigureBeamSolDiffuseReflFactors
PUBLIC  CalcBeamSolSpecularReflFactors
PRIVATE FigureBeamSolSpecularReflFactors
PUBLIC  CalcSkySolDiffuseReflFactors
PRIVATE PierceSurface
PRIVATE CrossProduct

CONTAINS

          ! MODULE SUBROUTINES:

SUBROUTINE InitSolReflRecSurf

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   September 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Initializes the derived type SolReflRecSurf, which contains information
          ! needed to calculate factors for solar reflection from obstructions and ground.

          ! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE Vectors

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS: na
          ! INTERFACE BLOCK SPECIFICATIONS: na
          ! DERIVED TYPE DEFINITIONS: na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: SurfNum              ! Surface number
  INTEGER   :: RecSurfNum           ! Receiving surface number
  INTEGER   :: loop                 ! DO loop indices
  INTEGER   :: loop1                ! DO loop indices
  INTEGER   :: loopA                ! DO loop indices
  INTEGER   :: loopb                ! DO loop indices
  INTEGER   :: ObsSurfNum           ! Surface number of an obstruction
  LOGICAL   :: ObsBehindRec         ! True if an obstruction is entirely behind a receiving surface
  LOGICAL   :: ObsHasView           ! True if view between receiving surface and heat trans surf obstruction
  REAL(r64) :: RecVec(3)            ! First vertex of a receiving surface (m)
  REAL(r64) :: ObsVec(3)            ! A vertex of a candidate obstructing surface (m)
  REAL(r64) :: VecAB(3)             ! Vector from receiving surface vertex to obstruction surface vertex (m)
  REAL(r64) :: HitPt(3)             ! Hit point (m)
  REAL(r64) :: DotProd              ! Dot product of vectors (m2)
  INTEGER   :: RecPtNum             ! Receiving point number
!unused  REAL(r64)         :: SumX                 ! Sum of X (or Y or Z) coordinate values of a surface
!unused  REAL(r64)         :: SumY                 ! Sum of X (or Y or Z) coordinate values of a surface
!unused  REAL(r64)         :: SumZ                 ! Sum of X (or Y or Z) coordinate values of a surface
  REAL(r64) :: PhiSurf              ! Altitude of normal to receiving surface (radians)
  REAL(r64) :: ThetaSurf            ! Azimuth of normal to receiving surface (radians)
  REAL(r64) :: PhiMin               ! Minimum and maximum values of ray altitude angle (radians)
  REAL(r64) :: PhiMax               ! Minimum and maximum values of ray altitude angle (radians)
  REAL(r64) :: ThetaMin             ! Minimum and maximum values of ray azimuth angle (radians)
  REAL(r64) :: ThetaMax             ! Minimum and maximum values of ray azimuth angle (radians)
  REAL(r64) :: Phi                  ! Ray altitude angle, increment, sine, and cosine
  REAL(r64) :: DPhi                 ! Ray altitude angle, increment, sine, and cosine
  REAL(r64) :: SPhi                 ! Ray altitude angle, increment, sine, and cosine
  REAL(r64) :: CPhi                 ! Ray altitude angle, increment, sine, and cosine
  REAL(r64) :: Theta                ! Ray azimuth angle and increment
  REAL(r64) :: DTheta               ! Ray azimuth angle and increment
  INTEGER   :: IPhi                 ! Ray altitude angle and azimuth angle indices
  INTEGER   :: ITheta               ! Ray altitude angle and azimuth angle indices
!unused  REAL(r64)         :: APhi                 ! Intermediate variable
  INTEGER   :: RayNum               ! Ray number
  REAL(r64) :: URay(3)              ! Unit vector along ray pointing away from receiving surface
  REAL(r64) :: CosIncAngRay         ! Cosine of angle of incidence of ray on receiving surface
  REAL(r64) :: dOmega               ! Solid angle associated with a ray
  INTEGER   :: IHit                 ! = 1 if obstruction is hit, 0 otherwise
  INTEGER   :: TotObstructionsHit   ! Number of obstructions hit by a ray
  REAL(r64) :: HitDistance          ! Distance from receiving point to hit point for a ray (m)
  INTEGER   :: NearestHitSurfNum    ! Surface number of nearest obstruction hit by a ray
  REAL(r64) :: NearestHitPt(3)      ! Nearest hit pit for a ray (m)
  REAL(r64) :: NearestHitDistance   ! Distance from receiving point to nearest hit point for a ray (m)
  INTEGER   :: ObsSurfNumToSkip     ! Surface number of obstruction to be ignored
  REAL(r64) :: RecPt(3)             ! Receiving point (m)
  REAL(r64) :: RayVec(3)            ! Unit vector along ray
  REAL(r64) :: Vec1(3)              ! Vectors between hit surface vertices (m)
  REAL(r64) :: Vec2(3)              ! Vectors between hit surface vertices (m)
  REAL(r64) :: VNorm(3)             ! For a hit surface, unit normal vector pointing into the hemisphere
                                    ! containing the receiving point
  INTEGER   :: ObsConstrNum         ! Construction number of obstruction; = 0 if a shading surface
  REAL(r64) :: Alfa,Beta            ! Direction angles for ray heading towards the ground (radians)
  REAL(r64) :: HorDis               ! Distance between ground hit point and proj'n of receiving pt onto ground (m)
  REAL(r64) :: GroundHitPt(3)       ! Coordinates of ground hit point
!unused  REAL(r64)         :: ArgASin
  REAL(r64) :: ACosTanTan
  INTEGER   :: J                    ! DO loop indices
  INTEGER   :: K                    ! DO loop indices
  INTEGER   :: L                    ! DO loop indices
  INTEGER   :: NumRecPts            ! Number of surface receiving points for reflected solar radiation
  REAL(r64) :: VertexWt             ! Vertex weighting factor for calculating receiving points

          ! FLOW:

! Find number of surfaces that are sun-exposed exterior building heat transfer surfaces.
! These are candidates for receiving solar reflected from obstructions and ground.
! CR 7640.  12/3/2008 BG simplified logic to allow for Other Side Conditions Modeled boundary condition.
!           and solar collectors on shading surfaces that need this.
!

! shading surfaces have ExtSolar = False, so they are not included in TotSolReflRecSurf
TotSolReflRecSurf = 0
DO SurfNum = 1,TotSurfaces
  IF (Surface(SurfNum)%ExtSolar) THEN
    TotSolReflRecSurf = TotSolReflRecSurf + 1
  ENDIF
END DO

! TH 3/29/2010. ShadowSurfPossibleReflector is not used!
! Set flag that determines whether a surface can be an exterior reflector
!DO SurfNum = 1,TotSurfaces
!  Surface(SurfNum)%ShadowSurfPossibleReflector = .FALSE.
  ! Exclude non-exterior heat transfer surfaces (but not OtherSideCondModeledExt = -4 CR7640)
!  IF(Surface(SurfNum)%HeatTransSurf .AND. Surface(SurfNum)%ExtBoundCond > 0 ) CYCLE
!  IF(Surface(SurfNum)%HeatTransSurf .AND. Surface(SurfNum)%ExtBoundCond == Ground) CYCLE
!  IF(Surface(SurfNum)%HeatTransSurf .AND. Surface(SurfNum)%ExtBoundCond == OtherSideCoefNoCalcExt) CYCLE
!  IF(Surface(SurfNum)%HeatTransSurf .AND. Surface(SurfNum)%ExtBoundCond == OtherSideCoefCalcExt) CYCLE

  ! Exclude daylighting shelves. A separate solar reflection calculation is done for these.
!  IF(Surface(SurfNum)%Shelf > 0) CYCLE

  ! Exclude duplicate shading surfaces
  ! TH 3/24/2010. Why? a mirror shading surface can reflect solar (either beam or diffuse)
  !  can use a flag like Surface(SurfNum)%Mirrored (True or False) to avoid string comparison
  !   and to allow surface names starting with 'Mir'
  !IF(Surface(SurfNum)%Name(1:3) == 'Mir') CYCLE
!  IF(Surface(SurfNum)%MirroredSurf) CYCLE

!  Surface(SurfNum)%ShadowSurfPossibleReflector = .TRUE.
!END DO

IF(TotSolReflRecSurf == 0) THEN
  CALL ShowWarningError('Calculation of solar reflected from obstructions has been requested but there')
  CALL ShowContinueError('are no building surfaces that can receive reflected solar. Calculation will not be done.')
  CalcSolRefl = .FALSE.
  RETURN
END IF

! Should this be moved up front?
IF (IgnoreSolarRadiation) THEN
  TotSolReflRecSurf = 0
  CalcSolRefl = .FALSE.
  RETURN
ENDIF

ALLOCATE(SolReflRecSurf(TotSolReflRecSurf))

ALLOCATE(ReflFacBmToDiffSolObs(TotSurfaces,24))
ReflFacBmToDiffSolObs = 0.0d0
ALLOCATE(ReflFacBmToDiffSolGnd(TotSurfaces,24))
ReflFacBmToDiffSolGnd = 0.0d0
ALLOCATE(ReflFacBmToBmSolObs(TotSurfaces,24))
ReflFacBmToBmSolObs = 0.0d0
ALLOCATE(ReflFacSkySolObs(TotSurfaces))
ReflFacSkySolObs = 0.0d0
ALLOCATE(ReflFacSkySolGnd(TotSurfaces))
ReflFacSkySolGnd = 0.0d0
ALLOCATE(CosIncAveBmToBmSolObs(TotSurfaces,24))
CosIncAveBmToBmSolObs = 0.0d0

! Only surfaces with sun exposure can receive solar reflection from ground or onstructions
!  Shading surfaces are always not exposed to solar (ExtSolar = False)
RecSurfNum = 0
DO SurfNum = 1,TotSurfaces
  Surface(SurfNum)%ShadowSurfRecSurfNum = 0
  IF (Surface(SurfNum)%ExtSolar) THEN
    RecSurfNum = RecSurfNum + 1
    SolReflRecSurf(RecSurfNum)%SurfNum = SurfNum
    SolReflRecSurf(RecSurfNum)%SurfName = Surface(SurfNum)%Name
    Surface(SurfNum)%ShadowSurfRecSurfNum = RecSurfNum

    ! Warning if any receiving surface vertex is below ground level, taken to be at Z = 0 in absolute coords
    DO loop = 1,Surface(SurfNum)%Sides
      IF(Surface(SurfNum)%Vertex(loop)%Z < GroundLevelZ) THEN
        CALL ShowWarningError('Calculation of reflected solar onto surface=' &
               //TRIM(Surface(SurfNum)%Name)//' may be inaccurate')
        CALL ShowContinueError('because it has one or more vertices below ground level.')
        EXIT
      END IF
    END DO
  END IF
END DO

! Get MaxRecPts for allocating SolReflRecSurf arrays that depend on number of receiving points
MaxRecPts = 1
DO RecSurfNum = 1,TotSolReflRecSurf
  SolReflRecSurf(RecSurfNum)%NumRecPts = Surface(SolReflRecSurf(RecSurfNum)%SurfNum)%Sides
  IF(SolReflRecSurf(RecSurfNum)%NumRecPts > MaxRecPts) MaxRecPts = SolReflRecSurf(RecSurfNum)%NumRecPts
END DO

MaxReflRays = AltAngStepsForSolReflCalc * AzimAngStepsForSolReflCalc
DO RecSurfNum = 1,TotSolReflRecSurf
  SolReflRecSurf(RecSurfNum)%NormVec = 0.0d0
  ALLOCATE(SolReflRecSurf(RecSurfNum)%RecPt(3,MaxRecPts))
  SolReflRecSurf(RecSurfNum)%RecPt = 0.0d0
  ALLOCATE(SolReflRecSurf(RecSurfNum)%RayVec(3,MaxReflRays))
  SolReflRecSurf(RecSurfNum)%RayVec = 0.0d0
  ALLOCATE(SolReflRecSurf(RecSurfNum)%CosIncAngRay(MaxReflRays))
  SolReflRecSurf(RecSurfNum)%CosIncAngRay = 0.0d0
  ALLOCATE(SolReflRecSurf(RecSurfNum)%dOmegaRay(MaxReflRays))
  SolReflRecSurf(RecSurfNum)%dOmegaRay = 0.0d0
  ALLOCATE(SolReflRecSurf(RecSurfNum)%HitPt(3,MaxRecPts,MaxReflRays))
  SolReflRecSurf(RecSurfNum)%HitPt = 0.0d0
  ALLOCATE(SolReflRecSurf(RecSurfNum)%HitPtSurfNum(MaxRecPts,MaxReflRays))
  SolReflRecSurf(RecSurfNum)%HitPtSurfNum = 0
  ALLOCATE(SolReflRecSurf(RecSurfNum)%HitPtSolRefl(MaxRecPts,MaxReflRays))
  SolReflRecSurf(RecSurfNum)%HitPtSolRefl = 0.0d0
  ALLOCATE(SolReflRecSurf(RecSurfNum)%RecPtHitPtDis(MaxRecPts,MaxReflRays))
  SolReflRecSurf(RecSurfNum)%RecPtHitPtDis = 0.0d0
  ALLOCATE(SolReflRecSurf(RecSurfNum)%HitPtNormVec(3,MaxRecPts,MaxReflRays))
  SolReflRecSurf(RecSurfNum)%HitPtNormVec = 0.0d0
  ALLOCATE(SolReflRecSurf(RecSurfNum)%PossibleObsSurfNums(TotSurfaces))
  SolReflRecSurf(RecSurfNum)%PossibleObsSurfNums = 0
END DO

DO RecSurfNum = 1,TotSolReflRecSurf
  SurfNum = SolReflRecSurf(RecSurfNum)%SurfNum
  ! Outward norm to receiving surface
  SolReflRecSurf(RecSurfNum)%NormVec = Surface(SurfNum)%OutNormVec(1:3)
  RecVec = Surface(SurfNum)%Vertex(1)
  ! Loop over all surfaces and find those that can be obstructing surfaces for this receiving surf
  SolReflRecSurf(RecSurfNum)%NumPossibleObs = 0
  DO ObsSurfNum = 1,TotSurfaces
    ! Exclude the receiving surface itself and its base surface (if it has one)
    IF(ObsSurfNum == SurfNum .OR. ObsSurfNum == Surface(SurfNum)%BaseSurf) CYCLE
    ! Exclude non-exterior heat transfer surfaces
    IF(Surface(ObsSurfNum)%HeatTransSurf .AND. Surface(ObsSurfNum)%ExtBoundCond /= 0) CYCLE
    ! Exclude duplicate shading surfaces
    !IF(Surface(ObsSurfNum)%Name(1:3) == 'Mir') CYCLE
    !TH2 CR8959
    !IF(Surface(ObsSurfNum)%MirroredSurf) CYCLE

    ! Exclude surfaces that are entirely behind the receiving surface.This is true if dot products of the
    ! rec. surface outward normal and vector from first vertex of rec. surface and each vertex of
    ! obstructing surface are all negative.
    ObsBehindRec = .TRUE.
    DO loop = 1,Surface(ObsSurfNum)%Sides
      ObsVec = Surface(ObsSurfNum)%Vertex(loop)
      DotProd = DOT_PRODUCT(SolReflRecSurf(RecSurfNum)%NormVec,ObsVec-RecVec)
!CR8251      IF(DotProd > 0.01d0) THEN  ! This obstructing-surface vertex is not behind receiving surface
      IF(DotProd > 1.d-6) THEN  ! This obstructing-surface vertex is not behind receiving surface
        ObsBehindRec = .FALSE.
        EXIT
      END IF
    END DO
    IF(ObsBehindRec) CYCLE

    ! Exclude heat transfer surfaces that have no view with the receiving surface.
    ! There is view if: for at least one vector VecAB from a receiving surface vertex to
    ! a vertex of a potential obstructing surface that satisfies VecAB.nA > 0.0 and VecAB.nB < 0.0,
    ! where nA and nB are the outward normal to the receiving and obstructing surface, resp.
    IF(Surface(ObsSurfNum)%HeatTransSurf) THEN
      ObsHasView = .FALSE.
      DO loopA = 1,Surface(SurfNum)%Sides
        DO loopB = 1,Surface(ObsSurfNum)%Sides
          VecAB = Surface(ObsSurfNum)%Vertex(loopB) - Surface(SurfNum)%Vertex(loopA)
          IF(DOT_PRODUCT(VecAB,SolReflRecSurf(RecSurfNum)%NormVec) > 0.0d0 .AND.  &
             DOT_PRODUCT(VecAB,Surface(ObsSurfNum)%OutNormVec) < 0.0d0) THEN
               ObsHasView = .TRUE.
               EXIT
          END IF
        END DO
        IF(ObsHasView) EXIT
      END DO
      IF(.NOT.ObsHasView) CYCLE
    END IF

    ! This is a possible obstructing surface for this receiving surface
    SolReflRecSurf(RecSurfNum)%NumPossibleObs = SolReflRecSurf(RecSurfNum)%NumPossibleObs + 1
    SolReflRecSurf(RecSurfNum)%PossibleObsSurfNums(SolReflRecSurf(RecSurfNum)%NumPossibleObs) = ObsSurfNum
  END DO

  ! Get coordinates of receiving points on this receiving surface. The number of receiving points
  ! is equal to the number of surface vertices (3 or higher).

  NumRecPts = SolReflRecSurf(RecSurfNum)%NumRecPts
  DO J = 1,NumRecPts
    DO L = 1,3
      SolReflRecSurf(RecSurfNum)%RecPt(L,J) = 0.0d0
    END DO
    DO K = 1,NumRecPts
      IF(NumRecPts == 3) THEN  ! Receiving surface is a triangle
        VertexWt = 0.2d0
        IF(K == J) VertexWt = 0.6d0
      ELSE                     ! Receiving surface has 4 or more vertices
        VertexWt = 1.d0/(2.d0*REAL(NumRecPts,r64))
        IF(K == J) VertexWt = (REAL(NumRecPts,r64)+1.d0)/(2.d0*REAL(NumRecPts,r64))
      END IF
        SolReflRecSurf(RecSurfNum)%RecPt(1,J) = SolReflRecSurf(RecSurfNum)%RecPt(1,J) +  &
          VertexWt * Surface(SurfNum)%Vertex(K)%X
        SolReflRecSurf(RecSurfNum)%RecPt(2,J) = SolReflRecSurf(RecSurfNum)%RecPt(2,J) +  &
          VertexWt * Surface(SurfNum)%Vertex(K)%Y
        SolReflRecSurf(RecSurfNum)%RecPt(3,J) = SolReflRecSurf(RecSurfNum)%RecPt(3,J) +  &
          VertexWt * Surface(SurfNum)%Vertex(K)%Z
    END DO
  END DO

  ! Create rays going outward from receiving surface. The same rays will be used at each receiving point.
  ! The rays are used in calculating diffusely reflected solar incident on receiving surface.

  ! Divide hemisphere around receiving surface into elements of altitude Phi and
  ! azimuth Theta and create ray unit vector at each Phi,Theta pair in front of the surface.
  ! Phi = 0 at the horizon; Phi = Pi/2 at the zenith

  PhiSurf = ASIN(SolReflRecSurf(RecSurfNum)%NormVec(3))
  IF(ABS(SolReflRecSurf(RecSurfNum)%NormVec(1)) > 1.0d-5 .OR. ABS(SolReflRecSurf(RecSurfNum)%NormVec(2)) > 1.0d-5) THEN
    ThetaSurf = ATAN2(SolReflRecSurf(RecSurfNum)%NormVec(2), SolReflRecSurf(RecSurfNum)%NormVec(1))
  ELSE
    ThetaSurf = 0.d0
  END IF
  SolReflRecSurf(RecSurfNum)%PhiNormVec   = PhiSurf
  SolReflRecSurf(RecSurfNum)%ThetaNormVec = ThetaSurf
  PhiMin = MAX(-PiOvr2, PhiSurf - PiOvr2)
  PhiMax = MIN(PiOvr2, PhiSurf + PiOvr2)
  DPhi = (PhiMax - PhiMin) / AltAngStepsForSolReflCalc
  RayNum = 0

  ! Altitude loop
  DO IPhi = 1,AltAngStepsForSolReflCalc
    Phi = PhiMin + (IPhi - 0.5d0) * DPhi
    SPhi = SIN(Phi)
    CPhi = COS(Phi)
    ! Third component of ray unit vector in (Theta,Phi) direction
    URay(3) = SPhi

    IF(PhiSurf >= 0.0d0) THEN
      IF(Phi >= PiOvr2 - PhiSurf) THEN
        ThetaMin = -Pi
        ThetaMax = Pi
      ELSE
        ACosTanTan = ACOS(-TAN(Phi)*TAN(PhiSurf))
        ThetaMin = ThetaSurf - ABS(ACosTanTan)
        ThetaMax = ThetaSurf + ABS(ACosTanTan)
      END IF

    ELSE  ! PhiSurf < 0.0
      IF(Phi <= -PhiSurf - PiOvr2) THEN
        ThetaMin = -Pi
        ThetaMax = Pi
      ELSE
        ACosTanTan = ACOS(-TAN(Phi)*TAN(PhiSurf))
        ThetaMin = ThetaSurf - ABS(ACosTanTan)
        ThetaMax = ThetaSurf + ABS(ACosTanTan)
      END IF
    END IF

    DTheta = (ThetaMax - ThetaMin) / AzimAngStepsForSolReflCalc
    dOmega = CPhi * DTheta * DPhi

    ! Azimuth loop
    DO ITheta = 1,AzimAngStepsForSolReflCalc
      Theta = ThetaMin + (ITheta - 0.5d0) * DTheta
      URay(1) = CPhi * COS(Theta)
      URay(2) = CPhi * SIN(Theta)
      ! Cosine of angle of incidence of ray on receiving surface
      CosIncAngRay = SPhi * SIN(PhiSurf) + CPhi * COS(PhiSurf) * COS(Theta-ThetaSurf)
      IF (CosIncAngRay < 0.0d0) CYCLE ! Ray is behind receiving surface (although there shouldn't be any)
      RayNum = RayNum + 1
      SolReflRecSurf(RecSurfNum)%RayVec(1:3,RayNum) = URay
      SolReflRecSurf(RecSurfNum)%CosIncAngRay(RayNum) = CosIncAngRay
      SolReflRecSurf(RecSurfNum)%dOmegaRay(RayNum) = dOmega
    END DO ! End of azimuth loop

  END DO  ! End of altitude loop
  SolReflRecSurf(RecSurfNum)%NumReflRays = RayNum

END DO  ! End of loop over receiving surfaces

! Loop again over receiving surfaces and, for each ray, get hit point and info associated with that point
! (hit point = point that ray intersects nearest obstruction, or, if ray is downgoing and hits no
! obstructions, point that ray intersects ground plane).

DO RecSurfNum = 1, TotSolReflRecSurf
  SurfNum = SolReflRecSurf(RecSurfNum)%SurfNum
  DO RecPtNum = 1, SolReflRecSurf(RecSurfNum)%NumRecPts
    RecPt = SolReflRecSurf(RecSurfNum)%RecPt(1:3,RecPtNum)
    DO RayNum = 1, SolReflRecSurf(RecSurfNum)%NumReflRays
      IHit = 0
      ! Loop over possible obstructions. If ray hits one or more obstructions get hit point on closest obstruction.
      ! If ray hits no obstructions and is going upward set HitPointSurfNum = 0.
      ! If ray hits no obstructions and is going downward set HitPointSurfNum = -1 and get hit point on ground.
      TotObstructionsHit = 0
      NearestHitSurfNum = 0
      NearestHitDistance = 1.d+8
      ObsSurfNumToSkip = 0
      RayVec = SolReflRecSurf(RecSurfNum)%RayVec(1:3,RayNum)
      DO loop1 = 1,SolReflRecSurf(RecSurfNum)%NumPossibleObs
        ! Surface number of this obstruction
        ObsSurfNum = SolReflRecSurf(RecSurfNum)%PossibleObsSurfNums(loop1)
        ! If a window was hit previously (see below), ObsSurfNumToSkip was set to the window's base surface in order
        ! to remove that surface from consideration as a hit surface for this ray
        IF(ObsSurfNum == ObsSurfNumToSkip) CYCLE
        ! Determine if this ray hits ObsSurfNum (in which case IHit > 0) and, if so, what the
        ! distance from the receiving point to the hit point is
        CALL PierceSurface(ObsSurfNum,RecPt,RayVec,IHit,HitPt)
        IF(IHit > 0) THEN
          ! added TH 3/29/2010 to set ObsSurfNumToSkip
          IF(Surface(ObsSurfNum)%Class == SurfaceClass_Window) THEN
            ObsSurfNumToSkip = Surface(ObsSurfNum)%BaseSurf
          ENDIF

          ! If obstruction is a window and its base surface is the nearest obstruction hit so far,
          ! set NearestHitSurfNum to this window. Note that in this case NearestHitDistance has already
          ! been calculated, so does not have to be recalculated.
          IF(Surface(ObsSurfNum)%Class == SurfaceClass_Window .AND. Surface(ObsSurfNum)%BaseSurf == NearestHitSurfNum) THEN
            NearestHitSurfNum = ObsSurfNum
          ELSE
            TotObstructionsHit = TotObstructionsHit + 1
            ! Distance from receiving point to hit point
            HitDistance = SQRT(DOT_PRODUCT(HitPt-RecPt,HitPt-RecPt))
            ! Reset NearestHitSurfNum and NearestHitDistance if this hit point is closer than previous closest
            IF(HitDistance < NearestHitDistance) THEN
              NearestHitDistance = HitDistance
              NearestHitSurfNum  = ObsSurfNum
              NearestHitPt = HitPt
            ELSE IF(HitDistance == NearestHitDistance) THEN ! TH2 CR8959
              ! Ray hits mirrored surfaces. Choose the surface facing the ray.
              IF(DOT_PRODUCT(Surface(ObsSurfNum)%OutNormVec,RayVec) <= 0.0d0) THEN
                NearestHitSurfNum = ObsSurfNum
              END IF
            END IF
          END IF
        END IF  ! End of check if obstruction was hit
      END DO  ! End of loop over possible obstructions for this ray

      IF(TotObstructionsHit > 0) THEN
        ! One or more obstructions were hit by this ray
        SolReflRecSurf(RecSurfNum)%HitPtSurfNum(RecPtNum,RayNum) = NearestHitSurfNum
        SolReflRecSurf(RecSurfNum)%RecPtHitPtDis(RecPtNum,RayNum) = NearestHitDistance
        SolReflRecSurf(RecSurfNum)%HitPt(1:3,RecPtNum,RayNum) = NearestHitPt
        ! For hit surface, calculate unit normal vector pointing into the hemisphere
        ! containing the receiving point
        Vec1 = Surface(NearestHitSurfNum)%Vertex(1) -  Surface(NearestHitSurfNum)%Vertex(3)
        Vec2 = Surface(NearestHitSurfNum)%Vertex(2) -  Surface(NearestHitSurfNum)%Vertex(3)
        CALL CrossProduct(Vec1,Vec2,VNorm)
        VNorm = VNorm/SQRT(DOT_PRODUCT(VNorm,VNorm))
        IF(DOT_PRODUCT(VNorm,-RayVec) < 0.0d0) VNorm = -VNorm
        SolReflRecSurf(RecSurfNum)%HitPtNormVec(1:3,RecPtNum,RayNum) = VNorm
        ! Get solar and visible beam-to-diffuse reflectance at nearest hit point
        ObsConstrNum = Surface(NearestHitSurfNum)%Construction
        IF(ObsConstrNum > 0) THEN
          ! Exterior building surface is nearest hit
          IF(.NOT.Construct(ObsConstrNum)%TypeIsWindow) THEN
            ! Obstruction is not a window, i.e., is an opaque surface
            SolReflRecSurf(RecSurfNum)%HitPtSolRefl(RecPtNum,RayNum) = &
              1.0d0 - Construct(ObsConstrNum)%OutsideAbsorpSolar
          ELSE
            ! Obstruction is a window. Assume it is bare so that there is no beam-to-diffuse reflection
            ! (beam-to-beam reflection is calculated in subroutine CalcBeamSolSpecularReflFactors).
            SolReflRecSurf(RecSurfNum)%HitPtSolRefl(RecPtNum,RayNum) = 0.0d0
          END IF
        ELSE
          ! Shading surface is nearest hit
          SolReflRecSurf(RecSurfNum)%HitPtSolRefl(RecPtNum,RayNum) = Surface(NearestHitSurfNum)%ShadowSurfDiffuseSolRefl
        END IF
      ELSE
        ! No obstructions were hit by this ray
        SolReflRecSurf(RecSurfNum)%HitPtSurfNum(RecPtNum,RayNum) = 0
        ! If ray is going downward find the hit point on the ground plane if the receiving point
        ! is above ground level; note that GroundLevelZ is <= 0.0
        IF(RayVec(3) < 0.0d0 .AND. SolReflRecSurf(RecSurfNum)%RecPt(3,RecPtNum) > GroundLevelZ) THEN
          ! Ray hits ground
          Alfa = ACOS(-RayVec(3))
          Beta = ATAN2(RayVec(2),RayVec(1))
          HorDis = (RecPt(3)-GroundLevelZ)*TAN(Alfa)
          GroundHitPt(3) = GroundLevelZ
          GroundHitPt(1) = RecPt(1) + HorDis*COS(Beta)
          GroundHitPt(2) = RecPt(2) + HorDis*SIN(Beta)
          SolReflRecSurf(RecSurfNum)%HitPt(1:3,RecPtNum,RayNum) = GroundHitPt(1:3)
          SolReflRecSurf(RecSurfNum)%HitPtSurfNum(RecPtNum,RayNum) = -1
          SolReflRecSurf(RecSurfNum)%RecPtHitPtDis(RecPtNum,RayNum) = (RecPt(3)-GroundLevelZ)/(-RayVec(3))
          SolReflRecSurf(RecSurfNum)%HitPtSolRefl(RecPtNum,RayNum) = GndReflectance
          SolReflRecSurf(RecSurfNum)%HitPtNormVec(1,RecPtNum,RayNum) = 0.0d0
          SolReflRecSurf(RecSurfNum)%HitPtNormVec(2,RecPtNum,RayNum) = 0.0d0
          SolReflRecSurf(RecSurfNum)%HitPtNormVec(3,RecPtNum,RayNum) = 1.0d0
        END IF  ! End of check if ray hits ground
      END IF  ! End of check if obstruction hit
    END DO  ! End of RayNum loop
  END DO  ! End of receiving point loop
END DO  ! End of receiving surface loop

RETURN
END SUBROUTINE InitSolReflRecSurf

!=====================================================================================================

SUBROUTINE CalcBeamSolDiffuseReflFactors

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   September 2003
          !       MODIFIED       TH 4/6/2010, fixed CR 7872
          !       RE-ENGINEERED  B. Griffith, October 2012, for timestep integrated solar

          ! PURPOSE OF THIS SUBROUTINE:
          ! manage calculations for factors for irradiance on exterior heat transfer surfaces due to
          ! beam-to-diffuse solar reflection from obstructions and ground.

          ! METHODOLOGY EMPLOYED: call worker routine depending on solar calculation method

          ! REFERENCES: na

          ! USE STATEMENTS: na
  USE DataGlobals,         ONLY: HourOfDay
  USE DataSystemVariables, ONLY: DetailedSolarTimestepIntegration

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS: na
          ! INTERFACE BLOCK SPECIFICATIONS: na
          ! DERIVED TYPE DEFINITIONS: na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: IHr                  =0   ! Hour number

          ! FLOW:

  IF (.NOT. DetailedSolarTimestepIntegration) THEN
    IF(BeginSimFlag) THEN
      CALL DisplayString('Calculating Beam-to-Diffuse Exterior Solar Reflection Factors')
    ELSE
      CALL DisplayString('Updating Beam-to-Diffuse Exterior Solar Reflection Factors')
    END IF
    ReflFacBmToDiffSolObs = 0.d0
    ReflFacBmToDiffSolGnd = 0.d0
    DO IHr = 1,24
      CALL FigureBeamSolDiffuseReflFactors(IHr)
    ENDDO  ! End of IHr loop
  ELSE ! timestep integrated solar, use current hour of day
    ReflFacBmToDiffSolObs(1:TotSurfaces,HourOfDay) = 0.d0
    ReflFacBmToDiffSolGnd(1:TotSurfaces,HourOfDay) = 0.d0
    CALL FigureBeamSolDiffuseReflFactors(HourOfDay)
  ENDIF

  RETURN
END SUBROUTINE CalcBeamSolDiffuseReflFactors

SUBROUTINE FigureBeamSolDiffuseReflFactors(iHour)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann, derived from original CalcBeamSolDiffuseReflFactors
          !       DATE WRITTEN   September 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  B. Griffith, October 2012, revised for timestep integrated solar

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates factors for irradiance on exterior heat transfer surfaces due to
          ! beam-to-diffuse solar reflection from obstructions and ground.

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: iHour

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: SunVec(3)            =0.0d0 ! Unit vector to sun
  INTEGER   :: RecSurfNum           =0   ! Receiving surface number
  INTEGER   :: SurfNum              =0   ! Heat transfer surface number corresponding to RecSurfNum
  INTEGER   :: RecPtNum             =0   ! Receiving point number
  INTEGER   :: NumRecPts            =0   ! Number of receiving points on a receiving surface
  INTEGER   :: HitPtSurfNum         =0   ! Surface number of hit point: -1 = ground,
                                    ! 0 = sky or obstruction with receiving point below ground level,
                                    ! >0 = obstruction with receiving point above ground level
  REAL(r64) :: ReflBmToDiffSolObs(MaxRecPts) ! Irradiance at a receiving point for
                                            ! beam solar diffusely reflected from obstructions, divided by
                                            ! beam normal irradiance
  REAL(r64) :: ReflBmToDiffSolGnd(MaxRecPts) ! Irradiance at a receiving point for
                                            ! beam solar diffusely reflected from the ground, divided by
                                            ! beam normal irradiance
  INTEGER   :: RayNum               =0   ! Ray number
  INTEGER   :: IHit                 =0   ! > 0 if obstruction is hit; otherwise = 0
  REAL(r64) :: OriginThisRay(3)     =0.0d0 ! Origin point of a ray (m)
  REAL(r64) :: ObsHitPt(3)          =0.0d0 ! Hit point on obstruction (m)
  INTEGER   :: ObsSurfNum           =0   ! Obstruction surface number
  REAL(r64) :: CosIncBmAtHitPt      =0.0d0 ! Cosine of incidence angle of beam solar at hit point
  REAL(r64) :: CosIncBmAtHitPt2     =0.0d0 ! Cosine of incidence angle of beam solar at hit point,
                                         !  the mirrored shading surface
  REAL(r64) :: BmReflSolRadiance    =0.0d0 ! Solar radiance at hit point due to incident beam, divided
                                         !  by beam normal irradiance
  REAL(r64) :: dReflBeamToDiffSol   =0.0d0 ! Contribution to reflection factor at a receiving point
                                         !  from beam solar reflected from a hit point
  REAL(r64) :: SunLitFract          =0.0d0 ! Sunlit fraction

  ReflBmToDiffSolObs    = 0.d0
  ReflBmToDiffSolGnd    = 0.d0

  ! Unit vector to sun
  SunVec = SunCosHr(1:3,iHour)

  ! loop through each surface that can receive beam solar reflected as diffuse solar from other surfaces
  DO RecSurfNum = 1,TotSolReflRecSurf
    SurfNum = SolReflRecSurf(RecSurfNum)%SurfNum

    DO RecPtNum = 1, SolReflRecSurf(RecSurfNum)%NumRecPts
      ReflBmToDiffSolObs(RecPtNum) = 0.0d0
      ReflBmToDiffSolGnd(RecPtNum) = 0.0d0

      DO RayNum = 1, SolReflRecSurf(RecSurfNum)%NumReflRays
        HitPtSurfNum = SolReflRecSurf(RecSurfNum)%HitPtSurfNum(RecPtNum,RayNum)

        ! Skip rays that do not hit an obstruction or ground.
        ! (Note that if a downgoing ray does not hit an obstruction it will have HitPtSurfNum = 0
        ! if the receiving point is below ground level (see subr. InitSolReflRecSurf); this means
        ! that a below-ground-level receiving point receives no ground-reflected radiation although
        ! it is allowed to receive obstruction-reflected solar radiation and direct (unreflected)
        ! beam and sky solar radiation. As far as reflected solar is concerned, the program does
        ! not handle a sloped ground plane or a horizontal ground plane whose level is different
        ! from one side of the building to another.)
        IF(HitPtSurfNum == 0) CYCLE  ! Ray hits sky or obstruction with receiving pt. below ground level

        IF(HitPtSurfNum > 0) THEN
          ! Skip rays that hit a daylighting shelf, from which solar reflection is calculated separately.
          IF(Surface(HitPtSurfNum)%Shelf > 0) CYCLE

          ! Skip rays that hit a window
          ! If hit point's surface is a window or glass door go to next ray since it is assumed for now
          ! that windows have only beam-to-beam, not beam-to-diffuse, reflection
          ! TH 3/29/2010. Code modified and moved
          IF(Surface(HitPtSurfNum)%Class == SurfaceClass_Window .OR. &
             Surface(HitPtSurfNum)%Class == SurfaceClass_GLASSDOOR) CYCLE

          ! Skip rays that hit non-sunlit surface. Assume first time step of the hour.
          SunlitFract = SunlitFrac(HitPtSurfNum,iHour,1)

          ! If hit point's surface is not sunlit go to next ray
          ! TH 3/25/2010. why limit to HeatTransSurf? shading surfaces should also apply
          !IF(Surface(HitPtSurfNum)%HeatTransSurf .AND. SunlitFract < 0.01d0) CYCLE
          IF(SunlitFract < 0.01d0) CYCLE

          ! TH 3/26/2010. If the hit point falls into the shadow even though SunlitFract > 0, can Cycle.
          !  This cannot be done now, therefore there are follow-up checks of blocking sun ray
          !   from the hit point.

          ! TH 3/29/2010. Code modified and moved up
          ! If hit point's surface is a window go to next ray since it is assumed for now
          ! that windows have only beam-to-beam, not beam-to-diffuse, reflection
          !IF(Surface(HitPtSurfNum)%Construction > 0) THEN
          !  IF(Construct(Surface(HitPtSurfNum)%Construction)%TypeIsWindow) CYCLE
          !END IF
        END IF

        ! Does an obstruction block the vector from this ray's hit point to the sun?
        IHit = 0
        OriginThisRay = SolReflRecSurf(RecSurfNum)%HitPt(1:3,RecPtNum,RayNum)

        ! Note: if sun is in back of hit surface relative to receiving point, CosIncBmAtHitPt will be < 0
        CosIncBmAtHitPt = DOT_PRODUCT(SolReflRecSurf(RecSurfNum)%HitPtNormVec(1:3,RecPtNum,RayNum),SunVec)
        IF(CosIncBmAtHitPt <= 0.0d0) CYCLE

        ! CR 7872 - TH 4/6/2010. The shading surfaces should point to the receiveing heat transfer surface
        !  according to the the right hand rule. If user inputs do not follow the rule, use the following
        !  code to check the mirrored shading surface
        IF (HitPtSurfNum >0) THEN
          IF (Surface(HitPtSurfNum)%ShadowingSurf) THEN
            IF (HitPtSurfNum+1 < TotSurfaces) THEN
              IF (Surface(HitPtSurfNum+1)%ShadowingSurf .AND. Surface(HitPtSurfNum+1)%MirroredSurf) THEN
                ! Check whether the sun is behind the mirrored shading surface
                CosIncBmAtHitPt2 = DOT_PRODUCT(Surface(HitPtSurfNum+1)%OutNormVec,SunVec)
                IF(CosIncBmAtHitPt2 >= 0.0d0) CYCLE
              ENDIF
            ENDIF
          ENDIF
        ENDIF

        ! TH 3/25/2010. CR 7872. Seems should loop over all possible obstructions for the HitPtSurfNum
        !  rather than RecSurfNum, because if the HitPtSurfNum is a shading surface,
        !  it does not belong to SolReflRecSurf which only contain heat transfer surfaces
        !  that can receive reflected solar (ExtSolar = True)!

        ! To speed up, ideally should store all possible shading surfaces for the HitPtSurfNum
        !  obstruction surface in the SolReflSurf(HitPtSurfNum)%PossibleObsSurfNums(loop) array as well
        DO ObsSurfNum = 1, TotSurfaces
!        DO loop = 1,SolReflRecSurf(RecSurfNum)%NumPossibleObs
!          ObsSurfNum = SolReflRecSurf(RecSurfNum)%PossibleObsSurfNums(loop)

          !CR 8959 -- The other side of a mirrored surface cannot obstruct the mirrored surface
          IF (HitPtSurfNum >0) THEN
            IF (Surface(HitPtSurfNum)%MirroredSurf) THEN
              IF (ObsSurfNum == HitPtSurfNum-1) CYCLE
            END IF
          END IF

          ! skip the hit surface
          IF (ObsSurfNum == HitPtSurfNum) CYCLE

          ! skip mirrored surfaces
          IF (Surface(ObsSurfNum)%MirroredSurf) CYCLE
          !IF(Surface(ObsSurfNum)%ShadowingSurf .AND. Surface(ObsSurfNum)%Name(1:3) == 'Mir') THEN
          !  CYCLE
          !ENDIF

          ! skip interior surfaces
          IF(Surface(ObsSurfNum)%ExtBoundCond >= 1) CYCLE


          ! For now it is assumed that obstructions that are shading surfaces are opaque.
          ! An improvement here would be to allow these to have transmittance.
          CALL PierceSurface(ObsSurfNum, OriginThisRay, SunVec, IHit, ObsHitPt)
          IF (IHit > 0) EXIT  ! An obstruction was hit
        END DO
        IF(IHit > 0) CYCLE    ! Sun does not reach this ray's hit point

        ! Sun reaches this ray's hit point; get beam-reflected diffuse radiance at hit point for
        ! unit beam normal solar

        !CosIncBmAtHitPt = DOT_PRODUCT(SolReflRecSurf(RecSurfNum)%HitPtNormVec(1:3,RecPtNum,RayNum),SunVec)
        ! Note: if sun is in back of hit surface relative to receiving point, CosIncBmAtHitPt will be < 0
        ! and use of MAX in following gives zero beam solar reflecting at hit point.
        !BmReflSolRadiance = MAX(0.0d0,CosIncBmAtHitPt)*SolReflRecSurf(RecSurfNum)%HitPtSolRefl(RecPtNum,RayNum)

        BmReflSolRadiance = CosIncBmAtHitPt * SolReflRecSurf(RecSurfNum)%HitPtSolRefl(RecPtNum,RayNum)

        IF(BmReflSolRadiance > 0.0d0) THEN
          ! Contribution to reflection factor from this hit point
          IF(HitPtSurfNum > 0) THEN
            ! Ray hits an obstruction
            dReflBeamToDiffSol = BmReflSolRadiance * SolReflRecSurf(RecSurfNum)%dOmegaRay(RayNum) * &
                                 SolReflRecSurf(RecSurfNum)%CosIncAngRay(RayNum)/Pi
            ReflBmToDiffSolObs(RecPtNum) = ReflBmToDiffSolObs(RecPtNum) + dReflBeamToDiffSol
          ELSE
            ! Ray hits ground (in this case we do not multiply by BmReflSolRadiance since
            ! ground reflectance and cos of incidence angle of sun on
            ! ground is taken into account later when ReflFacBmToDiffSolGnd is used)
            dReflBeamToDiffSol = SolReflRecSurf(RecSurfNum)%dOmegaRay(RayNum) * &
                                 SolReflRecSurf(RecSurfNum)%CosIncAngRay(RayNum)/Pi
            ReflBmToDiffSolGnd(RecPtNum) = ReflBmToDiffSolGnd(RecPtNum) + dReflBeamToDiffSol
          END IF
        END IF
      END DO  ! End of loop over rays from receiving point
    END DO  ! End of loop over receiving points

    ! Average over receiving points
    ReflFacBmToDiffSolObs(SurfNum,iHour) = 0.0d0
    ReflFacBmToDiffSolGnd(SurfNum,iHour) = 0.0d0
    NumRecPts = SolReflRecSurf(RecSurfNum)%NumRecPts
    DO RecPtNum = 1, NumRecPts
      ReflFacBmToDiffSolObs(SurfNum,iHour) = ReflFacBmToDiffSolObs(SurfNum,iHour) + ReflBmToDiffSolObs(RecPtNum)
      ReflFacBmToDiffSolGnd(SurfNum,iHour) = ReflFacBmToDiffSolGnd(SurfNum,iHour) + ReflBmToDiffSolGnd(RecPtNum)
    END DO
    ReflFacBmToDiffSolObs(SurfNum,iHour) = ReflFacBmToDiffSolObs(SurfNum,iHour)/NumRecPts
    ReflFacBmToDiffSolGnd(SurfNum,iHour) = ReflFacBmToDiffSolGnd(SurfNum,iHour)/NumRecPts

    ! Do not allow ReflFacBmToDiffSolGnd to exceed the surface's unobstructed ground view factor
    ReflFacBmToDiffSolGnd(SurfNum,iHour) = MIN(0.5d0*(1.d0-Surface(SurfNum)%CosTilt),  &
                                             ReflFacBmToDiffSolGnd(SurfNum,iHour))
    ! Note: the above factors are dimensionless; they are equal to
    ! (W/m2 reflected solar incident on SurfNum)/(W/m2 beam normal solar)
  END DO  ! End of loop over receiving surfaces

  RETURN

END SUBROUTINE FigureBeamSolDiffuseReflFactors


!=================================================================================================

SUBROUTINE CalcBeamSolSpecularReflFactors

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   September 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  B. Griffith, October 2012, for timestep integrated solar

          ! PURPOSE OF THIS SUBROUTINE:
          ! Manage calculation of factors for beam solar irradiance on exterior heat transfer surfaces due to
          ! specular (beam-to-beam) reflection from obstructions such as a highly-glazed neighboring
          ! building.

          ! METHODOLOGY EMPLOYED:
          ! call worker routine as appropriate

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE DataGlobals,         ONLY: HourOfDay
  USE DataSystemVariables, ONLY: DetailedSolarTimestepIntegration

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS: na
          ! INTERFACE BLOCK SPECIFICATIONS: na
          ! DERIVED TYPE DEFINITIONS: na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: IHr                  =0   ! Hour number

          ! FLOW:
  IF (.NOT. DetailedSolarTimestepIntegration) THEN
    IF(BeginSimFlag) THEN
      CALL DisplayString('Calculating Beam-to-Beam Exterior Solar Reflection Factors')
    ELSE
      CALL DisplayString('Updating Beam-to-Beam Exterior Solar Reflection Factors')
    END IF
    ReflFacBmToBmSolObs   = 0.d0
    CosIncAveBmToBmSolObs = 0.d0
    DO IHr = 1,24
      CALL FigureBeamSolSpecularReflFactors(IHr)
    END DO  ! End of IHr loop
  ELSE ! timestep integrated solar, use current hour of day
    ReflFacBmToBmSolObs(1:TotSurfaces,HourOfDay)   = 0.d0
    CosIncAveBmToBmSolObs(1:TotSurfaces,HourOfDay) = 0.d0
    CALL FigureBeamSolSpecularReflFactors(HourOfDay)
  ENDIF

  RETURN
END SUBROUTINE CalcBeamSolSpecularReflFactors

SUBROUTINE FigureBeamSolSpecularReflFactors(iHour)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   September 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  B. Griffith, October 2012, for timestep integrated solar

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates factors for beam solar irradiance on exterior heat transfer surfaces due to
          ! specular (beam-to-beam) reflection from obstructions such as a highly-glazed neighboring
          ! building. Specular reflection can occur from shading surfaces with non-zero specular
          ! reflectance and from exterior windows of the building (in calculating reflection from
          ! these windows, they are assumed to have no shades or blinds).
          ! Reflection from the ground and opaque building surfaces is assumed to be totally diffuse,
          ! i.e. these surfaces has no specular reflection component.

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General,             ONLY: POLYF

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: iHour

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: loop                 =0   ! DO loop indices
  INTEGER           :: loop2                =0   ! DO loop indices
  REAL(r64)         :: SunVec(3)            =0.0d0 ! Unit vector to sun
  REAL(r64)         :: SunVecMir(3)         =0.0d0 ! Unit vector to sun mirrored by a reflecting surface
  INTEGER           :: RecSurfNum           =0   ! Receiving surface number
  INTEGER           :: SurfNum              =0   ! Heat transfer surface number corresponding to RecSurfNum
  INTEGER           :: NumRecPts            =0   ! Number of receiving points on a receiving surface
  INTEGER           :: RecPtNum             =0   ! Receiving point number
  REAL(r64)         :: RecPt(3)             =0.0d0 ! Receiving point (m)
  REAL(r64)         :: HitPtRefl(3)         =0.0d0 ! Hit point on a reflecting surface (m)
  REAL(r64)         :: ReflBmToDiffSolObs(MaxRecPts) ! Irradiance at a receiving point for
                                            ! beam solar diffusely reflected from obstructions, divided by
                                            ! beam normal irradiance
!unused  INTEGER           :: RayNum               =0   ! Ray number
  INTEGER           :: IHitRefl             =0   ! > 0 if reflecting surface is hit; otherwise = 0
  INTEGER           :: IHitObs              =0   ! > 0 if obstruction is hit
  REAL(r64)         :: HitPtObs(3)          =0.0d0 ! Hit point on obstruction (m)
  INTEGER           :: IHitObsRefl          =0   ! > 0 if obstruction hit between rec. pt. and reflection point
  INTEGER           :: ObsSurfNum           =0   ! Obstruction surface number
  INTEGER           :: ReflSurfNum          =0   ! Reflecting surface number
  INTEGER           :: ReflSurfRecNum       =0   ! Receiving surface number corresponding to a reflecting surface number
  REAL(r64)         :: ReflNorm(3)          =0.0d0 ! Unit normal to reflecting surface
  REAL(r64)         :: ReflBmToBmSolObs(MaxRecPts) ! Irradiance at a receiving point for
                                            ! beam solar specularly reflected from obstructions, divided by
                                            ! beam normal irradiance
  REAL(r64)         :: ReflDistance         =0.0d0 ! Distance from receiving point to hit point on a reflecting surface (m)
  REAL(r64)         :: ObsDistance          =0.0d0 ! Distance from receiving point to hit point on an obstruction (m)
  REAL(r64)         :: SpecReflectance      =0.0d0 ! Specular reflectance of a reflecting surface
  INTEGER           :: ConstrNumRefl        =0   ! Construction number of a reflecting surface
  REAL(r64)         :: CosIncAngRefl        =0.0d0 ! Cosine of incidence angle of beam on reflecting surface
  REAL(r64)         :: CosIncAngRec         =0.0d0 ! Angle of incidence of reflected beam on receiving surface
  REAL(r64)         :: ReflFac              =0.0d0 ! Contribution to specular reflection factor
  REAL(r64)         :: ReflFacTimesCosIncSum(MaxRecPts) ! Sum of ReflFac times CosIncAngRefl
  REAL(r64)         :: CosIncWeighted       =0.0d0 ! Cosine of incidence angle on receiving surf weighted by reflection factor


  ReflBmToDiffSolObs    = 0.0d0
  ReflFacTimesCosIncSum = 0.0d0

  IF(SunCosHr(3,iHour) < SunIsUpValue) RETURN  ! Skip if sun is below horizon

  ! Unit vector to sun
  SunVec = SunCosHr(1:3,iHour)

  DO RecSurfNum = 1,TotSolReflRecSurf
    SurfNum = SolReflRecSurf(RecSurfNum)%SurfNum
    IF(SolReflRecSurf(RecSurfNum)%NumPossibleObs > 0) THEN
      ReflBmToBmSolObs = 0.0d0
      ReflFacTimesCosIncSum = 0.0d0
      ! Find possible reflecting surfaces for this receiving surface
      DO loop = 1, SolReflRecSurf(RecSurfNum)%NumPossibleObs
        ReflSurfNum = SolReflRecSurf(RecSurfNum)%PossibleObsSurfNums(loop)
        ! Keep windows; keep shading surfaces with specular reflectance
        IF((Surface(ReflSurfNum)%Class == SurfaceClass_Window .AND. Surface(ReflSurfNum)%ExtSolar) .OR. &
            (Surface(ReflSurfNum)%ShadowSurfGlazingFrac > 0.0d0 .AND. &
            Surface(ReflSurfNum)%ShadowingSurf)) THEN
          ! Skip if window and not sunlit
          IF(Surface(ReflSurfNum)%Class == SurfaceClass_Window .AND. SunlitFrac(ReflSurfNum,iHour,1) < 0.01d0) CYCLE
          ! Check if sun is in front of this reflecting surface.
          ReflNorm = Surface(ReflSurfNum)%OutNormVec(1:3)
          CosIncAngRefl = DOT_PRODUCT(SunVec,ReflNorm)
          IF(CosIncAngRefl < 0.0d0) CYCLE

          ! Get sun position unit vector for mirror image of sun in reflecting surface
          SunVecMir = SunVec - 2.0d0*DOT_PRODUCT(SunVec,ReflNorm)*ReflNorm
          ! Angle of incidence of reflected beam on receiving surface
          CosIncAngRec = DOT_PRODUCT(SolReflRecSurf(RecSurfNum)%NormVec,SunVecMir)
          IF(CosIncAngRec <= 0.0d0) CYCLE
          DO RecPtNum = 1,SolReflRecSurf(RecSurfNum)%NumRecPts
            ! See if ray from receiving point to mirrored sun hits the reflecting surface
            RecPt = SolReflRecSurf(RecSurfNum)%RecPt(1:3,RecPtNum)
            CALL PierceSurface(ReflSurfNum, RecPt, SunVecMir, IHitRefl, HitPtRefl)
            IF(IHitRefl > 0) THEN
              ! Reflecting surface was hit
              ReflDistance = SQRT(DOT_PRODUCT(HitPtRefl-RecPt,HitPtRefl-RecPt))
              ! Determine if ray from receiving point to hit point is obstructed
              IHitObsRefl = 0
              DO loop2 = 1,SolReflRecSurf(RecSurfNum)%NumPossibleObs
                ObsSurfNum = SolReflRecSurf(RecSurfNum)%PossibleObsSurfNums(loop2)
                IF(ObsSurfNum == ReflSurfNum .OR. ObsSurfNum == Surface(ReflSurfNum)%BaseSurf) CYCLE
                CALL PierceSurface(ObsSurfNum,RecPt,SunVecMir,IHitObs,HitPtObs)
                IF(IHitObs > 0) THEN
                  ObsDistance = SQRT(DOT_PRODUCT(HitPtObs-RecPt,HitPtObs-RecPt))
                    IF(ObsDistance < ReflDistance) THEN
                    IHitObsRefl = 1
                    EXIT
                  END IF
                END IF
              END DO
              IF(IHitObsRefl > 0) CYCLE  ! Obstruct'n closer than reflect'n pt. was hit; go to next rec. pt.
              ! There is no obstruction for this ray between rec. pt. and hit point on reflecting surface.
              ! See if ray from hit pt. on reflecting surface to original (unmirrored) sun position is obstructed
              IHitObs = 0
              IF(Surface(ReflSurfNum)%Class == SurfaceClass_Window) THEN
                ! Reflecting surface is a window.
                ! Receiving surface number for this window.
                ReflSurfRecNum = Surface(ReflSurfNum)%ShadowSurfRecSurfNum
                IF(ReflSurfRecNum > 0) THEN
                  ! Loop over possible obstructions for this window
                  DO loop2 = 1,SolReflRecSurf(ReflSurfRecNum)%NumPossibleObs
                    ObsSurfNum = SolReflRecSurf(ReflSurfRecNum)%PossibleObsSurfNums(loop2)
                    CALL PierceSurface(ObsSurfNum,HitPtRefl,SunVec,IHitObs,HitPtObs)
                    IF(IHitObs > 0) EXIT
                  END DO
                END IF
              ELSE
                ! Reflecting surface is a building shade
                DO ObsSurfNum = 1, TotSurfaces
                  IF(.NOT.Surface(ObsSurfNum)%ShadowSurfPossibleObstruction) CYCLE
                  IF(ObsSurfNum == ReflSurfNum) CYCLE

                  !TH2 CR8959 -- Skip mirrored surfaces
                  IF(Surface(ObsSurfNum)%MirroredSurf) CYCLE
                  !TH2 CR8959 -- The other side of a mirrored surface cannot obstruct the mirrored surface
                  IF (Surface(ReflSurfNum)%MirroredSurf) THEN
                    IF (ObsSurfNum == ReflSurfNum-1) CYCLE
                  END IF

                  CALL PierceSurface(ObsSurfNum,HitPtRefl,SunVec,IHitObs,HitPtObs)
                  IF(IHitObs > 0) EXIT
                END DO
              END IF

              IF(IHitObs > 0) CYCLE ! Obstruct'n hit between reflect'n hit point and sun; go to next receiving pt.

              ! No obstructions. Calculate reflected beam irradiance at receiving pt. from this reflecting surface.
              SpecReflectance = 0.0d0
              IF(Surface(ReflSurfNum)%Class == SurfaceClass_Window) THEN
                ConstrNumRefl = Surface(ReflSurfNum)%Construction
                SpecReflectance = POLYF(ABS(CosIncAngRefl),Construct(ConstrNumRefl)%ReflSolBeamFrontCoef(1:6))
              END IF
              IF(Surface(ReflSurfNum)%ShadowingSurf.AND.Surface(ReflSurfNum)%ShadowSurfGlazingConstruct > 0) THEN
                ConstrNumRefl = Surface(ReflSurfNum)%ShadowSurfGlazingConstruct
                SpecReflectance = Surface(ReflSurfNum)%ShadowSurfGlazingFrac *   &
                   POLYF(ABS(CosIncAngRefl),Construct(ConstrNumRefl)%ReflSolBeamFrontCoef(1:6))
              ENDIF
              ! Angle of incidence of reflected beam on receiving surface
              CosIncAngRec = DOT_PRODUCT(SolReflRecSurf(RecSurfNum)%NormVec,SunVecMir)
              ReflFac = SpecReflectance * CosIncAngRec
              ! Contribution to specular reflection factor
              ReflBmToBmSolObs(RecPtNum) = ReflBmToBmSolObs(RecPtNum) + ReflFac
              ReflFacTimesCosIncSum(RecPtNum) = ReflFacTimesCosIncSum(RecPtNum) + ReflFac*CosIncAngRec
            END IF  ! End of check if reflecting surface was hit
          END DO  ! End of loop over receiving points
        END IF  ! End of check if valid reflecting surface
      END DO  ! End of loop over obstructing surfaces
      ! Average over receiving points
      NumRecPts = SolReflRecSurf(RecSurfNum)%NumRecPts

      DO RecPtNum = 1,NumRecPts
        IF (ReflBmToBmSolObs(RecPtNum) /= 0.0d0) THEN
          CosIncWeighted = ReflFacTimesCosIncSum(RecPtNum) / ReflBmToBmSolObs(RecPtNum)
        ELSE
          CosIncWeighted = 0.0d0
        ENDIF
        CosIncAveBmToBmSolObs(SurfNum,iHour) = CosIncAveBmToBmSolObs(SurfNum,iHour) + CosIncWeighted
        ReflFacBmToBmSolObs(SurfNum,iHour) = ReflFacBmToBmSolObs(SurfNum,iHour) + ReflBmToBmSolObs(RecPtNum)
      END DO
      ReflFacBmToBmSolObs(SurfNum,iHour) = ReflFacBmToBmSolObs(SurfNum,iHour) / REAL(NumRecPts,r64)
      CosIncAveBmToBmSolObs(SurfNum,iHour) = CosIncAveBmToBmSolObs(SurfNum,iHour) / REAL(NumRecPts,r64)
    END IF ! End of check if number of possible obstructions > 0
  END DO  ! End of loop over receiving surfaces

  RETURN

END SUBROUTINE FigureBeamSolSpecularReflFactors

!=================================================================================================

SUBROUTINE CalcSkySolDiffuseReflFactors

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   October 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates factors for irradiance on exterior heat transfer surfaces due to
          ! reflection of sky diffuse solar radiation from obstructions and ground.

          ! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE DataSystemVariables, ONLY: DetailedSkyDiffuseAlgorithm
  USE Vectors

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS: na
          ! INTERFACE BLOCK SPECIFICATIONS: na
          ! DERIVED TYPE DEFINITIONS: na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: RecSurfNum           =0 ! Receiving surface number
  INTEGER   :: SurfNum              =0 ! Heat transfer surface number corresponding to RecSurfNum
  INTEGER   :: ObsSurfNum           =0 ! Obstruction surface number
  INTEGER   :: RecPtNum             =0 ! Receiving point number
  INTEGER   :: NumRecPts            =0 ! Number of receiving points on a receiving surface
  INTEGER   :: HitPtSurfNum         =0 ! Surface number of hit point: -1 = ground,
                                    ! 0 = sky or obstruction with receiving point below ground level,
                                    ! >0 = obstruction with receiving point above ground level
  INTEGER   :: HitPtSurfNumX        =0 ! For a shading surface, HitPtSurfNum for original surface,
                                    ! HitPitSurfNum + 1 for mirror surface
  REAL(r64) :: ReflSkySolObs(MaxRecPts) ! Irradiance at a receiving point for sky diffuse solar
                                    ! reflected from obstructions, divided by unobstructed
                                    ! sky diffuse horizontal irradiance
  REAL(r64) :: ReflSkySolGnd(MaxRecPts) ! Irradiance at a receiving point for sky diffuse solar
                                    ! reflected from ground, divided by unobstructed
                                    ! sky diffuse horizontal irradiance
  INTEGER   :: RayNum               =0 ! Ray number
  REAL(r64) :: HitPtRefl(3)         =0.0d0 ! Coordinates of hit point on obstruction or ground (m)
  INTEGER   :: IHitObs              =0 ! > 0 if obstruction is hit; otherwise = 0
  REAL(r64) :: HitPtObs(3)          =0.0d0 ! Hit point on an obstruction (m)
!unused  REAL(r64)         :: ObsHitPt(3)          =0.0 ! Hit point on obstruction (m)
  REAL(r64) :: dOmega               =0.0d0 ! Solid angle increment (steradians)
  REAL(r64) :: CosIncAngRayToSky            =0.0d0 ! Cosine of incidence angle on ground of ray to sky
  REAL(r64) :: SkyReflSolRadiance   =0.0d0 ! Reflected radiance at hit point divided by unobstructed
                                    !  sky diffuse horizontal irradiance
  REAL(r64) :: dReflSkySol          =0.0d0 ! Contribution to reflection factor at a receiving point
                                    !  from sky solar reflected from a hit point
  REAL(r64) :: Phi                  =0.0d0 ! Altitude angle and increment (radians)
  REAL(r64) :: DPhi                 =0.0d0 ! Altitude angle and increment (radians)
  REAL(r64) :: SPhi                 =0.0d0 ! Sine of Phi
  REAL(r64) :: CPhi                 =0.0d0 ! Cosine of Phi
  REAL(r64) :: Theta                =0.0d0 ! Azimuth angle (radians)
  REAL(r64) :: DTheta               =0.0d0 ! Azimuth increment (radians)
  INTEGER   :: IPhi                 =0 ! Altitude angle index
  INTEGER   :: ITheta               =0 ! Azimuth angle index
  REAL(r64) :: URay(3)              =0.0d0 ! Unit vector along ray from ground hit point
  REAL(r64) :: SurfVertToGndPt(3)   =0.0d0 ! Vector from a vertex of possible obstructing surface to ground
                                    !  hit point (m)
  REAL(r64) :: SurfVert(3)          =0.0d0 ! Surface vertex (m)
  REAL(r64) :: dReflSkyGnd          =0.0d0 ! Factor for ground radiance due to direct sky diffuse reflection
          ! FLOW:

CALL DisplayString('Calculating Sky Diffuse Exterior Solar Reflection Factors')
ReflSkySolObs=0.0d0
ReflSkySolGnd=0.0d0

DO RecSurfNum = 1,TotSolReflRecSurf
  SurfNum = SolReflRecSurf(RecSurfNum)%SurfNum
  DO RecPtNum = 1, SolReflRecSurf(RecSurfNum)%NumRecPts
    ReflSkySolObs(RecPtNum) = 0.0d0
    ReflSkySolGnd(RecPtNum) = 0.0d0
    DO RayNum = 1, SolReflRecSurf(RecSurfNum)%NumReflRays
      HitPtSurfNum = SolReflRecSurf(RecSurfNum)%HitPtSurfNum(RecPtNum,RayNum)
      ! Skip rays that do not hit an obstruction or ground.
      ! (Note that if a downgoing ray does not hit an obstruction it will have HitPtSurfNum = 0
      ! if the receiving point is below ground level (see subr. InitSolReflRecSurf); this means
      ! that a below-ground-level receiving point receives no ground-reflected radiation although
      ! it is allowed to receive obstruction-reflected solar radiation and direct (unreflected)
      ! beam and sky solar radiation. As far as reflected solar is concerned, the program does
      ! not handle a sloped ground plane or a horizontal ground plane whose level is different
      ! from one side of the building to another.)
      IF(HitPtSurfNum == 0) CYCLE  ! Ray hits sky or obstruction with receiving pt. below ground level
      HitPtRefl = SolReflRecSurf(RecSurfNum)%HitPt(1:3,RecPtNum,RayNum)
      IF(HitPtSurfNum > 0) THEN
        ! Ray hits an obstruction
        ! Skip hit points on daylighting shelves, from which solar reflection is separately calculated
        IF(Surface(HitPtSurfNum)%Shelf > 0) CYCLE
        ! Reflected radiance at hit point divided by unobstructed sky diffuse horizontal irradiance
        HitPtSurfNumX = HitPtSurfNum
        ! Each shading surface has a "mirror" duplicate surface facing in the opposite direction.
        ! The following gets the correct side of a shading surface in order to get the right value
        ! of DifShdgRatioIsoSky (the two sides can have different sky shadowing).
        IF(Surface(HitPtSurfNum)%ShadowingSurf) THEN
          IF(DOT_PRODUCT(SolReflRecSurf(RecSurfNum)%RayVec(1:3,RayNum),Surface(HitPtSurfNum)%OutNormVec)>0.0d0)THEN
            IF (HitPtSurfNum + 1 < TotSurfaces) HitPtSurfNumX = HitPtSurfNum + 1
            IF(Surface(HitPtSurfNumX)%Shelf > 0) CYCLE
          ENDIF
        END IF

        IF (.not. DetailedSkyDiffuseAlgorithm .or. .not.  ShadingTransmittanceVaries .or.  &
            SolarDistribution == MinimalShadowing) THEN
          SkyReflSolRadiance = Surface(HitPtSurfNumX)%ViewFactorSky * DifShdgRatioIsoSky(HitPtSurfNumX) *  &
                                 SolReflRecSurf(RecSurfNum)%HitPtSolRefl(RecPtNum,RayNum)
        ELSE
          SkyReflSolRadiance = Surface(HitPtSurfNumX)%ViewFactorSky * DifShdgRatioIsoSkyHRTS(HitPtSurfNumX,1,1) *  &
                                 SolReflRecSurf(RecSurfNum)%HitPtSolRefl(RecPtNum,RayNum)
        ENDIF
        dReflSkySol = SkyReflSolRadiance * SolReflRecSurf(RecSurfNum)%dOmegaRay(RayNum) * &
                                 SolReflRecSurf(RecSurfNum)%CosIncAngRay(RayNum)/Pi
        ReflSkySolObs(RecPtNum) = ReflSkySolObs(RecPtNum) + dReflSkySol
      ELSE
        ! Ray hits ground;
        ! Find radiance at hit point due to reflection of sky diffuse reaching
        ! ground directly, i.e., without reflecting from obstructions.
        ! Send rays upward from hit point and see which ones are unobstructed and so go to sky.
        ! Divide hemisphere centered at ground hit point into elements of altitude Phi and
        ! azimuth Theta and create upward-going ray unit vector at each Phi,Theta pair.
        ! Phi = 0 at the horizon; Phi = Pi/2 at the zenith.
        DPhi = PiOvr2 / (AltAngStepsForSolReflCalc/2.d0)
        dReflSkyGnd = 0.0d0
        ! Altitude loop
        DO IPhi = 1,(AltAngStepsForSolReflCalc/2)
          Phi = (IPhi - 0.5d0) * DPhi
          SPhi = SIN(Phi)
          CPhi = COS(Phi)
          ! Third component of ray unit vector in (Theta,Phi) direction
          URay(3) = SPhi
          DTheta = 2.d0*Pi / (2.d0*AzimAngStepsForSolReflCalc)
          dOmega = CPhi * DTheta * DPhi
          ! Cosine of angle of incidence of ray on ground
          CosIncAngRayToSky = SPhi
          ! Azimuth loop
          DO ITheta = 1,2*AzimAngStepsForSolReflCalc
            Theta = (ITheta - 0.5d0) * DTheta
            URay(1) = CPhi * COS(Theta)
            URay(2) = CPhi * SIN(Theta)
            ! Does this ray hit an obstruction?
            IHitObs = 0
            DO ObsSurfNum = 1, TotSurfaces
              IF(.NOT.Surface(ObsSurfNum)%ShadowSurfPossibleObstruction) CYCLE
              ! Horizontal roof surfaces cannot be obstructions for rays from ground
              IF(Surface(ObsSurfNum)%Tilt < 5.0d0) CYCLE
              IF(.NOT.Surface(ObsSurfNum)%ShadowingSurf) THEN
                IF(DOT_PRODUCT(URay,Surface(ObsSurfNum)%OutNormVec) >= 0.0d0) CYCLE
                ! Special test for vertical surfaces with URay dot OutNormVec < 0; excludes
                ! case where ground hit point is in back of ObsSurfNum
                IF(Surface(ObsSurfNum)%Tilt > 89.0d0 .AND. Surface(ObsSurfNum)%Tilt < 91.0d0) THEN
                  SurfVert = Surface(ObsSurfNum)%Vertex(2)
                  SurfVertToGndPt = HitPtRefl - SurfVert
                  IF(DOT_PRODUCT(SurfVertToGndPt,Surface(ObsSurfNum)%OutNormVec) < 0.0d0) CYCLE
                END IF
              END IF
              CALL PierceSurface(ObsSurfNum,HitPtRefl,URay,IHitObs,HitPtObs)
              IF(IHitObs > 0) EXIT
            END DO

            IF(IHitObs > 0) CYCLE ! Obstruction hit
            ! Sky is hit
            dReflSkyGnd = dReflSkyGnd + CosIncAngRayToSky*dOmega/Pi
          END DO ! End of azimuth loop
        END DO  ! End of altitude loop
        ReflSkySolGnd(RecPtNum) = ReflSkySolGnd(RecPtNum) + dReflSkyGnd *  &
          SolReflRecSurf(RecSurfNum)%dOmegaRay(RayNum) * SolReflRecSurf(RecSurfNum)%CosIncAngRay(RayNum)/Pi
      END IF  ! End of check if ray from receiving point hits obstruction or ground
    END DO  ! End of loop over rays from receiving point
  END DO  ! End of loop over receiving points

  ! Average over receiving points
  ReflFacSkySolObs(SurfNum) = 0.0d0
  ReflFacSkySolGnd(SurfNum) = 0.0d0
  NumRecPts = SolReflRecSurf(RecSurfNum)%NumRecPts
  DO RecPtNum = 1, NumRecPts
    ReflFacSkySolObs(SurfNum) = ReflFacSkySolObs(SurfNum) + ReflSkySolObs(RecPtNum)
    ReflFacSkySolGnd(SurfNum) = ReflFacSkySolGnd(SurfNum) + ReflSkySolGnd(RecPtNum)
  END DO
  ReflFacSkySolObs(SurfNum) = ReflFacSkySolObs(SurfNum)/NumRecPts
  ReflFacSkySolGnd(SurfNum) = ReflFacSkySolGnd(SurfNum)/NumRecPts
  ! Do not allow ReflFacBmToDiffSolGnd to exceed the surface's unobstructed ground view factor
  ReflFacSkySolGnd(SurfNum) = MIN(0.5d0*(1.0d0-Surface(SurfNum)%CosTilt),  &
                                           ReflFacSkySolGnd(SurfNum))
  ! Note: the above factors are dimensionless; they are equal to
  ! (W/m2 reflected solar incident on SurfNum)/(W/m2 unobstructed horizontal sky diffuse irradiance)
END DO  ! End of loop over receiving surfaces

RETURN
END SUBROUTINE CalcSkySolDiffuseReflFactors


!=================================================================================================

SUBROUTINE CrossProduct(A, B, C)

  ! Cross product between vectors A and B

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: A(3),B(3),C(3)          ! Vector components: C = A X B

          ! FLOW:
  C(1) = A(2) * B(3) - A(3) * B(2)
  C(2) = A(3) * B(1) - A(1) * B(3)
  C(3) = A(1) * B(2) - A(2) * B(1)

  RETURN

END SUBROUTINE CrossProduct

SUBROUTINE PierceSurface(ISurf, R1, RN, IPIERC, CPhit)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   July 1997
          !       MODIFIED       Sept 2003, FCW: modification of Daylighting routine DayltgPierceSurface
          !       RE-ENGINEERED  na


          ! PURPOSE OF THIS SUBROUTINE:
          ! Returns point CPhit that line through point R1 in direction of unit vector RN intersects
          ! the plan of surface ISurf. IPIERC = 1 if CPhit is inside the perimeter of ISurf. If not,
          ! IPIERC = 0. This routine works for convex and concave surfaces with 3 or more vertices.
          !
          ! METHODOLOGY EMPLOYED:


          ! REFERENCES:
          ! Based on DOE-2.1E subroutine DPIERC.


          ! USE STATEMENTS:na


  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE PARAMETER DEFINITIONS:na
          ! INTERFACE BLOCK SPECIFICATIONS:na
          ! DERIVED TYPE DEFINITIONS:na


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: ISurf      ! Surface index
  REAL(r64), INTENT(IN)        :: R1(3)      ! Point from which ray originates
  REAL(r64), INTENT(IN)        :: RN(3)      ! Unit vector along in direction of ray whose
                                             !  intersection with surface is to be determined
  INTEGER, INTENT(OUT)         :: IPIERC     ! =1 if line through point R1 in direction of unit vector
                                             !  RN intersects surface ISurf; =0 otherwise.
  REAL(r64), INTENT(OUT)       :: CPhit(3)   ! Point that ray along RN intersects plane of surface


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: NV                       ! Number of vertices (3 or 4)
  REAL(r64) :: AXC(3)                   ! Cross product of A and C
  REAL(r64) :: SN(3)                    ! Vector normal to surface (SN = A1 X A2)
!unused  REAL(r64) :: AA(3)                    ! AA(I) = A(N,I)
!unused  REAL(r64) :: CC(3)                    ! CC(I) = C(N,I)
  REAL(r64) :: CCC(3)                   ! Vector from vertex 2 to CP
  REAL(r64) :: AAA(3)                   ! Vector from vertex 2 to vertex 1
  REAL(r64) :: BBB(3)                   ! Vector from vertex 2 to vertex 3
  INTEGER   :: N                        ! Vertex loop index
  REAL(r64) :: F1,F2                    ! Intermediate variables
  REAL(r64) :: SCALE                    ! Scale factor
  REAL(r64) :: DOTCB                    ! Dot product of vectors CCC and BBB
  REAL(r64) :: DOTCA                    ! Dot product of vectors CCC and AAA
!unused  REAL(r64) :: DOTAXCSN                 ! Dot product of vectors AXC and SN


  REAL(r64), ALLOCATABLE, SAVE, DIMENSION(:,:) :: V     ! Vertices of surfaces
  REAL(r64), ALLOCATABLE, SAVE, DIMENSION(:,:) :: A     ! Vertex-to-vertex vectors; A(1,i) is from vertex 1 to 2, etc.
  REAL(r64), ALLOCATABLE, SAVE, DIMENSION(:,:) :: C     ! Vectors from vertices to intersection point
  LOGICAL,SAVE :: FirstTime=.true.


          ! FLOW:
  IPIERC = 0


  ! Vertex vectors
  IF (FirstTime) THEN
    ALLOCATE(V(MaxVerticesPerSurface,3))
    V=0.0d0
    ALLOCATE(A(MaxVerticesPerSurface,3))
    A=0.0d0
    ALLOCATE(C(MaxVerticesPerSurface,3))
    C=0.0d0
    FirstTime=.false.
  ENDIF


  ! Set the first two V & A
  V(1,1) = Surface(ISurf)%Vertex(1)%X
  V(1,2) = Surface(ISurf)%Vertex(1)%Y
  V(1,3) = Surface(ISurf)%Vertex(1)%Z


  A(1,1) = Surface(ISurf)%Vertex(2)%X - V(1,1)
  A(1,2) = Surface(ISurf)%Vertex(2)%Y - V(1,2)
  A(1,3) = Surface(ISurf)%Vertex(2)%Z - V(1,3)


  V(2,1) = Surface(ISurf)%Vertex(2)%X
  V(2,2) = Surface(ISurf)%Vertex(2)%Y
  V(2,3) = Surface(ISurf)%Vertex(2)%Z


  A(2,1) = Surface(ISurf)%Vertex(3)%X - V(2,1)
  A(2,2) = Surface(ISurf)%Vertex(3)%Y - V(2,2)
  A(2,3) = Surface(ISurf)%Vertex(3)%Z - V(2,3)


  ! Vector normal to surface
  SN(1) = A(1,2) * A(2,3) - A(1,3) * A(2,2)
  SN(2) = A(1,3) * A(2,1) - A(1,1) * A(2,3)
  SN(3) = A(1,1) * A(2,2) - A(1,2) * A(2,1)


  ! Scale factor, the solution of SN.(CPhit-V2) = 0 and
  ! CPhit = R1 + SCALE*RN, where CPhit is the point that RN,
  ! when extended, intersects the plane of the surface.
  F2 = DOT_PRODUCT(SN, RN)
  IF (ABS(F2) < 0.01d0) RETURN   ! Skip surfaces that are parallel to RN
  F1 = SN(1)*(V(2,1)-R1(1)) + SN(2)*(V(2,2)-R1(2)) + SN(3)*(V(2,3)-R1(3))
  !F1 = DOT_PRODUCT(SN, V2 - R1)
  SCALE = F1 / F2
  IF (SCALE <= 0.0d0) RETURN  ! Skip surfaces that RN points away from
  CPhit = R1 + RN * SCALE  ! Point that RN intersects plane of surface


  ! Two cases: rectangle and non-rectangle; do rectangle
  ! first since most common shape and faster calculation
  IF (Surface(ISurf)%Shape == Rectangle .OR. Surface(ISurf)%Shape == RectangularDoorWindow .or.         &
      Surface(ISurf)%Shape == RectangularOverhang .OR. Surface(ISurf)%Shape == RectangularLeftFin .or.  &
      Surface(ISurf)%Shape == RectangularRightFin) THEN
    !
    ! Surface is rectangular
    !
    ! Vectors from vertex 2 to vertex 1 and vertex 2 to vertex 3


    ! Intersection point, CCC, is inside rectangle if
    ! 0 < CCC.BBB < BBB.BBB AND 0 < CCC.AAA < AAA.AAA


    !CCC = CPhit - V2  ! Vector from vertex 2 to CPhit
    CCC(1) = CPhit(1) - V(2,1)
    CCC(2) = CPhit(2) - V(2,2)
    CCC(3) = CPhit(3) - V(2,3)


    ! Set third V just for here
    V(3,1) = Surface(ISurf)%Vertex(3)%X
    V(3,2) = Surface(ISurf)%Vertex(3)%Y
    V(3,3) = Surface(ISurf)%Vertex(3)%Z


    !BBB = V3 - V2
    BBB(1) = V(3,1) - V(2,1)
    BBB(2) = V(3,2) - V(2,2)
    BBB(3) = V(3,3) - V(2,3)


    DOTCB = DOT_PRODUCT(CCC, BBB)
    IF (DOTCB < 0.0d0) RETURN
    IF (DOTCB > DOT_PRODUCT(BBB,BBB)) RETURN


    !AAA = V1 - V2
    AAA(1) = V(1,1) - V(2,1)
    AAA(2) = V(1,2) - V(2,2)
    AAA(3) = V(1,3) - V(2,3)


    DOTCA = DOT_PRODUCT(CCC, AAA)
    IF (DOTCA < 0.0d0) RETURN
    IF (DOTCA > DOT_PRODUCT(AAA,AAA)) RETURN
    ! Surface is intersected
    IPIERC = 1


 ELSE
    !
    ! Surface is not rectangular
    !


    !if (NV == 3) then
    !else ! NV=4
    !endif


    ! First two of V & A already set
    ! test first vertex:
    C(1,1) = CPhit(1) - V(1,1)
    C(1,2) = CPhit(2) - V(1,2)
    C(1,3) = CPhit(3) - V(1,3)
    AXC(1) = A(1,2) * C(1,3) - A(1,3) * C(1,2)
    AXC(2) = A(1,3) * C(1,1) - A(1,1) * C(1,3)
    AXC(3) = A(1,1) * C(1,2) - A(1,2) * C(1,1)
    IF (DOT_PRODUCT(AXC,SN) < 0.0d0) RETURN  ! If at least one dot product is negative, intersection outside of surface


    ! test second vertex:
    C(2,1) = CPhit(1) - V(2,1)
    C(2,2) = CPhit(2) - V(2,2)
    C(2,3) = CPhit(3) - V(2,3)
    AXC(1) = A(2,2) * C(2,3) - A(2,3) * C(2,2)
    AXC(2) = A(2,3) * C(2,1) - A(2,1) * C(2,3)
    AXC(3) = A(2,1) * C(2,2) - A(2,2) * C(2,1)
    IF (DOT_PRODUCT(AXC,SN) < 0.0d0) RETURN  ! If at least one dot product is negative, intersection outside of surface


    NV = Surface(ISurf)%Sides
    ! Since first two of V & A already set, start with 3.  (so if NV=3, this loop won't happen)
    DO N = 3,NV-1
      V(N,1) = Surface(ISurf)%Vertex(N)%X
      V(N,2) = Surface(ISurf)%Vertex(N)%Y
      V(N,3) = Surface(ISurf)%Vertex(N)%Z

      A(N,1) = Surface(ISurf)%Vertex(N+1)%X - V(N,1)
      A(N,2) = Surface(ISurf)%Vertex(N+1)%Y - V(N,2)
      A(N,3) = Surface(ISurf)%Vertex(N+1)%Z - V(N,3)


      C(N,1) = CPhit(1) - V(N,1)
      C(N,2) = CPhit(2) - V(N,2)
      C(N,3) = CPhit(3) - V(N,3)


      AXC(1) = A(N,2) * C(N,3) - A(N,3) * C(N,2)
      AXC(2) = A(N,3) * C(N,1) - A(N,1) * C(N,3)
      AXC(3) = A(N,1) * C(N,2) - A(N,2) * C(N,1)


      IF (DOT_PRODUCT(AXC,SN) < 0.0d0) RETURN  ! If at least one dot product is negative, intersection outside of surface


    END DO


    ! Last vertex (NV=3 or NV=4)
    V(NV,1) = Surface(ISurf)%Vertex(NV)%X
    V(NV,2) = Surface(ISurf)%Vertex(NV)%Y
    V(NV,3) = Surface(ISurf)%Vertex(NV)%Z

    A(NV,1) = V(1,1) - V(NV,1)
    A(NV,2) = V(1,2) - V(NV,2)
    A(NV,3) = V(1,3) - V(NV,3)

    C(NV,1) = CPhit(1) - V(NV,1)
    C(NV,2) = CPhit(2) - V(NV,2)
    C(NV,3) = CPhit(3) - V(NV,3)


    AXC(1) = A(NV,2) * C(NV,3) - A(NV,3) * C(NV,2)
    AXC(2) = A(NV,3) * C(NV,1) - A(NV,1) * C(NV,3)
    AXC(3) = A(NV,1) * C(NV,2) - A(NV,2) * C(NV,1)

    IF (DOT_PRODUCT(AXC,SN) < 0.0d0) RETURN  ! If at least one dot product is negative, intersection outside of surface


    IPIERC = 1      ! Surface is intersected
  END IF


  RETURN

END SUBROUTINE PierceSurface

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

END MODULE SolarReflectionManager