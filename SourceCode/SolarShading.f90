MODULE SolarShading

          ! MODULE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   March 1997
          !       MODIFIED       December 1998, FCW
          !       MODIFIED       July 1999, Linda Lawrie, eliminate shadefl.scr,
          !                      do shadowing calculations during simulation
          !       MODIFIED       June 2001, FCW, handle window blinds
          !       MODIFIED       May 2004, LKL, Polygons > 4 sides (not subsurfaces)
          !       MODIFIED       January 2007, LKL, Taking parameters back to original integer (HC)
          !       MODIFIED       August 2011, JHK, Including Complex Fenestration optical calculations
          !       MODIFIED       November 2012, BG, Timestep solar and daylighting calculations
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! The purpose of this module is to encompass the routines and data
          ! which are need to perform the solar calculations in EnergyPlus.
          ! This also requires that shading and geometry routines and data
          ! which are used by the solar calculations be included in this module.

          ! METHODOLOGY EMPLOYED:
          ! Many of the methods used in this module have been carried over from the
          ! (I)BLAST program.  As such, there is not much documentation on the
          ! methodology used.  The original code was written mainly by George
          ! Walton and requires coordinate transformations.  It calculates
          ! shading using an overlapping polygon approach.

          ! REFERENCES:
          ! TARP Manual, NIST Publication.
          ! Passive Solar Extension of the BLAST Program, CERL/UIUC Publication.

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals
USE DataEnvironment
USE DataHeatBalance
USE DataSurfaces
USE DataShadowingCombinations
USE DaylightingManager, ONLY:ProfileAngle
USE SolarReflectionManager
USE DataReportingFlags
USE DataInterfaces
USE DataBSDFWindow    , ONLY: SUNCOSTS , MaxBkSurf , ComplexWind
USE DataVectorTypes
USE DataTimings

IMPLICIT NONE    ! Enforce explicit typing of all variables

PRIVATE

          ! MODULE PARAMETER DEFINITIONS:
          ! General Parameters...
REAL(r64), PARAMETER :: SmallIncrement=1.0d-10     ! Small increment added for shading/sunlit area calculations.
REAL(r64), PARAMETER :: HCMULT = 100000.d0   ! Multiplier used to change meters to .01 millimeters for homogeneous coordinates.
     ! Homogeneous Coordinates are represented in integers (64 bit). This changes the surface coordinates from meters
     ! to .01 millimeters -- making that the resolution for shadowing, polygon clipping, etc.
REAL(r64), PARAMETER :: sqHCMULT = HCMULT*HCMULT   ! Square of HCMult used in Homogeneous coordinates
REAL(r64), PARAMETER :: kHCMULT = 1.0d0/(HCMULT*HCMULT)   ! half of inverse square of HCMult used in Homogeneous coordinates

!INTEGER,          PRIVATE, PARAMETER :: MAXCMB = 2000   ! Length of SHDCMB array
!INTEGER,          PARAMETER :: MAXHCS = 15000 ! 200      ! Maximum number of HC surfaces (was 56)
!INTEGER,          PARAMETER :: MAXHCV = 12      ! Maximum number of HC vertices

          ! Parameters for use with the variable OverlapStatus...
INTEGER, PARAMETER :: NoOverlap             = 1
INTEGER, PARAMETER :: FirstSurfWithinSecond = 2
INTEGER, PARAMETER :: SecondSurfWithinFirst = 3
INTEGER, PARAMETER :: PartialOverlap        = 4
INTEGER, PARAMETER :: TooManyVertices       = 5
INTEGER, PARAMETER :: TooManyFigures        = 6
CHARACTER(len=*), PARAMETER, DIMENSION(6) :: cOverLapStatus=  &
                   (/'No-Overlap         ',  &
                     '1st-Surf-within-2nd',  &
                     '2nd-Surf-within-1st',  &
                     'Partial-Overlap    ',  &
                     'Too-Many-Vertices  ',  &
                     'Too-Many-Figures   '/)

          ! DERIVED TYPE DEFINITIONS:
TYPE SurfaceErrorTracking
  INTEGER :: SurfIndex1 = 0  ! Tracking for main error message
  INTEGER :: SurfIndex2 = 0  ! Tracking for Overlapping Figure Name or Surface # 1
  INTEGER :: MiscIndex  = 0  ! Used for other pertinent information to be stored
END TYPE
          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! MODULE VARIABLE DECLARATIONS:
INTEGER   :: MAXHCV = 12      ! Maximum number of HC vertices
                                      ! (needs to be based on maxnumvertices)
INTEGER   :: MAXHCS = 15000 ! 200      ! Maximum number of HC surfaces (was 56)
! Following are initially set in AllocateModuleArrays
INTEGER,PUBLIC   :: MAXHCArrayBounds = 0      ! Bounds based on Max Number of Vertices in surfaces
INTEGER   :: MAXHCArrayIncrement = 0   ! Increment based on Max Number of Vertices in surfaces
          ! The following variable should be re-engineered to lower in module hierarchy but need more analysis
INTEGER   :: NVS    ! Number of vertices of the shadow/clipped surface
INTEGER   :: NumVertInShadowOrClippedSurface
INTEGER   :: CurrentSurfaceBeingShadowed
INTEGER   :: CurrentShadowingSurface
INTEGER   :: OverlapStatus ! Results of overlap calculation:
             ! 1=No overlap; 2=NS1 completely within NS2
             ! 3=NS2 completely within NS1; 4=Partial overlap

REAL(r64), ALLOCATABLE, DIMENSION(:)       :: CTHETA ! Cosine of angle of incidence of sun's rays on surface NS
INTEGER   :: FBKSHC ! HC location of first back surface
INTEGER   :: FGSSHC ! HC location of first general shadowing surface
INTEGER   :: FINSHC ! HC location of first back surface overlap
INTEGER   :: FRVLHC ! HC location of first reveal surface
INTEGER   :: FSBSHC ! HC location of first subsurface
INTEGER   :: LOCHCA=0 ! Location of highest data in the HC arrays
INTEGER   :: NBKSHC ! Number of back surfaces in the HC arrays
INTEGER   :: NGSSHC ! Number of general shadowing surfaces in the HC arrays
INTEGER   :: NINSHC ! Number of back surface overlaps in the HC arrays
INTEGER   :: NRVLHC ! Number of reveal surfaces in HC array
INTEGER   :: NSBSHC ! Number of subsurfaces in the HC arrays
LOGICAL   :: CalcSkyDifShading  ! True when sky diffuse solar shading is
INTEGER   :: ShadowingCalcFrequency = 0 ! Frequency for Shadowing Calculations
INTEGER   :: ShadowingDaysLeft = 0 ! Days left in current shadowing period
LOGICAL   :: debugging=.false.
INTEGER   :: OutputFileShading
INTEGER, ALLOCATABLE, DIMENSION(:)        :: HCNS   ! Surface number of back surface HC figures
INTEGER, ALLOCATABLE, DIMENSION(:)        :: HCNV   ! Number of vertices of each HC figure
INTEGER(i64), ALLOCATABLE, DIMENSION(:,:) :: HCA    ! 'A' homogeneous coordinates of sides
INTEGER(i64), ALLOCATABLE, DIMENSION(:,:) :: HCB    ! 'B' homogeneous coordinates of sides
INTEGER(i64), ALLOCATABLE, DIMENSION(:,:) :: HCC    ! 'C' homogeneous coordinates of sides
INTEGER(i64), ALLOCATABLE, DIMENSION(:,:) :: HCX    ! 'X' homogeneous coordinates of vertices of figure.
INTEGER(i64), ALLOCATABLE, DIMENSION(:,:) :: HCY    ! 'Y' homogeneous coordinates of vertices of figure.
INTEGER, ALLOCATABLE, DIMENSION(:,:,:)    :: WindowRevealStatus
REAL(r64), ALLOCATABLE, DIMENSION(:) :: HCAREA ! Area of each HC figure.  Sign Convention:  Base Surface
                                               ! - Positive, Shadow - Negative, Overlap between two shadows
                                               ! - positive, etc., so that sum of HC areas=base sunlit area
REAL(r64), ALLOCATABLE, DIMENSION(:)       :: HCT    ! Transmittance of each HC figure
REAL(r64), ALLOCATABLE, DIMENSION(:)       :: ISABSF ! For simple interior solar distribution (in which all beam
                                                     ! radiation entering zone is assumed to strike the floor),
                                                     ! fraction of beam radiation absorbed by each floor surface
REAL(r64), ALLOCATABLE, DIMENSION(:)       :: SAREA  ! Sunlit area of heat transfer surface HTS
                                                     ! Excludes multiplier for windows
! Shadowing combinations data structure...See ShadowingCombinations type
TYPE (SurfaceErrorTracking), ALLOCATABLE, DIMENSION(:)         :: TrackTooManyFigures
INTEGER :: NumTooManyFigures = 0
TYPE (SurfaceErrorTracking), ALLOCATABLE, DIMENSION(:)         :: TrackTooManyVertices
INTEGER :: NumTooManyVertices = 0
TYPE (SurfaceErrorTracking), ALLOCATABLE, DIMENSION(:)         :: TrackBaseSubSurround
INTEGER :: NumBaseSubSurround = 0
TYPE (SurfaceErrorTracking), ALLOCATABLE, DIMENSION(:)         :: TempSurfErrorTracking
REAL(r64), DIMENSION(3) :: SUNCOS ! Direction cosines of solar position
REAL(r64)               :: XShadowProjection  ! X projection of a shadow (formerly called C)
REAL(r64)               :: YShadowProjection  ! Y projection of a shadow (formerly called S)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: XTEMP  ! Temporary 'X' values for HC vertices of the overlap
REAL(r64), ALLOCATABLE, DIMENSION(:) :: XVC    ! X-vertices of the clipped figure
REAL(r64), ALLOCATABLE, DIMENSION(:) :: XVS    ! X-vertices of the shadow
REAL(r64), ALLOCATABLE, DIMENSION(:) :: YTEMP  ! Temporary 'Y' values for HC vertices of the overlap
REAL(r64), ALLOCATABLE, DIMENSION(:) :: YVC    ! Y-vertices of the clipped figure
REAL(r64), ALLOCATABLE, DIMENSION(:) :: YVS    ! Y-vertices of the shadow
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZVC    ! Z-vertices of the clipped figure
! Used in Sutherland Hodman poly clipping
REAL(r64), ALLOCATABLE, DIMENSION(:) :: ATEMP    ! Temporary 'A' values for HC vertices of the overlap
REAL(r64), ALLOCATABLE, DIMENSION(:) :: BTEMP    ! Temporary 'B' values for HC vertices of the overlap
REAL(r64), ALLOCATABLE, DIMENSION(:) :: CTEMP    ! Temporary 'C' values for HC vertices of the overlap
REAL(r64), ALLOCATABLE, DIMENSION(:) :: XTEMP1   ! Temporary 'X' values for HC vertices of the overlap
REAL(r64), ALLOCATABLE, DIMENSION(:) :: YTEMP1   ! Temporary 'Y' values for HC vertices of the overlap
INTEGER,PUBLIC :: maxNumberOfFigures=0

          ! SUBROUTINE SPECIFICATIONS FOR MODULE SolarShading

PUBLIC  InitSolarCalculations
PRIVATE AllocateModuleArrays
PUBLIC  AnisoSkyViewFactors
PRIVATE CHKBKS
PRIVATE CHKGSS
PRIVATE CHKSBS
PRIVATE ComputeIntSolarAbsorpFactors
PRIVATE CLIP
PRIVATE CTRANS
PRIVATE HTRANS
PRIVATE HTRANS0
PRIVATE HTRANS1
PRIVATE INCLOS
PRIVATE INTCPT
PRIVATE CLIPPOLY
PRIVATE MULTOL
PRIVATE ORDER
PRIVATE DeterminePolygonOverlap
PRIVATE CalcPerSolarBeam
PRIVATE DetermineShadowingCombinations
PRIVATE GetShadowingInput
PRIVATE SHADOW
PRIVATE SHDBKS
PRIVATE SHDGSS
PRIVATE SHDRVL
PRIVATE SHDSBS
PUBLIC  PerformSolarCalculations
PRIVATE SUN3
PRIVATE SUN4
PRIVATE CalcWinTransDifSolInitialDistribution
PRIVATE CalcComplexWindowOverlap
PUBLIC  TimestepInitComplexFenestration
PUBLIC  CalcInteriorSolarDistribution
PUBLIC  CalcBeamSolarOnWinRevealSurface
PUBLIC  CalcWindowProfileAngles
PUBLIC  WindowShadingManager
PUBLIC  WindowGapAirflowControl
PUBLIC  ReportSurfaceShading
PUBLIC  ReportSurfaceErrors
PRIVATE FigureSunCosines
PUBLIC  SurfaceScheduledSolarInc
PUBLIC  WindowScheduledSolarAbs

CONTAINS

          ! MODULE SUBROUTINES:

SUBROUTINE InitSolarCalculations

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   September 1977
          !       MODIFIED       na
          !       RE-ENGINEERED  Mar97, RKS, Initial EnergyPlus Version

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine controls the computation of the solar flux multipliers.

          ! METHODOLOGY EMPLOYED:
          ! All shadowing calculations have been grouped under this routine to
          ! allow segmentation separating it from the hourly loads calculation.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  LOGICAL,SAVE :: GetInputFlag=.true.
  INTEGER, EXTERNAL :: GetNewUnitNumber
  LOGICAL, SAVE :: firsttime=.true.
  INTEGER :: write_stat

          ! FLOW:
#ifdef EP_Count_Calls
  NumInitSolar_Calls=NumInitSolar_Calls+1
#endif
  IF (BeginSimFlag) THEN

    OutputFileShading=GetNewUnitNumber()
    OPEN(OutputFileShading,FILE='eplusout.shd', Action='write',iostat=write_stat)
    IF (write_stat /= 0) THEN
     CALL ShowFatalError('InitSolarCalculations: Could not open file "eplustbl.shd" for output (write).')
    ENDIF

    IF (GetInputFlag) THEN
      CALL GetShadowingInput
      GetInputFlag=.false.
      MAXHCV=MAX(12,MaxVerticesPerSurface+1)
    ENDIF

    if (firsttime) CALL DisplayString('Allocate Solar Module Arrays')
    CALL AllocateModuleArrays

    IF (SolarDistribution /=  FullInteriorExterior) THEN
      if (firsttime) CALL DisplayString('Computing Interior Solar Absorption Factors')
      CALL ComputeIntSolarAbsorpFactors
    ENDIF

    if (firsttime) CALL DisplayString('Determining Shadowing Combinations')
    CALL DetermineShadowingCombinations

    if (firsttime) CALL DisplayString('Computing Window Shade Absorption Factors')
    CALL ComputeWinShadeAbsorpFactors

    IF(CalcSolRefl) THEN
      CALL DisplayString('Initializing Solar Reflection Factors')
      CALL InitSolReflRecSurf
    ENDIF

    if (firsttime) CALL DisplayString('Proceeding with Initializing Solar Calculations')

  END IF

  IF (BeginEnvrnFlag) THEN
    CTHETA=0.0d0
    SAREA=0.0d0
    SurfSunLitArea=0.0d0
    SurfSunLitFrac=0.0d0
    SunlitFracHR=0.0d0
    SunlitFrac=0.0d0
    SunlitFracWithoutReveal=0.0d0
    BackSurfaces = 0
    OverlapAreas = 0.0d0
    CosIncAngHR=0.0d0
    CosIncAng=0.0d0
    AnisoSkyMult = 1.0d0   !For isotropic sky; recalculated in AnisoSkyViewFactors if anisotropic radiance
!    WithShdgIsoSky=0.0
!    WoShdgIsoSky=0.0
!    WithShdgHoriz=0.0
!    WoShdgHoriz=0.0
!    DifShdgRatioIsoSky=0.0
!    DifShdgRatioHoriz=0.0
    MultIsoSky=0.0d0
    MultCircumSolar=0.0d0
    MultHorizonZenith=0.0d0
    WinTransSolar=0.0d0
    WinBmSolar=0.0d0
    WinBmBmSolar=0.0d0
    WinBmDifSolar=0.0d0

    WinDifSolar=0.0d0
    WinDirSolTransAtIncAngle=0.0d0
    WinHeatGain=0.0d0
    WinHeatGainRep=0.0d0
    WinHeatLossRep=0.0d0
    WinGainConvGlazToZoneRep        = 0.0D0
    WinGainIRGlazToZoneRep          = 0.0D0
    WinLossSWZoneToOutWinRep        = 0.0D0
    WinGainFrameDividerToZoneRep    = 0.0D0
    WinGainConvGlazShadGapToZoneRep = 0.0D0
    WinGainConvShadeToZoneRep       = 0.0D0
    OtherConvGainInsideFaceToZoneRep= 0.0D0
    WinGainIRShadeToZoneRep         = 0.0D0
    WinGapConvHtFlowRep=0.0d0
    WinShadingAbsorbedSolar=0.0d0
    WinSysSolTransmittance=0.0d0
    WinSysSolReflectance=0.0d0
    WinSysSolAbsorptance=0.0d0
    InsideGlassCondensationFlag=0
    InsideFrameCondensationFlag=0
    InsideDividerCondensationFlag=0
    ZoneTransSolar=0.0d0
    ZoneBmSolFrExtWinsRep = 0.0d0
    ZoneBmSolFrIntWinsRep = 0.0d0
    InitialZoneDifSolReflW = 0.0d0
    ZoneDifSolFrExtWinsRep = 0.0d0
    ZoneDifSolFrIntWinsRep = 0.0d0
    ZoneWinHeatGain=0.0d0
    ZoneWinHeatGainRep=0.0d0
    ZoneWinHeatLossRep=0.0d0
    ZoneOpaqSurfInsFaceCond=0.0d0
    ZoneOpaqSurfInsFaceCondGainRep=0.0d0
    ZoneOpaqSurfInsFaceCondLossRep=0.0d0
    QRadSWOutIncident=0.0d0
    QRadSWOutIncidentBeam=0.0d0
    BmIncInsSurfIntensRep=0.0d0
    BmIncInsSurfAmountRep=0.0d0
!    DifIncInsSurfIntensRep=0.0
!    DifIncInsSurfAmountRep=0.0
    IntBmIncInsSurfIntensRep=0.0d0
    IntBmIncInsSurfAmountRep=0.0d0
!    IntDifIncInsSurfIntensRep=0.0
!    IntDifIncInsSurfAmountRep=0.0
    QRadSWOutIncidentSkyDiffuse=0.0d0
    QRadSWOutIncidentGndDiffuse=0.0d0
    QRadSWOutIncBmToDiffReflGnd=0.0d0
    QRadSWOutIncSkyDiffReflGnd=0.0d0
    QRadSWOutIncBmToBmReflObs=0.0d0
    QRadSWOutIncBmToDiffReflObs=0.0d0
    QRadSWOutIncSkyDiffReflObs=0.0d0
    CosIncidenceAngle=0.0d0
    QRadSWwinAbsTot=0.0d0
    SWwinAbsTotalReport = 0.0d0
    InitialDifSolInAbsReport = 0.0d0
    InitialDifSolInTransReport = 0.0d0
    SWInAbsTotalReport = 0.0d0
    WindowRevealStatus=0
    !energy
    WinTransSolarEnergy=0.0d0
    WinBmSolarEnergy=0.0d0
    WinBmBmSolarEnergy=0.0d0
    WinBmDifSolarEnergy=0.0d0

    WinDifSolarEnergy=0.0d0
    WinHeatGainRepEnergy=0.0d0
    WinHeatLossRepEnergy=0.0d0
    WinGapConvHtFlowRepEnergy=0.0d0
    WinShadingAbsorbedSolarEnergy=0.0d0
    ZoneTransSolarEnergy = 0.0d0
    ZoneBmSolFrExtWinsRepEnergy = 0.0d0
    ZoneBmSolFrIntWinsRepEnergy = 0.0d0
    ZoneDifSolFrExtWinsRepEnergy = 0.0d0
    ZoneDifSolFrIntWinsRepEnergy = 0.0d0
    ZoneWinHeatGainRepEnergy=0.0d0
    ZoneWinHeatLossRepEnergy=0.0d0
    ZnOpqSurfInsFaceCondGnRepEnrg=0.0d0
    ZnOpqSurfInsFaceCondLsRepEnrg=0.0d0
    BmIncInsSurfAmountRepEnergy=0.0d0
!    DifIncInsSurfAmountRepEnergy=0.0
    IntBmIncInsSurfAmountRepEnergy=0.0d0
!    IntDifIncInsSurfAmountRepEnergy=0.0
    QRadSWwinAbsTotEnergy=0.0d0

  ENDIF

  firsttime=.false.

  RETURN    ! Normal subroutine end

END SUBROUTINE InitSolarCalculations

SUBROUTINE GetShadowingInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   July 1999
          !       MODIFIED       B. Griffith, Nov 2012, add calculaton method
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the Shadowing Calculation object.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound,GetObjectItem,SameString
  USE General, ONLY: RoundSigDigits
  USE DataIPShortCuts
  USE DataSystemVariables, ONLY: SutherlandHodgman,DetailedSkyDiffuseAlgorithm, &
                                 DetailedSolarTimestepIntegration


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: fmta="(A)"

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER NumItems
  INTEGER NumNumbers
  INTEGER NumAlphas
  INTEGER IOStat

  rNumericArgs(1:4)=0.0d0  ! so if nothing gotten, defaults will be maintained.
  cAlphaArgs(1)=' '
  cAlphaArgs(2)=' '
  cCurrentModuleObject='ShadowCalculation'
  NumItems=GetNumObjectsFound(cCurrentModuleObject)
  NumAlphas=0
  NumNumbers=0
  IF (NumItems > 1) THEN
    CALL ShowWarningError(TRIM(cCurrentModuleObject)//': More than 1 occurence of this object found, only first will be used.')
  ENDIF

  IF (NumItems /= 0) THEN
    CALL GetObjectItem(cCurrentModuleObject,1,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    ShadowingCalcFrequency=rNumericArgs(1)
  ENDIF

  IF (ShadowingCalcFrequency <= 0) THEN
    !  Set to default value
    ShadowingCalcFrequency=20
  ENDIF
  IF (ShadowingCalcFrequency > 31) THEN
    CALL ShowWarningError(TRIM(cCurrentModuleObject)//': suspect '//trim(cNumericFieldNames(1)))
    CALL ShowContinueError('Value entered=['//trim(RoundSigDigits(rNumericArgs(1),0))//  &
       '], Shadowing Calculations will be inaccurate.')
  ENDIF

  IF (rNumericArgs(2) > 199.d0)  THEN
    MAXHCS=rNumericArgs(2)
  ELSE
    MAXHCS=15000
  ENDIF

  IF (NumAlphas >= 1) THEN
    IF (SameString(cAlphaArgs(1), 'AverageOverDaysInFrequency')) THEN
      DetailedSolarTimestepIntegration = .FALSE.
      cAlphaArgs(1) = 'AverageOverDaysInFrequency'
    ELSEIF (SameString(cAlphaArgs(1), 'TimestepFrequency')) THEN
      DetailedSolarTimestepIntegration = .TRUE.
      cAlphaArgs(1) = 'TimestepFrequency'
    ELSE
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//': invalid '//trim(cAlphaFieldNames(1)))
      CALL ShowContinueError('Value entered="'//trim(cAlphaArgs(1))//'", AverageOverDaysInFrequency will be used.')
      DetailedSolarTimestepIntegration = .FALSE.
      cAlphaArgs(1) = 'AverageOverDaysInFrequency'
    ENDIF
  ELSE
    DetailedSolarTimestepIntegration = .FALSE.
    cAlphaArgs(1) = 'AverageOverDaysInFrequency'
  ENDIF

  IF (NumAlphas >= 2) THEN
    IF (SameString(cAlphaArgs(2),'SutherlandHodgman')) THEN
      SutherlandHodgman=.true.
      cAlphaArgs(2)='SutherlandHodgman'
    ELSEIF (SameString(cAlphaArgs(2),'ConvexWeilerAtherton')) THEN
      SutherlandHodgman=.false.
      cAlphaArgs(2)='ConvexWeilerAtherton'
    ELSEIF (lAlphaFieldBlanks(2)) THEN
      IF (.not. SutherlandHodgman) THEN  ! if already set.
        cAlphaArgs(2)='ConvexWeilerAtherton'
      ELSE
        cAlphaArgs(2)='SutherlandHodgman'
      ENDIF
    ELSE
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//': invalid '//trim(cAlphaFieldNames(2)))
      IF (.not. SutherlandHodgman) THEN
        CALL ShowContinueError('Value entered="'//trim(cAlphaArgs(2))//'", ConvexWeilerAtherton will be used.')
      ELSE
        CALL ShowContinueError('Value entered="'//trim(cAlphaArgs(2))//'", SutherlandHodgman will be used.')
      ENDIF
    ENDIF
  ELSE
    IF (.not. SutherlandHodgman) THEN
      cAlphaArgs(2)='ConvexWeilerAtherton'
    ELSE
      cAlphaArgs(2)='SutherlandHodgman'
    ENDIF
  ENDIF

  IF (NumAlphas >= 3) THEN
    IF (SameString(cAlphaArgs(3),'SimpleSkyDiffuseModeling')) THEN
      DetailedSkyDiffuseAlgorithm=.false.
      cAlphaArgs(3)='SimpleSkyDiffuseModeling'
    ELSEIF (SameString(cAlphaArgs(3),'DetailedSkyDiffuseModeling')) THEN
      DetailedSkyDiffuseAlgorithm=.true.
      cAlphaArgs(3)='DetailedSkyDiffuseModeling'
    ELSEIF (lAlphaFieldBlanks(3)) THEN
      DetailedSkyDiffuseAlgorithm=.false.
      cAlphaArgs(3)='SimpleSkyDiffuseModeling'
    ELSE
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//': invalid '//trim(cAlphaFieldNames(3)))
      CALL ShowContinueError('Value entered="'//trim(cAlphaArgs(3))//'", SimpleSkyDiffuseModeling will be used.')
    ENDIF
  ELSE
    cAlphaArgs(3)='SimpleSkyDiffuseModeling'
    DetailedSkyDiffuseAlgorithm=.false.
  ENDIF

  IF (.not. DetailedSkyDiffuseAlgorithm .and. ShadingTransmittanceVaries .and.  &
      SolarDistribution /= MinimalShadowing) THEN
    CALL ShowWarningError('GetShadowingInput: The shading transmittance for shading devices changes throughout the year.'//  &
       ' Choose DetailedSkyDiffuseModeling in the '//trim(cCurrentModuleObject)//' object to remove this warning.')
    CALL ShowContinueError('Simulation has been reset to use DetailedSkyDiffuseModeling. Simulation continues.')
    DetailedSkyDiffuseAlgorithm=.true.
    cAlphaArgs(2)='DetailedSkyDiffuseModeling'
    IF (ShadowingCalcFrequency > 1) THEN
      CALL ShowContinueError('Better accuracy may be gained by setting the '//trim(cNumericFieldNames(1))//   &
         ' to 1 in the '//trim(cCurrentModuleObject)//' object.')
    ENDIF
  ELSEIF (DetailedSkyDiffuseAlgorithm) THEN
    IF (.not. ShadingTransmittanceVaries .or. SolarDistribution == MinimalShadowing) THEN
      CALL ShowWarningError('GetShadowingInput: DetailedSkyDiffuseModeling is chosen but not needed as'//  &
          ' either the shading transmittance for shading devices does not change throughout the year')
      CALL ShowContinueError(' or MinimalShadowing has been chosen.')
      CALL ShowContinueError('Simulation should be set to use SimpleSkyDiffuseModeling, but is left at Detailed for simulation.')
      CALL ShowContinueError('Choose SimpleSkyDiffuseModeling in the '//trim(cCurrentModuleObject)//  &
         ' object to reduce computation time.')
    ENDIF
  ENDIF


  Write(OutputFileInits,fmta) '! <Shadowing/Sun Position Calculations> [Annual Simulations], Calculation Method,'//  &
     'Value {days}, Allowable Number Figures in Shadow Overlap {}, Polygon Clipping Algorithm, '//  &
     'Sky Diffuse Modeling Algorithm'
  Write(OutputFileInits,fmta) 'Shadowing/Sun Position Calculations,'//  &
                                TRIM(cAlphaArgs(1))//','// &
                                TRIM(RoundSigDigits(ShadowingCalcFrequency))//','//  &
                                TRIM(RoundSigDigits(MAXHCS))//','//  &
                                trim(cAlphaArgs(2))//','//trim(cAlphaArgs(3))


  RETURN

END SUBROUTINE GetShadowingInput

SUBROUTINE AllocateModuleArrays

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   February 1998
          !       MODIFIED       August 2005 JG - Added output variables for energy in J
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine allocates all of the arrays at the module level which
          ! require allocation.

          ! METHODOLOGY EMPLOYED:
          ! Allocation is dependent on the user input file.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY : RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER SurfLoop, ZoneLoop
  INTEGER I, MaxNumOfLayers, NumOfLayers

          ! FLOW:

  ALLOCATE (CTHETA(TotSurfaces))
  CTHETA=0.0d0
  ALLOCATE (SAREA(TotSurfaces))
  SAREA=0.0d0
  ALLOCATE(SurfSunLitArea(TotSurfaces))
  SurfSunLitArea=0.0d0
  ALLOCATE(SurfSunLitFrac(TotSurfaces))
  SurfSunLitFrac=0.0d0
  ALLOCATE (SunlitFracHR(TotSurfaces,24))
  SunlitFracHR=0.0d0
  ALLOCATE (SunlitFrac(TotSurfaces,24,NumOfTimeStepInHour))
  SunlitFrac=0.0d0
  ALLOCATE (SunlitFracWithoutReveal(TotSurfaces,24,NumOfTimeStepInHour))
  SunlitFracWithoutReveal=0.0d0
  ALLOCATE (BackSurfaces(TotSurfaces,MaxBkSurf,24,NumOfTimeStepInHour))
  BackSurfaces = 0
  ALLOCATE (OverlapAreas(TotSurfaces,MaxBkSurf,24,NumOfTimeStepInHour))
  OverlapAreas = 0.0d0
  ALLOCATE (CosIncAngHR(TotSurfaces,24))
  CosIncAngHR=0.0d0
  ALLOCATE (CosIncAng(TotSurfaces,24,NumOfTimeStepInHour))
  CosIncAng=0.0d0
  ALLOCATE (AnisoSkyMult(TotSurfaces))
  AnisoSkyMult = 1.0d0  ! For isotropic sky; recalculated in AnisoSkyViewFactors if anisotropic radiance
!  ALLOCATE (WithShdgIsoSky(TotSurfaces))
!  WithShdgIsoSky=0.0
!  ALLOCATE (WoShdgIsoSky(TotSurfaces))
!  WoShdgIsoSky=0.0
!  ALLOCATE (WithShdgHoriz(TotSurfaces))
!  WithShdgHoriz=0.0
!  ALLOCATE (WoShdgHoriz(TotSurfaces))
!  WoShdgHoriz=0.0
!  ALLOCATE (DifShdgRatioIsoSky(TotSurfaces))
!  DifShdgRatioIsoSky=0.0
!  ALLOCATE (DifShdgRatioHoriz(TotSurfaces))
!  DifShdgRatioHoriz=0.0
  ALLOCATE (MultIsoSky(TotSurfaces))
  MultIsoSky=0.0d0
  ALLOCATE (MultCircumSolar(TotSurfaces))
  MultCircumSolar=0.0d0
  ALLOCATE (MultHorizonZenith(TotSurfaces))
  MultHorizonZenith=0.0d0
  ALLOCATE (WinTransSolar(TotSurfaces))
  WinTransSolar=0.0d0
  ALLOCATE (WinBmSolar(TotSurfaces))
  WinBmSolar=0.0d0
  ALLOCATE (WinBmBmSolar(TotSurfaces))
  WinBmBmSolar=0.0d0
  ALLOCATE (WinBmDifSolar(TotSurfaces))
  WinBmDifSolar=0.0d0

  ALLOCATE (WinDifSolar(TotSurfaces))
  WinDifSolar=0.0d0
  ALLOCATE (WinDirSolTransAtIncAngle(TotSurfaces))
  WinDirSolTransAtIncAngle=0.0d0
  ALLOCATE (WinHeatGain(TotSurfaces))
  WinHeatGain=0.0d0
  ALLOCATE (WinHeatGainRep(TotSurfaces))
  WinHeatGainRep=0.0d0
  ALLOCATE (WinHeatLossRep(TotSurfaces))
  WinHeatLossRep=0.0d0
  ALLOCATE (WinGainConvGlazToZoneRep(TotSurfaces))
  WinGainConvGlazToZoneRep        = 0.0d0
  ALLOCATE (WinGainIRGlazToZoneRep(TotSurfaces))
  WinGainIRGlazToZoneRep          = 0.0d0
  ALLOCATE (WinLossSWZoneToOutWinRep(TotSurfaces))
  WinLossSWZoneToOutWinRep        = 0.0d0
  ALLOCATE (WinGainFrameDividerToZoneRep(TotSurfaces))
  WinGainFrameDividerToZoneRep    = 0.0d0
  ALLOCATE (WinGainConvGlazShadGapToZoneRep(TotSurfaces))
  WinGainConvGlazShadGapToZoneRep = 0.0d0
  ALLOCATE (WinGainConvShadeToZoneRep(TotSurfaces))
  WinGainConvShadeToZoneRep       = 0.0d0
  ALLOCATE (OtherConvGainInsideFaceToZoneRep(TotSurfaces))
  OtherConvGainInsideFaceToZoneRep=0.0d0
  ALLOCATE (WinGainIRShadeToZoneRep(TotSurfaces))
  WinGainIRShadeToZoneRep         = 0.0d0
  ALLOCATE (WinGapConvHtFlowRep(TotSurfaces))
  WinGapConvHtFlowRep=0.0d0
  ALLOCATE (WinShadingAbsorbedSolar(TotSurfaces))
  WinShadingAbsorbedSolar=0.0d0
  ALLOCATE (WinSysSolTransmittance(TotSurfaces))
  WinSysSolTransmittance=0.0d0
  ALLOCATE (WinSysSolReflectance(TotSurfaces))
  WinSysSolReflectance=0.0d0
  ALLOCATE (WinSysSolAbsorptance(TotSurfaces))
  WinSysSolAbsorptance=0.0d0
  ALLOCATE (InsideGlassCondensationFlag(TotSurfaces))
  InsideGlassCondensationFlag=0
  ALLOCATE (InsideFrameCondensationFlag(TotSurfaces))
  InsideFrameCondensationFlag=0
  ALLOCATE (InsideDividerCondensationFlag(TotSurfaces))
  InsideDividerCondensationFlag=0
  ALLOCATE (ZoneTransSolar(NumOfZones))
  ZoneTransSolar=0.0d0
  ALLOCATE (ZoneBmSolFrExtWinsRep(NumOfZones))
  ZoneBmSolFrExtWinsRep = 0.0d0
  ALLOCATE (ZoneBmSolFrIntWinsRep(NumOfZones))
  ZoneBmSolFrIntWinsRep = 0.0d0
  ALLOCATE (InitialZoneDifSolReflW(NumOfZones))
  InitialZoneDifSolReflW = 0.0d0
  ALLOCATE (ZoneDifSolFrExtWinsRep(NumOfZones))
  ZoneDifSolFrExtWinsRep = 0.0d0
  ALLOCATE (ZoneDifSolFrIntWinsRep(NumOfZones))
  ZoneDifSolFrIntWinsRep = 0.0d0
  ALLOCATE (ZoneWinHeatGain(NumOfZones))
  ZoneWinHeatGain=0.0d0
  ALLOCATE (ZoneWinHeatGainRep(NumOfZones))
  ZoneWinHeatGainRep=0.0d0
  ALLOCATE (ZoneWinHeatLossRep(NumOfZones))
  ZoneWinHeatLossRep=0.0d0
  ALLOCATE (ZoneOpaqSurfInsFaceCond(NumOfZones))
  ZoneOpaqSurfInsFaceCond=0.0d0
  ALLOCATE (ZoneOpaqSurfInsFaceCondGainRep(NumOfZones))
  ZoneOpaqSurfInsFaceCondGainRep=0.0d0
  ALLOCATE (ZoneOpaqSurfInsFaceCondLossRep(NumOfZones))
  ZoneOpaqSurfInsFaceCondLossRep=0.0d0
  ALLOCATE (ZoneOpaqSurfExtFaceCond(NumOfZones))
  ZoneOpaqSurfExtFaceCond=0.0d0
  ALLOCATE (ZoneOpaqSurfExtFaceCondGainRep(NumOfZones))
  ZoneOpaqSurfExtFaceCondGainRep=0.0d0
  ALLOCATE (ZoneOpaqSurfExtFaceCondLossRep(NumOfZones))
  ZoneOpaqSurfExtFaceCondLossRep=0.0d0

  ALLOCATE (QRadSWOutIncident(TotSurfaces))
  QRadSWOutIncident=0.0d0
  ALLOCATE (QRadSWOutIncidentBeam(TotSurfaces))
  QRadSWOutIncidentBeam=0.0d0
  ALLOCATE (BmIncInsSurfIntensRep(TotSurfaces))
  BmIncInsSurfIntensRep=0.0d0
  ALLOCATE (BmIncInsSurfAmountRep(TotSurfaces))
  BmIncInsSurfAmountRep=0.0d0
!  ALLOCATE (DifIncInsSurfIntensRep(TotSurfaces))
!  DifIncInsSurfIntensRep=0.0
!  ALLOCATE (DifIncInsSurfAmountRep(TotSurfaces))
!  DifIncInsSurfAmountRep=0.0
  ALLOCATE (IntBmIncInsSurfIntensRep(TotSurfaces))
  IntBmIncInsSurfIntensRep=0.0d0
  ALLOCATE (IntBmIncInsSurfAmountRep(TotSurfaces))
  IntBmIncInsSurfAmountRep=0.0d0
!  ALLOCATE (IntDifIncInsSurfIntensRep(TotSurfaces))
!  IntDifIncInsSurfIntensRep=0.0
!  ALLOCATE (IntDifIncInsSurfAmountRep(TotSurfaces))
!  IntDifIncInsSurfAmountRep=0.0
  ALLOCATE (QRadSWOutIncidentSkyDiffuse(TotSurfaces))
  QRadSWOutIncidentSkyDiffuse=0.0d0
  ALLOCATE (QRadSWOutIncidentGndDiffuse(TotSurfaces))
  QRadSWOutIncidentGndDiffuse=0.0d0
  ALLOCATE (QRadSWOutIncBmToDiffReflGnd(TotSurfaces))
  QRadSWOutIncBmToDiffReflGnd=0.0d0
  ALLOCATE (QRadSWOutIncSkyDiffReflGnd(TotSurfaces))
  QRadSWOutIncSkyDiffReflGnd=0.0d0
  ALLOCATE (QRadSWOutIncBmToBmReflObs(TotSurfaces))
  QRadSWOutIncBmToBmReflObs=0.0d0
  ALLOCATE (QRadSWOutIncBmToDiffReflObs(TotSurfaces))
  QRadSWOutIncBmToDiffReflObs=0.0d0
  ALLOCATE (QRadSWOutIncSkyDiffReflObs(TotSurfaces))
  QRadSWOutIncSkyDiffReflObs=0.0d0
  ALLOCATE (CosIncidenceAngle(TotSurfaces))
  CosIncidenceAngle=0.0d0
  ALLOCATE (BSDFBeamDirectionRep(TotSurfaces))
  BSDFBeamDirectionRep=0
  ALLOCATE (BSDFBeamThetaRep(TotSurfaces))
  BSDFBeamThetaRep=0.0d0
  ALLOCATE (BSDFBeamPhiRep(TotSurfaces))
  BSDFBeamPhiRep=0.0d0
  ALLOCATE (QRadSWwinAbsTot(TotSurfaces))
  QRadSWwinAbsTot=0.0d0

  ALLOCATE (QRadSWwinAbsLayer(TotSurfaces, MaxSolidWinLayers))
  QRadSWwinAbsLayer = 0.0d0

  ALLOCATE (FenLaySurfTempFront(TotSurfaces, MaxSolidWinLayers))
  FenLaySurfTempFront = 0.0d0
  ALLOCATE (FenLaySurfTempBack(TotSurfaces, MaxSolidWinLayers))
  FenLaySurfTempBack = 0.0d0

  ALLOCATE (SWwinAbsTotalReport(TotSurfaces))
  SWwinAbsTotalReport=0.0d0
  ALLOCATE (InitialDifSolInAbsReport(TotSurfaces))
  InitialDifSolInAbsReport=0.0d0
  ALLOCATE (InitialDifSolInTransReport(TotSurfaces))
  InitialDifSolInTransReport=0.0d0
  ALLOCATE (SWInAbsTotalReport(TotSurfaces))
  SWInAbsTotalReport=0.0d0
  ALLOCATE (WindowRevealStatus(TotSurfaces,24,NumOfTimeStepInHour))
  WindowRevealStatus=0

  ! Weiler-Atherton
  MAXHCArrayBounds=2*(MaxVerticesPerSurface+1)
  MAXHCArrayIncrement=MaxVerticesPerSurface+1
  ALLOCATE(XTEMP((MaxVerticesPerSurface+1)*2))
  XTEMP=0.0d0
  ALLOCATE(YTEMP((MaxVerticesPerSurface+1)*2))
  YTEMP=0.0d0
  ALLOCATE(XVC(MaxVerticesPerSurface+1))
  XVC=0.0d0
  ALLOCATE(XVS(MaxVerticesPerSurface+1))
  XVS=0.0d0
  ALLOCATE(YVC(MaxVerticesPerSurface+1))
  YVC=0.0d0
  ALLOCATE(YVS(MaxVerticesPerSurface+1))
  YVS=0.0d0
  ALLOCATE(ZVC(MaxVerticesPerSurface+1))
  ZVC=0.0d0

  !Sutherland-Hodgman
  ALLOCATE(ATEMP(2*(MaxVerticesPerSurface + 1)))
  ATEMP=0.0d0
  ALLOCATE(BTEMP(2*(MaxVerticesPerSurface + 1)))
  BTEMP=0.0d0
  ALLOCATE(CTEMP(2*(MaxVerticesPerSurface + 1)))
  CTEMP=0.0d0
  ALLOCATE(XTEMP1(2*(MaxVerticesPerSurface + 1)))
  XTEMP1=0.0d0
  ALLOCATE(YTEMP1(2*(MaxVerticesPerSurface + 1)))
  YTEMP1=0.0d0

  !energy
  ALLOCATE (WinTransSolarEnergy(TotSurfaces))
  WinTransSolarEnergy=0.0d0
  ALLOCATE (WinBmSolarEnergy(TotSurfaces))
  WinBmSolarEnergy=0.0d0

  ALLOCATE (WinBmBmSolarEnergy(TotSurfaces))
  WinBmBmSolarEnergy=0.0d0
  ALLOCATE (WinBmDifSolarEnergy(TotSurfaces))
  WinBmDifSolarEnergy=0.0d0

  ALLOCATE (WinDifSolarEnergy(TotSurfaces))
  WinDifSolarEnergy=0.0d0
  ALLOCATE (WinHeatGainRepEnergy(TotSurfaces))
  WinHeatGainRepEnergy=0.0d0
  ALLOCATE (WinHeatLossRepEnergy(TotSurfaces))
  WinHeatLossRepEnergy=0.0d0
  ALLOCATE (WinGapConvHtFlowRepEnergy(TotSurfaces))
  WinGapConvHtFlowRepEnergy=0.0d0
  ALLOCATE (ZoneTransSolarEnergy(NumOfZones))
  ZoneTransSolarEnergy=0.0d0
  ALLOCATE (ZoneBmSolFrExtWinsRepEnergy(NumOfZones))
  ZoneBmSolFrExtWinsRepEnergy = 0.0d0
  ALLOCATE (ZoneBmSolFrIntWinsRepEnergy(NumOfZones))
  ZoneBmSolFrIntWinsRepEnergy = 0.0d0
  ALLOCATE (ZoneDifSolFrExtWinsRepEnergy(NumOfZones))
  ZoneDifSolFrExtWinsRepEnergy = 0.0d0
  ALLOCATE (ZoneDifSolFrIntWinsRepEnergy(NumOfZones))
  ZoneDifSolFrIntWinsRepEnergy = 0.0d0
  ALLOCATE (ZoneWinHeatGainRepEnergy(NumOfZones))
  ZoneWinHeatGainRepEnergy=0.0d0
  ALLOCATE (ZoneWinHeatLossRepEnergy(NumOfZones))
  ZoneWinHeatLossRepEnergy=0.0d0
  ALLOCATE (BmIncInsSurfAmountRepEnergy(TotSurfaces))
  BmIncInsSurfAmountRepEnergy=0.0d0
  ALLOCATE (ZnOpqSurfInsFaceCondGnRepEnrg(NumOfZones))
  ZnOpqSurfInsFaceCondGnRepEnrg=0.0d0
  ALLOCATE (ZnOpqSurfInsFaceCondLsRepEnrg(NumOfZones))
  ZnOpqSurfInsFaceCondLsRepEnrg=0.0d0
  ALLOCATE (ZnOpqSurfExtFaceCondGnRepEnrg(NumOfZones))
  ZnOpqSurfExtFaceCondGnRepEnrg=0.0d0
  ALLOCATE (ZnOpqSurfExtFaceCondLsRepEnrg(NumOfZones))
  ZnOpqSurfExtFaceCondLsRepEnrg=0.0d0
!  ALLOCATE (DifIncInsSurfAmountRepEnergy(TotSurfaces))
!  DifIncInsSurfAmountRepEnergy=0.0
  ALLOCATE (IntBmIncInsSurfAmountRepEnergy(TotSurfaces))
  IntBmIncInsSurfAmountRepEnergy=0.0d0
!  ALLOCATE (IntDifIncInsSurfAmountRepEnergy(TotSurfaces))
!  IntDifIncInsSurfAmountRepEnergy=0.0
  ALLOCATE (QRadSWwinAbsTotEnergy(TotSurfaces))
  QRadSWwinAbsTotEnergy=0.0d0
  ALLOCATE (WinShadingAbsorbedSolarEnergy(TotSurfaces))
  WinShadingAbsorbedSolarEnergy=0.0d0
  SurfaceWindow%BmSolAbsdOutsReveal = 0.0d0
  SurfaceWindow%BmSolRefldOutsRevealReport = 0.0d0
  SurfaceWindow%BmSolAbsdInsReveal = 0.0d0
  SurfaceWindow%BmSolRefldInsReveal = 0.0d0
  SurfaceWindow%BmSolRefldInsRevealReport = 0.0d0
  SurfaceWindow%OutsRevealDiffOntoGlazing = 0.0d0
  SurfaceWindow%InsRevealDiffOntoGlazing = 0.0d0
  SurfaceWindow%InsRevealDiffIntoZone = 0.0d0
  SurfaceWindow%OutsRevealDiffOntoFrame = 0.0d0
  SurfaceWindow%InsRevealDiffOntoFrame = 0.0d0

  ! Added report variables for inside reveal to debug CR 7596. TH 5/26/2009
  SurfaceWindow%InsRevealDiffOntoGlazingReport = 0.0d0
  SurfaceWindow%InsRevealDiffIntoZoneReport = 0.0d0
  SurfaceWindow%InsRevealDiffOntoFrameReport = 0.0d0
  SurfaceWindow%BmSolAbsdInsRevealReport = 0.0d0


  CALL DisplayString('Initializing Zone Report Variables')
  ! CurrentModuleObject='Zone'
  DO ZoneLoop=1,NumOfZones
    CALL SetupOutputVariable('Zone Windows Total Transmitted Solar Radiation Rate [W]', &
                              ZoneTransSolar(ZoneLoop),'Zone','Average',Zone(ZoneLoop)%Name)
    CALL SetupOutputVariable('Zone Exterior Windows Total Transmitted Beam Solar Radiation Rate [W]', &
                              ZoneBmSolFrExtWinsRep(ZoneLoop),  &
                             'Zone','Average',Zone(ZoneLoop)%Name)
    CALL SetupOutputVariable('Zone Interior Windows Total Transmitted Beam Solar Radiation Rate [W]', &
                              ZoneBmSolFrIntWinsRep(ZoneLoop),  &
                             'Zone','Average',Zone(ZoneLoop)%Name)
    CALL SetupOutputVariable('Zone Exterior Windows Total Transmitted Diffuse Solar Radiation Rate [W]', &
                              ZoneDifSolFrExtWinsRep(ZoneLoop),  &
                             'Zone','Average',Zone(ZoneLoop)%Name)
    CALL SetupOutputVariable('Zone Interior Windows Total Transmitted Diffuse Solar Radiation Rate [W]', &
                              ZoneDifSolFrIntWinsRep(ZoneLoop),  &
                             'Zone','Average',Zone(ZoneLoop)%Name)
    CALL SetupOutputVariable('Zone Windows Total Heat Gain Rate [W]', &
                              ZoneWinHeatGainRep(ZoneLoop),'Zone','Average',Zone(ZoneLoop)%Name)
    CALL SetupOutputVariable('Zone Windows Total Heat Loss Rate [W]',ZoneWinHeatLossRep(ZoneLoop), &
                             'Zone','Average',Zone(ZoneLoop)%Name)
    ! Energy variables
    CALL SetupOutputVariable('Zone Windows Total Transmitted Solar Radiation Energy [J]',ZoneTransSolarEnergy(ZoneLoop), &
                             'Zone','Sum',Zone(ZoneLoop)%Name)
    CALL SetupOutputVariable('Zone Exterior Windows Total Transmitted Beam Solar Radiation Energy [J]', &
                              ZoneBmSolFrExtWinsRepEnergy(ZoneLoop),  &
                             'Zone','Sum',Zone(ZoneLoop)%Name)
    CALL SetupOutputVariable('Zone Interior Windows Total Transmitted Beam Solar Radiation Energy [J]', &
                              ZoneBmSolFrIntWinsRepEnergy(ZoneLoop),  &
                             'Zone','Sum',Zone(ZoneLoop)%Name)
    CALL SetupOutputVariable('Zone Exterior Windows Total Transmitted Diffuse Solar Radiation Energy [J]', &
                              ZoneDifSolFrExtWinsRepEnergy(ZoneLoop),  &
                             'Zone','Sum',Zone(ZoneLoop)%Name)
    CALL SetupOutputVariable('Zone Interior Windows Total Transmitted Diffuse Solar Radiation Energy [J]', &
                              ZoneDifSolFrIntWinsRepEnergy(ZoneLoop),  &
                             'Zone','Sum',Zone(ZoneLoop)%Name)
    CALL SetupOutputVariable('Zone Windows Total Heat Gain Energy [J]', &
                              ZoneWinHeatGainRepEnergy(ZoneLoop), &
                             'Zone','Sum',Zone(ZoneLoop)%Name)
    CALL SetupOutputVariable('Zone Windows Total Heat Loss Energy [J]',ZoneWinHeatLossRepEnergy(ZoneLoop), &
           'Zone','Sum',Zone(ZoneLoop)%Name)

    IF (DisplayAdvancedReportVariables) THEN
      ! CurrentModuleObject='Zone(Advanced)'
      CALL SetupOutputVariable('Zone Opaque Surface Inside Faces Total Conduction Heat Gain Rate [W]', &
                                ZoneOpaqSurfInsFaceCondGainRep(ZoneLoop), &
                               'Zone','Average',Zone(ZoneLoop)%Name)
      CALL SetupOutputVariable('Zone Opaque Surface Inside Faces Total Conduction Heat Loss Rate [W]', &
                                ZoneOpaqSurfInsFaceCondLossRep(ZoneLoop), &
                               'Zone','Average',Zone(ZoneLoop)%Name)
      ! Energy variables
      CALL SetupOutputVariable('Zone Opaque Surface Inside Faces Total Conduction Heat Gain Energy [J]', &
                                ZnOpqSurfInsFaceCondGnRepEnrg(ZoneLoop), &
                               'Zone','Sum',Zone(ZoneLoop)%Name)
      CALL SetupOutputVariable('Zone Opaque Surface Inside Faces Total Conduction Heat Loss Energy [J]', &
                                ZnOpqSurfInsFaceCondLsRepEnrg(ZoneLoop), &
                               'Zone','Sum',Zone(ZoneLoop)%Name)
    ENDIF
  END DO

  CALL DisplayString('Initializing Surface (Shading) Report Variables')
  ! CurrentModuleObject='Surfaces'
  DO SurfLoop=1,TotSurfaces
    IF (Surface(SurfLoop)%ExtSolar) THEN
      CALL SetupOutputVariable('Surface Outside Face Sunlit Area [m2]', &
                                SurfSunlitArea(SurfLoop),'Zone','State',Surface(SurfLoop)%Name)
      CALL SetupOutputVariable('Surface Outside Face Sunlit Fraction []', &
                                SurfSunlitFrac(SurfLoop),'Zone','State',Surface(SurfLoop)%Name)
      CALL SetupOutputVariable('Surface Outside Face Incident Solar Radiation Rate per Area [W/m2]', &
                                QRadSWOutIncident(SurfLoop), &
                               'Zone','Average', &
                                Surface(SurfLoop)%Name)
      CALL SetupOutputVariable('Surface Outside Face Incident Beam Solar Radiation Rate per Area [W/m2]', &
                                QRadSWOutIncidentBeam(SurfLoop),'Zone', &
                               'Average',Surface(SurfLoop)%Name)
      CALL SetupOutputVariable('Surface Outside Face Incident Sky Diffuse Solar Radiation Rate per Area [W/m2]', &
                               QRadSWOutIncidentSkyDiffuse(SurfLoop),'Zone', &
                               'Average',Surface(SurfLoop)%Name)
      CALL SetupOutputVariable('Surface Outside Face Incident Ground Diffuse Solar Radiation Rate per Area [W/m2]', &
                                QRadSWOutIncidentGndDiffuse(SurfLoop),'Zone', &
                               'Average',Surface(SurfLoop)%Name)
      CALL SetupOutputVariable( &
                  'Surface Outside Face Beam Solar Incident Angle Cosine Value []',CosIncidenceAngle(SurfLoop),'Zone', &
                               'Average',Surface(SurfLoop)%Name)
      CALL SetupOutputVariable( &
                  'Surface Outside Face Incident Sky Diffuse Ground Reflected Solar Radiation Rate per Area [W/m2]', &
                               QRadSWOutIncSkyDiffReflGnd(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
      CALL SetupOutputVariable( &
                  'Surface Outside Face Incident Sky Diffuse Surface Reflected Solar Radiation Rate per Area [W/m2]', &
                               QRadSWOutIncSkyDiffReflObs(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
      CALL SetupOutputVariable( &
                  'Surface Outside Face Incident Beam To Beam Surface Reflected Solar Radiation Rate per Area [W/m2]', &
                               QRadSWOutIncBmToBmReflObs(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
      CALL SetupOutputVariable( &
                  'Surface Outside Face Incident Beam To Diffuse Surface Reflected Solar Radiation Rate per Area [W/m2]', &
                               QRadSWOutIncBmToDiffReflObs(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
      CALL SetupOutputVariable( &
                  'Surface Outside Face Incident Beam To Diffuse Ground Reflected Solar Radiation Rate per Area [W/m2]', &
                               QRadSWOutIncBmToDiffReflGnd(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
      CALL SetupOutputVariable('Surface Anisotropic Sky Multiplier []', &
                               AnisoSkyMult(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
      CALL SetupOutputVariable('Surface Window BSDF Beam Direction Number []',BSDFBeamDirectionRep(SurfLoop),'Zone','Average', &
                                Surface(SurfLoop)%Name)
      CALL SetupOutputVariable('Surface Window BSDF Beam Theta Angle [rad]',BSDFBeamThetaRep(SurfLoop),'Zone','Average', &
                                Surface(SurfLoop)%Name)
      CALL SetupOutputVariable('Surface Window BSDF Beam Phi Angle [rad]',BSDFBeamPhiRep(SurfLoop),'Zone','Average', &
                                Surface(SurfLoop)%Name)
    END IF
    IF (.NOT. Surface(SurfLoop)%HeatTransSurf) CYCLE

    IF (Surface(SurfLoop)%Class == SurfaceClass_Window) THEN
      ! CurrentModuleObject='Windows/GlassDoors'
      IF (Surface(SurfLoop)%ExtSolar) THEN
        CALL SetupOutputVariable('Surface Window Total Glazing Layers Absorbed Solar Radiation Rate [W]', &
                                  QRadSWwinAbsTot(SurfLoop),'Zone','Average', &
                                  Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window Total Glazing Layers Absorbed Shortwave Radiation Rate [W]',&
                                  SWwinAbsTotalReport(SurfLoop), &
                                 'Zone','Average', Surface(SurfLoop)%Name)

        IF (Construct(Surface(SurfLoop)%Construction)%WindowTypeBSDF) THEN
          NumOfLayers = Construct(Surface(SurfLoop)%Construction)%TotSolidLayers
        ELSE
          NumOfLayers = Construct(Surface(SurfLoop)%Construction)%TotLayers
        END IF
        DO I = 1, NumOfLayers
          CALL SetupOutputVariable('Surface Window Total Absorbed Shortwave Radiation Rate Layer '//TRIM(RoundSigDigits(I))//&
                                ' [W]', QRadSWwinAbsLayer(SurfLoop, I), 'Zone','Average', TRIM(Surface(SurfLoop)%Name))
          CALL SetupOutputVariable('Surface Window Front Face Temperature Layer '//TRIM(RoundSigDigits(I))//' [C]', &
                                FenLaySurfTempFront(SurfLoop, I), 'Zone','Average', TRIM(Surface(SurfLoop)%Name))
          CALL SetupOutputVariable('Surface Window Back Face Temperature Layer '//TRIM(RoundSigDigits(I))//' [C]', &
                                FenLaySurfTempBack(SurfLoop, I), 'Zone','Average', TRIM(Surface(SurfLoop)%Name))
        END DO

        CALL SetupOutputVariable('Surface Window Transmitted Solar Radiation Rate [W]', &
                                  WinTransSolar(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window Transmitted Beam Solar Radiation Rate [W]', &
                                  WinBmSolar(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)

        !added TH 12/9/2009
        CALL SetupOutputVariable('Surface Window Transmitted Beam To Beam Solar Radiation Rate [W]',WinBmBmSolar(SurfLoop),  &
           'Zone','Average',Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window Transmitted Beam To Diffuse Solar Radiation Rate [W]',WinBmDifSolar(SurfLoop),  &
           'Zone','Average',Surface(SurfLoop)%Name)

        CALL SetupOutputVariable('Surface Window Transmitted Diffuse Solar Radiation Rate [W]',WinDifSolar(SurfLoop),'Zone', &
                               'Average',Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window Heat Gain Rate [W]',WinHeatGainRep(SurfLoop), &
                                 'Zone','Average',Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window Heat Loss Rate [W]',WinHeatLossRep(SurfLoop), &
                                 'Zone','Average',Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window Gap Convective Heat Transfer Rate [W]',&
                                  WinGapConvHtFlowRep(SurfLoop),'Zone','Average',  &
                                  Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window Shading Device Absorbed Solar Radiation Rate [W]', &
                                  WinShadingAbsorbedSolar(SurfLoop), &
                                 'Zone','Average',Surface(SurfLoop)%Name)

        IF (DisplayAdvancedReportVariables) THEN
          ! CurrentModuleObject='Windows/GlassDoors(Advanced)'
          CALL SetupOutputVariable('Surface Window Inside Face Glazing Zone Convection Heat Gain Rate [W]', &
                                   WinGainConvGlazToZoneRep(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Inside Face Glazing Net Infrared Heat Transfer Rate [W]', &
                                   WinGainIRGlazToZoneRep(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Shortwave from Zone Back Out Window Heat Transfer Rate [W]', &
                                   WinLossSWZoneToOutWinRep(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Inside Face Frame and Divider Zone Heat Gain Rate [W]', &
                                   WinGainFrameDividerToZoneRep(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Inside Face Gap between Shade and Glazing Zone Convection Heat Gain Rate [W]', &
                                   WinGainConvGlazShadGapToZoneRep(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Inside Face Shade Zone Convection Heat Gain Rate [W]', &
                                   WinGainConvShadeToZoneRep(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Inside Face Shade Net Infrared Heat Transfer Rate [W]', &
                                   WinGainIRShadeToZoneRep(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
          IF ( Construct(Surface(SurfLoop)%Construction)%WindowTypeEQL ) THEN
               CALL SetupOutputVariable('Surface Window Inside Face Other Convection Heat Gain Rate [W]', &
                                         OtherConvGainInsideFaceToZoneRep(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
          ENDIF
        ENDIF


        ! Added TH 12/23/2008 for thermochromic windows
        ! CurrentModuleObject='Thermochromic Windows'
        IF (Construct(Surface(SurfLoop)%Construction)%TCFlag == 1) THEN
          CALL SetupOutputVariable('Surface Window Thermochromic Layer Temperature [C]',SurfaceWindow(SurfLoop)%TCLayerTemp, &
                                  'Zone','Average', Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Thermochromic Layer Property Specification Temperature [C]', &
                                  SurfaceWindow(SurfLoop)%SpecTemp,'Zone', 'Average', Surface(SurfLoop)%Name)
        ENDIF

        ! Added TH 5/26/2009 for switchable windows to report switching factor (tinted level)
        ! CurrentModuleObject='Switchable Windows'
        IF(Surface(SurfLoop)%WindowShadingControlPtr >0) THEN
          IF(WindowShadingControl(Surface(SurfLoop)%WindowShadingControlPtr)%ShadingType == WSC_ST_SwitchableGlazing)  THEN
          !IF (SurfaceWindow(SurfLoop)%ShadingFlag == SwitchableGlazing) THEN  !ShadingFlag is not set to SwitchableGlazing yet!
            CALL SetupOutputVariable('Surface Window Switchable Glazing Switching Factor []', &
                                 SurfaceWindow(SurfLoop)%SwitchingFactor, &
                                'Zone', 'Average',Surface(SurfLoop)%Name)
            CALL SetupOutputVariable('Surface Window Switchable Glazing Visible Transmittance []', &
                                 SurfaceWindow(SurfLoop)%VisTransSelected, &
                                'Zone', 'Average',Surface(SurfLoop)%Name)
          ENDIF
        ENDIF

        IF (SurfaceWindow(SurfLoop)%FrameArea > 0.0d0) THEN
          ! CurrentModuleObject='Window Frames'
          CALL SetupOutputVariable('Surface Window Frame Heat Gain Rate [W]',SurfaceWindow(SurfLoop)%FrameHeatGain, &
                                'Zone','Average',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Frame Heat Loss Rate [W]',SurfaceWindow(SurfLoop)%FrameHeatLoss, &
                                'Zone','Average',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Frame Inside Temperature [C]',SurfaceWindow(SurfLoop)%FrameTempSurfIn, &
                                'Zone','Average',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Frame Outside Temperature [C]',SurfaceWindow(SurfLoop)%FrameTempSurfOut, &
                                'Zone','Average',Surface(SurfLoop)%Name)
        ENDIF
        IF (SurfaceWindow(SurfLoop)%DividerArea > 0.0d0) THEN
          ! CurrentModuleObject='Window Dividers'
          CALL SetupOutputVariable('Surface Window Divider Heat Gain Rate [W]',SurfaceWindow(SurfLoop)%DividerHeatGain, &
                                'Zone','Average',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Divider Heat Loss Rate [W]',SurfaceWindow(SurfLoop)%DividerHeatLoss, &
                                'Zone','Average',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Divider Inside Temperature [C]',SurfaceWindow(SurfLoop)%DividerTempSurfIn, &
                                'Zone','Average',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Divider Outside Temperature [C]',SurfaceWindow(SurfLoop)%DividerTempSurfOut, &
                                'Zone','Average',Surface(SurfLoop)%Name)
        ENDIF

        ! CurrentModuleObject='Windows'
        ! Energy
        CALL SetupOutputVariable('Surface Window Total Glazing Layers Absorbed Solar Radiation Energy [J]', &
                                  QRadSWwinAbsTotEnergy(SurfLoop), &
                                'Zone','Sum', Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window Transmitted Solar Radiation Energy [J]',WinTransSolarEnergy(SurfLoop), &
                                'Zone','Sum',Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window Transmitted Beam Solar Radiation Energy [J]',WinBmSolarEnergy(SurfLoop), &
                                'Zone','Sum',Surface(SurfLoop)%Name)

        !added TH 12/9/2009
        CALL SetupOutputVariable('Surface Window Transmitted Beam To Beam Solar Radiation Energy [J]', &
                                  WinBmBmSolarEnergy(SurfLoop), &
                                 'Zone','Sum',Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window Transmitted Beam To Diffuse Solar Radiation Energy [J]', &
                                  WinBmDifSolarEnergy(SurfLoop), &
                                 'Zone','Sum',Surface(SurfLoop)%Name)

        CALL SetupOutputVariable('Surface Window Transmitted Diffuse Solar Radiation Energy [J]',WinDifSolarEnergy(SurfLoop), &
                                'Zone', 'Sum',Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window Heat Gain Energy [J]',WinHeatGainRepEnergy(SurfLoop), &
                                'Zone','Sum',Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window Heat Loss Energy [J]',WinHeatLossRepEnergy(SurfLoop), &
                                'Zone','Sum',Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window Gap Convective Heat Transfer Energy [J]',WinGapConvHtFlowRepEnergy(SurfLoop), &
                                'Zone','Sum',Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window Shading Device Absorbed Solar Radiation Energy [J]',  &
                                WinShadingAbsorbedSolarEnergy(SurfLoop), &
                                'Zone','Sum',Surface(SurfLoop)%Name)

        CALL SetupOutputVariable('Surface Window System Solar Transmittance []',WinSysSolTransmittance(SurfLoop), &
                                'Zone','Average',Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window System Solar Reflectance []',WinSysSolReflectance(SurfLoop), &
                                'Zone','Average',Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window System Solar Absorptance []',WinSysSolAbsorptance(SurfLoop), &
                                'Zone','Average',Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window Inside Face Glazing Condensation Status []', &
                                  InsideGlassCondensationFlag(SurfLoop), &
                                 'Zone','State',Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window Inside Face Frame Condensation Status []',InsideFrameCondensationFlag(SurfLoop), &
                                 'Zone','State',Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window Inside Face Divider Condensation Status []', &
                                  InsideDividerCondensationFlag(SurfLoop), &
                                 'Zone','State',Surface(SurfLoop)%Name)

      ! Outside reveal report variables
      !IF (Surface(SurfLoop)%Reveal > 0.0) THEN
        CALL SetupOutputVariable('Surface Window Outside Reveal Reflected Beam Solar Radiation Rate [W]', &
                                 SurfaceWindow(SurfLoop)%BmSolRefldOutsRevealReport, &
                                'Zone', 'State',Surface(SurfLoop)%Name)
        ! Energy
        CALL SetupOutputVariable('Surface Window Outside Reveal Reflected Beam Solar Radiation Energy [J]', &
                                 SurfaceWindow(SurfLoop)%BmSolRefldOutsRevealRepEnergy, &
                                'Zone', 'Sum',Surface(SurfLoop)%Name)
      !ENDIF

      ! Inside reveal report variables
        IF (SurfaceWindow(SurfLoop)%InsideReveal > 0.0d0 .OR. SurfaceWindow(SurfLoop)%InsideSillDepth > 0.0d0) THEN
          CALL SetupOutputVariable('Surface Window Inside Reveal Reflected Beam Solar Radiation Rate [W]', &
                                   SurfaceWindow(SurfLoop)%BmSolRefldInsRevealReport, &
                                   'Zone', 'State',Surface(SurfLoop)%Name)
          ! Energy
          CALL SetupOutputVariable('Surface Window Inside Reveal Reflected Beam Solar Radiation Energy [J]', &
                                   SurfaceWindow(SurfLoop)%BmSolRefldInsRevealRepEnergy, &
                                  'Zone', 'Sum',Surface(SurfLoop)%Name)

          ! Added report variables for inside reveal to debug CR 7596. TH 5/26/2009
          ! All reflected solar by the inside reveal is turned into diffuse
          CALL SetupOutputVariable('Surface Window Inside Reveal Absorbed Beam Solar Radiation Rate [W]', &
                                   SurfaceWindow(SurfLoop)%BmSolAbsdInsRevealReport, &
                                  'Zone', 'State',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Inside Reveal Reflected Diffuse Zone Solar Radiation Rate [W]', &
                                   SurfaceWindow(SurfLoop)%InsRevealDiffIntoZoneReport, &
                                  'Zone', 'State',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Inside Reveal Reflected Diffuse Frame Solar Radiation Rate [W]', &
                                   SurfaceWindow(SurfLoop)%InsRevealDiffOntoFrameReport, &
                                  'Zone', 'State',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Inside Reveal Reflected Diffuse Glazing Solar Radiation Rate [W]', &
                                   SurfaceWindow(SurfLoop)%InsRevealDiffOntoGlazingReport, &
                                  'Zone', 'State',Surface(SurfLoop)%Name)
        ENDIF

    !     Output blind report variables only when blinds are used
        IF(SurfaceWindow(SurfLoop)%BlindNumber .GT. 0)THEN
          ! CurrentModuleObject='Window Blinds'
          CALL SetupOutputVariable('Surface Window Blind Beam to Beam Solar Transmittance []', &
                                    SurfaceWindow(SurfLoop)%BlTsolBmBm, &
                                   'Zone', 'State',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Blind Beam to Diffuse Solar Transmittance []', &
                                    SurfaceWindow(SurfLoop)%BlTsolBmDif, &
                                   'Zone', 'State',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Blind Diffuse to Diffuse Solar Transmittance []', &
                                    SurfaceWindow(SurfLoop)%BlTsolDifDif, &
                                   'Zone', 'State',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Blind and Glazing System Beam Solar Transmittance []', &
                                    SurfaceWindow(SurfLoop)%BlGlSysTsolBmBm, &
                                   'Zone', 'State',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Blind and Glazing System Diffuse Solar Transmittance []', &
                                    SurfaceWindow(SurfLoop)%BlGlSysTsolDifDif, &
                                   'Zone', 'State',Surface(SurfLoop)%Name)
        END IF

  !     Output screen report variables only when screens are used
        IF(SurfaceWindow(SurfLoop)%ScreenNumber .GT. 0)THEN
          ! CurrentModuleObject='Window Screens'
          CALL SetupOutputVariable('Surface Window Screen Beam to Beam Solar Transmittance []', &
                                    SurfaceWindow(SurfLoop)%ScTsolBmBm, &
                                   'Zone', 'State',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Screen Beam to Diffuse Solar Transmittance []', &
                                    SurfaceWindow(SurfLoop)%ScTsolBmDif, &
                                   'Zone', 'State',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Screen Diffuse to Diffuse Solar Transmittance []', &
                                    SurfaceWindow(SurfLoop)%ScTsolDifDif, &
                                   'Zone', 'State',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Screen and Glazing System Beam Solar Transmittance []', &
                                    SurfaceWindow(SurfLoop)%ScGlSysTsolBmBm, &
                                   'Zone', 'State',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Screen and Glazing System Diffuse Solar Transmittance []', &
                                    SurfaceWindow(SurfLoop)%ScGlSysTsolDifDif, &
                                   'Zone', 'State',Surface(SurfLoop)%Name)
        END IF

        ! CurrentModuleObject='Windows'
        CALL SetupOutputVariable('Surface Window Solar Horizontal Profile Angle [deg]', &
                                 SurfaceWindow(SurfLoop)%ProfileAngHor, &
                                'Zone', 'State',Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window Solar Vertical Profile Angle [deg]', &
                                 SurfaceWindow(SurfLoop)%ProfileAngVert, &
                                'Zone', 'State',Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window Glazing Beam to Beam Solar Transmittance []', &
                                 SurfaceWindow(SurfLoop)%GlTsolBmBm, &
                                'Zone', 'State',Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window Glazing Beam to Diffuse Solar Transmittance []', &
                                 SurfaceWindow(SurfLoop)%GlTsolBmDif, &
                                'Zone', 'State',Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window Glazing Diffuse to Diffuse Solar Transmittance []', &
                                 SurfaceWindow(SurfLoop)%GlTsolDifDif, &
                                'Zone', 'State',Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Surface Window Model Solver Iteration Count []', &
                                 SurfaceWindow(SurfLoop)%WindowCalcIterationsRep, &
                                'Zone', 'State',Surface(SurfLoop)%Name)
      ELSEIF (.not. Surface(SurfLoop)%ExtSolar) THEN ! Not ExtSolar
        IF (DisplayAdvancedReportVariables) THEN
          ! CurrentModuleObject='InteriorWindows(Advanced)'
          IF(SurfaceWindow(SurfLoop)%OriginalClass /= SurfaceClass_TDD_Diffuser) &
            CALL SetupOutputVariable('Surface Window Total Glazing Layers Absorbed Solar Radiation Rate [W]', &
                                      QRadSWwinAbsTot(SurfLoop),'Zone','Average', &
                                      Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Total Glazing Layers Absorbed Shortwave Radiation Rate [W]', &
                                    SWwinAbsTotalReport(SurfLoop), &
                                   'Zone','Average', Surface(SurfLoop)%Name)
          IF(SurfaceWindow(SurfLoop)%OriginalClass /= SurfaceClass_TDD_Diffuser) &
             CALL SetupOutputVariable('Surface Window Transmitted Solar Radiation Rate [W]', &
                                       WinTransSolar(SurfLoop),'Zone','Average',  &
                                       Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Transmitted Beam Solar Radiation Rate [W]',WinBmSolar(SurfLoop), &
                                   'Zone','Average',Surface(SurfLoop)%Name)

          !added TH 12/9/2009
          CALL SetupOutputVariable('Surface Window Transmitted Beam To Beam Solar Radiation Rate [W]',WinBmBmSolar(SurfLoop), &
                                   'Zone','Average',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Transmitted Beam To Diffuse Solar Radiation Rate [W]', &
                                    WinBmDifSolar(SurfLoop), &
                                   'Zone','Average',Surface(SurfLoop)%Name)

          CALL SetupOutputVariable('Surface Window Transmitted Diffuse Solar Radiation Rate [W]',WinDifSolar(SurfLoop),'Zone', &
                                 'Average',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Heat Gain Rate [W]', &
                                    WinHeatGainRep(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Heat Loss Rate [W]', &
                                    WinHeatLossRep(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Gap Convective Heat Transfer Rate [W]', &
                                  WinGapConvHtFlowRep(SurfLoop),'Zone','Average',  &
                                  Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Shading Device Absorbed Solar Radiation Rate [W]', &
                                    WinShadingAbsorbedSolar(SurfLoop), &
                                   'Zone','Average',Surface(SurfLoop)%Name)
          IF (SurfaceWindow(SurfLoop)%FrameArea > 0.0d0) THEN
            CALL SetupOutputVariable('Surface Window Frame Heat Gain Rate [W]',SurfaceWindow(SurfLoop)%FrameHeatGain, &
                                  'Zone','Average',Surface(SurfLoop)%Name)
            CALL SetupOutputVariable('Surface Window Frame Heat Loss Rate [W]',SurfaceWindow(SurfLoop)%FrameHeatLoss, &
                                  'Zone','Average',Surface(SurfLoop)%Name)
            CALL SetupOutputVariable('Surface Window Frame Inside Temperature [C]',SurfaceWindow(SurfLoop)%FrameTempSurfIn, &
                                  'Zone','Average',Surface(SurfLoop)%Name)
            CALL SetupOutputVariable('Surface Window Frame Outside Temperature [C]',SurfaceWindow(SurfLoop)%FrameTempSurfOut, &
                                  'Zone','Average',Surface(SurfLoop)%Name)
          ENDIF
          IF (SurfaceWindow(SurfLoop)%DividerArea > 0.0d0) THEN
            CALL SetupOutputVariable('Surface Window Divider Heat Gain Rate [W]',SurfaceWindow(SurfLoop)%DividerHeatGain, &
                                  'Zone','Average',Surface(SurfLoop)%Name)
            CALL SetupOutputVariable('Surface Window Divider Heat Loss Rate [W]',SurfaceWindow(SurfLoop)%DividerHeatLoss, &
                                  'Zone','Average',Surface(SurfLoop)%Name)
            CALL SetupOutputVariable('Surface Window Divider Inside Temperature [C]',SurfaceWindow(SurfLoop)%DividerTempSurfIn, &
                                  'Zone','Average',Surface(SurfLoop)%Name)
            CALL SetupOutputVariable('Surface Window Divider Outside Temperature [C]',SurfaceWindow(SurfLoop)%DividerTempSurfOut, &
                                  'Zone','Average',Surface(SurfLoop)%Name)
          ENDIF
          ! Energy
          IF(SurfaceWindow(SurfLoop)%OriginalClass /= SurfaceClass_TDD_Diffuser) &
            CALL SetupOutputVariable('Surface Window Total Glazing Layers Absorbed Solar Radiation Energy [J]', &
                                      QRadSWwinAbsTotEnergy(SurfLoop), &
                                     'Zone','Sum', Surface(SurfLoop)%Name)
          IF(SurfaceWindow(SurfLoop)%OriginalClass /= SurfaceClass_TDD_Diffuser) &
            CALL SetupOutputVariable('Surface Window Transmitted Solar Radiation Energy [J]',WinTransSolarEnergy(SurfLoop), &
                                  'Zone','Sum',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Transmitted Beam Solar Radiation Energy [J]',WinBmSolarEnergy(SurfLoop), &
                                  'Zone','Sum',Surface(SurfLoop)%Name)

          CALL SetupOutputVariable('Surface Window Transmitted Beam To Beam Solar Radiation Energy [J]', &
                                    WinBmBmSolarEnergy(SurfLoop), &
                                   'Zone','Sum',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Transmitted Beam To Diffuse Solar Radiation Energy [J]', &
                                    WinBmDifSolarEnergy(SurfLoop), &
                                   'Zone','Sum',Surface(SurfLoop)%Name)

          CALL SetupOutputVariable('Surface Window Transmitted Diffuse Solar Radiation Energy [J]',WinDifSolarEnergy(SurfLoop), &
                                  'Zone', 'Sum',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Heat Gain Energy [J]',WinHeatGainRepEnergy(SurfLoop), &
                                  'Zone','Sum',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Heat Loss Energy [J]',WinHeatLossRepEnergy(SurfLoop), &
                                  'Zone','Sum',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Gap Convective Heat Transfer Energy [J]',WinGapConvHtFlowRepEnergy(SurfLoop), &
                                  'Zone','Sum',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Shading Device Absorbed Solar Radiation Energy [J]',  &
                                   WinShadingAbsorbedSolarEnergy(SurfLoop), &
                                  'Zone','Sum',Surface(SurfLoop)%Name)

          CALL SetupOutputVariable('Surface Window System Solar Transmittance []',WinSysSolTransmittance(SurfLoop), &
                                  'Zone','Average',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window System Solar Reflectance []',WinSysSolReflectance(SurfLoop), &
                                  'Zone','Average',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window System Solar Absorptance []',WinSysSolAbsorptance(SurfLoop), &
                                  'Zone','Average',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Inside Face Glazing Condensation Status []', &
                                    InsideGlassCondensationFlag(SurfLoop), &
                                   'Zone','State',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Inside Face Frame Condensation Status []', &
                                    InsideFrameCondensationFlag(SurfLoop), &
                                   'Zone','State',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Inside Face Divider Condensation Status []', &
                                    InsideDividerCondensationFlag(SurfLoop), &
                                   'Zone','State',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Outside Reveal Reflected Beam Solar Radiation Rate [W]', &
                                   SurfaceWindow(SurfLoop)%BmSolRefldOutsRevealReport, &
                                  'Zone', 'State',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Inside Reveal Reflected Beam Solar Radiation Rate [W]', &
                                   SurfaceWindow(SurfLoop)%BmSolRefldInsRevealReport, &
                                  'Zone', 'State',Surface(SurfLoop)%Name)
          ! Energy
          CALL SetupOutputVariable('Surface Window Outside Reveal Reflected Beam Solar Radiation Energy [J]', &
                                   SurfaceWindow(SurfLoop)%BmSolRefldOutsRevealRepEnergy, &
                                  'Zone', 'Sum',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Inside Reveal Reflected Beam Solar Radiation Energy [J]', &
                                   SurfaceWindow(SurfLoop)%BmSolRefldInsRevealRepEnergy, &
                                  'Zone', 'Sum',Surface(SurfLoop)%Name)

    !     Output blind report variables only when blinds are used
          IF(SurfaceWindow(SurfLoop)%BlindNumber .GT. 0)THEN
            CALL SetupOutputVariable('Surface Window Blind Beam to Beam Solar Transmittance []', &
                                      SurfaceWindow(SurfLoop)%BlTsolBmBm, &
                                     'Zone', 'State',Surface(SurfLoop)%Name)
            CALL SetupOutputVariable('Surface Window Blind Beam to Diffuse Solar Transmittance []', &
                                      SurfaceWindow(SurfLoop)%BlTsolBmDif, &
                                     'Zone', 'State',Surface(SurfLoop)%Name)
            CALL SetupOutputVariable('Surface Window Blind Diffuse to Diffuse Solar Transmittance []', &
                                      SurfaceWindow(SurfLoop)%BlTsolDifDif, &
                                     'Zone', 'State',Surface(SurfLoop)%Name)
            CALL SetupOutputVariable('Surface Window Blind and Glazing System Beam Solar Transmittance []', &
                                      SurfaceWindow(SurfLoop)%BlGlSysTsolBmBm, &
                                     'Zone', 'State',Surface(SurfLoop)%Name)
            CALL SetupOutputVariable('Surface Window Blind and Glazing System Diffuse Solar Transmittance []', &
                                      SurfaceWindow(SurfLoop)%BlGlSysTsolDifDif, &
                                     'Zone', 'State',Surface(SurfLoop)%Name)
          END IF

    !     Output screen report variables only when screens are used
          IF(SurfaceWindow(SurfLoop)%ScreenNumber .GT. 0)THEN
            CALL SetupOutputVariable('Surface Window Screen Beam to Beam Solar Transmittance []', &
                                      SurfaceWindow(SurfLoop)%ScTsolBmBm, &
                                     'Zone', 'State',Surface(SurfLoop)%Name)
            CALL SetupOutputVariable('Surface Window Screen Beam to Diffuse Solar Transmittance []', &
                                      SurfaceWindow(SurfLoop)%ScTsolBmDif, &
                                     'Zone', 'State',Surface(SurfLoop)%Name)
            CALL SetupOutputVariable('Surface Window Screen Diffuse to Diffuse Solar Transmittance []', &
                                      SurfaceWindow(SurfLoop)%ScTsolDifDif, &
                                     'Zone', 'State',Surface(SurfLoop)%Name)
            CALL SetupOutputVariable('Surface Window Screen and Glazing System Beam Solar Transmittance []', &
                                      SurfaceWindow(SurfLoop)%ScGlSysTsolBmBm, &
                                     'Zone', 'State',Surface(SurfLoop)%Name)
            CALL SetupOutputVariable('Surface Window Screen and Glazing System Diffuse Solar Transmittance []', &
                                      SurfaceWindow(SurfLoop)%ScGlSysTsolDifDif, &
                                     'Zone', 'State',Surface(SurfLoop)%Name)
          END IF

          CALL SetupOutputVariable('Surface Window Solar Horizontal Profile Angle [deg]', &
                                   SurfaceWindow(SurfLoop)%ProfileAngHor, &
                                  'Zone', 'State',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Solar Vertical Profile Angle [deg]', &
                                   SurfaceWindow(SurfLoop)%ProfileAngVert, &
                                  'Zone', 'State',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Glazing Beam to Beam Solar Transmittance []', &
                                   SurfaceWindow(SurfLoop)%GlTsolBmBm, &
                                  'Zone', 'State',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Glazing Beam to Diffuse Solar Transmittance []', &
                                   SurfaceWindow(SurfLoop)%GlTsolBmDif, &
                                  'Zone', 'State',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Glazing Diffuse to Diffuse Solar Transmittance []', &
                                   SurfaceWindow(SurfLoop)%GlTsolDifDif, &
                                  'Zone', 'State',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Surface Window Model Solver Iteration Count []', &
                                   SurfaceWindow(SurfLoop)%WindowCalcIterationsRep, &
                                  'Zone', 'State',Surface(SurfLoop)%Name)
        END IF
      END IF ! end non extsolar reporting as advanced variables
    END IF  ! Window Reporting
    IF (Surface(SurfLoop)%Class == SurfaceClass_Window .AND. Surface(SurfLoop)%ExtBoundCond > 0   &
         .and. Surface(SurfLoop)%ExtBoundCond /= SurfLoop) THEN  !Interzone window
      ! CurrentModuleObject='InterzoneWindows'
      CALL SetupOutputVariable('Surface Window Transmitted Beam Solar Radiation Rate [W]', &
                               SurfaceWindow(SurfLoop)%BmSolTransThruIntWinRep, &
                              'Zone', 'State',Surface(SurfLoop)%Name)
      !energy
      CALL SetupOutputVariable('Surface Window Transmitted Beam Solar Radiation Energy [J]', &
                               SurfaceWindow(SurfLoop)%BmSolTransThruIntWinRepEnergy, &
                              'Zone', 'Sum',Surface(SurfLoop)%Name)
    END IF
    IF(Surface(SurfLoop)%Class == SurfaceClass_TDD_Dome .AND. Surface(SurfLoop)%ExtSolar) THEN
      ! CurrentModuleObject='TDD Domes'
      CALL SetupOutputVariable('Surface Window Total Glazing Layers Absorbed Solar Radiation Rate [W]',&
                                QRadSWwinAbsTot(SurfLoop),'Zone','Average', &
                                Surface(SurfLoop)%Name)
      CALL SetupOutputVariable('Surface Window Transmitted Solar Radiation Rate [W]', &
                                WinTransSolar(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
      !energy
      CALL SetupOutputVariable('Surface Window Total Glazing Layers Absorbed Solar Radiation Energy [J]', &
                                QRadSWwinAbsTotEnergy(SurfLoop), &
                               'Zone','Sum', Surface(SurfLoop)%Name)
      CALL SetupOutputVariable('Surface Window Transmitted Solar Radiation Energy [J]',WinTransSolarEnergy(SurfLoop), &
                              'Zone','Sum',Surface(SurfLoop)%Name)
    END IF
    IF(SurfaceWindow(SurfLoop)%OriginalClass == SurfaceClass_TDD_Diffuser) THEN
      ! CurrentModuleObject='TDD Diffusers'
      CALL SetupOutputVariable('Surface Outside Face Incident Solar Radiation Rate per Area [W/m2]', &
                                QRadSWOutIncident(SurfLoop),'Zone','Average', &
                                Surface(SurfLoop)%Name)
      CALL SetupOutputVariable('Surface Window Total Glazing Layers Absorbed Solar Radiation Rate [W]', &
                                QRadSWwinAbsTot(SurfLoop),'Zone','Average', &
                                Surface(SurfLoop)%Name)
      CALL SetupOutputVariable('Surface Window Transmitted Solar Radiation Rate [W]', &
                                WinTransSolar(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
      !energy
      CALL SetupOutputVariable('Surface Window Total Glazing Layers Absorbed Solar Radiation Energy [J]', &
                                QRadSWwinAbsTotEnergy(SurfLoop), &
                               'Zone','Sum', Surface(SurfLoop)%Name)
      CALL SetupOutputVariable('Surface Window Transmitted Solar Radiation Energy [J]',WinTransSolarEnergy(SurfLoop),&
                              'Zone','Sum',Surface(SurfLoop)%Name)
    END IF
  ENDDO

  DO SurfLoop=1,TotSurfaces
    IF (.NOT. Surface(SurfLoop)%HeatTransSurf) CYCLE
    ! CurrentModuleObject='Surfaces'
    CALL SetupOutputVariable('Surface Inside Face Exterior Windows Incident Beam Solar Radiation Rate per Area [W/m2]',  &
                              BmIncInsSurfIntensRep(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
    CALL SetupOutputVariable('Surface Inside Face Exterior Windows Incident Beam Solar Radiation Rate [W]',  &
                              BmIncInsSurfAmountRep(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
    CALL SetupOutputVariable('Surface Inside Face Interior Windows Incident Beam Solar Radiation Rate per Area [W/m2]',  &
                              IntBmIncInsSurfIntensRep(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
    CALL SetupOutputVariable('Surface Inside Face Interior Windows Incident Beam Solar Radiation Rate [W]',  &
                              IntBmIncInsSurfAmountRep(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
    CALL SetupOutputVariable('Surface Inside Face Initial Transmitted Diffuse Absorbed Solar Radiation Rate [W]',  &
                              InitialDifSolInAbsReport(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
    CALL SetupOutputVariable('Surface Inside Face Initial Transmitted Diffuse Transmitted Out Window Solar Radiation Rate [W]',  &
                              InitialDifSolInTransReport(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
    CALL SetupOutputVariable('Surface Inside Face Absorbed Shortwave Radiation Rate [W]',  &
                              SWInAbsTotalReport(SurfLoop),'Zone','Average',Surface(SurfLoop)%Name)
    !energy
    CALL SetupOutputVariable('Surface Inside Face Exterior Windows Incident Beam Solar Radiation Energy [J]',  &
                              BmIncInsSurfAmountRepEnergy(SurfLoop),'Zone','Sum',Surface(SurfLoop)%Name)
    CALL SetupOutputVariable('Surface Inside Face Interior Windows Incident Beam Solar Radiation Energy [J]',  &
                              IntBmIncInsSurfAmountRepEnergy(SurfLoop),'Zone','Sum',Surface(SurfLoop)%Name)
  END DO

  RETURN

END SUBROUTINE AllocateModuleArrays

SUBROUTINE AnisoSkyViewFactors

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   April 1999
          !       MODIFIED       LKL; Dec 2002 -- Anisotropic is only sky radiance option
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates view factor multiplier, AnisoSkyMult, for diffuse
          ! sky irradiance on exterior surfaces taking into account
          ! anisotropic radiance of the sky. Called by InitSurfaceHeatBalance
          !
          ! In this case the diffuse sky irradiance on a surface is given by
          !
          !  AnisoSkyMult(SurfNum) * DifSolarRad
          !
          ! AnisoSkyMult accounts not only for the sky radiance distribution but
          ! also for the effects of shading of sky diffuse radiation by
          ! shadowing surfaces such as overhangs. It does not account for reflection
          ! of sky diffuse radiation from shadowing surfaces.
          !
          ! Based on an empirical model described in
          ! R. Perez, P. Ineichen, R. Seals, J. Michalsky and R. Stewart,
          ! "Modeling Daylight Availability and Irradiance Components from Direct
          ! and Global Irradiance," Solar Energy 44, 271-289, 1990.
          ! In this model the radiance of the sky consists of three distributions
          ! that are superimposed:

          ! (1) An isotropic distribution that covers the entire sky dome;
          ! (2) A circumsolar brightening centered around the position of the sun;
          ! (3) A horizon brightening
          !
          ! The circumsolar brightening is assumed to be concentrated at a point
          ! source at the center of the sun although this region actually begins at the
          ! periphery of the solar disk and falls off in intensity with increasing
          ! angular distance from the periphery.
          !
          ! The horizon brightening is assumed to be concentrated at the horizon and
          ! to be independent of azimuth. In actuality, for clear skies, the horizon
          ! brightening is highest at the horizon and decreases in intensity away from
          ! the horizon. For overcast skies the horizon brightening has a negative value
          ! since for such skies the sky radiance increases rather than decreases away
          ! from the horizon.
          !
          ! The F11R, F12R, etc. values were provided by R. Perez, private communication,
          ! 5/21/99. These values have higher precision than those listed in the above
          ! paper.

          ! USE STATEMENTS:
USE DataSystemVariables, ONLY: DetailedSkyDiffuseAlgorithm

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
REAL(r64), PARAMETER, DIMENSION(7) :: EpsilonLimit=  &         ! Upper limit of bins of the sky clearness parameter, Epsilon
      (/1.065d0,1.23d0,1.5d0,1.95d0,2.8d0,4.5d0,6.2d0/)
       ! Circumsolar brightening coefficients; index corresponds to range of Epsilon, the sky clearness parameter
REAL(r64), PARAMETER, DIMENSION(8) :: F11R(8)=  &
               (/ -0.0083117d0,  0.1299457d0,  0.3296958d0,  0.5682053d0, &
                   0.8730280d0,  1.1326077d0,  1.0601591d0,  0.6777470d0 /)
REAL(r64), PARAMETER, DIMENSION(8) :: F12R(8)=  &
               (/  0.5877285d0,  0.6825954d0,  0.4868735d0,  0.1874525d0, &
                  -0.3920403d0, -1.2367284d0, -1.5999137d0, -0.3272588d0 /)
REAL(r64), PARAMETER, DIMENSION(8) :: F13R(8)=  &
               (/ -0.0620636d0, -0.1513752d0, -0.2210958d0, -0.2951290d0, &
                  -0.3616149d0, -0.4118494d0, -0.3589221d0, -0.2504286d0 /)
       ! Horizon/zenith brightening coefficient array; index corresponds to range of Epsilon, the sky clearness parameter
REAL(r64), PARAMETER, DIMENSION(8) :: F21R(8)=  &
               (/ -0.0596012d0, -0.0189325d0,  0.0554140d0,  0.1088631d0, &
                   0.2255647d0,  0.2877813d0,  0.2642124d0,  0.1561313d0 /)
REAL(r64), PARAMETER, DIMENSION(8) :: F22R(8)=  &
               (/  0.0721249d0,  0.0659650d0, -0.0639588d0, -0.1519229d0, &
                  -0.4620442d0, -0.8230357d0, -1.1272340d0, -1.3765031d0 /)
REAL(r64), PARAMETER, DIMENSION(8) :: F23R(8)=  &
               (/ -0.0220216d0, -0.0288748d0, -0.0260542d0, -0.0139754d0, &
                   0.0012448d0,  0.0558651d0,  0.1310694d0,  0.2506212d0 /)

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

REAL(r64)   :: CosZenithAng            ! Cosine of solar zenith angle
REAL(r64)   :: ZenithAng               ! Solar zenith angle (radians)
REAL(r64)   :: ZenithAngDeg            ! Solar zenith angle (degrees)
REAL(r64)   :: F1                      ! Circumsolar brightening coefficient
REAL(r64)   :: F2                      ! Horizon/zenith brightening coefficient
REAL(r64)   :: Epsilon                 ! Sky clearness parameter
REAL(r64)   :: Delta                   ! Sky brightness parameter
REAL(r64)   :: CosIncAngBeamOnSurface  ! Cosine of incidence angle of beam solar on surface
REAL(r64)   :: IncAng                  ! Incidence angle of beam solar on surface (radians)
INTEGER     :: SurfNum                 ! Surface number
INTEGER     :: EpsilonBin              ! Sky clearness (Epsilon) bin index
REAL(r64)   :: AirMass                 ! Relative air mass
REAL(r64)   :: AirMassH                ! Intermediate variable for relative air mass calculation
REAL(r64)   :: CircumSolarFac          ! Ratio of cosine of incidence angle to cosine of zenith angle
REAL(r64)   :: KappaZ3                 ! Intermediate variable
REAL(r64)   :: ViewFactorSkyGeom       ! Geometrical sky view factor


        ! FLOW:
#ifdef EP_Count_Calls
      NumAnisoSky_Calls=NumAnisoSky_Calls+1
#endif

      CosZenithAng = SOLCOS(3)
      ZenithAng = ACOS(CosZenithAng)
      ZenithAngDeg = ZenithAng/DegToRadians

      AnisoSkyMult = 0.0d0

!           Relative air mass
      AirMassH = (1.d0 - 0.1d0 * Elevation / 1000.d0)
      IF(ZenithAngDeg <= 75.d0) THEN
        AirMass  = AirMassH/CosZenithAng
      ELSE
        AirMass = AirMassH/(CosZenithAng + 0.15d0*(93.9d0-ZenithAngDeg)**(-1.253d0))
      END IF
      KappaZ3 = 1.041d0*ZenithAng**3
      Epsilon = ((BeamSolarRad+DifSolarRad)/DifSolarRad + KappaZ3)/(1.0d0+KappaZ3)
      Delta = DifSolarRad*AirMass/1353.d0 ! 1353 is average extraterrestrial irradiance (W/m2)
!           Circumsolar (F1) and horizon/zenith (F2) brightening coefficients
      DO EpsilonBin=1,8
        IF(EpsilonBin == 8) EXIT
        IF (Epsilon < EpsilonLimit(EpsilonBin)) EXIT
      END DO
      F1 = MAX(0.d0,F11R(EpsilonBin) + F12R(EpsilonBin)*Delta + F13R(EpsilonBin)*ZenithAng)
      F2 = F21R(EpsilonBin) + F22R(EpsilonBin)*Delta + F23R(EpsilonBin)*ZenithAng

    DO SurfNum =1,TotSurfaces
      IF (.NOT. Surface(SurfNum)%ExtSolar) CYCLE

      CosIncAngBeamOnSurface = SOLCOS(1)*Surface(SurfNum)%OutNormVec(1)  &
                + SOLCOS(2)*Surface(SurfNum)%OutNormVec(2)  &
                + SOLCOS(3)*Surface(SurfNum)%OutNormVec(3)
      IncAng = ACOS(CosIncAngBeamOnSurface)

      ViewFactorSkyGeom = Surface(SurfNum)%ViewFactorSky
      MultIsoSky(SurfNum) = ViewFactorSkyGeom * (1.d0-F1)
!           0.0871557 below corresponds to a zenith angle of 85 deg
      CircumSolarFac = MAX(0.d0,CosIncAngBeamOnSurface)/MAX(0.0871557d0,CosZenithAng)
!           For near-horizontal roofs, model has an inconsistency that gives sky diffuse
!           irradiance significantly different from DifSolarRad when zenith angle is
!           above 85 deg. The following forces irradiance to be very close to DifSolarRad
!           in this case.
      IF(CircumSolarFac > 0.d0 .AND. CosZenithAng < 0.0871557d0 .AND. Surface(SurfNum)%Tilt < 2.0d0) &
        CircumSolarFac = 1.d0
      MultCircumSolar(SurfNum) = F1*CircumSolarFac
      MultHorizonZenith(SurfNum) = F2*Surface(SurfNum)%SinTilt
      IF (.not. DetailedSkyDiffuseAlgorithm .or. .not.  ShadingTransmittanceVaries .or.  &
          SolarDistribution == MinimalShadowing) THEN
        AnisoSkyMult(SurfNum) = &
          MultIsoSky(SurfNum) * DifShdgRatioIsoSky(SurfNum) + &
          MultCircumSolar(SurfNum) * SunLitFrac(SurfNum,HourOfDay,TimeStep) + &
          MultHorizonZenith(SurfNum) * DifShdgRatioHoriz(SurfNum)
      ELSE
        AnisoSkyMult(SurfNum) = &
          MultIsoSky(SurfNum) * DifShdgRatioIsoSkyHRTS(SurfNum,HourOfDay,TimeStep) + &
          MultCircumSolar(SurfNum) * SunLitFrac(SurfNum,HourOfDay,TimeStep) + &
          MultHorizonZenith(SurfNum) * DifShdgRatioHorizHRTS(SurfNum,HourOfDay,TimeStep)
        curDifShdgRatioIsoSky(SurfNum) = DifShdgRatioIsoSkyHRTS(SurfNum,HourOfDay,TimeStep)
      ENDIF
      AnisoSkyMult(SurfNum)=MAX(0.0d0,AnisoSkyMult(SurfNum))  ! make sure not negative.
    END DO

END SUBROUTINE AnisoSkyViewFactors

SUBROUTINE CHKBKS(NBS,NRS)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN
          !       MODIFIED       Nov 2001, FW: Reverse subroutine arguments NRS and NBS to
          !                                    correspond to how CHKBKS is called
          !                      Jan 2002, FW: change error message
          !       RE-ENGINEERED  Lawrie, Oct 2000

          ! PURPOSE OF THIS SUBROUTINE:
          ! Determines whether a any vertices of the back surface are in front of the receiving surface;
          ! if so, gives severe error.  Only base heat transfer surfaces are checked.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! BLAST/IBLAST code, original author George Walton

          ! USE STATEMENTS:
  USE Vectors

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: NBS       ! Surface Number of the potential back surface
  INTEGER, INTENT(IN) :: NRS       ! Surface Number of the potential shadow receiving surface

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: ValFmt="(F20.4)"

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER N             ! Loop Control (vertex counter)
  INTEGER NVRS          ! Number of vertices of the receiving surface
  INTEGER NVBS          ! Number of vertices of the back surface
  TYPE (Vector) AVec    ! Vector from vertex 2 to vertex 1, both same surface
  TYPE (Vector) BVec    ! Vector from vertex 2 to vertex 3, both same surface
  TYPE (Vector) CVec    ! Vector perpendicular to surface at vertex 2
  TYPE (Vector) DVec    ! Vector from vertex 2 of first surface to vertex 'n' of second surface
  REAL(r64) DOTP ! Dot product of C and D
  CHARACTER(len=20) CharDotP  ! for error messages
  CHARACTER(len=4) VTString

  NVRS=Surface(NRS)%Sides
  NVBS=Surface(NBS)%Sides

          ! SEE IF ANY VERTICES OF THE back surface ARE IN FRONT OF THE receiving surface

  AVec=Surface(NRS)%Vertex(1)-Surface(NRS)%Vertex(2)
  BVec=Surface(NRS)%Vertex(3)-Surface(NRS)%Vertex(2)

  CVec=BVec*AVec

  DO N = 1, NVBS
    DVec=Surface(NBS)%Vertex(N)-Surface(NRS)%Vertex(2)
    DOTP=CVec.dot.DVec
    IF (DOTP > .0009d0) THEN
      CALL ShowSevereError('Problem in interior solar distribution calculation (CHKBKS)')
      CALL ShowContinueError( &
       '   Solar Distribution = FullInteriorExterior will not work in Zone='//TRIM(Surface(NRS)%ZoneName))
      WRITE(VTString,'(I4)') N
      VTString=ADJUSTL(VTString)
      CALL ShowContinueError( &
       '   because vertex '//TRIM(VTString)//' of back surface='//TRIM(Surface(NBS)%Name) &
       //' is in front of receiving surface='//TRIM(Surface(NRS)%Name))
      WRITE(CharDotP,ValFmt) DOTP
      CharDotP=ADJUSTL(CharDotp)
      CALL ShowContinueError('   (Dot Product indicator='//TRIM(CharDotP)//')')
      CALL ShowContinueError('   Check surface geometry; if OK, use Solar Distribution = FullExterior instead.')
    END IF
  END DO

  RETURN

END SUBROUTINE CHKBKS

SUBROUTINE CHKGSS(NRS,NSS,ZMIN,CannotShade)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN
          !       MODIFIED       na
          !       RE-ENGINEERED  Lawrie, Oct 2000

          ! PURPOSE OF THIS SUBROUTINE:
          ! Determines the possible shadowing combinations.  The
          ! routine checks detached shadowing or base heat transfer surfaces
          ! for the possibility that they cannot shade a given base heat transfer surface.

          ! METHODOLOGY EMPLOYED:
          ! Shadowing is not possible if:
          ! 1.  The lowest point of the shadow receiving surface (receiving surface)
          !     Is higher than the highest point of the shadow casting surface (s.s.)
          ! 2.  The shadow casting surface Faces up (e.g. A flat roof)
          ! 3.  The shadow casting surface Is behind the receiving surface
          ! 4.  The receiving surface is behind the shadow casting surface

          ! REFERENCES:
          ! BLAST/IBLAST code, original author George Walton

          ! USE STATEMENTS:
  USE Vectors

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: ZMIN   ! Lowest point of the receiving surface
  INTEGER, INTENT(IN)          :: NRS    ! Surface number of the potential shadow receiving surface
  INTEGER, INTENT(IN)          :: NSS    ! Surface number of the potential shadow casting surface
  LOGICAL, INTENT(OUT)         :: CannotShade ! TRUE if shadow casting surface cannot shade receiving surface.

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(KIND=r64) :: TolValue=0.0003d0

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER I             ! Loop Control (vertex counter)
  INTEGER NVRS          ! Number of vertices of the receiving surface
  INTEGER NVSS          ! Number of vertices of the shadow casting surface
  TYPE (Vector) AVec    ! Vector from vertex 2 to vertex 1, both same surface
  TYPE (Vector) BVec    ! Vector from vertex 2 to vertex 3, both same surface
  TYPE (Vector) CVec    ! Vector perpendicular to surface at vertex 2
  TYPE (Vector) DVec    ! Vector from vertex 2 of first surface to vertex 'n' of second surface
  REAL(r64) ZMAX ! Highest point of the shadow casting surface
  REAL(r64) DOTP ! Dot Product

  CannotShade=.TRUE.
  NVRS=Surface(NRS)%Sides
  NVSS=Surface(NSS)%Sides

          ! see if no point of shadow casting surface is above low point of receiving surface

  ZMAX=MAXVAL(Surface(NSS)%Vertex(1:Surface(NSS)%Sides)%Z)
  IF (ZMAX <= ZMIN) RETURN

          ! SEE IF Shadow Casting Surface IS HORIZONTAL AND FACING UPWARD.

  IF (Surface(NSS)%OutNormVec(3) > 0.9999d0) RETURN

          ! SEE IF ANY VERTICES OF THE Shadow Casting Surface ARE ABOVE THE PLANE OF THE receiving surface

  AVec=Surface(NRS)%Vertex(1)-Surface(NRS)%Vertex(2)
  BVec=Surface(NRS)%Vertex(3)-Surface(NRS)%Vertex(2)

  CVec=BVec*AVec

  DO I = 1, NVSS
    DVec=Surface(NSS)%Vertex(I)-Surface(NRS)%Vertex(2)
    DOTP=CVec.dot.DVec
    IF (DOTP > TolValue) EXIT ! DO loop
  END DO

          ! SEE IF ANY VERTICES OF THE receiving surface ARE ABOVE THE PLANE OF THE S.S.

  IF (DOTP > TolValue) THEN

    AVec=Surface(NSS)%Vertex(1)-Surface(NSS)%Vertex(2)
    BVec=Surface(NSS)%Vertex(3)-Surface(NSS)%Vertex(2)

    CVec=BVec*AVec

    DO I = 1, NVRS
      DVec=Surface(NRS)%Vertex(I)-Surface(NSS)%Vertex(2)
      DOTP=CVec.dot.DVec
      IF (DOTP > TolValue) THEN
        CannotShade=.false.
        EXIT ! DO loop
      ENDIF
    END DO

  END IF

  RETURN

END SUBROUTINE CHKGSS

SUBROUTINE CHKSBS(HTS,GRSNR,SBSNR)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN
          !       MODIFIED       na
          !       RE-ENGINEERED  Lawrie, Oct 2000

          ! PURPOSE OF THIS SUBROUTINE:
          ! Checks that a subsurface is completely
          ! enclosed by its base surface.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! BLAST/IBLAST code, original author George Walton

          ! 3D Planar Polygons
          ! In 3D applications, one sometimes wants to test a point and polygon that are in the same plane.
          ! For example, one may have the intersection point of a ray with the plane of a polyhedron's face,
          ! and want to test if it is inside the face.  Or one may want to know if the base of a 3D perpendicular
          ! dropped from a point is inside a planar polygon.

          ! 3D inclusion is easily determined by projecting the point and polygon into 2D.  To do this, one simply
          ! ignores one of the 3D coordinates and uses the other two.  To optimally select the coordinate to ignore,
          ! compute a normal vector to the plane, and select the coordinate with the largest absolute value [Snyder & Barr, 1987].
          ! This gives the projection of the polygon with maximum area, and results in robust computations.
          ! John M. Snyder & Alan H. Barr, "Ray Tracing Complex Models Containing Surface Tessellations",
          ! Computer Graphics 21(4), 119-126 (1987) [also in the Proceedings of SIGGRAPH 1987]
          !--- using adapted routine from Triangulation code -- EnergyPlus.



          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: HTS      ! Heat transfer surface number of the general receiving surf
  INTEGER, INTENT(IN) :: GRSNR    ! Surface number of general receiving surface
  INTEGER, INTENT(IN) :: SBSNR    ! Surface number of subsurface

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! MSG - for error message
  CHARACTER(len=8), PARAMETER, DIMENSION(4) :: MSG = (/'misses  ','        ','within  ','overlaps'/)
  CHARACTER(len=*), PARAMETER :: AFormat='(1X,A)'
  CHARACTER(len=*), PARAMETER :: A4Format='(1X,A,A,A,A)'
  CHARACTER(len=*), PARAMETER :: A2I2Format='(1X,A,I5,A,I5)'
  CHARACTER(len=*), PARAMETER :: VFormat='(1X,A,I5,A,"(",2(F15.2,","),F15.2,")")'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER N        ! Loop Control
  INTEGER NVT      ! Number of vertices
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: XVT      ! X Vertices of
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: YVT      ! Y vertices of
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: ZVT      ! Z vertices of

  INTEGER NS1      ! Number of the figure being overlapped
  INTEGER NS2      ! Number of the figure doing overlapping
  INTEGER NS3      ! Location to place results of overlap

  LOGICAL, SAVE :: OneTimeFlag=.true.
  logical :: inside

  LOGICAL Out
  REAL(r64) X1,Y1,Z1,X2,Y2,Z2,BX,BY,BZ,BMAX !,SX,SY,SZ
!  INTEGER M

  IF (OneTimeFlag) THEN
    ALLOCATE(XVT(MaxVerticesPerSurface+1))
    ALLOCATE(YVT(MaxVerticesPerSurface+1))
    ALLOCATE(ZVT(MaxVerticesPerSurface+1))
    XVT=0.0d0
    YVT=0.0d0
    ZVT=0.0d0
    OneTimeFlag=.false.
  ENDIF

  NS1    = 1
  NS2    = 2
  NS3    = 3
  HCT(1) = 0.0d0
  HCT(2) = 0.0d0

          ! Put coordinates of base surface into clockwise sequence on the x'-y' plane.

  XVT=0.0d0
  YVT=0.0d0
  ZVT=0.0d0
  XVS=0.0d0
  YVS=0.0d0
  CALL CTRANS(GRSNR,HTS,NVT,XVT,YVT,ZVT)
  DO N = 1, NVT
    XVS(N) = XVT(NVT+1-N)
    YVS(N) = YVT(NVT+1-N)
  END DO

  CALL HTRANS1(NS2,NVT)

          ! Put coordinates of the subsurface into clockwise sequence.

  NVS=Surface(SBSNR)%Sides
  DO N = 1, NVS
    XVS(N) = ShadeV(SBSNR)%XV(NVS+1-N)
    YVS(N) = ShadeV(SBSNR)%YV(NVS+1-N)
  END DO
  CALL HTRANS1(NS1,NVS)

          ! Determine the overlap condition.

  CALL DeterminePolygonOverlap(NS1,NS2,NS3)

          ! Print error condition if necessary.

  IF (OverlapStatus /= FirstSurfWithinSecond) THEN
      OUT=.FALSE.
!C                            COMPUTE COMPONENTS OF VECTOR
!C                            NORMAL TO BASE SURFACE.
      X1 = Surface(GRSNR)%Vertex(1)%X-Surface(GRSNR)%Vertex(2)%X !XV(1,GRSNR)-XV(2,GRSNR)
      Y1 = Surface(GRSNR)%Vertex(1)%Y-Surface(GRSNR)%Vertex(2)%Y !YV(1,GRSNR)-YV(2,GRSNR)
      Z1 = Surface(GRSNR)%Vertex(1)%Z-Surface(GRSNR)%Vertex(2)%Z !ZV(1,GRSNR)-ZV(2,GRSNR)
      X2 = Surface(GRSNR)%Vertex(3)%X-Surface(GRSNR)%Vertex(2)%X !XV(3,GRSNR)-XV(2,GRSNR)
      Y2 = Surface(GRSNR)%Vertex(3)%Y-Surface(GRSNR)%Vertex(2)%Y !YV(3,GRSNR)-YV(2,GRSNR)
      Z2 = Surface(GRSNR)%Vertex(3)%Z-Surface(GRSNR)%Vertex(2)%Z !ZV(3,GRSNR)-ZV(2,GRSNR)
      BX = Y1*Z2-Y2*Z1
      BY = Z1*X2-Z2*X1
      BZ = X1*Y2-X2*Y1
!C                            FIND LARGEST COMPONENT.
      BMAX=MAX(ABS(BX),ABS(BY),ABS(BZ))
!C
      IF(ABS(BX).EQ.BMAX) THEN
!        write(outputfiledebug,*) ' looking bx-bmax',bmax
        DO N=1,Surface(SBSNR)%Sides !NV(SBSNR)
          inside=polygon_contains_point(Surface(GRSNR)%Sides,Surface(GRSNR)%Vertex,Surface(SBSNR)%Vertex(N),.true.,.false.,.false.)
          IF (.not. inside) THEN
            OUT=.true.
!            do m=1,surface(grsnr)%sides
!            write(outputfiledebug,*) 'grsnr,side=',m,surface(grsnr)%vertex
!            write(outputfiledebug,*) 'point outside=',surface(sbsnr)%vertex(n)
!            enddo
!            EXIT
          ENDIF
!          Y1 = Surface(GRSNR)%Vertex(Surface(GRSNR)%Sides)%Y-Surface(SBSNR)%Vertex(N)%Y !YV(NV(GRSNR),GRSNR)-YV(N,SBSNR)
!          Z1 = Surface(GRSNR)%Vertex(Surface(GRSNR)%Sides)%Z-Surface(SBSNR)%Vertex(N)%Z !ZV(NV(GRSNR),GRSNR)-ZV(N,SBSNR)
!          DO M=1,Surface(GRSNR)%Sides !NV(GRSNR)
!            Y2 = Y1
!            Z2 = Z1
!            Y1 = Surface(GRSNR)%Vertex(M)%Y-Surface(SBSNR)%Vertex(N)%Y !YV(M,GRSNR)-YV(N,SBSNR)
!            Z1 = Surface(GRSNR)%Vertex(M)%Z-Surface(SBSNR)%Vertex(N)%Z !ZV(M,GRSNR)-ZV(N,SBSNR)
!            SX = Y1*Z2-Y2*Z1
!            IF(SX*BX.LT.-1.0E-6) THEN
!              OUT=.TRUE.
!              write(outputfiledebug,*) 'sx*bx=',sx*bx
!              write(outputfiledebug,*) 'grsnr=',surface(grsnr)%vertex(m)
!              write(outputfiledebug,*) 'sbsnr=',surface(sbsnr)%vertex(n)
!            endif
!          ENDDO
!          IF (OUT) EXIT
        ENDDO
      ELSE IF(ABS(BY).EQ.BMAX) THEN
!        write(outputfiledebug,*) ' looking by-bmax',bmax
        DO N=1,Surface(SBSNR)%Sides !NV(SBSNR)
          inside=polygon_contains_point(Surface(GRSNR)%Sides,Surface(GRSNR)%Vertex,Surface(SBSNR)%Vertex(N),.false.,.true.,.false.)
          IF (.not. inside) THEN
            OUT=.true.
!            do m=1,surface(grsnr)%sides
!            write(outputfiledebug,*) 'grsnr,side=',m,surface(grsnr)%vertex
!            write(outputfiledebug,*) 'point outside=',surface(sbsnr)%vertex(n)
!            enddo
!            EXIT
          ENDIF
!          Z1 = Surface(GRSNR)%Vertex(Surface(GRSNR)%Sides)%Z-Surface(SBSNR)%Vertex(N)%Z !ZV(NV(GRSNR),GRSNR)-ZV(N,SBSNR)
!          X1 = Surface(GRSNR)%Vertex(Surface(GRSNR)%Sides)%X-Surface(SBSNR)%Vertex(N)%X !XV(NV(GRSNR),GRSNR)-XV(N,SBSNR)
!          DO M=1,Surface(GRSNR)%Sides !NV(GRSNR)
!            Z2 = Z1
!            X2 = X1
!            Z1 = Surface(GRSNR)%Vertex(M)%Z-Surface(SBSNR)%Vertex(N)%Z !ZV(M,GRSNR)-ZV(N,SBSNR)
!            X1 = Surface(GRSNR)%Vertex(M)%X-Surface(SBSNR)%Vertex(N)%X !XV(M,GRSNR)-XV(N,SBSNR)
!            SY = Z1*X2-Z2*X1
!            IF(SY*BY.LT.-1.0E-6) THEN
!              OUT=.TRUE.
!              write(outputfiledebug,*) 'sy*by=',sy*by
!              write(outputfiledebug,*) 'grsnr=',surface(grsnr)%vertex(m)
!              write(outputfiledebug,*) 'sbsnr=',surface(sbsnr)%vertex(n)
!            ENDIF
!          ENDDO
!          IF (OUT) EXIT
        ENDDO
      ELSE
!        write(outputfiledebug,*) ' looking bz-bmax',bmax
        DO N=1,Surface(SBSNR)%Sides !NV(SBSNR)
          inside=polygon_contains_point(Surface(GRSNR)%Sides,Surface(GRSNR)%Vertex,Surface(SBSNR)%Vertex(N),.false.,.false.,.true.)
          IF (.not. inside) THEN
            OUT=.true.
!            do m=1,surface(grsnr)%sides
!            write(outputfiledebug,*) 'grsnr,side=',m,surface(grsnr)%vertex
!            write(outputfiledebug,*) 'point outside=',surface(sbsnr)%vertex(n)
!            enddo
!            EXIT
          ENDIF
!          X1 = Surface(GRSNR)%Vertex(Surface(GRSNR)%Sides)%X-Surface(SBSNR)%Vertex(N)%X !XV(NV(GRSNR),GRSNR)-XV(N,SBSNR)
!          Y1 = Surface(GRSNR)%Vertex(Surface(GRSNR)%Sides)%Y-Surface(SBSNR)%Vertex(N)%Y !YV(NV(GRSNR),GRSNR)-YV(N,SBSNR)
!          DO M=1,Surface(GRSNR)%Sides !NV(GRSNR)
!            X2 = X1
!            Y2 = Y1
!            X1 = Surface(GRSNR)%Vertex(M)%X-Surface(SBSNR)%Vertex(N)%X !XV(M,GRSNR)-XV(N,SBSNR)
!            Y1 = Surface(GRSNR)%Vertex(M)%Y-Surface(SBSNR)%Vertex(N)%Y !YV(M,GRSNR)-YV(N,SBSNR)
!            SZ = X1*Y2-X2*Y1
!            IF(SZ*BZ.LT.-1.0E-6) THEN
!              OUT=.TRUE.
!              write(outputfiledebug,*) 'sz*bz=',sz*bz
!              write(outputfiledebug,*) 'grsnr=',surface(grsnr)%vertex(m)
!              write(outputfiledebug,*) 'sbsnr=',surface(sbsnr)%vertex(n)
!            ENDIF
!          ENDDO
!          IF (OUT) EXIT
        ENDDO
      END IF
!    CALL ShowWarningError('Base surface does not surround subsurface (CHKSBS), Overlap Status='//  &
!                           TRIM(cOverLapStatus(OverLapStatus)))
!    CALL ShowContinueError('Surface "'//TRIM(Surface(GRSNR)%Name)//'" '//TRIM(MSG(OverlapStatus))//  &
!                     ' SubSurface "'//TRIM(Surface(SBSNR)%Name)//'"')
!    IF (FirstSurroundError) THEN
!      CALL ShowWarningError('Base Surface does not surround subsurface errors occuring...'//  &
!                     'Check that the SurfaceGeometry object is expressing the proper starting corner and '//  &
!                     'direction [CounterClockwise/Clockwise]')
!      FirstSurroundError=.false.
!    ENDIF
IF (Out) THEN
    NumBaseSubSurround=NumBaseSubSurround+1
    IF (NumBaseSubSurround > 1) THEN
      ALLOCATE(TempSurfErrorTracking(NumBaseSubSurround))
      TempSurfErrorTracking(1:NumBaseSubSurround-1)=TrackBaseSubSurround
      DEALLOCATE(TrackBaseSubSurround)
      ALLOCATE(TrackBaseSubSurround(NumBaseSubSurround))
      TrackBaseSubSurround=TempSurfErrorTracking
      DEALLOCATE(TempSurfErrorTracking)
    ELSE
      ALLOCATE(TrackBaseSubSurround(NumBaseSubSurround))
    ENDIF
    TrackBaseSubSurround(NumBaseSubSurround)%SurfIndex1=GRSNR
    TrackBaseSubSurround(NumBaseSubSurround)%SurfIndex2=SBSNR
    TrackBaseSubSurround(NumBaseSubSurround)%MiscIndex=OverLapStatus
!    CALL ShowRecurringWarningErrorAtEnd('Base surface does not surround subsurface (CHKSBS), Overlap Status='//  &
!                       TRIM(cOverLapStatus(OverLapStatus)), &
!                       TrackBaseSubSurround(GRSNR)%ErrIndex1)
!    CALL ShowRecurringContinueErrorAtEnd('Surface "'//TRIM(Surface(GRSNR)%Name)//'" '//TRIM(MSG(OverlapStatus))//  &
!                       ' SubSurface "'//TRIM(Surface(SBSNR)%Name)//'"',  &
!                      TrackBaseSubSurround(SBSNR)%ErrIndex2)

    write(OutputFileShading,AFormat) '==== Base does not Surround subsurface details ===='
    write(OutputFileShading,A4Format) 'Surface=',TRIM(Surface(GRSNR)%Name),' ',TRIM(cOverLapStatus(OverLapStatus))
    write(OutputFileShading,A2I2Format) 'Surface#=',GRSNR,' NSides=',Surface(GRSNR)%Sides
    DO N=1,Surface(GRSNR)%Sides
      write(OutputFileShading,VFormat) 'Vertex ',N,'=',Surface(GRSNR)%Vertex(N)
    ENDDO
    write(OutputFileShading,A4Format) 'SubSurface=',TRIM(Surface(SBSNR)%Name)
    write(OutputFileShading,A2I2Format) 'Surface#=',SBSNR,' NSides=',Surface(SBSNR)%Sides
    DO N=1,Surface(SBSNR)%Sides
      write(OutputFileShading,VFormat) 'Vertex ',N,'=',Surface(SBSNR)%Vertex(N)
    ENDDO
    write(OutputFileShading,AFormat) '================================'
  END IF
ENDIF
  RETURN

contains

function polygon_contains_point ( nsides, polygon_3d, point_3d, ignorex, ignorey, ignorez) result(inside)

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
  Use DataVectorTypes

  Implicit none ! Enforce explicit typing of all variables in this routine

          ! Function argument definitions:
  integer :: nsides      ! number of sides (vertices)
  type(vector) :: polygon_3d(nsides)  ! points of polygon
  type(vector) :: point_3d  ! point to be tested
  logical :: ignorex
  logical :: ignorey
  logical :: ignorez
  logical :: inside  ! return value, true=inside, false = not inside

          ! Function parameter definitions:
  REAL(r64), parameter :: point_tolerance=.00001d0

          ! Interface block specifications:
          ! na

          ! Derived type definitions:
          ! na

          ! Function local variable declarations:
  integer i
  integer ip1
  type(vector_2d) polygon(nsides)
  type(vector_2d) point

  inside = .false.
  if (ignorex) then
    polygon%x=polygon_3d%y
    polygon%y=polygon_3d%z
    point%x=point_3d%y
    point%y=point_3d%z
  elseif (ignorey) then
    polygon%x=polygon_3d%x
    polygon%y=polygon_3d%z
    point%x=point_3d%x
    point%y=point_3d%z
  elseif (ignorez) then
    polygon%x=polygon_3d%x
    polygon%y=polygon_3d%y
    point%x=point_3d%x
    point%y=point_3d%y
  endif

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

end function polygon_contains_point

END SUBROUTINE CHKSBS

SUBROUTINE ComputeIntSolarAbsorpFactors

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN
          !       MODIFIED       B. Griffith, Oct 2010, deal with no floor case
          !                      L. Lawrie, Mar 2012, relax >154 tilt even further (>120 considered non-wall by ASHRAE)
          !       RE-ENGINEERED  Lawrie, Oct 2000

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine computes the fractions of diffusely transmitted
          ! solar energy absorbed by each zone surface.

          ! METHODOLOGY EMPLOYED:
          ! It is assumed that all transmitted solar energy is incident
          ! on the floors of the zone.  The fraction directly absorbed in
          ! the floor is given by 'ISABSF'.  It is proportional to the
          ! area * solar absorptance.  The remaining solar energy is then
          ! distributed uniformly around the room according to
          ! area*absorptance product

          ! REFERENCES:
          ! BLAST/IBLAST code, original author George Walton

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits
  USE DataWindowEquivalentLayer


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER      ::    ConstrNum        ! Index for constructions
  INTEGER      ::    FirstZoneSurf    ! Index of first surface in current zone
  INTEGER      ::    LastZoneSurf     ! Index of last surface in current zone
  REAL(r64)    ::    AreaSum              ! Intermediate calculation value
  INTEGER      ::    SurfNum          ! DO loop counter for zone surfaces
  INTEGER      ::    ZoneNum          ! Loop Counter for Zones
  INTEGER      ::    Lay              ! Window glass layer number
  REAL(r64)    ::    AbsDiffTotWin    ! Sum of a window's glass layer solar absorptances
  REAL(r64)    ::    TestFractSum
  REAL(r64)    ::    HorizAreaSum

          ! FLOW:

  IF (.not. ALLOCATED(ISABSF)) THEN
    ALLOCATE(ISABSF(TotSurfaces))
  ENDIF
  ISABSF=0.0d0

  DO ZoneNum=1,NumOfZones

    FirstZoneSurf=Zone(ZoneNum)%SurfaceFirst
    LastZoneSurf=Zone(ZoneNum)%SurfaceLast
    AreaSum           = 0.0D0

    DO SurfNum = FirstZoneSurf, LastZoneSurf

      IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
        !CR 8229, relaxed from -0.99 to -0.5  (Tilt > 154)
        ! CR8769   !use ASHRAE std of >120, -0.9 to -0.5  (Tilt > 120)
!      IF (Surface(SurfNum)%Class == SurfaceClass_Floor) THEN
!        write(outputfiledebug,*) 'surf=',trim(surface(surfnum)%name),Surface(SurfNum)%CosTilt
!      endif
      IF (Zone(ZoneNum)%OfType == StandardZone .and. Surface(SurfNum)%CosTilt < -0.5d0) &
           AreaSum = AreaSum + Surface(SurfNum)%Area
      !  Next is not implemented but would be:
      ! IF ((Zone(ZoneNum)%OfType .eq. SolarWallZone .or Zone(ZoneNum)%OfType .eq. RoofPondZone) .and.     &
      !      Surface(SurfNum)%ExtBoundCond > 0)    AreaSum = AreaSum + Surface(SurfNum)%Area

    END DO

    HorizAreaSum = AreaSum

    IF ((.NOT. Zone(ZoneNum)%HasFloor) .AND. (HorizAreaSum > 0.d0)) THEN
      !fill floor area even though surfs not called "Floor", they are roughly horizontal and face upwards.
      Zone(ZoneNum)%FloorArea= HorizAreaSum
      CALL ShowWarningError('ComputeIntSolarAbsorpFactors: Solar distribution model is set to place solar gains '//  &
               'on the zone floor,')
      CALL ShowContinueError('...Zone="'//trim(Zone(ZoneNum)%Name)//'" has no floor, but has approximate horizontal surfaces.')
      CALL ShowContinueError('...these Tilt > 120°, (area=['//trim(RoundSigDigits(HorizAreaSum,2))//'] m2) will be used.')
    ENDIF

          ! Compute ISABSF

    DO SurfNum = FirstZoneSurf, LastZoneSurf

      IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE

        ! only horizontal surfaces. !      !CR 8229, relaxed from -0.99 to -0.5  (Tilt > 154)
        ! only horizontal surfaces. !      !CR8769 use ASHRAE std of >120, -0.9 to -0.5  (Tilt > 120)
      IF ( (Zone(ZoneNum)%OfType /= StandardZone .or. Surface(SurfNum)%CosTilt < -0.5d0) .and. &
           (Zone(ZoneNum)%OfType .eq. StandardZone .or. Surface(SurfNum)%ExtBoundCond > 0) ) THEN

        ConstrNum=Surface(SurfNum)%Construction
! last minute V3.1
        IF (Construct(ConstrNum)%TransDiff <= 0.0d0) THEN  !Opaque surface
          IF (AreaSum > 0.0d0) &
            ISABSF(SurfNum) = Surface(SurfNum)%Area*Construct(ConstrNum)%InsideAbsorpSolar/AreaSum
        ELSE  !Window (floor windows are assumed to have no shading device and no divider,
              !and assumed to be non-switchable)
          IF(SurfaceWindow(SurfNum)%StormWinFlag==1) ConstrNum = Surface(SurfNum)%StormWinConstruction
          AbsDiffTotWin = 0.0d0
          IF ( .NOT. Construct(Surface(SurfNum)%Construction)%WindowTypeEQL) THEN
              DO Lay = 1,Construct(ConstrNum)%TotGlassLayers
                AbsDiffTotWin = AbsDiffTotWin + Construct(ConstrNum)%AbsDiffBack(Lay)
              END DO
          ELSE
              DO Lay = 1, CFS(Construct(ConstrNum)%EQLConsPtr)%NL
                AbsDiffTotWin = AbsDiffTotWin + Construct(ConstrNum)%AbsDiffBackEQL(Lay)
              END DO
          ENDIF
          IF (AreaSum > 0.0d0) &
            ISABSF(SurfNum) = Surface(SurfNum)%Area * AbsDiffTotWin / AreaSum
        END IF

      END IF

    END DO

    !CR 8229  test ISABSF for problems
    TestFractSum = SUM(ISABSF(FirstZoneSurf:LastZoneSurf))

    IF ( TestFractSum <= 0.0d0) THEN
      IF (Zone(ZoneNum)%ExtWindowArea > 0.0d0) THEN ! we have a problem, the sun has no floor to go to
        IF (Zone(ZoneNum)%FloorArea <= 0.0d0) THEN
          CALL ShowSevereError('ComputeIntSolarAbsorpFactors: Solar distribution model is set to place solar gains '//  &
               'on the zone floor,')
          CALL ShowContinueError('but Zone ="'//TRIM(Zone(ZoneNum)%Name)//'" does not appear to have any floor surfaces.')
          CALL ShowContinueError('Solar gains will be spread evenly on all surfaces in the zone, and the simulation continues...')
        ELSE ! Floor Area > 0 but still can't absorb
          CALL ShowSevereError('ComputeIntSolarAbsorpFactors: Solar distribution model is set to place solar gains '//  &
               'on the zone floor,')
          CALL ShowContinueError('but Zone ="'//TRIM(Zone(ZoneNum)%Name)//'" floor cannot absorb any solar gains. ')
          CALL ShowContinueError('Check the solar absorptance of the inside layer of the floor surface construction/material.')
          CALL ShowContinueError('Solar gains will be spread evenly on all surfaces in the zone, and the simulation continues...')
        ENDIF

        ! try again but use an even spread across all the surfaces in the zone, regardless of horizontal
        !  so as to not lose solar energy
        AreaSum = 0.0d0
        DO SurfNum = FirstZoneSurf, LastZoneSurf
          IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
          AreaSum = AreaSum + Surface(SurfNum)%Area
        ENDDO

        DO SurfNum = FirstZoneSurf, LastZoneSurf

          IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
          ConstrNum=Surface(SurfNum)%Construction
          IF (Construct(ConstrNum)%TransDiff <= 0.0d0) THEN  !Opaque surface
            IF (AreaSum > 0.0d0) &
              ISABSF(SurfNum) = Surface(SurfNum)%Area*Construct(ConstrNum)%InsideAbsorpSolar/AreaSum
          ELSE  !Window (floor windows are assumed to have no shading device and no divider,
                !and assumed to be non-switchable)
            IF(SurfaceWindow(SurfNum)%StormWinFlag==1) ConstrNum = Surface(SurfNum)%StormWinConstruction
            AbsDiffTotWin = 0.0d0
            IF ( .NOT. Construct(Surface(SurfNum)%Construction)%WindowTypeEQL) THEN
              DO Lay = 1,Construct(ConstrNum)%TotGlassLayers
                AbsDiffTotWin = AbsDiffTotWin + Construct(ConstrNum)%AbsDiffBack(Lay)
              END DO
            ELSE
              DO Lay = 1, CFS(Construct(ConstrNum)%EQLConsPtr)%NL
                AbsDiffTotWin = AbsDiffTotWin + Construct(ConstrNum)%AbsDiffBackEQL(Lay)
              END DO
            ENDIF

            IF (AreaSum > 0.0d0) &
              ISABSF(SurfNum) = Surface(SurfNum)%Area * AbsDiffTotWin / AreaSum
          END IF
        ENDDO
      ENDIF

    ENDIF

  END DO ! zone loop

  RETURN

END SUBROUTINE ComputeIntSolarAbsorpFactors

SUBROUTINE CLIP(NVT,XVT,YVT,ZVT)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN
          !       MODIFIED       na
          !       RE-ENGINEERED  Lawrie, Oct 2000

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine 'clips' the shadow casting surface polygon so that
          ! none of it lies below the plane of the receiving surface polygon.  This
          ! prevents the casting of 'false' shadows.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! BLAST/IBLAST code, original author George Walton

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)               :: NVT
  REAL(r64), INTENT(INOUT), DIMENSION(:) :: XVT
  REAL(r64), INTENT(INOUT), DIMENSION(:) :: YVT
  REAL(r64), INTENT(INOUT), DIMENSION(:) :: ZVT

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER  N            ! Loop Control (vertex number)
  INTEGER  NABOVE       ! Number of vertices of shadow casting surface. above the plane of receiving surface
  INTEGER  NEXT         ! First vertex above plane of receiving surface
  INTEGER  NON          ! Number of vertices of shadow casting surface. on plane of receiving surface
  REAL(r64) XIN  ! X of entry point of shadow casting surface. into plane of receiving surface
  REAL(r64) XOUT ! X of exit point of shadow casting surface. from plane of receiving surface
  REAL(r64) YIN  ! Y of entry point of shadow casting surface. into plane of receiving surface
  REAL(r64) YOUT ! Y of exit point of shadow casting surface. from plane of receiving surface
!  INTEGER NVS      ! Number of vertices of the shadow/clipped surface

  ! Determine if the shadow casting surface. is above, below, or intersects with the plane of the receiving surface

  NABOVE=0
  NON=0
  NumVertInShadowOrClippedSurface = NVS
  DO N = 1, NVT
    IF (ZVT(N) > 0.0d0) NABOVE = NABOVE+1
    IF (ZVT(N) == 0.0d0) NON    = NON+1
  END DO

  IF (NABOVE+NON == NVT) THEN   ! Rename the unclipped shadow casting surface.

    NVS=NVT
    NumVertInShadowOrClippedSurface = NVT
    DO N = 1, NVT
      XVC(N) = XVT(N)
      YVC(N) = YVT(N)
      ZVC(N) = ZVT(N)
    END DO

  ELSEIF (NABOVE == 0) THEN ! Totally submerged shadow casting surface.

    NVS=0
    NumVertInShadowOrClippedSurface = 0

  ELSE  ! Remove (clip) that portion of the shadow casting surface. which is below the receiving surface

    NVS        = NABOVE + 2
    NumVertInShadowOrClippedSurface = NABOVE + 2
    XVT(NVT+1) = XVT(1)
    YVT(NVT+1) = YVT(1)
    ZVT(NVT+1) = ZVT(1)
    DO N = 1, NVT
      IF (ZVT(N) >= 0.0d0 .AND. ZVT(N+1) < 0.0d0) THEN ! Line enters plane of receiving surface

        XIN=(ZVT(N+1)*XVT(N)-ZVT(N)*XVT(N+1))/(ZVT(N+1)-ZVT(N))
        YIN=(ZVT(N+1)*YVT(N)-ZVT(N)*YVT(N+1))/(ZVT(N+1)-ZVT(N))

      ELSEIF (ZVT(N) <= 0.0d0 .AND. ZVT(N+1) > 0.0d0) THEN ! Line exits plane of receiving surface

        NEXT=N+1
        XOUT=(ZVT(N+1)*XVT(N)-ZVT(N)*XVT(N+1))/(ZVT(N+1)-ZVT(N))
        YOUT=(ZVT(N+1)*YVT(N)-ZVT(N)*YVT(N+1))/(ZVT(N+1)-ZVT(N))

      END IF
    END DO

          ! Renumber the vertices of the clipped shadow casting surface. so they are still counter-clockwise sequential.

    XVC(1)   = XOUT
    YVC(1)   = YOUT
    ZVC(1)   = 0.0d0
    XVC(NVS) = XIN
    YVC(NVS) = YIN
    ZVC(NVS) = 0.0d0
    DO N = 1, NABOVE
      IF (NEXT > NVT) NEXT = 1
      XVC(N+1) = XVT(NEXT)
      YVC(N+1) = YVT(NEXT)
      ZVC(N+1) = ZVT(NEXT)
      NEXT     = NEXT+1
    END DO

  END IF

  RETURN

END SUBROUTINE CLIP

SUBROUTINE CTRANS(NS,NGRS,NVT,XVT,YVT,ZVT)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN
          !       MODIFIED       na
          !       RE-ENGINEERED  Lawrie, Oct 2000

          ! PURPOSE OF THIS SUBROUTINE:
          ! Transforms the general coordinates of the vertices
          ! of surface NS to coordinates in the plane of the receiving surface NGRS.
          ! See subroutine 'CalcCoordinateTransformation' SurfaceGeometry Module.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! BLAST/IBLAST code, original author George Walton
          ! NECAP subroutine 'SHADOW'

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)             :: NS     ! Surface number whose vertex coordinates are being transformed
  INTEGER, INTENT(IN)             :: NGRS   ! Base surface number for surface NS
  INTEGER, INTENT(OUT)            :: NVT    ! Number of vertices for surface NS
  REAL(r64), INTENT(OUT), DIMENSION(:) :: XVT    ! XYZ coordinates of vertices of NS in plane of NGRS
  REAL(r64), INTENT(OUT), DIMENSION(:) :: YVT
  REAL(r64), INTENT(OUT), DIMENSION(:) :: ZVT

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER N             ! Loop Control (vertex number)
  REAL(r64) Xdif ! Intermediate Result
  REAL(r64) Ydif ! Intermediate Result
  REAL(r64) Zdif ! Intermediate Result

        ! Perform transformation.

  NVT=Surface(NS)%Sides

  DO N = 1, NVT

    Xdif = Surface(NS)%Vertex(N)%X-X0(NGRS)
    Ydif = Surface(NS)%Vertex(N)%Y-Y0(NGRS)
    Zdif = Surface(NS)%Vertex(N)%Z-Z0(NGRS)

    IF (ABS(Xdif) <= 1.D-15) Xdif=0.0D0
    IF (ABS(Ydif) <= 1.D-15) Ydif=0.0D0
    IF (ABS(Zdif) <= 1.D-15) Zdif=0.0D0

    XVT(N)=Surface(NGRS)%lcsx%x*Xdif+Surface(NGRS)%lcsx%y*Ydif+Surface(NGRS)%lcsx%z*Zdif
    YVT(N)=Surface(NGRS)%lcsy%x*Xdif+Surface(NGRS)%lcsy%y*Ydif+Surface(NGRS)%lcsy%z*Zdif
    ZVT(N)=Surface(NGRS)%lcsz%x*Xdif+Surface(NGRS)%lcsz%y*Ydif+Surface(NGRS)%lcsz%z*Zdif

  END DO

  RETURN

END SUBROUTINE CTRANS

SUBROUTINE HTRANS(I,NS,NumVertices)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN
          !       MODIFIED       na
          !       RE-ENGINEERED  Lawrie, Oct 2000

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sets up the homogeneous coordinates.
          ! This routine converts the cartesian coordinates of a surface
          ! or shadow polygon to homogeneous coordinates.  It also
          ! computes the area of the polygon.

          ! METHODOLOGY EMPLOYED:
          ! Note: Original legacy code used integer arithmetic (tests in subroutines
          ! INCLOS and INTCPT are sensitive to round-off error).  However, porting to Fortran 77
          ! (BLAST/IBLAST) required some variables to become REAL(r64) instead.

          ! Notes on homogeneous coordinates:
          ! A point (X,Y) is represented by a 3-element vector
          ! (W*X,W*Y,W), where W may be any REAL(r64) number except 0.  a line
          ! is also represented by a 3-element vector (A,B,C).  The
          ! directed line (A,B,C) from point (W*X1,W*Y1,W) to point
          ! (V*X2,V*Y2,V) is given by (A,B,C) = (W*X1,W*Y1,W) cross
          ! (V*X2,V*Y2,V).  The sequence of the cross product is a
          ! convention to determine sign.  The condition that a point lie
          ! on a line is that (A,B,C) dot (W*X,W*Y,W) = 0.  'Normalize'
          ! the representation of a point by setting W to 1.  Then if
          ! (A,B,C) dot (X,Y,1) > 0.0, The point is to the left of the
          ! line, and if it is less than zero, the point is to the right
          ! of the line.  The intercept of two lines is given by
          ! (W*X,W*Y,W) = (A1,B1,C1) cross (A2,B2,C3).

          ! REFERENCES:
          ! BLAST/IBLAST code, original author George Walton
          ! W. M. Newman & R. F. Sproull, 'Principles of Interactive Computer Graphics', Appendix II, McGraw-Hill, 1973.
          ! 'CRC Math Tables', 22 ED, 'Analytic Geometry', P.369

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: I   ! Mode selector: 0 - Compute H.C. of sides
                             !                1 - Compute H.C. of vertices & sides
  INTEGER, INTENT(IN) :: NS  ! Figure Number
  INTEGER, INTENT(IN) :: NumVertices ! Number of vertices

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER N              ! Loop Control (vertex number)
  REAL(r64) SUM   ! Sum variable
  INTEGER(i64) ::  ITEMP !

  IF (NS > MaxHCS*2) THEN
    CALL ShowFatalError('Solar Shading: HTrans: Too many Figures (>'//TRIM(TrimSigDigits(MaxHCS))//')')
  ENDIF

  HCNV(NS)=NumVertices

  IF (I /= 0) THEN  ! Transform vertices of figure ns.

    ! See comment at top of module regarding HCMULT
    DO N = 1, NumVertices
      ITEMP     = NINT(XVS(N)*HCMULT,i64)
      HCX(N,NS) = ITEMP
      ITEMP     = NINT(YVS(N)*HCMULT,i64)
      HCY(N,NS) = ITEMP
    END DO

  END IF

          ! Establish extra point for finding lines between points.

  HCX(NumVertices+1,NS)=HCX(1,NS)
  HCY(NumVertices+1,NS)=HCY(1,NS)

          ! Determine lines between points.
  SUM=0.0D0
  DO N = 1, NumVertices
    HCA(N,NS) = HCY(N,NS) - HCY(N+1,NS)
    HCB(N,NS) = HCX(N+1,NS) - HCX(N,NS)
    HCC(N,NS) = (HCY(N+1,NS)*HCX(N,NS)) - (HCX(N+1,NS)*HCY(N,NS))
    SUM = SUM + HCC(N,NS)
  END DO

          ! Compute area of polygon.
!  SUM=0.0D0
!  DO N = 1, NumVertices
!    SUM = SUM + HCX(N,NS)*HCY(N+1,NS) - HCY(N,NS)*HCX(N+1,NS) ! Since HCX and HCY integerized, value of SUM should be ok
!  END DO
  HCAREA(NS)=(0.5d0*SUM)/(sqHCMULT)
!  HCAREA(NS)=0.5d0*SUM*(kHCMULT)


  RETURN

END SUBROUTINE HTRANS

SUBROUTINE HTRANS0(NS,NumVertices)
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
  INTEGER, INTENT(IN) :: NS  ! Figure Number
  INTEGER, INTENT(IN) :: NumVertices ! Number of vertices
  INTEGER N              ! Loop Control (vertex number)
  REAL(r64) SUM   ! Sum variable

  IF (NS > MaxHCS*2) THEN
    CALL ShowFatalError('Solar Shading: HTrans0: Too many Figures (>'//TRIM(TrimSigDigits(MaxHCS))//')')
  ENDIF

  HCNV(NS)=NumVertices
  HCX(NumVertices+1,NS)=HCX(1,NS)
  HCY(NumVertices+1,NS)=HCY(1,NS)

  SUM=0.0D0
  DO N = 1, NumVertices
    HCA(N,NS) = HCY(N,NS) - HCY(N+1,NS)
    HCB(N,NS) = HCX(N+1,NS) - HCX(N,NS)
    HCC(N,NS) = (HCY(N+1,NS)*HCX(N,NS)) - (HCX(N+1,NS)*HCY(N,NS))
    SUM = SUM + HCC(N,NS)
  END DO

  HCAREA(NS)=(0.5d0*SUM)/(sqHCMULT)

  RETURN
END SUBROUTINE HTRANS0

SUBROUTINE HTRANS1(NS,NumVertices)
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine
  INTEGER, INTENT(IN) :: NS  ! Figure Number
  INTEGER, INTENT(IN) :: NumVertices ! Number of vertices
  INTEGER N              ! Loop Control (vertex number)
  REAL(r64) SUM   ! Sum variable
  INTEGER(i64) ::  ITEMP !

  IF (NS > MaxHCS*2) THEN
    CALL ShowFatalError('Solar Shading: HTrans1: Too many Figures (>'//TRIM(TrimSigDigits(MaxHCS))//')')
  ENDIF

  ! only in HTRANS1
  DO N = 1, NumVertices
     ITEMP     = NINT(XVS(N)*HCMULT,i64)
     HCX(N,NS) = ITEMP
     ITEMP     = NINT(YVS(N)*HCMULT,i64)
     HCY(N,NS) = ITEMP
  END DO

  HCNV(NS)=NumVertices
  HCX(NumVertices+1,NS)=HCX(1,NS)
  HCY(NumVertices+1,NS)=HCY(1,NS)

  SUM=0.0D0
  DO N = 1, NumVertices
    HCA(N,NS) = HCY(N,NS) - HCY(N+1,NS)
    HCB(N,NS) = HCX(N+1,NS) - HCX(N,NS)
    HCC(N,NS) = (HCY(N+1,NS)*HCX(N,NS)) - (HCX(N+1,NS)*HCY(N,NS))
    SUM = SUM + HCC(N,NS)
  END DO

  HCAREA(NS)=(0.5d0*SUM)/(sqHCMULT)

  RETURN
END SUBROUTINE HTRANS1

SUBROUTINE INCLOS(N1,N1NumVert,N2,N2NumVert,NumVerticesOverlap,NIN)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN
          !       MODIFIED       na
          !       RE-ENGINEERED  Lawrie, Oct 2000

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine determines which vertices of figure N1 lie within figure N2.

          ! METHODOLOGY EMPLOYED:
          ! For vertex N of figure N1 to lie within figure N2, it must be
          ! on or to the right of all sides of figure N2, assuming
          ! figure N2 is convex.

          ! REFERENCES:
          ! BLAST/IBLAST code, original author George Walton

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: N1         ! Figure number of figure 1
  INTEGER, INTENT(IN)    :: N1NumVert  ! Number of vertices of figure 1
  INTEGER, INTENT(IN)    :: N2         ! Figure number of figure 2
  INTEGER, INTENT(IN)    :: N2NumVert  ! Number of vertices of figure 2
  INTEGER, INTENT(INOUT) :: NumVerticesOverlap ! Number of vertices which overlap
  INTEGER, INTENT(OUT)   :: NIN        ! Number of vertices of figure 1 within figure 2

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER K  ! Vertex number of the overlap
  INTEGER M  ! Side number of figure N2
  INTEGER N  ! Vertex number of figure N1
  LOGICAL CycleMainLoop ! Sets when to cycle main loop
  REAL(r64) HFunct

  NIN=0

  DO N = 1, N1NumVert

    CycleMainLoop = .FALSE.

          ! Eliminate cases where vertex N is to the left of side M.

    DO M = 1, N2NumVert
      HFunct=HCX(N,N1)*HCA(M,N2)+HCY(N,N1)*HCB(M,N2)+HCC(M,N2)
      IF (HFunct > 0.0D0 ) THEN
        CycleMainLoop = .TRUE.  ! Set to cycle to the next value of N
        EXIT ! M DO loop
      END IF
    END DO

    IF (CycleMainLoop) CYCLE
    NIN=NIN+1

          ! Check for duplication of previously determined points.

    IF (NumVerticesOverlap /= 0) THEN
      DO K = 1, NumVerticesOverlap
        IF ((XTEMP(K) == HCX(N,N1)).AND.(YTEMP(K) == HCY(N,N1))) THEN
          CycleMainLoop = .TRUE.    ! Set to cycle to the next value of N
          EXIT ! K DO loop
        END IF
      END DO
      IF (CycleMainLoop) CYCLE
    END IF

          ! Record enclosed vertices in temporary arrays.

   NumVerticesOverlap        = NumVerticesOverlap + 1
   XTEMP(NumVerticesOverlap) = HCX(N,N1)
   YTEMP(NumVerticesOverlap) = HCY(N,N1)

  END DO

  RETURN

END SUBROUTINE INCLOS

SUBROUTINE INTCPT(NV1,NV2,NV3,NS1,NS2)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN
          !       MODIFIED       na
          !       RE-ENGINEERED  Lawrie, Oct 2000

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine determines all intercepts between the sides of figure NS1
          ! and the sides of figure NS2.

          ! METHODOLOGY EMPLOYED:
          ! The requirements for intersection are that the end points of
          ! line N lie on both sides of line M and vice versa.  Also
          ! eliminate cases where the end point of one line lies exactly
          ! on the other to reduce duplication with the enclosed points.

          ! REFERENCES:
          ! BLAST/IBLAST code, original author George Walton

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: NV1      ! Number of vertices of figure NS1
  INTEGER, INTENT(IN)    :: NV2      ! Number of vertices of figure NS2
  INTEGER, INTENT(INOUT) :: NV3      ! Number of vertices of figure NS3
  INTEGER, INTENT(IN)    :: NS1      ! Number of the figure being overlapped
  INTEGER, INTENT(IN)    :: NS2      ! Number of the figure doing overlapping

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) W        ! Normalization factor
  REAL(r64) XUntrunc ! Untruncated X coordinate
  REAL(r64) YUntrunc ! Untruncated Y coordinate
  INTEGER(i64) :: I1       ! Intermediate result for testing intersection
  INTEGER(i64) :: I2       ! Intermediate result for testing intersection
  INTEGER K                 !
  INTEGER KK                !
  INTEGER M                 ! Side number of figure NS2
  INTEGER N                 ! Side number of figure NS1
  REAL(r64), ALLOCATABLE, DIMENSION(:)      :: XTEMP1  ! Temporary 'X' values for HC vertices of the overlap
  REAL(r64), ALLOCATABLE, DIMENSION(:)      :: YTEMP1  ! Temporary 'Y' values for HC vertices of the overlap

  DO N = 1, NV1
    DO M = 1, NV2

          ! Eliminate cases where sides N and M do not intersect.

      I1 = HCA(N,NS1)*HCX(M,NS2)   + HCB(N,NS1)*HCY(M,NS2)   + HCC(N,NS1)
      I2 = HCA(N,NS1)*HCX(M+1,NS2) + HCB(N,NS1)*HCY(M+1,NS2) + HCC(N,NS1)
      IF (I1 >= 0 .AND. I2 >= 0) CYCLE
      IF (I1 <= 0 .AND. I2 <= 0) CYCLE

      I1 = HCA(M,NS2)*HCX(N,NS1)   + HCB(M,NS2)*HCY(N,NS1)   + HCC(M,NS2)
      I2 = HCA(M,NS2)*HCX(N+1,NS1) + HCB(M,NS2)*HCY(N+1,NS1) + HCC(M,NS2)
      IF (I1 >= 0 .AND. I2 >= 0) CYCLE
      IF (I1 <= 0 .AND. I2 <= 0) CYCLE

          ! Determine the point of intersection and record in the temporary array.

      KK         = NV3
      NV3        = NV3 + 1
      W          = HCB(M,NS2)*HCA(N,NS1) - HCA(M,NS2)*HCB(N,NS1)
      XUntrunc   = (HCC(M,NS2)*HCB(N,NS1)-HCB(M,NS2)*HCC(N,NS1))/W
      YUntrunc   = (HCA(M,NS2)*HCC(N,NS1)-HCC(M,NS2)*HCA(N,NS1))/W
      IF (NV3 > SIZE(XTEMP)) THEN
!        write(outputfiledebug,*) 'nv3=',nv3,' size(xtemp)=',size(xtemp)
        ALLOCATE(XTEMP1(SIZE(XTEMP)+10))
        ALLOCATE(YTEMP1(SIZE(YTEMP)+10))
        XTEMP1=0.0d0
        YTEMP1=0.0d0
        XTEMP1(1:NV3-1)=XTEMP(1:NV3-1)
        YTEMP1(1:NV3-1)=YTEMP(1:NV3-1)
        DEALLOCATE(XTEMP)
        DEALLOCATE(YTEMP)
        ALLOCATE(XTEMP(SIZE(XTEMP1)))
        ALLOCATE(YTEMP(SIZE(YTEMP1)))
        XTEMP=XTEMP1
        YTEMP=YTEMP1
        DEALLOCATE(XTEMP1)
        DEALLOCATE(YTEMP1)
      ENDIF
      XTEMP(NV3) = NINT(XUntrunc,i64)
      YTEMP(NV3) = NINT(YUntrunc,i64)

          ! Eliminate near-duplicate points.

      IF (KK /= 0) THEN
        DO K = 1, KK
          IF (ABS(XTEMP(NV3)-XTEMP(K)) > 2.0d0) CYCLE
          IF (ABS(YTEMP(NV3)-YTEMP(K)) > 2.0d0) CYCLE
          NV3 = KK
          EXIT ! K DO loop
        END DO
      END IF

    END DO

  END DO

  RETURN

END SUBROUTINE INTCPT

SUBROUTINE CLIPPOLY(NS1,NS2,NV1,NV2,NV3)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Tyler Hoyt
          !       DATE WRITTEN   May 4, 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Populate global arrays XTEMP and YTEMP with the vertices
          ! of the overlap between NS1 and NS2, and determine relevant
          ! overlap status.

          ! METHODOLOGY EMPLOYED:
          ! The Sutherland-Hodgman algorithm for polygon clipping is employed.

          ! METHODOLOGY EMPLOYED:
          !

          ! REFERENCES:
          !

          ! USE STATEMENTS:
  USE General, ONLY: ReallocateRealArray,SafeDivide

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)      :: NS1     ! Figure number of figure 1 (The subject polygon)
  INTEGER, INTENT(IN)      :: NS2     ! Figure number of figure 2 (The clipping polygon)
  INTEGER, INTENT(IN)      :: NV1     ! Number of vertices of figure 1
  INTEGER, INTENT(IN)      :: NV2     ! Number of vertices of figure 2
  INTEGER, INTENT(INOUT)   :: NV3     ! Number of vertices of figure 3

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL INTFLAG    ! For overlap status
  INTEGER E          ! Edge loop index
  INTEGER P          ! Point loop index
  INTEGER S          ! Test vertex
  INTEGER K          ! Duplicate test index
  INTEGER KK         ! Duplicate test index
  INTEGER NVOUT      ! Current output length for loops
  INTEGER NVTEMP
  INTEGER SaveArrayBounds

  REAL(r64) W        ! Normalization factor
  REAL(r64) HFunct
!  REAL(r64), DIMENSION(2*(MaxVerticesPerSurface + 1))  :: ATEMP    ! Temporary 'A' values for HC vertices of the overlap
!  REAL(r64), DIMENSION(2*(MaxVerticesPerSurface + 1))  :: BTEMP    ! Temporary 'B' values for HC vertices of the overlap
!  REAL(r64), DIMENSION(2*(MaxVerticesPerSurface + 1))  :: CTEMP    ! Temporary 'C' values for HC vertices of the overlap
!  REAL(r64), DIMENSION(2*(MaxVerticesPerSurface + 1))  :: XTEMP1   ! Temporary 'X' values for HC vertices of the overlap
!  REAL(r64), DIMENSION(2*(MaxVerticesPerSurface + 1))  :: YTEMP1   ! Temporary 'Y' values for HC vertices of the overlap

#ifdef EP_Count_Calls
  NumClipPoly_Calls=NumClipPoly_Calls+1
#endif
  ! Populate the arrays with the original polygon
  DO P=1, NV1
     XTEMP(P) = HCX(P,NS1)
     YTEMP(P) = HCY(P,NS1)
     ATEMP(P) = HCA(P,NS1)
     BTEMP(P) = HCB(P,NS1)
     CTEMP(P) = HCC(P,NS1)
  END DO

  NVOUT   = NV1 ! First point-loop is the length of the subject polygon.
  INTFLAG = .false.
  NVTEMP   = 0
  KK       = 0

  DO E=1, NV2  ! Loop over edges of the clipping polygon
    DO P=1, NVOUT
        XTEMP1(P) = XTEMP(P)
        YTEMP1(P) = YTEMP(P)
    END DO
    S = NVOUT
    DO P=1, NVOUT
      HFunct=XTEMP1(P)*HCA(E,NS2)+YTEMP1(P)*HCB(E,NS2)+HCC(E,NS2)
      IF (HFunct <= 0.0D0 ) THEN          ! Vertex is not in the clipping plane
        HFunct=XTEMP1(S)*HCA(E,NS2)+YTEMP1(S)*HCB(E,NS2)+HCC(E,NS2)
        IF (HFunct > 0.0D0 ) THEN        ! Test vertex is in the clipping plane

          ! Find/store the intersection of the clip edge and the line connecting S and P
          KK            = NVTEMP
          NVTEMP        = NVTEMP + 1
          W             = HCB(E,NS2)*ATEMP(S)-HCA(E,NS2)*BTEMP(S)
          IF (W /= 0.0d0) THEN
            XTEMP(NVTEMP) = NINT((HCC(E,NS2)*BTEMP(S)-HCB(E,NS2)*CTEMP(S))/W,i64)
            YTEMP(NVTEMP) = NINT((HCA(E,NS2)*CTEMP(S)-HCC(E,NS2)*ATEMP(S))/W,i64)
          ELSE
            XTEMP(NVTEMP) = SafeDivide((HCC(E,NS2)*BTEMP(S)-HCB(E,NS2)*CTEMP(S)),W)
            YTEMP(NVTEMP) = SafeDivide((HCA(E,NS2)*CTEMP(S)-HCC(E,NS2)*ATEMP(S)),W)
          ENDIF
          INTFLAG = .true.

          IF(E==NV2) THEN       ! Remove near-duplicates on last edge
            IF (KK /= 0) THEN
              DO K = 1, KK
                IF (ABS(XTEMP(NVTEMP)-XTEMP(K)) > 2.0d0) CYCLE
                IF (ABS(YTEMP(NVTEMP)-YTEMP(K)) > 2.0d0) CYCLE
                NVTEMP = KK
                EXIT ! K DO loop
              END DO
            END IF
          END IF

        END IF

        KK            = NVTEMP
        NVTEMP        = NVTEMP + 1
        IF (NVTEMP > MAXHCArrayBounds) THEN
          SaveArrayBounds=MAXHCArrayBounds
          CALL ReallocateRealArray(XTEMP,SaveArrayBounds,MAXHCArrayIncrement)
          SaveArrayBounds=MAXHCArrayBounds
          CALL ReallocateRealArray(YTEMP,SaveArrayBounds,MAXHCArrayIncrement)
          SaveArrayBounds=MAXHCArrayBounds
          CALL ReallocateRealArray(XTEMP1,SaveArrayBounds,MAXHCArrayIncrement)
          SaveArrayBounds=MAXHCArrayBounds
          CALL ReallocateRealArray(YTEMP1,SaveArrayBounds,MAXHCArrayIncrement)
          SaveArrayBounds=MAXHCArrayBounds
          CALL ReallocateRealArray(ATEMP,SaveArrayBounds,MAXHCArrayIncrement)
          SaveArrayBounds=MAXHCArrayBounds
          CALL ReallocateRealArray(BTEMP,SaveArrayBounds,MAXHCArrayIncrement)
          SaveArrayBounds=MAXHCArrayBounds
          CALL ReallocateRealArray(CTEMP,SaveArrayBounds,MAXHCArrayIncrement)
          MAXHCArrayBounds=SaveArrayBounds
        ENDIF

        XTEMP(NVTEMP) = XTEMP1(P)
        YTEMP(NVTEMP) = YTEMP1(P)

        IF(E==NV2) THEN            ! Remove near-duplicates on last edge
          IF (KK /= 0) THEN
            DO K = 1, KK
              IF (ABS(XTEMP(NVTEMP)-XTEMP(K)) > 2.0d0) CYCLE
              IF (ABS(YTEMP(NVTEMP)-YTEMP(K)) > 2.0d0) CYCLE
              NVTEMP = KK
              EXIT ! K DO loop
            END DO
          END IF
        END IF

        ELSE
          HFunct=XTEMP1(S)*HCA(E,NS2)+YTEMP1(S)*HCB(E,NS2)+HCC(E,NS2)
          IF (HFunct <= 0.0D0) THEN      ! Test vertex is not in the clipping plane

            KK            = NVTEMP
            NVTEMP        = NVTEMP + 1
            W             = HCB(E,NS2)*ATEMP(S)-HCA(E,NS2)*BTEMP(S)
            IF (W /= 0.0d0) THEN
              XTEMP(NVTEMP) = NINT((HCC(E,NS2)*BTEMP(S)-HCB(E,NS2)*CTEMP(S))/W,i64)
              YTEMP(NVTEMP) = NINT((HCA(E,NS2)*CTEMP(S)-HCC(E,NS2)*ATEMP(S))/W,i64)
            ELSE
              XTEMP(NVTEMP) = SafeDivide((HCC(E,NS2)*BTEMP(S)-HCB(E,NS2)*CTEMP(S)),W)
              YTEMP(NVTEMP) = SafeDivide((HCA(E,NS2)*CTEMP(S)-HCC(E,NS2)*ATEMP(S)),W)
            ENDIF
            INTFLAG = .true.

            IF(E==NV2) THEN         ! Remove near-duplicates on last edge
              IF (KK /= 0) THEN
                DO K = 1, KK
                  IF (ABS(XTEMP(NVTEMP)-XTEMP(K)) > 2.0d0) CYCLE
                  IF (ABS(YTEMP(NVTEMP)-YTEMP(K)) > 2.0d0) CYCLE
                  NVTEMP = KK
                  EXIT ! K DO loop
                END DO
              END IF
            END IF

          END IF
        END IF
        S = P
      END DO  ! end loop over points of subject polygon

      NVOUT  = NVTEMP
      NVTEMP = 0

      IF (E /= NV2) THEN
        IF (NVOUT > 2) THEN ! Compute HC values for edges of output polygon
          DO P=1, NVOUT-1
            ATEMP(P) = YTEMP(P)-YTEMP(P+1)
            BTEMP(P) = XTEMP(P+1)-XTEMP(P)
            CTEMP(P) = XTEMP(P)*YTEMP(P+1)-YTEMP(P)*XTEMP(P+1)
          END DO
          ATEMP(NVOUT) = YTEMP(NVOUT)-YTEMP(1)
          BTEMP(NVOUT) = XTEMP(1)-XTEMP(NVOUT)
          CTEMP(NVOUT) = XTEMP(NVOUT)*YTEMP(1)-YTEMP(NVOUT)*XTEMP(1)
        END IF
      END IF

    END DO  ! end loop over edges in NS2

    NV3 = NVOUT

  IF(NV3 < 3) THEN     ! Determine overlap status
    OverlapStatus = NoOverlap
  ELSE IF(.not. INTFLAG) THEN
    OverlapStatus = FirstSurfWithinSecond
  END IF

END SUBROUTINE CLIPPOLY


SUBROUTINE MULTOL(NNN,LOC0,NRFIGS)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN
          !       MODIFIED       na
          !       RE-ENGINEERED  Lawrie, Oct 2000

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine determines the overlaps of figure 'NS2' with previous figures
          ! 'LOC0+1' through 'LOC0+NRFIGS'.  For example, if NS2
          ! is a shadow, overlap with previous shadows.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! BLAST/IBLAST code, original author George Walton

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: NNN    ! argument
  INTEGER, INTENT(IN) :: LOC0   ! Location in the homogeneous coordinate array
                                ! first figure overlapped (minus 1)
  INTEGER, INTENT(IN) :: NRFIGS ! Number of figures overlapped

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER  I  ! Loop Control
  INTEGER NS1 ! Number of the figure being overlapped
  INTEGER NS2 ! Number of the figure doing overlapping
  INTEGER NS3 ! Location to place results of overlap

  maxNumberOfFigures=MAX(maxNumberOfFigures,NRFIGS)

  NS2=NNN
  DO I = 1, NRFIGS
    NS1=LOC0+I
    NS3=LOCHCA+1

    CALL DeterminePolygonOverlap(NS1,NS2,NS3) ! Find overlap of figure NS2 on figure NS1.

          ! Process overlap cases:

    IF (OverlapStatus == NoOverlap) CYCLE

    IF ( (OverlapStatus == TooManyVertices).OR. &
         (OverlapStatus == TooManyFigures) ) EXIT

    LOCHCA=NS3  ! Increment h.c. arrays pointer.

  END DO

  RETURN

END SUBROUTINE MULTOL

SUBROUTINE ORDER(NV3,NS3)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN
          !       MODIFIED       na
          !       RE-ENGINEERED  Lawrie, Oct 2000

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine sorts the vertices found by inclosure and
          ! intercept in to clockwise order so that the overlap polygon
          ! may be used in computing subsequent overlaps.

          ! METHODOLOGY EMPLOYED:
          ! The slopes of the lines from the left-most vertex to all
          ! others are found.  The slopes are sorted into descending
          ! sequence.  This sequence puts the vertices in clockwise order.

          ! REFERENCES:
          ! BLAST/IBLAST code, original author George Walton

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: NV3  ! Number of vertices of figure NS3
  INTEGER, INTENT(IN) :: NS3  ! Location to place results of overlap

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64), ALLOCATABLE, SAVE, DIMENSION(:) :: SLOPE  ! Slopes from left-most vertex to others
  REAL(r64) DELTAX ! Difference between X coordinates of two vertices
  REAL(r64) DELTAY ! Difference between Y coordinates of two vertices
  REAL(r64) SAVES  ! Temporary location for exchange of variables
  REAL(r64) SAVEX  ! Temporary location for exchange of variables
  REAL(r64) SAVEY  ! Temporary location for exchange of variables
  REAL(r64) XMIN   ! X coordinate of left-most vertex
  REAL(r64) YXMIN  !
  INTEGER I      ! Sort index
  INTEGER IM1    ! Sort control
  INTEGER J      ! Sort index
  INTEGER M      ! Number of slopes to be sorted
  INTEGER N      ! Vertex number
  INTEGER P      ! Location of first slope to be sorted
  LOGICAL, SAVE :: FirstTimeFlag=.true.

  IF (FirstTimeFlag) THEN
    ALLOCATE(SLOPE(MAX(10,MaxVerticesPerSurface+1)))
    FirstTimeFlag=.false.
  ENDIF
          ! Determine left-most vertex.

  XMIN  = XTEMP(1)
  YXMIN = YTEMP(1)
  DO N = 2, NV3
    IF (XTEMP(N) >= XMIN) CYCLE
    XMIN  = XTEMP(N)
    YXMIN = YTEMP(N)
  END DO

          ! Determine slopes from left-most vertex to all others.  Identify
          ! first and second or last points as they occur.

  P = 1
  M = 0
  DO N = 1, NV3

    DELTAX = XTEMP(N) - XMIN
    DELTAY = YTEMP(N) - YXMIN

    IF (ABS(DELTAX) > 0.5d0) THEN

      M        = M + 1
      SLOPE(M) = DELTAY/DELTAX
      XTEMP(M) = XTEMP(N)
      YTEMP(M) = YTEMP(N)

    ELSEIF (DELTAY > 0.5d0) THEN

      P          = 2
      HCX(2,NS3) = NINT(XTEMP(N),i64)
      HCY(2,NS3) = NINT(YTEMP(N),i64)

    ELSEIF (DELTAY < -0.5d0) THEN

      HCX(NV3,NS3) = NINT(XTEMP(N),i64)
      HCY(NV3,NS3) = NINT(YTEMP(N),i64)

    ELSE

      HCX(1,NS3) = NINT(XMIN,i64)
      HCY(1,NS3) = NINT(YXMIN,i64)

    END IF

  END DO

  ! Sequence the temporary arrays in order of decreasing slopes.(bubble sort)

  IF (M /= 1) THEN

    DO I = 2, M
      IM1 = I - 1
      DO J = 1, IM1
        IF (SLOPE(I) <= SLOPE(J)) CYCLE
        SAVEX    = XTEMP(I)
        SAVEY    = YTEMP(I)
        SAVES    = SLOPE(I)
        XTEMP(I) = XTEMP(J)
        YTEMP(I) = YTEMP(J)
        SLOPE(I) = SLOPE(J)
        XTEMP(J) = SAVEX
        YTEMP(J) = SAVEY
        SLOPE(J) = SAVES
      END DO
    END DO

  END IF

          ! Place sequenced points in the homogeneous coordinate arrays.

  DO N = 1, M
    HCX(N+P,NS3)=NINT(XTEMP(N),i64)
    HCY(N+P,NS3)=NINT(YTEMP(N),i64)
  END DO

  RETURN

END SUBROUTINE ORDER

SUBROUTINE DeterminePolygonOverlap(NS1,NS2,NS3)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN
          !       MODIFIED       na
          !       RE-ENGINEERED  Lawrie, Oct 2000

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine computes the possible overlap of two polygons.
          ! It uses homogeneous coordinate techniques to determine the overlap area
          ! between two convex polygons.  Results are stored in the homogeneous coordinate (HC) arrays.

          ! METHODOLOGY EMPLOYED:
          ! The vertices defining the overlap between fig.1 and fig.2
          ! consist of: the vertices of fig.1 enclosed by fig.2 (A)
          ! plus the vertices of fig.2 enclosed by fig.1 (B)
          ! plus the intercepts of fig.1 and fig.2 (C & D)

          !                               +----------------------+
          !                               !                      !
          !                               !         FIG.2        !
          !                               !                      !
          !                +--------------C----------A           !
          !                !              !         /            !
          !                !              !        /             !
          !                !              B-------D--------------+
          !                !    FIG.1            /
          !                !                    /
          !                +-------------------+

          ! REFERENCES:
          ! BLAST/IBLAST code, original author George Walton

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits
  USE DataSystemVariables, ONLY: SutherlandHodgman

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: NS1  ! Number of the figure being overlapped
  INTEGER, INTENT(IN) :: NS2  ! Number of the figure doing overlapping
  INTEGER, INTENT(IN) :: NS3  ! Location to place results of overlap

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER N  ! Loop index
  INTEGER NV1      ! Number of vertices of figure NS1
  INTEGER NV2      ! Number of vertices of figure NS2
  INTEGER NV3      ! Number of vertices of figure NS3 (the overlap of NS1 and NS2)
  INTEGER NIN1     ! Number of vertices of NS1 within NS2
  INTEGER NIN2     ! Number of vertices of NS2 within NS1
  LOGICAL, SAVE :: TooManyFiguresMessage=.false.
  LOGICAL, SAVE :: TooManyVerticesMessage=.false.

          ! Check for exceeding array limits.
#ifdef EP_Count_Calls
NumDetPolyOverlap_Calls=NumDetPolyOverlap_Calls+1
#endif

  IF (NS3 > MAXHCS) THEN

    OverlapStatus = TooManyFigures

    IF (.not. TooManyFiguresMessage .and. .not. DisplayExtraWarnings) THEN
      CALL ShowWarningError('DeterminePolygonOverlap: Too many figures [>'//  &
           TRIM(RoundSigDigits(MaxHCS))//']  detected in an overlap calculation.'//  &
           ' Use Output:Diagnostics,DisplayExtraWarnings; for more details.')
      TooManyFiguresMessage=.true.
    ENDIF

    IF (DisplayExtraWarnings) THEN
      NumTooManyFigures=NumTooManyFigures+1
      IF (NumTooManyFigures > 1) THEN
        ALLOCATE(TempSurfErrorTracking(NumTooManyFigures))
        TempSurfErrorTracking(1:NumTooManyFigures-1)=TrackTooManyFigures
        DEALLOCATE(TrackTooManyFigures)
        ALLOCATE(TrackTooManyFigures(NumTooManyFigures))
        TrackTooManyFigures=TempSurfErrorTracking
        DEALLOCATE(TempSurfErrorTracking)
      ELSE
        ALLOCATE(TrackTooManyFigures(NumTooManyFigures))
      ENDIF
      TrackTooManyFigures(NumTooManyFigures)%SurfIndex1=CurrentShadowingSurface
      TrackTooManyFigures(NumTooManyFigures)%SurfIndex2=CurrentSurfaceBeingShadowed
    ENDIF

    RETURN

  ENDIF

  OverlapStatus = PartialOverlap
  NV1           = HCNV(NS1)
  NV2           = HCNV(NS2)
  NV3           = 0


  IF (.not. SutherlandHodgman) THEN
    CALL INCLOS(NS1,NV1,NS2,NV2,NV3,NIN1) ! Find vertices of NS1 within NS2.

    IF (NIN1 >= NV1) THEN

      OverlapStatus = FirstSurfWithinSecond

    ELSE

      CALL INCLOS(NS2,NV2,NS1,NV1,NV3,NIN2)   ! Find vertices of NS2 within NS1.

      IF (NIN2 >= NV2) THEN

        OverlapStatus = SecondSurfWithinFirst

      ELSE

        CALL INTCPT(NV1,NV2,NV3,NS1,NS2)   ! Find intercepts of NS1 & NS2.

        IF (NV3 < 3) THEN    ! Overlap must have 3 or more vertices
          OverlapStatus = NoOverlap
          RETURN
        END IF

      END IF

    END IF

  ELSE
    ! simple polygon clipping
    CALL CLIPPOLY(NS1,NS2,NV1,NV2,NV3)
  ENDIF

  IF (NV3 < MAXHCV .and. NS3 <= MAXHCS) THEN

    IF (.not. SutherlandHodgman) THEN
      CALL ORDER(NV3,NS3)  ! Put vertices in clockwise order.
    ELSE
      DO N = 1, NV3
        HCX(N,NS3)=NINT(XTEMP(N),i64)
        HCY(N,NS3)=NINT(YTEMP(N),i64)
      END DO
    ENDIF

    CALL HTRANS0(NS3,NV3)  ! Determine h.c. values of sides.
        ! Skip overlaps of negligible area.

    IF (ABS(HCAREA(NS3))*HCMULT < ABS(HCAREA(NS1))) THEN
      OverlapStatus = NoOverlap
    ELSE
      IF (HCAREA(NS1)*HCAREA(NS2) > 0.0d0) HCAREA(NS3) = -HCAREA(NS3)  ! Determine sign of area of overlap
      HCT(NS3) = HCT(NS2)*HCT(NS1)  ! Determine transmission of overlap
      if (HCT(NS2) /= 1.0d0 .and. HCT(NS2) /= 0.0d0 .and. HCT(NS1) /= 1.0d0 .and. HCT(NS1) /= 0.0d0) then
        if (HCT(NS2) >= .5d0 .and. HCT(NS1) >= .5d0) then
          HCT(NS3)=1.0d0-HCT(NS3)
        endif
      endif
    END IF

  ELSEIF (NV3 > MAXHCV) THEN

    OverlapStatus = TooManyVertices

    IF (.not. TooManyVerticesMessage .and. .not. DisplayExtraWarnings) THEN
      CALL ShowWarningError('DeterminePolygonOverlap: Too many vertices [>'//  &
           TRIM(RoundSigDigits(MaxHCV))//'] detected in an overlap calculation.'//  &
           ' Use Output:Diagnostics,DisplayExtraWarnings; for more details.')
      TooManyVerticesMessage=.true.
    ENDIF

    IF (DisplayExtraWarnings) THEN
      NumTooManyVertices=NumTooManyVertices+1
      IF (NumTooManyVertices > 1) THEN
        ALLOCATE(TempSurfErrorTracking(NumTooManyVertices))
        TempSurfErrorTracking(1:NumTooManyVertices-1)=TrackTooManyVertices
        DEALLOCATE(TrackTooManyVertices)
        ALLOCATE(TrackTooManyVertices(NumTooManyVertices))
        TrackTooManyVertices=TempSurfErrorTracking
        DEALLOCATE(TempSurfErrorTracking)
      ELSE
        ALLOCATE(TrackTooManyVertices(NumTooManyVertices))
      ENDIF
      TrackTooManyVertices(NumTooManyVertices)%SurfIndex1=CurrentShadowingSurface
      TrackTooManyVertices(NumTooManyVertices)%SurfIndex2=CurrentSurfaceBeingShadowed
    ENDIF

  ELSEIF (NS3 > MAXHCS) THEN

    OverlapStatus = TooManyFigures

    IF (.not. TooManyFiguresMessage .and. .not. DisplayExtraWarnings) THEN
      CALL ShowWarningError('DeterminePolygonOverlap: Too many figures [>'//  &
           TRIM(RoundSigDigits(MaxHCS))//']  detected in an overlap calculation.'//  &
           ' Use Output:Diagnostics,DisplayExtraWarnings; for more details.')
      TooManyFiguresMessage=.true.
    ENDIF

    IF (DisplayExtraWarnings) THEN
      NumTooManyFigures=NumTooManyFigures+1
      IF (NumTooManyFigures > 1) THEN
        ALLOCATE(TempSurfErrorTracking(NumTooManyFigures))
        TempSurfErrorTracking(1:NumTooManyFigures-1)=TrackTooManyFigures
        DEALLOCATE(TrackTooManyFigures)
        ALLOCATE(TrackTooManyFigures(NumTooManyFigures))
        TrackTooManyFigures=TempSurfErrorTracking
        DEALLOCATE(TempSurfErrorTracking)
      ELSE
        ALLOCATE(TrackTooManyFigures(NumTooManyFigures))
      ENDIF
      TrackTooManyFigures(NumTooManyFigures)%SurfIndex1=CurrentShadowingSurface
      TrackTooManyFigures(NumTooManyFigures)%SurfIndex2=CurrentSurfaceBeingShadowed
    ENDIF

  ENDIF

  RETURN

END SUBROUTINE DeterminePolygonOverlap

SUBROUTINE CalcPerSolarBeam(AvgEqOfTime,AvgSinSolarDeclin,AvgCosSolarDeclin)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN
          !       MODIFIED       BG, Nov 2012 - Timestep solar.  DetailedSolarTimestepIntegration
          !       RE-ENGINEERED  Lawrie, Oct 2000

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine manages computation of solar gain multipliers for beam radiation.  These
          ! are calculated for a period of days depending on the input "Shadowing Calculations".

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! BLAST/IBLAST code, original author George Walton

          ! USE STATEMENTS:
  USE WindowComplexManager,ONLY: InitComplexWindows, UpdateComplexWindows
  USE DataSystemVariables, ONLY: DetailedSkyDiffuseAlgorithm, DetailedSolarTimestepIntegration
  USE DataGlobals,         ONLY: TimeStepZone, HourOfDay, TimeStep

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: AvgSinSolarDeclin ! Average value of Sine of Solar Declination for period
  REAL(r64), INTENT(IN) :: AvgCosSolarDeclin ! Average value of Cosine of Solar Declination for period
  REAL(r64), INTENT(IN) :: AvgEqOfTime       ! Average value of Equation of Time for period

          ! SUBROUTINE PARAMETER DEFINITIONS:



          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER IHOUR   ! Hour index number
  INTEGER TS     ! TimeStep Loop Counter
  LOGICAL, SAVE :: Once=.true.

  IF (Once) CALL InitComplexWindows
  Once=.false.

  IF (KickOffSizing .or. KickOffSimulation) RETURN  ! Skip solar calcs for these Initialization steps.

#ifdef EP_Count_Calls
  NumCalcPerSolBeam_Calls=NumCalcPerSolBeam_Calls+1
#endif

  ! Intialize some values for the appropriate period
  IF (.NOT. DetailedSolarTimestepIntegration) THEN
    SunLitFracHR=0.0d0
    SunLitFrac=0.0d0
    SunLitFracWithoutReveal=0.0d0
    CTHETA=0.0d0
    CosIncAngHR=0.0d0
    CosIncAng=0.0d0
    AOSurf=0.0d0
    BackSurfaces=0
    OverlapAreas=0.0d0
    DO IHOUR = 1,24
      SurfaceWindow%OutProjSLFracMult(IHOUR)   = 1.d0
      SurfaceWindow%InOutProjSLFracMult(IHOUR) = 1.d0
    END DO

  ELSE
    SunLitFracHR(1:TotSurfaces,HourOfDay)                      = 0.d0
    SunLitFrac(1:TotSurfaces,HourOfDay,TimeStep)               = 0.d0
    SunLitFracWithoutReveal(1:TotSurfaces,HourOfDay,TimeStep)  = 0.d0
    CTHETA(1:TotSurfaces)                                      = 0.d0
    CosIncAngHR(1:TotSurfaces,HourOfDay)                       = 0.d0
    CosIncAng(1:TotSurfaces,HourOfDay,TimeStep)                = 0.d0
    AOSurf(1:TotSurfaces)                                      = 0.d0
    BackSurfaces(1:TotSurfaces,1:MaxBkSurf,HourOfDay,TimeStep) = 0
    OverlapAreas(1:TotSurfaces,1:MaxBkSurf,HourOfDay,TimeStep) = 0.d0
    SurfaceWindow%OutProjSLFracMult(HourOfDay)                =1.0d0
    SurfaceWindow%InOutProjSLFracMult(HourOfDay)              =1.0d0
  ENDIF

  IF (.NOT. DetailedSolarTimestepIntegration) THEN
    DO IHOUR = 1, 24  ! Do for all hours.
      DO TS=1,NumOfTimeStepInHour
        CALL FigureSunCosines(IHOUR, TS, AvgEqOfTime, AvgSinSolarDeclin, AvgCosSolarDeclin)
      END DO
    END DO
  ELSE
    CALL FigureSunCosines(HourOfDay, TimeStep, AvgEqOfTime, AvgSinSolarDeclin, AvgCosSolarDeclin)
  ENDIF
  !
  !Initialize/update the Complex Fenestration geometry and optical properties
  CALL UpdateComplexWindows
  !
  IF (.NOT. DetailedSolarTimestepIntegration) THEN
    DO IHOUR = 1, 24  ! Do for all hours.
      DO TS=1,NumOfTimeStepInHour
        CALL FigureSolarBeamAtTimestep(IHOUR, TS)
      END DO  ! TimeStep Loop
    END DO  ! Hour Loop
  ELSE
    CALL FigureSolarBeamAtTimestep(HourOfDay, TimeStep)
  ENDIF

  RETURN

END SUBROUTINE CalcPerSolarBeam

SUBROUTINE FigureSunCosines(iHour, iTimeStep, EqOfTime, SinSolarDeclin, CosSolarDeclin)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   October 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
            ! Determine solar position.  Default for sun below horizon.

          ! METHODOLOGY EMPLOYED:
            ! Given hour, timestep, equation of time, solar declination sine, and solar declination cosine,
            ! determine sun directions for use elsewhere

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals,         ONLY: TimeStepZone
  USE DataSystemVariables, ONLY: DetailedSolarTimestepIntegration

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)   :: iHour
  INTEGER, INTENT(IN)   :: iTimeStep
  REAL(r64), INTENT(IN) :: EqOfTime    !  value of Equation of Time for period
  REAL(r64), INTENT(IN) :: SinSolarDeclin !  value of Sine of Solar Declination for period
  REAL(r64), INTENT(IN) :: CosSolarDeclin  !  value of Cosine of Solar Declination for period

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) CurrentTime ! Current Time for passing to Solar Position Routine

  IF (NumOfTimeStepInHour /= 1) THEN
    CurrentTime=REAL(iHour-1,r64) + REAL(iTimeStep,r64)*(TimeStepZone)
  ELSE
    CurrentTime=REAL(iHour,r64)+TS1TimeOffset
  ENDIF
  CALL SUN4(CurrentTime,EqOfTime,SinSolarDeclin,CosSolarDeclin)

  ! Save hourly values for use in DaylightingManager
  IF (.NOT. DetailedSolarTimestepIntegration) THEN
    IF (iTimeStep == NumOfTimeStepInHour) SUNCOSHR(1:3,iHour) = SUNCOS
  ELSE
    SUNCOSHR(1:3,iHour) = SUNCOS
  ENDIF
  ! Save timestep values for use in WindowComplexManager
  SUNCOSTS(1:3,iHour,iTimeStep) = SUNCOS

  RETURN

END SUBROUTINE FigureSunCosines

SUBROUTINE FigureSolarBeamAtTimestep(iHour, iTimeStep)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B.Griffith, derived from CalcPerSolarBeam, Legacy and Lawrie.
          !       DATE WRITTEN   October 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine computes solar gain multipliers for beam solar

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSystemVariables,    ONLY: DetailedSkyDiffuseAlgorithm, DetailedSolarTimestepIntegration

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: iHour
  INTEGER, INTENT(IN)  :: iTimeStep

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER,PARAMETER  :: NPhi = 6          ! Number of altitude angle steps for sky integration
  INTEGER,PARAMETER  :: NTheta = 24       ! Number of azimuth angle steps for sky integration
  REAL(r64),PARAMETER :: Eps = 1.d-10       ! Small number
  REAL(r64),PARAMETER :: DPhi = PiOvr2/NPhi         ! Altitude step size, 15 deg for NPhi = 6
  REAL(r64),PARAMETER :: DTheta = 2.d0*Pi/NTheta    ! Azimuth step size, 15 deg for NTheta = 24
  REAL(r64),PARAMETER :: DThetaDPhi = DTheta*DPhi   ! Product of DTheta and DPhi
  REAL(r64),PARAMETER :: PhiMin = 0.5d0*DPhi        ! Minimum altitude

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS
  REAL(r64)  :: SurfArea          ! Surface area. For walls, includes all window frame areas.
  REAL(r64)  :: CosPhi            ! Cosine of Phi
  INTEGER    :: SurfNum           ! Surface Loop index
  REAL(r64)  :: Fac1WoShdg        ! Intermediate calculation factor, without shading
  REAL(r64)  :: Fac1WithShdg      ! Intermediate calculation factor, with shading

  !Recover the sun direction from the array stored in previous loop
  SUNCOS = SUNCOSTS(1:3,iHour,iTimeStep)

  CTHETA=0.0d0

  IF (SUNCOS(3) < SunIsUpValue) RETURN

  DO SurfNum = 1, TotSurfaces
    CTHETA(SurfNum) = SUNCOS(1)*Surface(SurfNum)%OutNormVec(1)  &
                    + SUNCOS(2)*Surface(SurfNum)%OutNormVec(2)  &
                    + SUNCOS(3)*Surface(SurfNum)%OutNormVec(3)
    IF (.NOT. DetailedSolarTimestepIntegration) THEN
      IF (iTimeStep == NumOfTimeStepInHour) CosIncAngHR(SurfNum,iHour) = CTHETA(SurfNum)
    ELSE
      CosIncAngHR(SurfNum,iHour) = CTHETA(SurfNum)
    ENDIF
    CosIncAng(SurfNum,iHour,iTimeStep) = CTHETA(SurfNum)
  END DO

  CALL SHADOW(iHour,iTimeStep)  ! Determine sunlit areas and solar multipliers for all surfaces.

  DO SurfNum = 1, TotSurfaces
    IF (Surface(SurfNum)%Area >= 1.d-10) THEN
      SurfArea = Surface(SurfNum)%NetAreaShadowCalc
      IF (.NOT. DetailedSolarTimestepIntegration) THEN
        IF (iTimeStep == NumOfTimeStepInHour) SunLitFracHR(SurfNum,iHour) = SAREA(SurfNum)/SurfArea
      ELSE
        SunLitFracHR(SurfNum,iHour) = SAREA(SurfNum)/SurfArea
      ENDIF
      SunLitFrac(SurfNum,iHour,iTimeStep) = SAREA(SurfNum)/SurfArea
      IF (SunLitFrac(SurfNum,iHour,iTimeStep) < 1.d-5) SunLitFrac(SurfNum,iHour,iTimeStep)=0.0d0
    ENDIF

    !Added check
    IF (SunLitFrac(SurfNum,iHour,iTimeStep) > 1.d0) THEN
      SunLitFrac(SurfNum,iHour,iTimeStep) = 1.d0
    ENDIF
  END DO

!   Note -- if not the below, values are set in SkyDifSolarShading routine (constant for simulation)
  IF (DetailedSkyDiffuseAlgorithm .AND. ShadingTransmittanceVaries .AND.  &
      SolarDistribution /= MinimalShadowing) THEN
    CosPhi=1.0d0-SUNCOS(3)

    DO SurfNum = 1,TotSurfaces

      IF (.NOT. Surface(SurfNum)%ShadowingSurf .AND.  &
         (.NOT. Surface(SurfNum)%HeatTransSurf .OR. .NOT. Surface(SurfNum)%ExtSolar .OR. &
         (Surface(SurfNum)%ExtBoundCond /= ExternalEnvironment .AND. &
          Surface(SurfNum)%ExtBoundCond /= OtherSideCondModeledExt) ) ) CYCLE

      IF(CTHETA(SurfNum) < 0.0d0) CYCLE

      Fac1WoShdg = CosPhi * DThetaDPhi * CTHETA(SurfNum)
      Fac1WithShdg = Fac1WoShdg * SunLitFrac(SurfNum,iHour,iTimeStep)
      WithShdgIsoSky(SurfNum) = Fac1WithShdg
      WoShdgIsoSky(SurfNum) = Fac1WoShdg

      ! Horizon region
      IF (SUNCOS(3) <= PhiMin) THEN
        WithShdgHoriz(SurfNum) = Fac1WithShdg
        WoShdgHoriz(SurfNum) = Fac1WoShdg
      END IF
    END DO  ! End of surface loop

    DO SurfNum = 1,TotSurfaces

      IF (.NOT. Surface(SurfNum)%ShadowingSurf .AND.                                                     &
         (.NOT.Surface(SurfNum)%HeatTransSurf .OR. .NOT.Surface(SurfNum)%ExtSolar .OR. &
         (Surface(SurfNum)%ExtBoundCond /= ExternalEnvironment .AND. &
          Surface(SurfNum)%ExtBoundCond /= OtherSideCondModeledExt) )) CYCLE

      IF (ABS(WoShdgIsoSky(SurfNum)) > Eps) THEN
        DifShdgRatioIsoSkyHRTS(SurfNum,iHour,iTimeStep) = (WithShdgIsoSky(SurfNum))/(WoShdgIsoSky(SurfNum))
      ELSE
        DifShdgRatioIsoSkyHRTS(SurfNum,iHour,iTimeStep) = (WithShdgIsoSky(SurfNum))/(WoShdgIsoSky(SurfNum)+Eps)
      ENDIF
      IF (ABS(WoShdgHoriz(SurfNum)) > Eps) THEN
        DifShdgRatioHorizHRTS(SurfNum,iHour,iTimeStep) =  (WithShdgHoriz(SurfNum))/(WoShdgHoriz(SurfNum))
      ELSE
        DifShdgRatioHorizHRTS(SurfNum,iHour,iTimeStep) =  (WithShdgHoriz(SurfNum))/(WoShdgHoriz(SurfNum)+Eps)
      ENDIF
    END DO

  !  ! Get IR view factors. An exterior surface can receive IR radiation from
  !  ! sky, ground or shadowing surfaces. Assume shadowing surfaces have same
  !  ! temperature as outside air (and therefore same temperature as ground),
  !  ! so that the view factor to these shadowing surfaces can be included in
  !  ! the ground view factor. Sky IR is assumed to be isotropic and shadowing
  !  ! surfaces are assumed to be opaque to IR so they totally "shade" IR from
  !  ! sky or ground.

  !  DO SurfNum = 1,TotSurfaces
  !    Surface(SurfNum)%ViewFactorSkyIR = Surface(SurfNum)%ViewFactorSkyIR * DifShdgRatioIsoSky(SurfNum,IHOUR,TS)
  !    Surface(SurfNum)%ViewFactorGroundIR = 1.0 - Surface(SurfNum)%ViewFactorSkyIR
  !  END DO

  ENDIF  ! test for shading surfaces

  DO SurfNum = 1, TotSurfaces
  ! For exterior windows with frame/divider that are partially or fully sunlit,
  ! correct SunLitFrac due to shadowing of frame and divider projections onto window glass.
  ! Note: if SunLitFrac = 0.0 the window is either completely shaded or the sun is in back
  ! of the window; in either case, frame/divider shadowing doesn't have to be done.

    IF(Surface(SurfNum)%Class == SurfaceClass_Window .AND.   &
       Surface(SurfNum)%ExtBoundCond == ExternalEnvironment .AND. &
       SunLitFrac(SurfNum,iHour,iTimeStep) > 0.0d0 .AND. Surface(SurfNum)%FrameDivider > 0) &
          CALL CalcFrameDividerShadow(SurfNum,Surface(SurfNum)%FrameDivider,iHour)
  END DO
  RETURN

END SUBROUTINE FigureSolarBeamAtTimestep

SUBROUTINE DetermineShadowingCombinations

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         From Legacy Code
          !       DATE WRITTEN
          !       MODIFIED       LKL; March 2002 -- another missing translation from BLAST's routine
          !                      FCW; Jan 2003 -- removed line that prevented beam solar through interior windows
          !       RE-ENGINEERED  Rick Strand; 1998
          !                      Linda Lawrie; Oct 2000

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine prepares a list of heat transfer surfaces and
          ! their possible shadowers which is used to direct the hourly
          ! calculation of shadows and sunlit areas.

          ! METHODOLOGY EMPLOYED:
          ! As appropriate surfaces are identified, they are placed into the
          ! ShadowComb data structure (module level) with the accompanying lists
          ! of other surface numbers.

          ! REFERENCES:
          ! BLAST/IBLAST code, original author George Walton

          ! USE STATEMENTS:
  USE OutputReportPredefined, ONLY: ShadowRelate,numShadowRelate,recKindSurface,recKindSubsurface
  USE DataErrorTracking
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: fmta='(A)'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER, ALLOCATABLE, DIMENSION(:) :: GSS ! List of shadowing surfaces numbers for a receiving surface
  INTEGER, ALLOCATABLE, DIMENSION(:) :: BKS ! List of back surface numbers for a receiving surface
  INTEGER, ALLOCATABLE, DIMENSION(:) :: SBS ! List of subsurfaces for a receiving surface
  INTEGER, ALLOCATABLE, DIMENSION(:) :: ListTemp ! Temporary array for reallocations
  INTEGER :: MaxGSS=50 ! Current Max for GSS array
  INTEGER :: MaxBKS=50 ! Current Max for BKS array
  INTEGER :: MaxSBS=50 ! Current Max for SBS array
  LOGICAL CannotShade  ! TRUE if subsurface cannot shade receiving surface
  LOGICAL HasWindow   ! TRUE if a window is present on receiving surface
  REAL(r64) ZMIN ! Lowest point on the receiving surface
  INTEGER BackSurfaceNumber ! Back surface number
  INTEGER HTS      ! Heat transfer surface number for a receiving surface
  INTEGER GRSNR    ! Receiving surface number
  INTEGER GSSNR    ! Shadowing surface number
  INTEGER SBSNR    ! Subsurface number
  INTEGER NBKS     ! Number of back surfaces for a receiving surface
  INTEGER NGSS     ! Number of shadowing surfaces for a receiving surface
  INTEGER NSBS     ! Number of subsurfaces for a receiving surface
  LOGICAL ShadowingSurf  ! True if a receiving surface is a shadowing surface
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: CastingSurface ! tracking during setup of ShadowComb

  INTEGER :: MaxDim=0

#ifdef EP_Count_Calls
  NumDetShadowCombs_Calls=NumDetShadowCombs_Calls+1
#endif

  ALLOCATE(ShadowComb(TotSurfaces))
  ShadowComb%UseThisSurf=.false.
  ShadowComb%NumGenSurf=0
  ShadowComb%NumSubSurf=0
  ShadowComb%NumBackSurf=0

  ALLOCATE(CastingSurface(TotSurfaces))
  CastingSurface=.false.

  ALLOCATE(HCA(MaxHCV+1,MaxHCS*2))
  HCA=0
  ALLOCATE(HCB(MaxHCV+1,MaxHCS*2))
  HCB=0
  ALLOCATE(HCC(MaxHCV+1,MaxHCS*2))
  HCC=0
  ALLOCATE(HCX(MaxHCV+1,MaxHCS*2))
  HCX=0
  ALLOCATE(HCY(MaxHCV+1,MaxHCS*2))
  HCY=0
  ALLOCATE(HCAREA(MaxHCS*2))
  HCAREA=0.0D0
  ALLOCATE(HCNS(MaxHCS*2))
  HCNS=0
  ALLOCATE(HCNV(MaxHCS*2))
  HCNV=0
  ALLOCATE(HCT(MaxHCS*2))
  HCT=0.0D0

  ALLOCATE(GSS(MaxGSS))
  ALLOCATE(BKS(MaxGSS))
  ALLOCATE(SBS(MaxGSS))
  GSS=0
  BKS=0
  SBS=0

  HTS = 0

  ! Check every surface as a possible shadow receiving surface ("RS" = receiving surface).
  IF (IgnoreSolarRadiation) THEN
    RETURN
  ENDIF

  DO GRSNR = 1, TotSurfaces ! Loop through all surfaces (looking for potential receiving surfaces)...

    ShadowingSurf = Surface(GRSNR)%ShadowingSurf
    NGSS=0
    NSBS=0
    NBKS=0

    IF (.NOT. ShadowingSurf .AND. .NOT. Surface(GRSNR)%HeatTransSurf) CYCLE
    HTS = GRSNR
    IF (.NOT. ShadowingSurf .AND. .NOT.Surface(GRSNR)%ExtSolar) CYCLE      ! Skip surfaces with no external solar

    IF (.NOT. ShadowingSurf .AND. Surface(GRSNR)%BaseSurf /= GRSNR) CYCLE  ! Skip subsurfaces (SBS)

          ! Get the lowest point of receiving surface
    ZMIN=MINVAL(Surface(GRSNR)%Vertex(:)%Z)

          ! Check every surface as a possible shadow casting surface ("SS" = shadow sending)
    NGSS=0
    IF (SolarDistribution /= MinimalShadowing) THEN ! Except when doing simplified exterior shadowing.

      DO GSSNR = 1, TotSurfaces ! Loop through all surfaces, looking for ones that could shade GRSNR

        IF (GSSNR == GRSNR) CYCLE   ! Receiving surface cannot shade itself
        IF ((Surface(GSSNR)%HeatTransSurf).AND. &
            (Surface(GSSNR)%BaseSurf == GRSNR)) CYCLE ! A heat transfer subsurface of a receiving surface
                                                      ! cannot shade the receiving surface
        IF(ShadowingSurf) THEN
          ! If receiving surf is a shadowing surface exclude matching shadow surface as sending surface
          !IF((GSSNR == GRSNR+1 .AND. Surface(GSSNR)%Name(1:3) == 'Mir').OR. &
          !   (GSSNR == GRSNR-1 .AND. Surface(GRSNR)%Name(1:3) == 'Mir')) CYCLE
          IF(((GSSNR == GRSNR+1) .AND. Surface(GSSNR)%MirroredSurf).OR. &
             ((GSSNR == GRSNR-1) .AND. Surface(GRSNR)%MirroredSurf)) CYCLE
        END IF

        IF (Surface(GSSNR)%BaseSurf == GRSNR) THEN ! Shadowing subsurface of receiving surface

          NGSS=NGSS+1
          IF (NGSS > MaxGSS) THEN
            ALLOCATE(ListTemp(MaxGSS*2))
            ListTemp=0
            ListTemp(1:MaxGSS)=GSS(1:MaxGSS)
            DEALLOCATE(GSS)
            ALLOCATE(GSS(MaxGSS*2))
            GSS=ListTemp
            MaxGSS=MaxGSS*2
            DEALLOCATE(ListTemp)
          ENDIF
          GSS(NGSS)=GSSNR

        ELSEIF ((Surface(GSSNR)%BaseSurf == 0).OR. &       ! Detached shadowing surface or
                ((Surface(GSSNR)%BaseSurf == GSSNR).AND. & ! any other base surface exposed to outside environment
                ((Surface(GSSNR)%ExtBoundCond == ExternalEnvironment) .or. &
                 Surface(GSSNR)%ExtBoundCond == OtherSideCondModeledExt ) )) THEN

          CALL CHKGSS(GRSNR,GSSNR,ZMIN,CannotShade)  ! Check to see if this can shade the receiving surface
          IF (.NOT. CannotShade) THEN ! Update the shadowing surface data if shading is possible
            NGSS=NGSS+1
            IF (NGSS > MaxGSS) THEN
              ALLOCATE(ListTemp(MaxGSS*2))
              ListTemp=0
              ListTemp(1:MaxGSS)=GSS(1:MaxGSS)
              DEALLOCATE(GSS)
              ALLOCATE(GSS(MaxGSS*2))
              GSS=ListTemp
              MaxGSS=MaxGSS*2
              DEALLOCATE(ListTemp)
            ENDIF
            GSS(NGSS)=GSSNR
          END IF

        END IF

      END DO    ! ...end of surfaces DO loop (GSSNR)
    ELSE  ! Simplified Distribution -- still check for Shading Subsurfaces

      DO GSSNR = 1, TotSurfaces ! Loop through all surfaces (looking for surfaces which could shade GRSNR) ...

        IF (GSSNR == GRSNR) CYCLE   ! Receiving surface cannot shade itself
        IF ((Surface(GSSNR)%HeatTransSurf).AND. &
            (Surface(GSSNR)%BaseSurf == GRSNR)) CYCLE ! Skip heat transfer subsurfaces of receiving surface
        IF (Surface(GSSNR)%BaseSurf == GRSNR) THEN ! Shadowing subsurface of receiving surface
          NGSS=NGSS+1
          IF (NGSS > MaxGSS) THEN
            ALLOCATE(ListTemp(MaxGSS*2))
            ListTemp=0
            ListTemp(1:MaxGSS)=GSS(1:MaxGSS)
            DEALLOCATE(GSS)
            ALLOCATE(GSS(MaxGSS*2))
            GSS=ListTemp
            MaxGSS=MaxGSS*2
            DEALLOCATE(ListTemp)
          ENDIF
          GSS(NGSS)=GSSNR
        ENDIF
      ENDDO

    END IF  ! ...end of check for simplified solar distribution

          ! Check every surface as a receiving subsurface of the receiving surface
    NSBS=0
    HasWindow=.FALSE.
    !legacy: IF (OSENV(HTS) > 10) WINDOW=.TRUE. -->Note: WINDOW was set true for roof ponds, solar walls, or other zones
    DO SBSNR = 1, TotSurfaces   ! Loop through the surfaces yet again (looking for subsurfaces of GRSNR)...

      IF (.NOT.Surface(SBSNR)%HeatTransSurf) CYCLE  ! Skip non heat transfer subsurfaces
      IF (SBSNR == GRSNR) CYCLE                     ! Surface itself cannot be its own subsurface
      IF (Surface(SBSNR)%BaseSurf /= GRSNR) CYCLE   ! Ignore subsurfaces of other surfaces and other surfaces

      IF (Construct(Surface(SBSNR)%Construction)%TransDiff > 0.0d0) HasWindow=.TRUE. ! Check for window
      CALL CHKSBS(HTS,GRSNR,SBSNR)   ! Check that the receiving surface completely encloses the subsurface;
                                     ! severe error if not
      NSBS=NSBS+1
      IF (NSBS > MaxSBS) THEN
        ALLOCATE(ListTemp(MaxSBS*2))
        ListTemp=0
        ListTemp(1:MaxSBS)=SBS(1:MaxSBS)
        DEALLOCATE(SBS)
        ALLOCATE(SBS(MaxSBS*2))
        SBS=ListTemp
        MaxSBS=MaxSBS*2
        DEALLOCATE(ListTemp)
      ENDIF
      SBS(NSBS)=SBSNR

    END DO  ! ...end of surfaces DO loop (SBSNR)

          ! Check every surface as a back surface
    NBKS=0
!                                        Except for simplified
!                                        interior solar distribution,
    IF ((SolarDistribution == FullInteriorExterior) .AND. & ! For full interior solar distribution
                                           (HasWindow)) THEN   ! and a window present on base surface (GRSNR)

      DO BackSurfaceNumber = 1, TotSurfaces ! Loop through surfaces yet again, looking for back surfaces to GRSNR

        IF (.NOT.Surface(BackSurfaceNumber)%HeatTransSurf) CYCLE  ! Skip non-heat transfer surfaces
        IF (Surface(BackSurfaceNumber)%BaseSurf == GRSNR) CYCLE   ! Skip subsurfaces of this GRSNR
        IF (BackSurfaceNumber == GRSNR) CYCLE                     ! A back surface cannot be GRSNR itself
        IF (Surface(BackSurfaceNumber)%Zone /= Surface(GRSNR)%Zone) CYCLE   ! Skip if back surface not in zone

        IF (Surface(BackSurfaceNumber)%Class == SurfaceClass_IntMass) CYCLE

        ! Following line removed 1/27/03 by FCW. Was in original code that didn't do beam solar transmitted through
        ! interior windows. Was removed to allow such beam solar but then somehow was put back in.
        !IF (Surface(BackSurfaceNumber)%BaseSurf /= BackSurfaceNumber) CYCLE ! Not for subsurfaces of Back Surface

        CALL CHKBKS(BackSurfaceNumber,GRSNR)    ! CHECK FOR CONVEX ZONE; severe error if not
        NBKS=NBKS+1
        IF (NBKS > MaxBKS) THEN
          ALLOCATE(ListTemp(MaxBKS*2))
          ListTemp=0
          ListTemp(1:MaxBKS)=BKS(1:MaxBKS)
          DEALLOCATE(BKS)
          ALLOCATE(BKS(MaxBKS*2))
          BKS=ListTemp
          MaxBKS=MaxBKS*2
          DEALLOCATE(ListTemp)
        ENDIF
        BKS(NBKS)=BackSurfaceNumber

      END DO    ! ...end of surfaces DO loop (BackSurfaceNumber)

    END IF

    ! Put this into the ShadowComb data structure
    ShadowComb(GRSNR)%UseThisSurf=.true.
    ShadowComb(GRSNR)%NumGenSurf=NGSS
    ShadowComb(GRSNR)%NumBackSurf=NBKS
    ShadowComb(GRSNR)%NumSubSurf=NSBS
    MaxDim=MAX(MaxDim,NGSS,NBKS,NSBS)

    ALLOCATE(ShadowComb(GRSNR)%GenSurf(0:ShadowComb(GRSNR)%NumGenSurf))
    ShadowComb(GRSNR)%GenSurf(0)=0
    IF (ShadowComb(GRSNR)%NumGenSurf > 0) THEN
      ShadowComb(GRSNR)%GenSurf(1:ShadowComb(GRSNR)%NumGenSurf)=GSS(1:NGSS)
    ENDIF

    ALLOCATE(ShadowComb(GRSNR)%BackSurf(0:ShadowComb(GRSNR)%NumBackSurf))
    ShadowComb(GRSNR)%BackSurf(0)=0
    IF (ShadowComb(GRSNR)%NumBackSurf > 0) THEN
      ShadowComb(GRSNR)%BackSurf(1:ShadowComb(GRSNR)%NumBackSurf)=BKS(1:NBKS)
    ENDIF

    ALLOCATE(ShadowComb(GRSNR)%SubSurf(0:ShadowComb(GRSNR)%NumSubSurf))
    ShadowComb(GRSNR)%SubSurf(0)=0
    IF (ShadowComb(GRSNR)%NumSubSurf > 0) THEN
      ShadowComb(GRSNR)%SubSurf(1:ShadowComb(GRSNR)%NumSubSurf)=SBS(1:NSBS)
    ENDIF

  END DO ! ...end of surfaces (GRSNR) DO loop

  DEALLOCATE(GSS)
  DEALLOCATE(SBS)
  DEALLOCATE(BKS)

  WRITE(OutputFileShading,fmta) ' Shadowing Combinations'
  SELECT CASE (SolarDistribution)
    CASE (MinimalShadowing)
      WRITE(OutputFileShading,fmta) ' ..Solar Distribution=Minimal Shadowing, Detached Shading will not be used'//  &
                                      ' in shadowing calculations'
    CASE (FullExterior)
      IF (CalcSolRefl) THEN
        WRITE(OutputFileShading,fmta) '..Solar Distribution=FullExteriorWithReflectionsFromExteriorSurfaces'
      ELSE
        WRITE(OutputFileShading,fmta) '..Solar Distribution=FullExterior'
      ENDIF
    CASE (FullInteriorExterior)
      IF (CalcSolRefl) THEN
        WRITE(OutputFileShading,fmta) '..Solar Distribution=FullInteriorAndExteriorWithReflectionsFromExteriorSurfaces'
      ELSE
        WRITE(OutputFileShading,fmta) '..Solar Distribution=FullInteriorAndExterior'
      ENDIF
    CASE DEFAULT
   END SELECT

   WRITE(OutputFileShading,fmta) '..In the following, only the first 10 reference surfaces will be shown.'
   WRITE(OutputFileShading,fmta) '..But all surfaces are used in the calculations.'

   DO HTS=1,TotSurfaces
    WRITE(OutputFileShading,fmta) ' =================================='
    IF (ShadowComb(HTS)%UseThisSurf) THEN
      IF (Surface(HTS)%IsConvex) THEN
        WRITE(OutputFileShading,fmta) ' Surface='//TRIM(Surface(HTS)%Name)//  &
             ' is used as Receiving Surface in calculations and is convex.'
      ELSE
        WRITE(OutputFileShading,fmta) ' Surface='//TRIM(Surface(HTS)%Name)//  &
             ' is used as Receiving Surface in calculations and is non-convex.'
        IF (ShadowComb(HTS)%NumGenSurf > 0) THEN
          IF (DisplayExtraWarnings) THEN
            CALL ShowWarningError('DetermineShadowingCombinations: Surface="'//  &
               trim(Surface(HTS)%Name)//'" is a receiving surface and is non-convex.')
            CALL ShowContinueError('...Shadowing values may be inaccurate. Check .shd report file for more surface shading details')
          ELSE
            TotalReceivingNonConvexSurfaces=TotalReceivingNonConvexSurfaces+1
          ENDIF
        ENDIF
      ENDIF
    ELSE
      WRITE(OutputFileShading,fmta) ' Surface='//TRIM(Surface(HTS)%Name)//  &
           ' is not used as Receiving Surface in calculations.'
    ENDIF
    WRITE(OutputFileShading,*) 'Number of general casting surfaces=',ShadowComb(HTS)%NumGenSurf
    DO NGSS=1,ShadowComb(HTS)%NumGenSurf
      IF (NGSS <= 10) &
        WRITE(OutputFileShading,fmta) ' ..Surface='//TRIM(Surface(ShadowComb(HTS)%GenSurf(NGSS))%Name)
      CastingSurface(ShadowComb(HTS)%GenSurf(NGSS))=.true.
    ENDDO
    WRITE(OutputFileShading,*) 'Number of back surfaces=',ShadowComb(HTS)%NumBackSurf
    DO NGSS=1,MIN(10,ShadowComb(HTS)%NumBackSurf)
      WRITE(OutputFileShading,fmta) ' ...Surface='//TRIM(Surface(ShadowComb(HTS)%BackSurf(NGSS))%Name)
    ENDDO
    WRITE(OutputFileShading,*) 'Number of receiving sub surfaces=',ShadowComb(HTS)%NumSubSurf
    DO NGSS=1,MIN(10,ShadowComb(HTS)%NumSubSurf)
      WRITE(OutputFileShading,fmta) ' ....Surface='//TRIM(Surface(ShadowComb(HTS)%SubSurf(NGSS))%Name)
    ENDDO
  ENDDO

  DO HTS=1,TotSurfaces
    IF (CastingSurface(HTS) .and. .not. Surface(HTS)%IsConvex) THEN
      IF (DisplayExtraWarnings) THEN
        CALL ShowSevereError('DetermineShadowingCombinations: Surface="'//  &
           trim(Surface(HTS)%Name)//'" is a casting surface and is non-convex.')
        CALL ShowContinueError('...Shadowing values may be inaccurate. Check .shd report file for more surface shading details')
      ELSE
        TotalCastingNonConvexSurfaces=TotalCastingNonConvexSurfaces+1
      ENDIF
    ENDIF
  ENDDO

  DEALLOCATE(CastingSurface)

  IF (TotalReceivingNonConvexSurfaces > 0) THEN
    CALL ShowWarningMessage('DetermineShadowingCombinations: There are '//trim(TrimSigDigits(TotalReceivingNonConvexSurfaces))//  &
       ' surfaces which are receiving surfaces and are non-convex.')
    CALL ShowContinueError('...Shadowing values may be inaccurate. Check .shd report file for more surface shading details')
    CALL ShowContinueError('...Add Output:Diagnostics,DisplayExtraWarnings; to see individual warnings for each surface.')
    TotalWarningErrors=TotalWarningErrors+TotalReceivingNonConvexSurfaces
  ENDIF

  IF (TotalCastingNonConvexSurfaces > 0) THEN
    CALL ShowSevereMessage('DetermineShadowingCombinations: There are '//trim(TrimSigDigits(TotalCastingNonConvexSurfaces))//  &
       ' surfaces which are casting surfaces and are non-convex.')
    CALL ShowContinueError('...Shadowing values may be inaccurate. Check .shd report file for more surface shading details')
    CALL ShowContinueError('...Add Output:Diagnostics,DisplayExtraWarnings; to see individual severes for each surface.')
    TotalSevereErrors=TotalSevereErrors+TotalCastingNonConvexSurfaces
  ENDIF

  RETURN

END SUBROUTINE DetermineShadowingCombinations

SUBROUTINE SHADOW(IHOUR,TS)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN
          !       MODIFIED       Nov 2003, FCW: modify to do shadowing on shadowing surfaces
          !       RE-ENGINEERED  Lawrie, Oct 2000

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine is a driving routine for calculations of shadows
          ! and sunlit areas used in computing the solar beam flux multipliers.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! BLAST/IBLAST code, original author George Walton

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: IHOUR  ! Hour index
  INTEGER, INTENT(IN) :: TS     ! Time Step

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) XS  ! Intermediate result
  REAL(r64) YS  ! Intermediate result
  REAL(r64) ZS  ! Intermediate result
  INTEGER N     ! Vertex number
  INTEGER NGRS  ! Coordinate transformation index
  INTEGER NZ    ! Zone Number of surface
  INTEGER NVT
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: XVT      ! X Vertices of Shadows
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: YVT      ! Y vertices of Shadows
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: ZVT      ! Z vertices of Shadows
  LOGICAL, SAVE :: OneTimeFlag=.true.
  INTEGER HTS      ! Heat transfer surface number of the general receiving surface
  INTEGER GRSNR    ! Surface number of general receiving surface
  INTEGER NBKS     ! Number of back surfaces
  INTEGER NGSS     ! Number of general shadowing surfaces
  INTEGER NSBS     ! Number of subsurfaces (windows and doors)
  REAL(r64) :: SurfArea    ! Surface area. For walls, includes all window frame areas.
                   ! For windows, includes divider area

  IF (OneTimeFlag) THEN
    ALLOCATE(XVT(MaxVerticesPerSurface+1))
    ALLOCATE(YVT(MaxVerticesPerSurface+1))
    ALLOCATE(ZVT(MaxVerticesPerSurface+1))
    XVT=0.0d0
    YVT=0.0d0
    ZVT=0.0d0
    OneTimeFlag=.false.
  ENDIF

#ifdef EP_Count_Calls
  IF (IHOUR == 0) THEN
    NumShadow_Calls=NumShadow_Calls+1
  ELSE
    NumShadowAtTS_Calls=NumShadowAtTS_Calls+1
  ENDIF
#endif

  SAREA=0.0d0

  DO GRSNR=1,TotSurfaces

    IF (.NOT. ShadowComb(GRSNR)%UseThisSurf) CYCLE

    SAREA(GRSNR)=0.0d0

    NZ     = Surface(GRSNR)%Zone
    NGSS   = ShadowComb(GRSNR)%NumGenSurf
    NGSSHC = 0
    NBKS   = ShadowComb(GRSNR)%NumBackSurf
    NBKSHC = 0
    NSBS   = ShadowComb(GRSNR)%NumSubSurf
    NRVLHC = 0
    NSBSHC = 0
    LOCHCA = 1
          ! Temporarily determine the old heat transfer surface number (HTS)
    HTS=GRSNR

    IF (CTHETA(GRSNR) < SunIsUpValue) THEN !.001) THEN ! Receiving surface is not in the sun

      SAREA(HTS) = 0.0d0
      CALL SHDSBS(IHOUR,GRSNR,NBKS,NSBS,HTS,TS)

    ELSEIF ((NGSS <= 0).AND.(NSBS <= 0)) THEN   ! Simple surface--no shaders or subsurfaces

      SAREA(HTS) = Surface(GRSNR)%NetAreaShadowCalc
    ELSE    ! Surface in sun and either shading surfaces or subsurfaces present (or both)

      NGRS=Surface(GRSNR)%BaseSurf
      IF(Surface(GRSNR)%ShadowingSurf) NGRS = GRSNR

          ! Compute the X and Y displacements of a shadow.
      XS = Surface(NGRS)%lcsx%x*SUNCOS(1) + Surface(NGRS)%lcsx%y*SUNCOS(2) + Surface(NGRS)%lcsx%z*SUNCOS(3)
      YS = Surface(NGRS)%lcsy%x*SUNCOS(1) + Surface(NGRS)%lcsy%y*SUNCOS(2) + Surface(NGRS)%lcsy%z*SUNCOS(3)
      ZS = Surface(NGRS)%lcsz%x*SUNCOS(1) + Surface(NGRS)%lcsz%y*SUNCOS(2) + Surface(NGRS)%lcsz%z*SUNCOS(3)

      IF (ABS(ZS) > 1.d-4) THEN
        XShadowProjection = XS/ZS
        YShadowProjection = YS/ZS
        IF (ABS(XShadowProjection) < 1.d-8) XShadowProjection=0.0d0
        IF (ABS(YShadowProjection) < 1.d-8) YShadowProjection=0.0d0
      ELSE
        XShadowProjection = 0.0d0
        YShadowProjection = 0.0d0
      END IF

      CALL CTRANS(GRSNR,NGRS,NVT,XVT,YVT,ZVT)   ! Transform coordinates of the receiving surface to 2-D form

          ! Re-order its vertices to clockwise sequential.
      DO N = 1, NVT
        XVS(N) = XVT(NVT+1-N)
        YVS(N) = YVT(NVT+1-N)
      END DO

      CALL HTRANS1(1,NVT)  ! Transform to homogeneous coordinates.

      HCAREA(1)=-HCAREA(1)  ! Compute (+) gross surface area.
      HCT(1)=1.0d0

      CALL SHDGSS(NGRS,IHOUR,TS,GRSNR,NGSS,HTS)   ! Determine shadowing on surface.
      IF(.NOT.CalcSkyDifShading) THEN
        CALL SHDBKS(NGRS,GRSNR,NBKS,HTS) ! Determine possible back surfaces.
      END IF

      CALL SHDSBS(IHOUR,GRSNR,NBKS,NSBS,HTS,TS) ! Subtract subsurf areas from total

          ! Error checking:  require that 0 <= SAREA <= AREA.  + or - .01*AREA added for round-off errors
      SurfArea = Surface(GRSNR)%NetAreaShadowCalc
      SAREA(HTS)=MAX(0.0d0,SAREA(HTS))

      SAREA(HTS)=MIN(SAREA(HTS),SurfArea)

    END IF  ! ...end of surface in sun/surface with shaders and/or subsurfaces IF-THEN block

          ! NOTE:
          ! There used to be a call to legacy subroutine SHDCVR here when the
          ! zone type was not a standard zone.

  END DO

  RETURN

END SUBROUTINE SHADOW

SUBROUTINE SHDBKS(NGRS,CurSurf,NBKS,HTS)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN
          !       MODIFIED       na
          !       RE-ENGINEERED  Lawrie, Oct 2000

          ! PURPOSE OF THIS SUBROUTINE:
          ! This is the driving subroutine for computing
          ! the sunlit areas for back surfaces.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! BLAST/IBLAST code, original author George Walton

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: NGRS     ! Number of the general receiving surface
  INTEGER, INTENT(IN) :: CurSurf
  INTEGER, INTENT(IN) :: HTS      ! Heat transfer surface number of the general receiving surf
  INTEGER, INTENT(IN) :: NBKS     ! Number of back surfaces

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER I
  INTEGER M
  INTEGER N
  INTEGER NVR
  INTEGER NVT  ! Number of vertices of back surface
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: XVT    ! X,Y,Z coordinates of vertices of
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: YVT    ! back surfaces projected into system
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: ZVT    ! relative to receiving surface
  LOGICAL, SAVE :: OneTimeFlag=.true.
  INTEGER BackSurfaceNumber
  INTEGER NS1      ! Number of the figure being overlapped
  INTEGER NS2      ! Number of the figure doing overlapping
  INTEGER NS3      ! Location to place results of overlap

  IF (OneTimeFlag) THEN
    ALLOCATE(XVT(MaxVerticesPerSurface+1))
    ALLOCATE(YVT(MaxVerticesPerSurface+1))
    ALLOCATE(ZVT(MaxVerticesPerSurface+1))
    XVT=0.0d0
    YVT=0.0d0
    ZVT=0.0d0
    OneTimeFlag=.false.
  ENDIF

  IF ( (NBKS <= 0).OR.(SAREA(HTS) <= 0.0d0).OR. &
       (OverlapStatus == TooManyVertices).OR. &
       (OverlapStatus == TooManyFigures) ) RETURN

  FBKSHC = LOCHCA + 1

  DO I = 1, NBKS ! Loop through all back surfaces associated with the receiving surface

    BackSurfaceNumber = ShadowComb(CurSurf)%BackSurf(I)

    IF (CTHETA(BackSurfaceNumber) > -SunIsUpValue) CYCLE !-0.001) CYCLE ! go to next back surface since inside of this surface
                                                  ! cannot be in sun if the outside can be

          ! Transform coordinates of back surface from general system to the
          ! plane of the receiving surface

    CALL CTRANS(BackSurfaceNumber,NGRS,NVT,XVT,YVT,ZVT)

          ! Project "shadow" from back surface along sun's rays to receiving surface.  Back surface vertices
          ! become clockwise sequential.

    DO N = 1, NVT
      XVS(N) = XVT(N) - XShadowProjection*ZVT(N)
      YVS(N) = YVT(N) - YShadowProjection*ZVT(N)
    END DO

          ! Transform to the homogeneous coordinate system.

    NS3      = LOCHCA+1
    HCT(NS3) = 0.0d0
    CALL HTRANS1(NS3,NVT)

          ! Adjust near-duplicate points.

    NVR = HCNV(1)
    DO N = 1, NVT
      DO M = 1, NVR
        IF (ABS(HCX(M,1)-HCX(N,NS3)) > 6) CYCLE
        IF (ABS(HCY(M,1)-HCY(N,NS3)) > 6) CYCLE
        HCX(N,NS3) = HCX(M,1)
        HCY(N,NS3) = HCY(M,1)
      END DO
    END DO

    CALL HTRANS0(NS3,NVT)

          ! Determine area of overlap of projected back surface and receiving surface.

    NS1      = 1
    NS2      = NS3
    HCT(NS3) = 1.0d0
    CALL DeterminePolygonOverlap(NS1,NS2,NS3)

    IF (OverlapStatus == NoOverlap) CYCLE ! to next back surface
    IF ( (OverlapStatus == TooManyVertices).OR. &
         (OverlapStatus == TooManyFigures) ) EXIT ! back surfaces DO loop

          ! Increment back surface count.

    LOCHCA         = NS3
    HCNS(LOCHCA)   = BackSurfaceNumber
    HCAREA(LOCHCA) = -HCAREA(LOCHCA)
    NBKSHC         = LOCHCA - FBKSHC + 1

  END DO

  RETURN

END SUBROUTINE SHDBKS

SUBROUTINE SHDGSS(NGRS,IHOUR,TS,CurSurf,NGSS,HTS)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN
          !       MODIFIED       na
          !       RE-ENGINEERED  Lawrie, Oct 2000

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine determines the shadows on a general receiving surface.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! BLAST/IBLAST code, original author George Walton

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: LookUpScheduleValue, GetCurrentScheduleValue, GetScheduleMinValue, GetScheduleName

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: NGRS
  INTEGER, INTENT(IN) :: IHOUR    ! Hour Counter
  INTEGER, INTENT(IN) :: TS       ! TimeStep
  INTEGER, INTENT(IN) :: CurSurf  ! Current Surface
  INTEGER, INTENT(IN) :: HTS      ! Heat transfer surface number of the general receiving surf
  INTEGER, INTENT(IN) :: NGSS     ! Number of general shadowing surfaces

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER I
  INTEGER N
  INTEGER M
  INTEGER NVR
  REAL(r64) A  ! Area
  INTEGER GSSNR    ! General shadowing surface number
  INTEGER MainOverlapStatus ! Overlap status of the main overlap calculation not the check for
                            ! multiple overlaps (unless there was an error)
  INTEGER NVT
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: XVT
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: YVT
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: ZVT
  LOGICAL, SAVE :: OneTimeFlag=.true.
  INTEGER NS1      ! Number of the figure being overlapped
  INTEGER NS2      ! Number of the figure doing overlapping
  INTEGER NS3      ! Location to place results of overlap
  REAL(r64) SchValue    ! Value for Schedule of shading transmittence
  INTEGER ExitLoopStatus

  IF (OneTimeFlag) THEN
    ALLOCATE(XVT(MaxVerticesPerSurface+1))
    ALLOCATE(YVT(MaxVerticesPerSurface+1))
    ALLOCATE(ZVT(MaxVerticesPerSurface+1))
    XVT=0.0d0
    YVT=0.0d0
    ZVT=0.0d0
    OneTimeFlag=.false.
  ENDIF

  FGSSHC        = LOCHCA + 1
  MainOverlapStatus = NoOverlap ! Set to ensure that the value from the last surface is not saved
  OverlapStatus= NoOverlap

  IF (NGSS <= 0) THEN   ! IF NO S.S., receiving surface FULLY SUNLIT.

    SAREA(HTS) = HCAREA(1)  ! Surface fully sunlit

  ELSE

  ShadowingSurfaces:  DO I = 1, NGSS    ! Loop through all shadowing surfaces...

      GSSNR = ShadowComb(CurSurf)%GenSurf(I)
      CurrentSurfaceBeingShadowed=GSSNR
      CurrentShadowingSurface=I

      ExitLoopStatus=-1

      IF (CTHETA(GSSNR) > SunIsUpValue) CYCLE !.001) CYCLE ! NO SHADOW IF GSS IN SUNLIGHT.

!     This used to check to see if the shadowing surface was not opaque (within the scheduled dates of
!            transmittance value.  Perhaps it ignored it if it were outside the range.  (if so, was an error)
!     The proper action seems to be delete this statement all together, but there would also be no shading if
!            the shading surface were transparent...
!---former stmt      IF ((.NOT.Surface(GSSNR)%HeatTransSurf) .AND. &
!---former stmt            GetCurrentScheduleValue(Surface(GSSNR)%SchedShadowSurfIndex,IHOUR) == 0.0) CYCLE

     IF (.NOT.Surface(GSSNR)%HeatTransSurf) THEN
       IF (Surface(GSSNR)%IsTransparent) CYCLE
       IF (Surface(GSSNR)%SchedShadowSurfIndex > 0) THEN
         IF (LookUpScheduleValue(Surface(GSSNR)%SchedShadowSurfIndex,IHOUR) == 1.0d0) CYCLE
       ENDIF
     ENDIF

!      No shadow if shading surface is transparent
      IF (.not. CalcSkyDifShading) THEN
        IF (.not.Surface(GSSNR)%HeatTransSurf) THEN
          IF (Surface(GSSNR)%IsTransparent) CYCLE
          IF (Surface(GSSNR)%SchedShadowSurfIndex > 0) THEN
            IF (LookUpScheduleValue(Surface(GSSNR)%SchedShadowSurfIndex,IHOUR,TS) == 1.0d0) CYCLE
          ENDIF
        ENDIF
      ELSE
        IF (.not.Surface(GSSNR)%HeatTransSurf) THEN
          IF (Surface(GSSNR)%SchedShadowSurfIndex > 0) THEN
            IF (Surface(GSSNR)%IsTransparent) CYCLE
          ENDIF
        ENDIF
      ENDIF

!      IF ((.NOT.Surface(GSSNR)%HeatTransSurf) .AND. &
!            GetCurrentScheduleValue(Surface(GSSNR)%SchedShadowSurfIndex) == 1.0) CYCLE

          ! Transform shadow casting surface from cartesian to homogeneous coordinates according to surface type.

      IF ((Surface(GSSNR)%BaseSurf /= 0).AND.(.NOT.Surface(GSSNR)%HeatTransSurf)) THEN

          ! For shadowing subsurface coordinates of shadow casting surface are relative to the receiving surface
          ! project shadow to the receiving surface

        NVS = Surface(GSSNR)%Sides
        DO N = 1, NVS
          XVS(N) = ShadeV(GSSNR)%XV(N) - XShadowProjection*ShadeV(GSSNR)%ZV(N)
          YVS(N) = ShadeV(GSSNR)%YV(N) - YShadowProjection*ShadeV(GSSNR)%ZV(N)
        END DO

      ELSE
        ! Transform coordinates of shadow casting surface from general system to the system relative to the receiving surface
        CALL CTRANS(GSSNR,NGRS,NVT,XVT,YVT,ZVT)
        CALL CLIP(NVT,XVT,YVT,ZVT) ! Clip portions of the shadow casting surface which are behind the receiving surface

        IF (NumVertInShadowOrClippedSurface <= 2) CYCLE

          ! Project shadow from shadow casting surface along sun's rays to receiving surface  Shadow vertices
          ! become clockwise sequential

        DO N = 1, NumVertInShadowOrClippedSurface
          XVS(N) = XVC(N) - XShadowProjection*ZVC(N)
          YVS(N) = YVC(N) - YShadowProjection*ZVC(N)
        END DO

      END IF

          ! Transform to the homogeneous coordinate system.

      NS3 = LOCHCA + 1
      CALL HTRANS1(NS3,NVS)

          ! Adjust near-duplicate points.

      NVR = HCNV(1)
      DO N = 1, NumVertInShadowOrClippedSurface
        DO M = 1, NVR
          IF (ABS(HCX(M,1)-HCX(N,NS3)) > 6) CYCLE
          IF (ABS(HCY(M,1)-HCY(N,NS3)) > 6) CYCLE
          HCX(N,NS3) = HCX(M,1)
          HCY(N,NS3) = HCY(M,1)
        END DO
      END DO
      CALL HTRANS0(NS3,NumVertInShadowOrClippedSurface)
      IF (.not. CalcSkyDifShading) THEN
        IF (IHOUR /= 0) THEN
          SchValue=LookUpScheduleValue(Surface(GSSNR)%SchedShadowSurfIndex,IHOUR,TS)
        ELSE
          SchValue=Surface(GSSNR)%SchedMinValue
        ENDIF
      ELSE
        SchValue=Surface(GSSNR)%SchedMinValue
      ENDIF

      HCT(NS3)=SchValue

          ! Determine overlap of shadow with receiving surface

      NS1 = 1
      NS2 = NS3
      CALL DeterminePolygonOverlap(NS1,NS2,NS3)
      !  Next statement is special to deal with transmitting shading devices
      IF (OverlapStatus == FirstSurfWithinSecond .and. SchValue > 0.0d0) OverlapStatus=PartialOverlap
      MainOverlapStatus = OverlapStatus
      ExitLoopStatus=MainOverlapStatus

      SELECT CASE (MainOverlapStatus)

        CASE (NoOverlap)  ! No overlap of general surface shadow and receiving surface
          CYCLE

        CASE (FirstSurfWithinSecond,TooManyVertices,TooManyFigures)
          EXIT ShadowingSurfaces

        CASE (SecondSurfWithinFirst,PartialOverlap)
          ! Determine overlaps with previous shadows.
          LOCHCA = NS3
          NGSSHC = LOCHCA - FGSSHC + 1
          IF (NGSSHC <= 1) CYCLE
          CALL MULTOL(LOCHCA,FGSSHC-1,NGSSHC-1)  ! HOYT - Remove this call

        CASE DEFAULT
          EXIT ShadowingSurfaces

      END SELECT

      ExitLoopStatus=-1

    END DO ShadowingSurfaces

          ! Compute sunlit area of surface (excluding effects of subsurfs).

    SELECT CASE (ExitLoopStatus)

      CASE (FirstSurfWithinSecond)    ! Surface fully shaded
        SAREA(HTS) = 0.0d0
        LOCHCA     = FGSSHC

      CASE (TooManyVertices,TooManyFigures)   ! Array limits exceeded, estimate
        SAREA(HTS) = 0.25d0*HCAREA(1)

      CASE DEFAULT

        ! Compute the sunlit area here.
        ! Call UnionShadow(FGSSHC,LOCHCA)

        NGSSHC = LOCHCA - FGSSHC + 1
        IF (NGSSHC <= 0) THEN
          SAREA(HTS) = HCAREA(1)  ! Surface fully sunlit
        ELSE
          A = HCAREA(1)
          DO I = 1, NGSSHC
            A = A + HCAREA(FGSSHC-1+I)*(1.0d0-HCT(FGSSHC-1+I))
          END DO
          SAREA(HTS) = A
          IF (SAREA(HTS) <= 0.0d0) THEN ! Surface fully shaded
            SAREA(HTS)=0.0d0
            LOCHCA=FGSSHC
          END IF
        END IF

    END SELECT

  END IF

  NGSSHC = LOCHCA - FGSSHC + 1

  RETURN

END SUBROUTINE SHDGSS

SUBROUTINE CalcInteriorSolarOverlaps(IHOUR,NBKS,HTSS,GRSNR,TS)

        ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   January 1999
          !       MODIFIED       Nov 2001, FW: include beam radiation overlaps with
          !                       back windows and doors; previously these subsurfaces ignored.
          !                      May 2002, FW: fix problem where reveal was not being considered
          !                       in calculating overlap areas if window is shaded only by reveal.
          !                      June 2002, FW: fix problem that gave incorrect calculation when
          !                       window is not shaded only by reveal
          !                      June 2002, FW: remove incorrect multiplication of overlap areas
          !                       by sunlit fraction when window is shaded only by reveal
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! For an exterior window with surface number HTSS, determines (1) the surface numbers of back
          ! surfaces receiving beam radiation from the window and (2) for each such back surface, the area
          ! of the portion of the window sending beam radiation to the back surface; this is called the
          ! "overlap area."

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! BLAST/IBLAST code, original author George Walton

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: IHOUR  ! Hour Index
  INTEGER, INTENT(IN) :: TS     ! Time step Index
  INTEGER, INTENT(IN) :: HTSS   ! Surface number of the subsurface (exterior window)
  INTEGER, INTENT(IN) :: GRSNR  ! General receiving surface number (base surface of the exterior window)
  INTEGER, INTENT(IN) :: NBKS   ! Number of back surfaces associated with this GRSNR (in general, only
                                !  some of these will receive beam radiation from HTSS this hour)

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: None                       = 0  ! for use with RevealStatus
  INTEGER, PARAMETER :: EntireWindowShadedByReveal = 1  ! for use with RevealStatus
  INTEGER, PARAMETER :: WindowShadedOnlyByReveal   = 2  ! for use with RevealStatus

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER          :: I,J             ! DO loop index
  INTEGER          :: IBKS            ! Back surface DO loop index
  INTEGER          :: JBKS            ! Counter of back surfaces with non-zero overlap with HTSS
  INTEGER          :: JBKSbase        ! Back base surface counter
  INTEGER          :: BackSurfNum     ! Back surface number
  REAL(r64) :: OverlapArea     ! Overlap area (m2)

  LOGICAL        :: UseSimpleDistribution ! TRUE means simple interior solar distribution
                                          ! (all incoming beam assumed to strike floor),
                                          ! FALSE means exact interior solar distribution
                                          ! (track which back surfaces beam illuminates)

  IF (SAREA(HTSS) > 0.0d0) THEN

    UseSimpleDistribution = .FALSE.

    IF ((NBKS <= 0).OR.(Surface(GRSNR)%ExtBoundCond > 0)) THEN

      UseSimpleDistribution = .TRUE.

    ELSE
          ! Using 'exact' distribution, replace subsurface HC entries with reveal HC entries
          ! so that the reveal HC is used in calculating interior solar overlap areas

      ! Adding the following line fixes a problem where, if the window was shaded only
      ! by reveal, then the reveal was not considered in calculating interior solar
      ! overlap areas (FCW 5/3/02).
        !IF(Surface(HTSS)%Reveal > 0.0) NRVLHC = 1
      ! Changing the line to the following avoids incorrect calculation when window is not shaded
      ! only by reveal (FCW 6/28/02).
      IF(WindowRevealStatus(HTSS,IHOUR,TS) == WindowShadedOnlyByReveal) NRVLHC = 1
      IF (NRVLHC > 0) THEN
        DO I = 1, NRVLHC
          HCT(FSBSHC-1+I)    = HCT(FRVLHC-1+I)
          HCNV(FSBSHC-1+I)   = HCNV(FRVLHC-1+I)
          HCAREA(FSBSHC-1+I) = HCAREA(FRVLHC-1+I)
          DO J = 1, MAXHCV
            HCX(J,FSBSHC-1+I) = HCX(J,FRVLHC-1+I)
            HCY(J,FSBSHC-1+I) = HCY(J,FRVLHC-1+I)
            HCA(J,FSBSHC-1+I) = HCA(J,FRVLHC-1+I)
            HCB(J,FSBSHC-1+I) = HCB(J,FRVLHC-1+I)
            HCC(J,FSBSHC-1+I) = HCC(J,FRVLHC-1+I)
          END DO
        END DO
        NSBSHC=NRVLHC
      END IF

    END IF

          ! Check for array space.
    IF (FSBSHC+NBKSHC > MAXHCS) UseSimpleDistribution = .TRUE.

    IF (.NOT.UseSimpleDistribution) THEN    ! Compute overlaps

      FINSHC = FSBSHC + NSBSHC

      JBKS = 0
      JBKSbase = 0

      DO IBKS = 1, NBKSHC  ! Loop over back surfaces to GRSNR this hour. NBKSHC is the number of
                           ! back surfaces that would receive beam radiation from the base surface, GRSNR,
                           ! if the base surface was transparent. In general, some (at least one) or all of these
                           ! will receive beam radiation from the exterior window subsurface, HTSS, of GRSNR,
                           ! depending on the size of HTSS and its location on GRSNR

        BackSurfNum = HCNS(FBKSHC-1+IBKS)

        ! Determine if this back surface number can receive beam radiation from the
        ! exterior window, HTSS, this hour, i.e., overlap area is positive

        LOCHCA  = FINSHC - 1

        CALL MULTOL(FBKSHC-1+IBKS,FSBSHC-1,NSBSHC)

          ! Compute overlap area for this back surface

        NINSHC = LOCHCA - FINSHC + 1
        IF (NINSHC <= 0) CYCLE
        OverlapArea = HCAREA(FINSHC)
        DO J = 2, NINSHC
          OverlapArea = OverlapArea + HCAREA(FINSHC-1+J)*(1.0d0-HCT(FINSHC-1+J))
        END DO

        IF(OverlapArea > 0.001d0) THEN
          JBKS = JBKS + 1
          IF(Surface(BackSurfNum)%BaseSurf == BackSurfNum) JBKSbase = JBKS
          IF(JBKS <= MaxBkSurf) THEN
            BackSurfaces(HTSS,JBKS,IHOUR,TS) = BackSurfNum
            ! Remove following IF check: multiplying by sunlit fraction in the following is incorrect
            ! (FCW, 6/28/02)
              !IF (WindowRevealStatus(HTSS,IHOUR,TS) == WindowShadedOnlyByReveal) THEN
              !  OverlapArea = OverlapArea*(SAREA(HTSS)/Surface(HTSS)%Area)
              !ENDIF
            OverlapAreas(HTSS,JBKS,IHOUR,TS) = OverlapArea*SurfaceWindow(HTSS)%GlazedFrac
            ! If this is a subsurface, subtract its overlap area from the base surface
            IF(Surface(BackSurfNum)%BaseSurf /= BackSurfNum .AND. JBKSbase /= 0) THEN
              OverlapAreas(HTSS,JBKSbase,IHOUR,TS) =  MAX(0.0d0, &
              OverlapAreas(HTSS,JBKSbase,IHOUR,TS)- OverlapAreas(HTSS,JBKS,IHOUR,TS))
            END IF
          END IF
        END IF

      END DO  ! End of loop over back surfaces

    END IF

  END IF  ! End of check that sunlit area > 0.

  RETURN

END SUBROUTINE CalcInteriorSolarOverlaps

SUBROUTINE CalcInteriorSolarDistribution

        ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   January 1999
          !       MODIFIED       Nov 1999, FW, for Window5 calculation method
          !                      Oct 2000, FW: add transmitted solar variables for reporting
          !                      Mar 2001, FW: add new calc of solar absorbed by window shades
          !                      May 2001, FW: add calc of solar transmitted and absorbed by window blinds
          !                      Oct 2001, LL: remove interpolation, solar now at time step
          !                      Oct 2001, FW: add solar transmitted through interior windows
          !                      Mar 24, 2001, FW: remove incorrect multiplication of Boverlap by sunlit fraction
          !                                        since effect of shadowing is already included in Aoverlap
          !                      Apr 2001, FW: add effects of beam solar reflection from outside and inside reveals
          !                      Jan 2003, FW: add between-glass shades and blinds
          !                      Dec 2003, FW: report beam incident on inside of surface
          !                      Jan 2004, FW: for blinds with horizontal slats, allow different diffuse/diffuse
          !                                    transmittance for ground and sky solar
          !                      Apr 2004, FW: allow diffusing glazing
          !                      May 2006, RR: allow external window screen
          !                      Jan 2010, TH: add calculating and reporting of WinBmBmSolar, WinBmDifSolar,
          !                                    WinBmBmSolarEnergy, and WinBmDifSolarEnergy
          !                      Jun 2013, SV: scheduled surface gains for walls and windows
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! For a time step, calculates solar radiation absorbed by exterior
          ! surfaces and interior solar radiation distribution

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
USE ScheduleManager, ONLY: GetCurrentScheduleValue
USE General, ONLY: POLYF, InterpSw, InterpBlind, InterpSlatAng, InterpProfSlatAng, BlindBeamBeamTrans, InterpProfAng
USE DataDaylightingDevices
USE DaylightingDevices, ONLY: FindTDDPipe, TransTDD
USE WindowEquivalentLayer, ONLY: CalcEQLOpticalProperty, CFSDiffAbsTrans
USE DataWindowEquivalentLayer

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER        :: ZoneNum           ! Zone number
INTEGER        :: SurfNum           ! Surface number
INTEGER        :: SurfNum2          ! Secondary surface number for tubular daylighting device diffuser (TDD:DIFFUSER)
INTEGER        :: PipeNum           ! TDD pipe object number
INTEGER        :: ShelfNum          ! Daylighting shelf object number
INTEGER        :: InShelfSurf       ! Inside daylighting shelf surface number
INTEGER        :: OutShelfSurf      ! Outside daylighting shelf surface number
REAL(r64)      :: ShelfSolarRad     ! Shelf diffuse solar radiation
INTEGER        :: BackSurfNum       ! Back surface number
INTEGER        :: IBack             ! Back surface counter
INTEGER        :: RevSurfInd        !Back surface counter value for reversed surfaces
INTEGER        :: KRevSurf          !Additional Back surface counter for reversed surfaces
INTEGER        :: FloorNum          ! Floor surface number
INTEGER        :: AdjSurfNum        ! Adjacent surface number
INTEGER        :: AdjZoneNum        ! Adjacent zone number
REAL(r64)      :: CosTlt            ! Cosine of surface tilt angle
INTEGER        :: ConstrNum         ! Construction number
INTEGER        :: ConstrNumSh       ! Shaded construction number
INTEGER        :: ConstrNumBack     ! Construction number of back surface
INTEGER        :: ConstrNumBackSh   ! Shaded construction number of back surface
INTEGER        :: FlConstrNum       ! Construction number of floor surface
INTEGER        :: ShadeFlag         ! Shading flag for a window
INTEGER        :: ShadeFlagBack     ! Shading flag for a window that is a back surface
INTEGER        :: Lay               ! Glass layer number
REAL(r64)      :: SwitchFac         ! Switching factor for a window
REAL(r64)      :: SwitchFacBack     ! Switching factor for a window that is a back surface
REAL(r64)      :: TransBeamWin      ! Beam solar transmittance of a window
REAL(r64)      :: TransBeamWinSh    ! Beam solar transmittance of a shaded window
REAL(r64)      :: AbsBeamWin(MaxSolidWinLayers) ! Glass layer beam solar absorptance of a window
REAL(r64)      :: AbsBeamWinSh(MaxSolidWinLayers) ! Glass layer beam solar absorptance of a shaded window
REAL(r64)      :: AbsBeamWinEQL(CFSMAXNL+1) ! layers beam solar absorptance of a window
REAL(r64)      :: AbsBeamTotWin     ! Sum of window glass layer beam solar absorptances
REAL(r64)      :: ProfAng           ! Window solar profile angle (radians)
REAL(r64)      :: ProfAngBack       ! Back window solar profile angle (radians)
INTEGER        :: BlNum             ! Blind number
INTEGER        :: ScNum             ! Screen number
INTEGER        :: BlNumBack         ! Back surface blind number
INTEGER        :: ScNumBack         ! Back surface screen number
REAL(r64)      :: TBmBm             ! Beam-beam solar transmittance for bare window or window with switchable glazing
REAL(r64)      :: TBmDif            ! Beam-diffuse solar transmittance for bare window with diffusing glass
REAL(r64)      :: TBlBmDif          ! Beam-diffuse solar transmittance of blind
REAL(r64)      :: TScBmDif          ! Beam-diffuse solar transmittance of screen
REAL(r64)      :: TBlDifDif         ! Diffuse-diffuse solar transmittance of blind
REAL(r64)      :: TScDifDif         ! Diffuse-diffuse solar transmittance of screen
REAL(r64)      :: RhoBlBmDifFr      ! Beam-diffuse front reflectance of blind
REAL(r64)      :: RhoBlBmDifBk      ! Beam-diffuse back reflectance of blind
REAL(r64)      :: RScBmDifBk        ! Beam-diffuse back reflectance of blind
REAL(r64)      :: RhoBlDifDifFr     ! Diffuse-diffuse front refectance of blind
REAL(r64)      :: RhoBlDifDifBk     ! Diffuse-diffuse back refectance of blind
REAL(r64)      :: RScDifDifBk       ! Diffuse-diffuse back refectance of screen
REAL(r64)      :: RGlBmFr           ! Beam front reflectance of glass
REAL(r64)      :: RGlDifFr          ! Diffuse front reflectance of glass
REAL(r64)      :: RGlDifBk          ! Diffuse back reflectance of glass
REAL(r64)      :: TBmBmBl           ! Beam-beam transmittance for window with blind
REAL(r64)      :: TBmBmSc           ! Beam-beam transmittance for window with screen
REAL(r64)      :: TBmAllShBlSc      ! Beam-beam + beam-diffuse transmittance for window with shade, blind, screen,
                                    ! or switchable glazing
REAL(r64)      :: TBmAll            ! Window beam-to-(beam+diffuse) transmittance
REAL(r64)      :: TBm               ! Window beam-beam transmittance
REAL(r64)      :: DifSolarInc       ! Exterior diffuse solar incident on window (W/m2)
REAL(r64)      :: SkySolarTrans     ! Exterior diffuse sky solar transmitted by TDD (W/m2)
REAL(r64)      :: GndSolarTrans     ! Exterior diffuse ground solar transmitted by TDD (W/m2)
REAL(r64)      :: TDifBare          ! Bare diffuse transmittance of exterior window
REAL(r64)      :: TGlDif            ! Bare diffuse transmittance of back window
REAL(r64)      :: TGlBm             ! Glazing system front solar beam transmittance
REAL(r64)      :: TGlBmBack         ! Glazing system back solar beam transmittance
REAL(r64)      :: AGlDiffBack       ! Glass layer back diffuse solar absorptance
REAL(r64)      :: RGlDiffBack       ! Glazing system back diffuse solar reflectance
REAL(r64)      :: AGlDiffFront      ! Glass layer front diffuse solar absorptance
REAL(r64)      :: RGlDiffFront      ! Glazing system front diffuse solar reflectance
REAL(r64)      :: TotReflect        !Total directional-hemispherical solar reflectance of a back surface window
REAL(r64)      :: RhoBlFront        ! Blind solar front beam reflectance
REAL(r64)      :: RhoBlBack         ! Blind solar back beam-diffuse reflectance
REAL(r64)      :: RScBack           ! Screen solar back beam-diffuse reflectance
REAL(r64)      :: RScDifBack        ! Screen solar back diffuse-diffuse reflectance
REAL(r64)      :: AbsBlFront        ! Blind solar front beam absorptance
REAL(r64)      :: AbsScBeam         ! Screen solar beam absorptance
REAL(r64)      :: AbsBlBack         ! Blind solar back beam absorptance
REAL(r64)      :: AbsScBack         ! Screen solar back beam absorptance
REAL(r64)      :: AbsBlDiffFront    ! Blind solar front diffuse absorptance
REAL(r64)      :: AbsBlDiffBack     ! Blind solar back diffuse absorptance
REAL(r64)      :: AbsScDiffBack     ! Screen solar back diffuse absorptance
REAL(r64)      :: ABlBack           ! Blind solar back absorptance for interior solar
REAL(r64)      :: AScBack           ! Screen solar back absorptance for interior solar
REAL(r64)      :: TrSh              ! Shade material solar transmittance
REAL(r64)      :: AbsSh             ! Shade material solar absorptance
REAL(r64)      :: RhoSh             ! Shade material solar reflectance
REAL(r64)      :: AShBack           ! System shade absorptance for interior beam solar
REAL(r64)      :: TBlBmBm           ! Blind solar front beam-beam transmittance
REAL(r64)      :: TScBmBm           ! Screen solar front beam-beam transmittance
REAL(r64)      :: TBlBmBmBack       ! Blind solar back beam-beam transmittance
REAL(r64)      :: TScBmBmBack       ! Screen solar back beam-beam transmittance
REAL(r64)      :: TBlBmDiff         ! Blind solar front beam-diffuse transmittance
REAL(r64)      :: TScBmDiff         ! Screen solar front beam-diffuse transmittance
REAL(r64)      :: TBlBmDiffBack     ! Blind solar back beam-diffuse transmittance
REAL(r64)      :: TScBmDiffBack     ! Screen solar back beam-diffuse transmittance
REAL(r64)      :: RhoBlDiffFront    ! Blind solar front diffuse reflectance
REAL(r64)      :: RhoBlDiffBack     ! Blind solar back diffuse reflectance
REAL(r64)      :: RScDiffBack       ! Screen solar back diffuse reflectance
REAL(r64)      :: RGlFront          ! Glazing system solar front beam-beam reflectance
REAL(r64)      :: RGlBack           ! Glazing system solar back beam-beam reflectance
REAL(r64)      :: BTOTWinZone       ! Transmitted beam solar factor for a window
REAL(r64)      :: BTOTZone          ! (Solar entering a zone as beam or diffuse radiation, originating as beam solar
                                    !  incident on exterior windows)/(Beam normal solar) [W/(W/m2)]
REAL(r64)      :: BTOTZoneSSG       ! Solar entering a zone in case of scheduled surface gains
REAL(r64)      :: AbWin(MaxSolidWinLayers) ! Factor for front beam radiation absorbed in window glass layers
REAL(r64)      :: AbWinBack         ! Factor for back beam radiation absorbed in window glass layers
REAL(r64)      :: AbWinSh(MaxSolidWinLayers) ! Like AbWin, but for shaded window
REAL(r64)      :: AbWinEQL(CFSMAXNL+1) ! Factor for front beam radiation absorbed for equivalent layer window model
REAL(r64)      :: AdWinEQL(CFSMAXNL+1) ! Factor for front diffuse radiation absorbed for equivalent layer window model
REAL(r64)      :: BABSZone          ! Beam radiation from exterior windows absorbed in a zone or transmitted through
REAL(r64)      :: BABSZoneSSG       ! Beam radiation from exterior windows absorbed in a zone (only for scheduled surface gains)
REAL(r64)      :: AOverlap          ! Back surface area irradiated by beam solar from an exterior window,
                                    !   projected onto window plane
REAL(r64)      :: BOverlap          ! AOverlap multiplied by exterior window beam transmittance
                                    ! and cosine of incidence angle
REAL(r64)      :: AbsScreen         ! Exterior screen beam solar absorptance
REAL(r64)      :: AbsShade          ! Interior shade or blind beam solar absorptance
REAL(r64)      :: AbsShadeDiff      ! Interior shade or blind diffuse solar absorptance
REAL(r64)      :: DSZoneWin         ! Factor for sky diffuse solar gain into a zone from an exterior window
REAL(r64)      :: DSZoneWinSh       ! Factor for sky diffuse solar gain into a zone from a shaded exterior window
REAL(r64)      :: DGZoneWin         ! Factor for ground diffuse solar gain into a zone
REAL(r64)      :: DGZoneWinSh       ! Factor for ground diffuse solar gain into a zone from a shaded exterior window
REAL(r64)      :: HMovInsul         ! Conductance of movable wall insulation
REAL(r64)      :: AbsIntSurf, AbsInt ! Interior solar absorptance of opaque surface

REAL(r64)      :: MovInsulSchedVal  ! Value of the movable insulation schedule for current time
REAL(r64)      :: FracSunLit        ! Effective fraction of window that is sunlit;
                                    !  takes shadowing effects of frame and divider into account
REAL(r64)      :: SunLitFract       ! Sunlit fraction w/o shadowing effects of frame and divider
REAL(r64)      :: InOutProjSLFracMult ! = SurfaceWindow(SurfNum)%InOutProjSLFracMult(HourOfDay)
REAL(r64)      :: CosInc,CosIncBack ! Incidence angle of beam solar radiation on window
REAL(r64)      :: SlatAng,SlatAngBack   ! Slat angle this time step for window with blind on (deg)
LOGICAL        :: VarSlats,VarSlatsBack ! True if variable slat angle
REAL(r64)      :: ADiffWin(5)       ! Diffuse solar absorptance of glass layers, bare window
REAL(r64)      :: ADiffWinSh(5)     ! Diffuse solar absorptance of glass layers, window with shading device
REAL(r64)      :: DiffTrans         ! Glazing diffuse solar transmittance (including shade/blind/switching, if present)
REAL(r64)      :: DiffTransGnd      ! Ground diffuse solar transmittance for glazing with blind with horiz. slats or complex fen
REAL(r64)      :: DiffTransBmGnd   !Complex fen: diffuse solar transmittance for ground-reflected beam radiation
REAL(r64)      :: DiffTransSky      ! Sky diffuse solar transmittance for glazing with blind with horiz. slats or complex fen
REAL(r64)      :: NomDiffTrans     !
INTEGER        :: BaseSurfNum       ! Base surface number
REAL(r64)      :: t1,t2,t3          ! Bare-glass beam solar transmittance for glass layers 1,2 and 3
REAL(r64)      :: t1t2              ! t1*t2
REAL(r64)      :: af1,af2,af3       ! Bare-glass beam solar front absorptance for glass layers 1,2 and 3
REAL(r64)      :: ab1,ab2,ab3       ! Bare-glass beam solar back absorptance for glass layers 1,2 and 3
REAL(r64)      :: rf1,rf2,rf3       ! Bare-glass beam solar front reflectance for glass layers 1,2 and 3
REAL(r64)      :: rb1,rb2,rb3       ! Bare-glass beam solar back reflectance for glass layers 1,2 and 3
REAL(r64)      :: td1,td2,td3       ! Bare-glass diffuse solar transmittance for glass layers 1,2 and 3
REAL(r64)      :: td1td2            ! td1*td2
REAL(r64)      :: afd1,afd2,afd3    ! Bare-glass diffuse solar front absorptance for glass layers 1,2 and 3
REAL(r64)      :: abd1,abd2,abd3    ! Bare-glass diffuse solar back absorptance for glass layers 1,2 and 3
REAL(r64)      :: rfd1,rfd2,rfd3    ! Bare-glass diffuse solar front reflectance for glass layers 1,2 and 3
REAL(r64)      :: rbd1,rbd2,rbd3    ! Bare-glass diffuse solar back reflectance for glass layers 1,2 and 3
REAL(r64)      :: tfshBB,tbshBB     ! Bare-blind front and back beam-beam solar transmittance
REAL(r64)      :: tfshBd,tbshBd     ! Bare-blind front and back beam-diffuse solar transmittance
REAL(r64)      :: tfshd,tbshd       ! Bare-blind front and back diffuse-diffuse solar transmittance
REAL(r64)      :: afshB,abshB       ! Bare-blind front and back beam solar absorptance
REAL(r64)      :: afshd,abshd       ! Bare-blind front and back diffuse solar absorptance
REAL(r64)      :: rfshB,rbshB       ! Bare-blind front and back beam solar reflectance
REAL(r64)      :: rfshd,rbshd       ! Bare-blind front and back diffuse solar reflectance
REAL(r64)      :: t1k,t2k,t3k       ! Back surface bare-glass beam solar transmittance for glass layers 1,2,3
REAL(r64)      :: af2k,af3k         ! Back surface bare-glass beam solar front absorptance for glass layers 2 and 3
REAL(r64)      :: ab1k,ab2k,ab3k    ! Back surface bare-glass beam solar back absorptance for glass layers 1,2 and 3
REAL(r64)      :: rb1k,rb2k         ! Back surface bare-glass beam solar back reflectance for glass layers 1,2
REAL(r64)      :: td1k,td2k         ! Back surface bare-glass beam diffuse solar transmittance for glass layers 1,2
REAL(r64)      :: afd2k,afd3k       ! Back surface bare-glass diffuse solar front absorptance for glass layer 2 and 3
REAL(r64)      :: abd1k,abd2k       ! Back surface bare-glass diffuse solar back absorptance for glass layer 1 and 2
REAL(r64)      :: rfd2k,rfd3k       ! Back surface bare-glass diffuse solar front reflectance for glass layer 2 and 3
REAL(r64)      :: rbd1k,rbd2k       ! Back surface bare-glass diffuse solar back reflectance for glass layer 1 and 2
REAL(r64)      :: tfshBBk,tbshBBk   ! Back surface bare-blind beam-beam solar front and back transmittance
REAL(r64)      :: tfshBdk,tbshBdk   ! Back surface bare-blind beam-diffuse solar front and back transmittance
REAL(r64)      :: tfshdk,tbshdk     ! Back surface bare-blind diffuse-diffuse solar front and back transmittance
REAL(r64)      :: rfshBk,rbshBk     ! Back surface bare-blind beam solar front, back reflectance
REAL(r64)      :: rfshdk,rbshdk     ! Back surface bare-blind diffuse solar front, back reflectance
REAL(r64)      :: afshBk,abshBk     ! Back surface bare-blind beam solar front, back absorptance
REAL(r64)      :: afshdk,abshdk     ! Back surface bare-blind diffuse solar front, back absorptance
INTEGER        :: NGlass            ! Number of glass layers in a construction
INTEGER        :: NBackGlass        ! Number of glass layers in the "back" construction
REAL(r64)      :: SkySolarInc       ! Incident solar radiation on a window: sky diffuse plus beam
                                    !   reflected from obstruction (W/m2)
REAL(r64)      :: GndSolarInc       ! Incident solar radiation on a window from the ground (W/m2)
REAL(r64)      :: SkyGndTrans       ! complex fen: transmitted ground-reflected sky radiation (W/m2)
REAL(r64)      :: BmGndTrans        ! complex fen: transmitted ground-reflected beam radiation (W/m2)

REAL(r64), SAVE,ALLOCATABLE, DIMENSION(:) :: ExtBeamAbsByShadFac  ! Factor for exterior beam radiation absorbed by shade
                                                             ! (1/m2) (absorbed radation = beam incident * ExtBeamAbsByShad
REAL(r64), SAVE,ALLOCATABLE, DIMENSION(:) :: IntBeamAbsByShadFac  ! Like ExtBeamAbsByShadFac, but for interior beam radiation.
REAL(r64), SAVE,ALLOCATABLE, DIMENSION(:) :: WinTransBmSolar   ! Factor for exterior beam solar transmitted through window,
                                                          ! or window plus shade, into zone at current time (m2)
REAL(r64), SAVE,ALLOCATABLE, DIMENSION(:) :: WinTransDifSolar  ! Factor for exterior diffuse solar transmitted through window,
                                                          ! or window plus shade, into zone at current time (m2)

REAL(r64), SAVE,ALLOCATABLE, DIMENSION(:) :: WinTransDifSolarGnd  ! Factor for exterior ground diffuse solar transmitted through
                                                          ! window with horizontally-slatted blind into zone at current time (m2)
REAL(r64), SAVE,ALLOCATABLE, DIMENSION(:) :: WinTransDifSolarSky  ! Factor for exterior sky diffuse solar transmitted through
                                                          ! window with horizontally-slatted blind into zone at current time (m2)
LOGICAL,SAVE :: MustAlloc=.true.  ! True when local arrays must be allocated
REAL(r64) :: TBmDenom  ! TBmDenominator

REAL(r64) :: TBmBmShBlSc            ! Beam-beam transmittance for window with shade, blind, screen, or switchable glazing
REAL(r64) :: TBmDifShBlSc           ! Beam-diffuse transmittance for window with shade, blind, screen, or switchable glazing
REAL(r64) :: WinTransBmBmSolar      ! Factor for exterior beam to beam solar transmitted through window,
                                    !  or window plus shade, into zone at current time (m2)
REAL(r64) :: WinTransBmDifSolar     ! Factor for exterior beam to diffuse solar transmitted through window,
                                    !  or window plus shade, into zone at current time (m2)

REAL(r64) :: TBmBmEQL               ! Beam-beam solar transmittance for equivalent layer model window W/WO shade
REAL(r64) :: TBmDiffEQL             ! Beam-diffuse solar transmittance for equivalent layer model window W/WO shade

! Variables for complex fenestration
INTEGER :: CurCplxFenState  ! Current state for complex fenestration
INTEGER :: CurBackState ! Current state for back surface if that surface is complex fenestration
INTEGER :: CurTrnDir  ! Current back window surface BSDF direction
INTEGER :: CurBackDir ! current hit direction to complex fenestration
INTEGER :: IBm  ! Incoming direction of the Sun (for window BSDF)
INTEGER :: IConst ! Current surface construction number (it depends of state too)
INTEGER :: NBkSurf ! Number of back surfaces
INTEGER :: BaseSurf ! Base surface number for current complex window
INTEGER :: BackSurfaceNumber ! Back surface number
REAL(r64), ALLOCATABLE, DIMENSION(:) :: CFBoverlap  ! Sum of boverlap for each back surface
REAL(r64), ALLOCATABLE, DIMENSION(:, :) :: CFDirBoverlap ! Directional boverlap (Direction, IBack)
REAL(r64) :: CurLambda ! Current lambda value in BSDF outgoing directions
REAL(r64) :: DirTrans ! Current BSDF directional transmittance
                      ! (for incoming I and outgoing J directions)
REAL(r64) :: bestDot  ! complex fenestration hits other complex fenestration, it is important to find
                      ! matching beam directions.  Beam leving one window will have certaing number for it's basis
                      ! while same beam reaching back surface will have different beam number.  This value is used
                      ! to keep best matching dot product for those directions
REAL(r64) :: curDot   ! temporary variable for current dot product
INTEGER :: bestTrn    ! Direction corresponding best dot product for master window
INTEGER :: bestBackTrn ! Direction corresponding best dot product for back surface window
INTEGER :: TotSolidLay ! Number of window solid layers
REAL(r64) :: tempVec1(3) ! temporary vector for performing dot_product
REAL(r64) :: tempVec2(3) ! temporary vector for performing dot_product

REAL(r64) :: AbsSolBeamEQL(CFSMAXNL+1,2)     ! absorbed exterior beam radiation by layers fraction
REAL(r64) :: AbsSolDiffEQL(CFSMAXNL+1,2)     ! absorbed exterior diffuse radiation by layers fraction
INTEGER   :: EQLNum                          ! equivalent layer fenestration index
REAL(r64) :: AbsSolBeamBackEQL(CFSMAXNL+1,2) ! absorbed interior beam radiation by layers fraction from back
REAL(r64) :: AbsSolDiffBackEQL(CFSMAXNL+1,2) ! absorbed exterior diffuse radiation by layers fraction from back

! scheduled surface gains local variables
INTEGER :: FenSolAbsPtr
INTEGER :: SurfSolIncPtr
INTEGER :: iSSG  ! scheduled surface gains counter
REAL(r64) :: SolarIntoZone ! Solar radiation into zone to current surface

IF (MustAlloc) THEN
  ALLOCATE (DBZoneIntWin(NumOfZones))
  ALLOCATE (IntBeamAbsByShadFac(TotSurfaces))
  ALLOCATE (ExtBeamAbsByShadFac(TotSurfaces))
  ALLOCATE (WinTransBmSolar(TotSurfaces))
  ALLOCATE (WinTransDifSolar(TotSurfaces))
  ALLOCATE (WinTransDifSolarGnd(TotSurfaces))
  ALLOCATE (WinTransDifSolarSky(TotSurfaces))
  MustAlloc=.false.
ENDIF

#ifdef EP_Count_Calls
NumIntSolarDist_Calls=NumIntSolarDist_Calls+1
#endif

DSZone = 0.0d0
DGZone = 0.0d0
DBZone = 0.0d0
DBZoneSSG = 0.0d0
DBZoneIntWin = 0.0d0
AISurf  = 0.0d0
AOSurf  = 0.0d0
ABWin  = 0.0d0
ABWinSh= 0.0d0
AWinSurf = 0.0d0
WinTransBmSolar = 0.0d0
WinTransDifSolar = 0.0d0
WinTransDifSolarGnd = 0.0d0
WinTransDifSolarSky = 0.0d0
WinBmSolar = 0.0d0
WinBmBmSolar = 0.0d0
WinBmDifSolar = 0.0d0
WinTransBmBmSolar = 0.0d0
WinTransBmDifSolar = 0.0d0
TBmBm = 0.0d0
TBmDif = 0.0d0
TBmBmEQL = 0.0d0
TBmDiffEQL = 0.0d0

WinDifSolar = 0.0d0
ZoneTransSolar = 0.0d0
ZoneBmSolFrExtWinsRep = 0.0d0
ZoneBmSolFrIntWinsRep = 0.0d0
ZoneDifSolFrExtWinsRep = 0.0d0
ZoneDifSolFrIntWinsRep = 0.0d0
IntBeamAbsByShadFac=0.0d0
ExtBeamAbsByShadFac=0.0d0
SurfaceWindow%BmSolTransThruIntWinRep=0.0d0
!energy
WinBmSolarEnergy = 0.0d0
WinBmBmSolarEnergy=0.0d0
WinBmDifSolarEnergy=0.0d0

WinDifSolarEnergy = 0.0d0
ZoneTransSolarEnergy = 0.0d0
ZoneBmSolFrExtWinsRepEnergy = 0.0d0
ZoneBmSolFrIntWinsRepEnergy = 0.0d0
ZoneDifSolFrExtWinsRepEnergy = 0.0d0
ZoneDifSolFrIntWinsRepEnergy = 0.0d0
SurfaceWindow%BmSolTransThruIntWinRepEnergy=0.0d0

DO ZoneNum = 1, NumOfZones

  BTOTZone = 0.0d0
  BABSZone = 0.0d0

        ! Loop over exterior surfaces in this zone

  DO SurfNum = Zone(ZoneNum)%SurfaceFirst, Zone(ZoneNum)%SurfaceLast
    IF ( ((Surface(SurfNum)%ExtBoundCond /= ExternalEnvironment) .AND. &
          (Surface(SurfNum)%ExtBoundCond /= OtherSideCondModeledExt) ) &
         .AND. SurfaceWindow(SurfNum)%OriginalClass /= SurfaceClass_TDD_Diffuser) CYCLE
    IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE
    ! TH added 3/24/2010 while debugging CR 7872
    IF (.NOT. Surface(SurfNum)%ExtSolar) CYCLE
    ConstrNum   = Surface(SurfNum)%Construction
    ConstrNumSh = SurfaceWindow(SurfNum)%ShadedConstruction
    IF(SurfaceWindow(SurfNum)%StormWinFlag==1) THEN
      ConstrNum   = Surface(SurfNum)%StormWinConstruction
      ConstrNumSh = Surface(SurfNum)%StormWinShadedConstruction
    END IF
    BlNum       = SurfaceWindow(SurfNum)%BlindNumber
    ScNum       = SurfaceWindow(SurfNum)%ScreenNumber
    ShadeFlag   = SurfaceWindow(SurfNum)%ShadingFlag  ! Set in subr. WindowShadingManager
    ProfAng     = 0.0d0
    IF (ShadeFlag /= ExtScreenOn .and. BlNum > 0) CALL ProfileAngle(SurfNum,SOLCOS,Blind(BlNum)%SlatOrientation,ProfAng)
    SlatAng     = SurfaceWindow(SurfNum)%SlatAngThisTS
    VarSlats    = SurfaceWindow(SurfNum)%MovableSlats

    IF (SurfaceWindow(SurfNum)%OriginalClass == SurfaceClass_TDD_Diffuser) THEN
      PipeNum = FindTDDPipe(SurfNum)
      SurfNum2 = TDDPipe(PipeNum)%Dome
    ELSE
      SurfNum2 = SurfNum
    END IF

    ShelfNum = Surface(SurfNum)%Shelf
    IF (ShelfNum > 0) THEN ! Daylighting shelf
      InShelfSurf = Shelf(ShelfNum)%InSurf
      OutShelfSurf = Shelf(ShelfNum)%OutSurf
    ELSE
      InShelfSurf = 0
      OutShelfSurf = 0
    END IF

    CosInc = CosIncAng(SurfNum2,HourOfDay,TimeStep)
    SunLitFract = SunLitFrac(SurfNum2,HourOfDay,TimeStep)

        !-------------------------------------------------------------------------
        ! EXTERIOR BEAM SOLAR RADIATION ABSORBED ON THE OUTSIDE OF OPAQUE SURFACES
        !-------------------------------------------------------------------------

    IF(SunLitFract > 0.0d0 .AND. Construct(ConstrNum)%TransDiff <= 0.0d0) THEN
      AOSurf(SurfNum) = Construct(ConstrNum)%OutsideAbsorpSolar * CosInc * SunLitFract

      ! Note: movable insulation, if present, is accounted for in subr. InitIntSolarDistribution,
      ! where QRadSWOutMvIns is calculated from QRadSWOutAbs and insulation solar absorptance

    END IF

        !-------------------------------------------------------------------------------------------
        ! EXTERIOR BEAM AND DIFFUSE SOLAR RADIATION ABSORBED IN THE GLASS LAYERS OF EXTERIOR WINDOWS
        !-------------------------------------------------------------------------------------------

    IF(Surface(SurfNum)%Class /= SurfaceClass_Window .AND. Surface(SurfNum)%Class /= SurfaceClass_TDD_Dome) CYCLE

    ! Somewhat of a kludge
    IF(Surface(SurfNum)%Class == SurfaceClass_TDD_Dome .OR. SurfaceWindow(SurfNum)%OriginalClass == SurfaceClass_TDD_Diffuser) &
      SunlitFracWithoutReveal(SurfNum,HourOfDay,TimeStep) = SunLitFract ! Frames/dividers not allowed

    WinTransBmBmSolar = 0.0d0
    WinTransBmDifSolar = 0.0d0

    InOutProjSLFracMult = SurfaceWindow(SurfNum)%InOutProjSLFracMult(HourOfDay)
    IF(SunlitFracWithoutReveal(SurfNum,HourOfDay,TimeStep) > 0.0d0) THEN

      IF (SurfaceWindow(SurfNum)%WindowModelType /= WindowBSDFModel .AND. &
          SurfaceWindow(SurfNum)%WindowModelType /= WindowEQLModel) THEN

        ! For bare glazing or switchable glazing, the following includes the effects of
        ! (1) diffuse solar produced by beam solar incident on the outside and inside reveal
        ! surfaces, and (2) absorption of beam solar by outside and inside reveal surfaces.
        ! If there is an exterior shade/blind both of these effects are ignored. If there
        ! is an interior or between-glass shade/blind the effects of beam incident on
        ! inside reveal surfaces is ignored.

        NGlass = Construct(ConstrNum)%TotGlassLayers

        DO Lay = 1,NGlass
          ABWin(Lay)    = POLYF(CosInc,Construct(ConstrNum)%AbsBeamCoef(Lay,1:6)) * &
                            CosInc * SunLitFract * SurfaceWindow(SurfNum)%OutProjSLFracMult(HourOfDay)
          ADiffWin(Lay) = Construct(ConstrNum)%AbsDiff(Lay)
          IF(ShadeFlag <= 0 .OR. ShadeFlag >= 10) THEN

                                   ! Bare window (ShadeFlag = -1 or 0 or shading device of off)

            AWinSurf(SurfNum,Lay) = ABWin(Lay) &
               ! Add contribution of beam reflected from outside and inside reveal
              + SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing * Construct(ConstrNum)%AbsDiff(Lay) &
              + SurfaceWindow(SurfNum)%InsRevealDiffOntoGlazing * Construct(ConstrNum)%AbsDiffBack(Lay)

          ELSE

                                   ! Shade, screen, blind or switchable glazing on (ShadeFlag > 0)

            FracSunLit = SunLitFract*SurfaceWindow(SurfNum)%OutProjSLFracMult(HourOfDay)
            IF(ShadeFlag==ExtShadeOn .OR. ShadeFlag==ExtBlindOn .OR. ShadeFlag==ExtScreenOn) FracSunLit = SunLitFract
            IF(ShadeFlag==IntShadeOn.OR.ShadeFlag==ExtShadeOn.OR.ShadeFlag==BGShadeOn .OR. ShadeFlag==SwitchableGlazing) THEN

                                   ! Shade or switchable glazing on

              ABWinSh(Lay) = POLYF(CosInc,Construct(ConstrNumSh)%AbsBeamCoef(Lay,1:6)) * CosInc * FracSunLit

              ADiffWinSh(Lay) = Construct(ConstrNumSh)%AbsDiff(Lay)

            ELSE
                                   ! Blind or screen on

              IF(Lay == 1 .AND. ShadeFlag/=ExtScreenOn) CALL ProfileAngle(SurfNum,SOLCOS,Blind(BlNum)%SlatOrientation,ProfAng)

              IF(ShadeFlag == IntBlindOn) THEN

                                   ! Interior blind on
                IF(Lay==1) THEN
                  TGlBm          = POLYF(CosInc,Construct(ConstrNum)%TransSolBeamCoef(1:6))
                  RGlDiffBack    = Construct(ConstrNum)%ReflectSolDiffBack
                  RhoBlFront     = InterpProfSlatAng(ProfAng,SlatAng,VarSlats,Blind(BlNum)%SolFrontBeamDiffRefl)
                  RhoBlDiffFront = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolFrontDiffDiffRefl)
                END IF
                AGlDiffBack     = Construct(ConstrNum)%AbsDiffBack(Lay)
                ABWinSh(Lay)    = AbWin(Lay) + (TGlBm*AGlDiffBack*RhoBlFront/(1.d0-RhoBlFront*RGlDiffBack))* &
                  CosInc * FracSunLit
                ADiffWinSh(Lay) = ADiffWin(Lay) + Construct(ConstrNum)%TransDiff*AGlDiffBack*RhoBlDiffFront/ &
                                                      (1.d0-RhoBlDiffFront*RGlDiffBack)
              ELSE IF(ShadeFlag == ExtBlindOn) THEN

                                   ! Exterior blind on
                IF(Lay==1) THEN
                  TBlBmBm       = BlindBeamBeamTrans(ProfAng,SlatAng,Blind(BlNum)%SlatWidth, &
                                               Blind(BlNum)%SlatSeparation,Blind(BlNum)%SlatThickness)
                  TBlBmDiff     = InterpProfSlatAng(ProfAng,SlatAng,VarSlats,Blind(BlNum)%SolFrontBeamDiffTrans)
                  RhoBlBack     = InterpProfSlatAng(ProfAng,SlatAng,VarSlats,Blind(BlNum)%SolBackBeamDiffRefl)
                  RhoBlDiffBack = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolBackDiffDiffRefl)
                  RGlFront      =  POLYF(CosInc,Construct(ConstrNum)%ReflSolBeamFrontCoef(1:6))
                  RGlDiffFront  = Construct(ConstrNum)%ReflectSolDiffFront
                  TBlDifDif     = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolFrontDiffDiffTrans)
                  RGlDifFr      = Construct(ConstrNum)%ReflectSolDiffFront
                  RhoBlDifDifBk = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolBackDiffDiffRefl)
                END IF
                AGlDiffFront = Construct(ConstrNum)%AbsDiff(Lay)
                ABWinSh(Lay) = TBlBmBm*ABWin(Lay) + ((TBlBmBm*RGlFront*RhoBlBack + TBlBmDiff) * AGlDiffFront / &
                  (1 - RGlDiffFront*RhoBlDiffBack)) * CosInc * FracSunLit
                !ADiffWinSh(Lay) = 0.0  ! Assumes no contribution from reveal reflection when exterior blind in place
                !  Replaced above line with (FCW, 2/10/03):
                ADiffWinSh(Lay) = ADiffWin(Lay) * TBlDifDif/(1.d0-RGlDifFr*RhoBlDifDifBk)

              ELSE IF(ShadeFlag == ExtScreenOn) THEN

                                   ! Exterior screen on
                IF(Lay==1) THEN
                  TScBmBm       = SurfaceScreens(ScNum)%BmBmTrans
                  TScBmDiff     = SurfaceScreens(ScNum)%BmDifTrans
                  RScBack       = SurfaceScreens(ScNum)%ReflectSolBeamFront
                  RScDifBack   = SurfaceScreens(ScNum)%DifReflect
                  RGlFront      = POLYF(CosInc,Construct(ConstrNum)%ReflSolBeamFrontCoef(1:6))
                  RGlDiffFront  = Construct(ConstrNum)%ReflectSolDiffFront
                  TScDifDif     = SurfaceScreens(ScNum)%DifDifTrans
                  RGlDifFr      = Construct(ConstrNum)%ReflectSolDiffFront
                END IF
                AGlDiffFront = Construct(ConstrNum)%AbsDiff(Lay)

  !             Reduce the bare window absorbed beam by the screen beam transmittance and then account for interreflections
                ABWinSh(Lay) = TScBmBm*ABWin(Lay) + (TScBmBm*RGlFront*RScBack + TScBmDiff) *  &
                                  Construct(ConstrNum)%AbsDiff(Lay)/(1.d0-RGlDiffFront*RScDifBack) * CosInc * FracSunLit

                ADiffWinSh(Lay) = ADiffWin(Lay) * TScDifDif/(1.d0-RGlDifFr*RScDifBack)

              ELSE
                                   ! Between-glass blind on

                ! Isolated glass and blind properties at current incidence angle, profile angle and slat angle
                IF(Lay==1) THEN
                  t1     = POLYF(CosInc,Construct(ConstrNum)%tBareSolCoef(1,1:6))
                  t2     = POLYF(CosInc,Construct(ConstrNum)%tBareSolCoef(2,1:6))
                  af1    = POLYF(CosInc,Construct(ConstrNum)%afBareSolCoef(1,1:6))
                  af2    = POLYF(CosInc,Construct(ConstrNum)%afBareSolCoef(2,1:6))
                  ab1    = POLYF(CosInc,Construct(ConstrNum)%abBareSolCoef(1,1:6))
                  ab2    = POLYF(CosInc,Construct(ConstrNum)%abBareSolCoef(2,1:6))
                  rf1    = POLYF(CosInc,Construct(ConstrNum)%rfBareSolCoef(1,1:6))
                  rf2    = POLYF(CosInc,Construct(ConstrNum)%rfBareSolCoef(2,1:6))
                  rb1    = POLYF(CosInc,Construct(ConstrNum)%rbBareSolCoef(1,1:6))
                  rb2    = POLYF(CosInc,Construct(ConstrNum)%rbBareSolCoef(2,1:6))
                  td1    = Construct(ConstrNum)%tBareSolDiff(1)
                  td2    = Construct(ConstrNum)%tBareSolDiff(2)
                  afd1   = Construct(ConstrNum)%afBareSolDiff(1)
                  afd2   = Construct(ConstrNum)%afBareSolDiff(2)
                  abd1   = Construct(ConstrNum)%abBareSolDiff(1)
                  abd2   = Construct(ConstrNum)%abBareSolDiff(2)
                  rfd1   = Construct(ConstrNum)%rfBareSolDiff(1)
                  rfd2   = Construct(ConstrNum)%rfBareSolDiff(2)
                  rbd1   = Construct(ConstrNum)%rbBareSolDiff(1)
                  rbd2   = Construct(ConstrNum)%rbBareSolDiff(2)
                  tfshBB = BlindBeamBeamTrans(ProfAng,SlatAng,Blind(BlNum)%SlatWidth, &
                                    Blind(BlNum)%SlatSeparation,Blind(BlNum)%SlatThickness)
                  tfshBd = InterpProfSlatAng(ProfAng,SlatAng,VarSlats,Blind(BlNum)%SolFrontBeamDiffTrans)
                  tfshd  = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolFrontDiffDiffTrans)
                  tbshBB = BlindBeamBeamTrans(ProfAng,PI-SlatAng,Blind(BlNum)%SlatWidth, &
                                    Blind(BlNum)%SlatSeparation,Blind(BlNum)%SlatThickness)
                  tbshBd = InterpProfSlatAng(ProfAng,SlatAng,VarSlats,Blind(BlNum)%SolBackBeamDiffTrans)
                  tbshd  = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolBackDiffDiffTrans)
                  afshB  = InterpProfSlatAng(ProfAng,SlatAng,VarSlats,Blind(BlNum)%SolFrontBeamAbs)
                  abshB  = InterpProfSlatAng(ProfAng,SlatAng,VarSlats,Blind(BlNum)%SolBackBeamAbs)
                  afshd  = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolFrontDiffAbs)
                  abshd  = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolBackDiffAbs)
                  rfshB  = InterpProfSlatAng(ProfAng,SlatAng,VarSlats,Blind(BlNum)%SolFrontBeamDiffRefl)
                  rbshB  = InterpProfSlatAng(ProfAng,SlatAng,VarSlats,Blind(BlNum)%SolBackBeamDiffRefl)
                  rfshd  = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolFrontDiffDiffRefl)
                  rbshd  = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolBackDiffDiffRefl)
                END IF

                IF(Lay==1.AND.NGlass==3) THEN
                  t1t2 = t1*t2
                  td1td2 = td1*td2
                  t3   = POLYF(CosInc,Construct(ConstrNum)%tBareSolCoef(3,1:6))
                  af3  = POLYF(CosInc,Construct(ConstrNum)%afBareSolCoef(3,1:6))
                  ab3  = POLYF(CosInc,Construct(ConstrNum)%abBareSolCoef(3,1:6))
                  rf3  = POLYF(CosInc,Construct(ConstrNum)%rfBareSolCoef(3,1:6))
                  rb3  = POLYF(CosInc,Construct(ConstrNum)%rbBareSolCoef(3,1:6))
                  td3  = Construct(ConstrNum)%tBareSolDiff(3)
                  afd3 = Construct(ConstrNum)%afBareSolDiff(3)
                  abd3 = Construct(ConstrNum)%abBareSolDiff(3)
                  rfd3 = Construct(ConstrNum)%rfBareSolDiff(3)
                  rbd3 = Construct(ConstrNum)%rbBareSolDiff(3)
                END IF

                IF(NGlass==2) THEN
                  IF(Lay==1) THEN
                    ABWinSh(1)    = CosInc * FracSunLit * (af1 + t1*tfshBB*rf2*tbshBB*ab1 + &
                         t1*(rfshB + rfshB*rbd1*rfshd + tfshBB*rf2*tbshBd + tfshBd*rfd2*tbshd)*abd1)
                    ADiffWinSh(1) = afd1 + td1*(rfshd + rfshd*rbd1*rfshd + tfshd*rfd2*tbshd)*abd1
                  ELSE IF (Lay==2) THEN
                    ABWinSh(2)    = CosInc * FracSunLit * (t1*rfshB*af2 + &
                         t1*(rfshB*rf2*rbshd + tfshBd*(1+rfd2*rbshd) + rfshB*rbd1*tfshd)*afd2)
                    ADiffWinSh(2) = td1*(tfshd*(1+rfd2*rbshd) + rfshd*rbd1*tfshd)*afd2
                  END IF
                END IF  ! End of check if NGlass = 2

                IF(NGlass==3) THEN
                  IF(Lay==1) THEN
                    ABWinSh(1)    = CosInc * FracSunLit * (af1 + t1*rf2*ab1 + t1t2*tfshBB*rf3*tbshBB*t2*ab1 + &
                           t1t2*(rfshB*td2 + rfshB*rbd2*rfshd*td2 + tfshBd*rfd3*tbshd*td2)*abd1)
                    ADiffWinSh(1) = afd1 + td1*rbd2*abd1 + &
                           td1td2*(rfshd*(1 + rbd2*rfshd + td2*rbd1*td2*rfshd) + &
                           tfshd*(rfd3*tbshd + rfd3*rbshd*rfd3*tbshd))*td2*abd1
                  ELSE IF(Lay==2) THEN
                    ABWinSh(2)    = CosInc * FracSunLit * (t1*af2 + t1t2*(tfshBB*rf3*tbshBB*ab2 + rfshB*td2*rbd1*afd2) + &
                           t1t2*(rfshB*(1+rbd2*rfshd) + tfshBB*rf3*tbshBd + tfshBd*rfd3*tbshd)*abd2)
                    ADiffWinSh(2) = td1*afd2 + td1td2*rfshd*td2*rbd1*afd2 + &
                           td1td2*(rfshd*(1+rbd2*rfshd) + tfshd*rfd3*tbshd)*abd2
                  ELSE IF(Lay==3) THEN
                    ABWinSh(3) = CosInc * FracSunLit * (t1t2*tfshBB*af3 + &
                           t1t2*(tfshBB*rf3*rbshB + tfshBd*(1+rfd3*rbshd) + rfshB*(rbd2*tfshd + td2*rbd1*td2*tfshd))*afd3)
                    ADiffWinSh(3) = td1td2*(tfshd*(1+rfd3*rbshd) + rfshd*(rbd2*tfshd + td2*rbd1*td2*tfshd))*afd3
                  END IF
                END IF  ! End of check if NGlass = 3

              END IF  ! End of check if blind is interior, exterior or between-glass
            END IF  ! End of check if a blind is on

            IF(ShadeFlag /= SwitchableGlazing) THEN

                                    ! Interior or between glass shade or blind on

              AWinSurf(SurfNum,Lay) = ABWinSh(Lay)
              IF(ShadeFlag==IntShadeOn.OR.ShadeFlag==IntBlindOn.OR.ShadeFlag==BGShadeOn.OR.ShadeFlag==BGBlindOn) &
                  ! Add contribution of diffuse from beam on outside reveal
                 AWinSurf(SurfNum,Lay) = AWinSurf(SurfNum,Lay) +  &
                      ADiffWinSh(Lay) * SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing

            ELSE
                                    ! Switchable glazing

              SwitchFac = SurfaceWindow(SurfNum)%SwitchingFactor
              AWinSurf(SurfNum,Lay) = InterpSw(SwitchFac,ABWin(Lay),ABWinSh(Lay))
                 ! Add contribution of diffuse from beam on outside and inside reveal
              AWinSurf(SurfNum,Lay) = AWinSurf(SurfNum,Lay) + &
                  InterpSW(SwitchFac,ADiffWin(Lay),ADiffWinSh(Lay)) &
                    * SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing + &
                  InterpSW(SwitchFac,Construct(ConstrNum)%AbsDiffBack(Lay),Construct(ConstrNumSh)%AbsDiffBack(Lay)) &
                    * SurfaceWindow(SurfNum)%InsRevealDiffOntoGlazing
            END IF
          END IF  ! End of check if window has shading device
        END DO  ! End of loop over window glass layers

            !-----------------------------------------
            ! EXTERIOR BEAM ABSORBED BY SHADING DEVICE
            !-----------------------------------------

            ! Exterior beam absorbed by INTERIOR SHADE

        IF(ShadeFlag == IntShadeOn) THEN
          ! Note that AbsBeamShadeCoef includes effect of shade/glazing inter-reflection
          AbsShade = POLYF(CosInc,Construct(ConstrNumSh)%AbsBeamShadeCoef(1:6))
          ExtBeamAbsByShadFac(SurfNum) = ( AbsShade * CosInc * SunLitFract * InOutProjSLFracMult &

             + SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing * Construct(ConstrNumSh)%AbsDiffShade ) * &
                  SurfaceWindow(SurfNum)%GlazedFrac
          ! In the above, GlazedFrac corrects for shadowing of divider onto interior shade
        END IF

            ! Exterior beam absorbed by EXTERIOR SHADE

        IF(ShadeFlag == ExtShadeOn) THEN
          ExtBeamAbsByShadFac(SurfNum) =  &
                Construct(ConstrNumSh)%AbsDiffShade * CosInc * SunLitFract

        END IF

            ! Exterior beam absorbed by BETWEEN-GLASS SHADE

        IF(ShadeFlag == BGShadeOn) THEN
          AbsShade = POLYF(CosInc,Construct(ConstrNumSh)%AbsBeamShadeCoef(1:6))
          ExtBeamAbsByShadFac(SurfNum) = AbsShade * CosInc * SunLitFract + &
             SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing * Construct(ConstrNumSh)%AbsDiffShade
        END IF

            ! Exterior beam absorbed by INTERIOR BLIND

        IF(ShadeFlag == IntBlindOn) THEN
          TBmBm          = POLYF(CosInc,Construct(ConstrNum)%TransSolBeamCoef(1:6))
          RGlDiffBack    = Construct(ConstrNum)%ReflectSolDiffBack
          RhoBlFront     = InterpProfSlatAng(ProfAng,SlatAng,VarSlats,Blind(BlNum)%SolFrontBeamDiffRefl)
          AbsBlFront     = InterpProfSlatAng(ProfAng,SlatAng,VarSlats,Blind(BlNum)%SolFrontBeamAbs)
          RhoBlDiffFront = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolFrontDiffDiffRefl)
          AbsBlDiffFront = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolFrontDiffAbs)
          AbsShade       = TBmBm * (AbsBlFront + &
            RhoBlFront*RGlDiffBAck*AbsBlDiffFront/(1.d0-RhoBlDiffFront*RGlDiffBack))
          AbsShadeDiff   = Construct(ConstrNum)%TransDiff * (AbsBlDiffFront + RhoBlDiffFront * &
            RGlDiffBAck*AbsBlDiffFront/(1.d0-RhoBlDiffFront*RGlDiffBack))
          ExtBeamAbsByShadFac(SurfNum) = ( AbsShade * CosInc * SunLitFract * InOutProjSLFracMult &

            + SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing * AbsShadeDiff ) * SurfaceWindow(SurfNum)%GlazedFrac
                ! In the above, GlazedFrac corrects for shadowing of divider onto interior blind
        END IF

          ! Exterior beam absorbed by EXTERIOR BLIND

        IF(ShadeFlag == ExtBlindOn) THEN
          TBlBmBm       = BlindBeamBeamTrans(ProfAng,SlatAng,Blind(BlNum)%SlatWidth,Blind(BlNum)%SlatSeparation, &
                                       Blind(BlNum)%SlatThickness)
          RGlFront      = POLYF(CosInc,Construct(ConstrNum)%ReflSolBeamFrontCoef(1:6))
          AbsBlFront    = InterpProfSlatAng(ProfAng,SlatAng,VarSlats,Blind(BlNum)%SolFrontBeamAbs)
          AbsBlBack     = InterpProfSlatAng(ProfAng,SlatAng,VarSlats,Blind(BlNum)%SolBackBeamAbs)
          AbsBlDiffBack = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolBackDiffAbs)
          RGlDiffFront  = Construct(ConstrNum)%ReflectSolDiffFront
          RhoBlDiffBack = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolBackDiffDiffRefl)
          RhoBlBack     = InterpProfSlatAng(ProfAng,SlatAng,VarSlats,Blind(BlNum)%SolBackBeamDiffRefl)
          TBlBmDiff     = InterpProfSlatAng(ProfAng,SlatAng,VarSlats,Blind(BlNum)%SolFrontBeamDiffTrans)
          AbsShade      = AbsBlFront + AbsBlBack * RGlFront * TBlBmBm &
                            + (AbsBlDiffBack*RGlDiffFront/(1.d0-RhoBlDiffBack*RGlDiffFront)) * &
                              (RGlFront*TBlBmBm*RhoBlBack + TBlBmDiff)
          ExtBeamAbsByShadFac(SurfNum) = AbsShade * CosInc * SunLitFract * InOutProjSLFracMult

        END IF

          ! Exterior beam absorbed by EXTERIOR SCREEN
        IF(ShadeFlag == ExtScreenOn) THEN
          TScBmBm       = SurfaceScreens(SurfaceWindow(SurfNum)%ScreenNumber)%BmBmTrans
  !        TScBmDiff     = SurfaceScreens(SurfaceWindow(SurfNum)%ScreenNumber)%BmDifTrans
          RGlFront      = POLYF(CosInc,Construct(ConstrNum)%ReflSolBeamFrontCoef(1:6))
          RGlDiffFront  = Construct(ConstrNum)%ReflectSolDiffFront

          AbsScBeam     = SurfaceScreens(ScNum)%AbsorpSolarBeamFront
          AbsScDiffBack = SurfaceScreens(ScNum)%DifScreenAbsorp
          RScDifBack   = SurfaceScreens(ScNum)%DifReflect
          RScBack       = SurfaceScreens(ScNum)%ReflectSolBeamFront

          AbsScreen      = AbsScBeam * (1.0d0 + TScBmBm * RGlFront) + &
                         (AbsScDiffBack*TScBmBm*RGlFront*RGlDiffFront*RScBack/(1.d0-RScDifBack*RGlDiffFront))

          ExtBeamAbsByShadFac(SurfNum) = AbsScreen * CosInc * SunLitFract * InOutProjSLFracMult

        END IF

          ! Exterior beam absorbed by BETWEEN-GLASS BLIND

        IF(ShadeFlag == BGBlindOn) THEN
          IF(NGlass == 2) THEN
            AbsShade     = t1*(afshB + tfshBB*rf2*abshB + tfshBd*rfd2*abshd + rfshB*rbd1*afshd)
            AbsShadeDiff = td1*(afshd*(1 + rfshd*rbd1) + tfshd*rfd2*abshd)
          ELSE IF(NGlass == 3) THEN
            AbsShade     = t1t2*(afshB*(1 + tfshBB*rf3) + afshd*(tfshBd*rfd3 + rfshB*(rbd2 + td2*rbd1*td2)))
            AbsShadeDiff = td1td2*(afshd + tfshd*rfd3*abshd + rfshd*(rfd2 + td2*rbd2*td2)*afshd)
          END IF
          ExtBeamAbsByShadFac(SurfNum) =  AbsShade * CosInc * SunLitFract * InOutProjSLFracMult &
                                            + SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing * AbsShadeDiff
        END IF  ! End of check if between-glass blind

      ELSEIF (SurfaceWindow(SurfNum)%WindowModelType == WindowBSDFModel) THEN

        FenSolAbsPtr = WindowScheduledSolarAbs(SurfNum, ConstrNum)

        ! Do not read from schedule file here since this will be called only if direct beam is hitting the window and schedule
        ! will not be loaded in that case even if diffuse part of solar radiation is entering through the window
        IF (FenSolAbsPtr == 0) THEN
          ! Put in the equivalent layer absorptions
          DO Lay = 1,SurfaceWindow(SurfNum)%ComplexFen%State(SurfaceWindow(SurfNum)%ComplexFen%CurrentState) &
               %NLayers
            ABWin(Lay)    = SurfaceWindow(SurfNum)%ComplexFen &
                  %State(SurfaceWindow(SurfNum)%ComplexFen%CurrentState)%WinBmFtAbs(Lay,HourOfDay,TimeStep) &
                           * CosInc * SunLitFract * SurfaceWindow(SurfNum)%OutProjSLFracMult(HourOfDay)

              ! Add contribution of beam reflected from outside and inside reveal
            AWinSurf(SurfNum,Lay) = ABWin(Lay) &
                  + SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing * &
                      SurfaceWindow(SurfNum)%ComplexFen &
                          %State(SurfaceWindow(SurfNum)%ComplexFen%CurrentState)%WinFtHemAbs(Lay)&
                  + SurfaceWindow(SurfNum)%InsRevealDiffOntoGlazing * &
                      SurfaceWindow(SurfNum)%ComplexFen &
                          %State(SurfaceWindow(SurfNum)%ComplexFen%CurrentState)%WinBkHemAbs(Lay)
          END DO
        END IF

      ELSEIF (SurfaceWindow(SurfNum)%WindowModelType == WindowEQLModel) THEN
        ! call the ASHWAT fenestration model for optical properties
        ! determine the beam radiation absorptance and tranmittance of the
        ! the equivalent layer window model
        CALL CalcEQLOpticalProperty(SurfNum, isBEAM, AbsSolBeamEQL)

        ! recalcuate the diffuse absorptance and transmittance of the
        ! the equivalent layer window model if there is shade control
        EQLNum = Construct(Surface(SurfNum)%Construction)%EQLConsPtr
        IF (CFS(EQLNum)%ISControlled) THEN
          CALL CalcEQLOpticalProperty(SurfNum, isDIFF, AbsSolDiffEQL)
        ELSE
          AbsSolDiffEQL(1:CFS(EQLNum)%NL+1,:) = CFSDiffAbsTrans(EQLNum,1:CFS(EQLNum)%NL+1,:)
        ENDIF
        Construct(ConstrNum)%TransDiff = AbsSolDiffEQL(CFS(EQLNum)%NL+1,1)

        DO Lay = 1, CFS(EQLNum)%NL+1
           AbWinEQL(Lay) = AbsSolBeamEQL(Lay,1) * CosInc * SunLitFract * InOutProjSLFracMult
           IF ( CFS(EQLNum)%L(1)%LTYPE /= ltyGLAZE) THEN
               ! if the first layer is not glazing (or it is a shade) do not
               AWinSurf(SurfNum,Lay) = AbWinEQL(Lay)
           ELSE
               ! the first layer is a glazing, include the outside reveal reflection
               ! and the inside reveal reflection until indoor shade layer is encountered.
               IF (CFS(EQLNum)%L(Lay)%LTYPE == ltyGLAZE) THEN
                   AWinSurf(SurfNum,Lay) = AbWinEQL(Lay) &
                   + SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing * AbsSolBeamEQL(Lay,1) &
                   + SurfaceWindow(SurfNum)%InsRevealDiffOntoGlazing * AbsSolDiffEQL(Lay,2)
               ELSE
                    AWinSurf(SurfNum,Lay) = AbWinEQL(Lay) &
                    + SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing * AbsSolBeamEQL(Lay,1)
               ENDIF
           ENDIF
        END DO
        TBmBmEQL = AbsSolBeamEQL(CFS(EQLNum)%NL+1,1)
        ! Beam-diffuse transmittance
        TBmDiffEQL = MAX(0.0d0, AbsSolBeamEQL(CFS(EQLNum)%NL+1,2))
        ! Beam-beam transmittance: difference between beam-total and beam-diffuse transmittance
        TBmBmEQL = MAX(0.0d0, (TBmBmEQL-TBmDiffEQL))
      ENDIF

    END IF ! End of SunLitFrac check


      !-----------------------------------------------------------------
      ! SKY AND GROUND DIFFUSE SOLAR GAIN INTO ZONE FROM EXTERIOR WINDOW
      !-----------------------------------------------------------------

    SkySolarInc = SurfaceWindow(SurfNum)%SkySolarInc
    GndSolarInc = SurfaceWindow(SurfNum)%GndSolarInc

    IF (SurfaceWindow(SurfNum)%WindowModelType  /= WindowBSDFModel .AND. & ! Regular window
        SurfaceWindow(SurfNum)%WindowModelType  /= WindowEQLModel) THEN

      DiffTrans = Construct(ConstrNum)%TransDiff
      IF (DifSolarRad /= 0.0d0) THEN
        DSZoneWin = (SkySolarInc * DiffTrans * Surface(SurfNum)%Area) / (DifSolarRad)
      ELSE
        DSZoneWin = (SkySolarInc * DiffTrans * Surface(SurfNum)%Area) / (1.d-8)
      ENDIF
      IF (GndSolarRad /= 0.0d0) THEN
        DGZoneWin = (GndSolarInc * DiffTrans * Surface(SurfNum)%Area) / (GndSolarRad)
      ELSE
        DGZoneWin = (GndSolarInc * DiffTrans * Surface(SurfNum)%Area) / (1.d-8)
      ENDIF
    ELSEIF (SurfaceWindow(SurfNum)%OriginalClass == SurfaceClass_TDD_Diffuser) THEN
      DiffTrans = TransTDD(PipeNum, CosInc, SolarAniso)

      DSZoneWin = AnisoSkyMult(SurfNum2) * DiffTrans * Surface(SurfNum)%Area
      DGZoneWin = Surface(SurfNum2)%ViewFactorGround * TDDPipe(PipeNum)%TransSolIso * Surface(SurfNum)%Area

    ELSE IF (Surface(SurfNum)%Class == SurfaceClass_TDD_Dome) THEN
      DiffTrans = Construct(ConstrNum)%TransDiff

      DSZoneWin = 0.0d0 ! Solar not added by TDD:DOME; added to zone via TDD:DIFFUSER
      DGZoneWin = 0.0d0 ! Solar not added by TDD:DOME; added to zone via TDD:DIFFUSER

    ELSE IF (OutShelfSurf > 0) THEN ! Outside daylighting shelf
      DiffTrans = Construct(ConstrNum)%TransDiff

      DSZoneWin = AnisoSkyMult(SurfNum) * DiffTrans * Surface(SurfNum)%Area

      ShelfSolarRad = (BeamSolarRad * SunlitFrac(OutShelfSurf,HourOfDay,TimeStep) &
        * CosIncAng(OutShelfSurf,HourOfDay,TimeStep) + DifSolarRad * AnisoSkyMult(OutShelfSurf)) &
        * Shelf(ShelfNum)%OutReflectSol

      ! Add all reflected solar from the outside shelf to the ground solar
      ! NOTE:  If the shelf blocks part of the view to the ground, the user must reduce the ground view factor!!

      ! In order to get the effect of the daylighting shelf in here, must take into account the fact that this
      ! is ultimately multiplied by GndSolarRad to get QD and QDV in InitSolarHeatGains.
      !
      ! DGZoneWin = (GndVF*Trans*Area*GndSolarRad + ShelfVF*Trans*Area*ShelfSolarRad) / GndSolarRad
      !
      IF (GndSolarRad /= 0.0d0) THEN
        DGZoneWin = (Surface(SurfNum)%ViewFactorGround * DiffTrans * Surface(SurfNum)%Area * GndSolarRad &
          + Shelf(ShelfNum)%ViewFactor * DiffTrans * Surface(SurfNum)%Area * ShelfSolarRad) / GndSolarRad
      ELSE
        DGZoneWin = 0.0d0
      ENDIF

    ELSEIF (SurfaceWindow(SurfNum)%WindowModelType  == WindowBSDFModel) THEN ! complex fenestration
      FenSolAbsPtr = WindowScheduledSolarAbs(SurfNum, ConstrNum)
      IF (FenSolAbsPtr == 0) THEN
        !Sky Diffuse transmitted by Complex Fen
        DiffTransSky  = SurfaceWindow(SurfNum)%ComplexFen%State( SurfaceWindow(SurfNum)%ComplexFen%CurrentState )%WinSkyTrans
        IF (DifSolarRad /= 0.0d0) THEN
          DSZoneWin = SkySolarInc * DiffTransSky * Surface(SurfNum)%Area / (DifSolarRad)
        ELSE
          DSZoneWin = SkySolarInc * DiffTransSky * Surface(SurfNum)%Area / (1.d-8)
        END IF
          !Ground Diffuse transmitted by Complex Fen
        DiffTransGnd = SurfaceWindow(SurfNum)%ComplexFen%State(SurfaceWindow(SurfNum)%ComplexFen%CurrentState)%WinSkyGndTrans
        DiffTransBmGnd = SurfaceWindow(SurfNum)%ComplexFen%State( SurfaceWindow(SurfNum)%ComplexFen%CurrentState )  &
               %WinBmGndTrans(HourOfDay,TimeStep)
        IF (GndSolarRad /= 0.0d0) THEN
          DGZoneWin = ((SurfaceWindow(SurfNum)%BmGndSolarInc * DiffTransBmGnd &
               +SurfaceWindow(SurfNum)%SkyGndSolarInc * DiffTransGnd ) * Surface(SurfNum)%Area) / (GndSolarRad)
        ELSE
          DGZoneWin = ((SurfaceWindow(SurfNum)%BmGndSolarInc * DiffTransBmGnd &
               +SurfaceWindow(SurfNum)%SkyGndSolarInc * DiffTransGnd ) * Surface(SurfNum)%Area) / (1.d-8)
        END IF

        !Define the effective transmittance for total sky and ground radiation
        IF ((SkySolarInc + SurfaceWindow(SurfNum)%BmGndSolarInc + SurfaceWindow(SurfNum)%SkyGndSolarInc ) /= 0.0d0) THEN
          DiffTrans = ( SkySolarInc*DiffTransSky +SurfaceWindow(SurfNum)%BmGndSolarInc * DiffTransBmGnd &
               + SurfaceWindow(SurfNum)%SkyGndSolarInc * DiffTransGnd )/  &
                  (SkySolarInc + SurfaceWindow(SurfNum)%BmGndSolarInc + SurfaceWindow(SurfNum)%SkyGndSolarInc )
        ELSE
          DiffTrans = 0.0d0
        END IF

        !Also update the nominal diffuse transmittance
        NomDiffTrans = SurfaceWindow(SurfNum)%ComplexFen%State(SurfaceWindow(SurfNum)%ComplexFen%CurrentState )%WinDiffTrans
        Construct(Surface(SurfNum)%Construction)%TransDiff = NomDiffTrans
      ELSE
        DSZoneWin = 0.0d0
        DGZoneWin = 0.0d0
        DiffTrans = 0.0d0
        TBmBm     = 0.0d0
        TBmDif    = 0.0d0
        NomDiffTrans = 0.0d0
      END IF

    ELSEIF (SurfaceWindow(SurfNum)%WindowModelType == WindowEQLModel) THEN

        DiffTrans = Construct(ConstrNum)%TransDiff

        IF (DifSolarRad /= 0.0d0) THEN
          DSZoneWin = (SkySolarInc * DiffTrans * Surface(SurfNum)%Area) / (DifSolarRad)
        ELSE
          DSZoneWin = (SkySolarInc * DiffTrans * Surface(SurfNum)%Area) / (1.d-8)
        ENDIF
        IF (GndSolarRad /= 0.0d0) THEN
          DGZoneWin = (GndSolarInc * DiffTrans * Surface(SurfNum)%Area) / (GndSolarRad)
        ELSE
          DGZoneWin = (GndSolarInc * DiffTrans * Surface(SurfNum)%Area) / (1.d-8)
        ENDIF

    END IF

    IF ((SurfaceWindow(SurfNum)%WindowModelType  /= WindowBSDFModel).AND.&
        (SurfaceWindow(SurfNum)%WindowModelType /= WindowEQLModel)) THEN
        IF(ShadeFlag <= 0 .OR. ShadeFlag >= 10) THEN
                 ! Unshaded window
          DSZone(ZoneNum) = DSZone(ZoneNum) + DSZoneWin
          DGZone(ZoneNum) = DGZone(ZoneNum) + DGZoneWin
        ELSE IF(ShadeFlag /= SwitchableGlazing) THEN
                 ! Shade or blind
          IF(ShadeFlag==IntShadeOn.OR.ShadeFlag==ExtShadeOn.OR.ShadeFlag==BGShadeOn.OR.ShadeFlag==ExtScreenOn) THEN
                 ! Shade or screen
            DiffTrans = Construct(ConstrNumSh)%TransDiff
          ELSE
                 ! Blind
            DiffTrans = InterpSlatAng(SlatAng,VarSlats,Construct(ConstrNumSh)%BlTransDiff)
            ! For blinds with horizontal slats, allow different diffuse/diffuse transmittance for
            ! ground and sky solar
            IF(Blind(SurfaceWindow(SurfNum)%BlindNumber)%SlatOrientation == Horizontal) THEN
              DiffTransGnd = InterpSlatAng(SlatAng,VarSlats,Construct(ConstrNumSh)%BlTransDiffGnd)
              DiffTransSky = InterpSlatAng(SlatAng,VarSlats,Construct(ConstrNumSh)%BlTransDiffSky)
            END IF
          END IF
          IF (DifSolarRad /= 0.0d0) THEN
             DSZoneWinSh     = SkySolarInc * DiffTrans * Surface(SurfNum)%Area / (DifSolarRad)
          ELSE
             DSZoneWinSh     = SkySolarInc * DiffTrans * Surface(SurfNum)%Area / (1.d-8)
          END IF

          IF (GndSolarRad /= 0.0d0) THEN
             DGZoneWinSh     = GndSolarInc * DiffTrans * Surface(SurfNum)%Area / (GndSolarRad)
          ELSE
             DGZoneWinSh     = GndSolarInc * DiffTrans * Surface(SurfNum)%Area / (1.d-8)
          END IF

          IF(ShadeFlag==IntBlindOn.OR.ShadeFlag==ExtBlindOn.OR.ShadeFlag==BGBlindOn) THEN
             IF(Blind(SurfaceWindow(SurfNum)%BlindNumber)%SlatOrientation == Horizontal) THEN
               CosTlt = Surface(SurfNum)%CosTilt

               IF (DifSolarRad /= 0.0d0) THEN
                 DSZoneWinSh   = SkySolarInc * Surface(SurfNum)%Area * &
                          (0.5d0*ABS(CosTlt)*DiffTransGnd + (1.d0-0.5d0*ABS(CosTlt))*DiffTransSky) / (DifSolarRad)
               ELSE
                 DSZoneWinSh   = SkySolarInc * Surface(SurfNum)%Area * &
                          (0.5d0*ABS(CosTlt)*DiffTransGnd + (1.d0-0.5d0*ABS(CosTlt))*DiffTransSky) / (1.d-8)
               END IF

               IF (GndSolarRad /= 0.0d0) THEN
                 DGZoneWinSh   = GndSolarInc * Surface(SurfNum)%Area * &
                          ((1.d0-0.5d0*ABS(CosTlt))*DiffTransGnd + 0.5d0*ABS(CosTlt)*DiffTransSky) / (GndSolarRad)
               ELSE
                 DGZoneWinSh   = GndSolarInc * Surface(SurfNum)%Area * &
                          ((1.d0-0.5d0*ABS(CosTlt))*DiffTransGnd + 0.5d0*ABS(CosTlt)*DiffTransSky) / (1.d-8)
               END IF
             END IF
          END IF
          DSZone(ZoneNum) = DSZone(ZoneNum) + DSZoneWinSh
          DGZone(ZoneNum) = DGZone(ZoneNum) + DGZoneWinSh
        ELSE
                 ! Switchable glazing
          SwitchFac       = SurfaceWindow(SurfNum)%SwitchingFactor
          DiffTrans       = InterpSW(SwitchFac,Construct(ConstrNum)%TransDiff,Construct(ConstrNumSh)%TransDiff)
           IF (DifSolarRad /= 0.0d0) THEN
        DSZoneWinSh     = SkySolarInc * DiffTrans * Surface(SurfNum)%Area / (DifSolarRad)
      ELSE
        DSZoneWinSh     = SkySolarInc * DiffTrans * Surface(SurfNum)%Area / (1.d-8)
      END IF
      IF (GndSolarRad /= 0.0d0) THEN
        DGZoneWinSh     = GndSolarInc * DiffTrans * Surface(SurfNum)%Area / (GndSolarRad)
      ELSE
        DGZoneWinSh     = GndSolarInc * DiffTrans * Surface(SurfNum)%Area / (1.d-8)
      END IF
          DSZone(ZoneNum) = DSZone(ZoneNum) + InterpSw(SwitchFac,DSZoneWin,DSZoneWinSh)
          DGZone(ZoneNum) = DGZone(ZoneNum) + InterpSw(SwitchFac,DGZoneWin,DGZoneWinSh)
        END IF
    ELSEIF (SurfaceWindow(SurfNum)%WindowModelType  == WindowBSDFModel) THEN
        DSZone(ZoneNum) = DSZone(ZoneNum) + DSZoneWin
        DGZone(ZoneNum) = DGZone(ZoneNum) + DGZoneWin
    ELSEIF (SurfaceWindow(SurfNum)%WindowModelType == WindowEQLModel) THEN
        ! For equivalent layer model the zone total diffuse solar heat gain
        ! through exterior fenestrations are reported as single value.
        DSZoneWin     = SkySolarInc * DiffTrans * Surface(SurfNum)%Area / (DifSolarRad + 1.d-8)
        DGZoneWin     = GndSolarInc * DiffTrans * Surface(SurfNum)%Area / (GndSolarRad + 1.d-8)

        DSZone(ZoneNum) = DSZone(ZoneNum) + DSZoneWin
        DGZone(ZoneNum) = DGZone(ZoneNum) + DGZoneWin
    ENDIF
        !-----------------------------------------------------------------
        ! BEAM SOLAR ON EXTERIOR WINDOW TRANSMITTED AS BEAM AND/OR DIFFUSE
        !-----------------------------------------------------------------

    TBmBm        = 0.0d0
    TBmDif       = 0.0d0
    TBmAllShBlSc = 0.0d0
    TBmBmShBlSc = 0.0d0
    TBmDifShBlSc = 0.0d0

    ! Beam-beam transmittance for bare exterior window
    IF (SunlitFract > 0.0d0) THEN
      IF (SurfaceWindow(SurfNum)%OriginalClass == SurfaceClass_TDD_Diffuser) THEN
        TBmDif = TransTDD(PipeNum, CosInc, SolarBeam)
        TDDPipe(PipeNum)%TransSolBeam = TBmDif ! Report variable
      ELSEIF(SurfaceWindow(SurfNum)%WindowModelType  /= WindowBSDFModel .AND. &
             SurfaceWindow(SurfNum)%WindowModelType  /= WindowEQLModel) THEN ! Regular window
        IF(.not.SurfaceWindow(SurfNum)%SolarDiffusing) THEN   ! Clear glazing
          TBmBm = POLYF(CosInc,Construct(ConstrNum)%TransSolBeamCoef(1:6)) ![-]
        ELSE                                                  ! Diffusing glazing
          TBmDif = POLYF(CosInc,Construct(ConstrNum)%TransSolBeamCoef(1:6)) ![-]
        END IF
      ELSEIF (SurfaceWindow(SurfNum)%WindowModelType  == WindowBSDFModel) THEN
        ! Need to check what effect, if any, defining these here has
        TBmBm = SurfaceWindow(SurfNum)%ComplexFen%State( SurfaceWindow(SurfNum)%ComplexFen%CurrentState )  &
                 %WinDirSpecTrans( HourOfDay,TimeStep )
        TBmDif = SurfaceWindow(SurfNum)%ComplexFen%State( SurfaceWindow(SurfNum)%ComplexFen%CurrentState ) &
                 %WinDirHemiTrans( HourOfDay,TimeStep )  - TBmBm
      ELSEIF (SurfaceWindow(SurfNum)%WindowModelType == WindowEQLModel) THEN
        ! get ASHWAT fenestration model beam-beam and beam-diffuse properties
        TBmBm = TBmBmEQL
        TBmDif = TBmDiffEQL
      END IF
    END IF

    ! Report variables
    SurfaceWindow(SurfNum)%GlTsolBmBm  = TBmBm
    SurfaceWindow(SurfNum)%GlTsolBmDif = TBmDif

    ! Diffuse-diffuse transmittance for bare exterior window
    IF (SurfaceWindow(SurfNum)%OriginalClass == SurfaceClass_TDD_Diffuser) THEN
      TDifBare = TransTDD(PipeNum, CosInc, SolarAniso)
    ELSE
      IF ( SurfaceWindow(SurfNum)%WindowModelType  == WindowBSDFModel) THEN
            !Complex Fenestration: use hemispherical ave of directional-hemispherical transmittance
            !Note: this is not quite the same as the effective transmittance for total of sky and ground radiation
        TDifBare = SurfaceWindow(SurfNum)%ComplexFen%State( SurfaceWindow(SurfNum)%ComplexFen%CurrentState )%WinDiffTrans
      ELSEIF (SurfaceWindow(SurfNum)%WindowModelType == WindowEQLModel) THEN
            !get ASHWAT fenestration model diffuse-diffuse properties includes shade if present
        TDifBare = Construct(ConstrNum)%TransDiff
      ELSE  ! Regular window
        TDifBare = Construct(ConstrNum)%TransDiff
      END IF
    END IF
    SurfaceWindow(SurfNum)%GlTsolDifDif = TDifBare


    IF (SurfaceWindow(SurfNum)%WindowModelType /= WindowEQLModel) THEN
        IF(ShadeFlag > 0 .AND. ShadeFlag < 10) THEN

                            ! Shade or screen or blind on, or switchable glazing
                            ! (note in the following that diffusing glass is not allowed in a window with
                            ! shade, blind or switchable glazing)

          IF(ShadeFlag /= IntBlindOn .AND. ShadeFlag /= ExtBlindOn .AND. ShadeFlag /= BGBlindOn .AND. ShadeFlag /= ExtScreenOn) THEN

                            ! Shade on or switchable glazing

            IF(SunlitFract > 0.0d0) TBmAllShBlSc = POLYF(CosInc,Construct(ConstrNumSh)%TransSolBeamCoef(1:6))

          ELSE

                            ! Blind or Screen on

            SurfaceWindow(SurfNum)%BlGlSysTsolDifDif = DiffTrans
            SurfaceWindow(SurfNum)%ScGlSysTsolDifDif = DiffTrans
            IF(ShadeFlag == IntBlindOn .OR. ShadeFlag == ExtBlindOn .OR. ShadeFlag == BGBlindOn)THEN
              SurfaceWindow(SurfNum)%BlTsolDifDif = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolFrontDiffDiffTrans)
            ELSE IF(ShadeFlag == ExtScreenOn)THEN
              SurfaceWindow(SurfNum)%ScTsolDifDif = SurfaceScreens(ScNum)%DifDifTrans
            END IF

            IF(SunlitFract > 0.0d0) THEN
              IF(ShadeFlag == ExtScreenOn)THEN
    !           beam transmittance (written in subroutine CalcScreenTransmittance each time step)
                TScBmBm  = SurfaceScreens(ScNum)%BmBmTrans
                SurfaceWindow(SurfNum)%ScTsolBmBm = TScBmBm
              ELSE
                TBlBmBm  = BlindBeamBeamTrans(ProfAng,SlatAng,Blind(BlNum)%SlatWidth,Blind(BlNum)%SlatSeparation, &
                                           Blind(BlNum)%SlatThickness)
                SurfaceWindow(SurfNum)%BlTsolBmBm = TBlBmBm
              END IF
              IF(ShadeFlag==IntBlindOn .OR. ShadeFlag==ExtBlindOn) THEN
                ! Interior or exterior blind
                TBmBmBl  = TBmBm * TBlBmBm
              ELSE IF(ShadeFlag==ExtScreenOn) THEN
                ! Exterior screen
                TBmBmSc  = TBmBm * TScBmBm
              ELSE
                            ! Between-glass blind
                IF(NGlass==2) THEN
                  TBmBmBl = t1*tfshBB*t2
                ELSE  ! NGlass = 3
                  TBmBmBl = t1*t2*tfshBB*t3
                END IF
              END IF
              IF(ShadeFlag == ExtScreenOn)THEN
    !           Report variable for Beam-to-Beam transmittance
                SurfaceWindow(SurfNum)%ScGlSysTsolBmBm = TBmBmSc
              ELSE
                SurfaceWindow(SurfNum)%BlGlSysTsolBmBm = TBmBmBl
              END IF

              IF(ShadeFlag == ExtScreenOn)THEN
                TScBmDif = SurfaceScreens(ScNum)%BmDifTrans
    !           Report variable for Beam-to-Diffuse transmittance (scattered transmittance)
                SurfaceWindow(SurfNum)%ScTsolBmDif  = TScBmDif
              ELSE
                TBlBmDif = InterpProfSlatAng(ProfAng,SlatAng,VarSlats,Blind(BlNum)%SolFrontBeamDiffTrans)
                SurfaceWindow(SurfNum)%BlTsolBmDif  = TBlBmDif
    !CR6913     SurfaceWindow(SurfNum)%BlTsolDifDif = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolFrontDiffDiffTrans)
              END IF

              !added TH 12/9/2009
              TBmBmShBlSc = 0.0d0
              TBmDifShBlSc = 0.0d0

              IF(ShadeFlag == IntBlindOn) THEN

                            ! Interior blind on: beam-beam and diffuse transmittance of exterior beam

                TBlDifDif     = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolFrontDiffDiffTrans)
                RhoBlBmDifFr  = InterpProfSlatAng(ProfAng,SlatAng,VarSlats,Blind(BlNum)%SolFrontBeamDiffRefl)
                RGlDifBk      = Construct(ConstrNum)%ReflectSolDiffBack
                RhoBlDifDifFr = InterpSlatAng(SlatAng,VarSlats,Blind(BlNum)%SolFrontDiffDiffRefl)
                TBmAllShBlSc  = TBmBm*( TBlBmBm + TBlBmDif + &
                                  TBlDifDif*RhoBlBmDifFr*RGlDifBk/(1-RhoBlDifDifFr*RGlDifBk) )

                !added TH 12/9/2009
                TBmBmShBlSc = TBmBmBl    !TBmBm * TBlBmBm
                TBmDifShBlSc = TBmAllShBlSc - TBmBmShBlSc
                IF (TBmDifShBlSc < 0.0d0) TBmDifShBlSc = 0.0d0

              ELSE IF(ShadeFlag == ExtBlindOn) THEN

                            ! Exterior blind on: beam-beam and diffuse transmittance of exterior beam

                RhoBlBmDifBk  = InterpProfSlatAng(ProfAng,SlatAng,VarSlats,Blind(BlNum)%SolBackBeamDiffRefl)
                RGlBmFr       = POLYF(CosInc,Construct(ConstrNum)%ReflSolBeamFrontCoef(1:6))
                TBmAllShBlSc  = TBlBmBm * (TBmBm + TDifBare*RGlBmFr*RhoBlBmDifBk/(1-RGlDifFr*RhoBlDifDifBk)) +  &
                                  TBlBmDif*TDifBare/(1-RGlDifFr*RhoBlDifDifBk)

                !added TH 12/9/2009
                TBmBmShBlSc = TBmBmBl    !TBmBm * TBlBmBm
                TBmDifShBlSc = TBmAllShBlSc - TBmBmShBlSc

              ELSE IF(ShadeFlag == ExtScreenOn) THEN

                            ! Exterior screen on: beam-beam and diffuse transmittance of exterior beam

                RScBack       = SurfaceScreens(ScNum)%ReflectSolBeamFront
                RScDifDifBk   = SurfaceScreens(ScNum)%DifReflect
                RGlBmFr       = POLYF(CosInc,Construct(ConstrNum)%ReflSolBeamFrontCoef(1:6))
                TBmAllShBlSc  = TScBmBm * (TBmBm + RGlBmFr*RScBack*TDifBare/(1-RGlDifFr*RScDifDifBk)) + &
                                           TScBmDif*TDifBare/(1-RGlDifFr*RScDifDifBk)

                !added TH 12/9/2009
                TBmBmShBlSc = TBmBmSc    !
                TBmDifShBlSc = TBmAllShBlSc - TBmBmShBlSc

              ELSE
                            ! Between-glass blind on: beam-beam and diffuse transmittance of exterior beam

                IF(NGlass==2) THEN
                  TBmAllShBlSc = t1*tfshBB*t2 + t1*(tfshBB*rf2*rbshB + tfshBd*(1.0d0 + rfd2*rbshd) + rfshB*rbd1*rfshd)*td2
                ELSE   ! NGlass = 3
                  TBmAllShBlSc = t1t2*tfshBB*t3 + &
                     t1t2*(tfshBB*rf3*rbshB + tfshBd*(1.0d0 + rfd3*rbshd) + rbshB*(rbd2*tfshd + td2*rbd1*td2*tfshd))*td3
                END IF

                  !added TH 12/9/2009
                  TBmBmShBlSc = TBmBmBl
                  TBmDifShBlSc = TBmAllShBlSc - TBmBmShBlSc

              END IF
            END IF

          END IF

        END IF  ! End of check if ShadeFlag > 0 and ShadeFlag < 10
    ENDIF

    IF(ShadeFlag == SwitchableGlazing) THEN

                        ! Switchable glazing

      SwitchFac = SurfaceWindow(SurfNum)%SwitchingFactor
      IF(.not.SurfaceWindow(SurfNum)%SolarDiffusing) THEN
        TBmBm     = InterpSw(SwitchFac,TBmBm,TBmAllShBlSc)
      ELSE
        TBmDif    = InterpSw(SwitchFac,TBmDif,TBmAllShBlSc)
      ENDIF
    END IF

    ! The following WinTransBmSolar and WinTransDifSolar will be combined later to give
    ! WinTransSolar for reporting
    IF (SurfaceWindow(SurfNum)%WindowModelType /= WindowEQLModel) THEN
        WinTransDifSolar(SurfNum) = DiffTrans * Surface(SurfNum)%Area
        IF(ShadeFlag==IntBlindOn.OR.ShadeFlag==ExtBlindOn.OR.ShadeFlag==BGBlindOn) THEN
          IF(Blind(SurfaceWindow(SurfNum)%BlindNumber)%SlatOrientation==Horizontal) THEN
            WinTransDifSolarGnd(SurfNum) = DiffTransGnd * Surface(SurfNum)%Area
            WinTransDifSolarSky(SurfNum) = DiffTransSky * Surface(SurfNum)%Area
          END IF
        END IF
    ELSE
        ! In equivalent layer window model system diffuse transmittance is based on unit
        ! diffuse radiation flux, and hence doesn't distinguish between sky and
        ! ground reflected diffuse radiations
        WinTransDifSolar(SurfNum) = DiffTrans * Surface(SurfNum)%Area
        WinTransDifSolarGnd(SurfNum) = DiffTrans * Surface(SurfNum)%Area
        WinTransDifSolarSky(SurfNum) = DiffTrans * Surface(SurfNum)%Area
    ENDIF
    IF(ShadeFlag < 1 .OR. ShadeFlag == SwitchableGlazing .OR. ShadeFlag >= 10) THEN ! Unshaded or switchable glazing
      !Note: with previous defs of TBmBm & TBmDif, these come out right for Complex Fenestration
      ! WinTransBmSolar uses the directional-hemispherical transmittance
      WinTransBmSolar(SurfNum)  = (TBmBm + TBmDif) * SunLitFract * CosInc * Surface(SurfNum)%Area * InOutProjSLFracMult


      !added TH 12/9/2009
      WinTransBmBmSolar  = TBmBm * SunLitFract * CosInc * Surface(SurfNum)%Area * InOutProjSLFracMult          ! m2
      WinTransBmDifSolar = TBmDif * SunLitFract * CosInc * Surface(SurfNum)%Area * InOutProjSLFracMult         ! m2

    ELSE
      WinTransBmSolar(SurfNum)  = TBmAllShBlSc * SunLitFract * CosInc * Surface(SurfNum)%Area * InOutProjSLFracMult

      !added TH 12/9/2009
      WinTransBmBmSolar  = TBmBmShBlSc * SunLitFract * CosInc * Surface(SurfNum)%Area * InOutProjSLFracMult
      WinTransBmDifSolar = TBmDifShBlSc * SunLitFract * CosInc * Surface(SurfNum)%Area * InOutProjSLFracMult
    END IF

    ! Add diffuse transmitted by window from beam reflected from outside reveal

    IF( SurfaceWindow(SurfNum)%WindowModelType  == WindowBSDFModel) THEN  !Complex Fenestration
      FenSolAbsPtr = WindowScheduledSolarAbs(SurfNum, ConstrNum)
        IF (FenSolAbsPtr == 0) THEN
          WinTransBmSolar(SurfNum) = WinTransBmSolar(SurfNum) + &
            SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing * NomDiffTrans * Surface(SurfNum)%Area

          WinTransBmDifSolar = WinTransBmDifSolar + SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing * NomDiffTrans &
                            * Surface(SurfNum)%Area
        ELSE
          WinTransBmSolar(SurfNum) = 0.0d0
          WinTransBmDifSolar = 0.0d0
        END IF
    ELSE  !Regular window
          ! this is also valid for equivalent layer window
      WinTransBmSolar(SurfNum) = WinTransBmSolar(SurfNum) + &
          SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing * DiffTrans * Surface(SurfNum)%Area

      !added TH 12/9/2009
      WinTransBmDifSolar = WinTransBmDifSolar + SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing * DiffTrans &
        * Surface(SurfNum)%Area
    END IF

    ! Increment factor for total exterior beam solar entering zone through window as beam or diffuse

    IF(SunLitFract > 0.0d0 .AND. Surface(SurfNum)%Class /= SurfaceClass_TDD_Dome) THEN

      IF(ShadeFlag==IntShadeOn.OR.ShadeFlag==ExtShadeOn.OR.ShadeFlag==IntBlindOn.OR.ShadeFlag==ExtBlindOn.OR.&
         ShadeFlag==BGShadeOn.OR.ShadeFlag==BGBlindOn.OR.ShadeFlag==ExtScreenOn) THEN
        TBmAll = TBmAllShBlSc
      ELSE
        TBmAll = TBmBm + TBmDif
      END IF

      FenSolAbsPtr = WindowScheduledSolarAbs(SurfNum, ConstrNum)

      ! Window is schedule surface gained. Do not make addition to what enters into zone since that information is not
      ! available
      IF (FenSolAbsPtr == 0) THEN
        BTOTZone = BTOTZone + TBmAll * SunLitFract * CosInc * Surface(SurfNum)%Area * InOutProjSLFracMult ! [m2]
      END IF

    END IF

    ! Correct for effect of (1) beam absorbed by inside reveal, (2) diffuse entering zone from beam
    ! reflected by inside reveal and (3) diffuse transmitted by window from beam reflected from
    ! outside reveal.
    IF(CosInc > 0.0d0) THEN
      ! old code
      ! BTOTZone = BTOTZone + (SurfaceWindow(SurfNum)%InsRevealDiffIntoZone &
      !                       - SurfaceWindow(SurfNum)%BmSolAbsdInsReveal &
      !                       + SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing * DiffTrans) * Surface(SurfNum)%Area

      ! CR 7596. TH 5/27/2009
      ! The BTOTZone is the solar into zone assuming no inside or outside reveals
      ! The inside reveals receive solar (reflected part + absorbed part) from the window, this amount should be
      ! deducted from the BTOTZone, then adds the InsRevealDiffIntoZone
      IF( SurfaceWindow(SurfNum)%WindowModelType  == WindowBSDFModel) THEN  !Complex Fenestration
        SurfSolIncPtr = SurfaceScheduledSolarInc(BackSurfaceNumber, ConstrNumBack)

        ! Do not add total into zone from scheduled surface gains.  That will be added later
        IF (SurfSolIncPtr == 0) THEN
          BTOTZone = BTOTZone - SurfaceWindow(SurfNum)%BmSolRefldInsReveal &
                            - SurfaceWindow(SurfNum)%BmSolAbsdInsReveal &
                            + SurfaceWindow(SurfNum)%InsRevealDiffIntoZone &
                            + SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing * NomDiffTrans * Surface(SurfNum)%Area
        END IF
      ELSE  !Regular window
        BTOTZone = BTOTZone - SurfaceWindow(SurfNum)%BmSolRefldInsReveal &
                          - SurfaceWindow(SurfNum)%BmSolAbsdInsReveal &
                          + SurfaceWindow(SurfNum)%InsRevealDiffIntoZone &
                          + SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing * DiffTrans * Surface(SurfNum)%Area
      END IF
      ! Add beam solar absorbed by outside reveal to outside of window's base surface.
      ! Add beam solar absorbed by inside reveal to inside of window's base surface.
      ! This ignores 2-D heat transfer effects.
      BaseSurfNum = Surface(SurfNum)%BaseSurf
      AISurf(BaseSurfNum) = AISurf(BaseSurfNum) + SurfaceWindow(SurfNum)%BmSolAbsdInsReveal/Surface(BaseSurfNum)%Area
      AOSurf(BaseSurfNum) = AOSurf(BaseSurfNum) + SurfaceWindow(SurfNum)%BmSolAbsdOutsReveal/Surface(BaseSurfNum)%Area
    END IF

    IF(SunLitFract > 0.0d0) THEN

      !---------------------------------------------------------------------------------
      ! INTERIOR BEAM FROM EXTERIOR WINDOW THAT IS ABSORBED/TRANSMITTED BY BACK SURFACES
      !---------------------------------------------------------------------------------

      ! If shade is in place or there is a diffusing glass layer there is no interior beam
      ! from this exterior window since the beam-beam transmittance of shades and diffusing glass
      ! is assumed to be zero. The beam-beam transmittance of tubular daylighting devices is also
      ! assumed to be zero.

      IF(ShadeFlag==IntShadeOn .OR. ShadeFlag==ExtShadeOn .OR. ShadeFlag==BGShadeOn &
        .OR. SurfaceWindow(SurfNum)%SolarDiffusing  &
        .OR. SurfaceWindow(SurfNum)%OriginalClass == SurfaceClass_TDD_Diffuser &
        .OR. Surface(SurfNum)%Class == SurfaceClass_TDD_Dome) CYCLE

      ! Find interior beam radiation that is:
      ! (1) absorbed by opaque back surfaces;
      ! (2) absorbed by glass layers of back surfaces that are interior or exterior windows;
      ! (3) absorbed by interior, exterior or between-glass shades or blinds of back surfaces
      !       that are exterior windows; and
      ! (4) transmitted through back surfaces that are interior or exterior windows.

      ! Beam-beam transmittance of exterior window
      IF(ShadeFlag==IntBlindOn .OR. ShadeFlag==ExtBlindOn .OR. ShadeFlag==BGBlindOn) THEN
        TBm = TBmBmBl    ! Interior, exterior or between-glass blind on
      ELSE IF(ShadeFlag==ExtScreenOn) THEN
        TBm = TBmBmSc    ! Exterior screen on
      ELSE
        TBm = TBmBm      ! Bare glass or switchable glazing

                         ! Correction for beam absorbed by inside reveal
        TBmDenom=(SunLitFract *  CosInc * Surface(SurfNum)%Area * InOutProjSLFracMult)
        IF (TBmDenom /= 0.0d0) THEN  ! when =0.0, no correction
          TBm = TBm - SurfaceWindow(SurfNum)%BmSolAbsdInsReveal / TBmDenom
        ENDIF

        TBm = MAX(0.0d0,TBm)
      END IF

      IF(TBm == 0.0d0) CYCLE

      IF(InShelfSurf > 0) THEN ! Inside daylighting shelf
        ! Inside daylighting shelves assume that no beam will pass the end of the shelf.
        ! Since all beam is absorbed on the shelf, this might cause them to get unrealistically hot at times.

        BTOTWinZone = TBm * SunLitFract * Surface(SurfNum)%Area * CosInc * InOutProjSLFracMult ![m2]

        ! Shelf surface area is divided by 2 because only one side sees beam (Area was multiplied by 2 during init)
        AISurf(InShelfSurf) = AISurf(InShelfSurf) + BTOTWinZone / (0.5d0 * Surface(InShelfSurf)%Area) ![-]
        BABSZone = BABSZone + BTOTWinZone ![m2]

        CYCLE
      END IF

      IF(SolarDistribution ==  FullInteriorExterior) THEN  ! Full interior solar distribution

       IF (SurfaceWindow(SurfNum)%WindowModelType /= WindowBSDFModel .AND. &
           SurfaceWindow(SurfNum)%WindowModelType /= WindowEQLModel) THEN
        ! Loop over back surfaces irradiated by beam from this exterior window

        DO IBack = 1,MaxBkSurf

          BackSurfNum = BackSurfaces(SurfNum,IBack,HourOfDay,TimeStep)

          IF(BackSurfNum == 0) EXIT   ! No more irradiated back surfaces for this exterior window
          ConstrNumBack = Surface(BackSurfNum)%Construction
          NBackGlass = Construct(ConstrNumBack)%TotGlassLayers
             ! Irradiated (overlap) area for this back surface, projected onto window plane
             ! (includes effect of shadowing on exterior window)
          Aoverlap = OverlapAreas(SurfNum,IBack,HourOfDay,TimeStep)
          Boverlap = TBm * Aoverlap * CosInc ![m2]

          IF(Construct(ConstrNumBack)%TransDiff <= 0.0d0) THEN

            ! Back surface is opaque interior or exterior wall

            AbsIntSurf = Construct(ConstrNumBack)%InsideAbsorpSolar

            ! Check for movable insulation; reproduce code from subr. EvalInsideMovableInsulation;
            ! Can't call that routine here since cycle prevents SolarShadingGeometry from USEing
            ! HeatBalanceSurfaceManager, which contains EvalInsideMovableInsulation
            HMovInsul = 0.0d0
            IF (Surface(BackSurfNum)%MaterialMovInsulInt.GT.0) THEN
              MovInsulSchedVal = GetCurrentScheduleValue(Surface(BackSurfNum)%SchedMovInsulExt)
              IF (MovInsulSchedVal.LE.0.0d0) THEN ! Movable insulation not present at current time
                HMovInsul = 0.0d0
              ELSE  ! Movable insulation present
                HMovInsul = 1.0d0/(MovInsulSchedVal*Material(Surface(BackSurfNum)%MaterialMovInsulInt)%Resistance)
                AbsInt    = Material(Surface(BackSurfNum)%MaterialMovInsulInt)%AbsorpSolar
              END IF
            END IF
            IF (HMovInsul > 0.0d0) AbsIntSurf = AbsInt  ! Movable inside insulation present

            AISurf(BackSurfNum) = AISurf(BackSurfNum) + &     ![-]
              Boverlap * AbsIntSurf / Surface(BackSurfNum)%Area
            BABSZone            = BABSZone + Boverlap * AbsIntSurf       ![m2]

          ELSE

            ! Back surface is an interior or exterior window

            ! Note that exterior back windows can have a shading device but interior back windows
            ! are assumed to be bare, i.e., they have no shading device and are non-switchable.
            ! The layer order for interior windows is "outside" to "inside," where "outside" refers to
            ! the adjacent zone and "inside" refers to the current zone.

            ShadeFlagBack   = SurfaceWindow(BackSurfNum)%ShadingFlag
            SlatAngBack     = SurfaceWindow(BackSurfNum)%SlatAngThisTS
            VarSlatsBack    = SurfaceWindow(BackSurfNum)%MovableSlats
            CosIncBack      = ABS(CosIncAng(BackSurfNum,HourOfDay,TimeStep))
            !
            IF(SurfaceWindow(SurfNum)%WindowModelType == WindowBSDFModel) THEN
                !Transmitting window is complex fen, change the incident angle to one for ray joining
                ! transmitting and back window centers
              CosIncBack = ABS( ComplexWind(SurfNum)%sdotN(IBack) )
            ENDIF
              !
            ConstrNumBackSh = Surface(BackSurfNum)%ShadedConstruction
            IF(SurfaceWindow(BackSurfNum)%StormWinFlag==1) THEN
              ConstrNum   = Surface(BackSurfNum)%StormWinConstruction
              ConstrNumSh = Surface(BackSurfNum)%StormWinShadedConstruction
            END IF
            AbsBeamWin      = 0.d0
            TransBeamWin    = 0.d0

                 ! Interior beam absorptance of glass layers and beam transmittance of back exterior  &
                 ! or interior window WITHOUT SHADING this timestep

            IF(ShadeFlagBack <= 0) THEN
              DO Lay = 1,NBackGlass
                AbsBeamWin(Lay) = POLYF(CosIncBack,Construct(ConstrNumBack)%AbsBeamBackCoef(Lay,1:6))
              END DO
              TransBeamWin      = POLYF(CosIncBack,Construct(ConstrNumBack)%TransSolBeamCoef(1:6))
            END IF

                 ! Interior beam absorptance of glass layers and beam transmittance
                 ! of back exterior window with SHADE

            IF(ShadeFlagBack==IntShadeOn .OR. ShadeFlagBack==ExtShadeOn .OR. ShadeFlagBack==BGShadeOn) THEN
              DO Lay = 1,Construct(ConstrNumBackSh)%TotGlassLayers
                AbsBeamWin(Lay) = POLYF(CosIncBack,Construct(ConstrNumBackSh)%AbsBeamBackCoef(Lay,1:6))
              END DO
              TransBeamWin  = POLYF(CosIncBack,Construct(ConstrNumBackSh)%TransSolBeamCoef(1:6))

            END IF

                 ! Interior beam absorbed by INTERIOR SHADE of back exterior window

            IF(ShadeFlagBack == IntShadeOn) THEN
              IntBeamAbsByShadFac(BackSurfNum) = Boverlap * Construct(ConstrNumBackSh)%AbsDiffBackShade &
                / (Surface(BackSurfNum)%Area + SurfaceWindow(BackSurfNum)%DividerArea)
              BABSZone = BABSZone + Boverlap * Construct(ConstrNumBackSh)%AbsDiffBackShade
            END IF

                 ! Interior beam absorbed by EXTERIOR SHADE of back exterior window

            IF(ShadeFlagBack == ExtShadeOn) THEN
              RGlFront = Construct(ConstrNumBack)%ReflectSolDiffFront
              AbsSh    = Material(Construct(ConstrNumBackSh)%LayerPoint(1))%AbsorpSolar
              RhoSh    = 1.d0-AbsSh-Material(Construct(ConstrNumBackSh)%LayerPoint(1))%Trans
              AShBack  = POLYF(CosIncBack,Construct(ConstrNumBack)%TransSolBeamCoef(1:6)) * &
                AbsSh / (1.d0-RGlFront*RhoSh)
              BABSZone = BABSZone + Boverlap * AshBack
              IntBeamAbsByShadFac(BackSurfNum) = Boverlap * AShBack / &
                    (Surface(BackSurfNum)%Area + SurfaceWindow(BackSurfNum)%DividerArea)
            END IF

                 ! Interior beam absorbed by BETWEEN-GLASS SHADE of back exterior window

            IF(ShadeFlagBack == BGShadeOn) THEN
              rbd1k = Construct(ConstrNumBack)%rbBareSolDiff(1)
              IF(NBackGlass==2) THEN
                t2k = POLYF(CosIncBack,Construct(ConstrNumBack)%tBareSolCoef(2,1:6))
                rfd2k = Construct(ConstrNumBack)%rfBareSolDiff(2)
                TrSh = Material(Construct(ConstrNumBackSh)%LayerPoint(3))%Trans
                RhoSh = Material(Construct(ConstrNumBackSh)%LayerPoint(3))%ReflectShade
                AbsSh = MIN(1.0d0,MAX(0.0d0,1-TrSh-RhoSh))
                AShBack = t2k*(1 + RhoSh*rfd2k + TrSh*rbd1k)*AbsSh
              ELSE  ! NBackGlass = 3
                t3k = POLYF(CosIncBack,Construct(ConstrNumBack)%tBareSolCoef(3,1:6))
                TrSh = Material(Construct(ConstrNumBackSh)%LayerPoint(5))%Trans
                RhoSh = Material(Construct(ConstrNumBackSh)%LayerPoint(5))%ReflectShade
                AbsSh = MIN(1.0d0,MAX(0.0d0,1-TrSh-RhoSh))
                AShBack = t3k*(1 + RhoSh*rfd3k + TrSh*(rbd2k + td2k*rbd1k*td2k))*AbsSh
              END IF
              IntBeamAbsByShadFac(BackSurfNum) = Boverlap * AShBack / Surface(BackSurfNum)%Area
              BABSZone = BABSZone + Boverlap * AShBack
            END IF

                 ! Interior beam absorptance of glass layers and beam absorbed in blind
                 ! of back exterior window with BLIND

            IF(ShadeFlagBack==IntBlindOn .OR. ShadeFlagBack==ExtBlindOn .OR. ShadeFlagBack==BGBlindOn) THEN
              BlNumBack     = SurfaceWindow(BackSurfNum)%BlindNumber
              CALL ProfileAngle(BackSurfNum,SOLCOS,Blind(BlNumBack)%SlatOrientation,ProfAngBack)
              TGlBmBack     = POLYF(CosIncBack,Construct(ConstrNumBack)%TransSolBeamCoef(1:6))
              TBlBmBmBack   = BlindBeamBeamTrans(ProfAngBack,PI-SlatAngBack,Blind(BlNumBack)%SlatWidth,  &
                                       Blind(BlNumBack)%SlatSeparation,Blind(BlNumBack)%SlatThickness)
              TBlBmDiffBack = InterpProfSlatAng(ProfAngBack,SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolBackBeamDiffTrans)

              IF(ShadeFlagBack == IntBlindOn) THEN

                 ! Interior beam absorptance of GLASS LAYERS of exterior back window with INTERIOR BLIND

                RhoBlFront    = InterpProfSlatAng(ProfAngBack,SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolFrontBeamDiffRefl)
                RhoBlDiffFront= InterpSlatAng(SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolFrontDiffDiffRefl)
                RGlBack       = POLYF(CosIncBack,Construct(ConstrNumBack)%ReflSolBeamBackCoef(1:6))
                RGlDiffBack   = Construct(ConstrNumBack)%ReflectSolDiffBack
                DO Lay =1,NBackGlass
                  AbWinBack   = POLYF(CosIncBack,Construct(ConstrNumBack)%AbsBeamBackCoef(Lay,1:6))
                  AGlDiffBack = Construct(ConstrNumBack)%AbsDiffBack(Lay)
                  AbsBeamWin(Lay) = TBlBmBmBack*AbWinBack + &
                    ((TBlBmBmBack*RGlBack*RhoBlFront + TBlBmDiffBack) * AGlDiffBack/ &
                    (1.d0 - RGlDiffBack*RhoBlDiffFront))
                END DO

                 ! Interior beam transmitted by exterior back window with INTERIOR BLIND

                TGlDif = Construct(ConstrNumBack)%TransDiff
                TransBeamWin  = TBlBmBmBack * (TGlBmBack + TGlDif*RGlBack*RhoBlFront/ &
                                 (1.d0-RGlDiffBack*RhoBlDiffFront)) + TBlBmDiffBack*TGlDif/(1-RGlDiffBack*RhoBlDiffFront)

                 ! Interior beam absorbed by BLIND on exterior back window with INTERIOR BLIND

                AbsBlFront    = InterpProfSlatAng(ProfAngBack,SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolFrontBeamAbs)
                AbsBlBack     = InterpProfSlatAng(ProfAngBack,SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolBackBeamAbs)
                AbsBlDiffFront= InterpSlatAng(SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolFrontDiffAbs)
                ABlBack       = AbsBlBack + TBlBmBmBack * RGlBack * AbsBlFront &
                  + (AbsBlDiffFront*RGlDiffBack/(1-RhoBlDiffFront*RGlDiffBack)) &
                  * (RGlBack*TBlBmBmBack*RhoBlFront + TBlBmDiffBack)
                IntBeamAbsByShadFac(BackSurfNum) = Boverlap * &
                  ABlBack / (Surface(BackSurfNum)%Area + SurfaceWindow(BackSurfNum)%DividerArea)
                BABSZone      = BABSZone + Boverlap * ABlBack
              ENDIF

              IF(ShadeFlagBack == ExtBlindOn) THEN

                 ! Interior beam absorptance of GLASS LAYERS of exterior back window with EXTERIOR BLIND

                RGlDiffFront  = Construct(ConstrNumBack)%ReflectSolDiffFront
                RhoBlBack     = InterpProfSlatAng(ProfAngBack,SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolBackBeamDiffRefl)
                DO Lay = 1,NBackGlass
                  AbWinBack   = POLYF(CosIncBack,Construct(ConstrNumBack)%AbsBeamBackCoef(Lay,1:6))
                  AGlDiffFront= Construct(ConstrNumBack)%AbsDiff(Lay)
                  AbsBeamWin(Lay) = AbWinBack + &
                    (TGlBmBack*AGlDiffFront*RhoBlBack / (1.d0 - RhoBlBack*RGlDiffFront))
                END DO

                 ! Interior beam transmitted by exterior back window with EXTERIOR BLIND

                TBlDifDif     = InterpSlatAng(SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolBackDiffDiffTrans)
                RhoBlBmDifBk  = InterpProfSlatAng(ProfAngBack,SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolBackBeamDiffRefl)
                RGlDifFr      = Construct(ConstrNum)%ReflectSolDiffFront
                RhoBlDifDifBk = InterpSlatAng(SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolBackDiffDiffRefl)
                TransBeamWin  = TGlBmBack * ( TBlBmBmBack + TBlBmDiffBack + &
                                  TBlDifDif*RhoBlBmDifBk*RGlDifFr/(1.d0-RhoBlDifDifBk*RGlDifFr) )

                 ! Interior beam absorbed by EXTERIOR BLIND on exterior back window

                AbsBlBack     = InterpProfSlatAng(ProfAngBack,SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolBackBeamAbs)
                AbsBlDiffBack = InterpSlatAng(SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolBackDiffAbs)
                RhoBlDiffBack = InterpSlatAng(SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolBackDiffDiffRefl)
                ABlBack = TGlBmBack*(AbsBlBack + RhoBlBack*RGlDiffFront*AbsBlDiffBack/(1-RhoBlDiffBack*RGlDiffFront))
                BABSZone = BABSZone + Boverlap * ABlBack
                IntBeamAbsByShadFac(BackSurfNum) = Boverlap * &
                  ABlBack / (Surface(BackSurfNum)%Area + SurfaceWindow(BackSurfNum)%DividerArea)
              END IF  ! End of check if exterior blind on back window

              IF(ShadeFlagBack == BGBlindOn) THEN

                t1k     = POLYF(CosIncBack,Construct(ConstrNumBack)%tBareSolCoef(1,1:6))
                t2k     = POLYF(CosIncBack,Construct(ConstrNumBack)%tBareSolCoef(2,1:6))
                af2k    = POLYF(CosIncBack,Construct(ConstrNumBack)%afBareSolCoef(2,1:6))
                ab1k    = POLYF(CosIncBack,Construct(ConstrNumBack)%abBareSolCoef(1,1:6))
                ab2k    = POLYF(CosIncBack,Construct(ConstrNumBack)%abBareSolCoef(2,1:6))
                rb1k    = POLYF(CosIncBack,Construct(ConstrNumBack)%rbBareSolCoef(1,1:6))
                rb2k    = POLYF(CosIncBack,Construct(ConstrNumBack)%rbBareSolCoef(2,1:6))
                td1k    = Construct(ConstrNumBack)%tBareSolDiff(1)
                td2k    = Construct(ConstrNumBack)%tBareSolDiff(2)
                afd2k   = Construct(ConstrNumBack)%afBareSolDiff(2)
                abd1k   = Construct(ConstrNumBack)%abBareSolDiff(1)
                abd2k   = Construct(ConstrNumBack)%abBareSolDiff(2)
                rfd2k   = Construct(ConstrNumBack)%rfBareSolDiff(2)
                rbd1k   = Construct(ConstrNumBack)%rbBareSolDiff(1)
                rbd2k   = Construct(ConstrNumBack)%rbBareSolDiff(2)
                tfshBBk = BlindBeamBeamTrans(ProfAngBack,SlatAngBack,Blind(BlNumBack)%SlatWidth, &
                                  Blind(BlNumBack)%SlatSeparation,Blind(BlNumBack)%SlatThickness)
                tfshBdk = InterpProfSlatAng(ProfAngBack,SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolFrontBeamDiffTrans)
                tfshdk  = InterpSlatAng(SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolFrontDiffDiffTrans)
                tbshBBk = BlindBeamBeamTrans(ProfAngBack,PI-SlatAngBack,Blind(BlNumBack)%SlatWidth, &
                                  Blind(BlNumBack)%SlatSeparation,Blind(BlNumBack)%SlatThickness)
                tbshBdk = InterpProfSlatAng(ProfAngBack,SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolBackBeamDiffTrans)
                tbshdk  = InterpSlatAng(SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolBackDiffDiffTrans)
                rfshBk  = InterpProfSlatAng(ProfAngBack,SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolFrontBeamDiffRefl)
                rbshBk  = InterpProfSlatAng(ProfAngBack,SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolBackBeamDiffRefl)
                rfshdk  = InterpSlatAng(SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolFrontDiffDiffRefl)
                rbshdk  = InterpSlatAng(SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolBackDiffDiffRefl)
                afshdk  = InterpSlatAng(SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolFrontDiffAbs)
                abshdk  = InterpSlatAng(SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolBackDiffAbs)
                afshBk  = InterpProfSlatAng(ProfAngBack,SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolFrontBeamAbs)
                abshBk  = InterpProfSlatAng(ProfAngBack,SlatAngBack,VarSlatsBack,Blind(BlNumBack)%SolBackBeamAbs)

                IF(NBackGlass==3) THEN
                  t3k   = POLYF(CosIncBack,Construct(ConstrNumBack)%tBareSolCoef(3,1:6))
                  af3k  = POLYF(CosIncBack,Construct(ConstrNumBack)%afBareSolCoef(3,1:6))
                  ab3k  = POLYF(CosIncBack,Construct(ConstrNumBack)%abBareSolCoef(3,1:6))
                  afd3k = Construct(ConstrNumBack)%afBareSolDiff(3)
                  rfd3k = Construct(ConstrNumBack)%rfBareSolDiff(3)
                END IF

                 ! Interior beam absorptance of GLASS LAYERS of exterior back window with BETWEEN-GLASS BLIND

                IF(NBackGlass==2) THEN
                  AbsBeamWin(2) = ab2k + t2k*tbshBBk*rb1k*tfshBBk*af2k + &
                     t2k*(tbshBBk*rb1k*tfshBdk + tbshBdk*rbd1k*tfshdk + rbshBk*(1.0d0 + rfd2k*rbshdk))*afd2k
                  AbsBeamWin(1) = t2k*tbshBBk*ab1k + t2k*(rbshBk*rfd2k*tbshdk + tbshBdk*(1.0d0 + rbd1k*rfshdk))*abd1k
                ELSE  ! NBackGlass = 3
                  AbsBeamWin(3) = ab3k + t3k*tbshBBk*(rb2k + t2k*rb1k*t2k)*tfshBBk*af3k + &
                     t3k*(tbshBdk*rbd2k*tfshdk + tbshBdk*td2k*rbd1k*td2k*tfshdk + rbshBk*(1.0d0 + rfd3k*rbshdk))*afd3k
                  AbsBeamWin(2) = t3k*tbshBBk*(ab2k + t2k*rb1k*(af2k + t2k*rfshBk*abd2k)) + &
                     t3k*(tbshBdk + tbshBdk*(rbd2k + td2k*rbd1k*td2k)*rfshdk + rbshBk*rfd3k*tbshdk)*abd2k + &
                     t3k*tbshBdk*td2k*rbd1k*afd2k
                  AbsBeamWin(1) = t3k*tbshBBk*(t2k*ab1k + (rb2k + t2k*rb1k*t2k)*rfshBk*td2k*abd1k) + &
                     t3k*(rbshBk*rfd3k*tbshdk + tbshBdk*(1.0d0 + rbd2k*rfshdk + td2k*rbd2k*td2k*rfshdk))*td2k*abd1k
                END IF

                 ! Interior beam transmitted by exterior back window with BETWEEN-GLASS BLIND

                IF(NBackGlass==2) THEN
                  TransBeamWin = t2k*tbshBBk*t1k + &
                     t2k*(tbshBBk*rb1k*rfshBk + rbshBk*rfd2k*tbshdk + tbshBdk*(1.0d0 + rbd1k*rfshdk))*td1k
                ELSE  ! NGlass = 3
                  TransBeamWin = t3k*tbshBBk*t2k*t1k + &
                     t3k*(tbshBBk*(rb2k*rfshBk + t2k*rb1k*t2k*rfshBk) + rbshBk*rfd3k*tbshdk + &
                        tbshBdk*(1.0d0+ rbd2k*rfshdk + td2k*rbd1k*td2k*rfshdk))*td2k*td1k
                END IF

                 ! Interior beam absorbed by BLIND on exterior back window with BETWEEN-GLASS BLIND

                IF(NBackGlass==2) THEN
                  ABlBack = t2k*(abshBk + tbshBBk*rb1k*afshBk + rbshBk*rfd2k*abshdk + tbshBdk*rbd1k*afshdk)
                ELSE  ! NBackGlass = 3
                  ABlBack = t3k*abshBk + t3k*tbshBBk*(rb2k + t2k*rb1k*t2k)*afshBk + t3k*rbshBk*rfd3k*abshdk + &
                     t3k*tbshBdk*(rbd2k + td2k*rbd1k*td2k)*afshdk
                END IF

                BABSZone = BABSZone + Boverlap * ABlBack
                IntBeamAbsByShadFac(BackSurfNum) = Boverlap * ABlBack/Surface(BackSurfNum)%Area

              END IF  ! End of check if between-glass blind is on back window

            END IF  ! End of check if blind is on back window

            IF(ShadeFlagBack == ExtScreenOn) THEN

              ! Interior beam absorptance of GLASS LAYERS of exterior back window with EXTERIOR SCREEN
              ScNumBack     = SurfaceWindow(BackSurfNum)%ScreenNumber
              TGlBmBack     = POLYF(CosIncBack,Construct(ConstrNumBack)%TransSolBeamCoef(1:6))
              RGlDiffFront  = Construct(ConstrNumBack)%ReflectSolDiffFront
              TScBmBmBack   = SurfaceScreens(ScNumBack)%BmBmTransBack
              TScBmDiffBack = SurfaceScreens(ScNumBack)%BmDifTransBack
              RScBack       = SurfaceScreens(ScNumBack)%ReflectSolBeamFront
              RScDifBack   = SurfaceScreens(ScNumBack)%DifReflect
              DO Lay = 1,NBackGlass
                AbWinBack   = POLYF(CosIncBack,Construct(ConstrNumBack)%AbsBeamBackCoef(Lay,1:6))
                AGlDiffFront= Construct(ConstrNumBack)%AbsDiff(Lay)
                AbsBeamWin(Lay) = AbWinBack + (TGlBmBack*AGlDiffFront*RScBack / (1.d0 - RScDifBack*RGlDiffFront))
              END DO

               ! Interior beam transmitted by exterior back window with EXTERIOR SCREEN

              TScDifDif     = SurfaceScreens(ScNumBack)%DifDifTrans
              RScBmDifBk       = SurfaceScreens(ScNumBack)%ReflectSolBeamBack
              RGlDifFr      = Construct(ConstrNum)%ReflectSolDiffFront
              RScDifDifBk   = SurfaceScreens(ScNumBack)%DifReflect
              TransBeamWin  = TGlBmBack * ( TScBmBmBack + TScBmDiffBack + &
                                TScDifDif*RScBmDifBk*RGlDifFr/(1.d0-RScDifDifBk*RGlDifFr) )

               ! Interior beam absorbed by EXTERIOR SCREEN on exterior back window

              AbsScBack     = SurfaceScreens(ScNumBack)%AbsorpSolarBeamBack
              AbsScDiffBack = SurfaceScreens(ScNumBack)%DifScreenAbsorp
              RScDiffBack   = SurfaceScreens(ScNumBack)%ReflectSolBeamFront
              AScBack = TGlBmBack*(AbsScBack + RScBack*RGlDiffFront*AbsScDiffBack/(1.d0-RScDiffBack*RGlDiffFront))
              BABSZone = BABSZone + Boverlap * AScBack
              IntBeamAbsByShadFac(BackSurfNum) = Boverlap * &
                AScBack / (Surface(BackSurfNum)%Area + SurfaceWindow(BackSurfNum)%DividerArea)
            END IF  ! End of check if exterior screen on back window

                 ! Interior beam absorptance of glass layers of back exterior window with SWITCHABLE GLAZING

            IF(ShadeFlagBack == SwitchableGlazing .AND. Surface(BackSurfNum)%ExtBoundCond == 0) THEN

              SwitchFacBack   = SurfaceWindow(BackSurfNum)%SwitchingFactor
              DO Lay = 1,NBackGlass
                AbsBeamWinSh(Lay)  = POLYF(CosIncBack,Construct(ConstrNumBackSh)%AbsBeamBackCoef(Lay,1:6))
              END DO
              DO Lay = 1,NBackGlass
                AbsBeamWin(Lay)    = InterpSw(SwitchFac,AbsBeamWin(Lay),AbsBeamWinSh(Lay))
              END DO
              TransBeamWinSh  = POLYF(CosIncBack,Construct(ConstrNumBackSh)%TransSolBeamCoef(1:6))
              TransBeamWin    = InterpSw(SwitchFac,TransBeamWin,TransBeamWinSh)
            END IF

                 ! Sum of interior beam absorbed by all glass layers of back window

            AbsBeamTotWin = 0.0d0
            DO Lay = 1,NBackGlass
              AbsBeamTotWin = AbsBeamTotWin + AbsBeamWin(Lay)
              AWinSurf(BackSurfNum,Lay) = AWinSurf(BackSurfNum,Lay) + &    ![-]
                Boverlap * AbsBeamWin(Lay) / (Surface(BackSurfNum)%Area + SurfaceWindow(BackSurfNum)%DividerArea)
            END DO

                 ! To BABSZon, add interior beam glass absorption and overall beam transmission for this back window

            BABSZone = BABSZone + Boverlap * (AbsBeamTotWin + TransBeamWin)

                 ! Interior beam transmitted to adjacent zone through an interior back window (assumed unshaded);
                 ! this beam radiation is categorized as diffuse radiation in the adjacent zone.

            AdjSurfNum = Surface(BackSurfNum)%ExtBoundCond
            IF(AdjSurfNum > 0) THEN
              AdjZoneNum = Surface(AdjSurfNum)%Zone
              DBZoneIntWin(AdjZoneNum) = DBZoneIntWin(AdjZoneNum) + Boverlap * TransBeamWin  ![m2]
              SurfaceWindow(BackSurfNum)%BmSolTransThruIntWinRep = SurfaceWindow(BackSurfNum)%BmSolTransThruIntWinRep + &
                    Boverlap * TransBeamWin * BeamSolarRad   ![W]
              SurfaceWindow(BackSurfNum)%BmSolTransThruIntWinRepEnergy = SurfaceWindow(BackSurfNum)%BmSolTransThruIntWinRep &
                    * TimeStepZone * SecInHour
            END IF
          END IF  ! End of check if back surface is opaque or window
          BmIncInsSurfAmountRep(BackSurfNum) = BmIncInsSurfAmountRep(BackSurfNum) + Boverlap
          BmIncInsSurfAmountRepEnergy(BackSurfNum) = BmIncInsSurfAmountRep(BackSurfNum) * TimeStepZone * SecInHour
        END DO  ! End of loop over back surfaces
       ELSE IF  (SurfaceWindow(SurfNum)%WindowModelType == WindowBSDFModel) THEN
         ! For complex window calculation goes over outgoing basis directions
         ! for current state
         CurCplxFenState = SurfaceWindow(SurfNum)%ComplexFen%CurrentState

         ! Get construction number which keeps transmittance properties
         IConst = SurfaceWindow(SurfNum)%ComplexFen%State(CurCplxFenState)%Konst

         FenSolAbsPtr = WindowScheduledSolarAbs(SurfNum, IConst)

         ! Solar radiation from this window will be calculated only in case when this window is not scheduled surface gained
         IF (FenSolAbsPtr == 0) THEN
           ! Current incoming direction number (Sun direction)
           IBm = ComplexWind(SurfNum)%Geom(CurCplxFenState)%SolBmIndex(HourOfDay, TimeStep)

           ! Report variables for complex fenestration here
           BSDFBeamDirectionRep(SurfNum) = IBm
           BSDFBeamThetaRep(SurfNum) = ComplexWind(SurfNum)%Geom(CurCplxFenState)%ThetaBm(HourOfDay, TimeStep)
           BSDFBeamPhiRep(SurfNum) = ComplexWind(SurfNum)%Geom(CurCplxFenState)%PhiBm(HourOfDay, TimeStep)

           BaseSurf = Surface(SurfNum)%BaseSurf
           ! Get total number of back surfaces for current window (surface)
           ! Note that it is organized by base surface
           NBkSurf   = ShadowComb(BaseSurf)%NumBackSurf

           IF (.not.ALLOCATED(CFBoverlap)) THEN
             ALLOCATE(CFBoverlap(NBkSurf))
           END IF

           IF (.not.ALLOCATED(CFDirBoverlap)) THEN
             ALLOCATE(CFDirBoverlap(ComplexWind(SurfNum)%Geom(CurCplxFenState)%Trn%NBasis, NBkSurf))
           END IF

           CFBoverlap = 0.0d0
           ! delete values from previous timestep
           AWinCFOverlap = 0.0d0

           ! Calculate effects on all back surfaces for each of basis directions.  Each of basis directions from the back of the
           ! window has to be considered as beam and therefore calcualte CFBoverlap for each of them
           DO CurTrnDir = 1, ComplexWind(SurfNum)%Geom(CurCplxFenState)%Trn%NBasis
             CurLambda = ComplexWind(SurfNum)%Geom(CurCplxFenState)%Trn%Lamda(CurTrnDir)
             DirTrans = Construct(IConst)%BSDFInput%SolFrtTrans(CurTrnDir, IBm)
             ! Now calculate effect of this direction on all back surfaces
             DO IBack = 1, NBkSurf
              CFDirBoverlap(CurTrnDir, IBack) = ComplexWind(SurfNum)%Geom(CurCplxFenState)% &
                Aoverlap(CurTrnDir, IBack) * DirTrans * CurLambda * CosInc
              CFBoverlap(IBack) = CFBoverlap(IBack) + CFDirBoverlap(CurTrnDir, IBack)
             END DO ! DO IBack = 1,MaxBkSurf
           END DO

           ! Summarizing results
           DO IBack = 1, NBkSurf
             BackSurfaceNumber = ShadowComb(BaseSurf)%BackSurf(IBack)
             ConstrNumBack = Surface(BackSurfaceNumber)%Construction

             ! Do not perform any calculation if surface is scheduled for incoming solar radiation
             SurfSolIncPtr = SurfaceScheduledSolarInc(BackSurfaceNumber, ConstrNumBack)

             IF (SurfSolIncPtr == 0) THEN
                ! Surface hit is another complex fenestration
                IF (SurfaceWindow(BackSurfaceNumber)%WindowModelType == WindowBSDFModel) THEN
                  CurBackState = SurfaceWindow(BackSurfaceNumber)%ComplexFen%CurrentState

                  ! Do not take into account this window if it is scheduled for surface gains
                  FenSolAbsPtr = WindowScheduledSolarAbs(BackSurfaceNumber, ConstrNumBack)

                  IF (FenSolAbsPtr == 0) THEN
                    ! Calculate energy loss per each outgoing orientation
                    DO CurTrnDir = 1, ComplexWind(SurfNum)%Geom(CurCplxFenState)%Trn%NBasis
                      DO CurBackDir = 1, ComplexWind(BackSurfaceNumber)%Geom(CurBackState)%Trn%NBasis
                         ! Purpose of this part is to find best match for outgoing beam number of window back surface and incoming beam
                         ! number of complex fenestration which this beam will hit on (back surface again)
                         tempVec1(1) = ComplexWind(SurfNum)%Geom(CurCplxFenState)%sTrn(CurTrnDir)%X
                         tempVec1(2) = ComplexWind(SurfNum)%Geom(CurCplxFenState)%sTrn(CurTrnDir)%Y
                         tempVec1(3) = ComplexWind(SurfNum)%Geom(CurCplxFenState)%sTrn(CurTrnDir)%Z
                         tempVec2(1) = ComplexWind(BackSurfaceNumber)%Geom(CurBackState)%sTrn(CurBackDir)%X
                         tempVec2(2) = ComplexWind(BackSurfaceNumber)%Geom(CurBackState)%sTrn(CurBackDir)%Y
                         tempVec2(3) = ComplexWind(BackSurfaceNumber)%Geom(CurBackState)%sTrn(CurBackDir)%Z
                         curDot = DOT_PRODUCT(tempVec1, tempVec2)
                         IF (CurBackDir == 1) THEN
                           bestDot = curDot
                           bestTrn = CurTrnDir
                           bestBackTrn = CurBackDir
                         ELSE
                           IF (curDot < bestDot) THEN
                             bestDot = curDot
                             bestTrn = CurTrnDir
                             bestBackTrn = CurBackDir
                           END IF
                         END IF
                      END DO
                      ! CurLambda = ComplexWind(BackSurfaceNumber)%Geom(CurBackState)%Trn%Lamda(CurTrnDir)
                      ! Add influence of this exact direction to what stays in the zone.  It is important to note that
                      ! this needs to be done for each outgoing direction
                      BABSZone = BABSZone + CFDirBoverlap(CurTrnDir, IBack) * &
                         &     (1 - SurfaceWindow(BackSurfaceNumber)%ComplexFen%State(CurBackState)%IntegratedBkRefl(bestBackTrn))

                      ! Absorptance from current back direction
                      TotSolidLay = Construct(ConstrNumBack)%TotSolidLayers
                      DO Lay = 1, TotSolidLay
                       !IF (ALLOCATED(Construct(ConstrNumBack)%BSDFInput)) THEN
                         ! CFDirBoverlap is energy transmitted for current basis beam.  It is important to note that AWinOverlap array
                         ! needs to contain flux and not absorbed energy because later in the code this will be multiplied with window
                         ! area
                         AWinCFOverlap(BackSurfaceNumber, Lay) = AWinCFOverlap(BackSurfaceNumber, Lay) + &
                           & Construct(ConstrNumBack)%BSDFInput%Layer(Lay)%BkAbs(1, bestBackTrn) * CFDirBoverlap(CurTrnDir, IBack) &
                           & / Surface(BackSurfaceNumber)%Area
                       !END IF
                      END DO

                      ! Interior beam transmitted to adjacent zone through an interior back window;
                      ! This beam radiation is categorized as diffuse radiation in the adjacent zone.
                      ! Note that this is done for each outgoing direction of exterior window

                      AdjSurfNum = Surface(BackSurfaceNumber)%ExtBoundCond
                      IF(AdjSurfNum > 0) THEN
                         AdjZoneNum = Surface(AdjSurfNum)%Zone
                         DBZoneIntWin(AdjZoneNum) = DBZoneIntWin(AdjZoneNum) + CFDirBoverlap(CurTrnDir, IBack) * &
                           & SurfaceWindow(BackSurfaceNumber)%ComplexFen%State(CurBackState)%IntegratedBkTrans(bestBackTrn)
                         SurfaceWindow(BackSurfaceNumber)%BmSolTransThruIntWinRep = &
                           & SurfaceWindow(BackSurfaceNumber)%BmSolTransThruIntWinRep + &
                           & CFDirBoverlap(CurTrnDir, IBack) * &
                           & SurfaceWindow(BackSurfaceNumber)%ComplexFen%State(CurBackState)%IntegratedBkTrans(bestBackTrn) * &
                           & BeamSolarRad   ![W]
                         SurfaceWindow(BackSurfaceNumber)%BmSolTransThruIntWinRepEnergy = &
                           & SurfaceWindow(BackSurfaceNumber)%BmSolTransThruIntWinRep &
                           & * TimeStepZone * SecInHour
                      END IF
                    END DO
                  END IF
                ELSE
                   IF (Construct(ConstrNumBack)%TransDiff <= 0.0d0) THEN
                     ! Do not take into account this window if it is scheduled for surface gains
                     SurfSolIncPtr = SurfaceScheduledSolarInc(BackSurfaceNumber, ConstrNumBack)

                     IF (SurfSolIncPtr == 0) THEN
                      AbsIntSurf = Construct(ConstrNumBack)%InsideAbsorpSolar
                      AISurf(BackSurfaceNumber) = AISurf(BackSurfaceNumber) + CFBoverlap(IBack) * &
                         AbsIntSurf/Surface(BackSurfaceNumber)%Area
                      BABSZone = BABSZone + CFBoverlap(IBack) * AbsIntSurf
                     END IF
                   ELSE
                     ! Code for mixed windows goes here.  It is same as above code for "ordinary" windows.
                     ! Try to do something which will not produce duplicate code.
                   ENDIF
                ENDIF
             ENDIF
           END DO

           IF (ALLOCATED(CFBoverlap)) DEALLOCATE(CFBoverlap)
           IF (ALLOCATED(CFDirBoverlap)) DEALLOCATE(CFDirBoverlap)
         END IF

       ELSE IF (SurfaceWindow(SurfNum)%WindowModelType == WindowEQLModel) THEN

         DO IBack = 1,MaxBkSurf

            BackSurfNum = BackSurfaces(SurfNum,IBack,HourOfDay,TimeStep)

            IF(BackSurfNum == 0) EXIT   ! No more irradiated back surfaces for this exterior window
            IF ( SurfaceWindow(IBack)%WindowModelType /= WindowEQLModel) CYCLE ! only EQL back window is allowed

            ConstrNumBack = Surface(BackSurfNum)%Construction
            NBackGlass = Construct(ConstrNumBack)%TotGlassLayers
                ! Irradiated (overlap) area for this back surface, projected onto window plane
                ! (includes effect of shadowing on exterior window)
            Aoverlap = OverlapAreas(SurfNum,IBack,HourOfDay,TimeStep)
            Boverlap = TBm * Aoverlap * CosInc ![m2]

            IF(Construct(ConstrNumBack)%TransDiff <= 0.0d0) THEN

                ! Back surface is opaque interior or exterior wall

                AbsIntSurf = Construct(ConstrNumBack)%InsideAbsorpSolar

                ! Check for movable insulation; reproduce code from subr. EvalInsideMovableInsulation;
                ! Can't call that routine here since cycle prevents SolarShadingGeometry from USEing
                ! HeatBalanceSurfaceManager, which contains EvalInsideMovableInsulation
                HMovInsul = 0.0d0
                IF (Surface(BackSurfNum)%MaterialMovInsulInt.GT.0) THEN
                    MovInsulSchedVal = GetCurrentScheduleValue(Surface(BackSurfNum)%SchedMovInsulExt)
                    IF (MovInsulSchedVal.LE.0.0d0) THEN ! Movable insulation not present at current time
                    HMovInsul = 0.0d0
                    ELSE  ! Movable insulation present
                    HMovInsul = 1.0d0/(MovInsulSchedVal*Material(Surface(BackSurfNum)%MaterialMovInsulInt)%Resistance)
                    AbsInt    = Material(Surface(BackSurfNum)%MaterialMovInsulInt)%AbsorpSolar
                    END IF
                END IF
                IF (HMovInsul > 0.0d0) AbsIntSurf = AbsInt  ! Movable inside insulation present

                AISurf(BackSurfNum) = AISurf(BackSurfNum) + &     ![-]
                    Boverlap * AbsIntSurf / Surface(BackSurfNum)%Area
                BABSZone            = BABSZone + Boverlap * AbsIntSurf       ![m2]

            ELSE

                ! Back surface is an interior or exterior window
                ! Note that exterior back windows with and without shades are treated as defined.
                ! Equivalent Layer window model has no distinction when treating windows with and
                ! without shades (interior, inbetween and exterior shades)

                CosIncBack      = ABS(CosIncAng(BackSurfNum,HourOfDay,TimeStep))
                !
                !  Note in equivalent layer window model if storm window exists it is defined as part of
                !  window construction, hence it does not require a separate treatment
                AbsBeamWinEQL   = 0.d0
                TransBeamWin    = 0.d0

                ! Interior beam absorptance of glass layers and beam transmittance of back exterior  &
                ! or interior window (treates windows with/without shades as defined) for this timestep

                ! call the ASHWAT fenestration model for beam radiation here
                CALL CalcEQLOpticalProperty(BackSurfNum, isBEAM, AbsSolBeamBackEQL)

                EQLNum = Construct(ConstrNumBack)%EQLConsPtr
                AbsBeamWinEQL(1:CFS(EQLNum)%NL) = AbsSolBeamBackEQL(1:CFS(EQLNum)%NL,1)
                ! get the interior beam transmitted through back exterior or interior EQL window
                TransBeamWin = AbsSolBeamBackEQL(CFS(EQLNum)%NL+1,1)
                !   Absorbed by the interior shade layer of back exterior window
                IF ( CFS(EQLNum)%L(CFS(EQLNum)%NL)%LTYPE /= ltyGLAZE) THEN
                    IntBeamAbsByShadFac(BackSurfNum) = Boverlap * AbsSolBeamBackEQL(CFS(EQLNum)%NL,1) &
                           / (Surface(BackSurfNum)%Area + SurfaceWindow(BackSurfNum)%DividerArea)
                    BABSZone = BABSZone + Boverlap * AbsSolBeamBackEQL(CFS(EQLNum)%NL,1)
                ENDIF
                !   Absorbed by the exterior shade layer of back exterior window
                IF ( CFS(EQLNum)%L(1)%LTYPE /= ltyGLAZE) THEN
                    IntBeamAbsByShadFac(BackSurfNum) = Boverlap * AbsSolBeamBackEQL(1,1) &
                           / (Surface(BackSurfNum)%Area + SurfaceWindow(BackSurfNum)%DividerArea)
                    BABSZone = BABSZone + Boverlap * AbsSolBeamBackEQL(1,1)
                ENDIF

                ! determine the number of glass layers
                NBackGlass = 0
                DO Lay = 1, CFS(EQLNum)%NL
                 IF ( CFS(EQLNum)%L(Lay)%LTYPE /= ltyGLAZE ) CYCLE
                 NBackGlass = NBackGlass + 1
                END DO
                IF (NBackGlass >= 2) THEN
                 ! If the number of glass is greater than 2, in between glass shade can be present
                 DO Lay = 2, CFS(EQLNum)%NL-1
                    IF (CFS(EQLNum)%L(CFS(EQLNum)%NL)%LTYPE /= ltyGLAZE) THEN
                       ! if there is in between shade glass determine the shade absorptance
                       IntBeamAbsByShadFac(BackSurfNum) = IntBeamAbsByShadFac(BackSurfNum) &
                         + Boverlap * AbsSolBeamBackEQL(Lay,1) / Surface(BackSurfNum)%Area
                       BABSZone = BABSZone + Boverlap * AbsSolBeamBackEQL(Lay,1)
                    END IF
                 END DO
                ENDIF
                ! Sum of interior beam absorbed by all glass layers of back window
                AbsBeamTotWin = 0.0d0
                DO Lay = 1, CFS(EQLNum)%NL
                    AbsBeamTotWin = AbsBeamTotWin + AbsBeamWinEQL(Lay)
                    AWinSurf(BackSurfNum,Lay) = AWinSurf(BackSurfNum,Lay) + &    ![-]
                    Boverlap * AbsBeamWinEQL(Lay) / (Surface(BackSurfNum)%Area + SurfaceWindow(BackSurfNum)%DividerArea)
                END DO

                ! To BABSZon, add interior beam glass absorption and overall beam transmission for this back window

                BABSZone = BABSZone + Boverlap * (AbsBeamTotWin + TransBeamWin)

                ! Interior beam transmitted to adjacent zone through an interior back window (assumed unshaded);
                ! this beam radiation is categorized as diffuse radiation in the adjacent zone.

                AdjSurfNum = Surface(BackSurfNum)%ExtBoundCond
                IF(AdjSurfNum > 0) THEN
                    AdjZoneNum = Surface(AdjSurfNum)%Zone
                    DBZoneIntWin(AdjZoneNum) = DBZoneIntWin(AdjZoneNum) + Boverlap * TransBeamWin  ![m2]
                    SurfaceWindow(BackSurfNum)%BmSolTransThruIntWinRep = SurfaceWindow(BackSurfNum)%BmSolTransThruIntWinRep + &
                        Boverlap * TransBeamWin * BeamSolarRad   ![W]
                    SurfaceWindow(BackSurfNum)%BmSolTransThruIntWinRepEnergy = SurfaceWindow(BackSurfNum)%BmSolTransThruIntWinRep &
                        * TimeStepZone * SecInHour
                END IF
            END IF  ! End of check if back surface is opaque or window
            BmIncInsSurfAmountRep(BackSurfNum) = BmIncInsSurfAmountRep(BackSurfNum) + Boverlap
            BmIncInsSurfAmountRepEnergy(BackSurfNum) = BmIncInsSurfAmountRep(BackSurfNum) * TimeStepZone * SecInHour
         END DO  ! End of loop over back surfaces

         !  *****************************

       END IF ! IF (SurfaceWindow(SurfNum)%WindowModelType /= WindowBSDFModel) THEN
      ELSE  ! Simple interior solar distribution. All beam from exterior windows falls on floor;
            ! some of this is absorbed/transmitted, rest is reflected to other surfaces.

        DO FloorNum = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
          ! In following, ISABSF is zero except for nominal floor surfaces
          IF (.not. Surface(FloorNum)%HeatTransSurf) CYCLE
          IF(ISABSF(FloorNum) <= 0.0d0 .OR. FloorNum == SurfNum) CYCLE  ! Keep only floor surfaces
          FlConstrNum = Surface(FloorNum)%Construction

          BTOTWinZone = TBm * SunLitFract * Surface(SurfNum)%Area * CosInc * InOutProjSLFracMult  ![m2]

          IF(Construct(FlConstrNum)%TransDiff <= 0.0d0) THEN
              ! Opaque surface

            AISurf(FloorNum) = AISurf(FloorNum) + BTOTWinZone*ISABSF(FloorNum)/Surface(FloorNum)%Area  ![-]
          ELSE
              ! Window

              ! Note that diffuse solar absorptance is used here for floor windows even though we're
              ! dealing with incident beam radiation. This is because, for this simple interior distribution,
              ! the beam radiation from exterior windows is assumed to be uniformly distributed over the
              ! floor and so it makes no sense to use directional absorptances. Note also that floor windows
              ! are assumed to not have blinds or shades in this calculation.
              ! For the case of the floor window a complex fenestration (strange situation) the correct back
              ! diffuse layer absorptions have already been put into the construction
            IF(SurfaceWindow(FloorNum)%StormWinFlag==1) FlConstrNum = Surface(FloorNum)%StormWinConstruction
            AbsBeamTotWin = 0.0d0
            DO Lay = 1,Construct(FlConstrNum)%TotGlassLayers
              AbsBeamTotWin = AbsBeamTotWin + Construct(FlConstrNum)%AbsDiffBack(Lay)
            END DO
              ! In the following we have to multiply by the AbsDiffBack(Lay)/AbsBeamTotWin ratio to get the
              ! layer by layer absorbed beam since ISABSF(FloorNum) is proportional to AbsBeamTotWin
              ! (see ComputeIntSolarAbsorpFactors).
            DO Lay = 1,Construct(FlConstrNum)%TotGlassLayers
              AWinSurf(FloorNum,Lay) = AWinSurf(FloorNum,Lay) + &    ![-]
                Construct(FlConstrNum)%AbsDiffBack(Lay)/AbsBeamTotWin * &
                BTOTWinZone*ISABSF(FloorNum)/Surface(FloorNum)%Area
            END DO
          END IF

          BABSZone = BABSZone + BTOTWinZone*ISABSF(FloorNum)  ![m2]

          AdjSurfNum = Surface(FloorNum)%ExtBoundCond
          IF(Construct(FlConstrNum)%TransDiff > 0.0d0 .AND. AdjSurfNum > 0) THEN

            ! Window in an interior floor

            AdjZoneNum = Surface(AdjSurfNum)%Zone

            ! Contribution (assumed diffuse) to adjacent zone of beam radiation passing
            ! through this window
            DBZoneIntWin(AdjZoneNum) = DBZoneIntWin(AdjZoneNum) + &
              BTOTWinZone * ISABSF(FloorNum) * Construct(FlConstrNum)%TransDiff / AbsBeamTotWin

            BABSZone = BABSZone + &
              BTOTWinZone * ISABSF(FloorNum) * Construct(FlConstrNum)%TransDiff / AbsBeamTotWin
          END IF

        END DO  ! End of loop over floor sections
      END IF  ! End of check on complex vs. simple interior solar distribution

    END IF  ! End of sunlit fraction > 0 test

  END DO  ! End of first loop over surfaces in zone

  ! It is importatnt to do this only one time
  !IF (ZoneNum == 1) THEN
  BABSZoneSSG = 0.0d0
  BTOTZoneSSG = 0.0d0
  DO iSSG = 1, TotSurfIncSolSSG
    SurfNum = SurfIncSolSSG(iSSG)%SurfPtr
    ! do calculation only if construction number match.
    IF (SurfIncSolSSG(iSSG)%ConstrPtr == Surface(SurfNum)%Construction) THEN
      IF (Surface(SurfNum)%Zone == ZoneNum) THEN
        AbsIntSurf = Construct(Surface(SurfNum)%Construction)%InsideAbsorpSolar
        !SolarIntoZone = GetCurrentScheduleValue(SurfIncSolSSG(iSSG)%SchedPtr) * Surface(SurfNum)%Area
        SolarIntoZone = GetCurrentScheduleValue(SurfIncSolSSG(iSSG)%SchedPtr)
        AISurf(SurfNum) = SolarIntoZone * AbsIntSurf
        BABSZoneSSG = BABSZoneSSG +  AISurf(SurfNum) * Surface(SurfNum)%Area
        BTOTZoneSSG = BTotZoneSSG + SolarIntoZone * Surface(SurfNum)%Area
      END IF
    END IF
  END DO
  DBZoneSSG(ZoneNum) = BTotZoneSSG - BABSZoneSSG
  !END IF

  DBZone(ZoneNum) = BTOTZone - BABSZone

  IF(DBZone(ZoneNum) < 0.0d0) THEN
    DBZone(ZoneNum) = 0.0d0
  END IF

  ! Variables for reporting
  DO SurfNum = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
    IF (.NOT. Surface(SurfNum)%HeatTransSurf) CYCLE
    IF(SolarDistribution == FullInteriorExterior) THEN
      BmIncInsSurfAmountRep(SurfNum) = BeamSolarRad * BmIncInsSurfAmountRep(SurfNum)
      BmIncInsSurfAmountRepEnergy(SurfNum) = BmIncInsSurfAmountRep(SurfNum) * TimeStepZone * SecInHour
      BmIncInsSurfIntensRep(SurfNum) = BmIncInsSurfAmountRep(SurfNum) /  &
          (Surface(SurfNum)%Area + SurfaceWindow(SurfNum)%DividerArea)
    ELSE  ! Simple interior solar distribution. All beam falls on floor.
      IF(ISABSF(SurfNum) > 0.0d0 .AND. Surface(SurfNum)%HeatTransSurf) THEN

        IF (Zone(ZoneNum)%FloorArea > 0.0d0) THEN
          ! spread onto all floor surfaces, these may or may not be called "floor"
          BmIncInsSurfIntensRep(SurfNum) = BeamSolarRad * BTOTZone/Zone(ZoneNum)%FloorArea
        ELSEIF (Zone(ZoneNum)%TotalSurfArea > 0.d0) THEN
          ! spread onto all interior surfaces
          BmIncInsSurfIntensRep(SurfNum) =BeamSolarRad * BTOTZone/Zone(ZoneNum)%TotalSurfArea
        ELSE !divide be zero otherwise
          BmIncInsSurfIntensRep(SurfNum) = 0.d0
        ENDIF
      ENDIF
      BmIncInsSurfAmountRep(SurfNum) = Surface(SurfNum)%Area * BmIncInsSurfIntensRep(SurfNum)
      BmIncInsSurfAmountRepEnergy(SurfNum) = BmIncInsSurfAmountRep(SurfNum) * TimeStepZone * SecInHour
    END IF
    IF (Surface(SurfNum)%Class == SurfaceClass_Window .OR. Surface(SurfNum)%Class == SurfaceClass_TDD_Dome) THEN

      SurfaceWindow(SurfNum)%IntBeamAbsByShade = IntBeamAbsByShadFac(SurfNum)
      SurfaceWindow(SurfNum)%ExtBeamAbsByShade = BeamSolarRad * ExtBeamAbsByShadFac(SurfNum)

      IF ((Surface(SurfNum)%ExtBoundCond == ExternalEnvironment) .OR. &
          (Surface(SurfNum)%ExtBoundCond == OtherSideCondModeledExt) )  THEN

        ShadeFlag = SurfaceWindow(SurfNum)%ShadingFlag
        BlNum = SurfaceWindow(SurfNum)%BlindNumber
        ShelfNum = Surface(SurfNum)%Shelf
        IF (ShelfNum > 0) THEN ! Outside daylighting shelf
          OutShelfSurf = Shelf(ShelfNum)%OutSurf
        ELSE
          OutShelfSurf = 0
        END IF

        ! This lookup may be avoid if this 2nd surf loop can be combined with the 1st
        IF (SurfaceWindow(SurfNum)%OriginalClass == SurfaceClass_TDD_Diffuser) THEN
          PipeNum = FindTDDPipe(SurfNum)
          SurfNum2 = TDDPipe(PipeNum)%Dome

          DifSolarInc = DifSolarRad * AnisoSkyMult(SurfNum2) + GndSolarRad * Surface(SurfNum2)%ViewFactorGround

          SkySolarTrans = DifSolarRad * TransTDD(PipeNum, CosInc, SolarAniso) * AnisoSkyMult(SurfNum2)
          GndSolarTrans = GndSolarRad * TDDPipe(PipeNum)%TransSolIso * Surface(SurfNum2)%ViewFactorGround

          WinBmSolar(SurfNum)  = BeamSolarRad * WinTransBmSolar(SurfNum)
          WinDifSolar(SurfNum) = SkySolarTrans * Surface(SurfNum)%Area + GndSolarTrans * Surface(SurfNum)%Area
          WinBmSolarEnergy(SurfNum)  = WinBmSolar(SurfNum) * TimeStepZone * SecInHour
          WinDifSolarEnergy(SurfNum) = WinDifSolar(SurfNum) * TimeStepZone * SecInHour

          WinTransSolar(SurfNum) = WinBmSolar(SurfNum) + WinDifSolar(SurfNum) ![W]
          WinTransSolarEnergy(SurfNum) = WinTransSolar(SurfNum) * TimeStepZone * SecInHour

          TDDPipe(PipeNum)%TransmittedSolar = WinTransSolar(SurfNum)
          !TDDPipe(PipeNum)%TransSolBeam = TBmBm ! Reported above
          IF (DifSolarInc > 0) THEN
            TDDPipe(PipeNum)%TransSolDiff = (SkySolarTrans + GndSolarTrans) / DifSolarInc
          ELSE
            TDDPipe(PipeNum)%TransSolDiff = 0.0d0
          END IF

        ELSE IF (OutShelfSurf > 0) THEN ! Outside daylighting shelf
          ShelfSolarRad = (BeamSolarRad * SunlitFrac(OutShelfSurf,HourOfDay,TimeStep) &
            * CosIncAng(OutShelfSurf,HourOfDay,TimeStep) + DifSolarRad * AnisoSkyMult(OutShelfSurf)) &
            * Shelf(ShelfNum)%OutReflectSol

          DifSolarInc = DifSolarRad * AnisoSkyMult(SurfNum) + GndSolarRad * Surface(SurfNum)%ViewFactorGround &
            + ShelfSolarRad * Shelf(ShelfNum)%ViewFactor

          WinBmSolar(SurfNum)  = BeamSolarRad * WinTransBmSolar(SurfNum)
          WinDifSolar(SurfNum) = DifSolarInc * WinTransDifSolar(SurfNum)
          WinBmSolarEnergy(SurfNum)  = WinBmSolar(SurfNum) * TimeStepZone * SecInHour
          WinDifSolarEnergy(SurfNum) = WinDifSolar(SurfNum) * TimeStepZone * SecInHour

          WinTransSolar(SurfNum) = WinBmSolar(SurfNum) + WinDifSolar(SurfNum) ![W]
          WinTransSolarEnergy(SurfNum) = WinTransSolar(SurfNum) * TimeStepZone * SecInHour

        ELSE ! Regular window
          SkySolarInc = SurfaceWindow(SurfNum)%SkySolarInc
          GndSolarInc = SurfaceWindow(SurfNum)%GndSolarInc
          DifSolarInc = SkySolarInc + GndSolarInc
          WinBmSolar(SurfNum)  = BeamSolarRad * WinTransBmSolar(SurfNum)
          !Note: for complex fenestration, WinTransDifSolar has previously been defined using the effective
          ! transmittance for sky and ground diffuse radiation (including beam radiation reflected from the ground)
          ! so these calculations should be correct
          WinDifSolar(SurfNum) = DifSolarInc * WinTransDifSolar(SurfNum)
          WinBmSolarEnergy(SurfNum)  = WinBmSolar(SurfNum) * TimeStepZone * SecInHour
          WinDifSolarEnergy(SurfNum) = WinDifSolar(SurfNum) * TimeStepZone * SecInHour
          IF(ShadeFlag==IntBlindOn.OR.ShadeFlag==ExtBlindOn.OR.ShadeFlag==BGBlindOn) THEN
            IF(Blind(SurfaceWindow(SurfNum)%BlindNumber)%SlatOrientation == Horizontal) THEN
              WinDifSolar(SurfNum) = SkySolarInc * WinTransDifSolarSky(SurfNum) +  &
                                     GndSolarInc * WinTransDifSolarGnd(SurfNum)
              WinDifSolarEnergy(SurfNum) = WinDifSolar(SurfNum) * TimeStepZone * SecInHour
            END IF
          END IF

          WinTransSolar(SurfNum) = WinBmSolar(SurfNum) + WinDifSolar(SurfNum) ![W]
          WinTransSolarEnergy(SurfNum) = WinTransSolar(SurfNum) * TimeStepZone * SecInHour

        END IF

        !added TH 12/9/2009, CR 7907 & 7809
        WinBmBmSolar(SurfNum)  = BeamSolarRad * WinTransBmBmSolar

        WinBmDifSolar(SurfNum)  = BeamSolarRad * WinTransBmDifSolar
        WinBmBmSolarEnergy(SurfNum)  = WinBmBmSolar(SurfNum) * TimeStepZone * SecInHour
        WinBmDifSolarEnergy(SurfNum)  = WinBmDifSolar(SurfNum) * TimeStepZone * SecInHour

        WinDirSolTransAtIncAngle(SurfNum) = TBmBm + TBmDif ! For TDD:DIFFUSER this is the TDD transmittance

        ! Solar not added by TDD:DOME; added to zone via TDD:DIFFUSER
        IF (Surface(SurfNum)%Class /= SurfaceClass_TDD_Dome) THEN
          ZoneTransSolar(ZoneNum)      = ZoneTransSolar(ZoneNum) + WinTransSolar(SurfNum) ![W]
          ZoneTransSolarEnergy(ZoneNum) = ZoneTransSolar(ZoneNum) * TimeStepZone * SecInHour ![J]
          ZoneBmSolFrExtWinsRep(ZoneNum)  = ZoneBmSolFrExtWinsRep(ZoneNum) + WinBmSolar(SurfNum)
          ZoneDifSolFrExtWinsRep(ZoneNum) = ZoneDifSolFrExtWinsRep(ZoneNum) +  WinDifSolar(SurfNum)
          ZoneBmSolFrExtWinsRepEnergy(ZoneNum)  = ZoneBmSolFrExtWinsRep(ZoneNum) * TimeStepZone * SecInHour ![J]
          ZoneDifSolFrExtWinsRepEnergy(ZoneNum) = ZoneDifSolFrExtWinsRep(ZoneNum) * TimeStepZone * SecInHour ![J]
        END IF

      END IF
    END IF
  END DO  ! End of second loop over surfaces in zone

END DO  ! End of first zone loop

          ! Add interior window contribution to DBZone

DO ZoneNum = 1,NumOfZones
  DBZone(ZoneNum) = DBZone(ZoneNum) + DBZoneIntWin(ZoneNum)
  ZoneBmSolFrIntWinsRep(ZoneNum) = DBZoneIntWin(ZoneNum) * BeamSolarRad
  ZoneBmSolFrIntWinsRepEnergy(ZoneNum) = ZoneBmSolFrIntWinsRep(ZoneNum) * TimeStepZone * SecInHour ![J]
END DO

! RJH - Calculate initial distribution of diffuse solar transmitted by exterior windows into each zone
!       to all interior surfaces in the zone
!       Includes subsequent transmittance of diffuse solar to adjacent zones through interior windows
CALL CalcWinTransDifSolInitialDistribution

RETURN

END SUBROUTINE CalcInteriorSolarDistribution

INTEGER FUNCTION WindowScheduledSolarAbs(SurfNum, ConstNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Simon Vidanovic
          !       DATE WRITTEN   June 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Returns scheduled surface gain object for given surface-construction combination

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  implicit none    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  integer, intent(in) :: SurfNum ! Surface number
  integer, intent(in) :: ConstNum ! Construction number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  integer :: i

  WindowScheduledSolarAbs = 0

  do i = 1, TotFenLayAbsSSG
    if ((FenLayAbsSSG(i)%SurfPtr == SurfNum) .and. (FenLayAbsSSG(i)%ConstrPtr == ConstNum)) then
      WindowScheduledSolarAbs = i
      return
    end if
  end do

  return
END FUNCTION WindowScheduledSolarAbs

INTEGER FUNCTION SurfaceScheduledSolarInc(SurfNum, ConstNum)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Simon Vidanovic
          !       DATE WRITTEN   June 2013
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Returns scheduled surface gain pointer for given surface-construction combination

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:

  implicit none    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  integer, intent(in) :: SurfNum ! Surface number
  integer, intent(in) :: ConstNum ! Construction number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  integer :: i

  SurfaceScheduledSolarInc = 0

  do i = 1, TotSurfIncSolSSG
    if ((SurfIncSolSSG(i)%SurfPtr == SurfNum) .and. (SurfIncSolSSG(i)%ConstrPtr == ConstNum)) then
      SurfaceScheduledSolarInc = i
      return
    end if
  end do

  return
END FUNCTION SurfaceScheduledSolarInc

SUBROUTINE PerformSolarCalculations

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   July 1999
          !       MODIFIED       Sept 2003, FCW: add calls to CalcBeamSolDiffuseReflFactors and
          !                       CalcBeamSolSpecularReflFactors
          !                      Jan 2004, FCW: call CalcDayltgCoefficients if storm window status on
          !                       any window has changed
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine determines if new solar/shading calculations need
          ! to be performed and calls the proper routines to do the job.

          ! METHODOLOGY EMPLOYED:
          ! Users are allowed to enter a value for number of days in each period that
          ! will be used for calculating solar.  (Later, this could be more complicated as
          ! in allowing a number of days in a month or something).  Using this value or the
          ! default (20 days) if nothing is entered by the user, the routine will use the
          ! number of days left to determine if a new set of calculations should be done.
          ! The calculations use the average of "equation of time" and "solar declination"
          ! to perform the calculations.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DaylightingManager,  ONLY: CalcDayltgCoefficients, TotWindowsWithDayl
  USE DataSystemVariables, ONLY: DetailedSolarTimestepIntegration

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) SumDec
  REAL(r64) SumET
  REAL(r64) AvgEqOfTime
  REAL(r64) AvgSinSolarDeclin
  REAL(r64) AvgCosSolarDeclin
  INTEGER PerDayOfYear
  INTEGER Count
  REAL(r64) SinDec
  REAL(r64) EqTime
  !not used INTEGER SurfNum

  ! Calculate sky diffuse shading

  IF(BeginSimFlag) THEN
    CalcSkyDifShading = .TRUE.
    CALL SkyDifSolarShading ! Calculate factors for shading of sky diffuse solar
    CalcSkyDifShading = .FALSE.
  END IF

  IF (BeginEnvrnFlag) THEN
    ShadowingDaysLeft = 0
  ENDIF

  IF (ShadowingDaysLeft <= 0 .OR. DetailedSolarTimestepIntegration) THEN

    IF (.NOT. DetailedSolarTimestepIntegration) THEN
      !  Perform calculations.
      ShadowingDaysLeft=ShadowingCalcFrequency
      IF (DayOfSim + ShadowingDaysLeft > NumOfDayInEnvrn) THEN
        ShadowingDaysLeft=NumOfDayInEnvrn-DayOfSim+1
      ENDIF

      !  Calculate average Equation of Time, Declination Angle for this period

      IF (.not. WarmUpFlag) THEN
        CALL DisplayString('Updating Shadowing Calculations, Start Date='//CurMnDy)
        DisplayPerfSimulationFlag=.true.
      ENDIF

      PerDayOfYear=DayOfYear
      SumDec=0.0d0
      SumET=0.0d0
      DO Count=1,ShadowingDaysLeft
        CALL Sun3(PerDayOfYear,SinDec,EqTime)
        SumDec=SumDec+SinDec
        SumET=SumET+EqTime
        PerDayOfYear=PerDayOfYear+1
      ENDDO

      !  Compute Period Values
      AvgSinSolarDeclin=SumDec/REAL(ShadowingDaysLeft,r64)
      AvgCosSolarDeclin=SQRT(1.0d0-AvgSinSolarDeclin**2)
      AvgEqOfTime=SumET/REAL(ShadowingDaysLeft,r64)
    ELSE
      CALL Sun3(DayOfYear,AvgSinSolarDeclin,AvgEqOfTime)
      AvgCosSolarDeclin=SQRT(1.0d0-AvgSinSolarDeclin**2)
    ENDIF

    CALL CalcPerSolarBeam(AvgEqOfTime,AvgSinSolarDeclin,AvgCosSolarDeclin)

    ! Calculate factors for solar reflection
    IF(CalcSolRefl) THEN
      CALL CalcBeamSolDiffuseReflFactors
      CALL CalcBeamSolSpecularReflFactors
      IF(BeginSimFlag) CALL CalcSkySolDiffuseReflFactors
    END IF

    !  Calculate daylighting coefficients
    CALL CalcDayltgCoefficients

  ENDIF

  IF (.not. WarmUpFlag) THEN
    ShadowingDaysLeft=ShadowingDaysLeft-1
  ENDIF

  ! Recalculate daylighting coefficients if storm window has been added
  ! or removed from one or more windows at beginning of day
  IF(TotWindowsWithDayl > 0 .AND. .not.BeginSimFlag .AND. &
     .not.BeginEnvrnFlag .AND. .not.WarmupFlag .AND. TotStormWin > 0 .AND. StormWinChangeThisDay) THEN
    CALL CalcDayltgCoefficients
  END IF

  RETURN

END SUBROUTINE PerformSolarCalculations

SUBROUTINE SHDRVL(HTSS,SBSNR,Hour,TS)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN
          !       MODIFIED       May 2002 (FCW): allow triangular windows to have reveal.
          !       RE-ENGINEERED  Lawrie, Oct 2000

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine computes the shadowing from a reveal onto a subsurface.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! BLAST/IBLAST code, original author George Walton

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: HTSS     ! Heat transfer surface number of the subsurface
  INTEGER, INTENT(IN) :: SBSNR    ! Subsurface number
  INTEGER, INTENT(IN) :: Hour
  INTEGER, INTENT(IN) :: TS

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: None                       = 0  ! for use with RevealStatus
  INTEGER, PARAMETER :: EntireWindowShadedByReveal = 1  ! for use with RevealStatus
  INTEGER, PARAMETER :: WindowShadedOnlyByReveal   = 2  ! for use with RevealStatus

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) A            ! Area
  REAL(r64) R            ! Depth of the reveal (m)
  INTEGER I                     ! Loop control
  INTEGER N                     ! Vertex number
  INTEGER NS1,NS2               ! Locations in homogeneous coordinate array
  INTEGER NVS                   ! Number of verticies
                                ! note, below dimensions not changed because subsurface still max 4
  REAL(r64) XVT(5)                   ! Projected X coordinates of vertices
  REAL(r64) YVT(5)                   ! Projected Y coordinates of vertices
  LOGICAL :: RevealStatusSet    ! Used to control flow through this subroutine.
                                ! Certain operations performed only if reveal status not yet set.
  INTEGER :: RevealStatus       ! Status of the reveal, takes the parameter values above

          ! FLOW:
  RevealStatus    =  None
  RevealStatusSet = .FALSE.

  IF (.not. CalcSkyDifShading) THEN
    WindowRevealStatus(SBSNR,Hour,TS)=None
  ENDIF

  R = Surface(SBSNR)%Reveal
  IF (R <= 0.0d0) THEN
    RevealStatus    =  None
    RevealStatusSet = .TRUE.
  END IF

  IF (.NOT.RevealStatusSet) THEN

    FRVLHC = LOCHCA + 1
    LOCHCA = LOCHCA + 1
    NVS = Surface(SBSNR)%Sides

    ! Currently (06May02) windows are either rectangles (NVS=4) or triangles (NVS=3)

    SELECT CASE (NVS)

      CASE(4)  ! Rectangular subsurface

            ! Determine vertices of reveal.
            ! Project the subsurface up to the plane of the wall.

        XVT(1) = ShadeV(SBSNR)%XV(1) + R*MAX(XShadowProjection,0.0d0)
        XVT(2) = ShadeV(SBSNR)%XV(2) + R*MAX(XShadowProjection,0.0d0)
        XVT(3) = ShadeV(SBSNR)%XV(3) + R*MIN(XShadowProjection,0.0d0)
        XVT(4) = ShadeV(SBSNR)%XV(4) + R*MIN(XShadowProjection,0.0d0)
        YVT(1) = ShadeV(SBSNR)%YV(1) + R*MIN(YShadowProjection,0.0d0)
        YVT(2) = ShadeV(SBSNR)%YV(2) + R*MAX(YShadowProjection,0.0d0)
        YVT(3) = ShadeV(SBSNR)%YV(3) + R*MAX(YShadowProjection,0.0d0)
        YVT(4) = ShadeV(SBSNR)%YV(4) + R*MIN(YShadowProjection,0.0d0)

              ! Check for complete shadowing.

        IF ((XVT(2) >= XVT(3)).OR.(YVT(2) >= YVT(1))) THEN

          RevealStatus    =  EntireWindowShadedByReveal
          RevealStatusSet = .TRUE.

        ELSE
              ! Re-order vertices to clockwise.

          DO N = 1, NVS
            XVS(N) = XVT(NVS+1-N)
            YVS(N) = YVT(NVS+1-N)
          END DO

              ! Transform to homogeneous coordinates

          CALL HTRANS1(FRVLHC,NVS)
          HCAREA(FRVLHC) = -HCAREA(FRVLHC)
          HCT(FRVLHC)    =  1.0d0

          IF (HCAREA(FRVLHC) <= 0.0d0) THEN
            RevealStatus    =  EntireWindowShadedByReveal
            RevealStatusSet = .TRUE.
          END IF

        END IF

      CASE(3)   ! Triangular window

              ! Project window to outside plane of parent surface

        DO N = 1,3
          XVT(N) = ShadeV(SBSNR)%XV(N) + R*XShadowProjection
          YVT(N) = ShadeV(SBSNR)%YV(N) + R*YShadowProjection
        END DO

              ! Find the overlap between the original window and the projected window
              ! Put XVT,YVT in clockwise order

        DO N = 1, NVS
          XVS(N) = XVT(NVS+1-N)
          YVS(N) = YVT(NVS+1-N)
        END DO

             ! Transform to homogeneous coordinates

        NS1 = LOCHCA + 1
        LOCHCA = NS1
        CALL HTRANS1(NS1,NVS)

             ! Put XV,YV in clockwise order

        DO N = 1, NVS
          XVS(N) = ShadeV(SBSNR)%XV(NVS+1-N)
          YVS(N) = ShadeV(SBSNR)%YV(NVS+1-N)
        END DO

             ! Transform to homogenous coordinates

        NS2 = LOCHCA + 1
        LOCHCA = NS2
        CALL HTRANS1(NS2,NVS)
        HCT(FRVLHC) = 1.0d0

             ! Find overlap

        CALL DeterminePolygonOverlap(NS1,NS2,FRVLHC)
        IF(OverlapStatus == NoOverlap) THEN
            RevealStatus    =  EntireWindowShadedByReveal
            RevealStatusSet = .TRUE.
        END IF

    END SELECT

  END IF

  IF (.NOT.RevealStatusSet) THEN

          ! Check for no shadows on window.

    IF (NSBSHC <= 1) THEN
      RevealStatus    =  WindowShadedOnlyByReveal
      RevealStatusSet = .TRUE.
    ELSE
          ! Reduce all previous shadows to size of reveal opening.
      LOCHCA = FRVLHC
      CALL MULTOL(LOCHCA,FSBSHC,NSBSHC-1)
      IF ( (OverlapStatus == TooManyVertices).OR. &
           (OverlapStatus == TooManyFigures) ) THEN
        RevealStatus    =  None
        RevealStatusSet = .TRUE.
      ELSE
        NRVLHC = LOCHCA - FRVLHC + 1
        IF (NRVLHC <= 1) THEN
          RevealStatus    =  WindowShadedOnlyByReveal
          RevealStatusSet = .TRUE.
        END IF
      END IF
    END IF
  END IF

  IF (.NOT.RevealStatusSet) THEN
          ! Compute sunlit area.
    A = HCAREA(FRVLHC)
    DO I = 2, NRVLHC
      A = A + HCAREA(FRVLHC-1+I)*(1.0d0-HCT(FRVLHC-1+I))
    END DO
    SAREA(HTSS) = A
  END IF

  IF ((RevealStatus == EntireWindowShadedByReveal).OR.(SAREA(HTSS) < 0.0d0)) THEN
    SAREA(HTSS)=0.0d0   ! Window entirely shaded by reveal.
  ELSEIF (RevealStatus == WindowShadedOnlyByReveal) THEN
    SAREA(HTSS)=HCAREA(FRVLHC)    ! Window shaded only by reveal.
  END IF

  IF (.not. CalcSkyDifShading) THEN
    WindowRevealStatus(SBSNR,Hour,TS)=RevealStatus
  ENDIF

  RETURN

END SUBROUTINE SHDRVL

SUBROUTINE SHDSBS(IHOUR,CurSurf,NBKS,NSBS,HTS,TS)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN
          !       MODIFIED       FCW, Oct 2002: Surface%Area --> Surface%Area + SurfaceWindow%DividerArea
          !                       in calculation of SunlitFracWithoutReveal (i.e., use full window area, not
          !                       just glass area.
          !                      TH, May 2009: Bug fixed to address part of CR 7596 - inside reveals
          !                       causing high cooling loads
          !       RE-ENGINEERED  Lawrie, Oct 2000

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine determines the shadowing on subsurfaces and
          ! revises the base surface area accordingly.  It also computes
          ! the effect of transparent subsurfaces.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! BLAST/IBLAST code, original author George Walton

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: IHOUR  ! Hour Index
  INTEGER, INTENT(IN) :: TS     ! Time step Index
  INTEGER, INTENT(IN) :: CurSurf
  INTEGER, INTENT(IN) :: NBKS     ! Number of back surfaces
  INTEGER, INTENT(IN) :: NSBS     ! Number of subsurfaces
  INTEGER, INTENT(IN) :: HTS      ! Heat transfer surface number of the general receiving surf

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) A  ! Area
  INTEGER I  ! Loop control
  INTEGER J  ! Loop control
  INTEGER K  ! Window construction number
  INTEGER N  ! Vertex number
  REAL(r64) SurfArea         ! Surface area. For walls, includes all window frame areas.
                             ! For windows, includes divider area
!  REAL(r64) FrameAreaAdd    ! Additional frame area sunlit
!  REAL(r64) DividerAreaAdd  ! Additional frame area sunlit
  INTEGER HTSS     ! Heat transfer surface number of the subsurface
  INTEGER SBSNR    ! Subsurface number

  IF (NSBS > 0) THEN   ! Action taken only if subsurfaces present

    FSBSHC = LOCHCA + 1

    DO I = 1, NSBS  ! Do for all subsurfaces (sbs).

      SBSNR = ShadowComb(CurSurf)%SubSurf(I)

      HTSS=SBSNR

      K = Surface(SBSNR)%Construction

      IF ( (OverlapStatus /= TooManyVertices).AND. &
           (OverlapStatus /= TooManyFigures) .AND. &
           (SAREA(HTS) > 0.0d0) )              THEN

          ! Re-order vertices to clockwise sequential; compute homogeneous coordinates.
        NVS = Surface(SBSNR)%Sides
        DO N = 1, NVS
          XVS(N) = ShadeV(SBSNR)%XV(NVS+1-N)
          YVS(N) = ShadeV(SBSNR)%YV(NVS+1-N)
        END DO
        LOCHCA = FSBSHC
        CALL HTRANS1(LOCHCA,NVS)
        HCAREA(LOCHCA) = -HCAREA(LOCHCA)
        HCT(LOCHCA)    =  1.0d0
        NSBSHC = LOCHCA - FSBSHC + 1

          ! Determine sunlit area of subsurface due to shadows on general receiving surface.
        IF (NGSSHC > 0) THEN
          CALL MULTOL(LOCHCA,FGSSHC-1,NGSSHC)
          IF ( (OverlapStatus /= TooManyVertices).AND. &
               (OverlapStatus /= TooManyFigures) ) NSBSHC = LOCHCA - FSBSHC + 1
        END IF

      END IF

      IF ( (OverlapStatus == TooManyVertices).OR. &
           (OverlapStatus == TooManyFigures) .OR. &
           (SAREA(HTS) <= 0.0d0) )              THEN  ! General receiving surface totally shaded.

        SAREA(HTSS) = 0.0d0

        IF(IHour > 0 .AND. TS > 0) SunLitFracWithoutReveal(HTSS,IHour,TS) = 0.0d0

      ELSEIF ((NGSSHC <= 0).OR.(NSBSHC == 1)) THEN  ! No shadows.

        SAREA(HTSS) = HCAREA(FSBSHC)
        SAREA(HTS)  = SAREA(HTS) - SAREA(HTSS)    ! Revise sunlit area of general receiving surface.

        ! TH. This is a bug.  SunLitFracWithoutReveal should be a ratio of area
        !IF(IHour > 0 .AND. TS > 0) SunLitFracWithoutReveal(HTSS,IHour,TS) = &
        !      Surface(HTSS)%NetAreaShadowCalc

        ! new code fixed part of CR 7596. TH 5/29/2009
        IF(IHour > 0 .AND. TS > 0) SunLitFracWithoutReveal(HTSS,IHour,TS) = SAREA(HTSS) / Surface(HTSS)%NetAreaShadowCalc


        CALL SHDRVL(HTSS,SBSNR,IHour,TS)   ! Determine shadowing from reveal.

        IF ( (OverlapStatus == TooManyVertices).OR. &
             (OverlapStatus == TooManyFigures) ) SAREA(HTSS) = 0.0d0

      ELSE    ! Compute area.

        A = HCAREA(FSBSHC)
        DO J = 2, NSBSHC
          A = A + HCAREA(FSBSHC-1+J)*(1.0d0-HCT(FSBSHC-1+J))
        END DO
        SAREA(HTSS) = A
        IF (SAREA(HTSS) > 0.0d0) THEN

          SAREA(HTS) = SAREA(HTS) - SAREA(HTSS) ! Revise sunlit area of general receiving surface.

          IF(IHour > 0 .AND. TS > 0) SunLitFracWithoutReveal(HTSS,IHour,TS) = SAREA(HTSS)/Surface(HTSS)%Area

          CALL SHDRVL(HTSS,SBSNR,IHOUR,TS)   ! Determine shadowing from reveal.

          IF ( (OverlapStatus == TooManyVertices).OR. &
               (OverlapStatus == TooManyFigures) ) SAREA(HTSS) = 0.0d0

        ELSE    ! General receiving surface totally shaded.

          SAREA(HTSS) = 0.0d0

        END IF

      END IF

          ! Determine transmittance and absorptances of sunlit window.
      IF (Construct(K)%TransDiff > 0.0d0) THEN

        IF(.NOT.CalcSkyDifShading) THEN  !Overlaps calculation is only done for beam solar
                                         !shading, not for sky diffuse solar shading

          CALL CalcInteriorSolarOverlaps(IHOUR,NBKS,HTSS,CurSurf,TS)

        END IF

      END IF

          ! Error checking.
      SurfArea = Surface(SBSNR)%NetAreaShadowCalc
      SAREA(HTSS)=MAX(0.0d0,SAREA(HTSS))

      SAREA(HTSS)=MIN(SAREA(HTSS),SurfArea)

    END DO  ! End of subsurface loop

  END IF

  RETURN

END SUBROUTINE SHDSBS

SUBROUTINE SUN3(JulianDayOfYear,SineOfSolarDeclination,EquationOfTime)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN
          !       MODIFIED       na
          !       RE-ENGINEERED  Linda K. Lawrie

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine computes the coefficients for determining
          ! the solar position.

          ! METHODOLOGY EMPLOYED:
          ! The expressions are based on least-squares fits of data on p.316 of 'Thermal
          ! Environmental Engineering' by Threlkeld and on p.387 of the ASHRAE Handbook
          ! of Fundamentals (need date of ASHRAE HOF).

          ! REFERENCES:
          ! BLAST/IBLAST code, original author George Walton

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: JulianDayOfYear     ! Julian Day Of Year
  REAL(r64), INTENT(OUT)   :: SineOfSolarDeclination    ! Sine of Solar Declination
  REAL(r64), INTENT(OUT)   :: EquationOfTime            ! Equation of Time (Degrees)

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER, DIMENSION(9) :: SineSolDeclCoef = &  !Fitted coefficients of Fourier series
        (/ .00561800d0, .0657911d0, -.392779d0,   .00064440d0,-.00618495d0, & ! SINE OF DECLINATION
          -.00010101d0,-.00007951d0,-.00011691d0, .00002096d0/)             ! COEFFICIENTS
  REAL(r64), PARAMETER, DIMENSION(9) :: EqOfTimeCoef = &  !Fitted coefficients of Fourier Series
        (/ .00021971d0,-.122649d0,   .00762856d0,-.156308d0,  -.0530028d0,  & ! EQUATION OF TIME
          -.00388702d0,-.00123978d0,-.00270502d0,-.00167992d0/)              ! COEFFICIENTS

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    X     ! Day of Year in Radians (Computed from Input JulianDayOfYear)
  REAL(r64)    COSX  ! COS(X)
  REAL(r64)    SineX ! SIN(X)

  X = .017167d0 * JulianDayOfYear   ! Convert julian date to angle X

          ! Calculate sines and cosines of X
  SineX  = SIN(X)
  CosX   = COS(X)

  SineOfSolarDeclination = SineSolDeclCoef(1) + &
                           SineSolDeclCoef(2)*SineX +  &
                           SineSolDeclCoef(3)*CosX + &
                           SineSolDeclCoef(4)*(SineX*CosX*2.d0) + &
                           SineSolDeclCoef(5)*(CosX**2 - SineX**2) + &
                           SineSolDeclCoef(6)*(SineX*(CosX**2 - SineX**2) + CosX*(SineX*CosX*2.d0)) + &
                           SineSolDeclCoef(7)*(CosX*(CosX**2 - SineX**2) - SineX*(SineX*CosX*2.d0)) + &
                           SineSolDeclCoef(8)*(2.d0*(SineX*CosX*2.d0)*(CosX**2 - SineX**2)) + &
                           SineSolDeclCoef(9)*((CosX**2 - SineX**2)**2 - (SineX*CosX*2.d0)**2)

  EquationOfTime = EqOfTimeCoef(1) + &
                   EqOfTimeCoef(2)*SineX +  &
                   EqOfTimeCoef(3)*CosX + &
                   EqOfTimeCoef(4)*(SineX*CosX*2.d0) + &
                   EqOfTimeCoef(5)*(CosX**2 - SineX**2) + &
                   EqOfTimeCoef(6)*(SineX*(CosX**2 - SineX**2) + CosX*(SineX*CosX*2.d0)) + &
                   EqOfTimeCoef(7)*(CosX*(CosX**2 - SineX**2) - SineX*(SineX*CosX*2.d0)) + &
                   EqOfTimeCoef(8)*(2.d0*(SineX*CosX*2.d0)*(CosX**2 - SineX**2)) + &
                   EqOfTimeCoef(9)*((CosX**2 - SineX**2)**2 - (SineX*CosX*2.d0)**2)

  RETURN

END SUBROUTINE SUN3

SUBROUTINE SUN4(CurrentTime,EqOfTime,SinSolarDeclin,CosSolarDeclin)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN
          !       MODIFIED       na
          !       RE-ENGINEERED  Lawrie, Oct 2000

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine computes solar direction cosines for a given hour.  These
          ! cosines are used in the shadowing calculations.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! BLAST/IBLAST code, original author George Walton

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN) :: CurrentTime ! Time to use in shadowing calculations
  REAL(r64), INTENT(IN) :: EqOfTime        ! Equation of time for current day
  REAL(r64), INTENT(IN) :: SinSolarDeclin  ! Sine of the Solar declination (current day)
  REAL(r64), INTENT(IN) :: CosSolarDeclin  ! Cosine of the Solar declination (current day)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) H        ! Hour angle (before noon = +) (in radians)
  REAL(r64) HrAngle  ! Basic hour angle

          ! Compute the hour angle
  HrAngle = (15.d0*(12.d0-(CurrentTime+EqOfTime))+(TimeZoneMeridian-Longitude))
  H=HrAngle*DegToRadians

          ! Compute the cosine of the solar zenith angle.
  SUNCOS(3) = SinSolarDeclin*SinLatitude + CosSolarDeclin*CosLatitude*COS(H)
  SUNCOS(2) = 0.0d0
  SUNCOS(1) = 0.0d0

  IF (SUNCOS(3) < SunIsUpValue) RETURN    ! Return if sun not above horizon.

          ! Compute other direction cosines.
  SUNCOS(2) = SinSolarDeclin*CosLatitude - CosSolarDeclin*SinLatitude*COS(H)
  SUNCOS(1) = CosSolarDeclin*SIN(H)

  RETURN

END SUBROUTINE SUN4

SUBROUTINE WindowShadingManager

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   December 1998
          !       MODIFIED       November 1999 (FW)
          !                      Aug 2001 (FW): change shading control names, change approach
          !                       to scheduling and glare control, add movable
          !                       insulation controls (mainly for heating reduction)
          !                      Dec 2001 (FW): add slat angle control for blinds
          !                      Aug 2002 (FW): add four new control types:
          !                        OnIfHighOutsideAirTempAndHighSolarOnWindow
          !                        OnIfHighOutsideAirTempAndHighHorizontalSolar
          !                        OnIfHighZoneAirTempAndHighSolarOnWindow
          !                        OnIfHighZoneAirTempAndHighHorizontalSolar
          !                      Dec 2002 (FW): add between-glass shade/blind
          !                      Mar 2003 (FW): allow GlareControlIsActive = .true. only for daylit zones
          !                      Apr 2003 (FW): use SNLoadCoolRate or SNLoadHeatRate only if not first time step
          !                                     (fixes problem when used first time thru and not allocated)
          !                      May 2006 (RR): add exterior window screen
          !                      May 2009 (BG): add EMS actuator override for shade flag and slat angle
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! For windows with shading, selects the shaded construction
          ! that is used in the heat balance calculation, and sets
          ! the window shading flag, which is:
          !  -1: if window has no shading device
          !   0: if shading device is off
          !   1: if interior shade is on
          !   2: if glazing is switched to darker state
          !   3: if exterior shade is on
          !   6: if interior blind is on
          !   7: if exterior blind is on
          !   8: if between-glass shade is on
          !   9: if between-glass blind is on
          !  10: window has interior shade that is off but may be triggered on later
          !       to control daylight glare
          !  20: window has switchable glazing that is unswitched but may be switched later
          !       to control daylight glare or daylight illuminance
          !  30: window has exterior shade that is off but may be triggered on later
          !       to control daylaight glare or daylight illuminance
          !  60: window has interior blind that is off but may be triggered on later
          !       to control daylaight glare or daylight illuminance
          !  70: window has exterior blind that is off but may be triggered on later
          !       to control daylaight glare or daylight illuminance
          !  80: window has between-glass shade that is off but may be triggered on later
          !       to control daylaight glare or daylight illuminance
          !  90: window has between-glass blind that is off but may be triggered on later
          !       to control daylaight glare or daylight illuminance
          ! A "shading device" may be an exterior, interior or between-glass shade or blind,
          ! or the lower-transmitting (dark) state of switchable glazing (e.g., electrochromic).
          ! In all cases, the unshaded condition is represented
          ! by the construction given by window's Surface()%Construction and
          ! the shaded condition is represented by the construction given by
          ! the window's Surface()%ShadedConstruction
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          USE DataHeatBalFanSys, ONLY: MAT
          USE ScheduleManager, ONLY: GetCurrentScheduleValue
          USE DataDaylighting, ONLY: ZoneDaylight
          USE General, ONLY: POLYF

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER            :: ISurf           ! Surface counter
  INTEGER            :: IZone           ! Zone counter
  INTEGER            :: IShadingCtrl    ! Pointer to a window's shading control
  REAL(r64)          :: BeamSolarOnWindow ! Direct solar intensity on window (W/m2)
  REAL(r64)          :: SolarOnWindow   ! Direct plus diffuse solar intensity on window (W/m2)
  REAL(r64)          :: SkySolarOnWindow ! Sky diffuse solar intensity on window (W/m2)
  INTEGER            :: SchedulePtr     ! Schedule pointer
  REAL(r64)          :: HorizSolar      ! Horizontal direct plus diffuse solar intensity
  REAL(r64)          :: SetPoint        ! Control setpoint
  REAL(r64)          :: SetPoint2       ! Second control setpoint
  INTEGER            :: ShType          ! 1 = interior shade is on,
                                        ! 2 = glass is switched to dark state,
                                        ! 3 = exterior shade is on,
                                        ! 4 = exterior screen is on,
                                        ! 6 = interior blind is on,
                                        ! 7 = exterior blind is on,
                                        ! 8 = between-glass shade is on,
                                        ! 9 = between-glass blind is on.
!  CHARACTER(len=32)  :: ShadingType     ! Type of shading (interior shade, interior blind, etc.)
  INTEGER            :: ShadingType     ! Type of shading (interior shade, interior blind, etc.)
  LOGICAL            :: SchedAllowsControl ! True if control schedule is not specified or is
                                           !  specified and schedule value = 1
  LOGICAL            :: GlareControlIsActive ! True if glare control is active
  INTEGER            :: BlNum           ! Blind number
  REAL(r64)          :: InputSlatAngle  ! Slat angle of associated Material:WindowBlind (rad)
  REAL(r64)          :: ProfAng         ! Solar profile angle (rad)
  REAL(r64)          :: SlatAng         ! Slat angle this time step (rad)
  REAL(r64)          :: PermeabilityA,PermeabilityB ! Intermediate variables in blind permeability calc
  REAL(r64)          :: ThetaBase       ! Intermediate slat angle variable (rad)
  REAL(r64)          :: ThetaBlock1, ThetaBlock2 ! Slat angles that just block beam solar (rad)
  REAL(r64),SAVE     :: ThetaBig,ThetaSmall ! Larger and smaller value of ThetaBlock1 and ThetaBlock2, resp.
  REAL(r64),SAVE     :: ThetaMin,ThetaMax ! Minimum and maximum allowed slat angle, resp. (rad)

  INTEGER            :: IConst          ! Construction

  DO ISurf = 1,TotSurfaces
    SurfaceWindow(ISurf)%ExtIntShadePrevTS = SurfaceWindow(ISurf)%ShadingFlag
    SurfaceWindow(ISurf)%ShadingFlag = NoShade
    SurfaceWindow(ISurf)%FracTimeShadingDeviceOn = 0.0d0

    IF(Surface(ISurf)%Class /= SurfaceClass_Window) CYCLE
    IF(Surface(ISurf)%ExtBoundCond /= ExternalEnvironment) CYCLE
    IF(Surface(ISurf)%WindowShadingControlPtr == 0) CYCLE

            ! Initialize switching factor (applicable only to switchable glazing) to unswitched
    SurfaceWindow(ISurf)%SwitchingFactor = 0.0d0

    IConst = Surface(ISurf)%Construction
    ! Vis trans at normal incidence of unswitched glass. Counting the GlazedFrac
    IF (IConst > 0) SurfaceWindow(ISurf)%VisTransSelected = POLYF(1.0d0,Construct(IConst)%TransVisBeamCoef(1)) &
      * SurfaceWindow(ISurf)%GlazedFrac

            ! Window has shading control
    IShadingCtrl = Surface(ISurf)%WindowShadingControlPtr
    ShadingType = WindowShadingControl(IShadingCtrl)%ShadingType
    SurfaceWindow(ISurf)%ShadingFlag = ShadeOff  ! Initialize shading flag to off
    IZone = Surface(ISurf)%Zone
            ! Setpoint for shading
    SetPoint  = WindowShadingControl(IShadingCtrl)%SetPoint
    SetPoint2 = WindowShadingControl(IShadingCtrl)%SetPoint2

!                                           ShType = NoShade           ! =-1 (see DataHeatBalance)
!                                           ShType = ShadeOff          ! =0
    IF(ShadingType == WSC_ST_InteriorShade)      ShType = IntShadeOn        ! =1
    IF(ShadingType == WSC_ST_SwitchableGlazing)  ShType = SwitchableGlazing ! =2
    IF(ShadingType == WSC_ST_ExteriorShade)      ShType = ExtShadeOn        ! =3
    IF(ShadingType == WSC_ST_ExteriorScreen)     ShType = ExtScreenOn       ! =4
    IF(ShadingType == WSC_ST_InteriorBlind)      ShType = IntBlindOn        ! =6
    IF(ShadingType == WSC_ST_ExteriorBlind)      ShType = ExtBlindOn        ! =7
    IF(ShadingType == WSC_ST_BetweenGlassShade)  ShType = BGShadeOn         ! =8
    IF(ShadingType == WSC_ST_BetweenGlassBlind)  ShType = BGBlindOn         ! =9

    SchedAllowsControl = .TRUE.
    SchedulePtr = WindowShadingControl(IShadingCtrl)%Schedule
    IF(SchedulePtr /= 0) THEN
      IF(WindowShadingControl(IShadingCtrl)%ShadingControlIsScheduled .AND. &
         GetCurrentScheduleValue(SchedulePtr) <= 0.0d0) SchedAllowsControl = .FALSE.
    END IF

    GlareControlIsActive = (ZoneDaylight(IZone)%TotalDaylRefPoints > 0 .AND. SunIsUp .AND. &
                            WindowShadingControl(IShadingCtrl)%GlareControlIsActive)

    SolarOnWindow = 0.0d0
    BeamSolarOnWindow = 0.0d0
    HorizSolar = 0.0d0
    IF(SunIsUp) THEN
      SkySolarOnWindow = AnisoSkyMult(ISurf)*DifSolarRad
      BeamSolarOnWindow = BeamSolarRad * CosIncAng(ISurf,HourOfDay,TimeStep)*SunLitFrac(ISurf,HourOfDay,TimeStep)
      SolarOnWindow = BeamSolarOnWindow + SkySolarOnWindow + GndSolarRad*Surface(ISurf)%ViewFactorGround
      HorizSolar = BeamSolarRad*SOLCOS(3) + DifSolarRad
    END IF

            ! Determine whether to deploy shading depending on type of control

    SELECT CASE (WindowShadingControl(IShadingCtrl)%ShadingControlType)

    CASE(WSCT_AlwaysOn) ! 'ALWAYSON'
    SurfaceWindow(ISurf)%ShadingFlag = ShType

    CASE(WSCT_AlwaysOff) ! 'ALWAYSOFF'
    SurfaceWindow(ISurf)%ShadingFlag = ShadeOff

    CASE(WSCT_OnIfScheduled) ! 'ONIFSCHEDULEALLOWS'
    IF(SchedAllowsControl) SurfaceWindow(ISurf)%ShadingFlag = ShType

    CASE(WSCT_HiSolar) ! 'ONIFHIGHSOLARONWINDOW'  ! Direct plus diffuse solar intensity on window
    IF(SunIsUp) THEN
      IF(SolarOnWindow > SetPoint.AND.SchedAllowsControl) THEN
        SurfaceWindow(ISurf)%ShadingFlag = ShType
      ELSE IF(GlareControlIsActive) THEN
        SurfaceWindow(ISurf)%ShadingFlag = 10*ShType
      END IF
    END IF

    CASE(WSCT_HiHorzSolar) ! 'ONIFHIGHHORIZONTALSOLAR'  ! Direct plus diffuse exterior horizontal solar intensity
    IF(SunIsUp) THEN
      IF(HorizSolar > SetPoint.AND.SchedAllowsControl) THEN
        SurfaceWindow(ISurf)%ShadingFlag = ShType
      ELSE IF(GlareControlIsActive) THEN
        SurfaceWindow(ISurf)%ShadingFlag = 10*ShType
      END IF
    END IF

    CASE(WSCT_HiOutAirTemp) ! 'OnIfHighOutdoorAirTemperature'
    IF(Surface(ISurf)%OutDryBulbTemp > SetPoint.AND.SchedAllowsControl) THEN
      SurfaceWindow(ISurf)%ShadingFlag = ShType
    ELSE IF(GlareControlIsActive) THEN
      SurfaceWindow(ISurf)%ShadingFlag = 10*ShType
    END IF

    CASE(WSCT_HiZoneAirTemp) ! 'OnIfHighZoneAirTemperature'  ! Previous time step zone air temperature
    IF(MAT(IZone) > SetPoint.AND.SchedAllowsControl) THEN
      SurfaceWindow(ISurf)%ShadingFlag = ShType
    ELSE IF(GlareControlIsActive) THEN
      SurfaceWindow(ISurf)%ShadingFlag = 10*ShType
    END IF

    CASE(WSCT_OnHiOutTemp_HiSolarWindow) ! 'OnIfHighOutdoorAirTempAndHighSolarOnWindow'  ! Outside air temp and solar on window
    IF(SunIsUp) THEN
      IF(Surface(ISurf)%OutDryBulbTemp > SetPoint.AND.SolarOnWindow > Setpoint2.AND.SchedAllowsControl) THEN
        SurfaceWindow(ISurf)%ShadingFlag = ShType
      ELSE IF(GlareControlIsActive) THEN
        SurfaceWindow(ISurf)%ShadingFlag = 10*ShType
      END IF
    END IF

    CASE(WSCT_OnHiOutTemp_HiHorzSolar) ! 'OnIfHighOutdoorAirTempAndHighHorizontalSolar'  ! Outside air temp and horizontal solar
    IF(SunIsUp) THEN
      IF(Surface(ISurf)%OutDryBulbTemp > SetPoint.AND.HorizSolar > Setpoint2.AND.SchedAllowsControl) THEN
        SurfaceWindow(ISurf)%ShadingFlag = ShType
      ELSE IF(GlareControlIsActive) THEN
        SurfaceWindow(ISurf)%ShadingFlag = 10*ShType
      END IF
    END IF

    CASE(WSCT_OnHiZoneTemp_HiSolarWindow) ! 'ONIFHIGHZONEAIRTEMPANDHIGHSOLARONWINDOW'  ! Zone air temp and solar on window
    IF(SunIsUp) THEN
      IF(MAT(IZone) > SetPoint.AND.SolarOnWindow > Setpoint2.AND.SchedAllowsControl) THEN
        SurfaceWindow(ISurf)%ShadingFlag = ShType
      ELSE IF(GlareControlIsActive) THEN
        SurfaceWindow(ISurf)%ShadingFlag = 10*ShType
      END IF
    END IF

    CASE(WSCT_OnHiZoneTemp_HiHorzSolar) ! 'ONIFHIGHZONEAIRTEMPANDHIGHHORIZONTALSOLAR'  ! Zone air temp and horizontal solar
    IF(SunIsUp) THEN
      IF(MAT(IZone) > SetPoint.AND.HorizSolar > Setpoint2.AND.SchedAllowsControl) THEN
        SurfaceWindow(ISurf)%ShadingFlag = ShType
      ELSE IF(GlareControlIsActive) THEN
        SurfaceWindow(ISurf)%ShadingFlag = 10*ShType
      END IF
    END IF

    CASE(WSCT_HiZoneCooling) ! 'ONIFHIGHZONECOOLING'  ! Previous time step zone sensible cooling rate [W]
    ! In the following, the check on BeginSimFlag is needed since SNLoadCoolRate (and SNLoadHeatRate,
    ! used in other CASEs) are not allocated at this point for the first time step of the simulation.
    IF(.NOT.BeginSimFlag) THEN
      IF(SNLoadCoolRate(IZone) > SetPoint.AND.SchedAllowsControl) THEN
        SurfaceWindow(ISurf)%ShadingFlag = ShType
      ELSE IF(GlareControlIsActive) THEN
        SurfaceWindow(ISurf)%ShadingFlag = 10*ShType
      END IF
    END IF

    CASE(WSCT_HiGlare) ! 'ONIFHIGHGLARE'  ! Daylight glare index at first reference point in the zone.
    ! This type of shading control is done in DayltgInteriorIllum. Glare control is not affected
    ! by control schedule.
    IF(SunIsUp) SurfaceWindow(ISurf)%ShadingFlag = 10*ShType

    CASE(WSCT_MeetDaylIlumSetp) ! 'MEETDAYLIGHTILLUMINANCESETPOINT')  !  Daylight illuminance test is done in DayltgInteriorIllum
    ! Only switchable glazing does daylight illuminance control
    IF(SunIsUp.AND.SchedAllowsControl) SurfaceWindow(ISurf)%ShadingFlag = GlassConditionallyLightened

    CASE(WSCT_OnNightLoOutTemp_OffDay) ! 'OnNightIfLowOutdoorTempAndOffDay'
    IF(.NOT.SunIsUp .AND. Surface(ISurf)%OutDryBulbTemp < SetPoint .AND. SchedAllowsControl) THEN
      SurfaceWindow(ISurf)%ShadingFlag = ShType
    ELSE IF(GlareControlIsActive) THEN
      SurfaceWindow(ISurf)%ShadingFlag = 10*ShType
    END IF

    CASE(WSCT_OnNightLoInTemp_OffDay) ! 'OnNightIfLowInsideTempAndOffDay')
    IF(.NOT.SunIsUp .AND. MAT(IZone) < SetPoint .AND. SchedAllowsControl) THEN
      SurfaceWindow(ISurf)%ShadingFlag = ShType
    ELSE IF(GlareControlIsActive) THEN
      SurfaceWindow(ISurf)%ShadingFlag = 10*ShType
    END IF

    CASE(WSCT_OnNightIfHeating_OffDay) ! 'OnNightIfHeatingAndOffDay'
    IF(.NOT.BeginSimFlag) THEN
      IF(.NOT.SunIsUp .AND. SNLoadHeatRate(IZone) > SetPoint .AND. SchedAllowsControl) THEN
        SurfaceWindow(ISurf)%ShadingFlag = ShType
      ELSE IF(GlareControlIsActive) THEN
        SurfaceWindow(ISurf)%ShadingFlag = 10*ShType
      END IF
    END IF

    CASE(WSCT_OnNightLoOutTemp_OnDayCooling) ! 'OnNightIfLowOutdoorTempAndOnDayIfCooling'
    IF(.NOT.BeginSimFlag) THEN
      IF(.NOT.SunIsUp) THEN  ! Night
        IF(Surface(ISurf)%OutDryBulbTemp < SetPoint .AND. SchedAllowsControl) SurfaceWindow(ISurf)%ShadingFlag = ShType
      ELSE                   ! Day
        IF(SNLoadCoolRate(IZone) > 0.0d0 .AND. SchedAllowsControl) THEN
          SurfaceWindow(ISurf)%ShadingFlag = ShType
        ELSE IF(GlareControlIsActive) THEN
          SurfaceWindow(ISurf)%ShadingFlag = 10*ShType
        END IF
      END IF
    END IF

    CASE(WSCT_OnNightIfHeating_OnDayCooling) ! 'OnNightIfHeatingAndOnDayIfCooling'
    IF(.NOT.BeginSimFlag) THEN
      IF(.NOT.SunIsUp) THEN  ! Night
        IF(SNLoadHeatRate(IZone) > SetPoint .AND. SchedAllowsControl) SurfaceWindow(ISurf)%ShadingFlag = ShType
      ELSE                   ! Day
        IF(SNLoadCoolRate(IZone) > 0.0d0 .AND. SchedAllowsControl) THEN
          SurfaceWindow(ISurf)%ShadingFlag = ShType
        ELSE IF(GlareControlIsActive) THEN
          SurfaceWindow(ISurf)%ShadingFlag = 10*ShType
        END IF
      END IF
    END IF

    CASE(WSCT_OffNight_OnDay_HiSolarWindow) ! 'OffNightAndOnDayIfCoolingAndHighSolarOnWindow'
    IF(.NOT.BeginSimFlag) THEN
      IF(SunIsUp .AND. SNLoadCoolRate(IZone) > 0.0d0 .AND. SchedAllowsControl) THEN
        IF(SolarOnWindow > SetPoint) SurfaceWindow(ISurf)%ShadingFlag = ShType
      ELSE IF(GlareControlIsActive) THEN
        SurfaceWindow(ISurf)%ShadingFlag = 10*ShType
      END IF
    END IF

    CASE(WSCT_OnNight_OnDay_HiSolarWindow) ! 'OnNightAndOnDayIfCoolingAndHighSolarOnWindow'
    IF(.NOT.BeginSimFlag) THEN
      IF(SunIsUp .AND. SNLoadCoolRate(IZone) > 0.0d0 .AND. SchedAllowsControl) THEN
        IF(SolarOnWindow > SetPoint) SurfaceWindow(ISurf)%ShadingFlag = ShType
      ELSE IF(.NOT.SunIsUp .AND. SchedAllowsControl) THEN
        SurfaceWindow(ISurf)%ShadingFlag = ShType
      ELSE IF(GlareControlIsActive) THEN
        SurfaceWindow(ISurf)%ShadingFlag = 10*ShType
      END IF
    END IF

    END SELECT

          ! Set switching factor to fully switched if ShadingFlag = 2
    IF(SurfaceWindow(ISurf)%ShadingFlag == SwitchableGlazing) THEN
      SurfaceWindow(ISurf)%SwitchingFactor = 1.0d0

      ! Added TH 1/20/2010
      ! Vis trans at normal incidence of fully switched glass
      IConst = Surface(ISurf)%ShadedConstruction
      SurfaceWindow(ISurf)%VisTransSelected = POLYF(1.0d0,Construct(IConst)%TransVisBeamCoef(1)) &
        * SurfaceWindow(ISurf)%GlazedFrac
    ENDIF

    ! Slat angle control for blinds

    SurfaceWindow(ISurf)%SlatAngThisTS = 0.0d0
    SurfaceWindow(ISurf)%SlatAngThisTSDeg = 0.0d0
    SurfaceWindow(ISurf)%SlatsBlockBeam = .FALSE.
    IF(SurfaceWindow(ISurf)%ShadingFlag==IntBlindOn.OR.SurfaceWindow(ISurf)%ShadingFlag==10*IntBlindOn.OR. &
       SurfaceWindow(ISurf)%ShadingFlag==ExtBlindOn.OR.SurfaceWindow(ISurf)%ShadingFlag==10*ExtBlindOn.OR. &
       SurfaceWindow(ISurf)%ShadingFlag==BGBlindOn.OR.SurfaceWindow(ISurf)%ShadingFlag==10*BGBlindOn) THEN
         ! Blind in place or may be in place due to glare control
      BlNum = SurfaceWindow(ISurf)%BlindNumber
      IF(BlNum > 0) THEN
        InputSlatAngle = Blind(BlNum)%SlatAngle * DegToRadians

        IF(Blind(BlNum)%SlatWidth > Blind(BlNum)%SlatSeparation .AND. BeamSolarOnWindow > 0.0d0) THEN
          CALL ProfileAngle(ISurf,SOLCOS,Blind(BlNum)%SlatOrientation,ProfAng)
          ThetaBase   = ACOS(COS(ProfAng) * Blind(BlNum)%SlatSeparation/Blind(BlNum)%SlatWidth)
          ! There are two solutions for the slat angle that just blocks beam radiation
          ThetaBlock1 = ProfAng + ThetaBase
          ThetaBlock2 = ProfAng + PI - ThetaBase
          ThetaSmall  = MIN(ThetaBlock1,ThetaBlock2)
          ThetaBig    = MAX(ThetaBlock1,ThetaBlock2)
          ThetaMin    = Blind(BlNum)%MinSlatAngle*DegToRadians
          ThetaMax    = Blind(BlNum)%MaxSlatAngle*DegToRadians
        END IF

        ! TH 5/20/2010, CR 8064: Slat Width <= Slat Separation
        IF(Blind(BlNum)%SlatWidth <= Blind(BlNum)%SlatSeparation .AND. BeamSolarOnWindow > 0.0d0) THEN
          IF (WindowShadingControl(IShadingCtrl)%SlatAngleControlForBlinds == WSC_SAC_BlockBeamSolar) THEN

            CALL ProfileAngle(ISurf,SOLCOS,Blind(BlNum)%SlatOrientation,ProfAng)

            IF (ABS(COS(ProfAng)*Blind(BlNum)%SlatSeparation/Blind(BlNum)%SlatWidth) <= 1.0D0) THEN
              ! set to block 100% of beam solar, not necessarily to block maximum solar (beam + diffuse)
              ThetaBase   = ACOS(COS(ProfAng)*Blind(BlNum)%SlatSeparation/Blind(BlNum)%SlatWidth)
              SurfaceWindow(ISurf)%SlatsBlockBeam = .TRUE.
            ELSE
              ! cannot block 100% of beam solar, turn slats to be perpendicular to sun beam to block maximal beam solar
              ThetaBase = 0.0d0
            ENDIF

            ! There are two solutions for the slat angle that just blocks beam radiation
            ThetaBlock1 = ProfAng + ThetaBase
            ThetaBlock2 = ProfAng - ThetaBase + PI

            ThetaSmall  = MIN(ThetaBlock1,ThetaBlock2)
            ThetaBig    = MAX(ThetaBlock1,ThetaBlock2)
            ThetaMin    = Blind(BlNum)%MinSlatAngle*DegToRadians
            ThetaMax    = Blind(BlNum)%MaxSlatAngle*DegToRadians
         ENDIF
       END IF

        SELECT CASE(WindowShadingControl(IShadingCtrl)%SlatAngleControlForBlinds)

        CASE(WSC_SAC_FixedSlatAngle) ! 'FIXEDSLATANGLE'
        SurfaceWindow(ISurf)%SlatAngThisTS = InputSlatAngle
        IF((SurfaceWindow(ISurf)%SlatAngThisTS <= ThetaSmall .OR. SurfaceWindow(ISurf)%SlatAngThisTS >= ThetaBig) &
             .AND. (Blind(BlNum)%SlatWidth > Blind(BlNum)%SlatSeparation) .AND. &
             (BeamSolarOnWindow > 0.0d0)) SurfaceWindow(ISurf)%SlatsBlockBeam = .TRUE.

        CASE(WSC_SAC_ScheduledSlatAngle) ! 'SCHEDULEDSLATANGLE'
        SurfaceWindow(ISurf)%SlatAngThisTS = GetCurrentScheduleValue(WindowShadingControl(IShadingCtrl)%SlatAngleSchedule)
        SurfaceWindow(ISurf)%SlatAngThisTS = MAX(Blind(BlNum)%MinSlatAngle, &
            MIN(SurfaceWindow(ISurf)%SlatAngThisTS,Blind(BlNum)%MaxSlatAngle))*DegToRadians
        IF((SurfaceWindow(ISurf)%SlatAngThisTS <= ThetaSmall .OR. SurfaceWindow(ISurf)%SlatAngThisTS >= ThetaBig) &
             .AND. (Blind(BlNum)%SlatWidth > Blind(BlNum)%SlatSeparation) .AND. &
              (BeamSolarOnWindow > 0.0d0)) SurfaceWindow(ISurf)%SlatsBlockBeam = .TRUE.

        CASE(WSC_SAC_BlockBeamSolar) ! 'BLOCKBEAMSOLAR'
        IF(BeamSolarOnWindow > 0.0d0) THEN
          IF(Blind(BlNum)%SlatSeparation >= Blind(BlNum)%SlatWidth) THEN
            ! TH 5/20/2010. CR 8064.
            ! The following line of code assumes slats are always vertical/closed to minimize solar penetration
            ! The slat angle can however change if the only goal is to block maximum amount of direct beam solar
            !SurfaceWindow(ISurf)%SlatAngThisTS = 0.0  ! Allows beam penetration but minimizes it

            IF(ThetaSmall >= ThetaMin .AND. ThetaSmall <= ThetaMax) THEN
              SurfaceWindow(ISurf)%SlatAngThisTS = ThetaSmall
            ELSE IF(ThetaBig >= ThetaMin .AND. ThetaBig <= ThetaMax) THEN
              SurfaceWindow(ISurf)%SlatAngThisTS = ThetaBig
            ELSE IF(ThetaSmall < ThetaMin .AND. ThetaBig < ThetaMin) THEN
              SurfaceWindow(ISurf)%SlatAngThisTS = ThetaMin
            ELSE IF(ThetaSmall > ThetaMax .AND. ThetaBig > ThetaMax) THEN
              SurfaceWindow(ISurf)%SlatAngThisTS = ThetaMax
            ELSE  ! ThetaBig > ThetaMax and ThetaSmall < ThetaMin (no-block condition)
              SurfaceWindow(ISurf)%SlatAngThisTS = ThetaMin
            END IF

          ELSE  ! Usual case -- slat width greater than slat separation
            IF(ThetaSmall >= ThetaMin .AND. ThetaSmall <= ThetaMax) THEN
              SurfaceWindow(ISurf)%SlatAngThisTS = ThetaSmall
              SurfaceWindow(ISurf)%SlatsBlockBeam = .TRUE.
            ELSE IF(ThetaBig >= ThetaMin .AND. ThetaBig <= ThetaMax) THEN
              SurfaceWindow(ISurf)%SlatAngThisTS = ThetaBig
              SurfaceWindow(ISurf)%SlatsBlockBeam = .TRUE.
            ELSE IF(ThetaSmall < ThetaMin .AND. ThetaBig < ThetaMin) THEN
              SurfaceWindow(ISurf)%SlatAngThisTS = ThetaMin
              SurfaceWindow(ISurf)%SlatsBlockBeam = .TRUE.
            ELSE IF(ThetaSmall > ThetaMax .AND. ThetaBig > ThetaMax) THEN
              SurfaceWindow(ISurf)%SlatAngThisTS = ThetaMax
              SurfaceWindow(ISurf)%SlatsBlockBeam = .TRUE.
            ELSE  ! ThetaBig > ThetaMax and ThetaSmall < ThetaMin (no-block condition)
              SurfaceWindow(ISurf)%SlatAngThisTS = ThetaMin
            END IF
          END IF
        ELSE
          SurfaceWindow(ISurf)%SlatAngThisTS = InputSlatAngle
        END IF

        END SELECT

        SurfaceWindow(ISurf)%SlatAngThisTSDeg = SurfaceWindow(ISurf)%SlatAngThisTS / DegToRadians
        IF (SurfaceWindow(ISurf)%SlatAngThisTSDegEMSon) THEN
          SurfaceWindow(ISurf)%SlatAngThisTSDeg = SurfaceWindow(ISurf)%SlatAngThisTSDegEMSValue
          SurfaceWindow(ISurf)%SlatAngThisTS = DegToRadians * SurfaceWindow(ISurf)%SlatAngThisTSDeg
       ENDIF
        ! Air flow permeability for calculation of convective air flow between blind and glass
        SlatAng = SurfaceWindow(ISurf)%SlatAngThisTS
        PermeabilityA = SIN(SlatAng) - Blind(BlNum)%SlatThickness/Blind(BlNum)%SlatSeparation
        PermeabilityB = 1.0-(ABS(Blind(BlNum)%SlatWidth*COS(SlatAng)) + Blind(BlNum)%SlatThickness*SIN(SlatAng))/ &
                            Blind(BlNum)%SlatSeparation
        SurfaceWindow(ISurf)%BlindAirFlowPermeability = MIN(1.0d0,MAX(0.0d0,PermeabilityA,PermeabilityB))
      END IF
    END IF  ! End of check if interior or exterior blind in place

!   CALL CalcScreenTransmittance to intialized all screens prior to HB calc's
    IF(SurfaceWindow(ISurf)%ShadingFlag == ExtScreenOn .AND. SunIsUp)THEN
      CALL CalcScreenTransmittance(ISurf)
    END IF

    ! EMS Actuator Point: override setting if ems flag on
    IF (SurfaceWindow(ISurf)%ShadingFlagEMSOn) THEN
      SurfaceWindow(ISurf)%ShadingFlag = SurfaceWindow(ISurf)%ShadingFlagEMSValue
    ENDIF



  END DO  !End of surface loop
Return
END SUBROUTINE WindowShadingManager

SUBROUTINE WindowGapAirflowControl

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   February 2003
          !       MODIFIED       June 2003, FCW: add fatal error for illegal schedule value
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! For airflow windows, determines the airflow in the gap of
          ! double glazing and in the inner gap of triple glazing.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          USE ScheduleManager, ONLY: GetCurrentScheduleValue

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na
          ! INTERFACE BLOCK SPECIFICATIONS
          ! na
          ! DERIVED TYPE DEFINITIONS
          ! na
          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER            :: ISurf           ! Surface counter
  INTEGER            :: SchedulePtr     ! Schedule pointer
  REAL(r64)          :: ScheduleMult    ! Multiplier value from schedule

  DO ISurf = 1,TotSurfaces

    IF(Surface(ISurf)%Class /= SurfaceClass_Window) CYCLE

    SurfaceWindow(ISurf)%AirflowThisTS = 0.0d0
    IF(SurfaceWindow(ISurf)%MaxAirflow == 0.0d0) CYCLE
    IF(Surface(ISurf)%ExtBoundCond /= ExternalEnvironment) CYCLE

    SELECT CASE (SurfaceWindow(ISurf)%AirflowControlType)

    CASE(AirFlowWindow_ControlType_MaxFlow)
      SurfaceWindow(ISurf)%AirflowThisTS = SurfaceWindow(ISurf)%MaxAirflow

    CASE(AirFlowWindow_ControlType_AlwaysOff)
      SurfaceWindow(ISurf)%AirflowThisTS = 0.0d0

    CASE(AirFlowWindow_ControlType_Schedule)
      IF(SurfaceWindow(ISurf)%AirflowHasSchedule) THEN
        SchedulePtr = SurfaceWindow(ISurf)%AirflowSchedulePtr
        ScheduleMult = GetCurrentScheduleValue(SchedulePtr)
        IF(ScheduleMult < 0.0d0 .OR. ScheduleMult > 1.0d0) THEN
          CALL ShowFatalError('Airflow schedule has a value outside the range 0.0 to 1.0 for window=' &
             //TRIM(Surface(ISurf)%Name))
        END IF
        SurfaceWindow(ISurf)%AirflowThisTS = ScheduleMult * SurfaceWindow(ISurf)%MaxAirflow
      END IF

    END SELECT

  END DO  ! End of surface loop

  RETURN
  END SUBROUTINE WindowGapAirflowControl

SUBROUTINE SkyDifSolarShading

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   May 1999
          !       MODIFIED       Sep 2000, FCW: add IR view factor calc
          !                      Sep 2002, FCW: correct error in expression for ground IR view factor.
          !                         Affects only non-vertical surfaces that are shadowed. For these surfaces
          !                         error caused underestimate of IR from ground and shadowing surfaces.
          !                      Dec 2002; LKL: Sky Radiance Distribution now only anisotropic
          !                      Nov 2003: FCW: modify to do sky solar shading of shadowing surfaces
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates factors that account for shading of sky diffuse
          ! solar radiation by shadowing surfaces such as overhangs and detached
          ! shades.
          ! Called by PerformSolarCalculations
          !
          ! For each exterior heat transfer surface calculates the following
          ! ratio (called DifShdgRatioIsoSky in this subroutine):
          !
          !  R1 = (Diffuse solar from sky dome on surface, with shading)/
          !       (Diffuse solar from sky dome on surface, without shading)
          !
          ! To calculate the incident diffuse radiation on a surface the sky
          ! hemisphere is divided into source elements ("patches"). Each patch
          ! is assumed to have the same radiance, i.e. the sky radiance is isotropic.
          ! The irradiance from each patch on a surface is calculated. Then these
          ! irradiances are summed to get the net irradiance on a surface, which
          ! the denominator of R1.
          !
          ! To get the numerator of R1 the same summation is done, but for each surface
          ! and each patch the Shadow subroutine is called to determine how much
          ! radiation from a patch is blocked by shading surfaces.
          !
          ! Also calculated is the following ratio (called DifShdgRatioHoriz in this routine):
          !
          !  R2 = (Diffuse solar from sky horizon band on surface, with shading)/
          !       (Diffuse solar from sky horizon band on surface, without shading)
          !
          ! For this ratio only a band of sky just above the horizon is considered.
          !
          ! R1 and R2 are used in SUBROUTINE AnisoSkyViewFactors, which determines the
          ! sky diffuse solar irradiance on each exterior heat transfer surface each
          ! time step. In that routine the sky radiance distribution is a superposition
          ! of an isotropic distribution,
          ! a horizon brightening distribution and a circumsolar brightening distribution,
          ! where the proportion of each distribution depends
          ! on cloud cover, sun position and other factors. R1 multiplies the irradiance
          ! due to the isotropic component and R2 multiplies the irradiance due to the
          ! horizon brightening component.
          !
          ! Calculates sky and ground IR view factors assuming sky IR is isotropic and
          ! shadowing surfaces are opaque to IR.
          !
          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataSystemVariables, ONLY: DetailedSkyDiffuseAlgorithm

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
INTEGER,PARAMETER  :: NPhi = 6          ! Number of altitude angle steps for sky integration
INTEGER,PARAMETER  :: NTheta = 24       ! Number of azimuth angle steps for sky integration
REAL(r64),PARAMETER     :: Eps = 1.d-10       ! Small number

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER    :: SurfNum           ! Surface counter
INTEGER    :: IPhi              ! Altitude step counter
INTEGER    :: ITheta            ! Azimuth step counter
REAL(r64)  :: DPhi              ! Altitude step size
REAL(r64)  :: DTheta            ! Azimuth step size
REAL(r64)  :: DThetaDPhi        ! Product of DTheta and DPhi
REAL(r64)  :: PhiMin            ! Minimum altitude
REAL(r64)  :: Phi               ! Altitude angle
REAL(r64)  :: Theta             ! Azimuth angle
REAL(r64)  :: CosPhi            ! Cosine of Phi
REAL(r64)  :: Fac1WoShdg        ! Intermediate calculation factor, without shading
REAL(r64)  :: FracIlluminated   ! Fraction of surface area illuminated by a sky patch
REAL(r64)  :: Fac1WithShdg      ! Intermediate calculation factor, with shading
REAL(r64)  :: SurfArea          ! Surface area (m2)
LOGICAL    :: ShadowingSurf     ! True if surface is a shadowing surface
!REAL(r64), ALLOCATABLE, DIMENSION(:) :: WithShdgIsoSky     ! Diffuse solar irradiance from isotropic
!                                                          ! sky on surface, with shading
!REAL(r64), ALLOCATABLE, DIMENSION(:) :: WoShdgIsoSky       ! Diffuse solar from isotropic
!                                                           ! sky on surface, without shading
!REAL(r64), ALLOCATABLE, DIMENSION(:) :: WithShdgHoriz      ! Diffuse solar irradiance from horizon portion of
!                                                           ! sky on surface, with shading
!REAL(r64), ALLOCATABLE, DIMENSION(:) :: WoShdgHoriz        ! Diffuse solar irradiance from horizon portion of
!                                                           ! sky on surface, without shading
!INTEGER iHour,iTS

          ! FLOW:

! Initialize Surfaces Arrays
  SAREA = 0.0d0
  ALLOCATE (WithShdgIsoSky(TotSurfaces))
  WithShdgIsoSky=0.0d0
  ALLOCATE (WoShdgIsoSky(TotSurfaces))
  WoShdgIsoSky=0.0d0
  ALLOCATE (WithShdgHoriz(TotSurfaces))
  WithShdgHoriz=0.0d0
  ALLOCATE (WoShdgHoriz(TotSurfaces))
  WoShdgHoriz=0.0d0
  ALLOCATE (DifShdgRatioIsoSky(TotSurfaces))
  ALLOCATE (DifShdgRatioHoriz(TotSurfaces))
  ! initialized as no shading
  DifShdgRatioIsoSky = 1.0d0
  DifShdgRatioHoriz = 1.0d0
  IF (DetailedSkyDiffuseAlgorithm .and. ShadingTransmittanceVaries .and.  &
      SolarDistribution /= MinimalShadowing) THEN
    ALLOCATE (curDifShdgRatioIsoSky(TotSurfaces))
    curDifShdgRatioIsoSky = 1.0d0
  ENDIF

  ! only for detailed.
  IF (DetailedSkyDiffuseAlgorithm .and. ShadingTransmittanceVaries .and.  &
      SolarDistribution /= MinimalShadowing) THEN
    ALLOCATE (DifShdgRatioIsoSkyHRTS(TotSurfaces,24,NumOfTimeStepInHour))
    DifShdgRatioIsoSkyHRTS=1.0d0
    ALLOCATE (DifShdgRatioHorizHRTS(TotSurfaces,24,NumOfTimeStepInHour))
    DifShdgRatioHorizHRTS=1.0d0
  ENDIF

  do surfnum=1,totsurfaces
  IF (.not. Surface(surfnum)%ExtSolar) cycle

  ! CurrentModuleObject='Surfaces'
  IF (DetailedSkyDiffuseAlgorithm .and. ShadingTransmittanceVaries .and.  &
      SolarDistribution /= MinimalShadowing) THEN
    CALL SetupOutputVariable('Debug Surface Solar Shading Model DifShdgRatioIsoSky []',curDifShdgRatioIsoSky(Surfnum),  &
                                                        'Zone','Average',Surface(surfnum)%Name)
  ELSE
    CALL SetupOutputVariable('Debug Surface Solar Shading Model DifShdgRatioIsoSky []',DifShdgRatioIsoSky(Surfnum),  &
                                                        'Zone','Average',Surface(surfnum)%Name)
  ENDIF
  CALL SetupOutputVariable('Debug Surface Solar Shading Model DifShdgRatioHoriz []',DifShdgRatioHoriz(Surfnum),  &
                                                      'Zone','Average',Surface(surfnum)%Name)
  CALL SetupOutputVariable('Debug Surface Solar Shading Model WithShdgIsoSky []',WithShdgIsoSky(surfnum),  &
                                                      'Zone','Average',Surface(surfnum)%Name)
  CALL SetupOutputVariable('Debug Surface Solar Shading Model WoShdgIsoSky []',WoShdgIsoSky(surfnum),  &
                                                      'Zone','Average',Surface(surfnum)%Name)
  enddo

  DPhi = PiOvr2/NPhi              ! 15 deg for NPhi = 6
  DTheta = 2.d0*Pi/NTheta           ! 15 deg for NTheta = 24
  DThetaDPhi = DTheta*DPhi
  PhiMin = 0.5d0*DPhi               ! 7.5 deg for DPhi = 15 deg

  DO IPhi = 1,NPhi                ! Loop over patch altitude values
    Phi = PhiMin + (IPhi-1)*DPhi  ! 7.5,22.5,37.5,52.5,67.5,82.5 for NPhi = 6
    SUNCOS(3) = SIN(Phi)
    CosPhi = COS(Phi)

    DO ITheta = 1,NTheta          ! Loop over patch azimuth values
      Theta = (ITheta-1)*DTheta   ! 0,15,30,....,330,345 for NTheta = 24
      SUNCOS(1) = CosPhi*COS(Theta)
      SUNCOS(2) = CosPhi*SIN(Theta)

      DO SurfNum = 1,TotSurfaces  ! Cosine of angle of incidence on surface of solar
                                  ! radiation from patch
        ShadowingSurf = Surface(SurfNum)%ShadowingSurf

        IF (.NOT. ShadowingSurf .AND. .NOT. Surface(SurfNum)%HeatTransSurf) CYCLE

        CTHETA(SurfNum) = SUNCOS(1)*Surface(SurfNum)%OutNormVec(1)   &
                        + SUNCOS(2)*Surface(SurfNum)%OutNormVec(2)   &
                        + SUNCOS(3)*Surface(SurfNum)%OutNormVec(3)
      END DO

      CALL SHADOW(0,0)

      DO SurfNum = 1,TotSurfaces
        ShadowingSurf = Surface(SurfNum)%ShadowingSurf

        IF(.NOT.ShadowingSurf .AND. (.NOT.Surface(SurfNum)%HeatTransSurf .OR. &
           .NOT.Surface(SurfNum)%ExtSolar .OR. &
           (Surface(SurfNum)%ExtBoundCond /= ExternalEnvironment .AND. &
           Surface(SurfNum)%ExtBoundCond /= OtherSideCondModeledExt ))) CYCLE

        IF(CTHETA(SurfNum) < 0.0d0) CYCLE

        Fac1WoShdg = CosPhi * DThetaDPhi * CTHETA(SurfNum)
        SurfArea = Surface(SurfNum)%NetAreaShadowCalc
        IF (SurfArea > Eps) THEN
          FracIlluminated = SAREA(SurfNum)/SurfArea
        ELSE
          FracIlluminated = SAREA(SurfNum)/(SurfArea+Eps)
        ENDIF
        Fac1WithShdg = Fac1WoShdg * FracIlluminated
        WithShdgIsoSky(SurfNum) = WithShdgIsoSky(SurfNum) + Fac1WithShdg
        WoShdgIsoSky(SurfNum) = WoShdgIsoSky(SurfNum) + Fac1WoShdg

        ! Horizon region
        IF(IPhi == 1) THEN
          WithShdgHoriz(SurfNum) = WithShdgHoriz(SurfNum) + Fac1WithShdg
          WoShdgHoriz(SurfNum) = WoShdgHoriz(SurfNum) + Fac1WoShdg
        END IF
      END DO  ! End of surface loop
    END DO    ! End of Theta loop
  END DO      ! End of Phi loop

  DO SurfNum = 1,TotSurfaces
    ShadowingSurf = Surface(SurfNum)%ShadowingSurf

    IF(.NOT.ShadowingSurf .AND.                                                     &
      (.NOT.Surface(SurfNum)%HeatTransSurf .OR. .NOT.Surface(SurfNum)%ExtSolar .OR. &
      (Surface(SurfNum)%ExtBoundCond /= ExternalEnvironment .AND. &
         Surface(SurfNum)%ExtBoundCond /= OtherSideCondModeledExt) )) CYCLE

    IF (ABS(WoShdgIsoSky(SurfNum)) > Eps) THEN
      DifShdgRatioIsoSky(SurfNum) = (WithShdgIsoSky(SurfNum))/(WoShdgIsoSky(SurfNum))
    ELSE
      DifShdgRatioIsoSky(SurfNum) = (WithShdgIsoSky(SurfNum))/(WoShdgIsoSky(SurfNum)+Eps)
    ENDIF
    IF (ABS(WoShdgHoriz(SurfNum)) > Eps) THEN
      DifShdgRatioHoriz(SurfNum) =  (WithShdgHoriz(SurfNum))/(WoShdgHoriz(SurfNum))
    ELSE
      DifShdgRatioHoriz(SurfNum) =  (WithShdgHoriz(SurfNum))/(WoShdgHoriz(SurfNum)+Eps)
    ENDIF
  END DO

  ! Get IR view factors. An exterior surface can receive IR radiation from
  ! sky, ground or shadowing surfaces. Assume shadowing surfaces have same
  ! temperature as outside air (and therefore same temperature as ground),
  ! so that the view factor to these shadowing surfaces can be included in
  ! the ground view factor. Sky IR is assumed to be isotropic and shadowing
  ! surfaces are assumed to be opaque to IR so they totally "shade" IR from
  ! sky or ground.

  DO SurfNum = 1,TotSurfaces
    IF (.not. DetailedSkyDiffuseAlgorithm .or. .not. ShadingTransmittanceVaries .or.  &
        SolarDistribution == MinimalShadowing) THEN
      Surface(SurfNum)%ViewFactorSkyIR = Surface(SurfNum)%ViewFactorSkyIR * DifShdgRatioIsoSky(SurfNum)
    ELSE
      Surface(SurfNum)%ViewFactorSkyIR = Surface(SurfNum)%ViewFactorSkyIR * DifShdgRatioIsoSkyHRTS(SurfNum,1,1)
    ENDIF
    Surface(SurfNum)%ViewFactorGroundIR = 1.0d0 - Surface(SurfNum)%ViewFactorSkyIR
  END DO



!  DEALLOCATE (WithShdgIsoSky)
!  DEALLOCATE (WoShdgIsoSky)
!  DEALLOCATE (WithShdgHoriz)
!  DEALLOCATE (WoShdgHoriz)

  IF (DetailedSkyDiffuseAlgorithm .and. ShadingTransmittanceVaries .and.  &
      SolarDistribution /= MinimalShadowing) THEN
    DO SurfNum = 1,TotSurfaces
      DifShdgRatioIsoSkyHRTS(SurfNum,1:24,1:NumOfTimeStepInHour)=DifShdgRatioIsoSky(SurfNum)
      DifShdgRatioHorizHRTS(SurfNum,1:24,1:NumOfTimeStepInHour)=DifShdgRatioHoriz(SurfNum)
    ENDDO
  ENDIF

  RETURN

END SUBROUTINE SkyDifSolarShading

SUBROUTINE CalcWindowProfileAngles

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   April 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na
          ! PURPOSE OF THIS SUBROUTINE:
          ! Called by CalcPerSolarBeam for wholly or partially sunlit exterior windows
          ! Calculates horizontal and vertical beam solar profile angles

          ! REFERENCES: na
          ! USE STATEMENTS: na

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS: na
          ! INTERFACE BLOCK SPECIFICATIONS: na
          ! DERIVED TYPE DEFINITIONS: na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER            :: SurfNum           ! Surface number
REAL(r64)          :: ElevSun           ! Sun elevation; angle between sun and horizontal
REAL(r64)          :: ElevWin           ! Window elevation: angle between window outward normal and horizontal
REAL(r64)          :: AzimWin           ! Window azimuth (radians)
REAL(r64)          :: AzimSun           ! Sun azimuth (radians)
REAL(r64)          :: ProfileAngHor     ! Solar profile angle (radians) for horizontally oriented window elements
                                        ! such as the top and bottom of a frame.
                                        ! This is the incidence angle in a plane that is normal to the window
                                        ! and parallel to the Y-axis of the window (the axis along
                                        ! which the height of the window is measured).
REAL(r64)          :: ProfileAngVert    ! Solar profile angle (radians) for vertically oriented elements
                                        ! such as the sides of a frame.
                                        ! This is the incidence angle in a plane that is normal to the window
                                        ! and parallel to the X-axis of the window (the axis along
                                        ! which the width of the window is measured).
REAL(r64)          :: WinNorm(3)        ! Unit vector normal to window
REAL(r64)          :: WinNormCrossBase(3) ! Cross product of WinNorm and vector along window baseline
REAL(r64)          :: SunPrime(3)       ! Projection of sun vector onto plane (perpendicular to
                                        !  window plane) determined by WinNorm and vector along
                                        !  baseline of window
REAL(r64)          :: ThWin             ! Azimuth angle of WinNorm (radians)
real(r64) :: dot1,dot2,dot3

ElevSun = Piovr2 - ACOS(SOLCOS(3))
AzimSun = ATAN2(SOLCOS(1),SOLCOS(2))

DO SurfNum = 1,TotSurfaces

  IF(Surface(SurfNum)%Class /= SurfaceClass_Window .OR. &
    (Surface(SurfNum)%ExtBoundCond /= ExternalEnvironment .AND. &
    Surface(SurfNum)%ExtBoundCond /= OtherSideCondModeledExt)) CYCLE

  SurfaceWindow(SurfNum)%ProfileAngHor     = 0.0d0
  SurfaceWindow(SurfNum)%ProfileAngVert    = 0.0d0
  IF(CosIncAng(SurfNum,HourOfDay,TimeStep) <= 0.0d0) CYCLE

  ElevWin = Piovr2 - Surface(SurfNum)%Tilt * DegToRadians
  AzimWin = Surface(SurfNum)%Azimuth * DegToRadians

  ProfileAngHor = ATAN(SIN(ElevSun)/ABS(COS(ElevSun)*COS(AzimWin-AzimSun))) - ElevWin

!CR9280 - were having negative profile angles on west sides.  commenting out previous code (original code) for
! vertical windows
!  IF(ABS(ElevWin) < 0.1d0) THEN  ! Near-vertical window
!    ProfileAngVert = ABS(AzimWin-AzimSun)
!  ELSE
    WinNorm=Surface(SurfNum)%OutNormVec
    ThWin = AzimWin - PiOvr2
    WinNormCrossBase(1) = -SIN(ElevWin)*COS(ThWin)
    WinNormCrossBase(2) =  SIN(ElevWin)*SIN(ThWin)
    WinNormCrossBase(3) =  COS(ElevWin)
    SunPrime = SOLCOS - WinNormCrossBase*DOT_PRODUCT(SOLCOS,WinNormCrossBase)
    dot1=DOT_PRODUCT(WinNorm,SunPrime)
    dot2=SQRT(DOT_PRODUCT(SunPrime,SunPrime))
    dot3=dot1/dot2
    if (dot3 > 1.0d0) then
      dot3=1.0d0
    elseif (dot3 < -1.0d0) then
      dot3=-1.0d0
    endif
!    ProfileAngVert = ABS(ACOS(DOT_PRODUCT(WinNorm,SunPrime)/SQRT(DOT_PRODUCT(SunPrime,SunPrime))))
    ProfileAngVert = ABS(ACOS(dot3))
!  END IF
  ! Constrain to 0 to pi
  IF(ProfileAngVert > Pi) ProfileAngVert = 2.d0*Pi - ProfileAngVert

  SurfaceWindow(SurfNum)%ProfileAngHor     = ProfileAngHor/DegToRadians
  SurfaceWindow(SurfNum)%ProfileAngVert    = ProfileAngVert/DegToRadians
  SurfaceWindow(SurfNum)%TanProfileAngHor  = ABS(TAN(ProfileAngHor))
  SurfaceWindow(SurfNum)%TanProfileAngVert = ABS(TAN(ProfileAngVert))

END DO

RETURN
END SUBROUTINE CalcWindowProfileAngles

SUBROUTINE CalcFrameDividerShadow(SurfNum,FrDivNum,HourNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   June 2000
          !       MODIFIED       Aug 2000, FW: add effective shadowing by inside
          !                      projections
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Called by CalcPerSolarBeam for wholly or partially sunlit exterior windows
          ! with a frame and/or divider. Using beam solar profile angles,
          ! calculates fraction of glass shaded by exterior frame and divider projections,
          ! The frame and divider profiles are assumed to be rectangular.
          !
          ! A similar shadowing approach is used to calculate the fraction of glass area
          ! that produces beam solar illumination on interior frame and divider projections.
          ! This fraction is used in CalcWinFrameAndDividerTemps to determine the
          ! beam solar absorbed by inside projections. Beam solar reflected by inside projections
          ! is assumed to stay in the zone (as beam solar) although in actuality roughly
          ! half of this is reflected back onto the glass and the half that is reflected
          ! into the zone is diffuse.
          !
          ! For multipane glazing the effect of solar absorbed by the exposed portion of
          ! frame or divider between the panes is not calculated. Beam solar incident on
          ! these portions is assumed to be transmitted into the zone unchanged.
          !
          ! The shadowing of diffuse solar radiation by projections is not considered.
          !

          ! REFERENCES: na
          ! USE STATEMENTS: na

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER,INTENT(IN) :: SurfNum           ! Surface number
INTEGER,INTENT(IN) :: FrDivNum          ! Frame/divider number
INTEGER,INTENT(IN) :: HourNum           ! Hour number

          ! SUBROUTINE PARAMETER DEFINITIONS: na
          ! INTERFACE BLOCK SPECIFICATIONS: na
          ! DERIVED TYPE DEFINITIONS: na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
REAL(r64)   :: ElevSun           ! Sun elevation; angle between sun and horizontal
REAL(r64)   :: ElevWin           ! Window elevation: angle between window outward normal and horizontal
REAL(r64)   :: AzimWin           ! Window azimuth (radians)
REAL(r64)   :: AzimSun           ! Sun azimuth (radians)
REAL(r64)   :: ProfileAngHor     ! Solar profile angle (radians) for horizontally oriented projections
                                        ! such as the top and bottom of a frame or horizontal dividers.
                                        ! This is the incidence angle in a plane that is normal to the window
                                        ! and parallel to the Y-axis of the window (the axis along
                                        ! which the height of the window is measured).
REAL(r64)   :: ProfileAngVert    ! Solar profile angle (radians) for vertically oriented projections
                                        ! such as the top and bottom of a frame or horizontal dividers.
                                        ! This is the incidence angle in a plane that is normal to the window
                                        ! and parallel to the X-axis of the window (the axis along
                                        ! which the width of the window is measured).
REAL(r64)   :: TanProfileAngHor  ! Tangent of ProfileAngHor
REAL(r64)   :: TanProfileAngVert ! Tangent of ProfileAngVert
REAL(r64)          :: FrWidth           ! Frame width (m)
REAL(r64)          :: DivWidth          ! Divider width (m)
REAL(r64)          :: FrProjOut         ! Outside frame projection (m)
REAL(r64)          :: DivProjOut        ! Outside divider projection (m)
REAL(r64)          :: FrProjIn          ! Inside frame projection (m)
REAL(r64)          :: DivProjIn         ! Inside divider projection (m)
INTEGER            :: NHorDiv           ! Number of horizontal dividers
INTEGER            :: NVertDiv          ! Number of vertical dividers
REAL(r64)          :: GlArea            ! Glazed area (m2)
REAL(r64)   :: Arealite          ! Area of a single lite of glass (m2); glazed area, GlArea,
                                        ! if there is no divider (in which case there is only one lite).
REAL(r64)   :: ArealiteCol       ! Area of a vertical column of lites (m2)
REAL(r64)   :: ArealiteRow       ! Area of a horizontal row of lites (m2)
REAL(r64)          :: AshVDout          ! Shaded area from all vertical divider outside projections (m2)
REAL(r64)          :: AshVDin           ! Shaded area from all vertical divider inside projections (m2)
REAL(r64)          :: AshHDout          ! Shaded area from all horizontal divider outside projections (m2)
REAL(r64)          :: AshHDin           ! Shaded area from all horizontal divider inside projections (m2)
REAL(r64)          :: AshVFout          ! Shaded area from outside projection of vertical sides of frame (m2)
REAL(r64)          :: AshVFin           ! Shaded area from inside projection of vertical sides of frame (m2)
REAL(r64)          :: AshHFout          ! Shaded area from outside projection of horizontal sides
                                        !   (top) of frame (m2)
REAL(r64)          :: AshHFin           ! Shaded area from inside projection of horizontal sides
                                        !   (top) of frame (m2)
REAL(r64)          :: AshDDover         ! Divider/divider shadow overlap area (m2)
REAL(r64)          :: AshFFover         ! Frame/frame shadow overlap area (m2)
REAL(r64)          :: AshFVDover        ! Frame/vertical divider overlap area (m2)
REAL(r64)          :: AshFHDover        ! Frame/horizontal divider overlap area (m2)
REAL(r64)          :: AshFDtotOut       ! Total outside projection shadow area (m2)
REAL(r64)          :: AshFDtotIn        ! Total inside projection shadow area (m2)
REAL(r64)          :: FracShFDOut       ! Fraction of glazing shadowed by frame and divider
                                        !  outside projections
REAL(r64)          :: FracShFDin        ! Fraction of glazing that illuminates frame and divider
                                        !  inside projections with beam radiation

REAL(r64)          :: WinNorm(3)        ! Window outward normal unit vector
REAL(r64)   :: ThWin             ! Azimuth angle of WinNorm
REAL(r64)   :: SunPrime(3)       ! Projection of sun vector onto plane (perpendicular to
                                        !  window plane) determined by WinNorm and vector along
                                        !  baseline of window
REAL(r64)          :: WinNormCrossBase(3) ! Cross product of WinNorm and vector along window baseline

IF (FrameDivider(FrDivNum)%FrameProjectionOut==0.0d0   .AND.  &
    FrameDivider(FrDivNum)%FrameProjectionIn==0.0d0    .AND.   &
    FrameDivider(FrDivNum)%DividerProjectionOut==0.0d0 .AND.   &
    FrameDivider(FrDivNum)%DividerProjectionIn==0.0d0) RETURN

FrProjOut = FrameDivider(FrDivNum)%FrameProjectionOut
FrProjIn = FrameDivider(FrDivNum)%FrameProjectionIn
DivProjOut = FrameDivider(FrDivNum)%DividerProjectionOut
DivProjIn = FrameDivider(FrDivNum)%DividerProjectionIn

GlArea = Surface(SurfNum)%Area
ElevWin = Piovr2 - Surface(SurfNum)%Tilt * DegToRadians
ElevSun = Piovr2 - ACOS(SUNCOS(3))
AzimWin = Surface(SurfNum)%Azimuth * DegToRadians
AzimSun = ATAN2(SUNCOS(1),SUNCOS(2))

ProfileAngHor = ATAN(SIN(ElevSun)/ABS(COS(ElevSun)*COS(AzimWin-AzimSun))) - ElevWin
IF(ABS(ElevWin) < 0.1d0) THEN  ! Near-vertical window
  ProfileAngVert = ABS(AzimWin-AzimSun)
ELSE
  WinNorm=Surface(SurfNum)%OutNormVec
  ThWin = AzimWin - PiOvr2
  WinNormCrossBase(1) = -SIN(ElevWin)*COS(ThWin)
  WinNormCrossBase(2) =  SIN(ElevWin)*SIN(ThWin)
  WinNormCrossBase(3) =  COS(ElevWin)
  SunPrime = SUNCOS - WinNormCrossBase*DOT_PRODUCT(SUNCOS,WinNormCrossBase)
  ProfileAngVert = ABS(ACOS(DOT_PRODUCT(WinNorm,SunPrime)/SQRT(DOT_PRODUCT(SunPrime,SunPrime))))
END IF
! Constrain to 0 to pi
IF(ProfileAngVert > Pi) ProfileAngVert = 2*Pi - ProfileAngVert
TanProfileAngHor = ABS(TAN(ProfileAngHor))
TanProfileAngVert = ABS(TAN(ProfileAngVert))

NHorDiv = FrameDivider(FrDivNum)%HorDividers
NVertDiv = FrameDivider(FrDivNum)%VertDividers
FrWidth = FrameDivider(FrDivNum)%FrameWidth
DivWidth = FrameDivider(FrDivNum)%DividerWidth

Arealite = (Surface(SurfNum)%Height/(NHorDiv + 1.d0) - DivWidth/2.d0) * &
           (Surface(SurfNum)%Width/(NVertDiv + 1.d0) - DivWidth/2.d0)
IF(DivProjOut > 0.0d0 .OR. DivProjIn > 0.0d0) THEN
  ArealiteCol = (NHorDiv+1)*Arealite
  ArealiteRow = (NVertDiv+1)*Arealite
ELSE
  ArealiteCol = GlArea
  ArealiteRow = GlArea
END IF
AshVDout = 0.0d0
AshVDin = 0.0d0
AshHDout = 0.0d0
AshHDin = 0.0d0
AshVFout = 0.0d0
AshVFin = 0.0d0
AshHFout = 0.0d0
AshHFin = 0.0d0
AshDDover = 0.0d0
AshFFover = 0.0d0
AshFVDover = 0.0d0
AshFHDover = 0.0d0

IF(DivProjOut > 0.0d0 .OR. DivProjIn > 0.0d0) THEN

  ! Shaded area from all vertical dividers
  AshVDout = NVertDiv * &
    MIN((Surface(SurfNum)%Height-NHorDiv*DivWidth)*DivProjOut*TanProfileAngVert,ArealiteCol)
  AshVDin = NVertDiv * &
    MIN((Surface(SurfNum)%Height-NHorDiv*DivWidth)*DivProjIn*TanProfileAngVert,ArealiteCol)

  ! Shaded area from all horizontal dividers
  AshHDout = NHorDiv * &
    MIN((Surface(SurfNum)%Width-NVertDiv*DivWidth)*DivProjOut*TanProfileAngHor,ArealiteRow)
  AshHDin = NHorDiv * &
    MIN((Surface(SurfNum)%Width-NVertDiv*DivWidth)*DivProjIn*TanProfileAngHor,ArealiteRow)

  ! Horizontal divider/vertical divider shadow overlap
  AshDDover = MIN(DivProjOut*TanProfileAngHor*DivProjOut*TanProfileAngVert,Arealite)*NHorDiv*NVertDiv

END IF

IF(FrProjOut > 0.0d0 .OR. FrProjIn > 0.0d0) THEN

  ! Shaded area from sides of frame; to avoid complications from possible overlaps between
  ! shadow from side of frame and shadow from vertical divider the shaded area from side of
  ! frame is restricted to the area of one column of lites.
  AshVFOut = MIN((Surface(SurfNum)%Height-NHorDiv*DivWidth)*FrProjOut*TanProfileAngVert,ArealiteCol)
  AshVFin = MIN((Surface(SurfNum)%Height-NHorDiv*DivWidth)*FrProjIn*TanProfileAngVert,ArealiteCol)

  ! Shaded area from top or bottom of frame; to avoid complications from possible overlaps
  ! between shadow from top or bottom of frame and shadow from horizontal divider, the shaded
  ! area from the top or bottom of frame is restricted to the area of one row of lites.
  AshHFOut = MIN((Surface(SurfNum)%Width-NVertDiv*DivWidth)*FrProjOut*TanProfileAngHor,ArealiteRow)
  AshHFin = MIN((Surface(SurfNum)%Width-NVertDiv*DivWidth)*FrProjIn*TanProfileAngHor,ArealiteRow)

  ! Top/bottom of frame/side of frame shadow overlap
  AshFFover = MIN(FrProjOut*TanProfileAngHor*FrProjOut*TanProfileAngVert,Arealite)

  IF(DivProjOut > 0.0d0) THEN
    ! Frame/vertical divider shadow overlap
    AshFVDover = MIN(FrProjOut*DivProjOut*TanProfileAngHor*TanProfileAngVert,Arealite)*NVertDiv

    ! Frame/horizontal divider shadow overlap
    AshFHDover = MIN(FrProjOut*DivProjOut*TanProfileAngHor*TanProfileAngVert,Arealite)*NHorDiv

  END IF

END IF

AshFDtotOut = AshVDout + AshHDout + AshVFout + AshHFout - &
              (AshDDover + AshFFover + AshFVDover + AshFHDover)
AshFDtotIn = (AshVDin + AshHDin)*FrameDivider(FrDivNum)%DividerSolAbsorp + &
             (AshVFin + AshHFin)*FrameDivider(FrDivNum)%FrameSolAbsorp

! Divide by the glazed area of the window
FracShFDout = AshFDtotOut/GlArea
FracShFDin = AshFDtotIn/GlArea
SurfaceWindow(SurfNum)%OutProjSLFracMult(HourNum) = 1.d0-FracShFDout
SurfaceWindow(SurfNum)%InOutProjSLFracMult(HourNum) = 1.d0-(FracShFDin + FracShFDout)

RETURN
END SUBROUTINE CalcFrameDividerShadow

SUBROUTINE CalcBeamSolarOnWinRevealSurface

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         F. Winkelmann
          !       DATE WRITTEN   April 2002
          !       MODIFIED:na
          !       RE-ENGINEERED:na

          ! PURPOSE OF THIS SUBROUTINE
          ! Called by InitHeatGains when the sun is up.
          ! Calculates beam solar radiation absorbed and reflected by top, bottom,
          ! right and left sides of outside and inside window reveal surfaces.
          ! In doing this calculation, the shadowing on a reveal surface by other reveal surfaces
          ! is determined using the orientation of the reveal surfaces and the sun position.
          ! It is assumed that:
          !
          ! (1) The window is an exterior window and is rectangular.
          ! (2) The reveal surfaces are perpendicular to the window plane.
          ! (3) If an exterior shade or blind is in place, there is no beam solar on
          !     on exterior or interior reveal surfaces.
          ! (3) If an interior shade or blind is in place, there is no beam solar on
          !     interior reveal surfaces.
          ! (4) The effect of window divider, if present, is ignored, including shadowing
          !     of divider on inside reveal surfaces.

          ! In the variable names, the "subscript" 1 = outside reveal, 2 = inside reveal
          !
          ! The outside reveal surfaces (top, bottom, left, right) are assumed to have the same depth
          ! (given by Surface%Reveal and determined from vertices of window and vertices of parent
          ! wall) and the same solar absorptance. The inside reveal surfaces are divided into
          ! two categories: (1) the bottom reveal surface, called here the "inside sill;" and
          ! the other reveal surfaces (left, right and top). The left, right and top inside reveal
          ! surfaces are assumed to have the same depth and solar absorptance.
          ! The depth of the outside reveal is measured from the outside surface of the glazing;
          ! The depth of the inside sill and the other reveal surfaces is measured from the inside
          ! surface of the glazing. The inside sill is
          ! allowed to have depth and solar absorptance values that are different from the corresponding
          ! values for the other inside reveal surfaces. The inside sill depth is required to be
          ! greater than or equal to the depth of the other inside reveal surfaces. If the inside sill
          ! depth is greater than zero the depth of the other inside reveal surfaces is required to
          ! to be greater than zero.
          !
          ! The reflection of beam solar radiation from all reveal surfaces is assumed to be isotropic
          ! diffuse; there is no specular component. Half of the beam solar reflected from outside
          ! reveal surfaces is assumed to go towards the window; the other half is assumed to go back
          ! to the exterior environment (i.e., reflection of this outward-going component from
          ! other outside reveal surfaces is not considered). The half that goes towards the window
          ! is added to the other radiation incident on the window.
          ! Correspondingly, half of the beam solar reflected from inside reveal surfaces is assumed
          ! to go towards the window, with the other half going into the zone (this half, and the portion
          ! going towards the window that is reflected) is added in CalcInteriorSolarDistribution
          ! to the variable BTOTzone, which is the total beam solar entering the zone as beam or diffuse.
          ! The portion going towards the window that is not reflected is absorbed in the glazing or
          ! transmitted back out into the exterior environment.
          !
          ! The beam solar that is absorbed by outside reveal surfaces is added to the solar absorbed
          ! by the outside surface of the window's parent wall; similarly, the beam solar absorbed
          ! by the inside reveal surfaces is added to the solar absorbed by the inside surface of the
          ! parent wall (and is subtracted from BTOTzone).
          !
          ! The net effect of beam solar reflected from outside reveal surfaces is to INCREASE the
          ! the heat gain to the zone, whereas the effect of beam solar reflected from interior reveal
          ! surfaces is to DECREASE the heat gain to the zone since part of this reflected solar is
          ! transmitted back out the window.
          !
          ! If the window has a frame, the absorption of reflected beam solar by the inside and outside
          ! surfaces of the frame is considered. The shadowing of the frame onto interior reveal
          ! surfaces is also considered.

          ! The total glazing thickness is taken to be the sum of the thickness of the glass layers
          ! and between-glass gas layers. If the window has an exterior, movable, storm window glass layer
          ! the presence of this layer and its adjacent air gap is considered in calculating the glazing
          ! properties (solar transmittance, etc.). But the storm window glass is assumed to be close
          ! enough to the rest of the glazing that its effect on total glazing thickness and outside
          ! reveal depth can be ignored.

          ! METHODOLOGY EMPLOYED
          ! na

          ! REFERENCES
          ! na

          ! USE STATEMENTS
USE General, ONLY: POLYF,InterpSw

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS

INTEGER        :: ConstrNum             ! Construction number
INTEGER        :: ConstrNumSh           ! Shaded construction number
REAL(r64)      :: CosBetaBottom         ! Cosine of beam solar angle of incidence on bottom reveal
REAL(r64)      :: CosBetaLeft           ! Cosine of beam solar angle of incidence on left reveal
REAL(r64)      :: CosBeta               ! ABS of CosBetaBottom or CosBetaLeft
REAL(r64)      :: d1                    ! Depth of outside reveal + half of glazing thickness (m)
REAL(r64)      :: d2                    ! Depth of inside sill or of inside reveal plus half of glazing thickness (m)
REAL(r64)      :: d2prime               ! Depth of shadow cast on a reveal surface by opposite reveal (m)
REAL(r64)      :: d2prime2              ! Depth of shadow cast by frame onto inside reveal (m)
REAL(r64)      :: d12                   ! d12 = d1 + d2 - d2prime (m)
REAL(r64)      :: TanAlpha              ! Tangent of horizontal or vertical profile angle
REAL(r64)      :: TanGamma              ! Tangent of vertical or horizontal profile angle
REAL(r64)      :: H,W                   ! Window height, width (m)
REAL(r64)      :: L                     ! Window height or width (m)
REAL(r64)      :: A1sh                  ! Shadowed area of outside horizontal or vertical reveal (m2)
REAL(r64)      :: A2sh                  ! Shadowed area of inside horizontal or vertical reveal (m2)
REAL(r64)      :: A1ill                 ! Illuminated area of outside horizontal or vertical reveal (m2)
REAL(r64)      :: A2ill                 ! Illuminated area of inside horizontal or vertical reveal (m2)
REAL(r64)      :: SolTransGlass         ! Beam solar transmittance of glazing
REAL(r64)      :: SolTransGlassSh       ! For switchable glazing, beam solar trans in switched state
REAL(r64)      :: DiffReflGlass         ! Diffuse back reflectance of glazing
REAL(r64)      :: DiffReflGlassSh       ! For switchable glazing, diffuse back refl in switched state
INTEGER        :: HorVertReveal         ! Index: 1 = horizontal reveal, 2 = vertical reveal
REAL(r64)      :: OutsReveal            ! Depth of outside reveal (from outside glazing plane to outside wall plane) (m)
REAL(r64)      :: InsReveal             ! Depth of inside reveal (from inside glazing plane to inside wall plane (m)
REAL(r64)      :: InsSillDepth          ! Depth of inside sill, measured from innermost face of glazing (m)
REAL(r64)      :: GlazingThickness      ! Thickness of glazing, measured from innermost face to outermost face (m)
REAL(r64)      :: InsideRevealSolAbs    ! Solar absorptance of inside reveal or inside sill
REAL(r64)      :: BmSolRefldOutsReveal  ! Multiplied by beam solar gives beam solar reflected by horiz or vertical
                                        !  outside reveal surface (m2)
REAL(r64)      :: BmSolRefldInsReveal   ! Multiplied by beam solar gives beam solar reflected by horiz or vertical
                                        !  inside reveal surface (m2)
INTEGER        :: SurfNum               ! Surface number
INTEGER        :: ShadeFlag             ! Shading flag
INTEGER        :: FrameDivNum           ! Frame/Divider number
REAL(r64)      :: FrameWidth            ! Frame width (m)
REAL(r64)      :: P1,P2                 ! Frame outside/inside projection plus half of glazing thickness (m)
REAL(r64)      :: f1,f2                 ! f1=d1-P1, f2=d2-P2 (m)
REAL(r64)      :: L1,L2                 ! Average distance of outside/inside illuminated area to frame;
                                        ! used in calculating view factor to frame (m)
REAL(r64)      :: FracToGlassOuts       ! View factor from outside horizontal or vertical reveal to glass
REAL(r64)      :: FracToGlassIns        ! View factor from inside horizontal or vertical reveal to glass
REAL(r64)      :: TanProfileAngVert     ! Tangent of vertical profile angle (the profile angle appropriate for
                                        ! vertical reveal surfaces.
REAL(r64)      :: TanProfileAngHor      ! Tangent of horizontal profile angle (the profile angle appropriate for
                                        ! horizontal reveal surfaces.

REAL(r64)      :: tmp_SunlitFracWithoutReveal      ! Temporary variable

DO SurfNum = 1,TotSurfaces

  ! Added TH for initialization. CR 7596 inside reveal causing high cooling loads
      ! for outside reveals
      SurfaceWindow(SurfNum)%BmSolAbsdOutsReveal = 0.0d0
      SurfaceWindow(SurfNum)%BmSolRefldOutsRevealReport = 0.0d0
      SurfaceWindow(SurfNum)%BmSolRefldOutsRevealRepEnergy = 0.0d0
      SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing = 0.0d0
      SurfaceWindow(SurfNum)%OutsRevealDiffOntoFrame = 0.0d0
      ! for inside reveals
      SurfaceWindow(SurfNum)%BmSolAbsdInsReveal = 0.0d0
      SurfaceWindow(SurfNum)%BmSolAbsdInsRevealReport = 0.0d0
      SurfaceWindow(SurfNum)%BmSolRefldInsReveal = 0.0d0
      SurfaceWindow(SurfNum)%BmSolRefldInsRevealReport = 0.0d0
      SurfaceWindow(SurfNum)%BmSolRefldInsRevealRepEnergy = 0.0d0
      SurfaceWindow(SurfNum)%InsRevealDiffOntoGlazing = 0.0d0
      SurfaceWindow(SurfNum)%InsRevealDiffOntoGlazingReport = 0.0d0
      SurfaceWindow(SurfNum)%InsRevealDiffOntoFrame = 0.0d0
      SurfaceWindow(SurfNum)%InsRevealDiffOntoFrameReport = 0.0d0
      SurfaceWindow(SurfNum)%InsRevealDiffIntoZone = 0.0d0
      SurfaceWindow(SurfNum)%InsRevealDiffIntoZoneReport = 0.0d0


  IF(Surface(SurfNum)%Class /= SurfaceClass_Window .OR. &
    (Surface(SurfNum)%ExtBoundCond /= ExternalEnvironment .AND. &
    Surface(SurfNum)%ExtBoundCond /= OtherSideCondModeledExt)) CYCLE
  IF(Surface(SurfNum)%Reveal == 0.0d0 .AND. SurfaceWindow(SurfNum)%InsideReveal == 0.0d0 .AND. &
       SurfaceWindow(SurfNum)%InsideSillDepth == 0.0d0) CYCLE
  IF(Surface(SurfNum)%Sides /= 4) CYCLE
  IF(SurfaceWindow(SurfNum)%InsideSillDepth < SurfaceWindow(SurfNum)%InsideReveal) CYCLE

  ShadeFlag = SurfaceWindow(SurfNum)%ShadingFlag
  IF(ShadeFlag == ExtShadeOn .OR. ShadeFlag == ExtBlindOn) CYCLE

  IF(CosIncAng(SurfNum,HourOfDay,TimeStep) <= 0.0d0) CYCLE

  tmp_SunlitFracWithoutReveal = SunlitFracWithoutReveal(SurfNum,HourOfDay,TimeStep)

  ! Calculate cosine of angle of incidence of beam solar on reveal surfaces,
  ! assumed to be perpendicular to window plane

  CosBetaBottom = -SOLCOS(1) * Surface(SurfNum)%SinAzim * Surface(SurfNum)%CosTilt - &
                   SOLCOS(2) * Surface(SurfNum)%CosAzim * Surface(SurfNum)%CosTilt + &
                   SOLCOS(3) * Surface(SurfNum)%SinTilt

  CosBetaLeft =   -SOLCOS(1) * Surface(SurfNum)%CosAzim - &
                   SOLCOS(2) * Surface(SurfNum)%SinAzim

  !Note: CosBetaTop = -CosBetaBottom, CosBetaRight = -CosBetaLeft

  OutsReveal = Surface(SurfNum)%Reveal
  InsReveal  = SurfaceWindow(SurfNum)%InsideReveal
  InsideRevealSolAbs=0.0d0
  GlazingThickness = SurfaceWindow(SurfNum)%TotGlazingThickness
  H = Surface(SurfNum)%Height
  W = Surface(SurfNum)%Width
  d1 = OutsReveal + 0.5d0*GlazingThickness
  ConstrNum = Surface(SurfNum)%Construction
  ConstrNumSh = Surface(SurfNum)%ShadedConstruction
  IF(SurfaceWindow(SurfNum)%StormWinFlag==1) THEN
    ConstrNum = Surface(SurfNum)%StormWinConstruction
    ConstrNumSh = Surface(SurfNum)%StormWinShadedConstruction
  END IF
  SolTransGlass = POLYF(CosIncAng(SurfNum,HourOfDay,TimeStep),Construct(ConstrNum)%TransSolBeamCoef(1:6))
  TanProfileAngVert = SurfaceWindow(SurfNum)%TanProfileAngVert
  TanProfileAngHor  = SurfaceWindow(SurfNum)%TanProfileAngHor
  FrameDivNum = Surface(SurfNum)%FrameDivider
  FrameWidth = 0.0d0
  IF(FrameDivNum /= 0) THEN
    FrameWidth = FrameDivider(FrameDivNum)%FrameWidth
    IF(FrameWidth > 0.0d0) THEN
      P1  = FrameDivider(FrameDivNum)%FrameProjectionOut + 0.5d0*GlazingThickness
      P2  = FrameDivider(FrameDivNum)%FrameProjectionIn + 0.5d0*GlazingThickness
      IF(OutsReveal+0.5d0*GlazingThickness <= P1) d1 = P1 + 0.001d0
    END IF
  END IF
                                      ! Loop over vertical and horizontal reveal surfaces
  DO HorVertReveal = 1,2

    FracToGlassOuts = 0.5d0
    FracToGlassIns  = 0.5d0
    BmSolRefldOutsReveal = 0.0d0
    BmSolRefldInsReveal = 0.0d0
    A1ill = 0.0d0
    A2ill = 0.0d0

    ! Added TH. 5/27/2009
    A1sh = 0.0d0
    A2sh = 0.0d0

    IF(HorVertReveal == 1) THEN       ! Vertical reveal
      TanAlpha = TanProfileAngHor
      TanGamma = TanProfileAngVert
      CosBeta = ABS(CosBetaLeft)
      L = Surface(SurfNum)%Height
      d2 = InsReveal + 0.5d0*GlazingThickness
      d2prime = d1 + d2 - W/TanGamma
      InsideRevealSolAbs = SurfaceWindow(SurfNum)%InsideRevealSolAbs
    ELSE                              ! Horizontal reveal
      InsSillDepth = SurfaceWindow(SurfNum)%InsideSillDepth
      TanAlpha = TanProfileAngVert
      TanGamma = TanProfileAngHor
      CosBeta = ABS(CosBetaBottom)
      L = Surface(SurfNum)%Width
      IF(CosBetaBottom > 0.0d0) THEN  ! Bottom reveal surfaces may be illuminated
        d2 = InsSillDepth + 0.5d0*GlazingThickness
        InsideRevealSolAbs = SurfaceWindow(SurfNum)%InsideSillSolAbs
      ELSE                          ! Top reveal surfaces may be illuminated
        d2 = InsReveal + 0.5d0*GlazingThickness
        InsideRevealSolAbs = SurfaceWindow(SurfNum)%InsideRevealSolAbs
      END IF
      d2prime = d1 + d2 - H/TanGamma
    END IF
    IF(d2prime < 0.0d0) d2prime = 0.0d0  ! No shadow from opposing reveal
    d12 = d1 + d2 - d2prime

    IF(FrameWidth <= 0.001d0) THEN
      ! Window without frame

      ! Find inside and outside shadowed area of vertical or horizontal reveal surfaces
      ! that can be illuminated by beam solar; shadowing is by other reveal surfaces.

      IF(d2prime <= d2) THEN
        IF(d12*TanAlpha <= L) THEN
          A1sh = 0.5d0*TanAlpha*d1**2
          A2sh = d2prime*L + 0.5d0*TanAlpha*d12**2 - A1sh
        ELSE  ! d12*TanAlpha > L
          IF(d1*TanAlpha <= L) THEN
            A1sh = 0.5d0*TanAlpha*d1**2
            A2sh = d2*L - 0.5d0*TanAlpha*(L/TanAlpha - d1)**2
          ELSE  ! d1*TanAlpha > L
            A1sh = d1*L - (0.5d0/TanAlpha)*L**2
            A2sh = d2*L
          END IF
        END IF
      ELSE  ! d2prime > d2
        A2sh = d2*L
        IF(d2prime < d1+d2) THEN
          IF(d12*TanAlpha <= L) THEN
            A1sh = L*(d2prime-d2) + 0.5d0*TanAlpha*d12**2
          ELSE  ! d12*TanAlpha > L
            A1sh = d1*L - 0.5d0*L**2/TanAlpha
          END IF
        ELSE  ! d2prime >= d1+d2
          A1sh = d1*L
        END IF
      END IF

      ! Added TH. 5/27/2009
      IF (A1sh < 0.0d0) A1sh = 0.0d0
      IF (A2sh < 0.0d0) A2sh = 0.0d0

      IF(OutsReveal >= 0.001d0) A1ill = d1*L - A1sh  ! A1ill = 0.0 if OutsReveal < 0.001
      IF(InsReveal >= 0.001d0)  A2ill = d2*L - A2sh  ! A2ill = 0.0 if InsReveal < 0.001

    ELSE                             ! Window with frame; take into account shadowing
                                     ! of inside reveal surfaces by frame
      f1 = d1-P1
      f2 = d2-P2
      d2prime2 = FrameWidth/TanGamma
      IF(HorVertReveal == 1) THEN  ! Vertical reveal
          IF(InsReveal+0.5d0*GlazingThickness <= P2) d2 = P2 + 0.001d0
      ELSE                ! Horizontal
        IF(CosBetaBottom > 0.0d0) THEN  ! Bottom reveal surfaces may be illuminated
          IF(InsSillDepth+0.5d0*GlazingThickness <= P2) d2 = P2 + 0.001d0
        ELSE                          ! Top reveal surfaces may be illuminated
          IF(InsReveal+0.5d0*GlazingThickness <= P2) d2 = P2 + 0.001d0
        END IF
      END IF

      IF(d2prime <= f2) THEN   ! Shadow from opposing reveal does not go beyond inside surface of frame

        IF(d12*TanAlpha <= L) THEN
          A1sh = 0.5d0*TanAlpha*f1**2
          L1   = f1*(f1*TanAlpha/(6.d0*L)+0.5d0)
          IF(d2-(d2prime+d2prime2+P2) >= 0.0d0) THEN
            A2sh = (d2prime+d2prime2)*L + 0.5d0*TanAlpha*((d1+d2-d2prime)**2-(d1+p2+d2prime2)**2)
            L2   = d2prime2 + 0.5d0*(d2-(d2prime+d2prime2+P2))
          ELSE  ! d2-(d2prime+d2prime2+P2) < 0.  ! Inside reveal is fully shadowed by frame and/or opposing reveal
            A2sh = f2*L
            L2   = f2
          END IF
        ELSE  ! d12*TanAlpha >= L
          IF((d1+P2)*TanAlpha <= L) THEN
            A1sh = 0.5d0*TanAlpha*f1**2
            L1 = f1*((f1*TanAlpha)/(6.d0*L) + 0.5d0)
            IF((d1+P2+d2prime2)*TanAlpha >= L) THEN
              A2sh = f2*L
              L2     = f2
            ELSE  ! (d1+P2+d2prime2)*TanAlpha < L
              A2sh = f2*L - 0.5d0*(L-(d1+P2)*TanAlpha)**2/TanAlpha + d2prime2*(L-(d1+P2+d2prime2/2.d0)*TanAlpha)
              L2 = d2prime2 + (L/TanAlpha - (d1+P2+d2prime2))/3.d0
            END IF
          ELSE  ! (d1+P2)*TanAlpha > L
            L2 = f2
            A2sh = f2*L
            IF(f1*TanAlpha <= L) THEN
              A1sh = 0.5d0*TanAlpha*f1**2
              L1 = f1*((f1*TanAlpha)/(6.d0*L) + 0.5d0)
            ELSE  ! f1*TanAlpha > L
              A1sh = f1*L - 0.5d0*L**2/TanAlpha
              L1 = f1-(L/TanAlpha)/3.d0
            END IF
          END IF
        END IF

      ELSE  ! d2prime > f2   ! Shadow from opposing reveal goes beyond inside of frame

        A2sh = f2*L
        L2 = f2
        IF(d2prime >= d1+d2) THEN
          A1sh = 0.0d0
          L1   = f1
        ELSE  ! d2prime < d1+d2
          IF(d2prime <= d2+P1) THEN
            IF(f1*TanAlpha <= L) THEN
              A1sh = 0.5d0*TanAlpha*f1**2
              L1 = f1*((f1*TanAlpha)/(6.d0*L) + 0.5d0)
            ELSE  ! f1*TanAlpha > L
              A1sh = f1*L - 0.5d0*L**2/TanAlpha
              L1   = f1 - (L/TanAlpha)/3.d0
            END IF
          ELSE  ! d2prime > d2+P1
            IF(d12*TanAlpha <= L) THEN
              A1sh = L*(d2prime-(d2+P1)) + 0.5d0*TanAlpha*d12**2
              L1   = (L*(f1-d12/2.d0)-d12*TanAlpha*(f1/2-d12/3.d0))/(L-d12*TanAlpha/2.d0)
            ELSE  ! d12*TanAlpha > L
              A1sh = f1*L - 0.5d0*L**2/TanAlpha
              L1   = f1 - (L/TanAlpha)/3.d0
            END IF
          END IF
        END IF

      END IF

      ! Added TH. 5/27/2009
      IF (A1sh < 0.0d0) A1sh = 0.0d0
      IF (A2sh < 0.0d0) A2sh = 0.0d0

      IF(OutsReveal >= P1+0.5d0*GlazingThickness+0.001d0) A1ill = L*f1 - A1sh
      IF(InsReveal >= P2+0.5d0*GlazingThickness+0.001d0)  A2ill = L*f2 - A2sh
      IF(L1 == 0.0d0) THEN
        FracToGlassOuts = 0.0d0
      ELSE
        FracToGlassOuts = 0.5d0*(1.0d0 - ATAN(FrameWidth/L1)/PiOvr2)
      END IF
      IF(L2 == 0.0d0) THEN
        FracToGlassIns = 0.0d0
      ELSE
        FracToGlassIns  = 0.5d0*(1.0d0 - ATAN(FrameWidth/L2)/PiOvr2)
      END IF
    END IF  ! End of check if window has frame

    ! Added TH. 5/27/2009
    IF (A1ill < 0.0d0) A1ill = 0.0d0
    IF (A2ill < 0.0d0) A2ill = 0.0d0

    ! Quantities related to outside reveal
    IF(A1ill > 1.0d-6) THEN

      SurfaceWindow(SurfNum)%BmSolAbsdOutsReveal = SurfaceWindow(SurfNum)%BmSolAbsdOutsReveal + &
        A1ill * SurfaceWindow(SurfNum)%OutsideRevealSolAbs * CosBeta * tmp_SunlitFracWithoutReveal

      BmSolRefldOutsReveal = A1ill * (1.d0- SurfaceWindow(SurfNum)%OutsideRevealSolAbs) * CosBeta * &
        tmp_SunlitFracWithoutReveal

      SurfaceWindow(SurfNum)%BmSolRefldOutsRevealReport = SurfaceWindow(SurfNum)%BmSolRefldOutsRevealReport &
                           + BeamSolarRad * BmSolRefldOutsReveal
      SurfaceWindow(SurfNum)%BmSolRefldOutsRevealRepEnergy = SurfaceWindow(SurfNum)%BmSolRefldOutsRevealReport &
                            * TimeStepZone * SecInHour

      ! Reflected solar from outside horizontal and vertical reveal incident on glazing
      SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing = SurfaceWindow(SurfNum)%OutsRevealDiffOntoGlazing + &
        FracToGlassOuts * BmSolRefldOutsReveal / Surface(SurfNum)%Area

      IF(FrameWidth > 0.0d0) THEN
      ! Reflected solar from outside horizontal and vertical reveal incident on frame
        SurfaceWindow(SurfNum)%OutsRevealDiffOntoFrame =  SurfaceWindow(SurfNum)%OutsRevealDiffOntoFrame + &
          (0.5d0-FracToGlassOuts) * BmSolRefldOutsReveal / SurfaceWindow(SurfNum)%FrameArea
      END IF

    END IF  ! End of check if A1ill > 0.0d0

   ! Quantities related to inside reveal; inside reveal reflection/absorption is assumed
   ! to occur only if an interior shade or blind is not in place.

    IF(ShadeFlag <= 0 .OR. ShadeFlag == SwitchableGlazing) THEN

      IF(A2ill > 1.0d-6) THEN

        DiffReflGlass = Construct(ConstrNum)%ReflectSolDiffBack
        IF(ShadeFlag == SwitchableGlazing) THEN
          SolTransGlassSh = POLYF(CosIncAng(SurfNum,HourOfDay,TimeStep),Construct(ConstrNumSh)%TransSolBeamCoef(1:6))
          SolTransGlass   = InterpSW(SurfaceWindow(SurfNum)%SwitchingFactor,SolTransGlass,SolTransGlassSh)
          DiffReflGlassSh = Construct(ConstrNumSh)%ReflectSolDiffBack
          DiffReflGlass   = InterpSW(SurfaceWindow(SurfNum)%SwitchingFactor,DiffReflGlass,DiffReflGlassSh)
        END IF

        ! Calc beam solar sbsorbed (m2)
        SurfaceWindow(SurfNum)%BmSolAbsdInsReveal = SurfaceWindow(SurfNum)%BmSolAbsdInsReveal + &
          A2ill * SolTransGlass * InsideRevealSolAbs * CosBeta * tmp_SunlitFracWithoutReveal

        ! Added TH 5/26/2009 for reporting purpose - Beam solar absorbed by the inside reveal (W)
        SurfaceWindow(SurfNum)%BmSolAbsdInsRevealReport = SurfaceWindow(SurfNum)%BmSolAbsdInsRevealReport + &
          BeamSolarRad * A2ill * SolTransGlass * InsideRevealSolAbs * CosBeta * tmp_SunlitFracWithoutReveal

        ! in m2 = Area * solar transmitted fraction * inside reveal reflection fraction
        BmSolRefldInsReveal = A2ill * SolTransGlass * (1.0d0 - InsideRevealSolAbs) * CosBeta *  &
          tmp_SunlitFracWithoutReveal

        SurfaceWindow(SurfNum)%BmSolRefldInsReveal = SurfaceWindow(SurfNum)%BmSolRefldInsReveal + &
          BmSolRefldInsReveal

        SurfaceWindow(SurfNum)%BmSolRefldInsRevealReport = SurfaceWindow(SurfNum)%BmSolRefldInsRevealReport &
                                  + BeamSolarRad * BmSolRefldInsReveal  ! W, BeamSolarRad in W/m2
        SurfaceWindow(SurfNum)%BmSolRefldInsRevealRepEnergy = SurfaceWindow(SurfNum)%BmSolRefldInsRevealReport &
                                  * TimeStepZone * SecInHour

        ! Reflected solar from inside horizontal and vertical reveal incident on glazing
        SurfaceWindow(SurfNum)%InsRevealDiffOntoGlazing = SurfaceWindow(SurfNum)%InsRevealDiffOntoGlazing + &
          FracToGlassIns * BmSolRefldInsReveal / Surface(SurfNum)%Area

        ! Added TH 5/26/2009 for reporting purpose - diffuse on window glass from inside reveal (W)
        SurfaceWindow(SurfNum)%InsRevealDiffOntoGlazingReport = SurfaceWindow(SurfNum)%InsRevealDiffOntoGlazingReport + &
          BeamSolarRad * FracToGlassIns * BmSolRefldInsReveal

        ! Reflected solar from inside horizontal and vertical reveal incident on frame
        IF(FrameWidth > 0.0d0) THEN
          SurfaceWindow(SurfNum)%InsRevealDiffOntoFrame = SurfaceWindow(SurfNum)%InsRevealDiffOntoFrame + &
            (0.5d0-FracToGlassIns)*BmSolRefldInsReveal / SurfaceWindow(SurfNum)%FrameArea

          ! Added TH 5/26/2009 for reporting purpose - diffuse on window frame from inside reveal (W)
          SurfaceWindow(SurfNum)%InsRevealDiffOntoFrameReport = SurfaceWindow(SurfNum)%InsRevealDiffOntoFrameReport + &
            BeamSolarRad * (0.5d0-FracToGlassIns)*BmSolRefldInsReveal
        END IF

        ! Reflected solar from inside reveal going directly into zone and reflected from glass.
        ! Assumes half of solar reflected from inside reveal goes as diffuse radiation into the zone and
        ! half goes as diffuse radiation towards window.
        SurfaceWindow(SurfNum)%InsRevealDiffIntoZone = SurfaceWindow(SurfNum)%InsRevealDiffIntoZone +  &
          BmSolRefldInsReveal * (0.5d0 + DiffReflGlass * FracToGlassIns)

        ! Added TH 5/26/2009 for reporting purpose - diffuse into zone from inside reveal (W)
        SurfaceWindow(SurfNum)%InsRevealDiffIntoZoneReport = SurfaceWindow(SurfNum)%InsRevealDiffIntoZoneReport +  &
          BeamSolarRad * BmSolRefldInsReveal * (0.5d0 + DiffReflGlass * FracToGlassIns)

      END IF  ! End of check if A2ill > 0.0d0

    END IF  ! End of check if interior shade or blind is in place

  END DO  ! End of loop over vertical and horizontal reveal

END DO  ! End of surface loop

RETURN
END SUBROUTINE CalcBeamSolarOnWinRevealSurface

SUBROUTINE ReportSurfaceShading

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   April 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine uses the internal variables used in the Shading
          ! calculations and prepares them for reporting (at timestep level).

          ! METHODOLOGY EMPLOYED:
          ! Because all of the calculations are done on a "daily" basis in this
          ! module, it is difficult to formulate the values that might be useful
          ! for reporting.  SunLitFrac was the first of these two arrays to be
          ! made into "two dimensions".  It is not clear that both have to be
          ! two dimensions.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE OutputReportPredefined

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER SurfNum ! Loop Counter
  INTEGER RepCol  ! the column of the predefined report

  DO SurfNum=1,TotSurfaces
    SurfSunlitFrac(SurfNum)=SunLitFrac(SurfNum,HourOfDay,TimeStep)
    SurfSunlitArea(SurfNum)=SunLitFrac(SurfNum,HourOfDay,TimeStep)*Surface(SurfNum)%Area
  ENDDO
  !added for predefined reporting
  RepCol = 0
  IF (Month == 3 .and. DayOfMonth == 21) THEN
    IF ((HourOfDay .EQ. 9) .AND. (TimeStep .EQ. 4)) THEN
      RepCol = pdchSlfMar21_9
    ELSEIF ((HourOfDay .EQ. 12) .AND. (TimeStep .EQ. 4)) THEN
      RepCol = pdchSlfMar21_12
    ELSEIF ((HourOfDay .EQ. 15) .AND. (TimeStep .EQ. 4)) THEN
      RepCol = pdchSlfMar21_15
    END IF
  ELSEIF (Month == 6 .and. DayOfMonth == 21)  THEN
    IF ((HourOfDay .EQ. 9) .AND. (TimeStep .EQ. 4)) THEN
      RepCol = pdchSlfJun21_9
    ELSEIF ((HourOfDay .EQ. 12) .AND. (TimeStep .EQ. 4)) THEN
      RepCol = pdchSlfJun21_12
    ELSEIF ((HourOfDay .EQ. 15) .AND. (TimeStep .EQ. 4)) THEN
      RepCol = pdchSlfJun21_15
    END IF
  ELSEIF (Month == 12 .and. DayOfMonth == 21)  THEN
    IF ((HourOfDay .EQ. 9) .AND. (TimeStep .EQ. 4)) THEN
      RepCol = pdchSlfDec21_9
    ELSEIF ((HourOfDay .EQ. 12) .AND. (TimeStep .EQ. 4)) THEN
      RepCol = pdchSlfDec21_12
    ELSEIF ((HourOfDay .EQ. 15) .AND. (TimeStep .EQ. 4)) THEN
      RepCol = pdchSlfDec21_15
    END IF
  END IF
  IF (RepCol .NE. 0) THEN
    DO SurfNum=1,TotSurfaces
      IF (surface(SurfNum)%Class .EQ. SurfaceClass_Window) THEN
        CALL PreDefTableEntry(RepCol,surface(SurfNum)%Name,SurfSunlitFrac(SurfNum))
      END IF
    END DO
  END IF
  RETURN

END SUBROUTINE ReportSurfaceShading

SUBROUTINE ReportSurfaceErrors

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   November 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reports some recurring type errors that can get mixed up with more important
          ! errors in the error file.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataErrorTracking  ! for error tracking
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=8), PARAMETER, DIMENSION(4) :: MSG = (/'misses  ','        ','within  ','overlaps'/)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Loop1
  INTEGER Loop2
  INTEGER Count
  INTEGER TotCount
  CHARACTER(len=25) CountOut
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: SurfErrorReported
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: SurfErrorReported2

  IF (NumTooManyFigures+NumTooManyVertices+NumBaseSubSurround > 0) THEN
    CALL ShowMessage(' ')
    CALL ShowMessage('===== Recurring Surface Error Summary =====')
    CALL ShowMessage('The following surface error messages occurred.')
    CALL ShowMessage(' ')

    IF (NumBaseSubSurround > 0) THEN
      CALL ShowMessage('Base Surface does not surround subsurface errors occuring...')
      CALL ShowMessage('Check that the GlobalGeometryRules object is expressing the proper starting corner and '//  &
                     'direction [CounterClockwise/Clockwise]')
      CALL ShowMessage(' ')
    ENDIF

    ALLOCATE(SurfErrorReported(TotSurfaces))
    SurfErrorReported=.false.
    TotCount=0
    DO Loop1=1,NumBaseSubSurround
      Count=0
      IF (SurfErrorReported(TrackBaseSubSurround(Loop1)%SurfIndex1)) CYCLE
      DO Loop2=1,NumBaseSubSurround
        IF (TrackBaseSubSurround(Loop1)%SurfIndex1 == TrackBaseSubSurround(Loop2)%SurfIndex1 .and.  &
            TrackBaseSubSurround(Loop1)%MiscIndex == TrackBaseSubSurround(Loop2)%MiscIndex) THEN
          Count=Count+1
        ENDIF
      ENDDO
      WRITE(CountOut,*) Count
      TotCount=TotCount+Count
      TotalWarningErrors=TotalWarningErrors+Count-1
      CALL ShowWarningError('Base surface does not surround subsurface (CHKSBS), Overlap Status='//  &
                             TRIM(cOverLapStatus(TrackBaseSubSurround(Loop1)%MiscIndex)))
      CALL ShowContinueError('  The base surround errors occurred '//TRIM(ADJUSTL(CountOut))//' times.')
      DO Loop2=1,NumBaseSubSurround
        IF (TrackBaseSubSurround(Loop1)%SurfIndex1 == TrackBaseSubSurround(Loop2)%SurfIndex1 .and.  &
            TrackBaseSubSurround(Loop1)%MiscIndex == TrackBaseSubSurround(Loop2)%MiscIndex) THEN
          CALL ShowContinueError('Surface "'//TRIM(Surface(TrackBaseSubSurround(Loop1)%SurfIndex1)%Name)//  &
                                 '" '//TRIM(MSG(TrackBaseSubSurround(Loop1)%MiscIndex))//  &
                                 ' SubSurface "'//TRIM(Surface(TrackBaseSubSurround(Loop2)%SurfIndex2)%Name)//'"')
        ENDIF
      ENDDO
      SurfErrorReported(TrackBaseSubSurround(Loop1)%SurfIndex1)=.true.
    ENDDO
    IF (TotCount > 0) THEN
      CALL ShowMessage(' ')
      WRITE(CountOut,*) TotCount
      CALL ShowContinueError('  The base surround errors occurred '//TRIM(ADJUSTL(CountOut))//' times (total).')
      CALL ShowMessage(' ')
    ENDIF

    ALLOCATE(SurfErrorReported2(TotSurfaces))
    SurfErrorReported=.false.
    TotCount=0
    IF (NumTooManyVertices > 0) THEN
      CALL ShowMessage('Too many vertices [>='//  &
          TRIM(RoundSigDigits(MaxHCV))//'] in shadow overlap errors occurring...')
      CALL ShowMessage('These occur throughout the year and may occur several times for the same surfaces. '//  &
                     'You may be able to reduce them by adding Output:Diagnostics,DoNotMirrorDetachedShading;')
    ENDIF
    DO Loop1=1,NumTooManyVertices
      Count=0
      SurfErrorReported2=.false.
      IF (SurfErrorReported(TrackTooManyVertices(Loop1)%SurfIndex1)) CYCLE
      DO Loop2=1,NumTooManyVertices
        IF (TrackTooManyVertices(Loop1)%SurfIndex1 == TrackTooManyVertices(Loop2)%SurfIndex1) THEN
          Count=Count+1
        ENDIF
      ENDDO
      WRITE(CountOut,*) Count
      TotCount=TotCount+Count
      TotalWarningErrors=TotalWarningErrors+Count-1
      CALL ShowMessage(' ')
      CALL ShowWarningError('Too many vertices [>='//  &
          TRIM(RoundSigDigits(MaxHCV))//'] in a shadow overlap')
      CALL ShowContinueError('Overlapping figure='//TRIM(Surface(TrackTooManyVertices(Loop1)%SurfIndex1)%Name)//  &
                             ', Surface Class=['//TRIM(cSurfaceClass(Surface(TrackTooManyVertices(Loop1)%SurfIndex1)%Class))//']')
      CALL ShowContinueError('  This error occurred '//TRIM(ADJUSTL(CountOut))//' times.')
      DO Loop2=1,NumTooManyVertices
        IF (TrackTooManyVertices(Loop1)%SurfIndex1 == TrackTooManyVertices(Loop2)%SurfIndex1) THEN
          IF (SurfErrorReported2(TrackTooManyVertices(Loop2)%SurfIndex2)) CYCLE
          CALL ShowContinueError('Figure being Overlapped='//TRIM(Surface(TrackTooManyVertices(Loop2)%SurfIndex2)%Name)//  &
                       ', Surface Class=['//TRIM(cSurfaceClass(Surface(TrackTooManyVertices(Loop2)%SurfIndex2)%Class))//']')
          SurfErrorReported2(TrackTooManyVertices(Loop2)%SurfIndex2)=.true.
        ENDIF
      ENDDO
      SurfErrorReported(TrackTooManyVertices(Loop1)%SurfIndex1)=.true.
    ENDDO
    IF (TotCount > 0) THEN
      CALL ShowMessage(' ')
      WRITE(CountOut,*) TotCount
      CALL ShowContinueError('  The too many vertices errors occurred '//TRIM(ADJUSTL(CountOut))//' times (total).')
      CALL ShowMessage(' ')
    ENDIF

    SurfErrorReported=.false.
    TotCount=0
    IF (NumTooManyFigures > 0) THEN
      CALL ShowMessage('Too many figures [>='//  &
          TRIM(RoundSigDigits(MaxHCS))//'] in shadow overlap errors occurring...')
      CALL ShowMessage('These occur throughout the year and may occur several times for the same surfaces. '//  &
                     'You may be able to reduce them by adding OutputDiagnostics,DoNotMirrorDetachedShading;')
    ENDIF
    DO Loop1=1,NumTooManyFigures
      Count=0
      SurfErrorReported2=.false.
      IF (SurfErrorReported(TrackTooManyFigures(Loop1)%SurfIndex1)) CYCLE
      DO Loop2=1,NumTooManyFigures
        IF (TrackTooManyFigures(Loop1)%SurfIndex1 == TrackTooManyFigures(Loop2)%SurfIndex1) THEN
          Count=Count+1
        ENDIF
      ENDDO
      WRITE(CountOut,*) Count
      TotCount=TotCount+Count
      TotalWarningErrors=TotalWarningErrors+Count-1
      CALL ShowMessage(' ')
      CALL ShowWarningError('Too many figures [>='//  &
          TRIM(RoundSigDigits(MaxHCS))//'] in a shadow overlap')
      CALL ShowContinueError('Overlapping figure='//TRIM(Surface(TrackTooManyFigures(Loop1)%SurfIndex1)%Name)//  &
                             ', Surface Class=['//TRIM(cSurfaceClass(Surface(TrackTooManyFigures(Loop1)%SurfIndex1)%Class))//']')
      CALL ShowContinueError('  This error occurred '//TRIM(ADJUSTL(CountOut))//' times.')
      DO Loop2=1,NumTooManyFigures
        IF (TrackTooManyFigures(Loop1)%SurfIndex1 == TrackTooManyFigures(Loop2)%SurfIndex1) THEN
          IF (SurfErrorReported2(TrackTooManyFigures(Loop2)%SurfIndex2)) CYCLE
          CALL ShowContinueError('Figure being Overlapped='//TRIM(Surface(TrackTooManyFigures(Loop2)%SurfIndex2)%Name)//  &
                       ', Surface Class=['//TRIM(cSurfaceClass(Surface(TrackTooManyFigures(Loop2)%SurfIndex2)%Class))//']')
          SurfErrorReported2(TrackTooManyFigures(Loop2)%SurfIndex2)=.true.
        ENDIF
      ENDDO
      SurfErrorReported(TrackTooManyFigures(Loop1)%SurfIndex1)=.true.
    ENDDO
    IF (TotCount > 0) THEN
      CALL ShowMessage(' ')
      WRITE(CountOut,*) TotCount
      CALL ShowContinueError('  The too many figures errors occurred '//TRIM(ADJUSTL(CountOut))//' times (total).')
      CALL ShowMessage(' ')
    ENDIF
    DEALLOCATE(SurfErrorReported)
    DEALLOCATE(SurfErrorReported2)
  ENDIF

  RETURN

END SUBROUTINE ReportSurfaceErrors

SUBROUTINE ComputeWinShadeAbsorpFactors

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   Mar 2001
          !       MODIFIED       Oct 2002,FCW: change ConstrNumSh = WindowShadingControl(WinShadeCtrlNum)%ShadedConstruction
          !                      to Surface(SurfNum)%ShadedConstruction
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Called by InitSolarCalculations. Finds fractions that apportion radiation absorbed by a
          ! window shade to the two faces of the shade. For radiation incident from the left,
          ! ShadeAbsFacFace(1) is the fraction of radiation absorbed in the left-hand half of the
          ! of the shade and ShadeAbsFacFace(2) is the fraction absorbed in the right-hand half.
          ! The shade is assumed to be homogeneous.

          ! REFERENCES: See EnergyPlus engineering documentation
          ! USE STATEMENTS: na

IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS: na
          ! INTERFACE BLOCK SPECIFICATIONS: na
          ! DERIVED TYPE DEFINITIONS: na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

INTEGER            :: WinShadeCtrlNum   ! Window shading control number
INTEGER            :: SurfNum           ! Surface number
INTEGER            :: ConstrNumSh       ! Window construction number with shade
INTEGER            :: TotLay            ! Total layers in a construction
INTEGER            :: MatNumSh          ! Shade layer material number
REAL(r64)          :: AbsorpEff         ! Effective absorptance of isolated shade layer (fraction of
                                        !  of incident radiation remaining after reflected portion is
                                        !  removed that is absorbed
DO SurfNum = 1,TotSurfaces
  IF(Surface(SurfNum)%Class == SurfaceClass_Window .AND. Surface(SurfNum)%WindowShadingControlPtr > 0) THEN
    WinShadeCtrlNum = Surface(SurfNum)%WindowShadingControlPtr
    IF(WindowShadingControl(WinShadeCtrlNum)%ShadingType == WSC_ST_InteriorShade .OR. &
       WindowShadingControl(WinShadeCtrlNum)%ShadingType == WSC_ST_ExteriorShade .OR. &
       WindowShadingControl(WinShadeCtrlNum)%ShadingType == WSC_ST_BetweenGlassShade) THEN
      ConstrNumSh = Surface(SurfNum)%ShadedConstruction
      TotLay = Construct(ConstrNumSh)%TotLayers
      IF(WindowShadingControl(WinShadeCtrlNum)%ShadingType == WSC_ST_InteriorShade) THEN
        MatNumSh = Construct(ConstrNumSh)%LayerPoint(TotLay)  ! Interior shade
      ELSE IF(WindowShadingControl(WinShadeCtrlNum)%ShadingType == WSC_ST_ExteriorShade) THEN
        MatNumSh = Construct(ConstrNumSh)%LayerPoint(1)       ! Exterior shade
      ELSE IF(WindowShadingControl(WinShadeCtrlNum)%ShadingType == WSC_ST_BetweenGlassShade) THEN
        IF(Construct(ConstrNumSh)%TotGlassLayers == 2) THEN
          MatNumSh = Construct(ConstrNumSh)%LayerPoint(3)   ! Double pane with between-glass shade
        ELSE
          MatNumSh = Construct(ConstrNumSh)%LayerPoint(5)   ! Triple pane with between-glass shade
        END IF
      END IF
      AbsorpEff = Material(MatNumSh)%AbsorpSolar/(Material(MatNumSh)%AbsorpSolar + Material(MatNumSh)%Trans + 0.0001d0)
      AbsorpEff = MIN(MAX(AbsorpEff,0.0001d0),0.999d0) ! Constrain to avoid problems with following log eval
      SurfaceWindow(SurfNum)%ShadeAbsFacFace(1) = (1.d0-EXP(0.5d0*LOG(1.0d0-AbsorpEff)))/AbsorpEff
      SurfaceWindow(SurfNum)%ShadeAbsFacFace(2) = 1.d0 - SurfaceWindow(SurfNum)%ShadeAbsFacFace(1)
    END IF
  END IF
END DO

END SUBROUTINE ComputeWinShadeAbsorpFactors

SUBROUTINE CalcWinTransDifSolInitialDistribution

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rob Hitchcock
          !       DATE WRITTEN   July 2007
          !       MODIFIED       N/A
          !       RE-ENGINEERED  N/A

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the initial distribution
          ! of diffuse solar transmitted through exterior windows
          ! to individual heat transfer surfaces in each zone.

          ! METHODOLOGY EMPLOYED:
          ! Apportions diffuse solar transmitted through each exterior window
          ! that is then absorbed, reflected, and/or transmitted
          ! by other heat transfer surfaces in the zone.
          ! Calculations use:
          ! 1. WinDifSolar calculated in SUBROUTINE CalcInteriorSolarDistribution,
          ! 2. view factors between each exterior window and
          ! other heat transfer surfaces in a zone
          ! calculated in SUBROUTINE CalcApproximateViewFactors, and
          ! 3. surface absorptances, reflectances, and transmittances
          ! determined here using revised code from SUBROUTINE InitIntSolarDistribution

          ! REFERENCES:

          ! USE STATEMENTS:
  USE General, ONLY: InterpSw, InterpSlatAng
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE DataViewFactorInformation

  USE DataHeatBalSurface, ONLY: InitialDifSolInAbs, InitialDifSolInTrans
  USE DataHeatBalance, ONLY: InitialDifSolwinAbs, InitialZoneDifSolReflW

  USE WindowEquivalentLayer, ONLY: CalcEQLOpticalProperty
  USE DataWindowEquivalentLayer

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ZoneNum            ! DO loop counter for zones
  INTEGER :: AdjZoneNum         ! Index for adjacent zones
  INTEGER :: AdjSurfNum         ! Index for adjacent surfaces
  INTEGER :: DifTransSurfNum    ! Diffuse Solar Transmitting Surface number
  INTEGER :: HeatTransSurfNum   ! Heat Transfer Surface number
  INTEGER :: ConstrNum          ! Construction number
  INTEGER :: AdjConstrNum       ! Construction number of other side surface
  INTEGER :: ConstrNumSh        ! Shaded construction number
  INTEGER :: IGlass             ! Glass layer counter
  INTEGER :: TotGlassLayers     ! Number of glass layers in a window construction
  INTEGER :: ShadeFlag          ! Shading flag
  REAL(r64)    :: AbsInt             ! Tmp var for Inside surface short-wave absorptance
  REAL(r64)    :: MovInsulSchedVal   ! Value of the movable insulation schedule for current time
  REAL(r64)    :: HMovInsul          ! Conductance of movable insulation
  REAL(r64)    :: InsideDifAbsorptance ! Inside diffuse solar absorptance of a surface
  REAL(r64)    :: InsideDifReflectance ! Inside diffuse solar reflectance of a surface
  INTEGER :: BlNum              ! Blind number
  REAL(r64)    :: BlAbsDiffBk        ! Glass layer back diffuse solar absorptance when blind in place
  REAL(r64)    :: AbsDiffBkBl        ! Blind diffuse back solar absorptance as part of glazing system

!  REAL(r64)    :: DividerSolAbs      ! Window divider solar absorptance
!  REAL(r64)    :: DividerSolRefl     ! Window divider solar reflectance
!  INTEGER :: MatNumGl           ! Glass layer material number
!  INTEGER :: MatNumSh           ! Shade layer material number
!  REAL(r64)    :: TransGl,ReflGl,AbsGl ! Glass layer solar transmittance, reflectance, absorptance

  REAL(r64)    :: ViewFactor         ! temp var for view factor
  REAL(r64)    :: ViewFactorTotal    ! debug var for view factor total
  REAL(r64)    :: WinDifSolarTrans   ! debug var for WinDifSolar() [W]
  REAL(r64)    :: WinDifSolarDistTotl    ! debug var for window total distributed diffuse solar [W]
  REAL(r64)    :: WinDifSolarDistAbsorbedTotl    ! debug var for individual exterior window total distributed
                                            !    diffuse solar absorbed [W]
  REAL(r64)    :: WinDifSolarDistReflectedTotl   ! debug var for individual exterior window total distributed
                                            !    diffuse solar reflected [W]
  REAL(r64)    :: WinDifSolarDistTransmittedTotl ! debug var for individual exterior window total distributed
                                            !    diffuse solar transmitted [W]
  REAL(r64)    :: WinDifSolLayAbsW   ! temp var for diffuse solar absorbed by individual glass layer [W]
  REAL(r64)    :: ZoneDifSolarTrans   ! debug var for WinDifSolar() [W]
  REAL(r64)    :: ZoneDifSolarDistTotl    ! debug var for zone total distributed diffuse solar [W]
  REAL(r64)    :: ZoneDifSolarDistAbsorbedTotl    ! debug var for zone total distributed diffuse solar absorbed [W]
  REAL(r64)    :: ZoneDifSolarDistReflectedTotl   ! debug var for zone total distributed diffuse solar reflected [W]
  REAL(r64)    :: ZoneDifSolarDistTransmittedTotl ! debug var for zone total distributed diffuse solar transmitted [W]

  REAL(r64)    :: DifSolarAbsW       ! temp var for diffuse solar absorbed by surface [W]
  REAL(r64)    :: DifSolarAbs        ! temp var for diffuse solar absorbed by surface [W/m2]
  REAL(r64)    :: DifSolarReflW      ! temp var for diffuse solar reflected by surface [W]
  REAL(r64)    :: DifSolarTransW     ! temp var for diffuse solar transmitted through interior window surface [W]
  REAL(r64)    :: ShBlDifSolarAbsW   ! temp var for diffuse solar absorbed by shade/blind [W]

  REAL(r64) :: AbsSolBeamEQL(CFSMAXNL+1,2) ! absorbed exterior beam radiation by layers fraction
  REAL(r64) :: AbsSolDiffEQL(CFSMAXNL+1,2) ! absorbed exterior diffuse radiation by layers fraction
  REAL(r64) :: AbsSolBeamBackEQL(CFSMAXNL+1,2) ! absorbed interior beam radiation by layers fraction from back
  REAL(r64) :: AbsSolDiffBackEQL(CFSMAXNL+1,2) ! absorbed exterior diffuse radiation by layers fraction from back
  INTEGER   :: EQLNum                      ! equivalent layer fenestration index
  INTEGER   :: Lay                         ! equivalent layer fenestration layer index

  INTEGER :: FirstZoneSurf      ! conversion index for ViewFactor
  INTEGER :: LastZoneSurf       ! debug

  ! Init accumulators for absorbed diffuse solar for all surfaces for later heat balance calcs
  InitialDifSolInAbs  = 0.0d0
  InitialDifSolwinAbs = 0.0d0

  ! Init accumulator for total reflected diffuse solar within each zone for interreflection calcs
  InitialZoneDifSolReflW = 0.0d0

  ! Init accumulator for transmitted diffuse solar for all surfaces for reporting
  InitialDifSolInTrans  = 0.0d0

  ! Loop over all zones doing initial distribution of diffuse solar to interior heat transfer surfaces
  DO ZoneNum = 1, NumOfZones

    ! Init Zone accumulators for debugging
    ZoneDifSolarTrans = 0.0d0
    ZoneDifSolarDistAbsorbedTotl = 0.0d0
    ZoneDifSolarDistReflectedTotl = 0.0d0
    ZoneDifSolarDistTransmittedTotl = 0.0d0

    ! Loop over all diffuse solar transmitting surfaces (i.e., exterior windows and TDDs) in the current zone
    FirstZoneSurf = Zone(ZoneNum)%SurfaceFirst
    LastZoneSurf = Zone(ZoneNum)%SurfaceLast
    DO DifTransSurfNum = Zone(ZoneNum)%SurfaceFirst, Zone(ZoneNum)%SurfaceLast
      ! Skip surfaces that are not exterior, except for TDD_Diffusers
      IF (((Surface(DifTransSurfNum)%ExtBoundCond /= ExternalEnvironment) .AND. &
            (Surface(DifTransSurfNum)%ExtBoundCond /= OtherSideCondModeledExt) ) &
           .AND. SurfaceWindow(DifTransSurfNum)%OriginalClass /= SurfaceClass_TDD_Diffuser) CYCLE

      ! Do I need to do anything special for TDDs?
      IF (SurfaceWindow(DifTransSurfNum)%OriginalClass == SurfaceClass_TDD_Diffuser) THEN
      END IF

      ! Skip surfaces that are not exterior windows or TDD diffusers
      IF(Surface(DifTransSurfNum)%Class /= SurfaceClass_Window .AND. &
          SurfaceWindow(DifTransSurfNum)%OriginalClass /= SurfaceClass_TDD_Diffuser) CYCLE

      !----------------------------------------------------------------------------------------------------------
      ! DISTRIBUTE TRANSMITTED DIFFUSE SOLAR THROUGH EXTERIOR WINDOWS AND TDDS TO INTERIOR HEAT TRANSFER SURFACES
      !----------------------------------------------------------------------------------------------------------

      ! Init transmitted solar debug vars
      ViewFactorTotal = 0.0d0
      WinDifSolarTrans = WinDifSolar(DifTransSurfNum)
      ZoneDifSolarTrans = ZoneDifSolarTrans + WinDifSolar(DifTransSurfNum)

      ! Init Exterior Window accumulators for debugging
      WinDifSolarDistAbsorbedTotl = 0.0d0
      WinDifSolarDistReflectedTotl = 0.0d0
      WinDifSolarDistTransmittedTotl = 0.0d0

      ! Loop over all heat transfer surfaces in the current zone that might receive diffuse solar
      DO HeatTransSurfNum = Zone(ZoneNum)%SurfaceFirst, Zone(ZoneNum)%SurfaceLast
        ! Skip surfaces that are not heat transfer surfaces
        IF (.NOT.Surface(HeatTransSurfNum)%HeatTransSurf) CYCLE
        ! Skip tubular daylighting device domes
        IF (Surface(HeatTransSurfNum)%Class == SurfaceClass_TDD_Dome) CYCLE

        ! View factor from current (sending) window DifTransSurfNum to current (receiving) surface HeatTransSurfNum
        ViewFactor = ZoneInfo(ZoneNum)%F(DifTransSurfNum-FirstZoneSurf+1, HeatTransSurfNum-FirstZoneSurf+1)
        ! debug ViewFactorTotal
        ViewFactorTotal = ViewFactorTotal + ViewFactor  ! debug

        ! Skip receiving surfaces with 0.0 view factor
        IF (ViewFactor <= 0.0d0) CYCLE

        ! Calculate diffuse solar from current exterior window absorbed and reflected by current heat transfer surface
        ! And calculate transmitted diffuse solar to adjacent zones through interior windows
        ConstrNum=Surface(HeatTransSurfNum)%Construction
        IF (Construct(ConstrNum)%TransDiff <= 0.0d0) THEN  ! Interior Opaque Surface

          ! Determine the inside (back) diffuse solar absorptance
          ! and reflectance of the current heat transfer surface
          InsideDifAbsorptance = Construct(ConstrNum)%InsideAbsorpSolar
          ! Check for movable insulation; reproduce code from subr. EvalInsideMovableInsulation;
          ! Can't call that routine here since cycle prevents SolarShadingGeometry from USEing
          ! HeatBalanceSurfaceManager, which contains EvalInsideMovableInsulation
          HMovInsul = 0.0d0
          IF (Surface(HeatTransSurfNum)%MaterialMovInsulInt.GT.0) THEN
            MovInsulSchedVal = GetCurrentScheduleValue(Surface(HeatTransSurfNum)%SchedMovInsulExt)
            IF (MovInsulSchedVal.LE.0.0d0) THEN ! Movable insulation not present at current time
              HMovInsul = 0.0d0
            ELSE  ! Movable insulation present
              HMovInsul = 1.0d0/(MovInsulSchedVal*Material(Surface(HeatTransSurfNum)%MaterialMovInsulInt)%Resistance)
              AbsInt    = Material(Surface(HeatTransSurfNum)%MaterialMovInsulInt)%AbsorpSolar
            END IF
          END IF
          IF (HMovInsul > 0.0d0) InsideDifAbsorptance = AbsInt  ! Movable inside insulation present
          ! Inside (back) diffuse solar reflectance is assumed to be 1 - absorptance
          InsideDifReflectance = 1.0d0 - InsideDifAbsorptance

          ! Absorbed diffuse solar [W] = current window transmitted diffuse solar [W]
          !    * view factor from current (sending) window DifTransSurfNum to current (receiving) surface HeatTransSurfNum
          !    * current surface inside solar absorptance
          DifSolarAbsW = WinDifSolar(DifTransSurfNum) & ! [W]
                          * ViewFactor &
                          * InsideDifAbsorptance

          ! Absorbed diffuse solar [W/m2] = Absorbed diffuse solar [W]
          !                                 / current surface net area
          DifSolarAbs = DifSolarAbsW &
                         / Surface(HeatTransSurfNum)%Area

          ! Accumulate absorbed diffuse solar [W/m2] on this surface for heat balance calcs
          InitialDifSolInAbs(HeatTransSurfNum) = InitialDifSolInAbs(HeatTransSurfNum) &
                                                  + DifSolarAbs

          ! Reflected diffuse solar [W] = current window transmitted diffuse solar
          !    * view factor from current (sending) window DifTransSurfNum to current (receiving) surface HeatTransSurfNum
          !    * current window inside solar reflectance
          DifSolarReflW = WinDifSolar(DifTransSurfNum) &
                          * ViewFactor &
                          * InsideDifReflectance

          ! Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
          InitialZoneDifSolReflW(ZoneNum) = InitialZoneDifSolReflW(ZoneNum) &
                                                   + DifSolarReflW ! [W]

          ! Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
          ! For opaque surfaces all incident diffuse is either absorbed or reflected
          WinDifSolarDistAbsorbedTotl = WinDifSolarDistAbsorbedTotl + DifSolarAbsW  ! debug [W]
          WinDifSolarDistReflectedTotl = WinDifSolarDistReflectedTotl + DifSolarReflW ! debug [W]
          ZoneDifSolarDistAbsorbedTotl = ZoneDifSolarDistAbsorbedTotl + DifSolarAbsW  ! debug [W]
          ZoneDifSolarDistReflectedTotl = ZoneDifSolarDistReflectedTotl + DifSolarReflW ! debug [W]

        ELSE  ! Exterior or Interior Window

          ConstrNumSh = Surface(HeatTransSurfNum)%ShadedConstruction
          IF(SurfaceWindow(HeatTransSurfNum)%StormWinFlag==1) THEN
            ConstrNum   = Surface(HeatTransSurfNum)%StormWinConstruction
            ConstrNumSh = Surface(HeatTransSurfNum)%StormWinShadedConstruction
          END IF
          TotGlassLayers = Construct(ConstrNum)%TotGlassLayers
          ShadeFlag = SurfaceWindow(HeatTransSurfNum)%ShadingFlag

          IF (SurfaceWindow(HeatTransSurfNum)%WindowModelType /= WindowEQLModel) THEN
              IF(ShadeFlag <= 0) THEN  ! No window shading
                ! Init accumulator for transmittance calc below
                DifSolarAbsW = 0.0d0

                ! Calc diffuse solar absorbed by all window glass layers
                ! Note: I am assuming here that individual glass layer absorptances have been corrected
                !       to account for layer by layer transmittance and reflection effects.
                DO IGlass = 1, TotGlassLayers
                  ! Calc diffuse solar absorbed from the inside by each window glass layer [W]
                  AbsInt = Construct(ConstrNum)%AbsDiffBack(IGlass)
                  WinDifSolLayAbsW = WinDifSolar(DifTransSurfNum)* ViewFactor * Construct(ConstrNum)%AbsDiffBack(IGlass)

                  ! Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
                  DifSolarAbsW = DifSolarAbsW + WinDifSolLayAbsW

                  ! Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                  WinDifSolarDistAbsorbedTotl = WinDifSolarDistAbsorbedTotl + WinDifSolLayAbsW  ! debug
                  ZoneDifSolarDistAbsorbedTotl = ZoneDifSolarDistAbsorbedTotl + WinDifSolLayAbsW  ! debug

                  ! Accumulate diffuse solar absorbed from the inside by each window glass layer [W/m2] for heat balance calcs
                  InitialDifSolwinAbs(HeatTransSurfNum,IGlass) = InitialDifSolwinAbs(HeatTransSurfNum,IGlass)  &
                                                             + (WinDifSolLayAbsW  &
                                                             / Surface(HeatTransSurfNum)%Area)
                END DO

                ! Calc diffuse solar reflected back to zone
                ! I don't really care if this is a window or opaque surface since I am just
                ! accumulating all reflected diffuse solar in a zone bucket for "interreflected" distribution
                ! Reflected diffuse solar [W] = current window transmitted diffuse solar
                !    * view factor from current (sending) window DifTransSurfNum to current (receiving) surface HeatTransSurfNum
                !    * current window inside solar reflectance
                InsideDifReflectance = Construct(ConstrNum)%ReflectSolDiffBack
                DifSolarReflW = WinDifSolar(DifTransSurfNum) &
                                * ViewFactor &
                                * Construct(ConstrNum)%ReflectSolDiffBack

                ! Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
                InitialZoneDifSolReflW(ZoneNum) = InitialZoneDifSolReflW(ZoneNum) &
                                              + DifSolarReflW ! [W]

                ! Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                WinDifSolarDistReflectedTotl = WinDifSolarDistReflectedTotl + DifSolarReflW ! debug
                ZoneDifSolarDistReflectedTotl = ZoneDifSolarDistReflectedTotl + DifSolarReflW ! debug

                !------------------------------------------------------------------------------
                ! DISTRIBUTE TRANSMITTED DIFFUSE SOLAR THROUGH INTERIOR WINDOW TO ADJACENT ZONE
                !------------------------------------------------------------------------------

                ! If this receiving window surface (HeatTransSurfNum) is an interior window,
                ! calc distributed solar transmitted to adjacent zone [W]
                ! NOTE: This calc is here because interior windows are currently assumed to have no shading

                ! Get the adjacent surface number for this receiving window surface
                AdjSurfNum = Surface(HeatTransSurfNum)%ExtBoundCond
                ! If the adjacent surface number is > 0, this is an interior window
                IF(AdjSurfNum > 0) THEN ! this is an interior window surface

                  ! Calc diffuse solar from current exterior window
                  ! transmitted through this interior window to adjacent zone [W]
                  ! Transmitted diffuse solar [W] = current exterior window transmitted diffuse solar
                  !    * view factor from current (sending) window DifTransSurfNum to current (receiving) surface HeatTransSurfNum
                  !    - diffuse absorbed by this interior window
                  !    - diffuse reflected by this interior window
                  DifSolarTransW = WinDifSolar(DifTransSurfNum) &
                                    * ViewFactor &
                                    - DifSolarAbsW &
                                    - DifSolarReflW
    ! HERE 8/15/07 Note Construct(AdjConstrNum)%TransDiff could be used here since the "front" transmittance for an interior window
    ! in the adjacent zone is the correct direction as long as I use the Construct() of the Surface in the adjacent zone.
    ! However, the above calculation better conserves energy, although possibly at the expense of less accurate
    ! transmittance calcs.
    ! Preliminary tests showed fairly good agreement between the two DifSolarTransW calculation methods,
    ! but for consistency I stuck with the above.
                  AdjConstrNum=Surface(AdjSurfNum)%Construction
    !              DifSolarTransW = WinDifSolar(DifTransSurfNum) &
    !                                * ViewFactor &
    !                                * Construct(AdjConstrNum)%TransDiff

                  ! Get the adjacent zone index
                  AdjZoneNum = Surface(AdjSurfNum)%Zone

                  ! Call routine to distribute diffuse solar transmitted through this interior window into adjacent zone
                  CALL CalcInteriorWinTransDifSolInitialDistribution(AdjZoneNum, AdjSurfNum, DifSolarTransW)

                ELSE ! this is an exterior window surface

                  ! Calc transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
                  ! This is not very effective since it assigns whatever distributed diffuse solar has not been
                  ! absorbed or reflected to transmitted.
                  DifSolarTransW = WinDifSolar(DifTransSurfNum) &
                                  * ViewFactor &
                                  - DifSolarAbsW &
                                  - DifSolarReflW

                END IF ! this is an interior window surface

                ! Accumulate transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
                WinDifSolarDistTransmittedTotl = WinDifSolarDistTransmittedTotl + DifSolarTransW  ! debug [W]
                ZoneDifSolarDistTransmittedTotl = ZoneDifSolarDistTransmittedTotl + DifSolarTransW ! debug [W]

                ! Accumulate transmitted diffuse solar for reporting
                InitialDifSolInTrans(HeatTransSurfNum) = InitialDifSolInTrans(HeatTransSurfNum) &
                                                         + (DifSolarTransW  &
                                                         / Surface(HeatTransSurfNum)%Area)

              ELSE IF(ShadeFlag == IntShadeOn .OR. ShadeFlag >= 3) THEN
                    ! Interior, exterior or between-glass shade, screen or blind in place

                ! Init accumulator for transmittance calc below
                DifSolarAbsW = 0.0d0
                WinDifSolLayAbsW = 0.0d0

                ! First calc diffuse solar absorbed by each glass layer in this window with shade/blind in place
                DO IGlass = 1,Construct(ConstrNumSh)%TotGlassLayers
                  IF(ShadeFlag==IntShadeOn .OR. ShadeFlag==ExtShadeOn .OR. ShadeFlag==BGShadeOn .OR. ShadeFlag==ExtScreenOn) THEN
                    ! Calc diffuse solar absorbed in each window glass layer and shade
                    WinDifSolLayAbsW = WinDifSolar(DifTransSurfNum)* ViewFactor * Construct(ConstrNumSh)%AbsDiffBack(IGlass)
                  END IF

                  IF(ShadeFlag == IntBlindOn .OR. ShadeFlag == ExtBlindOn .OR. ShadeFlag == BGBlindOn) THEN
                    BlAbsDiffBk = InterpSlatAng(SurfaceWindow(HeatTransSurfNum)%SlatAngThisTS,  &
                                         SurfaceWindow(HeatTransSurfNum)%MovableSlats, &
                                         Construct(ConstrNumSh)%BlAbsDiffBack(IGlass,1:MaxSlatAngs))
                    ! Calc diffuse solar absorbed in each window glass layer and shade
                    WinDifSolLayAbsW = WinDifSolar(DifTransSurfNum)* ViewFactor * BlAbsDiffBk
                  END IF

                  ! Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
                  DifSolarAbsW = DifSolarAbsW + WinDifSolLayAbsW

                  ! Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                  WinDifSolarDistAbsorbedTotl = WinDifSolarDistAbsorbedTotl + WinDifSolLayAbsW  ! debug
                  ZoneDifSolarDistAbsorbedTotl = ZoneDifSolarDistAbsorbedTotl + WinDifSolLayAbsW  ! debug

                  ! Accumulate diffuse solar absorbed from the inside by each window glass layer [W/m2] for heat balance calcs
                  InitialDifSolwinAbs(HeatTransSurfNum,IGlass) = InitialDifSolwinAbs(HeatTransSurfNum,IGlass)  &
                                                                   + (WinDifSolLayAbsW  &
                                                                   / Surface(HeatTransSurfNum)%Area)
               END DO

                ! Next calc diffuse solar reflected back to zone from window with shade or blind on
                ! Diffuse back solar reflectance, bare glass or shade on
                InsideDifReflectance = Construct(ConstrNum)%ReflectSolDiffBack
                IF(ShadeFlag == IntBlindOn .OR. ShadeFlag == ExtBlindOn) THEN
                  ! Diffuse back solar reflectance, blind present, vs. slat angle
                  InsideDifReflectance = InterpSlatAng(SurfaceWindow(HeatTransSurfNum)%SlatAngThisTS, &
                                          SurfaceWindow(HeatTransSurfNum)%MovableSlats, &
                                          Construct(ConstrNum)%BlReflectSolDiffBack)
                END IF
                DifSolarReflW = WinDifSolar(DifTransSurfNum) &
                                * ViewFactor &
                                * InsideDifReflectance

                ! Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
                InitialZoneDifSolReflW(ZoneNum) = InitialZoneDifSolReflW(ZoneNum) &
                                              + DifSolarReflW ! [W]

                ! Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                WinDifSolarDistReflectedTotl = WinDifSolarDistReflectedTotl + DifSolarReflW ! debug
                ZoneDifSolarDistReflectedTotl = ZoneDifSolarDistReflectedTotl + DifSolarReflW ! debug

                ! Now calc diffuse solar absorbed by shade/blind itself
                BlNum = SurfaceWindow(HeatTransSurfNum)%BlindNumber
                IF(ShadeFlag==IntShadeOn .OR. ShadeFlag==ExtShadeOn .OR. ShadeFlag==BGShadeOn .OR. ShadeFlag==ExtScreenOn) THEN
                  ! Calc diffuse solar absorbed by shade or screen [W]
                  ShBlDifSolarAbsW = WinDifSolar(DifTransSurfNum)* ViewFactor * Construct(ConstrNumSh)%AbsDiffBackShade
                END IF
                IF(ShadeFlag==IntBlindOn .OR. ShadeFlag==ExtBlindOn .OR. ShadeFlag==BGBlindOn) THEN
                  ! Calc diffuse solar absorbed by blind [W]
                  AbsDiffBkBl = InterpSlatAng(SurfaceWindow(HeatTransSurfNum)%SlatAngThisTS,  &
                                         SurfaceWindow(HeatTransSurfNum)%MovableSlats, &
                                         Construct(ConstrNumSh)%AbsDiffBackBlind)
                  ShBlDifSolarAbsW = WinDifSolar(DifTransSurfNum)* ViewFactor * AbsDiffBkBl
                END IF
                ! Correct for divider shadowing
                IF(ShadeFlag == ExtShadeOn.OR.ShadeFlag == ExtBlindOn.OR.ShadeFlag == ExtScreenOn) &
                  ShBlDifSolarAbsW = ShBlDifSolarAbsW * SurfaceWindow(HeatTransSurfNum)%GlazedFrac

                ! Accumulate diffuse solar absorbed  by shade or screen [W/m2] for heat balance calcs
                SurfaceWindow(HeatTransSurfNum)%InitialDifSolAbsByShade = SurfaceWindow(HeatTransSurfNum)%InitialDifSolAbsByShade &
                                                                    + (ShBlDifSolarAbsW / Surface(HeatTransSurfNum)%Area)

                ! Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
                DifSolarAbsW = DifSolarAbsW + ShBlDifSolarAbsW

                ! Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                WinDifSolarDistAbsorbedTotl = WinDifSolarDistAbsorbedTotl + ShBlDifSolarAbsW  ! debug
                ZoneDifSolarDistAbsorbedTotl = ZoneDifSolarDistAbsorbedTotl + ShBlDifSolarAbsW  ! debug

                ! Accumulate transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
                ! This is not very effective since it assigns whatever distributed diffuse solar has not been
                ! absorbed or reflected to transmitted.
                DifSolarTransW = WinDifSolar(DifTransSurfNum) &
                                * ViewFactor &
                                - DifSolarAbsW &
                                - DifSolarReflW
                WinDifSolarDistTransmittedTotl = WinDifSolarDistTransmittedTotl + DifSolarTransW  ! debug [W]
                ZoneDifSolarDistTransmittedTotl = ZoneDifSolarDistTransmittedTotl + DifSolarTransW ! debug [W]

                ! Accumulate transmitted diffuse solar for reporting
                InitialDifSolInTrans(HeatTransSurfNum) = InitialDifSolInTrans(HeatTransSurfNum) &
                                                         + (DifSolarTransW  &
                                                         / Surface(HeatTransSurfNum)%Area)

              ELSE IF(ShadeFlag == SwitchableGlazing) THEN  ! Switchable glazing
                ! Init accumulator for transmittance calc below
                DifSolarAbsW = 0.0d0

                DO IGlass = 1,TotGlassLayers
                  ! Calc diffuse solar absorbed in each window glass layer
                  WinDifSolLayAbsW = WinDifSolar(DifTransSurfNum)* ViewFactor &
                                      * InterpSw(SurfaceWindow(HeatTransSurfNum)%SwitchingFactor,  &
                                        Construct(ConstrNum)%AbsDiffBack(IGlass),  &
                                        Construct(ConstrNumSh)%AbsDiffBack(IGlass))

                  ! Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
                  DifSolarAbsW = DifSolarAbsW + WinDifSolLayAbsW

                  ! Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                  WinDifSolarDistAbsorbedTotl = WinDifSolarDistAbsorbedTotl + WinDifSolLayAbsW  ! debug
                  ZoneDifSolarDistAbsorbedTotl = ZoneDifSolarDistAbsorbedTotl + WinDifSolLayAbsW  ! debug

                  ! Accumulate diffuse solar absorbed from the inside by each window glass layer [W/m2] for heat balance calcs
                  InitialDifSolwinAbs(HeatTransSurfNum,IGlass) = InitialDifSolwinAbs(HeatTransSurfNum,IGlass)  &
                                                                   + (WinDifSolLayAbsW  &
                                                                   / Surface(HeatTransSurfNum)%Area)

                END DO

                ! Calc diffuse solar reflected back to zone
                DifSolarReflW = WinDifSolar(DifTransSurfNum) &
                                * ViewFactor &
                                * InterpSw(SurfaceWindow(HeatTransSurfNum)%SwitchingFactor,  &
                                  Construct(ConstrNum)%ReflectSolDiffBack,  &
                                  Construct(ConstrNumSh)%ReflectSolDiffBack)

                ! Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
                InitialZoneDifSolReflW(ZoneNum) = InitialZoneDifSolReflW(ZoneNum) &
                                              + DifSolarReflW ! [W]

                ! Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                WinDifSolarDistReflectedTotl = WinDifSolarDistReflectedTotl + DifSolarReflW ! debug
                ZoneDifSolarDistReflectedTotl = ZoneDifSolarDistReflectedTotl + DifSolarReflW ! debug

                ! Accumulate transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
                ! This is not very effective since it assigns whatever distributed diffuse solar has not been
                ! absorbed or reflected to transmitted.
                DifSolarTransW = WinDifSolar(DifTransSurfNum) &
                                * ViewFactor &
                                - DifSolarAbsW &
                                - DifSolarReflW
                WinDifSolarDistTransmittedTotl = WinDifSolarDistTransmittedTotl + DifSolarTransW  ! debug [W]
                ZoneDifSolarDistTransmittedTotl = ZoneDifSolarDistTransmittedTotl + DifSolarTransW ! debug [W]

                ! Accumulate transmitted diffuse solar for reporting
                InitialDifSolInTrans(HeatTransSurfNum) = InitialDifSolInTrans(HeatTransSurfNum) &
                                                         + (DifSolarTransW  &
                                                         / Surface(HeatTransSurfNum)%Area)

              END IF  ! End of shading flag check

          ELSE
              ! SurfaceWindow(HeatTransSurfNum)%WindowModelType == WindowEQLModel
              ! ConstrNum=Surface(HeatTransSurfNum)%Construction
              ! call the ASHWAT fenestration model for diffuse radiation here
              CALL CalcEQLOpticalProperty(HeatTransSurfNum, isDIFF, AbsSolDiffBackEQL)

              EQLNum = Construct(ConstrNum)%EQLConsPtr
              DO Lay = 1, CFS(EQLNum)%NL


                  ! Calc diffuse solar absorbed from the inside by each layer of EQL model [W]
                  !WinDifSolLayAbsW = WinDifSolar(DifTransSurfNum)* ViewFactor * Construct(ConstrNum)%AbsDiffBack(Lay)
                  WinDifSolLayAbsW = WinDifSolar(DifTransSurfNum)* ViewFactor * AbsSolDiffBackEQL(Lay,2)

                  ! Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
                  DifSolarAbsW = DifSolarAbsW + WinDifSolLayAbsW

                  ! Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
                  WinDifSolarDistAbsorbedTotl = WinDifSolarDistAbsorbedTotl + WinDifSolLayAbsW  ! debug
                  ZoneDifSolarDistAbsorbedTotl = ZoneDifSolarDistAbsorbedTotl + WinDifSolLayAbsW  ! debug

                  ! Accumulate diffuse solar absorbed from the inside by each window layer [W/m2] for heat balance calcs
                  InitialDifSolwinAbs(HeatTransSurfNum,Lay) = InitialDifSolwinAbs(HeatTransSurfNum,Lay)  &
                                                            + (WinDifSolLayAbsW  &
                                                            / Surface(HeatTransSurfNum)%Area)

                  ! ASHWAT equivalent layer model may require not the individual layer absorption but the flux
                  ! InitialDifSolwinEQL(HeatTransSurfNum) = WinDifSolar(DifTransSurfNum)* ViewFactor

              END DO

              ! Calc diffuse solar reflected back to zone
              ! I don't really care if this is a window or opaque surface since I am just
              ! accumulating all reflected diffuse solar in a zone bucket for "interreflected" distribution
              ! Reflected diffuse solar [W] = current window transmitted diffuse solar
              !    * view factor from current (sending) window DifTransSurfNum to current (receiving) surface HeatTransSurfNum
              !    * current window inside solar reflectance
              InsideDifReflectance = Construct(ConstrNum)%ReflectSolDiffBack
              DifSolarReflW = WinDifSolar(DifTransSurfNum) &
                            * ViewFactor &
                            * Construct(ConstrNum)%ReflectSolDiffBack

              ! Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
              InitialZoneDifSolReflW(ZoneNum) = InitialZoneDifSolReflW(ZoneNum) &
                                              + DifSolarReflW ! [W]

              ! Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
              WinDifSolarDistReflectedTotl = WinDifSolarDistReflectedTotl + DifSolarReflW ! debug
              ZoneDifSolarDistReflectedTotl = ZoneDifSolarDistReflectedTotl + DifSolarReflW ! debug

                !------------------------------------------------------------------------------
                ! DISTRIBUTE TRANSMITTED DIFFUSE SOLAR THROUGH INTERIOR WINDOW TO ADJACENT ZONE
                !------------------------------------------------------------------------------

                ! If this receiving window surface (HeatTransSurfNum) is an interior window,
                ! calc distributed solar transmitted to adjacent zone [W]
                ! NOTE: This calc is here because interior windows are currently assumed to have no shading

                ! Get the adjacent surface number for this receiving window surface
              AdjSurfNum = Surface(HeatTransSurfNum)%ExtBoundCond
                ! If the adjacent surface number is > 0, this is an interior window
              IF(AdjSurfNum > 0) THEN ! this is an interior window surface

                  ! Calc diffuse solar from current exterior window
                  ! transmitted through this interior window to adjacent zone [W]
                  ! Transmitted diffuse solar [W] = current exterior window transmitted diffuse solar
                  !    * view factor from current (sending) window DifTransSurfNum to current (receiving) surface HeatTransSurfNum
                  DifSolarTransW = AbsSolDiffBackEQL(CFS(EQLNum)%NL+1,2) * ViewFactor
                                   AdjConstrNum=Surface(AdjSurfNum)%Construction
                  ! Get the adjacent zone index
                  AdjZoneNum = Surface(AdjSurfNum)%Zone
                  ! Call routine to distribute diffuse solar transmitted through this interior window into adjacent zone
                  CALL CalcInteriorWinTransDifSolInitialDistribution(AdjZoneNum, AdjSurfNum, DifSolarTransW)

              ELSE ! this is an exterior window surface

                    ! Calc transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
                    ! This is not very effective since it assigns whatever distributed diffuse solar has not been
                    ! absorbed or reflected to transmitted.
                    DifSolarTransW = AbsSolDiffBackEQL(CFS(EQLNum)%NL+1,2) * ViewFactor

              END IF ! this is an interior window surface

              ! Accumulate transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
              WinDifSolarDistTransmittedTotl = WinDifSolarDistTransmittedTotl + DifSolarTransW  ! debug [W]
              ZoneDifSolarDistTransmittedTotl = ZoneDifSolarDistTransmittedTotl + DifSolarTransW ! debug [W]
              ! Accumulate transmitted diffuse solar for reporting
              InitialDifSolInTrans(HeatTransSurfNum) = InitialDifSolInTrans(HeatTransSurfNum) &
                                                     + (DifSolarTransW  &
                                                     / Surface(HeatTransSurfNum)%Area)

          END IF      !IF (SurfaceWindow(HeatTransSurfNum)%WindowModelType /= WindowEQLModel) THEN

! HERE 8/14/07 Ignore absorptance and reflectance of Frames and Dividers for now.
! I would need revised view factors that included these surface types.
! By ignoring them here, the diffuse solar is accounted for on the other surfaces

!          IF(SurfaceWindow(HeatTransSurfNum)%FrameArea > 0.0) THEN  ! Window has a frame
            ! Note that FrameQRadInAbs is initially calculated in InitSolarHeatGains
!          END IF

!          IF(SurfaceWindow(HeatTransSurfNum)%DividerArea > 0.0) THEN  ! Window has dividers
!            DividerSolAbs = SurfaceWindow(HeatTransSurfNum)%DividerSolAbsorp
!            IF(SurfaceWindow(HeatTransSurfNum)%DividerType == Suspended) THEN ! Suspended divider; account for inside glass
!              MatNumGl = Construct(ConstrNum)%LayerPoint(Construct(ConstrNum)%TotLayers)
!              TransGl = Material(MatNumGl)%Trans
!              ReflGl = Material(MatNumGl)%ReflectSolDiffBack
!              AbsGl = 1.-TransGl-ReflGl
!              DividerSolRefl = 1.-DividerSolAbs
!              DividerSolAbs = AbsGl + TransGl*(DividerSolAbs + DividerSolRefl*AbsGl)/(1.-DividerSolRefl*ReflGl)
!            END IF
            ! Correct for interior shade transmittance
!            IF(ShadeFlag == IntShadeOn) THEN
!              MatNumSh = Construct(ConstrNumSh)%LayerPoint(Construct(ConstrNumSh)%TotLayers)
!              DividerSolAbs = DividerSolAbs * Material(MatNumSh)%Trans
!            ELSE IF(ShadeFlag == IntBlindOn) THEN
!              DividerSolAbs = DividerSolAbs * InterpSlatAng(SurfaceWindow(HeatTransSurfNum)%SlatAngThisTS, &
!                  SurfaceWindow(HeatTransSurfNum)%MovableSlats,Blind(BlNum)%SolBackDiffDiffTrans)
!            END IF
            ! Note that DividerQRadInAbs is initially calculated in InitSolarHeatGains

!          END IF  ! Window has dividers

        END IF  ! opaque or window heat transfer surface

      END DO  ! HeatTransSurfNum = Zone(ZoneNum)%SurfaceFirst, Zone(ZoneNum)%SurfaceLast

      ! Check debug var for view factors here
      ! ViewFactorTotal
      ! Check debug vars for individual transmitting surfaces here
      WinDifSolarDistTotl = WinDifSolarDistAbsorbedTotl + WinDifSolarDistReflectedTotl + WinDifSolarDistTransmittedTotl
      ! WinDifSolarTrans

    END DO  ! DifTransSurfNum = Zone(ZoneNum)%SurfaceFirst, Zone(ZoneNum)%SurfaceLast

    ! Check debug vars for zone totals here
    ZoneDifSolarDistTotl = ZoneDifSolarDistAbsorbedTotl + ZoneDifSolarDistReflectedTotl + ZoneDifSolarDistTransmittedTotl
    ! ZoneDifSolarTrans
    ! ZoneDifSolarDistAbsorbedTotl
    ! ZoneDifSolarDistReflectedTotl
    ! ZoneDifSolarDistTransmittedTotl
!    CALL DisplayString('Diffuse Solar Distribution Zone Totals')

  END DO  ! ZoneNum = 1, NumOfZones

  RETURN

END SUBROUTINE CalcWinTransDifSolInitialDistribution


SUBROUTINE CalcInteriorWinTransDifSolInitialDistribution(ZoneNum, IntWinSurfNum, IntWinDifSolarTransW)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rob Hitchcock
          !       DATE WRITTEN   August 2007
          !       MODIFIED       N/A
          !       RE-ENGINEERED  N/A

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the initial distribution
          ! of diffuse solar transmitted through the given interior window
          ! to individual heat transfer surfaces in the given zone.
          ! Diffuse solar transmitted through interior windows in this zone
          ! to adjacent zones, is added to the InitialZoneDifSolReflW
          ! of the adjacent zone for subsequent interreflection calcs

          ! METHODOLOGY EMPLOYED:
          ! Similar to method used in CalcWinTransDifSolInitialDistribution.
          ! Apportions diffuse solar transmitted through an interior window
          ! that is then absorbed, reflected, and/or transmitted
          ! by other heat transfer surfaces in the given zone.
          ! Calculations use:
          ! 1. DifSolarTransW calculated in SUBROUTINE CalcWinTransDifSolInitialDistribution,
          ! 2. view factors between the interior window and
          ! other heat transfer surfaces in the given zone
          ! calculated in SUBROUTINE CalcApproximateViewFactors, and
          ! 3. surface absorptances, reflectances, and transmittances
          ! determined here using revised code from SUBROUTINE InitIntSolarDistribution

          ! REFERENCES:

          ! USE STATEMENTS:
  USE General, ONLY: InterpSw, InterpSlatAng
  USE ScheduleManager, ONLY: GetCurrentScheduleValue
  USE DataViewFactorInformation

  USE DataHeatBalSurface, ONLY: InitialDifSolInAbs, InitialDifSolInTrans
  USE DataHeatBalance, ONLY: InitialDifSolwinAbs, InitialZoneDifSolReflW

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER :: ZoneNum              ! Zone index number
  INTEGER :: IntWinSurfNum        ! Interior Window Surface number in Zone ZoneNum
  REAL(r64)    :: IntWinDifSolarTransW ! Diffuse Solar transmitted through Interior Window IntWinSurfNum from adjacent zone [W]

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: AdjZoneNum         ! Index for adjacent zones
  INTEGER :: AdjSurfNum         ! Index for adjacent surfaces
  INTEGER :: HeatTransSurfNum   ! Heat Transfer Surface number
  INTEGER :: ConstrNum          ! Construction number
!  INTEGER :: AdjConstrNum       ! Construction number of other side surface
  INTEGER :: ConstrNumSh        ! Shaded construction number
  INTEGER :: IGlass             ! Glass layer counter
  INTEGER :: TotGlassLayers     ! Number of glass layers in a window construction
  INTEGER :: ShadeFlag          ! Shading flag
  REAL(r64)    :: AbsInt             ! Tmp var for Inside surface short-wave absorptance
  REAL(r64)    :: MovInsulSchedVal   ! Value of the movable insulation schedule for current time
  REAL(r64)    :: HMovInsul          ! Conductance of movable insulation
  REAL(r64)    :: InsideDifAbsorptance ! Inside diffuse solar absorptance of a surface
  REAL(r64)    :: InsideDifReflectance ! Inside diffuse solar reflectance of a surface
  INTEGER :: BlNum              ! Blind number
  REAL(r64)    :: BlAbsDiffBk        ! Glass layer back diffuse solar absorptance when blind in place
  REAL(r64)    :: AbsDiffBkBl        ! Blind diffuse back solar absorptance as part of glazing system

!  REAL(r64)    :: DividerSolAbs      ! Window divider solar absorptance
!  REAL(r64)    :: DividerSolRefl     ! Window divider solar reflectance
!  INTEGER :: MatNumGl           ! Glass layer material number
!  INTEGER :: MatNumSh           ! Shade layer material number
!  REAL(r64)    :: TransGl,ReflGl,AbsGl ! Glass layer solar transmittance, reflectance, absorptance

  REAL(r64)    :: ViewFactor         ! temp var for view factor
  REAL(r64)    :: ViewFactorTotal    ! debug var for view factor total
  REAL(r64)    :: WinDifSolarTrans   ! debug var for WinDifSolar() [W]
  REAL(r64)    :: WinDifSolarDistTotl    ! debug var for window total distributed diffuse solar [W]
  REAL(r64)    :: WinDifSolarDistAbsorbedTotl    ! debug var for individual exterior window total distributed
                                            !           diffuse solar absorbed [W]
  REAL(r64)    :: WinDifSolarDistReflectedTotl   ! debug var for individual exterior window total distributed
                                            !           diffuse solar reflected [W]
  REAL(r64)    :: WinDifSolarDistTransmittedTotl ! debug var for individual exterior window total distributed
                                            !           diffuse solar transmitted [W]
  REAL(r64)    :: WinDifSolLayAbsW   ! temp var for diffuse solar absorbed by individual glass layer [W]
  REAL(r64)    :: ZoneDifSolarTrans   ! debug var for WinDifSolar() [W]
!  REAL(r64)    :: ZoneDifSolarDistTotl    ! debug var for zone total distributed diffuse solar [W]
  REAL(r64)    :: ZoneDifSolarDistAbsorbedTotl    ! debug var for zone total distributed diffuse solar absorbed [W]
  REAL(r64)    :: ZoneDifSolarDistReflectedTotl   ! debug var for zone total distributed diffuse solar reflected [W]
  REAL(r64)    :: ZoneDifSolarDistTransmittedTotl ! debug var for zone total distributed diffuse solar transmitted [W]

  REAL(r64)    :: DifSolarAbsW       ! temp var for diffuse solar absorbed by surface [W]
  REAL(r64)    :: DifSolarAbs        ! temp var for diffuse solar absorbed by surface [W/m2]
  REAL(r64)    :: DifSolarReflW      ! temp var for diffuse solar reflected by surface [W]
  REAL(r64)    :: DifSolarTransW     ! temp var for diffuse solar transmitted through interior window surface [W]
  REAL(r64)    :: ShBlDifSolarAbsW   ! temp var for diffuse solar absorbed by shade/blind [W]

  INTEGER :: FirstZoneSurf      ! conversion index for ViewFactor
  INTEGER :: LastZoneSurf       ! debug

  !-------------------------------------------------------------------------------------------------
  ! DISTRIBUTE TRANSMITTED DIFFUSE SOLAR THROUGH INTERIOR WINDOW TO INTERIOR HEAT TRANSFER SURFACES
  !-------------------------------------------------------------------------------------------------

  ! Init debug vars
  ViewFactorTotal = 0.0d0
  WinDifSolarTrans = IntWinDifSolarTransW

  ! Init Interior Window accumulators for debugging
  WinDifSolarDistAbsorbedTotl = 0.0d0
  WinDifSolarDistReflectedTotl = 0.0d0
  WinDifSolarDistTransmittedTotl = 0.0d0

  ! Init first and last surfnums for this zone
  FirstZoneSurf = Zone(ZoneNum)%SurfaceFirst
  LastZoneSurf = Zone(ZoneNum)%SurfaceLast

  ZoneDifSolarTrans = 0.0d0
  ZoneDifSolarDistAbsorbedTotl = 0.0d0
  ZoneDifSolarDistReflectedTotl = 0.0d0
  ZoneDifSolarDistTransmittedTotl = 0.0d0

  ! Loop over all heat transfer surfaces in the current zone that might receive diffuse solar
  DO HeatTransSurfNum = Zone(ZoneNum)%SurfaceFirst, Zone(ZoneNum)%SurfaceLast
    ! Skip surfaces that are not heat transfer surfaces
    IF (.NOT.Surface(HeatTransSurfNum)%HeatTransSurf) CYCLE
    ! Skip tubular daylighting device domes
    IF (Surface(HeatTransSurfNum)%Class == SurfaceClass_TDD_Dome) CYCLE

    ! View factor from current (sending) window IntWinSurfNum to current (receiving) surface HeatTransSurfNum
    ViewFactor = ZoneInfo(ZoneNum)%F(IntWinSurfNum-FirstZoneSurf+1, HeatTransSurfNum-FirstZoneSurf+1)
    ! debug ViewFactorTotal
    ViewFactorTotal = ViewFactorTotal + ViewFactor  ! debug

    ! Skip receiving surfaces with 0.0 view factor
    IF (ViewFactor <= 0.0d0) CYCLE

    ! Calculate diffuse solar from current interior window absorbed and reflected by current heat transfer surface
    ! And calculate transmitted diffuse solar to adjacent zones through interior windows
    ConstrNum=Surface(HeatTransSurfNum)%Construction
    IF (Construct(ConstrNum)%TransDiff <= 0.0d0) THEN  ! Interior Opaque Surface

      ! Determine the inside (back) diffuse solar absorptance
      ! and reflectance of the current heat transfer surface
      InsideDifAbsorptance = Construct(ConstrNum)%InsideAbsorpSolar
      ! Check for movable insulation; reproduce code from subr. EvalInsideMovableInsulation;
      ! Can't call that routine here since cycle prevents SolarShadingGeometry from USEing
      ! HeatBalanceSurfaceManager, which contains EvalInsideMovableInsulation
      HMovInsul = 0.0d0
      IF (Surface(HeatTransSurfNum)%MaterialMovInsulInt.GT.0) THEN
        MovInsulSchedVal = GetCurrentScheduleValue(Surface(HeatTransSurfNum)%SchedMovInsulExt)
        IF (MovInsulSchedVal.LE.0.0d0) THEN ! Movable insulation not present at current time
          HMovInsul = 0.0d0
        ELSE  ! Movable insulation present
          HMovInsul = 1.0d0/(MovInsulSchedVal*Material(Surface(HeatTransSurfNum)%MaterialMovInsulInt)%Resistance)
          AbsInt    = Material(Surface(HeatTransSurfNum)%MaterialMovInsulInt)%AbsorpSolar
        END IF
      END IF
      IF (HMovInsul > 0.0d0) InsideDifAbsorptance = AbsInt  ! Movable inside insulation present
      ! Inside (back) diffuse solar reflectance is assumed to be 1 - absorptance
      InsideDifReflectance = 1.0d0 - InsideDifAbsorptance

      ! Absorbed diffuse solar [W] = current window transmitted diffuse solar [W]
      !    * view factor from current (sending) window IntWinSurfNum to current (receiving) surface HeatTransSurfNum
      !    * current surface inside solar absorptance
      DifSolarAbsW = IntWinDifSolarTransW & ! [W]
                      * ViewFactor &
                      * InsideDifAbsorptance

      ! Absorbed diffuse solar [W/m2] = Absorbed diffuse solar [W]
      !                                 / current surface net area
      DifSolarAbs = DifSolarAbsW &
                     / Surface(HeatTransSurfNum)%Area

      ! Accumulate absorbed diffuse solar [W/m2] on this surface for heat balance calcs
      InitialDifSolInAbs(HeatTransSurfNum) = InitialDifSolInAbs(HeatTransSurfNum) &
                                              + DifSolarAbs

      ! Reflected diffuse solar [W] = current window transmitted diffuse solar
      !    * view factor from current (sending) window IntWinSurfNum to current (receiving) surface HeatTransSurfNum
      !    * current window inside solar reflectance
      DifSolarReflW = IntWinDifSolarTransW &
                      * ViewFactor &
                      * InsideDifReflectance

      ! Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
      InitialZoneDifSolReflW(ZoneNum) = InitialZoneDifSolReflW(ZoneNum) &
                                    + DifSolarReflW ! [W]

      ! Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
      ! For opaque surfaces all incident diffuse is either absorbed or reflected
      WinDifSolarDistAbsorbedTotl = WinDifSolarDistAbsorbedTotl + DifSolarAbsW  ! debug [W]
      WinDifSolarDistReflectedTotl = WinDifSolarDistReflectedTotl + DifSolarReflW ! debug [W]
      ZoneDifSolarDistAbsorbedTotl = ZoneDifSolarDistAbsorbedTotl + DifSolarAbsW  ! debug [W]
      ZoneDifSolarDistReflectedTotl = ZoneDifSolarDistReflectedTotl + DifSolarReflW ! debug [W]

    ELSE  ! Exterior or Interior Window

      ConstrNumSh = Surface(HeatTransSurfNum)%ShadedConstruction
      IF(SurfaceWindow(HeatTransSurfNum)%StormWinFlag==1) THEN
        ConstrNum   = Surface(HeatTransSurfNum)%StormWinConstruction
        ConstrNumSh = Surface(HeatTransSurfNum)%StormWinShadedConstruction
      END IF
      TotGlassLayers = Construct(ConstrNum)%TotGlassLayers
      ShadeFlag = SurfaceWindow(HeatTransSurfNum)%ShadingFlag

      IF(ShadeFlag <= 0) THEN  ! No window shading
        ! Init accumulator for transmittance calc below
        DifSolarAbsW = 0.0d0

        ! Calc diffuse solar absorbed by all window glass layers
        ! Note: I am assuming here that individual glass layer absorptances have been corrected
        !       to account for layer by layer transmittance and reflection effects.
        DO IGlass = 1, TotGlassLayers
          ! Calc diffuse solar absorbed from the inside by each window glass layer [W]
          AbsInt = Construct(ConstrNum)%AbsDiffBack(IGlass)
          WinDifSolLayAbsW = IntWinDifSolarTransW* ViewFactor * Construct(ConstrNum)%AbsDiffBack(IGlass)

          ! Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
          DifSolarAbsW = DifSolarAbsW + WinDifSolLayAbsW

          ! Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
          WinDifSolarDistAbsorbedTotl = WinDifSolarDistAbsorbedTotl + WinDifSolLayAbsW  ! debug
          ZoneDifSolarDistAbsorbedTotl = ZoneDifSolarDistAbsorbedTotl + WinDifSolLayAbsW  ! debug

          ! Accumulate diffuse solar absorbed from the inside by each window glass layer [W/m2] for heat balance calcs
          InitialDifSolwinAbs(HeatTransSurfNum,IGlass) = InitialDifSolwinAbs(HeatTransSurfNum,IGlass)  &
                                                           + (WinDifSolLayAbsW  &
                                                           / Surface(HeatTransSurfNum)%Area)
        END DO

        ! Calc diffuse solar reflected back to zone
        ! I don't really care if this is a window or opaque surface since I am just
        ! accumulating all reflected diffuse solar in a zone bucket for "interreflected" distribution
        ! Reflected diffuse solar [W] = current window transmitted diffuse solar
        !    * view factor from current (sending) window IntWinSurfNum to current (receiving) surface HeatTransSurfNum
        !    * current window inside solar reflectance
        DifSolarReflW = IntWinDifSolarTransW &
                        * ViewFactor &
                        * Construct(ConstrNum)%ReflectSolDiffBack

        ! Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
        InitialZoneDifSolReflW(ZoneNum) = InitialZoneDifSolReflW(ZoneNum) &
                                      + DifSolarReflW ! [W]

        ! Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
        WinDifSolarDistReflectedTotl = WinDifSolarDistReflectedTotl + DifSolarReflW ! debug
        ZoneDifSolarDistReflectedTotl = ZoneDifSolarDistReflectedTotl + DifSolarReflW ! debug

        ! Calc transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
        ! This is not very effective since it assigns whatever distributed diffuse solar has not been
        ! absorbed or reflected to transmitted.
        DifSolarTransW = IntWinDifSolarTransW &
                        * ViewFactor &
                        - DifSolarAbsW &
                        - DifSolarReflW

        ! Accumulate transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
        WinDifSolarDistTransmittedTotl = WinDifSolarDistTransmittedTotl + DifSolarTransW  ! debug [W]
        ZoneDifSolarDistTransmittedTotl = ZoneDifSolarDistTransmittedTotl + DifSolarTransW ! debug [W]

        ! Accumulate transmitted diffuse solar for reporting
        InitialDifSolInTrans(HeatTransSurfNum) = InitialDifSolInTrans(HeatTransSurfNum) &
                                                 + (DifSolarTransW  &
                                                 / Surface(HeatTransSurfNum)%Area)

        !-----------------------------------------------------------------------------------
        ! ADD TRANSMITTED DIFFUSE SOLAR THROUGH INTERIOR WINDOW TO ADJACENT ZONE
        ! TOTAL REFLECTED DIFFUSE SOLAR FOR SUBSEQUENT INTERREFLECTION CALCS
        !-----------------------------------------------------------------------------------

        ! If this receiving window surface (HeatTransSurfNum) is an interior window,
        ! add transmitted diffuse solar to adjacent zone total reflected distributed
        ! diffuse solar for subsequent interreflection calcs
        ! NOTE: This calc is here because interior windows are currently assumed to have no shading

        ! Get the adjacent surface number for this receiving window surface
        AdjSurfNum = Surface(HeatTransSurfNum)%ExtBoundCond
        ! If the adjacent surface number is > 0, this is an interior window
        IF(AdjSurfNum > 0) THEN ! this is an interior window surface

          ! Get the adjacent zone index
          AdjZoneNum = Surface(AdjSurfNum)%Zone

          ! Add transmitted diffuse solar to total reflected distributed diffuse solar for each zone
          ! for subsequent interreflection calcs
          InitialZoneDifSolReflW(AdjZoneNum) = InitialZoneDifSolReflW(AdjZoneNum) &
                                           + DifSolarTransW ! [W]

        END IF

      ELSE IF(ShadeFlag == IntShadeOn .OR. ShadeFlag >= 3) THEN
            ! Interior, exterior or between-glass shade, screen or blind in place

        ! Init accumulator for transmittance calc below
        DifSolarAbsW = 0.0d0
        WinDifSolLayAbsW = 0.0d0

        ! First calc diffuse solar absorbed by each glass layer in this window with shade/blind in place
        DO IGlass = 1,Construct(ConstrNumSh)%TotGlassLayers
          IF(ShadeFlag==IntShadeOn .OR. ShadeFlag==ExtShadeOn .OR. ShadeFlag==BGShadeOn .OR. ShadeFlag==ExtScreenOn) THEN
            ! Calc diffuse solar absorbed in each window glass layer and shade
            WinDifSolLayAbsW = IntWinDifSolarTransW * ViewFactor * Construct(ConstrNumSh)%AbsDiffBack(IGlass)
          END IF

          IF(ShadeFlag == IntBlindOn .OR. ShadeFlag == ExtBlindOn .or. ShadeFlag == BGBlindOn) THEN
            BlAbsDiffBk = InterpSlatAng(SurfaceWindow(HeatTransSurfNum)%SlatAngThisTS,  &
                                 SurfaceWindow(HeatTransSurfNum)%MovableSlats, &
                                 Construct(ConstrNumSh)%BlAbsDiffBack(IGlass,1:MaxSlatAngs))
            ! Calc diffuse solar absorbed in each window glass layer and shade
            WinDifSolLayAbsW = IntWinDifSolarTransW * ViewFactor * BlAbsDiffBk
          END IF

          ! Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
          DifSolarAbsW = DifSolarAbsW + WinDifSolLayAbsW

          ! Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
          WinDifSolarDistAbsorbedTotl = WinDifSolarDistAbsorbedTotl + WinDifSolLayAbsW  ! debug
          ZoneDifSolarDistAbsorbedTotl = ZoneDifSolarDistAbsorbedTotl + WinDifSolLayAbsW  ! debug

          ! Accumulate diffuse solar absorbed from the inside by each window glass layer [W/m2] for heat balance calcs
          InitialDifSolwinAbs(HeatTransSurfNum,IGlass) = InitialDifSolwinAbs(HeatTransSurfNum,IGlass)  &
                                                           + (WinDifSolLayAbsW  &
                                                           / Surface(HeatTransSurfNum)%Area)
        END DO

        ! Next calc diffuse solar reflected back to zone from window with shade or blind on
        ! Diffuse back solar reflectance, bare glass or shade on
        InsideDifReflectance = Construct(ConstrNum)%ReflectSolDiffBack
        IF(ShadeFlag == IntBlindOn .OR. ShadeFlag == ExtBlindOn) THEN
          ! Diffuse back solar reflectance, blind present, vs. slat angle
          InsideDifReflectance = InterpSlatAng(SurfaceWindow(HeatTransSurfNum)%SlatAngThisTS, &
                                  SurfaceWindow(HeatTransSurfNum)%MovableSlats, &
                                  Construct(ConstrNum)%BlReflectSolDiffBack)
        END IF
        DifSolarReflW = IntWinDifSolarTransW &
                        * ViewFactor &
                        * InsideDifReflectance

        ! Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
        InitialZoneDifSolReflW(ZoneNum) = InitialZoneDifSolReflW(ZoneNum) &
                                      + DifSolarReflW ! [W]

        ! Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
        WinDifSolarDistReflectedTotl = WinDifSolarDistReflectedTotl + DifSolarReflW ! debug
        ZoneDifSolarDistReflectedTotl = ZoneDifSolarDistReflectedTotl + DifSolarReflW ! debug

        ! Now calc diffuse solar absorbed by shade/blind itself
        BlNum = SurfaceWindow(HeatTransSurfNum)%BlindNumber
        IF(ShadeFlag==IntShadeOn .OR. ShadeFlag==ExtShadeOn .OR. ShadeFlag==BGShadeOn .OR. ShadeFlag==ExtScreenOn) THEN
          ! Calc diffuse solar absorbed by shade or screen [W]
          ShBlDifSolarAbsW = IntWinDifSolarTransW * ViewFactor * Construct(ConstrNumSh)%AbsDiffBackShade
        END IF
        IF(ShadeFlag==IntBlindOn .OR. ShadeFlag==ExtBlindOn .OR. ShadeFlag==BGBlindOn) THEN
          ! Calc diffuse solar absorbed by blind [W]
          AbsDiffBkBl = InterpSlatAng(SurfaceWindow(HeatTransSurfNum)%SlatAngThisTS,SurfaceWindow(HeatTransSurfNum)%MovableSlats, &
                                 Construct(ConstrNumSh)%AbsDiffBackBlind)
          ShBlDifSolarAbsW = IntWinDifSolarTransW * ViewFactor * AbsDiffBkBl
        END IF
        ! Correct for divider shadowing
        IF(ShadeFlag == ExtShadeOn.OR.ShadeFlag == ExtBlindOn.OR.ShadeFlag == ExtScreenOn) &
          ShBlDifSolarAbsW = ShBlDifSolarAbsW * SurfaceWindow(HeatTransSurfNum)%GlazedFrac

        ! Accumulate diffuse solar absorbed  by shade or screen [W/m2] for heat balance calcs
        SurfaceWindow(HeatTransSurfNum)%InitialDifSolAbsByShade = SurfaceWindow(HeatTransSurfNum)%InitialDifSolAbsByShade &
                                                            + (ShBlDifSolarAbsW / Surface(HeatTransSurfNum)%Area)

        ! Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
        DifSolarAbsW = DifSolarAbsW + ShBlDifSolarAbsW

        ! Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
        WinDifSolarDistAbsorbedTotl = WinDifSolarDistAbsorbedTotl + ShBlDifSolarAbsW  ! debug
        ZoneDifSolarDistAbsorbedTotl = ZoneDifSolarDistAbsorbedTotl + ShBlDifSolarAbsW  ! debug

        ! Accumulate transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
        ! This is not very effective since it assigns whatever distributed diffuse solar has not been
        ! absorbed or reflected to transmitted.
        DifSolarTransW = IntWinDifSolarTransW &
                        * ViewFactor &
                        - DifSolarAbsW &
                        - DifSolarReflW
        WinDifSolarDistTransmittedTotl = WinDifSolarDistTransmittedTotl + DifSolarTransW  ! debug [W]
        ZoneDifSolarDistTransmittedTotl = ZoneDifSolarDistTransmittedTotl + DifSolarTransW ! debug [W]

        ! Accumulate transmitted diffuse solar for reporting
        InitialDifSolInTrans(HeatTransSurfNum) = InitialDifSolInTrans(HeatTransSurfNum) &
                                                 + (DifSolarTransW  &
                                                 / Surface(HeatTransSurfNum)%Area)

      ELSE IF(ShadeFlag == SwitchableGlazing) THEN  ! Switchable glazing
        ! Init accumulator for transmittance calc below
        DifSolarAbsW = 0.0d0

        DO IGlass = 1,TotGlassLayers
          ! Calc diffuse solar absorbed in each window glass layer
          WinDifSolLayAbsW = IntWinDifSolarTransW * ViewFactor &
                              * InterpSw(SurfaceWindow(HeatTransSurfNum)%SwitchingFactor,  &
                                Construct(ConstrNum)%AbsDiffBack(IGlass),  &
                                Construct(ConstrNumSh)%AbsDiffBack(IGlass))

          ! Accumulate distributed diffuse solar absorbed [W] by overall window for transmittance calc below
          DifSolarAbsW = DifSolarAbsW + WinDifSolLayAbsW

          ! Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
          WinDifSolarDistAbsorbedTotl = WinDifSolarDistAbsorbedTotl + WinDifSolLayAbsW  ! debug
          ZoneDifSolarDistAbsorbedTotl = ZoneDifSolarDistAbsorbedTotl + WinDifSolLayAbsW  ! debug

          ! Accumulate diffuse solar absorbed from the inside by each window glass layer [W/m2] for heat balance calcs
          InitialDifSolwinAbs(HeatTransSurfNum,IGlass) = InitialDifSolwinAbs(HeatTransSurfNum,IGlass)  &
                                                           + (WinDifSolLayAbsW  &
                                                           / Surface(HeatTransSurfNum)%Area)

        END DO

        ! Calc diffuse solar reflected back to zone
        DifSolarReflW = IntWinDifSolarTransW &
                        * ViewFactor &
                        * InterpSw(SurfaceWindow(HeatTransSurfNum)%SwitchingFactor,  &
                          Construct(ConstrNum)%ReflectSolDiffBack,  &
                          Construct(ConstrNumSh)%ReflectSolDiffBack)

        ! Accumulate total reflected distributed diffuse solar for each zone for subsequent interreflection calcs
        InitialZoneDifSolReflW(ZoneNum) = InitialZoneDifSolReflW(ZoneNum) &
                                      + DifSolarReflW ! [W]

        ! Accumulate Window and Zone total distributed diffuse solar to check for conservation of energy
        WinDifSolarDistReflectedTotl = WinDifSolarDistReflectedTotl + DifSolarReflW ! debug
        ZoneDifSolarDistReflectedTotl = ZoneDifSolarDistReflectedTotl + DifSolarReflW ! debug

        ! Accumulate transmitted Window and Zone total distributed diffuse solar to check for conservation of energy
        ! This is not very effective since it assigns whatever distributed diffuse solar has not been
        ! absorbed or reflected to transmitted.
        DifSolarTransW = IntWinDifSolarTransW &
                        * ViewFactor &
                        - DifSolarAbsW &
                        - DifSolarReflW
        WinDifSolarDistTransmittedTotl = WinDifSolarDistTransmittedTotl + DifSolarTransW  ! debug [W]
        ZoneDifSolarDistTransmittedTotl = ZoneDifSolarDistTransmittedTotl + DifSolarTransW ! debug [W]

        ! Accumulate transmitted diffuse solar for reporting
        InitialDifSolInTrans(HeatTransSurfNum) = InitialDifSolInTrans(HeatTransSurfNum) &
                                                 + (DifSolarTransW  &
                                                 / Surface(HeatTransSurfNum)%Area)

      END IF  ! End of shading flag check

! HERE 8/14/07 Ignore absorptance and reflectance of Frames and Dividers for now.
! I would need revised view factors that included these surface types.
! By ignoring them here, the diffuse solar is accounted for on the other surfaces

!          IF(SurfaceWindow(HeatTransSurfNum)%FrameArea > 0.0) THEN  ! Window has a frame
            ! Note that FrameQRadInAbs is initially calculated in InitSolarHeatGains
!          END IF

!          IF(SurfaceWindow(HeatTransSurfNum)%DividerArea > 0.0) THEN  ! Window has dividers
!            DividerSolAbs = SurfaceWindow(HeatTransSurfNum)%DividerSolAbsorp
!            IF(SurfaceWindow(HeatTransSurfNum)%DividerType == Suspended) THEN ! Suspended divider; account for inside glass
!              MatNumGl = Construct(ConstrNum)%LayerPoint(Construct(ConstrNum)%TotLayers)
!              TransGl = Material(MatNumGl)%Trans
!              ReflGl = Material(MatNumGl)%ReflectSolDiffBack
!              AbsGl = 1.-TransGl-ReflGl
!              DividerSolRefl = 1.-DividerSolAbs
!              DividerSolAbs = AbsGl + TransGl*(DividerSolAbs + DividerSolRefl*AbsGl)/(1.-DividerSolRefl*ReflGl)
!            END IF
            ! Correct for interior shade transmittance
!            IF(ShadeFlag == IntShadeOn) THEN
!              MatNumSh = Construct(ConstrNumSh)%LayerPoint(Construct(ConstrNumSh)%TotLayers)
!              DividerSolAbs = DividerSolAbs * Material(MatNumSh)%Trans
!            ELSE IF(ShadeFlag == IntBlindOn) THEN
!              DividerSolAbs = DividerSolAbs * InterpSlatAng(SurfaceWindow(HeatTransSurfNum)%SlatAngThisTS, &
!                  SurfaceWindow(HeatTransSurfNum)%MovableSlats,Blind(BlNum)%SolBackDiffDiffTrans)
!            END IF
            ! Note that DividerQRadInAbs is initially calculated in InitSolarHeatGains

!          END IF  ! Window has dividers
    END IF  ! opaque or window heat transfer surface

  END DO  ! HeatTransSurfNum = Zone(ZoneNum)%SurfaceFirst, Zone(ZoneNum)%SurfaceLast

  ! Check debug var for view factors here
  ! ViewFactorTotal
  ! Check debug vars for individual transmitting surfaces here
  WinDifSolarDistTotl = WinDifSolarDistAbsorbedTotl + WinDifSolarDistReflectedTotl + WinDifSolarDistTransmittedTotl
  ! WinDifSolarTrans

  RETURN

END SUBROUTINE CalcInteriorWinTransDifSolInitialDistribution

subroutine CalcComplexWindowOverlap(Geom, Window, ISurf)
  ! SUBROUTINE INFORMATION:
  !       AUTHOR         Simon Vidanovic
  !       DATE WRITTEN   May 2012
  !       MODIFIED       Simon Vidanovic (May 2013) - added overlaps calculations for daylighting
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS SUBROUTINE:
  ! For each of basis directions on back surface of the window calculates
  ! overlap areas. It also calculates overlap areas and reflectances for daylighting calculations

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES:
  ! na

  ! USE STATEMENTS:
  USE vectors
  USE DataHeatBalance, ONLY : Material

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

  ! SUBROUTINE PARAMETER DEFINITIONS:

  INTEGER, INTENT(IN)      ::  ISurf    !Surface number of the complex fenestration
  TYPE (BSDFWindowGeomDescr),INTENT(IN)    ::  Window  !Window Geometry
  TYPE (BSDFGeomDescr), INTENT(INOUT)  ::  Geom    !State Geometry
  ! INTERFACE BLOCK SPECIFICATIONS

  ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)        ::  XShadowProjection   ! temporary buffer
  REAL(r64)        ::  YShadowProjection   ! temporary buffer
  REAL(r64)        ::  XSp           !for calc BSDF projection direction
  REAL(r64)        ::  YSp           !for calc BSDF projection direction
  REAL(r64)        ::  ZSp           !for calc BSDF projection direction
  REAL(r64)             ::  SdotX           !temporary variable for manipulating .dot. product
  REAL(r64)             ::  SdotY           !temporary variable for manipulating .dot. product
  REAL(r64)             ::  SdotZ           !temporary variable for manipulating .dot. product
  INTEGER         :: BackSurfaceNumber ! current back surface number
  INTEGER NVR
  INTEGER NVT  ! Number of vertices of back surface
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: XVT    ! X,Y,Z coordinates of vertices of
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: YVT    ! back surfaces projected into system
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: ZVT    ! relative to receiving surface
  INTEGER NS1      ! Number of the figure being overlapped
  INTEGER NS2      ! Number of the figure doing overlapping
  INTEGER NS3      ! Location to place results of overlap
  INTEGER IRay     ! Current ray of BSDF direction
  INTEGER KBkSurf  ! Current back surface
  INTEGER BaseSurf ! Base surface number
  INTEGER N, M
  INTEGER CurBaseSurf ! Currnet base surface number for shadow overlap calcualtions
  INTEGER curBackSurface ! Current back surface number for base surface
  INTEGER LOCStore ! Use to store pointer to highes data in local array
                   ! When that counter is used in this routine, it just
                   ! can be taken back to old number because all results
                   ! are stored within this routine

  ! Daylighting
  INTEGER IConst   ! Construction number of back surface
  INTEGER InsideConLay ! Construction's inside material layer number
  REAL(r64) :: VisibleReflectance  ! Visible reflectance for inside surface material
  REAL(r64) :: TotAOverlap     ! Total overlap area for given outgoing direction
  REAL(r64) :: TotARhoVisOverlap ! Total overlap area time reflectance for given outgoing direction

  ALLOCATE(XVT(MaxVerticesPerSurface+1))
  ALLOCATE(YVT(MaxVerticesPerSurface+1))
  ALLOCATE(ZVT(MaxVerticesPerSurface+1))
  XVT=0.0d0
  YVT=0.0d0
  ZVT=0.0d0

  ALLOCATE(Geom%Aoverlap(Geom%Trn%NBasis, Window%NBkSurf))
  Geom%Aoverlap = 0.0d0
  ALLOCATE(Geom%ARhoVisOverlap(Geom%Trn%NBasis, Window%NBkSurf))
  Geom%ARhoVisOverlap = 0.0d0
  ALLOCATE(Geom%AveRhoVisOverlap(Geom%Trn%NBasis))
  Geom%AveRhoVisOverlap = 0.0d0

  ! First to calculate and store coordinates of the window surface
  LOCHCA = 1
  BaseSurf = Surface(ISurf)%BaseSurf

  ! Base surface contains current window surface (ISurf).
  ! Since that is case, bellow transformation should always return ZVT = 0.0d0
  ! for every possible transformation
  CALL CTRANS(ISurf,BaseSurf,NVT,XVT,YVT,ZVT)

  ! HTRANS routine is using coordinates stored in XVS and YVS in order to calculate
  ! surface area.  Since both projections are equal to zero, then simply
  ! compy these values into XVS and YVS arrays
  DO N = 1, NVT
    XVS(N) = XVT(N)
    YVS(N) = YVT(N)
  END DO

  ! This calculates the area stored in XVS and YVS
  !CALL HTRANS(1,LOCHCA,NVT)
  CALL HTRANS1(LOCHCA,NVT)
  !HCAREA(LOCHCA) = -HCAREA(LOCHCA)

  ! Calculation of overlap areas for each outgoing basis direction
  DO IRay = 1, Geom%Trn%NBasis ! basis directions loop (on back surface)
    ! For current basis direction calculate dot product between window surface
    ! and basis direction.  This will be used to calculate projection of each
    ! of the back surfaces to window surface for given basis direciton
    SdotX = Surface(ISurf)%lcsx.dot.Geom%sTrn(IRay)
    SdotY = Surface(ISurf)%lcsy.dot.Geom%sTrn(IRay)
    SdotZ = Surface(ISurf)%lcsz.dot.Geom%sTrn(IRay)
    XSp = -SdotX
    YSp = -SdotY
    ZSp = -SdotZ

    ! Projection of shadows for current basis direciton
    IF (ABS(ZSp) > 1.d-4) THEN
      XShadowProjection = XSp/ZSp
      YShadowProjection = YSp/ZSp
      IF (ABS(XShadowProjection) < 1.d-8) XShadowProjection=0.0d0
      IF (ABS(YShadowProjection) < 1.d-8) YShadowProjection=0.0d0
    ELSE
      XShadowProjection = 0.0d0
      YShadowProjection = 0.0d0
    ENDIF

    DO KBkSurf  = 1 , Window%NBkSurf    !back surf loop
      !BaseSurf = Surface(ISurf)%BaseSurf
      BackSurfaceNumber = ShadowComb(BaseSurf)%BackSurf(KBkSurf)

      ! Transform coordinates of back surface from general system to the
      ! plane of the receiving surface
      CALL CTRANS(BackSurfaceNumber,BaseSurf,NVT,XVT,YVT,ZVT)

      ! Project "shadow" from back surface along sun's rays to receiving surface.  Back surface vertices
      ! become clockwise sequential.

      DO N = 1, NVT
        XVS(N) = XVT(N) - XShadowProjection*ZVT(N)
        YVS(N) = YVT(N) - YShadowProjection*ZVT(N)
      END DO

      ! Transform to the homogeneous coordinate system.

      NS3      = LOCHCA+1
      !NS3      = LOCHCA
      HCT(NS3) = 0.0d0
      !CALL HTRANS(1,NS3,NVT)
      CALL HTRANS1(NS3,NVT)

      ! Determine area of overlap of projected back surface and receiving surface.

      NS1      = 1
      NS2      = NS3
      HCT(NS3) = 1.0d0
      CALL DeterminePolygonOverlap(NS1,NS2,NS3)

      IF (OverlapStatus == NoOverlap) CYCLE ! to next back surface
      IF ( (OverlapStatus == TooManyVertices).OR. &
        (OverlapStatus == TooManyFigures) ) EXIT ! back surfaces DO loop

      LOCHCA         = NS3
      HCNS(LOCHCA)   = BackSurfaceNumber
      HCAREA(LOCHCA) = -HCAREA(LOCHCA)

      Geom%Aoverlap(IRay, KBkSurf) = HCAREA(LOCHCA)
    END DO ! DO KBkSurf  = 1 , NBkSurf

    ! If some of back surfaces is contained in base surface, then need to substract shadow of subsurface
    ! from shadow on base surface.  Reson is that above shadowing algorithm is calculating shadow wihtout
    ! influence of subsurfaces
    DO KBkSurf  = 1 , Window%NBkSurf    !back surf loop
      BackSurfaceNumber = ShadowComb(BaseSurf)%BackSurf(KBkSurf)
      CurBaseSurf = Surface(BackSurfaceNumber)%BaseSurf
      IF (CurBaseSurf /= BackSurfaceNumber) THEN
        ! Search if that base surface in list of back surfaces for current window
        CurBackSurface = 0
        DO N = 1, Window%NBkSurf
          IF (ShadowComb(BaseSurf)%BackSurf(N) == CurBaseSurf) THEN
            curBackSurface = N
            EXIT
          END IF
        END DO
        IF (CurBackSurface /= 0) THEN
          Geom%Aoverlap(IRay, curBackSurface) = Geom%Aoverlap(IRay, curBackSurface) - Geom%Aoverlap(IRay, KBkSurf)
        END IF
      END IF
    END DO

    ! Calculate overlap area times reflectance.  This is necessary for complex fenestration daylighting calculations
    TotAOverlap = 0.0d0
    TotARhoVisOverlap = 0.0d0
    DO KBkSurf  = 1 , Window%NBkSurf    !back surf loop
      BackSurfaceNumber = ShadowComb(BaseSurf)%BackSurf(KBkSurf)
      CurBaseSurf = Surface(BackSurfaceNumber)%BaseSurf
      IConst = Surface(BackSurfaceNumber)%Construction
      InsideConLay = Construct(IConst)%TotLayers
      IF (SurfaceWindow(BackSurfaceNumber)%WindowModelType == WindowBSDFModel) THEN
        VisibleReflectance = Construct(IConst)%ReflectVisDiffBack
      ELSE
        VisibleReflectance = (1.0d0 -  Material(InsideConLay)%AbsorpVisible)
      END IF
      Geom%ARhoVisoverlap(IRay, KBkSurf) = Geom%Aoverlap(IRay, KBkSurf) * VisibleReflectance
      TotAOverlap = TotAOverlap + Geom%Aoverlap(IRay, KBkSurf)
      TotARhoVisOverlap = TotARhoVisOverlap + Geom%ARhoVisoverlap(IRay, KBkSurf)
    END DO

    IF (TotAOverlap /= 0.0d0) THEN
      Geom%AveRhoVisOverlap(IRay) = TotARhoVisOverlap / TotAOverlap
    END IF

  END DO ! DO IRay = 1, Geom%Trn%NBasis

  ! Reset back shadowing counter since complex windows do not need it anymore
  LOCHCA = 1

  DEALLOCATE(XVT)
  DEALLOCATE(YVT)
  DEALLOCATE(ZVT)

end subroutine CalcComplexWindowOverlap

subroutine TimestepInitComplexFenestration
  ! SUBROUTINE INFORMATION:
  !       AUTHOR         Simon Vidanovic
  !       DATE WRITTEN   May 2012
  !       MODIFIED       May 2012 (Initialize complex fenestration in case of EMS)
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS SUBROUTINE:
  ! Performs initialization of complex fenestration. It also performs check if current surface containing
  ! complex fenestration have construction changed (by EMS) in which case performs addition of current states
  ! into complex fenestration array

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES:
  ! na

  ! USE STATEMENTS:
  use WindowComplexManager, only: CheckCFSStates

  integer iSurf  ! Current surface number
  integer iState ! current state number
  integer NumOfStates ! number of states for current window


  do iSurf = 1, TotSurfaces
    if (SurfaceWindow(iSurf)%WindowModelType == WindowBSDFModel) then
      ! This will check complex fenestrations state and add new one if necessary (EMS case)
      call CheckCFSStates(iSurf)

      NumOfStates = ComplexWind(iSurf)%NumStates

      ! Check for overlap areas and initialize if necessary
      do iState = 1, NumOfStates
        ! do initialization only once
        if (ComplexWind(iSurf)%Geom(iState)%InitState) then
          call CalcComplexWindowOverlap(ComplexWind(iSurf)%Geom(iState), ComplexWind(iSurf), iSurf)
          ComplexWind(iSurf)%Geom(iState)%InitState = .FALSE.
        end if
      end do
    end if
  end do

end subroutine TimestepInitComplexFenestration

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

END MODULE SolarShading