MODULE DataDaylighting

          ! MODULE INFORMATION:
          !       AUTHOR         Linda Lawrie/Fred Winkelmann (Re-engineered by Peter Graham Ellis)
          !       DATE WRITTEN   May 1998
          !       MODIFIED       B.Griffith added interior window data for associated exterior windows
          !       RE-ENGINEERED  April 2003

          ! PURPOSE OF THIS MODULE:
          ! This data-only module is a repository for variables used in daylighting which
          ! are shared by several modules.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! OTHER NOTES:
          ! na

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals, ONLY: MaxNameLength

IMPLICIT NONE   ! Enforce explicit typing of all variables

PUBLIC          ! By definition, all variables which are placed in this data
                ! -only module should be available to other modules and routines.
                ! Thus, all variables in this module must be PUBLIC.


          ! MODULE PARAMETER DEFINITIONS:
          ! Two kinds of reference points: used directly in daylighting, used to show illuminance map of zone
INTEGER, PARAMETER :: MaxRefPoints = 2        ! Maximum number of daylighting reference points, 2
INTEGER, PARAMETER :: MaxMapRefPoints=2500    ! Maximum number of Illuminance Map Ref Points

INTEGER, PARAMETER :: NotInOrAdjZoneExtWin = 0 ! Exterior window is not in a Daylighting:Detailed zone
                                               ! or in an adjacent zone with a shared interior window
INTEGER, PARAMETER :: InZoneExtWin         = 1 ! Exterior window is in a Daylighting:Detailed zone
INTEGER, PARAMETER :: AdjZoneExtWin        = 2 ! Exterior window is in a zone adjacent to a Daylighting:
                                               ! Detailed zone with which it shares an interior window

INTEGER, PARAMETER :: CalledForRefPoint    = 101
INTEGER, PARAMETER :: CalledForMapPoint    = 102

! Parameters for "DaylightType"
INTEGER, PARAMETER :: NoDaylighting       = 0
INTEGER, PARAMETER :: DetailedDaylighting = 1
INTEGER, PARAMETER :: DElightDaylighting  = 2
CHARACTER(len=*), PARAMETER, DIMENSION(2) :: DaylightTypes =   &
                               (/'Daylighting:Controls        ',  &
                                 'Daylighting:DELight:Controls'/)

          ! DERIVED TYPE DEFINITIONS:
TYPE IntWinAdjZoneExtWinStruct ! nested structure for ZoneDaylight
  INTEGER                            :: SurfNum        =0 ! exterior window index
  INTEGER                            :: NumOfIntWindows=0 ! count of interior windows associated with this ext win
  INTEGER, ALLOCATABLE, DIMENSION(:) :: IntWinNum ! index numbers for interior windows assoc with this ext win
END TYPE IntWinAdjZoneExtWinStruct

TYPE ZoneDaylightCalc
  INTEGER  :: DaylightType               = 0   ! Type of Daylighting (1=Detailed, 2=DElight)
  INTEGER  :: AvailSchedNum             = 0    ! pointer to availability schedule if present
  INTEGER  :: TotalDaylRefPoints        = 0   ! Number of detailed daylighting reference points in a zone (0,1 or 2)
  INTEGER  :: TotalDElightRefPts        = 0   ! Number of DElight daylighting reference points in a zone (0,1 or 2) - RJH
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: DaylRefPtAbsCoord ! =0.0 ! X,Y,Z coordinates of all daylighting reference points
                                                        ! in absolute coordinate system (m)
                                                        ! Points 1 and 2 are the control reference points
  LOGICAL(kind=1), ALLOCATABLE, DIMENSION(:) :: DaylRefPtInBounds  ! True when coordinates are in bounds of zone coordinates
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: FracZoneDaylit ! =0.0  ! Fraction of zone controlled by each reference point
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: IllumSetPoint  ! =0.0  ! Illuminance setpoint at each reference point (lux)
  INTEGER  :: LightControlType          = 1   ! Lighting control type (same for all reference points)
                                              ! (1=continuous, 2=stepped, 3=continuous/off)
  REAL(r64):: ViewAzimuthForGlare       =0.0d0  ! View direction relative to window for glare calculation (deg)
  INTEGER  :: MaxGlareallowed           = 0   ! Maximum allowable discomfort glare index
  REAL(r64):: MinPowerFraction          = 0.0d0 ! Minimum fraction of power input that continuous dimming system can dim down to
  REAL(r64):: MinLightFraction          =0.0d0  ! Minimum fraction of light output that continuous dimming system can dim down to
  INTEGER  :: LightControlSteps         = 0   ! Number of levels (excluding zero) of stepped control system
  REAL(r64):: LightControlProbability   = 0.0d0 ! For manual control of stepped systems, probability that lighting will
  INTEGER  :: TotalExtWindows           = 0   ! Total number of exterior windows in the zone
  REAL(r64):: AveVisDiffReflect         =0.0d0  ! Area-weighted average inside surface visible reflectance of zone
  REAL(r64),ALLOCATABLE, Dimension (:) :: RefPtPowerReductionFactor !=1.0  ! Electric power reduction factor at reference points
                                                                  ! due to daylighting
  REAL(r64):: ZonePowerReductionFactor      =1.0D0  ! Electric power reduction factor for entire zone due to daylighting
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: DaylIllumAtRefPt  !=0.0 ! Daylight illuminance at reference points (lux)
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: GlareIndexAtRefPt !=0.0 ! Glare index at reference points
  INTEGER, ALLOCATABLE, DIMENSION(:) :: AdjIntWinZoneNums ! List of zone numbers of adjacent zones that have exterior windows and
                                     ! share one or more interior windows with target zone
  INTEGER  :: NumOfIntWinAdjZones        = 0   ! Number of adjacent zones that have exterior windows and share one or
                                              ! more interior windows with target zone
  INTEGER  :: NumOfIntWinAdjZoneExtWins  = 0   ! number of exterior windows associated with zone via interior windows
  TYPE(IntWinAdjZoneExtWinStruct), ALLOCATABLE, DIMENSION(:) &  ! nested structure
                                     :: IntWinAdjZoneExtWin ! info about exterior window associated with zone via interior window
  INTEGER  :: NumOfDayltgExtWins         = 0   ! Number of associated exterior windows providing daylight to this zone
  INTEGER, ALLOCATABLE, DIMENSION(:) ::  DayltgExtWinSurfNums  ! List of surface numbers of zone's exterior windows or
                                                ! exterior windows in adjacent zones sharing interior windows with the zone
  INTEGER, ALLOCATABLE, DIMENSION(:) ::  DayltgFacPtrsForExtWins ! Zone's daylighting factor pointers.
                                                                 ! Entries in this list have a one-to-one
                                                                 ! correspondence with the DayltgExtWinSurfNums list
  REAL(r64):: MinIntWinSolidAng             =0.0d0 ! Minimum solid angle subtended by an interior window in a zone
  REAL(r64):: TotInsSurfArea                =0.0d0 ! Total inside surface area of a daylit zone (m2)
  REAL(r64):: FloorVisRefl                  =0.0d0 ! Area-weighted visible reflectance of floor of a daylit zone
  REAL(r64):: InterReflIllFrIntWins         =0.0d0 ! Inter-reflected illuminance due to beam and diffuse solar passing
                                             !  through a zone's interior windows (lux)
  REAL(r64),ALLOCATABLE, DIMENSION(:) :: BacLum          ! =0.0 ! Background luminance at each reference point (cd/m2)
  REAL(r64), ALLOCATABLE, DIMENSION(:,:)     :: SolidAngAtRefPt !(MaxRefPoints,50)
  REAL(r64), ALLOCATABLE, DIMENSION(:,:)     :: SolidAngAtRefPtWtd !(MaxRefPoints,50)
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:)   :: IllumFromWinAtRefPt !(MaxRefPoints,2,50)
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:)   :: BackLumFromWinAtRefPt  !(MaxRefPoints,2,50)
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:)   :: SourceLumFromWinAtRefPt  !(MaxRefPoints,2,50)
  ! Allocatable daylight factor arrays
  ! Arguments for Dayl---Sky are:
  !  1: Daylit window number (1 to NumOfDayltgExtWins)
  !  2: Reference point number (1 to MaxRefPoints)
  !  3: Sky type (1 to 4; 1 = clear, 2 = clear turbid, 3 = intermediate, 4 = overcast
  !  4: Shading index (1 to MaxSlatAngs+1; 1 = bare window; 2 = with shade, or, if blinds
  !      2 = first slat position, 3 = second position, ..., MaxSlatAngs+1 = last position)
  !  5: Sun position index (1 to 24)
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: DaylIllFacSky
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: DaylSourceFacSky
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: DaylBackFacSky

  ! Arguments for Dayl---Sun are:
  !  1: Daylit window number (1 to NumOfDayltgExtWins)
  !  2: Reference point number (1 to MaxRefPoints)
  !  3: Shading index (1 to MaxShadeIndex; 1 = no shade; 2 = with shade, or, if blinds
  !      2 = first slat position, 3 = second position, ..., MaxSlatAngs+1 = last position)
  !  4: Sun position index (1 to 24)
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:) :: DaylIllFacSun
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:) :: DaylIllFacSunDisk
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:) :: DaylSourceFacSun
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:) :: DaylSourceFacSunDisk
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:) :: DaylBackFacSun
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:) :: DaylBackFacSunDisk

  ! Time exceeding maximum allowable discomfort glare index at reference points (hours)
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: TimeExceedingGlareIndexSPAtRefPt

  ! Time exceeding daylight illuminance setpoint at reference points (hours)
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: TimeExceedingDaylightIlluminanceSPAtRefPt

  ! True if at least one adjacent zone, sharing one or more interior windows, has daylighting control
  LOGICAL :: AdjZoneHasDayltgCtrl = .false.
  INTEGER :: MapCount             = 0  ! Number of maps assigned to Zone
  INTEGER, ALLOCATABLE, DIMENSION(:) :: ZoneToMap  ! Pointers to maps allocated to Zone
END TYPE ZoneDaylightCalc

TYPE IllumMapData
  CHARACTER(len=MaxNameLength) :: Name     =' '  ! Map name
  INTEGER                      :: Zone     =0    ! Pointer to zone being mapped
  REAL(r64)                    :: Z        =0.0d0  ! Elevation or height
  REAL(r64)                    :: Xmin     =0.0d0  ! Minimum X value
  REAL(r64)                    :: Xmax     =0.0d0  ! Maximum X value
  INTEGER                      :: Xnum     =0    ! Number of X reference points (going N-S)
  REAL(r64)                    :: Xinc     =0.0d0  ! Increment between X reference points
  REAL(r64)                    :: Ymin     =0.0d0  ! Minimum Y value
  REAL(r64)                    :: Ymax     =0.0d0  ! Maximum Y value
  INTEGER                      :: Ynum     =0    ! Number of Y reference points (going E-W)
  REAL(r64)                    :: Yinc     =0.0d0  ! Increment between Y reference points
  INTEGER                      :: UnitNo   =0    ! Unit number for map output (later merged to final file)
  LOGICAL                      :: HeaderXLineLengthNeeded = .true. ! X header will likely be the longest line in the file
  INTEGER                      :: HeaderXLineLength = 0   ! actual length of this X header line
END TYPE

TYPE MapCalcData
  INTEGER  :: TotalMapRefPoints = 0   ! Number of illuminance map reference points in this zone (up to 100)
  INTEGER  :: Zone               =0    ! Pointer to zone being mapped
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: MapRefPtAbsCoord  ! X,Y,Z coordinates of all illuminance map reference points
                                                        ! in absolute coordinate system (m)
                                                        ! Points 1 and 2 are the control reference points
  LOGICAL(kind=1), ALLOCATABLE, DIMENSION(:) :: MapRefPtInBounds  ! True when coordinates are in bounds of zone coordinates
  REAL(r64), ALLOCATABLE, DIMENSION(:)     :: DaylIllumAtMapPt  ! Daylight illuminance at illuminance map points (lux)
  REAL(r64), ALLOCATABLE, DIMENSION(:)     :: GlareIndexAtMapPt ! Glare index at illuminance map points
        ! following Hr - report avg hr
  REAL(r64), ALLOCATABLE, DIMENSION(:)     :: DaylIllumAtMapPtHr  ! Daylight illuminance at illuminance map points (lux)
  REAL(r64), ALLOCATABLE, DIMENSION(:)     :: GlareIndexAtMapPtHr ! Glare index at illuminance map points
  REAL(r64), ALLOCATABLE, DIMENSION(:,:)   :: SolidAngAtMapPt !(MaxRefPoints,50)
  REAL(r64), ALLOCATABLE, DIMENSION(:,:)   :: SolidAngAtMapPtWtd !(MaxRefPoints,50)
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:) :: IllumFromWinAtMapPt !(MaxRefPoints,2,50)
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:) :: BackLumFromWinAtMapPt  !(MaxRefPoints,2,50)
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:) :: SourceLumFromWinAtMapPt  !(MaxRefPoints,2,50)
  !  1: Daylit window number (1 to NumOfDayltgExtWins)
  !  2: Reference point number (1 to MaxRefPoints)
  !  3: Sky type (1 to 4; 1 = clear, 2 = clear turbid, 3 = intermediate, 4 = overcast
  !  4: Shading index (1 to MaxSlatAngs+1; 1 = bare window; 2 = with shade, or, if blinds
  !      2 = first slat position, 3 = second position, ..., MaxSlatAngs+1 = last position)
  !  5: Sun position index (1 to 24)
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: DaylIllFacSky
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: DaylSourceFacSky
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:,:) :: DaylBackFacSky
  ! Arguments for Dayl---Sun are:
  !  1: Daylit window number (1 to NumOfDayltgExtWins)
  !  2: Reference point number (1 to MaxRefPoints)
  !  3: Shading index (1 to MaxShadeIndex; 1 = no shade; 2 = with shade, or, if blinds
  !      2 = first slat position, 3 = second position, ..., MaxSlatAngs+1 = last position)
  !  4: Sun position index (1 to 24)
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:) :: DaylIllFacSun
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:) :: DaylIllFacSunDisk
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:) :: DaylSourceFacSun
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:) :: DaylSourceFacSunDisk
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:) :: DaylBackFacSun
  REAL(r64), ALLOCATABLE, DIMENSION(:,:,:,:) :: DaylBackFacSunDisk
END TYPE

          ! MODULE VARIABLE TYPE DECLARATIONS:
TYPE (ZoneDaylightCalc), ALLOCATABLE, DIMENSION(:) :: ZoneDaylight
TYPE (IllumMapData),     ALLOCATABLE, DIMENSION(:) :: IllumMap
TYPE (MapCalcData),      ALLOCATABLE, DIMENSION(:) :: IllumMapCalc

          ! INTERFACE BLOCK SPECIFICATIONS: na

          ! MODULE VARIABLE DECLARATIONS:
INTEGER :: TotIllumMaps    = 0
LOGICAL :: mapResultsToReport=.false.  ! used when only partial hour has "sun up"
LOGICAL :: mapResultsReported=.false.  ! when no map results are ever reported this will still be false
CHARACTER(len=1) :: MapColSep=' '   ! Character for separating map columns (tab, space, comma)

LOGICAL :: DFSReportSizingDays = .false.
LOGICAL :: DFSReportAllShadowCalculationDays = .false.


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

END MODULE DataDaylighting
