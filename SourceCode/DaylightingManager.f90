MODULE DaylightingManager

          ! MODULE INFORMATION
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   July 1997, December 1998
          !       MODIFIED       Oct 2004; LKL -- Efficiencies and code restructure
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Manages the daylighting calculations for each thermal zone that has an associated
          ! DAYLIGHTING:DETAILED object.

          ! Includes calculation of interior daylight illuminance and glare
          ! from each of the windows in a zone, control of window shading devices
          ! to reduce glare, and control of overhead electric lighting in response
          ! to interior daylight illuminance level at one or two user-specified
          ! reference points at which sensors are located.

          ! METHODOLOGY EMPLOYED:
          !
          ! REFERENCES:
          ! "Daylighting Calculation in DOE-2," F.C.Winkelmann, LBL-11353, May 1983
          ! "Daylighting Simulation in the DOE-2 Building Energy Analysis Program,"
          ! F.C. Winkelmann and S. Selkowitz, Energy and Buildings 8(1985)271-286

          ! OTHER NOTES:
          ! This module was created from DOE-2.1E subroutines.

          ! Correspondence between DOE-2.1E and EnergyPlus subroutine names:

          ! DOE-2.1E    EnergyPlus                      In Module           Called from Module
          !
          ! DAVREF      DayltgAveInteriorReflectance    DaylightingManager DaylightingManager
          ! DCOF        CalcDayltgCoefficients          DaylightingManager DaylightingManager
          ! DCROSS      DayltgCrossProduct              DaylightingManager DaylightingManager
          ! DEXTIL      DayltgCurrentExtHorizIllum      WeatherManager     WeatherManager
          ! DGLARE      DayltgGlare                     DaylightingManager DaylightingManager
          ! DHILL       DayltgExtHorizIllum             DaylightingManager DaylightingManager
          ! DHITSH      DayltgHitObstruction            DaylightingManager DaylightingManager
          ! DINTIL      DayltgInteriorIllum             DaylightingManager HeatBalanceSurfaceManager
          ! DLTSYS      DayltgElecLightingControl       DaylightingManager HeatBalanceSurfaceManager
          ! DNSOL       not used
          ! DPFAC       DayltgPositionFactor            DaylightingManager DaylightingManager
          ! DPIERC      DayltgPierceSurface             DaylightingManager DaylightingManager
          ! DREFLT      DayltgInterReflectedIllum       DaylightingManager DaylightingManager
          ! DSKYLU      DayltgSkyLuminance              DaylightingManager DaylightingManager
          ! DTHLIM      DayltgAzimuthLimits             DaylightingManager DaylightingManager
          ! DLUMEF      DayltgLuminousEfficacy          WeatherManager     WeatherManager

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals
USE DataHeatBalance
USE DataSurfaces
USE DataEnvironment
USE DataDaylighting
USE DataDaylightingDevices
USE DataInterfaces

USE ScheduleManager
!USE Vectors

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE

          ! MODULE PARAMETER DEFINITIONS: na

          ! MODULE VARIABLE DECLARATIONS:
INTEGER,PUBLIC    :: TotWindowsWithDayl       =0    ! Total number of exterior windows in all daylit zones
INTEGER :: OutputFileDFS  =0 ! Unit number for daylight factors
REAL(r64) :: DaylIllum(MaxRefPoints)  =0.0  ! Daylight illuminance at reference points (lux)
REAL(r64) :: PHSUN                    =0.0  ! Solar altitude (radians)
REAL(r64) :: SPHSUN                   =0.0  ! Sine of solar altitude
REAL(r64) :: CPHSUN                   =0.0  ! Cosine of solar altitude
REAL(r64) :: THSUN                    =0.0  ! Solar azimuth (rad) in Absolute Coordinate System (azimuth=0 along east)
REAL(r64) :: PHSUNHR(24)              =0.0  ! Hourly values of PHSUN
REAL(r64) :: SPHSUNHR(24)             =0.0  ! Hourly values of the sine of PHSUN
REAL(r64) :: CPHSUNHR(24)             =0.0  ! Hourly values of the cosine of PHSUN
REAL(r64) :: THSUNHR(24)              =0.0  ! Hourly values of THSUN

! In the following I,J,K arrays:
! I = 1 for clear sky, 2 for clear turbid, 3 for intermediate, 4 for overcast;
! J = 1 for bare window, 2 - 12 for shaded;
! K = sun position index.
REAL(r64) :: EINTSK(4,MaxSlatAngs+1,24)  =0.0  ! Sky-related portion of internally reflected illuminance
REAL(r64) :: EINTSU(MaxSlatAngs+1,24)    =0.0  ! Sun-related portion of internally reflected illuminance,
                                               ! excluding entering beam
REAL(r64) :: EINTSUdisk(MaxSlatAngs+1,24)=0.0  ! Sun-related portion of internally reflected illuminance
                                               ! due to entering beam
REAL(r64) :: WLUMSK(4,MaxSlatAngs+1,24)  =0.0  ! Sky-related window luminance
REAL(r64) :: WLUMSU(MaxSlatAngs+1,24)    =0.0  ! Sun-related window luminance, excluding view of solar disk
REAL(r64) :: WLUMSUdisk(MaxSlatAngs+1,24)=0.0  ! Sun-related window luminance, due to view of solar disk

REAL(r64) :: GILSK(4,24)                 =0.0  ! Horizontal illuminance from sky, by sky type, for each hour of the day
REAL(r64) :: GILSU(24)                   =0.0  ! Horizontal illuminance from sun for each hour of the day

!! Allocatable daylight factor arrays  -- are in the ZoneDaylight Structure

REAL(r64), ALLOCATABLE :: TDDTransVisBeam(:,:)
REAL(r64), ALLOCATABLE :: TDDFluxInc(:,:,:)
REAL(r64), ALLOCATABLE :: TDDFluxTrans(:,:,:)

INTEGER, ALLOCATABLE, DIMENSION(:,:) :: MapErrIndex
INTEGER, ALLOCATABLE, DIMENSION(:,:) :: RefErrIndex

LOGICAL, ALLOCATABLE, DIMENSION(:) :: CheckTDDZone


CHARACTER(len=2500) :: mapLine   ! character variable to hold map outputs

PUBLIC DayltgInteriorIllum
PUBLIC DayltgInteriorTDDIllum
PUBLIC DayltgElecLightingControl
PUBLIC CalcDayltgCoefficients
PUBLIC ProfileAngle
PUBLIC CloseReportIllumMaps
PUBLIC CloseDFSFile
PUBLIC DayltgInterReflIllFrIntWins
PUBLIC DayltgGlareWithIntWins
PUBLIC ReportIllumMap

PUBLIC DayltgInteriorMapIllum

PRIVATE DayltgAveInteriorReflectance
PRIVATE GetDaylightingParametersDetaild
PRIVATE DayltgGlare
PRIVATE DayltgExtHorizIllum
PRIVATE DayltgPierceSurface
PRIVATE DayltgHitObstruction
PRIVATE DayltgHitInteriorObstruction
PRIVATE DayltgInterReflectedIllum
PRIVATE DayltgSkyLuminance
PRIVATE DayltgCrossProduct
PRIVATE DayltgSetupAdjZoneListsAndPointers
PRIVATE CheckForGeometricTransform

          ! SUBROUTINE SPECIFICATIONS FOR MODULE DaylightingModule
CONTAINS

          ! MODULE SUBROUTINES:

SUBROUTINE DayltgAveInteriorReflectance(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   July 1997
          !       MODIFIED       Mar 2004, FCW: add calculation of following SurfaceWindow variables:
          !                        ZoneAreaMinusThisSurf, ZoneAreaReflProdMinusThisSurf, RhoCeilingWall,
          !                        RhoFloorWall, FractionUpgoing. Add calculation of ZoneDaylight%FloorVisRefl.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Called by CalcDayltgCoefficients for each daylit zone. Determines total
          ! area and area-weighted average visible reflectance of
          ! all inside faces of the surfaces of a zone.  In addition, finds
          ! area and average reflectance of interzone, underground and exterior
          ! heat-transfer surfaces in the following categories: floor (tilt > 170 deg),
          ! ceiling (tilt < 10 deg), and wall (10 < tilt < 170 deg).
          ! The window reflectance values used here assume the windows have no shading
          ! devices. This information is used in the calculation of the
          ! internally-reflected daylighting component.

          ! Finds total number of exterior windows in the space.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! Based on DOE-2.1E subroutine DAVREF

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER           :: ZoneNum        ! Zone number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: IType          ! Surface type/class
  REAL(r64) :: AREA           ! Inside surface area (m2)
  REAL(r64) :: AInsTot        ! Total inside surface area of a zone (m2)
  REAL(r64) :: ARHTOT         ! Sum over surfaces of AREA*(inside visible reflectance) (m2)
  INTEGER   :: ISurf          ! Surface number
  INTEGER   :: IWin           ! Window number
  INTEGER   :: ITILT          ! Surface tilt category (1 = floor, 2 = wall, 3 = ceiling)
  INTEGER   :: IT             ! Tilt index
  REAL(r64) :: AR(3)          ! Inside surface area sum for floor/wall/ceiling (m2)
  REAL(r64) :: ARH(3)         ! Inside surface area*reflectance sum for floor/wall/ceiling (m2)
  REAL(r64) :: AP(3)          ! Zone inside surface floor/wall/ceiling area without a selected
                              !  floor/wall/ceiling (m2)
  REAL(r64) :: ARHP(3)        ! Zone inside surface floor/wall/ceiling area*reflectance without
                              !  a selected floor/wall/ceiling (m2)
  REAL(r64) :: ATWL           ! Opaque surface area (m2)
  REAL(r64) :: ARHTWL         ! ATWL times inside visible reflectance of surface (m2)
  INTEGER   :: IWinDr         ! Window/door surface number
  REAL(r64) :: ETA            ! Ratio of floor-to-window-center height and average floor-to-ceiling height



          ! FLOW:

  ! Total inside surface area, including windows
  AInsTOT   = 0.
  ! Sum of products of inside surface area * vis reflectance
  ARHTOT = 0.
  ! Area sum and area * reflectance sum for different orientations
  AR = 0.
  ARH = 0.
  ! Loop over surfaces
  DO ISurf = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
    IType = Surface(ISurf)%Class
    ! Error if window has multiplier > 1 since this causes incorrect illuminance calc
    IF (IType == SurfaceClass_Window .AND. Surface(ISurf)%Multiplier > 1.0) THEN
      IF (ZoneDaylight(ZoneNum)%TotalDaylRefPoints > 0) THEN
        CALL ShowSevereError('DayltgAveInteriorReflectance: Multiplier > 1.0 for window '//  &
           trim(Surface(ISurf)%Name)//' in Zone='//trim(Surface(ISurf)%ZoneName))
        CALL ShowContinueError('...not allowed since it is in a zone with daylighting.')
        CALL ShowFatalError('Progrem terminates due to preceding conditions.')
      ELSE
        CALL ShowSevereError('DayltgAveInteriorReflectance: Multiplier > 1.0 for window '//  &
           trim(Surface(ISurf)%Name)//' in Zone='//trim(Surface(ISurf)%ZoneName))
        CALL ShowContinueError('...an adjacent Zone has daylighting. Simulation cannot proceed.')
        CALL ShowFatalError('Progrem terminates due to preceding conditions.')
      ENDIF
    ENDIF
    IF (IType == SurfaceClass_Wall.OR.IType == SurfaceClass_Floor.OR.IType == SurfaceClass_Roof &
      .OR.IType == SurfaceClass_Window.OR.IType == SurfaceClass_Door) THEN
      AREA = Surface(ISurf)%Area
      ! In following, FrameArea and DividerArea can be non-zero only for exterior windows
      AInsTOT = AInsTOT + AREA + SurfaceWindow(ISurf)%FrameArea*(1.+0.5*SurfaceWindow(ISurf)%ProjCorrFrIn) &
            + SurfaceWindow(ISurf)%DividerArea*(1.+SurfaceWindow(ISurf)%ProjCorrDivIn)
      ARHTOT = ARHTOT + AREA * Construct(Surface(ISurf)%Construction)%ReflectVisDiffBack + &
                 SurfaceWindow(ISurf)%FrameArea * (1.d0+0.5d0*SurfaceWindow(ISurf)%ProjCorrFrIn) * &
                 (1.d0-SurfaceWindow(ISurf)%FrameSolAbsorp) + &
                 SurfaceWindow(ISurf)%DividerArea * (1.d0+SurfaceWindow(ISurf)%ProjCorrDivIn) * &
                 (1.d0-SurfaceWindow(ISurf)%DividerSolAbsorp)
      ITILT = 3 ! Ceiling
      IF (Surface(ISurf)%Tilt > 10.0d0 .AND.Surface(ISurf)%Tilt < 170.0d0) ITILT = 2 ! Wall
      IF (Surface(ISurf)%Tilt >= 170.0d0) ITILT = 1 ! Floor
      AR(ITILT) = AR(ITILT) + AREA + &
                     SurfaceWindow(ISurf)%FrameArea * (1.d0+0.5d0*SurfaceWindow(ISurf)%ProjCorrFrIn) &
                     + SurfaceWindow(ISurf)%DividerArea * (1.d0+SurfaceWindow(ISurf)%ProjCorrDivIn)
      ARH(ITILT) = ARH(ITILT) + AREA*Construct(Surface(ISurf)%Construction)%ReflectVisDiffBack +  &
                     SurfaceWindow(ISurf)%FrameArea * (1.d0+0.5d0*SurfaceWindow(ISurf)%ProjCorrFrIn) * &
                     (1.d0-SurfaceWindow(ISurf)%FrameSolAbsorp) + &
                     SurfaceWindow(ISurf)%DividerArea * (1.+SurfaceWindow(ISurf)%ProjCorrDivIn) * &
                     (1.d0-SurfaceWindow(ISurf)%DividerSolAbsorp)

    END IF
  END DO

  ! Average inside surface reflectance of zone
  ZoneDaylight(ZoneNum)%AveVisDiffReflect  = ARHTOT / AInsTOT
  ! Total inside surface area of zone
  ZoneDaylight(ZoneNum)%TotInsSurfArea = AInsTOT
  ! Average floor visible reflectance
  ZoneDaylight(ZoneNum)%FloorVisRefl = ARH(3) / (AR(3) + 1.d-6)

  DO ISurf = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
    IType = Surface(ISurf)%Class
    IF(IType == SurfaceClass_Wall.OR.IType == SurfaceClass_Floor.OR.IType == SurfaceClass_Roof) THEN
      ! Remove this surface from the zone inside surface area and area*reflectivity
      ! The resulting areas are AP(ITILT). The resulting area*reflectivity is ARHP(ITILT).
      ! Initialize gross area of surface (including subsurfaces)
      ATWL = Surface(ISurf)%Area ! This is the surface area less subsurfaces
      ! Area * reflectance for this surface, excluding attached windows and doors
      ARHTWL = Surface(ISurf)%Area * Construct(Surface(ISurf)%Construction)%ReflectVisDiffBack
      ! Tilt index
      IF(Surface(ISurf)%Tilt > 45.0d0 .AND. Surface(ISurf)%Tilt < 135.0d0) THEN
        ITILT = 2 ! Wall
      ELSE IF(Surface(ISurf)%Tilt >= 135.0d0) THEN
        ITILT = 1 ! Floor
      ELSE
        ITILT = 3 ! Ceiling
      END IF
      ! Loop over windows and doors on this wall
      DO IWinDr = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
        IF((Surface(IWinDr)%Class == SurfaceClass_Window .OR. Surface(IWinDr)%Class == SurfaceClass_Door) &
              .AND. Surface(IWinDr)%BaseSurf == ISurf) THEN
          ATWL   = ATWL + Surface(IWinDr)%Area + &
                   SurfaceWindow(IWinDr)%FrameArea * (1.d0+0.5d0*SurfaceWindow(IWinDr)%ProjCorrFrIn) + &
                   SurfaceWindow(IWinDr)%DividerArea * (1.d0+SurfaceWindow(IWinDr)%ProjCorrDivIn)
          ARHTWL = ARHTWL + Surface(IWinDr)%Area*Construct(Surface(IWinDr)%Construction)%ReflectVisDiffBack + &
                   SurfaceWindow(IWinDr)%FrameArea*(1.d0+0.5d0*SurfaceWindow(IWinDr)%ProjCorrFrIn)* &
                    (1.d0-SurfaceWindow(IWinDr)%FrameSolAbsorp) + &
                   SurfaceWindow(IWinDr)%DividerArea*(1.d0+SurfaceWindow(IWinDr)%ProjCorrDivIn)* &
                    (1.d0-SurfaceWindow(IWinDr)%DividerSolAbsorp)
        END IF
      END DO
      ! Inside surface area of floor, walls and ceilings, minus surface ISurf and its subsurfaces
      DO IT = 1,3
        IF(IT == ITILT) THEN
          AP(IT) = AR(IT) - ATWL
          ARHP(IT) = ARH(IT) - ARHTWL
        ELSE
          AP(IT) = AR(IT)
          ARHP(IT) = ARH(IT)
        END IF
      END DO
      SurfaceWindow(ISurf)%ZoneAreaMinusThisSurf = AP
      SurfaceWindow(ISurf)%ZoneAreaReflProdMinusThisSurf = ARHP
    END IF
  END DO ! End of loop over opaque surfaces in zone

  DO IWin = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
    IF(Surface(IWin)%Class == SurfaceClass_Window) THEN
      ISurf = Surface(IWin)%BaseSurf
      ! Ratio of floor-to-window-center height and average floor-to-ceiling height
      ETA = MAX(0.0d0, MIN(1.0d0,(SurfaceWindow(IWin)%WinCenter(3) - Zone(ZoneNum)%OriginZ) * &
              Zone(ZoneNum)%FloorArea / Zone(ZoneNum)%Volume))
      AP = SurfaceWindow(ISurf)%ZoneAreaMinusThisSurf
      ARHP = SurfaceWindow(ISurf)%ZoneAreaReflProdMinusThisSurf
      ! Average reflectance seen by light moving up (RhoCeilingWall) and down (RhoFloorWall)
      ! across horizontal plane through center of window
      SurfaceWindow(IWin)%RhoCeilingWall = (ARHP(2) * (1.d0 - ETA) + ARHP(3)) / (AP(2) * (1.d0 - ETA) + AP(3) + 1.0d-5)
      SurfaceWindow(IWin)%RhoFloorWall   = (ARHP(2) * ETA + ARHP(1)) / (AP(2) * ETA + AP(1) + 1.d-9)

      ! Angle factor for windows with diffusing shades. SurfaceWindow(IWin)%FractionUpgoing is
      ! fraction of light from the shade that goes up toward ceiling and upper part of walls.
      ! 1 - SurfaceWindow(IWin)%FractionUpgoing is fraction that goes down toward floor and lower part of walls.
      SurfaceWindow(IWin)%FractionUpgoing = Surface(IWin)%Tilt/180.0d0

      ! Daylighting shelf simplication:  All light goes up to the ceiling regardless of orientation of shelf
      IF(Surface(IWin)%Shelf > 0) THEN
        IF (Shelf(Surface(IWin)%Shelf)%InSurf > 0) SurfaceWindow(IWin)%FractionUpgoing = 1.0
      END IF
    END IF
  END DO

  RETURN

END SUBROUTINE DayltgAveInteriorReflectance


SUBROUTINE CalcDayltgCoefficients

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   July 1997
          !       MODIFIED       FW, Jan 2002: add variable slat angle blinds
          !                      FW, Mar 2002: add triangular windows
          !                      FW, Oct 2002: remove warning on window discretization relative to
          !                                    reference point distance to window plane
          !                      FW, Jan 2003: add between-glass shades and blinds
          !                      FW, Apr 2003: initialize shading type to 'NOSHADE' in window loop
          !                      PE, May 2003: add light pipes (tubular daylighting devices)
          !                      FW, Jul 2003: account for possible non-zero transmittance of
          !                                    shading surfaces (previously all shading surfaces were
          !                                    assumed to be opaque)
          !                      PE, Aug 2003: add daylighting shelves
          !                      FW, Sep 2003: write the bare-window overcast sky daylight factors to the eio file
          !                      FW, Nov 2003: add exterior beam and sky solar diffuse reflection from obstructions;
          !                                    add beam solar and sky solar reflection from ground with obstructions.
          !                      FW, Nov 2003: change expression for NDIVX, NDIVY (no. of window elements in X,Y) to
          !                                    round up to nearest integer rather than down
          !                      FW, Nov 2003: add specular reflection of beam solar from obstructions
          !                      RJH, Jan 2004: add alternative daylighting analysis using DElight
          !                                     All modifications demarked with RJH (Rob Hitchcock)
          !                      FW, Feb 2004: add daylighting through interior windows
          !                      FW, Apr 2004: add light well efficiency that multiplies glazing transmittance
          !                      FW, Apr 2004: add diffusing glazing
          !                      RJH, Jul 2004: add error handling for warnings/errors returned from DElight
          !                      LKL, Oct 2004: Separate "map" and "ref" point calculations -- move some input routines to
          !                                     separate routines.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates daylighting factors for later use in the time-step loop.

          ! METHODOLOGY EMPLOYED:

          ! For each combination of exterior window and reference point in a zone,
          ! calculates daylighting factors (interior illuminance / exterior illuminance)
          ! and glare factors for clear and overcast skies and for windows with and
          ! without shading devices. These factors are calculated for each hourly
          ! sun position for design days and for selected days throughout the year.

          ! If a target zone has one or more interior windows, also calculates daylighting
          ! factors for the target zone that are associated with exterior windows in adjacent
          ! zones that share interior windows with the target zone.

          ! The daylight illuminance at a reference point from a window is determined
          ! by dividing the window into rectangular elements and calculating the illuminance
          ! reaching the reference point directly from each element. The illumination
          ! from an element can come from the sky or ground if the window is unshaded, or from
          ! a shading device illuminated by solar radiation. Also considered are the
          ! illuminance contribution from interreflection among the zone's interior surfaces
          ! and sunlight striking the reference point.

          ! In calculating sky-related interior illuminance and luminance quantities,
          ! the sky luminance for the different sky types are determined from distributions
          ! in which the zenith luminance is normalized to 1.0 cd/m2. Similarly, sun-related
          ! illuminance and luminance quantities are based on beam normal solar illuminance
          ! normalized to 1.0 lux.
          !
          ! The daylight and glare factors calculated in this subroutine are used in DayltgInteriorIllum
          ! to get the daylight illuminance and glare at each time step.
          ! Based on this information and user-input lighting setpoint and type of lighting
          ! control system, DayltgElecLightingControl then determines how much the overhead eletric lighting
          ! can be reduced.

          ! REFERENCES:
          ! Based on DOE-2.1E subroutine DCOF.

          ! USE STATEMENTS:
  USE General, ONLY: POLYF, InterpProfAng, BlindBeamBeamTrans, RoundSigDigits
  USE DaylightingDevices, ONLY: FindTDDPipe, TransTDD
  USE SolarReflectionManager, ONLY: SolReflRecSurf

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: fmtA='(A)'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER           :: ZoneNum                      ! Zone number
  INTEGER           :: IHR                          ! Hour of day counter
  INTEGER           :: IWin                         ! Window counter
  INTEGER           :: Loop             ! DO loop indices
  LOGICAL, SAVE     :: FirstTime = .TRUE.
  LOGICAL, SAVE     :: FirstTimeDaylFacCalc = .TRUE.
  REAL(r64)         :: DaylFac1 ! sky daylight factor at ref pt 1
  REAL(r64)         :: DaylFac2 ! sky daylight factor at ref pt 2

  ! added for output all daylight factors
  INTEGER, EXTERNAL :: GetNewUnitNumber  ! External  function to "get" a unit number
  INTEGER           :: write_stat
  REAL(r64)         :: DFClrSky1,DFClrTbSky1,DFIntSky1,DFOcSky1,DFClrSky2,DFClrTbSky2,DFIntSky2,DFOcSky2
  REAL(r64)         :: SlatAngle
  INTEGER           :: ISA, ICtrl
  INTEGER           :: ISlatAngle

  LOGICAL, SAVE     :: CreateDFSReportFile = .TRUE.

          ! FLOW:
  IF (FirstTime) THEN
    CALL GetDaylightingParametersInput
    CALL CheckTDDsAndLightShelvesInDaylitZones
    FirstTime = .FALSE.
    IF (ALLOCATED(CheckTDDZone)) DEALLOCATE(CheckTDDZone)
  END IF  ! End of check if FirstTime

  ! Find the total number of exterior windows associated with all Daylighting:Detailed zones.
  ! An exterior window is associated with such a zone if (1) it is an exterior window in the zone, or
  ! (2) it is an exterior window in an adjacent zone that shares an interior window with the zone.
  ! Note that exterior windows in category (2) may be counted more than once if an adjacent zone
  ! is adjacent to more than one daylit zone with which the adjacent zone shares interior windows.
  ! If there are no interior windows in a building, than TotWindowsWithDayl is just the total number of
  ! exterior windows in Daylighting:Detailed zones. Note that it is possible for a
  ! Daylighting:Detailed zone to have zero exterior windows of its own, but it may have an interior
  ! through which daylight passes from adjacent zones with exterior windows.
  IF (BeginSimFlag) THEN
    TotWindowsWithDayl = 0
    DO ZoneNum = 1,NumOfZones
      TotWindowsWithDayl = TotWindowsWithDayl + ZoneDaylight(ZoneNum)%NumOfDayltgExtWins
    END DO
  END IF

  IF (TotWindowsWithDayl == 0) RETURN

  !-----------------------------------------!
  ! Detailed daylighting factor calculation !
  !-----------------------------------------!

  IF (WarmUpFlag) THEN
    CALL DisplayString('Calculating Detailed Daylighting Factors, Start Date='//CurMnDy)
  ELSE
    CALL DisplayString('Updating Detailed Daylighting Factors, Start Date='//CurMnDy)
  END IF


  IF(BeginSimFlag) THEN

  ! Find minimum solid angle subtended by an interior window in Daylighting:Detailed zones.
  ! Used in calculating daylighting through interior windows.
    CALL CalcMinIntWinSolidAngs

    ALLOCATE(TDDTransVisBeam(NumOfTDDPipes,24))
    ALLOCATE(TDDFluxInc(NumOfTDDPipes,4,24))
    ALLOCATE(TDDFluxTrans(NumOfTDDPipes,4,24))

    ! Warning if detailed daylighting has been requested for a zone with no associated exterior windows.
    DO ZoneNum = 1,NumOfZones
      IF(ZoneDaylight(ZoneNum)%TotalDaylRefPoints > 0 .AND. ZoneDaylight(ZoneNum)%NumOfDayltgExtWins == 0) THEN
        CALL ShowWarningError('Detailed daylighting will not be done for zone='//TRIM(Zone(ZoneNum)%Name))
        CALL ShowContinueError('because it has no associated exterior windows.')
      END IF
    END DO

    ! Find area and reflectance quantities used in calculating inter-reflected illuminance.
    DO ZoneNum = 1,NumOfZones
      !TH 9/10/2009. Need to calculate for zones without daylighting controls (TotalDaylRefPoints = 0)
      ! but with adjacent zones having daylighting controls.
      IF((ZoneDaylight(ZoneNum)%TotalDaylRefPoints > 0 .AND. ZoneDaylight(ZoneNum)%NumOfDayltgExtWins > 0) &
        .OR. ZoneDaylight(ZoneNum)%AdjZoneHasDayltgCtrl) THEN
        CALL DayltgAveInteriorReflectance(ZoneNum)
      ENDIF
    END DO

  END IF

  ! Zero daylighting factor arrays
  TDDTransVisBeam = 0.0
  TDDFluxInc = 0.0
  TDDFluxTrans = 0.0

  IF (BeginDayFlag) THEN
    ! Calculate hourly sun angles, clear sky zenith luminance, and exterior horizontal illuminance
    PHSUN=0.0
    SPHSUN=0.0
    CPHSUN=0.0
    THSUN=0.0

    PHSUNHR=0.0
    SPHSUNHR=0.0
    CPHSUNHR=0.0
    THSUNHR=0.0
    GILSK=0.0
    GILSU=0.0
    DO IHR = 1,24
      IF (SUNCOSHR(3,IHR) < SunIsUpValue) CYCLE ! Skip if sun is below horizon
      PHSUN = PIOVR2 - ACOS(SUNCOSHR(3,IHR))
      PHSUNHR(IHR) = PHSUN
      SPHSUNHR(IHR) = SIN(PHSUN)
      CPHSUNHR(IHR) = COS(PHSUN)
      THSUNHR(IHR) = ATAN2(SUNCOSHR(2,IHR),SUNCOSHR(1,IHR))
      ! Get exterior horizontal illuminance from sky and sun
      THSUN=THSUNHR(IHR)
      SPHSUN = SPHSUNHR(IHR)
      CPHSUN = CPHSUNHR(IHR)
      CALL DayltgExtHorizIllum(GILSK(1,IHR),GILSU(IHR))
    END DO
  ENDIF

  !           -----------
  ! ---------- ZONE LOOP ----------
  !           -----------

  DO ZoneNum = 1,NumOfZones
    ! Skip zones that are not Daylighting:Detailed zones.
    ! TotalDaylRefPoints = 0 means zone has (1) no daylighting or
    ! (3) Daylighting:DElight
    IF(ZoneDaylight(ZoneNum)%TotalDaylRefPoints == 0) CYCLE

    ! Skip zones with no exterior windows in the zone or in adjacent zone with which an interior window is shared
    IF(ZoneDaylight(ZoneNum)%NumOfDayltgExtWins == 0) CYCLE

    ZoneDaylight(ZoneNum)%DaylIllFacSky         = 0.0
    ZoneDaylight(ZoneNum)%DaylSourceFacSky      = 0.0
    ZoneDaylight(ZoneNum)%DaylBackFacSky        = 0.0
    ZoneDaylight(ZoneNum)%DaylIllFacSun         = 0.0
    ZoneDaylight(ZoneNum)%DaylIllFacSunDisk     = 0.0
    ZoneDaylight(ZoneNum)%DaylSourceFacSun      = 0.0
    ZoneDaylight(ZoneNum)%DaylSourceFacSunDisk  = 0.0
    ZoneDaylight(ZoneNum)%DaylBackFacSun        = 0.0
    ZoneDaylight(ZoneNum)%DaylBackFacSunDisk    = 0.0

    CALL CalcDayltgCoeffsRefMapPoints(ZoneNum)

  END DO ! End of zone loop, ZoneNum

  IF(FirstTimeDaylFacCalc .AND. TotWindowsWithDayl > 0) THEN
    ! Write the bare-window four sky daylight factors at noon time to the eio file; this is done only
    ! for first time that daylight factors are calculated and so is insensitive to possible variation
    ! due to change in ground reflectance from month to month, or change in storm window status.
    Write(OutputFileInits,700)
    700 Format( &
    '! <Sky Daylight Factors>, MonthAndDay, Zone Name, Window Name, Daylight Fac: Ref Pt #1, Daylight Fac: Ref Pt #2')
    DO ZoneNum = 1, NumOfZones
      IF(ZoneDaylight(ZoneNum)%NumOfDayltgExtWins == 0) CYCLE
      DO loop = 1,ZoneDaylight(ZoneNum)%NumOfDayltgExtWins
        IWin = ZoneDaylight(ZoneNum)%DayltgExtWinSurfNums(loop)
        ! For this report, do not include ext wins in zone adjacent to ZoneNum since the inter-reflected
        ! component will not be calculated for these windows until the time-step loop.
        IF(Surface(IWin)%Zone == ZoneNum) THEN
            ! clear sky
            DaylFac1 = ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,1,1,1,12)
            DaylFac2 = 0.0
            IF(ZoneDaylight(ZoneNum)%TotalDaylRefPoints > 1) DaylFac2 = ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,2,1,1,12)
            Write(OutputFileInits,701) ' Clear Sky Daylight Factors,',   &
              trim(CurMnDy),TRIM(Zone(ZoneNum)%Name),TRIM(Surface(IWin)%Name),  &
              trim(RoundSigDigits(DaylFac1,4)),trim(RoundSigDigits(DaylFac2,4))

            ! clear Turbid sky
            DaylFac1 = ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,1,2,1,12)
            DaylFac2 = 0.0
            IF(ZoneDaylight(ZoneNum)%TotalDaylRefPoints > 1) DaylFac2 = ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,2,2,1,12)
            Write(OutputFileInits,701) ' Clear Turbid Sky Daylight Factors,',   &
              trim(CurMnDy),TRIM(Zone(ZoneNum)%Name),TRIM(Surface(IWin)%Name),  &
              trim(RoundSigDigits(DaylFac1,4)),trim(RoundSigDigits(DaylFac2,4))

            ! Intermediate sky
            DaylFac1 = ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,1,3,1,12)
            DaylFac2 = 0.0
            IF(ZoneDaylight(ZoneNum)%TotalDaylRefPoints > 1) DaylFac2 = ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,2,3,1,12)
            Write(OutputFileInits,701) ' Intermediate Sky Daylight Factors,',   &
              trim(CurMnDy),TRIM(Zone(ZoneNum)%Name),TRIM(Surface(IWin)%Name),  &
              trim(RoundSigDigits(DaylFac1,4)),trim(RoundSigDigits(DaylFac2,4))

            ! Overcast sky
            DaylFac1 = ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,1,4,1,12)
            DaylFac2 = 0.0
            IF(ZoneDaylight(ZoneNum)%TotalDaylRefPoints > 1) DaylFac2 = ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,2,4,1,12)
            Write(OutputFileInits,701) ' Overcast Sky Daylight Factors,',   &
              trim(CurMnDy),TRIM(Zone(ZoneNum)%Name),TRIM(Surface(IWin)%Name),  &
              trim(RoundSigDigits(DaylFac1,4)),trim(RoundSigDigits(DaylFac2,4))
            701 Format(A,A,',',A,',',A,',',A,',',A)
        END IF
      END DO
    END DO
    FirstTimeDaylFacCalc = .FALSE.
  END IF


  ! TH 7/2010 report all daylight factors for the two reference points of daylight zones ...

  ! Skip if no daylight windows
  IF(TotWindowsWithDayl == 0) RETURN

  ! Skip if no request of reporting
  IF((.NOT. DFSReportSizingDays) .AND. (.NOT. DFSReportAllShadowCalculationDays)) RETURN

  ! Skip duplicate calls
  IF (DoingSizing) RETURN
  IF (KickOffSimulation) RETURN

  IF (DFSReportSizingDays) THEN
    IF (DoWeathSim .AND. DoDesDaySim) THEN
      IF (KindOfSim == ksRunPeriodWeather) RETURN
    ENDIF
  ENDIF

  IF (DFSReportAllShadowCalculationDays) THEN
    IF (KindOfSim /= ksRunPeriodWeather) RETURN
  ENDIF

  ! open a new file eplusout.dfs for saving the daylight factors
  IF (CreateDFSReportFile) THEN
    OutputFileDFS = GetNewUnitNumber()
    OPEN(OutputFileDFS,FILE='eplusout.dfs', Action='write',iostat=write_stat)
    IF (write_stat /= 0) THEN
     CALL ShowFatalError('CalcDayltgCoefficients: Could not open file "eplusout.dfs" for output (write).')
    ELSE
      Write(OutputFileDFS,fmtA) 'This file contains daylight factors for all exterior windows of daylight zones.'
      Write(OutputFileDFS,fmtA) 'If only one reference point the last 4 columns in the data will be zero.'
      Write(OutputFileDFS,fmtA) 'MonthAndDay,Zone Name,Window Name,Window State'
      Write(OutputFileDFS,fmtA) 'Hour,Daylight Factor for Clear Sky at Reference point 1,'//  &
          'Daylight Factor for Clear Turbid Sky at Reference point 1,Daylight Factor for Intermediate Sky at Reference point 1,'// &
          'Daylight Factor for Overcast Sky at Reference point 1,Daylight Factor for Clear Sky at Reference point 2,'// &
          'Daylight Factor for Clear Turbid Sky at Reference point 2,Daylight Factor for Intermediate Sky at Reference point 2,'// &
          'Daylight Factor for Overcast Sky at Reference point 2'
    ENDIF
    CreateDFSReportFile = .false.
  ENDIF

    DO ZoneNum = 1, NumOfZones
      IF(ZoneDaylight(ZoneNum)%NumOfDayltgExtWins == 0) CYCLE

      DO loop = 1,ZoneDaylight(ZoneNum)%NumOfDayltgExtWins
        IWin = ZoneDaylight(ZoneNum)%DayltgExtWinSurfNums(loop)
        ICtrl = Surface(IWin)%WindowShadingControlPtr

        ! For this report, do not include ext wins in zone adjacent to ZoneNum since the inter-reflected
        ! component will not be calculated for these windows until the time-step loop.
        IF(Surface(IWin)%Zone == ZoneNum) THEN

          IF (SurfaceWindow(IWin)%MovableSlats) THEN
            ! variable slat angle - MaxSlatangle sets
            ISA = MaxSlatAngs + 1
          ELSEIF (ICtrl > 0) THEN
            ! window shade or blind with fixed slat angle
            ISA = 2
          ELSE
            ! base window
            ISA = 1
          ENDIF

          ! loop over each slat angle
          DO ISlatAngle = 1, ISA
            IF (ISlatAngle == 1) THEN
              ! base window without shades, screens, or blinds
              Write(OutputFileDFS,801) trim(CurMnDy), TRIM(Zone(ZoneNum)%Name), TRIM(Surface(IWin)%Name), 'Base Window'
            ELSEIF (ISlatAngle == 2 .AND. ISA == 2) THEN
              ! window shade or blind with fixed slat angle
              Write(OutputFileDFS,801) trim(CurMnDy), TRIM(Zone(ZoneNum)%Name), TRIM(Surface(IWin)%Name), ' '
            ELSE
              ! blind with variable slat angle
              SlatAngle = 180.d0/real((MaxSlatAngs - 1),r64) * real((ISlatAngle - 2),r64)
              Write(OutputFileDFS,801) trim(CurMnDy), TRIM(Zone(ZoneNum)%Name), TRIM(Surface(IWin)%Name),   &
                 trim(RoundSigDigits(SlatAngle,1))
            ENDIF
            801 Format(A,',', A, ',', A, ',',A)

            DO IHR = 1, 24
              ! daylight reference point 1
              DFClrSky1 = ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,1,1,ISlatAngle,IHR)     ! clear sky
              DFClrTbSky1 = ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,1,2,ISlatAngle,IHR)   ! clear Turbid sky
              DFIntSky1 = ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,1,3,ISlatAngle,IHR)     ! Intermediate sky
              DFOcSky1 = ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,1,4,ISlatAngle,IHR)      ! Overcast sky

              ! daylight reference point 2
              IF(ZoneDaylight(ZoneNum)%TotalDaylRefPoints > 1) THEN
                DFClrSky2 = ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,2,1,ISlatAngle,IHR)
                DFClrTbSky2 = ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,2,2,ISlatAngle,IHR)
                DFIntSky2 = ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,2,3,ISlatAngle,IHR)
                DFOcSky2 = ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,2,4,ISlatAngle,IHR)
              ELSE
                DFClrSky2 = 0.0
                DFClrTbSky2 = 0.0
                DFIntSky2 = 0.0
                DFOcSky2 = 0.0
              ENDIF

              ! write daylight factors - 4 sky types for each daylight ref point
              Write(OutputFileDFS,803) trim(RoundSigDigits(IHR)),  &
                 trim(RoundSigDigits(DFClrSky1,5)),trim(RoundSigDigits(DFClrTbSky1,5)),  &
                 trim(RoundSigDigits(DFIntSky1,5)),trim(RoundSigDigits(DFOcSky1,5)),  &
                 trim(RoundSigDigits(DFClrSky2,5)),trim(RoundSigDigits(DFClrTbSky2,5)),  &
                 trim(RoundSigDigits(DFIntSky2,5)),trim(RoundSigDigits(DFOcSky2,5))
              803 Format(A,8(',',A))
            END DO
          END DO
        END IF
      END DO
    END DO

  RETURN

END SUBROUTINE CalcDayltgCoefficients

SUBROUTINE CalcDayltgCoeffsRefMapPoints(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2004
          !       MODIFIED       May 2006 (RR): added exterior window screens
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine does the daylighting coefficient calculation for the
          ! daylighting and illuminance map reference points.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: POLYF, InterpProfAng, BlindBeamBeamTrans, SafeDivide, RoundSigDigits
  USE DaylightingDevices, ONLY: FindTDDPipe, TransTDD
  USE SolarReflectionManager, ONLY: SolReflRecSurf
  USE Vectors
  USE DataSystemVariables, ONLY: DetailedSkyDiffuseAlgorithm

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ZoneNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: tmpDFCalc = 0.05d0             ! cut out illuminance (lux) for exterior horizontal in calculating
                                            ! the daylighting and glare factors

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: W1(3)                        ! First vertex of window (where vertices are numbered
                                            ! counter-clockwise starting at upper left as viewed
                                            ! from inside of room
  REAL(r64) :: W2(3)                        ! Second vertex of window
  REAL(r64) :: W3(3)                        ! Third vertex of window
  REAL(r64) :: U1(3)                        ! First vertex of window for TDD:DOME (if exists)
  REAL(r64) :: U2(3)                        ! Second vertex of window for TDD:DOME (if exists)
  REAL(r64) :: U3(3)                        ! Third vertex of window for TDD:DOME (if exists)
  REAL(r64) :: WC(3)                        ! Center point of window
  REAL(r64) :: RREF(3)                      ! Location of a reference point in absolute coordinate system
  REAL(r64) :: RREF2(3)                     ! Location of virtual reference point in absolute coordinate system
  REAL(r64) :: RWIN(3)                      ! Center of a window element in absolute coordinate system
  REAL(r64) :: RWIN2(3)                     ! Center of a window element for TDD:DOME (if exists) in abs coord sys
  REAL(r64) :: RAY(3)                       ! Unit vector along ray from reference point to window element
  REAL(r64) :: W21(3)                       ! Vector from window vertex 2 to window vertex 1
  REAL(r64) :: W23(3)                       ! Vector from window vertex 2 to window vertex 3
  REAL(r64) :: U21(3)                       ! Vector from window vertex 2 to window vertex 1 for TDD:DOME (if exists)
  REAL(r64) :: U23(3)                       ! Vector from window vertex 2 to window vertex 3 for TDD:DOME (if exists)
  REAL(r64) :: REFWC(3)                     ! Vector from reference point to center of window
  REAL(r64) :: REFD(3)                      ! Vector from ref pt to center of win in TDD:DIFFUSER coord sys (if exists)
  REAL(r64) :: WNORM(3)                     ! Unit vector normal to window (pointing away from room)
  REAL(r64) :: WNORM2(3)                    ! Unit vector normal to TDD:DOME (if exists)
  REAL(r64) :: W2REF(3)                     ! Vector from window origin to project of ref. pt. on window plane
  REAL(r64) :: VIEWVC(3)                    ! View vector in absolute coordinate system
  REAL(r64) :: VIEWVC2(3)                   ! Virtual view vector in absolute coordinate system
  REAL(r64) :: VIEWVD(3)                    ! Virtual view vector in TDD:DIFFUSER coord sys (if exists)
  REAL(r64) :: ZF(2)                        ! Fraction of zone controlled by each reference point
                                            !  In the following four variables, I=1 for clear sky, 2 for overcast.
  REAL(r64) :: XEDIRSK(4)                   ! Illuminance contribution from luminance element, sky-related
  REAL(r64) :: XEDIRSU                      ! Illuminance contribution from luminance element, sun-related
  REAL(r64) :: XAVWLSK(4)                   ! Luminance of window element, sky-related
                                            !  In the following I,J,K or J,K arrays, I=1 for clear sky, 2 for clear turbid,
                                            !  3 for intermediate, 4 for overcast;
                                            !  J=1 for bare window, 2 for window shade or blind with fixed slat ang,
                                            !  or 2 to MaxSlatAngs+1 for blind with movable slats; K = sun position index.
  REAL(r64) :: EDIRSK(4,MaxSlatAngs+1,24)   ! Sky-related component of direct illuminance
  REAL(r64) :: EDIRSU(MaxSlatAngs+1,24)     ! Sun-related component of direct illuminance (excluding beam solar at ref pt)
  REAL(r64) :: EDIRSUdisk(MaxSlatAngs+1,24) ! Sun-related component of direct illuminance due to beam solar at ref pt
  REAL(r64) :: AVWLSK(4,MaxSlatAngs+1,24)   ! Sky-related average window luminance
  REAL(r64) :: AVWLSU(MaxSlatAngs+1,24)     ! Sun-related average window luminance, excluding view of solar disk
  REAL(r64) :: AVWLSUdisk(MaxSlatAngs+1,24) ! Sun-related average window luminance due to view of solar disk
  REAL(r64) :: RAYCOS(3)                    ! Unit vector from reference point to sun
  INTEGER   :: IHR                          ! Hour of day counter
  INTEGER   :: NRF                          ! Number of daylighting reference points in a zone
  INTEGER   :: IL                           ! Reference point counter
  REAL(r64) :: AZVIEW                       ! Azimuth of view vector in absolute coord system for
                                            !  glare calculation (radians)
  INTEGER   :: IConst                       ! Construction counter
  INTEGER   :: ICtrl                        ! Window control counter
  INTEGER   :: IWin                         ! Window counter
  INTEGER   :: IWin2                        ! Secondary window counter (for TDD:DOME object, if exists)
  INTEGER   :: PipeNum                      ! TDD pipe object number
  INTEGER   :: ShelfNum                     ! Daylighting shelf object number
  INTEGER   :: InShelfSurf                  ! Inside daylighting shelf surface number
  INTEGER   :: BlNum                        ! Window blind number
  INTEGER   :: ScNum                        ! Window screen number
  INTEGER   :: JB                           ! Slat angle counter
!  CHARACTER(len=30) :: ShType                       ! Window shading type
  INTEGER   :: ShType                       ! Window shading type
  REAL(r64) :: TransBmBmMult(MaxSlatAngs)   ! Beam-beam transmittance of isolated blind
  REAL(r64) :: TransBmBmMultRefl(MaxSlatAngs) ! As above but for beam reflected from exterior obstruction
  REAL(r64) :: ProfAng                      ! Solar profile angle on a window (radians)
  INTEGER   :: IConstShaded                 ! Shaded construction counter
  REAL(r64) :: WW                           ! Window width (m)
  REAL(r64) :: HW                           ! Window height (m)
  INTEGER   :: LSHCAL                       ! Interior shade calculation flag: 0=not yet
                                            !  calculated, 1=already calculated
  INTEGER   :: NDIVX                        ! Number of window x divisions for daylighting calc
  INTEGER   :: NDIVY                        ! Number of window y divisions for daylighting calc
  REAL(r64) :: ALF                          ! Distance from reference point to window plane (m)
  REAL(r64) :: D1a                          ! Projection of vector from window origin to reference
                                            !  on window X  axis (m)
  REAL(r64) :: D1b                          ! Projection of vector from window origin to reference
                                            !  on window Y axis (m)
  INTEGER   :: NWX                          ! Number of window elements in x direction for dayltg calc
  INTEGER   :: NWY                          ! Number of window elements in y direction for dayltg calc
  INTEGER   :: NWYlim                       ! For triangle, largest NWY for a given IX
  REAL(r64) :: DWX                          ! Horizontal dimension of window element (m)
  REAL(r64) :: DWY                          ! Vertical dimension of window element (m)
  INTEGER   :: IX                           ! Counter for window elements in the x direction
  INTEGER   :: IY                           ! Counter for window elements in the y direction
  REAL(r64) :: DIS                          ! Distance between reference point and center of window element (m)
  REAL(r64) :: COSB                         ! Cosine of angle between window outward normal and ray from
                                            !  reference point to window element
  REAL(r64) :: PHRAY                        ! Altitude of ray from reference point to window element (radians)
  REAL(r64) :: THRAY                        ! Azimuth of ray from reference point to window element (radians)
  REAL(r64) :: DOMEGA                       ! Solid angle subtended by window element wrt reference point (steradians)
  REAL(r64) :: POSFAC                       ! Position factor for a window element / ref point / view vector combination
  REAL(r64) :: RR                           ! Distance from ref point to intersection of view vector
                                            !  and plane normal to view vector and window element (m)
  REAL(r64) :: ASQ                          ! Square of distance from above intersection to window element (m2)
  REAL(r64) :: YD                           ! Vertical displacement of window element wrt ref point
  REAL(r64) :: XR                           ! Horizontal displacement ratio
  REAL(r64) :: YR                           ! Vertical displacement ratio
  REAL(r64) :: TVISB                        ! Visible transmittance of window for COSB angle of incidence (times light well
                                            !   efficiency, if appropriate)
  INTEGER   :: ISunPos                      ! Sun position counter; used to avoid calculating various
                                            !  quantities that do not depend on sun position.
  INTEGER   :: IHIT                         ! Hit flag; =1 if ray from ref point thru window element hits
                                            !  an obstruction, =0 otherwise.
  INTEGER   :: IHitIntObs                   ! = 1 if interior obstruction hit, = 0 otherwise
  REAL(r64) :: ObTrans                      ! Product of solar transmittances of exterior obstructions hit by ray
                                            ! from reference point through a window element
  REAL(r64) :: ObTransDisk                  ! Product of solar transmittances of exterior obstructions hit by ray
                                            ! from reference point to sun
  REAL(r64) :: HP(3)                        ! Hit coordinates, if ray hits
  REAL(r64) :: LumAtHitPtFrSun              ! Luminance at hit point of obstruction by reflection of direct light from
                                            !  sun (cd/m2)
  INTEGER   :: ISky                         ! Sky type index: 1=clear, 2=clear turbid, 3=intermediate, 4=overcast
  INTEGER   :: JSH                          ! Shading index: J=1 is unshaded window, J=2 is shaded window
  REAL(r64) :: ELUM                         ! Sky or ground luminance (cd/m2)
  REAL(r64) :: DEDIR                        ! Illuminance contribution at reference point from window element (lux)
  REAL(r64) :: COSI                         ! Cosine of angle between direct sun and window outward normal
  INTEGER   :: IP                           ! IP=1 if ray passes thru window, =0 if not
  REAL(r64) :: TVISS                        ! Direct solar visible transmittance of window at given angle of incidence
                                            !  (times light well efficiency, if appropriate)
  REAL(r64) :: XAVWL                        ! XAVWL*TVISS is contribution of window luminance from solar disk (cd/m2)
  REAL(r64) :: VTR                          ! For switchable glazing, ratio of visible transmittance of
                                            !  fully-switched state to that of the unswitched state
  REAL(r64) :: SlatAng                      ! Blind slat angle (rad)
  INTEGER   :: Loop,Loop2                   ! DO loop indices
  INTEGER   :: loopwin                      ! loop index for exterior windows associated with a daylit zone
  LOGICAL   :: Rectangle                    ! True if window is rectangular
  LOGICAL   :: Triangle                     ! True if window is triangular
  REAL(r64) :: DAXY                         ! Area of window element
  REAL(r64) :: DAXY1                        ! For triangle, area of window element at end of column
  REAL(r64) :: SinCornerAng                 ! For triangle, sine of corner angle of window element
  INTEGER   :: NearestHitSurfNum            ! Surface number of nearest obstruction
  INTEGER   :: NearestHitSurfNumX           ! Surface number to use when obstruction is a shadowing surface
  REAL(r64) :: NearestHitPt(3)              ! Hit point of ray on nearest obstruction
  REAL(r64) :: SunObstructionMult           ! = 1.0 if sun hits a ground point; otherwise = 0.0
  REAL(r64) :: SkyObstructionMult           ! Ratio of obstructed to unobstructed sky diffuse at a ground point
  REAL(r64) :: HorDis                       ! Distance between ground hit point and proj'n of center
                                            !  of window element onto ground (m)
  REAL(r64) :: Alfa,Beta                    ! Intermediate variables
  REAL(r64) :: GroundHitPt(3)               ! Coordinates of point that ray hits ground (m)
  INTEGER   :: IHitObs                      ! 1 if obstruction is hit; 0 otherwise
  REAL(r64) :: ObsHitPt(3)                  ! Coordinates of hit point on an obstruction (m)
  INTEGER   :: ObsSurfNum                   ! Surface number of obstruction
  INTEGER   :: ObsConstrNum                 ! Construction number of obstruction
  REAL(r64) :: ObsVisRefl                   ! Visible reflectance of obstruction
  REAL(r64) :: SkyReflVisLum                ! Reflected sky luminance at hit point divided by
  REAL(r64) :: SkyGndUnObs                  ! Unobstructed sky irradiance at a ground point
  REAL(r64) :: SkyGndObs                    ! Obstructed sky irradiance at a ground point
  REAL(r64) :: Phi,Theta                    ! Altitude and azimuth angle of ray from a ground point (radians)
  INTEGER   :: IPhi,ITheta                  ! Phi and Theta indices
  REAL(r64) :: DPhi,DTheta                  ! Phi and Theta increment (radians)
  REAL(r64) :: SPhi,CPhi                    ! Sin and cos of Phi
  REAL(r64) :: dOmegaGnd                    ! Solid angle element of ray from ground point (steradians)
  REAL(r64) :: URay(3)                      ! Unit vector in (Phi,Theta) direction
  REAL(r64) :: CosIncAngURay                ! Cosine of incidence angle of URay on ground plane
  REAL(r64) :: IncAngSolidAngFac            ! CosIncAngURay*dOmegaGnd/Pi
  INTEGER   :: RecSurfNum                   ! Receiving surface number
  INTEGER   :: ReflSurfNum,ReflSurfNumX     ! Reflecting surface number
  REAL(r64) :: ReflNorm(3)                  ! Normal vector to reflecting surface
  REAL(r64) :: CosIncAngRefl                ! Cos of angle of incidence of beam on reflecting surface
  REAL(r64) :: SunVecMir(3)                 ! Sun ray mirrored in reflecting surface
  REAL(r64) :: CosIncAngRec                 ! Cos of angle of incidence of reflected beam on receiving window
  INTEGER   :: IHitRefl                     ! 1 if ray hits reflecting surface; 0 otherwise
  REAL(r64) :: HitPtRefl(3)                 ! Point that ray hits reflecting surface
  REAL(r64) :: ReflDistance                 ! Distance between ref pt and hit point on reflecting surf (m)
  INTEGER   :: IHitObsRefl                  ! > 0 if obstruction hit between ref pt and reflection point
  REAL(r64) :: HitPtObs(3)                  ! Hit point on obstruction
  REAL(r64) :: ObsDistance                  ! Distance from ref pt to reflection point
  INTEGER   :: ReflSurfRecNum               ! Receiving surface number for a reflecting window
  REAL(r64) :: SpecReflectance              ! Specular reflectance of a reflecting surface
  REAL(r64) :: TVisRefl                     ! Bare window vis trans for reflected beam
                                            !  (times light well efficiency, if appropriate)
  INTEGER   :: ConstrNumRefl                ! Window construction number for a specularly reflecting shading surf
  REAL(r64) :: PHSUNrefl                    ! Altitude angle of reflected sun (radians)
  REAL(r64) :: THSUNrefl                    ! Azimuth anggle of reflected sun (radians)
  INTEGER   :: ExtWinType                   ! Exterior window type (InZoneExtWin, AdjZoneExtWin, NotInOrAdjZoneExtWin)
  REAL(r64) :: SolidAngExtWin               ! Approx. solid angle subtended by an ext. window wrt ref pt
  REAL(r64) :: SolidAngMinIntWin            ! Approx. smallest solid angle subtended by an int. window wrt ref pt
  REAL(r64) :: SolidAngRatio                ! Ratio of SolidAngExtWin and SolidAngMinIntWin
  INTEGER   :: ZoneNumThisWin               ! A window's zone number
  INTEGER   :: IntWin                       ! Interior window surface index
  INTEGER   :: IntWinHitNum                 ! Surface number of interior window that is intersected
  INTEGER   :: IHitIntWin                   ! Ray from ref pt passes through interior window
  INTEGER   :: IHitExtObs                   ! 1 if ray from ref pt to ext win hits an exterior obstruction
  REAL(r64) :: HitPtIntWin(3)               ! Intersection point on an interior window for ray from ref pt to ext win (m)
  REAL(r64) :: TVISIntWin                   ! Visible transmittance of int win at COSBIntWin for light from ext win

  INTEGER   :: IHitIntWinDisk               ! 1 if ray from ref pt to sun passes thru an int window; 0 otherwise
  INTEGER   :: IHitIntObsDisk               ! 1 if ray from ref pt to sun hits an interior obstruction; 0 otherwise
  INTEGER   :: IHitExtObsDisk               ! 1 if ray from ref pt to sun hits an exterior obstruction; 0 otherwise

  INTEGER   :: IntWinDisk                   ! Surface loop index for finding int windows betw ref pt and sun
  REAL(r64) :: HitPtIntWinDisk(3)           ! Intersection point on an interior window for ray from ref pt to sun (m)
  INTEGER   :: IntWinDiskHitNum             ! Surface number of int window intersected by ray betw ref pt and sun
  REAL(r64) :: COSBIntWin                   ! Cos of angle between int win outward normal and ray betw ref pt and
                                            !  exterior window element or between ref pt and sun
  REAL(r64) :: TVISIntWinDisk               ! Visible transmittance of int win at COSBIntWin for sun
  REAL(r64) :: TVisIntWinMult               ! Interior window vis trans multiplier for ext win in adjacent zone
  REAL(r64) :: TVisIntWinDiskMult           ! Interior window vis trans solar disk multiplier for ext win in adj zone
  INTEGER   :: CalcLoop                     ! 1=Ref points, 2=Map points
  LOGICAL   :: DoingRefs                    ! True when doing "refs".
  INTEGER   :: EndLoop                      ! When doing sizing, only do first loop
  INTEGER   :: BRef
  INTEGER   :: ILB
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: MapWindowSolidAngAtRefPt
  REAL(r64), ALLOCATABLE, DIMENSION(:,:) :: MapWindowSolidAngAtRefPtWtd
  LOGICAL, SAVE     :: mapFirstTime=.true.
  LOGICAL, SAVE     :: refFirstTime=.true.
  LOGICAL, SAVE     :: VeryFirstTime=.true.
  INTEGER TZoneNum
  LOGICAL ErrorsFound

  IF (VeryFirstTime) THEN
    ! make sure all necessary surfaces match to pipes
    ErrorsFound=.false.
    DO TZoneNum=1,NumOfZones
      DO loopwin = 1,ZoneDaylight(TZoneNum)%NumOfDayltgExtWins
        IWin = ZoneDaylight(TZoneNum)%DayltgExtWinSurfNums(loopwin)
        IF (SurfaceWindow(IWin)%OriginalClass /= SurfaceClass_TDD_Diffuser) CYCLE
        ! Look up the TDD:DOME object
        PipeNum = FindTDDPipe(IWin)
        IF (PipeNum == 0) THEN
          CALL ShowSevereError('GetTDDInput: Surface='//TRIM(Surface(IWin)%Name)//  &
             ', TDD:Dome object does not reference a valid Diffuser object.')
          CALL ShowContinueError('...needs DaylightingDevice:Tubular of same name as Surface.')
          ErrorsFound=.true.
        ENDIF
      ENDDO
    ENDDO

    IF (ErrorsFound) THEN
      CALL ShowFatalError('Not all TubularDaylightDome objects have corresponding DaylightingDevice:Tubular objects.'//  &
        ' Program terminates.')
    ENDIF
    VeryFirstTime=.false.
  ENDIF

  IF (mapFirstTime .and. ANY(ZoneDaylight%TotalMapRefPoints > 0) ) THEN
    IL=-999
    DO CalcLoop=1,NumOfZones
      IL=MAX(IL,ZoneDaylight(CalcLoop)%TotalMapRefPoints)
    ENDDO
    ALLOCATE(MapErrIndex(TotSurfaces,IL))
    MapErrIndex=0
    mapFirstTime=.false.
  ENDIF

  IF (refFirstTime .and. ANY(ZoneDaylight%TotalDaylRefPoints > 0) ) THEN
    IL=-999
    DO CalcLoop=1,NumOfZones
      IL=MAX(IWin,ZoneDaylight(CalcLoop)%TotalDaylRefPoints)
    ENDDO
    ALLOCATE(RefErrIndex(TotSurfaces,IL))
    RefErrIndex=0
    refFirstTime=.false.
  ENDIF

  ! Azimuth of view vector in absolute coord sys
  AZVIEW = (ZoneDaylight(ZoneNum)%ViewAzimuthForGlare + Zone(ZoneNum)%RelNorth + BuildingAzimuth + BuildingRotationAppendixG)  &
               * DegToRadians
  ! View vector components in absolute coord sys
  VIEWVC(1) = SIN(AZVIEW)
  VIEWVC(2) = COS(AZVIEW)
  VIEWVC(3) = 0.

  IF (.not. DoingSizing) THEN
    !Calc for daylighting reference points and illuminance map
    EndLoop=2
  ELSE
    !Calc for daylighting reference points only
    EndLoop=1
  ENDIF

  DO CalcLoop = 1, EndLoop

    !           ---------------------
    ! ---------- REFERENCE POINT LOOP ----------
    !           ---------------------

    DoingRefs=(CalcLoop == 1)

    IF (DoingRefs) THEN
      NRF = ZoneDaylight(ZoneNum)%TotalDaylRefPoints
      ZF=0.0
      ZF(1:NRF) = ZoneDaylight(ZoneNum)%FracZoneDaylit(1:NRF)
      BRef=0
    ELSE
      NRF = ZoneDaylight(ZoneNum)%TotalMapRefPoints
      ZF=0.
      BRef=2
    ENDIF

    DO IL = 1,NRF

      IF (DoingRefs) THEN
        ! Reference point in absolute coordinate system
        RREF(1:3) = ZoneDaylight(ZoneNum)%DaylRefPtAbsCoord(IL,1:3) ! (x, y, z)
      ELSE
        RREF(1:3) = ZoneDaylight(ZoneNum)%MapRefPtAbsCoord(IL,1:3) ! (x, y, z)
      ENDIF

        !           -------------
        ! ---------- WINDOW LOOP ----------
        !           -------------

        IF (.not. DoingRefs) THEN
          ALLOCATE(MapWindowSolidAngAtRefPt(ZoneDaylight(ZoneNum)%NumOfDayltgExtWins,NRF))
          MapWindowSolidAngAtRefPt=0.0
          ALLOCATE(MapWindowSolidAngAtRefPtWtd(ZoneDaylight(ZoneNum)%NumOfDayltgExtWins,NRF))
          MapWindowSolidAngAtRefPtWtd=0.0
        ENDIF

        DO loopwin = 1,ZoneDaylight(ZoneNum)%NumOfDayltgExtWins
          IWin = ZoneDaylight(ZoneNum)%DayltgExtWinSurfNums(loopwin)

          IF (DoingRefs) THEN
            ZoneDaylight(ZoneNum)%SolidAngAtRefPt(IL,loopwin) = 0.0
            ZoneDaylight(ZoneNum)%SolidAngAtRefPtWtd(IL,loopwin) = 0.0
          ELSE
            ZoneDaylight(ZoneNum)%SolidAngAtMapPt(IL,loopwin) = 0.0
            ZoneDaylight(ZoneNum)%SolidAngAtMapPtWtd(IL,loopwin) = 0.0
          ENDIF

          ZoneNumThisWin = Surface(Surface(IWin)%BaseSurf)%Zone
          IF(ZoneNumThisWin == ZoneNum) THEN
            ExtWinType = InZoneExtWin
          ELSE
            ExtWinType = AdjZoneExtWin
          END IF

          IConst = Surface(IWin)%Construction
          IF(SurfaceWindow(IWin)%StormWinFlag == 1) IConst = Surface(IWin)%StormWinConstruction

          ! TH Added 6/29/2009.
          ! For thermochromic windows, the daylight and glare factros are calculated for a base window cosntruction
          !  at base TC layer temperature. During each time step calculations at DayltgInteriorIllum,
          !  DayltgInteriorMapIllum, and DayltgGlare, the daylight and glare factors are adjusted by the visible
          !  transmittance ratio = VT of actual TC window based on last hour TC layer temperature / VT of the base TC window
          IF (Construct(IConst)%TCFlag == 1) THEN
            ! For thermochromic windows, use the base window construction at base temperature of the TC layer
            IConst = Construct(IConst)%TCMasterConst
          ENDIF

          ICtrl = Surface(IWin)%WindowShadingControlPtr
          ShType = WSC_ST_NoShade ! 'NOSHADE'
          IF (ICtrl > 0) ShType = WindowShadingControl(ICtrl)%ShadingType
          BlNum = SurfaceWindow(IWin)%BlindNumber
          ScNum = SurfaceWindow(IWin)%ScreenNumber

          ShelfNum = Surface(IWin)%Shelf
          IF (ShelfNum > 0) THEN
            InShelfSurf = Shelf(Surface(IWin)%Shelf)%InSurf ! Inside daylighting shelf present if > 0
          ELSE
            InShelfSurf = 0
          END IF

          Rectangle = .FALSE.
          Triangle = .FALSE.
          IF (Surface(IWin)%Sides == 3) Triangle = .TRUE.
          IF (Surface(IWin)%Sides == 4) Rectangle = .TRUE.

          IF (Rectangle) THEN
            ! Vertices of window (numbered counter-clockwise starting at upper left as viewed
            ! from inside of room). Assumes original vertices are numbered counter-clockwise from
            ! upper left as viewed from outside.
            W3 = Surface(IWin)%Vertex(2)
            W2 = Surface(IWin)%Vertex(3)
            W1 = Surface(IWin)%Vertex(4)
          ELSE IF (Triangle) THEN
            W3 = Surface(IWin)%Vertex(2)
            W2 = Surface(IWin)%Vertex(3)
            W1 = Surface(IWin)%Vertex(1)
          END IF

          ! Shade/blind calculation flag
          LSHCAL = 0

          ! Visible transmittance at normal incidence
          SurfaceWindow(IWin)%VisTransSelected = POLYF(1.0d0,Construct(IConst)%TransVisBeamCoef(1))* &
                                                 SurfaceWindow(IWin)%GlazedFrac
          ! For windows with switchable glazing, ratio of visible transmittance at normal
          ! incidence for fully switched (dark) state to that of unswitched state
          SurfaceWindow(IWin)%VisTransRatio = 1.0
          IF (ICtrl > 0) THEN
            IF (ShType == WSC_ST_SwitchableGlazing) THEN
              IConstShaded = Surface(IWin)%ShadedConstruction
              SurfaceWindow(IWin)%VisTransRatio = POLYF(1.0d0,Construct(IConstShaded)%TransVisBeamCoef(1)) / &
              POLYF(1.0d0,Construct(IConst)%TransVisBeamCoef(1))
            END IF
          END IF

          ! Unit vectors from window vertex 2 to 1 and 2 to 3,
          ! center point of window, and vector from ref pt to center of window
          W21 = W1 - W2
          W23 = W3 - W2
          HW = SQRT(DOT_PRODUCT(W21,W21))
          WW = SQRT(DOT_PRODUCT(W23,W23))
          IF (Rectangle) THEN
            WC  = W2 + (W23 + W21) / 2.d0
          ELSE IF (Triangle) THEN
            WC  = W2 + (W23 + W21) / 3.d0
          END IF
          SurfaceWindow(IWin)%WinCenter = WC
          REFWC = WC - RREF
          ! Unit vectors
          W21 = W21/HW
          W23 = W23/WW

          ! Unit vector normal to window (pointing away from room)
          WNORM = Surface(IWin)%lcsz

          ! Initialize number of window elements
          NDIVX = 40
          NDIVY = 40

          ! Distance from ref point to window plane
          ALF = ABS(DOT_PRODUCT(WNORM, REFWC))

          ! Check if ref point to close to window due to input error (0.1524 m below is 0.5 ft)
          IF (ALF < 0.1524d0 .AND. ExtWinType == InZoneExtWin) THEN
            ! Ref pt is close to window plane. Get vector from window
            ! origin to projection of ref pt on window plane.
            W2REF = RREF + ALF * WNORM - W2

            D1a = DOT_PRODUCT(W2REF, W23)
!            IF (D1 < 0.0 .OR. D1 > WW) GO TO 4254
            D1b = DOT_PRODUCT(W2REF, W21)
!            IF (D1 < 0.0 .OR. D1 > HW) GO TO 4254

!            ! Error message if ref pt is too close to window.
            IF (D1a > 0.0 .and. D1b > 0.0 .and. D1b <= HW .and. D1a <= WW) THEN
              IF (DoingRefs) THEN
                CALL ShowSevereError('Daylighting calculation cannot be done for zone '//TRIM(Zone(ZoneNum)%Name)// &
                ' because reference point #'//TRIM(RoundSigDigits(IL))//' is less than 0.15m (6") from window plane '//  &
                 TRIM(Surface(IWin)%Name))
                CALL ShowContinueError('Distance=['//TRIM(RoundSigDigits(ALF,5))//  &
                          ']. This is too close; check position of reference point.')
                CALL ShowFatalError('Program terminates due to preceding condition.')
              END IF
            END IF
          ELSE IF (ALF < 0.1524d0 .and. ExtWinType == AdjZoneExtWin) THEN
            IF (DoingRefs) THEN
              IF (RefErrIndex(IWin,IL) == 0) THEN ! only show error message once
                CALL ShowWarningError('CalcDaylightCoeffRefPoints: For Zone="'//TRIM(Zone(ZoneNum)%Name)//        &
                    '" External Window="'//TRIM(Surface(IWin)%Name)//'"in Zone="'//TRIM(Zone(Surface(IWin)%Zone)%Name)//      &
                     '" reference point is less than 0.15m (6") from window plane ')
                CALL ShowContinueError('Distance=['//trim(RoundSigDigits(ALF,1))//' m] to ref point=['//  &
                   TRIM(RoundSigDigits(RREF(1),1))//  &
                   ','//TRIM(RoundSigDigits(RREF(2),1))//  &
                   ','//TRIM(RoundSigDigits(RREF(3),1))//'], Inaccuracy in Daylighting Calcs may result.')
                RefErrIndex(IWin,IL)=1
              ENDIF
            ELSE
              IF (MapErrIndex(IWin,IL) == 0) THEN ! only show error message once
                CALL ShowWarningError('CalcDaylightCoeffMapPoints: For Zone="'//TRIM(Zone(ZoneNum)%Name)//        &
                   '" External Window="'//TRIM(Surface(IWin)%Name)//'"in Zone="'//TRIM(Zone(Surface(IWin)%Zone)%Name)//      &
                   '" map point is less than 0.15m (6") from window plane ')
                CALL ShowContinueError('Distance=['//trim(RoundSigDigits(ALF,1))//' m] map point=['//  &
                   TRIM(RoundSigDigits(RREF(1),1))//  &
                   ','//TRIM(RoundSigDigits(RREF(2),1))//  &
                   ','//TRIM(RoundSigDigits(RREF(3),1))//'], Inaccuracy in Map Calcs may result.')
                MapErrIndex(IWin,IL)=1
              ENDIF
            END IF
          END IF

          ! Number of window elements in X and Y for daylighting calculation
          IF (ALF > 0.1524d0) THEN
            NDIVX = 1 + INT(4.d0 * WW / ALF)
            NDIVY = 1 + INT(4.d0 * HW / ALF)
          ENDIF

!4254      CONTINUE

          IF(ExtWinType == AdjZoneExtWin) THEN
            ! Adjust number of exterior window elements to give acceptable number of rays through
            ! interior windows in the zone (for accuracy of interior window daylighting calculation)
            SolidAngExtWin = SafeDivide( ((Surface(IWin)%Area + SurfaceWindow(IWin)%DividerArea) / Surface(IWin)%Multiplier ),  &
                                                              ALF**2)
            SolidAngMinIntWin = ZoneDaylight(ZoneNum)%MinIntWinSolidAng
            SolidAngRatio = MAX(1.0d0,SolidAngExtWin/SolidAngMinIntWin)
            NDIVX = SQRT(SolidAngRatio)*NDIVX
            NDIVY = SQRT(SolidAngRatio)*NDIVY
          END IF

          NWX = MIN(40,NDIVX)
          NWY = MIN(40,NDIVY)

          ! Discretization of triangle is simpler if NWX = NWY
          IF (Triangle) THEN
            NWX = MAX(NWX,NWY)
            NWY = NWX
          END IF

          ! Edge lengths of window elements
          DWX = WW / NWX
          DWY = HW / NWY

          ! Azimuth and altitude of window normal
          SurfaceWindow(IWin)%Phi = ASIN(WNORM(3))
          IF (ABS(WNORM(1)) > 1.0d-5 .OR. ABS(WNORM(2)) > 1.0d-5) THEN
            SurfaceWindow(IWin)%Theta = ATAN2(WNORM(2), WNORM(1))
          ELSE
            SurfaceWindow(IWin)%Theta = 0.
          END IF

          ! Recalculation of values for TDD:DOME
          IF (SurfaceWindow(IWin)%OriginalClass .EQ. SurfaceClass_TDD_Diffuser) THEN

            ! Look up the TDD:DOME object
            PipeNum = FindTDDPipe(IWin)
            IWin2 = TDDPipe(PipeNum)%Dome

            ! Calculate reference point coords relative to the diffuser coordinate system
            ! W21, W23, and WNORM are the unit vectors
            REFD(1) = DOT_PRODUCT(REFWC,W21)
            REFD(2) = DOT_PRODUCT(REFWC,W23)
            REFD(3) = DOT_PRODUCT(REFWC,WNORM)

            ! Calculate view vector coords relative to the diffuser coordinate system
            VIEWVD(1) = DOT_PRODUCT(VIEWVC,W21)
            VIEWVD(2) = DOT_PRODUCT(VIEWVC,W23)
            VIEWVD(3) = DOT_PRODUCT(VIEWVC,WNORM)

            U3 = Surface(IWin2)%Vertex(2)
            U2 = Surface(IWin2)%Vertex(3)

            IF(Surface(IWin2)%Sides == 4) THEN
              ! Vertices of window (numbered counter-clockwise starting
              ! at upper left as viewed from inside of room)
              ! Assumes original vertices are numbered counter-clockwise from
              ! upper left as viewed from outside.
              U3 = Surface(IWin2)%Vertex(2)
              U2 = Surface(IWin2)%Vertex(3)
              U1 = Surface(IWin2)%Vertex(4)
            ELSE IF(Surface(IWin2)%Sides == 3) THEN
              U3 = Surface(IWin2)%Vertex(2)
              U2 = Surface(IWin2)%Vertex(3)
              U1 = Surface(IWin2)%Vertex(1)
            END IF

            ! Unit vectors from window vertex 2 to 1 and 2 to 3,
            ! center point of window, and vector from ref pt to center of window
            U21 = U1 - U2
            U23 = U3 - U2
            HW = SQRT(DOT_PRODUCT(U21,U21))
            WW = SQRT(DOT_PRODUCT(U23,U23))
            IF(Surface(IWin2)%Sides == 4) THEN
              WC = U2 + (U23 + U21) / 2.d0
            ELSE IF(Surface(IWin2)%Sides == 3) THEN
              WC = U2 + (U23 + U21) / 3.0d0
            END IF
            SurfaceWindow(IWin2)%WinCenter = WC
            ! Unit vectors
            U21 = U21 / HW
            U23 = U23 / WW

            ! Unit vector normal to dome (pointing away from TDD)
            ! These are specific to the exterior.
            ! NOTE:  Preserve WNORM for later in the code.
            CALL DayltgCrossProduct(U21, U23, WNORM2)
            WNORM2 = WNORM2 / (SQRT(DOT_PRODUCT(WNORM2,WNORM2)))

            ! Azimuth and altitude of dome normal
            ! These are specific to the exterior.
            SurfaceWindow(IWin2)%Phi = ASIN(WNORM2(3))
            IF (ABS(WNORM2(1)) > 1.0d-5 .OR. ABS(WNORM2(2)) > 1.0d-5) THEN
              SurfaceWindow(IWin2)%Theta = ATAN2(WNORM2(2), WNORM2(1))
            ELSE
              SurfaceWindow(IWin2)%Theta = 0.0
            END IF

            ! Calculate new virtual reference point coords relative to dome coord system
            ! W21, W23, and WNORM2 are now the unit vectors for the dome coord system
            REFWC = REFD(1) * U21 + REFD(2) * U23 + REFD(3) * WNORM2
            RREF2 = WC - REFWC

            ! Calculate new virtual view vector coords relative to dome coord system
            VIEWVC2 = VIEWVD(1) * U21 + VIEWVD(2) * U23 + VIEWVD(3) * WNORM2

            ! Copy several values from the diffuser so that DayltgInterReflectedIllum works correctly
            ! These are specific to the interior.
            SurfaceWindow(IWin2)%RhoCeilingWall = SurfaceWindow(IWin)%RhoCeilingWall
            SurfaceWindow(IWin2)%RhoFloorWall = SurfaceWindow(IWin)%RhoFloorWall
            SurfaceWindow(IWin2)%FractionUpgoing = SurfaceWindow(IWin)%FractionUpgoing
            SurfaceWindow(IWin2)%GlazedFrac = SurfaceWindow(IWin)%GlazedFrac

          ELSE
            ! This is not a TDD:DIFFUSER.  Make sure nothing is messed up for a regular window.
            IWin2 = IWin
            WNORM2 = WNORM
            RREF2 = RREF
            VIEWVC2 = VIEWVC

            U2 = W2
            U21 = W21
            U23 = W23
          END IF

          ! Initialize sky and sun components of direct illuminance (arrays EDIRSK, EDIRSU, EDIRSUdisk)
          ! and average window luminance (arrays AVWLSK, AVWLSU, AVWLSUdisk), at ref pt.
          EDIRSK = 0.
          EDIRSU = 0.
          EDIRSUdisk = 0.
          AVWLSK = 0.
          AVWLSU = 0.
          AVWLSUdisk = 0.

          ! Initialize solid angle subtended by window wrt ref pt
          ! and solid angle weighted by glare position factor
          IF (DoingRefs) THEN
            SurfaceWindow(IWin)%SolidAngAtRefPt(IL) = 0.
            SurfaceWindow(IWin)%SolidAngAtRefPtWtd(IL) = 0.
          ELSE
            MapWindowSolidAngAtRefPt(loopwin,IL) = 0.
            MapWindowSolidAngAtRefPtWtd(loopwin,IL) = 0.
          ENDIF

          ! Area of window element
          IF (Rectangle) THEN
            DAXY = DWX * DWY
          ELSE IF (Triangle) THEN
            SinCornerAng = SQRT(1.0d0 - DOT_PRODUCT(W21,W23)**2)
            DAXY = DWX * DWY * SinCornerAng
          END IF

          !           ---------------------
          ! ---------- WINDOW ELEMENT LOOP ----------
          !           ---------------------

          DO IX = 1,NWX
            IF (Rectangle) THEN
              NWYlim = NWY
            ELSE IF (Triangle) THEN
              NWYlim = NWY - IX + 1
            END IF

            DO IY = 1,NWYlim
              LSHCAL = LSHCAL + 1
              SkyObstructionMult = 1.0

              ! Center of win element in absolute coord sys
              RWIN = W2 + (REAL(IX,r64) - 0.5d0) * W23 * DWX + (REAL(IY,r64) - 0.5d0) * W21 * DWY

              ! Center of win element on TDD:DOME in absolute coord sys
              ! If no TDD, RWIN2 = RWIN
              RWIN2 = U2 + (REAL(IX,r64) - 0.5d0) * U23 * DWX + (REAL(IY,r64) - 0.5d0) * U21 * DWY

              ! Distance between ref pt and window element
              DIS = SQRT(DOT_PRODUCT(RWIN - RREF, RWIN - RREF))

              ! Unit vector along ray from ref pt to element
              RAY = (RWIN - RREF) / DIS

              ! Cosine of angle between ray and window outward normal
              COSB = DOT_PRODUCT(WNORM2, RAY)

              ! If COSB > 0, direct light from window can reach ref pt. Otherwise go to loop
              ! over sun position and calculate inter-reflected component of illuminance
              IF (COSB > 0.) THEN
                ! Azimuth (-pi to pi) and altitude (-pi/2 to pi/2) of ray. Azimuth = 0 is along east.
                PHRAY = ASIN(RAY(3))
                IF (ABS(RAY(1)) > 1.0d-5 .OR. ABS(RAY(2)) > 1.0d-5) THEN
                  THRAY = ATAN2(RAY(2), RAY(1))
                ELSE
                  THRAY = 0.
                END IF

                ! Solid angle subtended by element wrt ref pt.
                DAXY1 = DAXY
                ! For triangle, at end of Y column only one half of parallelopiped's area contributes
                IF (Triangle .AND. IY == NWYlim) DAXY1 = 0.5d0 * DAXY
                DOMEGA = DAXY1 * COSB / (DIS * DIS)

                ! Calculate position factor (used in glare calculation) for this
                ! win element / ref pt / view-vector combination
                POSFAC = 0.

                ! Distance from ref pt to intersection of view vector and plane
                ! normal to view vector containing the window element

                RR = DIS * DOT_PRODUCT(RAY, VIEWVC2)
                IF (RR > 0.) THEN
                  ! Square of distance from above intersection point to win element
                  ASQ = DIS * DIS - RR * RR
                  ! Vertical displacement of win element wrt ref pt
                  YD = RWIN2(3) - RREF2(3)
                  ! Horizontal and vertical displacement ratio and position factor
                  XR = SQRT(ABS(ASQ - YD * YD)) / RR
                  YR = ABS(YD / RR)
                  POSFAC = DayltgGlarePositionFactor(XR, YR)
                END IF

                IHitIntObs = 0
                IntWinHitNum = 0
                IHitIntWin = 0
                TVISIntWinDisk = 0.0 ! Init Value
                TVISIntWin = 0.0

                IF (SurfaceWindow(IWin)%OriginalClass .EQ. SurfaceClass_TDD_Diffuser) THEN
                  ! Unshaded visible transmittance of TDD for a single ray from sky/ground element
                  TVISB = TransTDD(PipeNum, COSB, VisibleBeam) * SurfaceWindow(IWin)%GlazedFrac

                ELSE ! Regular window
                  ! Vis trans of glass for COSB incidence angle
                  TVISB = POLYF(COSB,Construct(IConst)%TransVisBeamCoef(1)) * SurfaceWindow(IWin)%GlazedFrac * &
                             SurfaceWindow(IWin)%LightWellEff
                  IF(ExtWinType == AdjZoneExtWin) THEN
                    IHitIntWin = 0
                    ! Does ray pass through an interior window in zone (ZoneNum) containing the ref point?
                    DO IntWin = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
                      IF(Surface(IntWin)%Class == SurfaceClass_Window .AND. Surface(IntWin)%ExtBoundCond >= 1) THEN
                        IF(Surface(Surface(IntWin)%ExtBoundCond)%Zone == Surface(IWin)%Zone) THEN
                          CALL DayltgPierceSurface(IntWin,RREF,RAY,IHitIntWin,HitPtIntWin)
                          IF(IHitIntWin > 0) THEN
                            IntWinHitNum = IntWin
                            COSBIntWin = DOT_PRODUCT(Surface(IntWin)%OutNormVec(1:3),RAY)
                            IF(COSBIntWin <= 0.0) THEN
                              IHitIntWin = 0
                              IntWinHitNum = 0
                              CYCLE
                            END IF
                            TVISIntWin = POLYF(COSBIntWin,Construct(Surface(IntWin)%Construction)%TransVisBeamCoef(1))
                            TVISB = TVISB * TVISIntWin
                            EXIT  ! Ray passes thru interior window; exit from DO loop
                          END IF
                        END IF
                      END IF
                    END DO  ! End of loop over surfaces in zone ZoneNum

                    IF(IHitIntWin == 0) THEN
                      ! Ray does not pass through an int win in ZoneNum. Therefore, it hits the opaque part
                      ! of a surface between ref point in ZoneNum and ext win element in adjacent zone.
                      IHitIntObs = 1
                    END IF
                  END IF  ! End of check if this is an ext win in an adjacent zone
                END IF  ! End of check if TDD:Diffuser or regular exterior window

                ! Check for interior obstructions
                IF(ExtWinType == InZoneExtWin .AND. IHitIntObs == 0) THEN
                  ! Check for obstruction between reference point and window element
                  ! Returns IHitIntObs = 1 if obstruction is hit, IHitIntObs = 0 otherwise.
                  ! (Example of interior obstruction is a wall in an L-shaped room that lies
                  ! between reference point and window.)
                  CALL DayltgHitInteriorObstruction(IWin,RREF,RWIN,IHitIntObs)
                END IF

                IF(ExtWinType == AdjZoneExtWin .AND. IntWinHitNum > 0 .AND. IHitIntObs == 0) THEN
                  ! Check for obstruction between ref point and interior window through which ray passes
                  CALL DayltgHitInteriorObstruction(IntWinHitNum,RREF,HitPtIntWin,IHitIntObs)
                  IF(IHitIntObs == 0) THEN
                    ! Check for obstruction between intersection point on int window and ext win element
                    CALL DayltgHitBetWinObstruction(IntWinHitNum,IWin,HitPtIntwin,RWIN,IHitIntObs)
                  END IF
                END IF

                IF(IHitIntObs == 0) THEN
                  IF(ExtWinType==InZoneExtWin .OR. (ExtWinType==AdjZoneExtWin .AND. IHitIntWin>0)) THEN
                    IF (DoingRefs) THEN
                      ! Increment solid angle subtended by portion of window above ref pt
                      SurfaceWindow(IWin)%SolidAngAtRefPt(IL) = SurfaceWindow(IWin)%SolidAngAtRefPt(IL) + DOMEGA
                      ZoneDaylight(ZoneNum)%SolidAngAtRefPt(IL,loopwin) =   &
                                    ZoneDaylight(ZoneNum)%SolidAngAtRefPt(IL,loopwin) + DOMEGA
                      ! Increment position-factor-modified solid angle
                      SurfaceWindow(IWin)%SolidAngAtRefPtWtd(IL) =  &
                            SurfaceWindow(IWin)%SolidAngAtRefPtWtd(IL) + DOMEGA * POSFAC
                      ZoneDaylight(ZoneNum)%SolidAngAtRefPtWtd(IL,loopwin) =  &
                            ZoneDaylight(ZoneNum)%SolidAngAtRefPtWtd(IL,loopwin) + DOMEGA * POSFAC
                    ELSE
                      MapWindowSolidAngAtRefPt(loopwin,IL) = MapWindowSolidAngAtRefPt(loopwin,IL) + DOMEGA
                      ZoneDaylight(ZoneNum)%SolidAngAtMapPt(IL,loopwin) =   &
                                    ZoneDaylight(ZoneNum)%SolidAngAtMapPt(IL,loopwin) + DOMEGA
                      MapWindowSolidAngAtRefPtWtd(loopwin,IL) =   &
                                    MapWindowSolidAngAtRefPtWtd(loopwin,IL)  + DOMEGA * POSFAC
                      ZoneDaylight(ZoneNum)%SolidAngAtMapPtWtd(IL,loopwin) =  &
                            ZoneDaylight(ZoneNum)%SolidAngAtMapPtWtd(IL,loopwin) + DOMEGA * POSFAC
                    ENDIF
                  END IF
                END IF

                IF(IHitIntObs == 1) ObTrans = 0.0

                IHitExtObs = 0
                IF (IHitIntObs == 0) THEN
                  ! No interior obstruction was hit.
                  ! Check for exterior obstructions between window element and sky/ground.
                  ! Get product of transmittances of obstructions hit by ray.
                  ! ObTrans = 1.0 will be returned if no exterior obstructions are hit.
                  CALL DayltgHitObstruction(IHr,IWin2,RWIN2,RAY,ObTrans)
                  IF(ObTrans < 1.0) IHitExtObs = 1
                END IF

                IF(CalcSolRefl .AND. PHRAY < 0.0 .AND. ObTrans > 1.d-6) THEN
                  ! Calculate effect of obstructions on shading of sky diffuse reaching the ground point hit
                  ! by the ray. This effect is given by the ratio SkyObstructionMult =
                  ! (obstructed sky diffuse at ground point)/(unobstructed sky diffuse at ground point).
                  ! This ratio is calculated for an isotropic sky.
                  ! Ground point hit by the ray:
                  Alfa = ACOS(-RAY(3))
                  Beta = ATAN2(RAY(2),RAY(1))
                  HorDis = (RWIN2(3)-GroundLevelZ)*TAN(Alfa)
                  GroundHitPt(3) = GroundLevelZ
                  GroundHitPt(1) = RWIN2(1) + HorDis*COS(Beta)
                  GroundHitPt(2) = RWIN2(2) + HorDis*SIN(Beta)
                  ! Send rays upward from hit point and see which ones are unobstructed and so go to sky.
                  ! Divide hemisphere centered at ground hit point into elements of altitude Phi and
                  ! azimuth Theta and create upward-going ground ray unit vector at each Phi,Theta pair.
                  ! Phi = 0 at the horizon; Phi = Pi/2 at the zenith.
                  DPhi = PiOvr2 / (AltAngStepsForSolReflCalc/2.d0)
                  DTheta = 2.d0*Pi / (2.d0*AzimAngStepsForSolReflCalc)
                  SkyGndObs = 0.0
                  SkyGndUnObs = 0.0
                  ! Altitude loop
                  DO IPhi = 1,(AltAngStepsForSolReflCalc/2)
                    Phi = (IPhi - 0.5d0) * DPhi
                    SPhi = SIN(Phi)
                    CPhi = COS(Phi)
                    ! Third component of ground ray unit vector in (Theta,Phi) direction
                    URay(3) = SPhi
                    dOmegaGnd = CPhi * DTheta * DPhi
                    ! Cosine of angle of incidence of ground ray on ground plane
                    CosIncAngURay = SPhi
                    IncAngSolidAngFac = CosIncAngURay*dOmegaGnd/Pi
                    ! Azimuth loop
                    DO ITheta = 1,2*AzimAngStepsForSolReflCalc
                      Theta = (ITheta - 0.5d0) * DTheta
                      URay(1) = CPhi * COS(Theta)
                      URay(2) = CPhi * SIN(Theta)
                      SkyGndUnObs = SkyGndUnObs + IncAngSolidAngFac
                      ! Does this ground ray hit an obstruction?
                      IHitObs = 0
                      DO ObsSurfNum = 1, TotSurfaces
                        IF(.NOT.Surface(ObsSurfNum)%ShadowSurfPossibleObstruction) CYCLE
                        CALL DayltgPierceSurface(ObsSurfNum,GroundHitPt,URay,IHitObs,ObsHitPt)
                        IF(IHitObs > 0) EXIT
                      END DO
                      IF(IHitObs > 0) CYCLE ! Obstruction hit
                      ! Sky is hit
                      SkyGndObs = SkyGndObs + IncAngSolidAngFac
                    END DO ! End of azimuth loop
                  END DO  ! End of altitude loop
                  SkyObstructionMult = SkyGndObs / (SkyGndUnObs + 1.E-8)
                END IF  ! End of check if solar reflection calculation is in effect

              END IF  ! End of check if COSB > 0

              !           -------------------
              ! ---------- SUN POSITION LOOP ----------
              !           -------------------

              ! Sun position counter. Used to avoid calculating various quantities
              ! that do not depend on sun position.
              ISunPos = 0

              DO IHR = 1,24
                IF (SUNCOSHR(3,IHR) < SunIsUpValue) CYCLE
                ISunPos = ISunPos + 1

                ! Altitude of sun (degrees)
                PHSUN = PHSUNHR(IHR)
                SPHSUN = SPHSUNHR(IHR)
                CPHSUN = CPHSUNHR(IHR)

                ! Azimuth of sun in absolute coord sys
                THSUN = THSUNHR(IHR)

                ! First time through, call routine to calculate inter-reflected illuminance
                ! at reference point and luminance of window with shade, screen or blind.

                ! Rob/TH - Not sure whether this call is necessary for interior zones with interior windows only.
!  new code would be -
! IF (LSHCAL == 1 .AND. ExtWinType /= AdjZoneExtWin) CALL DayltgInterReflectedIllum(ISunPos,IHR,ZoneNum,IWin2)
                IF (LSHCAL == 1) CALL DayltgInterReflectedIllum(ISunPos,IHR,ZoneNum,IWin2)

                ! Daylighting shelf simplification:  The shelf completely blocks all view of the window,
                ! only interrelflected illumination is allowed (see DayltgInterReflectedIllum above).
                ! Everything else in this loop has to do with direct luminance from the window.
                IF (InShelfSurf > 0) CYCLE

                IF (COSB <= 0.0) CYCLE

                XEDIRSK = 0.
                XEDIRSU = 0.
                XAVWLSK = 0.

                ! Add contribution of this window element to glare and to
                ! direct illuminance at reference point

                ! The I,J,K indices for sky and sun components of direct illuminance
                ! (EDIRSK, EDIRSU) and average window luminance (AVWLSK, AVWLSU) are:
                ! I=1 for clear sky, =2 Clear turbid, =3 Intermediate, =4 Overcast;
                ! J=1 for bare window, =2 for window with shade or fixed slat-angle blind;
                !  = 2,3,...,MaxSlatAngs+1 for window with variable slat-angle blind;
                ! K = sun position index.

                ! ----- CASE I -- BARE WINDOW (no shading device)

                ! Beam solar and sky solar reflected from nearest obstruction.
                ! In the following IHitIntObs == 0  ==> no interior obstructions hit, and
                !                  IHitExtObs == 1  ==> one or more exterior obstructions hit.
                IF(CalcSolRefl .AND. IHitIntObs == 0 .AND. IHitExtObs == 1) THEN
                  ! One or more exterior obstructions was hit; get contribution of reflection
                  ! from nearest obstruction.
                  ! Find obstruction whose hit point is closest to this ray's window element
                  CALL DayltgClosestObstruction(RWIN2,Ray,NearestHitSurfNum,NearestHitPt)
                  IF(NearestHitSurfNum > 0) THEN

                    ! Beam solar reflected from nearest obstruction

                    CALL DayltgSurfaceLumFromSun(IHr,Ray,NearestHitSurfNum,NearestHitPt,LumAtHitPtFrSun)
                    AVWLSU(1,IHR) = AVWLSU(1,IHR) + LumAtHitPtFrSun * TVISB
                    IF (PHRAY >= 0.) EDIRSU(1,IHR) = EDIRSU(1,IHR) + LumAtHitPtFrSun * DOMEGA * RAY(3) * TVISB

                    ! Sky solar reflected from nearest obstruction

                    ObsConstrNum = Surface(NearestHitSurfNum)%Construction
                    IF(ObsConstrNum > 0) THEN
                      ! Exterior building surface is nearest hit
                      IF(.NOT.Construct(ObsConstrNum)%TypeIsWindow) THEN
                        ! Obstruction is not a window, i.e., is an opaque surface
                        ObsVisRefl = 1.0d0 - Material(Construct(ObsConstrNum)%LayerPoint(1))%AbsorpVisible
                      ELSE
                        ! Obstruction is a window; assume it is bare
                        IF(SurfaceWindow(NearestHitSurfNum)%StormWinFlag==1)  &
                          ObsConstrNum = Surface(NearestHitSurfNum)%StormWinConstruction
                        ObsVisRefl = Construct(ObsConstrNum)%ReflectVisDiffFront
                      END IF
                    ELSE
                      ! Shadowing surface is nearest hit
                      IF(Surface(NearestHitSurfNum)%Shelf > 0) THEN
                        ! This is a daylighting shelf, for which reflection is separately calculated
                        ObsVisRefl = 0.
                      ELSE
                        ObsVisRefl = Surface(NearestHitSurfNum)%ShadowSurfDiffuseVisRefl
                        IF(Surface(NearestHitSurfNum)%ShadowSurfGlazingConstruct > 0) &
                          ObsVisRefl = ObsVisRefl + Surface(NearestHitSurfNum)%ShadowSurfGlazingFrac *  &
                          Construct(Surface(NearestHitSurfNum)%ShadowSurfGlazingConstruct)%ReflectVisDiffFront
                      END IF
                    END IF
                    NearestHitSurfNumX = NearestHitSurfNum
                    ! Each shadowing surface has a "mirror" duplicate surface facing in the opposite direction.
                    ! The following gets the correct side of a shadowing surface for reflection.
                    IF(Surface(NearestHitSurfNum)%ShadowingSurf) THEN
                      IF(DOT_PRODUCT(RAY,Surface(NearestHitSurfNum)%OutNormVec) > 0.0)  NearestHitSurfNumX = &
                           NearestHitSurfNum + 1
                    END IF
                    IF (.not. DetailedSkyDiffuseAlgorithm .or. .not.  ShadingTransmittanceVaries .or.  &
                        SolarDistribution == MinimalShadowing) THEN
                      SkyReflVisLum = ObsVisRefl * Surface(NearestHitSurfNumX)%ViewFactorSky *  &
                               DifShdgRatioIsoSky(NearestHitSurfNumX) / Pi
                    ELSE
                      SkyReflVisLum = ObsVisRefl * Surface(NearestHitSurfNumX)%ViewFactorSky *  &
                               DifShdgRatioIsoSkyHRTS(NearestHitSurfNumX,IHR,1) / Pi
                    ENDIF
                    DO ISky = 1,4
                      XAVWLSK(ISky) = GILSK(ISky,IHR) * SkyReflVisLum
                      AVWLSK(ISky,1,IHR) = AVWLSK(ISky,1,IHR) + XAVWLSK(ISky) * TVISB
                      IF (PHRAY >= 0.) THEN
                        XEDIRSK(ISky) = GILSK(ISky,IHR) * SkyReflVisLum * DOMEGA * RAY(3)
                        EDIRSK(ISky,1,IHR) = EDIRSK(ISky,1,IHR) + XEDIRSK(ISky) * TVISB
                      END IF
                    END DO
                  END IF
                END IF  ! End of check if solar reflection calculation is in effect

                IF(ObTrans > 1.d-6) THEN
                  ! Ray did not hit an obstruction or the transmittance product of hit obstructions is non-zero.
                  ! Contribution of sky or ground luminance in cd/m2
                  IF (SurfaceWindow(IWin)%OriginalClass .EQ. SurfaceClass_TDD_Diffuser) THEN
                    ! Make all transmitted light diffuse for a TDD with a bare diffuser
                    DO ISKY = 1,4
                      AVWLSK(ISKY,1,IHR) = AVWLSK(ISKY,1,IHR) + WLUMSK(ISKY,1,IHR)
                      IF(ISky == 1) THEN
                        AVWLSU(1,IHR) = AVWLSU(1,IHR) + WLUMSU(1,IHR)
                        AVWLSUdisk(1,IHR) = AVWLSUdisk(1,IHR) + WLUMSUdisk(1,IHR)
                      END IF
                      IF(PHRAY > 0.) THEN
                        EDIRSK(ISKY,1,IHR) = EDIRSK(ISKY,1,IHR) + WLUMSK(ISKY,1,IHR) * DOMEGA * RAY(3)
                        IF(ISky == 1) EDIRSU(1,IHR) = EDIRSU(1,IHR) + WLUMSU(1,IHR) * DOMEGA * RAY(3)
                      END IF
                    END DO

                  ELSE ! Bare window

                    DO ISKY = 1,4
                      IF (PHRAY > 0.) THEN
                        ! Ray heads upward to sky
                        ELUM = DayltgSkyLuminance(ISky,THRAY,PHRAY)
                        XEDIRSK(ISKY) = ELUM * DOMEGA * RAY(3)
                        DEDIR  = XEDIRSK(ISKY) * TVISB
                        EDIRSK(ISKY,1,IHR) = EDIRSK(ISKY,1,IHR) + DEDIR * ObTrans
                        AVWLSK(ISKY,1,IHR) = AVWLSK(ISKY,1,IHR) + ELUM * TVISB * ObTrans
                        XAVWLSK(ISKY) = ELUM * ObTrans
                      ELSE ! PHRAY <= 0.
                        ! Ray heads downward to ground.
                        ! Contribution from sky diffuse reflected from ground
                        XAVWLSK(ISKY) = GILSK(ISKY,IHR) * (GndReflectanceForDayltg / PI) * ObTrans *  &
                                                  SkyObstructionMult
                        AVWLSK(ISKY,1,IHR) = AVWLSK(ISKY,1,IHR) + TVISB * XAVWLSK(ISKY)
                        ! Contribution from beam solar reflected from ground (beam reaching ground point
                        ! can be obstructed [SunObstructionMult < 1.0] if CalcSolRefl = .TRUE.)
                        IF(ISky == 1) THEN
                          SunObstructionMult = 1.0
                          IF(CalcSolRefl) THEN
                            ! Coordinates of ground point hit by the ray
                            Alfa = ACOS(-RAY(3))
                            Beta = ATAN2(RAY(2),RAY(1))
                            HorDis = (RWIN2(3)-GroundLevelZ)*TAN(Alfa)
                            GroundHitPt(3) = GroundLevelZ
                            GroundHitPt(1) = RWIN2(1) + HorDis*COS(Beta)
                            GroundHitPt(2) = RWIN2(2) + HorDis*SIN(Beta)
                            ! Sun reaches ground point if vector from this point to the sun is unobstructed
                            IHitObs = 0
                            DO ObsSurfNum = 1,TotSurfaces
                               IF(.NOT.Surface(ObsSurfNum)%ShadowSurfPossibleObstruction) CYCLE
                               CALL DayltgPierceSurface(ObsSurfNum,GroundHitPt,SunCosHr(1:3,IHr),IHitObs,ObsHitPt)
                               IF(IHitObs > 0) EXIT
                            END DO
                            IF(IHitObs > 0) SunObstructionMult = 0.0
                          END IF
                          AVWLSU(1,IHR) = AVWLSU(1,IHR) +  &
                               TVISB * GILSU(IHR) * (GndReflectanceForDayltg / PI) * ObTrans * SunObstructionMult
                        END IF  ! End of check if ISky = 1
                      END IF  ! End of check if ray is going up or down
                    END DO  ! End of loop over sky types
                  END IF ! End of check if bare window or TDD:DIFFUSER
                END IF ! End of check if ObTrans > 1.E-6

                !
                ! Illuminance from beam solar (without interior reflection)
                !
                ! Just run this once on the last pass
                IF (IX == NWX .AND. IY == NWY) THEN ! Last pass

                  ! Beam solar reaching reference point directly without exterior reflection

                  ! Unit vector from ref. pt. to sun
                  RAYCOS(1) = CPHSUN * COS(THSUN)
                  RAYCOS(2) = CPHSUN * SIN(THSUN)
                  RAYCOS(3) = SPHSUN

                  ! Is sun on front side of exterior window?
                  COSI = DOT_PRODUCT(WNORM2,RAYCOS)
                  IF (COSI > 0.) THEN

                    ! Does RAYCOS pass thru exterior window? HP is point that RAYCOS intersects window plane.
                    CALL DayltgPierceSurface(IWin2,RREF2,RAYCOS,IP,HP)
                    IHitIntObsDisk = 0
                    IF (IP > 0) THEN
                      IF(ExtWinType == InZoneExtWin) THEN
                        ! Check for interior obstructions between reference point and HP.
                        CALL DayltgHitInteriorObstruction(IWin2,RREF2,HP,IHitIntObsDisk)
                      END IF
                      ObTransDisk = 0.0  ! Init value
                      ! Init flag for vector from RP to sun passing through interior window
                      IHitIntWinDisk = 0
                      IF(ExtWinType == AdjZoneExtWin) THEN ! This block is for RPs in zones with interior windows
                                                           ! adjacent to zones with exterior windows
                        ! Does RAYCOS pass through interior window in zone containing RP?
                        DO IntWinDisk = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
                          IF(Surface(IntWinDisk)%Class==SurfaceClass_Window .AND. Surface(IntWinDisk)%ExtBoundCond >= 1) THEN
                            IF(Surface(Surface(IntWinDisk)%ExtBoundCond)%Zone == Surface(IWin2)%Zone) THEN
                              CALL DayltgPierceSurface(IntWinDisk,RREF,RAYCOS,IHitIntWinDisk,HitPtIntWinDisk)
                              IF(IHitIntWinDisk > 0) THEN
                                IntWinDiskHitNum = IntWinDisk
                                COSBIntWin = DOT_PRODUCT(Surface(IntWinDisk)%OutNormVec(1:3),RAYCOS)
                                IF(COSBIntWin <= 0.0) THEN
                                  IHitIntWinDisk = 0
                                  IntWinDiskHitNum = 0
                                  CYCLE
                                END IF
                                TVISIntWinDisk = POLYF(COSBIntWin,Construct(Surface(IntWinDisk)%Construction)%TransVisBeamCoef(1))
                                EXIT
                              END IF
                            END IF
                          END IF
                        END DO

                        IF(IHitIntWinDisk == 0) THEN  ! Vector from RP to sun does not pass through interior window
                          ObTransDisk = 0.0
                          IHIT = 1             !!fcw Is this needed?
                        END IF

                        ! Check for interior obstructions between ref point and interior window
                        IHitIntObsDisk = 0
                        IF(IHitIntWinDisk > 0) THEN
                          CALL DayltgHitInteriorObstruction(IntWinDiskHitNum,RREF,HitPtIntWinDisk,IHitIntObsDisk)
                          ! If no obstruction between RP and hit int win, check for obstruction
                          ! between int win and ext win
                          IF(IHitIntObsDisk == 0) &
                            CALL DayltgHitBetWinObstruction(IntWinDiskHitNum,IWin2,HitPtIntWinDisk,HP,IHitIntObsDisk)
                        END IF
                        IF(IHitIntObsDisk == 1) ObTransDisk = 0.0
                      END IF  ! case where RP is in zone with interior window adjacent to zone with exterior window

                      IHitExtObsDisk = 0
                      ! RJH 08-25-07 IHitIntWinDisk should not be reset to 0 here, and should be tested below.
                      ! This is to correct logic flaw causing direct solar to reach adjacent zone refpt
                      ! when vector to sun does not pass through interior window
                      ! IHitIntWinDisk = 0
                      IF(IHitIntObsDisk == 0) THEN  ! No interior obstruction was hit
                        ! Net transmittance of exterior obstructions encountered by RAYCOS
                        ! ObTransDisk = 1.0 will be returned if no exterior obstructions are hit.
                        CALL DayltgHitObstruction(IHr,IWin2,RREF2,RAYCOS,ObTransDisk)
                        IF(ObTransDisk < 1.0) IHitExtObsDisk = 1
                        ! RJH 08-26-07 However, if this is a case of interior window
                        ! and vector to sun does not pass through interior window
                        ! then reset ObTransDisk to 0.0 since it is the key test for adding
                        ! contribution of sun to RP below.
                        IF((ExtWinType == AdjZoneExtWin) .AND. (IHitIntWinDisk == 0)) THEN
                          ObTransDisk = 0.0
                        END IF
                      END IF

                      ! PETER: need side wall mounted TDD to test this
                      ! PETER: probably need to replace RREF2 with RWIN2
                      ! PETER: need to check for interior obstructions too.

                      IF (ObTransDisk > 1.d-6) THEN

                        ! Sun reaches reference point;  increment illuminance.
                        ! Direct normal illuminance is normalized to 1.0

                        IF (SurfaceWindow(IWin)%OriginalClass .EQ. SurfaceClass_TDD_Diffuser) THEN
                          ! No beam is transmitted.  Takes care of TDD with a bare diffuser and all types of blinds.
                          TVISS = 0.0
                        ELSE
                          ! Beam transmittance for bare window and all types of blinds
                          TVISS = POLYF(COSI,Construct(IConst)%TransVisBeamCoef(1)) * SurfaceWindow(IWin)%GlazedFrac * &
                                     SurfaceWindow(IWin)%LightWellEff
                          IF(ExtWinType==AdjZoneExtWin.AND.IHitIntWinDisk==1)  &
                            TVISS = TVISS * TVISIntWinDisk
                        END IF

                        EDIRSUdisk(1,IHR) = RAYCOS(3) * TVISS * ObTransDisk  ! Bare window

                        TransBmBmMult = 0.0
                        IF (ShType == WSC_ST_ExteriorBlind .OR. ShType == WSC_ST_InteriorBlind .OR.   &
                            ShType == WSC_ST_BetweenGlassBlind) THEN
                          CALL ProfileAngle(IWin,RAYCOS,Blind(BlNum)%SlatOrientation,ProfAng)
                          ! Contribution of beam passing through slats and reaching reference point
                          DO JB = 1,MaxSlatAngs
                            !IF (.NOT.SurfaceWindow(IWin)%MovableSlats .AND. JB > 1) EXIT
                            IF (SurfaceWindow(IWin)%MovableSlats) THEN
                              SlatAng = (JB - 1) * PI / (MaxSlatAngs - 1)
                            ELSE
                              SlatAng = Blind(BlNum)%SlatAngle * DegToRadians
                            END IF
                            TransBmBmMult(JB) = BlindBeamBeamTrans(ProfAng,SlatAng,Blind(BlNum)%SlatWidth, &
                                                 Blind(BlNum)%SlatSeparation,Blind(BlNum)%SlatThickness)
                            EDIRSUdisk(JB+1,IHR) = RAYCOS(3) * TVISS * TransBmBmMult(JB) * ObTransDisk

                            ! do this only once for fixed slat blinds
                            IF (.NOT.SurfaceWindow(IWin)%MovableSlats) EXIT
                          END DO
                        ELSE IF(ShType == WSC_ST_ExteriorScreen) THEN
!                          pass angle from sun to window normal here using PHSUN and THSUN from above and surface angles
!                          SunAltitudeToWindowNormalAngle = PHSUN - SurfaceWindow(IWin)%Phi
!                          SunAzimuthToWindowNormalAngle = THSUN - SurfaceWindow(IWin)%Theta
                          CALL CalcScreenTransmittance(IWin, Phi=(PHSUN - SurfaceWindow(IWin)%Phi), &
                                                           Theta=(THSUN - SurfaceWindow(IWin)%Theta))
                          TransBmBmMult(1) = SurfaceScreens(SurfaceWindow(IWin)%ScreenNumber)%BmBmTrans
                          EDIRSUdisk(2,IHR) = RAYCOS(3) * TVISS * TransBmBmMult(1) * ObTransDisk
                        END IF

                        ! Glare from solar disk

                        ! Position factor for sun (note that AZVIEW is wrt y-axis and THSUN is wrt
                        ! x-axis of absolute coordinate system.
                        XR = TAN(ABS(PIOVR2 - AZVIEW - THSUN) + 0.001d0)
                        YR = TAN(PHSUN + 0.001d0)
                        POSFAC = DayltgGlarePositionFactor(XR,YR)

                        IF (DoingRefs) THEN
                          IF (POSFAC /= 0.0 .AND. SurfaceWindow(IWin)%SolidAngAtRefPtWtd(IL) > 0.000001d0) THEN
                            ! Increment window luminance.  Luminance of solar disk (cd/m2)
                            ! is 1.47*10^4*(direct normal solar illuminance) for direct normal solar
                            ! illuminance in lux (lumens/m2). For purposes of calculating daylight factors
                            ! direct normal solar illuminance = 1.0.
                            ! Solid angle subtended by sun is 0.000068 steradians

                            XAVWL = 14700.d0 * SQRT(0.000068d0 * POSFAC) *  REAL(NWX * NWY,r64) /  &
                                     SurfaceWindow(IWin)%SolidAngAtRefPtWtd(IL)**0.8d0
                            AVWLSUdisk(1,IHR) = XAVWL * TVISS * ObTransDisk  ! Bare window

                            IF (ShType == WSC_ST_ExteriorBlind .OR. ShType == WSC_ST_InteriorBlind .OR.  &
                                ShType == WSC_ST_BetweenGlassBlind) THEN
                              DO JB = 1,MaxSlatAngs
                                !IF (.NOT. SurfaceWindow(IWin)%MovableSlats .AND. JB > 1) EXIT
                                AVWLSUdisk(JB+1,IHR) = XAVWL * TVISS * TransBmBmMult(JB) * ObTransDisk
                                IF (.NOT. SurfaceWindow(IWin)%MovableSlats) EXIT
                              END DO
                            ELSE IF(ShType == WSC_ST_ExteriorScreen) THEN
                              AVWLSUdisk(2,IHR) = XAVWL * TVISS * TransBmBmMult(1) * ObTransDisk
                            END IF
                          END IF ! Position factor
                        ELSE
                          IF (POSFAC /= 0.0 .AND. MapWindowSolidAngAtRefPtWtd(loopwin,IL) > 0.000001d0) THEN
                            ! Increment window luminance.  Luminance of solar disk (cd/m2)
                            ! is 1.47*10^4*(direct normal solar illuminance) for direct normal solar
                            ! illuminance in lux (lumens/m2). For purposes of calculating daylight factors
                            ! direct normal solar illuminance = 1.0.
                            ! Solid angle subtended by sun is 0.000068 steradians

                            XAVWL = 14700.d0 * SQRT(0.000068d0 * POSFAC) *  REAL(NWX * NWY,r64) /  &
                                     MapWindowSolidAngAtRefPtWtd(loopwin,IL)**0.8d0
                            AVWLSUdisk(1,IHR) = XAVWL * TVISS * ObTransDisk  ! Bare window

                            IF (ShType == WSC_ST_ExteriorBlind .OR. ShType == WSC_ST_InteriorBlind .OR.  &
                                ShType == WSC_ST_BetweenGlassBlind) THEN
                              DO JB = 1,MaxSlatAngs
                                !IF (.NOT. SurfaceWindow(IWin)%MovableSlats .AND. JB > 1) EXIT
                                AVWLSUdisk(JB+1,IHR) = XAVWL * TVISS * TransBmBmMult(JB) * ObTransDisk
                                IF (.NOT. SurfaceWindow(IWin)%MovableSlats) EXIT
                              END DO
                            ELSE IF(ShType == WSC_ST_ExteriorScreen) THEN
                              AVWLSUdisk(2,IHR) = XAVWL * TVISS * TransBmBmMult(1) * ObTransDisk
                            END IF
                          END IF ! Position factor
                        ENDIF
                      END IF ! Beam avoids all obstructions
                    END IF ! Beam passes thru window
                  END IF ! Sun on front side

                  ! Beam solar reaching reference point after beam-beam (specular) reflection from
                  ! an exterior surface

                  IF(CalcSolRefl) THEN
                    ! Receiving surface number corresponding this window
                    RecSurfNum = Surface(IWin2)%ShadowSurfRecSurfNum
                    IF (RecSurfNum > 0) THEN    ! interior windows do not apply
                      IF(SolReflRecSurf(RecSurfNum)%NumPossibleObs > 0) THEN
                        ! This window has associated obstructions that could reflect beam onto the window
                        DO loop = 1,SolReflRecSurf(RecSurfNum)%NumPossibleObs
                          ReflSurfNum = SolReflRecSurf(RecSurfNum)%PossibleObsSurfNums(loop)
                          ReflSurfNumX = ReflSurfNum
                          ! Each shadowing surface has a "mirror" duplicate surface facing in the opposite direction.
                          ! The following gets the correct side of a shadowing surface for reflection.
                          IF(Surface(ReflSurfNum)%ShadowingSurf) THEN
                            IF(DOT_PRODUCT(RAYCOS,Surface(ReflSurfNum)%OutNormVec) < 0.0)  ReflSurfNumX = &
                            ReflSurfNum + 1
                          END IF
                          ! Require that the surface can have specular reflection
                          IF(Surface(ReflSurfNum)%Class == SurfaceClass_Window .OR. &
                             Surface(ReflSurfNum)%ShadowSurfGlazingFrac > 0.0) THEN
                            ReflNorm = Surface(ReflSurfNumX)%OutNormVec
                            ! Vector to sun that is mirrored in obstruction
                            SunVecMir = RAYCOS - 2.0d0*DOT_PRODUCT(RAYCOS,ReflNorm)*ReflNorm
                            ! Skip if reflecting surface is not sunlit
                            IF(SunlitFrac(ReflSurfNumX,IHr,1) < 0.01d0) CYCLE
                            ! Skip if altitude angle of mirrored sun is negative since reflected sun cannot
                            ! reach reference point in this case
                            IF(SunVecMir(3) <= 0.0) CYCLE
                            ! Cosine of incidence angle of reflected beam on window
                            CosIncAngRec = DOT_PRODUCT(Surface(IWin2)%OutNormVec,SunVecMir)
                            IF(CosIncAngRec <= 0.0) CYCLE
                            ! Does ray from ref. pt. along SunVecMir pass through window?
                            CALL DayltgPierceSurface(IWin2,RREF2,SunVecMir,IP,HP)
                            IF(IP == 0) CYCLE  ! Ray did not pass through window
                            ! Check if this ray hits interior obstructions
                            CALL DayltgHitInteriorObstruction(IWin2,RREF2,HP,IHit)
                            IF(IHit > 0) CYCLE ! Interior obstruction was hit
                            ! Does ray hit this reflecting surface?
                            CALL DayltgPierceSurface(ReflSurfNum,RREF2,SunVecMir,IHitRefl,HitPtRefl)
                            IF(IHitRefl == 0) CYCLE  ! Ray did not hit this reflecting surface
                            ReflDistance = SQRT(DOT_PRODUCT(HitPtRefl-RREF2,HitPtRefl-RREF2))
                            ! Is ray from ref. pt. to reflection point (HitPtRefl) obstructed?
                            IHitObsRefl = 0
                            DO loop2 = 1,SolReflRecSurf(RecSurfNum)%NumPossibleObs
                              ObsSurfNum = SolReflRecSurf(RecSurfNum)%PossibleObsSurfNums(loop2)
                              IF(ObsSurfNum == ReflSurfNum .OR. ObsSurfNum == Surface(ReflSurfNum)%BaseSurf) CYCLE
                              CALL DayltgPierceSurface(ObsSurfNum,RREF2,SunVecMir,IHitObs,HitPtObs)
                              IF(IHitObs > 0) THEN
                                ObsDistance = SQRT(DOT_PRODUCT(HitPtObs-RREF2,HitPtObs-RREF2))
                                IF(ObsDistance < ReflDistance) THEN
                                  IHitObsRefl = 1
                                  EXIT
                                END IF
                              END IF
                            END DO
                            IF(IHitObsRefl > 0) CYCLE  ! Obstruct'n closer than reflect'n pt. was hit; go to next obstruction
                            ! There is no obstruction for this ray between ref pt and hit pt on reflecting surface.
                            ! See if ray from hit pt on reflecting surface to original (unmirrored) sun position is obstructed
                            IHitObs = 0
                            IF(Surface(ReflSurfNum)%Class == SurfaceClass_Window) THEN
                              ! Reflecting surface is a window.
                              ! Receiving surface number for this reflecting window.
                              ReflSurfRecNum = Surface(ReflSurfNum)%ShadowSurfRecSurfNum
                              IF(ReflSurfRecNum > 0) THEN
                                ! Loop over possible obstructions for this reflecting window
                                DO loop2 = 1,SolReflRecSurf(ReflSurfRecNum)%NumPossibleObs
                                  ObsSurfNum = SolReflRecSurf(ReflSurfRecNum)%PossibleObsSurfNums(loop2)
                                  CALL DayltgPierceSurface(ObsSurfNum,HitPtRefl,RAYCOS,IHitObs,HitPtObs)
                                  IF(IHitObs > 0) EXIT
                                END DO
                              END IF
                            ELSE
                              ! Reflecting surface is a building shade
                              DO ObsSurfNum = 1, TotSurfaces
                                IF(.NOT.Surface(ObsSurfNum)%ShadowSurfPossibleObstruction) CYCLE
                                IF(ObsSurfNum == ReflSurfNum) CYCLE
                                CALL DayltgPierceSurface(ObsSurfNum,HitPtRefl,RAYCOS,IHitObs,HitPtObs)
                                IF(IHitObs > 0) EXIT
                              END DO
                            END IF  ! End of check if reflector is a window or shadowing surface

                            IF(IHitObs > 0) CYCLE ! Obstruct'n hit between reflect'n hit point and sun; go to next obstruction

                            ! No obstructions. Calculate reflected beam illuminance at ref. pt. from this reflecting surface.
                            SpecReflectance = 0.0
                            CosIncAngRefl = ABS(DOT_PRODUCT(RAYCOS,ReflNorm))
                            IF(Surface(ReflSurfNum)%Class == SurfaceClass_Window) THEN
                              ConstrNumRefl = Surface(ReflSurfNum)%Construction
                              IF(SurfaceWindow(ReflSurfNum)%StormWinFlag==1) &
                                ConstrNumRefl = Surface(ReflSurfNum)%StormWinConstruction
                              SpecReflectance = POLYF(ABS(CosIncAngRefl),Construct(ConstrNumRefl)%ReflSolBeamFrontCoef(1:6))
                            END IF
                            IF(Surface(ReflSurfNum)%ShadowingSurf  &
                               .AND.Surface(ReflSurfNum)%ShadowSurfGlazingConstruct > 0)  &
                               SpecReflectance = Surface(ReflSurfNum)%ShadowSurfGlazingFrac * POLYF(ABS(CosIncAngRefl), &
                                     Construct(Surface(ReflSurfNum)%ShadowSurfGlazingConstruct)%ReflSolBeamFrontCoef(1:6))
                            TVisRefl = POLYF(CosIncAngRec,Construct(IConst)%TransVisBeamCoef(1)) *   &
                                                SurfaceWindow(IWin)%GlazedFrac * SurfaceWindow(IWin)%LightWellEff
                            EDIRSUdisk(1,IHR) = EDIRSUdisk(1,IHR) + SunVecMir(3) * SpecReflectance * TVisRefl  ! Bare window

                            TransBmBmMultRefl = 0.0
                            IF (ShType == WSC_ST_ExteriorBlind .OR. ShType == WSC_ST_InteriorBlind .OR.  &
                                ShType == WSC_ST_BetweenGlassBlind) THEN
                              CALL ProfileAngle(IWin,SunVecMir,Blind(BlNum)%SlatOrientation,ProfAng)
                              ! Contribution of reflected beam passing through slats and reaching reference point
                              DO JB = 1,MaxSlatAngs
                                !IF (.NOT.SurfaceWindow(IWin)%MovableSlats .AND. JB > 1) EXIT
                                IF (SurfaceWindow(IWin)%MovableSlats) THEN
                                  SlatAng = (JB - 1) * PI / (MaxSlatAngs - 1)
                                ELSE
                                  SlatAng = Blind(BlNum)%SlatAngle * DegToRadians
                                END IF
                                TransBmBmMultRefl(JB) = BlindBeamBeamTrans(ProfAng,SlatAng,Blind(BlNum)%SlatWidth, &
                                                   Blind(BlNum)%SlatSeparation,Blind(BlNum)%SlatThickness)
                                EDIRSUdisk(JB+1,IHR) = EDIRSUdisk(JB+1,IHR) +  &
                                   SunVecMir(3) * SpecReflectance * TVisRefl * TransBmBmMultRefl(JB)

                                IF (.NOT.SurfaceWindow(IWin)%MovableSlats) EXIT
                              END DO
                            ELSE IF(ShType == WSC_ST_ExteriorScreen) THEN
!                             pass angle from sun to window normal here using PHSUN and THSUN from above and surface angles
!                             SunAltitudeToWindowNormalAngle = PHSUN - SurfaceWindow(IWin)%Phi
!                             SunAzimuthToWindowNormalAngle = THSUN - SurfaceWindow(IWin)%Theta
                              CALL CalcScreenTransmittance(IWin, Phi=(PHSUN - SurfaceWindow(IWin)%Phi), &
                                                               Theta=(THSUN - SurfaceWindow(IWin)%Theta))
                              TransBmBmMultRefl(1) = SurfaceScreens(SurfaceWindow(IWin)%ScreenNumber)%BmBmTrans
                              EDIRSUdisk(2,IHR) = EDIRSUdisk(2,IHR) +  &
                                   SunVecMir(3) * SpecReflectance * TVisRefl * TransBmBmMultRefl(1)
                            END IF  ! End of check if window has a blind or screen

                            ! Glare from reflected solar disk

                            PHSUNrefl = SunVecMir(3)
                            THSUNrefl = ATAN2(SunVecMir(2),SunVecMir(1))
                            XR = TAN(ABS(PiOvr2 - AZVIEW - THSUNrefl) + 0.001d0)
                            YR = TAN(PHSUNrefl + 0.001)
                            POSFAC=DayltgGlarePositionFactor(XR,YR)
                            IF (DoingRefs) THEN
                              IF(POSFAC /= 0.0 .AND. SurfaceWindow(IWin)%SolidAngAtRefPtWtd(IL) > 0.000001d0) THEN
                                XAVWL = 14700. * SQRT(0.000068d0 * POSFAC) *  REAL(NWX * NWY,r64) /  &
                                       SurfaceWindow(IWin)%SolidAngAtRefPtWtd(IL)**0.8d0
                                AVWLSUdisk(1,IHR) = AVWLSUdisk(1,IHR) + XAVWL * TVisRefl * SpecReflectance  ! Bare window
                                IF (ShType == WSC_ST_ExteriorBlind .OR. ShType == WSC_ST_InteriorBlind .OR.  &
                                    ShType == WSC_ST_BetweenGlassBlind) THEN
                                  DO JB = 1,MaxSlatAngs
                                    !IF(.NOT. SurfaceWindow(IWin)%MovableSlats .AND. JB > 1) EXIT
                                    AVWLSUdisk(JB+1,IHR) = AVWLSUdisk(JB+1,IHR) +  &
                                          XAVWL * TVisRefl * SpecReflectance * TransBmBmMultRefl(JB)
                                    IF(.NOT. SurfaceWindow(IWin)%MovableSlats) EXIT
                                  END DO
                                ELSE IF(ShType == WSC_ST_ExteriorScreen) THEN
                                    AVWLSUdisk(2,IHR) = AVWLSUdisk(2,IHR) +  &
                                          XAVWL * TVisRefl * SpecReflectance * TransBmBmMultRefl(1)
                                END IF
                              END IF
                            ELSE
                              IF (POSFAC /= 0.0 .AND. MapWindowSolidAngAtRefPtWtd(loopwin,IL) > 0.000001d0) THEN
                                XAVWL = 14700. * SQRT(0.000068d0 * POSFAC) *  REAL(NWX * NWY,r64) /  &
                                       MapWindowSolidAngAtRefPtWtd(loopwin,IL)**0.8d0
                              AVWLSUdisk(1,IHR) = XAVWL * TVISS * ObTransDisk  ! Bare window
                                AVWLSUdisk(1,IHR) = AVWLSUdisk(1,IHR) + XAVWL * TVisRefl * SpecReflectance  ! Bare window
                                IF (ShType == WSC_ST_ExteriorBlind .OR. ShType == WSC_ST_InteriorBlind .OR.  &
                                    ShType == WSC_ST_BetweenGlassBlind) THEN
                                  DO JB = 1,MaxSlatAngs
                                    !IF(.NOT. SurfaceWindow(IWin)%MovableSlats .AND. JB > 1) EXIT
                                    AVWLSUdisk(JB+1,IHR) = AVWLSUdisk(JB+1,IHR) +  &
                                          XAVWL * TVisRefl * SpecReflectance * TransBmBmMultRefl(JB)
                                    IF(.NOT. SurfaceWindow(IWin)%MovableSlats) EXIT
                                  END DO
                                ELSE IF(ShType == WSC_ST_ExteriorScreen) THEN
                                    AVWLSUdisk(2,IHR) = AVWLSUdisk(2,IHR) +  &
                                          XAVWL * TVisRefl * SpecReflectance * TransBmBmMultRefl(1)
                                END IF
                              END IF
                            ENDIF

                          END IF  ! End of check that obstruction can specularly reflect
                        END DO  ! End of loop over obstructions associated with this window

                      END IF  ! End of check if this window has associated obstructions
                    END IF   ! End of check to see if this is exterior type window
                  END IF  ! End of check if exterior reflection calculation is in effect

                END IF ! Last pass

                IF((ICtrl > 0 .AND.  &
                   (ShType ==WSC_ST_InteriorShade.OR.ShType ==WSC_ST_ExteriorShade.OR.ShType ==WSC_ST_BetweenGlassShade.OR. &
                    ShType ==WSC_ST_InteriorBlind.OR.ShType ==WSC_ST_ExteriorBlind.OR.ShType ==WSC_ST_BetweenGlassBlind .OR. &
                    ShType ==WSC_ST_ExteriorScreen)).OR.SurfaceWindow(IWin)%SolarDiffusing) THEN

                  ! ----- CASE II -- WINDOW WITH SCREEN, SHADE, BLIND, OR DIFFUSING WINDOW

                  ! Interior window visible transmittance multiplier for exterior window in adjacent zone
                  TVisIntWinMult = 1.0
                  TVisIntWinDiskMult = 1.0
                  IF(Surface(IWin)%Zone /= ZoneNum) THEN
                    TVisIntWinMult = TVisIntWin
                    TVisIntWinDiskMult = TVisIntWinDisk
                  END IF

                  DO ISKY = 1,4
                    DO JB = 1,MaxSlatAngs
                      !IF (.NOT.SurfaceWindow(IWin)%MovableSlats .AND. JB > 1) EXIT
                      AVWLSK(ISKY,JB+1,IHR) = AVWLSK(ISKY,JB+1,IHR) + WLUMSK(ISKY,JB+1,IHR)*TVisIntWinMult
                      IF (ISky == 1) THEN
                        AVWLSU(JB+1,IHR) = AVWLSU(JB+1,IHR) + WLUMSU(JB+1,IHR)*TVisIntWinMult
                        AVWLSUdisk(JB+1,IHR) = AVWLSUdisk(JB+1,IHR) + WLUMSUdisk(JB+1,IHR)*TVisIntWinDiskMult
                      END IF
                      IF (PHRAY > 0.) THEN
                        EDIRSK(ISKY,JB+1,IHR) = EDIRSK(ISKY,JB+1,IHR) + WLUMSK(ISKY,JB+1,IHR)*DOMEGA*RAY(3)*TVisIntWinMult
                        IF (ISky == 1) EDIRSU(JB+1,IHR) = EDIRSU(JB+1,IHR) + WLUMSU(JB+1,IHR)*DOMEGA*RAY(3)*TVisIntWinMult
                      END IF
                      IF (.NOT.SurfaceWindow(IWin)%MovableSlats) EXIT
                    END DO
                  END DO
                END IF

              END DO ! End of hourly sun position loop, IHR
            END DO ! End of window Y-element loop, IY
          END DO ! End of window X-element loop, IX

          ! Loop again over hourly sun positions and calculate daylight factors by adding
          ! direct and inter-reflected illum components, then dividing by exterior horiz illum.
          ! Also calculate corresponding glare factors.
          ISunPos = 0
!          IDaylFacPtr = ZoneDaylight(ZoneNum)%DayltgFacPtrsForExtWins(loopwin)
          ILB=BRef+IL

          DO IHR = 1,24
            IF (SUNCOSHR(3,IHR) < SunIsUpValue) CYCLE

            ISunPos = ISunPos + 1

            ! Altitude of sun (degrees)
            PHSUN = PHSUNHR(IHR)
            SPHSUN = SPHSUNHR(IHR)
            CPHSUN = CPHSUNHR(IHR)

            ! Azimuth of sun in absolute coord sys
            THSUN = THSUNHR(IHR)

            DO ISKY = 1,4 ! Loop over sky types

              ! Loop over shading index (1=bare window; 2=diffusing glazing, shade, screen or fixed slat-angle blind;
              ! 2 to MaxSlatAngs+1 for variable slat-angle blind)

              ! TH. 9/22/2009. CR 7625 - daylight illuminance spikes during some sunset hours due to the calculated sky and sun
              !  related daylight factors > 1, which theoretically can occur when sun is perpendicular to the window
              !  and interior surfaces with high visible reflectance.
              ! Added tmpDFCalc (default to 0.05 lux) as the cap for GILSK and GILSU in calculating the daylight factors
              !  the assumption behind it is if exterior horizontal surface does not get daylight, spaces do not get daylight.

              DO JSH = 1,MaxSlatAngs+1
                IF (.NOT.SurfaceWindow(IWin)%MovableSlats .AND. JSH > 2) EXIT

                IF (GILSK(ISKY,IHR) > tmpDFCalc) THEN
                  ZoneDaylight(ZoneNum)%DaylIllFacSky(loopwin,ILB,ISky,JSH,IHR) =   &
                                 (EDIRSK(ISKY,JSH,IHR) + EINTSK(ISKY,JSH,IHR)) / GILSK(ISKY,IHR)
                  ZoneDaylight(ZoneNum)%DaylSourceFacSky(loopwin,ILB,ISky,JSH,IHR) =   &
                                 AVWLSK(ISKY,JSH,IHR) / (NWX*NWY * GILSK(ISKY,IHR))
                  ZoneDaylight(ZoneNum)%DaylBackFacSky(loopwin,ILB,ISky,JSH,IHR) =   &
                      EINTSK(ISKY,JSH,IHR) * ZoneDaylight(ZoneNum)%AveVisDiffReflect / (PI*GILSK(ISKY,IHR))
                ELSE
                  ZoneDaylight(ZoneNum)%DaylIllFacSky(loopwin,ILB,ISky,JSH,IHR) = 0.0
                  ZoneDaylight(ZoneNum)%DaylSourceFacSky(loopwin,ILB,ISky,JSH,IHR) = 0.0
                  ZoneDaylight(ZoneNum)%DaylBackFacSky(loopwin,ILB,ISky,JSH,IHR) = 0.0
                ENDIF

                IF (ISky == 1) THEN
                  IF (GILSU(IHR) > tmpDFCalc) THEN
                    ZoneDaylight(ZoneNum)%DaylIllFacSun(loopwin,ILB,JSH,IHR) =   &
                                      (EDIRSU(JSH,IHR) + EINTSU(JSH,IHR)) / (GILSU(IHR) + 0.0001d0)
                    ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loopwin,ILB,JSH,IHR) =   &
                                  (EDIRSUdisk(JSH,IHR) + EINTSUdisk(JSH,IHR)) / (GILSU(IHR) + 0.0001d0)

                    ZoneDaylight(ZoneNum)%DaylSourceFacSun(loopwin,ILB,JSH,IHR) =  &
                                   AVWLSU(JSH,IHR) / (NWX*NWY * (GILSU(IHR) + 0.0001d0))
                    ZoneDaylight(ZoneNum)%DaylSourceFacSunDisk(loopwin,ILB,JSH,IHR) =   &
                                   AVWLSUdisk(JSH,IHR) / (NWX*NWY * (GILSU(IHR) + 0.0001d0))

                    ZoneDaylight(ZoneNum)%DaylBackFacSun(loopwin,ILB,JSH,IHR) =   &
                      EINTSU(JSH,IHR) * ZoneDaylight(ZoneNum)%AveVisDiffReflect / (Pi*(GILSU(IHR) + 0.0001d0))
                    ZoneDaylight(ZoneNum)%DaylBackFacSunDisk(loopwin,ILB,JSH,IHR) =   &
                      EINTSUdisk(JSH,IHR) * ZoneDaylight(ZoneNum)%AveVisDiffReflect / (Pi*(GILSU(IHR) + 0.0001d0))
                  ELSE
                    ZoneDaylight(ZoneNum)%DaylIllFacSun(loopwin,ILB,JSH,IHR) = 0.0
                    ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loopwin,ILB,JSH,IHR) = 0.0

                    ZoneDaylight(ZoneNum)%DaylSourceFacSun(loopwin,ILB,JSH,IHR) = 0.0
                    ZoneDaylight(ZoneNum)%DaylSourceFacSunDisk(loopwin,ILB,JSH,IHR) = 0.0

                    ZoneDaylight(ZoneNum)%DaylBackFacSun(loopwin,ILB,JSH,IHR) = 0.0
                    ZoneDaylight(ZoneNum)%DaylBackFacSunDisk(loopwin,ILB,JSH,IHR) = 0.0
                  ENDIF
                END IF
              END DO ! End of shading index loop, JSH

              ! For switchable glazing put daylighting factors for switched (dark) state in IS=2 location
              IF (ICtrl > 0) THEN
                IF(WindowShadingControl(ICtrl)%ShadingType == WSC_ST_SwitchableGlazing) THEN
                  VTR = SurfaceWindow(IWin)%VisTransRatio
                  ZoneDaylight(ZoneNum)%DaylIllFacSky(loopwin,ILB,ISky,2,IHR) =   &
                                    ZoneDaylight(ZoneNum)%DaylIllFacSky(loopwin,ILB,ISky,1,IHR)*VTR
                  ZoneDaylight(ZoneNum)%DaylSourceFacSky(loopwin,ILB,ISky,2,IHR) =   &
                                    ZoneDaylight(ZoneNum)%DaylSourceFacSky(loopwin,ILB,ISky,1,IHR)*VTR
                  ZoneDaylight(ZoneNum)%DaylBackFacSky(loopwin,ILB,ISky,2,IHR) =   &
                                    ZoneDaylight(ZoneNum)%DaylBackFacSky(loopwin,ILB,ISky,1,IHR)*VTR
                  IF (ISky == 1) THEN
                    ZoneDaylight(ZoneNum)%DaylIllFacSun(loopwin,ILB,2,IHR) =   &
                                    ZoneDaylight(ZoneNum)%DaylIllFacSun(loopwin,ILB,1,IHR)*VTR
                    ZoneDaylight(ZoneNum)%DaylSourceFacSun(loopwin,ILB,2,IHR) =   &
                                    ZoneDaylight(ZoneNum)%DaylSourceFacSun(loopwin,ILB,1,IHR)*VTR
                    ZoneDaylight(ZoneNum)%DaylBackFacSun(loopwin,ILB,2,IHR) =   &
                                    ZoneDaylight(ZoneNum)%DaylBackFacSun(loopwin,ILB,1,IHR)*VTR
                    ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loopwin,ILB,2,IHR) =   &
                                    ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loopwin,ILB,1,IHR)*VTR
                    ZoneDaylight(ZoneNum)%DaylSourceFacSunDisk(loopwin,ILB,2,IHR) =   &
                                    ZoneDaylight(ZoneNum)%DaylSourceFacSunDisk(loopwin,ILB,1,IHR)*VTR
                    ZoneDaylight(ZoneNum)%DaylBackFacSunDisk(loopwin,ILB,2,IHR) =     &
                                    ZoneDaylight(ZoneNum)%DaylBackFacSunDisk(loopwin,ILB,1,IHR)*VTR
                  END IF
                END IF
              END IF ! ICtrl > 0

            END DO ! End of sky type loop, ISky
          END DO ! End of sun position loop, IHR
        END DO ! End of window loop, loopwin - IWin

        IF (.not. DoingRefs) THEN
          DEALLOCATE(MapWindowSolidAngAtRefPt)
          DEALLOCATE(MapWindowSolidAngAtRefPtWtd)
        ENDIF

    END DO ! End of reference point loop, IL

  END DO ! End of CalcLoop

  RETURN

END SUBROUTINE CalcDayltgCoeffsRefMapPoints

SUBROUTINE GetDaylightingParametersInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   Oct 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine provides a simple structure to get all daylighting
          ! parameters.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem
  ! RJH DElight Modification Begin
  USE DElightManagerF     ! Module for managing DElight subroutines
  ! RJH DElight Modification End
  USE DataSystemVariables, ONLY: GoodIOStatValue


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
  INTEGER   :: TotDaylightingDetailed       ! Total Daylighting:Detailed inputs
  INTEGER   :: IntWin                       ! Interior window surface index
  LOGICAL   :: ErrorsFound                  ! Error flag
  INTEGER   :: SurfNum                      ! Surface counter (loop)
  INTEGER   :: WindowShadingControlPtr      ! Pointer for WindowShadingControl
  INTEGER   :: ZoneNum                      ! Zone Number (loop counter)
  INTEGER   :: SurfNumAdj                   ! Surface Number for adjacent surface
  INTEGER   :: ZoneNumAdj                   ! Zone Number for adjacent zone
  ! RJH DElight Modification Begin - local variable declarations
  INTEGER   :: TotDaylightingDElight  ! Total Daylighting:DElight inputs
  REAL(r64) :: dLatitude    ! double for argument passing
  INTEGER   :: iErrorFlag             ! Error Flag for warning/errors returned from DElight
  INTEGER, EXTERNAL :: GetNewUnitNumber  ! External  function to "get" a unit number
  INTEGER   :: iDElightErrorFile      ! Unit number for reading DElight Error File (eplusout.delightdfdmp)
  INTEGER   :: iReadStatus            ! Error File Read Status
  CHARACTER(len=210) cErrorLine     ! Each DElight Error line can be up to 210 characters long
  CHARACTER(len=200) cErrorMsg      ! Each DElight Error Message can be up to 200 characters long
  LOGICAL   :: bEndofErrFile          ! End of Error File flag
  LOGICAL   :: bRecordsOnErrFile      ! true if there are records on the error file
  ! RJH DElight Modification End - local variable declarations

  INTEGER   :: NumReports, RepNum, NumNames, NumNumbers, IOStat

    ErrorsFound=.false.
    cCurrentModuleObject='Daylighting:Controls'
    TotDaylightingDetailed = GetNumObjectsFound(TRIM(cCurrentModuleObject))
    IF (TotDaylightingDetailed > 0) THEN
      CALL GetDaylightingParametersDetaild(TotDaylightingDetailed,ErrorsFound)
      CALL GetLightWellData(ErrorsFound)
      IF(ErrorsFound) CALL ShowFatalError('Program terminated for above reasons, related to DAYLIGHTING')
      CALL DayltgSetupAdjZoneListsAndPointers
    END IF

    DO SurfNum = 1, TotSurfaces
      IF(Surface(SurfNum)%Class /= SurfaceClass_Window) CYCLE
      ZoneNum = Surface(SurfNum)%Zone
      IF (ZoneDaylight(ZoneNum)%TotalDaylRefPoints > 0) THEN
        IF (.not. SurfaceWindow(SurfNum)%SurfDayLightInit) THEN
          ALLOCATE(SurfaceWindow(SurfNum)%SolidAngAtRefPt(MaxRefPoints))
          SurfaceWindow(SurfNum)%SolidAngAtRefPt=0.0
          ALLOCATE(SurfaceWindow(SurfNum)%SolidAngAtRefPtWtd(MaxRefPoints))
          SurfaceWindow(SurfNum)%SolidAngAtRefPtWtd=0.0
          ALLOCATE(SurfaceWindow(SurfNum)%IllumFromWinAtRefPt(MaxRefPoints,2))
          SurfaceWindow(SurfNum)%IllumFromWinAtRefPt=0.0
          ALLOCATE(SurfaceWindow(SurfNum)%BackLumFromWinAtRefPt(MaxRefPoints,2))
          SurfaceWindow(SurfNum)%BackLumFromWinAtRefPt=0.0
          ALLOCATE(SurfaceWindow(SurfNum)%SourceLumFromWinAtRefPt(MaxRefPoints,2))
          SurfaceWindow(SurfNum)%SourceLumFromWinAtRefPt=0.0
          SurfaceWindow(SurfNum)%SurfDayLightInit=.true.
        ENDIF
      ELSE
        SurfNumAdj = Surface(SurfNum)%ExtBoundCond
        IF (SurfNumAdj > 0) THEN
          ZoneNumAdj = Surface(SurfNumAdj)%Zone
          IF (ZoneDaylight(ZoneNumAdj)%TotalDaylRefPoints > 0) THEN
            IF (.not. SurfaceWindow(SurfNum)%SurfDayLightInit) THEN
              ALLOCATE(SurfaceWindow(SurfNum)%SolidAngAtRefPt(MaxRefPoints))
              SurfaceWindow(SurfNum)%SolidAngAtRefPt=0.0
              ALLOCATE(SurfaceWindow(SurfNum)%SolidAngAtRefPtWtd(MaxRefPoints))
              SurfaceWindow(SurfNum)%SolidAngAtRefPtWtd=0.0
              ALLOCATE(SurfaceWindow(SurfNum)%IllumFromWinAtRefPt(MaxRefPoints,2))
              SurfaceWindow(SurfNum)%IllumFromWinAtRefPt=0.0
              ALLOCATE(SurfaceWindow(SurfNum)%BackLumFromWinAtRefPt(MaxRefPoints,2))
              SurfaceWindow(SurfNum)%BackLumFromWinAtRefPt=0.0
              ALLOCATE(SurfaceWindow(SurfNum)%SourceLumFromWinAtRefPt(MaxRefPoints,2))
              SurfaceWindow(SurfNum)%SourceLumFromWinAtRefPt=0.0
              SurfaceWindow(SurfNum)%SurfDayLightInit=.true.
            ENDIF
          ENDIF
        ENDIF
      ENDIF

      IF (Surface(SurfNum)%ExtBoundCond == ExternalEnvironment) THEN

        WindowShadingControlPtr = Surface(SurfNum)%WindowShadingControlPtr
        IF(WindowShadingControlPtr > 0) THEN
          IF(WindowShadingControl(WindowShadingControlPtr)%GlareControlIsActive) THEN
            ! Error if GlareControlIsActive but window is not in a Daylighting:Detailed zone
            IF(ZoneDaylight(Surface(SurfNum)%Zone)%TotalDaylRefPoints == 0) THEN
              CALL ShowSevereError('Window='//TRIM(Surface(SurfNum)%Name)// ' has Window Shading Control with')
              CALL ShowContinueError('GlareControlIsActive = Yes but it is not in a Daylighting zone.')
              CALL ShowContinueError('Zone indicated='//TRIM(Zone(ZoneNum)%Name))
              ErrorsFound=.true.
            END IF
            ! Error if GlareControlIsActive and window is in a Daylighting:Detailed zone with
            ! an interior window adjacent to another Daylighting:Detailed zone
            IF(ZoneDaylight(ZoneNum)%TotalDaylRefPoints > 0) THEN
              DO IntWin = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
                SurfNumAdj = Surface(IntWin)%ExtBoundCond
                IF(Surface(IntWin)%Class == SurfaceClass_Window .AND. SurfNumAdj > 0) THEN
                  ZoneNumAdj = Surface(SurfNumAdj)%Zone
                  IF(ZoneDaylight(ZoneNumAdj)%TotalDaylRefPoints > 0) THEN
                    CALL ShowSevereError('Window='//TRIM(Surface(SurfNum)%Name)// ' has Window Shading Control with')
                    CALL ShowContinueError('GlareControlIsActive = Yes and is in a Daylighting zone')
                    CALL ShowContinueError('that shares an interior window with another Daylighting zone')
                    CALL ShowContinueError('Adjacent Zone indicated='//TRIM(Zone(ZoneNumAdj)%Name))
                    ErrorsFound=.true.
                  END IF
                END IF
              END DO
            END IF
          END IF

          IF(WindowShadingControl(WindowShadingControlPtr)%ShadingControlType == WSCT_MeetDaylIlumSetp) THEN
            ! Error if window has ShadingControlType = MeetDaylightingIlluminanceSetpoint &
            ! but is not in a Daylighting:Detailed zone
            IF(ZoneDaylight(Surface(SurfNum)%Zone)%TotalDaylRefPoints == 0) THEN
              CALL ShowSevereError('Window='//TRIM(Surface(SurfNum)%Name)// ' has Window Shading Control with')
              CALL ShowContinueError('MeetDaylightingIlluminanceSetpoint but it is not in a Daylighting zone.')
              CALL ShowContinueError('Zone indicated='//TRIM(Zone(ZoneNum)%Name))
              ErrorsFound=.true.
            END IF
            ! Error if window has ShadingControlType = MeetDaylightIlluminanceSetpoint and is in a &
            ! Daylighting:Detailed zone with an interior window adjacent to another Daylighting:Detailed zone
            IF(ZoneDaylight(ZoneNum)%TotalDaylRefPoints > 0) THEN
              DO IntWin = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
                SurfNumAdj = Surface(IntWin)%ExtBoundCond
                IF(Surface(IntWin)%Class == SurfaceClass_Window .AND. SurfNumAdj > 0) THEN
                  ZoneNumAdj = Surface(SurfNumAdj)%Zone
                  IF(ZoneDaylight(ZoneNumAdj)%TotalDaylRefPoints > 0) THEN
                    CALL ShowSevereError('Window='//TRIM(Surface(SurfNum)%Name)// ' has Window Shading Control with')
                    CALL ShowContinueError('MeetDaylightIlluminanceSetpoint and is in a Daylighting zone')
                    CALL ShowContinueError('that shares an interior window with another Daylighting zone')
                    CALL ShowContinueError('Adjacent Zone indicated='//TRIM(Zone(ZoneNumAdj)%Name))
                    ErrorsFound=.true.
                  END IF
                END IF
              END DO
            END IF
          END IF

        END IF
      END IF
    END DO

    ! RJH DElight Modification Begin - Calls to DElight preprocessing subroutines
    TotDaylightingDElight = GetNumObjectsFound('Daylighting:DELight:Controls')
    IF (TotDaylightingDElight .GT. 0) THEN
        dLatitude = Latitude
        CALL DisplayString('Calculating DElight Daylighting Factors')
        CALL DElightInputGenerator
        ! Init Error Flag to 0 (no Warnings or Errors)
        CALL DisplayString('ReturnFrom DElightInputGenerator')
        iErrorFlag = 0
        CALL DisplayString('Calculating DElight DaylightCoefficients')
        CALL DElightDaylightCoefficients(dLatitude, iErrorFlag)
        ! Check Error Flag for Warnings or Errors returning from DElight
        ! RJH 2008-03-07: open file for READWRITE and DELETE file after processing
        CALL DisplayString('ReturnFrom DElight DaylightCoefficients Calc')
        IF (iErrorFlag .NE. 0) THEN
            ! Open DElight Daylight Factors Error File for reading
            iDElightErrorFile=GetNewUnitNumber()
            Open (unit=iDElightErrorFile, file='eplusout.delightdfdmp', action='READWRITE')

            ! Sequentially read lines in DElight Daylight Factors Error File
            ! and process them using standard EPlus warning/error handling calls
            ! Process all error/warning messages first
            ! Then, if any error has occurred, ShowFatalError to terminate processing
            bEndofErrFile=.false.
            bRecordsOnErrFile=.false.
            DO WHILE (.not. bEndofErrFile)
                READ(iDElightErrorFile,'(A)',IOSTAT=iReadStatus) cErrorLine
                IF (iReadStatus < GoodIOStatValue) THEN
                    bEndofErrFile=.true.
                    CYCLE
                ENDIF
                bRecordsOnErrFile=.true.
                ! Is the current line a Warning message?
                IF (cErrorLine(1:9) == 'WARNING: ') THEN
                    cErrorMsg = cErrorLine(10:210)
                    cErrorMsg = TRIM(cErrorMsg)
                    CALL ShowWarningError(cErrorMsg)
                ENDIF
                ! Is the current line an Error message?
                IF (cErrorLine(1:7) == 'ERROR: ') THEN
                    cErrorMsg = cErrorLine(8:210)
                    cErrorMsg = TRIM(cErrorMsg)
                    CALL ShowSevereError(cErrorMsg)
                    iErrorFlag = 1
                ENDIF
            ENDDO

            ! Close and Delete DElight Error File
            IF (bRecordsOnErrFile) THEN
              Close (unit=iDElightErrorFile,Status='DELETE')
            ELSE
              Close (unit=iDElightErrorFile,Status='DELETE')
            ENDIF
            ! If any DElight Error occurred then ShowFatalError to terminate
            IF (iErrorFlag .GT. 0) THEN
                ErrorsFound=.true.
            ENDIF
        ELSE
            ! Open, Close, and Delete DElight Daylight Factors Error File for reading
            iDElightErrorFile=GetNewUnitNumber()
            Open (unit=iDElightErrorFile, file='eplusout.delightdfdmp', action='READWRITE')
            Close (unit=iDElightErrorFile,Status='DELETE')
        ENDIF
        CALL SetupDElightOutput4EPlus
    ENDIF
    ! RJH DElight Modification End - Calls to DElight preprocessing subroutines

    ! TH 6/3/2010, added to report daylight factors
    cCurrentModuleObject = 'Output:DaylightFactors'
    NumReports = GetNumObjectsFound(TRIM(cCurrentModuleObject))
    IF (NumReports > 0) THEN
      CALL GetObjectItem(TRIM(cCurrentModuleObject),1,cAlphaArgs,NumNames,rNumericArgs,NumNumbers, &
                     IOStat, AlphaBlank=lAlphaFieldBlanks, NumBlank=lNumericFieldBlanks,  &
                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IF (cAlphaArgs(1)(1:10) == 'SIZINGDAYS') THEN
        DFSReportSizingDays = .true.
      ELSEIF (cAlphaArgs(1)(1:24) == 'ALLSHADOWCALCULATIONDAYS') THEN
        DFSReportAllShadowCalculationDays = .true.
      ENDIF
    ENDIF

  IF(ErrorsFound) CALL ShowFatalError('Program terminated for above reasons')

  RETURN

END SUBROUTINE GetDaylightingParametersInput

SUBROUTINE GetDaylightingParametersDetaild(TotDaylightingDetailed,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   March 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Obtain the user input data for Daylighting:Detailed objects in the input file.
          ! For detailed daylighting, a calculation of interior daylight illuminance is done at one
          !    or two reference points; the illuminance level, setpoint and type of control
          !    system determines lighting power reduction.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! none

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor,   ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, FindIteminList
  USE DataStringGlobals, ONLY: CharSpace, CharComma, CharTab
  USE InternalHeatGains, ONLY: CheckLightsReplaceableMinMaxForZone
  USE General, ONLY: TrimSigDigits, RoundSigDigits
  USE OutputReportPredefined


  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: TotDaylightingDetailed ! Total "simple" daylighting inputs
  LOGICAL, INTENT(INOUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: Blank=' '

          ! INTERFACE BLOCK SPECIFICATIONS: na
          ! DERIVED TYPE DEFINITIONS: na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                                    :: IOStat
  INTEGER                                    :: Loop1
  INTEGER                                    :: MapStyleIn
  INTEGER                                    :: NumAlpha
  INTEGER                                    :: NumNumber
  INTEGER                                    :: ZoneNum
  INTEGER                                    :: MapNum
  INTEGER                                    :: RefPt
  INTEGER                                    :: X, Y
  INTEGER                                    :: SurfLoop
  INTEGER, EXTERNAL                          :: FindNumberInList
  INTEGER                                    :: AddMapPoints
  INTEGER                                    :: ZoneFound
  CHARACTER(len=MaxNameLength)               :: refName
  REAL(r64) :: CosBldgRelNorth        ! Cosine of Building rotation
  REAL(r64) :: SinBldgRelNorth        ! Sine of Building rotation
  REAL(r64) :: CosZoneRelNorth        ! Cosine of Zone rotation
  REAL(r64) :: SinZoneRelNorth        ! Sine of Zone rotation
  REAL(r64) :: CosBldgRotAppGonly =0.0D0 ! Cosine of the building rotation for appendix G only (relative north)
  REAL(r64) :: SinBldgRotAppGonly =0.0D0 ! Sine of the building rotation for appendix G only (relative north)
  REAL(r64) :: Xb                     ! temp var for transformation calc
  REAL(r64) :: Yb                     ! temp var for transformation calc
  REAL(r64) :: Xo, XnoRot, Xtrans
  REAL(r64) :: Yo, YnoRot, Ytrans
  Logical   :: doTransform
  REAL(r64) :: OldAspectRatio
  REAL(r64) :: NewAspectRatio

          ! FLOW:

    ! Calc cos and sin of Building Relative North values for later use in transforming Reference Point coordinates
    CosBldgRelNorth = COS(-(BuildingAzimuth + BuildingRotationAppendixG)*DegToRadians)
    SinBldgRelNorth = SIN(-(BuildingAzimuth + BuildingRotationAppendixG)*DegToRadians)
    ! these are only for Building Rotation for Appendix G when using world coordinate system
    CosBldgRotAppGonly = COS(-BuildingRotationAppendixG*DegToRadians)
    SinBldgRotAppGonly = SIN(-BuildingRotationAppendixG*DegToRadians)

  doTransform=.false.
  OldAspectRatio=1.0
  NewAspectRatio=1.0

  CALL CheckForGeometricTransform(DoTransform,OldAspectRatio,NewAspectRatio)

  ! Get and initialize illuminance map objects
  cCurrentModuleObject='Output:IlluminanceMap'
  TotIllumMaps = GetNumObjectsFound(TRIM(cCurrentModuleObject))

  ALLOCATE(IllumMap(TotIllumMaps))

  IF (TotIllumMaps > 0) THEN
    DO MapNum = 1, TotIllumMaps
      CALL GetObjectItem(TRIM(cCurrentModuleObject),MapNum,cAlphaArgs,NumAlpha,rNumericArgs,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IllumMap(MapNum)%Name = cAlphaArgs(1)
      IllumMap(MapNum)%Zone = FindIteminList(cAlphaArgs(2),Zone%Name,NumOfZones)

      IF (IllumMap(MapNum)%Zone == 0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                             '", invalid '//TRIM(cAlphaFieldNames(2))//'="'//  &
                             TRIM(cAlphaArgs(2))//'".')
        ErrorsFound = .TRUE.
      ENDIF

      IllumMap(MapNum)%Z = rNumericArgs(1)

      IllumMap(MapNum)%Xmin = rNumericArgs(2)
      IllumMap(MapNum)%Xmax = rNumericArgs(3)
      IF (rNumericArgs(2) > rNumericArgs(3)) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                             '", invalid entry.')
        CALL ShowContinueError('...'//trim(cNumericFieldNames(2))//'['//trim(RoundSigDigits(rNumericArgs(2),2))// &
                    '] must be <= '//trim(cNumericFieldNames(3))//'['//trim(RoundSigDigits(rNumericArgs(3),2))//'].')
        ErrorsFound = .TRUE.
      ENDIF
      IllumMap(MapNum)%Xnum = rNumericArgs(4)
      IF (IllumMap(MapNum)%Xnum /= 1) THEN
        IllumMap(MapNum)%Xinc = (IllumMap(MapNum)%Xmax - IllumMap(MapNum)%Xmin) / (IllumMap(MapNum)%Xnum - 1)
      ELSE
        IllumMap(MapNum)%Xinc = 0.0
      ENDIF

      IllumMap(MapNum)%Ymin = rNumericArgs(5)
      IllumMap(MapNum)%Ymax = rNumericArgs(6)
      IF (rNumericArgs(5) > rNumericArgs(6)) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                             '", invalid entry.')
        CALL ShowContinueError('...'//trim(cNumericFieldNames(5))//'['//trim(RoundSigDigits(rNumericArgs(5),2))// &
                    '] must be <= '//trim(cNumericFieldNames(6))//'['//trim(RoundSigDigits(rNumericArgs(6),2))//'].')
        ErrorsFound = .TRUE.
      ENDIF
      IllumMap(MapNum)%Ynum = rNumericArgs(7)
      IF (IllumMap(MapNum)%Ynum /= 1) THEN
        IllumMap(MapNum)%Yinc = (IllumMap(MapNum)%Ymax - IllumMap(MapNum)%Ymin) / (IllumMap(MapNum)%Ynum - 1)
      ELSE
        IllumMap(MapNum)%Yinc = 0.0
      ENDIF
      IF (IllumMap(MapNum)%Xnum*IllumMap(MapNum)%Ynum > MaxMapRefPoints) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                             '", too many map points specified.')
        CALL ShowContinueError('...'//trim(cNumericFieldNames(4))//'['//trim(RoundSigDigits(IllumMap(MapNum)%Xnum))// &
                    '] * '//trim(cNumericFieldNames(7))//'['//trim(RoundSigDigits(IllumMap(MapNum)%Ynum))//'].'// &
                    '= ['//TRIM(RoundSigDigits(IllumMap(MapNum)%Xnum*IllumMap(MapNum)%Ynum))//'] must be <= ['//  &
                    TRIM(RoundSigDigits(MaxMapRefPoints))//'].')
        ErrorsFound=.true.
      ENDIF
    END DO ! MapNum

    cCurrentModuleObject='OutputControl:IlluminanceMap:Style'
    MapStyleIn = GetNumObjectsFound(TRIM(cCurrentModuleObject))

    IF (MapStyleIn == 0) THEN
      cAlphaArgs(1)='COMMA'
      MapColSep = CharComma !comma
    ELSEIF (MapStyleIn == 1) THEN
      CALL GetObjectItem(TRIM(cCurrentModuleObject),1,cAlphaArgs,NumAlpha,rNumericArgs,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      IF (cAlphaArgs(1) == 'COMMA') THEN
        MapColSep = CharComma !comma
      ELSEIF (cAlphaArgs(1) == 'TAB') THEN
        MapColSep = CharTab  !tab
      ELSEIF (cAlphaArgs(1) == 'FIXED' .or. cAlphaArgs(1) == 'SPACE') THEN
        MapColSep = CharSpace ! space
      ELSE
        MapColSep = CharComma !comma
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(1))//'="'//  &
                      TRIM(cAlphaArgs(1))//'", Commas will be used to separate fields.')
        cAlphaArgs(1)='COMMA'
      ENDIF
    ENDIF
    Write(OutputFileInits,'(A)') '! <Daylighting:Illuminance Maps>,#Maps,Style'
    CALL ConvertCaseToLower(cAlphaArgs(1),cAlphaArgs(2))
    cAlphaArgs(1)(2:)=cAlphaArgs(2)(2:)
    WRITE(OutputFileInits,"('Daylighting:Illuminance Maps,',A,',',A)") TRIM(TrimSigDigits(TotIllumMaps)),TRIM(cAlphaArgs(1))

  END IF

  cCurrentModuleObject='Daylighting:Controls'
  DO Loop1 = 1, TotDaylightingDetailed
    cAlphaArgs='  '
    rNumericArgs=0.0
    CALL GetObjectItem(TRIM(cCurrentModuleObject),Loop1,cAlphaArgs,NumAlpha,rNumericArgs,NumNumber,IOStat,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    ! First is Zone Name
    ZoneFound=FindIteminList(cAlphaArgs(1),Zone%Name,NumOfZones)
    IF (ZoneFound == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(1))//'="'//  &
         TRIM(cAlphaArgs(1))//'".')
      ErrorsFound=.true.
      CYCLE
    END IF

    ! Calc cos and sin of Zone Relative North values for later use in transforming Reference Point coordinates
    CosZoneRelNorth = COS(-Zone(ZoneFound)%RelNorth*DegToRadians)
    SinZoneRelNorth = SIN(-Zone(ZoneFound)%RelNorth*DegToRadians)

    IF (ZoneDaylight(ZoneFound)%DaylightType /= NoDaylighting) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//  &
         ': Attempted to apply Detailed Daylighting to a Zone with Previous Daylighting')
      CALL ShowContinueError('Error discovered for Zone='//TRIM(cAlphaArgs(1)))
      CALL ShowContinueError('Previously applied Daylighting Type='//TRIM(DaylightTypes(ZoneDaylight(ZoneFound)%DaylightType)))
      ErrorsFound=.true.
      CYCLE
    ENDIF
    ZoneDaylight(ZoneFound)%DaylightType=DetailedDaylighting
    ZoneDaylight(ZoneFound)%TotalDaylRefPoints=rNumericArgs(1)

    CALL CheckLightsReplaceableMinMaxForZone(ZoneFound)

    ALLOCATE(ZoneDaylight(ZoneFound)%DaylRefPtAbsCoord(MaxRefPoints,3))
    ZoneDaylight(ZoneFound)%DaylRefPtAbsCoord   =0.0
    ALLOCATE(ZoneDaylight(ZoneFound)%DaylRefPtInBounds(MaxRefPoints))
    ZoneDaylight(ZoneFound)%DaylRefPtInBounds   =.true.
    ALLOCATE(ZoneDaylight(ZoneFound)%FracZoneDaylit(MaxRefPoints))
    ZoneDaylight(ZoneFound)%FracZoneDaylit       =0.0
    ALLOCATE(ZoneDaylight(ZoneFound)%IllumSetPoint(MaxRefPoints))
    ZoneDaylight(ZoneFound)%IllumSetPoint        =0.0
    ALLOCATE(ZoneDaylight(ZoneFound)%RefPtPowerReductionFactor(MaxRefPoints))
    ZoneDaylight(ZoneFound)%RefPtPowerReductionFactor =1.0
    ALLOCATE(ZoneDaylight(ZoneFound)%DaylIllumAtRefPt(MaxRefPoints))
    ZoneDaylight(ZoneFound)%DaylIllumAtRefPt     =0.0
    ALLOCATE(ZoneDaylight(ZoneFound)%GlareIndexAtRefPt(MaxRefPoints))
    ZoneDaylight(ZoneFound)%GlareIndexAtRefPt    =0.0
    ALLOCATE(ZoneDaylight(ZoneFound)%BacLum(MaxRefPoints))
    ZoneDaylight(ZoneFound)%BacLum               =0.0

    !added TH 12/2/2008
    ALLOCATE(ZoneDaylight(ZoneFound)%TimeExceedingGlareIndexSPAtRefPt(MaxRefPoints))
    ZoneDaylight(ZoneFound)%TimeExceedingGlareIndexSPAtRefPt = 0.0

    !added TH 7/6/2009
    ALLOCATE(ZoneDaylight(ZoneFound)%TimeExceedingDaylightIlluminanceSPAtRefPt(MaxRefPoints))
    ZoneDaylight(ZoneFound)%TimeExceedingDaylightIlluminanceSPAtRefPt = 0.0

    IF (ZoneDaylight(ZoneFound)%TotalDaylRefPoints >= 1) THEN
      IF (DaylRefWorldCoordSystem) THEN
        !transform only by appendix G rotation
        ZoneDaylight(ZoneFound)%DaylRefPtAbsCoord(1,1) = rNumericArgs(2)*CosBldgRotAppGonly - rNumericArgs(3)*SinBldgRotAppGonly
        ZoneDaylight(ZoneFound)%DaylRefPtAbsCoord(1,2) = rNumericArgs(2)*SinBldgRotAppGonly + rNumericArgs(3)*CosBldgRotAppGonly
        ZoneDaylight(ZoneFound)%DaylRefPtAbsCoord(1,3) = rNumericArgs(4)
      ELSE
        !Transform reference point coordinates into building coordinate system
        Xb = rNumericArgs(2)*CosZoneRelNorth &
            - rNumericArgs(3)*SinZoneRelNorth &
            + Zone(ZoneFound)%OriginX
        Yb = rNumericArgs(2)*SinZoneRelNorth &
            + rNumericArgs(3)*CosZoneRelNorth &
            + Zone(ZoneFound)%OriginY
        !Transform into World Coordinate System
        ZoneDaylight(ZoneFound)%DaylRefPtAbsCoord(1,1) = Xb*CosBldgRelNorth - Yb*SinBldgRelNorth
        ZoneDaylight(ZoneFound)%DaylRefPtAbsCoord(1,2) = Xb*SinBldgRelNorth + Yb*CosBldgRelNorth
        ZoneDaylight(ZoneFound)%DaylRefPtAbsCoord(1,3) = rNumericArgs(4) + &
                                Zone(ZoneFound)%OriginZ
        IF (doTransform) THEN
          Xo = ZoneDaylight(ZoneFound)%DaylRefPtAbsCoord(1,1) ! world coordinates.... shifted by relative north angle...
          Yo = ZoneDaylight(ZoneFound)%DaylRefPtAbsCoord(1,2)
          ! next derotate the building
          XnoRot=Xo * CosBldgRelNorth + Yo * SinBldgRelNorth
          YnoRot=Yo * CosBldgRelNorth - Xo * SinBldgRelNorth
          ! translate
          Xtrans = XnoRot * SQRT(NewAspectRatio/OldAspectRatio)
          Ytrans = YnoRot * SQRT(OldAspectRatio/NewAspectRatio)
          ! rerotate
          ZoneDaylight(ZoneFound)%DaylRefPtAbsCoord(1,1) = Xtrans * CosBldgRelNorth - Ytrans * SinBldgRelNorth

          ZoneDaylight(ZoneFound)%DaylRefPtAbsCoord(1,2) = Xtrans * SinBldgRelNorth + Ytrans * CosBldgRelNorth
        ENDIF
      ENDIF
      ZoneDaylight(ZoneFound)%FracZoneDaylit(1) = rNumericArgs(8)
      ZoneDaylight(ZoneFound)%IllumSetPoint(1) = rNumericArgs(10)
    ENDIF
    IF (ZoneDaylight(ZoneFound)%TotalDaylRefPoints >= 2) THEN
      IF (DaylRefWorldCoordSystem) THEN
        !transform only by appendix G rotation
        ZoneDaylight(ZoneFound)%DaylRefPtAbsCoord(2,1) = rNumericArgs(5)*CosBldgRotAppGonly - rNumericArgs(6)*SinBldgRotAppGonly
        ZoneDaylight(ZoneFound)%DaylRefPtAbsCoord(2,2) = rNumericArgs(5)*SinBldgRotAppGonly + rNumericArgs(6)*CosBldgRotAppGonly
        ZoneDaylight(ZoneFound)%DaylRefPtAbsCoord(2,3) = rNumericArgs(7)
      ELSE
        !Transform reference point coordinates into building coordinate system
        Xb = rNumericArgs(5)*CosZoneRelNorth &
            - rNumericArgs(6)*SinZoneRelNorth &
            + Zone(ZoneFound)%OriginX
        Yb = rNumericArgs(5)*SinZoneRelNorth &
            + rNumericArgs(6)*CosZoneRelNorth &
            + Zone(ZoneFound)%OriginY
        !Transform into World Coordinate System
        ZoneDaylight(ZoneFound)%DaylRefPtAbsCoord(2,1) = Xb*CosBldgRelNorth - Yb*SinBldgRelNorth
        ZoneDaylight(ZoneFound)%DaylRefPtAbsCoord(2,2) = Xb*SinBldgRelNorth + Yb*CosBldgRelNorth
        ZoneDaylight(ZoneFound)%DaylRefPtAbsCoord(2,3) = rNumericArgs(7) + &
                                Zone(ZoneFound)%OriginZ
        IF (doTransform) THEN
          Xo = ZoneDaylight(ZoneFound)%DaylRefPtAbsCoord(2,1) ! world coordinates.... shifted by relative north angle...
          Yo = ZoneDaylight(ZoneFound)%DaylRefPtAbsCoord(2,2)
          ! next derotate the building
          XnoRot=Xo * CosBldgRelNorth + Yo * SinBldgRelNorth
          YnoRot=Yo * CosBldgRelNorth - Xo * SinBldgRelNorth
          ! translate
          Xtrans = XnoRot * SQRT(NewAspectRatio/OldAspectRatio)
          Ytrans = YnoRot * SQRT(OldAspectRatio/NewAspectRatio)
          ! rerotate
          ZoneDaylight(ZoneFound)%DaylRefPtAbsCoord(2,1) = Xtrans * CosBldgRelNorth - Ytrans * SinBldgRelNorth

          ZoneDaylight(ZoneFound)%DaylRefPtAbsCoord(2,2) = Xtrans * SinBldgRelNorth + Ytrans * CosBldgRelNorth
        ENDIF
      ENDIF
      ZoneDaylight(ZoneFound)%FracZoneDaylit(2) = rNumericArgs(9)
      ZoneDaylight(ZoneFound)%IllumSetPoint(2) = rNumericArgs(11)
    ENDIF
    do refpt=1,ZoneDaylight(ZoneFound)%TotalDaylRefPoints
      IF (ZoneDaylight(ZoneFound)%DaylrefptAbsCoord(refpt,1) < Zone(ZoneFound)%MinimumX .or.  &
          ZoneDaylight(ZoneFound)%DaylrefptAbsCoord(refpt,1) > Zone(ZoneFound)%MaximumX) THEN
        ZoneDaylight(ZoneFound)%DaylrefptInBounds(refpt)=.false.
        CALL ShowWarningError('GetDetailedDaylighting: Reference point X Value outside Zone Min/Max X, Zone='//  &
             TRIM(Zone(ZoneFound)%Name))
        CALL ShowContinueError('...X Reference Point= '//                                                        &
               TRIM(RoundSigDigits(ZoneDaylight(ZoneFound)%DaylrefptAbsCoord(refpt,1),2))//', Zone Minimum X= '//  &
               TRIM(RoundSigDigits(Zone(ZoneFound)%MinimumX,2))//', Zone Maximum X= '//                             &
               TRIM(RoundSigDigits(Zone(ZoneFound)%MaximumX,2)))
        IF (ZoneDaylight(ZoneFound)%DaylrefptAbsCoord(RefPt,1) < Zone(ZoneFound)%MinimumX) THEN
          CALL ShowContinueError('...X Reference Distance Outside MinimumX= '//  &
               TRIM(RoundSigDigits(Zone(ZoneFound)%MinimumX-ZoneDaylight(ZoneFound)%DaylrefptAbsCoord(RefPt,1),4))//' m.')
        ELSE
          CALL ShowContinueError('...X Reference Distance Outside MaximumX= '//  &
               TRIM(RoundSigDigits(ZoneDaylight(ZoneFound)%DaylrefptAbsCoord(RefPt,1)-Zone(ZoneFound)%MaximumX,4))//' m.')
        ENDIF
      ENDIF
      IF (ZoneDaylight(ZoneFound)%DaylrefptAbsCoord(refpt,2) < Zone(ZoneFound)%MinimumY .or.  &
          ZoneDaylight(ZoneFound)%DaylrefptAbsCoord(refpt,2) > Zone(ZoneFound)%MaximumY) THEN
        ZoneDaylight(ZoneFound)%DaylrefptInBounds(refpt)=.false.
        CALL ShowWarningError('GetDetailedDaylighting: Reference point Y Value outside Zone Min/Max Y, Zone='//  &
             TRIM(Zone(ZoneFound)%Name))
        CALL ShowContinueError('...Y Reference Point= '//                                                        &
               TRIM(RoundSigDigits(ZoneDaylight(ZoneFound)%DaylrefptAbsCoord(refpt,2),2))//', Zone Minimum Y= '//  &
               TRIM(RoundSigDigits(Zone(ZoneFound)%MinimumY,2))//', Zone Maximum Y= '//                             &
               TRIM(RoundSigDigits(Zone(ZoneFound)%MaximumY,2)))
        IF (ZoneDaylight(ZoneFound)%DaylrefptAbsCoord(RefPt,2) < Zone(ZoneFound)%MinimumY) THEN
          CALL ShowContinueError('...Y Reference Distance Outside MinimumY= '//  &
               TRIM(RoundSigDigits(Zone(ZoneFound)%MinimumY-ZoneDaylight(ZoneFound)%DaylrefptAbsCoord(RefPt,2),4))//' m.')
        ELSE
          CALL ShowContinueError('...Y Reference Distance Outside MaximumY= '//  &
               TRIM(RoundSigDigits(ZoneDaylight(ZoneFound)%DaylrefptAbsCoord(RefPt,2)-Zone(ZoneFound)%MaximumY,4))//' m.')
        ENDIF
      ENDIF
      IF (ZoneDaylight(ZoneFound)%DaylrefptAbsCoord(refpt,3) < Zone(ZoneFound)%MinimumZ .or.  &
          ZoneDaylight(ZoneFound)%DaylrefptAbsCoord(refpt,3) > Zone(ZoneFound)%MaximumZ) THEN
        ZoneDaylight(ZoneFound)%DaylrefptInBounds(refpt)=.false.
        CALL ShowWarningError('GetDetailedDaylighting: Reference point Z Value outside Zone Min/Max Z, Zone='//  &
             TRIM(Zone(ZoneFound)%Name))
        CALL ShowContinueError('...Z Reference Point= '//                                                        &
               TRIM(RoundSigDigits(ZoneDaylight(ZoneFound)%DaylrefptAbsCoord(refpt,3),2))//', Zone Minimum Z= '//  &
               TRIM(RoundSigDigits(Zone(ZoneFound)%MinimumZ,2))//', Zone Maximum Z= '//                             &
               TRIM(RoundSigDigits(Zone(ZoneFound)%MaximumZ,2)))
        IF (ZoneDaylight(ZoneFound)%DaylrefptAbsCoord(RefPt,3) < Zone(ZoneFound)%MinimumZ) THEN
          CALL ShowContinueError('...Z Reference Distance Outside MinimumZ= '//  &
               TRIM(RoundSigDigits(Zone(ZoneFound)%MinimumZ-ZoneDaylight(ZoneFound)%DaylrefptAbsCoord(RefPt,3),4))//' m.')
        ELSE
          CALL ShowContinueError('...Z Reference Distance Outside MaximumZ= '//  &
               TRIM(RoundSigDigits(ZoneDaylight(ZoneFound)%DaylrefptAbsCoord(RefPt,3)-Zone(ZoneFound)%MaximumZ,4))//' m.')
        ENDIF
      ENDIF
    END DO ! RefPt
    IF (SUM(ZoneDaylight(ZoneFound)%FracZoneDaylit) < 1.0) THEN
      CALL ShowWarningError('GetDetailedDaylighting: Fraction of Zone controlled by the Daylighting reference points is < 1.0.')
      CALL ShowContinueError('..discovered in "'//TRIM(cCurrentModuleObject)//'" for Zone="'//TRIM(cAlphaArgs(1))//'", only '//  &
                    TRIM(RoundSigDigits(SUM(ZoneDaylight(ZoneFound)%FracZoneDaylit),2))//' of the zone is controlled.')
    ENDIF
    IF (SUM(ZoneDaylight(ZoneFound)%FracZoneDaylit) > 1.0) THEN
      CALL ShowSevereError('GetDetailedDaylighting: Fraction of Zone controlled by the Daylighting reference points is > 1.0.')
      CALL ShowContinueError('..discovered in "'//TRIM(cCurrentModuleObject)//'" for Zone="'//TRIM(cAlphaArgs(1))//  &
         '", trying to control '//TRIM(RoundSigDigits(SUM(ZoneDaylight(ZoneFound)%FracZoneDaylit),2))//' of the zone.')
      ErrorsFound=.true.
    ENDIF
    ZoneDaylight(ZoneFound)%LightControlType=rNumericArgs(12)  ! Relies on IDD limits for verification
    ZoneDaylight(ZoneFound)%ViewAzimuthForGlare=rNumericArgs(13)
    ZoneDaylight(ZoneFound)%MaxGlareallowed=rNumericArgs(14)
    ZoneDaylight(ZoneFound)%MinPowerFraction=rNumericArgs(15)
    ZoneDaylight(ZoneFound)%MinLightFraction=rNumericArgs(16)
    ZoneDaylight(ZoneFound)%LightControlSteps=rNumericArgs(17)
    IF (ZoneDaylight(ZoneFound)%LightControlType == 2 .and. ZoneDaylight(ZoneFound)%LightControlSteps <= 0) THEN
      CALL ShowWarningError('GetDetailedDaylighting: For Stepped Control, the number of steps must be > 0')
      CALL ShowContinueError('..discovered in "'//TRIM(cCurrentModuleObject)//'" for Zone="'//TRIM(cAlphaArgs(1))//  &
         '", will use 1')
      ZoneDaylight(ZoneFound)%LightControlSteps=1
    ENDIF
    ZoneDaylight(ZoneFound)%LightControlProbability=rNumericArgs(18)

    IF (ZoneDaylight(ZoneFound)%TotalDaylRefPoints .GE. 1) THEN
      refName = TRIM(cAlphaArgs(1)) // ' - REF 1'
      CALL PreDefTableEntry(pdchDyLtZone,refName,cAlphaArgs(1))
      CALL PreDefTableEntry(pdchDyLtKind,refName,'Detailed')
      ! (1=continuous, 2=stepped, 3=continuous/off)
      SELECT CASE (ZoneDaylight(ZoneFound)%LightControlType)
        CASE (1)
          CALL PreDefTableEntry(pdchDyLtCtrl,refName,'Continuous')
        CASE (2)
          CALL PreDefTableEntry(pdchDyLtCtrl,refName,'Stepped')
        CASE (3)
          CALL PreDefTableEntry(pdchDyLtCtrl,refName,'Continuous/Off')
      END SELECT
      CALL PreDefTableEntry(pdchDyLtFrac,refName,ZoneDaylight(ZoneFound)%FracZoneDaylit(1))
    END IF
    IF (ZoneDaylight(ZoneFound)%TotalDaylRefPoints .GE. 2) THEN
      refName = TRIM(cAlphaArgs(1)) // ' - REF 2'
      CALL PreDefTableEntry(pdchDyLtZone,refName,cAlphaArgs(1))
      CALL PreDefTableEntry(pdchDyLtKind,refName,'Detailed')
      ! (1=continuous, 2=stepped, 3=continuous/off)
      SELECT CASE (ZoneDaylight(ZoneFound)%LightControlType)
        CASE (1)
          CALL PreDefTableEntry(pdchDyLtCtrl,refName,'Continuous')
        CASE (2)
          CALL PreDefTableEntry(pdchDyLtCtrl,refName,'Stepped')
        CASE (3)
          CALL PreDefTableEntry(pdchDyLtCtrl,refName,'Continuous/Off')
      END SELECT
      CALL PreDefTableEntry(pdchDyLtFrac,refName,ZoneDaylight(ZoneFound)%FracZoneDaylit(2))
    END IF

    ! Check for illuminance maps associated with this zone
    DO MapNum = 1, TotIllumMaps
      ! For now this will only process the first map found for a zone
      ! Later could do more than one map per zone
      IF (IllumMap(MapNum)%Zone == ZoneFound) THEN

        IF (ALLOCATED(ZoneDaylight(ZoneFound)%MapRefPtAbsCoord)) THEN ! protect hard crash if more than one map per zone
          CALL ShowWarningError('GetDetailedDaylighting: found multiple illuminance maps in a single zone')
          CALL ShowContinueError('Zone name ='//TRIM(Zone(ZoneFound)%Name))
          Call ShowContinueError('Only the first illuminance map requested will be generated. The simulation continues.')
          CYCLE
        ENDIF
        IF (IllumMap(MapNum)%Xnum*IllumMap(MapNum)%Ynum > 0)  THEN
          ! Add additional daylighting reference points for map
          AddMapPoints = IllumMap(MapNum)%Xnum*IllumMap(MapNum)%Ynum
          ZoneDaylight(ZoneFound)%TotalMapRefPoints=AddMapPoints
          ALLOCATE(ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(AddMapPoints,3))
          ZoneDaylight(ZoneFound)%MapRefPtAbsCoord=0.0
          ALLOCATE(ZoneDaylight(ZoneFound)%MapRefPtInBounds(AddMapPoints))
          ZoneDaylight(ZoneFound)%MapRefPtInBounds   =.true.
          ALLOCATE(ZoneDaylight(ZoneFound)%DaylIllumAtMapPt(AddMapPoints))
          ZoneDaylight(ZoneFound)%DaylIllumAtMapPt=0.0
          ALLOCATE(ZoneDaylight(ZoneFound)%GlareIndexAtMapPt(AddMapPoints))
          ZoneDaylight(ZoneFound)%GlareIndexAtMapPt=0.0
          ALLOCATE(ZoneDaylight(ZoneFound)%DaylIllumAtMapPtHr(AddMapPoints))
          ZoneDaylight(ZoneFound)%DaylIllumAtMapPtHr=0.0
          ALLOCATE(ZoneDaylight(ZoneFound)%GlareIndexAtMapPtHr(AddMapPoints))
          ZoneDaylight(ZoneFound)%GlareIndexAtMapPtHr=0.0

          IF (AddMapPoints > MaxMapRefPoints) THEN
            CALL ShowSevereError('GetDaylighting Parameters: Total Map Reference points entered is greater than maximum allowed.')
            CALL ShowContinueError('Occurs in Zone='//TRIM(Zone(ZoneFound)%Name))
            CALL ShowContinueError('Maximum reference points allowed='//  &
                                   TRIM(TrimSigDigits(MaxMapRefPoints))//  &
                                   ', entered amount (when error first occurred)='//  &
                                   TRIM(TrimSigDigits(AddMapPoints)))
            ErrorsFound=.true.
            EXIT
          ENDIF
          RefPt=1
          ! Calc cos and sin of Zone Relative North values for later use in transforming Map Point coordinates
          CosZoneRelNorth = COS(-Zone(ZoneFound)%RelNorth*DegToRadians)
          SinZoneRelNorth = SIN(-Zone(ZoneFound)%RelNorth*DegToRadians)
          IF (IllumMap(MapNum)%Xnum /= 1) THEN
            IllumMap(MapNum)%Xinc = (IllumMap(MapNum)%Xmax - IllumMap(MapNum)%Xmin) / (IllumMap(MapNum)%Xnum - 1)
          ELSE
            IllumMap(MapNum)%Xinc = 0.0
          ENDIF
          IF (IllumMap(MapNum)%Ynum /= 1) THEN
            IllumMap(MapNum)%Yinc = (IllumMap(MapNum)%Ymax - IllumMap(MapNum)%Ymin) / (IllumMap(MapNum)%Ynum - 1)
          ELSE
            IllumMap(MapNum)%Yinc = 0.0
          ENDIF

          ! Map points and increments are stored in AbsCoord and then that is operated on if relative coords entered.
          DO Y = 1, IllumMap(MapNum)%Ynum
            DO X = 1, IllumMap(MapNum)%Xnum
              ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,1) = IllumMap(MapNum)%Xmin + (X - 1)*IllumMap(MapNum)%Xinc
              ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,2) = IllumMap(MapNum)%Ymin + (Y - 1)*IllumMap(MapNum)%Yinc
              ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,3) = IllumMap(MapNum)%Z
              RefPt = RefPt + 1
            ENDDO
          ENDDO
          RefPt=1
          DO Y = 1, IllumMap(MapNum)%Ynum
            DO X = 1, IllumMap(MapNum)%Xnum
              IF (.not. DaylRefWorldCoordSystem) THEN
                Xb = ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,1) * CosZoneRelNorth &
                     - ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,2) * SinZoneRelNorth &
                     + Zone(ZoneFound)%OriginX
                Yb = ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,1) * SinZoneRelNorth &
                     + ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,2) * CosZoneRelNorth &
                     + Zone(ZoneFound)%OriginY
                ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,1) = Xb * CosBldgRelNorth - Yb * SinBldgRelNorth
                ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,2) = Xb * SinBldgRelNorth + Yb * CosBldgRelNorth
                ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,3) = ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,3) +  &
                             Zone(ZoneFound)%OriginZ
                IF (doTransform) THEN
                  Xo = ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,1) ! world coordinates.... shifted by relative north angle...
                  Yo = ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,2)
                  ! next derotate the building
                  XnoRot=Xo * CosBldgRelNorth + Yo * SinBldgRelNorth
                  YnoRot=Yo * CosBldgRelNorth - Xo * SinBldgRelNorth
                  ! translate
                  Xtrans = XnoRot * SQRT(NewAspectRatio/OldAspectRatio)
                  Ytrans = YnoRot * SQRT(OldAspectRatio/NewAspectRatio)
                  ! rerotate
                  ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,1) = Xtrans * CosBldgRelNorth - Ytrans * SinBldgRelNorth

                  ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,2) = Xtrans * SinBldgRelNorth + Ytrans * CosBldgRelNorth
                ENDIF
              ELSE
                Xb = ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,1)
                Yb = ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,2)
                ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,1) = Xb * CosBldgRotAppGonly - Yb * SinBldgRotAppGonly
                ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,2) = Xb * SinBldgRotAppGonly + Yb * CosBldgRotAppGonly
              ENDIF
              IF (RefPt == 1) THEN
                IllumMap(MapNum)%Xmin=ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,1)
                IllumMap(MapNum)%Ymin=ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,2)
                IllumMap(MapNum)%Xmax=ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,1)
                IllumMap(MapNum)%Ymax=ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,2)
                IllumMap(MapNum)%Z   =ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,3)
              ENDIF
              IllumMap(MapNum)%Xmin=MIN(IllumMap(MapNum)%Xmin,ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,1))
              IllumMap(MapNum)%Ymin=MIN(IllumMap(MapNum)%Ymin,ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,2))
              IllumMap(MapNum)%Xmax=MAX(IllumMap(MapNum)%Xmax,ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,1))
              IllumMap(MapNum)%Ymax=MAX(IllumMap(MapNum)%Ymax,ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,2))
              IF ((ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,1) < Zone(ZoneFound)%MinimumX .and.          &
                  (Zone(ZoneFound)%MinimumX - ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,1)) > .001) .or.  &
                  (ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,1) > Zone(ZoneFound)%MaximumX .and.          &
                  (ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,1) - Zone(ZoneFound)%MaximumX) > .001) .or.  &
                  (ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,2) < Zone(ZoneFound)%MinimumY .and.          &
                  (Zone(ZoneFound)%MinimumY - ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,2)) > .001) .or.  &
                  (ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,2) > Zone(ZoneFound)%MaximumY .and.          &
                  (ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,2) - Zone(ZoneFound)%MaximumY) > .001) .or.  &
                  (ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,3) < Zone(ZoneFound)%MinimumZ .and.          &
                  (Zone(ZoneFound)%MinimumZ - ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,3)) > .001) .or.  &
                  (ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,3) > Zone(ZoneFound)%MaximumZ .and.          &
                  (ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,3) - Zone(ZoneFound)%MaximumZ) > .001)) THEN
                ZoneDaylight(ZoneFound)%MapRefPtInBounds(RefPt)=.false.
              ENDIF
              ! Test extremes of Map Points against Zone Min/Max
              IF (RefPt == 1 .or. RefPt == ZoneDaylight(ZoneFound)%TotalMapRefPoints) THEN
                IF ((ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,1) < Zone(ZoneFound)%MinimumX  .or.  &
                     ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,1) > Zone(ZoneFound)%MaximumX) .and. &
                     .not. ZoneDaylight(ZoneFound)%MapRefPtInBounds(RefPt)) THEN
                  CALL ShowWarningError('GetDetailedDaylighting: Reference Map point #['//  &
                     TRIM(RoundSigDigits(RefPt))//'], X Value outside Zone Min/Max X, Zone='//  &
                       TRIM(Zone(ZoneFound)%Name))
                  CALL ShowContinueError('...X Reference Point= '//                                                        &
                         TRIM(RoundSigDigits(ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,1),2))//', Zone Minimum X= '//  &
                         TRIM(RoundSigDigits(Zone(ZoneFound)%MinimumX,2))//', Zone Maximum X= '//                             &
                         TRIM(RoundSigDigits(Zone(ZoneFound)%MaximumX,2)))
                  IF (ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,1) < Zone(ZoneFound)%MinimumX) THEN
                    CALL ShowContinueError('...X Reference Distance Outside MinimumX= '//  &
                       TRIM(RoundSigDigits(Zone(ZoneFound)%MinimumX-ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,1),4))//' m.')
                  ELSE
                    CALL ShowContinueError('...X Reference Distance Outside MaximumX= '//  &
                       TRIM(RoundSigDigits(ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,1)-Zone(ZoneFound)%MaximumX,4))//' m.')
                  ENDIF
                ENDIF
                IF ((ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,2) < Zone(ZoneFound)%MinimumY  .or.  &
                     ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,2) > Zone(ZoneFound)%MaximumY) .and. &
                     .not. ZoneDaylight(ZoneFound)%MapRefPtInBounds(RefPt)) THEN
                  CALL ShowWarningError('GetDetailedDaylighting: Reference Map point #['//  &
                     TRIM(RoundSigDigits(RefPt))//'], Y Value outside Zone Min/Max Y, Zone='//  &
                       TRIM(Zone(ZoneFound)%Name))
                  CALL ShowContinueError('...Y Reference Point= '//                                                        &
                         TRIM(RoundSigDigits(ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,2),2))//', Zone Minimum Y= '//  &
                         TRIM(RoundSigDigits(Zone(ZoneFound)%MinimumY,2))//', Zone Maximum Y= '//                             &
                         TRIM(RoundSigDigits(Zone(ZoneFound)%MaximumY,2)))
                  IF (ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,2) < Zone(ZoneFound)%MinimumY) THEN
                    CALL ShowContinueError('...Y Reference Distance Outside MinimumY= '//  &
                       TRIM(RoundSigDigits(Zone(ZoneFound)%MinimumY-ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,2),4))//' m.')
                  ELSE
                    CALL ShowContinueError('...Y Reference Distance Outside MaximumY= '//  &
                       TRIM(RoundSigDigits(ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,2)-Zone(ZoneFound)%MaximumY,4))//' m.')
                  ENDIF
                ENDIF
                IF ((ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,3) < Zone(ZoneFound)%MinimumZ  .or.  &
                     ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,3) > Zone(ZoneFound)%MaximumZ) .and. &
                     .not. ZoneDaylight(ZoneFound)%MapRefPtInBounds(RefPt)) THEN
                  CALL ShowWarningError('GetDetailedDaylighting: Reference Map point #['//  &
                     TRIM(RoundSigDigits(RefPt))//'], Z Value outside Zone Min/Max Z, Zone='//  &
                       TRIM(Zone(ZoneFound)%Name))
                  CALL ShowContinueError('...Z Reference Point= '//                                                        &
                         TRIM(RoundSigDigits(ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,3),2))//', Zone Minimum Z= '//  &
                         TRIM(RoundSigDigits(Zone(ZoneFound)%MinimumZ,2))//', Zone Maximum Z= '//                             &
                         TRIM(RoundSigDigits(Zone(ZoneFound)%MaximumZ,2)))
                  IF (ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,3) < Zone(ZoneFound)%MinimumZ) THEN
                    CALL ShowContinueError('...Z Reference Distance Outside MinimumZ= '//  &
                       TRIM(RoundSigDigits(Zone(ZoneFound)%MinimumZ-ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,3),4))//' m.')
                  ELSE
                    CALL ShowContinueError('...Z Reference Distance Outside MaximumZ= '//  &
                       TRIM(RoundSigDigits(ZoneDaylight(ZoneFound)%MapRefPtAbsCoord(RefPt,3)-Zone(ZoneFound)%MaximumZ,4))//' m.')
                  ENDIF
                ENDIF
              ENDIF
              RefPt = RefPt + 1
            END DO ! X
          END DO ! Y

        END IF
      END IF
    END DO ! MapNum

  END DO


  DO MapNum = 1, TotIllumMaps
    IF (IllumMap(MapNum)%Zone == 0) CYCLE
    IF (ZoneDaylight(IllumMap(MapNum)%Zone)%DaylightType /= DetailedDaylighting) THEN
      CALL ShowSevereError('Zone Name in Output:IlluminanceMap is not used for Daylighting:Controls='//  &
                         TRIM(Zone(IllumMap(MapNum)%Zone)%Name))
      ErrorsFound = .TRUE.
    ENDIF
  ENDDO

  IF (TotIllumMaps > 0) THEN
    Write(OutputFileInits,'(A)')   &
     '! <Daylighting:Illuminance Maps:Detail>,Name,Zone,XMin {m},XMax {m},Xinc {m},#X Points,'//  &
        'YMin {m},YMax {m},Yinc {m},#Y Points,Z {m}'
  ENDIF
  DO MapNum = 1, TotIllumMaps
    WRITE(OutputFileInits,"('Daylighting:Illuminance Maps:Detail',11(',',A))") TRIM(IllumMap(MapNum)%Name),   &
      TRIM(Zone(IllumMap(MapNum)%Zone)%Name),  &
      TRIM(RoundSigDigits(IllumMap(MapNum)%XMin,2)),TRIM(RoundSigDigits(IllumMap(MapNum)%XMax,2)),  &
         TRIM(RoundSigDigits(IllumMap(MapNum)%Xinc,2)),TRIM(RoundSigDigits(IllumMap(MapNum)%XNum)), &
      TRIM(RoundSigDigits(IllumMap(MapNum)%YMin,2)),TRIM(RoundSigDigits(IllumMap(MapNum)%YMax,2)),  &
         TRIM(RoundSigDigits(IllumMap(MapNum)%Yinc,2)),TRIM(RoundSigDigits(IllumMap(MapNum)%YNum)), &
      TRIM(RoundSigDigits(IllumMap(MapNum)%Z,2))
  ENDDO

  IF (ErrorsFound) RETURN

  DO ZoneNum = 1, NumOfZones

    IF (ZoneDaylight(ZoneNum)%TotalDaylRefPoints == 0) CYCLE

    IF (ZoneDaylight(ZoneNum)%TotalDaylRefPoints > 0) THEN
      CALL SetupOutputVariable('Daylight Illum at Ref Point 1 [lux]', &
        ZoneDaylight(ZoneNum)%DaylIllumAtRefPt(1), 'Zone', 'Average', &
        Zone(ZoneNum)%Name)
      CALL SetupOutputVariable('Glare Index at Ref Point 1 []', &
        ZoneDaylight(ZoneNum)%GlareIndexAtRefPt(1), 'Zone', 'Average', &
        Zone(ZoneNum)%Name)

      !added TH 12/2/2008 to calculate the time exceeding the glare index setpoint
      CALL SetupOutputVariable('Time Exceeding Glare Index Setpoint at Ref Point 1 [hr]', &
        ZoneDaylight(ZoneNum)%TimeExceedingGlareIndexSPAtRefPt(1), 'Zone', 'Sum', Zone(ZoneNum)%Name)

      !added TH 7/6/2009 to calculate the time exceeding the illuminance setpoint
      CALL SetupOutputVariable('Time Exceeding Daylight Illuminance Setpoint at Ref Point 1 [hr]', &
        ZoneDaylight(ZoneNum)%TimeExceedingDaylightIlluminanceSPAtRefPt(1), 'Zone', 'Sum', Zone(ZoneNum)%Name)
    ENDIF

    IF (ZoneDaylight(ZoneNum)%TotalDaylRefPoints > 1) THEN
      CALL SetupOutputVariable('Daylight Illum at Ref Point 2 [lux]', &
        ZoneDaylight(ZoneNum)%DaylIllumAtRefPt(2), 'Zone', 'Average', &
        Zone(ZoneNum)%Name)
      CALL SetupOutputVariable('Glare Index at Ref Point 2 []', &
        ZoneDaylight(ZoneNum)%GlareIndexAtRefPt(2), 'Zone', 'Average', &
        Zone(ZoneNum)%Name)

      !added TH 12/2/2008 to calculate the time exceeding the glare index setpoint
      CALL SetupOutputVariable('Time Exceeding Glare Index Setpoint at Ref Point 2 [hr]', &
        ZoneDaylight(ZoneNum)%TimeExceedingGlareIndexSPAtRefPt(2), 'Zone', 'Sum', Zone(ZoneNum)%Name)

      !added TH 7/6/2009 to calculate the time exceeding the illuminance setpoint
      CALL SetupOutputVariable('Time Exceeding Daylight Illuminance Setpoint at Ref Point 2 [hr]', &
        ZoneDaylight(ZoneNum)%TimeExceedingDaylightIlluminanceSPAtRefPt(2), 'Zone', 'Sum', Zone(ZoneNum)%Name)
    END IF
    CALL SetupOutputVariable('Ltg Power Multiplier from Daylighting []', &
      ZoneDaylight(ZoneNum)%ZonePowerReductionFactor, 'Zone', 'Average', &
      Zone(ZoneNum)%Name)
  END DO

  DO SurfLoop = 1,TotSurfaces
    IF(Surface(SurfLoop)%Class == SurfaceClass_Window .AND. Surface(SurfLoop)%ExtSolar) THEN
      IF(ZoneDaylight(Surface(SurfLoop)%Zone)%TotalDaylRefPoints > 0 .AND. .NOT. Zone(Surface(SurfLoop)%Zone)%IntZWindow) THEN
        CALL SetupOutputVariable('Daylight Illum at Ref Point 1 from Window[lux]', &
                                 SurfaceWindow(SurfLoop)%IllumFromWinAtRefPt1Rep, &
                                'Zone', 'Average',Surface(SurfLoop)%Name)
        CALL SetupOutputVariable('Daylight Luminance of Window As Viewed From Ref Point 1[cd/m2]', &
                                 SurfaceWindow(SurfLoop)%LumWinFromRefPt1Rep, &
                                'Zone', 'Average',Surface(SurfLoop)%Name)
        IF (ZoneDaylight(Surface(SurfLoop)%Zone)%TotalDaylRefPoints > 1) THEN
          CALL SetupOutputVariable('Daylight Illum at Ref Point 2 from Window[lux]', &
                                  SurfaceWindow(SurfLoop)%IllumFromWinAtRefPt2Rep, &
                                  'Zone', 'Average',Surface(SurfLoop)%Name)
          CALL SetupOutputVariable('Daylight Luminance of Window As Viewed From Ref Point 2[cd/m2]', &
                                   SurfaceWindow(SurfLoop)%LumWinFromRefPt2Rep, &
                                  'Zone', 'Average',Surface(SurfLoop)%Name)
        END IF
      END IF
    END IF
  END DO

  RETURN

END SUBROUTINE GetDaylightingParametersDetaild

Subroutine CheckTDDsAndLightShelvesInDaylitZones
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   Dec 2007
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine checks daylighting input for TDDs and light shelfs
          !  which need to be checked after daylighting input has been read in (CR 7145)
          !  (eventually this should be changed once/if implementations change to decouple from daylighting calcs so that
          !  these devices can be used in models without daylighting controls
          !
          ! CR 7145 was for TDDs, but also implenting check for light shelves, the other "daylighting device"

          ! METHODOLOGY EMPLOYED:
          ! loop thru daylighting devices and check that their zones have daylight controls

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  Use DataHeatBalance, ONLY: Zone
  USE DataDaylighting, ONLY: ZoneDaylight, NoDaylighting
  USE DataDaylightingDevices
  USE General, ONLY: RoundSigDigits

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
  INTEGER       :: PipeNum     ! TDD pipe object number
  INTEGER       :: ShelfNum    ! light shelf object number
  INTEGER       :: SurfNum     ! daylight device surface number
  LOGICAL       :: ErrorsFound
  LOGICAL,SAVE  :: FirstTime=.true.

  IF (FirstTime) THEN
    ALLOCATE(CheckTDDZone(NumOfZones))
    CheckTDDZone=.true.
    FirstTime=.false.
  ENDIF

  ErrorsFound = .FALSE.

  DO PipeNum = 1, NumOfTDDPipes
    SurfNum = TDDPipe(PipeNum)%Diffuser
    IF (SurfNum > 0) THEN
      IF (ZoneDaylight(Surface(SurfNum)%zone)%DaylightType == NoDaylighting) THEN
        CALL ShowSevereError('DaylightingDevice:Tubular = '//TRIM(TDDPipe(PipeNum)%Name)// &
            ':  is not connected to a Zone that has Daylighting.  ')
        CALL ShowContinueError('Add Daylighting:Controls (or Daylighting:DELight:Controls) ' //&
            'to Zone named:  '//TRIM(Zone(Surface(SurfNum)%zone)%name) )
        CALL ShowContinueError('A sufficient control is provided on the .dbg file.')
        ErrorsFound = .TRUE.
        IF (CheckTDDZone(Surface(SurfNum)%zone)) THEN
          WRITE(OutputFileDebug,fmta) ' ! Following control is to allow tubular reporting in this Zone'
          WRITE(OutputFileDebug,fmta) 'Daylighting:Controls,  !- this control controls 0% of zone.'
          WRITE(OutputFileDebug,fmta) '   '//trim(Zone(Surface(SurfNum)%zone)%name)//',  !- Zone Name'
          WRITE(OutputFileDebug,fmta) '     1,   !- Total Daylighting Reference Points'
          IF (DaylRefWorldCoordSystem) THEN
            ! world coordinates, use zone origin for ref pt
            WRITE(OutputFileDebug,fmta) '   '//trim(RoundSigDigits(Zone(Surface(SurfNum)%zone)%OriginX,2))//  &
                                    ',   !- X-Coordinate of First Reference Point {m}'
            WRITE(OutputFileDebug,fmta) '   '//trim(RoundSigDigits(Zone(Surface(SurfNum)%zone)%OriginY,2))//  &
                                    ',   !- Y-Coordinate of First Reference Point {m}'
            WRITE(OutputFileDebug,fmta) '   '//trim(RoundSigDigits(Zone(Surface(SurfNum)%zone)%OriginZ,2))//  &
                                    ',   !- Z-Coordinate of First Reference Point {m}'
          ELSE
            ! relative coordinates, use 0,0,0 for ref pt
            WRITE(OutputFileDebug,fmta) '   0.0,   !- X-Coordinate of First Reference Point {m}'
            WRITE(OutputFileDebug,fmta) '   0.0,   !- Y-Coordinate of First Reference Point {m}'
            WRITE(OutputFileDebug,fmta) '   0.0,   !- Z-Coordinate of First Reference Point {m}'
          ENDIF
          WRITE(OutputFileDebug,fmta) '      ,   !- X-Coordinate of Second Reference Point'
          WRITE(OutputFileDebug,fmta) '      ,   !- Y-Coordinate of Second Reference Point'
          WRITE(OutputFileDebug,fmta) '      ,   !- Z-Coordinate of Second Reference Point'
          WRITE(OutputFileDebug,fmta) '   0.0,   !- Fraction of Zone Controlled by First Reference Point'
          WRITE(OutputFileDebug,fmta) '   0.0,   !- Fraction of Zone Controlled by Second Reference Point'
          WRITE(OutputFileDebug,fmta) '   0.0,   !- Illuminance Setpoint at First Reference Point'
          WRITE(OutputFileDebug,fmta) '   0.0,   !- Illuminance Setpoint at Second Reference Point'
          WRITE(OutputFileDebug,fmta) '     3,   !- Lighting Control Type'
          WRITE(OutputFileDebug,fmta) '   0.0,   !- Glare Calculation Azimuth Angle of View Direction Clockwise from Zone y-Axis'
          WRITE(OutputFileDebug,fmta) '      ,   !- Maximum Allowable Discomfort Glare Index'
          WRITE(OutputFileDebug,fmta) '   0.0,   !- Minimum Input Power Fraction for Continuous Dimming Control'
          WRITE(OutputFileDebug,fmta) '   0.0,   !- Minimum Light Output Fraction for Continuous Dimming Control'
          WRITE(OutputFileDebug,fmta) '     0,   !- Number of Stepped Control Steps'
          WRITE(OutputFileDebug,fmta) '   0.0;   !- Probability Lighting will be Reset When Needed in Manual Stepped Control'

          CheckTDDZone(Surface(SurfNum)%zone)=.false.
        ENDIF
      ENDIF

    ELSE ! surfNum == 0
      ! should not come here (would have already been caught in TDD get input), but is an error
      CALL ShowSevereError('DaylightingDevice:Tubular = '//TRIM(TDDPipe(PipeNum)%Name)// &
            ':  Diffuser surface not found ')
      ErrorsFound = .TRUE.
    ENDIF
  END DO ! PipeNum

  DO ShelfNum = 1, NumOfShelf
    SurfNum = Shelf(ShelfNum)%Window
!    IF (SurfNum > 0) THEN
!      IF (ZoneDaylight(Surface(SurfNum)%zone)%DaylightType == NoDaylighting) THEN
!        CALL ShowSevereError('DaylightingDevice:Shelf = '//TRIM(Shelf(ShelfNum)%Name)// &
!            ':  is not connected to a Zone that has Daylighting.  ')
!        CALL ShowContinueError('Add Daylighting:Controls (or Daylighting:DELight:Controls) ' //&
!            'to Zone named:  '//TRIM(Zone(Surface(SurfNum)%zone)%name) )
!          ErrorsFound = .TRUE.
!      ENDIF
!
!    ELSE ! surfNum == 0
    IF (SurfNum == 0) THEN
      ! should not come here (would have already been caught in shelf get input), but is an error
      CALL ShowSevereError('DaylightingDevice:Shelf = '//TRIM(Shelf(ShelfNum)%Name)// &
            ':  window not found ')
      ErrorsFound = .TRUE.
    ENDIF
  END DO ! ShelfNum

 IF (ErrorsFound) CALL ShowFatalError('CheckTDDsAndLightShelvesInDaylitZones: Errors in DAYLIGHTING input.')

END SUBROUTINE CheckTDDsAndLightShelvesInDaylitZones

SUBROUTINE GetLightWellData(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   Apr 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Gets data for a light well associated with a rectangular exterior window.
          ! Calculates light well efficiency, defined as the ratio of the amount of visible
          ! solar radiation leaving a well to the amount entering the well.

          ! METHODOLOGY EMPLOYED:
          ! Based on fit to Fig. 8-21, "Efficiency factors for various depths of light wells
          ! based on well-interreflectance values," Lighting Handbook, 8th Edition, Illuminating
          ! Engineering Society of North America, 1993.

          ! REFERENCES: see above.

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound ! If errors found in input

          ! SUBROUTINE PARAMETER DEFINITIONS:na
          ! INTERFACE BLOCK SPECIFICATIONS:na
          ! DERIVED TYPE DEFINITIONS:na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER :: IOStat                   ! IO Status when calling get input subroutine
  INTEGER :: NumAlpha                 ! Number of alpha names being passed
  INTEGER :: NumProp                  ! Number of properties being passed
  INTEGER :: TotLightWells            ! Total Light Well objects
  INTEGER :: Loop                     ! DO loop index
  INTEGER :: SurfNum                  ! Surface number
  LOGICAL :: WrongSurfaceType         ! True if associated surface is not an exterior window
  REAL(r64) :: HeightWell                  ! Well height (from window to bottom of well) (m)
  REAL(r64) :: PerimWell                   ! Well perimeter (at bottom of well) (m)
  REAL(r64) :: AreaWell                    ! Well area (at bottom of well) (m2)
  REAL(r64) :: VisReflWell                 ! Area-weighted visible reflectance of well walls
  REAL(r64) :: WellCavRatio                ! Well cavity ratio

  ! Get the total number of Light Well objects
  cCurrentModuleObject='DaylightingDevice:LightWell'
  TotLightWells = GetNumObjectsFound(TRIM(cCurrentModuleObject))
  IF(TotLightWells == 0) RETURN

  DO Loop = 1, TotLightWells

    CALL GetObjectItem(TRIM(cCurrentModuleObject),Loop,cAlphaArgs,NumAlpha,rNumericArgs,NumProp,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    SurfNum = FindItemInList(cAlphaArgs(1),Surface%Name,TotSurfaces)
    IF(SurfNum == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(1))//  &
          '="'//TRIM(cAlphaArgs(1))//'" not found.')
    END IF

    ! Check that associated surface is an exterior window
    WrongSurfaceType = .FALSE.
    IF(SurfNum /= 0) THEN
      IF(Surface(SurfNum)%Class /= SurfaceClass_Window.AND. Surface(SurfNum)%ExtBoundCond /= ExternalEnvironment) &
         WrongSurfaceType = .TRUE.
      IF(WrongSurfaceType) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(1))//  &
          '="'//TRIM(cAlphaArgs(1))//'" - not an exterior window.')
        ErrorsFound = .TRUE.
      END IF
    END IF

    IF(.NOT.ErrorsFound) THEN

      ! Associated surface is an exterior window; calculate light well efficiency.

      SurfaceWindow(SurfNum)%LightWellEff = 1.0
      HeightWell = rNumericArgs(1)
      PerimWell  = rNumericArgs(2)
      AreaWell   = rNumericArgs(3)
      VisReflWell = rNumericArgs(4)

      ! Warning if light well area is less than window area
      IF(AreaWell < (Surface(SurfNum)%Area+SurfaceWindow(SurfNum)%DividerArea-0.1d0)) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': invalid '//TRIM(cAlphaFieldNames(1))//  &
            '="'//TRIM(cAlphaArgs(1))//'" - Areas.')
        CALL ShowContinueError('has Area of Bottom of Well='//TRIM(RoundSigDigits(Surface(SurfNum)%Area,1))//  &
           ' that is less than window area='//TRIM(RoundSigDigits(AreaWell,1)))
      END IF

      IF(HeightWell >= 0.0 .AND. PerimWell > 0.0 .AND. AreaWell > 0.0) THEN
        WellCavRatio = 2.5d0*HeightWell*PerimWell/AreaWell
        SurfaceWindow(SurfNum)%LightWellEff = EXP(-WellCavRatio*(0.16368d0-0.14467d0*VisReflWell))
      END IF

    END IF

  END DO  ! End of loop over light well objects

  RETURN

END SUBROUTINE GetLightWellData

SUBROUTINE DayltgGlare(IL, BLUM, GLINDX, ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   July 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! CALCULATE GLARE INDEX.

          ! METHODOLOGY EMPLOYED:
          ! Called from DayltgInteriorIllum.  Finds glare index at reference
          ! point no. IL in a space using the Cornell/BRS large source
          ! glare formula. BLUM is the background luminance (cd/m**2).
          !
          ! TH comment 1/21/2010: The SurfaceWindow(IWin)%ShadingFlag has to be set
          !  before calling this subroutine. For switchable glazings this is tricky
          !  because the ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,2,loop)
          !  may change every time step to represent intermediate switched state.

          ! REFERENCES:
          ! Based on DOE-2.1E subroutine DGLARE.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER           :: IL                           ! Reference point index: 1=first ref pt, 2=second ref pt
  REAL(r64)         :: BLUM                         ! Window background (surround) luminance (cd/m2)
  REAL(r64)         :: GLINDX                       ! Glare index
  INTEGER           :: ZoneNum                      ! Zone number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)         :: GTOT                         ! Glare constant
  REAL(r64)         :: GTOT1                        ! Portion of glare constant
  REAL(r64)         :: GTOT2                        ! Portion of glare constant
  INTEGER           :: IWin                         ! Window counter
  INTEGER           :: IS                           ! Window shading index: 1=unshaded, 2=shaded
  INTEGER           :: loop                         ! Loop index

          ! FLOW:
  ! Initialize glare constant
  GTOT = 0.0d0

  ! Loop over exterior windows associated with zone
  DO loop = 1,ZoneDaylight(ZoneNum)%NumOfDayltgExtWins
    IWin = ZoneDaylight(ZoneNum)%DayltgExtWinSurfNums(loop)
    IS = 1
    IF((SurfaceWindow(IWin)%ShadingFlag >= 1 .AND. SurfaceWindow(IWin)%ShadingFlag <= 9) .OR. &
           SurfaceWindow(IWin)%SolarDiffusing) IS = 2
    ! Conversion from ft-L to cd/m2, with cd/m2 = 0.2936 ft-L, gives the 0.4794 factor
    ! below, which is (0.2936)**0.6
    GTOT1  = 0.4794d0*(ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,IS,loop)**1.6d0) *   &
                ZoneDaylight(ZoneNum)%SolidAngAtRefPtWtd(IL,loop)**0.8d0
    GTOT2  = BLUM + 0.07d0 * (ZoneDaylight(ZoneNum)%SolidAngAtRefPt(IL,loop)**0.5d0) *  &
                ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,IS,loop)
    GTOT = GTOT + GTOT1 / (GTOT2 + 0.000001d0)
  END DO

  ! Glare index (adding 0.000001 prevents LOG10 (0))
  GLINDX = 10.0d0*LOG10(GTOT+0.000001d0)
  ! Set glare index to zero for GTOT < 1
  GLINDX = MAX(0.0d0, GLINDX)

  RETURN

END SUBROUTINE DayltgGlare

SUBROUTINE DayltgGlareWithIntWins(GLINDX,ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   March 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate daylighting glare index for zones with interior windows.

          ! METHODOLOGY EMPLOYED:
          ! Finds glare index at reference point IL in a daylit zone using the Cornell/BRS large source
          ! glare formula. Takes into account inter-reflected illuminance from light entering
          ! the zone through interior windows

          ! REFERENCES:
          ! Based on subroutine DayltgGlare.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS
  INTEGER, INTENT(IN)  :: ZoneNum                      ! Zone number
  REAL(r64), INTENT(OUT)    :: GLINDX(2)                    ! Glare index

          ! SUBROUTINE PARAMETER DEFINITIONS: na
          ! INTERFACE BLOCK SPECIFICATIONS: na
          ! DERIVED TYPE DEFINITIONS: na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: IL                           ! Reference point index: 1=first ref pt, 2=second ref pt
  REAL(r64) :: GTOT                         ! Glare constant
  REAL(r64) :: GTOT1                        ! Portion of glare constant
  REAL(r64) :: GTOT2                        ! Portion of glare constant
  INTEGER   :: IWin                         ! Window counter
  INTEGER   :: IS                           ! Window shading index: 1=unshaded, 2=shaded
  REAL(r64) :: BacLum                       ! Background luminance (cd/m2)
  INTEGER   :: loop                         ! Loop index
  INTEGER   :: RefPoints                    ! Number of daylighting reference points in zone
          ! FLOW:
  ! Initialize glare constant
  GTOT = 0.0d0

  ! Calculate background luminance including effect of inter-reflected illuminance from light
  ! entering zone through its interior windows

  RefPoints = MIN(2,ZoneDaylight(ZoneNum)%TotalDaylRefPoints)
  DO IL = 1,RefPoints
    BacLum = ZoneDaylight(ZoneNum)%BacLum(IL) +  &
               ZoneDaylight(ZoneNum)%InterReflIllFrIntWins * ZoneDaylight(ZoneNum)%AveVisDiffReflect / Pi
    BacLum = MAX(ZoneDaylight(ZoneNum)%IllumSetPoint(IL)*ZoneDaylight(ZoneNum)%AveVisDiffReflect/Pi,BacLum)

    ! Loop over exterior windows associated with zone
    DO loop = 1,ZoneDaylight(ZoneNum)%NumOfDayltgExtWins
      IWin = ZoneDaylight(ZoneNum)%DayltgExtWinSurfNums(loop)
      IS = 1
      IF((SurfaceWindow(IWin)%ShadingFlag >= 1 .AND. SurfaceWindow(IWin)%ShadingFlag <= 9) .OR. &
            SurfaceWindow(IWin)%SolarDiffusing) IS = 2
      ! Conversion from ft-L to cd/m2, with cd/m2 = 0.2936 ft-L, gives the 0.4794 factor
      ! below, which is (0.2936)**0.6
      GTOT1  = 0.4794d0*(ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,IS,loop)**1.6d0) *   &
                  ZoneDaylight(ZoneNum)%SolidAngAtRefPtWtd(IL,loop)**0.8d0
      GTOT2  = BacLum + 0.07d0 * (ZoneDaylight(ZoneNum)%SolidAngAtRefPt(IL,loop)**0.5d0) *  &
                  ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,IS,loop)
      GTOT = GTOT + GTOT1 / (GTOT2 + 0.000001d0)
    END DO

    ! Glare index
    GLINDX(IL) = 10.0d0*LOG10(GTOT+0.000001d0)
    ! Set glare index to zero for GTOT < 1
    GLINDX(IL) = MAX(0.0d0, GLINDX(IL))
  END DO

  RETURN
END SUBROUTINE DayltgGlareWithIntWins


SUBROUTINE DayltgExtHorizIllum(HISK,HISU)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   July 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates exterior daylight illuminance.

          ! METHODOLOGY EMPLOYED:
          ! Called by CalcDayltgCoefficients. Calculates illuminance
          ! on unobstructed horizontal surface by integrating
          ! over the luminance distribution of standard CIE skies.
          ! Calculates horizontal beam illuminance.
          !
          ! REFERENCES:
          ! Based on DOE-2.1E subroutine DHILL.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64)         :: HISK(4)                 ! Horizontal illuminance from sky for different sky types
                                               !  and overcast sky (lux)
  REAL(r64)         :: HISU                    ! Horizontal illuminance from sun for unit beam normal
                                               !   illuminance (lux)

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: NTH = 18                  ! Number of azimuth steps for sky integration
  INTEGER, PARAMETER :: NPH = 8                   ! Number of altitude steps for sky integration
  REAL(r64), PARAMETER    :: DTH = 2. * PI / NTH       ! Sky integration azimuth stepsize (radians)
  REAL(r64), PARAMETER    :: DPH = PIOVR2 / NPH        ! Sky integration altitude stepsize (radians)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                  :: IPH       ! Altitude index for sky integration
  INTEGER                  :: ITH       ! Azimuth index for sky integration
  REAL(r64),SAVE,DIMENSION(NPH) :: PH        ! Altitude of sky element (radians)
  REAL(r64),SAVE,DIMENSION(NTH) :: TH        ! Azimuth of sky element (radians)
  INTEGER                  :: ISky      ! Sky type index
  REAL(r64),SAVE,DIMENSION(NPH) :: SPHCPH    ! Sine times cosine of altitude of sky element
  LOGICAL, SAVE            :: FirstTime=.true.  ! flag for first time thru to initialize

          ! FLOW:
  ! Integrate to obtain illuminance from sky.
  ! The contribution in lumens/m2 from a patch of sky at altitude PH and azimuth TH
  ! is L(TH,PH)*SIN(PH)*COS(PH)*DTH*DPH, where L(TH,PH) is the luminance
  ! of the patch in cd/m2.
  !  Init
  IF (FirstTime) THEN
    DO IPH = 1,NPH
      PH(IPH) = (IPH - 0.5d0) * DPH
      SPHCPH(IPH) = SIN(PH(IPH)) * COS(PH(IPH)) ! DA = COS(PH)*DTH*DPH
    END DO
    DO ITH = 1,NTH
      TH(ITH) = (ITH - 0.5d0) * DTH
    END DO
    FirstTime=.false.
  ENDIF

  HISK = 0.

  ! Sky integration
  DO IPH = 1,NPH
    DO ITH = 1,NTH
      DO ISky = 1,4
        HISK(ISky) = HISK(ISky) + DayltgSkyLuminance(ISky,TH(ITH),PH(IPH)) * SPHCPH(IPH)
      END DO
    END DO
  END DO

  DO ISky = 1,4
    HISK(ISky) = HISK(ISky) * DTH * DPH
  END DO

  ! Direct solar horizontal illum (for unit direct normal illuminance)
  HISU  = SPHSUN * 1.0

  RETURN

END SUBROUTINE DayltgExtHorizIllum

SUBROUTINE DayltgCrossProduct(A, B, C)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   unknown
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates Cross product between vectors A and B.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64) :: A(3),B(3),C(3)          ! Vector components: C = A X B

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
          ! FLOW:
  C(1) = A(2) * B(3) - A(3) * B(2)
  C(2) = A(3) * B(1) - A(1) * B(3)
  C(3) = A(1) * B(2) - A(2) * B(1)

  RETURN

END SUBROUTINE DayltgCrossProduct

SUBROUTINE DayltgPierceSurface(ISurf, R1, RN, IPIERC, CP)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   July 1997
          !       MODIFIED       Sept 2003, FCW: change shape test for rectangular surface to exclude
          !                       triangular windows (Surface%Shape=8)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Called to determine if (1) solar disk is visible from reference point,
          ! (2) if a ray intersects an obstruction, or (3) if ray passes through an interior window.
          ! Returns 0 if line through point R1 in direction of unit vector
          ! RN does not intersect surface ISurf.
          !
          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! Based on DOE-2.1E subroutine DPIERC.

          ! USE STATEMENTS:na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: ISurf      ! Surface index
  REAL(r64), INTENT(IN)             :: R1(3)      ! Point from which ray originates
  REAL(r64), INTENT(IN) :: RN(3)      ! Unit vector along in direction of ray whose
                                             !  intersection with surface is to be determined
  INTEGER, INTENT(OUT)         :: IPIERC     ! =1 if line through point R1 in direction of unit vector
                                             !  RN intersects surface ISurf; =0 otherwise.
  REAL(r64), INTENT(INOUT)          :: CP(3)      ! Point that ray along RN intersects plane of surface

          ! SUBROUTINE PARAMETER DEFINITIONS:na
          ! INTERFACE BLOCK SPECIFICATIONS:na
          ! DERIVED TYPE DEFINITIONS:na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: V1(3)                    ! First vertex
  REAL(r64) :: V2(3)                    ! Second vertex
  REAL(r64) :: V3(3)                    ! Third vertex
  INTEGER   :: NV                       ! Number of vertices (3 or 4)
  REAL(r64) :: A1(3)                    ! Vector from vertex 1 to 2
  REAL(r64) :: A2(3)                    ! Vector from vertex 2 to 3
  REAL(r64) :: AXC(3)                   ! Cross product of A and C
  REAL(r64) :: SN(3)                    ! Vector normal to surface (SN = A1 X A2)
  REAL(r64) :: AA(3)                    ! AA(I) = A(N,I)
  REAL(r64) :: CC(3)                    ! CC(I) = C(N,I)
  REAL(r64) :: CCC(3)                   ! Vector from vertex 2 to CP
  REAL(r64) :: AAA(3)                   ! Vector from vertex 2 to vertex 1
  REAL(r64) :: BBB(3)                   ! Vector from vertex 2 to vertex 3
  INTEGER   :: N                        ! Vertex loop index
  INTEGER   :: I                        ! Vertex-to-vertex index
  REAL(r64) :: F1,F2                    ! Intermediate variables
  REAL(r64) :: SCALE                    ! Scale factor
  REAL(r64) :: DOTCB                    ! Dot product of vectors CCC and BBB
  REAL(r64) :: DOTCA                    ! Dot product of vectors CCC and AAA
  REAL(r64) :: DOTAXCSN                 ! Dot product of vectors AXC and SN
  ! Following must be allocated to MaxVerticesPerSurface
!  REAL(r64)      :: A(4,3)                   ! Vertex-to-vertex vectors; A(1,i) is from vertex 1 to 2, etc.
!  REAL(r64)      :: C(4,3)                   ! Vectors from vertices to intersection point
!  REAL(r64)      :: V(4,3)                   ! Vertices of surfaces
  REAL(r64),SAVE,ALLOCATABLE,DIMENSION(:,:) :: A   ! Vertex-to-vertex vectors; A(1,i) is from vertex 1 to 2, etc.
  REAL(r64),SAVE,ALLOCATABLE,DIMENSION(:,:) :: C   ! Vectors from vertices to intersection point
  REAL(r64),SAVE,ALLOCATABLE,DIMENSION(:,:) :: V   ! Vertices of surfaces
  LOGICAL, SAVE :: FirstTimeFlag=.true.

          ! FLOW:
  IF (FirstTimeFlag) THEN
    ALLOCATE(A(MaxVerticesPerSurface,3))
    ALLOCATE(C(MaxVerticesPerSurface,3))
    ALLOCATE(V(MaxVerticesPerSurface,3))
    FirstTimeFlag=.false.
  ENDIF
  IPIERC = 0
  ! Vertex vectors
  NV = Surface(ISurf)%Sides
  DO N = 1,NV
    V(N,1) = Surface(ISurf)%Vertex(N)%X
    V(N,2) = Surface(ISurf)%Vertex(N)%Y
    V(N,3) = Surface(ISurf)%Vertex(N)%Z
  END DO

  ! Vertex-to-vertex vectors. A(1,2) is from vertex 1 to 2, etc.
  DO I = 1,3
    DO N = 1,NV-1
      A(N,I) = V(N+1,I) - V(N,I)
    END DO
    A(NV,I) = V(1,I) - V(NV,I)
    A1(I) = A(1,I)
    A2(I) = A(2,I)
    V1(I) = V(1,I)
    V2(I) = V(2,I)
    V3(I) = V(3,I)
  END DO

  ! Vector normal to surface
  CALL DayltgCrossProduct(A1, A2, SN)
  ! Scale factor, the solution of SN.(CP-V2) = 0 and
  ! CP = R1 + SCALE*RN, where CP is the point that RN,
  ! when extended, intersects the plane of the surface.
  F1 = DOT_PRODUCT(SN, V2 - R1)
  F2 = DOT_PRODUCT(SN, RN)
  ! Skip surfaces that are parallel to RN
  IF (ABS(F2) < 0.01d0) RETURN
  SCALE = F1 / F2
  ! Skip surfaces that RN points away from
  IF (SCALE <= 0.0) RETURN
  ! Point that RN intersects plane of surface
  CP = R1 + RN * SCALE
  ! Vector from vertex 2 to CP
  CCC = CP - V2
  ! Two cases: rectangle and non-rectangle; do rectangle
  ! first since most common shape and faster calculation
  IF (Surface(ISurf)%Shape == Rectangle .or. Surface(ISurf)%Shape == RectangularDoorWindow .or.  &
      Surface(ISurf)%Shape == RectangularOverhang .or. Surface(ISurf)%Shape == RectangularLeftFin .or. &
      Surface(ISurf)%Shape == RectangularRightFin) THEN
    !
    ! Surface is rectangular
    !
    ! Vectors from vertex 2 to vertex 1 and vertex 2 to vertex 3
    AAA = V1 - V2
    BBB = V3 - V2
    ! Intersection point, CCC, is inside rectangle if
    ! 0 < CCC.BBB < BBB.BBB AND 0 < CCC.AAA < AAA.AAA
    DOTCB = DOT_PRODUCT(CCC, BBB)
    IF (DOTCB < 0.) RETURN
    IF (DOTCB > DOT_PRODUCT(BBB,BBB)) RETURN
    DOTCA = DOT_PRODUCT(CCC, AAA)
    IF (DOTCA < 0.) RETURN
    IF (DOTCA > DOT_PRODUCT(AAA,AAA)) RETURN
    ! Surface is intersected
    IPIERC = 1
  ELSE
    !
    ! Surface is not rectangular
    !
    ! Vectors from surface vertices to CP
    DO N = 1,NV
      DO I=1,3
        C(N,I) = CP(I) - V(N,I)
      END DO
    END DO
    ! Cross products of vertex-to-vertex vectors and
    ! vertex-to-CP vectors
    DO N = 1,NV
      DO I=1,3
        AA(I) = A(N,I)
        CC(I) = C(N,I)
      END DO
      CALL DayltgCrossProduct(AA,CC,AXC)
      DOTAXCSN = DOT_PRODUCT(AXC,SN)
       ! If at least one of these dot products is negative
       ! intersection point is outside of surface
      IF (DOTAXCSN < 0.0) RETURN
    END DO
    ! Surface is intersected
    IPIERC = 1
  END IF

  RETURN

END SUBROUTINE DayltgPierceSurface

SUBROUTINE DayltgHitObstruction(IHOUR,IWin,R1,RN,ObTrans)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   July 1997
          !       MODIFIED       FCW, May 2003: update list of surface classes that qualify as obstructions;
          !                        add interior surfaces as possible obstructors;
          !                        return from DO loop over surfaces as soon as any obstruction is hit;
          !                      FCW, July 2003: change from returning whether an obstruction is hit or not
          !                        to product of solar transmittances of hit obstructions.
          !                      FCW, Nov 2003: remove interior surfaces as possible obstructors since there
          !                        is now a separate check for interior obstructions; exclude windows and
          !                        doors as obstructors since if they are obstructors their base surfaces will
          !                        also be obstructors
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Determines the product of the solar transmittances of the obstructions hit by a ray
          ! from R1 in the direction of vector RN.

          ! METHODOLOGY EMPLOYED:na

          ! REFERENCES:
          ! Based on DOE-2.1E subroutine DHITSH.

          ! USE STATEMENTS:
  USE ScheduleManager, ONLY: LookUpScheduleValue

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: IHOUR             ! Hour number
  INTEGER, INTENT(IN)          :: IWin              ! Window index
  REAL(r64), INTENT(IN)             :: R1(3)             ! Origin of ray (m)
  REAL(r64), INTENT(IN) :: RN(3)             ! Unit vector along ray
  REAL(r64), INTENT(OUT)            :: ObTrans           ! Product of solar transmittances of exterior obstructions
                                                    !  (shading surfaces, building walls, etc.) that are hit

          ! SUBROUTINE PARAMETER DEFINITIONS:na
          ! INTERFACE BLOCK SPECIFICATIONS:na
          ! DERIVED TYPE DEFINITIONS:na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: ISurf                        ! Surface index
  INTEGER   :: IType                        ! Surface type/class
                                            !  mirror surfaces of shading surfaces
  REAL(r64) :: HP(3)                        ! Hit coordinates, if ray hits an obstruction
  INTEGER   :: Pierce                       ! 1 if a particular obstruction is hit, 0 otherwise
  REAL(r64) :: Trans                        ! Solar transmittance of a shading surface
          ! FLOW:

  ObTrans = 1.0

  ! Loop over obstructions, which can be building elements, like walls,
  ! or shadowing surfaces, like overhangs. Exclude base surface of window IWin.
  ! Building elements are assumed to be opaque. A shadowing surface is opaque unless
  ! its transmittance schedule value is non-zero.

  DO ISurf = 1,TotSurfaces
    IF(.NOT.Surface(ISurf)%ShadowSurfPossibleObstruction) CYCLE
    IType = Surface(ISurf)%Class
    IF ((IType==SurfaceClass_Wall .OR. IType==SurfaceClass_Roof .OR. IType==SurfaceClass_Floor) &
       .AND. ISurf /= Surface(IWin)%BaseSurf) THEN
      CALL DayltgPierceSurface(ISurf,R1,RN,Pierce,HP)
      IF(Pierce > 0) THEN  ! Building element is hit (assumed opaque)
        ObTrans = 0.0
        EXIT
      END IF
    ELSE IF (Surface(ISurf)%ShadowingSurf) THEN
      !!fw following check on mirror shadow surface can be removed with addition of above
      !!fw check on ShadowSurfPossibleObstruction (which is false for mirror shadow surfaces)
      IF(Surface(ISurf)%Name(1:4) /= 'Mir-') THEN  ! This check skips mirror surfaces
        CALL DayltgPierceSurface(ISurf,R1,RN,Pierce,HP)
        IF(Pierce > 0) THEN  ! Shading surface is hit
          ! Get solar transmittance of the shading surface
          Trans = 0.0
          IF(Surface(ISurf)%SchedShadowSurfIndex > 0) &
                 Trans = LookUpScheduleValue(Surface(ISurf)%SchedShadowSurfIndex,IHOUR,1)
          IF(Trans < 1.d-6) THEN
            ObTrans = 0.0
            EXIT
          ELSE
            ObTrans = Obtrans * Trans
          END IF
        END IF
      END IF
    END IF  ! End of test if building element or shading surface
  END DO

  RETURN

END SUBROUTINE DayltgHitObstruction


SUBROUTINE DayltgHitInteriorObstruction(IWin,R1,R2,IHIT)


          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   July 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine checks for interior obstructions between reference point and window element.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: IWin              ! Window index
  REAL(r64), INTENT(IN)             :: R1(3)             ! Origin of ray (m)
  REAL(r64), INTENT(IN)             :: R2(3)             ! Destination of ray (m)
  INTEGER, INTENT(OUT)         :: IHIT              ! Hit flag: 1 = ray hits an obstruction, 0 = does not

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: ISurf                        ! Surface index
  INTEGER   :: IType                        ! Surface type/class
  REAL(r64) :: HP(3)                        ! Hit coordinates, if ray hits an obstruction
  REAL(r64) :: r12                          ! Distance between R1 and R2
  REAL(r64) :: d                            ! Distance between R1 and pierced surface
  REAL(r64) :: RN(3)                        ! Unit vector along ray

          ! FLOW:
  IHIT = 0

  r12 = SQRT(DOT_PRODUCT(R1 - R2,R1 - R2))
  RN = R2 - R1
  RN = RN / (SQRT(DOT_PRODUCT(RN,RN))) ! Make unit vector

  ! Loop over obstructions, which can be building elements, like walls,
  ! or shadowing surfaces, like overhangs. Exclude base surface of window IWin.
  DO ISurf = 1,TotSurfaces
    IType = Surface(ISurf)%Class

    IF ((IType==SurfaceClass_Wall .OR. IType==SurfaceClass_Roof .OR. IType==SurfaceClass_Floor) &
       .AND. ISurf /= Surface(IWin)%BaseSurf .AND. ISurf /= Surface(Surface(IWin)%BaseSurf)%ExtBoundCond) THEN

      IF(Surface(ISurf)%Zone == Surface(IWin)%Zone) THEN  ! Wall/ceiling/floor is in same zone as window
        CALL DayltgPierceSurface(ISurf,R1,RN,IHIT,HP)
        IF (IHIT > 0) THEN
          d = SQRT(DOT_PRODUCT(R1 - HP,R1 - HP))
          IF (d > r12) THEN  ! Discount any hits farther than the window.
            IHIT = 0
          ELSE               ! The hit is closer than the window.
            EXIT
          END IF
        END IF
      END IF

    ELSE IF (Surface(ISurf)%ShadowingSurf) THEN

      CALL DayltgPierceSurface(ISurf,R1,RN,IHIT,HP)
      IF (IHIT > 0) THEN
        d = SQRT(DOT_PRODUCT(R1 - HP,R1 - HP))
        IF (d > r12) THEN  ! Discount any hits farther than the window.
          IHIT = 0
        ELSE               ! The hit is closer than the window.
          EXIT
        END IF
      END IF

    END IF
  END DO

  RETURN

END SUBROUTINE DayltgHitInteriorObstruction

SUBROUTINE DayltgHitBetWinObstruction(IWin1,IWin2,R1,R2,IHIT)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   Feb 2004
          !       MODIFIED na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Determines if a ray from point R1 on window IWin1 to point R2
          ! on window IWin2 hits an obstruction

          ! METHODOLOGY EMPLOYED:na
          ! REFERENCES:na
          ! USE STATEMENTS:na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)          :: IWin1             ! Surface number of origin window
  INTEGER, INTENT(IN)          :: IWin2             ! Surface number of destination window
  REAL(r64), INTENT(IN)             :: R1(3)             ! Origin of ray (on IWin1) (m)
  REAL(r64), INTENT(IN)             :: R2(3)             ! Destination of ray (on IWin2) (m)
  INTEGER, INTENT(OUT)         :: IHIT              ! Hit flag: 1 = ray hits an obstruction, 0 = does not

          ! SUBROUTINE PARAMETER DEFINITIONS: na
          ! INTERFACE BLOCK SPECIFICATIONS: na
          ! DERIVED TYPE DEFINITIONS: na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: ISurf                        ! Surface index
  INTEGER   :: IType                        ! Surface type/class
  REAL(r64) :: HP(3)                        ! Hit coordinates, if ray hits an obstruction surface (m)
  REAL(r64) :: r12                          ! Distance between R1 and R2 (m)
  REAL(r64) :: d                            ! Distance between R1 and obstruction surface (m)
  REAL(r64) :: RN(3)                        ! Unit vector along ray from R1 to R2

          ! FLOW:
  IHIT = 0

  r12 = SQRT(DOT_PRODUCT(R1 - R2,R1 - R2))
  RN = R2 - R1
  RN = RN / (SQRT(DOT_PRODUCT(RN,RN))) ! Unit vector

  ! Loop over obstructions, which can be building elements, like walls,
  ! or shadowing surfaces, like overhangs. Exclude base surface of window IWin1.
  ! Exclude base surface of window IWin2.
  DO ISurf = 1,TotSurfaces
    IType = Surface(ISurf)%Class

    IF ((IType==SurfaceClass_Wall .OR. IType==SurfaceClass_Roof .OR. IType==SurfaceClass_Floor) &
       .AND. ISurf /= Surface(IWin2)%BaseSurf .AND. ISurf /= Surface(IWin1)%BaseSurf &
       .AND. ISurf /= Surface(Surface(IWin2)%BaseSurf)%ExtBoundCond &
       .AND. ISurf /= Surface(Surface(IWin1)%BaseSurf)%ExtBoundCond) THEN

      IF(Surface(ISurf)%Zone == Surface(IWin2)%Zone) THEN  ! Wall/ceiling/floor is in same zone as destination window
        CALL DayltgPierceSurface(ISurf,R1,RN,IHIT,HP)
        IF (IHIT > 0) THEN
          d = SQRT(DOT_PRODUCT(R1 - HP,R1 - HP))
          IF (d > r12) THEN  ! Discount any hits farther than the window.
            IHIT = 0
          ELSE               ! The hit is closer than the window.
            EXIT
          END IF
        END IF
      END IF

    ELSE IF (Surface(ISurf)%ShadowingSurf) THEN

      CALL DayltgPierceSurface(ISurf,R1,RN,IHIT,HP)
      IF (IHIT > 0) THEN
        d = SQRT(DOT_PRODUCT(R1 - HP,R1 - HP))
        IF (d > r12) THEN  ! Discount any hits farther than the window.
          IHIT = 0
        ELSE               ! The hit is closer than the window.
          EXIT
        END IF
      END IF

    END IF
  END DO

  RETURN

END SUBROUTINE DayltgHitBetWinObstruction

SUBROUTINE DayltgInteriorIllum(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   July 1997
          !       MODIFIED       March 2000, FCW: interpolate clear-sky daylight factors using
          !                      HourOfDay/WeightNow and NextHour/WeightNextHour. Previously
          !                      only HourOfDay was used
          !                      Jan 2001, FCW: interpolate in slat angle for windows with blinds
          !                      that have movable slats
          !                      Oct 2002, LKL: changed interpolation steps to HourOfDay/WeightNow
          !                      LastHour/WeightPreviousHour
          !                      Aug 2003, FCW: fix bug that prevented ShadingControlType =
          !                      MEETDAYLIGHTILLUMINANCESETPOINT from working
          !                      Mar 2004, FCW: fix bug in calc of illuminance setpoint contribution
          !                      to background luminance: now it is divided by pi to give cd/m2
          !                      Mar 2004, FCW: modify to handle daylighting through interior windows
          !                      June 2009, TH: modified for thermochromic windows
          !                      Jan 2010, TH (CR 7984): added iterations for switchable windows with shading
          !                       control of MeetDaylightIlluminanceSetpoint and glare control is active
          !                       Also corrected bugs (CR 7988) for switchable glazings not related to CR 7984

          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Using daylighting factors and exterior illuminance, determine
          ! the current-hour interior daylight illuminance and glare index
          ! at each reference point in a space. Deploy window shading window by window
          ! if glare control is active for window and if the acceptable glare index
          ! is exceeded at both reference points.

          ! Called by InitSurfaceHeatBalance.

          ! METHODOLOGY EMPLOYED:na

          ! REFERENCES:
          ! Based on DOE-2.1E subroutine DINTIL.

          ! USE STATEMENTS:
  USE General, ONLY: POLYF, InterpSlatAng

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER           :: ZoneNum               ! Zone number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: NREFPT                ! Number of daylighting reference points
  INTEGER   :: ISky                  ! Sky type index
  INTEGER   :: ISky1, ISky2          ! Sky type index values for averaging two sky types
  REAL(r64) :: SetPnt(2)             ! Illuminance setpoint at reference points (lux)
  REAL(r64) :: DFSKHR(4,2)           ! Sky daylight factor for sky type (first index),
                                     !   bare/shaded window (second index)
  REAL(r64) :: DFSUHR(2)             ! Sun daylight factor for bare/shaded window
  REAL(r64) :: BFSKHR(4,2)           ! Sky background luminance factor for sky type (first index),
                                     !   bare/shaded window (second index)
  REAL(r64) :: BFSUHR(2)             ! Sun background luminance factor for bare/shaded window
  REAL(r64) :: SFSKHR(4,2)           ! Sky source luminance factor for sky type (first index),
                                     !   bare/shaded window (second index)
  REAL(r64) :: SFSUHR(2)             ! Sun source luminance factor for bare/shaded window
  REAL(r64) :: WDAYIL(2,2)           ! Illuminance from window at reference point (first index)
                                     !   for shade open/closed (second index)
  REAL(r64) :: WBACLU(2,2)           ! Background illuminance from window at reference point (first index)
                                     !   for shade open/closed (second index)
  REAL(r64) :: RDAYIL(2)             ! Illuminance from window at reference point after closing shade
  REAL(r64) :: RBACLU(2)             ! Background illuminance from window at reference point after closing shade
  REAL(r64) :: GLRNDX(2)             ! Glare index at reference point
  REAL(r64) :: GLRNEW(2)             ! New glare index at reference point
  INTEGER   :: IL                    ! Reference point index
  INTEGER   :: IWin                  ! Window index
  INTEGER   :: IS                    ! IS=1 for unshaded window, =2 for shaded window
  INTEGER   :: ISWFLG                ! Switchable glazing flag: =1 if one or more windows in a zone
                                     !  has switchable glazing that adjusts visible transmittance to just meet
                                     !  daylighting set point; =0 otherwise.
  INTEGER   :: IConst                ! Window construction pointer
  INTEGER   :: IConstShaded          ! Pointer to shaded window construction
  INTEGER   :: ICtrl                 ! Window shading control pointer
  REAL(r64) :: DILLSW,DILLUN         ! Illuminance a ref point from windows that can be switched,
                                     !  and from those that can't (lux)
  REAL(r64) :: ASETIL                ! Illuminance ratio (lux)
  REAL(r64) :: TVIS1                 ! Visible transmittance at normal incidence of unswitched glazing
  REAL(r64) :: TVIS2                 ! Visible transmittance at normal incidence of fully-switched glazing
  REAL(r64) :: VTRAT                 ! Ratio between switched and unswitched visible transmittance at normal incidence
  REAL(r64) :: BACL                  ! Window background (surround) luminance for glare calc (cd/m2)
  REAL(r64) :: SkyWeight             ! Weighting factor used to average two different sky types
  REAL(r64) :: HorIllSky(4)          ! Horizontal illuminance for different sky types
  REAL(r64) :: HorIllSkyFac          ! Ratio between horizontal illuminance from sky horizontal irradiance and
                                     !   luminous efficacy and horizontal illuminance from averaged sky
  REAL(r64) :: SlatAng               ! Blind slat angle (rad)
  LOGICAL   :: VarSlats              ! True if slats are movable, i.e., variable angle
  LOGICAL   :: GlareFlag             ! True if maximum glare is exceeded
  INTEGER   :: loop                  ! Loop index

  REAL(r64) :: VTRatio               ! VT (visible transmittance) ratio = VTNow / VTMaster
  REAL(r64) :: VTNow                 ! VT of the time step actual TC window
  REAL(r64) :: VTMaster              ! VT of the base/master TC window

  ! Added variables for glare iterations for switchable glazings
  REAL(r64) :: tmpSWSL1 = 0.0
  REAL(r64) :: tmpSWSL2 = 0.0
  REAL(r64) :: tmpSWIterStep = 0.05d0 ! step of switching factor, assuming maximum of 20 switching states
  REAL(r64) :: tmpSWFactor = 0.0      ! new switching factor to meet glare criteria
  REAL(r64) :: tmpSWFactor0  = 0.0    ! original switching factor to meet daylight illuminance
  REAL(r64) :: tmpMult = 0.0
  LOGICAL   :: GlareOK = .False.
  REAL(r64), SAVE, ALLOCATABLE, DIMENSION(:,:,:) :: tmpIllumFromWinAtRefPt
  REAL(r64), SAVE, ALLOCATABLE, DIMENSION(:,:,:) :: tmpBackLumFromWinAtRefPt
  REAL(r64), SAVE, ALLOCATABLE, DIMENSION(:,:,:) :: tmpSourceLumFromWinAtRefPt

  LOGICAL   :: blnCycle = .False.

  ! Three arrays to save original clear and dark (fully switched) states'
  !  zone/window daylighting properties.
  IF (.NOT.ALLOCATED(tmpIllumFromWinAtRefPt)) THEN
    ALLOCATE(tmpIllumFromWinAtRefPt(2,2,MAX(MAXVAL(Zone%NumSubSurfaces),MAXVAL(ZoneDaylight%NumOfDayltgExtWins))))
    ALLOCATE(tmpBackLumFromWinAtRefPt(2,2,MAX(MAXVAL(Zone%NumSubSurfaces),MAXVAL(ZoneDaylight%NumOfDayltgExtWins))))
    ALLOCATE(tmpSourceLumFromWinAtRefPt(2,2,MAX(MAXVAL(Zone%NumSubSurfaces),MAXVAL(ZoneDaylight%NumOfDayltgExtWins))))
  ENDIF
  tmpIllumFromWinAtRefPt = 0.0d0
  tmpBackLumFromWinAtRefPt = 0.0d0
  tmpSourceLumFromWinAtRefPt = 0.0d0

          ! FLOW:
  ! Limit the number of control reference points to 2
  NREFPT = ZoneDaylight(ZoneNum)%TotalDaylRefPoints
  IF (NREFPT > 2) NREFPT = 2

  ! Initialize reference point illuminance and window background luminance
  DO IL = 1,NREFPT
    SetPnt(IL) = ZoneDaylight(ZoneNum)%IllumSetPoint(IL)
    DaylIllum(IL) = 0.
    ZoneDaylight(ZoneNum)%BacLum(IL) = 0.
  END DO

  IF (SkyClearness > 3.0d0) THEN ! Sky is average of clear and clear turbid
    SkyWeight = MIN(1.0d0,(SkyClearness-3.d0)/3.d0)
    ISky1 = 1
    ISky2 = 2
  ELSE IF (SkyClearness > 1.2d0) THEN ! Sky is average of clear turbid and intermediate
    SkyWeight = (SkyClearness - 1.2d0)/1.8d0
    ISky1 = 2
    ISky2 = 3
  ELSE ! Sky is average of intermediate and overcast
    SkyWeight = MIN(1.0d0, MAX(0.0d0, (SkyClearness-1.d0)/0.2d0, (SkyBrightness-0.05d0)/0.4d0))
    ISky1 = 3
    ISky2 = 4
  END IF

  ! First loop over exterior windows associated with this zone. The window may be an exterior window in
  ! the zone or an exterior window in an adjacent zone that shares an interior window with the zone.
  ! Find contribution of each window to the daylight illum and to the glare numerator at each reference point.
  ! Use shading flags set in WindowShadingManager.
  DO loop = 1,ZoneDaylight(ZoneNum)%NumOfDayltgExtWins
    IWin = ZoneDaylight(ZoneNum)%DayltgExtWinSurfNums(loop)

    ! Added TH 6/29/2009 for thermochromic windows
    VTRatio = 1.0d0
    IF (NREFPT > 0) THEN
      IConst = Surface(IWin)%Construction
      IF (Construct(IConst)%TCFlag == 1) THEN
        ! For thermochromic windows, daylight and glare factors are always calculated
        !  based on the master construction. They need to be adjusted by the VTRatio, including:
        !  ZoneDaylight()%DaylIllFacSky, DaylIllFacSun, DaylIllFacSunDisk; DaylBackFacSky,
        !  DaylBackFacSun, DaylBackFacSunDisk, DaylSourceFacSky, DaylSourceFacSun, DaylSourceFacSunDisk
        VTNow = POLYF(1.0d0,Construct(IConst)%TransVisBeamCoef(1))
        VTMaster = POLYF(1.0d0,Construct(Construct(IConst)%TCMasterConst)%TransVisBeamCoef(1))
        VTRatio = VTNow / VTMaster
      ENDIF
    ENDIF

    ! Loop over reference points
    DO IL = 1, NREFPT

      ! Daylight factors for current sun position
      DO ISky = 1,4

        ! ===Bare window===
        DFSKHR(ISky,1) = VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,IL,ISky,1,HourOfDay) + &
                         WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,IL,ISky,1,PreviousHour))

        IF (ISky == 1) DFSUHR(1) = VTRatio * (WeightNow * (ZoneDaylight(ZoneNum)%DaylIllFacSun(loop,IL,1,HourOfDay) +   &
                                                ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loop,IL,1,HourOfDay)) + &
                                   WeightPreviousHour * (ZoneDaylight(ZoneNum)%DaylIllFacSun(loop,IL,1,PreviousHour) +   &
                                                         ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loop,IL,1,PreviousHour)))

        BFSKHR(ISky,1) = VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylBackFacSky(loop,IL,ISky,1,HourOfDay) + &
                         WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylBackFacSky(loop,IL,ISky,1,PreviousHour))

        IF (ISky == 1) BFSUHR(1) = VTRatio * (WeightNow * (ZoneDaylight(ZoneNum)%DaylBackFacSun(loop,IL,1,HourOfDay) +   &
                                                ZoneDaylight(ZoneNum)%DaylBackFacSunDisk(loop,IL,1,HourOfDay)) + &
                         WeightPreviousHour * (ZoneDaylight(ZoneNum)%DaylBackFacSun(loop,IL,1,PreviousHour) +   &
                                                ZoneDaylight(ZoneNum)%DaylBackFacSunDisk(loop,IL,1,PreviousHour)))

        SFSKHR(ISky,1) = VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylSourceFacSky(loop,IL,ISky,1,HourOfDay) + &
                         WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylSourceFacSky(loop,IL,ISky,1,PreviousHour))

        IF (ISky == 1) SFSUHR(1) = VTRatio * (WeightNow * (ZoneDaylight(ZoneNum)%DaylSourceFacSun(loop,IL,1,HourOfDay) +   &
                                                ZoneDaylight(ZoneNum)%DaylSourceFacSunDisk(loop,IL,1,HourOfDay)) + &
                      WeightPreviousHour * (ZoneDaylight(ZoneNum)%DaylSourceFacSun(loop,IL,1,PreviousHour) +   &
                                            ZoneDaylight(ZoneNum)%DaylSourceFacSunDisk(loop,IL,1,PreviousHour)))

        IF (SurfaceWindow(IWin)%ShadingFlag >= 1 .OR. SurfaceWindow(IWin)%SolarDiffusing) THEN

          ! ===Shaded window or window with diffusing glass===
          IF (.NOT.SurfaceWindow(IWin)%MovableSlats) THEN
            ! Shade, screen, blind with fixed slats, or diffusing glass
            DFSKHR(ISky,2) = VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,IL,ISky,2,HourOfDay) + &
                             WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,IL,ISky,2,PreviousHour))

            IF (ISky == 1) THEN
              DFSUHR(2) = VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylIllFacSun(loop,IL,2,HourOfDay) +  &
                          WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylIllFacSun(loop,IL,2,PreviousHour))


              IF (.NOT.SurfaceWindow(IWin)%SlatsBlockBeam) &
                DFSUHR(2) = DFSUHR(2) + VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loop,IL,2,HourOfDay) + &
                                        WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loop,IL,2,PreviousHour))
            END IF

            BFSKHR(ISky,2) = VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylBackFacSky(loop,IL,ISky,2,HourOfDay) + &
                             WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylBackFacSky(loop,IL,ISky,2,PreviousHour))

            IF (ISky == 1) THEN
              BFSUHR(2) = VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylBackFacSun(loop,IL,2,HourOfDay) +  &
                          WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylBackFacSun(loop,IL,2,PreviousHour))
              IF (.NOT.SurfaceWindow(IWin)%SlatsBlockBeam) &
                BFSUHR(2) = BFSUHR(2) + VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylBackFacSunDisk(loop,IL,2,HourOfDay) + &
                                        WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylBackFacSunDisk(loop,IL,2,PreviousHour))
            END IF

            SFSKHR(ISky,2) = VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylSourceFacSky(loop,IL,ISky,2,HourOfDay) + &
                             WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylSourceFacSky(loop,IL,ISky,2,PreviousHour))

            IF (ISky == 1) THEN
              SFSUHR(2) = VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylSourceFacSun(loop,IL,2,HourOfDay) +  &
                          WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylSourceFacSun(loop,IL,2,PreviousHour))
              IF (.NOT.SurfaceWindow(IWin)%SlatsBlockBeam) &
                SFSUHR(2) = SFSUHR(2) + VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylSourceFacSunDisk(loop,IL,2,HourOfDay) + &
                                        WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylSourceFacSunDisk(loop,IL,2,PreviousHour))
            END IF

          ELSE ! Blind with movable slats
            VarSlats = SurfaceWindow(IWin)%MovableSlats
            SlatAng = SurfaceWindow(IWin)%SlatAngThisTs

            DFSKHR(ISky,2) = VTRatio * (WeightNow * InterpSlatAng(SlatAng,VarSlats,  &
                               ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,IL,ISky,2:MaxSlatAngs+1,HourOfDay)) + &
                             WeightPreviousHour * InterpSlatAng(SlatAng,VarSlats,  &
                               ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,IL,ISky,2:MaxSlatAngs+1,PreviousHour)))

            IF (ISky == 1) THEN
              DFSUHR(2) = VTRatio * (WeightNow * InterpSlatAng(SlatAng,VarSlats,  &
                            ZoneDaylight(ZoneNum)%DaylIllFacSun(loop,IL,2:MaxSlatAngs+1,HourOfDay)) + &
                          WeightPreviousHour * InterpSlatAng(SlatAng,VarSlats,  &
                            ZoneDaylight(ZoneNum)%DaylIllFacSun(loop,IL,2:MaxSlatAngs+1,PreviousHour)))

              ! We add the contribution from the solar disk if slats do not block beam solar
              ! TH CR 8010. DaylIllFacSunDisk needs to be interpolated!
              !IF (.NOT.SurfaceWindow(IWin)%SlatsBlockBeam) DFSUHR(2) = DFSUHR(2) + &
              !            VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loop,IL,2,HourOfDay) + &
              !            WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loop,IL,2,PreviousHour))
              IF (.NOT.SurfaceWindow(IWin)%SlatsBlockBeam) DFSUHR(2) = DFSUHR(2) + &
                          VTRatio * (WeightNow * InterpSlatAng(SlatAng,VarSlats,  &
                          ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loop,IL,2:MaxSlatAngs+1,HourOfDay)) + &
                          WeightPreviousHour * InterpSlatAng(SlatAng,VarSlats,  &
                          ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loop,IL,2:MaxSlatAngs+1,PreviousHour)))
            END IF

            BFSKHR(ISky,2) = VTRatio * (WeightNow * InterpSlatAng(SlatAng,VarSlats,  &
                               ZoneDaylight(ZoneNum)%DaylBackFacSky(loop,IL,ISky,2:MaxSlatAngs+1,HourOfDay)) + &
                             WeightPreviousHour * InterpSlatAng(SlatAng,VarSlats,  &
                               ZoneDaylight(ZoneNum)%DaylBackFacSky(loop,IL,ISky,2:MaxSlatAngs+1,PreviousHour)))

            IF (ISky == 1) THEN
              BFSUHR(2) = VTRatio * (WeightNow * InterpSlatAng(SlatAng,VarSlats,  &
                            ZoneDaylight(ZoneNum)%DaylBackFacSun(loop,IL,2:MaxSlatAngs+1,HourOfDay)) + &
                          WeightPreviousHour * InterpSlatAng(SlatAng,VarSlats,  &
                            ZoneDaylight(ZoneNum)%DaylBackFacSun(loop,IL,2:MaxSlatAngs+1,PreviousHour)))

              ! TH CR 8010. DaylBackFacSunDisk needs to be interpolated!
              !IF (.NOT.SurfaceWindow(IWin)%SlatsBlockBeam) THEN
              !  BFSUHR(2) = BFSUHR(2) + &
              !            VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylBackFacSunDisk(loop,IL,2,HourOfDay) + &
              !            WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylBackFacSunDisk(loop,IL,2,PreviousHour))
              IF (.NOT.SurfaceWindow(IWin)%SlatsBlockBeam) THEN
                BFSUHR(2) = BFSUHR(2) + &
                          VTRatio * (WeightNow * InterpSlatAng(SlatAng,VarSlats,  &
                          ZoneDaylight(ZoneNum)%DaylBackFacSunDisk(loop,IL,2:MaxSlatAngs+1,HourOfDay)) + &
                          WeightPreviousHour * InterpSlatAng(SlatAng,VarSlats,  &
                          ZoneDaylight(ZoneNum)%DaylBackFacSunDisk(loop,IL,2:MaxSlatAngs+1,PreviousHour)))
              END IF
            END IF

            SFSKHR(ISky,2) = VTRatio * (WeightNow * InterpSlatAng(SlatAng,VarSlats,  &
                         ZoneDaylight(ZoneNum)%DaylSourceFacSky(loop,IL,ISky,2:MaxSlatAngs+1,HourOfDay)) + &
                             WeightPreviousHour * InterpSlatAng(SlatAng,VarSlats,  &
                         ZoneDaylight(ZoneNum)%DaylSourceFacSky(loop,IL,ISky,2:MaxSlatAngs+1,PreviousHour)))

            IF (ISky == 1) THEN
              SFSUHR(2) = VTRatio * (WeightNow * InterpSlatAng(SlatAng,VarSlats,  &
                     ZoneDaylight(ZoneNum)%DaylSourceFacSun(loop,IL,2:MaxSlatAngs+1,HourOfDay)) + &
                          WeightPreviousHour * InterpSlatAng(SlatAng,VarSlats,  &
                     ZoneDaylight(ZoneNum)%DaylSourceFacSun(loop,IL,2:MaxSlatAngs+1,PreviousHour)))

              ! TH CR 8010. DaylSourceFacSunDisk needs to be interpolated!
              !IF (.NOT.SurfaceWindow(IWin)%SlatsBlockBeam) THEN
              !  SFSUHR(2) = SFSUHR(2) + &
              !           VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylSourceFacSunDisk(loop,IL,2,HourOfDay) + &
              !           WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylSourceFacSunDisk(loop,IL,2,PreviousHour))
              IF (.NOT.SurfaceWindow(IWin)%SlatsBlockBeam) THEN
                SFSUHR(2) = SFSUHR(2) + &
                         VTRatio * (WeightNow * InterpSlatAng(SlatAng,VarSlats,  &
                         ZoneDaylight(ZoneNum)%DaylSourceFacSunDisk(loop,IL,2:MaxSlatAngs+1,HourOfDay)) + &
                         WeightPreviousHour * InterpSlatAng(SlatAng,VarSlats,  &
                         ZoneDaylight(ZoneNum)%DaylSourceFacSunDisk(loop,IL,2:MaxSlatAngs+1,PreviousHour)))
              END IF
            END IF

          END IF ! End of check if window has blind with movable slats
        END IF ! End of check if window is shaded or has diffusing glass
      END DO ! End of sky type loop, ISky

      ! Get illuminance at ref point from bare and shaded window by
      ! multiplying daylight factors by exterior horizontal illuminance

      ! Adding 0.001 in the following prevents zero HorIllSky in early morning or late evening when sun
      ! is up in the present time step but GILSK(ISky,HourOfDay) and GILSK(ISky,NextHour) are both zero.
      DO ISky = 1,4
        ! HorIllSky(ISky) = WeightNow * GILSK(ISky,HourOfDay) + WeightNextHour * GILSK(ISky,NextHour) + 0.001
        HorIllSky(ISky) = WeightNow * GILSK(ISky,HourOfDay) + WeightPreviousHour * GILSK(ISky,PreviousHour) + 0.001
      END DO

      ! HISKF is current time step horizontal illuminance from sky, calculated in DayltgLuminousEfficacy,
      ! which is called in WeatherManager. HISUNF is current time step horizontal illuminance from sun,
      ! also calculated in DayltgLuminousEfficacy.

      HorIllSkyFac = HISKF/((1-SkyWeight)*HorIllSky(ISky2) + SkyWeight*HorIllSky(ISky1))

      DO IS = 1,2
        IF (IS == 2.and.SurfaceWindow(IWin)%ShadingFlag<=0.and..NOT.SurfaceWindow(IWin)%SolarDiffusing) EXIT

        ZoneDaylight(ZoneNum)%IllumFromWinAtRefpt(IL,IS,loop) = &
          DFSUHR(IS)*HISUNF + HorIllSkyFac * (DFSKHR(ISky1,IS)*SkyWeight*HorIllSky(ISky1) + &
                              DFSKHR(ISky2,IS)*(1.-SkyWeight)*HorIllSky(ISky2))
        ZoneDaylight(ZoneNum)%BackLumFromWinAtRefPt(IL,IS,loop) = &
          BFSUHR(IS)*HISUNF + HorIllSkyFac * (BFSKHR(ISky1,IS)*SkyWeight*HorIllSky(ISky1) + &
                              BFSKHR(ISky2,IS)*(1.-SkyWeight)*HorIllSky(ISky2))

        ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,IS,loop) = &
          SFSUHR(IS)*HISUNF + HorIllSkyFac * (SFSKHR(ISky1,IS)*SkyWeight*HorIllSky(ISky1) + &
                              SFSKHR(ISky2,IS)*(1.-SkyWeight)*HorIllSky(ISky2))

        ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,IS,loop) = &
          MAX(ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,IS,loop),0.0d0)

        ! Added TH 1/21/2010 - save the original clear and dark (fully switched) states'
        !  zone daylighting values, needed for switachable glazings
        tmpIllumFromWinAtRefPt(IL,IS,loop) = ZoneDaylight(ZoneNum)%IllumFromWinAtRefpt(IL,IS,loop)
        tmpBackLumFromWinAtRefPt(IL,IS,loop) = ZoneDaylight(ZoneNum)%BackLumFromWinAtRefPt(IL,IS,loop)
        tmpSourceLumFromWinAtRefPt(IL,IS,loop) = ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,IS,loop)
      END DO ! IS

    END DO ! End of reference point loop, IL
  END DO ! End of first loop over exterior windows associated with this zone

  ! Initialize flag that one or more windows has switchable glazing
  ! control that adjusts visible transmittance to just meet dayltg set point
  ! (and the window has not already been switched)
  ISWFLG = 0

  ! Second loop over windows. Find total daylight illuminance and background luminance
  ! for each ref pt from all exterior windows associated with the zone.  Use shading flags.
  ! This illuminance excludes contribution of inter-reflected illuminance produced by solar
  ! entering the zone through interior windows (which is calculated in DayltgInterReflIllFrIntWins.

  DO loop = 1,ZoneDaylight(ZoneNum)%NumOfDayltgExtWins
    IWin = ZoneDaylight(ZoneNum)%DayltgExtWinSurfNums(loop)
    ICtrl = Surface(IWin)%WindowShadingControlPtr
    IF (ICtrl > 0 .AND. ISWFLG == 0) THEN
      IF (WindowShadingControl(ICtrl)%ShadingControlType == WSCT_MeetDaylIlumSetp.AND. &
       SurfaceWindow(IWin)%ShadingFlag == GlassConditionallyLightened) ISWFLG = 1
    END IF

    ! Determine if illuminance contribution is from bare or shaded window
    !  For switchable glazings with shading control type of WSCT_MeetDaylIlumSetp,
    !   the shading flag is initialized at GlassConditionallyLightened (20), and
    !   the window is initialized at clear state: IS = 1
    !  For other windows with glare control, the shading flag is initialized at >10, to be determined
    IS = 1
    IF((SurfaceWindow(IWin)%ShadingFlag >= 1 .AND. SurfaceWindow(IWin)%ShadingFlag <= 9) .OR. &
      SurfaceWindow(IWin)%SolarDiffusing) IS = 2

    DO IL = 1,NREFPT
      DaylIllum(IL) = DaylIllum(IL) + ZoneDaylight(ZoneNum)%IllumFromWinAtRefPt(IL,IS,loop)
      ZoneDaylight(ZoneNum)%BacLum(IL) = ZoneDaylight(ZoneNum)%BacLum(IL) +  &
          ZoneDaylight(ZoneNum)%BackLumFromWinAtRefPt(IL,IS,loop)
    END DO
  END DO ! End of second window loop over exterior windows associated with this zone

  ! Optical switching control (e.g. electrochromic glass) to adjust
  ! window's vis trans downward so daylight level equals or is as
  ! close as possible to the illuminance setpoint at first reference point.
  ! Assumes vis trans in the fully switched state is less than that in the
  ! unswitched state. Assumes some windows in a space may have this control and
  ! others not.

  ! If daylight illuminance is above set point, allow switching
  IF (ISWFLG /= 0 .AND. DaylIllum(1) > SETPNT(1)) THEN

    ! Third loop over windows.  Get illuminance at ref pt 1 from
    ! windows that can be switched (DILLSW) and those that can't (DILLUN).
    ! Windows that can be switched are initially in the unswitched state.
    DILLSW = 0.
    DILLUN = 0.
    DO loop = 1,ZoneDaylight(ZoneNum)%NumOfDayltgExtWins
      IWin = ZoneDaylight(ZoneNum)%DayltgExtWinSurfNums(loop)
      ICtrl = Surface(IWin)%WindowShadingControlPtr
      IS = 1
      IF((SurfaceWindow(IWin)%ShadingFlag >= 1 .AND. SurfaceWindow(IWin)%ShadingFlag <= 9) .OR. &
             SurfaceWindow(IWin)%SolarDiffusing) IS = 2
      IF (ICtrl > 0) THEN
        IF (SurfaceWindow(IWin)%ShadingFlag == GlassConditionallyLightened .AND. &
        WindowShadingControl(ICtrl)%ShadingControlType == WSCT_MeetDaylIlumSetp) THEN
          DILLSW = DILLSW + ZoneDaylight(ZoneNum)%IllumFromWinAtRefPt(1,IS,loop)
        ELSE
          DILLUN = DILLUN + ZoneDaylight(ZoneNum)%IllumFromWinAtRefPt(1,IS,loop)
        END IF
      END IF
    END DO ! End of third window loop, IWin

    ! Transmittance multiplier
    ASETIL = (SETPNT(1) - DILLUN) / (DILLSW + 0.00001d0)

    ! ASETIL < 1 means there's enough light, so check for switching
    IF (ASETIL < 1.0d0) THEN

      ! Fourth loop over windows to determine which to switch
      DO loop = 1,ZoneDaylight(ZoneNum)%NumOfDayltgExtWins
        IWin = ZoneDaylight(ZoneNum)%DayltgExtWinSurfNums(loop)

        ICtrl = Surface(IWin)%WindowShadingControlPtr
        IF (ICtrl == 0) CYCLE

        IF (SurfaceWindow(IWin)%ShadingFlag /= GlassConditionallyLightened .OR. &
          WindowShadingControl(ICtrl)%ShadingControlType /= WSCT_MeetDaylIlumSetp) CYCLE

        IConst = Surface(IWin)%Construction
        IF(SurfaceWindow(IWin)%StormWinFlag == 1) IConst = Surface(IWin)%StormWinConstruction
        ! Vis trans at normal incidence of unswitched glass
        TVIS1 = POLYF(1.0d0,Construct(IConst)%TransVisBeamCoef(1))*SurfaceWindow(IWin)%GlazedFrac

        ! Vis trans at normal incidence of fully switched glass
        IConstShaded = Surface(IWin)%ShadedConstruction
        TVIS2 = POLYF(1.0d0,Construct(IConstShaded)%TransVisBeamCoef(1))*SurfaceWindow(IWin)%GlazedFrac

        ! Reset shading flag to indicate that window is shaded by being partially or fully switched
        SurfaceWindow(IWin)%ShadingFlag = SwitchableGlazing

        ! ASETIL < 0 means illuminance from non-daylight-switchable windows exceeds setpoint,
        ! so completely switch all daylight-switchable windows to minimize solar gain
        IF (ASETIL <= 0.0) THEN
          SurfaceWindow(IWin)%SwitchingFactor = 1.0d0
          SurfaceWindow(IWin)%VisTransSelected = TVIS2
        ELSE
          ! Case where 0 < ASETIL < 1: darken glass in all
          ! daylight-switchable windows to just meet illuminance setpoint
          ! From this equation: SETPNT(1) = DILLUN + DILLSW/TVIS1 * VisTransSelected
          SurfaceWindow(IWin)%VisTransSelected = MAX(TVIS2, ASETIL * TVIS1) + 0.000001d0
          SurfaceWindow(IWin)%SwitchingFactor = (TVIS1 - SurfaceWindow(IWin)%VisTransSelected)/ (TVIS1 - TVIS2 + 0.000001d0)
        END IF

        ! Adjust daylight quantities based on ratio between switched and unswitched visible transmittance
        DO IL = 1,NREFPT
          ! DaylIllum(IL) and BacLum(IL) were calculated at the clear state: IS = 1,
          !  and need to adjusted for intermediate switched state at VisTransSelected: IS = 2
          IS = 1
          VTRAT = SurfaceWindow(IWin)%VisTransSelected/(TVIS1+0.000001d0)
          DaylIllum(IL) = DaylIllum(IL) + (VTRAT - 1.0d0) * ZoneDaylight(ZoneNum)%IllumFromWinAtRefPt(IL,IS,loop)
          ZoneDaylight(ZoneNum)%BacLum(IL) =  ZoneDaylight(ZoneNum)%BacLum(IL) +  &
                         (VTRAT - 1.0d0) * ZoneDaylight(ZoneNum)%BackLumFromWinAtRefPt(IL,IS,loop)

          ! Adjust illum, background illum and source luminance for this window in intermdeiate switched state
          !  for later used in the DayltgGlare calc because SurfaceWindow(IWin)%ShadingFlag = SwitchableGlazing = 2
          IS = 2
          VTRAT = SurfaceWindow(IWin)%VisTransSelected/(TVIS2+0.000001d0)
          ZoneDaylight(ZoneNum)%IllumFromWinAtRefPt(IL,IS,loop)    = VTRAT * tmpIllumFromWinAtRefPt(IL,IS,loop)
          ZoneDaylight(ZoneNum)%BackLumFromWinAtRefPt(IL,IS,loop)  = VTRAT * tmpBackLumFromWinAtRefPt(IL,IS,loop)
          ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,IS,loop)= VTRAT * tmpSourceLumFromWinAtRefPt(IL,IS,loop)
        END DO ! IL

        ! If new daylight does not exceed the illuminance setpoint, done, no more checking other switchable glazings
        !  even though this should not happen because all switchable glazings suppose to be dimmed by a same ratio ASETIL
        !   In real world, this can be improved by setting priority of each switchable glazing to switch - NFP.
        IF (DaylIllum(1) <= SETPNT(1)) THEN
          EXIT
        ENDIF
      END DO ! End of fourth window loop, IWin -- end of switching to control daylight illuminance

    END IF ! ASETIL < 1
  END IF ! ISWFLG /= 0 .AND. DaylIllum(1) > SETPNT(1)

  ! Calculate glare index at each reference point assuming the daylight illuminance setpoint is
  !  met at both reference points, either by daylight or electric lights
  DO IL = 1,NREFPT
    BACL = MAX(SETPNT(IL) * ZoneDaylight(ZoneNum)%AveVisDiffReflect / Pi, ZoneDaylight(ZoneNum)%BacLum(IL))
    ! DayltgGlare uses ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,1,loop) for unshaded windows, and
    !  ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,2,loop) for shaded windows
    CALL DayltgGlare(IL, BACL, GLRNDX(IL), ZoneNum)
  END DO

  ! Check if glare level is less than maximum allowed at each ref pt.  If maximum
  ! is exceeded at either ref pt, attempt to reduce glare to acceptable level by closing
  ! shading device on windows that have shades that have not already been closed.
  GlareFlag = .FALSE.
  DO IL = 1,NREFPT
    IF (GLRNDX(IL) > ZoneDaylight(ZoneNum)%MaxGlareallowed) THEN
      GlareFlag = .TRUE.
      EXIT
    END IF
  END DO

  IF (GlareFlag) THEN
    ! Glare is too high at a ref pt.  Loop through windows.
    DO loop = 1,ZoneDaylight(ZoneNum)%NumOfDayltgExtWins
      IWin = ZoneDaylight(ZoneNum)%DayltgExtWinSurfNums(loop)

      ! Check if window is eligible for glare control
      ! TH 1/21/2010. Switchable glazings already in partially switched state
      !  should be allowed to further dim to control glare
      !IF (SurfaceWindow(IWin)%ShadingFlag < 10) CYCLE
      IF (SurfaceWindow(IWin)%ShadingFlag < 10 .AND. SurfaceWindow(IWin)%ShadingFlag /= SwitchableGlazing) CYCLE

      ICtrl = Surface(IWin)%WindowShadingControlPtr
      IF (ICtrl == 0) CYCLE
      IF (WindowShadingControl(ICtrl)%GlareControlIsActive) THEN

        ! Illuminance (WDAYIL) and background luminance (WBACLU) contribution from this
        ! window without shading (IS=1) and with shading (IS=2) for each ref pt
        !  For switchable windows, this may be partially switched rather than fully dark
        DO IL = 1,NREFPT
          DO IS = 1,2
            WDAYIL(IL,IS) = ZoneDaylight(ZoneNum)%IllumFromWinAtRefPt(IL,IS,loop)
            WBACLU(IL,IS) = ZoneDaylight(ZoneNum)%BackLumFromWinAtRefPt(IL,IS,loop)
          END DO
        END DO

        ! Recalculate illuminance and glare with shading on this window.
        !  For switchable glazings, this is the fully switched (dark) state
        DO IL = 1,NREFPT
          IF (SurfaceWindow(IWin)%ShadingFlag /= SwitchableGlazing) THEN
            ! for non switchable glazings or switchable glazings not switched yet (still in clear state)
            !  SurfaceWindow(IWin)%ShadingFlag = GlassConditionallyLightened
            RDAYIL(IL) = DaylIllum(IL) - WDAYIL(IL,1) + WDAYIL(IL,2)
            RBACLU(IL) = ZoneDaylight(ZoneNum)%BacLum(IL) - WBACLU(IL,1) + WBACLU(IL,2)
          ELSE
            ! switchable glazings already in partially switched state when calc the RDAYIL(IL) & RBACLU(IL)
            RDAYIL(IL) = DaylIllum(IL) - WDAYIL(IL,2) + tmpIllumFromWinAtRefPt(IL,2,loop)
            RBACLU(IL) = ZoneDaylight(ZoneNum)%BacLum(IL) - WBACLU(IL,2) + tmpBackLumFromWinAtRefPt(IL,2,loop)
          ENDIF
        END DO

        IF (SurfaceWindow(IWin)%ShadingFlag /= SwitchableGlazing) &
          SurfaceWindow(IWin)%ShadingFlag = SurfaceWindow(IWin)%ShadingFlag / 10

        !For switchable glazings, it is switched to fully dark state,
        ! update ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,2,loop) for use in DayltgGlare
        IF (SurfaceWindow(IWin)%ShadingFlag == SwitchableGlazing) THEN
          DO IL = 1,NREFPT
            ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,2,loop) = tmpSourceLumFromWinAtRefPt(IL,2,loop)
            ZoneDaylight(ZoneNum)%IllumFromWinAtRefPt(IL,2,loop)    = tmpIllumFromWinAtRefPt(IL,2,loop)
            ZoneDaylight(ZoneNum)%BackLumFromWinAtRefPt(IL,2,loop)  = tmpBackLumFromWinAtRefPt(IL,2,loop)
          END DO

          IConst = Surface(IWin)%Construction
          ! Vis trans at normal incidence of unswitched glass
          TVIS1 = POLYF(1.0d0,Construct(IConst)%TransVisBeamCoef(1))*SurfaceWindow(IWin)%GlazedFrac

          ! Vis trans at normal incidence of fully switched glass
          IConstShaded = Surface(IWin)%ShadedConstruction
          TVIS2 = POLYF(1.0d0,Construct(IConstShaded)%TransVisBeamCoef(1))*SurfaceWindow(IWin)%GlazedFrac
        ENDIF

        ! Re-calc daylight and glare at shaded state. For switchable glazings, it is the fully dark state.
        DO IL = 1,NREFPT
          BACL = MAX(SETPNT(IL) * ZoneDaylight(ZoneNum)%AveVisDiffReflect / Pi, RBACLU(IL))
          ! DayltgGlare uses ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,2,loop) for shaded state
          CALL DayltgGlare(IL, BACL, GLRNEW(IL), ZoneNum)
        END DO

        blnCycle = .False.
        IF (NREFPT == 1 .AND. GLRNEW(1) > GLRNDX(1)) THEN
          ! One ref pt;  go to next window if glare has increased.
          blnCycle = .True.
        ELSEIF (NREFPT > 1) THEN
          ! Two ref pts.  There are three cases depending on glare values.
          IF (GLRNDX(1) > ZoneDaylight(ZoneNum)%MaxGlareallowed .AND. GLRNDX(2) > ZoneDaylight(ZoneNum)%MaxGlareallowed) THEN
            !
            ! (1) Initial glare too high at both ref pts.  Deploy shading on
            !     this window if this decreases glare at both ref pts.
            IF (GLRNEW(1) > GLRNDX(1) .OR. GLRNEW(2) > GLRNDX(2)) blnCycle = .True.
          ELSE IF (GLRNDX(1) > ZoneDaylight(ZoneNum)%MaxGlareallowed .AND. GLRNDX(2) <= ZoneDaylight(ZoneNum)%MaxGlareallowed) THEN
            !
            ! (2) Initial glare too high only at first ref pt.  Deploy shading
            !     on this window if glare at first ref pt decreases and
            !     glare at second ref pt stays below max.
            IF (GLRNEW(1) > GLRNDX(1) .OR. GLRNEW(2) > ZoneDaylight(ZoneNum)%MaxGlareallowed) blnCycle = .True.
          ELSE
            !
            ! (3) Initial glare too high at second ref pt.  Deploy shading if glare
            !     at second ref pt decreases and glare at first ref pt stays below max.
            IF (GLRNEW(2) > GLRNDX(2) .OR. GLRNEW(1) > ZoneDaylight(ZoneNum)%MaxGlareallowed) blnCycle = .True.
          END IF
        END IF

        ! Shading this window has not improved the glare situation.
        ! Reset shading flag to no shading condition, go to next window.
        IF (blnCycle) THEN
          !  for switchable glazings, reset properties to clear state or partial switched state?
          IF (SurfaceWindow(IWin)%ShadingFlag == SwitchableGlazing) THEN
            SurfaceWindow(IWin)%SwitchingFactor = 0.0d0
            SurfaceWindow(IWin)%VisTransSelected = TVIS1

            ! RESET properties for fully dark state
            DO IL = 1,NREFPT
              ZoneDaylight(ZoneNum)%IllumFromWinAtRefPt(IL,2,loop)    = tmpIllumFromWinAtRefPt(IL,2,loop)
              ZoneDaylight(ZoneNum)%BackLumFromWinAtRefPt(IL,2,loop)  = tmpBackLumFromWinAtRefPt(IL,2,loop)
              ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,2,loop)= tmpSourceLumFromWinAtRefPt(IL,2,loop)
            ENDDO
          ENDIF

          SurfaceWindow(IWin)%ShadingFlag = ShadeOff
          CYCLE
        ENDIF

        ! Shading this window has improved the glare situation.
        ! Reset background luminance, glare index, and daylight illuminance at each ref pt.
        ! For switchable glazings, this is fully switched, dark state
        DO IL = 1,NREFPT
          ZoneDaylight(ZoneNum)%BacLum(IL) = RBACLU(IL)
          GLRNDX(IL) = GLRNEW(IL)
          DaylIllum(IL) = RDAYIL(IL)
        END DO


        ! TH comments (5/22/2009): seems for EC windows, if the calculated glare exceeds the max setpoint,
        !  the EC windows will be reset to fully dark state which significantly reduces the available daylight.
        !  A better way is to dim the EC windows as necessary just to meet the glare index, which will still
        !  provide more daylight while not exceeding the max glare! The question is then how to set the
        !  SwitchingFactor to just meet the glare index.
        !  This was addressed in CR 7984 for E+ 5.0. 1/19/2010

        ! If switchable glazing, set switching factor to 1: fully switched.
        IF (SurfaceWindow(IWin)%ShadingFlag == SwitchableGlazing) THEN
          tmpSWFactor0 = SurfaceWindow(IWin)%SwitchingFactor  ! save original switching factor
          SurfaceWindow(IWin)%SwitchingFactor = 1.0d0
          SurfaceWindow(IWin)%VisTransSelected = TVIS2

          ! restore fully dark values
          DO IL = 1,NREFPT
             WDAYIL(IL,2) = tmpIllumFromWinAtRefPt(IL,2,loop)
             WBACLU(IL,2) = tmpBackLumFromWinAtRefPt(IL,2,loop)
             ZoneDaylight(ZoneNum)%IllumFromWinAtRefPt(IL,2,loop)    = tmpIllumFromWinAtRefPt(IL,2,loop)
             ZoneDaylight(ZoneNum)%BackLumFromWinAtRefPt(IL,2,loop)  = tmpBackLumFromWinAtRefPt(IL,2,loop)
             ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,2,loop)= tmpSourceLumFromWinAtRefPt(IL,2,loop)
          END DO
        ENDIF

        ! Check if glare now acceptable at each ref pt.
        GlareOK = .False.
        IF (NREFPT == 1) THEN
          IF (GLRNDX(1) <= ZoneDaylight(ZoneNum)%MaxGlareallowed) GlareOK = .True.
        ELSEIF (NREFPT > 1) THEN
          IF (GLRNDX(1) <= ZoneDaylight(ZoneNum)%MaxGlareallowed .AND. &
            GLRNDX(2) <= ZoneDaylight(ZoneNum)%MaxGlareallowed) GlareOK = .True.
        ENDIF

        IF (GlareOK) THEN
          IF (SurfaceWindow(IWin)%ShadingFlag == SwitchableGlazing .AND. &
            WindowShadingControl(ICtrl)%ShadingControlType == WSCT_MeetDaylIlumSetp) THEN
            ! Added TH 1/14/2010
            ! Only for switchable glazings with MeetDaylightIlluminanceSetpoint control
            ! The glazing is in fully dark state, it might lighten a bit to provide more daylight
            !  while meeting maximum discomfort glare index
            ! Iteration to find the right switching factor meeting the glare index

            ! get fully dark state values
            tmpSWSL1 = tmpSourceLumFromWinAtRefPt(1,2,loop)
            IF (NREFPT > 1) tmpSWSL2 = tmpSourceLumFromWinAtRefPt(2,2,loop)

            ! use simple fixed step search in iteraction, can be improved in future
            tmpSWFactor = 1.0d0 - tmpSWIterStep
            DO WHILE (tmpSWFactor > 0)
              ! calc new glare at new switching state
              DO IL = 1,NREFPT
                RDAYIL(IL) = DaylIllum(IL) + (WDAYIL(IL,1) - WDAYIL(IL,2)) * (1.0d0 - tmpSWFactor)
                RBACLU(IL) = ZoneDaylight(ZoneNum)%BacLum(IL) + (WBACLU(IL,1) - WBACLU(IL,2)) * (1.0d0 - tmpSWFactor)
                BACL = MAX(SETPNT(IL) * ZoneDaylight(ZoneNum)%AveVisDiffReflect / Pi, RBACLU(IL))
                ! needs to update SourceLumFromWinAtRefPt(IL,2,loop) before re-calc DayltgGlare
                tmpMult = (TVIS1 - (TVIS1 - TVIS2) * tmpSWFactor) / TVIS2
                IF (IL == 1) THEN
                  ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,2,loop) = tmpSWSL1 * tmpMult
                ELSE
                  ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,2,loop) = tmpSWSL2 * tmpMult
                ENDIF
                ! Calc new glare
                CALL DayltgGlare(IL, BACL, GLRNEW(IL), ZoneNum)
              END DO

              ! Check whether new glare is OK
              GlareOK = .False.
              IF (NREFPT == 1) THEN
                IF (GLRNEW(1) <= ZoneDaylight(ZoneNum)%MaxGlareallowed) GlareOK = .True.
              ELSEIF (NREFPT > 1) THEN
                IF (GLRNEW(1) <= ZoneDaylight(ZoneNum)%MaxGlareallowed .AND. &
                  GLRNEW(2) <= ZoneDaylight(ZoneNum)%MaxGlareallowed) GlareOK = .True.
              ENDIF

              IF (GlareOK) THEN
                IF (tmpSWFactor >= tmpSWIterStep) THEN
                  ! Continue to lighten the glazing
                  tmpSWFactor = tmpSWFactor - tmpSWIterStep
                  CYCLE
                ELSE
                  ! Glare still OK but glazing already in clear state, no more lighten
                  EXIT
                ENDIF
              ELSE
                ! Glare too high, exit and use previous switching state
                tmpSWFactor = tmpSWFactor + tmpSWIterStep
                EXIT
              ENDIF
            END DO

            ! Final re-calculation if needed
            IF (.NOT. GlareOK) THEN
              ! Glare too high, use previous state and re-calc
              DO IL = 1,NREFPT
                RDAYIL(IL) = DaylIllum(IL) + (WDAYIL(IL,1) - WDAYIL(IL,2)) * (1.0d0 - tmpSWFactor)
                RBACLU(IL) = ZoneDaylight(ZoneNum)%BacLum(IL) + (WBACLU(IL,1) - WBACLU(IL,2)) * (1.0d0 - tmpSWFactor)
                BACL = MAX(SETPNT(IL) * ZoneDaylight(ZoneNum)%AveVisDiffReflect / Pi, RBACLU(IL))

                ! needs to update SourceLumFromWinAtRefPt(IL,2,IWin) before re-calc DayltgGlare
                tmpMult = (TVIS1 - (TVIS1 - TVIS2) * tmpSWFactor) / TVIS2
                IF (IL == 1) THEN
                  ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(1,2,loop) = tmpSWSL1 * tmpMult
                ELSE
                  ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(2,2,loop) = tmpSWSL2 * tmpMult
                ENDIF
                CALL DayltgGlare(IL, BACL, GLRNEW(IL), ZoneNum)
              END DO
            ENDIF

            !Update final results
            DO IL = 1,NREFPT
              ZoneDaylight(ZoneNum)%BacLum(IL) = RBACLU(IL)
              GLRNDX(IL) = GLRNEW(IL)
              DaylIllum(IL) = RDAYIL(IL)

              tmpMult = (TVIS1 - (TVIS1 - TVIS2) * tmpSWFactor) / TVIS2
              !update report variables
              ZoneDaylight(ZoneNum)%IllumFromWinAtRefPt(IL,2,loop) = tmpIllumFromWinAtRefPt(IL,2,loop) * tmpMult
              ZoneDaylight(ZoneNum)%BackLumFromWinAtRefPt(IL,2,loop) = tmpBackLumFromWinAtRefPt(IL,2,loop) * tmpMult
            END DO
            SurfaceWindow(IWin)%SwitchingFactor = tmpSWFactor
            SurfaceWindow(IWin)%VisTransSelected = TVIS1 - (TVIS1 - TVIS2) * tmpSWFactor

          ELSE
            !For un-switchable glazing or switchable glazing but not MeetDaylightIlluminaceSetpoint control,
            ! it is in shaded state and glare is ok - job is done, exit the window loop - IWin
            EXIT
          ENDIF
     !
     !   ELSE
     !     ! glare still high at either ref pt. go to next window
     !     !  clean up for switchable glazings
     !     IF (SurfaceWindow(IWin)%ShadingFlag == SwitchableGlazing) THEN
     !       ! Already in fully dark state
     !       DO IL = 1,NREFPT
     !         ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(IL,2,loop) = tmpSourceLumFromWinAtRefPt(IL,2,loop)
     !         ZoneDaylight(ZoneNum)%IllumFromWinAtRefPt(IL,2,loop) = tmpIllumFromWinAtRefPt(IL,2,loop)
     !         ZoneDaylight(ZoneNum)%BackLumFromWinAtRefPt(IL,2,loop) = tmpBackLumFromWinAtRefPt(IL,2,loop)
     !       END DO
     !     ENDIF
        ENDIF

      END IF ! End of check if window glare control is active
    END DO ! End of window loop, IWin
  END IF ! GlareFlag

  ! Loop again over windows and reset remaining shading flags that
  ! are 10 or higher (i.e., conditionally off) to off
  DO IWin = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
    IF (Surface(IWin)%Class /= SurfaceClass_Window) CYCLE
    IF (Surface(IWin)%ExtBoundCond /= ExternalEnvironment) CYCLE
    IF (SurfaceWindow(IWin)%ShadingFlag >= 10) SurfaceWindow(IWin)%ShadingFlag = ShadeOff
  END DO

  ! Variables for reporting
  DO IL = 1,NREFPT
    ZoneDaylight(ZoneNum)%DaylIllumAtRefPt(IL) = DaylIllum(IL)
    ZoneDaylight(ZoneNum)%GlareIndexAtRefPt(IL) = GLRNDX(IL)

    !added TH 12/2/2008
    IF (GLRNDX(IL) > ZoneDaylight(ZoneNum)%MaxGlareallowed) THEN
      ZoneDaylight(ZoneNum)%TimeExceedingGlareIndexSPAtRefPt(IL) = TimeStepZone  !fraction of hours
    ELSE
      ZoneDaylight(ZoneNum)%TimeExceedingGlareIndexSPAtRefPt(IL) = 0.0d0
    ENDIF

    !added TH 7/6/2009
    IF (DaylIllum(IL) > ZoneDaylight(ZoneNum)%IllumSetPoint(IL)) THEN
      ZoneDaylight(ZoneNum)%TimeExceedingDaylightIlluminanceSPAtRefPt(IL) = TimeStepZone  !fraction of hours
    ELSE
      ZoneDaylight(ZoneNum)%TimeExceedingDaylightIlluminanceSPAtRefPt(IL) = 0.0d0
    ENDIF
  END DO

  ! The following report variables are valid only for daylit zones without interior windows
  IF(.NOT.Zone(ZoneNum)%IntZWindow) THEN
    DO loop = 1,ZoneDaylight(ZoneNum)%NumOfDayltgExtWins
      IWin = ZoneDaylight(ZoneNum)%DayltgExtWinSurfNums(loop)
      IS = 1
      IF(SurfaceWindow(IWin)%ShadingFlag > 0 .OR. SurfaceWindow(IWin)%SolarDiffusing) IS = 2
      SurfaceWindow(IWin)%IllumFromWinAtRefPt1Rep = ZoneDaylight(ZoneNum)%IllumFromWinAtRefPt(1,IS,loop)
      SurfaceWindow(IWin)%LumWinFromRefPt1Rep     = ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(1,IS,loop)
      IF (ZoneDaylight(ZoneNum)%TotalDaylRefPoints > 1) THEN
        SurfaceWindow(IWin)%IllumFromWinAtRefPt2Rep = ZoneDaylight(ZoneNum)%IllumFromWinAtRefPt(2,IS,loop)
        SurfaceWindow(IWin)%LumWinFromRefPt2Rep     = ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(2,IS,loop)
      ENDIF
    END DO
  END IF

  RETURN

END SUBROUTINE DayltgInteriorIllum

SUBROUTINE DayltgInteriorTDDIllum

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   October 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculate the TDD Pipe illuminance values

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: PipeNum               ! TDD pipe object number
  REAL(r64) :: TDDTransVisDiffNow    ! TDD diffuse visible transmittance at the current hour
  REAL(r64) :: TDDTransVisDiffPrev   ! TDD diffuse visible transmittance at the previous hour
  REAL(r64) :: TDDTransVisDiff(4)    ! Weighted diffuse visible transmittance for each sky type
  INTEGER   :: ISky                  ! Sky type index
  INTEGER   :: ISky1, ISky2          ! Sky type index values for averaging two sky types
  REAL(r64) :: SkyWeight             ! Weighting factor used to average two different sky types

  IF (SkyClearness > 3.0d0) THEN ! Sky is average of clear and clear turbid
    SkyWeight = MIN(1.0d0,(SkyClearness-3.d0)/3.d0)
    ISky1 = 1
    ISky2 = 2
  ELSE IF (SkyClearness > 1.2d0) THEN ! Sky is average of clear turbid and intermediate
    SkyWeight = (SkyClearness - 1.2d0)/1.8d0
    ISky1 = 2
    ISky2 = 3
  ELSE ! Sky is average of intermediate and overcast
    SkyWeight = MIN(1.0d0, MAX(0.0d0, (SkyClearness-1.d0)/0.2d0, (SkyBrightness-0.05d0)/0.4d0))
    ISky1 = 3
    ISky2 = 4
  END IF

  ! Calculate and report TDD visible transmittances
  DO PipeNum = 1, NumOfTDDPipes

    TDDPipe(PipeNum)%TransVisBeam = WeightNow * TDDTransVisBeam(PipeNum,HourOfDay) &
      + WeightPreviousHour * TDDTransVisBeam(PipeNum,PreviousHour)

    DO ISky = 1,4
      IF (TDDFluxInc(PipeNum,ISky,HourOfDay) > 0.0) THEN
        TDDTransVisDiffNow = TDDFluxTrans(PipeNum,ISky,HourOfDay) / TDDFluxInc(PipeNum,ISky,HourOfDay)
      ELSE
        TDDTransVisDiffNow = 0.0
      END IF

      IF (TDDFluxInc(PipeNum,ISky,PreviousHour) > 0.0) THEN
        TDDTransVisDiffPrev = TDDFluxTrans(PipeNum,ISky,PreviousHour) / TDDFluxInc(PipeNum,ISky,PreviousHour)
      ELSE
        TDDTransVisDiffPrev = 0.0
      END IF

      TDDTransVisDiff(ISky) = WeightNow * TDDTransVisDiffNow + WeightPreviousHour * TDDTransVisDiffPrev
    END DO ! ISky

    TDDPipe(PipeNum)%TransVisDiff = SkyWeight * TDDTransVisDiff(ISky1) + (1.0d0 - SkyWeight) * TDDTransVisDiff(ISky2)
  END DO ! PipeNum


  RETURN

END SUBROUTINE DayltgInteriorTDDIllum

SUBROUTINE DayltgElecLightingControl(ZoneNum)

        ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   July 1997
          !       MODIFIED       Mar 2004, FCW: add inter-reflected illuminance from interior windows to DaylIllum
          !                      Apr 2004, FCW: move CALL ReportIllumMap from DayltgInteriorIllum2 (DayltgInteriorMapIllum)
          !                      Apr 2010, BG NREL: remove inter-reflected illuminance to stop double counting
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! For a daylit space, determines lighting power reduction factor due to
          ! daylighting for different lighting control systems.

          ! Called by InitSurfaceHeatBalance.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! Based on DOE-2.1E subroutine DLTSYS.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER           :: ZoneNum                      ! Zone number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: TotReduction                 ! Electric lighting power reduction factor for a zone
                                            !  due to daylighting
  INTEGER   :: NREFPT                       ! Number of daylighting reference points in a zone
  REAL(r64) :: ZFTOT                        ! Fraction of zone's floor area that has daylighting controls
  INTEGER   :: IL                           ! Reference point index
  INTEGER   :: LSYSTP                       ! Lighting control type: 1=continuous dimming, 2=stepped,
                                            !  3=continuous dimming then off
  REAL(r64) :: ZFRAC                        ! Fraction of zone controlled by a reference point
  REAL(r64) :: FL                           ! Fraction electric lighting output required to meet setpoint
  REAL(r64) :: FP                           ! Fraction electric lighting power input required to meet setpoint
  REAL(r64) :: XRAN                         ! Random number between 0 and 1
  INTEGER   :: MapNum                       ! Illuminance map number

          ! FLOW:

  TotReduction = 0.

  ! Limit the number of control reference points to 2
  NREFPT = ZoneDaylight(ZoneNum)%TotalDaylRefPoints
  IF (NREFPT > 2) NREFPT = 2

  ! Total fraction of zone that is daylit
  ZFTOT  = ZoneDaylight(ZoneNum)%FracZoneDaylit(1)
  IF (NREFPT > 1) ZFTOT = ZFTOT + ZoneDaylight(ZoneNum)%FracZoneDaylit(2)

  ! Loop over reference points
  DO IL = 1,NREFPT
    DaylIllum(IL)  = ZoneDaylight(ZoneNum)%DaylIllumAtRefPt(IL)
    IF (DaylIllum(IL) >= ZoneDaylight(ZoneNum)%IllumSetPoint(IL)) THEN
      FL = 0.
    ELSE
      FL = (ZoneDaylight(ZoneNum)%IllumSetPoint(IL) - DaylIllum(IL)) / ZoneDaylight(ZoneNum)%IllumSetPoint(IL)
    END IF

    ! BRANCH ON LIGHTING SYSTEM TYPE
    LSYSTP = ZoneDaylight(ZoneNum)%LightControlType
    IF (LSYSTP /= 2) THEN
      ! Continuously dimmable system with linear power curve
      !
      ! Fractional output power required to meet setpoint
      FP = 1.0
      ! LIGHT-CTRL-TYPE = CONTINUOUS (LSYSTP = 1)
      IF (FL <= ZoneDaylight(ZoneNum)%MinLightFraction) FP = ZoneDaylight(ZoneNum)%MinPowerFraction
      ! LIGHT-CTRL-TYPE = CONTINUOUS/OFF (LSYSTP = 3)
      IF (FL <= ZoneDaylight(ZoneNum)%MinLightFraction .AND. LSYSTP == 3) FP = 0.0
      IF (FL > ZoneDaylight(ZoneNum)%MinLightFraction .AND. FL < 1.0) &
        FP = (FL + (1.d0 - FL) * ZoneDaylight(ZoneNum)%MinPowerFraction - ZoneDaylight(ZoneNum)%MinLightFraction) / &
             (1.d0 - ZoneDaylight(ZoneNum)%MinLightFraction)

    ELSE ! LSYSTP = 2
      ! Stepped system
      FP = 0.
      IF (DaylIllum(IL) > 0.0 .AND. DaylIllum(IL) < ZoneDaylight(ZoneNum)%IllumSetPoint(IL)) &
        FP = REAL(INT(ZoneDaylight(ZoneNum)%LightControlSteps*FL) + 1,r64) / ZoneDaylight(ZoneNum)%LightControlSteps

      IF (DaylIllum(IL) == 0.) FP = 1.0

      IF (ZoneDaylight(ZoneNum)%LightControlProbability < 1.0) THEN
        ! Manual operation.  Occupant sets lights one level too high a fraction of the time equal to
        ! 1. - ZoneDaylight(ZoneNum)%LightControlProbability.  RANDOM_NUMBER returns a random number
        ! between 0 and 1.
        CALL RANDOM_NUMBER(XRAN)
        IF (XRAN >= ZoneDaylight(ZoneNum)%LightControlProbability) THEN
          ! Set level one higher
          IF (FP < 1.0) FP = FP + 1.0 / ZoneDaylight(ZoneNum)%LightControlSteps
        END IF ! XRAN
      END IF ! Light Control Probability < 1
    END IF ! Lighting System Type

    ZoneDaylight(ZoneNum)%RefPtPowerReductionFactor(IL) = FP

    ! Accumulate net ltg power reduction factor for entire zone
    ZFRAC = ZoneDaylight(ZoneNum)%FracZoneDaylit(IL)
    TotReduction = TotReduction + ZoneDaylight(ZoneNum)%RefPtPowerReductionFactor(IL) * ZFRAC

  END DO ! End of loop over reference points, IL

  ! Correct for fraction of zone (1-ZFTOT) not controlled by
  ! the reference points.  For this fraction (which is usually zero),
  ! the electric lighting is unaffected and the power reduction
  ! factor is therefore 1.0.
  TotReduction = TotReduction + (1.0 - ZFTOT)

  ZoneDaylight(ZoneNum)%ZonePowerReductionFactor = TotReduction

  IF(TotIllumMaps > 0 .and. .not. DoingSizing .and. .not. WarmupFlag) THEN
    ! If an illuminance map is associated with this zone, generate the map
    IF (TimeStep == 1) mapResultsToReport=.false.
    DO MapNum = 1, TotIllumMaps
      IF (IllumMap(MapNum)%Zone == ZoneNum) THEN
        DO IL = 1,ZoneDaylight(ZoneNum)%TotalMapRefPoints
          ZoneDaylight(ZoneNum)%DaylIllumAtMapPtHr(IL) = ZoneDaylight(ZoneNum)%DaylIllumAtMapPtHr(IL) + &
              ZoneDaylight(ZoneNum)%DaylIllumAtMapPt(IL)/NumOfTimeStepInHour
          IF (ZoneDaylight(ZoneNum)%DaylIllumAtMapPtHr(IL) > 0.0d0) THEN
            mapResultsToReport=.true.
            mapResultsReported=.true.
          ENDIF
        END DO
        CALL ReportIllumMap(MapNum)
        IF (TimeStep == NumOfTimeStepInHour) THEN
          ZoneDayLight(ZoneNum)%DaylIllumAtMapPtHr=0.0
          ZoneDaylight(ZoneNum)%DaylIllumAtMapPt=0.0
        ENDIF
      END IF
    END DO
  END IF

  RETURN

END SUBROUTINE DayltgElecLightingControl


FUNCTION DayltgGlarePositionFactor(X, Y)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   July 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! by table interpolation, evaluates the
          ! Hopkinson position factor used in glare calculation
          ! (Hopkinson, Petherbridge, AND Longmore -- Daylighting,
          ! London, 1966, PP 307, 323).  X (Y) is the lateral
          ! (vertical) distance of luminous window element from
          ! horizontal line of vision, divided by horizontal distance
          ! from eye of observer. The array PF contains values of
          ! the position factor for X = 0, 0.5, 1.0, 1.5, 2.0, 2.5,
          ! and 3.0 and Y = 0, 0.5, 1.0, 1.5, 2.0. Called by CalcDayltgCoefficients.

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! Based on DOE-2.1E subroutine DPFAC.

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64)   :: X,Y                     ! Lateral and vertical distance of luminous window element from
                                         !  horizontal line of vision, divided by horizontal distance from
                                         !  eye of observer

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER     :: IX,IY                   ! Lateral and vertical displacement indices
  REAL(r64)   :: X1,Y1                   ! Lateral and vertical displacement ratios
  REAL(r64)   :: FA,FB                   ! Intermediate variables
  REAL(r64)   :: DayltgGlarePositionFactor  ! Position factor

  REAL(r64), SAVE, DIMENSION(7,5) :: PF  ! Position factor array
  DATA PF  &
   / 1.00d0, .492d0, .226d0, .128d0, .081d0, .061d0, .057d0,  &
     .123d0, .119d0, .065d0, .043d0, .029d0, .026d0, .023d0,  &
     .019d0, .026d0, .019d0, .016d0, .014d0, .011d0, .011d0,  &
     .008d0, .008d0, .008d0, .008d0, .008d0, .006d0, .006d0,  &
     0.00d0, 0.00d0, .003d0, .003d0, .003d0, .003d0, .003d0 /


          ! FLOW:
  DayltgGlarePositionFactor = 0.
  IF (X < 0.0 .OR. X >= 3.0) RETURN
  IF (Y < 0.0 .OR. Y >= 2.0) RETURN

  IX = 1 + INT(2.d0 * X)
  IY = 1 + INT(2.d0 * Y)
  X1 = 0.5 * REAL(IX - 1,r64)
  Y1 = 0.5 * REAL(IY - 1,r64)
  FA = PF(IX,IY) + 2. * (X - X1) * (PF(IX + 1,IY) - PF(IX,IY))
  FB = PF(IX,IY + 1) + 2. * (X-X1) * (PF(IX + 1,IY + 1) - PF(IX,IY + 1))
  DayltgGlarePositionFactor = FA + 2.d0 * (Y - Y1) * (FB - FA)

  RETURN

END FUNCTION DayltgGlarePositionFactor


SUBROUTINE DayltgInterReflectedIllum(ISunPos,IHR,ZoneNum,IWin)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   July 1997
          !       MODIFIED       FCW December 1998
          !                      FCW June 2001: Add blind calculations
          !                      FCW Jan 2001: Add blinds with movable slats
          !                      FCW Jan 2003: Add between-glass blinds
          !                      FCW Jul 2003: account for transmittance of shading surfaces
          !                       (previously these were assumed opaque even if transmittance schedule
          !                        value was non-zero)
          !                      FCW Aug 2003: modify initialization of WLUMSK from WLUMSK = 0. TO
          !                        WLUMSK(:,:,IHR) = 0. Otherwise values calculated in previous
          !                        call are incorrectly zeroed. Result was that window luminance with
          !                        shade or blind included only contribution from first window element
          !                        in window element loop in CalcDayltgCoefficients, thus seriously
          !                        undercalculating window luminance for windows with more than one
          !                        window element. Similarly, modified initialization of WLUMSU from
          !                        WLUMSU = 0. to WLUMSU(:,IHR) = 0., and of WLUMSUdisk from
          !                        WLUMSUdisk = 0. to WLUMSUdisk(:,IHR) = 0.
          !                      PGE Aug 2003: Add daylighting shelves.
          !                      FCW Nov 2003: Add beam solar and sky solar reflected from obstructions;
          !                                    add beam solar reflected from ground accounting for obstructions.
          !                      FCW Nov 2003: increase NPHMAX from 9 to 10 to avoid rays with altitude angle = 0
          !                                    for vertical surfaces.
          !                      FCW Nov 2003: fix the expression for min and max limits of azimuth; old expression
          !                                    broke down for window normals with negative altitude angle
          !                      FCW Nov 2003: add specular reflection from exterior obstructions
          !                      FCW Apr 2004: add light well efficiency multiplying window transmittance
          !                      FCW Apr 2004: add diffusing glazing
          !                      RAR (FSEC)  May 2006: add exterior window screen
          !                      B. Griffith NREL April 2010: CR7869 add adjacent zone area if window is not on this zone
          !                                    apply interior window transmission and blocking to beam transmission from ext win
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Called from CalcDayltgCoefficients for each window and reference point in a daylit
          ! space, for each sun position. Calculates illuminance (EINTSK and EINTSU) at reference point due
          ! to internally reflected light by integrating to determine the amount of flux from
          ! sky and ground (and beam reflected from obstructions) transmitted through
          ! the center of the window and then reflecting this
          ! light from the inside surfaces of the space.  The "split-flux" method is used
          ! (Lynes, Principles of Natural Lighting, 1968).  EINT is determined for
          ! different sky types and for window with and without shades, screens or blinds.
          !
          ! Also finds luminance (WLUMSK and WLUMSU) of window with shade or blind, &
          ! or with diffusing glass, for different sky types.

          ! METHODOLOGY EMPLOYED:na

          ! REFERENCES:
          ! Based on DOE-2.1E subroutine DREFLT.

          ! USE STATEMENTS:
  USE General, ONLY: InterpProfAng, POLYF, BlindBeamBeamTrans
  USE DaylightingDevices, ONLY: FindTDDPipe, TransTDD
  USE DataSystemVariables, ONLY: DetailedSkyDiffuseAlgorithm

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER           :: ISunPos                   ! Sun position counter; used to avoid calculating various
                                                 !  quantities that do not depend on sun position.
  INTEGER           :: IHR                       ! Hour of day
  INTEGER           :: ZoneNum                   ! Zone number
  INTEGER           :: IWin                      ! Window index

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER  :: NPHMAX = 10             ! Number of sky/ground integration steps in altitude
  INTEGER, PARAMETER  :: NTHMAX = 16             ! Number of sky/ground integration steps in azimuth

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  ! In the following I,J arrays:
  ! I = sky type;
  ! J = 1 for bare window, 2 and above for window with shade or blind.
  REAL(r64) :: FLFWSK(4,MaxSlatAngs+1)   ! Sky-related downgoing luminous flux
  REAL(r64) :: FLFWSU(MaxSlatAngs+1)     ! Sun-related downgoing luminous flux, excluding entering beam
  REAL(r64) :: FLFWSUdisk(MaxSlatAngs+1) ! Sun-related downgoing luminous flux, due to entering beam
  REAL(r64) :: FLCWSK(4,MaxSlatAngs+1)   ! Sky-related upgoing luminous flux
  REAL(r64) :: FLCWSU(MaxSlatAngs+1)     ! Sun-related upgoing luminous flux

  INTEGER   :: ISKY                      ! Sky type index: 1=clear, 2=clear turbid,
                                         !  3=intermediate, 4=overcast
  REAL(r64) :: TransMult(MaxSlatAngs)    ! Transmittance multiplier
  REAL(r64) :: TransBmBmMult(MaxSlatAngs)! Isolated blind beam-beam transmittance
  REAL(r64) :: DPH,DTH                   ! Sky/ground element altitude and azimuth increments (radians)
  INTEGER   :: IPH,ITH                   ! Sky/ground element altitude and azimuth indices
  REAL(r64) :: PH,TH                     ! Sky/ground element altitude and azimuth (radians)
  REAL(r64) :: SPH,CPH                   ! Sine and cosine of PH
  REAL(r64) :: PHMIN,PHMAX               ! Limits of altitude integration (radians)
  REAL(r64) :: THMIN,THMAX               ! Limits of azimuth integration (radians)
  REAL(r64) :: PhWin,ThWin               ! Altitude, azimuth angle of window normal (radians)
  REAL(r64) :: ACosTanTan                ! ACOS(-TAN(Ph)*TAN(PhWin))
  INTEGER   :: IConst                    ! Construction pointer
  REAL(r64) :: DA                        ! CPH*DTH*DPH
  REAL(r64) :: COSB                      ! Cosine of angle of incidence of light from sky or ground
  REAL(r64) :: TVISBR                    ! Transmittance of window without shading at COSB
                                         !  (times light well efficiency, if appropriate)
  REAL(r64) :: ZSK(4),ZSU                ! Sky-related and sun-related illuminance on window from sky/ground
                                         !  element for clear and overcast sky
  REAL(r64) :: U(3)                      ! Unit vector in (PH,TH) direction
  REAL(r64) :: ObTrans                   ! Product of solar transmittances of obstructions seen by a light ray
  REAL(r64), SAVE :: ObTransM(NTHMAX,NPHMAX)   ! ObTrans value for each (TH,PH) direction
!!unused  REAL(r64)         :: HitPointLumFrClearSky     ! Luminance of obstruction from clear sky (cd/m2)
!!unused  REAL(r64)         :: HitPointLumFrOvercSky     ! Luminance of obstruction from overcast sky (cd/m2)
!!unused  REAL(r64)         :: HitPointLumFrSun          ! Luminance of obstruction from sun (cd/m2)
  INTEGER   :: ICtrl                     ! Window control pointer
  INTEGER   :: IConstShaded              ! Pointer to shaded construction for a window
  INTEGER   :: JSH                       ! Shading index: JSH=1 is bare window, JSH=2 is shaded window
  REAL(r64) :: COSBSun                   ! Cosine of angle of incidence of direct sun on window
  REAL(r64) :: TVISBSun                  ! Window's visible transmittance at COSBSun
                                         !  (times light well efficiency, if appropriate)
  REAL(r64) :: ZSU1                      ! Transmitted direct normal illuminance (lux)
!  CHARACTER(len=30) :: ShType                    ! Window shading device type
  INTEGER   :: ShType                    ! Window shading device type
  LOGICAL   :: ShadeOn                   ! True if exterior or interior window shade present
  LOGICAL   :: BlindOn                   ! True if exterior or interior window blind present
  LOGICAL   :: ScreenOn                  ! True if exterior window screen present
  INTEGER   :: BlNum                     ! Blind number
  INTEGER   :: ScNum                     ! Screen number
  INTEGER   :: PipeNum                   ! TDD pipe object number
  INTEGER   :: ShelfNum                  ! Daylighting shelf object number
  INTEGER   :: InShelfSurf               ! Inside daylighting shelf surface number
  INTEGER   :: OutShelfSurf              ! Outside daylighting shelf surface number
  REAL(r64) :: TransBlBmDiffFront        ! Isolated blind vis beam-diffuse front transmittance
  REAL(r64) :: TransScBmDiffFront        ! Isolated screen vis beam-diffuse front transmittance
  REAL(r64) :: TransScDiffDiffFront      ! Isolated screen vis diffuse-diffuse front transmittance
  REAL(r64) :: ReflGlDiffDiffBack        ! Bare glazing system vis diffuse back reflectance
  REAL(r64) :: ReflGlDiffDiffFront       ! Bare glazing system vis diffuse front reflectance
  REAL(r64) :: ReflBlBmDiffFront         ! Isolated blind vis beam-diffuse front reflectance
  REAL(r64) :: TransBlDiffDiffFront      ! Isolated blind vis diffuse-diffuse front transmittance
  REAL(r64) :: ReflBlDiffDiffFront       ! Isolated blind vis diffuse-diffuse front reflectance
  REAL(r64) :: ReflBlDiffDiffBack        ! Isolated blind vis diffuse-diffuse back reflectance
  REAL(r64) :: ReflScDiffDiffBack        ! Isolated screen vis diffuse-diffuse back reflectance
  REAL(r64) :: ProfAng                   ! Solar profile angle (radians)
  REAL(r64) :: SlatAng                   ! Blind slat angle
  INTEGER   :: JB                        ! Blind slat angle index
  REAL(r64) :: t1,t2                     ! Beam-beam vis trans of bare glass layers 1 and 2
  REAL(r64) :: td2,td3                   ! Diffuse-diffuse vis trans of bare glass layers 2 and 3
  REAL(r64) :: rbd1,rbd2                 ! Beam-diffuse back vis reflectance of bare glass layers 1 and 2
  REAL(r64) :: rfd2,rfd3                 ! Beam-diffuse front vis reflectance of bare glass layers 2 and 3
  REAL(r64) :: tfshBd                    ! Beam-diffuse front vis trans of bare blind
  REAL(r64) :: rfshB                     ! Beam-diffuse front vis reflectance of bare blind
  REAL(r64) :: tfshd                     ! Diffuse-diffuse front vis trans of bare blind
  REAL(r64) :: rbshd                     ! Diffuse-diffuse back vis reflectance of bare blind
!!unused  REAL(r64)         :: A                         ! Intermediate value for azimuth limits calculation
  REAL(r64) :: ZSUObsRefl                ! Illuminance on window from beam solar reflected by an
                                         !  obstruction (for unit beam normal illuminance)
  INTEGER   :: NearestHitSurfNum         ! Surface number of nearest obstruction
  INTEGER   :: NearestHitSurfNumX        ! Surface number to use when obstruction is a shadowing surface
  REAL(r64) :: NearestHitPt(3)           ! Hit point of ray on nearest obstruction (m)
  REAL(r64) :: LumAtHitPtFrSun           ! Luminance at hit point on obstruction from solar reflection
                                         !  for unit beam normal illuminance (cd/m2)
  REAL(r64) :: SunObstructionMult        ! = 1 if sun hits a ground point; otherwise = 0
  REAL(r64), SAVE :: SkyObstructionMult(NTHMAX,NPHMAX) ! Ratio of obstructed to unobstructed sky diffuse at
                                                  ! a ground point for each (TH,PH) direction
  REAL(r64) :: Alfa,Beta            ! Direction angles for ray heading towards the ground (radians)
  REAL(r64) :: HorDis               ! Distance between ground hit point and proj'n of window center onto ground (m)
  REAL(r64) :: GroundHitPt(3)       ! Coordinates of point that ray from window center hits the ground (m)
  INTEGER   :: ObsSurfNum           ! Obstruction surface number
  INTEGER   :: IHitObs              ! = 1 if obstruction is hit, = 0 otherwise
  REAL(r64) :: ObsHitPt(3)          ! Coordinates of hit point on an obstruction (m)
  INTEGER   :: ObsConstrNum         ! Construction number of obstruction
  REAL(r64) :: ObsVisRefl           ! Visible reflectance of obstruction
  REAL(r64) :: SkyReflVisLum        ! Reflected sky luminance at hit point divided by unobstructed sky
                                    !  diffuse horizontal illuminance [(cd/m2)/lux]
  REAL(r64) :: dReflObsSky          ! Contribution to sky-related illuminance on window due to sky diffuse
                                    !  reflection from an obstruction
  REAL(r64) :: SkyGndUnObs          ! Unobstructed sky irradiance at a ground point
  REAL(r64) :: SkyGndObs            ! Obstructed sky irradiance at a ground point
  REAL(r64) :: Phi,Theta            ! Altitude and azimuth angle of ray from a ground point (radians)
  INTEGER   :: IPhi,ITheta          ! Phi and Theta indices
  REAL(r64) :: DPhi,DTheta          ! Phi and Theta increment (radians)
  REAL(r64) :: SPhi,CPhi            ! Sin and cos of Phi
  REAL(r64) :: dOmega               ! Solid angle element of ray from ground point (steradians)
  REAL(r64) :: URay(3)              ! Unit vector in (Phi,Theta) direction
  REAL(r64) :: CosIncAngURay            ! Cosine of incidence angle of URay on ground plane
  REAL(r64) :: IncAngSolidAngFac    ! CosIncAngURay*dOmega/Pi
  REAL(r64) :: TVisSunRefl          ! Diffuse vis trans of bare window for beam reflection calc
                                    !  (times light well efficiency, if appropriate)
  REAL(r64) :: ZSU1refl             ! Beam normal illuminance times ZSU1refl = illuminance on window
                                            !  due to specular reflection from exterior surfaces

  INTEGER   :: ZoneNumThisWin   ! temporary to check if this window is actually in adjacent zone
  INTEGER   :: ExtWinType                   ! Exterior window type (InZoneExtWin, AdjZoneExtWin, NotInOrAdjZoneExtWin)
  REAL(r64) :: ZoneInsideSurfArea ! temporary for calculations, total surface area of zone surfaces m2
  INTEGER   :: IntWinAdjZoneExtWinNum ! the index of the exterior window in IntWinAdjZoneExtWin nested struct
  INTEGER   :: AdjExtWinLoop  ! loop index for searching IntWinAdjZoneExtWin
  INTEGER   :: IntWinLoop ! loop index for searching interior windows
  INTEGER   :: IntWinNum ! window index for interior windows associated with exterior windows
  REAL(r64) :: COSBintWin

  ZoneNumThisWin = Surface(Surface(IWin)%BaseSurf)%Zone
  ! The inside surface area, ZoneDaylight(ZoneNum)%TotInsSurfArea was calculated in subr DayltgAveInteriorReflectance

  IF(ZoneNumThisWin == ZoneNum) THEN
    ExtWinType = InZoneExtWin
    ZoneInsideSurfArea = ZoneDaylight(ZoneNum)%TotInsSurfArea
    IntWinAdjZoneExtWinNum = 0
  ELSE
    ExtWinType = AdjZoneExtWin
    ! If window is exterior window in adjacent zone, then use areas of both zones
    ZoneInsideSurfArea = ZoneDaylight(ZoneNum)%TotInsSurfArea + ZoneDaylight(ZoneNumThisWin)%TotInsSurfArea
    ! find index in IntWinAdjZoneExtWin
    Do AdjExtWinLoop = 1, ZoneDaylight(ZoneNum)%NumOfIntWinAdjZoneExtWins
      IF (IWin == ZoneDaylight(ZoneNum)%IntWinAdjZoneExtWin(AdjExtWinLoop)%SurfNum) THEN ! found it
        IntWinAdjZoneExtWinNum = AdjExtWinLoop
        EXIT  ! added TH 4/13/2010
      ENDIF
    ENDDO
  END IF

         ! FLOW:
  ! Initialize window luminance and fluxes for split-flux calculation
  WLUMSK(:,:,IHR) = 0.
  WLUMSU(:,IHR) = 0.
  WLUMSUdisk(:,IHR) = 0.
  FLFWSK = 0.
  FLFWSU = 0.
  FLFWSUdisk = 0.
  FLCWSK = 0.
  FLCWSU = 0.

  IConst = Surface(IWin)%Construction
  IF(SurfaceWindow(IWin)%StormWinFlag==1) IConst = Surface(IWin)%StormWinConstruction
  BlindOn = .FALSE.
  ShadeOn = .FALSE.
  ScreenOn = .FALSE.

  IF (SurfaceWindow(IWin)%OriginalClass .EQ. SurfaceClass_TDD_Dome) THEN
    PipeNum = FindTDDPipe(IWin)
  END IF

  ShelfNum = Surface(IWin)%Shelf
  IF (ShelfNum > 0) THEN
    InShelfSurf = Shelf(ShelfNum)%InSurf ! Inside daylighting shelf present if > 0
    OutShelfSurf = Shelf(ShelfNum)%OutSurf ! Outside daylighting shelf present if > 0
  ELSE
    InShelfSurf = 0
    OutShelfSurf = 0
  END IF

  ! Divide sky and ground into elements of altitude PH and
  ! azimuth TH, and add the contribution of light coming from each
  ! element to the transmitted flux at the center of the window
  !
  ! Azimuth ranges over a maximum of 2 Pi radians.
  ! Altitude ranges over a maximum of Pi/2 radians between -Pi/2 < PH < +Pi/2, so that elements are not counted twice
  ! PH = 0 at the horizon; PH = Pi/2 at the zenith
  PHMIN = MAX(-PIOVR2, SurfaceWindow(IWin)%Phi - PIOVR2)
  PHMAX = MIN(PIOVR2, SurfaceWindow(IWin)%Phi + PIOVR2)
  DPH = (PHMAX - PHMIN) / REAL(NPHMAX,r64)

  ! Sky/ground element altitude integration
  DO IPH = 1,NPHMAX
    PH = PHMIN + (REAL(IPH,r64) - 0.5d0) * DPH

    SPH = SIN(PH)
    CPH = COS(PH)
    ! Third component of unit vector in (TH,PH) direction
    U(3) = SPH

    ! Limits of azimuth integration
    PhWin = SurfaceWindow(IWin)%Phi
    ThWin = SurfaceWindow(IWin)%Theta
    IF(PhWin >= 0.0) THEN
      IF(Ph >= PiOvr2 - PhWin) THEN
        ThMin = -Pi
        ThMax = Pi
      ELSE
        ACosTanTan = ACOS(-TAN(Ph)*TAN(PhWin))
        ThMin = ThWin - ABS(ACosTanTan)
        ThMax = ThWin + ABS(ACosTanTan)
      END IF

    ELSE  ! PhiSurf < 0.0
      IF(Ph <= -PhWin - PiOvr2) THEN
        ThMin = -Pi
        ThMax = Pi
      ELSE
        ACosTanTan = ACOS(-TAN(Ph)*TAN(PhWin))
        ThMin = ThWin - ABS(ACosTanTan)
        ThMax = ThWin + ABS(ACosTanTan)
      END IF
    END IF

    DTH = (THMAX - THMIN) / REAL(NTHMAX,r64)
    DA = CPH * DTH * DPH

    ! Sky/ground element azimuth integration
    DO ITH = 1,NTHMAX
      TH = THMIN + (REAL(ITH,r64) - 0.5d0) * DTH
      U(1) = CPH * COS(TH)
      U(2) = CPH * SIN(TH)
      ! Cosine of angle of incidence of light from sky or ground element
      COSB = SPH * SIN(SurfaceWindow(IWin)%Phi) + CPH * COS(SurfaceWindow(IWin)%Phi) &
                 * COS(TH-SurfaceWindow(IWin)%Theta)
      IF (COSB < 0.0) CYCLE ! Sky/ground elements behind window (although there shouldn't be any)

      ! Initialize illuminance on window for this sky/ground element
      ZSK = 0.
      ZSU = 0.
      ! Initialize illuminance on window from beam solar reflection if ray hits an obstruction
      ZSUObsRefl = 0.

      IF (ISunPos == 1) THEN ! Intersection calculation has to be done only for first sun position
        ! Determine net transmittance of obstructions that the ray hits. ObTrans will be 1.0
        ! if no obstructions are hit.
        CALL DayltgHitObstruction(IHr,IWin,SurfaceWindow(IWin)%WinCenter,U,ObTrans)
        ObTransM(ITH,IPH) = ObTrans
      END IF

      ! SKY AND GROUND RADIATION ON WINDOW

      ! Contribution is from sky if PH > 0 (ray goes upward), and from ground if PH < 0 (ray goes downward)
      ! (There may also be contributions from reflection from obstructions; see 'BEAM SOLAR AND SKY SOLAR
      ! REFLECTED FROM NEAREST OBSTRUCTION,' below.)

      IF(ISunPos == 1) SkyObstructionMult(ITH,IPH) = 1.0
      IF (PH > 0.0) THEN ! Contribution is from sky
        DO ISKY = 1,4
          ZSK(ISKY) = DayltgSkyLuminance(ISky,TH,PH) * COSB * DA * ObTransM(ITH,IPH)
        END DO
      ELSE               ! PH <= 0.0; contribution is from ground
        IF(CalcSolRefl .AND. ObTransM(ITH,IPH) > 1.d-6 .AND. ISunPos == 1) THEN
          ! Calculate effect of obstructions on shading of sky diffuse reaching the ground point hit
          ! by the ray. This effect is given by the ratio SkyObstructionMult =
          ! (obstructed sky diffuse at ground point)/(unobstructed sky diffuse at ground point).
          ! This ratio is calculated for an isotropic sky.
          ! Ground point hit by the ray:
          Alfa = ACOS(-U(3))
          Beta = ATAN2(U(2),U(1))
          HorDis = (SurfaceWindow(IWin)%WinCenter(3)-GroundLevelZ)*TAN(Alfa)
          GroundHitPt(3) = GroundLevelZ
          GroundHitPt(1) = SurfaceWindow(IWin)%WinCenter(1) + HorDis*COS(Beta)
          GroundHitPt(2) = SurfaceWindow(IWin)%WinCenter(2) + HorDis*SIN(Beta)
          ! Send rays upward from hit point and see which ones are unobstructed and so go to sky.
          ! Divide hemisphere centered at ground hit point into elements of altitude Phi and
          ! azimuth Theta and create upward-going ray unit vector at each Phi,Theta pair.
          ! Phi = 0 at the horizon; Phi = Pi/2 at the zenith.
          DPhi = PiOvr2 / (AltAngStepsForSolReflCalc/2.d0)
          DTheta = 2.d0*Pi / (2.d0*AzimAngStepsForSolReflCalc)
          SkyGndObs = 0.0
          SkyGndUnObs = 0.0
          ! Altitude loop
          DO IPhi = 1,(AltAngStepsForSolReflCalc/2)
            Phi = (IPhi - 0.5d0) * DPhi
            SPhi = SIN(Phi)
            CPhi = COS(Phi)
            ! Third component of ray unit vector in (Theta,Phi) direction
            URay(3) = SPhi
            dOmega = CPhi * DTheta * DPhi
            ! Cosine of angle of incidence of ray on ground
            CosIncAngURay = SPhi
            IncAngSolidAngFac = CosIncAngURay*dOmega/Pi
            ! Azimuth loop
            DO ITheta = 1,2*AzimAngStepsForSolReflCalc
              Theta = (ITheta - 0.5d0) * DTheta
              URay(1) = CPhi * COS(Theta)
              URay(2) = CPhi * SIN(Theta)
              SkyGndUnObs = SkyGndUnObs + IncAngSolidAngFac
              ! Does this ray hit an obstruction?
              IHitObs = 0
              DO ObsSurfNum = 1, TotSurfaces
                IF(.NOT.Surface(ObsSurfNum)%ShadowSurfPossibleObstruction) CYCLE
                CALL DayltgPierceSurface(ObsSurfNum,GroundHitPt,URay,IHitObs,ObsHitPt)
                IF(IHitObs > 0) EXIT
              END DO
              IF(IHitObs > 0) CYCLE ! Obstruction hit
              ! Sky is hit
              SkyGndObs = SkyGndObs + IncAngSolidAngFac
            END DO ! End of azimuth loop
          END DO  ! End of altitude loop
          SkyObstructionMult(ITH,IPH) = SkyGndObs / (SkyGndUnObs + 1.d-8)
        END IF  ! End of check if solar reflection calc is in effect
        DO ISKY = 1,4
          ! Below, luminance of ground in cd/m2 is illuminance on ground in lumens/m2
          ! times ground reflectance, divided by pi, times obstruction multiplier.
          ZSK(ISKY) = (GILSK(ISKY,IHR) * GndReflectanceForDayltg / PI) * COSB * DA * ObTransM(ITH,IPH) *  &
                           SkyObstructionMult(ITH,IPH)
        END DO
        ! Determine if sun illuminates the point that ray hits the ground. If the solar reflection
        ! calculation has been requested (CalcSolRefl = .TRUE.) shading by obstructions, including
        ! the building itself, is considered in determining whether sun hits the ground point.
        ! Otherwise this shading is ignored and the sun always hits the ground point.
        SunObstructionMult = 1.0
        IF(CalcSolRefl .AND. ObTransM(ITH,IPH) > 1.d-6) THEN
          ! Sun reaches ground point if vector from this point to the sun is unobstructed
          IHitObs = 0
          DO ObsSurfNum = 1,TotSurfaces
            IF(.NOT.Surface(ObsSurfNum)%ShadowSurfPossibleObstruction) CYCLE
            CALL DayltgPierceSurface(ObsSurfNum,GroundHitPt,SunCosHr(1:3,IHr),IHitObs,ObsHitPt)
            IF(IHitObs > 0) EXIT
          END DO
          IF(IHitObs > 0) SunObstructionMult = 0.0
        END IF
        ZSU = (GILSU(IHR) * GndReflectanceForDayltg / PI) * COSB * DA * ObTransM(ITH,IPH) * &
                SunObstructionMult
      END IF

      ! BEAM SOLAR AND SKY SOLAR REFLECTED FROM NEAREST OBSTRUCTION

      IF(CalcSolRefl .AND. ObTransM(ITH,IPH) < 1.0) THEN
        ! Find obstruction whose hit point is closest to the center of the window
        CALL DayltgClosestObstruction(SurfaceWindow(IWin)%WinCenter,U,NearestHitSurfNum,NearestHitPt)
        IF(NearestHitSurfNum > 0) THEN

          ! Beam solar reflected from nearest obstruction.
          CALL DayltgSurfaceLumFromSun(IHr,U,NearestHitSurfNum,NearestHitPt,LumAtHitPtFrSun)
          ZSUObsRefl = LumAtHitPtFrSun * COSB * DA
          ZSU = ZSU + ZSUObsRefl

          ! Sky solar reflected from nearest obstruction.
          ObsConstrNum = Surface(NearestHitSurfNum)%Construction
          IF(ObsConstrNum > 0) THEN
            ! Exterior building surface is nearest hit
            IF(.NOT.Construct(ObsConstrNum)%TypeIsWindow) THEN
              ! Obstruction is not a window, i.e., is an opaque surface
              ObsVisRefl = 1.0 - Material(Construct(ObsConstrNum)%LayerPoint(1))%AbsorpVisible
            ELSE
              ! Obstruction is a window; assume it is bare
              IF(SurfaceWindow(NearestHitSurfNum)%StormWinFlag==1) &
                ObsConstrNum = Surface(NearestHitSurfNum)%StormWinConstruction
              ObsVisRefl = Construct(ObsConstrNum)%ReflectVisDiffFront
            END IF
          ELSE
            ! Shadowing surface is nearest hit
            IF(Surface(NearestHitSurfNum)%Shelf > 0) THEN
              ! Skip daylighting shelves, whose reflection is separately calculated
              ObsVisRefl = 0.0
            ELSE
              ObsVisRefl = Surface(NearestHitSurfNum)%ShadowSurfDiffuseVisRefl
              IF(Surface(NearestHitSurfNum)%ShadowSurfGlazingConstruct > 0) &
                 ObsVisRefl = ObsVisRefl + Surface(NearestHitSurfNum)%ShadowSurfGlazingFrac *  &
                  Construct(Surface(NearestHitSurfNum)%ShadowSurfGlazingConstruct)%ReflectVisDiffFront
              ! Note in the above that ShadowSurfDiffuseVisRefl is the reflectance of opaque part of
              ! shadowing surface times (1 - ShadowSurfGlazingFrac)
            END IF
          END IF
          NearestHitSurfNumX = NearestHitSurfNum
          ! Each shadowing surface has a "mirror" duplicate surface facing in the opposite direction.
          ! The following gets the correct side of a shadowing surface for reflection.
          IF(Surface(NearestHitSurfNum)%ShadowingSurf) THEN
            IF(DOT_PRODUCT(U,Surface(NearestHitSurfNum)%OutNormVec) > 0.0)  NearestHitSurfNumX = &
                      NearestHitSurfNum + 1
          END IF
          IF (.not. DetailedSkyDiffuseAlgorithm .or. .not.  ShadingTransmittanceVaries .or.  &
              SolarDistribution == MinimalShadowing) THEN
            SkyReflVisLum = ObsVisRefl * Surface(NearestHitSurfNumX)%ViewFactorSky *  &
                               DifShdgRatioIsoSky(NearestHitSurfNumX) / Pi
          ELSE
            SkyReflVisLum = ObsVisRefl * Surface(NearestHitSurfNumX)%ViewFactorSky *  &
                               DifShdgRatioIsoSkyHRTS(NearestHitSurfNumX,IHR,1) / Pi
          ENDIF
          dReflObsSky = SkyReflVisLum * COSB * DA
          DO ISky = 1,4
            ZSK(ISky) = ZSK(ISky) + GILSK(ISky,IHr) * dReflObsSky
          END DO
        END IF
      END IF  ! End of check if exterior solar reflection calculation is active

      !  ===Bare window (no shade or blind; non-diffusing glass)===

      ! Increment flux entering space and window luminance (cd/m2).
      ! FLCW--(I,J) = part of incoming flux (in lumens) that goes up to ceiling and upper part of walls.
      ! FLFW--(I,J) = part that goes down to floor and lower part of walls

      IF (SurfaceWindow(IWIN)%OriginalClass .EQ. SurfaceClass_TDD_Dome) THEN
        ! Unshaded visible transmittance of TDD for a single ray from sky/ground element
        TVISBR = TransTDD(PipeNum, COSB, VisibleBeam) * SurfaceWindow(IWin)%GlazedFrac

        ! Make all transmitted light diffuse for a TDD with a bare diffuser
        DO ISKY = 1,4
          WLUMSK(ISKY,1,IHR) = WLUMSK(ISKY,1,IHR) + ZSK(ISKY) * TVISBR / PI
          FLFWSK(ISKY,1) = FLFWSK(ISKY,1) + ZSK(ISKY) * TVISBR * (1.0 - SurfaceWindow(IWin)%FractionUpgoing)
          FLCWSK(ISKY,1) = FLCWSK(ISKY,1) + ZSK(ISKY) * TVISBR * SurfaceWindow(IWin)%FractionUpgoing

          ! For later calculation of diffuse visible transmittance
          TDDFluxInc(PipeNum,ISKY,IHR) = TDDFluxInc(PipeNum,ISKY,IHR) + ZSK(ISKY)
          TDDFluxTrans(PipeNum,ISKY,IHR) = TDDFluxTrans(PipeNum,ISKY,IHR) + ZSK(ISKY) * TVISBR

          IF(ISky == 1) THEN
            WLUMSU(1,IHR) = WLUMSU(1,IHR) + ZSU * TVISBR / PI
            FLFWSU(1) = FLFWSU(1) + ZSU * TVISBR * (1.0d0 - SurfaceWindow(IWin)%FractionUpgoing)
            FLCWSU(1) = FLCWSU(1) + ZSU * TVISBR * SurfaceWindow(IWin)%FractionUpgoing

            ! For later calculation of diffuse visible transmittance
            TDDFluxInc(PipeNum,ISKY,IHR) = TDDFluxInc(PipeNum,ISKY,IHR) + ZSU
            TDDFluxTrans(PipeNum,ISKY,IHR) = TDDFluxTrans(PipeNum,ISKY,IHR) + ZSU * TVISBR
          END IF
        END DO

      ELSE ! Bare window

        ! Transmittance of bare window for this sky/ground element
        TVISBR = POLYF(COSB,Construct(IConst)%TransVisBeamCoef(1:6)) * SurfaceWindow(IWin)%GlazedFrac *  &
                    SurfaceWindow(IWin)%LightWellEff

        IF (InShelfSurf > 0) THEN ! Inside daylighting shelf
          ! Daylighting shelf simplification:  All light is diffuse
          ! SurfaceWindow(IWin)%FractionUpgoing is already set to 1.0 earlier
          DO ISKY = 1,4
            FLCWSK(ISKY,1) = FLCWSK(ISKY,1) + ZSK(ISKY) * TVISBR * SurfaceWindow(IWin)%FractionUpgoing

            IF(ISky == 1) THEN
              FLCWSU(1) = FLCWSU(1) + ZSU * TVISBR * SurfaceWindow(IWin)%FractionUpgoing
            END IF
          END DO

        ELSE ! Normal window

          ! CR 7869  correct TVISBR if disk beam passes thru interior window
          IF (ExtWinType ==AdjZoneExtWin) THEN
            ! modify TVISBR by second window transmission
            ! first determine if ray from point passes thru any interior window
            IHitObs = 0
            DO IntWinLoop = 1, ZoneDaylight(ZoneNum)%IntWinAdjZoneExtWin(IntWinAdjZoneExtWinNum)%NumOfIntWindows
              IntWinNum = ZoneDaylight(ZoneNum)%IntWinAdjZoneExtWin(IntWinAdjZoneExtWinNum)%IntWinNum(IntWinLoop)
              Call DayltgPierceSurface(IntWinNum, SurfaceWindow(IntWinNum)%WinCenter,SunCosHr(1:3,IHr),IHitObs, ObsHitPt)
              IF (IHitObs == 1) THEN ! disk passes thru
                ! cosine of incidence angle of light from sky or ground element for
                COSBintWin = SPH * SIN(SurfaceWindow(IntWinNum)%Phi) + CPH * COS(SurfaceWindow(IntWinNum)%Phi) &
                                   * COS(TH-SurfaceWindow(IntWinNum)%Theta)
                TVISBR = TVISBR * POLYF(COSBintWin,Construct(Surface(IntWinNum)%Construction)%TransVisBeamCoef(1:6))
                EXIT
              ENDIF
            ENDDO
            IF (IHitObs == 0) THen ! blocked by opaque parts, beam does not actually pass thru interior window to reach zone
              TVISBR = 0.0D0
            ENDIF
          ENDIF
          DO ISKY = 1,4
            !IF (PH < 0.) THEN
            !Fixed by FCW, Nov. 2003:
            IF (PH > 0.) THEN
              FLFWSK(ISKY,1) = FLFWSK(ISKY,1) + ZSK(ISKY) * TVISBR
              IF (ISky == 1) FLFWSU(1) = FLFWSU(1) + ZSU * TVISBR
            ELSE
              FLCWSK(ISKY,1) = FLCWSK(ISKY,1) + ZSK(ISKY) * TVISBR
              IF (ISky == 1) FLCWSU(1) = FLCWSU(1) + ZSU * TVISBR
            END IF

          END DO
        END IF ! End of check if window with daylighting shelf or normal window
      END IF ! End of check if TDD:DOME or bare window

      ! Check if window has shade or blind
      ICtrl = Surface(IWin)%WindowShadingControlPtr
      IF (ICtrl > 0) THEN
        ShType = WindowShadingControl(ICtrl)%ShadingType
        BlNum  = SurfaceWindow(IWin)%BlindNumber
        ScNum  = SurfaceWindow(IWin)%ScreenNumber

        ShadeOn = (ShType == WSC_ST_InteriorShade .OR. ShType == WSC_ST_ExteriorShade.OR. ShType == WSC_ST_BetweenGlassShade)
        BlindOn = (ShType == WSC_ST_InteriorBlind .OR. ShType == WSC_ST_ExteriorBlind.OR. ShType == WSC_ST_BetweenGlassBlind)
        ScreenOn = (ShType == WSC_ST_ExteriorScreen)
      END IF

      IF (ShadeOn .OR. BlindOn .OR. ScreenOn .OR. SurfaceWindow(IWin)%SolarDiffusing) THEN

        ! ===Window with interior or exterior shade or blind, exterior screen, or with diffusing glass===

        ! Increment flux entering space and window luminance. Shades and diffusing glass are
        ! assumed to be perfect diffusers, i.e., the transmittance is independent of angle of
        ! incidence and the transmitted light is isotropic. The transmittance of a blind is
        ! assumed to depend on profile angle and slat angle; the diffuse light entering the room from
        ! the slats of the blind is assumed to be isotropic. With blinds, light can also enter
        ! the room by passing between the slats without reflection. The beam transmittance of a screen
        ! is assumed to depend on sun azimuth and azimuth angle.

        ! For light from a shade, or from diffusing glass, or from the slats of a blind, a flux fraction,
        ! SurfaceWindow(IWin)%FractionUpgoing (determined by window tilt), goes up toward
        ! ceiling and upper part of walls, and 1-Surfacewindow(iwin)%FractionUpgoing
        ! goes down toward floor and lower part of walls. For a blind, the light passing
        ! between the slats goes either up or down depending on the altitude angle of the
        ! element from which the light came. For a screen, the light passing
        ! between the screen's cylinders goes either up or down depending on the altitude angle of the
        ! element from which the light came.

        IConstShaded = Surface(IWin)%ShadedConstruction
        IF(SurfaceWindow(IWin)%StormWinFlag==1) IConstShaded = Surface(IWin)%StormWinShadedConstruction
        IF(SurfaceWindow(IWin)%SolarDiffusing) IConstShaded = Surface(IWin)%Construction

        ! Transmittance of window including shade, screen or blind
        TransBmBmMult = 0.0
        TransMult = 0.0

        IF (ShadeOn) THEN ! Shade
          IF (SurfaceWindow(IWin)%OriginalClass .EQ. SurfaceClass_TDD_Dome) THEN
            ! Shaded visible transmittance of TDD for a single ray from sky/ground element
            TransMult(1) = TransTDD(PipeNum, COSB, VisibleBeam) * SurfaceWindow(IWin)%GlazedFrac
          ELSE ! Shade only, no TDD
            ! Calculate transmittance of the combined window and shading device for this sky/ground element
            TransMult(1) = POLYF(COSB,Construct(IConstShaded)%TransVisBeamCoef(1)) * SurfaceWindow(IWin)%GlazedFrac * &
                              SurfaceWindow(IWin)%LightWellEff
          END IF

        ELSE IF(ScreenOn) THEN ! Screen: get beam-beam, beam-diffuse and diffuse-diffuse vis trans/ref of screen and glazing system
            CALL CalcScreenTransmittance(IWin, Phi=(PH-SurfaceWindow(IWin)%Phi), Theta=(TH-SurfaceWindow(IWin)%Theta))
            ReflGlDiffDiffFront = Construct(IConst)%ReflectVisDiffFront
            ReflScDiffDiffBack = SurfaceScreens(SurfaceWindow(IWin)%ScreenNumber)%DifReflectVis
            TransScBmDiffFront = SurfaceScreens(SurfaceWindow(IWin)%ScreenNumber)%BmDifTransVis
            TransMult(1) = TransScBmDiffFront * SurfaceWindow(IWin)%GlazedFrac * &
                            Construct(IConst)%TransDiffVis/(1-ReflGlDiffDiffFront*ReflScDiffDiffBack) * &
                              SurfaceWindow(IWin)%LightWellEff
            TransBmBmMult(1) = SurfaceScreens(SurfaceWindow(IWin)%ScreenNumber)%BmBmTransVis

        ELSE IF(BlindOn) THEN ! Blind: get beam-diffuse and beam-beam vis trans of blind+glazing system
          ! PETER:  As long as only interior blinds are allowed for TDDs, no need to change TransMult calculation
          !         for TDDs because it is based on TVISBR which is correctly calculated for TDDs above.

          CALL ProfileAngle(IWin,U,Blind(BlNum)%SlatOrientation,ProfAng)

          DO JB = 1, MaxSlatAngs
            IF (.NOT. SurfaceWindow(IWin)%MovableSlats .AND. JB > 1) EXIT

            TransBlBmDiffFront = InterpProfAng(ProfAng,Blind(BlNum)%VisFrontBeamDiffTrans(1:37,JB))

            IF (ShType == WSC_ST_InteriorBlind) THEN ! Interior blind
              ReflGlDiffDiffBack = Construct(IConst)%ReflectVisDiffBack
              ReflBlBmDiffFront  = InterpProfAng(ProfAng,Blind(BlNum)%VisFrontBeamDiffRefl(1:37,JB))
              ReflBlDiffDiffFront = Blind(BlNum)%VisFrontDiffDiffRefl(JB)
              TransBlDiffDiffFront = Blind(BlNum)%VisFrontDiffDiffTrans(JB)
              TransMult(JB) = TVISBR*(TransBlBmDiffFront + ReflBlBmDiffFront*ReflGlDiffDiffBack*TransBlDiffDiffFront/ &
                              (1.d0 - ReflBlDiffDiffFront*ReflGlDiffDiffBack))

            ELSE IF (ShType == WSC_ST_ExteriorBlind) THEN     ! Exterior blind
              ReflGlDiffDiffFront = Construct(IConst)%ReflectVisDiffFront
              ReflBlDiffDiffBack = Blind(BlNum)%VisBackDiffDiffRefl(JB)
              TransMult(JB) = TransBlBmDiffFront * SurfaceWindow(IWin)%GlazedFrac * &
                               Construct(IConst)%TransDiffVis/(1.d0-ReflGlDiffDiffFront*ReflBlDiffDiffBack) * &
                               SurfaceWindow(IWin)%LightWellEff

            ELSE                                        ! Between-glass blind
              t1     = POLYF(COSB,Construct(IConst)%tBareVisCoef(1,1:6))
              td2    = Construct(IConst)%tBareVisDiff(2)
              rbd1   = Construct(IConst)%rbBareVisDiff(1)
              rfd2   = Construct(IConst)%rfBareVisDiff(2)
              tfshBd = InterpProfAng(ProfAng,Blind(BlNum)%VisFrontBeamDiffTrans(1:37,JB))
              tfshd  = Blind(BlNum)%VisFrontDiffDiffTrans(JB)
              rfshB  = InterpProfAng(ProfAng,Blind(BlNum)%VisFrontBeamDiffRefl(1:37,JB))
              rbshd  = Blind(BlNum)%VisFrontDiffDiffRefl(JB)
              IF (Construct(IConst)%TotGlassLayers == 2) THEN  ! 2 glass layers
                TransMult(JB) = t1*(tfshBd*(1.d0 + rfd2*rbshd) + rfshB*rbd1*tfshd)*td2 * SurfaceWindow(IWin)%LightWellEff
              ELSE                                             ! 3 glass layers; blind between layers 2 and 3
                t2   = POLYF(COSB,Construct(IConst)%tBareVisCoef(2,1:6))
                td3  = Construct(IConst)%tBareVisDiff(3)
                rfd3 = Construct(IConst)%rfBareVisDiff(3)
                rbd2 = Construct(IConst)%rbBareVisDiff(2)
                TransMult(JB) = t1*t2*(tfshBd*(1.d0 + rfd3*rbshd) + rfshB*(rbd2*tfshd + td2*rbd1*td2*tfshd))*td3 *  &
                                   SurfaceWindow(IWin)%LightWellEff
              END IF
            END IF

            IF (SurfaceWindow(IWin)%MovableSlats) THEN
              SlatAng = (JB-1)*PI/(MaxSlatAngs-1)
            ELSE
              SlatAng = Blind(BlNum)%SlatAngle * DegToRadians
            END IF
            TransBmBmMult(JB) = TVISBR * BlindBeamBeamTrans(ProfAng,SlatAng,Blind(BlNum)%SlatWidth, &
                                  Blind(BlNum)%SlatSeparation,Blind(BlNum)%SlatThickness)
          END DO ! End of loop over slat angles

        ELSE  ! Diffusing glass
          TransMult(1) = POLYF(COSB,Construct(IConstShaded)%TransVisBeamCoef(1)) * SurfaceWindow(IWin)%GlazedFrac * &
                              SurfaceWindow(IWin)%LightWellEff
        END IF ! End of check if shade, blind or diffusing glass

        IF (SurfaceWindow(IWin)%OriginalClass .EQ. SurfaceClass_TDD_Dome) THEN
          ! No beam is transmitted.  This takes care of all types of screens and blinds.
          TransBmBmMult = 0.0
        END IF

        ! Daylighting shelf simplification:  No beam makes it past end of shelf, all light is diffuse
        IF (InShelfSurf > 0) THEN ! Inside daylighting shelf
          TransBmBmMult = 0.0 ! No beam, diffuse only
        END IF

        ! TransBmBmMult is used in the following for windows with blinds or screens to get contribution from light
        ! passing directly between slats or between screen material without reflection.

        DO ISKY=1,4
          DO JB = 1, MaxSlatAngs
            ! EXIT after first pass if not movable slats or exterior window screen
            IF (.NOT.SurfaceWindow(IWin)%MovableSlats .AND. JB > 1) EXIT

            WLUMSK(ISKY,JB+1,IHR) = WLUMSK(ISKY,JB+1,IHR) + ZSK(ISKY) * TransMult(JB) / PI
            FLFWSK(ISKY,JB+1) = FLFWSK(ISKY,JB+1) + ZSK(ISKY) * TransMult(JB) * (1.-SurfaceWindow(IWin)%FractionUpgoing)
            IF (PH > 0.0 .AND. (BlindOn .OR. ScreenOn)) FLFWSK(ISKY,JB+1) = FLFWSK(ISKY,JB+1) + ZSK(ISKY) * TransBmBmMult(JB)
            FLCWSK(ISKY,JB+1) = FLCWSK(ISKY,JB+1) + ZSK(ISKY) * TransMult(JB) * SurfaceWindow(IWin)%FractionUpgoing
            IF (PH <= 0.0 .AND. (BlindOn .OR. ScreenOn)) FLCWSK(ISKY,JB+1) = FLCWSK(ISKY,JB+1) + ZSK(ISKY) * TransBmBmMult(JB)
            IF (ISky == 1) THEN
              WLUMSU(JB+1,IHR) = WLUMSU(JB+1,IHR) + ZSU * TransMult(JB) / PI
              FLFWSU(JB+1) = FLFWSU(JB+1) + ZSU * TransMult(JB) * (1.-SurfaceWindow(IWin)%FractionUpgoing)
              IF (PH > 0.0 .AND. (BlindOn .OR. ScreenOn)) FLFWSU(JB+1) = FLFWSU(JB+1) + ZSU * TransBmBmMult(JB)
              FLCWSU(JB+1) = FLCWSU(JB+1) + ZSU * TransMult(JB) * SurfaceWindow(IWin)%FractionUpgoing
              IF (PH <= 0.0 .AND. (BlindOn .OR. ScreenOn)) FLCWSU(JB+1) = FLCWSU(JB+1) + ZSU * TransBmBmMult(JB)
            END IF
          END DO
        END DO
      END IF ! End of window with shade, screen, blind or diffusing glass

    END DO ! End of azimuth integration loop, ITH
  END DO ! End of altitude integration loop, IPH

  IF (OutShelfSurf > 0) THEN ! Outside daylighting shelf
    ! Add exterior diffuse illuminance due to outside shelf
    ! Since all of the illuminance is added to the zone as upgoing diffuse, it can be added as a lump sum here

    TVISBR = Construct(IConst)%TransDiffVis ! Assume diffuse transmittance for shelf illuminance

    DO ISKY = 1,4
      ! This is only an estimate because the anisotropic sky view of the shelf is not yet taken into account.
      ! AnisoSkyMult would be great to use but it is not available until the heat balance starts up.
      ZSK(ISKY) = GILSK(ISKY,IHR) * 1.0 * Shelf(ShelfNum)%OutReflectVis * Shelf(ShelfNum)%ViewFactor

      ! SurfaceWindow(IWin)%FractionUpgoing is already set to 1.0 earlier
      FLCWSK(ISKY,1) = FLCWSK(ISKY,1) + ZSK(ISKY) * TVISBR * SurfaceWindow(IWin)%FractionUpgoing

      IF(ISky == 1) THEN
        ZSU = GILSU(IHR) * SunLitFracHR(OutShelfSurf,IHR) * Shelf(ShelfNum)%OutReflectVis * Shelf(ShelfNum)%ViewFactor
        FLCWSU(1) = FLCWSU(1) + ZSU * TVISBR * SurfaceWindow(IWin)%FractionUpgoing
      END IF
    END DO ! ISKY
  END IF

  ! Sky-related portion of internally reflected illuminance.
  ! The inside surface area, ZoneDaylight(ZoneNum)%TotInsSurfArea, and ZoneDaylight(ZoneNum)%AveVisDiffReflect,
  ! were calculated in subr DayltgAveInteriorReflectance.

  DO ISKY = 1,4
    DO JSH = 1,MaxSlatAngs+1
      IF (.NOT.SurfaceWindow(IWin)%MovableSlats .AND. JSH > 2) EXIT
      ! Full area of window is used in following since effect of dividers on reducing
      ! effective window transmittance has already been accounted for in calc of FLFWSK and FLCWSK.
      EINTSK(ISKY,JSH,IHR) = (FLFWSK(ISKY,JSH) * SurfaceWindow(IWin)%RhoFloorWall &
                             + FLCWSK(ISKY,JSH) * SurfaceWindow(IWin)%RhoCeilingWall) &
                             * (Surface(IWin)%Area/SurfaceWindow(IWin)%GlazedFrac) &
                             / (ZoneInsideSurfArea * (1.0d0 - ZoneDaylight(ZoneNum)%AveVisDiffReflect))
    END DO ! JSH
  END DO ! ISKY

  ! BEAM SOLAR RADIATION ON WINDOW

  ! Beam reaching window directly (without specular reflection from exterior obstructions)

  IF (SunLitFracHR(IWin,IHR) > 0.0) THEN
    ! Cos of angle of incidence
    COSBSun = SPHSUN * SIN(SurfaceWindow(IWin)%Phi) + CPHSUN * COS(SurfaceWindow(IWin)%Phi) &
              * COS(THSUN - SurfaceWindow(IWin)%Theta)

    IF (COSBSun > 0.) THEN
      ! Multiply direct normal illuminance (normalized to 1.0 lux)
      ! by incident angle factor and by fraction of window that is sunlit.
      ! Note that in the following SunLitFracHR accounts for possibly non-zero transmittance of
      ! shading surfaces.

      ZSU1 = COSBSun * SunLitFracHR(IWin,IHR)

      ! Contribution to window luminance and downgoing flux

      ! -- Bare window

      IF (SurfaceWindow(IWin)%OriginalClass .EQ. SurfaceClass_TDD_Dome) THEN
        ! Unshaded visible transmittance of TDD for collimated beam from the sun
        TVISBSun = TransTDD(PipeNum, COSBSun, VisibleBeam) * SurfaceWindow(IWin)%GlazedFrac
        TDDTransVisBeam(PipeNum, IHR) = TVISBSun

        FLFWSUdisk(1) = 0.0 ! Diffuse light only

        WLUMSU(1,IHR) = WLUMSU(1,IHR) + ZSU1 * TVISBSun / PI
        FLFWSU(1) = FLFWSU(1) + ZSU1 * TVISBSun * (1.0d0 - SurfaceWindow(IWin)%FractionUpgoing)
        FLCWSU(1) = FLCWSU(1) + ZSU1 * TVISBSun * SurfaceWindow(IWin)%FractionUpgoing

      ELSE ! Bare window
        TVISBSun = POLYF(COSBSun,Construct(IConst)%TransVisBeamCoef(1)) * SurfaceWindow(IWin)%GlazedFrac * &
                      SurfaceWindow(IWin)%LightWellEff

        ! Daylighting shelf simplification:  No beam makes it past end of shelf, all light is diffuse
        IF (InShelfSurf > 0) THEN ! Inside daylighting shelf
          FLFWSUdisk(1) = 0.0 ! Diffuse light only

          ! SurfaceWindow(IWin)%FractionUpgoing is already set to 1.0 earlier
          !WLUMSU(1,IHR) = WLUMSU(1,IHR) + ZSU1 * TVISBSun / PI
          !FLFWSU(1) = FLFWSU(1) + ZSU1 * TVISBSun * (1.0 - SurfaceWindow(IWin)%FractionUpgoing)
          FLCWSU(1) = FLCWSU(1) + ZSU1 * TVISBSun * SurfaceWindow(IWin)%FractionUpgoing
        ELSE ! Normal window
          FLFWSUdisk(1) = ZSU1 * TVISBSun
        END If
      END IF

      ! -- Window with shade, screen, blind or diffusing glass
      IF (ShadeOn .OR. BlindOn .OR. ScreenOn .OR. SurfaceWindow(IWin)%SolarDiffusing) THEN
        TransBmBmMult = 0.0
        TransMult = 0.0

        ! TH 7/7/2010 moved from inside the loop: DO JB = 1,MaxSlatAngs
        IF (BlindOn) CALL ProfileAngle(IWin,SUNCOSHR(1:3,IHR),Blind(BlNum)%SlatOrientation,ProfAng)

        DO JB = 1,MaxSlatAngs
          IF (.NOT.SurfaceWindow(IWin)%MovableSlats .AND. JB > 1) EXIT

          IF (ShadeOn .OR. ScreenOn .OR. SurfaceWindow(IWin)%SolarDiffusing) THEN ! Shade or screen on or diffusing glass
            IF (SurfaceWindow(IWin)%OriginalClass .EQ. SurfaceClass_TDD_Dome) THEN
              ! Shaded visible transmittance of TDD for collimated beam from the sun
              TransMult(1) = TransTDD(PipeNum, COSBSun, VisibleBeam) * SurfaceWindow(IWin)%GlazedFrac
            ELSE
              IF(ScreenOn)THEN
                TransMult(1) = SurfaceScreens(SurfaceWindow(IWin)%ScreenNumber)%BmBmTransVis * SurfaceWindow(IWin)%GlazedFrac * &
                                SurfaceWindow(IWin)%LightWellEff
              ELSE
                TransMult(1) = POLYF(COSBSun,Construct(IConstShaded)%TransVisBeamCoef(1)) * SurfaceWindow(IWin)%GlazedFrac * &
                                SurfaceWindow(IWin)%LightWellEff
              END IF
            END IF

          ELSE ! Blind on

            ! PETER:  As long as only interior blinds are allowed for TDDs, no need to change TransMult calculation
            !         for TDDs because it is based on TVISBSun which is correctly calculated for TDDs above.

            ! TH 7/7/2010: This call is moved outside the loop - DO JB = 1,MaxSlatAngs
            !CALL ProfileAngle(IWin,SUNCOSHR(1:3,IHR),Blind(BlNum)%SlatOrientation,ProfAng)

            TransBlBmDiffFront = InterpProfAng(ProfAng,Blind(BlNum)%VisFrontBeamDiffTrans(1:37,JB))

            IF (ShType == WSC_ST_InteriorBlind) THEN         ! Interior blind
              ! TH CR 8121, 7/7/2010
              !ReflBlBmDiffFront = InterpProfAng(ProfAng,Blind(BlNum)%VisFrontBeamDiffRefl)
              ReflBlBmDiffFront = InterpProfAng(ProfAng,Blind(BlNum)%VisFrontBeamDiffRefl(1:37,JB))

              ! TH added 7/12/2010 for CR 8121
              ReflBlDiffDiffFront = Blind(BlNum)%VisFrontDiffDiffRefl(JB)
              TransBlDiffDiffFront = Blind(BlNum)%VisFrontDiffDiffTrans(JB)

              TransMult(JB) = TVISBSun * (TransBlBmDiffFront + ReflBlBmDiffFront*ReflGlDiffDiffBack*TransBlDiffDiffFront / &
                                (1.d0 - ReflBlDiffDiffFront*ReflGlDiffDiffBack))

            ELSE IF (ShType == WSC_ST_ExteriorBlind) THEN    ! Exterior blind
              TransMult(JB) = TransBlBmDiffFront * (Construct(IConst)%TransDiffVis / &
                                (1.d0-ReflGlDiffDiffFront*Blind(BlNum)%VisBackDiffDiffRefl(JB))) &
                                * SurfaceWindow(IWin)%GlazedFrac * SurfaceWindow(IWin)%LightWellEff

            ELSE                                             ! Between-glass blind
              t1     = POLYF(COSBSun,Construct(IConst)%tBareVisCoef(1,1:6))
              tfshBd = InterpProfAng(ProfAng,Blind(BlNum)%VisFrontBeamDiffTrans(1:37,JB))
              rfshB  = InterpProfAng(ProfAng,Blind(BlNum)%VisFrontBeamDiffRefl(1:37,JB))
              IF (Construct(IConst)%TotGlassLayers == 2) THEN   ! 2 glass layers
                TransMult(JB) = t1*(tfshBd*(1.d0 + rfd2*rbshd) + rfshB*rbd1*tfshd)*td2 * SurfaceWindow(IWin)%LightWellEff
              ELSE                                             ! 3 glass layers; blind between layers 2 and 3
                t2            = POLYF(COSBSun,Construct(IConst)%tBareVisCoef(2,1:6))
                TransMult(JB) = t1*t2*(tfshBd*(1.d0 + rfd3*rbshd) + rfshB*(rbd2*tfshd + td2*rbd1*td2*tfshd))*td3 *  &
                                         SurfaceWindow(IWin)%LightWellEff
              END IF
            END IF
            IF (SurfaceWindow(IWin)%MovableSlats) THEN
              SlatAng = (JB - 1) * PI/(MaxSlatAngs - 1)
            ELSE
              SlatAng = Blind(BlNum)%SlatAngle * DegToRadians
            END IF
            TransBmBmMult(JB) = TVISBSun * BlindBeamBeamTrans(ProfAng,SlatAng,Blind(BlNum)%SlatWidth, &
                                  Blind(BlNum)%SlatSeparation,Blind(BlNum)%SlatThickness)
          END IF ! ShadeOn/ScreenOn/BlindOn/Diffusing glass

          IF (SurfaceWindow(IWin)%OriginalClass .EQ. SurfaceClass_TDD_Dome) THEN
            TransBmBmMult = 0.0 ! No beam, diffuse only
          END IF

          ! Daylighting shelf simplification:  No beam makes it past end of shelf, all light is diffuse
          IF (InShelfSurf > 0) THEN ! Inside daylighting shelf
            TransBmBmMult = 0.0 ! No beam, diffuse only (Not sure if this really works)
            ! SurfaceWindow(IWin)%FractionUpgoing is already set to 1.0 earlier
          END IF

          WLUMSU(JB+1,IHR) = WLUMSU(JB+1,IHR) + ZSU1 * TransMult(JB) / PI
          WLUMSUdisk(JB+1,IHR) = ZSU1 * TransBmBmMult(JB) / PI
          FLFWSU(JB+1) = FLFWSU(JB+1) + ZSU1 * TransMult(JB) * (1.0d0 - SurfaceWindow(IWin)%FractionUpgoing)
          FLFWSUdisk(JB+1) = ZSU1 * TransBmBmMult(JB)
          FLCWSU(JB+1) = FLCWSU(JB+1) + ZSU1 * TransMult(JB) * SurfaceWindow(IWin)%FractionUpgoing
        END DO ! End of loop over slat angles
      END IF ! End of window with shade or blind
    END IF ! COSBSun > 0
  END IF ! SunLitFracHR > 0

  ! Beam reaching window after specular reflection from exterior obstruction

  ! In the following, Beam normal illuminance times ZSU1refl = illuminance on window due to
  ! specular reflection from exterior surfaces

  IF(CalcSolRefl .AND. SurfaceWindow(IWin)%OriginalClass /= SurfaceClass_TDD_Dome) THEN
    ZSU1refl = ReflFacBmToBmSolObs(IWin,IHr)

    IF(ZSU1refl > 0.0) THEN
      ! Contribution to window luminance and downgoing flux

      ! -- Bare window. We use diffuse-diffuse transmittance here rather than beam-beam to avoid
      !    complications due to specular reflection from multiple exterior surfaces

      TVisSunRefl = Construct(IConst)%TransDiffVis * SurfaceWindow(IWin)%GlazedFrac * SurfaceWindow(IWin)%LightWellEff
      ! In the following it is assumed that all reflected beam is going downward, as it would be in the
      ! important case of reflection from a highly glazed facade of a neighboring building. However, in
      ! rare cases (such as upward specular reflection from a flat horizontal skylight) it may
      ! actually be going upward.
      FLFWSUdisk(1) = FLFWSUdisk(1) + ZSU1refl * TVisSunRefl

      ! -- Window with shade, blind or diffusing glass

      IF(ShadeOn .OR. BlindOn .OR. ScreenOn .OR. SurfaceWindow(IWin)%SolarDiffusing) THEN
        TransBmBmMult = 0.0
        TransMult = 0.0

        DO JB = 1, MaxSlatAngs
          IF(.NOT.SurfaceWindow(IWin)%MovableSlats .AND. JB > 1) EXIT

          IF(ShadeOn .OR. SurfaceWindow(IWin)%SolarDiffusing) THEN ! Shade on or diffusing glass
            TransMult(1) = Construct(IConstShaded)%TransDiffVis * SurfaceWindow(IWin)%GlazedFrac *  &
                              SurfaceWindow(IWin)%LightWellEff

          ELSEIF (ScreenOn) THEN ! Exterior screen on
            TransScDiffDiffFront = SurfaceScreens(SurfaceWindow(IWin)%ScreenNumber)%DifDifTransVis
            TransMult(1) = TransScDiffDiffFront * (Construct(IConst)%TransDiffVis / &
                                 (1.d0-ReflGlDiffDiffFront*ReflScDiffDiffBack)) &
                                 * SurfaceWindow(IWin)%GlazedFrac * SurfaceWindow(IWin)%LightWellEff

          ELSE             ! Blind on
            TransBlDiffDiffFront = Blind(BlNum)%VisFrontDiffDiffTrans(JB)
            IF(ShType == WSC_ST_InteriorBlind) THEN         ! Interior blind
                ReflBlDiffDiffFront = Blind(BlNum)%VisFrontDiffDiffRefl(JB)
                TransMult(JB) = TVISSunRefl * (TransBlDiffDiffFront +  &
                                  ReflBlDiffDiffFront*ReflGlDiffDiffBack*TransBlDiffDiffFront / &
                                  (1.d0 - ReflBlDiffDiffFront*ReflGlDiffDiffBack))

            ELSE IF(ShType == WSC_ST_ExteriorBlind) THEN    ! Exterior blind
              TransMult(JB) = TransBlDiffDiffFront * (Construct(IConst)%TransDiffVis / &
                                 (1.d0-ReflGlDiffDiffFront*Blind(BlNum)%VisBackDiffDiffRefl(JB))) &
                                 * SurfaceWindow(IWin)%GlazedFrac * SurfaceWindow(IWin)%LightWellEff

            ELSE                                            ! Between-glass blind
              t1     = Construct(IConst)%tBareVisDiff(1)
              tfshBd = Blind(BlNum)%VisFrontDiffDiffTrans(JB)
              rfshB  = Blind(BlNum)%VisFrontDiffDiffRefl(JB)
              IF (Construct(IConst)%TotGlassLayers == 2) THEN  ! 2 glass layers
                TransMult(JB) = t1*(tfshBd*(1.d0 + rfd2*rbshd) + rfshB*rbd1*tfshd)*td2 * SurfaceWindow(IWin)%LightWellEff
              ELSE                                             ! 3 glass layers; blind between layers 2 and 3
                t2            = Construct(IConst)%tBareVisDiff(2)
                TransMult(JB) = t1*t2*(tfshBd*(1.d0 + rfd3*rbshd) + rfshB*(rbd2*tfshd + td2*rbd1*td2*tfshd))*td3 *  &
                                    SurfaceWindow(IWin)%LightWellEff
              END IF
            END IF  ! End of check of interior/exterior/between-glass blind
          END IF    ! ShadeOn/BlindOn

          WLUMSU(JB+1,IHR) = WLUMSU(JB+1,IHR) + ZSU1refl * TransMult(JB) / PI
          FLFWSU(JB+1) = FLFWSU(JB+1) + ZSU1refl * TransMult(JB) * (1.0d0 - SurfaceWindow(IWin)%FractionUpgoing)
          FLCWSU(JB+1) = FLCWSU(JB+1) + ZSU1refl * TransMult(JB) * SurfaceWindow(IWin)%FractionUpgoing
        END DO ! End of loop over slat angles
      END IF ! End of check if window has shade, blind or diffusing glass
    END IF  ! End of check if ZSU1refl > 0.0
  END IF  ! End of check if solar reflections are in effect

  ! Sun-related portion of internally reflected illuminance

  DO JSH = 1,MaxSlatAngs + 1
    IF (.NOT.SurfaceWindow(IWin)%MovableSlats .AND. JSH > 2) EXIT

    ! Full area of window is used in following since effect of dividers on reducing
    ! effective window transmittance already accounted for in calc of FLFWSU and FLCWSU
    ! CR 7869 added effect of intervening interior windows on transmittance and
    ! added inside surface area of adjacent zone
    EINTSU(JSH,IHR) = (FLFWSU(JSH) * SurfaceWindow(IWin)%RhoFloorWall &
                      + FLCWSU(JSH) * SurfaceWindow(IWin)%RhoCeilingWall) *  &
                      (Surface(IWin)%Area/SurfaceWindow(IWin)%GlazedFrac) / &
                      (ZoneInsideSurfArea*(1.d0-ZoneDaylight(ZoneNum)%AveVisDiffReflect))

    EINTSUdisk(JSH,IHR) = FLFWSUdisk(JSH) * SurfaceWindow(IWin)%RhoFloorWall  *  &
                      (Surface(IWin)%Area/SurfaceWindow(IWin)%GlazedFrac) / &
                      (ZoneInsideSurfArea*(1.d0-ZoneDaylight(ZoneNum)%AveVisDiffReflect))
  END DO

  RETURN

END SUBROUTINE DayltgInterReflectedIllum


FUNCTION DayltgSkyLuminance(ISky, THSKY, PHSKY)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   July 1997
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Called by CalcDayltgCoefficients, DayltgExtHorizIllum AND DayltgInterReflectedIllum.  gives
          ! luminance in cd/m2 for four different sky types, as described in R.Perez, P.Ineichen,
          ! R.Seals, J.Michalsky and R.Stewart, "Modeling daylight availability and irradiance
          ! components from direct and global irradiance," Solar Energy 44, 1990, 271-289.
          ! The luminance distributions in this routine are normalized such that
          ! the zenith luminance is 1.0, i.e., DayltgSkyLuminance =
          ! (sky luminance at THSKY, PHSKY)/(zenith luminance), which is dimensionless.
          ! The sky types are:
          ! 1. Standard CIE clear sky
          ! 2. Standard CIE high-turbidity clear sky
          ! 3. CIE intermediate sky
          ! 4. CIE overcast sky

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:
          ! Based on DOE-2.1E subroutine DSKYLU, which did only clear and overcast skies.

          ! OTHER NOTES:
          ! THSKY ranges from 0 to 2Pi starting with 0 directly East and rotating clockwise.
          ! PHSKY ranges from 0 to Pi starting with 0 at the horizon and Pi/2 at the zenith.

          ! USE STATEMENTS: na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER           :: ISky                      ! Sky type: 1=clear, 2=clear turbid, 3=intermediate, 4=overcast
  REAL(r64)         :: THSKY,PHSKY               ! Azimuth and altitude of sky element (radians)

          ! FUNCTION PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: SPHSKY                    ! Sine of PHSKY
  REAL(r64) :: G                         ! Angle between sun and element of sky (radians)
  REAL(r64) :: COSG                      ! Cosine of G
  REAL(r64) :: Z                         ! Solar zenith angle (radians)
  REAL(r64) :: Z1,Z2,Z3,Z4               ! Luminance factors (intermediate variables)
  REAL(r64) :: DayltgSkyLuminance        ! Luminance of sky element divided by zenith luminance

          ! FLOW:
  SPHSKY = MAX(SIN(PHSKY),0.01d0) ! Prevent floating point underflows
  Z = PIOVR2 - PHSUN
  IF (ISky >= 1 .AND. ISky <= 3) THEN ! Following not needed for overcast sky
    COSG = SPHSKY * SPHSUN + COS(PHSKY) * CPHSUN * COS(THSKY - THSUN)
    COSG = MAX(constant_minusone,MIN(COSG,1.0d0)) ! Prevent out of range due to roundoff
    G = ACOS(COSG)
  END IF

  SELECT CASE(ISky)
    CASE(1) ! Clear Sky
      Z1 = 0.910d0 + 10.d0 * EXP(-3.d0 * G) + 0.45d0 * COSG * COSG
      Z2 = 1.d0 - EXP(-0.32d0 / SPHSKY)
      Z3 = 0.27385d0 * (0.91d0 + 10.d0 * EXP(-3.d0 * Z) + 0.45d0 * SPHSUN * SPHSUN)
      DayltgSkyLuminance = Z1 * Z2 / Z3

    CASE(2) ! Clear turbid sky
      Z1 = 0.856d0 + 16.d0 * EXP(-3.d0 * G) + 0.3d0 * COSG * COSG
      Z2 = 1.d0 - EXP(-0.32d0 / SPHSKY)
      Z3 = 0.27385d0 * (0.856d0 + 16.d0 * EXP(-3.d0 * Z) + 0.3d0 * SPHSUN * SPHSUN)
      DayltgSkyLuminance = Z1 * Z2 / Z3

    CASE(3) ! Intermediate sky
      Z1 = (1.35d0 * (SIN(3.59d0 * PHSKY - 0.009d0) + 2.31d0) * SIN(2.6d0 * PHSUN + 0.316d0) + PHSKY + 4.799d0) / 2.326d0
      Z2 = EXP(-G * 0.563d0 * ((PHSUN - 0.008d0) * (PHSKY + 1.059d0) + 0.812d0))
      Z3 = 0.99224d0 * SIN(2.6d0 * PHSUN + 0.316d0) + 2.73852d0
      Z4 = EXP(-Z * 0.563d0 * ((PHSUN - 0.008d0) * 2.6298d0 + 0.812d0))
      DayltgSkyLuminance = Z1 * Z2 / (Z3 * Z4)

    CASE(4) ! Overcast sky
      DayltgSkyLuminance = (1.0d0 + 2.0d0 * SPHSKY) / 3.0d0

  END SELECT

  RETURN

END FUNCTION DayltgSkyLuminance

SUBROUTINE ProfileAngle(SurfNum,CosDirSun,HorOrVert,ProfileAng)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   May 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates profile angle for a surface.

          ! REFERENCES: na
          ! USE STATEMENTS:

  USE DataGlobals
  USE DataSurfaces

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER   :: SurfNum           ! Surface number
  REAL(r64) :: CosDirSun(3)      ! Solar direction cosines
  INTEGER   :: HorOrVert         ! If HORIZONTAL, calculates ProfileAngHor
  REAL(r64) :: ProfileAng        ! Solar profile angle (radians).
                                 ! For HorOrVert = HORIZONTAL,
                                 !  this is the incidence angle in a plane that is normal to the window
                                 !  and parallel to the Y-axis of the window (the axis along
                                 !  which the height of the window is measured).
                                 !  For HorOrVert = VERTICAL,
                                 !  this is the incidence angle in a plane that is normal to the window
                                 !  and parallel to the X-axis of the window (the axis along
                                 !  which the width of the window is measured).
                                 ! If VERTICAL, calculates ProfileAngVert

          ! SUBROUTINE PARAMETER DEFINITIONS: na
          ! INTERFACE BLOCK SPECIFICATIONS: na
          ! DERIVED TYPE DEFINITIONS: na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: ElevSun           ! Sun elevation; angle between sun and horizontal (radians)
  REAL(r64) :: ElevWin           ! Window elevation: angle between window outward normal and horizontal (radians)
  REAL(r64) :: AzimWin           ! Window azimuth (radians)
  REAL(r64) :: AzimSun           ! Sun azimuth (radians)
  REAL(r64) :: WinNorm(3)        ! Window outward normal unit vector
  REAL(r64) :: ThWin             ! Azimuth angle of WinNorm
  REAL(r64) :: SunPrime(3)       ! Projection of sun vector onto plane (perpendicular to
                                        !  window plane) determined by WinNorm and vector along
                                        !  baseline of window
  REAL(r64) :: WinNormCrossBase(3) ! Cross product of WinNorm and vector along window baseline
!  INTEGER            :: IComp             ! Vector component index

          ! FLOW:
  IF (HorOrVert == Horizontal) THEN ! Profile angle for horizontal structures
    ElevWin = Piovr2 - Surface(SurfNum)%Tilt * DegToRadians
    AzimWin = (90.d0 - Surface(SurfNum)%Azimuth) * DegToRadians
    ElevSun = ASIN(CosDirSun(3))
    AzimSun = ATAN2(CosDirSun(2),CosDirSun(1))
    ProfileAng = ATAN(SIN(ElevSun)/ABS(COS(ElevSun) * COS(AzimWin-AzimSun))) - ElevWin
  ELSE ! Profile angle for vertical structures
    ElevWin = Piovr2 - Surface(SurfNum)%Tilt * DegToRadians
    AzimWin = Surface(SurfNum)%Azimuth * DegToRadians ! 7952
    AzimSun = ATAN2(CosDirSun(1),CosDirSun(2)) ! 7952
    IF (ABS(ElevWin) < 0.1d0) THEN ! Near-vertical window
      ProfileAng = AzimWin -  AzimSun !CR7952 allow sign changes.
    ELSE
      WinNorm=Surface(SurfNum)%OutNormVec
      ThWin = AzimWin - PiOvr2
      WinNormCrossBase(1) = -SIN(ElevWin) * COS(ThWin)
      WinNormCrossBase(2) =  SIN(ElevWin) * SIN(ThWin)
      WinNormCrossBase(3) =  COS(ElevWin)
      SunPrime = CosDirSun - WinNormCrossBase * DOT_PRODUCT(CosDirSun,WinNormCrossBase)
      ProfileAng = ABS(ACOS(DOT_PRODUCT(WinNorm,SunPrime)/SQRT(DOT_PRODUCT(SunPrime,SunPrime))))
      !CR7952 correct sign of result for vertical slats
      IF ((AzimWin - AzimSun) < 0.0D0) ProfileAng = -1.0D0 * ProfileAng
    END IF
    ! Constrain to 0 to pi
    IF (ProfileAng > Pi) ProfileAng = 2.d0 * Pi - ProfileAng
  END IF

  RETURN

END SUBROUTINE ProfileAngle

SUBROUTINE DayltgClosestObstruction(RecPt,RayVec,NearestHitSurfNum,NearestHitPt)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   November 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Determines surface number and hit point of closest exterior obstruction hit
          ! by a ray from a window. If no obstruction is hit, NearestHitSurfNum = 0.

          ! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! USE STATEMENTS: na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)     :: RecPt(3)           ! Point on window from which ray emanates (m)
  REAL(r64), INTENT(IN) :: RayVec(3)  ! Unit vector along ray pointing away from window (m)
  INTEGER, INTENT(OUT) :: NearestHitSurfNum  ! Surface number of nearest obstruction that is hit by ray;
                                             !  = 0 if no obstruction is hit.
  REAL(r64), INTENT(OUT)    :: NearestHitPt(3)    ! Ray's hit point on nearest obstruction (m)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na
  REAL(r64) :: HitPt(3)           ! Hit point on an obstruction (m)
  INTEGER   :: IHit               ! > 0 if obstruction is hit, 0 otherwise
  INTEGER   :: ObsSurfNum         ! Obstruction surface number

  INTEGER   :: TotObstructionsHit ! Number of obstructions hit by a ray
  REAL(r64) :: HitDistance        ! Distance from receiving point to hit point for a ray (m)
  REAL(r64) :: NearestHitDistance ! Distance from receiving point to nearest hit point for a ray (m)
  INTEGER   :: ObsSurfNumToSkip   ! Surface number of obstruction to be ignored

          ! FLOW:

TotObstructionsHit = 0
NearestHitSurfNum = 0
NearestHitDistance = 1.d+8
NearestHitPt = 0.0
ObsSurfNumToSkip = 0
DO ObsSurfNum = 1,TotSurfaces
  IF(.NOT.Surface(ObsSurfNum)%ShadowSurfPossibleObstruction) CYCLE
  ! If a window was hit previously (see below), ObsSurfNumToSkip was set to the window's base surface in order
  ! to remove that surface from consideration as a hit surface for this ray
  IF(ObsSurfNum == ObsSurfNumToSkip) CYCLE
  ! Determine if this ray hits ObsSurfNum (in which case IHit > 0) and, if so, get the
  ! distance from the receiving point to the hit
  CALL DayltgPierceSurface(ObsSurfNum,RecPt,RayVec,IHit,HitPt)
  IF(IHit > 0) THEN
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
      END IF
    END IF
  END IF  ! End of check if obstruction was hit
END DO  ! End of loop over possible obstructions for this ray

RETURN
END SUBROUTINE DayltgClosestObstruction

SUBROUTINE DayltgSurfaceLumFromSun(IHr,Ray,ReflSurfNum,ReflHitPt,LumAtReflHitPtFrSun)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   November 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates exterior surface luminance due to beam solar diffuse reflection.

          ! METHODOLOGY EMPLOYED: na

          ! REFERENCES: na

          ! USE STATEMENTS: na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: IHr                ! Hour number
  REAL(r64), INTENT(IN) :: Ray(3)    ! Ray from window to reflecting surface (m)
  INTEGER, INTENT(IN) :: ReflSurfNum        ! Number of surface for which luminance is being calculated
  REAL(r64), INTENT(IN)    :: ReflHitPt(3)       ! Point on ReflSurfNum for luminance calculation (m)
  REAL(r64), INTENT(OUT)   :: LumAtReflHitPtFrSun ! Luminance at ReflHitPt from beam solar reflection for unit
                                            !  beam normal illuminance (cd/m2)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: ReflNorm(3)        ! Unit normal to reflecting surface (m)
  INTEGER   :: ObsSurfNum         ! Obstruction surface number
  INTEGER   :: IHitObs            ! > 0 if obstruction is hit
  REAL(r64) :: ObsHitPt(3)        ! Hit point on obstruction (m)
  REAL(r64) :: CosIncAngAtHitPt   ! Cosine of angle of incidence of sun at HitPt
  REAL(r64) :: DiffVisRefl        ! Diffuse visible reflectance of ReflSurfNum

          ! FLOW:

LumAtReflHitPtFrSun = 0.0
! Skip daylighting shelves since reflection from these is separately calculated
IF(Surface(ReflSurfNum)%Shelf > 0) RETURN
! Normal to reflecting surface in hemisphere containing window element
ReflNorm = Surface(ReflSurfNum)%OutNormVec
IF(Surface(ReflSurfNum)%ShadowingSurf) THEN
  IF(DOT_PRODUCT(ReflNorm,Ray) > 0.0) ReflNorm = -ReflNorm
END IF
! Cosine of angle of incidence of sun at HitPt if sun were to reach HitPt
CosIncAngAtHitPt = DOT_PRODUCT(ReflNorm,SunCosHr(1:3,IHr))
! Require that the sun be in front of this surface relative to window element
IF(CosIncAngAtHitPt <= 0.0) RETURN  ! Sun is in back of reflecting surface
! Sun reaches ReflHitPt if vector from ReflHitPt to sun is unobstructed
IHitObs = 0
DO ObsSurfNum = 1,TotSurfaces
  IF(.NOT.Surface(ObsSurfNum)%ShadowSurfPossibleObstruction) CYCLE
  ! Exclude as a possible obstructor ReflSurfNum and its base surface (if it has one)
  IF(ObsSurfNum == ReflSurfNum .OR. ObsSurfNum == Surface(ReflSurfNum)%BaseSurf) CYCLE
  CALL DayltgPierceSurface(ObsSurfNum,ReflHitPt,SunCosHr(1:3,IHr),IHitObs,ObsHitPt)
  IF(IHitObs > 0) EXIT
END DO
IF(IHitObs > 0) RETURN  ! Obstruction was hit, blocking sun
! Obstruction was not hit; sun reaches ReflHitPt.
! Calculate luminance at ReflHitPt due to beam solar reflection (for unit beam normal illuminance)
IF(Surface(ReflSurfNum)%ShadowingSurf) THEN
  DiffVisRefl = Surface(ReflSurfNum)%ShadowSurfDiffuseVisRefl
  ! Note that if the shadowing surface has a non-zero glazing fraction (e.g., neighboring bldg) that the above is
  ! (1 - glazing fraction) * (vis refl of opaque part of shadowing surface); specular reflection is
  ! excluded in this value of DiffVisRefl.
ELSE  ! Exterior building surface
  IF(.NOT.Construct(Surface(ReflSurfNum)%Construction)%TypeIsWindow) THEN
    DiffVisRefl = 1.0d0 - Construct(Surface(ReflSurfNum)%Construction)%OutsideAbsorpSolar
  ELSE
    ! Window; assume bare so no beam-to-diffuse reflection
    DiffVisRefl = 0.0
  END IF
END IF
LumAtReflHitPtFrSun = CosIncAngAtHitPt * DiffVisRefl / Pi

RETURN
END SUBROUTINE DayltgSurfaceLumFromSun

SUBROUTINE DayltgInteriorMapIllum(ZoneNum)

! *****super modified version of DayltgInteriorIllum by Peter Graham Ellis
! *****removes all control code, just calculates illum and glare with previously determined control settings
! *****this should be packaged into a subroutine called from 2 places

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   July 1997
          !       MODIFIED       March 2000, FW: interpolate clear-sky daylight factors using
          !                      HourOfDay/WeightNow and NextHour/WeightNextHour. Previously
          !                      only HourOfDay was used
          !                      Jan 2001, FW: interpolate in slat angle for windows with blinds
          !                      that have movable slats
          !                      Dec 2003, FW: fix bug--even though between-glass shade/blind is on
          !                        daylight illum at ref pt was calculated as though it was off
          !                      June 2009, TH: modified for thermochromic windows
          !                      March 2010, TH: fix bug (CR 8057) for electrochromic windows
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Using daylighting factors and exterior illuminance, determine
          ! the current-hour interior daylight illuminance and glare index
          ! at each reference point in a space.

          ! Called by InitSurfaceHeatBalance.

          ! METHODOLOGY EMPLOYED:na

          ! REFERENCES:
          ! Based on DOE-2.1E subroutine DINTIL.

          ! USE STATEMENTS:
  USE General, ONLY: POLYF, InterpSlatAng

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
INTEGER           :: ZoneNum               ! Zone number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
INTEGER   :: NREFPT                ! Number of daylighting reference points
INTEGER   :: REFPT1                ! 1st reference point
INTEGER   :: ISky                  ! Sky type index
INTEGER   :: ISky1, ISky2          ! Sky type index values for averaging two sky types
REAL(r64) :: DFSKHR (4,2)          ! Sky daylight factor for sky type (first index),
                                   !   bare/shaded window (second index)
REAL(r64) :: DFSUHR (2)            ! Sun daylight factor for bare/shaded window
REAL(r64) :: BFSKHR (4,2)          ! Sky background luminance factor for sky type (first index),
                                   !   bare/shaded window (second index)
REAL(r64) :: BFSUHR (2)            ! Sun background luminance factor for bare/shaded window
REAL(r64) :: SFSKHR (4,2)          ! Sky source luminance factor for sky type (first index),
                                   !   bare/shaded window (second index)
REAL(r64) :: SFSUHR (2)            ! Sun source luminance factor for bare/shaded window
INTEGER   :: IL                    ! Reference point index
INTEGER   :: IWin                  ! Window index
INTEGER   :: IS                    ! IS=1 for unshaded window, =2 for shaded window
INTEGER   :: ISWFLG                ! Switchable glazing flag: =1 if one or more windows in a zone
                                   !  has switchable glazing that adjusts visible transmittance to just meet
                                   !  daylighting set point; =0 otherwise.
INTEGER   :: ICtrl                 ! Window shading control pointer
REAL(r64) :: SkyWeight             ! Weighting factor used to average two different sky types
REAL(r64) :: HorIllSky(4)          ! Horizontal illuminance for different sky types
REAL(r64) :: HorIllSkyFac          ! Ratio between horizontal illuminance from sky horizontal irradiance and
                                   !   luminous efficacy and horizontal illuminance from averaged sky
REAL(r64) :: SlatAng               ! Blind slat angle (rad)
LOGICAL   :: VarSlats              ! True if slats are movable, i.e., variable angle
INTEGER   :: loop                  ! Window loop index
REAL(r64) :: GTOT
REAL(r64) :: GTOT1
REAL(r64) :: GTOT2
REAL(r64), SAVE, ALLOCATABLE, DIMENSION(:) :: DaylIllum
REAL(r64), SAVE, ALLOCATABLE, DIMENSION(:) :: BACLUM
REAL(r64), SAVE, ALLOCATABLE, DIMENSION(:) :: GLRNDX
LOGICAL, SAVE :: FirstTimeFlag=.true.
INTEGER   :: ILB

INTEGER   :: IConst
REAL(r64) :: VTRatio
REAL(r64) :: VTNow
REAL(r64) :: VTMaster

REAL(r64) :: VTDark = 0.0d0         ! Visible transmittance (VT) of electrochromic (EC) windows in fully dark state
REAL(r64) :: VTMULT = 1.0d0         ! VT multiplier for EC windows
INTEGER   :: IConstShaded = 0       ! The shaded window construction for switchable windows


      IF (FirstTimeFlag) THEN
        ALLOCATE(DaylIllum(MaxMapRefPoints))
        ALLOCATE(BACLUM(MaxMapRefPoints))
        ALLOCATE(GLRNDX(MaxMapRefPoints))
        FirstTimeFlag=.false.
      ENDIF

      IF (WarmUpFlag) RETURN
!              Initialize reference point illuminance and window background luminance

      NREFPT = ZoneDaylight(ZoneNum)%TotalMapRefPoints
      REFPT1 = 1

      DaylIllum = 0.
      BACLUM = 0.
      GLRNDX=0.

      IF(SkyClearness > 3.0d0) THEN       !Sky is average of clear and clear turbid
        SkyWeight = MIN(1.0d0,(SkyClearness-3.d0)/3.d0)
        ISky1 = 1
        ISky2 = 2
      ELSE IF(SkyClearness > 1.2d0) THEN  !Sky is average of clear turbid and intermediate
        SkyWeight = (SkyClearness - 1.2d0)/1.8d0
        ISky1 = 2
        ISky2 = 3
      ELSE                              !Sky is average of intermediate and overcast
        SkyWeight = MIN(1.0d0, MAX(0.0d0, (SkyClearness-1.d0)/0.2d0, (SkyBrightness-0.05d0)/0.4d0))
        ISky1 = 3
        ISky2 = 4
      END IF

!              First loop over windows in this space.
!              Find contribution of each window to the daylight illum
!              and to the glare numerator at each reference point.
!              Use shading flags set in WindowShadingManager.

      DO 2000 loop = 1,ZoneDaylight(ZoneNum)%NumOfDayltgExtWins
        IWin = ZoneDaylight(ZoneNum)%DayltgExtWinSurfNums(loop)

        ! Added TH 6/29/2009 for thermochromic windows
        VTRatio = 1.0d0
        IF (NREFPT > 0) THEN
          IConst = Surface(IWin)%Construction
          IF (Construct(IConst)%TCFlag == 1) THEN
            ! For thermochromic windows, daylight and glare factors are always calculated
            !  based on the master construction. They need to be adjusted by the VTRatio, including:
            !  ZoneDaylight()%DaylIllFacSky, DaylIllFacSun, DaylIllFacSunDisk; DaylBackFacSky,
            !  DaylBackFacSun, DaylBackFacSunDisk, DaylSourceFacSky, DaylSourceFacSun, DaylSourceFacSunDisk
            VTNow = POLYF(1.0d0,Construct(IConst)%TransVisBeamCoef(1))
            VTMaster = POLYF(1.0d0,Construct(Construct(IConst)%TCMasterConst)%TransVisBeamCoef(1))
            VTRatio = VTNow / VTMaster
          ENDIF
        ENDIF

!              Loop over reference points
          DO 3000 IL = REFPT1,NREFPT
            ! in this structure the Map points are all 3...
            ILB=2+IL

!              Daylight factors for current sun position
            DO ISky = 1,4

!                                ===Bare window===
              DFSKHR(ISky,1) = VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,ILB,ISky,1,HourOfDay) + &
                               WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,ILB,ISky,1,PreviousHour))

              IF(ISky == 1) DFSUHR(1) = VTRatio * (WeightNow * &
                   (ZoneDaylight(ZoneNum)%DaylIllFacSun(loop,ILB,1,HourOfDay) +   &
                     ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loop,ILB,1,HourOfDay)) + &
                                        WeightPreviousHour * &
                   (ZoneDaylight(ZoneNum)%DaylIllFacSun(loop,ILB,1,PreviousHour) +   &
                             ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loop,ILB,1,PreviousHour)))

              BFSKHR(ISky,1) = VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylBackFacSky(loop,ILB,ISky,1,HourOfDay) + &
                               WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylBackFacSky(loop,ILB,ISky,1,PreviousHour))

              IF(ISky == 1) BFSUHR(1) = VTRatio * (WeightNow * &
                   (ZoneDaylight(ZoneNum)%DaylBackFacSun(loop,ILB,1,HourOfDay) +   &
                                ZoneDaylight(ZoneNum)%DaylBackFacSunDisk(loop,ILB,1,HourOfDay)) +&
                               WeightPreviousHour * &
                   (ZoneDaylight(ZoneNum)%DaylBackFacSun(loop,ILB,1,PreviousHour) +   &
                        ZoneDaylight(ZoneNum)%DaylBackFacSunDisk(loop,ILB,1,PreviousHour)))

              SFSKHR(ISky,1) = VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylSourceFacSky(loop,ILB,ISky,1,HourOfDay) + &
                               WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylSourceFacSky(loop,ILB,ISky,1,PreviousHour))

              IF(ISky == 1) SFSUHR(1) = VTRatio * (WeightNow * &
                   (ZoneDaylight(ZoneNum)%DaylSourceFacSun(loop,ILB,1,HourOfDay) +   &
                      ZoneDaylight(ZoneNum)%DaylSourceFacSunDisk(loop,ILB,1,HourOfDay)) + &
                            WeightPreviousHour * &
                   (ZoneDaylight(ZoneNum)%DaylSourceFacSun(loop,ILB,1,PreviousHour) +   &
                      ZoneDaylight(ZoneNum)%DaylSourceFacSunDisk(loop,ILB,1,PreviousHour)))

              IF (SurfaceWindow(IWin)%ShadingFlag >= 1 .OR. SurfaceWindow(IWin)%SolarDiffusing) THEN

!                                 ===Shaded window===
                IF(.NOT.SurfaceWindow(IWin)%MovableSlats) THEN

                ! Shade, screen, blind with fixed slats, or diffusing glass
                  DFSKHR(ISky,2) = VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,ILB,ISky,2,HourOfDay) + &
                                   WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,ILB,ISky,2,PreviousHour))

                  IF(ISky == 1) THEN
                    DFSUHR(2) = VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylIllFacSun(loop,ILB,2,HourOfDay) +  &
                                WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylIllFacSun(loop,ILB,2,PreviousHour))

                    IF(.NOT.SurfaceWindow(IWin)%SlatsBlockBeam) &
                      DFSUHR(2) = DFSUHR(2) + VTRatio * (WeightNow *   &
                                     ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loop,ILB,2,HourOfDay) + &
                                     WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loop,ILB,2,PreviousHour))
                  END IF

                  BFSKHR(ISky,2) = VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylBackFacSky(loop,ILB,ISky,2,HourOfDay) + &
                                   WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylBackFacSky(loop,ILB,ISky,2,PreviousHour))

                  IF(ISky == 1) THEN
                    BFSUHR(2) = VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylBackFacSun(loop,ILB,2,HourOfDay) +  &
                                WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylBackFacSun(loop,ILB,2,PreviousHour))
                    IF(.NOT.SurfaceWindow(IWin)%SlatsBlockBeam) &
                      BFSUHR(2) = BFSUHR(2) + VTRatio *   &
                         (WeightNow * ZoneDaylight(ZoneNum)%DaylBackFacSunDisk(loop,ILB,2,HourOfDay) + &
                          WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylBackFacSunDisk(loop,ILB,2,PreviousHour))
                  END IF

                  SFSKHR(ISky,2) = VTRatio *   &
                     (WeightNow * ZoneDaylight(ZoneNum)%DaylSourceFacSky(loop,ILB,ISky,2,HourOfDay) + &
                      WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylSourceFacSky(loop,ILB,ISky,2,PreviousHour))

                  IF(ISky == 1) THEN
                    SFSUHR(2) = VTRatio *   &
                       (WeightNow * ZoneDaylight(ZoneNum)%DaylSourceFacSun(loop,ILB,2,HourOfDay) +  &
                        WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylSourceFacSun(loop,ILB,2,PreviousHour))
                    IF(.NOT.SurfaceWindow(IWin)%SlatsBlockBeam) &
                      SFSUHR(2) = SFSUHR(2) + VTRatio *   &
                         (WeightNow * ZoneDaylight(ZoneNum)%DaylSourceFacSunDisk(loop,ILB,2,HourOfDay) + &
                          WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylSourceFacSunDisk(loop,ILB,2,PreviousHour))
                  END IF

                ELSE  ! Blind with movable slats
                  VarSlats = SurfaceWindow(IWin)%MovableSlats
                  SlatAng = SurfaceWindow(IWin)%SlatAngThisTs

                  DFSKHR(ISky,2) = VTRatio * (WeightNow * InterpSlatAng(SlatAng,VarSlats,  &
                                   ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,ILB,ISky,2:MaxSlatAngs+1,HourOfDay)) + &
                                   WeightPreviousHour * InterpSlatAng(SlatAng,VarSlats,  &
                                   ZoneDaylight(ZoneNum)%DaylIllFacSky(loop,ILB,ISky,2:MaxSlatAngs+1,PreviousHour)))

                  IF(ISky == 1) THEN
                    DFSUHR(2) = VTRatio* (WeightNow * InterpSlatAng(SlatAng,VarSlats,  &
                       ZoneDaylight(ZoneNum)%DaylIllFacSun(loop,ILB,2:MaxSlatAngs+1,HourOfDay)) + &
                       WeightPreviousHour * InterpSlatAng(SlatAng,VarSlats,  &
                       ZoneDaylight(ZoneNum)%DaylIllFacSun(loop,ILB,2:MaxSlatAngs+1,PreviousHour)))

                    ! We add the contribution from the solar disk if slats do not block beam solar
                    ! TH CR 8010, DaylIllFacSunDisk needs to be interpolated
                    !IF(.NOT.SurfaceWindow(IWin)%SlatsBlockBeam) DFSUHR(2) = DFSUHR(2) + &
                    !  VTRatio * (WeightNow * ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loop,ILB,2,HourOfDay) + &
                    !            WeightPreviousHour * ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loop,ILB,2,PreviousHour))
                    IF(.NOT.SurfaceWindow(IWin)%SlatsBlockBeam) DFSUHR(2) = DFSUHR(2) + &
                                VTRatio * (WeightNow * InterpSlatAng(SlatAng,VarSlats,  &
                                ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loop,ILB,2:MaxSlatAngs+1,HourOfDay)) + &
                                WeightPreviousHour * InterpSlatAng(SlatAng,VarSlats,  &
                                ZoneDaylight(ZoneNum)%DaylIllFacSunDisk(loop,ILB,2:MaxSlatAngs+1,PreviousHour)))
                  END IF

                  BFSKHR(ISky,2) = VTRatio*(WeightNow * InterpSlatAng(SlatAng,VarSlats,  &
                            ZoneDaylight(ZoneNum)%DaylBackFacSky(loop,ILB,ISky,2:MaxSlatAngs+1,HourOfDay)) + &
                            WeightPreviousHour * InterpSlatAng(SlatAng,VarSlats,  &
                            ZoneDaylight(ZoneNum)%DaylBackFacSky(loop,ILB,ISky,2:MaxSlatAngs+1,PreviousHour)))

                  IF(ISky == 1) THEN
                    BFSUHR(2) = VTRatio*(WeightNow * InterpSlatAng(SlatAng,VarSlats,  &
                       ZoneDaylight(ZoneNum)%DaylBackFacSun(loop,ILB,2:MaxSlatAngs+1,HourOfDay)) + &
                       WeightPreviousHour * InterpSlatAng(SlatAng,VarSlats,  &
                       ZoneDaylight(ZoneNum)%DaylBackFacSun(loop,ILB,2:MaxSlatAngs+1,PreviousHour)))

                    ! TH CR 8010, DaylBackFacSunDisk needs to be interpolated
                    IF(.NOT.SurfaceWindow(IWin)%SlatsBlockBeam) BFSUHR(2) = BFSUHR(2) + &
                       VTRatio * (WeightNow * InterpSlatAng(SlatAng,VarSlats,  &
                       ZoneDaylight(ZoneNum)%DaylBackFacSunDisk(loop,ILB,2:MaxSlatAngs+1,HourOfDay)) + &
                       WeightPreviousHour * InterpSlatAng(SlatAng,VarSlats,  &
                       ZoneDaylight(ZoneNum)%DaylBackFacSunDisk(loop,ILB,2:MaxSlatAngs+1,PreviousHour)))
                  END IF

                  SFSKHR(ISky,2) = VTRatio*(WeightNow * InterpSlatAng(SlatAng,VarSlats,  &
                                  ZoneDaylight(ZoneNum)%DaylSourceFacSky(loop,ILB,ISky,2:MaxSlatAngs+1,HourOfDay)) + &
                                  WeightPreviousHour * InterpSlatAng(SlatAng,VarSlats,  &
                                  ZoneDaylight(ZoneNum)%DaylSourceFacSky(loop,ILB,ISky,2:MaxSlatAngs+1,PreviousHour)))

                  IF(ISky == 1) THEN
                    SFSUHR(2) = VTRatio*(WeightNow * InterpSlatAng(SlatAng,VarSlats,  &
                                ZoneDaylight(ZoneNum)%DaylSourceFacSun(loop,ILB,2:MaxSlatAngs+1,HourOfDay)) + &
                                WeightPreviousHour * InterpSlatAng(SlatAng,VarSlats,  &
                                ZoneDaylight(ZoneNum)%DaylSourceFacSun(loop,ILB,2:MaxSlatAngs+1,PreviousHour)))

                    ! TH CR 8010, DaylSourceFacSunDisk needs to be interpolated
                    IF(.NOT.SurfaceWindow(IWin)%SlatsBlockBeam) SFSUHR(2) = SFSUHR(2) + &
                       VTRatio * (WeightNow * InterpSlatAng(SlatAng,VarSlats,  &
                       ZoneDaylight(ZoneNum)%DaylSourceFacSunDisk(loop,ILB,2:MaxSlatAngs+1,HourOfDay)) + &
                       WeightPreviousHour * InterpSlatAng(SlatAng,VarSlats,  &
                       ZoneDaylight(ZoneNum)%DaylSourceFacSunDisk(loop,ILB,2:MaxSlatAngs+1,PreviousHour)))
                  END IF

                END IF ! End of check if window has blind with movable slats

              ENDIF  ! End of check if window is shaded or has diffusing glass

          END DO

!              Get illuminance at ref point from bare and shaded window by
!              multiplying daylight factors by exterior horizontal illuminance

! Adding 0.001 in the following prevents zero HorIllSky in early morning or late evening when sun
! is up in the present time step but GILSK(ISky,HourOfDay) and GILSK(ISky,NextHour) are both zero.
      DO ISky = 1,4
        HorIllSky(ISky) = WeightNow * GILSK(ISky,HourOfDay) + WeightPreviousHour * GILSK(ISky,PreviousHour) + 0.001d0
      END DO

      ! HISKF is current time step horizontal illuminance from sky, calculated in DayltgLuminousEfficacy,
      ! which is called in WeatherManager. HISUNF is current time step horizontal illuminance from sun,
      ! also calculated in DayltgLuminousEfficacy.
      HorIllSkyFac = HISKF/((1.d0-SkyWeight)*HorIllSky(ISky2) + SkyWeight*HorIllSky(ISky1))

      DO IS = 1,2
        IF(IS == 2.AND.SurfaceWindow(IWin)%ShadingFlag<=0.AND..not.SurfaceWindow(IWin)%SolarDiffusing) EXIT

        ZoneDaylight(ZoneNum)%IllumFromWinAtMapPt(IL,IS,loop) = &
          DFSUHR(IS)*HISUNF + HorIllSkyFac * (DFSKHR(ISky1,IS)*SkyWeight*HorIllSky(ISky1) + &
                              DFSKHR(ISky2,IS)*(1.-SkyWeight)*HorIllSky(ISky2))

        ZoneDaylight(ZoneNum)%BackLumFromWinAtMapPt(IL,IS,loop) = &
          BFSUHR(IS)*HISUNF + HorIllSkyFac * (BFSKHR(ISky1,IS)*SkyWeight*HorIllSky(ISky1) + &
                              BFSKHR(ISky2,IS)*(1.-SkyWeight)*HorIllSky(ISky2))

        ZoneDaylight(ZoneNum)%SourceLumFromWinAtMapPt(IL,IS,loop) = &
          SFSUHR(IS)*HISUNF + HorIllSkyFac * (SFSKHR(ISky1,IS)*SkyWeight*HorIllSky(ISky1) + &
                              SFSKHR(ISky2,IS)*(1.-SkyWeight)*HorIllSky(ISky2))
        ZoneDaylight(ZoneNum)%SourceLumFromWinAtMapPt(IL,IS,loop) = &
            MAX(ZoneDaylight(ZoneNum)%SourceLumFromWinAtMapPt(IL,IS,loop),0.0d0)
      ENDDO

 3000 CONTINUE  ! End of reference point loop
 2000 CONTINUE  ! End of first loop over windows

!              Second loop over windows. Find total daylight illuminance
!              and background luminance for each ref pt from all windows in
!              the space.  Use shading flags.

      DO loop = 1,ZoneDaylight(ZoneNum)%NumOfDayltgExtWins
        IWin = ZoneDaylight(ZoneNum)%DayltgExtWinSurfNums(loop)

        IS = 1
        IF ((SurfaceWindow(IWin)%ShadingFlag >= 1 .AND. SurfaceWindow(IWin)%ShadingFlag <= 9) .OR. &
             SurfaceWindow(IWin)%SolarDiffusing) IS = 2

        ! CR 8057. 3/17/2010.
        ! Switchable windows may be in partially switched state rather than fully dark state
        VTMULT = 1.0d0

        ICtrl = Surface(IWin)%WindowShadingControlPtr
        IF(ICtrl > 0) THEN
          IF (WindowShadingControl(ICtrl)%ShadingControlType == WSCT_MeetDaylIlumSetp .AND. &
              SurfaceWindow(IWin)%ShadingFlag == SwitchableGlazing) THEN
            ! switchable windows in partial or fully switched state,
            !  get its intermediate VT calculated in DayltgInteriorIllum
            IConstShaded = Surface(IWin)%ShadedConstruction
            IF (IConstShaded > 0) VTDark = POLYF(1.0d0,Construct(IConstShaded)%TransVisBeamCoef(1))* &
                                           SurfaceWindow(IWin)%GlazedFrac
            IF (VTDark > 0) VTMULT = SurfaceWindow(IWin)%VisTransSelected / VTDark
          ENDIF
        ENDIF

        DO IL = REFPT1,NREFPT
!              Determine if illuminance contribution is from bare or shaded window
          DaylIllum(IL) = DaylIllum(IL) + VTMULT * ZoneDaylight(ZoneNum)%IllumFromWinAtMapPt(IL,IS,loop)
          BACLUM(IL) = BACLUM(IL) + VTMULT * ZoneDaylight(ZoneNum)%BackLumFromWinAtMapPt(IL,IS,loop)
        END DO

      END DO  ! End of second window loop

!              Calculate glare index at each reference point
      DO IL = REFPT1,NREFPT
!        Following code taken directly from DayltgGlare ... duplicate calculation
         ! Initialize glare constant
        GTOT = 0.0d0

        ! Loop over exterior windows associated with zone
        DO loop = 1,ZoneDaylight(ZoneNum)%NumOfDayltgExtWins
          IWin = ZoneDaylight(ZoneNum)%DayltgExtWinSurfNums(loop)
          IS = 1
          IF((SurfaceWindow(IWin)%ShadingFlag >= 1 .AND. SurfaceWindow(IWin)%ShadingFlag <= 9) .OR. &
              SurfaceWindow(IWin)%SolarDiffusing) IS = 2

          ! CR 8057. 3/17/2010
          VTMULT = 1.0d0

          ICtrl = Surface(IWin)%WindowShadingControlPtr
          IF(ICtrl > 0) THEN
            IF (WindowShadingControl(ICtrl)%ShadingControlType == WSCT_MeetDaylIlumSetp .AND. &
                SurfaceWindow(IWin)%ShadingFlag == SwitchableGlazing) THEN
              ! switchable windows in partial or fully switched state,
              !  get its intermediate VT calculated in DayltgInteriorIllum
              IConstShaded = Surface(IWin)%ShadedConstruction
              IF (IConstShaded > 0) VTDark = POLYF(1.0d0,Construct(IConstShaded)%TransVisBeamCoef(1))* &
                                             SurfaceWindow(IWin)%GlazedFrac
              IF (VTDark > 0) VTMULT = SurfaceWindow(IWin)%VisTransSelected / VTDark
            ENDIF
          ENDIF

          ! Conversion from ft-L to cd/m2, with cd/m2 = 0.2936 ft-L, gives the 0.4794 factor
          ! below, which is (0.2936)**0.6
          GTOT1  = 0.4794d0*((VTMULT * ZoneDaylight(ZoneNum)%SourceLumFromWinAtMapPt(IL,IS,loop))**1.6d0) *   &
                          ZoneDaylight(ZoneNum)%SolidAngAtMapPtWtd(IL,loop)**0.8d0
          GTOT2  = BACLUM(IL) + 0.07d0 * (ZoneDaylight(ZoneNum)%SolidAngAtMapPt(IL,loop)**0.5d0) *  &
                          VTMULT * ZoneDaylight(ZoneNum)%SourceLumFromWinAtMapPt(IL,IS,loop)
          GTOT = GTOT + GTOT1 / (GTOT2 + 0.000001d0)
        END DO

        ! Glare index (adding 0.000001 prevents LOG10 (0))
        GLRNDX(IL) = 10.0d0*LOG10(GTOT+0.000001d0)
        ! Set glare index to zero for GTOT < 1
        GLRNDX(IL) = MAX(0.0d0, GLRNDX(IL))
      ENDDO

!              Variables for reporting
      DO IL = REFPT1,NREFPT
        ZoneDaylight(ZoneNum)%DaylIllumAtMapPt(IL)  = DaylIllum(IL)
        ZoneDaylight(ZoneNum)%GlareIndexAtMapPt(IL) = GLRNDX(IL)
      ENDDO


      RETURN
END SUBROUTINE DayltgInteriorMapIllum


SUBROUTINE ReportIllumMap(MapNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Ellis
          !       DATE WRITTEN   May 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine produces the Daylighting Illuminance Map output.  Each separate map (by zone)
          ! is placed on a temporary file and later (see CloseReportIllumMaps) coallesced into a single
          ! output file.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits
  USE DataStringGlobals, ONLY: CharTab, CharComma, CharSpace
! BSLLC Start
  USE SQLiteProcedures
! BSLLC Finish

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: MapNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: FmtA="(A)"
  CHARACTER(len=*), PARAMETER :: HrFmt="(I2.2)"

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER, EXTERNAL   :: GetNewUnitNumber
  CHARACTER(len=100)  :: string
  INTEGER             :: RefPt
  INTEGER             :: X, Y
  INTEGER             :: R
  REAL(r64)           :: NumOut
  INTEGER             :: IllumOut

  LOGICAL, SAVE       :: FirstTime = .TRUE.
  LOGICAL, SAVE, ALLOCATABLE, DIMENSION(:) :: FirstTimeMaps
  LOGICAL, SAVE, ALLOCATABLE, DIMENSION(:) :: EnvrnPrint
  CHARACTER(len=5), SAVE, ALLOCATABLE, DIMENSION(:) :: SavedMnDy
  CHARACTER(len=50), ALLOCATABLE, DIMENSION(:,:), SAVE :: RefPts
  CHARACTER(len=15) MapNoString
  CHARACTER(len=2) HrString
  INTEGER linelen
  CHARACTER(len=50) AddXorYString
! BSLLC Start
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: XValue
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE :: YValue
  REAL(r64), ALLOCATABLE, DIMENSION(:,:),SAVE  :: IllumValue
  INTEGER :: SQMonth
  INTEGER :: SQDayOfMonth
  INTEGER :: IllumIndex
  LOGICAL, SAVE :: SQFirstTime=.true.
  LOGICAL, SAVE :: CommaDelimited=.true.
! BSLLC Finish

          ! FLOW:
  IF (FirstTime) THEN
    FirstTime = .FALSE.
    ALLOCATE (FirstTimeMaps(TotIllumMaps))
    FirstTimeMaps=.true.
    ALLOCATE (EnvrnPrint(TotIllumMaps))
    EnvrnPrint=.true.
    ALLOCATE(RefPts(MaxRefPoints,NumOfZones))
    RefPts=' '
    ALLOCATE(SavedMnDy(TotIllumMaps))
    SavedMnDy=' '
  ENDIF

  IF (FirstTimeMaps(MapNum)) THEN

    FirstTimeMaps(MapNum)=.false.
    IllumMap(MapNum)%UnitNo=GetNewUnitNumber()
    WRITE(MapNoString,*) MapNum
    MapNoString=ADJUSTL(MapNoString)
    IF (MapColSep == CharTab) THEN
      OPEN(UNIT=IllumMap(MapNum)%UnitNo,FILE='eplusmap.tab'//TRIM(MapNoString),STATUS='UNKNOWN',ACTION='readwrite',ERR=901)
      CommaDelimited=.false.
    ELSEIF (MapColSep == CharComma) THEN
      OPEN(UNIT=IllumMap(MapNum)%UnitNo,FILE='eplusmap.csv'//TRIM(MapNoString),STATUS='UNKNOWN',ACTION='readwrite',ERR=902)
      CommaDelimited=.true.
    ELSE
      OPEN(UNIT=IllumMap(MapNum)%UnitNo,FILE='eplusmap.txt'//TRIM(MapNoString),STATUS='UNKNOWN',ACTION='readwrite',ERR=903)
      CommaDelimited=.false.
    ENDIF

    SavedMnDy(MapNum)=CurMnDyHr(1:5)

    IllumMap(MapNum)%Name=trim(IllumMap(MapNum)%Name)//' at '//trim(RoundSigDigits(IllumMap(MapNum)%Z,2))//'m'

    DO R=1,ZoneDaylight(IllumMap(MapNum)%Zone)%TotalDaylRefPoints
      WRITE(String,*) R
      String=ADJUSTL(String)
      RefPts(R,IllumMap(MapNum)%Zone)='RefPt'//TRIM(String)//'=('
      String=RoundSigDigits(ZoneDaylight(IllumMap(MapNum)%Zone)%DaylRefPtAbsCoord(R,1),2)
      RefPts(R,IllumMap(MapNum)%Zone)=TRIM(RefPts(R,IllumMap(MapNum)%Zone))//TRIM(String)//':'
      String=RoundSigDigits(ZoneDaylight(IllumMap(MapNum)%Zone)%DaylRefPtAbsCoord(R,2),2)
      RefPts(R,IllumMap(MapNum)%Zone)=TRIM(RefPts(R,IllumMap(MapNum)%Zone))//TRIM(String)//':'
      String=RoundSigDigits(ZoneDaylight(IllumMap(MapNum)%Zone)%DaylRefPtAbsCoord(R,3),2)
      RefPts(R,IllumMap(MapNum)%Zone)=TRIM(RefPts(R,IllumMap(MapNum)%Zone))//TRIM(String)//')'
    ENDDO
    ! could have multiple maps for the same zone!!!
  END IF
  IF (SavedMnDy(MapNum) /= CurMnDyHr(1:5)) THEN
    EnvrnPrint(MapNum)=.true.
    SavedMnDy(MapNum)=CurMnDyHr(1:5)
  ENDIF
  IF (EnvrnPrint(MapNum)) THEN
! BSLLC Start
    CALL WriteDaylightMapTitle (MapNum, IllumMap(MapNum)%UnitNo, IllumMap(MapNum)%Name, &
        EnvironmentName, IllumMap(MapNum)%Zone, RefPts(1,IllumMap(MapNum)%Zone), &
        RefPts(2,IllumMap(MapNum)%Zone),IllumMap(MapNum)%Z)
! BSLLC Finish
    EnvrnPrint(MapNum)=.false.
  ENDIF

  IF (.NOT. WarmUpFlag) THEN
    IF (TimeStep == NumOfTimeStepInHour) THEN ! Report only hourly

      ! Write X scale column header
      WRITE(HrString,HrFmt) HourOfDay
      mapLine=' '//TRIM(SavedMnDy(MapNum))//' '//TRIM(HrString)//':00'
      IF (IllumMap(MapNum)%HeaderXLineLengthNeeded) &
           linelen=len_trim(mapLine)
      RefPt=1
      DO X = 1, IllumMap(MapNum)%Xnum
         AddXorYString=MapColSep//'('//  &
           trim(RoundSigDigits(ZoneDaylight(IllumMap(MapNum)%Zone)%MapRefPtAbsCoord(RefPt,1),2))//';'//  &
           trim(RoundSigDigits(ZoneDaylight(IllumMap(MapNum)%Zone)%MapRefPtAbsCoord(RefPt,2),2))//')='
         IF (IllumMap(MapNum)%HeaderXLineLengthNeeded) &
           linelen=linelen+len_trim(AddXorYString)
         mapLine = TRIM(mapLine)//AddXorYString
         RefPt=RefPt+1
      END DO ! X

      IF (IllumMap(MapNum)%HeaderXLineLengthNeeded) THEN
        IllumMap(MapNum)%HeaderXLineLength=linelen
        IF (IllumMap(MapNum)%HeaderXLineLength > len(mapLine)) THEN
          CALL ShowWarningError('ReportIllumMap: Map="'//trim(IllumMap(MapNum)%Name)//  &
             '" -- the X Header overflows buffer -- will be truncated at '//  &
             trim(RoundSigDigits(len(MapLine)))//' characters.')
          CALL ShowContinueError('...needed '//trim(RoundSigDigits(IllumMap(MapNum)%HeaderXLineLength))// &
            ' characters. Please contact EnergyPlus support.')
        ENDIF
        IllumMap(MapNum)%HeaderXLineLengthNeeded=.false.
      ENDIF

      WRITE(IllumMap(MapNum)%UnitNo,FmtA) TRIM(mapLine)

      ! Write Y scale prefix and illuminance values
      RefPt = 1
      DO Y = 1, IllumMap(MapNum)%Ynum
        mapLine='('//trim(RoundSigDigits(ZoneDaylight(IllumMap(MapNum)%Zone)%MapRefPtAbsCoord(RefPt,1),2))//';'//  &
             trim(RoundSigDigits(ZoneDaylight(IllumMap(MapNum)%Zone)%MapRefPtAbsCoord(RefPt,2),2))//')='
        DO R=RefPt,RefPt+IllumMap(MapNum)%Xnum-1
          IllumOut=NINT(ZoneDaylight(IllumMap(MapNum)%Zone)%DaylIllumAtMapPtHr(R))
          IF (ZoneDayLight(IllumMap(MapNum)%Zone)%MapRefPtInBounds(R)) THEN
            WRITE(String,*) IllumOut
          ELSE
            WRITE(String,*) IllumOut
            String='*'//ADJUSTL(String)
          ENDIF
          mapLine=TRIM(mapLine)//MapColSep//ADJUSTL(String)
        ENDDO

        WRITE(IllumMap(MapNum)%UnitNo,FmtA) TRIM(mapLine)

        RefPt = RefPt + IllumMap(MapNum)%Xnum
      END DO ! X

      IF (WriteOutputToSQLite) THEN
        IF (SQFirstTime) THEN
            ALLOCATE (XValue(MAXVAL(IllumMap(1:TotIllumMaps)%Xnum)))
            ALLOCATE (YValue(MAXVAL(IllumMap(1:TotIllumMaps)%Ynum)))
            ALLOCATE (IllumValue(MAXVAL(IllumMap(1:TotIllumMaps)%Xnum), MAXVAL(IllumMap(1:TotIllumMaps)%Ynum)))
          SQFirstTime=.false.
        ENDIF

        SQMonth=Month
        SQDayOfMonth=DayOfMonth

        DO Y = 1, IllumMap(MapNum)%Ynum
            YValue(Y) = IllumMap(MapNum)%Ymin + (Y - 1)*IllumMap(MapNum)%Yinc
            DO X = 1, IllumMap(MapNum)%Xnum
                XValue(X) = IllumMap(MapNum)%Xmin + (X - 1)*IllumMap(MapNum)%Xinc
                IllumIndex = X + (Y - 1)*IllumMap(MapNum)%Xnum
                IllumValue(X,Y) = NINT(ZoneDaylight(IllumMap(MapNum)%Zone)%DaylIllumAtMapPtHr(IllumIndex))
                IF (.NOT. ZoneDayLight(IllumMap(MapNum)%Zone)%MapRefPtInBounds(IllumIndex)) THEN
                    IllumValue(X,Y) = -IllumValue(X,Y)
                ENDIF
            END DO ! X Loop
        END DO ! Y Loop

        CALL CreateSQLiteDaylightMap (MapNum, SQMonth, SQDayOfMonth, HourOfDay, &
            IllumMap(MapNum)%Xnum, XValue, IllumMap(MapNum)%Ynum, YValue, IllumValue)

      END IF ! WriteOutputToSQLite
    END IF ! end time step
  END IF ! not Warmup

  RETURN

  901 CALL ShowFatalError('ReportIllumMap: Could not open file "eplusmap.tab'//TRIM(MapNoString)//'" for output (write).')
  RETURN

  902 CALL ShowFatalError('ReportIllumMap: Could not open file "eplusmap.csv'//TRIM(MapNoString)//'" for output (write).')
  RETURN

  903 CALL ShowFatalError('ReportIllumMap: Could not open file "eplusmap.txt'//TRIM(MapNoString)//'" for output (write).')
  RETURN

END SUBROUTINE ReportIllumMap

SUBROUTINE CloseReportIllumMaps

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   June 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine "closes" out the created daylight illuminance maps by merging them
          ! into the "eplusout.map" file.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataStringGlobals, ONLY: CharTab, CharComma, CharSpace
  USE General, ONLY: TrimSigDigits
  USE DataErrorTracking, ONLY: AbortProcessing

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: FmtA="(A)"

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER, SAVE       :: MapOutputFile
  INTEGER, EXTERNAL   :: GetNewUnitNumber
  INTEGER :: MapNum
  INTEGER :: ios=0
  INTEGER :: NumLines

  IF (TotIllumMaps > 0) THEN

    MapOutputFile=GetNewUnitNumber()  ! can add this to DataGlobals with the others...

    ! Write map header
    IF (MapColSep == CharTab) THEN
      OPEN(UNIT=MapOutputFile,FILE='eplusmap.tab',STATUS='UNKNOWN',ACTION='write',ERR=901)
    ELSEIF (MapColSep == CharComma) THEN
      OPEN(UNIT=MapOutputFile,FILE='eplusmap.csv',STATUS='UNKNOWN',ACTION='write',ERR=902)
    ELSE
      OPEN(UNIT=MapOutputFile,FILE='eplusmap.txt',STATUS='UNKNOWN',ACTION='write',ERR=903)
    ENDIF

    DO MapNum=1,TotIllumMaps
      IF (IllumMap(MapNum)%UnitNo == 0) CYCLE  ! fatal error processing
      NumLines=0
      REWIND(IllumMap(MapNum)%UnitNo)
      ios=0
      DO WHILE (ios == 0)
        READ(IllumMap(MapNum)%UnitNo,FmtA,IOSTAT=ios) mapLine
        IF (ios > 0) THEN  ! usually a read error
          CALL ShowFatalError('CloseReportIllumMaps: Failed to read map. IOError='//trim(TrimSigDigits(ios)))
        ELSEIF (ios /= 0) THEN
          IF (NumLines == 0) THEN
            CALL ShowSevereError('CloseReportIllumMaps: IllumMap="'//trim(IllumMap(MapNum)%Name)//'" is empty.')
          ENDIF
          EXIT
        ENDIF
        NumLines=NumLines+1
        WRITE(MapOutputFile,FmtA) TRIM(mapLine)
      ENDDO
      CLOSE(IllumMap(MapNum)%UnitNo,STATUS='DELETE')
    ENDDO

    IF (.not. mapResultsReported .and. .not. AbortProcessing) THEN
      CALL ShowSevereError('CloseReportIllumMaps: Illuminance maps requested but no data ever reported. '//  &
         'Likely cause is no solar.')
      WRITE(MapOutputFile,FmtA) 'CloseReportIllumMaps: Illuminance maps requested but no data ever reported. '//  &
         'Likely cause is no solar.'
    ENDIF

    CLOSE(MapOutputFile)

  ENDIF

  RETURN

  901 CALL ShowFatalError('CloseReportIllumMaps: Could not open file "eplusmap.tab" for output (write).')
  RETURN

  902 CALL ShowFatalError('CloseReportIllumMaps: Could not open file "eplusmap.csv" for output (write).')
  RETURN

  903 CALL ShowFatalError('CloseReportIllumMaps: Could not open file "eplusmap.txt" for output (write).')
  RETURN

END SUBROUTINE CloseReportIllumMaps

SUBROUTINE CloseDFSFile

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   August 2010
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Make sure DFSFile is closed at exit time.  Do not rely on operating system to
          ! take care of it.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  IF (OutputFileDFS > 0) CLOSE(OutputFileDFS)

  RETURN

END SUBROUTINE CloseDFSFile

SUBROUTINE DayltgSetupAdjZoneListsAndPointers

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   Feb. 2004
          !       MODIFIED:      June 2010;LKL - Merged two routines.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! For each Daylighting:Detailed zone, Z, creates a list of other zones, Zadj,
          ! that have one or more exterior windows and that share one or more interior
          ! windows with Z. Used in calculation of daylighting through interior windows.

          ! Sets the daylighting factor pointers for each Daylighting:Detailed zone. The pointer
          ! may be associated with an exterior window in a daylit target zone or an exterior window in
          ! an adjacent zone, daylit or not, that shares interior windows with the target zone.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER  :: ZoneNum            ! Zone number
  INTEGER  :: NumList            ! Counter of adjacent zone numbers
  INTEGER  :: ZoneNumAdj         ! Zone number
  LOGICAL  :: AdjZoneHasExtWins  ! True if adjacent zone has one or more exterior windows
  INTEGER  :: SurfNumAdj         ! Surface number
  INTEGER  :: SurfNumAdj2         ! Surface number
  INTEGER  :: ExtWinIndex
  INTEGER  :: IntWinIndex
  INTEGER  :: ZoneAdjLoop
  INTEGER  :: NumOfIntWindowsCount
  INTEGER  :: DayltgFacPtr       ! Daylighting factor pointer
  INTEGER  :: ZoneExtWinCtr      ! Exterior window counter
  INTEGER  :: SurfNum            ! Surface number
  INTEGER  :: loop               ! DO loop index
  INTEGER, ALLOCATABLE, DIMENSION(:) :: ZoneExtWin
  INTEGER  :: Winsize
  INTEGER  :: Refsize

          ! FLOW:
! Count number of exterior Windows (use to allocate arrays)


          ! FLOW:

DO ZoneNum = 1,NumOfZones
  ! Count exterior windows in this zone
    DO SurfNum = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
      IF((Surface(SurfNum)%Class == SurfaceClass_Window .AND. Surface(SurfNum)%ExtBoundCond == ExternalEnvironment) .OR. &
             SurfaceWindow(SurfNum)%OriginalClass == SurfaceClass_TDD_Diffuser) THEN
        ZoneDaylight(ZoneNum)%TotalExtWindows = ZoneDaylight(ZoneNum)%TotalExtWindows + 1
      END IF
    END DO
ENDDO

DO ZoneNum = 1,NumOfZones
  NumList = 0
  IF(ZoneDaylight(ZoneNum)%TotalDaylRefPoints == 0) CYCLE
  ! This is a Daylighting:Detailed zone
  ! Find adjacent zones
  DO ZoneNumAdj = 1,NumOfZones
    IF(ZoneNumAdj == ZoneNum) CYCLE
    ! Require that ZoneNumAdj have a least one exterior window
    AdjZoneHasExtWins = .FALSE.
    DO SurfNumAdj = Zone(ZoneNumAdj)%SurfaceFirst,Zone(ZoneNumAdj)%SurfaceLast
      IF(Surface(SurfNumAdj)%Class == SurfaceClass_Window .AND. Surface(SurfNumAdj)%ExtBoundCond == ExternalEnvironment) THEN
        AdjZoneHasExtWins = .TRUE.
        EXIT
      END iF
    END DO
    IF(.NOT.AdjZoneHasExtWins) CYCLE
    ! Loop again through surfaces in ZoneNumAdj and see if any are interior windows adjacent to ZoneNum
    DO SurfNumAdj = Zone(ZoneNumAdj)%SurfaceFirst,Zone(ZoneNumAdj)%SurfaceLast
      IF(Surface(SurfNumAdj)%Class == SurfaceClass_Window .AND. Surface(SurfNumAdj)%ExtBoundCond >= 1) THEN
        ! This is an interior window in ZoneNumAdj
        IF(Surface(Surface(SurfNumAdj)%ExtBoundCond)%Zone == ZoneNum) THEN
          ! This interior window is adjacent to ZoneNum
          NumList = NumList + 1
          EXIT
        END IF
      END If
    END DO
  END DO
  ALLOCATE(ZoneDaylight(ZoneNum)%AdjIntWinZoneNums(NumList))
  ZoneDaylight(ZoneNum)%AdjIntWinZoneNums=0
END DO

DO ZoneNum = 1,NumOfZones
  NumList = 0
  IF(ZoneDaylight(ZoneNum)%TotalDaylRefPoints == 0) CYCLE
  ! This is a Daylighting:Detailed zone
  ! Find adjacent zones
  DO ZoneNumAdj = 1,NumOfZones
    IF(ZoneNumAdj == ZoneNum) CYCLE
    ! Require that ZoneNumAdj have a least one exterior window
    AdjZoneHasExtWins = .FALSE.
    DO SurfNumAdj = Zone(ZoneNumAdj)%SurfaceFirst,Zone(ZoneNumAdj)%SurfaceLast
      IF(Surface(SurfNumAdj)%Class == SurfaceClass_Window .AND. Surface(SurfNumAdj)%ExtBoundCond == ExternalEnvironment) THEN
        AdjZoneHasExtWins = .TRUE.
        EXIT
      END iF
    END DO
    IF(.NOT.AdjZoneHasExtWins) CYCLE
    ! Loop again through surfaces in ZoneNumAdj and see if any are interior windows adjacent to ZoneNum
    DO SurfNumAdj = Zone(ZoneNumAdj)%SurfaceFirst,Zone(ZoneNumAdj)%SurfaceLast
      IF(Surface(SurfNumAdj)%Class == SurfaceClass_Window .AND. Surface(SurfNumAdj)%ExtBoundCond >= 1) THEN
        ! This is an interior window in ZoneNumAdj
        IF(Surface(Surface(SurfNumAdj)%ExtBoundCond)%Zone == ZoneNum) THEN
          ! This interior window is adjacent to ZoneNum
          NumList = NumList + 1
          ZoneDaylight(ZoneNum)%AdjIntWinZoneNums(NumList) = ZoneNumAdj
          ZoneDaylight(ZoneNumAdj)%AdjZoneHasDayltgCtrl = .True.
          EXIT
        END IF
      END If
    END DO
  END DO
  ZoneDaylight(ZoneNum)%NumOfIntWinAdjZones = NumList
END DO

! now fill out information on relationship between adjacent exterior windows and associated interior windows
DO ZoneNum = 1,NumOfZones
    ! first find count of exterior windows
  IF (ZoneDaylight(ZoneNum)%NumOfIntWinAdjZones <= 0) THEN
    ZoneDaylight(ZoneNum)%NumOfIntWinAdjZoneExtWins = 0
    CYCLE
  ENDIF
  DO ZoneAdjLoop = 1, ZoneDaylight(ZoneNum)%NumOfIntWinAdjZones
    ZoneNumAdj = ZoneDaylight(ZoneNum)%AdjIntWinZoneNums(ZoneAdjLoop)
    DO SurfNumAdj = Zone(ZoneNumAdj)%SurfaceFirst,Zone(ZoneNumAdj)%SurfaceLast
      IF(Surface(SurfNumAdj)%Class == SurfaceClass_Window .AND. Surface(SurfNumAdj)%ExtBoundCond == ExternalEnvironment) THEN
        ZoneDaylight(ZoneNum)%NumOfIntWinAdjZoneExtWins = ZoneDaylight(ZoneNum)%NumOfIntWinAdjZoneExtWins + 1
      ENDIF
    ENDDO
  ENDDO
  ! now allocate nested struct based on exterior window count
  ALLOCATE(ZoneDaylight(ZoneNum)%IntWinAdjZoneExtWin(ZoneDaylight(ZoneNum)%NumOfIntWinAdjZoneExtWins))

  ! now fill nested structure
  ExtWinIndex = 0
  DO ZoneAdjLoop = 1, ZoneDaylight(ZoneNum)%NumOfIntWinAdjZones
    ZoneNumAdj = ZoneDaylight(ZoneNum)%AdjIntWinZoneNums(ZoneAdjLoop)
    DO SurfNumAdj = Zone(ZoneNumAdj)%SurfaceFirst,Zone(ZoneNumAdj)%SurfaceLast
      IF(Surface(SurfNumAdj)%Class == SurfaceClass_Window .AND. Surface(SurfNumAdj)%ExtBoundCond == ExternalEnvironment) THEN
        ExtWinIndex = ExtWinIndex + 1
        ZoneDaylight(ZoneNum)%IntWinAdjZoneExtWin(ExtWinIndex)%SurfNum = SurfNumAdj

        ! now count interior windows shared by both zones
        NumOfIntWindowsCount = 0
        DO SurfNumAdj2 = Zone(ZoneNumAdj)%SurfaceFirst,Zone(ZoneNumAdj)%SurfaceLast
          IF(Surface(SurfNumAdj2)%Class == SurfaceClass_Window .AND. Surface(SurfNumAdj2)%ExtBoundCond >= 1) THEN
            ! This is an interior window in ZoneNumAdj
            IF(Surface(Surface(SurfNumAdj2)%ExtBoundCond)%Zone == ZoneNum) THEN
              ! This interior window is adjacent to ZoneNum and associated with this
              NumOfIntWindowsCount = NumOfIntWindowsCount + 1
            ENDIF
          ENDIF
        ENDDO
        ! allocate nested array
        ALLOCATE(ZoneDaylight(ZoneNum)%IntWinAdjZoneExtWin(ExtWinIndex)%IntWinNum(NumOfIntWindowsCount))
        ZoneDaylight(ZoneNum)%IntWinAdjZoneExtWin(ExtWinIndex)%IntWinNum=0
        IntWinIndex = 0
        DO SurfNumAdj2 = Zone(ZoneNumAdj)%SurfaceFirst,Zone(ZoneNumAdj)%SurfaceLast
          IF(Surface(SurfNumAdj2)%Class == SurfaceClass_Window .AND. Surface(SurfNumAdj2)%ExtBoundCond >= 1) THEN
            ! This is an interior window in ZoneNumAdj
            IF(Surface(Surface(SurfNumAdj2)%ExtBoundCond)%Zone == ZoneNum) THEN
              ! This interior window is adjacent to ZoneNum and associated with this
              IntWinIndex = IntWinIndex + 1
              ZoneDaylight(ZoneNum)%IntWinAdjZoneExtWin(ExtWinIndex)%IntWinNum(IntWinIndex) = SurfNumAdj2
            ENDIF
          ENDIF
        ENDDO
      ENDIF
    ENDDO
  ENDDO

ENDDO

ALLOCATE(ZoneExtWin(NumOfZones))
ZoneExtWin=0

DO ZoneNum = 1,NumOfZones
  IF(ZoneDaylight(ZoneNum)%TotalDaylRefPoints > 0) THEN
    ! This is a Daylighting:Detailed zone

    ! Get exterior windows in this zone
    DO SurfNum = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
      IF((Surface(SurfNum)%Class == SurfaceClass_Window .AND. Surface(SurfNum)%ExtBoundCond == ExternalEnvironment) .OR. &
             SurfaceWindow(SurfNum)%OriginalClass == SurfaceClass_TDD_Diffuser) THEN
        ZoneExtWin(ZoneNum) = ZoneExtWin(ZoneNum) + 1
      END IF
    END DO

    ! Get exterior windows in adjacent zones that share interior windows with ZoneNum
    IF(ZoneDaylight(ZoneNum)%NumOfIntWinAdjZones > 0) THEN
      DO loop = 1,ZoneDaylight(ZoneNum)%NumOfIntWinAdjZones
        ZoneNumAdj = ZoneDaylight(ZoneNum)%AdjIntWinZoneNums(loop)
        ! Get exterior windows in ZoneNumAdj -- there must be at least one, otherwise
        ! it would not be an "AdjIntWinZone"
        DO SurfNumAdj = Zone(ZoneNumAdj)%SurfaceFirst,Zone(ZoneNumAdj)%SurfaceLast
          IF((Surface(SurfNumAdj)%Class == SurfaceClass_Window .AND. Surface(SurfNumAdj)%ExtBoundCond == ExternalEnvironment) .OR. &
                 SurfaceWindow(SurfNumAdj)%OriginalClass == SurfaceClass_TDD_Diffuser) THEN
            ZoneExtWin(ZoneNum) = ZoneExtWin(ZoneNum) + 1
          END IF
        END DO
      END DO
    END IF

  END IF  ! End of check if a Daylighting:Detailed zone
END DO  ! End of primary zone loop

DayltgFacPtr = 0
DO ZoneNum = 1,NumOfZones
  ZoneDaylight(ZoneNum)%NumOfDayltgExtWins = 0
  IF(ZoneDaylight(ZoneNum)%TotalDaylRefPoints > 0) THEN
    ! This is a Daylighting:Detailed zone

    ! Get exterior windows in this zone
    IF (ZoneExtWin(ZoneNum) == 0) CYCLE
    ALLOCATE(ZoneDayLight(ZoneNum)%DayltgExtWinSurfNums(ZoneExtWin(ZoneNum)))
    ZoneDayLight(ZoneNum)%DayltgExtWinSurfNums=0
    ALLOCATE(ZoneDaylight(ZoneNum)%DayltgFacPtrsForExtWins(ZoneExtWin(ZoneNum)))
    ZoneDaylight(ZoneNum)%DayltgFacPtrsForExtWins=0

    ALLOCATE(ZoneDaylight(ZoneNum)%SolidAngAtRefPt(ZoneDaylight(ZoneNum)%TotalDaylRefPoints,ZoneExtWin(ZoneNum)))
    ZoneDaylight(ZoneNum)%SolidAngAtRefPt=0.0
    ALLOCATE(ZoneDaylight(ZoneNum)%SolidAngAtRefPtWtd(ZoneDaylight(ZoneNum)%TotalDaylRefPoints,ZoneExtWin(ZoneNum)))
    ZoneDaylight(ZoneNum)%SolidAngAtRefPtWtd=0.0
    ALLOCATE(ZoneDaylight(ZoneNum)%IllumFromWinAtRefPt(ZoneDaylight(ZoneNum)%TotalDaylRefPoints,2,ZoneExtWin(ZoneNum)))
    ZoneDaylight(ZoneNum)%IllumFromWinAtRefPt=0.0
    ALLOCATE(ZoneDaylight(ZoneNum)%BackLumFromWinAtRefPt(ZoneDaylight(ZoneNum)%TotalDaylRefPoints,2,ZoneExtWin(ZoneNum)))
    ZoneDaylight(ZoneNum)%BackLumFromWinAtRefPt=0.0
    ALLOCATE(ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt(ZoneDaylight(ZoneNum)%TotalDaylRefPoints,2,ZoneExtWin(ZoneNum)))
    ZoneDaylight(ZoneNum)%SourceLumFromWinAtRefPt=0.0

    IF (ZoneDaylight(ZoneNum)%TotalMapRefPoints > 0) THEN
      ! might be able to use TotalMapRefPoints for zone in below.
      ALLOCATE(ZoneDaylight(ZoneNum)%SolidAngAtMapPt(ZoneDaylight(ZoneNum)%TotalMapRefPoints,ZoneExtWin(ZoneNum)))
      ZoneDaylight(ZoneNum)%SolidAngAtMapPt=0.0
      ALLOCATE(ZoneDaylight(ZoneNum)%SolidAngAtMapPtWtd(ZoneDaylight(ZoneNum)%TotalMapRefPoints,ZoneExtWin(ZoneNum)))
      ZoneDaylight(ZoneNum)%SolidAngAtMapPtWtd=0.0
      ALLOCATE(ZoneDaylight(ZoneNum)%IllumFromWinAtMapPt(ZoneDaylight(ZoneNum)%TotalMapRefPoints,2,ZoneExtWin(ZoneNum)))
      ZoneDaylight(ZoneNum)%IllumFromWinAtMapPt=0.0
      ALLOCATE(ZoneDaylight(ZoneNum)%BackLumFromWinAtMapPt(ZoneDaylight(ZoneNum)%TotalMapRefPoints,2,ZoneExtWin(ZoneNum)))
      ZoneDaylight(ZoneNum)%BackLumFromWinAtMapPt=0.0
      ALLOCATE(ZoneDaylight(ZoneNum)%SourceLumFromWinAtMapPt(ZoneDaylight(ZoneNum)%TotalMapRefPoints,2,ZoneExtWin(ZoneNum)))
      ZoneDaylight(ZoneNum)%SourceLumFromWinAtMapPt=0.0
    ENDIF

    ZoneExtWinCtr = 0

    DO SurfNum = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
      IF((Surface(SurfNum)%Class == SurfaceClass_Window .AND. Surface(SurfNum)%ExtBoundCond == ExternalEnvironment) .OR. &
             SurfaceWindow(SurfNum)%OriginalClass == SurfaceClass_TDD_Diffuser) THEN
        ZoneExtWinCtr = ZoneExtWinCtr + 1
        DayltgFacPtr = DayltgFacPtr + 1
        ZoneDaylight(ZoneNum)%DayltgExtWinSurfNums(ZoneExtWinCtr) = SurfNum
        ZoneDaylight(ZoneNum)%DayltgFacPtrsForExtWins(ZoneExtWinCtr) = DayltgFacPtr
      END IF
    END DO

    ! Get exterior windows in adjacent zones that share interior windows with ZoneNum
    IF(ZoneDaylight(ZoneNum)%NumOfIntWinAdjZones > 0) THEN
      DO loop = 1,ZoneDaylight(ZoneNum)%NumOfIntWinAdjZones
        ZoneNumAdj = ZoneDaylight(ZoneNum)%AdjIntWinZoneNums(loop)
        ! Get exterior windows in ZoneNumAdj -- there must be at least one, otherwise
        ! it would not be an "AdjIntWinZone"
        DO SurfNumAdj = Zone(ZoneNumAdj)%SurfaceFirst,Zone(ZoneNumAdj)%SurfaceLast
          IF((Surface(SurfNumAdj)%Class == SurfaceClass_Window .AND. Surface(SurfNumAdj)%ExtBoundCond == ExternalEnvironment) .OR. &
                 SurfaceWindow(SurfNumAdj)%OriginalClass == SurfaceClass_TDD_Diffuser) THEN
            ZoneExtWinCtr = ZoneExtWinCtr + 1
            DayltgFacPtr = DayltgFacPtr + 1
            ZoneDaylight(ZoneNum)%DayltgExtWinSurfNums(ZoneExtWinCtr) = SurfNumAdj
            ZoneDaylight(ZoneNum)%DayltgFacPtrsForExtWins(ZoneExtWinCtr) = DayltgFacPtr

            ! If no daylighting in that zone, set up variables anyway:
            IF (ZoneDaylight(ZoneNumAdj)%TotalDaylRefPoints == 0) THEN
              IF (.not. SurfaceWindow(SurfNumAdj)%SurfDayLightInit) THEN
                ALLOCATE(SurfaceWindow(SurfNumAdj)%SolidAngAtRefPt(ZoneDaylight(ZoneNum)%TotalDaylRefPoints))
                SurfaceWindow(SurfNumAdj)%SolidAngAtRefPt=0.0
                ALLOCATE(SurfaceWindow(SurfNumAdj)%SolidAngAtRefPtWtd(ZoneDaylight(ZoneNum)%TotalDaylRefPoints))
                SurfaceWindow(SurfNumAdj)%SolidAngAtRefPtWtd=0.0
                ALLOCATE(SurfaceWindow(SurfNumAdj)%IllumFromWinAtRefPt(ZoneDaylight(ZoneNum)%TotalDaylRefPoints,2))
                SurfaceWindow(SurfNumAdj)%IllumFromWinAtRefPt=0.0
                ALLOCATE(SurfaceWindow(SurfNumAdj)%BackLumFromWinAtRefPt(ZoneDaylight(ZoneNum)%TotalDaylRefPoints,2))
                SurfaceWindow(SurfNumAdj)%BackLumFromWinAtRefPt=0.0
                ALLOCATE(SurfaceWindow(SurfNumAdj)%SourceLumFromWinAtRefPt(ZoneDaylight(ZoneNum)%TotalDaylRefPoints,2))
                SurfaceWindow(SurfNumAdj)%SourceLumFromWinAtRefPt=0.0
                SurfaceWindow(SurfNumAdj)%SurfDayLightInit=.true.
              ENDIF
            ENDIF
          END IF
        END DO
      END DO
    END IF

    ZoneDaylight(ZoneNum)%NumOfDayltgExtWins = ZoneExtWin(ZoneNum)
    WinSize=ZoneExtWin(ZoneNum)
    RefSize=2+ZoneDaylight(ZoneNum)%TotalMapRefPoints
    ALLOCATE(ZoneDaylight(ZoneNum)%DaylIllFacSky         (WinSize,RefSize,4,MaxSlatAngs+1,24))
    ALLOCATE(ZoneDaylight(ZoneNum)%DaylSourceFacSky      (WinSize,RefSize,4,MaxSlatAngs+1,24))
    ALLOCATE(ZoneDaylight(ZoneNum)%DaylBackFacSky        (WinSize,RefSize,4,MaxSlatAngs+1,24))
    ALLOCATE(ZoneDaylight(ZoneNum)%DaylIllFacSun         (WinSize,RefSize,MaxSlatAngs+1,24))
    ALLOCATE(ZoneDaylight(ZoneNum)%DaylIllFacSunDisk     (WinSize,RefSize,MaxSlatAngs+1,24))
    ALLOCATE(ZoneDaylight(ZoneNum)%DaylSourceFacSun      (WinSize,RefSize,MaxSlatAngs+1,24))
    ALLOCATE(ZoneDaylight(ZoneNum)%DaylSourceFacSunDisk  (WinSize,RefSize,MaxSlatAngs+1,24))
    ALLOCATE(ZoneDaylight(ZoneNum)%DaylBackFacSun        (WinSize,RefSize,MaxSlatAngs+1,24))
    ALLOCATE(ZoneDaylight(ZoneNum)%DaylBackFacSunDisk    (WinSize,RefSize,MaxSlatAngs+1,24))

  END IF  ! End of check if a Daylighting:Detailed zone
END DO  ! End of primary zone loop

WRITE(OutputFileInits,700)
700 Format('! <Zone/Window Adjacency Daylighting Counts>, Zone Name, ',  &
           'Number of Exterior Windows, Number of Exterior Windows in Adjacent Zones')
DO ZoneNum=1,NumOfZones
  IF(ZoneDaylight(ZoneNum)%TotalDaylRefPoints == 0) CYCLE
  WRITE(OutputFileInits,701) trim(Zone(ZoneNum)%Name),trim(RoundSigDigits(ZoneDaylight(ZoneNum)%TotalExtWindows)),  &
    trim(RoundSigDigits(ZoneDaylight(ZoneNum)%NumOfDayltgExtWins-ZoneDaylight(ZoneNum)%TotalExtWindows))
END DO
701 Format('Zone/Window Adjacency Daylighting Counts, ',A,',',A,',',A)

WRITE(OutputFileInits,702)
702 Format('! <Zone/Window Adjacency Daylighting Matrix>, Zone Name, Number of Adjacent Zones with Windows,',  &
           'Adjacent Zone Names - 1st 100 (max)')
DO ZoneNum=1,NumOfZones
  IF(ZoneDaylight(ZoneNum)%TotalDaylRefPoints == 0) CYCLE
  WRITE(OutputFileInits,703) trim(Zone(ZoneNum)%Name),trim(RoundSigDigits(ZoneDaylight(ZoneNum)%NumOfIntWinAdjZones)),  &
    (trim(Zone(ZoneDaylight(ZoneNum)%AdjIntWinZoneNums(loop))%Name),loop=1,MIN(ZoneDaylight(ZoneNum)%NumOfIntWinAdjZones,100))
END DO
703 Format('Zone/Window Adjacency Daylighting Matrix, ',A,',',A,100(',',A))

DEALLOCATE(ZoneExtWin)

RETURN
END SUBROUTINE DayltgSetupAdjZoneListsAndPointers

SUBROUTINE DayltgInterReflIllFrIntWins(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   Mar. 2004
          !       MODIFIED:na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the inter-reflected illuminance in a daylit zone from beam
          ! and diffuse daylight entering the zone through interior windows. This illuminance
          ! is determined by the split-flux method and is assumed to be uniform, i.e., the same
          ! at all reference points.

          ! METHODOLOGY EMPLOYED:na
          ! REFERENCES:na
          ! USE STATEMENTS:

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER       :: ZoneNum            ! Zone number

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: IWin               ! Window number
  INTEGER   :: ConstrNum          ! Window construction number
  INTEGER   :: AdjZoneNum         ! Adjacent zone number
  REAL(r64) :: QDifTrans          ! Luminous flux transmitted through an int win from adjacent zone (lumens)
  REAL(r64) :: QDifTransUp        ! Upgoing part of QDifTrans (lumens)
  REAL(r64) :: QDifTransDn        ! Downgoing part of QDifTrans (lumens)
  REAL(r64) :: DifInterReflIllThisWin ! Inter-reflected illuminance due to QDifTrans (lux)
  REAL(r64) :: BmInterReflIll     ! Inter-reflected illuminance due to beam solar entering ZoneNum
                                      !  through its interior windows (lux)
          ! FLOW:

ZoneDaylight(ZoneNum)%InterReflIllFrIntWins = 0.0

DO IWin = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
  IF(Surface(IWin)%Class == SurfaceClass_Window .AND. Surface(IWin)%ExtBoundCond >= 1) THEN
    ! This is an interior window in ZoneNum
    ConstrNum = Surface(IWin)%Construction
    AdjZoneNum = Surface(Surface(IWin)%ExtBoundCond)%Zone
    QDifTrans = QSDifSol(AdjZoneNum) * Construct(ConstrNum)%TransDiffVis * Surface(IWin)%Area * PDIFLW
    QDifTransUp = QDifTrans * SurfaceWindow(IWin)%FractionUpgoing
    QDifTransDn = QDifTrans * (1.d0 - SurfaceWindow(IWin)%FractionUpgoing)
    IF (ZoneDaylight(ZoneNum)%TotInsSurfArea * (1.d0-ZoneDaylight(ZoneNum)%AveVisDiffReflect) /= 0.0) THEN
      DifInterReflIllThisWin =  &
        (QDifTransDn * SurfaceWindow(IWin)%RhoFloorWall + QDifTransUp * SurfaceWindow(IWin)%RhoCeilingWall)/ &
        (ZoneDaylight(ZoneNum)%TotInsSurfArea * (1.d0-ZoneDaylight(ZoneNum)%AveVisDiffReflect))
    ELSE
      DifInterReflIllThisWin = 0.0
    ENDIF
    ZoneDaylight(ZoneNum)%InterReflIllFrIntWins = ZoneDaylight(ZoneNum)%InterReflIllFrIntWins +  &
      DifInterReflIllThisWin
  END IF
END DO

! Add inter-reflected illuminance from beam solar entering ZoneNum through interior windows
! TH, CR 7873, 9/17/2009
BmInterReflIll = 0.0
IF (ZoneDaylight(ZoneNum)%TotInsSurfArea > 0) THEN
  BmInterReflIll = (DBZoneIntWin(ZoneNum) * BeamSolarRad * PDIRLW * ZoneDaylight(ZoneNum)%FloorVisRefl)/  &
    (ZoneDaylight(ZoneNum)%TotInsSurfArea * (1.d0-ZoneDaylight(ZoneNum)%AveVisDiffReflect))
ENDIF

ZoneDaylight(ZoneNum)%InterReflIllFrIntWins = ZoneDaylight(ZoneNum)%InterReflIllFrIntWins + BmInterReflIll

RETURN
END SUBROUTINE DayltgInterReflIllFrIntWins

SUBROUTINE CalcMinIntWinSolidAngs

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   Feb. 2004
          !       MODIFIED:na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! For each Daylighting:Detailed zone finds the minimum solid angle subtended
          ! by interior windows through which daylight can pass from adjacent zones with
          ! exterior windows.

          ! METHODOLOGY EMPLOYED:na
          ! REFERENCES:na
          ! USE STATEMENTS:
  USE Vectors

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS: na
          ! SUBROUTINE PARAMETER DEFINITIONS: na
          ! INTERFACE BLOCK SPECIFICATIONS: na
          ! DERIVED TYPE DEFINITIONS: na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER   :: ZoneNum            ! Zone number
  INTEGER   :: ZoneNumAdj         ! Adjacent zone number
  INTEGER   :: IWin               ! Window surface number
  INTEGER   :: IL                 ! Reference point number
  INTEGER   :: loop               ! DO loop index
  LOGICAL   :: Triangle           ! True if window is a triangle
  LOGICAL   :: Rectangle          ! True if window is a rectangle
  LOGICAL   :: IntWinNextToIntWinAdjZone  ! True if an interior window is next to a zone with
                                         ! one or more exterior windows
  REAL(r64) :: IntWinSolidAng     ! Approximation to solid angle subtended by an interior window
                             ! from a point a distance sqrt(zone floor area) away.
  REAL(r64) :: W1(3),W2(3),W3(3)  ! Window vertices
  REAL(r64) :: WC(3)              ! Center point of window
  REAL(r64) :: W21(3),W23(3)      ! Unit vectors from window vertex 2 to 1 and 2 to 3
  REAL(r64) :: HW,WW              ! Window height and width (m)
  REAL(r64) :: RREF(3)            ! Location of a reference point in absolute coordinate system
  REAL(r64) :: RAY(3)             ! Unit vector along ray from reference point to window center
  REAL(r64) :: REFWC(3)           ! Vector from reference point to center of window
  REAL(r64) :: WNORM(3)           ! Unit vector normal to window (pointing away from room)
  REAL(r64) :: DIS                ! Distance from ref point to window center (m)
  REAL(r64) :: COSB               ! Cosine of angle between ray from ref pt to center of window
                                  !  and window outward normal

          ! FLOW:

DO ZoneNum = 1,NumOfZones
  ZoneDaylight(ZoneNum)%MinIntWinSolidAng = 2.d0*Pi
  IF(ZoneDaylight(ZoneNum)%TotalDaylRefPoints == 0) CYCLE
  IF(ZoneDaylight(ZoneNum)%NumOfIntWinAdjZones == 0) CYCLE
  DO IWin = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
    IF(Surface(IWin)%Class == SurfaceClass_Window .and. Surface(IWin)%ExtBoundCond >= 1) THEN
      ZoneNumAdj = Surface(Surface(IWin)%ExtBoundCond)%Zone
      IntWinNextToIntWinAdjZone = .FALSE.
      DO loop = 1,ZoneDaylight(ZoneNum)%NumOfIntWinAdjZones
        IF(ZoneNumAdj == ZoneDaylight(ZoneNum)%AdjIntWinZoneNums(loop)) THEN
          IntWinNextToIntWinAdjZone = .TRUE.
          EXIT
        END IF
      END DO
      IF(IntWinNextToIntWinAdjZone) THEN
        DO IL = 1,ZoneDaylight(ZoneNum)%TotalDaylRefPoints
          ! Reference point in absolute coordinate system
          RREF(1:3) = ZoneDaylight(ZoneNum)%DaylRefPtAbsCoord(IL,1:3)
          Rectangle = .FALSE.
          Triangle = .FALSE.
          IF (Surface(IWin)%Sides == 3) Triangle = .TRUE.
          IF (Surface(IWin)%Sides == 4) Rectangle = .TRUE.
          IF (Rectangle) THEN
            ! Vertices of window numbered counter-clockwise starting at upper left as viewed
            ! from inside of room. Assumes original vertices are numbered counter-clockwise from
            ! upper left as viewed from outside.
            W3 = Surface(IWin)%Vertex(2)
            W2 = Surface(IWin)%Vertex(3)
            W1 = Surface(IWin)%Vertex(4)
          ELSE IF (Triangle) THEN
            W3 = Surface(IWin)%Vertex(2)
            W2 = Surface(IWin)%Vertex(3)
            W1 = Surface(IWin)%Vertex(1)
          END IF
          ! Unit vectors from window vertex 2 to 1 and 2 to 3, center point of window,
          ! and vector from ref pt to center of window
          W21 = W1 - W2
          W23 = W3 - W2
          HW = SQRT(DOT_PRODUCT(W21,W21))
          WW = SQRT(DOT_PRODUCT(W23,W23))
          IF (Rectangle) THEN
            WC  = W2 + (W23 + W21)/2
          ELSE IF (Triangle) THEN
            WC  = W2 + (W23 + W21)/3
          END IF
          ! Vector from ref point to center of window
          REFWC = WC - RREF
          W21 = W21/HW
          W23 = W23/WW
          ! Unit vector normal to window (pointing away from room)
          WNORM = Surface(IWin)%OutNormVec
          ! Distance from ref point to center of window
          DIS = SQRT(DOT_PRODUCT(REFWC,REFWC))
          ! Unit vector from ref point to center of window
          RAY = REFWC/DIS
          ! Cosine of angle between ray from ref pt to center of window and window outward normal
          COSB = DOT_PRODUCT(WNORM, RAY)
          IF(COSB > 0.01765d0) THEN  ! 0 <= B < 89 deg
            ! Above test avoids case where ref point cannot receive daylight directly from the
            ! interior window
            IntWinSolidAng = COSB * Surface(IWin)%Area / (DIS**2 + 0.001d0)
            ZoneDaylight(ZoneNum)%MinIntWinSolidAng =  &
             MIN(ZoneDaylight(ZoneNum)%MinIntWinSolidAng,IntWinSolidAng)
          END IF
        END DO  ! End of loop over reference points
      END IF
    END IF
  END DO  ! End of loop over surfaces in zone
END DO  ! End of loop over zones

RETURN
END SUBROUTINE CalcMinIntWinSolidAngs

SUBROUTINE CheckForGeometricTransform(DoTransform,OldAspectRatio,NewAspectRatio)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! check for geometrytransform in the daylighting access for reference and map points
          !

          ! METHODOLOGY EMPLOYED:
          ! once reference points  have been converted to WCS,
          !  change them to reflect a different aspect
          ! ratio for the entire building based on user input.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor
  USE DataDaylighting, ONLY: ZoneDaylight

 IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  Logical,  INTENT(INOUT)  :: doTransform
  REAL(r64), INTENT(INOUT) :: OldAspectRatio
  REAL(r64), INTENT(INOUT) :: NewAspectRatio

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: CurrentModuleObject='GeometryTransform'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  CHARACTER(len=MaxNameLength), DIMENSION(1) :: cAlphas
  REAL(r64), DIMENSION(2) :: rNumerics
  INTEGER            :: NAlphas
  INTEGER            :: NNum
  INTEGER            :: IOSTAT
  CHARACTER(len=2)   :: transformPlane

  !begin execution
  !get user input...
  doTransform=.false.
  OldAspectRatio = 1.0
  NewAspectRatio = 1.0

  IF (GetNumObjectsFound(CurrentModuleObject) == 1) then
     CALL GetObjectItem(CurrentModuleObject,1,cAlphas,NAlphas,rNumerics,NNum,IOSTAT,  &
                 AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                 AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
     OldAspectRatio = rNumerics(1)
     NewAspectRatio = rNumerics(2)
     transformPlane = cAlphas(1)
     IF (transformPlane /= 'XY') then
       CALL ShowWarningError(CurrentModuleObject//': invalid '//TRIM(cAlphaFieldNames(1))//  &
         '="'//TRIM(cAlphas(1))//'...ignored.')
     ENDIF
     doTransform = .true.
     AspectTransform = .true.
  Endif
  IF (WorldCoordSystem) THEN
    doTransform=.false.
    AspectTransform=.false.
  ENDIF

  Return

END SUBROUTINE CheckForGeometricTransform

! BSLLC Start
SUBROUTINE WriteDaylightMapTitle (mapNum, unitNo, mapName, environmentName, zone, refPt1, refPt2, zcoord)
    ! SUBROUTINE INFORMATION:
    !       AUTHOR         Greg Stark
    !       DATE WRITTEN   Sept 2008
    !       MODIFIED       na
    !       RE-ENGINEERED  na

    ! PURPOSE OF THIS SUBROUTINE:
    ! The purpose of the routine is to allow the daylighting map data to be written in various formats

    ! METHODOLOGY EMPLOYED:
    ! na

    ! REFERENCES:
    ! na

    ! USE STATEMENTS:
    USE SQLiteProcedures

    IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


    ! SUBROUTINE ARGUMENT DEFINITIONS:
    INTEGER, INTENT(IN) :: mapNum
    INTEGER, INTENT(IN) :: unitNo
    CHARACTER(len=*), INTENT(IN) :: mapName
    CHARACTER(len=*), INTENT(IN) :: environmentName
    INTEGER, INTENT(IN) :: zone
    REAL(r64), INTENT(IN) :: zcoord
    CHARACTER(len=*), INTENT(IN) :: refPt1
    CHARACTER(len=*), INTENT(IN) :: refPt2

    ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: FmtA="(A)"

    ! INTERFACE BLOCK SPECIFICATIONS
    ! na

    ! DERIVED TYPE DEFINITIONS
    ! na

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

    ! must add correct number of commas at end
    WRITE(unitNo, FmtA) 'Date/Time,'//TRIM(environmentName)//':'//TRIM(mapName)//   &
        ':Illuminance [lux] (Hourly)'//MapColSep//TRIM(refPt1)//MapColSep// &
        TRIM(refPt2)//MapColSep//MapColSep

    IF (WriteOutputToSQLite) THEN
        CALL CreateSQLiteDaylightMapTitle (mapNum, mapName, environmentName, zone, refPt1, refPt2, zcoord)
    END IF

RETURN

END SUBROUTINE WriteDaylightMapTitle
! BSLLC Finish

!     NOTICE
!
!     Copyright © 1996-2011 The Board of Trustees of the University of Illinois
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

END MODULE DaylightingManager
