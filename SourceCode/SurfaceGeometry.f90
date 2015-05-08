MODULE SurfaceGeometry

  ! Module containing the routines dealing with the Surface Geometry

  ! MODULE INFORMATION:
  !       AUTHOR         Linda Lawrie
  !       DATE WRITTEN   June 2000
  !       MODIFIED       DJS (PSU Dec 2006) to add ecoroof
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! This module performs the functions required of the surface geometry.

  ! METHODOLOGY EMPLOYED:
  ! na

  ! REFERENCES: none

  ! OTHER NOTES: none

  ! USE STATEMENTS:
  ! Use statements for data only modules
USE DataPrecisionGlobals
USE DataGlobals
USE DataEnvironment
USE DataHeatBalance
USE DataSurfaces
USE DataInterfaces
USE DataWindowEquivalentLayer, ONLY: CFSMAXNL

  ! Use statements for access to subroutines in other modules
  ! na

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  !MODULE PARAMETER DEFINITIONS
CHARACTER(len=*), PARAMETER :: Blank=' '
CHARACTER(len=*), PARAMETER, DIMENSION(3) :: BaseSurfCls =(/'WALL ','FLOOR','ROOF '/)
CHARACTER(len=*), PARAMETER, DIMENSION(6) :: SubSurfCls  =(/'WINDOW                 ',  &
                                                            'DOOR                   ',  &
                                                            'GLASSDOOR              ',&
                                                            'SHADING                ',  &
                                                            'TUBULARDAYLIGHTDOME    ',  &
                                                            'TUBULARDAYLIGHTDIFFUSER'/)
INTEGER, PARAMETER, DIMENSION(3) :: BaseSurfIDs =(/  &
           SurfaceClass_Wall,  &
           SurfaceClass_Floor, &
           SurfaceClass_Roof/)

INTEGER, PARAMETER, DIMENSION(6) :: SubSurfIDs =(/  &
           SurfaceClass_Window,     &
           SurfaceClass_Door,       &
           SurfaceClass_GlassDoor,  &
           SurfaceClass_Shading,    &
           SurfaceClass_TDD_Dome,   &
           SurfaceClass_TDD_Diffuser/)

INTEGER, PARAMETER :: UnenteredAdjacentZoneSurface=-998     ! allows users to enter one zone surface ("Zone")
                                                            ! referencing another in adjacent zone
INTEGER, PARAMETER :: UnreconciledZoneSurface=-999          ! interim value between entering surfaces ("Surface") and reconciling
                                                            ! surface names in other zones

  character(len=*), parameter :: fmt3="(A,3(1x,f18.13))"

  ! DERIVED TYPE DEFINITIONS

  !MODULE VARIABLE DECLARATIONS:
  ! Following are used only during getting vertices, so are module variables here.
REAL(r64)   :: CosBldgRelNorth =0.0d0 ! Cosine of the building rotation (relative north) (includes appendix G rotation)
REAL(r64)   :: SinBldgRelNorth =0.0d0 ! Sine of the building rotation (relative north)   (includes appendix G rotation)
REAL(r64)   :: CosBldgRotAppGonly =0.0d0 ! Cosine of the building rotation for appendix G only(relative north)
REAL(r64)   :: SinBldgRotAppGonly =0.0d0 ! Sine of the building rotation for appendix G only (relative north)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: CosZoneRelNorth ! Cosine of the zone rotation (relative north)
REAL(r64), ALLOCATABLE, DIMENSION(:) :: SinZoneRelNorth ! Sine of the zone rotation (relative north)
TYPE (SurfaceData),        ALLOCATABLE, DIMENSION(:) :: SurfaceTmp  ! Allocated/Deallocated during input processing
TYPE (SurfaceData),        ALLOCATABLE, DIMENSION(:) :: SurfaceTmpSave  ! Allocated/Deallocated during input processing

LOGICAL :: NoGroundTempObjWarning=.true.  ! This will cause a warning to be issued if surfaces with "Ground"
                                          ! outside environment are used but no ground temperature object was input.
LOGICAL :: NoFCGroundTempObjWarning=.true.  ! This will cause a warning to be issued if surfaces with "GroundFCfactorMethod"
                                          ! outside environment are used but no FC ground temperatures was input.
LOGICAL :: RectSurfRefWorldCoordSystem=.false.  ! GlobalGeometryRules=World (true) or Relative (false)
INTEGER :: Warning1Count=0  ! counts of Modify Window 5/6 windows
INTEGER :: Warning2Count=0  ! counts of overriding exterior windows with Window 5/6 glazing systems
INTEGER :: Warning3Count=0  ! counts of overriding interior windows with Window 5/6 glazing systems

  !SUBROUTINE SPECIFICATIONS FOR MODULE SurfaceGeometry
PUBLIC  SetupZoneGeometry
PRIVATE AllocateModuleArrays
PRIVATE GetSurfaceData
PRIVATE GetGeometryParameters
PRIVATE GetDetShdSurfaceData
PRIVATE GetRectDetShdSurfaceData
PRIVATE GetHTSurfaceData
PRIVATE GetRectSurfaces
PRIVATE MakeRectangularVertices
PRIVATE GetHTSubSurfaceData
PRIVATE GetRectSubSurfaces
PRIVATE CheckWindowShadingControlFrameDivider
PRIVATE CheckSubSurfaceMiscellaneous
PRIVATE MakeRelativeRectangularVertices
PRIVATE GetAttShdSurfaceData
PRIVATE GetSimpleShdSurfaceData
PRIVATE GetIntMassSurfaceData
PRIVATE GetShadingSurfReflectanceData
PRIVATE GetHTSurfExtVentedCavityData
PRIVATE GetSurfaceHeatTransferAlgorithmOverrides
PRIVATE GetVertices
PRIVATE MakeMirrorSurface
PRIVATE GetWindowShadingControlData
PRIVATE GetStormWindowData
PRIVATE GetWindowGapAirflowControlData
PRIVATE GetOSCData
PRIVATE GetOSCMData
PRIVATE GetMovableInsulationData
PRIVATE CalculateZoneVolume
PRIVATE ProcessSurfaceVertices
PRIVATE CalcCoordinateTransformation
PRIVATE CreateShadedWindowConstruction
PRIVATE CreateStormWindowConstructions
PRIVATE ModifyWindow
PRIVATE TransformVertsByAspect
PRIVATE CalcSurfaceCentroid
PRIVATE SetupShadeSurfacesForSolarCalcs
PRIVATE CheckConvexity

CONTAINS

SUBROUTINE SetupZoneGeometry(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton
          !       DATE WRITTEN   September 1977
          !       MODIFIED       April 2002 (FCW): add warning for Solar Distribution
          !                      = FullInteriorExterior when window has reveal
          !                      Add fatal error when triangular window has reveal
          !                      May 2002(FCW): Allow triangular windows to have reveal (subr SHDRVL
          !                      in SolarShading). Remove above warning and fatal error.
          !       RE-ENGINEERED  November 1997 (RKS,LKL)

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine controls the processing of detached shadowing and
          ! zone surfaces for computing their vertices.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataVectorTypes
  USE OutputReportPredefined
  USE General, ONLY: RoundSigDigits
  USE DataReportingFlags
  USE InputProcessor, ONLY: GetNumSectionsFound

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: ValFmt="(F20.2)"
  CHARACTER(len=*), PARAMETER :: fmtA="(A)"
  CHARACTER(len=*), PARAMETER :: RoutineName='SetUpZoneGeometry: '
          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)          :: AverageHeight   ! Used to keep track of average height of a surface/zone
  INTEGER            :: SurfNum         ! Surface number (DO loop counter)
  INTEGER            :: ZoneNum         ! Zone number for current surface and DO loop counter
  REAL(r64)          :: ZMax            ! Maximum Z of a surface (detailed outside coefficient calculation)
  REAL(r64)          :: ZMin            ! Minimum Z of a surface (detailed outside coefficient calculation)
  REAL(r64)          :: ZCeilAvg
  REAL(r64)          :: CeilCount
  REAL(r64)          :: ZFlrAvg
  REAL(r64)          :: FloorCount
  REAL(r64)          :: TotSurfArea
  REAL(r64)          :: Z1
  REAL(r64)          :: Z2
  CHARACTER(len=32) String1
  CHARACTER(len=32) String2
  CHARACTER(len=3) String3
  INTEGER Count    ! To count wall surfaces for ceiling height calculation
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: ZoneCeilingHeightEntered
  REAL(r64), ALLOCATABLE, DIMENSION(:) :: ZoneCeilingArea
  INTEGER, SAVE :: ErrCount=0
  REAL(r64) :: NominalUwithConvCoeffs
  CHARACTER(len=32) :: cNominalU
  CHARACTER(len=32) :: cNominalUwithConvCoeffs
  LOGICAL :: isWithConvCoefValid
!  INTEGER, ALLOCATABLE, DIMENSION(:) :: ZoneSurfacesCount
!  INTEGER, ALLOCATABLE, DIMENSION(:) :: ZoneSubSurfacesCount
!  INTEGER, ALLOCATABLE, DIMENSION(:) :: ZoneShadingSurfacesCount



  LOGICAL :: nonInternalMassSurfacesPresent
  LOGICAL :: DetailedWWR


          ! FLOW:
          ! Allocations and initializations...

          ! Zones must have been "gotten" before this call
          ! The RelNorth variables are used if "relative" coordinates are input as well
          ! as setting up DaylightingCoords

  ! these include building north axis and Building Rotation for Appendix G
  CosBldgRelNorth = COS(-(BuildingAzimuth + BuildingRotationAppendixG)*DegToRadians)
  SinBldgRelNorth = SIN(-(BuildingAzimuth + BuildingRotationAppendixG)*DegToRadians)

  ! these are only for Building Rotation for Appendix G when using world coordinate system
  CosBldgRotAppGonly = COS(-BuildingRotationAppendixG*DegToRadians)
  SinBldgRotAppGonly = SIN(-BuildingRotationAppendixG*DegToRadians)

  ALLOCATE (CosZoneRelNorth(NumOfZones))
  ALLOCATE (SinZoneRelNorth(NumOfZones))

  ALLOCATE (ZoneCeilingHeightEntered(NumOfZones))
  ZoneCeilingHeightEntered=.false.
  ALLOCATE (ZoneCeilingArea(NumOfZones))
  ZoneCeilingArea=0.0d0

  DO ZoneNum = 1, NumOfZones

    CosZoneRelNorth(ZoneNum) =  COS(-Zone(ZoneNum)%RelNorth*DegToRadians)
    SinZoneRelNorth(ZoneNum) =  SIN(-Zone(ZoneNum)%RelNorth*DegToRadians)

  END DO
  CALL GetSurfaceData(ErrorsFound)

  IF (ErrorsFound) THEN
    DEALLOCATE (CosZoneRelNorth)
    DEALLOCATE (SinZoneRelNorth)
    RETURN
  ENDIF

  CALL GetWindowGapAirflowControlData(ErrorsFound)

  CALL GetStormWindowData(ErrorsFound)

  IF(.NOT.ErrorsFound .AND. TotStormWin > 0) CALL CreateStormWindowConstructions

  DEALLOCATE (CosZoneRelNorth)
  DEALLOCATE (SinZoneRelNorth)

  CALL AllocateModuleArrays ! This needs to be moved to the main manager routine of SSG at a later date

  ALLOCATE(AirSkyRadSplit(TotSurfaces))
  AirSkyRadSplit=0.0d0

  CalcWindowRevealReflection = .FALSE. ! Set to True in ProcessSurfaceVertices if beam solar reflection from window reveals
                                       ! is requested for one or more exterior windows.
  BuildingShadingCount =0
  FixedShadingCount    =0
  AttachedShadingCount =0

  DO SurfNum = 1, TotSurfaces   ! Loop through all surfaces...

    AirSkyRadSplit(SurfNum) = SQRT(0.5d0*(1.0d0+Surface(SurfNum)%CosTilt))

    ! Set flag that determines whether a surface is a shadowing surface
    Surface(SurfNum)%ShadowingSurf = .FALSE.
    IF(Surface(SurfNum)%Class == SurfaceClass_Shading .OR. Surface(SurfNum)%Class == SurfaceClass_Detached_F .OR.  &
       Surface(SurfNum)%Class == SurfaceClass_Detached_B) Surface(SurfNum)%ShadowingSurf = .TRUE.
    IF (Surface(SurfNum)%Class == SurfaceClass_Shading) AttachedShadingCount=AttachedShadingCount+1
    IF (Surface(SurfNum)%Class == SurfaceClass_Detached_F) FixedShadingCount=FixedShadingCount+1
    IF (Surface(SurfNum)%Class == SurfaceClass_Detached_B) BuildingShadingCount=BuildingShadingCount+1

    IF (Surface(SurfNum)%Class /= SurfaceClass_IntMass) CALL ProcessSurfaceVertices(SurfNum,ErrorsFound)
  END DO

  Zone%ExtWindowArea=0.0d0
  Zone%HasInterZoneWindow=.false.
  Zone%HasWindow=.false.
  Zone%ExtGrossWallArea=0.0d0
  Zone%ExtNetWallArea=0.0d0
  Zone%TotalSurfArea=0.0d0

  DetailedWWR=(GetNumSectionsFound('DETAILEDWWR_DEBUG') > 0)
  IF (DetailedWWR) THEN
    WRITE(OutputFileDebug,'(A)') '=======User Entered Classification ================='
    WRITE(OutputFileDebug,'(A)') 'Surface,Class,Area,Tilt'
  ENDIF

  DO SurfNum = 1, TotSurfaces ! Loop through all surfaces to find windows...

    IF (.NOT.Surface(SurfNum)%HeatTransSurf) CYCLE    ! Skip shadowing (sub)surfaces
    ZoneNum=Surface(SurfNum)%Zone
    Zone(ZoneNum)%TotalSurfArea = Zone(ZoneNum)%TotalSurfArea + Surface(SurfNum)%Area
    IF(Construct(Surface(SurfNum)%Construction)%TypeIsWindow) THEN
      Zone(ZoneNum)%TotalSurfArea = Zone(ZoneNum)%TotalSurfArea + SurfaceWindow(SurfNum)%FrameArea
      Zone(ZoneNum)%HasWindow = .true.
    ENDIF
    IF (Surface(SurfNum)%Class == SurfaceClass_Roof) ZoneCeilingArea(ZoneNum)=ZoneCeilingArea(ZoneNum)+Surface(SurfNum)%Area
    IF (.NOT. Construct(Surface(SurfNum)%Construction)%TypeIsWindow) THEN
      IF (Surface(SurfNum)%ExtBoundCond == ExternalEnvironment  .or. &
          Surface(SurfNum)%ExtBoundCond == OtherSideCondModeledExt ) THEN
        Zone(ZoneNum)%ExteriorTotalSurfArea=Zone(ZoneNum)%ExteriorTotalSurfArea +   &
                           Surface(SurfNum)%GrossArea
        IF (Surface(SurfNum)%Class == SurfaceClass_Wall) THEN
          Zone(ZoneNum)%ExtNetWallArea = Zone(ZoneNum)%ExtNetWallArea +   &
                           Surface(SurfNum)%Area
          Zone(ZoneNum)%ExtGrossWallArea = Zone(ZoneNum)%ExtGrossWallArea +   &
                           Surface(SurfNum)%GrossArea
          Zone(ZoneNum)%ExtGrossWallArea_Multiplied = Zone(ZoneNum)%ExtGrossWallArea_Multiplied +   &
                           Surface(SurfNum)%GrossArea * Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
          IF (DetailedWWR) THEN
            WRITE(OutputFileDebug,'(A)') trim(Surface(SurfNum)%Name)//',Wall,'//  &
               trim(RoundSigDigits(Surface(SurfNum)%GrossArea*Zone(ZoneNum)%Multiplier*Zone(ZoneNum)%ListMultiplier,2))//  &
               ','//trim(RoundSigDigits(Surface(SurfNum)%Tilt,1))
          ENDIF
        ENDIF
      ELSEIF (Surface(SurfNum)%ExtBoundCond == Ground .or. Surface(SurfNum)%ExtBoundCond == GroundFCfactorMethod) THEN
        Zone(ZoneNum)%ExteriorTotalGroundSurfArea=Zone(ZoneNum)%ExteriorTotalGroundSurfArea +   &
                           Surface(SurfNum)%GrossArea
        IF (Surface(SurfNum)%Class == SurfaceClass_Wall) THEN
          Zone(ZoneNum)%ExtGrossGroundWallArea = Zone(ZoneNum)%ExtGrossGroundWallArea +   &
                           Surface(SurfNum)%GrossArea
          Zone(ZoneNum)%ExtGrossGroundWallArea_Multiplied = Zone(ZoneNum)%ExtGrossGroundWallArea_Multiplied +   &
                           Surface(SurfNum)%GrossArea * Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier
          IF (DetailedWWR) THEN
            WRITE(OutputFileDebug,'(A)') trim(Surface(SurfNum)%Name)//',Wall-GroundContact,'//  &
               trim(RoundSigDigits(Surface(SurfNum)%GrossArea*Zone(ZoneNum)%Multiplier*Zone(ZoneNum)%ListMultiplier,2))//  &
               ','//trim(RoundSigDigits(Surface(SurfNum)%Tilt,1))
          ENDIF
        ENDIF
      ENDIF

    ELSE  ! For Windows

      IF ((Surface(SurfNum)%ExtBoundCond > 0) .AND. (Surface(SurfNum)%BaseSurf /= SurfNum)) THEN ! Interzone window present
        IF (.not. IgnoreInteriorWindowTransmission) THEN
          Zone(Surface(SurfNum)%Zone)%HasInterZoneWindow=.TRUE.
        ENDIF
      ELSE
        IF (( (Surface(SurfNum)%ExtBoundCond == ExternalEnvironment) .OR.   &
                (Surface(SurfNum)%ExtBoundCond == OtherSideCondModeledExt) )&
             .AND. (Surface(SurfNum)%Class .NE. SurfaceClass_TDD_Dome)) THEN
          Zone(Surface(SurfNum)%Zone)%ExtWindowArea = Zone(Surface(SurfNum)%Zone)%ExtWindowArea + Surface(SurfNum)%GrossArea
          Zone(Surface(SurfNum)%Zone)%ExtWindowArea_Multiplied = Zone(Surface(SurfNum)%Zone)%ExtWindowArea +   &
             Surface(SurfNum)%GrossArea*Surface(SurfNum)%Multiplier*Zone(ZoneNum)%Multiplier*Zone(ZoneNum)%ListMultiplier
          IF (DetailedWWR) THEN
            WRITE(OutputFileDebug,'(A)') trim(Surface(SurfNum)%Name)//',Window,'//  &
               trim(RoundSigDigits(Surface(SurfNum)%GrossArea*Surface(SurfNum)%Multiplier*  &
                                   Zone(ZoneNum)%Multiplier*Zone(ZoneNum)%ListMultiplier,2))//  &
                    ','//trim(RoundSigDigits(Surface(SurfNum)%Tilt,1))
          ENDIF
       ENDIF
      ENDIF

    ENDIF

  END DO    ! ...end of surfaces windows DO loop

!  DO SurfNum = 1, TotSurfaces ! Set areas for Sunlit area calculations for Windows
!    IF (Surface(SurfNum)%Class /= SurfaceClass_Window) CYCLE
!    SurfaceWindow(SurfNum)%AreaCalcForSunlitArea = (Surface(SurfNum)%Area + SurfaceWindow(SurfNum)%DividerArea) /  &
!                                  Surface(SurfNum)%Multiplier
!  ENDDO
  IF (DetailedWWR) THEN
    WRITE(OutputFileDebug,'(A)') '========================'
    WRITE(OutputFileDebug,'(A)') 'Zone,ExtWallArea,ExtWindowArea'
  ENDIF

  DO ZoneNum = 1, NumOfZones
    CeilCount=0.0d0
    FloorCount=0.0d0
    Count=0
    AverageHeight=0.0d0
    ZCeilAvg=0.0d0
    ZFlrAvg=0.0d0
    ZMax=-99999.0d0
    ZMin=99999.0d0
    IF (DetailedWWR) THEN
      WRITE(OutputFileDebug,'(A)') trim(Zone(ZoneNum)%Name)//','//  &
               trim(RoundSigDigits(Zone(ZoneNum)%ExtGrossWallArea,2))//','//  &
               trim(RoundSigDigits(Zone(ZoneNum)%ExtWindowArea,2))
    ENDIF
    DO SurfNum=Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
      IF (Surface(SurfNum)%Class == SurfaceClass_Roof) THEN
        ! Use Average Z for surface, more important for roofs than floors...
        CeilCount=CeilCount+1.0d0
        Z1=MINVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z)
        Z2=MAXVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z)
!        ZCeilAvg=ZCeilAvg+(Z1+Z2)/2.d0
        ZCeilAvg=ZCeilAvg+((Z1+Z2)/2.d0)*(Surface(SurfNum)%Area/ZoneCeilingArea(ZoneNum))
      ENDIF
      IF (Surface(SurfNum)%Class == SurfaceClass_Floor) THEN
        ! Use Average Z for surface, more important for roofs than floors...
        FloorCount=FloorCount+1.0d0
        Z1=MINVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z)
        Z2=MAXVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z)
!        ZFlrAvg=ZFlrAvg+(Z1+Z2)/2.d0
        ZFlrAvg=ZFlrAvg+((Z1+Z2)/2.d0)*(Surface(SurfNum)%Area/Zone(ZoneNum)%FloorArea)
      ENDIF
      IF (Surface(SurfNum)%Class == SurfaceClass_Wall) THEN
        ! Use Wall calculation in case no roof & floor in zone
        Count=Count+1
        IF (Count == 1) THEN
          ZMax=Surface(SurfNum)%Vertex(1)%Z
          ZMin=ZMax
        ENDIF
        ZMax=MAX(ZMax,MAXVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z))
        ZMin=MIN(ZMin,MINVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z))
      ENDIF
    ENDDO
    IF (CeilCount > 0.0d0 .and. FloorCount > 0.0d0) THEN
!      ZCeilAvg=ZCeilAvg/CeilCount
!      ZFlrAvg=ZFlrAvg/FloorCount
      AverageHeight=ZCeilAvg-ZFlrAvg
    ELSE
      AverageHeight=(ZMax-ZMin)
    ENDIF
    IF (AverageHeight <= 0.0d0) THEN
      AverageHeight=(ZMax-ZMin)
    ENDIF

    IF (Zone(ZoneNum)%CeilingHeight > 0.0d0) THEN
      ZoneCeilingHeightEntered(ZoneNum)=.true.
      IF (AverageHeight > 0.0d0) THEN
        IF (ABS(AverageHeight-Zone(ZoneNum)%CeilingHeight)/Zone(ZoneNum)%CeilingHeight  > .05d0) THEN
          IF (ErrCount == 1 .and. .not. DisplayExtraWarnings) THEN
            CALL ShowWarningError(RoutineName//'Entered Ceiling Height for some zone(s) significantly '//  &
               'different from calculated Ceiling Height')
            CALL ShowContinueError('...use Output:Diagnostics,DisplayExtraWarnings; '// &
                 'to show more details on each max iteration exceeded.')
          ENDIF
          IF (DisplayExtraWarnings) THEN
            CALL ShowWarningError(RoutineName//'Entered Ceiling Height for Zone="'//TRIM(Zone(ZoneNum)%Name)//  &
                                  '" significantly different from calculated Ceiling Height')
            WRITE(String1,ValFmt) Zone(ZoneNum)%CeilingHeight
            String1=ADJUSTL(String1)
            WRITE(String2,ValFmt) AverageHeight
            String2=ADJUSTL(String2)
            CALL ShowContinueError(RoutineName//'Entered Ceiling Height='//TRIM(String1)//  &
               ', Calculated Ceiling Height='//TRIM(String2)// &
               ', entered height will be used in calculations.')
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    IF ((Zone(ZoneNum)%CeilingHeight <= 0.0d0).AND.(AverageHeight > 0.0d0)) &
         Zone(ZoneNum)%CeilingHeight = AverageHeight

  END DO

  CALL CalculateZoneVolume(ErrorsFound,ZoneCeilingHeightEntered)    ! Calculate Zone Volumes

  ! Calculate zone centroid (and min/max x,y,z for zone)
  DO ZoneNum = 1, NumOfZones
    nonInternalMassSurfacesPresent=.false.
    TotSurfArea = 0.0d0
    Zone(ZoneNum)%Centroid = Vector(0.0d0, 0.0d0, 0.0d0)
    IF (Surface(Zone(ZoneNum)%SurfaceFirst)%Sides > 0) THEN
      Zone(ZoneNum)%MinimumX=Surface(Zone(ZoneNum)%SurfaceFirst)%Vertex(1)%x
      Zone(ZoneNum)%MaximumX=Surface(Zone(ZoneNum)%SurfaceFirst)%Vertex(1)%x
      Zone(ZoneNum)%MinimumY=Surface(Zone(ZoneNum)%SurfaceFirst)%Vertex(1)%y
      Zone(ZoneNum)%MaximumY=Surface(Zone(ZoneNum)%SurfaceFirst)%Vertex(1)%y
      Zone(ZoneNum)%MinimumZ=Surface(Zone(ZoneNum)%SurfaceFirst)%Vertex(1)%z
      Zone(ZoneNum)%MaximumZ=Surface(Zone(ZoneNum)%SurfaceFirst)%Vertex(1)%z
    ENDIF
    DO SurfNum = Zone(ZoneNum)%SurfaceFirst, Zone(ZoneNum)%SurfaceLast
      IF (Surface(SurfNum)%Class == SurfaceClass_IntMass) CYCLE
      nonInternalMassSurfacesPresent=.true.
      IF (Surface(SurfNum)%Class == SurfaceClass_Wall  &
        .OR. (Surface(SurfNum)%Class == SurfaceClass_Roof) &
        .OR. (Surface(SurfNum)%Class == SurfaceClass_Floor)) THEN

        Zone(ZoneNum)%Centroid%x = Zone(ZoneNum)%Centroid%x + Surface(SurfNum)%Centroid%x * Surface(SurfNum)%GrossArea
        Zone(ZoneNum)%Centroid%y = Zone(ZoneNum)%Centroid%y + Surface(SurfNum)%Centroid%y * Surface(SurfNum)%GrossArea
        Zone(ZoneNum)%Centroid%z = Zone(ZoneNum)%Centroid%z + Surface(SurfNum)%Centroid%z * Surface(SurfNum)%GrossArea
        TotSurfArea = TotSurfArea + Surface(SurfNum)%GrossArea
      END IF
      Zone(ZoneNum)%MinimumX=MIN(Zone(ZoneNum)%MinimumX,MINVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%x))
      Zone(ZoneNum)%MaximumX=MAX(Zone(ZoneNum)%MaximumX,MAXVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%x))
      Zone(ZoneNum)%MinimumY=MIN(Zone(ZoneNum)%MinimumY,MINVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%y))
      Zone(ZoneNum)%MaximumY=MAX(Zone(ZoneNum)%MaximumY,MAXVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%y))
      Zone(ZoneNum)%MinimumZ=MIN(Zone(ZoneNum)%MinimumZ,MINVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%z))
      Zone(ZoneNum)%MaximumZ=MAX(Zone(ZoneNum)%MaximumZ,MAXVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%z))
    END DO
    IF (TotSurfArea > 0.0d0) THEN
      Zone(ZoneNum)%Centroid%x = Zone(ZoneNum)%Centroid%x / TotSurfArea
      Zone(ZoneNum)%Centroid%y = Zone(ZoneNum)%Centroid%y / TotSurfArea
      Zone(ZoneNum)%Centroid%z = Zone(ZoneNum)%Centroid%z / TotSurfArea
    ENDIF
    IF (.not. nonInternalMassSurfacesPresent) THEN
      CALL ShowSevereError(RoutineName//'Zone="'//trim(Zone(ZoneNum)%Name)//  &
             '" has only internal mass surfaces.  Need at least one other surface.')
      ErrorsFound=.true.
    ENDIF
  END DO

  DEALLOCATE(ZoneCeilingHeightEntered)
  DEALLOCATE(ZoneCeilingArea)

  ALLOCATE(AdjacentZoneToSurface(TotSurfaces))
  AdjacentZoneToSurface=0
  ! note -- adiabatic surfaces will show same zone as surface
  do surfnum=1,totsurfaces
    if (Surface(surfnum)%ExtBoundCond <= 0) CYCLE
    AdjacentZoneToSurface(surfnum)=Surface(Surface(surfNum)%ExtBoundCond)%Zone
  enddo

  do zonenum=1,NumOfZones
    do surfnum=1,totsurfaces
      if (.not. Surface(surfnum)%HeatTransSurf .and. Surface(surfnum)%ZoneName == Zone(zonenum)%Name) &
        Zone(zonenum)%NumShadingSurfaces=Zone(zonenum)%NumShadingSurfaces+1

      if (Surface(surfnum)%Zone /= zonenum) CYCLE

      if (Surface(surfnum)%HeatTransSurf .and. (Surface(surfnum)%class == SurfaceClass_Wall .or.          &
          Surface(surfnum)%class == SurfaceClass_Roof .or. Surface(surfnum)%class == SurfaceClass_Floor)) &
        Zone(zonenum)%NumSurfaces=Zone(zonenum)%NumSurfaces+1

      if (Surface(surfnum)%HeatTransSurf .and. (Surface(surfnum)%class == SurfaceClass_Window .or.                   &
          Surface(surfnum)%class == SurfaceClass_GlassDoor .or. Surface(surfnum)%class == SurfaceClass_Door .or.     &
          Surface(surfnum)%class == SurfaceClass_TDD_Dome .or. Surface(surfnum)%class == SurfaceClass_TDD_Diffuser)) &
        Zone(zonenum)%NumSubSurfaces=Zone(zonenum)%NumSubSurfaces+1

    enddo  ! surfaces
  enddo ! zones

  do surfnum=1,totsurfaces
    if (Surface(surfnum)%Construction > 0 .and. Surface(surfnum)%Construction <= TotConstructs) THEN
      NominalUwithConvCoeffs = ComputeNominalUwithConvCoeffs(surfnum,isWithConvCoefValid)
      IF (isWithConvCoefValid) THEN
        cNominalUwithConvCoeffs=RoundSigDigits(NominalUwithConvCoeffs,3)
      ELSE
        cNominalUwithConvCoeffs = '[invalid]'
      END IF
      IF ((Surface(surfnum)%class == SurfaceClass_Window) .OR. (Surface(surfnum)%class == SurfaceClass_TDD_Dome)) THEN
        ! SurfaceClass_Window also covers glass doors and TDD:Diffusers
        cNominalU='N/A'
      ELSE
        cNominalU=RoundSigDigits(NominalU(Surface(surfnum)%Construction),3)
      END IF
    else
      CNominalUwithConvCoeffs = '**'
      CNominalU = '**'
    endif

    ! save the U-value nominal for use later in tabular report
    Surface(surfnum)%UNomWOFilm = cNominalU
    Surface(surfnum)%UNomFilm = cNominalUwithConvCoeffs
    !populate the predefined report related to u-values with films
    !only exterior surfaces including underground
    IF ((Surface(surfnum)%ExtBoundCond .EQ. ExternalEnvironment) .OR. (Surface(surfnum)%ExtBoundCond .EQ. Ground)) THEN
      SELECT CASE (Surface(surfnum)%Class)
        CASE (SurfaceClass_Wall,SurfaceClass_Floor,SurfaceClass_Roof)
          CALL PreDefTableEntry(pdchOpUfactFilm,Surface(surfnum)%Name,NominalUwithConvCoeffs,3)
        CASE (SurfaceClass_Door)
          CALL PreDefTableEntry(pdchDrUfactFilm,Surface(surfnum)%Name,NominalUwithConvCoeffs,3)
      END SELECT
    END IF
  enddo  ! surfaces

  ! Write number of shadings to initialization output file
  WRITE(OutputFileInits,fmtA) '! <Shading Summary>, Number of Fixed Detached Shades, Number of Building Detached Shades, '//  &
     'Number of Attached Shades'

  WRITE(OutputFileInits,fmtA) ' Shading Summary,'//TRIM(RoundSigDigits(FixedShadingCount))//','//  &
     trim(RoundSigDigits(BuildingShadingCount))//','//  &
     trim(RoundSigDigits(AttachedShadingCount))

  ! Write number of zones header to initialization output file
  WRITE(OutputFileInits,fmtA) '! <Zone Summary>, Number of Zones, Number of Zone Surfaces, Number of SubSurfaces'

  WRITE(OutputFileInits,fmtA) ' Zone Summary,'//TRIM(RoundSigDigits(NumOfZones))//','//  &
     trim(RoundSigDigits(TotSurfaces-FixedShadingCount-BuildingShadingCount-AttachedShadingCount))//','//  &
     trim(RoundSigDigits(SUM(Zone%NumSubSurfaces)))

  ! Write Zone Information header to the initialization output file
  WRITE(OutputFileInits,721)

  DO ZoneNum=1,NumOfZones
    ! Write Zone Information to the initialization output file

    SELECT CASE (Zone(ZoneNum)%InsideConvectionAlgo)
      CASE (ASHRAESimple)
        String1='Simple'
      CASE (ASHRAETARP)
        String1='TARP'
      CASE (CeilingDiffuser)
        String1='CeilingDiffuser'
      CASE (TrombeWall)
        String1='TrombeWall'
      CASE (AdaptiveConvectionAlgorithm)
        String1='AdaptiveConvectionAlgorithm'
    END SELECT

    SELECT CASE (Zone(ZoneNum)%OutsideConvectionAlgo)
      CASE (ASHRAESimple)
        String2='Simple'
      CASE (ASHRAETARP)
        String2='TARP'
      CASE (TarpHcOutside)
        String2='TARP'
      CASE (MoWittHcOutside)
        String2='MoWitt'
      CASE (DOE2HcOutside)
        String2='DOE-2'
!      CASE (BLASTHcOutside)
!        String2='BLAST'
      CASE (AdaptiveConvectionAlgorithm)
        String2='AdaptiveConvectionAlgorithm'

    END SELECT

    IF (Zone(ZoneNum)%isPartOfTotalArea) THEN
      String3='Yes'
    ELSE
      String3='No'
    ENDIF

    WRITE(OutputFileInits,720) TRIM(Zone(ZoneNum)%Name),TRIM(RoundSigDigits(Zone(ZoneNum)%RelNorth,1)),                         &
                             TRIM(RoundSigDigits(Zone(ZoneNum)%OriginX,2)),TRIM(RoundSigDigits(Zone(ZoneNum)%OriginY,2)),       &
                             TRIM(RoundSigDigits(Zone(ZoneNum)%OriginZ,2)),TRIM(RoundSigDigits(Zone(ZoneNum)%Centroid%X,2)),    &
                             TRIM(RoundSigDigits(Zone(ZoneNum)%Centroid%Y,2)),TRIM(RoundSigDigits(Zone(ZoneNum)%Centroid%Z,2)), &
                             TRIM(RoundSigDigits(Zone(ZoneNum)%OfType)),                                                &
                             TRIM(RoundSigDigits(Zone(ZoneNum)%Multiplier)),                                            &
                             TRIM(RoundSigDigits(Zone(ZoneNum)%ListMultiplier)),                                        &
                             TRIM(RoundSigDigits(Zone(ZoneNum)%MinimumX,2)),TRIM(RoundSigDigits(Zone(ZoneNum)%MaximumX,2)),     &
                             TRIM(RoundSigDigits(Zone(ZoneNum)%MinimumY,2)),TRIM(RoundSigDigits(Zone(ZoneNum)%MaximumY,2)),     &
                             TRIM(RoundSigDigits(Zone(ZoneNum)%MinimumZ,2)),TRIM(RoundSigDigits(Zone(ZoneNum)%MaximumZ,2)),     &
                             TRIM(RoundSigDigits(Zone(ZoneNum)%CeilingHeight,2)),TRIM(RoundSigDigits(Zone(ZoneNum)%Volume,2)),  &
                             TRIM(String1),TRIM(String2),TRIM(RoundSigDigits(Zone(ZoneNum)%FloorArea,2)),                       &
                             TRIM(RoundSigDigits(Zone(ZoneNum)%ExtGrossWallArea,2)),                                            &
                             TRIM(RoundSigDigits(Zone(ZoneNum)%ExtNetWallArea,2)),                                              &
                             TRIM(RoundSigDigits(Zone(ZoneNum)%ExtWindowArea,2)),                                               &
                             TRIM(RoundSigDigits(Zone(ZoneNum)%NumSurfaces)),                                                  &
                             TRIM(RoundSigDigits(Zone(ZoneNum)%NumSubSurfaces)),                                               &
                             TRIM(RoundSigDigits(Zone(ZoneNum)%NumShadingSurfaces)),                                           &
                             TRIM(String3)

  END DO ! ZoneNum

! Do the Stratosphere check
  CALL SetOutBulbTempAt(NumOfZones, Zone(1:NumOfZones)%Centroid%Z,           &
       Zone(1:NumOfZones)%OutDryBulbTemp, Zone(1:NumOfZones)%OutWetBulbTemp, 'Zone')

!  IF (ALLOCATED(ZoneSurfacesCount)) DEALLOCATE(ZoneSurfacesCount)
!  IF (ALLOCATED(ZoneSubSurfacesCount)) DEALLOCATE(ZoneSubSurfacesCount)
!  IF (ALLOCATED(ZoneShadingSurfacesCount)) DEALLOCATE(ZoneShadingSurfacesCount)

720 FORMAT(' Zone Information, ',A,28(',',A))
721 FORMAT('! <Zone Information>,Zone Name,North Axis {deg},', &
           'Origin X-Coordinate {m},Origin Y-Coordinate {m},Origin Z-Coordinate {m},', &
           'Centroid X-Coordinate {m},Centroid Y-Coordinate {m},Centroid Z-Coordinate {m},', &
           'Type,Zone Multiplier,Zone List Multiplier,Minimum X {m},Maximum X {m},',         &
           'Minimum Y {m},Maximum Y {m},Minimum Z {m},Maximum Z {m},Ceiling Height {m},Volume {m3},', &
           'Zone Inside Convection Algorithm {Simple-Detailed-CeilingDiffuser-TrombeWall},', &
           'Zone Outside Convection Algorithm {Simple-Detailed-Tarp-MoWitt-DOE-2-BLAST},', &
          ' Floor Area {m2},Exterior Gross Wall Area {m2},Exterior Net Wall Area {m2},Exterior Window Area {m2},', &
          ' Number of Surfaces, Number of SubSurfaces, Number of Shading SubSurfaces, ',  &
          ' Part of Total Building Area')

  RETURN

END SUBROUTINE SetUpZoneGeometry

SUBROUTINE AllocateModuleArrays

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Rick Strand
          !       DATE WRITTEN   February 1998
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine allocates all of the arrays at the module level which
          ! require allocation.

          ! METHODOLOGY EMPLOYED:
          ! Allocation is dependent on the user input file.

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
          ! na

          ! FLOW:

  ALLOCATE (ShadeV(TotSurfaces))
  ShadeV%NVert=0
  ! Individual components (XV,YV,ZV) allocated in routine ProcessSurfaceVertices
  ALLOCATE (X0(TotSurfaces))
  X0=0.0d0
  ALLOCATE (Y0(TotSurfaces))
  Y0=0.0d0
  ALLOCATE (Z0(TotSurfaces))
  Z0=0.0d0

  ALLOCATE (CBZone(NumOfZones))
  CBZone=0.0d0
  ALLOCATE (DSZone(NumOfZones))
  DSZone=0.0d0
  ALLOCATE (DGZone(NumOfZones))
  DGZone=0.0d0
  ALLOCATE (DBZone(NumOfZones))
  DBZone=0.0d0
  ALLOCATE (DBZoneSSG(NumOfZones))
  DBZoneSSG = 0.0d0
  ALLOCATE (QSDifSol(NumOfZones))
  QSDifSol=0.0d0
  ALLOCATE (AISurf(TotSurfaces))
  AISurf=0.0d0
  ALLOCATE (AOSurf(TotSurfaces))
  AOSurf=0.0d0
  ALLOCATE (BmToBmReflFacObs(TotSurfaces))
  BmToBmReflFacObs=0.0d0
  ALLOCATE (BmToDiffReflFacObs(TotSurfaces))
  BmToDiffReflFacObs=0.0d0
  ALLOCATE (BmToDiffReflFacGnd(TotSurfaces))
  BmToDiffReflFacGnd=0.0d0
  ALLOCATE (AWinSurf(TotSurfaces,CFSMAXNL+1))
  AWinSurf=0.0d0
  ALLOCATE (AWinCFOverlap(TotSurfaces,MaxSolidWinLayers))
  AWinCFOverlap=0.0d0

  RETURN

END SUBROUTINE AllocateModuleArrays

SUBROUTINE GetSurfaceData(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Richard Liesen
          !       DATE WRITTEN   November 1997
          !       MODIFIED       April 1999, Linda Lawrie
          !                      Dec. 2000, FW (add "one-wall zone" checks)
          !       RE-ENGINEERED  May 2000, Linda Lawrie (breakout surface type gets)

          ! PURPOSE OF THIS SUBROUTINE:
          ! The purpose of this subroutine is to read in the surface information
          ! from the input data file and interpret and put in the derived type

          ! METHODOLOGY EMPLOYED:
          ! The order of surfaces does not matter and the surfaces are resorted into
          ! the hierarchical order:
          !  Detached Surfaces
          !  Base Surface for zone x
          !    Subsurfaces for base surface
          !  Base Surface for zone x
          !    etc
          !  Heat Transfer Surfaces and Shading surfaces are mixed in the list
          !  Pointers are set in the zones (First, Last)

          ! REFERENCES:
  !   This routine manages getting the input for the following Objects:
  ! SurfaceGeometry
  ! Surface:Shading:Detached
  ! Surface:HeatTransfer
  ! Surface:HeatTransfer:Sub
  ! Surface:Shading:Attached
  ! Surface:InternalMass

  !
  ! Vertex input:
  !  N3 , \field Number of Surface Vertices -- Number of (X,Y,Z) groups in this surface
  !       \note currently limited 3 or 4, later?
  !       \min 3
  !       \max 4
  !       \memo vertices are given in SurfaceGeometry coordinates -- if relative, all surface coordinates
  !       \memo are "relative" to the Zone Origin.  if WCS, then building and zone origins are used
  !       \memo for some internal calculations, but all coordinates are given in an "absolute" system.
  !  N4,  \field Vertex 1 X-coordinate
  !       \units m
  !       \type real
  !  N5 , \field Vertex 1 Y-coordinate
  !       \units m
  !       \type real
  !  N6 , \field Vertex 1 Z-coordinate
  !       \units m
  !       \type real
  !  N7,  \field Vertex 2 X-coordinate
  !       \units m
  !       \type real
  !  N8,  \field Vertex 2 Y-coordinate
  !       \units m
  !       \type real
  !  N9,  \field Vertex 2 Z-coordinate
  !       \units m
  !       \type real
  !  N10, \field Vertex 3 X-coordinate
  !       \units m
  !       \type real
  !  N11, \field Vertex 3 Y-coordinate
  !       \units m
  !       \type real
  !  N12, \field Vertex 3 Z-coordinate
  !       \units m
  !       \type real
  !  N13, \field Vertex 4 X-coordinate
  !       \units m
  !       \type real
  !  N14, \field Vertex 4 Y-coordinate
  !       \type real
  !       \units m
  !  N15; \field Vertex 4 Z-coordinate
  !       \units m
  !       \type real

  ! The vertices are stored in the surface derived type.
  !
  !      +(1)-------------------------(4)+
  !      |                               |
  !      |                               |
  !      |                               |
  !      +(2)-------------------------(3)+
  !
  !  The above diagram shows the actual coordinate points of a typical wall
  !  (you're on the outside looking toward the wall) as stored into
  !  Surface%Vertex(1:<number-of-sides>)
  !
  !
  !


          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList, SameString, VerifyName
  USE General, ONLY: TrimSigDigits,RoundSigDigits
  USE Vectors
  USE ScheduleManager, ONLY: GetScheduleMinValue, GetScheduleMaxValue
  USE DataErrorTracking

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound ! If errors found in input

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: SurfaceClass_Moved=-1
  CHARACTER(len=*), PARAMETER :: RoutineName='GetSurfaceData: '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER :: ConstrNum         ! Construction number
  INTEGER :: SubSurfNum        ! DO loop counter/index for sub-surface number
  INTEGER :: SurfNum           ! DO loop counter/index for surface number
  INTEGER :: ZoneNum           ! DO loop counter (zones)
  INTEGER :: Found             ! For matching interzone surfaces
  INTEGER :: ConstrNumFound    ! Construction number of matching interzone surface
  LOGICAL :: NonMatch=.false.  ! Error for non-matching interzone surfaces
  INTEGER :: Lay               ! Layer number
  INTEGER :: MovedSurfs        ! Number of Moved Surfaces (when sorting into hierarchical structure)
  LOGICAL :: SurfError=.false. ! General Surface Error, causes fatal error at end of routine
  INTEGER :: Loop
  INTEGER :: BaseSurfNum
  INTEGER :: TotLay            ! Total layers in a construction
  INTEGER :: TotLayFound       ! Total layers in the construction of a matching interzone surface
  INTEGER :: TotDetachedFixed  ! Total Shading:Site:Detailed entries
  INTEGER :: TotDetachedBldg   ! Total Shading:Building:Detailed entries
  INTEGER :: TotRectDetachedFixed  ! Total Shading:Site entries
  INTEGER :: TotRectDetachedBldg   ! Total Shading:Building entries
  INTEGER :: TotHTSurfs        ! Number of BuildingSurface:Detailed items to obtain
  INTEGER :: TotDetailedWalls  ! Number of Wall:Detailed items to obtain
  INTEGER :: TotDetailedRoofs  ! Number of RoofCeiling:Detailed items to obtain
  INTEGER :: TotDetailedFloors ! Number of Floor:Detailed items to obtain
  INTEGER :: TotHTSubs         ! Number of FenestrationSurface:Detailed items to obtain
  INTEGER :: TotShdSubs        ! Number of Shading:Zone:Detailed items to obtain
  INTEGER :: TotIntMass        ! Number of InternalMass items to obtain
  ! Simple Surfaces (Rectangular)
  INTEGER :: TotRectExtWalls   ! Number of Exterior Walls to obtain
  INTEGER :: TotRectIntWalls   ! Number of Adiabatic Walls to obtain
  INTEGER :: TotRectIZWalls    ! Number of Interzone Walls to obtain
  INTEGER :: TotRectUGWalls    ! Number of Underground to obtain
  INTEGER :: TotRectRoofs      ! Number of Roofs to obtain
  INTEGER :: TotRectCeilings   ! Number of Adiabatic Ceilings to obtain
  INTEGER :: TotRectIZCeilings ! Number of Interzone Ceilings to obtain
  INTEGER :: TotRectGCFloors   ! Number of Floors with Ground Contact to obtain
  INTEGER :: TotRectIntFloors  ! Number of Adiabatic Walls to obtain
  INTEGER :: TotRectIZFloors   ! Number of Interzone Floors to obtain
  INTEGER :: TotRectWindows
  INTEGER :: TotRectDoors
  INTEGER :: TotRectGlazedDoors
  INTEGER :: TotRectIZWindows
  INTEGER :: TotRectIZDoors
  INTEGER :: TotRectIZGlazedDoors
  INTEGER :: TotOverhangs
  INTEGER :: TotOverhangsProjection
  INTEGER :: TotFins
  INTEGER :: TotFinsProjection
  CHARACTER(len=20) ClassMsg
  CHARACTER(len=20) Msg2
  INTEGER :: OpaqueHTSurfs     ! Number of floors, walls and roofs in a zone
  INTEGER :: OpaqueHTSurfsWithWin ! Number of floors, walls and roofs with windows in a zone
  INTEGER :: InternalMassSurfs ! Number of internal mass surfaces in a zone
  LOGICAL :: RelWarning=.false.
  INTEGER :: ConstrNumSh       ! Shaded construction number for a window
  INTEGER :: LayNumOutside     ! Outside material numbers for a shaded construction
  INTEGER :: BlNum             ! Blind number
  LOGICAL :: WinShadingCtrlReferenced ! True if a WindowShadingControl is referenced by at least one window
  INTEGER :: ShadingCtrl       ! WindowShadingControl number
  INTEGER :: AddedSubSurfaces  ! Subsurfaces (windows) added when windows reference Window5 Data File
                               ! entries with two glazing systems
  INTEGER :: NeedToAddSurfaces ! Surfaces that will be added due to "unentered" other zone surface
  INTEGER :: NeedToAddSubSurfaces ! SubSurfaces that will be added due to "unentered" other zone surface
  INTEGER :: CurNewSurf
  INTEGER :: FirstTotalSurfaces
  INTEGER :: NVert
  INTEGER :: Vert
  INTEGER :: N
  REAL(r64) SurfWorldAz
  REAL(r64) :: surfTilt

  INTEGER :: MultFound, MultSurfNum
  CHARACTER(len=20) :: MultString
  LOGICAL,SAVE :: WarningDisplayed=.false.
  INTEGER, SAVE :: ErrCount1=0
  INTEGER, SAVE :: ErrCount2=0
  INTEGER, SAVE :: ErrCount3=0
  INTEGER, SAVE :: ErrCount4=0  ! counts of interzone area mismatches.
  LOGICAL :: SubSurfaceSevereDisplayed
 ! INTEGER :: Warning4Count=0  ! counts of nonmatched flat surface subsurface orientations
 ! INTEGER :: Warning5Count=0  ! counts of nonmatched flat surface subsurface orientations - could not be resolved
  LOGICAL :: errFlag

  INTEGER :: iTmp1
  INTEGER :: iTmp2
!unused  INTEGER :: SchID
  INTEGER :: BlNumNew
  INTEGER :: WinShadingControlPtr
  INTEGER :: ShadingType
!unused  REAL(r64) :: SchSlatAngle = 0.0D0
!unused  LOGICAL :: initmsg
  INTEGER :: errCount
  REAL(r64) :: diffp
!  TYPE (vector), ALLOCATABLE, DIMENSION(:) :: TestVertex
!  INTEGER :: Vrt
!  INTEGER :: testV
!  INTEGER :: testVsave
!  INTEGER :: countSides
!  INTEGER :: LLCVrt
!  REAL(r64) :: maxX
!  REAL(r64) :: maxY
!  REAL(r64) :: testX
!  REAL(r64) :: testY
  REAL(r64) :: surfAzimuth
!  LOGICAL :: Located
  LOGICAL :: sameSurfNormal
  LOGICAL :: izConstDiff  ! differences in construction for IZ surfaces
  LOGICAL :: izConstDiffMsg  ! display message about hb diffs only once.

          ! FLOW:
! Get the total number of surfaces to allocate derived type and for surface loops

  CALL GetGeometryParameters(ErrorsFound)

  IF (WorldCoordSystem) THEN
    IF (BuildingAzimuth /= 0.0d0) RelWarning=.true.
    DO ZoneNum=1,NumOfZones
      IF (Zone(ZoneNum)%RelNorth /= 0.0d0) RelWarning=.true.
    ENDDO
    IF (RelWarning .and. .not. WarningDisplayed) THEN
      CALL ShowWarningError(RoutineName//'World Coordinate System selected.  '// &
                            'Any non-zero Building/Zone North Axes or non-zero Zone Origins are ignored.')
      CALL ShowContinueError('These may be used in daylighting reference point coordinate calculations '//  &
         ' but not in normal geometry inputs.')
      WarningDisplayed=.true.
    ENDIF
    RelWarning=.false.
    DO ZoneNum=1,NumOfZones
      IF (Zone(ZoneNum)%OriginX /= 0.0d0) RelWarning=.true.
      IF (Zone(ZoneNum)%OriginY /= 0.0d0) RelWarning=.true.
      IF (Zone(ZoneNum)%OriginZ /= 0.0d0) RelWarning=.true.
    ENDDO
    IF (RelWarning .and. .not. WarningDisplayed) THEN
      CALL ShowWarningError(RoutineName//'World Coordinate System selected.  '// &
                            'Any non-zero Building/Zone North Axes or non-zero Zone Origins are ignored.')
      CALL ShowContinueError('These may be used in daylighting reference point coordinate calculations '//  &
         ' but not in normal geometry inputs.')
      WarningDisplayed=.true.
    ENDIF
  ENDIF


  TotDetachedFixed      =GetNumObjectsFound('Shading:Site:Detailed')
  TotDetachedBldg       =GetNumObjectsFound('Shading:Building:Detailed')
  TotRectDetachedFixed  =GetNumObjectsFound('Shading:Site')
  TotRectDetachedBldg   =GetNumObjectsFound('Shading:Building')
  TotHTSurfs            =GetNumObjectsFound('BuildingSurface:Detailed')
  TotDetailedWalls      =GetNumObjectsFound('Wall:Detailed')
  TotDetailedRoofs      =GetNumObjectsFound('RoofCeiling:Detailed')
  TotDetailedFloors     =GetNumObjectsFound('Floor:Detailed')
  TotHTSubs             =GetNumObjectsFound('FenestrationSurface:Detailed')
  TotShdSubs            =GetNumObjectsFound('Shading:Zone:Detailed')
  TotOverhangs          =GetNumObjectsFound('Shading:Overhang')
  TotOverhangsProjection=GetNumObjectsFound('Shading:Overhang:Projection')
  TotFins               =GetNumObjectsFound('Shading:Fin')
  TotFinsProjection     =GetNumObjectsFound('Shading:Fin:Projection')
  TotIntMass            =GetNumObjectsFound('InternalMass')
  TotRectWindows        =GetNumObjectsFound('Window')
  TotRectDoors          =GetNumObjectsFound('Door')
  TotRectGlazedDoors    =GetNumObjectsFound('GlazedDoor')
  TotRectIZWindows      =GetNumObjectsFound('Window:Interzone')
  TotRectIZDoors        =GetNumObjectsFound('Door:Interzone')
  TotRectIZGlazedDoors  =GetNumObjectsFound('GlazedDoor:Interzone')
  TotRectExtWalls       =GetNumObjectsFound('Wall:Exterior')
  TotRectIntWalls       =GetNumObjectsFound('Wall:Adiabatic')
  TotRectIZWalls        =GetNumObjectsFound('Wall:Interzone')
  TotRectUGWalls        =GetNumObjectsFound('Wall:Underground')
  TotRectRoofs          =GetNumObjectsFound('Roof')
  TotRectCeilings       =GetNumObjectsFound('Ceiling:Adiabatic')
  TotRectIZCeilings     =GetNumObjectsFound('Ceiling:Interzone')
  TotRectGCFloors       =GetNumObjectsFound('Floor:GroundContact')
  TotRectIntFloors      =GetNumObjectsFound('Floor:Adiabatic ')
  TotRectIZFloors       =GetNumObjectsFound('Floor:Interzone')

  TotOSC     =0

  TotSurfaces=(TotDetachedFixed+TotDetachedBldg+TotRectDetachedFixed+TotRectDetachedBldg)*2 + &
     TotHTSurfs + TotHTSubs + TotShdSubs*2 + TotIntMass +   &
     TotOverhangs*2 + TotOverhangsProjection*2 + TotFins*4 + TotFinsProjection*4 + &
     TotDetailedWalls + TotDetailedRoofs + TotDetailedFloors + &
     TotRectWindows + TotRectDoors + TotRectGlazedDoors + TotRectIZWindows + TotRectIZDoors + TotRectIZGlazedDoors + &
     TotRectExtWalls + TotRectIntWalls + TotRectIZWalls + TotRectUGWalls + &
     TotRectRoofs + TotRectCeilings + TotRectIZCeilings + &
     TotRectGCFloors + TotRectIntFloors + TotRectIZFloors

  ALLOCATE (SurfaceTmp(TotSurfaces))   ! Allocate the Surface derived type appropriately
    ! SurfaceTmp structure is allocated via derived type initialization.

  SurfNum=0
  AddedSubSurfaces=0
  AskForSurfacesReport=.true.

  CALL GetDetShdSurfaceData(ErrorsFound,SurfNum,TotDetachedFixed,TotDetachedBldg)

  CALL GetRectDetShdSurfaceData(ErrorsFound,SurfNum,TotRectDetachedFixed,TotRectDetachedBldg)

  CALL GetHTSurfaceData(ErrorsFound,SurfNum,TotHTSurfs,TotDetailedWalls,TotDetailedRoofs,TotDetailedFloors,  &
                 BaseSurfCls,BaseSurfIDs,NeedToAddSurfaces)

  CALL GetRectSurfaces(ErrorsFound,SurfNum,TotRectExtWalls,TotRectIntWalls,TotRectIZWalls,TotRectUGWalls,  &
      TotRectRoofs,TotRectCeilings,TotRectIZCeilings,TotRectGCFloors,TotRectIntFloors,TotRectIZFloors,     &
      BaseSurfIDs,NeedToAddSurfaces)

  CALL GetHTSubSurfaceData(ErrorsFound,SurfNum,TotHTSubs,SubSurfCls,SubSurfIDs,AddedSubSurfaces,NeedToAddSubSurfaces)

  CALL GetRectSubSurfaces(ErrorsFound,SurfNum,TotRectWindows,TotRectDoors,TotRectGlazedDoors,  &
      TotRectIZWindows,TotRectIZDoors,TotRectIZGlazedDoors,SubSurfIDs,AddedSubSurfaces,NeedToAddSubSurfaces)

  CALL GetAttShdSurfaceData(ErrorsFound,SurfNum,TotShdSubs)

  CALL GetSimpleShdSurfaceData(ErrorsFound,SurfNum,TotOverhangs,TotOverhangsProjection,TotFins,TotFinsProjection)

  CALL GetIntMassSurfaceData(ErrorsFound,SurfNum,TotIntMass)

  CALL GetMovableInsulationData(ErrorsFound)

  IF(CalcSolRefl) CALL GetShadingSurfReflectanceData(ErrorsFound)

  TotSurfaces=SurfNum + AddedSubSurfaces + NeedToAddSurfaces + NeedToAddSubSurfaces

  ! Have to make room for added surfaces, if needed
  FirstTotalSurfaces=SurfNum + AddedSubSurfaces
  IF (NeedToAddSurfaces+NeedToAddSubsurfaces > 0) THEN
    ALLOCATE(SurfaceTmpSave(TotSurfaces))
    SurfaceTmpSave(1:FirstTotalSurfaces)=SurfaceTmp
    DEALLOCATE(SurfaceTmp)
    ALLOCATE(SurfaceTmp(TotSurfaces))
    SurfaceTmp=SurfaceTmpSave
    DEALLOCATE(SurfaceTmpSave)
  ENDIF

  ALLOCATE (SurfaceWindow(TotSurfaces))

! add the "need to add" surfaces
  if (NeedtoAddSurfaces+NeedToAddSubSurfaces > 0)   &
!Debug    write(outputfiledebug,*) ' need to add ',NeedtoAddSurfaces+NeedToAddSubSurfaces
  CurNewSurf=FirstTotalSurfaces
  DO SurfNum=1,FirstTotalSurfaces
    IF (SurfaceTmp(SurfNum)%ExtBoundCond /= UnenteredAdjacentZoneSurface) CYCLE
    ! Need to add surface
    CurNewSurf=CurNewSurf+1
!Debug    write(outputfiledebug,*) ' adding surface=',curnewsurf
    SurfaceTmp(CurNewSurf)=SurfaceTmp(SurfNum)
    !  Basic parameters are the same for both surfaces.
    Found=FindItemInList(SurfaceTmp(SurfNum)%ExtBoundCondName,Zone%Name,NumOfZones)
    IF (Found == 0) CYCLE
    SurfaceTmp(CurNewSurf)%Zone=Found
    SurfaceTmp(CurNewSurf)%ZoneName=Zone(Found)%Name
    ! Reverse Construction
    SurfaceTmp(CurNewSurf)%Construction=AssignReverseConstructionNumber(SurfaceTmp(SurfNum)%Construction,SurfError)
    SurfaceTmp(CurNewSurf)%ConstructionStoredInputValue  = SurfaceTmp(CurNewSurf)%Construction
    ! Reverse Vertices
    NVert=SurfaceTmp(SurfNum)%Sides
    DO Vert=1,SurfaceTmp(SurfNum)%Sides
      SurfaceTmp(CurNewSurf)%Vertex(Vert)=SurfaceTmp(SurfNum)%Vertex(NVert)
      NVert=NVert-1
    ENDDO
    IF (SurfaceTmp(CurNewSurf)%Sides > 2) THEN
      CALL CreateNewellAreaVector(SurfaceTmp(CurNewSurf)%Vertex,SurfaceTmp(CurNewSurf)%Sides,  &
           SurfaceTmp(CurNewSurf)%NewellAreaVector)
      SurfaceTmp(CurNewSurf)%GrossArea=VecLength(SurfaceTmp(CurNewSurf)%NewellAreaVector)
      SurfaceTmp(CurNewSurf)%Area=SurfaceTmp(CurNewSurf)%GrossArea
      SurfaceTmp(CurNewSurf)%NetAreaShadowCalc = SurfaceTmp(CurNewSurf)%Area
      CALL CreateNewellSurfaceNormalVector(SurfaceTmp(CurNewSurf)%Vertex,SurfaceTmp(CurNewSurf)%Sides,  &
           SurfaceTmp(CurNewSurf)%NewellSurfaceNormalVector)
      CALL DetermineAzimuthAndTilt(SurfaceTmp(CurNewSurf)%Vertex,SurfaceTmp(CurNewSurf)%Sides,SurfWorldAz,SurfTilt,  &
                                   SurfaceTmp(CurNewSurf)%lcsx,SurfaceTmp(CurNewSurf)%lcsy,SurfaceTmp(CurNewSurf)%lcsz,  &
                                   SurfaceTmp(CurNewSurf)%GrossArea,SurfaceTmp(CurNewSurf)%NewellSurfaceNormalVector)
      SurfaceTmp(CurNewSurf)%Azimuth=SurfWorldAz
      SurfaceTmp(CurNewSurf)%Tilt=SurfTilt

      ! Sine and cosine of azimuth and tilt
      SurfaceTmp(CurNewSurf)%SinAzim = SIN(SurfWorldAz*DegToRadians)
      SurfaceTmp(CurNewSurf)%CosAzim = COS(SurfWorldAz*DegToRadians)
      SurfaceTmp(CurNewSurf)%SinTilt = SIN(SurfTilt*DegToRadians)
      SurfaceTmp(CurNewSurf)%CosTilt = COS(SurfTilt*DegToRadians)
      ! Outward normal unit vector (pointing away from room)
      SurfaceTmp(CurNewSurf)%OutNormVec = SurfaceTmp(CurNewSurf)%NewellSurfaceNormalVector
      DO N=1,3
        IF (ABS(SurfaceTmp(CurNewSurf)%OutNormVec(N)-1.0d0) < 1.d-06) SurfaceTmp(CurNewSurf)%OutNormVec(N) = +1.0d0
        IF (ABS(SurfaceTmp(CurNewSurf)%OutNormVec(N)+1.0d0) < 1.d-06) SurfaceTmp(CurNewSurf)%OutNormVec(N) = -1.0d0
        IF (ABS(SurfaceTmp(CurNewSurf)%OutNormVec(N))     < 1.d-06) SurfaceTmp(CurNewSurf)%OutNormVec(N) =  0.0d0
      ENDDO

      ! Can perform tests on this surface here
      SurfaceTmp(CurNewSurf)%ViewFactorSky=0.5d0*(1.d0+SurfaceTmp(CurNewSurf)%CosTilt)
      SurfaceTmp(CurNewSurf)%ViewFactorGround=0.5d0*(1.d0-SurfaceTmp(CurNewSurf)%CosTilt)

      ! The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
      ! surfaces
      SurfaceTmp(CurNewSurf)%ViewFactorSkyIR = SurfaceTmp(CurNewSurf)%ViewFactorSky
      SurfaceTmp(CurNewSurf)%ViewFactorGroundIR = 0.5d0*(1.d0-SurfaceTmp(CurNewSurf)%CosTilt)
    ENDIF


    ! Change Name
    SurfaceTmp(CurNewSurf)%Name='iz-'//TRIM(SurfaceTmp(SurfNum)%Name)
!Debug   write(outputfiledebug,*) ' new surf name=',trim(SurfaceTmp(CurNewSurf)%Name)
!Debug   write(outputfiledebug,*) ' new surf in zone=',trim(surfacetmp(curnewsurf)%zoneName)
    SurfaceTmp(CurNewSurf)%ExtBoundCond=UnreconciledZoneSurface
    SurfaceTmp(SurfNum)%ExtBoundCond=UnreconciledZoneSurface
    SurfaceTmp(CurNewSurf)%ExtBoundCondName=SurfaceTmp(SurfNum)%Name
    SurfaceTmp(SurfNum)%ExtBoundCondName=SurfaceTmp(CurNewSurf)%Name
    IF (SurfaceTmp(CurNewSurf)%Class == SurfaceClass_Roof .or.   &
        SurfaceTmp(CurNewSurf)%Class == SurfaceClass_Wall .or.   &
        SurfaceTmp(CurNewSurf)%Class == SurfaceClass_Floor) THEN
        ! base surface
        IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Roof) THEN
          SurfaceTmp(CurNewSurf)%Class=SurfaceClass_Floor
!Debug          write(outputfiledebug,*) ' new surfaces is a floor'
        ELSE IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Floor) THEN
          SurfaceTmp(CurNewSurf)%Class=SurfaceClass_Roof
!Debug          write(outputfiledebug,*) ' new surfaces is a roof'
        ENDIF
        SurfaceTmp(CurNewSurf)%BaseSurf=CurNewSurf
        SurfaceTmp(CurnewSurf)%BaseSurfName=SurfaceTmp(CurNewSurf)%Name
!Debug        write(outputfiledebug,*) ' basesurf, extboundcondname=',trim(SurfaceTmp(CurNewSurf)%ExtBoundCondName)
    ELSE
        ! subsurface
      Found=FindIteminList('iz-'//SurfaceTmp(SurfNum)%BaseSurfName,SurfaceTmp%Name,FirstTotalSurfaces+CurNewSurf-1)
      IF (Found > 0) THEN
        SurfaceTmp(CurNewSurf)%BaseSurfName='iz-'//SurfaceTmp(SurfNum)%BaseSurfName
        SurfaceTmp(CurNewSurf)%BaseSurf=Found
        SurfaceTmp(Found)%Area =  &
               SurfaceTmp(Found)%Area - SurfaceTmp(CurNewSurf)%Area
        IF (SurfaceTmp(CurNewSurf)%Class == SurfaceClass_Window .OR.SurfaceTmp(CurNewSurf)%Class == SurfaceClass_GlassDoor) THEN
          SurfaceTmp(Found)%NetAreaShadowCalc =  &
          SurfaceTmp(Found)%NetAreaShadowCalc - SurfaceTmp(CurNewSurf)%Area/ &
            SurfaceTmp(CurNewSurf)%Multiplier
        ELSE ! Door, TDD:Diffuser, TDD:DOME
          SurfaceTmp(Found)%NetAreaShadowCalc =  &
          SurfaceTmp(Found)%NetAreaShadowCalc - SurfaceTmp(CurNewSurf)%Area
        ENDIF
        SurfaceTmp(CurNewSurf)%ExtBoundCond=SurfaceTmp(Found)%ExtBoundCond
        SurfaceTmp(CurNewSurf)%ExtBoundCondName=SurfaceTmp(SurfNum)%Name
        SurfaceTmp(CurNewSurf)%ExtSolar=SurfaceTmp(Found)%ExtSolar
        SurfaceTmp(CurNewSurf)%ExtWind=SurfaceTmp(Found)%ExtWind
        SurfaceTmp(CurNewSurf)%Zone=SurfaceTmp(Found)%Zone
        SurfaceTmp(CurNewSurf)%ZoneName=SurfaceTmp(Found)%ZoneName
        SurfaceTmp(CurNewSurf)%OSCPtr=SurfaceTmp(Found)%OSCPtr
!Debug        write(outputfiledebug,*) ' subsurf, extboundcondname=',trim(SurfaceTmp(CurNewSurf)%ExtBoundCondName)
!Debug        write(outputfiledebug,*) ' subsurf, basesurf=',trim('iz-'//SurfaceTmp(SurfNum)%BaseSurfName)
      ELSE
        CALL ShowSevereError(RoutineName//'Adding unentered subsurface, could not find base surface='// &
            TRIM('iz-'//SurfaceTmp(SurfNum)%BaseSurfName))
        SurfError=.true.
      ENDIF
    ENDIF

  ENDDO
!**********************************************************************************
! After all of the surfaces have been defined then the base surfaces for the
! sub-surfaces can be defined.  Loop through surfaces and match with the sub-surface
! names.
  DO SurfNum= 1, TotSurfaces
    IF (.not. SurfaceTmp(SurfNum)%HeatTransSurf) CYCLE

    ! why are we doing this again?  this should have already been done.
    IF (SameString(SurfaceTmp(SurfNum)%BaseSurfName,SurfaceTmp(SurfNum)%Name)) THEN
      Found=SurfNum
    ELSE
      Found=FindIteminList(SurfaceTmp(SurfNum)%BaseSurfName,SurfaceTmp%Name,TotSurfaces)
    ENDIF
    If (Found > 0) THEN
      SurfaceTmp(SurfNum)%BaseSurf=Found
      IF (SurfNum /= Found) THEN  ! for subsurfaces
        IF (SurfaceTmp(SurfNum)%HeatTransSurf) SurfaceTmp(Found)%NumSubSurfaces=SurfaceTmp(Found)%NumSubSurfaces+1
        IF (SurfaceTmp(SurfNum)%Class < SurfaceClass_Window .or.   &
            SurfaceTmp(SurfNum)%Class > SurfaceClass_TDD_Diffuser) THEN
          IF (SurfaceTmp(SurfNum)%Class == 0) THEN
            CALL ShowSevereError(RoutineName//'Invalid SubSurface detected, Surface='//TRIM(SurfaceTmp(SurfNum)%Name))
          ELSE
            CALL ShowSevereError(RoutineName//'Invalid SubSurface detected, Surface='//TRIM(SurfaceTmp(SurfNum)%Name)//  &
               ', class='//TRIM(BaseSurfCls(SurfaceTmp(SurfNum)%Class))//' invalid class for subsurface')
            SurfError=.true.
          ENDIF
        ENDIF
      ENDIF
    End If

  END DO  ! ...end of the Surface DO loop for finding BaseSurf
!**********************************************************************************

!**********************************************************************************
! orientation of flat subsurfaces (window/door/etc) need to match base surface
! CR8628
!  ALLOCATE(TestVertex(4)) ! subsurfaces we will look at have max of 4 vertices
  DO SurfNum=1,TotSurfaces
    IF (.not. SurfaceTmp(SurfNum)%HeatTransSurf) CYCLE
    ! If flat surface
    surfAzimuth = SurfaceTmp(SurfNum)%Azimuth
    surfTilt    = SurfaceTmp(SurfNum)%Tilt
    IF (ABS(surfTilt) <= 1.d-5 .or. ABS(surfTilt-180.d0) <= 1.d-5)  THEN
      ! see if there are any subsurfaces on roofs/floors
      DO itmp1=1,TotSurfaces
        IF (itmp1 == SurfNum) CYCLE
        IF (.not. SurfaceTmp(itmp1)%BaseSurf == SurfNum) CYCLE
        IF (.not. SurfaceTmp(itmp1)%HeatTransSurf) CYCLE
!          write(outputfiledebug,'(A)') 'roof/floor basesurface='//trim(SurfaceTmp(SurfNum)%Name)
!          write(outputfiledebug,'(A,3f7.2)') 'basesurface lc vectors=',SurfaceTmp(SurfNum)%lcsx
!          write(outputfiledebug,'(3f7.2)') SurfaceTmp(SurfNum)%lcsy
!          write(outputfiledebug,'(3f7.2)') SurfaceTmp(SurfNum)%lcsz
!          write(outputfiledebug,'(A,3f7.2)') 'basesurface surfnorm=',SurfaceTmp(SurfNum)%NewellSurfaceNormalVector
!          write(outputfiledebug,'(A)') 'subsurface='//trim(SurfaceTmp(itmp1)%Name)
!          write(outputfiledebug,'(A,3f7.2)') 'subsurface lc vectors=',SurfaceTmp(itmp1)%lcsx
!          write(outputfiledebug,'(3f7.2)') SurfaceTmp(itmp1)%lcsy
!          write(outputfiledebug,'(3f7.2)') SurfaceTmp(itmp1)%lcsz
!          write(outputfiledebug,'(A,3f7.2)') 'subsurface surfnorm=',SurfaceTmp(itmp1)%NewellSurfaceNormalVector
        IF (ABS(SurfaceTmp(itmp1)%Azimuth-surfAzimuth) <= 10.d0) CYCLE
        CALL CompareTwoVectors(SurfaceTmp(SurfNum)%NewellSurfaceNormalVector,SurfaceTmp(itmp1)%NewellSurfaceNormalVector,  &
           sameSurfNormal,.001d0)
        IF (sameSurfNormal) THEN  ! copy lcs vectors
          SurfaceTmp(itmp1)%lcsx=SurfaceTmp(SurfNum)%lcsx
          SurfaceTmp(itmp1)%lcsy=SurfaceTmp(SurfNum)%lcsy
          SurfaceTmp(itmp1)%lcsy=SurfaceTmp(SurfNum)%lcsy
          CYCLE
        ENDIF
!        IF (ABS(SurfaceTmp(itmp1)%Azimuth-360.0d0) < .01d0) THEN
!          SurfaceTmp(itmp1)%Azimuth=360.0d0-SurfaceTmp(itmp1)%Azimuth
!        ENDIF
!        IF (ABS(surfAzimuth-360.0d0) < .01d0) THEN
!          surfAzimuth=360.0d0-surfAzimuth
!          SurfaceTmp(SurfNum)%Azimuth=surfAzimuth
!        ENDIF
!        IF (ABS(SurfaceTmp(itmp1)%Azimuth-surfAzimuth) <= 10.d0) CYCLE
        ! have subsurface of base surface
        ! warning error
!        Warning4Count=Warning4Count+1
!        IF (Warning4Count == 1 .and. .not. DisplayExtraWarnings) THEN
!          CALL ShowSevereError(Routinename//'Some Outward Facing angles of subsurfaces differ '//  &
!                               'significantly from flat roof/floor base surface.')
!          CALL ShowContinueError('Fixes will be attempted to align subsurface with base surface.')
!          CALL ShowContinueError('...use Output:Diagnostics,DisplayExtraWarnings; '//  &
!                   'to show more details on individual surfaces.')
!        ENDIF
        IF (DisplayExtraWarnings) THEN
          CALL ShowSevereError(Routinename//'Outward facing angle ['//  &
               TRIM(RoundSigDigits(SurfaceTmp(itmp1)%Azimuth,3))// &
               '] of subsurface="'//TRIM(SurfaceTmp(itmp1)%Name)//  &
               '" significantly different than')
          CALL ShowContinueError('..facing angle ['//TRIM(RoundSigDigits(SurfaceTmp(SurfNum)%Azimuth,3))//  &
              '] of base surface='//TRIM(SurfaceTmp(SurfNum)%Name))
          CALL ShowContinueError('..surface class of base surface='//TRIM(cSurfaceClass(SurfaceTmp(SurfNum)%Class)))
!          CALL ShowContinueError('Fixes will be attempted to align subsurface with base surface.')
        ENDIF
!        Vrt=1
!        testV=2
!        testVsave=2
!        Located=.false.
!        DO CountSides=1,SurfaceTmp(itmp1)%Sides
!          DO Vrt=1,SurfaceTmp(itmp1)%Sides
!            TestVertex(Vrt)=SurfaceTmp(itmp1)%Vertex(testV)
!            testV=testV+1
!            if (testV > SurfaceTmp(itmp1)%Sides) testV=1
!          ENDDO
!          CALL CreateNewellSurfaceNormalVector(TestVertex,SurfaceTmp(itmp1)%Sides,  &
!                SurfaceTmp(itmp1)%NewellSurfaceNormalVector)
!          CALL DetermineAzimuthAndTilt(TestVertex,SurfaceTmp(itmp1)%Sides,SurfWorldAz,surfTilt,  &
!                                SurfaceTmp(itmp1)%lcsx,SurfaceTmp(itmp1)%lcsy,SurfaceTmp(itmp1)%lcsz,  &
!                                SurfaceTmp(itmp1)%GrossArea,SurfaceTmp(itmp1)%NewellSurfaceNormalVector)
!          IF (ABS(surfAzimuth-surfWorldAz) <= 1.d-5) THEN  ! found it
!            DO Vrt=1,SurfaceTmp(itmp1)%Sides
!              SurfaceTmp(itmp1)%Vertex(Vrt)=TestVertex(Vrt)
!            ENDDO
!            SurfaceTmp(itmp1)%Azimuth=SurfWorldAz
!            SurfaceTmp(itmp1)%Tilt=surfTilt
!            Located=.true.
!            EXIT
!          ENDIF
!          testV=testVsave+1
!          IF (testV > SurfaceTmp(itmp1)%Sides) EXIT
!        ENDDO
!        IF (.not. Located) THEN
!          Warning5Count=Warning5Count+1
!          ! another warning
!          IF (DisplayExtraWarnings) THEN
!            CALL ShowContinueError('Fix could not be accomplished.  Original orientation is retained.')
!          ENDIF
!          CALL CreateNewellSurfaceNormalVector(SurfaceTmp(itmp1)%Vertex,SurfaceTmp(itmp1)%Sides,  &
!                  SurfaceTmp(itmp1)%NewellSurfaceNormalVector)
!          CALL DetermineAzimuthAndTilt(SurfaceTmp(itmp1)%Vertex,SurfaceTmp(itmp1)%Sides,SurfWorldAz,surfTilt,  &
!                                SurfaceTmp(itmp1)%lcsx,SurfaceTmp(itmp1)%lcsy,SurfaceTmp(itmp1)%lcsz,  &
!                                SurfaceTmp(itmp1)%GrossArea,SurfaceTmp(itmp1)%NewellSurfaceNormalVector)
!          SurfaceTmp(itmp1)%Azimuth=SurfWorldAz
!          SurfaceTmp(itmp1)%Tilt=surfTilt
!        ENDIF
      ENDDO
    ENDIF
  ENDDO
!  IF (Warning5Count > 0) THEN
!    CALL ShowMessage(RoutineName//'There were '//trim(RoundSigDigits(Warning5Count))//  &
!       ' subsurfaces whose orientation (azimuth) could not be fixed to align with the base surface.')
!    CALL ShowMessage('Shadowing calculations may be inaccurate. Use Output:Diagnostics,DisplayExtraWarnings; for details.')
!  ENDIF
!  DEALLOCATE(TestVertex)

! The surfaces need to be hierarchical.  Input is allowed to be in any order.  In
! this section it is reordered into:

!    Detached shadowing surfaces
!    For each zone:
!      For each Wall
!        subsurfaces (windows, doors, shading) for that wall
!      For each Floor
!        subsurfaces for that floor
!      For each Roof
!        subsurfaces for that roof/ceiling
!      Internal Mass
!
!    After reordering, MovedSurfs should equal TotSurfaces

  MovedSurfs=0
  ALLOCATE (Surface(TotSurfaces))   ! Allocate the Surface derived type appropriately

  ! Move all Detached Surfaces to Front

  DO SurfNum=1,TotSurfaces
    IF (SurfaceTmp(SurfNum)%Class /= SurfaceClass_Detached_F .and.  &
        SurfaceTmp(SurfNum)%Class /= SurfaceClass_Detached_B .and.  &
        SurfaceTmp(SurfNum)%Class /= SurfaceClass_Shading) CYCLE

    !  A shading surface

    MovedSurfs=MovedSurfs+1
    Surface(MovedSurfs)=SurfaceTmp(SurfNum)
    SurfaceTmp(SurfNum)%Class=SurfaceClass_Moved !'Moved'
  ENDDO

  !  For each zone

  DO ZoneNum=1,NumOfZones

    !  For each Base Surface Type (Wall, Floor, Roof)

    DO Loop=1,3

      DO SurfNum=1,TotSurfaces

        IF (SurfaceTmp(SurfNum)%Zone == 0) CYCLE

        IF (.not. SameString(SurfaceTmp(SurfNum)%ZoneName,Zone(ZoneNum)%Name)) CYCLE
        IF (SurfaceTmp(SurfNum)%Class /= BaseSurfIDs(Loop)) CYCLE

        MovedSurfs=MovedSurfs+1
        Surface(MovedSurfs)=SurfaceTmp(SurfNum)
        SurfaceTmp(SurfNum)%Class=SurfaceClass_Moved ! 'Moved'
        SurfaceTmp(SurfNum)%BaseSurf=-1 ! Default has base surface = base surface
        BaseSurfNum=MovedSurfs
        Surface(MovedSurfs)%BaseSurf=BaseSurfNum

        !  Find all subsurfaces to this surface
        DO SubSurfNum=1,TotSurfaces

          IF (SurfaceTmp(SubSurfNum)%Zone == 0) CYCLE
          IF (SurfaceTmp(SubSurfNum)%BaseSurf /= SurfNum) CYCLE

          MovedSurfs=MovedSurfs+1
          Surface(MovedSurfs)=SurfaceTmp(SubSurfNum)
          SurfaceTmp(SubSurfNum)%Class=SurfaceClass_Moved ! 'Moved'
          Surface(MovedSurfs)%BaseSurf=BaseSurfNum
          SurfaceTmp(SubSurfNum)%BaseSurf=-1
        ENDDO
      ENDDO
    ENDDO

    DO SurfNum=1,TotSurfaces

      IF (SurfaceTmp(SurfNum)%ZoneName /= Zone(ZoneNum)%Name) CYCLE
      IF (SurfaceTmp(SurfNum)%Class /= SurfaceClass_IntMass) CYCLE

      MovedSurfs=MovedSurfs+1
      Surface(MovedSurfs)=SurfaceTmp(SurfNum)
      Surface(MovedSurfs)%BaseSurf=MovedSurfs
      SurfaceTmp(SurfNum)%Class=SurfaceClass_Moved ! 'Moved'
    ENDDO
  ENDDO

  IF (MovedSurfs /= TotSurfaces) THEN
    WRITE(ClassMsg,*) MovedSurfs
    ClassMsg=ADJUSTL(ClassMsg)
    WRITE(Msg2,*) TotSurfaces
    Msg2=ADJUSTL(Msg2)
    CALL ShowSevereError(RoutineName//'Reordered # of Surfaces ('//TRIM(ClassMsg)//  &
       ') not = Total # of Surfaces ('//TRIM(Msg2)//')')
    SurfError=.true.
    DO Loop=1,TotSurfaces
      IF (SurfaceTmp(Loop)%Class /= SurfaceClass_Moved) THEN
         IF (SurfaceTmp(Loop)%Class > 100) THEN
           CALL ShowSevereError(RoutineName//'Error in Surface= "'//TRIM(SurfaceTmp(Loop)%Name)//  &
               '" Class='//TRIM(cSurfaceClass(SurfaceTmp(Loop)%Class-100))// &
               ' indicated Zone="'//TRIM(SurfaceTmp(Loop)%ZoneName)//'"')
        ENDIF
      ENDIF
    ENDDO
    CALL ShowWarningError(RoutineName//'Remaining surface checks will use "reordered number of surfaces", '//  &
       'not number of original surfaces')
  ENDIF

  DEALLOCATE (SurfaceTmp)     ! DeAllocate the Temp Surface derived type


  !  For each Base Surface Type (Wall, Floor, Roof)

  DO Loop=1,3

    DO SurfNum=1,TotSurfaces

      IF (Surface(SurfNum)%Zone == 0) CYCLE

      IF (Surface(SurfNum)%Class /= BaseSurfIDs(Loop)) CYCLE

      !  Find all subsurfaces to this surface
      DO SubSurfNum=1,TotSurfaces

        IF (SurfNum == SubSurfNum) CYCLE
        IF (Surface(SubSurfNum)%Zone == 0) CYCLE
        IF (Surface(SubSurfNum)%BaseSurf /= SurfNum) CYCLE

        ! Check facing angle of Sub compared to base
        ! ignore problems of subsurfaces on roofs/ceilings/floors with azimuth
!          IF (Surface(SurfNum)%Class == SurfaceClass_Roof .or. Surface(SurfNum)%Class == SurfaceClass_Floor) CYCLE
!          write(outputfiledebug,'(A)') 'basesurface='//trim(surface(SurfNum)%Name)
!          write(outputfiledebug,'(A,3F7.2)') 'basesurface lc vectors=',Surface(SurfNum)%lcsx
!          write(outputfiledebug,'(3f7.2)') Surface(SurfNum)%lcsy
!          write(outputfiledebug,'(3f7.2)') Surface(SurfNum)%lcsz
!          write(outputfiledebug,'(A,3f7.2)') 'basesurface surfnorm=',Surface(SurfNum)%NewellSurfaceNormalVector
!          write(outputfiledebug,'(A)') 'subsurface='//trim(surface(SubSurfNum)%Name)
!          write(outputfiledebug,'(A,3F7.2)') 'subsurface lc vectors=',Surface(SubSurfNum)%lcsx
!          write(outputfiledebug,'(3f7.2)') Surface(SubSurfNum)%lcsy
!          write(outputfiledebug,'(3f7.2)') Surface(SubSurfNum)%lcsz
!          write(outputfiledebug,'(A,3f7.2)') 'subsurface surfnorm=',Surface(SubSurfNum)%NewellSurfaceNormalVector
        IF (ABS(Surface(SubSurfNum)%Azimuth-Surface(SurfNum)%Azimuth) <= 30.0d0) CYCLE
        CALL CompareTwoVectors(Surface(SurfNum)%NewellSurfaceNormalVector,Surface(SubSurfNum)%NewellSurfaceNormalVector,  &
           sameSurfNormal,.001d0)
        IF (sameSurfNormal) THEN  ! copy lcs vectors
          Surface(SubSurfNum)%lcsx=Surface(SurfNum)%lcsx
          Surface(SubSurfNum)%lcsy=Surface(SurfNum)%lcsy
          Surface(SubSurfNum)%lcsy=Surface(SurfNum)%lcsy
          CYCLE
        ENDIF
        IF (ABS(Surface(SubSurfNum)%Azimuth-360.0d0) < .01d0) THEN
          Surface(SubSurfNum)%Azimuth=360.0d0-Surface(SubSurfNum)%Azimuth
        ENDIF
        IF (ABS(Surface(SurfNum)%Azimuth-360.0d0) < .01d0) THEN
          Surface(SurfNum)%Azimuth=360.0d0-Surface(SurfNum)%Azimuth
        ENDIF
        IF (ABS(Surface(SubSurfNum)%Azimuth-Surface(SurfNum)%Azimuth) > 30.0d0) THEN
          IF (ABS(Surface(SurfNum)%SinTilt) > .17d0) THEN
            ErrCount1=ErrCount1+1
            IF (ErrCount1 == 1 .and. .not. DisplayExtraWarnings) THEN
              CALL ShowSevereError(Routinename//'Some Outward Facing angles of subsurfaces differ '//  &
                                   'significantly from base surface.')
              CALL ShowContinueError('...use Output:Diagnostics,DisplayExtraWarnings; '//  &
                       'to show more details on individual surfaces.')
            ENDIF
            IF (DisplayExtraWarnings) THEN
              CALL ShowSevereError(Routinename//'Outward facing angle ['//  &
                   TRIM(RoundSigDigits(Surface(SubSurfNum)%Azimuth,1))// &
                   '] of subsurface="'//TRIM(Surface(SubSurfNum)%Name)//  &
                   '" significantly different than')
              CALL ShowContinueError('..facing angle ['//TRIM(RoundSigDigits(Surface(SurfNum)%Azimuth,1))//  &
                  '] of base surface='//TRIM(Surface(SurfNum)%Name)//' Tilt='//TRIM(RoundSigDigits(Surface(SurfNum)%Tilt,1)))
              CALL ShowContinueError('..surface class of base surface='//TRIM(cSurfaceClass(Surface(SurfNum)%Class)))
            ENDIF
          ENDIF
!            SurfError=.true.
        ENDIF
      ENDDO
    ENDDO
  ENDDO

!**********************************************************************************
! Now, match up interzone surfaces
!
  NonMatch=.false.
  izConstDiffMsg=.false.
  DO SurfNum= 1, MovedSurfs !TotSurfaces
    !  Clean up Shading Surfaces, make sure they don't go through here.
    !  Shading surfaces have "Zone=0", should also have "BaseSurf=0"
    !  PGE: Revised so that shading surfaces can have BaseSurf /= 0 if they are daylighting shelves
    !       or other exterior reflecting surfaces.
    !IF (Surface(SurfNum)%Zone == 0) THEN
    !  Surface(SurfNum)%BaseSurf=0
    !  CYCLE
    !ENDIF
    IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
    !   If other surface, match it up
    !  Both interzone and "internal" surfaces have this pointer set
    !  Internal surfaces point to themselves, Interzone to another
    IF (Surface(SurfNum)%ExtBoundCond == UnreconciledZoneSurface) THEN
      IF (Surface(SurfNum)%ExtBoundCondName /= '   ') THEN
        IF (Surface(SurfNum)%ExtBoundCondName == Surface(SurfNum)%Name) THEN
          Found=SurfNum
        ELSE
          Found=FindIteminList(Surface(SurfNum)%ExtBoundCondName,Surface%Name,MovedSurfs)
        ENDIF
        IF (Found /= 0) THEN
          Surface(SurfNum)%ExtBoundCond=Found
          ! Check that matching surface is also "OtherZoneSurface"
          IF (Surface(Found)%ExtBoundCond <= 0 .and. Surface(Found)%ExtBoundCond /= UnreconciledZoneSurface) THEN
            CALL ShowSevereError(RoutineName//'Potential "OtherZoneSurface" is not matched correctly:')

            CALL ShowContinueError('Surface='//TRIM(Surface(SurfNum)%Name)//', Zone='//TRIM(Surface(SurfNum)%ZoneName))
            CALL ShowContinueError('Nonmatched Other/InterZone Surface='//TRIM(Surface(Found)%Name)//', Zone='//  &
                                   TRIM(Surface(Found)%ZoneName))
            SurfError=.true.
          ENDIF
          ! Check that matching interzone surface has construction with reversed layers
          IF (Found /= SurfNum) THEN  ! Interzone surface
            ! Make sure different zones too (CR 4110)
            IF (Surface(SurfNum)%Zone == Surface(Found)%Zone) THEN
              ErrCount2=ErrCount2+1
              IF (ErrCount2 == 1 .and. .not. DisplayExtraWarnings) THEN
                CALL ShowWarningError(RoutineName//'CAUTION -- Interzone surfaces are occuring in the same zone(s).')
                CALL ShowContinueError('...use Output:Diagnostics,DisplayExtraWarnings; '//  &
                      'to show more details on individual occurrences.')
              ENDIF
              IF (DisplayExtraWarnings) THEN
                CALL ShowWarningError(RoutineName//'CAUTION -- Interzone surfaces are usually in different zones')
                CALL ShowContinueError('Surface='//TRIM(Surface(SurfNum)%Name)//', Zone='//TRIM(Surface(SurfNum)%ZoneName))
                CALL ShowContinueError('Surface='//TRIM(Surface(Found)%Name)//', Zone='//TRIM(Surface(Found)%ZoneName))
              ENDIF
            ENDIF
            ConstrNum = Surface(SurfNum)%Construction
            ConstrNumFound = Surface(Found)%Construction
            IF (ConstrNum <= 0 .or. ConstrNumFound <= 0) CYCLE
            IF (Construct(ConstrNum)%ReverseConstructionNumLayersWarning .and.   &
                Construct(ConstrNumFound)%ReverseConstructionNumLayersWarning) CYCLE
            IF (Construct(ConstrNum)%ReverseConstructionLayersOrderWarning .and.   &
                Construct(ConstrNumFound)%ReverseConstructionLayersOrderWarning) CYCLE
            TotLay = Construct(ConstrNum)%TotLayers
            TotLayFound = Construct(ConstrNumFound)%TotLayers
            IF(TotLay /= TotLayFound) THEN  ! Different number of layers
              ! match on like Uvalues (nominal)
              IF (ABS(NominalU(ConstrNum)-NominalU(ConstrNumFound)) > .001d0) THEN
                CALL ShowSevereError(RoutineName//'Construction '//TRIM(Construct(ConstrNum)%Name)// &
                                     ' of interzone surface '//TRIM(Surface(SurfNum)%Name)// &
                                     ' does not have the same number of layers as the construction '   &
                                     //TRIM(Construct(ConstrNumFound)%Name)//  &
                                     ' of adjacent surface '//TRIM(Surface(Found)%Name))
                IF (.not. Construct(ConstrNum)%ReverseConstructionNumLayersWarning .or.  &
                    .not. Construct(ConstrNumFound)%ReverseConstructionNumLayersWarning) THEN
                  CALL ShowContinueError('...this problem for this pair will not be reported again.')
                  Construct(ConstrNum)%ReverseConstructionNumLayersWarning=.true.
                  Construct(ConstrNumFound)%ReverseConstructionNumLayersWarning=.true.
                ENDIF
                SurfError=.true.
              ENDIF
            ELSE                            ! Same number of layers; check for reverse layers
              ! check layers as number of layers is the same
              izConstDiff=.false.
              ! ok if same nominal U
              DO Lay = 1,TotLay
                IF(Construct(ConstrNum)%LayerPoint(Lay) /=  &
                   Construct(ConstrNumFound)%LayerPoint(TotLay-Lay+1)) THEN
                   izConstDiff=.true.
                   EXIT  ! exit when diff
                  END IF
              END DO
              IF (izConstDiff .and. ABS(NominalU(ConstrNum)-NominalU(ConstrNumFound)) > .001d0) THEN
                CALL ShowSevereError(RoutineName//'Construction '//TRIM(Construct(ConstrNum)%Name)// &
                                    ' of interzone surface '//TRIM(Surface(SurfNum)%Name)// &
                                    ' does not have the same materials in the reverse order as the construction '   &
                                    //TRIM(Construct(ConstrNumFound)%Name)//  &
                                    ' of adjacent surface '//TRIM(Surface(Found)%Name))
                IF (.not. Construct(ConstrNum)%ReverseConstructionLayersOrderWarning .or.  &
                    .not. Construct(ConstrNumFound)%ReverseConstructionLayersOrderWarning) THEN
                  CALL ShowContinueError('...this problem for this pair will not be reported again.')
                  Construct(ConstrNum)%ReverseConstructionLayersOrderWarning=.true.
                  Construct(ConstrNumFound)%ReverseConstructionLayersOrderWarning=.true.
                ENDIF
                SurfError=.true.
              ELSEIF (izConstDiff) THEN
                CALL ShowWarningError(RoutineName//'Construction '//TRIM(Construct(ConstrNum)%Name)// &
                                         ' of interzone surface '//TRIM(Surface(SurfNum)%Name)// &
                                         ' does not have the same materials in the reverse order as the construction '   &
                                         //TRIM(Construct(ConstrNumFound)%Name)//  &
                                         ' of adjacent surface '//TRIM(Surface(Found)%Name))
                CALL ShowContinueError('...but Nominal U values are similar, diff=['//  &
                   trim(RoundSigDigits(ABS(NominalU(ConstrNum)-NominalU(ConstrNumFound)),4))//'] ... simulation proceeds.')
                IF (.not. izConstDiffMsg) THEN
                  CALL ShowContinueError('...if the two zones are expected to have significantly different temperatures, '//  &
                     'the proper "reverse" construction should be created.')
                  izConstDiffMsg=.true.
                ENDIF
                IF (.not. Construct(ConstrNum)%ReverseConstructionLayersOrderWarning .or.  &
                    .not. Construct(ConstrNumFound)%ReverseConstructionLayersOrderWarning) THEN
                  CALL ShowContinueError('...this problem for this pair will not be reported again.')
                  Construct(ConstrNum)%ReverseConstructionLayersOrderWarning=.true.
                  Construct(ConstrNumFound)%ReverseConstructionLayersOrderWarning=.true.
                ENDIF
              ENDIF
            END IF

            ! If significantly different areas -- this would not be good
            MultFound = Zone(Surface(Found)%Zone)%Multiplier * Zone(Surface(Found)%Zone)%ListMultiplier
            MultSurfNum = Zone(Surface(SurfNum)%Zone)%Multiplier * Zone(Surface(SurfNum)%Zone)%ListMultiplier
            IF (Surface(Found)%Area > 0.0d0) THEN
              IF (ABS((Surface(Found)%Area*MultFound - Surface(SurfNum)%Area*MultSurfNum)/  &
                       Surface(Found)%Area*MultFound) > .02d0) THEN  ! 2% difference in areas
                ErrCount4=ErrCount4+1
                IF (ErrCount4 == 1 .and. .not. DisplayExtraWarnings) THEN
                  CALL ShowWarningError(RoutineName//''//  &
                    'InterZone Surface Areas do not match as expected and might not satisfy conservation of energy:')
                  CALL ShowContinueError('...use Output:Diagnostics,DisplayExtraWarnings; '//  &
                       'to show more details on individual mismatches.')
                ENDIF
                IF (DisplayExtraWarnings) THEN
                  CALL ShowWarningError(RoutineName//''//  &
                    'InterZone Surface Areas do not match as expected and might not satisfy conservation of energy:')

                  IF (MultFound == 1 .and. MultSurfNum == 1) THEN
                    CALL ShowContinueError('  Area='//TRIM(TrimSigDigits(Surface(SurfNum)%Area,1))// &
                                           ' in Surface='//TRIM(Surface(SurfNum)%Name)// &
                                           ', Zone='//TRIM(Surface(SurfNum)%ZoneName))
                    CALL ShowContinueError('  Area='//TRIM(TrimSigDigits(Surface(Found)%Area,1))// &
                                           ' in Surface='//TRIM(Surface(Found)%Name)// &
                                           ', Zone='//TRIM(Surface(Found)%ZoneName))
                  ELSE ! Show multiplier info
                    WRITE(MultString,*) MultSurfNum
                    MultString=ADJUSTL(MultString)
                    CALL ShowContinueError('  Area='//TRIM(TrimSigDigits(Surface(SurfNum)%Area,1))// &
                                           ', Multipliers='//TRIM(MultString)// &
                                           ', Total Area='//TRIM(TrimSigDigits(Surface(SurfNum)%Area*MultSurfNum,1))// &
                                           ' in Surface='//TRIM(Surface(SurfNum)%Name)// &
                                           ' Zone='//TRIM(Surface(SurfNum)%ZoneName))
                    WRITE(MultString,*) MultFound
                    MultString=ADJUSTL(MultString)
                    CALL ShowContinueError('  Area='//TRIM(TrimSigDigits(Surface(Found)%Area,1))// &
                                           ', Multipliers='//TRIM(MultString)// &
                                           ', Total Area='//TRIM(TrimSigDigits(Surface(Found)%Area*MultFound,1))// &
                                           ' in Surface='//TRIM(Surface(Found)%Name)// &
                                           ' Zone='//TRIM(Surface(Found)%ZoneName))
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
            ! Check opposites Azimuth and Tilt
            ! Tilt
            IF (ABS(ABS(Surface(Found)%Tilt+Surface(SurfNum)%Tilt)-180.d0) > 1.0d0) THEN
              CALL ShowWarningError(RoutineName//'InterZone Surface Tilts do not match as expected.')
              CALL ShowContinueError('  Tilt='//TRIM(TrimSigDigits(Surface(SurfNum)%Tilt,1))//  &
                                     ' in Surface='//TRIM(Surface(SurfNum)%Name)//', Zone='//TRIM(Surface(SurfNum)%ZoneName))
              CALL ShowContinueError('  Tilt='//TRIM(TrimSigDigits(Surface(Found)%Tilt,1))//  &
                                     ' in Surface='//TRIM(Surface(Found)%Name)//', Zone='//TRIM(Surface(Found)%ZoneName))
            ENDIF
            ! check surface class match.  interzone surface.
            IF ((Surface(SurfNum)%Class == SurfaceClass_Wall .and. Surface(Found)%Class /= SurfaceClass_Wall) .or.  &
                (Surface(SurfNum)%Class /= SurfaceClass_Wall .and. Surface(Found)%Class == SurfaceClass_Wall) ) THEN
              CALL ShowWarningError(RoutineName//'InterZone Surface Classes do not match as expected.')
              CALL ShowContinueError('Surface="'//trim(Surface(SurfNum)%Name)//'", surface class='//  &
                 TRIM(cSurfaceClass(Surface(SurfNum)%Class)))
              CALL ShowContinueError('Adjacent Surface="'//trim(Surface(Found)%Name)//'", surface class='//  &
                 TRIM(cSurfaceClass(Surface(Found)%Class)))
              CALL ShowContinueError('Other errors/warnings may follow about these surfaces.')
            ENDIF
            IF ((Surface(SurfNum)%Class == SurfaceClass_Roof .and. Surface(Found)%Class /= SurfaceClass_Floor) .or.  &
                (Surface(SurfNum)%Class /= SurfaceClass_Roof .and. Surface(Found)%Class == SurfaceClass_Floor) ) THEN
              CALL ShowWarningError(RoutineName//'InterZone Surface Classes do not match as expected.')
              CALL ShowContinueError('Surface="'//trim(Surface(SurfNum)%Name)//'", surface class='//  &
                 TRIM(cSurfaceClass(Surface(SurfNum)%Class)))
              CALL ShowContinueError('Adjacent Surface="'//trim(Surface(Found)%Name)//'", surface class='//  &
                 TRIM(cSurfaceClass(Surface(Found)%Class)))
              CALL ShowContinueError('Other errors/warnings may follow about these surfaces.')
            ENDIF
            IF (Surface(SurfNum)%Class /= SurfaceClass_Roof .and. Surface(SurfNum)%Class /= SurfaceClass_Floor) THEN
              ! Walls, Windows, Doors, Glass Doors
              IF (Surface(SurfNum)%Class /= SurfaceClass_Wall) THEN
                ! Surface is a Door, Window or Glass Door
                IF (Surface(SurfNum)%BaseSurf == 0) CYCLE   ! error detected elsewhere
                IF (Surface(Surface(SurfNum)%BaseSurf)%Class == SurfaceClass_Roof .or.   &
                    Surface(Surface(SurfNum)%BaseSurf)%Class == SurfaceClass_Floor) CYCLE
              ENDIF
              IF (ABS(ABS(Surface(SurfNum)%Azimuth-Surface(Found)%Azimuth)-180.d0) > 1.d0) THEN
                IF (ABS(Surface(SurfNum)%SinTilt) > .5d0 .or. DisplayExtraWarnings) THEN
                  ! if horizontal surfaces, then these are windows/doors/etc in those items.
                  CALL ShowWarningError(RoutineName//'InterZone Surface Azimuths do not match as expected.')
                  CALL ShowContinueError('  Azimuth='//TRIM(TrimSigDigits(Surface(SurfNum)%Azimuth,1))//  &
                                         ', Tilt='//TRIM(TrimSigDigits(Surface(SurfNum)%Tilt,1))//        &
                                         ', in Surface='//TRIM(Surface(SurfNum)%Name)//', Zone='//TRIM(Surface(SurfNum)%ZoneName))
                  CALL ShowContinueError('  Azimuth='//TRIM(TrimSigDigits(Surface(Found)%Azimuth,1))//  &
                                         ', Tilt='//TRIM(TrimSigDigits(Surface(Found)%Tilt,1))//        &
                                         ', in Surface='//TRIM(Surface(Found)%Name)//', Zone='//TRIM(Surface(Found)%ZoneName))
                  CALL ShowContinueError('..surface class of first surface='//TRIM(cSurfaceClass(Surface(SurfNum)%Class)))
                  CALL ShowContinueError('..surface class of second surface='//TRIM(cSurfaceClass(Surface(Found)%Class)))
                ENDIF
              ENDIF
            ELSE  ! Roofs, Floors
              ! should be looking at opposite tilts, not azimuth for roof/floor matches...
!              IF (ABS(ABS(Surface(SurfNum)%Azimuth+Surface(Found)%Azimuth)-360.) > 1.0d0) THEN
!                CALL ShowWarningError('InterZone Surface Azimuths do not match as expected.')
!                CALL ShowContinueError('  Azimuth='//TRIM(TrimSigDigits(Surface(SurfNum)%Azimuth,1))//  &
!                                       ' in Surface='//TRIM(Surface(SurfNum)%Name)//', Zone='//TRIM(Surface(SurfNum)%ZoneName))
!                CALL ShowContinueError('  Azimuth='//TRIM(TrimSigDigits(Surface(Found)%Azimuth,1))//  &
!                                       ' in Surface='//TRIM(Surface(Found)%Name)//', Zone='//TRIM(Surface(Found)%ZoneName))
!              ENDIF
            ENDIF

            ! Make sure exposures (Sun, Wind) are the same.....and are "not"
            IF (Surface(SurfNum)%ExtSolar .or. Surface(Found)%ExtSolar) THEN
              CALL ShowWarningError(RoutineName//'Interzone surfaces cannot be "SunExposed" -- removing SunExposed')
              CALL ShowContinueError('  Surface='//TRIM(Surface(SurfNum)%Name)//', Zone='//TRIM(Surface(SurfNum)%ZoneName))
              CALL ShowContinueError('  Surface='//TRIM(Surface(Found)%Name)//', Zone='//TRIM(Surface(Found)%ZoneName))
              Surface(SurfNum)%ExtSolar=.false.
              Surface(Found)%ExtSolar=.false.
            ENDIF
            IF (Surface(SurfNum)%ExtWind .or. Surface(Found)%ExtWind) THEN
              CALL ShowWarningError(RoutineName//'Interzone surfaces cannot be "WindExposed" -- removing WindExposed')
              CALL ShowContinueError('  Surface='//TRIM(Surface(SurfNum)%Name)//', Zone='//TRIM(Surface(SurfNum)%ZoneName))
              CALL ShowContinueError('  Surface='//TRIM(Surface(Found)%Name)//', Zone='//TRIM(Surface(Found)%ZoneName))
              Surface(SurfNum)%ExtWind=.false.
              Surface(Found)%ExtWind=.false.
            ENDIF
          END IF
          ! Set opposing surface back to this one (regardless of error)
          Surface(Found)%ExtBoundCond=SurfNum
          ! Check subsurfaces...  make sure base surface is also an interzone surface
          IF (Surface(SurfNum)%BaseSurf /= SurfNum) THEN  ! Subsurface
            IF (Surface(SurfNum)%ExtBoundCond /= SurfNum .and. Surface(SurfNum)%ExtBoundCondName /= '   ') THEN
              ! if not internal subsurface
              IF (Surface(Surface(SurfNum)%BaseSurf)%ExtBoundCond == Surface(SurfNum)%BaseSurf) THEN
                     ! base surface is not interzone surface
                CALL ShowSevereError(RoutineName//'SubSurface="'//TRIM(Surface(SurfNum)%Name)//'" is an interzone subsurface.')
                CALL ShowContinueError('..but the Base Surface is not an interzone surface, Surface="'//  &
                    TRIM(Surface(Surface(SurfNum)%BaseSurf)%Name)//'".')
                SurfError=.true.
              ENDIF
            ENDIF
          ENDIF
        Else
          !  Seems unlikely that an internal surface would be missing itself, so this message
          !  only indicates for adjacent (interzone) surfaces.
          CALL ShowSevereError(RoutineName//'Adjacent Surface not found: '//TRIM(Surface(SurfNum)%ExtBoundCondName)// &
                               ' adjacent to surface '//TRIM(Surface(SurfNum)%Name) )
          NonMatch=.true.
          SurfError=.true.
        End If
      ELSEIF (Surface(SurfNum)%BaseSurf /= SurfNum) THEN  ! Subsurface
        IF (Surface(Surface(SurfNum)%BaseSurf)%ExtBoundCond > 0 .and.    &       ! If Interzone surface, subsurface must be also.
            Surface(Surface(SurfNum)%BaseSurf)%ExtBoundCond /= Surface(SurfNum)%BaseSurf) THEN
          CALL ShowSevereError(RoutineName//'SubSurface on Interzone Surface must be an Interzone SubSurface.')
          CALL ShowContinueError('...OutsideFaceEnvironment is blank, in Surface='//TRIM(Surface(SurfNum)%Name))
          SurfError=.true.
        ELSE
          ErrCount3=ErrCount3+1
          IF (ErrCount3 == 1 .and. .not. DisplayExtraWarnings) THEN
            CALL ShowWarningError(RoutineName//'Blank name for Outside Boundary Condition Objects.')
            CALL ShowContinueError('...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual surfaces.')
          ENDIF
          IF (DisplayExtraWarnings) THEN
            CALL ShowWarningError(RoutineName//'Blank name for Outside Boundary Condition Object, in surface='//  &
                                  TRIM(Surface(SurfNum)%Name))
            CALL ShowContinueError('Resetting this surface to be an internal zone surface, zone='//TRIM(Surface(SurfNum)%ZoneName))
          ENDIF
          Surface(SurfNum)%ExtBoundCondName=Surface(SurfNum)%Name
          Surface(SurfNum)%ExtBoundCond=SurfNum
        ENDIF
      ELSE
        ErrCount3=ErrCount3+1
        IF (ErrCount3 == 1 .and. .not. DisplayExtraWarnings) THEN
          CALL ShowSevereError(RoutineName//'Blank name for Outside Boundary Condition Objects.')
          CALL ShowContinueError('...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual surfaces.')
        ENDIF
        IF (DisplayExtraWarnings) THEN
          CALL ShowWarningError(RoutineName//'Blank name for Outside Boundary Condition Object, in surface='//  &
                                TRIM(Surface(SurfNum)%Name))
          CALL ShowContinueError('Resetting this surface to be an internal zone (adiabatic) surface, zone='//  &
             TRIM(Surface(SurfNum)%ZoneName))
        ENDIF
        Surface(SurfNum)%ExtBoundCondName=Surface(SurfNum)%Name
        Surface(SurfNum)%ExtBoundCond=SurfNum
        SurfError=.true.
      ENDIF
    End If

  END DO  ! ...end of the Surface DO loop for finding BaseSurf
  If (NonMatch) THEN
    CALL ShowSevereError(RoutineName//'Non matching interzone surfaces found')
  End If

!**********************************************************************************
! Warn about interzone surfaces that have adiabatic windows/vice versa
  SubSurfaceSevereDisplayed=.false.
  DO SurfNum=1,TotSurfaces
    IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
    IF (Surface(SurfNum)%BaseSurf == SurfNum) CYCLE  ! base surface
    ! not base surface.  Check it.
    IF (Surface(Surface(SurfNum)%BaseSurf)%ExtBoundCond <= 0) THEN  ! exterior or other base surface
      IF (Surface(SurfNum)%ExtBoundCond /= Surface(Surface(SurfNum)%BaseSurf)%ExtBoundCond) THEN ! should match base surface
        IF (Surface(SurfNum)%ExtBoundCond == SurfNum) THEN
          CALL ShowSevereError(RoutineName//'Subsurface="'//trim(Surface(SurfNum)%Name)//  &
             '" exterior condition [adiabatic surface] in a base surface="'//  &
             trim(Surface(Surface(SurfNum)%BaseSurf)%Name)//  &
             '" with exterior condition ['//  &
             trim(cExtBoundCondition(Surface(Surface(SurfNum)%BaseSurf)%ExtBoundCond))//']')
          SurfError=.true.
        ELSEIF (Surface(SurfNum)%ExtBoundCond > 0) THEN
          CALL ShowSevereError(RoutineName//'Subsurface="'//trim(Surface(SurfNum)%Name)//  &
             '" exterior condition [interzone surface] in a base surface="'//  &
             trim(Surface(Surface(SurfNum)%BaseSurf)%Name)//  &
             '" with exterior condition ['//  &
             trim(cExtBoundCondition(Surface(Surface(SurfNum)%BaseSurf)%ExtBoundCond))//']')
          SurfError=.true.
        ELSEIF (Surface(Surface(SurfNum)%BaseSurf)%ExtBoundCond == OtherSideCondModeledExt) THEN
          CALL ShowWarningError(RoutineName//'Subsurface="'//trim(Surface(SurfNum)%Name)//  &
           '" exterior condition ['//  &
           trim(cExtBoundCondition(Surface(SurfNum)%ExtBoundCond))//  &
           '] in a base surface="'//trim(Surface(Surface(SurfNum)%BaseSurf)%Name)//  &
           '" with exterior condition ['//  &
           trim(cExtBoundCondition(Surface(Surface(SurfNum)%BaseSurf)%ExtBoundCond))//']')
          CALL ShowContinueError('...SubSurface will not use the exterior condition model of the base surface.')
        ELSE
          CALL ShowSevereError(RoutineName//'Subsurface="'//trim(Surface(SurfNum)%Name)//  &
           '" exterior condition ['//  &
           trim(cExtBoundCondition(Surface(SurfNum)%ExtBoundCond))//  &
           '] in a base surface="'//trim(Surface(Surface(SurfNum)%BaseSurf)%Name)//  &
           '" with exterior condition ['//  &
           trim(cExtBoundCondition(Surface(Surface(SurfNum)%BaseSurf)%ExtBoundCond))//']')
          SurfError=.true.
        ENDIF
        IF (.not. SubSurfaceSevereDisplayed .and. SurfError) THEN
          CALL ShowContinueError('...calculations for heat balance would be compromised.')
          SubSurfaceSevereDisplayed=.true.
        ENDIF
      ENDIF
    ELSEIF (Surface(Surface(SurfNum)%BaseSurf)%BaseSurf == Surface(Surface(SurfNum)%BaseSurf)%ExtBoundCond) THEN
      ! adiabatic surface. make sure subsurfaces match
      IF (Surface(SurfNum)%ExtBoundCond /= SurfNum) THEN ! not adiabatic surface
        IF (Surface(SurfNum)%ExtBoundCond > 0) THEN
          CALL ShowSevereError(RoutineName//'Subsurface="'//trim(Surface(SurfNum)%Name)//  &
             '" exterior condition [interzone surface] in a base surface="'//  &
             trim(Surface(Surface(SurfNum)%BaseSurf)%Name)//  &
             '" with exterior condition [adiabatic surface]')
        ELSE
          CALL ShowSevereError(RoutineName//'Subsurface="'//trim(Surface(SurfNum)%Name)//  &
             '" exterior condition ['//  &
             trim(cExtBoundCondition(Surface(SurfNum)%ExtBoundCond))//  &
             '] in a base surface="'//trim(Surface(Surface(SurfNum)%BaseSurf)%Name)//  &
             '" with exterior condition [adiabatic surface]')
        ENDIF
        IF (.not. SubSurfaceSevereDisplayed) THEN
          CALL ShowContinueError('...calculations for heat balance would be compromised.')
          SubSurfaceSevereDisplayed=.true.
        ENDIF
        SurfError=.true.
      ENDIF
    ELSEIF (Surface(Surface(SurfNum)%BaseSurf)%ExtBoundCond > 0) THEN  ! interzone surface
      IF (Surface(SurfNum)%ExtBoundCond == SurfNum) THEN
        CALL ShowSevereError(RoutineName//'Subsurface="'//trim(Surface(SurfNum)%Name)//  &
           '" is an adiabatic surface in an Interzone base surface="'//trim(Surface(Surface(SurfNum)%BaseSurf)%Name)//'"')
        IF (.not. SubSurfaceSevereDisplayed) THEN
          CALL ShowContinueError('...calculations for heat balance would be compromised.')
          SubSurfaceSevereDisplayed=.true.
        ENDIF
!        SurfError=.true.
      ENDIF
    ENDIF
  ENDDO

!**********************************************************************************
!   Set up Zone Surface Pointers
  DO ZoneNum=1,NumOfZones
    DO SurfNum=1,MovedSurfs !TotSurfaces
      IF (Surface(SurfNum)%Zone == ZoneNum) THEN
        IF (Zone(ZoneNum)%SurfaceFirst == 0) THEN
          Zone(ZoneNum)%SurfaceFirst=SurfNum
          EXIT
        ENDIF
      ENDIF
    END DO
  END DO
  !  Surface First pointers are set, set last
  IF (NumOfZones > 0) THEN
    Zone(NumOfZones)%SurfaceLast=TotSurfaces
  ENDIF
  DO ZoneNum=1,NumOfZones-1
    Zone(ZoneNum)%SurfaceLast=Zone(ZoneNum+1)%SurfaceFirst-1
  END DO

  DO ZoneNum=1,NumOfZones
    IF (Zone(ZoneNum)%SurfaceFirst == 0) THEN
      CALL ShowSevereError(RoutineName//'Zone has no surfaces, Zone='//TRIM(Zone(ZoneNum)%Name))
      SurfError=.true.
    ENDIF
  ENDDO

! Set up Floor Areas for Zones
  IF (.not. SurfError) THEN
    DO ZoneNum=1,NumOfZones
      DO SurfNum=Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
        IF (Surface(SurfNum)%Class == SurfaceClass_Floor) THEN
          Zone(ZoneNum)%FloorArea=Zone(ZoneNum)%FloorArea+Surface(SurfNum)%Area
          Zone(ZoneNum)%HasFloor=.true.
        ENDIF
        IF (Surface(SurfNum)%Class == SurfaceClass_Roof) THEN
          Zone(ZoneNum)%HasRoof=.true.
        ENDIF
      END DO
    END DO
    errCount=0
    DO ZoneNum=1,NumOfZones
      Zone(ZoneNum)%CalcFloorArea = Zone(ZoneNum)%FloorArea
      IF (Zone(ZoneNum)%UserEnteredFloorArea /= AutoCalculate) THEN
      ! Check entered vs calculated
        IF (Zone(ZoneNum)%UserEnteredFloorArea > 0.0d0) THEN   ! User entered zone floor area,
                                                             ! produce message if not near calculated
          IF (Zone(ZoneNum)%CalcFloorArea > 0.0d0) THEN
            diffp=ABS(Zone(ZoneNum)%CalcFloorArea-Zone(ZoneNum)%UserEnteredFloorArea)/Zone(ZoneNum)%UserEnteredFloorArea
            IF (diffp  > .05d0) THEN
              ErrCount=ErrCount+1
              IF (ErrCount == 1 .and. .not. DisplayExtraWarnings) THEN
                CALL ShowWarningError(RoutineName//'Entered Zone Floor Areas differ from calculated Zone Floor Area(s).')
                CALL ShowContinueError('...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual zones.')
              ENDIF
              IF (DisplayExtraWarnings) THEN
                ! Warn user of using specified Zone Floor Area
                CALL ShowWarningError(RoutineName//'Entered Floor Area entered for Zone="'//TRIM(Zone(ZoneNum)%Name)// &
                            '" significantly different from calculated Floor Area')
                CALL ShowContinueError('Entered Zone Floor Area value='//  &
                            TRIM(RoundSigDigits(Zone(ZoneNum)%UserEnteredFloorArea,2))//  &
                            ', Calculated Zone Floor Area value='//TRIM(RoundSigDigits(Zone(ZoneNum)%CalcFloorArea,2))// &
                            ', entered Floor Area will be used in calculations.')
              ENDIF
            ENDIF
          ENDIF
          Zone(ZoneNum)%FloorArea = Zone(ZoneNum)%UserEnteredFloorArea
          Zone(ZoneNum)%HasFloor=.true.
        ENDIF
      ELSE
        Zone(ZoneNum)%FloorArea = Zone(ZoneNum)%CalcFloorArea  ! redundant, already done.
      ENDIF
    END DO
  ENDIF

  DO SurfNum=1,MovedSurfs !TotSurfaces
    IF (Surface(SurfNum)%Area < 1.d-06) THEN
      CALL ShowSevereError(RoutineName//'Zero or negative surface area['//trim(RoundSigDigits(Surface(SurfNum)%Area,5))//  &
         '], Surface='//Trim(Surface(SurfNum)%Name))
      SurfError=.true.
    ENDIF
    IF (Surface(SurfNum)%Area >= 1.d-06 .and. Surface(SurfNum)%Area < .001d0) THEN
      CALL ShowWarningError(RoutineName//'Very small surface area['//trim(RoundSigDigits(Surface(SurfNum)%Area,5))//  &
         '], Surface='//Trim(Surface(SurfNum)%Name))
    ENDIF
  END DO

  DO SurfNum=1,MovedSurfs !TotSurfaces
    ! GLASSDOORs and TDD:DIFFUSERs will be treated as windows in the subsequent heat transfer and daylighting
    ! calculations. Reset class to 'Window' after saving the original designation in SurfaceWindow.

    SurfaceWindow(SurfNum)%OriginalClass = Surface(SurfNum)%Class

    IF (Surface(SurfNum)%Class == SurfaceClass_GlassDoor &
      .OR. Surface(SurfNum)%Class == SurfaceClass_TDD_Diffuser) Surface(SurfNum)%Class = SurfaceClass_Window

    IF (Surface(SurfNum)%Class.EQ.SurfaceClass_TDD_Dome) THEN
      ! Reset the TDD:DOME subsurface to act as a base surface that can shade and be shaded
      ! NOTE: This must be set early so that subsequent shading calculations are done correctly
      Surface(SurfNum)%BaseSurf=SurfNum
    END IF
  END DO

  errFlag=.false.
  IF (.not. SurfError) THEN
    DO SurfNum=1,MovedSurfs !TotSurfaces
      ! Set ShadedConstruction numbers for windows whose shaded constructions were created
      ! when shading device was specified in the WindowShadingControl for the window
      IF(Surface(SurfNum)%ShadedConstruction /= 0) &
        SurfaceWindow(SurfNum)%ShadedConstruction = Surface(SurfNum)%ShadedConstruction

      ! no need to set the below -- it is the default
      ! Set variable that indicates if shading device has movable slats
!      SurfaceWindow(SurfNum)%MovableSlats = .FALSE.

      ! TH 2/9/2010. Fixed for CR 8010 for speed up purpose rather than fixing the problem
      WinShadingControlPtr = Surface(SurfNum)%WindowShadingControlPtr
      IF(WinShadingControlPtr /= 0) THEN
        IF(WindowShadingControl(WinShadingControlPtr)%SlatAngleControlForBlinds /= WSC_SAC_FixedSlatAngle) &
          SurfaceWindow(SurfNum)%MovableSlats = .TRUE.
        ! for a constant schedule of slat angle, it acts the same way as fixed angle
        ! TH 3/14/2011, CR 8347. Code was commented out due to the use of ExternalInterface (BCVTB)
        !IF(WindowShadingControl(WinShadingControlPtr)%SlatAngleControlForBlinds == WSC_SAC_ScheduledSlatAngle) THEN
          ! get schedule index
        !  SchID = WindowShadingControl(WinShadingControlPtr)%SlatAngleSchedule
        !  IF (SchID /= 0 ) THEN
        !    SchSlatAngle = GetScheduleMinValue(SchID)
        !    IF (SchSlatAngle == GetScheduleMaxValue(SchID)) THEN
        !      SurfaceWindow(SurfNum)%MovableSlats = .FALSE.
        !    ENDIF
        !  ENDIF
        !ENDIF
            ENDIF

      ConstrNumSh = SurfaceWindow(SurfNum)%ShadedConstruction
      IF(ConstrNumSh <= 0) CYCLE

      ShadingType = WindowShadingControl(WinShadingControlPtr)%ShadingType

      ! only for blinds
      IF(ShadingType == WSC_ST_ExteriorBlind .OR. ShadingType == WSC_ST_InteriorBlind &
        .OR. ShadingType == WSC_ST_BetweenGlassBlind ) THEN

        ! TH 1/7/2010. CR 7930
        ! The old code did not consider between-glass blind. Also there should not be two blinds - both interior and exterior
        ! Use the new generic code (assuming only one blind) as follows
        DO iTmp1 = 1, Construct(ConstrNumSh)%TotLayers
          iTmp2 = Construct(ConstrNumSh)%LayerPoint(iTmp1)
          IF(Material(iTmp2)%Group == WindowBlind) THEN
            BlNum = Material(iTmp2)%BlindDataPtr
            SurfaceWindow(SurfNum)%BlindNumber = BlNum
            ! TH 2/18/2010. CR 8010
            ! if it is a blind with movable slats, create one new blind and set it to VariableSlat if not done so yet.
            !  the new blind is created only once, it can be shared by multiple windows though.
            IF(SurfaceWindow(SurfNum)%MovableSlats .AND. Blind(BlNum)%SlatAngleType /= VariableSlats) THEN
              errFlag=.false.
              CALL AddVariableSlatBlind(BlNum,BlNumNew,errFlag)
              ! point to the new blind
              Material(iTmp2)%BlindDataPtr = BlNumNew
              ! window surface points to new blind
              SurfaceWindow(SurfNum)%BlindNumber = BlNumNew
            ENDIF
            EXIT
          END IF
        END DO

        IF (errFlag) THEN
          ErrorsFound=.true.
          CALL ShowContinueError('WindowProperty:ShadingControl '//  &
            TRIM(WindowShadingControl(WinShadingControlPtr)%Name)// &
            ' has errors, program will terminate.')
        ENDIF

        ! TH 5/17/2010. Fixed for CR 8121. Overwrite the blind slat angle with the constant scheduled value
        ! TH 3/14/2011. With fix for CR 8347, the following code is no longer needed.
        !IF (SurfaceWindow(SurfNum)%BlindNumber >0 .AND. WinShadingControlPtr >0 ) THEN
        !  IF (.NOT. SurfaceWindow(SurfNum)%MovableSlats .AND. &
        !    WindowShadingControl(WinShadingControlPtr)%SlatAngleControlForBlinds == WSC_SAC_ScheduledSlatAngle) THEN
        !    Blind(SurfaceWindow(SurfNum)%BlindNumber)%SlatAngle = SchSlatAngle
        !  ENDIF
        !ENDIF

      ENDIF

    END DO  ! End of surface loop

    ! Warning if a WindowShadingControl is not referenced by any window; user may think
    ! window shading is occurring when it really isn't
    DO ShadingCtrl = 1,TotWinShadingControl
      WinShadingCtrlReferenced = .FALSE.
      DO SurfNum = 1,TotSurfaces
        IF(Surface(SurfNum)%WindowShadingControlPtr == ShadingCtrl) WinShadingCtrlReferenced = .TRUE.
      END DO
      IF(.NOT.WinShadingCtrlReferenced) THEN
        CALL ShowWarningError(RoutineName//'WindowProperty:ShadingControl: "'//TRIM(WindowShadingControl(ShadingCtrl)%Name)// &
                    '" is not referenced by any window.')
      END IF
    END DO
  ENDIF

  ! Check for zones with not enough surfaces
  DO ZoneNum = 1,NumOfZones
    OpaqueHTSurfs = 0
    OpaqueHTSurfsWithWin = 0
    InternalMassSurfs = 0
    IF (Zone(ZoneNum)%SurfaceFirst == 0) CYCLE  ! Zone with no surfaces
    DO SurfNum = Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast
      IF(Surface(SurfNum)%Class == SurfaceClass_Floor .OR. Surface(SurfNum)%Class == SurfaceClass_Wall .OR. &
         Surface(SurfNum)%Class == SurfaceClass_Roof) OpaqueHTSurfs = OpaqueHTSurfs + 1
      IF(Surface(SurfNum)%Class == SurfaceClass_IntMass) InternalMassSurfs = InternalMassSurfs + 1
      IF(Surface(SurfNum)%Class == SurfaceClass_Window) THEN
         ! Count base surface only once for multiple windows on a wall
         IF(SurfNum > 1 .AND. Surface(SurfNum-1)%Class /= SurfaceClass_Window) &
           OpaqueHTSurfsWithWin = OpaqueHTSurfsWithWin + 1
      END IF
    END DO
    IF(OpaqueHTSurfsWithWin == 1 .AND. OpaqueHTSurfs == 1 .AND. InternalMassSurfs == 0) THEN
      SurfError = .true.
      CALL ShowSevereError(RoutineName//'Zone '//Trim(Zone(ZoneNum)%Name)// &
        ' has only one floor, wall or roof, and this surface has a window.')
      CALL ShowContinueError('Add more floors, walls or roofs, or an internal mass surface.')
    END IF
    IF((OpaqueHTSurfs + InternalMassSurfs) < 6) THEN
      CALL ShowWarningError(RoutineName//'The total number of floors, walls, roofs and internal mass surfaces in Zone '// &
                            Trim(Zone(ZoneNum)%Name))
      CALL ShowContinueError('is < 6. This may cause an inaccurate zone heat balance calculation.')
    END IF
  END DO

    ! set up vertex of centroid for each surface.
  CALL CalcSurfaceCentroid

  CALL SetupShadeSurfacesForSolarCalcs  ! if shading surfaces are solar collectors or PV, then we need full solar calc.

  LayNumOutside=0
  DO SurfNum=1,TotSurfaces
    ! Check for EcoRoof and only 1 allowed to be used.
    IF (.not. Surface(SurfNum)%ExtEcoRoof) CYCLE
    IF (LayNumOutSide == 0) THEN
      LayNumOutSide=Construct(Surface(SurfNum)%Construction)%LayerPoint(1)
      CYCLE
    ENDIF
    IF (LayNumOutSide /= Construct(Surface(SurfNum)%Construction)%LayerPoint(1)) THEN
      CALL ShowSevereError(RoutineName//'Only one EcoRoof Material is currently allowed for all constructions.')
      CALL ShowContinueError('... first material='//TRIM(Material(LayNumOutSide)%Name))
      CALL ShowContinueError('... conflicting Construction='//TRIM(Construct(Surface(SurfNum)%Construction)%Name)//  &
                    ' uses material='//TRIM(Material(Construct(Surface(SurfNum)%Construction)%LayerPoint(1))%Name))
      ErrorsFound=.true.
    ENDIF
  ENDDO

  ! Set flag that determines whether a surface can be an exterior obstruction
  DO SurfNum = 1,TotSurfaces
    Surface(SurfNum)%ShadowSurfPossibleObstruction = .FALSE.
    ! Exclude non-exterior heat transfer surfaces (but not OtherSideCondModeledExt = -4 CR7640)
    IF(Surface(SurfNum)%HeatTransSurf .AND. Surface(SurfNum)%ExtBoundCond > 0 ) CYCLE
    IF(Surface(SurfNum)%HeatTransSurf .AND. Surface(SurfNum)%ExtBoundCond == Ground) CYCLE
    IF(Surface(SurfNum)%HeatTransSurf .AND. Surface(SurfNum)%ExtBoundCond == OtherSideCoefNoCalcExt) CYCLE
    IF(Surface(SurfNum)%HeatTransSurf .AND. Surface(SurfNum)%ExtBoundCond == OtherSideCoefCalcExt) CYCLE
    ! Exclude windows and doors, i.e., consider only their base surfaces as possible obstructions
    IF(Surface(SurfNum)%Class == SurfaceClass_Window .OR. Surface(SurfNum)%Class == SurfaceClass_Door) CYCLE
    ! Exclude duplicate shading surfaces
    ! TH 3/25/2010 CR 7872
    !  Shading surface names can start with Mir, a better way to use another flag
    !   to indicate whether a surface is a mirrored one.
    !IF(Surface(SurfNum)%Name(1:3) == 'Mir') CYCLE
    IF(Surface(SurfNum)%MirroredSurf) CYCLE

    Surface(SurfNum)%ShadowSurfPossibleObstruction = .TRUE.
  END DO

  ! Check for IRT surfaces in invalid places.
  iTmp1=0
  IF (ANY(Construct%TypeIsIRT)) THEN
    DO SurfNum = 1,TotSurfaces
      IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE  ! ignore shading surfaces
      IF (Surface(SurfNum)%ExtBoundCond > 0 .and. Surface(SurfNum)%ExtBoundCond /= SurfNum) CYCLE ! interzone, not adiabatic surface
      IF (.not. Construct(Surface(SurfNum)%Construction)%TypeIsIRT) CYCLE
      IF (.not. DisplayExtraWarnings) THEN
        iTmp1=iTmp1+1
      ELSE
        CALL ShowWarningError(RoutineName//'Surface="'//trim(Surface(SurfNum)%Name)//'" uses InfraredTransparent'//  &
           ' construction in a non-interzone surface. (illegal use)')
      ENDIF
    ENDDO
    IF (iTmp1 > 0) THEN
      CALL ShowWarningError(RoutineName//'Surfaces use InfraredTransparent constructions '//trim(TrimSigDigits(iTmp1))//  &
         ' in non-interzone surfaces. (illegal use)')
      CALL ShowContinueError('For explicit details on each use, use Output:Diagnostics,DisplayExtraWarnings;')
    ENDIF
  ENDIF

! Note, could do same for Window Area and detecting if Interzone Surface in Zone

  IF (Warning1Count > 0) THEN
    CALL ShowWarningMessage(RoutineName//'Window dimensions differ from Window 5/6 data file dimensions, '//  &
          trim(TrimSigDigits(Warning1Count))//' times.')
    CALL ShowContinueError('This will affect the frame heat transfer calculation if the frame in the Data File entry')
    CALL ShowContinueError('is not uniform, i.e., has sections with different geometry and/or thermal properties.')
    CALL ShowContinueError('For explicit details on each window, use Output:Diagnostics,DisplayExtraWarnings;')
  ENDIF
  IF (Warning2Count > 0) THEN
    CALL ShowWarningMessage(RoutineName//'Exterior Windows have been replaced with Window 5/6 two glazing systems, '//  &
          trim(TrimSigDigits(Warning2Count))//' times.')
    CALL ShowContinueError('Note that originally entered dimensions are overridden.')
    CALL ShowContinueError('For explicit details on each window, use Output:Diagnostics,DisplayExtraWarnings;')
  ENDIF
  IF (Warning3Count > 0) THEN
    CALL ShowWarningMessage(RoutineName//'Interior Windows have been replaced with Window 5/6 two glazing systems, '//  &
          trim(TrimSigDigits(Warning3Count))//' times.')
    CALL ShowContinueError('Note that originally entered dimensions are overridden.')
    CALL ShowContinueError('For explicit details on each window, use Output:Diagnostics,DisplayExtraWarnings;')
  ENDIF

  IF (TotalMultipliedWindows > 0) THEN
    CALL ShowWarningMessage(RoutineName//'There are '//trim(TrimSigDigits(TotalMultipliedWindows))//' window/glass door(s) '//  &
            'that may cause inaccurate shadowing due to Solar Distribution.')
    CALL ShowContinueError('For explicit details on each window, use Output:Diagnostics,DisplayExtraWarnings;')
    TotalWarningErrors=TotalWarningErrors+TotalMultipliedWindows
  ENDIF
  IF (TotalCoincidentVertices > 0) THEN
    CALL ShowWarningMessage(RoutineName//'There are '//trim(TrimSigDigits(TotalCoincidentVertices))//  &
       ' coincident/collinear vertices; These have been deleted unless the deletion would bring the number of surface sides < 3.')
    CALL ShowContinueError('For explicit details on each problem surface, use Output:Diagnostics,DisplayExtraWarnings;')
    TotalWarningErrors=TotalWarningErrors+TotalCoincidentVertices
  ENDIF
  IF (TotalDegenerateSurfaces > 0) THEN
    CALL ShowSevereMessage(RoutineName//'There are '//trim(TrimSigDigits(TotalDegenerateSurfaces))//  &
       ' degenerate surfaces; Degenerate surfaces are those with number of sides < 3.')
    CALL ShowContinueError('These surfaces should be deleted.')
    CALL ShowContinueError('For explicit details on each problem surface, use Output:Diagnostics,DisplayExtraWarnings;')
    TotalSevereErrors=TotalSevereErrors+TotalDegenerateSurfaces
  ENDIF

 CALL GetHTSurfExtVentedCavityData(ErrorsFound)

 CALL GetSurfaceHeatTransferAlgorithmOverrides(ErrorsFound)

 IF (SurfError .or. ErrorsFound) THEN
   ErrorsFound=.true.
   CALL ShowFatalError(RoutineName//'Errors discovered, program terminates.')
 ENDIF

 RETURN

END SUBROUTINE GetSurfaceData

SUBROUTINE GetGeometryParameters(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine reads in the "Surface Geometry" parameters, verifies them,
          ! and sets "global" variables that will tell other routines how the surface
          ! vertices are expected in input.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! GlobalGeometryRules Definition
          !GlobalGeometryRules,
          !      \required-object
          !      \unique-object
          !  A1, \field Starting Vertex Position
          !      \required-field
          !      \note Specified as entry for a 4 sided surface/rectangle
          !      \note Surfaces are specified as viewed from outside the surface
          !      \note Shading surfaces as viewed from behind.  (towards what they are shading)
          !      \type choice
          !      \key UpperLeftCorner
          !      \key LowerLeftCorner
          !      \key UpperRightCorner
          !      \key LowerRightCorner
          !  A2, \field Vertex Entry Direction
          !      \required-field
          !      \type choice
          !      \key Counterclockwise
          !      \key Clockwise
          !  A3, \field Coordinate System
          !      \required-field
          !      \note relative -- coordinates are entered relative to zone origin
          !      \note world -- all coordinates entered are "absolute" for this facility
          !      \note absolute -- same as world
          !      \type choice
          !      \key Relative
          !      \key World
          !      \key Absolute
          !  A4, \field Daylighting Reference Point Coordinate System
          !      \type choice
          !      \key Relative
          !      \default Relative
          !      \note Relative -- coordinates are entered relative to zone origin
          !      \key World
          !      \note World -- all coordinates entered are "absolute" for this facility
          !      \key Absolute
          !      \note absolute -- same as world
          !  A5; \field Rectangular Surface Coordinate System
          !      \type choice
          !      \key Relative
          !      \default Relative
          !      \note Relative -- Starting corner is entered relative to zone origin
          !      \key World
          !      \note World -- Starting corner is entered in "absolute"
          !      \key Absolute
          !      \note absolute -- same as world

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItem, SameString, VerifyName

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT)  :: ErrorsFound  ! set to true if errors found during input

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=3), PARAMETER, DIMENSION(4)  :: AbCorners=(/'ULC','LLC','LRC','URC'/)
  CHARACTER(len=16), PARAMETER, DIMENSION(4) :: FlCorners=(/'UpperLeftCorner ','LowerLeftCorner ',  &
                                                            'LowerRightCorner','UpperRightCorner'/)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER NumStmt
  CHARACTER(len=MaxNameLength), DIMENSION(5) :: GAlphas
  INTEGER NAlphas
  REAL(r64), DIMENSION(1) :: GNum
  INTEGER NNum
  INTEGER IOSTAT
  LOGICAL OK
  INTEGER Found
  CHARACTER(len=150) :: OutMsg

  cCurrentModuleObject='GlobalGeometryRules'
  NumStmt=GetNumObjectsFound(cCurrentModuleObject)
  OutMsg=' Surface Geometry,'

  SELECT CASE(NumStmt)

  CASE (1)
    ! This is the valid case
    CALL GetObjectItem(cCurrentModuleObject,1,GAlphas,NAlphas,GNum,NNum,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    ! Even though these will be validated, set defaults in case error here -- wont
    ! cause aborts in later surface gets (hopefully)
    Corner=UpperLeftCorner
    WorldCoordSystem=.true.
    CCW=.true.

    OK=.false.
    Found=FindItem(GAlphas(1),AbCorners,4)
    IF (Found == 0) THEN
      Found=FindItem(GAlphas(1),FlCorners,4)
      IF (Found == 0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Invalid '//TRIM(cAlphaFieldNames(1))//'='//TRIM(GAlphas(1)))
        ErrorsFound=.true.
      ELSE
        Corner=Found
        OK=.true.
        OutMsg=TRIM(OutMsg)//TRIM(FLCorners(Corner))//','
      ENDIF
    ELSE
      Corner=Found
      OutMsg=TRIM(OutMsg)//TRIM(FLCorners(Corner))//','
      OK=.true.
    ENDIF

    OK=.false.
    IF (SameString(GAlphas(2),'CCW') .or. SameString(GAlphas(2),'Counterclockwise')) THEN
      CCW=.true.
      OutMsg=TRIM(OutMsg)//'Counterclockwise'//','
      OK=.true.
    ENDIF
    IF (SameString(GAlphas(2),'CW')  .or. SameString(GAlphas(2),'Clockwise')) THEN
      CCW=.false.
      OutMsg=TRIM(OutMsg)//'Clockwise'//','
      OK=.true.
    ENDIF
    IF (.not. OK) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Invalid '//TRIM(cAlphaFieldNames(2))//'='//TRIM(GAlphas(2)))
      ErrorsFound=.true.
    ENDIF

    OK=.false.
    IF (SameString(GAlphas(3),'WCS') .or. SameString(GAlphas(3),'WorldCoordinateSystem') .or.   &
        SameString(GAlphas(3),'World') .or. SameString(GAlphas(3),'Absolute')) THEN
      WorldCoordSystem=.true.
      OutMsg=TRIM(OutMsg)//'WorldCoordinateSystem'//','
      OK=.true.
    ENDIF
    IF (SameString(GAlphas(3)(1:3),'Rel') .or. SameString(GAlphas(3)(1:8),'Relative') .or. SameString(GAlphas(3),'Local')) THEN
      WorldCoordSystem=.false.
      OutMsg=TRIM(OutMsg)//'RelativeCoordinateSystem'//','
      OK=.true.
    ENDIF
    IF (.not. OK) THEN
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//': Invalid '//TRIM(cAlphaFieldNames(3))//'='//TRIM(GAlphas(3)))
      CALL ShowContinueError(TRIM(cAlphaFieldNames(3))//' defaults to "WorldCoordinateSystem"')
      WorldCoordSystem=.true.
      OutMsg=TRIM(OutMsg)//'WorldCoordinateSystem'//','
    ENDIF

    OK=.false.
    IF (SameString(GAlphas(4),'WCS') .or. SameString(GAlphas(4),'WorldCoordinateSystem') .or.   &
        SameString(GAlphas(4),'World') .or. SameString(GAlphas(4),'Absolute')) THEN
      DaylRefWorldCoordSystem=.true.
      OutMsg=TRIM(OutMsg)//'WorldCoordinateSystem'//','
      OK=.true.
    ENDIF
    IF (SameString(GAlphas(4)(1:3),'Rel') .or. SameString(GAlphas(4)(1:8),'Relative') .or.   &
        SameString(GAlphas(4),'Local') .or. GAlphas(4) == Blank) THEN
      DaylRefWorldCoordSystem=.false.
      OutMsg=TRIM(OutMsg)//'RelativeCoordinateSystem'//','
      OK=.true.
    ENDIF
    IF (.not. OK) THEN
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//': Invalid '//TRIM(cAlphaFieldNames(4))//'='//TRIM(GAlphas(4)))
      CALL ShowContinueError(TRIM(cAlphaFieldNames(4))//' defaults to "RelativeToZoneOrigin"')
      DaylRefWorldCoordSystem=.false.
      OutMsg=TRIM(OutMsg)//'RelativeToZoneOrigin'//','
    ENDIF

    OK=.false.
    IF (SameString(GAlphas(5),'WCS') .or. SameString(GAlphas(5),'WorldCoordinateSystem') .or.   &
        SameString(GAlphas(5),'World') .or. SameString(GAlphas(5),'Absolute')) THEN
      RectSurfRefWorldCoordSystem=.true.
      OutMsg=TRIM(OutMsg)//'WorldCoordinateSystem'
      OK=.true.
    ENDIF
    IF (SameString(GAlphas(5)(1:3),'Rel') .or. SameString(GAlphas(5)(1:8),'Relative') .or.   &
        SameString(GAlphas(5),'Local') .or. GAlphas(5) == Blank) THEN
      RectSurfRefWorldCoordSystem=.false.
      OutMsg=TRIM(OutMsg)//'RelativeToZoneOrigin'
      OK=.true.
    ENDIF
    IF (.not. OK) THEN
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//': Invalid '//TRIM(cAlphaFieldNames(5))//'='//TRIM(GAlphas(5)))
      CALL ShowContinueError(TRIM(cAlphaFieldNames(5))//' defaults to "RelativeToZoneOrigin"')
      RectSurfRefWorldCoordSystem=.false.
      OutMsg=TRIM(OutMsg)//'RelativeToZoneOrigin'
    ENDIF

  CASE (0)

    CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Required object not found.')
    OutMsg=TRIM(OutMsg)//'None found in input'
    ErrorsFound=.true.

  CASE DEFAULT

    CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Too many objects entered.  Only one allowed.')
    ErrorsFound=.true.

  END SELECT

  IF (.not. WorldCoordSystem) THEN
    IF (DaylRefWorldCoordSystem) THEN
      CALL ShowWarningError(trim(cCurrentModuleObject)//': Potential mismatch of coordinate specifications.')
      CALL ShowContinueError(trim(cAlphaFieldNames(3))//'="'//trim(GAlphas(3))//'"; while ')
      CALL ShowContinueError(trim(cAlphaFieldNames(4))//'="'//trim(GAlphas(4))//'".')
    ENDIF
    IF (RectSurfRefWorldCoordSystem) THEN
      CALL ShowWarningError(trim(cCurrentModuleObject)//': Potential mismatch of coordinate specifications.')
      CALL ShowContinueError(trim(cAlphaFieldNames(3))//'="'//trim(GAlphas(3))//'"; while ')
      CALL ShowContinueError(trim(cAlphaFieldNames(5))//'="'//trim(GAlphas(5))//'".')
    ENDIF
  ENDIF

  WRITE(OutputFileInits,720) '! <SurfaceGeometry>,Starting Corner,'//  &
              'Vertex Input Direction,Coordinate System,'//  &
              'Daylight Reference Point Coordinate System,'//'Rectangular (Simple) Surface Coordinate System'
  WRITE(OutputFileInits,720) TRIM(OutMsg)

720 Format(A)

  RETURN

END SUBROUTINE GetGeometryParameters

SUBROUTINE GetDetShdSurfaceData(ErrorsFound,SurfNum,TotDetachedFixed,TotDetachedBldg)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the Detached Shading Surface Data,
          ! checks it for errors, etc.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
  ! Detached Shading Surface Definition(s)
  !Surface:Shading:Detached:Fixed,
  !       \memo used for shading elements such as trees
  !       \memo these items are fixed in space and would not move with relative geometry
  !  A1 , \field User Supplied Surface Name
  !       \required-field
  !       \type alpha
  !  A2,  \field TransSchedShadowSurf
  !       \note Transmittance schedule for the shading device, defaults to zero (always opaque)
  !       \type object-list
  !       \object-list ScheduleNames
  !  N1 , \field Number of Surface Vertex Groups -- Number of (X,Y,Z) groups in this surface
  !       \required-field
  !       \note shown with 12 vertex coordinates -- extensible object
  !       \autocalculatable
  !       \default autocalculate
  !       \minimum 3
  !       \note Rules for vertices are given in SurfaceGeometry coordinates --
  !       \note For this object all surface coordinates are relative to the building origin (0,0,0)
  !       \note and will rotate with the BUILDING north axis.
  !  N2,  \field Vertex 1 X-coordinate
  !       \units m
  !       \type real
  !  N3-37; as indicated by the N1 value
  !
  !Surface:Shading:Detached:Building,
  !       \memo used for shading elements such as trees, other buildings, parts of this building not being modeled
  !       \memo these items are relative to the current building and would move with relative geometry
  !  A1 , \field User Supplied Surface Name
  !       \required-field
  !       \type alpha
  !  A2,  \field TransSchedShadowSurf
  !       \note Transmittance schedule for the shading device, defaults to zero (always opaque)
  !       \type object-list
  !       \object-list ScheduleNames
  !  N1 , \field Number of Surface Vertex Groups -- Number of (X,Y,Z) groups in this surface
  !       \required-field
  !       \note shown with 12 vertex coordinates -- extensible object
  !       \autocalculatable
  !       \default autocalculate
  !       \minimum 3
  !       \note Rules for vertices are given in SurfaceGeometry coordinates --
  !       \note For this object all surface coordinates are relative to the building origin (0,0,0)
  !       \note and will rotate with the BUILDING north axis.
  !  N2,  \field Vertex 1 X-coordinate
  !       \units m
  !       \type real
  !  N3-37; as indicated by the N1 value

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE DataReportingFlags
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, GetObjectDefMaxArgs
  USE ScheduleManager, ONLY: GetScheduleIndex,CheckScheduleValueMinMax,GetScheduleMinValue,GetScheduleMaxValue
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT)    :: ErrorsFound       ! Error flag indicator (true if errors found)
  INTEGER, INTENT(INOUT)    :: SurfNum           ! Count of Current SurfaceNumber
  INTEGER, INTENT(IN)       :: TotDetachedFixed  ! Number of Fixed Detached Shading Surfaces to obtain
  INTEGER, INTENT(IN)       :: TotDetachedBldg   ! Number of Building Detached Shading Surfaces to obtain

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER, DIMENSION(2) :: cModuleObjects=  &
    (/'Shading:Site:Detailed    ',  &
      'Shading:Building:Detailed'/)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: IOStat            ! IO Status when calling get input subroutine
  INTEGER :: NumAlphas   ! Number of material alpha names being passed
  INTEGER :: NumNumbers    ! Number of material properties being passed
  INTEGER :: Loop
  LOGICAL :: ErrorInName
  LOGICAL :: IsBlank
  INTEGER :: Item
  INTEGER :: ItemsToGet
  INTEGER :: ClassItem
  INTEGER :: numSides
  REAL(r64) :: SchedMinValue
  REAL(r64) :: SchedMaxValue

  IF ((TotDetachedFixed+TotDetachedBldg) > 0 .and. SolarDistribution == MinimalShadowing) THEN
    CALL ShowWarningError('Detached shading effects are ignored when Solar Distribution = MinimalShadowing')
  ENDIF

  IF ((TotDetachedFixed+TotDetachedBldg) == 0) RETURN

  DO Item=1,2

    cCurrentModuleObject=cModuleObjects(Item)
    IF (Item == 1) THEN
      ItemsToGet=TotDetachedFixed
      ClassItem=SurfaceClass_Detached_F
    ELSE  !IF (Item == 2) THEN
      ItemsToGet=TotDetachedBldg
      ClassItem=SurfaceClass_Detached_B
    ENDIF

    CALL GetObjectDefMaxArgs(cCurrentModuleObject,Loop,NumAlphas,NumNumbers)
    IF (NumAlphas /= 2) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Object Definition indicates'// &
                           'not = 2 Alpha Objects, Number Indicated='//  &
                           TRIM(TrimSigDigits(NumAlphas)))
      ErrorsFound=.true.
    ENDIF

    DO Loop=1,ItemsToGet
      CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlphas, &
                         rNumericArgs,NumNumbers,IOSTAT,  &
                         AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      ErrorInName=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),SurfaceTmp%Name,SurfNum,ErrorInName,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (ErrorInName) THEN
        CALL ShowContinueError('...each surface name must not duplicate other surface names (of any type)')
        ErrorsFound=.true.
        CYCLE
      ENDIF

      SurfNum=SurfNum+1
      SurfaceTmp(SurfNum)%Name = cAlphaArgs(1)  ! Set the Surface Name in the Derived Type
      SurfaceTmp(SurfNum)%Class=ClassItem
      SurfaceTmp(SurfNum)%HeatTransSurf=.false.
    ! Base transmittance of a shadowing (sub)surface
      IF (.not. lAlphaFieldBlanks(2)) THEN
         ! Schedule for a shadowing (sub)surface
        SurfaceTmp(SurfNum)%SchedShadowSurfIndex = GetScheduleIndex(cAlphaArgs(2))
        IF (SurfaceTmp(SurfNum)%SchedShadowSurfIndex == 0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                               '", '//TRIM(cAlphaFieldNames(2))//' not found='//TRIM(cAlphaArgs(2)))
          ErrorsFound=.true.
        ENDIF
      ELSE
        SurfaceTmp(SurfNum)%SchedShadowSurfIndex=0
      ENDIF
      IF (SurfaceTmp(SurfNum)%SchedShadowSurfIndex /= 0) THEN
        IF (.not. CheckScheduleValueMinMax(SurfaceTmp(SurfNum)%SchedShadowSurfIndex,'>=',0.0d0,'<=',1.0d0)) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                               '", '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))// &
                               '", values not in range [0,1].')
          ErrorsFound=.true.
        ENDIF
        SchedMinValue=GetScheduleMinValue(SurfaceTmp(SurfNum)%SchedShadowSurfIndex)
        SurfaceTmp(SurfNum)%SchedMinValue=SchedMinValue
        SchedMaxValue=GetScheduleMaxValue(SurfaceTmp(SurfNum)%SchedShadowSurfIndex)
        IF (SchedMinValue == 1.0d0) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                               '", '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))// &
                               '", is always transparent.')
          SurfaceTmp(SurfNum)%IsTransparent=.true.
        ENDIF
        IF (SchedMinValue < 0.0d0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                               '", '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))// &
                               '", has schedule values < 0.')
          CALL ShowContinueError('...Schedule values < 0 have no meaning for shading elements.')
        ENDIF
        IF (SchedMaxValue > 1.0d0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                               '", '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))// &
                               '", has schedule values > 1.')
          CALL ShowContinueError('...Schedule values > 1 have no meaning for shading elements.')
        ENDIF
        IF (ABS(SchedMinValue-SchedMaxValue) > 1.0d-6) THEN
          SurfaceTmp(SurfNum)%ShadowSurfSchedVaries=.true.
          ShadingTransmittanceVaries=.true.
        ENDIF
      ENDIF
      IF (lNumericFieldBlanks(1) .or. rNumericArgs(1) == AutoCalculate) THEN
        numSides=(NumNumbers-1)/3
        SurfaceTmp(SurfNum)%Sides=numSides
        IF (MOD(NumNumbers-1,3) /= 0) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                          '", '//TRIM(cNumericFieldNames(1))//          &
                          ' not even multiple of 3. Will read in '//   &
                          TRIM(TrimSigDigits(SurfaceTmp(SurfNum)%Sides)))
        ENDIF
        IF (numSides < 3) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                          '", '//TRIM(cNumericFieldNames(1))//' (autocalculate) must be >= 3. Only '//  &
                          TRIM(TrimSigDigits(SurfaceTmp(SurfNum)%Sides))//' provided.')
          ErrorsFound=.true.
          CYCLE
        ENDIF
      ELSE
        numSides=(NumNumbers-1)/3
        SurfaceTmp(SurfNum)%Sides=rNumericArgs(1)
        IF (numSides > SurfaceTmp(SurfNum)%Sides) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                          '", field '//TRIM(cNumericFieldNames(1))//'='//TRIM(TrimSigDigits(SurfaceTmp(SurfNum)%Sides)))
          CALL ShowContinueError('...but '//TRIM(TrimSigDigits(numSides))//' were entered. Only the indicated '//  &
                          TRIM(cNumericFieldNames(1))//' will be used.')
        ENDIF
      ENDIF
      ALLOCATE(SurfaceTmp(SurfNum)%Vertex(SurfaceTmp(SurfNum)%Sides))
      CALL GetVertices(SurfNum,SurfaceTmp(SurfNum)%Sides,rNumericArgs(2:))
      CALL CheckConvexity(SurfNum,SurfaceTmp(SurfNum)%Sides)
      IF (MakeMirroredDetachedShading) THEN
        CALL MakeMirrorSurface(SurfNum)
      ENDIF
    ENDDO

  ENDDO ! Item Loop

  RETURN

END SUBROUTINE GetDetShdSurfaceData

SUBROUTINE GetRectDetShdSurfaceData(ErrorsFound,SurfNum,TotRectDetachedFixed,TotRectDetachedBldg)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Gets the simple, rectantular detached surfaces.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE DataReportingFlags
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, VerifyName, GetObjectDefMaxArgs
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT)    :: ErrorsFound       ! Error flag indicator (true if errors found)
  INTEGER, INTENT(INOUT)    :: SurfNum           ! Count of Current SurfaceNumber
  INTEGER, INTENT(IN)       :: TotRectDetachedFixed  ! Number of Fixed Detached Shading Surfaces to obtain
  INTEGER, INTENT(IN)       :: TotRectDetachedBldg   ! Number of Building Detached Shading Surfaces to obtain

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER, DIMENSION(2) :: cModuleObjects=  &
    (/'Shading:Site    ',  &
      'Shading:Building'/)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: IOStat            ! IO Status when calling get input subroutine
  INTEGER :: NumAlphas   ! Number of material alpha names being passed
  INTEGER :: NumNumbers    ! Number of material properties being passed
  INTEGER :: Loop
  LOGICAL :: ErrorInName
  LOGICAL :: IsBlank
  INTEGER :: Item
  INTEGER :: ItemsToGet
  INTEGER :: ClassItem

  IF ((TotRectDetachedFixed+TotRectDetachedBldg) > 0 .and. SolarDistribution == MinimalShadowing) THEN
    CALL ShowWarningError('Detached shading effects are ignored when Solar Distribution = MinimalShadowing')
  ENDIF

  IF (TotRectDetachedFixed+TotRectDetachedBldg == 0) RETURN

  DO Item=1,2

    cCurrentModuleObject=cModuleObjects(Item)
    IF (Item == 1) THEN
      ItemsToGet=TotRectDetachedFixed
      ClassItem=SurfaceClass_Detached_F
    ELSE  !IF (Item == 2) THEN
      ItemsToGet=TotRectDetachedBldg
      ClassItem=SurfaceClass_Detached_B
    ENDIF

    CALL GetObjectDefMaxArgs(cCurrentModuleObject,Loop,NumAlphas,NumNumbers)
    IF (NumAlphas /= 1) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Object Definition indicates'// &
                           'not = 1 Alpha Objects, Number Indicated='//  &
                           TRIM(TrimSigDigits(NumAlphas)))
      ErrorsFound=.true.
    ENDIF

    DO Loop=1,ItemsToGet
      CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlphas, &
                         rNumericArgs,NumNumbers,IOSTAT,  &
                         AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                         AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      ErrorInName=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),SurfaceTmp%Name,SurfNum,ErrorInName,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (ErrorInName) THEN
        CALL ShowContinueError('...each surface name must not duplicate other surface names (of any type)')
        ErrorsFound=.true.
        CYCLE
      ENDIF

      SurfNum=SurfNum+1
      SurfaceTmp(SurfNum)%Name = cAlphaArgs(1)  ! Set the Surface Name in the Derived Type
      SurfaceTmp(SurfNum)%Class=ClassItem
      SurfaceTmp(SurfNum)%HeatTransSurf=.false.

      SurfaceTmp(SurfNum)%Azimuth=rNumericArgs(1)
      IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Detached_B .and. .not. WorldCoordSystem) THEN
        SurfaceTmp(SurfNum)%Azimuth=SurfaceTmp(SurfNum)%Azimuth+BuildingAzimuth
      ENDIF
      IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Detached_B) THEN
        SurfaceTmp(SurfNum)%Azimuth=SurfaceTmp(SurfNum)%Azimuth+BuildingRotationAppendixG
      ENDIF
      SurfaceTmp(SurfNum)%Tilt=rNumericArgs(2)

      SurfaceTmp(SurfNum)%Sides=4
      ALLOCATE(SurfaceTmp(SurfNum)%Vertex(SurfaceTmp(SurfNum)%Sides))

      CALL MakeRectangularVertices(SurfNum,rNumericArgs(3),  &
                                   rNumericArgs(4),rNumericArgs(5),rNumericArgs(6),rNumericArgs(7),RectSurfRefWorldCoordSystem)

      IF (SurfaceTmp(SurfNum)%Area <= 0.0d0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
              '", Surface Area <= 0.0; Entered Area='//  &
              TRIM(TrimSigDigits(SurfaceTmp(SurfNum)%Area,2)))
        ErrorsFound=.true.
      ENDIF

      IF (MakeMirroredDetachedShading) THEN
        CALL MakeMirrorSurface(SurfNum)
      ENDIF
    ENDDO

  ENDDO ! Item Loop

  RETURN

END SUBROUTINE GetRectDetShdSurfaceData

SUBROUTINE GetHTSurfaceData(ErrorsFound,SurfNum,TotHTSurfs,TotDetailedWalls,TotDetailedRoofs,TotDetailedFloors,  &
                  BaseSurfCls,BaseSurfIDs,NeedToAddSurfaces)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the HeatTransfer Surface Data,
          ! checks it for errors, etc.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
  ! Heat Transfer Surface Definition
  !BuildingSurface:Detailed,
  !  \extensible:3 -- duplicate last set of x,y,z coordinates (last 3 fields), remembering to remove ; from "inner" fields.
  !  \format vertices
  !  A1 , \field Name
  !       \required-field
  !       \type alpha
  !       \reference SurfaceNames
  !       \reference SurfAndSubSurfNames
  !       \reference AllHeatTranSurfNames
  !       \reference HeatTranBaseSurfNames
  !       \reference OutFaceEnvNames
  !       \reference AllHeatTranAngFacNames
  !       \reference RadGroupAndSurfNames
  !       \reference SurfGroupAndHTSurfNames
  !       \reference AllShadingAndHTSurfNames
  !  A2 , \field Surface Type
  !       \required-field
  !       \type choice
  !       \key Floor
  !       \key Wall
  !       \key Ceiling
  !       \key Roof
  !  A3 , \field Construction Name
  !       \required-field
  !       \note To be matched with a construction in this input file
  !       \type object-list
  !       \object-list ConstructionNames
  !  A4 , \field Zone Name
  !       \required-field
  !       \note Zone the surface is a part of
  !       \type object-list
  !       \object-list ZoneNames
  !  A5 , \field Outside Boundary Condition
  !       \required-field
  !       \type choice
  !       \key Adiabatic
  !       \key Surface
  !       \key Zone
  !       \key Outdoors
  !       \key Ground
  !       \key GroundFCfactorMethod
  !       \key OtherSideCoefficients
  !       \key OtherSideConditionsModel
  !       \key GroundSlabPreprocessorAverage
  !       \key GroundSlabPreprocessorCore
  !       \key GroundSlabPreprocessorPerimeter
  !       \key GroundBasementPreprocessorAverageWall
  !       \key GroundBasementPreprocessorAverageFloor
  !       \key GroundBasementPreprocessorUpperWall
  !       \key GroundBasementPreprocessorLowerWall
  !  A6,  \field Outside Boundary Condition Object
  !       \type object-list
  !       \object-list OutFaceEnvNames
  !       \note Non-blank only if the field Outside Boundary Condition is Surface,
  !       \note Zone, OtherSideCoefficients or OtherSideConditionsModel
  !       \note If Surface, specify name of corresponding surface in adjacent zone or
  !       \note specify current surface name for internal partition separating like zones
  !       \note If Zone, specify the name of the corresponding zone and
  !       \note the program will generate the corresponding interzone surface
  !       \note If OtherSideCoefficients, specify name of SurfaceProperty:OtherSideCoefficients
  !       \note If OtherSideConditionsModel, specify name of SurfaceProperty:OtherSideConditionsModel
  !  A7 , \field Sun Exposure
  !       \required-field
  !       \type choice
  !       \key SunExposed
  !       \key NoSun
  !       \default SunExposed
  !  A8,  \field Wind Exposure
  !       \required-field
  !       \type choice
  !       \key WindExposed
  !       \key NoWind
  !       \default WindExposed
  !  N1,  \field View Factor to Ground
  !       \type real
  !       \note From the exterior of the surface
  !       \note Unused if one uses the "reflections" options in Solar Distribution in Building input
  !       \note unless a DaylightingDevice:Shelf or DaylightingDevice:Tubular object has been specified.
  !       \note autocalculate will automatically calculate this value from the tilt of the surface
  !       \autocalculatable
  !       \minimum 0.0
  !       \maximum 1.0
  !       \default autocalculate
  !  N2 , \field Number of Vertices
  !       \note shown with 120 vertex coordinates -- extensible object
  !       \note  "extensible" -- duplicate last set of x,y,z coordinates (last 3 fields),
  !       \note remembering to remove ; from "inner" fields.
  !       \note for clarity in any error messages, renumber the fields as well.
  !       \note (and changing z terminator to a comma "," for all but last one which needs a semi-colon ";")
  !       \autocalculatable
  !       \minimum 3
  !       \default autocalculate
  !       \note vertices are given in GlobalGeometryRules coordinates -- if relative, all surface coordinates
  !       \note are "relative" to the Zone Origin.  If world, then building and zone origins are used
  !       \note for some internal calculations, but all coordinates are given in an "absolute" system.
  !  N3-xx as indicated by the N3 value

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList, VerifyName, SameString, GetObjectDefMaxArgs
  USE General, ONLY: RoundSigDigits,TrimSigDigits


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT)         :: ErrorsFound       ! Error flag indicator (true if errors found)
  INTEGER, INTENT(INOUT)         :: SurfNum           ! Count of Current SurfaceNumber
  INTEGER, INTENT(IN)            :: TotHTSurfs        ! Number of Heat Transfer Base Surfaces to obtain
  INTEGER, INTENT(IN)            :: TotDetailedWalls  ! Number of Wall:Detailed items to obtain
  INTEGER, INTENT(IN)            :: TotDetailedRoofs  ! Number of RoofCeiling:Detailed items to obtain
  INTEGER, INTENT(IN)            :: TotDetailedFloors ! Number of Floor:Detailed items to obtain
  CHARACTER(len=*), DIMENSION(:), INTENT(IN) :: BaseSurfCls  ! Valid Classes for Base Surfaces
  INTEGER, DIMENSION(:), INTENT(IN) :: BaseSurfIDs
  INTEGER, INTENT(OUT)           :: NeedToAddSurfaces ! Number of surfaces to add, based on unentered IZ surfaces

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER, DIMENSION(4) :: cModuleObjects=  &
    (/'BuildingSurface:Detailed',  &
      'Wall:Detailed           ',  &
      'Floor:Detailed          ',  &
      'RoofCeiling:Detailed    '/)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: IOStat            ! IO Status when calling get input subroutine
  INTEGER :: SurfaceNumAlpha   ! Number of material alpha names being passed
  INTEGER :: SurfaceNumProp    ! Number of material properties being passed
  INTEGER :: ZoneNum           ! DO loop counter (zones)
  INTEGER :: Found             ! For matching interzone surfaces
  INTEGER :: Loop
  LOGICAL :: ErrorInName
  LOGICAL :: IsBlank
  INTEGER :: Item
  INTEGER :: ItemsToGet
  INTEGER :: ClassItem
  INTEGER :: ArgPointer
  INTEGER :: numSides

  CALL GetOSCData(ErrorsFound)
  Call GetOSCMData(ErrorsFound)

  NeedToAddSurfaces=0

  DO Item=1,4

    cCurrentModuleObject=cModuleObjects(Item)
    IF (Item == 1) THEN
      ItemsToGet=TotHTSurfs
      ClassItem=0
    ELSEIF (Item == 2) THEN
      ItemsToGet=TotDetailedWalls
      ClassItem=1
    ELSEIF (Item == 3) THEN
      ItemsToGet=TotDetailedFloors
      ClassItem=2
    ELSE !IF (Item == 4) THEN
      ItemsToGet=TotDetailedRoofs
      ClassItem=3
    ENDIF

    CALL GetObjectDefMaxArgs(cCurrentModuleObject,Loop,SurfaceNumAlpha,SurfaceNumProp)
    IF (Item == 1) THEN
      IF (SurfaceNumAlpha /= 8) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Object Definition indicates '// &
                             'not = 8 Alpha Objects, Number Indicated='//  &
                             TRIM(TrimSigDigits(SurfaceNumAlpha)))
        ErrorsFound=.true.
      ENDIF
    ELSE
      IF (SurfaceNumAlpha /= 7) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Object Definition indicates '// &
                             'not = 7 Alpha Objects, Number Indicated='//  &
                             TRIM(TrimSigDigits(SurfaceNumAlpha)))
        ErrorsFound=.true.
      ENDIF
    ENDIF

    DO Loop=1,ItemsToGet
      CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,SurfaceNumAlpha,rNumericArgs,SurfaceNumProp,IOSTAT,  &
                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      ErrorInName=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),SurfaceTmp%Name,SurfNum,ErrorInName,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (ErrorInName) THEN
        CALL ShowContinueError('...each surface name must not duplicate other surface names (of any type)')
        ErrorsFound=.true.
        CYCLE
      ENDIF

      SurfNum=SurfNum+1
      SurfaceTmp(SurfNum)%Name = cAlphaArgs(1)  ! Set the Surface Name in the Derived Type
      ArgPointer=2
      IF (Item == 1) THEN
        IF (cAlphaArgs(2) == 'CEILING') cAlphaArgs(2)='ROOF'
        ClassItem=FindItemInList(cAlphaArgs(2),BaseSurfCls,3)
        IF (ClassItem == 0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                                 '", invalid '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2)))
          ErrorsFound=.true.
        ELSE
          SurfaceTmp(SurfNum)%Class = BaseSurfIDs(ClassItem)
        ENDIF
        ArgPointer=ArgPointer+1
      ELSE
        SurfaceTmp(SurfNum)%Class = BaseSurfIDs(ClassItem)
      ENDIF

      SurfaceTmp(SurfNum)%Construction=FindIteminList(cAlphaArgs(ArgPointer),Construct%Name,TotConstructs)

      IF(SurfaceTmp(SurfNum)%Construction == 0) THEN
        ErrorsFound = .true.
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                               '", invalid '//TRIM(cAlphaFieldNames(ArgPointer))//'="'//TRIM(cAlphaArgs(ArgPointer))//'".')
      ELSEIF (Construct(SurfaceTmp(SurfNum)%Construction)%TypeIsWindow) THEN
        ErrorsFound = .true.
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                               '", invalid '//TRIM(cAlphaFieldNames(ArgPointer))//'="'//TRIM(cAlphaArgs(ArgPointer))//  &
                               '" - has Window materials.')
        IF (Item == 1) THEN
          CALL ShowContinueError('...because '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
        ELSE
          CALL ShowContinueError('...because Surface Type='//trim(BaseSurfCls(ClassItem)))
        ENDIF
      ELSE
        Construct(SurfaceTmp(SurfNum)%Construction)%IsUsed=.true.
        SurfaceTmp(SurfNum)%ConstructionStoredInputValue  = SurfaceTmp(SurfNum)%Construction
      END IF
      SurfaceTmp(SurfNum)%HeatTransSurf=.true.
      SurfaceTmp(SurfNum)%BaseSurf = SurfNum
      SurfaceTmp(SurfNum)%BaseSurfName=SurfaceTmp(SurfNum)%Name

      ArgPointer=ArgPointer+1
      SurfaceTmp(SurfNum)%ZoneName=cAlphaArgs(ArgPointer)
      ZoneNum=FindItemInList(SurfaceTmp(SurfNum)%ZoneName,Zone%Name,NumOfZones)

      IF (ZoneNum /= 0) THEN
        SurfaceTmp(SurfNum)%Zone = ZoneNum
      ELSE
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                               '", invalid '//TRIM(cAlphaFieldNames(ArgPointer))//'="'//TRIM(cAlphaArgs(ArgPointer))//'".')
        SurfaceTmp(SurfNum)%Class=SurfaceTmp(SurfNum)%Class+100
        SurfaceTmp(SurfNum)%ZoneName='Unknown Zone'
        ErrorsFound=.true.
      ENDIF
  ! Get the ExteriorBoundaryCondition flag from input There are 4 conditions that
  ! can take place. The conditions are set with a 0, -1, or -2, or all of the
  ! zone names have to be looked at and generate the interzone array number
      ArgPointer=ArgPointer+1
      SurfaceTmp(SurfNum)%ExtBoundCondName=cAlphaArgs(ArgPointer+1)

      If(SameString(cAlphaArgs(ArgPointer),'Outdoors')) Then
        SurfaceTmp(SurfNum)%ExtBoundCond = ExternalEnvironment

      Else If(SameString(cAlphaArgs(ArgPointer),'Adiabatic'))         Then
        SurfaceTmp(SurfNum)%ExtBoundCond = UnreconciledZoneSurface
        SurfaceTmp(SurfNum)%ExtBoundCondName=SurfaceTmp(SurfNum)%Name

      Else If(SameString(cAlphaArgs(ArgPointer),'Ground'))         Then
        SurfaceTmp(SurfNum)%ExtBoundCond = Ground

        IF (NoGroundTempObjWarning) THEN
          IF (.not. GroundTempObjInput) THEN
            CALL ShowWarningError('GetHTSurfaceData: Surfaces with interface to Ground '//  &
               'found but no "Ground Temperatures" were input.')
            CALL ShowContinueError('Found first in surface='//TRIM(cAlphaArgs(1)))
            CALL ShowContinueError('Defaults, constant throughout the year of ('//TRIM(RoundSigDigits(GroundTemp,1))// &
                               ') will be used.')
          ENDIF
          NoGroundTempObjWarning=.false.
        ENDIF

      ! Added for FCfactor method
      Else If(SameString(cAlphaArgs(ArgPointer),'GroundFCfactorMethod'))         Then
        SurfaceTmp(SurfNum)%ExtBoundCond = GroundFCfactorMethod
        IF (NoFCGroundTempObjWarning) THEN
          IF (.not. FCGroundTemps) THEN
            CALL ShowSevereError('GetHTSurfaceData: Surfaces with interface to GroundFCfactorMethod found '//  &
                'but no "FC Ground Temperatures" were input.')
            CALL ShowContinueError('Found first in surface='//TRIM(cAlphaArgs(1)))
            CALL ShowContinueError('Either add a "Site:GroundTemperature:FCfactorMethod" object or '//  &
                ' use a weather file with Ground Temperatures.')
            ErrorsFound=.true.
            NoFCGroundTempObjWarning=.false.
          ENDIF
        End IF
        IF (SurfaceTmp(SurfNum)%Construction > 0) THEN
          IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Wall .and.   &
              .not. Construct(SurfaceTmp(SurfNum)%Construction)%TypeIsCfactorWall) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                                 '", invalid '//TRIM(cAlphaFieldNames(ArgPointer)))
            CALL ShowContinueError('Construction="'//trim(Construct(SurfaceTmp(SurfNum)%Construction)%Name)//  &
               '" is not type Construction:CfactorUndergroundWall.')
          ErrorsFound=.true.
          ENDIF
          IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Floor .and.   &
              .not. Construct(SurfaceTmp(SurfNum)%Construction)%TypeIsFfactorFloor) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                                 '", invalid '//TRIM(cAlphaFieldNames(ArgPointer)))
            CALL ShowContinueError('Construction="'//trim(Construct(SurfaceTmp(SurfNum)%Construction)%Name)//  &
               '" is not type Construction:FfactorGroundFloor.')
          ErrorsFound=.true.
          ENDIF
        ENDIF

      Else If(SameString(cAlphaArgs(ArgPointer),'OtherSideCoefficients')) Then
        Found=FindItemInList(SurfaceTmp(SurfNum)%ExtBoundCondName,OSC%Name,TotOSC)
        IF (Found == 0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                      '", invalid '//TRIM(cAlphaFieldNames(ArgPointer+1))//'="'//TRIM(cAlphaArgs(ArgPointer+1))//'".')
          CALL ShowContinueError(' no OtherSideCoefficients of that name.')
          ErrorsFound=.true.
        ELSE
          SurfaceTmp(SurfNum)%OSCPtr=Found
          IF (OSC(Found)%SurfFilmCoef > 0.0d0) THEN
            SurfaceTmp(SurfNum)%ExtBoundCond = OtherSideCoefCalcExt
          ELSE
            SurfaceTmp(SurfNum)%ExtBoundCond = OtherSideCoefNoCalcExt
          ENDIF
        ENDIF

      Else If (SameString(cAlphaArgs(ArgPointer),'Surface')) Then
            ! it has to be another surface which needs to be found
            ! this will be found on the second pass through the surface input
            ! for flagging, set the value to UnreconciledZoneSurface
        ! name (ExtBoundCondName) will be validated later.
        SurfaceTmp(SurfNum)%ExtBoundCond = UnreconciledZoneSurface
        IF (lAlphaFieldBlanks(ArgPointer+1)) THEN
          SurfaceTmp(SurfNum)%ExtBoundCondName=SurfaceTmp(SurfNum)%Name
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                        '", invalid '//TRIM(cAlphaFieldNames(ArgPointer+1))//'=<blank>.')
          CALL ShowContinueError('..'//trim(cAlphaFieldNames(ArgPointer))//'="Surface" must be non-blank.')
          CALL ShowContinueError('..This surface will become an adiabatic surface - no doors/windows allowed.')
        ENDIF

      Else If (SameString(cAlphaArgs(ArgPointer),'Zone')) Then
            ! This is the code for an unmatched "other surface"
            ! will be set up later.
        SurfaceTmp(SurfNum)%ExtBoundCond = UnenteredAdjacentZoneSurface
        ! check OutsideFaceEnvironment for legal zone
        Found=FindItemInList(SurfaceTmp(SurfNum)%ExtBoundCondName,Zone%Name,NumOfZones)
        NeedToAddSurfaces=NeedToAddSurfaces+1

        IF (Found == 0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                        '", invalid '//TRIM(cAlphaFieldNames(ArgPointer))//'="'//TRIM(cAlphaArgs(ArgPointer))//'".')
          CALL ShowContinueError('..Referenced as Zone for this surface.')
          ErrorsFound=.true.
        ENDIF

      ELSE IF (SameString(cAlphaArgs(ArgPointer), 'OtherSideConditionsModel')) Then
        Found=FindItemInList(SurfaceTmp(SurfNum)%ExtBoundCondName,OSCM%Name,TotOSCM)
        IF (Found == 0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                        '", invalid '//TRIM(cAlphaFieldNames(ArgPointer))//'="'//TRIM(cAlphaArgs(ArgPointer))//'".')
          ErrorsFound=.true.
        ENDIF
        SurfaceTmp(SurfNum)%OSCMPtr=Found
        SurfaceTmp(SurfNum)%ExtBoundCond = OtherSideCondModeledExt

      Else If (SameString(cAlphaArgs(ArgPointer),'GroundSlabPreprocessorAverage')          .or.  &
               SameString(cAlphaArgs(ArgPointer),'GroundSlabPreprocessorCore')             .or.  &
               SameString(cAlphaArgs(ArgPointer),'GroundSlabPreprocessorPerimeter')        .or.  &
               SameString(cAlphaArgs(ArgPointer),'GroundBasementPreprocessorAverageFloor') .or.  &
               SameString(cAlphaArgs(ArgPointer),'GroundBasementPreprocessorAverageWall')  .or.  &
               SameString(cAlphaArgs(ArgPointer),'GroundBasementPreprocessorUpperWall')    .or.  &
               SameString(cAlphaArgs(ArgPointer),'GroundBasementPreprocessorLowerWall') )  Then
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                      '", invalid '//TRIM(cAlphaFieldNames(ArgPointer))//'="'//TRIM(cAlphaArgs(ArgPointer))//'".')
        CALL ShowContinueError('The ExpandObjects program has not been run or is not in your EnergyPlus.exe folder.')
        ErrorsFound=.true.

      Else
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                      '", invalid '//TRIM(cAlphaFieldNames(ArgPointer))//'="'//TRIM(cAlphaArgs(ArgPointer))//'".')
        CALL ShowContinueError('Should be one of "Outdoors", "Adiabatic", Ground", "Surface",'//  &
                               ' "OtherSideCoefficients", "OtherSideConditionsModel" or "Zone"')
        ErrorsFound=.true.
      End If  ! ... End of the ExtBoundCond logical IF Block


      ArgPointer=ArgPointer+2
      !Set the logical flag for the exterior solar
      IF (SameString(cAlphaArgs(ArgPointer),'SunExposed')) THEN
        SurfaceTmp(SurfNum)%ExtSolar=.true.

        IF ((SurfaceTmp(SurfNum)%ExtBoundCond /= ExternalEnvironment) .AND. &
            (SurfaceTmp(SurfNum)%ExtBoundCond /= OtherSideCondModeledExt) ) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                         '", '//TRIM(cAlphaFieldNames(ArgPointer))//'="'//TRIM(cAlphaArgs(ArgPointer))//'".')
          CALL ShowContinueError('..This surface is not exposed to External Environment.  Sun exposure has no effect.')
        ENDIF

      ELSE IF (SameString(cAlphaArgs(ArgPointer),'NoSun')) THEN
        SurfaceTmp(SurfNum)%ExtSolar=.false.
      ELSE
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                      '", invalid '//TRIM(cAlphaFieldNames(ArgPointer))//'="'//TRIM(cAlphaArgs(ArgPointer))//'".')
        ErrorsFound=.true.
      END IF

      ArgPointer=ArgPointer+1
      !Set the logical flag for the exterior wind
      If (SameString(cAlphaArgs(ArgPointer),'WindExposed')) THEN
        SurfaceTmp(SurfNum)%ExtWind=.true.
      ElseIf (SameString(cAlphaArgs(ArgPointer),'NoWind')) THEN
        SurfaceTmp(SurfNum)%ExtWind=.false.
      Else
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                      '", invalid '//TRIM(cAlphaFieldNames(ArgPointer))//'="'//TRIM(cAlphaArgs(ArgPointer))//'".')
        ErrorsFound=.true.
      End If

      !Set the logical flag for the EcoRoof presented, this is only based on the flag in the construction type
      IF (SurfaceTmp(SurfNum)%Construction > 0) &
        SurfaceTmp(SurfNum)%ExtEcoRoof=Construct(SurfaceTmp(SurfNum)%Construction)%TypeIsEcoRoof

      SurfaceTmp(SurfNum)%ViewFactorGround = rNumericArgs(1)
      IF (lNumericFieldBlanks(1)) SurfaceTmp(SurfNum)%ViewFactorGround = AutoCalculate
      IF (lNumericFieldBlanks(2) .or. rNumericArgs(2) == AutoCalculate) THEN
        numSides=(SurfaceNumProp-2)/3
        SurfaceTmp(SurfNum)%Sides=numSides
        IF (MOD(SurfaceNumProp-2,3) /= 0) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                          '", '//TRIM(cNumericFieldNames(2))//          &
                          ' not even multiple of 3. Will read in '//   &
                          TRIM(TrimSigDigits(SurfaceTmp(SurfNum)%Sides)))
        ENDIF
        IF (numSides < 3) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                         '", '//TRIM(cNumericFieldNames(2))//' (autocalculate) must be >= 3. Only '//  &
                         TRIM(TrimSigDigits(SurfaceTmp(SurfNum)%Sides))//' provided.')
          ErrorsFound=.true.
          CYCLE
        ENDIF
      ELSE
        numSides=(SurfaceNumProp-2)/3
        SurfaceTmp(SurfNum)%Sides=rNumericArgs(2)
        IF (numSides > SurfaceTmp(SurfNum)%Sides) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                          '", field '//TRIM(cNumericFieldNames(2))//'='//TRIM(TrimSigDigits(SurfaceTmp(SurfNum)%Sides)))
          CALL ShowContinueError('...but '//TRIM(TrimSigDigits(numSides))//' were entered. Only the indicated '//  &
                          TRIM(cNumericFieldNames(2))//' will be used.')
        ENDIF
      ENDIF
      ALLOCATE(SurfaceTmp(SurfNum)%Vertex(SurfaceTmp(SurfNum)%Sides))
      CALL GetVertices(SurfNum,SurfaceTmp(SurfNum)%Sides,rNumericArgs(3:))
      IF (SurfaceTmp(SurfNum)%Area <= 0.0d0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
              '", Surface Area <= 0.0; Entered Area='//  &
              TRIM(TrimSigDigits(SurfaceTmp(SurfNum)%Area,2)))
        ErrorsFound=.true.
      ENDIF

      CALL CheckConvexity(SurfNum,SurfaceTmp(SurfNum)%Sides)
      IF (SurfaceTmp(SurfNum)%Construction > 0) THEN
        !Check wall height for the CFactor walls
        IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Wall .and.   &
            Construct(SurfaceTmp(SurfNum)%Construction)%TypeIsCfactorWall) THEN
          IF (ABS(SurfaceTmp(SurfNum)%Height - Construct(SurfaceTmp(SurfNum)%Construction)%Height)>0.05d0) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                       '", underground Wall Height = '//TRIM(TrimSigDigits(SurfaceTmp(SurfNum)%Height,2)))
            CALL ShowContinueError('..which does not match its construction height.')
          ENDIF
        ENDIF

        !Check area and perimeter for the FFactor floors
        IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Floor .and.   &
            Construct(SurfaceTmp(SurfNum)%Construction)%TypeIsFfactorFloor) THEN
          IF (ABS(SurfaceTmp(SurfNum)%Area - Construct(SurfaceTmp(SurfNum)%Construction)%Area)>0.1d0) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                       '", underground Floor Area = '//TRIM(TrimSigDigits(SurfaceTmp(SurfNum)%Area,2)))
            CALL ShowContinueError('..which does not match its construction area.')
          ENDIF
          IF (SurfaceTmp(SurfNum)%Perimeter < Construct(SurfaceTmp(SurfNum)%Construction)%PerimeterExposed - 0.1d0) THEN
            CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                       '", underground Floor Perimeter = '//TRIM(TrimSigDigits(SurfaceTmp(SurfNum)%Perimeter,2)))
            CALL ShowContinueError('..which is less than its construction exposed perimeter.')
          ENDIF
        ENDIF
      ENDIF

    ENDDO
  ENDDO  ! Item Looop
  RETURN

END SUBROUTINE GetHTSurfaceData

SUBROUTINE GetRectSurfaces(ErrorsFound,SurfNum,TotRectExtWalls,TotRectIntWalls,TotRectIZWalls,TotRectUGWalls,  &
      TotRectRoofs,TotRectCeilings,TotRectIZCeilings,TotRectGCFloors,TotRectIntFloors,TotRectIZFloors,     &
      BaseSurfIDs,NeedToAddSurfaces)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   December 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Get simple (rectangular, LLC corner specified) walls

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList, VerifyName, GetObjectDefMaxArgs
  USE General, ONLY: TrimSigDigits,RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT)         :: ErrorsFound       ! Error flag indicator (true if errors found)
  INTEGER, INTENT(INOUT)         :: SurfNum           ! Count of Current SurfaceNumber
  INTEGER, INTENT(IN)            :: TotRectExtWalls   ! Number of Exterior Walls to obtain
  INTEGER, INTENT(IN)            :: TotRectIntWalls   ! Number of Adiabatic Walls to obtain
  INTEGER, INTENT(IN)            :: TotRectIZWalls    ! Number of Interzone Walls to obtain
  INTEGER, INTENT(IN)            :: TotRectUGWalls    ! Number of Underground to obtain
  INTEGER, INTENT(IN)            :: TotRectRoofs      ! Number of Roofs to obtain
  INTEGER, INTENT(IN)            :: TotRectCeilings   ! Number of Adiabatic Ceilings to obtain
  INTEGER, INTENT(IN)            :: TotRectIZCeilings ! Number of Interzone Ceilings to obtain
  INTEGER, INTENT(IN)            :: TotRectGCFloors   ! Number of Floors with Ground Contact to obtain
  INTEGER, INTENT(IN)            :: TotRectIntFloors  ! Number of Adiabatic Walls to obtain
  INTEGER, INTENT(IN)            :: TotRectIZFloors   ! Number of Interzone Floors to obtain
  INTEGER, DIMENSION(:), INTENT(IN) :: BaseSurfIDs    ! ID Assignments for valid surface classes
  INTEGER, INTENT(INOUT)         :: NeedToAddSurfaces ! Number of surfaces to add, based on unentered IZ surfaces

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER, DIMENSION(10) :: cModuleObjects=  &
    (/'Wall:Exterior      ',  &
      'Wall:Adiabatic     ',  &
      'Wall:Interzone     ',  &
      'Wall:Underground   ',  &
      'Roof               ',  &
      'Ceiling:Adiabatic  ',  &
      'Ceiling:Interzone  ',  &
      'Floor:GroundContact',  &
      'Floor:Adiabatic    ',  &
      'Floor:Interzone    '/)


          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

  INTEGER :: Item
  INTEGER :: ItemsToGet
  INTEGER :: Loop
  INTEGER :: NumAlphas
  INTEGER :: NumNumbers
  INTEGER :: IOStat            ! IO Status when calling get input subroutine
  INTEGER :: Found             ! For matching base surfaces
  LOGICAL :: ErrorInName
  LOGICAL :: IsBlank
  LOGICAL :: GettingIZSurfaces
  INTEGER :: OtherSurfaceField
  INTEGER :: ExtBoundCondition
  INTEGER :: ClassItem
  INTEGER :: ZoneNum

  DO Item=1,10

    cCurrentModuleObject=cModuleObjects(Item)
    IF (Item == 1) THEN
      ItemsToGet=TotRectExtWalls
      GettingIZSurfaces=.false.
      OtherSurfaceField=0
      ExtBoundCondition=ExternalEnvironment
      ClassItem=1
    ELSEIF (Item == 2) THEN
      ItemsToGet=TotRectIntWalls
      GettingIZSurfaces=.false.
      OtherSurfaceField=0
      ExtBoundCondition=UnreconciledZoneSurface
      ClassItem=1
    ELSEIF (Item == 3) THEN
      ItemsToGet=TotRectIZWalls
      GettingIZSurfaces=.true.
      OtherSurfaceField=4
      ExtBoundCondition=UnreconciledZoneSurface
      ClassItem=1
    ELSEIF (Item == 4) THEN
      ItemsToGet=TotRectUGWalls
      GettingIZSurfaces=.false.
      OtherSurfaceField=0
      ExtBoundCondition=Ground
      ClassItem=1
    ELSEIF (Item == 5) THEN
      ItemsToGet=TotRectRoofs
      GettingIZSurfaces=.false.
      OtherSurfaceField=0
      ExtBoundCondition=ExternalEnvironment
      ClassItem=3
    ELSEIF (Item == 6) THEN
      ItemsToGet=TotRectCeilings
      GettingIZSurfaces=.false.
      OtherSurfaceField=0
      ExtBoundCondition=UnreconciledZoneSurface
      ClassItem=3
    ELSEIF (Item == 7) THEN
      ItemsToGet=TotRectIZCeilings
      GettingIZSurfaces=.false.
      OtherSurfaceField=4
      ExtBoundCondition=UnreconciledZoneSurface
      ClassItem=3
    ELSEIF (Item == 8) THEN
      ItemsToGet=TotRectGCFloors
      GettingIZSurfaces=.false.
      OtherSurfaceField=0
      ExtBoundCondition=Ground
      ClassItem=2
    ELSEIF (Item == 9) THEN
      ItemsToGet=TotRectIntFloors
      GettingIZSurfaces=.false.
      OtherSurfaceField=0
      ExtBoundCondition=UnreconciledZoneSurface
      ClassItem=2
    ELSE  !IF (Item == 10) THEN
      ItemsToGet=TotRectIZFloors
      GettingIZSurfaces=.true.
      OtherSurfaceField=4
      ExtBoundCondition=UnreconciledZoneSurface
      ClassItem=2
    ENDIF

    DO Loop=1,ItemsToGet
      CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOSTAT,  &
                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      ErrorInName=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),SurfaceTmp%Name,SurfNum,ErrorInName,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (ErrorInName) THEN
        CALL ShowContinueError('...each surface name must not duplicate other surface names (of any type)')
        ErrorsFound=.true.
        CYCLE
      ENDIF

      IF (NumNumbers < 7) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                        '", Too few number of numeric args=['//TRIM(TrimSigDigits(NumNumbers))//'].')
        ErrorsFound=.true.
      ENDIF

      SurfNum=SurfNum+1
      SurfaceTmp(SurfNum)%Name = cAlphaArgs(1)  ! Set the Surface Name in the Derived Type
      SurfaceTmp(SurfNum)%Class = BaseSurfIDs(ClassItem) ! Set class number

      SurfaceTmp(SurfNum)%Construction=FindIteminList(cAlphaArgs(2),Construct%Name,TotConstructs)

      IF(SurfaceTmp(SurfNum)%Construction == 0) THEN
        ErrorsFound = .true.
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                               '", invalid '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
      ELSEIF (Construct(SurfaceTmp(SurfNum)%Construction)%TypeIsWindow) THEN
        ErrorsFound = .true.
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                               '", invalid '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(2))//  &
                               '" - has Window materials.')
        CALL ShowContinueError('...because '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
      ELSE
        Construct(SurfaceTmp(SurfNum)%Construction)%IsUsed=.true.
        SurfaceTmp(SurfNum)%ConstructionStoredInputValue  = SurfaceTmp(SurfNum)%Construction
      END IF
      SurfaceTmp(SurfNum)%HeatTransSurf=.true.
      SurfaceTmp(SurfNum)%BaseSurf = SurfNum
      SurfaceTmp(SurfNum)%BaseSurfName=SurfaceTmp(SurfNum)%Name

      SurfaceTmp(SurfNum)%ZoneName=cAlphaArgs(3)
      ZoneNum=FindItemInList(SurfaceTmp(SurfNum)%ZoneName,Zone%Name,NumOfZones)

      IF (ZoneNum /= 0) THEN
        SurfaceTmp(SurfNum)%Zone = ZoneNum
      ELSE
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                               '", invalid '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
        SurfaceTmp(SurfNum)%Class=SurfaceTmp(SurfNum)%Class+100
        SurfaceTmp(SurfNum)%ZoneName='Unknown Zone'
        ErrorsFound=.true.
      ENDIF

      SurfaceTmp(SurfNum)%ExtBoundCond = ExtBoundCondition
      IF (SurfaceTmp(SurfNum)%Construction > 0) THEN
        IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Wall .and.   &
            Construct(SurfaceTmp(SurfNum)%Construction)%TypeIsCfactorWall .and. &
            SurfaceTmp(SurfNum)%ExtBoundCond == Ground) THEN
              SurfaceTmp(SurfNum)%ExtBoundCond = GroundFCfactorMethod
        ELSEIF (Construct(SurfaceTmp(SurfNum)%Construction)%TypeIsCfactorWall) THEN
          ErrorsFound = .true.
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
            '", Construction type is "Construction:CfactorUndergroundWall" but invalid for this object.')
        ENDIF
        IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Floor .and.   &
            Construct(SurfaceTmp(SurfNum)%Construction)%TypeIsFfactorFloor .and. &
            SurfaceTmp(SurfNum)%ExtBoundCond == Ground) THEN
              SurfaceTmp(SurfNum)%ExtBoundCond = GroundFCfactorMethod
        ELSEIF (Construct(SurfaceTmp(SurfNum)%Construction)%TypeIsFfactorFloor) THEN
          ErrorsFound = .true.
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
            '", Construction type is "Construction:FfactorGroundFloor" but invalid for this object.')
        ENDIF
      ENDIF
      SurfaceTmp(SurfNum)%ExtSolar=.false.
      SurfaceTmp(SurfNum)%ExtWind=.false.
      SurfaceTmp(SurfNum)%ViewFactorGround = AutoCalculate

      IF (SurfaceTmp(SurfNum)%ExtBoundCond == ExternalEnvironment) THEN
        SurfaceTmp(SurfNum)%ExtSolar=.true.
        SurfaceTmp(SurfNum)%ExtWind=.true.

        !Set the logical flag for the EcoRoof presented, this is only based on the flag in the construction type
        IF (SurfaceTmp(SurfNum)%Construction > 0) &
          SurfaceTmp(SurfNum)%ExtEcoRoof=Construct(SurfaceTmp(SurfNum)%Construction)%TypeIsEcoRoof

      Else If (SurfaceTmp(SurfNum)%ExtBoundCond == UnreconciledZoneSurface) THEN
        IF (GettingIZSurfaces) THEN
          SurfaceTmp(SurfNum)%ExtBoundCondName=cAlphaArgs(OtherSurfaceField)
          Found=FindItemInList(SurfaceTmp(SurfNum)%ExtBoundCondName,Zone%Name,NumOfZones)
          ! see if match to zone, then it's an unentered other surface, else reconciled later
          IF (Found > 0) THEN
            NeedToAddSurfaces=NeedToAddSurfaces+1
            SurfaceTmp(SurfNum)%ExtBoundCond = UnenteredAdjacentZoneSurface
          ENDIF
        ELSE
          SurfaceTmp(SurfNum)%ExtBoundCondName=SurfaceTmp(SurfNum)%Name
        ENDIF

      Else If (SurfaceTmp(SurfNum)%ExtBoundCond == Ground) THEN

        IF (NoGroundTempObjWarning) THEN
          IF (.not. GroundTempObjInput) THEN
            CALL ShowWarningError('GetRectSurfaces: Surfaces with interface to Ground found '//  &
                 'but no "Ground Temperatures" were input.')
            CALL ShowContinueError('Found first in surface='//TRIM(cAlphaArgs(1)))
            CALL ShowContinueError('Defaults, constant throughout the year of ('//TRIM(RoundSigDigits(GroundTemp,1))// &
                               ') will be used.')
          ENDIF
          NoGroundTempObjWarning=.false.
        ENDIF

      Else If (SurfaceTmp(SurfNum)%ExtBoundCond == GroundFCfactorMethod) THEN
        IF (NoFCGroundTempObjWarning) THEN
          IF (.not. FCGroundTemps) THEN
            CALL ShowSevereError('GetRectSurfaces: Surfaces with interface to GroundFCfactorMethod found '//  &
                'but no "FC Ground Temperatures" were input.')
            CALL ShowContinueError('Found first in surface='//TRIM(cAlphaArgs(1)))
            CALL ShowContinueError('Either add a "Site:GroundTemperature:FCfactorMethod" object or '//  &
                ' use a weather file with Ground Temperatures.')
            ErrorsFound=.true.
            NoFCGroundTempObjWarning=.false.
          ENDIF
        End IF

      End If  ! ... End of the ExtBoundCond logical IF Block

      SurfaceTmp(SurfNum)%Azimuth=rNumericArgs(1)
      SurfaceTmp(SurfNum)%Tilt=rNumericArgs(2)
      IF (.not. WorldCoordSystem) THEN
        IF (ZoneNum /= 0) THEN
          SurfaceTmp(SurfNum)%Azimuth=SurfaceTmp(SurfNum)%Azimuth+BuildingAzimuth+Zone(ZoneNum)%RelNorth
        ENDIF
      ENDIF
      IF (ZoneNum /= 0) THEN
        SurfaceTmp(SurfNum)%Azimuth=SurfaceTmp(SurfNum)%Azimuth+BuildingRotationAppendixG
      ENDIF

      SurfaceTmp(SurfNum)%Sides=4
      ALLOCATE(SurfaceTmp(SurfNum)%Vertex(SurfaceTmp(SurfNum)%Sides))

      CALL MakeRectangularVertices(SurfNum,rNumericArgs(3),  &
                                   rNumericArgs(4),rNumericArgs(5),rNumericArgs(6),rNumericArgs(7),RectSurfRefWorldCoordSystem)

      IF (SurfaceTmp(SurfNum)%Area <= 0.0d0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
              '", Surface Area <= 0.0; Entered Area='//  &
              TRIM(TrimSigDigits(SurfaceTmp(SurfNum)%Area,2)))
        ErrorsFound=.true.
      ENDIF

      !Check wall height for the CFactor walls
      IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Wall .and. SurfaceTmp(SurfNum)%ExtBoundCond == GroundFCfactorMethod) THEN
        IF (ABS(SurfaceTmp(SurfNum)%Height - Construct(SurfaceTmp(SurfNum)%Construction)%Height)>0.05d0) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                     '", underground Wall Height = '//TRIM(TrimSigDigits(SurfaceTmp(SurfNum)%Height,2)))
          CALL ShowContinueError('..which deos not match its construction height.')
        ENDIF
      ENDIF

      !Check area and perimeter for the FFactor floors
      IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Floor .and. SurfaceTmp(SurfNum)%ExtBoundCond == GroundFCfactorMethod) THEN
        IF (ABS(SurfaceTmp(SurfNum)%Area - Construct(SurfaceTmp(SurfNum)%Construction)%Area)>0.1d0) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                     '", underground Floor Area = '//TRIM(TrimSigDigits(SurfaceTmp(SurfNum)%Area,2)))
          CALL ShowContinueError('..which does not match its construction area.')
        ENDIF
        IF (SurfaceTmp(SurfNum)%Perimeter < Construct(SurfaceTmp(SurfNum)%Construction)%PerimeterExposed - 0.1d0) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                     '", underground Floor Perimeter = '//TRIM(TrimSigDigits(SurfaceTmp(SurfNum)%Perimeter,2)))
          CALL ShowContinueError('..which is less than its construction exposed perimeter.')
        ENDIF
      ENDIF
    ENDDO  ! Getting Items

  ENDDO

  RETURN

END SUBROUTINE GetRectSurfaces

SUBROUTINE MakeRectangularVertices(SurfNum,XCoord,YCoord,ZCoord,Length,Height,SurfWorldCoordSystem)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   December 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine creates world/3d coordinates for rectangular surfaces using azimuth, tilt, LLC (X,Y,Z), length & height.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Vectors

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: SurfNum
  REAL(r64), INTENT(IN) :: XCoord
  REAL(r64), INTENT(IN) :: YCoord
  REAL(r64), INTENT(IN) :: ZCoord
  REAL(r64), INTENT(IN) :: Length
  REAL(r64), INTENT(IN) :: Height
  LOGICAL, INTENT(IN) :: SurfWorldCoordSystem

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: SurfAzimuth  ! Surface Azimuth/Facing (same as Base Surface)
  REAL(r64) :: SurfTilt     ! Tilt (same as Base Surface)
  REAL(r64) :: XLLC
  REAL(r64) :: YLLC
  REAL(r64) :: ZLLC
  REAL(r64) :: CosSurfAzimuth
  REAL(r64) :: SinSurfAzimuth
  REAL(r64) :: CosSurfTilt
  REAL(r64) :: SinSurfTilt
  REAL(r64) :: XX(4),YY(4)
  REAL(r64) :: Xb, Yb
  REAL(r64) :: Perimeter
  INTEGER :: N
  INTEGER :: Vrt

  IF (SurfaceTmp(SurfNum)%Zone == 0 .and. &
     (SurfaceTmp(SurfNum)%Class /= SurfaceClass_Detached_F .and.   &
      SurfaceTmp(SurfNum)%Class /= SurfaceClass_Detached_B)) RETURN

  SurfaceTmp(SurfNum)%Height=Height
  SurfaceTmp(SurfNum)%Width=Length

  SurfAzimuth = SurfaceTmp(SurfNum)%Azimuth
  SurfTilt    = SurfaceTmp(SurfNum)%Tilt
  CosSurfAzimuth=COS(SurfAzimuth*DegToRadians)
  SinSurfAzimuth=SIN(SurfAzimuth*DegToRadians)
  CosSurfTilt=COS(SurfTilt*DegToRadians)
  SinSurfTilt=SIN(SurfTilt*DegToRadians)
  IF (.not. SurfWorldCoordSystem) THEN
    IF (SurfaceTmp(SurfNum)%Zone > 0) THEN
      Xb    = XCoord*CosZoneRelNorth(SurfaceTmp(SurfNum)%Zone) &
             -YCoord*SinZoneRelNorth(SurfaceTmp(SurfNum)%Zone) + Zone(SurfaceTmp(SurfNum)%Zone)%OriginX
      Yb    = XCoord*SinZoneRelNorth(SurfaceTmp(SurfNum)%Zone) &
             +YCoord*CosZoneRelNorth(SurfaceTmp(SurfNum)%Zone) + Zone(SurfaceTmp(SurfNum)%Zone)%OriginY
      XLLC  = Xb*CosBldgRelNorth - Yb*SinBldgRelNorth
      YLLC  = Xb*SinBldgRelNorth + Yb*CosBldgRelNorth
      ZLLC  = ZCoord + Zone(SurfaceTmp(SurfNum)%Zone)%OriginZ
    ELSE
      IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Detached_B) THEN
        Xb    = XCoord
        Yb    = YCoord
        XLLC  = Xb*CosBldgRelNorth - Yb*SinBldgRelNorth
        YLLC  = Xb*SinBldgRelNorth + Yb*CosBldgRelNorth
        ZLLC  = ZCoord
      ELSE
        XLLC  = XCoord
        YLLC  = YCoord
        ZLLC  = ZCoord
      ENDIF
    ENDIF
  ELSE
    ! for world coordinates, only rotate for appendix G
    Xb  = XCoord
    Yb  = YCoord
    ZLLC  = ZCoord
    IF (SurfaceTmp(SurfNum)%Class /= SurfaceClass_Detached_F) THEN
      XLLC= Xb*CosBldgRotAppGonly - Yb*SinBldgRotAppGonly
      YLLC= Xb*SinBldgRotAppGonly + Yb*CosBldgRotAppGonly
    ELSE
      XLLC = Xb
      YLLC = Yb
    ENDIF
  ENDIF

  XX(1)=0.0d0
  XX(2)=0.0d0
  XX(3)=Length
  XX(4)=Length
  YY(1)=Height
  YY(4)=Height
  YY(3)=0.0d0
  YY(2)=0.0d0

  DO N = 1, SurfaceTmp(SurfNum)%Sides
    Vrt=N
    SurfaceTmp(SurfNum)%Vertex(Vrt)%X=XLLC-XX(N)*CosSurfAzimuth-YY(N)*CosSurfTilt*SinSurfAzimuth
    SurfaceTmp(SurfNum)%Vertex(Vrt)%Y=YLLC+XX(N)*SinSurfAzimuth-YY(N)*CosSurfTilt*CosSurfAzimuth
    SurfaceTmp(SurfNum)%Vertex(Vrt)%Z=ZLLC+YY(N)*SinSurfTilt
  END DO

  CALL CreateNewellAreaVector(SurfaceTmp(SurfNum)%Vertex,SurfaceTmp(SurfNum)%Sides,SurfaceTmp(SurfNum)%NewellAreaVector)
  SurfaceTmp(SurfNum)%GrossArea=VecLength(SurfaceTmp(SurfNum)%NewellAreaVector)
  SurfaceTmp(SurfNum)%Area=SurfaceTmp(SurfNum)%GrossArea
  SurfaceTmp(SurfNum)%NetAreaShadowCalc = SurfaceTmp(SurfNum)%Area
  CALL CreateNewellSurfaceNormalVector(SurfaceTmp(SurfNum)%Vertex,SurfaceTmp(SurfNum)%Sides,  &
     SurfaceTmp(SurfNum)%NewellSurfaceNormalVector)
  CALL DetermineAzimuthAndTilt(SurfaceTmp(SurfNum)%Vertex,SurfaceTmp(SurfNum)%Sides,SurfAzimuth,SurfTilt,  &
                               SurfaceTmp(SurfNum)%lcsx,SurfaceTmp(SurfNum)%lcsy,SurfaceTmp(SurfNum)%lcsz, &
                               SurfaceTmp(SurfNum)%GrossArea,SurfaceTmp(SurfNum)%NewellSurfaceNormalVector)
  SurfaceTmp(SurfNum)%Azimuth=SurfAzimuth
  SurfaceTmp(SurfNum)%Tilt=SurfTilt
  ! Sine and cosine of azimuth and tilt
  SurfaceTmp(SurfNum)%SinAzim = SinSurfAzimuth
  SurfaceTmp(SurfNum)%CosAzim = CosSurfAzimuth
  SurfaceTmp(SurfNum)%SinTilt = SinSurfTilt
  SurfaceTmp(SurfNum)%CosTilt = CosSurfTilt
  SurfaceTmp(SurfNum)%ViewFactorGround = 0.5d0 * (1.0d0 - SurfaceTmp(SurfNum)%CosTilt)
  ! Outward normal unit vector (pointing away from room)
  SurfaceTmp(SurfNum)%OutNormVec = SurfaceTmp(SurfNum)%NewellSurfaceNormalVector
  DO N=1,3
    IF (ABS(SurfaceTmp(SurfNum)%OutNormVec(N)-1.0d0) < 1.d-06) SurfaceTmp(SurfNum)%OutNormVec(N) = +1.0d0
    IF (ABS(SurfaceTmp(SurfNum)%OutNormVec(N)+1.0d0) < 1.d-06) SurfaceTmp(SurfNum)%OutNormVec(N) = -1.0d0
    IF (ABS(SurfaceTmp(SurfNum)%OutNormVec(N))     < 1.d-06) SurfaceTmp(SurfNum)%OutNormVec(N) =  0.0d0
  ENDDO

!  IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Roof .and. SurfTilt > 80.) THEN
!    WRITE(TiltString,'(F5.1)') SurfTilt
!    TiltString=ADJUSTL(TiltString)
!    CALL ShowWarningError('Roof/Ceiling Tilt='//TRIM(TiltString)//', much greater than expected tilt of 0,'// &
!                          ' for Surface='//TRIM(SurfaceTmp(SurfNum)%Name)//  &
!                          ', in Zone='//TRIM(SurfaceTmp(SurfNum)%ZoneName))
!  ENDIF
!  IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Floor .and. SurfTilt < 170.) THEN
!    WRITE(TiltString,'(F5.1)') SurfTilt
!    TiltString=ADJUSTL(TiltString)
!    CALL ShowWarningError('Floor Tilt='//TRIM(TiltString)//', much less than expected tilt of 180,'//   &
!                          ' for Surface='//TRIM(SurfaceTmp(SurfNum)%Name)//  &
!                          ', in Zone='//TRIM(SurfaceTmp(SurfNum)%ZoneName))
!  ENDIF

  ! Can perform tests on this surface here
  SurfaceTmp(SurfNum)%ViewFactorSky=0.5d0*(1.d0+SurfaceTmp(SurfNum)%CosTilt)
  ! The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
  ! surfaces
  SurfaceTmp(SurfNum)%ViewFactorSkyIR = SurfaceTmp(SurfNum)%ViewFactorSky
  SurfaceTmp(SurfNum)%ViewFactorGroundIR = 0.5d0*(1.d0-SurfaceTmp(SurfNum)%CosTilt)

  Perimeter=Distance(SurfaceTmp(SurfNum)%Vertex(SurfaceTmp(SurfNum)%Sides),SurfaceTmp(SurfNum)%Vertex(1))
  DO Vrt=2,SurfaceTmp(SurfNum)%Sides
    Perimeter = Perimeter+Distance(SurfaceTmp(SurfNum)%Vertex(Vrt),SurfaceTmp(SurfNum)%Vertex(Vrt-1))
  ENDDO
  SurfaceTmp(SurfNum)%Perimeter=Perimeter

  ! Call to transform vertices

  Call TransformVertsByAspect(SurfNum,SurfaceTmp(SurfNum)%Sides)

  RETURN

END SUBROUTINE MakeRectangularVertices

SUBROUTINE GetHTSubSurfaceData(ErrorsFound,SurfNum,TotHTSubs,SubSurfCls,SubSurfIDs,AddedSubSurfaces,NeedToAddSurfaces)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2000
          !       MODIFIED       August 2012 - line up subsurfaces with base surface types
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the HeatTransfer Sub Surface Data,
          ! checks it for errors, etc.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
  ! Heat Transfer Subsurface Definition
  ! FenestrationSurface:Detailed,
  !        \min-fields 19
  !        \memo Used for windows, doors, glass doors, tubular daylighting devices
  !        \format vertices
  !   A1 , \field Name
  !        \required-field
  !        \type alpha
  !   A2 , \field Surface Type
  !        \required-field
  !        \type choice
  !        \key Window
  !        \key Door
  !        \key GlassDoor
  !        \key TubularDaylightDome
  !        \key TubularDaylightDiffuser
  !   A3 , \field Construction Name
  !        \required-field
  !        \note To be matched with a construction in this input file
  !        \type object-list
  !        \object-list ConstructionNames
  !   A4 , \field Building Surface Name
  !        \required-field
  !        \type object-list
  !        \object-list SurfaceNames
  !   A5,  \field Outside Boundary Condition Object
  !        \type object-list
  !        \object-list OutFaceEnvNames
  !        \note Non-blank only if base surface field Outside Boundary Condition is
  !        \note Surface or OtherSideCoefficients
  !        \note If Base Surface's Surface, specify name of corresponding subsurface in adjacent zone or
  !        \note specify current subsurface name for internal partition separating like zones
  !        \note If OtherSideCoefficients, specify name of SurfaceProperty:OtherSideCoefficients
  !        \note  or leave blank to inherit Base Surface's OtherSide Coefficients
  !   N1, \field View Factor to Ground
  !        \type real
  !        \note From the exterior of the surface
  !        \note Unused if one uses the "reflections" options in Solar Distribution in Building input
  !        \note unless a DaylightingDevice:Shelf or DaylightingDevice:Tubular object has been specified.
  !        \note autocalculate will automatically calculate this value from the tilt of the surface
  !        \autocalculatable
  !        \minimum 0.0
  !        \maximum 1.0
  !        \default autocalculate
  !   A6, \field Shading Control Name
  !        \note enter the name of a WindowProperty:ShadingControl object
  !        \type object-list
  !        \object-list WindowShadeControlNames
  !        \note used for windows and glass doors only
  !        \note If not specified, window or glass door has no shading (blind, roller shade, etc.)
  !   A7, \field Frame and Divider Name
  !        \note Enter the name of a WindowProperty:FrameAndDivider object
  !        \type object-list
  !        \object-list WindowFrameAndDividerNames
  !        \note Used only for exterior windows (rectangular) and glass doors.
  !        \note Unused for triangular windows.
  !        \note If not specified (blank), window or glass door has no frame or divider
  !        \note and no beam solar reflection from reveal surfaces.
  !   N2 , \field Multiplier
  !        \note Used only for Surface Type = WINDOW, GLASSDOOR or DOOR
  !        \note Non-integer values will be truncated to integer
  !        \default 1.0
  !        \minimum 1.0
  !   N3 , \field Number of Vertices
  !        \minimum 3
  !        \maximum 4
  !        \autocalculatable
  !        \default autocalculate
  !        \note vertices are given in GlobalGeometryRules coordinates -- if relative, all surface coordinates
  !        \note are "relative" to the Zone Origin.  If world, then building and zone origins are used
  !        \note for some internal calculations, but all coordinates are given in an "absolute" system.
  !  N4-15 as indicated by the N3 value

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList, VerifyName, GetObjectDefMaxArgs
  USE General, ONLY: TrimSigDigits,RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT)         :: ErrorsFound       ! Error flag indicator (true if errors found)
  INTEGER, INTENT(INOUT)         :: SurfNum           ! Count of Current SurfaceNumber
  INTEGER, INTENT(IN)            :: TotHTSubs         ! Number of Heat Transfer SubSurfaces to obtain
  CHARACTER(len=*), DIMENSION(:), INTENT(IN) :: SubSurfCls   ! Valid Classes for Sub Surfaces
  INTEGER, DIMENSION(:), INTENT(IN) :: SubSurfIDs     ! ID Assignments for valid sub surface classes
  INTEGER, INTENT(INOUT)         :: AddedSubSurfaces  ! Subsurfaces added when windows reference Window5
                                                      !  data file entry with two glazing systems
  INTEGER, INTENT(OUT)           :: NeedToAddSurfaces ! Number of surfaces to add, based on unentered IZ surfaces

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: IOStat            ! IO Status when calling get input subroutine
  INTEGER :: SurfaceNumAlpha   ! Number of material alpha names being passed
  INTEGER :: SurfaceNumProp    ! Number of material properties being passed
  INTEGER :: Found             ! For matching interzone surfaces
  INTEGER :: Loop
  LOGICAL :: ErrorInName
  LOGICAL :: IsBlank
  INTEGER :: ValidChk
  INTEGER :: numSides

  CALL GetWindowShadingControlData(ErrorsFound)

  cCurrentModuleObject='FenestrationSurface:Detailed'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,Loop,SurfaceNumAlpha,SurfaceNumProp)

  IF (SurfaceNumAlpha /= 7) THEN
    CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Object Definition indicates '// &
                         'not = 7 Alpha Objects, Number Indicated='//  &
                         TRIM(TrimSigDigits(SurfaceNumAlpha)))
    ErrorsFound=.true.
  ENDIF

  IF (SurfaceNumProp /= 15) THEN
    CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Object Definition indicates '// &
                         '> 15 Numeric Objects, Number Indicated='//  &
                         TRIM(TrimSigDigits(SurfaceNumAlpha)))
    ErrorsFound=.true.
  ENDIF
  NeedToAddSurfaces=0

  DO Loop=1,TotHTSubs
    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,SurfaceNumAlpha,rNumericArgs,SurfaceNumProp,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    ErrorInName=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),SurfaceTmp%Name,SurfNum,ErrorInName,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (ErrorInName) THEN
      CALL ShowContinueError('...each surface name must not duplicate other surface names (of any type)')
      ErrorsFound=.true.
      CYCLE
    ENDIF

    IF (SurfaceNumProp < 12) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                      '", Too few number of numeric args=['//TRIM(TrimSigDigits(SurfaceNumProp))//'].')
      ErrorsFound=.true.
    ENDIF

    SurfNum=SurfNum+1
    SurfaceTmp(SurfNum)%Name = cAlphaArgs(1)  ! Set the Surface Name in the Derived Type
    ValidChk=FindItemInList(cAlphaArgs(2),SubSurfCls,6)
    IF (ValidChk == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                             '", invalid '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2)))
      ErrorsFound=.true.
    ELSE
      SurfaceTmp(SurfNum)%Class = SubSurfIDs(ValidChk) ! Set class number
    ENDIF

    SurfaceTmp(SurfNum)%Construction=FindIteminList(cAlphaArgs(3),Construct%Name,TotConstructs)

    IF(SurfaceTmp(SurfNum)%Construction == 0) THEN
      ErrorsFound = .true.
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                             '", invalid '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
    ELSE
      Construct(SurfaceTmp(SurfNum)%Construction)%IsUsed=.true.
      SurfaceTmp(SurfNum)%ConstructionStoredInputValue  = SurfaceTmp(SurfNum)%Construction
    END IF

    IF(SurfaceTmp(SurfNum)%Class.EQ.SurfaceClass_Window.OR.SurfaceTmp(SurfNum)%Class.EQ.SurfaceClass_GlassDoor &
      .OR.SurfaceTmp(SurfNum)%Class.EQ.SurfaceClass_TDD_Diffuser.OR.SurfaceTmp(SurfNum)%Class.EQ.SurfaceClass_TDD_Dome) THEN

      IF (SurfaceTmp(SurfNum)%Construction /= 0) THEN
        IF (.NOT.Construct(SurfaceTmp(SurfNum)%Construction)%TypeIsWindow) THEN
          ErrorsFound = .true.
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                 '" has an opaque surface construction; it should have a window construction.')
        ENDIF
      ENDIF

    ELSEIF (SurfaceTmp(SurfNum)%Construction /= 0) THEN
      IF (Construct(SurfaceTmp(SurfNum)%Construction)%TypeIsWindow) THEN
        ErrorsFound = .true.
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                               '", invalid '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//  &
                               '" - has Window materials.')
        CALL ShowContinueError('...because '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2)))
      ENDIF
    ENDIF

    SurfaceTmp(SurfNum)%HeatTransSurf=.true.

    SurfaceTmp(SurfNum)%BaseSurfName=cAlphaArgs(4)
    !  The subsurface inherits properties from the base surface
    !  Exterior conditions, Zone, etc.
    !  We can figure out the base surface though, because they've all been entered
    Found=FindIteminList(SurfaceTmp(SurfNum)%BaseSurfName,SurfaceTmp%Name,TotSurfaces)
    IF (Found > 0) THEN
      SurfaceTmp(SurfNum)%BaseSurf=Found
      SurfaceTmp(SurfNum)%ExtBoundCond=SurfaceTmp(Found)%ExtBoundCond
      SurfaceTmp(SurfNum)%ExtBoundCondName=SurfaceTmp(Found)%ExtBoundCondName
      SurfaceTmp(SurfNum)%ExtSolar=SurfaceTmp(Found)%ExtSolar
      SurfaceTmp(SurfNum)%ExtWind=SurfaceTmp(Found)%ExtWind
      SurfaceTmp(SurfNum)%Zone=SurfaceTmp(Found)%Zone
      SurfaceTmp(SurfNum)%ZoneName=SurfaceTmp(Found)%ZoneName
      SurfaceTmp(SurfNum)%OSCPtr=SurfaceTmp(Found)%OSCPtr
      IF (SurfaceTmp(Found)%ExtBoundCond == UnreconciledZoneSurface .and.   &
          SurfaceTmp(Found)%ExtBoundCondName == SurfaceTmp(Found)%Name) THEN   ! Adiabatic surface, no windows or doors allowed
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                               '", invalid '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
        CALL ShowContinueError('... adiabatic surfaces cannot have windows or doors.')
        CALL ShowContinueError('... no solar transmission will result for these windows or doors. '//  &
          'You must have interior windows or doors on Interzone surfaces for transmission to result.')
      ENDIF
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                             '", invalid '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4)))
      SurfaceTmp(SurfNum)%ZoneName='Unknown Zone'
      ErrorsFound=.true.
    ENDIF

    IF (SurfaceTmp(SurfNum)%Class.EQ.SurfaceClass_TDD_Dome.OR.SurfaceTmp(SurfNum)%Class.EQ.SurfaceClass_TDD_Diffuser) THEN
      SurfaceTmp(SurfNum)%ExtBoundCond = ExternalEnvironment
    END IF

    IF (SurfaceTmp(SurfNum)%ExtBoundCond == ExternalEnvironment) THEN
      IF (.not. lAlphaFieldBlanks(5)) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                             '", invalid field '//TRIM(cAlphaFieldNames(5)))
        CALL ShowContinueError('...when Base surface uses "Outdoors" as '//trim(cAlphaFieldNames(5))//  &
           ', subsurfaces need to be blank to inherit the outdoor characteristics.')
        CALL ShowContinueError('...Surface external characteristics changed to reflect base surface.')
      ENDIF
    ENDIF

    IF (SurfaceTmp(SurfNum)%ExtBoundCond == UnreconciledZoneSurface) THEN ! "Surface" Base Surface
      IF (.not. lAlphaFieldBlanks(5)) THEN
        SurfaceTmp(SurfNum)%ExtBoundCondName=cAlphaArgs(5)
      ELSE
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                             '", invalid blank '//TRIM(cAlphaFieldNames(5)))
        CALL ShowContinueError('...when Base surface uses "Surface" as '//trim(cAlphaFieldNames(5))//  &
           ', subsurfaces must also specify specific surfaces in the adjacent zone.')
        SurfaceTmp(SurfNum)%ExtBoundCondName=cAlphaArgs(5)  ! putting it as blank will not confuse things later.
        ErrorsFound=.true.
      ENDIF
    ENDIF

    IF (SurfaceTmp(SurfNum)%ExtBoundCond == UnenteredAdjacentZoneSurface) THEN ! "Zone" - unmatched interior surface
      NeedToAddSurfaces=NeedToAddSurfaces+1
      ! ignoring window5datafiles for now -- will need to add.
    ENDIF

    IF (SurfaceTmp(SurfNum)%ExtBoundCond == OtherSideCoefNoCalcExt .or.   &
        SurfaceTmp(SurfNum)%ExtBoundCond == OtherSideCoefCalcExt) THEN
        IF (.not. lAlphaFieldBlanks(5)) THEN  ! Otherside Coef special Name
          Found=FindItemInList(cAlphaArgs(5),OSC%Name,TotOSC)
          IF (Found == 0) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                          '", invalid '//TRIM(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'".')
            CALL ShowContinueError('...base surface requires that this subsurface have OtherSideCoefficients -- not found.')
            ErrorsFound=.true.
          ELSE  ! found
            ! The following allows for a subsurface that has different characteristics than
            ! the base surface with OtherSide Coeff -- do we want that or is it an error?
            SurfaceTmp(SurfNum)%OSCPtr=Found
            SurfaceTmp(SurfNum)%ExtBoundCondName=cAlphaArgs(5)
            IF (OSC(Found)%SurfFilmCoef > 0.0d0) THEN
              SurfaceTmp(SurfNum)%ExtBoundCond=OtherSideCoefCalcExt
            ELSE
              SurfaceTmp(SurfNum)%ExtBoundCond = OtherSideCoefNoCalcExt
            ENDIF
          ENDIF
        ENDIF
    ENDIF

    IF (SurfaceTmp(SurfNum)%ExtBoundCond == OtherSideCondModeledExt) THEN
        SurfaceTmp(SurfNum)%ExtBoundCond = ExternalEnvironment
    ENDIF

    IF (SurfaceTmp(SurfNum)%ExtBoundCondName == Blank) THEN
      SurfaceTmp(SurfNum)%ExtBoundCondName=SurfaceTmp(SurfNum)%Name
    ENDIF
    SurfaceTmp(SurfNum)%ViewFactorGround = rNumericArgs(1)
    IF (lNumericFieldBlanks(1)) SurfaceTmp(SurfNum)%ViewFactorGround = AutoCalculate

    IF (lNumericFieldBlanks(3) .or. rNumericArgs(3) == AutoCalculate) THEN
      rNumericArgs(3)=(SurfaceNumProp-3)/3
      SurfaceTmp(SurfNum)%Sides=rNumericArgs(3)
      IF (MOD(SurfaceNumProp-3,3) /= 0) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                             '", '//TRIM(cNumericFieldNames(3))//          &
                              ' not even multiple of 3. Will read in '//   &
                              TRIM(TrimSigDigits(SurfaceTmp(SurfNum)%Sides)))
      ENDIF
      IF (rNumericArgs(3) < 3) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                             '", '//TRIM(cNumericFieldNames(3))//' (autocalculate) must be >= 3. Only '//  &
                             TRIM(TrimSigDigits(SurfaceTmp(SurfNum)%Sides))//' provided.')
        ErrorsFound=.true.
        CYCLE
      ENDIF
    ELSE
      numSides=(SurfaceNumProp-2)/3
      SurfaceTmp(SurfNum)%Sides=rNumericArgs(3)
      IF (numSides > SurfaceTmp(SurfNum)%Sides) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                  '", field '//TRIM(cNumericFieldNames(3))//'='//TRIM(TrimSigDigits(SurfaceTmp(SurfNum)%Sides)))
        CALL ShowContinueError('...but '//TRIM(TrimSigDigits(numSides))//' were entered. Only the indicated '//  &
                        TRIM(cNumericFieldNames(3))//' will be used.')
      ENDIF
    ENDIF
    ALLOCATE(SurfaceTmp(SurfNum)%Vertex(SurfaceTmp(SurfNum)%Sides))
    IF(SurfaceTmp(SurfNum)%Class.EQ.SurfaceClass_Window .or. SurfaceTmp(SurfNum)%Class.EQ.SurfaceClass_GlassDoor .or.  &
       SurfaceTmp(SurfNum)%Class.EQ.SurfaceClass_Door) &
      SurfaceTmp(SurfNum)%Multiplier = INT(rNumericArgs(2))
    ! Only windows, glass doors and doors can have Multiplier > 1:
    IF ( (SurfaceTmp(SurfNum)%Class .NE. SurfaceClass_Window .and. SurfaceTmp(SurfNum)%Class .ne. SurfaceClass_GlassDoor .and.  &
          SurfaceTmp(SurfNum)%Class .NE. SurfaceClass_Door) .AND. rNumericArgs(2) > 1.0d0) THEN
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                          '", invalid '//TRIM(cNumericFieldNames(2))//'=['//TRIM(TrimSigDigits(rNumericArgs(2),1))//'].')
      CALL ShowContinueError('...because '//TRIM(cAlphaFieldNames(2))//'='//TRIM(cAlphaArgs(2))//   &
      ' multiplier will be set to 1.0.')
      SurfaceTmp(SurfNum)%Multiplier = 1.0d0
    END IF

    CALL GetVertices(SurfNum,SurfaceTmp(SurfNum)%Sides,rNumericArgs(4:))

    CALL CheckConvexity(SurfNum,SurfaceTmp(SurfNum)%Sides)
    SurfaceTmp(SurfNum)%WindowShadingControlPtr = 0

    IF(SurfaceTmp(SurfNum)%Class.EQ.SurfaceClass_Window.OR.SurfaceTmp(SurfNum)%Class.EQ.SurfaceClass_GlassDoor &
      .OR.SurfaceTmp(SurfNum)%Class.EQ.SurfaceClass_TDD_Diffuser.OR.SurfaceTmp(SurfNum)%Class.EQ.SurfaceClass_TDD_Dome) THEN

      IF (SurfaceTmp(SurfNum)%ExtBoundCond == OtherSideCoefNoCalcExt .or.   &
          SurfaceTmp(SurfNum)%ExtBoundCond == OtherSideCoefCalcExt) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
            '", Other side coefficients are not allowed with windows.')
        ErrorsFound=.true.
      END IF

      IF (SurfaceTmp(SurfNum)%ExtBoundCond == Ground) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
            '", Exterior boundary condition = Ground is not be allowed with windows.')
        ErrorsFound=.true.
      END IF

      IF (cAlphaArgs(6) /= Blank) THEN
        IF (TotWinShadingControl > 0) THEN
          SurfaceTmp(SurfNum)%WindowShadingControlPtr = &
            FindIteminList(cAlphaArgs(6),WindowShadingControl%Name,TotWinShadingControl)
        ENDIF
        IF(SurfaceTmp(SurfNum)%WindowShadingControlPtr == 0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                           '", invalid '//TRIM(cAlphaFieldNames(6))//'="'//TRIM(cAlphaArgs(6))//'".')
          ErrorsFound=.true.
        END IF


        ! Error if this is not an exterior window and shading device has been specified
        ! PETER: should doors be disallowed too?
        IF (SurfaceTmp(SurfNum)%WindowShadingControlPtr > 0 .AND. &
            SurfaceTmp(SurfNum)%ExtBoundCond /= ExternalEnvironment) THEN

          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                           '", invalid '//TRIM(cAlphaFieldNames(6))//' because it is not an exterior window.')
          ErrorsFound=.true.

        ELSEIF (Construct(SurfaceTmp(SurfNum)%Construction)%WindowTypeEQL .AND. &
                SurfaceTmp(SurfNum)%WindowShadingControlPtr > 0) THEN

          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                           '", invalid '//TRIM(cAlphaFieldNames(6))//'="'//TRIM(cAlphaArgs(6))//'".')
          CALL ShowContinueError('.. equivalent layer window model does not use shading control object.')
          CALL ShowContinueError('.. Shading control is set to none or zero, and simulation continues.')
          SurfaceTmp(SurfNum)%WindowShadingControlPtr = 0
        END IF
      END IF

      CALL CheckWindowShadingControlFrameDivider('GetHTSubSurfaceData',ErrorsFound,SurfNum,7)

      IF(SurfaceTmp(SurfNum)%Sides == 3) THEN  ! Triangular window
        IF(cAlphaArgs(7) /= Blank) THEN
          CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                           '", invalid '//TRIM(cAlphaFieldNames(7))//'="'//TRIM(cAlphaArgs(7))//'".')
          CALL ShowContinueError('.. because it is a triangular window and cannot have a frame or divider or reveal reflection.')
          CALL ShowContinueError('Frame, divider and reveal reflection will be ignored for this window.')
        END IF
        SurfaceTmp(SurfNum)%FrameDivider = 0
      END IF  ! End of check if window is triangular or rectangular

    END IF  ! check on non-opaquedoor subsurfaces

    CALL CheckSubSurfaceMiscellaneous('GetHTSubSurfaceData',ErrorsFound,SurfNum,cAlphaArgs(1),cAlphaArgs(3),AddedSubSurfaces)

  ENDDO  ! End of main loop over subsurfaces

  RETURN

END SUBROUTINE GetHTSubSurfaceData

SUBROUTINE GetRectSubSurfaces(ErrorsFound,SurfNum,TotWindows,TotDoors,TotGlazedDoors, &
                               TotIZWindows,TotIZDoors,TotIZGlazedDoors, &
                               SubSurfIDs,AddedSubSurfaces,NeedToAddSubSurfaces)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   December 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Get simple (rectangular, relative origin to base surface) windows, doors, glazed doors.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList, VerifyName, GetObjectDefMaxArgs
  USE General, ONLY: TrimSigDigits,RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT)         :: ErrorsFound       ! Error flag indicator (true if errors found)
  INTEGER, INTENT(INOUT)         :: SurfNum           ! Count of Current SurfaceNumber
  INTEGER, INTENT(IN)            :: TotWindows        ! Number of Window SubSurfaces to obtain
  INTEGER, INTENT(IN)            :: TotDoors          ! Number of Door SubSurfaces to obtain
  INTEGER, INTENT(IN)            :: TotGlazedDoors    ! Number of Glass Door SubSurfaces to obtain
  INTEGER, INTENT(IN)            :: TotIZWindows      ! Number of Interzone Window SubSurfaces to obtain
  INTEGER, INTENT(IN)            :: TotIZDoors        ! Number of Interzone Door SubSurfaces to obtain
  INTEGER, INTENT(IN)            :: TotIZGlazedDoors  ! Number of Interzone Glass Door SubSurfaces to obtain
  INTEGER, DIMENSION(:), INTENT(IN) :: SubSurfIDs     ! ID Assignments for valid sub surface classes
  INTEGER, INTENT(INOUT)         :: AddedSubSurfaces  ! Subsurfaces added when windows reference Window5
                                                      !  data file entry with two glazing systems
  INTEGER, INTENT(INOUT)         :: NeedToAddSubSurfaces ! Number of surfaces to add, based on unentered IZ surfaces

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER, DIMENSION(6) :: cModuleObjects=  &
    (/'Window              ',  &
      'Door                ',  &
      'GlazedDoor          ',  &
      'Window:Interzone    ',  &
      'Door:Interzone      ',  &
      'GlazedDoor:Interzone'/)

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Item
  INTEGER :: ItemsToGet
  INTEGER :: Loop
  INTEGER :: NumAlphas
  INTEGER :: NumNumbers
  INTEGER :: IOStat            ! IO Status when calling get input subroutine
  INTEGER :: Found             ! For matching base surfaces
  LOGICAL :: ErrorInName
  LOGICAL :: IsBlank
  LOGICAL :: GettingIZSurfaces
  INTEGER :: WindowShadingField
  INTEGER :: FrameField
  INTEGER :: OtherSurfaceField
  INTEGER :: ClassItem
  INTEGER :: IZFound

  DO Item=1,6

    cCurrentModuleObject=cModuleObjects(Item)
    IF (Item == 1) THEN
      ItemsToGet=TotWindows
      GettingIZSurfaces=.false.
      WindowShadingField=4
      FrameField=5
      OtherSurfaceField=0
      ClassItem=1
    ELSEIF (Item == 2) THEN
      ItemsToGet=TotDoors
      GettingIZSurfaces=.false.
      WindowShadingField=0
      FrameField=0
      OtherSurfaceField=0
      ClassItem=2
    ELSEIF (Item == 3) THEN
      ItemsToGet=TotGlazedDoors
      GettingIZSurfaces=.false.
      WindowShadingField=4
      FrameField=5
      OtherSurfaceField=0
      ClassItem=3
    ELSEIF (Item == 4) THEN
      ItemsToGet=TotIZWindows
      GettingIZSurfaces=.true.
      WindowShadingField=0
      FrameField=0
      OtherSurfaceField=4
      ClassItem=1
    ELSEIF (Item == 5) THEN
      ItemsToGet=TotIZDoors
      GettingIZSurfaces=.true.
      WindowShadingField=0
      FrameField=0
      OtherSurfaceField=4
      ClassItem=2
    ELSE  ! Item = 6
      ItemsToGet=TotIZGlazedDoors
      GettingIZSurfaces=.true.
      WindowShadingField=0
      FrameField=0
      OtherSurfaceField=4
      ClassItem=3
    ENDIF

    DO Loop=1,ItemsToGet
      CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOSTAT,  &
                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      ErrorInName=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),SurfaceTmp%Name,SurfNum,ErrorInName,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (ErrorInName) THEN
        CALL ShowContinueError('...each surface name must not duplicate other surface names (of any type)')
        ErrorsFound=.true.
        CYCLE
      ENDIF

      IF (NumNumbers < 5) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                        '", Too few number of numeric args=['//TRIM(TrimSigDigits(NumNumbers))//'].')
        ErrorsFound=.true.
      ENDIF

      SurfNum=SurfNum+1
      SurfaceTmp(SurfNum)%Name = cAlphaArgs(1)  ! Set the Surface Name in the Derived Type
      SurfaceTmp(SurfNum)%Class = SubSurfIDs(ClassItem) ! Set class number

      SurfaceTmp(SurfNum)%Construction=FindIteminList(cAlphaArgs(2),Construct%Name,TotConstructs)

      IF(SurfaceTmp(SurfNum)%Construction == 0) THEN
        ErrorsFound = .true.
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                               '", invalid '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
      ELSE
        Construct(SurfaceTmp(SurfNum)%Construction)%IsUsed=.true.
        SurfaceTmp(SurfNum)%ConstructionStoredInputValue  = SurfaceTmp(SurfNum)%Construction
      END IF

      IF(SurfaceTmp(SurfNum)%Class.EQ.SurfaceClass_Window.OR.SurfaceTmp(SurfNum)%Class.EQ.SurfaceClass_GlassDoor) THEN

        IF (SurfaceTmp(SurfNum)%Construction /= 0) THEN
          IF (.NOT.Construct(SurfaceTmp(SurfNum)%Construction)%TypeIsWindow) THEN
            ErrorsFound = .true.
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                   '" has an opaque surface construction; it should have a window construction.')
          ENDIF
        ENDIF

      ELSEIF (SurfaceTmp(SurfNum)%Construction /= 0) THEN
        IF (Construct(SurfaceTmp(SurfNum)%Construction)%TypeIsWindow) THEN
          ErrorsFound = .true.
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                                 '", invalid '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//  &
                                 '" - has Window materials.')
        ENDIF
      ENDIF

      SurfaceTmp(SurfNum)%HeatTransSurf=.true.

      SurfaceTmp(SurfNum)%BaseSurfName=cAlphaArgs(3)
      !  The subsurface inherits properties from the base surface
      !  Exterior conditions, Zone, etc.
      !  We can figure out the base surface though, because they've all been entered
      Found=FindIteminList(SurfaceTmp(SurfNum)%BaseSurfName,SurfaceTmp%Name,TotSurfaces)
      IF (Found > 0) THEN
        SurfaceTmp(SurfNum)%BaseSurf=Found
        SurfaceTmp(SurfNum)%ExtBoundCond=SurfaceTmp(Found)%ExtBoundCond
        SurfaceTmp(SurfNum)%ExtBoundCondName=SurfaceTmp(Found)%ExtBoundCondName
        SurfaceTmp(SurfNum)%ExtSolar=SurfaceTmp(Found)%ExtSolar
        SurfaceTmp(SurfNum)%ExtWind=SurfaceTmp(Found)%ExtWind
        SurfaceTmp(SurfNum)%Tilt=SurfaceTmp(Found)%Tilt
        SurfaceTmp(SurfNum)%Azimuth=SurfaceTmp(Found)%Azimuth
        SurfaceTmp(SurfNum)%Zone=SurfaceTmp(Found)%Zone
        SurfaceTmp(SurfNum)%ZoneName=SurfaceTmp(Found)%ZoneName
        SurfaceTmp(SurfNum)%OSCPtr=SurfaceTmp(Found)%OSCPtr
        SurfaceTmp(SurfNum)%ViewFactorGround = SurfaceTmp(Found)%ViewFactorGround
        SurfaceTmp(SurfNum)%ViewFactorSky = SurfaceTmp(Found)%ViewFactorSky
      ELSE
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                               '", invalid '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3)))
        SurfaceTmp(SurfNum)%ZoneName='Unknown Zone'
        ErrorsFound=.true.
        CYCLE
      ENDIF
      IF (SurfaceTmp(Found)%ExtBoundCond == UnreconciledZoneSurface .and.  &
          SurfaceTmp(Found)%ExtBoundCondName == SurfaceTmp(Found)%Name) THEN   ! Adiabatic surface, no windows or doors allowed
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                               '", invalid '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
        CALL ShowContinueError('... adiabatic surfaces cannot have windows or doors.')
        CALL ShowContinueError('... no solar transmission will result for these windows or doors. '//  &
          'You must have interior windows or doors on Interzone surfaces for transmission to result.')
      ENDIF

      IF (SurfaceTmp(SurfNum)%ExtBoundCond == UnreconciledZoneSurface) THEN ! "Surface" Base Surface
        IF (.not. GettingIZSurfaces) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                               '", invalid use of object')
          CALL ShowContinueError('...when Base surface uses "Surface" as '//trim(cAlphaFieldNames(5))//  &
             ', subsurfaces must also specify specific surfaces in the adjacent zone.')
          CALL ShowContinueError('...Please use '//trim(cCurrentModuleObject)//':Interzone to enter this surface.')
          SurfaceTmp(SurfNum)%ExtBoundCondName=blank  ! putting it as blank will not confuse things later.
          ErrorsFound=.true.
        ENDIF
      ENDIF

      IF (SurfaceTmp(SurfNum)%ExtBoundCond == UnreconciledZoneSurface) THEN ! "Surface" Base Surface
        IF (GettingIZSurfaces) THEN
          SurfaceTmp(SurfNum)%ExtBoundCondName=cAlphaArgs(OtherSurfaceField)
          IZFound=FindItemInList(SurfaceTmp(SurfNum)%ExtBoundCondName,Zone%Name,NumOfZones)
          IF (IZFound > 0) SurfaceTmp(SurfNum)%ExtBoundCond=UnenteredAdjacentZoneSurface
        ELSE  ! Interior Window
          SurfaceTmp(SurfNum)%ExtBoundCondName=SurfaceTmp(SurfNum)%Name
        ENDIF
      ENDIF

      ! This is the parent's property:
      IF (SurfaceTmp(SurfNum)%ExtBoundCond == UnenteredAdjacentZoneSurface) THEN ! OtherZone - unmatched interior surface
        IF (GettingIZSurfaces) THEN
          NeedToAddSubSurfaces=NeedToAddSubSurfaces+1
        ELSE  ! Interior Window
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                               '", invalid Interzone Surface, specify '//trim(cCurrentModuleObject)//':InterZone')
          CALL ShowContinueError('...when base surface is an interzone surface, subsurface must also be an interzone surface.')
          NeedToAddSubSurfaces=NeedToAddSubSurfaces+1
          ErrorsFound=.true.
        ENDIF
      ENDIF

      IF (GettingIZSurfaces) THEN
        IF (lAlphaFieldBlanks(OtherSurfaceField)) THEN
          ! blank -- set it up for unentered adjacent zone
          IF (SurfaceTmp(SurfNum)%ExtBoundCond == UnenteredAdjacentZoneSurface) THEN  ! already set but need Zone
            SurfaceTmp(SurfNum)%ExtBoundCondName=SurfaceTmp(Found)%ExtBoundCondName  ! base surface has it
          ELSEIF (SurfaceTmp(SurfNum)%ExtBoundCond == UnreconciledZoneSurface) THEN
            SurfaceTmp(SurfNum)%ExtBoundCondName=SurfaceTmp(Found)%ZoneName  ! base surface has it
            SurfaceTmp(SurfNum)%ExtBoundCond=UnenteredAdjacentZoneSurface
          ELSE  ! not correct boundary condition for interzone subsurface
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                                 '", invalid Base Surface type for Interzone Surface')
            CALL ShowContinueError('...when base surface is not an interzone surface, '//  &
               'subsurface must also not be an interzone surface.')
            ErrorsFound=.true.
          ENDIF
        ENDIF
      ENDIF

      IF (SurfaceTmp(SurfNum)%ExtBoundCond == OtherSideCondModeledExt) THEN
          SurfaceTmp(SurfNum)%ExtBoundCond = ExternalEnvironment
      ENDIF

!      SurfaceTmp(SurfNum)%ViewFactorGround = AutoCalculate

      SurfaceTmp(SurfNum)%Sides=4
      ALLOCATE(SurfaceTmp(SurfNum)%Vertex(SurfaceTmp(SurfNum)%Sides))
      IF(SurfaceTmp(SurfNum)%Class.EQ.SurfaceClass_Window .or. SurfaceTmp(SurfNum)%Class.EQ.SurfaceClass_GlassDoor .or.  &
         SurfaceTmp(SurfNum)%Class.EQ.SurfaceClass_Door) &
        SurfaceTmp(SurfNum)%Multiplier = INT(rNumericArgs(1))
      ! Only windows, glass doors and doors can have Multiplier > 1:
      IF ( (SurfaceTmp(SurfNum)%Class .NE. SurfaceClass_Window .and. SurfaceTmp(SurfNum)%Class .ne. SurfaceClass_GlassDoor .and.  &
            SurfaceTmp(SurfNum)%Class .NE. SurfaceClass_Door) .AND. rNumericArgs(1) > 1.0d0) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                            '", invalid '//TRIM(cNumericFieldNames(1))//'=['//TRIM(TrimSigDigits(rNumericArgs(1),1))//'].')
        CALL ShowContinueError('...because '//TRIM(cAlphaFieldNames(1))//'='//TRIM(cAlphaArgs(1))//   &
        ' multiplier will be set to 1.0.')
        SurfaceTmp(SurfNum)%Multiplier = 1.0d0
      END IF

      CALL MakeRelativeRectangularVertices(SurfaceTmp(SurfNum)%BaseSurf,SurfNum,  &
                         rNumericArgs(2),rNumericArgs(3),rNumericArgs(4),rNumericArgs(5))

      IF (SurfaceTmp(SurfNum)%Area <= 0.0d0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
              '", Surface Area <= 0.0; Entered Area='//  &
              TRIM(TrimSigDigits(SurfaceTmp(SurfNum)%Area,2)))
        ErrorsFound=.true.
      ENDIF

      SurfaceTmp(SurfNum)%WindowShadingControlPtr = 0

      IF(.not. GettingIZSurfaces .and. &
          (SurfaceTmp(SurfNum)%Class.EQ.SurfaceClass_Window.OR.SurfaceTmp(SurfNum)%Class.EQ.SurfaceClass_GlassDoor)) THEN

        IF (SurfaceTmp(SurfNum)%ExtBoundCond == OtherSideCoefNoCalcExt .or.   &
            SurfaceTmp(SurfNum)%ExtBoundCond == OtherSideCoefCalcExt) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
              '", Other side coefficients are not allowed with windows.')
          ErrorsFound=.true.
        END IF

        IF (SurfaceTmp(SurfNum)%ExtBoundCond == Ground) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
              '", Exterior boundary condition = Ground is not be allowed with windows.')
          ErrorsFound=.true.
        END IF

        IF (cAlphaArgs(WindowShadingField) /= Blank) THEN
          IF (TotWinShadingControl > 0) THEN
            SurfaceTmp(SurfNum)%WindowShadingControlPtr = &
              FindIteminList(cAlphaArgs(WindowShadingField),WindowShadingControl%Name,TotWinShadingControl)
          ENDIF
          IF(SurfaceTmp(SurfNum)%WindowShadingControlPtr == 0) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                   '", invalid '//TRIM(cAlphaFieldNames(WindowShadingField))//'="'//TRIM(cAlphaArgs(WindowShadingField))//'".')
            ErrorsFound=.true.
          END IF


          ! Error if this is not an exterior window and shading device has been specified
          ! PETER: should doors be disallowed too?
          IF (SurfaceTmp(SurfNum)%WindowShadingControlPtr > 0 .AND. &
              SurfaceTmp(SurfNum)%ExtBoundCond /= ExternalEnvironment) THEN

            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                   '", invalid '//TRIM(cAlphaFieldNames(WindowShadingField))//' because it is not an exterior window.')
            ErrorsFound=.true.
          END IF
        END IF

        CALL CheckWindowShadingControlFrameDivider('GetRectSubSurfaces',ErrorsFound,SurfNum,FrameField)

      END IF  ! check on non-opaquedoor subsurfaces

      CALL CheckSubSurfaceMiscellaneous('GetRectSubSurfaces',ErrorsFound,SurfNum,cAlphaArgs(1),cAlphaArgs(2),AddedSubSurfaces)

    ENDDO  ! Getting Items

  ENDDO

  RETURN

END SUBROUTINE GetRectSubSurfaces

SUBROUTINE CheckWindowShadingControlFrameDivider(cRoutineName,ErrorsFound,SurfNum,FrameField)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   December 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine performs checks on WindowShadingControl settings and Frame/Divider Settings.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: TrimSigDigits,RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: cRoutineName  ! routine name calling this one (for error messages)
  LOGICAL, INTENT(INOUT) :: ErrorsFound  ! true if errors have been found or are found here
  INTEGER, INTENT(IN)    :: SurfNum      ! current surface number
  INTEGER, INTENT(IN)    :: FrameField   ! field number for frame/divider

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: WSCPtr       ! WindowShadingControl Index
  INTEGER :: ConstrNumSh  ! Construction number with Shade
  INTEGER :: ConstrNum    ! Construction number
  INTEGER :: ShDevNum     ! Shading Device number
  INTEGER :: Lay               ! Layer number
  INTEGER :: TotGlassLayers    ! Number of glass layers in window construction
  INTEGER :: TotLayers         ! Number of layers in unshaded construction
  INTEGER :: TotShLayers       ! Number of layers in shaded construction
  INTEGER :: MatGap            ! Gap material number
  INTEGER :: MatGap1           ! Material number of gap to left (outer side) of between-glass shade/blind
  INTEGER :: MatGap2           ! Material number of gap to right (inner side) of between-glass shade/blind
  INTEGER :: MatSh             ! Between-glass shade/blind material number
  REAL(r64) :: MatGapCalc      ! Calculated MatGap diff for shaded vs non-shaded constructions

  ! If WindowShadingControl has been specified for this window --
  ! Set shaded construction number if shaded construction was specified in WindowShadingControl.
  ! Otherwise, create shaded construction if WindowShadingControl for this window has
  ! interior or exterior shade/blind (but not between-glass shade/blind) specified.


  WSCptr = SurfaceTmp(SurfNum)%WindowShadingControlPtr
  ConstrNumSh=0
  IF(.NOT.ErrorsFound .AND. WSCptr > 0) THEN
    ConstrNumSh = WindowShadingControl(WSCptr)%ShadedConstruction
    IF(ConstrNumSh > 0) THEN
      SurfaceTmp(SurfNum)%ShadedConstruction = ConstrNumSh
    ELSE
      IF(WindowShadingControl(WSCptr)%ShadingType==WSC_ST_InteriorShade.OR. &
         WindowShadingControl(WSCptr)%ShadingType==WSC_ST_InteriorBlind.OR. &
         WindowShadingControl(WSCptr)%ShadingType==WSC_ST_ExteriorShade.OR. &
         WindowShadingControl(WSCptr)%ShadingType==WSC_ST_ExteriorScreen.OR. &
         WindowShadingControl(WSCptr)%ShadingType==WSC_ST_ExteriorBlind) THEN
        ShDevNum = WindowShadingControl(WSCptr)%ShadingDevice
        IF(ShDevNum > 0) THEN
          CALL CreateShadedWindowConstruction(SurfNum,WSCptr,ShDevNum)
          ConstrNumSh = SurfaceTmp(SurfNum)%ShadedConstruction
        ENDIF
      END IF
    END IF
  END IF

  ! Error checks for shades and blinds

  ConstrNum = SurfaceTmp(SurfNum)%Construction
  IF (.NOT.ErrorsFound .AND. WSCptr > 0 .AND. ConstrNum > 0 .AND. ConstrNumSh > 0) THEN

    IF(WindowShadingControl(WSCptr)%ShadingType==WSC_ST_InteriorShade.OR. &
       WindowShadingControl(WSCptr)%ShadingType==WSC_ST_InteriorBlind) THEN
      TotLayers = Construct(ConstrNum)%TotLayers
      TotShLayers = Construct(ConstrNumSh)%TotLayers
      IF (TotShLayers-1 /= TotLayers) THEN
        CALL ShowWarningError('WindowProperty:ShadingControl: Interior shade or blind: Potential problem in match of '//  &
                              'unshaded/shaded constructions, shaded should have 1 more layers than unshaded.')
        CALL ShowContinueError('Unshaded construction='//TRIM(Construct(ConstrNum)%Name))
        CALL ShowContinueError('Shaded construction='//TRIM(Construct(ConstrNumSh)%Name))
        CALL ShowContinueError('If preceding two constructions are same name, you have likely specified a '//   &
                               'WindowProperty:ShadingControl (Field #3) with the Window Construction rather than '//    &
                               'a shaded construction.')
      ENDIF
      DO Lay = 1,Construct(ConstrNum)%TotLayers
        IF(Construct(ConstrNum)%LayerPoint(Lay) /= Construct(ConstrNumSh)%LayerPoint(Lay)) THEN
          ErrorsFound = .TRUE.
          CALL ShowSevereError &
            (' The glass and gas layers in the shaded and unshaded constructions do not match for window=' &
              //TRIM(SurfaceTmp(SurfNum)%Name))
          CALL ShowContinueError('Unshaded construction='//TRIM(Construct(ConstrNum)%Name))
          CALL ShowContinueError('Shaded construction='//TRIM(Construct(ConstrNumSh)%Name))
          EXIT
        END IF
      END DO
    END IF

    IF(WindowShadingControl(WSCptr)%ShadingType==WSC_ST_ExteriorShade.OR. &
       WindowShadingControl(WSCptr)%ShadingType==WSC_ST_ExteriorScreen.OR. &
       WindowShadingControl(WSCptr)%ShadingType==WSC_ST_ExteriorBlind) THEN
      TotLayers = Construct(ConstrNum)%TotLayers
      TotShLayers = Construct(ConstrNumSh)%TotLayers
      IF (TotShLayers-1 /= TotLayers) THEN
        CALL ShowWarningError('WindowProperty:ShadingControl: Exterior shade, screen or blind: '//  &
           'Potential problem in match of unshaded/shaded constructions, shaded should have 1 more layer than unshaded.')
        CALL ShowContinueError('Unshaded construction='//TRIM(Construct(ConstrNum)%Name))
        CALL ShowContinueError('Shaded construction='//TRIM(Construct(ConstrNumSh)%Name))
        CALL ShowContinueError('If preceding two constructions have the same name, you have likely specified a '//   &
                               'WindowProperty:ShadingControl (Field #3) with the Window Construction rather than '//    &
                               'a shaded construction.')
      ENDIF
      DO Lay = 1,Construct(ConstrNum)%TotLayers
        IF(Construct(ConstrNum)%LayerPoint(Lay) /= Construct(ConstrNumSh)%LayerPoint(Lay+1)) THEN
          ErrorsFound = .TRUE.
          CALL ShowSevereError &
            (' The glass and gas layers in the shaded and unshaded constructions do not match for window=' &
              //TRIM(SurfaceTmp(SurfNum)%Name))
          CALL ShowContinueError('Unshaded construction='//TRIM(Construct(ConstrNum)%Name))
          CALL ShowContinueError('Shaded construction='//TRIM(Construct(ConstrNumSh)%Name))
          EXIT
        END IF
      END DO
    END IF

    IF(WindowShadingControl(WSCptr)%ShadingType==WSC_ST_BetweenGlassShade.OR. &
       WindowShadingControl(WSCptr)%ShadingType==WSC_ST_BetweenGlassBlind) THEN
          ! Divider not allowed with between-glass shade or blind
      IF(SurfaceTmp(SurfNum)%FrameDivider > 0) THEN
        IF(FrameDivider(SurfaceTmp(SurfNum)%FrameDivider)%DividerWidth > 0.0d0) THEN
          CALL ShowWarningError('A divider cannot be specified for window '//TRIM(SurfaceTmp(SurfNum)%Name))
          CALL ShowContinueError(', which has a between-glass shade or blind.')
          CALL ShowContinueError('Calculation will proceed without the divider for this window.')
          FrameDivider(SurfaceTmp(SurfNum)%FrameDivider)%DividerWidth = 0.0d0
        END IF
      END IF
          ! Check consistency of gap widths between unshaded and shaded constructions
      TotGlassLayers = Construct(ConstrNum)%TotGlassLayers
      TotLayers = Construct(ConstrNum)%TotLayers
      TotShLayers = Construct(ConstrNumSh)%TotLayers
      IF (TotShLayers-2 /= TotLayers) THEN
        CALL ShowWarningError('WindowProperty:ShadingControl: Between Glass Shade/Blind: Potential problem in match of '//  &
                              'unshaded/shaded constructions, shaded should have 2 more layers than unshaded.')
        CALL ShowContinueError('Unshaded construction='//TRIM(Construct(ConstrNum)%Name))
        CALL ShowContinueError('Shaded construction='//TRIM(Construct(ConstrNumSh)%Name))
        CALL ShowContinueError('If preceding two constructions are same name, you have likely specified a '//   &
                               'WindowProperty:ShadingControl (Field #3) with the Window Construction rather than '//    &
                               'a shaded construction.')
      ENDIF
      IF (Construct(ConstrNum)%LayerPoint(TotLayers) /= Construct(ConstrNumSh)%LayerPoint(TotShLayers)) THEN
        CALL ShowSevereError(trim(cRoutineName)//': Mis-match in unshaded/shaded inside layer materials.  These should match.')
        CALL ShowContinueError('Unshaded construction='//TRIM(Construct(ConstrNum)%Name)//  &
                 ', Material='//TRIM(Material(Construct(ConstrNum)%LayerPoint(TotLayers))%Name))
        CALL ShowContinueError('Shaded construction='//TRIM(Construct(ConstrNumSh)%Name)// &
                 ', Material='//TRIM(Material(Construct(ConstrNumSh)%LayerPoint(TotShLayers))%Name))
        ErrorsFound=.true.
      ENDIF
      IF (Construct(ConstrNum)%LayerPoint(1) /= Construct(ConstrNumSh)%LayerPoint(1)) THEN
        CALL ShowSevereError(trim(cRoutineName)//': Mis-match in unshaded/shaded inside layer materials.  These should match.')
        CALL ShowContinueError('Unshaded construction='//TRIM(Construct(ConstrNum)%Name)//  &
                 ', Material='//TRIM(Material(Construct(ConstrNum)%LayerPoint(1))%Name))
        CALL ShowContinueError('Shaded construction='//TRIM(Construct(ConstrNumSh)%Name)// &
                 ', Material='//TRIM(Material(Construct(ConstrNumSh)%LayerPoint(1))%Name))
        ErrorsFound=.true.
      ENDIF
      IF(TotGlassLayers == 2 .OR. TotGlassLayers == 3) THEN
        MatGap =  Construct(ConstrNum)%LayerPoint(2*TotGlassLayers-2)
        MatGap1 = Construct(ConstrNumSh)%LayerPoint(2*TotGlassLayers-2)
        MatGap2 = Construct(ConstrNumSh)%LayerPoint(2*TotGlassLayers)
        MatSh = Construct(ConstrNumSh)%LayerPoint(2*TotGlassLayers-1)
        IF(WindowShadingControl(WSCptr)%ShadingType==WSC_ST_BetweenGlassBlind) THEN
          MatGapCalc=ABS(Material(MatGap)%Thickness-(Material(MatGap1)%Thickness+Material(MatGap2)%Thickness))
          IF(MatGapCalc > 0.001d0) THEN
            CALL ShowSevereError(trim(cRoutineName)//': The gap width(s) for the unshaded window construction ' &
                  //TRIM(Construct(ConstrNum)%Name))
            CALL ShowContinueError('are inconsistent with the gap widths for shaded window construction ' &
                  //TRIM(Construct(ConstrNumSh)%Name))
            CALL ShowContinueError('for window '//TRIM(SurfaceTmp(SurfNum)%Name)//', which has a between-glass blind.')
            CALL ShowContinueError('..Material='//TRIM(Material(MatGap)%Name)//   &
                            ' thickness='//TRIM(RoundSigDigits(Material(MatGap)%Thickness,3))//' -')
            CALL ShowContinueError('..( Material='//TRIM(Material(MatGap1)%Name)//  &
                            ' thickness='//TRIM(RoundSigDigits(Material(MatGap1)%Thickness,3))//' +')
            CALL ShowContinueError('..Material='//TRIM(Material(MatGap2)%Name)//  &
                            ' thickness='//TRIM(RoundSigDigits(Material(MatGap2)%Thickness,3))//' )=['//  &
                            trim(RoundSigDigits(MatGapCalc,3))//'] >.001')
            ErrorsFound=.true.
          END IF
        ELSE  ! Between-glass shade
          MatGapCalc=ABS(Material(MatGap)%Thickness-  &
                         (Material(MatGap1)%Thickness+Material(MatGap2)%Thickness+Material(MatSh)%Thickness))
          IF(MatGapCalc > 0.001d0) THEN
            CALL ShowSevereError(trim(cRoutineName)//': The gap width(s) for the unshaded window construction ' &
                  //TRIM(Construct(ConstrNum)%Name))
            CALL ShowContinueError('are inconsistent with the gap widths for shaded window construction ' &
                  //TRIM(Construct(ConstrNumSh)%Name))
            CALL ShowContinueError('for window '//TRIM(SurfaceTmp(SurfNum)%Name)//', which has a between-glass shade.')
            CALL ShowContinueError('..Material='//TRIM(Material(MatGap)%Name)//  &
                            ' thickness='//TRIM(RoundSigDigits(Material(MatGap)%Thickness,3))//' -')
            CALL ShowContinueError('...( Material='//TRIM(Material(MatGap1)%Name)//  &
                            ' thickness='//TRIM(RoundSigDigits(Material(MatGap1)%Thickness,3))//' +')
            CALL ShowContinueError('..Material='//TRIM(Material(MatGap2)%Name)//  &
                            ' thickness='//TRIM(RoundSigDigits(Material(MatGap2)%Thickness,3))//' +')
            CALL ShowContinueError('..Material='//TRIM(Material(MatSh)%Name)//    &
                            ' thickness='//TRIM(RoundSigDigits(Material(MatSh)%Thickness,3))//' )=['//  &
                            trim(RoundSigDigits(MatGapCalc,3))//'] >.001')
            ErrorsFound=.true.
          END IF
        END IF
      END IF
    END IF
  END IF

  IF(SurfaceTmp(SurfNum)%Sides /= 3) THEN  ! Rectangular Window
    ! Initialize the FrameDivider number for this window. W5FrameDivider will be positive if
    ! this window's construction came from the Window5 data file and that construction had an
    ! associated frame or divider. It will be zero if the window's construction is not from the
    ! Window5 data file, or the construction is from the data file, but the construction has no
    ! associated frame or divider. Note that if there is a FrameDivider candidate for this
    ! window from the Window5 data file it is used instead of the window's input FrameDivider.

    IF(SurfaceTmp(SurfNum)%Construction /= 0) THEN
      SurfaceTmp(SurfNum)%FrameDivider = Construct(SurfaceTmp(SurfNum)%Construction)%W5FrameDivider

      ! Warning if FrameAndDivider for this window is over-ridden by one from Window5 Data File
      IF(SurfaceTmp(SurfNum)%FrameDivider > 0 .AND. .not. lAlphaFieldBlanks(FrameField)) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                       '", '//TRIM(cAlphaFieldNames(FrameField))//'="'//TRIM(cAlphaArgs(FrameField))//'"')
        CALL ShowContinueError('will be replaced with FrameAndDivider from Window5 Data File entry ' &
              //TRIM(Construct(SurfaceTmp(SurfNum)%Construction)%Name))
      END IF

      IF (.not. lAlphaFieldBlanks(FrameField) .AND. SurfaceTmp(SurfNum)%FrameDivider == 0) THEN
        SurfaceTmp(SurfNum)%FrameDivider = &
          FindIteminList(cAlphaArgs(FrameField),FrameDivider%Name,TotFrameDivider)
        IF(SurfaceTmp(SurfNum)%FrameDivider == 0) THEN
          IF (.NOT. Construct(SurfaceTmp(SurfNum)%Construction)%WindowTypeEQL) THEN
               CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                    '", invalid '//TRIM(cAlphaFieldNames(FrameField))//'="'//TRIM(cAlphaArgs(FrameField))//'"')
               ErrorsFound=.true.
          ELSE
               CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                   '", invalid '//TRIM(cAlphaFieldNames(FrameField))//'="'//TRIM(cAlphaArgs(FrameField))//'"')
               CALL ShowContinueError('...Frame/Divider is not supported in Equivalent Layer Window model.')
          ENDIF
        END IF
            ! Divider not allowed with between-glass shade or blind
        IF(.NOT.ErrorsFound .AND. WSCptr > 0 .AND. ConstrNumSh > 0) THEN
          IF(WindowShadingControl(WSCptr)%ShadingType==WSC_ST_BetweenGlassShade.OR. &
            WindowShadingControl(WSCptr)%ShadingType==WSC_ST_BetweenGlassBlind) THEN
            IF(SurfaceTmp(SurfNum)%FrameDivider > 0) THEN
              IF(FrameDivider(SurfaceTmp(SurfNum)%FrameDivider)%DividerWidth > 0.0d0) THEN
                CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                       '", invalid '//TRIM(cAlphaFieldNames(FrameField))//'="'//TRIM(cAlphaArgs(FrameField))//'"')
                CALL ShowContinueError('Divider cannot be specified because the construction has a between-glass shade or blind.')
                CALL ShowContinueError('Calculation will proceed without the divider for this window.')
                CALL ShowContinueError('Divider width = ['//  &
                   trim(RoundSigDigits(FrameDivider(SurfaceTmp(SurfNum)%FrameDivider)%DividerWidth,2))//'].')
                FrameDivider(SurfaceTmp(SurfNum)%FrameDivider)%DividerWidth = 0.0d0
              END IF
            END IF ! End of check if window has divider
          END IF  ! End of check if window has a between-glass shade or blind
        END IF  ! End of check if window has a shaded construction
      END IF  ! End of check if window has an associated FrameAndDivider
    END IF  ! End of check if window has a construction
  END IF

  IF (Construct(SurfaceTmp(SurfNum)%Construction)%WindowTypeEQL) THEN
     IF(SurfaceTmp(SurfNum)%FrameDivider > 0) THEN
        ! Equivalent Layer window does not have frame/divider model
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
             '", invalid '//TRIM(cAlphaFieldNames(FrameField))//'="'//TRIM(cAlphaArgs(FrameField))//'"')
        CALL ShowContinueError('Frame/Divider is not supported in Equivalent Layer Window model.')
        SurfaceTmp(SurfNum)%FrameDivider = 0
     ENDIF
  ENDIF

  RETURN

END SUBROUTINE CheckWindowShadingControlFrameDivider

SUBROUTINE CheckSubSurfaceMiscellaneous(cRoutineName,ErrorsFound,SurfNum,SubSurfaceName,SubSurfaceConstruction,AddedSubSurfaces)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   December 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine performs miscellaneous checks on subsurfaces: Windows, GlassDoors, Doors, Tubular Devices.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
!  USE DataIPShortCuts
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: TrimSigDigits,RoundSigDigits
  USE DataErrorTracking

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: cRoutineName  ! routine name calling this one (for error messages)
  LOGICAL, INTENT(INOUT) :: ErrorsFound  ! true if errors have been found or are found here
  INTEGER, INTENT(IN)    :: SurfNum      ! current surface number
  CHARACTER(len=*), INTENT(IN)    :: SubSurfaceName    !  name of the surface
  CHARACTER(len=*), INTENT(IN)    :: SubSurfaceConstruction    !  name of the construction
  INTEGER, INTENT(INOUT) :: AddedSubSurfaces

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: NumShades  ! count on number of shading layers
  INTEGER :: Lay               ! Layer number
  INTEGER :: LayerPtr          ! Layer pointer
  INTEGER :: ConstrNum    ! Construction number
  INTEGER :: Found        ! when item is found

  ! Warning if window has multiplier > 1 and SolarDistribution = FullExterior or FullInteriorExterior

  IF((SurfaceTmp(SurfNum)%Class == SurfaceClass_Window .or. SurfaceTmp(SurfNum)%Class == SurfaceClass_GlassDoor)  &
      .AND. SolarDistribution > MinimalShadowing .AND. SurfaceTmp(SurfNum)%Multiplier > 1.0d0) THEN
    IF (DisplayExtraWarnings) THEN
      CALL ShowWarningError(trim(cRoutineName)//': A Multiplier > 1.0 for window/glass door '//TRIM(SurfaceTmp(SurfNum)%Name))
      CALL ShowContinueError('in conjunction with SolarDistribution = FullExterior or FullInteriorExterior')
      CALL ShowContinueError('can cause inaccurate shadowing on the window and/or')
      CALL ShowContinueError('inaccurate interior solar distribution from the window.')
    ENDIF
    TotalMultipliedWindows=TotalMultipliedWindows+1
  END IF

  !  Require that a construction referenced by a surface that is a window
  !  NOT have a shading device layer; use WindowShadingControl to specify a shading device.
  ConstrNum=SurfaceTmp(SurfNum)%Construction
  IF(ConstrNum > 0) THEN
    NumShades = 0
    DO Lay = 1,Construct(ConstrNum)%TotLayers
      LayerPtr = Construct(ConstrNum)%LayerPoint(Lay)
      IF (LayerPtr == 0) CYCLE  ! Error is caught already, will terminate later
      IF(Material(LayerPtr)%Group==Shade .OR. Material(LayerPtr)%Group==WindowBlind .OR. &
         Material(LayerPtr)%Group==Screen) NumShades = NumShades + 1
    END DO
    IF(NumShades /= 0) THEN
      CALL ShowSevereError(trim(cRoutineName)//': Window "'//TRIM(SubSurfaceName)//'" must not directly reference')
      CALL ShowContinueError('a Construction (i.e, "'//TRIM(SubSurfaceConstruction)//'") with a shading device.')
      CALL ShowContinueError('Use WindowProperty:ShadingControl to specify a shading device for a window.')
      ErrorsFound = .true.
    END IF
  END IF

  ! Disallow glass transmittance dirt factor for interior windows and glass doors

  IF(SurfaceTmp(SurfNum)%ExtBoundCond /= ExternalEnvironment .AND. &
    (SurfaceTmp(SurfNum)%Class == SurfaceClass_Window .OR. SurfaceTmp(SurfNum)%Class == SurfaceClass_GlassDoor)) THEN
    ConstrNum = SurfaceTmp(SurfNum)%Construction
    IF (ConstrNum > 0) THEN
      DO Lay = 1,Construct(ConstrNum)%TotLayers
        LayerPtr = Construct(ConstrNum)%LayerPoint(Lay)
        IF(Material(LayerPtr)%Group == WindowGlass .AND. Material(LayerPtr)%GlassTransDirtFactor < 1.0d0) THEN
          CALL ShowSevereError(trim(cRoutineName)//': Interior Window or GlassDoor '//TRIM(SubSurfaceName)// &
            ' has a glass layer with')
          CALL ShowContinueError('Dirt Correction Factor for Solar and Visible Transmittance < 1.0')
          CALL ShowContinueError('A value less than 1.0 for this factor is only allowed for exterior windows and glass doors.')
          ErrorsFound = .true.
        END IF
      END DO
    ENDIF
  END IF

  ! If this is a window with a construction from the Window5DataFile, call routine that will
  ! (1) if one glazing system on Data File, give warning message if window height or width
  !     differ by more than 10% from those of the glazing system on the Data File;
  ! (2) if two glazing systems (separated by a mullion) on Data File, create a second window
  !     and adjust the dimensions of the original and second windows to those on the Data File

  IF (SurfaceTmp(SurfNum)%Construction /= 0) THEN

    IF(Construct(SurfaceTmp(SurfNum)%Construction)%FromWindow5DataFile) THEN

      CALL ModifyWindow(SurfNum,ErrorsFound,AddedSubSurfaces)

    ELSE
         ! Calculate net area for base surface (note that ModifyWindow, above, adjusts net area of
         ! base surface for case where window construction is from Window5 Data File
         ! In case there is in error in this window's base surface (i.e. none)..
      IF (SurfaceTmp(SurfNum)%BaseSurf > 0) THEN
        SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%Area =  &
               SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%Area - SurfaceTmp(SurfNum)%Area

        ! Subtract TDD:DIFFUSER area from other side interzone surface
        IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_TDD_Diffuser .AND. &
          SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%ExtBoundCondName /= '   ') THEN ! Base surface is an interzone surface

          ! Lookup interzone surface of the base surface
          ! (Interzone surfaces have not been assigned yet, but all base surfaces should already be loaded.)
          Found = FindIteminList(SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%ExtBoundCondName,SurfaceTmp%Name,SurfNum)
          IF (Found /= 0) SurfaceTmp(Found)%Area = SurfaceTmp(Found)%Area - SurfaceTmp(SurfNum)%Area
        END IF

        IF (SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%Area <= 0.0d0) THEN
          CALL ShowSevereError(trim(cRoutineName)//': Surface Openings have too much area for base surface='//  &
                               TRIM(SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%Name))
          CALL ShowContinueError('Opening Surface creating error='//TRIM(SurfaceTmp(SurfNum)%Name))
          ErrorsFound=.true.
        ENDIF
           ! Net area of base surface with unity window multipliers (used in shadowing checks)
           ! For Windows, Glass Doors and Doors, just one area is subtracted.  For the rest, should be
           ! full area.
        IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Window .OR.SurfaceTmp(SurfNum)%Class == SurfaceClass_GlassDoor) THEN
          SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%NetAreaShadowCalc =  &
              SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%NetAreaShadowCalc -   &
                 SurfaceTmp(SurfNum)%Area/SurfaceTmp(SurfNum)%Multiplier
        ELSEIF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Door) THEN ! Door, TDD:Diffuser, TDD:DOME
          SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%NetAreaShadowCalc =  &
            SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%NetAreaShadowCalc -   &
               SurfaceTmp(SurfNum)%Area/SurfaceTmp(SurfNum)%Multiplier
        ELSE
          SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%NetAreaShadowCalc =  &
             SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%NetAreaShadowCalc - SurfaceTmp(SurfNum)%Area
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  RETURN

END SUBROUTINE CheckSubSurfaceMiscellaneous

SUBROUTINE MakeRelativeRectangularVertices(BaseSurfNum,SurfNum,XCoord,ZCoord,Length,Height)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   December 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine creates world/3d coordinates for rectangular surfaces using relative X and Z, length & height.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Vectors

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: BaseSurfNum  ! Base surface
  INTEGER, INTENT(IN) :: SurfNum
  REAL(r64), INTENT(IN) :: XCoord
  REAL(r64), INTENT(IN) :: ZCoord
  REAL(r64), INTENT(IN) :: Length
  REAL(r64), INTENT(IN) :: Height

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) :: SurfAzimuth  ! Surface Azimuth/Facing (same as Base Surface)
  REAL(r64) :: SurfTilt     ! Tilt (same as Base Surface)
  REAL(r64) :: XLLC
  REAL(r64) :: YLLC
  REAL(r64) :: ZLLC
  REAL(r64) :: CosSurfAzimuth
  REAL(r64) :: SinSurfAzimuth
  REAL(r64) :: CosSurfTilt
  REAL(r64) :: SinSurfTilt
  REAL(r64) :: BaseCosSurfAzimuth
  REAL(r64) :: BaseSinSurfAzimuth
  REAL(r64) :: BaseCosSurfTilt
  REAL(r64) :: BaseSinSurfTilt
  REAL(r64) :: XX(4),YY(4)
  REAL(r64) :: Perimeter
  INTEGER :: N
  INTEGER :: Vrt


  IF (BaseSurfNum == 0) RETURN  ! invalid base surface, don't bother

  ! Tilt and Facing (Azimuth) will be same as the Base Surface

  SurfaceTmp(SurfNum)%Height=Height
  SurfaceTmp(SurfNum)%Width=Length

  SurfAzimuth = SurfaceTmp(SurfNum)%Azimuth
  SurfTilt    = SurfaceTmp(SurfNum)%Tilt
  CosSurfAzimuth=COS(SurfAzimuth*DegToRadians)
  SinSurfAzimuth=SIN(SurfAzimuth*DegToRadians)
  CosSurfTilt=COS(SurfTilt*DegToRadians)
  SinSurfTilt=SIN(SurfTilt*DegToRadians)
  BaseCosSurfAzimuth=SurfaceTmp(BaseSurfNum)%CosAzim
  BaseSinSurfAzimuth=SurfaceTmp(BaseSurfNum)%SinAzim
  BaseCosSurfTilt=SurfaceTmp(BaseSurfNum)%CosTilt
  BaseSinSurfTilt=SurfaceTmp(BaseSurfNum)%SinTilt

  XLLC  = SurfaceTmp(BaseSurfNum)%Vertex(2)%X-XCoord*BaseCosSurfAzimuth-ZCoord*BaseCosSurfTilt*BaseSinSurfAzimuth
  YLLC  = SurfaceTmp(BaseSurfNum)%Vertex(2)%Y+XCoord*BaseSinSurfAzimuth-ZCoord*BaseCosSurfTilt*BaseCosSurfAzimuth
  ZLLC  = SurfaceTmp(BaseSurfNum)%Vertex(2)%Z+ZCoord*BaseSinSurfTilt

  XX(1)=0.0d0
  XX(2)=0.0d0
  XX(3)=Length
  XX(4)=Length
  YY(1)=Height
  YY(4)=Height
  YY(3)=0.0d0
  YY(2)=0.0d0

  DO N = 1, SurfaceTmp(SurfNum)%Sides
    Vrt=N
    SurfaceTmp(SurfNum)%Vertex(Vrt)%X=XLLC-XX(N)*CosSurfAzimuth-YY(N)*CosSurfTilt*SinSurfAzimuth
    SurfaceTmp(SurfNum)%Vertex(Vrt)%Y=YLLC+XX(N)*SinSurfAzimuth-YY(N)*CosSurfTilt*CosSurfAzimuth
    SurfaceTmp(SurfNum)%Vertex(Vrt)%Z=ZLLC+YY(N)*SinSurfTilt
  END DO

  CALL CreateNewellAreaVector(SurfaceTmp(SurfNum)%Vertex,SurfaceTmp(SurfNum)%Sides,SurfaceTmp(SurfNum)%NewellAreaVector)
  SurfaceTmp(SurfNum)%GrossArea=VecLength(SurfaceTmp(SurfNum)%NewellAreaVector)
  SurfaceTmp(SurfNum)%Area=SurfaceTmp(SurfNum)%GrossArea
  SurfaceTmp(SurfNum)%NetAreaShadowCalc = SurfaceTmp(SurfNum)%Area
  CALL CreateNewellSurfaceNormalVector(SurfaceTmp(SurfNum)%Vertex,SurfaceTmp(SurfNum)%Sides,  &
     SurfaceTmp(SurfNum)%NewellSurfaceNormalVector)
  CALL DetermineAzimuthAndTilt(SurfaceTmp(SurfNum)%Vertex,SurfaceTmp(SurfNum)%Sides,SurfAzimuth,SurfTilt,  &
                               SurfaceTmp(SurfNum)%lcsx,SurfaceTmp(SurfNum)%lcsy,SurfaceTmp(SurfNum)%lcsz, &
                               SurfaceTmp(SurfNum)%GrossArea,SurfaceTmp(SurfNum)%NewellSurfaceNormalVector)
  SurfaceTmp(SurfNum)%Azimuth=SurfAzimuth
  SurfaceTmp(SurfNum)%Tilt=SurfTilt
  ! Sine and cosine of azimuth and tilt
  SurfaceTmp(SurfNum)%SinAzim = SinSurfAzimuth
  SurfaceTmp(SurfNum)%CosAzim = CosSurfAzimuth
  SurfaceTmp(SurfNum)%SinTilt = SinSurfTilt
  SurfaceTmp(SurfNum)%CosTilt = CosSurfTilt
  IF (SurfaceTmp(SurfNum)%Class /= SurfaceClass_Window .and. SurfaceTmp(SurfNum)%Class /= SurfaceClass_GlassDoor .and.  &
      SurfaceTmp(SurfNum)%Class /= SurfaceClass_Door) &
    SurfaceTmp(SurfNum)%ViewFactorGround = 0.5d0 * (1.0d0 - SurfaceTmp(SurfNum)%CosTilt)
  ! Outward normal unit vector (pointing away from room)
  SurfaceTmp(SurfNum)%OutNormVec = SurfaceTmp(SurfNum)%NewellSurfaceNormalVector
  DO N=1,3
    IF (ABS(SurfaceTmp(SurfNum)%OutNormVec(N)-1.0d0) < 1.d-06) SurfaceTmp(SurfNum)%OutNormVec(N) = +1.0d0
    IF (ABS(SurfaceTmp(SurfNum)%OutNormVec(N)+1.0d0) < 1.d-06) SurfaceTmp(SurfNum)%OutNormVec(N) = -1.0d0
    IF (ABS(SurfaceTmp(SurfNum)%OutNormVec(N))     < 1.d-06) SurfaceTmp(SurfNum)%OutNormVec(N) =  0.0d0
  ENDDO

!  IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Roof .and. SurfTilt > 80.) THEN
!    WRITE(TiltString,'(F5.1)') SurfTilt
!    TiltString=ADJUSTL(TiltString)
!    CALL ShowWarningError('Roof/Ceiling Tilt='//TRIM(TiltString)//', much greater than expected tilt of 0,'// &
!                          ' for Surface='//TRIM(SurfaceTmp(SurfNum)%Name)//  &
!                          ', in Zone='//TRIM(SurfaceTmp(SurfNum)%ZoneName))
!  ENDIF
!  IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Floor .and. SurfTilt < 170.) THEN
!    WRITE(TiltString,'(F5.1)') SurfTilt
!    TiltString=ADJUSTL(TiltString)
!    CALL ShowWarningError('Floor Tilt='//TRIM(TiltString)//', much less than expected tilt of 180,'//   &
!                          ' for Surface='//TRIM(SurfaceTmp(SurfNum)%Name)//  &
!                          ', in Zone='//TRIM(SurfaceTmp(SurfNum)%ZoneName))
!  ENDIF
  IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Window .or. SurfaceTmp(SurfNum)%Class == SurfaceClass_GlassDoor .or.  &
      SurfaceTmp(SurfNum)%Class == SurfaceClass_Door) &
      SurfaceTmp(SurfNum)%Area =  SurfaceTmp(SurfNum)%Area * SurfaceTmp(SurfNum)%Multiplier
  ! Can perform tests on this surface here
  SurfaceTmp(SurfNum)%ViewFactorSky=0.5d0*(1.d0+SurfaceTmp(SurfNum)%CosTilt)
  ! The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
  ! surfaces
  SurfaceTmp(SurfNum)%ViewFactorSkyIR = SurfaceTmp(SurfNum)%ViewFactorSky
  SurfaceTmp(SurfNum)%ViewFactorGroundIR = 0.5d0*(1.d0-SurfaceTmp(SurfNum)%CosTilt)

  Perimeter=Distance(SurfaceTmp(SurfNum)%Vertex(SurfaceTmp(SurfNum)%Sides),SurfaceTmp(SurfNum)%Vertex(1))
  DO Vrt=2,SurfaceTmp(SurfNum)%Sides
    Perimeter = Perimeter+Distance(SurfaceTmp(SurfNum)%Vertex(Vrt),SurfaceTmp(SurfNum)%Vertex(Vrt-1))
  ENDDO
  SurfaceTmp(SurfNum)%Perimeter=Perimeter

  ! Call to transform vertices

  Call TransformVertsByAspect(SurfNum,SurfaceTmp(SurfNum)%Sides)

  RETURN

END SUBROUTINE MakeRelativeRectangularVertices

SUBROUTINE GetAttShdSurfaceData(ErrorsFound,SurfNum,TotShdSubs)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the HeatTransfer Surface Data,
          ! checks it for errors, etc.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
  !  Attached Shading Surface Definition
  !Surface:Shading:Attached,
  !       \memo used For fins, overhangs, elements that shade the building, are attached to the building
  !       \memo but are not part of the heat transfer calculations
  !  A1 , \field User Supplied Surface Name
  !       \required-field
  !       \type alpha
  !       \reference AttachedShadingSurfNames
  !  A2 , \field Base Surface Name
  !       \required-field
  !       \type object-list
  !       \object-list SurfaceNames
  !  A3,  \field TransSchedShadowSurf
  !       \note Transmittance schedule for the shading device, defaults to zero (always opaque)
  !       \type object-list
  !       \object-list ScheduleNames
  !  N1 , \field Number of Surface Vertex Groups -- Number of (X,Y,Z) groups in this surface
  !       \required-field
  !       \note currently limited 3 or 4, later?
  !       \minimum 3
  !       \maximum 4
  !       \note vertices are given in SurfaceGeometry coordinates -- if relative, all surface coordinates
  !       \note are "relative" to the Zone Origin.  if WCS, then building and zone origins are used
  !       \note for some internal calculations, but all coordinates are given in an "absolute" system.
  !  N2,  \field Vertex 1 X-coordinate
  !       \units m
  !       \type real
  !  N3-13; as indicated by the N2 value

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList, VerifyName, GetObjectDefMaxArgs
  USE ScheduleManager, ONLY: GetScheduleIndex,CheckScheduleValueMinMax,GetScheduleMinValue,GetScheduleMaxValue
  USE General, ONLY: TrimSigDigits
  USE DataReportingFlags

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT)      :: ErrorsFound       ! Error flag indicator (true if errors found)
  INTEGER, INTENT(INOUT)      :: SurfNum           ! Count of Current SurfaceNumber
  INTEGER, INTENT(IN)         :: TotShdSubs        ! Number of Attached Shading SubSurfaces to obtain

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: IOStat            ! IO Status when calling get input subroutine
  INTEGER :: NumAlphas         ! Number of alpha names being passed
  INTEGER :: NumNumbers        ! Number of properties being passed
  INTEGER :: Found             ! For matching interzone surfaces
  INTEGER :: Loop
  LOGICAL :: ErrorInName
  LOGICAL :: IsBlank
  REAL(r64) :: SchedMinValue
  REAL(r64) :: SchedMaxValue

  IF (TotShdSubs > 0 .and. SolarDistribution == MinimalShadowing) THEN
    CALL ShowWarningError('Shading effects of Fins and Overhangs are ignored when Solar Distribution = MinimalShadowing')
  ENDIF

  cCurrentModuleObject='Shading:Zone:Detailed'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,Loop,NumAlphas,NumNumbers)
  IF (NumAlphas /= 3) THEN
    CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Object Definition indicates '// &
                         'not = 3 Alpha Objects, Number Indicated='//  &
                         TRIM(TrimSigDigits(NumAlphas)))
    ErrorsFound=.true.
  ENDIF

  DO Loop=1,TotShdSubs
    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlphas, &
                       rNumericArgs,NumNumbers,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    ErrorInName=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),SurfaceTmp%Name,SurfNum,ErrorInName,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (ErrorInName) THEN
      CALL ShowContinueError('...each surface name must not duplicate other surface names (of any type)')
      ErrorsFound=.true.
      CYCLE
    ENDIF

    SurfNum=SurfNum+1
    SurfaceTmp(SurfNum)%Name = cAlphaArgs(1)  ! Set the Surface Name in the Derived Type
    SurfaceTmp(SurfNum)%Class= SurfaceClass_Shading
    SurfaceTmp(SurfNum)%HeatTransSurf=.false.
    SurfaceTmp(SurfNum)%BaseSurfName=cAlphaArgs(2)
    !  The subsurface inherits properties from the base surface
    !  Exterior conditions, Zone, etc.
    !  We can figure out the base surface though, because they've all been entered
    Found=FindIteminList(SurfaceTmp(SurfNum)%BaseSurfName,SurfaceTmp%Name,TotSurfaces)
    IF (Found > 0) THEN
      !SurfaceTmp(SurfNum)%BaseSurf=Found
      SurfaceTmp(SurfNum)%ExtBoundCond=SurfaceTmp(Found)%ExtBoundCond
      SurfaceTmp(SurfNum)%ExtSolar=SurfaceTmp(Found)%ExtSolar
      SurfaceTmp(SurfNum)%ExtWind=SurfaceTmp(Found)%ExtWind
      SurfaceTmp(SurfNum)%Zone=SurfaceTmp(Found)%Zone ! Necessary to do relative coordinates in GetVertices below
      SurfaceTmp(SurfNum)%ZoneName=SurfaceTmp(Found)%ZoneName ! Necessary to have surface drawn in OutputReports
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                             '", invalid '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2)))
      ErrorsFound=.true.
    ENDIF
    IF (SurfaceTmp(SurfNum)%ExtBoundCond == UnenteredAdjacentZoneSurface) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                             '", invalid '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2)))
      CALL ShowContinueError('...trying to attach a shading device to an interzone surface.')
      ErrorsFound=.true.
      SurfaceTmp(SurfNum)%ExtBoundCond = ExternalEnvironment  ! reset so program won't crash during "add surfaces"
    ENDIF
    IF (SurfaceTmp(SurfNum)%ExtBoundCond == UnreconciledZoneSurface) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                             '", invalid '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2)))
      CALL ShowContinueError('...trying to attach a shading device to an interior surface.')
      ErrorsFound=.true.
      SurfaceTmp(SurfNum)%ExtBoundCond = ExternalEnvironment  ! reset so program won't crash during "add surfaces"
    ENDIF

    IF (.not. lAlphaFieldBlanks(3)) THEN
      SurfaceTmp(SurfNum)%SchedShadowSurfIndex = GetScheduleIndex(cAlphaArgs(3))
      IF (SurfaceTmp(SurfNum)%SchedShadowSurfIndex == 0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                             '", '//TRIM(cAlphaFieldNames(3))//' not found="'//TRIM(cAlphaArgs(3)))
        ErrorsFound=.true.
      ENDIF
    ELSE
      SurfaceTmp(SurfNum)%SchedShadowSurfIndex=0
    ENDIF
    IF (SurfaceTmp(SurfNum)%SchedShadowSurfIndex /= 0) THEN
      IF (.not. CheckScheduleValueMinMax(SurfaceTmp(SurfNum)%SchedShadowSurfIndex,'>=',0.0d0,'<=',1.0d0)) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                             '", '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))// &
                             '", values not in range [0,1].')
        ErrorsFound=.true.
      ENDIF
      SchedMinValue=GetScheduleMinValue(SurfaceTmp(SurfNum)%SchedShadowSurfIndex)
      SurfaceTmp(SurfNum)%SchedMinValue=SchedMinValue
      SchedMaxValue=GetScheduleMaxValue(SurfaceTmp(SurfNum)%SchedShadowSurfIndex)
      IF (SchedMinValue == 1.0d0) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                             '", '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))// &
                             '", is always transparent.')
        SurfaceTmp(SurfNum)%IsTransparent=.true.
      ENDIF
      IF (SchedMinValue < 0.0d0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                             '", '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))// &
                             '", has schedule values < 0.')
        CALL ShowContinueError('...Schedule values < 0 have no meaning for shading elements.')
      ENDIF
      IF (SchedMaxValue > 1.0d0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                             '", '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))// &
                             '", has schedule values > 1.')
        CALL ShowContinueError('...Schedule values > 1 have no meaning for shading elements.')
      ENDIF
      IF (ABS(SchedMinValue-SchedMaxValue) > 1.0d-6) THEN
        SurfaceTmp(SurfNum)%ShadowSurfSchedVaries=.true.
        ShadingTransmittanceVaries=.true.
      ENDIF
    ENDIF
    IF (lNumericFieldBlanks(1) .or. rNumericArgs(1) == AutoCalculate) THEN
      rNumericArgs(1)=(NumNumbers-1)/3
      SurfaceTmp(SurfNum)%Sides=rNumericArgs(1)
      IF (MOD(NumNumbers-1,3) /= 0) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                             '", '//TRIM(cNumericFieldNames(1))//          &
                              ' not even multiple of 3. Will read in '//   &
                              TRIM(TrimSigDigits(SurfaceTmp(SurfNum)%Sides)))
      ENDIF
      IF (rNumericArgs(1) < 3) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                             '", '//TRIM(cNumericFieldNames(1))//' (autocalculate) must be >= 3. Only '//  &
                             TRIM(TrimSigDigits(SurfaceTmp(SurfNum)%Sides))//' provided.')
        ErrorsFound=.true.
        CYCLE
      ENDIF
    ELSE
      SurfaceTmp(SurfNum)%Sides=rNumericArgs(1)
    ENDIF
    ALLOCATE(SurfaceTmp(SurfNum)%Vertex(SurfaceTmp(SurfNum)%Sides))
    CALL GetVertices(SurfNum,SurfaceTmp(SurfNum)%Sides,rNumericArgs(2:))
    CALL CheckConvexity(SurfNum,SurfaceTmp(SurfNum)%Sides)
!    IF (SurfaceTmp(SurfNum)%Sides == 3) THEN
!      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
!                        ' should not be triangular.')
!      CALL ShowContinueError('...Check results carefully.')
!      ErrorsFound=.true.
!    ENDIF
    ! Reset surface to be "detached"
    SurfaceTmp(SurfNum)%BaseSurf=0
!    SurfaceTmp(SurfNum)%BaseSurfName='  '
    SurfaceTmp(SurfNum)%Zone=0
    !SurfaceTmp(SurfNum)%ZoneName='  '
    IF (MakeMirroredAttachedShading) THEN
      CALL MakeMirrorSurface(SurfNum)
    END IF

  ENDDO

  RETURN

END SUBROUTINE GetAttShdSurfaceData

SUBROUTINE GetSimpleShdSurfaceData(ErrorsFound,SurfNum,TotOverhangs,TotOverhangsProjection,TotFins,TotFinsProjection)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   January 2009
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Get simple overhang and fin descriptions.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList, VerifyName, GetObjectDefMaxArgs
  USE General, ONLY: TrimSigDigits,RoundSigDigits
  USE DataReportingFlags
  USE Vectors

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT)         :: ErrorsFound       ! Error flag indicator (true if errors found)
  INTEGER, INTENT(INOUT)         :: SurfNum           ! Count of Current SurfaceNumber
  INTEGER, INTENT(IN)            :: TotOverhangs      ! Number of Overhangs to obtain
  INTEGER, INTENT(IN)            :: TotOverhangsProjection  ! Number of Overhangs (projection) to obtain
  INTEGER, INTENT(IN)            :: TotFins           ! Number of Fins to obtain
  INTEGER, INTENT(IN)            :: TotFinsProjection ! Number of Fins (projection) to obtain

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER, DIMENSION(4) :: cModuleObjects=  &
    (/'Shading:Overhang           ',  &
      'Shading:Overhang:Projection',  &
      'Shading:Fin                ',  &
      'Shading:Fin:Projection     '/)
  CHARACTER(len=*), PARAMETER :: dfmt='(A,3(2x,f6.2))'

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: Item
  INTEGER :: ItemsToGet
  INTEGER :: Loop
  INTEGER :: NumAlphas
  INTEGER :: NumNumbers
  INTEGER :: IOStat            ! IO Status when calling get input subroutine
  INTEGER :: Found             ! For matching base surfaces
  LOGICAL :: ErrorInName
  LOGICAL :: IsBlank
  REAL(r64) :: Depth
  REAL(r64) :: Length
  REAL(r64) :: Xp
  REAL(r64) :: Yp
  REAL(r64) :: Zp
  REAL(r64) :: XLLC
  REAL(r64) :: YLLC
  INTEGER :: BaseSurfNum
  REAL(r64) :: TiltAngle
  LOGICAL :: MakeFin

  IF ((TotOverhangs+TotOverhangsProjection+TotFins+TotFinsProjection) > 0 .and. SolarDistribution == MinimalShadowing) THEN
    CALL ShowWarningError('Shading effects of Fins and Overhangs are ignored when Solar Distribution = MinimalShadowing')
  ENDIF

  DO Item=1,4

    cCurrentModuleObject=cModuleObjects(Item)
    IF (Item == 1) THEN
      ItemsToGet=TotOverhangs
    ELSEIF (Item == 2) THEN
      ItemsToGet=TotOverhangsProjection
    ELSEIF (Item == 3) THEN
      ItemsToGet=TotFins
    ELSE  ! ! (Item == 4) THEN
      ItemsToGet=TotFinsProjection
    ENDIF

    DO Loop=1,ItemsToGet
      CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOSTAT,  &
                     AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                     AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      ErrorInName=.false.
      IsBlank=.false.
      CALL VerifyName(cAlphaArgs(1),SurfaceTmp%Name,SurfNum,ErrorInName,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (ErrorInName) THEN
        CALL ShowContinueError('...each surface name must not duplicate other surface names (of any type)')
        ErrorsFound=.true.
        CYCLE
      ENDIF

      SurfNum=SurfNum+1
      SurfaceTmp(SurfNum)%Name = cAlphaArgs(1)  ! Set the Surface Name in the Derived Type
      SurfaceTmp(SurfNum)%Class= SurfaceClass_Shading
      SurfaceTmp(SurfNum)%HeatTransSurf=.false.
      ! this object references a window or door....
      Found=FindIteminList(cAlphaArgs(2),SurfaceTmp%Name,TotSurfaces)
      IF (Found > 0) THEN
        BaseSurfNum=SurfaceTmp(Found)%BaseSurf
        SurfaceTmp(SurfNum)%BaseSurfName=SurfaceTmp(Found)%BaseSurfName
        SurfaceTmp(SurfNum)%ExtBoundCond=SurfaceTmp(Found)%ExtBoundCond
        SurfaceTmp(SurfNum)%ExtSolar=SurfaceTmp(Found)%ExtSolar
        SurfaceTmp(SurfNum)%ExtWind=SurfaceTmp(Found)%ExtWind
        SurfaceTmp(SurfNum)%Zone=SurfaceTmp(Found)%Zone ! Necessary to do relative coordinates in GetVertices below
        SurfaceTmp(SurfNum)%ZoneName=SurfaceTmp(Found)%ZoneName ! Necessary to have surface drawn in OutputReports
      ELSE
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                               '", invalid '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2)))
        ErrorsFound=.true.
        CYCLE
      ENDIF
      IF (SurfaceTmp(SurfNum)%ExtBoundCond == UnenteredAdjacentZoneSurface) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                               '", invalid '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2)))
        CALL ShowContinueError('...trying to attach a shading device to an interzone surface.')
        ErrorsFound=.true.
        SurfaceTmp(SurfNum)%ExtBoundCond = ExternalEnvironment  ! reset so program won't crash during "add surfaces"
      ENDIF
      IF (SurfaceTmp(SurfNum)%ExtBoundCond == UnreconciledZoneSurface) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                               '", invalid '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2)))
        CALL ShowContinueError('...trying to attach a shading device to an interior surface.')
        ErrorsFound=.true.
        SurfaceTmp(SurfNum)%ExtBoundCond = ExternalEnvironment  ! reset so program won't crash during "add surfaces"
      ENDIF

      SurfaceTmp(SurfNum)%SchedShadowSurfIndex=0

!===== Overhang =====

      IF (Item < 3) THEN
        !  Found is the surface window or door.
        !   N1,  \field Height above Window or Door
        !        \units m
        !   N2,  \field Tilt Angle from Window/Door
        !        \units deg
        !        \default 90
        !        \minimum 0
        !        \maximum 180
        !   N3,  \field Left extension from Window/Door Width
        !        \units m
        !   N4,  \field Right extension from Window/Door Width
        !        \note N3 + N4 + Window/Door Width is Overhang Length
        !        \units m
        !   N5;  \field Depth
        !        \units m
        ! for projection option:
        !   N5;  \field Depth as Fraction of Window/Door Height
        !        \units m
        Length=rNumericArgs(3)+rNumericArgs(4)+SurfaceTmp(Found)%Width
        IF (Item == 1) THEN
          Depth=rNumericArgs(5)
        ELSEIF (Item == 2) THEN
          Depth=rNumericArgs(5)*SurfaceTmp(Found)%Height
        ENDIF

        IF (Length*Depth <= 0.0d0) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//'="'//trim(cAlphaArgs(1))//'", illegal surface area=['//  &
            trim(RoundSigDigits(Length*Depth,2))//']. Surface will NOT be entered.')
          CYCLE
        ENDIF

        TiltAngle=SurfaceTmp(Found)%Tilt+rNumericArgs(2)
        SurfaceTmp(SurfNum)%Tilt=TiltAngle
        SurfaceTmp(SurfNum)%Azimuth=SurfaceTmp(Found)%Azimuth

        ! Make it relative to surface origin.....
        Xp=SurfaceTmp(Found)%Vertex(2)%x-SurfaceTmp(BaseSurfNum)%Vertex(2)%x
        Yp=SurfaceTmp(Found)%Vertex(2)%y-SurfaceTmp(BaseSurfNum)%Vertex(2)%y
        Zp=SurfaceTmp(Found)%Vertex(2)%z-SurfaceTmp(BaseSurfNum)%Vertex(2)%z

        XLLC=-Xp*SurfaceTmp(BaseSurfNum)%CosAzim+Yp*SurfaceTmp(BaseSurfNum)%SinAzim

        YLLC=-Xp*SurfaceTmp(BaseSurfNum)%SinAzim*SurfaceTmp(BaseSurfNum)%CosTilt-  &
           Yp*SurfaceTmp(BaseSurfNum)%CosAzim*SurfaceTmp(BaseSurfNum)%CosTilt+Zp*SurfaceTmp(BaseSurfNum)%SinTilt

        SurfaceTmp(SurfNum)%Sides=4
        ALLOCATE(SurfaceTmp(SurfNum)%Vertex(SurfaceTmp(SurfNum)%Sides))

        CALL MakeRelativeRectangularVertices(BaseSurfNum,SurfNum,  &
                         XLLC-rNumericArgs(3),YLLC+SurfaceTmp(Found)%Height+rNumericArgs(1),Length,Depth)


      ! Reset surface to be "detached"
      !    SurfaceTmp(SurfNum)%BaseSurfName='  '
      !    SurfaceTmp(SurfNum)%ZoneName='  '

        SurfaceTmp(SurfNum)%BaseSurf=0
        SurfaceTmp(SurfNum)%Zone=0

      ! and mirror
        IF (MakeMirroredAttachedShading) THEN
          CALL MakeMirrorSurface(SurfNum)
        END IF

      ELSE  ! Fins

!===== Fins =====

!===== Left Fin =====

        !   N1,  \field Left Extension from Window/Door
        !        \units m
        !   N2,  \field Left Distance Above Top of Window
        !        \units m
        !   N3,  \field Left Distance Below Bottom of Window
        !        \units m
        !        \note N2 + N3 + height of Window/Door is height of Fin
        !   N4,  \field Left Tilt Angle from Window/Door
        !        \units deg
        !        \default 90
        !        \minimum 0
        !        \maximum 180
        !   N5,  \field Left Depth
        !        \units m
        ! for projection option:
        !   N5,  \field Left Depth as Fraction of Window/Door Width
        !        \units m
        SurfaceTmp(SurfNum)%Name=TRIM(SurfaceTmp(SurfNum)%Name)//' Left'
        Length=rNumericArgs(2)+rNumericArgs(3)+SurfaceTmp(Found)%Height
        IF (Item == 3) THEN
          Depth=rNumericArgs(5)
        ELSEIF (Item == 4) THEN
          Depth=rNumericArgs(5)*SurfaceTmp(Found)%Width
        ENDIF

        MakeFin=.true.
        IF (Length*Depth <= 0.0d0) THEN
          CALL ShowWarningError(trim(cCurrentModuleObject)//'=Left Fin of "'//trim(cAlphaArgs(1))//  &
            '", illegal surface area=['//  &
            trim(RoundSigDigits(Length*Depth,2))//']. Surface will NOT be entered.')
          MakeFin=.false.
        ENDIF

        IF (MakeFin) THEN
          TiltAngle=SurfaceTmp(Found)%Tilt
          SurfaceTmp(SurfNum)%Tilt=TiltAngle
          SurfaceTmp(SurfNum)%Azimuth=SurfaceTmp(Found)%Azimuth-(180.0d0-rNumericArgs(4))

          ! Make it relative to surface origin.....

          Xp=SurfaceTmp(Found)%Vertex(2)%x-SurfaceTmp(BaseSurfNum)%Vertex(2)%x
          Yp=SurfaceTmp(Found)%Vertex(2)%y-SurfaceTmp(BaseSurfNum)%Vertex(2)%y
          Zp=SurfaceTmp(Found)%Vertex(2)%z-SurfaceTmp(BaseSurfNum)%Vertex(2)%z

          XLLC=-Xp*SurfaceTmp(BaseSurfNum)%CosAzim+Yp*SurfaceTmp(BaseSurfNum)%SinAzim

          YLLC=-Xp*SurfaceTmp(BaseSurfNum)%SinAzim*SurfaceTmp(BaseSurfNum)%CosTilt-  &
             Yp*SurfaceTmp(BaseSurfNum)%CosAzim*SurfaceTmp(BaseSurfNum)%CosTilt+Zp*SurfaceTmp(BaseSurfNum)%SinTilt

          SurfaceTmp(SurfNum)%CosAzim=COS(SurfaceTmp(SurfNum)%Azimuth*DegToRadians)
          SurfaceTmp(SurfNum)%SinAzim=SIN(SurfaceTmp(SurfNum)%Azimuth*DegToRadians)
          SurfaceTmp(SurfNum)%CosTilt=COS(SurfaceTmp(SurfNum)%Tilt*DegToRadians)
          SurfaceTmp(SurfNum)%SinTilt=SIN(SurfaceTmp(SurfNum)%Tilt*DegToRadians)

          SurfaceTmp(SurfNum)%Sides=4
          ALLOCATE(SurfaceTmp(SurfNum)%Vertex(SurfaceTmp(SurfNum)%Sides))

          CALL MakeRelativeRectangularVertices(BaseSurfNum,SurfNum,  &
                           XLLC-rNumericArgs(1),YLLC-rNumericArgs(3),-Depth,Length)

        ! Reset surface to be "detached"
        !    SurfaceTmp(SurfNum)%BaseSurfName='  '
        !    SurfaceTmp(SurfNum)%ZoneName='  '

          SurfaceTmp(SurfNum)%BaseSurf=0
          SurfaceTmp(SurfNum)%Zone=0

        ! and mirror
          IF (MakeMirroredAttachedShading) THEN
            CALL MakeMirrorSurface(SurfNum)
          END IF
        ELSE
          SurfNum=SurfNum-1
        ENDIF

!===== Right Fin =====

        !   N6,  \field Right Extension from Window/Door
        !        \units m
        !   N7,  \field Right Distance Above Top of Window
        !        \units m
        !   N8,  \field Right Distance Below Bottom of Window
        !        \note N7 + N8 + height of Window/Door is height of Fin
        !        \units m
        !   N9,  \field Right Tilt Angle from Window/Door
        !        \units deg
        !        \default 90
        !        \minimum 0
        !        \maximum 180
        !   N10; \field Right Depth
        !        \units m
        ! for projection option:
        !   N10; \field Right Depth as Fraction of Window/Door Width
        !        \units m

        SurfNum=SurfNum+1
        SurfaceTmp(SurfNum)%Name = TRIM(cAlphaArgs(1))//' Right'  ! Set the Surface Name in the Derived Type
        SurfaceTmp(SurfNum)%Class= SurfaceClass_Shading
        SurfaceTmp(SurfNum)%HeatTransSurf=.false.
        BaseSurfNum=SurfaceTmp(Found)%BaseSurf
        SurfaceTmp(SurfNum)%BaseSurfName=SurfaceTmp(Found)%BaseSurfName
        SurfaceTmp(SurfNum)%ExtBoundCond=SurfaceTmp(Found)%ExtBoundCond
        SurfaceTmp(SurfNum)%ExtSolar=SurfaceTmp(Found)%ExtSolar
        SurfaceTmp(SurfNum)%ExtWind=SurfaceTmp(Found)%ExtWind
        SurfaceTmp(SurfNum)%Zone=SurfaceTmp(Found)%Zone ! Necessary to do relative coordinates in GetVertices below
        SurfaceTmp(SurfNum)%ZoneName=SurfaceTmp(Found)%ZoneName ! Necessary to have surface drawn in OutputReports

        SurfaceTmp(SurfNum)%SchedShadowSurfIndex=0
        Length=rNumericArgs(7)+rNumericArgs(8)+SurfaceTmp(Found)%Height
        IF (Item == 3) THEN
          Depth=rNumericArgs(10)
        ELSEIF (Item == 4) THEN
          Depth=rNumericArgs(10)*SurfaceTmp(Found)%Width
        ENDIF

        MakeFin=.true.
        IF (Length*Depth <= 0.0d0) THEN
          CALL ShowWarningError(trim(cCurrentModuleObject)//'=Right Fin of "'//trim(cAlphaArgs(1))//  &
            '", illegal surface area=['//  &
            trim(RoundSigDigits(Length*Depth,2))//']. Surface will NOT be entered.')
          MakeFin=.false.
        ENDIF

        IF (MakeFin) THEN
          ! Make it relative to surface origin.....

          Xp=SurfaceTmp(Found)%Vertex(2)%x-SurfaceTmp(BaseSurfNum)%Vertex(2)%x
          Yp=SurfaceTmp(Found)%Vertex(2)%y-SurfaceTmp(BaseSurfNum)%Vertex(2)%y
          Zp=SurfaceTmp(Found)%Vertex(2)%z-SurfaceTmp(BaseSurfNum)%Vertex(2)%z

          XLLC=-Xp*SurfaceTmp(BaseSurfNum)%CosAzim+Yp*SurfaceTmp(BaseSurfNum)%SinAzim

          YLLC=-Xp*SurfaceTmp(BaseSurfNum)%SinAzim*SurfaceTmp(BaseSurfNum)%CosTilt-  &
              Yp*SurfaceTmp(BaseSurfNum)%CosAzim*SurfaceTmp(BaseSurfNum)%CosTilt+Zp*SurfaceTmp(BaseSurfNum)%SinTilt

          TiltAngle=SurfaceTmp(Found)%Tilt
          SurfaceTmp(SurfNum)%Tilt=TiltAngle
          SurfaceTmp(SurfNum)%Azimuth=SurfaceTmp(Found)%Azimuth-(180.0-rNumericArgs(9))
          SurfaceTmp(SurfNum)%CosAzim=COS(SurfaceTmp(SurfNum)%Azimuth*DegToRadians)
          SurfaceTmp(SurfNum)%SinAzim=SIN(SurfaceTmp(SurfNum)%Azimuth*DegToRadians)
          SurfaceTmp(SurfNum)%CosTilt=COS(SurfaceTmp(SurfNum)%Tilt*DegToRadians)
          SurfaceTmp(SurfNum)%SinTilt=SIN(SurfaceTmp(SurfNum)%Tilt*DegToRadians)

          SurfaceTmp(SurfNum)%Sides=4
          ALLOCATE(SurfaceTmp(SurfNum)%Vertex(SurfaceTmp(SurfNum)%Sides))

          CALL MakeRelativeRectangularVertices(BaseSurfNum,SurfNum,  &
                           XLLC+SurfaceTmp(Found)%Width+rNumericArgs(6),YLLC-rNumericArgs(8),-Depth,Length)

        ! Reset surface to be "detached"
        !    SurfaceTmp(SurfNum)%BaseSurfName='  '
        !    SurfaceTmp(SurfNum)%ZoneName='  '

          SurfaceTmp(SurfNum)%BaseSurf=0
          SurfaceTmp(SurfNum)%Zone=0

        ! and mirror
          IF (MakeMirroredAttachedShading) THEN
            CALL MakeMirrorSurface(SurfNum)
          END IF
        ELSE
          SurfNum=SurfNum-1
        ENDIF
      ENDIF

    ENDDO

  ENDDO

  RETURN

END SUBROUTINE GetSimpleShdSurfaceData

SUBROUTINE GetIntMassSurfaceData(ErrorsFound,SurfNum,TotIntMass)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the Internal Surface Data,
          ! checks it for errors, etc.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
  ! Internal Mass Surface Definition
  !Surface:HeatTransfer:InternalMass,
  !       \note used to describe internal zone surface area that does not need to be part of geometric representation
  !  A1 , \field User Supplied Surface Name
  !       \type alpha
  !       \reference SurfaceNames
  !  A2 , \field Construction Name of the Surface
  !       \note To be matched with a construction in this input file
  !       \type object-list
  !       \object-list ConstructionNames
  !  A3 , \field Interior Environment
  !       \note Zone the surface is a part of
  !       \type object-list
  !       \object-list ZoneNames
  !  N1,  \field View factor to Person (to people?)
  !       \type real
  !       \note from the interior of the surface
  !  N2 ; \field Surface area
  !       \units m2

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList, VerifyName
  USE Vectors

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT)      :: ErrorsFound       ! Error flag indicator (true if errors found)
  INTEGER, INTENT(INOUT)      :: SurfNum           ! Count of Current SurfaceNumber
  INTEGER, INTENT(IN)         :: TotIntMass        ! Number of Internal Mass Surfaces to obtain

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: IOStat            ! IO Status when calling get input subroutine
  INTEGER :: SurfaceNumAlpha   ! Number of material alpha names being passed
  INTEGER :: SurfaceNumProp    ! Number of material properties being passed
  INTEGER :: ZoneNum           ! DO loop counter (zones)
  INTEGER :: Loop
  LOGICAL :: ErrorInName
  LOGICAL :: IsBlank

  cCurrentModuleObject='InternalMass'
  DO Loop=1,TotIntMass
    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,SurfaceNumAlpha,  &
                   rNumericArgs,SurfaceNumProp,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    ErrorInName=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),SurfaceTmp%Name,SurfNum,ErrorInName,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (ErrorInName) THEN
      CALL ShowContinueError('...each surface name must not duplicate other surface names (of any type)')
      ErrorsFound=.true.
      CYCLE
    ENDIF

    SurfNum=SurfNum+1
    SurfaceTmp(SurfNum)%Name = cAlphaArgs(1)  ! Set the Surface Name in the Derived Type
    SurfaceTmp(SurfNum)%Class= SurfaceClass_IntMass
    SurfaceTmp(SurfNum)%HeatTransSurf=.true.
    SurfaceTmp(SurfNum)%Construction=FindIteminList(cAlphaArgs(2),Construct%Name,TotConstructs)

    IF(SurfaceTmp(SurfNum)%Construction == 0) THEN
      ErrorsFound = .true.
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                             '", '//TRIM(cAlphaFieldNames(2))//' not found='//TRIM(cAlphaArgs(2)))
    ELSEIF (Construct(SurfaceTmp(SurfNum)%Construction)%TypeIsWindow) THEN
      ErrorsFound = .true.
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                             '", invalid '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))// &
                             '" - has Window materials.')
    ELSE
      Construct(SurfaceTmp(SurfNum)%Construction)%IsUsed=.true.
      SurfaceTmp(SurfNum)%ConstructionStoredInputValue  = SurfaceTmp(SurfNum)%Construction
    END IF
    SurfaceTmp(SurfNum)%ZoneName=cAlphaArgs(3)
    ZoneNum=FindItemInList(SurfaceTmp(SurfNum)%ZoneName,Zone%Name,NumOfZones)

    IF (ZoneNum /= 0) THEN
      SurfaceTmp(SurfNum)%Zone = ZoneNum
    ELSE
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                             '", invalid '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'".')
      SurfaceTmp(SurfNum)%Class=SurfaceTmp(SurfNum)%Class+100
!      SurfaceTmp(SurfNum)%Class=0
      SurfaceTmp(SurfNum)%ZoneName='Unknown Zone'
      ErrorsFound=.true.
    ENDIF
    SurfaceTmp(SurfNum)%GrossArea=rNumericArgs(1)
    SurfaceTmp(SurfNum)%Area=SurfaceTmp(SurfNum)%GrossArea
    SurfaceTmp(SurfNum)%NetAreaShadowCalc = SurfaceTmp(SurfNum)%Area
    SurfaceTmp(SurfNum)%Width=SurfaceTmp(SurfNum)%Area
    SurfaceTmp(SurfNum)%Height=1.0d0
    SurfaceTmp(SurfNum)%Tilt=90.d0
    SurfaceTmp(SurfNum)%CosTilt=COS(90.d0*DegToRadians)
    SurfaceTmp(SurfNum)%SinTilt=SIN(90.d0*DegToRadians)
    SurfaceTmp(SurfNum)%Azimuth=0.0d0
    SurfaceTmp(SurfNum)%CosAzim=COS(0.0d0)
    SurfaceTmp(SurfNum)%SinAzim=SIN(0.0d0)
    ! Outward normal unit vector (pointing away from room)
    SurfaceTmp(SurfNum)%OutNormVec = SurfaceTmp(SurfNum)%lcsz
    SurfaceTmp(SurfNum)%ViewFactorSky=.5d0
    SurfaceTmp(SurfNum)%ExtSolar=.false.
    SurfaceTmp(SurfNum)%ExtWind=.false.
    SurfaceTmp(SurfNum)%BaseSurf = SurfNum
    SurfaceTmp(SurfNum)%BaseSurfName=SurfaceTmp(SurfNum)%Name
    SurfaceTmp(SurfNum)%ExtBoundCondName=SurfaceTmp(SurfNum)%Name
    SurfaceTmp(SurfNum)%ExtBoundCond=UnreconciledZoneSurface

  ENDDO

  RETURN

END SUBROUTINE GetIntMassSurfaceData

SUBROUTINE GetShadingSurfReflectanceData(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   Sept 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Gets data for a Shading Surface Reflectance object.  This is only called when the
          ! Solar Distribution is to be calculated for reflectances.

          ! METHODOLOGY EMPLOYED: na
          ! REFERENCES: na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound ! If errors found in input

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: fmtA='(A)'
          ! INTERFACE BLOCK SPECIFICATIONS:na
          ! DERIVED TYPE DEFINITIONS:na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER :: IOStat                   ! IO Status when calling get input subroutine
  INTEGER :: NumAlpha                 ! Number of alpha names being passed
  INTEGER :: NumProp                  ! Number of properties being passed
  INTEGER :: TotShadingSurfaceReflectance  ! Total Shading Surface Refleftance statements
  INTEGER :: Loop                     ! DO loop index
  INTEGER :: SurfNum                  ! Surface number
  INTEGER :: GlConstrNum              ! Glazing construction number
  LOGICAL :: WrongSurfaceType

  ! For shading surfaces, initialize value of reflectance values to default values. These values
  ! may be overridden below for shading surfaces with an associated Shading Surface Reflectance object.
  DO SurfNum = 1, TotSurfaces
      IF (.not. (SurfaceTmp(SurfNum)%Class == SurfaceClass_Shading .or.      &
                 SurfaceTmp(SurfNum)%Class == SurfaceClass_Detached_F .or.   &
                 SurfaceTmp(SurfNum)%Class == SurfaceClass_Detached_B .or.   &
                 SurfaceTmp(SurfNum)%Class == SurfaceClass_Overhang .or.     &
                 SurfaceTmp(SurfNum)%Class == SurfaceClass_Fin) ) CYCLE
      SurfaceTmp(SurfNum)%ShadowSurfDiffuseSolRefl  = 0.2d0
      SurfaceTmp(SurfNum)%ShadowSurfDiffuseVisRefl  = 0.2d0
      SurfaceTmp(SurfNum)%ShadowSurfGlazingFrac = 0.0d0
      SurfaceTmp(SurfNum)%ShadowSurfGlazingConstruct = 0
  END DO

  ! Get the total number of Shading Surface Reflectance objects
  cCurrentModuleObject='ShadingProperty:Reflectance'
  TotShadingSurfaceReflectance = GetNumObjectsFound(cCurrentModuleObject)
!  IF(TotShadingSurfaceReflectance.EQ.0) RETURN

  DO Loop = 1, TotShadingSurfaceReflectance

    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlpha,rNumericArgs,NumProp,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    SurfNum = FindItemInList(cAlphaArgs(1),SurfaceTmp%Name,TotSurfaces)
    IF(SurfNum == 0) THEN
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                             '", invalid specification')
      CALL ShowContinueError('.. not found '//TRIM(cAlphaFieldNames(1))//'="'//trim(cAlphaArgs(1))//'".')
!      ErrorsFound =.true.
      CYCLE
    END IF

    ! Check that associated surface is a shading surface
    WrongSurfaceType = .FALSE.
    IF(SurfNum /= 0) THEN
      IF (.not. (SurfaceTmp(SurfNum)%Class == SurfaceClass_Shading .or.      &
                 SurfaceTmp(SurfNum)%Class == SurfaceClass_Detached_F .or.   &
                 SurfaceTmp(SurfNum)%Class == SurfaceClass_Detached_B .or.   &
                 SurfaceTmp(SurfNum)%Class == SurfaceClass_Overhang .or.     &
                 SurfaceTmp(SurfNum)%Class == SurfaceClass_Fin) ) WrongSurfaceType = .TRUE.
      IF(WrongSurfaceType) THEN
        CALL ShowSevereError('GetShadingSurfReflectanceData: '//TRIM(cCurrentModuleObject)//'="'//  &
                  TRIM(SurfaceTmp(SurfNum)%Name)//  &
                  '", surface is not a shading surface.')
        ErrorsFound = .TRUE.
        CYCLE
      END IF
    END IF

    ! If associated surface is a shading surface, set reflectance values
    SurfaceTmp(SurfNum)%ShadowSurfGlazingFrac = rNumericArgs(3)
    SurfaceTmp(SurfNum)%ShadowSurfDiffuseSolRefl  = (1.d0-rNumericArgs(3)) * rNumericArgs(1)
    SurfaceTmp(SurfNum)%ShadowSurfDiffuseVisRefl  = (1.d0-rNumericArgs(3)) * rNumericArgs(2)
    IF(rNumericArgs(3) > 0.0d0) THEN
      GlConstrNum = FindItemInList(cAlphaArgs(2),Construct%Name,TotConstructs)
      IF(GlConstrNum == 0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                           '", '//TRIM(cAlphaFieldNames(2))//' not found='//TRIM(cAlphaArgs(2)))
        ErrorsFound =.true.
      ELSE
        Construct(GlConstrNum)%IsUsed=.true.
      END IF
      SurfaceTmp(SurfNum)%ShadowSurfGlazingConstruct = GlConstrNum
    END IF
    SurfNum = FindItemInList('Mir-'//cAlphaArgs(1),SurfaceTmp%Name,TotSurfaces)
    IF (SurfNum == 0) CYCLE
    SurfaceTmp(SurfNum)%ShadowSurfGlazingFrac = rNumericArgs(3)
    SurfaceTmp(SurfNum)%ShadowSurfDiffuseSolRefl  = (1.d0-rNumericArgs(3)) * rNumericArgs(1)
    SurfaceTmp(SurfNum)%ShadowSurfDiffuseVisRefl  = (1.d0-rNumericArgs(3)) * rNumericArgs(2)
    IF(rNumericArgs(3) > 0.0d0) THEN
      GlConstrNum = FindItemInList(cAlphaArgs(2),Construct%Name,TotConstructs)
      IF(GlConstrNum /= 0) THEN
        Construct(GlConstrNum)%IsUsed=.true.
      END IF
      SurfaceTmp(SurfNum)%ShadowSurfGlazingConstruct = GlConstrNum
    END IF

  END DO  ! End of loop over Shading Surface Reflectance objects

  ! Write reflectance values to .eio file.
  Write(OutputFileInits,fmtA) '! <ShadingProperty Reflectance>,Shading Surface Name,Shading Type,Diffuse Solar Reflectance, '//  &
     'Diffuse Visible Reflectance,Surface Glazing Fraction,Surface Glazing Contruction'

  DO SurfNum = 1, TotSurfaces
    IF (.not. (SurfaceTmp(SurfNum)%Class == SurfaceClass_Shading .or.      &
               SurfaceTmp(SurfNum)%Class == SurfaceClass_Detached_F .or.   &
               SurfaceTmp(SurfNum)%Class == SurfaceClass_Detached_B .or.   &
               SurfaceTmp(SurfNum)%Class == SurfaceClass_Overhang .or.     &
               SurfaceTmp(SurfNum)%Class == SurfaceClass_Fin) ) CYCLE
    IF (SurfaceTmp(SurfNum)%ShadowSurfGlazingConstruct /= 0) THEN
      Write(OutputFileInits,'(A)') 'ShadingProperty Reflectance,'//trim(SurfaceTmp(SurfNum)%Name)//','//  &
         trim(cSurfaceClass(SurfaceTmp(SurfNum)%class))//','//  &
         trim(RoundSigDigits(SurfaceTmp(SurfNum)%ShadowSurfDiffuseSolRefl,2))//','//  &
         trim(RoundSigDigits(SurfaceTmp(SurfNum)%ShadowSurfDiffuseVisRefl,2))//','//  &
         trim(RoundSigDigits(SurfaceTmp(SurfNum)%ShadowSurfGlazingFrac,2))//','//  &
         trim(Construct(SurfaceTmp(SurfNum)%ShadowSurfGlazingConstruct)%Name)
    ELSE
      Write(OutputFileInits,'(A)') 'ShadingProperty Reflectance,'//trim(SurfaceTmp(SurfNum)%Name)//','//  &
         trim(cSurfaceClass(SurfaceTmp(SurfNum)%class))//','//  &
         trim(RoundSigDigits(SurfaceTmp(SurfNum)%ShadowSurfDiffuseSolRefl,2))//','//  &
         trim(RoundSigDigits(SurfaceTmp(SurfNum)%ShadowSurfDiffuseVisRefl,2))//','//  &
         trim(RoundSigDigits(SurfaceTmp(SurfNum)%ShadowSurfGlazingFrac,2))//', N/A'
    ENDIF
  END DO


  RETURN

END SUBROUTINE GetShadingSurfReflectanceData

SUBROUTINE GetHTSurfExtVentedCavityData(ErrorsFound )

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         BGriffith
          !       DATE WRITTEN   January 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! load input data for Exterior Vented Cavity Special case for heat transfer surfaces

          ! METHODOLOGY EMPLOYED:
          ! usual E+ input processes

          ! REFERENCES:
          ! derived from SUBROUTINE GetTranspiredCollectorInput

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor,   ONLY: GetNumObjectsFound, GetObjectItem, GetObjectDefMaxArgs, FindItemInList , &
                              SameString, VerifyName
  USE General,          ONLY: TrimSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

       ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT)      :: ErrorsFound       ! Error flag indicator (true if errors found)

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER                        :: Item    ! Item to be "gotten"
  INTEGER                        :: NumAlphas  ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: MaxNumAlphas !argument for call to GetObjectDefMaxArgs
  INTEGER                        :: MaxNumNumbers !argument for call to GetObjectDefMaxArgs
  INTEGER                        :: Dummy !argument for call to GetObjectDefMaxArgs
  INTEGER                        :: IOStatus   ! Used in GetObjectItem
  INTEGER                        :: Found
  INTEGER                        :: AlphaOffset !local temp var
  CHARACTER(len=MaxNameLength)   :: Roughness
  INTEGER                        :: thisSurf  ! do loop counter
  REAL(r64)                      :: AvgAzimuth ! temp for error checking
  REAL(r64)                      :: AvgTilt    ! temp for error checking
  INTEGER                        :: SurfID  ! local surface "pointer"
  LOGICAL                        :: IsBlank
  LOGICAL                        :: ErrorInName

  cCurrentModuleObject='SurfaceProperty:ExteriorNaturalVentedCavity'
  CALL GetObjectDefMaxArgs(cCurrentModuleObject,Dummy, MaxNumAlphas,MaxNumNumbers)

  IF (MaxNumNumbers /= 8) THEN
    CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Object Definition indicates '// &
                         'not = 8 Number Objects, Number Indicated='//  &
                         TRIM(TrimSigDigits(MaxNumNumbers)))
    ErrorsFound=.true.
  ENDIF

  TotExtVentCav = GetNumObjectsFound(cCurrentModuleObject)

  ALLOCATE(ExtVentedCavity(TotExtVentCav))

  DO Item=1,TotExtVentCav
    CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    ! first handle cAlphaArgs
    ErrorInName=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),ExtVentedCavity%Name,Item-1,ErrorInName,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (ErrorInName) THEN
      CALL ShowContinueError('...cannot not duplicate other names')
      ErrorsFound=.true.
      CYCLE
    ENDIF
    ExtVentedCavity(Item)%Name     = cAlphaArgs(1)

    ExtVentedCavity(Item)%OSCMName = cAlphaArgs(2)
    IF (.not. lAlphaFieldBlanks(2)) THEN
      Found = FindItemInList(ExtVentedCavity(Item)%OSCMName,OSCM%Name,TotOSCM)
      IF (Found == 0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(ExtVentedCavity(Item)%Name)//  &
                               '", invalid '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
          ErrorsFound=.true.
      ENDIF
    ELSE
      Found=0
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(ExtVentedCavity(Item)%Name)//  &
                           '", invalid '//TRIM(cAlphaFieldNames(2))//' cannot be blank.')
      ErrorsFound=.true.
    ENDIF
    ExtVentedCavity(Item)%OSCMPtr = Found

    Roughness = cAlphaArgs(3)
    !Select the correct Number for the associated ascii name for the roughness type
    IF (SameString(Roughness,'VeryRough'))    ExtVentedCavity(Item)%BaffleRoughness=VeryRough
    IF (SameString(Roughness,'Rough'))        ExtVentedCavity(Item)%BaffleRoughness=Rough
    IF (SameString(Roughness,'MediumRough'))  ExtVentedCavity(Item)%BaffleRoughness=MediumRough
    IF (SameString(Roughness,'MediumSmooth')) ExtVentedCavity(Item)%BaffleRoughness=MediumSmooth
    IF (SameString(Roughness,'Smooth'))       ExtVentedCavity(Item)%BaffleRoughness=Smooth
    IF (SameString(Roughness,'VerySmooth'))   ExtVentedCavity(Item)%BaffleRoughness=VerySmooth

    ! Was it set?
    IF (ExtVentedCavity(Item)%BaffleRoughness == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(ExtVentedCavity(Item)%Name)//  &
                             '", invalid '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3)))
      ErrorsFound=.true.
    ENDIF

    AlphaOffset = 3
    ExtVentedCavity(Item)%NumSurfs = NumAlphas - AlphaOffset
    IF (ExtVentedCavity(Item)%NumSurfs == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(ExtVentedCavity(Item)%Name)//  &
                           '", no underlying surfaces specified. Must have at least one.')
      ErrorsFound = .true.
      CYCLE
    ENDIF
    ALLOCATE(ExtVentedCavity(Item)%SurfPtrs(ExtVentedCavity(Item)%NumSurfs))
    ExtVentedCavity(Item)%SurfPtrs = 0
    DO thisSurf = 1, ExtVentedCavity(Item)%NumSurfs
        Found = FindItemInList(cAlphaArgs(thisSurf + AlphaOffset), Surface%Name, TotSurfaces)
        If (Found == 0) Then
           CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(ExtVentedCavity(Item)%Name)//  &
                                '", invalid '//TRIM(cAlphaFieldNames(thisSurf + AlphaOffset))//  &
                                '="'//TRIM(cAlphaArgs(thisSurf + AlphaOffset)))
           ErrorsFound=.true.
           CYCLE
        ENDIF
        ! check that surface is appropriate, Heat transfer, Sun, Wind,
        IF (.not. surface(Found)%HeatTransSurf) then
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(ExtVentedCavity(Item)%Name)//  &
                                '", invalid '//TRIM(cAlphaFieldNames(thisSurf + AlphaOffset))//  &
                                '="'//TRIM(cAlphaArgs(thisSurf + AlphaOffset)))
            CALL ShowContinueError('...because it is not a Heat Transfer Surface.')
            ErrorsFound=.true.
            CYCLE
        ENDIF
        IF (.not. surface(found)%ExtSolar) then
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(ExtVentedCavity(Item)%Name)//  &
                                '", invalid '//TRIM(cAlphaFieldNames(thisSurf + AlphaOffset))//  &
                                '="'//TRIM(cAlphaArgs(thisSurf + AlphaOffset)))
            CALL ShowContinueError('...because it is not exposed to Sun.')
            ErrorsFound=.true.
            CYCLE
        ENDIF
        IF (.not. surface(found)%ExtWind) then
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(ExtVentedCavity(Item)%Name)//  &
                                '", invalid '//TRIM(cAlphaFieldNames(thisSurf + AlphaOffset))//  &
                                '="'//TRIM(cAlphaArgs(thisSurf + AlphaOffset)))
            CALL ShowContinueError('...because it is not exposed to Wind.')
            ErrorsFound=.true.
            CYCLE
        ENDIF
        If(surface(found)%ExtBoundCond /= OtherSideCondModeledExt) Then
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(ExtVentedCavity(Item)%Name)//  &
                                '", is invalid')
            CALL ShowContinueError('...because '//TRIM(cAlphaFieldNames(thisSurf + AlphaOffset))//  &
                                '="'//TRIM(cAlphaArgs(thisSurf + AlphaOffset))//'".')
            CALL ShowContinueError('...is not an OtherSideConditionedModel surface.')
            ErrorsFound=.true.
            CYCLE
        ENDIF
        ExtVentedCavity(Item)%SurfPtrs(thisSurf) = Found

        ! now set info in Surface structure
        Surface(Found)%ExtCavNum        = Item
        Surface(Found)%ExtCavityPresent = .true.

    ENDDO

    IF (ErrorsFound) CYCLE  ! previous inner do loop may have detected problems that need to be cycle'd again to avoid crash

    ! now that we should have all the surfaces, do some preperations and checks.

    ! are they all similar tilt and azimuth? Issue warnings so people can do it if they really want
    AvgAzimuth = SUM(Surface(ExtVentedCavity(Item)%SurfPtrs)%Azimuth * Surface(ExtVentedCavity(Item)%SurfPtrs)%Area) &
                /SUM(Surface(ExtVentedCavity(Item)%SurfPtrs)%Area)
    AvgTilt    = SUM(Surface(ExtVentedCavity(Item)%SurfPtrs)%Tilt * Surface(ExtVentedCavity(Item)%SurfPtrs)%Area) &
                /SUM(Surface(ExtVentedCavity(Item)%SurfPtrs)%Area)
    DO thisSurf = 1, ExtVentedCavity(Item)%NumSurfs
       SurfID = ExtVentedCavity(Item)%SurfPtrs(thisSurf)
       If (ABS(Surface(SurfID)%Azimuth - AvgAzimuth) > 15.d0 ) Then
            Call ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(ExtVentedCavity(Item)%Name)//  &
            ', Surface '//TRIM(Surface(SurfID)%name)//' has Azimuth different from others in '// &
            'the associated group.')
       ENDIF
       IF (ABS(Surface(SurfID)%Tilt - AvgTilt) > 10.d0 ) Then
            Call ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(ExtVentedCavity(Item)%Name)//  &
            ', Surface '//TRIM(Surface(SurfID)%name)//' has Tilt different from others in '// &
            'the associated group.')
       ENDIF

       !test that there are no windows.  Now allow windows
      ! If (Surface(SurfID)%GrossArea >  Surface(SurfID)%Area) Then
      !      Call ShowWarningError('Surface '//TRIM(Surface(SurfID)%name)//' has a subsurface whose area is not being ' &
      !         //'subtracted in the group of surfaces associated with '//TRIM(ExtVentedCavity(Item)%Name))
      ! endif

    ENDDO
    ExtVentedCavity(Item)%Tilt    = AvgTilt
    ExtVentedCavity(Item)%Azimuth = AvgAzimuth

    ! find area weighted centroid.
    ExtVentedCavity(Item)%Centroid%Z =   &
                SUM(Surface(ExtVentedCavity(Item)%SurfPtrs)%Centroid%Z*Surface(ExtVentedCavity(Item)%SurfPtrs)%Area) &
                            /SUM(Surface(ExtVentedCavity(Item)%SurfPtrs)%Area)

    !now handle rNumericArgs from input object
    ExtVentedCavity(Item)%Porosity      = rNumericArgs(1)
    ExtVentedCavity(Item)%LWEmitt       = rNumericArgs(2)
    ExtVentedCavity(Item)%SolAbsorp     = rNumericArgs(3)
    ExtVentedCavity(Item)%HdeltaNPL     = rNumericArgs(4)
    ExtVentedCavity(Item)%PlenGapThick  = rNumericArgs(5)
    IF (ExtVentedCavity(Item)%PlenGapThick <= 0.0d0) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(ExtVentedCavity(Item)%Name)//  &
                                '", invalid .')
         ErrorsFound=.true.
         CALL ShowContinueError('...because Plenum gap must be greater than Zero=['//  &
            TRIM(TrimSigDigits(rNumericArgs(5),2))//'].')
         CYCLE
    ENDIF
    ExtVentedCavity(Item)%AreaRatio     = rNumericArgs(6)
    ExtVentedCavity(Item)%Cv            = rNumericArgs(7)
    ExtVentedCavity(Item)%Cd            = rNumericArgs(8)

    ! Fill out data we now know
    ! sum areas of HT surface areas
    ExtVentedCavity(Item)%ProjArea      = SUM(Surface(ExtVentedCavity(Item)%SurfPtrs)%Area)
    IF (ExtVentedCavity(Item)%ProjArea <= 0.0d0) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(ExtVentedCavity(Item)%Name)//  &
                                '", invalid .')
         ErrorsFound=.true.
         CALL ShowContinueError('...because gross area of underlying surfaces must be greater than Zero=['//  &
            TRIM(TrimSigDigits(ExtVentedCavity(Item)%ProjArea,2))//'].')
         CYCLE
    endif
    ExtVentedCavity(Item)%ActualArea    = ExtVentedCavity(Item)%ProjArea * ExtVentedCavity(Item)%AreaRatio

    CALL SetupOutputVariable('Surface Exterior Cavity Baffle Surface Temperature [C]',ExtVentedCavity(Item)%Tbaffle, &
                               'System','Average',ExtVentedCavity(Item)%Name)
    CALL SetupOutputVariable('Surface Exterior Cavity Air Drybulb Temperature [C]',ExtVentedCavity(Item)%TAirCav, &
                               'System','Average',ExtVentedCavity(Item)%Name)
    CALL SetupOutputVariable('Surface Exterior Cavity Total Natural Ventilation Air Change Rate [ACH]', &
                              ExtVentedCavity(Item)%PassiveACH, &
                              'System','Average',ExtVentedCavity(Item)%Name)
    CALL SetupOutputVariable('Surface Exterior Cavity Total Natural Ventilation Mass Flow Rate [kg/s]', &
                              ExtVentedCavity(Item)%PassiveMdotVent, &
                               'System','Average',ExtVentedCavity(Item)%Name)
    CALL SetupOutputVariable('Surface Exterior Cavity Natural Ventilation from Wind Mass Flow Rate [kg/s]', &
                              ExtVentedCavity(Item)%PassiveMdotWind, &
                               'System','Average',ExtVentedCavity(Item)%Name)
    CALL SetupOutputVariable('Surface Exterior Cavity Natural Ventilation from Buoyancy Mass Flow Rate [kg/s]',  &
                               ExtVentedCavity(Item)%PassiveMdotTherm, &
                               'System','Average',ExtVentedCavity(Item)%Name)

  ENDDO

  RETURN

END SUBROUTINE GetHTSurfExtVentedCavityData

SUBROUTINE GetSurfaceHeatTransferAlgorithmOverrides(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith, portions from ApplyConvectionValue by Linda Lawrie
          !       DATE WRITTEN   July 2012
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! <description>

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortcuts
  USE InputProcessor,  ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList
  USE DataSurfaces,    ONLY: Surface
  USE DataHeatBalance, ONLY: HeatTransferAlgosUsed, NumberOfHeatTransferAlgosUsed &
                             ,LowHConvLimit, HighHConvLimit
  USE General,         ONLY: RoundSigDigits

  USE DataHeatBalSurface ,ONLY:MaxSurfaceTempLimit
  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:

  CHARACTER(len=*), PARAMETER :: fmtA="(A)"

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: CountHTAlgoObjectsSingleSurf
  INTEGER :: CountHTAlgoObjectsMultiSurf
  INTEGER :: CountHTAlgoObjectsSurfList
  INTEGER :: IOStatus   ! Used in GetObjectItem
  LOGICAL :: ErrorsFoundSingleSurf = .FALSE.
  LOGICAL :: ErrorsFoundMultiSurf  = .FALSE.
  LOGICAL :: ErrorsFoundSurfList   = .FALSE.
  LOGICAL :: ErrorsFoundByConstruct= .FALSE.
  INTEGER :: tmpAlgoInput
  INTEGER :: Item
  INTEGER :: Item1
  INTEGER :: NumAlphas
  INTEGER :: NumNumbers
  INTEGER :: Found
  LOGICAL :: SurfacesOfType
  INTEGER :: SurfNum
!  INTEGER :: Index
  INTEGER, DIMENSION(:), ALLOCATABLE :: tmpHeatTransferAlgosUsed
  INTEGER :: NumEMPDMat
  INTEGER :: NumPCMat
  INTEGER :: NumVTCMat
  INTEGER :: NumHAMTMat1
  INTEGER :: NumHAMTMat2
  INTEGER :: NumHAMTMat3
  INTEGER :: NumHAMTMat4
  INTEGER :: NumHAMTMat5
  INTEGER :: NumHAMTMat6
  INTEGER :: SumHAMTMat
  LOGICAL :: msgneeded
  CHARACTER(len=MaxNameLength) :: AlgoName


  ! first initialize each heat transfer surface with the overall model type, array assignment
  Surface%HeatTransferAlgorithm  = HeatTransferAlgosUsed(1)
!

  cCurrentModuleObject = 'SurfaceProperty:HeatTransferAlgorithm'
  CountHTAlgoObjectsSingleSurf = GetNumObjectsFound(cCurrentModuleObject)


  cCurrentModuleObject = 'SurfaceProperty:HeatTransferAlgorithm'
  DO Item=1, CountHTAlgoObjectsSingleSurf
    CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    ErrorsFoundSingleSurf = .FALSE.
    Found = FindItemInList(cAlphaArgs(1), Surface%Name,  TotSurfaces)

    IF (Found == 0) Then
      Call ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM( cAlphaArgs(1))//  &
                                '", did not find matching surface.')
      ErrorsFoundSingleSurf = .TRUE.

    ENDIF

    SELECT CASE (cAlphaArgs(2))

    CASE ('CONDUCTIONTRANSFERFUNCTION')
      tmpAlgoInput = HeatTransferModel_CTF
    CASE ('MOISTUREPENETRATIONDEPTHCONDUCTIONTRANSFERFUNCTION')
      tmpAlgoInput = HeatTransferModel_EMPD
    CASE ('COMBINEDHEATANDMOISTUREFINITEELEMENT')
      tmpAlgoInput = HeatTransferModel_HAMT
    CASE ('CONDUCTIONFINITEDIFFERENCE')
      tmpAlgoInput = HeatTransferModel_CondFD
    CASE DEFAULT
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                             '", invalid '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2)))
      ErrorsFoundSingleSurf = .TRUE.
    END SELECT

    IF (.NOT. ErrorsFoundSingleSurf) THEN
      Surface(Found)%HeatTransferAlgorithm   = tmpAlgoInput

      IF (.NOT. ANY(HeatTransferAlgosUsed == tmpAlgoInput)) THEN ! add new algo
        ALLOCATE(tmpHeatTransferAlgosUsed(NumberOfHeatTransferAlgosUsed))
        tmpHeatTransferAlgosUsed = HeatTransferAlgosUsed
        NumberOfHeatTransferAlgosUsed = NumberOfHeatTransferAlgosUsed + 1
        DEALLOCATE(HeatTransferAlgosUsed)
        ALLOCATE(HeatTransferAlgosUsed(NumberOfHeatTransferAlgosUsed))
        HeatTransferAlgosUsed(1:NumberOfHeatTransferAlgosUsed-1) = tmpHeatTransferAlgosUsed
        HeatTransferAlgosUsed(NumberOfHeatTransferAlgosUsed) = tmpAlgoInput
        DEALLOCATE(tmpHeatTransferAlgosUsed)
      ENDIF

    ELSE
      ErrorsFound = .TRUE.
    ENDIF
  ENDDO  ! single surface heat transfer algorithm override


  cCurrentModuleObject = 'SurfaceProperty:HeatTransferAlgorithm:MultipleSurface'
  CountHTAlgoObjectsMultiSurf = GetNumObjectsFound(cCurrentModuleObject)

  DO Item=1, CountHTAlgoObjectsMultiSurf
    CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    ErrorsFoundMultiSurf = .FALSE.
    SELECT CASE (cAlphaArgs(3))

    CASE ('CONDUCTIONTRANSFERFUNCTION')
      tmpAlgoInput = HeatTransferModel_CTF
    CASE ('MOISTUREPENETRATIONDEPTHCONDUCTIONTRANSFERFUNCTION')
      tmpAlgoInput = HeatTransferModel_EMPD
    CASE ('COMBINEDHEATANDMOISTUREFINITEELEMENT')
      tmpAlgoInput = HeatTransferModel_HAMT
    CASE ('CONDUCTIONFINITEDIFFERENCE')
      tmpAlgoInput = HeatTransferModel_CondFD
    CASE DEFAULT
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                             '", invalid '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3)))
      ErrorsFoundMultiSurf = .TRUE.
    END SELECT

    SELECT CASE(cAlphaArgs(2))

      CASE ('ALLEXTERIORSURFACES')
        SurfacesOfType=.false.
        DO SurfNum=1,TotSurfaces
          IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
          IF (Surface(SurfNum)%ExtBoundCond > 0) CYCLE    ! Interior surfaces
          IF (Construct(Surface(SurfNum)%Construction)%TypeIsWindow) CYCLE
          SurfacesOfType=.true.
          Surface(SurfNum)%HeatTransferAlgorithm   = tmpAlgoInput
        ENDDO

      CASE ('ALLEXTERIORWALLS')
        SurfacesOfType=.false.
        DO SurfNum=1,TotSurfaces
          IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
          IF (Surface(SurfNum)%ExtBoundCond > 0) CYCLE    ! Interior surfaces
          IF (.not. Surface(SurfNum)%Class == SurfaceClass_Wall) CYCLE
          IF (Construct(Surface(SurfNum)%Construction)%TypeIsWindow) CYCLE
          SurfacesOfType=.true.
          Surface(SurfNum)%HeatTransferAlgorithm   = tmpAlgoInput
       ENDDO

      CASE('ALLEXTERIORROOFS')
        SurfacesOfType=.false.
        DO SurfNum=1,TotSurfaces
          IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
          IF (Surface(SurfNum)%ExtBoundCond > 0) CYCLE    ! Interior surfaces
          IF (.not. Surface(SurfNum)%Class == SurfaceClass_Roof) CYCLE
          IF (Construct(Surface(SurfNum)%Construction)%TypeIsWindow) CYCLE
          SurfacesOfType=.true.
          Surface(SurfNum)%HeatTransferAlgorithm   = tmpAlgoInput
        ENDDO

      CASE('ALLEXTERIORFLOORS')
        SurfacesOfType=.false.
        DO SurfNum=1,TotSurfaces
          IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
          IF (Surface(SurfNum)%ExtBoundCond > 0) CYCLE    ! Interior surfaces
          IF (.not. Surface(SurfNum)%Class == SurfaceClass_Floor) CYCLE
          IF (Construct(Surface(SurfNum)%Construction)%TypeIsWindow) CYCLE
          SurfacesOfType=.true.
          Surface(SurfNum)%HeatTransferAlgorithm   = tmpAlgoInput
        ENDDO

      CASE ('ALLGROUNDCONTACTSURFACES')
        SurfacesOfType=.false.
        DO SurfNum=1,TotSurfaces
          IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
          IF (Surface(SurfNum)%ExtBoundCond /= Ground) CYCLE    ! ground BC
          IF (Construct(Surface(SurfNum)%Construction)%TypeIsWindow) CYCLE
          SurfacesOfType=.true.
          Surface(SurfNum)%HeatTransferAlgorithm   = tmpAlgoInput
        ENDDO
      CASE ('ALLINTERIORSURFACES')
        SurfacesOfType=.false.
        DO SurfNum=1,TotSurfaces
          IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
          IF (Surface(SurfNum)%ExtBoundCond <= 0) CYCLE    ! Exterior surfaces
          IF (Construct(Surface(SurfNum)%Construction)%TypeIsWindow) CYCLE
          SurfacesOfType=.true.
          Surface(SurfNum)%HeatTransferAlgorithm   = tmpAlgoInput
        ENDDO

      CASE ('ALLINTERIORWALLS')
        SurfacesOfType=.false.
        DO SurfNum=1,TotSurfaces
          IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
          IF (Surface(SurfNum)%ExtBoundCond <= 0) CYCLE    ! Exterior surfaces
          IF (.not. Surface(SurfNum)%Class == SurfaceClass_Wall) CYCLE
          IF (Construct(Surface(SurfNum)%Construction)%TypeIsWindow) CYCLE
          SurfacesOfType=.true.
          Surface(SurfNum)%HeatTransferAlgorithm   = tmpAlgoInput
        ENDDO

      CASE('ALLINTERIORROOFS','ALLINTERIORCEILINGS')
        SurfacesOfType=.false.
        DO SurfNum=1,TotSurfaces
          IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
          IF (Surface(SurfNum)%ExtBoundCond <= 0) CYCLE    ! Exterior surfaces
          IF (.not. Surface(SurfNum)%Class == SurfaceClass_Roof) CYCLE
          IF (Construct(Surface(SurfNum)%Construction)%TypeIsWindow) CYCLE
          SurfacesOfType=.true.
          Surface(SurfNum)%HeatTransferAlgorithm   = tmpAlgoInput
        ENDDO

      CASE('ALLINTERIORFLOORS')
        SurfacesOfType=.false.
        DO SurfNum=1,TotSurfaces
          IF (.not. Surface(SurfNum)%HeatTransSurf) CYCLE
          IF (Surface(SurfNum)%ExtBoundCond <= 0) CYCLE    ! Exterior surfaces
          IF (.not. Surface(SurfNum)%Class == SurfaceClass_Floor) CYCLE
          IF (Construct(Surface(SurfNum)%Construction)%TypeIsWindow) CYCLE
          SurfacesOfType=.true.
          Surface(SurfNum)%HeatTransferAlgorithm   = tmpAlgoInput
        ENDDO
      CASE DEFAULT
        SurfacesOfType=.false.
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                             '", invalid '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2)))
        ErrorsFoundMultiSurf = .TRUE.
    END SELECT

    IF (.not. SurfacesOfType) THEN
      CALL ShowWarningError('In '//TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))// &
         '", for Multiple Surface Assignment="'//trim(cAlphaArgs(2))//  &
         '", there were no surfaces of that type found for assignment.')
    ELSE
      IF (.NOT. ANY(HeatTransferAlgosUsed == tmpAlgoInput)) THEN ! add new algo
        ALLOCATE(tmpHeatTransferAlgosUsed(NumberOfHeatTransferAlgosUsed))
        tmpHeatTransferAlgosUsed = HeatTransferAlgosUsed
        NumberOfHeatTransferAlgosUsed = NumberOfHeatTransferAlgosUsed + 1
        DEALLOCATE(HeatTransferAlgosUsed)
        ALLOCATE(HeatTransferAlgosUsed(NumberOfHeatTransferAlgosUsed))
        HeatTransferAlgosUsed(1:NumberOfHeatTransferAlgosUsed-1) = tmpHeatTransferAlgosUsed
        HeatTransferAlgosUsed(NumberOfHeatTransferAlgosUsed) = tmpAlgoInput
        DEALLOCATE(tmpHeatTransferAlgosUsed)
      ENDIF
    ENDIF
    IF (ErrorsFoundMultiSurf) ErrorsFound = .TRUE.


  ENDDO ! multi surface heat transfer algo override


  cCurrentModuleObject = 'SurfaceProperty:HeatTransferAlgorithm:SurfaceList'
  CountHTAlgoObjectsSurfList = GetNumObjectsFound(cCurrentModuleObject)
  DO Item=1, CountHTAlgoObjectsSurfList
    CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    ErrorsFoundSurfList = .FALSE.
    SELECT CASE (cAlphaArgs(2))

    CASE ('CONDUCTIONTRANSFERFUNCTION')
      tmpAlgoInput = HeatTransferModel_CTF
    CASE ('MOISTUREPENETRATIONDEPTHCONDUCTIONTRANSFERFUNCTION')
      tmpAlgoInput = HeatTransferModel_EMPD
    CASE ('COMBINEDHEATANDMOISTUREFINITEELEMENT')
      tmpAlgoInput = HeatTransferModel_HAMT
    CASE ('CONDUCTIONFINITEDIFFERENCE')
      tmpAlgoInput = HeatTransferModel_CondFD
    CASE DEFAULT
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                             '", invalid '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2)))
      ErrorsFoundSurfList = .TRUE.
    END SELECT


    Do item1 = 3, NumAlphas

      Found = FinditemInList(cAlphaArgs(item1), Surface%Name,  TotSurfaces)

      IF (Found == 0) Then
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM( cAlphaArgs(1))//  &
                                '", did not find matching surface.')
        CALL ShowContinueError('Name of surface not found = "'//TRIM(cAlphaArgs(item1))//'"')
        ErrorsFoundSurfList = .TRUE.

      ENDIF

      IF (.NOT. ErrorsFoundSurfList) THEN
        Surface(Found)%HeatTransferAlgorithm   = tmpAlgoInput
        IF (.NOT. ANY(HeatTransferAlgosUsed == tmpAlgoInput)) THEN ! add new algo
          ALLOCATE(tmpHeatTransferAlgosUsed(NumberOfHeatTransferAlgosUsed))
          tmpHeatTransferAlgosUsed = HeatTransferAlgosUsed
          NumberOfHeatTransferAlgosUsed = NumberOfHeatTransferAlgosUsed + 1
          DEALLOCATE(HeatTransferAlgosUsed)
          ALLOCATE(HeatTransferAlgosUsed(NumberOfHeatTransferAlgosUsed))
          HeatTransferAlgosUsed(1:NumberOfHeatTransferAlgosUsed-1) = tmpHeatTransferAlgosUsed
          HeatTransferAlgosUsed(NumberOfHeatTransferAlgosUsed) = tmpAlgoInput
          DEALLOCATE(tmpHeatTransferAlgosUsed)
        ENDIF
      ELSE
        ErrorsFound = .TRUE.
      ENDIF

    ENDDO

  ENDDO

  cCurrentModuleObject = 'SurfaceProperty:HeatTransferAlgorithm:Construction'
  CountHTAlgoObjectsSurfList = GetNumObjectsFound(cCurrentModuleObject)
  DO Item=1, CountHTAlgoObjectsSurfList
    CALL GetObjectItem(cCurrentModuleObject,Item,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    ErrorsFoundByConstruct = .FALSE.
    SELECT CASE (cAlphaArgs(2))

    CASE ('CONDUCTIONTRANSFERFUNCTION')
      tmpAlgoInput = HeatTransferModel_CTF
    CASE ('MOISTUREPENETRATIONDEPTHCONDUCTIONTRANSFERFUNCTION')
      tmpAlgoInput = HeatTransferModel_EMPD
    CASE ('COMBINEDHEATANDMOISTUREFINITEELEMENT')
      tmpAlgoInput = HeatTransferModel_HAMT
    CASE ('CONDUCTIONFINITEDIFFERENCE')
      tmpAlgoInput = HeatTransferModel_CondFD
    CASE DEFAULT
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                             '", invalid '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2)))
      ErrorsFoundByConstruct = .TRUE.
    END SELECT

    Found = 0
    Found = FindItemInList(cAlphaArgs(3), Construct%Name,  TotConstructs )
    IF (Found == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                             '", invalid '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3)))
      ErrorsFoundByConstruct = .TRUE.
    ENDIF

    IF (.NOT. ErrorsFoundByConstruct ) THEN
      DO item1=1, TotSurfaces
        IF (Surface(item1)%Construction == Found) THEN
          Surface(item1)%HeatTransferAlgorithm   = tmpAlgoInput
          IF (.NOT. ANY(HeatTransferAlgosUsed == tmpAlgoInput)) THEN ! add new algo
            ALLOCATE(tmpHeatTransferAlgosUsed(NumberOfHeatTransferAlgosUsed))
            tmpHeatTransferAlgosUsed = HeatTransferAlgosUsed
            NumberOfHeatTransferAlgosUsed = NumberOfHeatTransferAlgosUsed + 1
            DEALLOCATE(HeatTransferAlgosUsed)
            ALLOCATE(HeatTransferAlgosUsed(NumberOfHeatTransferAlgosUsed))
            HeatTransferAlgosUsed(1:NumberOfHeatTransferAlgosUsed-1) = tmpHeatTransferAlgosUsed
            HeatTransferAlgosUsed(NumberOfHeatTransferAlgosUsed) = tmpAlgoInput
            DEALLOCATE(tmpHeatTransferAlgosUsed)
          ENDIF
        ENDIF
      ENDDO
    ENDIF
  ENDDO

  ! test for missing materials for algorithms selected
  NumEMPDMat=GetNumObjectsFound('MaterialProperty:MoisturePenetrationDepth:Settings')
  NumPCMat=GetNumObjectsFound('MaterialProperty:PhaseChange') ! needs detailed algo
  NumVTCMat=GetNumObjectsFound('MaterialProperty:VariableThermalConductivity')
  NumHAMTMat1=GetNumObjectsFound('MaterialProperty:HeatAndMoistureTransfer:Settings')
  NumHAMTMat2=GetNumObjectsFound('MaterialProperty:HeatAndMoistureTransfer:SorptionIsotherm')
  NumHAMTMat3=GetNumObjectsFound('MaterialProperty:HeatAndMoistureTransfer:Suction')
  NumHAMTMat4=GetNumObjectsFound('MaterialProperty:HeatAndMoistureTransfer:Redistribution')
  NumHAMTMat5=GetNumObjectsFound('MaterialProperty:HeatAndMoistureTransfer:Diffusion')
  NumHAMTMat6=GetNumObjectsFound('MaterialProperty:HeatAndMoistureTransfer:ThermalConductivity')
  SumHAMTMat=NumHAMTMat1+NumHAMTMat2+NumHAMTMat3+NumHAMTMat4+NumHAMTMat5+NumHAMTMat6
  msgneeded=.false.

  IF (NumEMPDMat > 0 .AND. .NOT. ANY(HeatTransferAlgosUsed == HeatTransferModel_EMPD)) THEN
    CALL ShowWarningError('The input file includes '// &
           TRIM(RoundSigDigits(NumEMPDMat))//' MaterialProperty:MoisturePenetrationDepth:Settings objects' &
           // ' but the moisture penetration depth algorithm is not used anywhere.')
    msgneeded=.true.
  ENDIF
  IF (NumPCMat > 0 .AND. .NOT. ANY(HeatTransferAlgosUsed == HeatTransferModel_CondFD)) THEN
    CALL ShowWarningError('The input file includes '// &
        TRIM(RoundSigDigits(NumPCMat))//' MaterialProperty:PhaseChange objects' &
        // ' but the conduction finite difference algorithm is not used anywhere.')
    msgneeded=.true.
  ENDIF
  IF (NumVTCMat > 0 .AND. .NOT. ANY(HeatTransferAlgosUsed == HeatTransferModel_CondFD)) THEN
    CALL ShowWarningError('The input file includes '// &
        TRIM(RoundSigDigits(NumVTCMat))//' MaterialProperty:VariableThermalConductivity objects' &
        // ' but the conduction finite difference algorithm is not used anywhere.')
    msgneeded=.true.
  ENDIF
  IF (SumHAMTMat > 0 .AND. .NOT. ANY(HeatTransferAlgosUsed == HeatTransferModel_HAMT )) THEN
    CALL ShowWarningError('The input file includes '// &
        trim(RoundSigDigits(SumHAMTMat))//' MaterialProperty:HeatAndMoistureTransfer:* objects' &
        // ' but the combined heat and moisture finite difference algorithm is not used anywhere.')
    msgneeded=.true.
  ENDIF
  IF (msgneeded) THEN
    CALL ShowContinueError('Previous materials will be ignored due to HeatBalanceAlgorithm choice.')
  ENDIF
  msgneeded = .FALSE.
  IF (NumEMPDMat == 0 .AND. ANY(HeatTransferAlgosUsed == HeatTransferModel_EMPD)) THEN
    CALL ShowWarningError('The moisture penetration depth conduction transfer function algorithm is used' &
               // ' but the input file includes no MaterialProperty:MoisturePenetrationDepth:Settings objects.')
    msgneeded=.TRUE.
  ENDIF
  IF (SumHAMTMat == 0 .AND. ANY(HeatTransferAlgosUsed == HeatTransferModel_HAMT ) ) THEN
    CALL ShowWarningError('The combined heat and moisture finite element algorithm is used but the input file includes '// &
                            ' no MaterialProperty:HeatAndMoistureTransfer:* objects.')
    msgneeded=.TRUE.
  ENDIF
  IF (msgneeded) THEN
    CALL ShowContinueError('Certain materials objects are necessary to achieve proper results with the heat transfer ' &
                            // 'algorithm(s) selected.')
  ENDIF


     ! Write Solution Algorithm to the initialization output file for User Verification
   Write(OutputFileInits,fmtA) '! <Surface Heat Transfer Algorithm>, Value {CTF - ConductionTransferFunction | '//  &
           'EMPD - MoisturePenetrationDepthConductionTransferFunction | '// &
           'CondFD - ConductionFiniteDifference | '// &
           'HAMT - CombinedHeatAndMoistureFiniteElement} - Description,Inside Surface Max Temperature Limit{C}, ' // &
           'Surface Convection Coefficient Lower Limit {W/m2-K}, Surface Convection Coefficient Upper Limit {W/m2-K}'

   DO item1 = 1, NumberOfHeatTransferAlgosUsed
     AlgoName = ' '
     SELECT CASE (HeatTransferAlgosUsed(item1))

     CASE (HeatTransferModel_CTF)
       AlgoName = 'CTF - ConductionTransferFunction'
     CASE (HeatTransferModel_CondFD)
       AlgoName = 'CondFD - ConductionFiniteDifference'
     CASE (HeatTransferModel_EMPD)
       AlgoName = 'EMPD - MoisturePenetrationDepthConductionTransferFunction'
     CASE (HeatTransferModel_HAMT)
       AlgoName = 'HAMT - CombinedHeatAndMoistureFiniteElement'
     END SELECT

     WRITE(OutputFileInits,725) TRIM(AlgoName),TRIM(RoundSigDigits(MaxSurfaceTempLimit,0)), &
            TRIM(RoundSigDigits(LowHConvLimit, 2)), TRIM(RoundSigDigits(HighHConvLimit, 1))
   ENDDO

   725 Format('Surface Heat Transfer Algorithm, ',A,',',A, ',', A, ',', A)

   !Assign model type to windows, shading surfaces, and TDDs
   DO item=1, TotSurfaces
     IF (Surface(item)%Class == SurfaceClass_Window .OR. &
         Surface(item)%Class == SurfaceClass_GlassDoor) THEN
       ! todo, add complex fenestration switch  HeatTransferModel_ComplexFenestration
       IF (SurfaceWindow(item)%WindowModelType == WindowBSDFModel) THEN
         Surface(item)%HeatTransferAlgorithm = HeatTransferModel_ComplexFenestration
       ELSE
         Surface(item)%HeatTransferAlgorithm = HeatTransferModel_Window5
       ENDIF
     ENDIF
     IF (Surface(item)%Class == SurfaceClass_Detached_B .OR. &
         Surface(item)%Class == SurfaceClass_Detached_F .OR. &
         Surface(item)%Class == SurfaceClass_Shading  .OR. &
         Surface(item)%Class == SurfaceClass_Overhang .OR. &
         Surface(item)%Class == SurfaceClass_Fin) THEN
       Surface(item)%HeatTransferAlgorithm = HeatTransferModel_None
     ENDIF
     IF ( Surface(item)%Class == SurfaceClass_TDD_Diffuser) THEN
       Surface(item)%HeatTransferAlgorithm = HeatTransferModel_TDD
     ENDIF

   ENDDO


  RETURN

END SUBROUTINE GetSurfaceHeatTransferAlgorithmOverrides

SUBROUTINE GetVertices(SurfNum,NSides,Vertices)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the surface vertices from the arrays
          ! passed by the calling routine.  These had previously been obtained
          ! from the InputProcessor (GetObjectItem).  This routine will provide
          ! a standard place for determining various properties of the surface
          ! from the vertices.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Vectors
  USE General, ONLY: RoundSigDigits
  USE DataErrorTracking

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)                     :: SurfNum     ! Current surface number
  INTEGER, INTENT(IN)                     :: NSides      ! Number of sides to figure
  REAL(r64), INTENT(IN), DIMENSION(1:NSides*3) :: Vertices    ! Vertices, in specified order

          ! SUBROUTINE PARAMETER DEFINITIONS:
    CHARACTER(len=*), PARAMETER :: RoutineName='GetVertices: '
    character(len=*), parameter :: fmt3="(A,I5,A,3(1x,f18.13))"
    TYPE (vector), PARAMETER     :: TestVector=vector(0.0d0,0.0d0,1.0d0)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER Ptr  ! Pointer into Vertice array
    INTEGER N    ! Loop counter
    INTEGER NSrc   ! Used for CW -> CCW transformation
    INTEGER NTar   ! Used for CW -> CCW transformation
    REAL(r64) SurfWorldAz
    REAL(r64) SurfTilt
    REAL(r64)     :: Perimeter       ! Perimeter length of the surface
    INTEGER Vrt   ! Used for calculating perimeter
    TYPE (vector) :: temp
    REAL(r64)   :: Xb                 ! Intermediate calculation
    REAL(r64)   :: Yb                 ! Intermediate calculation
    INTEGER :: ZoneNum
    INTEGER :: ThisCorner
    CHARACTER(len=10) :: TiltString
    REAL(r64) ThisWidth
    REAL(r64) ThisHeight
    REAL(r64) :: DistanceCheck
!unused    REAL(r64) :: ccwtest
!unused    LOGICAL   :: SurfaceCCW
    REAL(r64) :: dotp

    IF (NSides > MaxVerticesPerSurface) MaxVerticesPerSurface=NSides
    Ptr=1
    DO N=1,NSides
      SurfaceTmp(SurfNum)%Vertex(N)%X=Vertices(Ptr)
      Ptr=Ptr+1
      SurfaceTmp(SurfNum)%Vertex(N)%Y=Vertices(Ptr)
      Ptr=Ptr+1
      SurfaceTmp(SurfNum)%Vertex(N)%Z=Vertices(Ptr)
      Ptr=Ptr+1
    ENDDO

    ! Address changing vertices if they were put in in CW order rather than CCW
    IF (.not. CCW) THEN
      ! If even number of sides, this will transfer appropriately
      ! If odd number, will leave the "odd" one, which is what you want.
      NSrc=NSides
      NTar=2
      DO N=1,(NSides-1)/2
        temp=SurfaceTmp(SurfNum)%Vertex(NSrc)
        SurfaceTmp(SurfNum)%Vertex(NSrc)=SurfaceTmp(SurfNum)%Vertex(NTar)
        SurfaceTmp(SurfNum)%Vertex(NTar)=temp
        NSrc=NSrc-1
        NTar=NTar+1
      ENDDO
    ENDIF
    ! Now address which "Corner" has been put in first.  Note: the azimuth and tilt and area
    ! calculations do not care which corner is put in first.
    ! 2/2011 - don't think the shading calculations have a corner preference.  Will keep this for
    ! consistency (for now)
    ThisCorner=Corner
    DO WHILE (ThisCorner /= UpperLeftCorner)
      IF (NSides < 4) THEN
        IF (ThisCorner == UpperRightCorner) THEN
          ThisCorner=UpperLeftCorner
          EXIT
        ENDIF
      ENDIF
      NTar=ThisCorner
      NSrc=ThisCorner+1
      IF (NSrc > NSides) NSrc=1
      DO N=1,NSides-1
        temp=SurfaceTmp(SurfNum)%Vertex(NTar)
        SurfaceTmp(SurfNum)%Vertex(NTar)=SurfaceTmp(SurfNum)%Vertex(NSrc)
        SurfaceTmp(SurfNum)%Vertex(NSrc)=temp
        NTar=NTar+1
        NSrc=NSrc+1
        IF (NTar > NSides) NTar=1
        IF (NSrc > NSides) NSrc=1
      ENDDO
      ThisCorner=ThisCorner+1
      IF (ThisCorner > NSides) ThisCorner=1
    ENDDO ! Corners
    IF (.not. WorldCoordSystem) THEN
      ! Input in "relative" coordinates, use Building and Zone North Axes and Origins
      !                                  to translate each point (including rotation for Appendix G)
      ZoneNum=SurfaceTmp(SurfNum)%Zone
      IF (ZoneNum > 0) THEN
        DO N=1,NSides
          Xb    = SurfaceTmp(SurfNum)%Vertex(N)%X*CosZoneRelNorth(ZoneNum) &
                 -SurfaceTmp(SurfNum)%Vertex(N)%Y*SinZoneRelNorth(ZoneNum) + Zone(ZoneNum)%OriginX
          Yb    = SurfaceTmp(SurfNum)%Vertex(N)%X*SinZoneRelNorth(ZoneNum) &
                 +SurfaceTmp(SurfNum)%Vertex(N)%Y*CosZoneRelNorth(ZoneNum) + Zone(ZoneNum)%OriginY
          SurfaceTmp(SurfNum)%Vertex(N)%X  = Xb*CosBldgRelNorth - Yb*SinBldgRelNorth
          SurfaceTmp(SurfNum)%Vertex(N)%Y  = Xb*SinBldgRelNorth + Yb*CosBldgRelNorth
          SurfaceTmp(SurfNum)%Vertex(N)%Z  = SurfaceTmp(SurfNum)%Vertex(N)%Z + Zone(ZoneNum)%OriginZ
        ENDDO
      ELSEIF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Detached_B) THEN
        DO N=1,NSides
          Xb    = SurfaceTmp(SurfNum)%Vertex(N)%X
          Yb    = SurfaceTmp(SurfNum)%Vertex(N)%Y
          SurfaceTmp(SurfNum)%Vertex(N)%X  = Xb*CosBldgRelNorth - Yb*SinBldgRelNorth
          SurfaceTmp(SurfNum)%Vertex(N)%Y  = Xb*SinBldgRelNorth + Yb*CosBldgRelNorth
        ENDDO
      ENDIF
    ELSE
      !if world coordinate only need to rotate for Appendix G
      ZoneNum=SurfaceTmp(SurfNum)%Zone
      IF (ZoneNum > 0) THEN
        DO N=1,NSides
          Xb    = SurfaceTmp(SurfNum)%Vertex(N)%X
          Yb    = SurfaceTmp(SurfNum)%Vertex(N)%Y
          SurfaceTmp(SurfNum)%Vertex(N)%X  = Xb*CosBldgRotAppGonly - Yb*SinBldgRotAppGonly
          SurfaceTmp(SurfNum)%Vertex(N)%Y  = Xb*SinBldgRotAppGonly + Yb*CosBldgRotAppGonly
        ENDDO
      ELSEIF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Detached_B) THEN
        DO N=1,NSides
          Xb    = SurfaceTmp(SurfNum)%Vertex(N)%X
          Yb    = SurfaceTmp(SurfNum)%Vertex(N)%Y
          SurfaceTmp(SurfNum)%Vertex(N)%X  = Xb*CosBldgRotAppGonly - Yb*SinBldgRotAppGonly
          SurfaceTmp(SurfNum)%Vertex(N)%Y  = Xb*SinBldgRotAppGonly + Yb*CosBldgRotAppGonly
        ENDDO
      END IF
    ENDIF

    IF (NSides > 2) THEN
      DistanceCheck=Distance(SurfaceTmp(SurfNum)%Vertex(SurfaceTmp(SurfNum)%Sides),SurfaceTmp(SurfNum)%Vertex(1))
      IF (DistanceCheck < .01d0) THEN
        IF (DisplayExtraWarnings) THEN
          CALL ShowWarningError(RoutineName//'Distance between two vertices < .01, possibly coincident.'//  &
                                ' for Surface='//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                                ', in Zone='//TRIM(SurfaceTmp(SurfNum)%ZoneName))
          CALL ShowContinueError('Vertex ['//trim(RoundSigDigits(SurfaceTmp(SurfNum)%Sides))//  &
             ']=('//trim(RoundSigDigits(SurfaceTmp(SurfNum)%Vertex(SurfaceTmp(SurfNum)%Sides)%x,2))//','//          &
             trim(RoundSigDigits(SurfaceTmp(SurfNum)%Vertex(SurfaceTmp(SurfNum)%Sides)%y,2))//','//                 &
             trim(RoundSigDigits(SurfaceTmp(SurfNum)%Vertex(SurfaceTmp(SurfNum)%Sides)%z,2))//')')
          CALL ShowContinueError('Vertex ['//trim(RoundSigDigits(1))//  &
             ']=('//trim(RoundSigDigits(SurfaceTmp(SurfNum)%Vertex(1)%x,2))//','//          &
             trim(RoundSigDigits(SurfaceTmp(SurfNum)%Vertex(1)%y,2))//','//                 &
             trim(RoundSigDigits(SurfaceTmp(SurfNum)%Vertex(1)%z,2))//')')
        ENDIF
        TotalCoincidentVertices=TotalCoincidentVertices+1
        IF (SurfaceTmp(SurfNum)%Sides > 3) THEN
          IF (DisplayExtraWarnings) THEN
            CALL ShowContinueError('Dropping Vertex ['//trim(RoundSigDigits(SurfaceTmp(SurfNum)%Sides))//'].')
          ENDIF
          SurfaceTmp(SurfNum)%Sides=SurfaceTmp(SurfNum)%Sides-1
        ELSE
          IF (DisplayExtraWarnings) THEN
            CALL ShowContinueError('Cannot Drop Vertex ['//trim(RoundSigDigits(SurfaceTmp(SurfNum)%Sides))//']; '//  &
                 'Number of Surface Sides at minimum. This surface is now a degenerate surface.')
          ENDIF
          TotalDegenerateSurfaces=TotalDegenerateSurfaces+1
          ! mark degenerate surface?
        ENDIF
        DistanceCheck=0.0d0
      ENDIF
      Perimeter=DistanceCheck
!      DO Vrt=2,SurfaceTmp(SurfNum)%Sides
      Vrt=2
      DO
        DistanceCheck=Distance(SurfaceTmp(SurfNum)%Vertex(Vrt),SurfaceTmp(SurfNum)%Vertex(Vrt-1))
        IF (DistanceCheck < .01d0) THEN
          IF (DisplayExtraWarnings) THEN
            CALL ShowWarningError(RoutineName//'Distance between two vertices < .01, possibly coincident.'//  &
                                  ' for Surface='//TRIM(SurfaceTmp(SurfNum)%Name)//  &
                                  ', in Zone='//TRIM(SurfaceTmp(SurfNum)%ZoneName))
            CALL ShowContinueError('Vertex ['//trim(RoundSigDigits(Vrt))//  &
               ']=('//trim(RoundSigDigits(SurfaceTmp(SurfNum)%Vertex(Vrt)%x,2))//','//          &
               trim(RoundSigDigits(SurfaceTmp(SurfNum)%Vertex(Vrt)%y,2))//','//                 &
               trim(RoundSigDigits(SurfaceTmp(SurfNum)%Vertex(Vrt)%z,2))//')')
            CALL ShowContinueError('Vertex ['//trim(RoundSigDigits(Vrt-1))//  &
               ']=('//trim(RoundSigDigits(SurfaceTmp(SurfNum)%Vertex(Vrt-1)%x,2))//','//          &
               trim(RoundSigDigits(SurfaceTmp(SurfNum)%Vertex(Vrt-1)%y,2))//','//                 &
               trim(RoundSigDigits(SurfaceTmp(SurfNum)%Vertex(Vrt-1)%z,2))//')')
          ENDIF
          TotalCoincidentVertices=TotalCoincidentVertices+1
          IF (Vrt == SurfaceTmp(SurfNum)%Sides) THEN
            IF (SurfaceTmp(SurfNum)%Sides > 3) THEN
              IF (DisplayExtraWarnings) THEN
                CALL ShowContinueError('Dropping Vertex ['//trim(RoundSigDigits(SurfaceTmp(SurfNum)%Sides))//'].')
              ENDIF
              SurfaceTmp(SurfNum)%Sides=SurfaceTmp(SurfNum)%Sides-1
            ELSE
              IF (DisplayExtraWarnings) THEN
                CALL ShowContinueError('Cannot Drop Vertex ['//trim(RoundSigDigits(SurfaceTmp(SurfNum)%Sides))//']; '//  &
                     'Number of Surface Sides at minimum. This surface is now a degenerate surface.')
              ENDIF
              TotalDegenerateSurfaces=TotalDegenerateSurfaces+1
              ! mark degenerate surface?
            ENDIF
            DistanceCheck=0.0d0
          ELSE
            IF (SurfaceTmp(SurfNum)%Sides > 3) THEN
              IF (DisplayExtraWarnings) THEN
                CALL ShowContinueError('Dropping Vertex ['//trim(RoundSigDigits(Vrt))//'].')
              ENDIF
              DO N=Vrt,SurfaceTmp(SurfNum)%Sides-1
                SurfaceTmp(SurfNum)%Vertex(N)%x=SurfaceTmp(SurfNum)%Vertex(N+1)%x
                SurfaceTmp(SurfNum)%Vertex(N)%y=SurfaceTmp(SurfNum)%Vertex(N+1)%y
                SurfaceTmp(SurfNum)%Vertex(N)%z=SurfaceTmp(SurfNum)%Vertex(N+1)%z
              ENDDO
              SurfaceTmp(SurfNum)%Sides=SurfaceTmp(SurfNum)%Sides-1
            ELSE
              IF (DisplayExtraWarnings) THEN
                CALL ShowContinueError('Cannot Drop Vertex ['//trim(RoundSigDigits(SurfaceTmp(SurfNum)%Sides))//']; '//  &
                   'Number of Surface Sides at minimum. This surface is now a degenerate surface.')
              ENDIF
              TotalDegenerateSurfaces=TotalDegenerateSurfaces+1
              ! mark degenerate surface?
            ENDIF
            DistanceCheck=0.0d0
          ENDIF
        ENDIF
        Perimeter = Perimeter+DistanceCheck
        Vrt=Vrt+1
        IF (Vrt > SurfaceTmp(SurfNum)%Sides) EXIT
      ENDDO

      SurfaceTmp(SurfNum)%Perimeter=Perimeter

      CALL CreateNewellSurfaceNormalVector(SurfaceTmp(SurfNum)%Vertex,SurfaceTmp(SurfNum)%Sides,  &
         SurfaceTmp(SurfNum)%NewellSurfaceNormalVector)
      CALL CreateNewellAreaVector(SurfaceTmp(SurfNum)%Vertex,SurfaceTmp(SurfNum)%Sides,SurfaceTmp(SurfNum)%NewellAreaVector)
      ! For surfaces with subsurfaces, the following two areas are turned into net areas later by
      ! subtracting subsurface areas
      SurfaceTmp(SurfNum)%GrossArea=VecLength(SurfaceTmp(SurfNum)%NewellAreaVector)
      SurfaceTmp(SurfNum)%Area=SurfaceTmp(SurfNum)%GrossArea
      SurfaceTmp(SurfNum)%NetAreaShadowCalc = SurfaceTmp(SurfNum)%Area
      CALL DetermineAzimuthAndTilt(SurfaceTmp(SurfNum)%Vertex,SurfaceTmp(SurfNum)%Sides,SurfWorldAz,SurfTilt,  &
                                   SurfaceTmp(SurfNum)%lcsx,SurfaceTmp(SurfNum)%lcsy,SurfaceTmp(SurfNum)%lcsz, &
                                   SurfaceTmp(SurfNum)%GrossArea,SurfaceTmp(SurfNum)%NewellSurfaceNormalVector)
      dotp=SurfaceTmp(SurfNum)%NewellSurfaceNormalVector .dot. TestVector
      IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Roof .and. dotp < -0.000001d0) THEN
        TiltString=RoundSigDigits(SurfTilt,1)
        CALL ShowWarningError(RoutineName//'Roof/Ceiling is upside down! Tilt angle=['//TRIM(TiltString)//  &
           '], should be near 0,'//   &
           ' Surface="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
           '", in Zone="'//TRIM(SurfaceTmp(SurfNum)%ZoneName)//'".')
        CALL ShowContinueError('Automatic fix is attempted.')
        CALL ReverseAndRecalculate(SurfNum,SurfaceTmp(SurfNum)%Sides,SurfWorldAz,SurfTilt)
      ELSEIF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Roof .and. SurfTilt > 80.0d0) THEN
        TiltString=RoundSigDigits(SurfTilt,1)
        CALL ShowWarningError(RoutineName//'Roof/Ceiling is not oriented correctly! Tilt angle=['//TRIM(TiltString)//  &
           '], should be near 0,'//   &
           ' Surface="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
           '", in Zone="'//TRIM(SurfaceTmp(SurfNum)%ZoneName)//'".')
      ENDIF
      IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Floor .and. dotp > 0.000001d0) THEN
        TiltString=RoundSigDigits(SurfTilt,1)
        CALL ShowWarningError(RoutineName//'Floor is upside down! Tilt angle=['//TRIM(TiltString)//  &
           '], should be near 180,'//   &
           ' Surface="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
           '", in Zone="'//TRIM(SurfaceTmp(SurfNum)%ZoneName)//'".')
        CALL ShowContinueError('Automatic fix is attempted.')
        CALL ReverseAndRecalculate(SurfNum,SurfaceTmp(SurfNum)%Sides,SurfWorldAz,SurfTilt)
      ELSEIF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Floor .and. SurfTilt < 158.2d0) THEN  ! slope/grade = 40%!
        TiltString=RoundSigDigits(SurfTilt,1)
        CALL ShowWarningError(RoutineName//'Floor is not oriented correctly! Tilt angle=['//TRIM(TiltString)//  &
           '], should be near 180,'//   &
           ' Surface="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
           '", in Zone="'//TRIM(SurfaceTmp(SurfNum)%ZoneName)//'".')
      ENDIF
      SurfaceTmp(SurfNum)%Azimuth=SurfWorldAz
      SurfaceTmp(SurfNum)%Tilt=SurfTilt

      ! Sine and cosine of azimuth and tilt
      SurfaceTmp(SurfNum)%SinAzim = SIN(SurfWorldAz*DegToRadians)
      SurfaceTmp(SurfNum)%CosAzim = COS(SurfWorldAz*DegToRadians)
      SurfaceTmp(SurfNum)%SinTilt = SIN(SurfTilt*DegToRadians)
      SurfaceTmp(SurfNum)%CosTilt = COS(SurfTilt*DegToRadians)
      IF (SurfaceTmp(SurfNum)%ViewFactorGround == AutoCalculate) THEN
        SurfaceTmp(SurfNum)%ViewFactorGround = 0.5d0 * (1.0d0 - SurfaceTmp(SurfNum)%CosTilt)
      ENDIF
      ! Outward normal unit vector (pointing away from room)
      SurfaceTmp(SurfNum)%OutNormVec = SurfaceTmp(SurfNum)%NewellSurfaceNormalVector
      DO N=1,3
        IF (ABS(SurfaceTmp(SurfNum)%OutNormVec(N)-1.0d0) < 1.d-06) SurfaceTmp(SurfNum)%OutNormVec(N) = +1.0d0
        IF (ABS(SurfaceTmp(SurfNum)%OutNormVec(N)+1.0d0) < 1.d-06) SurfaceTmp(SurfNum)%OutNormVec(N) = -1.0d0
        IF (ABS(SurfaceTmp(SurfNum)%OutNormVec(N))     < 1.d-06) SurfaceTmp(SurfNum)%OutNormVec(N) =  0.0d0
      ENDDO

      IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Window .or. SurfaceTmp(SurfNum)%Class == SurfaceClass_GlassDoor .or.  &
          SurfaceTmp(SurfNum)%Class == SurfaceClass_Door) &
          SurfaceTmp(SurfNum)%Area =  SurfaceTmp(SurfNum)%Area * SurfaceTmp(SurfNum)%Multiplier
      ! Can perform tests on this surface here
      SurfaceTmp(SurfNum)%ViewFactorSky=0.5d0*(1.d0+SurfaceTmp(SurfNum)%CosTilt)
      ! The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
      ! surfaces
      SurfaceTmp(SurfNum)%ViewFactorSkyIR = SurfaceTmp(SurfNum)%ViewFactorSky
      SurfaceTmp(SurfNum)%ViewFactorGroundIR = 0.5d0*(1.d0-SurfaceTmp(SurfNum)%CosTilt)


      ! Call to transform vertices

      Call TransformVertsByAspect(SurfNum,SurfaceTmp(SurfNum)%Sides)

    ELSE
      CALL ShowFatalError(RoutineName//'Called with less than 2 sides, Surface='//TRIM(SurfaceTmp(SurfNum)%Name))
    ENDIF

    ! Preliminary Height/Width
    temp=SurfaceTmp(SurfNum)%Vertex(3)-SurfaceTmp(SurfNum)%Vertex(2)
    ThisWidth=VecLength(temp)
    temp=SurfaceTmp(SurfNum)%Vertex(2)-SurfaceTmp(SurfNum)%Vertex(1)
    ThisHeight=VecLength(temp)
    SurfaceTmp(SurfNum)%Height=ThisHeight
    SurfaceTmp(SurfNum)%Width=ThisWidth

  RETURN

END SUBROUTINE GetVertices

SUBROUTINE ReverseAndRecalculate(SurfNum,NSides,SurfAzimuth,SurfTilt)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine reverses the vertices for a surface (needed when roof/floor is upside down)
          ! and recalculates the azimuth, etc for the surface.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Vectors
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: SurfNum   ! Surface number for the surface
  INTEGER, INTENT(IN) :: NSides    ! number of sides to surface
  REAL(r64), INTENT(INOUT) :: SurfAzimuth ! Surface Facing angle (will be 0 for roofs/floors)
  REAL(r64), INTENT(INOUT) :: SurfTilt    ! Surface tilt (

          ! SUBROUTINE PARAMETER DEFINITIONS:
  character(len=*), parameter :: fmt3="(A,I5,A,3(1x,f18.13))"
  character(len=*), parameter :: RoutineName='ReverseAndRecalculate: '

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    INTEGER :: N ! Loop Control
    INTEGER :: RevPtr ! pointer for reversing vertices
    TYPE(vector), DIMENSION(NSides) :: Vertices    ! Vertices, in specified order
    CHARACTER(len=10) :: TiltString

    DO N=1,NSides
      Vertices(N)=SurfaceTmp(SurfNum)%Vertex(N)
    ENDDO
    RevPtr=NSides
    DO N=1,NSides
      SurfaceTmp(SurfNum)%Vertex(N)=Vertices(RevPtr)
      RevPtr=RevPtr-1
    ENDDO

    write(outputfiledebug,*) 'Reversing Surface Name='//trim(surfacetmp(surfnum)%Name)
    do N=1,nsides
      write(outputfiledebug,fmt3) 'side=',n,' abs coord vertex=',surfacetmp(surfnum)%vertex(n)%x,  &
         surfacetmp(surfnum)%vertex(n)%y,surfacetmp(surfnum)%vertex(n)%z
    enddo

    CALL CreateNewellSurfaceNormalVector(SurfaceTmp(SurfNum)%Vertex,SurfaceTmp(SurfNum)%Sides,  &
       SurfaceTmp(SurfNum)%NewellSurfaceNormalVector)
    CALL DetermineAzimuthAndTilt(SurfaceTmp(SurfNum)%Vertex,SurfaceTmp(SurfNum)%Sides,SurfAzimuth,SurfTilt,  &
                                 SurfaceTmp(SurfNum)%lcsx,SurfaceTmp(SurfNum)%lcsy,SurfaceTmp(SurfNum)%lcsz, &
                                 SurfaceTmp(SurfNum)%GrossArea,SurfaceTmp(SurfNum)%NewellSurfaceNormalVector)
    IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Roof .and. SurfTilt > 80.0d0) THEN
      TiltString=RoundSigDigits(SurfTilt,1)
      CALL ShowWarningError(RoutineName//'Roof/Ceiling is still upside down! Tilt angle=['//TRIM(TiltString)//  &
           '], should be near 0, please fix manually.')
    ENDIF
    IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Floor .and. SurfTilt < 158.2d0) THEN  ! 40% grade!
      CALL ShowWarningError(RoutineName//'Floor is still upside down! Tilt angle=['//TRIM(TiltString)//  &
         '], should be near 180, please fix manually.')
    ENDIF

  RETURN

END SUBROUTINE ReverseAndRecalculate

SUBROUTINE MakeMirrorSurface(SurfNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   June 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine creates a "mirror" surface using the indicated surface.
          ! This is the simple approach for bi-directional shading devices.  If, perchance,
          ! the user has already taken care of this (e.g. fins in middle of wall), there will
          ! be extra shading devices shown.

          ! METHODOLOGY EMPLOYED:
          ! Reverse the vertices in the original surface.  Add "bi" to name.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE Vectors

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(INOUT) :: SurfNum  ! In=>Surface to Mirror, Out=>new Surface index

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER Vert
  INTEGER NVert
  REAL(r64) SurfWorldAz
  REAL(r64) SurfTilt
  INTEGER N
!  TYPE (Vector) :: temp1

  NVert=SurfaceTmp(SurfNum)%Sides
  ALLOCATE(SurfaceTmp(SurfNum+1)%Vertex(NVert))
! doesn't work when Vertex are pointers  SurfaceTmp(SurfNum+1)=SurfaceTmp(SurfNum)
  SurfaceTmp(SurfNum+1)%Name=SurfaceTmp(SurfNum)%Name
  SurfaceTmp(SurfNum+1)%Construction=SurfaceTmp(SurfNum)%Construction
  SurfaceTmp(SurfNum+1)%ConstructionStoredInputValue=SurfaceTmp(SurfNum)%ConstructionStoredInputValue
  SurfaceTmp(SurfNum+1)%Class=SurfaceTmp(SurfNum)%Class
  SurfaceTmp(SurfNum+1)%GrossArea=SurfaceTmp(SurfNum)%GrossArea
  SurfaceTmp(SurfNum+1)%Area=SurfaceTmp(SurfNum)%Area
  SurfaceTmp(SurfNum+1)%Azimuth=SurfaceTmp(SurfNum)%Azimuth
  SurfaceTmp(SurfNum+1)%Height=SurfaceTmp(SurfNum)%Height
  SurfaceTmp(SurfNum+1)%Reveal=SurfaceTmp(SurfNum)%Reveal
  SurfaceTmp(SurfNum+1)%Shape=SurfaceTmp(SurfNum)%Shape
  SurfaceTmp(SurfNum+1)%Sides=SurfaceTmp(SurfNum)%Sides
  SurfaceTmp(SurfNum+1)%Tilt=SurfaceTmp(SurfNum)%Tilt
  SurfaceTmp(SurfNum+1)%Width=SurfaceTmp(SurfNum)%Width
  SurfaceTmp(SurfNum+1)%HeatTransSurf=SurfaceTmp(SurfNum)%HeatTransSurf
  SurfaceTmp(SurfNum+1)%BaseSurfName=SurfaceTmp(SurfNum)%BaseSurfName
  SurfaceTmp(SurfNum+1)%BaseSurf=SurfaceTmp(SurfNum)%BaseSurf
  SurfaceTmp(SurfNum+1)%ZoneName=SurfaceTmp(SurfNum)%ZoneName
  SurfaceTmp(SurfNum+1)%Zone=SurfaceTmp(SurfNum)%Zone
  SurfaceTmp(SurfNum+1)%ExtBoundCondName=SurfaceTmp(SurfNum)%ExtBoundCondName
  SurfaceTmp(SurfNum+1)%ExtBoundCond=SurfaceTmp(SurfNum)%ExtBoundCond
  SurfaceTmp(SurfNum+1)%ExtSolar=SurfaceTmp(SurfNum)%ExtSolar
  SurfaceTmp(SurfNum+1)%ExtWind=SurfaceTmp(SurfNum)%ExtWind
  SurfaceTmp(SurfNum+1)%ViewFactorGround=SurfaceTmp(SurfNum)%ViewFactorGround
  SurfaceTmp(SurfNum+1)%ViewFactorSky=SurfaceTmp(SurfNum)%ViewFactorSky
  SurfaceTmp(SurfNum+1)%ViewFactorGroundIR=SurfaceTmp(SurfNum)%ViewFactorGroundIR
  SurfaceTmp(SurfNum+1)%ViewFactorSkyIR=SurfaceTmp(SurfNum)%ViewFactorSkyIR
  SurfaceTmp(SurfNum+1)%SchedShadowSurfIndex=SurfaceTmp(SurfNum)%SchedShadowSurfIndex
  SurfaceTmp(SurfNum+1)%ShadowSurfSchedVaries=SurfaceTmp(SurfNum)%ShadowSurfSchedVaries
  SurfaceTmp(SurfNum+1)%SchedMinValue=SurfaceTmp(SurfNum)%SchedMinValue
  SurfaceTmp(SurfNum+1)%IsTransparent=SurfaceTmp(SurfNum)%IsTransparent
  SurfaceTmp(SurfNum+1)%ShadowingSurf=SurfaceTmp(SurfNum)%ShadowingSurf
  SurfaceTmp(SurfNum+1)%MaterialMovInsulExt=SurfaceTmp(SurfNum)%MaterialMovInsulExt
  SurfaceTmp(SurfNum+1)%MaterialMovInsulInt=SurfaceTmp(SurfNum)%MaterialMovInsulInt
  SurfaceTmp(SurfNum+1)%SchedMovInsulExt=SurfaceTmp(SurfNum)%SchedMovInsulExt
  SurfaceTmp(SurfNum+1)%SchedMovInsulInt=SurfaceTmp(SurfNum)%SchedMovInsulInt
  SurfaceTmp(SurfNum+1)%WindowShadingControlPtr=SurfaceTmp(SurfNum)%WindowShadingControlPtr
  SurfaceTmp(SurfNum+1)%ShadedConstruction=SurfaceTmp(SurfNum)%ShadedConstruction
  SurfaceTmp(SurfNum+1)%FrameDivider=SurfaceTmp(SurfNum)%FrameDivider
  SurfaceTmp(SurfNum+1)%Multiplier=SurfaceTmp(SurfNum)%Multiplier
  SurfaceTmp(SurfNum+1)%NetAreaShadowCalc=SurfaceTmp(SurfNum)%NetAreaShadowCalc
  SurfaceTmp(SurfNum+1)%Perimeter=SurfaceTmp(SurfNum)%Perimeter

  DO Vert=1,SurfaceTmp(SurfNum)%Sides
    SurfaceTmp(SurfNum+1)%Vertex(Vert)=SurfaceTmp(SurfNum)%Vertex(NVert)
    NVert=NVert-1
  ENDDO
  SurfNum=SurfNum+1
  SurfaceTmp(SurfNum)%Name='Mir-'//TRIM(SurfaceTmp(SurfNum-1)%Name)

  ! TH 3/26/2010
  SurfaceTmp(SurfNum)%MirroredSurf = .TRUE.

  IF (SurfaceTmp(SurfNum)%Sides > 2) THEN
    CALL CreateNewellAreaVector(SurfaceTmp(SurfNum)%Vertex,SurfaceTmp(SurfNum)%Sides,SurfaceTmp(surfnum)%NewellAreaVector)
    SurfaceTmp(SurfNum)%GrossArea=VecLength(SurfaceTmp(surfnum)%NewellAreaVector)
    SurfaceTmp(SurfNum)%Area=SurfaceTmp(SurfNum)%GrossArea
    SurfaceTmp(SurfNum)%NetAreaShadowCalc = SurfaceTmp(SurfNum)%Area
    CALL CreateNewellSurfaceNormalVector(SurfaceTmp(SurfNum)%Vertex,SurfaceTmp(SurfNum)%Sides,  &
       SurfaceTmp(surfnum)%NewellSurfaceNormalVector)
    CALL DetermineAzimuthAndTilt(SurfaceTmp(SurfNum)%Vertex,SurfaceTmp(SurfNum)%Sides,SurfWorldAz,SurfTilt,  &
                                 SurfaceTmp(SurfNum)%lcsx,SurfaceTmp(SurfNum)%lcsy,SurfaceTmp(SurfNum)%lcsz,  &
                                 SurfaceTmp(SurfNum)%GrossArea,SurfaceTmp(SurfNum)%NewellSurfaceNormalVector)
    SurfaceTmp(SurfNum)%Azimuth=SurfWorldAz
    SurfaceTmp(SurfNum)%Tilt=SurfTilt

      ! Sine and cosine of azimuth and tilt
      SurfaceTmp(SurfNum)%SinAzim = SIN(SurfWorldAz*DegToRadians)
      SurfaceTmp(SurfNum)%CosAzim = COS(SurfWorldAz*DegToRadians)
      SurfaceTmp(SurfNum)%SinTilt = SIN(SurfTilt*DegToRadians)
      SurfaceTmp(SurfNum)%CosTilt = COS(SurfTilt*DegToRadians)
      ! Outward normal unit vector (pointing away from room)
      SurfaceTmp(SurfNum)%OutNormVec = SurfaceTmp(SurfNum)%NewellSurfaceNormalVector
      DO N=1,3
        IF (ABS(SurfaceTmp(SurfNum)%OutNormVec(N)-1.0d0) < 1.d-06) SurfaceTmp(SurfNum)%OutNormVec(N) = +1.0d0
        IF (ABS(SurfaceTmp(SurfNum)%OutNormVec(N)+1.0d0) < 1.d-06) SurfaceTmp(SurfNum)%OutNormVec(N) = -1.0d0
        IF (ABS(SurfaceTmp(SurfNum)%OutNormVec(N))       < 1.d-06) SurfaceTmp(SurfNum)%OutNormVec(N) =  0.0d0
      ENDDO

    ! Can perform tests on this surface here
    SurfaceTmp(SurfNum)%ViewFactorSky=0.5d0*(1.d0+SurfaceTmp(SurfNum)%CosTilt)
    ! The following IR view factors are modified in subr. SkyDifSolarShading if there are shadowing
    ! surfaces
    SurfaceTmp(SurfNum)%ViewFactorSkyIR = SurfaceTmp(SurfNum)%ViewFactorSky
    SurfaceTmp(SurfNum)%ViewFactorGroundIR = 0.5d0*(1.d0-SurfaceTmp(SurfNum)%CosTilt)
  ENDIF

  RETURN

END SUBROUTINE MakeMirrorSurface

SUBROUTINE GetWindowShadingControlData(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   November 1998
          !       MODIFIED       Aug 2001 (FW): add handling of new ShadingControlIsScheduled
          !                      and GlareControlIsActive fields
          !                      Nov 2001 (FW): add ShadingDevice as alternative to ShadedConstruction
          !                      Dec 2001 (FW): add slat angle controls for blinds
          !                      Aug 2002 (FW): add Setpoint2; check that specified control type is legal
          !                      Feb 2003 (FW): add error if Material Name of Shading Device is used with
          !                        Shading Type = BetweenGlassShade or BetweenGlassBlind
          !                      Dec 2003 (FW): improve BetweenGlassBlind error messages
          !                      Feb 2009 (BG): improve error checking for OnIfScheduleAllows
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Reads in the window shading control information
          ! from the input data file, interprets it and puts it in the derived type

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList, VerifyName
  USE ScheduleManager, ONLY: GetScheduleIndex

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound ! If errors found in input

          ! SUBROUTINE PARAMETER DEFINITIONS:
  INTEGER, PARAMETER :: NumValidShadingTypes=8
  CHARACTER(len=*), PARAMETER, DIMENSION(NumValidShadingTypes) :: cValidShadingTypes=(/  &
                'INTERIORSHADE    ',  &
                'EXTERIORSHADE    ',  &
                'EXTERIORSCREEN   ',  &
                'INTERIORBLIND    ',  &
                'EXTERIORBLIND    ',  &
                'BETWEENGLASSSHADE',  &
                'BETWEENGLASSBLIND',  &
                'SWITCHABLEGLAZING'/)
  INTEGER, PARAMETER, DIMENSION(NumValidShadingTypes) :: ValidShadingTypes=(/  &
       WSC_ST_InteriorShade     , &
       WSC_ST_ExteriorShade     , &
       WSC_ST_ExteriorScreen    , &
       WSC_ST_InteriorBlind     , &
       WSC_ST_ExteriorBlind     , &
       WSC_ST_BetweenGlassShade , &
       WSC_ST_BetweenGlassBlind , &
       WSC_ST_SwitchableGlazing /)

  INTEGER, PARAMETER :: NumValidWindowShadingControlTypes=21
  CHARACTER(len=*), PARAMETER, DIMENSION(NumValidWindowShadingControlTypes) :: cValidWindowShadingControlTypes=(/  &
       'ALWAYSON                                      ', &
       'ALWAYSOFF                                     ', &
       'ONIFSCHEDULEALLOWS                            ', &
       'ONIFHIGHSOLARONWINDOW                         ', &
       'ONIFHIGHHORIZONTALSOLAR                       ', &
       'ONIFHIGHOUTDOORAIRTEMPERATURE                 ', &
       'ONIFHIGHZONEAIRTEMPERATURE                    ', &
       'ONIFHIGHZONECOOLING                           ', &
       'ONIFHIGHGLARE                                 ', &
       'MEETDAYLIGHTILLUMINANCESETPOINT               ', &
       'ONNIGHTIFLOWOUTDOORTEMPANDOFFDAY              ', &
       'ONNIGHTIFLOWINSIDETEMPANDOFFDAY               ', &
       'ONNIGHTIFHEATINGANDOFFDAY                     ', &
       'ONNIGHTIFLOWOUTDOORTEMPANDONDAYIFCOOLING      ', &
       'ONNIGHTIFHEATINGANDONDAYIFCOOLING             ', &
       'OFFNIGHTANDONDAYIFCOOLINGANDHIGHSOLARONWINDOW ', &
       'ONNIGHTANDONDAYIFCOOLINGANDHIGHSOLARONWINDOW  ', &
       'ONIFHIGHOUTDOORAIRTEMPANDHIGHSOLARONWINDOW    ', &
       'ONIFHIGHOUTDOORAIRTEMPANDHIGHHORIZONTALSOLAR  ', &
       'ONIFHIGHZONEAIRTEMPANDHIGHSOLARONWINDOW       ', &
       'ONIFHIGHZONEAIRTEMPANDHIGHHORIZONTALSOLAR     '/)

  INTEGER, PARAMETER, DIMENSION(NumValidWindowShadingControlTypes) :: ValidWindowShadingControlTypes=(/  &
       WSCT_ALWAYSON                      , & ! 'ALWAYSON                                    ', &
       WSCT_ALWAYSOFF                     , & ! 'ALWAYSOFF                                   ', &
       WSCT_ONIFSCHEDULED                 , & ! 'ONIFSCHEDULEALLOWS                          ', &
       WSCT_HISOLAR                       , & ! 'ONIFHIGHSOLARONWINDOW                       ', &
       WSCT_HIHORZSOLAR                   , & ! 'ONIFHIGHHORIZONTALSOLAR                     ', &
       WSCT_HIOUTAIRTEMP                  , & ! 'ONIFHIGHOUTDOORAIRTEMPERATURE                      ', &
       WSCT_HIZONEAIRTEMP                 , & ! 'ONIFHIGHZONEAIRTEMPERATURE                         ', &
       WSCT_HIZONECOOLING                 , & ! 'ONIFHIGHZONECOOLING                         ', &
       WSCT_HIGLARE                       , & ! 'ONIFHIGHGLARE                               ', &
       WSCT_MEETDAYLILUMSETP              , & ! 'MEETDAYLIGHTILLUMINANCESETPOINT             ', &
       WSCT_ONNIGHTLOOUTTEMP_OFFDAY       , & ! 'ONNIGHTIFLOWOUTDOORTEMPANDOFFDAY              ', &
       WSCT_ONNIGHTLOINTEMP_OFFDAY        , & ! 'ONNIGHTIFLOWINSIDETEMPANDOFFDAY               ', &
       WSCT_ONNIGHTIFHEATING_OFFDAY       , & ! 'ONNIGHTIFHEATINGANDOFFDAY                     ', &
       WSCT_ONNIGHTLOOUTTEMP_ONDAYCOOLING , & ! 'ONNIGHTIFLOWOUTDOORTEMPANDONDAYIFCOOLING      ', &
       WSCT_ONNIGHTIFHEATING_ONDAYCOOLING , & ! 'ONNIGHTIFHEATINGANDONDAYIFCOOLING             ', &
       WSCT_OFFNIGHT_ONDAY_HISOLARWINDOW  , & ! 'OFFNIGHTANDONDAYIFCOOLINGANDHIGHSOLARONWINDOW ', &
       WSCT_ONNIGHT_ONDAY_HISOLARWINDOW   , & ! 'ONNIGHTANDONDAYIFCOOLINGANDHIGHSOLARONWINDOW  ', &
       WSCT_ONHIOUTTEMP_HISOLARWINDOW     , & ! 'ONIFHIGHOUTDOORAIRTEMPANDHIGHSOLARONWINDOW  ', &
       WSCT_ONHIOUTTEMP_HIHORZSOLAR       , & ! 'ONIFHIGHOUTDOORAIRTEMPANDHIGHHORIZONTALSOLAR', &
       WSCT_ONHIZONETEMP_HISOLARWINDOW    , & ! 'ONIFHIGHZONEAIRTEMPANDHIGHSOLARONWINDOW     ', &
       WSCT_ONHIZONETEMP_HIHORZSOLAR      /) !  'ONIFHIGHZONEAIRTEMPANDHIGHHORIZONTALSOLAR   '/)


          ! INTERFACE BLOCK SPECIFICATIONS:na
          ! DERIVED TYPE DEFINITIONS:na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER :: IOStat            ! IO Status when calling get input subroutine
  INTEGER :: ControlNumAlpha   ! Number of control alpha names being passed
  INTEGER :: ControlNumProp    ! Number of control properties being passed
  INTEGER :: ControlNum        ! DO loop counter/index for window shading control number
  INTEGER :: IShadedConst      ! Construction number of shaded construction
  INTEGER :: IShadingDevice    ! Material number of shading device
  INTEGER :: NLayers           ! Layers in shaded construction
  LOGICAL :: ErrorInName
  LOGICAL :: IsBlank
  INTEGER :: Loop
  INTEGER :: ShTyp       ! Shading type
  CHARACTER(MaxNameLength) :: ControlType ! Shading control type
  LOGICAL :: BGShadeBlindError  ! True if problem with construction that is supposed to have between-glass
                                ! shade or blind
  INTEGER :: Found

          ! FLOW:
! Get the total number of window shading control blocks
  cCurrentModuleObject='WindowProperty:ShadingControl'
  TotWinShadingControl = GetNumObjectsFound(cCurrentModuleObject)
  IF(TotWinShadingControl.EQ.0) RETURN

  ALLOCATE (WindowShadingControl(TotWinShadingControl))

  ControlNum=0
  DO Loop = 1, TotWinShadingControl

    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,ControlNumAlpha, &
                       rNumericArgs,ControlNumProp,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    ErrorInName=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),WindowShadingControl%Name,ControlNum,ErrorInName,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (ErrorInName) THEN
      ErrorsFound=.true.
      CYCLE
    ENDIF

    ControlNum=ControlNum+1
    WindowShadingControl(ControlNum)%Name = cAlphaArgs(1)  ! Set the Control Name in the Derived Type
    WindowShadingControl(ControlNum)%ShadedConstruction = FindIteminList(cAlphaArgs(3),Construct%Name,TotConstructs)
    WindowShadingControl(ControlNum)%ShadingDevice = FindIteminList(cAlphaArgs(8),Material%Name,TotMaterials)
    WindowShadingControl(ControlNum)%Schedule = GetScheduleIndex(cAlphaArgs(5))
    WindowShadingControl(ControlNum)%SetPoint  = rNumericArgs(1)
    WindowShadingControl(ControlNum)%SetPoint2 = rNumericArgs(2)
    WindowShadingControl(ControlNum)%ShadingControlIsScheduled = .FALSE.
    IF(cAlphaArgs(6) == 'YES') WindowShadingControl(ControlNum)%ShadingControlIsScheduled = .TRUE.
    WindowShadingControl(ControlNum)%GlareControlIsActive = .FALSE.
    IF(cAlphaArgs(7) == 'YES') WindowShadingControl(ControlNum)%GlareControlIsActive = .TRUE.
    WindowShadingControl(ControlNum)%SlatAngleSchedule = GetScheduleIndex(cAlphaArgs(10))

    ControlType = cAlphaArgs(4)

    IF(ControlType=='SCHEDULE') THEN
      ControlType = 'ONIFSCHEDULEALLOWS'
      WindowShadingControl(ControlNum)%ShadingControlIsScheduled = .TRUE.
      WindowShadingControl(ControlNum)%GlareControlIsActive = .FALSE.
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
             '" is using obsolete '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))// &
             '", changing to "'//TRIM(ControlType)//'"')
      ! Error if schedule has not been specified
      IF(WindowShadingControl(ControlNum)%Schedule <= 0) THEN
        ErrorsFound = .TRUE.
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
              ' has '//TRIM(cAlphaFieldNames(4))//' "'//TRIM(ControlType)// &
              '" but a schedule has not been specified.')
      END IF
    END IF

    IF(ControlType(1:11)=='SCHEDULEAND') THEN
      ControlType = 'ONIFHIGH'//ControlType(12:)
      WindowShadingControl(ControlNum)%ShadingControlIsScheduled = .TRUE.
      WindowShadingControl(ControlNum)%GlareControlIsActive = .FALSE.
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
             '" is using obsolete '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))// &
             '", changing to "'//TRIM(ControlType)//'"')
      ! Error if schedule has not been specified
      IF(WindowShadingControl(ControlNum)%Schedule <= 0) THEN
        ErrorsFound = .TRUE.
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
              ' has '//TRIM(cAlphaFieldNames(4))//' "'//TRIM(ControlType)// &
              '" but a schedule has not been specified.')
      END IF
    END IF

    IF(ControlType(1:7)=='GLAREOR') THEN
      ControlType = 'ONIFHIGH'//ControlType(8:)
      WindowShadingControl(ControlNum)%ShadingControlIsScheduled = .FALSE.
      WindowShadingControl(ControlNum)%GlareControlIsActive = .TRUE.
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
             '" is using obsolete '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))// &
             '", changing to "'//TRIM(ControlType)//'"')
    END IF

    IF(ControlType=='GLARE') THEN
      ControlType = 'ONIFHIGHGLARE'
      WindowShadingControl(ControlNum)%ShadingControlIsScheduled = .FALSE.
      WindowShadingControl(ControlNum)%GlareControlIsActive = .TRUE.
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
             '" is using obsolete '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))// &
             '", changing to "'//TRIM(ControlType)//'"')
    END IF

    IF(WindowShadingControl(ControlNum)%ShadingDevice .GT. 0)THEN
      IF(Material(WindowShadingControl(ControlNum)%ShadingDevice)%Group == Screen .AND. .NOT. &
         (ControlType == 'ALWAYSON' .OR. ControlType == 'ALWAYSOFF' .OR. &
          ControlType == 'ONIFSCHEDULEALLOWS'))THEN
        ErrorsFound = .TRUE.
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
             '" invalid '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))// &
             '" for exterior screens.')
        CALL ShowContinueError('Valid shading control types for exterior window screens'// &
             ' are ALWAYSON, ALWAYSOFF, or ONIFSCHEDULEALLOWS.')
      END IF
    ELSE
      IF (WindowShadingControl(ControlNum)%ShadedConstruction > 0) THEN
        Construct(WindowShadingControl(ControlNum)%ShadedConstruction)%IsUsed=.true.
        IF(Material(Construct(WindowShadingControl(ControlNum)%ShadedConstruction)%LayerPoint(1))%Group == Screen .AND. .NOT. &
           (ControlType == 'ALWAYSON' .OR. ControlType == 'ALWAYSOFF' .OR. &
            ControlType == 'ONIFSCHEDULEALLOWS'))THEN
          ErrorsFound = .TRUE.
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
               '" invalid '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))// &
               '" for exterior screens.')
          CALL ShowContinueError('Valid shading control types for exterior window screens'// &
               ' are ALWAYSON, ALWAYSOFF, or ONIFSCHEDULEALLOWS.')
        END IF
      ELSEIF (lAlphaFieldBlanks(3)) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
           '", '//trim(cAlphaFieldNames(3))//' is blank.')
        CALL ShowContinueError('A valid construction is required.')
        ErrorsFound=.true.
      ELSE
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
           '", '//trim(cAlphaFieldNames(3))//' is invalid.')
        CALL ShowContinueError('Construction="'//trim(cAlphaArgs(3))//'" was used. A valid construction is required.')
        ErrorsFound=.true.
      ENDIF
    END IF

    ! Warning if setpoint is unintentionally zero
    IF(WindowShadingControl(ControlNum)%SetPoint == 0 .AND. &
      ControlType /= 'ALWAYSON' .AND. &
      ControlType /= 'ALWAYSOFF' .AND. &
      ControlType /= 'ONIFSCHEDULEALLOWS' .AND. ControlType /= 'SCHEDULE' .AND. &
      ControlType /= 'ONIFHIGHGLARE' .AND. ControlType /= 'GLARE' .AND. &
      ControlType /= 'DAYLIGHTILLUMINANCE' ) THEN
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
             '", The first SetPoint is zero.')
      CALL ShowContinueError('..You may have forgotten to specify that setpoint.')
    END IF

    ! Upward compatibility for old Shading Control Type names
    IF(ControlType=='SOLARONWINDOW' .OR. ControlType=='HORIZONTALSOLAR' .OR. ControlType=='OUTSIDEAIRTEMP' .OR. &
       ControlType=='ZONEAIRTEMP' .OR. ControlType=='ZONECOOLING') THEN
       ControlType = 'ONIFHIGH'//TRIM(ControlType)
       WindowShadingControl(ControlNum)%ShadingControlIsScheduled = .FALSE.
       WindowShadingControl(ControlNum)%GlareControlIsActive = .FALSE.
       CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
             '" is using obsolete '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))// &
             '", changing to "'//TRIM(ControlType)//'"')
    END IF

    ! Error if illegal control type
    Found=FindItemInList(ControlType,cValidWindowShadingControlTypes,NumValidWindowShadingControlTypes)
    IF (Found == 0) THEN
      ErrorsFound = .TRUE.
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
             '" invalid '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
    ELSE
     WindowShadingControl(ControlNum)%ShadingControlType = ValidWindowShadingControlTypes(Found)
    END IF


    ! Error checks
    IF(cAlphaArgs(6) /= 'YES' .AND. cAlphaArgs(6) /= 'NO') THEN
      ErrorsFound = .true.
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
             '" invalid '//TRIM(cAlphaFieldNames(6))//'="'//TRIM(cAlphaArgs(6))//'".')
    END IF
    IF(cAlphaArgs(7) /= 'YES' .AND. cAlphaArgs(7) /= 'NO') THEN
      ErrorsFound = .true.
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
             '" invalid '//TRIM(cAlphaFieldNames(7))//'="'//TRIM(cAlphaArgs(7))//'".')
    END IF

    IF ((WindowShadingControl(ControlNum)%ShadingControlType == WSCT_ONIFSCHEDULED) .and. &
        (.NOT. WindowShadingControl(ControlNum)%ShadingControlIsScheduled)) THEN ! CR 7709 BG
      ErrorsFound = .TRUE.
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//' = "'//TRIM(WindowShadingControl(ControlNum)%Name)//'" invalid, '// &
           TRIM(cAlphaFieldNames(6))//' must be set to "Yes" for '//TRIM(cAlphaFieldNames(4))//' = OnIfScheduleAllows')
    ENDIF

    IF(cAlphaArgs(9) /= 'FIXEDSLATANGLE' .AND. cAlphaArgs(9) /= 'SCHEDULEDSLATANGLE' .AND. &
       cAlphaArgs(9) /= 'BLOCKBEAMSOLAR') THEN
      ErrorsFound = .true.
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
             '" invalid '//TRIM(cAlphaFieldNames(9))//'="'//TRIM(cAlphaArgs(9))//'".')
    ELSEIF (cAlphaArgs(9) == 'FIXEDSLATANGLE') THEN
      WindowShadingControl(ControlNum)%SlatAngleControlForBlinds = WSC_SAC_FixedSlatAngle
    ELSEIF (cAlphaArgs(9) == 'SCHEDULEDSLATANGLE') THEN
      WindowShadingControl(ControlNum)%SlatAngleControlForBlinds = WSC_SAC_ScheduledSlatAngle
    ELSEIF (cAlphaArgs(9) == 'BLOCKBEAMSOLAR') THEN
      WindowShadingControl(ControlNum)%SlatAngleControlForBlinds = WSC_SAC_BlockBeamSolar
    END IF

    ! For upward compatibility change old "noninsulating" and "insulating" shade types to
    ! INTERIORSHADE or EXTERIORSHADE
    IF(cAlphaArgs(2) == 'INTERIORNONINSULATINGSHADE' .OR.   &
       cAlphaArgs(2) == 'INTERIORINSULATINGSHADE') THEN
       CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
             '" is using obsolete '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))// &
             '", changing to "InteriorShade"')
       WindowShadingControl(ControlNum)%ShadingType = WSC_ST_InteriorShade
       cAlphaArgs(2)='INTERIORSHADE'
    ENDIF
    IF(cAlphaArgs(2) == 'EXTERIORNONINSULATINGSHADE' .OR.   &
       cAlphaArgs(2) == 'EXTERIORINSULATINGSHADE') THEN
       CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
             '" is using obsolete '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))// &
             '", changing to "ExteriorShade"')
       WindowShadingControl(ControlNum)%ShadingType = WSC_ST_ExteriorShade
       cAlphaArgs(2)='EXTERIORSHADE'
    ENDIF

    IF (ControlType == 'MEETDAYLIGHTILLUMINANCESETPOINT' .and.  &
        cAlphaArgs(2) /= 'SWITCHABLEGLAZING') THEN
      ErrorsFound = .true.
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
             '" invalid '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'".')
      CALL ShowContinueError('...'//trim(cAlphaFieldNames(2))//' must be SwitchableGlazing for this control, but'//  &
                ' entered type="'//trim(cAlphaArgs(2))//'".')
    ENDIF

    ! Check for illegal shading type name
    Found=FindItemInList(cAlphaArgs(2),cValidShadingTypes,NumValidShadingTypes)
    IF (Found == 0) THEN
      ErrorsFound = .true.
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
             '" invalid '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'".')
    ELSE
      WindowShadingControl(ControlNum)%ShadingType=ValidShadingTypes(Found)
    END IF

    ShTyp = WindowShadingControl(ControlNum)%ShadingType
    IShadedConst   = WindowShadingControl(ControlNum)%ShadedConstruction
    IShadingDevice = WindowShadingControl(ControlNum)%ShadingDevice


    IF(IShadedConst==0 .AND. IShadingDevice == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
          '" has no matching shaded construction or shading device.')
      ErrorsFound=.true.
    ELSE IF(IShadedConst == 0 .AND. IShadingDevice > 0) THEN
      IF(ShTyp == WSC_ST_SwitchableGlazing) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
          '" has '//TRIM(cAlphaArgs(2))//'= SwitchableGlazing but no matching shaded construction')
        ErrorsFound = .true.
      END IF
      IF((ShTyp==WSC_ST_InteriorShade.OR. ShTyp==WSC_ST_ExteriorShade).AND. Material(IShadingDevice)%Group /= Shade) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
          '" has '//TRIM(cAlphaArgs(2))//'= InteriorShade or ExteriorShade but matching shading device is not a window shade')
        CALL ShowContinueError(TRIM(cAlphaFieldNames(8))//' in error="'//TRIM(Material(IShadingDevice)%Name)//'".')
        ErrorsFound = .true.
      END IF
      IF((ShTyp==WSC_ST_ExteriorScreen).AND. Material(IShadingDevice)%Group /= Screen) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
          '" has '//TRIM(cAlphaArgs(2))//'= ExteriorScreen but matching shading device is not a window screen')
        CALL ShowContinueError(TRIM(cAlphaFieldNames(8))//' in error="'//TRIM(Material(IShadingDevice)%Name)//'".')
        ErrorsFound = .true.
      END IF
      IF((ShTyp==WSC_ST_InteriorBlind.OR. ShTyp==WSC_ST_ExteriorBlind).AND. Material(IShadingDevice)%Group /= WindowBlind) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
          '" has '//TRIM(cAlphaArgs(2))//'= InteriorBlind or ExteriorBlind but matching shading device is not a window blind')
        CALL ShowContinueError(TRIM(cAlphaFieldNames(8))//' in error="'//TRIM(Material(IShadingDevice)%Name)//'".')
        ErrorsFound = .true.
      END IF
      IF(ShTyp==WSC_ST_BetweenGlassBlind.OR. ShTyp==WSC_ST_BetweenGlassBlind) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
        '" has '//TRIM(cAlphaArgs(2))//'= BetweenGlassShade or BetweenGlassBlind and')
        CALL ShowContinueError(TRIM(cAlphaFieldNames(8))//' is specified. This is illegal. Specify shaded construction instead.')
        ErrorsFound = .true.
      END IF
    ELSE IF(IShadedConst > 0 .AND. IShadingDevice > 0) THEN
      IShadingDevice = 0
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
                  '" Both '//TRIM(cAlphaFieldNames(3))//' and '//TRIM(cAlphaFieldNames(8))//' are specified.')
      CALL ShowContinueError('The '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(Construct(IShadedConst)%Name)//'" will be used.')
    END IF

    ! If type = interior or exterior shade or blind require that the shaded construction
    ! have a shade layer in the correct position
    IF(IShadedConst /= 0) THEN

      NLayers = Construct(IShadedConst)%TotLayers
      BGShadeBlindError = .FALSE.
      IShadingDevice=0
      IF (Construct(IShadedConst)%LayerPoint(NLayers) /= 0) THEN
        IF(WindowShadingControl(ControlNum)%ShadingType == WSC_ST_InteriorShade) THEN
          IShadingDevice=Construct(IShadedConst)%LayerPoint(NLayers)
          IF(Material(Construct(IShadedConst)%LayerPoint(NLayers))%Group /= Shade) THEN
            ErrorsFound = .TRUE.
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
              '" the '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'"')
            CALL ShowContinueError('of '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//  &
               '" should have a shade layer on the inside of the window.')
          END IF
        ELSE IF(WindowShadingControl(ControlNum)%ShadingType == WSC_ST_ExteriorShade) THEN
          IShadingDevice=Construct(IShadedConst)%LayerPoint(1)
          IF(Material(Construct(IShadedConst)%LayerPoint(1))%Group /= Shade) THEN
            ErrorsFound = .TRUE.
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
              '" the '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'"')
            CALL ShowContinueError('of '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//  &
               '" should have a shade layer on the outside of the window.')
          END IF
        ELSE IF(WindowShadingControl(ControlNum)%ShadingType == WSC_ST_ExteriorScreen) THEN
          IShadingDevice=Construct(IShadedConst)%LayerPoint(1)
          IF(Material(Construct(IShadedConst)%LayerPoint(1))%Group /= Screen) THEN
            ErrorsFound = .TRUE.
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
              '" the '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'"')
            CALL ShowContinueError('of '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//  &
               '" should have a screen layer on the outside of the window.')
          END IF
        ELSE IF(WindowShadingControl(ControlNum)%ShadingType == WSC_ST_InteriorBlind) THEN
          IShadingDevice=Construct(IShadedConst)%LayerPoint(NLayers)
          IF(Material(Construct(IShadedConst)%LayerPoint(NLayers))%Group /= WindowBlind) THEN
            ErrorsFound = .TRUE.
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
              '" the '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'"')
            CALL ShowContinueError('of '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//  &
               '" should have a blind layer on the inside of the window.')
          END IF
        ELSE IF(WindowShadingControl(ControlNum)%ShadingType == WSC_ST_ExteriorBlind) THEN
          IShadingDevice=Construct(IShadedConst)%LayerPoint(1)
          IF(Material(Construct(IShadedConst)%LayerPoint(1))%Group /= WindowBlind) THEN
            ErrorsFound = .TRUE.
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
              '" the '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'"')
            CALL ShowContinueError('of '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//  &
               '" should have a blind layer on the outside of the window.')
          END IF
        ELSE IF(WindowShadingControl(ControlNum)%ShadingType == WSC_ST_BetweenGlassShade) THEN
          IF(NLayers /= 5 .AND. NLayers /= 7) BGShadeBlindError = .TRUE.
          IF(NLayers==5) THEN
            IF(Material(Construct(IShadedConst)%LayerPoint(3))%Group /= Shade) BGShadeBlindError = .TRUE.
          END IF
          IF(NLayers==7) THEN
            IF(Material(Construct(IShadedConst)%LayerPoint(5))%Group /= Shade) BGShadeBlindError = .TRUE.
          END IF
          IF(BGShadeBlindError) THEN
            ErrorsFound = .TRUE.
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
              '" the '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'"')
            CALL ShowContinueError('of '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//  &
               '" should have two or three glass layers and a')
            CALL ShowContinueError('between-glass shade layer with a gas layer on each side.')
          END IF
        ELSE IF(WindowShadingControl(ControlNum)%ShadingType == WSC_ST_BetweenGlassBlind) THEN
          IF(NLayers /= 5 .AND. NLayers /= 7) BGShadeBlindError = .TRUE.
          IF(NLayers==5) THEN
            IF(Material(Construct(IShadedConst)%LayerPoint(3))%Group /= WindowBlind) BGShadeBlindError = .TRUE.
          END IF
          IF(NLayers==7) THEN
            IF(Material(Construct(IShadedConst)%LayerPoint(5))%Group /= WindowBlind) BGShadeBlindError = .TRUE.
          END IF
          IF(BGShadeBlindError) THEN
            ErrorsFound = .TRUE.
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
              '" the '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'"')
            CALL ShowContinueError('of '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//  &
               '" should have two or three glass layers and a')
            CALL ShowContinueError('between-glass blind layer with a gas layer on each side.')
          END IF
        END IF
      ENDIF
      IF(IShadingDevice > 0) THEN
        IF((ShTyp==WSC_ST_InteriorShade.OR. ShTyp==WSC_ST_ExteriorShade).AND. Material(IShadingDevice)%Group /= Shade) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
            '" has '//TRIM(cAlphaFieldNames(2))//'= InteriorShade or ExteriorShade '//  &
            'but matching shading device is not a window shade')
          CALL ShowContinueError('Shading Device in error="'//TRIM(Material(IShadingDevice)%Name)//'".')
          ErrorsFound = .true.
        END IF
        IF((ShTyp==WSC_ST_ExteriorScreen).AND. Material(IShadingDevice)%Group /= Screen) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
            '" has '//TRIM(cAlphaFieldNames(2))//'= ExteriorScreen '//  &
            'but matching shading device is not an exterior window screen.')
          CALL ShowContinueError('Shading Device in error="'//TRIM(Material(IShadingDevice)%Name)//'".')
          ErrorsFound = .true.
        END IF
        IF((ShTyp==WSC_ST_InteriorBlind.OR. ShTyp==WSC_ST_ExteriorBlind).AND. Material(IShadingDevice)%Group /= WindowBlind) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(WindowShadingControl(ControlNum)%Name)//  &
            '" has '//TRIM(cAlphaFieldNames(2))//'= InteriorBlind or ExteriorBlind '//  &
            'but matching shading device is not a window blind.')
          CALL ShowContinueError('Shading Device in error="'//TRIM(Material(IShadingDevice)%Name)//'".')
          ErrorsFound = .true.
        END IF
      END IF

    END IF

  END DO  ! End of loop over window shading controls

RETURN

END SUBROUTINE GetWindowShadingControlData

SUBROUTINE GetStormWindowData(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   December 2003
          !       MODIFIED       na

          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Reads in the storm window data from the input file,
          ! interprets it and puts it in the derived type

          ! METHODOLOGY EMPLOYED:

          ! REFERENCES:

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList, VerifyName
  USE General, ONLY: JulianDay,TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound ! If errors found in input

          ! SUBROUTINE PARAMETER DEFINITIONS:na
          ! INTERFACE BLOCK SPECIFICATIONS:na
          ! DERIVED TYPE DEFINITIONS:na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER :: IOStat            ! IO Status when calling get input subroutine
  INTEGER :: StormWinNumAlpha   ! Number of alpha names being passed
  INTEGER :: StormWinNumProp    ! Number of properties being passed
  INTEGER :: StormWinNum        ! Index for storm window number
  INTEGER :: loop               ! Do loop counter
  INTEGER :: SurfNum            ! Surface number
  INTEGER :: MatNum             ! Material number

          ! FLOW:

  ! Get the total number of storm window input objects
  cCurrentModuleObject='WindowProperty:StormWindow'
  TotStormWin = GetNumObjectsFound(cCurrentModuleObject)
  IF(TotStormWin == 0) RETURN

  ALLOCATE (StormWindow(TotStormWin))

  StormWinNum = 0
  DO loop = 1,TotStormWin

    CALL GetObjectItem(cCurrentModuleObject,loop,cAlphaArgs,StormWinNumAlpha, &
                       rNumericArgs,StormWinNumProp,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    StormWinNum=StormWinNum+1
    StormWindow(StormWinNum)%BaseWindowNum = FindIteminList(cAlphaArgs(1),Surface%Name,TotSurfaces)
    StormWindow(StormWinNum)%StormWinMaterialNum = FindIteminList(cAlphaArgs(2),Material%Name,TotMaterials)
    StormWindow(StormWinNum)%StormWinDistance = rNumericArgs(1)
    StormWindow(StormWinNum)%MonthOn = rNumericArgs(2)
    StormWindow(StormWinNum)%DayOfMonthOn = rNumericArgs(3)
    StormWindow(StormWinNum)%DateOn=JulianDay(StormWindow(StormWinNum)%MonthOn,StormWindow(StormWinNum)%DayOfMonthOn,1)
    StormWindow(StormWinNum)%MonthOff = rNumericArgs(4)
    StormWindow(StormWinNum)%DayOfMonthOff = rNumericArgs(5)
    StormWindow(StormWinNum)%DateOff=JulianDay(StormWindow(StormWinNum)%MonthOff,StormWindow(StormWinNum)%DayOfMonthOff,1)

    IF (StormWindow(StormWinNum)%DateOn == StormWindow(StormWinNum)%DateOff) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Date On = Date Off -- not allowed,'//  &
                           ' occured in WindowProperty:StormWindow Input #'//TRIM(TrimSigDigits(StormWinNum)))
      ErrorsFound=.true.
    ENDIF

    SELECT CASE (StormWindow(StormWinNum)%MonthOn)

     CASE (1,3,5,7,8,10,12)
       IF (StormWindow(StormWinNum)%DayOfMonthOn > 31) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Date On (Day of Month) ['//  &
                                 TRIM(TrimSigDigits(StormWindow(StormWinNum)%DayOfMonthOn))//'],'//  &
                                 ' invalid for WindowProperty:StormWindow Input #'//TRIM(TrimSigDigits(StormWinNum)))
         ErrorsFound=.true.
       ENDIF
     CASE (4,6,9,11)
       IF (StormWindow(StormWinNum)%DayOfMonthOn > 30) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Date On (Day of Month) ['//  &
                                 TRIM(TrimSigDigits(StormWindow(StormWinNum)%DayOfMonthOn))//'],'//  &
                                 ' invalid for WindowProperty:StormWindow Input #'//TRIM(TrimSigDigits(StormWinNum)))
         ErrorsFound=.true.
       ENDIF
     CASE (2)
       IF (StormWindow(StormWinNum)%DayOfMonthOn > 29) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Date On (Day of Month) ['//  &
                                 TRIM(TrimSigDigits(StormWindow(StormWinNum)%DayOfMonthOn))//'],'//  &
                                 ' invalid for WindowProperty:StormWindow Input #'//TRIM(TrimSigDigits(StormWinNum)))
         ErrorsFound=.true.
       ENDIF
     CASE DEFAULT
       CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Date On Month ['//  &
                               TRIM(TrimSigDigits(StormWindow(StormWinNum)%MonthOn))//'],'//  &
                               ' invalid for WindowProperty:StormWindow Input #'//TRIM(TrimSigDigits(StormWinNum)))
       ErrorsFound=.true.
    END SELECT
    SELECT CASE (StormWindow(StormWinNum)%MonthOff)

     CASE (1,3,5,7,8,10,12)
       IF (StormWindow(StormWinNum)%DayOfMonthOff > 31) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Date Off (Day of Month) ['//  &
                                 TRIM(TrimSigDigits(StormWindow(StormWinNum)%DayOfMonthOff))//'],'//  &
                                 ' invalid for WindowProperty:StormWindow Input #'//TRIM(TrimSigDigits(StormWinNum)))
         ErrorsFound=.true.
       ENDIF
     CASE (4,6,9,11)
       IF (StormWindow(StormWinNum)%DayOfMonthOff > 30) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Date Off (Day of Month) ['//  &
                                 TRIM(TrimSigDigits(StormWindow(StormWinNum)%DayOfMonthOff))//'],'//  &
                                 ' invalid for WindowProperty:StormWindow Input #'//TRIM(TrimSigDigits(StormWinNum)))
         ErrorsFound=.true.
       ENDIF
     CASE (2)
       IF (StormWindow(StormWinNum)%DayOfMonthOff > 29) THEN
         CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Date Off (Day of Month) ['//  &
                                 TRIM(TrimSigDigits(StormWindow(StormWinNum)%DayOfMonthOff))//'],'//  &
                                 ' invalid for WindowProperty:StormWindow Input #'//TRIM(TrimSigDigits(StormWinNum)))
         ErrorsFound=.true.
       ENDIF
     CASE DEFAULT
       CALL ShowSevereError(TRIM(cCurrentModuleObject)//': Date Off Month ['//  &
                               TRIM(TrimSigDigits(StormWindow(StormWinNum)%MonthOff))//'],'//  &
                               ' invalid for WindowProperty:StormWindow Input #'//TRIM(TrimSigDigits(StormWinNum)))
       ErrorsFound=.true.
    END SELECT
  END DO

  ! Error checks

  DO StormWinNum = 1,TotStormWin
    ! Require BaseWindowNum be that of an exterior window
    SurfNum = StormWindow(StormWinNum)%BaseWindowNum
    IF(SurfNum == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '" invalid.')
      ErrorsFound = .true.
    ELSE
      IF(Surface(SurfNum)%Class /= SurfaceClass_Window .OR. Surface(SurfNum)%ExtBoundCond /= 0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowSevereError('cannot be used with surface='//TRIM(Surface(SurfNum)%Name))
        CALL ShowContinueError('because that surface is not an exterior window.')
        ErrorsFound = .true.
      END IF
    END IF

    ! Require that storm window material be glass
    MatNum = StormWindow(StormWinNum)%StormWinMaterialNum
    IF(SurfNum > 0) THEN
      IF(MatNum == 0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError(TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//  &
            '" not found as storm window layer.')
        ErrorsFound = .true.
      ELSE
        IF(Material(MatNum)%Group /= WindowGlass) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
          CALL ShowContinueError(TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//  &
              'must be a WindowMaterial:Glazing or WindowMaterial:Glazing:RefractionExtinctionMethod')
          ErrorsFound = .true.
        END IF
      END IF
    END IF

    ! Error if base window has airflow control
    IF(SurfNum > 0) THEN
      IF(SurfaceWindow(SurfNum)%AirflowControlType /= 0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'"')
        CALL ShowContinueError(' cannot be used '//  &
          'because it is an airflow window (i.e., has WindowProperty:AirflowControl specified)')
        ErrorsFound = .true.
      END IF
    END IF

    ! Check for reversal of on and off times
    IF(SurfNum > 0) THEN
      IF((Latitude > 0.0d0  .AND. (StormWindow(StormWinNum)%MonthOn < StormWindow(StormWinNum)%MonthOff)) .OR. &
         (Latitude <= 0.0d0 .AND. (StormWindow(StormWinNum)%MonthOn > StormWindow(StormWinNum)%MonthOff))) THEN
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'" check times that storm window')
        CALL ShowContinueError('is put on (month='//TRIM(TrimSigDigits(StormWindow(StormWinNum)%MonthOn))//  &
           ', day='//TRIM(TrimSigDigits(StormWindow(StormWinNum)%DayOfMonthOn))//')'// &
           ' and taken off (month='//TRIM(TrimSigDigits(StormWindow(StormWinNum)%MonthOff))//  &
           ', day='//TRIM(TrimSigDigits(StormWindow(StormWinNum)%DayOfMonthOff))//');')
        CALL ShowContinueError('these times may be reversed for your building latitude='//  &
           TRIM(TrimSigDigits(Latitude,2))//' deg.')
      END IF
    END IF
  END DO

RETURN
END SUBROUTINE GetStormWindowData

SUBROUTINE GetWindowGapAirflowControlData(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   Feb 2003
          !       MODIFIED       June 2003, FCW: add destination = return air;
          !                        more error messages
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Reads in the window airflow control information from the input data file,
          ! interprets it and puts it in the SurfaceWindow derived type

          ! METHODOLOGY EMPLOYED: na
          ! REFERENCES: na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList, SameString
  USE ScheduleManager, ONLY: GetScheduleIndex

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound ! If errors found in input

          ! SUBROUTINE PARAMETER DEFINITIONS:na
          ! INTERFACE BLOCK SPECIFICATIONS:na
          ! DERIVED TYPE DEFINITIONS:na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

  INTEGER :: IOStat                   ! IO Status when calling get input subroutine
  INTEGER :: ControlNumAlpha          ! Number of control alpha names being passed
  INTEGER :: ControlNumProp           ! Number of control properties being passed
  INTEGER :: TotWinAirflowControl     ! Total window airflow control statements
  LOGICAL :: WrongSurfaceType         ! True if associated surface is not 2- or 3-pane exterior window
  INTEGER :: Loop
  INTEGER :: SurfNum                  ! Surface number
  INTEGER :: ConstrNum                ! Construction number
  INTEGER :: ConstrNumSh              ! Shaded Construction number
  INTEGER :: WSCptr                   ! Window shading control pointer
  INTEGER :: MatGapFlow               ! Material number of gas in airflow gap of window's construction
  INTEGER :: MatGapFlow1,MatGapFlow2  ! Material number of gas on either side of a between-glass shade/blind
                                      ! of the shaded construction of airflow window

        ! Get the total number of window airflow control statements
  cCurrentModuleObject='WindowProperty:AirflowControl'
  TotWinAirflowControl = GetNumObjectsFound(cCurrentModuleObject)
  IF(TotWinAirflowControl.EQ.0) RETURN

  DO Loop = 1,TotWinAirflowControl   ! Loop through all surfaces in the input...

    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,ControlNumAlpha, &
                       rNumericArgs,ControlNumProp,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)

    SurfNum = FindItemInList(cAlphaArgs(1),Surface%Name,TotSurfaces)
    IF(SurfNum == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//'" not found.')
      ErrorsFound =.true.
    END IF
          ! Check that associated surface is a 2- or 3-pane exterior window
    WrongSurfaceType = .FALSE.
    IF(SurfNum /= 0) THEN
      IF(Surface(SurfNum)%Class /= SurfaceClass_Window) WrongSurfaceType = .TRUE.
      IF(Surface(SurfNum)%Class == SurfaceClass_Window) THEN
        ConstrNum = Surface(SurfNum)%Construction
        IF(Construct(ConstrNum)%TotGlassLayers /= 2 .AND. Construct(ConstrNum)%TotGlassLayers /= 3) &
           WrongSurfaceType = .TRUE.
        IF(Surface(SurfNum)%ExtBoundCond /= ExternalEnvironment) WrongSurfaceType = .TRUE.
      END IF
      IF(WrongSurfaceType) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
          '" is not an exterior window with 2 or 3 glass layers.')
        ErrorsFound = .TRUE.
      END IF
    END IF

    ! Error if illegal airflow source
    IF(cAlphaArgs(2) /= 'INDOORAIR'.AND. cAlphaArgs(2) /= 'OUTDOORAIR') THEN
      ErrorsFound = .TRUE.
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '" invalid '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'"')
    END IF

    ! Error if illegal airflow destination
    IF(cAlphaArgs(3) /= 'INDOORAIR'.AND. cAlphaArgs(3) /= 'OUTDOORAIR'.AND. &
       cAlphaArgs(3) /= 'RETURNAIR'  ) THEN
      ErrorsFound = .TRUE.
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '" invalid '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'"')
    END IF


    ! Error if source = OutsideAir and destination = ReturnAir
    IF(cAlphaArgs(2) == 'OUTDOORAIR'.AND. cAlphaArgs(3) == 'RETURNAIR') THEN
      ErrorsFound = .TRUE.
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '" invalid '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'"')
      CALL ShowContinueError('..when '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'"')
    END IF

    ! Error if illegal airflow control type
    IF(cAlphaArgs(4) /= 'ALWAYSONATMAXIMUMFLOW'.AND. cAlphaArgs(4) /= 'ALWAYSOFF'.AND. &
          cAlphaArgs(4) /= 'SCHEDULEDONLY') THEN
      ErrorsFound = .TRUE.
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '" invalid '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'"')
    END IF

    ! Error if illegal value for Airflow Has Multiplier Schedule
    IF(cAlphaArgs(5) /= 'YES'.AND. cAlphaArgs(5) /= 'NO') THEN
      ErrorsFound = .TRUE.
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '" invalid '//TRIM(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'"')
    END IF

    ! Error if Airflow Control Type = ScheduledOnly and Airflow Has Multiplier Schedule = No
    IF(cAlphaArgs(4) == 'SCHEDULEDONLY'.AND. cAlphaArgs(5) == 'NO') THEN
      ErrorsFound = .TRUE.
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
         '" invalid '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'"')
      CALL ShowContinueError('..when '//TRIM(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//'"')

    END IF

    ! Warning if Airflow Control Type = AlwaysOnAtMaxFlow and Airflow Has Multiplier Schedule = Yes
    IF(cAlphaArgs(4) == 'ALWAYSONATMAXIMUMFLOW'.AND. cAlphaArgs(5) == 'YES') THEN
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
        'has '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'"')
      CALL ShowContinueError('..but '//TRIM(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//  &
        'If specified, the '//TRIM(cAlphaFieldNames(5))//' will be ignored.')
    END IF

    ! Warning if Airflow Control Type = AlwaysOff and Airflow Has Multiplier Schedule = Yes
    IF(cAlphaArgs(4) == 'ALWAYSOFF'.AND. cAlphaArgs(5) == 'YES') THEN
      CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
        'has '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'"')
      CALL ShowContinueError('..but '//TRIM(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//  &
        '". If specified, the '//TRIM(cAlphaFieldNames(5))//' will be ignored.')
    END IF

    IF(SurfNum > 0) THEN
      AirflowWindows = .TRUE.
      IF (SameString(cAlphaArgs(2),'IndoorAir')) THEN
        SurfaceWindow(SurfNum)%AirflowSource = AirFlowWindow_Source_IndoorAir
      ELSEIF (SameString(cAlphaArgs(2),'OutdoorAir')) THEN
        SurfaceWindow(SurfNum)%AirflowSource = AirFlowWindow_Source_OutdoorAir
      ENDIF
      IF (SameString(cAlphaArgs(3),'IndoorAir')) THEN
        SurfaceWindow(SurfNum)%AirflowDestination = AirFlowWindow_Destination_IndoorAir
      ELSEIF (SameString(cAlphaArgs(3),'OutdoorAir')) THEN
        SurfaceWindow(SurfNum)%AirflowDestination = AirFlowWindow_Destination_OutdoorAir
      ELSEIF (SameString(cAlphaArgs(3),'ReturnAir')) THEN
        SurfaceWindow(SurfNum)%AirflowDestination = AirFlowWindow_Destination_ReturnAir
      ENDIF
      IF (SameString(cAlphaArgs(4),'AlwaysOnAtMaximumFlow'))  THEN
        SurfaceWindow(SurfNum)%AirflowControlType = AirFlowWindow_ControlType_MaxFlow
      ELSEIF (SameString(cAlphaArgs(4),'AlwaysOff'))  THEN
        SurfaceWindow(SurfNum)%AirflowControlType = AirFlowWindow_ControlType_AlwaysOff
      ELSEIF (SameString(cAlphaArgs(4),'ScheduledOnly'))  THEN
        SurfaceWindow(SurfNum)%AirflowControlType = AirFlowWindow_ControlType_Schedule
      ENDIF
      SurfaceWindow(SurfNum)%MaxAirflow = rNumericArgs(1)
      IF(cAlphaArgs(4) == 'SCHEDULEDONLY' .AND. cAlphaArgs(5) == 'YES') THEN
        IF(lAlphaFieldBlanks(6)) THEN
          ErrorsFound = .TRUE.
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
             '", has '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'"')
          CALL ShowContinueError('..and '//TRIM(cAlphaFieldNames(5))//'="'//TRIM(cAlphaArgs(5))//  &
             '", but no '//TRIM(cAlphaFieldNames(6))//' specified.')
        ELSE
          SurfaceWindow(SurfNum)%AirflowHasSchedule = .TRUE.
          SurfaceWindow(SurfNum)%AirflowSchedulePtr = GetScheduleIndex(cAlphaArgs(6))
          IF(SurfaceWindow(SurfNum)%AirflowSchedulePtr==0) THEN
            ErrorsFound = .TRUE.
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
              '", invalid '//TRIM(cAlphaFieldNames(6))//'="'//TRIM(cAlphaArgs(6))//'"')
          END IF
        END IF
      END IF
      ! Warning if associated window is an interior window
      IF(Surface(SurfNum)%ExtBoundCond /= ExternalEnvironment .AND. .NOT.ErrorsFound) &
        CALL ShowWarningError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                 '", is an Interior window; cannot be an airflow window.')
      IF(.NOT.ErrorsFound) THEN
        ! Require that gas in airflow gap has type = air
        MatGapFlow = Construct(ConstrNum)%LayerPoint(2)
        IF(Construct(ConstrNum)%TotGlassLayers==3) MatGapFlow = Construct(ConstrNum)%LayerPoint(4)
        IF(Material(MatGapFlow)%GasType(1) /= 1) THEN
          ErrorsFound = .TRUE.
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
            '", Gas type not air in airflow gap of construction '//TRIM(Construct(ConstrNum)%Name))
        END IF
        ! Require that gas be air in airflow gaps on either side of a between glass shade/blind
        WSCptr = Surface(SurfNum)%WindowShadingControlPtr
        IF(WSCptr > 0) THEN
          IF(WindowShadingControl(WSCptr)%ShadingType==WSC_ST_BetweenGlassShade .OR. &
             WindowShadingControl(WSCptr)%ShadingType==WSC_ST_BetweenGlassBlind) THEN
             ConstrNumSh = WindowShadingControl(WSCptr)%ShadedConstruction
             IF(Construct(ConstrNum)%TotGlassLayers==2) THEN
               MatGapFlow1 = Construct(ConstrNumSh)%LayerPoint(2)
               MatGapFlow2 = Construct(ConstrNumSh)%LayerPoint(4)
             ELSE
               MatGapFlow1 = Construct(ConstrNumSh)%LayerPoint(4)
               MatGapFlow2 = Construct(ConstrNumSh)%LayerPoint(6)
             END IF
             IF(Material(MatGapFlow1)%GasType(1) /= 1 .OR. Material(MatGapFlow2)%GasType(1) /= 1) THEN
               ErrorsFound = .TRUE.
               CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
                   '", gas type must be air on either side of the shade/blind')
             END IF
          END IF
        END IF
      END IF
    END IF

  END DO  ! End of loop over window airflow controls

RETURN

END SUBROUTINE GetWindowGapAirflowControlData

SUBROUTINE GetOSCData(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2000
          !       MODIFIED       Jul 2011, M.J. Witte and C.O. Pedersen, add new fields to OSC for last T, max and min
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the OtherSideCoefficient data.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! Other Side Coefficient Definition
          ! OtherSideCoefficients,
          !       \memo This object sets the other side conditions for a surface in a variety of ways.
          !   A1, \field OtherSideCoeff Name
          !       \required-field
          !       \reference OSCNames
          !       \reference OutFaceEnvNames
          !   N1, \field Combined convective/radiative film coefficient
          !       \required-field
          !       \type real
          !       \note if>0, N1 becomes exterior convective/radiative film coefficient and other fields
          !       \note are used to calc outside air temp then exterior surface temp based on outside air
          !       \note and specified coefficient
          !       \note if<=0, then remaining fields calculate the outside surface temperature(?)
          !       \note following fields are used in the equation:
          !       \note SurfTemp=N7*TempZone + N4*OutsideDryBulb + N2*N3 + GroundTemp*N5 + WindSpeed*N6*OutsideDryBulb
          !   N2, \field User selected Constant Temperature
          !       \units C
          !       \type real
          !       \note This parameter will be overwritten by the values from the schedule(A2 below) if one is present
          !   N3, \field Coefficient modifying the user selected constant temperature
          !       \note This coefficient is used even with a schedule.  It should normally be 1.0 in that case
          !   N4, \field Coefficient modifying the external dry bulb temperature
          !       \type real
          !   N5, \field Coefficient modifying the ground temperature
          !       \type real
          !   N6, \field Coefficient modifying the wind speed term (s/m)
          !       \type real
          !   N7, \field Coefficient modifying the zone air temperature part of the equation
          !       \type real
          !   A2, \field ScheduleName for constant temperature
          !       \note Name of Schedule for values of "const" temperature.
          !       \note Schedule values replace N2 - User selected constant temperature.
          !       \type object-list
          !       \object-list ScheduleNames
          !   A3, \field Sinusoidal Variation of Constant Temperature Coefficient
          !       \note Optionally used to vary Constant Temperature Coefficient with unitary sine wave
          !       \type choice
          !       \key Yes
          !       \key No
          !       \default No
          !   N8; \field Period of Sinusoidal Variation
          !       \note Use with sinusoidal variation to define the time period
          !       \type real
          !       \units hr
          !       \default 24
          !  N9, \field Previous Other Side Temperature Coefficient
          !      \note This coeffient multiplies the other side temperature result from the
          !      \note previous zone timestep
          !      \type real
          !      \default 0
          ! N10, \field Minimum Other Side Temperature
          !      \type real
          !      \units C
          !      \default -100
          ! N11; \field Maximum Other Side Temperature
          !      \type real
          !      \units C
          !      \default 200


          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList, VerifyName, SameString
  USE ScheduleManager, ONLY: GetScheduleIndex
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: OSCFormat1="('! <Other Side Coefficients>,Name,"// &
                            "Combined convective/radiative film coefficient {W/m2-K},"// &
                            "User selected Constant Temperature {C},Coefficient modifying the constant temperature term,"// &
                            "Coefficient modifying the external dry bulb temperature term,"//  &
                            "Coefficient modifying the ground temperature term,"//  &
                            "Coefficient modifying the wind speed term {s/m},"//  &
                            "Coefficient modifying the zone air temperature term,"//  &
                            "Constant Temperature Schedule Name,"//  &
                            "Sinusoidal Variation,"//  &
                            "Period of Sinusoidal Variation,"//  &
                            "Previous Other Side Temperature Coefficient,"//  &
                            "Minimum Other Side Temperature {C},"//  &
                            "Maximum Other Side Temperature {C}')"

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER NumAlphas
  INTEGER NumProps
  INTEGER Loop
  INTEGER IOSTAT
  INTEGER OSCNum
  LOGICAL ErrorInName
  LOGICAL IsBlank
  CHARACTER(len=52) cOSCLimitsString

  cCurrentModuleObject='SurfaceProperty:OtherSideCoefficients'
  TotOSC=GetNumObjectsFound(cCurrentModuleObject)
  ALLOCATE(OSC(TotOSC))

  OSCNum=0
  DO Loop=1,TotOSC
    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlphas, &
                       rNumericArgs,NumProps,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    ErrorInName=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),OSC%Name,OSCNum,ErrorInName,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (ErrorInName) THEN
      ErrorsFound=.true.
      CYCLE
    ENDIF

    OSCNum=OSCNum+1
    OSC(OSCNum)%Name            = cAlphaArgs(1)
    OSC(OSCNum)%SurfFilmCoef    = rNumericArgs(1)
    OSC(OSCNum)%ConstTemp       = rNumericArgs(2)  !  This will be replaced if  schedule is used
    OSC(OSCNum)%ConstTempCoef   = rNumericArgs(3)  !  This multiplier is used (even with schedule).  It should normally be 1.0
    OSC(OSCNum)%ExtDryBulbCoef  = rNumericArgs(4)
    OSC(OSCNum)%GroundTempCoef  = rNumericArgs(5)
    OSC(OSCNum)%WindSpeedCoef   = rNumericArgs(6)
    OSC(OSCNum)%ZoneAirTempCoef = rNumericArgs(7)
    OSC(OSCNum)%SinusoidPeriod  = rNumericArgs(8)

    IF(( .not. lAlphaFieldBlanks(2)) .and. (NumAlphas /= 1)) THEN  !  Const temp will come from schedule specified below.
      OSC(OSCNum)%ConstTempScheduleName = cAlphaArgs(2)
      IF (OSC(OSCNum)%ConstTempScheduleName /= Blank) THEN
        OSC(OSCNum)%ConstTempScheduleIndex=GetScheduleIndex(OSC(OSCNum)%ConstTempScheduleName)
        IF (OSC(OSCNum)%ConstTempScheduleIndex == 0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
            '", invalid '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2)))
          ErrorsFound=.true.
        ENDIF
      ENDIF
    ENDIF

    IF (.NOT. lAlphaFieldBlanks(3)) THEN

      IF (SameString(cAlphaArgs(3), 'No')) THEN
        OSC(OSCNum)%SinusoidalConstTempCoef = .FALSE.
      ELSEIF (SameString(cAlphaArgs(3), 'Yes')) THEN
        OSC(OSCNum)%SinusoidalConstTempCoef = .True.
      ELSE
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
            '", invalid '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3)))
        ErrorsFound=.true.
      ENDIF
    ENDIF

    IF (rNumericArgs(1) > 0.0d0 .and. .not. ANY(rNumericArgs(3:7) /= 0.0d0) &
         .AND. (.not. OSC(OSCNum)%SinusoidalConstTempCoef)) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
              '" has zeros for all coefficients.')
      CALL ShowContinueError('...The outdoor air temperature for surfaces using this OtherSideCoefficients '//  &
        'object will always be 0C.')
    ENDIF

    IF (rNumericArgs(1) <= 0.0d0 .and. .not. ANY(rNumericArgs(3:7) /= 0.0d0) &
         .AND. (.not. OSC(OSCNum)%SinusoidalConstTempCoef)) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//'="'//TRIM(cAlphaArgs(1))//  &
              '" has zeros for all coefficients.')
      CALL ShowContinueError('...The outside surface temperature for surfaces using this OtherSideCoefficients '//  &
        'object will always be 0C.')
    ENDIF

    OSC(OSCNum)%TPreviousCoef  = rNumericArgs(9)

    IF (.NOT. lNumericFieldBlanks(10)) THEN
      OSC(OSCNum)%MinLimitPresent = .TRUE.
      OSC(OSCNum)%MinTempLimit   = rNumericArgs(10)
      cOSCLimitsString = RoundSigDigits(rNumericArgs(10),3)
    ELSE
      cOSCLimitsString = 'N/A'
    ENDIF
    IF (.NOT. lNumericFieldBlanks(11)) THEN
      OSC(OSCNum)%MaxLimitPresent = .TRUE.
      OSC(OSCNum)%MaxTempLimit   = rNumericArgs(11)
      cOSCLimitsString = TRIM(cOSCLimitsString)//','//TRIM(RoundSigDigits(rNumericArgs(10),3))
    ELSE
      cOSCLimitsString = TRIM(cOSCLimitsString)//','//'N/A'
    ENDIF

  ENDDO

  DO Loop=1,TotOSC
    IF (Loop == 1) THEN
      WRITE(OutputFileInits,OSCFormat1)
    ENDIF
    IF (OSC(Loop)%SurfFilmCoef > 0.0d0) THEN
      cAlphaArgs(1)=RoundSigDigits(OSC(Loop)%SurfFilmCoef,3)
      CALL SetupOutputVariable('Surface Other Side Coefficients Exterior Air Drybulb Temperature [C]',OSC(Loop)%OSCTempCalc, &
                                 'System','Average',OSC(Loop)%Name)
    ELSE
      cAlphaArgs(1)='N/A'
    ENDIF
    IF (OSC(Loop)%ConstTempScheduleIndex /= 0) THEN
      cAlphaArgs(2)=OSC(Loop)%ConstTempScheduleName
      WRITE(OutputFileInits,'(A)') 'Other Side Coefficients,'//TRIM(OSC(Loop)%Name)//','//TRIM(cAlphaArgs(1))//','// &
        'N/A,'//TRIM(RoundSigDigits(OSC(Loop)%ConstTempCoef,3))//','// &
        TRIM(RoundSigDigits(OSC(Loop)%ExtDryBulbCoef,3))//','//TRIM(RoundSigDigits(OSC(Loop)%GroundTempCoef,3))//','// &
        TRIM(RoundSigDigits(OSC(Loop)%WindSpeedCoef,3))//','//TRIM(RoundSigDigits(OSC(Loop)%ZoneAirTempCoef,3))//','// &
        TRIM(cAlphaArgs(2))//','//TRIM(cAlphaArgs(3))//','//TRIM(RoundSigDigits(OSC(Loop)%SinusoidPeriod,3))//','// &
        TRIM(RoundSigDigits(OSC(Loop)%TPreviousCoef,3))//','//TRIM(cOSCLimitsString)
    ELSE
      cAlphaArgs(2)='N/A'
      WRITE(OutputFileInits,'(A)') 'Other Side Coefficients,'//TRIM(OSC(Loop)%Name)//','//TRIM(cAlphaArgs(1))//','// &
        TRIM(RoundSigDigits(OSC(Loop)%ConstTemp,2))//','//TRIM(RoundSigDigits(OSC(Loop)%ConstTempCoef,3))//','// &
        TRIM(RoundSigDigits(OSC(Loop)%ExtDryBulbCoef,3))//','//TRIM(RoundSigDigits(OSC(Loop)%GroundTempCoef,3))//','// &
        TRIM(RoundSigDigits(OSC(Loop)%WindSpeedCoef,3))//','//TRIM(RoundSigDigits(OSC(Loop)%ZoneAirTempCoef,3))//','// &
        TRIM(cAlphaArgs(2))//','//TRIM(cAlphaArgs(3))//','//TRIM(RoundSigDigits(OSC(Loop)%SinusoidPeriod,3))//','// &
        TRIM(RoundSigDigits(OSC(Loop)%TPreviousCoef,3))//','//TRIM(cOSCLimitsString)
    ENDIF

  ENDDO
  RETURN

END SUBROUTINE GetOSCData

SUBROUTINE GetOSCMData(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   November 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the OtherSideConditionsModel data.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! derived from GetOSCData subroutine by Linda Lawrie

          !  OtherSideConditionsModel,
          !      \memo This object sets up modifying the other side conditions for a surface from other model results.
          !  A1, \field OtherSideConditionsModel Name
          !      \required-field
          !      \reference OSCMNames
          !      \reference OutFaceEnvNames
          !  A2; \field Type of Model to determine Boundary Conditions
          !      \type choice
          !      \key Transpired Collector
          !      \key Vented PV Cavity
          !      \key Hybrid PV Transpired Collector

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList, VerifyName

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: OSCMFormat1="('! <Other Side Conditions Model>,Name,Class')"

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER NumAlphas
  INTEGER NumProps
  INTEGER Loop
  INTEGER IOSTAT
  INTEGER OSCMNum
  LOGICAL ErrorInName
  LOGICAL IsBlank

  cCurrentModuleObject='SurfaceProperty:OtherSideConditionsModel'
  TotOSCM=GetNumObjectsFound(cCurrentModuleObject)
  ALLOCATE(OSCM(TotOSCM))
  ! OSCM is already initialized in derived type defn.

  OSCMNum=0
  DO Loop=1,TotOSCM
    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NumAlphas, &
                       rNumericArgs,NumProps,IOSTAT)
    ErrorInName=.false.
    IsBlank=.false.
    CALL VerifyName(cAlphaArgs(1),OSCM%Name,OSCMNum,ErrorInName,IsBlank,TRIM(cCurrentModuleObject)//' Name')
    IF (ErrorInName) THEN
      ErrorsFound=.true.
      CYCLE
    ENDIF

    OSCMNum=OSCMNum+1
    OSCM(OSCMNum)%Name            = cAlphaArgs(1)
    ! Note no validation of the below at this time:
    OSCM(OSCMNum)%Class           = cAlphaArgs(2)
    ! setup output vars for modeled coefficients
    CALL SetupOutputVariable('Surface Other Side Conditions Modeled Convection Air Temperature [C]',OSCM(OSCMNum)%TConv, &
                               'System','Average',OSCM(OSCMNum)%Name)
    CALL SetupOutputVariable('Surface Other Side Conditions Modeled Convection Heat Transfer Coefficient [W/m2-K]', &
                              OSCM(OSCMNum)%HConv, &
                               'System','Average',OSCM(OSCMNum)%Name)
    CALL SetupOutputVariable('Surface Other Side Conditions Modeled Radiation Temperature [C]',OSCM(OSCMNum)%TRad, &
                               'System','Average',OSCM(OSCMNum)%Name)
    CALL SetupOutputVariable('Surface Other Side Conditions Modeled Radiation Heat Transfer Coefficient [W/m2-K]', &
                              OSCM(OSCMNum)%HRad, &
                               'System','Average',OSCM(OSCMNum)%Name)

    IF (AnyEnergyManagementSystemInModel) THEN
      CALL SetupEMSActuator('Other Side Boundary Conditions',  OSCM(OSCMNum)%Name, &
                           'Convection Bulk Air Temperature', '[C]', &
                           OSCM(OSCMNum)%EMSOverrideOnTConv, &
                           OSCM(OSCMNum)%EMSOverrideTConvValue)
      CALL SetupEMSActuator('Other Side Boundary Conditions',  OSCM(OSCMNum)%Name, &
                           'Convection Heat Transfer Coefficient', '[W/m2-K]', &
                           OSCM(OSCMNum)%EMSOverrideOnHConv, &
                           OSCM(OSCMNum)%EMSOverrideHConvValue)
      CALL SetupEMSActuator('Other Side Boundary Conditions',  OSCM(OSCMNum)%Name, &
                           'Radiation Effective Temperature', '[C]', &
                           OSCM(OSCMNum)%EMSOverrideOnTRad, &
                           OSCM(OSCMNum)%EMSOverrideTRadValue)
      CALL SetupEMSActuator('Other Side Boundary Conditions',  OSCM(OSCMNum)%Name, &
                           'Radiation Linear Heat Transfer Coefficient', '[W/m2-K]', &
                           OSCM(OSCMNum)%EMSOverrideOnHrad, &
                           OSCM(OSCMNum)%EMSOverrideHradValue)
    ENDIF
  ENDDO

  DO Loop=1,TotOSCM
    IF (Loop == 1) THEN
      WRITE(OutputFileInits,OSCMFormat1)
    ENDIF
    WRITE(OutputFileInits,'(A)') 'Other Side Conditions Model,'//TRIM(OSCM(Loop)%Name)//','//TRIM(OSCM(Loop)%Class)
  ENDDO

  RETURN

END SUBROUTINE GetOSCMData

SUBROUTINE GetMovableInsulationData(ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   May 2000
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine gets the movable insulation data that can be associated with
          ! a surface.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
  ! Movable Insulation Definition
  ! SurfaceControl:MovableInsulation,
  !       \memo Exterior or Interior Insulation on opaque surfaces
  !   A1, \field Insulation Type
  !       \required-field
  !       \type choice
  !       \key Outside
  !       \key Inside
  !   A2, \field Surface Name
  !       \required-field
  !       \type object-list
  !       \object-list SurfaceNames
  !   A3, \field Material Name
  !       \required-field
  !       \object-list MaterialName
  !   A4; \field Schedule Name
  !        \required-field
  !        \type object-list
  !        \object-list ScheduleNames

          ! USE STATEMENTS:
    USE DataIPShortCuts
    USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList, VerifyName, SameString
    USE ScheduleManager, ONLY: GetScheduleIndex
    USE General, ONLY: TrimSigDigits,RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound ! If errors found in input

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER NAlphas
  INTEGER NNums
  INTEGER IOSTAT
  INTEGER Loop
  INTEGER NMatInsul
  INTEGER SurfNum
  INTEGER MaterNum
  INTEGER SchNum
  INTEGER InslType

  cCurrentModuleObject='SurfaceControl:MovableInsulation'
  NMatInsul=GetNumObjectsFound(cCurrentModuleObject)
  DO Loop=1,NMatInsul
    CALL GetObjectItem(cCurrentModuleObject,Loop,cAlphaArgs,NAlphas,rNumericArgs,NNums,IOSTAT,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
    SurfNum=FindItemInList(cAlphaArgs(2),SurfaceTmp%Name,TotSurfaces)
    MaterNum=FindItemInList(cAlphaArgs(3),Material%Name,TotMaterials)
    SchNum=GetScheduleIndex(cAlphaArgs(4))
    IF (SameString(cAlphaArgs(1),'Outside')) THEN
      InslType=1
    ELSEIF (SameString(cAlphaArgs(1),'Inside')) THEN
      InslType=2
    ELSE
      InslType=0
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//', '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//  &
         '", invalid data.')
      CALL ShowContinueError(' invalid '//TRIM(cAlphaFieldNames(1))//'="'//TRIM(cAlphaArgs(1))//  &
         '", [should be Inside or Outside]')
      ErrorsFound=.false.
    ENDIF
    IF (SurfNum == 0) THEN
      CALL ShowSevereError(TRIM(cCurrentModuleObject)//', '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//  &
         '", invalid data.')
      CALL ShowContinueError(' invalid (not found) '//TRIM(cAlphaFieldNames(2)))
      ErrorsFound=.true.
    ELSE
      IF (MaterNum == 0) THEN
        CALL ShowSevereError(TRIM(cCurrentModuleObject)//', '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//  &
           '", invalid data.')
        CALL ShowContinueError(' invalid (not found) '//TRIM(cAlphaFieldNames(3))//'="'//TRIM(cAlphaArgs(3))//'"')
        ErrorsFound=.true.
      ELSE
        IF (SchNum == 0) THEN
          CALL ShowSevereError(TRIM(cCurrentModuleObject)//', '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//  &
           '", invalid data.')
          CALL ShowContinueError(' invalid (not found) '//TRIM(cAlphaFieldNames(4))//'="'//TRIM(cAlphaArgs(4))//'"')
          ErrorsFound=.true.
        ELSE
          SELECT CASE (InslType)
          CASE (1)
            IF (SurfaceTmp(SurfNum)%MaterialMovInsulExt > 0) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//', '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//  &
                 '", already assigned.')
              CALL ShowContinueError('"Outside", was already assigned Material="'//  &
                 trim(Material(SurfaceTmp(SurfNum)%MaterialMovInsulInt)%Name)//'".')
              CALL ShowContinueError('attempting to assign Material="'//trim(Material(MaterNum)%Name)//'".')
              ErrorsFound=.true.
            ENDIF
            SurfaceTmp(SurfNum)%MaterialMovInsulExt=MaterNum
            SurfaceTmp(SurfNum)%SchedMovInsulExt=SchNum
            IF (Material(MaterNum)%Resistance <= 0.0d0) THEN
              IF (Material(MaterNum)%Conductivity <= 0.0d0 .or. Material(MaterNum)%Thickness <= 0.0d0) THEN
                CALL ShowSevereError(TRIM(cCurrentModuleObject)//', '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//  &
                   '", invalid material.')
                CALL ShowContinueError('"Outside", invalid material for movable insulation.')
                CALL ShowContinueError('Material="'//trim(Material(MaterNum)%Name)//'",'//  &
                   'Resistance=['//trim(RoundSigDigits(Material(MaterNum)%Resistance,3))//  &
                   '], must be > 0 for use in Movable Insulation.')
                ErrorsFound=.true.
              ELSEIF (Material(MaterNum)%Conductivity > 0.0d0) THEN
                Material(MaterNum)%Resistance=Material(MaterNum)%Thickness/Material(MaterNum)%Conductivity
              ENDIF
            ENDIF
            IF (Material(MaterNum)%Conductivity <= 0.0d0) THEN
              IF (Material(MaterNum)%Resistance <= 0.0d0) THEN
                CALL ShowSevereError(TRIM(cCurrentModuleObject)//', '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//  &
                   '", invalid material.')
                CALL ShowContinueError('"Outside", invalid material for movable insulation.')
                CALL ShowContinueError('Material="'//trim(Material(MaterNum)%Name)//'",'//  &
                   'Conductivity=['//trim(RoundSigDigits(Material(MaterNum)%Conductivity,3))//  &
                   '], must be > 0 for use in Movable Insulation.')
                ErrorsFound=.true.
              ENDIF
            ENDIF
          CASE (2)
            IF (SurfaceTmp(SurfNum)%MaterialMovInsulInt > 0) THEN
              CALL ShowSevereError(TRIM(cCurrentModuleObject)//', '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//  &
                 '", already assigned.')
              CALL ShowContinueError('"Inside", was already assigned Material="'//  &
                 trim(Material(SurfaceTmp(SurfNum)%MaterialMovInsulInt)%Name)//'".')
              CALL ShowContinueError('attempting to assign Material="'//trim(Material(MaterNum)%Name)//'".')
              ErrorsFound=.true.
            ENDIF
            SurfaceTmp(SurfNum)%MaterialMovInsulInt=MaterNum
            SurfaceTmp(SurfNum)%SchedMovInsulInt=SchNum
            IF (Material(MaterNum)%Resistance <= 0.0d0) THEN
              IF (Material(MaterNum)%Conductivity <= 0.0d0 .or. Material(MaterNum)%Thickness <= 0.0d0) THEN
                CALL ShowSevereError(TRIM(cCurrentModuleObject)//', '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//  &
                   '", invalid material.')
                CALL ShowContinueError('"Inside", invalid material for movable insulation.')
                CALL ShowContinueError('Material="'//trim(Material(MaterNum)%Name)//'",'//  &
                   'Resistance=['//trim(RoundSigDigits(Material(MaterNum)%Resistance,3))//  &
                   '], must be > 0 for use in Movable Insulation.')
                ErrorsFound=.true.
              ELSEIF (Material(MaterNum)%Conductivity > 0.0d0) THEN
                Material(MaterNum)%Resistance=Material(MaterNum)%Thickness/Material(MaterNum)%Conductivity
              ENDIF
            ENDIF
          CASE DEFAULT
          END SELECT
          IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Window) THEN
            CALL ShowSevereError(TRIM(cCurrentModuleObject)//', '//TRIM(cAlphaFieldNames(2))//'="'//TRIM(cAlphaArgs(2))//'"')
            CALL ShowContinueError('invalid use on a Window. Use WindowProperty:ShadingControl instead.')
            ErrorsFound=.true.
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDDO

  RETURN

END SUBROUTINE GetMovableInsulationData

SUBROUTINE CalculateZoneVolume(ErrorsFound,CeilingHeightEntered)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code
          !       DATE WRITTEN   1992-1994
          !       MODIFIED       Sep 2007
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calculates the volume (m3) of a zone using the
          ! surfaces as possible.

          ! METHODOLOGY EMPLOYED:
          ! Uses surface area information for calculations.  Modified to use the
          ! user-entered ceiling height (x floor area, if applicable) instead of using
          ! the calculated volume when the user enters the ceiling height.

          ! REFERENCES:
          ! Legacy Code (IBLAST)

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList,GetNumSectionsFound
  USE Vectors
  USE General, ONLY: RoundSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  LOGICAL, INTENT(INOUT) :: ErrorsFound ! If errors found in input
  LOGICAL, DIMENSION(:), INTENT(IN)    :: CeilingHeightEntered

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: VolFmt="(F20.2)"

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64) MinimumVolume        ! The minimum allowable Zone volume (equivalent to a ceiling height of 2.5 meters)
  REAL(r64) SumAreas             ! Sum of the Zone surface areas that are not "internal mass"
  REAL(r64) SurfCount            ! Surface Count
  INTEGER SurfNum           ! Loop counter for surfaces
  INTEGER ZoneNum           ! Loop counter for Zones
  LOGICAL ErrorFlag
  REAL(r64) TempVolume           ! Temporary for calculating volume
  type(polyhedron) :: ZoneStruct
  integer, allocatable, dimension(:) :: surfacenotused
  integer :: notused
  INTEGER NFaces
  INTEGER NActFaces
  REAL(r64) CalcVolume
  LOGICAL initmsg
  integer iside
  LOGICAL :: ShowZoneSurfaces=.false.
  LOGICAL :: ShowZoneSurfaceHeaders=.true.
  INTEGER, SAVE :: ErrCount=0

  initmsg=.true.
  ShowZoneSurfaces=(GetNumSectionsFound('SHOWZONESURFACES_DEBUG') > 0)

  DO ZoneNum = 1, NumOfZones

    IF (.not. Zone(ZoneNum)%HasFloor) THEN
      CALL ShowWarningError('No floor exists in Zone="'//TRIM(Zone(ZoneNum)%Name)//  &
         '", zone floor area is zero. All values for this zone that are entered per floor area will be zero.')
    ENDIF

    SumAreas=0.0d0
    SurfCount=0.0d0
    NFaces=Zone(ZoneNum)%SurfaceLast-Zone(ZoneNum)%SurfaceFirst+1
    notused=0
    ZoneStruct%NumSurfaceFaces=NFaces
    ALLOCATE(ZoneStruct%SurfaceFace(NFaces))
    NActFaces=0
    ALLOCATE(surfacenotused(NFaces))
    surfacenotused=0

    DO SurfNum=Zone(ZoneNum)%SurfaceFirst,Zone(ZoneNum)%SurfaceLast

      ! Only include Base Surfaces in Calc.

      IF (Surface(SurfNum)%Class /= SurfaceClass_Wall .and. Surface(SurfNum)%Class /= SurfaceClass_Floor .and. &
          Surface(SurfNum)%Class /= SurfaceClass_Roof) THEN
        notused=notused+1
        surfacenotused(notused)=SurfNum
        CYCLE
      ENDIF

      NActFaces=NActFaces+1
      ALLOCATE(ZoneStruct%SurfaceFace(NActFaces)%FacePoints(Surface(SurfNum)%Sides))
      ZoneStruct%SurfaceFace(NActFaces)%NSides=Surface(SurfNum)%Sides
      ZoneStruct%SurfaceFace(NActFaces)%SurfNum=SurfNum
      ZoneStruct%SurfaceFace(NActFaces)%FacePoints(1:Surface(SurfNum)%Sides)=Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)
      CALL CreateNewellAreaVector(ZoneStruct%SurfaceFace(NActFaces)%FacePoints,ZoneStruct%SurfaceFace(NActFaces)%NSides,  &
                              ZoneStruct%SurfaceFace(NActFaces)%NewellAreaVector)
      SumAreas=SumAreas+VecLength(ZoneStruct%SurfaceFace(NActFaces)%NewellAreaVector)
    ENDDO
    ZoneStruct%NumSurfaceFaces=NActFaces
    SurfCount=REAL(NActFaces,r64)
    CALL CalcPolyhedronVolume(ZoneStruct,CalcVolume)

    IF(Zone(ZoneNum)%FloorArea > 0.0d0) THEN
      MinimumVolume=Zone(ZoneNum)%FloorArea * 2.5d0
      IF (Zone(ZoneNum)%CeilingHeight > 0.0d0) THEN
        MinimumVolume=Zone(ZoneNum)%FloorArea*Zone(ZoneNum)%CeilingHeight
      ENDIF
    ELSE
      IF (SurfCount > 0) THEN
        MinimumVolume=SQRT(SumAreas/SurfCount)**3
      ELSE
        MinimumVolume=0.0d0
      ENDIF
    ENDIF
    IF (CalcVolume > 0.0d0) THEN
      TempVolume=CalcVolume
    ELSE
      TempVolume=MinimumVolume
    ENDIF

    IF (Zone(ZoneNum)%Volume > 0.0d0) THEN   ! User entered zone volume, produce message if not near calculated
      IF (TempVolume > 0.0d0) THEN
        IF (ABS(TempVolume-Zone(ZoneNum)%Volume)/Zone(ZoneNum)%Volume  > .05d0) THEN
          ErrCount=ErrCount+1
          IF (ErrCount == 1 .and. .not. DisplayExtraWarnings) THEN
            IF (initmsg) THEN
              CALL ShowMessage('Note that the following warning(s) may/will occur if you have not enclosed your zone completely.')
              initmsg=.false.
            ENDIF
            CALL ShowWarningError('Entered Zone Volumes differ from calculated zone volume(s).')
            CALL ShowContinueError('...use Output:Diagnostics,DisplayExtraWarnings; to show more details on individual zones.')
          ENDIF
          IF (DisplayExtraWarnings) THEN
            IF (initmsg) THEN
              CALL ShowMessage('Note that the following warning(s) may/will occur if you have not enclosed your zone completely.')
              initmsg=.false.
            ENDIF
            ! Warn user of using specified Zone Volume
            CALL ShowWarningError('Entered Volume entered for Zone="'//TRIM(Zone(ZoneNum)%Name)// &
                                  '" significantly different from calculated Volume')
            CALL ShowContinueError('Entered Zone Volume value='//TRIM(RoundSigDigits(Zone(ZoneNum)%Volume,2))//  &
                                   ', Calculated Zone Volume value='//TRIM(RoundSigDigits(TempVolume,2))// &
                                  ', entered volume will be used in calculations.')
          ENDIF
        ENDIF
      ENDIF
    ELSEIF (CeilingHeightEntered(ZoneNum)) THEN   ! User did not enter zone volume, but entered ceiling height
      IF (Zone(ZoneNum)%FloorArea > 0.0d0) THEN
        Zone(ZoneNum)%Volume=Zone(ZoneNum)%FloorArea*Zone(ZoneNum)%CeilingHeight
      ELSE ! ceiling height entered but floor area zero
        Zone(ZoneNum)%Volume=TempVolume
      ENDIF
    ELSE  ! Neither ceiling height nor volume entered
      Zone(ZoneNum)%Volume=TempVolume
    ENDIF

    IF (Zone(ZoneNum)%Volume <= 0.0d0) THEN
      CALL ShowSevereError('Indicated Zone Volume <= 0.0 for Zone='//TRIM(Zone(ZoneNum)%Name))
      CALL ShowContinueError('Zone Volume calculated was='//TRIM(RoundSigDigits(Zone(ZoneNum)%Volume,2)))
    ENDIF

    IF (ShowZoneSurfaces) THEN
      IF (ShowZoneSurfaceHeaders) THEN
        write(outputfiledebug,*) '==================================='
        write(outputfiledebug,*) 'showing zone surfaces used and not used in volume calculation'
        write(outputfiledebug,*) 'for volume calculation, only floors, walls and roofs/ceilings are used'
        write(outputfiledebug,*) 'surface class, 1=wall, 2=floor, 3=roof/ceiling'
        write(outputfiledebug,*) 'unused surface class(es), 5=internal mass, 11=window, 12=glass door'
        write(outputfiledebug,*) '                          13=door, 14=shading, 15=overhang, 16=fin'
        write(outputfiledebug,*) '                          17=TDD Dome, 18=TDD Diffuser'
        ShowZoneSurfaceHeaders=.false.
      ENDIF
      write(outputfiledebug,*) '==================================='
      write(outputfiledebug,*) 'zone=',trim(zone(zonenum)%name),' calc volume=',calcvolume
      write(outputfiledebug,*) ' nsurfaces=',nfaces,' nactual=',nactfaces
    ENDIF
    do SurfNum=1,ZoneStruct%NumSurfaceFaces
      IF (ShowZoneSurfaces) THEN
        if (surfnum <= nactfaces) then
          write(outputfiledebug,*) 'surface=',zonestruct%surfaceface(surfnum)%surfnum,  &
                                   ' nsides=',zonestruct%surfaceface(surfnum)%Nsides
          write(outputfiledebug,*) 'surface name=',trim(surface(zonestruct%surfaceface(surfnum)%surfnum)%name),  &
                                    ' class=',surface(zonestruct%surfaceface(surfnum)%surfnum)%class
          write(outputfiledebug,*) 'area=',surface(zonestruct%surfaceface(surfnum)%surfnum)%grossarea
          do iside=1, zonestruct%surfaceface(surfnum)%Nsides
            write(outputfiledebug,*) zonestruct%surfaceface(surfnum)%facepoints(iside)
          enddo
        endif
      ENDIF
      deallocate(ZoneStruct%SurfaceFace(SurfNum)%FacePoints)
    enddo
    IF (ShowZoneSurfaces) THEN
      do surfnum=1,notused
        write(outputfiledebug,*) 'notused:surface=',surfacenotused(surfnum),' name=',  &
          trim(surface(surfacenotused(surfnum))%name),  &
          ' class=',surface(surfacenotused(surfnum))%class
      enddo
    ENDIF

    deallocate(ZoneStruct%SurfaceFace)
    deallocate(surfacenotused)

  END DO

  ErrorFlag=.false.
  DO ZoneNum=1,NumOfZones
    IF (Zone(ZoneNum)%Volume <= 0.0d0) ErrorFlag=.true.
  END DO
  IF (ErrorFlag) THEN
    CALL ShowSevereError('All ZONE Volumes must be > 0.0')
    ErrorsFound=.true.
  ENDIF

  RETURN

END SUBROUTINE CalculateZoneVolume

SUBROUTINE ProcessSurfaceVertices(ThisSurf,ErrorsFound)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Legacy Code (Walton)
          !       DATE WRITTEN   1976
          !       MODIFIED        FW, Mar 2002: Add triangular windows
          !                       FW, May 2002: modify test for 4-sided but non-rectangular subsurfaces
          !                       FW, Sep 2002: add shape for base surfaces (walls and detached shading surfaces)
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine processes each surface into the vertex representation used
          ! by the shading procedures.
          ! This routine depends on the surfaces coming in:
          !  Base Surface
          !   SubSurface (Window/Door)
          !   SubSurface
          !  Base Surface
          !   SubSurface
          !   SubSurface
          !  Thus, some attributes of the "Base Surface" must be SAVEd.

          ! METHODOLOGY EMPLOYED:
          ! Detached Shading, Base Surfaces, Attached Shading surfaces are represented in the
          ! same manner as original.  Subsurfaces (windows, doors) are a "relative coordinate".

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE General, ONLY: TrimSigDigits
  USE Vectors

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)    :: ThisSurf    ! Surface Number
  LOGICAL, INTENT(INOUT) :: ErrorsFound

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: RoutineName='ProcessSurfaceVertices: '

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na

          ! LOCAL VARIABLES
!  REAL(r64) :: X00    ! Intermediate Result
!  REAL(r64) :: Y00    ! Intermediate Result
!  REAL(r64) :: Z00    ! Intermediate Result
!  REAL(r64) :: A(3,3) ! Surface Rotation Matrix
!  REAL(r64), SAVE :: B(3,3) ! Inverse Of Rotation Matrix
  REAL(r64) :: X1    ! Intermediate Result
  REAL(r64) :: Y1    ! Intermediate Result
  REAL(r64) :: Z1    ! Intermediate Result
  REAL(r64), SAVE :: XSHIFT  ! Shift of X to Lower Left Corner
  REAL(r64), SAVE :: YSHIFT  ! Shift of Y to Lower Left Corner
  REAL(r64)  :: XLLC   ! X-coordinate of lower left corner
  REAL(r64)  :: YLLC   ! Y-coordinate of lower left corner
  REAL(r64)  :: ZLLC   ! Z-coordinate of lower left corner
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE  :: X
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE  :: Y
  REAL(r64), ALLOCATABLE, DIMENSION(:), SAVE  :: Z
  LOGICAL :: OneTimeFlag=.true.
!  INTEGER :: I  ! Loop Control
!  INTEGER :: J  ! Loop Control
  INTEGER :: N  ! Vertex Number in Loop
  INTEGER ThisBaseSurface  ! Current base surface
  REAL(r64) Xp
  REAL(r64) Yp
  REAL(r64) Zp
  REAL(r64), SAVE :: BaseCosAzimuth
  REAL(r64), SAVE :: BaseCosTilt
  REAL(r64), SAVE :: BaseSinAzimuth
  REAL(r64), SAVE :: BaseSinTilt
  REAL(r64), SAVE :: BaseXLLC
  REAL(r64), SAVE :: BaseYLLC
  REAL(r64), SAVE :: BaseZLLC
  REAL(r64) SurfWorldAz  ! Surface Azimuth (facing)
  REAL(r64) SurfTilt     ! Surface Tilt
  type (planeeq) BasePlane
!  type (planeeq) PlanarEQ
!  type (vector), dimension(3) :: TriVect
!  REAL(r64) testval
!  INTEGER ploop
!  INTEGER vloop
  type (vector) TVect
  type (vector) CoordinateTransVector
  INTEGER ThisShape
  LOGICAL BaseSurface   ! True if a base surface or a detached shading surface
  REAL(r64) ThisSurfAz
  REAL(r64) ThisSurfTilt
  REAL(r64) ThisReveal
  REAL(r64) ThisWidth
  REAL(r64) ThisHeight
  INTEGER FrDivNum ! Frame/divider number
  REAL(r64) FrWidth    ! Frame width for exterior windows (m)
  REAL(r64) FrArea     ! Frame area for exterior windows(m2)
  REAL(r64) DivWidth   ! Divider width for exterior windows (m)
  REAL(r64) DivArea    ! Divider area for exterior windows (m2)
  REAL(r64) DivFrac    ! Fraction of divider area without overlaps
  LOGICAL ErrorInSurface ! false/true, depending on pass through routine
  REAL(r64) Diagonal1  ! Length of diagonal of 4-sided figure (m)
  REAL(r64) Diagonal2  ! Length of diagonal of 4-sided figure (m)
  LOGICAL SError
  LOGICAL HeatTransSurf
  LOGICAL IsCoplanar
  REAL(r64) OutOfLine
  INTEGER LastVertexInError

  ErrorInSurface=.false.

  IF (OneTimeFlag) THEN
    ALLOCATE(X(MaxVerticesPerSurface))
    ALLOCATE(Y(MaxVerticesPerSurface))
    ALLOCATE(Z(MaxVerticesPerSurface))
    X=0.0d0
    Y=0.0d0
    Z=0.0d0
    OneTimeFlag=.false.
  ENDIF

! Categorize this surface

   IF (Surface(ThisSurf)%BaseSurf == 0 .or. Surface(ThisSurf)%BaseSurf == ThisSurf) THEN
     BaseSurface=.true.
   ELSE
     BaseSurface=.false.
   ENDIF

   ThisBaseSurface=Surface(ThisSurf)%BaseSurf  ! Dont know if this is still needed or not
   HeatTransSurf = Surface(ThisSurf)%HeatTransSurf

   ! Kludge for daylighting shelves
   IF (Surface(ThisSurf)%ShadowingSurf) THEN
     ThisBaseSurface = ThisSurf
     HeatTransSurf = .TRUE.
   END IF

   !IF (Surface(ThisSurf)%Name(1:3) /= 'Mir') THEN
   IF (.NOT. Surface(ThisSurf)%MirroredSurf) THEN
     CALL CalcCoPlanarNess(Surface(ThisSurf)%Vertex,Surface(ThisSurf)%Sides,IsCoplanar,OutOfLine,LastVertexInError)
     IF (.not. IsCoPlanar) THEN
       IF (OutOfLine > .01d0) THEN
         CALL ShowSevereError(RoutineName//'Suspected non-planar surface:"'//TRIM(Surface(ThisSurf)%Name)//'",'// &
                              ' Max "out of line"='//TRIM(TrimSigDigits(OutOfLine,5))//' at Vertex # '//  &
                              TRIM(TrimSigDigits(LastVertexInError)))
       ELSE
         CALL ShowWarningError(RoutineName//'Possible non-planar surface:"'//TRIM(Surface(ThisSurf)%Name)//'",'// &
                              ' Max "out of line"='//TRIM(TrimSigDigits(OutOfLine,5))//' at Vertex # '//  &
                              TRIM(TrimSigDigits(LastVertexInError)))
       ENDIF
!       ErrorInSurface=.true.
     ENDIF
   ENDIF

   IF (BaseSurface) THEN
     SurfWorldAz=Surface(ThisSurf)%Azimuth
     SurfTilt=Surface(ThisSurf)%Tilt
     BaseCosAzimuth=COS(SurfWorldAz*DegToRadians)
     BaseSinAzimuth=SIN(SurfWorldAz*DegToRadians)
     BaseCosTilt=COS(SurfTilt*DegToRadians)
     BaseSinTilt=SIN(SurfTilt*DegToRadians)
     DO N=1,Surface(ThisSurf)%Sides
       X(N)=Surface(ThisSurf)%Vertex(N)%X
       Y(N)=Surface(ThisSurf)%Vertex(N)%Y
       Z(N)=Surface(ThisSurf)%Vertex(N)%Z
     ENDDO
     BaseXLLC=Surface(ThisSurf)%Vertex(2)%X
     BaseYLLC=Surface(ThisSurf)%Vertex(2)%Y
     BaseZLLC=Surface(ThisSurf)%Vertex(2)%Z
     TVect=Surface(ThisSurf)%Vertex(3)-Surface(ThisSurf)%Vertex(2)
     ThisWidth=VecLength(TVect)
     TVect=Surface(ThisSurf)%Vertex(2)-Surface(ThisSurf)%Vertex(1)
     ThisHeight=VecLength(TVect)
     Surface(ThisSurf)%Width=ThisWidth
     Surface(ThisSurf)%Height=ThisHeight ! For a horizontal surface this is actually length!
     IF(Surface(ThisSurf)%Sides==3) Surface(ThisSurf)%Shape = Triangle
     IF(Surface(ThisSurf)%Sides==4) THEN
       Diagonal1 = VecLength(Surface(ThisSurf)%Vertex(1)-Surface(ThisSurf)%Vertex(3))
       Diagonal2 = VecLength(Surface(ThisSurf)%Vertex(2)-Surface(ThisSurf)%Vertex(4))
       ! Test for rectangularity
       IF(ABS(Diagonal1-Diagonal2) < 0.020d0) THEN
         Surface(ThisSurf)%Shape = Rectangle
       ELSE
         Surface(ThisSurf)%Shape = Quadrilateral
       END IF
     END IF
     IF (Surface(ThisSurf)%Sides >4) THEN
       Surface(ThisSurf)%Shape = Polygonal
       if (abs(thisheight*thiswidth-surface(thissurf)%grossarea) > .001d0) then
         surface(thissurf)%width=sqrt(surface(thissurf)%grossarea)
         surface(thissurf)%height=surface(thissurf)%width
         thiswidth=surface(thissurf)%width
         thisheight=surface(thissurf)%height
       endif
     ENDIF

   ELSE   ! It's a subsurface to previous basesurface in this set of calls

     ThisSurfAz=Surface(ThisSurf)%Azimuth
     ThisSurfTilt=Surface(ThisSurf)%Tilt

     IF (HeatTransSurf) then

       IF(Surface(ThisSurf)%Sides == 4) THEN
          ThisShape=RectangularDoorWindow
       ELSE IF(Surface(ThisSurf)%Sides == 3 .AND. Surface(ThisSurf)%Class == SurfaceClass_Window) THEN
          ThisShape=TriangularWindow
       ELSE IF(Surface(ThisSurf)%Sides == 3 .AND. Surface(ThisSurf)%Class == SurfaceClass_Door) THEN
          ThisShape=TriangularDoor
       END IF

     ELSE    !  this is a shadowing subsurface

       IF (ABS(Surface(Surface(ThisSurf)%Basesurf)%Tilt-ThisSurfTilt) <= .01d0) then
         ! left or right fin
         IF (ThisSurfAz < 0.0d0) ThisSurfAz=ThisSurfAz+360.d0
         IF (ThisSurfAz > Surface(Surface(ThisSurf)%BaseSurf)%Azimuth) then
           ThisShape=RectangularLeftFin
         ELSE
           ThisShape=RectangularRightFin
         ENDIF
       ELSE
         ThisShape=RectangularOverhang
       ENDIF

     ENDIF

     ! Setting relative coordinates for shadowing calculations for subsurfaces
     SELECT CASE (ThisShape)

     CASE (RectangularDoorWindow) ! Rectangular heat transfer subsurface

       CALL PlaneEquation(Surface(Surface(ThisSurf)%BaseSurf)%Vertex,Surface(Surface(ThisSurf)%BaseSurf)%Sides,BasePlane,SError)
       IF (SError) THEN
         CALL ShowSevereError(RoutineName//'Degenerate surface (likely two vertices equal):"'//TRIM(Surface(ThisSurf)%Name)//'".')
         ErrorInSurface=.true.
       ENDIF
       ThisReveal=-Pt2Plane(Surface(ThisSurf)%Vertex(2),BasePlane)
       IF (ABS(ThisReveal) < .0002d0) ThisReveal=0.0d0
       Surface(ThisSurf)%Reveal=ThisReveal
       Xp=Surface(ThisSurf)%Vertex(2)%x-BaseXLLC
       Yp=Surface(ThisSurf)%Vertex(2)%y-BaseYLLC
       Zp=Surface(ThisSurf)%Vertex(2)%z-BaseZLLC
       XLLC=-Xp*BaseCosAzimuth+Yp*BaseSinAzimuth
       YLLC=-Xp*BaseSinAzimuth*BaseCosTilt-Yp*BaseCosAzimuth*BaseCosTilt+Zp*BaseSinTilt
       ZLLC= Xp*BaseSinAzimuth*BaseSinTilt+Yp*BaseCosAzimuth*BaseSinTilt+Zp*BaseCosTilt
       TVect=Surface(ThisSurf)%Vertex(3)-Surface(ThisSurf)%Vertex(2)
       ThisWidth=VecLength(TVect)
       TVect=Surface(ThisSurf)%Vertex(2)-Surface(ThisSurf)%Vertex(1)
       ThisHeight=VecLength(TVect)
       Surface(ThisSurf)%Width=ThisWidth
       Surface(ThisSurf)%Height=ThisHeight
       Diagonal1 = VecLength(Surface(ThisSurf)%Vertex(1)-Surface(ThisSurf)%Vertex(3))
       Diagonal2 = VecLength(Surface(ThisSurf)%Vertex(2)-Surface(ThisSurf)%Vertex(4))

       ! Test for rectangularity
       IF(ABS(Diagonal1-Diagonal2) > 0.020d0) THEN
         CALL ShowSevereError(RoutineName//'Suspected 4-sided but non-rectangular Window, Door or GlassDoor:')
         CALL ShowContinueError('Surface='//TRIM(Surface(ThisSurf)%Name) &
                     //', Diagonal1='//TRIM(TrimSigDigits(Diagonal1,3))//', Diagonal2='//TRIM(TrimSigDigits(Diagonal2,3)))
         ErrorInSurface=.true.
       ENDIF

       X(1) = XLLC
       X(2) = XLLC
       X(3) = XLLC + Surface(ThisSurf)%Width
       X(4) = XLLC + Surface(ThisSurf)%Width
       Y(1) = YLLC + Surface(ThisSurf)%Height
       Y(4) = YLLC + Surface(ThisSurf)%Height
       Y(2) = YLLC
       Y(3) = YLLC
       Z(1) = ZLLC
       Z(2) = ZLLC
       Z(3) = ZLLC
       Z(4) = ZLLC

       IF(Surface(ThisSurf)%Class == SurfaceClass_Window .AND. &
          Surface(ThisSurf)%ExtBoundCond == ExternalEnvironment .AND. &
          Surface(ThisSurf)%FrameDivider > 0) THEN
         FrDivNum = Surface(ThisSurf)%FrameDivider
         ! Set flag for calculating beam solar reflection from outside and/or inside window reveal
         IF((Surface(ThisSurf)%Reveal > 0.0d0 .AND. FrameDivider(FrDivNum)%OutsideRevealSolAbs > 0.0d0) .OR. &
            (FrameDivider(FrDivNum)%InsideSillDepth > 0.0d0 .AND. FrameDivider(FrDivNum)%InsideSillSolAbs > 0.0d0) .OR. &
            (FrameDivider(FrDivNum)%InsideReveal > 0.0d0 .AND. FrameDivider(FrDivNum)%InsideRevealSolAbs > 0.0d0)) &
           CalcWindowRevealReflection = .TRUE.

         ! For exterior window with frame, subtract frame area from base surface
         ! (only rectangular windows are allowed to have a frame and/or divider;
         ! Surface(ThisSurf)%FrameDivider will be 0 for triangular windows)
         FrWidth = FrameDivider(FrDivNum)%FrameWidth
         IF(FrWidth > 0.0d0) THEN
           FrArea = (Surface(ThisSurf)%Height + 2.0d0*FrWidth)*(Surface(ThisSurf)%Width + 2.0d0*FrWidth) &
                        - Surface(ThisSurf)%Area/Surface(ThisSurf)%Multiplier
           SurfaceWindow(ThisSurf)%FrameArea = FrArea * Surface(ThisSurf)%Multiplier
           IF((Surface(Surface(ThisSurf)%BaseSurf)%Area - SurfaceWindow(ThisSurf)%FrameArea) <= 0.0d0) THEN
             CALL ShowSevereError(RoutineName//'Base Surface="'//TRIM(Surface(Surface(ThisSurf)%BaseSurf)%Name)//'", ')
             CALL ShowContinueError('Window Surface="'//trim(Surface(ThisSurf)%Name)//  &
                '" area (with frame) is too large to fit on the surface.')
             CALL ShowContinueError('Base surface area (-windows and doors)=['//  &
                trim(TrimSigDigits(Surface(Surface(ThisSurf)%BaseSurf)%Area,2))//  &
                '] m2, frame area=['//trim(TrimSigDigits(SurfaceWindow(ThisSurf)%FrameArea,2))//'] m2.')
             ErrorInSurface=.true.
           ENDIF
           Surface(Surface(ThisSurf)%BaseSurf)%Area =  &
               Surface(Surface(ThisSurf)%BaseSurf)%Area - SurfaceWindow(ThisSurf)%FrameArea
         END IF
       ! If exterior window has divider, subtract divider area to get glazed area
         DivWidth = FrameDivider(Surface(ThisSurf)%FrameDivider)%DividerWidth
         IF(DivWidth > 0.0d0 .and. .not. ErrorInSurface) THEN
           DivArea = DivWidth * &
             (FrameDivider(FrDivNum)%HorDividers * Surface(ThisSurf)%Width + &
              FrameDivider(FrDivNum)%VertDividers * Surface(ThisSurf)%Height - &
              FrameDivider(FrDivNum)%HorDividers * FrameDivider(FrDivNum)%VertDividers * DivWidth)
           SurfaceWindow(ThisSurf)%DividerArea = DivArea * Surface(ThisSurf)%Multiplier
           IF((Surface(ThisSurf)%Area - SurfaceWindow(ThisSurf)%DividerArea) <= 0.0d0) THEN
             CALL ShowSevereError(RoutineName//'Divider area exceeds glazed opening for window '&
                                 //TRIM(Surface(ThisSurf)%Name))
             CALL ShowContinueError('Window surface area=['//  &
                trim(TrimSigDigits(Surface(ThisSurf)%Area,2))//  &
                '] m2, divider area=['//trim(TrimSigDigits(SurfaceWindow(ThisSurf)%DividerArea,2))//'] m2.')
             ErrorInSurface=.true.
           ENDIF
           Surface(ThisSurf)%Area = Surface(ThisSurf)%Area - SurfaceWindow(ThisSurf)%DividerArea  ! Glazed area
           IF (DivArea <= 0.0d0) THEN
             CALL ShowWarningError(RoutineName//'Calculated Divider Area <= 0.0 for Window='//  &
                TRIM(Surface(ThisSurf)%Name))
             IF (FrameDivider(FrDivNum)%HorDividers == 0) THEN
               CALL ShowContinueError('..Number of Horizontal Dividers = 0.')
             ENDIF
             IF (FrameDivider(FrDivNum)%VertDividers == 0) THEN
               CALL ShowContinueError('..Number of Vertical Dividers = 0.')
             ENDIF
           ELSE
             SurfaceWindow(ThisSurf)%GlazedFrac = Surface(ThisSurf)%Area/(Surface(ThisSurf)%Area +  &
                                 SurfaceWindow(ThisSurf)%DividerArea)
             ! Correction factor for portion of divider subject to divider projection correction
             DivFrac = (1.0d0-FrameDivider(FrDivNum)%HorDividers * &
               FrameDivider(FrDivNum)%VertDividers * DivWidth**2 / DivArea)
             SurfaceWindow(ThisSurf)%ProjCorrDivOut = DivFrac * &
               FrameDivider(FrDivNum)%DividerProjectionOut/DivWidth
             SurfaceWindow(ThisSurf)%ProjCorrDivIn = DivFrac * &
               FrameDivider(FrDivNum)%DividerProjectionIn/DivWidth
             ! Correction factor for portion of frame subject to frame projection correction
             IF(FrWidth > 0.0d0) THEN
               SurfaceWindow(ThisSurf)%ProjCorrFrOut = (FrameDivider(FrDivNum)%FrameProjectionOut/FrWidth)* &
                   (ThisHeight+ThisWidth-(FrameDivider(FrDivNum)%HorDividers + FrameDivider(FrDivNum)%VertDividers)* &
                  DivWidth)/(ThisHeight+ThisWidth+2*FrWidth)
               SurfaceWindow(ThisSurf)%ProjCorrFrIn = (FrameDivider(FrDivNum)%FrameProjectionIn/FrWidth)* &
                   (ThisHeight+ThisWidth-(FrameDivider(FrDivNum)%HorDividers + FrameDivider(FrDivNum)%VertDividers)* &
                   DivWidth)/(ThisHeight+ThisWidth+2*FrWidth)

             END IF
           END IF
         END IF
       END IF

     CASE (TriangularWindow,TriangularDoor)

       CALL PlaneEquation(Surface(Surface(ThisSurf)%BaseSurf)%Vertex,Surface(Surface(ThisSurf)%BaseSurf)%Sides,BasePlane,SError)
       IF (SError) THEN
         CALL ShowSevereError(RoutineName//'Degenerate surface (likely two vertices equal):"'//TRIM(Surface(ThisSurf)%Name)//'".')
         ErrorInSurface=.true.
       ENDIF
       ThisReveal=-Pt2Plane(Surface(ThisSurf)%Vertex(2),BasePlane)
       IF (ABS(ThisReveal) < .0002d0) ThisReveal=0.0d0
       Surface(ThisSurf)%Reveal=ThisReveal
       Xp=Surface(ThisSurf)%Vertex(2)%x-BaseXLLC
       Yp=Surface(ThisSurf)%Vertex(2)%y-BaseYLLC
       Zp=Surface(ThisSurf)%Vertex(2)%z-BaseZLLC
       X(2)=-Xp*BaseCosAzimuth+Yp*BaseSinAzimuth
       Y(2)=-Xp*BaseSinAzimuth*BaseCosTilt-Yp*BaseCosAzimuth*BaseCosTilt+Zp*BaseSinTilt
       Z(2)= Xp*BaseSinAzimuth*BaseSinTilt+Yp*BaseCosAzimuth*BaseSinTilt+Zp*BaseCosTilt
       TVect=Surface(ThisSurf)%Vertex(3)-Surface(ThisSurf)%Vertex(2)
       ThisWidth=VecLength(TVect)
       TVect=Surface(ThisSurf)%Vertex(2)-Surface(ThisSurf)%Vertex(1)
       ThisHeight=VecLength(TVect)
       Surface(ThisSurf)%Width=ThisWidth
       Surface(ThisSurf)%Height=ThisHeight
       ! Effective height and width of a triangular window for use in calc of convective air flow
       ! in gap between glass and shading device when shading device is present
       Surface(ThisSurf)%Height = 4.d0*Surface(ThisSurf)%Area/(3.d0*Surface(ThisSurf)%Width)
       Surface(ThisSurf)%Width  = 0.75d0*Surface(ThisSurf)%Width

       Xp=Surface(ThisSurf)%Vertex(1)%x-BaseXLLC
       Yp=Surface(ThisSurf)%Vertex(1)%y-BaseYLLC
       Zp=Surface(ThisSurf)%Vertex(1)%z-BaseZLLC
       X(1)=-Xp*BaseCosAzimuth+Yp*BaseSinAzimuth
       Y(1)=-Xp*BaseSinAzimuth*BaseCosTilt-Yp*BaseCosAzimuth*BaseCosTilt+Zp*BaseSinTilt
       Z(1)= Xp*BaseSinAzimuth*BaseSinTilt+Yp*BaseCosAzimuth*BaseSinTilt+Zp*BaseCosTilt

       Xp=Surface(ThisSurf)%Vertex(3)%x-BaseXLLC
       Yp=Surface(ThisSurf)%Vertex(3)%y-BaseYLLC
       Zp=Surface(ThisSurf)%Vertex(3)%z-BaseZLLC
       X(3)=-Xp*BaseCosAzimuth+Yp*BaseSinAzimuth
       Y(3)=-Xp*BaseSinAzimuth*BaseCosTilt-Yp*BaseCosAzimuth*BaseCosTilt+Zp*BaseSinTilt
       Z(3)= Xp*BaseSinAzimuth*BaseSinTilt+Yp*BaseCosAzimuth*BaseSinTilt+Zp*BaseCosTilt

     CASE (RectangularOverhang)

       Xp=Surface(ThisSurf)%Vertex(2)%x-BaseXLLC
       Yp=Surface(ThisSurf)%Vertex(2)%y-BaseYLLC
       Zp=Surface(ThisSurf)%Vertex(2)%z-BaseZLLC
       XLLC=-Xp*BaseCosAzimuth+Yp*BaseSinAzimuth
       YLLC=-Xp*BaseSinAzimuth*BaseCosTilt-Yp*BaseCosAzimuth*BaseCosTilt+Zp*BaseSinTilt
       ZLLC= Xp*BaseSinAzimuth*BaseSinTilt+Yp*BaseCosAzimuth*BaseSinTilt+Zp*BaseCosTilt
       TVect=Surface(ThisSurf)%Vertex(3)-Surface(ThisSurf)%Vertex(2)
       ThisWidth=VecLength(TVect)
       TVect=Surface(ThisSurf)%Vertex(2)-Surface(ThisSurf)%Vertex(1)
       ThisHeight=VecLength(TVect)
       Surface(ThisSurf)%Width=ThisWidth
       Surface(ThisSurf)%Height=ThisHeight
       X(1) = XLLC
       X(2) = XLLC
       X(3) = XLLC + Surface(ThisSurf)%Width
       X(4) = XLLC + Surface(ThisSurf)%Width
       Y(1) = YLLC
       Y(2) = YLLC
       Y(3) = YLLC
       Y(4) = YLLC
       Z(1) = Surface(ThisSurf)%Height
       Z(4) = Surface(ThisSurf)%Height
       Z(2) = 0.0d0
       Z(3) = 0.0d0

     CASE (RectangularLeftFin)

       Xp=Surface(ThisSurf)%Vertex(2)%x-BaseXLLC
       Yp=Surface(ThisSurf)%Vertex(2)%y-BaseYLLC
       Zp=Surface(ThisSurf)%Vertex(2)%z-BaseZLLC
       XLLC=-Xp*BaseCosAzimuth+Yp*BaseSinAzimuth
       YLLC=-Xp*BaseSinAzimuth*BaseCosTilt-Yp*BaseCosAzimuth*BaseCosTilt+Zp*BaseSinTilt
       ZLLC= Xp*BaseSinAzimuth*BaseSinTilt+Yp*BaseCosAzimuth*BaseSinTilt+Zp*BaseCosTilt
       TVect=Surface(ThisSurf)%Vertex(3)-Surface(ThisSurf)%Vertex(2)
       ThisWidth=VecLength(TVect)
       TVect=Surface(ThisSurf)%Vertex(2)-Surface(ThisSurf)%Vertex(1)
       ThisHeight=VecLength(TVect)
       Surface(ThisSurf)%Width=ThisWidth
       Surface(ThisSurf)%Height=ThisHeight
       X(1) = XLLC
       X(2) = XLLC
       X(3) = XLLC
       X(4) = XLLC
       Y(1) = YLLC
       Y(2) = YLLC
       Y(3) = YLLC + Surface(ThisSurf)%Width
       Y(4) = YLLC + Surface(ThisSurf)%Width
       Z(1) = Surface(ThisSurf)%Height
       Z(4) = Surface(ThisSurf)%Height
       Z(2) = 0.0d0
       Z(3) = 0.0d0

     CASE (RectangularRightFin)

       Xp=Surface(ThisSurf)%Vertex(2)%x-BaseXLLC
       Yp=Surface(ThisSurf)%Vertex(2)%y-BaseYLLC
       Zp=Surface(ThisSurf)%Vertex(2)%z-BaseZLLC
       XLLC=-Xp*BaseCosAzimuth+Yp*BaseSinAzimuth
       YLLC=-Xp*BaseSinAzimuth*BaseCosTilt-Yp*BaseCosAzimuth*BaseCosTilt+Zp*BaseSinTilt
       ZLLC= Xp*BaseSinAzimuth*BaseSinTilt+Yp*BaseCosAzimuth*BaseSinTilt+Zp*BaseCosTilt
       TVect=Surface(ThisSurf)%Vertex(3)-Surface(ThisSurf)%Vertex(2)
       ThisWidth=VecLength(TVect)
       TVect=Surface(ThisSurf)%Vertex(2)-Surface(ThisSurf)%Vertex(1)
       ThisHeight=VecLength(TVect)
       Surface(ThisSurf)%Width=ThisWidth
       Surface(ThisSurf)%Height=ThisHeight
       X(1) = XLLC
       X(2) = XLLC
       X(3) = XLLC
       X(4) = XLLC
       Y(1) = YLLC + Surface(ThisSurf)%Width
       Y(2) = YLLC + Surface(ThisSurf)%Width
       Y(3) = YLLC
       Y(4) = YLLC
       Z(1) = Surface(ThisSurf)%Height
       Z(4) = Surface(ThisSurf)%Height
       Z(2) = 0.0d0
       Z(3) = 0.0d0

     CASE DEFAULT
       ! Error Condition
       CALL ShowSevereError(RoutineName//'Incorrect surface shape number.',OutputFileStandard)
       CALL ShowContinueError('Please notify EnergyPlus support of this error and send input file.')
       ErrorInSurface=.true.

     END SELECT

     DO N = 1, Surface(ThisSurf)%Sides
      ! if less than 1/10 inch
       X(N)=ANINT(10000.d0*X(N),r64)/10000.d0
       IF (ABS(X(N)) < .0025d0) X(N)=0.0d0
       Y(N)=ANINT(10000.d0*Y(N),r64)/10000.d0
       IF (ABS(Y(N)) < .0025d0) Y(N)=0.0d0
       Z(N)=ANINT(10000.d0*Z(N),r64)/10000.d0
       IF (ABS(Z(N)) < .0025d0) Z(N)=0.0d0
     ENDDO

     Surface(ThisSurf)%Shape=ThisShape

   ENDIF  ! End of check if ThisSurf is a base surface

   IF (ErrorInSurface) THEN
     ErrorsFound=.true.
     RETURN
   ENDIF

    ! Transfer to XV,YV,ZV arrays

   ShadeV(ThisSurf)%NVert=Surface(ThisSurf)%Sides
   ALLOCATE(ShadeV(ThisSurf)%XV(Surface(ThisSurf)%Sides))
   ALLOCATE(ShadeV(ThisSurf)%YV(Surface(ThisSurf)%Sides))
   ALLOCATE(ShadeV(ThisSurf)%ZV(Surface(ThisSurf)%Sides))

   DO N = 1, Surface(ThisSurf)%Sides
    ! if less than 1/10 inch
     ShadeV(ThisSurf)%XV(N) = X(N)
     ShadeV(ThisSurf)%YV(N) = Y(N)
     ShadeV(ThisSurf)%ZV(N) = Z(N)
   END DO

          ! Process Surfaces According to Type of Coordinate Origin.
   IF (BaseSurface) THEN

          ! General Surfaces:
     CALL CalcCoordinateTransformation(ThisSurf,CoordinateTransVector) !X00,Y00,Z00,X,Y,Z,A)    ! Compute Coordinate Transformation

          ! RECORD DIRECTION COSINES.
     IF (HeatTransSurf) THEN ! This is a general surface but not detached shading surface

          ! RECORD COORDINATE TRANSFORMATION FOR BASE SURFACES.
       X0(ThisBaseSurface) = CoordinateTransVector%x
       Y0(ThisBaseSurface) = CoordinateTransVector%y
       Z0(ThisBaseSurface) = CoordinateTransVector%z

          ! COMPUTE INVERSE TRANSFORMATION.
       X1     = X(2) - CoordinateTransVector%x
       Y1     = Y(2) - CoordinateTransVector%y
       Z1     = Z(2) - CoordinateTransVector%z
       XSHIFT = Surface(ThisBaseSurface)%lcsx%x*X1 + Surface(ThisBaseSurface)%lcsx%y*Y1 + Surface(ThisBaseSurface)%lcsx%z*Z1
       YSHIFT = Surface(ThisBaseSurface)%lcsy%x*X1 + Surface(ThisBaseSurface)%lcsy%y*Y1 + Surface(ThisBaseSurface)%lcsy%z*Z1

     ENDIF

          ! SUBSURFACES: (Surface(ThisSurf)%BaseSurf /= ThisSurf)
   ELSE
          ! WINDOWS OR DOORS:

          ! SHIFT RELATIVE COORDINATES FROM LOWER LEFT CORNER TO ORIGIN DEFINED
          ! BY CTRAN AND SET DIRECTION COSINES SAME AS BASE SURFACE.

     DO N = 1, Surface(ThisSurf)%Sides
       ShadeV(ThisSurf)%XV(N) = ShadeV(ThisSurf)%XV(N) + XSHIFT
       ShadeV(ThisSurf)%YV(N) = ShadeV(ThisSurf)%YV(N) + YSHIFT
     ENDDO

   ENDIF

   IF (ErrorInSurface) THEN
     ErrorsFound=.true.
   ENDIF

  RETURN

END SUBROUTINE ProcessSurfaceVertices

SUBROUTINE CalcCoordinateTransformation(SurfNum,CompCoordTranslVector)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         George Walton, BLAST
          !       DATE WRITTEN   August 1976
          !       MODIFIED       LKL, May 2004 -- >4 sided polygons
          !       RE-ENGINEERED  Yes

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine develops a coordinate transformation such that the X-axis goes
          ! through points 2 and 3 and the Y-axis goes through point 1
          ! of a plane figure in 3-d space.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! 'NECAP' - NASA'S Energy-Cost Analysis Program

          ! USE STATEMENTS:
  USE Vectors

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)           :: SurfNum                 ! Surface Number
  TYPE(vector) :: CompCoordTranslVector  ! Coordinate Translation Vector

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER :: ErrFmt="(' (',F8.3,',',F8.3,',',F8.3,')')"

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER I               ! Loop Control
  REAL(r64) GAMMA              ! Intermediate Result
  REAL(r64) DotSelfX23
  CHARACTER(len=132) :: ErrLineOut=Blank ! Character string for producing error messages
  TYPE (vector) :: x21
  TYPE (vector) :: x23

          ! Determine Components of the Coordinate Translation Vector.

  x21=Surface(SurfNum)%Vertex(2)-Surface(SurfNum)%Vertex(1)
  x23=Surface(SurfNum)%Vertex(2)-Surface(SurfNum)%Vertex(3)

  DotSelfX23=(x23 .dot. x23)

  IF (ABS(DotSelfX23) <= .1d-6) THEN
    CALL ShowSevereError('CalcCoordinateTransformation: Invalid dot product, surface="'//  &
                         TRIM(Surface(SurfNum)%Name)//'":')
    DO I=1,Surface(SurfNum)%Sides
      WRITE(ErrLineOut,ErrFmt) Surface(SurfNum)%Vertex(I)
      CALL ShowContinueError(ErrLineOut)
    ENDDO
    CALL ShowFatalError('CalcCoordinateTransformation: Program terminates due to preceding condition.',OutputFileStandard)
    RETURN
  END IF

  Gamma = (x21 .dot. x23) / (x23 .dot. x23)

  CompCoordTranslVector = Surface(SurfNum)%Vertex(2) + Gamma * (Surface(SurfNum)%Vertex(3)-Surface(SurfNum)%Vertex(2))

  RETURN

END SUBROUTINE CalcCoordinateTransformation

SUBROUTINE CreateShadedWindowConstruction(SurfNum,WSCptr,ShDevNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   Nov 2001
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Creates a shaded window construction for windows whose WindowShadingControl
          ! has a shading device specified instead of a shaded construction

          ! METHODOLOGY EMPLOYED:na

          ! REFERENCES:na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)       :: SurfNum           ! Surface number
  INTEGER, INTENT(IN)       :: WSCptr            ! Pointer to WindowShadingControl for SurfNum
  INTEGER, INTENT(IN)       :: ShDevNum          ! Shading device material number for WSCptr

          ! SUBROUTINE PARAMETER DEFINITIONS:na
          ! INTERFACE BLOCK SPECIFICATIONS;na
          ! DERIVED TYPE DEFINITIONS:na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: ConstrNum                       ! Number of unshaded construction
  INTEGER :: ConstrNewSh                     ! Number of shaded construction that is created
  CHARACTER(len=MaxNameLength) :: ShDevName  ! Shading device material name
  CHARACTER(len=MaxNameLength) :: ConstrName ! Unshaded construction name
  CHARACTER(len=MaxNameLength) :: ConstrNameSh ! Shaded construction name
  INTEGER :: TotLayersOld                    ! Total layers in old (unshaded) construction
  INTEGER :: TotLayersNew                    ! Total layers in new (shaded) construction
!  INTEGER :: loop                            ! DO loop index

ShDevName = Material(ShDevNum)%Name
ConstrNum = SurfaceTmp(SurfNum)%Construction
ConstrName= Construct(ConstrNum)%Name
IF(WindowShadingControl(WSCptr)%ShadingType==WSC_ST_InteriorShade.OR. &
   WindowShadingControl(WSCptr)%ShadingType==WSC_ST_InteriorBlind) THEN
  ConstrNameSh = TRIM(ConstrName)//':'//TRIM(ShDevName)//':'//'INT'
ELSE
  ConstrNameSh = TRIM(ConstrName)//':'//TRIM(ShDevName)//':'//'EXT'
END IF

! If this construction name already exists, set the surface's shaded construction number to it

ConstrNewSh = FindIteminList(ConstrNameSh,Construct%Name,TotConstructs)

IF(ConstrNewSh > 0) THEN
  SurfaceTmp(SurfNum)%ShadedConstruction = ConstrNewSh
ELSE

  ! Create new construction

  ConstrNewSh = TotConstructs + 1
  SurfaceTmp(SurfNum)%ShadedConstruction = ConstrNewSh
  ALLOCATE(ConstructSave(TotConstructs))
  ALLOCATE(NominalRSave(TotConstructs))
  ALLOCATE(NominalUSave(TotConstructs))
  ConstructSave(1:TotConstructs) = Construct(1:TotConstructs)
  NominalRSave(1:TotConstructs) = NominalRforNominalUCalculation(1:TotConstructs)
  NominalUSave(1:TotConstructs) = NominalU(1:TotConstructs)
  DEALLOCATE(Construct)
  DEALLOCATE(NominalRforNominalUCalculation)
  DEALLOCATE(NominalU)
  TotConstructs = ConstrNewSh
  ALLOCATE(Construct(TotConstructs))
  ALLOCATE(NominalRforNominalUCalculation(TotConstructs))
  ALLOCATE(NominalU(TotConstructs))
  NominalRforNominalUCalculation=0.0d0
  NominalU=0.0d0
  Construct(1:TotConstructs-1) = ConstructSave(1:TotConstructs-1)
  NominalRforNominalUCalculation(1:TotConstructs-1) = NominalRSave(1:TotConstructs-1)
  NominalU(1:TotConstructs-1) = NominalUSave(1:TotConstructs-1)
  DEALLOCATE(ConstructSave)
  DEALLOCATE(NominalRSave)
  DEALLOCATE(NominalUSave)

  TotLayersOld = Construct(ConstrNum)%TotLayers
  TotLayersNew = TotLayersOld + 1

  Construct(ConstrNewSh)%LayerPoint = 0

  IF(WindowShadingControl(WSCptr)%ShadingType==WSC_ST_InteriorShade.OR. &
     WindowShadingControl(WSCptr)%ShadingType==WSC_ST_InteriorBlind) THEN
    ! Interior shading device
    Construct(ConstrNewSh)%LayerPoint(1:TotLayersOld) = Construct(ConstrNum)%LayerPoint(1:TotLayersOld)
    Construct(ConstrNewSh)%LayerPoint(TotLayersNew)   = ShDevNum
    Construct(ConstrNewSh)%InsideAbsorpSolar    = Material(ShDevNum)%AbsorpSolar
    Construct(ConstrNewSh)%OutsideAbsorpSolar   = Material(Construct(ConstrNewSh)%LayerPoint(1))%AbsorpSolar
    Construct(ConstrNewSh)%OutsideAbsorpThermal = Material(Construct(ConstrNewSh)%LayerPoint(1))%AbsorpThermalFront
  ELSE
    ! Exterior shading device
    Construct(ConstrNewSh)%LayerPoint(1) = ShDevNum
    Construct(ConstrNewSh)%LayerPoint(2:TotLayersNew) = Construct(ConstrNum)%LayerPoint(1:TotLayersOld)
    Construct(ConstrNewSh)%InsideAbsorpSolar    = &
      Material(Construct(ConstrNewSh)%LayerPoint(TotLayersNew))%AbsorpSolar
    Construct(ConstrNewSh)%OutsideAbsorpSolar   = Material(ShDevNum)%AbsorpSolar
    Construct(ConstrNewSh)%OutsideAbsorpThermal = Material(ShDevNum)%AbsorpThermalFront
  END IF
  ! The following InsideAbsorpThermal applies only to inside glass; it is corrected
  !  later in InitGlassOpticalCalculations if construction has inside shade or blind.
  Construct(ConstrNewSh)%InsideAbsorpThermal = &
        Material(Construct(ConstrNum)%LayerPoint(TotLayersOld))%AbsorpThermalBack
  Construct(ConstrNewSh)%OutSideRoughness     = VerySmooth
  Construct(ConstrNewSh)%DayltPropPtr        = 0
  Construct(ConstrNewSh)%CTFCross            = 0.0D0
  Construct(ConstrNewSh)%CTFFlux             = 0.0D0
  Construct(ConstrNewSh)%CTFInside           = 0.0D0
  Construct(ConstrNewSh)%CTFOutside          = 0.0D0
  Construct(ConstrNewSh)%CTFSourceIn         = 0.0D0
  Construct(ConstrNewSh)%CTFSourceOut        = 0.0D0
  Construct(ConstrNewSh)%CTFTimeStep         = 0.0D0
  Construct(ConstrNewSh)%CTFTSourceOut       = 0.0D0
  Construct(ConstrNewSh)%CTFTSourceIn        = 0.0D0
  Construct(ConstrNewSh)%CTFTSourceQ         = 0.0D0
  Construct(ConstrNewSh)%CTFTUserOut         = 0.0D0
  Construct(ConstrNewSh)%CTFTUserIn          = 0.0D0
  Construct(ConstrNewSh)%CTFTUserSource      = 0.0D0
  Construct(ConstrNewSh)%NumHistories        = 0
  Construct(ConstrNewSh)%NumCTFTerms         = 0
  Construct(ConstrNewSh)%UValue              = 0.0d0
  Construct(ConstrNewSh)%SourceSinkPresent   = .FALSE.
  Construct(ConstrNewSh)%SolutionDimensions  = 0
  Construct(ConstrNewSh)%SourceAfterLayer    = 0
  Construct(ConstrNewSh)%TempAfterLayer      = 0
  Construct(ConstrNewSh)%ThicknessPerpend    = 0.0d0
  Construct(ConstrNewSh)%AbsDiff             = 0.0d0
  Construct(ConstrNewSh)%AbsDiffBack         = 0.0d0
  Construct(ConstrNewSh)%AbsDiffShade        = 0.0d0
  Construct(ConstrNewSh)%AbsDiffBackShade    = 0.0d0
  Construct(ConstrNewSh)%ShadeAbsorpThermal  = 0.0d0
  Construct(ConstrNewSh)%AbsBeamCoef         = 0.0d0
  Construct(ConstrNewSh)%AbsBeamBackCoef     = 0.0d0
  Construct(ConstrNewSh)%AbsBeamShadeCoef    = 0.0d0
  Construct(ConstrNewSh)%TransDiff           = 0.0d0
  Construct(ConstrNewSh)%TransDiffVis        = 0.0d0
  Construct(ConstrNewSh)%ReflectSolDiffBack  = 0.0d0
  Construct(ConstrNewSh)%ReflectSolDiffFront = 0.0d0
  Construct(ConstrNewSh)%ReflectVisDiffBack  = 0.0d0
  Construct(ConstrNewSh)%ReflectVisDiffFront = 0.0d0
  Construct(ConstrNewSh)%TransSolBeamCoef    = 0.0d0
  Construct(ConstrNewSh)%TransVisBeamCoef    = 0.0d0
  Construct(ConstrNewSh)%ReflSolBeamFrontCoef= 0.0d0
  Construct(ConstrNewSh)%ReflSolBeamBackCoef = 0.0d0
  Construct(ConstrNewSh)%W5FrameDivider      = 0
  Construct(ConstrNewSh)%FromWindow5DataFile = .false.

  Construct(ConstrNewSh)%Name = ConstrNameSh
  Construct(ConstrNewSh)%TotLayers = TotLayersNew
  Construct(ConstrNewSh)%TotSolidLayers = Construct(ConstrNum)%TotSolidLayers + 1
  Construct(ConstrNewSh)%TotGlassLayers = Construct(ConstrNum)%TotGlassLayers
  Construct(ConstrNewSh)%TypeIsWindow = .true.
  Construct(ConstrNewSh)%IsUsed       = .true.

END IF

RETURN
END SUBROUTINE CreateShadedWindowConstruction

SUBROUTINE CreateStormWindowConstructions

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   Jan 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! For windows with an associated StormWindow object, creates a construction
          ! consisting of the base construction plus a storm window and air gap on the outside.
          ! If the window has an interior or between-glass shade/blind, also creates a
          ! construction consisting of the storm window added to the shaded construction.

          ! METHODOLOGY EMPLOYED:na
          ! REFERENCES:na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! SUBROUTINE PARAMETER DEFINITIONS:na
          ! INTERFACE BLOCK SPECIFICATIONS;na
          ! DERIVED TYPE DEFINITIONS:na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: SurfNum               ! Surface number
  INTEGER :: StormWinNum           ! Number of StormWindow object
  INTEGER :: ConstrNum             ! Number of unshaded construction
  INTEGER :: ConstrNumSh           ! Number of shaded construction
  INTEGER :: ConstrOld             ! Number of old construction (unshaded or shaded)
  INTEGER :: ConstrNewSt           ! Number of unshaded storm window construction that is created
  INTEGER :: ConstrNewStSh         ! Number of shaded storm window construction that is created
  INTEGER :: ConstrNew             ! Number of new construction with storm window (unshaded or shaded)
  INTEGER :: MatNewStAir           ! Number of created air layer material
  CHARACTER(len=MaxNameLength) :: ConstrName   ! Name of original unshaded window construction
  CHARACTER(len=MaxNameLength) :: ConstrNameSh ! Name of original shaded window construction
  CHARACTER(len=MaxNameLength) :: ConstrNameSt ! Name of unshaded construction with storm window
  CHARACTER(len=MaxNameLength) :: ConstrNameStsh ! Name of shaded construction with storm window
  CHARACTER(len=MaxNameLength) :: MatNameStAir ! Name of created air layer material
  INTEGER :: StormWinMatNum        ! Material number of storm window glass layer
  INTEGER :: IntDistance           ! Thickness of air gap between storm window and rest of window (mm)
  CHARACTER(len=20) :: ChrIntDistance ! Character representation of IntDistance
  CHARACTER(len=29) :: ChrNum      ! Character representation of storm window number
  INTEGER :: TotLayers             ! Total layers in a construction
  INTEGER :: TotGlassLayers        ! Total glass layers in a construction
  INTEGER :: TotLayersOld          ! Total layers in old (without storm window) construction
  INTEGER :: MatIntSh              ! Material number of interior shade or blind
  INTEGER :: MatBGsh               ! Material number of between-glass shade or blind
  INTEGER :: loop                  ! DO loop index
  LOGICAL :: ShAndSt               ! True if unshaded and shaded window can have a storm window
!  INTEGER :: LenName               ! Name length

CALL DisplayString('Creating Storm Window Constructions')

DO StormWinNum = 1,TotStormWin
  SurfNum = StormWindow(StormWinNum)%BaseWindowNum
  ConstrNum    = Surface(SurfNum)%Construction
  ! Fatal error if base construction has more than three glass layers
  IF(Construct(ConstrNum)%TotGlassLayers > 3) THEN
    CALL ShowFatalError('Window='//TRIM(Surface(SurfNum)%Name) &
         //' has more than 3 glass layers; a storm window cannot be applied.')
  END IF
  ConstrNumSh  = Surface(SurfNum)%ShadedConstruction
  ConstrName   = Construct(ConstrNum)%Name
  StormWinMatNum = StormWindow(StormWinNum)%StormWinMaterialNum
  IntDistance = INT(1000*StormWindow(StormWinNum)%StormWinDistance)
  WRITE(ChrIntDistance,*) IntDistance
  ChrIntDistance = ADJUSTL(ChrIntDistance)
  ! Set ShAndSt, which is true if the window has a shaded construction to which a storm window
  ! can be added. (A storm window can be added if there is an interior shade or blind and up to three
  ! glass layers, or there is a between-glass shade or blind and two glass layers.)
  ShAndSt = .false.
  IF(ConstrNumSh > 0) THEN
    ConstrNameSh = Construct(ConstrNumSh)%Name
    TotLayers = Construct(ConstrNumSh)%TotLayers
    TotGlassLayers = Construct(ConstrNumSh)%TotGlassLayers
    MatIntSh = Construct(ConstrNumSh)%LayerPoint(TotLayers)
    MatBGsh  = 0
    IF(TotLayers == 5) MatBGsh = Construct(ConstrNumSh)%LayerPoint(3)
    IF(TotGlassLayers <= 3 .AND. (Material(MatIntSh)%Group == Shade .OR. &
              Material(MatIntSh)%Group == WindowBlind)) ShAndSt = .true.
    IF(MatBGsh > 0) THEN
      IF(Material(MatBGsh)%Group == Shade .OR. Material(MatBGsh)%Group == WindowBlind) ShAndSt = .true.
    END IF
    IF(.not.ShAndSt) THEN
      CALL ShowContinueError('Window='//TRIM(Surface(SurfNum)%Name) &
         //' has a shaded construction to which a storm window cannot be applied.')
      CALL ShowContinueError('Storm windows can only be applied to shaded constructions that:')
      CALL ShowContinueError('have an interior shade or blind and up to three glass layers, or')
      CALL ShowContinueError('have a between-glass shade or blind and two glass layers.')
      CALL ShowFatalError('EnergyPlus is exiting due to reason stated above.')
    END IF
  END IF

  ! Loop over unshaded (loop=1) and shaded (loop=2) constructions and create new constructions
  ! with storm window and air gap added on outside
  DO loop = 1,2
    IF(loop == 1) THEN
      WRITE(ChrNum,*) StormWinNum
      ChrNum = ADJUSTL(ChrNum)
      ConstrNameSt = 'BARECONSTRUCTIONWITHSTORMWIN:'//TRIM(ChrNum)
      ! If this construction name already exists, set the surface's storm window construction number to it
      ConstrNewSt = FindIteminList(ConstrNameSt,Construct%Name,TotConstructs)
      ConstrNewStSh=0
      IF(ConstrNewSt > 0) Surface(SurfNum)%StormWinConstruction = ConstrNewSt
    ELSE
      IF(.not.ShAndSt) EXIT
      ConstrNameStSh = 'SHADEDCONSTRUCTIONWITHSTORMWIN:'//TRIM(ChrNum)
      ConstrNewStSh = FindIteminList(ConstrNameStSh,Construct%Name,TotConstructs)
      IF(ConstrNewStSh > 0) Surface(SurfNum)%StormWinShadedConstruction = ConstrNewStSh
    END IF

    IF(loop==1 .AND. ConstrNewSt==0) THEN
      ! If necessary, create new material corresponding to the air layer between the storm winddow
      ! and the rest of the window
      MatNameStAir = 'AIR:STORMWIN:'//TRIM(ChrIntDistance)//'MM'
      MatNewStAir = FindItemInList(MatNameStAir,Material%Name,TotMaterials)
      IF(MatNewStAir == 0) THEN
        ! Create new material
        MatNewStAir = TotMaterials + 1
        ALLOCATE(MaterialSave(TotMaterials))
        ALLOCATE(NominalRSave(TotMaterials))
        MaterialSave(1:TotMaterials) = Material(1:TotMaterials)
        NominalRSave(1:TotMaterials) = NominalR(1:TotMaterials)
        DEALLOCATE(Material)
        DEALLOCATE(NominalR)
        TotMaterials = MatNewStAir
        ALLOCATE(Material(TotMaterials))
        ALLOCATE(NominalR(TotMaterials))
        Material(1:TotMaterials-1) = MaterialSave(1:TotMaterials-1)
        NominalR(1:TotMaterials-1) = NominalRSave(1:TotMaterials-1)
        DEALLOCATE(MaterialSave)
        DEALLOCATE(NominalRSave)
        Material(TotMaterials)%Name = MatNameStAir
        Material(TotMaterials)%Group = WindowGas
        Material(TotMaterials)%Roughness = 3
        Material(TotMaterials)%Conductivity = 0.0d0
        Material(TotMaterials)%Density = 0.0d0
        Material(TotMaterials)%IsoMoistCap = 0.0d0
        Material(TotMaterials)%Porosity = 0.0d0
        Material(TotMaterials)%Resistance = 0.0d0
        Material(TotMaterials)%SpecHeat = 0.0d0
        Material(TotMaterials)%ThermGradCoef = 0.0d0
        Material(TotMaterials)%Thickness = StormWindow(StormWinNum)%StormWinDistance
        Material(TotMaterials)%VaporDiffus = 0.0d0
        Material(TotMaterials)%GasType = 0
        Material(TotMaterials)%GasCon = 0.0d0
        Material(TotMaterials)%GasVis = 0.0d0
        Material(TotMaterials)%GasCp = 0.0d0
        Material(TotMaterials)%GasWght = 0.0d0
        Material(TotMaterials)%GasFract = 0.0d0
        Material(TotMaterials)%GasType(1) = 1
        Material(TotMaterials)%GlassSpectralDataPtr = 0
        Material(TotMaterials)%NumberOfGasesInMixture = 1
        Material(TotMaterials)%GasCon(1,1) = 2.873d-3
        Material(TotMaterials)%GasCon(1,2) = 7.760d-5
        Material(TotMaterials)%GasVis(1,1) = 3.723d-6
        Material(TotMaterials)%GasVis(1,2) = 4.940d-8
        Material(TotMaterials)%GasCp(1,1)  = 1002.737d0
        Material(TotMaterials)%GasCp(1,2)  = 1.2324d-2
        Material(TotMaterials)%GasWght(1) = 28.97d0
        Material(TotMaterials)%GasFract(1) = 1.0d0
        Material(TotMaterials)%AbsorpSolar = 0.0d0
        Material(TotMaterials)%AbsorpThermal = 0.0d0
        Material(TotMaterials)%AbsorpVisible = 0.0d0
        Material(TotMaterials)%Trans = 0.0d0
        Material(TotMaterials)%TransVis = 0.0d0
        Material(TotMaterials)%GlassTransDirtFactor = 0.0d0
        Material(TotMaterials)%ReflectShade = 0.0d0
        Material(TotMaterials)%ReflectShadeVis = 0.0d0
        Material(TotMaterials)%AbsorpThermalBack = 0.0d0
        Material(TotMaterials)%AbsorpThermalFront = 0.0d0
        Material(TotMaterials)%ReflectSolBeamBack = 0.0d0
        Material(TotMaterials)%ReflectSolBeamFront = 0.0d0
        Material(TotMaterials)%ReflectSolDiffBack = 0.0d0
        Material(TotMaterials)%ReflectSolDiffFront = 0.0d0
        Material(TotMaterials)%ReflectVisBeamBack = 0.0d0
        Material(TotMaterials)%ReflectVisBeamFront = 0.0d0
        Material(TotMaterials)%ReflectVisDiffBack = 0.0d0
        Material(TotMaterials)%ReflectVisDiffFront = 0.0d0
        Material(TotMaterials)%TransSolBeam = 0.0d0
        Material(TotMaterials)%TransThermal = 0.0d0
        Material(TotMaterials)%TransVisBeam = 0.0d0
        Material(TotMaterials)%BlindDataPtr = 0
        Material(TotMaterials)%WinShadeToGlassDist = 0.0d0
        Material(TotMaterials)%WinShadeTopOpeningMult = 0.0d0
        Material(TotMaterials)%WinShadeBottomOpeningMult = 0.0d0
        Material(TotMaterials)%WinShadeLeftOpeningMult = 0.0d0
        Material(TotMaterials)%WinShadeRightOpeningMult = 0.0d0
        Material(TotMaterials)%WinShadeAirFlowPermeability = 0.0d0
        Material(TotMaterials)%EMPDVALUE = 0.0d0
        Material(TotMaterials)%MoistACoeff = 0.0d0
        Material(TotMaterials)%MoistBCoeff = 0.0d0
        Material(TotMaterials)%MoistCCoeff = 0.0d0
        Material(TotMaterials)%MoistDCoeff = 0.0d0
        Material(TotMaterials)%EMPDaCoeff = 0.0d0
        Material(TotMaterials)%EMPDbCoeff = 0.0d0
        Material(TotMaterials)%EMPDcCoeff = 0.0d0
        Material(TotMaterials)%EMPDdCoeff = 0.0d0
      END IF  ! End of check if new air layer material has to be created
    END IF

    IF((loop==1.AND.ConstrNewSt==0).OR.(loop==2.AND.ConstrNewStSh==0)) THEN
      ! Create new constructions
      ConstrNew = TotConstructs + 1
      IF(loop==1) THEN
        Surface(SurfNum)%StormWinConstruction = ConstrNew
      ELSE
        Surface(SurfNum)%StormWinShadedConstruction = ConstrNew
      END IF
      ALLOCATE(ConstructSave(TotConstructs))
      ALLOCATE(NominalRSave(TotConstructs))
      ALLOCATE(NominalUSave(TotConstructs))
      ConstructSave(1:TotConstructs) = Construct(1:TotConstructs)
      NominalRSave(1:TotConstructs) = NominalRforNominalUCalculation(1:TotConstructs)
      NominalUSave(1:TotConstructs) = NominalU(1:TotConstructs)
      DEALLOCATE(Construct)
      DEALLOCATE(NominalRforNominalUCalculation)
      DEALLOCATE(NominalU)
      TotConstructs = ConstrNew
      ALLOCATE(Construct(TotConstructs))
      ALLOCATE(NominalRforNominalUCalculation(TotConstructs))
      ALLOCATE(NominalU(TotConstructs))
      Construct(1:TotConstructs-1) = ConstructSave(1:TotConstructs-1)
      NominalRforNominalUCalculation(1:TotConstructs-1) = NominalRSave(1:TotConstructs-1)
      NominalU(1:TotConstructs-1) = NominalUSave(1:TotConstructs-1)
      DEALLOCATE(ConstructSave)
      DEALLOCATE(NominalRSave)
      DEALLOCATE(NominalUSave)

      ConstrOld = ConstrNum
      IF(loop==2) ConstrOld = ConstrNumSh
      TotLayersOld = Construct(ConstrOld)%TotLayers
      Construct(ConstrNew)%LayerPoint(1:MaxLayersInConstruct) = 0
      Construct(ConstrNew)%LayerPoint(1) = StormWinMatNum
      Construct(ConstrNew)%LayerPoint(2) = MatNewStAir
      Construct(ConstrNew)%LayerPoint(3:TotLayersOld + 2) = Construct(ConstrOld)%LayerPoint(1:TotLayersOld)
      Construct(ConstrNew)%Name = ConstrNameSt
      IF(loop==2) Construct(ConstrNew)%Name = ConstrNameStSh
      Construct(ConstrNew)%TotLayers = TotLayersOld + 2
      Construct(ConstrNew)%TotSolidLayers = Construct(ConstrOld)%TotSolidLayers + 1
      Construct(ConstrNew)%TotGlassLayers = Construct(ConstrOld)%TotGlassLayers + 1
      Construct(ConstrNew)%TypeIsWindow = .true.
      Construct(ConstrNew)%InsideAbsorpVis      = 0.0d0
      Construct(ConstrNew)%OutsideAbsorpVis     = 0.0d0
      Construct(ConstrNew)%InsideAbsorpSolar    = 0.0d0
      Construct(ConstrNew)%OutsideAbsorpSolar   = 0.0d0
      Construct(ConstrNew)%InsideAbsorpThermal  = Construct(ConstrOld)%InsideAbsorpThermal
      Construct(ConstrNew)%OutsideAbsorpThermal = Material(StormWinMatNum)%AbsorpThermalFront
      Construct(ConstrNew)%OutSideRoughness     = VerySmooth
      Construct(ConstrNew)%DayltPropPtr        = 0
      Construct(ConstrNew)%CTFCross            = 0.0D0
      Construct(ConstrNew)%CTFFlux             = 0.0D0
      Construct(ConstrNew)%CTFInside           = 0.0D0
      Construct(ConstrNew)%CTFOutside          = 0.0D0
      Construct(ConstrNew)%CTFSourceIn         = 0.0D0
      Construct(ConstrNew)%CTFSourceOut        = 0.0D0
      Construct(ConstrNew)%CTFTimeStep         = 0.0D0
      Construct(ConstrNew)%CTFTSourceOut       = 0.0D0
      Construct(ConstrNew)%CTFTSourceIn        = 0.0D0
      Construct(ConstrNew)%CTFTSourceQ         = 0.0D0
      Construct(ConstrNew)%CTFTUserOut         = 0.0D0
      Construct(ConstrNew)%CTFTUserIn          = 0.0D0
      Construct(ConstrNew)%CTFTUserSource      = 0.0D0
      Construct(ConstrNew)%NumHistories        = 0
      Construct(ConstrNew)%NumCTFTerms         = 0
      Construct(ConstrNew)%UValue              = 0.0d0
      Construct(ConstrNew)%SourceSinkPresent   = .false.
      Construct(ConstrNew)%SolutionDimensions  = 0
      Construct(ConstrNew)%SourceAfterLayer    = 0
      Construct(ConstrNew)%TempAfterLayer      = 0
      Construct(ConstrNew)%ThicknessPerpend    = 0.0d0
      Construct(ConstrNew)%AbsDiffIn           = 0.0d0
      Construct(ConstrNew)%AbsDiffOut          = 0.0d0
      Construct(ConstrNew)%AbsDiff             = 0.0d0
      Construct(ConstrNew)%AbsDiffBack         = 0.0d0
      Construct(ConstrNew)%AbsDiffShade        = 0.0d0
      Construct(ConstrNew)%AbsDiffBackShade    = 0.0d0
      Construct(ConstrNew)%ShadeAbsorpThermal  = 0.0d0
      Construct(ConstrNew)%AbsBeamCoef         = 0.0d0
      Construct(ConstrNew)%AbsBeamBackCoef     = 0.0d0
      Construct(ConstrNew)%AbsBeamShadeCoef    = 0.0d0
      Construct(ConstrNew)%TransDiff           = 0.0d0
      Construct(ConstrNew)%TransDiffVis        = 0.0d0
      Construct(ConstrNew)%ReflectSolDiffBack  = 0.0d0
      Construct(ConstrNew)%ReflectSolDiffFront = 0.0d0
      Construct(ConstrNew)%ReflectVisDiffBack  = 0.0d0
      Construct(ConstrNew)%ReflectVisDiffFront = 0.0d0
      Construct(ConstrNew)%TransSolBeamCoef    = 0.0d0
      Construct(ConstrNew)%TransVisBeamCoef    = 0.0d0
      Construct(ConstrNew)%ReflSolBeamFrontCoef= 0.0d0
      Construct(ConstrNew)%ReflSolBeamBackCoef = 0.0d0
      Construct(ConstrNew)%W5FrameDivider      = 0
      Construct(ConstrNew)%FromWindow5DataFile = .false.
      Construct(ConstrNew)%W5FileMullionWidth  = 0.0d0
      Construct(ConstrNew)%W5FileMullionOrientation = 0
      Construct(ConstrNew)%W5FileGlazingSysWidth = 0.0d0
      Construct(ConstrNew)%W5FileGlazingSysHeight = 0.0d0
    END IF  ! End of check if new window constructions have to be created
  END DO  ! End of loop over unshaded and shaded window constructions
END DO  ! End of loop over storm window objects

RETURN
END SUBROUTINE CreateStormWindowConstructions

SUBROUTINE ModifyWindow(SurfNum,ErrorsFound,AddedSubSurfaces)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Fred Winkelmann
          !       DATE WRITTEN   Feb 2002
          !       MODIFIED       June 2004, FCW: SinAzim, CosAzim, SinTilt, CosTilt, OutNormVec, GrossArea
          !                       and Perimeter weren't being set for created window for case when
          !                       window from Window5DataFile had two different glazing systems. Also,
          !                       GrossArea and Perimeter of original window were not being recalculated.
          !                      October 2007, LKL: Net area for shading calculations was not being
          !                       recalculated.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! If a window from the Window5DataFile has one glazing system, modify the
          ! vertex coordinates of the original window to correspond to the dimensions
          ! of the glazing system on the Data File.
          ! If a window from the Window5DataFile has two different glazing systems, split
          ! the window into two separate windows with different properties and adjust the
          ! vertices of these windows taking into account the dimensions of the glazing systems
          ! on the Data File and the width and orientation of the mullion that separates
          ! the glazing systems.

          ! METHODOLOGY EMPLOYED:na
          ! REFERENCES:na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: RoundSigDigits
  USE Vectors

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)       :: SurfNum     ! SurfNum has construction of glazing system from Window5 Data File;
                                           ! If there is a second glazing systme on the Data File, SurfNum+1
                                           ! has the construction of the second glazing system.

  LOGICAL, INTENT(INOUT)    :: ErrorsFound ! Set to true if errors found
  INTEGER, INTENT(INOUT)    :: AddedSubSurfaces ! Subsurfaces added when window references a
                                                ! 2-glazing system Window5 data file entry
          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
  type rectangularwindow
    type(vector), dimension(4) :: Vertex
  end type rectangularwindow

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!unused1208  INTEGER :: TotSurfacesPrev                 ! Total number of surfaces before splitting window
!unused1208  INTEGER :: loop                            ! DO loop index
  REAL(r64)    :: H,W                             ! Height and width of original window (m)
!unused1208  REAL(r64)    :: MulWidth                        ! Mullion width (m)
  REAL(r64)    :: h1,w1                           ! height and width of first glazing system (m)
!unused1208  REAL(r64)    :: h2,w2                           ! height and width of second glazing system (m)
  type (vector) TVect
!unused1208  type (rectangularwindow) :: NewCoord
  type (rectangularwindow) :: OriginalCoord
  INTEGER :: IConst                          ! Construction number of first glazing system
  INTEGER :: IConst2                         ! Construction number of second glazing system
  CHARACTER(len=MaxNameLength) :: Const2Name ! Name of construction of second glazing system
!unused1208  REAL(r64)    :: AreaNew                         ! Sum of areas of the two glazing systems (m2)

IConst = SurfaceTmp(SurfNum)%Construction

! Height and width of original window
TVect=SurfaceTmp(SurfNum)%Vertex(3)-SurfaceTmp(SurfNum)%Vertex(2)
W = VecLength(TVect)  !SQRT((X(3)-X(2))**2 + (Y(3)-Y(2))**2 + (Z(3)-Z(2))**2)
TVect=SurfaceTmp(SurfNum)%Vertex(2)-SurfaceTmp(SurfNum)%Vertex(1)
H = VecLength(TVect)  !SQRT((X(1)-X(2))**2 + (Y(1)-Y(2))**2 + (Z(1)-Z(2))**2)

! Save coordinates of original window in case Window 5 data overwrites.
OriginalCoord%Vertex(1:SurfaceTmp(SurfNum)%Sides)=SurfaceTmp(SurfNum)%Vertex(1:SurfaceTmp(SurfNum)%Sides)

! Height and width of first glazing system
h1  = Construct(IConst)%W5FileGlazingSysHeight
w1  = Construct(IConst)%W5FileGlazingSysWidth

Const2Name = TRIM(Construct(IConst)%Name)//':2'
IConst2 = FindItemInList(Const2Name, Construct%Name,TotConstructs)

IF(IConst2 == 0) THEN  ! Only one glazing system on Window5 Data File for this window.

  ! So... original dimensions and area of window are used (entered in IDF)
  ! Warning if dimensions of original window differ from those on Data File by more than 10%

  IF(ABS((H-h1)/H) > 0.10d0 .OR. ABS((W-w1)/W) > 0.10d0) THEN

    IF (DisplayExtraWarnings) THEN
      CALL ShowWarningError('SurfaceGeometry: ModifyWindow: Window '//TRIM(SurfaceTmp(SurfNum)%Name)// &
       ' uses the Window5 Data File Construction '//TRIM(Construct(IConst)%Name))
      CALL ShowContinueError('The height '//TRIM(RoundSigDigits(H,3))//'(m) or width '// &
       TRIM(RoundSigDigits(W,3))//' (m) of this window differs by more than 10%')
      CALL ShowContinueError('from the corresponding height '//TRIM(RoundSigDigits(h1,3))// &
       ' (m) or width '//TRIM(RoundSigDigits(w1,3))//' (m) on the Window5 Data file.')
      CALL ShowContinueError('This will affect the frame heat transfer calculation if the frame in the Data File entry')
      CALL ShowContinueError('is not uniform, i.e., has sections with different geometry and/or thermal properties.')
    ELSE
      Warning1Count=Warning1Count+1
    ENDIF

  END IF

     ! Calculate net area for base surface
  SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%Area =  &
         SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%Area - SurfaceTmp(SurfNum)%Area
  IF (SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%Area <= 0.0d0) THEN
    CALL ShowSevereError('Subsurfaces have too much area for base surface='//  &
                         TRIM(SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%Name))
    CALL ShowContinueError('Subsurface creating error='//TRIM(SurfaceTmp(SurfNum)%Name))
    ErrorsFound=.true.
  ENDIF

     ! Net area of base surface with unity window multipliers (used in shadowing checks)
  SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%NetAreaShadowCalc =  &
     SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%NetAreaShadowCalc -   &
         SurfaceTmp(SurfNum)%Area/SurfaceTmp(SurfNum)%Multiplier

ELSE ! Two glazing systems on Window5 data file for this window

! if exterior window, okay.

  IF (SurfaceTmp(SurfNum)%ExtBoundCond == ExternalEnvironment) THEN
    !There are two glazing systems (separated by a vertical or horizontal mullion) on the Window5 Data File.
    ! Fill in geometry data for the second window (corresponding to the second glazing system on the data file.
    ! The first glazing system is assumed to be at left for vertical mullion, at bottom for horizontal mullion.
    ! The second glazing system is assumed to be at right for vertical mullion, at top for horizontal mullion.
    ! The lower left-hand corner of the original window (its vertex #2) is assumed to coincide with
    ! vertex #2 of the first glazing system.

    IF (DisplayExtraWarnings) THEN
      CALL ShowMessage('SurfaceGeometry: ModifyWindow: Window '// &
                      TRIM(SurfaceTmp(SurfNum)%Name)//' has been replaced with the Window 5/6 two glazing system="'//  &
                      TRIM(Construct(IConst)%Name)//'".')
      CALL ShowContinueError('Note that originally entered dimensions are overridden.')
    ELSE
      Warning2Count=Warning2Count+1
    ENDIF

      ! Allocate another window
    CALL AddWindow(SurfNum,ErrorsFound,AddedSubSurfaces)

  ELSEIF (SurfaceTmp(SurfNum)%ExtBoundCond > 0) THEN ! Interior window, specified  ! not external environment

    IF (DisplayExtraWarnings) THEN
      CALL ShowWarningError('SurfaceGeometry: ModifyWindow: Interior Window '// &
                      TRIM(SurfaceTmp(SurfNum)%Name)//' has been replaced with the Window 5/6 two glazing system="'//  &
                      TRIM(Construct(IConst)%Name)//'".')
      CALL ShowContinueError('Please check to make sure interior window is correct. '//  &
         'Note that originally entered dimensions are overridden.')
    ELSE
      Warning3Count=Warning3Count+1
    ENDIF

    CALL AddWindow(SurfNum,ErrorsFound,AddedSubSurfaces)

  ELSE    ! Interior window, specified not entered

    CALL ShowSevereError('SurfaceGeometry: ModifyWindow: Interior Window '// &
                    TRIM(SurfaceTmp(SurfNum)%Name)//' is a window in an adjacent zone.')
    CALL ShowContinueError('Attempted to add/reverse Window 5/6 multiple glazing system="'//  &
                    TRIM(Construct(IConst)%Name)//'".')
    CALL ShowContinueError('Cannot use these Window 5/6 constructs for these Interior Windows. Program will terminate.')
    ErrorsFound=.true.

  ENDIF

END IF ! End of check if one or two glazing systems are on the Window5 Data File

END SUBROUTINE ModifyWindow

SUBROUTINE AddWindow(SurfNum,ErrorsFound,AddedSubSurfaces)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   Nov 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This routine is called from ModifyWindow to add a window.  Allows it to be
          ! called in more than one place in the calling routine so as to be able to have
          ! specific warnings or errors issued.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: FindItemInList
  USE General, ONLY: RoundSigDigits
  USE Vectors

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)       :: SurfNum     ! SurfNum has construction of glazing system from Window5 Data File;
                                           ! If there is a second glazing systme on the Data File, SurfNum+1
                                           ! has the construction of the second glazing system.

  LOGICAL, INTENT(INOUT)    :: ErrorsFound ! Set to true if errors found
  INTEGER, INTENT(INOUT)    :: AddedSubSurfaces ! Subsurfaces added when window references a
                                                ! 2-glazing system Window5 data file entry

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS:
          ! na

          ! DERIVED TYPE DEFINITIONS:
  type rectangularwindow
    type(vector), dimension(4) :: Vertex
  end type rectangularwindow


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: TotSurfacesPrev                 ! Total number of surfaces before splitting window
  INTEGER :: loop                            ! DO loop index
  REAL(r64)    :: H,W                             ! Height and width of original window (m)
  REAL(r64)    :: MulWidth                        ! Mullion width (m)
  REAL(r64)    :: h1,w1                           ! height and width of first glazing system (m)
  REAL(r64)    :: h2,w2                           ! height and width of second glazing system (m)
  REAL(r64)    :: xa,ya,za,xb,yb,zb               ! Vertex intermediate variables (m)
  REAL(r64)    :: dx,dy                           ! Vertex displacements from original window (m)
  type (vector) TVect
  type (rectangularwindow) :: NewCoord
  type (rectangularwindow) :: OriginalCoord
  INTEGER :: IConst                          ! Construction number of first glazing system
  INTEGER :: IConst2                         ! Construction number of second glazing system
  CHARACTER(len=MaxNameLength) :: Const2Name ! Name of construction of second glazing system
  REAL(r64)    :: AreaNew                         ! Sum of areas of the two glazing systems (m2)

  IConst = SurfaceTmp(SurfNum)%Construction

  ! Height and width of original window
  TVect=SurfaceTmp(SurfNum)%Vertex(3)-SurfaceTmp(SurfNum)%Vertex(2)
  W = VecLength(TVect)  !SQRT((X(3)-X(2))**2 + (Y(3)-Y(2))**2 + (Z(3)-Z(2))**2)
  TVect=SurfaceTmp(SurfNum)%Vertex(2)-SurfaceTmp(SurfNum)%Vertex(1)
  H = VecLength(TVect)  !SQRT((X(1)-X(2))**2 + (Y(1)-Y(2))**2 + (Z(1)-Z(2))**2)

  ! Save coordinates of original window in case Window 5 data overwrites.
  OriginalCoord%Vertex(1:SurfaceTmp(SurfNum)%Sides)=SurfaceTmp(SurfNum)%Vertex(1:SurfaceTmp(SurfNum)%Sides)

  ! Height and width of first glazing system
  h1  = Construct(IConst)%W5FileGlazingSysHeight
  w1  = Construct(IConst)%W5FileGlazingSysWidth

  Const2Name = TRIM(Construct(IConst)%Name)//':2'
  IConst2 = FindItemInList(Const2Name, Construct%Name,TotConstructs)

    AddedSubSurfaces = AddedSubSurfaces + 1
    TotSurfacesPrev = TotSurfaces
    ALLOCATE(SurfaceTmpSave(TotSurfacesPrev))
    DO loop = 1,TotSurfacesPrev
      SurfaceTmpSave(loop) = SurfaceTmp(loop)
    END DO
    DEALLOCATE(SurfaceTmp)
    TotSurfaces = TotSurfaces + 1
    ALLOCATE(SurfaceTmp(TotSurfaces))
    DO loop = 1,TotSurfacesPrev
      SurfaceTmp(loop) = SurfaceTmpSave(loop)
    END DO
    DEALLOCATE(SurfaceTmpSave)

    ALLOCATE(SurfaceTmp(TotSurfaces)%Vertex(4))

    SurfaceTmp(TotSurfaces)%Name = TRIM(SurfaceTmp(SurfNum)%Name)//':2'
    SurfaceTmp(TotSurfaces)%Construction = IConst2
    SurfaceTmp(TotSurfaces)%ConstructionStoredInputValue = IConst2
    SurfaceTmp(TotSurfaces)%Class = SurfaceTmp(SurfNum)%Class
    SurfaceTmp(TotSurfaces)%Azimuth = SurfaceTmp(SurfNum)%Azimuth
    ! Sine and cosine of azimuth and tilt
    SurfaceTmp(TotSurfaces)%SinAzim = SurfaceTmp(SurfNum)%SinAzim
    SurfaceTmp(TotSurfaces)%CosAzim = SurfaceTmp(SurfNum)%CosAzim
    SurfaceTmp(TotSurfaces)%SinTilt = SurfaceTmp(SurfNum)%SinTilt
    SurfaceTmp(TotSurfaces)%CosTilt = SurfaceTmp(SurfNum)%CosTilt
    ! Outward normal unit vector (pointing away from room)
    SurfaceTmp(TotSurfaces)%Centroid = SurfaceTmp(SurfNum)%Centroid
    SurfaceTmp(TotSurfaces)%lcsx = SurfaceTmp(SurfNum)%lcsx
    SurfaceTmp(TotSurfaces)%lcsy = SurfaceTmp(SurfNum)%lcsy
    SurfaceTmp(TotSurfaces)%lcsz = SurfaceTmp(SurfNum)%lcsz
    SurfaceTmp(TotSurfaces)%NewellAreaVector = SurfaceTmp(SurfNum)%NewellAreaVector
    SurfaceTmp(TotSurfaces)%OutNormVec = SurfaceTmp(SurfNum)%OutNormVec
    SurfaceTmp(TotSurfaces)%Reveal = SurfaceTmp(SurfNum)%Reveal
    SurfaceTmp(TotSurfaces)%Shape = SurfaceTmp(SurfNum)%Shape
    SurfaceTmp(TotSurfaces)%Sides = SurfaceTmp(SurfNum)%Sides
    SurfaceTmp(TotSurfaces)%Tilt = SurfaceTmp(SurfNum)%Tilt
    SurfaceTmp(TotSurfaces)%HeatTransSurf = SurfaceTmp(SurfNum)%HeatTransSurf
    SurfaceTmp(TotSurfaces)%BaseSurfName = SurfaceTmp(SurfNum)%BaseSurfName
    SurfaceTmp(TotSurfaces)%BaseSurf = SurfaceTmp(SurfNum)%BaseSurf
    SurfaceTmp(TotSurfaces)%ZoneName = SurfaceTmp(SurfNum)%ZoneName
    SurfaceTmp(TotSurfaces)%Zone = SurfaceTmp(SurfNum)%Zone
    SurfaceTmp(TotSurfaces)%ExtBoundCondName = SurfaceTmp(SurfNum)%ExtBoundCondName
    SurfaceTmp(TotSurfaces)%ExtBoundCond = SurfaceTmp(SurfNum)%ExtBoundCond
    SurfaceTmp(TotSurfaces)%ExtSolar = SurfaceTmp(SurfNum)%ExtSolar
    SurfaceTmp(TotSurfaces)%ExtWind = SurfaceTmp(SurfNum)%ExtWind
    SurfaceTmp(TotSurfaces)%ViewFactorGround = SurfaceTmp(SurfNum)%ViewFactorGround
    SurfaceTmp(TotSurfaces)%ViewFactorSky = SurfaceTmp(SurfNum)%ViewFactorSky
    SurfaceTmp(TotSurfaces)%ViewFactorGroundIR = SurfaceTmp(SurfNum)%ViewFactorGroundIR
    SurfaceTmp(TotSurfaces)%ViewFactorSkyIR = SurfaceTmp(SurfNum)%ViewFactorSkyIR
    SurfaceTmp(TotSurfaces)%OSCPtr = SurfaceTmp(SurfNum)%OSCPtr
    SurfaceTmp(TotSurfaces)%SchedShadowSurfIndex = SurfaceTmp(SurfNum)%SchedShadowSurfIndex
    SurfaceTmp(TotSurfaces)%ShadowSurfSchedVaries = SurfaceTmp(SurfNum)%ShadowSurfSchedVaries
    SurfaceTmp(TotSurfaces)%MaterialMovInsulExt = SurfaceTmp(SurfNum)%MaterialMovInsulExt
    SurfaceTmp(TotSurfaces)%MaterialMovInsulInt = SurfaceTmp(SurfNum)%MaterialMovInsulInt
    SurfaceTmp(TotSurfaces)%SchedMovInsulExt = SurfaceTmp(SurfNum)%SchedMovInsulExt
    SurfaceTmp(TotSurfaces)%WindowShadingControlPtr = SurfaceTmp(SurfNum)%WindowShadingControlPtr
    SurfaceTmp(TotSurfaces)%ShadedConstruction = SurfaceTmp(SurfNum)%ShadedConstruction
    SurfaceTmp(TotSurfaces)%FrameDivider = SurfaceTmp(SurfNum)%FrameDivider
    SurfaceTmp(TotSurfaces)%Multiplier = SurfaceTmp(SurfNum)%Multiplier
    SurfaceTmp(TotSurfaces)%NetAreaShadowCalc = SurfaceTmp(SurfNum)%NetAreaShadowCalc

    MulWidth = Construct(IConst)%W5FileMullionWidth
    w2  = Construct(IConst2)%W5FileGlazingSysWidth
    h2  = Construct(IConst2)%W5FileGlazingSysHeight

    ! Correction to net area of base surface. Add back in the original glazing area and subtract the
    ! area of the two glazing systems. Note that for Surface(SurfNum)%Class = 'Window' the effect
    ! of a window multiplier is included in the glazing area. Note that frame areas are subtracted later.

    AreaNew = SurfaceTmp(SurfNum)%Multiplier * (h1*w1 + h2*w2)  ! both glazing systems
       ! Adjust net area for base surface
    SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%Area =  &
           SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%Area - AreaNew

       ! Net area of base surface with unity window multipliers (used in shadowing checks)
    SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%NetAreaShadowCalc =  &
    SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%NetAreaShadowCalc - AreaNew/SurfaceTmp(SurfNum)%Multiplier

       ! Reset area, etc. of original window
    SurfaceTmp(SurfNum)%Area = SurfaceTmp(SurfNum)%Multiplier * (h1*w1)
    SurfaceTmp(SurfNum)%GrossArea = SurfaceTmp(SurfNum)%Area
    SurfaceTmp(SurfNum)%NetAreaShadowCalc = h1*w1
    SurfaceTmp(SurfNum)%Perimeter = 2*(h1+w1)
    SurfaceTmp(SurfNum)%Height = h1
    SurfaceTmp(SurfNum)%Width  = w1
       ! Set area, etc. of new window
    SurfaceTmp(TotSurfaces)%Area = SurfaceTmp(TotSurfaces)%Multiplier * (h2*w2)
    SurfaceTmp(TotSurfaces)%GrossArea = SurfaceTmp(TotSurfaces)%Area
    SurfaceTmp(TotSurfaces)%NetAreaShadowCalc = h2*w2
    SurfaceTmp(TotSurfaces)%Perimeter = 2*(h2+w2)
    SurfaceTmp(TotSurfaces)%Height = h2
    SurfaceTmp(TotSurfaces)%Width  = w2

    IF (SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%Area <= 0.0d0) THEN
      CALL ShowSevereError('SurfaceGeometry: ModifyWindow: Subsurfaces have too much area for base surface='//  &
                         TRIM(SurfaceTmp(SurfaceTmp(SurfNum)%BaseSurf)%Name))
      CALL ShowContinueError('Subsurface (window) creating error='//TRIM(SurfaceTmp(SurfNum)%Name))
      CALL ShowContinueError('This window has been replaced by two windows from the Window5 Data File of total area '//  &
                             TRIM(RoundSigDigits(AreaNew,2))//' m2')
      ErrorsFound=.true.
    ENDIF

    ! Assign vertices to the new window; modify vertices of original window.
    ! In the following, vertices are numbered counter-clockwise with vertex #1 at the upper left.

    IF(Construct(IConst)%W5FileMullionOrientation == Vertical) THEN

      ! VERTICAL MULLION: original window is modified to become left-hand glazing (system #1);
      ! new window is created to become right-hand glazing (system #2)

      ! Left-hand glazing

      ! Vertex 1
      dx = 0.0d0
      dy = H - h1
      xa = OriginalCoord%Vertex(1)%x + (dy/H)*(OriginalCoord%Vertex(2)%x-OriginalCoord%Vertex(1)%x)
      ya = OriginalCoord%Vertex(1)%y + (dy/H)*(OriginalCoord%Vertex(2)%y-OriginalCoord%Vertex(1)%y)
      za = OriginalCoord%Vertex(1)%z + (dy/H)*(OriginalCoord%Vertex(2)%z-OriginalCoord%Vertex(1)%z)
      xb = OriginalCoord%Vertex(4)%x + (dy/H)*(OriginalCoord%Vertex(3)%x-OriginalCoord%Vertex(4)%x)
      yb = OriginalCoord%Vertex(4)%y + (dy/H)*(OriginalCoord%Vertex(3)%y-OriginalCoord%Vertex(4)%y)
      zb = OriginalCoord%Vertex(4)%z + (dy/H)*(OriginalCoord%Vertex(3)%z-OriginalCoord%Vertex(4)%z)
      NewCoord%Vertex(1)%x = xa + (dx/W)*(xb-xa)
      NewCoord%Vertex(1)%y = ya + (dx/W)*(yb-ya)
      NewCoord%Vertex(1)%z = za + (dx/W)*(zb-za)

      ! Vertex 2
      dx = 0.0d0
      dy = H
      xa = OriginalCoord%Vertex(1)%x + (dy/H)*(OriginalCoord%Vertex(2)%x-OriginalCoord%Vertex(1)%x)
      ya = OriginalCoord%Vertex(1)%y + (dy/H)*(OriginalCoord%Vertex(2)%y-OriginalCoord%Vertex(1)%y)
      za = OriginalCoord%Vertex(1)%z + (dy/H)*(OriginalCoord%Vertex(2)%z-OriginalCoord%Vertex(1)%z)
      xb = OriginalCoord%Vertex(4)%x + (dy/H)*(OriginalCoord%Vertex(3)%x-OriginalCoord%Vertex(4)%x)
      yb = OriginalCoord%Vertex(4)%y + (dy/H)*(OriginalCoord%Vertex(3)%y-OriginalCoord%Vertex(4)%y)
      zb = OriginalCoord%Vertex(4)%z + (dy/H)*(OriginalCoord%Vertex(3)%z-OriginalCoord%Vertex(4)%z)
      NewCoord%Vertex(2)%x = xa + (dx/W)*(xb-xa)
      NewCoord%Vertex(2)%y = ya + (dx/W)*(yb-ya)
      NewCoord%Vertex(2)%z = za + (dx/W)*(zb-za)

      ! Vertex 3
      dx = w1
      dy = H
      xa = OriginalCoord%Vertex(1)%x + (dy/H)*(OriginalCoord%Vertex(2)%x-OriginalCoord%Vertex(1)%x)
      ya = OriginalCoord%Vertex(1)%y + (dy/H)*(OriginalCoord%Vertex(2)%y-OriginalCoord%Vertex(1)%y)
      za = OriginalCoord%Vertex(1)%z + (dy/H)*(OriginalCoord%Vertex(2)%z-OriginalCoord%Vertex(1)%z)
      xb = OriginalCoord%Vertex(4)%x + (dy/H)*(OriginalCoord%Vertex(3)%x-OriginalCoord%Vertex(4)%x)
      yb = OriginalCoord%Vertex(4)%y + (dy/H)*(OriginalCoord%Vertex(3)%y-OriginalCoord%Vertex(4)%y)
      zb = OriginalCoord%Vertex(4)%z + (dy/H)*(OriginalCoord%Vertex(3)%z-OriginalCoord%Vertex(4)%z)
      NewCoord%Vertex(3)%x = xa + (dx/W)*(xb-xa)
      NewCoord%Vertex(3)%y = ya + (dx/W)*(yb-ya)
      NewCoord%Vertex(3)%z = za + (dx/W)*(zb-za)

      ! Vertex 4
      dx = w1
      dy = H - h1
      xa = OriginalCoord%Vertex(1)%x + (dy/H)*(OriginalCoord%Vertex(2)%x-OriginalCoord%Vertex(1)%x)
      ya = OriginalCoord%Vertex(1)%y + (dy/H)*(OriginalCoord%Vertex(2)%y-OriginalCoord%Vertex(1)%y)
      za = OriginalCoord%Vertex(1)%z + (dy/H)*(OriginalCoord%Vertex(2)%z-OriginalCoord%Vertex(1)%z)
      xb = OriginalCoord%Vertex(4)%x + (dy/H)*(OriginalCoord%Vertex(3)%x-OriginalCoord%Vertex(4)%x)
      yb = OriginalCoord%Vertex(4)%y + (dy/H)*(OriginalCoord%Vertex(3)%y-OriginalCoord%Vertex(4)%y)
      zb = OriginalCoord%Vertex(4)%z + (dy/H)*(OriginalCoord%Vertex(3)%z-OriginalCoord%Vertex(4)%z)
      NewCoord%Vertex(4)%x = xa + (dx/W)*(xb-xa)
      NewCoord%Vertex(4)%y = ya + (dx/W)*(yb-ya)
      NewCoord%Vertex(4)%z = za + (dx/W)*(zb-za)

      DO loop = 1,SurfaceTmp(SurfNum)%Sides
        SurfaceTmp(SurfNum)%Vertex(loop)  = NewCoord%Vertex(loop)
      END DO

      ! Right-hand glazing

      ! Vertex 1
      dx = w1 + MulWidth
      dy = H - (h1+h2)/2.0d0
      xa = OriginalCoord%Vertex(1)%x + (dy/H)*(OriginalCoord%Vertex(2)%x-OriginalCoord%Vertex(1)%x)
      ya = OriginalCoord%Vertex(1)%y + (dy/H)*(OriginalCoord%Vertex(2)%y-OriginalCoord%Vertex(1)%y)
      za = OriginalCoord%Vertex(1)%z + (dy/H)*(OriginalCoord%Vertex(2)%z-OriginalCoord%Vertex(1)%z)
      xb = OriginalCoord%Vertex(4)%x + (dy/H)*(OriginalCoord%Vertex(3)%x-OriginalCoord%Vertex(4)%x)
      yb = OriginalCoord%Vertex(4)%y + (dy/H)*(OriginalCoord%Vertex(3)%y-OriginalCoord%Vertex(4)%y)
      zb = OriginalCoord%Vertex(4)%z + (dy/H)*(OriginalCoord%Vertex(3)%z-OriginalCoord%Vertex(4)%z)
      NewCoord%Vertex(1)%x = xa + (dx/W)*(xb-xa)
      NewCoord%Vertex(1)%y = ya + (dx/W)*(yb-ya)
      NewCoord%Vertex(1)%z = za + (dx/W)*(zb-za)

      ! Vertex 2
      dx = w1 + MulWidth
      dy = H + (h2-h1)/2.0d0
      xa = OriginalCoord%Vertex(1)%x + (dy/H)*(OriginalCoord%Vertex(2)%x-OriginalCoord%Vertex(1)%x)
      ya = OriginalCoord%Vertex(1)%y + (dy/H)*(OriginalCoord%Vertex(2)%y-OriginalCoord%Vertex(1)%y)
      za = OriginalCoord%Vertex(1)%z + (dy/H)*(OriginalCoord%Vertex(2)%z-OriginalCoord%Vertex(1)%z)
      xb = OriginalCoord%Vertex(4)%x + (dy/H)*(OriginalCoord%Vertex(3)%x-OriginalCoord%Vertex(4)%x)
      yb = OriginalCoord%Vertex(4)%y + (dy/H)*(OriginalCoord%Vertex(3)%y-OriginalCoord%Vertex(4)%y)
      zb = OriginalCoord%Vertex(4)%z + (dy/H)*(OriginalCoord%Vertex(3)%z-OriginalCoord%Vertex(4)%z)
      NewCoord%Vertex(2)%x = xa + (dx/W)*(xb-xa)
      NewCoord%Vertex(2)%y = ya + (dx/W)*(yb-ya)
      NewCoord%Vertex(2)%z = za + (dx/W)*(zb-za)

      ! Vertex 3
      dx = w1 + MulWidth + w2
      dy = H + (h2-h1)/2.0d0
      xa = OriginalCoord%Vertex(1)%x + (dy/H)*(OriginalCoord%Vertex(2)%x-OriginalCoord%Vertex(1)%x)
      ya = OriginalCoord%Vertex(1)%y + (dy/H)*(OriginalCoord%Vertex(2)%y-OriginalCoord%Vertex(1)%y)
      za = OriginalCoord%Vertex(1)%z + (dy/H)*(OriginalCoord%Vertex(2)%z-OriginalCoord%Vertex(1)%z)
      xb = OriginalCoord%Vertex(4)%x + (dy/H)*(OriginalCoord%Vertex(3)%x-OriginalCoord%Vertex(4)%x)
      yb = OriginalCoord%Vertex(4)%y + (dy/H)*(OriginalCoord%Vertex(3)%y-OriginalCoord%Vertex(4)%y)
      zb = OriginalCoord%Vertex(4)%z + (dy/H)*(OriginalCoord%Vertex(3)%z-OriginalCoord%Vertex(4)%z)
      NewCoord%Vertex(3)%x = xa + (dx/W)*(xb-xa)
      NewCoord%Vertex(3)%y = ya + (dx/W)*(yb-ya)
      NewCoord%Vertex(3)%z = za + (dx/W)*(zb-za)

      ! Vertex 4
      dx = w1 + MulWidth + w2
      dy = H - (h1+h2)/2.0d0
      xa = OriginalCoord%Vertex(1)%x + (dy/H)*(OriginalCoord%Vertex(2)%x-OriginalCoord%Vertex(1)%x)
      ya = OriginalCoord%Vertex(1)%y + (dy/H)*(OriginalCoord%Vertex(2)%y-OriginalCoord%Vertex(1)%y)
      za = OriginalCoord%Vertex(1)%z + (dy/H)*(OriginalCoord%Vertex(2)%z-OriginalCoord%Vertex(1)%z)
      xb = OriginalCoord%Vertex(4)%x + (dy/H)*(OriginalCoord%Vertex(3)%x-OriginalCoord%Vertex(4)%x)
      yb = OriginalCoord%Vertex(4)%y + (dy/H)*(OriginalCoord%Vertex(3)%y-OriginalCoord%Vertex(4)%y)
      zb = OriginalCoord%Vertex(4)%z + (dy/H)*(OriginalCoord%Vertex(3)%z-OriginalCoord%Vertex(4)%z)
      NewCoord%Vertex(4)%x = xa + (dx/W)*(xb-xa)
      NewCoord%Vertex(4)%y = ya + (dx/W)*(yb-ya)
      NewCoord%Vertex(4)%z = za + (dx/W)*(zb-za)

      DO loop = 1,SurfaceTmp(TotSurfaces)%Sides
        SurfaceTmp(TotSurfaces)%Vertex(loop)  = NewCoord%Vertex(loop)
      END DO

    ELSE ! Horizontal mullion

      ! HORIZONTAL MULLION: original window is modified to become bottom glazing (system #1);
      ! new window is created to become top glazing (system #2)

      ! Bottom glazing

      ! Vertex 1
      dx = 0.0d0
      dy = H - h1
      xa = OriginalCoord%Vertex(1)%x + (dy/H)*(OriginalCoord%Vertex(2)%x-OriginalCoord%Vertex(1)%x)
      ya = OriginalCoord%Vertex(1)%y + (dy/H)*(OriginalCoord%Vertex(2)%y-OriginalCoord%Vertex(1)%y)
      za = OriginalCoord%Vertex(1)%z + (dy/H)*(OriginalCoord%Vertex(2)%z-OriginalCoord%Vertex(1)%z)
      xb = OriginalCoord%Vertex(4)%x + (dy/H)*(OriginalCoord%Vertex(3)%x-OriginalCoord%Vertex(4)%x)
      yb = OriginalCoord%Vertex(4)%y + (dy/H)*(OriginalCoord%Vertex(3)%y-OriginalCoord%Vertex(4)%y)
      zb = OriginalCoord%Vertex(4)%z + (dy/H)*(OriginalCoord%Vertex(3)%z-OriginalCoord%Vertex(4)%z)
      NewCoord%Vertex(1)%x = xa + (dx/W)*(xb-xa)
      NewCoord%Vertex(1)%y = ya + (dx/W)*(yb-ya)
      NewCoord%Vertex(1)%z = za + (dx/W)*(zb-za)

      ! Vertex 2
      dx = 0.0d0
      dy = H
      xa = OriginalCoord%Vertex(1)%x + (dy/H)*(OriginalCoord%Vertex(2)%x-OriginalCoord%Vertex(1)%x)
      ya = OriginalCoord%Vertex(1)%y + (dy/H)*(OriginalCoord%Vertex(2)%y-OriginalCoord%Vertex(1)%y)
      za = OriginalCoord%Vertex(1)%z + (dy/H)*(OriginalCoord%Vertex(2)%z-OriginalCoord%Vertex(1)%z)
      xb = OriginalCoord%Vertex(4)%x + (dy/H)*(OriginalCoord%Vertex(3)%x-OriginalCoord%Vertex(4)%x)
      yb = OriginalCoord%Vertex(4)%y + (dy/H)*(OriginalCoord%Vertex(3)%y-OriginalCoord%Vertex(4)%y)
      zb = OriginalCoord%Vertex(4)%z + (dy/H)*(OriginalCoord%Vertex(3)%z-OriginalCoord%Vertex(4)%z)
      NewCoord%Vertex(2)%x = xa + (dx/W)*(xb-xa)
      NewCoord%Vertex(2)%y = ya + (dx/W)*(yb-ya)
      NewCoord%Vertex(2)%z = za + (dx/W)*(zb-za)

      ! Vertex 3
      dx = w1
      dy = H
      xa = OriginalCoord%Vertex(1)%x + (dy/H)*(OriginalCoord%Vertex(2)%x-OriginalCoord%Vertex(1)%x)
      ya = OriginalCoord%Vertex(1)%y + (dy/H)*(OriginalCoord%Vertex(2)%y-OriginalCoord%Vertex(1)%y)
      za = OriginalCoord%Vertex(1)%z + (dy/H)*(OriginalCoord%Vertex(2)%z-OriginalCoord%Vertex(1)%z)
      xb = OriginalCoord%Vertex(4)%x + (dy/H)*(OriginalCoord%Vertex(3)%x-OriginalCoord%Vertex(4)%x)
      yb = OriginalCoord%Vertex(4)%y + (dy/H)*(OriginalCoord%Vertex(3)%y-OriginalCoord%Vertex(4)%y)
      zb = OriginalCoord%Vertex(4)%z + (dy/H)*(OriginalCoord%Vertex(3)%z-OriginalCoord%Vertex(4)%z)
      NewCoord%Vertex(3)%x = xa + (dx/W)*(xb-xa)
      NewCoord%Vertex(3)%y = ya + (dx/W)*(yb-ya)
      NewCoord%Vertex(3)%z = za + (dx/W)*(zb-za)

      ! Vertex 4
      dx = w1
      dy = H - h1
      xa = OriginalCoord%Vertex(1)%x + (dy/H)*(OriginalCoord%Vertex(2)%x-OriginalCoord%Vertex(1)%x)
      ya = OriginalCoord%Vertex(1)%y + (dy/H)*(OriginalCoord%Vertex(2)%y-OriginalCoord%Vertex(1)%y)
      za = OriginalCoord%Vertex(1)%z + (dy/H)*(OriginalCoord%Vertex(2)%z-OriginalCoord%Vertex(1)%z)
      xb = OriginalCoord%Vertex(4)%x + (dy/H)*(OriginalCoord%Vertex(3)%x-OriginalCoord%Vertex(4)%x)
      yb = OriginalCoord%Vertex(4)%y + (dy/H)*(OriginalCoord%Vertex(3)%y-OriginalCoord%Vertex(4)%y)
      zb = OriginalCoord%Vertex(4)%z + (dy/H)*(OriginalCoord%Vertex(3)%z-OriginalCoord%Vertex(4)%z)
      NewCoord%Vertex(4)%x = xa + (dx/W)*(xb-xa)
      NewCoord%Vertex(4)%y = ya + (dx/W)*(yb-ya)
      NewCoord%Vertex(4)%z = za + (dx/W)*(zb-za)

      DO loop = 1,SurfaceTmp(SurfNum)%Sides
        SurfaceTmp(SurfNum)%Vertex(loop)  = NewCoord%Vertex(loop)
      END DO

      ! Top glazing

      ! Vertex 1
      dx = (w1 - w2)/2.0d0
      dy = H - (h1+h2+MulWidth)
      xa = OriginalCoord%Vertex(1)%x + (dy/H)*(OriginalCoord%Vertex(2)%x-OriginalCoord%Vertex(1)%x)
      ya = OriginalCoord%Vertex(1)%y + (dy/H)*(OriginalCoord%Vertex(2)%y-OriginalCoord%Vertex(1)%y)
      za = OriginalCoord%Vertex(1)%z + (dy/H)*(OriginalCoord%Vertex(2)%z-OriginalCoord%Vertex(1)%z)
      xb = OriginalCoord%Vertex(4)%x + (dy/H)*(OriginalCoord%Vertex(3)%x-OriginalCoord%Vertex(4)%x)
      yb = OriginalCoord%Vertex(4)%y + (dy/H)*(OriginalCoord%Vertex(3)%y-OriginalCoord%Vertex(4)%y)
      zb = OriginalCoord%Vertex(4)%z + (dy/H)*(OriginalCoord%Vertex(3)%z-OriginalCoord%Vertex(4)%z)
      NewCoord%Vertex(1)%x = xa + (dx/W)*(xb-xa)
      NewCoord%Vertex(1)%y = ya + (dx/W)*(yb-ya)
      NewCoord%Vertex(1)%z = za + (dx/W)*(zb-za)

      ! Vertex 2
      dx = (w1 - w2)/2.0d0
      dy = H - (h1+MulWidth)
      xa = OriginalCoord%Vertex(1)%x + (dy/H)*(OriginalCoord%Vertex(2)%x-OriginalCoord%Vertex(1)%x)
      ya = OriginalCoord%Vertex(1)%y + (dy/H)*(OriginalCoord%Vertex(2)%y-OriginalCoord%Vertex(1)%y)
      za = OriginalCoord%Vertex(1)%z + (dy/H)*(OriginalCoord%Vertex(2)%z-OriginalCoord%Vertex(1)%z)
      xb = OriginalCoord%Vertex(4)%x + (dy/H)*(OriginalCoord%Vertex(3)%x-OriginalCoord%Vertex(4)%x)
      yb = OriginalCoord%Vertex(4)%y + (dy/H)*(OriginalCoord%Vertex(3)%y-OriginalCoord%Vertex(4)%y)
      zb = OriginalCoord%Vertex(4)%z + (dy/H)*(OriginalCoord%Vertex(3)%z-OriginalCoord%Vertex(4)%z)
      NewCoord%Vertex(2)%x = xa + (dx/W)*(xb-xa)
      NewCoord%Vertex(2)%y = ya + (dx/W)*(yb-ya)
      NewCoord%Vertex(2)%z = za + (dx/W)*(zb-za)

      ! Vertex 3
      dx = (w1 + w2)/2.0d0
      dy = H - (h1+MulWidth)
      xa = OriginalCoord%Vertex(1)%x + (dy/H)*(OriginalCoord%Vertex(2)%x-OriginalCoord%Vertex(1)%x)
      ya = OriginalCoord%Vertex(1)%y + (dy/H)*(OriginalCoord%Vertex(2)%y-OriginalCoord%Vertex(1)%y)
      za = OriginalCoord%Vertex(1)%z + (dy/H)*(OriginalCoord%Vertex(2)%z-OriginalCoord%Vertex(1)%z)
      xb = OriginalCoord%Vertex(4)%x + (dy/H)*(OriginalCoord%Vertex(3)%x-OriginalCoord%Vertex(4)%x)
      yb = OriginalCoord%Vertex(4)%y + (dy/H)*(OriginalCoord%Vertex(3)%y-OriginalCoord%Vertex(4)%y)
      zb = OriginalCoord%Vertex(4)%z + (dy/H)*(OriginalCoord%Vertex(3)%z-OriginalCoord%Vertex(4)%z)
      NewCoord%Vertex(3)%x = xa + (dx/W)*(xb-xa)
      NewCoord%Vertex(3)%y = ya + (dx/W)*(yb-ya)
      NewCoord%Vertex(3)%z = za + (dx/W)*(zb-za)

      ! Vertex 4
      dx = (w1 + w2)/2.0d0
      dy = H - (h1+h2+MulWidth)
      xa = OriginalCoord%Vertex(1)%x + (dy/H)*(OriginalCoord%Vertex(2)%x-OriginalCoord%Vertex(1)%x)
      ya = OriginalCoord%Vertex(1)%y + (dy/H)*(OriginalCoord%Vertex(2)%y-OriginalCoord%Vertex(1)%y)
      za = OriginalCoord%Vertex(1)%z + (dy/H)*(OriginalCoord%Vertex(2)%z-OriginalCoord%Vertex(1)%z)
      xb = OriginalCoord%Vertex(4)%x + (dy/H)*(OriginalCoord%Vertex(3)%x-OriginalCoord%Vertex(4)%x)
      yb = OriginalCoord%Vertex(4)%y + (dy/H)*(OriginalCoord%Vertex(3)%y-OriginalCoord%Vertex(4)%y)
      zb = OriginalCoord%Vertex(4)%z + (dy/H)*(OriginalCoord%Vertex(3)%z-OriginalCoord%Vertex(4)%z)
      NewCoord%Vertex(4)%x = xa + (dx/W)*(xb-xa)
      NewCoord%Vertex(4)%y = ya + (dx/W)*(yb-ya)
      NewCoord%Vertex(4)%z = za + (dx/W)*(zb-za)

      DO loop = 1,SurfaceTmp(TotSurfaces)%Sides
        SurfaceTmp(TotSurfaces)%Vertex(loop)  = NewCoord%Vertex(loop)
      END DO

    END IF  ! End of check if vertical or horizontal mullion

  RETURN

END SUBROUTINE AddWindow

SUBROUTINE TransformVertsByAspect(SurfNum,NSides)
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent T Griffith
          !       DATE WRITTEN   April 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Alter input for surface geometry
          ! Optimizing building design for energy can involve
          !  altering building geometry.  Rather than assemble routines to transform
          !  geometry through pre-processing on input, it may be simpler to change
          !  vertices within EnergyPlus since it already reads the data from the input
          !  file and there would no longer be a need to rewrite the text data.
          !  This is essentially a crude hack to allow adjusting geometry with
          !  a single parameter...
          !

          ! METHODOLOGY EMPLOYED:
          ! once vertices have been converted to WCS, change them to reflect a different aspect
          ! ratio for the entire building based on user input.
          ! This routine is called once for each surface by subroutine GetVertices

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor

 IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine


          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)                     :: SurfNum     ! Current surface number
  INTEGER, INTENT(IN)                     :: NSides      ! Number of sides to figure


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
  REAL(r64),SAVE          :: OldAspectRatio
  REAL(r64),SAVE          :: NewAspectRatio
  Logical, save      :: firstTime = .true.
  Logical, save      :: noTransform = .true.
  CHARACTER(len=2),save   :: transformPlane
  INTEGER            :: N
  REAL(r64)          :: Xo, XnoRot, Xtrans
  REAL(r64)          :: Yo, YnoRot, Ytrans
  !begin execution
  !get user input...
  IF (firstTime) then
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
       firstTime   = .false.
       noTransform = .false.
       AspectTransform = .true.
       IF (WorldCoordSystem) THEN
         CALL ShowWarningError(CurrentModuleObject//': must use Relative Coordinate System.  '// &
               'Transform request ignored.')
         noTransform=.true.
         AspectTransform=.false.
       ENDIF
    ELSE
       firstTime = .false.
    Endif
  endif
  If (noTransform) return

  !check surface type.
  IF (.not. SurfaceTmp(SurfNum)%HeatTransSurf) THEN
    ! Site Shading do not get transformed.
    IF (SurfaceTmp(SurfNum)%Class == SurfaceClass_Detached_F) return
  ENDIF


    !testing method of transforming  x and y coordinates as follows


    ! this works if not rotated wrt north axis ... but if it is, then trouble
    ! try to first derotate it , transform by aspect and then rotate back.

    DO N=1,NSides
      Xo = SurfaceTmp(SurfNum)%Vertex(N)%X ! world coordinates.... shifted by relative north angle...
      Yo = SurfaceTmp(SurfNum)%Vertex(N)%Y
      ! next derotate the building
      XnoRot=Xo * CosBldgRelNorth + Yo * SinBldgRelNorth
      YnoRot=Yo * CosBldgRelNorth - Xo * SinBldgRelNorth
      ! translate
      Xtrans = XnoRot * SQRT(NewAspectRatio/OldAspectRatio)
      Ytrans = YnoRot * SQRT(OldAspectRatio/NewAspectRatio)
      ! rerotate
      SurfaceTmp(SurfNum)%Vertex(N)%X = Xtrans * CosBldgRelNorth - Ytrans * SinBldgRelNorth

      SurfaceTmp(SurfNum)%Vertex(N)%Y = Xtrans * SinBldgRelNorth + Ytrans * CosBldgRelNorth
    ENDDO

  Return

END SUBROUTINE TransformVertsByAspect

SUBROUTINE CalcSurfaceCentroid

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Feb. 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! compute centroid of all the surfaces in the main
          ! surface structure. Store the vertex coordinates of
          ! the centroid in the 'SURFACE' derived type.

          ! METHODOLOGY EMPLOYED:
          ! The centroid of triangle is easily computed by averaging the vertices
          ! The centroid of a quadrilateral is computed by area weighting the centroids
          ! of two triangles.
          ! (Algorithm would need to be changed for higher order
          ! polygons with more than four sides).

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
    USE Vectors
    USE General, ONLY: RoundSigDigits

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
    Type(vector), Dimension(3)   :: Triangle1 !working struct for a 3-sided surface
    Type(vector), Dimension(3)   :: Triangle2 !working struct for a 3-sided surface

    INTEGER   :: thisSurf  ! working variable for do loop
    REAL(r64) :: Tri1Area  ! working variable for denominator
    REAL(r64) :: Tri2Area  ! working variable for denominator
    REAL(r64) :: TotalArea ! working variable for denominator
    REAL(r64) :: Xcm       ! temporary X coord for centriod
    REAL(r64) :: Ycm       ! temporary Y coord for centriod
    REAL(r64) :: Zcm       ! temporary Z coord for centriod
    REAL(r64) :: XcmTri1   ! temporary X coord for centriod of triangle 1
    REAL(r64) :: YcmTri1   ! temporary Y coord for centriod of triangle 1
    REAL(r64) :: ZcmTri1   ! temporary Z coord for centriod of triangle 1
    REAL(r64) :: XcmTri2   ! temporary X coord for centriod of triangle 2
    REAL(r64) :: YcmTri2   ! temporary Y coord for centriod of triangle 2
    REAL(r64) :: ZcmTri2   ! temporary Z coord for centriod of triangle 2
    type(vector) :: VecAvg ! Average (calc for multisided polygons (>4 sides))
    integer   :: vert
    integer   :: negZcount  ! for warning error in surface centroids

    negZcount=0

    ! loop through all the surfaces
    Do thisSurf=1, TotSurfaces

!      IF (Surface(thisSurf)%CLASS == 'INTMASS') CYCLE
      IF (Surface(thisSurf)%CLASS == SurfaceClass_IntMass) CYCLE

      !re-init
      Xcm = 0.0d0
      Ycm = 0.0d0
      Zcm = 0.0d0


      SELECT CASE (surface(thissurf)%sides)  !is this a 3- or 4-sided surface

      CASE (3) !3-sided polygon
        ! centriod is simple average
        Xcm = sum(surface(thisSurf)%vertex%x) / 3.0d0
        Ycm = sum(surface(thisSurf)%vertex%y) / 3.0d0
        Zcm = sum(surface(thisSurf)%vertex%z) / 3.0d0

      CASE (4) !4-sided polygon

        ! re-init
        Triangle1%X = 0.0d0
        Triangle1%Y = 0.0d0
        Triangle1%Z = 0.0d0

        Triangle2%X = 0.0d0
        Triangle2%Y = 0.0d0
        Triangle2%Z = 0.0d0

        XcmTri1     = 0.0d0
        YcmTri1     = 0.0d0
        ZcmTri1     = 0.0d0

        XcmTri2     = 0.0d0
        YcmTri2     = 0.0d0
        ZcmTri2     = 0.0d0

        Tri1Area    = 0.0d0
        Tri2Area    = 0.0d0

        ! split into 2 3-sided polygons (Triangle 1 and Triangle 2)
        Triangle1%X = surface(thisSurf)%vertex( (/1,2,3/) )%X
        Triangle1%Y = surface(thisSurf)%vertex( (/1,2,3/) )%Y
        Triangle1%Z = surface(thisSurf)%vertex( (/1,2,3/) )%Z

        Triangle2%X = surface(thisSurf)%vertex( (/1,3,4/) )%X
        Triangle2%Y = surface(thisSurf)%vertex( (/1,3,4/) )%Y
        Triangle2%Z = surface(thisSurf)%vertex( (/1,3,4/) )%Z

        ! get area of triangles.
        Tri1Area = AreaPolygon(3,Triangle1)
        Tri2Area = AreaPolygon(3,Triangle2)

        ! get total Area of quad.
        TotalArea = surface(thisSurf)%grossarea

        IF (TotalArea <= 0.0d0 ) then
           !catch a problem....
            CALL ShowWarningError('CalcSurfaceCentroid: zero area surface, for surface='//TRIM(surface(thisSurf)%Name))
            cycle
        endif
        ! get centroid of Triangle 1
        XcmTri1 = sum(Triangle1%x) / 3.0d0
        YcmTri1 = sum(Triangle1%y) / 3.0d0
        ZcmTri1 = sum(Triangle1%z) / 3.0d0

        ! get centroid of Triangle 2
        XcmTri2 = sum(Triangle2%x) / 3.0d0
        YcmTri2 = sum(Triangle2%y) / 3.0d0
        ZcmTri2 = sum(Triangle2%z) / 3.0d0

        ! find area weighted combination of the two centroids.

        Xcm = (XcmTri1*Tri1Area + XcmTri2*Tri2Area) / TotalArea
        Ycm = (YcmTri1*Tri1Area + YcmTri2*Tri2Area) / TotalArea
        Zcm = (ZcmTri1*Tri1Area + ZcmTri2*Tri2Area) / TotalArea

      CASE (5:) !multi-sided polygon
         ! (Maybe triangulate?  For now, use old "z" average method")
         ! and X and Y -- straight average

!        X1=MINVAL(Surface(thisSurf)%Vertex(1:Surface(thisSurf)%Sides)%X)
!        X2=MAXVAL(Surface(thisSurf)%Vertex(1:Surface(thisSurf)%Sides)%X)
!        Y1=MINVAL(Surface(thisSurf)%Vertex(1:Surface(thisSurf)%Sides)%Y)
!        Y2=MAXVAL(Surface(thisSurf)%Vertex(1:Surface(thisSurf)%Sides)%Y)
!        Z1=MINVAL(Surface(thisSurf)%Vertex(1:Surface(thisSurf)%Sides)%Z)
!        Z2=MAXVAL(Surface(thisSurf)%Vertex(1:Surface(thisSurf)%Sides)%Z)
!        Xcm=(X1+X2)/2.0d0
!        Ycm=(Y1+Y2)/2.0d0
!        Zcm=(Z1+Z2)/2.0d0

        ! Calc centroid as average of surfaces
        VecAvg=vector(0.0d0,0.0d0,0.0d0)

        do vert=1,Surface(thisSurf)%Sides
          VecAvg=VecAvg + Surface(thisSurf)%Vertex(vert)
        enddo
        VecAvg=VecAvg/REAL(Surface(thisSurf)%Sides,r64)
        Xcm=VecAvg%x
        Ycm=VecAvg%y
        Zcm=VecAvg%z

      CASE DEFAULT

      IF (Surface(thisSurf)%Name /= Blank) THEN
          CALL ShowWarningError('CalcSurfaceCentroid: caught problem with # of sides, for surface='//TRIM(surface(thisSurf)%Name))
          CALL ShowContinueError('... number of sides must be >= 3, this surface # sides='//  &
                           TRIM(RoundSigDigits(surface(thisSurf)%Sides)))
      ELSE
          CALL ShowWarningError('CalcSurfaceCentroid: caught problem with # of sides, for surface=#'//  &
             TRIM(RoundSigDigits(thisSurf)))
          CALL ShowContinueError('...surface name is blank. Examine surfaces -- '//  &
             'this may be a problem with ill-formed interzone surfaces.')
          CALL ShowContinueError('... number of sides must be >= 3, this surface # sides='//  &
                           TRIM(RoundSigDigits(surface(thisSurf)%Sides)))
      ENDIF

      END SELECT

      ! store result in the surface structure in DataSurfaces
      Surface(thissurf)%centroid%x = Xcm
      Surface(thissurf)%centroid%y = Ycm
      Surface(thissurf)%centroid%z = Zcm

      if (Zcm < 0.0d0) then
        if (Surface(thisSurf)%ExtWind .or. Surface(thisSurf)%ExtBoundCond == ExternalEnvironment) negZcount=negZcount+1
      endif

    ENDDO  !loop through surfaces

    if (negZcount > 0) then
      CALL ShowWarningError('CalcSurfaceCentroid: '//TRIM(RoundSigDigits(negZcount))//  &
              ' Surfaces have the Z coordinate < 0.')
      CALL ShowContinueError('...in any calculations, Wind Speed will be 0.0 for these surfaces.')
      CALL ShowContinueError('...in any calculations, Outside temperatures will be the '//   &
         'outside temperature + '//TRIM(RoundSigDigits(WeatherFileTempModCoeff,3))//' for these surfaces.')
      CALL ShowContinueError('...that is, these surfaces will have conditions as though at ground level.')
    endif

    RETURN

END SUBROUTINE CalcSurfaceCentroid

SUBROUTINE SetupShadeSurfacesForSolarCalcs
          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Dec. 2008
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! determine if Shading surfaces need full solar calcs because they
          ! are also used to define geometry of an active solar component.
          ! Normally, a shading surface is not included in calculations of incident solar, only shading
          !

          ! METHODOLOGY EMPLOYED:
          ! Mine solar renewables input and collect surface names.
          ! find shading surfaces with names that match those in solar objects.
          ! setup flags for shading surfaces so that the solar renewables can resuse incident solar calcs
          ! new solar component models that use shading surfaces will have to extend the code here.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound, GetObjectItem, FindItemInList, SameString
  USE DataIPShortCuts
  USE DataHeatBalance, ONLY:

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
  CHARACTER(len=MaxNameLength),  ALLOCATABLE, DIMENSION(:) :: TmpCandidateSurfaceNames
  CHARACTER(len=MaxNameLength),  ALLOCATABLE, DIMENSION(:) :: TmpCandidateICSSurfaceNames
  CHARACTER(len=MaxNameLength),  ALLOCATABLE, DIMENSION(:) :: TmpCandidateICSBCTypeNames
  INTEGER :: NumCandidateNames
  INTEGER :: NumOfCollectors
  INTEGER :: NumOfICSUnits
  INTEGER :: NumOfFlatPlateUnits
  INTEGER :: NumPVTs
  INTEGER :: NumPVs
  INTEGER :: SurfNum
  INTEGER :: found
  INTEGER :: CollectorNum
  INTEGER :: PVTNum
  INTEGER :: PVNum
  INTEGER :: NumAlphas ! Number of alpha names being passed
  INTEGER :: NumNumbers   ! Number of numeric parameters being passed
  INTEGER :: IOStatus

  !First collect names of surfaces referenced by active solar components
  cCurrentModuleObject = 'SolarCollector:FlatPlate:Water'
  NumOfFlatPlateUnits = GetNumObjectsFound(cCurrentModuleObject)
  cCurrentModuleObject = 'SolarCollector:FlatPlate:PhotovoltaicThermal'
  NumPVTs              = GetNumObjectsFound(cCurrentModuleObject)
  cCurrentModuleObject = 'Generator:Photovoltaic'
  NumPVs                 = GetNumObjectsFound(cCurrentModuleObject )
  cCurrentModuleObject = 'SolarCollector:IntegralCollectorStorage'
  NumOfICSUnits = GetNumObjectsFound(cCurrentModuleObject)

  NumCandidateNames = NumOfFlatPlateUnits + NumPVTs + NumPVs + NumOfICSUnits
  NumOfCollectors = NumOfFlatPlateUnits + NumOfICSUnits

  ALLOCATE(TmpCandidateSurfaceNames(NumCandidateNames), TmpCandidateICSSurfaceNames(NumOfCollectors), &
           TmpCandidateICSBCTypeNames(NumOfCollectors))

  IF (NumOfCollectors > 0) THEN
    cCurrentModuleObject = 'SolarCollector:FlatPlate:Water'
    DO CollectorNum = 1, NumOfFlatPlateUnits

      CALL GetObjectItem(cCurrentModuleObject,CollectorNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus)

      TmpCandidateSurfaceNames(CollectorNum) = cAlphaArgs(3)
      TmpCandidateICSBCTypeNames(CollectorNum) = ' '
    ENDDO
  ENDIF

  IF (NumPVTs > 0) THEN
    cCurrentModuleObject = 'SolarCollector:FlatPlate:PhotovoltaicThermal'
    DO PVTNum = 1, NumPVTs

      CALL GetObjectItem(cCurrentModuleObject,PVTNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus)

      TmpCandidateSurfaceNames(NumOfFlatPlateUnits + PVTNum) = cAlphaArgs(2)
    ENDDO
  ENDIF

  IF (NumPVs > 0) THEN
    cCurrentModuleObject = 'Generator:Photovoltaic'
    DO PVNum = 1, NumPVs
      CALL GetObjectItem(cCurrentModuleObject,PVNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus)
      TmpCandidateSurfaceNames(NumOfFlatPlateUnits + NumPVTs + PVNum) = cAlphaArgs(2)
    ENDDO
  ENDIF

  IF (NumOfICSUnits > 0) THEN
    cCurrentModuleObject = 'SolarCollector:IntegralCollectorStorage'
    DO CollectorNum = 1, NumOfICSUnits
      CALL GetObjectItem(cCurrentModuleObject,CollectorNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus)
      TmpCandidateSurfaceNames(NumOfFlatPlateUnits + NumPVTs + NumPVs + CollectorNum) = cAlphaArgs(3)
      TmpCandidateICSSurfaceNames(NumOfFlatPlateUnits + CollectorNum) = cAlphaArgs(3)
      TmpCandidateICSBCTypeNames(NumOfFlatPlateUnits + CollectorNum) = cAlphaArgs(4)
    ENDDO
  ENDIF

  ! loop through all the surfaces
  Do SurfNum=1, TotSurfaces

    Found = FindItemInList(Surface(SurfNum)%name, TmpCandidateSurfaceNames, NumCandidateNames)
    IF (Found > 0) Then
      IF (.NOT. Surface(SurfNum)%HeatTransSurf ) Then ! not BIPV, must be a shading surf with solar device
        ! Setup missing values to allow shading surfaces to model incident solar and wind
        Surface(SurfNum)%ExtSolar = .TRUE.
        Surface(SurfNum)%ExtWind = .TRUE.
        Surface(SurfNum)%ViewFactorGround = 0.5d0 * (1.0d0 - Surface(SurfNum)%CosTilt)

      ENDIF
      ! check if this surface is used for ICS collector mounting and has OthersideCondictionsModel as its
      ! boundary condition
      IF (NumOfICSUnits > 0) Then
        DO CollectorNum = 1, NumOfCollectors
          IF ( SameString(Surface(SurfNum)%name, TmpCandidateICSSurfaceNames(CollectorNum)) .AND. &
               SameString(TmpCandidateICSBCTypeNames(CollectorNum), 'OTHERSIDECONDITIONSMODEL') ) THEN
               Surface(SurfNum)%IsICS = .TRUE.
               Surface(SurfNum)%ICSPtr = CollectorNum
          ENDIF

        END DO
      ENDIF

    ENDIF   ! end of IF (Found > 0) Then

  ENDDO

  DEALLOCATE(TmpCandidateSurfaceNames,TmpCandidateICSBCTypeNames,TmpCandidateICSSurfaceNames)
  RETURN

END SUBROUTINE SetupShadeSurfacesForSolarCalcs

SUBROUTINE CheckConvexity(SurfNum,NSides)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Tyler Hoyt
          !       DATE WRITTEN   December 2010
          !       MODIFIED       CR8752 - incorrect note of non-convex polygons
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE: This subroutine verifies the convexity of a
          ! surface that is exposed to the sun in the case that full shading calculations
          ! are required. The calculation conveniently detects collinear points as well,
          ! and returns a list of indices that are collinear within the plane of the surface.

          ! METHODOLOGY EMPLOYED: First the surface is determined to have dimension 2 in
          ! either the xy, yz, or xz plane. That plane is selected to do the testing.
          ! Vectors representing the edges of the polygon and the perpendicular dot product
          ! between adjacent edges are computed. This allows the turning angle to be determined.
          ! If the turning angle is greater than pi/2, it turns to the right, and if it is
          ! less than pi/2, it turns left. The direction of the turn is stored, and if it
          ! changes as the edges are iterated the surface is not convex. Meanwhile it stores
          ! the indices of vertices that are collinear and are later removed.

          ! REFERENCES:
          ! http://mathworld.wolfram.com/ConvexPolygon.html

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits
  USE DataErrorTracking

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)  :: SurfNum     ! Current surface number
  INTEGER, INTENT(IN)  :: NSides      ! Number of sides to figure

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64),PARAMETER :: TurnThreshold = 0.000001d0 ! Sensitivity of convexity test, in radians
  CHARACTER(len=*), PARAMETER :: ErrFmt="(' (',F8.3,',',F8.3,',',F8.3,')')"

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER   :: N             ! Loop index
  INTEGER   :: Np1             ! Loop index
  INTEGER   :: Np2             ! Loop index
  REAL(r64) :: Det           ! Determinant for picking projection plane
  REAL(r64) :: DotProd       ! Dot product for determining angle
  REAL(r64) :: Theta         ! Angle between edge vectors
  REAL(r64) :: LastTheta     ! Angle between edge vectors
  REAL(r64) :: V1len         ! Edge vector length
  REAL(r64) :: V2len         ! Edge vector length
  LOGICAL   :: SignFlag      ! Direction of edge turn : .true. is right, .false. is left
  LOGICAL   :: PrevSignFlag  ! Container for the sign of the previous iteration's edge turn
  LOGICAL   :: FirstTimeFlag ! Flag indicating first iteration
  REAL(r64), ALLOCATABLE, SAVE, DIMENSION(:) :: X ! containers for x,y,z vertices of the surface
  REAL(r64), ALLOCATABLE, SAVE, DIMENSION(:) :: Y
  REAL(r64), ALLOCATABLE, SAVE, DIMENSION(:) :: Z
  REAL(r64), ALLOCATABLE, SAVE, DIMENSION(:) :: A    ! containers for convexity test
  REAL(r64), ALLOCATABLE, SAVE, DIMENSION(:) :: B
  INTEGER, ALLOCATABLE, SAVE, DIMENSION(:) :: SurfCollinearVerts ! Array containing indices of collinear vertices
  INTEGER, SAVE :: VertSize  ! size of X,Y,Z,A,B arrays
  REAL(r64) :: cosarg
  INTEGER   :: M             ! Array index for SurfCollinearVerts container
  INTEGER   :: J             ! Loop index
  INTEGER   :: K             ! Loop index
  INTEGER   :: Ind           ! Location of surface vertex to be removed
  LOGICAL, SAVE :: firstTime=.true.
  REAL(r64), SAVE :: ACosZero  ! set on firstTime
  LOGICAL :: SurfCollinearWarning
  CHARACTER(len=132) :: ErrLineOut=Blank ! Character string for producing error messages

  if (firstTime) then
    ACosZero=ACOS(0.0d0)
    ALLOCATE(X(MaxVerticesPerSurface+2))
    ALLOCATE(Y(MaxVerticesPerSurface+2))
    ALLOCATE(Z(MaxVerticesPerSurface+2))
    ALLOCATE(A(MaxVerticesPerSurface+2))
    ALLOCATE(B(MaxVerticesPerSurface+2))
    ALLOCATE(SurfCollinearVerts(MaxVerticesPerSurface))
    VertSize=MaxVerticesPerSurface
    firstTime=.false.
  endif

  if (NSides > VertSize) then
    DEALLOCATE(X)
    DEALLOCATE(Y)
    DEALLOCATE(Z)
    DEALLOCATE(A)
    DEALLOCATE(B)
    DEALLOCATE(SurfCollinearVerts)
    ALLOCATE(X(MaxVerticesPerSurface+2))
    ALLOCATE(Y(MaxVerticesPerSurface+2))
    ALLOCATE(Z(MaxVerticesPerSurface+2))
    ALLOCATE(A(MaxVerticesPerSurface+2))
    ALLOCATE(B(MaxVerticesPerSurface+2))
    ALLOCATE(SurfCollinearVerts(MaxVerticesPerSurface))
    VertSize=MaxVerticesPerSurface
  endif

  DO N = 1, NSides
    X(N) = SurfaceTmp(SurfNum)%Vertex(N)%X
    Y(N) = SurfaceTmp(SurfNum)%Vertex(N)%Y
    Z(N) = SurfaceTmp(SurfNum)%Vertex(N)%Z
  END DO
  X(Nsides+1) = SurfaceTmp(SurfNum)%Vertex(1)%X
  Y(Nsides+1) = SurfaceTmp(SurfNum)%Vertex(1)%Y
  Z(Nsides+1) = SurfaceTmp(SurfNum)%Vertex(1)%Z
  X(Nsides+2) = SurfaceTmp(SurfNum)%Vertex(2)%X
  Y(Nsides+2) = SurfaceTmp(SurfNum)%Vertex(2)%Y
  Z(Nsides+2) = SurfaceTmp(SurfNum)%Vertex(2)%Z


  ! Determine a suitable plane in which to do the tests
  Det = 0.0d0
  DO N = 1, NSides
    Det = Det + X(N)*Y(N+1) - X(N+1)*Y(N)
  END DO
  IF (ABS(Det) > 1.d-4) THEN
    A = X
    B = Y
  ELSE
    Det = 0.0d0
    DO N = 1, NSides
      Det = Det + X(N)*Z(N+1) - X(N+1)*Z(N)
    END DO
    IF (ABS(Det) > 1.d-4) THEN
      A = X
      B = Z
    ELSE
      Det = 0.0d0
      DO N = 1, NSides
        Det = Det + Y(N)*Z(N+1) - Y(N+1)*Z(N)
      END DO
      IF (ABS(Det) > 1.d-4) THEN
        A = Y
        B = Z
      ELSE
        ! This condition should not be reached if the surfaces are guaranteed to be planar already
        CALL ShowSevereError('CheckConvexity: Surface="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
          '" is non-planar.')
        CALL ShowContinueError('Coincident Vertices will be removed as possible.')
        DO N=1,SurfaceTmp(SurfNum)%Sides
          WRITE(ErrLineOut,ErrFmt) SurfaceTmp(SurfNum)%Vertex(N)
          CALL ShowContinueError(ErrLineOut)
        ENDDO
      END IF
    END IF
  END IF

  M = 0
  FirstTimeFlag = .true.
  SurfCollinearWarning=.false.
  DO N = 1, NSides   ! perform convexity test in the plane determined above.
    DotProd = (A(N+1)-A(N))*(B(N+2)-B(N+1))-(B(N+1)-B(N))*(A(N+2)-A(N+1))
    V1len = SQRT((A(N+1)-A(N))**2+(B(N+1)-B(N))**2)
    V2len = SQRT((A(N+2)-A(N+1))**2+(B(N+2)-B(N+1))**2)
    IF (V1Len <= 1.d-8 .or. V2Len <= 1.d-8) CYCLE
    cosarg=DotProd/(V1len * V2len)
    if (cosarg < -1.0d0) then
      cosarg=-1.0d0
    elseif (cosarg > 1.0d0) then
      cosarg=1.0d0
    endif
    Theta   = ACOS(cosarg)
    IF (Theta < (ACosZero-TurnThreshold)) THEN
      SignFlag = .true.
    ELSE
      IF (Theta > (ACosZero+TurnThreshold)) THEN
        SignFlag = .false.
      ELSE   ! Store the index of the collinear vertex for removal
        IF (.not. SurfCollinearWarning) THEN
          IF (DisplayExtraWarnings) THEN
            CALL ShowWarningError('CheckConvexity: Surface="'//TRIM(SurfaceTmp(SurfNum)%Name)//  &
               '", Collinear points have been removed.')
          ENDIF
          SurfCollinearWarning=.true.
        ENDIF
        TotalCoincidentVertices=TotalCoincidentVertices+1
        M = M + 1
        SurfCollinearVerts(M) = N + 1
        CYCLE
      END IF
    END IF

    IF (FirstTimeFlag) THEN
      PrevSignFlag = SignFlag
      FirstTimeFlag = .false.
      CYCLE
    END IF

    IF (SignFlag .neqv. PrevSignFlag) THEN
      IF (SolarDistribution /= MinimalShadowing .and. SurfaceTmp(SurfNum)%ExtSolar) THEN
        IF (DisplayExtraWarnings) THEN
          CALL ShowWarningError('CheckConvexity: Zone="'//trim(Zone(SurfaceTmp(SurfNum)%Zone)%Name)//  &
             '", Surface="'//TRIM(SurfaceTmp(SurfNum)%Name)//'" is non-convex.')
          Np1=N+1
          IF (Np1 > NSides) Np1=Np1-NSides
          Np2=N+2
          IF (Np2 > NSides) Np2=Np2-NSides
          CALL ShowContinueError('...vertex '//trim(RoundSigDigits(N))//' to vertex '//trim(RoundSigDigits(Np1))//  &
             ' to vertex '//trim(RoundSigDigits(Np2)))
          CALL ShowContinueError('...vertex '//trim(RoundSigDigits(N))//'=['//  &
             trim(RoundSigDigits(X(N),2))//','//  &
             trim(RoundSigDigits(Y(N),2))//','//  &
             trim(RoundSigDigits(Z(N),2))//']')
          CALL ShowContinueError('...vertex '//trim(RoundSigDigits(Np1))//'=['//  &
             trim(RoundSigDigits(X(N+1),2))//','//  &
             trim(RoundSigDigits(Y(N+1),2))//','//  &
             trim(RoundSigDigits(Z(N+1),2))//']')
          CALL ShowContinueError('...vertex '//trim(RoundSigDigits(Np2))//'=['//  &
             trim(RoundSigDigits(X(N+2),2))//','//  &
             trim(RoundSigDigits(Y(N+2),2))//','//  &
             trim(RoundSigDigits(Z(N+2),2))//']')
!          CALL ShowContinueError('...theta angle=['//trim(RoundSigDigits(Theta,6))//']')
!          CALL ShowContinueError('...last theta angle=['//trim(RoundSigDigits(LastTheta,6))//']')
        ENDIF
        SurfaceTmp(SurfNum)%IsConvex=.false.
        EXIT
      END IF
    END IF
    PrevSignFlag = SignFlag
    LastTheta=Theta
  END DO

  ! must check to make sure don't remove NSides below 3
  IF (M > 0) THEN ! Remove the collinear points determined above
    IF (NSides-M > 2) THEN
      SurfaceTmp(SurfNum)%Sides = NSides - M
    ELSE  ! too many
      IF (DisplayExtraWarnings) THEN
        CALL ShowWarningError('CheckConvexity: Surface="'//TRIM(SurfaceTmp(SurfNum)%Name)//'" has ['//  &
             trim(RoundSigDigits(M))//'] collinear points.')
        CALL ShowContinueError('...too many to remove all.  Will leave the surface with 3 sides. '//  &
           'But this is now a degenerate surface')
      ENDIF
      TotalDegenerateSurfaces=TotalDegenerateSurfaces+1
      SurfaceTmp(SurfNum)%Sides = MAX(NSides-M,3)
      M=NSides-SurfaceTmp(SurfNum)%Sides
    ENDIF
    DO J = 1, M
      Ind = SurfCollinearVerts(J)
      DO K = Ind, NSides - J
        SurfaceTmp(SurfNum)%Vertex(K-J+1)%X = SurfaceTmp(SurfNum)%Vertex(K-J+2)%X
        SurfaceTmp(SurfNum)%Vertex(K-J+1)%Y = SurfaceTmp(SurfNum)%Vertex(K-J+2)%Y
        SurfaceTmp(SurfNum)%Vertex(K-J+1)%Z = SurfaceTmp(SurfNum)%Vertex(K-J+2)%Z
      END DO
    END DO
  END IF


END SUBROUTINE CheckConvexity

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

END MODULE SurfaceGeometry


