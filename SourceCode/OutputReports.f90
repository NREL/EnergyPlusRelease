SUBROUTINE ReportSurfaces

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   February 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine calls several optional routines to report
          ! the surfaces to output formats that can render the data
          ! into a descriptive picture.

          ! METHODOLOGY EMPLOYED:
          ! Use a REPORT command to determine if there should be
          ! a file created.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataGlobals, ONLY: MaxNameLength
  USE DataInterfaces, ONLY: ShowWarningError
  USE DataErrorTracking, ONLY: AskForSurfacesReport
  USE DataSurfaceColors
  USE General, ONLY: ScanForReports

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
          ! na

          ! SUBROUTINE PARAMETER DEFINITIONS:

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER SurfDetails
  LOGICAL SurfVert
  LOGICAL SurfDet
  LOGICAL DXFDone
  LOGICAL VRMLDone
  CHARACTER(len=MaxNameLength) :: Option1
  CHARACTER(len=MaxNameLength) :: Option2
  LOGICAL DoReport

  AskForSurfacesReport=.false.

  SurfDetails=0
  SurfVert=.false.
  SurfDet=.false.
  DXFDone=.false.
  VRMLDone=.false.
  Option1=' '
  Option2=' '

  CALL ScanForReports('Surfaces',DoReport,'Lines',Option1)
  IF (DoReport) CALL LinesOut(Option1)

  CALL ScanForReports('Surfaces',DoReport,'Vertices')
  IF (DoReport) THEN
    IF (.not. SurfVert) THEN
      SurfDetails=SurfDetails+1
      SurfVert=.true.
    ENDIF
  ENDIF

  CALL ScanForReports('Surfaces',DoReport,'Details')
  IF (DoReport) THEN
    IF (.not. SurfDet) THEN
      SurfDetails=SurfDetails+10
      SurfDet=.true.
    ENDIF
  ENDIF

  CALL ScanForReports('Surfaces',DoReport,'DetailsWithVertices')
  IF (DoReport) THEN
    IF (.not. SurfDet) THEN
      SurfDetails=SurfDetails+10
      SurfDet=.true.
    ENDIF
    IF (.not. SurfVert) THEN
      SurfDetails=SurfDetails+1
      SurfVert=.true.
    ENDIF
  ENDIF

  CALL ScanForReports('Surfaces',DoReport,'DXF',Option1=Option1,Option2=Option2)
  IF (DoReport) THEN
    IF (.not. DXFDone) THEN
      IF (Option2 /= ' ') THEN
        CALL SetUpSchemeColors(Option2,'DXF')
      ENDIF
      CALL DXFOut(Option1,Option2)
      DXFDone=.true.
    ELSE
      CALL ShowWarningError('ReportSurfaces: DXF output already generated.  DXF with option=['//TRIM(Option1)//  &
                   '] will not be generated.')
    ENDIF
  ENDIF

  CALL ScanForReports('Surfaces',DoReport,'DXF:WireFrame',Option1=Option1,Option2=Option2)
  IF (DoReport) THEN
    IF (.not. DXFDone) THEN
      IF (Option2 /= ' ') THEN
        CALL SetUpSchemeColors(Option2,'DXF')
      ENDIF
      CALL DXFOutWireFrame(Option2)
      DXFDone=.true.
    ELSE
        CALL ShowWarningError('ReportSurfaces: DXF output already generated.  DXF:WireFrame will not be generated.')
    ENDIF
  ENDIF

  CALL ScanForReports('Surfaces',DoReport,'VRML',Option1,Option2)
  IF (DoReport) THEN
    IF (.not. VRMLDone) THEN
      CALL VRMLOut(Option1,Option2)
      VRMLDone=.true.
    ELSE
      CALL ShowWarningError('ReportSurfaces: VRML output already generated.  VRML with option=['//TRIM(Option1)//  &
                    '] will not be generated.')
    ENDIF
  ENDIF

  CALL ScanForReports('Surfaces',DoReport,'CostInfo')
  IF (DoReport) THEN
    CALL CostInfoOut
  ENDIF

  IF (SurfDet .or. SurfVert) THEN
    CALL DetailsForSurfaces(SurfDetails)
  ENDIF

 RETURN

END SUBROUTINE ReportSurfaces

SUBROUTINE LinesOut(option)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 1999
          !       MODIFIED       March 2006 -- add option for "IDF segments out"
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine produces a file of lines in the surfaces.

          ! METHODOLOGY EMPLOYED:
          ! Use the surface absolute coordinate information to produce
          ! lines.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataHeatBalance
  USE DataSurfaces
  USE DataInterfaces, ONLY: ShowWarningError, ShowContinueError, ShowFatalError
  USE General, ONLY: RoundSigDigits


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  character(len=*), intent(IN) :: option

          ! SUBROUTINE PARAMETER DEFINITIONS:
  character(len=*), parameter :: fmt700="(5(f10.2,','),f10.2)"
  character(len=*), parameter :: fmta="(A)"
  character(len=*), parameter :: fmtcoord="(2X,2(f10.2,','),f10.2,A,A)"
  character(len=*), parameter :: vertexstring='X,Y,Z ==> Vertex'

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  logical, save :: optiondone=.false.
  character(len=MaxNameLength), SAVE :: lastoption=' '
  integer unit              ! Unit number on which to write file
  integer surf              ! Loop variable for surfaces
  integer vert              ! Loop counter
  integer,external :: getnewunitnumber  ! External function for a new unit number
  character(len=1) :: optcommasemi
  integer :: write_stat

  if (totsurfaces > 0 .and. .not. allocated(surface)) then
    ! no error needed, probably in end processing, just return
    return
  endif

  if (optiondone) then
    CALL ShowWarningError('Report of Surfaces/Lines Option has already been completed with option='//trim(lastoption))
    CALL ShowContinueError('..option="'//trim(Option)//'" will not be done this time.')
    return
  endif

  lastoption=option
  optiondone=.true.

  unit=getnewunitnumber()
  open(unit,file='eplusout.sln', Action='write', iostat=write_stat)
  if (write_stat /= 0) then
   CALL ShowFatalError('LinesOut: Could not open file "eplusout.sln" for output (write).')
  endif

  if (option /= 'IDF') then
    do surf=1,totsurfaces
      if (surface(surf)%class .eq. SurfaceClass_IntMass) CYCLE
      if (surface(surf)%sides == 0) CYCLE
      write(unit,fmta) trim(surface(surf)%ZoneName)//':'//trim(surface(surf)%Name)
      do vert=1,Surface(Surf)%Sides
        if (vert /= Surface(Surf)%Sides) then
          write(unit,fmt700) surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y,surface(surf)%vertex(vert)%z, &
            surface(surf)%vertex(vert+1)%x,surface(surf)%vertex(vert+1)%y,surface(surf)%vertex(vert+1)%z
        else
          write(unit,fmt700) surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y,surface(surf)%vertex(vert)%z, &
            surface(surf)%vertex(1)%x,surface(surf)%vertex(1)%y,surface(surf)%vertex(1)%z
        endif
      enddo
    enddo
  else
    write(unit,fmta) ' Building North Axis = 0'
    write(unit,fmta) 'GlobalGeometryRules,UpperLeftCorner,CounterClockwise,WorldCoordinates;'
    do surf=1,totsurfaces
      if (surface(surf)%class .eq. SurfaceClass_IntMass) CYCLE
      if (surface(surf)%sides == 0) CYCLE
      ! process heat transfer surfaces
      write(unit,fmta) ' Surface='//trim(cSurfaceClass(surface(surf)%class))//', Name='//trim(surface(surf)%Name)// &
        ', Azimuth='//trim(roundsigdigits(surface(surf)%Azimuth,1))
      write(unit,fmta) '  '//trim(roundsigdigits(surface(surf)%Sides))//',  !- Number of (X,Y,Z) groups in this surface'
      do vert=1,Surface(Surf)%Sides
        optcommasemi=','
        if (vert == Surface(Surf)%Sides) optcommasemi=';'
        write(unit,fmtcoord) surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y,surface(surf)%vertex(vert)%z,  &
           optcommasemi,'  !- '//trim(vertexstring)//' '//trim(roundsigdigits(vert))
      enddo
    enddo
  endif

  close(unit)

  return

END SUBROUTINE LinesOut

SUBROUTINE DXFOut(PolygonAction,ColorScheme)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   March 1999
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine produces a file of DXF objects for the surfaces.

          ! METHODOLOGY EMPLOYED:
          ! Use the surface absolute coordinate information to produce
          ! lines.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataHeatBalance, ONLY: BuildingName,Zone
  USE DataSurfaces
  USE DataSurfaceColors
  USE DataDaylighting, ONLY: ZoneDaylight,TotIllumMaps,IllumMapCalc
  USE DataGlobals, ONLY: DegToRadians,NumOfZones
  USE DataInterfaces, ONLY: ShowWarningError, ShowContinueError, ShowFatalError
  USE DataStringGlobals, ONLY: VerString
  USE DXFEarClipping
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) :: PolygonAction
  CHARACTER(len=*) :: ColorScheme    ! Name from user for color scheme or blank

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64), dimension(4) :: StemX =(/-10.d0,-10.d0,-10.d0,-10.d0/)
  REAL(r64), dimension(4) :: StemY =(/3.d0,3.d0,0.d0,0.d0/)
  REAL(r64), dimension(4) :: StemZ =(/.1d0,0.d0,0.d0,.1d0/)
  REAL(r64), dimension(4) :: Head1X =(/-10.d0,-10.d0,-10.5d0,-10.5d0/)
  REAL(r64), dimension(4) :: Head1Y =(/3.d0,3.d0,2.133975d0,2.133975d0/)
  REAL(r64), dimension(4) :: Head1Z =(/.1d0,0.d0,0.d0,.1d0/)
  REAL(r64), dimension(4) :: Head2X =(/-10.d0,-10.d0,-9.5d0,-9.5d0/)
  REAL(r64), dimension(4) :: Head2Y =(/3.d0,3.d0,2.133975d0,2.133975d0/)
  REAL(r64), dimension(4) :: Head2Z =(/.1d0,0.d0,0.d0,.1d0/)
  REAL(r64), dimension(4) :: NSide1X =(/-10.5d0,-10.5d0,-10.5d0,-10.5d0/)
  REAL(r64), dimension(4) :: NSide1Y =(/4.5d0,4.5d0,3.5d0,3.5d0/)
  REAL(r64), dimension(4) :: NSide1Z =(/.1d0,0.d0,0.d0,.1d0/)
  REAL(r64), dimension(4) :: NSide2X =(/-10.5d0,-10.5d0,-9.5d0,-9.5d0/)
  REAL(r64), dimension(4) :: NSide2Y =(/4.5d0,4.5d0,3.5d0,3.5d0/)
  REAL(r64), dimension(4) :: NSide2Z =(/.1d0,0.d0,0.d0,.1d0/)
  REAL(r64), dimension(4) :: NSide3X =(/-9.5d0,-9.5d0,-9.5d0,-9.5d0/)
  REAL(r64), dimension(4) :: NSide3Y =(/4.5d0,4.5d0,3.5d0,3.5d0/)
  REAL(r64), dimension(4) :: NSide3Z =(/.1d0,0.d0,0.d0,.1d0/)
!  integer, dimension(7) :: colorno=(/3,4,5,6,2,8,9/)
  integer unit              ! Unit number on which to write file
  integer surf              ! Loop variable for surfaces
  integer vert              ! Loop counter
  integer,external :: getnewunitnumber  ! External function for a new unit number
  integer colorindex        ! color index by surface type
  REAL(r64) minx                 ! minimum x in surface data
  REAL(r64) miny                 ! minimum y in surface data
  REAL(r64) minz                 ! minimum z in surface data (for polygon output)
  integer zones             ! loop counter for zone loop
  character(len=25) zonenum
  character(len=MaxNameLength) TempZoneName
  integer pos
  character(len=25) ShadeType
  logical :: ThickPolyline=.false.
  logical :: RegularPolyline=.false.
  character(len=5) :: PolylineWidth=' 0.55'
  logical :: TriangulateFace=.false.
  integer :: ntri
  integer :: svert
  type (dTriangle), allocatable, dimension(:) :: mytriangles
  integer :: vv0
  integer :: vv1
  integer :: vv2
  integer :: refpt  ! for daylighting ref points
  integer :: curcolorno ! again for daylighting ref pts
  integer :: write_stat
  integer :: mapnum

  IF (PolygonAction == 'TRIANGULATE3DFACE' .or. PolygonAction == 'TRIANGULATE' .or. PolygonAction == ' ') THEN
    TriangulateFace=.true.
    RegularPolyline=.false.
    ThickPolyline=.false.
  ELSEIF (PolygonAction == 'THICKPOLYLINE') THEN
    ThickPolyline=.true.
    RegularPolyline=.false.
    TriangulateFace=.false.
  ELSEIF (PolygonAction == 'REGULARPOLYLINE') THEN
    RegularPolyline=.true.
    TriangulateFace=.false.
    ThickPolyline=.false.
    PolylineWidth=' 0'
  ELSE
    CALL ShowWarningError('DXFOut: Illegal key specified for Surfaces with > 4 sides='//TRIM(PolygonAction))
    CALL ShowContinueError('...Valid keys are: "ThickPolyline", "RegularPolyline", "Triangulate3DFace".')
    CALL ShowContinueError('"Triangulate3DFace" will be used for any surfaces with > 4 sides.')
    TriangulateFace=.true.
    RegularPolyline=.false.
    ThickPolyline=.false.
  ENDIF

  if (totsurfaces > 0 .and. .not. allocated(surface)) then
    ! no error needed, probably in end processing, just return
    return
  endif

  unit=getnewunitnumber()
  open(unit,file='eplusout.dxf', Action='write', iostat=write_stat)
  if (write_stat /= 0) then
   CALL ShowFatalError('DXFOut: Could not open file "eplusout.dxf" for output (write).')
  endif


  write(unit,702)   ! Start of Entities section
  702 format('  0',/,'SECTION',/,'  2',/,'ENTITIES')

  write(unit,707)    ! Comment
  707 format('999',/,'DXF created from EnergyPlus')

  write(unit,708) 'Program Version',',',TRIM(VerString)
  708 format('999',/,A,A,A)

  IF (PolygonAction == ' ') THEN
    write(unit,708) 'Polygon Action',',','ThickPolyline'
  ELSE
    write(unit,708) 'Polygon Action',',',TRIM(PolygonAction)
  ENDIF

  IF (ColorScheme == ' ') THEN
    write(unit,708) 'Color Scheme',',','Default'
  ELSE
    write(unit,708) 'Color Scheme',',',TRIM(ColorScheme)
  ENDIF

  minx=99999.d0
  miny=99999.d0
  do surf=1,totsurfaces
    if (surface(surf)%class == SurfaceClass_IntMass) CYCLE
    do vert=1,surface(surf)%sides
      minx=MIN(minx,surface(surf)%vertex(vert)%x)
      miny=MIN(miny,surface(surf)%vertex(vert)%y)
    enddo
  enddo

  do vert=1,4
    StemX(vert)=StemX(vert)+minx
    StemY(vert)=StemY(vert)+miny
    Head1X(vert)=Head1X(vert)+minx
    Head1Y(vert)=Head1Y(vert)+miny
    Head2X(vert)=Head2X(vert)+minx
    Head2Y(vert)=Head2Y(vert)+miny
    NSide1X(vert)=NSide1X(vert)+minx
    NSide1Y(vert)=NSide1Y(vert)+miny
    NSide2X(vert)=NSide2X(vert)+minx
    NSide2Y(vert)=NSide2Y(vert)+miny
    NSide3X(vert)=NSide3X(vert)+minx
    NSide3Y(vert)=NSide3Y(vert)+miny
  enddo

  ! This writes "True North" above the Arrow Head
  write(unit,710) 'Text - True North'
  write(unit,800) DXFcolorno(colorno_Text),StemX(1)-1.0d0 ,StemY(1),StemZ(1)
  800 format('  0',/,'TEXT',/,'  8',/,'1',/,'  6',/,'Continuous',/,  &
    ' 62',/,I3,/,' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,  &
    ' 40',/,' .25',/,'  1',/,'True North',/,' 41',/,' 0.0',/,'  7',/,'MONOTXT',/,  &
    '210',/,'0.0',/,'220',/,'0.0',/,'230',/,'1.0')

  write(unit,710) 'Text - Building Title'
  write(unit,801) DXFcolorno(colorno_Text),StemX(1)-4.0d0,StemY(1)-4.0d0 ,StemZ(1),'Building - '//TRIM(BuildingName)
  801 format('  0',/,'TEXT',/,'  8',/,'1',/,'  6',/,'Continuous',/,  &
    ' 62',/,I3,/,' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,  &
    ' 40',/,' .4',/,'  1',/,A,/,' 41',/,' 0.0',/,'  7',/,'MONOTXT',/,  &
    '210',/,'0.0',/,'220',/,'0.0',/,'230',/,'1.0')

  ! We want to point the north arrow to true north
  write(unit,710) 'North Arrow Stem'
  write(unit,703) DXFcolorno(colorno_Text),(StemX(vert),StemY(vert),StemZ(vert),vert=1,4)
  write(unit,710) 'North Arrow Head 1'
  write(unit,703) DXFcolorno(colorno_Text),(Head1X(vert),Head1Y(vert),Head1Z(vert),vert=1,4)
  write(unit,710) 'North Arrow Head 2'
  write(unit,703) DXFcolorno(colorno_Text),(Head2X(vert),Head2Y(vert),Head2Z(vert),vert=1,4)
  write(unit,710) 'North Arrow Side 1'
  write(unit,703) DXFcolorno(colorno_Text),(NSide1X(vert),NSide1Y(vert),NSide1Z(vert),vert=1,4)
  write(unit,710) 'North Arrow Side 2'
  write(unit,703) DXFcolorno(colorno_Text),(NSide2X(vert),NSide2Y(vert),NSide2Z(vert),vert=1,4)
  write(unit,710) 'North Arrow Side 3'
  write(unit,703) DXFcolorno(colorno_Text),(NSide3X(vert),NSide3Y(vert),NSide3Z(vert),vert=1,4)

  703 format('  0',/,'3DFACE',/,'  8',/,'1',/,' 62',/,I3,/,  &
    ' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,  &
    ' 11',/,f15.5,/,' 21',/,f15.5,/,' 31',/,f15.5,/,  &
    ' 12',/,f15.5,/,' 22',/,f15.5,/,' 32',/,f15.5,/,  &
    ' 13',/,f15.5,/,' 23',/,f15.5,/,' 33',/,f15.5)

  write(unit,710) 'Zone Names'
  do zones=1,NumOfZones
    write(zonenum,*) zones
    zonenum=adjustl(zonenum)
    TempZoneName=Zone(Zones)%Name
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    END DO
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    END DO
    write(unit,710) 'Zone='//trim(zonenum)//':'//trim(TempZoneName)
  enddo

  colorindex=colorno_ShdDetFix
  !  Do all detached shading surfaces first
  do surf=1,totsurfaces
    if (surface(surf)%heattranssurf) CYCLE
    if (surface(surf)%class == SurfaceClass_Shading) CYCLE
    if (surface(surf)%sides == 0) CYCLE
    if (surface(surf)%class .eq. SurfaceClass_Detached_F) colorindex=colorno_ShdDetFix
    if (surface(surf)%class .eq. SurfaceClass_Detached_B) colorindex=colorno_ShdDetBldg
    if (surface(surf)%isPV) colorindex=colorno_PV
    if (surface(surf)%class .eq. SurfaceClass_Detached_F) then
      ShadeType='Fixed Shading'
      write(unit,710) 'Fixed Shading:'//trim(surface(surf)%Name)
    elseif (surface(surf)%class .eq. SurfaceClass_Detached_B) then
      ShadeType='Building Shading'
      write(unit,710) 'Building Shading:'//trim(surface(surf)%Name)
    endif
    if (surface(surf)%sides <= 4) then
      write(unit,704) TRIM(ShadeType),DXFcolorno(colorindex),(surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y, &
                      surface(surf)%vertex(vert)%z,vert=1,3)
      if (surface(surf)%sides == 3) then
        write(unit,705) surface(surf)%vertex(3)%x,surface(surf)%vertex(3)%y,surface(surf)%vertex(3)%z
      else
        write(unit,705) surface(surf)%vertex(4)%x,surface(surf)%vertex(4)%y,surface(surf)%vertex(4)%z
      endif
    else  ! polygon
      if (.not. TriangulateFace) then
        minz=99999.d0
        do vert=1,surface(surf)%sides
          minz=MIN(minz,surface(surf)%vertex(vert)%z)
        enddo
        write(unit,715) TRIM(ShadeType),DXFcolorno(colorindex),minz,TRIM(PolylineWidth),TRIM(PolylineWidth)
        do vert=1,surface(surf)%sides
          write(unit,716) TRIM(ShadeType),  &
                          surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y,surface(surf)%vertex(vert)%z
        enddo
        write(unit,717) TRIM(ShadeType)
      else
        ntri=triangulate(surface(surf)%sides,surface(surf)%vertex,mytriangles,surface(surf)%azimuth,  &
                   surface(surf)%tilt,surface(surf)%name,surface(surf)%class)
        do svert=1,ntri
            vv0=mytriangles(svert)%vv0
            vv1=mytriangles(svert)%vv1
            vv2=mytriangles(svert)%vv2
          write(unit,704) TRIM(ShadeType),DXFcolorno(colorindex),  &
            surface(surf)%vertex(vv0)%x,surface(surf)%vertex(vv0)%y,surface(surf)%vertex(vv0)%z, &
            surface(surf)%vertex(vv1)%x,surface(surf)%vertex(vv1)%y,surface(surf)%vertex(vv1)%z, &
            surface(surf)%vertex(vv2)%x,surface(surf)%vertex(vv2)%y,surface(surf)%vertex(vv2)%z
          write(unit,705) surface(surf)%vertex(vv2)%x,surface(surf)%vertex(vv2)%y,surface(surf)%vertex(vv2)%z
        enddo
        deallocate(mytriangles)
      endif
    endif
  enddo

  ! now do zone surfaces, by zone
  do zones=1,NumOfZones
    TempZoneName=Zone(Zones)%Name
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    END DO
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    END DO
    do surf=max(zone(zones)%surfacefirst,1),zone(zones)%surfacelast
      if (surface(surf)%sides == 0) CYCLE
      if (surface(surf)%class .eq. SurfaceClass_IntMass) CYCLE
      if (surface(surf)%class .eq. SurfaceClass_Wall) colorindex=colorno_Wall
      if (surface(surf)%class .eq. SurfaceClass_Roof) colorindex=colorno_Roof
      if (surface(surf)%class .eq. SurfaceClass_Floor) colorindex=colorno_Floor
      if (surface(surf)%class .eq. SurfaceClass_Door) colorindex=colorno_Door
      if (surface(surf)%class .eq. SurfaceClass_Window) then
        if (surfacewindow(surf)%originalclass .eq. SurfaceClass_Window) colorindex=colorno_Window
        if (surfacewindow(surf)%originalclass .eq. SurfaceClass_GlassDoor) colorindex=colorno_GlassDoor
        if (surfacewindow(surf)%originalclass .eq. SurfaceClass_TDD_Dome) colorindex=colorno_TDDDome
        if (surfacewindow(surf)%originalclass .eq. SurfaceClass_TDD_Diffuser) colorindex=colorno_TDDDiffuser
      endif
      if (surface(surf)%isPV) colorindex=colorno_PV

      write(unit,710) trim(surface(surf)%ZoneName)//':'//trim(surface(surf)%Name)
      if (surface(surf)%sides <= 4) then
        write(unit,704) TRIM(TempZoneName),DXFcolorno(colorindex),(surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y, &
                        surface(surf)%vertex(vert)%z,vert=1,3)
        if (surface(surf)%sides == 3) then
          write(unit,705) surface(surf)%vertex(3)%x,surface(surf)%vertex(3)%y,surface(surf)%vertex(3)%z
        else
          write(unit,705) surface(surf)%vertex(4)%x,surface(surf)%vertex(4)%y,surface(surf)%vertex(4)%z
        endif
      else  ! polygon surface
        if (.not. TriangulateFace) then
          minz=99999.d0
          do vert=1,surface(surf)%sides
            minz=MIN(minz,surface(surf)%vertex(vert)%z)
          enddo
          write(unit,715) TRIM(TempZoneName),DXFcolorno(colorindex),minz,TRIM(PolylineWidth),TRIM(PolylineWidth)
          do vert=1,surface(surf)%sides
            write(unit,716) TRIM(TempZoneName),  &
                          surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y,surface(surf)%vertex(vert)%z
          enddo
          write(unit,717) TRIM(TempZoneName)
        else
          ntri=triangulate(surface(surf)%sides,surface(surf)%vertex,mytriangles,surface(surf)%azimuth,  &
                   surface(surf)%tilt,surface(surf)%name,surface(surf)%class)
          do svert=1,ntri
            vv0=mytriangles(svert)%vv0
            vv1=mytriangles(svert)%vv1
            vv2=mytriangles(svert)%vv2
            write(unit,704) TRIM(TempZoneName),DXFcolorno(colorindex),  &
              surface(surf)%vertex(vv0)%x,surface(surf)%vertex(vv0)%y,surface(surf)%vertex(vv0)%z, &
              surface(surf)%vertex(vv1)%x,surface(surf)%vertex(vv1)%y,surface(surf)%vertex(vv1)%z, &
              surface(surf)%vertex(vv2)%x,surface(surf)%vertex(vv2)%y,surface(surf)%vertex(vv2)%z
            write(unit,705) surface(surf)%vertex(vv2)%x,surface(surf)%vertex(vv2)%y,surface(surf)%vertex(vv2)%z
          enddo
          deallocate(mytriangles)
        endif
      endif
 715 format('  0',/,'POLYLINE',/,'  8',/,A,/,' 62',/,I3,/,' 66',/,'  1',/,  &
    ' 10',/,' 0.0',/,' 20',/,' 0.0',/,' 30',/,f15.5,/,  &
    ' 70',/,'   9',/,' 40',/,A,/,' 41',/,A)
 716 format('  0',/'VERTEX',/,'  8',/,A,/,  &
    ' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5)
 717 format('  0',/'SEQEND',/,'  8',/,A)

    enddo
    ! still have to do shading surfaces for zone
    do surf=1,totsurfaces
      !if (surface(surf)%heattranssurf) CYCLE ! Shading with a construction is allowed to be HT surf for daylighting shelves
      if (surface(surf)%class .ne. SurfaceClass_Shading) CYCLE
      if (surface(surf)%zonename /= zone(zones)%Name) CYCLE
      if (surface(surf)%sides == 0) CYCLE
      colorindex=colorno_ShdAtt
      if (surface(surf)%isPV) colorindex=colorno_PV
      write(unit,710) trim(surface(surf)%ZoneName)//':'//trim(surface(surf)%Name)
      if (surface(surf)%sides <= 4) then
        write(unit,704) TRIM(TempZoneName),DXFcolorno(colorindex),(surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y, &
                        surface(surf)%vertex(vert)%z,vert=1,3)
        if (surface(surf)%sides == 3) then
          write(unit,705) surface(surf)%vertex(3)%x,surface(surf)%vertex(3)%y,surface(surf)%vertex(3)%z
        else
          write(unit,705) surface(surf)%vertex(4)%x,surface(surf)%vertex(4)%y,surface(surf)%vertex(4)%z
        endif
      else  ! polygon attached shading
        if (.not. TriangulateFace) then
          minz=99999.d0
          do vert=1,surface(surf)%sides
            minz=MIN(minz,surface(surf)%vertex(vert)%z)
          enddo
          write(unit,715) TRIM(TempZoneName),DXFcolorno(colorindex),minz,TRIM(PolylineWidth),TRIM(PolylineWidth)
          do vert=1,surface(surf)%sides
            write(unit,716) TRIM(TempZoneName),  &
                          surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y,surface(surf)%vertex(vert)%z
          enddo
          write(unit,717) TRIM(TempZoneName)
        else
          if (surface(surf)%shape == RectangularOverhang) then
            ntri=triangulate(surface(surf)%sides,surface(surf)%vertex,mytriangles,surface(surf)%azimuth,  &
                   surface(surf)%tilt,surface(surf)%name,SurfaceClass_Overhang)
          else
            ntri=triangulate(surface(surf)%sides,surface(surf)%vertex,mytriangles,surface(surf)%azimuth,  &
                   surface(surf)%tilt,surface(surf)%name,SurfaceClass_Fin)
          endif
          do svert=1,ntri
            vv0=mytriangles(svert)%vv0
            vv1=mytriangles(svert)%vv1
            vv2=mytriangles(svert)%vv2
            write(unit,704) TRIM(TempZoneName),DXFcolorno(colorindex),  &
              surface(surf)%vertex(vv0)%x,surface(surf)%vertex(vv0)%y,surface(surf)%vertex(vv0)%z, &
              surface(surf)%vertex(vv1)%x,surface(surf)%vertex(vv1)%y,surface(surf)%vertex(vv1)%z, &
              surface(surf)%vertex(vv2)%x,surface(surf)%vertex(vv2)%y,surface(surf)%vertex(vv2)%z
            write(unit,705) surface(surf)%vertex(vv2)%x,surface(surf)%vertex(vv2)%y,surface(surf)%vertex(vv2)%z
          enddo
          deallocate(mytriangles)
        endif
      endif
    enddo
  enddo

  704 format('  0',/,'3DFACE',/,'  8',/,A,/,' 62',/,I3,/,  &
    ' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,  &
    ' 11',/,f15.5,/,' 21',/,f15.5,/,' 31',/,f15.5,/,  &
    ' 12',/,f15.5,/,' 22',/,f15.5,/,' 32',/,f15.5)

  705 format(' 13',/,f15.5,/,' 23',/,f15.5,/,' 33',/,f15.5)

!  711 format('  0',/,'LINE',/,'  8',/,A,/,' 62',/,I3)
!  712 format(' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,  &
!             ' 11',/,f15.5,/,' 21',/,f15.5,/,' 31',/,f15.5)



  ! Do any daylighting reference points on layer for zone
  do zones=1,numofzones
    curcolorno=colorno_DaylSensor1
    TempZoneName=Zone(Zones)%Name
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    END DO
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    END DO
    do refpt=1,zonedaylight(zones)%TotalDaylRefPoints
      write(unit,710) trim(zone(zones)%Name)//':DayRefPt:'//TRIM(TrimSigDigits(refpt))
      write(unit,709) trim(TempZoneName),DXFcolorno(curcolorno),zonedaylight(zones)%DaylRefPtAbsCoord(refpt,1),  &
                      zonedaylight(zones)%DaylRefPtAbsCoord(refpt,2),             &
                      zonedaylight(zones)%DaylRefPtAbsCoord(refpt,3),.2
      curcolorno=colorno_DaylSensor2  ! ref pts 2 and later are this color
    enddo
  enddo

  do zones=1,numofzones
    curcolorno=colorno_DaylSensor1
    TempZoneName=Zone(Zones)%Name
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    END DO
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    END DO
    do mapnum=1,totillummaps
      if (IllumMapCalc(mapnum)%Zone /= zones) cycle
      do refpt=1,IllumMapCalc(mapnum)%TotalMapRefPoints
        write(unit,710) trim(zone(zones)%Name)//':MapRefPt:'//TRIM(TrimSigDigits(refpt))
        write(unit,709) trim(TempZoneName),DXFcolorno(curcolorno),IllumMapCalc(mapnum)%MapRefPtAbsCoord(refpt,1),  &
                        IllumMapCalc(mapnum)%MapRefPtAbsCoord(refpt,2),             &
                        IllumMapCalc(mapnum)%MapRefPtAbsCoord(refpt,3),.05
      enddo
    enddo
  enddo

  ! now do DElight reference points
  do zones=1,numofzones
    curcolorno=colorno_DaylSensor1
    TempZoneName=Zone(Zones)%Name
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    END DO
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    END DO
    do refpt=1,zonedaylight(zones)%TotalDElightRefPts
      write(unit,710) trim(zone(zones)%Name)//':DEDayRefPt:'//TRIM(TrimSigDigits(refpt))
      write(unit,709) Trim(TempZoneName),DXFcolorno(curcolorno),zonedaylight(zones)%DaylRefPtAbsCoord(refpt,1),  &
                      zonedaylight(zones)%DaylRefPtAbsCoord(refpt,2),             &
                      zonedaylight(zones)%DaylRefPtAbsCoord(refpt,3),.2
      curcolorno=colorno_DaylSensor2  ! ref pts 2 and later are this color
    enddo
  enddo

  write(unit,706)
  706 format('  0',/,'ENDSEC',/,'  0',/,'EOF')

  709 format('  0',/,'CIRCLE',/,'  8',/,A,/,' 62',/,I3,/,  &
    ' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,  &
    ' 40',/,f15.5)


  710 format('999',/,A)

  close(unit)

  return

END SUBROUTINE DXFOut

SUBROUTINE DXFOutLines(ColorScheme)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   August 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine produces a points file of lines in the surfaces.

          ! METHODOLOGY EMPLOYED:
          ! Use the surface absolute coordinate information to produce
          ! lines.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataHeatBalance, ONLY: BuildingName,Zone
  USE DataSurfaces
  USE DataSurfaceColors
  USE DataDaylighting, ONLY: ZoneDaylight
  USE DataGlobals, ONLY: DegToRadians,NumOfZones
  USE DataInterfaces, ONLY: ShowWarningError, ShowContinueError, ShowFatalError
  USE DataStringGlobals, ONLY: VerString
  USE General, ONLY: TrimSigDigits

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ColorScheme

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64), dimension(4) :: StemX =(/-10.d0,-10.d0,-10.d0,-10.d0/)
  REAL(r64), dimension(4) :: StemY =(/3.d0,3.d0,0.d0,0.d0/)
  REAL(r64), dimension(4) :: StemZ =(/.1d0,0.d0,0.d0,.1d0/)
  REAL(r64), dimension(4) :: Head1X =(/-10.d0,-10.d0,-10.5d0,-10.5d0/)
  REAL(r64), dimension(4) :: Head1Y =(/3.d0,3.d0,2.133975d0,2.133975d0/)
  REAL(r64), dimension(4) :: Head1Z =(/.1d0,0.d0,0.d0,.1d0/)
  REAL(r64), dimension(4) :: Head2X =(/-10.d0,-10.d0,-9.5d0,-9.5d0/)
  REAL(r64), dimension(4) :: Head2Y =(/3.d0,3.d0,2.133975d0,2.133975d0/)
  REAL(r64), dimension(4) :: Head2Z =(/.1d0,0.d0,0.d0,.1d0/)
  REAL(r64), dimension(4) :: NSide1X =(/-10.5d0,-10.5d0,-10.5d0,-10.5d0/)
  REAL(r64), dimension(4) :: NSide1Y =(/4.5d0,4.5d0,3.5d0,3.5d0/)
  REAL(r64), dimension(4) :: NSide1Z =(/.1d0,0.d0,0.d0,.1d0/)
  REAL(r64), dimension(4) :: NSide2X =(/-10.5d0,-10.5d0,-9.5d0,-9.5d0/)
  REAL(r64), dimension(4) :: NSide2Y =(/4.5d0,4.5d0,3.5d0,3.5d0/)
  REAL(r64), dimension(4) :: NSide2Z =(/.1d0,0.d0,0.d0,.1d0/)
  REAL(r64), dimension(4) :: NSide3X =(/-9.5d0,-9.5d0,-9.5d0,-9.5d0/)
  REAL(r64), dimension(4) :: NSide3Y =(/4.5d0,4.5d0,3.5d0,3.5d0/)
  REAL(r64), dimension(4) :: NSide3Z =(/.1d0,0.d0,0.d0,.1d0/)
!  integer, dimension(7) :: colorno=(/3,4,5,6,2,8,9/)
  integer unit              ! Unit number on which to write file
  integer surf              ! Loop variable for surfaces
  integer vert              ! Loop counter
  integer,external :: getnewunitnumber  ! External function for a new unit number
  integer colorindex        ! color index by surface type
  REAL(r64) minx                 ! minimum x in surface data
  REAL(r64) miny                 ! minimum y in surface data
  REAL(r64) minz                 ! minimum z in surface data (for polygon output)
  integer zones             ! loop counter for zone loop
  character(len=25) zonenum
  character(len=MaxNameLength) TempZoneName
  integer pos
  character(len=25) ShadeType
!unused  character(len=5) :: PolylineWidth=' 0.55'
  character(len=25) cSurfNum
  integer surfcount
  integer sptr
  integer refpt
  integer curcolorno
  integer write_stat

  if (totsurfaces > 0 .and. .not. allocated(surface)) then
    ! no error needed, probably in end processing, just return
    return
  endif

  unit=getnewunitnumber()
  open(unit,file='eplusout.dxf', Action='write', iostat=write_stat)
  if (write_stat /= 0) then
   CALL ShowFatalError('DXFOutLines: Could not open file "eplusout.dxf" for output (write).')
  endif

  write(unit,702)   ! Start of Entities section
  702 format('  0',/,'SECTION',/,'  2',/,'ENTITIES')

  write(unit,707)    ! Comment
  707 format('999',/,'DXF created from EnergyPlus')

  write(unit,708) 'Program Version',',',TRIM(VerString)
  708 format('999',/,A,A,A)

  write(unit,708) 'DXF using Lines',' ',' '

  IF (ColorScheme == ' ') THEN
    write(unit,708) 'Color Scheme',',','Default'
  ELSE
    write(unit,708) 'Color Scheme',',',TRIM(ColorScheme)
  ENDIF


  minx=99999.d0
  miny=99999.d0
  do surf=1,totsurfaces
    if (surface(surf)%class == SurfaceClass_IntMass) CYCLE
    do vert=1,surface(surf)%sides
      minx=MIN(minx,surface(surf)%vertex(vert)%x)
      miny=MIN(miny,surface(surf)%vertex(vert)%y)
    enddo
  enddo

  do vert=1,4
    StemX(vert)=StemX(vert)+minx
    StemY(vert)=StemY(vert)+miny
    Head1X(vert)=Head1X(vert)+minx
    Head1Y(vert)=Head1Y(vert)+miny
    Head2X(vert)=Head2X(vert)+minx
    Head2Y(vert)=Head2Y(vert)+miny
    NSide1X(vert)=NSide1X(vert)+minx
    NSide1Y(vert)=NSide1Y(vert)+miny
    NSide2X(vert)=NSide2X(vert)+minx
    NSide2Y(vert)=NSide2Y(vert)+miny
    NSide3X(vert)=NSide3X(vert)+minx
    NSide3Y(vert)=NSide3Y(vert)+miny
  enddo

  ! This writes "True North" above the Arrow Head
  write(unit,710) 'Text - True North'
  write(unit,800) DXFcolorno(colorno_Text),StemX(1)-1.0d0 ,StemY(1),StemZ(1)
  800 format('  0',/,'TEXT',/,'  8',/,'1',/,'  6',/,'Continuous',/,  &
    ' 62',/,I3,/,' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,  &
    ' 40',/,' .25',/,'  1',/,'True North',/,' 41',/,' 0.0',/,'  7',/,'MONOTXT',/,  &
    '210',/,'0.0',/,'220',/,'0.0',/,'230',/,'1.0')

  write(unit,710) 'Text - Building Title'
  write(unit,801) DXFcolorno(colorno_Text),StemX(1)-4.0d0,StemY(1)-4.0d0 ,StemZ(1),'Building - '//TRIM(BuildingName)
  801 format('  0',/,'TEXT',/,'  8',/,'1',/,'  6',/,'Continuous',/,  &
    ' 62',/,I3,/,' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,  &
    ' 40',/,' .4',/,'  1',/,A,/,' 41',/,' 0.0',/,'  7',/,'MONOTXT',/,  &
    '210',/,'0.0',/,'220',/,'0.0',/,'230',/,'1.0')

  ! We want to point the north arrow to true north
  write(unit,710) 'North Arrow Stem'
  write(unit,703) DXFcolorno(colorno_Text),(StemX(vert),StemY(vert),StemZ(vert),vert=1,4)
  write(unit,710) 'North Arrow Head 1'
  write(unit,703) DXFcolorno(colorno_Text),(Head1X(vert),Head1Y(vert),Head1Z(vert),vert=1,4)
  write(unit,710) 'North Arrow Head 2'
  write(unit,703) DXFcolorno(colorno_Text),(Head2X(vert),Head2Y(vert),Head2Z(vert),vert=1,4)
  write(unit,710) 'North Arrow Side 1'
  write(unit,703) DXFcolorno(colorno_Text),(NSide1X(vert),NSide1Y(vert),NSide1Z(vert),vert=1,4)
  write(unit,710) 'North Arrow Side 2'
  write(unit,703) DXFcolorno(colorno_Text),(NSide2X(vert),NSide2Y(vert),NSide2Z(vert),vert=1,4)
  write(unit,710) 'North Arrow Side 3'
  write(unit,703) DXFcolorno(colorno_Text),(NSide3X(vert),NSide3Y(vert),NSide3Z(vert),vert=1,4)

  703 format('  0',/,'3DFACE',/,'  8',/,'1',/,' 62',/,I3,/,  &
    ' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,  &
    ' 11',/,f15.5,/,' 21',/,f15.5,/,' 31',/,f15.5,/,  &
    ' 12',/,f15.5,/,' 22',/,f15.5,/,' 32',/,f15.5,/,  &
    ' 13',/,f15.5,/,' 23',/,f15.5,/,' 33',/,f15.5)

  write(unit,710) 'Zone Names'
  do zones=1,NumOfZones
    write(zonenum,*) zones
    zonenum=adjustl(zonenum)
    TempZoneName=Zone(Zones)%Name
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    END DO
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    END DO
    write(unit,710) 'Zone='//trim(zonenum)//':'//trim(TempZoneName)
  enddo

  !  Do all detached shading surfaces first
  surfcount=0
  do surf=1,totsurfaces
    if (surface(surf)%heattranssurf) CYCLE
    if (surface(surf)%class == SurfaceClass_Shading) CYCLE
    if (surface(surf)%class .eq. SurfaceClass_Detached_F) colorindex=colorno_ShdDetFix
    if (surface(surf)%class .eq. SurfaceClass_Detached_B) colorindex=colorno_ShdDetBldg
    if (surface(surf)%isPV) colorindex=colorno_PV
    if (surface(surf)%class .eq. SurfaceClass_Detached_F) then
      ShadeType='Fixed Shading'
      write(unit,710) 'Fixed Shading:'//trim(surface(surf)%Name)
    elseif (surface(surf)%class .eq. SurfaceClass_Detached_B) then
      ShadeType='Building Shading'
      write(unit,710) 'Building Shading:'//trim(surface(surf)%Name)
    endif
    surfcount=surfcount+1
    write(cSurfNum,*) surfcount
    cSurfNum=adjustl(cSurfNum)
    ShadeType=TRIM(ShadeType)//'_'//TRIM(cSurfNum)
    minz=99999.d0
    do vert=1,surface(surf)%sides
      minz=MIN(minz,surface(surf)%vertex(vert)%z)
    enddo
    if (surface(surf)%sides <= 4) then
!      write(unit,711) TRIM(ShadeType),colorno(colorindex) !,minz ,TRIM(PolylineWidth),TRIM(PolylineWidth)
      do vert=1,surface(surf)%sides
        if (vert /= surface(surf)%sides) then
          sptr=vert+1
        else
          sptr=1
        endif
        write(unit,711) TRIM(ShadeType),DXFcolorno(colorindex) !,minz ,TRIM(PolylineWidth),TRIM(PolylineWidth)
        write(unit,712) surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y,surface(surf)%vertex(vert)%z,  &
                        surface(surf)%vertex(sptr)%x,surface(surf)%vertex(sptr)%y,surface(surf)%vertex(sptr)%z
      enddo
    else  ! polygon
      do vert=1,surface(surf)%sides
        if (vert /= surface(surf)%sides) then
          sptr=vert+1
        else
          sptr=1
        endif
        write(unit,711) TRIM(ShadeType),DXFcolorno(colorindex) !,minz ,TRIM(PolylineWidth),TRIM(PolylineWidth)
        write(unit,712) surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y,surface(surf)%vertex(vert)%z,  &
                        surface(surf)%vertex(sptr)%x,surface(surf)%vertex(sptr)%y,surface(surf)%vertex(sptr)%z
      enddo
    endif
  enddo

  ! now do zone surfaces, by zone
  do zones=1,NumOfZones
    TempZoneName=Zone(Zones)%Name
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    END DO
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    END DO

    surfcount=0
    do surf=max(zone(zones)%surfacefirst,1),zone(zones)%surfacelast
      if (surface(surf)%class .eq. SurfaceClass_IntMass) CYCLE
      if (surface(surf)%class .eq. SurfaceClass_Wall) colorindex=colorno_Wall
      if (surface(surf)%class .eq. SurfaceClass_Roof) colorindex=colorno_Roof
      if (surface(surf)%class .eq. SurfaceClass_Floor) colorindex=colorno_Floor
      if (surface(surf)%class .eq. SurfaceClass_Door) colorindex=colorno_Door
      if (surface(surf)%class .eq. SurfaceClass_Window) then
        if (surfacewindow(surf)%originalclass .eq. SurfaceClass_Window) colorindex=colorno_Window
        if (surfacewindow(surf)%originalclass .eq. SurfaceClass_GlassDoor) colorindex=colorno_GlassDoor
        if (surfacewindow(surf)%originalclass .eq. SurfaceClass_TDD_Dome) colorindex=colorno_TDDDome
        if (surfacewindow(surf)%originalclass .eq. SurfaceClass_TDD_Diffuser) colorindex=colorno_TDDDiffuser
      endif
      if (surface(surf)%isPV) colorindex=colorno_PV
      surfcount=surfcount+1
      surfcount=surfcount+1
      write(cSurfNum,*) surfcount
      cSurfNum=adjustl(cSurfNum)

      write(unit,710) trim(surface(surf)%ZoneName)//':'//trim(surface(surf)%Name)
      TempZoneName=TRIM(TempZoneName)//'_'//TRIM(cSurfNum)
      minz=99999.d0
      do vert=1,surface(surf)%sides
        minz=MIN(minz,surface(surf)%vertex(vert)%z)
      enddo
      if (surface(surf)%sides <= 4) then
        do vert=1,surface(surf)%sides
          if (vert /= surface(surf)%sides) then
            sptr=vert+1
          else
            sptr=1
          endif
          write(unit,711) TRIM(TempZoneName),DXFcolorno(colorindex) !,minz,TRIM(PolylineWidth),TRIM(PolylineWidth)
          write(unit,712) surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y,surface(surf)%vertex(vert)%z,  &
                        surface(surf)%vertex(sptr)%x,surface(surf)%vertex(sptr)%y,surface(surf)%vertex(sptr)%z
        enddo
      else  ! polygon
        do vert=1,surface(surf)%sides
          if (vert /= surface(surf)%sides) then
            sptr=vert+1
          else
            sptr=1
          endif
          write(unit,711) TRIM(TempZoneName),DXFcolorno(colorindex) !,minz,TRIM(PolylineWidth),TRIM(PolylineWidth)
          write(unit,712) surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y,surface(surf)%vertex(vert)%z,  &
                        surface(surf)%vertex(sptr)%x,surface(surf)%vertex(sptr)%y,surface(surf)%vertex(sptr)%z
        enddo
      endif
! 715 format('  0',/,'POLYLINE',/,'  8',/,A,/,' 62',/,I3,/,' 66',/,'  1',/,  &
!    ' 10',/,' 0.0',/,' 20',/,' 0.0',/,' 30',/,f15.5,/,  &
!    ' 70',/,'   1',/,' 40',/,A,/,' 41',/,A)
! 716 format('  0',/'VERTEX',/,'  8',/,A,/,  &
!    ' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5)
! 717 format('  0',/'SEQEND',/,'  8',/,A)

    enddo
    ! still have to do shading surfaces for zone
    surfcount=0
    do surf=1,totsurfaces
      !if (surface(surf)%heattranssurf) CYCLE ! Shading with a construction is allowed to be HT surf for daylighting shelves
      if (surface(surf)%class .ne. SurfaceClass_Shading) CYCLE
      if (surface(surf)%zonename /= zone(zones)%Name) CYCLE
      colorindex=colorno_ShdAtt
      if (surface(surf)%isPV) colorindex=colorno_PV
      surfcount=surfcount+1
      write(cSurfNum,*) surfcount
      cSurfNum=adjustl(cSurfNum)

      write(unit,710) trim(surface(surf)%ZoneName)//':'//trim(surface(surf)%Name)
      TempZoneName=TRIM(TempZoneName)//'_'//TRIM(cSurfNum)
      minz=99999.d0
      do vert=1,surface(surf)%sides
        minz=MIN(minz,surface(surf)%vertex(vert)%z)
      enddo
      if (surface(surf)%sides <= 4) then
        do vert=1,surface(surf)%sides
          if (vert /= surface(surf)%sides) then
            sptr=vert+1
          else
            sptr=1
          endif
          write(unit,711) TRIM(TempZoneName),DXFcolorno(colorindex) !,minz,TRIM(PolylineWidth),TRIM(PolylineWidth)
          write(unit,712) surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y,surface(surf)%vertex(vert)%z,  &
                        surface(surf)%vertex(sptr)%x,surface(surf)%vertex(sptr)%y,surface(surf)%vertex(sptr)%z
        enddo
      else  ! polygon attached shading
        do vert=1,surface(surf)%sides
          if (vert /= surface(surf)%sides) then
            sptr=vert+1
          else
            sptr=1
          endif
          write(unit,711) TRIM(TempZoneName),DXFcolorno(colorindex) !,minz,TRIM(PolylineWidth),TRIM(PolylineWidth)
          write(unit,712) surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y,surface(surf)%vertex(vert)%z,  &
                        surface(surf)%vertex(sptr)%x,surface(surf)%vertex(sptr)%y,surface(surf)%vertex(sptr)%z
        enddo
      endif
    enddo
  enddo

  704 format('  0',/,'3DFACE',/,'  8',/,A,/,' 62',/,I3,/,  &
    ' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,  &
    ' 11',/,f15.5,/,' 21',/,f15.5,/,' 31',/,f15.5,/,  &
    ' 12',/,f15.5,/,' 22',/,f15.5,/,' 32',/,f15.5)

  705 format(' 13',/,f15.5,/,' 23',/,f15.5,/,' 33',/,f15.5)

  711 format('  0',/,'LINE',/,'  8',/,A,/,' 62',/,I3)
  712 format(' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,  &
             ' 11',/,f15.5,/,' 21',/,f15.5,/,' 31',/,f15.5)



  ! Do any daylighting reference points on layer for zone
  do zones=1,numofzones
    curcolorno=colorno_DaylSensor1
    TempZoneName=Zone(Zones)%Name
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    END DO
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    END DO
    do refpt=1,zonedaylight(zones)%TotalDaylRefPoints
      write(unit,710) trim(zone(zones)%Name)//':DayRefPt:'//TRIM(TrimSigDigits(refpt))
      write(unit,709) trim(TempZoneName),DXFcolorno(curcolorno),zonedaylight(zones)%DaylRefPtAbsCoord(refpt,1),  &
                      zonedaylight(zones)%DaylRefPtAbsCoord(refpt,2),             &
                      zonedaylight(zones)%DaylRefPtAbsCoord(refpt,3),.2
      curcolorno=colorno_DaylSensor2  ! ref pts 2 and later are this color
    enddo
  enddo

  ! now do DElight reference points
  do zones=1,numofzones
    curcolorno=colorno_DaylSensor1
    TempZoneName=Zone(Zones)%Name
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    END DO
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    END DO
    do refpt=1,zonedaylight(zones)%TotalDElightRefPts
      write(unit,710) trim(zone(zones)%Name)//':DEDayRefPt:'//TRIM(TrimSigDigits(refpt))
      write(unit,709) Trim(TempZoneName),DXFcolorno(curcolorno),zonedaylight(zones)%DaylRefPtAbsCoord(refpt,1),  &
                      zonedaylight(zones)%DaylRefPtAbsCoord(refpt,2),             &
                      zonedaylight(zones)%DaylRefPtAbsCoord(refpt,3),.2
      curcolorno=colorno_DaylSensor2  ! ref pts 2 and later are this color
    enddo
  enddo

  write(unit,706)
  706 format('  0',/,'ENDSEC',/,'  0',/,'EOF')

  709 format('  0',/,'CIRCLE',/,'  8',/,A,/,' 62',/,I3,/,  &
    ' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,  &
    ' 40',/,f15.5)


  710 format('999',/,A)

  close(unit)

  return

END SUBROUTINE DXFOutLines

SUBROUTINE DXFOutWireFrame(ColorScheme)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   August 2005
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine produces a file of DXF objects for the surfaces (all lines -- wireframe).

          ! METHODOLOGY EMPLOYED:
          ! Use the surface absolute coordinate information to produce
          ! lines.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataHeatBalance, ONLY: BuildingName,Zone
  USE DataSurfaces
  USE DataSurfaceColors
  USE DataDaylighting, ONLY: ZoneDaylight
  USE DataGlobals, ONLY: DegToRadians,NumOfZones
  USE DataInterfaces, ONLY: ShowWarningError, ShowContinueError, ShowFatalError
  USE DataStringGlobals, ONLY: VerString
  USE General, ONLY: TrimSigDigits


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*), INTENT(IN) :: ColorScheme

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64), dimension(4) :: StemX =(/-10.d0,-10.d0,-10.d0,-10.d0/)
  REAL(r64), dimension(4) :: StemY =(/3.d0,3.d0,0.d0,0.d0/)
  REAL(r64), dimension(4) :: StemZ =(/.1d0,0.d0,0.d0,.1d0/)
  REAL(r64), dimension(4) :: Head1X =(/-10.d0,-10.d0,-10.5d0,-10.5d0/)
  REAL(r64), dimension(4) :: Head1Y =(/3.d0,3.d0,2.133975d0,2.133975d0/)
  REAL(r64), dimension(4) :: Head1Z =(/.1d0,0.d0,0.d0,.1d0/)
  REAL(r64), dimension(4) :: Head2X =(/-10.d0,-10.d0,-9.5d0,-9.5d0/)
  REAL(r64), dimension(4) :: Head2Y =(/3.d0,3.d0,2.133975d0,2.133975d0/)
  REAL(r64), dimension(4) :: Head2Z =(/.1d0,0.d0,0.d0,.1d0/)
  REAL(r64), dimension(4) :: NSide1X =(/-10.5d0,-10.5d0,-10.5d0,-10.5d0/)
  REAL(r64), dimension(4) :: NSide1Y =(/4.5d0,4.5d0,3.5d0,3.5d0/)
  REAL(r64), dimension(4) :: NSide1Z =(/.1d0,0.d0,0.d0,.1d0/)
  REAL(r64), dimension(4) :: NSide2X =(/-10.5d0,-10.5d0,-9.5d0,-9.5d0/)
  REAL(r64), dimension(4) :: NSide2Y =(/4.5d0,4.5d0,3.5d0,3.5d0/)
  REAL(r64), dimension(4) :: NSide2Z =(/.1d0,0.d0,0.d0,.1d0/)
  REAL(r64), dimension(4) :: NSide3X =(/-9.5d0,-9.5d0,-9.5d0,-9.5d0/)
  REAL(r64), dimension(4) :: NSide3Y =(/4.5d0,4.5d0,3.5d0,3.5d0/)
  REAL(r64), dimension(4) :: NSide3Z =(/.1d0,0.d0,0.d0,.1d0/)
!  integer, dimension(7) :: colorno=(/3,4,5,6,2,8,9/)
  integer unit              ! Unit number on which to write file
  integer surf              ! Loop variable for surfaces
  integer vert              ! Loop counter
  integer,external :: getnewunitnumber  ! External function for a new unit number
  integer colorindex        ! color index by surface type
  REAL(r64) minx                 ! minimum x in surface data
  REAL(r64) miny                 ! minimum y in surface data
  REAL(r64) minz                 ! minimum z in surface data (for polygon output)
  integer zones             ! loop counter for zone loop
  character(len=25) zonenum
  character(len=MaxNameLength) TempZoneName
  character(len=MaxNameLength) SaveZoneName
  integer pos
  character(len=25) ShadeType
  character(len=5) :: PolylineWidth=' 0.55'
  character(len=25) cSurfNum
  integer surfcount
  integer refpt
  integer curcolorno
  integer write_stat

  if (totsurfaces > 0 .and. .not. allocated(surface)) then
    ! no error needed, probably in end processing, just return
    return
  endif

  unit=getnewunitnumber()
  open(unit,file='eplusout.dxf', Action='write', iostat=write_stat)
  if (write_stat /= 0) then
   CALL ShowFatalError('DXFOutWireFrame: Could not open file "eplusout.dxf" for output (write).')
  endif

  write(unit,702)   ! Start of Entities section
  702 format('  0',/,'SECTION',/,'  2',/,'ENTITIES')

  write(unit,707)    ! Comment
  707 format('999',/,'DXF created from EnergyPlus')

  write(unit,708) 'Program Version',',',TRIM(VerString)
  708 format('999',/,A,A,A)

  write(unit,708) 'DXF using Wireframe',' ',' '

  IF (ColorScheme == ' ') THEN
    write(unit,708) 'Color Scheme',',','Default'
  ELSE
    write(unit,708) 'Color Scheme',',',TRIM(ColorScheme)
  ENDIF

  minx=99999.d0
  miny=99999.d0
  do surf=1,totsurfaces
    if (surface(surf)%class == SurfaceClass_IntMass) CYCLE
    do vert=1,surface(surf)%sides
      minx=MIN(minx,surface(surf)%vertex(vert)%x)
      miny=MIN(miny,surface(surf)%vertex(vert)%y)
    enddo
  enddo

  do vert=1,4
    StemX(vert)=StemX(vert)+minx
    StemY(vert)=StemY(vert)+miny
    Head1X(vert)=Head1X(vert)+minx
    Head1Y(vert)=Head1Y(vert)+miny
    Head2X(vert)=Head2X(vert)+minx
    Head2Y(vert)=Head2Y(vert)+miny
    NSide1X(vert)=NSide1X(vert)+minx
    NSide1Y(vert)=NSide1Y(vert)+miny
    NSide2X(vert)=NSide2X(vert)+minx
    NSide2Y(vert)=NSide2Y(vert)+miny
    NSide3X(vert)=NSide3X(vert)+minx
    NSide3Y(vert)=NSide3Y(vert)+miny
  enddo

  ! This writes "True North" above the Arrow Head
  write(unit,710) 'Text - True North'
  write(unit,800) DXFcolorno(colorno_Text),StemX(1)-1.0d0 ,StemY(1),StemZ(1)
  800 format('  0',/,'TEXT',/,'  8',/,'1',/,'  6',/,'Continuous',/,  &
    ' 62',/,I3,/,' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,  &
    ' 40',/,' .25',/,'  1',/,'True North',/,' 41',/,' 0.0',/,'  7',/,'MONOTXT',/,  &
    '210',/,'0.0',/,'220',/,'0.0',/,'230',/,'1.0')

  write(unit,710) 'Text - Building Title'
  write(unit,801) DXFcolorno(colorno_Text),StemX(1)-4.0d0,StemY(1)-4.0d0 ,StemZ(1),'Building - '//TRIM(BuildingName)
  801 format('  0',/,'TEXT',/,'  8',/,'1',/,'  6',/,'Continuous',/,  &
    ' 62',/,I3,/,' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,  &
    ' 40',/,' .4',/,'  1',/,A,/,' 41',/,' 0.0',/,'  7',/,'MONOTXT',/,  &
    '210',/,'0.0',/,'220',/,'0.0',/,'230',/,'1.0')

  ! We want to point the north arrow to true north
  write(unit,710) 'North Arrow Stem'
  write(unit,703) DXFcolorno(colorno_Text),(StemX(vert),StemY(vert),StemZ(vert),vert=1,4)
  write(unit,710) 'North Arrow Head 1'
  write(unit,703) DXFcolorno(colorno_Text),(Head1X(vert),Head1Y(vert),Head1Z(vert),vert=1,4)
  write(unit,710) 'North Arrow Head 2'
  write(unit,703) DXFcolorno(colorno_Text),(Head2X(vert),Head2Y(vert),Head2Z(vert),vert=1,4)
  write(unit,710) 'North Arrow Side 1'
  write(unit,703) DXFcolorno(colorno_Text),(NSide1X(vert),NSide1Y(vert),NSide1Z(vert),vert=1,4)
  write(unit,710) 'North Arrow Side 2'
  write(unit,703) DXFcolorno(colorno_Text),(NSide2X(vert),NSide2Y(vert),NSide2Z(vert),vert=1,4)
  write(unit,710) 'North Arrow Side 3'
  write(unit,703) DXFcolorno(colorno_Text),(NSide3X(vert),NSide3Y(vert),NSide3Z(vert),vert=1,4)

  703 format('  0',/,'3DFACE',/,'  8',/,'1',/,' 62',/,I3,/,  &
    ' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,  &
    ' 11',/,f15.5,/,' 21',/,f15.5,/,' 31',/,f15.5,/,  &
    ' 12',/,f15.5,/,' 22',/,f15.5,/,' 32',/,f15.5,/,  &
    ' 13',/,f15.5,/,' 23',/,f15.5,/,' 33',/,f15.5)

  write(unit,710) 'Zone Names'
  do zones=1,NumOfZones
    write(zonenum,*) zones
    zonenum=adjustl(zonenum)
    TempZoneName=Zone(Zones)%Name
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    END DO
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    END DO
    write(unit,710) 'Zone='//trim(zonenum)//':'//trim(TempZoneName)
  enddo

  !  Do all detached shading surfaces first
  surfcount=0
  do surf=1,totsurfaces
    if (surface(surf)%heattranssurf) CYCLE
    if (surface(surf)%class == SurfaceClass_Shading) CYCLE
    if (surface(surf)%class .eq. SurfaceClass_Detached_F) colorindex=colorno_ShdDetFix
    if (surface(surf)%class .eq. SurfaceClass_Detached_B) colorindex=colorno_ShdDetBldg
    if (surface(surf)%isPV) colorindex=colorno_PV
    if (surface(surf)%class .eq. SurfaceClass_Detached_F) then
      ShadeType='Fixed Shading'
      write(unit,710) 'Fixed Shading:'//trim(surface(surf)%Name)
    elseif (surface(surf)%class .eq. SurfaceClass_Detached_B) then
      ShadeType='Building Shading'
      write(unit,710) 'Building Shading:'//trim(surface(surf)%Name)
    endif
    surfcount=surfcount+1
    write(cSurfNum,*) surfcount
    cSurfNum=adjustl(cSurfNum)
    ShadeType=TRIM(ShadeType)//'_'//TRIM(cSurfNum)
    minz=99999.d0
    do vert=1,surface(surf)%sides
      minz=MIN(minz,surface(surf)%vertex(vert)%z)
    enddo
    if (surface(surf)%sides <= 4) then
      write(unit,715) TRIM(ShadeType),DXFcolorno(colorindex),minz,TRIM(PolylineWidth),TRIM(PolylineWidth)
      do vert=1,surface(surf)%sides
        write(unit,716) TRIM(ShadeType),  &
                        surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y,surface(surf)%vertex(vert)%z
      enddo
      write(unit,717) TRIM(ShadeType)
    else  ! polygon
      write(unit,715) TRIM(ShadeType),DXFcolorno(colorindex),minz,TRIM(PolylineWidth),TRIM(PolylineWidth)
      do vert=1,surface(surf)%sides
        write(unit,716) TRIM(ShadeType),  &
                        surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y,surface(surf)%vertex(vert)%z
      enddo
      write(unit,717) TRIM(ShadeType)
    endif
  enddo

  ! now do zone surfaces, by zone
  do zones=1,NumOfZones
    SaveZoneName=Zone(Zones)%Name
    pos=INDEX(SaveZoneName(1:LEN_TRIM(SaveZoneName)),' ')
    DO WHILE (pos /= 0)
      SaveZoneName(pos:pos)='_'
      pos=INDEX(SaveZoneName(1:LEN_TRIM(SaveZoneName)),' ')
    END DO
    pos=INDEX(SaveZoneName(1:LEN_TRIM(SaveZoneName)),':')
    DO WHILE (pos /= 0)
      SaveZoneName(pos:pos)='_'
      pos=INDEX(SaveZoneName(1:LEN_TRIM(SaveZoneName)),':')
    END DO

    surfcount=0
    do surf=max(zone(zones)%surfacefirst,1),zone(zones)%surfacelast
      if (surface(surf)%class .eq. SurfaceClass_IntMass) CYCLE
      if (surface(surf)%class .eq. SurfaceClass_Wall) colorindex=colorno_Wall
      if (surface(surf)%class .eq. SurfaceClass_Roof) colorindex=colorno_Roof
      if (surface(surf)%class .eq. SurfaceClass_Floor) colorindex=colorno_Floor
      if (surface(surf)%class .eq. SurfaceClass_Door) colorindex=colorno_Door
      if (surface(surf)%class .eq. SurfaceClass_Window) then
        if (surfacewindow(surf)%originalclass .eq. SurfaceClass_Window) colorindex=colorno_Window
        if (surfacewindow(surf)%originalclass .eq. SurfaceClass_GlassDoor) colorindex=colorno_GlassDoor
        if (surfacewindow(surf)%originalclass .eq. SurfaceClass_TDD_Dome) colorindex=colorno_TDDDome
        if (surfacewindow(surf)%originalclass .eq. SurfaceClass_TDD_Diffuser) colorindex=colorno_TDDDiffuser
      endif
      if (surface(surf)%isPV) colorindex=colorno_PV
      surfcount=surfcount+1
      write(cSurfNum,*) surfcount
      cSurfNum=adjustl(cSurfNum)

      write(unit,710) trim(surface(surf)%ZoneName)//':'//trim(surface(surf)%Name)
      TempZoneName=TRIM(SaveZoneName)//'_'//TRIM(cSurfNum)
      minz=99999.d0
      do vert=1,surface(surf)%sides
        minz=MIN(minz,surface(surf)%vertex(vert)%z)
      enddo
      if (surface(surf)%sides <= 4) then
        write(unit,715) TRIM(TempZoneName),DXFcolorno(colorindex),minz,TRIM(PolylineWidth),TRIM(PolylineWidth)
        do vert=1,surface(surf)%sides
          write(unit,716) TRIM(TempZoneName),  &
                        surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y,surface(surf)%vertex(vert)%z
        enddo
        write(unit,717) TRIM(TempZoneName)
      else  ! polygon
        write(unit,715) TRIM(TempZoneName),DXFcolorno(colorindex),minz,TRIM(PolylineWidth),TRIM(PolylineWidth)
        do vert=1,surface(surf)%sides
          write(unit,716) TRIM(TempZoneName),  &
                        surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y,surface(surf)%vertex(vert)%z
        enddo
        write(unit,717) TRIM(TempZoneName)
      endif
 715 format('  0',/,'POLYLINE',/,'  8',/,A,/,' 62',/,I3,/,' 66',/,'  1',/,  &
    ' 10',/,' 0.0',/,' 20',/,' 0.0',/,' 30',/,f15.5,/,  &
    ' 70',/,'   9',/,' 40',/,A,/,' 41',/,A)
 716 format('  0',/'VERTEX',/,'  8',/,A,/,  &
    ' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5)
 717 format('  0',/'SEQEND',/,'  8',/,A)

    enddo
    ! still have to do shading surfaces for zone
    surfcount=0
    do surf=1,totsurfaces
      !if (surface(surf)%heattranssurf) CYCLE ! Shading with a construction is allowed to be HT surf for daylighting shelves
      if (surface(surf)%class .ne. SurfaceClass_Shading) CYCLE
      if (surface(surf)%zonename /= zone(zones)%Name) CYCLE
      colorindex=colorno_ShdAtt
      if (surface(surf)%isPV) colorindex=colorno_PV
      surfcount=surfcount+1
      write(cSurfNum,*) surfcount
      cSurfNum=adjustl(cSurfNum)

      write(unit,710) trim(surface(surf)%ZoneName)//':'//trim(surface(surf)%Name)
      TempZoneName=TRIM(SaveZoneName)//'_'//TRIM(cSurfNum)
      minz=99999.d0
      do vert=1,surface(surf)%sides
        minz=MIN(minz,surface(surf)%vertex(vert)%z)
      enddo
      if (surface(surf)%sides <= 4) then
        write(unit,715) TRIM(TempZoneName),DXFcolorno(colorindex),minz,TRIM(PolylineWidth),TRIM(PolylineWidth)
        do vert=1,surface(surf)%sides
          write(unit,716) TRIM(TempZoneName),  &
                        surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y,surface(surf)%vertex(vert)%z
        enddo
        write(unit,717) TRIM(TempZoneName)
      else  ! polygon attached shading
        write(unit,715) TRIM(TempZoneName),DXFcolorno(colorindex),minz,TRIM(PolylineWidth),TRIM(PolylineWidth)
        do vert=1,surface(surf)%sides
          write(unit,716) TRIM(TempZoneName),  &
                        surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y,surface(surf)%vertex(vert)%z
        enddo
        write(unit,717) TRIM(TempZoneName)
      endif
    enddo
  enddo

  704 format('  0',/,'3DFACE',/,'  8',/,A,/,' 62',/,I3,/,  &
    ' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,  &
    ' 11',/,f15.5,/,' 21',/,f15.5,/,' 31',/,f15.5,/,  &
    ' 12',/,f15.5,/,' 22',/,f15.5,/,' 32',/,f15.5)

  705 format(' 13',/,f15.5,/,' 23',/,f15.5,/,' 33',/,f15.5)

!  711 format('  0',/,'LINE',/,'  8',/,A,/,' 62',/,I3)
!  712 format(' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,  &
!             ' 11',/,f15.5,/,' 21',/,f15.5,/,' 31',/,f15.5)



  ! Do any daylighting reference points on layer for zone
  do zones=1,numofzones
    curcolorno=colorno_DaylSensor1
    TempZoneName=Zone(Zones)%Name
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    END DO
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    END DO
    do refpt=1,zonedaylight(zones)%TotalDaylRefPoints
      write(unit,710) trim(zone(zones)%Name)//':DayRefPt:'//TRIM(TrimSigDigits(refpt))
      write(unit,709) trim(TempZoneName),DXFcolorno(curcolorno),zonedaylight(zones)%DaylRefPtAbsCoord(refpt,1),  &
                      zonedaylight(zones)%DaylRefPtAbsCoord(refpt,2),             &
                      zonedaylight(zones)%DaylRefPtAbsCoord(refpt,3),.2
      curcolorno=colorno_DaylSensor2  ! ref pts 2 and later are this color
    enddo
  enddo

  ! now do DElight reference points
  do zones=1,numofzones
    curcolorno=colorno_DaylSensor1
    TempZoneName=Zone(Zones)%Name
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    END DO
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    END DO
    do refpt=1,zonedaylight(zones)%TotalDElightRefPts
      write(unit,710) trim(zone(zones)%Name)//':DEDayRefPt:'//TRIM(TrimSigDigits(refpt))
      write(unit,709) Trim(TempZoneName),DXFcolorno(curcolorno),zonedaylight(zones)%DaylRefPtAbsCoord(refpt,1),  &
                      zonedaylight(zones)%DaylRefPtAbsCoord(refpt,2),             &
                      zonedaylight(zones)%DaylRefPtAbsCoord(refpt,3),.2
      curcolorno=colorno_DaylSensor2  ! ref pts 2 and later are this color
    enddo
  enddo

  write(unit,706)
  706 format('  0',/,'ENDSEC',/,'  0',/,'EOF')

  709 format('  0',/,'CIRCLE',/,'  8',/,A,/,' 62',/,I3,/,  &
    ' 10',/,f15.5,/,' 20',/,f15.5,/,' 30',/,f15.5,/,  &
    ' 40',/,f15.5)


  710 format('999',/,A)

  close(unit)

  return

END SUBROUTINE DXFOutWireFrame

SUBROUTINE DetailsForSurfaces(RptType)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda Lawrie
          !       DATE WRITTEN   February 2002
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine provides an optional detailed surface report
          ! for each surface in the input file.

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataHeatBalance
  USE DataSurfaces
  USE DataGlobals, ONLY: NumOfZones,OutputFileInits
  USE General, ONLY: RoundSigDigits,TrimSigDigits
  USE ScheduleManager, ONLY: GetScheduleName,GetScheduleMinValue,GetScheduleMaxValue


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: RptType ! (1=Vertices only, 10=Details only, 11=Details with vertices)

          ! SUBROUTINE PARAMETER DEFINITIONS:
  CHARACTER(len=*), PARAMETER, DIMENSION(1:9) :: ConvCoeffCalcs=(/'ASHRAESimple               ',  &
                                                                  'ASHRAETARP                 ', &
                                                                  'CeilingDiffuser            ', &
                                                                  'TrombeWall                 ', &
                                                                  'TARP                       ', &
                                                                  'MoWitt                     ', &
                                                                  'DOE-2                      ', &
                                                                  'BLAST                      ', &
                                                                  'AdaptiveConvectionAlgorithm'/)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  integer unit              ! Unit number on which to write file
  integer surf              ! Loop variable for surfaces
  integer vert              ! Loop counter
  integer zonenum           ! Loop counter
  character(len=MaxNameLength) :: BaseSurfName
  character(len=MaxNameLength) :: ConstructionName
  character(len=MaxNameLength) :: scheduleName
  CHARACTER(len=32) :: IntConvCoeffCalc
  CHARACTER(len=32) :: ExtConvCoeffCalc
  REAL(r64) :: NominalUwithConvCoeffs
  CHARACTER(len=32) :: cNominalU
  CHARACTER(len=32) :: cNominalUwithConvCoeffs
  CHARACTER(len=32) :: cSchedMin
  CHARACTER(len=32) :: cSchedMax
  CHARACTER(len=3)  :: SolarDiffusing
  integer fd
  character(len=MaxNameLength) :: AlgoName
  logical :: isWithConvCoefValid

  if (totsurfaces > 0 .and. .not. allocated(surface)) then
    ! no error needed, probably in end processing, just return
    return
  endif

  unit=OutputFileInits

!!!!    Write Header lines for report
  if (RptType == 10) then  ! Details only
    write(unit,700)
    write(unit,701,advance='No')
    write(unit,7011)
    write(unit,702,advance='No')
    write(unit,7021)
  elseif (RptType == 11) then ! Details with Vertices
    write(unit,700,advance='No')
    write(unit,710)
    write(unit,701,advance='No')
    write(unit,7011,advance='No')
    write(unit,707)
    write(unit,702,advance='No')
    write(unit,7021,advance='No')
    write(unit,708)
  else     ! Vertices only
    write(unit,700,advance='No')
    write(unit,710)
    write(unit,701,advance='No')
    write(unit,7012,advance='No')
    write(unit,707)
    write(unit,702,advance='No')
    write(unit,708)
  endif

  ! Do just "detached" shading first
  do surf=1,totsurfaces
    if (Surface(surf)%Zone /= 0) EXIT
  enddo
  if ((surf-1) > 0) then
    write(unit,703) 'Shading_Surfaces','Number of Shading Surfaces',surf-1
    do surf=1,totsurfaces
      if (Surface(surf)%Zone /= 0) EXIT
      AlgoName = 'None'
      write(unit,704,advance='No') 'Shading',trim(Surface(surf)%Name),trim(cSurfaceClass(Surface(surf)%class)),  &
                                                          trim(Surface(surf)%BaseSurfName), TRIM(AlgoName)
      if (RptType == 10) then
        if (Surface(surf)%SchedShadowSurfIndex > 0) then
          scheduleName=GetScheduleName(Surface(surf)%SchedShadowSurfIndex)
          cSchedMin=RoundSigDigits(GetScheduleMinValue(Surface(surf)%SchedShadowSurfIndex),2)
          cSchedMax=RoundSigDigits(GetScheduleMaxValue(Surface(surf)%SchedShadowSurfIndex),2)
        else
          scheduleName=' '
          cSchedMin='0.0'
          cSchedMax='0.0'
        endif
        write(unit,7044,advance='No') trim(scheduleName),trim(cSchedMin),trim(cSchedMax),' ',  &
                                      trim(RoundSigDigits(Surface(surf)%Area,2)),trim(RoundSigDigits(Surface(surf)%GrossArea,2)),  &
                                      trim(RoundSigDigits(Surface(surf)%NetAreaShadowCalc,2)),  &
                                      trim(RoundSigDigits(Surface(surf)%Azimuth,2)),trim(RoundSigDigits(Surface(surf)%Tilt,2)),   &
                                      trim(RoundSigDigits(Surface(surf)%Width,2)),trim(RoundSigDigits(Surface(surf)%Height,2))
        write(unit,7061) trim(TrimSigDigits(Surface(surf)%Sides))
      elseif (RptType == 1) then
        write(unit,7042,advance='No') trim(TrimSigDigits(Surface(surf)%Sides))
      else
        if (Surface(surf)%SchedShadowSurfIndex > 0) then
          scheduleName=GetScheduleName(Surface(surf)%SchedShadowSurfIndex)
          cSchedMin=RoundSigDigits(GetScheduleMinValue(Surface(surf)%SchedShadowSurfIndex),2)
          cSchedMax=RoundSigDigits(GetScheduleMaxValue(Surface(surf)%SchedShadowSurfIndex),2)
        else
          scheduleName=' '
          cSchedMin='0.0'
          cSchedMax='0.0'
        endif
        write(unit,7044,advance='No') trim(scheduleName),trim(cSchedMin),trim(cSchedMax),' ',  &
                                      trim(RoundSigDigits(Surface(surf)%Area,2)),trim(RoundSigDigits(Surface(surf)%GrossArea,2)), &
                                      trim(RoundSigDigits(Surface(surf)%NetAreaShadowCalc,2)),  &
                                      trim(RoundSigDigits(Surface(surf)%Azimuth,2)),trim(RoundSigDigits(Surface(surf)%Tilt,2)),   &
                                      trim(RoundSigDigits(Surface(surf)%Width,2)),trim(RoundSigDigits(Surface(surf)%Height,2))
        write(unit,7061,advance='No') trim(TrimSigDigits(Surface(surf)%Sides))
      endif
      if (RptType == 10) CYCLE
      do vert=1,Surface(surf)%Sides
        if (vert /= Surface(Surf)%Sides) then
          write(unit,709,advance='No') trim(RoundSigDigits(Surface(surf)%vertex(vert)%x,2)),  &
                                       trim(RoundSigDigits(Surface(surf)%vertex(vert)%y,2)),  &
                                       trim(RoundSigDigits(Surface(surf)%vertex(vert)%z,2))
        else
          write(unit,709)  trim(RoundSigDigits(Surface(surf)%vertex(vert)%x,2)),  &
                           trim(RoundSigDigits(Surface(surf)%vertex(vert)%y,2)),  &
                           trim(RoundSigDigits(Surface(surf)%vertex(vert)%z,2))
        endif
      enddo
      !  This shouldn't happen with shading surface -- always have vertices
      if (Surface(surf)%Sides == 0) write(unit,711)
    enddo
  endif

  do zonenum=1,NumOfZones
    write(unit,703) 'Zone_Surfaces',TRIM(Zone(zonenum)%Name),(Zone(zonenum)%SurfaceLast-Zone(zonenum)%SurfaceFirst+1)
    do surf=1,totsurfaces
      if (Surface(surf)%Zone /= zonenum) CYCLE
      SolarDiffusing=' '
      if (RptType == 10 .or. RptType == 11) then  ! Details and Details with Vertices
        if (Surface(surf)%BaseSurf == surf) then
          BaseSurfName=' '
        else
          BaseSurfName=Surface(surf)%BaseSurfName
        endif
        SELECT CASE (Surface(surf)%HeatTransferAlgorithm)
        CASE (HeatTransferModel_None)
          AlgoName = 'None'
        CASE (HeatTransferModel_CTF)
          AlgoName = 'CTF - ConductionTransferFunction'
        CASE (HeatTransferModel_CondFD)
          AlgoName = 'CondFD - ConductionFiniteDifference'
        CASE (HeatTransferModel_EMPD)
          AlgoName = 'EMPD - MoisturePenetrationDepthConductionTransferFunction'
        CASE (HeatTransferModel_HAMT)
          AlgoName = 'HAMT - CombinedHeatAndMoistureFiniteElement'
        CASE (HeatTransferModel_Window5)
          AlgoName = 'Window5 Detailed Fenestration'
        CASE (HeatTransferModel_ComplexFenestration)
          AlgoName = 'Window7 Complex Fenestration'
        CASE (HeatTransferModel_TDD)
          AlgoName = 'Tubular Daylighting Device'
        END SELECT
        ! Default Convection Coefficient Calculation Algorithms
        IntConvCoeffCalc=ConvCoeffCalcs(Zone(zonenum)%InsideConvectionAlgo)
        ExtConvCoeffCalc=ConvCoeffCalcs(Zone(zonenum)%OutsideConvectionAlgo)


        write(unit,704,advance='No') 'HeatTransfer',trim(Surface(surf)%Name),trim(cSurfaceClass(Surface(surf)%class)),  &
                                                                              trim(BaseSurfName), trim(AlgoName)

        ! NOTE - THIS CODE IS REPEATED IN SurfaceGeometry.F90 IN SetupZoneGeometry
        ! Calculate Nominal U-value with convection/film coefficients for reporting by adding on
        ! prescribed R-values for interior and exterior convection coefficients as found in ASHRAE 90.1-2004, Appendix A
        if (Surface(surf)%Construction > 0 .and. Surface(surf)%Construction <= TotConstructs) THEN
          NominalUwithConvCoeffs = ComputeNominalUwithConvCoeffs(surf,isWithConvCoefValid)
          ConstructionName=Construct(Surface(surf)%Construction)%Name
          IF (isWithConvCoefValid) THEN
            cNominalUwithConvCoeffs=RoundSigDigits(NominalUwithConvCoeffs,3)
          ELSE
            cNominalUwithConvCoeffs = '[invalid]'
          END IF
          IF ((Surface(surf)%class == SurfaceClass_Window) .OR. (Surface(surf)%class == SurfaceClass_TDD_Dome)) THEN
            ! SurfaceClass_Window also covers glass doors and TDD:Diffusers
            cNominalU='N/A'
            IF (SurfaceWindow(surf)%SolarDiffusing) THEN
              SolarDiffusing='Yes'
            ELSE
              SolarDiffusing='No'
            ENDIF
          ELSE
            cNominalU=RoundSigDigits(NominalU(Surface(surf)%Construction),3)
          END IF
        else
          CNominalUwithConvCoeffs = '**'
          CNominalU = '**'
          ConstructionName='**invalid**'
        endif

        write(unit,7041,advance='No')trim(ConstructionName),trim(cNominalU),trim(cNominalUwithConvCoeffs),trim(SolarDiffusing),  &
                            trim(RoundSigDigits(Surface(surf)%Area,2)),trim(RoundSigDigits(Surface(surf)%GrossArea,2)),   &
                            trim(RoundSigDigits(Surface(surf)%NetAreaShadowCalc,2)),  &
                            trim(RoundSigDigits(Surface(surf)%Azimuth,2)),trim(RoundSigDigits(Surface(surf)%Tilt,2)),    &
                            trim(RoundSigDigits(Surface(surf)%Width,2)),trim(RoundSigDigits(Surface(surf)%Height,2)),    &
                            trim(RoundSigDigits(Surface(surf)%Reveal,2))
        if (Surface(surf)%IntConvCoeff > 0) THEN
          SELECT CASE (UserIntConvectionCoeffs(Surface(surf)%IntConvCoeff)%OverrideType)
          CASE ( ConvCoefValue)
            IntConvCoeffCalc='User Supplied Value'
          CASE ( ConvCoefSchedule )
            IntConvCoeffCalc='User Supplied Schedule'
          CASE (ConvCoefUserCurve)
            ExtConvCoeffCalc='User Supplied Curve'
          CASE (ConvCoefSpecifiedModel)
            ExtConvCoeffCalc='User Specified Model'
          END SELECT
        elseif (Surface(surf)%IntConvCoeff < 0) THEN  ! not in use yet.
          IntConvCoeffCalc=ConvCoeffCalcs(ABS(Surface(surf)%IntConvCoeff))
        endif
        if (Surface(surf)%ExtConvCoeff > 0) THEN
          SELECT CASE (UserExtConvectionCoeffs(Surface(surf)%ExtConvCoeff)%OverrideType)
          CASE (ConvCoefValue)
            ExtConvCoeffCalc='User Supplied Value'
          CASE (ConvCoefSchedule)
            ExtConvCoeffCalc='User Supplied Schedule'
          CASE (ConvCoefUserCurve)
            ExtConvCoeffCalc='User Supplied Curve'
          CASE (ConvCoefSpecifiedModel)
            ExtConvCoeffCalc='User Specified Model'
          END SELECT
        elseif (Surface(surf)%ExtConvCoeff < 0) THEN
          ExtConvCoeffCalc=ConvCoeffCalcs(ABS(Surface(surf)%ExtConvCoeff))
        endif
        if (Surface(surf)%extboundcond == ExternalEnvironment) then
          write(unit,705,advance='No') 'ExternalEnvironment'
          write(unit,705,advance='No') trim(ExtConvCoeffCalc)
          write(unit,705,advance='No') trim(IntConvCoeffCalc)
        elseif (Surface(surf)%extboundcond == Ground) then
          write(unit,705,advance='No') 'Ground'
          write(unit,705,advance='No') 'N/A-Ground'
          write(unit,705,advance='No') trim(IntConvCoeffCalc)
        elseif (Surface(surf)%extboundcond == GroundFCfactorMethod) then
          write(unit,705,advance='No') 'FCGround'
          write(unit,705,advance='No') 'N/A-FCGround'
          write(unit,705,advance='No') trim(IntConvCoeffCalc)
        elseif (Surface(surf)%extboundcond == OtherSideCoefNoCalcExt .or. Surface(surf)%extboundcond == OtherSideCoefCalcExt) then
          write(unit,705,advance='No') TRIM(OSC(Surface(surf)%OSCPtr)%Name)
          write(unit,705,advance='No') 'N/A-OSC'
          write(unit,705,advance='No') trim(IntConvCoeffCalc)
        elseif (Surface(surf)%extboundcond == OtherSideCondModeledExt) then
          write(unit,705,advance='No') TRIM(OSCM(Surface(surf)%OSCMPtr)%Name)
          write(unit,705,advance='No') 'N/A-OSCM'
          write(unit,705,advance='No') trim(IntConvCoeffCalc)
        else
          write(unit,705,advance='No') trim(Surface(surf)%ExtBoundCondName)
          write(unit,705,advance='No') 'Other/Same Surface Int Conv'
          write(unit,705,advance='No') trim(IntConvCoeffCalc)
        endif
        if (Surface(surf)%ExtSolar) then
          write(unit,705,advance='No') 'SunExposed'
        else
          write(unit,705,advance='No') 'NoSun'
        endif
        if (Surface(surf)%ExtWind) then
          write(unit,705,advance='No') 'WindExposed'
        else
          write(unit,705,advance='No') 'NoWind'
        endif
        if (RptType == 10) then
          write(unit,706) trim(RoundSigDigits(Surface(surf)%ViewFactorGround,2)),    &
                          trim(RoundSigDigits(Surface(surf)%ViewFactorSky,2)),       &
                          trim(RoundSigDigits(Surface(surf)%ViewFactorGroundIR,2)),  &
                          trim(RoundSigDigits(Surface(surf)%ViewFactorSkyIR,2)),     &
                          trim(TrimSigDigits(Surface(surf)%Sides))
        else
          write(unit,706,advance='No')  trim(RoundSigDigits(Surface(surf)%ViewFactorGround,2)),    &
                          trim(RoundSigDigits(Surface(surf)%ViewFactorSky,2)),       &
                          trim(RoundSigDigits(Surface(surf)%ViewFactorGroundIR,2)),  &
                          trim(RoundSigDigits(Surface(surf)%ViewFactorSkyIR,2)),     &
                          trim(TrimSigDigits(Surface(surf)%Sides))
          do vert=1,Surface(surf)%Sides
            if (vert /= Surface(Surf)%Sides) then
              write(unit,709,advance='No') trim(RoundSigDigits(Surface(surf)%vertex(vert)%x,2)),  &
                                       trim(RoundSigDigits(Surface(surf)%vertex(vert)%y,2)),  &
                                       trim(RoundSigDigits(Surface(surf)%vertex(vert)%z,2))
            else
              write(unit,709)  trim(RoundSigDigits(Surface(surf)%vertex(vert)%x,2)),  &
                           trim(RoundSigDigits(Surface(surf)%vertex(vert)%y,2)),  &
                           trim(RoundSigDigits(Surface(surf)%vertex(vert)%z,2))
            endif
          enddo
          IF (Surface(surf)%Sides == 0) write(unit,711)
        endif
! if window, report frame/divider as appropriate
        if (Surface(surf)%FrameDivider > 0) then
          fd=Surface(surf)%FrameDivider
          if (FrameDivider(fd)%FrameWidth > 0.0d0) then
            SELECT CASE (Surface(surf)%HeatTransferAlgorithm)
            CASE (HeatTransferModel_None)
              AlgoName = 'None'
            CASE (HeatTransferModel_CTF)
              AlgoName = 'CTF - ConductionTransferFunction'
            CASE (HeatTransferModel_CondFD)
              AlgoName = 'CondFD - ConductionFiniteDifference'
            CASE (HeatTransferModel_EMPD)
              AlgoName = 'EMPD - MoisturePenetrationDepthConductionTransferFunction'
            CASE (HeatTransferModel_HAMT)
              AlgoName = 'HAMT - CombinedHeatAndMoistureFiniteElement'
            CASE (HeatTransferModel_Window5)
              AlgoName = 'Window5 Detailed Fenestration'
            CASE (HeatTransferModel_ComplexFenestration)
              AlgoName = 'Window7 Complex Fenestration'
            CASE (HeatTransferModel_TDD)
              AlgoName = 'Tubular Daylighting Device'
            END SELECT
            write(unit,704,advance='No') 'Frame/Divider',trim(FrameDivider(fd)%Name),'Frame',trim(Surface(surf)%Name) &
                           , trim(AlgoName)
            write(unit,7045) trim(RoundSigDigits(SurfaceWindow(surf)%FrameArea,2)),  &
               trim(RoundSigDigits(SurfaceWindow(surf)%FrameArea/Surface(surf)%Multiplier,2)),'*','N/A','N/A',  &
                             trim(RoundSigDigits(FrameDivider(fd)%FrameWidth,2)),'N/A'
          endif
          if (FrameDivider(fd)%DividerWidth > 0.0d0) then
            if (FrameDivider(fd)%DividerType == DividedLite) then
              write(unit,704,advance='No') 'Frame/Divider',trim(FrameDivider(fd)%Name),  &
                                          'Divider:DividedLite',trim(Surface(surf)%Name)
            else
              write(unit,704,advance='No') 'Frame/Divider',trim(FrameDivider(fd)%Name),  &
                                          'Divider:Suspended',trim(Surface(surf)%Name)
            endif
            write(unit,7045) trim(RoundSigDigits(SurfaceWindow(surf)%DividerArea,2)),  &
               trim(RoundSigDigits(SurfaceWindow(surf)%DividerArea/Surface(surf)%Multiplier,2)),'*','N/A','N/A',  &
                             trim(RoundSigDigits(FrameDivider(fd)%DividerWidth,2)),'N/A'
          endif
        endif
      else  ! RptType=1  Vertices only
        if (Surface(surf)%BaseSurf == surf) then
          BaseSurfName=' '
        else
          BaseSurfName=Surface(surf)%BaseSurfName
        endif
        SELECT CASE (Surface(surf)%HeatTransferAlgorithm)
        CASE (HeatTransferModel_None)
          AlgoName = 'None'
        CASE (HeatTransferModel_CTF)
          AlgoName = 'CTF - ConductionTransferFunction'
        CASE (HeatTransferModel_CondFD)
          AlgoName = 'CondFD - ConductionFiniteDifference'
        CASE (HeatTransferModel_EMPD)
          AlgoName = 'EMPD - MoisturePenetrationDepthConductionTransferFunction'
        CASE (HeatTransferModel_HAMT)
          AlgoName = 'HAMT - CombinedHeatAndMoistureFiniteElement'
        CASE (HeatTransferModel_Window5)
          AlgoName = 'Window5 Detailed Fenestration'
        CASE (HeatTransferModel_ComplexFenestration)
          AlgoName = 'Window7 Complex Fenestration'
        CASE (HeatTransferModel_TDD)
          AlgoName = 'Tubular Daylighting Device'
        END SELECT
        write(unit,704,advance='No') 'HeatTransfer',trim(Surface(surf)%Name),trim(cSurfaceClass(Surface(surf)%class)),  &
                                                                                trim(BaseSurfName), trim(AlgoName)
        write(unit,7042,advance='No') trim(TrimSigDigits(Surface(surf)%Sides))
        do vert=1,Surface(surf)%Sides
          if (vert /= Surface(Surf)%Sides) then
            write(unit,709,advance='No') trim(RoundSigDigits(Surface(surf)%vertex(vert)%x,2)),  &
                                       trim(RoundSigDigits(Surface(surf)%vertex(vert)%y,2)),  &
                                       trim(RoundSigDigits(Surface(surf)%vertex(vert)%z,2))
          else
            write(unit,709)  trim(RoundSigDigits(Surface(surf)%vertex(vert)%x,2)),  &
                           trim(RoundSigDigits(Surface(surf)%vertex(vert)%y,2)),  &
                           trim(RoundSigDigits(Surface(surf)%vertex(vert)%z,2))
          endif
        enddo
        IF (Surface(surf)%Sides == 0) write(unit,711)
      endif
    enddo  ! surfaces
  enddo ! zones

  700 format('! <Zone/Shading Surfaces>,<Zone Name>/#Shading Surfaces,# Surfaces')
  701 format('! <HeatTransfer/Shading/Frame/Divider_Surface>,Surface Name,Surface Class,Base Surface,Heat Transfer Algorithm')
  7011 format(',Construction/Transmittance Schedule,Nominal U (w/o film coefs)/Min Schedule Value,',  &
             'Nominal U (with film coefs)/Max Schedule Value,Solar Diffusing,', &
             'Area (Net),Area (Gross),Area (Sunlit Calc),Azimuth,Tilt,~Width,~Height,Reveal,', &
             '<ExtBoundCondition>,<ExtConvCoeffCalc>,<IntConvCoeffCalc>,<SunExposure>,<WindExposure>,', &
             'ViewFactorToGround,ViewFactorToSky,ViewFactorToGround-IR,ViewFactorToSky-IR,#Sides')
  7012 format(',#Sides')
  702 format('! <Units>,,,,')
  7021 format(',{W/m2-K}/{},{W/m2-K}/{},{},{m2},{m2},{m2},{deg},{deg},{m},{m},{m},,,,,,,,,,')
  7022 format(',')
  703 format(A,',',A,',',I5)
  704 format(A,'_Surface,',A,',',A,',',A,',',A)
  7041 format(',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',2(A,','),A)
  7042 format(',',A)
  7044 format(',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',A,',',2(A,','))
  7045 format(',,,,,',A,',',A,',',A,',',A,',',A,',',A,',',A)
  705 format(',',A)
  706 format(4(',',A),',',A)
  7061 format(',,,,,,,,,,',A)

  707 format(',{Vertex 1},,,{Vertex 2},,,{Vertex 3},,,{Vertex 4},,,{etc}')
  708 format(4(',X {m},Y {m},Z {m}'))
  709 format(3(',',A))

  710 format(', Vertices are shown starting at Upper-Left-Corner => Counter-Clockwise => World Coordinates')
  711 format(1X)

  RETURN

END SUBROUTINE DetailsForSurfaces

SUBROUTINE CostInfoOut

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Brent Griffith
          !       DATE WRITTEN   April 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine produces a file with information about surfaces.
          ! for the purpose of producing first cost estimates to include in objective value functions
          ! for design optimization

          ! METHODOLOGY EMPLOYED:
          ! Access data in DataSurfaces and report

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataHeatBalance
  USE DataSurfaces
  USE DataInterfaces, ONLY: ShowFatalError


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
  integer unit              ! Unit number on which to write file
  integer surf              ! Loop variable for surfaces
  integer, external :: getnewunitnumber  ! External function for a new unit number
  Logical,ALLOCATABLE, DIMENSION(:) :: uniqueSurf
  integer write_stat

  if (totsurfaces > 0 .and. .not. allocated(surface)) then
    ! no error needed, probably in end processing, just return
    return
  endif

  ! need to determine unique surfacs... some surfaces are shared by zones and hence doubled
  Allocate(uniqueSurf(TotSurfaces))
  uniqueSurf = .true.

  do surf=1, TotSurfaces
    If (surface(surf)%ExtBoundCond > 0) then
        If (surface(surf)%ExtBoundCond < surf) then !already cycled through
          uniqueSurf(surf) = .false.

        endif

    endif
    If ( surface(surf)%Construction == 0) then  !throw out others for now
         uniqueSurf(surf) = .false.
    endif
  enddo

  unit=getnewunitnumber()
  ! .sci = surface cost info
  open(unit,file='eplusout.sci', Action='write', iostat=write_stat)
  if (write_stat /= 0) then
   CALL ShowFatalError('CostInfoOut: Could not open file "eplusout.sci" for output (write).')
  endif
  write(unit, *)  totsurfaces , count(uniqueSurf)
  write(unit, *) 'data for surfaces useful for cost information'
  write(unit, *) 'Number, Name, Construction, class, area, grossarea'

  do surf=1,TotSurfaces
    !if (surface(surf)%class .eq. SurfaceClass_IntMass) CYCLE
    If (.not. uniqueSurf(surf)) cycle
    ! why the heck are constructions == 0 ?
    If ( surface(surf)%Construction /= 0) then
      write(unit, 801) surf, trim(surface(surf)%name), trim(Construct(surface(surf)%Construction)%name), &
            trim(cSurfaceClass(surface(surf)%class)), surface(surf)%Area, surface(surf)%GrossArea
    endif

  enddo

  801 format(I5,',',A,',',A,',',A, ',',f14.5,',',f14.5 )

  close(unit)

  deallocate(uniqueSurf)
  return

END SUBROUTINE CostInfoOut

SUBROUTINE VRMLOut(PolygonAction,ColorScheme)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Linda K. Lawrie
          !       DATE WRITTEN   August 2006
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine produces a file of VRML output for the surfaces.

          ! METHODOLOGY EMPLOYED:
          ! Use the surface absolute coordinate information to produce
          ! lines.

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataPrecisionGlobals
  USE DataHeatBalance, ONLY: BuildingName,Zone
  USE DataSurfaces
  USE DataDaylighting, ONLY: ZoneDaylight
  USE DataGlobals, ONLY: DegToRadians,NumOfZones
  USE DataInterfaces, ONLY: ShowWarningError, ShowContinueError, ShowFatalError
  USE DataStringGlobals, ONLY: VerString
  USE DXFEarClipping

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  CHARACTER(len=*) :: PolygonAction
  CHARACTER(len=*) :: ColorScheme

          ! SUBROUTINE PARAMETER DEFINITIONS:
  character(len=*), parameter, dimension(7) :: colorstring=  &
        (/'WALL      ',  &
          'WINDOW    ',  &
          'FIXEDSHADE',  &
          'SUBSHADE  ',  &
          'ROOF      ',  &
          'FLOOR     ',  &
          'BLDGSHADE '/)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  integer unit              ! Unit number on which to write file
  integer surf              ! Loop variable for surfaces
  integer vert              ! Loop counter
  integer,external :: getnewunitnumber  ! External function for a new unit number
  integer colorindex        ! color index by surface type
!  REAL(r64) minx                 ! minimum x in surface data
!  REAL(r64) miny                 ! minimum y in surface data
!  REAL(r64) minz                 ! minimum z in surface data (for polygon output)
  integer zones             ! loop counter for zone loop
  character(len=25) zonenum
  character(len=MaxNameLength) TempZoneName
  integer pos
  character(len=25) ShadeType
  logical :: ThickPolyline=.false.
  logical :: RegularPolyline=.false.
  character(len=5) :: PolylineWidth=' 0.55'
  logical :: TriangulateFace=.false.
  integer :: ntri
  integer :: svert
  type (dTriangle), allocatable, dimension(:) :: mytriangles
  integer :: vv0
  integer :: vv1
  integer :: vv2
  character(len=25) :: csurfnumber
  character(len=25) :: csidenumber
  integer write_stat

  IF (PolygonAction == 'TRIANGULATE3DFACE' .or. PolygonAction == 'TRIANGULATE') THEN
    TriangulateFace=.true.
  ELSEIF (PolygonAction == 'THICKPOLYLINE' .or. PolygonAction == ' ') THEN
    ThickPolyline=.true.
  ELSEIF (PolygonAction == 'REGULARPOLYLINE') THEN
    RegularPolyline=.true.
    PolylineWidth=' 0'
  ELSE
    CALL ShowWarningError('VRMLOut: Illegal key specified for Surfaces with > 4 sides='//TRIM(PolygonAction))
    CALL ShowContinueError('"TRIANGULATE 3DFACE" will be used for any surfaces with > 4 sides.')
    TriangulateFace=.true.
  ENDIF

  if (totsurfaces > 0 .and. .not. allocated(surface)) then
    ! no error needed, probably in end processing, just return
    return
  endif

  unit=getnewunitnumber()
  open(unit,file='eplusout.wrl', Action='write', iostat=write_stat)
  if (write_stat /= 0) then
   CALL ShowFatalError('VRMLOut: Could not open file "eplusout.wrl" for output (write).')
  endif


  write(unit,702)   ! Beginning
  702 format('#VRML V2.0 utf8')

  if (ColorScheme == ' ') then
    write(unit,707) TRIM(BuildingName),TRIM(VerString),'Default'   ! World Info
  else
    write(unit,707) TRIM(BuildingName),TRIM(VerString),TRIM(ColorScheme)   ! World Info
  endif
  707 format('WorldInfo {',/,3X,'title "Building - ',A,'"',/,      &
              3X,'info ["EnergyPlus Program Version ',A,'"]',/,    &
              3X,'info ["Surface Color Scheme ',A,'"]',/,    &
             '}')

  write(unit,710) '# Zone Names'
  do zones=1,NumOfZones
    write(zonenum,*) zones
    zonenum=adjustl(zonenum)
    TempZoneName=Zone(Zones)%Name
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    END DO
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    END DO
    write(unit,710) '# Zone='//trim(zonenum)//':'//trim(TempZoneName)
  enddo

  ! Define the colors:
  800 Format ('Shape {',/,'appearance DEF ',A,' Appearance {',/,   &
       'material Material { diffuseColor ',A,' }',/,'}',/,'}')

  write(unit,800) 'FLOOR','0.502 0.502 0.502'
  write(unit,800) 'ROOF','1 1 0'
  write(unit,800) 'WALL','0 1 0'
  write(unit,800) 'WINDOW','0 1 1'
  write(unit,800) 'DOOR','0 1 1'
  write(unit,800) 'GLASSDOOR','0 1 1'
  write(unit,800) 'FIXEDSHADE','1 0 1'
  write(unit,800) 'BLDGSHADE','0 0 1'
  write(unit,800) 'SUBSHADE','1 0 1'
  write(unit,800) 'BACKCOLOR','0.502 0.502 0.784'

  801 Format('Shape {',/,'appearance USE ',A,/,       &
    'geometry IndexedFaceSet {',/,                    &
    'solid TRUE',/,                                   &
    'coord DEF ',A,' Coordinate {',/,                 &
    'point [')

  802 Format(F15.5,1X,F15.5,1X,F15.5,',')

  803 Format(']',/,'}',/,'coordIndex [')

  804 Format(A)

  805 Format(']',/,'ccw TRUE',/,'solid TRUE',/,'}',/,'}')


  !  Do all detached shading surfaces first
  do surf=1,totsurfaces
    if (surface(surf)%heattranssurf) CYCLE
    if (surface(surf)%class == SurfaceClass_Shading) CYCLE
    if (surface(surf)%sides == 0) CYCLE
    if (surface(surf)%class .eq. SurfaceClass_Detached_F) colorindex=3
    if (surface(surf)%class .eq. SurfaceClass_Detached_B) colorindex=7
    if (surface(surf)%class .eq. SurfaceClass_Detached_F) then
      ShadeType='Fixed Shading'
      write(unit,710) '# Fixed Shading:'//trim(surface(surf)%Name)
    elseif (surface(surf)%class .eq. SurfaceClass_Detached_B) then
      ShadeType='Building Shading'
      write(unit,710) '# Building Shading:'//trim(surface(surf)%Name)
    endif
    write(csurfnumber,*) surf
    csurfnumber=adjustl(csurfnumber)
    write(unit,801) trim(colorstring(colorindex)),'Surf'//TRIM(csurfnumber)
    write(unit,802) (surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y, &
                    surface(surf)%vertex(vert)%z,vert=1,surface(surf)%sides)
    write(unit,803)
    if (surface(surf)%sides <= 4 .or. .not. TriangulateFace) then
      do vert=1,surface(surf)%sides
        write(csidenumber,*) vert-1
        csidenumber=adjustl(csidenumber)
        write(unit,804,advance='No') ' '//trim(csidenumber)
        if (vert == surface(surf)%sides) write(unit,804) ' -1'
      enddo
      write(unit,805)
    else  ! will be >4 sided polygon with triangulate option
      ntri=triangulate(surface(surf)%sides,surface(surf)%vertex,mytriangles,surface(surf)%azimuth,  &
                  surface(surf)%tilt,surface(surf)%name,surface(surf)%class)
      do svert=1,ntri
        vv0=mytriangles(svert)%vv0
        write(csidenumber,*) vv0-1
        csidenumber=adjustl(csidenumber)
        write(unit,804,advance='No') ' '//trim(csidenumber)
        vv1=mytriangles(svert)%vv1
        write(csidenumber,*) vv1-1
        csidenumber=adjustl(csidenumber)
        write(unit,804,advance='No') ' '//trim(csidenumber)
        vv2=mytriangles(svert)%vv2
        write(csidenumber,*) vv2-1
        csidenumber=adjustl(csidenumber)
        write(unit,804,advance='No') ' '//trim(csidenumber)
        write(unit,804) ' -1'
      enddo
      write(unit,805)
      deallocate(mytriangles)
    endif
  enddo
!
!  ! now do zone surfaces, by zone
  do zones=1,NumOfZones
    TempZoneName=Zone(Zones)%Name
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),' ')
    END DO
    pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    DO WHILE (pos /= 0)
      TempZoneName(pos:pos)='_'
      pos=INDEX(TempZoneName(1:LEN_TRIM(TempZoneName)),':')
    END DO
    do surf=max(zone(zones)%surfacefirst,1),zone(zones)%surfacelast
      if (surface(surf)%sides == 0) CYCLE
      if (surface(surf)%class .eq. SurfaceClass_IntMass) CYCLE
      if (surface(surf)%class .eq. SurfaceClass_Wall) colorindex=1
      if (surface(surf)%class .eq. SurfaceClass_Roof) colorindex=5
      if (surface(surf)%class .eq. SurfaceClass_TDD_Dome) colorindex=2
      if (surface(surf)%class .eq. SurfaceClass_Floor) colorindex=6
      if (surface(surf)%class .eq. SurfaceClass_Window) colorindex=2
      if (surface(surf)%class .eq. SurfaceClass_Door) colorindex=2
!
      write(csurfnumber,*) surf
      csurfnumber=adjustl(csurfnumber)
      write(unit,710) '# '//trim(surface(surf)%ZoneName)//':'//trim(surface(surf)%Name)
      write(unit,801) trim(colorstring(colorindex)),'Surf'//TRIM(csurfnumber)
      write(unit,802) (surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y, &
                      surface(surf)%vertex(vert)%z,vert=1,surface(surf)%sides)
      write(unit,803)
      if (surface(surf)%sides <= 4 .or. .not. TriangulateFace) then
        do vert=1,surface(surf)%sides
          write(csidenumber,*) vert-1
          csidenumber=adjustl(csidenumber)
          write(unit,804,advance='No') ' '//trim(csidenumber)
          if (vert == surface(surf)%sides) write(unit,804) ' -1'
        enddo
        write(unit,805)
      else  ! will be >4 sided polygon with triangulate option
        ntri=triangulate(surface(surf)%sides,surface(surf)%vertex,mytriangles,surface(surf)%azimuth,  &
                    surface(surf)%tilt,surface(surf)%name,surface(surf)%class)
        do svert=1,ntri
          vv0=mytriangles(svert)%vv0
          write(csidenumber,*) vv0-1
          csidenumber=adjustl(csidenumber)
          write(unit,804,advance='No') ' '//trim(csidenumber)
          vv1=mytriangles(svert)%vv1
          write(csidenumber,*) vv1-1
          csidenumber=adjustl(csidenumber)
          write(unit,804,advance='No') ' '//trim(csidenumber)
          vv2=mytriangles(svert)%vv2
          write(csidenumber,*) vv2-1
          csidenumber=adjustl(csidenumber)
          write(unit,804,advance='No') ' '//trim(csidenumber)
          write(unit,804) ' -1'
        enddo
        write(unit,805)
        deallocate(mytriangles)
      endif
    enddo
    ! still have to do shading surfaces for zone
    colorindex=4
    do surf=1,totsurfaces
!      !if (surface(surf)%heattranssurf) CYCLE ! Shading with a construction is allowed to be HT surf for daylighting shelves
      if (surface(surf)%class .ne. SurfaceClass_Shading) CYCLE
      if (surface(surf)%zonename /= zone(zones)%Name) CYCLE
      if (surface(surf)%sides == 0) CYCLE
      write(unit,710) '# '//trim(surface(surf)%ZoneName)//':'//trim(surface(surf)%Name)
      write(unit,801) trim(colorstring(colorindex)),'Surf'//TRIM(csurfnumber)
      write(unit,802) (surface(surf)%vertex(vert)%x,surface(surf)%vertex(vert)%y, &
                      surface(surf)%vertex(vert)%z,vert=1,surface(surf)%sides)
      write(unit,803)
      if (surface(surf)%sides <= 4 .or. .not. TriangulateFace) then
        do vert=1,surface(surf)%sides
          write(csidenumber,*) vert-1
          csidenumber=adjustl(csidenumber)
          write(unit,804,advance='No') ' '//trim(csidenumber)
          if (vert == surface(surf)%sides) write(unit,804) ' -1'
        enddo
        write(unit,805)
      else  ! will be >4 sided polygon with triangulate option
        ntri=triangulate(surface(surf)%sides,surface(surf)%vertex,mytriangles,surface(surf)%azimuth,  &
                    surface(surf)%tilt,surface(surf)%name,surface(surf)%class)
        do svert=1,ntri
          vv0=mytriangles(svert)%vv0
          write(csidenumber,*) vv0-1
          csidenumber=adjustl(csidenumber)
          write(unit,804,advance='No') ' '//trim(csidenumber)
          vv1=mytriangles(svert)%vv1
          write(csidenumber,*) vv1-1
          csidenumber=adjustl(csidenumber)
          write(unit,804,advance='No') ' '//trim(csidenumber)
          vv2=mytriangles(svert)%vv2
          write(csidenumber,*) vv2-1
          csidenumber=adjustl(csidenumber)
          write(unit,804,advance='No') ' '//trim(csidenumber)
          write(unit,804) ' -1'
        enddo
        write(unit,805)
        deallocate(mytriangles)
      endif
    enddo
  enddo

! vrml does not have daylighting reference points included

  710 format(A)

  close(unit)

  return

END SUBROUTINE VRMLOut


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

