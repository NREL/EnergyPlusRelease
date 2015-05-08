MODULE DaylightingDevices

          ! MODULE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   May 2003
          !       MODIFIED       PGE, Aug 2003:  Added daylighting shelves.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS MODULE:
          ! Simulates daylighting devices, namely tubular daylighting devices (a.k.a. light pipes, sun pipes, or
          ! tubular skylights) and daylighting shelves (a.k.a. light shelves).

          ! METHODOLOGY EMPLOYED:
          !
          ! TUBULAR DAYLIGHTING DEVICE
          ! A tubular daylighting device (TDD) is constructed of three components:  a dome, a pipe, and a diffuser.
          ! The dome and diffuser are treated as special window surfaces to take advantage of many of the already
          ! existing daylighting and heat transfer routines.  Together the dome and diffuser become "receiver"
          ! and "transmitter", i.e. radiation entering the dome ends up exiting the diffuser.  The geometry and
          ! construction of the pipe and the constructions of the window surfaces determine the transmittance of
          ! the TDD.
          !
          ! The main task of the module is to determine the total transmittance of the TDD for several
          ! types of radiation, including visible beam, solar beam, solar isotropic, and solar anisotropic sky.
          ! The fundamental building block for each type of radiation is the transmittance of a beam or ray of
          ! radiation (visible or solar) at a given incident angle.  This transmittance is calculated and
          ! tabulated for each TDD during initialization using a numerical integral based on the analytical
          ! solution derived by Swift and Smith.  Diffuse transmittances are subsequently calculated by integrating
          ! discrete rays over the viewable area.
          !
          ! There are three parts to the TDD model:
          !   1. Daylighting
          !   2. Solar gain
          !   3. Thermal conductive/convective gain
          !
          ! The daylighting simulation uses the visible beam transmittance to find the amount of direct beam
          ! solar illuminance that enters the zone.  The visible beam transmittance is also used for calculating
          ! the contribution of each discrete ray from a differential area during a comprehensive sky/ground
          ! integration.
          !
          ! The heat balance simulation handles both the solar gain and thermal conductive/convective gain.
          ! Although visible and solar radiation are similar, solar gain is simulated very differently from the
          ! daylighting illuminance calculations.  The gain from direct beam solar is found using the
          ! solar beam transmittance.  The diffuse solar, however, is more complicated.  A sky/ground integration
          ! is NOT performed.  Instead anisotropic sky view factor multipliers (AnisoSkyMult) are calculated for
          ! each surface.  The diffuse sky/ground transmittance of the TDD is solved using a modification of the
          ! AnisoSkyMult.  The ground radiation transmittance and anisotropic sky transmittance are found separately.
          ! See CalcTDDTransSolIso, CalcTDDTransSolHorizon, CalcTDDTransSolAniso below.
          !
          ! For thermal conductive/convective gain, TDDs are treated as one big object with an effective R value.
          ! The outside face temperature of the dome and the inside face temperature of the diffuser are calculated
          ! with the outside and inside heat balances respectively.  The temperatures are then copied to the inside
          ! face of the dome and the outside face of the diffuser.  Normal exterior and interior convection and IR
          ! radiation exchange occurs for both surfaces.
          !
          ! Solar radiation that is not transmitted through the pipe is absorbed and distributed among the transition
          ! zones that the pipe passes through between dome and diffuser.  The heat is distributed proportionate to
          ! the length of the zone.  Any exterior length of pipe also receives a proportionate amount of heat, but
          ! this is lost to the outside.
          !
          ! REFERENCES:
          ! Ellis, P. G., and Strand, R. K.  Paper to be published.
          ! Swift, P. D., and Smith, G. B.  "Cylindrical Mirror Light Pipes",
          !   Solar Energy Materials and Solar Cells 36 (1995), pp. 159-168.
          !
          !
          ! DAYLIGHTING SHELVES
          ! A daylighting shelf is constructed of up to three components: a window, an inside shelf, and an outside
          ! shelf.  Both inside shelf and outside shelf are optional, but if neither is specified, nothing happens.
          ! The window must be divided into two window surfaces: an upper window and a lower window.  The upper
          ! window interacts with the daylighting shelf but the lower window does not, except to receive shading from
          ! the outside shelf.  The inside shelf, if specified, acts to reflect all transmitted light from the
          ! upper window onto the ceiling of the zone as diffuse light.  The outside shelf, if specified, changes
          ! the total amount of light incident on the window.  All light reflected from the outside shelf also goes
          ! onto the zone ceiling.
          !
          ! Most of the work for daylighting shelves is actually done in DaylightingManager.f90, SolarShading.f90,
          ! and HeatBalanceSurfaceManager.f90.  The main task of the module is to get the input and initialize the
          ! shelf.  The biggest part of initialization is calculating the window view factor to the outside shelf.
          ! It is up to the user to reduce the window view factor to ground accordingly.
          !
          ! The inside shelf is modeled in both daylighting and heat balance simulations by converting all light
          ! transmitted by the upper window into diffuse upgoing flux.  No beam or downgoing flux can pass the end
          ! of the shelf regardless of the shelf's position or orientation.  Since it is defined as a partition,
          ! the inside shelf is essentially the same as an internal mass surface.  The initialization doubles the
          ! surface area so that both sides are exposed to the zone air.  All beam solar transmitted by the window
          ! is absorbed in one side of the shelf, i.e. half of the doubled area.
          !
          ! The outside shelf is modeled in the daylighting simulation after the detailed sky/ground integration has
          ! been completed.  Since exterior surfaces currently do not reflect or have a luminance in the Daylighting
          ! Manager, the shelf just serves to block part of the ground luminance.  The luminance of the shelf itself
          ! is added as a lump sum based on the view factor to the shelf, the sunlit fraction, the reflectance of the
          ! shelf construction, and the sun and sky illuminance on the shelf.  All the luminance is added to the
          ! diffuse upgoing flux.  The shelf view factor to sky is assumed to be 1.0 for lack of better information.
          !
          ! The outside shelf is treated similarly in the heat balance simulation, but here the shelf view factor to
          ! sky is conveniently given by AnisoSkyMult.  NOTE:  The solar shading code was modified to allow sunlit
          ! fraction, sunlit area, AnisoSkyMult, etc. to be calculated for attached shading surfaces.
          !
          ! Future shelf model improvements:
          ! 1. Allow beam and downgoing flux to pass the end of the inside shelf depending on actual shelf goemetry.
          ! 2. Reduce outside shelf view factor to sky (for daylighting) by taking into account anisotropic sky
          !    distribution and shading, i.e. the daylighting equivalent of AnisoSkyMult.
          ! 3. Expand view factor to shelf calculation to handle more complicated geometry.
          !
          ! REFERENCES:
          ! Mills, A. F.  Heat and Mass Transfer, 1995, p. 499.  (Shape factor for adjacent rectangles.)

          ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals,     ONLY: NumOfZones, MaxNameLength, DegToRadians, Pi, PiOvr2, OutputFileInits
USE DataInterfaces,  ONLY: SetupOutputVariable, ShowWarningError, ShowFatalError, ShowSevereError, ShowContinueError
USE DataHeatBalance, ONLY: Zone, Construct, TotConstructs, SolarDistribution, MinimalShadowing
USE DataSurfaces,    ONLY: Surface, TotSurfaces, SurfaceWindow, ExternalEnvironment, CalcSolRefl, SurfaceClass_Window,  &
                           SurfaceClass_Shading,SurfaceClass_TDD_Dome,SurfaceClass_TDD_Diffuser, ShadingTransmittanceVaries
USE DataDaylightingDevices

IMPLICIT NONE ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

          ! MODULE PARAMETER DEFINITIONS: na
          ! DERIVED TYPE DEFINITIONS: na
          ! MODULE VARIABLE TYPE DECLARATIONS: na

          ! MODULE VARIABLE DECLARATIONS:
REAL(r64), DIMENSION(NumOfAngles) :: COSAngle ! List of cosines of incident angle

          ! SUBROUTINE SPECIFICATIONS:
PUBLIC InitDaylightingDevices
PUBLIC FindTDDPipe
PUBLIC TransTDD
PUBLIC DistributeTDDAbsorbedSolar
PRIVATE GetTDDInput
PRIVATE GetShelfInput
PRIVATE CalcPipeTransBeam
PRIVATE CalcTDDTransSolIso
PRIVATE CalcTDDTransSolHorizon
PRIVATE CalcTDDTransSolAniso
PRIVATE InterpolatePipeTransBeam
PRIVATE CalcViewFactorToShelf
PUBLIC  FigureTDDZoneGains

CONTAINS

          ! MODULE SUBROUTINES:
SUBROUTINE InitDaylightingDevices

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   May 2003
          !       MODIFIED       PGE, Aug 2003:  Added daylighting shelves.
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! This subroutine initializes all daylighting device:  TDD pipes and daylighting shelves.
          ! This is only called once at the beginning of the simulation under the BeginSimFlag.

          ! METHODOLOGY EMPLOYED:
          ! Daylighting and thermal variables are calculated.  BeamTrans/COSAngle table is calculated.

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE General, ONLY: RoundSigDigits
  USE DataHeatBalance, ONLY : IntGainTypeOf_DaylightingDeviceTubular
  USE DataInterfaces

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS: na

          ! DERIVED TYPE DEFINITIONS:
  TYPE TDDPipeStoredData
    REAL(r64)                    :: AspectRatio = 0.0d0 ! Aspect ratio, length / diameter
    REAL(r64)                    :: Reflectance = 0.0d0 ! Reflectance of surface
    REAL(r64), DIMENSION(NumOfAngles) :: TransBeam = 0.0d0   ! Table of beam transmittance vs. cosine angle
  END TYPE TDDPipeStoredData

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER          :: PipeNum            ! TDD pipe object number
  INTEGER          :: StoredNum          ! Stored TDD pipe object number
  INTEGER          :: AngleNum
  INTEGER          :: TZoneNum
  INTEGER          :: Loop
  REAL(r64)        :: Theta              ! Angle of entry in degrees, 0 is parallel to pipe axis
  REAL(r64)        :: dTheta             ! Angle increment
  REAL(r64)        :: Reflectance        ! Visible or solar reflectance of surface
  REAL(r64)        :: SumTZoneLengths
  LOGICAL          :: Found
  TYPE (TDDPipeStoredData), ALLOCATABLE, DIMENSION(:) :: TDDPipeStored
  INTEGER          :: ShelfNum           ! Daylighting shelf object number
  INTEGER          :: ShelfSurf          ! Daylighting shelf surface number
  INTEGER          :: WinSurf            ! Window surface number

  INTEGER          :: NumStored = 0      ! Counter for number of pipes stored as they are calculated
  LOGICAL          :: ShelfReported=.false.

          ! FLOW:
  ! Initialize tubular daylighting devices (TDDs)
  CALL GetTDDInput

  IF (NumOfTDDPipes > 0) THEN
    CALL DisplayString('Initializing Tubular Daylighting Devices')
    ! Setup COSAngle list for all TDDs
    COSAngle(1) = 0.0d0
    COSAngle(NumOfAngles) = 1.0d0

    dTheta = 90.0d0 * DegToRadians / (NumOfAngles - 1.0d0)
    Theta = 90.0d0 * DegToRadians
    DO AngleNum = 2, NumOfAngles - 1
      Theta = Theta - dTheta
      COSAngle(AngleNum) = COS(Theta)
    END DO ! AngleNum

    ALLOCATE(TDDPipeStored(NumOfTDDPipes * 2))

    DO PipeNum = 1, NumOfTDDPipes
      ! Initialize optical properties
      TDDPipe(PipeNum)%AspectRatio = TDDPipe(PipeNum)%TotLength/TDDPipe(PipeNum)%Diameter
      TDDPipe(PipeNum)%ReflectVis = 1.0d0 - Construct(TDDPipe(PipeNum)%Construction)%InsideAbsorpVis
      TDDPipe(PipeNum)%ReflectSol = 1.0d0 - Construct(TDDPipe(PipeNum)%Construction)%InsideAbsorpSolar

      ! Calculate the beam transmittance table for visible and solar spectrum
      ! First time thru use the visible reflectance
      Reflectance = TDDPipe(PipeNum)%ReflectVis
      DO Loop = 1, 2
        ! For computational efficiency, search stored pipes to see if an identical pipe has already been calculated
        Found = .FALSE.
        DO StoredNum = 1, NumStored
          IF (TDDPipeStored(StoredNum)%AspectRatio .NE. TDDPipe(PipeNum)%AspectRatio) CYCLE
          IF (TDDPipeStored(StoredNum)%Reflectance .EQ. Reflectance) THEN
            Found = .TRUE. ! StoredNum points to the matching TDDPipeStored
            EXIT
          END IF
        END DO ! StoredNum

        IF (.NOT. Found) THEN ! Not yet calculated

          ! Add a new pipe to TDDPipeStored
          NumStored = NumStored + 1
          TDDPipeStored(NumStored)%AspectRatio = TDDPipe(PipeNum)%AspectRatio
          TDDPipeStored(NumStored)%Reflectance = Reflectance

          ! Set beam transmittances for 0 and 90 degrees
          TDDPipeStored(NumStored)%TransBeam(1) = 0.0d0
          TDDPipeStored(NumStored)%TransBeam(NumOfAngles) = 1.0d0

          ! Calculate intermediate beam transmittances between 0 and 90 degrees
          Theta = 90.0d0 * DegToRadians
          DO AngleNum = 2, NumOfAngles - 1
            Theta = Theta - dTheta
            TDDPipeStored(NumStored)%TransBeam(AngleNum) = CalcPipeTransBeam(Reflectance, TDDPipe(PipeNum)%AspectRatio, Theta)
          END DO ! AngleNum

          StoredNum = NumStored
        END IF

        ! Assign stored values to TDDPipe
        IF (Loop .EQ. 1)  THEN ! Visible
          TDDPipe(PipeNum)%PipeTransVisBeam = TDDPipeStored(StoredNum)%TransBeam
        ELSE ! Solar
          TDDPipe(PipeNum)%PipeTransSolBeam = TDDPipeStored(StoredNum)%TransBeam
        END IF

        ! Second time thru use the solar reflectance
        Reflectance = TDDPipe(PipeNum)%ReflectSol
      END DO ! Loop

      ! Calculate the solar isotropic diffuse and horizon transmittances.  These values are constant for a given TDD.
      TDDPipe(PipeNum)%TransSolIso = CalcTDDTransSolIso(PipeNum)
      TDDPipe(PipeNum)%TransSolHorizon = CalcTDDTransSolHorizon(PipeNum)

      ! Initialize thermal properties
      SumTZoneLengths= 0.0d0
      DO TZoneNum = 1, TDDPipe(PipeNum)%NumOfTZones
        SumTZoneLengths = SumTZoneLengths + TDDPipe(PipeNum)%TZoneLength(TZoneNum)

        CALL SetupZoneInternalGain(TDDPipe(PipeNum)%TZone(TZoneNum), &
                                   'DaylightingDevice:Tubular', &
                                   TDDPipe(PipeNum)%Name, &
                                   IntGainTypeOf_DaylightingDeviceTubular, &
                                   ConvectionGainRate = TDDPipe(PipeNum)%TZoneHeatGain(TZoneNum) )

      END DO ! TZoneNum

      TDDPipe(PipeNum)%ExtLength = TDDPipe(PipeNum)%TotLength - SumTZoneLengths

      ! Setup report variables: CurrentModuleObject='DaylightingDevice:Tubular'
      CALL SetupOutputVariable('Tubular Daylighting Device Transmitted Solar Radiation Rate [W]', &
        TDDPipe(PipeNum)%TransmittedSolar,'Zone','Average', TDDPipe(PipeNum)%Name)
      CALL SetupOutputVariable('Tubular Daylighting Device Pipe Absorbed Solar Radiation Rate [W]', &
        TDDPipe(PipeNum)%PipeAbsorbedSolar,'Zone','Average', TDDPipe(PipeNum)%Name)
      CALL SetupOutputVariable('Tubular Daylighting Device Heat Gain Rate [W]',   &
         TDDPipe(PipeNum)%HeatGain,'Zone','Average', TDDPipe(PipeNum)%Name)
      CALL SetupOutputVariable('Tubular Daylighting Device Heat Loss Rate [W]',   &
         TDDPipe(PipeNum)%HeatLoss,'Zone','Average', TDDPipe(PipeNum)%Name)

      CALL SetupOutputVariable('Tubular Daylighting Device Beam Solar Transmittance []', &
        TDDPipe(PipeNum)%TransSolBeam,'Zone','Average', TDDPipe(PipeNum)%Name)
      CALL SetupOutputVariable('Tubular Daylighting Device Beam Visible Transmittance []', &
        TDDPipe(PipeNum)%TransVisBeam,'Zone','Average', TDDPipe(PipeNum)%Name)
      CALL SetupOutputVariable('Tubular Daylighting Device Diffuse Solar Transmittance []', &
        TDDPipe(PipeNum)%TransSolDiff,'Zone','Average',TDDPipe(PipeNum)%Name)
      CALL SetupOutputVariable('Tubular Daylighting Device Diffuse Visible Transmittance []', &
        TDDPipe(PipeNum)%TransVisDiff,'Zone','Average',TDDPipe(PipeNum)%Name)

    END DO ! PipeNum

    DEALLOCATE(TDDPipeStored)
  END IF

  ! Initialize daylighting shelves
  CALL GetShelfInput

  IF(NumOfShelf > 0) CALL DisplayString('Initializing Light Shelf Daylighting Devices')

  DO ShelfNum = 1, NumOfShelf
    WinSurf = Shelf(ShelfNum)%Window

    ShelfSurf = Shelf(ShelfNum)%InSurf
    IF (ShelfSurf > 0) THEN
      ! Double surface area so that both sides of the shelf are treated as internal mass
      Surface(ShelfSurf)%Area = 2.0d0* Surface(ShelfSurf)%Area
    END IF

    ShelfSurf = Shelf(ShelfNum)%OutSurf
    IF (ShelfSurf > 0) THEN
      Shelf(ShelfNum)%OutReflectVis = 1.0d0 - Construct(Shelf(ShelfNum)%Construction)%OutsideAbsorpVis
      Shelf(ShelfNum)%OutReflectSol = 1.0d0 - Construct(Shelf(ShelfNum)%Construction)%OutsideAbsorpSolar

      IF (Shelf(ShelfNum)%ViewFactor < 0) CALL CalcViewFactorToShelf(ShelfNum)

      IF (Shelf(ShelfNum)%ViewFactor + Surface(WinSurf)%ViewFactorSky + Surface(WinSurf)%ViewFactorGround > 1.0d0) THEN
        CALL ShowWarningError('DaylightingDevice:Shelf = '//TRIM(Shelf(ShelfNum)%Name)// &
          ':  Window view factors to sky ['//trim(RoundSigDigits(Surface(WinSurf)%ViewFactorSky,2))//'],')
        CALL ShowContinueError('ground ['//trim(RoundSigDigits(Surface(WinSurf)%ViewFactorGround,2))//  &
          '], and outside shelf ['//trim(RoundSigDigits(Shelf(ShelfNum)%ViewFactor,2))//'] add up to > 1.0.')
      ENDIF

      ! Report calculated view factor so that user knows what to make the view factor to ground
      IF (.not. ShelfReported) THEN
        Write(OutputFileInits,'(A)')   &
         '! <Shelf Details>,Name,View Factor to Outside Shelf,Window Name,Window View Factor to Sky,Window View Factor to Ground'
        ShelfReported=.true.
      ENDIF
      Write(OutputFileInits,'(A)') trim(Shelf(ShelfNum)%Name)//','//trim(RoundSigDigits(Shelf(ShelfNum)%ViewFactor,2))//','// &
        trim(Surface(WinSurf)%Name)//','//trim(RoundSigDigits(Surface(WinSurf)%ViewFactorSky,2))//','// &
        trim(RoundSigDigits(Surface(WinSurf)%ViewFactorGround,2))
!      CALL SetupOutputVariable('View Factor To Outside Shelf []', &
!        Shelf(ShelfNum)%ViewFactor,'Zone','Average',Shelf(ShelfNum)%Name)
    END IF
  END DO

  ! Warning that if Calculate Solar Reflection From Exterior Surfaces = Yes in Building input, then
  ! solar reflection calculated from obstructions will not be used in daylighting shelf or tubular device
  ! calculation

  IF(CalcSolRefl .AND. (NumOfTDDPipes > 0 .OR. NumOfShelf > 0)) THEN
    CALL ShowWarningError('InitDaylightingDevices: Solar Distribution Model includes Solar Reflection calculations;')
    CALL ShowContinueError('the resulting reflected solar values will not be used in the')
    CALL ShowContinueError('DaylightingDevice:Shelf or DaylightingDevice:Tubular calculations.')
  END IF

  RETURN

END SUBROUTINE InitDaylightingDevices


SUBROUTINE GetTDDInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   May 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Gets the input for TDD pipes and does some error checking.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE DataIPShortCuts
  USE InputProcessor, ONLY: GetNumObjectsFound, FindItemInList, GetObjectItem, VerifyName
  USE DataDaylighting, ONLY: ZoneDaylight
  USE General, ONLY: RoundSigDigits, SafeDivide

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS: na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
!unused1208  CHARACTER(len=MaxNameLength), &
!                   DIMENSION(20) :: Alphas                ! Alpha items for object
  LOGICAL                        :: ErrorsFound = .FALSE. ! Set to true if errors in input, fatal at end of routine
  INTEGER                        :: IOStatus              ! Used in GetObjectItem
  LOGICAL                        :: IsBlank               ! TRUE if the name is blank
  LOGICAL                        :: IsNotOk               ! TRUE if there was a problem with a list name
!unused1208  REAL(r64), DIMENSION(9)             :: Numbers               ! Numeric items for object
  INTEGER                        :: NumAlphas             ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers            ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: PipeNum               ! TDD pipe object number
  INTEGER                        :: SurfNum               ! Dome or diffuser surface
  INTEGER                        :: TZoneNum              ! Transition zone loop
  CHARACTER(len=MaxNameLength)   :: TZoneName             ! Transition zone name
  REAL(r64)                      :: PipeArea

          ! FLOW:
  cCurrentModuleObject='DaylightingDevice:Tubular'
  NumOfTDDPipes = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumOfTDDPipes > 0) THEN
    ALLOCATE(TDDPipe(NumOfTDDPipes))

    DO PipeNum = 1, NumOfTDDPipes
      CALL GetObjectItem(cCurrentModuleObject,PipeNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      ! Pipe name
      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(cAlphaArgs(1),TDDPipe%Name,PipeNum-1,IsNotOK,IsBlank,TRIM(cCurrentModuleObject)//' Name')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
      END IF
      TDDPipe(PipeNum)%Name = cAlphaArgs(1)

      ! Get TDD:DOME object
      SurfNum = FindItemInList(cAlphaArgs(2),Surface%Name,TotSurfaces)

      IF (SurfNum == 0) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
          ':  Dome '//TRIM(cAlphaArgs(2))//' not found.')
        ErrorsFound = .TRUE.
      ELSE
        IF (FindTDDPipe(SurfNum) > 0) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Dome '//TRIM(cAlphaArgs(2))//' is referenced by more than one TDD.')
          ErrorsFound = .TRUE.
        END IF

        IF (Surface(SurfNum)%Class .NE. SurfaceClass_TDD_Dome) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Dome '//TRIM(cAlphaArgs(2))//' is not of surface type TubularDaylightDome.')
          ErrorsFound = .TRUE.
        END IF

        IF (Construct(Surface(SurfNum)%Construction)%TotGlassLayers > 1) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Dome '//TRIM(cAlphaArgs(2))//' construction ('//  &
            trim(Construct(Surface(SurfNum)%Construction)%Name)//') must have only 1 glass layer.')
          ErrorsFound = .TRUE.
        END IF

        IF (Surface(SurfNum)%WindowShadingControlPtr > 0) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Dome '//TRIM(cAlphaArgs(2))//' must not have a shading control.')
          ErrorsFound = .TRUE.
        END IF

        IF (Surface(SurfNum)%FrameDivider > 0) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Dome '//TRIM(cAlphaArgs(2))//' must not have a frame/divider.')
          ErrorsFound = .TRUE.
        END IF

        IF (Construct(Surface(SurfNum)%Construction)%WindowTypeEQL) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Dome '//TRIM(cAlphaArgs(2))//' Equivalent Layer Window is not supported.')
          ErrorsFound = .TRUE.
        ENDIF
        ! Window multiplier is already handled in SurfaceGeometry.f90

        IF (.NOT. Surface(SurfNum)%ExtSolar) THEN
          CALL ShowWarningError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Dome '//TRIM(cAlphaArgs(2))//' is not exposed to exterior radiation.')
        END IF

        TDDPipe(PipeNum)%Dome = SurfNum
      END IF

      ! Get TDD:DIFFUSER object
      SurfNum = FindItemInList(cAlphaArgs(3),Surface%Name,TotSurfaces)

      IF (SurfNum == 0) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
          ':  Diffuser '//TRIM(cAlphaArgs(3))//' not found.')
        ErrorsFound = .TRUE.
      ELSE
        IF (FindTDDPipe(SurfNum) > 0) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Diffuser '//TRIM(cAlphaArgs(3))//' is referenced by more than one TDD.')
          ErrorsFound = .TRUE.
        END IF

        IF (SurfaceWindow(SurfNum)%OriginalClass .NE. SurfaceClass_TDD_Diffuser) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Diffuser '//TRIM(cAlphaArgs(3))//' is not of surface type TubularDaylightDiffuser.')
          ErrorsFound = .TRUE.
        END IF

        IF (Construct(Surface(SurfNum)%Construction)%TotGlassLayers > 1) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Diffuser '//TRIM(cAlphaArgs(3))//' construction ('//  &
            trim(Construct(Surface(SurfNum)%Construction)%Name)//') must have only 1 glass layer.')
          ErrorsFound = .TRUE.
        END IF

        IF (Construct(Surface(SurfNum)%Construction)%Transdiff <= 1.d-10) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Diffuser '//TRIM(cAlphaArgs(3))//' construction ('//  &
            trim(Construct(Surface(SurfNum)%Construction)%Name)//') invalid value.')
          CALL ShowContinueError('Diffuse solar transmittance of construction ['//  &
             trim(RoundSigDigits(Construct(Surface(SurfNum)%Construction)%Transdiff,4))//'] too small for calculations.')
          ErrorsFound = .TRUE.
        END IF

        IF (TDDPipe(PipeNum)%Dome > 0 .AND. ABS(Surface(SurfNum)%Area - Surface(TDDPipe(PipeNum)%Dome)%Area) > 0.1d0) THEN
          IF (SafeDivide(ABS(Surface(SurfNum)%Area - Surface(TDDPipe(PipeNum)%Dome)%Area),  &
                         Surface(TDDPipe(PipeNum)%Dome)%Area) > .1d0) THEN  ! greater than 10%
            CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Dome and diffuser areas are significantly different (>10%).')
            CALL ShowContinueError('...Diffuser Area=['//trim(RoundSigDigits(Surface(SurfNum)%Area,4))//  &
              ']; Dome Area=['//trim(RoundSigDigits(Surface(TDDPipe(PipeNum)%Dome)%Area,4))//'].')
            ErrorsFound = .TRUE.
          ELSE
            CALL ShowWarningError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Dome and diffuser areas differ by > .1 m2.')
            CALL ShowContinueError('...Diffuser Area=['//trim(RoundSigDigits(Surface(SurfNum)%Area,4))//  &
              ']; Dome Area=['//trim(RoundSigDigits(Surface(TDDPipe(PipeNum)%Dome)%Area,4))//'].')
          ENDIF
        END IF

        IF (Surface(SurfNum)%WindowShadingControlPtr > 0) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Diffuser '//TRIM(cAlphaArgs(3))//' must not have a shading control.')
          ErrorsFound = .TRUE.
        END IF

        IF (Surface(SurfNum)%FrameDivider > 0) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Diffuser '//TRIM(cAlphaArgs(3))//' must not have a frame/divider.')
          ErrorsFound = .TRUE.
        END IF

        IF (Construct(Surface(SurfNum)%Construction)%WindowTypeEQL) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Diffuser '//TRIM(cAlphaArgs(2))//' Equivalent Layer Window is not supported.')
          ErrorsFound = .TRUE.
        ENDIF

        ! Window multiplier is already handled in SurfaceGeometry.f90

        TDDPipe(PipeNum)%Diffuser = SurfNum
      END IF

      ! Construction
      TDDPipe(PipeNum)%Construction = FindItemInList(cAlphaArgs(4),Construct%Name,TotConstructs)

      IF(TDDPipe(PipeNum)%Construction == 0) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
          ':  Pipe construction '//TRIM(cAlphaArgs(4))//' not found.')
        ErrorsFound = .TRUE.
      ELSE
        Construct(TDDPipe(PipeNum)%Construction)%IsUsed=.true.
      END IF

      IF (rNumericArgs(1) > 0) THEN
        TDDPipe(PipeNum)%Diameter = rNumericArgs(1)
      ELSE
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
          ':  Pipe diameter must be greater than zero.')
        ErrorsFound = .TRUE.
      END IF

      PipeArea=0.25d0 * Pi * TDDPipe(PipeNum)%Diameter**2
      IF (TDDPipe(PipeNum)%Dome > 0 .AND. ABS(PipeArea - Surface(TDDPipe(PipeNum)%Dome)%Area) > 0.1d0) THEN
        IF (SafeDivide(ABS(PipeArea - Surface(TDDPipe(PipeNum)%Dome)%Area),  &
                         Surface(TDDPipe(PipeNum)%Dome)%Area) > .1d0) THEN  ! greater than 10%
          CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Pipe and dome/diffuser areas are significantly different (>10%).')
          CALL ShowContinueError('...Pipe Area=['//trim(RoundSigDigits(PipeArea,4))//']; Dome/Diffuser Area=['//  &
               trim(RoundSigDigits(Surface(TDDPipe(PipeNum)%Dome)%Area,4))//'].')
          ErrorsFound = .TRUE.
        ELSE
          CALL ShowWarningError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Pipe and dome/diffuser areas differ by > .1 m2.')
          CALL ShowContinueError('...Pipe Area=['//trim(RoundSigDigits(PipeArea,4))//']; Dome/Diffuser Area=['//  &
               trim(RoundSigDigits(Surface(TDDPipe(PipeNum)%Dome)%Area,4))//'].')
        ENDIF
      END IF

      IF (rNumericArgs(2) > 0) THEN
        TDDPipe(PipeNum)%TotLength = rNumericArgs(2)
      ELSE
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
          ':  Pipe length must be greater than zero.')
        ErrorsFound = .TRUE.
      END IF

      IF (rNumericArgs(3) > 0) THEN
        TDDPipe(PipeNum)%Reff = rNumericArgs(3)
      ELSE
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
          ':  Effective thermal resistance (R value) must be greater than zero.')
        ErrorsFound = .TRUE.
      END IF

      ! Transition zones
      TDDPipe(PipeNum)%NumOfTZones = NumAlphas - 4

      IF (TDDPipe(PipeNum)%NumOfTZones < 1) THEN
        CALL ShowWarningError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
          ':  No transition zones specified.  All pipe absorbed solar goes to exterior.')
      ELSE IF (TDDPipe(PipeNum)%NumOfTZones > MaxTZones) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
          ':  Maximum number of transition zones exceeded.')
        ErrorsFound = .TRUE.
      ELSE
        ALLOCATE(TDDPipe(PipeNum)%TZone(TDDPipe(PipeNum)%NumOfTZones))
        ALLOCATE(TDDPipe(PipeNum)%TZoneLength(TDDPipe(PipeNum)%NumOfTZones))
        ALLOCATE(TDDPipe(PipeNum)%TZoneHeatGain(TDDPipe(PipeNum)%NumOfTZones))

        TDDPipe(PipeNum)%TZone = 0
        TDDPipe(PipeNum)%TZoneLength = 0.d0
        TDDPipe(PipeNum)%TZoneHeatGain = 0.d0

        DO TZoneNum = 1, TDDPipe(PipeNum)%NumOfTZones
          TZoneName = cAlphaArgs(TZoneNum + 4)
          TDDPipe(PipeNum)%TZone(TZoneNum) = FindItemInList(TZoneName,Zone%Name,NumOfZones)
          IF (TDDPipe(PipeNum)%TZone(TZoneNum) == 0) THEN
            CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Transition zone '//TRIM(TZoneName)//' not found.')
            ErrorsFound = .TRUE.
          END IF

          TDDPipe(PipeNum)%TZoneLength(TZoneNum) = rNumericArgs(TZoneNum + 3)
          IF (TDDPipe(PipeNum)%TZoneLength(TZoneNum) < 0) THEN
            CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1)) &
              //':  Transition zone length for '//TRIM(TZoneName)//' must be zero or greater.')
            ErrorsFound = .TRUE.
          END IF
        END DO ! TZoneNum
      END IF

    END DO ! PipeNum

    IF (ErrorsFound) CALL ShowFatalError('Errors in DaylightingDevice:Tubular input.')
  END IF

  RETURN

END SUBROUTINE GetTDDInput


SUBROUTINE GetShelfInput

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   August 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Gets the input for light shelves and does some error checking.

          ! METHODOLOGY EMPLOYED:
          ! Standard EnergyPlus methodology.

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE InputProcessor, ONLY: GetNumObjectsFound, FindItemInList, GetObjectItem, VerifyName
  USE DataIPShortCuts

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS: na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL                        :: ErrorsFound = .FALSE. ! Set to true if errors in input, fatal at end of routine
  INTEGER                        :: IOStatus              ! Used in GetObjectItem
  LOGICAL                        :: IsBlank               ! TRUE if the name is blank
  LOGICAL                        :: IsNotOk               ! TRUE if there was a problem with a list name
  INTEGER                        :: NumAlphas             ! Number of Alphas for each GetObjectItem call
  INTEGER                        :: NumNumbers            ! Number of Numbers for each GetObjectItem call
  INTEGER                        :: ShelfNum              ! Daylighting shelf object number
  INTEGER                        :: SurfNum               ! Window, inside, or outside shelf surfaces
  INTEGER                        :: ConstrNum             ! Outside shelf construction object number

          ! FLOW:
  cCurrentModuleObject='DaylightingDevice:Shelf'
  NumOfShelf = GetNumObjectsFound(cCurrentModuleObject)

  IF (NumOfShelf > 0) THEN
    ALLOCATE(Shelf(NumOfShelf))

    DO ShelfNum = 1, NumOfShelf
      CALL GetObjectItem('DaylightingDevice:Shelf',ShelfNum,cAlphaArgs,NumAlphas,rNumericArgs,NumNumbers,IOStatus,  &
                   AlphaBlank=lAlphaFieldBlanks,NumBlank=lNumericFieldBlanks,  &
                   AlphaFieldnames=cAlphaFieldNames,NumericFieldNames=cNumericFieldNames)
      ! Shelf name
      IsNotOK = .FALSE.
      IsBlank = .FALSE.
      CALL VerifyName(cAlphaArgs(1),Shelf%Name,ShelfNum-1,IsNotOK,IsBlank,'DaylightingDevice:Shelf')
      IF (IsNotOK) THEN
        ErrorsFound = .TRUE.
        IF (IsBlank) cAlphaArgs(1) = 'xxxxx'
      END IF
      Shelf(ShelfNum)%Name = cAlphaArgs(1)

      ! Get window object
      SurfNum = FindItemInList(cAlphaArgs(2),Surface%Name,TotSurfaces)

      IF (SurfNum == 0) THEN
        CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
          ':  Window '//TRIM(cAlphaArgs(2))//' not found.')
        ErrorsFound = .TRUE.
      ELSE
        IF (Surface(SurfNum)%Class .NE. SurfaceClass_Window) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Window '//TRIM(cAlphaArgs(2))//' is not of surface type WINDOW.')
          ErrorsFound = .TRUE.
        END IF

        IF (Surface(SurfNum)%Shelf > 0) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Window '//TRIM(cAlphaArgs(2))//' is referenced by more than one shelf.')
          ErrorsFound = .TRUE.
        END IF

        IF (Surface(SurfNum)%WindowShadingControlPtr > 0) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Window '//TRIM(cAlphaArgs(2))//' must not have a shading control.')
          ErrorsFound = .TRUE.
        END IF

        IF (Surface(SurfNum)%FrameDivider > 0) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Window '//TRIM(cAlphaArgs(2))//' must not have a frame/divider.')
          ErrorsFound = .TRUE.
        END IF

        IF (Surface(SurfNum)%Sides /= 4) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Window '//TRIM(cAlphaArgs(2))//' must have 4 sides.')
          ErrorsFound = .TRUE.
        END IF
        !
        IF (Construct(Surface(SurfNum)%Construction)%WindowTypeEQL) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Window '//TRIM(cAlphaArgs(2))//' Equivalent Layer Window is not supported.')
          ErrorsFound = .TRUE.
        ENDIF


        Shelf(ShelfNum)%Window = SurfNum
        Surface(SurfNum)%Shelf = ShelfNum
      END IF

      ! Get inside shelf heat transfer surface (optional)
      IF (cAlphaArgs(3) .NE. '') THEN
        SurfNum = FindItemInList(cAlphaArgs(3),Surface%Name,TotSurfaces)

        IF (SurfNum == 0) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Inside shelf '//TRIM(cAlphaArgs(3))//' not found.')
          ErrorsFound = .TRUE.
        ELSE
          ! No error if shelf belongs to more than one window, e.g. concave corners

          IF (Surface(SurfNum)%ExtBoundCond .NE. SurfNum) THEN
            CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Inside shelf '//TRIM(cAlphaArgs(3))//  &
              ' must be its own Outside Boundary Condition Object.')
            ErrorsFound = .TRUE.
          END IF

          IF (Surface(SurfNum)%Sides /= 4) THEN
            CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Inside shelf '//TRIM(cAlphaArgs(3))//' must have 4 sides.')
            ErrorsFound = .TRUE.
          END IF

          Shelf(ShelfNum)%InSurf = SurfNum
        END IF
      END IF

        ! Get outside shelf attached shading surface (optional)
      IF (cAlphaArgs(4) .NE. '') THEN
        SurfNum = FindItemInList(cAlphaArgs(4),Surface%Name,TotSurfaces)

        IF (SurfNum == 0) THEN
          CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
            ':  Outside shelf '//TRIM(cAlphaArgs(4))//' not found.')
          ErrorsFound = .TRUE.
        ELSE
          ! No error if shelf belongs to more than one window, e.g. concave corners

          IF (Surface(SurfNum)%Class .NE. SurfaceClass_Shading) THEN
            CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Outside shelf '//TRIM(cAlphaArgs(4))//' is not a Shading:Zone:Detailed object.')
            ErrorsFound = .TRUE.
          END IF

          IF (Surface(SurfNum)%SchedShadowSurfIndex > 0) THEN
            CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Outside shelf '//TRIM(cAlphaArgs(4))//' must not have a transmittance schedule.')
            ErrorsFound = .TRUE.
          END IF

          IF (Surface(SurfNum)%Sides /= 4) THEN
            CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Outside shelf '//TRIM(cAlphaArgs(4))//' must have 4 sides.')
            ErrorsFound = .TRUE.
          END IF

          ! Get outside shelf construction (required if outside shelf is specified)
          IF (cAlphaArgs(5) .NE. '') THEN
            ConstrNum = FindIteminList(cAlphaArgs(5),Construct%Name,TotConstructs)

            IF (ConstrNum == 0) THEN
              CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
                ':  Outside shelf construction '//TRIM(cAlphaArgs(5))//' not found.')
              ErrorsFound = .TRUE.
            ELSE IF (Construct(ConstrNum)%TypeIsWindow) THEN
              CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
                ':  Outside shelf construction '//TRIM(cAlphaArgs(5))//' must not have WindowMaterial:Glazing.')
              ErrorsFound = .TRUE.
            ELSE
              Shelf(ShelfNum)%Construction = ConstrNum
              Construct(ConstrNum)%IsUsed=.true.
            END IF
          ELSE
            CALL ShowSevereError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
              ':  Outside shelf requires an outside shelf construction to be specified.')
            ErrorsFound = .TRUE.
          END IF

          ! Get view factor to outside shelf (optional)
          IF (NumNumbers > 0) THEN
            Shelf(ShelfNum)%ViewFactor = rNumericArgs(1)

            IF (rNumericArgs(1) == 0.0d0) THEN
              CALL ShowWarningError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
                ':  View factor to outside shelf is zero.  Shelf does not reflect on window.')
            END IF
          ELSE
            Shelf(ShelfNum)%ViewFactor = -1.0d0! Flag to have the view factor calculated during initialization
          END IF

          Shelf(ShelfNum)%OutSurf = SurfNum

          ! Reset some properties of the SURFACE:SHADING:ATTACHED object in order to receive radiation and shading
          ! Normally this would be done during initialization, but that's not early enough for some shading calculations
          Surface(SurfNum)%BaseSurf = SurfNum
          Surface(SurfNum)%HeatTransSurf = .TRUE.
          Surface(SurfNum)%Construction = ConstrNum ! Kludge to allow shading surface to be a heat transfer surface
          Construct(ConstrNum)%IsUsed=.true.
        END IF
      END IF

      IF (Shelf(ShelfNum)%InSurf == 0 .AND. Shelf(ShelfNum)%OutSurf == 0) &
        CALL ShowWarningError(trim(cCurrentModuleObject)//' = '//TRIM(cAlphaArgs(1))// &
          ':  No inside shelf or outside shelf was specified.')

    END DO ! ShelfNum

    IF (ErrorsFound) CALL ShowFatalError('Errors in DaylightingDevice:Shelf input.')
  END IF

  RETURN

END SUBROUTINE GetShelfInput


REAL(r64) FUNCTION CalcPipeTransBeam(R, A, Theta)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   May 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the numerical integral for the transmittance of a reflective cylinder with
          ! incident collimated beam radiation as described in Swift and Smith.

          ! METHODOLOGY EMPLOYED:
          ! Since this integral can be slow, a table of values is calculated and stored during
          ! initialization of the TDD.  Intermediate values are calculated by interpolation.
          ! Transmittance of sky and ground diffuse radiation is done by other functions.

          ! REFERENCES:
          ! Swift, P. D., and Smith, G. B.  "Cylindrical Mirror Light Pipes",
          !   Solar Energy Materials and Solar Cells 36 (1995), pp. 159-168.

          ! OTHER NOTES:
          ! The execution time of this function can be reduced by adjusting parameters N and xTol below.
          ! However, there is some penalty in accuracy for N < 100,000 and xTol > 150.

          ! USE STATEMENTS: na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)            :: R            ! Reflectance of surface, constant (can be made R = f(theta) later)
  REAL(r64), INTENT(IN)            :: A            ! Aspect ratio, L / d
  REAL(r64), INTENT(IN)            :: Theta        ! Angle of entry in radians

          ! FUNCTION PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: N = 100000.0d0 ! Number of integration points
  REAL(r64), PARAMETER :: xTol = 150.0d0 ! Tolerance factor to skip iterations where dT is approximately 0
                                              ! Must be >= 1.0, increase this number to decrease the execution time
  REAL(r64), PARAMETER :: myLocalTiny =  TINY(1.0D0)

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64)       :: i            ! Integration interval between points
  REAL(r64)       :: s            ! Entry point
  REAL(r64)       :: dT
  REAL(r64)       :: T            ! Beam transmittance for collimated solar real
  REAL(r64)       :: x, c1, c2    ! Intermediate variables for speed optimization
  REAL(r64)       :: xLimit       ! Limiting x value to prevent floating point underflow

          ! FLOW:
  CalcPipeTransBeam = 0.0d0

  T = 0.0d0
  i = 1.0d0 / N

  xLimit = (LOG(N**2.0d0*myLocalTiny)/LOG(R))/xTol

  c1 = A * TAN(Theta)
  c2 = 4.0d0 / Pi

  s = i
  DO WHILE (s < (1.0d0 - i))
    x = c1 / s

    IF (x < xLimit) THEN
      dT = c2 * (R**INT(x)) * (1.0d0 - (1.0d0 - R) * (x - INT(x))) * (s**2) / SQRT(1.0d0 - s**2)
      T = T + dT
    END IF

    s = s + i
  END DO

  T = T / (N - 1.0d0) ! - 1.0, because started on i, not 0

  CalcPipeTransBeam = T

  RETURN

END FUNCTION CalcPipeTransBeam


REAL(r64) FUNCTION CalcTDDTransSolIso(PipeNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   July 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the transmittance of sky isotropic radiation for use with the anisotropic sky transmittance.
          ! This value is also used for all ground reflected solar radiation (which is isotropic).

          ! METHODOLOGY EMPLOYED:
          ! The transmittance is calculated and stored once at initialization because the value is a constant.
          ! The routine numerically integrates over the entire sky.  All radiation is isotropic, but incident
          ! angle varies over the hemisphere.
          !
          ! Trans = Flux Transmitted / Flux Incident
          !
          ! Not sure if shading and tilt is adequately accounted for by DifShdgRatioIsoSky later on or not...

          ! REFERENCES:
          ! See AnisoSkyViewFactors in SolarShading.f90.

          ! USE STATEMENTS: na

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: PipeNum    ! TDD pipe object number

          ! FUNCTION PARAMETER DEFINITIONS:
  INTEGER, PARAMETER  :: NPH = 1000 ! Number of altitude integration points

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: FluxInc    ! Incident solar flux
  REAL(r64)    :: FluxTrans  ! Transmitted solar flux
  REAL(r64)    :: trans      ! Total beam solar transmittance of TDD
  INTEGER             :: N          ! Loop counter
  REAL(r64)           :: PH         ! Altitude angle of sky element
  REAL(r64)           :: dPH        ! Altitude angle increment
  REAL(r64)           :: COSI       ! Cosine of incident angle
  REAL(r64)           :: SINI       ! Sine of incident angle
  REAL(r64)           :: P          ! Angular distribution function

          ! FLOW:
  FluxInc = 0.0d0
  FluxTrans = 0.0d0

  ! Integrate from 0 to Pi/2 altitude
  dPH = 90.0d0 * DegToRadians / NPH
  PH = 0.5d0 * dPH
  DO N = 1, NPH
    COSI = COS(PiOvr2 - PH)
    SINI = SIN(PiOvr2 - PH)

    P = COSI ! Angular distribution function: P = COS(Incident Angle) for diffuse isotropic

    ! Calculate total TDD transmittance for given angle
    trans = TransTDD(PipeNum, COSI, SolarBeam)

    FluxInc = FluxInc + P * SINI * dPH
    FluxTrans = FluxTrans + trans * P * SINI * dPH

    PH = PH + dPH ! Increment the altitude angle
  END DO ! N

  CalcTDDTransSolIso = FluxTrans / FluxInc

  RETURN

END FUNCTION CalcTDDTransSolIso


REAL(r64) FUNCTION CalcTDDTransSolHorizon(PipeNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   July 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the transmittance of sky horizon radiation for use with the anisotropic sky transmittance.

          ! METHODOLOGY EMPLOYED:
          ! The transmittance is calculated and stored once at initialization because the value is a constant.
          ! The routine numerically integrates over the horizon as an infinitesimally narrow strip of the sky.
          ! Horizon radiation is isotropic, but incident angle varies over the semicircle.
          !
          ! Trans = Flux Transmitted / Flux Incident
          !
          ! Not sure if shading is adequately accounted for by DifShdgRatioHoriz later on or not...

          ! REFERENCES:
          ! See AnisoSkyViewFactors in SolarShading.f90.

          ! USE STATEMENTS:
  USE DataSurfaces

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: PipeNum    ! TDD pipe object number

          ! FUNCTION PARAMETER DEFINITIONS:
  INTEGER, PARAMETER  :: NTH = 18   ! Number of azimuth integration points

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  REAL(r64)    :: FluxInc    ! Incident solar flux
  REAL(r64)    :: FluxTrans  ! Transmitted solar flux
  REAL(r64)    :: trans      ! Total beam solar transmittance of TDD
  INTEGER             :: N          ! Loop counter
  REAL(r64)           :: TH         ! Azimuth angle of sky horizon element
  REAL(r64)           :: dTH        ! Azimuth angle increment
  REAL(r64)           :: THMIN      ! Minimum azimuth integration limit
  REAL(r64)           :: THMAX      ! Maximum azimuth integration limit
  REAL(r64)           :: CosPhi     ! Cosine of TDD:DOME altitude angle
  REAL(r64)           :: Theta      ! TDD:DOME azimuth angle
  REAL(r64)           :: COSI       ! Cosine of the incident angle

          ! FLOW:
  FluxInc = 0.0d0
  FluxTrans = 0.0d0

  CosPhi = COS(PiOvr2 - Surface(TDDPipe(PipeNum)%Dome)%Tilt * DegToRadians)
  Theta = Surface(TDDPipe(PipeNum)%Dome)%Azimuth * DegToRadians

  IF (CosPhi > 0.01d0) THEN ! Dome has a view of the horizon
    ! Integrate over the semicircle
    THMIN = Theta - PiOvr2
    THMAX = Theta + PiOvr2
    dTH = 180.0d0 * DegToRadians / NTH
    TH = THMIN + 0.5d0 * dTH
    DO N = 1, NTH
      ! Calculate incident angle between dome outward normal and horizon element
      COSI = CosPhi * COS(TH - Theta)

      ! Calculate total TDD transmittance for given angle
      trans = TransTDD(PipeNum, COSI, SolarBeam)

      FluxInc = FluxInc + COSI * dTH
      FluxTrans = FluxTrans + trans * COSI * dTH

      TH = TH + dTH ! Increment the azimuth angle
    END DO ! N

    CalcTDDTransSolHorizon = FluxTrans / FluxInc

  ELSE ! Dome is nearly horizontal and has almost no view of the horizon
    CalcTDDTransSolHorizon = 0.0d0 ! = TransTDD(PipeNum, ???, SolarBeam) ! Could change to an angle near the horizon
  END IF

  RETURN

END FUNCTION CalcTDDTransSolHorizon


REAL(r64) FUNCTION CalcTDDTransSolAniso(PipeNum, COSI)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   July 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the transmittance of the anisotropic sky.

          ! METHODOLOGY EMPLOYED:
          ! Similar to the Trans = FluxTrans/FluxInc integrations above, the anisotropic sky can be decomposed
          ! and have a different transmittance applied to each component.
          !
          !   FluxInc = IsoSkyRad + CircumSolarRad + HorizonRad
          !   FluxTrans = T1*IsoSkyRad + T2*CircumSolarRad + T3*HorizonRad
          !
          ! It turns out that FluxTrans/FluxInc is equivalent to AnisoSkyTDDMult/AnisoSkyMult.
          ! AnisoSkyMult has been conveniently calculated already in AnisoSkyViewFactors in SolarShading.f90.
          !
          ! AnisoSkyMult = MultIsoSky*DifShdgRatioIsoSky + MultCircumSolar*SunLitFrac + MultHorizonZenith*DifShdgRatioHoriz
          !
          ! In this routine a similar AnisoSkyTDDMult is calculated that applies the appropriate transmittance to each
          ! of the components above.  The result is Trans = AnisoSkyTDDMult/AnisoSkyMult.
          !
          ! Shading and orientation are already taken care of by DifShdgRatioIsoSky and DifShdgRatioHoriz.

          ! REFERENCES:
          ! See AnisoSkyViewFactors in SolarShading.f90.

          ! USE STATEMENTS: na
  USE DataGlobals, ONLY: HourOfDay, TimeStep
  USE DataHeatBalance, ONLY: SunlitFrac, AnisoSkyMult, DifShdgRatioIsoSky, DifShdgRatioHoriz, &
    MultIsoSky, MultCircumSolar, MultHorizonZenith, curDifShdgRatioIsoSky, DifShdgRatioHorizHRTS
  USE DataSystemVariables, ONLY: DetailedSkyDiffuseAlgorithm

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: PipeNum         ! TDD pipe object number
  REAL(r64), INTENT(IN)    :: COSI            ! Cosine of the incident angle

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: DomeSurf        ! TDD:DOME surface number
  REAL(r64)           :: IsoSkyRad       ! Isotropic sky radiation component
  REAL(r64)           :: CircumSolarRad  ! Circumsolar sky radiation component
  REAL(r64)           :: HorizonRad      ! Horizon sky radiation component
  REAL(r64)           :: AnisoSkyTDDMult ! Anisotropic sky multiplier for TDD

          ! FLOW:
  DomeSurf = TDDPipe(PipeNum)%Dome

  IF (.not. DetailedSkyDiffuseAlgorithm .or. .not.  ShadingTransmittanceVaries .or.  &
      SolarDistribution == MinimalShadowing) THEN
    IsoSkyRad = MultIsoSky(DomeSurf) * DifShdgRatioIsoSky(DomeSurf)
    HorizonRad = MultHorizonZenith(DomeSurf) * DifShdgRatioHoriz(DomeSurf)
  ELSE
    IsoSkyRad = MultIsoSky(DomeSurf) * curDifShdgRatioIsoSky(DomeSurf)
    HorizonRad = MultHorizonZenith(DomeSurf) * DifShdgRatioHorizHRTS(DomeSurf,HourOfDay,TimeStep)
  ENDIF
  CircumSolarRad = MultCircumSolar(DomeSurf) * SunlitFrac(DomeSurf,HourOfDay,TimeStep)

  AnisoSkyTDDMult = &
    TDDPipe(PipeNum)%TransSolIso * IsoSkyRad &
    + TransTDD(PipeNum, COSI, SolarBeam) * CircumSolarRad &
    + TDDPipe(PipeNum)%TransSolHorizon * HorizonRad

  IF (AnisoSkyMult(DomeSurf) > 0.0d0) THEN
    CalcTDDTransSolAniso = AnisoSkyTDDMult / AnisoSkyMult(DomeSurf)
  ELSE
    CalcTDDTransSolAniso = 0.0d0
  ENDIF

  RETURN

END FUNCTION CalcTDDTransSolAniso


RECURSIVE REAL(r64) FUNCTION TransTDD(PipeNum, COSI, RadiationType)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   May 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Calculates the total transmittance of the TDD for specified radiation type.

          ! METHODOLOGY EMPLOYED:
          ! The transmittances for each component (i.e. TDD:DIFFUSER, TDD:DOME, and pipe) are calculated.
          ! All transmittances are multiplied to get the total for the TDD:
          !   TransTDD = transDome * transPipe * transDiff
          !
          ! Transmittance of beam radiation is calculated by interpolating the values in a
          ! table created during initialization.  The table values are from Swift and Smith's
          ! numerical integral for collimated beam radiation.
          !
          ! Transmittances of isotropic and anisotropic diffuse radiation are more complicated and call
          ! other subroutines in this module.
          !
          ! All light reaching the TDD:DIFFUSER is assumed to be diffuse.
          ! NOTE: Dome transmittance could be improved by taking into account curvature of the dome.

          ! REFERENCES:
          ! Swift, P. D., and Smith, G. B.  "Cylindrical Mirror Light Pipes",
          !   Solar Energy Materials and Solar Cells 36 (1995), pp. 159-168.

          ! USE STATEMENTS:
  USE General, ONLY: POLYF

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)           :: PipeNum        ! TDD pipe object number
  REAL(r64), INTENT(IN)              :: COSI           ! Cosine of the incident angle
  INTEGER, INTENT(IN)           :: RadiationType  ! Radiation type flag

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER                       :: constDome      ! Construction object number for TDD:DOME
  INTEGER                       :: constDiff      ! Construction object number for TDD:DIFFUSER
  REAL(r64)                     :: transDome
  REAL(r64)                     :: transPipe
  REAL(r64)                     :: transDiff

          ! FLOW:
  TransTDD = 0.0d0

  ! Get constructions of each TDD component
  constDome = Surface(TDDPipe(PipeNum)%Dome)%Construction
  constDiff = Surface(TDDPipe(PipeNum)%Diffuser)%Construction

  ! Get the transmittance of each component and of total TDD
  SELECT CASE (RadiationType)

    CASE (VisibleBeam)
      transDome = POLYF(COSI, Construct(constDome)%TransVisBeamCoef)
      transPipe = InterpolatePipeTransBeam(COSI, TDDPipe(PipeNum)%PipeTransVisBeam)
      transDiff = Construct(constDiff)%TransDiffVis ! May want to change to POLYF also!

      TransTDD = transDome * transPipe * transDiff

    CASE (SolarBeam)
      transDome = POLYF(COSI, Construct(constDome)%TransSolBeamCoef)
      transPipe = InterpolatePipeTransBeam(COSI, TDDPipe(PipeNum)%PipeTransSolBeam)
      transDiff = Construct(constDiff)%TransDiff ! May want to change to POLYF also!

      TransTDD = transDome * transPipe * transDiff

    CASE (SolarAniso)
      TransTDD = CalcTDDTransSolAniso(PipeNum, COSI)

    CASE (SolarIso)
      TransTDD = TDDPipe(PipeNum)%TransSolIso

  END SELECT

  RETURN

END FUNCTION TransTDD


REAL(r64) FUNCTION InterpolatePipeTransBeam(COSI, transBeam)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   July 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Interpolates the beam transmittance vs. cosine angle table.

          ! METHODOLOGY EMPLOYED: na
          ! REFERENCES: na

          ! USE STATEMENTS:
  USE FluidProperties, ONLY: FindArrayIndex ! USEd code could be copied here to eliminate dependence on FluidProperties

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  REAL(r64), INTENT(IN)                         :: COSI         ! Cosine of the incident angle
  REAL(r64), DIMENSION(NumOfAngles), INTENT(IN) :: transBeam    ! Table of beam transmittance vs. cosine angle

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER                                  :: Lo, Hi
  REAL(r64)                                :: m, b

          ! FLOW:
  InterpolatePipeTransBeam = 0.0d0

  ! Linearly interpolate transBeam/COSAngle table to get value at current cosine of the angle
  Lo = FindArrayIndex(COSI, COSAngle)
  Hi = Lo + 1

  IF (Lo > 0 .AND. Hi <= NumOfAngles) THEN
    m = (transBeam(Hi) - transBeam(Lo)) / (COSAngle(Hi) - COSAngle(Lo))
    b = transBeam(Lo) - m * COSAngle(Lo)

    InterpolatePipeTransBeam = m * COSI + b
  ELSE
    InterpolatePipeTransBeam = 0.0d0
  END IF

  RETURN

END FUNCTION InterpolatePipeTransBeam


INTEGER FUNCTION FindTDDPipe(WinNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   May 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Given the TDD:DOME or TDD:DIFFUSER object number, returns TDD pipe number.

          ! METHODOLOGY EMPLOYED:
          ! Similar to FindItemInList defined in InputProcessor.

          ! REFERENCES: na
          ! USE STATEMENTS:
  USE DataSurfaces, ONLY: Surface

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! FUNCTION ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: WinNum

          ! FUNCTION LOCAL VARIABLE DECLARATIONS:
  INTEGER             :: PipeNum       ! TDD pipe object number

          ! FLOW:
  FindTDDPipe = 0

  IF (NumOfTDDPipes <= 0) THEN
    CALL ShowFatalError('FindTDDPipe: Surface='//TRIM(Surface(WinNum)%Name)//  &
        ', TDD:Dome object does not reference a valid Diffuser object.' // &
        '...needs DaylightingDevice:Tubular of same name as Surface.')
  ENDIF

  DO PipeNum = 1, NumOfTDDPipes
     IF ((WinNum .EQ. TDDPipe(PipeNum)%Dome) .OR. (WinNum .EQ. TDDPipe(PipeNum)%Diffuser)) THEN
       FindTDDPipe = PipeNum
       EXIT
     END IF
  END DO ! PipeNum

  RETURN

END FUNCTION FindTDDPipe


SUBROUTINE DistributeTDDAbsorbedSolar

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   July 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Sums the absorbed solar gains from TDD pipes that pass through transition zones.

          ! METHODOLOGY EMPLOYED:
          ! The total absorbed solar gain is a sum of the following gains:
          !   1. Inward bound solar absorbed by multiple pipe reflections (solar entering pipe - solar exiting pipe)
          !   2. Outward bound solar absorbed by multiple pipe reflections due to:
          !     a. Reflection off of diffuser surface (inside of TDD)
          !     b. Zone diffuse interior shortwave incident on the diffuser from windows, lights, etc.
          !   3. Inward absorbed solar in dome and diffuser glass
          !
          ! This subroutine is called by InitIntSolarDistribution in HeatBalanceSurfaceManager.f90.

          ! REFERENCES: na

          ! USE STATEMENTS:
  USE DataHeatBalance, ONLY: QRadSWOutIncident, QRadSWwinAbs, QRadSWwinAbsTot, QS, ZoneIntGain
  USE DataSurfaces, ONLY: WinTransSolar

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS: na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER :: PipeNum        ! TDD pipe object number
  INTEGER :: DiffSurf       ! Surface number of TDD:DIFFUSER
  INTEGER :: TZoneNum       ! Transition zone index
!  INTEGER :: ActualZone     ! Actual transition zone number
  REAL(r64)    :: transDiff      ! Diffuse transmittance of TDD:DIFFUSER
  REAL(r64)    :: QRefl          ! Diffuse radiation reflected back up the pipe
  REAL(r64)    :: TotTDDPipeGain ! Total absorbed solar gain in the tubular daylighting device pipe

          ! FLOW:
  DO PipeNum = 1, NumOfTDDPipes
    DiffSurf = TDDPipe(PipeNum)%Diffuser
    transDiff = Construct(Surface(DiffSurf)%Construction)%TransDiff

    ! Calculate diffuse solar reflected back up the pipe by the inside surface of the TDD:DIFFUSER
    ! All solar arriving at the diffuser is assumed to be isotropically diffuse by this point
    QRefl = (QRadSWOutIncident(DiffSurf) - QRadSWwinAbsTot(DiffSurf)) * Surface(DiffSurf)%Area - WinTransSolar(DiffSurf)

    ! Add diffuse interior shortwave reflected from zone surfaces and from zone sources, lights, etc.
    QRefl = QRefl + QS(Surface(DiffSurf)%Zone) * Surface(DiffSurf)%Area * transDiff

    TotTDDPipeGain = WinTransSolar(TDDPipe(PipeNum)%Dome) & ! Solar entering pipe
      - QRadSWOutIncident(DiffSurf) * Surface(DiffSurf)%Area & ! Solar exiting pipe
      + QRefl * (1.0d0 - TDDPipe(PipeNum)%TransSolIso / transDiff) & ! Absorbed due to reflections on the way out
      + QRadSWwinAbs(TDDPipe(PipeNum)%Dome,1) * Surface(DiffSurf)%Area / 2.0d0 & ! Inward absorbed solar from dome glass
      + QRadSWwinAbs(DiffSurf,1) * Surface(DiffSurf)%Area / 2.0d0 ! Inward absorbed solar from diffuser glass

    TDDPipe(PipeNum)%PipeAbsorbedSolar = MAX(0.0d0, TotTDDPipeGain) ! Report variable [W]

    DO TZoneNum = 1, TDDPipe(PipeNum)%NumOfTZones
      ! Distribute absorbed solar gain in proportion to transition zone length
      TDDPipe(PipeNum)%TZoneHeatGain(TZoneNum) = &
          TotTDDPipeGain * (TDDPipe(PipeNum)%TZoneLength(TZoneNum) / TDDPipe(PipeNum)%TotLength)
    END DO ! TZoneNum
  END DO

  RETURN

END SUBROUTINE DistributeTDDAbsorbedSolar


SUBROUTINE CalcViewFactorToShelf(ShelfNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         Peter Graham Ellis
          !       DATE WRITTEN   August 2003
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! Attempts to calculate exact analytical view factor from window to outside shelf.

          ! METHODOLOGY EMPLOYED:
          ! Uses a standard analytical solution.  It is required that window and shelf have the same width, i.e.
          ! one edge (or two vertices) shared in common.  An error or warning is issued if not true.
          !
          ! A more general routine should be implemented at some point to solve for more complicated geometries.
          ! Until then, the user has the option to specify their own solution for the view factor in the input object.

          ! REFERENCES:
          ! Mills, A. F.  Heat and Mass Transfer, 1995, p. 499.  (Shape factor for adjacent rectangles.)

          ! USE STATEMENTS:
  USE Vectors, ONLY: Distance

  IMPLICIT NONE ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN) :: ShelfNum           ! Daylighting shelf object number

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)           :: W, H, L            ! Width, height, and length of window/shelf geometry
  REAL(r64)           :: M, N               ! Intermediate variables
  REAL(r64)           :: E1, E2, E3, E4     ! Intermediate equations
  INTEGER             :: VWin, VShelf       ! Vertex indices
  INTEGER             :: NumMatch           ! Number of vertices matched

          ! FLOW:
  W = Surface(Shelf(ShelfNum)%Window)%Width
  H = Surface(Shelf(ShelfNum)%Window)%Height

  ! Find length, i.e. projection, of outside shelf
  IF (Surface(Shelf(ShelfNum)%OutSurf)%Width == W) THEN
    L = Surface(Shelf(ShelfNum)%OutSurf)%Height
  ELSE IF (Surface(Shelf(ShelfNum)%OutSurf)%Height == W) THEN
    L = Surface(Shelf(ShelfNum)%OutSurf)%Width
  ELSE
    CALL ShowFatalError('DaylightingDevice:Shelf = '//TRIM(Shelf(ShelfNum)%Name)// &
      ':  Width of window and outside shelf do not match.')
  END IF

  ! Error if more or less than two vertices match
  NumMatch = 0
  DO VWin = 1, 4
    DO VShelf = 1, 4
      IF (Distance(Surface(Shelf(ShelfNum)%Window)%Vertex(VWin), Surface(Shelf(ShelfNum)%OutSurf)%Vertex(VShelf)) == 0.0d0) &
        NumMatch = NumMatch + 1
    END DO
  END DO

  IF (NumMatch < 2) THEN
    CALL ShowWarningError('DaylightingDevice:Shelf = '//TRIM(Shelf(ShelfNum)%Name)// &
      ':  Window and outside shelf must share two vertices.  View factor calculation may be inaccurate.')
  ELSE IF (NumMatch > 2) THEN
    CALL ShowFatalError('DaylightingDevice:Shelf = '//TRIM(Shelf(ShelfNum)%Name)// &
      ':  Window and outside shelf share too many vertices.')
  END IF

  ! Calculate exact analytical view factor from window to outside shelf
  M = H / W
  N = L / W

  E1 = M * ATAN((1.0d0 / M)) + N * ATAN((1.0d0 / N)) - (SQRT(N**2 + M**2)) * ATAN(((N**2 + M**2))**(-0.5d0))
  E2 = ((1.0d0 + M**2) * (1.0d0 + N**2)) / (1.0d0 + M**2 + N**2)
  E3 = ((M**2) * (1.0d0 + M**2 + N**2) / ((1.0d0 + M**2) * (M**2 + N**2)))**(M**2)
  E4 = ((N**2) * (1.0d0 + M**2 + N**2) / ((1.0d0 + N**2) * (M**2 + N**2)))**(N**2)

  Shelf(ShelfNum)%ViewFactor = (1.0d0 / (Pi * M)) * (E1 + 0.25d0 * LOG(E2 * E3 * E4))

  RETURN

END SUBROUTINE CalcViewFactorToShelf

SUBROUTINE FigureTDDZoneGains

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         B. Griffith
          !       DATE WRITTEN   Dec 2011
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          ! intialize zone gains at begin new environment

          ! METHODOLOGY EMPLOYED:
          ! <description>

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
  USE DataGlobals, ONLY: BeginEnvrnFlag

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
  LOGICAL, SAVE :: MyEnvrnFlag = .TRUE.
  INTEGER   :: Loop

  IF (NumOfTDDPipes == 0) RETURN

  IF (BeginEnvrnFlag .AND. MyEnvrnFlag) THEN
    DO Loop = 1, NumOfTDDPipes
      TDDPipe(Loop)%TZoneHeatGain = 0.d0
    ENDDO
    MyEnvrnFlag = .FALSE.
  ENDIF
  IF( .NOT. BeginEnvrnFlag) MyEnvrnFlag = .TRUE.

  RETURN

END SUBROUTINE FigureTDDZoneGains

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

END MODULE DaylightingDevices

