MODULE DisplacementVentMgr


  ! MODULE INFORMATION:
  !       AUTHOR         G. Carrilho da Graca
  !       DATE WRITTEN   February 2004
  !       MODIFIED       na
  !       RE-ENGINEERED  na

  ! PURPOSE OF THIS MODULE:
  ! Routines that implement the UCSD Displacement Ventilation

  ! METHODOLOGY EMPLOYED:
  !

  ! REFERENCES:
  !

  ! OTHER NOTES: none

  ! USE STATEMENTS:
USE DataPrecisionGlobals
USE DataGlobals
USE DataLoopNode
USE DataEnvironment
USE DataHeatBalance
USE DataHeatBalSurface
USE DataSurfaces
USE DataRoomAirModel
USE ConvectionCoefficients, ONLY: CalcDetailedHcInForDVModel
USE DataHVACGlobals, ONLY: SysTimeElapsed, PreviousTimeStep, ShortenTimeStepSysRoomAir
USE DataUCSDSharedData
USE DAtaAirflowNetwork
USE DataInterfaces

IMPLICIT NONE         ! Enforce explicit typing of all variables

PRIVATE ! Everything private unless explicitly made public

  ! MODULE PARAMETER DEFINITIONS:

  ! DERIVED TYPE DEFINITIONS:
  ! na

  ! MODULE VARIABLE DECLARATIONS:
  REAL(r64)  :: HAT_MX     ! HAT_MX Convection Coefficient times Area times Temperature for the upper subzone
  REAL(r64)  :: HA_MX      ! HA_MX Convection Coefficient times Area for the upper subzone
  REAL(r64)  :: HAT_OC     ! HAT_OC Convection Coefficient times Area times Temperature for the lower subzone
  REAL(r64)  :: HA_OC      ! HA_OC Convection Coefficient times Area for the lower subzone
  REAL(r64)  :: HAT_FLOOR  ! HAT_FLOOR Convection Coefficient times Area times Temperature for the floor(?) subzone
  REAL(r64)  :: HA_FLOOR   ! HA_FLOOR Convection Coefficient times Area for the floor(?) subzone
  REAL(r64)  :: HeightFloorSubzoneTop = 0.2d0   ! Assumed thickness of floor subzone
  REAL(r64)  :: ThickOccupiedSubzoneMin = 0.2d0 ! Minimum thickness of occupied subzone
  REAL(r64)  :: HeightIntMass=0.0d0 ! Height of internal mass surfaces, assumed vertical, cannot exceed ceiling height
  REAL(r64)  :: HeightIntMassDefault = 2.0d0 ! Default height of internal mass surfaces

  ! SUBROUTINE SPECIFICATIONS:
PUBLIC ManageUCSDDVModel
PRIVATE CalcUCSDDV
PRIVATE InitUCSDDV
PRIVATE HcUCSDDV

CONTAINS



SUBROUTINE ManageUCSDDVModel(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         G. Carrilho da Graca
          !       DATE WRITTEN   February 2004
          !       MODIFIED       na
          !       RE-ENGINEERED  na

          ! PURPOSE OF THIS SUBROUTINE:
          !   manage the UCSD Displacement Ventilation model

          ! METHODOLOGY EMPLOYED:
          ! na

          ! REFERENCES:
          ! na

          ! USE STATEMENTS:
          ! na
  USE DataHeatBalSurface,         ONLY : TempSurfIn

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER, INTENT(IN)                     :: ZoneNum      ! index number for the specified zone

          ! SUBROUTINE PARAMETER DEFINITIONS
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:

          ! FLOW:

    ! initialize Displacement Ventilation model
    CALL InitUCSDDV(ZoneNum)

    ! perform Displacement Ventilation model calculations
    CALL CalcUCSDDV(ZoneNum)

    RETURN

END SUBROUTINE ManageUCSDDVModel

!**************************************************************************************************

SUBROUTINE InitUCSDDV(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         G. Carrilho da Graca
          !       DATE WRITTEN   February 2004
          !       MODIFIED       -
          !       RE-ENGINEERED  -

          ! PURPOSE OF THIS SUBROUTINE:
          ! Low Energy Cooling by Ventilation initialization subroutine.
          ! All the data preparation needed to run the LECV models.
          ! The subroutines sets up arrays with the locations in the main EnergyPlus surface array of
          ! ceiling, windows, doors and walls. The zone maximum and minimum height is calculated.

          ! -
          ! METHODOLOGY EMPLOYED:
          ! -
          ! -
          ! -
          ! -

          ! REFERENCES:
          ! -
          ! -

          ! USE STATEMENTS:

  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,INTENT (IN)                     :: ZoneNum

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  LOGICAL,SAVE             :: MyOneTimeFlag = .true.
  LOGICAL, ALLOCATABLE,Save, DIMENSION(:) :: MyEnvrnFlag

  ! Do the one time initializations
  IF (MyOneTimeFlag) THEN
    ALLOCATE(MyEnvrnFlag(NumOfZones))
    MyEnvrnFlag = .TRUE.
    HeightFloorSubzoneTop = 0.2d0
    ThickOccupiedSubzoneMin= 0.2d0
    HeightIntMassDefault = 2.0d0
    MyOneTimeFlag = .FALSE.
  END IF

  ! Do the begin environment initializations
  IF (BeginEnvrnFlag .and. MyEnvrnFlag(ZoneNum)) THEN
    HAT_MX    = 0.0d0
    HAT_OC    = 0.0d0
    HA_MX     = 0.0d0
    HA_OC     = 0.0d0
    HAT_FLOOR = 0.0d0
    HA_FLOOR  = 0.0d0
    MyEnvrnFlag(ZoneNum) = .FALSE.
  END IF

  IF (.not. BeginEnvrnFlag) THEN
    MyEnvrnFlag(ZoneNum) = .true.
  ENDIF

  ! initialize these module variables every timestep
  HeightIntMass=HeightIntMassDefault

  RETURN

END SUBROUTINE InitUCSDDV

!**************************************************************************************************

SUBROUTINE HcUCSDDV(ZoneNum,FractionHeight)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         G. Carrilho da Graca
          !       DATE WRITTEN   February 2004
          !       MODIFIED       -
          !       RE-ENGINEERED  -

          ! PURPOSE OF THIS SUBROUTINE:
          ! Main subroutine for convection calculation in the UCSD Displacement Ventilation model.
          ! It calls CalcDetailedHcInForDVModel for convection coefficient
          ! initial calculations and averages the final result comparing the position of the surface with
          ! the interface subzone height.

          ! METHODOLOGY EMPLOYED:
          ! -
          ! -
          ! -
          ! -

          ! REFERENCES:
          ! -
          ! -

          ! USE STATEMENTS:
  USE DataRoomAirModel ,                     ONLY: AirModel
  USE DataHeatBalFanSys
  USE DataEnvironment
  USE DataHeatBalance
  USE InputProcessor
  USE ScheduleManager,                       ONLY: GetScheduleIndex
  USE DataGlobals,                           ONLY: BeginEnvrnFlag


  IMPLICIT NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,INTENT (IN)                     :: ZoneNum             !
  REAL(r64),INTENT (IN)                        :: FractionHeight      !

          ! SUBROUTINE PARAMETER DEFINITIONS:
          ! na

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  INTEGER                                 :: Ctd                 ! DO loop counter for surfaces
  REAL(r64)                               :: HLD                 ! Convection coefficient for the lower area of surface
  REAL(r64)                               :: TmedDV              ! Average temperature for DV
  REAL(r64)                               :: Z1                  ! auxiliary var for lowest height
  REAL(r64)                               :: Z2                  ! auxiliary var for highest height
  REAL(r64)                               :: ZSupSurf            ! highest height for this surface
  REAL(r64)                               :: ZInfSurf            ! lowest height for this surface
  REAL(r64)                               :: HLU                 ! Convection coefficient for the upper area of surface
  REAL(r64)                               :: LayH                ! Height of the Occupied/Mixed subzone interface
  REAL(r64)                               :: LayFrac             ! Fraction height of the Occupied/Mixed subzone interface
  INTEGER                                 :: SurfNum             ! Surface number

  HAT_MX    = 0.0d0
  HAT_OC    = 0.0d0
  HA_MX     = 0.0d0
  HA_OC     = 0.0d0
  HAT_FLOOR = 0.0d0
  HA_FLOOR  = 0.0d0
  ! Is the air flow model for this zone set to UCSDDV Displacement Ventilation?
  IF(IsZoneDV(ZoneNum)) THEN
    LayFrac = FractionHeight
    LayH    = FractionHeight*(ZoneCeilingHeight((ZoneNum-1)*2 + 2)- &
                                                  ZoneCeilingHeight((ZoneNum-1)*2 + 1))
    ! WALL Hc, HA and HAT calculation
    DO Ctd = PosZ_Wall((ZoneNum-1)*2 + 1),PosZ_Wall((ZoneNum-1)*2 + 2)
      SurfNum = APos_Wall(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp
      IF (SurfNum == 0) CYCLE
      Z1 = MINVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z)
      Z2 = MAXVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z)
      ZSupSurf = Z2 - ZoneCeilingHeight((ZoneNum-1)*2 + 1)
      ZInfSurf = Z1 - ZoneCeilingHeight((ZoneNum-1)*2 + 1)

      ! The Wall surface is in the upper subzone
      IF(ZInfSurf > LayH)THEN
        TempEffBulkAir(SurfNum) = ZTMX(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,DVHcIn)
        HWall(Ctd)= DVHcIn(SurfNum)
        HAT_MX = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HWall(Ctd) + HAT_MX
        HA_MX  = Surface(SurfNum)%Area*HWall(Ctd) + HA_MX
      ENDIF

        ! The Wall surface is in the lower subzone
      IF(ZSupSurf < LayH)THEN
        TempEffBulkAir(SurfNum) = ZTOC(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,DVHcIn)
        HWall(Ctd)= DVHcIn(SurfNum)
        HAT_OC = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HWall(Ctd) + HAT_OC
        HA_OC  = Surface(SurfNum)%Area*HWall(Ctd) + HA_OC
      ENDIF

      ! The Wall surface is partially in upper and partially in lower subzone
      IF(ZInfSurf <= LayH .and. ZSupSurf >= LayH) THEN
        TempEffBulkAir(SurfNum) = ZTMX(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,DVHcIn)
        HLU= DVHcIn(SurfNum)
        TempEffBulkAir(SurfNum) = ZTOC(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,DVHcIn)
        HLD= DVHcIn(SurfNum)
        TmedDV    = ((ZSupSurf-LayH)*ZTMX(ZoneNum) + (LayH-ZInfSurf)*ZTOC(ZoneNum))/(ZSupSurf-ZInfSurf)
        HWall(Ctd)= ((LayH-ZInfSurf)*HLD + (ZSupSurf-LayH)*HLU)/(ZSupSurf-ZInfSurf)
        HAT_MX    = Surface(SurfNum)%Area*(ZSupSurf-LayH)/(ZSupSurf-ZInfSurf)* &
                             TempSurfIn(SurfNum)*HLU + HAT_MX
        HA_MX     = Surface(SurfNum)%Area*(ZSupSurf-LayH)/(ZSupSurf-ZInfSurf)*HLU + HA_MX
        HAT_OC    = Surface(SurfNum)%Area*(LayH-ZInfSurf)/(ZSupSurf-ZInfSurf)* &
                             TempSurfIn(SurfNum)*HLD + HAT_OC
        HA_OC     = Surface(SurfNum)%Area*(LayH-ZInfSurf)/(ZSupSurf-ZInfSurf)*HLD + HA_OC
        TempEffBulkAir(SurfNum) = TmedDV
      ENDIF

      DVHcIn(SurfNum) = HWall(Ctd)

    END DO  ! END WALL

    ! WINDOW Hc, HA and HAT CALCULATION
    DO Ctd = PosZ_Window((ZoneNum-1)*2 + 1),PosZ_Window((ZoneNum-1)*2 + 2)
      SurfNum = APos_Window(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp
      IF (SurfNum == 0) CYCLE
      IF (Surface(SurfNum)%Tilt > 10.0d0 .AND. Surface(SurfNum)%Tilt < 170.0d0) THEN ! Window Wall
        Z1 = MINVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z)
        Z2 = MAXVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z)
        ZSupSurf = Z2-ZoneCeilingHeight((ZoneNum-1)*2 + 1)
        ZInfSurf = Z1-ZoneCeilingHeight((ZoneNum-1)*2 + 1)

        IF(ZInfSurf > LayH)THEN
          TempEffBulkAir(SurfNum) = ZTMX(ZoneNum)
          CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,DVHcIn)
          HWindow(Ctd)= DVHcIn(SurfNum)
          HAT_MX = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HWindow(Ctd) + HAT_MX
          HA_MX = Surface(SurfNum)%Area*HWindow(Ctd) + HA_MX
        ENDIF

        IF(ZSupSurf < LayH)THEN
          TempEffBulkAir(SurfNum) = ZTOC(ZoneNum)
          CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,DVHcIn)
          HWindow(Ctd)= DVHcIn(SurfNum)
          HAT_OC = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HWindow(Ctd) + HAT_OC
          HA_OC = Surface(SurfNum)%Area*HWindow(Ctd) + HA_OC
        ENDIF

        IF(ZInfSurf <= LayH .and. ZSupSurf >= LayH) THEN
           TempEffBulkAir(SurfNum) = ZTMX(ZoneNum)
           CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,DVHcIn)
           HLU= DVHcIn(SurfNum)
           TempEffBulkAir(SurfNum) = ZTOC(ZoneNum)
           CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,DVHcIn)
           HLD= DVHcIn(SurfNum)
           TmedDV = ((ZSupSurf-LayH)*ZTMX(ZoneNum) + (LayH-ZInfSurf)*ZTOC(ZoneNum))/(ZSupSurf-ZInfSurf)
           HWindow(Ctd) = ((LayH-ZInfSurf)*HLD + (ZSupSurf-LayH)*HLU)/(ZSupSurf-ZInfSurf)
           HAT_MX = Surface(SurfNum)%Area*(ZSupSurf-LayH)/(ZSupSurf-ZInfSurf)* &
                                    TempSurfIn(SurfNum)*HLU + HAT_MX
           HA_MX = Surface(SurfNum)%Area*(ZSupSurf-LayH)/(ZSupSurf-ZInfSurf)*HLU + HA_MX
           HAT_OC = Surface(SurfNum)%Area*(LayH-ZInfSurf)/(ZSupSurf-ZInfSurf)* &
                                    TempSurfIn(SurfNum)*HLD + HAT_OC
           HA_OC = Surface(SurfNum)%Area*(LayH-ZInfSurf)/(ZSupSurf-ZInfSurf)*HLD + HA_OC
           TempEffBulkAir(SurfNum) = TmedDV
         ENDIF
       ENDIF

       IF (Surface(SurfNum)%Tilt <= 10.0d0) THEN ! Window Ceiling
         TempEffBulkAir(SurfNum) = ZTMX(ZoneNum)
         CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,DVHcIn)
         HWindow(Ctd)= DVHcIn(SurfNum)
         HAT_MX = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HWindow(Ctd) + HAT_MX
         HA_MX  = Surface(SurfNum)%Area*HWindow(Ctd) + HA_MX
       ENDIF

       IF (Surface(SurfNum)%Tilt >= 170.0d0) THEN ! Window Floor
         TempEffBulkAir(SurfNum) = ZTOC(ZoneNum)
         CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,DVHcIn)
         HWindow(Ctd)= DVHcIn(SurfNum)
         HAT_OC = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HWindow(Ctd) + HAT_OC
         HA_OC  = Surface(SurfNum)%Area*HWindow(Ctd) + HA_OC
       ENDIF

       DVHcIn(SurfNum) = HWindow(Ctd)

    END DO ! END WINDOW

    ! DOOR Hc, HA and HAT CALCULATION
    DO Ctd = PosZ_Door((ZoneNum-1)*2 + 1),PosZ_Door((ZoneNum-1)*2 + 2) ! DOOR
      SurfNum = APos_Door(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp
      IF (SurfNum == 0) CYCLE
        Z1 = MINVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z)
        Z2 = MAXVAL(Surface(SurfNum)%Vertex(1:Surface(SurfNum)%Sides)%Z)
        ZSupSurf = Z2-ZoneCeilingHeight((ZoneNum-1)*2 + 1)
        ZInfSurf = Z1-ZoneCeilingHeight((ZoneNum-1)*2 + 1)

      IF(ZInfSurf > LayH)THEN
        TempEffBulkAir(SurfNum) = ZTMX(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,DVHcIn)
        HDoor(Ctd)= DVHcIn(SurfNum)
        HAT_MX = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HDoor(Ctd) + HAT_MX
        HA_MX  = Surface(SurfNum)%Area*HDoor(Ctd) + HA_MX
      ENDIF

      IF(ZSupSurf < LayH)THEN
        TempEffBulkAir(SurfNum) = ZTOC(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,DVHcIn)
        HDoor(Ctd)= DVHcIn(SurfNum)
        HAT_OC = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HDoor(Ctd) + HAT_OC
        HA_OC  = Surface(SurfNum)%Area*HDoor(Ctd) + HA_OC
      ENDIF

      IF(ZInfSurf <= LayH .and. ZSupSurf >= LayH) THEN
        TempEffBulkAir(SurfNum) = ZTMX(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,DVHcIn)
        HLU= DVHcIn(SurfNum)
        TempEffBulkAir(SurfNum) = ZTOC(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,DVHcIn)
        HLD= DVHcIn(SurfNum)
        TmedDV = ((ZSupSurf-LayH)*ZTMX(ZoneNum) + (LayH-ZInfSurf)*ZTOC(ZoneNum))/(ZSupSurf-ZInfSurf)
        HDoor(Ctd) = ((LayH-ZInfSurf)*HLD + (ZSupSurf-LayH)*HLU)/(ZSupSurf-ZInfSurf)
        HAT_MX = Surface(SurfNum)%Area*(ZSupSurf-LayH)/(ZSupSurf-ZInfSurf)* &
                                      TempSurfIn(SurfNum)*HLU + HAT_MX
        HA_MX  = Surface(SurfNum)%Area*(ZSupSurf-LayH)/(ZSupSurf-ZInfSurf)*HLU + HA_MX
        HAT_OC = Surface(SurfNum)%Area*(LayH-ZInfSurf)/(ZSupSurf-ZInfSurf)* &
                                      TempSurfIn(SurfNum)*HLD + HAT_OC
        HA_OC  = Surface(SurfNum)%Area*(LayH-ZInfSurf)/(ZSupSurf-ZInfSurf)*HLD + HA_OC
        TempEffBulkAir(SurfNum) = TmedDV
      ENDIF

      DVHcIn(SurfNum) = HDoor(Ctd)

   END DO  ! END DOOR

    ! INTERNAL Hc, HA and HAT CALCULATION
    HeightIntMass = MIN(HeightIntMassDefault,  &
        (ZoneCeilingHeight((ZoneNum-1)*2 + 2)- ZoneCeilingHeight((ZoneNum-1)*2 + 1)))
    DO Ctd = PosZ_Internal((ZoneNum-1)*2 + 1),PosZ_Internal((ZoneNum-1)*2 + 2)
      SurfNum = APos_Internal(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp
      IF (SurfNum == 0) CYCLE
      ZSupSurf = HeightIntMass
      ZInfSurf = 0.0d0

      IF(ZSupSurf < LayH)THEN
        TempEffBulkAir(SurfNum) = ZTOC(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,DVHcIn)
        HInternal(Ctd)= DVHcIn(SurfNum)
        HAT_OC = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HInternal(Ctd) + HAT_OC
        HA_OC  = Surface(SurfNum)%Area*HInternal(Ctd) + HA_OC
      ENDIF

      IF(ZInfSurf <= LayH .and. ZSupSurf >= LayH) THEN
        TempEffBulkAir(SurfNum) = ZTMX(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,DVHcIn)
        HLU= DVHcIn(SurfNum)
        TempEffBulkAir(SurfNum) = ZTOC(ZoneNum)
        CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,DVHcIn)
        HLD= DVHcIn(SurfNum)
        TmedDV = ((ZSupSurf-LayH)*ZTMX(ZoneNum) + (LayH-ZInfSurf)*ZTOC(ZoneNum))/(ZSupSurf-ZInfSurf)
        HInternal(Ctd) = ((LayH-ZInfSurf)*HLD + (ZSupSurf-LayH)*HLU)/(ZSupSurf-ZInfSurf)
        HAT_MX = Surface(SurfNum)%Area*(ZSupSurf-LayH)/(ZSupSurf-ZInfSurf)* &
                                      TempSurfIn(SurfNum)*HLU + HAT_MX
        HA_MX  = Surface(SurfNum)%Area*(ZSupSurf-LayH)/(ZSupSurf-ZInfSurf)*HLU + HA_MX
        HAT_OC = Surface(SurfNum)%Area*(LayH-ZInfSurf)/(ZSupSurf-ZInfSurf)* &
                                      TempSurfIn(SurfNum)*HLD + HAT_OC
        HA_OC  = Surface(SurfNum)%Area*(LayH-ZInfSurf)/(ZSupSurf-ZInfSurf)*HLD + HA_OC
        TempEffBulkAir(SurfNum) = TmedDV
      ENDIF

      DVHcIn(SurfNum) = HInternal(Ctd)
    END DO  ! END INTERNAL

    ! CEILING Hc, HA and HAT CALCULATION
    DO Ctd = PosZ_Ceiling((ZoneNum-1)*2 + 1),PosZ_Ceiling((ZoneNum-1)*2 + 2)
      SurfNum = APos_Ceiling(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp
      IF (SurfNum == 0) CYCLE
      TempEffBulkAir(SurfNum) = ZTMX(ZoneNum)
      CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,DVHcIn)
      HCeiling(Ctd)= DVHcIn(SurfNum)
      HAT_MX = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HCeiling(Ctd) + HAT_MX
      HA_MX  = Surface(SurfNum)%Area*HCeiling(Ctd) + HA_MX
      DVHcIn(SurfNum) = HCeiling(Ctd)
    END DO  ! END CEILING

    ! FLOOR Hc, HA and HAT CALCULATION
    DO Ctd = PosZ_Floor((ZoneNum-1)*2 + 1),PosZ_Floor((ZoneNum-1)*2 + 2)
      SurfNum = APos_Floor(Ctd)
      Surface(SurfNum)%TAirRef = AdjacentAirTemp
      IF (SurfNum == 0) CYCLE
      TempEffBulkAir(SurfNum) = ZTFLOOR(ZoneNum)
      CALL CalcDetailedHcInForDVModel(SurfNum,TempSurfIn,DVHcIn)
      HFloor(Ctd)= DVHcIn(SurfNum)
      HAT_FLOOR = Surface(SurfNum)%Area*TempSurfIn(SurfNum)*HFloor(Ctd) + HAT_FLOOR
      HA_FLOOR  = Surface(SurfNum)%Area*HFloor(Ctd) + HA_FLOOR
      TempEffBulkAir(SurfNum) = ZTFLOOR(ZoneNum)
      DVHcIn(SurfNum) = HFloor(Ctd)
     END DO  ! END FLOOR

  ENDIF


END SUBROUTINE HcUCSDDV

!**************************************************************************************************

SUBROUTINE CalcUCSDDV(ZoneNum)

          ! SUBROUTINE INFORMATION:
          !       AUTHOR         G. Carrilho da Graca
          !       DATE WRITTEN   February 2004
          !       MODIFIED       Brent Griffith June 2008 for new interpolation and time history
          !       RE-ENGINEERED  -

          ! PURPOSE OF THIS SUBROUTINE:
          ! Subroutine for displacement ventilation modelling.
          ! This subroutine calculates the mixed subzone height, surface heat transfer coefficients and
          ! room air equivalent temperatures and three space temperatures (floor subzone, occupied zone and upper,
          ! mixed subzone temperature)

          ! METHODOLOGY EMPLOYED:
          ! -
          ! -
          ! -
          ! -

          ! REFERENCES:
          ! Model developed by Paul Linden (UCSD), G. Carrilho da Graca (UCSD) and P. Haves (LBL).
          ! Work funded by the California Energy Comission. More information on the model can found in:
          ! "Simplified Models for Heat Transfer in Rooms" G. Carrilho da Graça, Ph.D. thesis UCSD. December 2003.



          ! USE STATEMENTS:
  USE DataHeatBalFanSys
  USE DataEnvironment
  USE DataHeatBalance
  USE InputProcessor
  USE ScheduleManager,            ONLY: GetScheduleIndex, GetCurrentScheduleValue
  USE DataZoneEquipment,          ONLY: ZoneEquipConfig
  USE Psychrometrics,             ONLY: PsyRhoAirFnPbTdbW, PsyCpAirFnWTdb
  USE DataHVACGlobals,            ONLY: TimestepSys, UseZoneTimeStepHistory
  USE InternalHeatGains,          ONLY: SumInternalConvectionGainsByTypes, SumReturnAirConvectionGainsByTypes

  IMPLICIT         NONE    ! Enforce explicit typing of all variables in this routine

          ! SUBROUTINE ARGUMENT DEFINITIONS:
  INTEGER,                         INTENT(IN) :: ZoneNum   ! Which Zonenum

          ! SUBROUTINE PARAMETER DEFINITIONS:
  REAL(r64), PARAMETER :: OneThird=(1.d0/3.d0)

          ! INTERFACE BLOCK SPECIFICATIONS
          ! na

          ! DERIVED TYPE DEFINITIONS
          ! na

          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
          ! na


          ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
  REAL(r64)     :: HeightFrac           ! Fractional height of transition between occupied and mixed subzones
  REAL(r64)     :: GainsFrac            ! Fraction of lower subzone internal gains that mix as opposed to forming plumes
  REAL(r64)     :: ConvGains            ! Total convective gains in the room
  REAL(r64)     :: ConvGainsOccupiedSubzone  ! Total convective gains released in occupied subzone
  REAL(r64)     :: ConvGainsMixedSubzone  ! Total convective gains released in mixed subzone
  REAL(r64)     :: MCP_Total            ! Total capacity rate into the zone - assumed to enter at low level
  REAL(r64)     :: ZTAveraged
  REAL(r64)     :: TempDiffCritRep      ! Minimum temperature difference between mixed and occupied subzones for reporting
  LOGICAL       :: MIXFLAG
  INTEGER       :: CTD
  REAL(r64)     :: MinFlow
  REAL(r64)     :: NumPLPP              ! Number of plumes per person
  INTEGER       :: NumberOfOccupants
  REAL(r64)     :: MTGAUX
  INTEGER       :: ZoneEquipConfigNum
  INTEGER       :: NodeNum
  REAL(r64)     :: PowerInPlumes
  REAL(r64)     :: SumSysMCp
  REAL(r64)     :: SumSysMCpT
  REAL(r64)     :: NodeTemp
  REAL(r64)     :: MassFlowRate
  REAL(r64)     :: CpAir
  REAL(r64)     :: MCpT_Total
  INTEGER       :: ZoneNodeNum           ! index number of the zone node
  REAL(r64)     :: NumberOfPlumes
  REAL(r64)     :: SumMCp
  REAL(r64)     :: SumMCpT
  REAL(r64)     :: AirCap
  REAL(r64)     :: TempHistTerm
  REAL(r64)     :: PowerPerPlume
  REAL(r64)     :: HeightMixedSubzoneAve     ! Height of center of mixed air subzone
  REAL(r64)     :: HeightOccupiedSubzoneAve  ! Height of center of occupied air subzone
  REAL(r64)     :: HeightFloorSubzoneAve     ! Height of center of floor air subzone
  REAL(r64)     :: HeightThermostat        ! Height of center of thermostat/temperature control sensor
  REAL(r64)     :: HeightComfort           ! Height at which air temperature value is used to calculate comfort
  REAL(r64)     :: CeilingHeight
  REAL(r64)     :: ZoneMult                ! total zone multiplier
  INTEGER       :: Loop
  INTEGER       :: FlagApertures
  REAL(r64)     :: TempDepCoef=0.0d0           ! Formerly CoefSumha, coef in zone temp equation with dimensions of h*A
  REAL(r64)     :: TempIndCoef=0.0d0           ! Formerly CoefSumhat, coef in zone temp equation with dimensions of h*A(T1
  INTEGER, DIMENSION(28) :: IntGainTypesOccupied = (/IntGainTypeOf_People, &
                                                    IntGainTypeOf_WaterHeaterMixed, &
                                                    IntGainTypeOf_WaterHeaterStratified, &
                                                    IntGainTypeOf_ThermalStorageChilledWaterMixed, &
                                                    IntGainTypeOf_ThermalStorageChilledWaterStratified, &
                                                    IntGainTypeOf_ElectricEquipment, &
                                                    IntGainTypeOf_GasEquipment, &
                                                    IntGainTypeOf_HotWaterEquipment, &
                                                    IntGainTypeOf_SteamEquipment, &
                                                    IntGainTypeOf_OtherEquipment, &
                                                    IntGainTypeOf_ZoneBaseboardOutdoorTemperatureControlled, &
                                                    IntGainTypeOf_GeneratorFuelCell, &
                                                    IntGainTypeOf_WaterUseEquipment, &
                                                    IntGainTypeOf_GeneratorMicroCHP, &
                                                    IntGainTypeOf_ElectricLoadCenterTransformer, &
                                                    IntGainTypeOf_ElectricLoadCenterInverterSimple, &
                                                    IntGainTypeOf_ElectricLoadCenterInverterFunctionOfPower, &
                                                    IntGainTypeOf_ElectricLoadCenterInverterLookUpTable, &
                                                    IntGainTypeOf_ElectricLoadCenterStorageBattery, &
                                                    IntGainTypeOf_ElectricLoadCenterStorageSimple, &
                                                    IntGainTypeOf_PipeIndoor, &
                                                    IntGainTypeOf_RefrigerationCase, &
                                                    IntGainTypeOf_RefrigerationCompressorRack, &
                                                    IntGainTypeOf_RefrigerationSystemAirCooledCondenser ,&
                                                    IntGainTypeOf_RefrigerationSystemSuctionPipe, &
                                                    IntGainTypeOf_RefrigerationSecondaryReceiver, &
                                                    IntGainTypeOf_RefrigerationSecondaryPipe, &
                                                    IntGainTypeOf_RefrigerationWalkIn/)

  INTEGER, DIMENSION(2) :: IntGainTypesMixedSubzone = (/IntGainTypeOf_DaylightingDeviceTubular , &
                                                        IntGainTypeOf_Lights/)
  REAL(r64)   :: RetAirGain

  ! Exact solution or Euler method
  If (ZoneAirSolutionAlgo .NE. Use3rdOrder) Then
    If (ShortenTimeStepSysRoomAir .and. TimeStepSys .LT. TimeStepZone) Then
      If (PreviousTimeStep < TimeStepZone) Then
        Zone1Floor(ZoneNum) = ZoneM2Floor(ZoneNum)
        Zone1OC(ZoneNum) = ZoneM2OC(ZoneNum)
        Zone1MX(ZoneNum) = ZoneM2MX(ZoneNum)
      Else
        Zone1Floor(ZoneNum) = ZoneMXFloor(ZoneNum)
        Zone1OC(ZoneNum) = ZoneMXOC(ZoneNum)
        Zone1MX(ZoneNum) = ZoneMXMX(ZoneNum)
      End If
    Else
      Zone1Floor(ZoneNum) = ZTFLOOR(ZoneNum)
      Zone1OC(ZoneNum) = ZTOC(ZoneNum)
      Zone1MX(ZoneNum) = ZTMX(ZoneNum)
    End If
  End If

  MIXFLAG = .FALSE.
  Flagapertures=1
  DVHcIn = HConvIn
  CeilingHeight = ZoneCeilingHeight((ZoneNum-1)*2 + 2) - ZoneCeilingHeight((ZoneNum-1)*2 + 1)
  ZoneMult = Zone(ZoneNum)%Multiplier * Zone(ZoneNum)%ListMultiplier

  DO Ctd=1,TotUCSDDV
    IF(ZoneNum == ZoneUCSDDV(Ctd)%ZonePtr) THEN
      GainsFrac = GetCurrentScheduleValue(ZoneUCSDDV(Ctd)%SchedGainsPtr)
      NumPLPP = ZoneUCSDDV(Ctd)%NumPlumesPerOcc
      HeightThermostat = ZoneUCSDDV(Ctd)%ThermostatHeight
      HeightComfort = ZoneUCSDDV(Ctd)%ComfortHeight
      TempDiffCritRep = ZoneUCSDDV(Ctd)%TempTrigger
    ENDIF
  ENDDO

  CALL SumInternalConvectionGainsByTypes(ZoneNum, IntGainTypesOccupied, ConvGainsOccupiedSubzone)

  ConvGainsOccupiedSubzone = ConvGainsOccupiedSubzone &
                      + 0.5d0*SysDepZoneLoadsLagged(ZoneNum)

  ! Add heat to return air if zonal system (no return air) or cycling system (return air frequently very
  ! low or zero)
  IF (Zone(ZoneNum)%NoHeatToReturnAir) THEN
    CALL SumReturnAirConvectionGainsByTypes(ZoneNum,IntGainTypesOccupied, RetAirGain )
    ConvGainsOccupiedSubzone = ConvGainsOccupiedSubzone + RetAirGain

  END IF

  CALL SumInternalConvectionGainsByTypes(ZoneNum, IntGainTypesMixedSubzone, ConvGainsMixedSubzone)
  ConvGainsMixedSubzone = ConvGainsMixedSubzone  + SumConvHTRadSys(ZoneNum) &
                           + 0.5d0*SysDepZoneLoadsLagged(ZoneNum)
  IF (Zone(ZoneNum)%NoHeatToReturnAir) THEN
    CALL SumReturnAirConvectionGainsByTypes(ZoneNum, IntGainTypesMixedSubzone, RetAirGain)
    ConvGainsMixedSubzone = ConvGainsMixedSubzone + RetAirGain
  END IF

  ConvGains = ConvGainsOccupiedSubzone + ConvGainsMixedSubzone

  !=================== Entering air system temperature and flow====================
  SumSysMCp = 0.0d0
  SumSysMCpT = 0.0d0
  ! Check to make sure if this is a controlled zone and determine ZoneEquipConfigNum
  ZoneEquipConfigNum = ZoneNum
  IF (ZoneEquipConfig(ZoneEquipConfigNum)%IsControlled) THEN
    DO NodeNum = 1,ZoneEquipConfig(ZoneEquipConfigNum)%NumInletNodes
      NodeTemp = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%Temp
      MassFlowRate = Node(ZoneEquipConfig(ZoneEquipConfigNum)%InletNode(NodeNum))%MassFlowRate
      CpAir = PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum), NodeTemp)
      SumSysMCp = SumSysMCp + MassFlowRate * CpAir
      SumSysMCpT = SumSysMCpT + MassFlowRate * CpAir * NodeTemp
    ENDDO
  ENDIF

  SumMCp = MCPI(ZoneNum) + MCPV(ZoneNum) + MCPM(ZoneNum) + MCPE(ZoneNum) + MCPC(ZoneNum) + MdotCPOA(ZoneNum)
  SumMCpT = MCPTI(ZoneNum) + MCPTV(ZoneNum) + MCPTM(ZoneNum) + MCPTE(ZoneNum) + MCPTC(ZoneNum) + &
            MdotCPOA(ZoneNum)*Zone(ZoneNum)%OutDryBulbTemp
  if (SimulateAirflowNetwork == AirflowNetworkControlMultizone) then
    SumMCp = AirflowNetworkExchangeData(ZoneNum)%SumMCp+AirflowNetworkExchangeData(ZoneNum)%SumMMCp
    SumMCpT = AirflowNetworkExchangeData(ZoneNum)%SumMCpT+AirflowNetworkExchangeData(ZoneNum)%SumMMCpT
  end if

  MCP_Total = SumMCp + SumSysMCp
  MCpT_Total = SumMCpT + SumSysMCpT

  IF (TotPeople > 0) THEN
    NumberOfOccupants = 0
    NumberOfPlumes = 0.0d0
    DO Ctd = 1,TotPeople
      IF(People(Ctd)%ZonePtr == ZoneNum) THEN
        NumberOfOccupants = NumberOfOccupants + People(Ctd)%NumberOfPeople ! *GetCurrentScheduleValue(People(Ctd)%NumberOfPeoplePtr)
        NumberOfPlumes = NumberOfOccupants*NumPLPP
      ENDIF
    ENDDO
    IF (NumberOfPlumes == 0.0d0) THEN
      NumberOfPlumes = 1.0d0
    ENDIF
    PowerInPlumes = (1.0d0-GainsFrac)*ConvGainsOccupiedSubzone
    PowerPerPlume = PowerInPlumes/NumberOfPlumes
  ELSE
    NumberOfPlumes = 1.0d0
    PowerInPlumes = (1.0d0-GainsFrac)*ConvGainsOccupiedSubzone
    PowerPerPlume = PowerInPlumes/NumberOfPlumes
  ENDIF


  ! When AirflowNetwork is used verify if bottom apertures are inflowing and upper apertures are
  ! outflowing. The lower apertures have to be located below 0.8m and the upper apertures
  ! have to be located above 1.8m.

IF (NumOfLinksMultizone >0) THEN
  DO Loop=1, AirflowNetworkSurfaceUCSDCV(ZoneNum,0)
     ! direct AirflowNetwork surface

     IF (surface(MultizoneSurfaceData(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%SurfNum)%zone == ZoneNum) THEN

       IF ((SurfParametersCVDV(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%Zmax<0.8d0 .and. &
           AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%VolFlow>0)) THEN
             FlagApertures=0
             EXIT
       END IF
       IF (SurfParametersCVDV(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%Zmin>1.8d0 .and. &
           AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%VolFlow2>0) THEN
             FlagApertures=0
             EXIT
       END IF

       IF ((SurfParametersCVDV(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%Zmin>0.8d0 .and. &
           SurfParametersCVDV(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%Zmin<1.8d0) .or. &
           (SurfParametersCVDV(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%Zmax>0.8d0 .and. &
           SurfParametersCVDV(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%Zmax<1.8d0)) THEN
             FlagApertures=0
             EXIT
       END IF
     ! indirect AirflowNetwork surface; this is an interzone surface
     ELSE

        IF (SurfParametersCVDV(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%Zmax+ &
            zone(Surface(MultizoneSurfaceData(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%SurfNum)%zone)%originz- &
            zone(zonenum)%originz<0.8d0 .and. AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%VolFlow2>0) THEN
             FlagApertures=0
             EXIT
       END IF
       IF (SurfParametersCVDV(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%Zmin+ &
           zone(Surface(MultizoneSurfaceData(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%SurfNum)%zone)%originz- &
           zone(zonenum)%originz>1.8d0 .and. AirflowNetworkLinkSimu(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%VolFlow>0) THEN
             FlagApertures=0
             EXIT
       END IF
       IF ((SurfParametersCVDV(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%Zmin+ &
           zone(Surface(MultizoneSurfaceData(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%SurfNum)%zone)%originz- &
           zone(zonenum)%originz>0.8d0 .and. &
           SurfParametersCVDV(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%Zmin+ &
           zone(Surface(MultizoneSurfaceData(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%SurfNum)%zone)%originz- &
           zone(zonenum)%originz<1.8d0) .or. &
           (SurfParametersCVDV(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%Zmax+ &
           zone(Surface(MultizoneSurfaceData(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%SurfNum)%zone)%originz- &
           zone(zonenum)%originz>0.8d0 .and. &
           SurfParametersCVDV(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%Zmax+ &
           zone(Surface(MultizoneSurfaceData(AirflowNetworkSurfaceUCSDCV(ZoneNum,Loop))%SurfNum)%zone)%originz- &
           zone(zonenum)%originz<1.8d0)) THEN
             FlagApertures=0
             EXIT
       END IF
     ENDIF
  END DO
ENDIF

  IF ((PowerInPlumes .EQ. 0.0d0) .OR. (MCPT_Total .EQ. 0.0d0) .OR. FlagApertures==0) THEN
    ! The system will mix
    HeightFrac = 0.0d0
  ELSE
    HeightFrac = Min(24.55d0*(MCP_Total*0.000833d0/(NumberOfPlumes*PowerPerPlume**(1.d0/3.d0)))**0.6d0 / CeilingHeight , 1.0d0)
    DO Ctd = 1,4
      CALL HcUCSDDV(ZoneNum,HeightFrac)
      HeightFrac = Min(24.55d0*(MCP_Total*0.000833d0/(NumberOfPlumes*PowerPerPlume**(1.d0/3.d0)))**0.6d0 / CeilingHeight , 1.0d0)
!EPTeam-replaces above (cause diffs)      HeightFrac = Min(24.55d0*(MCP_Total*0.000833d0/(NumberOfPlumes*PowerPerPlume**(1.d0/3.d0)))**0.6 / CeilingHeight , 1.0d0)
      HeightTransition(ZoneNum) = HeightFrac * CeilingHeight
      AIRRATFloor(ZoneNum) = Zone(ZoneNum)%Volume*MIN(HeightTransition(ZoneNum),HeightFloorSubzoneTop) &
                           /CeilingHeight*ZoneVolCapMultpSens &
                           *PsyRhoAirFnPbTdbW(OutBaroPress,MATFloor(ZoneNum),ZoneAirHumRat(ZoneNum)) &
                           *PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum),MATFloor(ZoneNum))/(TimeStepSys*SecInHour)

      AIRRATOC(ZoneNum) = Zone(ZoneNum)%Volume*(HeightTransition(ZoneNum)-MIN(HeightTransition(ZoneNum),0.2d0)) &
                        /CeilingHeight*ZoneVolCapMultpSens &
                        *PsyRhoAirFnPbTdbW(OutBaroPress,MATOC(ZoneNum),ZoneAirHumRat(ZoneNum)) &
                        *PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum),MATOC(ZoneNum))/(TimeStepSys*SecInHour)

      AIRRATMX(ZoneNum) = Zone(ZoneNum)%Volume*(CeilingHeight-HeightTransition(ZoneNum)) &
                        /CeilingHeight*ZoneVolCapMultpSens &
                        *PsyRhoAirFnPbTdbW(OutBaroPress,MATMX(ZoneNum),ZoneAirHumRat(ZoneNum)) &
                        *PsyCpAirFnWTdb(ZoneAirHumRat(ZoneNum),MATMX(ZoneNum))/(TimeStepSys*SecInHour)

      IF (UseZoneTimeStepHistory) THEN
        ZTM3Floor(ZoneNum) = XM3TFloor(ZoneNum)
        ZTM2Floor(ZoneNum) = XM2TFloor(ZoneNum)
        ZTM1Floor(ZoneNum) = XMATFloor(ZoneNum)

        ZTM3OC(ZoneNum)    = XM3TOC(ZoneNum)
        ZTM2OC(ZoneNum)    = XM2TOC(ZoneNum)
        ZTM1OC(ZoneNum)    = XMATOC(ZoneNum)

        ZTM3MX(ZoneNum)    = XM3TMX(ZoneNum)
        ZTM2MX(ZoneNum)    = XM2TMX(ZoneNum)
        ZTM1MX(ZoneNum)    = XMATMX(ZoneNum)

      ELSE
        ZTM3Floor(ZoneNum) = DSXM3TFloor(ZoneNum)
        ZTM2Floor(ZoneNum) = DSXM2TFloor(ZoneNum)
        ZTM1Floor(ZoneNum) = DSXMATFloor(ZoneNum)

        ZTM3OC(ZoneNum)    = DSXM3TOC(ZoneNum)
        ZTM2OC(ZoneNum)    = DSXM2TOC(ZoneNum)
        ZTM1OC(ZoneNum)    = DSXMATOC(ZoneNum)

        ZTM3MX(ZoneNum)    = DSXM3TMX(ZoneNum)
        ZTM2MX(ZoneNum)    = DSXM2TMX(ZoneNum)
        ZTM1MX(ZoneNum)    = DSXMATMX(ZoneNum)

      ENDIF

      AirCap = AIRRATFloor(ZoneNum)
      TempHistTerm = AirCap*(3.0d0*ZTM1Floor(ZoneNum)-(3.0d0/2.0d0)*ZTM2Floor(ZoneNum)+(1.0d0/3.0d0)*ZTM3Floor(ZoneNum))
      TempDepCoef = HA_FLOOR + MCP_Total
      TempIndCoef = HAT_FLOOR + MCPT_Total + NonAirSystemResponse(ZoneNum)/ZoneMult
      SELECT CASE (ZoneAirSolutionAlgo)
        CASE (Use3rdOrder)
          ZTFLOOR(ZoneNum) = (TempHistTerm + HAT_FLOOR + MCPT_Total + NonAirSystemResponse(ZoneNum)/ZoneMult) &
                         / ((11.0d0/6.0d0)*AirCap + HA_FLOOR + MCP_Total)
        CASE (UseAnalyticalSolution)
          If (TempDepCoef .eq. 0.0d0) Then ! B=0
            ZTFLOOR(ZoneNum) = Zone1Floor(ZoneNum) + TempIndCoef/AirCap
          Else
            ZTFLOOR(ZoneNum) = (Zone1Floor(ZoneNum)-TempIndCoef/TempDepCoef)*exp(MIN(700.d0,-TempDepCoef/AirCap))+ &
                               TempIndCoef/TempDepCoef
          End If
        CASE (UseEulerMethod)
          ZTFLOOR(ZoneNum) = (AirCap*Zone1Floor(ZoneNum)+TempIndCoef)/(AirCap+TempDepCoef)
      END SELECT
      AirCap = AIRRATOC(ZoneNum)
      TempHistTerm = AirCap*(3.0d0*ZTM1OC(ZoneNum)-(3.0d0/2.0d0)*ZTM2OC(ZoneNum)+(1.0d0/3.0d0)*ZTM3OC(ZoneNum))
      TempDepCoef = HA_OC + MCP_Total
      TempIndCoef = ConvGainsOccupiedSubzone*GainsFrac + HAT_OC + ZTFLOOR(ZoneNum)*MCP_Total
      SELECT CASE (ZoneAirSolutionAlgo)
        CASE (Use3rdOrder)
          ZTOC(ZoneNum) = (TempHistTerm + ConvGainsOccupiedSubzone*GainsFrac + HAT_OC + ZTFLOOR(ZoneNum)*MCP_Total) &
                   / ((11.0d0/6.0d0)*AirCap + HA_OC + MCP_Total)
        CASE (UseAnalyticalSolution)
          If (TempDepCoef .eq. 0.0d0) Then ! B=0
            ZTOC(ZoneNum) = Zone1OC(ZoneNum) + TempIndCoef/AirCap
          Else
            If (AirCap .eq. 0.0d0) Then
              ZTOC(ZoneNum) = TempIndCoef/TempDepCoef
            Else
              ZTOC(ZoneNum) = (Zone1OC(ZoneNum)-TempIndCoef/TempDepCoef)*exp(MIN(700.d0,-TempDepCoef/AirCap))+ &
                              TempIndCoef/TempDepCoef
            End If
          End If
        CASE (UseEulerMethod)
          ZTOC(ZoneNum) = (AirCap*Zone1OC(ZoneNum)+TempIndCoef)/(AirCap+TempDepCoef)
      END SELECT
      AirCap = AIRRATMX(ZoneNum)
      TempHistTerm = AirCap*(3.0d0*ZTM1MX(ZoneNum)-(3.0d0/2.0d0)*ZTM2MX(ZoneNum)+(1.0d0/3.0d0)*ZTM3MX(ZoneNum))
      TempDepCoef = HA_MX + MCP_Total
      TempIndCoef = ConvGainsOccupiedSubzone*(1.0d0-GainsFrac) + ConvGainsMixedSubzone+ HAT_MX + ZTOC(ZoneNum)*MCP_Total
      SELECT CASE (ZoneAirSolutionAlgo)
        CASE (Use3rdOrder)
          ZTMX(ZoneNum) = (TempHistTerm + ConvGainsOccupiedSubzone*(1.0d0-GainsFrac) + ConvGainsMixedSubzone &
                    + HAT_MX + ZTOC(ZoneNum)*MCP_Total) &
                   / ((11.0d0/6.0d0)*AirCap + HA_MX + MCP_Total)
        CASE (UseAnalyticalSolution)
          If (TempDepCoef .eq. 0.0d0) Then ! B=0
            ZTMX(ZoneNum) = Zone1MX(ZoneNum) + TempIndCoef/AirCap
          Else
            If (AirCap .eq. 0.0d0) Then
              ZTMX(ZoneNum) = TempIndCoef/TempDepCoef
            Else
              ZTMX(ZoneNum) = (Zone1MX(ZoneNum)-TempIndCoef/TempDepCoef)*exp(MIN(700.d0,-TempDepCoef/AirCap))+ &
                              TempIndCoef/TempDepCoef
            End If
          End If
        CASE (UseEulerMethod)
          ZTMX(ZoneNum) = (AirCap*Zone1MX(ZoneNum)+TempIndCoef)/(AirCap+TempDepCoef)
      END SELECT
    END DO

    ! MinFlow for interface layer at z = 1.0
    MinFlow = (1.d0/24.55d0*1.0d0)**(1.0d0/0.6d0)*NumberOfPlumes*PowerPerPlume**(1.0d0/3.0d0)
!EPTeam above replaces (cause diffs?)   MinFlow = (1.d0/24.55d0*1.0d0)**(1.0d0/0.6d0)*NumberOfPlumes*PowerPerPlume**(1./3.)
    IF (MinFlow /= 0.0d0) THEN
        FracMinFlow(ZoneNum) = MCP_Total*0.000833d0 / MinFlow
    ELSE
        FracMinFlow(ZoneNum) = 9.999d0
    ENDIF
    AirModel(ZoneNum)%SimAirModel = .TRUE.
  END IF

  !=============================== M I X E D  Calculation ==============================================
  IF(ZTMX(ZoneNum) < ZTOC(ZoneNum) .or. MCP_Total <= 0.0d0 .or. &
      HeightFrac*CeilingHeight < (HeightFloorSubzoneTop + ThickOccupiedSubzoneMin)) THEN
      MIXFLAG = .TRUE.
      HeightFrac = 0.0d0
      AvgTempGrad(ZoneNum) = 0.0d0
      MaxTempGrad(ZoneNum) = 0.0d0
      AirModel(ZoneNum)%SimAirModel = .FALSE.
      AirCap = AIRRAT(ZoneNum)
      TempHistTerm = AirCap*(3.0d0*ZTM1(ZoneNum)-(3.0d0/2.0d0)*ZTM2(ZoneNum)+(1.0d0/3.0d0)*ZTM3(ZoneNum))

      DO Ctd = 1,3
        TempDepCoef = HA_MX + HA_OC + HA_FLOOR + MCP_Total
        TempIndCoef = ConvGains + HAT_MX + HAT_OC + HAT_FLOOR + MCpT_Total
        SELECT CASE (ZoneAirSolutionAlgo)
          CASE (Use3rdOrder)
            ZTAveraged = (TempHistTerm + ConvGains + HAT_MX + HAT_OC + HAT_FLOOR + MCpT_Total)/ &
                    ((11.0d0/6.0d0)*AirCap + HA_MX + HA_OC + HA_FLOOR + MCP_Total)
          CASE (UseAnalyticalSolution)
            If (TempDepCoef .eq. 0.0d0) Then ! B=0
              ZTAveraged = ZoneT1(ZoneNum) + TempIndCoef/AirCap
            Else
              ZTAveraged = (ZoneT1(ZoneNum)-TempIndCoef/TempDepCoef)*exp(MIN(700.d0,-TempDepCoef/AirCap))+ &
                            TempIndCoef/TempDepCoef
            End If
          CASE (UseEulerMethod)
            ZTAveraged = (AirCap*ZoneT1(ZoneNum)+TempIndCoef)/(AirCap+TempDepCoef)
        END SELECT
        ZTOC(ZoneNum) = ZTAveraged
        ZTMX(ZoneNum) = ZTAveraged
        ZTFLOOR(ZoneNum) = ZTAveraged
        CALL HcUCSDDV(ZoneNum,HeightFrac)
        TempDepCoef = HA_MX + HA_OC + HA_FLOOR + MCP_Total
        TempIndCoef = ConvGains + HAT_MX + HAT_OC + HAT_FLOOR + MCpT_Total
        SELECT CASE (ZoneAirSolutionAlgo)
          CASE (Use3rdOrder)
            ZTAveraged = (TempHistTerm + ConvGains + HAT_MX + HAT_OC + HAT_FLOOR + MCpT_Total)/ &
                    ((11.0d0/6.0d0)*AirCap + HA_MX + HA_OC + HA_FLOOR + MCP_Total)
          CASE (UseAnalyticalSolution)
            If (TempDepCoef .eq. 0.0d0) Then ! B=0
              ZTAveraged = ZoneT1(ZoneNum) + TempIndCoef/AirCap
            Else
              ZTAveraged = (ZoneT1(ZoneNum)-TempIndCoef/TempDepCoef)*exp(MIN(700.d0,-TempDepCoef/AirCap))+ &
                           TempIndCoef/TempDepCoef
            End If
          CASE (UseEulerMethod)
            ZTAveraged = (AirCap*ZoneT1(ZoneNum)+TempIndCoef)/(AirCap+TempDepCoef)
        END SELECT
        ZTOC(ZoneNum) = ZTAveraged
        ZTMX(ZoneNum) = ZTAveraged
        ZTFLOOR(ZoneNum) = ZTAveraged
      END DO

  END IF
  !=========================================================================================

! Comfort temperature and temperature at the thermostat/temperature control sensor

  HeightTransition(ZoneNum) = HeightFrac * CeilingHeight
  HeightMixedSubzoneAve = (CeilingHeight + HeightTransition(ZoneNum)) / 2.d0
  HeightOccupiedSubzoneAve = (HeightFloorSubzoneTop + HeightTransition(ZoneNum)) / 2.d0
  HeightFloorSubzoneAve = HeightFloorSubzoneTop / 2.d0

! Comfort temperature

  IF (MIXFLAG) THEN
    TCMF(ZoneNum) = ZTAveraged
  ELSE
    IF (HeightComfort >= 0.0d0 .AND. HeightComfort < HeightFloorSubzoneAve) THEN
      CALL ShowWarningError ('Displacement ventilation comfort height is in floor subzone in Zone: '//TRIM(Zone(ZoneNum)%Name))
      TCMF(ZoneNum) = ZTFloor(ZoneNum)
    ELSEIF (HeightComfort >= HeightFloorSubzoneAve .AND. HeightComfort < HeightOccupiedSubzoneAve) THEN
      TCMF(ZoneNum) = (ZTFloor(ZoneNum) * (HeightOccupiedSubzoneAve - HeightComfort) &
                    + ZTOC(ZoneNum) * (HeightComfort - HeightFloorSubzoneAve)) &
                    / (HeightOccupiedSubzoneAve - HeightFloorSubzoneAve)
!!      TCMF(ZoneNum) = (ZTFloor(ZoneNum) * (HeightOccupiedSubzoneAve - HeightComfort) &
!!                    + ZTMX(ZoneNum) * (HeightComfort - HeightFloorSubzoneAve)) &
!!                    / (HeightOccupiedSubzoneAve - HeightFloorSubzoneAve)
    ELSEIF (HeightComfort >= HeightOccupiedSubzoneAve .AND. HeightComfort < HeightMixedSubzoneAve) THEN
      TCMF(ZoneNum) = (ZTOC(ZoneNum) * (HeightMixedSubzoneAve - HeightComfort) &
                    + ZTMX(ZoneNum) * (HeightComfort - HeightOccupiedSubzoneAve)) &
                    / (HeightMixedSubzoneAve - HeightOccupiedSubzoneAve)
    ELSEIF (HeightComfort >= HeightMixedSubzoneAve .AND. HeightComfort <= CeilingHeight) THEN
      TCMF(ZoneNum) = ZTMX(ZoneNum)
    ELSE
      CALL ShowFatalError ('Displacement ventilation comfort height is above ceiling or below floor in Zone: '//  &
                            TRIM(Zone(ZoneNum)%Name))
    ENDIF
  ENDIF

! Temperature at the thermostat/temperature control sensor

  IF (MIXFLAG) THEN
    TempTstatAir(ZoneNum) = ZTAveraged
  ELSE
    IF (HeightThermostat >= 0.0d0 .AND. HeightThermostat < HeightFloorSubzoneAve) THEN
      CALL ShowWarningError ('Displacement thermostat is in floor subzone in Zone: '//TRIM(Zone(ZoneNum)%Name))
      TempTstatAir(ZoneNum) = ZTFloor(ZoneNum)
    ELSEIF (HeightThermostat >= HeightFloorSubzoneAve .AND. HeightThermostat < HeightOccupiedSubzoneAve) THEN
      TempTstatAir(ZoneNum) = (ZTFloor(ZoneNum) * (HeightOccupiedSubzoneAve - HeightThermostat) &
                    + ZTOC(ZoneNum) * (HeightThermostat - HeightFloorSubzoneAve)) &
                    / (HeightOccupiedSubzoneAve - HeightFloorSubzoneAve)
!!      TempTstatAir(ZoneNum) = (ZTFloor(ZoneNum) * (HeightOccupiedSubzoneAve - HeightThermostat) &
!!                    + ZTMX(ZoneNum) * (HeightThermostat - HeightFloorSubzoneAve)) &
!!                    / (HeightOccupiedSubzoneAve - HeightFloorSubzoneAve)
    ELSEIF (HeightThermostat >= HeightOccupiedSubzoneAve .AND. HeightThermostat < HeightMixedSubzoneAve) THEN
      TempTstatAir(ZoneNum) = (ZTOC(ZoneNum) * (HeightMixedSubzoneAve - HeightThermostat) &
                            + ZTMX(ZoneNum) * (HeightThermostat - HeightOccupiedSubzoneAve)) &
                            / (HeightMixedSubzoneAve - HeightOccupiedSubzoneAve)
    ELSEIF (HeightThermostat >= HeightMixedSubzoneAve .AND. HeightThermostat <= CeilingHeight) THEN
      TempTstatAir(ZoneNum) = ZTMX(ZoneNum)
    ELSE
      CALL ShowFatalError ('Displacement ventilation thermostat height is above ceiling or below floor in Zone: '//  &
                            TRIM(Zone(ZoneNum)%Name))
    ENDIF
  ENDIF

! Temperature gradients

  IF ((HeightMixedSubzoneAve - HeightFloorSubzoneAve) > 0.1d0) THEN
    AvgTempGrad(ZoneNum) = (ZTMX(ZoneNum) - ZTFLOOR(ZoneNum))/(HeightMixedSubzoneAve - HeightFloorSubzoneAve)
  ELSE
    AvgTempGrad(ZoneNum) = -9.999d0
  ENDIF
  IF ((HeightOccupiedSubzoneAve - HeightFloorSubzoneAve) > 0.1d0) THEN
    MaxTempGrad(ZoneNum) = (ZTOC(ZoneNum) - ZTFLOOR(ZoneNum))/(HeightOccupiedSubzoneAve - HeightFloorSubzoneAve)
  ELSE
    MaxTempGrad(ZoneNum) = -9.999d0
  ENDIF
  IF ((HeightMixedSubzoneAve - HeightOccupiedSubzoneAve) > 0.1d0) THEN
    MTGAUX = (ZTMX(ZoneNum)-ZTOC(ZoneNum))/(HeightMixedSubzoneAve - HeightOccupiedSubzoneAve)
  ELSE
    MTGAUX = -9.999d0
  ENDIF

  IF (MTGAUX > MaxTempGrad(ZoneNum)) THEN
    MaxTempGrad(ZoneNum) = MTGAUX
  ENDIF

  IF (MIXFLAG) THEN
    ZoneDVMixedFlag(ZoneNum) = 1
    AirModel(ZoneNum)%SimAirModel = .FALSE.
  ELSE
    ZoneDVMixedFlag(ZoneNum) = 0
    AirModel(ZoneNum)%SimAirModel = .TRUE.
  ENDIF

  IF (ZoneEquipConfig(ZoneNum)%IsControlled) THEN
    ZoneNodeNum = Zone(ZoneNum)%SystemZoneNodeNumber
    Node(ZoneNodeNum)%Temp = ZTMX(ZoneNum)
  ENDIF

! Mixed for reporting purposes
  IF ((MIXFLAG) .OR. ((ZTMX(ZoneNum)-ZTOC(ZoneNum)).LT.TempDiffCritRep)) THEN
    ZoneDVMixedFlagRep(ZoneNum) = 1.d0
    FracMinFlow(ZoneNum) = -1.0d0
    HeightTransition(ZoneNum) = -9.999d0
    AvgTempGrad(ZoneNum) = -9.999d0
    MaxTempGrad(ZoneNum) = -9.999d0
  ELSE
    ZoneDVMixedFlagRep(ZoneNum) = 0.0d0
  ENDIF



END SUBROUTINE CalcUCSDDV



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

END MODULE DisplacementVentMgr
